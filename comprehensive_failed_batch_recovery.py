#!/usr/bin/env python3
"""
Comprehensive Failed Batch Recovery
Re-extracts data from source files with correct schema to recover all failed batches
"""

import os
import json
import requests
import hashlib
import re
from datetime import datetime
from secrets_loader import load_secrets

class ComprehensiveFailedBatchRecovery:
    def __init__(self):
        load_secrets()
        self.search_endpoint = os.getenv('AZURE_SEARCH_ENDPOINT') or os.getenv('SEARCH_ENDPOINT')
        self.search_key = os.getenv('AZURE_SEARCH_KEY') or os.getenv('SEARCH_KEY')
        self.headers = {
            'api-key': self.search_key,
            'Content-Type': 'application/json'
        }
        self.screen_index = 'new_cobol_screen_nodes'
        self.source_index = 'new-cobol-files'
        self.batch_size = 25  # Smaller batches for reliability
        
    def get_source_files_sample(self):
        """Get a sample of source files to re-extract from"""
        print("   🔍 Getting source files for re-extraction...")
        
        search_url = f"{self.search_endpoint}/indexes/{self.source_index}/docs/search?api-version=2023-11-01"
        search_body = {
            "search": "hasScreens:true OR SCREEN OR ACCEPT OR DISPLAY",
            "select": "id,content,path",
            "top": 100  # Process a reasonable sample
        }
        
        try:
            response = requests.post(search_url, headers=self.headers, json=search_body)
            if response.status_code == 200:
                result = response.json()
                files = result.get('value', [])
                print(f"   ✅ Found {len(files)} source files with screen potential")
                return files
            else:
                print(f"   ❌ Error getting source files: {response.status_code}")
                return []
        except Exception as e:
            print(f"   ❌ Exception: {e}")
            return []
    
    def extract_screens_with_correct_schema(self, file_content, file_id, file_path):
        """Extract screens using the correct schema format"""
        screens = []
        
        if not file_content:
            return screens
        
        lines = file_content.split('\n')
        
        # Method 1: SCREEN SECTION extraction
        screens.extend(self._extract_screen_sections(lines, file_id, file_path))
        
        # Method 2: WORKING-STORAGE field extraction  
        screens.extend(self._extract_working_storage_fields(lines, file_id, file_path))
        
        # Method 3: ACCEPT/DISPLAY patterns
        screens.extend(self._extract_accept_display_patterns(lines, file_id, file_path))
        
        return screens
    
    def _extract_screen_sections(self, lines, file_id, file_path):
        """Extract from SCREEN SECTION with correct schema"""
        screens = []
        
        in_screen_section = False
        current_screen = None
        screen_lines = []
        
        for i, line in enumerate(lines):
            line_stripped = line.strip()
            
            if 'SCREEN' in line_stripped and 'SECTION' in line_stripped:
                in_screen_section = True
                continue
            
            if in_screen_section:
                if line_stripped.startswith(('PROCEDURE', 'DATA', 'FILE', 'WORKING-STORAGE')):
                    in_screen_section = False
                    if current_screen:
                        screens.append(self._build_screen_document(current_screen, screen_lines, file_id, file_path, 'screen_section'))
                    break
                
                # Look for screen definitions
                if re.match(r'^\d+\s+[A-Z][A-Z0-9-]+', line_stripped):
                    if current_screen:
                        screens.append(self._build_screen_document(current_screen, screen_lines, file_id, file_path, 'screen_section'))
                    
                    current_screen = {
                        'name': line_stripped.split()[1],
                        'line_number': i + 1
                    }
                    screen_lines = [line]
                elif current_screen:
                    screen_lines.append(line)
        
        # Handle last screen
        if current_screen:
            screens.append(self._build_screen_document(current_screen, screen_lines, file_id, file_path, 'screen_section'))
        
        return screens
    
    def _extract_working_storage_fields(self, lines, file_id, file_path):
        """Extract field definitions from WORKING-STORAGE"""
        screens = []
        
        in_working_storage = False
        
        for i, line in enumerate(lines):
            line_stripped = line.strip()
            
            if 'WORKING-STORAGE' in line_stripped and 'SECTION' in line_stripped:
                in_working_storage = True
                continue
            
            if in_working_storage:
                if line_stripped.startswith(('PROCEDURE', 'FILE', 'LINKAGE')):
                    break
                
                # Look for level 01 or 05 fields that look like screens
                if re.match(r'^\d+\s+[A-Z][A-Z0-9-]*(-SCREEN|-FORM|-DISPLAY|-MAP)', line_stripped):
                    field_name = line_stripped.split()[1]
                    
                    # Collect related lines
                    field_lines = [line]
                    for j in range(i + 1, min(i + 20, len(lines))):
                        if lines[j].strip() and not lines[j].strip().startswith(('01', '77')):
                            field_lines.append(lines[j])
                        else:
                            break
                    
                    screen_doc = self._build_screen_document(
                        {'name': field_name, 'line_number': i + 1},
                        field_lines,
                        file_id,
                        file_path,
                        'working_storage'
                    )
                    screens.append(screen_doc)
        
        return screens
    
    def _extract_accept_display_patterns(self, lines, file_id, file_path):
        """Extract UI interaction patterns"""
        screens = []
        
        for i, line in enumerate(lines):
            line_stripped = line.strip().upper()
            
            if 'ACCEPT' in line_stripped or 'DISPLAY' in line_stripped:
                # Extract screen name from ACCEPT/DISPLAY statements
                match = re.search(r'(ACCEPT|DISPLAY)\s+([A-Z][A-Z0-9-]+)', line_stripped)
                if match:
                    screen_name = match.group(2)
                    
                    # Look for related lines
                    context_lines = []
                    start = max(0, i - 3)
                    end = min(len(lines), i + 4)
                    
                    for j in range(start, end):
                        context_lines.append(lines[j])
                    
                    screen_doc = self._build_screen_document(
                        {'name': screen_name, 'line_number': i + 1},
                        context_lines,
                        file_id,
                        file_path,
                        'accept_display'
                    )
                    screens.append(screen_doc)
        
        return screens
    
    def _build_screen_document(self, screen_info, content_lines, file_id, file_path, method):
        """Build screen document with correct schema"""
        
        screen_name = screen_info.get('name', 'UNKNOWN')
        line_number = screen_info.get('line_number', 0)
        
        # Create unique screen_id
        unique_key = f"{file_id}_{screen_name}_{method}_{line_number}"
        screen_id = hashlib.sha256(unique_key.encode()).hexdigest()
        
        # Analyze content for fields
        field_names = self._extract_field_names(content_lines)
        
        # Determine screen type
        screen_type = 'form'
        content_text = '\n'.join(content_lines).upper()
        if any(word in content_text for word in ['REPORT', 'LIST', 'DISPLAY']):
            screen_type = 'report'
        
        # Build document with correct schema
        document = {
            "screen_id": screen_id,
            "program_id": file_id,
            "screen_name": screen_name,
            "screen_type": screen_type,
            "field_count": len(field_names),
            "action_count": content_text.count('ACCEPT') + content_text.count('DISPLAY'),
            "transition_count": 0,
            
            "fields_json": json.dumps(field_names),
            "actions_json": json.dumps([]),
            "transitions_json": json.dumps([]),
            "screen_elements_json": json.dumps([]),
            "layout_info_json": json.dumps({"method": method, "line": line_number}),
            
            "raw_span_text": '\n'.join(content_lines),
            "summary_text": f"Screen {screen_name} from {file_path}; {len(field_names)} fields; extracted via {method}",
            
            "generated_at": datetime.now().isoformat() + 'Z',
            "has_vector": False
        }
        
        return document
    
    def _extract_field_names(self, lines):
        """Extract field names from COBOL content"""
        field_names = []
        
        for line in lines:
            # Look for field definitions
            match = re.search(r'^\s*\d+\s+([A-Z][A-Z0-9-]+)', line.strip())
            if match:
                field_name = match.group(1)
                if not field_name.endswith(('-SECTION', '-DIVISION')):
                    field_names.append(field_name)
        
        return field_names
    
    def get_existing_screen_ids(self):
        """Get existing screen IDs to avoid duplicates"""
        print("   📋 Getting existing screen IDs...")
        
        existing_ids = set()
        search_url = f"{self.search_endpoint}/indexes/{self.screen_index}/docs/search?api-version=2023-11-01"
        
        # Get all existing IDs in batches
        skip = 0
        while True:
            search_body = {
                "search": "*",
                "select": "screen_id",
                "top": 1000,
                "skip": skip
            }
            
            try:
                response = requests.post(search_url, headers=self.headers, json=search_body)
                if response.status_code == 200:
                    result = response.json()
                    docs = result.get('value', [])
                    
                    if not docs:
                        break
                    
                    for doc in docs:
                        existing_ids.add(doc.get('screen_id'))
                    
                    skip += len(docs)
                    if len(docs) < 1000:
                        break
                else:
                    break
            except:
                break
        
        print(f"   ✅ Found {len(existing_ids)} existing screen IDs")
        return existing_ids
    
    def upload_screens_batch(self, screens):
        """Upload screens with proper error handling"""
        if not screens:
            return True, 0, 0
        
        upload_url = f"{self.search_endpoint}/indexes/{self.screen_index}/docs/index?api-version=2023-11-01"
        upload_body = {
            "value": [
                {"@search.action": "upload", **screen} for screen in screens
            ]
        }
        
        try:
            response = requests.post(upload_url, headers=self.headers, json=upload_body)
            
            if response.status_code == 200:
                result = response.json()
                successful = sum(1 for item in result.get('value', []) if item.get('status'))
                failed = len(screens) - successful
                return True, successful, failed
            else:
                return False, 0, len(screens)
                
        except Exception as e:
            print(f"   ❌ Upload error: {e}")
            return False, 0, len(screens)
    
    def run_comprehensive_recovery(self):
        """Main recovery process"""
        print("🔧 COMPREHENSIVE FAILED BATCH RECOVERY")
        print("=" * 50)
        
        # Get initial stats
        print("📊 Step 1: Initial status...")
        stats_url = f"{self.search_endpoint}/indexes/{self.screen_index}/stats?api-version=2023-11-01"
        try:
            response = requests.get(stats_url, headers=self.headers)
            if response.status_code == 200:
                stats = response.json()
                initial_count = stats.get('documentCount', 0)
                print(f"   📈 Initial screen count: {initial_count}")
            else:
                initial_count = 0
        except:
            initial_count = 0
        
        # Get existing IDs
        existing_ids = self.get_existing_screen_ids()
        
        # Get source files to re-process
        print("\n🔍 Step 2: Getting source files for re-extraction...")
        source_files = self.get_source_files_sample()
        
        if not source_files:
            print("   ❌ No source files found")
            return
        
        # Process files and extract screens
        print(f"\n⚙️ Step 3: Re-extracting screens from {len(source_files)} files...")
        all_new_screens = []
        processed_files = 0
        
        for file_data in source_files:
            file_id = file_data.get('id', 'unknown')
            file_path = file_data.get('path', 'unknown')
            content = file_data.get('content', '')
            
            if content:
                screens = self.extract_screens_with_correct_schema(content, file_id, file_path)
                
                # Filter out existing screens
                new_screens = [s for s in screens if s['screen_id'] not in existing_ids]
                all_new_screens.extend(new_screens)
                
                # Update existing IDs set
                for screen in new_screens:
                    existing_ids.add(screen['screen_id'])
                
                processed_files += 1
                if processed_files % 10 == 0:
                    print(f"   📁 Processed {processed_files}/{len(source_files)} files, found {len(all_new_screens)} new screens")
        
        print(f"   ✅ Extracted {len(all_new_screens)} new screens from {processed_files} files")
        
        # Upload in batches
        print(f"\n📤 Step 4: Uploading new screens...")
        total_uploaded = 0
        total_failed = 0
        
        for i in range(0, len(all_new_screens), self.batch_size):
            batch = all_new_screens[i:i + self.batch_size]
            batch_num = (i // self.batch_size) + 1
            total_batches = (len(all_new_screens) + self.batch_size - 1) // self.batch_size
            
            print(f"   📦 Uploading batch {batch_num}/{total_batches} ({len(batch)} screens)...")
            
            success, uploaded, failed = self.upload_screens_batch(batch)
            total_uploaded += uploaded
            total_failed += failed
            
            if success:
                print(f"      ✅ Batch {batch_num}: {uploaded} uploaded, {failed} failed")
            else:
                print(f"      ❌ Batch {batch_num}: Upload failed")
        
        # Final status
        print(f"\n📊 Step 5: Final status...")
        try:
            response = requests.get(stats_url, headers=self.headers)
            if response.status_code == 200:
                stats = response.json()
                final_count = stats.get('documentCount', 0)
                net_increase = final_count - initial_count
                print(f"   📈 Final screen count: {final_count}")
                print(f"   ➕ Net increase: {net_increase}")
            else:
                final_count = initial_count
                net_increase = 0
        except:
            final_count = initial_count
            net_increase = 0
        
        # Summary
        print(f"\n🎯 RECOVERY SUMMARY:")
        print(f"   📁 Processed files: {processed_files}")
        print(f"   🔍 New screens extracted: {len(all_new_screens)}")
        print(f"   ✅ Successfully uploaded: {total_uploaded}")
        print(f"   ❌ Failed uploads: {total_failed}")
        print(f"   📈 Net database increase: {net_increase}")
        
        success_rate = (total_uploaded / (total_uploaded + total_failed) * 100) if (total_uploaded + total_failed) > 0 else 0
        print(f"   🎉 Upload success rate: {success_rate:.1f}%")
        
        if net_increase > 0:
            print(f"   ✅ Recovery successful! Added {net_increase} screens to the database.")
        else:
            print(f"   ℹ️ No new screens added (likely due to existing duplicates)")

if __name__ == "__main__":
    recovery = ComprehensiveFailedBatchRecovery()
    recovery.run_comprehensive_recovery()