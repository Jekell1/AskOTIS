#!/usr/bin/env python3
"""
Remove hardcoded secrets from all files and replace with environment variable references
"""
import os
import re
from pathlib import Path

# Define secrets to remove (partial matches to avoid re-exposing)
SECRETS = {
    'X38B4_cTI2Bb2Wc1lk': 'AZURE_FUNCTION_KEY',
    'ytClysW2tFUN8FxpSC': 'AZURE_SEARCH_ADMIN_KEY', 
    'CC+qIlIhGidHQycBZ/': 'AZURE_STORAGE_CONNECTION_STRING'
}

def remove_secrets_from_file(file_path):
    """Remove secrets from a single file"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        original_content = content
        modified = False
        
        # Replace each secret pattern
        for secret_start, env_var in SECRETS.items():
            # Find full secret values (looking for base64-like strings)
            pattern = rf'{re.escape(secret_start)}[A-Za-z0-9+/=]+'
            
            if re.search(pattern, content):
                print(f"  Found secret in {file_path.name}")
                
                # Replace based on context
                if '.env' in file_path.name or 'template' in file_path.name:
                    # In template files, replace with placeholder
                    content = re.sub(pattern, f'your_{env_var.lower()}_here', content)
                elif file_path.suffix == '.json':
                    # In JSON files, replace value
                    content = re.sub(rf'("[^"]*?){pattern}([^"]*?")', rf'\1<{env_var}>\2', content)
                elif file_path.suffix == '.md':
                    # In markdown, replace with placeholder
                    content = re.sub(pattern, f'<{env_var}>', content)
                else:
                    # In Python files, replace with os.environ.get()
                    content = re.sub(
                        rf"['\"]({pattern})['\"]",
                        f'os.environ.get("{env_var}")',
                        content
                    )
                
                modified = True
        
        # Write back if modified
        if modified and content != original_content:
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(content)
            print(f"  ✅ Updated {file_path}")
            return True
        
        return False
        
    except Exception as e:
        print(f"  ❌ Error processing {file_path}: {e}")
        return False

def main():
    """Remove secrets from all files in repository"""
    print("🔒 REMOVING HARDCODED SECRETS FROM REPOSITORY")
    print("=" * 60)
    
    root_dir = Path(__file__).parent
    files_updated = 0
    
    # Process all Python, JSON, and markdown files
    for pattern in ['**/*.py', '**/*.json', '**/*.md', '**/.env*']:
        for file_path in root_dir.glob(pattern):
            # Skip certain directories
            if any(skip in str(file_path) for skip in ['_git', '.git', '__pycache__', 'node_modules']):
                continue
            
            if file_path.is_file():
                if remove_secrets_from_file(file_path):
                    files_updated += 1
    
    print(f"\n{'=' * 60}")
    print(f"✅ Updated {files_updated} files")
    print(f"\n⚠️  CRITICAL NEXT STEPS:")
    print(f"1. Review changes: git diff")
    print(f"2. Commit changes: git add -A && git commit -m 'Remove hardcoded secrets'")
    print(f"3. Force push: git push origin master --force")
    print(f"4. ROTATE ALL EXPOSED SECRETS in Azure Portal:")
    print(f"   - Regenerate Azure Function Key")
    print(f"   - Regenerate Azure Search Admin Key")
    print(f"   - Regenerate Storage Account Access Key")
    print(f"5. Update local .env file with new keys")

if __name__ == "__main__":
    main()
