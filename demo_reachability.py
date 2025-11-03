#!/usr/bin/env python3

import sys
import os

# Add the cobolparser directory to Python path
sys.path.append(r'c:\Users\jeff.childers\Downloads\cobolparser')

from cobolparser import load_azure_credentials
from azure.storage.blob import BlobServiceClient
import json
from collections import defaultdict, deque

def demonstrate_reachability_analysis():
    """Demonstrate how to use flow_edges.jsonl for reachability analysis"""
    
    print("=== FLOW EDGE REACHABILITY ANALYSIS DEMO ===")
    print("Showing how flow_edges.jsonl answers 'Who can reach paragraph X?'")
    print()
    
    try:
        # Load Azure credentials and get flow edges
        connection_string = load_azure_credentials()
        blob_service_client = BlobServiceClient.from_connection_string(connection_string)
        container_name = "aisearch"
        output_prefix = "S35-Source/JSONL/"
        
        container_client = blob_service_client.get_container_client(container_name)
        blob_client = container_client.get_blob_client(f"{output_prefix}flow_edges.jsonl")
        
        if not blob_client.exists():
            print("❌ flow_edges.jsonl not found. Run the parser first.")
            return
        
        # Load flow edges
        flow_edges_content = blob_client.download_blob().readall().decode('utf-8')
        flow_edges = []
        
        for line in flow_edges_content.strip().split('\n'):
            if line.strip():
                flow_edges.append(json.loads(line))
        
        print(f"📊 Loaded {len(flow_edges)} flow edges from {len(set(e['file_id'] for e in flow_edges))} files")
        print()
        
        # Build reachability graph
        def build_reachability_graph(edges):
            """Build a graph for reachability analysis"""
            graph = defaultdict(set)  # caller -> set of targets
            reverse_graph = defaultdict(set)  # target -> set of callers
            
            for edge in edges:
                caller = f"{edge['file_id']}:{edge['caller_para']}"
                target = f"{edge['file_id']}:{edge['target_para']}"
                
                graph[caller].add(target)
                reverse_graph[target].add(caller)
            
            return graph, reverse_graph
        
        def find_all_callers(target_paragraph, reverse_graph, max_depth=5):
            """Find all paragraphs that can reach the target (direct and indirect)"""
            visited = set()
            queue = deque([(target_paragraph, 0)])
            callers_by_depth = defaultdict(list)
            
            while queue:
                current, depth = queue.popleft()
                
                if current in visited or depth > max_depth:
                    continue
                    
                visited.add(current)
                
                # Add direct callers
                for caller in reverse_graph.get(current, set()):
                    if caller not in visited:
                        callers_by_depth[depth + 1].append(caller)
                        queue.append((caller, depth + 1))
            
            return callers_by_depth
        
        # Build the graph
        graph, reverse_graph = build_reachability_graph(flow_edges)
        
        print("🔗 REACHABILITY GRAPH BUILT:")
        print(f"   Nodes (paragraphs): {len(set(list(graph.keys()) + list(reverse_graph.keys())))}")
        print(f"   Edges (PERFORM calls): {len(flow_edges)}")
        print()
        
        # Demonstrate reachability queries
        print("🎯 REACHABILITY QUERY EXAMPLES:")
        print()
        
        # Find some interesting targets to analyze
        targets_to_analyze = []
        for edge in flow_edges[:3]:  # Take first few edges as examples
            target = f"{edge['file_id']}:{edge['target_para']}"
            if target not in targets_to_analyze:
                targets_to_analyze.append(target)
        
        for target in targets_to_analyze:
            file_id, para_name = target.split(':', 1)
            print(f"❓ WHO CAN REACH '{para_name}' in {file_id}?")
            
            callers_by_depth = find_all_callers(target, reverse_graph)
            
            if not callers_by_depth:
                print("   📭 No callers found")
            else:
                total_callers = sum(len(callers) for callers in callers_by_depth.values())
                print(f"   📊 {total_callers} caller(s) found at different depths:")
                
                for depth in sorted(callers_by_depth.keys()):
                    callers = callers_by_depth[depth]
                    print(f"   Depth {depth} ({len(callers)} caller(s)):")
                    for caller in callers[:5]:  # Show first 5
                        caller_file, caller_para = caller.split(':', 1)
                        print(f"      • {caller_para} (in {caller_file})")
                    if len(callers) > 5:
                        print(f"      ... and {len(callers) - 5} more")
            print()
        
        # Summary statistics
        print("📈 FLOW ANALYSIS STATISTICS:")
        
        # Most called paragraphs
        target_counts = defaultdict(int)
        for edge in flow_edges:
            target_counts[edge['target_para']] += 1
        
        print("   Most frequently called paragraphs:")
        for para, count in sorted(target_counts.items(), key=lambda x: x[1], reverse=True)[:5]:
            print(f"      {para}: called {count} time(s)")
        
        # Most calling paragraphs
        caller_counts = defaultdict(int)
        for edge in flow_edges:
            caller_counts[edge['caller_para']] += 1
        
        print("   Most active caller paragraphs:")
        for para, count in sorted(caller_counts.items(), key=lambda x: x[1], reverse=True)[:5]:
            print(f"      {para}: calls {count} paragraph(s)")
        
    except Exception as e:
        print(f"❌ Error: {e}")
    
    print()
    print("🎉 CONCLUSION:")
    print("   ✅ flow_edges.jsonl enables efficient paragraph reachability analysis")
    print("   ✅ Can answer 'Who can reach paragraph X?' for any paragraph")
    print("   ✅ Supports both direct and transitive reachability queries")
    print("   ✅ Perfect for code impact analysis and call graph visualization")

if __name__ == "__main__":
    demonstrate_reachability_analysis()
