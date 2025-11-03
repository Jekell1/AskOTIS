"""Query Application Insights to analyze recent queries and their routing."""
import os
import json
from datetime import datetime, timedelta
from azure.monitor.query import LogsQueryClient, LogsQueryStatus
from azure.identity import DefaultAzureCredential

# Application Insights workspace ID
WORKSPACE_ID = "71e4625d-e7d4-4e14-b083-0b2d368e48e2"

def query_recent_queries(hours=1):
    """Query recent requests to see routing and results."""
    
    credential = DefaultAzureCredential()
    client = LogsQueryClient(credential)
    
    # Query for recent requests with custom dimensions
    query = f"""
    requests
    | where timestamp > ago({hours}h)
    | where name == "query"
    | project 
        timestamp,
        duration,
        resultCode,
        success,
        customDimensions
    | order by timestamp desc
    | limit 20
    """
    
    try:
        response = client.query_workspace(
            workspace_id=WORKSPACE_ID,
            query=query,
            timespan=timedelta(hours=hours)
        )
        
        if response.status == LogsQueryStatus.SUCCESS:
            print(f"\n=== Recent Queries (last {hours} hours) ===\n")
            for row in response.tables[0].rows:
                timestamp = row[0]
                duration = row[1]
                result_code = row[2]
                success = row[3]
                custom_dims = row[4]
                
                print(f"Time: {timestamp}")
                print(f"Duration: {duration}ms")
                print(f"Success: {success}")
                
                if custom_dims:
                    dims = json.loads(custom_dims) if isinstance(custom_dims, str) else custom_dims
                    if 'question_length' in dims:
                        print(f"Question length: {dims.get('question_length')}")
                    if 'answer_length' in dims:
                        print(f"Answer length: {dims.get('answer_length')}")
                print("-" * 80)
        else:
            print(f"Query failed with status: {response.status}")
            
    except Exception as e:
        print(f"Error querying Application Insights: {e}")
        print("\nTry using Azure Portal instead:")
        print("https://portal.azure.com/#@/resource/.../providers/microsoft.insights/components/func-otis-rag-insights/logs")

def query_custom_metrics(hours=1):
    """Query custom metrics for query performance."""
    
    credential = DefaultAzureCredential()
    client = LogsQueryClient(credential)
    
    query = f"""
    customMetrics
    | where timestamp > ago({hours}h)
    | where name in ("query_duration", "query_count")
    | project 
        timestamp,
        name,
        value,
        customDimensions
    | order by timestamp desc
    | limit 20
    """
    
    try:
        response = client.query_workspace(
            workspace_id=WORKSPACE_ID,
            query=query,
            timespan=timedelta(hours=hours)
        )
        
        if response.status == LogsQueryStatus.SUCCESS:
            print(f"\n=== Custom Metrics (last {hours} hours) ===\n")
            for row in response.tables[0].rows:
                timestamp = row[0]
                metric_name = row[1]
                value = row[2]
                custom_dims = row[3]
                
                print(f"Time: {timestamp}")
                print(f"Metric: {metric_name} = {value}")
                if custom_dims:
                    print(f"Dimensions: {custom_dims}")
                print("-" * 80)
        else:
            print(f"Query failed with status: {response.status}")
            
    except Exception as e:
        print(f"Error querying metrics: {e}")

if __name__ == '__main__':
    print("Querying Application Insights for OTIS RAG system...")
    query_recent_queries(hours=2)
    query_custom_metrics(hours=2)
