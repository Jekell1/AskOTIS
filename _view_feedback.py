"""View feedback reports from Azure Table Storage."""

from azure.data.tables import TableServiceClient
from azure.identity import DefaultAzureCredential
import sys

# Try to get connection string from Azure Key Vault or use DefaultAzureCredential
try:
    # Use DefaultAzureCredential to authenticate
    credential = DefaultAzureCredential()
    
    # Replace with your actual storage account name
    storage_account_name = "YOUR_STORAGE_ACCOUNT_NAME"  # Update this!
    table_service_url = f"https://{storage_account_name}.table.core.windows.net"
    
    print(f"🔐 Connecting to {table_service_url}...")
    client = TableServiceClient(endpoint=table_service_url, credential=credential)
    
    # List all tables
    print("\n📋 Available tables:")
    tables = list(client.list_tables())
    for table in tables:
        print(f"  ✓ {table.name}")
    
    # Check for feedbackreports
    if 'feedbackreports' in [t.name for t in tables]:
        print("\n✅ 'feedbackreports' table found!")
        
        table_client = client.get_table_client('feedbackreports')
        entities = list(table_client.list_entities())
        
        print(f"\n📊 Total feedback entries: {len(entities)}")
        
        if len(entities) > 0:
            print("\n" + "="*80)
            for i, entity in enumerate(entities, 1):
                print(f"\n📝 Feedback #{i}")
                print(f"   Session: {entity.get('PartitionKey', 'N/A')}")
                print(f"   Time: {entity.get('RowKey', 'N/A')}")
                print(f"   Question: {entity.get('question', 'N/A')[:100]}...")
                print(f"   Answer (first 200 chars): {entity.get('answer', 'N/A')[:200]}...")
                print(f"   Context items: {len(entity.get('context_used', ''))}")
                print("-"*80)
        else:
            print("\n⚠️  No feedback entries yet")
    else:
        print("\n⚠️  'feedbackreports' table not found")
        print("   It will be created when first feedback is submitted")
        
except Exception as e:
    print(f"\n❌ Error: {e}")
    print("\nNote: You may need to:")
    print("  1. Update storage_account_name in this script")
    print("  2. Sign in with 'az login'")
    print("  3. Have proper permissions on the storage account")
