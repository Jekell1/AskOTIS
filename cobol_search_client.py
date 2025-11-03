import json
import requests
from typing import Dict, List, Optional, Any

class CobolSearchAPI:
    """
    Python client for the COBOL Search API based on the OpenAPI specification.
    """
    
    def __init__(self, search_service_name: str, api_key: str, api_version: str = "2024-07-01"):
        """
        Initialize the COBOL Search API client.
        
        Args:
            search_service_name: Azure Search service name (e.g., 'az-use1-ai-search')
            api_key: Azure Search admin or query key
            api_version: API version to use
        """
        self.base_url = f"https://{search_service_name}.search.windows.net"
        self.headers = {
            'Content-Type': 'application/json',
            'api-key': api_key
        }
        self.api_version = api_version
    
    def search(
        self,
        search: str,
        select: Optional[str] = None,
        filter: Optional[str] = None,
        orderby: Optional[str] = None,
        top: int = 50,
        skip: int = 0,
        count: bool = False,
        search_mode: str = "any",
        search_fields: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Search the COBOL index.
        
        Args:
            search: Search query (use "*" for all documents)
            select: Comma-separated fields to return
            filter: OData filter expression
            orderby: Field to sort by
            top: Max results to return (1-1000)
            skip: Results to skip for pagination
            count: Include total count
            search_mode: "any" or "all" terms must match
            search_fields: Fields to search in
            
        Returns:
            Search response dictionary
            
        Example:
            # Simple search
            results = api.search("CUSTOMER-ACCNT-NO")
            
            # Filtered search for variables
            results = api.search(
                search="*",
                filter="symbol_kind eq 'variable'",
                select="repo_path,line,code,symbol_name",
                top=20
            )
        """
        url = f"{self.base_url}/indexes/cobol-index/docs/search"
        params = {"api-version": self.api_version}
        
        # Build request body
        body = {
            "search": search,
            "top": top,
            "skip": skip,
            "count": count,
            "searchMode": search_mode
        }
        
        # Add optional parameters
        if select:
            body["select"] = select
        if filter:
            body["filter"] = filter
        if orderby:
            body["orderby"] = orderby
        if search_fields:
            body["searchFields"] = search_fields
        
        response = requests.post(url, headers=self.headers, params=params, json=body)
        response.raise_for_status()
        return response.json()
    
    def get_document_count(self) -> int:
        """
        Get the total number of documents in the index.
        
        Returns:
            Total document count
        """
        result = self.search("*", top=0, count=True)
        return result.get("@odata.count", 0)
    
    def find_variables(self, variable_name: Optional[str] = None, top: int = 50) -> List[Dict]:
        """
        Find COBOL variables.
        
        Args:
            variable_name: Specific variable name to search for (optional)
            top: Max results to return
            
        Returns:
            List of variable documents
        """
        search_query = f'"{variable_name}"' if variable_name else "*"
        
        result = self.search(
            search=search_query,
            filter="symbol_kind eq 'variable'",
            select="repo_path,line,code,symbol_name,symbol_kind",
            top=top
        )
        return result.get("value", [])
    
    def find_programs(self, program_name: Optional[str] = None, top: int = 50) -> List[Dict]:
        """
        Find COBOL programs.
        
        Args:
            program_name: Specific program name to search for (optional)
            top: Max results to return
            
        Returns:
            List of program documents
        """
        search_query = f'"{program_name}"' if program_name else "*"
        
        result = self.search(
            search=search_query,
            filter="symbol_kind eq 'program'",
            select="repo_path,line,code,symbol_name,symbol_kind",
            top=top
        )
        return result.get("value", [])
    
    def find_code_containing(self, code_pattern: str, top: int = 50) -> List[Dict]:
        """
        Find COBOL code lines containing a specific pattern.
        
        Args:
            code_pattern: Text pattern to search for in code
            top: Max results to return
            
        Returns:
            List of matching code documents
        """
        result = self.search(
            search=code_pattern,
            search_fields="code",
            select="repo_path,line,code,symbol_name,symbol_kind",
            orderby="repo_path,line",
            top=top
        )
        return result.get("value", [])


def main():
    """
    Demo usage of the COBOL Search API client.
    """
    # Load configuration
    with open('local.settings.json', 'r') as f:
        config = json.load(f)
    
    # Initialize API client
    api = CobolSearchAPI(
        search_service_name="az-use1-ai-search",
        api_key=config['Values']['SEARCH_KEY']
    )
    
    print("🔍 COBOL Search API Demo")
    print("=" * 40)
    
    # Get document count
    total_docs = api.get_document_count()
    print(f"📊 Total documents in index: {total_docs:,}")
    
    print("\n1. SEARCHING FOR PROGRAMS:")
    print("-" * 30)
    programs = api.find_programs(top=5)
    for i, prog in enumerate(programs, 1):
        filename = prog['repo_path'].split('/')[-1]
        print(f"{i}. {prog['symbol_name']} in {filename}")
        print(f"   Line {prog['line']}: {prog['code'].strip()}")
    
    print("\n2. SEARCHING FOR 'IDENTIFICATION DIVISION':")
    print("-" * 40)
    divisions = api.find_code_containing("IDENTIFICATION DIVISION", top=5)
    for i, div in enumerate(divisions, 1):
        filename = div['repo_path'].split('/')[-1]
        print(f"{i}. {filename} (line {div['line']})")
        print(f"   {div['code'].strip()}")
    
    print("\n3. SEARCHING FOR VARIABLES:")
    print("-" * 30)
    variables = api.find_variables(top=5)
    if variables:
        for i, var in enumerate(variables, 1):
            filename = var['repo_path'].split('/')[-1]
            print(f"{i}. {var.get('symbol_name', 'Unknown')} in {filename}")
            print(f"   Line {var['line']}: {var['code'].strip()}")
    else:
        print("No variables found (symbol extraction may need improvement)")
    
    print("\n4. CUSTOM SEARCH - 'PROGRAM-ID':")
    print("-" * 35)
    custom_results = api.search(
        search='"PROGRAM-ID"',
        select="repo_path,line,code",
        top=3
    )
    
    for i, result in enumerate(custom_results.get('value', []), 1):
        filename = result['repo_path'].split('/')[-1]
        print(f"{i}. {filename} (line {result['line']})")
        print(f"   {result['code'].strip()}")
        print(f"   Score: {result.get('@search.score', 'N/A')}")
    
    print(f"\n✅ API client working successfully!")
    print(f"📈 Index contains {total_docs:,} searchable COBOL documents")
    print("=" * 40)


if __name__ == "__main__":
    main()
