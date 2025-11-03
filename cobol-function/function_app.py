import logging
import re
import json
import sys
from pathlib import Path
import azure.functions as func

# Add parent directory to path for otis_rag module
sys.path.insert(0, str(Path(__file__).parent.parent))

# COBOL parsing regular expressions
DECL_VAR = re.compile(r"^\s*(\d{1,2})\s+([A-Z0-9-]+)\s+PIC\b", re.IGNORECASE)
PARA = re.compile(r"^([A-Z0-9-]+)\s*\.$")
SECTION = re.compile(r"^([A-Z0-9-]+)\s+SECTION\.$", re.IGNORECASE)
PROGID = re.compile(r"\bPROGRAM-ID\.\s+([A-Z0-9-]+)\s*\.", re.IGNORECASE)
CALL = re.compile(r"\bCALL\s+['\"]?([A-Z0-9-]+)['\"]?", re.IGNORECASE)

def parse_line(code: str):
    """Parse a line of COBOL code and extract symbols and calls"""
    sym_name = None
    sym_kind = None
    calls = []
    
    if PROGID.search(code):
        sym_name = PROGID.search(code).group(1)
        sym_kind = "program"
    elif SECTION.match(code):
        sym_name = SECTION.match(code).group(1)
        sym_kind = "section"
    elif PARA.match(code) and "SECTION" not in code.upper():
        sym_name = PARA.match(code).group(1)
        sym_kind = "paragraph"
    elif DECL_VAR.match(code):
        sym_name = DECL_VAR.match(code).group(2)
        sym_kind = "variable"
    
    # Find all CALL statements
    for m in CALL.findall(code):
        calls.append(m)
    
    return sym_name, sym_kind, calls

# Create the Function App
app = func.FunctionApp()

@app.function_name(name="cobol-parse")
@app.route(route="cobol-parse", methods=["POST"], auth_level=func.AuthLevel.FUNCTION)
def cobol_parse(req: func.HttpRequest) -> func.HttpResponse:
    """
    Azure Cognitive Search WebAPI Custom Skill for COBOL parsing
    
    Expected input format:
    {
        "values": [
            {
                "recordId": "record1",
                "data": {
                    "code": "PROGRAM-ID. TESTPROG.",
                    "repo_path": "/path/to/file",
                    "line_number": 1
                }
            }
        ]
    }
    """
    logging.info('COBOL parsing function triggered.')
    
    try:
        # Get the batch request
        batch = req.get_json()
        if not batch:
            return func.HttpResponse(
                json.dumps({"error": "No JSON body provided"}),
                status_code=400,
                mimetype="application/json"
            )
        
        values = batch.get("values", [])
        if not values:
            return func.HttpResponse(
                json.dumps({"error": "No 'values' array provided"}),
                status_code=400,
                mimetype="application/json"
            )
        
        # Process each item in the batch
        out = {"values": []}
        for item in values:
            try:
                rec_id = item.get("recordId")
                data = item.get("data", {})
                
                # Extract input data
                code = data.get("code", "")
                repo_path = data.get("repo_path", "")
                line_number = data.get("line_number", 0)
                
                logging.info(f'Processing record {rec_id}: {len(code)} characters')
                
                # Parse the COBOL code
                sym_name, sym_kind, calls = parse_line(code or "")
                
                # Create output record
                result = {
                    "recordId": rec_id,
                    "data": {
                        "symbols": sym_name if sym_name else None,
                        "calls": calls if calls else [],
                        "procedures": sym_name if sym_kind in ["program", "section", "paragraph"] else None,
                        "variables": sym_name if sym_kind == "variable" else None,
                        "symbol_type": sym_kind,
                        "processed_repo_path": repo_path,
                        "processed_line_number": line_number
                    }
                }
                
                out["values"].append(result)
                logging.info(f'Processed record {rec_id}: found {sym_kind} "{sym_name}", {len(calls)} calls')
                
            except Exception as item_error:
                logging.error(f"Error processing item {item.get('recordId', 'unknown')}: {str(item_error)}")
                # Add error record
                out["values"].append({
                    "recordId": item.get("recordId", "unknown"),
                    "errors": [{"message": f"Processing error: {str(item_error)}"}]
                })
        
        logging.info(f'Batch processing complete: {len(out["values"])} records processed')
        
        return func.HttpResponse(
            json.dumps(out, indent=2),
            mimetype="application/json",
            status_code=200
        )
        
    except Exception as e:
        logging.exception("Critical error in COBOL parsing function")
        error_response = {
            "error": f"Function error: {str(e)}",
            "values": []
        }
        return func.HttpResponse(
            json.dumps(error_response),
            status_code=500,
            mimetype="application/json"
        )

@app.function_name(name="rag-ask")
@app.route(route="rag-ask", methods=["POST"], auth_level=func.AuthLevel.ANONYMOUS)
def rag_ask(req: func.HttpRequest) -> func.HttpResponse:
    """
    RAG endpoint for COBOL code questions
    
    Expected input:
    {
        "query": "What programs use LPBW1IN.CPY"
    }
    """
    logging.info('RAG ask function triggered.')
    
    try:
        from otis_rag import OTISRAG
        
        # Get query
        req_body = req.get_json()
        if not req_body:
            return func.HttpResponse(
                json.dumps({"error": "No JSON body provided"}),
                status_code=400,
                mimetype="application/json"
            )
        
        query = req_body.get("query", "")
        if not query:
            return func.HttpResponse(
                json.dumps({"error": "No 'query' field provided"}),
                status_code=400,
                mimetype="application/json"
            )
        
        logging.info(f'Processing query: {query}')
        
        # Initialize RAG system
        rag = OTISRAG()
        
        # Get answer
        answer = rag.ask(query)
        
        logging.info(f'Generated answer: {len(answer)} characters')
        
        return func.HttpResponse(
            json.dumps({"answer": answer}),
            mimetype="application/json",
            status_code=200
        )
        
    except Exception as e:
        logging.exception("Error in RAG function")
        return func.HttpResponse(
            json.dumps({"error": f"RAG error: {str(e)}"}),
            status_code=500,
            mimetype="application/json"
        )
