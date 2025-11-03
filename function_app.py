"""
Azure Functions HTTP triggers for OTIS RAG system.

This module provides REST API endpoints for the OTIS codebase Q&A system:
- POST /api/query - Answer questions about the OTIS COBOL codebase
- GET /api/health - Health check endpoint
- GET /api/stats - System statistics

Environment variables required:
- AZURE_SEARCH_ENDPOINT
- AZURE_SEARCH_API_KEY
- AZURE_OPENAI_ENDPOINT
- AZURE_OPENAI_API_KEY
- AZURE_OPENAI_CHAT_DEPLOYMENT
- AZURE_OPENAI_EMBEDDING_DEPLOYMENT
- APPLICATIONINSIGHTS_CONNECTION_STRING (optional, for telemetry)
"""

import logging
import json
import os
import time
import azure.functions as func
from typing import Optional

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Initialize Application Insights if configured
app_insights_enabled = False
telemetry_client = None

try:
    connection_string = os.environ.get('APPLICATIONINSIGHTS_CONNECTION_STRING')
    if connection_string:
        from opencensus.ext.azure.log_exporter import AzureLogHandler
        from opencensus.ext.azure import metrics_exporter
        from opencensus.stats import aggregation as aggregation_module
        from opencensus.stats import measure as measure_module
        from opencensus.stats import stats as stats_module
        from opencensus.stats import view as view_module
        from opencensus.tags import tag_map as tag_map_module
        
        # Add Azure logging handler
        logger.addHandler(AzureLogHandler(connection_string=connection_string))
        
        # Setup metrics exporter
        exporter = metrics_exporter.new_metrics_exporter(connection_string=connection_string)
        
        # Create measures for custom metrics
        query_duration_measure = measure_module.MeasureFloat("query_duration", "Duration of query processing", "ms")
        query_count_measure = measure_module.MeasureInt("query_count", "Number of queries processed", "1")
        
        # Create views
        query_duration_view = view_module.View(
            "query_duration_view",
            "Distribution of query processing durations",
            [],
            query_duration_measure,
            aggregation_module.DistributionAggregation([50, 100, 200, 500, 1000, 2000, 5000, 10000])
        )
        
        query_count_view = view_module.View(
            "query_count_view",
            "Total number of queries",
            [],
            query_count_measure,
            aggregation_module.CountAggregation()
        )
        
        # Register views
        stats = stats_module.stats
        view_manager = stats.view_manager
        view_manager.register_view(query_duration_view)
        view_manager.register_view(query_count_view)
        
        # Setup stats recorder
        stats_recorder = stats.stats_recorder
        mmap = stats_recorder.new_measurement_map()
        tmap = tag_map_module.TagMap()
        
        app_insights_enabled = True
        logger.info("Application Insights telemetry enabled")
        
except Exception as e:
    logger.warning(f"Application Insights initialization failed (will continue without telemetry): {e}")

# Initialize Function App
app = func.FunctionApp(http_auth_level=func.AuthLevel.ANONYMOUS)

# Global RAG instance (singleton pattern for performance)
_rag_instance = None


def get_rag_instance():
    """
    Get or create the OTIS RAG instance.
    Uses singleton pattern to avoid reinitializing on every request.
    """
    global _rag_instance
    
    if _rag_instance is None:
        try:
            logger.info("Initializing OTIS RAG system...")
            from otis_rag.rag import OTISRAG
            _rag_instance = OTISRAG()
            logger.info("OTIS RAG system initialized successfully")
        except Exception as e:
            logger.error(f"Failed to initialize RAG system: {e}", exc_info=True)
            raise
    
    return _rag_instance


@app.route(route="query", methods=["POST"])
def query(req: func.HttpRequest) -> func.HttpResponse:
    """
    Answer questions about the OTIS COBOL codebase.
    
    Request body:
    {
        "question": "What programs handle elevator dispatching?",
        "top_k": 5,  // optional, default 5
        "temperature": 0.3,  // optional, default 0.3
        "stream": false  // optional, set to true for streaming
    }
    
    Response:
    {
        "question": "...",
        "answer": "...",
        "sources": [...],
        "duration_seconds": 2.5
    }
    """
    logger.info("Query endpoint called")
    
    start_time = time.time()
    question_text = None
    
    try:
        # Parse request body
        try:
            req_body = req.get_json()
        except ValueError:
            logger.warning("Invalid JSON in request body")
            return func.HttpResponse(
                json.dumps({"error": "Invalid JSON in request body"}),
                status_code=400,
                mimetype="application/json"
            )
        
        # Validate required fields
        question = req_body.get('question')
        if not question:
            logger.warning("Missing question field in request")
            return func.HttpResponse(
                json.dumps({"error": "Missing 'question' field"}),
                status_code=400,
                mimetype="application/json"
            )
        
        question_text = question[:100]  # Save truncated version for telemetry
        
        # Extract optional parameters
        top_k = req_body.get('top_k', 5)
        temperature = req_body.get('temperature', 0.3)
        stream = req_body.get('stream', False)
        session_id = req_body.get('session_id')  # Optional session ID for conversation memory
        enable_persistence = req_body.get('enable_persistence', False)  # Enable persistent memory
        
        logger.info(f"Processing question: {question_text}... (stream={stream}, session_id={session_id})")
        
        # Log custom event to Application Insights
        if app_insights_enabled:
            logger.info(
                f"RAG Query: {question_text}...",
                extra={
                    'custom_dimensions': {
                        'question_length': len(question),
                        'stream_enabled': stream,
                        'top_k': top_k,
                        'temperature': temperature,
                        'has_session_id': bool(session_id),
                        'persistence_enabled': enable_persistence
                    }
                }
            )
        
        # Get RAG instance with session support if needed
        if session_id and enable_persistence:
            # Create a new RAG instance with session-specific memory
            from otis_rag import OTISRAG
            rag = OTISRAG(enable_persistence=True, session_id=session_id)
        else:
            # Use singleton instance for stateless queries
            rag = get_rag_instance()

        # Get the full answer with timing breakdown
        # Note: Azure Functions doesn't support true SSE streaming with generators
        # So we return the full answer and let the frontend simulate streaming
        result = rag.ask_with_timing(question, verbose=False)
        answer = result['answer']
        timing = result['timing']
        context_docs = result.get('context_docs', [])  # Get retrieved documents
        
        # Calculate duration
        duration_ms = (time.time() - start_time) * 1000
        
        # Log metrics to Application Insights
        if app_insights_enabled:
            try:
                mmap.measure_float_put(query_duration_measure, duration_ms)
                mmap.measure_int_put(query_count_measure, 1)
                mmap.record(tmap)
                
                logger.info(
                    f"Query completed in {duration_ms:.2f}ms",
                    extra={
                        'custom_dimensions': {
                            'duration_ms': duration_ms,
                            'answer_length': len(answer),
                            'question_preview': question_text,
                            'question_type': timing.get('question_type'),
                            'retrieve_ms': timing.get('retrieve_seconds', 0) * 1000,
                            'generate_ms': timing.get('generate_seconds', 0) * 1000
                        }
                    }
                )
            except Exception as telemetry_error:
                logger.warning(f"Failed to record telemetry: {telemetry_error}")
        
        # Return response
        response_data = {
            "question": question,
            "answer": answer,
            "sources": context_docs,  # Include retrieved context documents
            "stream_mode": stream,  # Frontend can use this to simulate streaming
            "duration_ms": duration_ms,
            "timing": timing  # Include detailed timing breakdown
        }
        
        logger.info(f"Query completed successfully in {duration_ms:.2f}ms")
        
        return func.HttpResponse(
            json.dumps(response_data, indent=2),
            status_code=200,
            mimetype="application/json",
            headers={
                "Access-Control-Allow-Origin": "*",
                "Access-Control-Allow-Methods": "POST, OPTIONS",
                "Access-Control-Allow-Headers": "Content-Type"
            }
        )
        
    except Exception as e:
        duration_ms = (time.time() - start_time) * 1000
        logger.error(
            f"Error processing query after {duration_ms:.2f}ms: {e}",
            exc_info=True,
            extra={
                'custom_dimensions': {
                    'error_type': type(e).__name__,
                    'duration_ms': duration_ms,
                    'question_preview': question_text if question_text else 'N/A'
                }
            }
        )
        return func.HttpResponse(
            json.dumps({"error": str(e)}),
            status_code=500,
            mimetype="application/json",
            headers={
                "Access-Control-Allow-Origin": "*"
            }
        )


@app.route(route="health", methods=["GET"])
def health(req: func.HttpRequest) -> func.HttpResponse:
    """
    Health check endpoint.
    Returns 200 if the system is operational.
    """
    logger.info("Health check endpoint called")
    
    try:
        # Try to get RAG instance (lazy initialization)
        rag = get_rag_instance()
        
        health_data = {
            "status": "healthy",
            "rag_initialized": rag is not None,
            "message": "OTIS RAG system is operational"
        }
        
        return func.HttpResponse(
            json.dumps(health_data, indent=2),
            status_code=200,
            mimetype="application/json",
            headers={
                "Access-Control-Allow-Origin": "*"
            }
        )
        
    except Exception as e:
        logger.error(f"Health check failed: {e}", exc_info=True)
        return func.HttpResponse(
            json.dumps({
                "status": "unhealthy",
                "error": str(e)
            }),
            status_code=503,
            mimetype="application/json",
            headers={
                "Access-Control-Allow-Origin": "*"
            }
        )


@app.route(route="stats", methods=["GET"])
def stats(req: func.HttpRequest) -> func.HttpResponse:
    """
    Return system statistics and configuration info.
    """
    logger.info("Stats endpoint called")
    
    try:
        rag = get_rag_instance()
        
        # Get configuration from RAG system
        config = rag.config  # Access config through RAG instance
        
        stats_data = {
            "indexes_configured": len(config.indexes),
            "index_names": list(config.indexes.keys()),
            "chat_model": config.chat_deployment,
            "embedding_model": config.embed_deployment,
            "phase1_enabled": True,  # Enhanced prompts
            "phase2_enabled": True,  # Transaction taxonomy + complexity metrics
            "status": "operational"
        }
        
        return func.HttpResponse(
            json.dumps(stats_data, indent=2),
            status_code=200,
            mimetype="application/json",
            headers={
                "Access-Control-Allow-Origin": "*"
            }
        )
        
    except Exception as e:
        logger.error(f"Error getting stats: {e}", exc_info=True)
        return func.HttpResponse(
            json.dumps({"error": str(e)}),
            status_code=500,
            mimetype="application/json",
            headers={
                "Access-Control-Allow-Origin": "*"
            }
        )


@app.route(route="history", methods=["GET", "DELETE"])
def history(req: func.HttpRequest) -> func.HttpResponse:
    """
    Retrieve or delete conversation history for a session.
    
    GET - Retrieve history:
    Query parameters:
    - session_id: The session identifier (required)
    
    Response:
    {
        "session_id": "...",
        "turns": [
            {"user": "...", "assistant": "..."},
            ...
        ]
    }
    
    DELETE - Delete history:
    Query parameters:
    - session_id: The session identifier (required)
    
    Response:
    {
        "session_id": "...",
        "deleted": true,
        "count": 5
    }
    """
    method = req.method
    logger.info(f"History endpoint called with method: {method}")
    
    try:
        # Get session_id from query parameters
        session_id = req.params.get('session_id')
        
        if not session_id:
            logger.warning("Missing session_id parameter")
            return func.HttpResponse(
                json.dumps({"error": "Missing 'session_id' query parameter"}),
                status_code=400,
                mimetype="application/json",
                headers={
                    "Access-Control-Allow-Origin": "*",
                    "Access-Control-Allow-Methods": "GET, DELETE, OPTIONS"
                }
            )
        
        from otis_rag.config import Config
        config = Config()
        
        if method == "DELETE":
            # Delete conversation history from Table Storage
            logger.info(f"Deleting history for session: {session_id}")
            
            try:
                from azure.data.tables import TableServiceClient
                from azure.core.exceptions import ResourceNotFoundError
                
                service_client = TableServiceClient.from_connection_string(
                    config.storage_connection_string
                )
                table_client = service_client.get_table_client("OTISChatHistory")
                
                # Query all entities for this session
                entities = list(table_client.query_entities(
                    query_filter=f"PartitionKey eq '{session_id}'"
                ))
                
                deleted_count = 0
                for entity in entities:
                    try:
                        table_client.delete_entity(
                            partition_key=entity['PartitionKey'],
                            row_key=entity['RowKey']
                        )
                        deleted_count += 1
                    except ResourceNotFoundError:
                        pass  # Already deleted
                
                logger.info(f"Deleted {deleted_count} conversation turns for session {session_id}")
                
                return func.HttpResponse(
                    json.dumps({
                        "session_id": session_id,
                        "deleted": True,
                        "count": deleted_count
                    }, indent=2),
                    status_code=200,
                    mimetype="application/json",
                    headers={
                        "Access-Control-Allow-Origin": "*",
                        "Access-Control-Allow-Methods": "GET, DELETE, OPTIONS"
                    }
                )
                
            except Exception as delete_error:
                logger.error(f"Error deleting history: {delete_error}", exc_info=True)
                return func.HttpResponse(
                    json.dumps({"error": f"Failed to delete history: {str(delete_error)}"}),
                    status_code=500,
                    mimetype="application/json",
                    headers={
                        "Access-Control-Allow-Origin": "*",
                        "Access-Control-Allow-Methods": "GET, DELETE, OPTIONS"
                    }
                )
        
        else:  # GET
            logger.info(f"Fetching history for session: {session_id}")
            
            # Load conversation history from Table Storage
            from otis_rag.memory import ConversationMemory
            
            memory = ConversationMemory(
                backend="table",
                connection_string=config.storage_connection_string,
                table_name="OTISChatHistory"
            )
            
            # Load the session
            memory.load_session(session_id)
            
            # Format turns for response
            turns = []
            for turn in memory.turns:
                turns.append({
                    "user": turn['user'],
                    "assistant": turn['assistant']
                })
            
            logger.info(f"Retrieved {len(turns)} conversation turns for session {session_id}")
            
            return func.HttpResponse(
                json.dumps({
                    "session_id": session_id,
                    "turns": turns,
                    "count": len(turns)
                }, indent=2),
                status_code=200,
                mimetype="application/json",
                headers={
                    "Access-Control-Allow-Origin": "*",
                    "Access-Control-Allow-Methods": "GET, DELETE, OPTIONS"
                }
            )
        
    except Exception as e:
        logger.error(f"Error in history endpoint: {e}", exc_info=True)
        return func.HttpResponse(
            json.dumps({"error": str(e)}),
            status_code=500,
            mimetype="application/json",
            headers={
                "Access-Control-Allow-Origin": "*",
                "Access-Control-Allow-Methods": "GET, DELETE, OPTIONS"
            }
        )


@app.route(route="feedback", methods=["POST", "OPTIONS"])
def feedback_endpoint(req: func.HttpRequest) -> func.HttpResponse:
    """
    Record user feedback for bad answers.
    Stores feedback in Azure Table Storage for quality monitoring.
    """
    logger.info("========== FEEDBACK ENDPOINT CALLED ==========")
    logger.info(f"Method: {req.method}")
    logger.info(f"URL: {req.url}")
    logger.info(f"Headers: {dict(req.headers)}")
    
    # Handle CORS preflight
    if req.method == "OPTIONS":
        logger.info("Handling CORS preflight request")
        return func.HttpResponse(
            status_code=200,
            headers={
                "Access-Control-Allow-Origin": "*",
                "Access-Control-Allow-Methods": "POST, OPTIONS",
                "Access-Control-Allow-Headers": "Content-Type"
            }
        )
    
    try:
        from azure.data.tables import TableServiceClient
        from otis_rag.config import Config
        
        config = Config()
        logger.info(f"✅ Config loaded successfully")
        
        # Parse feedback data
        logger.info("Parsing request body...")
        raw_body = req.get_body().decode('utf-8')
        logger.info(f"Raw body length: {len(raw_body)}")
        logger.info(f"Raw body preview: {raw_body[:200]}")
        
        feedback_data = req.get_json()
        logger.info(f"Parsed JSON keys: {list(feedback_data.keys())}")
        
        session_id = feedback_data.get('session_id', 'unknown')
        timestamp = feedback_data.get('timestamp', '')
        question = feedback_data.get('question', '')
        answer = feedback_data.get('answer', '')
        context_used = feedback_data.get('context_used', '')
        
        logger.info(f"📝 Feedback details:")
        logger.info(f"  Session: {session_id}")
        logger.info(f"  Timestamp: {timestamp}")
        logger.info(f"  Question length: {len(question)}")
        logger.info(f"  Answer length: {len(answer)}")
        logger.info(f"  Context length: {len(context_used)}")
        
        # Connect to Azure Table Storage
        logger.info("Connecting to Azure Table Storage...")
        logger.info(f"Storage connection string available: {bool(config.storage_connection_string)}")
        
        table_service = TableServiceClient.from_connection_string(
            config.storage_connection_string
        )
        logger.info("✅ Connected to Table Storage")
        
        # Get or create feedback table
        logger.info("Getting/creating feedbackreports table...")
        table_client = table_service.get_table_client('feedbackreports')
        try:
            table_client.create_table()
            logger.info("✅ Created new feedbackreports table")
        except Exception as create_ex:
            logger.info(f"Table already exists or creation skipped: {create_ex}")
            pass
        
        # Prepare entity for storage
        logger.info("Preparing entity for storage...")
        # Azure Table Storage has 32KB limit per property, 1MB per entity
        row_key = timestamp.replace(':', '-').replace('.', '-')  # Make valid row key
        logger.info(f"RowKey: {row_key}")
        
        entity = {
            'PartitionKey': session_id,
            'RowKey': row_key,
            'question': question[:32000],  # Truncate if needed
            'answer': answer[:32000],
            'context_used': context_used[:32000]
        }
        logger.info(f"Entity prepared with {len(entity)} fields")
        
        # Insert feedback
        logger.info("Inserting entity into table...")
        table_client.create_entity(entity)
        
        logger.info(f"✅✅✅ Feedback recorded successfully for session {session_id} ✅✅✅")
        
        return func.HttpResponse(
            json.dumps({
                "status": "success",
                "message": "Feedback recorded successfully"
            }),
            status_code=200,
            mimetype="application/json",
            headers={
                "Access-Control-Allow-Origin": "*"
            }
        )
        
    except Exception as e:
        logger.error(f"❌❌❌ ERROR RECORDING FEEDBACK ❌❌❌")
        logger.error(f"Error type: {type(e).__name__}")
        logger.error(f"Error message: {str(e)}", exc_info=True)
        return func.HttpResponse(
            json.dumps({
                "status": "error",
                "message": str(e)
            }),
            status_code=500,
            mimetype="application/json",
            headers={
                "Access-Control-Allow-Origin": "*"
            }
        )
