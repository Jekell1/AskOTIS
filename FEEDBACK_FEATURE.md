# Feedback Feature Documentation

## Overview
Added a feedback reporting system that allows users to report bad answers. When a user clicks the feedback button, the system records the question, answer, and context used to generate the answer in Azure Table Storage.

## Components

### 1. Frontend (otis-rag-chat.html)

**UI Elements:**
- Frowny face button (☹️) positioned left of the question input box
- Button is initially disabled and becomes enabled after receiving an answer
- Hover tooltip: "Report a bad answer"
- Changes to checkmark (✓) with green background after successful submission

**CSS Styling:**
```css
.feedback-btn {
    width: 40px;
    height: 40px;
    border-radius: 50%;
    border: 2px solid #ef4444;
    background: white;
    font-size: 20px;
    /* Red border, white background */
}

.feedback-btn.sent {
    background: #10b981;
    border-color: #10b981;
    /* Green when feedback sent */
}
```

**JavaScript Tracking:**
- `lastQuestion`: Tracks the user's last question
- `lastAnswer`: Tracks the LLM's last answer
- `lastContext`: Tracks all retrieved documents used to generate the answer
- `reportBadAnswer()`: Async function that sends feedback to backend

### 2. Backend (function_app.py)

**New Endpoint:** `/api/feedback`
- Method: POST
- Accepts JSON payload with feedback data
- Creates Azure Table Storage table named "feedbackreports" if it doesn't exist
- Stores feedback with session_id as PartitionKey and timestamp as RowKey

**Feedback Data Structure:**
```json
{
    "session_id": "abc123-session-id",
    "timestamp": "2025-11-01T01:30:00.000Z",
    "question": "User's original question",
    "answer": "LLM's response that was flagged",
    "context_used": "[{...}, {...}]"  // JSON array of retrieved documents
}
```

**Azure Table Storage:**
- Table Name: `feedbackreports`
- Partition Key: `session_id` (groups feedback by user session)
- Row Key: `timestamp` (ensures uniqueness and chronological ordering)
- Fields: `question`, `answer`, `context_used`
- Field Limit: 32KB per field (automatically truncated if needed)

### 3. Dependencies

**Requirements:**
- `azure-data-tables>=12.4.0` (already in requirements.txt)
- `azure-identity>=1.15.0` (for authentication)
- Uses existing `config.storage_connection_string` from environment

## User Workflow

1. **User asks question** → System retrieves context and generates answer
2. **Answer displayed** → Feedback button becomes enabled
3. **User clicks feedback button** → System sends feedback to backend
4. **Backend stores feedback** → Azure Table Storage records the data
5. **Confirmation shown** → Button turns green with checkmark, alert shows "Thank you for your feedback!"

## Deployment

**Status:** ✅ Deployed to Azure Functions
**Endpoint:** https://func-otis-rag.azurewebsites.net/api/feedback
**CORS:** Configured for cross-origin requests

**Available Functions:**
- `/api/query` - Main RAG query endpoint
- `/api/health` - Health check
- `/api/stats` - System statistics
- `/api/history` - Conversation history
- `/api/feedback` - **NEW** Feedback reporting

## Testing

**Test the feedback feature:**
1. Open the OTIS chat interface
2. Ask any question and receive an answer
3. Observe feedback button becomes enabled (red circle with frowny face)
4. Hover to see "Report a bad answer" tooltip
5. Click the button
6. Verify alert shows: "Thank you for your feedback! This will help us improve the system."
7. Observe button changes to green checkmark and becomes disabled

**View feedback reports:**
1. Open Azure Portal
2. Navigate to Storage Account
3. Open Table Storage
4. Find table named "feedbackreports"
5. View feedback entries organized by session_id

## Data Analysis

**Query feedback by session:**
```python
from azure.data.tables import TableServiceClient

# Connect to Table Storage
table_service = TableServiceClient.from_connection_string(connection_string)
table_client = table_service.get_table_client('feedbackreports')

# Get all feedback for a specific session
session_feedback = table_client.query_entities(
    query_filter=f"PartitionKey eq 'session_abc123'"
)

for feedback in session_feedback:
    print(f"Question: {feedback['question']}")
    print(f"Answer: {feedback['answer'][:200]}...")
    print(f"Timestamp: {feedback['RowKey']}")
    print("---")
```

**Get recent feedback across all sessions:**
```python
# Get all feedback ordered by timestamp
all_feedback = table_client.list_entities()

# Sort by RowKey (timestamp) descending
recent_feedback = sorted(
    all_feedback, 
    key=lambda x: x['RowKey'], 
    reverse=True
)[:20]

for feedback in recent_feedback:
    print(f"Session: {feedback['PartitionKey']}")
    print(f"Time: {feedback['RowKey']}")
    print(f"Q: {feedback['question']}")
    print("---")
```

## Benefits

1. **Quality Monitoring:** Identify incorrect or unhelpful responses
2. **Improvement Opportunities:** Analyze context used to find retrieval issues
3. **User Engagement:** Simple one-click reporting mechanism
4. **Session Tracking:** Link feedback to specific user sessions
5. **Context Preservation:** Full context captured for debugging

## Future Enhancements

Potential improvements:
- Add optional text feedback field for user comments
- Create analytics dashboard for feedback trends
- Email notifications for new feedback reports
- Automatic retry with different retrieval strategy for flagged answers
- Thumbs up/down rating system for positive feedback too
- Export feedback reports to CSV for analysis
