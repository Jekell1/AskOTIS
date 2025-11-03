# Application Insights Setup for OTIS RAG

## Overview

Application Insights has been configured for the OTIS RAG Azure Function to provide comprehensive monitoring, logging, and telemetry.

## What's Configured

### Azure Resources

1. **Application Insights Resource**
   - **Name**: `func-otis-rag-insights`
   - **Resource Group**: `azure_ai_rg`
   - **Location**: `eastus`
   - **Type**: Web application
   - **Instrumentation Key**: `71e4625d-e7d4-4e14-b083-0b2d368e48e2`

2. **Function App Integration**
   - Connection string configured in function app settings
   - Automatic integration with Azure Functions runtime
   - Custom telemetry and metrics enabled

### Custom Telemetry

The function app collects:

1. **Standard Metrics** (Automatic):
   - Request count
   - Response times
   - Failed requests
   - Dependency calls (Azure Search, OpenAI)
   - Exceptions

2. **Custom Metrics** (via OpenCensus):
   - `query_duration_view`: Distribution of query processing times
   - `query_count_view`: Total number of queries processed
   - Duration buckets: 50ms, 100ms, 200ms, 500ms, 1s, 2s, 5s, 10s

3. **Custom Logging** (via Azure Log Handler):
   - Query text previews (first 100 characters)
   - Query parameters (stream mode, top_k, temperature)
   - Answer length
   - Processing duration
   - Error details with stack traces

### Log Properties

Custom dimensions added to logs:
- `question_length`: Length of user question
- `stream_enabled`: Whether streaming was requested
- `top_k`: Number of results requested
- `temperature`: LLM temperature setting
- `duration_ms`: Query processing time in milliseconds
- `answer_length`: Length of generated answer
- `question_preview`: First 100 chars of question
- `error_type`: Exception type (for errors)

## Accessing Application Insights

### Azure Portal

1. Navigate to: https://portal.azure.com
2. Search for: `func-otis-rag-insights`
3. Or go to Resource Group: `azure_ai_rg` → `func-otis-rag-insights`

### Quick Links

- **Live Metrics**: See real-time telemetry as requests come in
- **Transaction Search**: Search and filter individual requests
- **Logs**: Query logs using Kusto Query Language (KQL)
- **Metrics**: View aggregated metrics and charts
- **Performance**: Analyze dependency calls and bottlenecks
- **Failures**: Investigate errors and exceptions

## Useful Queries

### Query Processing Times

```kql
customMetrics
| where name == "query_duration"
| summarize 
    avg(value), 
    percentile(value, 50), 
    percentile(value, 95), 
    percentile(value, 99) 
by bin(timestamp, 1h)
| render timechart
```

### Query Volume

```kql
customMetrics
| where name == "query_count"
| summarize count() by bin(timestamp, 1h)
| render timechart
```

### Recent Queries with Details

```kql
traces
| where message contains "RAG Query:"
| extend 
    question = tostring(customDimensions.question_preview),
    duration = todouble(customDimensions.duration_ms),
    answerLength = toint(customDimensions.answer_length)
| project timestamp, question, duration, answerLength
| order by timestamp desc
| take 50
```

### Error Analysis

```kql
traces
| where severityLevel >= 3  // Warning and above
| extend 
    errorType = tostring(customDimensions.error_type),
    question = tostring(customDimensions.question_preview)
| summarize count() by errorType, bin(timestamp, 1h)
| render columnchart
```

### Slow Queries (> 5 seconds)

```kql
traces
| where message contains "Query completed"
| extend duration = todouble(customDimensions.duration_ms)
| where duration > 5000
| extend question = tostring(customDimensions.question_preview)
| project timestamp, question, duration
| order by duration desc
```

### All Custom Dimensions

```kql
traces
| where timestamp > ago(1h)
| extend 
    question_length = toint(customDimensions.question_length),
    stream_enabled = tobool(customDimensions.stream_enabled),
    duration_ms = todouble(customDimensions.duration_ms),
    answer_length = toint(customDimensions.answer_length),
    question_preview = tostring(customDimensions.question_preview),
    error_type = tostring(customDimensions.error_type)
| project timestamp, question_preview, question_length, duration_ms, answer_length, stream_enabled, error_type
| order by timestamp desc
```

## Monitoring Dashboard

### Key Metrics to Watch

1. **Response Time (P95)**: Should be < 5 seconds
2. **Error Rate**: Should be < 1%
3. **Request Count**: Track usage patterns
4. **Dependency Duration**: 
   - Azure Search calls: < 500ms
   - OpenAI calls: < 3 seconds

### Alerts (Recommended)

Consider setting up alerts for:

1. **High Error Rate**
   - Condition: Error rate > 5% over 5 minutes
   - Action: Email/SMS notification

2. **Slow Response Times**
   - Condition: P95 response time > 10 seconds
   - Action: Email notification

3. **High Request Volume**
   - Condition: > 100 requests/minute
   - Action: Email notification (may need scaling)

4. **Dependency Failures**
   - Condition: Azure Search or OpenAI failures
   - Action: Email/SMS notification

## Configuration Details

### Environment Variables

The function app uses this environment variable for Application Insights:

```
APPLICATIONINSIGHTS_CONNECTION_STRING=InstrumentationKey=71e4625d-e7d4-4e14-b083-0b2d368e48e2;IngestionEndpoint=https://eastus-8.in.applicationinsights.azure.com/;LiveEndpoint=https://eastus.livediagnostics.monitor.azure.com/;ApplicationId=9a520419-8d14-48ef-84b3-ac7ff2a97a57
```

This is automatically configured and doesn't need to be changed.

### Dependencies Added

**requirements.txt**:
```python
opencensus-ext-azure>=1.1.13
opencensus-ext-logging>=0.1.1
```

These packages provide:
- Azure Log Handler for log streaming
- Metrics exporter for custom metrics
- Distributed tracing support

### host.json Configuration

Already configured with Application Insights sampling:

```json
{
  "logging": {
    "applicationInsights": {
      "samplingSettings": {
        "isEnabled": true,
        "maxTelemetryItemsPerSecond": 20
      }
    }
  }
}
```

**Sampling**: Enabled to reduce costs. Samples telemetry at 20 items/second max. In high-traffic scenarios, not every request will be logged, but metrics and aggregations remain accurate.

## Cost Considerations

### Pricing Tiers

Application Insights has two pricing models:
1. **Pay-as-you-go**: $2.88/GB ingested
2. **Commitment tiers**: Reduced rates for higher volumes

### Typical Costs

For the OTIS RAG function with current traffic:
- **Logs**: ~5 MB per 1000 queries
- **Metrics**: ~2 MB per 1000 queries  
- **Traces**: ~10 MB per 1000 queries

**Estimated**: $0.05 per 1000 queries (~$50/month for 1M queries)

### Cost Optimization

1. **Sampling**: Already enabled (20 items/sec limit)
2. **Log Levels**: Set to INFO (not DEBUG)
3. **Retention**: Default 90 days (configurable)
4. **Data Volume Cap**: Can set daily cap if needed

## Troubleshooting

### Telemetry Not Appearing

1. **Check Configuration**:
   ```powershell
   az functionapp config appsettings list --name func-otis-rag --resource-group azure_ai_rg --query "[?name=='APPLICATIONINSIGHTS_CONNECTION_STRING']"
   ```

2. **Check Logs**:
   - Azure Portal → Function App → Log stream
   - Look for "Application Insights telemetry enabled" message

3. **Wait for Delay**:
   - Telemetry can take 2-5 minutes to appear
   - Live Metrics shows data immediately
   - Logs/Analytics have ~2 minute delay

### High Costs

1. **Review Data Volume**:
   ```kql
   Usage
   | where TimeGenerated > ago(7d)
   | summarize sum(Quantity) by DataType
   | render piechart
   ```

2. **Adjust Sampling**:
   - Edit `host.json`
   - Reduce `maxTelemetryItemsPerSecond`
   - Redeploy

3. **Set Daily Cap**:
   - Azure Portal → Application Insights → Usage and estimated costs
   - Set daily volume cap

### Missing Custom Metrics

Ensure OpenCensus packages are installed:
```bash
pip list | grep opencensus
```

Should show:
- opencensus==0.11.4
- opencensus-ext-azure==1.1.15
- opencensus-ext-logging==0.1.1

## Best Practices

1. **Use Custom Dimensions**: Already implemented for rich context
2. **Log at Appropriate Levels**:
   - INFO: Normal operations
   - WARNING: Unexpected but handled
   - ERROR: Failures requiring attention
3. **Include Correlation IDs**: Helps trace distributed calls
4. **Monitor Regularly**: Review dashboard weekly
5. **Set Up Alerts**: Be proactive, not reactive

## Related Resources

- [Application Insights Documentation](https://docs.microsoft.com/en-us/azure/azure-monitor/app/app-insights-overview)
- [Azure Functions Monitoring](https://docs.microsoft.com/en-us/azure/azure-functions/functions-monitoring)
- [Kusto Query Language (KQL)](https://docs.microsoft.com/en-us/azure/data-explorer/kusto/query/)
- [OpenCensus Python](https://github.com/census-instrumentation/opencensus-python)

---

**Created**: October 21, 2025  
**Resource Group**: azure_ai_rg  
**Application Insights**: func-otis-rag-insights  
**Function App**: func-otis-rag
