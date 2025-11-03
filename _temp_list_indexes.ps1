python load_search_env.py --ps | Invoke-Expression
$env:SEARCH_ENDPOINT = $env:AZURE_SEARCH_ENDPOINT
$env:SEARCH_KEY = $env:AZURE_SEARCH_KEY
$ks = [bool]$env:AZURE_SEARCH_KEY
Write-Host "Endpoint: $($env:AZURE_SEARCH_ENDPOINT)"
Write-Host "Key set: $ks"
$uri = "$($env:AZURE_SEARCH_ENDPOINT)/indexes?api-version=2024-07-01"
try {
  $resp = Invoke-RestMethod -Method Get -Uri $uri -Headers @{'api-key'=$env:AZURE_SEARCH_KEY}
  Write-Host 'First indexes:'
  $resp.value | Select-Object -First 10 -ExpandProperty name | ForEach-Object { Write-Host ' -' $_ }
} catch {
  Write-Host 'List failed:' $_ -ForegroundColor Red
}
