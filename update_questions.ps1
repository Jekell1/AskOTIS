$file = "c:\Users\jeff.childers\Documents\OTISCodeResearcher\otis-rag-chat.html"
$content = Get-Content $file -Raw

# Read the new function from the JS file
$newFunction = Get-Content "c:\Users\jeff.childers\Documents\OTISCodeResearcher\updated_question_generator.js" -Raw

# Find the start and end of the function
$startPattern = 'function generateRandomQuestions\(count = 500\) \{'
$endPattern = 'return Array\.from\(questions\);\s+\}'

# Use regex to replace
$pattern = "$startPattern[\s\S]*?$endPattern"
$replacement = $newFunction.Trim()

$newContent = $content -replace $pattern, $replacement

# Write back
Set-Content $file $newContent -NoNewline

Write-Host "Successfully updated generateRandomQuestions function!" -ForegroundColor Green
