$file = "c:\Users\jeff.childers\Documents\OTISCodeResearcher\otis-rag-chat.html"
$content = Get-Content $file -Raw

# Find the Session section and add Export section after it
$pattern = '(<div class="sidebar-section">\s*<h3>💾 Session</h3>[\s\S]*?</div>)'
$replacement = '$1

                <div class="sidebar-section">
                    <h3>📥 Export Conversation</h3>
                    <button class="example-btn" onclick="exportToExcel()" style="background: linear-gradient(135deg, #10b981 0%, #059669 100%); border: none; color: white; font-weight: 600; margin-bottom: 8px;">
                        📊 Export to Excel
                    </button>
                    <button class="example-btn" onclick="exportToWord()" style="background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%); border: none; color: white; font-weight: 600;">
                        📄 Export to Word
                    </button>
                    <div style="margin-top: 8px; font-size: 11px; color: #6b7280;">
                        Export your conversation history
                    </div>
                </div>'

$newContent = $content -replace $pattern, $replacement

Set-Content $file $newContent -NoNewline

Write-Host "Successfully added export buttons!" -ForegroundColor Green
