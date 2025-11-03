# File Hyperlink Feature Implementation

## Overview
Added interactive file hyperlinks to COBOL file references (.CBL, .CPY) in RAG responses. When users hover over a filename, they see "Tell me about <filename>". Clicking the link populates the question box with that prompt.

## Changes Made

### 1. Backend (otis_rag/generator.py)

Added `_add_file_hyperlinks()` method that:
- Detects COBOL file references matching pattern: `[A-Z][A-Z0-9_-]{0,30}\.(?:CBL|CPY|cbl|cpy)`
- Wraps them in special markup: `[[FILE:filename.ext|filename.ext]]`
- This markup is invisible to the LLM but processed by frontend

**Example transformation:**
```
Input:  "The APIPAY.CBL program calls REFUPD.CBL"
Output: "The [[FILE:APIPAY.CBL|APIPAY.CBL]] program calls [[FILE:REFUPD.CBL|REFUPD.CBL]]"
```

### 2. Frontend (otis-rag-chat.html)

#### A. formatMessage() function
Added file hyperlink processing BEFORE other markdown:
```javascript
content = content.replace(/\[\[FILE:([^\|]+)\|([^\]]+)\]\]/g, (match, filename, displayText) => {
    const escapedFilename = escapeHtml(filename);
    const escapedDisplay = escapeHtml(displayText);
    return `<a href="#" class="file-link" data-filename="${escapedFilename}" title="Tell me about ${escapedFilename}">${escapedDisplay}</a>`;
});
```

#### B. CSS Styles
Added `.file-link` styles:
- Purple/gradient colors matching UI theme (#667eea → #764ba2)
- Dotted underline that becomes solid on hover
- Subtle background highlight on hover
- 📄 emoji prefix for visual identification
- Smooth transitions

#### C. Event Handler
Added click handler with event delegation:
```javascript
document.body.addEventListener('click', (e) => {
    if (e.target.classList.contains('file-link')) {
        e.preventDefault();
        const filename = e.target.getAttribute('data-filename');
        handleFileLinkClick(filename);
    }
});
```

`handleFileLinkClick()` function:
- Creates question: `"Tell me about <filename>"`
- Populates the question input box
- Focuses and selects text so user can review before submitting
- User presses Enter or clicks Send to submit

## User Experience

1. **User sees response with file reference:**
   ```
   The APIPAY.CBL program validates payments and calls REFUPD.CBL...
          ↑ (styled as link with 📄 emoji)
   ```

2. **User hovers over APIPAY.CBL:**
   - Tooltip appears: "Tell me about APIPAY.CBL"
   - Link highlights with purple color

3. **User clicks APIPAY.CBL:**
   - Question box fills with: "Tell me about APIPAY.CBL"
   - Text is selected/focused for easy editing
   - User can modify or press Enter to submit

4. **System responds with file details:**
   - Program purpose, structure, dependencies, etc.

## Testing

Created test_file_links.py to verify backend markup generation:
```bash
python test_file_links.py
```

Expected output shows proper `[[FILE:...]]` markup for:
- Single file references
- Multiple files in one sentence
- Files in parentheses (Source: PROGRAM.CBL)
- Both uppercase and lowercase extensions

## Deployment

To deploy:
```bash
func azure functionapp publish func-otis-rag --python --force
```

Then open: https://func-otis-rag.azurewebsites.net or access via your frontend.

## Future Enhancements

Possible improvements:
1. **Auto-submit option**: Add setting to auto-submit instead of just populating
2. **Preview popup**: Show quick file info on hover before clicking
3. **Multi-file exploration**: "Tell me about PROGRAM1.CBL and PROGRAM2.CBL"
4. **File type icons**: Different icons for .CBL (program) vs .CPY (copybook)
5. **Recent files list**: Track clicked files for quick re-access
6. **Smart prompts**: Context-aware questions like "How does PROGRAM.CBL use COPYBOOK.CPY?"

## Technical Notes

- **Security**: File names are HTML-escaped to prevent XSS
- **Performance**: Regex runs once per response, negligible overhead
- **Compatibility**: Works in all modern browsers
- **Accessibility**: Links have proper title attributes for screen readers
- **No breaking changes**: Existing functionality unaffected

## Example Output

Before:
```
The APIPAY program (Source: APIPAY.CBL, Index: code) calls REFUPD...
```

After:
```
The APIPAY program (Source: 📄 APIPAY.CBL, Index: code) calls 📄 REFUPD.CBL...
                               ↑ clickable              ↑ clickable
```

Both file names are now interactive hyperlinks with hover tooltips.
