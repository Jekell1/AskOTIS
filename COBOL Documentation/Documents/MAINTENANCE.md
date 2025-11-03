# COBOL Documentation Maintenance Guide

This guide explains how to update and maintain the static HTML documentation system.

## Quick Update

The easiest way to regenerate the documentation after making changes:

1. **Double-click `regenerate_docs.bat`** - This will automatically scan for new files and regenerate all HTML
2. **Or run manually**: `python generate_static_html.py`

## How the Auto-Detection Works

The generator script now automatically discovers your documentation files and processes external diagram references:

### Markdown Documentation Files
- **Location**: `Documents/` folder
- **Pattern**: Files ending with `_Documentation.md`
- **Example**: `APIPAY_Documentation.md`, `NEWPROGRAM_Documentation.md`

### Mermaid Diagram Files  
- **Location**: `Documents/Diagrams/` folder
- **Pattern**: Files ending with `.mmd`
- **Example**: `APIPAY_Transaction_Flow.mmd`, `NEWPROGRAM_Workflow.mmd`

### External Diagram References
- **Pattern**: `[Title](Diagrams/filename.mmd)` in Markdown files
- **Processing**: Automatically detects links to `.mmd` files and embeds them inline
- **Result**: Interactive Mermaid diagrams display directly in the documentation pages

### Example External Reference Processing
```markdown
### [Transaction Processing Flow](Diagrams/APIPAY_Transaction_Processing.mmd)
```
This gets automatically converted to:
```markdown
### Transaction Processing Flow

\`\`\`mermaid
graph TD
  A[Start] --> B[Process]
  B --> C[End]
\`\`\`
```

## Adding New Documentation

### Adding a New Program

1. **Create the Markdown file**: `NEWPROGRAM_Documentation.md` in the `Documents/` folder
2. **Add diagrams** (optional): Create `.mmd` files in `Documents/Diagrams/`
3. **Regenerate**: Run `regenerate_docs.bat` or `python generate_static_html.py`

The script will automatically:
- Detect the new program
- Create appropriate HTML files
- Update the main index with links
- Include any related diagrams

### Program Metadata

For known programs (APIPAY, LONPFB, LONPFC, etc.), the script uses predefined metadata (icons, descriptions, tags). For new programs, it will:
- Use a default 📄 icon
- Generate a basic title and description
- Assign a "Documentation" tag

To customize metadata for new programs, edit the `program_metadata` dictionary in `generate_static_html.py`.

## File Structure

```
Documents/
├── generate_static_html.py      # Generator script
├── regenerate_docs.bat          # Easy regeneration script
├── view_documentation.bat       # Opens the documentation
├── index.html                   # Main documentation index (generated)
├── diagrams.html               # Diagram viewer (generated)
├── APIPAY_Documentation.md     # Source markdown
├── APIPAY_Documentation.html   # Generated HTML
├── LONPFC_Documentation.md     # Source markdown  
├── LONPFC_Documentation.html   # Generated HTML
├── ... (other program files)
└── Diagrams/
    ├── APIPAY_Transaction_Processing.mmd
    ├── LONPFC_Main_Flow.mmd
    └── ... (other diagram files)
```

## Generated Files

The script generates these HTML files:
- `index.html` - Main documentation portal
- `diagrams.html` - Interactive diagram viewer (if diagrams exist)
- `[PROGRAM]_Documentation.html` - Individual program documentation pages

## Tips for Maintenance

### Best Practices
1. **Consistent naming**: Use `PROGRAMNAME_Documentation.md` format
2. **Regular updates**: Run regeneration after any content changes
3. **Backup originals**: Keep your `.md` and `.mmd` files safe - HTML files can be regenerated

### Troubleshooting
- **Missing programs**: Ensure Markdown files follow the `*_Documentation.md` naming pattern
- **Missing diagrams**: Check that `.mmd` files are in the `Documents/Diagrams/` folder
- **External diagrams not showing**: Verify the reference format `[Title](Diagrams/filename.mmd)` in Markdown
- **Diagrams not rendering**: Ensure you have an internet connection (Mermaid loads from CDN)
- **Python errors**: Ensure the `markdown` module is installed (`pip install markdown`)

### Customization
- **Styling**: Edit the CSS in `generate_static_html.py`
- **Layout**: Modify the HTML templates in the script
- **Metadata**: Update the `program_metadata` dictionary for new programs

## Dependencies

- **Python 3.x**: Required to run the generator
- **Markdown module**: Install with `pip install markdown`
- **Modern web browser**: For viewing the generated HTML

## Automation Options

For automatic regeneration when files change, you could:
1. Set up a file watcher script
2. Use a scheduled task to run `regenerate_docs.bat` daily
3. Integrate with version control hooks

The current system prioritizes simplicity - just run the batch file when you need updates!
