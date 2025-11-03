import re

def extract_program_names(md_path, section_name):
    with open(md_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    # Find the section
    section_start = None
    for i, line in enumerate(lines):
        if line.strip().lower() == f'#### {section_name.lower()}':
            section_start = i
            break
    if section_start is None:
        print(f"Section '{section_name}' not found.")
        return

    # Collect program names until next section or end
    program_names = []
    for line in lines[section_start+1:]:
        if line.strip().startswith('####') or line.strip().startswith('---'):
            break
        match = re.match(r'-?\s*\*?\*?\[?([A-Z0-9\-]+)\]?\*?\*?\s*-', line)
        if not match:
            match = re.match(r'-?\s*([A-Z0-9\-]+)\s*-', line)
        if match:
            program_names.append(match.group(1))
    print(', '.join(program_names))

if __name__ == "__main__":
    # Example usage
    extract_program_names(r".\Documents\APIPAY_Documentation.md", "File Management - Open Operations")
