import os
import re
import sys
from pathlib import Path

def search_file_in_directory(file_name, search_directory):
    """
    Search for a file name in all files within the specified directory and subdirectories.
    
    Args:
        file_name (str): The file name to search for
        search_directory (str): The root directory to search in
    
    Returns:
        list: List of file paths that match the file name
    """
    results = []
    search_path = Path(search_directory)
    
    if not search_path.exists():
        print(f"Error: Directory '{search_directory}' does not exist.")
        return results
    
    # Common file extensions for COBOL and related files
    file_extensions = ['.cbl', '.cob', '.cobol', '.cpy', '.copy', '.txt', '.dat', '.cfg', '.ctl']
    
    print(f"Searching for '{file_name}' in {search_directory}...")
    print("-" * 60)
    
    file_count = 0
    
    # Walk through all directories and subdirectories
    for file_path in search_path.rglob('*'):
        if file_path.is_file():
            # Check if file has relevant extension or no extension
            if file_path.suffix.lower() in file_extensions or file_path.suffix == '':
                file_count += 1
                try:
                    with open(file_path, 'r', encoding='utf-8', errors='ignore') as file:
                        for line_num, line in enumerate(file, 1):
                            # Search for program name (case-insensitive)
                            if re.search(rf'\b{re.escape(file_name)}\b', line, re.IGNORECASE):
                                results.append((str(file_path), line_num, line.strip()))
                                # Return immediately after finding the first match
                                print(f"Searched {file_count} files.")
                                return results
                except Exception as e:
                    # Skip files that can't be read
                    continue
    
    print(f"Searched {file_count} files.")
    return results

def main():
    """Main function to get user input and perform search."""
    
    print("COBOL Program Search Tool")
    print("=" * 30)
    
    # Check if program name was provided as command line argument
    if len(sys.argv) > 1:
        program_name = sys.argv[1].strip()
        print(f"Searching for program: {program_name}")
    else:
        print("Usage: python \"Search S35.py\" <program_name>")
        print("Example: python \"Search S35.py\" LPLCAP")
        print("\nNo program name provided. Exiting...")
        return
    
    if not program_name:
        print("Error: Please enter a valid program name.")
        return
    
    # Define the search directory
    search_directory = r"C:\Users\jeff.childers\Documents\COBOL Documentation\S35-Source"
    
    # Perform the search
    results = search_file_in_directory(program_name, search_directory)
    
    # Display results
    if results:
        print(f"\nFound first occurrence of '{program_name}':")
        print("=" * 60)
        
        file_path, line_num, line_content = results[0]
        print(f"📁 File: {file_path}")
        print(f"   Line {line_num:4d}: {line_content}")
        
        print("\n" + "=" * 60)
        print("SUMMARY:")
        print(f"📄 {file_path} (first occurrence)")
            
    else:
        print(f"\n❌ No occurrences of '{program_name}' found in {search_directory}")
        print("\nSuggestions:")
        print("- Check the spelling of the program name")
        print("- Try searching for a partial name")
        print("- The program might be in a different directory")

if __name__ == "__main__":
    main()