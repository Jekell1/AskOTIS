import os
import re
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
    
    print(f"Searching for file '{file_name}' in {search_directory}...")
    print("-" * 60)
    
    file_count = 0
    
    # Walk through all directories and subdirectories
    for file_path in search_path.rglob('*'):
        if file_path.is_file():
            file_count += 1
            # Check if the file name matches exactly (case-insensitive)
            if file_name.lower() == file_path.name.lower():
                results.append(str(file_path))
    
    print(f"Searched {file_count} files.")
    return results

def search_file_silent(file_name, search_directory=None):
    """
    Search for a file name silently without printing progress.
    
    Args:
        file_name (str): The file name to search for
        search_directory (str): The root directory to search in. 
                               Defaults to S35-Source if not provided.
    
    Returns:
        list: List of file paths that match the file name
    """
    if search_directory is None:
        search_directory = r"C:\Users\jeff.childers\Documents\COBOL Documentation\S35-Source"
    
    results = []
    search_path = Path(search_directory)
    
    if not search_path.exists():
        return results
    
    # Walk through all directories and subdirectories
    for file_path in search_path.rglob('*'):
        if file_path.is_file():
            # Check if the file name matches exactly (case-insensitive)
            if file_name.lower() == file_path.name.lower():
                results.append(str(file_path))
    
    return results

def search_and_display(file_name, search_directory=None):
    """
    Search for a file and display results with formatting.
    
    Args:
        file_name (str): The file name to search for
        search_directory (str): The root directory to search in.
                               Defaults to S35-Source if not provided.
    
    Returns:
        list: List of file paths that match the file name
    """
    if search_directory is None:
        search_directory = r"C:\Users\jeff.childers\Documents\COBOL Documentation\S35-Source"
    
    # Perform the search
    results = search_file_in_directory(file_name, search_directory)
    
    # Display results
    if results:
        print(f"\nFound {len(results)} file(s) matching '{file_name}':")
        print("=" * 60)
        
        for i, file_path in enumerate(results, 1):
            print(f"{i:2d}. {file_path}")
        
        print("\n" + "=" * 60)
        print("SUMMARY:")
        print(f"📁 Search Directory: {search_directory}")
        print(f"🔍 Search Term: {file_name}")
        print(f"📄 Files Found: {len(results)}")
            
    else:
        print(f"\n❌ No files matching '{file_name}' found in {search_directory}")
        print("\nSuggestions:")
        print("- Check the spelling of the file name")
        print("- Try searching for a partial name")
        print("- The file might be in a different directory")
        print("- Try searching without the file extension")
    
    return results

def main(file_name=None):
    """
    Main function to get user input and perform search.
    
    Args:
        file_name (str, optional): File name to search for. If not provided, 
                                  prompts user for input.
    """
    
    if file_name is None:
        # Ask user for file name
        print("COBOL File Search Tool")
        print("=" * 30)
        file_name = input("Enter the file name to search for: ").strip()
        
        if not file_name:
            print("Error: Please enter a valid file name.")
            return []
    
    return search_and_display(file_name)

if __name__ == "__main__":
    main()
