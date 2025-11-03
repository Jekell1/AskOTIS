import os
import re

def extract_section(pattern, code, flags=0):
    match = re.search(pattern, code, flags)
    return match.group(1).strip() if match else None

def get_existing_filenames(directory):
    file_names = input(f"Enter base file names (comma-separated, e.g., C-DATER-MAIN,C-TIMEND) to use for documentation in {directory}: ")
    return [name.strip() for name in file_names.split(',') if name.strip()]

def get_prompt(prompt_file):
    with open(prompt_file, 'r', encoding='utf-8') as f:
        return f.read().strip()

def main():
    docs_dir = r"C:\Users\jeff.childers\Documents\COBOL Documentation\Documents"
    apipay_dir = r"C:\Users\jeff.childers\Documents\COBOL Documentation\APIPAY"
    base_dir = os.path.dirname(docs_dir)
    prompt_file = os.path.join(base_dir, "prompt.txt")
    while True:
        file_names = get_existing_filenames(docs_dir)
        # Clear terminal after file name entry
        os.system('cls' if os.name == 'nt' else 'clear')
        file_names_str = ','.join(file_names)
        for file_name in file_names:
            prompt = get_prompt(prompt_file)
            prompt_with_filename = prompt.replace("{file_name}", file_names_str)
            doc_file_name = f"{file_name}_Documentation.md"
            doc_file_path = os.path.join(docs_dir, doc_file_name)
            cobol_src_path = os.path.join(apipay_dir, "APIPAY_Inlined.CBL")
            if os.path.isfile(doc_file_path):
                response = input(f"Documentation file '{doc_file_name}' already exists. Delete and recreate? (y/n): ").strip().lower()
                if response == 'y':
                    os.remove(doc_file_path)
                    print(f"Deleted '{doc_file_name}'. You can now recreate it.")
                else:
                    print(f"Keeping existing file '{doc_file_name}'.")
            else:
                print(f"Documentation file '{doc_file_name}' does not exist. Proceeding to create it.")
            print("\n--- Prompt with file name ---\n")
            print(prompt_with_filename)
            print("\n--- End of prompt ---\n")
            print(f"Selected file: {file_name}")
        cont = input("Do you want to process another file? (y/n): ").strip().lower()
        if cont != 'y':
            print("Exiting loop.")
            break

if __name__ == "__main__":
    main()
