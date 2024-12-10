# Function:  Remove all formatting from a Prolog program

# Author:    Claude AI

# Caution:   There may be a problem with specific file names embedded in long predicates.

import re
import sys
import os

def process_prolog_code(prolog_code):
    # Regular expression pattern to match filenames ending with a quote, strip periods
    pattern = r'([a-zA-Z0-9]+(?:\.[a-zA-Z0-9]+)+)\''

	# Function to replace matched filenames
    def replace_filename(match):
        return re.sub(r'\.', '', match.group(1)) + "'"

    # Use re.sub to find and replace all matching filenames
    converted_filenames = re.sub(pattern, replace_filename, prolog_code)
    
    # Remove single-line comments
    code_without_single_line = re.sub(r'%.*$', '', converted_filenames, flags=re.MULTILINE)
    
    # Remove multi-line comments
    code_without_comments = re.sub(r'/\*[\s\S]*?\*/', '', code_without_single_line)
    
    # Split code into predicate segments
    predicates = re.split(r'(?<!\d)\.(?!\d)', code_without_comments)
    
    processed_predicates = []
    for predicate in predicates:
        # Remove all whitespace
        cleaned_predicate = re.sub(r'\s+', '', predicate)
        if cleaned_predicate:
            # Add the period back to the end of the predicate
            processed_predicates.append(cleaned_predicate + '.')
    
    # Join predicates with newlines
    return '\n'.join(processed_predicates)

def get_output_filename(input_filename):
    base, ext = os.path.splitext(input_filename)
    return f"{base}_processed{ext}"

def main():
    if len(sys.argv) != 2:
        print("Usage: python script.py <input_file>")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = get_output_filename(input_file)

    try:
        with open(input_file, 'r') as file:
            prolog_code = file.read()
    except FileNotFoundError:
        print(f"Error: File '{input_file}' not found.")
        sys.exit(1)
    except IOError:
        print(f"Error: Unable to read file '{input_file}'.")
        sys.exit(1)

    processed_code = process_prolog_code(prolog_code)
    
    try:
        with open(output_file, 'w') as file:
            file.write(processed_code)
        print(f"Processed Prolog code has been written to '{output_file}'")
    except IOError:
        print(f"Error: Unable to write to file '{output_file}'.")
        sys.exit(1)

if __name__ == "__main__":
    main()
