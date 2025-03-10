#!/usr/bin/env python3

import sys
import argparse
import os
import pandas as pd
import numpy as np
import time
import re
import glob

# 🚨 IMPORTANT: DO NOT REMOVE ANY FUNCTION FROM THIS SCRIPT 🚨
# The following **20 key features** must always be retained:
# 1️⃣ Preserve list-like values (`[ ]`).
# 2️⃣ Save **original** column list before filtering and log it.
# 3️⃣ Remove **columns with only one unique value** and log them separately.
# 4️⃣ Log min/max values for each column.
# 5️⃣ Save **final** column list after filtering and log it separately.
# 6️⃣ Ensure mixed-type columns are treated as strings and log it.
# 7️⃣ Process all CSV files in directories.
# 8️⃣ Generate unique predicate names based on file paths and log them.
# 9️⃣ Save and log Prolog facts and column metadata.
# 🔟 Save and log output to a user-defined directory (`--tree`).
# 1️⃣1️⃣ Track and log separate times for file reading, type analysis, and file saving.
# 1️⃣2️⃣ Ensure `log_file` is always used.
# 1️⃣3️⃣ Retain `export_csv_to_prolog()` for full file processing.
# 1️⃣4️⃣ Keep `__main__` intact for CLI execution.
# 1️⃣5️⃣ **Sort files by size before processing.**
# 1️⃣6️⃣ **Detect and handle file read errors**.
# 1️⃣7️⃣ **Skip empty CSV files**.
# 1️⃣8️⃣ **Ensure non-existing directories are created**.
# 1️⃣9️⃣ **Use `low_memory=False` in Pandas to prevent incorrect column detection**.
# 2️⃣0️⃣ **Log original, removed, and final columns properly**.


def format_time(seconds):
    if seconds < 90:
        return f"{seconds:.2f}s"
    elif seconds < 3600:
        return f"{seconds / 60:.1f} min"
    else:
        h, rem = divmod(seconds, 3600)
        m, s = divmod(rem, 60)
        return f"{int(h):02d}:{int(m):02d}:{int(s):02d}"

def invert_quotes(input_str):
    import ast

    try:
        # Parse the input string safely into a Python list
        parsed_list = ast.literal_eval(input_str)

        # Convert the Python list to a string representation with single quotes
        single_quote_str = str(parsed_list)

        return single_quote_str
    except (SyntaxError, ValueError):
        # Return the original string if there is a syntax error
        return input_str


def local_repr_slow(value):
    """Return a Prolog-friendly representation. Use repr() unless it's a string starting with '['."""
    
    if isinstance(value, str):
        if value.startswith("[") and value.endswith("]"):
            return invert_quotes(value)  # Print lists directly
        return repr(value)
    if isinstance(value, list):
        return repr(value)
    if (value > 100000):
        if not isinstance(value, float):
            return f"{value:_}"
    return repr(value)


def format_prolog_fact_slow(predicate, args):
    """Formats a Prolog fact with the given predicate and arguments using local_repr."""
    args_str = ", ".join(map(local_repr_slow, args))  # Ensure strings/lists are properly formatted
    return f"{predicate}({args_str}).\n"


def local_repr_fast(value):
    """Return a Prolog-friendly representation. Use repr() unless it's a string starting with '['."""
    if isinstance(value, str):
        if value.startswith("[") and value.endswith("]"):
            return invert_quotes(value)  # Print lists directly
    return repr(value)


def format_prolog_fact(predicate, args):
    """Formats a Prolog fact with the given predicate and arguments using local_repr."""
    args_str = ", ".join(map(local_repr_fast, args))  # Ensure strings/lists are properly formatted
    return f"{predicate}({args_str}).\n"



def simplify_dtype(dtype):
    """Convert Pandas dtype to a simplified Prolog-friendly type."""
    if np.issubdtype(dtype, np.integer):
        return "int"
    elif np.issubdtype(dtype, np.floating):
        return "float"
    elif np.issubdtype(dtype, np.object_) or np.issubdtype(dtype, str):
        return "str"
    else:
        return "unknown"

def sanitize_predicate_name(name):
    """Sanitize a string to make it a valid Prolog predicate name."""
    name = name.lower().replace("-", "_").replace(" ", "_")  # Normalize dashes/spaces
    name = re.sub(r"[^a-zA-Z0-9_]", "", name)  # Remove invalid characters
    return name

def compute_predicate_name(input_path, common_path):
    """Compute a unique Prolog predicate name based on file path, directory, and label column."""
    rel_path = os.path.relpath(input_path, start=common_path)  # Get relative path
    rel_path_parts = os.path.normpath(rel_path).split(os.sep)  # Split directories
    base_name = os.path.splitext(rel_path_parts[-1])[0]  # Get file base name

    predicate_name = sanitize_predicate_name(base_name)  # Start with sanitized base name

    # Prepend missing directory names if not already part of base name
    for part in reversed(rel_path_parts[:-1]):  # Ignore the file itself
        part_clean = sanitize_predicate_name(part)
        if part_clean and part_clean not in predicate_name:
            predicate_name = f"{part_clean}_{predicate_name}"

    return predicate_name

def analyze_columns(df):  # 2️⃣ 3️⃣ 4️⃣ 5️⃣ 6️⃣ 1️⃣1️⃣
    """Analyze columns, compute min/max values, remove redundant columns, and log results."""
    
    start_time = time.time()  # 1️⃣1️⃣ Start timing for type analysis
    original_columns = df.columns.tolist()  # 2️⃣ Save original column list
    unique_counts = df.nunique(dropna=True)
    dtypes = df.dtypes

    column_info = []
    removed_columns = [col for col in df.columns if df[col].nunique(dropna=True) == 1]

    for col in df.columns:
        if col in removed_columns:
            single_value = df[col].dropna().iloc[0] if not df[col].dropna().empty else None
            type_info = type(single_value).__name__ if single_value is not None else "unknown"
            column_info.append((col, 1, type_info, single_value, single_value))
            continue

        numeric_values = pd.to_numeric(df[col], errors="coerce")

        if numeric_values.notna().all():
            if np.all(numeric_values % 1 == 0):
                df[col] = numeric_values.astype(int)
                type_info = "int"
            else:
                df[col] = numeric_values.astype(float)
                type_info = "float"
            min_value, max_value = df[col].min(), df[col].max()
        else:
            # Cache string conversion once
            string_values = df[col].dropna().astype(str)

            if string_values.str.match(r"^\[.*\]$").any():
                type_info = "list"
            else:
                type_info = "str"

            min_value = string_values.min()
            max_value = string_values.max()

        column_info.append((col, unique_counts[col], type_info, min_value, max_value))


    final_columns = [col for col in original_columns if col not in removed_columns]  # 5️⃣ Save final column list
    elapsed_time = time.time() - start_time  # 1️⃣1️⃣ End timing for type analysis
    return column_info, elapsed_time, original_columns, removed_columns, final_columns

def log_column_analysis(log_file, predicate_name, input_file, total_rows, column_info, original_columns, removed_columns, final_columns):
    """Writes column analysis as Prolog facts to the log file, prints to the console, and returns the complete analysis as a string."""

    analysis_lines = []

    analysis_intro = f"% Analysis for {input_file}"
    analysis_lines.append(analysis_intro)

    source_file_fact = format_prolog_fact_slow("analysis_source_file", [predicate_name, input_file, total_rows]).strip()
    analysis_lines.append(source_file_fact)

    original_columns_fact = format_prolog_fact_slow("original_columns", [predicate_name, original_columns]).strip()
    analysis_lines.append(original_columns_fact)

    removed_columns_fact = format_prolog_fact_slow("removed_columns", [predicate_name, removed_columns]).strip()
    analysis_lines.append(removed_columns_fact)

    final_columns_fact = format_prolog_fact_slow("final_columns", [predicate_name, final_columns]).strip()
    analysis_lines.append(final_columns_fact)

    for col, num_unique, type_info, min_value, max_value in column_info:
        column_fact = format_prolog_fact_slow("analysis_column", [predicate_name, col, num_unique, type_info, min_value, max_value]).strip()
        analysis_lines.append(column_fact)

    # Return the analysis lines
    return analysis_lines


def process_directory(input_directory):
    """Recursively find all CSV files in the directory and sort them by size."""
    csv_files = []
    for root, _, files in os.walk(input_directory):
        for file in files:
            if file.endswith(".csv"):
                full_path = os.path.join(root, file)
                csv_files.append(full_path)

    # Sort CSV files by size (smallest first)
    csv_files.sort(key=os.path.getsize)
    return csv_files
    
import shutil    
def export_csv_to_prolog(input_csv, output_base_dir, log_file, common_path, clobber=False):
    print(f"\n\n\n----------------------------------------------------------------------")

    # Compute Prolog predicate name
    predicate_name = compute_predicate_name(input_csv, common_path)

    # Preserve directory structure
    relative_path = os.path.relpath(input_csv, common_path)
    relative_dir = os.path.dirname(relative_path)
    output_dir = os.path.join(output_base_dir, relative_dir)
    os.makedirs(output_dir, exist_ok=True)

    output_pl = os.path.join(output_dir, os.path.splitext(os.path.basename(input_csv))[0] + "_mw.pl")

    print(f"📊 Starting '{input_csv}'\n\t -> '{predicate_name}'\n\t  -> '{output_pl}'\n\n\n")

    if not clobber and os.path.exists(output_pl):
        print(f"⚠️  Skipping existing file (use --clobber to overwrite): {output_pl}")

    overall_start_time = time.time()

    # Timing file read
    read_start = time.time()
    try:
        df = pd.read_csv(
            input_csv,
            sep="|",
            on_bad_lines="warn",
            encoding="utf-8",
            dtype=str,
            low_memory=False
        )
    except Exception as e:
        print(f"⚠️  Skipping file '{input_csv}' due to read error: {e}")
        return

    read_time = time.time() - read_start

    if df.empty:
        print(f"⚠️  Skipping empty file: {input_csv}")
        return

    # Timing column analysis
    analysis_start = time.time()
    column_info, analysis_elapsed, original_columns, removed_columns, final_columns = analyze_columns(df)
    analysis_time = time.time() - analysis_start

    total_rows = len(df)

    # Timing log writing separately
    log_start = time.time()

    column_names_before = df.columns.tolist()
    previous_schema_fact = format_prolog_fact_slow("previous_predicate_schema", [predicate_name, f"[{', '.join(column_names_before)}]"])

    single_value_columns = [col for col, num_unique, *_ in column_info if num_unique == 1]
    df.drop(columns=single_value_columns, inplace=True)

    column_names_after = df.columns.tolist()
    schema_fact = format_prolog_fact_slow("predicate_schema", [predicate_name, f"[{', '.join(column_names_after)}]"])

    analysis_lines = log_column_analysis(log_file, predicate_name, input_csv, total_rows, column_info, original_columns, removed_columns, final_columns)

    with open(log_file, "a", encoding="utf-8") as log:
        log.write(previous_schema_fact)
        # Join the lines into a single string block
        log.write("\n".join(analysis_lines) + "\n")
        log.write(schema_fact)

    print(f"Column heuristics logged in '{log_file}'.")

    log_time = time.time() - log_start


    print(f"% {previous_schema_fact}")
    print("\n% ".join(analysis_lines)+"\n")
    print(f"% {schema_fact}")


    # Timing Prolog file saving (excluding logging)
    save_start = time.time()

    total_rowsM1 = total_rows - 1
    tmp_output = output_pl + ".tmp"

    with open(tmp_output, "w", encoding="utf-8") as pl_file:
        pl_file.write(f"% {previous_schema_fact}\n")
        pl_file.write("/*" + "\n".join(analysis_lines) + "*/\n")
        pl_file.write(f"% {schema_fact}\n")

        for i, (_, row) in enumerate(df.iterrows()):
            fact = format_prolog_fact(predicate_name, row.tolist())
            pl_file.write(fact)
            if i < 3 or i == total_rowsM1:
                print(fact.strip())
            elif i == 3:
                print(f"...{(total_rows - 4):_} more...")

    # Atomically move temporary file to final destination
    shutil.move(tmp_output, output_pl)

    save_time = time.time() - save_start


    total_time = time.time() - overall_start_time

    # Detailed Timing Statistics
    print("\n📊 Detailed Timing Statistics:")
    print(f"- File Reading Time:\t {format_time(read_time)}")
    print(f"- Column Analysis Time:\t {format_time(analysis_time)}")
    print(f"- Drop+Log Time:\t {format_time(log_time)}")
    print(f"- Output Saving Time:\t {format_time(save_time)}")
    print(f"- Total Processing Time:\t {format_time(total_time)}\n")

    print(f"✅ Processed '{input_csv}' -> '{output_pl}'")
    print(f"📄 Exported Prolog facts to '{output_pl}' (Columns omitted: {len(single_value_columns)})")

import os
import time
import pandas as pd
import shutil

def track_time(func):
    def wrapper(*args, **kwargs):
        start_time = time.time()
        result = func(*args, **kwargs)
        elapsed_time = time.time() - start_time
        print(f"⏱️ Time spent in {func.__name__}: {elapsed_time:.2f}s")
        return result
    return wrapper

@track_time
def export_csv_to_edge_files(input_csv, output_base_dir, common_path):
    print(f"\n\n\n----------------------------------------------------------------------")
    print(f"📊 Processing '{input_csv}'")
    
    if "nodes" in input_csv.lower():
       print(f"⚠️  Skipping file '{input_csv}' as it appears to be a node file.")
       return

    # Read CSV file
    try:
        df = pd.read_csv(
            input_csv,
            sep="|",
            on_bad_lines="warn",
            encoding="utf-8",
            dtype=str,
            low_memory=False
        )
        print(f"✅ Read file '{input_csv}'")
    except Exception as e:
        print(f"⚠️  Skipping file '{input_csv}' due to read error: {e}")
        return
    
    if df.empty or "source_id" not in df.columns or "target_id" not in df.columns:
        print(f"⚠️  Skipping file '{input_csv}' due to missing 'source_id' or 'target_id' columns.")
        return
    
    link_name = df["label"].iloc[0] if "label" in df.columns and not df["label"].empty else "unknown"
    df = df.filter(items=["source_id", "target_id"], axis=1)
    
    if df.empty:
        print(f"⚠️  Skipping file '{input_csv}' after filtering.")
        return
    
    output_links_dir = os.path.join(output_base_dir, "global_links")
    os.makedirs(output_links_dir, exist_ok=True)
    
    output_file = os.path.join(output_links_dir, f"{link_name}.csv")
    df.to_csv(output_file, sep="|", index=False, header=False, mode='a')
    print(f"✅ Appended edges to '{output_file}'")

#⏱️ Time spent in deduplicate_edge_files: 302.18s
import os
import time
import numpy as np
import pandas as pd
@track_time
def deduplicate_edge_files(output_base_dir, write_prolog=True):
    """
    Deduplicates CSV files, analyzes them, and optionally writes Prolog files.

    Parameters:
    - output_base_dir (str): Base directory containing 'global_links'.
    - write_prolog (bool): Whether to write Prolog files.
    """
    output_links_dir = os.path.join(output_base_dir, "global_links")
    analysis_lines = []

    for file in os.listdir(output_links_dir):
        if file.endswith(".csv"):
            file_path = os.path.join(output_links_dir, file)
            prolog_file_path = file_path.replace(".csv", ".pl")  # Create Prolog filename
            predicate_name = os.path.splitext(file)[0]  # Use file name without extension as predicate

            try:
                # Track time for reading CSV
                start_time = time.time()
                df = pd.read_csv(file_path, sep="|", names=["source_id", "target_id"], dtype=str, on_bad_lines="warn")
                load_time = time.time() - start_time
                print(f"⏱️ Loaded '{file_path}' in {load_time:.2f}s")

                # Track time for deduplication
                start_time = time.time()
                df.drop_duplicates(inplace=True)
                df = df.dropna()
                df = df[df.apply(lambda row: len(row) == 2, axis=1)]  # Ensure exactly two columns
                dedupe_time = time.time() - start_time
                print(f"⏱️ Deduplicated '{file_path}' in {dedupe_time:.2f}s")

                # Track time for writing deduplicated CSV
                start_time = time.time()
                df.to_csv(file_path, sep="|", index=False, header=False)  # No header
                csv_write_time = time.time() - start_time
                print(f"⏱️ Wrote deduplicated CSV '{file_path}' in {csv_write_time:.2f}s")

                # Count unique values
                start_time = time.time()
                unique_counts = df.nunique(dropna=True)

                column_info = []
                for col in df.columns:
                    numeric_values = pd.to_numeric(df[col], errors="coerce")

                    if numeric_values.notna().all():
                        if np.all(numeric_values % 1 == 0):
                            df[col] = numeric_values.astype(int)
                            type_info = "int"
                        else:
                            df[col] = numeric_values.astype(float)
                            type_info = "float"
                        min_value, max_value = df[col].min(), df[col].max()
                    else:
                        # Convert to string for analysis
                        string_values = df[col].dropna().astype(str)

                        if string_values.str.match(r"^\[.*\]$").any():
                            type_info = "list"
                        else:
                            type_info = "str"

                        min_value = string_values.min()
                        max_value = string_values.max()

                    column_info.append((col, unique_counts[col], type_info, min_value, max_value))

                # Create analysis facts
                analysis_lines.append(f"/* Analysis for {file_path} */")
                analysis_lines.append(f"analysis_source_file('{predicate_name}', '{file_path}', {len(df)}).")
                for col, num_unique, type_info, min_value, max_value in column_info:
                    column_fact = format_prolog_fact_slow("analysis_column", 
                        [predicate_name, col, num_unique, type_info, min_value, max_value]).strip()
                    analysis_lines.append(column_fact)
                
                analysis_time = time.time() - start_time
                print(f"⏱️ Analyzed '{file_path}' in {analysis_time:.2f}s")

                if write_prolog:
                    # Track time for writing Prolog file
                    start_time = time.time()
                    with open(prolog_file_path, "w", encoding="utf-8") as prolog_file:
                        prolog_file.write(f"/* Unique source IDs: {unique_counts['source_id']}, Unique target IDs: {unique_counts['target_id']} */\n")
                        for _, row in df.iterrows():
                            prolog_file.write(f"{predicate_name}('{row['source_id']}', '{row['target_id']}').\n")
                    prolog_write_time = time.time() - start_time
                    print(f"⏱️ Wrote Prolog file '{prolog_file_path}' in {prolog_write_time:.2f}s")

                print(f"✅ Processing completed for '{file_path}'\n")

            except Exception as e:
                print(f"⚠️  Failed to process '{file_path}': {e}")

    # Save the analysis file if Prolog writing is enabled
    if write_prolog:
        try:
            analysis_file_path = os.path.join(output_base_dir, "links_analysis.pl")
            start_time = time.time()
            with open(analysis_file_path, "w", encoding="utf-8") as analysis_file:
                analysis_file.write("\n".join(analysis_lines) + "\n")
            analysis_write_time = time.time() - start_time
            print(f"⏱️ Wrote analysis file '{analysis_file_path}' in {analysis_write_time:.2f}s")
        except Exception as e:
            print(f"⚠️  Failed to write analysis file '{analysis_file_path}': {e}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Convert CSV files in a directory to Prolog facts.")
    parser.add_argument("input_path", help="Input directory or CSV file.")
    parser.add_argument("--tree", help="Output base directory.")
    parser.add_argument("--clobber", action="store_true", help="Overwrite existing files.")

    args = parser.parse_args()

    if os.path.isdir(args.input_path):
        input_files = sorted(
            glob.glob(os.path.join(args.input_path, "**", "*.csv"), recursive=True),
            key=os.path.getsize  # 1️⃣5️⃣ Sort files by size before processing
        )
        common_path = args.input_path
    else:
        input_files = [args.input_path]
        common_path = os.path.dirname(args.input_path)

    output_dir = args.tree if args.tree else common_path
    os.makedirs(output_dir, exist_ok=True)  #  1️⃣8️⃣ Ensure directory exists

    log_file = os.path.join(output_dir, "column_analysis_log.pl")
    #if args.clobber and os.path.exists(log_file):
    #    os.remove(log_file)

    output_links_dir = os.path.join(output_dir, "global_links")
    if args.clobber and os.path.exists(output_links_dir):
        shutil.rmtree(output_links_dir)

    os.makedirs(output_links_dir, exist_ok=True)


    #for input_csv in input_files:
    #    export_csv_to_prolog(input_csv, output_dir, log_file, common_path, args.clobber)

    for input_csv in input_files:
        if "edges" in input_csv.lower():
            export_csv_to_edge_files(input_csv, output_dir, common_path)

    deduplicate_edge_files(output_dir)
