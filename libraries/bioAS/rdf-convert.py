#!/usr/bin/env python3

import sys
import argparse
import subprocess
import shutil
import threading
import time
import os
import pandas as pd
import csv
import glob
from rdflib import Graph, plugin
from rdflib.serializer import Serializer
from rdflib.parser import Parser

EXTENSION_TO_SERIALIZER = {
    '.rdf': 'rdfxml', '.xml': 'rdfxml', '.ttl': 'turtle', '.nt': 'ntriples', '.n3': 'n3',
    '.jsonld': 'jsonld', '.json': 'jsonld', '.nq': 'nquads', '.trig': 'trig', '.trix': 'trix', '.hext': 'hext'
}

def format_time(seconds):
    if seconds < 90:
        return f"{seconds:.2f}s"
    elif seconds < 3600:
        return f"{seconds / 60:.1f} min"
    else:
        h, rem = divmod(seconds, 3600)
        m, s = divmod(rem, 60)
        return f"{int(h):02d}:{int(m):02d}:{int(s):02d}"

def list_formats():
    input_formats = sorted(set(plugin.plugins(None, Parser)), key=lambda x: x.name.lower())
    output_formats = sorted(set(plugin.plugins(None, Serializer)), key=lambda x: x.name.lower())

    print("Available input formats:")
    for fmt in input_formats:
        print(f"  - {fmt.name}")

    print("\nAvailable output formats:")
    for fmt in output_formats:
        print(f"  - {fmt.name}")

def monitor_output(stop_event, output_file, input_size_mb, start_time):
    while not stop_event.is_set():
        if os.path.exists(output_file):
            output_size_mb = os.path.getsize(output_file) / (1024 * 1024)
            percent_complete = min(100, (output_size_mb / input_size_mb) * 100)
            elapsed = time.time() - start_time
            print(f"Elapsed: {format_time(elapsed)} | Output size: {output_size_mb:.2f} MB ({percent_complete:.1f}%)", end='\r')
        time.sleep(2)

def convert_with_rapper(input_file, output_file, input_format, output_format):
    rapper_cmd = shutil.which('rapper')
    if not rapper_cmd:
        print("Error: 'rapper' tool is not installed or not found in PATH.")
        sys.exit(1)

    print("Using rapper (streaming) for conversion...")

    actual_output_format = 'turtle' if output_format == 'n3' else output_format

    cmd = [rapper_cmd, '-i', input_format, '-o', actual_output_format, input_file]
    start_time = time.time()

    stop_event = threading.Event()
    monitor_thread = threading.Thread(
        target=monitor_output,
        args=(stop_event, output_file, os.path.getsize(input_file)/(1024*1024), start_time)
    )
    monitor_thread.start()

    with open(output_file, 'w') as out_file:
        subprocess.run(cmd, stdout=out_file, check=True)

    stop_event.set()
    monitor_thread.join()

    elapsed = time.time() - start_time
    print(f"\nConversion completed using rapper in {format_time(elapsed)}.")

def convert_rdf(input_file, output_file, input_format=None, output_format=None, use_rapper=False):
    input_size_mb = os.path.getsize(input_file) / (1024 * 1024)

    input_format = input_format or EXTENSION_TO_SERIALIZER.get(os.path.splitext(input_file)[1], 'rdfxml')
    output_format = output_format or EXTENSION_TO_SERIALIZER.get(os.path.splitext(output_file)[1], 'n3')

    print(f"Input format: {input_format}, Output format: {output_format}")

    if use_rapper:
        convert_with_rapper(input_file, output_file, input_format, output_format)
        return

    g = Graph()
    start_time = time.time()

    print("Parsing...")
    g.parse(input_file, format=input_format)
    print("Serializing...")
    g.serialize(destination=output_file, format=output_format)

    elapsed = time.time() - start_time
    output_size_mb = os.path.getsize(output_file) / (1024 * 1024)

    print(f"Conversion completed in {format_time(elapsed)}.")
    print(f"Input size: {input_size_mb:.2f} MB, Output size: {output_size_mb:.2f} MB")

def export_csv_stream(input_csv, output_csv, delimiter='|', output_delimiter='|'):
    with open(input_csv, 'r', encoding='utf-8') as infile, open(output_csv, 'w', encoding='utf-8') as outfile:
        reader = csv.reader(infile, delimiter=delimiter)
        writer = csv.writer(outfile, delimiter=output_delimiter)
        for row in reader:
            writer.writerow(row)
    print(f"Exported CSV to '{output_csv}' via streaming.")












import time
import pandas as pd
import numpy as np
import os

LOG_FILE = "column_analysis_log.csv"  # Heuristics log file

def analyze_columns(df):
    """Optimized column analysis using Pandas' built-in methods while handling mixed types."""
    start_time = time.time()  # Start timing

    unique_counts = df.nunique(dropna=True)
    dtypes = df.dtypes

    column_info = []
    for col in df.columns:
        num_unique = unique_counts[col]
        dtype = str(dtypes[col])

        # Check if column can be fully converted to numeric
        numeric_values = pd.to_numeric(df[col], errors="coerce")
        if numeric_values.notna().all():
            type_info = "int" if np.all(numeric_values % 1 == 0) else "float"
            min_value, max_value = numeric_values.min(), numeric_values.max()
        else:
            # Handle mixed types: Separate numeric and string values
            min_value, max_value = None, None
            if df[col].apply(lambda x: isinstance(x, (int, float)) and not pd.isna(x)).any():
                min_value = numeric_values.min()
                max_value = numeric_values.max()
            
            string_values = df[col].dropna().astype(str)
            if not string_values.empty:
                min_string = string_values.min()
                max_string = string_values.max()
                min_value = min_string if min_value is None else min_value
                max_value = max_string if max_value is None else max_value

            type_info = f"mixed ({', '.join(set(df[col].dropna().map(type).astype(str)))})"

        column_info.append((col, num_unique, type_info, min_value, max_value))

    elapsed_time = time.time() - start_time  # Stop timing
    return column_info, elapsed_time

def log_column_analysis(input_file, total_rows, column_info):
    """Appends the analyzed column data to a heuristics CSV log file (excluding execution times)."""
    log_data = [[input_file, total_rows, col, num_unique, type_info, min_value, max_value]
                for col, num_unique, type_info, min_value, max_value in column_info]

    df_log = pd.DataFrame(log_data, columns=[
        "Source File", "Total Rows", "Column Name", "Unique Values", "Type Info", "Min Value", "Max Value"
    ])

    file_exists = os.path.isfile(LOG_FILE)
    df_log.to_csv(LOG_FILE, mode='a', header=not file_exists, index=False)
    print(f"Column heuristics logged in '{LOG_FILE}'")

def export_csv_without_repeats(input_csv, output_csv, delimiter='|', output_delimiter='|'):
    start_time = time.time()  # Start timing overall processing
    
    df = pd.read_csv(input_csv, delimiter=delimiter, header=None, encoding='utf-8', low_memory=False)
    
    header_was = ''
    if not df.iloc[0].equals(df.iloc[1]):
        df.columns = df.iloc[0]
        header_was = f"# Was: {', '.join(df.columns)}\n"
        df = df[1:]

    column_info, type_detection_time = analyze_columns(df)
    repeated_columns = [col for col, num_unique, dtype, min_value, max_value in column_info if num_unique == 1]
    total_rows = len(df)
    elapsed_time = time.time() - start_time
    
    comment = f"# Source: {input_csv}\n"
    comment += f"# Total Rows: {total_rows}\n"
    comment += f"# Processing Time: {elapsed_time:.2f} seconds\n"

    if type_detection_time > 1:
        print(f"Type detection took {type_detection_time:.2f} seconds.")
        comment += f"# Type Detection Time: {type_detection_time:.2f} seconds\n"

    comment += "# Column Summary (Name | Unique Values | Type Info | Min | Max):\n"
    for col, num_unique, type_info, min_value, max_value in column_info:
        comment += f"#   {col}: {num_unique} unique | {type_info} | min={min_value} | max={max_value}\n"

    if repeated_columns:
        removed_values = {col: df[col].iloc[0] for col in repeated_columns}
        comment += f"# NOTES: Omitting columns with repeated values: {', '.join(repeated_columns)}\n"
        comment += header_was
        comment += f"# Removed values: {removed_values}\n"

    df.drop(columns=repeated_columns, inplace=True)

    log_column_analysis(input_csv, total_rows, column_info)

    print(f"\nProcessed '{input_csv}' ? '{output_csv}' in {elapsed_time:.2f} seconds")
    if type_detection_time > 1:
        print(f"Type detection took {type_detection_time:.2f} seconds.")

    print(comment)
    print("# First 3 rows (after cleaning):\n# ", df.head(3).to_string(index=False), "\n")

    # Write the cleaned CSV with a comment header
    with open(output_csv, 'w', encoding='utf-8', errors='replace') as f:
        if comment:
            f.write(comment + '\n')
        df.to_csv(f, index=False, sep=output_delimiter)
    
    print(f"Exported cleaned CSV to '{output_csv}' in {elapsed_time:.2f} seconds")























if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Convert RDF or CSV files.')
    formatter_class=argparse.RawDescriptionHelpFormatter,
    epilog='''Examples:

  Single file conversion:
    rdf-convert.py input.rdf output.ttl

  Using specific formats:
    rdf-convert.py input.jsonld output.xml --input-format jsonld --output-format rdfxml

  Wildcard batch conversion (Preserving directory structure):  
    rdf-convert.py 'data/**/*.rdf' _output.n3

  The same but don't omit existing files:
    rdf-convert.py 'data/**/*.rdf' _output.n3 --clobber

  Prepend a directory name:
    rdf-convert.py 'data/**/*.rdf' _as_turtle.ttl --tree converted_files/

  All files just one level below data:
    rdf-convert.py 'data/*.rdf' _output.n3

  Listing available formats:
    rdf-convert.py --formats

  CSV file cleaning (omit columns with repeated values):
    rdf-convert.py 'data/**/*.csv' _cleaned.csv --clean --tree /tmp/cleaned_files/
'''


    parser.add_argument('input_file', help='Input file path or glob pattern.')
    parser.add_argument('output_suffix', help='Output file path or suffix.')
    parser.add_argument('--tree', help='Output base directory.')
    parser.add_argument('--clobber', action='store_true', help='Overwrite existing files.')
    parser.add_argument('--input-format', type=str, help='Specify input format explicitly.')
    parser.add_argument('--output-format', type=str, help='Specify output format explicitly.')
    parser.add_argument('--clean', action='store_true', help='Clean CSV columns.')
    parser.add_argument('--rapper', action='store_true', help='Use rapper tool.')
    parser.add_argument('--csv', action='store_true', help='Process CSV files.')
    parser.add_argument('--formats', action='store_true', help='List formats and exit.')

    args = parser.parse_args()

    if args.formats:
        list_formats()
        sys.exit(0)

    if not args.input_file or not args.output_suffix:
        parser.print_help()
        sys.exit(1)

    input_files = glob.glob(args.input_file, recursive=True)
    if not input_files:
        print(f"No files matched the pattern: {args.input_file}")
        sys.exit(1)

    common_path = os.path.commonpath(input_files)

    # Sort files by size (smallest to largest)
    input_files.sort(key=lambda file: os.path.getsize(file))

    for input_path in input_files:
        rel_path = os.path.relpath(input_path, start=common_path)
        base_name, input_ext = os.path.splitext(rel_path)

        output_rel_path = base_name + args.output_suffix
        output_path = os.path.join(args.tree, output_rel_path) if args.tree else output_rel_path

        os.makedirs(os.path.dirname(output_path), exist_ok=True)

        if not args.clobber and os.path.exists(output_path):
            print(f"Skipping existing file '{output_path}' (use --clobber to overwrite).")
            continue

        print(f"\nProcessing '{input_path}' ? '{output_path}'")

        if args.csv or input_ext.lower() == '.csv':
            if args.clean:
                export_csv_without_repeats(input_path, output_path)
            else:
                export_csv_stream(input_path, output_path)
          
        else:
            convert_rdf(input_file=input_path, output_file=output_path, use_rapper=args.rapper)
