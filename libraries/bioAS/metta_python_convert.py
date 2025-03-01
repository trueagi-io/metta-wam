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
    '.rdf': 'rdfxml',
    '.xml': 'rdfxml',
    '.ttl': 'turtle',
    '.nt': 'ntriples',
    '.n3': 'n3',
    '.jsonld': 'jsonld',
    '.json': 'jsonld',
    '.nq': 'nquads',
    '.trig': 'trig',
    '.trix': 'trix',
    '.hext': 'hext'
}

def list_formats():
    input_formats = sorted(set(plugin.plugins(None, Parser)), key=lambda x: x.name.lower())
    output_formats = sorted(set(plugin.plugins(None, Serializer)), key=lambda x: x.name.lower())

    print("Available input formats:")
    for fmt in input_formats:
        print(f"  - {fmt.name}")

    print("\nAvailable output formats:")
    for fmt in output_formats:
        print(f"  - {fmt.name}")

def format_time(seconds):
    if seconds < 90:
        return f"{seconds:.1f}s"
    elif seconds < 3600:
        return f"{seconds / 60:.1f} min"
    else:
        h, rem = divmod(seconds, 3600)
        m, s = divmod(rem, 60)
        return f"{int(h):02d}:{int(m):02d}:{int(s):02d}"

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
    monitor_thread = threading.Thread(target=monitor_output, args=(stop_event, output_file, os.path.getsize(input_file) / (1024 * 1024), start_time))
    monitor_thread.start()

    with open(output_file, 'w') as out_file:
        subprocess.run(cmd, stdout=out_file, check=True)

    stop_event.set()
    monitor_thread.join()

    elapsed = time.time() - start_time
    print(f"\nConversion completed using rapper in {format_time(elapsed)}.")

def convert_rdf(input_file, output_file, input_format=None, output_format=None, use_rapper=False):
    input_size_mb = os.path.getsize(input_file) / (1024 * 1024)

    if input_format is None:
        input_format = EXTENSION_TO_SERIALIZER.get(input_file[input_file.rfind('.'):].lower(), 'rdfxml')

    if output_format is None:
        output_format = EXTENSION_TO_SERIALIZER.get(output_file[output_file.rfind('.'):].lower(), 'n3')

    print(f"Input format: {input_format}")
    print(f"Output format: {output_format}")

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
    print(f"Input file size: {input_size_mb:.2f} MB")
    print(f"Output file size: {output_size_mb:.2f} MB")

def export_csv_stream(input_csv, output_csv, delimiter='|', output_delimiter='|'):
    with open(input_csv, 'r', newline='', encoding='utf-8') as infile, \
         open(output_csv, 'w', newline='', encoding='utf-8') as outfile:
        reader = csv.reader(infile, delimiter=delimiter)
        writer = csv.writer(outfile, delimiter=output_delimiter)
        for row in reader:
            writer.writerow(row)
    print(f"Exported CSV to '{output_csv}' via streaming.")

def export_csv_without_repeats(input_csv, output_csv, delimiter='|', output_delimiter='|'):
    df = pd.read_csv(input_csv, delimiter=delimiter, header=None)
    header_row = df.iloc[0]
    data_row = df.iloc[1]

    if not header_row.equals(data_row):
        df.columns = header_row
        df = df[1:]

    repeated_columns = [col for col in df.columns if df[col].nunique() == 1]
    if repeated_columns:
        print(f"Omitting columns with repeated values: {', '.join(repeated_columns)}")
    df.drop(columns=repeated_columns, inplace=True)
    df.to_csv(output_csv, index=False, sep=output_delimiter)
    print(f"Exported cleaned CSV to '{output_csv}'.")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
            description='Convert RDF or CSV files, supporting wildcards, structured outputs, and CSV cleaning.',
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
)

    parser.add_argument('input_file', help='Input file path or glob pattern.')
    parser.add_argument('output_suffix', help='Output file path or suffix.')
    parser.add_argument('--tree', type=str, help='Output base directory (preserves input structure).')
    parser.add_argument('--clobber', action='store_true', help='Overwrite existing files.')
    parser.add_argument('--input-format', type=str, help='Specify input format explicitly.')
    parser.add_argument('--output-format', type=str, help='Specify output format explicitly.')
    parser.add_argument('--clean', action='store_true', help='Omit columns with repeated values in CSV files.')
    parser.add_argument('--rapper', action='store_true', help='Use rapper for streaming RDF conversion.')
    parser.add_argument('--csv', action='store_true', help='Process CSV files.')

    args = parser.parse_args()

    if args.formats:
        list_formats()
        sys.exit()

     if not args.input_file or not args.output_suffix:
            parser.print_help()
            sys.exit(1)
    input_files = glob.glob(args.input_file, recursive=True)
    if not input_files:
        print(f"No files matched the pattern: {args.input_file}")
        sys.exit(1)


    common_path = os.path.commonpath(input_files)

    for input_path in input_files:
        rel_path = os.path.relpath(input_path, start=common_path)
        base_name, input_ext = os.path.splitext(rel_path)

        output_path = os.path.join(args.tree, base_name) if args.tree else base_name
        os.makedirs(os.path.dirname(output_path), exist_ok=True)

        if not args.clobber and os.path.exists(output_path):
            print(f"Skipping existing file '{output_path}' (use --clobber to overwrite).")
            continue

        print(f"\nProcessing '{input_path}' ? '{output_path}'")
    
        if args.csv or input_path.lower().endswith('.csv'):
            if args.clean:
                export_csv_without_repeats(input_path, output_path)
            else:
                export_csv_stream(input_path, output_path)

        else:
            convert_rdf(
                input_file=input_path, 
                output_file=output_path,
                input_format=args.input_format,
                output_format=args.output_format,
                use_rapper=args.rapper
            )
