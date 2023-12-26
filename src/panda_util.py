#!/usr/bin/env python3

print(";; ...doing...",__name__)

# Version Space Candidate Elimination inside of MeTTa
# This implementation focuses on bringing this machine learning algorithm into the MeTTa relational programming environment.
# Douglas R. Miles 2023

# Standard Library Imports
import atexit, io, inspect, json, os, re, subprocess, sys, traceback
from collections import Counter
from glob import glob
from time import monotonic_ns, time

# Global Variables
VSPACE_VERBOSE = os.environ.get("VSPACE_VERBOSE")
# 0 = for scripts/demos
# 1 = developer
# 2 = debugger
verbose = 1
if VSPACE_VERBOSE is not None:
 try: verbose = int(VSPACE_VERBOSE) # Convert it to an integer
 except ValueError: ""


def timeFrom(w, t0):
    elapsed_ns = monotonic_ns() - t0
    elapsed_s = elapsed_ns / 1e9
    elapsed_ms = elapsed_ns / 1e6
    elapsed_us = elapsed_ns / 1e3

    if elapsed_s >= 1:
        print_cmt(f"{w} took {elapsed_s:.5f} seconds")
    elif elapsed_ms >= 1:
        print_cmt(f"{w} took {elapsed_ms:.5f} milliseconds")
    else:
        print_cmt(f"{w} took {elapsed_us:.5f} microseconds")




def redirect_stdout(inner_function):
    old_stdout = sys.stdout # Save the current stdout stream
    new_stdout = io.StringIO() # Create a new StringIO buffer
    sys.stdout = new_stdout # Redirect stdout to the new buffer
    try:
        inner_function() # Execute the inner function
    finally:
        sys.stdout = old_stdout # Restore the original stdout stream
    output = new_stdout.getvalue() # Retrieve the output from the new buffer
    new_stdout.close() # Close the new buffer
    return output



def flush_console():
    try:
      if sys.__stdout__ is not None: sys.__stdout__.flush()
    except Exception: ""
    try:
      if sys.__stderr__ is not None: sys.__stderr__.flush()
    except Exception: ""
    try:
      if sys.stderr is not None and not (sys.stderr is sys.__stderr__): sys.sys.stderr.flush()
    except Exception: ""
    try:
      if sys.stdout is not None and not (sys.stdout is sys.__stdout__): sys.sys.stdout.flush()
    except Exception: ""


# Exporting to another CSV (for demonstration)
#df.to_csv("exported.csv", index=False)
#print_cmt("\n### Data Exported to 'exported.csv' ###")


import os
import pandas as pd
import re
import sys
import chardet

def detect_encoding(file_path, sample_size=20000):
    with open(file_path, 'rb') as f:
        raw = f.read(sample_size)
    return chardet.detect(raw)['encoding']

from collections.abc import Iterable

def is_lisp_dashed(s):
    pattern = re.compile('^[A-Za-z0-9-_:]+$')
    return bool(pattern.match(s))

def item_string(lst, functor=""):
    if isinstance(lst, str):
        if len(lst) == 0:
            return '""'
        if any(char in lst for char in [' ', '"', "'", "(", ")", ".", "\\"]):
            return json.dumps(lst)
        if isinstance(lst, (int, float)):
            return repr(lst)
        if is_float_string(lst):
            return repr(float(lst))
        if lst.isdigit():
            return repr(int(lst))
        if lst.isalnum():
            if lst[0].isdigit(): return json.dumps(lst)
            return lst
        if lst=="#":
            return lst
        if is_lisp_dashed(lst):
            return lst
        return json.dumps(lst)

    try:
        if isinstance(lst, Iterable):
            return '(' + functor + ' '.join([item_string(vv) for vv in lst]) + ')'
        else:
            return str(lst)
    except TypeError:
        return str(lst)

def list_string(lst, functor="# "):
    try:
        if isinstance(lst, Iterable) and not isinstance(lst, str):
            if len(lst) == 0:
                return '()'
            return '(' + functor + ' '.join([item_string(vv) for vv in lst]) + ')'
        else:
            return item_string(lst)
    except TypeError:
        return item_string(lst)

def is_float_string(s):
    return bool(re.fullmatch(r'[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?', s))

import pandas as pd

def update_dataframe_skipping_first_row(df):
    """
    Takes a DataFrame, skips the first row, recalculates unique counts and value counts,
    and infers the most appropriate datatypes for each column.

    Parameters:
    df (pandas.DataFrame): The original DataFrame.

    Returns:
    pandas.DataFrame: A DataFrame with the first row removed, updated with recalculated
    uniqueness and value counts, and with inferred datatypes.
    """
    # Check if the DataFrame is empty or has only one row
    if df.empty or df.shape[0] == 1:
        raise ValueError("DataFrame is empty or has only one row, which cannot be skipped.")

    # Skip the first row and reset the index
    updated_df = df.iloc[1:].reset_index(drop=True)

    # Attempt to infer better dtypes for object columns
    updated_df = updated_df.infer_objects()
    updated_df = updated_df.convert_dtypes()

    # Update DataFrame with uniqueness and value counts for each column
    for col in updated_df.columns:
        updated_df[f'{col}_unique_count'] = updated_df[col].nunique()
        updated_df[f'{col}_value_counts'] = updated_df[col].value_counts().to_dict().__str__()

    return updated_df



def analyze_csv_basename(file_path, sep=None):
    base_name = os.path.basename(file_path)
    base_name = base_name.replace('.tsv', '')
    base_name = base_name.replace('.fb', '')
    # Remove a sequence like _fb_####_## (where # represents a digit)
    base_name = re.sub(r'_fb_\d{4}_\d{2}', '', base_name)
    # Remove a sequence like ####_## at any place
    base_name = re.sub(r'\d{4}_\d{2}', '', base_name)
    # Replace periods with underscores, if not part of a file extension
    base_name = re.sub(r'\.(?=.*\.)', '_', base_name)
    analyze_csv(base_name, file_path, sep=sep)

needed_Skip = 0

def analyze_csv(base_name, file_path, sep=None):
    print_cmt(";;------------------------------------------------------------------------------------------------------------------")
    print_cmt(f"Analyzing file: {file_path}")
    missing_values_list = ["","-"," ","|",",","#",  "*",  "\"\"",  "+", "NULL", "N/A", "--",
                         "NaN","EMPTY","None","n/a","(none)",
                         # "0","Dmel","-1",
                          "MISSING", "?", "undefined", "unknown", "none", "[]", "."]

    def read_csv(enc, skip_rows, header_option):
            false_values_list = ["F", "f", "False", "false", "N", "No", "no", "FALSE"]
            true_values_list = ["T", "t", "True", "true", "Y", "Yes", "yes", "TRUE"]

            engine = 'python' if sep is None else 'c'
            return pd.read_csv(
                file_path,
                sep=sep,
                encoding=enc,
                comment='#',
                compression='infer',
                true_values=true_values_list,
                false_values=false_values_list,
                engine=engine,
                header=header_option,
                skiprows=skip_rows,
                #names=header_names,
                keep_default_na=False,
                skip_blank_lines=True,
                on_bad_lines='skip'
            )

    def read_csv_both_encodings(skip_rows=None, header_option=None):
        try:
            return read_csv('utf-8', skip_rows, header_option)
        except UnicodeDecodeError:
            encoding = detect_encoding(file_path)
            if encoding=='uft-8':
                print_cmt(";; Trying '{encoding}' encoding...")
                try:
                    return read_csv(encoding, skip_rows)
                except Exception as e:
                    print_cmt(f";; Error reading the file with 'utf-8' encoding: {e}")
                    return None
        except Exception as e:
            print_cmt(f";; Error reading the file: {e}")
            return None

    df = read_csv_both_encodings()

    # Function to check if a string contains any digits
    def contains_digit(s):
        return any(char.isdigit() for char in s)

    # Read the first few rows to check for digits
    header_candidates = df.head(3)
    first_row_has_no_digits = all(not contains_digit(str(value)) for value in header_candidates.iloc[0])
    second_third_row_has_digits = any(contains_digit(str(value)) for value in header_candidates.iloc[1]) and any(contains_digit(str(value)) for value in header_candidates.iloc[2])
    global needed_Skip
    # If the first row has no digits but the second and third do, treat the first row as a header
    if first_row_has_no_digits and second_third_row_has_digits:
        print_cmt("First row is set as header based on the digit check.")
        df = read_csv_both_encodings(skip_rows=1, header_option=None)
        needed_Skip = 1
        old_columns = header_candidates.iloc[0]
    else:
        print_cmt("Digit check is inconclusive for determining a header row. No header set.")
        old_columns = df.columns
        # If the columns should be anonymized or kept as is, handle that here


    need_anon_columns = False
    for col in old_columns:
        if not re.match("^[a-zA-Z]+$", str(col)):
            need_anon_columns = True
            break

    new_columns = [f'{i+1}' for i in range(df.shape[1])] if need_anon_columns else old_columns.tolist()
    if need_anon_columns:
        df.columns = new_columns

    col_names = ' '.join([f"{col}" for col in new_columns])

    numerical_columns = df.select_dtypes(include=['number']).columns.tolist()

    def metta_read(str):
        print(str)
#        res = the_python_runner.run(json.dumps(str))
#        if len(res) != 0: print_cmt(";;;="+ repr(res))

    metta_read(f"!(file-name {base_name}  {file_path})")
    metta_read(f"(num-columns {base_name} {df.shape[1]})")
    metta_read(f"(column-names {base_name} {list_string(old_columns)})")
    metta_read(f"(column-names-maybe {base_name} {list_string(df.columns)})")
    metta_read(f"(duplicated-rows {base_name} {df.duplicated().sum()})")
    metta_read(f"(total-rows {base_name} {len(df)})")
    for col in new_columns:
        metta_read(f"(unique-values {base_name} {col} {df[col].nunique()} {df[col].dtype})")

    # Print the unique NA values and their frequencies for each column
    for col in df.columns:
        missing_count = 0
        unique_na_values = []
        frequency_of_unique_na = []
        for na_val in missing_values_list:
            count = df[df[col] == na_val].shape[0]
            if count > 0:
                missing_count += count
                metta_read(f"(null-value-count {base_name} {col} \"{na_val}\" {count})")
                unique_na_values.append(na_val)
                frequency_of_unique_na.append(count)

        metta_read(f"(missing-values {base_name} {col} {missing_count} {list_string(unique_na_values)} {list_string(frequency_of_unique_na)})")

    hl =7
    for col in df.columns:
        if len(df) != df[col].nunique():

            isfrequents = [["#", val, cnt] for val, cnt in df[col].value_counts(ascending=False).head(hl).items() if val not in missing_values_list or len(val)>3]
            isfrequents.reverse()
            metta_read(f"(most-frequent {base_name} {col} {list_string(isfrequents)})\n")
            infrequents = [["#", val, cnt] for val, cnt in df[col].value_counts(ascending=True).head(hl).items() if val not in missing_values_list or len(val)>3]
            #infrequents.reverse()
            #infrequents.reverse()  # Since we can't use slicing on a generator, we reverse it here
            metta_read(f"(less-frequent {base_name} {col} {list_string(infrequents)})\n")

    #metta_read(f"(data-types {base_name} {col} {col.dtype} )")



def import_metta_file(string):
    global argmode
    if argmode=="mettalog":
        load_vspace()
        swip_exec(f"load_metta_file('{selected_space_name}','{string}')")
    else: the_python_runner.import_file(string)



import os
import sys

def vspace_main(*args):
    is_init=False
    #os.system('clear')
    t0 = monotonic_ns()
    flush_console()
    #if is_init==False: load_vspace()
    #if is_init==False: load_flybase()
    #if is_init==False:

    if isinstance(args, str):
        handle_arg(args)
    elif isinstance(args, list):
        for arg in args:
            if isinstance(arg, str):
                if len(arg) > 1: handle_arg(arg)

    flush_console()
    global argmode
    #the_python_runner.repl(mode=argmode)
    flush_console()
    if verbose>1: timeFrom("main", t0)
    flush_console()

def vspace_main_from_python(sysargv1toN):
    vspace_main(sysargv1toN)

def handle_arg(string, skip_filetypes=['.metta', '.md','.pl', '.png', '.jpg', '.obo']):

        lower = string.lower()

        if lower in ["--metta","--mettalog","--python"]:
            global argmode
            argmode = lower.lstrip('-')
            if verbose>0: print("; argmode=", argmode)
            return

        if os.path.isfile(string):
            if lower.endswith('.metta'):
                if verbose>0: print("; import_metta_file=", string)
                #import_metta_file(string)
                return

        global needed_Skip
        if string=="--analyze": sys.exit(needed_Skip)

        if os.path.isdir(string):
            # If it's a directory, traverse it
            for root, _, files in os.walk(string):
                for file in files:
                    try:
                        if any(file.endswith(ext) for ext in skip_filetypes):
                            if verbose>0: print_cmt(f"Skipping file: {file}")
                            continue
                        handle_arg([os.path.join(root, file)], skip_filetypes)
                    except Exception as e:
                        print_cmt(f"An error occurred while processing {string}: {e}")
            return

        elif os.path.isfile(string):
            if lower.endswith('.csv'):
                analyze_csv_basename(string, sep=',')
                return
            elif lower.endswith('.tsv'):
                analyze_csv_basename(string, sep='\t')
                return
            else:
                # Read only the first few lines
                try:
                    analyze_csv_basename(string)
                except UnicodeDecodeError:
                    print_cmt(f"Passing in file: {string}")
                    with open(string, 'r') as file:
                        for i, line in enumerate(file):
                            if i >= 10:
                                break
                            print_cmt(line.strip())
                return

        print_cmt(f"Skipping: {string}")

def print_cmt(*args, prefix=";; "):
   for arg in args:
       println(arg, prefix=prefix)
       flush_console()

def println(orig, prefix=""):
    """
    Prints the given object and returns it.

    Args:
        orig: The object to be printed.

    Returns:
        The same object that was passed in.
    """
    try:
      prefix_print(prefix, orig)
    except Exception as e:
      if verbose>0: print_cmt(f"println-Error: {e}")
    flush_console()
    return orig

def prefix_print(prefix, orig):

    obj = orig

    if isinstance(obj, str):
        objlns = obj.splitlines()
        for r in objlns:
            print(prefix, r)
        return

is_init=False

if __name__ == "__main__":
    vspace_main_from_python(sys.argv[1:])

