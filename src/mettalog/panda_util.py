#!/usr/bin/env python3

#if __name__ != "mettalog":f

# Version Space Candidate Elimination inside of MeTTa
# This implementation focuses on bringing this machine learning algorithm into the MeTTa relational programming environment.
# Douglas R. Miles 2023

# Standard Library Imports
import atexit, io, inspect, json, os, re, subprocess, sys, traceback
import sys
import os
import importlib.util
import importlib
import inspect
import types
import inspect
import ast
from typing import *
from typing import List, Dict, Set, Callable
from typing_extensions import *
from typing import get_type_hints
from collections import Counter
from glob import glob
from time import monotonic_ns, time
import traceback

from mettalog import *

print_l_cmt(2, f";; ...doing {__file__}...{__package__} name={__name__}")


import os
import pandas as pd
import re
import sys
import chardet

def detect_encoding(file_path, sample_size=20000):
    with open(file_path, 'rb') as f:
        raw = f.read(sample_size)
    return chardet.detect(raw)['encoding']
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
        res = get_metta().run(json.dumps(str))
        if len(res) != 0: print_cmt(";;;=" + repr(res))

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

print_l_cmt(2, f";; ...did {__file__}...{__package__} name={__name__}")

