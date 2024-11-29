#!/bin/bash

# Converts MeTTaLog to Prolog and benchmarks the Prolog implementation.

if [ $# -ne 1 ]; then
    echo "Usage: $0 <file.metta>"
    exit 1
fi

METTA_FILE=$1
PROLOG_FILE="${METTA_FILE%.metta}.pl"
TRANSLATOR_SCRIPT="./mettalog_to_prolog.py"

if [ ! -f "$TRANSLATOR_SCRIPT" ]; then
    echo "Translator script not found: $TRANSLATOR_SCRIPT"
    exit 1
fi

echo "Converting $METTA_FILE to $PROLOG_FILE..."
python3 "$TRANSLATOR_SCRIPT" "$METTA_FILE" > "$PROLOG_FILE"
if [ $? -ne 0 ]; then
    echo "Conversion failed."
    exit 1
fi

echo "Running benchmarks for $PROLOG_FILE..."
swipl -q -t run_tests -f "$PROLOG_FILE"
if [ $? -ne 0 ]; then
    echo "Benchmarking failed."
    exit 1
fi

echo "Prolog benchmarking completed successfully."
