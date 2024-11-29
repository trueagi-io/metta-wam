#!/bin/bash

# This script converts a .metta file to C and runs benchmarks.

# Check if the input file is provided
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <file.metta>"
    exit 1
fi

METTA_FILE=$1
C_FILE="${METTA_FILE%.metta}.c"
EXECUTABLE="${METTA_FILE%.metta}.out"

# Ensure the translator script exists
TRANSLATOR_SCRIPT="./mettalog_to_c.py"
if [ ! -f "$TRANSLATOR_SCRIPT" ]; then
    echo "Translator script not found: $TRANSLATOR_SCRIPT"
    exit 1
fi

# Convert .metta to .c using the translator script
echo "Converting $METTA_FILE to $C_FILE..."
python3 "$TRANSLATOR_SCRIPT" "$METTA_FILE" > "$C_FILE"

if [ $? -ne 0 ]; then
    echo "Error during conversion."
    exit 1
fi

# Compile the C file
echo "Compiling $C_FILE to $EXECUTABLE..."
gcc -O3 "$C_FILE" -o "$EXECUTABLE" -lm

if [ $? -ne 0 ]; then
    echo "Error during compilation."
    exit 1
fi

# Run the C benchmark
echo "Running benchmarks for $EXECUTABLE..."
time "./$EXECUTABLE"

if [ $? -ne 0 ]; then
    echo "Error during C benchmarking."
    exit 1
fi

echo "C benchmarking completed successfully."


