#!/bin/bash

# Detect if the script is being sourced, and if so, re-run it as a standalone script
if [[ "${BASH_SOURCE[0]}" != "${0}" ]]; then
    echo "⚠️ This script should not be sourced. Running it as a separate process..."
    bash "$0" "$@"
    return
fi

# Change to the directory where the script is located
cd "$(dirname "$(realpath "$0" 2>/dev/null || greadlink -f "$0")")" || exit 1

# Ensure both arguments are provided
if [ $# -ne 2 ]; then
    echo "Usage: $0 <input_directory> <output_directory>"
    exit 1
fi

# macOS does not support `realpath` by default; use greadlink (from coreutils) if available
if command -v realpath >/dev/null 2>&1; then
    indir="$(realpath "$1")"
    outdir="$(realpath "$2")"
elif command -v greadlink >/dev/null 2>&1; then
    indir="$(greadlink -f "$1")"
    outdir="$(greadlink -f "$2")"
else
    echo "Error: Neither 'realpath' nor 'greadlink' (coreutils) is available. Install coreutils on macOS using:"
    echo "brew install coreutils"
    exit 1
fi

# Remove trailing slashes
indir="${indir%/}"
outdir="${outdir%/}"

echo "📂 Using Input Directory:  $indir"
echo "📂 Using Output Directory: $outdir"

# Run Python script with cleaned paths
time python3 ./metta_python_convert.py "$indir/" --tree "$outdir/"

# Find and compile .pl files, then rename them to .txt
find "$outdir/" -name "*.pl" -printf "%s %p\n" 2>/dev/null | sort -n | awk '{print $2}' | while read -r file; do
    qlf_file="${file%.pl}.qlf"

    if [ ! -f "$qlf_file" ] || [ "$file" -nt "$qlf_file" ]; then
        echo "Compiling: $file"
        time swipl -q -g "set_prolog_flag(encoding, utf8), qcompile(\"$file\"), halt."
        mv "$file" "${file%.pl}.txt"
    else        
        echo "Skipping $file (up-to-date qlf exists)."
    fi

    mv "$file" "${file%.pl}.txt"
done

# Find all .qlf files
find "$outdir/" -name "*.qlf"

