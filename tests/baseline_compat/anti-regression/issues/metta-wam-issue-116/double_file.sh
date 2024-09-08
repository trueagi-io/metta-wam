#!/bin/bash

# Function to extract the number from the file name and double it
double_filename() {
    filename=$1
    base=$(basename "$filename" .metta)  # Extract the base name without extension
    num=$(echo "$base" | grep -o '[0-9]*')  # Extract the number from the base name
    doubled_num=$((num * 2))  # Double the number
    new_filename="hald_spo_$doubled_num.metta"  # Construct the new file name
    echo "$new_filename"
}

# Function to double the file size by concatenating the file with itself and renaming it
double_file() {
    filename=$1
    new_filename=$(double_filename "$filename")

    # Create the new file by concatenating the file with itself
    cat "$filename" "$filename" > "$new_filename"

    echo "File doubled and renamed to: $new_filename"
}

# Check if the script receives a valid file as input
if [ $# -eq 0 ]; then
    echo "Usage: $0 <filename>"
    exit 1
fi

# Call the function to double the file
double_file "$1"

