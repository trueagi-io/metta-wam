#!/bin/bash

# Check if two arguments are provided
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 file1 file2"
    exit 1
fi

# Assign arguments to variables for clarity
file1=$1
file2=$2

# Initialize counters
lines_added=0
lines_skipped=0

# Read each line from file2
while IFS= read -r line
do
    # Check if the line is not in file1
    if ! grep -Fxq "$line" "$file1"; then
        # If the line is not found, append it to file1
        echo "$line" >> "$file1"
        let lines_added++
    else
        let lines_skipped++
    fi
done < "$file2"

# Print the results
echo "Lines added: $lines_added"
echo "Lines skipped: $lines_skipped"

