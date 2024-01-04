#!/bin/bash

# Assign the first argument to TEST_DIR
TEST_DIR=$1

# Remove a specific file and find and delete certain files
cat /dev/null >  /tmp/SHARED.UNITS
find "$TEST_DIR" --name "*metta.html" -delete

# Clear the screen and run the MeTTa program with some arguments
clear
./MeTTa $2 "$TEST_DIR" --clean --test --timeout=25

# Create a directory named with the current date and time (YYYYMMDD_HHMMSS format)
CURRENT_TIME=$(date +"%Y%m%d_%H%M%S${2}")
mkdir "$CURRENT_TIME"

# Copy the contents of the specified directory to the new time-stamped directory
cp -a "$TEST_DIR" "$CURRENT_TIME"

# Write the command-line arguments to a file in the new directory
echo "$@" > "$CURRENT_TIME/results_plain.md"

# Run pass_fail_totals.sh and append the output to results_plain.md
./scripts/pass_fail_totals.sh "$TEST_DIR" >> "$CURRENT_TIME/results_plain.md"

# Process and save the test results in PASS_FAIL.md
{
    ./scripts/pass_fail_totals.sh "$TEST_DIR" > "$CURRENT_TIME/TEST_LINKS.md"
    echo "| STATUS | TEST NAME | TEST CONDITION | ACTUAL RESULT | EXPECTED RESULT |"
    echo "|--------|-----------|----------------|---------------|-----------------|"
    cat /tmp/SHARED.UNITS | awk -F'\\(|\\) \\| \\(' '{ print $2 " " $0 }' | sort -k2,2 | cut -d' ' -f2- | awk '!seen[$0]++'
} > "$CURRENT_TIME/PASS_FAIL.md"

# Append the output of total_loonits.sh to results_plain.md
rm -f /tmp/SHARED.UNITS
./scripts/total_loonits.sh >> "$CURRENT_TIME/results_plain.md"



