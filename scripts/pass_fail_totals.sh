#!/bin/bash

# Check if a start directory is provided
if [ -z "$1" ]; then
    echo "Usage: $0 <start_directory>"
    exit 1
fi

start_dir=$1


# Create a temporary file for storing directory and file count
temp_file=$(mktemp)

# Find all directories under the specified start directory and store in temp file with slash count
find "$start_dir" -type d -not -path '*/__pycache__*' -not -path '*~*' | while read -r dir; do
    # Count the number of slashes in the directory path
    slash_count=$(tr -cd '/' <<< "$dir" | wc -c)
    echo "$dir $slash_count" >> "$temp_file"
done


echo "| Pass | Fail |Miss|Percent| Directory |"
echo "|------|------|----|-------|-----------|"

# Find all directories under the specified start directory and loop through them
sort -k1,1r -k2,2nr  "$temp_file" | while read -r dir slash_count; do
    total_pass=0
    total_fail=0
    files_no_totals=0

    # Process each .metta.html file in the directory
    while read -r file; do
            # Extract successes and failures
       pass=$(tac "$file" | grep -oP 'Successes: \K\d+' | head -n 1 | bc || echo 0)
       fail=$(tac "$file" | grep -oP 'Failures: \K\d+' | head -n 1 | bc || echo 0)

            # Add to totals
            total_pass=$((total_pass + pass))
            total_fail=$((total_fail + fail))

            # Calculate and print the file percentage
            total_file_tests=$((pass + fail))
            if [ "$total_file_tests" -ne 0 ]; then
                had_files=true
                file_percent=$((100 * pass / total_file_tests))
            else
               files_no_totals=$((files_no_totals + 1))
            fi
    done < <(find "$dir" -name "*.metta.html" -type f)

    # Calculate and print the directory percentage
    dir_percent=0
    total_tests=$((total_pass + total_fail))
    if [ "$total_tests" -ne 0 ]; then
       if [ "$total_pass" -ne 0 ]; then
           missing=" "
           total_tests=$((total_pass + total_fail + files_no_totals))
           dir_percent=$((100 * total_pass / total_tests))
            if [ "$files_no_totals" -ne 0 ]; then
                  missing="${files_no_totals}"
            fi
           printf "| %5d| %5d| %2s |  %3d%% |%s |\n" "$total_pass" "$total_fail" "$missing" "$dir_percent" "$dir"
       fi
    fi

done







echo ""
echo ""
echo ""
echo ""




base_url="https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/"

# Sort the directories by file count in reverse order
sort -k2,2n "$temp_file" | while read -r dir slash_count; do
    total_pass=0
    total_fail=0
    cat /dev/null > file_info.tmp
    had_files=false

    # Process each .metta.html file in the directory
    while read -r file; do
        if [[ -f "$file" ]]; then
            # Extract successes and failures
           pass=$(tac "$file" | grep -oP 'Successes: \K\d+' | head -n 1 | bc || echo 0)
           fail=$(tac "$file" | grep -oP 'Failures: \K\d+' | head -n 1 | bc || echo 0)

            # Add to totals
            total_pass=$((total_pass + pass))
            total_fail=$((total_fail + fail))

            # Calculate and print the file percentage
            total_file_tests=$((pass + fail))
            file_percent=0
            if [ "$total_file_tests" -ne 0 ]; then
                had_files=true
                file_percent=$((100 * pass / total_file_tests))
            else
               total_fail=$((total_fail + 1))
               #total_pass=$((total_pass + 1))
               fail=-1
               pass=-1
            fi
            relative_path=$(echo "$file" | sed 's/^\.\///' | sed -e 's|examples|reports|g' -e 's|-reports|-examples|g')
            github_link="${base_url}${relative_path}"
            printf "| %5d | %5d |  %5d%%  | [%s](%s) |\n" "$pass" "$fail" "$file_percent" "$(basename "${file%.html}")" "$github_link" >> file_info.tmp
        fi
    done < <(find "$dir" -name "*.metta.html" -type f -maxdepth 1)

    # Calculate and print the directory percentage
    total_tests=$((total_pass + total_fail))
    dir_percent=0
    if [ "$total_tests" -ne 0 ]; then
        dir_percent=$((100 * total_pass / total_tests))
        if [ "$had_files" == "true" ]; then
           echo ""
           echo  "|  Pass |  Fail |  Percent | File/Directory Information                                                                              |"
           echo  "|-------|-------|----------|----------------------------------------------------------------------------------------------------|"
           printf "|       |       |          |                                                                                                    |\n"
           printf "|       |       |          | Directory:     ./%s                 |\n" "$dir/"
           printf "|       |       |          |                                                                                                    |\n"
           cat file_info.tmp
           printf "|       |       |          |                                                                                                    |\n"
           printf "| %5d | %5d |  %5d%%  | Total                                                                                              |\n" "$total_pass" "$total_fail" "$dir_percent"
           printf "|       |       |          |                                                                                                    |\n"
           echo ""
        fi
    fi
done


