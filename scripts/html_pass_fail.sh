#!/bin/bash

# Check if a start directory is provided
if [ -z "$1" ]; then
    echo "Usage: $0 <start_directory>"
    exit 1
fi

start_dir="${1%/}"

reverse_path() {
    local original_path=$1
    IFS='/' read -ra ADDR <<< "$original_path"
    local reversed_path=""
    for (( i=${#ADDR[@]}-1 ; i>=0 ; i-- )) ; do
        reversed_path+="${ADDR[i]}"
        if [ $i -gt 0 ]; then
            reversed_path+="/"
        fi
    done
    echo "$reversed_path"
}

convert_path() {
    local path=${1#./}
    local modified_path=${path#*/}
    echo "$modified_path"
}

temp_file=$(mktemp)

find "$start_dir" -type d -not -path '*/__pycache__*' | while read -r dir; do
    slash_count=$(tr -cd '/' <<< "$dir" | wc -c)
    echo "$dir $slash_count" >> "$temp_file"
done

echo "<html><head><title>Test Results</title></head><body>"
echo "<table border='1'><tr><th>Pass</th><th>Fail</th><th>Miss</th><th>Percent</th><th>Module</th><th>Directory</th></tr>" 

sort -k1,1r -k2,2nr  "$temp_file" | while read -r dir slash_count; do
    total_pass=0
    total_fail=0
    files_no_totals=0
    had_files=false

    while read -r file; do
       pass=$(tac "$file" | grep -oP 'Successes: \K\d+' | head -n 1 | bc || echo 0)
       fail=$(tac "$file" | grep -oP 'Failures: \K\d+' | head -n 1 | bc || echo 0)
       total_pass=$((total_pass + pass))
       total_fail=$((total_fail + fail))
       had_files=true
    done < <(find "$dir" -name "*.metta.html" -type f)

    if [ "$had_files" = true ]; then
        dir_percent=0
        total_tests=$((total_pass + total_fail + files_no_totals))
        if [ "$total_tests" -gt 0 ]; then
            dir_percent=$((100 * total_pass / total_tests))
            mdir="$(convert_path "$dir")"
            printf "<tr><td>%d</td><td>%d</td><td>%s</td><td>%d%%</td><td>%s</td><td>%s</td></tr>\n" "$total_pass" "$total_fail" "$files_no_totals" "$dir_percent" "$(reverse_path "$mdir")" "$mdir" 
        fi
    fi
done

# Closing the initial table
echo "</table><br><br><br>" 


base_url="https://logicmoo.org/public/metta/"

sort -k2,2n "$temp_file" | while read -r dir slash_count; do
    total_pass=0
    total_fail=0
    had_files=false
    cat /dev/null > file_info.tmp

    while read -r file; do
        if [[ -f "$file" ]]; then
            pass=$(tac "$file" | grep -oP 'Successes: \K\d+' | head -n 1 | bc || echo 0)
            fail=$(tac "$file" | grep -oP 'Failures: \K\d+' | head -n 1 | bc || echo 0)
            total_pass=$((total_pass + pass))
            total_fail=$((total_fail + fail))
            had_files=true
            file_percent=0
            total_file_tests=$((pass + fail))
            if [ "$total_file_tests" -gt 0 ]; then
                file_percent=$((100 * pass / total_file_tests))
            fi
            relative_path=$(echo "$file" | sed 's/^\.\///' | sed -e 's|examples|reports|g' -e 's|-reports|-examples|g')
            github_link="${base_url}${relative_path}"
            printf "<tr><td>%d</td><td>%d</td><td>%d%%</td><td><a href='%s'>%s</a></td></tr>\n" "$pass" "$fail" "$file_percent" "$github_link" "$(basename "${file%.html}")" >> file_info.tmp
        fi
    done < <(find "$dir" -name "*.metta.html" -type f -maxdepth 1)

    if [ "$had_files" = true ]; then
        total_tests=$((total_pass + total_fail))
        dir_percent=0
        if [ "$total_tests" -gt 0 ]; then
            dir_percent=$((100 * total_pass / total_tests))
            mdir="$(convert_path "$dir")"
            echo "<table border='1'><tr><td colspan='4'>Directory: ./$mdir</td></tr>" 
            echo "<tr><th>Pass</th><th>Fail</th><th>Percent</th><th>File/Module/Directory</th></tr>" 
            cat file_info.tmp 
            # Adding a total summary row for the directory
            echo "<tr><td><strong>$total_pass</strong></td><td><strong>$total_fail</strong></td><td><strong>$dir_percent%</strong></td><td><strong>Total</strong></td></tr>"
            echo "</table><br>" 
        fi
    fi
done

echo "</body></html>" 

rm "$temp_file" # Clean up the temporary file
rm file_info.tmp # Clean up temporary file


