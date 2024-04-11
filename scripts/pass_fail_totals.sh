#!/bin/bash

# Check if a start directory is provided
if [ -z "$1" ]; then
    echo "Usage: $0 <start_directory>"
    exit 1
fi

start_dir="${1%/}"
#start_dir=$1

reverse_path() {
    # The original path is read from the first argument to the function
    local original_path=$1

    # Split the path into components based on the slash character
    # and reverse the components
    IFS='/' read -ra ADDR <<< "$original_path"
    local reversed_path=""
    for (( i=${#ADDR[@]}-1 ; i>=0 ; i-- )) ; do
        reversed_path+="${ADDR[i]}"
        if [ $i -gt 0 ]; then
            reversed_path+="/"
        fi
    done

    # Print the reversed path
    echo "$reversed_path"
}

convert_path() {
    # Remove the leading './' from the path
    local path=${1#./}

    # Remove the first directory component from the path
    local modified_path=${path#*/}

    # Print the modified path
    echo "$modified_path"
}

convert_mod() {
    # Remove the leading './' from the path
        local mod=${1#./}
	mod="${mod%/}"
	mod="${mod//baseline/ }"
	mod="${mod//scripts/ }"	    	    
	mod="${mod//\_/ }"
	mod="${mod//\// }"
	mod="${mod//  / }"
	mod="${mod//  / }"
	mod="${mod//  / }"
	mod="${mod//  / }"
	mod="${mod//tests compiler/C }"
	mod="${mod//tests compat/I }"
     echo "$mod"
}

# Create a temporary file for storing directory and file count
temp_file=$(mktemp)

# Find all directories under the specified start directory and store in temp file with slash count
find "$start_dir" -type d -not -path '*/__pycache__*' | while read -r dir; do
    # Count the number of slashes in the directory path
    slash_count=$(tr -cd '/' <<< "$dir" | wc -c)
    echo "$dir $slash_count $(reverse_path $dir)" >> "$temp_file"
done


file_info_tmp=$(mktemp)
chart1=$(mktemp)

tput rmam

# Find all directories under the specified start directory and loop through them
sort -k1,1r -k2,2nr  "$temp_file" | while read -r dir slash_count rvdir; do
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
       	   total_pass_fail=$((total_pass + total_fail))
	   show_total_as="${total_pass_fail}"
	   missing=" "
           total_tests=$((total_pass_fail + files_no_totals))
           dir_percent=$((100 * total_pass / total_tests))
            if [ "$files_no_totals" -ne 0 ]; then
                  show_total_as="${show_total_as}"
		  missing="${files_no_totals}"
            fi
	    mdir="$(convert_path $dir)"
	    mod="$(convert_mod $mdir)"
           printf "|%4d|%-2s%3d|%4s|  %3d%% | %-30s | %s |\n" "$total_pass" "$missing" "$total_fail" "$show_total_as" "$dir_percent" "$mod" "$mdir"  >> $chart1

       fi
    fi

done

echo ""
echo "|Pass|EFail|Totl|Percent| Module | Directory |"
echo "|----|-----|----|-------|--------|-----------|"
awk -F '|' '{ 
    # Extract the 5th column and the "subfield" for sorting
    #split($5, module_parts, "|"); 
    # Print the sort keys followed by the original line
    print $4 " |" $0; 
}' $chart1 | sort -k1,1n | cut -d '|' -f2-




echo ""
echo ""
echo ""
echo ""





base_url="https://logicmoo.org/public/metta/"

# Sort the directories by slash count in reverse order
sort -k3,3 -k2,2nr "$temp_file" | while read -r dir slash_count rvdir; do

    total_pass=0
    total_fail=0
    cat /dev/null > $file_info_tmp
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
            printf "| %5d | %5d |  %5d%%  | [%s](%s) |\n" "$pass" "$fail" "$file_percent" "$(basename "${file%.html}")" "$github_link" >> $file_info_tmp
        fi
    done < <(find "$dir" -name "*.metta.html" -type f -maxdepth 1)

    # Calculate and print the directory percentage
    total_tests=$((total_pass + total_fail))
    dir_percent=0
    mdir="$(convert_path $dir)"
    if [ "$total_tests" -ne 0 ]; then
        dir_percent=$((100 * total_pass / total_tests))
        if [ "$had_files" == "true" ]; then
           echo ""
           echo  "|  Pass |  Fail |  Percent | File/Module/Directory Information                                                                              |" 
           echo  "|-------|-------|----------|----------------------------------------------------------------------------------------------------|"
           printf "|       |       |          |%-80s|\n" ""
           printf "|       |       |          |%-80s|\n" " Dir: ./$mdir"
           printf "|       |       |          |%-80s|\n" " Mod: $(convert_mod $mdir)"
           printf "|       |       |          |%-80s|\n" ""
           cat $file_info_tmp
           printf "|       |       |          |%-80s|\n" ""
           printf      "| %5d | %5d |  %5d%%  |%-80s|\n" "$total_pass" "$total_fail" "$dir_percent" " Total"
           printf "|       |       |          |%-80s|\n" ""
           echo ""
        fi
    fi
done

echo ""
echo "|Pass|EFail|Totl|Percent| Module | Directory |"
echo "|----|-----|----|-------|--------|-----------|"
awk -F '|' '{ 
    # Extract the 5th column and the "subfield" for sorting
    #split($5, module_parts, "|"); 
    # Print the sort keys followed by the original line
    print $4 " |" $0; 
}' $chart1 | sort -k1,1n | cut -d '|' -f2-

echo ""
echo ""
echo "|Pass|EFail|Totl|Percent| Module | Directory |"
echo "|----|-----|----|-------|--------|-----------|"
cat $chart1

rm -f $chart1
rm -f $file_info_tmp
rm -f $temp_file

tput smam



