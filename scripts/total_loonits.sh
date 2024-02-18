#!/bin/bash

export UNITS_DIR="${1:-examples/}"
export FOUND_UNITS=/tmp/found_units
trap 'rm -f "$FOUND_UNITS.sortme"' EXIT
declare -A dir_totals

function initialize_counters() {
    total_successes=0
    total_failures=0
}

function analyze_files() {
    cat /dev/null > $FOUND_UNITS.sortme
    find "${UNITS_DIR}" -name "*.metta.html" -type f > $FOUND_UNITS

    while read -r file; do
        process_file "$file"
    done < $FOUND_UNITS

    rm $FOUND_UNITS
    sort_and_calculate_totals

       cat $FOUND_UNITS.sortme
}

function process_file() {
    local file="$1"
    local current_successes current_failures relative_path basename github_link directory

    current_successes=$(get_current_successes "$file")
    current_failures=$(get_current_failures "$file")

    [ -z "$current_successes" ] && current_successes=0
    [ -z "$current_failures" ] && current_failures=0

    relative_path=$(echo "$file" | sed 's/^\.\///' | sed -e 's|examples|reports|g' -e 's|-reports|-examples|g')
    basename=$(basename $relative_path)
    basename=${basename%.*}
    directory=$(dirname $relative_path)
    github_link="${base_url}${relative_path}"

    printf "|%-5s|%-5s|%-35s|%-130s|\n" "  $current_successes" "  $current_failures" " ${basename}" "[$relative_path]($github_link)" >> $FOUND_UNITS.sortme

    accumulate_totals "$directory" $current_successes $current_failures
}

function accumulate_totals() {
    local directory=$1
    local current_successes=$2
    local current_failures=$3

    while [ "$directory" != "." ]; do
        dir_totals["$directory,success"]=$(( ${dir_totals["$directory,success"]} + current_successes ))
        dir_totals["$directory,failure"]=$(( ${dir_totals["$directory,failure"]} + current_failures ))

        if [[ $directory == *""* ]]; then
            total_successes=$((total_successes + current_successes))
            total_failures=$((total_failures + current_failures))
        fi

        directory=$(dirname "$directory")
    done
}

function get_current_successes() {
   local file="$1"

   # First, try getting the successes count using get_current_successes1
   local count_from_successes1=$(get_current_successes1 "$file")

   # Check if the count from get_current_successes1 is greater than zero
   if (( count_from_successes1 > 0 )); then
       echo "$count_from_successes1"
       return
   fi

   # If not, proceed with the original logic
    relative_path=$(echo "$file" | sed 's/^\.\///' | sed -e 's|examples|reports|g' -e 's|-reports|-examples|g')
    basename=$(basename "$relative_path")
    basename=${basename%.*}
    echo $(grep "$basename" /tmp/SHARED.UNITS | grep -c "| PASS |")
}



function get_current_failures() {
   local file="$1"

   # First, try getting the failures count using get_current_failures1
   local count_from_failures1=$(get_current_failures1 "$file")

   # Check if the count from get_current_failures1 is greater than zero
   if (( count_from_failures1 > 0 )); then
       echo "$count_from_failures1"
       return
   fi

   # If not, proceed with the original logic
    local file="$1"
    relative_path=$(echo "$file" | sed 's/^\.\///' | sed -e 's|examples|reports|g' -e 's|-reports|-examples|g')
    basename=$(basename "$relative_path")
    basename=${basename%.*}
    echo $(grep "$basename" /tmp/SHARED.UNITS | grep -c "| FAIL |")
}

function get_current_successes1() {
    local file="$1"
    cat "$file" | sed 's/\x1b\[[0-9;]*m//g' | grep 'Successes:' | awk -F: '{sum += $2} END {print sum}'
}

function get_current_failures1() {
    local file="$1"
    cat "$file" | sed 's/\x1b\[[0-9;]*m//g' | grep 'Failures:' | awk -F: '{sum += $2} END {print sum}'
}

function sort_and_calculate_totals() {
    sort -t'|' -k3,3nr -k2,2nr -k4,4 -o $FOUND_UNITS.sortme  $FOUND_UNITS.sortme
    awk -F'|' 'NR>1{print ( $3+$2 ) ( $2-$3 >= 0 ? $2-$3 : $3-$2 ) ,$0}' FOUND_UNITS.sortme | sort -n | cut -f3- -d' ' > FOUND_UNITS.sorted
    mv FOUND_UNITS.sorted FOUND_UNITS.sortme

    print_report
}

function print_report() {
    local percent_successes="$1"
    echo "# Bugs in MeTTaLog" > PASS_FAIL.md
    echo "" >> PASS_FAIL.md
    echo "| Pass | Fail |Percent| Directory                                        |" >> PASS_FAIL.md
    echo "|------|------|-------|--------------------------------------------------|" >> PASS_FAIL.md

    (
        for key in "${!dir_totals[@]}"; do
            if [[ $key == *",success" ]]; then
                directory=${key%,success}
                success=${dir_totals[$key]}
                failure=${dir_totals["$directory,failure"]}
                total=$((success + failure))
                if [ $total -ne 0 ]; then
                    percent=$(( 100 * success / total ))
                else
                    percent=0
                fi

                # Center aligning the numbers
                field_width=6 # 6 characters wide for Pass and Fail columns
                percent_width=7 # 7 characters wide for Percent column

                success_padding=$(( (field_width + ${#success}) / 2 ))
                failure_padding=$(( (field_width + ${#failure}) / 2 ))
                percent_padding=$(( (percent_width + ${#percent}) / 2 ))

                printf "|%${field_width}s|%${field_width}s|%${percent_width}s|%-50s|\n" \
                    "$success" "$failure" "$percent%" " $directory - "
            fi
        done
    ) | sort -t'_' -k3,3r -k2,2  >> PASS_FAIL.md

    echo "" >> PASS_FAIL.md

    cat PASS_FAIL.md
    echo "<details><summary>Expand for Core Summaries</summary>" >> PASS_FAIL.md
    echo "" >> PASS_FAIL.md
    echo "|Pass |Fail |File                               |GitHub Link                                                                                                                       |" >> PASS_FAIL.md
    echo "|-----|-----|-----------------------------------|----------------------------------------------------------------------------------------------------------------------------------|" >> PASS_FAIL.md
    cat $FOUND_UNITS.sortme >> PASS_FAIL.md
    echo "" >> PASS_FAIL.md
    echo "</details>" >> PASS_FAIL.md

}


function main() {

   if [[ ! -d "$UNITS_DIR" ]]; then
    echo "Error: Provided UNITS_DIR '$UNITS_DIR' is not a directory or does not exist." >&2
    exit 1
   fi
    base_url="https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/"
    initialize_counters
    analyze_files
}

main

