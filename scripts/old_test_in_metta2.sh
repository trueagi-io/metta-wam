#!/bin/bash

#set -xv
#set -e

# One-liner to check if the script is being sourced or run
export IS_SOURCED=$( [[ "${BASH_SOURCE[0]}" != "${0}" ]] && echo 1 || echo 0)

export RUST_BACKTRACE=full
#export PYTHONPATH=./metta_vspace

# Save the directory one above where this script resides
export SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )"

add_to_list() {
    local item="$1"
    local -n list_ref="$2"
    if [[ ! " ${list_ref[*]} " =~ " $item " ]]; then
        list_ref+=("$item")
    fi
}

# Initialize default values
# export variables for later use
export UNITS_DIR
export outer_extra_args
export METTALOG_MAX_TIME
export all_test_args="${@}"
auto_reply=""
generate_report=0
UNITS_DIR="examples/"
outer_extra_args=()
METTALOG_MAX_TIME=75
clean=0  # 0 means dont clean, 1 means do clean
fresh=0
if_failures=1
export RUST_METTA_MAX_TIME=120
GREP_ARGS=""

# Loop through all arguments
while [ "$#" -gt 0 ]; do
  case "$1" in
    -y)
      auto_reply="y"
      shift
      ;;
    -n)
      auto_reply="n"
      shift
      ;;
      --timeout=*)
        METTALOG_MAX_TIME="${1#*=}"
        add_to_list "$1" outer_extra_args
        shift
        ;;
     --report=*)
          generate_report="${1#*=}"
          shift
          ;;
    --*clud*=*)
      add_to_list "$1" outer_extra_args
      GREP_ARGS="${GREP_ARGS} ${1}"
      shift
      ;;

      --clean)
        clean=1
        if_failures=0
        # add_to_list "$1" outer_extra_args
        shift
        ;;

     --cont*)
             clean=0
             if_failures=0
             # add_to_list "$1" outer_extra_args
             shift
             ;;

     --fail*)
            clean=0
            if_failures=1
            # add_to_list "$1" outer_extra_args
            shift
            ;;

     --test)
          # add_to_list "$1" outer_extra_args
          shift
          ;;

     --fresh*)
       fresh=1
       # add_to_list "$1" outer_extra_args
       shift
     ;;



    -h|--help)
      echo "Usage: $0 [options] [directory] [extra args]"
      echo "Options:"
      echo "  -y                 Automatically choose 'y' for rerunning all tests"
      echo "  -n                 Automatically choose 'n'"
      echo "  --fresh             Clean up by deleting any .answers files under directory"
      echo "  --clean            Clean up by deleting all .html files under directory"
      echo "  -h|--help          Display this help message"
      echo " "
      echo "Arguments:"
      echo "  directory          Directory to find tests (current: ${SCRIPT_DIR}/${UNITS_DIR})"
      echo "  extra args         Optional command-line arguments passed to MeTTaLog like:"
      echo "                       --compile=full|true|false   Enable or disable compilation (current: false)"
      echo "                       --timeout=SECONDS           Specify a timeout value in seconds (current: $METTALOG_MAX_TIME)"
      echo " "
      echo "Examples:"
      echo "  # Run under '${SCRIPT_DIR}/examples/compat/sumo' with a 180 second timeout per test"
      echo "  $0 examples/compat/sumo --timeout=60   "
      echo " "
      echo "  # Automatically (chooses 'y') cleans up and runs all tests in default '${SCRIPT_DIR}/examples' directory with a 180 second timeout per test"
      echo "  $0 -y --clean --timeout=180 "
      echo " "
      echo "Note: Arguments can be in any order."
      [[ $IS_SOURCED -eq 1 ]] && return 0 || exit 0
      ;;

    *)
      if [ -d "$1" ] || [ -f "$1" ]; then
        UNITS_DIR="$1"
      else
        add_to_list "$1" outer_extra_args
      fi
      shift
      ;;
  esac
done



function delete_html_files() {
    cd "$SCRIPT_DIR"
    find "${UNITS_DIR}" -name "*.html" -type f -delete -print
}

if [ $clean -eq 1 ]; then
  delete_html_files
fi

file_in_array() {
    local file="$1"
    shift
    local arr=("$@")
    for elem in "${arr[@]}"; do
        [[ "$elem" == "$file" ]] && return 0
    done
    return 1
}

function run_tests() {

    #delete_html_files
    #rsync -avm --include='*.html' -f 'hide,! */' reports/ examples/

    # Initial setup
    cd "$SCRIPT_DIR"
    #find -name "*.answers" -size 0 -delete
    cat /dev/null > TEE.ansi.UNITS

    # Get files
       mapfile -t assert_files < <(find "${UNITS_DIR}" $EXTRA_FIND_ARGS -type f -name '*.metta' -print0 | xargs -0 grep -rl 'assert' -- $GREP_ARGS)
       mapfile -t test_files < <(find "${UNITS_DIR}" $EXTRA_FIND_ARGS -type f -iname "*test*.metta" $GREP_ARGS)
       mapfile -t has_tests < <(find "${UNITS_DIR}" $EXTRA_FIND_ARGS -type f -name '*.metta' -print0 | xargs -0 grep -rl '^!\([^!]*\)$' -- $GREP_ARGS)

    # Filtering out the has_tests from assert_files and test_files
    for htest in "${has_tests[@]}"; do
        assert_files=("${assert_files[@]/$htest}")
        test_files=("${test_files[@]/$htest}")
    done

    # Filtering test_files
    for afile in "${assert_files[@]}"; do
        test_files=("${test_files[@]/$afile}")
    done

    # Concatenate all three collections into a unified collection
    all_files=( "${assert_files[@]}" "${test_files[@]}" "${has_tests[@]}" )

    # Make the collection unique to avoid processing the same file more than once
    readarray -t unique_files < <(printf "%s\n" "${all_files[@]}" | sort -u)

    find_override_file() {
      local filename=$1
      local override_filename="${filename/\/compat\//\/override-compat\/}"

      if [[ -f "$override_filename" ]]; then
         echo "$override_filename"
      else
         echo "$filename"
      fi
    }

    # Shared logic across both file types
    process_file() {
       #local file=$(find_override_file "$1")
       local file="$1"

       local absfile=$(readlink -f "$file")

       local extra_args="${@:2}"
       shift

       file_html="${file}.html"

       echo ""
       echo "Testing:  $file"

        cd "$SCRIPT_DIR"
        echo ""



         # Add unique absolute paths to PYTHONPATH
         pp1=$(realpath "$(dirname "${file}")")
         pp2=$(realpath "$(dirname "${pp1}")")
         for pp in $pp1 $pp2; do
             if [[ ":$PYTHONPATH:" != *":$pp:"* ]]; then
                 export PYTHONPATH="${PYTHONPATH:+"$PYTHONPATH:"}$pp"
             fi
         done

         # Set OPENAI_API_KEY only if it's not already set
         if [ -z "$OPENAI_API_KEY" ]; then
             export OPENAI_API_KEY=freeve
         fi

         # Combined condition check
         if [[ "$fresh" -eq 1 ]] || [ ! -f "${file}.answers" ] || ([ "${file}" -nt "${file}.answers" ] && [ -s "${file}.answers" ]); then
             echo "Regenerating answers:  $file.answers"
             cat /dev/null > "${file}.answers"

             set +e

             # Function to handle SIGINT
             handle_sigint() {
                 echo "SIGINT received, stopping metta but continuing script..."
                 stty sane
             }

             # Trap SIGINT
             trap 'handle_sigint' SIGINT

             ( cd "$(dirname "${file}")" || true

                set -x
               timeout --foreground --kill-after=5 --signal=SIGINT $(($RUST_METTA_MAX_TIME + 1)) time metta "$absfile" 2>&1 | tee "${absfile}.answers"
               TEST_EXIT_CODE=$?
                set +x
              if [ $TEST_EXIT_CODE -eq 124 ]; then
                  echo "Rust MeTTa  Killed (definitely due to timeout) after $RUST_METTA_MAX_TIME seconds: ${TEST_CMD}"
                  [ "$if_failures" -eq 1 ] && rm -f "$file_html"
              elif [ $TEST_EXIT_CODE -ne 0 ]; then
                  echo "Rust MeTTa  Completed with error (EXITCODE=$TEST_EXIT_CODE) under $RUST_METTA_MAX_TIME seconds"
              else
                  echo "Rust MeTTa  Completed successfully (EXITCODE=0) under $RUST_METTA_MAX_TIME seconds"
              fi

             ) || true
            stty sane

             trap - SIGINT

             set -e

         else
             echo "Checked for answers:  $file.answers"
             cat "${file}.answers"
             echo "Using for answers:  $file.answers"
         fi

         local take_test=0

         if [[ ! -f "$file_html" ]]; then
             take_test=1
             echo "Taking test since HTML file does not exist."
         else
             if [ "$clean" -eq 1 ]; then
                 take_test=1
                 echo "Taking test since --clean."
             elif [ "$if_failures" -eq 1 ]; then
               failures_not_zero=$(grep -h -c "Failures: [^0]" "$file_html")
               if [ "$failures_not_zero" -eq 0 ]; then
                     success_missing=0  # Default to 0 (false)

                     # Check if the word "success" is in the file
                     if ! grep -q "Successes: " "$file_html"; then
                         # Set variable if "success" is missing
                         success_missing=1
                     fi

                     # Now you can use success_missing as needed
                     if [ "$success_missing" -eq 1 ]; then
                         echo "The word 'Success' is missing from $file_html."
                        failures_not_zero=1
                     else
                         echo "The word 'Success' is present in $file_html."
                     fi
               fi
               if [ "$failures_not_zero" -eq 1 ]; then
                   take_test=1
                   echo "Retaking test since failures are present."
                   rm -f "$file_html"
               else
                   echo "Not retaking since Failures: 0."
               fi
             else
                 echo "Results present, not taking test."
             fi
         fi

         if [ "$take_test" -eq 1 ]; then
             sleep 0.1
             touch "$file_html"

             TEST_CMD="./MeTTa --timeout=$METTALOG_MAX_TIME --html --repl=false $extra_args $outer_extra_args \"$file\" --halt=true"
             echo "Running command with timeout: $TEST_CMD"

             set +e
             time  $TEST_CMD
            TEST_EXIT_CODE=$?
            # set -e
             echo ""

              if [ $TEST_EXIT_CODE -eq 124 ]; then
                  echo "Killed (definitely due to timeout) after $METTALOG_MAX_TIME seconds: ${TEST_CMD}"
                  [ "$if_failures" -eq 1 ] && rm -f "$file_html"
              elif [ $TEST_EXIT_CODE -ne 0 ]; then
                  echo "Completed with error (EXITCODE=$TEST_EXIT_CODE) under $METTALOG_MAX_TIME seconds: ${TEST_CMD}"
              else
                  echo "Completed successfully (EXITCODE=0) under $METTALOG_MAX_TIME seconds: ${TEST_CMD}"
              fi
              #/scripts/total_loonits.sh
         fi

    }

    # Process assert_files
    #for file in "${assert_files[@]}"; do
    #   [ -f "${file}" ] && process_file "$file"
    #done

   sorted_array=($(for i in "${all_files[@]}"; do
       echo "$i"
   done | sort | uniq | awk 'NF'))

   # Create an empty array for the reversed elements
   reversed_array=()

   # Loop through the original array in reverse order
   for (( i=${#sorted_array[@]}-1; i>=0; i-- )); do
      reversed_array+=("${sorted_array[i]}")
   done

    sorted_array=( "${reversed_array[@]}" )


   # Process test_files
   for file in "${sorted_array[@]}"; do
       if [ -f "${file}" ]; then
           echo ""
           if file_in_array "$file" "${assert_files[@]}"; then
               [ -f "${file}" ] && process_file "$file"
           else
               [ -f "${file}" ] && process_file "$file" "--test-retval=true"
           fi
       fi
   done


}


function generate_final_MeTTaLog() {

    cd "$SCRIPT_DIR"

    passed=$(grep -c "| PASS |" TEE.ansi.UNITS)
    failed=$(grep -c "| FAIL |" TEE.ansi.UNITS)
    total=$((passed + failed))
    percent_passed=$(awk -v passed="$passed" -v total="$total" 'BEGIN { printf "%.2f", (passed/total)*100 }')

    {
    echo " "
    echo "| STATUS | TEST NAME | TEST CONDITION | ACTUAL RESULT | EXPECTED RESULT |"
    echo "|--------|-----------|----------------|---------------|-----------------|"
    } > TEST_LINKS.md

    sort -t'|' -k3 TEE.ansi.UNITS | sed 's/^[ \t]*//' | \
    awk -F '|' -v OFS='|' '{ $4 = substr($4, 1, 200); print }' | \
    awk -F '|' -v OFS='|' '{ $5 = substr($5, 1, 200); print }' | \
    awk -F '|' -v OFS='|' '{ $6 = substr($6, 1, 200); print }' >> TEST_LINKS.md

    echo -e "\n\n\n" >> TEST_LINKS.md

    {
    echo "Test Results:"
    echo "$passed Passed,"
    echo "$failed Failed,"
    echo "$total Total,"
    echo "$percent_passed% Passed"
    } > summary.md

    (
    cat PASS_FAIL.md
    echo " "
    cat TEST_LINKS.md
    cat summary.md
    echo " "
    ) > temp && mv temp TEST_LINKS.md

    rm summary.md

    awk '/# Bugs in MeTTaLog/{exit} 1' MeTTaLog.md > temp1.txt
    awk 'BEGIN{flag=0} /# Installation Guide/{flag=1} flag' MeTTaLog.md > temp2.txt

    cat temp1.txt TEST_LINKS.md temp2.txt > final_MeTTaLog.md

    #cat final_MeTTaLog.md

    # Clean up temporary files
    rm temp1.txt temp2.txt
}




function PreCommitReports() {

    cd "$SCRIPT_DIR"
    echo "Executing Tasks..."
    rsync -avm --include='*.html' -f 'hide,! */' examples/ reports/ \
    && echo "1) Synced HTML files from examples/ to reports/ and deleted the original HTML files in examples/"

    #find examples/ -name '*.html' -delete

    mv final_MeTTaLog.md MeTTaLog.md \
    && echo "2) Renamed final_MeTTaLog.md to MeTTaLog.md"

   # Get current branch name
   branch_name=$(git rev-parse --abbrev-ref HEAD)

   # Get the latest commit SHA
   latest_commit=$(git rev-parse HEAD)

   # Get the current timestamp
   timestamp=$(date +%Y%m%d%H%M%S)

   version_info="${branch_name}_${latest_commit}_${timestamp}"

   # Check if there are uncommitted changes
   if [[ -n $(git status --porcelain) ]]; then
       changes="_with_uncommitted_changes"
   fi

   # Construct the reference string
   version_info="${branch_name}_${latest_commit}_${timestamp}${changes}"

   echo " " >> NewResults.md
   echo $(date) >> NewResults.md
   echo "version_info=$version_info" >> NewResults.md
   echo " " >> NewResults.md
   cat Results.md  >> NewResults.md
   echo "Tasks Completed Successfully."
}


function compare_test_files() {

    if [ "$#" -ne 2 ]; then
        echo "Usage: compare_test_files <file1> <file2>"
        return 1
    fi

    file1="$1"
    file2="$2"

    cd "$SCRIPT_DIR"

    if [ ! -f "$file1" ] || [ ! -f "$file2" ]; then
        echo "Error: One or both of the files do not exist."
        return 1
    fi

    sorted1=$(mktemp)
    sorted2=$(mktemp)

    grep -E '\| PASS \||\| FAIL \|' "$file1" | awk -F'|' '{ gsub(/.*#/, "", $3); print $2, $3 }' | sort > "$sorted1"
    grep -E '\| PASS \||\| FAIL \|' "$file2" | awk -F'|' '{ gsub(/.*#/, "", $3); print $2, $3 }' | sort > "$sorted2"

    cat "$sorted1"

    [ -e Results.md ] && rm Results.md
    touch Results.md

    # Detect new and missing tests
    comm -13 "$sorted1" "$sorted2" > new_tests.md
    comm -23 "$sorted1" "$sorted2" > missing_tests.md

    comm -3 "$sorted1" "$sorted2" | while read -r line; do
        read -r status test <<< "$line"

        status1=$(grep "$test" "$sorted1" | awk '{print $1}' | tr -d ' ')
        status2=$(grep "$test" "$sorted2" | awk '{print $1}' | tr -d ' ')

        if [[ -n "$status1" && -n "$status2" && "$status1" != "$status2" ]]; then
            echo "Now ${status2}ING $test" >> Results.md
        fi
    done

    sort -u Results.md -o Results.md

    echo "New tests:"
    cat new_tests.md
    echo "-----------------------------------------"

    echo "Missing tests:"
    cat missing_tests.md
    echo "-----------------------------------------"

    echo "-----------------------------------------"
    grep 'PASSING' Results.md
    echo "-----------------------------------------"
    grep 'FAILING' Results.md
    echo "-----------------------------------------"

    new_passing=$(grep -c 'PASSING' Results.md)
    new_failing=$(grep -c 'FAILING' Results.md)

    echo "Number of newly PASSING tests: $new_passing"
    echo "Number of newly FAILING tests: $new_failing"

    rm -f "$sorted1" "$sorted2" new_tests.md missing_tests.md
}


(
   cd "$SCRIPT_DIR"

   # If auto_reply is empty, then ask the user
   if [ -z "$auto_reply" ]; then
     read -p "Rerun all tests? (y/N): " -n 1 -r
     echo ""
   else
     REPLY=$auto_reply
   fi


time (
   if [[ $REPLY =~ ^[Yy]$ ]]; then
     run_tests
   else
     echo "You chose not to run all tests."
   fi

   generate_final_MeTTaLog
   compare_test_files ./MeTTaLog.md ./final_MeTTaLog.md   )

   if [ -z "$generate_report" ]; then
      read -p "Are you ready to commit your code and generate unit reports? (y/N): " -n 1 -r
     echo ""
   else
     REPLY=$generate_report
   fi

   echo

   if [[ $REPLY =~ ^[Y]$ ]]; then
     PreCommitReports
   else
     echo "You chose not to commit your code and generate unit reports."
   fi
)

[[ $IS_SOURCED -eq 1 ]] && return 0 || exit 0

