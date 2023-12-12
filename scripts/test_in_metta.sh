#!/bin/bash

# This script performs various testing operations for MeTTaLog.
# It handles command-line arguments to customize its behavior.

# Check if the script is being sourced or run directly
export IS_SOURCED=$( [[ "${BASH_SOURCE[0]}" != "${0}" ]] && echo 1 || echo 0)

export RUST_BACKTRACE=full  # Enable full Rust backtrace for debugging

# Save the directory one level above where this script resides
export SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )"

# Function to add an item to a list if it's not already present
add_to_list() {
    local item="$1"
    local -n list_ref="$2"
    if [[ ! " ${list_ref[*]} " =~ " $item " ]]; then
        list_ref+=("$item")
    fi
}

# Initialize default values and export variables for later use
export UNITS_DIR
export passed_along_to_mettalog
export METTALOG_MAX_TIME
export all_test_args="${@}"
run_tests_auto_reply=""
generate_report_auto_reply=""
UNITS_DIR="examples/"
passed_along_to_mettalog=()
METTALOG_MAX_TIME=75
clean=0  # 0 means don't clean, 1 means do clean
fresh=0
if_failures=1
if_regressions=0
show_help=0
export RUST_METTA_MAX_TIME=120
EXTRA_FIND_ARGS=""
EXTRA_GREP_ARGS=""
explain_only=1

# command-line argument parsing
while [ "$#" -gt 0 ]; do
    case "$1" in
        -y|--yes) run_tests_auto_reply="y" ;;
        -n|--no) run_tests_auto_reply="n" ;;
        --timeout=*) METTALOG_MAX_TIME="${1#*=}" ;;
        --report=*) generate_report_auto_reply="${1#*=}" ;;
        --clean) clean=1; if_failures=0 ;;
        --regres*) clean=0; if_failures=0; if_regressions=1 ;;
        --cont*) clean=0; if_failures=0 ;;
        --fail*) clean=0; if_failures=1 ;;
        --explain) explain_only=1 ;;
        --test) explain_only=0 ;;
        --fresh*) fresh=1 ;;
        --exclude=*) EXTRA_FIND_ARGS+=" ! -path ${1#*=}"; CANT_HAVE="${1#*=}" ;;
        --include=*) EXTRA_FIND_ARGS+=" -path ${1#*=}"; MUST_HAVE="${1#*=}" ;;
        -h|--help) echo "Usage: $0 [options] [directory] [extra args]"; show_help=1; explain_only=1 ;;
        *)
            if [ -d "$1" ] || [ -f "$1" ]; then
                UNITS_DIR="$1"
            else
                add_to_list "$1" passed_along_to_mettalog
            fi
            ;;
    esac
    shift
done




if [  "$show_help" -eq 1 ]; then
      # Help section with detailed usage instructions
      echo "Options:"
      echo "  -y |--yes                Automatically choose 'y' for rerunning all tests"
      echo "  -n|--no                 Automatically choose 'n'"
      echo "  --fresh            Clean up by deleting any .answers files under directory"
      echo "  --clean           Clean up by deleting all .html files under directory"
      echo "  --continue     Continue running tests (Generating any missing html files)"
      echo "  --failures        Rerun unsuccessfull tests only"
      echo "  --timeout=SECONDS  Specify a timeout value in seconds (current: $METTALOG_MAX_TIME)"
      echo "  --report=(Y/N)  Generate a report (if not supplied, will be asked at the end)"
      echo "  --*clud*=PATTERN   Include or exclude tests based on pattern"
      echo "  -h|--help          Display this help message"
      echo ""
      echo "Arguments:"
      echo "  directory          Directory to find .metta file tests (current: ${SCRIPT_DIR}/${UNITS_DIR})"
      echo "  extra args         Optional command-line arguments passed to MeTTaLog"
      echo ""
      echo "Examples:"
      echo "  # Run under '${SCRIPT_DIR}/examples/baseline_compat/hyperon-pln_metta/sumo' with a 60 second timeout per test"
      echo "  $0 examples/ --include \"*sumo/\" --timeout=60"
      echo ""
      echo "  # Automatically (chooses 'y') cleans up and runs all tests in default '${SCRIPT_DIR}/examples' directory with a 180 second timeout per test"
      echo "  $0 -y --clean --timeout=180"
      echo ""
      echo "Note: Arguments can be in any order."
fi

IF_REALLY_DO() {
    if [ "$explain_only" -eq 1 ]; then
        echo "Would be doing: $*"
    else
        echo "Doing: $*"
        eval "$*"
    fi
}


function delete_html_files() {
    if [ -n "${UNITS_DIR}" ]; then  # Check if UNITS_DIR is not empty
        echo "Deleting .metta.html files in $UNITS_DIR"

        local include_pattern=""
        local exclude_pattern=""

        # Extract include and exclude patterns from EXTRA_FIND_ARGS
        for arg in $MUST_HAVE; do
            case "$arg" in
            esac
        done

        # Construct the find command
        local find_cmd="find \"${UNITS_DIR}\" -type f -name \"*.metta.html\""

        # Append include and exclude patterns
        find_cmd+=$EXTRA_FIND_ARGS

        if [ "$explain_only" -eq 1 ]; then
            echo "would be deleting these files: "
            find_cmd+=" -print"
        else
           find_cmd+=" -delete -print"
        fi

        eval $find_cmd
    fi
}


# Delete HTML files if the clean flag is set
if [ $clean -eq 1 ]; then
  delete_html_files
fi

# Function to check if a file is in an array
file_in_array() {
    local file="$1"
    shift
    local arr=("$@")
    for elem in "${arr[@]}"; do
        [[ "$elem" == "$file" ]] && return 0
    done
    return 1
}

# Function to run tests
function run_tests() {
    # Process test files
   BASE_DIR="${UNITS_DIR}"
    echo "Running tests in $BASE_DIR"

      set +v
         echo "Finding files with 'test' in their name and apply $EXTRA_FIND_ARGS ..."
         mapfile -t test_files < <(find "${UNITS_DIR}" $EXTRA_FIND_ARGS -type f -iname "*test*.metta")
         echo "'Test' files found: ${#test_files[@]}"

         echo "Finding files containing 'assert' keyword and apply $MUST_HAVE ..."
         mapfile -t assert_files < <(find "${UNITS_DIR}" $EXTRA_FIND_ARGS -type f -name '*.metta' -print0 | xargs -0 grep -rl 'assert' -- $GREP_ARGS)
         echo "Assert<*> files found: ${#assert_files[@]} "

         echo "Finding files containing execution directive (lines starting with '!') and apply $MUST_HAVE ..."
         mapfile -t has_tests < <(find "${UNITS_DIR}" $EXTRA_FIND_ARGS -type f -name '*.metta' -print0 | xargs -0 grep -rl '^!\([^!]*\)$' -- $GREP_ARGS)
        echo "Test directive files found: ${#has_tests[@]}"


      # Remove empty elements from arrays
      assert_files=("${assert_files[@]}" )
      test_files=("${test_files[@]}" )
      has_tests=("${has_tests[@]}" )

      # Function to filter out elements of array1 that are present in array2
      filter_arrays() {
          local -n array1=$1
          local -n array2=$2
          local temp_array=()
          local skip

          for item1 in "${array1[@]}"; do
              skip=0
              for item2 in "${array2[@]}"; do
                  if [[ "$item1" == "$item2" ]]; then
                      skip=1
                      break
                  fi
              done
              if [[ skip -eq 0 ]]; then
                  temp_array+=("$item1")
              fi
          done

          array1=("${temp_array[@]}")
      }

      # Filter out assert_files from has_tests
      filter_arrays has_tests assert_files
      # Filter out assert_files from test_files
      filter_arrays test_files assert_files
      # Filter out has_tests from test_files
      filter_arrays test_files has_tests
      # Remove empty elements from arrays
      assert_files=("${assert_files[@]}" )
      test_files=("${test_files[@]}" )
      has_tests=("${has_tests[@]}" )

      # for file in "${assert_files[@]}"; do [ -f "${file}" ] && echo assert_files "$file"; done
      # for file in "${test_files[@]}"; do [ -f "${file}" ] && echo test_files "$file"; done
      # for file in "${has_tests[@]}"; do [ -f "${file}" ] && echo has_tests "$file"; done


    # Combine all files and make the collection unique
    all_files=( "${assert_files[@]}" "${test_files[@]}" "${has_tests[@]}" )
    readarray -t unique_files < <(printf "%s\n" "${all_files[@]}" | sort -u)

    # Process each unique file
    for file in "${unique_files[@]}"; do
        if [ -f "${file}" ]; then
          process_file "$file"
        fi
    done
}

# Main execution block
function main() {
    cd "$SCRIPT_DIR"

    # Prompt user to rerun all tests if run_tests_auto_reply is not set
    if [ -z "$run_tests_auto_reply" ]; then
        read -p "Rerun all tests? (y/N): " -n 1 -r
        echo ""
    else
        REPLY=$run_tests_auto_reply
    fi

    # Run tests and generate MeTTaLog report
    if [[ $REPLY =~ ^[Yy]$ ]]; then
       # run our selected tests
        run_tests
        IF_REALLY_DO generate_final_MeTTaLog
    else
        echo "Skipping test run."
    fi

    # Prompt for code commit and unit report generation
    if [ -z "$generate_report_auto_reply" ]; then
        read -p "Commit code and generate unit reports? (y/N): " -n 1 -r
        echo ""
    else
        REPLY=$generate_report_auto_reply
    fi

    if [[ $REPLY =~ ^[Yy]$ ]]; then
        IF_REALLY_DO PreCommitReports
    else
        echo "Skipping report generation."
    fi
}

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
             IF_REALLY_DO cat /dev/null > "${file}.answers"

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
               IF_REALLY_DO timeout --foreground --kill-after=5 --signal=SIGINT $(($RUST_METTA_MAX_TIME + 1)) time metta "$absfile" 2>&1 | tee "${absfile}.answers"
               TEST_EXIT_CODE=$?
                #set +x
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
             echo "Using for answers:  $file.answers"
         fi

         local take_test=0

         if [ "$if_regressions" -eq 1 ]; then
             if [[ ! -f "$file_html" ]]; then
                 echo "Not taking test since HTML file does not exist."
                 return
             fi

             failures_zero=$(grep -h -c "Failures: 0" "$file_html")
             if [ "$failures_zero" -ne 0 ]; then
                 echo "Not taking test since Failures not 0."
                 return
             fi
             take_test=1
             echo "Taking test since Failures: 0 and looking for regressions."
         elif [[ ! -f "$file_html" ]]; then
             take_test=1
             echo "Taking test since HTML file does not exist."
         else
             if [ "$clean" -eq 1 ]; then
                 take_test=1
                 echo "Taking test since --clean."
             elif [ "$if_failures" -eq 1 ]; then
                 failures_not_zero=$(grep -h -c "Failures: [^0]" "$file_html")
                 if [ "$failures_not_zero" -eq 0 ]; then
                     success_missing=0

                     if ! grep -q "Successes: " "$file_html"; then
                         success_missing=1
                     fi

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
                     IF_REALLY_DO rm -f "$file_html"
                 else
                     echo "Not retaking since Failures: 0."
                 fi
             else
                 echo "Results present, not taking test."
             fi
         fi

         if [ "$take_test" -eq 1 ]; then
             sleep 0.1
             IF_REALLY_DO  touch "$file_html"

             TEST_CMD="./MeTTa --timeout=$METTALOG_MAX_TIME --html --repl=false $extra_args $passed_along_to_mettalog \"$file\" --halt=true"
             echo "Running command with timeout: $TEST_CMD"

             set +e
             IF_REALLY_DO time  $TEST_CMD
            TEST_EXIT_CODE=$?
            # set -e
             echo ""

              if [ $TEST_EXIT_CODE -eq 124 ]; then
                  echo "Killed (definitely due to timeout) after $METTALOG_MAX_TIME seconds: ${TEST_CMD}"
                  IF_REALLY_DO [ "$if_failures" -eq 1 ] && rm -f "$file_html"
              elif [ $TEST_EXIT_CODE -ne 0 ]; then
                  echo "Completed with error (EXITCODE=$TEST_EXIT_CODE) under $METTALOG_MAX_TIME seconds: ${TEST_CMD}"
              else
                  echo "Completed successfully (EXITCODE=0) under $METTALOG_MAX_TIME seconds: ${TEST_CMD}"
              fi
              #/scripts/total_loonits.sh
         fi
}


function generate_final_MeTTaLog() {
    # Change to the script directory
    cd "$SCRIPT_DIR" || exit 1

    # Calculate the number of passed and failed tests
    passed=$(grep -c "| PASS |" /tmp/SHARED.UNITS)
    failed=$(grep -c "| FAIL |" /tmp/SHARED.UNITS)
    total=$((passed + failed))
    percent_passed=$(awk -v passed="$passed" -v total="$total" 'BEGIN { printf "%.2f", (passed/total)*100 }')

    # Create a markdown file with test links and headers
    {
        echo " "
        echo "| STATUS | TEST NAME | TEST CONDITION | ACTUAL RESULT | EXPECTED RESULT |"
        echo "|--------|-----------|----------------|---------------|-----------------|"
        cat /tmp/SHARED.UNITS | awk -F'cuRRent|\\) \\| \\(' '{ print $2 " " $0 }'  | sort | cut -d' ' -f2- | tac | awk '!seen[$0]++' | tac
    } > ./examples/PASS_FAIL.md

   ./scripts/pass_fail_totals.sh examples/ > ./examples/TEST_LINKS.md

}



function PreCommitReports() {

    cd "$SCRIPT_DIR"
    echo "Executing Tasks..."
    rsync -avm --include='*.metta.html' -f 'hide,! */' examples/ reports/cuRRent/ \
    && echo "1) Synced HTML files from examples/ to reports/cuRRent/ and deleted the original HTML files in examples/"
    \cp -f examples/PASS_FAIL.md reports/PASS_FAIL.md
    \cp -f examples/TEST_LINKS.md reports/TEST_LINKS.md
    #mv final_MeTTaLog.md MeTTaLog.md \
    #&& echo "2) Renamed final_MeTTaLog.md to MeTTaLog.md"

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

main



