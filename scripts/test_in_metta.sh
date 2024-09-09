#!/bin/bash

SHOULD_EXIT=0

DEBUG_WHY() {
   DEBUG "${GREEN}WHY: ${BOLD}${*}${NC}"
}


process_file() {

    [[ $SHOULD_EXIT -eq 1 ]] && return

    #local file=$(find_override_file "$1")
    local file="$1"

    # Check if the file path contains a tilde
    if [[ "$file" == *"~"* ]]; then
       return 7
    fi

    local absfile=$(readlink -f "$file")

    local extra_args="${@:2}"
    shift

    export file_html="${METTALOG_OUTPUT}/${file}.html"

    export METTALOG_OUTPUT="${METTALOG_OUTPUT}"

    export HTML_OUT="${file}.html"

    DEBUG "==========================================================================="
    DEBUG "${BLUE}${BOLD}===========================================================================${NC}"
    DEBUG "${BOLD}"
    DEBUG ""
    DEBUG "Testing: $file"
    cd "$METTALOG_DIR"
    DEBUG "Output: $file_html"
    # Check if the file path contains a tilde
    if [[ "$absfile" == *"~"* ]]; then
       DEBUG "${RED}Warn on tilda'd path?${NC}"
    fi
    DEBUG ""
    DEBUG ""
    DEBUG "${BLUE}${BOLD}===========================================================================${NC}"
    DEBUG "==========================================================================="

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

    local take_test=0
    local TEST_EXIT_CODE=0
        set -e

    # Combined condition check
    if [[ "$fresh" -eq 1 ]] || [ ! -f "${file}.answers" ] || ([ "${file}" -nt "${file}.answers" ] && [ -s "${file}.answers" ]); then
        DEBUG_WHY "${YELLOW}Regenerating answers: $file.answers${NC}"
        #IF_REALLY_DO cat /dev/null > "${file}.answers"
        IF_REALLY_DO rm -f "${file}.answers"
       # git checkout "${file}.answers"

        # Function to handle SIGINT
        handle_sigint() {
            do_DEBUG "${RED}SIGINT received, stopping metta but continuing script...${NC}"
            stty sane
        }

        # Trap SIGINT
        trap 'handle_sigint' SIGINT

        (cd "$(dirname "${file}")" || true

            set +x
            IF_REALLY_DO "timeout --foreground --kill-after=5 --signal=SIGINT $(($RUST_METTA_MAX_TIME + 1)) time metta '$absfile' 2>&1 | tee '${absfile}.answers'"
            TEST_EXIT_CODE=$?
            take_test=1
            #set +x
            if [ $TEST_EXIT_CODE -eq 124 ]; then
                DEBUG "${RED}Rust MeTTa Killed (definitely due to timeout) after $RUST_METTA_MAX_TIME seconds: ${TEST_CMD}${NC}"
                [ "$if_failures" -eq 1 ] && rm -f "$file_html"
            elif [ $TEST_EXIT_CODE -ne 0 ]; then
                DEBUG "${RED}Rust MeTTa Completed with error (EXITCODE=$TEST_EXIT_CODE) under $RUST_METTA_MAX_TIME seconds${NC}"
            else
                DEBUG "${GREEN}Rust MeTTa Completed successfully (EXITCODE=0) under $RUST_METTA_MAX_TIME seconds${NC}!"		
            fi	    

        ) || true
        stty sane
	DEBUG ""        

        set -e

       trap - SIGINT
    else
        DEBUG "Comparing: $file.answers"
    fi

    if [ "$if_regressions" -eq 1 ]; then
        if [[ ! -f "$file_html" ]]; then
            DEBUG_WHY "Not taking test since HTML file does not exist."
            return
        fi

        failures_zero=$(grep -h -c "Failures: 0" "$file_html")
        if [ "$failures_zero" -eq 0 ]; then
            DEBUG_WHY "Not taking test since Failures not 0."
            return
        fi
        take_test=1
        DEBUG_WHY "Taking test since Failures: 0 and looking for regressions."
    elif [[ ! -f "$file_html" ]]; then
        take_test=1
        DEBUG_WHY "Taking test since HTML file does not exist."
    else
        if [ "$clean" -eq 1 ]; then
            take_test=1
            DEBUG_WHY "Taking test since --clean."
        elif [ "$if_failures" -eq 1 ]; then
            failures_not_zero=$(grep -h -c "Failures: [^0]" "$file_html")
            if [ "$failures_not_zero" -eq 0 ]; then
                success_missing=0

                if ! grep -q "Successes: " "$file_html"; then
                    success_missing=1
                fi

                if [ "$success_missing" -eq 1 ]; then
                    DEBUG_WHY "Retaking Test since the word 'Success' is missing from $file_html."
                    take_test=1
                else
                    DEBUG_WHY "The word 'Success' is present in $file_html."
                fi
            fi
            if [ "$failures_not_zero" -gt 0 ]; then
                take_test=1
                DEBUG_WHY "Retaking test since failures are present."
                IF_REALLY_DO rm -f "$file_html"
            else
                if [ "$take_test" -eq 0 ]; then
                    DEBUG_WHY "Not retaking since Failures: 0."
                fi
            fi
        else
            DEBUG_WHY "Results present, not taking test."
        fi
    fi

    set +e

    if [ "$take_test" -eq 1 ]; then
        sleep 0.1
        IF_REALLY_DO touch "$file_html"

        TEST_CMD="./mettalog '--output=$METTALOG_OUTPUT' --timeout=$METTALOG_MAX_TIME --html --repl=false ${extra_args[@]} ${passed_along_to_mettalog[@]} \"$file\" --halt=true"
        # DEBUG "${BOLD}$TEST_CMD${NC}"


            EXTRA_INFO="Under $METTALOG_MAX_TIME seconds"

            # Start the timer
            local START_TIME=$(date +%s)

            # Run the test command using eval to handle the single string properly
            IF_REALLY_DO eval "$TEST_CMD"
            local TEST_EXIT_CODE=$?

            # Stop the timer and calculate elapsed time
            local END_TIME=$(date +%s)
            local ELAPSED_TIME=$((END_TIME - START_TIME))

            # Determine the test status based on the exit code
            local DEBUG_MESSAGE
            local PASS_OR_FAIL
            local SHOULD_DELETE_HTML=0

        if [ $TEST_EXIT_CODE -eq 124 ]; then
                DEBUG_MESSAGE="${RED}Killed (definitely due to timeout) (EXITCODE=$TEST_EXIT_CODE) after $EXTRA_INFO seconds: $TEST_CMD${NC}"
                [ "$if_failures" -eq 1 ] && SHOULD_DELETE_HTML=1
                PASS_OR_FAIL="FAIL"
        elif [ $TEST_EXIT_CODE -eq 134 ]; then
                DEBUG_MESSAGE="${RED}Test aborted by user (EXITCODE=$TEST_EXIT_CODE) $EXTRA_INFO: $TEST_CMD${NC}"
                SHOULD_DELETE_HTML=1
                PASS_OR_FAIL="FAIL"
        elif [ $TEST_EXIT_CODE -eq 4 ]; then
                DEBUG_MESSAGE="${RED}Stopping tests (EXITCODE=$TEST_EXIT_CODE) $EXTRA_INFO: $TEST_CMD${NC}"
                SHOULD_DELETE_HTML=1
                PASS_OR_FAIL="FAIL"
		exit 4
        elif [ $TEST_EXIT_CODE -ne 7 ]; then
                DEBUG_MESSAGE="${YELLOW}Completed (EXITCODE=$TEST_EXIT_CODE) $EXTRA_INFO: $TEST_CMD${NC}"
                PASS_OR_FAIL="FAIL"
        else
                DEBUG_MESSAGE="${GREEN}Completed successfully (EXITCODE=$TEST_EXIT_CODE) $EXTRA_INFO: $TEST_CMD${NC}"
                PASS_OR_FAIL="PASS"
        fi

            # Generate the test name in the format WHOLE-TESTS.ParentDirectory.File
            local PARENT_DIR=$(basename "$(dirname "$file")")
            local BASE_FILE=$(basename "$file" .metta)  # Replace .metta with the correct file extension
            local TEST_NAME="WHOLE-TESTS.$PARENT_DIR.$BASE_FILE"

            # Generate the HTML link
            local HTML_LINK="file://$file_html#${TEST_NAME}"

            # Determine if the HTML file should be used as the logfile or a separate .log file should be created
            local LOGFILE
            if [ $SHOULD_DELETE_HTML -eq 1 ]; then
                # Create a separate .log file since the HTML file is planned for deletion
                LOGFILE="${file_html}.log"
                cp "$file_html" "$LOGFILE"
            else
                # Use the HTML file as the logfile since it won't be deleted
                LOGFILE="$file_html"
            fi

            # Redirect debug messages to both the logfile and console
            echo "$DEBUG_MESSAGE" | tee -a "$LOGFILE"

            # Write the line to "$SHARED_UNITS"
            echo "| $TEST_NAME | $PASS_OR_FAIL | [$TEST_NAME]($HTML_LINK) | $TEST_CMD | $TEST_EXIT_CODE | 7 | $ELAPSED_TIME | $LOGFILE |" >> "${SHARED_UNITS}"

            # Delete the HTML file if it was planned for deletion
            if [ $SHOULD_DELETE_HTML -eq 1 ]; then
                rm -f "$file_html"
            fi

        return $TEST_EXIT_CODE
        #set -e
    fi
}

# This script performs various testing operations for MeTTaLog.
# It handles command-line arguments to customize its behavior.

# Check if the script is being sourced or run directly
IS_SOURCED=$( [[ "${BASH_SOURCE[0]}" != "${0}" ]] && echo 1 || echo 0)
# Save the directory one level above where this script resides
METTALOG_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )"

passed_along_to_mettalog=()
METTALOG_MAX_TIME=45

SCRIPT_NAME=$(basename "$0")
run_tests_auto_reply=""
generate_report_auto_reply=""
METTALOG_OUTPUT="reports/tests_output/testrun_$(date +%Y%m%d_%H%M%S)"
fresh=0
clean=0  # 0 means don't clean, 1 means do clean
if_failures=0
if_regressions=0
show_help=0
RUST_METTA_MAX_TIME=60
EXTRA_FIND_ARGS=" ! -path '*/.*' ! -path '*~*' ! -size +10M"
EXTRA_GREP_ARGS=""
dry_run=0
debug_this_script=true


# Filters to exclude names starting with a dot, containing a tilde,
# starting with an underscore, and for files, excluding those not ending with 'metta'
DIR_FILTER=("^\." ".*~.*" "^_")
FILE_FILTER=("^\." ".*~.*" "^_" "!.*\.metta$")

# ANSI escape codes for colors
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
GREEN='\033[0;32m'
BOLD='\033[1m'
# ANSI escape code to reset color
NC='\033[0m' # No Color

do_DEBUG() {
    # Calculate the screen width and 74% of it
    local screen_width=$(tput cols)
    local threshold=$((screen_width * 95 / 100))

    # Construct the debug message
    local msg="; ${YELLOW}$SCRIPT_NAME${NC} $*"

    # Calculate the length of the debug message
    local msg_length=${#msg}

    if [ "$msg_length" -gt "$threshold" ]; then
      # If the message is longer than 74% of the screen width,
      # print a newline before and after the message
      echo >&2
      echo -e "$msg" >&2
      echo >&2
    else
      # If the message is not longer than 74% of the screen width, print it as usual
      echo -e "$msg" >&2
    fi
}

DEBUG() {
  if [ "$debug_this_script" == "true" ]; then
     do_DEBUG "$@"
  else 
     if [ "$dry_run" -eq 1 ]; then
        do_DEBUG "$@"
     fi
  fi
}

IF_REALLY_DO() {
    if [ "$dry_run" -eq 1 ]; then
        do_DEBUG "${BOLD}Dry Run:${NC} $*"
    else
        DEBUG "${GREEN}Doing:${NC} $*"
        eval "$@" 
	return $?
    fi
}

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
# Function to add an item to a list if it's not already present
add_to_list() {
    local item="$1"
    local -n list_ref="$2"
    if [[ ! " ${list_ref[*]} " =~ " $item " ]]; then
        list_ref+=("$item")
    fi
}

# Initialize arrays to hold the files to be tested, excluded, and containing '!'
declare -a files_to_test
declare -a excluded_files
declare -a excluded_dirs
declare -a files_with_exclamation

# Function to check if the filename matches the filter
matches_filter() {
    local filename="$(basename "$1")"
    local -n filter=$2
    for pattern in "${filter[@]}"; do
        if [[ "$pattern" == "!"* ]]; then
            pattern="${pattern#!}"  # Remove the '!' from the start
            if [[ "$filename" =~ $pattern ]]; then
                return 1  # File should be excluded
            fi
        else
            if [[ "$filename" =~ $pattern ]]; then
                return 0  # File matches the filter
            fi
        fi
    done
    return 1  # File does not match the filter
}

# More script functionality...

# Note: Instead of 'exporting' variables, just set them. Only export if necessary for subprocesses.

# Main function to handle files and directories based on patterns
find_test_masks() {
    shopt -s globstar nullglob  # Enable recursive glob patterns
    for pattern_or_path in "$@"; do  # Iterate over all arguments
        for file in $pattern_or_path; do
            if [ -f "$file" ]; then
                if matches_filter "$file" FILE_FILTER; then
                    excluded_files+=("$file")
                else
                    files_to_test+=("$file")
                    # Check if file contains lines starting with "!"
                    if grep -q '^!' "$file"; then
                        files_with_exclamation+=("$file")
                    fi
                fi
            elif [ -d "$file" ]; then
                if matches_filter "$(basename "$file")" DIR_FILTER; then
                    excluded_dirs+=("$file")
                else
                    DEBUG "Directory: $file"
		    while IFS= read -r -d $'\0' subfile; do
                        if [ -f "$subfile" ] && matches_filter "$(basename "$subfile")" FILE_FILTER; then
                            excluded_files+=("$subfile")
                        else
                            files_to_test+=("$subfile")
                            # Check if file contains lines starting with "!"
                            if grep -q '^!' "$subfile"; then
                                files_with_exclamation+=("$subfile")
                            fi
                        fi
                    done < <(find "$file" -type f -print0)		    
                fi
            fi
        done
    done
    shopt -u globstar nullglob  # Disable recursive glob patterns after use
}


# Function to run tests
function add_test_units_dir() {
    # Process test files
    local BASE_DIR="${1}"
    DEBUG "Running tests in $BASE_DIR"

      set +v
         DEBUG "Finding files with 'test' in their name and apply $EXTRA_FIND_ARGS ..."
         mapfile -t test_files < <(find "${BASE_DIR}" $EXTRA_FIND_ARGS -type f -iname "*test*.metta")
         DEBUG "'Test' files found: ${#test_files[@]}"

         DEBUG "Finding files containing 'assert' keyword and apply $MUST_HAVE ..."
         mapfile -t assert_files < <(find "${BASE_DIR}" $EXTRA_FIND_ARGS -type f -name '*.metta' -print0 | xargs -0 grep -rl 'assert' -- $GREP_ARGS)
         DEBUG "Assert<*> files found: ${#assert_files[@]} "

         DEBUG "Finding files containing execution directive (lines starting with '!') and apply $MUST_HAVE ..."
         mapfile -t has_tests < <(find "${BASE_DIR}" $EXTRA_FIND_ARGS -type f -name '*.metta' -print0 | xargs -0 grep -rl '^!\([^!]*\)$' -- $GREP_ARGS)
        DEBUG "Test directive files found: ${#has_tests[@]}"


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


    # Combine all files and make the collection unique
    all_files=( "${files_to_test[@]} ${assert_files[@]}" "${test_files[@]}" "${has_tests[@]}" )
    readarray -t unique_files < <(printf "%s\n" "${all_files[@]}" | sort -u)
    
    # add each unique file
    for file in "${unique_files[@]}"; do
        if [ -f "${file}" ]; then
                if matches_filter "$file" FILE_FILTER; then
                    excluded_files+=("$file")
                else
		    if matches_filter "$file" DIR_FILTER; then
			excluded_files+=("$file")
		    else
			add_to_list "$file" files_to_test
		    fi
		fi
        fi
    done
}


generate_final_MeTTaLog() {
    # Change to the script directory
    cd "$METTALOG_DIR" || exit 1

    if [ 1 -eq 0 ]; then
	python3 ./scripts/into_junit.py "${SHARED_UNITS}" > "$METTALOG_OUTPUT/junit.xml"

	junit2html "$METTALOG_OUTPUT/junit.xml"
	junit2html "$METTALOG_OUTPUT/junit.xml" --summary-matrix
	echo "saved to $METTALOG_OUTPUT/junit.xml.html"
    fi

    # Calculate the number of passed and failed tests
    passed=$(grep -c "| PASS |" "${SHARED_UNITS}")
    failed=$(grep -c "| FAIL |" "${SHARED_UNITS}")
    total=$((passed + failed))

    # Check if total is zero to avoid divide by zero error
    if [ "$total" -eq 0 ]; then
        percent_passed="N/A" # Or set to 0.00 or some default value
    else
        percent_passed=$(awk -v passed="$passed" -v total="$total" 'BEGIN { printf "%.2f", (passed/total)*100 }')
    fi


    # Create a markdown file with test links and headers
    {   echo "| TEST NAME | STATUS | URL LOCATION | TEST CONDITION | ACTUAL RESULT | EXPECTED RESULT |"
        echo "|-----------|--------|--------------|----------------|---------------|-----------------|"
        cat "${SHARED_UNITS}" | awk -F'\\(|\\) \\| \\(' '{ print $1 " " $0 }' | sort | cut -d' ' -f2- | tac | awk '!seen[$0]++' | tac
    } > ./$METTALOG_OUTPUT/PASS_FAIL.md


   ./scripts/pass_fail_totals.sh $METTALOG_OUTPUT/ > $METTALOG_OUTPUT/TEST_LINKS.md
   printf '%s\n' "${@}" > "$METTALOG_OUTPUT/_REPORT_.md"
   cat $METTALOG_OUTPUT/TEST_LINKS.md | sed -e "s|$METTALOG_OUTPUT|reports|g" \
   | sed -e "s|Directory:     ./reports/tests/|D: |g" >> "$METTALOG_OUTPUT/_REPORT_.md"
   ./scripts/html_pass_fail.sh $METTALOG_OUTPUT/ > $METTALOG_OUTPUT/REPORT.html

}


# Function to delete .html files from the included_files list
delete_html_files() {
    for file in "${files_to_test[@]}"; do
        if [ -f "${METTALOG_OUTPUT}/${file}.html" ]; then
  	    IF_REALLY_DO rm -f "${METTALOG_OUTPUT}/${file}.html"
        fi
    done
}

# Function to check if a file is in an array
run_tests() {
    for file in "${files_to_test[@]}"; do
	[[ $SHOULD_EXIT -eq 1 ]] && return
        if [ -f "${file}" ]; then
            process_file "$file"
        else
            do_DEBUG "${RED}File does not exist:${NC} $file"
        fi
    done
}




show_help() {
    # Help section with detailed usage instructions
    echo "Options:"
    echo "  -y|--yes                   Automatically choose 'y' for rerunning all tests"
    echo "  -n|--no                    Automatically choose 'n'"
    echo "  --fresh                    Clean up by deleting any .answers files under directory"
    echo "  --clean                    Clean up by deleting all .html files under directory"
    echo "  --continue                 Continue running tests (Generating any missing html files)"
    echo "  --failures                 Rerun unsuccessful tests only"
    echo "  --timeout=SECONDS          Specify a timeout value in seconds (current: $METTALOG_MAX_TIME)"
    echo "  --report=(Y/N)             Generate a report (if not supplied, will be asked at the end)"
    echo "  --output=DIR               Specify the output directory (current: $METTALOG_OUTPUT)"
    echo "  --[in|ex]clude=PATTERN     Include or exclude tests based on pattern"
    echo "  --dry-run                  Simulate the execution without actually running the tests"
    echo "  -h|--help                  Display this help message"
    echo ""
    echo "Arguments:"
    echo "  directory                   Directory to find .metta file tests (current: ${METTALOG_DIR}/${UNITS_DIR})"
    echo "  extra args                  Optional command-line arguments passed to MeTTaLog"
    echo ""
    echo "Examples:"
    echo "  # Run under '${METTALOG_DIR}/$METTALOG_OUTPUT/baseline_compat/hyperon-pln_metta/sumo' with a 60 second timeout per test"
    echo "  $0 $METTALOG_OUTPUT/ --include \"*sumo/\" --timeout=60"
    echo ""
    echo "  # Automatically (chooses 'y') cleans up and runs all tests in default '${METTALOG_DIR}/examples' directory with a 180 second timeout per test"
    echo "  $0 -y --clean --timeout=180"
    echo ""
    echo "Note: Arguments can be in any order."
}


# Function to extract directories and their parents from the files_to_test array
# Function to extract all parent directories for each file
extract_all_parent_directories() {
    declare -A directory_map  # Use an associative array to avoid duplicate entries

    for filepath in "${files_to_test[@]}"; do
        # Extract the directory path of the current file
        current_dir=$(dirname "$filepath")

        # Traverse up the directory path until the root or predefined level
        while [[ "$current_dir" != "." && "$current_dir" != "/" && "$current_dir" != "tests" ]]; do  
	  # Change "/" to a different level if needed
            # Add the directory to the map
	    # DEBUG "CD $current_dir"
            directory_map["$current_dir"]=1
            # Move up to the parent directory
            current_dir=$(dirname "$current_dir")
        done
    done

    # Convert the associative array keys to an indexed array
    unique_directories=("${!directory_map[@]}")
}

# Function to sort directories by their depth
sort_directories_by_depth() {
    # Use array to store sorted directories
    IFS=$'\n' read -d '' -r -a sorted_directories1 < <(for dir in "${unique_directories[@]}"; do
        echo "$dir"
    done | sort )

    # Assign sorted directories back to unique_directories
    unique_directories=("${sorted_directories1[@]}")

    # Use array to store sorted directories
    IFS=$'\n' read -d '' -r -a sorted_directories2 < <(for dir in "${unique_directories[@]}"; do
        echo "$dir"
    done | awk -F'/' '{print NF-1, $0}' | sort -nr | cut -d' ' -f2-)

    # Assign sorted directories back to unique_directories
    unique_directories=("${sorted_directories2[@]}")
}


PYSWIP_VERSION="main"

# Check if the file exists and Read the first line from the file
VERSION_FILE="$METTALOG_DIR/src/version-config"
if [ -f "$VERSION_FILE" ]; then    
    read -r FIRST_LINE < "$VERSION_FILE"
    FIRST_LINE="${FIRST_LINE%"${FIRST_LINE##*[![:space:]]}"}" 
    if [ ! -z "$FIRST_LINE" ]; then
	if [ -d "$METTALOG_DIR/src/$FIRST_LINE/" ]; then
	    PYSWIP_VERSION="$FIRST_LINE"
	fi
    fi
fi

cd "$METTALOG_DIR"

# command-line argument parsing
while [ "$#" -gt 0 ]; do
    case "$1" in
        -y|--yes) run_tests_auto_reply="y" ;;
        -n|--no) run_tests_auto_reply="n" ;;
        --timeout=*) METTALOG_MAX_TIME="${1#*=}" ;;
	--output=*) METTALOG_OUTPUT="${1#*=}"  ;;
        --report=*) generate_report_auto_reply="${1#*=}" ;;
        --clean) clean=1; if_failures=0 ;;
        --regression*) clean=0; if_failures=0; if_regressions=1 ;;
        --continu*) clean=0; if_failures=0 ;;	
        --failure*) clean=0; if_failures=1 ;;
        --dry-run) dry_run=1 ;;
        --test) dry_run=0 ; add_to_list "$1" passed_along_to_mettalog ;;	    
        --fresh) fresh=1 ;;
        --v=*) PYSWIP_VERSION="${1#*=}" ; add_to_list "$1" passed_along_to_mettalog ;;
        --exclude=*) EXTRA_FIND_ARGS+=" ! -path ${1#*=}"; CANT_HAVE="${1#*=}" ;;
        --include=*) EXTRA_FIND_ARGS+=" -path ${1#*=}"; MUST_HAVE="${1#*=}" ;;
        -h|--help) DEBUG "Usage: $0 [options] [directory] [extra args]"; show_help=1; dry_run=1 ;;
	-*) add_to_list "$1" passed_along_to_mettalog ;;
         *)
	    if [ -d "$1" ]; then
		add_test_units_dir "$1"
	    elif [ -f "$1" ]; then
		files_to_test+=("$1")
	    else
		find_test_masks "$1"
	    fi
            ;;
    esac
    shift
done

source ./scripts/ensure_venv

extract_all_parent_directories

sort_directories_by_depth

# Report excluded files and directories
DEBUG "Excluded files:"
printf '%s\n' "${excluded_files[@]}"

DEBUG "Excluded directories:"
printf '%s\n' "${excluded_dirs[@]}"

# DEBUG "All unique parent directories:"
for dir in "${unique_directories[@]}"; do
   IF_REALLY_DO mkdir -p "${METTALOG_OUTPUT}/$dir"
done

if [ $show_help -eq 1 ]; then
  show_help
fi

if [ -z "$SHARED_UNITS" ]; then
    if [ -d "$METTALOG_OUTPUT" ]; then
	export SHARED_UNITS=$(realpath $METTALOG_OUTPUT)/SHARED.UNITS
    fi
fi
touch $SHARED_UNITS

# Delete HTML files if the clean flag is set
if [ $clean -eq 1 ]; then
  delete_html_files
  cat /dev/null > "${SHARED_UNITS}"
fi

# Prompt user to rerun all tests if run_tests_auto_reply is not set
if [ -z "$run_tests_auto_reply" ]; then
    read -p "Rerun all tests? (y/N): " -n 1 -r
    DEBUG ""
else
    REPLY=$run_tests_auto_reply
fi

# Directory containing the .pl files
if [ -f "$PYSWIP_VERSION/metta_interp.pl" ]; then
  INTERP_SRC_DIR="$PYSWIP_VERSION"
else 
    if [ -f "$PYSWIP_VERSION/src/canary/metta_interp.pl" ]; then
      INTERP_SRC_DIR="$PYSWIP_VERSION/src/canary"
    else 
	if [ -f "$METTALOG_DIR/src/$PYSWIP_VERSION/metta_interp.pl" ]; then
	  INTERP_SRC_DIR="$METTALOG_DIR/src/$PYSWIP_VERSION"
	else 
	  INTERP_SRC_DIR="$METTALOG_DIR/src/canary"
	fi
    fi
fi

DEBUG "INTERP_SRC_DIR=$INTERP_SRC_DIR"
DEBUG "METTALOG_OUTPUT=$METTALOG_OUTPUT"
DEBUG "SHARED_UNITS=$SHARED_UNITS"

if [[ ! -f "${METTALOG_OUTPUT}/src/" ]]; then
  :
  #cat /dev/null > "${SHARED_UNITS}"
fi

mkdir -p "${METTALOG_OUTPUT}/src/"
cp -af "${INTERP_SRC_DIR}/"* "${METTALOG_OUTPUT}/src/"

#{ return 0 2>/dev/null || exit 0; }

# Run tests and generate MeTTaLog report
if [[ $REPLY =~ ^[Yy]$ ]]; then
   # run our selected tests
    run_tests
else
    DEBUG "Skipping test run."
fi

find $METTALOG_OUTPUT -type f  -name "*.metta.html" -exec sed -i 's/">/"\n>/g' {} \;

IF_REALLY_DO generate_final_MeTTaLog
cat $METTALOG_OUTPUT/TEST_LINKS.md

# Prompt for code commit and unit report generation
if [ -z "$generate_report_auto_reply" ]; then
    read -p "Commit code and generate unit reports? (y/N): " -n 1 -r
    DEBUG ""
else
    REPLY=$generate_report_auto_reply
fi

if [[ $REPLY =~ ^[Yy]$ ]]; then
    IF_REALLY_DO PreCommitReports
else
    DEBUG "Skipping report generation."
fi

