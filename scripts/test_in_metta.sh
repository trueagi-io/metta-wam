#!/usr/bin/env bash
set +e

SHOULD_EXIT=0

# Use extended 256-color code #208 for orange
BRIGHT_ORANGE='\033[38;5;208m'
DEBUG_H_E() {
   DEBUG "${BRIGHT_ORANGE}HYPERON: ${BOLD}${*}${NC}"
}
BRIGHT_GREEN='\033[1;32m'
DEBUG_WHY() {
   DEBUG "${BRIGHT_GREEN}METTALOG: ${BOLD}${*}${NC}"
}

# Function to resolve the full absolute path, handling symlinks & '..'
resolve_full_path() {
    local file="$1"
    local absfile=""

    # Check if file exists
    if [ ! -e "$file" ]; then
        #echo "$file"
        : #return
    fi

    # Try GNU and BSD-compatible commands first
    if command -v readlink >/dev/null 2>&1; then
        absfile=$(readlink -f "$file" 2>/dev/null) || absfile=$(realpath "$file" 2>/dev/null)
    elif command -v greadlink >/dev/null 2>&1; then
        absfile=$(greadlink -f "$file")
    else
        # Fallback if neither 'readlink' nor 'realpath' is available
        if [[ -d "$file" ]]; then
            absfile="$(cd "$file" && pwd -P)"
        else
            local dir base
            dir=$(dirname "$file")
            base=$(basename "$file")
            
            # macOS: Use stat -f "%N" if available, otherwise fallback
            if [[ "$(uname -s)" == "Darwin" ]] && command -v stat >/dev/null 2>&1; then
                absfile=$(stat -f "%N" "$file" 2>/dev/null)
            fi

            # If stat didn't work, use manual path resolution
            if [[ -z "$absfile" ]]; then
                absfile="$(cd "$dir" && pwd -P)/$base"
            fi
        fi
    fi

    echo "$absfile"
}

# Function to compute relative path from base directory to a target file
relative_path_from() {
    local base="$1"
    local target="$2"

    # Ensure paths are absolute before processing
    base=$(resolve_full_path "$base")
    target=$(resolve_full_path "$target")

    # If base and target are the same, return '.'
    if [ "$base" = "$target" ]; then
        echo "."
        return
    fi

    # Try native realpath first, fallback to Python if unavailable
    if command -v realpath >/dev/null 2>&1; then
        realpath --relative-to="$base" "$target" 2>/dev/null && return
    elif command -v grealpath >/dev/null 2>&1; then
        grealpath --relative-to="$base" "$target" 2>/dev/null && return
    fi

    # If realpath is unavailable, use Python for better cross-platform support
    python3 -c "import os.path; print(os.path.relpath('$target', '$base'))" 2>/dev/null && return

    # If everything fails, just return the absolute path (fallback)
    echo "$target"
}

# Function to correctly join two paths, handling '..' & removing redundant slashes
path_join() {
    local base="$1"
    local append="$2"

    # Ensure base does not end with '/', and append does not start with '/'
    base="${base%/}"
    append="${append#/}"

    # If base is empty (meaning root path), don't add extra '/'
    local full_path
    if [ -z "$base" ]; then
        full_path="/$append"
    else
        full_path="$base/$append"
    fi

    # Normalize path by resolving '..' and redundant slashes
    case "$(uname -s)" in
        Darwin)
            realpath "$full_path" 2>/dev/null || python3 -c "import os.path; print(os.path.normpath('$full_path'))"
            ;;
        *)
            realpath "$full_path" 2>/dev/null || python3 -c "import os.path; print(os.path.normpath('$full_path'))"
            ;;
    esac
}



process_file() {

    [[ $SHOULD_EXIT -eq 1 ]] && return

    #local file=$(find_override_file "$1")
    local file="$1"

    # Check if the file path contains a tilde
    if [[ "$file" == *"~"* ]]; then
       return 7
    fi


    local extra_args="${@:2}"
    shift
    
    absfile=$(resolve_full_path "$file")
    #relfile=$(relative_path_from "${METTALOG_DIR}" "$absfile")
    #file="$relfile"
    file="${file#./}"
    #path_after_output=$(path_join "${METTALOG_OUTPUT}" "$relfile")
    path_after_output=$(path_join "${METTALOG_OUTPUT}" "$file")    
    export file_html="${path_after_output}.html"
    

    # Extract the directory path from file_html
    html_dir=$(dirname "$file_html")

    # Create the directory if it doesn't exist
    ( mkdir -p "$html_dir" )

    export METTALOG_OUTPUT="${METTALOG_OUTPUT}"

    export HTML_OUT="$file_html"

    echo ""
    echo ""
    echo ""
    DEBUG "==========================================================================="
    DEBUG "${BLUE}${BOLD}===========================================================================${NC}"
    DEBUG "${BOLD}"
    DEBUG ""
    DEBUG "METTALOG_DIR: $METTALOG_DIR"
    DEBUG "     Testing: $file"
    cd "$METTALOG_DIR"
    DEBUG "      Output: $METTALOG_OUTPUT"
    rel_html=$(relative_path_from "${METTALOG_OUTPUT}" "$file_html")
    DEBUG "        Html: $rel_html"
    # Check if the file path contains a tilde
    if [[ "$absfile" == *"~"* ]]; then
       DEBUG "${RED}Warn on tilda'd path?${NC}"
    fi
    DEBUG ""
    DEBUG ""
    DEBUG "${BLUE}${BOLD}===========================================================================${NC}"
    DEBUG "==========================================================================="

    ##########################################################
    # Decide which is the hyperon_results (answers file)
    ##########################################################
    export hyperon_results=(${file}\..*)     # gets any other names as well 
    # List of excluded extensions 
    # (because now that any file can be an answer file we need to still be able to use test directories for test that 
    #   use things besides metta files    such as    
    #    buffer_test.metta.md   uses_config_test.metta.mettalogrc  )
    #  We really need a naming convertion for those generate file names that doenst already conflict with the test framework names  
    #      current conflicts are
    # 
    #     transpile_test.metta.pl (the prolog version of the translation)
    #     test.metta.bak (files i keep of backups)
    #     test.metta.html (created on test output for web results)
    #     large_file.metta.datalog  (currently created for large file that load slowly)
    #     uses_config_test.metta.mettalogrc (currently used for configs)
    #     a_mork_test.metta.xml (XML translated metta files)
    #     a_jetta_test.metta.js (...etc...)
    #
    #     Perhaps a predicatable nmewing convention that can be filtered out
    #         test_error  -> result_test_error
    #         unknown_error -> result_unknown_error
    excluded_extensions=( "tmp" "bak" "html" "~" "sav" "ansi" "pl" "metta" 
             "py" "txt" "md" "tee" "o" "dll" "so" "exe" "sh" "text" "rc" 
            "mettalogrc" "bat" "c" "java" "datalog" "in" "out" "xml" "obo" )
    
    local base_name="$absfile"
    test_local_dir=$(dirname "$base_name")
    rcfile="$test_local_dir/.mettalogrc"

    THIS_METTALOG_TIMEOUT=$METTALOG_TIMEOUT
    THIS_RUST_TIMEOUT=$METTALOG_TIMEOUT

    if [[ -f "$rcfile" ]]; then
	# Read each line in the config file
	while IFS= read -r line; do
	    case "$line" in
		--timeout=*)
		    THIS_METTALOG_TIMEOUT="${line#*=}"
		    ;;
		--rust-timeout=*)
		    THIS_RUST_TIMEOUT="${line#*=}"
		    ;;
		*) ;; # Unknown or unhandled directive
		    
	    esac
	done < "$rcfile"
    fi

    ##########################################################
    # Decide whether or not to run hyperon test
    ##########################################################
    local take_hyperon_test=false
    if [[ "$no_regen" -eq 1 ]]; then
        take_hyperon_test=false
        DEBUG_H_E "--no-regen flag is set. Disabling generation of $file.answers"
    else
        local file_found=false
        # Loop over all potential files matching the base pattern
        for potential_file in "${base_name}\.".*; do
            # Extract the extension of the file
            extension="${potential_file##*.}"
            exclude_item=false
    
            # Check if the extracted extension is in the excluded list
            for excluded in "${excluded_extensions[@]}"; do
                if [[ "$extension" == "$excluded" ]]; then
                    exclude_item=true
                    break
                fi
            done
    
            # If the extension is not in the excluded list, use the file
            if [ -f "$potential_file" ]; then
                if [[ "$exclude_item" == true ]]; then
                    DEBUG_H_E "Skipped $potential_file"
                else
                    DEBUG_H_E "Found $potential_file"
                    export hyperon_results=$potential_file
                    file_found=true
                    break  # break if you only need the first match
                fi
            fi
            if [[ "$file_found" == true ]]; then
                break
            fi
        done
    
        # Check if no file was found
        if [[ "$file_found" == false ]]; then
            export hyperon_results="${base_name}.answers"
            DEBUG_H_E "No alternate answer file extension: defaulting to ${hyperon_results} file."
            take_hyperon_test=true
        else
            DEBUG_H_E "Answer file extension: ${hyperon_results}"
            take_hyperon_test=false
        fi
    
        # Add unique absolute paths to PYTHONPATH
        pp1=$(readlink -f "$(dirname "${file}")")
        pp2=$(readlink -f "$(dirname "${pp1}")")
        for pp in $pp1 $pp2; do
            if [[ ":$PYTHONPATH:" != *":$pp:"* ]]; then
                export PYTHONPATH="${PYTHONPATH:+"$PYTHONPATH:"}$pp"
            fi
        done
    
        # Set OPENAI_API_KEY only if it's not already set
        if [ -z "$OPENAI_API_KEY" ]; then
            export OPENAI_API_KEY=freeve
        fi
    
        local take_mettalog_test=0
        local TEST_EXIT_CODE=0
    	    # set -e
    
        # Perform the checks and set the boolean
        if [ -f "${hyperon_results}" ]; then
            if grep -q "Got" "${hyperon_results}"; then
                # take_hyperon_test=true
                DEBUG_H_E "Failures found in ${hyperon_results}"
            else
                DEBUG_H_E "No failures are found in ${hyperon_results}"
                take_hyperon_test=false
            fi
        else
            DEBUG_H_E "Missing ${hyperon_results}"
            take_hyperon_test=true
        fi
    
        if [ ! -f "${hyperon_results}" ]; then
            take_hyperon_test=true
            DEBUG_H_E "Should regenerate: Answers file does not exist."
        elif [ "${file}" -nt "${hyperon_results}" ] && [ -s "${hyperon_results}" ]; then
            take_hyperon_test=true
            DEBUG_H_E "Should regenerate: Original file is newer than results file and results file is not empty."
            take_mettalog_test=1
        fi
    fi
    
    if [[ "$fresh" -eq 1 ]]; then
        take_hyperon_test=true
        DEBUG_H_E "Fresh flag is set. Forcing regeneration."
    fi
    
    ##########################################################
    # now maybe take hyperon test
    ##########################################################
    if $take_hyperon_test; then
        DEBUG_H_E "${YELLOW}Regenerating answers: $file.answers${NC}"
    
        # Function to handle SIGTERM
        handle_sigterm() {
            DEBUG_H_E "${RED}SIGTERM received, stopping metta but continuing script...${NC}"
            stty sane
            trap - SIGTERM
            trap - SIGINT
        }
        # Function to handle SIGINT
        handle_sigint() {
            DEBUG_H_E "${RED}SIGINT received, stopping metta but continuing script...${NC}"
            stty sane
            trap - SIGTERM
            trap - SIGINT
        }
        # Trap SIGTERM and SIGINT (cannot trap SIGKILL)
        trap 'handle_sigterm' SIGTERM
        trap 'handle_sigint' SIGINT
    
        set +x
        (
            cd "$(dirname "${file}")" || true
            # Record the start time
            start_time=$(date +%s)
            set +x
            (
                set +x
                IF_REALLY_DO "timeout --foreground --kill-after=5 --signal=SIGKILL $(($THIS_RUST_TIMEOUT + 1)) time metta '$absfile' 2>&1 | tee '${absfile}.answers'"
            )
            TEST_EXIT_CODE=$?
    
            # Record the current time
            end_time=$(date +%s)
            # Calculate elapsed time
            elapsed_time=$((end_time - start_time))
    
            if [ $TEST_EXIT_CODE -eq 124 ]; then
                INFO="INFO: ${elapsed_time} seconds (EXITCODE=$TEST_EXIT_CODE) Rust MeTTa Got Killed (definitely due to timeout) after $RUST_TIMEOUT seconds: ${TEST_CMD}"
                DEBUG_H_E "${RED}${INFO}${NC}"
                [ "$if_failures" -eq 1 ] && rm -f "$file_html"
            elif [ $TEST_EXIT_CODE -ne 0 ]; then
                INFO="INFO: ${elapsed_time} seconds (EXITCODE=$TEST_EXIT_CODE) Rust MeTTa Got Completed with error under $RUST_TIMEOUT seconds"
                DEBUG_H_E "${RED}${INFO}${NC}"
            else
                INFO="INFO: ${elapsed_time} seconds (EXITCODE=$TEST_EXIT_CODE) Rust MeTTa Completed successfully under $RUST_TIMEOUT seconds"
                DEBUG_H_E "${GREEN}$INFO${NC}"
            fi
    
            if [ -f "${hyperon_results}" ]; then
                if grep -q "Got" "${hyperon_results}"; then
                    DEBUG_H_E "${RED}Failures in Rust Answers  ${hyperon_results} ${NC}"
                    mv "${hyperon_results}" "${hyperon_results}.test_error"
                    export hyperon_results="${hyperon_results}.test_error"
                fi
                echo INFO >> "${hyperon_results}"
            fi
    
        ) || true
        stty sane
    
        # Remove traps if you only need them once
        trap - SIGTERM
        trap - SIGINT
    
        DEBUG_H_E ""
    else
        DEBUG_H_E "Kept: ${hyperon_results}"
    fi

    ##########################################################
    # Decide to run mettalog test
    #   (Based on the existence or contents of the HTML file)
    ##########################################################
    if [ "$if_regressions" -eq 1 ]; then
        if [[ ! -f "$file_html" ]]; then
            DEBUG_WHY "(--regressions) Not retaking MeTTaLog test since HTML file does not exist."
	    return 7
        fi

        failures_zero=$(grep -h -c "Failures: 0" "$file_html")
        if [ "$failures_zero" -eq 0 ]; then
            DEBUG_WHY "(--regressions) Not taking MeTTaLog test since Failures not 0. (only testing regressions from 100%)"
            return 7
        fi
        take_mettalog_test=1
        DEBUG_WHY "(--regressions) Taking MeTTaLog test since Failures: 0 and looking for regressions."

    elif [[ ! -f "$file_html" ]]; then
        take_mettalog_test=1
        DEBUG_WHY "Taking MeTTaLog test since HTML file does not exist."
    fi
    if [ "$clean" -eq 1 ]; then
	take_mettalog_test=1
	DEBUG_WHY "Taking test since --clean."
    fi
    if [ "$if_failures" -eq 1 ]; then
	if [[ -f "$file_html" ]]; then
	    failures_not_zero=$(grep -h -c "Failures: [^0]" "$file_html")
	    if [ "$failures_not_zero" -eq 0 ]; then
		success_missing=0

		if ! grep -q "Successes: " "$file_html"; then
		    success_missing=1
		fi

		if [ "$success_missing" -eq 1 ]; then
		    DEBUG_WHY "(--failure) Retaking Test since the word 'Success' is missing from $file_html."
		    take_mettalog_test=1
		else
		    DEBUG_WHY "(--failure) The word 'Success' is present in $file_html."
		fi
	    fi
	    if [ "$failures_not_zero" -gt 0 ]; then
		take_mettalog_test=1
		DEBUG_WHY "(--failure) Retaking test since failures are present."
		IF_REALLY_DO rm -f "$file_html"
	    else
		if [ "$take_mettalog_test" -eq 0 ]; then
		    DEBUG_WHY "(--failure) Not retaking since Failures: 0."
		fi
	    fi
	else
	    DEBUG_WHY "(--failure) Results present, not taking test."
	fi
    fi
    set +e
    if [ "$take_mettalog_test" -eq 1 ]; then
        if [[ "$skip_tests" -eq 1 ]]; then
            take_mettalog_test=0
            DEBUG_WHY "--skip-tests flag is set. Disabled taking test"
        fi
    fi

    if [ "$take_mettalog_test" -eq 1 ]; then
        sleep 0.1
        IF_REALLY_DO touch "$file_html"

        TEST_CMD="$METTALOG_DIR/mettalog '--output=$METTALOG_OUTPUT' --timeout=$THIS_METTALOG_TIMEOUT --html --repl=false ${extra_args[@]} ${passed_along_to_mettalog[@]} \"$file\" --halt=true"
        # DEBUG "${BOLD}$TEST_CMD${NC}"
	    
            EXTRA_INFO="Under $THIS_METTALOG_TIMEOUT seconds"

	    DEBUG "HTML_OUT=$HTML_OUT"

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

	    DEBUG "file_html=$file_html"

	    # Check if the file exists
	    if [ -f "$file_html" ]; then
		# Get the file size in bytes
		file_size=$(stat --format="%s" "$file_html")
		    if [ "$file_size" -lt 20 ]; then
			DEBUG "${RED}(warning) The file '$file_html' is less than 20 bytes!${NC}"
		    else
			DEBUG "The file '$file_html' exists and is $file_size bytes."
		    fi
	    else
		DEBUG "${RED}The file '$file_html' does not exist.${NC}"
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
SCRIPTS="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SCRIPTS="$(readlink -m "$SCRIPTS")"

METTALOG_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )"
METTALOG_DIR="$(readlink -m "$METTALOG_DIR")"

SCRIPT_NAME=$(basename "$0")

passed_along_to_mettalog=()
METTALOG_TIMEOUT=45
RUST_TIMEOUT=60

run_tests_auto_reply=""
generate_report_auto_reply=""
METTALOG_OUTPUT="$METTALOG_DIR/reports/tests_output/testrun_$(date +%Y%m%d_%H%M%S)"
fresh=0
no_regen=0
clean=0  # 0 means don't clean, 1 means do clean
if_failures=0
if_regressions=0
skip_tests=0
show_help=0
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

      #set +v
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
          local exclude_item

          for item1 in "${array1[@]}"; do
              exclude_item=0
              for item2 in "${array2[@]}"; do
                  if [[ "$item1" == "$item2" ]]; then
                      exclude_item=1
                      break
                  fi
              done
              if [[ exclude_item -eq 0 ]]; then
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
	python3 $SCRIPTS/into_junit.py "${SHARED_UNITS}" > "$METTALOG_OUTPUT/junit.xml"

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
    {
        echo "| TEST NAME | STATUS | URL LOCATION | TEST CONDITION | ACTUAL RESULT | EXPECTED RESULT |"
        echo "|-----------|--------|--------------|----------------|---------------|-----------------|"
        cat "${SHARED_UNITS}" \
          | awk -F'\\(|\\) \\| \\(' '{ print $1 " " $0 }' \
          | sort \
          | cut -d' ' -f2- \
          | tac \
          | awk '!seen[$0]++' \
          | tac
    } > $METTALOG_OUTPUT/PASS_FAIL.md


   echo "$SCRIPTS/pass_fail_totals.sh $METTALOG_OUTPUT/ > $METTALOG_OUTPUT/TEST_LINKS.md"

   METTALOG_OUTPUT_RELATIVE=$(realpath --no-symlinks --relative-to="$PWD" "$METTALOG_OUTPUT")
   $SCRIPTS/pass_fail_totals.sh $METTALOG_OUTPUT_RELATIVE/ > $METTALOG_OUTPUT/TEST_LINKS.md

   printf '%s\n' "${@}" > "$METTALOG_OUTPUT/_REPORT_.md"

   cat "$METTALOG_OUTPUT/_REPORT_.md"

    cat $METTALOG_OUTPUT/TEST_LINKS.md \
      | sed -e "s|$METTALOG_OUTPUT|reports|g" \
      | sed -e "s|Directory:     ./reports/tests/|D: |g" \
      >> "$METTALOG_OUTPUT/_REPORT_.md"

   $SCRIPTS/html_pass_fail.sh $METTALOG_OUTPUT/ > $METTALOG_OUTPUT/REPORT.html

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
    echo "  --no-regen                 Do not create/recreate .answers files"
    echo "  --clean                    Clean up by deleting all .html files under directory"
    echo "  --continue                 The default. Continue running tests (Generating any missing html files)"
    echo "  --failures                 Rerun Unsuccessful tests"
    echo "  --regress                  Rerun Successful tests (files where score was 100%)"
    echo "  --timeout=SECONDS          Specify a timeout value in seconds (current: $METTALOG_TIMEOUT)"
    echo "  --rust-timeout=SECONDS     Specify a timeout value in seconds (current: $RUST_TIMEOUT)"
    echo "  --report=(Y/N)             Generate a report (if not supplied, will be asked at the end)"
    echo "  --output=DIR               Specify the output directory (current: $METTALOG_OUTPUT)"
    echo "  --[in|ex]clude=PATTERN     Include or exclude tests based on pattern"
    echo "  --dry-run                  Simulate the execution without actually running the tests"
    echo "  --skip-tests               Dont actualy run mettalog tests"
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
VERSION_FILE="$METTALOG_DIR/version-config"
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
        --timeout=*) METTALOG_TIMEOUT="${1#*=}" ;;
	--rust-timeout=*) RUST_TIMEOUT="${1#*=}" ;;
	--output=*) METTALOG_OUTPUT="${1#*=}"  ;;
        --report=*) generate_report_auto_reply="${1#*=}" ;;
        --clean) clean=1; if_failures=0 ;;
        --regression*) clean=0; if_failures=0; if_regressions=1 ;;
        --continu*) clean=0; if_failures=0 ;;
        --fail*) clean=0; if_failures=1 ;;
	--skip-tests) skip_tests=1 ;;
        --dry-run) dry_run=1 ;;
        --test) dry_run=0 ; add_to_list "$1" passed_along_to_mettalog ;;	    
        --fresh) fresh=1 ;;
	--no-regen*) no_regen=1 ;;
        --v=*) PYSWIP_VERSION="${1#*=}"; add_to_list "$1" passed_along_to_mettalog ;;
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

source $SCRIPTS/ensure_venv

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
	export SHARED_UNITS=$(readlink -m $METTALOG_OUTPUT)/SHARED.UNITS
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
if [ -z "$INTERP_SRC_DIR" ]; then

    if [ -z "$PYSWIP_VERSION" ]; then
	   PYSWIP_VERSION="$METTALOG_DIR/prolog/metta_lang"
    fi 

    PYSWIP_VERSION="${PYSWIP_VERSION%/}"
    if [ -f "$PYSWIP_VERSION/metta_interp.pl" ]; then
  INTERP_SRC_DIR="$PYSWIP_VERSION"
    else 
    if [ -f "$PYSWIP_VERSION/prolog/metta_interp.pl" ]; then
      INTERP_SRC_DIR="$PYSWIP_VERSION/prolog"
    else 
      INTERP_SRC_DIR="$METTALOG_DIR/.Attic/$PYSWIP_VERSION"
    fi
    fi
fi



METTALOG_OUTPUT_RELATIVE="$METTALOG_OUTPUT"
METTALOG_OUTPUT="$(readlink -m "$METTALOG_OUTPUT")"
DEBUG "INTERP_SRC_DIR=$INTERP_SRC_DIR"
DEBUG "METTALOG_OUTPUT=$METTALOG_OUTPUT"
DEBUG "METTALOG_OUTPUT_RELATIVE=$METTALOG_OUTPUT_RELATIVE"
SHARED_UNITS="$(readlink -m "$SHARED_UNITS")"
DEBUG "SHARED_UNITS=$SHARED_UNITS"
export SHARED_UNITS

if [[ ! -f "${METTALOG_OUTPUT}/src/" ]]; then
  :
  #cat /dev/null > "${SHARED_UNITS}"
fi

mkdir -p "${METTALOG_OUTPUT}/src/"
cp -af "${INTERP_SRC_DIR}/"* "${METTALOG_OUTPUT}/src/"

# Run tests and generate MeTTaLog report
if [[ $REPLY =~ ^[Yy]$ ]]; then
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

