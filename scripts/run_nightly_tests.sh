#!/bin/bash

# parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    -t|--timestamp)
      timestamp="$2"
      shift # past argument
      shift # past value
      ;;
    *)
      # Ignore unknown options
      ;;
  esac
done

# generate the output directory with timestamp
if [ -z $timestamp ]; then
    timestamp=$(date +"%Y-%m-%d")
fi
output=./reports/BY_DATE/$timestamp
export METTALOG_OUTPUT=$(realpath $output)
export SHARED_UNITS=$METTALOG_OUTPUT/SHARED.UNITS

if [ ! -d $output ]; then
    mkdir -p $output
fi

touch $SHARED_UNITS

# run the tests

echo "Running nightly tests to $output with SHARED_UNITS=$SHARED_UNITS"

#cat ./reports/SHARED.UNITS.PREV.md > /tmp/SHARED.UNITS


# this function hides output without the command being aware of the redirection
run_mettalog_tests() {

    local max_time_per_test=$1
    local test_dir=$2
    shift 2  # Shift the first two arguments so the rest can be captured as additional arguments

    # Capture the rest of the arguments
    local rest_args="$@"

    # Run the command
    eval "mettalog --continue --output=$output --test --timeout=$max_time_per_test $test_dir $rest_args"

    if [ $? -eq 4 ]; then
	exit 4
    fi

}


run_single_timed_unit() {
    local TEST_CMD="$1"           # The command to run (passed as a single string)
    local file_html="$2"          # HTML output file
    local file="$3"               # Test file
    local EXTRA_INFO="$4"         # Renamed inside the function from METTALOG_MAX_TIME

    # Start the timer
    local START_TIME=$(date +%s)

    # Run the test command using eval to handle the single string properly
    eval "$TEST_CMD"
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
    elif [[ $TEST_EXIT_CODE -eq 4 ]] || [[ $TEST_EXIT_CODE -eq 134 ]]; then
	DEBUG_MESSAGE="${RED}Stopping tests (EXITCODE=$TEST_EXIT_CODE) $EXTRA_INFO: $TEST_CMD${NC}"
	SHOULD_DELETE_HTML=1
	PASS_OR_FAIL="FAIL"
    elif [ $TEST_EXIT_CODE -ne 7 ]; then
	DEBUG_MESSAGE="${YELLOW}Completed (EXITCODE=$TEST_EXIT_CODE) $EXTRA_INFO: $TEST_CMD${NC}"
	PASS_OR_FAIL="FAIL"
    else
	DEBUG_MESSAGE="${GREEN}Completed successfully (EXITCODE=$TEST_EXIT_CODE) $EXTRA_INFO: $TEST_CMD${NC}"
	PASS_OR_FAIL="PASS"
    fi

    # Generate the test name in the format WHOLE_TESTS.ParentDirectory.File
    local PARENT_DIR=$(basename "$(dirname "$file")")
    local BASE_FILE=$(basename "$file" .metta)  # Replace .metta with the correct file extension
    local TEST_NAME="WHOLE_TESTS.$PARENT_DIR.$BASE_FILE"

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

    # Write the line to /tmp/SHARED.UNITS
    echo "| $TEST_NAME | $PASS_OR_FAIL | [$TEST_NAME]($HTML_LINK) | $TEST_CMD | $TEST_EXIT_CODE | 7 | $ELAPSED_TIME seconds | $LOGFILE |" >> /tmp/SHARED.UNITS

    # Delete the HTML file if it was planned for deletion
    if [ $SHOULD_DELETE_HTML -eq 1 ]; then
	rm -f "$file_html"
    fi

    return $TEST_EXIT_CODE
}


# Construct the TEST_CMD string
#TEST_CMD="mettalog --output=$METTALOG_OUTPUT --timeout=$METTALOG_MAX_TIME --html --repl=false ${extra_args[*]} ${passed_along_to_mettalog[*]} \"$file\" --halt=true"

# Call the function with the constructed command and other variables
#IF_REALLY_DO return run_single_timed_unit "$TEST_CMD" "$file_html" "$file" "Under $METTALOG_MAX_TIME seconds"


SKIP_LONG=0

# 23+ tests (~30 seconds)
run_mettalog_tests 40 tests/baseline_compat/module-system/

# 200+ tests (~4 minutes)
run_mettalog_tests 40 tests/baseline_compat/hyperon-experimental_scripts/
run_mettalog_tests 40 tests/baseline_compat/hyperon-mettalog_sanity/

# 50+ tests (~2 minutes)
run_mettalog_tests 40 tests/baseline_compat/metta-morph_tests/

# Check if SKIP_LONG is not set to 1
if [ "$SKIP_LONG" != "1" ]; then


    # 50+ tests (~2 minutes)
    run_mettalog_tests 40 tests/baseline_compat/anti-regression/

    # 400+ tests (~7 minutes)
    run_mettalog_tests 40 tests/baseline_compat/


    run_mettalog_tests 40 tests/nars_interp/

    run_mettalog_tests 40 tests/more-anti-regression/
    run_mettalog_tests 40 tests/extended_compat/
    run_mettalog_tests 40 tests/douglas_pro_team_august_2024/
    run_mettalog_tests 40 tests/direct_comp/
    run_mettalog_tests 40 tests/features/
    run_mettalog_tests 40 tests/performance/
    #run_mettalog_tests 40 tests/compiler_baseline/
    #run_mettalog_tests 40 tests/nars_w_comp/
    # run_mettalog_tests 40 tests/python_compat/
fi

cat $SHARED_UNITS > /tmp/SHARED.UNITS

# if ran locally on our systme we might want to commit these
cat /tmp/SHARED.UNITS > ./reports/SHARED.UNITS.PREV.md



