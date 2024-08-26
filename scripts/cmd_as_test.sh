#!/bin/bash

local TEST_NAME="$1"          # HTML output file
local LOGFILE="$3"               # Test file
local EXTRA_INFO="$4"

local TEST_CMD="$@"           # The command to run (passed as a single string)

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

if [ $TEST_EXIT_CODE -eq 0 ]; then
    DEBUG_MESSAGE="${GREEN}Completed successfully (EXITCODE=$TEST_EXIT_CODE) $EXTRA_INFO: $TEST_CMD${NC}"
    PASS_OR_FAIL="PASS"
elif [ $TEST_EXIT_CODE -eq 124 ]; then
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

# Generate the HTML link
local HTML_LINK="file://$file_html#${TEST_NAME}"

# Determine if the HTML file should be used as the logfile or a separate .log file should be created
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
echo "| $TEST_NAME | $PASS_OR_FAIL | [$TEST_NAME]($HTML_LINK) | $TEST_CMD | $TEST_EXIT_CODE | 7 | $ELAPSED_TIME | $LOGFILE |" >> /tmp/SHARED.UNITS

# Delete the HTML file if it was planned for deletion
if [ $SHOULD_DELETE_HTML -eq 1 ]; then
    rm -f "$file_html"
fi

return $TEST_EXIT_CODE


