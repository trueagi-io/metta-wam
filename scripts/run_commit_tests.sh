#!/bin/bash
set -e # stop if any step fails

# install mettalog
chmod +x INSTALL.sh  # Make sure the script is executable
. ./INSTALL.sh --easy

# generate the output directory with timestamp
timestamp=$(date +"%Y-%m-%dT%H:%M:%S")
output=reports/tests_output/baseline-compat-$timestamp/

# run the tests
echo Running baseline_compat tests to $output
mettalog --output=$output tests/baseline_compat > /dev/null 2>&1
