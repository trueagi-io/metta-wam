#!/bin/bash

# generate the output directory with timestamp
timestamp=$(date +"%Y-%m-%dT%H:%M:%S")
output=reports/tests_output/baseline-compat-$timestamp/

# run the tests
. ./scripts/ensure_venv
echo Running baseline_compat tests to $output
mettalog --output=$output tests/baseline_compat #> /dev/null 2>&1
