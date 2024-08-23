#!/bin/bash

# generate the output directory with timestamp
timestamp=$(date +"%Y-%m-%dT%H:%M:%S")
output=reports/tests_output/baseline-compat-$timestamp/

# run the tests
echo Running baseline_compat tests to $output
cat ./reports/SHARED.UNITS.PREV.md > /tmp/SHARED.UNITS
#mettalog --output=$output tests/baseline_compat/anti-regression/comma_is_not_special.metta #> /dev/null 2>&1
