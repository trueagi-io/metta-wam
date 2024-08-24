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
    timestamp=$(date +"%Y-%m-%dT%H:%M:%S")
fi
output=reports/tests_output/baseline-compat-$timestamp/

# run the tests
mkdir -p $output
echo Running baseline_compat tests to $output
#cat ./reports/SHARED.UNITS.PREV.md > /tmp/SHARED.UNITS
cat /dev/null > /tmp/SHARED.UNITS
#mettalog --output=$output --test --clean tests/baseline_compat/anti-regression/comma_is_not_special.metta
mettalog --output=$output --test --clean tests/baseline_compat/module-system/
