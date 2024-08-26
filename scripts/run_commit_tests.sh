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
output=./reports/tests_output/baseline-compat-$timestamp/
export METTALOG_OUTPUT=$(realpath $output)
export SHARED_UNITS=$METTALOG_OUTPUT/SHARED.UNITS

# run the tests
mkdir -p $output
touch $SHARED_UNITS
echo Running baseline_compat tests to $output
#cat ./reports/SHARED.UNITS.PREV.md > /tmp/SHARED.UNITS
cat /dev/null > /tmp/SHARED.UNITS
#mettalog --output=$output --test --clean tests/baseline_compat/anti-regression/comma_is_not_special.metta
( mettalog --test --clean --output=$output  tests/baseline_compat/module-system/ )
# Stuff just generated
cat $SHARED_UNITS >> /tmp/SHARED.UNITS
# Tests ran locally by developer (temporary to see what a nightly with 1000+ tests looks like)
cat ./reports/SHARED.UNITS.PREV.md >> /tmp/SHARED.UNITS
# together
cat /tmp/SHARED.UNITS
