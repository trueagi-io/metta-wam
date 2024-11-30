#!/bin/bash

# Initialize the array for rest of the arguments
rest_of_args=()

# parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    -t|--timestamp)
      timestamp="$2"
      shift # past argument
      shift # past value
      ;;
    --clean)
      clean=true
      shift # past argument
      ;;
    *)
      rest_of_args+=("$1") # store rest of arguments
      shift
      ;;
  esac
done

# generate the output directory with timestamp
if [ -z $timestamp ]; then
    timestamp=$(date +"%Y-%m-%dT%H:%M:%S")
fi
output=./reports/tests_output/baseline-compat/

# run the tests
mkdir -p $output
export METTALOG_OUTPUT=$(realpath $output)
export SHARED_UNITS=$METTALOG_OUTPUT/SHARED.UNITS
touch $SHARED_UNITS

echo "Will run tests to $output ($METTALOG_OUTPUT) with SHARED_UNITS=$SHARED_UNITS"

source ./scripts/ensure_venv -v

# Check if 'ansi2html' is already installed
if ! python3 -m pip list | grep -q 'ansi2html'; then
    # Install 'ansi2html' if it is not installed
    python3 -m pip install ansi2html
fi

# This function runs MettaLog tests with configurable output suppression
run_mettalog_tests() {
    local max_time_per_test="$1"
    local test_dir="$2"
    shift 2  # Shift the first two arguments so the rest can be captured as additional arguments
    local args=("$@")
    local status=666

    # Construct the command using an array to handle spaces and special characters properly
    local cmd=(mettalog --output="$output" --test --timeout=$max_time_per_test "$test_dir")
    cmd+=("${args[@]}")
    cmd+=("${rest_of_args[@]}")

    # Optionally remove --clean from subsequent runs
    if [ "$clean" == true ]; then
        cmd+=("--clean")
        clean=false  # Reset or remove the clean option after using it
    fi

    if [ "$SHOW_ALL_OUTPUT" = true ]; then
	# Execute the command and capture the status
	"${cmd[@]}"
	local status=$?
    else
	# Execute the command silently and filter output, capturing status
	script -q -c "${cmd[*]}" /dev/null | tee >(grep -Ei --line-buffered '_CMD:|h3 id|loonit_|warning|es[:] ' >&2) > /dev/null
	local status=$?
    fi


    if [ $status -eq 4 ]; then
	echo "Something purposely interupted testing... results will not be written!"
	# exit $status # exit this script
    fi

    return $status
}

echo Running tests METTALOG_OUTPUT=$METTALOG_OUTPUT and SHARED_UNITS=$SHARED_UNITS
SKIP_LONG=0

#blank out the shared units
cat /dev/null > /tmp/SHARED.UNITS

SHOW_ALL_OUTPUT=false # Set to false normally, true for debugging


# 23+ tests (~30 seconds)
run_mettalog_tests 40 tests/baseline_compat/module-system/

# 200+ tests (~4 minutes)
run_mettalog_tests 40 tests/baseline_compat/hyperon-experimental_scripts/
run_mettalog_tests 40 tests/baseline_compat/hyperon-mettalog_sanity/
# 50+ tests (~2 minutes)
run_mettalog_tests 40 tests/baseline_compat/metta-morph_tests/

# Check if SKIP_LONG is not set to 1
if [ "$SKIP_LONG" != "1" ]; then

    :
    # 50+ tests (~2 minutes)
    #run_mettalog_tests 15 tests/baseline_compat/anti-regression/

    # 400+ tests (~7 minutes)
    #SHOW_ALL_OUTPUT=true # Set to false normally, true for debugging
    # Gets the rest
    #run_mettalog_tests 40 tests/baseline_compat/

fi


# Stuff just generated
cat $SHARED_UNITS >> /tmp/SHARED.UNITS

# Tests ran locally by developer together
cat ./reports/SHARED.UNITS.LOCAL.md >> /tmp/SHARED.UNITS

# if ran locally on our system we might want to commit these
cat /tmp/SHARED.UNITS > ./reports/SHARED.UNITS.PREV.md

echo "DID run tests to $output ($METTALOG_OUTPUT) with SHARED_UNITS=$SHARED_UNITS"
