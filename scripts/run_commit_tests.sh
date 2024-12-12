#!/bin/bash

# Initialize the array for rest of the arguments
rest_of_args=()
test_filter=""
timestamp=""
clean=false
SHOW_ALL_OUTPUT=false # Set to false normally, true for debugging
SKIP_LONG=0

# parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    -t|--timestamp)
      timestamp="$2"
      shift 2
      ;;
    --clean)
      clean=true
      shift
      ;;
    -f|--filter)
      test_filter="$2"
      shift 2
      ;;
    --show-all)
      SHOW_ALL_OUTPUT=true
      shift
      ;;
    *)
      rest_of_args+=("$1")
      shift
      ;;
  esac
done

# generate the output directory with timestamp
if [ -z "$timestamp" ]; then
    timestamp=$(date +"%Y-%m-%dT%H:%M:%S")
fi
output=./reports/tests_output/baseline-compat/

# Default test directories
test_dirs=(
    "tests/baseline_compat/module-system"
    "tests/baseline_compat/hyperon-experimental_scripts"
    "tests/baseline_compat/hyperon-mettalog_sanity"
    "tests/baseline_compat/metta-morph_tests"
)

# Filter test directories if filter is provided
if [ -n "$test_filter" ]; then
    filtered_dirs=()
    for dir in "${test_dirs[@]}"; do
        if [[ "$dir" == *"$test_filter"* ]]; then
            filtered_dirs+=("$dir")
        fi
    done
    test_dirs=("${filtered_dirs[@]}")
    
    if [ ${#test_dirs[@]} -eq 0 ]; then
        echo "Warning: No test directories match filter '$test_filter'"
        exit 1
    fi
    
    echo "Filtered test directories:"
    printf '%s\n' "${test_dirs[@]}"
fi

# Setup output directory and environment
mkdir -p "$output"
export METTALOG_OUTPUT=$(realpath "$output")
export SHARED_UNITS=$METTALOG_OUTPUT/SHARED.UNITS
touch "$SHARED_UNITS"

echo "Will run tests to $output ($METTALOG_OUTPUT) with SHARED_UNITS=$SHARED_UNITS"

# Source virtual environment
if [ -f "./scripts/ensure_venv" ]; then
    source ./scripts/ensure_venv -v
else
    echo "Warning: ensure_venv script not found, proceeding without it"
fi

# Check if mettalog is in PATH
if ! command -v mettalog &> /dev/null; then
    echo "Error: mettalog command not found"
    echo "Please ensure mettalog is installed and in your PATH"
    exit 1
fi

# Install required Python packages
python3 -m pip install ansi2html 2>/dev/null

# Initialize shared units file
: > /tmp/SHARED.UNITS

# This function runs MettaLog tests with configurable output suppression
run_mettalog_tests() {
    local max_time_per_test="$1"
    local test_dir="$2"
    shift 2  # Shift the first two arguments so the rest can be captured as additional arguments
    local args=("$@")
    local status=666

    # Check if test directory exists
    if [ ! -d "$test_dir" ]; then
        echo "Warning: Test directory $test_dir does not exist, skipping..."
        return 0
    fi    
    
    echo "Running tests in: $test_dir"

    # Construct the command
    local cmd=(mettalog --output="$output" --test --timeout="$max_time_per_test" "$test_dir")
    
    if [ "${#args[@]}" -gt 0 ]; then
        cmd+=("${args[@]}")
    fi
    if [ "${#rest_of_args[@]}" -gt 0 ]; then
        cmd+=("${rest_of_args[@]}")
    fi
    # Optionally remove --clean from subsequent runs
    if [ "$clean" == true ]; then
        cmd+=("--clean")
        clean=false  # Reset or remove the clean option after using it
    fi

    echo "Executing: ${cmd[*]}"
    
    # Execute command based on output mode
    if [ "$SHOW_ALL_OUTPUT" = true ]; then
        # Execute the command and show all output
        "${cmd[@]}"
        status=$?
    else
        # Execute the command silently and filter output
        # The grep pattern matches important test output while filtering noise
        script -q -c "${cmd[*]}" /dev/null | \
            tee >(grep -Ei --line-buffered '_CMD:|h3 id|loonit_|warning|es[:] ' >&2) > /dev/null
        status=$?
    fi

    if [ $status -eq 4 ]; then
        echo "Something purposely interrupted testing... results will not be written!"
        # Uncomment below to exit on interrupt
        # exit $status
    fi

    return $status
}

echo "Running tests METTALOG_OUTPUT=$METTALOG_OUTPUT and SHARED_UNITS=$SHARED_UNITS"

#blank out the shared units
cat /dev/null > /tmp/SHARED.UNITS


# 23+ tests (~30 seconds)
#run_mettalog_tests 40 tests/baseline_compat/module-system/
# 200+ tests (~4 minutes)
#run_mettalog_tests 40 tests/baseline_compat/hyperon-experimental_scripts/
#run_mettalog_tests 40 tests/baseline_compat/hyperon-mettalog_sanity/
# 50+ tests (~2 minutes)
#run_mettalog_tests 40 tests/baseline_compat/metta-morph_tests/

# Run filtered test suites
for test_dir in "${test_dirs[@]}"; do
    echo "Starting tests for: $test_dir"
    run_mettalog_tests 40 "$test_dir"
done

# Check if SKIP_LONG is not set to 1
if [ "$SKIP_LONG" != "1" ]; then

    : # Placeholder for additional tests
    # Uncomment below to run additional test suites
    # 50+ tests (~2 minutes)
    #run_mettalog_tests 15 tests/baseline_compat/anti-regression/

    # 400+ tests (~7 minutes)
    #SHOW_ALL_OUTPUT=true # Set to false normally, true for debugging
    # Gets the rest
    #run_mettalog_tests 40 tests/baseline_compat/

fi

# Combine test results
if [ -f "$SHARED_UNITS" ]; then
    # Stuff just generated
    cat "$SHARED_UNITS" >> /tmp/SHARED.UNITS
fi

if [ -f "./reports/SHARED.UNITS.LOCAL.md" ]; then
    # Tests ran locally by developer together
    cat "./reports/SHARED.UNITS.LOCAL.md" >> /tmp/SHARED.UNITS
fi

# Save previous results
# if ran locally on our system we might want to commit these
cat /tmp/SHARED.UNITS > ./reports/SHARED.UNITS.PREV.md

echo "DID run tests to $output ($METTALOG_OUTPUT) with SHARED_UNITS=$SHARED_UNITS"
