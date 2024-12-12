#!/bin/bash

# Set default timestamp format and paths
TIMESTAMP=$(date +"%Y-%m-%dT%H:%M:%S")
JOB_TYPE="ci"  # Default to CI, can be set to "nightly"
BASELINE_COMPAT_PATH="reports/tests_output/now"
ALLURE_RESULTS_DIR="build/allure-results"
ALLURE_REPORT_DIR="allure-report"
REPO_URL=""
GIT_USERNAME=""
GIT_EMAIL=""

detect_git_config() {
    # Try to get the remote URL from the current repository
    REPO_URL=$(git config --get remote.origin.url)
    if [ -z "$REPO_URL" ]; then
        REPO_URL=$(git remote get-url origin 2>/dev/null)
    fi
    
    # Get git user name and email from local config
    GIT_USERNAME=$(git config user.name)
    GIT_EMAIL=$(git config user.email)
    
    # Validate we have all required git info
    if [ -z "$REPO_URL" ] || [ -z "$GIT_USERNAME" ] || [ -z "$GIT_EMAIL" ]; then
        echo "Error: Unable to detect complete git configuration."
        echo "Missing values:"
        [ -z "$REPO_URL" ] && echo "- Repository URL"
        [ -z "$GIT_USERNAME" ] && echo "- Git username"
        [ -z "$GIT_EMAIL" ] && echo "- Git email"
        echo "Please ensure you're in a git repository or provide values via command line."
        exit 1
    fi
    
    # If URL doesn't include authentication and we have GH token in env, add it
    if [[ "$REPO_URL" == https://github.com/* ]] && [ -n "$GITHUB_TOKEN" ]; then
        REPO_URL="https://x-access-token:${GITHUB_TOKEN}@github.com/${REPO_URL#https://github.com/}"
    fi
    
    export REPO_URL
    export GIT_USERNAME
    export GIT_EMAIL
    
    echo "Detected git configuration:"
    echo "Repository URL: $REPO_URL"
    echo "Git Username: $GIT_USERNAME"
    echo "Git Email: $GIT_EMAIL"
}

detect_git_config


# Parse command line arguments
while getopts "t:j:r:u:e:" opt; do
    case $opt in
        t) TIMESTAMP="$OPTARG";;
        j) JOB_TYPE="$OPTARG";;
        r) REPO_URL="$OPTARG";;
        u) GIT_USERNAME="$OPTARG";;
        e) GIT_EMAIL="$OPTARG";;
        \?) echo "Invalid option -$OPTARG" >&2; exit 1;;
    esac
done

# Create necessary directories
mkdir -p "$BASELINE_COMPAT_PATH"
mkdir -p reports/
mkdir -p test-scripts/
mkdir -p previous-results/
mkdir -p "$ALLURE_RESULTS_DIR"
mkdir -p "$ALLURE_REPORT_DIR"

# Function to setup git configuration
setup_git() {

    return 0

    if [ -z "$REPO_URL" ]; then
        echo "No repository URL provided. Skipping git operations."
        return 1
    fi

    if [ -z "$GIT_USERNAME" ] || [ -z "$GIT_EMAIL" ]; then
        echo "Git username or email not provided. Using git config defaults."
        GIT_USERNAME=$(git config user.name)
        GIT_EMAIL=$(git config user.email)
        if [ -z "$GIT_USERNAME" ] || [ -z "$GIT_EMAIL" ]; then
            echo "No git config defaults found. Please provide git username and email."
            return 1
        fi
    fi

    git config user.name "$GIT_USERNAME"
    git config user.email "$GIT_EMAIL"
    return 0
}

# Function to ensure just-results branch exists and get previous results
setup_just_results_branch() {
    echo "Setting up just-results branch..."
    
    # Check if just-results branch exists remotely
    if git ls-remote --heads "$REPO_URL" just-results; then
        echo "just-results branch exists. Cloning it."
        git clone --single-branch --branch just-results --depth 1 "$REPO_URL" previous-results
    else
        echo "just-results branch does not exist. Creating it."
        git init previous-results
        cd previous-results
        git checkout -b just-results
        touch current_test_results.txt
        git add current_test_results.txt
        git commit -m "Initialize just-results branch"
        git remote add origin "$REPO_URL"
        git push origin just-results
        cd ..
    fi

    # Copy previous test results if they exist
    if [ -f "previous-results/current_test_results.txt" ]; then
        cp previous-results/current_test_results.txt previous_test_results.txt
    else
        echo "No previous test results found."
    fi
}

# Function to update just-results branch
update_just_results() {
    if [ "$TEST_CHANGED" != "true" ]; then
        echo "No test changes detected. Skipping just-results update."
        return
    fi

    echo "Updating just-results branch..."
    cp current_test_results.txt previous-results/
    cd previous-results

    # Configure git and commit changes
    git config user.name "$GIT_USERNAME"
    git config user.email "$GIT_EMAIL"
    git pull origin just-results
    git add current_test_results.txt
    git commit -m "Update test results - $TIMESTAMP"
    git push origin just-results

    cd ..
}

# Function to check dependencies
check_dependencies() {
    local missing_deps=()
    
    if ! command -v python3 &> /dev/null; then
        missing_deps+=("python3")
    else
        for package in "junit2html" "ansi2html" "hyperon"; do
            if ! python3 -c "import $package" 2>/dev/null; then
                missing_deps+=("$package")
            fi
        done
    fi
    
    if ! command -v allure &> /dev/null; then
        echo "Allure is not installed. Installing Allure..."
        install_allure
    fi
    
    if [ ${#missing_deps[@]} -ne 0 ]; then
        echo "Missing dependencies: ${missing_deps[*]}"
        echo "Please install missing dependencies:"
        echo "pip install ${missing_deps[*]}"
       # exit 1
    fi
}

# Function to install Allure
install_allure() {
    if command -v brew &> /dev/null; then
        brew install allure
    elif command -v apt-get &> /dev/null; then
        sudo apt-add-repository ppa:qameta/allure
        sudo apt-get update
        sudo apt-get install allure
    else
        echo "Could not install Allure automatically. Please install it manually:"
        echo "See: https://docs.qameta.io/allure/#_installing_a_commandline"
        exit 1
    fi
}

# Function to clone and setup test suite
setup_test_suite() {
    if [ ! -d "metta-testsuite" ]; then
        git clone --branch development --depth 1 https://github.com/logicmoo/metta-testsuite.git metta-testsuite
    fi
    
    cp -r metta-testsuite/tests/* tests/ 2>/dev/null || true
    cp -r metta-testsuite/reports/* reports/ 2>/dev/null || true
    cp -r metta-testsuite/test-scripts/* test-scripts/ 2>/dev/null || true
}

# Function to run tests
run_tests() {
    if [ "$JOB_TYPE" == "nightly" ]; then
        ./scripts/run_nightly_tests.sh -t "$TIMESTAMP"
    else
        ./scripts/run_commit_tests.sh -t "$TIMESTAMP"
    fi
}

# Function to parse and compare test results
parse_test_results() {
    # Extract test IDs and their statuses
    awk -F '|' '{print $2 "|" $3}' /tmp/SHARED.UNITS | grep -E 'PASS|FAIL' | sort > current_test_results.txt
    
    # Compare with previous results if they exist
    TEST_CHANGED="false"
    if [ -f "previous_test_results.txt" ]; then
        if diff previous_test_results.txt current_test_results.txt > /dev/null; then
            echo "No changes in test results."
            TEST_CHANGED="false"
        else
            echo "Changes detected in test results."
            TEST_CHANGED="true"
        fi
    else
        echo "No previous test results to compare."
        TEST_CHANGED="true"
    fi
    export TEST_CHANGED
}

# Function to generate environment properties
generate_environment_properties() {
    cat > "$ALLURE_RESULTS_DIR/environment.properties" << EOF
Timestamp: $TIMESTAMP
Job Type: $JOB_TYPE
Runner: Local
Branch: local
Commit: local-run
EOF
}

# Function to generate reports
generate_reports() {
    if [ "$TEST_CHANGED" != "true" ]; then
        echo "No test changes detected. Skipping report generation."
        return
    fi

    # Generate JUnit XML
    python3 scripts/into_junit.py /tmp/SHARED.UNITS "$TIMESTAMP" 1 > junit.xml
    cp junit.xml "$ALLURE_RESULTS_DIR/"
    
    # Generate HTML reports
    junit2html junit.xml "$BASELINE_COMPAT_PATH/junit-standard-report.html"
    junit2html --report-matrix "$BASELINE_COMPAT_PATH/junit-matrix-report.html" junit.xml
}

# Function to generate and deploy Allure report
handle_allure_report() {
    if [ "$TEST_CHANGED" != "true" ]; then
        return
    fi

    # Generate environment properties
    generate_environment_properties
    
    # Clean and generate Allure report
    rm -rf "$ALLURE_REPORT_DIR"
    allure generate "$ALLURE_RESULTS_DIR" -o "$ALLURE_REPORT_DIR" --clean
    
    # Setup for GitHub Pages
    mkdir -p "allure-history/$BASELINE_COMPAT_PATH"
    cp -r "$ALLURE_REPORT_DIR"/* allure-history/
    cp -r "$BASELINE_COMPAT_PATH"/* "allure-history/$BASELINE_COMPAT_PATH/"
    cp -r reports/* allure-history/reports/
    
    # Create index.html
    cat > allure-history/index.html << EOF
<html>
<head><title>Project Reports and Documentation</title></head>
<body>
<h1>Project Reports and Documentation</h1>
<ul>
<li><a href='./ci/'>Allure CI Reports</a></li>
<li><a href='./nightly/'>Allure Nightly Reports</a></li>
<li><a href='./$BASELINE_COMPAT_PATH/junit-standard-report.html'>JUnit Standard Report</a></li>
<li><a href='./$BASELINE_COMPAT_PATH/junit-matrix-report.html'>JUnit Matrix Report</a></li>
</ul>
</body>
</html>
EOF

    # Deploy to test-results branch
    if [ -n "$REPO_URL" ]; then
        cd allure-history
        git init
        git checkout -b test-results
        git add .
        git commit -m "Update test reports - $TIMESTAMP"
        git remote add origin "$REPO_URL"
        git push -f origin test-results
        cd ..
    fi
}

# Main execution
echo "Starting local CI workflow..."

if [ -z "$REPO_URL" ]; then
    echo "Usage: $0 [-t timestamp] [-j jobtype] -r repo_url [-u git_username] [-e git_email]"
    echo "Example: $0 -r https://github.com/username/repo.git -u \"John Doe\" -e \"john@example.com\""
    exit 1
fi

# Setup git configuration
setup_git

# Setup just-results branch and get previous results
setup_just_results_branch

# Check dependencies
check_dependencies

# Setup test suite
echo "Setting up test suite..."
setup_test_suite


# Prepare and run tests
chmod +x INSTALL.sh
. ./INSTALL.sh --easy
chmod +x scripts/*.sh

# Run tests
echo "Running tests..."
run_tests

# Parse and compare results
echo "Parsing test results..."
parse_test_results

# Update just-results branch
update_just_results

# Generate reports if tests changed
generate_reports

# Handle Allure report generation and deployment
handle_allure_report

# Serve Allure report locally
if [ "$TEST_CHANGED" == "true" ]; then
    echo "Starting Allure report server..."
    allure open "$ALLURE_REPORT_DIR"
fi

echo "Local CI workflow completed."
