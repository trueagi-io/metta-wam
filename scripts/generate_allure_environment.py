import sys
import subprocess
import os

def get_git_info(commit_sha):
    """Retrieve the commit message, author, and timestamp from the given commit SHA."""
    commit_message = subprocess.check_output(
        ["git", "log", "-1", "--pretty=%B", commit_sha]
    ).decode("utf-8").strip()

    commit_author = subprocess.check_output(
        ["git", "log", "-1", "--pretty=%an", commit_sha]
    ).decode("utf-8").strip()

    commit_time = subprocess.check_output(
        ["git", "log", "-1", "--pretty=%ci", commit_sha]
    ).decode("utf-8").strip()

    return {
        'COMMIT_MESSAGE': commit_message,
        'COMMIT_AUTHOR': commit_author,
        'COMMIT_TIME': commit_time,
        'COMMIT_SHA': commit_sha
    }

def list_all_env_vars():
    """List all environment variables."""
    return dict(os.environ)

def sort_and_print_props(final_props, priority_keys):
    """Sort dictionary keys, placing priority keys first."""
    # Ensure priority keys are sorted at the beginning
    sorted_keys = sorted(final_props.keys(), key=lambda k: (k not in priority_keys, k))
    for key in sorted_keys:
        print(f"{key} = {final_props[key]}")

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python scripts/generate_allure_environment.py <commit SHA> <branch>")
        sys.exit(1)

    commit_SHA = sys.argv[1]
    branch = sys.argv[2]

    # Retrieve git information and store in dictionary
    git_info = get_git_info(commit_SHA)
    git_info.update({
        'BRANCH': branch
    })

    # Get all environment variables
    env_vars = list_all_env_vars()

    # Combine git_info with environment variables, preferring git_info's values
    final_props = {**env_vars, **git_info}  # git_info overwrites env_vars where keys overlap

    # Write the content to environment.properties file
    with open("environment.properties", "w") as f:
        sort_and_print_props(final_props, list(git_info.keys()))
        for key in sorted(final_props.keys(), key=lambda k: (k not in git_info.keys(), k)):
            f.write(f"{key} = {final_props[key]}\n")

    # Print out the generated properties
    sort_and_print_props(final_props, list(git_info.keys()))


