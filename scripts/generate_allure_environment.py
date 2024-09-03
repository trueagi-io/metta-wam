import sys
import subprocess

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

    return commit_message, commit_author, commit_time

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python scripts/generate_allure_environment.py <commit SHA> <branch>")
        sys.exit(1)

    commit_SHA = sys.argv[1]
    branch = sys.argv[2]

    # Retrieve git information
    commit_message, commit_author, commit_time = get_git_info(commit_SHA)

    # Create the environment.properties content
    environment_content = (
        f"COMMIT_SHA = {commit_SHA}\n"
        f"BRANCH = {branch}\n"
        f"COMMIT_MESSAGE = {commit_message}\n"
        f"COMMIT_AUTHOR = {commit_author}\n"
        f"COMMIT_TIME = {commit_time}\n"
    )

    # Write the content to environment.properties file
    with open("environment.properties", "w") as f:
        f.write(environment_content)

    # Print out the generated properties
    print(environment_content)

