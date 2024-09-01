import sys

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python scripts/generate_allure_environment.py <commit SHA> <branch>")
        sys.exit(1)

    commit_SHA = sys.argv[1]
    branch = sys.argv[2]
    print("COMMIT_SHA = {}\nBRANCH = {}".format(commit_SHA, branch))