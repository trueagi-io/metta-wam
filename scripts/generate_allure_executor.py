import sys, json

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: python scripts/generate_allure_executor.py <server-url> <repo> <run-id>")
        sys.exit(1)

    server_url = sys.argv[1]
    repo = sys.argv[2]
    run_id = sys.argv[3]
    data = { 'name':'GitHub Actions', 'type':'github' }
    data['buildUrl'] = '{}/{}/actions/runs/{}'.format(server_url, repo, run_id)
    data['buildName'] = 'GitHub Actions Run #{}'.format(run_id)
    
    print(json.dumps(data))
