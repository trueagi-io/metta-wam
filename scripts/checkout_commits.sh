#!/bin/bash

# Make sure you're in the Git repository
if [ ! -d ".git" ]; then
    echo "This is not a Git repository."
    exit 1
fi

# Ask the user for the number of days to go back
read -p "Enter the number of days to go back in the Git log: " days_to_go_back

# Ask the user for the maximum number of checkouts per day
read -p "Enter the maximum number of commits to check out per day: " max_checkouts_per_day

# Create a directory for the commits if it doesn't exist
mkdir -p commits

# Declare an associative array to track how many commits we've checked out per day
declare -A commit_count_per_day

# Initialize a counter for the total number of commits checked out
total_commits_checked_out=0

# Initialize a variable to handle pagination of git log
skip=0
limit=20

# Get the current date in YYYY-MM-DD format
current_date=$(date +%Y-%m-%d)

# Loop through the commits, respecting the days and max checkouts per day
while true; do
    # Get the next set of commits (20 at a time, skip already processed ones)
    commits=$(git log --format="%H %ad" --date=iso --since="$days_to_go_back days ago" --skip=$skip -n $limit)

    # If no more commits are found, break the loop
    if [ -z "$commits" ]; then
        echo "No more commits available."
        break
    fi

    # Loop through each commit
    while IFS= read -r line; do
        # Extract the commit hash, date, and time
        commit=$(echo "$line" | awk '{print $1}')
        date=$(echo "$line" | awk '{print $2}')
        time=$(echo "$line" | awk '{print $3}' | sed 's/:/-/g')  # Replace colons with hyphens

        # Check if we've already checked out the maximum commits for this day
        if [[ ${commit_count_per_day[$date]} -ge $max_checkouts_per_day ]]; then
            echo "Already checked out $max_checkouts_per_day commits for $date, skipping commit $commit."
            continue
        fi

        # Create a folder named with DATE-TIME-SHA format
        folder_name="commits/${date}-${time}-${commit}"
        mkdir -p "$folder_name"

        # Clone the repository into the folder and checkout the specific commit
        git clone . "$folder_name"
        cd "$folder_name"
        git checkout "$commit"
        cd - > /dev/null

        echo "Checked out commit $commit (date: $date time: $time) into $folder_name"

        # Increment the count for this date
        commit_count_per_day[$date]=$((commit_count_per_day[$date] + 1))

    done <<< "$commits"

    # Increment the skip variable to get the next set of commits
    skip=$((skip + limit))

    # Break if we've reached the requested number of days
    if [ "$date" \< "$(date -d "$days_to_go_back days ago" +%Y-%m-%d)" ]; then
        break
    fi
done

echo "Finished checking out commits within the last $days_to_go_back days with a limit of $max_checkouts_per_day per day."

