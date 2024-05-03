#!/bin/bash

# Check if a command is passed
if [ $# -eq 0 ]; then
    echo "Usage: $0 <command>"
    exit 1
fi

# Collect all arguments as a single command
cmd="$@"

# Run the command, handle stdout and stderr separately
{
    $cmd 2> >(while IFS= read -r line; do echo -e "\e[31m$line\e[0m"; done) \
         1> >(while IFS= read -r line; do echo -e "\e[32m$line\e[0m"; done)
} || {
    # Optional: Handle command failure case if necessary
    echo "Command failed to execute properly."
}

