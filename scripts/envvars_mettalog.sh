#!/usr/bin/env bash


# Check if the script was sourced or executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # ANSI escape code for yellow foreground
    yellow='\033[1;33m'
    # Reset color
    NC='\033[0m'

    echo -e "${yellow}This script should be sourced to work correctly."
    echo -e "Please use '. ${BASH_SOURCE[0]}' \n   or 'source ${BASH_SOURCE[0]}' instead.${NC}"
    exit 1
fi


# Resolve the actual directory of the current script, following symlinks
SCRIPT=$(readlink -f "${BASH_SOURCE[0]}")
SCRIPT_DIR=$(dirname "$SCRIPT")
# Set METTALOG_DIR to one level above the script's directory
METTALOG_DIR=$(dirname "$SCRIPT_DIR")

export METTALOG_DIR

# Extend the PATH and PYTHONPATH environment variables

# Check if METTALOG_DIR is already in PATH, if not, add it
if [[ ":$PATH:" != *":$METTALOG_DIR:"* ]]; then
    export PATH=$PATH:${METTALOG_DIR}
fi

add_py_dirs_to_pythonpath() {
    local dir="$1"  # The initial directory to search
    if [[ -z "$dir" ]]; then
        echo "Usage: add_py_dirs_to_pythonpath <directory>"
        return 1
    fi

    # Convert existing PYTHONPATH into an array, removing duplicates
    IFS=':' read -r -a existing_dirs <<< "$PYTHONPATH"
    declare -A unique_dirs
    for d in "${existing_dirs[@]}"; do
        if [[ $d != .* ]] && [[ $d != *~* ]] && [[ -d $d ]]; then  # Ensure directories are not hidden or backup
            unique_dirs["$d"]=1
        fi
    done

    # Find directories containing .py files, excluding directories that are hidden or contain a tilde
    while IFS= read -r -d '' d; do
        # Skip directories that are hidden, contain a tilde, or are subdirectories of already-added directories
        if [[ $d =~ /.* ]] || [[ $d == .* ]] || [[ $d == *~* ]]; then
            continue
        fi
        local skip_dir=false
        for added_dir in "${!unique_dirs[@]}"; do
            if [[ $d == $added_dir* ]]; then
                skip_dir=true
                break
            fi
        done
        if $skip_dir; then
            continue
        fi
        # Check if the directory contains Python files
        local py_file_count=$(find "$d" -maxdepth 1 -type f -name "*.py" | wc -l)
        if [[ $py_file_count -gt 0 ]]; then
            unique_dirs["$d"]=1
        fi
    done < <(find "$dir" -type d -print0 | sort -uz)

    # Rebuild PYTHONPATH from unique directories
    PYTHONPATH=""
    for d in "${!unique_dirs[@]}"; do
        PYTHONPATH="${PYTHONPATH:+$PYTHONPATH:}$d"
    done    
}

# Resolve the absolute path and pass it to the function
#add_py_dirs_to_pythonpath "$(realpath "$METTALOG_DIR/../hyperon-experimental/python/sandbox")"
#add_py_dirs_to_pythonpath "$(realpath "$METTALOG_DIR/tests/")"


# Prepend METTALOG_DIR/python to PYTHONPATH if it's not already included
if [[ ":$PYTHONPATH:" != *":${METTALOG_DIR}/python:"* ]]; then
    PYTHONPATH="${METTALOG_DIR}/python${PYTHONPATH:+:$PYTHONPATH}"
fi

# Prepend METTALOG_DIR/tests/python_compat/metta-motto to PYTHONPATH if it's not already included
if [[ ":$PYTHONPATH:" != *":${METTALOG_DIR}/tests/python_compat/metta-motto:"* ]]; then
    PYTHONPATH="${METTALOG_DIR}/tests/python_compat/metta-motto${PYTHONPATH:+:$PYTHONPATH}"
fi

#echo "Updated PYTHONPATH: $PYTHONPATH"
# Optionally, print the values to verify they are set (you can remove these lines in production)
# echo "METTALOG_DIR=$METTALOG_DIR"
# echo "PATH=$PATH"
#echo "PYTHONPATH=$PYTHONPATH"
if [[ $- == *i* ]]; then
  # only source if interactive shell
  [[ -f /usr/share/bash-completion/bash_completion ]] && . /usr/share/bash-completion/bash_completion
  source $METTALOG_DIR/scripts/mettalog_completion.sh
fi


alias ensure_venv="source $METTALOG_DIR/scripts/ensure_venv"



