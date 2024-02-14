#!/bin/bash

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

# Check if METTALOG_DIR/metta_vspace is already in PYTHONPATH, if not, add it
if [[ ":$PYTHONPATH:" != *":${METTALOG_DIR}/metta_vspace:"* ]]; then
    export PYTHONPATH=${PYTHONPATH:+${PYTHONPATH}:}${METTALOG_DIR}/metta_vspace
fi

# Optionally, print the values to verify they are set (you can remove these lines in production)
# echo "METTALOG_DIR=$METTALOG_DIR"
# echo "PATH=$PATH"
# echo "PYTHONPATH=$PYTHONPATH"
