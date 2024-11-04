#!/bin/bash

# Define the target directory and symlink source and destination
TARGET_DIR="$HOME/.local/share/swi-prolog/pack"
LINK_NAME="$TARGET_DIR/lsp_server_metta"

# Determine the directory where the script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Define the relative path to LINK_TARGET
RELATIVE_LINK_TARGET="../src/packs/lsp_server_metta"

# Combine SCRIPT_DIR with the relative path
LINK_TARGET="${SCRIPT_DIR}/${RELATIVE_LINK_TARGET}"

# Resolve LINK_TARGET to an absolute path
if readlink -f / >/dev/null 2>&1; then
    # readlink supports -f
    LINK_TARGET="$(readlink -f "${LINK_TARGET}")"
elif command -v realpath >/dev/null 2>&1; then
    # realpath is available
    LINK_TARGET="$(realpath "${LINK_TARGET}")"
else
    # Fallback method using directory change
    LINK_TARGET="$(cd "${LINK_TARGET}" && pwd)"
fi

# Check if the directory already exists
if [ -e "$TARGET_DIR" ]; then
    echo "The directory $TARGET_DIR already exists."
else
    # Create the directory if it doesn't exist
    echo "Creating directory $TARGET_DIR..."
    mkdir -p "$TARGET_DIR"
    if [ $? -eq 0 ]; then
        echo "Directory created successfully."
    else
        echo "Failed to create directory."
        exit 1
    fi
fi

# Check if the symlink already exists
if [ -L "$LINK_NAME" ]; then
    echo "The symlink $LINK_NAME already exists."
elif [ -e "$LINK_NAME" ]; then
    echo "A file or directory already exists at $LINK_NAME, not creating symlink."
else
    # Check if the symlink target exists
    if [ ! -e "$LINK_TARGET" ]; then
        echo "The symlink target $LINK_TARGET does not exist. Cannot create symlink."
        exit 1
    fi
    # Create the symlink if it does not exist
    echo "Creating symlink from $LINK_NAME to $LINK_TARGET..."
    ln -s "$LINK_TARGET" "$LINK_NAME"
    if [ $? -eq 0 ]; then
        echo "Symlink created successfully."
    else
        echo "Failed to create symlink."
        exit 1
    fi
fi

# Source and target directories
SOURCE_DIR="$HOME/metta-wam/src/packs"
TARGET_DIR="$HOME/.local/share/swi-prolog/pack"


# Debugging output to see which directories are being checked
echo "Checking directories in: $SOURCE_DIR"

# Loop over each directory in the source directory
for dir in "$SOURCE_DIR"/*/; do
    # Ensure that path expansion is happening correctly
    if [ -d "$dir" ]; then
        #echo "Inspecting directory: $dir"

        # Check if the directory contains a file named 'pack.pl'
        if [ -f "${dir}pack.pl" ]; then
            # Get the name of the directory
            dir_name=$(basename "$dir")

            # Define the link name in the target directory
            link_name="$TARGET_DIR/$dir_name"

            # Check if a symlink already exists
            if [ -L "$link_name" ]; then
                echo "Symlink already exists for $link_name, skipping..."
            else
                # Create a symbolic link
                ln -s "$dir" "$link_name"
                echo "Created symlink for $dir at $link_name"
            fi
        else
            : #echo "No 'pack.pl' found in $dir, skipping..."
        fi
    else
        echo "Directory not found or glob did not expand: $dir"
    fi
done
