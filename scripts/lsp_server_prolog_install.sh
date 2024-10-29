#!/bin/bash

# Define the target directory and symlink source and destination
TARGET_DIR="$HOME/.local/share/swi-prolog/pack"
LINK_NAME="$TARGET_DIR/lsp_server_metta"
LINK_TARGET="$HOME/metta-wam/packs/lsp_server_metta"

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
        exit 0
    else
        echo "Failed to create symlink."
        exit 1
    fi
fi

