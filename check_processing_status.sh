#!/bin/bash

# Check if the correct number of arguments is provided
if [ "$#" -ne 1 ]; then
    echo "ERROR"
    echo "Include the following arguments behind your execution  <path_to_module_locked_folder> after calling for script"
    echo "Example usage: ./check_processing_status.sh /path/to/module_locked_folder"
    echo "Make directory is without "/" at the end"
    exit 1
fi

# Assign the argument to a variable
folder_path=$1

# Loop through each directory in the specified path
for dir in "$folder_path"/*; do
    # Check if the directory contains files matching the pattern
    if [ -z "$(find "$dir" -iname "999_ready*")" ]; then
        echo "$(basename "$dir")"
    fi
done