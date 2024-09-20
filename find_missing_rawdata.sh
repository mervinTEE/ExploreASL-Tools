#!/bin/bash

# Check if the correct number of arguments is provided, insert * for wildcard
# Example usage: ./find_missing_rawdata.sh /path/to/rawdata anat "*filename_pattern*"
# Make directory is without "/" at the end
if [ "$#" -ne 3 ]; then
    echo "ERROR"
    echo "Include the following arguments behind your execution <path_to_rawdata> <anat_or_perf> <"filename_pattern_case_insensitive">"
    echo "Example usage: ./find_missing_rawdata.sh /path/to/rawdata anat "*filename_pattern*""
    echo "Make directory is without "/" at the end"
    exit 1
fi


# Assign arguments to variables
folder_path=$1
subfolder_name=$2
iname_pattern=$3

# Initialize a flag to track if any missing files are found
found_missing=false


# Loop through each specified subfolder in the given path
for dir in "$folder_path"/*/"$subfolder_name"; do
    # Check if the directory contains files matching the pattern and echo if missing
    if [ -z "$(find "$dir" -iname "$iname_pattern")" ]; then
        echo "$(basename "$(dirname "$dir")") has missing file"
    fi
done

# Check the flag and print a message if no missing files were found
if [ "$found_missing" = false ]; then
    echo "All directories contain the specified files."
fi