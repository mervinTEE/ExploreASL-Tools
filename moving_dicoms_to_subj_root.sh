#!/bin/bash

# Check if at least one argument is provided
if [ "$#" -lt 1 ]; then
    echo "ERROR"
    echo "Usage: ./moving_dicoms_to_subj_root.sh <path_to_sourcedatafolder> [directory_name_pattern]"
    echo "Example: ./moving_dicoms_to_subj_root.sh /mnt/hdd/MT/NEURO_BMC/NEURO-ALL scans"
    exit 1
fi

# Assign the first argument to sourcedatafolder
sourcedatafolder=$1

# Assign the second argument to iname_pattern, default to "scans" if not provided
iname_pattern=${2:-scans}

# Loop through each subject directory
for subject_dir in "$sourcedatafolder"/*; do
    # Find the directory matching the pattern, even if it is nested
    target_dir=$(find "$subject_dir" -type d -iname "$iname_pattern" | head -n 1)
    
    # Check if a directory was found and it's not already in the root
    if [ -n "$target_dir" ] && [ "$target_dir" != "$subject_dir/$iname_pattern" ]; then
        # Move the directory to the subject's root directory
        mv "$target_dir" "$subject_dir"
        
        # Remove any empty directories left behind
        find "$subject_dir" -type d -empty -delete
    fi
done

# Echo completion message
echo "Moving is completed"