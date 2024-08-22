#!/bin/bash

# Directory to read .rsl files from
dir="$1"

# Script to execute
script="./myrsl"

# Check if directory exists
if [ ! -d "$dir" ]; then
    echo "Directory $dir does not exist."
    exit 1
fi

# Check if script exists
if [ ! -f "$script" ]; then
    echo "Script $script does not exist."
    exit 1
fi

# Iterate over .rsl files in directory
for file in "$dir"/*.rsl; do
    # Check if file exists
    if [ -f "$file" ]; then
        # Execute script with file as argument
        bash "$script" "typecheck" "$file" "-v" "-l"
    fi
done