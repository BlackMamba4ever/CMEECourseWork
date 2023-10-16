#!/bin/bash

# Author: Pu Zhao pu.zhao23@imperial.ac.uk
# Script: csvtospace.sh
# Description: Change the comma-separated format in the csv file
# to space-separated format and store it in a new txt file
#
# Date: Oct 2023

#Check the input file for wrong

if [ "$#" -ne 1 ]; then
  echo "$0 requires one input file"
  exit 1
fi

input_file="$1"

# Check if the input file exists
if [ ! -e "$input_file" ]; then
  echo "Input file not found: $input_file"
  exit 1
fi

# Check if the input file is .csv file
if [ "${input_file: -4}" != ".csv" ]; then
  echo "input wrong file type"
  exit 1
fi

# Check if the input file empty
if [ ! -s "$input_file" ]; then
  echo "nothing in the file"
  exit 1
fi

input_file_name=$(basename "$input_file")
output_file="../results/${input_file_name%.*}.txt"

#Convert CSV to space-separated txt file using tr and save as a new file
tr -s ',' ' ' <"$input_file" >>"$output_file"

echo "Successful! Output: $output_file"
