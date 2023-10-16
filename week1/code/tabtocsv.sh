#!/bin/sh
# Author: Pu Zhao pu.zhao23@imperial.ac.uk
# Script: tabtocsv.sh
# Description: substitute the tabs in the files with commas
#
# Saves the output into a .csv file
# Arguments: 1 -> tab delimited file
# Date: Oct 2023

if [ "$#" -ne 1 ]; then
  echo "Wrong input! $0 requires one input files."
  exit 1
fi

echo "Creating a comma delimited version of $1 ..."
cat $1 | tr -s "\t" "," >> $1.csv
echo "Done!"
exit