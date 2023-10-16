#!/bin/bash
# Author: Pu Zhao pu.zhao23@imperial.ac.uk
# Script: concatenateTwoFile.sh
# Description: Merge the first two input files to the third input file
# Date: Oct 2023

if [ "$#" -ne 3 ]; then
  echo "Wrong input! $0 requires two input files(the first two file), and a merged file(the third file)."
  exit 1
fi


cat $1 > $3
cat $2 >> $3
echo "Merged File is"
cat $3