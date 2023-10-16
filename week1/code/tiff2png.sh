#!/bin/bash
# Author: Pu Zhao pu.zhao23@imperial.ac.uk
# Script: tiff2png.sh
# Description: convert .tif files to .png files
# Date: Oct 2023

if [ "$#" -ne 1 ]; then
  echo "Wrong input! $0 requires one input directory."
  exit 1
fi

if [ -f "$1" ]; then
  echo "need to input directory"
  exit 1
fi

filedir="$1"

for f in $filedir/*.tif;
    do  
        echo "Converting $f"; 
        convert "$f"  ../results/"$(basename "$f" .tif).png";
    done