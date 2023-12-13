#!/bin/bash
# Author: PU ZHAO pu.zhao23@imperial.ac.uk
# Script: run_get_TreeHeight.sh
# Desc: runs the Rscript file get_TreeHeight.R, with default input file
# Arguments: 1 -> .csv file
# Date: Nov 2023

if [ $# -eq 0 ]; then
    echo $#
    echo -e "No input file, default file \"../data/trees.csv\" used\n"

    Rscript get_TreeHeight.R ../data/trees.csv
    echo -e "R file \"get_TreeHeight.R\" tested\n"

    ipython3 get_TreeHeight.py ../data/trees.csv
    echo -e "Python file \"get_TreeHeight.py\" tested\n"

elif [ $# -eq 1 ]; then
    echo $#
    echo -e "Input file \"$1\" used\n"

    Rscript get_TreeHeight.R $1
    echo -e "R file \"get_TreeHeight.R\" tested\n"

    ipython3 get_TreeHeight.py $1
    echo -e "Python file \"get_TreeHeight.py\" tested\n"

else
    echo $#
    echo "Too many input files, please enter one file name, or leave empty for default file to be read"

fi

