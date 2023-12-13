#!/usr/bin/env python3
import sys
import os
import numpy
import pandas as pd

#############################################################################
# Getting user input from SHELL
shell_input = sys.argv[1]

file_name = os.path.basename(shell_input)
file_name = os.path.splitext(file_name)[0]


#############################################################################
# Function for Calculating tree heights
def TreeHeight(degrees, distance):
    radians = degrees * numpy.pi/180
    height = distance * numpy.tan(radians)

    return height


#############################################################################
# Saving calculated values to file
all_trees = pd.read_csv(shell_input)

all_trees["Tree.Height.m"] = TreeHeight(all_trees["Angle.degrees"],all_trees["Distance.m"])

write_path = ("../results/" + file_name + "_treeheights_py.csv")
print("(Python) File written to: " + write_path)

all_trees.to_csv(write_path)

