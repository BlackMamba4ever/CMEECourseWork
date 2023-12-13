#!/usr/bin/env Rscript
# Author: PU ZHAO pu.zhao23@imperial.ac.uk
#Script: get_TreeHeight.R

#############################################################################
# Getting user input from SHELL
shell_input <- commandArgs(trailingOnly = TRUE)

if(length(shell_input) != 1) {
  stop("Incorrect number of input files, please only input name of 1 file.\n", call. = FALSE)
} else {
  file_name <- basename(shell_input)
  file_name <- tools::file_path_sans_ext(file_name)
}


#############################################################################
# Function for Calculating tree heights
TreeHeight <- function(degrees, distance) {
  radians <- degrees * pi / 180
  height <- distance * tan(radians)
  #print(paste("Tree height is:", height))
  
  return (height)
}


#############################################################################
# Saving calculated values to file
all_trees <- read.csv(shell_input)

all_trees$Tree.Height.m <- TreeHeight(all_trees$Angle.degrees, all_trees$Distance.m)

write_path <- paste("../results/", file_name, "_treeheights_R.csv", sep = "")
print(paste("(R) File written to:", write_path))

write.csv(all_trees, write_path, row.names = F)