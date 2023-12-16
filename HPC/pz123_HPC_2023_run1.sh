#!/bin/bash
# Desc: R code running script
#pz123@ic.ac.uk

#PBS -l walltime=12:00:00
#PBS -l select=1:ncpus=1:mem=1gb
module load anaconda3/personal
echo "Beginning run R"

cp $HOME/pz123_HPC_2023_main.R $TMPDIR
R --vanilla < $HOME/pz123_HPC_2023_neutral_cluster.R
mv pz123_1_* $HOME

echo "Finished run R"