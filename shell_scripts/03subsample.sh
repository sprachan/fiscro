#!/bin/bash
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH -c 32

# 03subsample.R takes -n (number of bins), -e (epsilon), -s (sample size), -t (plot tag).


Rscript ./data_processing/03subsample.R -n 200 -e 1e-4 -s 3e6 -t 200_3M



