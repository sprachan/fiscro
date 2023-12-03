#!/bin/bash
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH -c 32

# 05subsample.R takes -n (number of bins), -e (epsilon), -s (sample size), -t (plot tag).


 Rscript 05subsample.R -n 200 -e 1e-4 -s 1e6 -t 200_1M



