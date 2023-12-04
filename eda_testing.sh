#!/bin/bash
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH -c 32

# 06advanced_eda.R takes -s = species_code


Rscript 06advanced_eda_testing.R

