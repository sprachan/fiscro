#!/bin/bash
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH -c 32

# 06advanced_eda.R takes -s = species_code


Rscript 06-1uncompared_eda.R -s amerob -e 1e-3

Rscript 06-2yy_eda.R -s amerob

Rscript 06-3mm_eda.R -s amerob



