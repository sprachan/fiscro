#!/bin/bash
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH -c 32

# the zerofill R script takes arguments -e for ebd input and -s for sed input

Rscript ./data_processing/02zerofill.R -e us_fiscro_ebd.txt -s us_fiscro_sampling.txt;









