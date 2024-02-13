#!/bin/bash
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH -c 32



Rscript 06-1uncompared_eda.R -s fiscro -e 1e-3

Rscript 06-2yy_eda.R -s fiscro

Rscript 06-3mm_eda.R -s fiscro

