#!/bin/bash
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH -c 32

# 06advanced_eda.R takes -s = species_code

for n in fiscro amecro amerob ribgul moudov
do Rscript 06advanced_eda.R -s $n
done

#Rscript 06advanced_eda.R -s fiscro

