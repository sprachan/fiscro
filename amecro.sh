#!/bin/bash
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH -c 32

Rscript 07average_movement-sliding.R -s amecro

