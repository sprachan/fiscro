#!/bin/bash
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH -c 16

for n in us ca;
  do Rscript 02filtering.R --ebdinput ./data/${n}_amecro_ebd.txt --sedinput ./data/${n}_amecro_sampling.txt --ebdoutput ./filtered_data/${n}_amecro_ebd.txt --sedoutput ./filtered_data/${n}_amecro_sampling.txt;
done