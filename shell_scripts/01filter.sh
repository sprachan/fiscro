#!/bin/bash
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH -c 16

for i in fiscro amecro blujay amerob ribgul tuftit;
do
  for n in us ca;
  do 
    echo 'now starting' $i $n
    Rscript ./data_processing/01filtering.R --ebdinput /${n}_${i}_ebd.txt --sedinput ${n}_${i}_sampling.txt --ebdoutput ${n}_${i}_ebd.txt --sedoutput ${n}_${i}_sampling.txt;
    echo 'finished with' $i $n
  done;
done;

