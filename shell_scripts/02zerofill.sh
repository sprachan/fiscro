#!/bin/bash
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH -c 32

# the zerofill R script takes arguments -e for ebd input, -s for sed input, and -z for zerofill output


for i in fiscro amecro blujay amerob ribgul tuftit;
do
  for n in us ca;
  do 
    echo 'now starting' $i $n
    Rscript ./data_processing/02-1zerofill.R -e ${n}_${i}_ebd.txt -s ${n}_${i}_sampling.txt -z ${n}_${i}_zf.txt;
    echo $i $n 'zero-filled'
  done;
done;

# combine these output zerofilled files into one
Rscript ./data_processing/02-2combine_zf.R








