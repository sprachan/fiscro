#!/bin/bash
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH -c 16

# 03zerofill.R takes arguments -e for ebd input, -s for sed input, and -z for zerofill output

for n in ct de ma md me nh nj ny pa ri va vt
do Rscript 03zerofill.R -e ./filtered_data/${n}_ebd_*.txt -s ./filtered_data/${n}_sed_*.txt -z ./processed_data/${n}_zf.csv
   echo $n 'done'
done

# the above code produces one zerofilled data set for each state; below, we combine them

Rscript combine_zerofill.R








