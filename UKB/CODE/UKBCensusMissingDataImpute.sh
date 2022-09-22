#!/bin/bash
#SBATCH -t 5:00:00 ## WALL CLOCK TIME
#SBATCH -n 1 -c 20
#SBATCH -p fat
#SBATCH -output=logs/UKBCensusMissingDataImpute.log


module load 2021
module load R/4.1.0-foss-2021a

Rscript UKBCensusMissingDataImpute.R