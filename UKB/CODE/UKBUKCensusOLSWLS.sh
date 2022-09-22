#!/bin/bash
#SBATCH -t 12:00:00 ## WALL CLOCK TIME
#SBATCH -n 1
#SBATCH -N 1 -c 30
#SBATCH -p thin  
#SBATCH --output=logs/UKBUKCensusOLSWLS.log 

module load 2021
module load R/4.1.0-foss-2021a

Rscript UKBUKCensusOLSWLS.R