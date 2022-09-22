#!/bin/bash
#SBATCH -J UKBTables
#SBATCH -t 12:00:00 ## WALL CLOCK TIME
#SBATCH -N 1 -c 32 ## REQUEST NODES AND CORES (ONLY STAGING NODES CAN REQUEST SINGLE CORES)
#SBATCH --output=logs/UKBTables.log
#SBATCH -p thin ## NODE TYPE: normal, fat, gpu, short, staging

module load 2021
module load R/4.1.0-foss-2021a

Rscript UKBWeightedSumStat.R
Rscript UKBCensusSumStat.R
