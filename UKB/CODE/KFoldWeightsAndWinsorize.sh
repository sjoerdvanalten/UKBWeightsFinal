#!/bin/bash
#SBATCH -t 1:00:00 ## WALL CLOCK TIME
#SBATCH -N 1 -c 1## REQUEST NODES AND CORES (ONLY STAGING NODES CAN REQUEST SINGLE CORES)
#SBATCH -p staging ## NODE TYPE: normal, fat, gpu, short, staging
#SBATCH --output=logs/KFoldWeightsAndWinsorize.log

module load 2021
module load R/4.1.0-foss-2021a
Rscript KFoldWeightsAndWinsorize.R

