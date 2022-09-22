#!/bin/bash
#SBATCH -J UKBPrepareCensusStack
#SBATCH -t 12:00:00 ## WALL CLOCK TIME
#SBATCH -N 1 -c 40 ## REQUEST NODES AND CORES (ONLY STAGING NODES CAN REQUEST SINGLE CORES)
#SBATCH --output=logs/UKBPrepareCensusStack.log
#SBATCH -p fat ## NODE TYPE: normal, fat, gpu, short, staging

module load 2021
module load R/4.1.0-foss-2021a

Rscript UKBPrepare.R
Rscript UKBGeographyMatch.R
Rscript UKBStackWithCensus.R