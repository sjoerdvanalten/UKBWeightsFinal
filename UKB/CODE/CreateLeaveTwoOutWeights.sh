#!/bin/bash
#SBATCH -t 3-00:00:00 ## WALL CLOCK TIME
#SBATCH -N 1 -c 32## REQUEST NODES AND CORES (ONLY STAGING NODES CAN REQUEST SINGLE CORES)
#SBATCH -p fat ## NODE TYPE: normal, fat, gpu, short, staging

module load 2021
module load R/4.1.0-foss-2021a
Rscript CreateLeaveTwoOutWeights.R

