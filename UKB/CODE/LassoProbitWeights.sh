#!/bin/bash
#SBATCH -t 1-00:00:00 ## WALL CLOCK TIME
#SBATCH -N 1 -c 40## REQUEST NODES AND CORES (ONLY STAGING NODES CAN REQUEST SINGLE CORES)
#SBATCH -p fat ## NODE TYPE: normal, fat, gpu, short, staging
#SBATCH -output=logs/LassoProbitWeights.log

module load 2021
module load R/4.1.0-foss-2021a
Rscript LassoProbitWeights.R

