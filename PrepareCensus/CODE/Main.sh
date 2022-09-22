#!/bin/bash
#SBATCH -t 12:00:00 ## WALL CLOCK TIME
#SBATCH -N 1 -c 32
#SBATCH -p thin
#SBATCH --cpus-per-task=32
#SBATCH --output=Main.log 

module load 2021
module load R/4.1.0-foss-2021a

Rscript Main.R

mv -v ../DATA/CLEAN/* ../../UKB/INPUT
cp ../DATA/TEMP/UKBCentreBirthYearInfo.csv ../../UKB/INPUT/UKBCentreBirthYearInfo.csv
cp ../DATA/TEMP/UKB_Assessment_geography.geojson  ../../UKB/INPUT/UKB_Assessment_geography.geojson 

