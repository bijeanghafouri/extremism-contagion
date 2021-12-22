#!/bin/sh
#SBATCH --job-name=get-data
#SBATCH --time=06:55:00
#SBATCH --mem=184GB
#SBATCH --cpus-per-task=24

module load usc r
Rscript --vanilla hpc-code.R

