#!/usr/bin/env bash
#SBATCH --nodes=1
#SBATCH --tasks-per-node=16
#SBATCH --cpus-per-task=1
#SBATCH --job-name=EGTSimulations
#SBATCH --mem=8Gb
#SBATCH --partition=general

java -jar ~/egt.jar 1000 .9 .9 0 1 5 9
