#!/usr/bin/env bash
#SBATCH --job-name=EGTSimulations
#SBATCH --nodes=10
#SBATCH --tasks-per-node=10
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=8Gb
#SBATCH --partition=general

module load oracle_java/jdk1.8.0_181

srun egt_simulations.sh
