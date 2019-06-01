#!/usr/bin/env bash
#SBATCH --nodes=1
#SBATCH --tasks-per-node=16
#SBATCH --cpus-per-task=1
#SBATCH --job-name=EGTSimulations
#SBATCH --mem=8Gb
#SBATCH --partition=general

module load oracle_java/jdk1.8.0_181
java -jar ~/egt.jar 10000 .5 .9 1 1 5 9
