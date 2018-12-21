#!/bin/bash
#SBATCH --job-name=generate_population
#SBATCH --array=1-200
#SBATCH --time=01:00:00
#SBATCH --mem-per-cpu=1024
#SBATCH --partition=Student
echo "Task ID: $SLURM_ARRAY_TASK_ID"
cp -r tpp/ $LOCALSCRATCH/.
cd $LOCALSCRATCH/tpp/
./main "2111" $SLURM_ARRAY_TASK_ID
cp  -r $SLURM_ARRAY_TASK_ID/ ~/tpp/result/
