#!/bin/bash

#SBATCH -p 128x24
#SBATCH -J gol_slurm
#SBATCH -e error/gol_slurm_%j.err
#SBATCH -o output/gol_slurm_%j.out
#SBATCH --nodes 1
#SBATCH --ntasks-per-node 4
#SBATCH -t 00:01:00

mpirun -np 4 gol_mpi