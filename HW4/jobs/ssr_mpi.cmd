#!/bin/bash

#SBATCH -J ssr_slurm
#SBATCH -e error/ssr_slurm%j.err
#SBATCH -o output/ssr_slurm_%j.out
#SBATCH --nodes 1
#SBATCH --ntasks-per-node 4
#SBATCH -t 00:01:00

mpirun -np 4 ssr_mpi