#!/bin/bash

#SBATCH -p Instruction
#SBATCH -J hello_mpi_hb_1
#SBATCH -e ../error/hb_mpi_1_%j.err
#SBATCH -o ../output/hb_mpi_1_%j.out
#SBATCH --nodes 1
#SBATCH --ntasks-per-node 4
#SBATCH -t 00:05:00

cd ../
mpirun -np 4 hello_mpi_hb
