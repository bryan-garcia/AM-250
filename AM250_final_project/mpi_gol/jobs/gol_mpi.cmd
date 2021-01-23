#PBS -S /bin/tcsh
#PBS -q newest
#PBS -N gol_mpi
#PBS -o output
#PBS -e error
#PBS -l nodes=1:ppn=4
#PBS -l walltime=00:01:00

cd $PBS_O_WORKDIR
make clean
make gol
mpirun -np 4 gol_mpi
