#PBS -S /bin/tcsh
#PBS -q newest
#PBS -N hello_mpi_grape_1
#PBS -o ../output
#PBS -e ../error
#PBS -l nodes=1:ppn=4
#PBS -l walltime=00:01:00

cd $PBS_O_WORKDIR
cd ../
mpirun -np 4 hello_mpi_grape
