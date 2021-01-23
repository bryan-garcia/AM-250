#PBS -S /bin/tcsh
#PBS -q newest
#PBS -N ping_mpi
#PBS -o ../output
#PBS -e ../error
#PBS -l nodes=1:ppn=4

cd $PBS_O_WORKDIR
cd ../
mpirun -np 4 ping_mpi
