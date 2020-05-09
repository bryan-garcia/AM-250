#!/bin/bash

for NUM in 1 2
do
	qsub hello_mpi_grape_${NUM}.cmd
	qsub hello_omp_grape_${NUM}.cmd
done
