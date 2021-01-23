CC=mpif90

all:

batch:
	qsub jobs/gol_mpi.cmd

gol: gol_helper.o gol_mpi.o
	${CC} -o gol_mpi gol_helper.o gol_mpi.o

gol_helper.o:
	${CC} -c gol_helper.f90

gol_mpi.o:
	${CC} -c gol_mpi.f90

clean:
	rm -f *.o *.mod gol_mpi
	${MAKE} -C output gol_clean
	${MAKE} -C error gol_clean