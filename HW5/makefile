make_options = -s
current_dir = $(notdir $(shell pwd))

all:
	${MAKE} ${make_options} ping

ping: ping_mpi
	${MAKE} ${make_options} -C jobs ping

ping_mpi:
	mpif90 -o ping_mpi ping_mpi.f90

ping_clean: 
	rm -f ping_mpi
	${MAKE} -C output ping_clean
	${MAKE} -C error ping_clean

clean:
	${MAKE} ${make_options} ping_clean
