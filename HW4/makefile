make_options = -s
current_dir = $(notdir $(shell pwd))

all:

hello: hello_mpi
	${MAKE} ${make_options} -C jobs hello

hello_mpi:
	mpif90 -o hello_mpi hello_mpi.f90

hello_clean:
	rm -f hello_mpi
	${MAKE} -C output hello_clean
	${MAKE} -C error hello_clean

ssr: ssr_mpi
	${MAKE} ${make_options} -C jobs ssr

ssr_mpi:
	mpif90 -o ssr_mpi ssr_mpi.f90

ssr_clean:
	rm -f ssr_mpi
	${MAKE} -C output ssr_clean
	${MAKE} -C error ssr_clean

ping: ping_mpi
	${MAKE} ${make_options} -C jobs ping

ping_mpi:
	mpif90 -o ping_mpi ping_mpi.f90

ping_clean: 
	rm -f ping_mpi
	${MAKE} -C output ping_clean
	${MAKE} -C error ping_clean

ring: ring_mpi
	ssh compute-0-19

ring_mpi:
	mpif90 -o ring_mpi ring_mpi.f90

ring_clean:
	rm -f ring_mpi
	${MAKE} -C output ring_clean
	${MAKE} -C error ring_clean

pi: pi_mpi
	${MAKE} ${make_options} -C jobs pi

pi_mpi:
	mpif90 -o pi_mpi pi_mpi.f90

pi_clean:
	rm -f pi_mpi
	${MAKE} -C output pi_clean
	${MAKE} -C error pi_clean

clean:
	${MAKE} ${make_options} hello_clean
	${MAKE} ${make_options} ssr_clean
	${MAKE} ${make_options} ping_clean
	${MAKE} ${make_options} ring_clean
	${MAKE} ${make_options} pi_clean