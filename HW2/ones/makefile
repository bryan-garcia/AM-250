CC=gfortran

ones: ones_helper.o ones.o
	$(CC) -o ones ones.o ones_helper.o
	make clean

ones_helper.o:
	$(CC) -c ones_helper.f90

ones.o:
	$(CC) -c ones.f90

.PHONY: clean
clean:
	rm -f *.o
