CC=gfortran
FGDB=-c -g -Og -Wall

ones: ones_helper.o ones.o
	$(CC) -o ones ones.o ones_helper.o
	make clean

ones_helper.o:
	$(CC) -c ones_helper.f90

ones.o:
	$(CC) -c ones.f90

ones_debug: ones_helper_debug.o ones_debug.o
	$(CC) -o ones_debug ones.o ones_helper.o
	make clean

ones_debug.o:
	$(CC) $(FGDB) ones.f90

ones_helper_debug.o:
	$(CC) $(FGDB) ones_helper.f90

.PHONY: clean

clean:
	rm -f *.o
