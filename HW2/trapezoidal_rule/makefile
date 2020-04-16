CC=gfortran

trapezoidal_rule: trapezoidal_rule.o function.o
	$(CC) -o trapezoidal_rule trapezoidal_rule.o function.o
	make clean

trapezoidal_rule.o: trapezoidal_rule.f90
	$(CC) -c trapezoidal_rule.f90

function.o: function.f90
	$(CC) -c function.f90

.PHONY: clean
clean:
	rm -f trapezoidal_rule.o function.o
