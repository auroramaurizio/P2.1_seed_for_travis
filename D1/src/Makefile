SHELL=/bin/sh

# The compiler
FC = gfortran

FCFLAGS = -O3 -Wall -Wextra -std=f2003




# List of executables 

default: ./list_tools.f03 ./sorting.f03 ./simplesort_09.x ./bubblesort_10.x ./insertionsort_11.x


clean:	
	rm *.o
	rm *.x
	rm *.mod



./simplesort_09.x: list_tools.o sorting.o simplesort_09.o
	$(FC) -o $@ $^ $(FCFLAGS) 


./bubblesort_10.x: list_tools.o sorting.o bubblesort_10.o
	$(FC) -o $@ $^ $(FCFLAGS) 


./insertionsort_11.x: list_tools.o sorting.o insertionsort_11.o
	$(FC) -o $@ $^ $(FCFLAGS) 

%.o: %.f03
	$(FC) -c $(FCFLAGS) $< 
        


