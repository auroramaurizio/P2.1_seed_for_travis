SHELL=/bin/sh

# The compiler
FC = gfortran

FCFLAGS = -O3 -Wall -Wextra -std=f2003 




# List of executables 

default: ./readint_01.x ./readreal_02.x ./sorted_03.x ./sortedreal_04.x ./sortedreal_05.x ./funct_overl_06.x ./deriv_types_07.x ./multiplemodules_08.x ./simplesort_09.x ./bubblesort_10.x ./insertionsort_11.x ./quicksort_12.x ./mergesort_13.x ./next_prime_16.x


clean:	
	rm *.o
	rm *.x
	rm *.mod

./readint_01.x: readint_01.o
	$(FC) -o $@ $^ $(FCFLAGS) 

./readreal_02.x: readreal_02.o
	$(FC) -o $@ $^ $(FCFLAGS) 

./sorted_03.x: sorted_03.o
	$(FC) -o $@ $^ $(FCFLAGS) 

./sortedreal_04.x: list_types.o list_tools.o sortedreal_04.o
	$(FC) -o $@ $^ $(FCFLAGS) 

./sortedreal_05.x: list_types.o list_tools.o sortedreal_05.o
	$(FC) -o $@ $^ $(FCFLAGS) 

./funct_overl_06.x: list_types.o list_tools.o funct_overl_06.o
	$(FC) -o $@ $^ $(FCFLAGS) 
        
./deriv_types_07.x: deriv_types_07.o
	$(FC) -o $@ $^ $(FCFLAGS) 

./multiplemodules_08.x:  list_types.o list_tools.o multiplemodules_08.o
	$(FC) -o $@ $^ $(FCFLAGS) 

./simplesort_09.x: list_types.o list_tools.o sorting.o simplesort_09.o
	$(FC) -o $@ $^ $(FCFLAGS) 

./bubblesort_10.x: list_types.o list_tools.o sorting.o bubblesort_10.o
	$(FC) -o $@ $^ $(FCFLAGS) 

./insertionsort_11.x: list_types.o list_tools.o sorting.o insertionsort_11.o
	$(FC) -o $@ $^ $(FCFLAGS) 

./quicksort_12.x: list_types.o list_tools.o sorting.o quicksort_12.o
	$(FC) -o $@ $^ $(FCFLAGS) 

./mergesort_13.x: list_types.o list_tools.o sorting.o mergesort_13.o
	$(FC) -o $@ $^ $(FCFLAGS) 

./next_prime_16.x: next_prime_16.o
	$(FC) -o $@ $^ $(FCFLAGS) 


%.o: %.f03
	$(FC) -c $(FCFLAGS) $< 
        


