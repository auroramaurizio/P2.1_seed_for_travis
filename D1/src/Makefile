SHELL=/bin/sh

# The compiler
FC = gfortran

FCFLAGS = -O3 -Wall -Wextra -std=f2003 -p #-pg




# List of executables 

default: ./simplesort_09.x ./bubblesort_10.x ./insertionsort_11.x ./quicksort_12.x ./mergesort_13.x  ./next_prime_16.x ./readint_01.x ./readreal_02.x ./sorted_03.x ./sortedreal_04.x ./sortedreal_05.x ./funct_overl_06.x ./deriv_types_07.x ./multiplemodules_08.x


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

./quicksort_12.x: list_tools.o sorting.o quicksort_12.o
	$(FC) -o $@ $^ $(FCFLAGS) 

./mergesort_13.x: list_tools.o sorting.o mergesort_13.o
	$(FC) -o $@ $^ $(FCFLAGS) 


./next_prime_16.x: next_prime_16.o
	$(FC) -o $@ $^ $(FCFLAGS) 


./readint_01.x: readint_01.o
	$(FC) -o $@ $^ $(FCFLAGS) 

./readreal_02.x: readreal_02.o
	$(FC) -o $@ $^ $(FCFLAGS) $(LIBS)

./sorted_03.x: sorted_03.o
	$(FC) -o $@ $^ $(FCFLAGS) $(LIBS)


./sortedreal_04.x: list_types.o list_tools.o sortedreal_04.o
	$(FC) -o $@ $^ $(FCFLAGS) $(LIBS)


./sortedreal_05.x: list_types.o list_tools.o sortedreal_05.o
	$(FC) -o $@ $^ $(FCFLAGS) $(LIBS)


./funct_overl_06.x: list_types.o list_tools.o funct_overl_06.o
	$(FC) -o $@ $^ $(FCFLAGS) $(LIBS)

        
./deriv_types_07.x: deriv_types_07.o
	$(FC) -o $@ $^ $(FCFLAGS) $(LIBS)

./multiplemodules_08.x:  list_types.o list_tools.o multiplemodules_08.o
	$(FC) -o $@ $^ $(FCFLAGS) $(LIBS)


%.o: %.f03
	$(FC) -c $(FCFLAGS) $< 
        


