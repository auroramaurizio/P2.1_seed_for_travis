SHELL=/bin/sh

# The compiler
FC = gfortran

FCFLAGS = -O3 -Wall -Wextra -std=f2003




# List of executables 

default: ./readint_01.x ./readreal_02.x ./sorted_03.x ./sortedreal_04.x ./sortedreal_05.x ./funct_overl_06.x ./deriv_types_07.x ./multiplemodules_08.x


clean:	
	rm *.o
	rm *.x
	rm *.mod



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
        


