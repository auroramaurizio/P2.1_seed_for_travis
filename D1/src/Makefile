SHELL=/bin/sh

# The compiler
FC = gfortran

FCFLAGS = -O3 -Wall -Wextra -std=f2003 -p #-pg




# List of executables 

default:  ./next_prime_16.x 

clean:	
	rm *.o
	rm *.x
	rm *.mod






./next_prime_16.x: next_prime_16.o
	$(FC) -o $@ $^ $(FCFLAGS) 




%.o: %.f03
	$(FC) -c $(FCFLAGS) $< 
        


