FC = gfortran
FFLAGS = -Wimplicit-interface -ludunits -ffree-form
FCFLAGS = -O0 -Wall
OBJS = ../readncdf.o ../write_littler.o ../logging.o ../f_udunits_2.o ../check_status.o convert_littler_tests.o

.PHONY: test
test:  convert_littler_tests.f90
	$(FC) $(FFLAGS) -c convert_littler_tests.f90 -I../
	$(FC) $(FFLAGS) run_tests.f90 $(FCFLAGS) -o $@ $(OBJS) -L/usr/lib -I/usr/include -lnetcdf -lnetcdff
	
	
