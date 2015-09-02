main=convert_littler
FC = gfortran
FFLAGS = -Wimplicit-interface -ludunits -ffree-form
FCFLAGS = -O0 -Wall
OBJS = ../readncdf.o ../write_littler.o ../logging.o ../f_udunits_2.o ../check_status.o convert_littler_tests.o


all: f_udunits_2.f90 check_status.f90 write_littler.f90 readncdf.f90 logging.f90 convert_littler.f90
	$(FC) $(FFLAGS) -c logging.f90
	$(FC) $(FFLAGS) -c f_udunits_2.f90 
	$(FC) $(FFLAGS) -c check_status.f90 
	$(FC) $(FFLAGS) -c write_littler.f90
	$(FC) $(FFLAGS) -c readncdf.f90 check_status.f90  -L/usr/lib -I/usr/include -lnetcdf -lnetcdff
	$(FC) $(FFLAGS) -o $(main) convert_littler.f90 f_udunits_2.f90 check_status.f90 readncdf.f90 write_littler.f90 logging.f90 -L/usr/lib -I/usr/include -lnetcdf -lnetcdff -Wimplicit-interface -ludunits -ffree-form

tests:	convert_littler_tests.f90
	$(FC) $(FFLAGS) -c convert_littler_tests.f90 -I../
	$(FC) $(FFLAGS) run_tests.f90 $(FCFLAGS) -o $@ $(OBJS) -L/usr/lib -I/usr/include -lnetcdf -lnetcdff

clean :
	rm -f *.mod *.pcl *.pc *.o $(main) *.vo *.d
