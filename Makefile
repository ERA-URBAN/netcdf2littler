#gfortran -o test convert_littler.f f_udunits_2.f90 -L/usr/lib -I/usr/include -lnetcdf -lnetcdff -Wimplicit-interface -ludunits
gfortran -c f_udunits_2.f90 
gfortran -c check_status.f90
gfortran -c readncdf.f90 check_status.f90  -L/usr/lib -I/usr/include -lnetcdf -lnetcdff
gfortran -o test convert_littler.f90 f_udunits_2.f90 check_status.f90 readncdf.f90 -L/usr/lib -I/usr/include -lnetcdf -lnetcdff -Wimplicit-interface -ludunits -ffree-form

# -ffree-form is standard if extension f90 is used
