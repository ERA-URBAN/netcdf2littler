#gfortran -o test convert_littler.f f_udunits_2.f90 -L/usr/lib -I/usr/include -lnetcdf -lnetcdff -Wimplicit-interface -ludunits
gfortran -o test convert_littler.f90 f_udunits_2.f90 -L/usr/lib -I/usr/include -lnetcdf -lnetcdff -Wimplicit-interface -ludunits -ffree-form

# -ffree-form is standard if extension f90 is used
