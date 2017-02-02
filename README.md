[![Build Status](https://travis-ci.org/era-urban/netcdf2littler.svg?branch=master)](https://travis-ci.org/era-urban/netcdf2littler) [![codecov](https://codecov.io/gh/era-urban/netcdf2littler/branch/master/graph/badge.svg)](https://codecov.io/gh/era-urban/netcdf2littler)

# NetCDF2LittleR

### Description
A FORTRAN application to convert NetCDF files to the Little-R format. The Little-R format is the accepted input format for the WRF data assimilation system (WRFDA) preprocessor (obsproc).

### Installation
NetCDF2LittleR uses the cmake build system. NetCDF2LittleR requires building out of source, e.g. by creating a build directory. During build a binary is created with the name convert-littler.

### Usage
The binary convert-littler requires a Fortran namelist as argument, for example:
```
convert-littler input.namelist
```
The input namelist provides information to the application on the variables in the NetCDF file, the timeframe for which observations need to be converted, as well as the variables. Below is an example input.namelist (which has just one variable):
```
&GROUP_NAME
  filename = 'WMO_NOBatt.nc'
  outfile = 'wmo.txt'
  variable_name = 'temperature',
  variable_mapping = 'temperature'
  devices = 4
  dimensions = 2
  startdate = 20140701
  enddate = 20140801
/
```
here variable_name is the name of the variables in the NetCDF file, variable_mapping is a mapping to the name used in the application. Valid names to be used in the variable_mapping are:  
```
humidity, height, speed, temperature, dew_point, pressure, direction, thickness, uwind, vwind, psfc, refpres
```
devices and dimensions in the input namelist should either be both 1 (a single observation station), or dimensions should be 2 for multiple observation stations, with devices equal to the number of observation stations.

### Structure input NetCDF files
NetCDF2LittleR only supports NetCDF files with a structure according to (for a single observation station):
```
netcdf test_1d {
dimensions:
        time = UNLIMITED ; // (10 currently)
variables:
        float humidity(time) ;
                humidity:_FillValue = -999.99f ;
                humidity:units = "1" ;
                humidity:coordinates = "lat lon" ;
                humidity:cell_methods = "point device: mean" ;
        float lat ;
                lat:units = "degrees_north" ;
                lat:cell_methods = "device: mean" ;
        float lon ;
                lon:units = "degrees_east" ;
                lon:cell_methods = "device: mean" ;
        float temperature(time) ;
                temperature:_FillValue = -999.99f ;
                temperature:units = "degC" ;
                temperature:coordinates = "lat lon" ;
                temperature:cell_methods = "point device: mean" ;
        int time(time) ;
                time:_FillValue = -999 ;
                time:units = "seconds since 2010-01-01 00:00" ;
                time:calendar = "standard" ;
                time:timezone = "UTC" ;
```
Alternatively, for multiple observation stations, the following structure is supported as well:
```
netcdf test_2d {
dimensions:
        device = 29 ;
        time = UNLIMITED ; // (10 currently)
variables:
        int device(device) ;
                device:_FillValue = -999 ;
                device:coordinates = "lat lon" ;
                device:units = "1" ;
        float humidity(time, device) ;
                humidity:_FillValue = -999.99f ;
                humidity:units = "1" ;
                humidity:coordinates = "lat lon" ;
                humidity:cell_methods = "point" ;
        float lat(device) ;
                lat:units = "degrees_north" ;
        float lon(device) ;
                lon:units = "degrees_east" ;
        float temperature(time, device) ;
                temperature:_FillValue = -999.99f ;
                temperature:units = "degC" ;
                temperature:coordinates = "lat lon" ;
                temperature:cell_methods = "point" ;
        int time(time) ;
                time:_FillValue = -999 ;
                time:units = "seconds since 2010-01-01 00:00" ;
                time:calendar = "standard" ;
                time:timezone = "UTC" ;
```
Note that LittleR output files can be concatenated as well by the user in a postprocessing step.


