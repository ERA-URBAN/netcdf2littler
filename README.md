[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Build Status](https://travis-ci.org/ERA-URBAN/netcdf2littler.svg?branch=master)](https://travis-ci.org/ERA-URBAN/netcdf2littler) [![codecov](https://codecov.io/gh/ERA-URBAN/netcdf2littler/branch/master/graph/badge.svg)](https://codecov.io/gh/ERA-URBAN/netcdf2littler)

# NetCDF2LittleR

### Description
A FORTRAN application to convert NetCDF files to the Little-R format. The Little-R format is the accepted input format for the WRF data assimilation system (WRFDA) preprocessor (obsproc). Currently only the conversion of synoptical weather stations are supported by this application.

### Installation
NetCDF2LittleR uses the cmake build system. NetCDF2LittleR requires building out of source, e.g. by creating a build directory. During build a binary is created with the name convert-littler.

### Usage
The binary convert-littler requires a Fortran namelist as argument, for example:
```
netcdf2littler --namelist netcdf2littler.namelist
```
If no argument is supplied, `netcdf2littler` tries to use the file `netcdf2littler.namelist` from the current working directory.

The input namelist provides information to the application on the variables in the NetCDF file, the timeframe for which observations need to be converted, as well as the variables. Below is an example input.namelist (which has just one variable):
```
&NETCDF2LITTLER
  filename = 'test.nc'
  outfile = 'results.txt'
  variable_name = 'temperature', 'rltvh', 'pressure_station', 'winddir',
  variable_mapping = 'temperature' 'humidity' 'pressure' 'direction'
  devices = 1
  dimensions = 1
  startdate = 20140630
  enddate = 20140802
/
```
here variable_name is the name of the variables in the NetCDF file, variable_mapping is a mapping to the name used in the application. Valid names to be used in the variable_mapping definition (and their meaning) are:
```
temperature: temperature (degC or degK)
dew_point: dew point temperature (degC or degK)
humidity: relative humidity (%)
pressure: pressure at measurement height (Pa)
mslp: sea level pressure (Pa)
windspeed: windspeed (m/s)
direction: wind direction (deg)
uwind: wind speed in U direction (m/s)
vwind: wind speed in V direction (m/s)
```
Devices should be set equal to the number of devices in the netCDF file, i.e. this is equal to the number of observation stations.

### Structure input NetCDF files
NetCDF2LittleR only supports NetCDF files with a structure according to (for a single observation station):
```
netcdf test_1d {
dimensions:
        time = UNLIMITED ; // (10 currently)
        lon = 1 ;
        lat = 1 ;
        elevation = 1 ;
variables:
        int time(time) ;
                time:_FillValue = -999 ;
                time:units = "seconds since 2010-01-01 00:00" ;
                time:calendar = "standard" ;
                time:timezone = "UTC" ;
        float lat(lat) ;
                lat:units = "degrees_north" ;
                lat:cell_methods = "device: mean" ;
        float lon(lon) ;
                lon:units = "degrees_east" ;
                lon:cell_methods = "device: mean" ;
        float elevation(elevation) ;
                elevation:units = "meter" ;
                elevation:standard_name = "elevation" ;
        float temperature(time) ;
                temperature:_FillValue = -999.99f ;
                temperature:units = "degC" ;
                temperature:coordinates = "lat lon" ;
                temperature:cell_methods = "point device: mean" ;
        float humidity(time) ;
                humidity:_FillValue = -999.99f ;
                humidity:units = "1" ;
                humidity:coordinates = "lat lon" ;
                humidity:cell_methods = "point device: mean" ;
```
Alternatively, for multiple observation stations, the following structure is supported as well:
```
netcdf test_2d {
dimensions:
        device = 29 ;
        time = UNLIMITED ; // (10 currently)
        lat = 29 ;
        lon = 29 ;
        elevation = 29 ;
variables:
        int device(device) ;
                device:_FillValue = -999 ;
                device:coordinates = "lat lon" ;
                device:units = "1" ;
        int time(time) ;
                time:_FillValue = -999 ;
                time:units = "seconds since 2010-01-01 00:00" ;
                time:calendar = "standard" ;
                time:timezone = "UTC" ;
        float lat(lat) ;
                lat:units = "degrees_north" ;
        float lon(lon) ;
                lon:units = "degrees_east" ;
        float elevation(elevation) ;
                elevation:units = "meter" ;
                elevation:standard_name = "elevation" ;
        float temperature(time, device) ;
                temperature:_FillValue = -999.99f ;
                temperature:units = "degC" ;
                temperature:coordinates = "lat lon" ;
                temperature:cell_methods = "point" ;
        float humidity(time, device) ;
                humidity:_FillValue = -999.99f ;
                humidity:units = "1" ;
                humidity:coordinates = "lat lon" ;
                humidity:cell_methods = "point" ;
```
Note that LittleR output files can be concatenated as well by the user in a postprocessing step.


