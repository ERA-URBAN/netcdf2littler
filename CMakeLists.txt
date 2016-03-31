# CMake project file for NETCDF2LITTLER

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.5)

PROJECT(NETCDF2LITTLER Fortran)

# Set the NETCDF2LITTLER version
SET(VERSION alpha)

# Add local modules to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# check if fortran compiler supports fortran90
IF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    MESSAGE(FATAL_ERROR "Fortran compiler does not support F90")
ENDIF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)

# This INCLUDE statement executes code that sets the compile flags for DEBUG,
# RELEASE, and TESTING
INCLUDE(${CMAKE_MODULE_PATH}/SetCompilerFlags.cmake)

# There is an error in CMAKE with this flag for pgf90.  Unset it
GET_FILENAME_COMPONENT(FCNAME ${CMAKE_Fortran_COMPILER} NAME)
IF(FCNAME STREQUAL "pgf90")
    UNSET(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS)
ENDIF(FCNAME STREQUAL "pgf90")

# Define the executable in terms of the source files
# Check if NetCDF for Fortran90 is installed
SET (NETCDF_F90 "YES")
INCLUDE(${CMAKE_MODULE_PATH}/FindNetCDF.cmake)

# check if udunits2 is installed
INCLUDE(${CMAKE_MODULE_PATH}/FindUdunits2.cmake)

############################################################
# Define the actual files and folders that make up the build
############################################################

# Define the executable name
SET(NETCDF2LITTLEREXE convert-littler)

# Define the library name
SET(FUDUNITS2LIB f_udunits_2)

# Define some directories
SET(SRC ${CMAKE_SOURCE_DIR}/src)
SET(FUDUNITS2_INCLUDES ${CMAKE_CURRENT_BINARY_DIR}/src/f_udunits_2)
SET(SRCNETCDF2LITTLER ${SRC}/netcdf2littler)
SET(SRCFUDUNITS2 ${SRC}/f_udunits_2)

# The source for the f_udunits_2 library
ADD_SUBDIRECTORY(${SRCFUDUNITS2})

# The source for the netcdf2littler binary
ADD_SUBDIRECTORY(${SRCNETCDF2LITTLER})
