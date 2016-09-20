# CMake project file for NETCDF2LITTLER

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.5)

# Ensures that we do an out of source build

MACRO(MACRO_ENSURE_OUT_OF_SOURCE_BUILD MSG)
  STRING(COMPARE EQUAL "${CMAKE_SOURCE_DIR}" "${CMAKE_BINARY_DIR}" insource)
  GET_FILENAME_COMPONENT(PARENTDIR ${CMAKE_SOURCE_DIR} PATH)
  STRING(COMPARE EQUAL "${CMAKE_SOURCE_DIR}" "${PARENTDIR}" insourcesubdir)
  IF(insource OR insourcesubdir)
    IF(EXISTS ${CMAKE_SOURCE_DIR}/CMakeCache.txt OR EXISTS ${CMAKE_SOURCE_DIR}/CMakeFiles)
      MESSAGE(FATAL_ERROR "Found results from an in-source build in your source directory.")
    ENDIF()
    MESSAGE(FATAL_ERROR "${MSG}")
  ENDIF(insource OR insourcesubdir)
ENDMACRO(MACRO_ENSURE_OUT_OF_SOURCE_BUILD)

MACRO_ENSURE_OUT_OF_SOURCE_BUILD(
    "${CMAKE_PROJECT_NAME} requires an out of source build."
)

PROJECT(NETCDF2LITTLER Fortran)

# Set the NETCDF2LITTLER version
SET(VERSION alpha)

# Add local modules to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

SET(LIBRARY_OUTPUT_PATH  ${NETCDF2LITTLER_BINARY_DIR}/lib CACHE PATH "Single directory for all libraries." )
SET( EXECUTABLE_OUTPUT_PATH ${NETCDF2LITTLER_BINARY_DIR}/bin CACHE PATH "Single directory for all executables." )
MARK_AS_ADVANCED( EXECUTABLE_OUTPUT_PATH )
MARK_AS_ADVANCED( LIBRARY_OUTPUT_PATH )


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
SET(NETCDF2LITTLERTESTSEXE convert-littler-tests)

# Define the library name
SET(FUDUNITS2LIB f_udunits_2)
SET(netcdf2littlerLIB netcdf2littler)

# Define some directories
SET(SRC ${CMAKE_SOURCE_DIR}/src)
SET(FUDUNITS2_INCLUDES ${CMAKE_CURRENT_BINARY_DIR}/src/f_udunits_2)
SET(SRCNETCDF2LITTLER ${SRC}/netcdf2littler)
SET(SRCFUDUNITS2 ${SRC}/f_udunits_2)
SET(SRCNETCDF2LITTLERTESTS ${CMAKE_SOURCE_DIR}/tests)

# The source for the f_udunits_2 library
ADD_SUBDIRECTORY(${SRCFUDUNITS2})

# The source for the netcdf2littler binary
ADD_SUBDIRECTORY(${SRCNETCDF2LITTLER})

# The source for the netcdf2littler binary
ADD_SUBDIRECTORY(${SRCNETCDF2LITTLERTESTS})
