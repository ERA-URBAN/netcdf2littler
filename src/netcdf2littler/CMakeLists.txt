########################################
# Set up how to compile the source files
########################################

# Add the source files
SET(NETCDF2LITTLER_src ${SRCNETCDF2LITTLER}/check_status.f90
                       ${SRCNETCDF2LITTLER}/convert_littler.f90
                       ${SRCNETCDF2LITTLER}/logging.f90
                       ${SRCNETCDF2LITTLER}/readncdf.f90
                       ${SRCNETCDF2LITTLER}/write_littler.f90
)

ADD_EXECUTABLE(${NETCDF2LITTLEREXE} ${NETCDF2LITTLER_src})

#####################################################
# Add the needed libraries and special compiler flags
#####################################################

# This links foo to the f_udunits_2 library
INCLUDE_DIRECTORIES(${NETCDF_INCLUDES}
                    ${FUDUNITS2_INCLUDES})

TARGET_LINK_LIBRARIES(${NETCDF2LITTLEREXE} ${FUDUNITS2LIB}
                      ${UDUNITS2_LIBRARIES}
                      ${NETCDF_LIBRARIES_F90})


#####################################
# Tell how to install this executable
#####################################

IF(WIN32)
    SET(CMAKE_INSTALL_PREFIX "C:\\Program Files")
ELSE()
    SET(CMAKE_INSTALL_PREFIX /usr/local)
ENDIF(WIN32)

# install binary
INSTALL(TARGETS ${NETCDF2LITTLEREXE} RUNTIME DESTINATION bin)
