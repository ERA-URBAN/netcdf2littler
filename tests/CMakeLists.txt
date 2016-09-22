########################################
# Set up how to compile the source files
########################################

# Add the source files
enable_testing()
SET(NETCDF2LITTLERTESTS_src
                       ${SRCNETCDF2LITTLER}/check_status.f90
                       ${SRCNETCDF2LITTLER}/logging.f90
                       ${SRCNETCDF2LITTLER}/readncdf.f90
                       ${SRCNETCDF2LITTLER}/write_littler.f90
                       ${SRCNETCDF2LITTLERTESTS}/convert_littler_tests.f90
                       ${SRCNETCDF2LITTLERTESTS}/run_tests.f90
)

ADD_EXECUTABLE(${NETCDF2LITTLERTESTSEXE} ${NETCDF2LITTLERTESTS_src})
find_package(codecov)
ADD_COVERAGE(${NETCDF2LITTLERTESTSEXE})
add_test(test1 ${NETCDF2LITTLER_BINARY_DIR}/bin/${NETCDF2LITTLERTESTSEXE})
#####################################################
# Add the needed libraries and special compiler flags
#####################################################

# This links foo to the f_udunits_2 library
INCLUDE_DIRECTORIES(${NETCDF_INCLUDES}
                    ${FUDUNITS2_INCLUDES})
#                    ${NETCDF2LITTLER_SRC})

#TARGET_LINK_DIRECTORIES=(${NETCDF2LITTLER_SRC})
TARGET_LINK_LIBRARIES(${NETCDF2LITTLERTESTSEXE} ${FUDUNITS2LIB}
                      ${UDUNITS2_LIBRARIES}
                      ${NETCDF_LIBRARIES_F90})
#                      ${netcdf2littlerLIB})
#####################################
# Tell how to install this executable
#####################################

IF(WIN32)
    SET(CMAKE_INSTALL_PREFIX "C:\\Program Files")
ELSE()
    SET(CMAKE_INSTALL_PREFIX /usr/local)
ENDIF(WIN32)

# install binary
#INSTALL(TARGETS ${NETCDF2LITTLEREXE} RUNTIME DESTINATION bin)
