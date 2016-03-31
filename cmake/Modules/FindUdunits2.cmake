# Find UDUNITS2
# Find the native UDUNITS2 includes and library
#
#  UDUNITS2_INCLUDES    - where to find udunits2.h
#  UDUNITS2_LIBRARIES   - libraries to link with
#  UDUNITS2_FOUND       - True if UDUNITS2 was found.

if (UDUNITS2_INCLUDES)
  # Already in cache, be silent
  set (UDUNITS2_FIND_QUIETLY TRUE)
endif (UDUNITS2_INCLUDES)

find_path (UDUNITS2_INCLUDES udunits2.h
  HINTS UDUNITS2_DIR ENV UDUNITS2_DIR)

find_library (UDUNITS2_LIBRARIES NAMES udunits2)

set(CMAKE_REQUIRED_INCLUDES ${UDUNITS2_INCLUDES})
set(CMAKE_REQUIRED_LIBRARIES ${UDUNITS2_LIBRARIES})

# handle the QUIETLY and REQUIRED arguments and set UDUNITS2_FOUND to TRUE if
# all listed variables are TRUE
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (UDUNITS2 DEFAULT_MSG UDUNITS2_LIBRARIES UDUNITS2_INCLUDES)

mark_as_advanced (UDUNITS2_LIBRARIES UDUNITS2_INCLUDES)
