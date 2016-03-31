program run_tests

use convert_littler_tests

implicit none

integer :: logunit

! define logging file
logunit=99
open(unit=logunit,file='run_tests.log',form='formatted')

call main
end program run_tests