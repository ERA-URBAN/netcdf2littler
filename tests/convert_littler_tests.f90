module convert_littler_tests
! minimal unit testing framework for write_littler

use readncdf
use write_littler
use logging

implicit none

private
public :: main

integer, parameter :: stdout = 6

contains

logical function assert(condition, test_name)
 ! asserts if the condition is true/false and returns the status of the tests
  logical, intent(in) :: condition
  character(len=*), intent(in) :: test_name
  character(len=60) :: output_test_name
  assert = condition
  output_test_name = test_name  ! report only first 60 characters of test_name
  if (assert) then
    write(unit=stdout, fmt='(A)')'test '//output_test_name//': '//&
      char(27)//'[32mPASS'//char(27)//'[0m'
  else
    write(unit=stdout, fmt='(A)')'test '//output_test_name//': '//&
      char(27)//'[31mFAIL'//char(27)//'[0m'
  end if
end function assert


subroutine initialize_tests(tests, ntests)
  ! allocate logical array with test results
  ! write output header to stdout
  logical, dimension(:), allocatable, intent(inout) :: tests
  integer, intent(in) :: ntests
  ! allocate logical array with test results
  if (allocated(tests)) deallocate(tests)
  allocate(tests(ntests))
  ! write header of test results
  write(unit=stdout, fmt='(A)')
  write(unit=stdout, fmt='(71("-"))')
  write(unit=stdout, fmt='(T6, A,T66,A)') 'test name', 'result'
  write(unit=stdout, fmt='(71("-"))')
end subroutine initialize_tests


subroutine report_tests(tests)
! reports the total number of tests and the number of passes/fails
  logical, dimension(:), intent(in) :: tests
  integer :: n, nsuccess, nfailure
  ! set initial number of passes/fails to 0
  nsuccess = 0
  nfailure = 0
  ! loop over all tests and update passes/fails
  do n = 1, size(tests)  
    if (tests(n)) then
      nsuccess = nsuccess + 1
    else
      nfailure = nfailure + 1
    end if
  end do
  ! write the result to the screen
  write(unit=stdout, fmt='(71("-"))')
  write(unit=stdout, fmt='(A,I3,A)')'Ran a total of ', size(tests),' tests.'
  write(unit=stdout, fmt='(I3,A,I3,A)')nsuccess,' tests PASSED, ',nfailure,' tests FAILED.'
  write(unit=stdout, fmt='(A)')
end subroutine report_tests


subroutine main
  ! main routine that runs all the tests
  logical,dimension(:),allocatable :: tests  ! logical array with test results
  INTEGER :: ntests  ! total number of tests
  INTEGER :: n=1  ! test counter
  ntests = 21  ! modify if adding new tests
  call initialize_tests(tests,ntests)
  call test_dateint(tests, n)
  call test_get_default_littler(tests, n)
  call report_tests(tests)
end subroutine main


subroutine test_dateint(tests, n)
  ! test if dateint returns a character string with YYYYMMDDHHMMSS
  integer, intent(inout) :: n
  logical, dimension(*), intent(inout) :: tests
  tests(n) = assert(dateint(2010,01,02,23,30,59)=='20100102233059', 'dateint')
  n = n+1
end subroutine test_dateint


subroutine test_get_default_littler(tests, n)
  ! description
  integer, intent(inout) :: n
  logical, dimension(*), intent(inout) :: tests
  integer, parameter :: kx=1
  real, dimension(kx) :: dpressure, dheight, dtemperature, ddew_point
  real, dimension(kx) ::  dspeed, ddirection, du, dv, drh, dthickness
  integer, dimension(kx) :: dpressure_qc, dheight_qc, dtemperature_qc
  integer, dimension(kx) ::  ddew_point_qc, dspeed_qc, ddirection_qc, du_qc
  integer, dimension(kx) :: dv_qc, drh_qc, dthickness_qc
  call get_default_littler(dpressure, dheight, dtemperature, ddew_point, &
  dspeed, ddirection, du, dv, drh, dthickness,dpressure_qc, dheight_qc, dtemperature_qc, &
  ddew_point_qc, dspeed_qc, ddirection_qc, du_qc, &
  dv_qc, drh_qc, dthickness_qc, kx)
  ! default values
  tests(n) = assert(dpressure(1)==-888888., 'get_default_littler: default pressure')
  n=n+1
  tests(n) = assert(dheight(1)==-888888., 'get_default_littler: default height')
  n=n+1
  tests(n) = assert(dtemperature(1)==-888888., 'get_default_littler: default temperature')
  n=n+1
  tests(n) = assert(ddew_point(1)==-888888., 'get_default_littler: default dew_point')
  n=n+1
  tests(n) = assert(dspeed(1)==-888888., 'get_default_littler: default speed')
  n=n+1
  tests(n) = assert(ddirection(1)==-888888., 'get_default_littler: default direction')
  n=n+1
  tests(n) = assert(du(1)==-888888., 'get_default_littler: default u velocity')
  n=n+1
  tests(n) = assert(dv(1)==-888888., 'get_default_littler: default v velocity')
  n=n+1
  tests(n) = assert(drh(1)==-888888., 'get_default_littler: default relative humidity')
  n=n+1
  tests(n) = assert(dthickness(1)==-888888., 'get_default_littler: default thickness')
  n=n+1
  ! default qc values
  tests(n) = assert(dpressure_qc(1)==0, 'get_default_littler: default pressure_qc')
  n=n+1
  tests(n) = assert(dheight_qc(1)==0, 'get_default_littler: default height_qc')
  n=n+1
  tests(n) = assert(dtemperature_qc(1)==0, 'get_default_littler: default temperature_qc')
  n=n+1
  tests(n) = assert(ddew_point_qc(1)==0, 'get_default_littler: default dew_point_qc')
  n=n+1
  tests(n) = assert(dspeed_qc(1)==0, 'get_default_littler: default speed_qc')
  n=n+1
  tests(n) = assert(ddirection_qc(1)==0, 'get_default_littler: default direction_qc')
  n=n+1
  tests(n) = assert(du_qc(1)==0, 'get_default_littler: default u velocity_qc')
  n=n+1
  tests(n) = assert(dv_qc(1)==0, 'get_default_littler: default v velocity_qc')
  n=n+1
  tests(n) = assert(drh_qc(1)==0, 'get_default_littler: default relative humidity_qc')
  n=n+1
  tests(n) = assert(dthickness_qc(1)==0, 'get_default_littler: default thickness_qc')
end subroutine test_get_default_littler

end module convert_littler_tests