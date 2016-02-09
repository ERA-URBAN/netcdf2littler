program netcdftolittler

!     ... pressure is in Pa, height in m, temperature and dew point are in
!         K, speed is in m/s, and direction is in degrees

!     ... sea level pressure is in Pa, terrain elevation is in m, latitude
!         is in degrees N, longitude is in degrees E

!     ... to put in a surface observation, make only a single level "sounding"
!         and make the value of the height equal the terrain elevation -- PRESTO!

!     ... the first 40 character string may be used for the description of
!         the station (i.e. name city country, etc)

!     ... the second character string we use for our source

!     ... the third string should be left alone, it uses the phrase "FM-35 TEMP"
!         for an upper air station, and should use "FM-12 SYNOP" for surface data

!     ... the fourth string is unused, feel free to experiment with labels!

!     ... bogus data are not subject to quality control

! TODO: for surface data we need to write height to LITTLE_R file
! otherwise, we need to write pressure to LITTLE_R file

use readncdf
use write_littler
use logging

implicit none

integer, parameter :: kx=1

logical bogus
real :: slp, ter
integer :: idx
data bogus /.false./
integer:: iseq_num = 1

real,dimension(kx) :: p,z,t,td,spd,dir,u,v,rh,thick
integer,dimension(kx) :: p_qc,z_qc,t_qc,td_qc,spd_qc
integer, dimension(kx) :: dir_qc,u_qc,v_qc,rh_qc,thick_qc
! iseq_num: sequential number -> domain number
real, dimension(kx) :: dpressure, dheight, dtemperature, ddew_point
real, dimension(kx) ::  dspeed, ddirection, du, dv, drh, dthickness
integer, dimension(kx) :: dpressure_qc, dheight_qc, dtemperature_qc
integer, dimension(kx) ::  ddew_point_qc, dspeed_qc, ddirection_qc, du_qc
integer, dimension(kx) :: dv_qc, drh_qc, dthickness_qc
character(len=14) :: timechar
character *20 date_char
character *40 string1, string2 , string3 , string4
INTEGER :: timeLength, device
REAL,DIMENSION(:), ALLOCATABLE :: humidity, height, speed
REAL,DIMENSION(:), ALLOCATABLE :: temperature, dew_point
REAL,DIMENSION(:), ALLOCATABLE :: pressure, direction, thickness
REAL,DIMENSION(:), ALLOCATABLE :: uwind, vwind
character(len=14), dimension(:), allocatable :: time_littler
real,dimension(:), allocatable    :: time
character(len=100) :: timeunits
REAL :: lon, lat
character(len=30), dimension(99):: variable_name = 'not defined'
character(len=30), dimension(99):: variable_mapping = 'not defined'
character(len=30):: filename, outfile
integer :: devices, dimensions
real :: fill_value
integer :: logunit
character(19) :: datetime
integer :: i, number_of_variables = 0

! get filename, variable_names and variable_mappings from namelist
namelist /group_name/ filename, variable_name, variable_mapping, devices, &
    outfile, dimensions
  open(10,file='./wageningen.namelist')
  read(10,group_name)
  close(10)

! define logging file
logunit=99
open(unit=logunit,file='convert_littler.log',form='formatted')
call log_message('INFO', 'Parsing namelist finished.')

! find number of variables in namelist
do i=1,99
  if (variable_name(i) /= 'not defined') then
    number_of_variables = number_of_variables + 1
  end if
end do
call log_message('INFO', concat_str_int('Number of variables found: ', &
                 number_of_variables))

! check if dimensions and namelist are correct in namelist
if (.not. ((dimensions==1 .AND. devices==1) .or. &
  (dimensions==2 .AND. devices>=1))) then
  call log_message('ERROR', 'Error in namelist specification of &
    & dimensions and devices.')
  STOP 'Error in namelist specification of dimensions and devices'
end if

call current_datetime(datetime)
! get time and time units
call log_message('INFO', 'Extracting time and &
  & time units from netcdf file.')
call readtimedim(filename, time, timeunits)
timeLength = size(time)
allocate(time_littler(timeLength))
call log_message('INFO', 'Converting time to little_R date format')
call time_to_littler_date(time, timeunits, time_littler)

! loop over all devices
do device=1,devices
  call log_message('INFO', concat_str_int('Processing devices, device: ', &
    device))
  ! read variable
  if (allocated(temperature)) deallocate(temperature)
  allocate(temperature(timeLength))
  if (allocated(humidity)) deallocate(humidity)
  allocate(humidity(timeLength))
  if (allocated(height)) deallocate(height)
  allocate(height(timeLength))
  if (allocated(speed)) deallocate(speed)
  allocate(speed(timeLength))
  if (allocated(dew_point)) deallocate(dew_point)
  allocate(dew_point(timeLength))
  if (allocated(pressure)) deallocate(pressure)
  allocate(pressure(timeLength))
  if (allocated(direction)) deallocate(direction)
  allocate(direction(timeLength))
  if (allocated(thickness)) deallocate(thickness)
  allocate(thickness(timeLength))
  if (allocated(uwind)) deallocate(uwind)
  allocate(uwind(timeLength))
  if (allocated(vwind)) deallocate(vwind)
  allocate(vwind(timeLength))
  
  do idx=1,number_of_variables
    ! read specified variables from netCDF file
    call read_variables(lat, lon, humidity, height, speed, temperature, dew_point, &
      pressure, direction, thickness, uwind, vwind, variable_name, &
      variable_mapping, filename, fill_value, idx, device, dimensions)
  end do
  ! write obs to file in LITTLE_R format
  call write_obs_littler(pressure,height,temperature,dew_point,speed, &
  direction,uwind,vwind,humidity,thickness,p_qc,z_qc,t_qc,td_qc,spd_qc, &
  dir_qc,u_qc,v_qc,rh_qc,thick_qc,slp,ter,lat,lon,variable_mapping, &
  kx, bogus, iseq_num, time_littler, fill_value, outfile )
end do
stop 99999
end


