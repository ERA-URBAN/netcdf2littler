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

logical bogus, append
real :: ter
integer :: idx
data bogus /.false./
integer:: iseq_num = 1

real,dimension(kx) :: p,z,t,td,spd,dir,u,v,rh,thick, slp
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
integer :: timeLength, device
REAL,DIMENSION(:), ALLOCATABLE :: humidity, height, speed
REAL,DIMENSION(:), ALLOCATABLE :: temperature, dew_point
REAL,DIMENSION(:), ALLOCATABLE :: pressure, direction, thickness
REAL,DIMENSION(:), ALLOCATABLE :: uwind, vwind, refpres
character(len=14), dimension(:), allocatable :: time_littler
real,dimension(:), allocatable    :: time
character(len=100) :: timeunits
REAL :: lon, lat
real :: elevation
character(len=30), dimension(99):: variable_name = 'not defined'
character(len=30), dimension(99):: variable_mapping = 'not defined'
character(len=8):: startdate, enddate
character(len=99):: filename, outfile
integer :: devices, dimensions
real :: fill_value
character(19) :: datetime
integer :: i, number_of_variables = 0
integer :: startindex, countnum
logical::lookForFile=.FALSE.
integer::narg,cptArg
logical::fileExist
character(99) :: infile, name

! get filename, variable_names and variable_mappings from namelist
namelist /group_name/ filename, variable_name, variable_mapping, devices, &
    outfile, dimensions, startdate, enddate

! define logging file
call define_logfile('convert_littler.log')

!Check if any arguments are found
narg=command_argument_count()
!Loop over the arguments
if(narg>0) then
  do cptArg=1, narg
    call get_command_argument(cptArg, name)
    select case(adjustl(name))
    !First known args
      case("--namelist")
      lookForFile=.TRUE. !change logical value
      case default
      !Treat the second arg of a serie
      if(LookForFile) then
        infile=adjustl(name) !assign a value to infile
        inquire(file=infile, exist=fileExist) !check if the file exists
        if(.not.fileExist) then
          call log_message('CRITICAL', 'File not found: '//infile)
        endif
        LookForFile=.FALSE. !put the logical variable to its initial value
      else
        call log_message('ERROR', "Option "// trim(name)// " unknown")
      endif
    end select
  end do
else
  ! set a default namelist name
  infile = './input.namelist'
  inquire(file=infile,exist=fileExist)!check if it exist
  if(.not.fileExist)then
    write(*,*)'file ',infile,' not found'
    stop
  endif
endif

! read the namelist (either cli supplied or default
open(10,file=infile)
read(10,group_name)
close(10)

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
end if

inquire(file=filename, exist=fileExist) !check if the netcdf exists
if(.not.fileExist) then
  call log_message('CRITICAL', 'File not found: '//filename)
endif

! Set initial value of append to false (create a new output file)
append = .false.

call current_datetime(datetime)
! get time and time units
call log_message('INFO', 'Extracting time and &
  & time units from netcdf file.')
call readtimedim(filename, time, timeunits)
timeLength = size(time)
allocate(time_littler(timeLength))
call log_message('INFO', 'Converting time to little_R date format')
call time_to_littler_date(time, timeunits, time_littler, startindex, &
                          countnum, startdate, enddate)
call log_message('INFO', concat_str_int('Number of timesteps found: ', &
                 countnum))
! Don't write anything if there are no timesteps in the file for our selection
if (countnum==0) then
  call log_message('ERROR', 'No timesteps found in file: '//filename)
endif
! loop over all devices
do device=1,devices
  call log_message('INFO', concat_str_int('Processing devices, device: ', &
    device))
  ! read variable
  if (allocated(temperature)) deallocate(temperature)
  allocate(temperature(countnum))
  if (allocated(humidity)) deallocate(humidity)
  allocate(humidity(countnum))
  if (allocated(height)) deallocate(height)
  allocate(height(countnum))
  if (allocated(speed)) deallocate(speed)
  allocate(speed(countnum))
  if (allocated(dew_point)) deallocate(dew_point)
  allocate(dew_point(countnum))
  if (allocated(pressure)) deallocate(pressure)
  allocate(pressure(countnum))
  if (allocated(refpres)) deallocate(refpres)
  allocate(refpres(countnum))
  if (allocated(direction)) deallocate(direction)
  allocate(direction(countnum))
  if (allocated(thickness)) deallocate(thickness)
  allocate(thickness(countnum))
  if (allocated(uwind)) deallocate(uwind)
  allocate(uwind(countnum))
  if (allocated(vwind)) deallocate(vwind)
  allocate(vwind(countnum))
  do idx=1,number_of_variables
    ! read specified variables from netCDF file
    call read_variables(lat, lon, elevation, humidity, height, speed, temperature, dew_point, &
      pressure, refpres, direction, thickness, uwind, vwind, variable_name, &
      variable_mapping, filename, fill_value, idx, device, dimensions, startindex, countnum)
    end do
  ! write obs to file in LITTLE_R format
  call write_obs_littler(pressure,height,temperature,dew_point,speed, &
  direction,uwind,vwind,humidity,thickness,refpres, p_qc,z_qc,t_qc,td_qc,spd_qc, &
  dir_qc,u_qc,v_qc,rh_qc,thick_qc,elevation,lat,lon,variable_mapping, &
  kx, bogus, iseq_num, time_littler(startindex:startindex+countnum-1), fill_value, outfile, append )
end do
stop 99999
end


