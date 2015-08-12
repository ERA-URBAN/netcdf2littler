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

INTEGER:: pp
REAL :: lon, lat

character(len=30), dimension(2):: variable_name
character(len=30), dimension(2):: variable_mapping
character(len=30):: filename, outfile
integer :: devices
real :: fill_value
      

! get filename, variable_names and variable_mappings from namelist
namelist /group_name/ filename, variable_name, variable_mapping, devices, &
    outfile
  open(10,file='./wageningen.namelist')
  read(10,group_name)
  close(10)

call get_default_littler(dpressure, dheight, dtemperature, ddew_point, &
  dspeed, ddirection, du, dv, drh, dthickness,dpressure_qc, &
  dheight_qc, dtemperature_qc, ddew_point_qc, dspeed_qc, &
  ddirection_qc, du_qc, dv_qc, drh_qc, dthickness_qc, kx)
! get length of time axis and time axis
call readtimedim(filename, time, timeunits)
timeLength = size(time)
allocate(time_littler(timeLength))
call time_to_littler_date(time, timeunits, time_littler)

! loop over all devices
do device=1,devices
  ! read variable
  do idx=1,size(variable_name)
    select case (trim(variable_mapping(idx)))
    case ('temperature')
      if (allocated(temperature)) deallocate(temperature)
      allocate(temperature(timeLength))
      CALL readstepnc (filename, trim(variable_name(idx)), &
        temperature, fill_value, lon, lat, device)
      case ('humidity')
        if (.not. allocated(humidity)) allocate(humidity(timeLength))
        CALL readstepnc (filename, variable_name(idx), humidity, &
          fill_value, lon, lat, device)
      case ('speed')
        if (.not. allocated(speed)) allocate(speed(timeLength))
        CALL readstepnc_single (filename, variable_name(idx), speed, &
          fill_value, lon, lat)
      case ('pressure')
        if (.not. allocated(pressure)) allocate(pressure(timeLength))
        CALL readstepnc_single (filename, variable_name(idx), pressure, &
          fill_value, lon, lat)
      case ('direction')
        if (.not. allocated(direction)) allocate(direction(timeLength))
        CALL readstepnc_single (filename, variable_name(idx), direction, &
          fill_value, lon, lat)
      case ('uwind')
        if (.not. allocated(uwind)) allocate(uwind(timeLength))
        CALL readstepnc_single (filename, variable_name(idx), uwind, &
          fill_value, lon, lat)
      case ('uwind')
        if (.not. allocated(vwind)) allocate(vwind(timeLength))
        CALL readstepnc_single (filename, variable_name(idx), vwind, &
          fill_value, lon, lat)
      case ('height')
        if (.not. allocated(height)) allocate(height(timeLength))
        CALL readstepnc_single (filename, variable_name(idx), height, &
          fill_value, lon, lat)
      case ('dew_point')
        if (.not. allocated(dew_point)) allocate(dew_point(timeLength))
        CALL readstepnc_single (filename, variable_name(idx), dew_point, &
          fill_value, lon, lat)
    end select
  end do
  ! put this in a subroutine or function
  do idx=1,size(time_littler)
    ! set input data, fall back to default values
    ! add: allow for multiple levels
    if (ANY(variable_mapping=="pressure" ) .AND. &
      (pressure(idx) /= fill_value)) then
      p = pressure(idx)
    else
      p = dpressure
    end if
    if (ANY(variable_mapping=="height" ) .AND. &
      (height(idx) /= fill_value)) then
      z = height(idx) ! either p or z must be defined
    else
      z = 3.0
      !z = dheight
    endif
    if (ANY(variable_mapping=="temperature" ) .AND. &
      (temperature(idx) /= fill_value)) then
      t = temperature(idx) + 273.15 ! convert to K
    else
      t = dtemperature
    end if
    if (ANY(variable_mapping=="dew_point" ) .AND. &
      (dew_point(idx) /= fill_value)) then
      td = dew_point(idx)
    else
      td = ddew_point
    end if
    if (ANY(variable_mapping=="speed" ) .AND. &
      (spd(idx) /= fill_value)) then
      spd = speed(idx)
    else
      spd = dspeed
    end if
    if (ANY(variable_mapping=="direction") .AND. & 
      (direction(idx) /= fill_value)) then
      dir = direction(idx)
    else
      dir = ddirection
    end if
    if (ANY(variable_mapping=="uwind" ) .AND. &
      (uwind(idx) /= fill_value)) then
      u = uwind(idx)
    else
      u = du
    end if
    if (ANY(variable_mapping=="vwind" ) .AND. &
      (vwind(idx) /= fill_value)) then
      v = vwind(idx)
    else
      v = dv
    end if
    if (ANY(variable_mapping=="temperature" ) .AND. &
      (humidity(idx) /= fill_value)) then
      rh = humidity(idx)
    else
      rh = drh
    end if
    if (ANY(variable_mapping=="thickness") .AND. &
      (thickness(idx) /= fill_value)) then
      thick = thickness(idx)
    else
      thick = dthickness
    end if
    p_qc = dpressure_qc
    z_qc = dheight_qc
    t_qc = dtemperature_qc
    td_qc = ddew_point_qc
    spd_qc = dspeed_qc
    dir_qc = ddirection_qc
    u_qc = du_qc
    v_qc = dv_qc
    rh_qc = drh_qc
    thick_qc = dthickness_qc
    if ( kx == 1 ) then ! surface variables
      call write_obs(p,z,t,td,spd,dir,u,v,rh,thick, &
        p_qc,z_qc,t_qc,td_qc,spd_qc,dir_qc,u_qc,v_qc,rh_qc,thick_qc, &
        slp, ter, lat, lon, time_littler(idx), kx, &
        '99001  Maybe more site info             ', &
        'SURFACE DATA FROM ??????????? SOURCE    ', &
        'FM-12 SYNOP                             ', &
        '                                        ', &
        bogus , iseq_num , 2, outfile )
    else ! vertical profile
      call write_obs(p,z,t,td,spd,dir,u,v,rh,thick, &
        p_qc,z_qc,t_qc,td_qc,spd_qc,dir_qc,u_qc,v_qc,rh_qc,thick_qc, &
        slp, ter, lat, lon, time_littler(idx), kx, &
        '99001  Maybe more site info             ', &
        'SOUNDINGS FROM ????????? SOURCE         ', &
        'FM-35 TEMP                              ', &
        '                                        ', &
        bogus , iseq_num , 2, outfile )
    endif
  end do
end do
stop 99999
end


