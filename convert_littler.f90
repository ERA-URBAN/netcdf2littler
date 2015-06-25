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

parameter (kx=1)

logical bogus
integer :: idx
data slp/101325./
data ter/1./
data bogus /.false./
real,dimension(kx) :: p,z,t,td,spd,dir,u,v,rh,thick
integer,dimension(kx) :: p_qc,z_qc,t_qc,td_qc,spd_qc
integer, dimension(kx) :: dir_qc,u_qc,v_qc,rh_qc,thick_qc
          
real, dimension(kx) :: dpressure, dheight, dtemperature, ddew_point
real, dimension(kx) ::  dspeed, ddirection, du, dv, drh, dthickness
integer, dimension(kx) :: dpressure_qc, dheight_qc, dtemperature_qc
integer, dimension(kx) ::  ddew_point_qc, dspeed_qc, ddirection_qc, du_qc
integer, dimension(kx) :: dv_qc, drh_qc, dthickness_qc
      
character(len=14) :: timechar
character *20 date_char
character *40 string1, string2 , string3 , string4
          
INTEGER :: timeLength
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
character(len=30):: filename
real :: fill_value
      
! define interface
interface
  subroutine get_default_littler(dpressure, dheight, dtemperature, ddew_point, &
    dspeed, ddirection, du, dv, drh, dthickness,dpressure_qc, dheight_qc, dtemperature_qc, &
    ddew_point_qc, dspeed_qc, ddirection_qc, du_qc, &
    dv_qc, drh_qc, dthickness_qc, kx)
    integer, intent(in) :: kx
    real, dimension(kx), intent(out) :: dpressure, dheight, dtemperature, ddew_point
    real, dimension(kx), intent(out) ::  dspeed, ddirection, du, dv, drh, dthickness
    integer, dimension(kx), intent(out) :: dpressure_qc, dheight_qc, dtemperature_qc
    integer, dimension(kx), intent(out) ::  ddew_point_qc, dspeed_qc, ddirection_qc, du_qc
    integer, dimension(kx), intent(out) :: dv_qc, drh_qc, dthickness_qc
    integer :: k
  end subroutine get_default_littler
  subroutine write_obs(p,z,t,td,spd,dir,u,v,rh,thick, &
    p_qc,z_qc,t_qc,td_qc,spd_qc,dir_qc,u_qc,v_qc,rh_qc,thick_qc, &
    slp , ter , xlat , xlon , timechar , kx , &
    string1 , string2 , string3 , string4 , bogus , iseq_num , &
    iunit )
    real,dimension(kx),intent(in) :: p,z,t,td,spd,dir,u,v,rh,thick
    real, intent(in) :: xlon, xlat
    integer,dimension(kx),intent(in) :: p_qc,z_qc,t_qc,td_qc,spd_qc
    integer, dimension(kx), intent(in) :: dir_qc,u_qc,v_qc,rh_qc,thick_qc
    character(len=14) :: timechar
    character *20 date_char
    character *40 string1, string2 , string3 , string4
    logical :: bogus
  end subroutine write_obs
  subroutine time_to_littler_date(time, timeunits, time_littler)
    real,dimension(:),intent(in)    :: time
    character(len=100), intent(in) :: timeunits
    character(len=14), dimension(:), intent(out) :: time_littler
  end subroutine time_to_littler_date  
end interface

! get filename, variable_names and variable_mappings from namelist
namelist /group_name/ filename, variable_name, variable_mapping
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
  
  ! read variable
  do idx=1,size(variable_name)
    select case (trim(variable_mapping(idx)))
    case ('temperature')
      allocate(temperature(timeLength))
      CALL readstepnc (filename, trim(variable_name(idx)), &
        temperature, fill_value, lon, lat)
      case ('humidity')
        allocate(humidity(timeLength))
        CALL readstepnc (filename, variable_name(idx), humidity, &
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
      z = dheight
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
        bogus , iseq_num , 2 )
    else ! vertical profile
      call write_obs(p,z,t,td,spd,dir,u,v,rh,thick, &
        p_qc,z_qc,t_qc,td_qc,spd_qc,dir_qc,u_qc,v_qc,rh_qc,thick_qc, &
        slp, ter, lat, lon, time_littler(idx), kx, &
        '99001  Maybe more site info             ', &
        'SOUNDINGS FROM ????????? SOURCE         ', &
        'FM-35 TEMP                              ', &
        '                                        ', &
        bogus , iseq_num , 2 )
    endif
  end do
  stop 99999
end


subroutine write_obs(p,z,t,td,spd,dir,u,v,rh,thick, &
  p_qc,z_qc,t_qc,td_qc,spd_qc,dir_qc,u_qc,v_qc,rh_qc,thick_qc, &
  slp , ter , xlat , xlon , timechar , kx , &
  string1 , string2 , string3 , string4 , bogus , iseq_num , &
  iunit )
  ! TODO: add fields in the header format as arguments
  ! write observations in LITTLE_R format for WRF data assimilation
  ! in: - data variables &* strings to write in LITTLE_R format
  ! otu: - output file in LITTLE_R format
  !implicit none
  real,dimension(kx),intent(in) :: p,z,t,td,spd,dir,u,v,rh,thick
  real, intent(in) :: xlon, xlat
  integer,dimension(kx),intent(in) :: p_qc,z_qc,t_qc,td_qc,spd_qc
  integer, dimension(kx), intent(in) :: dir_qc,u_qc,v_qc,rh_qc,thick_qc
  
  character(len=14) :: timechar
  character *20 date_char
  character *40 string1, string2 , string3 , string4
  character *84  rpt_format 
  character *22  meas_format 
  character *14  end_format
  logical bogus

  rpt_format =  ' ( 2f20.5 , 2a40 , ' &
                  // ' 2a40 , 1f20.5 , 5i10 , 3L10 , ' &
                  // ' 2i10 , a20 ,  13( f13.5 , i7 ) ) '
  meas_format =  ' ( 10( f13.5 , i7 ) ) '
  end_format = ' ( 3 ( i7 ) ) ' 

  ! write header record
  WRITE ( UNIT = iunit , ERR = 19 , FMT = rpt_format ) &
    xlat,xlon, string1 , string2 , &
    string3 , string4 , ter, kx*6, 0,0,iseq_num,0, &
    .true.,bogus,.false., &
    -888888, -888888, timechar , &
    slp,0,-888888.,0, -888888.,0, -888888.,0, -888888.,0, &
    -888888.,0, &
    -888888.,0, -888888.,0, -888888.,0, -888888.,0, &
    -888888.,0, &
    -888888.,0, -888888.,0 
   
  do k = 1 , kx
    WRITE ( UNIT = iunit , ERR = 19 , FMT = meas_format ) &
               p(k), p_qc(k), z(k), z_qc(k), t(k), t_qc(k), td(k), td_qc(k), &
               spd(k), spd_qc(k), dir(k), dir_qc(k), u(k), u_qc(k), &
               v(k), v_qc(k), rh(k), rh_qc(k), thick(k), thick_qc(k)
  end do
      ! write ending record
      WRITE ( UNIT = iunit , ERR = 19 , FMT = meas_format ) & 
      -777777.,0, -777777.,0,float(kx),0, &
      -888888.,0, -888888.,0, -888888.,0, &
      -888888.,0, -888888.,0, -888888.,0, &
      -888888.,0
      WRITE ( UNIT = iunit , ERR = 19 , FMT = end_format )  kx, 0, 0

      return
19    continue
      print *,'troubles writing a sounding'
      stop 19
    end subroutine write_obs


subroutine get_default_littler(dpressure, dheight, dtemperature, ddew_point, &
  dspeed, ddirection, du, dv, drh, dthickness,dpressure_qc, dheight_qc, dtemperature_qc, &
  ddew_point_qc, dspeed_qc, ddirection_qc, du_qc, &
  dv_qc, drh_qc, dthickness_qc, kx)
  ! set default values for LITTLE_R format
  ! -888888.:  measurement not available
  ! _qc = 0: no quality control
  implicit none
  integer, intent(in) :: kx
  real, dimension(kx), intent(out) :: dpressure, dheight, dtemperature, ddew_point
  real, dimension(kx), intent(out) ::  dspeed, ddirection, du, dv, drh, dthickness
  integer, dimension(kx), intent(out) :: dpressure_qc, dheight_qc, dtemperature_qc
  integer, dimension(kx), intent(out) ::  ddew_point_qc, dspeed_qc, ddirection_qc, du_qc
  integer, dimension(kx), intent(out) :: dv_qc, drh_qc, dthickness_qc
  integer :: k
  do k=1,kx
    dpressure(k) = -888888.
    dheight(k) = -888888.
    dtemperature(k) = -888888.
    ddew_point(k) = -888888.
    dspeed(k) = -888888.
    ddirection(k) = -888888.
    du(k) = -888888.
    dv(k) = -888888.
    drh(k) = -888888.
    dthickness(k) = -888888.
    dpressure_qc(k) = 0
    dheight_qc(k) = 0
    dtemperature_qc(k) = 0
    ddew_point_qc(k) = 0
    dspeed_qc(k) = 0
    ddirection_qc(k) = 0
    du_qc(k) = 0
    dv_qc(k) = 0
    drh_qc(k) = 0
    dthickness_qc(k) = 0
  end do
end subroutine get_default_littler



subroutine time_to_littler_date(time, timeunits, time_littler)
  ! convert time to LITTLE_R time format
  ! in:   - time: time array
  !       - timeunits: units of time of time array
  ! out:  - time_littler: time in LITTLE_R format
  use f_udunits_2
  implicit none
  real(c_double) :: tt
  real,dimension(:),intent(in) :: time
  character(len=100), intent(in) :: timeunits
  character(len=14), dimension(:), intent(out) :: time_littler
  real(c_double) :: resolution
  type(cv_converter_ptr) :: time_cvt0, time_cvt
  type(ut_system_ptr) :: sys
  type(ut_unit_ptr) :: sec0, unit1, time_base0, time_base
  integer :: charset
  integer hour,minute,year,month,day
  real(c_double) :: second, converted_time
  real *8, parameter :: ZERO = 0.0
  character(len=99) :: char_a,char_b,char_c,char_d,char_e,char_f
  integer :: ii
  ! define interface
  interface
    pure character(len=14) function dateint(hour, minute, year,month,day,second)
      character(len=99) :: char_a,char_b,char_c,char_d,char_e,char_f
      integer, intent(in) :: hour,minute,year,month,day, second
    end function dateint
  end interface
  charset = UT_ASCII
  sys = f_ut_read_xml("")
  sec0 = f_ut_parse(sys,"second",charset)
  unit1 = f_ut_parse(sys,timeunits,charset)
  time_base0 = f_ut_offset_by_time(sec0,f_ut_encode_time(2001,01,01,00,00,ZERO))
  time_cvt0 = f_ut_get_converter(unit1,time_base0)
  year=0 ; month=0; day=0 ; hour=0 ; minute=0 ; second=0.0
  resolution = -999.999
  ! loop over all timesteps
  do ii=1,size(time)
    tt = time(ii)
    converted_time = f_cv_convert_double(time_cvt0,tt)
    call f_ut_decode_time(converted_time,year,month,day,hour,minute, &
                          second, resolution)
    time_littler(ii) = dateint(hour,minute,year,month,day,int(second))
  end do
end subroutine time_to_littler_date


pure character(len=14) function dateint(hour,minute,year,month,day,second)
  ! convert integers of hour, minute, year, month, day, second 
  ! into character string of YYYYMMDDhhmmss
  ! return character string
  implicit none
  character(len=99) :: char_a,char_b,char_c,char_d,char_e,char_f
  integer, intent(in) :: hour,minute,year,month,day, second
  ! convert integer to character strings, add leading 0 if needed
  write(char_a,fmt=*) year
  write(char_b,'(I0.2)')  month
  write(char_c,'(I0.2)')  day
  write(char_d, '(I0.2)')  hour
  write(char_e, '(I0.2)')  minute
  write(char_f, '(I0.2)')  second
  ! concatenate character strings
  dateint = trim(adjustl(char_a))//trim(adjustl(char_b))// &
    trim(adjustl(char_c))//trim(adjustl(char_d))// &
    trim(adjustl(char_e))//trim(adjustl(char_f))
  return
end function dateint