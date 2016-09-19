module readncdf

  use logging

  implicit none
 
  contains

subroutine readstepnc(fname,var_name,ff, fill_value, lon, lat,elevation, device, startindex, countnum)
  ! A condensed way to read a variable form  netcdf file
  ! it is implied that the format is ff(time, device)
  ! in:  - fname: netcdf filename
  !      - var_name: name of variable to read
  ! out: - ff: output array
  !      - lon, lat: longitude and latitude of measurement
  !      - fill_value: fill_value used in netcdf file
  use netcdf
  use f_udunits_2
  use check_status
  ! declare calling variables
  character(len=*),intent(in) :: fname, var_name
  real,dimension(:),intent(out) :: ff
  real,intent(out) :: lon, lat, elevation, fill_value
  integer, intent(in) :: device, startindex, countnum
  ! declare local variables
  integer :: nc_id,var_id,ndim,nvar,nattr,unlim_id,fmt, &
             ii,status,lo,la,le,ld,ti,lh, dlength, charset, &
             hour,minute,year,month,day
  character(len=15) :: dname, varname
  character(len=100) :: timeunits
  real,dimension(:), allocatable:: var_dummy
  real :: sf,ofs
  real(c_double) :: second, tt, resolution, converted_time
  type(cv_converter_ptr) :: time_cvt0, time_cvt
  type(ut_system_ptr) :: sys
  type(ut_unit_ptr) :: sec0, unit1, time_base0, time_base
  real *8, parameter :: ZERO = 0.0
  real,dimension(:),allocatable :: time
  character(len=14),dimension(:),allocatable :: time_littler
  call log_message('DEBUG', 'Entering subroutine readstepnc')
  call check(nf90_open(fname,nf90_nowrite,nc_id))
  call check(nf90_inquire(nc_id,ndim,nvar))
  ! take the dimension names and lengths 
  do ii=1,ndim
    call check(NF90_INQUIRE_DIMENSION(nc_id,ii,dname,len=dlength))
    select case (trim(dname))
      case ('lon','LON','Lon','Longitude','longitude','LONGITUDE')
        lo=dlength
      case ('lat','LAT','Lat','Latitude','latitude','LATITUDE')
        la=dlength
      case ('lev','Lev','LEV','level','levelist','Level')
        le=dlength
      case ('elevation','Elevation','ELEVATION', 'height', 'Height', 'HEIGHT')
        lh=dlength
      case ('device', 'DEVICE')
        ld=dlength
      case ('time','Time','TIME')
        ti=dlength
      case default
        print*,' Error while reading dimensions....'
        print*,' Some dimensions are missing.   '
        print*,' The program is terminating....';STOP
    end select
  end do
  ! extract latitude and longitude values
  do ii=1,nvar
    call check(nf90_inquire_variable(nc_id, ii, varname))
    select case (trim(varname))
      case ('lon','LON','Lon','Longitude','longitude','LONGITUDE')
        call check(nf90_inq_varid(nc_id,trim(varname),var_id))
        call check(nf90_get_var(nc_id, var_id, lon, &
                   start=(/device/)))
      case ('lat','LAT','Lat','Latitude','latitude','LATITUDE')
        call check(nf90_inq_varid(nc_id,trim(varname),var_id))
        call check(nf90_get_var(nc_id, var_id, lat, &
                   start=(/device/)))
      case ('elevation','Elevation','ELEVATION', 'height', 'Height', 'HEIGHT')
        call check(nf90_inq_varid(nc_id,trim(varname),var_id))
        call check(nf90_get_var(nc_id, var_id, elevation, &
                   start=(/device/)))
    end select
  end do
  ! allocate the matrix for reading data. The definition is
  allocate(var_dummy(countnum))
  ! Read all data
  call check(nf90_inq_varid(nc_id,trim(var_name),var_id))
  call check(nf90_get_var(nc_id,var_id,var_dummy, &
             start=(/device,startindex/), count=(/1,countnum/)))
  ! asking if there are the scale_factor and add_offset attributes
  status = nf90_get_att(nc_id,var_id,"scale_factor",sf)
  if (status == -43) sf=1.0
  status = nf90_get_att(nc_id,var_id,"add_offset",ofs)
  if (status == -43) ofs = 0.0
  ff = sf*var_dummy+ofs
  status = nf90_get_att(nc_id,var_id,"_FillValue",fill_value)
  call check(nf90_close(nc_id))
  deallocate(var_dummy)
  call log_message('DEBUG', 'Leaving subroutine readstepnc')
end subroutine readstepnc


subroutine readstepnc_single(fname,var_name,ff, fill_value, lon, lat, elevation, startindex, countnum)
  ! A condensed way to read a variable form  netcdf file
  ! it is implied that the format is ff(time, device)
  ! in:  - fname: netcdf filename
  !      - var_name: name of variable to read
  ! out: - ff: output array
  !      - lon, lat: longitude and latitude of measurement
  !      - fill_value: fill_value used in netcdf file
  use netcdf
  use f_udunits_2
  use check_status
  ! declare calling variables
  character(len=*),intent(in) :: fname, var_name
  real,dimension(:),intent(out) :: ff
  real,intent(out) :: lon, lat, elevation, fill_value
  ! declare local variables
  integer :: nc_id,var_id,ndim,nvar,nattr,unlim_id,fmt, &
             ii,status,lo,la,le,ld,lh, ti, dlength, charset, &
             hour,minute,year,month,day
  character(len=15) :: dname, varname
  character(len=100) :: timeunits
  real,dimension(:), allocatable:: var_dummy
  real :: sf,ofs
  integer, intent(in) :: startindex, countnum
  real(c_double) :: second, tt, resolution, converted_time
  type(cv_converter_ptr) :: time_cvt0, time_cvt
  type(ut_system_ptr) :: sys
  type(ut_unit_ptr) :: sec0, unit1, time_base0, time_base
  real *8, parameter :: ZERO = 0.0
  real,dimension(:),allocatable :: time
  character(len=14),dimension(:),allocatable :: time_littler
  elevation = -888888
  call log_message('DEBUG', 'Entering subroutine readstepnc_single')
  call check(nf90_open(fname,nf90_nowrite,nc_id))
  call check(nf90_inquire(nc_id,ndim,nvar))
  ! take the dimension names and lengths 
  do ii=1,ndim
    call check(NF90_INQUIRE_DIMENSION(nc_id,ii,dname,len=dlength))
    select case (trim(dname))
      case ('lon','LON','Lon','Longitude','longitude','LONGITUDE')
        lo=dlength
      case ('lat','LAT','Lat','Latitude','latitude','LATITUDE')
        la=dlength
      case ('lev','Lev','LEV','level','levelist','Level')
        le=dlength
      case ('elevation','Elevation','ELEVATION', 'height', 'Height', 'HEIGHT')
        lh=dlength
      case ('time','Time','TIME')
        ti=dlength
      case default
        print*,' Error while reading dimensions....'
        print*,' Some dimensions are missing.   '
        print*,' The program is terminating....';STOP
    end select
  end do
  ! extract latitude and longitude values
  do ii=1,nvar
    call check(nf90_inquire_variable(nc_id, ii, varname))
      select case (trim(varname))
      case ('lon','LON','Lon','Longitude','longitude','LONGITUDE')
        call check(nf90_inq_varid(nc_id,trim(varname),var_id))
        call check(nf90_get_var(nc_id, var_id, lon))
      case ('lat','LAT','Lat','Latitude','latitude','LATITUDE')
        call check(nf90_inq_varid(nc_id,trim(varname),var_id))
        call check(nf90_get_var(nc_id, var_id, lat))
      case ('elevation','Elevation','ELEVATION', 'height', 'Height', 'HEIGHT')
        call check(nf90_inq_varid(nc_id,trim(varname),var_id))
        call check(nf90_get_var(nc_id, var_id, elevation))
    end select
  end do
  if (elevation < -100) then
    elevation = -888888
  end if
  ! allocate the matrix for reading data. The definition is
  allocate(var_dummy(countnum))
  ! Read all data
  call check(nf90_inq_varid(nc_id,trim(var_name),var_id))

  call check(nf90_get_var(nc_id,var_id,var_dummy, start=(/startindex/), &
             count=(/countnum/)))
  ! asking if there are the scale_factor and add_offset attributes
  status = nf90_get_att(nc_id,var_id,"scale_factor",sf)
  if (status == -43) sf=1.0
  status = nf90_get_att(nc_id,var_id,"add_offset",ofs)
  if (status == -43) ofs = 0.0
  ! apply scale factor and offset
  ff = sf*var_dummy+ofs
  ! asking if there is a fill_value for the variable
  status = nf90_get_att(nc_id,var_id,"_FillValue",fill_value)
  ! close netcdf file
  call check(nf90_close(nc_id))
  deallocate(var_dummy)
  call log_message('DEBUG', 'Leaving subroutine readstepnc_single')
end subroutine readstepnc_single


subroutine readtimedim(fname, time, timeunits)
  ! read the time dimension of a netcdf file
  ! in:     - fname: netcdf filename
  ! out:    - ti : length of time axis
  !         - time_littler: time in LITTLE_R format
  use netcdf
  use check_status
  ! declare calling variables
  character(len=*),intent(in) :: fname
  real,dimension(:),intent(out), allocatable     :: time
  character(len=100), intent(out) :: timeunits
  integer :: ti
  ! declare local variables
  integer :: nc_id,ndim,nvar, dlength, var_id, &
             ii,lo,la,le,ld,lh, device
  character(len=15) :: dname, varname

  call log_message('DEBUG', 'Entering subroutine readtimedim')

  call check(nf90_open(fname,nf90_nowrite,nc_id))
  call check(nf90_inquire(nc_id,ndim,nvar))
  ! take the dimension names and lengths
  do ii=1,ndim
    call check(NF90_INQUIRE_DIMENSION(nc_id,ii,dname,len=dlength))
    select case (TRIM(dname))
      case ('lon','LON','Lon','Longitude','longitude','LONGITUDE')
        lo=dlength
      case ('lat','LAT','Lat','Latitude','latitude','LATITUDE')
        la=dlength
      case ('lev','Lev','LEV','level','levelist','Level')
        le=dlength
      case ('elevation','Elevation','ELEVATION', 'height', 'Height', 'HEIGHT')
        lh=dlength
      case ('device', 'DEVICE')
        ld=dlength
      case ('time','Time','TIME')
        ti=dlength
    end select
  end do
  ! extract latitude and longitude values
  do ii=1,nvar
    call check(nf90_inquire_variable(nc_id, ii, varname))
    select case (trim(varname))
      case ('time','Time','TIME')
        ! allocate dimensions time axis
        if (allocated(time)) deallocate(time)
        allocate(time(ti))
        !allocate(time_littler(ti))
        ! get time/timeunits from netcdf
        call check(nf90_inq_varid(nc_id,TRIM(varname),var_id))
        call check(nf90_get_var(nc_id, var_id, time))
        call check(nf90_get_att(nc_id, var_id, 'units', timeunits))
    end select
  end do
  call log_message('DEBUG', 'Leaving subroutine readtimedim')
end subroutine readtimedim


subroutine read_variables(lat, lon, elevation, humidity, height, speed, temperature, dew_point, &
      pressure, psfc, refpres, direction, thickness, uwind, vwind, variable_name, &
      variable_mapping, filename, fill_value, idx, device, dimensions, startindex, countnum)
  !
  ! description
  !
  REAL,DIMENSION(:), intent(inout) :: humidity, height, speed
  REAL,DIMENSION(:), intent(inout) :: temperature, dew_point
  REAL,DIMENSION(:), intent(inout) :: pressure, direction, thickness
  REAL,DIMENSION(:), intent(inout) :: uwind, vwind, psfc, refpres
  real, intent(out) :: lat, lon, elevation
  character(len=14), dimension(:), allocatable :: time_littler
  real,dimension(:), allocatable    :: time
  character(len=100) :: timeunits
  integer, intent(in) :: idx, startindex, countnum
  integer, intent(in) :: device
  character(len=30), dimension(:), intent(in):: variable_name
  character(len=30), dimension(:), intent(in):: variable_mapping
  character(len=30), intent(in) :: filename
  real, intent(out) :: fill_value
  integer, intent(in) :: dimensions
  call log_message('DEBUG', 'Entering subroutine read_variables')
  ! Reading variables
  call log_message('INFO', 'Reading variable: '//variable_name(idx))
  select case (dimensions)
  case (1)  ! dimensions==1
    select case (trim(variable_mapping(idx)))
    case ('temperature')
      CALL readstepnc_single (filename, trim(variable_name(idx)), &
          temperature, fill_value, lon, lat, elevation, startindex, countnum)
      case ('humidity')
        CALL readstepnc_single (filename, variable_name(idx), humidity, &
          fill_value, lon, lat, elevation, startindex, countnum)
      case ('speed')
        CALL readstepnc_single (filename, variable_name(idx), speed, &
          fill_value, lon, lat, elevation, startindex, countnum)
      case ('pressure')
        CALL readstepnc_single (filename, variable_name(idx), pressure, &
          fill_value, lon, lat, elevation, startindex, countnum)
      case ('direction')
        CALL readstepnc_single (filename, variable_name(idx), direction, &
          fill_value, lon, lat, elevation, startindex, countnum)
      case ('uwind')
        CALL readstepnc_single (filename, variable_name(idx), uwind, &
          fill_value, lon, lat, elevation, startindex, countnum)
      case ('vwind')
        CALL readstepnc_single (filename, variable_name(idx), vwind, &
          fill_value, lon, lat, elevation, startindex, countnum)
      case ('height')
        CALL readstepnc_single (filename, variable_name(idx), height, &
          fill_value, lon, lat, elevation, startindex, countnum)
      case ('dew_point')
        CALL readstepnc_single (filename, variable_name(idx), dew_point, &
          fill_value, lon, lat, elevation, startindex, countnum)
      case ('psfc')
        CALL readstepnc_single (filename, variable_name(idx), psfc, &
          fill_value, lon, lat, elevation, startindex, countnum)
      case ('refpres')
        CALL readstepnc_single (filename, variable_name(idx), refpres, &
          fill_value, lon, lat, elevation, startindex, countnum)
    end select
  case (2)  ! dimensions ==2
    select case (trim(variable_mapping(idx)))
    case ('temperature')
      CALL readstepnc (filename, trim(variable_name(idx)), &
        temperature, fill_value, lon, lat, elevation, device, startindex, countnum)
      case ('humidity')
        CALL readstepnc (filename, variable_name(idx), humidity, &
          fill_value, lon, lat, elevation, device, startindex, countnum)
      case ('speed')
        CALL readstepnc (filename, variable_name(idx), speed, &
          fill_value, lon, lat, elevation, device, startindex, countnum)
      case ('pressure')
        CALL readstepnc (filename, variable_name(idx), pressure, &
          fill_value, lon, lat, elevation, device, startindex, countnum)
      case ('direction')
        CALL readstepnc (filename, variable_name(idx), direction, &
          fill_value, lon, lat, elevation, device, startindex, countnum)
      case ('uwind')
        CALL readstepnc (filename, variable_name(idx), uwind, &
          fill_value, lon, lat, elevation, device, startindex, countnum)
      case ('vwind')
        CALL readstepnc (filename, variable_name(idx), vwind, &
          fill_value, lon, lat, elevation, device, startindex, countnum)
      case ('height')
        CALL readstepnc (filename, variable_name(idx), height, &
          fill_value, lon, lat, elevation, device, startindex, countnum)
      case ('dew_point')
        CALL readstepnc (filename, variable_name(idx), dew_point, &
          fill_value, lon, lat, elevation, device, startindex, countnum)
      case ('psfc')
        CALL readstepnc (filename, variable_name(idx), psfc, &
          fill_value, lon, lat, elevation, device, startindex, countnum)
      case ('refpres')
        CALL readstepnc (filename, variable_name(idx), refpres, &
          fill_value, lon, lat, elevation, device, startindex, countnum)
    end select
  case DEFAULT
    STOP 'Dimensions should be either 1 or 2'
  end select
  call log_message('DEBUG', 'Leaving subroutine read_variables')
end subroutine read_variables

end module readncdf
