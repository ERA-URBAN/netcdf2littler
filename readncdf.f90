module readncdf

 contains

subroutine readstepnc(fname,var_name,ff, fill_value, lon, lat)
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
  
  implicit none
  
  ! declare calling variables
  character(len=*),intent(in) :: fname, var_name
  real,dimension(:),intent(out) :: ff
  real,intent(out) :: lon, lat, fill_value
  ! declare local variables
  integer :: nc_id,var_id,ndim,nvar,nattr,unlim_id,fmt, &
             ii,status,lo,la,le,ld,ti, device, dlength, charset, &
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
        call check(nf90_get_var(nc_id, var_id, lon))
      case ('lat','LAT','Lat','Latitude','latitude','LATITUDE')
        call check(nf90_inq_varid(nc_id,trim(varname),var_id))
        call check(nf90_get_var(nc_id, var_id, lat))
    end select
  end do
  ! allocate the matrix for reading data. The definition is
  allocate(var_dummy(ti))
  ! Read all data
  call check(nf90_inq_varid(nc_id,trim(var_name),var_id))
  call check(nf90_get_var(nc_id,var_id,var_dummy, &
             start=(/1,1/), count=(/1,ti/)))
  ! asking if there are the scale_factor and add_offset attributes
  status = nf90_get_att(nc_id,var_id,"scale_factor",sf)
  if (status == -43) sf=1.0
  status = nf90_get_att(nc_id,var_id,"add_offset",ofs)
  if (status == -43) ofs = 0.0
  ff = sf*var_dummy+ofs
  status = nf90_get_att(nc_id,var_id,"_FillValue",fill_value)
  call check(nf90_close(nc_id))
  deallocate(var_dummy)
end subroutine readstepnc


subroutine readtimedim(fname, time, timeunits)
  ! read the time dimension of a netcdf file
  ! in:     - fname: netcdf filename
  ! out:    - ti : length of time axis
  !         - time_littler: time in LITTLE_R format
  use netcdf
  use check_status
  implicit none
  ! declare calling variables
  character(len=*),intent(in) :: fname
  real,dimension(:),intent(out), allocatable     :: time
  character(len=100), intent(out) :: timeunits
  integer :: ti
  ! declare local variables
  integer :: nc_id,ndim,nvar, dlength, var_id, &
             ii,lo,la,le,ld, device
  character(len=15) :: dname, varname
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
        allocate(time(ti))
        !allocate(time_littler(ti))
        ! get time/timeunits from netcdf
        call check(nf90_inq_varid(nc_id,TRIM(varname),var_id))
        call check(nf90_get_var(nc_id, var_id, time))
        call check(nf90_get_att(nc_id, var_id, 'units', timeunits))
    end select
  end do

end subroutine readtimedim

end module readncdf