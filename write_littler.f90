module write_littler

implicit none

  contains

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
  integer :: kx, k, iunit
  real,dimension(kx),intent(in) :: p,z,t,td,spd,dir,u,v,rh,thick
  real, intent(in) :: xlon, xlat, slp, ter
  integer,dimension(kx),intent(in) :: p_qc,z_qc,t_qc,td_qc,spd_qc
  integer, dimension(kx), intent(in) :: dir_qc,u_qc,v_qc,rh_qc,thick_qc
  integer :: iseq_num
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

end module write_littler