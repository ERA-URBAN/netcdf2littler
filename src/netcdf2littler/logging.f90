module logging

implicit none

  contains

  subroutine define_logfile(logfile)
    ! define logging file
    character(len=*), intent(in) :: logfile
    integer :: logunit
    logunit=99
    open(unit=logunit,file=logfile, form='formatted')
  end subroutine define_logfile


  subroutine current_datetime(datetime)
    ! set datetime to current date and time
    character(19), intent(out) :: datetime
    integer,dimension(8) :: values
    call date_and_time(VALUES=values)
    write ( datetime, 1000 )  values(3), values(2), values(1), values(5), &
    & values(6), values(7)
    1000 format (i2.2, '/', i2.2, '/', i4.4, ';', i2.2, ':', i2.2, ':', i2.2)
  end subroutine current_datetime


  subroutine log_message(logtype, logmessage)
    ! log message with current datetime, logtype and logmessage
    character(len=*), intent(in) :: logtype
    character(len=*), intent(in) :: logmessage
    character(19) :: datetime
    call current_datetime(datetime)
    write(99, '(A, T22, A,T31,A)') datetime, logtype, logmessage
  end subroutine log_message


  pure character(len=99) function concat_str_int(string, num_i)
    ! concatenate string and integer
    character(len=*), intent(in) :: string
    integer, intent(in) :: num_i
    character(len=99) :: char_num
    write(char_num, *) num_i
    ! concatenate string and integer
    concat_str_int = adjustl(string)//trim(adjustl(char_num))
    return
  end function concat_str_int


  pure character(len=99) function concat_str_real(string, num_i)
    ! concatenate string and real
    character(len=*), intent(in) :: string
    real, intent(in) :: num_i
    character(len=99) :: char_num
    write(char_num, *) num_i
    ! concatenate string and integer
    concat_str_real = adjustl(string)//trim(adjustl(char_num))
    return
  end function concat_str_real

end module logging
