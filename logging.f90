module logging

implicit none

  contains

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

end module logging