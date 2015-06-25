module check_status

implicit none

  contains

  subroutine check(status)
    implicit none
    integer, intent(in):: status
    if(status /= 0) then
      write (*,*) status
    end if
  end subroutine check

  end module check_status