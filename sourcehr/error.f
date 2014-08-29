      Subroutine error(MSG)

c    This subroutine checks for frequency of keyboard entry errors
c    Written by Warren Porter 5 April 2007
      implicit none
      
C    integer LINE
      CHARACTER*170 MSG

C    write(*,*)'Error trying to read line: ',LINE
      write(*,*)'Problem reading ',MSG
      write(*,*)'2 common errors: an extra decimal point in a number' 
      write(*,*)'or > 1 blank lines between data groups'
c    pause
      RETURN
      END