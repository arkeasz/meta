program example
    implicit none

    real :: example, a, c
    print *, "el ejemplo"

    interface
      function f(x) result(y) bind(C)
        real :: x, y
      end function f
    end interface

    read  *, example
    print *, example

end program example


! module root_mod
!   use iso_c_binding
! contains
!   function f(x) result(y) bind(C, name="f")
!     real(c_double), value :: x
!     real(c_double) :: y
!     y = x**2 - 2.0_c_double  
!   end function f
! end module root_mod
