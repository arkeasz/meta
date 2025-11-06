module math 

    use, intrinsic :: iso_fortran_env, only : dw => real64
    implicit none
    private
    public :: factorial 
contains

    recursive function factorial(n) result(res)
        real(kind=dw), intent(in) :: n
        real(kind=dw) :: res
        
        if (n <= 1.0_dw) then 
            res = 1.0_dw
        else 
            res = n*factorial(n - 1.0_dw) 
        end if
    end function factorial
end module math 