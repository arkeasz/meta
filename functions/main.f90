module root_mod
    use iso_c_binding
contains
    function f(x) result(y) bind(C, name="f")
        real(c_double), value :: x
        real(c_double) :: y
        y = x**2 - 2.0_c_double  
    end function f
end module root_mod
