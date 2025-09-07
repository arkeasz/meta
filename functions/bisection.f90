module bisection_mod
  use iso_c_binding
contains
  subroutine bisection(a, b, tol, root) bind(C, name="bisection")
    real(c_double), value :: a, b, tol
    real(c_double), intent(out) :: root
    real(c_double) :: fa, fb, fm, m

    interface
      function f(x) result(y) bind(C)
        import :: c_double
        real(c_double), value :: x
        real(c_double) :: y
      end function f
    end interface

    fa = f(a)
    fb = f(b)
    if (fa*fb > 0.0_c_double) then
      root = (a + b) / 2.0_c_double
      return
    end if

    do
      m = (a + b) / 2.0_c_double
      fm = f(m)
      if (abs(fm) < tol) then
        root = m
        return
      else if (fa * fm < 0.0_c_double) then
        b = m
        fb = fm
      else
        a = m
        fa = fm
      end if
    end do
  end subroutine bisection
end module bisection_mod
