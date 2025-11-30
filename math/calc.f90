module calc
    use iso_c_binding
    use calculators
contains
subroutine calcu(eq, variables, result) bind(C, name="calculator")
    use iso_c_binding
    implicit none
    real(c_double), intent(out) :: result
    character(kind=c_char), intent(in) :: eq(*), variables(*)
    integer :: n_eq, n_vars, i
    character(len=:), allocatable :: eq_f, vars_f

    
    n_eq = 0
    do while (eq(n_eq+1) /= c_null_char)
        n_eq = n_eq + 1
        if (n_eq > 1000) then
            print *, "Error: eq string not end up"
            return
        end if
    end do
    print *, "eq length detected:", n_eq

    n_vars = 0
    do while (variables(n_vars+1) /= c_null_char)
        n_vars = n_vars + 1
        if (n_vars > 1000) then
            print *, "Error: var string not end up"
            return
        end if
    end do
    print *, "vars length detected:", n_vars

    allocate(character(len=n_eq) :: eq_f)
    allocate(character(len=n_vars) :: vars_f)

    eq_f = transfer(eq(1:n_eq), eq_f)
    vars_f = transfer(variables(1:n_vars), vars_f)

    print *, "eq_f = '", trim(eq_f), "'"
    print *, "vars_f = '", trim(vars_f), "'"

    call calculate(eq_f, vars_f, result)
end subroutine calcu


end module calc
