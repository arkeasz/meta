module math 
    use class_parser
    use ast
    use evaluator
    use symtab
    use ansi_colors
    use, intrinsic :: iso_fortran_env, only : dw => real64
    implicit none
    private
    public :: factorial , differentiation, sgn
contains
    ! f'(x)≈ (f(x+h) - f(x-h))/2h
    ! where h = 0.01
    function differentiation(eq, x) result(res)
        real(kind=dw):: res, f_add, f_min
        real(kind=dw), intent(in):: x
        character(len=*), intent(in) :: eq
        type(Parser) :: p
        type(ASTNode), pointer :: root
        type(Vars), pointer :: v 
        integer :: h, i

        h = 0.01_dw
        i = 0
        p = new_parser(trim(eq))
        v => new_symtab()
        call tokenizer(p)
        call parse(root, p)

        call symtab_add(v, 'x', x+h)

        if (associated(root)) then
            f_add = eval(root, v)
        else
            print *, ESC // RED_BOLD // 'No AST produced (root is null).'
        end if

        call symtab_add(v, 'x', x-h)

        if (associated(root)) then
            f_min = eval(root, v)
        else
            print *, ESC // RED_BOLD // 'No AST produced (root is null).'
        end if

        res = (f_add - f_min)/(2*h)
        print *, "the value of differentiation", res

    end function differentiation

    function sgn(n) result(res)
        real(kind=dw), intent(in) :: n
        real(kind=dw) :: res

        if (n > 0) then
            res = 1
        else if (n < 0) then 
            res = -1
        else 
            res = 0
        end if
    end function sgn

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