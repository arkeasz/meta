program fixedpoint
    use class_parser
    use ast
    use evaluator
    use symtab
    use math
    use, intrinsic :: iso_fortran_env, only: dw => real64

implicit none
    real(kind=dw):: result,x, res
    character(len=64) :: eq
    ! character(len=*) :: variables
    type(Parser) :: p
    type(ASTNode), pointer :: root
    type(Vars), pointer :: v 
    integer :: N, i

    N = 50
    i = 0
    result = 0
    x = 0
    eq = "(0.5-x)^(1/3)"

    res = differentiation(eq, 3.0_dw)
    print *, res
    p = new_parser(trim(eq))
    v => new_symtab()
    call tokenizer(p)
    call parse(root, p)

    call symtab_add(v, 'x', 0.1_dw)
    print *, "this is the equation", eq
    do while ( i < N )
        if (associated(root)) then
            x = eval(root, v)
            print *, x
            call symtab_add(v, 'x', x)
    
        else
            print *, 'No AST produced (root is null).'
        end if
        i = i + 1
    end do

    result = x
    print *, result
    
end program fixedpoint
