program calculator
    use class_parser
    use ast
    use evaluator
    use, intrinsic :: iso_fortran_env, only: dw => real64
    implicit none
    real(kind=dw) :: result
    character(len=32) :: eq
    type(Parser) :: p
    type(ASTNode), pointer :: root

    eq = "fact(0)"
    p = new_parser(eq)
    call tokenizer(p)
    call parse(root, p)

    if (associated(root)) then
        result = eval(root)
        print *, "Equation: ", eq
        print *, 'Result =', result
        call print_tree(root)
    else
        print *, 'No AST produced (root is null).'
    end if

end program calculator
