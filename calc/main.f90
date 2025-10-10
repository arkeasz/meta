program calculator
    use class_parser
    use ast
    implicit none
    
    type(Parser) :: p
    type(ASTNode), pointer :: root
    ! p = new_parser("x^2 + 3*x - 5 + 2sin4y")
    ! call tokenizer(p)
    ! p = new_parser("0.5x^2 + 3*x - 5 + sin(x)")
    ! call tokenizer(p)
    ! p = new_parser("4.5x^2 + 3*x - 5 + sinx")
    ! call tokenizer(p)

    ! first only numbers
    p = new_parser("4+3*5^2+6")
    call tokenizer(p)
    call parse(root, p)
    if (associated(root)) then
        call print_tree(root)
    else
        print *, 'No AST produced (root is null).'
    end if

end program calculator
