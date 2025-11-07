module calculators
    use class_parser
    use ast
    use evaluator
    use symtab
    use, intrinsic :: iso_fortran_env, only: dw => real64
    implicit none
    private

    public :: calculate
contains
    subroutine calculate(eq, variables, result)
        real(kind=dw), intent(out) :: result
        character(len=*), intent(in) :: eq
        character(len=*), intent(in) :: variables
        type(Parser) :: p
        type(ASTNode), pointer :: root
        type(Vars), pointer :: v 

        p = new_parser(eq)
        v => new_symtab()
        call tokenizer(p)
        call parse(root, p)

        ! call symtab_add(v, 'y', 1.0_dw)
        ! call symtab_add(v, 'x', 4.5_dw)
        call symtab_from_string(v, variables)
        call symtab_print(v)
        if (associated(root)) then
            result = eval(root, v)
            ! call print_tree(root)
        else
            print *, 'No AST produced (root is null).'
        end if
    end subroutine calculate

end module calculators
