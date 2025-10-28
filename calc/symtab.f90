module symtab 
    use, intrinsic :: iso_fortran_env, only: dw => real64
    type Vars
        character(len=2) :: var
        real(kind=dw)    :: value
    end type Vars
contains
    function new_symtab() result(p)
        implicit none
        type(Vars) :: v
        
    end function new_symtab

    ! function new_parser(temp) result(p)
    !     implicit none
    !     character(len=*), intent(in) :: temp
    !     type(Parser) :: p

    !     allocate(character(len(trim(temp))):: p%equation)
    !     p%equation = trim(temp)

    ! end function new_parser

end module symtab