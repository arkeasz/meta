module class_parser
    implicit none
    private
    public :: Parser, print_eq, new_parser, tokenizer

    type Parser
        character(:), allocatable :: equation
    end type Parser
 
contains

    function new_parser(temp) result(p)
        implicit none
        character(len=*), intent(in) :: temp
        type(Parser) :: p

        allocate(character(len(trim(temp))):: p%equation)
        p%equation = trim(temp)

    end function new_parser

    subroutine print_eq(this)
        implicit none
        type(Parser), intent(in) :: this
        print *, "Equation: ", this%equation

    end subroutine print_eq

    subroutine tokenizer(this)
        use utils
        use tokens
        implicit none
        type(Parser), intent(in) :: this
        integer :: i
        character, allocatable :: arr(:)

        arr = string_to_array(this%equation, ' ')

        print *, arr

        call print_token(arr)

    end subroutine
end module class_parser

