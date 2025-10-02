module class_parser
    ! Input: 4x  + 3
    ! Note: 4x has an implicit multiplication
    ! Tokens:  
    !  -  4 (NUMERIC)
    !  -  x (IDENTIFIER)
    !  -  + (ADD)
    !  -  3 (NUMERIC)
    ! so the parser build:
    !         +
    !        / \
    !       *  3
    !      / \
    !     4  x
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
        character, allocatable :: arr(:), arr_token(:,:)

        arr = string_to_array(this%equation, ' ')

        print *, arr

        call tokenize(arr, arr_token)
    end subroutine tokenizer
end module class_parser

