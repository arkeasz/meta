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
        character, allocatable :: tok(:,:)
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
        type(Parser), intent(inout) :: this
        integer :: i
        character, allocatable :: arr(:)
        character(len= 32), allocatable ::arr_token(:,:)


        arr = string_to_array(this%equation, ' ')

        call tokenize(arr, arr_token)
        this%tok = arr_token
    end subroutine tokenizer

    ! unary operators acts on one operand, while a binary operator acts on two operands
    ! Parentheses thave precedence over all operators
    ! ^ (exponentiation) has precedence over unary - and the binary operators "/, *, - and +"
    ! * and / have precedence over unary - and binary - and +
    ! unary - has precedence over binary - and +
    ! ^ is right associative while all other binary operators are left associative.
    function parse(tokens)

    end function parse
end module class_parser

