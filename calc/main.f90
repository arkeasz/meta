
module class_parser
    implicit none
    private
    public :: Parser, print_eq, new_parser

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
end module class_parser


program calculator
    use class_parser
    implicit none
    
    type(Parser) :: p

    p = new_parser("x^2 + 3*x - 5")

    call print_eq(p)
end program calculator
