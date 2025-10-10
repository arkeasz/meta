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
    public :: Parser, print_eq, new_parser, tokenizer, parse

    type Parser
        character(:), allocatable :: equation
        character(len=32), allocatable :: tok(:,:)
        integer :: pos   = 1
        integer :: ntoks = 0
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
        use ast
        implicit none
        type(Parser), intent(inout) :: this
        character, allocatable :: arr(:)
        character(len= 32), allocatable ::arr_token(:,:)


        arr = string_to_array(this%equation, ' ')

        call tokenize(arr, arr_token)
        ! output of tokenize when the input will 4+3*5^2+6.5
        !  Index  Lexeme   Kind
        !    1       4       N
        !    2       +       A
        !    3       3       N
        !    4       *       M
        !    5       5       N
        !    6       ^       P
        !    7       2       N
        !    8       +       A
        !    9     6.5       N
        this%tok = arr_token
        this%ntoks = size(arr_token, 2)
        this%pos = 1
    end subroutine tokenizer

    ! unary operators acts on one operand, while a binary operator acts on two operands
    ! Parentheses thave precedence over all operators
    ! ^ (exponentiation) has precedence over unary - and the binary operators "/, *, - and +"
    ! * and / have precedence over unary - and binary - and +
    ! unary - has precedence over binary - and +
    ! ^ is right associative while all other binary operators are left associative.
    subroutine parse(root, p)
        use ast
        implicit none
        ! wherea
        ! tok(1,i) = lexemes
        ! tok(2,i) = kind

        ! the main value
        type(Parser), intent(inout) :: p;
        type(ASTNode), pointer, intent(out):: root
        p%pos = 1
        root => parse_E(p)
        if (trim(get_kind(p)) /= 'END') then
            print *, 'Warning: tokens remaining after parse at pos=', p%pos, ' kind=', trim(get_kind(p))
        end if
    end subroutine parse

    ! E -> T {("+" | "-") T}
    function parse_E(p) result(node)
        use ast
        implicit none
        type(Parser), intent(inout) :: p
        type(ASTNode), pointer :: node, left, right
        character(len=128) :: lexem, kind   
        nullify(node)
        left => parse_T(p) ! to parse the term in the left hand
          if (.not. associated(left)) then
            print *, "parse_E: error - left term is null at pos=", p%pos
            return
        end if
        do
            kind = trim(get_kind(p))
            lexem = trim(get_lexem(p))
            
            if (kind == "ADD" .or. kind == "SUB" ) then
                call advance(p) ! p%pos + 1 
                right => parse_T(p) ! 4+3*5^2+6
                node => make_binop(lexem, left, right)
                left => node
            else 
                exit
            end if
        end do

        if (.not. associated(node)) node => left
    end function parse_E

    ! T -> F {("*" | "/") F}
    recursive function parse_T(p) result(node)
        use ast
        implicit none
        type(Parser), intent(inout) :: p
        type(ASTNode), pointer :: node, left, right
        character(len=128) :: lexem, kind   
        
        left => parse_F(p)
        node => left

        do 
            kind = trim(get_kind(p))
            lexem = trim(get_lexem(p))

            if (kind == "MUL" .or. kind == "DIV") then
                call advance(p)
                right => parse_F(p)
                node => make_binop(lexem, left, right)
                left => node
            else 
                exit
            end if
        end do
        if (.not. associated(node)) node => left
    end function parse_T

    ! F -> P ["^" F]
    recursive function parse_F(p) result(node)
        use ast
        implicit none
        type(Parser), intent(inout) :: p
        type(ASTNode), pointer :: node, left, right
        character(len=128) :: lexem, kind   

        left => parse_P(p)
        kind = trim(get_kind(p))
        lexem = trim(get_lexem(p))
        if (kind == "POW") then
            call advance(p)
            right => parse_F(p)
            node => make_binop(lexem, left, right)
        else 
            node => left
        end if

        
    end function parse_F

    ! P -> v | "(" E ")" | "-" T
    recursive function parse_P(p) result(node)
        use ast
        implicit none
        type(Parser), intent(inout) :: p
        type(ASTNode), pointer :: node
        character(len=128) :: lexem, kind   

        kind = get_kind(p)
        lexem = get_lexem(p)
        if (kind == "NUMERIC") then
            node => make_num(lexem)
            call advance(p)
        else if (kind == "IDENT") then
            node => make_ident(lexem)
            call advance(p)
        else if (kind == "LPAREN") then
            call advance(p)
            node => parse_E(p)

            if (.not. (trim(get_kind(p)) == "RPAREN")) then
                print *, "Error: expected ) at pos=", p%pos
            else 
                call advance(p)
            end if
        else if (kind == "SUB") then
            call advance(p)
            node => parse_F(p)
            node => make_unop('neg', node)
        else
            print *, 'Parse error in P: unexpected token kind=', kind, ' lex=', lexem, ' pos=', p%pos
            nullify(node)
        end if       
    end function parse_P

    ! to ireate the tokens without innecesary loops
    subroutine advance(p)
        type(Parser), intent(inout) :: p
        if (p%pos <= p%ntoks) p%pos = p%pos + 1
    end subroutine advance

    ! to get the kind of token
    recursive function get_kind(p) result(kind)
        type(Parser), intent(in) :: p
        character(len=32) :: kind
        if (p%pos > p%ntoks) then 
            kind = "END"
        else 
            kind = adjustl(p%tok(2, p%pos))
        end if
    end function get_kind

    ! to get the lexema of the token
    recursive function get_lexem(p) result(lexem)
        type(Parser), intent(in) :: p
        character(len=128) :: lexem
        if (p%pos > p%ntoks) then 
            lexem = ""
        else
            lexem = adjustl(p%tok(1, p%pos))
        end if
    end function get_lexem
end module class_parser

