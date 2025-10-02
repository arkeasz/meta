module tokens
    implicit none
    private
    public :: print_token

    type Token
        character(len=8) :: kind ! 'DIGIT', 'IDENT', 'ADD', ...
    end type Token
    
contains
    subroutine expressions(arr) result(exprs)
        use utils
        implicit none
        character(len=1), intent(in) :: arr(:)
        character, allocatable :: expres(:,:)
        integer :: i
        real ::  realnum


        do i = 1, size(arr)
            select case (arr(i))
                case('+')
                    write(*,'(I3, 3X, A, 3X, A)') i, arr(i), "ADD"
                case('-') 
                    write(*,'(I3, 3X, A, 3X, A)') i, arr(i), "SUB"
                case('*')
                    write(*, '(I3, 3X, A, 3X, A)') i, arr(i), "MUL"
                case('/')
                    write(*, '(I3, 3X, A, 3X, A)') i, arr(i), "DIV"
                case('.')
                    write(*, '(I3, 3X, A, 3X, A)') i, arr(i), "DFLOAT"
                case('^')
                    write(*, '(I3, 3X, A, 3X, A)') i, arr(i), "POW"
                case('(')
                    write(*, '(I3, 3X, A, 3X, A)') i, arr(i), "LPAREN"
                case(')')
                    write(*, '(I3, 3X, A, 3X, A)') i, arr(i), "RPAREN"

                case default
                    if (is_numeric(arr(i))) then
                        write(*, '(I3, 3X, A, 3X, A)') i, arr(i), "NUMBER"
                    else
                        write(*, '(I3, 3X, A, 3X, A)') i, arr(i), "IDENT"
                    end if
            end select
        end do
    end subroutine expressions
end module tokens