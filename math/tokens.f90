module tokens
    implicit none
    private
    public :: tokenize

    ! reset color
    character(len=1), parameter :: esc = achar(27)


    type Token
        character(len=8)  :: kind   ! 'DIGIT', 'IDENT', 'ADD', ...
        character(len=16) :: lexeme ! '5.4', '^'
    end type Token
    
contains
    subroutine tokenize(arr, arr_token, ntoks)
        use utils
        use ansi_colors
        implicit none
        character(len=1), intent(in) :: arr(:)
        character(len=1024), allocatable :: temp(:,:)
        character(len=1024), allocatable, intent(out) :: arr_token(:,:)
        integer :: i, n_tokens, start, j, k, func_len
        integer, intent(inout) :: ntoks
        character(len=32) :: lex, remaining, numm
        ! each function name has a length of 5
        character(len=5), parameter :: funcs(15) =  &
            ['sin  ', 'cos  ', 'tan  ', 'asin ', 'acos ', 'atan ', &
            'sqrt ', 'log  ', 'ln   ', 'exp  ', 'abs  ', 'ceil ', 'floor', 'fact ', 'sign ']
        logical :: found_func

        allocate(arr_token(2, size(arr))) 
        arr_token = '' 
        
        n_tokens = 1
        i = 1
        do while (i <= size(arr))
            select case (arr(i))
                case('+') 
                    arr_token(1, n_tokens)  = arr(i)
                    arr_token(2, n_tokens)  = "ADD"
                    n_tokens = n_tokens + 1
                    i = i + 1
                case('-') 
                    arr_token(1, n_tokens)  = arr(i)
                    arr_token(2, n_tokens)  = "SUB"
                    n_tokens = n_tokens + 1
                    i = i + 1
                case('*')
                    arr_token(1, n_tokens)  = arr(i)
                    arr_token(2, n_tokens)  = "MUL"
                    n_tokens = n_tokens + 1
                    i = i + 1
                case('/')
                    arr_token(1, n_tokens)  = arr(i)
                    arr_token(2, n_tokens)  = "DIV"
                    n_tokens = n_tokens + 1
                    i = i + 1
                case('^')
                    arr_token(1, n_tokens)  = arr(i)
                    arr_token(2, n_tokens)  = "POW"
                    n_tokens = n_tokens + 1
                    i = i + 1
                case('(')
                    arr_token(1, n_tokens)  = arr(i)
                    arr_token(2, n_tokens)  = "LPAREN"
                    n_tokens = n_tokens + 1
                    i = i + 1
                case(')')
                    arr_token(1, n_tokens)  = arr(i)
                    arr_token(2, n_tokens)  = "RPAREN"
                    n_tokens = n_tokens + 1
                    i = i + 1

                case default
                    if (is_numeric(arr(i)) .or. arr(i) == '.') then
                        start = i
                        j = i
                        numm = ''

                        do while (j <= size(arr))
                            if (is_numeric(trim(trim(numm)//trim(arr(j)))) .or. arr(j) == '.') then
                                numm = trim(trim(numm) // trim(arr(j)))
                                j = j + 1
                                print *, "candidate number: ", numm
                            else 
                                print *, "this is the final number: ", numm
                                print *, "saliendo"
                                exit
                            end if
                        end do
                        arr_token(1, n_tokens) = numm
                        arr_token(2, n_tokens) = "NUMERIC"
                        n_tokens = n_tokens + 1
                        i = j
                    else 
                        ! EXAMPLE
                        ! input: sinx
                        ! token: FUNC(sin), IDENT(x)
                        start = i
                        j = i
                        do while (j <= size(arr) .and. .not. is_numeric(arr(j)) .and. &
                                  arr(j) /= '+' .and. arr(j) /= '-' .and. arr(j) /= '*' .and. &
                                  arr(j) /= '/' .and. arr(j) /= '^' .and. arr(j) /= '(' .and. &
                                  arr(j) /= ')' .and. arr(j) /= '.')
                            j = j + 1
                        end do
                        lex = ''
                        do while (start < j)
                            lex = trim(lex) // arr(start)
                            start = start + 1
                        end do


                        found_func = .false.

                        do k = 1, size(funcs)
                            func_len = len_trim(funcs(k))
                            if (len_trim(lex) >= func_len) then
                                if (lex(1:func_len) == trim(funcs(k))) then
                                    ! Found a function at the beginning
                                    arr_token(1, n_tokens) = trim(funcs(k))
                                    arr_token(2, n_tokens) = "FUNC"
                                    n_tokens = n_tokens + 1
                                    found_func = .true.
                                    
                                    ! Check if there's remaining text after function
                                    if (len_trim(lex) > func_len) then
                                        remaining = lex(func_len+1:)
                                        arr_token(1, n_tokens) = trim(remaining)
                                        arr_token(2, n_tokens) = "IDENT"
                                        n_tokens = n_tokens + 1
                                    end if
                                    exit
                                end if
                            end if
                        end do
                        
                        ! If no function found, its just an identifier
                        if (.not. found_func) then
                            arr_token(1, n_tokens) = lex
                            arr_token(2, n_tokens) = "IDENT"
                            n_tokens = n_tokens + 1
                        end if
                        
                        i = j
                    end if
            end select
        end do
        
        ! allocate the temporal array
        allocate(temp(2, n_tokens-1))
        temp(:,:) = arr_token(:,1:n_tokens-1)
        deallocate(arr_token)
        call move_alloc(from=temp, to=arr_token)
        allocate(temp(2, 2*n_tokens))
        i = 1
        n_tokens = 1
        ! GENERAL RULE
        ! NUMERIC followed by IDENT, FUNC, or LPAREN
        ! IDENT or RPAREN followed by IDENT, NUMERIC, FUNC or LPAREN
        ! -> insert MUL
        do while (i <= size(arr_token, 2))
            temp(2,n_tokens) = arr_token(2,i)
            temp(1,n_tokens) = arr_token(1,i)
            n_tokens = n_tokens + 1
            if (i == size(arr_token, 2)) then
                exit
            end if

            if (arr_token(2,i) == "NUMERIC") then
                if (arr_token(2,i+1) == "IDENT" .or. arr_token(2,i+1) == "FUNC" .or. arr_token(2,i+1) == "LPAREN" ) then
                    temp(2,n_tokens) = "MUL"
                    temp(1,n_tokens) = "*"
                    n_tokens = n_tokens + 1
                end if
            end if

            if (arr_token(2,i) == "IDENT" .or. arr_token(2,i) == "RPAREN") then
                if (arr_token(2,i+1) == "IDENT" .or. arr_token(2,i+1) == "NUMERIC" .or. arr_token(2,i+1) == "LPAREN" .or. arr_token(2,i+1) == "FUNC") then
                    temp(2,n_tokens) = "MUL"
                    temp(1,n_tokens) = "*"
                    n_tokens = n_tokens + 1
                end if
            end if
            i = i + 1
        end do

        deallocate(arr_token)
        call move_alloc(from=temp, to=arr_token)
        ntoks = n_tokens
        ! to print the lexems and kinds
        i = 1
        do while (i <= n_tokens)
            write(*,'(A,3X,A,2X,A,2X,A)') WHITE_BOLD, trim(arr_token(1,i)), RED_BOLD, trim(arr_token(2,i)), RESET

            i = i + 1
        end do 

    end subroutine tokenize
end module tokens
