module utils
    use, intrinsic :: iso_fortran_env, only : dw => real64
    use, intrinsic :: ieee_arithmetic
    implicit none
    private
    public :: string_to_array, is_numeric, is_even

contains
    function string_to_array(str, sep) result(arr)
        implicit none
        character(len=*), intent(in) :: str
        character(len=*), intent(in) :: sep     
        character(len=:), allocatable :: arr(:)
        integer :: i, start, len_str, n
        character(len=:), allocatable :: temp

        len_str = len_trim(str)
        temp = trim(str)

        if (len_trim(sep) == 0) then
            allocate(character(len=1) :: arr(len_str))
            do i = 1, len_str
                arr(i) = temp(i:i)
            end do
            return
        end if

        if (len(sep) /= 1) stop "ERROR: string_to_array only give one"

        n = 1
        do i = 1, len_str
            if (temp(i:i) == sep) n = n + 1
        end do

        allocate(character(len=len_str) :: arr(n))

        start = 1
        n = 0
        do i = 1, len_str
            if (temp(i:i) == sep) then
                n = n + 1
                arr(n) = adjustl(trim(temp(start:i-1)))
                start = i + 1
            endif
        end do

        n = n + 1
        arr(n) = adjustl(trim(temp(start:)))

        arr = arr(1:n)
    end function string_to_array

    function is_numeric(str) result(ok)
        implicit none
        character(len=*), intent(in) :: str
        logical :: ok
        integer :: i, n_dots, len_s
        character(len=1) :: ch

        ok = .false.
        if (len_trim(str) == 0) return
        len_s = len_trim(str)
        n_dots = 0

        do i = 1, len_s
            ch = str(i:i)
            select case (ch)
            case ('0':'9')
                cycle
            case ('.')
                n_dots = n_dots + 1
                if (n_dots > 1) then
                    ok = .false.
                    return
                end if
                cycle
            case ('+', '-')
                if (i /= 1) then
                    ok = .false.
                    return
                else
                    cycle
                end if
            case default
                ok = .false.
                return
            end select
        end do

        if (len_s == 1 .and. (str(1:1) == '+' .or. str(1:1) == '-')) then
            ok = .false.
            return
        end if

        ok = .true.
    end function is_numeric

    function is_even(n) result(ok)
        implicit none
        real(kind=dw), intent(in) :: n
        logical :: ok
        ok = .false.
        if (mod(n,2.0_dw) == 0.0_dw) then
            ok = .true.
        end if
    end function is_even
end module utils