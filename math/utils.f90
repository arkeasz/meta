module utils
    implicit none
    private
    public :: string_to_array, is_numeric

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
            elseif (i == len_str) then
                n = n + 1
                arr(n) = adjustl(trim(temp(start:i)))
            end if
        end do

        if (n < size(arr)) arr = arr(1:n)
    end function string_to_array


    function is_numeric(str) result(bool)
        implicit none
        character(len=*), intent(in) :: str
        logical :: bool
        real :: real_value
        integer :: io_status

        bool = .false.

        read(str, *, iostat = io_status) real_value

        if (io_status == 0) then
            bool = .true.
        end if
    end function is_numeric

end module utils