module utils
    implicit none
    private
    public :: string_to_array, is_numeric

contains
    function string_to_array(str, sep) result(arr)
        implicit none
        character(len=*), intent(in) ::  str
        character(len=*), intent(in) :: sep
        character, allocatable       :: arr(:)
        integer :: i, idx, real_len
        character(len=1) :: char   

        if (len(str) <= 0) then
            allocate(arr(0))
            return
        end if
        
        real_len = 0
        do i = 1, len(str)
            char = str(i:i)
            if (char /= sep) then 
                real_len = real_len + 1
            end if
        end do
        
        allocate(arr(real_len))

        idx =  1
        do i = 1, len(str)
            char = str(i:i)
            if (char /= sep) then
                arr(idx) = str(i:i)
                idx = idx + 1
            end if 
        end do

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