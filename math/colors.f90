module ansi_colors
    implicit none
    private
    public ESC, RED_BOLD, WHITE_BOLD, RESET
    character(len=1), parameter :: ESC         = achar(27)
    character(len=*), parameter :: RED_BOLD    = ESC // "[1;31m"
    character(len=*), parameter :: WHITE_BOLD  = ESC // "[1;37m"
    character(len=*), parameter :: RESET       = ESC // "[0m"
end module ansi_colors