module symtab 
    use, intrinsic :: iso_fortran_env, only: dw => real64
    implicit none
    private

    public :: Vars, new_symtab, symtab_add, symtab_lookup, symtab_print
    type Vars
        character(len=32), allocatable :: nam(:)
        real(kind=dw), allocatable    :: value(:)
    end type Vars

contains
    function new_symtab() result(v)
        implicit none
        ! we use a pointer because we want the object to be modifiable
        type(Vars), pointer :: v
        allocate(v)
        allocate(v%nam(0))
        allocate(v%value(0))
    end function new_symtab
 
    ! INSERT
    subroutine symtab_add(v, ident, val)
        implicit none
        type(Vars), intent(inout) :: v
        character(len=*), intent(in) :: ident
        real(kind=dw), intent(in) :: val

        integer :: i, n
        logical :: found
        character(len=32), allocatable :: tmpnam(:)
        real(kind=dw), allocatable :: tmpval(:)

        n = size(v%nam)
        found = .false.

        do i = 1, n
            if (trim(v%nam(i)) == trim(ident)) then
                v%value(i) = val
                found = .true.
                exit
            end if
        end do

        if (.not. found) then
            allocate(tmpnam(n+1))
            allocate(tmpval(n+1))
            if (n > 0) then
                tmpnam(1:n) = v%nam
                tmpval(1:n) = v%value
            end if
            tmpnam(n+1) = adjustl(trim(ident))
            tmpval(n+1) = val
            call move_alloc(tmpnam, v%nam)
            call move_alloc(tmpval, v%value)
        end if
    end subroutine symtab_add

    ! SEARCH
    function symtab_lookup(v, ident) result(value)
        implicit none
        character(len=*), intent(in) :: ident
        type(Vars), intent(in) :: v
        integer :: i, sizev
        real(kind=dw) :: value
        sizev = size(v%nam)
        value = 0.0_dw
        do i = 1, sizev
            if (trim(ident) == trim(v%nam(i))) then
                value = v%value(i)
                exit
            end if 
        end do 
    end function symtab_lookup
    
    ! DELETE

    ! DEBUG
    subroutine symtab_print(v)
        implicit none
        type(Vars), intent(in) :: v
        integer :: i
        write(*,*) "Symbol table contents:"
        do i = 1, size(v%nam)
            write(*,'(A,1X,F12.6)') trim(v%nam(i)//" ="), v%value(i)
        end do
    end subroutine symtab_print

end module symtab