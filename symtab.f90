module symtab 
    use, intrinsic :: iso_fortran_env, only: dw => real64
    
    type Vars
        character(len=2) :: nam(:)
        real(kind=dw)    :: value(:)
    end type Vars

    type IdentTyoe
        
    end type IdentTyoe


contains
    function new_symtab() result(v)
        implicit none
        ! we use a pointer because we want the object to be modifiable
        type(Vars), pointer :: v
        allocatable(v)
        allocate(p&nam(0), p%value(0))
    end function new_symtab
 
    ! INSERT
    subroutine add_ident(v, ident, value) 
        implicit none
        type(Vars), intent(inout) :: v
        character(len=2), intent(in) :: ident
        real(kind=dw), intent(in) :: ident

        integer :: i, current_size
        logical :: found
        
        found = .false.
        current_size = len(v%nam)

        ! MANAGE COLISIONS
        do i = 1, current_size
            if (trim(ident) == v%name(i)) then
                v%value = value
                found = .true.
                exit
            end if 
        end do

        if (.not. found) then 

            ! tenoirarky array
            type(Vars) :: temp_v
            
            allocate(temp_v%nam(current_size + 1), temp_v%value(current_size + 1))
            
            if (current_size > 0) then
                temp_v%nam(1:current_size) = v%nam
                temp_v%value(1:current_size) = v%value
            end if

            temp_v%nam(current_size + 1) = ident
            temp_v%value(current_size + 1) = value

            call move_alloc(from=temp_v%nam, to=v%nam)
            call move_alloc(from=temp_v%value, to=v%value)
        end if
    end subroutine add_ident

    ! SEARCH
    function search_ident(v, ident) result(ident, value)
    

    ! DELETE

end module symtab