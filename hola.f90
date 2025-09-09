! subroutine getmaxyx(win,y,x) bind(C, name='macro_getmaxyx')
!    use iso_c_binding
!    use ncurses_types
!    type (C_PTR), value :: win
!    integer(C_INT) :: y,x
! end subroutine getmaxyx

! no nos orientamos hacia el infinito, sino el número más grande posible
program hola
    implicit none

    character(10) :: name_function
    integer :: n
    real :: x

    type ascii
        character(3) :: TOP  
        character(3) :: TOP_LEFT  
        character(3) :: TOP_RIGHT  
        character(3) :: RIGHT
        character(3) :: LEFT
        character(3) :: BOTTOM 
        character(3) :: BOTTOM_LEFT 
        character(3) :: BOTTOM_RIGHT
    end type ascii

    type(ascii) :: rays = ascii(
        "---",     ! Línea superior horizontal
        "+",       ! Esquina superior izquierda
        "+",       ! Esquina superior derecha
        "|",       ! Línea derecha vertical
        "|",       ! Línea izquierda vertical
        "---",     ! Línea inferior horizontal
        "+",       ! Esquina inferior izquierda
        "+"        ! Esquina inferior derecha
    )

    print *, "Nombre de la funci\ón"
    read *, name_function

    print *, "En la función ", name_function    
    print *, "Introduce el valor de x: "
    read *, x
    print *, "Introduce el número en terminos de N"
    read *, n


    if ( name_function == "sinh" ) then 
        print *, "Introduce el valor de x: "
        read *, x
        print *, "Introduce el número en terminos de N"
        read *, n

        call sinah(x,n)
    end if

    if ( name_function == "cosh" ) then
        call cosah(x,n)
    end if
    
    print *, "fin del programa"
end program hola

subroutine cosah(x, n)
    implicit none
    ! donde n es el número de terminos
    ! donde i es el valor que va de 0 a n -1 
    ! donde j es el valor para el factorial
    ! donde termin_fact es el termino factorial
    integer :: n, i, j
    ! donde x es la variable de la función
    real :: x, suma, termino, potencia, termin_fact
    
    ! leemos los datos de entrada
    suma = 0.0

    do i = 0, n - 1
        ! calculamos x^(2n+1) en este caso i
        potencia = x**(2*i)
        print *, potencia

        termin_fact = 1.0

        do j = 1, 2*i
            ! termin_fact *= j
            termin_fact = termin_fact*j
        end do

        ! término i-esimo
        termino = potencia/termin_fact

        ! entonces sumamos el termino en la variable suma
        suma = suma + termino
    end do
    ! Resultado
    print *, "sinha(", x, ") =", suma    
    
end subroutine cosah


subroutine sinah(x, n)
    implicit none
    ! donde n es el número de terminos
    ! donde i es el valor que va de 0 a n -1 
    ! donde j es el valor para el factorial
    ! donde termin_fact es el termino factorial
    integer :: n, i, j
    ! donde x es la variable de la función
    real :: x, suma, termino, potencia, termin_fact
    
    ! leemos los datos de entrada
    suma = 0.0

    do i = 0, n - 1
        ! calculamos x^(2n+1) en este caso i
        potencia = x**(2*i + 1)
        print *, potencia

        termin_fact = 1.0

        do j = 1, 2*i + 1
            ! termin_fact *= j
            termin_fact = termin_fact*j
        end do

        ! término i-esimo
        termino = potencia/termin_fact

        ! entonces sumamos el termino en la variable suma
        suma = suma + termino
    end do
    ! Resultado
    print *, "sinha(", x, ") =", suma    

    
end subroutine sinah