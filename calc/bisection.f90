program biseccion 
    implicit none
    
    real :: resp

    
    do while ( resp > 0.0 )
        print *, ""
        print *, "INGRESO DE DATOS"
        print *, "****************"
        real :: A, C, err
        print *, "INGRESE A (LIMITE INFERIOR):"
        read *, A
        print *, "INGRESE A (LIMITE SUPERIOR):"
        read *, C
        print *, "INGRESE EL ERROR SOLICITADO (%):"
        read *, err

        

    end do
        
end program biseccion