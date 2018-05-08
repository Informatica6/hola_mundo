program M_Euler

    use funciones
    use algebra_lineal

    real(8)                 :: a,b,x,y
    real(8)                 :: h
    integer                 :: i,N 


    write(*,*) 'Escriba el intervalo'
    read(*,*) a,b 

    write(*,*) 'Escriba el numero de particiones'
    read(*,*) N 

    write(*,*) 'Escriba el valor inicial'
    read(*,*) Y

    h=(b-a)/(N-0.d0)

    open (unit=10, file='puntosE.txt', status='unknown')

        do i=0,N-1

            x=a+i*h 

            call Euler(fs1,x,y,h)

            write(10,*) x, y

        enddo

    close(10)

    call system ('python Grafica_E.py')

end program M_Euler