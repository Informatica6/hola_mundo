program M_Euler

    use funciones
    use algebra_lineal

    real(8)                 :: tiempo,S,R,Z,I,TIZ,T,HKZ,ZKH,D,BR,DTIZ,V,h 
    integer, parameter      :: N=100000
    integer                 :: j


    write(*,*) 'Tiempo que quieres que trascurra'
    read(*,*) tiempo

    open (unit=10, file='parametros.txt')

        read(10,*) 
        read(10,*) S,R,I,Z,V,TIZ,HKZ,ZKH,D,BR,DTIZ

    close(10)

    h=tiempo/(N-0.d0)

    open (unit=10, file='Juego.txt', status='unknown')

        do j=0,N-1 

            call Game(dr,dz,ds,di,S,R,Z,I,TIZ,HKZ,ZKH,D,BR,DTIZ,V,h)

            write(10,*) S,Z,R,I,j*h

        enddo

    close(10)

    call system ('python G_juego.py')

end program M_Euler