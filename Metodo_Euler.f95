program M_Euler

    use funciones
    use algebra_lineal

    real(8)                 :: S,R,Z,I,TIZ,T,HKZ,ZKH,D,BR,DTIZ,V,IN,NAT,C,h 
    real(8), allocatable    :: a(:) 
    integer, parameter      :: N=100000
    integer                 :: j,w,tiempo,k,count
    
    write(*,*) 'Tiempo que quieres que trascurra'
    read(*,*) tiempo

    open (unit=10, file='parametros.txt')

        read(10,*) 
        read(10,*) S,R,I,Z,V,TIZ,HKZ,ZKH,D,BR,DTIZ,NAT,C

    close(10)

    IN=0
    count=0

    allocate(A(tiempo))

    !do k = 1,tiempo
    !    a(k)=rand(w)    
    !enddo

    !write(*,*) a

    h=tiempo/(N-0.d0)

    open (unit=10, file='Juego.txt', status='unknown')

        do j=0,N-1 

            call Game(dr,dz,ds,di,din,S,R,Z,I,TIZ,HKZ,ZKH,D,BR,DTIZ,V,IN,NAT,C,h,a,tiempo,count,N)

            write(10,*) S+IN,Z,R,I,j*h

        enddo

    close(10)

    call system ('python G_juego.py')

end program M_Euler