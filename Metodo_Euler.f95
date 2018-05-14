program M_Euler

    use funciones
    use algebra_lineal

    real(8)                 :: S,R,Z,I,TIZ,T,THKZ,ZKH,D,BR,DTIZ,V,IN,NAT,C,h,HKZ,DES
    real(8), allocatable    :: a(:) 
    integer, parameter      :: N=10000
    integer                 :: j,tiempo,k,count
    
    write(*,*) 'Tiempo que quieres que trascurra'
    read(*,*) tiempo

    open (unit=10, file='parametros.txt')

        read(10,*) 
        read(10,*) S,R,I,Z,IN,DES,TIZ,THKZ,ZKH,D,DTIZ,NAT,C,HKZ

    close(10)

    h=tiempo/(N-0.d0)

    open (unit=10, file='Juego.txt', status='unknown')

        do j=0,N-1 

            call Game(dr,dz,ds,di,din,S,R,Z,I,TIZ,THKZ,ZKH,D,DTIZ,V,IN,NAT,C,h,a,tiempo,count,N,HKZ,DES)

            write(10,*) S+IN,Z,R,I,j*h

        enddo

    close(10)

    call system ('python G_juego.py')


    Write(*,*) '///////////////////////////////////////////////////////////'
    write(*,*) '/                                                         /'
    write(*,*) '/      BIENVENIDOS AL SIMULADOR DE UN APOCALIPSIS         /'
    write(*,*) '/                        ZOMBIE                           /'
    write(*,*) '/                                                         /'
    write(*,*) '///////////////////////////////////////////////////////////'

    if(Tiempo==30) then
        write(*,*)  
        write(*,*) 'Bienvenido al modo historia' 
        write(*,*) '(En este modo se relatará una historia acorde con la gráfica)'
        write(*,*) 
        write(*,*) 'Día 0: 15 de mayo de 2018'
        write(*,*) 'La población mundial no es consciente de los acontecimentos que proximamente les tocará vivir'
        write(*,*) 'Se detecta un caso aislado de una nueva enfermedad que parece inofensiva para la raza humana'
        write(*,*)
        write(*,*) '1º Semana'
        write(*,*) 'Esta nueva enfermedad parece muy contagiosa pero sigue sin existir sintamos de riesgo'
        write(*,*)
    endif

end program M_Euler