program main

    use Derivadas
    
    real(8), allocatable            :: Y(:),X(:)
    Real(8)                         :: h,Ax
    integer                         :: a,b
    Integer, parameter              :: n=10000 !Termino para determinar Ax

    Write(*,*) 'Bienvenido al cálculo de derivadas '
    
    Write(*,*) 'Intervalo que quieres'
    read(*,*) a,b 
    
    Write(*,*) 'Precision de la derivada' 
    Read(*,*) h
    
    Ax= (b-a)/(n-0.d0) !Distancia entre x(i) y x(i+1)

    allocate(Y(n),X(n))
    
    do i=1,n
        X(i)=a+i*Ax
        Y(i) = F(X(i)-0.d0) 
    enddo
        
    open(unit=10, file='Puntos.txt', status='unknown')

    do i=1,n
        write(10,*) x(i), Y(i)
    enddo

    close(10)

    open(unit=10, file='derivada_progresiva.txt', status='unknown') 
    
        do i=1,n
            write(10,*) x(i), derivada_progresiva(Y(i),X(i),h)
        enddo
            
    close(10)
    
    open(unit=10, file='derivada_regresiva.txt', status='unknown') 
    
        do i=1,n
            write(10,*) x(i), derivada_regresiva(Y(i),X(i),h)
        enddo
            
    close(10)
    
    open(unit=10, file='derivada_centrada.txt', status='unknown') 
    
        do i=1,n
            write(10,*) x(i), derivada_centrada(Y(i),X(i),h)
        enddo
    
    close(10)
    
end program main
    