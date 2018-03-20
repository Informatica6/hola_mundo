program main

    use Derivadas
    
    real(8), allocatable            :: Y(:),X(:)
    Real(8)                         :: h
    integer                         :: a,b
    
    Write(*,*) 'Intervalo que quieres'
    read(*,*) a,b
    
    Write(*,*) 'Ax' 
    Read(*,*) h
    
    allocate(Y(a:b),X(a:b))
    
    do i=a,b
        X(i)=i
        Y(i) = F(X(i)-0.d0)  
    enddo
    
    open(unit=10, file='derivada_progresiva.txt', status='unknown') 
    
        do i=a,b
            write(10,*) derivada_progresiva(Y(i),X(i),h)
        enddo
            
    close(10)
    
    open(unit=10, file='derivada_regresiva.txt', status='unknown') 
    
        do i=a,b
            write(10,*) derivada_regresiva(Y(i),X(i),h)
        enddo
            
    close(10)
    
    open(unit=10, file='derivada_centrada.txt', status='unknown') 
    
        do i=a,b
            write(10,*) derivada_centrada(Y(i),X(i),h)
        enddo
    
    close(10)
    
    end program main
    