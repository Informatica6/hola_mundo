program Practica
    
    use algebra_lineal
    
    Real(8)                 :: a,b,V1,V2,tol,A100,A500,A1000,A2000,A3000,A1250,A1750,A1500,A2250,A750,A350
    real(8),allocatable     :: baus(:),Y(:)

    Write(*,*) 'Problema de contorno'
    write(*,*) 'Hemos usado una barra de 1m de longitud'
    write(*,*) ''

    a=0             !Parametros de la barra
    b=1             !''
    V1=500.d0       !Temperatura inicial
    V2=298.15d0     !Temperatura final

    call contorno(a,b,V1,V2) !subroutine de entorno

  !-------------------Calculo del error------------------
  A100=0
  A500=0
  A750=0
  A1000=0
  A1250=0
  A1500=0
  A1750=0
  A2000=0
  A2250=0
  A3000=0
!-------------------------------------------

allocate(baus(100),Y(100))

  Ax=(b-a)/100

      open(unit=10,file='puntos100.txt', status='old')
              do i=1,100 
                  read(10,*) baus(i), Y(i)
              enddo 
      close(10)

  do i=1,100
      A100=a100+Ax*Y(i)
  enddo
deallocate(baus,Y)
!--------------------------------------------------------------

allocate(baus(350),Y(350))

  Ax=(b-a)/350

    open(unit=10,file='puntos350.txt', status='old')
        do i=1,350 
            read(10,*) baus(i), Y(i)
        enddo 
    close(10)

    do i=1,350
        A350=a350+Ax*Y(i)
    enddo
deallocate(baus,Y)
!--------------------------------------------------------------

allocate(baus(500),Y(500))

  Ax=(b-a)/500

    open(unit=10,file='puntos500.txt', status='unknown')
        do i=1,500 
            read(10,*) baus(i), Y(i)
        enddo 
    close(10)

    do i=1,500
        A500=a500+Ax*Y(i)
    enddo
deallocate(baus,Y)
!------------------------------------------------

allocate(baus(750),Y(750))

    Ax=(b-a)/750

    open(unit=10,file='puntos750.txt', status='unknown')
        do i=1,750 
            read(10,*) baus(i), Y(i)
        enddo 
    close(10)

    do i=1,750
        A750=a750+Ax*Y(i)
    enddo
deallocate(baus,Y)
!------------------------------------------------

allocate(baus(1000),Y(1000))

    Ax=(b-a)/1000
      
    open(unit=10,file='puntos1000.txt', status='unknown')
        do i=1,1000 
            read(10,*) baus(i), Y(i)
        enddo 
    close(10)
      
    do i=1,1000
        A1000=a1000+Ax*Y(i)
    enddo
deallocate(baus,Y)    
!--------------------------------------------

allocate(baus(1250),Y(1250))

    Ax=(b-a)/1250

    open(unit=10,file='puntos1250.txt', status='unknown')
        do i=1,1250 
            read(10,*) baus(i), Y(i)
        enddo 
    close(10)

    do i=1,1250
        A1250=a1250+Ax*Y(i)
    enddo
deallocate(baus,Y)
!------------------------------------------------

allocate(baus(1500),Y(1500))

    Ax=(b-a)/1500

    open(unit=10,file='puntos1500.txt', status='unknown')
        do i=1,1500 
            read(10,*) baus(i), Y(i)
        enddo 
    close(10)

    do i=1,1500
        A1500=a1500+Ax*Y(i)
    enddo
deallocate(baus,Y)
!------------------------------------------------

allocate(baus(1750),Y(1750))

    Ax=(b-a)/1750

    open(unit=10,file='puntos1750.txt', status='unknown')
        do i=1,1750 
            read(10,*) baus(i), Y(i)
        enddo 
    close(10)

    do i=1,1750
        A1750=a1750+Ax*Y(i)
    enddo
deallocate(baus,Y)
!------------------------------------------------

allocate(baus(2000),Y(2000))

    Ax=(b-a)/2000
      
    open(unit=10,file='puntos2000.txt', status='unknown')
        do i=1,2000 
            read(10,*) baus(i), Y(i)
        enddo 
    close(10)
      
    do i=1,2000
        A2000=a2000+Ax*Y(i)
    enddo   
deallocate(baus,Y) 
!--------------------------------------------

allocate(baus(2250),Y(2250))

    Ax=(b-a)/2250

    open(unit=10,file='puntos2250.txt', status='unknown')
        do i=1,2250 
            read(10,*) baus(i), Y(i)
        enddo 
    close(10)

    do i=1,2250
        A2250=a2250+Ax*Y(i)
    enddo
deallocate(baus,Y)
!------------------------------------------------

allocate(baus(3000),Y(3000))

    Ax=(b-a)/3000
      
    open(unit=10,file='puntos3000.txt', status='unknown')
        do i=1,3000 
            read(10,*) baus(i), Y(i)
        enddo 
    close(10)
      
    do i=1,3000
        A3000=a3000+Ax*Y(i)
    enddo
deallocate(baus,Y)
!--------------------------------------------    

allocate(baus(10),Y(10)) 

  baus(1)= 100
  baus(2)=350
  baus(3)= 500
  baus(4)= 750
  baus(5)= 1000
  baus(6)= 1250
  baus(7)= 1500
  baus(8)= 1750
  baus(9)= 2000
  baus(10)= 2250

  Y(1)=abs(a3000-a100)
  Y(2)=abs(a3000-a350)
  Y(3)=abs(a3000-a500)
  Y(4)=abs(a3000-a750)
  Y(5)=abs(a3000-a1000)
  Y(6)=abs(a3000-a1250)
  Y(7)=abs(a3000-a1500)
  Y(8)=abs(a3000-a1750)
  Y(9)=abs(a3000-a2000)
  Y(10)=abs(a3000-a2250)

open(unit=10, file='error.txt', status='unknown')
    write(10,*) size(Y)
  do i=1,10
    write(10,*) baus(i), Y(i)
  enddo
close(10)  

call MiniCu(Y)
end program 