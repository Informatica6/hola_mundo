!------------------------------------------------------------------------------------------------------
!                                       BIBLIOTECA WOLO                                                 
! Producto(A,b,axb,info) matriz A,matriz b, axb multiplicacion --------------------------------25       
! Potencia(A,k,Ak)  matriz A, k= pontencia a la que elevas, ak= matriz hecha la potencia-------51                                                                       
! Inversa (A,AI) matriz A a la que quieres hacer la inversa Ai matriz inversa------------------75
! Gauss(A,b,x) Ax=b----------------------------------------------------------------------------153
! FactorizacionLU(A,Al,Au) A matriz que quieres factorizar en LU Al(lower), Au(upper)----------209
! GaussFactorLU(A,b,X) Ax=b--------------------------------------------------------------------270
! MiniCu(A) A devulve los coeficientes del polinomio-------------------------------------------319
! Jacobi(A,b,Tol,x) Ax=b , donde tol es la tolerancia------------------------------------------366
! Norma(Vector,n) hace el modulo de un vector de tamaño n--------------------------------------434
! GAUSS_SEIDEL(A,b,x,ITE) Ax=b, donde ITE es la iteraciones------------------------------------455
! Radio_espectral (A,b,tol,autovalor)----------------------------------------------------------519
! Simpson(a,b,n,I,F)---------------------------------------------------------------------------563
! Trapecio(f,a,b,n,I)--------------------------------------------------------------------------598
! Riemann(a,b,n,I,F)---------------------------------------------------------------------------629
! function f1(x) !f1 es la funcion que se va a integrar----------------------------------------659
!----------------------------------------------------------------------------------------------------
module Algebra_lineal

implicit none 

contains

subroutine Producto(A,B,AxB,info)

    !Variables de entrada/salida
    real, intent(in)        :: A(:,:)
    real, intent(in)        :: B(:,:)
    real, intent(out)       :: AxB(:,:)
    integer, intent(out)    :: info     !info=  0 -> OK
                                        !info= -1 -> Error

    !Variables locales
    integer     :: N,M

    N = size(A(1,:))
    M = size(B(:,1))

    if (N==M) then
        info = 0
        AxB  = matmul(A,B)
    else 
        info = -1  
        write(*,*) "There is a problem with the dimensions of the matrices"
        stop
    endif 

end subroutine 
!------------------------------------------------------------------------------------------------------------------------------------------
subroutine Potencia(A,k,Ak)

    !Variables de entrada/salida
    real, intent(in)        :: A(:,:)
    integer, intent(in)     :: k
    real, intent(out)       :: Ak(:,:)

    !Variables locales
    integer                 :: info, i, n
    real,allocatable        :: Akplus1(:,:)

    n = size(A(1,:))
    allocate(Akplus1(n,n))

    Ak = A
    do i = 1, k-1
        call producto(A,Ak,Akplus1,info)
        Ak = Akplus1
    enddo 

end subroutine 

!-------------------------------------------------------------------------------------------------------------------------------

subroutine Inversa (A,AI) 

    !Element in/out
    real(8), intent(in)                     :: A(:,:)  !| !| Known dimension
    real(8), allocatable, intent(out)       :: AI(:,:)

    !local variable 
    real(8), allocatable    :: VT(:),Id(:,:),C(:,:),Ab(:,:)
    real(8)                 :: h 
    integer                 :: i,k,t,l,m
    integer                 :: count !Determina si el sistema es imcompatible

    m=size(A,1)
    allocate(Ab(m,2*m),VT(m+1),Id(m,m),C(m,m),AI(m,m))

    Id=0.d0
    do i=1,m
        Id(i,i)=1 !| Matriz indentidad
    enddo

    Ab(1:m,1:m) = A
    Ab(1:m,m+1:2*m) = Id

    !Triangulación por abajo
    do i = 1, m-1
        if (abs(Ab(i,i)) < epsilon(1.d0)) then
            do t = 1, m-i
                if(Ab(i+t,i)/=0) then !| Pivote Parcial

                    VT(:)=Ab(i,:) !Vector donde se guarda temporalmente una fila

                    Ab(i,:) = Ab(i+t,:) !|   Cambiador 
                    Ab(i+t,:) = VT(:)   !|   de filas 

                endif
            enddo 
        endif
        
        do  k = i+1,m                        !| filas por debajo   
            h = Ab(k,i)/Ab(i,i)              !| Factor que multiplica la fila i
            Ab(k,:) = Ab(k,:) - h*Ab(i,:)
        enddo

    count=0
        do l =1,m 
            if(Ab(i,i)==0) count=count+1 !| Cuando el contador sea igual que la dimension 
        enddo                            !| de la matriz entonces la diagonal esta llena de 0   

        if(count==m) then                       !| Detección de que no existe inversa 
            write(*,*) '----------------'
            write(*,*) 'No tiene inversa'
            write(*,*) 
            stop 
        endif
        
    enddo

    !Triangulación por arriba
    do i = m,2,-1
        do k = i-1,1,-1
            h=0
            h = Ab(k,i)/Ab(i,i)              
            Ab(k,:) = Ab(k,:) - h*Ab(i,:)
        enddo
    enddo

    !Hacer unos en la diagonal
    do i=1,m
        h=0
        h=Ab(i,i) 
        Ab(i,:)=Ab(i,:)/h
    enddo

    AI=Ab(1:m,m+1:2*m) !Descartando  las matriz identidad

end subroutine    
!------------------------------------------------------------------------------------------------------------------------------------------

subroutine Gauss(A,b,x)

    !Element in/out
    real(8), intent(in)     :: A(:,:) !|
    real(8), intent(in)     :: b(:)   !| Known dimension 
    real(8), intent(out)    :: x(:)   !|

    !local variable 
    integer                 :: m ! Problem dimesion A(m,m),b(m),x(m)
    real(8), allocatable    :: Ab(:,:) ! extended matrix
    real(8), allocatable    :: PIV(:),V(:)
    real(8)                 :: h 
    integer                 :: i,j,k,P

    m = size(A,1)
    allocate(Ab(m,m+1),PIV(m-1),V(m+1))
    Ab(1:m,1:m) = A
    Ab(1:m,m+1) = b

    !Triangulación por debajo
    do i = 1, m-1
        if (abs(Ab(i,i))<epsilon(1.d0)) then
            P=0
            PIV=0
            do j=1,m-i
                
                PIV(j)=abs(Ab(i+j,i))

            enddo        
            P=maxloc(PIV,1)
            V(:)=Ab(i,:)
            Ab(i,:)=Ab(i+P,:)
            Ab(i+P,:)=V(:)
        endif
            
        do  k = i+1,m                        !| filas por debajo   
            h = Ab(k,i)/Ab(i,i)              !| Factor que multiplica la fila i
            Ab(k,:) = Ab(k,:) - h*Ab(i,:)
        enddo
    enddo

    !Sustitución
    do  i = m,1,-1
        h = Ab(i,m+1) ! keeping value of the extended column on h 
            
        do  j = i+1,m
            h = h-Ab(i,j)*x(j)
        enddo
            
        x(i) = h/Ab(i,i)
    enddo

end subroutine

!-------------------------------------------------------------------------------------------------------------------------------------------

subroutine FactorizacionLU(A,Al,Au)

    !Element in/out
    real(8), intent(in)                      :: A(:,:)
    real(8), allocatable, intent(out)        :: Al(:,:), Au(:,:)

    !local variable 
    integer                     :: i,j,k,n          !| n dimension de la matriz A
    real(8)                     :: MULT             !| Variable de multiplicacion A,Au,Al

    n=size(A,1)
    Allocate(Al(n,n),Au(n,n))                       !| Determinando la dimension de las matrices

    Al=0 !| Dando valores
    Au=0 !| para tenerlas definidas

    Al(1,1)=A(1,1)      !| Elementos                                    
    Au(1,1)=1           !| invarientes

    !| Codigo de Factorizacion
    do k = 1,n-1
        Au(1,k+1)=A(1,k+1)/Al(1,1)
       
        do i =2,k
            do j= 1,i-1
                call multiplicacion(Al(i,j),Au(j,k+1),MULT) !| Operaciones 
                Au(i,k+1)=(A(i,k+1)-MULT)/Al(i,i)           !| para matriz Au
            enddo
        enddo
        
        Al(k+1,1)=A(k+1,1) !| Posiciones estáticas Al

        do i= 2,k
            do j= 1, i-1 
                call multiplicacion(Au(j,i),Al(k+1,j),MULT) !| Operaciones 
                Al(k+1,i)=A(k+1,i)-MULT                     !| para matriz Al
            enddo
        enddo   
        
        Au(k+1,k+1)=1 !| Posiciones estaticas Au

        do i=1,k 
            call multiplicacion(Al(k+1,i),Au(i,k+1),MULT) !| Operaciones
            Al(k+1,k+1)= A(k+1,k+1)-MULT                  !| para matriz Al
        enddo
    enddo

end subroutine

!-----------------------------------------------------------------------------------------------------------

subroutine Multiplicacion(a,b,ab)           !| Subroutine auxiliar 
    real(8), intent(in)         :: a,b      !| para la FactorizacionLU
    real(8), intent(out)        :: ab       !|

    ab=a*b

end subroutine 

!-----------------------------------------------------------------------------------------------------------  

subroutine GaussFactorLU(A,b,X)
    
    Real(8), intent(in)         :: A(:,:),b(:) !| Matriz de coeficentes y vector de valores
    Real(8), intent(out)        :: X(:)

    Real(8), allocatable        :: Al(:,:),Au(:,:)  !| Matrices tras la factorizacion
    Real(8), allocatable        :: Y(:)             !| Incognitas para el metodo
    integer                     :: i,j,n            !| n dimension de la matrices
    real(8)                     :: suma

    n= size(A,1)
    allocate(Al(n,n),Au(n,n),Y(n))
    
    call FactorizacionLU(A,Al,Au) 

    !Sustitucion Ly=b

    Y=0
    Y(1)=b(1)/Al(1,1) !Propiedad que ocurre en toda matriz

    do i=2,n
    suma=0

        do j=1,n
            if(i/=j) suma=suma+Al(i,j)*Y(j) !Se descarta la posicion donde esta la incognita a despejar
        enddo
        
    Y(i)=(b(i)-suma)/Al(i,i) !Se halla el valor de la incognita, solucion parcial 

    enddo

    !Sustitucion Ux=y

    X=0
    X(n)=Y(n)/Au(n,n) !Propiedad que ocurre en toda matriz 

    do i=n-1,1, -1
    suma=0

        do j=n,1, -1 
            if(i/=j) suma=suma+Au(i,j)*X(j) !Se descarta la posicion donde esta la incognita a despejar
        enddo
        
    X(i)=(Y(i)-suma)/Au(i,i) !Se halla el valor de la incognita, despejando la autentica solucion 
    
    enddo

end subroutine
!-----------------------------------------------------------------------------------------------------------
subroutine MiniCu(A)

    !Element in/out
    Real(8), allocatable, intent(out)       :: A(:)                     !Vector de coeficientes del p(x)

    !Variables del sistema
    Real(8), allocatable                    :: M(:,:),B(:),dato(:,:)
    Integer                                 :: i,l,n,k,p
    Real(8)                                 :: SUMAM,SUMAB

    write(*,*) 'Numero de elementos que quieres el primero empieza en x^0'
    read(*,*) p !Grado del polinomio

    open(unit=1, file='data_file.dat', status='old')
    read(1,*) n !Determinando el numero de puntos 

    allocate(M(p,p),B(p),A(p),Dato(n,2)) 

    do i=1,n
        read(1,*) dato(i,:) !leyendo los puntos en el vector datos(:,1:2)
    enddo

    close(1) ! Cierre del documento

    do l=1,p
        do k=1,p
        SUMAM=0
            do i=1,n
                SUMAM=SUMAM+((dato(i,1)**(l-1))*(dato(i,1)**(k-1))) 
            enddo
        M(l,k)=SUMAM
        enddo
    enddo

    do l=1,p
        SUMAB=0
        do i=1,n
            SUMAB=SUMAB+(dato(i,1)**(l-1)*(dato(i,2))) 
        enddo
        B(l)=SUMAB
    enddo

    call Gauss(M,B,A)

end subroutine
!-------------------------------------------------------------------------------------------------------------- 

subroutine Jacobi(A,b,Tol,x)

    !Element in/out
    Real(8),intent(inout)   :: A(:,:),b(:),x(:)
    real(8),intent(in)      :: Tol
    
    !Variables internas
    integer                 :: i,j,m,P,k
    real(8), allocatable    :: Ab(:,:),PIV(:),V(:),L(:,:),D(:,:),ID(:,:),U(:,:),Q(:),T(:,:),Xant(:)
        
    m = size(A,1)
    allocate(Ab(m,m+1),PIV(m-1),V(m+1),L(m,m),D(m,m),ID(m,m),U(m,m),Q(m),Xant(m),T(m,m))
        
    Ab(1:m,1:m) = A !| (A|b) matriz
    Ab(1:m,m+1) = b !|
        
    do i=1,m-1  
        if (abs(Ab(i,i))<epsilon(1.d0)) then
        P=0
        PIV=0

            do j=1,m-i
                PIV(j)=abs(Ab(i+j,i))
            enddo 

        P=maxloc(PIV,1)
        V(:)=Ab(i,:)
        Ab(i,:)=Ab(i+P,:)
        Ab(i+P,:)=V(:)
        endif
    enddo
        
    A = Ab(1:m,1:m)
    b = Ab(1:m,m+1)
        
    L=0.d0
    U=0.d0
    D=0.d0
        
    do i=1,m
        D(i,i)=A(i,i)
    enddo
        
        
    do i=1,m-1
        do j=1,m-i
            U(i,i+j)=A(i,i+j)
        enddo
    enddo
        
    L=A-U-D
        
    call Inversa(D,ID)
        
    Q=MATMUL(ID,b)
    T=MATMUL(-ID,(U+L))
        
    do k=1,1000000
        x=MATMUL(T,x)+Q
            if(((Norma(x-xant,m))/Norma(x,m))<=TOL)EXIT
        Xant=x
    enddo
        
    !write(*,*)'K=',k !Para saber en que valor se ha parado el bucle
        
end subroutine

!--------------------------------------------------------------------------------------------------------------
function Norma(vector,n) !Esta subroutine hace el modulo de un vector de n dimensiones 

    !Element in/out
    real(8),intent(in)      :: Vector(:)
    integer,intent(in)      :: n

    !Variable del sistema
    real(8)             :: Norma
    integer             :: i
           
    Norma=0.d0

    do i=1,n
        Norma = Norma + Vector(i)**2
    enddo
                
    Norma=sqrt(Norma)
                
end function

!----------------------------------------------------------------------------------------------------------------
subroutine GAUSS_SEIDEL(A,b,x,ITE) !ITE indica la iteraciones 

    !Element in/out
    real(8),intent(inout)   :: A(:,:)
    REAL(8),intent(inout)   :: b(:)
    real(8),intent(inout)   :: x(:)
    Integer,intent(in)      :: ITE
       
    !local variable 
    integer :: i,j,m,P,k
    real(8), allocatable :: Ab(:,:),PIV(:), V(:),L(:,:),D(:,:),IDL(:,:),U(:,:),Q(:),T(:,:),Xant(:),G(:,:)
                    
    m = size(A,1)
    allocate(Ab(m,m+1),PIV(m-1),V(m+1),L(m,m),D(m,m),IDL(m,m),U(m,m),Q(m),Xant(m),T(m,m),G(m,m))

                    
    Ab(1:m,1:m) = A
    Ab(1:m,m+1) = b
                        
    do i=1,m-1  
        if (abs(Ab(i,i))<epsilon(1.d0)) then
            P=0
            PIV=0
                do j=1,m-i
                    PIV(j)=abs(Ab(i+j,i))
                enddo        
            P=maxloc(PIV,1)
            V(:)=Ab(i,:)
            Ab(i,:)=Ab(i+P,:)
            Ab(i+P,:)=V(:)
        endif
    enddo
                        
    A = Ab(1:m,1:m)
    b = Ab(1:m,m+1)
                        
    L=0
    U=0
    D=0
                        
    do i=1,m
        D(i,i)=A(i,i)
    enddo
                        
                        
    do i=1,m-1
        do j=1,m-i
            U(i,i+j)=A(i,i+j)
        enddo
    enddo
                        
    L=A-U-D
    G=D+L
    call INVERSA(G,IDL)
                    
    Q=MATMUL(IDL,b)
    T=MATMUL(-IDL,U)
                    
    do k=1,ITE
        x=MATMUL(T,x)+Q
    enddo

end subroutine
!--------------------------------------------------------------------------------------------------------------------
Subroutine Radio_espectral(A,b,tol,Autovalor) !llamamos radio espectral a el máximo valor comparándolos en valor absoluto

    !Element in/out
    Real(8), intent(inout)          :: A(:,:)
    Real(8), intent(in)             :: b(:),tol 
    Real(8), intent(out)            :: autovalor

    !local variable 
    Real(8), allocatable            :: TV(:),Temp(:),Baux(:) !Vectores auxiliares
    real(8)                         :: t,suma
    Integer                         :: i
    integer                         :: n !Dimesion de la matriz

    n=size(A(1,:))

    Allocate(TV(n),Temp(n),Baux(n))

    TV=matmul(A,b)
    TV=(1/norma(tv,n))*tv !La primera vez con el vector de inicio que nos piden

    do i=1,100
        Temp=Tv             !| Proceso para
        Tv=0                !| converger
        Tv=matmul(A,temp)   !| en el
        Tv=Tv/norma(tv,n)   !| autovector

       !if(abs(Temp(1)-tv(1))<tol) exit no se como hacer esto
    enddo
    
    !Calculo del autovalor
    Baux=matmul(A,tv)
    suma=0

    do i=1,n 
        call multiplicacion(tv(i),Baux(i),T)
        suma=suma+T
    enddo
    
    Autovalor=suma/(norma(tv,n))**2 !Formula para obtener el autovalor

end subroutine 

!-------------------------------------------------------------------------------------------------------------------

subroutine Simpson(a,b,n,It1,F)

    Interface !se utiliza para llamar a la funcion f(x), que es la que se va cambiando
        function F(X)

        real(8),intent(in):: X
        real(8):: F

        end function
    end interface

integer :: j,k
integer, intent(in):: n !n es el numero de repeticiones de la integral
real(8) :: h,I1,I2 !h es la distacia entre dos divisiones
real(8), intent(in) :: a,b !a y b son respectivamente el valor inicial y el valor final de x
real(8), intent(out) :: It1 !solucion de la integral, el area

h=(b-a)/n*1.0
I1=0 
I2=0

do j=1,n-1,2
    I1=I1+F(a+j*h)
end do

do k=2,n-2,2
    I2=I2+F(a+k*h)
end do

It1=(h/3)*(F(a)+4*I1+2*I2+F(b))

end subroutine

!----------------------------------------------------------------------------------------------------------

subroutine trapecio(f,a,b,n,It2)

    interface
    function f(x)
    real(8),intent(in)::x
    real(8)::f
    end function
    end interface
    
    real(8),intent(inout)::a,b,n
    real(8),intent(out)::It2
    real(8)::x1,x2,h,A1,A2,j
    
    It2=0
    
    do j=1,n
      
    x1=(a+((j-1)*ABS(a-b)/n))
    x2=(a+(j*(ABS(a-b)/n)))
    
    h=((f(x2))-(f(x1)))
    
    A1=((x2-x1)*(f(x1)))
    A2=((A1))+(((x2-x1))*((h)/(2)))
    
    It2=It2+A2
    end do
    end subroutine trapecio

!------------------------------------------------------------------------------------------------------------

subroutine Riemann(a,b,n,It3,F)

    Interface !se utiliza para llamar a la funcion f(x), que es la que se va cambiando
        function F(X)

        real(8),intent(in):: X
        real(8):: F

        end function
    end interface

integer :: j,k !OJO, no usar i para el bucle porque I es la solucion de la integral
integer, intent(in):: n !n es el numero de divisiones del eje x de la parte que ocupa la curva
real(8) :: h,I1 !h es la distacia entre dos divisiones
real(8), intent(in) :: a,b !a y b son respectivamente el valor inicial y el valor final de x
real(8), intent(out) :: It3 !solucion de la integral, el area

h=(b-a)/n*1.0
I1=0 
It3=0

do j=1,n
    I1=I1+F(a+j*h)
end do

It3=I1*h

end subroutine

!-------------------------------------------------------------------------------------------------------

function f1(x) !f1 es la funcion que se va a integrar !para el ejemplo de clase el intervalo optimo de integracion es 1000
    real(8),intent(in)::x
    real(8)::f1
    
    f1=(EXP(-x**2))
    
end function
    
end module Algebra_lineal
