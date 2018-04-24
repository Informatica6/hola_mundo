!------------------------------------------------------------------------------------------------------
!                                          BIBLIOTECA                                                 
! Producto(A,b,axb,info) matriz A,matriz b, axb multiplicacion --------------------------------20      
! Potencia(A,k,Ak)  matriz A, k= pontencia a la que elevas, ak= matriz hecha la potencia-------47                                                                       
! Inversa (A,AI) matriz A a la que quieres hacer la inversa Ai matriz inversa------------------70
! Gauss(A,b,x) Ax=b----------------------------------------------------------------------------148
! FactorizacionLU(A,Al,Au) A matriz que quieres factorizar en LU Al(lower), Au(upper)----------203
! GaussFactorLU(A,b,X) Ax=b--------------------------------------------------------------------262
! MiniCu(A) A devulve los coeficientes del polinomio-------------------------------------------312
! Jacobi(A,b,Tol,x) Ax=b , donde tol es la tolerancia------------------------------------------359
! GAUSS_SEIDEL(A,b,x,ITE) Ax=b, donde ITE es la iteraciones------------------------------------427
! Radio_espectral (A,b,tol,autovalor)----------------------------------------------------------492
!----------------------------------------------------------------------------------------------------
module Algebra_lineal

implicit none

contains

Subroutine Producto(A,B,AxB,info)

    !Variables de entrada/salida
    real(8), intent(in)        :: A(:,:)
    real(8), intent(in)        :: B(:,:)
    real(8), intent(out)       :: AxB(:,:)
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

End subroutine 
!-----------------------------------------------------------------------------------------------------------

Subroutine Potencia(A,k,Ak)

    !Variables de entrada/salida
    real(8), intent(in)         :: A(:,:)
    integer, intent(in)         :: k
    real(8), intent(out)        :: Ak(:,:)

    !Variables locales
    integer                     :: info, i, n
    real(8),allocatable         :: Akplus1(:,:)

    n = size(A(1,:))
    allocate(Akplus1(n,n))

    Ak = A
    do i = 1, k-1
        call producto(A,Ak,Akplus1,info)
        Ak = Akplus1
    enddo 

End subroutine 
!-----------------------------------------------------------------------------------------------------------

Subroutine Inversa (A,AI) 

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

End subroutine    
!-----------------------------------------------------------------------------------------------------------

Subroutine Gauss(A,b,x)

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

End subroutine
!-----------------------------------------------------------------------------------------------------------

Subroutine FactorizacionLU(A,Al,Au)

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

End subroutine
!-----------------------------------------------------------------------------------------------------------

Subroutine Multiplicacion(a,b,ab)           !| Subroutine Aux 
    real(8), intent(in)         :: a,b      !| para la FactorizacionLU
    real(8), intent(out)        :: ab       !|

    ab=a*b

End subroutine 
!-----------------------------------------------------------------------------------------------------------  

Subroutine GaussFactorLU(A,b,X)
    
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

End subroutine
!-----------------------------------------------------------------------------------------------------------

Subroutine MiniCu(A)

    !Element in/out
    Real(8), allocatable, intent(out)       :: A(:)                     !Vector de coeficientes del p(x)

    !Variables del sistema
    Real(8), allocatable                    :: M(:,:),B(:),dato(:,:)
    Integer                                 :: i,l,n,k,p
    Real(8)                                 :: SUMAM,SUMAB

    write(*,*) 'Numero de elementos que quieres el primero empieza en x^0'
    read(*,*) p !Grado del polinomio

    open(unit=1, file='error.txt', status='old')
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

    open(unit=10, file='coeficentes.txt', status='unknown')

        do i=1,size(A)
            write(10,*) A(i)
        enddo

    close(10)

End subroutine
!-----------------------------------------------------------------------------------------------------------

Subroutine Jacobi(A,b,x,tol,norma)

    interface
        function Norma(vector) 

        !Element in/out
        real(8),intent(in)      :: Vector(:)
    
        !Variable del sistema
        real(8)             :: Norma
                    
        end function
    end interface 

    !Element in/out
    Real(8),intent(inout)           :: A(:,:),b(:),x(:)
    real(8),intent(in)              :: tol

    !Variables internas
    integer                         :: i,j,k,m,P
    Real(8)                         :: autovalor
    real(8), allocatable            :: Ab(:,:),PIV(:),V(:),L(:,:),D(:,:),ID(:,:),U(:,:),C(:),T(:,:),Xant(:)
        
    m = size(A,1)
    allocate(Ab(m,m+1),PIV(m-1),V(m+1),L(m,m),D(m,m),ID(m,m),U(m,m),C(m),Xant(m),T(m,m))
        
    Ab(1:m,1:m) = A !| (A|b) matriz
    Ab(1:m,m+1) = b !|
    
    !Sistema de pivotacion 
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
        
    !Aislando la diagonal de la matriz A
    do i=1,m
        D(i,i)=A(i,i)
    enddo
        
    !Aislado el triangulo superior de matriz A
    do i=1,m-1
        do j=1,m-i
            U(i,i+j)=A(i,i+j)
        enddo
    enddo

    !Aislando el triangulo inferior de la matriz A 
    L=A-U-D
        
    call Inversa(D,ID)
        
    C=matmul(ID,b)
    T=matmul(-ID,(U+L))
    
    do k=1,1000000
        x=matmul(T,x)+c
            if(((Norma(x-xant))/Norma(x))<=TOL) exit
        Xant=x
    enddo

End subroutine

!-----------------------------------------------------------------------------------------------------------
Subroutine Gauss_Seidel(A,b,x,ITE) !ITE indica la iteraciones 

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

End subroutine

!-----------------------------------------------------------------------------------------------------------
Subroutine Radio_espectral(A,tol,Autovalor,norma) !Radio espectral es el máx[abs(autovalor)]

    interface
        function Norma(vector) 

        !Element in/out
        real(8),intent(in)      :: Vector(:)

        !Variable del sistema
        real(8)             :: Norma
            
        end function
    end interface 


    !Element in/out
    Real(8), intent(inout)          :: A(:,:)
    Real(8), intent(in)             :: tol
    Real(8), intent(out)            :: autovalor

    !local variable
    Real(8), allocatable            :: TV(:),Temp(:),Baux(:),b(:) !Vectores auxiliares
    real(8)                         :: t,suma
    Integer                         :: i
    integer                         :: n !Dimesion de la matriz

    n=size(A(1,:))
    
    Allocate(TV(n),Temp(n),Baux(n),b(n))

    b=1 !Definimos con cualquier numero

    TV=matmul(A,b)
    TV=(1/norma(tv))*tv !La primera vez con el vector de inicio que nos piden

    do i=1,100
        Temp=Tv             !| Proceso para
        Tv=0                !| converger
        Tv=matmul(A,temp)   !| en el
        Tv=Tv/norma(tv)   !| autovector

       !if(abs(Temp(1)-tv(1))<tol) exit no se como hacer esto
    enddo
    
    !Calculo del autovalor
    Baux=matmul(A,tv)
    suma=0

    do i=1,n 
        call multiplicacion(tv(i),Baux(i),T)
        suma=suma+T
    enddo
    
    Autovalor=abs(suma/(norma(tv))**2) !Formula para obtener el autovalor


End subroutine 

!-----------------------------------------------------------------------------------------------------------
subroutine Riemann(f,a,b,I)

    interface
        function f(x)
            real(8),intent(in)      :: x
            real(8)                 :: f
        end function
    end interface

    Real(8), intent(in)         :: a,b  
    Real(8), intent(out)        :: I    
    
    Real(8)                     :: Ax
    Real(8), allocatable        :: X(:),Y(:) 
    Integer                     :: j,k
    integer,parameter           :: n=1000000

    Allocate(X(n+1),Y(n+1)) 

    Ax= (b-a)/(n-0.d0) 

    do j=1,n+1
        X(j)=a+(j-1)*Ax
        Y(j) = F(X(j)-0.d0) 
        I=I+Ax*Y(j)
    enddo

end subroutine 

!-----------------------------------------------------------------------------------------------------------
subroutine Trapecio(f,a,b,n,It2)

    interface
        function f(x)
            real(8)                 :: x,f
        end function
    end interface
        
    real(8),intent(inout)           ::a,b
    real(8),intent(out)             ::It2
    real(8)                         ::x1,x2,h,A1,A2
    integer                         :: n,j
        
    It2=0
        
    do j=1,n
          
        x1=(a+((j-1)*ABS(a-b)/n))
        x2=(a+(j*(ABS(a-b)/n)))
        
        h=((f(x2))-(f(x1)))
        
        A1=((x2-x1)*(f(x1)))
        A2=((A1))+(((x2-x1))*((h)/(2)))
        
        It2=It2+A2
    end do
end subroutine

!-----------------------------------------------------------------------------------------------------------   
subroutine Simpson(f,a,b,n,It1)

    Interface !se utiliza para llamar a la funcion f(x), que es la que se va cambiando
        function F(X)

        real(8),intent(in)          :: X
        real(8)                     :: F

        end function
    end interface

    integer                         :: j,k
    integer, intent(in)             :: n !n es el numero de repeticiones de la integral
    real(8)                         :: h,I1,I2 !h es la distacia entre dos divisiones
    real(8), intent(in)             :: a,b !a y b son respectivamente el valor inicial y el valor final de x
    real(8), intent(out)            :: It1 !solucion de la integral, el area

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

!-----------------------------------------------------------------------------------------------------------
subroutine mbisectriz(f,a,b,tol,xsol)

    interface
    
        function f(x)
            real(8)                 :: x,f
        end function
    
    end interface

    real(8),intent(inout)           :: a,b
    real(8),intent(out)             :: xsol
    real(8),intent(in)              :: tol
    integer                         :: i,ITER
    real(8), allocatable            :: x(:)
    
    ITER=0

    do i=0,100000000
        allocate(x(i+1))

        x(i+1)=((a+b)/2.d0)

        if ((f(x(i+1)))*f(a)<0) then
            a=a
            b=x(i+1)
        end if

        if (f(x(i+1))*f(b)<0) then 
            a=x(i+1)
            b=b
        endif

        if (abs(F(x(i+1)))<tol) then
            write(*,*)
            write(*,*)'La solucion es X=', X(i+1)
            exit 
        end if

        deallocate(x)

        ITER=ITER+1
    enddo

    write(*,*) 'Numero de iteraciones usadas para hallar la solucion', ITER

end subroutine

!-----------------------------------------------------------------------------------------------------------
subroutine Contorno(a,b,V1,V2)

    Real(8), intent(in)         :: a,b,V1,V2  !a es el valor inicial, b es el valor final, V1 y v2, temperatura inicial y final
    
    Real(8)                     :: Ax ! incremento de x 
    real(8), parameter          :: tol=0.00001 !Precision para el programa del radio espectral y Jacobi
    Real(8)                     :: Autovalor
    real(8), allocatable        :: T(:),M(:,:),D(:,:),K(:,:),b1(:),F(:,:),X(:),baus(:), Y(:)
    integer                     :: i,j,n,count,suma,suma1,countlu !n= numero de divisiones de la barra

    countlu=0
    suma=0 
    suma1=0

    write(*,*) 'Indique el numero de subdivisiones de la barra que desea realizar'
    read(*,*) n 

    Ax=(b-a)/(n-0.d0)
    
    allocate(M(n,n),D(n,n),T(n),K(n,n),b1(n),F(n,n),X(n)) 
      
    !definicion de la matriz D ( matriz de derivacion)

        D=0

        do i=1,n 
            if((i+1)/=(n+1)) D(i,i+1) = 1.d0/(2.d0*Ax)
        enddo
    
        do i=1,n 
            if((i-1)/=0) D(i,i-1) = -1.d0/(2.d0*Ax)
        enddo

        D(1,1) = -1.d0/Ax
        D(n,n) = 1.d0/Ax
        D(n,n-1) = -1.d0/Ax
        D(1,2) = 1.d0/Ax

    !definicion de K(x) (conductivadad termica)

        K=0 

        do i=1,n 
            do j=1,n 
                if((i==j).and.(i*Ax<=(b-a)/2.d0)) K(i,j) = 16.3d0
                if((i==j).and.(i*Ax>(b-a)/2.d0)) k(i,j) = 209.3d0
            enddo
        enddo

    !definicion de M=DKD

        F=matmul(D,K)
        M=matmul(F,D)

    !Aplicamos condiciones de contorno (condiciones inicales)
        M(1,:)=0
        M(n,:)=0
        M(1,1)=1
        M(n,n)=1

        b1=0
        b1(1)=V1
        b1(n)=V2

    !Metodos para hallar la solucion Ax=b
    
        !Diagonal dominate 
            do i=1,n 
                do j=1,n 
                    if(i==j) suma=suma+abs(M(i,j))
                    if(i/=j) suma1=suma1+Abs(M(i,j))
                enddo
            enddo
                

        if(n<=4) then 
            call GaussFactorLU(M,b1,T)
            write(*,*) 'Se ha utilizado el metodo GaussFactorLU'
        endif

        if(suma>suma1) then 
            Call Jacobi(M,b1,T,tol)
            write(*,*) 'Se ha utilizado el metodo de Jacobi'
        else
            call Gauss(M,b1,T) 
            write(*,*) 'Se ha utilizado el sistema Gauss'
        endif

    open(unit=10, file='Puntos.txt',status='unknown')

        X(n)=b

        do i=1,n
            X(i)= a+(i-1)*Ax 
            write(10,*) X(i), T(i)
        enddo

    close(10)

end subroutine
 
!------------------------------------------------------------------------------------------------------------   
subroutine Fractal(X) 

    real(8),intent(out)                 :: x(:)

    real(8),allocatable                 :: J(:,:),b(:),Y(:),x0(:)
    integer                             :: i
    real(8), parameter                  :: tol=0.001
    integer, parameter                  :: n=2, ITR=100


    allocate(J(n,n),b(n),x0(n),Y(n))


        J(1,1) = 3*x0(1)**2-3*x0(2)**2
        J(1,2) = -6*x0(1)*x0(2) 
        J(2,1) = 6*x0(1)*x0(2)
        J(2,2) = 3*x0(1)-3*x0(2)**2

        b(1) = x0(1)**3-3*x0(1)*x0(2)**2-(1.d0/6.d0)
        b(2) = 3*x0(1)**2*x0(1)-x0(2)**3
 
        call Gauss(J,b,y)

        x = x0-y

        if(norma(x-x0)<tol) then 

            x0 = x 
        endif

end subroutine 
!------------------------------------------------------------------------------ 

subroutine newton(x,f,df,tol,max_iter,iter) !para hallar el valor de x en f(x)=0 mediante iteracciones
    
    real(8),intent(inout)       :: x
    real(8),intent(in)          :: tol 
    integer, intent(in)         :: max_iter
    integer,intent(inout)       :: iter
     
    Interface
        function F(x)
            real(8) :: x
            real(8) :: F
        end function

    function df(x)

        real(8) :: x
        real(8) :: df
    
    end function
    
    end interface
    
    !locales
    real(8)      :: x0
    
    x0=x
    
    do iter=1,max_iter !contador del numero de iteracciones teniendo como limte la maxima, introducida anteriormete
    
        x=x0-(f(x0)/(df(x0))) !punto medio del intervalo
        
        if((abs((x-x0)/x)<tol).and.(abs(f(x))<tol)) exit !determina cuando nos hemos acercado lo sufuciente a la raiz segun la tolerancia y se puede considerar por lo tanto solucion
    
        x0=x
    
    enddo
    
end subroutine 
    
end module Algebra_lineal
