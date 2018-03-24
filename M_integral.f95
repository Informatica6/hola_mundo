module Integracion 

implicit none 

contains 

function F(x)

    Real(8),intent(in)          :: X
    Real(8)                     :: F 

    F = 1/x

end function 

subroutine Riemann(a,b,I)

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

subroutine Trapecio(f,a,b,n,It2)

    interface
        function f(x)
            real(8),intent(in)      :: x
            real(8)                 :: f
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
    
subroutine Simpson(a,b,n,It1,F)

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

end module 