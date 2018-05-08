module Funciones 

implicit none

contains

Function F(x)

    Real(8)                   :: F,x

    F = x**2-1

End function 
!------------------------------------------------------------------    
function derivada_progresiva(dx)

    Real(8)                         :: dx
    Real(8)                         :: derivada_progresiva,h 

    h = 0.0000001

    derivada_progresiva = (F(dx+h)-F(dx))/h

end function 

!-------------------------------------------------------------------
function derivada_regresiva(dx)

    Real(8)                          :: dx
    Real(8)                          :: derivada_regresiva,h 

    h = 0.0000001

    derivada_regresiva = (F(dx-2*h)-4*F(dx-h)+3*F(dx))/2*h

end function

!--------------------------------------------------------------------
function derivada_centrada(dx)

    Real(8)                         :: dx

    Real(8)                         :: derivada_centrada,h

    h=0.00000001

    derivada_centrada = (F(dx+h)-F(dx-h))/h

end function

!--------------------------------------------------------------------
function derivada_progresiva_2(Y,x,h)

    Real(8),intent(in)              :: Y,X,h
    
    Real(8)                         :: derivada_progresiva_2
    
    derivada_progresiva_2 = (-F(x+3*h)+4*F(x+2*h)-5*F(x+h)+2*Y)/h**2
    
end function

!--------------------------------------------------------------------
function derivada_regresiva_2(Y,x,h)

    Real(8),intent(in)              :: Y,X,h
    
    Real(8)                         :: derivada_regresiva_2
    
    derivada_regresiva_2 = (-F(X-3*h)-4*F(x-2*h)-5*F(x-h)+2*Y)/h**2
    
end function

!--------------------------------------------------------------------
function derivada_centrada_2(Y,x,h)

    Real(8),intent(in)              :: Y,X,h
    
    Real(8)                         :: derivada_centrada_2
    
    derivada_centrada_2 = (F(x+h)-2*Y+F(x-h))/h**2
    
end function 

!--------------------------------------------------------------------
function Norma(vector) 

    !Element in/out
    real(8)             :: Vector(:)
    
    !Variable del sistema
    real(8)             :: Norma
    integer             :: i
               
    Norma=0.d0
    
    do i=1,size(vector)
        Norma = Norma + Vector(i)**2
    enddo
                    
    Norma=sqrt(Norma)
                    
end function
!--------------------------------------------------------------------
function Fs1(x,y) 

    !Element in/out  
    real(8)                 :: x,y 

    !local variable
    real(8)                 :: Fs1 

    Fs1 = y-x**2+1

end function 

!--------------------------------------------------------------------
function Fs2(x,y) 

    !Element in/out  
    real(8)                 :: x,y 
    
    !local variable
    real(8)                 :: Fs2
    
    Fs2 = 3*x**2*y-y**3
end function 

!--------------------------------------------------------------------


!                       Juego del apocalisis Zombie 
    function Zombie(dz)
    end function

    function Susceptible(ds)
    end function 

    function Remove(dr)
    end function 

    function Infectacion(di) 
    end function 
!____________________________________________________________________
end module
