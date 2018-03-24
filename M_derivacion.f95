module Derivadas 

contains 

function F(x)

    Real(8),intent(in)          :: X
    Real(8)                     :: F 

    F = x**2

end function 

function derivada_progresiva(Y,x,h)

    Real(8),intent(in)              :: Y,X,h

    Real(8)                         :: derivada_progresiva

    derivada_progresiva = (F(x+h)-Y)/h

end function 

function derivada_regresiva(Y,x,h)

    Real(8),intent(in)              :: Y,X,h

    Real(8)                         :: derivada_regresiva

    derivada_regresiva = (F(x-2*h)-4*F(x-h)+3*Y)/2*h

end function

function derivada_centrada(Y,x,h)

    Real(8),intent(in)              :: Y,X,h

    Real(8)                         :: derivada_centrada

    derivada_centrada = (F(x+h)-F(x-h))/h

end function

function derivada_progresiva_2(Y,x,h)

    Real(8),intent(in)              :: Y,X,h
    
    Real(8)                         :: derivada_progresiva_2
    
    derivada_progresiva_2 = (-F(x+3*h)+4*F(x+2*h)-5*F(x+h)+2*Y)/h**2
    
end function

function derivada_regresiva_2(Y,x,h)

    Real(8),intent(in)              :: Y,X,h
    
    Real(8)                         :: derivada_regresiva_2
    
    derivada_regresiva_2 = (-F(X-3*h)-4*F(x-2*h)-5*F(x-h)+2*Y)/h**2
    
end function

function derivada_centrada_2(Y,x,h)

    Real(8),intent(in)              :: Y,X,h
    
    Real(8)                         :: derivada_centrada_2
    
    derivada_centrada_2 = (F(x+h)-2*Y+F(x-h))/h**2
    
end function    

function Norma(vector) 

    !Element in/out
    real(8),intent(in)      :: Vector(:)
    
    !Variable del sistema
    real(8)             :: Norma
    integer             :: i
               
    Norma=0.d0
    
    do i=1,size(vector)
        Norma = Norma + Vector(i)**2
    enddo
                    
    Norma=sqrt(Norma)
                    
end function
        
end module