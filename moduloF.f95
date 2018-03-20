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

    derivada_progresiva = (F(X+h)-Y)/h

end function 

function derivada_regresiva(Y,x,h)

    Real(8),intent(in)              :: Y,X,h

    Real(8)                         :: derivada_regresiva

    derivada_regresiva = (Y-F(X-h))/h

end function

function derivada_centrada(Y,x,h)

    Real(8),intent(in)              :: Y,X,h

    Real(8)                         :: derivada_centrada

    derivada_centrada = (F(x+h)-F(x-h))/(2*h)

end function
    
end module