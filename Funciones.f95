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


!                       Zombies` Apocalypse Game

    function dz(TIZ,I,R,S,Z,DTIZ,HKZ,V,IN,ZKH)                 ! Zombies

        real(8)             :: TIZ,I,R,S,Z,DTIZ,HKZ,V,IN,ZKH   ! V vacuna
        real(8)             :: dz

        dz = TIZ*I+DTIZ*R-HKZ*S*Z-HKZ*Z*IN-HKZ*I*Z

    end function

    function ds(ZKH,S,Z,D,V,I,C)                       ! Susceptible

        real(8)             :: S,Z,ZKH,D,V,I,C         ! ZKH zombie kills human propension marginal a que un humano sea asesinado por un zombie         
        real(8)             :: ds 
                                                            ! BR Personas que existen al principio
        ds = S-ZKH*S*Z*(0.9)-D*S-C*S                    
                                                            ! D propension marginal a morir por casusas naturales 
    end function 

    function Dr(D,S,I,HKZ,DTIZ,R,Z,IN,ZKH)                 ! Removed

        real(8)             :: D,S,I,HKZ,DTIZ,R,Z,IN,ZKH   ! DTIZ dead turns into zombie  muerto que se convierte en zombie 
        real(8)             :: dr                       ! R eliminados 

        dr = D*(S+IN+I)+ZKH*Z*(0.9)*(IN+I+S)-DTIZ*R

    end function 

    function di(ZKH,TIZ,D,S,Z,I,V,C)                    ! Infected 

        real(8)             :: ZKH,TIZ,D,S,Z,I,V,C      ! TIZ turn into zombie propension marginal a comvertirse en un zombie 
        real(8)             :: di

        di = C*S-TIZ*I-D*I-V*I+ZKH*Z*S*(0.10)           !Los infectados se camuflan entre los zombie

    end function 
    
    function din(IN,R,D,Z,V,I,NAT,ZKH) 
        
        real(8)             :: IN,R,D,Z,V,I,NAT,ZKH
        real(8)             :: din 

        din = I*V+NAT*IN-IN*D-ZKH*IN*Z

    end function
!____________________________________________________________________
end module
