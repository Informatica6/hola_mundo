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

    function dz(TIZ,I,R,S,Z,DTIZ,HKZ,V,IN,ZKH)                  ! Zombies

        real(8)             :: TIZ,I,R,S,Z,DTIZ,HKZ,V,IN,ZKH    
        real(8)             :: dz

        dz = TIZ*I+DTIZ*R-HKZ*Z*(S+IN+I*1.3)                    !Los infectados se camuflan entre los zombie y puede matar más

    end function

    function ds(ZKH,S,Z,D,V,I,C)                                ! Susceptible

        real(8)             :: S,Z,ZKH,D,V,I,C                          
        real(8)             :: ds                               
                                
        ds = S-ZKH*S*Z*(0.9)-D*S-C*S                            ! No en todas las ocasiones un zombie va a matar a un humano puede herirlo                    
                                    
    end function 

    function Dr(D,S,I,HKZ,DTIZ,R,Z,IN,ZKH)                      ! Removed

        real(8)             :: D,S,I,HKZ,DTIZ,R,Z,IN,ZKH         
        real(8)             :: dr                               

        dr = D*(S+IN+I)+ZKH*Z*(0.9)*S-DTIZ*R+ZKH*Z*(IN+I*0.7)   !A los zombie les cuesta más matar a infectados porque les cuesta distinguirlos

    end function 

    function di(ZKH,TIZ,D,S,Z,I,V,C)                    ! Infected 

        real(8)             :: ZKH,TIZ,D,S,Z,I,V,C      
        real(8)             :: di

        di = C*S-TIZ*I-D*I-V*I+ZKH*Z*S*(0.10)           ! NO en todas las ocasiones un zombie va a matar a un humano puede herirlo y hacer que se infecte

    end function 
    
    function din(IN,R,D,Z,V,I,NAT,ZKH) 
        
        real(8)             :: IN,R,D,Z,V,I,NAT,ZKH
        real(8)             :: din 

        din = I*V+NAT*IN-IN*D-ZKH*IN*Z                  ! Los inmunes tienen la posibilidad de reproducirse al estar apartados en cuarentenas 
                                                        ! Los hijos de los inmunes son tambien inmunes 
    end function
!____________________________________________________________________
end module
