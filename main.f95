program main

use algebra_lineal

real::Xi,Xd,I!,n !para la integral por trapecios n se define como real y para la de simpson y Riemann como entero
integer :: n

write(*,*)'elige un intervalo de integracion'

read(*,*)Xi !valor inicial de x
read(*,*)Xd !valor final de x

write(*,*)'el intervalo es:',Xi,Xd
!write(*,*)'elige un numero de subintervalos' !trapecio !riemann
write(*,*)'elige el numero de repeticiones de la integral' !simpson

read(*,*)n

!call trapecio(f1,Xi,Xd,n,I)
call simpson(Xi,Xd,n,I,f1)
!call Riemann(Xi,Xd,n,I,f1)

write(*,*)'el area de la integral es:',I

end program main
