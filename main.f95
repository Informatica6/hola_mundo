program main

use algebra_lineal

real(8)::Xi,Xd,It1,It2,It3,n !para la integral por trapecios n se define como real y para la de simpson y Riemann como entero
!integer :: n

write(*,*)'elige un intervalo de integracion'

read(*,*)Xi !valor inicial de x
read(*,*)Xd !valor final de x

write(*,*)'el intervalo es:',Xi,Xd
write(*,*)'elige un numero de subintervalos' 

read(*,*)n


call trapecio(f1,Xi,Xd,n,It2)
write(*,*)'el area de la integral por Trapecio es:',It2

!call simpson(Xi,Xd,n,It1,f1)
!write(*,*)'el area de la integral por Simpson es:',It1

!call Riemann(Xi,Xd,n,It3,f1)
!write(*,*)'el area de la integral por Riemann es:',It3


end program main
