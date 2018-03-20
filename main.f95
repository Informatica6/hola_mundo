program main

use algebra_lineal

real::Xi,Xd,I !para la integral por trapecios n se define como real y para la de simpson como entero
integer :: n
write(*,*)'elige un intervalo de integracion'
read(*,*)Xi
read(*,*)Xd
write(*,*)'el intervalo es:',Xi,Xd
!write(*,*)'elige un numero de subintervalos' !trapecio
write(*,*)'elige el numero de repeticiones de la integral' !simpson
read(*,*)n
!call integral(f1,Xi,Xd,n,I)
call SIMPSON(Xi,Xd,n,I,f1)
write(*,*)'el area de la integral es:',I

end program main
