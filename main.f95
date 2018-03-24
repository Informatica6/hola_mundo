program main

use Integracion

real(8)             :: a,b,It1,It2,I !para la integral por trapecios n se define como real y para la de simpson y Riemann como entero
!integer            :: n
character           :: contestacion 

1 write(*,*)'Intervalo de Integración'

read(*,*) a,b 

write(*,*)'¿El intervalo es correcto?',a,b 
write(*,*) 'Y/N'
read(*,*) contestacion 

if((contestacion=='n').or.(Contestacion=='N')) go to 1 

!write(*,*)'elige un numero de subintervalos' 

!read(*,*)n

!call trapecio(f1,a,b,n,It2)
!write(*,*)'el area de la integral por Trapecio es:',It2

!call simpson(a,b,n,It1,f1)
!write(*,*)'el area de la integral por Simpson es:',It1

call Riemann(a,b,I)
write(*,*)'el area de la integral por Riemann es:',I


end program main
