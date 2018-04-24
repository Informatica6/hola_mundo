program main
use funciones
use algebra_lineal 

!real(8)             :: a,b,It1,It2,I !para la integral por trapecios n se define como real y para la de simpson y Riemann como entero
!integer            :: n
!real(8):: a,b,tol
!integer:: i
!real(8):: xsol
!character           :: contestacion 

!1 write(*,*)'Intervalo de Integracion'

!read(*,*) a,b 

!write(*,*)'¿El intervalo es correcto?',a,b 
!write(*,*) 'Y/N'
!read(*,*) contestacion 

!if((contestacion=='n').or.(Contestacion=='N')) go to 1 

!write(*,*) "Escribe la tolerancia"
!read(*,*) tol 

!write(*,*)'elige un numero de subintervalos' 

!read(*,*)n

!call trapecio(f1,a,b,n,It2)
!write(*,*)'el area de la integral por Trapecio es:',It2

!call simpson(a,b,n,It1,f1)
!write(*,*)'el area de la integral por Simpson es:',It1

!Call Riemann(a,b,I)
!write(*,*)'el area de la integral por Riemann es:',I

!call mbisectriz(f,a,b,tol,xsol)



!Newton-rapshon !se obtiene la raiz de la funcion mediante iteracciones ( a partir de un x0 inicial). 
    !para ello se calcula la recta tg y luego la interseccion de esta con el eje, aproximandose cada vez mas a la raiz.
real(8) :: x,tol
integer:: iter,max_iter
write(*,*) "Escribe el x inicial, la tolerancia y el numero maximo de iteracciones"
read(*,*) x,tol,max_iter
write(*,*) x,tol,max_iter
call newton(x,f,df,tol,max_iter,iter)
write(*,*) x
write(*,*) "numero de iteracciones usadas",iter
End program main
