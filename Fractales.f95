!
!////////////////////////////////////////////////////////////////////////
!
!      fractales.f90
!      Created: 20 April 2018 at 10:40 
!      By: Gonzalo Rubio  
!
!////////////////////////////////////////////////////////////////////////
!
PROGRAM fractales

USE algebra_lineal

IMPLICIT NONE 

!///////////////////////////
! Declaracion de variables ! 
!///////////////////////////

INTEGER :: N                                    ! Dimension de la discretizacion
REAL(8), ALLOCATABLE  :: x(:), y(:)             ! Vectores con los valores de la malla 
REAL(8)               :: dx                     ! Distancia espacial entre nodos
INTEGER               :: i,j                    ! Variable auxiliar

REAL(8)               :: sol(2)


! Inicializacion de variables
N         = 100
dx        = 1.d0/(N-1)

! Reserva de memoria
ALLOCATE ( x(N) )
ALLOCATE ( y(N) )

! Inicializacion de variables a cero

DO i = 1, N 
    x(i) = -1d0 + (i-1) * 2 * dx  
    y(i) = -1d0 + (i-1) * 2 * dx  
ENDDO 

OPEN(UNIT=10,FILE="file.dat")

! Resolucion del problema z^3 - C = 0

DO i = 1, N 
   DO j = 1, N 
   
      sol(1) = x(i)
      sol(2) = y(j)
      
      Call fractal(sol)
      
      WRITE(10,*) x(i), y(j), sol(1)+sol(2)
         ! Las soluciones del problema son: 
         ! 1: c^(1/3)
         ! 2: -(-1)^(1/3) c^(1/3)
         ! 3: (-1)^(2/3) c^(1/3)
         ! Sumando la parte real y la imaginaria, pueden distinguirse 
         !    las tres soluciones 

   ENDDO 
ENDDO 

CLOSE(10)

END PROGRAM fractales   