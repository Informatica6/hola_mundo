subroutine Fractal(X) 

    real(8),intent(out)                 :: x(:)

    real(8),allocatable                 :: J(:,:),b(:),Y(:),x0(:)
    integer                             :: i
    real(8), parameter                  :: tol=0.001
    integer, parameter                  :: n=2, ITR=100


    allocate(J(n,n),b(n),x0(n))


    write(*,*) 'diga la primera pareja de numeros'
    read(*,*) x0(1),x0(2)


    do i=1,ITR

        allocate(Y(n))

        J(1,1) = 3*x0(1)**2-3*x0(2)**2
        J(1,2) = -6*x0(1)*x0(2) 
        J(2,1) = 6*x0(1)*x0(2)
        J(2,2) = 3*x0(1)-3*x0(2)**2

        b(1) = x0(1)**3-3*x0(1)*x0(2)**2-(1.d0/6.d0)
        b(2) = 3*x0(1)**2*x0(1)-x0(2)**3
 
        call Gauss(J,b,y)

        x = x0-y

        deallocate(Y)

        write(*,*) X    

        x0 = x 
    enddo

end subroutine 