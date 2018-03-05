program main

    use algebra_lineal

    real(8),allocatable     :: A(:,:), b(:)
    real(8)                 :: tol,autovalor
    integer, parameter      :: n=2

    Allocate(A(n,n),b(n))

    tol=0.01
    
    b=(/1,1/)
    A(1,:)=(/2,-12/)
    A(2,:)=(/1,-5/)

    call autovalores(A,b,tol,autovalor)

    write(*,*) autovalor
end program main
