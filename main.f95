program main

    use algebra_lineal

    real(8),allocatable     :: A(:)
    integer                 :: n

read(*,*) n
    call MiniCu(A)
    
do i=1,n

write(*,*) A(i)

enddo

end program main
