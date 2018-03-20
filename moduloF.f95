module funciones 

contains 

function mi_funcion(x)

    Real(8), intent(in)     :: x

    Real(8)                 :: mi_funcion 

    mi_funcion = 10.d0*x**2*exp(-x**2)


end function 

function derivada_centrada(h)


    real(8),intent(in)              :: h

    integer                         :: x
    real(8)                        :: derivada_centrada
    real(8), allocatable            :: Y(:)
    integer                         :: a,b,i

    read(*,*) a,b
    read(*,*) x

    allocate(Y(a:b))

    do i=a,b
        Y(i) = mi_funcion(i-0.d0) 
    enddo


    derivada_centrada = (Y(x+1)-Y(x-1))/2*h

    
end function 

end module