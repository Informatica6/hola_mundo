module problema_contorno
contains 
subroutine contorno(a,b)

    Real(8), intent(in)         :: a,b
    !Real(8), intent(out)        :: T(:) 

    Real(8)                     :: Ax,PM
    integer, parameter          :: n=5
    real(8), allocatable        :: X(:),M(:,:),D(:,:)
    integer                     :: i,j
   
    PM=(b-a)/2 
    Ax=(PM-a)/(n-0.d0)

    allocate(X(n),M(n,n),D(n,n))

!Definicion del vector X    
    X(1)=a

    do i=2,n
        x(i)=a+Ax
    enddo

!definicion de la matriz D

    do i=1,n
        D(1,i)=-1/Ax 
        D(n,i)=1/Ax  
    enddo

    D(1,1) = -1/Ax 
    D(n,n) = 1/Ax

    do i=2,n-1
        do j=1,i-1
            D(i,j) = -1/2*Ax !Colocando las negativa
        enddo
    enddo

    do i=n-1,2, -1
        do j=i-1,1, -1
            D(i,j)= 1/2*Ax !colocando las positiva
        enddo
    enddo
    
    do i=1,n
        do j=1,n
            If(i==j) D(i,j)=0
        enddo
    enddo
    
    do i=1,n
        D(1,i)=1/Ax 
        D(n,i)=-1/Ax  
    enddo

    do i=1,n 
        write(*,*) D(i,:)
    enddo
end subroutine 

end module 