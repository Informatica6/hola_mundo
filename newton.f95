subroutine newton(x,f,df,tol,max_iter,iter) !para hallar el valor de x en f(x)=0 mediante iteracciones
    real(8), intent(inout) :: x
    real(8), intent(in) :: tol
    integer, intent(in) :: max_iter
    integer, intent(inout) :: iter

    Interface
        function F(x)
            real(8) :: x
            real(8) :: F
        end function
        
        function df(x)
            real(8) :: x
            real(8) :: df
        end function
    end interface

    !locales
    real(8) :: x0

    x0=x
    do iter=1,max_iter !contador del numero de iteracciones teniendo como limte la maxima, introducida anteriormete
        x=x0-(f(x0)/(df(x0))) !punto medio del intervalo
        if((abs((x-x0)/x)<tol).and.(abs(f(x))<tol))  exit 
            !determina cuando nos hemos acercado lo sufuciente a la raiz segun la tolerancia y se puede considerar por lo tanto solucion
        x0=x
    enddo