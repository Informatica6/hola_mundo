module func
implicit none
contains

function F(x)

    Real(8)        :: X
    Real(8)        :: F 

    F = x**3+x**2-3*x-3

end function 
end module
