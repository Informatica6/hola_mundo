program Practica
    
    use algebra_lineal
    
    real(8)         :: a,b,V1,V2,tol

    
    a=0
    b=1
    v1=500.d0
    v2=298.15d0

    call contorno(a,b,V1,V2)
end program 