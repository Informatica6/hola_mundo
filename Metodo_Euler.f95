program M_Euler

    use funciones
    use algebra_lineal

    real(8)                 :: S,R,Z,I,TIZ,T,THKZ,ZKH,D,BR,DTIZ,V,IN,NAT,C,h,HKZ,TV,K 
    integer, parameter      :: N=1000
    integer                 :: j,tiempo
    character               :: Modo 

    tiempo=30

    Write(*,*) '///////////////////////////////////////////////////////////'
    write(*,*) '/                                                         /'
    write(*,*) '/      BIENVENIDOS AL SIMULADOR DE UN APOCALIPSIS         /'
    write(*,*) '/                        ZOMBIE                           /'
    write(*,*) '/                                                         /'
    write(*,*) '///////////////////////////////////////////////////////////'
    write(*,*)

    write(*,*) '¿Cúal modo de juego desea Historia o Libre (H/L)?'
        read(*,*) Modo

    if(Modo=='L') then
        write(*,*) '¿Cuánto tiempo quieres que trascurra?'
        read(*,*) tiempo
    endif

    open (unit=10, file='parametros.txt')

        read(10,*) 
        read(10,*) S,R,I,Z,IN,TV,TIZ,THKZ,ZKH,D,DTIZ,NAT,C,HKZ

    close(10)

    h=tiempo/(N-0.d0)

    open (unit=10, file='Juego.txt', status='unknown')

        do j=0,N-1

            call Game(dr,dz,ds,di,din,S,R,Z,I,TIZ,THKZ,ZKH,D,DTIZ,V,IN,NAT,C,h,HKZ,TV,N)

            write(10,*) S+IN,Z,R,I,j*h

        enddo

    close(10)

    if(Modo=='H') then
        write(*,*)  
        write(*,*) 'Bienvenido al modo historia' 
        write(*,*) '(En este modo se relatará una historia acorde con la gráfica)'
        write(*,*) 
        write(*,*) 'Día 0: 15 de mayo de 2018'
        write(*,*) 'La población mundial no es consciente de los acontecimentos que proximamente les tocará vivir'
        write(*,*) 'Se detecta un caso aislado de una nueva enfermedad que parece inofensiva para la raza humana'
        write(*,*)
        write(*,*) '1º Semana'
        write(*,*) 'Esta nueva enfermedad parece muy contagiosa pero sigue sin existir sintomas de riesgo'
        write(*,*)
        write(*,*) '2º Semana'
        write(*,*) 'Todos los gobiernos al unisono dan una voz de alarma al descubir'
        write(*,*) 'que los infectados de esta rara enfermedad se transforma en seres'
        write(*,*) 'sin ningún tipo de actividad congnitiva, provocando al mismo tiempo una mejora en los reflejos'
        write(*,*) 'y un aumento exponencial en la agresividad'
        write(*,*) 
        write(*,*) '4º semana'
        write(*,*) 'La población mundial esta alarmada centenares de ataques de estos seres ocurren a diaro.'
        write(*,*) 'La población se ha encargado de darles un nombre:'
        write(*,*) 'ZOMBIES'
        write(*,*) 
        write(*,*) '1º-5º Mes' 
        write(*,*) 'La población no sabe como combatir a los zombies,'
        write(*,*) 'provocando que la poblacion de zombie sea mayor que la de supervivientes'
        write(*,*) 'Además la enfermedad se ha contagido rápidamente llegando a todos los rincones del globo'
        write(*,*)
        write(*,*) '5º-20º Mes'
        write(*,*) 'Los supervivientes descubren como combatir a los zombies con mayor eficacia'
        write(*,*) 'y aunque estan en menor número consiguen hacer frente'
        write(*,*) 'Por otro lado, los intentos desesperados por encontrar una vacuna dan sus frutos,' 
        write(*,*) 'los vacunados son inmunes al virus Z'
        write(*,*)
        write(*,*) '20º-30º Mes'
        write(*,*) 'La creación de cuarentenas, permitiendo que se creen colonias de inmunes,'
        write(*,*) 'consiguen hacer frente a los zombies' 
        write(*,*) 'LA HUMANIDAD HA GANADO LA BATALLA'
        write(*,*) 
        write(*,*) 'Ahora cabe preguntarse, ¿De dónde ha salido este virus Z?,'
        write(*,*) '¿Es un intento de la naturaleza por controlar la descontrolada poblacion?'
    endif

    call system ('python G_juego.py')

end program M_Euler