import matplotlib.pyplot as plt #utilizacion de matplotlip y su abreviacion
import numpy as np #utilizacion de numpy y su abreviacion

data2= np.genfromtxt('puntos.txt') #leemos los valores desde un archivo .txt
x2 = data2[:,0]
y2 = data2[:,1]

fig, ax = plt.subplots()
ax.set_ylabel('Temperatura')
ax.set_title('Temperatura en funcion de la distancia')
ax.set_xlabel('Distancia')

plt.plot(x2,y2,'r',)

plt.show()
#ejecutamos arriba a la derecha en 'execute' y nos mostrara nuestra grafica de nube de puntos

