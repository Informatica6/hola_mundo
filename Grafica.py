import matplotlib.pyplot as plt #utilizacion de matplotlip y su abreviacion
import numpy as np #utilizacion de numpy y su abreviacion

data2= np.genfromtxt('puntos.txt') #leemos los valores desde un archivo .txt
x2 = data2[:,0]
y2 = data2[:,1]

plt.plot(x2,y2,'b', label='1derivada  ')


data3= np.genfromtxt('derivada_centrada.txt') #leemos los valores desde un archivo .txt
x3 = data3[:,0]
y3 = data3[:,1]

plt.plot(x3,y3,'g', label='1derivada  ')

data= np.genfromtxt('derivada_centrada_2.txt') #leemos los valores desde un archivo .txt
x = data[:,0]
y = data[:,1]

plt.plot(x,y,'r', label='1derivada  ')

plt.show()
#ejecutamos arriba a la derecha en 'execute' y nos mostrara nuestra grafica de nube de puntos

