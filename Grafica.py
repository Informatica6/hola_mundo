import matplotlib.pyplot as plt #utilizacion de matplotlip y su abreviacion
import numpy as np #utilizacion de numpy y su abreviacion

#leemos los valores desde un archivo .txt

data= np.genfromtxt('puntos3000.txt') 
x = data[:,0]
y = data[:,1]

data2= np.genfromtxt('puntos100.txt') 
x2 = data2[:,0]
y2 = data2[:,1]

data3= np.genfromtxt('puntos500.txt') 
x3 = data3[:,0]
y3 = data3[:,1]

data4= np.genfromtxt('puntos1000.txt') 
x4 = data4[:,0]
y4 = data4[:,1]

data5= np.genfromtxt('puntos2000.txt') 
x5 = data5[:,0]
y5 = data5[:,1]

fig, ax = plt.subplots()
ax.set_ylabel('Temperatura')
ax.set_title('Temperatura en funcion de la distancia')
ax.set_xlabel('Distancia')

#Generamos las gráficas

plt.plot(x,y,'k',)
plt.plot(x2,y2,'cyan',)
plt.plot(x3,y3,'yellow',)
plt.plot(x4,y4,'g',)
plt.plot(x5,y5,'r',)

plt.show()

#Repetir para el error (la gráfica del error y las de los puntos van en ventanas separadas

error= np.genfromtxt('error.txt') 
xe = error[:,0]
ye = error[:,1]

plt.plot(xe,ye,'k',)

plt.show()