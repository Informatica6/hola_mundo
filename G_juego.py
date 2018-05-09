import matplotlib.pyplot as plt #utilizacion de matplotlip y su abreviacion
import numpy as np #utilizacion de numpy y su abreviacion

#leemos los valores desde un archivo .txt

data= np.genfromtxt('juego.txt')

S = data[:,0]
Z = data[:,1]
R = data[:,2]
I = data[:,3]
T = data[:,4]

plt.plot(T,S,'b')
plt.plot(T,Z,'g') 
plt.plot(T,I,'y')
plt.plot(T,R,'r') 

plt.show()
