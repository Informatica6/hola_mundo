import matplotlib.pyplot as plt #utilizacion de matplotlip y su abreviacion
import numpy as np #utilizacion de numpy y su abreviacion

data= np.genfromtxt('puntosE.txt') 
x = data[:,0]
y = data[:,1]

plt.plot(x,y, 'b')

x1=np.arange(0,2,0.001)

y1=(x1+1)**2-np.exp(x1)/2

plt.plot(x1,y1, 'k')


plt.show()