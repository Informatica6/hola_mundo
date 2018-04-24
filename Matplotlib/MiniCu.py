import matplotlib.pyplot as plt
import numpy as np


data= np.genfromtxt('puntos.txt', skip_header = 1)

x = data[:,0]
y = data[:,1]

plt.plot(x,y, 'k+')

data1= np.genfromtxt('potencial1.txt', skip_header = 1)

x_1 = data1[:,0]
y_1 = data1[:,1]

plt.plot(x_1,y_1, 'b')

data2= np.genfromtxt('potencial2.txt', skip_header = 1)

x_2 = data2[:,0]
y_2 = data2[:,1]

plt.plot(x_2,y_2, 'b')

data3= np.genfromtxt('potencial3.txt', skip_header = 1)

x_3 = data3[:,0]
y_3 = data3[:,1]

plt.plot(x_3,y_3, 'b')

data4= np.genfromtxt('potencial4.txt', skip_header = 1)

x_4 = data4[:,0]
y_4 = data4[:,1]

plt.plot(x_4,y_4, 'b')

data5= np.genfromtxt('potencial5.txt', skip_header = 1)

x_5 = data5[:,0]
y_5 = data5[:,1]

plt.plot(x_5,y_5, 'b')

plt.axis([1, 15, 0, 11])

plt.show()
