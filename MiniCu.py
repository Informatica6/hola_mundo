import matplotlib.pyplot as plt
import numpy as np


data= np.genfromtxt('error.txt', skip_header = 1)

x_prueba = data[:,0]
y_prueba = data[:,1]

plt.plot(x_prueba,y_prueba, 'k+')

coef_1 = np.genfromtxt('coeficentes.txt')
coef_2 = np.genfromtxt('coeficentes.txt')
coef_3 = np.genfromtxt('coeficentes.txt')
coef_10 = np.genfromtxt('coeficentes.txt')

coefs = [coef_1, coef_2, coef_3, coef_10]

colores = ['b', 'g', 'r', 'm']

for i in range(4):
    l = len(coefs[i])

    for j in range(int(l/2)):
        temp = coefs[i][j]
        coefs[i][j] = coefs[i][l-1-j]
        coefs[i][l-1-j] = temp

    f = np.poly1d(coefs[i])
    y_new = f(x_prueba)
    plt.plot(x_prueba, y_new, color = colores[i])

plt.show()
