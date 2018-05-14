import matplotlib.pyplot as plt #utilizacion de matplotlip y su abreviacion
import numpy as np #utilizacion de numpy y su abreviacion

#leemos los valores desde un archivo .txt

data= np.genfromtxt('juego.txt')

S = data[:,0]
Z = data[:,1]
R = data[:,2]
I = data[:,3]
T = data[:,4] 

fig, ax = plt.subplots()
ax.plot(T, S, 'b', label='Supervivientes')
ax.plot(T, Z, 'g', label='Zombies')
ax.plot(T, I, 'y', label='Infectados')
ax.plot(T,R, 'r', label='Muertos')

ax.set_title('Apocalipsis Zombie')
ax.set_xlabel('Meses')
ax.set_ylabel('Número de individuos')
legend = ax.legend(loc='upper right', shadow=True, fontsize='x-large')

legend.get_frame().set_facecolor('#E6E1E0')

plt.show()
