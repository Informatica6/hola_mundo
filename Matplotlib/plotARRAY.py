import numpy as np
import matplotlib.pyplot as plt
import scipy.interpolate

N = 100 #number of points for plotting/interpolation

x, y, z = np.genfromtxt(r'file.dat', unpack=True)


xi = np.linspace(x.min(), x.max(), N)
yi = np.linspace(y.min(), y.max(), N)
zi = scipy.interpolate.griddata((x, y), z, (xi[None,:], yi[:,None]), method='nearest')

fig = plt.figure()
plt.contourf(xi,yi,zi)
plt.xlabel("X")
plt.ylabel("Y")
plt.show()

