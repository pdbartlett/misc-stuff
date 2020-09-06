import math

import matplotlib.pyplot as plt
import numpy as np

from matplotlib import cm

print(np.array([1,3,2,5]))

x = np.array([1,6,2])
y = np.array([1,4,3])

print(x, x.shape, x.size)
print(y, x.shape, x.size)
print(x + y)

m = np.array([[1, 2], [3, 4]])
print(m)
print(m**(0.5))

x = np.random.normal(size=50)
y = x + np.random.normal(size=50, loc=50, scale=0.1)
r = np.corrcoef(x, y)[0][1]
print(r)
a, b = np.polyfit(x, y, deg=1)
y_est = a * x + b
plt.plot(x, y, 'r+')
plt.plot(x, y_est)
plt.show()

x = np.arange(1, 11)
print(x)

a = np.linspace(-math.pi, math.pi)
X, Y = np.meshgrid(a, a)
Z = np.cos(Y) / (1 + X**2)
plt.contour(X, Y, Z)
plt.show()
plt.contour(X, Y, Z, 45)
plt.show()

ZA = (Z - np.transpose(Z)) / 2
plt.contour(X, Y, ZA, 15)
plt.show()
plt.imshow(ZA)
plt.show()

fig = plt.figure()
ax = fig.gca(projection='3d')
surf = ax.plot_surface(X, Y, ZA, cmap=cm.coolwarm, lw=0, antialiased=False)
plt.show()

A = np.transpose(np.arange(1, 17).reshape(4,4))
print(A)
print(A[1,2])
print(A[(0,2),:][:,(1,3)])
print(A[0:3,:][:,1:])
print(A[:2])
print(A[:,:2])
print(A[0])
