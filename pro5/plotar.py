from matplotlib import pyplot as plt
import numpy as np

r=2

def g(x):
    return r*x*(1-x)

x= np.linspace(0,1,50)
y= g(x)



plt.ylim(0,1)
plt.xlim(0,1)
plt.grid(True)
plt.plot(x, y)
plt.show()