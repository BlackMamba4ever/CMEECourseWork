#!/usr/bin/env python3

"""Find the best alignment and its corresponding score in an input file"""

__author__ = 'Pu Zhao (pu.zhao@imperial.ac.uk)'
__version__ = '0.0.1'

import numpy as np
import scipy as sc
import scipy.integrate as integrate
import matplotlib.pylab as p

m = np.identity(4)  # create an identity matrix
m

m.fill(16)  # fill the matrix with 16
m

mm = np.arange(1, 17)
mm = mm.reshape(4, 4)  # Convert to matrix
mm
mm.transpose()

mm = np.matrix(mm)  # convert to scipy/numpy matrix class
mm

##############

sc.stats.norm.rvs(size=10)
np.random.normal(size=10)
np.random.seed(1234)
sc.stats.norm.rvs(size=10)
sc.stats.norm.rvs(size=5, random_state=1234)
sc.stats.randint.rvs(0, 10, size=7)
sc.stats.randint.rvs(0, 10, size=7, random_state=1234)
sc.stats.randint.rvs(0, 10, size=7, random_state=3445)  # a different seed

###########################

y = np.array([5, 20, 18, 19, 18, 7, 4])  # The y values; can also use a python list here
p.plot(y)
area = integrate.trapz(y, dx=2)
print("area =", area)

area = integrate.trapz(y, dx=1)
print("area =", area)

area = integrate.trapz(y, dx=3)
print("area =", area)

area = integrate.simps(y, dx=2)
print("area =", area)

area = integrate.simps(y, dx=1)
print("area =", area)

area = integrate.simps(y, dx=3)
print("area =", area)


###########################

def dCR_dt(pops, t=0):
    R = pops[0]
    C = pops[1]
    dRdt = r * R - a * R * C
    dCdt = -z * C + e * a * R * C

    return np.array([dRdt, dCdt])


type(dCR_dt)

r = 1.
a = 0.1
z = 1.5
e = 0.75
t = np.linspace(0, 15, 1000)
R0 = 10
C0 = 5
RC0 = np.array([R0, C0])

pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)
pops
infodict
type(infodict)
infodict.keys()
infodict['message']

f1 = p.figure()
p.plot(t, pops[:, 0], 'g-', label='Resource density')  # Plot
p.plot(t, pops[:, 1], 'b-', label='Consumer density')
p.grid()
p.legend(loc='best')
p.xlabel('Time')
p.ylabel('Population density')
p.title('Consumer-Resource population dynamics')
p.show()  # To display the figure

f1.savefig('../results/LV_model.pdf')  # Save figure


def loop_product(a, b):
    N = len(a)
    c = np.zeros(N)
    for i in range(N):
        c[i] = a[i] * b[i]
    return c


def vect_product(a, b):
    return np.multiply(a, b)


import timeit

array_lengths = [1, 100, 10000, 1000000, 10000000]
t_loop = []
t_vect = []

for N in array_lengths:
    print(f"\nSet {N=}")
    # randomly generate our 1D arrays of length N
    a = np.random.rand(N)
    b = np.random.rand(N)

    # time loop_product 3 times and save the mean execution time.
    timer = timeit.repeat('loop_product(a, b)', globals=globals().copy(), number=3)
    t_loop.append(1000 * np.mean(timer))
    print(f"Loop method took {t_loop[-1]} ms on average.")

    # time vect_product 3 times and save the mean execution time.
    timer = timeit.repeat('vect_product(a, b)', globals=globals().copy(), number=3)
    t_vect.append(1000 * np.mean(timer))
    print(f"vectorized method took {t_vect[-1]} ms on average.")

p.figure()
p.plot(array_lengths, t_loop, label="loop method")
p.plot(array_lengths, t_vect, label="vect method")
p.xlabel("Array length")
p.ylabel("Execution time (ms)")
p.legend()
p.show()

N = 1000000000

a = np.random.rand(N)
b = np.random.rand(N)
c = vect_product(a, b)

# if no error, remove a, b, c from memory.
del a
del b
del c
import numpy as np

a = np.array([10, 5])
