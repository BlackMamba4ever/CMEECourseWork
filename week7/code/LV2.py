#!/usr/bin/env python3

"""Find the best alignment and its corresponding score in an input file"""

__author__ = 'Pu Zhao (pu.zhao@imperial.ac.uk)'
__version__ = '0.0.1'

import numpy as np
import scipy as sc
import scipy.integrate as integrate
import matplotlib.pylab as p
import sys


if __name__ == "__main__":
    r = float(sys.argv[1])
    a = float(sys.argv[2])
    z = float(sys.argv[3])
    e = float(sys.argv[4])
    K = float(sys.argv[5])

    t = np.linspace(0, 15, 1000)
    R0 = 10
    C0 = 5
    RC0 = np.array([R0, C0])


    def dCR_dt(pop, time=0):
        R = pop[0]
        C = pop[1]
        dRdt = r * R * (1 - K / R) - a * R * C
        dCdt = -z * C + e * a * R * C
        return np.array([dRdt, dCdt])


    pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)

    f1 = p.figure()
    p.plot(t, pops[:, 0], 'g-', label='Resource density')  # Plot
    p.plot(t, pops[:, 1], 'b-', label='Consumer density')
    p.grid()
    p.legend(loc='best')
    p.xlabel('time')
    p.ylabel('Population density')
    p.title('Consumer-Resource population dynamics')
    text = f'r={r}, a={a}, z={z}, e={e}, K={K}'
    p.text(0.72, 0.86, text, ha='center', va='center', fontsize=8.5, color='red', transform=f1.transFigure)
    p.show()  # To display the figure
    f1.savefig('../results/LV2.pdf')  # Save figure

    f2 = p.figure()
    p.plot(pops[:, 0], pops[:, 1], 'r-')  # Plot
    p.grid()
    p.xlabel('Resource density')
    p.ylabel('Consumer density')
    p.title('Consumer-Resource population dynamics')
    text = f'r={r}, a={a}, z={z}, e={e}, K={K}'
    p.text(0.72, 0.86, text, ha='center', va='center', fontsize=8.5, color='red', transform=f2.transFigure)
    p.show()  # To display the figure
    f2.savefig('../results/LV22.pdf')  # Save figure
    print()
    sys.exit(0)
