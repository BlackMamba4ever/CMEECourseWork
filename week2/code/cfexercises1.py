#!/usr/bin/env python3

"""Some functions exemplifying the use of control statements"""
# docstrings are considered part of the running code (normal comments are
# stripped). Hence, you can access your docstrings at run time.
__author__ = 'Samraat Pawar (s.pawar@imperial.ac.uk)'
__version__ = '0.0.1'

import sys


def foo_1(x):
    return x ** 0.5


def foo_2(x, y):
    if x > y:
        return x
    return y


def foo_3(x, y, z):
    if x > y:
        tmp = y
        y = x
        x = tmp
    if y > z:
        tmp = z
        z = y
        y = tmp
    return [x, y, z]


def foo_4(x):
    result = 1
    for i in range(1, x + 1):
        result = result * i
    return result


def foo_5(x):  # a recursive function that calculates the factorial of x
    if x == 1:
        return 1
    return x * foo_5(x - 1)


def foo_6(x):  # Calculate the factorial of x in a different way; no if statement involved
    facto = 1
    while x >= 1:
        facto = facto * x
        x = x - 1
    return facto


print(foo_5(10))

if __name__ == "__main__":
    sys.exit(0)
