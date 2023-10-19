#!/usr/bin/env python3

"""Some functions exemplifying the use of control statements"""
# docstrings are considered part of the running code (normal comments are
# stripped). Hence, you can access your docstrings at run time.
__author__ = 'Samraat Pawar (s.pawar@imperial.ac.uk)'
__version__ = '0.0.1'

import sys


def foo_1(x):
    x = float(x)
    return x ** 0.5


def foo_2(x, y):
    x, y = float(x), float(y)
    if x > y:
        return x
    return y


def foo_3(x, y, z):
    x, y, z = float(x), float(y), float(z)
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
    x = int(x)
    result = 1
    for i in range(1, x + 1):
        result = result * i
    return result


def foo_5(x):  # a recursive function that calculates the factorial of x
    x = float(x)
    if x == 1:
        return 1
    return x * foo_5(x - 1)


def foo_6(x):  # Calculate the factorial of x in a different way; no if statement involved
    x = float(x)
    facto = 1
    while x >= 1:
        facto = facto * x
        x = x - 1
    return facto


def main(argv):
    if len(argv) == 1:
        print(foo_1(10))
        print(foo_2(10, 20))
        print(foo_3(10, 20, 30))
        print(foo_4(10))
        print(foo_5(10))
        print(foo_6(10))
    elif len(argv) == 2:
        print(foo_1(argv[1]))
        print(foo_2(argv[1], 20))
        print(foo_3(argv[1], 20, 30))
        print(foo_4(argv[1]))
        print(foo_5(argv[1]))
        print(foo_6(argv[1]))
    elif len(argv) == 3:
        print(foo_1(argv[1]))
        print(foo_2(argv[1], argv[2]))
        print(foo_3(argv[1], argv[2], 30))
        print(foo_4(argv[1]))
        print(foo_5(argv[1]))
        print(foo_6(argv[1]))
    elif len(argv) == 4:
        print(foo_1(argv[1]))
        print(foo_2(argv[1], argv[2]))
        print(foo_3(argv[1], argv[2], argv[3]))
        print(foo_4(argv[1]))
        print(foo_5(argv[1]))
        print(foo_6(argv[1]))
    else:
        print("Too many arguments!")
    return 0


if __name__ == "__main__":
    status = main(sys.argv)
    exit(status)
