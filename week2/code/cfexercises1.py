#!/usr/bin/env python3

"""Some functions and make it like a "module" in Python"""

__author__ = 'Pu Zhao (pu.zhao@imperial.ac.uk)'
__version__ = '0.0.1'

import sys


def foo_1(x=10):
    x = float(x)
    return x ** 0.5


def foo_2(x=10, y=10):
    x, y = float(x), float(y)
    if x > y:
        return x
    return y


def foo_3(x=30, y=20, z=10):
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


def foo_4(x=10):
    x = int(x)
    result = 1
    for i in range(1, x + 1):
        result = result * i
    return result


def foo_5(x=10):  # a recursive function that calculates the factorial of x
    x = int(x)
    if x == 1:
        return 1
    return x * foo_5(x - 1)


def foo_6(x=10):  # Calculate the factorial of x in a different way; no if statement involved
    x = float(x)
    facto = 1
    while x >= 1:
        facto = facto * x
        x = x - 1
    return facto


def main(argv):
    if len(argv) == 1:
        print(foo_1())
        print(foo_2())
        print(foo_3())
        print(foo_4())
        print(foo_5())
        print(foo_6())
    elif len(argv) == 2:
        print(foo_1(argv[1]))
        print(foo_2(argv[1]))
        print(foo_3(argv[1]))
        print(foo_4(argv[1]))
        print(foo_5(argv[1]))
        print(foo_6(argv[1]))
    elif len(argv) == 3:
        print(foo_1(argv[1]))
        print(foo_2(argv[1], argv[2]))
        print(foo_3(argv[1], argv[2]))
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
        print("Wrong input!")
    return 0


if __name__ == "__main__":
    status = main(sys.argv)
    exit(status)
