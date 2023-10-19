#!/usr/bin/env python3

"""Debug and test function which is used to check whether the name is an oak."""

__author__ = 'Pu Zhao (pu.zhao@imperial.ac.uk)'
__version__ = '0.0.1'

import csv
import sys
import ipdb
import doctest


# Define function
def is_an_oak(name):
    """Returns True if name is starts with 'quercus'.

    >>> is_an_oak('quercus')
    True
    >>> is_an_oak('Fagus sylvatica')
    False
    >>> is_an_oak('Quercus')
    True
    >>> is_an_oak('quercuss')
    False
    >>> is_an_oak('Quercuss')
    False

    """
    # ipdb.set_trace()
    return name.lower() == 'quercus'


def main(argv):
    f = open('../data/TestOaksData.csv', 'r')
    g = open('../data/JustOaksData.csv', 'w')
    taxa = csv.reader(f)
    csv_write = csv.writer(g)
    oaks = set()
    for row in taxa:
        print(row)
        print("The genus is: ")
        print(row[0] + '\n')
        # ipdb.set_trace()
        if is_an_oak(row[0]):
            print('FOUND AN OAK!\n')
            csv_write.writerow([row[0], row[1]])

    return 0


if __name__ == "__main__":
    status = main(sys.argv)

doctest.testmod()
