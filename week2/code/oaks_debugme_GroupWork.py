#!/usr/bin/env python3

"""Use functions find the oak species from a list of species"""

__author__ = 'Pu Zhao (pu.zhao23@imperial.ac.uk)'
__version__ = '0.0.1'

import csv
import sys
import ipdb
import doctest


# Define function
def is_an_oak(name):
    """Returns True if name is starts with 'quercus'.
    """
    return name.lower() == 'quercus'


def main(argv):
    f = open('../data/TestOaksData.csv', 'r')
    g = open('../results/OutputOaksData.csv', 'w')
    taxa = csv.reader(f)
    csv_write = csv.writer(g)
    csv_write.writerow(['Genus', 'Species'])
    oaks = set()
    if next(taxa) == ['Genus', 'Species']:
        next(taxa)
    for row in taxa:
        print(row)
        print("The genus is: ")
        print(row[0] + '\n')
        if is_an_oak(row[0]):
            print('FOUND AN OAK!\n')
            csv_write.writerow([row[0], row[1]])

    return 0


if __name__ == "__main__":
    status = main(sys.argv)
    sys.exit(status)
