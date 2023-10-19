#!/usr/bin/env python3

"""write a file"""

__author__ = 'Pu Zhao (pu.zhao@imperial.ac.uk)'
__version__ = '0.0.1'

#############################
# FILE OUTPUT
#############################
# Save the elements of a list to a file
list_to_save = range(100)

f = open('../sandbox/testout.txt', 'w')
for i in list_to_save:
    f.write(str(i) + '\n')  # Add a new line at the end

f.close()
