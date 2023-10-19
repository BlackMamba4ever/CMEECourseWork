#!/usr/bin/env python3

"""pickle package and unpickle"""

__author__ = 'Pu Zhao (pu.zhao@imperial.ac.uk)'
__version__ = '0.0.1'

import pickle

#############################
# STORING OBJECTS
#############################
# To save an object (even complex) for later use
my_dictionary = {"a key": 10, "another key": 11}

f = open('../sandbox/testp.p', 'wb')  # note the b: accept binary files
pickle.dump(my_dictionary, f)
f.close()

# Load the data again
f = open('../sandbox/testp.p', 'rb')
another_dictionary = pickle.load(f)
f.close()

print(another_dictionary)
