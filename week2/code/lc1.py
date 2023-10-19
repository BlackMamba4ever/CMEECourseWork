"""use loop function and Vectorization to create three different
lists containing the latin names, common names and mean body masses for each species in birds, respectively"""

__author__ = 'Pu Zhao (pu.zhao@imperial.ac.uk)'
__version__ = '0.0.1'

birds = (('Passerculus sandwichensis', 'Savannah sparrow', 18.7),
         ('Delichon urbica', 'House martin', 19),
         ('Junco phaeonotus', 'Yellow-eyed junco', 19.5),
         ('Junco hyemalis', 'Dark-eyed junco', 19.6),
         ('Tachycineata bicolor', 'Tree swallow', 20.2),
         )

# (1) Write three separate list comprehensions that create three different
# lists containing the latin names, common names and mean body masses for
# each species in birds, respectively.

latin_name = [bird[0] for bird in birds]
common_name = [bird[1] for bird in birds]
mean_mass = [bird[2] for bird in birds]

# (2) Now do the same using conventional loops (you can choose to do this
# before 1 !). 


latin_name_forloop = []
common_name_forloop = []
mean_mass_forloop = []
for bird in birds:
    latin_name_forloop.append(bird[0])
    common_name_forloop.append(bird[1])
    mean_mass_forloop.append(bird[2])

# A nice example out put is:
# Step #1:
# Latin names:
# ['Passerculus sandwichensis', 'Delichon urbica', 'Junco phaeonotus', 'Junco hyemalis', 'Tachycineata bicolor']
# ... etc.

print("\nStep #1:\nLatin names:\n", latin_name_forloop, "\n\nStep #2:\nCommon names:\n", common_name_forloop,
      "\n\nStep #3:\nMean body masses:\n", mean_mass_forloop)
