taxa = [('Myotis lucifugus', 'Chiroptera'),
        ('Gerbillus henleyi', 'Rodentia',),
        ('Peromyscus crinitus', 'Rodentia'),
        ('Mus domesticus', 'Rodentia'),
        ('Cleithrionomys rutilus', 'Rodentia'),
        ('Microgale dobsoni', 'Afrosoricida'),
        ('Microgale talazaci', 'Afrosoricida'),
        ('Lyacon pictus', 'Carnivora'),
        ('Arctocephalus gazella', 'Carnivora'),
        ('Canis lupus', 'Carnivora'),
        ]

# Write a python script to populate a dictionary called taxa_dic derived from
# taxa so that it maps order names to sets of taxa and prints it to screen.
# 
# An example output is:
#  
# 'Chiroptera' : set(['Myotis lucifugus']) ... etc. 
# OR, 
# 'Chiroptera': {'Myotis  lucifugus'} ... etc

#### Your solution here #### 
dir_taxa = {}
for ele in taxa:
    if ele[1] in dir_taxa:
        dir_taxa[ele[1]].add(ele[0])
    else:
        dir_taxa[ele[1]] = {ele[0]}
# print(dir_taxa)
# Now write a list comprehension that does the same (including the printing after the dictionary has been created)

dir_taxa_c = {ele[1]: {ele2[0] for ele2 in taxa if ele2[1] == ele[1]} for ele in taxa}

print(dir_taxa_c)
#### Your solution here ####
