"""use loop function and Vectorization to do some work"""

__author__ = 'Pu Zhao (pu.zhao@imperial.ac.uk)'
__version__ = '0.0.1'

# Average UK Rainfall (mm) for 1910 by month
# http://www.metoffice.gov.uk/climate/uk/datasets
rainfall = (('JAN', 111.4),
            ('FEB', 126.1),
            ('MAR', 49.9),
            ('APR', 95.3),
            ('MAY', 71.8),
            ('JUN', 70.2),
            ('JUL', 97.1),
            ('AUG', 140.2),
            ('SEP', 27.0),
            ('OCT', 89.4),
            ('NOV', 128.4),
            ('DEC', 142.2),
            )

# (1) Use a list comprehension to create a list of month,rainfall tuples where
# the amount of rain was greater than 100 mm.

rainfall_greater_than100 = [rain_amount[0] for rain_amount in rainfall if rain_amount[1] > 100]

# (2) Use a list comprehension to create a list of just month names where the
# amount of rain was less than 50 mm.

rainfall_less_than50 = [rain_amount[0] for rain_amount in rainfall if rain_amount[1] < 50]
# (3) Now do (1) and (2) using conventional loops (you can choose to do
# this before 1 and 2 !).

rainfall_greater_than100_forloop = []
rainfall_less_than50_forloop = []
for rain_amount in rainfall:
    if rain_amount[1] > 100:
        rainfall_greater_than100_forloop.append(rain_amount[0])
    elif rain_amount[1] < 50:
        rainfall_less_than50_forloop.append(rain_amount[0])

# A good example output is:
#
# Step #1:
# Months and rainfall values when the amount of rain was greater than 100mm:
# [('JAN', 111.4), ('FEB', 126.1), ('AUG', 140.2), ('NOV', 128.4), ('DEC', 142.2)]
# ... etc.

print("\nStep #1:\nMonths and rainfall values when the amount of rain was greater than 100mm:\n",
      rainfall_greater_than100,
      "\n\nStep #2:\nMonths and rainfall values when the amount of rain was less than 50mm:\n",
      rainfall_less_than50)
