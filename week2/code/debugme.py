import ipdb


def buggyfunc(x):
    y = x
    for i in range(x):
        try:
            y = y - 1
            z = x / y
            print("set_trace")
            ipdb.set_trace()
            print(f"The result of dividing a number by zero is undefined")
        except:
            print(f"This didn't work;{x = }; {y = }")
        else:
            print(f"OK; {x = }; {y = }, {z = };")
    return z


buggyfunc(20)
