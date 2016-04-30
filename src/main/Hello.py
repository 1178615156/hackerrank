def hello():
    if True:
        return lambda x: 1
    else:
        return lambda x: 2


print(hello()(0))
