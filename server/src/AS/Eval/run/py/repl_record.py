
def f(x):
	return x * 4



def myFunc(x):
	return f(f(x))

a = 6
b = 10
c = myFunc(a+b)
