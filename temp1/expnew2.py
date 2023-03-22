import numpy as np
from numpy import math
def f(x):
	kmax=100
	tol=1.0e-14
	s=0
	for k in range (0,kmax):
		t=s
		s=s+x**k/math.factorial(k)
		if(abs(s-t)<tol):
			break
	return s	
y=f(2)
n=math.factorial(0)
print("exp(2) is  %12.5f" %(y))
# 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 (token)
