#cbrt() function
import numpy as nan
def cuberoot(x):
	if x==0:
  		return 0
	elif x<0:
		print("Can not find cuberoot")
		return nan
	else:
		return nan.cbrt(x)
		
		
x=float(input("Enter the number="))
print("The cuberoot is" ,cuberoot(x))
 
 
