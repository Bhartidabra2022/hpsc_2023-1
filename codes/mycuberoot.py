def cubert(x,s=1.,debug=False,specialCases=True):
	"""
	The actual cubert function
	Inputs
	x: the number whose square root is to be calculated
	debug: True if iteration details need to be printed. Default value is False
	specialCases: False to disable zero and negative number check. Default value is True

	"""
	#from numpy import nan
	print("You can also pass the intial guess along with value by default initial guess is 1: "
		"Second value will be initial guess.")
	if specialCases:
		if x==0.:
			return 0.
		"""elif x<0.:
			print("An error has occured since you have passed a negative value for"
				"x which does not have a real square root")
			return nan
	assert x>0., "Input not recognised"	"""

	kmax=100
	tol=1.0e-14
	for k in range(kmax):
		if debug:
			print("At iteration number %s, s= %20.15f" %(k,s))
		s0=s
		s = (1./3.)*(2*s+x/(s**2))
		delta_s=s-s0
		if(abs(delta_s/x))<tol:
			break
	if debug:
		print("After %s iterations,  s=%20.15f" %(k+1,s))
	return s

def test_main():
	from numpy import cbrt
	xvalues=[0., 1., -1000, 1.e6]
	for x in xvalues:
		print("Testing with x=%20.15e" %x)
		s=cubert(x,s=1.)
		s_numpy=cbrt(x)
		print("cubert s = %20.15e, numpy s = %20.15e" %(s,s_numpy))
		assert abs(s-s_numpy) < 1e-14, "Your  cuberoot does not agree with numpy cbrt"
		
