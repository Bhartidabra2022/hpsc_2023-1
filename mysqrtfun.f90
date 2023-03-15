program mysqrtfun
	implicit none 
	real(kind=8):: x,y,real_sqrt,s
	real(kind=8),external :: mysqrt
	x=2.d0
	y=mysqrt(x,s)
	real_sqrt= sqrt(x)
	print*, "my function sqyareroot", y
	print*, "real sqrt value is ", real_sqrt
	print*, "error=", y-real_sqrt
end program mysqrtfun
real(kind=8) function mysqrt(x,s)
	implicit none
	real(kind=8),intent(in):: x
	real(kind=8),intent(out):: s
	real(kind=8):: s0,tol,delta
	integer:: n
	s=1
	tol=1.0e-14
		do n=1,100
			s0=s
			s=0.5*(s+x/s)
			delta = s-s0
				if(abs(delta)/x .lt. tol)  then
					exit
				end if
		end do
		mysqrt=s
		
end function mysqrt
