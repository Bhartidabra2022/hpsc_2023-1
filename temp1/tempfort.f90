program fortcube
implicit none 
real(kind=8)::f,g
f=2.
call fsub(f,g)
print*,"square root is",g
end program fortcube

subroutine fsub(x,s)
	implicit none
	real(kind=8), intent(in)::x
	real(kind=8),intent(out)::s
	real(kind=8)::s0,delta,tol
	integer::n
	s=1;
	tol=1.0e-14
	do n=1,100
	s0=s
	s=0.5*(s+x/s)
	delta=s-s0
	if(abs(delta/x))<tol
	break 
	end if
	end do
	
end subroutine fsub
