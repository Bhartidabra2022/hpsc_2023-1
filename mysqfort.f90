program mysqfort
	implicit none
	real(kind=8)::j,k
	j=2
	call f(j,k)
	print*,"squareroot is",k
end program mysqfort


subroutine f(x,s)
	implicit none
	real(kind=8), intent(in)::x
	real(kind=8),intent(out)::s
	real::s0,tol,delta
	integer :: n
	s=1
	tol=1.0e-14
	do n=1,100
		s0=s
		s=0.5*(s+x/s)
		delta=s-s0
			if(abs(delta)/x .lt.tol) then
			exit
			end if
	end do
end subroutine f
	
