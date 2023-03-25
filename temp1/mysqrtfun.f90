! mysqrt via function

program mysqrtfun
	implicit none
	real(kind=8) :: k,j,s
	real(kind=8), external:: fun
	k= fun(2.d0,s)
	j=sqrt(2.d0)
	print*, "real,mysqrt",j,k
end program mysqrtfun

real(kind=8) function fun(x,s)
	implicit none
	real(kind=8),intent(in):: x
	real(kind=8),intent(out) ::s
	integer :: kmax,i
	real(kind=8) :: s0,tol
	logical :: debug
	debug=.false.
	if(x==0) then 
		s=0
	end if
	s=1.
	tol=1.0e-14
	kmax=100
	do i=1,kmax
	if (debug) then
		print*,"at iteration the value is",i,s
		end if
		s0=s
		s=0.5*(s+x/s)
		if(abs(s-s0)/x<tol) then
			exit
		end if
	enddo 
	fun=s
end function fun
		
