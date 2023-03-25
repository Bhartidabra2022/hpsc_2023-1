program arrayfort
	implicit none
	real(kind=8),dimension(3)::j,k
	integer ::n
	j=(/2.,3.,4./)
	n=size(j)
	call fsub(j,n,k)
	print*,"x=",k
end program arrayfort

subroutine fsub(x,n,y)
	implicit none
	real(kind=8), dimension(n), intent(in)::x
	real(kind=8),dimension(n),intent(out) ::y
	integer,intent(in)::n
	y=x**3
end subroutine fsub
