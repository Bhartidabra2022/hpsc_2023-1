program arraypasssub
	implicit none
	real(kind=8),dimension(4)::x
	integer ::i,n
	n=4
	call pass(x)
	do i=1,n
		print*,x(i)
	end do
		
end program arraypasssub

subroutine pass(x)
	implicit none 
	real(kind=8),dimension(n),intent(inout)::x
	integer :: i,n
	n=4
	do i=1,n
		print*, "enter the element="
		read*, x(i)
	end do
end subroutine pass
