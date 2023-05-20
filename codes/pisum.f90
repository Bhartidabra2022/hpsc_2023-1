program pisum
	use omp_lib
	implicit none
	real(kind=8):: dx, psum, pi, x
	!real(kind=8) pisum
	integer::i,n
	n=1000
	dx=1.d0/n
	psum=0.d0
	!$ call omp_set_num_threads(2)
	
	!$omp parallel do reduction(+ : psum) & 
	!$omp     private(x)
	do i=1,n
	x=(i-1.d0/2.d0)*dx
	psum=psum+1.d0/(1.d0+x**2)
	enddo
	pi=4.d0*dx*psum
	print*,"pi=",pi
end program
