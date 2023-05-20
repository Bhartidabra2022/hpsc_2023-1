program finenorm
	use omp_lib
	implicit none
	real(kind=8),allocatable,dimension(:)::x,y
	real(kind=8)::norm_thread,norm
	integer:: i,thread_num,points,istart,iend,n
	norm=0.d0
	n=4
	allocate(x(n))
	allocate(y(n))
	do i=1,n
		x(i)=i
		enddo
	!$ call omp_set_num_threads(2)
	!$omp parallel private(i)
	
	!$omp do reduction(+:norm)
	do i=1,n
		norm=norm+abs(x(i))
	enddo
	!$omp barrier
	print*,"norm=",norm
	!$omp do
	do i=1,n
		y(i)=x(i)/norm
		
		!$omp critical
		print*,"x(i)=",x(i),"y(i)=",y(i)
		!$omp end critical
	enddo

	!$omp end parallel 
end program
