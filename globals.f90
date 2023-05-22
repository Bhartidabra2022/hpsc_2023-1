module globals
	implicit none
	save
	integer ::count
	real(kind=8):: z
end module globals


program yeval
	use omp_lib
	implicit none
	real(kind=8),allocatable,dimension(:):: a,y
	
	real(kind=8),external::myfcn
	integer:: n,i,thread_num
	n=5
	allocate(y(n))
	allocate(a(n))
	
	do i=1,n
	a(i)=i
	enddo
	!$ call omp_set_num_threads(4) 
	
	!$omp parallel do 
	do i=1,n
		y(i)=myfcn(a(i))
		!$omp critical
		!$ thread_num=omp_get_thread_num()
		!$ print*,"thread=",thread_num
		print*,y(i)
		!$omp end critical
		enddo
	!$omp end parallel do    
end program 

real(kind=8) function myfcn(x)
	implicit none
	real(kind=8), intent(in):: x
	use globals
	count=count+1
	z=exp(x)
	myfcn=z*cos(x)
end function myfcn


