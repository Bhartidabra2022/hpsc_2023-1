program montellel
	use omp_lib
	implicit none
	real(kind=8)::x,y,estimated_pi
	integer ::inside,num_threads,num_points,i,num_thread,j
	character(len=32)::arg
	inside=0.d0
	
	call get_command_argument(1,arg)
	read(arg,*) num_points
	
	call get_command_argument(2,arg)   !with changing number of threads
	read(arg,*) num_thread
	
	!$ call omp_set_num_threads(num_thread)
	                                  
	!$omp parallel do reduction(+:inside)
	do j=0,num_thread
	do i=1,num_points
		call random_number(x)
		x=x-0.5
		call random_number(y)
		y=y-0.5
	
		if(x*x+y*y .le. 0.25) then
		inside=inside+1
		endif
		enddo
		!$omp critical
		!$ num_thread=omp_get_thread_num()
		!$ print*,"thread=",num_thread
			
	print*,"total number points=",num_points,"inside the circle =",inside
	
	estimated_pi=(inside*4.d0)/num_points
	print*,"my pi=",estimated_pi
	inside=0
	!$omp end critical
		enddo
	
	
	!$omp end parallel do
end program
