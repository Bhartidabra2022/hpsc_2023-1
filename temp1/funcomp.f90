program func
	use omp_lib
	implicit none
	real(kind=8),dimension(4)::y
	real(kind=8)::t,s
	real(kind=8),external::myfunc
	integer:: i,n,thread_num
	n=4
	t=2.0
	!$ call omp_set_num_threads(2)
	
	!$omp parallel do
	do i=1,n
		y(i)=myfunc(t,s)
		!$omp critical
		!$ thread_num=omp_get_thread_num()
		!$ print*,"thread=",thread_num
		print*,y(i)
		!$omp end critical
	enddo
	end program	
real(kind=8) function myfunc(x,s)
implicit none
	real(kind=8),intent(in):: x 
	real(kind=8),intent(out):: s
	real(kind=8):: z
	z=exp(x)
	s=z*cos(x)
	myfunc=s
	end function myfunc
	

