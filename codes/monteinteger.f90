program monteintegration
	use omp_lib
	implicit none
	real(kind=8):: sum1,ans,y
	real(kind=8),external:: f
	integer::i,num_points,nthreads,inside
	character(len=12):: arg
	inside=0.d0
	sum1=0.d0
	call get_command_argument(1,arg)
	read(arg,*)  num_points
	
	call get_command_argument(2,arg)
	read(arg,*) nthreads
	
	!$ call omp_set_num_threads(nthreads)
	!$omp parallel do reduction(+:sum1)
	do i=1,num_points
		  call random_number(y)
		  y=y+1
		  if(y<=2) then
		 sum1=sum1+f(abs(y))
		 endif
		enddo 
		!$omp barrier
	ans=sum1/num_points
	print*,"value=",ans
end program		   
		real(kind=8) function f(x)
		implicit none
		real(kind=8), intent(in)::x
		f=x*x
	end function f
