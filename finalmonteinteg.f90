!acceptance and rejection montecarlo
program monteintegration
	use omp_lib
	implicit none
	real(kind=8):: sum1,ans,y,a,b,r,x  
	real(kind=8),external:: f
	integer::i,num_points,nthreads,inside,count
	character(len=12):: arg
	inside=0
	sum1=0.d0
	a=2
	b=6
	count=0
	call get_command_argument(1,arg)
	read(arg,*)  num_points
	
	call get_command_argument(2,arg)
	read(arg,*) nthreads

	!$ call omp_set_num_threads(nthreads)
	!$omp parallel do reduction(+:count) private(x,y)
	do i=1,num_points
		  call random_number(y)
		  y=f(b)*y
		  call random_number(x)
		  x=a+(b-a)*x
		 if(y .lt. f(x)) then
		  count=count+1;
		  endif
		 enddo 
		!$omp barrier
		print*,"inside=",count
	ans=((b-a)*f(b)*count)/real (num_points)
	print*,"value=",ans
end program		   
		real(kind=8) function f(x)
		implicit none
		real(kind=8), intent(in)::x
		f=exp(x)
	end function f
