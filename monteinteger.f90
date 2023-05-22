!crude montecarlo
program monteintegration
	use omp_lib
	implicit none
	real(kind=8):: sum1,ans,y,a,b,r
	real(kind=8),external:: f
	integer::i,num_points,nthreads,inside
	character(len=12):: arg
	inside=0.d0
	sum1=0.d0
	a=3
	b=4
	call get_command_argument(1,arg)
	read(arg,*)  num_points
	
	call get_command_argument(2,arg)
	read(arg,*) nthreads
	inside=0.0
	!$ call omp_set_num_threads(nthreads)
	!$omp parallel do reduction(+:sum1) private(y)
	do i=1,num_points
		  call random_number(y)
		  y=a+y*(b-a)
		  !r=a+y*(b-a)
		  !if(y>=1 .or. y<=3) then
		  sum1=sum1+f(y)
		
		  !endif
		 enddo 
		!$omp barrier
	ans=(sum1*(b-a))/real (num_points)
	print*,"value=",ans
end program		   
		real(kind=8) function f(x)
		implicit none
		real(kind=8), intent(in)::x
		f=exp(x)
	end function f
