program yeval
	use omp_lib
	implicit none
	real(kind=8),allocatable,dimension(:):: u,y,eta,c
	integer:: n,i,thread_num,m,nsteps,istart,iend,points_per_thread,nthreads
	real(kind=8)::dt 
	allocate(u(n))
	allocate(eta(n))
	allocate(c(n))	 !coarse grained
	nthreads=2
	!$ call omp_set_num_threads(nthreads) 
	nsteps=3
	n=4
	dt=0.2
	points_per_thread=n/nthreads
	
	
	
	!$omp parallel private(istart,iend,i,thread_num)
	thread_num=0
	!$ thread_num=omp_get_thread_num()
	istart= thread_num*points_per_thread+1
	iend =min((thread_num+1)*points_per_thread,n)
	print*,"istart=",istart,"iend=",iend
	do i=istart,iend
	u(i)=i+1
	c(i)=i
	enddo
	
	!$omp do 
	do i=istart,iend
		do m=1,nsteps
		u(i)=u(i)*(1.d0+dt*c(i))
		enddo
		!$omp critical
		!$ print*,"thread=",thread_num
		print*,"i=",i,"step=",m,u(i)
		!$omp end critical
		
		
		
		enddo
		!$omp barrier
	!$omp end parallel  
end program 
