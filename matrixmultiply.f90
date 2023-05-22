program rough
use omp_lib
implicit none
real(kind=8),allocatable,dimension(:, :) :: x,a,b
integer::i,j,thread_num,m,n,k
m=3
n=3
allocate(a(m,n))
allocate(b(m,n))
allocate(x(m,n))
do j=1,m
do i=1,n
	a(i,j)=1.d0
	b(i,j)=1.d0
enddo
enddo

do j=1,m
!do i=1,n
	write(*,*) (a(i,j),i=1,m)
	    
	write(*,*) (b(i,j),i=1,n)
!enddo
enddo

!$ call omp_set_num_threads(3)

!$omp parallel do private(i,k)

do j=1,m
do i=1,n
	x(i,j)=0.d0
do k=1,m
	x(i,j)=x(i,j)+a(i,k)*b(k,j)
enddo
       !$omp critical
	!$ thread_num = omp_get_thread_num()
		!$ print*,"this thread = ",thread_num
	print*,x(i,j)
	!$omp end critical 
enddo
enddo
!$omp end parallel do
end program 
