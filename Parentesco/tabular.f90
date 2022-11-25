implicit none
integer i,j,n,padre(100),madre(100)
real*8 A(0:100,0:100)

open(5,file='Comet.txt')
read(5,*)n
do i = 1, n
  read(5,'(i4,i5,i5)')j,padre(i),madre(i)
enddo
close(5)

!relaciones geneticas aditivas
A = 0.0d0
do i = 1, n
  A(i,i) = 1.0d0 + 0.5d0 * A(padre(i),madre(i))
  do  j = i+1, n
    A(i,j) = .5d0 *(A(i,padre(j))+A(i,madre(j)))
    A(j,i) = A(i,j)
  enddo
enddo
print *,' '


do i=1,n
  write (*,'(100f10.5)')(A(i,j),j=1,n)
enddo
!coancestrias
A = 0.0d0
do i = 1, n
  A(i,i) = 0.5d0 + 0.5d0 * A(padre(i),madre(i))
  do  j = i+1, n
    A(i,j) = .5d0 *(A(i,padre(j))+A(i,madre(j)))
    A(j,i) = A(i,j)
  enddo
enddo
print *,' '


do i=1,n
  write (*,'(100f10.5)')(A(i,j),j=1,n)
enddo

stop
end

     
     
     
     
     
     
     
     
     
