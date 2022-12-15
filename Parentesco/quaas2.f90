implicit none
integer i,j,k,n,padre(100),madre(100)
real*8 v(0:100), u(0:100)

open(5,file='Carlos_II.txt')
read(5,*)n
do i = 1, n
  read(5,'(i4,i5,i5)')j,padre(i),madre(i)
enddo
close(5)

!Calculo de C con Henderson Quaas
u = 0.0d0
v = 0.0d0
do i = 1, n
   v = 0.0d0
   !elemento diagonal
   if (padre(i)*madre(i) > 0 ) then
      v(i) = dsqrt(0.5d0 - 0.25d0*(u(padre(i))+u(madre(i))-2.0d0))
   elseif (padre(i) > 0) then
      v(i) = dsqrt(0.75d0 - 0.25d0*(u(padre(i))-1.0d0))
   elseif (madre(i) > 0) then
      v(i) = dsqrt(0.75d0 - 0.25d0*(u(madre(i))-1.0d0))
   else
      v(i) = 1.0d0
   endif
   !debajo de la diagonal
   do j = i+1, n
     v(j) = 0.5d0 * (v(padre(j))+v(madre(j)))
   enddo
   do j=i,n
     u(j) = u(j) + v(j)**2
   enddo
enddo

write(*,'(100f8.4)')(u(i),i=1,n)

stop
end

     
     
     
     
     
     
     
     
     