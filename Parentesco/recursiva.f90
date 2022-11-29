implicit none
integer i,j,n,padre(100),madre(100)
common/p/padre,madre
real*8 A

open(5,file='Comet.txt')
read(5,*)n
do i = 1, n
  read(5,'(i4,i5,i5)')j,padre(i),madre(i)
enddo
close(5)
print *,A(12,12)
do i=1,n
  write (*,'(100f10.5)')(A(i,j),j=1,n)
enddo

stop
end

recursive function A(i,j) result(x)
integer i,j,n,padre(100),madre(100)
real*8 x
common/p/padre,madre
if (i == 0) then
  x = 0.0d0
else if (i > j) then
  x = A(j,i)
else if (i < j) then
  x = 0.5d0 * (A(i,padre(j)) + A(i,madre(j)))
else 
  x = 1.0d0 + 0.5d0 * (A(padre(i),madre(i)))
  !x = 0.5d0 + 0.5d0 * (A(padre(i),madre(i))) !para coancestrias
end if
return
end

     
     
     
     
     
     
     
     