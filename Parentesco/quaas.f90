implicit none
integer i,j,k,n,padre(100),madre(100)
real*8 A(0:100,0:100), C(0:100,0:100), f(0:100)

open(5,file='Carlos_II.txt')
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

print *,'A por el método tabular'
do i=20,n
  write (*,'(100f7.3)')(A(i,j),j=20,n)
enddo
print *,' '

!choleski
print *,'factor de cholesky de A'
C = 0.0d0
do i = 1, n
  !pivote
  C(i,i) = A(i,i)
  do j = 1, i-1
    C(i,i) = C(i,i) - C(i,j)**2
  enddo
  C(i,i) = dsqrt(C(i,i))
  !columna
  do j = i+1, n
    C(j,i) = A(j,i)
    do  k = 1,i-1
      C(j,i) = C(j,i)-C(i,k)*C(j,k)
    enddo
    C(j,i) = C(j,i) / C(i,i)
  enddo
enddo

do i=20,n
  write (*,'(100f7.3)')(C(i,j),j=20,i)
enddo
print *,' '

!Calculo de C con Henderson Quaas
C=0.0d0
do i = 1, n
   !elemento diagonal
   if (padre(i)*madre(i) > 0 ) then
      C(i,i) = dsqrt(0.5d0 - 0.25d0*(f(padre(i))+f(madre(i))))
   elseif (padre(i) > 0) then
      C(i,i) = dsqrt(0.75d0 - 0.25d0*(f(padre(i))))
   elseif (madre(i) > 0) then
      C(i,i) = dsqrt(0.75d0 - 0.25d0*(f(madre(i))))
   else
      C(i,i) = 1.0d0
   endif
   !debajo de la diagonal
   do j = i+1, n
     C(j,i) = 0.5d0 * (C(padre(j),i)+C(madre(j),i))
   enddo
   f(i) = 0.0d0
   do j = 1,i
     f(i) = f(i) + C(i,j)**2
   enddo
   f(i) = f(i) - 1.0d0
enddo


do i=20,n
  write (*,'(100f7.3)')(C(i,j),j=20,i)
enddo

write(*,'(100f8.4)')(A(i,i),i=1,n)

stop
end

     
     
     
     
     
     
     
     
     