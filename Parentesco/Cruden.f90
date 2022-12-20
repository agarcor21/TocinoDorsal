implicit none
integer i,j,igen,n,padre(500),madre(500)
real*8 Ah(0:500,0:500),Ap(0:500,0:500)
real*8 x1, Am
integer lunif

x1 = 0.8782938d+10
n = 500
Ah = 0.0d0
Ap = 0.0d0

do i = 1, n
  Ap(i,i) = 1.0d0
enddo
      
do igen= 1, 1000
  !simular apareamientos
  do i = 1, n
    padre(i) = lunif(x1,n/10)
    madre(i) = n/10+lunif(x1,9*n/10)
  enddo
  !calcular Ah
  do i = 1, n
    do j = 1, n
      Ah(i,j) = Ap(padre(i), padre(j)) + Ap(madre(i), madre(j)) 
      Ah(i,j) = Ah(i,j) + Ap(padre(i), madre(j)) + Ap(madre(i), padre(j))
      Ah(i,j) = Ah(i,j) / 4
    enddo
    Ah(i,i) = 1.0d0 + 0.5d0 * Ap(padre(i),madre(i))
  enddo
  !calcular promedio
  Am = 0.0d0
  do i=1,n
    do j=1,n
      Am = Am + Ah(i,j)
    enddo
  enddo
  Am = Am / (n*n)
  print *,igen, Am
  Ap = Ah
enddo

stop
end

     
!=================================================================
function lunif(x1,m)
!numero aleatorio entre 1 y m
real*8 x1,unif
integer lunif, m
x1 = mod (16807.0d0 * x1, 2147483647.0d0)
unif = x1/2147483647.0d0
lunif = unif * m + 1
return
end function lunif
!=================================================================
     
     
     
     
     