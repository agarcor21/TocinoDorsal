         integer n, dosn
         parameter (n = 12, dosn =2*n, ngen = 20)
         real*8 T(0:dosn,0:dosn), x(0:dosn,0:ngen), facto
         open(46,file='deriva.txt')
         do i=0,dosn
           do j=0,dosn
           T(i,j)=facto(dosn,j)*(dble(i)/dosn)**j*(1.0d0-dble(i)/dosn)**(dosn-j)
           enddo
         enddo
         x=0.0d0
         x(16,0) = 1.0d0
         do igen=1,ngen
           call multip(igen,x,T)
           write(46,'(50f7.4)')(x(i,igen),i=0,dosn)
         enddo
         close(46)
         stop
         end

         subroutine multip(igen,x,T)
         integer dosn
         parameter (n = 12, dosn =2*n, ngen = 20)
         real*8 T(0:dosn,0:dosn), x(0:dosn,0:ngen)
         !multiplicar x -- x*T
         do i = 0,dosn
           x(i,igen) = 0.0d0
           do j = 0,dosn
             x(i,igen) = x(i,igen) + x(j,igen-1) * T(j,i)
           end do
         end do
         return
         end
      
         function facto(i,j)
         integer i,j,k,j2
         real*8 facto
         if (i.lt.j) then
           print *,'error en bin'
           stop
         end if
         j2 = i - j
         if (j .ge. j2) then
           facto = 1.0d0
           do k = i, j+1,-1
             facto = facto * k
           end do
           do k = 1, j2
             facto = facto / k
           end do
         else
           facto = 1.0d0
           do k = i, j2+1,-1
             facto = facto * k
           end do
           do k = 1, j
             facto = facto / k
           end do
         end if
         return
         end