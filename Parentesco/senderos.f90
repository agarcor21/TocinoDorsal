implicit none
integer n,padre(100),madre(100)
character*20 nombre(100)
integer temp(10)      !marca padre o madre para ir revisando todos los senderos
integer sendero(10)   !maximo numero de generaciones
integer i,j,k,ii,jj,kk,i1,i2,maxgen 
integer itemp
integer senderos(1000000,10)  !sendero, individuos, i o j
integer long_senderos(1000000)
integer nsenderos, valido, nvalidos(100)

open(15,file='senderos.txt')
open(5,file='Setter.txt')
read(5,*)n
do i = 1, n
  read(5,'(i4,i5,i5,5x,a15)')j,padre(i),madre(i),nombre(i)
enddo

!calcular el parentesco de i1 con i2
i1 = 19
i2 = 20
maxgen = 6  !sendero mas largo
nvalidos = 0

!senderos a cada uno de los animales anteriores

do ii = 1, 18
   write(15,*)'senderos validos desde ',nombre(ii)
   temp = 1
   nsenderos = 0
   senderos = 0
   !buscar si temp conduce a al individuo ii
   do jj = 1, 9999999
     itemp = i1 
     sendero(1) = i1
     do i = 1, maxgen
       if (temp(i) == 1) then
          itemp = padre(itemp)
       else
          itemp = madre(itemp)
       end if
       sendero(i+1) = itemp
       if (itemp == ii)then !sendero valido)
          !mirar si coincide con el anterior
          do j = 1, i+1
            if (sendero(j) .ne. senderos(nsenderos,j)) then
              nsenderos = nsenderos + 1
              do kk = 1, i+1
                 senderos(nsenderos,kk) = sendero (kk)
              enddo
              long_senderos(nsenderos) = i 
              !print *,long_senderos(nsenderos),' sendero ', nombre(sendero(1:i+1))
              goto 7
            end if
          end do
       end if
       
       if (itemp == 0) exit
     enddo
7    continue     
     call next(temp, maxgen)
   enddo
   
   
   !print *,'senderos del ',nombre(ii), 'a ',nombre(i2)
   temp = 1
   !buscar si temp conduce a al individuo ii
   do jj = 1, 9999999
     itemp = i2 
     sendero (1) = i2
     do i = 1, maxgen
       if (temp(i) == 1) then
          itemp = padre(itemp)
       else
          itemp = madre(itemp)
       end if
       sendero(i+1) = itemp
       if (itemp == ii)then !sendero valido
          !mirar si coincide con el anterior
          do j = 1, i+1
            if (sendero(j) .ne. senderos(nsenderos,j)) then
              nsenderos = nsenderos + 1
              do kk = 1, i+1
                 senderos(nsenderos,kk) = sendero (kk)
              enddo
              long_senderos(nsenderos) = i 
              !print *,long_senderos(nsenderos),' sendero ', nombre(sendero(1:i+1))
              goto 8
            end if
          end do
       end if
       
       if (itemp == 0) exit
     enddo
8    continue     
     call next(temp, maxgen)
   enddo
   !print *,'nsenderos = ',nsenderos
   
   !buscar parejas de senderos validos
   do i = 1, nsenderos
     do j = i, nsenderos
        valido = 1
        !mirar si hay algun bicho comun
        do jj = 1, long_senderos(i)
          do kk = 1, long_senderos(j)
            if (senderos(i,jj) == senderos(j,kk)) then !sendero con bichos comunes
              valido = 0
            end if
          enddo
        enddo
        if (valido == 1) then
          nvalidos(ii) = nvalidos(ii) + 1
          do k = 1, long_senderos(i)
            write(15,'(2a)',advance='no') trim(nombre(senderos(i,k))),'-'
          enddo
          write(15,'(3a)',advance='no')' /',trim(nombre(senderos(i,long_senderos(i)+1))),'/ '
          do k = long_senderos(j), 1, -1
            write(15,'(2a)',advance='no') '-',trim(nombre(senderos(j,k)))
          enddo
          write(15,*)
        end if
     enddo
   enddo
   
enddo
do ii=1,18
  write (15,*) 'desde ',nombre(ii),' --> ',nvalidos(ii),' senderos validos'
enddo
write(15,*)'TOTAL = ',sum(nvalidos) 
stop
end

subroutine next(temp,maxgen)
integer temp(10),maxgen,i
do i = maxgen,1,-1
  if(temp(i) == 1) then
     temp(i) = 2
     do j = i+1,maxgen
       temp(j) = 1
     enddo
     return
   endif
enddo
return
end
     
     
     
     
     
     
     
     
     
     
     
     
