PROGRAM my_retention
  implicit none
   real c1,c2,c3,c4,c5,c6,c7,stellar_type
   integer i,counter,nsnapshot,ii,nstar
   CHARACTER(len=27)  :: filename
   COMMON/constant/	ii
    open(111,file='file-time-BH-NS-WD-M90k-a2o3.txt')
       nsnapshot=119
        DO ii=1,nsnapshot+1
             call inputname(filename)
             call filerows(nstar,filename)
			 call blackhole(nstar,filename)
        END DO
    CLOSE(111)
END
 !***************************************************
SUBROUTINE inputname(filename)
 INTEGER zero,ii
 CHARACTER(len=15) :: nn
 CHARACTER(len=2) :: n1
 CHARACTER(len=1) :: n2
 CHARACTER(len=1) :: n3
 CHARACTER(len=4) :: n4
 CHARACTER(len=1) :: n7
 CHARACTER(len=6) :: n6
 CHARACTER(len=1) :: mnum
 CHARACTER(len=27) :: filename
 CHARACTER(len=1) :: z1
 CHARACTER(len=2) :: z2
 CHARACTER(len=3) :: z3
 CHARACTER(len=3) :: n8
 COMMON/constant/	ii
   zero=0 
   nn='TOP-M90k-a2o300'
   n2='0'
   n1='00'
   n3='0'
   n8='all'
   n4='.txt'
    WRITE(mnum,'(i1)')zero
      IF ( ii  .lt.  11) THEN
		  WRITE(z1,'(i1)') ii-1 
	          filename=nn// n1  // z1 //n1 //n8// n4
         ELSE IF (   ii .lt. 101  .and.  ii .gt. 10) THEN
		  WRITE(z2,'(i2)') ii-1 
	          filename=nn// n2  //  z2 //n1 //n8// n4 
         ELSE IF (ii .lt. 1001 .and.  ii .gt. 100) THEN
		  WRITE(z3,'(i3)') ii-1 
   	          filename=nn//  z3 //n1 //n8// n4
      END IF
 RETURN
END
!***************************************************
SUBROUTINE filerows(nstar,filename)
  INTEGER:: counter,ios,ii,nstar 
  CHARACTER(len=27) :: filename
  REAL  whatever
  COMMON/constant/	ii,whatever,ios 
    counter=0
         OPEN(unit=1 ,FILE=filename)
     DO
		   READ (1,*,iostat=ios) whatever
           IF (ios/=0) exit     
         counter=counter+1
     END DO
    CLOSE(1)
	  nstar=counter
  RETURN
END
!***************************************************
!***************************************************
SUBROUTINE blackhole(nstar,filename)
  INTEGER:: counterBH,counterNS,counterWD,ios,ii,nstar 
  CHARACTER(len=27) :: filename
  REAL  whatever
  COMMON/constant/	ii,whatever,ios 
        counterBH=0
		counterNS=0
	    counterWD=0
 DO i=1,nstar
      open(1,FILE=filename)
      read(1,*)c1,c2,c3,c4,c5,c6,c7,stellar_type
	 IF( stellar_type .eq. 14)then
	   counterBH=counterBH+1
	 end if

	 IF( stellar_type .eq. 13)then
	   counterNS=counterNS+1
	 end if

	IF( stellar_type .eq. 12 .or. stellar_type .eq.11  .or. stellar_type .eq.10)then
	   counterWD=counterWD+1
	end if
 END DO
      write(111,*)ii-1,'00',counterBH,counterNS,counterWD
   close(1)
  RETURN
END
!***************************************************

