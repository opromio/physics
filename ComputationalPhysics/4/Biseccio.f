!<<<<<<<<<<<<<<<<<<<<<<<<<<METODE DE BISECCIO>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!OP OCT  2016
	SUBROUTINE BISECCIO(A,B,EPS,FUN,NITER,IERR,XARREL)
	  IMPLICIT NONE
	  REAL*8 A, B, EPS, XARREL, ERROR, C
	  REAL*8 FA, FB, FC, FUN
	  INTEGER NITER, IERR, I
	  EXTERNAL FUN
	  
	  IERR=1
	  !PRINT*, FUN(A), FUN(B)
	  IF(FUN(A)*FUN(B).GE.0) THEN
	    WRITE(*,*)'ERROR: F(A)F(B)>0' 
		RETURN
	  ENDIF
	  DO I=1, NITER
	    C=(A+B)/2.D0	
		!PRINT*, C
		FA=FUN(A)
		FB=FUN(B)
		FC=FUN(C)
		
		IF(FC.EQ.0) THEN
		  IERR=0
		  XARREL=C
5		  FORMAT('Solucion exacta: ', F20.12)		
		  WRITE(*,5), XARREL
		  EXIT
		ENDIF
		
		ERROR=DABS(B-A)
		!PRINT*, ERROR
		IF(ERROR.LT.EPS) THEN
		  IERR=0
		  XARREL=C
10		  FORMAT('Sol. APROX: ', F20.12, 2X, 'ERROR <', E20.1)	
		  WRITE(*,10),XARREL , EPS
		  EXIT
		ENDIF
		
	    IF(FC*FA.LT.0) THEN
		  B=C
		ELSE IF(FC*FB.LT.0) THEN
		  A=C
		ENDIF
	  ENDDO
	END
	  
	  
!F(X)= 6X**2 + X (dF(X)= 12x)
!	REAL*8 FUNCTION FUN(X)
!	  IMPLICIT NONE
!	  REAL*8 X, FUN
!	  FUN=6.D0*(X**2.D0) + X
!	END
