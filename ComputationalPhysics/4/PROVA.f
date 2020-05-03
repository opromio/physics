

      PROGRAM PROVA1
      IMPLICIT NONE
      REAL*8 POL, DPOL
      REAL*8 B, A, EPS, ARREL
      INTEGER NITER, IERR
      EXTERNAL POL, DPOL
      A=-1.d0
      B=-3.D0
      EPS=1.d-12
      ARREL=0.D0
      CALL BISECCIO(A,B, EPS, POL, NITER, IERR, ARREL)
      PRINT*, ARREL
      
      ENDPROGRAM
      
      DOUBLE PRECISION FUNCTION POL(X)
      REAL*8 X
      POL=2.D0*X+4.D0
      ENDFUNCTION
      
      DOUBLE PRECISION FUNCTION DPOL(X)
      DPOL=2.D0
      ENDFUNCTION
      
      
	SUBROUTINE NEWTONRAPHSON(X0,EPS,FUN,DFUN,NITER,IERR,XARREL)
	   IMPLICIT NONE
	   INTEGER IERR, NITER, MAXITER,J
	   PARAMETER (MAXITER=500)
	   REAL*8 X0, EPS, FUN, DFUN, XARREL, DIF, xin
	   REAL*8 F0, DF0
	   
	   XIN=X0
	   IERR=1
         
10	   FORMAT('N=',I6,2X,'Sol.APROX: ',F25.12, 2X,'ERROR <',E20.1)

	   DO NITER=1, MAXITER
	     F0=FUN(X0)
	     DF0=DFUN(X0)
	     XARREL=X0-(F0/DF0)
           DIF=DABS(XARREL-X0)
		 IF(DIF.LE.EPS) THEN
		   IERR=0
		   EXIT
		 ENDIF
		   X0=XARREL
	 ENDDO
15	   IF(IERR.EQ.0) THEN
	     WRITE(*,10) NITER, XARREL, EPS
	     RETURN
	   ELSE
  		  WRITE(*,*)'El metode no ha convergit per x= ',XIN
	   ENDIF
	ENDSUBROUTINE

	SUBROUTINE BISECCIO(A,B,EPS,FUN,NITER,IERR,XARREL)
	IMPLICIT NONE
	REAL*8 A, B, EPS, XARREL, ERROR, C
	REAL*8 FA, FB, FC, FUN
	INTEGER NITER, IERR, I
	  
	IERR=1
	NITER=NINT(DLOG(DABS(B-A)/EPS)/DLOG(2.D0))+1.D0
	  
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
5	    FORMAT('Solucion exacta: ', F20.12)		
	    WRITE(*,5), XARREL
	    EXIT
	  ENDIF
		
	  ERROR=DABS(B-A)
	  IF(ERROR.LT.EPS) THEN
	    IERR=0
1         XARREL=C
10        FORMAT('Sol. APROX: ', F20.12, 2X, 'ERROR <', E20.1)	
	    WRITE(*,10), XARREL , EPS
          EXIT
        ENDIF
		
	  IF(FC*FA.LT.0) THEN
	    B=C
	    ELSE IF(FC*FB.LT.0) THEN
	    A=C
	  ENDIF
	ENDDO
	ENDSUBROUTINE

      SUBROUTINE DERIFUN(NDAT,FU,X,DFU)  
      IMPLICIT NONE
      DOUBLE PRECISION X0, XF, XN
      DOUBLE PRECISION X(NDAT), FU(NDAT), H, DFU(NDAT)
      INTEGER I, NDAT, I1, I2
	
      X0=X(1)
      XF=X(NDAT)
      H=(XF-X0)/(NDAT-1)
      DO I=1, NDAT
       IF (I.EQ.1) THEN
        DFU(I)=(FU(2)-FU(1))/H
       ELSE IF (I.EQ.(NDAT)) THEN
        DFU(I)=(FU(NDAT)-FU(NDAT-1))/H
       ELSE
        DFU(I)=(FU(I+1)-FU(I-1))/(2.0D0*H)
       ENDIF
      ENDDO											
      ENDSUBROUTINE