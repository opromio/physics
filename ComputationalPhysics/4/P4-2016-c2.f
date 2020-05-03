!<<<<<<<<<<<<<<<<<<<<<<<<<<<PRACTICA 4>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!OP OCT  2016


	PROGRAM PRACTICA4
	  IMPLICIT NONE
	  INTEGER IERR, NITER, J, I
	  INTEGER N, NDAT, NP
	  REAL*8 POL, DPOL, PAS, T
	  REAL*8 ARREL1, ARREL2, A, B, EPS,X0, XI, H
	  REAL*8 V0, VF, VN, ESTAT, F, DF, ARRELS
	  REAL*8 V(90), P(90), DP(90), VOL0(5)
	  REAL*8 PL, PG, ARREL(10)
	  DOUBLE PRECISION DFUEX(200)
	  COMMON /ISO/ T
	  EXTERNAL POL, DPOL
11	  FORMAT(F20.12,2X, F20.12,2X, F20.12)
22 	  FORMAT(F20.12,2X, F20.12)
33    FORMAT(F16.2, 2X, I8,2X, F20.12)
	
	  OPEN(UNIT=1, STATUS='UNKNOWN', FILE='P4-2016-c2-res.dat')

	  NP=90
      V0= (1/3.0D0 +0.15D0)
	  VF= 4.0D0
	  T=0.93D0
	  PAS=(VF-V0)/(NP-1)
	  DO I=1,NP
	    V(I)= V0+(I-1)*PAS
		P(I)= ESTAT(V(I),T)
	  ENDDO
!____________________________________________________________________
	  CALL DERIFUN(NP, P, V, DP)
	  WRITE(1,*)'# TAULA VALORS V, P(V), dP(V)'
	  DO I=1,NP
		  WRITE(1,11) V(I), P(I), DP(I)
	  ENDDO
!_____________________________________________________________________
	  V0= (1/3.0D0 +0.15D0)
	  VF= 2.0D0
	  T= 0.94D0
	  PAS=(VF-V0)/(NP-1)
	  
	  DO J=1, NP
	    V(I)= V0+(J-1)*PAS
		P(I)= POL(V(I))
		WRITE(3,11) V(I), P(I)
	  ENDDO
	  
!  	1a Arrel; x1=0.77
	  A=0.6D0
	  B=1.0D0
	  EPS=1.D-12
	  WRITE(1,*)'#1A ARREL PER BISECCIO'
	  CALL BISECCIO(A,B,EPS,POL,NITER,IERR,ARREL1)
	  WRITE(1,11),ARREL1 , EPS
	  
!  	2a Arrel; x2=
	  A=1.35d0
	  B=1.5D0
	  WRITE(1,*)'#2A ARREL PER BISECCIO'
	  CALL BISECCIO(A,B,EPS,POL,NITER,IERR,ARREL2)
	  WRITE(1,11), ARREL2 , EPS  
	
!NEWTON-RAPHSON
!____________________________________________________________________________
	OPEN(UNIT=5, STATUS='UNKNOWN', FILE='Res5.dat')
	VOL0 = (/0.35D0, 0.49D0, 1D0, 1.2D0, 1.6D0/)
	T=0.98D0
	EPS=1.D-12
	WRITE(1,*)'#ARRELS NEWTON-RAPHSON (V0, N, Arrel)'
	DO J=1,5 
	  X0=VOL0(J)
	  CALL NEWTONRAPHSON(X0,EPS,POL,DPOL,NITER,IERR,ARRELS)
	  IF(IERR.EQ.0) THEN 
	    IF(ARREL1.GT.(1/3.0D0)) THEN
		  WRITE(1,33) VOL0(J), NITER, ARRELS
		ENDIF
	  ENDIF
	ENDDO
	
	!EMPIEZO EL EXTRA
	
	OPEN(UNIT=2, STATUS='UNKNOWN', FILE='P4-2016-c2-extra.dat')
	WRITE(2,*) '#APARTAT EXTRA'
	WRITE(*,*) 'APARTAT EXTRA'
	DO I=1,10
	T=(DBLE(I)/10.0D0)
	  DO J=1,5 
	    X0=VOL0(J)
	    CALL NEWTONRAPHSON(X0,EPS,POL,DPOL,NITER,IERR,ARREL(I))
	    ENDDO
	ENDDO
	!T=0.94D0
	!PL=ESTAT(ARREL1,T)
	!PG=ESTAT(ARREL2,T)
	!WRITE(2,11)T, PL, PG
	
	!T=0.98D0
	!V0=1.D0
	!CALL NEWTONRAPHSON(V0,EPS,POL,DPOL,NITER,IERR,ARREL1)
	!CALL NEWTONRAPHSON(V0,EPS,POL,DPOL,NITER,IERR,ARREL2)
	!PL=ESTAT(ARREL1,T)
	!PG=ESTAT(ARREL2,T)
!	WRITE(2,11)T, PL, PG
	!T=1.02D0
	ENDPROGRAM

!***************************************************************************
	  SUBROUTINE NEWTONRAPHSON(X0,EPS,FUN,DFUN,NITER,IERR,XARREL)
	   IMPLICIT NONE
	   INTEGER IERR, NITER, MAXITER,J
	   PARAMETER (MAXITER=500)
	   REAL*8 X0, EPS, FUN, DFUN, XARREL, DIF, xin
	   REAL*8 F0, DF0
!	   EXTERNAL FUN, DFUN
	   XIN=X0
	   IERR=1
25	   FORMAT(I8, 2X,F25.12)
10	   FORMAT('N=',I6,2X,'Sol.APROX: ',F25.12, 2X,'ERROR <',E20.1)
!5	   FORMAT(I6,2X,F25.12,2X,F25.12,2X,F25.12,2X,F25.12,2X,F25.12)

	   DO NITER=1, MAXITER
	     F0=FUN(X0)
	     DF0=DFUN(X0)
	     XARREL=X0-(F0/DF0)
		 DIF=DABS(XARREL-X0)
		 IF(XIN.EQ.2.54D0) THEN
		   WRITE(3,25) NITER, XARREL
		 ELSE IF(XIN.EQ.2.55D0) THEN
		   WRITE(4,25) NITER, XARREL
		 ELSE IF (XIN.EQ.2.7D0) THEN
		   WRITE(5,25) NITER, XARREL
		 ENDIF

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
	END
!**********************************************************************	
!*********************************************************************************	
	  SUBROUTINE BISECCIO(A,B,EPS,FUN,NITER,IERR,XARREL)
	  IMPLICIT NONE
	  REAL*8 A, B, EPS, XARREL, ERROR, C
	  REAL*8 FA, FB, FC, FUN
	  INTEGER NITER, IERR, I
	!  EXTERNAL FUN
	  
	  IERR=1
	  !En esta ecuacion deberia haber un +1 al final, pero con esas iteraciones no
	  !convergia para la 2a solucion de Pol, con una iteracion mas aseguramos la convergencia
	  !con la precision deseada.
	  NITER=NINT(DLOG((B-A)/EPS)/DLOG(2.D0))+2.D0
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
		IF(ERROR.LT.EPS) THEN
		  IERR=0
		  XARREL=C
10		  FORMAT('Sol. APROX: ', F20.12, 2X, 'ERROR <', E20.1)	
		  WRITE(*,10), XARREL , EPS
		  EXIT
		ENDIF
		
	    IF(FC*FA.LT.0) THEN
		  B=C
		ELSE IF(FC*FB.LT.0) THEN
		  A=C
		ENDIF
	  ENDDO
	END
!***********************************************************************
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
      END
	   
	   DOUBLE PRECISION FUNCTION POL(V)
	     IMPLICIT NONE
	     REAL*8 T,V
		 COMMON /ISO/ T
	     POL=4.0D0*T*(V**3.D0)-9.0D0*(V**2.0D0)+(6.0D0*V)-1.0D0
	   ENDFUNCTION
	   
	   DOUBLE PRECISION FUNCTION DPOL(V)
	     IMPLICIT NONE
	     REAL*8 T,V
	     COMMON /ISO/ T
	     DPOL=6.0D0*((2.0D0*T*(V**2.0D0))-3.0D0*V + 1.0D0)
	   ENDFUNCTION
	   
       DOUBLE PRECISION FUNCTION ESTAT(V,T)
         DOUBLE PRECISION V,T
         ESTAT=(8.0D0*T/(3.0D0*V-1.0D0))-3.0D0/(V**2)
         RETURN
       END FUNCTION
