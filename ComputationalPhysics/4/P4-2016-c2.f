!<<<<<<<<<<<<<<<<<<<<<<<<<<<PREPRACTICA 4>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!OP OCT  2016


	PROGRAM PREPRACTICA4
	  IMPLICIT NONE
	  INTEGER IERR, NITER, J, MAXITER, I
	  PARAMETER (MAXITER=100)
	  REAL*8 ARREL1,A,B,EPS,X0,XI
	  REAL*8 ARREL2,ARREL3,N(MAXITER),X(MAXITER)
	  REAL*8 V0(10)
	  EXTERNAL POL, DPOL
!	  EXTERNAL FUN, DFUN
      	  
5	  FORMAT('Sol.APROX: ',F25.12, 2X,'ERROR <',E20.1)
10	  FORMAT('N=',I8,2X,'Sol.APROX: ',F20.12, 2X,'ERROR <',E20.1)
15	  FORMAT('Per Vo= ',F16.2,2X, 'N= ',I8)

!	TEST BISECCIO
!----------------------------------------------------------------------------
     OPEN(UNIT=1,STATUS='UNKNOWN',FILE='P4-2016-c2-res1.dat')
!  	1a Arrel; x1=1.9
	  A=1.5D0
	  B=2.3D0
	  EPS=1.D-12
	  NITER=NINT(DLOG((B-A)/EPS)/DLOG(2.D0))+1.D0
	  CALL BISECCIO(A,B,EPS,POL,NITER,IERR,ARREL1)
	  WRITE(1,5),ARREL1 , EPS
!	2a Arrel; x2=3.1	  
	  A=2.9D0
	  B=3.3D0
	  NITER=NINT(DLOG((B-A)/EPS)/DLOG(2.D0))+1.D0
	  CALL BISECCIO(A,B,EPS,POL,NITER,IERR,ARREL1)
	  WRITE(1,5), ARREL1 , EPS
!	3a Arrel; x3=3.7	  
	  A=3.5D0
	  B=3.9D0
	  NITER=NINT(DLOG((B-A)/EPS)/DLOG(2.D0))+1.D0
	  CALL BISECCIO(A,B,EPS,POL,NITER,IERR,ARREL1)
	  WRITE(1,5), ARREL1 , EPS
  	  CLOSE(1)
!----------------------------------------------------------------------------
	  WRITE(*,*)'-------------------------------------------------'
!	TEST NEWTON-RAPHSON
!---------------------------------------------------------------------------- 
	  OPEN(UNIT=2,STATUS='UNKNOWN',FILE='P4-2016-c2-res2.dat')
	  OPEN(UNIT=3,STATUS='UNKNOWN',FILE='Fig2-2p54.dat')
	  OPEN(UNIT=4,STATUS='UNKNOWN',FILE='Fig2-2p55.dat')
	  OPEN(UNIT=5,STATUS='UNKNOWN',FILE='Fig2-2p7.dat')
	  V0 = (/0.1D0, 1.D0, 1.5D0, 2.5D0, 2.51D0, 2.52D0, 2.54D0,
     *2.55D0, 2.7D0, 3.D0/)
	  EPS=1.D-12
	  DO J=1,10
	  X0=V0(J)
	  CALL NEWTONRAPHSON(X0,EPS,pol,dpol,NITER,IERR,ARREL1)
	  IF(IERR.EQ.0) THEN
!	    WRITE(*,10) NITER, ARREL1, EPS  
		WRITE(2,15) V0(J), NITER
	  ENDIF
	 ENDDO
	 CLOSE(2)
	 CLOSE(3)
	 CLOSE(4)
	 CLOSE(5)
	 WRITE(*,*)'---------------------------------------------------'
!----------------------------------------------------------------------------
	END
	
!	POl
!***********************************************************************
      DOUBLE PRECISION FUNCTION POL(X)
      DOUBLE PRECISION X, F

      F=-(21793.0d0/1000.0d0)+((2439.0d0/100.0D0)*X)-((87.0d0/10.0d0)*X*
     +*2)+X**3
      
      POL=DCOSH(X)*F

      RETURN

      END
	  
!	Derivada de POL
!***********************************************************************
      DOUBLE PRECISION FUNCTION DPOL(X)
      DOUBLE PRECISION X, F, DF

      F=-(21793.0d0/1000.0d0)+((2439.0d0/100.0D0)*X)-((87.0d0/10.0d0)*X*
     +*2)+X**3

      DF=(2439.0D0/100.0D0)-((87.0D0/5.0D0)*X)+(3.0D0*X**2)

      DPOL=(DSINH(X)*F)+(DCOSH(X)*DF)

      RETURN

      END