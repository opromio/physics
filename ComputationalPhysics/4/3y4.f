      PROGRAM MYDERIVATOR
      IMPLICIT NONE
      INTEGER N, NDAT, I
      DOUBLE PRECISION X0, XF, XN, FUN, DFUN, H
      DOUBLE PRECISION DFUEX(200)
	  REAL*8 X1(10),FUNC1(10),DFUNC1(10) 
  	  REAL*8 X2(200),FUNC2(200),DFUNC2(200) 
	  COMMON /PASO/ H
      OPEN(UNIT=1,FILE="P4-2016-res3-c2-n10.dat")
      OPEN(UNIT=2,FILE="P4-2016-res3-c2-n200.dat")
5	  FORMAT(F20.12,2X,F20.12,2X,F20.12,2X,F20.12)
!	Valors inicial i final
	   X0=0.0D0
       XF=4.0D0
 !--------------------------------------------  
        NDAT=10 
		H=(XF-X0)/DBLE(NDAT-1.0D0)	
		DO I=1, NDAT
          XN=X0+(DBLE(I-1)*H)
          X1(I)=XN
          FUNC1(I)=FUN(XN)
          DFUEX(I)=DFUN(XN)
       ENDDO
	   CALL DERIFUN(NDAT,FUNC1,X1,DFUNC1)
	   DO I=1, NDAT
	     WRITE(1,5) X1(I), FUNC1(I), DFUNC1(I), DFUEX(I)
	   ENDDO
   
!----------------------------------------------	   
        NDAT=200
		H=(XF-X0)/DBLE(NDAT-1.0D0)
     
       DO I=1, NDAT
        XN=X0+(DBLE(I-1)*H)
        X2(I)=XN
        FUNC2(I)=FUN(XN)
        DFUEX(I)=DFUN(XN)
       ENDDO

       CALL DERIFUN(NDAT,FUNC2,X2,DFUNC2)

       DO I=1, NDAT
         WRITE(2,5) X2(I), FUNC2(I), DFUNC2(I), DFUEX(I)
       ENDDO

      ENDPROGRAM

!	Funcio Pol
      DOUBLE PRECISION FUNCTION FUN(X)
      IMPLICIT NONE
      DOUBLE PRECISION X, F
      
      F=-(21793.0d0/1000.0d0)+((2439.0d0/100)*X)-((87.0d0/10.0d0)*X**2
     +)+X**3
      
      FUN=DCOSH(X)*F

      RETURN

      END
!Derivada de la funcio Pol
      DOUBLE PRECISION FUNCTION DFUN(X)
      IMPLICIT NONE
      DOUBLE PRECISION X, F, DF

      F=-(21793.0d0/1000.0d0)+((2439.0d0/100)*X)-((87.0d0/10.0d0)*X**2
     +)+X**3

      DF=(2439.0D0/100.0D0)-((87.0D0/5.0D0)*X)+(3*X**2)

      DFUN=(DSINH(X)*F)+(DCOSH(X)*DF)     

      RETURN

      END
