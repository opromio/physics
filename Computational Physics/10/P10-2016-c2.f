!<<<<<<<<<<<<<<<<<<<<<<<<<<PRACTICA 10>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!OP DEC 2016
!     MAIN
      PROGRAM PRACTICA10
      IMPLICIT NONE
      REAL*8 LX, DX
      INTEGER I, J, K, NX, NTMAX, NT
      PARAMETER (LX=1.3d0, NX=50, DX=LX/NX)
      REAL*8 TOLD(0:NX), TNEW(0:NX), XI, OMEGA, RHO
      REAL*8 NMAX, ERROR, TOL, TINT, TAMB, BETA
      
      REAL*8 TNEW2(1:NX-1), KAPPA, DT, R, X1, X2, X3, X4
 
      REAL*8 A(1:NX-1),B(1:NX-1),C(1:NX-1)

      REAL*8 OPRIGHT(1:NX-1,1:NX-1)
      REAL*8 TCURRENT(1:NX-1), TRIGHT(1:NX-1)
      INTEGER IM
      PARAMETER (NTMAX=6000)
      REAL*8 INTEG1, X0, TPROM(1:NTMAX)
!     Output: Apartat 1)      
      OPEN(UNIT=100, STATUS='UNKNOWN', FILE='Apartat1.dat')
!     Output: Apartat 2) A      
      OPEN (UNIT=20, STATUS='UNKNOWN', FILE='Evolucio.dat')
!     OUTPUT APARTAT 2) B
      OPEN (UNIT=25, STATUS='UNKNOWN', FILE='K10.dat')
!     OUTPUT APARTAT 2) C
      OPEN (UNIT=10, STATUS='UNKNOWN', FILE='Gif.dat')
!     Apartat 1
!_______________________________________________________________________      
      OMEGA=1.89d0
      NMAX=1.d5
      TOL=1.d-3
      TINT=0.d0
      TAMB=20.d0
      BETA=2.d-4
      KAPPA=2.2d-5
      
!     Iniciamos variables
      TOLD(0)=0.d0
      TOLD(NX)=280.d0
      TNEW(0)=0.d0    
      TNEW(NX)=280.d0
      DO I=1, NX-1
        TOLD(I)=TINT
        TNEW(I)=TINT
      ENDDO
      
      K=0
10    CONTINUE
      K=K+1
      
      DO I=1, NX-1
        XI=I*DX
!         JACOBI
!-----------------------------------------------------------------------          
!          TNEW(I)=(TNEW(I+1)+TNEW(I-1)+2*TNEW(I))/2.d0
      ENDDO
!     Calcul errors     
      ERROR=0.0D0
      DO I=1,(NX-1)
          ERROR=ERROR+DABS(TOLD(I)-TNEW(I)/(TOLD(I)+TNEW(I)))
          TOLD(I)=TNEW(I)
      ENDDO
!     Fin bucles   
!     Test convergencia     
      IF ((K.lt.NMAX).AND.(ERROR.gt.TOL)) GOTO 10
!     OUTPUT       
      DO I=0, NX
        XI=I*DX
        WRITE(100,*) XI, TNEW(I)
      ENDDO
!     Fin metodo  
!_______________________________________________________________________     
!      Apartat 2
      
!     Iniciamos variables
      TOLD(0)=0.d0
      TOLD(NX)=280.d0
      TNEW(0)=0.d0    
      TNEW(NX)=280.d0
      DO I=1, NX-1
        TOLD(I)=TAMB
        TNEW(I)=TAMB
      ENDDO
      
      DT=4.d0/6.d0

      R=(KAPPA*DT)/(DX**2)
      
      X1=5.d0
      X2=35.d0
      X3=115.d0


!-----------------------------------------------------------------------
      A=0.d0
      B=0.d0
      C=0.d0
      OPRIGHT=0.d0

!     Creem la matriu      
      DO I=1, NX-1
        DO J=1, NX-1
          IF (I.eq.J) OPRIGHT(I,J)=2.d0*(1.d0-(BETA*DT/2.d0)-R)
          IF (I.eq.J+1) OPRIGHT(I,J)=R
          IF ((I+1).eq.J) OPRIGHT(I,J)=R
        ENDDO
        A(I)=-1.d0*R
        B(I)=2.d0*(1.d0+BETA*DT/2.d0+R)
        C(I)=-1.d0*R
      ENDDO
      A(1)=0.d0
      C(NX-1)=0.d0
      
      DO I=1,NX-1
        TCURRENT(I)=TNEW(I)
      ENDDO
        
!     Bucle del temps

      DO NT=1,NTMAX

        DO I=1, NX-1
          TRIGHT(I)=0.d0
          DO J=1, NX-1  
            TRIGHT(I)=TRIGHT(I)+OPRIGHT(I,J)*TCURRENT(J)
          ENDDO
        ENDDO 
        TRIGHT(1)=TRIGHT(1)+2.d0*R*TNEW(0)
        TRIGHT(NX-1)=TRIGHT(NX-1)+2.d0*R*TNEW(NX)
        
        CALL TRIDIAG(A, B, C, TRIGHT, TNEW2, NX-1)
        DO I=1, NX-1
          IF(MOD(NT,100).eq.0.d0) WRITE(10,*) NT*DT, I*DX, TNEW2(I)
          TCURRENT(I)=TNEW2(I)
        ENDDO
        IF(MOD(NT,100).eq.0.d0) THEN 
          WRITE(10,*) 
          WRITE(10,*) 
        ENDIF
        WRITE(20,*) NT*DT, TNEW2(INT(X1/DX)), TNEW2(INT(X2/DX)), 
     &              TNEW2(INT(X3/DX))        
        TNEW(0)=0.D0
        TNEW(NX)=280.d0
        DO I=1, NX-1
          TNEW(I)=TNEW2(I)
        ENDDO
!      Apartat b)
!-----------------------------------------------------------------------
        IM=2!!
        X0=0.d0!!
        CALL MYINTEGRATOR(IM, X0, LX, TNEW2, NX-1, INTEG1)
        TPROM(NT)=(1.d0/LX)*INTEG1
!        WRITE(25,*) NT*DT, TPROM(NT)          
      
      ENDDO
     
      
      ENDPROGRAM
      
!***********************************************************************
!INPUTS: im, a, b, yyin, npunts   OUTPUTS: valor      
	SUBROUTINE MYINTEGRATOR(IM, A, B, YYIN, NPUNTS, VALOR)	  
	IMPLICIT NONE
	INTEGER IM, K, NPUNTS
	REAL*8 H, VALOR
      REAL*8 A, B, YYIN(NPUNTS)

	H=(B-A)/DBLE(NPUNTS-1)	!Pas	
      VALOR=0.d0
      DO K=2, NPUNTS-1 
!	Si IM=1--> Aplicamos el metodo de los trapecios.
	  IF(IM.EQ.1) THEN
	    VALOR = VALOR + H*YYIN(K)
!	Si IM=2-->Aplicamos el metodo de simpson
        ELSE IF(IM.EQ.2) THEN	    
	    IF(MOD(K,2).EQ.0) THEN
	      VALOR=VALOR+(2.D0*H/3.0D0)*YYIN(K)
          ELSE 
	      VALOR=VALOR+(4.D0*H/3.0D0)*YYIN(K)
	    ENDIF
	  ELSE
	    PRINT*, 'Valor IM no valid'
	  ENDIF
      ENDDO
!     Valor en los extremos
	IF(IM.EQ.1) THEN 	!Trapecios
	  VALOR = VALOR + (H/2.0D0)*(YYIN(1) + YYIN(NPUNTS))
	ELSE IF(IM.EQ.2) THEN	!Simpson
	  VALOR = VALOR + (H/3.0D0)*(YYIN(1) + YYIN(NPUNTS))
	ENDIF
	
	END	
      
      SUBROUTINE TRIDIAG(A,B,C,R,PSI,IMAX)
      IMPLICIT double precision (A-H,K,O-Z)
      IMPLICIT INTEGER (I-J , L-N)
      double precision  BET
      double precision  GAM(4001)
      double precision A(IMAX),B(IMAX),C(IMAX),R(IMAX),PSI(IMAX)

        IF(B(1).EQ.0.) PAUSE
        BET=B(1)
        PSI(1)=R(1)/BET
        DO 11 J=2,IMAX
        GAM(J)=C(J-1)/BET
        BET=B(J)-A(J)*GAM(J)
        IF(BET.EQ.0) PAUSE
        PSI(J)=(R(J)-A(J)*PSI(J-1))/BET
11      CONTINUE

        DO 12 J=IMAX-1,1,-1
        PSI(J)=PSI(J)-GAM(J+1)*PSI(J+1)
12      CONTINUE

       RETURN
       END

