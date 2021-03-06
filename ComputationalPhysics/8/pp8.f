!<<<<<<<<<<<<<<<<<<<<<<<<<<<<PRACTICA8>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!ÓP NOV 2016

!MAIN
      PROGRAM PRACTICA8
      IMPLICIT NONE
      INTEGER NEQUS, NP, IT, K, J, C, IM
      PARAMETER (NEQUS=2)
      PARAMETER (NP=500)
      REAL*8 X,DX,XMIN,XMAX,EV1(3),EV2(3),E1,E2,E3,VAPS(3),VEPS(3,NP)
      REAL*8 YVAR(NEQUS), YOUT(NEQUS), NVEPS(3,NP)
      REAL*8 PHIE1, PHIE2, PHIE3, ACC, ENERGIA
      
      REAL*8 A, B, VALOR, VEPSJ(NP), NORM(3)
      
      COMMON /CTS/ ENERGIA
      
      OPEN(UNIT=1, STATUS='UNKNOWN', FILE='P8-2016-res.dat')
11    FORMAT(I6, 2X, F24.16)
111   FORMAT(4x, '#N', 16x, 'E')
      WRITE(1,111)
      
      XMAX=1.d0
      XMIN=0.d0
      DX=(XMAX-XMIN)/DBLE(NP-1)

!     E1,E2 --> VALORS PROPERS A LA ARREL,    ACC--> PRECISIÓ DESITJADA
!-----------------------------------------------------------------------       
      EV1=(/9.5d0,39.d0,88.5d0/)
      EV2=(/9.6d0,39.1d0,88.6d0/)
      ACC=1.d-8
!-----------------------------------------------------------------------      
      DO J=1,3
        C=0
        E1=EV1(J)
        E2=EV2(J)
!       COMIENZO BUCLE 
5       ENERGIA=E1
        C=C+1
!_______________________________________________________________________  
        YVAR(1)=0.d0
        YVAR(2)=0.1d0
        DO IT=1, NP
          X=XMIN+(IT-1)*DX
          CALL MIRUNGEKUTTA4(X,DX,YVAR,NEQUS,YOUT)             
          DO K=1, NEQUS
            YVAR(K)=YOUT(K)
          ENDDO
        ENDDO 
        PHIE1=YOUT(1)
!_______________________________________________________________________        
        YVAR(1)=0.d0
        YVAR(2)=0.1d0   
        ENERGIA=E2
        DO IT=1, NP
          X=XMIN+(IT-1)*DX
          CALL MIRUNGEKUTTA4(X,DX,YVAR,NEQUS,YOUT)             
          DO K=1, NEQUS
            YVAR(K)=YOUT(K)
          ENDDO
        ENDDO 
        PHIE2=YOUT(1)
!_______________________________________________________________________              
        YVAR(1)=0.d0
        YVAR(2)=0.1d0  
        E3=(E1*PHIE2-E2*PHIE1)/(PHIE2-PHIE1)
        ENERGIA=E3
        DO IT=1, NP
          X=XMIN+(IT-1)*DX
          CALL MIRUNGEKUTTA4(X,DX,YVAR,NEQUS,YOUT)             
          DO K=1, NEQUS
            YVAR(K)=YOUT(K)
          ENDDO
        ENDDO 
        PHIE3=YOUT(1)
!_______________________________________________________________________        
!       AVALUEM LA CONVERGENCIA
        IF(DABS(PHIE3).LT.ACC) THEN 
          VAPS(J)=E3
          WRITE(1,11) C, E3
          WRITE(1,*)
          WRITE(1,*)
        ELSE
          WRITE(1,11) C, E3
          E1=E2
          E2=E3      
          GOTO 5        
        ENDIF      
!FIN_BUCLE______________________________________________________________         
!       PRINT*, C, VAPS(J)
      ENDDO 
      
!-----------------------------------------------------------------------      
!     APARTAT 4
      OPEN(UNIT=2, STATUS='UNKNOWN', FILE='apartat4.dat')
1     FORMAT(F24.12, 2X, F24.12)
!     CALCULEM VEP'S SENSE NORMALITZAR
      DO J=1,3
        YVAR(1)=0.d0
        YVAR(2)=0.1d0   
        ENERGIA=VAPS(J)
        DO IT=1, NP
          X=XMIN+(IT-1)*DX
          CALL MIRUNGEKUTTA4(X,DX,YVAR,NEQUS,YOUT)
          VEPS(J,IT)=YOUT(1)
          DO K=1, NEQUS
            YVAR(K)=YOUT(K)
          ENDDO
        ENDDO 
      ENDDO
!     CALCULEM CONSTANTS NORMALITZACIO
      A=0.d0
      B=1.d0
      IM=2
      DO J=1,3
        DO IT=1, NP
          VEPSJ(IT)=(VEPS(J,IT))**2.d0
        ENDDO
        CALL MYINTEGRATOR(IM,A,B,VEPSJ,NP,VALOR)
        NORM(J)=1.d0/DSQRT(VALOR)   
        PRINT*, NORM(J)
      ENDDO 
!     NORMALITZEM VEP'S
      DO J=1,3
        WRITE(2,*)
        WRITE(2,*)
        WRITE(2,*) '#E= ', ENERGIA
        DO IT=1, NP
          X=XMIN+(IT-1)*DX        
          NVEPS(J,IT)=NORM(J)*VEPS(J,IT)
          WRITE(2,1) X, NVEPS(J,IT)
        ENDDO 
      ENDDO
      ENDPROGRAM
!***********************************************************************      
!INPUTS: t,dt,yyin,nequs         OUTPUTS: yyout      
      SUBROUTINE MIRUNGEKUTTA4(T,DT,YYIN,NEQUS,YYOUT)
      IMPLICIT NONE
      INTEGER NEQUS, J
      REAL*8 T, DT, YYIN(NEQUS), YYOUT(NEQUS), YYTEMP(NEQUS)
      REAL*8 K1(NEQUS), K2(NEQUS), K3(NEQUS), K4(NEQUS)

!     CALCUL K1           
      CALL DERIVAD(T,YYIN,K1,NEQUS)
!     CALCUL K2 
      DO J=1, NEQUS
        YYTEMP(J)=YYIN(J)+DT*(K1(J)/2.d0)
      ENDDO
      CALL DERIVAD(T+DT/2.d0, YYTEMP, K2, NEQUS)
!     CALCUL K3  
      DO J=1, NEQUS
        YYTEMP(J)=YYIN(J)+DT*(K2(J)/2.d0)
      ENDDO   
      CALL DERIVAD(T+DT/2.d0, YYTEMP, K3, NEQUS)
!     CALCUL K4      
      DO J=1, NEQUS
        YYTEMP(J)=YYIN(J)+DT*K3(J)
      ENDDO
      CALL DERIVAD(T+DT, YYTEMP, K4, NEQUS)
!     CALCUL VALOR Y1      
      DO J=1, NEQUS
        YYOUT(J)=YYIN(J)+(DT/6.d0)*(K1(J)+2.d0*K2(J)+2.d0*K3(J)+K4(J))
      ENDDO
      ENDSUBROUTINE
!***********************************************************************      
!INPUTS: t, yin, nequ             OUTPUTS: dyout
      SUBROUTINE DERIVAD(X, YIN, DYOUT, NEQU)
      IMPLICIT NONE
      INTEGER NEQU
      REAL*8 X, YIN(NEQU), DYOUT(NEQU), ENERGIA
!      REAL*8 G, KAPPA, M, XC
      
      COMMON /CTS/ ENERGIA
      
!      G=9.81d0
!      XC=0.2d0
!      KAPPA=2.d0
!      M=1.d0
      
!      DYOUT(1)=G-KAPPA*(YIN(1)**2.d0)/M   --> EJEMPLO 1: CAÍDA LIBRE
!      DYOUT(1)=YIN(2)
!      DYOUT(2)=(-KAPPA*YIN(1)-XC*YIN(2))/M
      DYOUT(1)=YIN(2)
      DYOUT(2)=-ENERGIA*YIN(1)
      ENDSUBROUTINE
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
	
      
      
      
      