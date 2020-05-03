!>>>>>>>>>>>>>>>>>>>>>>>>>Practica 5<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!OP NOV 2016
      PROGRAM PRACTICA5
      IMPLICIT NONE
      INTEGER I, J, NDAT1, NCAIX1!1)
      INTEGER NM, NT, CON!2)
      INTEGER NCAIX2, K !2)D)
      REAL*8 X0, Y0, T0, DT, T, DELTA !2)
      REAL*8 A, B, A2, B2 !2)B)
      

      PARAMETER (X0=0.D0, Y0=0.D0, DELTA=2.21D-5)!2)
      PARAMETER (NM=2500      , NT=240) !2)
      PARAMETER (NDAT1=120000) !1)
      PARAMETER (NCAIX1=120, NCAIX2=15) !1)
      
!     Definim vectors      
      REAL*8 XGAUS(NDAT1), XHIS1(NCAIX1), HIS1(NCAIX1), ERHIS1(NCAIX1) !1)
      REAL*8 X(NM,NT), Y(NM,NT) !2)
      REAL*8 VAR(NT) !2)B)
      REAL*8 D(NM) !2)D)
      REAL*8 XHIS2(NCAIX2), HIS2(NCAIX2), ERHIS2(NCAIX2) !2)D)

      OPEN(UNIT=10, STATUS='UNKNOWN', FILE='P5-2016-c2-res1.dat')
      
      OPEN(UNIT=1, STATUS='UNKNOWN', FILE='PART1.dat')
      OPEN(UNIT=2, STATUS='UNKNOWN', FILE='PART2.dat')
      OPEN(UNIT=3, STATUS='UNKNOWN', FILE='PART3.dat')
      OPEN(UNIT=4, STATUS='UNKNOWN', FILE='PART4.dat')
      OPEN(UNIT=5, STATUS='UNKNOWN', FILE='PART5.dat')
      OPEN(UNIT=6, STATUS='UNKNOWN', FILE='P5-2016-c2-res2.dat')
      OPEN(UNIT=7, STATUS='UNKNOWN', FILE='P5-2016-c2-res3.dat')

      
1     FORMAT(F25.12, 2X , F25.12 , 2X , F25.12)
2     FORMAT(F25.12, 2X, F25.12)

!     Apartat 1)      
      CALL SUBGAUSS(NDAT1, XGAUS)
      CALL HISTOGRAMA(NDAT1, XGAUS, NCAIX1, XHIS1, HIS1, ERHIS1)
      DO I=1, NCAIX1
        WRITE(10,1), XHIS1(I), HIS1(I), ERHIS1(I)
      ENDDO
      
!     Apartat 2)
!     A)
      T0=0.D0
      T=0.D0
      DT=0.02D0
      CON=1
      DO J=1,NM
        X(J,1)=X0
        Y(J,1)=Y0
      ENDDO
      DO I=1, NT-1

        DO J=1, NM
          CON=CON+2
          X(J,I+1)=X(J,I) + (DSQRT(DELTA*DT)*XGAUS(CON))
          Y(J,I+1)=Y(J,I) + (DSQRT(DELTA*DT)*XGAUS(CON+1))
        ENDDO       
      ENDDO             
      DO I=1, NT
        WRITE(1,2) X(1,I), Y(1,I)
        WRITE(2,2) X(2,I), Y(2,I)
        WRITE(3,2) X(3,I), Y(3,I)
        WRITE(4,2) X(4,I), Y(4,I)
        WRITE(5,2) X(5,I), Y(5,I)
      ENDDO
      
66    FORMAT(F14.8, 2X, F25.12)      
!     B) y  C)
      DO I=1, NT
        T=T0+I*DT
        DO J=1, NM
          A=A+(Y(J,I)**2.D0)
          B=B+Y(J,I)
        ENDDO
        A2=A/DBLE(NM)
        B2=B/DBLE(NM)
        VAR(I)=A2-(B2**2.D0)          ! EL CALCULO DE LA VARIANZA ESTA MAL!!! 
        WRITE(6,66) T, VAR(I)         ! DEBERIA DAR UNA RECTA. NO ENCUENTRO EL ERROR :(
      ENDDO
      
!     D)
      DO J=1, NM
        D(J)=DSQRT((X(J,NT)**2.D0)+(Y(J,NT)**2.D0))
      ENDDO
      
      CALL HISTOGRAMA(NM, D, NCAIX2,XHIS2,HIS2,ERHIS2)
      
      DO K=1, NCAIX2
        WRITE(7,1) XHIS2(K), HIS2(K), ERHIS2(K)
      ENDDO
      ENDPROGRAM
      
      
      
      
!SUBROUTINES
!------------------------------------------------------------------------

!SUBROUTINE que fa l'histograma	
	SUBROUTINE HISTOGRAMA(NDAT,XDATA,NCAIXES,XHISTO,HISTO,ERRHISTO) 
	IMPLICIT NONE
	INTEGER NDAT, NCAIXES, I, J, K
	REAL*8 HISTO(NCAIXES),ERRHISTO(NCAIXES)
	REAL*8 XDATA(NDAT),XHISTO(NCAIXES), NHISTO(NCAIXES)
	REAL*8 XN,H, X0, X1, A, B, DESVEST, S	
 	
	A=MINVAL(XDATA)
	B=MAXVAL(XDATA)
	
	H=(B-A)/DBLE(NCAIXES)
	
	DO I=1, NCAIXES
	  X0=A+(I-1)*H
	  X1=A+I*H
	  XHISTO(I)=((X0+X1)/2.0D0)
	  NHISTO(I)=0.0D0
        HISTO(I)=0.0D0
      ENDDO
	
	DO K=1,NDAT
	  XN=XDATA(K)
	  J=INT((XN-A)/H)+1
	  IF(XN.EQ.B) THEN
	    J=NCAIXES
	  ENDIF
	  NHISTO(J)=NHISTO(J)+1.0D0
	ENDDO
	S=0.0D0
!NORMALITZACIO I CALCUL ERROR, DESV. ESTANDAR
	DO I=1, NCAIXES	 
	  HISTO(I)=NHISTO(I)/(NDAT*H)
        DESVEST=(1.D0/H)*DSQRT((1.D0/DBLE(NDAT))*(NHISTO(I)/DBLE(NDAT))*
     *          (1.0D0-(NHISTO(I)/DBLE(NDAT))))
	  ERRHISTO(I)=(2.0D0*DESVEST)
	ENDDO
	ENDSUBROUTINE
      
!SUBROUTINE que fa la gaussiana
      SUBROUTINE SUBGAUSS(NDAT, XGAUS)
      IMPLICIT NONE
      INTEGER NDAT, ISEED, I, M
      REAL*8 XGAUS(NDAT), U1,U2,PI, MOMCEN
      REAL*8 VAR, S, MITJ, DESVEST, CONVAR
      
      PARAMETER (PI=DACOS(-1.0D0))

1     FORMAT(F25.12)     
11	FORMAT(F25.12,2X,F25.12,2X,F25.12)
111   FORMAT(10X,'#MITJ', 25X, 'VAR',28X,'DESV')

!      OPEN(UNIT=1,STATUS='UNKNOWN', FILE='Res.dat')

      ISEED=16307712
      
      CALL SRAND(ISEED)
!CALCULAMOS LOS VALORES DE LA GAUSSIANA, MEDIA, VARIANCIA, DESV. ESTANDAR
!-----------------------------------------------------------------------
      S=0.0D0
      CONVAR=0.0D0
      DO I=1, NDAT
        U1=RAND()
        U2=RAND()
        XGAUS(I)=DSQRT(-2.0D0*DLOG(U1))*DSIN(2.0D0*PI*U2)

        S=XGAUS(I)+S
        MITJ=S/NDAT
      ENDDO  
!      WRITE(*,*)'______________________________________________________'
      DO I=1, NDAT
        CONVAR=((XGAUS(I)-MITJ)**2.0D0)+ CONVAR   
!        WRITE(1,1) XGAUS(I)
!        WRITE(*,1) XGAUS(I)
      ENDDO
      VAR=CONVAR/NDAT
      DESVEST=DSQRT(VAR)
!      WRITE(*,*)'______________________________________________________'
 !    WRITE(1,*)'______________________________________________________'
 !     WRITE(1,111)
 !     WRITE(1,11) MITJ, VAR, DESVEST
!      WRITE(*,11) MITJ, VAR, DESVEST
      
!CALCULO DE LOS MOMENTOS
!-----------------------------------------------------------------------      
 !     WRITE(*,*)'______________________________________________________'
      DO M=2, 10
        CONVAR=0.0D0
        DO I=1,NDAT
          CONVAR = CONVAR + (XGAUS(I)-MITJ)**M
        ENDDO
        MOMCEN=CONVAR/NDAT
     
!        WRITE(1,1) MOMCEN
      
  !      WRITE(*,1) MOMCEN
      ENDDO  
      ENDSUBROUTINE
      
!SUBROUTINE Acceptacio i rebuig.
      SUBROUTINE SUBAIR(NDAT,XNUMS,FUN,A,B,M)
      IMPLICIT NONE
      INTEGER NDAT, I, J
      REAL*8 A, B, M, X1, X2, XN, P, C, FUN
      REAL*8 XNUMS(NDAT), VALMIT, VAR, DESVEST
      
      EXTERNAL FUN
     
!     COMMON /ERRORS/ VALMIT, VAR, DESVEST

2     FORMAT(F25.12)
22    FORMAT(I8,2X,F25.12)
!      WRITE(*,*)'ACCEPTACIO I REBUIG: '
!      WRITE(*,*)'______________________________________________________'
!-----------------------------------------------------------------------        
!     Comença el metode
      
      DO WHILE(J.LT.NDAT) 
5       X1=RAND()
        X2=RAND()
        XN=(B-A)*X1 + A
        P=M*X2
        IF(FUN(XN).GE.P) THEN
          J=J+1
          XNUMS(J)=XN
!          WRITE(*,22)J, XNUMS(J)

        ENDIF
      ENDDO
!-----------------------------------------------------------------------      
!     Calcul ValMit, Var, Desv. Estandar  
!      WRITE(*,*)'______________________________________________________'
      C=0.D0
      DO I=1,NDAT
        C=C+XNUMS(I)
      ENDDO
      VALMIT=C/DBLE(NDAT)
!      WRITE(*,2) VALMIT
!      WRITE(3,*)'#Valor mitja: ', VALMIT
      C=0.D0
!_________________________________________      
      DO I=1, NDAT
       C=C+((XNUMS(I)-VALMIT)**2.D0)
      ENDDO
      VAR=C/NDAT
!      WRITE(*,2) VAR
!      WRITE(3,*)'#Variança: ', VAR
!_________________________________________        
      DESVEST=DSQRT(VAR)
!      WRITE(*,2) DESVEST
!      WRITE(3,*)'#Desv. Est: ', DESVEST
       
      ENDSUBROUTINE
