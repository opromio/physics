!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<PRACTICA5>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!OP NOV 2016

!MAIN
	PROGRAM PRACTICA5
	IMPLICIT NONE
	INTEGER NDAT, NCAIXES, I, J, NCAJ
	PARAMETER (NDAT=10000)
	PARAMETER (NCAIXES=100)
      PARAMETER (NCAJ=50)
	REAL*8 XDATA(NDAT), XHIS(NCAIXES),HIS(NCAIXES), ERHIS(NCAIXES)
      REAL*8 XAIR(NDAT),XHIS2(NCAJ),HIS2(NCAJ), ERHIS2(NCAJ), A, B, M
      REAL*8 XNUMS(NDAT), FUN
      
      EXTERNAL FUN
            
1     FORMAT(F25.12)
11	FORMAT(F25.12,2X,F25.12,2X,F25.12)
111   FORMAT(10X,'#VALOR CENTRAL', 18X, 'N',28X,'ERR')

      OPEN(UNIT=1, STATUS='UNKNOWN', FILE='Res.dat')
      OPEN(UNIT=2, STATUS='UNKNOWN', FILE='Histo.dat')
      OPEN(UNIT=3, STATUS='UNKNOWN', FILE='AiR.dat')
!     APARTATS 1 Y 1'           
!-----------------------------------------------------------------------
      CALL SUBGAUSS(NDAT, XDATA)
      CALL HISTOGRAMA(NDAT,XDATA,NCAIXES,XHIS,HIS,ERHIS)
      
      WRITE(1,*)'HISTOGRAMA: '
      WRITE(1,*)'______________________________________________________'
      WRITE(*,*)'______________________________________________________'
      
	WRITE(1,111)
      DO J=1, NCAIXES
	  WRITE(1,11) XHIS(J), HIS(J), ERHIS(J)
        WRITE(2,11) XHIS(J), HIS(J), ERHIS(J)
	ENDDO
!-----------------------------------------------------------------------
      A=0.0D0
      B=3.D0/2.D0
      M=5.0D0
      CALL SUBAIR(NDAT,XNUMS, FUN, A, B, M)
      CALL HISTOGRAMA (NDAT, XNUMS, NCAJ, XHIS2, HIS2, ERHIS2)
      WRITE(3,111) 
      DO I=1, NCAJ
        WRITE(3,11) XHIS2(I), HIS2(I), ERHIS2(I)
      ENDDO
      
      CLOSE(1)
      CLOSE(2)
      CLOSE(3)

	END
	
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
        DESVEST=(1.D0/H)*DSQRT((1.0D0/NDAT)*(NHISTO(I)/NDAT)*
     *          (1.0D0-(NHISTO(I)/NDAT)))
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
      WRITE(*,*)'______________________________________________________'
      WRITE(1,*)'XGAUSS: '
      WRITE(1,*)'______________________________________________________'
      DO I=1, NDAT
        CONVAR=((XGAUS(I)-MITJ)**2.0D0)+ CONVAR   
        WRITE(1,1) XGAUS(I)
        WRITE(*,1) XGAUS(I)
      ENDDO
      VAR=CONVAR/NDAT
      DESVEST=DSQRT(VAR)
      WRITE(*,*)'______________________________________________________'
      WRITE(1,*)'______________________________________________________'
      WRITE(1,111)
      WRITE(1,11) MITJ, VAR, DESVEST
      WRITE(*,11) MITJ, VAR, DESVEST
      
!CALCULO DE LOS MOMENTOS
!-----------------------------------------------------------------------      
      WRITE(1,*)'______________________________________________________'7
      WRITE(1,*)'MOMENTOS: '
      WRITE(*,*)'______________________________________________________'
      DO M=2, 10
        CONVAR=0.0D0
        DO I=1,NDAT
          CONVAR = CONVAR + (XGAUS(I)-MITJ)**M
        ENDDO
        MOMCEN=CONVAR/NDAT
     
        WRITE(1,1) MOMCEN
      
        WRITE(*,1) MOMCEN
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
      WRITE(*,*)'ACCEPTACIO I REBUIG: '
      WRITE(*,*)'______________________________________________________'
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
          WRITE(*,22)J, XNUMS(J)
!          WRITE(3,22)J, XNUMS(J)
!        ELSE
!          GOTO 5
        ENDIF
      ENDDO
!-----------------------------------------------------------------------      
!     Calcul ValMit, Var, Desv. Estandar  
      WRITE(*,*)'______________________________________________________'
      C=0.D0
      DO I=1,NDAT
        C=C+XNUMS(I)
      ENDDO
      VALMIT=C/DBLE(NDAT)
      WRITE(*,2) VALMIT
      WRITE(3,*)'#Valor mitja: ', VALMIT
      C=0.D0
!_________________________________________      
      DO I=1, NDAT
       C=C+((XNUMS(I)-VALMIT)**2.D0)
      ENDDO
      VAR=C/NDAT
      WRITE(*,2) VAR
      WRITE(3,*)'#Variança: ', VAR
!_________________________________________        
      DESVEST=DSQRT(VAR)
      WRITE(*,2) DESVEST
      WRITE(3,*)'#Desv. Est: ', DESVEST
      
      
      ENDSUBROUTINE
       
!FUNCTION p(x)     
      REAL*8 FUNCTION FUN(X)
      IMPLICIT NONE
      REAL*8 X
      
      IF((X.GT.0.D0).AND.(X.LT.1.D0)) THEN
        FUN=(4.D0/3.D0)*X
      ELSE IF ((X.GT.1.D0).AND.(X.LT.(3.D0/2.D0))) THEN
        FUN=(4.D0/3.D0)*(-2.D0*X + 3.D0)
      ENDIF
      ENDFUNCTION
      
