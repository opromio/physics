!SUBROUTINE que fa la gaussiana
      SUBROUTINE SUBGAUSS(NDAT, XGAUS)
      IMPLICIT NONE
      INTEGER NDAT, ISEED, I, M
      REAL*8 XGAUS(NDAT), U1,U2,PI, MOMCEN
      REAL*8 VAR, S, MITJ, DESVEST, CONVAR
      
      PARAMETER (PI=DACOS(-1.0D0))

1     FORMAT(F25.16)     
11	FORMAT(F25.16,2X,F25.16,2X,F25.16)
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
      
