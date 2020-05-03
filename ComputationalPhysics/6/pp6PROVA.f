!<<<<<<<<<<<<<<<<<<<<<<<<<<<PRACTICA 6>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!OP NOV 2016
!     MAIN
      PROGRAM PACTICA6
      IMPLICIT NONE
      
      CALL MONTECARLOP6()
      CALL MULTIDMCP6()
      
      ENDPROGRAM
      
!SUBROUTINE MONTECARLO 1-D    
!-----------------------------------------------------------------------
      SUBROUTINE MONTECARLOP6()
      IMPLICIT NONE
      
      INTEGER N, NDAT, I, CON
      REAL*8 I1, A1, B1, X1, PI
      REAL*8 T, CUAD1, ERROR1, ERREX1, VALEX1
      
      REAL*8 I2, A2, B2, X2, FUN
      REAL*8 CUAD2, ERROR2, ERREX2, VALEX2
      
      PARAMETER (NDAT=1.D7)
      
      REAL*8 XGAUS(NDAT), CUAD3,  I3, CUAD4, I4, ERROR3, ERROR4
      REAL*8 XNUMS(NDAT), I5, CUAD5, ERROR5, A5, B5, M
      
      EXTERNAL SUBGAUSS
      EXTERNAL SUBAIR
      EXTERNAL FUN
      
      PARAMETER (A1=-1.0D0, B1=1.0D0)
      PARAMETER (PI=DACOS(-1.0D0))
      PARAMETER (A2=-PI, B2=PI)
      PARAMETER (A5=-PI, B5=PI)
      
      COMMON /GAUSS/ XGAUS
      
1     FORMAT(6X,'#N',18X,'I1',22X,'Error',22X,'I2',22X,'Error',22X,'I3',
     *   22X, 'Error',22X,'I4',22X,'Error',22X,'I5',22X,'Error')
     
11    FORMAT(I9, F24.12, 2X, F24.12, 2X, F24.12, 2X, F24.12, 2X, 
     * F24.12,2X,F24.12,2X,F24.12,2X,F24.12,2X,F24.12,2X,F24.12)
     
111   FORMAT(I9, F24.12, 2X, F24.12, 2X, F24.12, 2X, F24.12)
     
      OPEN(UNIT=10,STATUS='UNKNOWN',FILE='P6-2016-res.dat')
      WRITE(10,1)
      OPEN(UNIT=20,STATUS='UNKNOWN',FILE='Fig1.dat')
      
      
      VALEX1=PI/2.0D0
      VALEX2=(PI/32.0D0)*(8*(PI**2.D0)-15.0D0)
      CALL SUBGAUSS(NDAT,XGAUS)
      M=1.D0/PI
      CALL SUBAIR(NDAT, XNUMS, FUN, A5, B5, M)
        
      I1=0.0D0
      CUAD1=0.0D0
        
      I2=0.0D0
      CUAD2=0.0D0
     
      I3=0.0D0
      CUAD3=0.0D0
        
      I4=0.D0
      CUAD4=0.D0
        
      I5=0.0D0
      CUAD5=0.0D0
      CON=0
      DO N=1, NDAT               
            T=RAND()
!           1A INTEGRAL
            X1=((B1-A1)*T+A1)    
            I1=I1+(B1-A1)*DSQRT(1.D0-X1**2.0D0)
            CUAD1=CUAD1+((B1-A1)*DSQRT(1.D0-X1**2.D0))**2.D0           
!           2A INTEGRAL
            X2=((B2-A2)*T+A2)
            I2=I2+(B2-A2)*(X2**2.D0)*DSIN(X2)**4.D0
            CUAD2=CUAD2+((B2-A2)*((X2**2.D0)*DSIN(X2)**4.D0))**2.D0  
!           3A INTEGRAL
            CON=CON+1      
            I3=I3+(DSIN(XGAUS(CON)))**2.D0
            CUAD3=CUAD3+(DSIN(XGAUS(CON)))**4.0D0
!           4A INTEGRAL 
            I4=I4+(DSQRT(2.D0*PI)*(DCOS(XGAUS(CON)))**2.D0)
            CUAD4=CUAD4+(DSQRT(2.D0*PI)*(DCOS(XGAUS(CON)))**2.D0)**2.D0
!           5A INTEGRAL 
            I5=I5+((PI**2.D0)/2.D0)*(DSIN(XNUMS(CON))**2.D0)*
     *      DABS(XNUMS(CON))
            CUAD5=CUAD5+(((PI**2.D0)/2.D0)*(DSIN(XNUMS(CON))**2.D0)*
     *      DABS(XNUMS(CON)))**2.D0
            IF(MOD(N,1000).EQ.0) THEN
!-------Valors promig.        
!       1A INTEGRAL      
        I1=I1/DBLE(N)
        CUAD1=CUAD1/DBLE(N)
        ERROR1=((1.0D0/DSQRT(DBLE(N)))*DSQRT(CUAD1-(I1**2.0D0)))
        ERREX1=DABS(I1-VALEX1)    
!       2A INTEGRAL
        I2=I2/DBLE(N)
        CUAD2=CUAD2/DBLE(N)
        ERROR2=((1.0D0/DSQRT(DBLE(N)))*DSQRT(CUAD2-(I2**2.0D0)))      
        ERREX2=DABS(I2-VALEX2) 
         
!       3A INTEGRAL
        I3=I3/DBLE(N)
        CUAD3=CUAD3/DBLE(N)
        ERROR3=((1.0D0/DSQRT(DBLE(N)))*DSQRT(CUAD3-(I3**2.0D0))) 

!       4A INTEGRAL      
        I4=I4/DBLE(N)
        CUAD4=CUAD4/DBLE(N)
        ERROR4=((1.0D0/DSQRT(DBLE(N)))*DSQRT(CUAD4-(I4**2.0D0))) 

!       5A INTEGRAL      
        I5=I5/DBLE(N)
        CUAD5=CUAD5/DBLE(N)
        ERROR5=((1.0D0/DSQRT(DBLE(N)))*DSQRT(CUAD5-(I5**2.0D0))) 
              
!       OUTPUT     
        WRITE(10,11) N, I1, ERROR1, I2, ERROR2, I3, ERROR3, I4, ERROR4, 
     *              I5, ERROR5
        
        WRITE(20,111) N, ERROR1, ERREX1, ERROR2, ERREX2
!       TORNEM ALS VALORS DEL BUCLE        
        I1=I1*DBLE(N)
        CUAD1=CUAD1*DBLE(N)
        I2=I2*DBLE(N)
        CUAD2=CUAD2*DBLE(N)
        I3=I3*DBLE(N)
        CUAD3=CUAD3*DBLE(N)
        I4=I4*DBLE(N)
        CUAD4=CUAD4*DBLE(N)
        I5=I5*DBLE(N)
        CUAD5=CUAD5*DBLE(N)
        ENDIF
      ENDDO  
      
      ENDSUBROUTINE
!-----------------------------------------------------------------------      
!SUBROUTINE MTHD. MONTECARLO MULTIDIMENSIONAL
!-----------------------------------------------------------------------
      SUBROUTINE MULTIDMCP6()
      IMPLICIT NONE
      INTEGER NDAT1, NDAT, N, CON, I
      REAL*8 T1,T2,T3,T4,T5,QG,VALEX6, PI

      PARAMETER (NDAT1=1.D7)
      PARAMETER (NDAT=2.D6)
      PARAMETER (PI=DACOS(-1.0D0))

      REAL*8 XGAUS(NDAT1), I6, CUAD6, ERROR6, ERREX6
      
      COMMON /GAUSS/ XGAUS
      EXTERNAL QG
      
      OPEN(UNIT=30, STATUS='UNKNOWN',FILE='Fig2.dat')
2     FORMAT(I9,2X,F24.12,2X,F24.12)
22    FORMAT(6X,'#N',18X,'I6',22X,'Error',22X)
      
      VALEX6=(1.D0/4.D0)*(2.D0+(1.D0/(EXP(1.D0)**(1.D0/4.D0)))+
     * (1+EXP(1.D0))/EXP(1.D0))*(PI**(5.D0/2.D0))
       
       WRITE(10,*)
       WRITE(10,*)
       WRITE(10,*)'#Integral multidimensional:'
       WRITE(10,22)
!      PRINT*, VALEX     
       CON=0.0D0
       I6=0.0D0
       CUAD6=0.0D0
       DO N=1,NDAT
         CON=CON+1
         T1=XGAUS(CON)
         T2=XGAUS(CON+1)
         T3=XGAUS(CON+2)
         T4=XGAUS(CON+3)
         T5=XGAUS(CON+4)
         
         I6=I6+QG(T1,T2,T3,T4,T5)
         CUAD6=CUAD6+(QG(T1,T2,T3,T4,T5)**2.0D0)
 
         IF(MOD(N,1000).EQ.0) THEN
           I6=I6/DBLE(N)
           CUAD6=CUAD6/DBLE(N)
           ERROR6=((1.0D0/DSQRT(DBLE(5*N)))*DSQRT(CUAD6-(I6**2.0D0)))
           ERREX6=DABS(I6-VALEX6)
           
           WRITE(10,2) N, I6, ERROR6
           WRITE(30,2) N, ERROR6, ERREX6
!          TORNEM ALS VALORS DEL BUCLE           
           I6=I6*DBLE(N)
           CUAD6=CUAD6*DBLE(N)               
         ENDIF
       ENDDO  
      
      ENDSUBROUTINE
!-----------------------------------------------------------------------            
!SUBROUTINE GAUSSIANA
!-----------------------------------------------------------------------
      SUBROUTINE SUBGAUSS(NDAT, XGAUS)
      IMPLICIT NONE
      INTEGER NDAT, ISEED, I, M
      REAL*8 XGAUS(NDAT), U1,U2,PI, MOMCEN
      REAL*8 VAR, S, MITJ, DESVEST, CONVAR
      
      PARAMETER (PI=DACOS(-1.0D0))

      ISEED=16307712
      
      CALL SRAND(ISEED)
!CALCULAMOS LOS VALORES DE LA GAUSSIANA, MEDIA, VARIANCIA, DESV. ESTANDAR
      S=0.0D0
      CONVAR=0.0D0
      DO I=1, NDAT
        U1=RAND()
        U2=RAND()
        XGAUS(I)=DSQRT(-2.0D0*DLOG(U1))*DSIN(2.0D0*PI*U2)

        S=XGAUS(I)+S
        MITJ=S/NDAT
      ENDDO  
      DO I=1, NDAT
        CONVAR=((XGAUS(I)-MITJ)**2.0D0)+ CONVAR   

      ENDDO
      VAR=CONVAR/NDAT
      DESVEST=DSQRT(VAR)
      
!CALCULO DE LOS MOMENTOS      
      DO M=2, 10
        CONVAR=0.0D0
        DO I=1,NDAT
          CONVAR = CONVAR + (XGAUS(I)-MITJ)**M
        ENDDO
        MOMCEN=CONVAR/NDAT
      ENDDO  
      ENDSUBROUTINE
!-----------------------------------------------------------------------
!SUBROUTINE ACCEPTACIO I REBUIG      
!-----------------------------------------------------------------------
      SUBROUTINE SUBAIR(NDAT,XNUMS,FUN,A,B,M)
      IMPLICIT NONE
      INTEGER I, NDAT, N
      DOUBLE PRECISION X1, X2, X, P, B, A, M, FUN, XNUMS(NDAT)
      DOUBLE PRECISION S, VM, VAR, DE, MC(9)
      EXTERNAL FUN

      DO I=1, NDAT
1      X1=RAND()
       X2=RAND()
       X=((B-A)*X1)+A
       P=M*X2
       IF (FUN(X).GE.P) THEN 
        XNUMS(I)=X
       ELSE
        GOTO 1
       ENDIF
      ENDDO

!    Valor mitja, variancia i desviacio estandar de la distribucio
      S=0.0D0
      DO I=1, NDAT
       S=S+XNUMS(I)
      ENDDO
      VM=S/DBLE(NDAT)

      S=0.0D0
      DO I=1, NDAT
       S=S+((XNUMS(I)-VM)**2)
      ENDDO
      VAR=S/DBLE(NDAT)

      DE=DSQRT(VAR)

      RETURN

      END
!-----------------------------------------------------------------------

!**********************************************************************  
!FUNCTION DISTRIBUCIO DE PROBABILITAT
      REAL*8 FUNCTION FUN(X)
      IMPLICIT NONE
      REAL*8 X, PI

      PARAMETER (PI=DACOS(-1.0D0))

      IF ((X.GE.-PI).AND.(X.LE.PI)) THEN
        FUN=(2.D0/(PI**2.D0))*((DSIN(X))**2.D0)*DABS(X)
      ELSE
        PRINT*,  'Valor x fora del rang de p(x)'
      ENDIF
      
      ENDFUNCTION
!-----------------------------------------------------------------------      
!FUNCTION QG(X)=G(X)/GAUSSIANA
      REAL*8 FUNCTION QG(X1,X2,X3,X4,X5)
      IMPLICIT NONE
      REAL*8 X1,X2,X3,X4,X5,PI, EXPON
      PARAMETER (PI=DACOS(-1.0D0))

      EXPON=DEXP(-((X1**2)+(X2**2)+(X3**2)+(X4**2)+(X5**2))/2.D0)  
      QG=((2.D0*PI)**(5.D0/2.D0))*(((X1*X2)**2.D0)*DCOS(X4)+((X3**2.D0)*
     *  (1.D0+X1))+((DCOS(X4)*X5)**2.D0))*EXPON
      ENDFUNCTION
     