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
      
      INTEGER N, NDAT, I, CON, SEED
      REAL*8 I1, A1, B1, X1, PI
      REAL*8 T, CUAD1, ERROR1, ERREX1, VALEX1
      
      REAL*8 I2, A2, B2, X2, FUN, L
      REAL*8 CUAD2, ERROR2, ERREX2, VALEX2
      
      PARAMETER (PI=DACOS(-1.0D0))
      PARAMETER (NDAT=1.D6)
      PARAMETER (L=50.D0)
      
      REAL*8 XNUMS(NDAT), M
      
      EXTERNAL SUBAIR
      EXTERNAL FUN
      
      PARAMETER (A1=0.D0, B1=2.D0*PI)
      PARAMETER (A2=0.D0, B2=2.D0*L)

      
      COMMON /ALEAT/ XNUMS
      
1     FORMAT(6X,'#N',18X,'I1',22X,'Error',22X,'I2',22X,'Error')
     
11    FORMAT(I9, F24.12, 2X, F24.12, 2X, F24.12, 2X, F24.12)
     
     
      OPEN(UNIT=10,STATUS='UNKNOWN',FILE='P6-2016-c2-res.dat')
      WRITE(10,1)
      OPEN(UNIT=20,STATUS='UNKNOWN',FILE='Fig1.dat')
      
!Calcul de valors exactes      
      VALEX1=(PI**2.D0)*((2.D0*(PI**2.D0))+1.5D0)

      M=2.D0/L
      CALL SUBAIR(NDAT, XNUMS, FUN, A2, B2, M)
        
      I1=0.0D0
      CUAD1=0.0D0
        
      I2=0.0D0
      CUAD2=0.0D0
            
      CON=0
      
      SEED=16307712
      
      CALL SRAND(SEED)
      DO N=1, NDAT
        CON=CON+1
        T=RAND()
!       1A INTEGRAL
        X1=((B1-A1)*T+A1)    
        I1=I1+(B1-A1)*(X1**3.D0)*(DCOS(X1)**2.D0)
        CUAD1=CUAD1+((B1-A1)*(X1**3.D0)*(DCOS(X1)**2.D0))**2.D0       
            
!       2A INTEGRAL
        X2=XNUMS(CON)
        I2=I2+((1.D0/L)*(DSIN((PI*(X2-2.D0*L))/L))**2.D0)
        CUAD2=CUAD2+(((1.D0/L)*(DSIN((PI*(X2-2.D0*L))/L))**2.D0)**2.D0)
       

        IF(MOD(N,1000).EQ.0) THEN
!---------Valors promig.        
!         1A INTEGRAL      
          I1=I1/DBLE(N)
          CUAD1=CUAD1/DBLE(N)
          ERROR1=((1.0D0/DSQRT(DBLE(N)))*DSQRT(CUAD1-(I1**2.0D0)))
          ERREX1=DABS(I1-VALEX1)    
!         2A INTEGRAL
          I2=I2/DBLE(N)
          CUAD2=CUAD2/DBLE(N)
          ERROR2=((1.0D0/DSQRT(DBLE(N)))*DSQRT(CUAD2-(I2**2.0D0)))      
          ERREX2=DABS(I2-VALEX2) 
         
!       OUTPUT 
          WRITE(10,11) N, I1, ERROR1, I2, ERROR2
        
          WRITE(20,11) N, ERROR1, ERREX1, I2, ERROR2
!         RECUPEREM ELS VALORS DEL BUCLE        
          I1=I1*DBLE(N)
          CUAD1=CUAD1*DBLE(N)
          I2=I2*DBLE(N)
          CUAD2=CUAD2*DBLE(N)
        ENDIF
      ENDDO  
      
      ENDSUBROUTINE
!-----------------------------------------------------------------------      
!SUBROUTINE MTHD. MONTECARLO MULTIDIMENSIONAL
!-----------------------------------------------------------------------
      SUBROUTINE MULTIDMCP6()
      IMPLICIT NONE
      INTEGER NDAT1, NDAT, N, CON, I
      REAL*8 T1,T2,T3,T4,T5,AUX1,VALEX6, PI

      PARAMETER (NDAT1=1.D6)
      PARAMETER (NDAT=33.D4)
      PARAMETER (PI=DACOS(-1.0D0))

      REAL*8 XNUMS(NDAT1), I6, CUAD6, ERROR6, ERREX6, PHI, FUN
      
      COMMON /ALEAT/ XNUMS
      EXTERNAL PHI
      EXTERNAL FUN
      
      OPEN(UNIT=30, STATUS='UNKNOWN',FILE='Fig2.dat')
2     FORMAT(I9,2X,F24.12,2X,F24.12)
22    FORMAT(6X,'#N',18X,'I6',22X,'Error',22X)
      
      VALEX6=1.D0
       
       WRITE(10,*)
       WRITE(10,*)
       WRITE(10,*)'#Integral multidimensional:'
       WRITE(10,22)
   
       CON=0.0D0
       I6=0.0D0
       CUAD6=0.0D0
       DO N=1,NDAT
         CON=CON+1
         T1=XNUMS(CON)
         T2=XNUMS(CON+1)
         T3=XNUMS(CON+2)
         
         AUX1=(PHI(T1,T2,T3)/(FUN(T1)*FUN(T2)*FUN(T3)))
         
         I6=I6+AUX1
         CUAD6=CUAD6+(AUX1**2.D0)
 
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
      REAL*8 X, PI, L

      PARAMETER (L=50)
      PARAMETER (PI=DACOS(-1.0D0))

      IF ((X.GT.0).AND.(X.LT.(2.D0*L))) THEN
        FUN=(1.D0/L)*((DSIN(PI*(X-2.D0*L)/(2.D0*L)))**2.D0)
      ELSE
        PRINT*,  'Valor x fora del rang de p(x)'
      ENDIF
      
      ENDFUNCTION
!-----------------------------------------------------------------------      
!FUNCTION PHI**2
      REAL*8 FUNCTION PHI(X1,X2,X3)
      IMPLICIT NONE
      REAL*8 X1,X2,X3,PI
      REAL*8 AUX1, AUX2, AUX3, AUX4, L
      PARAMETER (PI=DACOS(-1.0D0))
      PARAMETER (L=50.D0)
      
      AUX1=((32.D0*32.D0*2.D0)/(6.D0*(2.D0*L)**3.D0))
      
      AUX2=((DCOS((PI*X1)/(2.D0*L))-(DCOS((PI*X2)/(2.D0*L))))*
     * (DCOS((PI*X2)/(2.D0*L))-(DCOS((PI*X3)/(2.D0*L))))*
     * (DCOS((PI*X1)/(2.D0*L))-(DCOS((PI*X3)/(2.D0*L)))))**2.D0
                  
      AUX3=(DSIN((PI*X1)/(2.D0*L))*DSIN((PI*X2)/(2.D0*L))*DSIN((PI*X3)/
     * (2.D0*L)))**2.D0
     
      AUX4=(DCOS((PI*X1)/(2.D0*L))+DCOS((PI*X2)/(2.D0*L))+DCOS((PI*X3)/
     * (2.D0*L)))**2.D0
      
      PHI=AUX1*AUX2*AUX3*AUX4

     
      ENDFUNCTION
     