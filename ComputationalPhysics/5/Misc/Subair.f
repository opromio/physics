!SUBROUTINE Acceptacio i rebuig.
      SUBROUTINE SUBAIR(NDAT,XNUMS,FUN,A,B,M)
      IMPLICIT NONE
      INTEGER NDAT, I, J
      REAL*8 A, B, M, X1, X2, X, P, FUN, C
      REAL*8 XNUMS(NDAT), VALMIT, VAR, DESVEST
     
!     COMMON /ERRORS/ VALMIT, VAR, DESVEST
!     EXTERNAL FUN
      
2     FORMAT(F25.12)
      WRITE(*,*)'ACCEPTACIO I REBUIG: '
      WRITE(*,*)'______________________________________________________'
!-----------------------------------------------------------------------        
!     Comença el metode        
      DO WHILE(J.LT.NDAT) 
5       X1=RAND()
        X2=RAND()
        X=(B-A)*X1 + A
        P=M*X2
        IF(FUN(X).GE.P) THEN
          J=J+1
          XNUMS(J)=X
          WRITE(*,2)J, XNUMS(J)
        ELSE
          GOTO 5
        ENDIF
      ENDDO
!-----------------------------------------------------------------------      
!     Calcul ValMit, Var, Desv. Estandar  
      C=0.D0
      DO I=1,NDAT
        C=C+XNUMS(I)
      ENDDO
      VALMIT=C/DBLE(NDAT)
      WRITE(*,2) VALMIT
      WRITE(3,2) '#Valor mitja: ', VALMIT
      C=0.D0
!_________________________________________      
      DO I=1, NDAT
       C=C+((XNUMS(I)-VALMIT)**2.D0)
      ENDDO
      VAR=C/NDAT
      WRITE(*,2) VAR
      WRITE(3,2)'#Variança: ', VAR
!_________________________________________        
      DESVEST=DSQRT(VAR)
      WRITE(*,2) DESVEST
      WRITE(3,2)'#Desv. Est: ', DESVEST
      
      
      ENDSUBROUTINE
  
 