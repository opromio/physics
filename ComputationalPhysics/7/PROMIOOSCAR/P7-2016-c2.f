!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<PRACTICA7>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!OP NOV 2016
!MAIN
      PROGRAM PRACTICA7
      IMPLICIT NONE
      INTEGER NDAT, K, I, NP(4)
!     Vectors      
      REAL*8 T0, THETA0, DTHETA0, DT, TK
      REAL*8 L,G,M, PI, TMAX, TN, WN
          
      PARAMETER (PI=DACOS(-1.d0)) 
      
      COMMON /BLOCK1/ DT, T0, NDAT
      COMMON /BLOCK2/ L, G, M
      
      OPEN(UNIT=10, STATUS='UNKNOWN', FILE='P7-2016-c2-res.dat')
     
      T0=0.d0      
      G=3.711d0
      M=1.5d0
      L=1.25d0
      
      WN=DSQRT(G/L)
      TN=(2.d0*PI)/WN
      TMAX=5.d0*TN
      
      NDAT=1200
      DT=(TMAX-T0)/DBLE(NDAT-1)
      
!A)   !INDEX0   
      WRITE(10,*)'#APARTAT A:'
      THETA0=0.1d0
      DTHETA0=0.d0
      CALL EULER(10, THETA0, DTHETA0)
      !INDEX1
      THETA0=0.1d0
      DTHETA0=0.d0      
      CALL BETTERCALLEULER(10, THETA0, DTHETA0)
      
!B)   !INDEX2
      NDAT=1500
      DT=(TMAX-T0)/DBLE(NDAT-1)
      WRITE(10,*)'#APARTAT B:'
      THETA0=PI-0.05d0
      DTHETA0=0.d0
      CALL EULER(10, THETA0, DTHETA0)
      !INDEX3
      THETA0=PI-0.05d0
      DTHETA0=0.d0
      CALL BETTERCALLEULER(10, THETA0, DTHETA0)
      
!C)   !INDEX4
      WRITE(10,*)'#APARTAT C:'
      THETA0=PI-0.015d0
      DTHETA0=0.1d0
      CALL EULER(10, THETA0, DTHETA0)
      !INDEX5
      THETA0=PI-0.015d0
      DTHETA0=0.1d0
      CALL BETTERCALLEULER(10, THETA0, DTHETA0)
!D)   !INDEX6
      TMAX=14.d0*TN
      NDAT=5000
      DT=(TMAX-T0)/DBLE(NDAT-1)   
      WRITE(10,*)'#APARTAT D(+0.1):'
      THETA0=0.d0
      DTHETA0=(2.d0*WN)+0.1d0
      CALL BETTERCALLEULER(10, THETA0, DTHETA0)
!D2)  !INDEX7   
      WRITE(10,*)'#APARTAT D(-0.1):'
      THETA0=0.d0
      DTHETA0=(2.d0*WN)-0.1d0
      CALL BETTERCALLEULER(10, THETA0, DTHETA0)

!E)   !INDEX 8, 9, 10, 11 
      WRITE(10,*)'#APARTAT E:'
      TMAX=8.d0*TN
      NP=(/300,800,3000,45000/)
      DO I=1,4
        NDAT=NP(I)
        DT=(TMAX-T0)/DBLE(NDAT-1)
        WRITE(10,*)'#NDAT= ', NDAT
        THETA0=3.1d0
        DTHETA0=0.d0
        CALL BETTERCALLEULER(10, THETA0, DTHETA0) 
      ENDDO
      CLOSE(10)
!F)
      OPEN(UNIT=20, STATUS='UNKNOWN', FILE='P7-2016-c2-resF.dat')
      WRITE(20,*)'#APARTAT F:'
      TMAX=14.1d0*TN
      DO I=1, 100
        NDAT=I*400
        DT=(TMAX-T0)/DBLE(NDAT-1)
        THETA0=PI-0.1d0
        DTHETA0=0.01d0
        CALL BETTERCALLEULER2(20, THETA0, DTHETA0)       
      ENDDO
      CLOSE(20)
      ENDPROGRAM
!SUBROUTINES
!***********************************************************************            
      SUBROUTINE EULER(ARCHIVO,THETA0, DTHETA0)
      IMPLICIT NONE
      INTEGER K, NDAT, ARCHIVO
      REAL*8 G, L, PI, M, THETA0, DTHETA0, THETA1, DTHETA1
      REAL*8 T0, TK, DT, PEND(2), T, U, E, EKIN, EPOT
!     Vectors      
      COMMON /BLOCK1/ DT, T0, NDAT
      COMMON /BLOCK2/ L, G, M
      PARAMETER (PI=DACOS(-1.d0)) 
      
      WRITE(ARCHIVO,*)"#METODE D'EULER: "          
      WRITE(ARCHIVO,*)
      
      T= EKIN(THETA0, DTHETA0)
      U= EPOT(THETA0)
      E=T+U
      WRITE(ARCHIVO,*) T0, THETA0, DTHETA0, T, U, E
      DO K=0, NDAT-1
        TK=T0+K*DT
        CALL PENDULO(THETA0, DTHETA0, PEND)
        THETA1=THETA0+DT*PEND(1)
        DTHETA1=DTHETA0+DT*PEND(2)
        T= EKIN(THETA0, DTHETA0)
        U= EPOT(THETA0)
        E=T+U
        WRITE(ARCHIVO,*) TK, THETA1, DTHETA1, T, U, E
        THETA0=THETA1
        DTHETA0=DTHETA1
      ENDDO       
      WRITE(ARCHIVO,*) 
      WRITE(ARCHIVO,*)
      
      ENDSUBROUTINE   
!-----------------------------------------------------------------------          
      SUBROUTINE BETTERCALLEULER(ARCHIVO, THETA0, DTHETA0)
      IMPLICIT NONE
      INTEGER K, NDAT, ARCHIVO
      
      REAL*8 L,G,M,T0, TK, T1, KT, UPOT, EKIN, EPOT
      REAL*8 THETA0, THETA1, THETA2, DTHETA0, DTHETA1, DTHETA2, DT
      REAL*8 PEND(2), T, U, E
      
      COMMON /BLOCK1/ DT, T0, NDAT
      COMMON /BLOCK2/ L, G, M
      
      WRITE(ARCHIVO,*) "#METODE D'EULER MILLORAT: "
      WRITE(ARCHIVO,*) 
!     Escrivim el punt inicial
      T= EKIN(THETA0, DTHETA0)
      U= EPOT(THETA0)
      E=T+U
      WRITE(ARCHIVO,*) T0, THETA0, DTHETA0, T, U, E

!     Calculem el primer punt per Euler simple
      T1=T0+DT
      CALL PENDULO(THETA0, DTHETA0, PEND)
      THETA1=THETA0+DT*PEND(1)
      DTHETA1=DTHETA0+DT*PEND(2)
      T= EKIN(THETA1, DTHETA1)
      U= EPOT(THETA1)
      E= T+U
      WRITE(ARCHIVO,*) T1, THETA1, DTHETA1, T, U, E 

     
!     Euler millorat                
      DO K=3, NDAT-1
        CALL PENDULO(THETA1, DTHETA1, PEND)
        TK=T0+K*DT
        THETA2=THETA0+2.d0*DT*PEND(1)
        DTHETA2=DTHETA0+2.d0*DT*PEND(2)
        T= EKIN(THETA2, DTHETA2)
        U= EPOT(THETA2)
        E=T+U   
        WRITE(ARCHIVO,*) TK, THETA2, DTHETA2, T, U, E
        THETA0=THETA1
        DTHETA0=DTHETA1
        THETA1=THETA2
        DTHETA1=DTHETA2
      ENDDO
      
      WRITE(ARCHIVO,*) 
      WRITE(ARCHIVO,*)

      ENDSUBROUTINE
!-----------------------------------------------------------------------          
      SUBROUTINE PENDULO(THETA,DTHETA,F)
      IMPLICIT NONE
      REAL*8 F(2), DTHETA, GG, XLONGI, THETA, L, G, M   
      COMMON /BLOCK2/ L, G, M
      F(1)=DTHETA
      F(2)=-G/L*DSIN(THETA)
      RETURN
      END     
!-----------------------------------------------------------------------
      SUBROUTINE BETTERCALLEULER2(ARCHIVO, THETA0, DTHETA0)
      IMPLICIT NONE
      INTEGER K, NDAT, ARCHIVO
      
      REAL*8 L,G,M,T0, TK, T1, KT, UPOT, EKIN, EPOT
      REAL*8 THETA0, THETA1, THETA2, DTHETA0, DTHETA1, DTHETA2, DT
      REAL*8 PEND(2), T, U, E
      
      COMMON /BLOCK1/ DT, T0, NDAT
      COMMON /BLOCK2/ L, G, M
             
!     Calculem el primer punt per Euler simple
      CALL PENDULO(THETA0, DTHETA0, PEND)
      THETA1=THETA0+DT*PEND(1)
      DTHETA1=DTHETA0+DT*PEND(2)
            
!     Euler millorat                
      DO K=3, NDAT-1
        CALL PENDULO(THETA1, DTHETA1, PEND)
        TK=T0+K*DT
        THETA2=THETA0+2.d0*DT*PEND(1)
        DTHETA2=DTHETA0+2.d0*DT*PEND(2)
        T= EKIN(THETA2, DTHETA2)
        U= EPOT(THETA2)
        E=T+U   
        THETA0=THETA1
        DTHETA0=DTHETA1
        THETA1=THETA2
        DTHETA1=DTHETA2
      ENDDO
      WRITE(ARCHIVO,*) DT, THETA1, DTHETA1, T, U

      ENDSUBROUTINE
!FUNCIONS      
!***********************************************************************     
      REAL*8 FUNCTION EKIN(THETA, DTHETA)
      IMPLICIT NONE
      REAL*8 THETA,DTHETA
      REAL*8 L,G,M
      COMMON /BLOCK2/ L, G, M

      EKIN=0.5d0*M*(DTHETA*L)**2.d0
      ENDFUNCTION
!-----------------------------------------------------------------------      
      REAL*8 FUNCTION EPOT(THETA)
      IMPLICIT NONE
      REAL*8 THETA,DTHETA
      REAL*8 L,G,M
      COMMON /BLOCK2/ L, G, M     
      EPOT=-M*G*L*DCOS(THETA)
      ENDFUNCTION
