!<<<<<<<<<<<<<<<<<<<<<<<<<<<PRACTICA9>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!OP DEC 2016
!     MAIN
      PROGRAM PRACTICA9
      IMPLICIT NONE
      
      INTEGER NX, NY, NMAX
      INTEGER I,J,K, ICONTROL
!     Constantes
      REAL*8 LX, LY, DX, DY, XI, YJ, TINT  
!     Dimensiones caja      
      PARAMETER(LX=44.5d0, LY=32.5d0) !cm
!     Paso en X e Y     
      PARAMETER (DX=0.5d0, DY=0.5d0)
      PARAMETER(NX=INT(LX/DX), NY=INT(LY/DY))  
!     Numero maximo iteraciones
      PARAMETER (NMAX=1.d5)
!     Matrices temperatura      
      REAL*8 TNEW(0:NX,0:NY), TOLD(0:NX,0:NY), ERROR, TOL, OMEGA, RHO
      
      OPEN(UNIT=10, STATUS='UNKNOWN', FILE='SR15.dat')
      OPEN(UNIT=100, STATUS='UNKNOWN', FILE='3DMAP.dat')
      
      TOL=1.d-3
      ICONTROL=3    !  1-> GAUSS-SEIDEL   2--> JACOBI   3--> SOBRE-RELAXACIO
      OMEGA=1.55d0
      TINT=15.d0
      
      IF(ICONTROL.eq.1) THEN
        WRITE(10,*) '#GAUSS-SEIDEL: '
      ELSE IF(ICONTROL.eq.2) THEN
        WRITE(10,*) '#JACOBI: '
      ELSE
        WRITE(10,*) '#SOBRE-RELAXACIO: '
      ENDIF
      
!     Inicializamos matrices con los puntos contorno 
      DO I=0, NX
        TOLD(I,0)=17.d0
        TNEW(I,0)=17.d0
        TOLD(I,NY)=25.3d0
        TNEW(I,NY)=25.3d0
      ENDDO
      
      DO J=0, NY  
        TOLD(0,J)=0.5d0
        TNEW(0,J)=0.5d0
        TOLD(NX,J)=11.2d0
        TNEW(NX,J)=11.2d0       
      ENDDO
!     Inicializamos el resto de puntos (a 1 p.ej.)      
      DO I=1,NX-1
	  DO J=1,NY-1
	    TOLD(I,J)=TINT
	    TNEW(I,J)=TINT
	  ENDDO
	ENDDO

      K=0
!     Comienza el metodo      
10    CONTINUE    
      K=K+1
      ERROR=0.D0
!     Inicio bucle
      DO I=1, NX-1
        XI=I*DX
        DO J=1, NY-1
          YJ=J*DY
          IF(ICONTROL.EQ.1) THEN
!         GAUSS-SEIDEL
!-----------------------------------------------------------------------          
          TNEW(I,J)=(TOLD(I+1,J)+TOLD(I-1,J)+TOLD(I,J+1)+TOLD(I,J-1)+
     &    (RHO(XI,YJ)*(DX*DY)))/4.d0                
            IF ((XI.EQ.25.5d0).AND.(YJ.EQ.13.5)) THEN
              WRITE(10,*) K, TNEW(I,J)
            ENDIF
          ELSE IF(ICONTROL.EQ.2) THEN
!         JACOBI
!-----------------------------------------------------------------------          
          TNEW(I ,J)=(TNEW(I+1,J)+TNEW(I-1,J)+TNEW(I,J+1)+TNEW(I,J-1)+
     &    RHO(XI,YJ)*(DX*DY))/4.d0
            IF ((XI.EQ.25.5d0).AND.(YJ.EQ.13.5)) THEN
              WRITE(10,*) K, TNEW (I,J)
            ENDIF
          ELSE
!         SOBRE-RELAXACIO          
!-----------------------------------------------------------------------          
          TNEW(I,J)=TOLD(I,J)+OMEGA*(TOLD(I+1,J)+TNEW(I-1,J)+TOLD(I,J+1)
     &    +TNEW(I,J-1)-4.d0*TOLD(I,J)+RHO(XI,YJ)*(DX*DY))/4.d0       
            IF ((XI.EQ.25.5d0).AND.(YJ.EQ.13.5)) THEN
              WRITE(10,*) K, TNEW (I,J)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!     Calcul errors     
      ERROR=0.0D0
      DO I=1,(NX-1)
        DO J=1,(NY-1) 
          ERROR=ERROR+DABS((TOLD(I,J)-TNEW(I,J))/(TOLD(I,J)+TNEW(I,J)))
          TOLD(I,J)=TNEW(I,J)
        ENDDO
      ENDDO
!     Fin bucles   
!     Test convergencia     
      IF ((K.lt.NMAX).AND.(ERROR.gt.TOL)) GOTO 10
!     OUTPUT     3D  
      DO I=0, NX
        XI=I*DX
        DO J=0, NY
          YJ=J*DY
          WRITE(100,*) XI, YJ, TNEW(I,J)
        ENDDO
        WRITE(100,*)
      ENDDO  
!     Fin metodo  

      ENDPROGRAM
!***********************************************************************      
    ! ! Funcio Rho      
      ! DOUBLE PRECISION FUNCTION RHO(X,Y)
      ! IMPLICIT NONE
      ! DOUBLE PRECISION RHO10, RHO1, RHO2, RHO3, XF, YF, XC, YC, X, Y
      ! REAL*8 RHO30, R1, R3

      ! RHO10=10.0D0
      ! R1=DSQRT(((X-22.5d0)**2)+((Y-8.d0)**2))
      
      ! RHO1=RHO10*DEXP(-((R1-4.0D0)**2)/((0.7D0)**2))

      ! XF=6.0D0
      ! YF=4.0D0
      ! XC=32.0D0
      ! YC=20.0D0

      ! IF ((X.GE.(XC-(XF/2.0D0))).AND.(X.LE.(XC+(XF/2.0D0)))) THEN
       ! IF ((Y.GE.(YC-(YF/2.0D0))).AND.(Y.LE.(YC+(YF/2.0D0)))) THEN
            ! RHO2=7.0D0
       ! ELSE
        ! RHO2=0.0D0
       ! ENDIF
      ! ELSE
       ! RHO2=0.0D0
      ! ENDIF

      ! RHO30=5.5D0
      ! R3=DSQRT(((X-10.5d0)**2)+((Y-22.d0)**2))
      
      ! RHO3=RHO30*DEXP(-((R3-5.0D0)**2)/((1.2D0)**2))

      
      ! RHO=RHO1+RHO2+RHO3
      ! END
      
!     RHO SENSE FONTS DE CALOR
      DOUBLE PRECISION FUNCTION RHO(X,Y)
      IMPLICIT NONE
      DOUBLE PRECISION X, Y
      RHO=0.d0
      END

    