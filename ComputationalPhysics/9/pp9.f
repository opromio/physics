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
      PARAMETER(LX=40.d0, LY=27.5d0) !cm
!     Paso en X e Y     
      PARAMETER (DX=0.5d0, DY=0.5d0)
      PARAMETER(NX=INT(LX/DX), NY=INT(LY/DY))  
!     Numero maximo iteraciones
      PARAMETER (NMAX=1.d5)
!     Matrices temperatura      
      REAL*8 TNEW(0:NX,0:NY), TOLD(0:NX,0:NY), ERROR, TOL, OMEGA, RHO
      
      OPEN(UNIT=10, STATUS='UNKNOWN', FILE='0.dat')
      OPEN(UNIT=100, STATUS='UNKNOWN', FILE='3DMAP.dat')
      
      TOL=1.d-3
      ICONTROL=3
      OMEGA=1.6d0
      TINT=440.d0
      
      IF(ICONTROL.eq.1) THEN
        WRITE(10,*) '#GAUSS-SEIDEL: '
      ELSE IF(ICONTROL.eq.2) THEN
        WRITE(10,*) '#SOBRE-RELAXACIO: '
      ELSE
        WRITE(10,*) '#JACOBI: '
      ENDIF
      
!     Inicializamos matrices con los puntos contorno 
      DO I=0, NX
        TOLD(I,0)=10.d0
        TNEW(I,0)=10.d0
        TOLD(I,NY)=13.6d0
        TNEW(I,NY)=13.6d0
      ENDDO
      
      DO J=0, NY  
        TOLD(0,J)=25.d0
        TNEW(0,J)=25.d0
        TOLD(NX,J)=35.d0
        TNEW(NX,J)=35.d0       
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
            IF ((XI.EQ.14.5d0).AND.(YJ.EQ.10.5)) THEN
              WRITE(10,*) K, TNEW(I,J)
            ENDIF
          ELSE IF(ICONTROL.EQ.2) THEN
!         SOBRE-RELAXACIO          
!-----------------------------------------------------------------------          
          TNEW(I,J)=TOLD(I,J)+OMEGA*(TOLD(I+1,J)+TNEW(I-1,J)+TOLD(I,J+1)
     &    +TNEW(I,J-1)-4.d0*TOLD(I,J)+RHO(XI,YJ)*(DX*DY))/4.d0       
            IF ((XI.EQ.14.5d0).AND.(YJ.EQ.10.5)) THEN
              WRITE(10,*) K, TNEW (I,J)
            ENDIF
          ELSE
!         JACOBI
!-----------------------------------------------------------------------          
          TNEW(I ,J)=(TNEW(I+1,J)+TNEW(I-1,J)+TNEW(I,J+1)+TNEW(I,J-1)+
     &    RHO(XI,YJ)*(DX*DY))/4.d0
            IF ((XI.EQ.14.5d0).AND.(YJ.EQ.10.5d0)) THEN
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
!     OUTPUT       
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
!     Funcio Rho      
      DOUBLE PRECISION FUNCTION RHO(X,Y)
      IMPLICIT NONE
      DOUBLE PRECISION RHO10, R, RHO1, RHO2, XF, YF, XC, YC, X, Y

      RHO10=10.0D0
      R=DSQRT(((X-7.0D0)**2)+((Y-18.0D0)**2))
      
      RHO1=RHO10*DEXP(-((R-3.0D0)**2)/((0.5D0)**2))

      XF=8.0D0
      YF=12.0D0
      XC=30.0D0
      YC=7.0D0

      IF ((X.GE.(XC-(XF/2.0D0))).AND.(X.LE.(XC+(XF/2.0D0)))) THEN
       IF ((Y.GE.(YC-(YF/2.0D0))).AND.(Y.LE.(YC+(YF/2.0D0)))) THEN
        RHO2=3.0D0
       ELSE
        RHO2=0.0D0
       ENDIF
      ELSE
       RHO2=0.0D0
      ENDIF

      RHO=RHO1+RHO2
      END

    
    
    ! ! Rho total      
      ! DOUBLE PRECISION FUNCTION RHO(X,Y)
      ! IMPLICIT NONE
      ! REAL*8 X, Y, RHO1, RHO2
        ! RHO=RHO1(X,Y)+ RHO2(X,Y)
      ! ENDFUNCTION
            
    ! ! Fogo 1
      ! DOUBLE PRECISION FUNCTION RHO1(X,Y)
      ! IMPLICIT NONE
      ! REAL*8 X, Y, RHO10, MODUL
        ! RHO10=10.d0
        ! RHO1=RHO10*DEXP(-4.d0*((MODUL(X,Y)-3.d0)**2))     
      ! ENDFUNCTION
      
    ! ! Fogo 2     
      ! REAL*8 FUNCTION RHO2(X,Y)
	! IMPLICIT NONE
	! REAL*8 X, Y
	! IF ((X.LE.34.D0) .AND. (X.GE.26.D0)) THEN
        ! IF ((Y.LE.13.D0) .AND. (Y.GE.1.D0)) THEN
	    ! RHO2 = 3.D0
        ! ELSE 
	    ! RHO2 = 0.D0
	  ! ENDIF
	  ! ELSE
          ! RHO2 = 0.D0
      ! ENDIF
	! ENDFUNCTION
	    
    ! ! Funcio que calcula el modul desde (7,18)      
      ! DOUBLE PRECISION FUNCTION MODUL(X,Y)
      ! IMPLICIT NONE
      ! REAL*8 X, Y
        ! MODUL=DSQRT(((X-7.d0)**2)+((Y-18.d0)**2))
      ! ENDFUNCTION
      