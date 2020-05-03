!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<PRACTICA 3 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!Programa que integra varies funcions mitjançant la subrutina
!externa MyIntegrator. ATENCIO!! ORDRE SUBRUTINA: A, L, M, IM, VALOO, FCN
!OP OCT 2016
	PROGRAM PRACTICA3
	  IMPLICIT NONE
!	Declarem variables
	  REAL*8 PI, AREATR1, AREASI1,ERRORTR2, ERRORSI2
	  REAL*8 L, H, ERRORSIMP, ERRORTRAP, VALORTR, VALORSI, A, B, T
	  REAL*8 EXAC, AREATR2,AREASI2, YCROMMELIN, Z
	  INTEGER IM, M
	  EXTERNAL YCROMMELIN
	  COMMON /PASO/ H
	  PARAMETER(PI=4.d0*DATAN(1.d0))
!	Constants modificables								
	  A=1376.3D0  !km*10**6
	  B=542.617D0 !km*10**6 
	  T=27.89D0 !ANYS
	
!	Formats
!10	  FORMAT(F20.14)
100   FORMAT(E20.14, 2X, E20.14, 2X, E20.14)
 
!  	Arxius necessaris
	  OPEN(UNIT=1,STATUS='UNKNOWN',FILE='P3-2016-c2-res1.dat')    	  
	  OPEN(UNIT=2,STATUS='UNKNOWN',FILE='P3-2016-c2-res2.dat')  
	  OPEN(UNIT=3,STATUS='UNKNOWN',FILE='P3-Erroresf1.dat')
	  OPEN(UNIT=4,STATUS='UNKNOWN',FILE='P3-Erroresf2.dat')	
!	CALCUL INTEGRAL I ESTUDI ERRORS (apartat 2)
!---------------------------------------------------------------------------------------
	DO M=4, 22
	A=1376.3D0
	B=542.617D0
	!H=B/(2**M)
	EXAC=PI*A*B
	Z=(-(5.D0*B)/2.D0)
	CALL MYINTEGRATOR(Z, B, M, 1, VALORTR, YCROMMELIN)
	AREATR1=4.0D0*VALORTR
	ERRORTRAP=DABS(AREATR1-EXAC)
	A=1376.3D0  
	B=542.617D0
	Z=(-(5.D0*B)/2.D0)
	CALL MYINTEGRATOR(Z, B, M, 2, VALORSI, YCROMMELIN)
	AREASI1=4.0D0*VALORSI
	ERRORSIMP=DABS(AREASI1-EXAC)
	WRITE(1,100), H, AREATR1, AREASI1
	WRITE(3,100) H, ERRORTRAP, ERRORSIMP
	A=1376.3D0
	B=542.617D0	 
!	2)C
	EXAC=A*B*(3.D0*DSQRT(3.D0)+2.D0*PI)/24.D0
	Z=-(9.d0*B)/4.d0
	CALL MYINTEGRATOR (Z, B/2.D0, M, 1, AREATR2, YCROMMELIN)
	CALL MYINTEGRATOR (Z, B/2.D0, M, 2, AREASI2, YCROMMELIN)
	WRITE(2,100), H, AREATR2, AREASI2
	ERRORTR2=DABS(EXAC-AREATR2)
	ERRORSI2=DABS(EXAC-AREASI2)
	WRITE(4,100) H, ERRORTR2,ERRORSI2

		ENDDO  
!----------------------------------------------------------------------------------------
	  CLOSE(1)
	  CLOSE(2)	
	  CLOSE(3)
	  CLOSE(4)
	 
	END
	
!Funcio YCrommelin
	REAL*8 FUNCTION YCROMMELIN(X)
	  IMPLICIT NONE
	  REAL*8 A, B, T, X, K, Y
	  PARAMETER (A=1376.3D0)	!km*10.D0**6.D0
	  PARAMETER (B=542.617D0)	!km*10.D0**6.D0
	  PARAMETER (T=27.89D0) !Anys
	  
	  Y=((X+(2.0D0*B))**(2.D0))/(B**2.D0)
	  K=1.D0-Y
	  IF(K.LT.0) THEN
	    YCROMMELIN=0.D0
	  ELSE
	    YCROMMELIN=A*SQRT(K)
        
	  ENDIF
	END