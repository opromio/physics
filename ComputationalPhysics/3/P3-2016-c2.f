!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< PREPRACTICA 3 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!Programa que integra varies funcions mitjançant la subrutina
!externa MyIntegrator. ATENCIO!! ORDRE SUBRUTINA: A, L, M, IM, VALOR, FCN
!OP OCT 2016
	PROGRAM PRACTICA3
	  IMPLICIT NONE
!	Declarem variables
	  REAL*8 VALOR, PI
	  REAL*8 A, L, R, DENS, H, ERROR, ERRORTRAP
	  REAL*8 LONGSIMP, LONGTRAP, MASSSIMP, MASSTRAP
	  INTEGER IM, M, IFU, MAXITER
	  EXTERNAL MIFUN
	  COMMON /IFU/ IFU
	  COMMON /PASO/ H!, ERROR
	  PARAMETER(PI=4.d0*ATAN(1.d0))
!	Constants modificables								
	  PARAMETER (R=3.325D0) !cm
!	Formats
10	  FORMAT(F14.8)
100	  FORMAT(E14.8, 2X, E14.8, 2X, E14.8)
!	Arxius necessaris
	  OPEN(UNIT=1,STATUS='UNKNOWN',FILE='P3-2016-res1.dat')    	  
	  OPEN(UNIT=2,STATUS='UNKNOWN',FILE='P3-2016-res2.dat')  
	  OPEN(UNIT=3,STATUS='UNKNOWN', FILE='P3-2016-res3.dat')
	  OPEN(UNIT=4,STATUS='UNKNOWN',FILE='P3-2016-res4.dat')
	  OPEN(UNIT=5,STATUS='UNKNOWN',FILE='P3-Erroresf1.dat')
	  OPEN(UNIT=6,STATUS='UNKNOWN',FILE='P3-Erroresf2.dat')
	  OPEN(UNIT=7,STATUS='UNKNOWN',FILE='P3-Erroresf3.dat')
!  	CALCUL INTEGRAL (apartat 3)
!--------------------------------------------------------------------------------------
	  M=10
	  !Longitud
	  IFU=0    	
	  CALL MYINTEGRATOR (0.0D0, 2.0D0*R, M, 1, VALOR, MIFUN)
	  !ERRORTRAP= ABS(VALOR-(PI*3.325D0))
	  !WRITE(1,*) 'Trapezis:', VALOR, 'Error: ', ERRORTRAP
	  WRITE(1,*) 'Longitud: '
	  WRITE(1,*)
	  WRITE(1,*) 'Trapezis'
	  WRITE(1,10) VALOR
	  
	  CALL MYINTEGRATOR (0.0D0, 2.0D0*R, M, 2, VALOR, MIFUN)
	  !ERROR= ABS(VALOR-(PI*3.325D0))
	  !WRITE(1,*) 'Simpson:', VALOR, 'Error: ', ERROR
	  WRITE(1,*) 'Simpson:'
	  WRITE(1,10) VALOR
	  WRITE(1,*) 
	  !Massa
	  IFU=1
	  CALL MYINTEGRATOR (0.0D0, 4.0D0, M, 1, VALOR, MIFUN)
	  L=4.0D0 !m
	  DENS=1.42D0 !Kg/m
	  ERRORTRAP=ABS(VALOR-(DENS*7.D0*PI*L/16.0D0))
	  !WRITE(1,*) 'Trapezis:', VALOR, 'Error: ', ERRORTRAP
	  WRITE(1,*) 'Massa:'
	  WRITE(1,*)
	  WRITE(1,*) 'Trapezis'
	  WRITE(1,10) VALOR
	  CALL MYINTEGRATOR (0.0D0, 4.0D0, M, 2, VALOR, MIFUN)
	  L=4.0D0 !m
	  DENS=1.42D0 !Kg/m
	  ERROR=ABS(VALOR-(DENS*7.D0*PI*L/16.0D0))
	  !WRITE(1,*) 'Simpson:', VALOR, 'Error: ', ERROR 
	  WRITE(1,*) 'Simpson:'
	  WRITE(1,10) VALOR
	  WRITE(1,*) 
	  
!	ESTUDI ERRORS (apartat 4 i 5)
!---------------------------------------------------------------------------------------
		DO M=2, 20
!		Funcio 1
	  	IFU=0
		CALL MYINTEGRATOR (0.0D0, 2.0D0*R, M, 1, LONGTRAP, MIFUN)
		ERRORTRAP=ABS(LONGTRAP-PI*R)	
		CALL MYINTEGRATOR (0.0D0, 2.0D0*R, M, 2, LONGSIMP, MIFUN)	
		ERROR=ABS(LONGSIMP-(PI*R))
		WRITE(2,100), H, LONGTRAP, LONGSIMP
		WRITE(5,100), H, ERRORTRAP, ERROR
!		Funcio 2
		IFU=1
		CALL MYINTEGRATOR (0.0D0, 4.0D0, M, 1, MASSTRAP, MIFUN)
		L=4.0D0
	    DENS=1.42D0
		ERRORTRAP=ABS(MASSTRAP-(DENS*7.D0*PI*L/16.0D0))
		CALL MYINTEGRATOR (0.0D0, 4.0D0, M, 2, MASSSIMP, MIFUN)
		L=4.0D0
	    DENS=1.42D0
		ERROR=ABS(MASSSIMP-(DENS*7.D0*PI*L/16.0D0))
		WRITE(3,100), H, MASSTRAP, MASSSIMP
	    WRITE(6,100), H, ERRORTRAP, ERROR            
!		Funcio 2 amb canvi de variable
	    IFU=2
	    CALL MYINTEGRATOR (0.0D0, PI, M, 1, MASSTRAP, MIFUN)
		L=4.0D0
	    DENS=1.42D0
	    ERRORTRAP=ABS(MASSTRAP-(DENS*7.D0*PI*L/16.0D0))
	    CALL MYINTEGRATOR (0.0D0, PI, M, 2, MASSSIMP, MIFUN)
		L=4.0D0
	    DENS=1.42D0
		ERROR=ABS(MASSSIMP-(DENS*7.D0*PI*L/16.0D0))
	    WRITE(4,100), H, MASSTRAP, MASSSIMP
	    WRITE(7,100), H, ERRORTRAP, ERROR	
		ENDDO  
	  CLOSE(1)
	  CLOSE(2)	
	  CLOSE(3)
	  CLOSE(4)
	  CLOSE(5)
	  CLOSE(6)
	  CLOSE(7)
	END
	
!Funcio a integrar (depen de IFU)
	REAL*8 FUNCTION MIFUN(X)
	  IMPLICIT NONE
	  REAL*8 X, R
	  REAL*8 DENS, L
	  INTEGER IFU
	  COMMON /IFU/ IFU
	  
	  PARAMETER (L=4.0D0) !m
	  PARAMETER (R=3.325D0) !cm
	  PARAMETER (DENS=1.42D0) !Kg/m
!	  Funció per IFU=0
	  IF(IFU.EQ.0) THEN
!		Evitem la singularitat en R i -R
	    IF((X.EQ.R).OR.(X.EQ.-R)) THEN
		  MIFUN = 0.D0
		ELSE
		  MIFUN=DSQRT((R**2.D0)/((R**2.D0)-(X**2.D0)))
		ENDIF
!	  Funcio per IFU=1 		
	  ELSE IF(IFU.EQ.1) THEN
		MIFUN=DENS*DSQRT(1.0D0-(2.D0*X/L)**2.D0)*(1.0D0-(2.D0*X/L))**3.D0 			 !x e [-L/2, L/2]
!	  Funcio per IFU=2		
	  ELSE IF(IFU.EQ.2) THEN
	    MIFUN=L*DCOS(X)/2.D0*DENS*DCOS(X)*(1.0D0-DSIN(X))**3.D0          !CANVI DE VARIABLE: SIN(T)=2X/L
!	  Per qualsevol altre valor d'IFU
	  ELSE
	    PRINT*, 'Valor IFU no valid'
	  ENDIF
	END  
		