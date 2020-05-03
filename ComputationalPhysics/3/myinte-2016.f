!<<<<<<<<<<<<<<<<<<<<<<<<<<< MY INTEGRATOR (TRAPEZIS/SIMPSON) >>>>>>>>>>>>>>>>>>>>>>>>>>>
!Subrutina que calcula la integral entre dos puntos por el metodo de los trapecios/Simpson
!OP OCT 2016
	SUBROUTINE MYINTEGRATOR(A, L, M, IM, VALOR, FCN)	  
		IMPLICIT NONE
	    REAL*8 X0, X1, H, VALOR, XK
		REAL*8 A, L,FCN, ERROR
		INTEGER IM, K, MAXITER, M
		COMMON /PASO/ H, ERROR
		EXTERNAL FCN
		
	    X0=A-(L/2.0D0)			!Punt inicial
	    X1=A+(L/2.0D0)			!Punt final
		MAXITER=2.D0**M			!N? iteracions
		H=(X1-X0)/DBLE(MAXITER)	!Pas
		
		VALOR=0
		DO K=1, MAXITER-1 
!	Si IM=1--> Aplicamos el metodo de los trapecios.
		  IF(IM.EQ.1) THEN
			XK=X0+K*H
	        VALOR = VALOR + H*FCN(XK)
!	Si IM=2-->Aplicamos el metodo de simpson
		  ELSE IF(IM.EQ.2) THEN
			XK=X0+K*H
		    IF(MOD(K,2).EQ.0) THEN
			  VALOR=VALOR+(2.D0*H/3.0D0)*FCN(XK)
			ELSE 
			  VALOR=VALOR+(4.D0*H/3.0D0)*FCN(XK)
			ENDIF
		  ELSE
		    PRINT*, 'Valor IM no valid'
		  ENDIF
!		El error mas duro de mi vida (oct 2016)		  
		ENDDO
 !		Valor en los extremos
		IF(IM.EQ.1) THEN 	!Trapecios
		  VALOR = VALOR + (H/2.0D0)*(FCN(X0) + FCN(X1))
!		  ERROR=(X1-X0)*(H**2.D0)
		ELSE IF(IM.EQ.2) THEN	!Simpson
		  VALOR = VALOR + (H/3.0D0)*(FCN(X0) + FCN(X1))
!		  ERROR=H**4.D0
		ENDIF
	
	END	
	
