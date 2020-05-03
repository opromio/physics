!>>>>>>>>>>>>>>>>> PREPRACTICA 1 <<<<<<<<<<<<<<<<<<<<
!Programa que calcula, per un valor de K, P(K)=(K^2+K+2)/2 i despres fa mes coses.
!OP SEP 2016
	PROGRAM PROBLEMA1
	IMPLICIT NONE	
! 	Declarem variables
	REAL P, LAZY, S
	INTEGER I, M, N, K
	INTEGER IEST
	REAL SASI, Q, S8
	DOUBLE PRECISION SUMA
!		Input
5		WRITE(*,*) 'Introdueix un valor per K'
		READ(*,*,IOSTAT=IEST), K
		
		IF((K.LT.15).OR.(K.GT.221).OR.(IEST.NE.0)) THEN
			WRITE(*,*) 'Error'
			GOTO 5
		ELSE
10		WRITE(*,*) 'Introdueix un valor per M i N'
		READ(*,*,IOSTAT=IEST) M, N
		
		IF((N.LT.0).OR.(M.LT.0).OR.(IEST.NE.0)) THEN
			WRITE(*,*) 'Error'
			GOTO 10
		ELSE
!		Cos del programa
			P=LAZY(K)
			S=SUMA(M,N)	
			OPEN(UNIT=1,STATUS='UNKNOWN',FILE='P1-2016-res1.dat')
			WRITE(1,*)'     #N       S8N        SASI         Q'
1000		!!!FORMAT(I10,2X,F14.4, 2X, F14.4, 2X, F9.8)
			DO N= 11,311, 3
				S8=SUMA(8,N)
				SASI=(N**3)/5.0
				Q=S8/SASI				
				WRITE(1,*) N, S8, SASI, Q
			ENDDO
!		Output 								
			WRITE(*,*) 'P = ', P
			WRITE(*,*) 'S= ', S
			CLOSE (UNIT=1)
		ENDIF
		ENDIF
	END
!	Funcio que calcula el valor P(K)
	REAL FUNCTION LAZY(K)
	IMPLICIT NONE
	REAL EN,P
	PARAMETER (EN=2.71828182846)
	INTEGER K
	P=((K**2.0)*(3/5.0)+10.0*K+EN)
	LAZY=P
	RETURN
	END FUNCTION
	
!	Funcio que calcula el sumatori	
	DOUBLE PRECISION FUNCTION SUMA(M,N)
	IMPLICIT NONE
	INTEGER M, N,I
	REAL T, LAZY
		SUMA=0.0D0
		DO I=M,N
			SUMA=SUMA+LAZY(I)
		ENDDO
	RETURN
	ENDFUNCTION
