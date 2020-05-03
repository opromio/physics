!>>>>>>>>>>>>>>>>> PREPRACTICA 1 <<<<<<<<<<<<<<<<<<<<
!Programa que calcula, per un valor de K, P(K)=(K^2+K+2)/2 i despres fa mes coses.
!OP SEP 2016
	PROGRAM PROBLEMA1
	IMPLICIT NONE	
! 	Declarem variables
	INTEGER P, K, LAZY
	INTEGER S, M, N, SUMA
	INTEGER S4, I
	INTEGER IEST
	REAL SASI,Q

!		Input
5		WRITE(*,*) 'Introdueix un valor per K'
		READ(*,*,IOSTAT=IEST) K
		
		IF((K.LT.0).OR.(IEST.NE.0)) THEN
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
			!!!OPEN(UNIT=2,STATUS='UNKNOWN', FILE='P1-2016-fig1.dat')
			!!!OPEN(UNIT=3,STATUS='UNKNOWN', FILE='P1-2016-fig2.dat')
			
			DO N= 5,201, 2
				S4=SUMA(4,N)
				!!!WRITE(1,*) N, S4, SASI, Q
				SASI=(N**3)/6
				!!!WRITE(2,*) N, SASI !!!
				Q=S4/SASI
				!!!WRITE(3,*) N, Q
				WRITE(1,*) N, S4, SASI, Q
			ENDDO
!		Output 								+ Cambiar archivo gnuplot y no usar "using 1:2" y "using 1:3"
			WRITE(*,*) 'P = ', P
			WRITE(*,*) 'S= ', S
			CLOSE (UNIT=1)
			!!!CLOSE (UNIT=2)!!!
			!!!CLOSE (UNIT=3)
		
		ENDIF
		ENDIF
	END
!	Funcio que calcula el valor P(K)
	INTEGER FUNCTION LAZY(K)
		INTEGER P,K
		P=((K**2)+K+2)/2
		LAZY=P
	RETURN
	END FUNCTION
!	Funcio que calcula el sumatori	
	INTEGER FUNCTION SUMA(M,N)
	INTEGER M, N
		DO I=M,N
			P=P+LAZY(I)
		ENDDO
		SUMA=P
	RETURN
	ENDFUNCTION
