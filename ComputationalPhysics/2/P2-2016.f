!<<<<<<<<<<<<<<<<<<<<<<<<PREPRACTICA 2>>>>>>>>>>>>>>>>>>>>>>
!Programa que calcula la posició dels 4 pistons d'un motor 
!per uns certs valors de L,w,t. Després calcula per interpolacio
!els valors de la funcio entre els punts.
!OP OCT 2016
	PROGRAM PRACTICA2
		IMPLICIT NONE
! 		Comptadors/constants/variables auxiliars.
		INTEGER I, J, K, NP, NMAX, NPIST
		REAL*8 L, W ,T, TFIN, RADI, TMAX, TMIN, TINI
!		CONSTANTS MODIFICABLES
		PARAMETER (NPIST=4)			!Nº pistons
		PARAMETER (NMAX=41)			!Nº punts calculats en l'interval [TMIN,TMAX] 
		PARAMETER (TMIN=0)			!Temps inicial
		PARAMETER (TMAX=4) 			!Temps final
		PARAMETER (NP=1000)			!Nº punts interpolacio
		PARAMETER (TINI=0)			!Temps inicial a partir del que es calcula la interpolacio
		PARAMETER (TFIN=2.0)		!Temps final fins el que es calcula la interpolacio
		PARAMETER (W=5.0) 			!Frequencia (Hz)
		PARAMETER (L=20.0) 			!Longitud bieles (cm)
!		Variables i matrius		
		REAL*8 XI(NMAX),TI(NMAX), X(NPIST)
		REAL*8 TRASH, XINTERPO, PASO, T0INT
		COMMON /POSIS/ XI,TI
		!!!FORMATS (ACTUAL: 6 CIFRAS SIGNIFICATIVAS)
1000	FORMAT(F4.1,2X,F15.6, 2X, F15.6, 2X, F15.6, 2X, F15.6) 
2000	FORMAT(F14.3,2X,F15.6)

!		Escrivim t,x1,x2,x3,x4 en el fitxer dat
		OPEN(UNIT=1,STATUS='UNKNOWN',FILE='P2-2016-res1.dat')
		PASO=(TMAX-TMIN)/(DBLE(NMAX)-1.0)
		DO I=0, NMAX-1 		
			T=TMIN+(DBLE(I)*PASO)
			CALL POSICIONES(L,W,T,X)
			WRITE(1,1000) T, X 
		ENDDO
		CLOSE(1)

!		Llegim les columnes 1,3 (pisto 2) del mateix fitxer.	
		OPEN(UNIT=2,STATUS='UNKNOWN',FILE='P2-2016-res1.dat')	!Es pot modificar la lectura de columnes canviant l'ordre de
		DO J=1,NMAX												!TI(J) i XI(J) en el READ. Si hi haguessin mes de 4 pistons afegim
			READ(2,*) TI(J), TRASH, XI(J), TRASH, TRASH			!TRASH on correspongui pels valors que NO volguem agafar.
		ENDDO
		CLOSE(2)
		!!!!!!NP-1 PER A QUE SIGUIN 1000 PUNTS, SINO COBRIRIA TOT L'INTERVAL DE 0 A 2 I SERIEN 1001 PUNTS. 
!		Obrim un 2n fitxer on guardem la taula amb els valors de la interpolacio.
		OPEN(UNIT=3,STATUS='UNKNOWN',FILE='P2-2016-res2.dat')																						
		PASO=(TFIN-TINI)/(DBLE(NP)-1.0)							!Es pot modificar l'interval de temps amb el parametre TFIN I TINI, pero si no	
		DO K=0, (NP-1)											!s'augmenta el numero de punts (NP) el metode pot perdre precisio.
			T= TINI +(DBLE(K)*PASO) 						
			WRITE(3,2000) T, XINTERPO (T)
		ENDDO
		CLOSE(3)
	END
	
!	Subrutina que calcula el radi de cada pisto	
	SUBROUTINE RADIUS(I,L,RADI)
		IMPLICIT NONE
		REAL*8 I,L,RADI
		RADI = (L/I)- 0.01
	END
	
!	Subrutina que calcula la posicio de cada pisto i els guarda en un vector x	
	SUBROUTINE POSICIONES (L,W,T,X)
		IMPLICIT NONE
		INTEGER I, NPIST
		PARAMETER (NPIST=4)
		REAL*8 X(NPIST), L, W, T, RADI
		DO I=1,NPIST							
			CALL RADIUS(DBLE(I),L,RADI)
			X(I)= RADI*COS(W*T)+SQRT((L**2.0)-((RADI**2.0)*(SIN(W*T)**2.0)))
		ENDDO
	END
	
!	Funcio que calcula el valor de la interpolacio de Xi i Ti per un valor t
	REAL*8 FUNCTION XINTERPO(T)		!INTERPOLACIO LINEAL--> F(X|X1;X2)=F(X1)+ [(F(X2)-F(X1))/(X2-X1)](X-X1) // !INTERPOLACIO 0-->F(X\X1;X2)=F(X1)(X-X1)
		IMPLICIT NONE
		INTEGER I, J, NMAX
		PARAMETER (NMAX=41)
		REAL*8 T, XI(NMAX), TI(NMAX), PEND, H
		COMMON /POSIS/ XI,TI					!|ARA MATEIX XI,TI CONTINDRIA ELS VALORS DEL PISTO 2|
		REAL*8 X1,X2,T1,T2
!		INTEGER TINT
!		TINT=INT(T*10.0D0)
!		H = (T-TI(TINT+1))	
!		PEND =((XI(TINT+2)-XI(TINT+1))/(TI(TINT+2)-TI(TINT+1)))*H
!		XINTERPO= XI(TINT+1)+ PEND
!----------------------------------------------------------------------------------------		
!		ALTERNATIVA: (millor?)	
		DO J=1,NMAX
			X1=XI(J)
			X2=XI(J+1)				
			T1=TI(J)
			T2=TI(J+1)
			IF((T.GT.T1).AND.(T.LT.T2)) THEN		
				H=(T-T1) 
				PEND =((X2-X1)/(T2-T1))*H
				XINTERPO= X1 + PEND
			ELSE IF(T.EQ.T1) THEN
				XINTERPO=X1
			ELSE IF(T.EQ.T2) THEN
				XINTERPO=X2
			ENDIF
		ENDDO
		RETURN
		END