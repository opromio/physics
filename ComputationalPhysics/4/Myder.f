!***********************************************************************
      SUBROUTINE DERIFUN(NDAT,FU,X,DFU)
      IMPLICIT NONE
      DOUBLE PRECISION X0, XF, XN
      DOUBLE PRECISION X(NDAT), FU(NDAT), H, DFU(NDAT)
      INTEGER I, NDAT, I1, I2
	  COMMON /PASO/ H
      X0=X(1)
      XF=X(NDAT)
!      H=(XF-X0)/(NDAT-1)
      DO I=1, NDAT
       IF (I.EQ.1) THEN
        DFU(I)=(FU(2)-FU(1))/H
       ELSE IF (I.EQ.(NDAT)) THEN
        DFU(I)=(FU(NDAT)-FU(NDAT-1))/H
       ELSE
        DFU(I)=(FU(I+1)-FU(I-1))/(2.0D0*H)
       ENDIF
      ENDDO											!!PENDIENTE DE COMPILAR
      END
