      FUNCTION ZBRENT(FUN,X1,X2,TOL)
C     EPS IS MACHINE FLOATING POINT PRECISION
C     FLOATING POINT PRECISION FOR THE COMPAQ III PORTABLE IS 5.0E-20
      PARAMETER (ITMAX=20,EPS=3.E-8)
      E=0.
      C=0.
      Diagnos = 0.
      A=X1
      B=X2
      FA=FUN(A)
      FB=FUN(B)
      IF((FB*FA.GT.0.) .and. (Diagnos.gt.0.0)) then 
c        Write(0,*)'Root must be bracketed for ZBRENT.' 
       else
      endif  
      FC=FB
      DO 11 ITER=1,ITMAX
        IF(FB*FC.GT.0.) THEN
          C=A
          FC=FA
          D=B-A
          E=D
        ENDIF
        IF(ABS(FC).LT.ABS(FB)) THEN
          A=B
          B=C
          C=A
          FA=FB
          FB=FC
          FC=FA
        ENDIF
        TOL1=2.*EPS*ABS(B)+0.5*TOL
        XM=.5*(C-B)
        IF(ABS(XM).LE.TOL1 .OR. FB.EQ.0.)THEN
          ZBRENT=B
          RETURN
        ENDIF
        IF(ABS(E).GE.TOL1 .AND. ABS(FA).GT.ABS(FB)) THEN
          S=FB/FA
          IF(A.EQ.C) THEN
            P=2.*XM*S
            Q=1.-S
          ELSE
            Q=FA/FC
            R=FB/FC
            P=S*(2.*XM*Q*(Q-R)-(B-A)*(R-1.))
            Q=(Q-1.)*(R-1.)*(S-1.)
          ENDIF
          IF(P.GT.0.) Q=-Q
          P=ABS(P)
          IF(2.*P .LT. MIN(3.*XM*Q-ABS(TOL1*Q),ABS(E*Q))) THEN
            E=D
            D=P/Q
          ELSE
            D=XM
            E=D
          ENDIF
        ELSE
          D=XM
          E=D
        ENDIF
        A=B
        FA=FB
        IF(ABS(D) .GT. TOL1) THEN
          B=B+D
        ELSE
          B=B+SIGN(TOL1,XM)
        ENDIF
        FB=FUN(B)
11    CONTINUE  
      If (Diagnos .gt. 0.0) then
c        Write(0,*) 'ZBRENT exceeding maximum iterations.' 
       else
      endif  
      ZBRENT=B
      RETURN
      END
