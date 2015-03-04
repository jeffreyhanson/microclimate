      SUBROUTINE SFODE  
      IMPLICIT NONE
C    COPYRIGHT 1997  WARREN P. PORTER,  ALL RIGHTS RESERVED

C          SOLVES SYSTEM OF N SIMULTANEOUS FIRST ORDER O.D.E.   
C          DY(I)/DX=G(I)  G(I)=G(I,X,Y1,Y2,...,YN) X0.LE.X.LE.XMAX  
C          USES ADAMS PREDICTOR-CORRECTOR WITH RUNGA-KUTTA STARTING     
C          DSUB IS USER SUBROUTINE FOR G(I)     
C          OUTPUT SUBROUTINE OSUB IS CALLED AT INTERVALS OF DX  
C          ERR IS TOTAL FRACTIONAL ERROR IN ALL VARIABLES AT END
C          STEP SIZE IS ADJUSTED ACCORDINGLY    
C          IF ERR=0.0  RUN WITH FIXED STEP DX   
C          IN EVENT OF TROUBLE, CONTROL TRANSFERS TO CALLING PROGRAM THROUGH
C          A NORMAL RETURM  
C          AT CALL X AND Y MUST BE THEIR INITIAL VALUES     
C          AT RETURN X AND Y CONTAIN THEIR LAST GOOD VALUES 

      REAL DX,ERH,ERP,ERR,ERT,F,G,H,HO24,S,X,X0,XMAX,XN,Y,YC,YI,YP,temp  
      REAL metout,shadmet,soil,shadsoil,viewf,tnew,dummy,terr,error
      REAL soilmoist,shadmoist,humid,shadhumid,soilpot,shadpot,lastime
      INTEGER cons,I,J,JX,K,KTR,KTT,L,L1,N,NC,NRUNGA,NN,NFACTR,NCOND 
      Integer I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,IPRINT,ep,moist
      INTEGER I91,I92,I93,I94,I95,I96,slipped 
      DIMENSION METOUT(24*7300,18),SHADMET(24*7300,18)
      DIMENSION SOIL(24*7300,12),SHADSOIL(24*7300,12),moist(10)
      DIMENSION SOILMOIST(24*7300,12),SHADMOIST(24*7300,12)
      DIMENSION HUMID(24*7300,12),SHADHUMID(24*7300,12)
      DIMENSION SOILPOT(24*7300,12),SHADPOT(24*7300,12)
      COMMON/ROUTPUT/METOUT,SHADMET,SOIL,SHADSOIL
     & ,SOILMOIST,SHADMOIST,HUMID,SHADHUMID,SOILPOT,SHADPOT      
      COMMON/NONSCR/N,NN,X,XMAX,DX,ERR,H,NCOND,NFACTR,IPRINT
      common/moistcom/moist,ep
c     I/O file designations: I1-I7 (in MAIN); I2 = output file
      COMMON/WMAIN/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I91,I92,I93
     & ,I94,I95,I96        
      COMMON/WORK/ERROR(40),TERR(40),TNEW(40),YI(40),YP(40),YC(40),     
     1 F(6,40),G(40),Y(40) 
     2  ,DUMMY(1160) 
      COMMON/VIEWFACT/VIEWF
      common/prevtime/lastime,slipped,temp(61)
     
c     Defining console value     
      cons = 0  
c     zeroing terms for catching slippage of integrator 
      slipped=0
      do 33 i=1,61
          temp(i)=0.
33    continue      
C          SAVE THE STARTING VALUES 
    2 XN=0  
      X0=X  
      DO 4 I=1,N
      G(I)=0.   
    4 YI(I)=Y(I)
C          INITIALIZE   
      NC=1  
      H=DX  
      KTR=NC
      X=X0  
      DO 50 I=1,N   
   50  F(1,I)=G(I)  
      NRUNGA=2  
      L=1   
      L1=0  
      HO24=H/24.0   
      ERT=0.0   
C        SPECIAL STARTING FOR ERR = 0.0000  
      IF(ERR.NE.0.0000) GOTO 100    
      CALL DSUB(X0,Y,G) 
      CALL OSUB (X,Y)   
      DO 60 J=1,4   
      DO 64 I=1,N   
      F(4,I)=F(3,I)     
      F(3,I)=F(2,I)     
      F(2,I)=F(1,I)     
      F(1,I)=G(I)   
   64 CONTINUE  
      CALL EVALXZ   
      CALL OSUB(X,Y)    
   60 CONTINUE  
      L=4   
      GOTO 200  
C          LOOP POINT   
C          SINCE WE NEED FOUR PREVIOUS VALUES FOR ADAMS PREDICTOR,  
C          SFODE HALVES THE STEP SIZE TWICE.  THE INITIAL HALVING IS    
C          USED FOR AN ERROR TEST.  THE SECOND HALVING IS USED TO   
C          GET THE MINIMUM OF FOUR PREVIOUS DERIVATIVES NEEDED FOR  
C         THE ADAMS PREDICTOR   
C          OBTAIN THE SECOND TWO DERIVATIVES AFTER RUNGA-KUTTA  
C         STARTING USING  THE RUNGA-KUTTA METHOD TWICE  
  100  IF(NRUNGA.NE.1) GOTO 150     
      H=H/2.0   
      XN=X  
      DO 107 I=1,N  
      YP(I)=Y(I)
      F(5,I) = F(3,I)   
      F(3,I) = F(2,I)   
  107 Y(I)=YI(I)
      X=X0  
      CALL EVALXZ   
      DO 109 I=1,N  
  109  F(4,I)=G(I)  
      DO 114 I=1,N  
  114  Y(I)=F(6,I)  
      X=X+H 
      CALL EVALXZ   
      DO 118 I=1,N  
      F(2,I)=G(I)   
  118  Y(I)=YP(I)   
      NC=2*NC   
      L=4   
      KTR=2*KTR 
      NRUNGA=2  
      X=XN  
      HO24=H/24.0   
  150 CALL DSUB(X,Y,G)  
      IF(KTR.LT.NC) GOTO 200
C          CALL THE OUTPUT SUBROUTINE   
      CALL OSUB(X,Y)    
      IF (X.GE.XMAX)  GO TO 700     
      KTR=0 
C          STARTING ROUTINE USING RUNGA-KUTTA   
  200  IF(L.LT.2) GOTO 218  
C          STORE DERIVATIVES INF(1,I) FOR ADAMS,S PREDICTOR 
      DO 210 I=1,N  
  210 F(1,I)=G(I)   
      GOTO 240  
C          RUNGA-KUTTA PREDICTOR FOR STARTING   
  218  DO 220 I=1,N     
  220 F(3,I)=G(I)   
      CALL EVALXZ   
      DO 222 I=1,N  
  222 YP(I) = YC(I)     
      GOTO 258  
C          FOUR POINT PREDICTOR     
  240 DO 242 I=1,N  
  242 YP(I)=Y(I)+HO24*(55.*F(1,I)-59.*F(2,I)+37.*F(3,I)-9.*F(4,I))  
C          SAVE Y(I) IN YC TEMPORARILY  
C          AND MOVE THE PREDICTED VALUES TO Y FOR DSUB CALL 
      DO 252 I=1,N     
      YC(I)=Y(I)
  252  Y(I)=YP(I)   
C          CALCULATE DERIVATIVES AT NEXT POINT BASED ON PREDICTED Y     
      XN=X+H
      CALL DSUB(XN,Y,G) 
      GOTO 270  
C          TO FACILITATE STARTING,STEP SIZE IS CUTI IN HALF 
C          RUNGA-KUTTA CORRECGOR  CUTS STEP SIZE IN HALF. THEN  
C          COMPARES VALUES AFTER TWO TIME STEPS WITH THE VALUES 
C          OBTAINED ABOVE. IF WITHIN TOLERANCE, SFODE CONTINUES WITH    
C          ADAMS MOULTON PREDICTOR CORRECTOR WITH STEP SIZE HALF OF THE 
C          SPECIFIED SIZE,  OTHERWISE STEP SIZE IS CUT IN HALF AND SFOD 
C          STARTS OVER. RETURN Y(I) TO INITIAL VALUES.  
  258 H = H/2.0 
      DO 260 I=1,N  
  260  Y(I)=YI(I)   
      X =X0 
      CALL EVALXZ   
C          RUNGA-KUTTA AT X=H/2     
      DO 262 I = 1,N    
      F(6,I)=YC(I)  
      Y(I)=YC(I)
  262 F(2,I)= G(I)  
      CALL EVALXZ   
      L=4   
      DO 267 I =1,N     
      Y(I) = YC(I)  
  267  F(1,I)=G(I)  
      KTR =2
      NC = 2*NC 
      XN=X  
      NRUNGA=1  
      GO TO 300 
C          FOUR POINT CORRECTOR     
  270 DO 272 I=1,N  
  272 Y(I)=YC(I)+HO24*(9.*G(I)+19.*F(1,I)-5.*F(2,I)+F(3,I)) 
C          ERROR CONTROL TEST   
C      IF ERR LESS THAN ZERO, A RELATIVE ERROR IS MINIMIZED.
C      IF ERR GREATER THAN ZERO, LOCAL ERROR IS MINIMIZED.  
C      IF ERR EQUAL ZERO, NO ERROR TEST IS PERFORMED
  300 ERH=0.
      S=0   
      IF(ERR.EQ.0.0) GO TO 600  
      DO 308 I=1,N  
      IF (ABS(Y(I)).LT.1.0E-01)  GO TO 306  
      IF(ERR.GT.0.0000) GO TO 306   
      ERP=ABS((Y(I)-YP(I))/Y(I))    
      GOTO 307  
  306 ERP=ABS(Y(I)-YP(I))   
  307 IF(ERP.GT.ABS(ERR)) GO TO 310 
      S=S+ERP   
      IF(ERP.GT.ERH) ERH=ERP
  308 CONTINUE  
      GOTO 500  
  310 IF(NC.GT.50) GOTO 485 
C          THE ERROR IS TO BIG - DO SOMETHING ABOUT IT  
C          HALVE THE STEP SIZE  
C          START FROM SCRATCH IF L.LE.4 
C          OBTAIN VALUES FROM F(3,I) FOR FIRST PORTION OF RUNGA-KUTTA   
      IF(L.GT.4)  GOTO 420  
      DO 409 I=1,N  
  409  YP(I)=F(6,I)     
      GOTO 258  
C          REDUCE MESH AND GO ON FROM HERE  
  420  H=0.5*H  
      HO24=H/24.
      NC = 2* NC
      KTR=2*KTR 
      L=L-2 
      IF(L.LE.4) L=5    
C          RESTORE Y TO LAST GOOD VALUES
      DO 422 I=1,N     
  422 Y(I)=YC(I)
C          INTERPOLATE FOR THE INTERMEDIATE DERIVATIVES     
C          FOUR POINT LAGRANGIAN INTERPOLATION SCHEME   
      DO 428 I=1,N  
      F(5,I)=F(3,I)     
      F(3,I)=F(2,I)     
      F(2,I)=(15.*F(1,I)+45.*F(3,I)-15.*F(5,I)+3.*F(4,I))/48.   
      F(6,I)=(15.*F(4,I)+45.*F(5,I)-15.*F(3,I)+3.*F(1,I))/48.   
  428 F(4,I)=(-3.*(F(4,I)+F(1,I))+27.*(F(5,I)+F(3,I)))/48.  
      ERP=0.5*ERP   
      L1=2  
      GO TO 100 
C          RETURN TO * IF MESH SIZE IS TOO SMALL
  485 IF(L.GT.4) CALL OSUB(X,Y)     
c      WRITE(I2,490) X,ERR,H 
      WRITE(cons,490) X,ERR,H        
      DO 486 I=1,N  
  486 Y(I)=YC(I)
C         AUTOMATIC RESTART IF L.GT.4   
      IF(L.LE.4) RETURN 
c      WRITE(I2,487) 
      WRITE(cons,487)       
  487 FORMAT('0***INTEGRATION HAS AUTOMATICALLY BEEN RESTARTED.***'/)   
      X=X+2.0*H 
      GOTO 2
  490 FORMAT('0  ****INTEGRATION CANNOT CONTINUE PAST TIME = ',E11.4    
     1,' WITHIN THITHIN THE ERROR BOUNDS ', E12.4, 
     1/'      WITHOUT REDUCING THE STEPSIZE BELOW ',E12.4,'****')    
C          CHECK FOR POSSIBLE STEP SIZE INCREASE
C          STEP SIZE CAN ONLY BE CHANGED  AT THE PRINT OUT INTERVAL     
  500 IF(ERH.GT.(ERR/100.0))GO TO 600   
      IF (NC.EQ.1)  GO TO 600   
      KTT=KTR+1 
      IF(KTT.NE.NC) GOTO 600
      IF (L.LT.6)  GO TO 600
      IF (L1.LT.3)  GO TO 600   
C          DOUBLE THE STEP SIZE     
      DO 508 I=1,N  
      F(3,I)=F(4,I)     
  508 F(4,I)=F(6,I)     
      L1=0  
      X=XN  
      NC=NC/2   
      ERP=2.*ERP
      H=2.*H
      HO24=2.*HO24  
      KTR=NC
      GO TO 100 
C          NO MESH CHANGE SHIFT DERIVATIVES IN F ARRAY AND GO AHEAD     
C          IF STARTING GO DIRECTLY TO STATEMENT 100 
  600  IF(NRUNGA.EQ.1) GOTO 100     
      JX=L  
      IF (JX.GT.5)  JX=5
      DO 606 I=1,N  
      DO 606 J=1,JX     
      K=JX+1-J  
  606 F(K+1,I)=F(K,I)   
      X=XN  
      L=L+1 
      L1=L1+1   
      KTR=KTR+1 
      ERT=ERT+S 
      GOTO 100  
C          CALCULATION COMPLETION MESSAGE   
 700  CONTINUE  
      S =(L*DX)/(XMAX-X0)   
c      WRITE(7,704)ERT,L,S   
c 704  FORMAT('0   ESTIMATED DISCRETIZATION ERROR AT END =',E14.6,I9,    
c     1' STEPS WERE TAKEN'/'    OR ABOUT',F6.1,' STEPS PER PRINT INTERVAL
c     2')    
      RETURN
      END   
