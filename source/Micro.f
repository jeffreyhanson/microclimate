      SUBROUTINE MICRO(Z,Z0,T1,T3,V,QC,AMOL,NAIR,ZZ,VV,T,ZEN)
      IMPLICIT NONE
C     Copyright Warren P. Porter 2007 
C    This subroutine computes a single unsegmented velocity and temperature profile      
      
      REAL ADUM,AMOL,AMOLN,DEL,DIFFT,DUM,DUMTES,GAM,PHI,PSI1   
      REAL PSI2,QC,RCP,RCPTKG,RHOCP,STB,STO,STS,T,T1,T3,TAVE,TZO,USTAR  
      REAL V,VEL,VV,X,X1,Y,Y1,YY,YY2,Z,Z0,Z01
      REAL Z02,ZEN,ZH1,ZH2,ZRATIO,ZZ

      INTEGER I,ITER,NAIR,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I91,I92
     & ,I93,I94,I95,I96           
      DIMENSION ZZ(1),VV(1),T(1)    
      COMMON/DMYCRO/Z01,Z02,ZH1,ZH2
      COMMON/WMAIN/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I91,I92,I93
     & ,I94,I95,I96 
C   
C**** 3 SEGMENT VELOCITY PROFILE - W. PORTER    
C**** VELOCITY PROFILE - BUSSINGER  
C**** SUBLAYER MODEL - GARRATT AND HICKS
C     Z=REFERENCE HEIGHT
C     Z0=ROUGHNESS HEIGHT   
C     T1=TEMPERATURE AT REFERENCE HEIGHT
C     T3=CURRENT ESTIMATE OF GROUND SURFACE TEMP.   
C     V=VELOCITY AT REF. HEIGHT     
C     QC=COMPUTED (HERE) CONVECTIVE HEAT TRANSFER AT THE SURFACE
C     AMOL=MONIN-OBUKHOV LENGTH     
C     NAIR=NO. OF HEIGHTS FOR AIR TEMP'S AND VELOCITIES     
C     ZZ=ARRAY OF HEIGHT VALUES     
C     VV=ARRAY OF COMPUTED (HERE) VELOCITIES FOR EACH HEIGHT
C     T=ARRAY OF AIR TEMP'S COMPUTED HERE ASSUMING A LOG PROFILE
C   
C     THIS SUBROUTINE IS MODIFIED (FEB. 1979)FOR SHEAR OCCURRING ABOVE  
C     THE SURFACE DUE TO VEGETATION SPACED OVER THE SURFACE.
C     TEMP. PROFILE REMAINS LOGARITHMIC. VEL. PROFILE LOGARITHMIC IN SEGMENTS   
C     SEGMENTS OF VEL. PROFILE= 200-100 CM, 100-30 CM, 30-0 CM. 
C     TREF=200 CM, VREF=30 CM FOR SANTA FE, GALAPAGOS   
C     Z0 IS PLOTTED FROM 30 CM VEL'S DOWN   
C   
C     ****WHEN STARTING AT MIDNIGHT ON THE VERY FIRST   
C     ITERATION BE SURE  BE SURE  BE SURE   
C     INITIAL TSURF GUESS IS LESS THAN TREF     
C     SO MICRO WILL GO TO LOWER HALF
C   
C     DEFINING Z0'S FROM THE TOP DOWN            Steve's
C     GALAPAGOS  TEXAS, WASHINGTON    NEVADA     Carlsbad NM
C     Z01=16.8    11.16               3.67       8.353  
C     Z02= 6.42   10.57               3.29       3.015  
C     Z0 = LOWEST (REAL) ROUGHNESS HEIGHT   
C     Z0 =       0.021                0.90       0.268  
C   
C     DEFINING HEIGHTS WHERE Z0 CHANGES FROM THE TOP
C     Z = 'FREE STREAM' REFERENCE HEIGHT = 200 CM   
C     ZH1=100.   84                   80           50   
C     ZH2=30.    13.                  60.          25   
 
      RHOCP(TAVE) = 0.08472/TAVE
      PHI(Z)=(1.-GAM*Z/AMOL)**.25   
      PSI1(X)=2.*ALOG((1.+X)/2.)+ALOG((1.+X*X)/2.)-2.*ATAN(X)+3.14159/2 
      PSI2(X)=2.*ALOG((1.+X*X)/2.)  
      GAM=16.   
C**** RHO*CP*T/(K*G) = 6.003E-8 IN CAL-MIN-CM-C UNITS   
      RCPTKG=6.003E-8   

C    CHECK FOR PROPER VALUE OF Z0 IN INPUT DATA (RUF)  
      IF (Z0 .LE. 0.00) THEN 
         WRITE(*,*)'Z0 (RUF) LESS THAN OR = TO 0.00',
     &       ', INCREASE THE VALUE OF Z0,',Z0
         GO TO 400  
      ENDIF 
    
C     COMPUTING VEL. PROFILE PARAMETERS FROM 200 CM REFERENCE VELOCITY 
      ZRATIO = Z/Z0 + 1
      DUM=ALOG(ZRATIO)
      dumtes = log(zratio) 
      VEL = V
      USTAR = 0.4*V/DUM 
C   
      DIFFT=T1-T3   
      TAVE=(T3+T1+546.)/2.
      IF(TAVE.LE. 0.000)THEN
        TAVE = 0.01
      ENDIF   
      RCP=RHOCP(TAVE)   
      AMOL=-30.0
      ITER=0
C      CHECK FOR FREE CONVECTION (LAPSE) CONDITIONS 
      IF(T1.GE.T3)GO TO 1000
      IF(T3.LE.80.)GO TO 1000   
      IF(ZEN .GE. 90.)GO TO 1000    
C     NEGLECTING FREE CONV. CORRECTION (4%)FOR SEGMENTED PROFILES.  
C   
C     ITERATING TO FIND THE MONIN-OBUKOV LENGTH (AMOL)  
C   
   1  X=PHI(Z)  
      Y=PSI1(X) 
      YY=PSI2(X)
      USTAR=0.4*V/(ALOG(Z/Z0)-Y) 
C     SUBLAYER STANTON NO., STS=.75RE* **-.45   
c      IF(USTAR-0.0) 100,101,101     
c  100 WRITE(I2,201) USTAR,ZH2,Z0,Y   
c  101 CONTINUE  
      STS=.62/(Z0*USTAR/12.)**.45   
C     BULK STANTON NO.  
      STB=(.64/DUM)*(1.-.1*Z/AMOL) 
      STO=STB/(1.+STB/STS)  
C   
      QC=RCP*DIFFT*USTAR*STO
C   
      AMOLN=RCPTKG*USTAR**3/QC  
      DEL=ABS((AMOLN-AMOL)/AMOL)    
      IF (DEL .LT. 1.0E-02) THEN
       GO TO 2   
      ENDIF
      AMOL=AMOLN
      ITER=ITER+1   
      IF(ITER .GT.30) GO TO 2000    
      GO TO 1   
C     END OF ITERATION LOOP TO FIND MONIN-OBUKOV LENGTH

    2 CONTINUE  
      IF(NAIR.LE.0) RETURN  
      DO 3 I=1,NAIR     
        X1=PHI(ZZ(I))     
        Y1=PSI1(X1)   
        YY2=PSI2(X1)  

C       FILL OUT VELOCITY AND TEMP. PROFILES  
        ADUM=ZZ(I)/Z0-Y1 
        IF(ADUM-0.0) 231,232,232  
  231   continue  
c      WRITE(I2,202) Z01,Z02,Z0,Z,ZH1,ZH2,ZZ(I),Z0,Y1,AMOL   
  232   CONTINUE  
        VV(I)=2.5*USTAR*ALOG(ADUM)   
        IF(VV(I)-0.0)291,292,292  
  291      continue    
c      WRITE(I2,199)VV(I),ZZ(I),Z0,Y1     
C       COMPUTING FICTITIOUS TEMP. AT TOP OF SUBLAYER 
  292   TZO=(T1*STB+T3*STS)/(STB+STS) 
        T(I+20)=TZO+(T1-TZO)*ALOG(ZZ(I)/Z0-YY2)/ALOG(Z/Z0-YY) 
    3 CONTINUE  
      RETURN
C     CALC'S BELOW WHEN NO FREE CONV. ENHANCEMENT OF VEL,TEMP PROFILES  
 1000 CONTINUE  
C     SUBLAYER STANTON NO.  
      STS=.62/(Z0*USTAR/12.)**.45   
C     BULK STANTON NO.  
      STB=.64/DUM  
C   
      QC=RCP*DIFFT*USTAR*STB/(1+STB/STS)
C   
      IF(NAIR.LE.0) RETURN  
      DO 4 I=1,NAIR     
C       FILL OUT VEL. AND TEMP. PROFILES  
        VV(I)=2.5*USTAR*ALOG(ZZ(I)/Z0+1)

        IF(VV(I)-0.0) 391,392,392     
  391      continue    
c      WRITE(I2,199) VV(I),ZZ(I),Z0   
C         COMPUTING FICTITIOUS TEMP. AT TOP OF SUBLAYER 
  392     TZO=(T1*STB+T3*STS)/(STB+STS) 
          T(I+20)=TZO+(T1-TZO)*ALOG(ZZ(I)/Z0+1)/DUM 
    4 CONTINUE  
      RETURN
 2000 continue
c     WRITE(I2,200)  
c  199 FORMAT(1X,'NEGATIVE VELOCITY COMPUTED IN MICRO. Z PROBABLY'   
c     1,' .LT. Z0',1X,5E12.4)
c  200 FORMAT(' TROUBLE IN SUBROUTINE MICRO.  CHECK INPUT DATA, ESPECIALL
c     1Y THE VELOCITIES')
c  201 FORMAT(1X,'NEGATIVE USTAR COMPUTED IN MICRO. Z.LT.Z0? HERE ARE',  
c     11X,'USTAR,ZH2,Z0 AND Y',1X,6E12.4)  
c  202 FORMAT(1X,'NEG. ALOG ARGUMENT IN MICRO.  HERE ARE Z01,Z02,Z0,',   
c     1'Z,ZH1,ZH2,ZZ(I),Z0,Y1,AMOL',/,1X,10E12.4)   
400   RETURN
      END   
