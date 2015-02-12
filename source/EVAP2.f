      SUBROUTINE EVAP2(TSURF,TAIR,RELHUM,HD,QEVAP) 

C     THIS SUBROUTINE COMPUTES SURFACE WATER LOSS.  
C     COPYRIGHT 2006 W.P. PORTER. ALL RIGHTS RESERVED.
c     warning - this is all in SI units!

      IMPLICIT NONE  

      REAL AIRVD,ALTT,BP,CP,DB,DENAIR,DP,E,EFFSUR,ESAT,GWSURF
      REAL HD,HTOVPR,MAXSHD,PSTD,PTWET
      REAL QEVAP,RH,RELHUM,RW,SABNEW,SHAYD,SURFVD
      REAL TAIR,TSURF,TVIR 
      REAL TVINC,VD,WATER,WB,WTRPOT,rainfall

      CHARACTER*1 SNO

      COMMON/GROUND/SHAYD,ALTT,MAXSHD,SABNEW,PTWET,rainfall
      COMMON/GRND2/SNO

C     CALCULATING SURFACE SATURATION VAPOR DENSITY
      RH = 100.
C     CHECK FOR TOO LOW A SURFACE TEMPERATURE
      IF (TSURF .LT. -60.) THEN
        DB = -60.   
       ELSE
C       CHECK FOR TOO HIGH A SURFACE TEMPERATURE (STABILITY CHECK)
c        IF (TSURF .GT. 10.) THEN
c          DB = 10.
c         ELSE
          DB = TSURF
c        ENDIF
      ENDIF

C     SETTING 3 PARAMETERS FOR WETAIR, SINCE RH IS KNOWN (SEE WETAIR LISTING)  
      WB=0.
      DP=999.  
C     BP CALCULATED FROM ALTITUDE USING THE STANDARD ATMOSPHERE
C     EQUATIONS FROM SUBROUTINE DRYAIR    (TRACY ET AL,1972)
      PSTD=101325.  
      BP = PSTD*((1.-(0.0065*ALTT/288.))**(1./0.190284)) 

      CALL WETAIR(DB,WB,RH,DP,BP,E,ESAT,VD,RW,TVIR,TVINC,  
     * DENAIR,CP,WTRPOT)
      SURFVD = VD 

C     CALCULATING AIR VAPOR DENSITY
      RH = RELHUM
      DB = TAIR
      CALL WETAIR(DB,WB,RH,DP,BP,E,ESAT,VD,RW,TVIR,TVINC,
     * DENAIR,CP,WTRPOT)
      AIRVD = VD   

C     COMPUTE SURFACE WATER LOSS BASED ON EFFECT WET AREA/UNIT SURFACE AREA (M^2)
C    CONVERTING TO THE FRACTION OF A UNIT SURFACE AREA THAT IS WET
      EFFSUR = 100./100.

C    AMOUNT OF WATER EVAPORATED FROM THE SURFACE (KG/S)
      WATER = EFFSUR * HD *(SURFVD - AIRVD)  

C     FROM DRYAIR: LATENT HEAT OF VAPORIZATION 
c      HTOVPR = 2.5012E+06 - 2.3787E+03 * TAIR
      if(TSURF.gt.0)then
      HTOVPR=2500.8-2.36*TSURF+0.0016*TSURF**2-0.00006*TSURF**3 
      else
       HTOVPR=2834.1-0.29*TSURF-0.004*TSURF**2 
      endif
      HTOVPR=HTOVPR*1000
c     convert qevap to cal/min/cm2 for dsub      
c      QEVAP = WATER * HTOVPR / 4.184 * 60. / 10000.  
      QEVAP = WATER * HTOVPR    
C    KG/S TO G/S 
      GWSURF  = WATER * 1000. 

      RETURN
      END