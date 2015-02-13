      SUBROUTINE OSUB(TIME,Y)
      
C    COPYRIGHT 2000  WARREN P. PORTER,  ALL RIGHTS RESERVED         

C     THIS SUBROUTINE IS CALLED FROM SFODE, THE NUMERICAL INTEGRATOR.  Y MAY BE EITHER 
C     THE VALUE BEING ESTIMATED OR ITS DERIVATIVE.  HERE IT IS NOT NEEDED AND SO SERVES
C     ONLY AS A DUMMY VARIABLE FOR WHAT IS REALLY COMPUTED IN DSUB AND TRANSFERRED HERE
C     IN COMMON STATEMENTS.

C     VERSION 2 SEPT. 2000
      IMPLICIT NONE
      EXTERNAL TAB 

      REAL ALIZ,ALPLIZ,ALTT,ARLIZ
      REAL C,CP,DAY,DENAIR,DEP,DEPP,DP,DTAU,DUMMY,E,EPSLIZ,ERR1,ESAT
      REAL FIN,H,HLIZ
      REAL JULDAY,LASTIME,MAXSHD,OUT
      REAL OUT2,PCTWET,PTWET,SAB,SABNEW,SHAYD,SIOUT,REFLS
      REAL RH,RHLOCL,RW,T,TANNUL
      REAL TAB,TD,TI,TIME,TIMEF,TMAX,TMAXX,TMIN,TMINN,TVINC,TVIR
      REAL VD,WC,Y,SOK,END,DAS 
      REAL MON,WLIZ,X,ZENR
      REAL TSKY,ARAD,CRAD,CLOUD,CLR,SOLR,QRADVG,QRADGR
      REAL RCSP,HGTP,RUFP,BEGP,PRTP,ERRP,snowout,curmoist,soiltemp,
     & SLEP,NONP,SUN,PLT,rainfall,snowtemp,snowdens,snowmelt,curhumid
      REAL CLEAR,QRADSK,SIGP,TAIR,SRAD,QEVAP,frosttest,vel2m,surflux
      real rainsnow,snowfall,htovpr,water,gwsurf,netsnow,air,EP,zz,vv

      REAL metout,shadmet,soil,shadsoil,rain,snowhr,FROST,soilmoist,
     &shadmoist,humid,shadhumid,curpot,soilpot,shadpot
      real bp,hrad,patmos,pstd,qrad,qradhl,viewf,wb,wtrpot,temp,SLE
      real DENDAY,SPDAY,TKDAY,DENDAY2,SPDAY2,TKDAY2,time2,time3,SLES,err
      real condep,rainmult,ptwet1,soilprop,moist,moists
      real Z01,Z02,ZH1,ZH2,qconv,ttest,hc,hd,VELR,AMOL,wcc
      
      real PE,KS,BB,BD,maxpool
      
      INTEGER CONS,I,IEND,IFINAL,ILOCT,IOUT,IPRINT,ITEST
      INTEGER J,JULNUM,MM,MOY,N,NAIR,ND,NOUT,dew,writecsv
      INTEGER I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,slipped
      INTEGER I91,I92,I93,I94,I95,I96,runmoist,evenrain
      
      INTEGER methour,microdaily

      CHARACTER*3 SYMBOL,INAME,STP  
      CHARACTER*6 NAME, HEAD
      CHARACTER*1 SNO,SNOW

C      IOUT IS THE NUMBER OF THE OUTPUT TERM DESIRED AS NUMBERED IN NAME ARRAY     
C      FILES 6,7,10 & 12 ARE CONSOLE, OUTPUT, METOUT & SOIL RESPECTIVELY
C      FILES 6,I2,I3 & I10 ARE CONSOLE, OUTPUT, METOUT & SOIL RESPECTIVELY

      DIMENSION DAY(7300),DEPP(30),OUT2(55),NAME(55),curmoist(10)
      DIMENSION TMAXX(7300),TMINN(7300),HEAD(55),JULDAY(7300)
      DIMENSION SNOW(7300),REFLS(7300),PCTWET(7300),rain(7300)

      DIMENSION METOUT(24*7300,18),SHADMET(24*7300,18),curhumid(10)
      DIMENSION SOIL(24*7300,12),SHADSOIL(24*7300,12),soiltemp(10)
      DIMENSION SOILMOIST(24*7300,12),SHADMOIST(24*7300,12)
      DIMENSION HUMID(24*7300,12),SHADHUMID(24*7300,12)
      DIMENSION SNOWHR(25*7300),moists(10,7300),moist(10),soilprop(10,6)
      DIMENSION DENDAY(10),SPDAY(10),TKDAY(10),DENDAY2(10),
     &    SPDAY2(10),TKDAY2(10),temp(61),SLES(7300)
      DIMENSION soilpot(24*7300,12),shadpot(24*7300,12),curpot(10)
      
      COMMON/TABLE/ILOCT(21),TI(200),TD(200)    
      COMMON/ARRAY/T(30),WC(20),C(20),DEP(30),IOUT(100),        
     1 OUT(101),ITEST(23)
      COMMON/PAR/SIGP,RCSP,SOK,SAB,HGTP,RUFP,BEGP,MON,PRTP,ERRP,END,   
     1 SLEP,DAS,NONP,SUN,PLT,FIN,STP   
      COMMON/CARRAY/INAME(20),SYMBOL(23)
      COMMON/NONSCR/MM,N,X,TIMEF,DTAU,ERR1,H,NOUT,NAIR,IPRINT    
      COMMON/PLIZ/ALIZ,WLIZ,ARLIZ,HLIZ,ALPLIZ,EPSLIZ,TMAX,TMIN                 
C    SI UNITS FROM DSUB FOR OUTPUT TO DATA FILE FOR ANIMAL ENERGY  
C    BALANCES  
      COMMON/SIUNIT/SIOUT(10)   
      COMMON/NDAY/ND 
      COMMON/WDSUB/TSKY,ARAD,CRAD,CLOUD,CLR,SOLR
      COMMON/WOSUB/DEPP 
      COMMON/WMAIN/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I91,I92,I93
     & ,I94,I95,I96 
      COMMON/DAYS/DAY,TMINN,TMAXX,TANNUL
      COMMON/DAYJUL/JULDAY,JULNUM,MOY
      COMMON/WINTER/SNOW
      COMMON/WINTER2/REFLS,PCTWET
      COMMON/SNOWPRED/SNOWHR,snowtemp,snowdens,snowmelt

C     PERCENT GROUND SHADE & ELEVATION (M) TO METOUT      
      COMMON/GROUND/SHAYD,ALTT,MAXSHD,SABNEW,PTWET,rainfall
      COMMON/GRND2/SNO  
      COMMON/LOCLHUM/RHLOCL  
      COMMON/ROUTPUT/METOUT,SHADMET,SOIL,SHADSOIL
     & ,SOILMOIST,SHADMOIST,HUMID,SHADHUMID,SOILPOT,SHADPOT
      COMMON/VIEWFACT/VIEWF
      COMMON/RAINY/RAIN      
      COMMON/SOYFILS/DENDAY,SPDAY,TKDAY
      common/prevtime/lastime,slipped,temp
      common/soilmoist/condep,rainmult,runmoist,maxpool,evenrain
      COMMON/DAILY/microdaily
      COMMON/NICHEMAPRIO/SLE,ERR,SLES,soilprop,moists,surflux
      common/moistcom/moist,ep
      COMMON/DMYCRO/Z01,Z02,ZH1,ZH2 
      COMMON/AIRRAY/ZZ(10),VV(10)
      common/campbell/PE,KS,BB,BD
      
      DATA NAME/'TIME ','TAIR','TSKY','TSURF','VEL','SOL  ','TLIZ',     
     1 'QSOLAR','QRAD','QCOND','QCONV','MOL ','STEP','T2','T3','T4',    
     1'T5','T6',
     2 'T7','T8','T9','T10','T11','T12','T13','T14','T15','T16','T17',  
     3 'T18','T19','T20','TA1','TA2','TA3','TA4','TA5','TA6','TA7',     
     4'TA8','TA9','TA10','VA1','VA2','VA3','VA4','VA5','VAL','VA7',     
     5'VA8','VA9','VA10','T/60  ','M-E','T*60'/ 
      DATA IFINAL/0/ 
C    SETTING MONTH OF YEAR COUNTER. OSUB CALLED ONCE PER END OF DAY
C    (CURRENTLY THE 15TH OF EVERY MONTH).     
      writecsv=1
C     KLUGE TO ELIMINATE COMPILER PROTEST ABOUT THE NON-USE OF Y
      DUMMY = Y  
      
C     SETTING CONSOLE OUTPUT VARIABLE
      CONS = 0 
      LASTIME=0.

C OUTPUT                                                           VARIABLE 
C VARIABLE     VARIABLE                                       VARIABLE N
C NUMBER    
C 1        TIME                                                      TIME   
C 2        TAIR(INPUT AT REF. HT.)                                   TAR
C 3        TSKY                                                      TSKY   
C 4        TSURFACE                                                  TSURF  
C 5        V AIR (INPUT AT REF. HT.)                                 VEL
C 6        SOLAR FLUX                                                SOL
C 7        PERCENT CLOUD COVER                                       CLOUD  
C 8        ABSORBED SOLAR HEAT FLUX (SOIL)                           QSOLAR 
C 9        RADIATION BETWEEN SURFACE AND SKY                         QRAD   
C 10       CONDUCTION FROM SURFACE TO NEXT NODE                      QCOND  
C 11       CONVECTION FROM SURFACE TO AIR                            QCONV  
C 12       MONIN-OBUKOV LENGTH                                       MOL
C 13       INTEGRATION STEP SIZE                                     STEP   
C 14 TO 32 SOIL TEMP'S AT DEP(I): FROM 1ST BELOW SURF DOWN           TI 
C 33 TO 42 AIR TEMP'S AT DEP(I): FROM 1ST ABOVE SURF UPWARD          TAI
C 43 TO 52 AIR VEL. AT DEP(I): FROM 1ST ABOVE SURF UPWARD            VAI
C 53       TIME/60.                                                  T/60   
C 54       DEW POINT TEMP.                                           TDP
C 55       TIME*60.                                                  T*60   
C   
C     DAY COUNTER TO DELETE OUTPUT FOR REPLICATE DAYS   
C     NEEDED TO ESTABLISH SOIL STEADY PERIODICS 
      if(moy.eq.2)then
          moy=2
      endif
      time2=0.
      time3=0.
      do 876 i=1,25
      if(time.ge.ti(i+11))then
      time2=ti(i+11)
      time3=time2-60.
      endif
876   continue
      if(time.ne.time2)then
          time=time2
      endif
      
      IF(TIME .GT. 0.0) GO TO 5     
        IFINAL=IFINAL+1 
        IF(IFINAL .EQ. 1)THEN 
C        SETTING SNOW VARIABLE
          SNO = SNOW(MOY) 
C        SETTING THIS MONTH'S SURFACE REFLECTIVITY FROM THE ABSORPTIVITIES
          SABNEW = 1.0 - REFLS(MOY)
C        SETTING THIS MONTH'S PERCENT OF SURFACE WITH FREE WATER/SNOW ON IT
          PTWET1 = PCTWET(MOY)
          rainfall=RAIN(MOY)
          CONTINUE
        ENDIF 
    5 CONTINUE 
          PTWET1 = PCTWET(MOY)
          rainfall=RAIN(MOY)

      if(microdaily.eq.1)then
          if(moy.gt.1)then
              ND=1
          endif
      endif
      

c      write(*,*) PTWET
C     NO OUTPUT IF REPEATED DAY,IFINAL, NOT THE FINAL ITERATION
      IF (IFINAL .LT. ND) GO TO 200 

C     OUTPUT FOR START OF A DAY     
      IF ((TIME-BEGP).GT.0.) GO TO 20
C*** SET UP OUTPUT FOR C, WC
      IF (IPRINT.NE.1) GO TO 19     
C      WRITE(CONS,111) MM   
c      WRITE(I2,111) MM   
c  111 FORMAT('0SOIL CONDUCTANCES BETWEEN NODES I AND I+1 FOR I=1,',I4)  
C      WRITE(CONS,110) (C(I),I=1,MM)    
c      WRITE(I2,110) (C(I),I=1,MM)    
c  110 FORMAT(6(1X,F11.4))   
C      WRITE(CONS,120) MM   
c      WRITE(I2,120) MM   
c  120 FORMAT(/' SOIL MASS*SPECIFIC HEAT FOR NODE I, I=1,',I4)   
C      WRITE(CONS,110) (WC(I),I=1,MM)   
c      WRITE(I2,110) (WC(I),I=1,MM)   
 19   CONTINUE  
C      WRITE(CONS,11)   
c      WRITE(I2,11)   
c   11 FORMAT(1H1)   
      DO 40 I=1,NOUT    
      J=IABS(IOUT(I))   
   40 HEAD(I)=NAME(J)   
C      WRITE(CONS,130) (HEAD(I),I=1,NOUT)   
c      WRITE(I2,130) (HEAD(I),I=1,NOUT)   
c  130 FORMAT(6(6X,A6)) 
  
C     FINDING THE # OF AIR NODES
C      NAIR =0   
C   72 NAIR=NAIR+1   
C      IF(DEP(NAIR)) 72,73,73   
C 73   NAIR=NAIR-1    

C     PRINT INTERVAL OUTPUT 

   20 CONTINUE  
      DO 50 I=1,NOUT    
      J=IABS(IOUT(I))   
   50 OUT2(I)=OUT(J)    
C      IF(NOUT .GT.6) WRITE(CONS,138)   
c      IF(NOUT .GT.6) WRITE(I2,138)   
c 138  FORMAT(1H )   
C      WRITE(CONS,140) (OUT2(I),I=1,NOUT)   
c      WRITE(I2,140) (OUT2(I),I=1,NOUT)   
c  140 FORMAT(6(1X,F11.5))   
      IF (TIME .LT. 1440.) GO TO 150
      IFINAL=0  

  150 CONTINUE
C     PUTTING SOIL DEPTH VALUES AT NODES INTO ARRAY DEPP
      DO 74 I=1,N   
       J=I+NAIR  
       DEPP(I)=DEP(J)   
74    CONTINUE    
C    WRITE TO OUTPUT FILE,METOUT, FOR SI UNITS ANIMAL ENERGY BALANCES  
C     WRITE COLUMNS LABEL
      IF (TIME .EQ. 0.00) THEN
C      PUT LABEL 'TIME TA(LOC)TA(2M) RH V(LOC) TS   T2   TDEEP ZEN SOLR TSKY
C       (C) ELEV(M) ABSOIL %SHAD TANNUAL'
        IF(SHAYD.LT.MAXSHD)THEN
c        WRITE(I3,158)
         ELSE
c        WRITE(I12,158)
        ENDIF
C       OUTPUT TO FILE 'SOIL'
C       LABEL DATA WITH DEPTHS FROM 200 M REF HEIGHT IN AIR TO DEEPEST SOIL NODE
        IF(SHAYD.LT.MAXSHD)THEN
c        WRITE(I10,156)(DEPP(IS),IS=1,N)
         ELSE
c        WRITE(I11,156)(DEPP(IS),IS=1,N)
        ENDIF
C       WRITE(CONS,156)(DEPP(IS),IS=1,N)
       ELSE
      ENDIF

C     WRITE TO FILE METOUT (FILE I3) & SOIL (FILE I10) & 100% SHADED SOIL(SHADSOIL) HOURLY DATA.  
C     IF TIME (SIOUT(1) = 0,INCLUDE ALTITUDE (M) & SUBSTRATE ABSORPTIVITY IN METOUT
C     THIS SIOUT ARRAY DEFINED AT THE BOTTOM OF SUB. DSUB.
      IEND = 14 + N - 2

C    SETTING UP THE OUTPUT
      TAIR=TAB('TAR',TIME)  
      ZENR=TAB('ZEN',TIME)  
      SOLR=TAB('SOL',TIME)
      CLOUD=TAB('CLD',TIME)
      T(N)=TAB('TDS',TIME)
      VEL2M=TAB('VEL',TIME)/(100.*60.)

C    Modification by M. Kearney for effect of cloud cover on direct solar radiation, using the
C    Angstrom formula (formula 5.33 on P. 177 of "Climate Data and Resources" by Edward Linacre 1992
      IF (CLOUD .GT. 0.) THEN
         SOLR = SOLR*(0.36+0.64*(1.-(CLOUD/100)))
      ENDIF 

C     CLEAR SKY RADIANT TEMPERATURE   
c      ARAD=(TSKY+273.)**4 replacted with formula from Campbell, converted to cal/min
c      ARAD=(0.0000092*(TAIR+273.16)**2)*0.0000000567*(TAIR+273.16)**4*60
c     &./(4.185*10000.)
      CLR=1.- (CLOUD/100.)
        RH = TAB('REL',TIME)
        WB = 0.
        DP = 999.
C       BP CALCULATED FROM ALTITUDE USING THE STANDARD ATMOSPHERE
C       EQUATIONS FROM SUBROUTINE DRYAIR    (TRACY ET AL,1972)
        PSTD=101325.  
        PATMOS=PSTD*((1.-(0.0065*ALTT/288.))**(1./0.190284)) 
        BP = PATMOS
        CALL WETAIR (TAIR,WB,RH,DP,BP,E,ESAT,VD,RW,TVIR,TVINC,DENAIR,
     &      CP,WTRPOT)
      ARAD=1.72*((E/1000.)/(TAIR+273.16))**(1./7.)*0.0000000567*
     &(TAIR+273.16)**4*60./(4.185*10000.)
c     Below is the Gates formula (7.1) - currently using Campbell instead
c     ARAD=(1.22*0.00000005673*(TAIR+273.)**4-171)
c     &.)
C     APPROXIMATING CLOUD RADIANT TEMPERATURE AS 2 M SHADE TEMPERATURE  
      CRAD=SIGP*SLEP*(TAIR+273.)**4     
c      CRAD=(TAIR+271.)**4  MK commented this out and put in above formula
c    Hillshade radiant temperature (approximating as air temperature)
      HRAD=SIGP*SLEP*(TAIR+273.)**4   
C     GROUND SURFACE RADIATION TEMPERATURE       
      SRAD=SIGP*SLEP*(T(1)+273.)**4 
C     TOTAL SKY IR AVAILABLE/UNIT AREA 
      CLEAR =  ARAD*CLR
      CLOUD =  CRAD*(CLOUD/100.)
c       previously SIGP*SLEP*(CLEAR + CLOUD)*((100.- SHAYD)/100.) but changed by MK to
c       allow the formula in Gates to be used
      QRADSK=(CLEAR + CLOUD)*((100.- SHAYD)/100.)
      
C     VEGETATION IR AVAILABLE/UNIT AREA
c     previously QRADVG=SIGP*SLEP*(SHAYD/100.)*CRAD but changed by MK to allow formula
c       in Campbell to be used
      QRADVG=(SHAYD/100.)*CRAD
C     GROUND SURFACE IR UPWARD/UNIT AREA       
c      QRADGR=SIGP*SLEP*SRAD MK commented this out and replaced with below    
      QRADGR=((100.-SHAYD)/100.)*SRAD+(SHAYD/100.)*CRAD 
c    TOTAL HILLSHADE RADIATION
      QRADHL=HRAD*(1-VIEWF)            
C     NET IR RADIATION: INCOMING FROM SKY + VEGETATION + HILLSHADE - OUTGOING FROM GROUND 
      QRAD = (QRADSK + QRADVG)*VIEWF + QRADHL*(1-VIEWF) - QRADGR
c      TSKY=((QRAD+QRADGR)/(SIGP))**(1./4.)-273
      TSKY=(((QRADSK + QRADVG)*VIEWF + QRADHL*(1-VIEWF))/(SIGP))**(1./4.
     & )-273
c    TSKY=((QRADSK + QRADVG)/(SIGP))**(1./4.)-273
      SIOUT(1) = TIME   
C    AIR TEMPERATURE AT ANIMAL HEIGHT (1ST NODE BELOW REFERENCE HEIGHT, 200 CM) 
      SIOUT(2) = OUT(34)   
C    RELATIVE HUMIDITY AT ANIMAL HEIGHT
      SIOUT(3) = TAB('REL',TIME) 
C    VELOCITY AT ANIMAL HEIGHT (OUT(43 - 52) ARE NODES DOWN FROM REFERENCE HEIGHT AIR NODE 
C     THE 3 DEFAULT NODE HEIGHTS DEFINED IN SUB. IOMET2. OUT(43) IS REFERENCE HEIGHT.
      SIOUT(4) = OUT(44)/(100.*60.)
C    SUBSTRATE TEMPERATURE 
      SIOUT(5) = T(1)
C     1ST SOIL NODE BELOW THE SURFACE (2.5 CM CURRENTLY)  
      SIOUT(6) = T(2)
C     DEEP SOIL NODE  
      SIOUT(7) = T(N)
C    SOLAR & ZENITH OUTPUT TO METOUT FOR A HORIZONTAL SURFACE 
C    FOR ANIMAL CALCULATIONS, ALTHOUGH SLOPE CORRECTIONS DONE
C    IN DSUB FOR THE GROUND
      SIOUT(8) = ZENR
C    CONVERTING FROM CAL/CM2-MIN TO W/M2
      SIOUT(9) = SOLR * 4.185 * 10000. / 60. 
C     BLACK BODY EQUIVALENT SKY INFRARED RADIATION (W/M2)  
c      SIOUT(10) = ((QRADSK+QRADVG)/SIGP)**0.25 - 273.15
      SIOUT(10) = TSKY
C    FROST

c     convert to W/m2      
      QEVAP = OUT(101)*4.185*10000./60.

c      if(SIOUT(1).le.1380)then
c    check if snow fell

      if(OUT(2).le.-15)then
       rainsnow=0.05
      else
       if(out(2).lt.0)then
        air=abs(out(2))**(1.5)
        air=air*(-1.)
       else
        air=out(2)**(1.5)
       endif
       rainsnow=0.05+0.0017*air
      endif
      rainsnow=rainsnow*snowdens
      if((OUT(2).le.snowtemp).and.(rainfall.ge.0.1))then
c     compute snow fall using conversion from daily rain to daily snow (disaggregated over 24 hours) and convert from mm rain to cm snow
      if(time.eq.0)then
c     snowfall=rainfall/24*0.1/rainsnow
      snowfall=rainfall*0.1/rainsnow
c     snowfall=rainfall/24./0.5
c    snowfall=OUT(2)
      else
       snowfall=0
      endif
      else
       snowfall=0
      endif
      
      
      if(out(4).gt.0)then
      HTOVPR=2500.8-2.36*out(4)+0.0016*out(4)**2-0.00006*out(4)**3 
      else
       HTOVPR=2834.1-0.29*out(4)-0.004*out(4)**2 
      endif
      HTOVPR=HTOVPR*1000
      WATER = QEVAP/HTOVPR   
    
C    KG/S TO G/H 
      GWSURF  = WATER * 1000. * 3600.
      
      methour=0
      methour=(int(SIOUT(1)/60)+1)+24*(moy-1)   
      if(runmoist.eq.1)then
      if((moy.eq.1).and.(methour.eq.1))then
       curmoist=moists(1:10,1)
      else
       curmoist=soilmoist(methour-1,3:12)
      endif   
c     choosing between even rainfall through the day or one event at midnight
      if(evenrain.eq.1)then
       if(time.ne.0)then
          rainfall=0
       endif
       condep=condep+rainfall*rainmult
       else
        condep=condep+rainfall/24.*rainmult
       endif
      if(condep.lt.0.)then
          condep=0.
      endif
        soiltemp(1)=OUT(4)
        soiltemp(2:10)=OUT(14:22)

c     now compute potential evaporation, EP        
      VELR=TAB('VEL',TIME)
C    COMPUTE VELOCITY AND TEMPERATURE PROFILES
      IF((ZH1.LE.0.000).AND.(ZH2.LE.0.000))THEN
C      NO SEGMENTED VELOCITY PROFILE (SINGLE LOG PROFILE) 
        CALL MICRO(HGTP,RUFP,TAIR,soiltemp(1),VELR,QCONV,AMOL,NAIR,ZZ,VV  
     &,T,ZENR)
       ELSE
C      SEGMENTED VELOCITY PROFILE (VEGETATION OR OTHER OBJECTS MODIFYING VELOCITY PROFILE)
        CALL MICROSEGMT(HGTP,RUFP,TAIR,soiltemp(1),VELR,QCONV,AMOL,NAIR 
     &,ZZ, VV,T,ZENR)
      ENDIF
C      GETTING THE RELATIVE HUMIDITY FOR THIS POINT IN TIME
        RH = TAB('REL',TIME)
        WB = 0.
        DP = 999.
C       BP CALCULATED FROM ALTITUDE USING THE STANDARD ATMOSPHERE
C       EQUATIONS FROM SUBROUTINE DRYAIR    (TRACY ET AL,1972)
        PSTD=101325.  
        PATMOS=PSTD*((1.-(0.0065*ALTT/288.))**(1./0.190284)) 
        BP = PATMOS
        CALL WETAIR (TAIR,WB,RH,DP,BP,E,ESAT,VD,RW,TVIR,TVINC,DENAIR,
     &      CP,WTRPOT)
C      COMPUTING THE HEAT & MASS TRANSFER COEFFICIENTS, hc & hd
        IF(soiltemp(1).LT.-60.)THEN
          soiltemp(1) = -60.
        ENDIF
C      CHECKING FOR DIVIDE BY ZERO
        IF(soiltemp(1).EQ.TAIR)THEN
          soiltemp(1)= soiltemp(1)+0.1
        ENDIF
C      CHECK FOR OUTSIZE T(1)
        TTEST = ABS(soiltemp(1))
        IF(TTEST.GT. 100)THEN
          T(1) = TAIR + 1.0
          HC = ABS((QCONV*4.184/60.*10000)/(soiltemp(1)-TAIR))
         ELSE
          HC = 0.01
          HC = ABS((QCONV*4.184/60.*10000)/(soiltemp(1)-TAIR))
        ENDIF
        HD = (HC/(CP*DENAIR))*(0.71/0.60)**0.666
      CALL EVAP2(soiltemp(1),TAIR,RH,HD,QEVAP)
      if(soiltemp(1).gt.0)then
      HTOVPR=2500.8-2.36*soiltemp(1)+0.0016*soiltemp(1)**2-0.00006
     &*soiltemp(1)**3  
      else
       HTOVPR=2834.1-0.29*soiltemp(1)-0.004*soiltemp(1)**2 
      endif
      HTOVPR=HTOVPR*1000
c     evaporation potential, mm/s (kg/s)      
      EP = QEVAP/HTOVPR
      if(EP.le.0)then
      EP=0.0001
      endif

      curmoist(1)=condep/(depp(2)*10)*(1-BD/2.6)
      if(curmoist(1).ge.(1-BD/2.6))then
          curmoist(1)=1-BD/2.6
      endif

      CALL RELHUMLOCAL
      if(RHLOCL.ge.99)then
          RHLOCL=99
      endif
      if(rhlocl.lt.0)then
          rhlocl=0.
      endif
      if((moy.eq.74).and.(time.eq.300))then
          moy=74
      endif

      call infil(rhlocl/100.,curmoist,EP,soiltemp,depp,surflux
     &,wcc,curhumid,curpot) 

      condep=condep+WCC-surflux

      if(condep.lt.0)then
          condep=0.
      endif

      if(condep.gt.maxpool)then
          condep=maxpool
      endif

      moists(1:10,moy)=curmoist
      moist(1:10)=curmoist

      ptwet=surflux/(ep*3600*100)
c     fixing %wet at 100% if water pooling on surface     
      if(condep.gt.depp(2)*10*(1-BD/2.6))then
          ptwet=100
      endif
      if(ptwet.lt.0)then
          ptwet=0
      endif
      if(ptwet.gt.100)then
          ptwet=100
      endif
c     end check for soil moisture model running      
      endif
      if(gwsurf.lt.0)then
      gwsurf=0
      endif

      netsnow=snowfall-gwsurf/snowmelt/(rainsnow*1.)
c     no snow output      
      netsnow=0
      
      if(netsnow.gt.0)then
      REFLS(MOY)=0.9
      PCTWET(MOY)=100.
       if(time.eq.1440.)then
        REFLS(MOY+1)=0.9
        PCTWET(MOY+1)=100.
       endif
      endif
          
      methour=0
      methour=(int(SIOUT(1)/60)+1)+24*(moy-1)        
      if((moy.eq.1).and.(methour.eq.1))then
       snowhr(methour)=netsnow
      else
       snowhr(methour)=snowhr(methour-1)+netsnow
c    snowhr(methour)=netsnow
       if(snowhr(methour).lt.0)then
        snowhr(methour)=0
       endif
      endif
c    endif

      frosttest=(out(34)+out(4))/2.
c    if((QEVAP.lt.0).and.(out(4).lt.0))then
      if((QEVAP.lt.-0.0000025).and.(out(34).lt.0))then
c    if((QEVAP.lt.-0.0000025).and.(out(4).lt.0))then
c    if((out(34).lt.0))then
c    if((QEVAP.lt.0).and.(frosttest.lt.0))then
       FROST = 1
      else
       FROST = 0
      endif

      if(QEVAP.lt.0)then
       DEW = 1
      else
       DEW = 0
      endif
c    frost=qevap

C    END OF OUTPUT SETUP

      SNOWOUT=snowhr(methour)
c    FROST=netsnow
c    FROST=1.101
c    SET UP LOCAL RELATIVE HUMIDITY
      CALL RELHUMLOCAL
c    RHLOCL = 10.

      TKDAY2=(TKDAY/60.)*418.5
      SPDAY2=SPDAY*4185
      DENDAY2=DENDAY*1.0E+3

      if(slipped.eq.1)then
      if(time.le.1380)then
      IF (SIOUT(1) .EQ. 0.0) THEN 
C      INCLUDE ALTITUDE ON EVERY LINE WITH TIME 0 FOR ANIMAL CALCULATIONS      
       IF(SHAYD.LT.MAXSHD)THEN
C          WRITE TO METOUT
C        TIME TA(LOC) TA(2M) RH(LOC) RH   V(LOC) TS   T2   TDEEP  ZEN   SOLR  TSKY(C) ELEV(M) ABSOIL %SHAD %WET TANNUL
c        WRITE(I3,154)SIOUT(1),SIOUT(2),OUT(2),RHLOCL,SIOUT(3),
c     *    SIOUT(4),SIOUT(5),SIOUT(6),SIOUT(7),SIOUT(8),SIOUT(9),
c     &    SIOUT(10),ALTT,SABNEW,SHAYD,PTWET,TANNUL
        methour=int(temp(31)-1)
        metout(methour,1)=temp(1)
        metout(methour,2)=temp(2)
        metout(methour,3)=temp(3)
        metout(methour,4)=temp(4)
        metout(methour,5)=temp(5)
        metout(methour,6)=temp(6)
        metout(methour,7)=temp(7)
        metout(methour,8)=temp(8)
        metout(methour,9)=temp(9)
        metout(methour,10)=temp(10)
        metout(methour,11)=temp(11)
        metout(methour,12)=temp(12)
        metout(methour,13)=temp(13)
        metout(methour,14)=temp(14)
        metout(methour,15)=temp(15)
        metout(methour,16)=temp(16)
        metout(methour,17)=temp(17)
c      metout(methour,17)=melted
        metout(methour,18)=temp(18)
       ELSE
        methour=int(temp(31)-1)
        shadmet(methour,1)=temp(1)
        shadmet(methour,2)=temp(2)
        shadmet(methour,3)=temp(3)
        shadmet(methour,4)=temp(4)
        shadmet(methour,5)=temp(5)
        shadmet(methour,6)=temp(6)
        shadmet(methour,7)=temp(7)
        shadmet(methour,8)=temp(8)
        shadmet(methour,9)=temp(9)
        shadmet(methour,10)=temp(10)
        shadmet(methour,11)=temp(11)
        shadmet(methour,12)=temp(12)
        shadmet(methour,13)=temp(13)
        shadmet(methour,14)=temp(14)
        shadmet(methour,15)=temp(15)
        shadmet(methour,16)=temp(16)
        shadmet(methour,17)=temp(17)
        shadmet(methour,18)=temp(18)
       ENDIF
C       WRITE TO FILE SOIL A LINE OF TIME, SOIL TEMPERATURES FROM THE SURFACE DOWN
       IF(SHAYD.LT.MAXSHD)THEN
        methour=int(temp(31)-1)
        soil(methour,1)=temp(19)
        soil(methour,2)=temp(20)
        soil(methour,3)=temp(21)
        soil(methour,4)=temp(22)
        soil(methour,5)=temp(23)
        soil(methour,6)=temp(24)
        soil(methour,7)=temp(25)
        soil(methour,8)=temp(26)
        soil(methour,9)=temp(27)
        soil(methour,10)=temp(28)
        soil(methour,11)=temp(29)
        soil(methour,12)=temp(30)
      if(runmoist.eq.1)then
        soilmoist(methour,1)=temp(19)
        soilmoist(methour,2)=temp(20)
        soilmoist(methour,3)=temp(32)
        soilmoist(methour,4)=temp(33)
        soilmoist(methour,5)=temp(34)
        soilmoist(methour,6)=temp(35)
        soilmoist(methour,7)=temp(36)
        soilmoist(methour,8)=temp(37)
        soilmoist(methour,9)=temp(38)
        soilmoist(methour,10)=temp(39)
        soilmoist(methour,11)=temp(40)
        soilmoist(methour,12)=temp(41)
        
        humid(methour,1)=temp(19)
        humid(methour,2)=temp(20)
        humid(methour,3)=temp(42)
        humid(methour,4)=temp(43)
        humid(methour,5)=temp(44)
        humid(methour,6)=temp(45)
        humid(methour,7)=temp(46)
        humid(methour,8)=temp(47)
        humid(methour,9)=temp(48)
        humid(methour,10)=temp(49)
        humid(methour,11)=temp(50)
        humid(methour,12)=temp(51)
        
        soilpot(methour,1)=temp(19)
        soilpot(methour,2)=temp(20)
        soilpot(methour,3)=temp(52)
        soilpot(methour,4)=temp(53)
        soilpot(methour,5)=temp(54)
        soilpot(methour,6)=temp(55)
        soilpot(methour,7)=temp(56)
        soilpot(methour,8)=temp(57)
        soilpot(methour,9)=temp(58)
        soilpot(methour,10)=temp(59)
        soilpot(methour,11)=temp(60)
        soilpot(methour,12)=temp(61)
      endif
       ELSE
         methour=int(temp(31)-1)
         shadsoil(methour,1)=temp(19)
         shadsoil(methour,2)=temp(20)
         shadsoil(methour,3)=temp(21)
         shadsoil(methour,4)=temp(22)
         shadsoil(methour,5)=temp(23)
         shadsoil(methour,6)=temp(24)
         shadsoil(methour,7)=temp(25)
         shadsoil(methour,8)=temp(26)
         shadsoil(methour,9)=temp(27)
         shadsoil(methour,10)=temp(28)
         shadsoil(methour,11)=temp(29)
         shadsoil(methour,12)=temp(30)
      if(runmoist.eq.1)then   
        shadmoist(methour,1)=temp(19)
        shadmoist(methour,2)=temp(20)
        shadmoist(methour,3)=temp(32)
        shadmoist(methour,4)=temp(33)
        shadmoist(methour,5)=temp(34)
        shadmoist(methour,6)=temp(35)
        shadmoist(methour,7)=temp(36)
        shadmoist(methour,8)=temp(37)
        shadmoist(methour,9)=temp(38)
        shadmoist(methour,10)=temp(39)
        shadmoist(methour,11)=temp(40)
        shadmoist(methour,12)=temp(41)
        
        shadhumid(methour,1)=temp(19)
        shadhumid(methour,2)=temp(20)
        shadhumid(methour,3)=temp(42)
        shadhumid(methour,4)=temp(43)
        shadhumid(methour,5)=temp(44)
        shadhumid(methour,6)=temp(45)
        shadhumid(methour,7)=temp(46)
        shadhumid(methour,8)=temp(47)
        shadhumid(methour,9)=temp(48)
        shadhumid(methour,10)=temp(49)
        shadhumid(methour,11)=temp(50)
        shadhumid(methour,12)=temp(51)
        
        shadpot(methour,1)=temp(19)
        shadpot(methour,2)=temp(20)
        shadpot(methour,3)=temp(52)
        shadpot(methour,4)=temp(53)
        shadpot(methour,5)=temp(54)
        shadpot(methour,6)=temp(55)
        shadpot(methour,7)=temp(56)
        shadpot(methour,8)=temp(57)
        shadpot(methour,9)=temp(58)
        shadpot(methour,10)=temp(59)
        shadpot(methour,11)=temp(60)
        shadpot(methour,12)=temp(61)
      endif
       ENDIF
      ELSE
C        OUTPUT TO METOUT OR SHADMET(100% SHADE)
        IF(SHAYD.LT.MAXSHD)THEN
         methour=int(temp(31)-1)
         metout(methour,1)=temp(1)
         metout(methour,2)=temp(2)
         metout(methour,3)=temp(3)
         metout(methour,4)=temp(4)
         metout(methour,5)=temp(5)
         metout(methour,6)=temp(6)
         metout(methour,7)=temp(7)
         metout(methour,8)=temp(8)
         metout(methour,9)=temp(9)
         metout(methour,10)=temp(10)
         metout(methour,11)=temp(11)
         metout(methour,12)=temp(12)
         metout(methour,13)=temp(13)
         metout(methour,14)=temp(14)
         metout(methour,15)=temp(15)
         metout(methour,16)=temp(16)
        metout(methour,17)=temp(17)
         metout(methour,18)=temp(18)
        ELSE
         methour=int(temp(31)-1)
         shadmet(methour,1)=temp(1)
         shadmet(methour,2)=temp(2)
         shadmet(methour,3)=temp(3)
         shadmet(methour,4)=temp(4)
         shadmet(methour,5)=temp(5)
         shadmet(methour,6)=temp(6)
         shadmet(methour,7)=temp(7)
         shadmet(methour,8)=temp(8)
         shadmet(methour,9)=temp(9)
         shadmet(methour,10)=temp(10)
         shadmet(methour,11)=temp(11)
         shadmet(methour,12)=temp(12)
         shadmet(methour,13)=temp(13)
         shadmet(methour,14)=temp(14)
         shadmet(methour,15)=temp(15)
         shadmet(methour,16)=temp(16)
         shadmet(methour,17)=temp(17)
         shadmet(methour,18)=temp(18)
        ENDIF
C        OUTPUT TO SOIL OR SHADSOIL(100% SHADE)
        IF(SHAYD.LT.MAXSHD)THEN
         methour=int(temp(31)-1)
         soil(methour,1)=temp(19)
         soil(methour,2)=temp(20)
         soil(methour,3)=temp(21)
         soil(methour,4)=temp(22)
         soil(methour,5)=temp(23)
         soil(methour,6)=temp(24)
         soil(methour,7)=temp(25)
         soil(methour,8)=temp(26)
         soil(methour,9)=temp(27)
         soil(methour,10)=temp(28)
         soil(methour,11)=temp(29)
         soil(methour,12)=temp(30)
      if(runmoist.eq.1)then   
        soilmoist(methour,1)=temp(19)
        soilmoist(methour,2)=temp(20)
        soilmoist(methour,3)=temp(32)
        soilmoist(methour,4)=temp(33)
        soilmoist(methour,5)=temp(34)
        soilmoist(methour,6)=temp(35)
        soilmoist(methour,7)=temp(36)
        soilmoist(methour,8)=temp(37)
        soilmoist(methour,9)=temp(38)
        soilmoist(methour,10)=temp(39)
        soilmoist(methour,11)=temp(40)
        soilmoist(methour,12)=temp(41)
        
        humid(methour,1)=temp(19)
        humid(methour,2)=temp(20)
        humid(methour,3)=temp(42)
        humid(methour,4)=temp(43)
        humid(methour,5)=temp(44)
        humid(methour,6)=temp(45)
        humid(methour,7)=temp(46)
        humid(methour,8)=temp(47)
        humid(methour,9)=temp(48)
        humid(methour,10)=temp(49)
        humid(methour,11)=temp(50)
        humid(methour,12)=temp(51)
         
        soilpot(methour,1)=temp(19)
        soilpot(methour,2)=temp(20)
        soilpot(methour,3)=temp(52)
        soilpot(methour,4)=temp(53)
        soilpot(methour,5)=temp(54)
        soilpot(methour,6)=temp(55)
        soilpot(methour,7)=temp(56)
        soilpot(methour,8)=temp(57)
        soilpot(methour,9)=temp(58)
        soilpot(methour,10)=temp(59)
        soilpot(methour,11)=temp(60)
        soilpot(methour,12)=temp(61)
      endif
        ELSE
         methour=int(temp(31)-1)
         shadsoil(methour,1)=temp(19)
         shadsoil(methour,2)=temp(20)
         shadsoil(methour,3)=temp(21)
         shadsoil(methour,4)=temp(22)
         shadsoil(methour,5)=temp(23)
         shadsoil(methour,6)=temp(24)
         shadsoil(methour,7)=temp(25)
         shadsoil(methour,8)=temp(26)
         shadsoil(methour,9)=temp(27)
         shadsoil(methour,10)=temp(28)
         shadsoil(methour,11)=temp(29)
         shadsoil(methour,12)=temp(30)
      if(runmoist.eq.1)then   
        shadmoist(methour,1)=temp(19)
        shadmoist(methour,2)=temp(20)
        shadmoist(methour,3)=temp(32)
        shadmoist(methour,4)=temp(33)
        shadmoist(methour,5)=temp(34)
        shadmoist(methour,6)=temp(35)
        shadmoist(methour,7)=temp(36)
        shadmoist(methour,8)=temp(37)
        shadmoist(methour,9)=temp(38)
        shadmoist(methour,10)=temp(39)
        shadmoist(methour,11)=temp(40)
        shadmoist(methour,12)=temp(41)
        
        shadhumid(methour,1)=temp(19)
        shadhumid(methour,2)=temp(20)
        shadhumid(methour,3)=temp(42)
        shadhumid(methour,4)=temp(43)
        shadhumid(methour,5)=temp(44)
        shadhumid(methour,6)=temp(45)
        shadhumid(methour,7)=temp(46)
        shadhumid(methour,8)=temp(47)
        shadhumid(methour,9)=temp(48)
        shadhumid(methour,10)=temp(49)
        shadhumid(methour,11)=temp(50)
        shadhumid(methour,12)=temp(51)
        
        shadpot(methour,1)=temp(19)
        shadpot(methour,2)=temp(20)
        shadpot(methour,3)=temp(52)
        shadpot(methour,4)=temp(53)
        shadpot(methour,5)=temp(54)
        shadpot(methour,6)=temp(55)
        shadpot(methour,7)=temp(56)
        shadpot(methour,8)=temp(57)
        shadpot(methour,9)=temp(58)
        shadpot(methour,10)=temp(59)
        shadpot(methour,11)=temp(60)
        shadpot(methour,12)=temp(61)
      endif
         ENDIF
       ENDIF
      ENDIF           
      slipped=0
c     end check for previous slippage      
      endif
      
      IF (((moy.eq.1).and.(time.eq.0)).or.(TIME .NE. LASTIME).or.
     & (TIME .eq.0)) THEN
      if(SIOUT(1).le.1380)then
      IF (SIOUT(1) .EQ. 0.0) THEN 
C      INCLUDE ALTITUDE ON EVERY LINE WITH TIME 0 FOR ANIMAL CALCULATIONS      
       IF(SHAYD.LT.MAXSHD)THEN
C          WRITE TO METOUT
C        TIME TA(LOC) TA(2M) RH(LOC) RH   V(LOC) TS   T2   TDEEP  ZEN   SOLR  TSKY(C) ELEV(M) ABSOIL %SHAD %WET TANNUL
c        WRITE(I3,154)SIOUT(1),SIOUT(2),OUT(2),RHLOCL,SIOUT(3),
c     *    SIOUT(4),SIOUT(5),SIOUT(6),SIOUT(7),SIOUT(8),SIOUT(9),
c     &    SIOUT(10),ALTT,SABNEW,SHAYD,PTWET,TANNUL
        methour=0
        methour=(int(SIOUT(1)/60)+1)+24*(moy-1)
c        write(*,*) methour
        metout(methour,1)=JULDAY(MOY)
        metout(methour,2)=SIOUT(1)
        metout(methour,3)=SIOUT(2)
        metout(methour,4)=OUT(2)
        metout(methour,5)=RHLOCL
        metout(methour,6)=SIOUT(3)
        metout(methour,7)=SIOUT(4)
        metout(methour,8)=VEL2M
        metout(methour,9)=SHAYD
        metout(methour,10)=condep
        metout(methour,11)=SIOUT(7)
        metout(methour,12)=SIOUT(8)
        metout(methour,13)=SIOUT(9)
        metout(methour,14)=SIOUT(10)
        metout(methour,15)=DEW
        metout(methour,16)=FROST
        metout(methour,17)=snowfall
c      metout(methour,17)=melted
        metout(methour,18)=snowout
      if(writecsv.eq.1)then
        WRITE(I3,154) metout(methour,1),",",metout(methour,2),",",
     &metout(methour,3),",",metout(methour,4),",",metout(methour,5),",",
     &metout(methour,6),",",metout(methour,7),",",metout(methour,8),",",
     &metout(methour,9),",",metout(methour,10),",",metout(methour,11),"
     &,",metout(methour,12),",",metout(methour,13),",",metout(methour,14
     &),",",metout(methour,15),",",metout(methour,16),",",
     &metout(methour,17),",",metout(methour,18)
      endif
       ELSE
C        WRITE TO SHADMET
c        WRITE(I12,154)SIOUT(1),SIOUT(2),OUT(2),RHLOCL,SIOUT(3),
c     *    SIOUT(4),SIOUT(5),SIOUT(6),SIOUT(7),SIOUT(8),SIOUT(9),
c     &    SIOUT(10),ALTT,SABNEW,SHAYD,PTWET,TANNUL
        methour=0
        methour=int(SIOUT(1)/60)+1+24*(moy-1)
        shadmet(methour,1)=JULDAY(MOY)
        shadmet(methour,2)=SIOUT(1)
        shadmet(methour,3)=SIOUT(2)
        shadmet(methour,4)=OUT(2)
        shadmet(methour,5)=RHLOCL
        shadmet(methour,6)=SIOUT(3)
        shadmet(methour,7)=SIOUT(4)
        shadmet(methour,8)=VEL2M
        shadmet(methour,9)=SHAYD
        shadmet(methour,10)=condep
        shadmet(methour,11)=SIOUT(7)
        shadmet(methour,12)=SIOUT(8)
        shadmet(methour,13)=SIOUT(9)
        shadmet(methour,14)=SIOUT(10)
        shadmet(methour,15)=DEW
        shadmet(methour,16)=FROST
        shadmet(methour,17)=snowfall
        shadmet(methour,18)=snowout
      if(writecsv.eq.1)then
        WRITE(I12,154) shadmet(methour,1),",",shadmet(methour,2),",",
     &shadmet(methour,3),",",shadmet(methour,4),",",shadmet(methour,5)
     &,",",shadmet(methour,6),",",shadmet(methour,7),",",shadmet(methour
     &,8),",",shadmet(methour,9),",",shadmet(methour,10),",",shadmet(met
     &hour,11),",",shadmet(methour,12),",",shadmet(methour,13),",",shadm
     &et(methour,14),",",shadmet(methour,15),",",shadmet(methour,16),","
     &,shadmet(methour,17),",",shadmet(methour,18)
      endif
       ENDIF
C       WRITE TO FILE SOIL A LINE OF TIME, SOIL TEMPERATURES FROM THE SURFACE DOWN
       IF(SHAYD.LT.MAXSHD)THEN
c        WRITE(I10,157)SIOUT(1),OUT(4),(OUT(I),I=14,IEND)
        methour=0
        methour=int(SIOUT(1)/60)+1+24*(moy-1)
        soil(methour,1)=JULDAY(MOY)
        soil(methour,2)=SIOUT(1)
        soil(methour,3)=OUT(4)
        soil(methour,4)=OUT(14)
        soil(methour,5)=OUT(15)
        soil(methour,6)=OUT(16)
        soil(methour,7)=OUT(17)
        soil(methour,8)=OUT(18)
        soil(methour,9)=OUT(19)
        soil(methour,10)=OUT(20)
        soil(methour,11)=OUT(21)
        soil(methour,12)=OUT(22)
        if(runmoist.eq.1)then
        soilmoist(methour,1)=JULDAY(MOY)
        soilmoist(methour,2)=SIOUT(1)
        soilmoist(methour,3)=curmoist(1)
        soilmoist(methour,4)=curmoist(2)
        soilmoist(methour,5)=curmoist(3)
        soilmoist(methour,6)=curmoist(4)
        soilmoist(methour,7)=curmoist(5)
        soilmoist(methour,8)=curmoist(6)
        soilmoist(methour,9)=curmoist(7)
        soilmoist(methour,10)=curmoist(8)
        soilmoist(methour,11)=curmoist(9)
        soilmoist(methour,12)=curmoist(10)
        
        humid(methour,1)=JULDAY(MOY)
        humid(methour,2)=SIOUT(1)
        humid(methour,3)=curhumid(1)
        humid(methour,4)=curhumid(2)
        humid(methour,5)=curhumid(3)
        humid(methour,6)=curhumid(4)
        humid(methour,7)=curhumid(5)
        humid(methour,8)=curhumid(6)
        humid(methour,9)=curhumid(7)
        humid(methour,10)=curhumid(8)
        humid(methour,11)=curhumid(9)
        humid(methour,12)=curhumid(10)
        
        soilpot(methour,1)=JULDAY(MOY)
        soilpot(methour,2)=SIOUT(1)
        soilpot(methour,3)=curpot(1)
        soilpot(methour,4)=curpot(2)
        soilpot(methour,5)=curpot(3)
        soilpot(methour,6)=curpot(4)
        soilpot(methour,7)=curpot(5)
        soilpot(methour,8)=curpot(6)
        soilpot(methour,9)=curpot(7)
        soilpot(methour,10)=curpot(8)
        soilpot(methour,11)=curpot(9)
        soilpot(methour,12)=curpot(10)
        endif
       
      if(writecsv.eq.1)then
        WRITE(I10,160) soil(methour,1),",",soil(methour,2),",",
     &soil(methour,3),",",soil(methour,4),",",soil(methour,5),",",
     &soil(methour,6),",",soil(methour,7),",",soil(methour,8),",",
     &soil(methour,9),",",soil(methour,10),",",soil(methour,11),"
     &,",soil(methour,12)
      if(runmoist.eq.1)then
      WRITE(I91,160) soilmoist(methour,1),",",soilmoist(methour,2),","
     &,soilmoist(methour,3),",",soilmoist(methour,4),",",soilmoist(metho
     &ur,5),",",soilmoist(methour,6),",",soilmoist(methour,7),",",soilmo
     &ist(methour,8),",",soilmoist(methour,9),",",soilmoist(methour,10)
     &,",",soilmoist(methour,11),",",soilmoist(methour,12)
      WRITE(I92,160) humid(methour,1),",",humid(methour,2),","
     &,humid(methour,3),",",humid(methour,4),",",humid(metho
     &ur,5),",",humid(methour,6),",",humid(methour,7),",",humid
     &(methour,8),",",humid(methour,9),",",humid(methour,10)
     &,",",humid(methour,11),",",humid(methour,12)
      WRITE(I95,161) soilpot(methour,1),",",soilpot(methour,2),","
     &,soilpot(methour,3),",",soilpot(methour,4),",",soilpot(metho
     &ur,5),",",soilpot(methour,6),",",soilpot(methour,7),",",soilpot
     &(methour,8),",",soilpot(methour,9),",",soilpot(methour,10)
     &,",",soilpot(methour,11),",",soilpot(methour,12)
      endif
      endif
       ELSE
c        WRITE(I11,157)SIOUT(1),OUT(4),(OUT(I),I=14,IEND)
        methour=0
        methour=int(SIOUT(1)/60)+1+24*(moy-1)
        shadsoil(methour,1)=JULDAY(MOY)
        shadsoil(methour,2)=SIOUT(1)
        shadsoil(methour,3)=OUT(4)
        shadsoil(methour,4)=OUT(14)
        shadsoil(methour,5)=OUT(15)
        shadsoil(methour,6)=OUT(16)
        shadsoil(methour,7)=OUT(17)
        shadsoil(methour,8)=OUT(18)
        shadsoil(methour,9)=OUT(19)
        shadsoil(methour,10)=OUT(20)
        shadsoil(methour,11)=OUT(21)
        shadsoil(methour,12)=OUT(22)
        if(runmoist.eq.1)then
        shadmoist(methour,1)=JULDAY(MOY)
        shadmoist(methour,2)=SIOUT(1)
        shadmoist(methour,3)=curmoist(1)
        shadmoist(methour,4)=curmoist(2)
        shadmoist(methour,5)=curmoist(3)
        shadmoist(methour,6)=curmoist(4)
        shadmoist(methour,7)=curmoist(5)
        shadmoist(methour,8)=curmoist(6)
        shadmoist(methour,9)=curmoist(7)
        shadmoist(methour,10)=curmoist(8)
        shadmoist(methour,11)=curmoist(9)
        shadmoist(methour,12)=curmoist(10)
        
        shadhumid(methour,1)=JULDAY(MOY)
        shadhumid(methour,2)=SIOUT(1)
        shadhumid(methour,3)=curhumid(1)
        shadhumid(methour,4)=curhumid(2)
        shadhumid(methour,5)=curhumid(3)
        shadhumid(methour,6)=curhumid(4)
        shadhumid(methour,7)=curhumid(5)
        shadhumid(methour,8)=curhumid(6)
        shadhumid(methour,9)=curhumid(7)
        shadhumid(methour,10)=curhumid(8)
        shadhumid(methour,11)=curhumid(9)
        shadhumid(methour,12)=curhumid(10)
        
        shadpot(methour,1)=JULDAY(MOY)
        shadpot(methour,2)=SIOUT(1)
        shadpot(methour,3)=curpot(1)
        shadpot(methour,4)=curpot(2)
        shadpot(methour,5)=curpot(3)
        shadpot(methour,6)=curpot(4)
        shadpot(methour,7)=curpot(5)
        shadpot(methour,8)=curpot(6)
        shadpot(methour,9)=curpot(7)
        shadpot(methour,10)=curpot(8)
        shadpot(methour,11)=curpot(9)
        shadpot(methour,12)=curpot(10)
        endif
      if(writecsv.eq.1)then
        WRITE(I11,157) shadsoil(methour,1),",",shadsoil(methour,2),",",
     &shadsoil(methour,3),",",shadsoil(methour,4),",",
     &shadsoil(methour,5),",",shadsoil(methour,6),",",
     &shadsoil(methour,7),",",shadsoil(methour,8),",",soil(methour,9)
     &,",",shadsoil(methour,10),",",shadsoil(methour,11)
     &,",",soil(methour,12)
      if(runmoist.eq.1)then
      WRITE(I93,160) shadmoist(methour,1),",",shadmoist(methour,2),","
     &,shadmoist(methour,3),",",shadmoist(methour,4),",",shadmoist(metho
     &ur,5),",",shadmoist(methour,6),",",shadmoist(methour,7),",",shadmo
     &ist(methour,8),",",shadmoist(methour,9),",",shadmoist(methour,10)
     &,",",shadmoist(methour,11),",",shadmoist(methour,12)
      WRITE(I94,160) shadhumid(methour,1),",",shadhumid(methour,2),","
     &,shadhumid(methour,3),",",shadhumid(methour,4),",",shadhumid(metho
     &ur,5),",",shadhumid(methour,6),",",shadhumid(methour,7),",",shadhu
     &mid(methour,8),",",shadhumid(methour,9),",",shadhumid(methour,10)
     &,",",shadhumid(methour,11),",",shadhumid(methour,12)
      WRITE(I96,161) shadpot(methour,1),",",shadpot(methour,2),","
     &,shadpot(methour,3),",",shadpot(methour,4),",",shadpot(metho
     &ur,5),",",shadpot(methour,6),",",shadpot(methour,7),",",shadpot
     &(methour,8),",",shadpot(methour,9),",",shadpot(methour,10)
     &,",",shadpot(methour,11),",",shadpot(methour,12)
      endif
      endif
       ENDIF
      ELSE
C        OUTPUT TO METOUT OR SHADMET(100% SHADE)
        IF(SHAYD.LT.MAXSHD)THEN
c          WRITE(I3,155)SIOUT(1),SIOUT(2),OUT(2),RHLOCL,SIOUT(3),
c     *      SIOUT(4),SIOUT(5),SIOUT(6),SIOUT(7),SIOUT(8),SIOUT(9),
c     &      SIOUT(10)
         methour=0
         methour=int(SIOUT(1)/60)+1+24*(moy-1)
c         write(*,*) methour
         metout(methour,1)=JULDAY(MOY)
         metout(methour,2)=SIOUT(1)
         metout(methour,3)=SIOUT(2)
         metout(methour,4)=OUT(2)
         metout(methour,5)=RHLOCL
         metout(methour,6)=SIOUT(3)
         metout(methour,7)=SIOUT(4)
         metout(methour,8)=VEL2M
         metout(methour,9)=SHAYD
         metout(methour,10)=condep
         metout(methour,11)=SIOUT(7)
         metout(methour,12)=SIOUT(8)
         metout(methour,13)=SIOUT(9)
         metout(methour,14)=SIOUT(10)
         metout(methour,15)=DEW
         metout(methour,16)=FROST
        metout(methour,17)=snowfall
c      metout(methour,17)=melted
         metout(methour,18)=snowout
      if(writecsv.eq.1)then
        WRITE(I3,154) metout(methour,1),",",metout(methour,2),",",
     &metout(methour,3),",",metout(methour,4),",",metout(methour,5),",",
     &metout(methour,6),",",metout(methour,7),",",metout(methour,8),",",
     &metout(methour,9),",",metout(methour,10),",",metout(methour,11),"
     &,",metout(methour,12),",",metout(methour,13),",",metout(methour,14
     &),",",metout(methour,15),",",metout(methour,16),",",
     &metout(methour,17),",",metout(methour,18)
      endif
        ELSE
c         WRITE(I12,155)SIOUT(1),SIOUT(2),OUT(2),RHLOCL,SIOUT(3),
c     *     SIOUT(4),SIOUT(5),SIOUT(6),SIOUT(7),SIOUT(8),SIOUT(9),
c     &     SIOUT(10)
         methour=0
         methour=int(SIOUT(1)/60)+1+24*(moy-1)
         shadmet(methour,1)=JULDAY(MOY)
         shadmet(methour,2)=SIOUT(1)
         shadmet(methour,3)=SIOUT(2)
         shadmet(methour,4)=OUT(2)
         shadmet(methour,5)=RHLOCL
         shadmet(methour,6)=SIOUT(3)
         shadmet(methour,7)=SIOUT(4)
         shadmet(methour,8)=VEL2M
         shadmet(methour,9)=SHAYD
         shadmet(methour,10)=condep
         shadmet(methour,11)=SIOUT(7)
         shadmet(methour,12)=SIOUT(8)
         shadmet(methour,13)=SIOUT(9)
         shadmet(methour,14)=SIOUT(10)
         shadmet(methour,15)=DEW
         shadmet(methour,16)=FROST
         shadmet(methour,17)=snowfall
         shadmet(methour,18)=snowout
      if(writecsv.eq.1)then
        WRITE(I12,154) shadmet(methour,1),",",shadmet(methour,2),",",
     &shadmet(methour,3),",",shadmet(methour,4),",",shadmet(methour,5)
     &,",",shadmet(methour,6),",",shadmet(methour,7),",",shadmet(methour
     &,8),",",shadmet(methour,9),",",shadmet(methour,10),",",shadmet(met
     &hour,11),",",shadmet(methour,12),",",shadmet(methour,13),",",shadm
     &et(methour,14),",",shadmet(methour,15),",",shadmet(methour,16),","
     &,shadmet(methour,17),",",shadmet(methour,18)
      endif
        ENDIF
C        OUTPUT TO SOIL OR SHADSOIL(100% SHADE)
        IF(SHAYD.LT.MAXSHD)THEN
c          WRITE(I10,157)SIOUT(1),OUT(4),(OUT(I),I=14,IEND)
         methour=0
         methour=int(SIOUT(1)/60)+1+24*(moy-1)
         soil(methour,1)=JULDAY(MOY)
         soil(methour,2)=SIOUT(1)
         soil(methour,3)=OUT(4)
         soil(methour,4)=OUT(14)
         soil(methour,5)=OUT(15)
         soil(methour,6)=OUT(16)
         soil(methour,7)=OUT(17)
         soil(methour,8)=OUT(18)
         soil(methour,9)=OUT(19)
         soil(methour,10)=OUT(20)
         soil(methour,11)=OUT(21)
         soil(methour,12)=OUT(22)
         if(runmoist.eq.1)then
        soilmoist(methour,1)=JULDAY(MOY)
        soilmoist(methour,2)=SIOUT(1)
        soilmoist(methour,3)=curmoist(1)
        soilmoist(methour,4)=curmoist(2)
        soilmoist(methour,5)=curmoist(3)
        soilmoist(methour,6)=curmoist(4)
        soilmoist(methour,7)=curmoist(5)
        soilmoist(methour,8)=curmoist(6)
        soilmoist(methour,9)=curmoist(7)
        soilmoist(methour,10)=curmoist(8)
        soilmoist(methour,11)=curmoist(9)
        soilmoist(methour,12)=curmoist(10)
        
        humid(methour,1)=JULDAY(MOY)
        humid(methour,2)=SIOUT(1)
        humid(methour,3)=curhumid(1)
        humid(methour,4)=curhumid(2)
        humid(methour,5)=curhumid(3)
        humid(methour,6)=curhumid(4)
        humid(methour,7)=curhumid(5)
        humid(methour,8)=curhumid(6)
        humid(methour,9)=curhumid(7)
        humid(methour,10)=curhumid(8)
        humid(methour,11)=curhumid(9)
        humid(methour,12)=curhumid(10)
        
        soilpot(methour,1)=JULDAY(MOY)
        soilpot(methour,2)=SIOUT(1)
        soilpot(methour,3)=curpot(1)
        soilpot(methour,4)=curpot(2)
        soilpot(methour,5)=curpot(3)
        soilpot(methour,6)=curpot(4)
        soilpot(methour,7)=curpot(5)
        soilpot(methour,8)=curpot(6)
        soilpot(methour,9)=curpot(7)
        soilpot(methour,10)=curpot(8)
        soilpot(methour,11)=curpot(9)
        soilpot(methour,12)=curpot(10)
         endif
      if(writecsv.eq.1)then
        WRITE(I10,160) soil(methour,1),",",soil(methour,2),",",
     &soil(methour,3),",",soil(methour,4),",",soil(methour,5),",",
     &soil(methour,6),",",soil(methour,7),",",soil(methour,8),",",
     &soil(methour,9),",",soil(methour,10),",",soil(methour,11),"
     &,",soil(methour,12)
      if(runmoist.eq.1)then
      WRITE(I91,160) soilmoist(methour,1),",",soilmoist(methour,2),","
     &,soilmoist(methour,3),",",soilmoist(methour,4),",",soilmoist(metho
     &ur,5),",",soilmoist(methour,6),",",soilmoist(methour,7),",",soilmo
     &ist(methour,8),",",soilmoist(methour,9),",",soilmoist(methour,10)
     &,",",soilmoist(methour,11),",",soilmoist(methour,12)
      WRITE(I92,160) humid(methour,1),",",humid(methour,2),","
     &,humid(methour,3),",",humid(methour,4),",",humid(metho
     &ur,5),",",humid(methour,6),",",humid(methour,7),",",humid
     &(methour,8),",",humid(methour,9),",",humid(methour,10)
     &,",",humid(methour,11),",",humid(methour,12)
      WRITE(I95,161) soilpot(methour,1),",",soilpot(methour,2),","
     &,soilpot(methour,3),",",soilpot(methour,4),",",soilpot(metho
     &ur,5),",",soilpot(methour,6),",",soilpot(methour,7),",",soilpot
     &(methour,8),",",soilpot(methour,9),",",soilpot(methour,10)
     &,",",soilpot(methour,11),",",soilpot(methour,12)
      endif
      endif
        ELSE
c          WRITE(I11,157)SIOUT(1),OUT(4),(OUT(I),I=14,IEND)
         methour=0
         methour=int(SIOUT(1)/60)+1+24*(moy-1)
         shadsoil(methour,1)=JULDAY(MOY)
         shadsoil(methour,2)=SIOUT(1)
         shadsoil(methour,3)=OUT(4)
         shadsoil(methour,4)=OUT(14)
         shadsoil(methour,5)=OUT(15)
         shadsoil(methour,6)=OUT(16)
         shadsoil(methour,7)=OUT(17)
         shadsoil(methour,8)=OUT(18)
         shadsoil(methour,9)=OUT(19)
         shadsoil(methour,10)=OUT(20)
         shadsoil(methour,11)=OUT(21)
         shadsoil(methour,12)=OUT(22)
         if(runmoist.eq.1)then
        shadmoist(methour,1)=JULDAY(MOY)
        shadmoist(methour,2)=SIOUT(1)
        shadmoist(methour,3)=curmoist(1)
        shadmoist(methour,4)=curmoist(2)
        shadmoist(methour,5)=curmoist(3)
        shadmoist(methour,6)=curmoist(4)
        shadmoist(methour,7)=curmoist(5)
        shadmoist(methour,8)=curmoist(6)
        shadmoist(methour,9)=curmoist(7)
        shadmoist(methour,10)=curmoist(8)
        shadmoist(methour,11)=curmoist(9)
        shadmoist(methour,12)=curmoist(10)
        
        shadhumid(methour,1)=JULDAY(MOY)
        shadhumid(methour,2)=SIOUT(1)
        shadhumid(methour,3)=curhumid(1)
        shadhumid(methour,4)=curhumid(2)
        shadhumid(methour,5)=curhumid(3)
        shadhumid(methour,6)=curhumid(4)
        shadhumid(methour,7)=curhumid(5)
        shadhumid(methour,8)=curhumid(6)
        shadhumid(methour,9)=curhumid(7)
        shadhumid(methour,10)=curhumid(8)
        shadhumid(methour,11)=curhumid(9)
        shadhumid(methour,12)=curhumid(10)
        
        shadpot(methour,1)=JULDAY(MOY)
        shadpot(methour,2)=SIOUT(1)
        shadpot(methour,3)=curpot(1)
        shadpot(methour,4)=curpot(2)
        shadpot(methour,5)=curpot(3)
        shadpot(methour,6)=curpot(4)
        shadpot(methour,7)=curpot(5)
        shadpot(methour,8)=curpot(6)
        shadpot(methour,9)=curpot(7)
        shadpot(methour,10)=curpot(8)
        shadpot(methour,11)=curpot(9)
        shadpot(methour,12)=curpot(10)
        endif
      if(writecsv.eq.1)then
        WRITE(I11,157) shadsoil(methour,1),",",shadsoil(methour,2),",",
     &shadsoil(methour,3),",",shadsoil(methour,4),",",
     &shadsoil(methour,5),",",shadsoil(methour,6),",",
     &shadsoil(methour,7),",",shadsoil(methour,8),",",soil(methour,9)
     &,",",shadsoil(methour,10),",",shadsoil(methour,11)
     &,",",soil(methour,12)
      if(runmoist.eq.1)then
      WRITE(I93,160) shadmoist(methour,1),",",shadmoist(methour,2),","
     &,shadmoist(methour,3),",",shadmoist(methour,4),",",shadmoist(metho
     &ur,5),",",shadmoist(methour,6),",",shadmoist(methour,7),",",shadmo
     &ist(methour,8),",",shadmoist(methour,9),",",shadmoist(methour,10)
     &,",",shadmoist(methour,11),",",shadmoist(methour,12)
      WRITE(I94,160) shadhumid(methour,1),",",shadhumid(methour,2),","
     &,shadhumid(methour,3),",",shadhumid(methour,4),",",shadhumid(metho
     &ur,5),",",shadhumid(methour,6),",",shadhumid(methour,7),",",shadhu
     &mid(methour,8),",",shadhumid(methour,9),",",shadhumid(methour,10)
     &,",",shadhumid(methour,11),",",shadhumid(methour,12)
      WRITE(I96,161) shadpot(methour,1),",",shadpot(methour,2),","
     &,shadpot(methour,3),",",shadpot(methour,4),",",shadpot(metho
     &ur,5),",",shadpot(methour,6),",",shadpot(methour,7),",",shadpot
     &(methour,8),",",shadpot(methour,9),",",shadpot(methour,10)
     &,",",shadpot(methour,11),",",shadpot(methour,12)
      endif
      endif
        ENDIF
       ENDIF
      ENDIF 
c     check if duplicate time due to integrator slipping 
      else
      slipped=1
      IF(SHAYD.LT.MAXSHD)THEN
        temp(31)=(int(SIOUT(1)/60)+1)+24*(moy-1)
        temp(1)=JULDAY(MOY)
        temp(2)=SIOUT(1)
        temp(3)=SIOUT(2)
        temp(4)=OUT(2)
        temp(5)=RHLOCL
        temp(6)=SIOUT(3)
        temp(7)=SIOUT(4)
        temp(8)=VEL2M
        temp(9)=SHAYD
        temp(10)=condep
        temp(11)=SIOUT(7)
        temp(12)=SIOUT(8)
        temp(13)=SIOUT(9)
        temp(14)=SIOUT(10)
        temp(15)=DEW
        temp(16)=FROST
        temp(17)=snowfall
        temp(18)=snowout
        temp(19)=JULDAY(MOY)
        temp(20)=SIOUT(1)
        temp(21)=OUT(4)
        temp(22)=OUT(14)
        temp(23)=OUT(15)
        temp(24)=OUT(16)
        temp(25)=OUT(17)
        temp(26)=OUT(18)
        temp(27)=OUT(19)
        temp(28)=OUT(20)
        temp(29)=OUT(21)
        temp(30)=OUT(22)
        if(runmoist.eq.1)then
        temp(32)=curmoist(1)
        temp(33)=curmoist(2)
        temp(34)=curmoist(3)
        temp(35)=curmoist(4)
        temp(36)=curmoist(5)
        temp(37)=curmoist(6)
        temp(38)=curmoist(7)
        temp(39)=curmoist(8)
        temp(40)=curmoist(9)
        temp(41)=curmoist(10)
        temp(42)=curmoist(1)
        temp(43)=curmoist(2)
        temp(44)=curmoist(3)
        temp(45)=curmoist(4)
        temp(46)=curmoist(5)
        temp(47)=curmoist(6)
        temp(48)=curmoist(7)
        temp(49)=curmoist(8)
        temp(50)=curmoist(9)
        temp(51)=curmoist(10)
        temp(52)=curpot(1)
        temp(53)=curpot(2)
        temp(54)=curpot(3)
        temp(55)=curpot(4)
        temp(56)=curpot(5)
        temp(57)=curpot(6)
        temp(58)=curpot(7)
        temp(59)=curpot(8)
        temp(60)=curpot(9)
        temp(61)=curpot(10)
        endif
            else
        temp(31)=int(SIOUT(1)/60)+1+24*(moy-1)
        temp(1)=JULDAY(MOY)
        temp(2)=SIOUT(1)
        temp(3)=SIOUT(2)
        temp(4)=OUT(2)
        temp(5)=RHLOCL
        temp(6)=SIOUT(3)
        temp(7)=SIOUT(4)
        temp(8)=VEL2M
        temp(9)=SHAYD
        temp(10)=condep
        temp(11)=SIOUT(7)
        temp(12)=SIOUT(8)
        temp(13)=SIOUT(9)
        temp(14)=SIOUT(10)
        temp(15)=DEW
        temp(16)=FROST
        temp(17)=snowfall
        temp(18)=snowout
        temp(19)=JULDAY(MOY)
         temp(20)=SIOUT(1)
         temp(21)=OUT(4)
         temp(22)=OUT(14)
         temp(23)=OUT(15)
         temp(24)=OUT(16)
         temp(25)=OUT(17)
         temp(26)=OUT(18)
         temp(27)=OUT(19)
         temp(28)=OUT(20)
         temp(29)=OUT(21)
         temp(30)=OUT(22)
         if(runmoist.eq.1)then
        temp(32)=curmoist(1)
        temp(33)=curmoist(2)
        temp(34)=curmoist(3)
        temp(35)=curmoist(4)
        temp(36)=curmoist(5)
        temp(37)=curmoist(6)
        temp(38)=curmoist(7)
        temp(39)=curmoist(8)
        temp(40)=curmoist(9)
        temp(41)=curmoist(10)
        temp(42)=curhumid(1)
        temp(43)=curhumid(2)
        temp(44)=curhumid(3)
        temp(45)=curhumid(4)
        temp(46)=curhumid(5)
        temp(47)=curhumid(6)
        temp(48)=curhumid(7)
        temp(49)=curhumid(8)
        temp(50)=curhumid(9)
        temp(51)=curhumid(10)
        temp(52)=curpot(1)
        temp(53)=curpot(2)
        temp(54)=curpot(3)
        temp(55)=curpot(4)
        temp(56)=curpot(5)
        temp(57)=curpot(6)
        temp(58)=curpot(7)
        temp(59)=curpot(8)
        temp(60)=curpot(9)
        temp(61)=curpot(10)
         endif
      endif
c     end check for current slippage      
      endif
      IF (SIOUT(1) .EQ. 1440.) THEN
        IF (MOY .LT. JULNUM) THEN
          IF(SHAYD.LT.MAXSHD)THEN
c          WRITE(I3,*)'END'
           ELSE
c          WRITE(I12,*)'END'
          ENDIF
         ELSE
C        END OF THE YEAR. PUT OUT STOP CODE FOR METOUT
          IF(SHAYD.LT.MAXSHD)THEN
C          WRITE TO FILE METOUT
c            WRITE(I3,*)'STP'
           ELSE
C          WRITE TO FILE SHADMET
c          WRITE(I12,*)'STP'
          ENDIF
        ENDIF
C      INCREMENT MONTH/TIME INTERVAL COUNTER
      IF (((moy.eq.1).and.(time.eq.0)).or.(TIME .NE. LASTIME)) THEN
        MOY = MOY + 1
      endif
      ENDIF
      lastime=time

c  154 FORMAT(1F5.0,2F6.1,2F5.0,1F6.2,4F6.1,1E12.4,1F6.1,1F10.3,1F7.2,
c     & 1F5.0,2F6.1,1I4)
  154 FORMAT(1F4.0,A,1F7.2,A,F6.2,A,F6.2,A,F6.2,A,F6.2,A,F7.3,A,F7.3,A,
     &F6.2,A,F6.2,A,F6.2,A,1F6.2,A,1F7.2,A,1F7.2,A,1F2.0,A,1F2.0,A,F7.2,
     &A,F7.2)     
c  155 FORMAT(1F5.0,2F6.1,2F5.0,1F6.2,4F6.1,1E12.4,1F6.1)
c  156 FORMAT(7X,1F6.0,10F7.2)
c 157 FORMAT(1F6.0,10F7.2)    
  157 FORMAT(1F4.0,A,1F7.2,A,F7.2,A,F7.2,A,F7.2,A,F7.2,A,F7.2,A,F7.2,A
     & ,F7.2,A,F7.2,A,F7.2,A,F7.2)
c  158 FORMAT('TIME TA(LOC)TA(2M) RH(LOC) RH V(LOC) TS   T2   TDEEP   
c     &ZEN   SOLR      TSKY(C) ELEV(M) ABSOIL %SHAD %WET TANNUL') 
  160 FORMAT(1F4.0,A,1F7.2,A,F7.2,A,F7.2,A,F7.2,A,F7.2,A,F7.2,A,F7.2,A
     & ,F7.2,A,F7.2,A,F7.2,A,F7.2,A,F7.2,A,F7.2)
  161 FORMAT(1F4.0,A,1F10.2,A,F10.2,A,F10.2,A,F10.2,A,F10.2,A,F10.2
     & ,A,F10.2,A,F10.2,A,F10.2,A,F10.2,A,F10.2,A,F10.2,A,F10.2)
  200 RETURN
      END   
  
