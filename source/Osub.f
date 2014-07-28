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
      REAL RCSP,HGTP,RUFP,BEGP,PRTP,ERRP,snowout,
     & SLEP,NONP,SUN,PLT,rainfall,snowtemp,snowdens,snowmelt
      REAL CLEAR,QRADSK,SIGP,TAIR,SRAD,QEVAP,frosttest,vel2m
      real rainsnow,snowfall,htovpr,water,gwsurf,netsnow,air

      REAL metout,shadmet,soil,shadsoil,rain,snowhr,FROST
      real bp,hrad,patmos,pstd,qrad,qradhl,viewf,wb,wtrpot,temp
      real DENDAY,SPDAY,TKDAY,DENDAY2,SPDAY2,TKDAY2,time2,time3

      INTEGER CONS,I,IEND,IFINAL,ILOCT,IOUT,IPRINT,ITEST
      INTEGER J,JULNUM,MM,MOY,N,NAIR,ND,NOUT,dew,writecsv
      INTEGER I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,slipped

      INTEGER methour

      CHARACTER*3 SYMBOL,INAME,STP  
      CHARACTER*6 NAME, HEAD
      CHARACTER*1 SNO,SNOW

C      IOUT IS THE NUMBER OF THE OUTPUT TERM DESIRED AS NUMBERED IN NAME ARRAY     
C      FILES 6,7,10 & 12 ARE CONSOLE, OUTPUT, METOUT & SOIL RESPECTIVELY
C      FILES 6,I2,I3 & I10 ARE CONSOLE, OUTPUT, METOUT & SOIL RESPECTIVELY

      DIMENSION DAY(7300),DEPP(30),OUT2(55),NAME(55)
      DIMENSION TMAXX(7300),TMINN(7300),HEAD(55),JULDAY(7300)
      DIMENSION SNOW(7300),REFLS(7300),PCTWET(7300),rain(7300)

      DIMENSION METOUT(24*7300,18),SHADMET(24*7300,18)
      DIMENSION SOIL(24*7300,12),SHADSOIL(24*7300,12)
      DIMENSION SNOWHR(25*7300)
      DIMENSION DENDAY(10),SPDAY(10),TKDAY(10),DENDAY2(10),
     &    SPDAY2(10),TKDAY2(10),temp(31)
        
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
      COMMON/WMAIN/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12
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
      COMMON/VIEWFACT/VIEWF
      COMMON/RAINY/RAIN      
      COMMON/SOYFILS/DENDAY,SPDAY,TKDAY
      common/prevtime/lastime,slipped,temp
         
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
          PTWET = PCTWET(MOY)
          rainfall=RAIN(MOY)
          CONTINUE
        ENDIF 
    5 CONTINUE 
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
      QEVAP = OUT(101)

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

      if(gwsurf.lt.0)then
      gwsurf=0
      endif

      netsnow=snowfall-gwsurf/snowmelt/(rainsnow*1.)
c    netsnow=snowfall

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
        metout(methour,9)=OUT(4)
        metout(methour,10)=OUT(14)
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
        shadmet(methour,9)=OUT(4)
        shadmet(methour,10)=OUT(14)
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
       
      if(writecsv.eq.1)then
        WRITE(I10,157) soil(methour,1),",",soil(methour,2),",",
     &soil(methour,3),",",soil(methour,4),",",soil(methour,5),",",
     &soil(methour,6),",",soil(methour,7),",",soil(methour,8),",",
     &soil(methour,9),",",soil(methour,10),",",soil(methour,11),"
     &,",soil(methour,12)
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
      if(writecsv.eq.1)then
        WRITE(I11,157) shadsoil(methour,1),",",shadsoil(methour,2),",",
     &shadsoil(methour,3),",",shadsoil(methour,4),",",
     &shadsoil(methour,5),",",shadsoil(methour,6),",",
     &shadsoil(methour,7),",",shadsoil(methour,8),",",soil(methour,9)
     &,",",shadsoil(methour,10),",",shadsoil(methour,11)
     &,",",soil(methour,12)
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
         metout(methour,9)=OUT(4)
         metout(methour,10)=OUT(14)
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
         shadmet(methour,9)=OUT(4)
         shadmet(methour,10)=OUT(14)
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
      if(writecsv.eq.1)then
        WRITE(I10,157) soil(methour,1),",",soil(methour,2),",",
     &soil(methour,3),",",soil(methour,4),",",soil(methour,5),",",
     &soil(methour,6),",",soil(methour,7),",",soil(methour,8),",",
     &soil(methour,9),",",soil(methour,10),",",soil(methour,11),"
     &,",soil(methour,12)
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
      if(writecsv.eq.1)then
        WRITE(I11,157) shadsoil(methour,1),",",shadsoil(methour,2),",",
     &shadsoil(methour,3),",",shadsoil(methour,4),",",
     &shadsoil(methour,5),",",shadsoil(methour,6),",",
     &shadsoil(methour,7),",",shadsoil(methour,8),",",soil(methour,9)
     &,",",shadsoil(methour,10),",",shadsoil(methour,11)
     &,",",soil(methour,12)
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
        temp(9)=OUT(4)
        temp(10)=OUT(14)
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
        temp(9)=OUT(4)
        temp(10)=OUT(14)
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
        MOY = MOY + 1
      ENDIF


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
  159 FORMAT(1F4.0,A,1F7.2,A,F7.2,A,F7.2,A,F7.2,A,F7.2,A,F7.2,A,F7.2,A
     & ,F7.2,A,F7.2,A,F7.2,A,F7.2,A,F7.2,A,F7.2)

  200 RETURN
      END   
  
