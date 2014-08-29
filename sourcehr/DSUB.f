      SUBROUTINE DSUB(TIME,T,DTDT) 
      
      IMPLICIT NONE
      EXTERNAL TAB 

C    COPYRIGHT 2011  WARREN P. PORTER,  ALL RIGHTS RESERVED

      REAL A,ALAT,ALIZ,ALONC,ALPLIZ,ALTT,AMOL,AMULT,ARAD,ARLIZ,AZMUTH
      REAL B,BEGP,BP,C,CC,CLEAR,CLOUD,CLR,CMH2O,CP,CRAD,CS,CZ,CZSL
      REAL D,DAS,DENAIR,DENDAY,DENSITYS,DEPP,DEP,DP,DTAU,DTDT
      REAL ERRP,E,END,EPSLIZ,ERR1,ESAT,F,F1,F2,FIN,GRDIN
      REAL H,HC,HD,HEMIS,HGTP,HLIZ,HTRN,JULDAY,KSOYL
      REAL MASS,MAXSHD,MON,OUT
      REAL PATMOS,PCTEVP,PI,PLT,PRESS,PRTP,PSTD,PTWET,PUNSH
      REAL QCOND,QCONV,QEVAP,snowtemp,snowdens,snowmelt
      REAL QRAD,QRADGR,QRADSK,QRADVG,QSOLAR,RB,RCSP,REFL,RH,RS
      REAL RUFP,RW,SAB,SABNEW,SHAYD,SHDLIZ,SIGP,SIOUT,SKYIN,SLEP
      REAL SLOPE,SMET,SOK,SOLR,SPDAY,SPHEATS,SRAD,STP,SUN
      REAL T,TAB,TAIR,TCI,TDS,TGRD,THCONDS,TKDAY,TIM,TIMCOR,TIME,TIMEF 
      REAL TMAX,TMIN,TS,TSI,TSKY,TSNHR,TSRHR,TTEST,TVINC,TVIR
      REAL VD,VELR,VV,WB,WC,WLIZ,WTRPOT,X,XXX,snow
      REAL ZENR,ZSLR,ZZ,Z01,Z02,ZH1,ZH2,HRAD,QRADHL,VIEWF,TT,snowhr
      REAL sles,sle,err,soilprop,moists,moist,Thconduct,Density,Spheat
      REAL refls,pctwet,rainfall,lastime,temp
    
      INTEGER I,I1,I2,IALT,IDA,IDAYST,IEND,IEP
      INTEGER IOUT,IPINT,IPRINT,ISTART,ITEST
      INTEGER IUV,J,JULNUM,K,L
      INTEGER M1,MM,MOY,N,N1,NAIR,NONP,NODES,NOSCAT,NOUT 
      INTEGER Numtyps,Numint,Intrvls,hour,slipped

      CHARACTER*3 INAME,SYMBOL
      CHARACTER*1 SNO  

      DIMENSION T(30),TT(30), DTDT(18), DEPP(30),KSOYL(10)
      DIMENSION Nodes(10,7300)
      DIMENSION Intrvls(7300),DENDAY(10),SPDAY(10),TKDAY(10),temp(31)
      DIMENSION soilprop(10,6),moists(10,7300),SLES(7300),moist(10)
      DIMENSION Thconduct(10),Density(10),Spheat(10),julday(7300)
      DIMENSION REFLS(7300),PCTWET(7300),snow(7300),SNOWHR(25*7300)
       
      COMMON/AIRRAY/ZZ(10),VV(10)
      COMMON/DMYCRO/Z01,Z02,ZH1,ZH2  
      COMMON/WDSUB/TSKY,ARAD,CRAD,CLOUD,CLR,SOLR
      COMMON/PLIZ/ALIZ,WLIZ,ARLIZ,HLIZ,ALPLIZ,EPSLIZ,TMAX,TMIN  
      COMMON/PAR/SIGP,RCSP,SOK,SAB,HGTP,RUFP,BEGP,MON,PRTP,ERRP,END,   
     1 SLEP,DAS,NONP,SUN,PLT,FIN,STP    
      COMMON/NONSCR/M1,N1,XXX,TIMEF,DTAU,ERR1,H,NOUT,NAIR,IPRINT  
      COMMON/ARRAY/X(30),WC(20),C(20),DEP(30),IOUT(100),       
     1 OUT(101),ITEST(23)   
      COMMON/CARRAY/INAME(20),SYMBOL(23)
      COMMON/FUN1/SKYIN,GRDIN,MASS,PCTEVP,TCI,TSI,TIM,TGRD,CC,CS,RS,    
     *A,B,D,F,HTRN,F1,F2,TS,SMET,RB,SHDLIZ  
      COMMON/SIUNIT/SIOUT(10)   
      COMMON/WIOCONS/IPINT,NOSCAT,IUV,PUNSH,IALT,ALAT,AMULT,PRESS,
     * CMH2O,REFL,ALONC,IDAYST,IDA,TIMCOR,AZMUTH,SLOPE,TSNHR,TSRHR,IEP,
     * ISTART,IEND,HEMIS  
      COMMON/GROUND/SHAYD,ALTT,MAXSHD,SABNEW,PTWET,rainfall
      COMMON/GRND2/SNO
      COMMON/WINTER/SNOW
      COMMON/SNOWPRED/SNOWHR,snowtemp,snowdens,snowmelt
      
      COMMON/DAYJUL/JULDAY,JULNUM,MOY 
c    Variable soil properties data from Iomet1
      COMMON/SOYVAR1/Numtyps,Numint,Intrvls
      COMMON/SOYVAR2/Thconds,Densitys,Spheats,Nodes,KSOYL 
      COMMON/SOYFILS/DENDAY,SPDAY,TKDAY
      COMMON/VIEWFACT/VIEWF
      COMMON/NICHEMAPRIO/SLE,ERR,SLES,soilprop,moists,moist
      COMMON/WINTER2/REFLS,PCTWET
      common/prevtime/lastime,slipped,temp

C    NOTATION
C    Key Variables
C    MOY = 'MONTH OF YEAR', = simulation day number
C    IDA = TOTAL DAYS OF SIMULATION
C    IDAST = STARTING DAY OF SIMULATION
C    Numtyps = # of substrate types, e.g. snow, rock, sandy soil
C    Numint = number of simulation days for each soil configuration of substrates vertically arranged, e.g.  snow depth, soil type(s) below snow
C    Intrvls = duration of each vertical soil configuration just defined above.
C    Thconds = substrate conductivities, e.g. snow, soil type(s)
C    Densitys = substrate densities, e.g. snow, soil type(s)
C    Spheats = substrate specific heats, e.g. snow, soil type(s)
C    Nodes = Deepest node for the each substrate type number for each time interval (duration) of vertical arrangement of substrates 
C    Nodes(max node depth,subst type) are real numbers. The number to the left of the decimal point is the deepest node for the substrate type, which is to the right of the decimal point.
      slipped=0
      do 33 i=1,31
          temp(i)=0.
33    continue
      hour=int(time/60+1)
      if(hour.eq.0)then
       hour=1
      endif
C      MM=M1 
      N=N1  
      PI=3.14159
C    CHECK FOR UNSTABLE CONDITIONS OF GROUND SURFACE TEMPERATURE, T(1)
      IF(T(1).GE. 80.0)THEN
        T(1) = 80.
       ELSE
        IF(T(1).LT.-80)THEN
          T(1) = -80
        ENDIF
      ENDIF 
      IF(ALIZ.LE.0.) GO TO 41   
      IF(TIME .LE. 0.) T(M1)=20.    
      K=1  
      MM=M1-K   
      N=N1-K
  41  CONTINUE  
      IF(TIME-BEGP) 1,1,100 
C**** FIND HEIGHTS ABOVE GROUND FOR DETERMINING TEMP AND VELOCITY AND   
C**** PUT DEPTHS IN DEPP(I) AND HEIGHTS IN ZZ(I)
C     HEIGHTS IN ZZ ARE FROM THE GROUND UP, SO ARE VELOCITIES, VV,  
C     BUT DO NOT INCLUDE THE SURFACE
    1 CONTINUE  
      DO 2 I=1,10   
        VV(I)=0.  
        ZZ(I)=0
    2 CONTINUE
      NAIR =0   
   72 NAIR=NAIR+1   
      IF(DEP(NAIR)) 72,73,73   
 73   NAIR=NAIR-1   
      DO 74 I=1,N   
      J=I+NAIR  
   74 DEPP(I)=DEP(J)   
      IF(NAIR.LE.0) GO TO 77
      DO 76 I=1,NAIR    
        J=NAIR+1-I
   76 ZZ(I)=ABS(DEP(J))
   77 CONTINUE  
      
C      SURFACE ABSORPTIVITY FOR SUB. DSUB CALCULATIONS
        SABNEW = 1.000 - REFLS(moy)
C      SURFACE REFLECTIVITY FOR SOLRAD CALCULATIONS
        REFL = REFLS(moy)
        SLE = SLES(moy) 
        moist=moists(1:10,moy)

C**** INITIALIZE VALUES OF C, WC 
c    DENSITY (RHO) TIMES HEAT CAPACITY = RCSP. Assuming unit area of ground surface
c    WC = mass * specific heat (thermal capacitance) = density*volume*specific heat. Assuming unit area. 
      IF(NUMTYPS.EQ.1)THEN
C      Start the CONSTANT SOIL VARIABLES WITH DEPTH initialization calculations
       TT=T
       call soilprops(TT,ALTT,soilprop,moist,Thconduct,Density,Spheat)
       TT=T 
       DENDAY(1)=Density(1)
       SPDAY(1)=Spheat(1)
       TKDAY(1)=Thconduct(1)         
        RCSP = DENDAY(1)*SPDAY(1)
        WC(1)=RCSP*DEPP(2)/2. 
C      Watts/m^2-C.
        SOK =TKDAY(1)
        C(1)=SOK/DEPP(2)
C      COMPUTE THERMAL PROPERTIES OF ALL NODES INTO THE SOIL BELOW THE SURFACE
        DO 5 I=2,N   
C       mass*specific heat product (per unit area)
         WC(I)=RCSP*(DEPP(I+1)-DEPP(I-1))/2.   
   5     C(I)=SOK/(DEPP(I+1)-DEPP(I))
       ELSE
c      Use variable soil properties with depth

       TT=T
       call soilprops(TT,ALTT,soilprop,moist,Thconduct,Density,Spheat)
       TT=T 
       DENDAY(1)=Density(1)
       SPDAY(1)=Spheat(1)
       TKDAY(1)=Thconduct(1)  

        RCSP = DENDAY(1)*SPDAY(1)
        WC(1)=RCSP*DEPP(2)/2.
        SOK =TKDAY(1)
        C(1)=SOK/DEPP(2)

        DO 51 I=2,N  
         
       TT=T
       call soilprops(TT,ALTT,soilprop,moist,Thconduct,Density,Spheat)
       TT=T 
       DENDAY(I)=Density(I)
       SPDAY(I)=Spheat(I)
       TKDAY(I)=Thconduct(I) 

C       mass*specific heat product (per unit area)
         RCSP = DENDAY(I)*SPDAY(I)
         WC(I)=RCSP*(DEPP(I+1)-DEPP(I-1))/2.
         SOK =TKDAY(I)   
   51    C(I)=SOK/(DEPP(I+1)-DEPP(I))
      ENDIF
  100 CONTINUE
c    End of initialization of variables

      TAIR=TAB('TAR',TIME)  
      ZENR=TAB('ZEN',TIME)  
      SOLR=TAB('SOL',TIME)
      CLOUD=TAB('CLD',TIME) 

c    if((moy.eq.1).and.(time.lt.60))then
c     continue
c    else
c     if(snowhr(moy*24-24+hour-1).gt.0)then
c      ptwet=100.
c      SABNEW=0.1
c     endif
c    endif


C    Modification by M. Kearney for effect of cloud cover on direct solar radiation, using the
C    Angstrom formula (formula 5.33 on P. 177 of "Climate Data and Resources" by Edward Linacre 1992
      IF (CLOUD .GT. 0.) THEN
         SOLR = SOLR*(0.36+0.64*(1.-(CLOUD/100)))
      ENDIF      
C    SOLAR RADIATION ON LEVEL GROUND. SURFACE ABSORPTIVITY, SAB, NOW CHANGING EACH MONTH/TIME INTERVAL, SABNEW
      QSOLAR=SABNEW*SOLR*((100.- SHAYD)/100.)  

C     SOLAR MODIFICATION FOR SLOPES 
      IF(SLOPE .GT. 0) THEN         
C       SLOPING SURFACE 
        CZ=COS(PI*ZENR/180.)  
        ZSLR=TAB('ZSL',TIME)  
        CZSL=COS(PI*ZSLR/180.)
        IF(ZENR .LT. 90.) THEN
C        CORRECT FOR SOLAR ON LEVEL GROUND FOR SOLAR ON A SLOPE                
          QSOLAR=(QSOLAR/CZ)*CZSL   
         ELSE
C        ZENITH ANGLE 90 DEGREES OR GREATER. NO DIRECT SOLAR.
        ENDIF
       ELSE
C      NO SLOPE. QSOLAR ALREADY CALCULATED, NO ADJUSTMENT FOR SLOPE NEEDED
      ENDIF

C     SWINBANK FORMULA FOR SKY TEMPERATURE
c      TSKY=0.0552*(TAIR+273.)**1.5-273. 
C     PERCENT CLOUD COVER 2 DEG. LESS THAN TAIR 

C     CONVERTING PERCENT CLOUDS TO FRACTION OF SKY CLEAR
      CLR=1.- (CLOUD/100.)

C     CLEAR SKY RADIANT TEMPERATURE   
c      ARAD=(TSKY+273.)**4 replacted with formula from Campbell, converted to cal/min
c      ARAD=(0.0000092*(TAIR+273.16)**2)*0.0000000567*(TAIR+273.16)**4*60
c     &./(4.185*10000.)

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
c    Campbell and Norman eq. 10.10 to get emissivity of sky
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
      QCOND=C(1)*(T(2)-T(1))
      VELR=TAB('VEL',TIME)
C    COMPUTE VELOCITY AND TEMPERATURE PROFILES
      IF((ZH1.LE.0.000).AND.(ZH2.LE.0.000))THEN
C      NO SEGMENTED VELOCITY PROFILE (SINGLE LOG PROFILE) 
        CALL MICRO(HGTP,RUFP,TAIR,T(1),VELR,QCONV,AMOL,NAIR,ZZ,VV,T,  
     &  ZENR)
       ELSE
C      SEGMENTED VELOCITY PROFILE (VEGETATION OR OTHER OBJECTS MODIFYING VELOCITY PROFILE)
        CALL MICROSEGMT(HGTP,RUFP,TAIR,T(1),VELR,QCONV,AMOL,NAIR,ZZ,  
     &  VV,T,ZENR)
      ENDIF

C    SOIL TRANSIENTS; FIRST THE NODE AT THE SURFACE. 
C    SIGN CONVENTION IN ALL EQUATIONS: POSITIVE TERMS = HEAT INPUT TO THE NODE, NEG. TERMS = HEAT LOSS FROM NODE
C    THIS SURFACE NODE EQUATION IS A HEAT BALANCE ON THE SOIL SURFACE NODE: 
C    QIN = QOUT + QSTORED  REARRANGED TO GET THE RATE OF CHANGE OF TEMPERATURE TERM IN QSTORED, m*c*dT/dt 
      IF(PTWET.EQ.0.00)THEN 
C      DRY SURFACE
        DTDT(1)=(QSOLAR+QRAD+QCOND+QCONV)/WC(1)
        QEVAP=0.
       ELSE
C      SNOW or WET SURFACE
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
        IF(T(1).LT.-60.)THEN
          T(1) = -60.
        ENDIF
C      CHECKING FOR DIVIDE BY ZERO
        IF(T(1).EQ.TAIR)THEN
          T(1)= T(1)+0.1
        ENDIF
C      CHECK FOR OUTSIZE T(1)
        TTEST = ABS(T(1))
        IF(TTEST.GT. 100)THEN
          T(1) = TAIR + 1.0
          HC = ABS(QCONV/(T(1)-TAIR))
         ELSE
          HC = 0.01
        ENDIF
        HD = (HC/(CP*DENAIR))*(0.71/0.60)**0.666
        CALL EVAP(T(1),TAIR,RH,HD,QEVAP)
        DTDT(1)=(QSOLAR+QRAD+QCOND+QCONV-QEVAP)/WC(1) 
      ENDIF  

C    SETTING UP THE DEEP SOIL TEMPERATURE, TDS, FOR SOIL TRANSIENTS.   
C    (N=MM+1); N = # OF SOIL NODES SET UP IN 'DEP' ARRAY IN INPUT DATA 
C      # OF SOIL NODES IS ASSUMED = 10, UNLESS SPECIFIED OTHERWISE BY A 
C      'NON' PARAMETER LINE PLUS DATA LINE IN DATA INPUT. MAX. # NODES = 18.
C      ALL DEFAULT PARAMETER VALUES ARE IN BLKDATA IN PAR ARRAY.  THEIR NAMES   
C      ARE IN SYMBOL ARRAY. 
      T(N)=TAB('TDS',TIME)                         
      TDS=T(N)  

C     CHECK FOR MAX # OF NODES  
      IF (N .GT. 18) THEN   
          WRITE(6,*)'MAX # OF NODES (18) EXCEEDED.' 
          GO TO 200 
         ELSE   
          CONTINUE  
      ENDIF 

C     COMPUTE DERIVATIVES OF THE REST OF THE SOIL NODES 
      DO 10 I=2,N  
   10 DTDT(I)=(C(I-1)*(T(I-1)-T(I))+C(I)*(T(I+1)-T(I)))/WC(I)   




C**** SET UP THE OUTPUT 
      OUT(1)=TIME   
      OUT(2)=TAIR   
      OUT(3)=TSKY   
      OUT(4)=T(1)   
      OUT(5)=VELR   
      OUT(6)=SOLR   
      OUT(7)=CLOUD  
      OUT(8)=QSOLAR     
      OUT(9)=QRAD   
      OUT(10)=QCOND     
      OUT(11)=QCONV     
      OUT(12)=AMOL  
      OUT(13)=H 
      OUT(53)=TIME/60.  
      OUT(55)=TIME*60.
      OUT(101)=QEVAP  
      I1=14 
      I2=32 
      DO 20 I=I1,I2     
   20 OUT(I)=T(I-12)    
      L=I1+N   
      OUT(L)=TDS
      IF(NAIR.LE.0) RETURN  
C    INSERTING AIR TEMPERATURES FROM THE GROUND UP, BUT NOT INCLUDING THE   
C     SURFACE  (T(21-30))   
      I1=33 
      I2=42 
      DO 22 I=I1,I2     
   22 OUT(I)=T(I-12)    
C       INSERTING VELOCITIES FROM THE GROUND UP, BUT NOT INCLUDING THE  
C     SURFACE (V(1-10)) 
      I1=43 
      I2=52 
      DO 24 I=I1,I2     
   24 OUT(I)=VV(I-42)   

C     SETTING UP SI UNITS OUTPUT NOW DONE IN OSUB  
C      METONY = 1
C      IF (METONY .GT. 0) THEN           
C      SIOUT(1) = TIME   
C      AIR TEMPERATURE AT ANIMAL HEIGHT (1ST NODE BELOW REFERENCE HEIGHT, 200 CM) 
C       SIOUT(2) = OUT(34)   
C      RELATIVE HUMIDITY AT ANIMAL HEIGHT
C        SIOUT(3) = TAB('REL',TIME) 
C      VELOCITY AT ANIMAL HEIGHT (OUT(43 - 52) ARE NODES DOWN FROM REFERENCE HEIGHT AIR NODE 
C       THE 3 DEFAULT NODE HEIGHTS DEFINED IN SUB. IOMET2. OUT(43) IS REFERENCE HEIGHT.
C       SIOUT(4) = OUT(44)/(100.*60.)
C      SUBSTRATE TEMPERATURE 
C        SIOUT(5) = T(1)
C       1ST SOIL NODE BELOW THE SURFACE (2.5 CM CURRENTLY)  
C        SIOUT(6) = T(2)
C       DEEP SOIL NODE  
C        SIOUT(7) = TDS
C      SOLAR & ZENITH OUTPUT TO METOUT FOR A HORIZONTAL SURFACE 
C      FOR ANIMAL CALCULATIONS, ALTHOUGH SLOPE CORRECTIONS DONE
C      IN DSUB FOR THE GROUND
C        SIOUT(8) = ZENR
C      CONVERTING FROM CAL/CM2-MIN TO W/M2
C        SIOUT(9) = SOLR * 4.185 * 10000. / 60. 
C       BLACK BODY EQUIVALENT SKY INFRARED RADIATION (W/M2)  
C        SIOUT(10) = ((QRADSK+QRADVG)/SIGP)**0.25 - 273.15
C      ENDIF 

  200 RETURN
      END   
