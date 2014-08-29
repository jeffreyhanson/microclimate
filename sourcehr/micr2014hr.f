c      subroutine micr2011b(julnum1,julstnd1,julday1,RUF1,SLE1,ERR2,
c     &Usrhyt1,DEP1,numtyps1,numint1,Thconds1,Densitys1,Spheats1,
c     &Intrvls1,Nodes1,Z011,Z021,ZH11,ZH21,idayst1,ida1,maxshades1,
c     &minshades1,hemis1,alat1,aminut1,along1,almint1,alref1,slope1,
c     &azmuth1,altt1,cmh2o1)

c      subroutine micr2011b(julnum1,julstnd1,julday1,RUF1,SLE1,ERR2,
c     &Usrhyt1,DEP1,numtyps1,numint1,Thconds1,Densitys1,Spheats1,
c     &Intrvls1,Nodes1,Z011,Z021,ZH11,ZH21,idayst1,ida1,maxshades1,
c     &minshades1,hemis1,alat1,aminut1,along1,almint1,alref1,slope1,
c     &azmuth1,altt1,cmh2o1,timaxs1,timins1,RHMAXX1,RHMINN1,CCMAXX1,
c     &CCMINN1,WNMAXX1,WNMINN1,TMAXX1,TMINN1,SNOW1,REFLS1,PCTWET1)

c      subroutine micr2011b(julnum1,julstnd1,julday1,RUF1,SLE1,ERR2,
c     &Usrhyt1,DEP1,numtyps1,numint1,Thconds1,Densitys1,Spheats1,
c     &Intrvls1,Nodes1,Z011,Z021,ZH11,ZH21,idayst1,ida1)

c    latest
c      subroutine micr2011b(julnum1,julday1,RUF1,SLES1,ERR2,
c     &Usrhyt1,DEP1,numtyps1,numint1,Thconds1,Densitys1,Spheats1,
c     &Intrvls1,maxshades1,minshades1,Nodes1,Z011,Z021,ZH11,ZH21,idayst1,
c     &ida1,hemis1,alat1,aminut1,along1,almint1,alref1,slope1,
c     &azmuth1,altt1,cmh2o1,timaxs1,timins1,RHMAXX1,RHMINN1,CCMAXX1,
c     &CCMINN1,WNMAXX1,WNMINN1,TMAXX1,TMINN1,SNOW1,REFLS1,PCTWET1,
c     &soilinit1,microdaily1,tannul1,hori1,tai1,ec1,viewf1,metout1,soil1,
c     &shadmet1,shadsoil1)

      subroutine micr2014hr(microinput1,julday1,SLES1,DEP1,Intrvls1,
     &maxshades1,minshades1,Nodes1,timaxs1
     &,timins1,RHMAXX1,RHMINN1,CCMAXX1,CCMINN1,WNMAXX1,WNMINN1,TMAXX1
     &,TMINN1,SNOW1,REFLS1,PCTWET1,soilinit1,hori1,tai1,soilprop1,
     &moists1,rain1,tannulrun1,TAIRhr1,RHhr1,WNhr1,CLDhr1,IRhr1,
     &SOLRhr1,RAINhr1,metout1,soil1,shadmet1,shadsoil1)

c      subroutine micr2011b(testing)

c      PROGRAM Micr2011b

C    COPYRIGHT 2000- 2011  WARREN P. PORTER,  ALL RIGHTS RESERVED

C    This is the main I/O driver program for the PRODUCTION version
C     of the microclimate program used to drive aniaml heat and mass 
C     balance programs.  Labels for input data have been added.

c    Version 9 March 2011 deletes single substrate reflectivity input near the top of Micro.dat - done
c    User can now select depths for simulations & deepest depth. Mean annual temperature now at deepest node. - done
c    Allows user to set variable soil properties temporally & spatially via a new input file, soil_varybl.dat read by MicrIO.for

c    Version 6/12/10 Stability problem in DSUB with T(1) now corrected with a check for too large a number.
c    Version 31 January 2010. Program can now handle up to 52 simulation days.

c    Version 7 August 2007.  Micromet velocity profiles returned to a logarithmic profile, instead of the disjointed profile for bushes
c    originally derived for Galapagos work. Local Rel. Hum. determined by the microclimate program using
c     2m Ta & Rel. Hum. and the local Ta at animal height.  The local Ta is between the ground surface temperature and the 2m Tair. Thus
c    changes in surface wetness that cause surface cooling automatically adjust the local relative humidity via the changes in local Tair.

C    Version 17 October 2006  Error check added for shades entry data to make sure Maxshades always at least 1% greater than minshades.
C    Version 11 October 2006
c    Changed input absorptivities = f(day of year) to reflectivities (albedos) = f(day of year)
c    to make input consistent with earlier single reflectivity value and to easily transfer albedo
c    data into Microv.dat.

c     Version 9 September 2004
c    Additions this version: 
c     1) variable Julian days (up to 52) as input to be run.
c     Additions 2003 version: 
c     2) reading in percent bare ground that 
c     is shaded, so that consequences of land use change can be assessed
c     2) output equivalent black body sky temperature instead of infrared
c     flux from the sky in file Metout.  This will make it easier to 
c     manually modify the file when using it as imput for sensitivity
c     analyses with the ectotherm or endotherm models. Metout is now the
c     standard meteorological input for both programs.  File soil is 
c    belowground temperatures. If shade is 100% then file shadmet and 
c    shadsoil are written.  Ecto2003 uses all 4 of the files and does
c    linear interpolation between 0% shade and 100% shade for aboveground
c    and belowground microenvironments.

c     Version 13 October 2004

c    Micr2005 allows monthly shade vegetation changes and automatically 
c    runs BOTH a minimum shade year and a maximum shade year and writes them 
c    to metout & soil, then shadmet & shadsoil respectively.  The minimum and 
c    maximum values of shade can be determined from the vegetation present in 
c    a given 'pixel' of landscape.  Values can range from 0 - 100% and vary by 
c    month as a function of vegetation type (deciduous or evergreen) for each 
c    pixel.  This was developed to deal with understory animals like toads and 
c    Bachman's sparrow where subtle differences in the amount of available shade 
c    are important for habitat selection.
C    NOTE TO USER: The data in solar.dat does NOT change the water vapor concentration
c    in the atmospheric profile (4th column in the 34d major block of data).  
c    It you want to change it, you would have to measure
c    it first, then put it in solar.dat each time.  It is currently set to 1 cm,
c    an average value of precipitable water in the air column.

c     Data input by the user is via file 'Microv.dat'.  
c     Data input for SOLRAD, the solar clear sky calculations program
c      embedded in this program, is the data file,'Solar.dat'. 
c     Both data files are required by this micrometeorology program.

C    If time dependent changes are needed as a function of month/time interval,
c    they are implimented at the top of Osub.for  12/6/05.
c    Note that the surface absorptivity changes are also in Solrad.for, which 
c    runs through the whole year BEFORE the numerical integrator, SFODE, is called to 
c    do the annual calculations.

C    Micr2004 calls iomet1, iomet2, and solrad. Solrad calls iosolr, gamma
C      (if needed), sinec, and vsine.  The user supplied input file,
C     'Microv.dat' is thus read by iomet1, iomet2, and iosolr. They 
C     write 'Dataky.dat' which is the data file the micromet program
C     actually uses.  Then main calls rdctrl, rdtabl, pttabl which read
C     'Dataky.dat' and sfode, the numerical integrator, which uses dsub,
C     which contains the actual energy balance equations.  Output goes to
C     osub.
C
C    IOMET1 is for time independent parameters.  
C    IOMET2 is part 2 of input of time independent parameters.
C    SOLRAD computes clear sky solar radiation for anywhere
C    IOSOLR is for solrad input and min, max values & 
c       for input of time dependent variables, air temperature,
C      relative humidity, cloud cover, and wind speed 
c     GAMMA computes scattered radiation in the ultraviolet to high precision.
c       GAMMA calls DCHXY and DEXPI during its calculations.
C    SINEC computes time dependent values of air temperature
C    VSINE computes time dependent values of relative humidity,
C      cloud cover & wind speed.
c     RDCTRL controls reading of variables rearranged for input to the
c       meteorology program
c     RDTABL reads table data of time dependent input variables, e.g. air
c       temperature, solar radiation, etc.
c     PTTABL prints out time dependent tables
c     SFODE is the Gear Predictor-Corrector numerical integration program
c       that solves the simultaneous heat balance equations for temperatures.
c     DSUB is the subroutine containing the heat balance equations in 
c       derivative form, which is called by SFODE.
c     OSUB outputs the microclimate calculations.
      IMPLICIT NONE

      REAL ALIZ,ALPLIZ,ARLIZ,BLIZ,C,DENSITYS,DEP,DTAU
      REAL EPSLIZ,ERR1,H,HLIZ,KSOYL,OUT
      REAL PAR,PTWET,SABNEW,SPHEATS
      Real T,TD,THCONDS,TI,TIME,TIMEF,TMAX,TMIN,WLIZ,WORK 
      Real shayd,altt,MAXSHD,MAXSHADES,MINSHADES,WC,JULDAY,viewf
      real itair,icld,iwind,irelhum,sles,rainfall
      REAL DEPS,TDSS,TINS,TARS,RELS,CLDS,VELS,SOLS,ZENS,ZSLS

      INTEGER I,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I20
      Integer ENDMON,IDAY,IDMAIN,IFINAL,ILOCT,IOUT
      INTEGER INTRVLS,IPCH,IPRINT,cnt
      Integer ISHADE,ITEST,J,JULNUM,K   
      Integer M,MM,MONLY,MOY,N,ND,NDEP,NDMAX,NDUM1,NKHT,NLIZ
      Integer NOCON,NODES,NOPRNT,NOSUM,NOTRAN
      INTEGER NOUT,NPOS,NUMINT,NUMRUN,NUMTYPS,writecsv
      
      CHARACTER*80 RWORK,LABL1,LABL2,LABL3
      CHARACTER*3 IBLK,INAME,INPUT,MA,SYMBOL    
      CHARACTER*6 AWORK(1000)   
      CHARACTER*4 TTLABL
      CHARACTER*1 ANS20,solout,SINE,ANS14,SNO,SNSLOP
      CHARACTER*12 FNAMEM,FNAME

      DIMENSION BLIZ(8) 
      DIMENSION TTLABL(20)
      DIMENSION MAXSHADES(7300),MINSHADES(7300),JULDAY(7300)
      DIMENSION Nodes(10,7300)
      DIMENSION Intrvls(7300),KSOYL(10),microinput1(30)
      DIMENSION soilprop(10,6),moists(10,7300),moists1(10,7300),
     &soilprop1(10,6),moist(10)
      DIMENSION DEPS(13),TDSS(7300),TINS(10,7300),TARS(25*7300),
     &RELS(25*7300),CLDS(25*7300),VELS(25*7300),SOLS(25*7300),
     &ZENS(25*7300),ZSLS(25*7300)
        
      COMMON/WORK/WORK(1720)
      COMMON/LABEL/LABL1,LABL2,LABL3,FNAME,SINE,ANS14,SNSLOP
      COMMON/NONSCR/MM,N,TIME,TIMEF,DTAU,ERR1,H,NOUT,NDUM1,IPRINT   
      COMMON/ARRAY/T(30),WC(20),C(20),DEP(30),IOUT(100),        
     & OUT(101),ITEST(23)   
      COMMON/CARRAY/INAME(20),SYMBOL(23)
      COMMON/TABLE/ILOCT(21),TI(200),TD(200)    
      COMMON/PLIZ/ ALIZ,WLIZ,ARLIZ,HLIZ,ALPLIZ,EPSLIZ,TMAX,TMIN 
      COMMON/OPSHUN/MONLY,NOPRNT,NOTRAN,NOCON,IPCH,ISHADE,NKHT, 
     & NPOS,NLIZ,NOSUM 
c      COMMON/WSTEDI/ASVLIZ(6),SHADE(4)  
      COMMON/PAR/PAR(18)
      COMMON/WMAIN/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12 
      COMMON/NDAY/ND
      common/solropt/solout
      COMMON/GROUND/SHAYD,ALTT,MAXSHD,SABNEW,PTWET,rainfall
      COMMON/GRND2/SNO
      COMMON/SHADES/MAXSHADES,MINSHADES
      COMMON/WICHDAY/NUMRUN
      COMMON/DAYJUL/JULDAY,JULNUM,MOY
c    Variable soil properties data from Iomet1
      COMMON/SOYVAR1/Numtyps,Numint,Intrvls
      COMMON/SOYVAR2/Thconds,Densitys,Spheats,Nodes,KSOYL 
      COMMON/DAILY/microdaily
      common/deep/tannul2
      common/atten/tai,ec
      common/VIEWFACT/VIEWF
      common/init/itair,icld,iwind,irelhum,iday
      COMMON/dataky/DEPS,TDSS,TINS,TARS,RELS,CLDS,VELS,SOLS,ZENS,CNT,
     &ZSLS 


c    adding in for NicheMapR
      REAL RUF,SLE,ERR,Usrhyt,Z01,Z02,ZH1,ZH2
      REAL ALAT,ALMINT,ALONC,ALONG,ALREF
      REAL AMINUT,AMULT,AZMUTH
      REAL CCMAXX,CCMINN,CMH2O,DAY
      REAL HEMIS,snowtemp,snowdens,snowmelt
      REAL PRESS,PUNSH,REFL,RHMAXX,RHMINN
      REAL SLOPE,REFLS
      REAL TANNUL,TIMCOR,TIMAXS,TIMINS,TMINN,TMAXX
      REAL TSRHR,TSNHR,WNMAXX,WNMINN,PCTWET,SNOW
      REAL metout,shadmet,soil,shadsoil,soilprop,moists
      REAL tannul2,hori,azi,tai,ec,moist,RAIN,snowhr
      REAL tannulrun,minutes

      INTEGER IALT,IEND,IEP,IPINT,ISTART
      INTEGER IUV,NOSCAT,IDA,IDAYST,julstnd
      INTEGER microdaily,MOYF,MOYS

      double precision julday1,DEP1,Intrvls1,Nodes1,maxshades1,
     &minshades1,timaxs1,timins1,RHMAXX1,RHMINN1,CCMAXX1,
     &CCMINN1,WNMAXX1,WNMINN1,TMAXX1,TMINN1,SNOW1,REFLS1,PCTWET1,
     &metout1,shadmet1,soil1,shadsoil1,soilinit1,hori1,tai1,
     &microinput1,sles1,moists1,soilprop1,rain1,tannulrun1,TAIRhr1,
     &RHhr1,WNhr1,CLDhr1,IRhr1,SOLRhr1,RAINhr1


      DIMENSION CCMAXX(7300),CCMINN(7300)
      DIMENSION RHMAXX(7300),RHMINN(7300),TIMINS(4),TIMAXS(4)
      DIMENSION TMINN(7300),TMAXX(7300),WNMAXX(7300),
     &    WNMINN(7300)
      DIMENSION SNOW(7300),REFLS(7300),PCTWET(7300),tai1(111),tai(111)
      DIMENSION TAIRhr1(24*7300),RHhr1(24*7300),WNhr1(24*7300),
     & CLDhr1(24*7300),IRhr1(24*7300),SOLRhr1(24*7300),RAINhr1(24*7300)

      DIMENSION Nodes1(10,7300),Intrvls1(7300),Dep1(10),minutes(25)
      DIMENSION CCMAXX1(7300),CCMINN1(7300),JULDAY1(7300)
      DIMENSION RHMAXX1(7300),RHMINN1(7300),TIMINS1(4),TIMAXS1(4)
      DIMENSION TMINN1(7300),TMAXX1(7300),WNMAXX1(7300),
     &    WNMINN1(7300),RAIN1(7300),RAIN(7300),tannulrun1(7300)
      DIMENSION MAXSHADES1(7300),MINSHADES1(7300)
      DIMENSION SNOW1(7300),REFLS1(7300),PCTWET1(7300),
     &    soilinit1(10),hori1(24),SLES1(7300),SLES(7300)

      DIMENSION DAY(7300),SNOWHR(25*7300),tannulrun(7300)
      DIMENSION julstnd(2)

      DIMENSION METOUT(24*7300,18),SHADMET(24*7300,18)
      DIMENSION SOIL(24*7300,12),SHADSOIL(24*7300,12)

      DIMENSION METOUT1(24*7300,18),SHADMET1(24*7300,18)
      DIMENSION SOIL1(24*7300,12),SHADSOIL1(24*7300,12)
      DIMENSION hori(24),azi(24)


c    double precision, allocatable, intent (out) :: METOUT1(:,:),
c     &SHADMET1(:,:),SOIL1(:,:),SHADSOIL1(:,:)          

      COMMON/WIOMT2/RUF 
      Common/Hyte/Usrhyt
      COMMON/DMYCRO/Z01,Z02,ZH1,ZH2
      COMMON/NICHEMAPRIO/SLE,ERR,SLES,soilprop,moists,moist
      COMMON/ENDS/JULSTND
      COMMON/WIOCONS/IPINT,NOSCAT,IUV,PUNSH,IALT,ALAT,AMULT,PRESS,
     * CMH2O,REFL,ALONC,IDAYST,IDA,TIMCOR,AZMUTH,SLOPE,TSNHR,TSRHR,IEP,
     * ISTART,IEND,HEMIS
      COMMON/DAYS/DAY,TMINN,TMAXX,TANNUL
      COMMON/DAYSS/CCMINN,CCMAXX,RHMINN,RHMAXX,WNMINN,WNMAXX,
     &  TIMINS,TIMAXS,TANNULRUN
      COMMON/WINTER/SNOW
      COMMON/WINTER2/REFLS,PCTWET
      COMMON/LATLONGS/AMINUT,ALONG,ALMINT,ALREF
      COMMON/RAINY/RAIN
      COMMON/SNOWPRED/SNOWHR,snowtemp,snowdens,snowmelt
      COMMON/ROUTPUT/METOUT,SHADMET,SOIL,SHADSOIL  
      common/horizon/hori,azi
      
      EQUIVALENCE(ALIZ,BLIZ(1))     
      DATA IBLK/'   '/      
      DATA IFINAL/1/
C    INITIALIZING MONTH OF YEAR COUNTER
      DATA MOY/1/
c    INITIALIZING DATAKY COUNTER
      DATA CNT/1/
      DATA MINUTES/0,60,120,180,240,300,360,420,480,540,600,660,720,780
     &    ,840,900,960,1020,1080,1140,1200,1260,1320,1380,1440/
C     SIG=.8126E-10     1   
C     RCS=.5            2   
C     STC=.05   
C     SSA=.7            4   
C     HGT=40.           5   
C     RUF=.05           6   
C     BEG=0.            7   
C     MON=0             8   
C     PRT=60.           9   
C     ERR=1.           10   
C     END=0            11   
C     SLE=1.           12   
C     DAS=0            13   
C     NON=10.          14   
C     SUN=1.           15   
C     PLT=0            16   
C     FIN=0            17   
C     STP=0            18   
C     TIME DEPENDENT TABLES NEEDED  
C     SOL  HOURLY SOLAR RADIATION   
C     TAR  TEMPERATURE OF AIR AT REFERENCE HEIGHT   
C     TDS  TEMPERATURE OF DEEP SOIL 
C     VEL  AIR VELOCITY AT REFERENCE HEIGHT     
C     THE FOLLOWING ARRAYS MUST ALSO BE INCLUDED
C     TIN  INITIAL TEMPERATURES MUST BE SAME NUMBER AS NUMBER OF NODES  
C     DEP  DEPTHS IN THE SOIL AND HEIGHTS IN THE AIR
C     DROP OUT OPTION DEP=0,2.5,5,10,15,20,30,40,50,60  
C     IOT  THE VARIABLES TO BE PRINTED ACCORDING TO KEY IN MANUAL   
C     DROP OUT OPTION.  IOT=1,2,3,4,5,6 
C      LIZARD PARAMETERS
C     ALIZ=0.   
C     WLIZ=50   
C     ARLIZ=150.
C     HLIZ=1.5  
C     ALPLIZ=.7 
C     EPSLIZ=1  
C     TMAX=40.  
C     TMIN=18.  
      writecsv=1
      M=0
c    Unpacking user input from R
      julnum=int(microinput1(1))
c    do 901 i=1,2
c    julstnd(i)=julstnd1(i)
c901    continue
      RUF=real(microinput1(2),4)
      SLES=real(SLES1,4)
      moists=real(moists1,4)
      soilprop=real(soilprop1,4)
      SLE=0.9
      ERR=real(microinput1(3),4)
      Usrhyt=real(microinput1(4),4)
      rain=real(rain1,4)
      tannulrun=real(tannulrun1,4)
      do 9191 i=1,25*7300
       snowhr(i)=0.
9191  continue

      do 919 i=1,24
      hori(i)=real(hori1(i),4)
      azi(i)=i*15
919   continue
      do 918 i=1,111
      tai(i)=real(tai1(i),4)
918   continue

      do 902 i=1,10
      DEP(i)=real(DEP1(i),4)
902   continue
      numtyps=int(microinput1(5))
      numint=int(microinput1(6))
      microdaily=int(microinput1(23))
      tannul2=real(microinput1(24),4)
      viewf=real(microinput1(26),4)
      snowtemp=real(microinput1(27),4)
      snowdens=real(microinput1(28),4)
      snowmelt=real(microinput1(29),4)

c    WRITE(I2,*)i,' ',j,' ',Thconds(i,j),' ',Thconds1(i,j)

      do 904 i=1,numint
      Intrvls(i)=int(Intrvls1(i))
904   continue
      do 905 i=1,numtyps
       do 906 j=1,numint
      Nodes(i,j)=int(Nodes1(i,j))
906    continue
905   continue
      Z01=real(microinput1(7),4)
      Z02=real(microinput1(8),4)
      ZH1=real(microinput1(9),4)
      ZH2=real(microinput1(10),4)


      IDAYST=int(microinput1(11))
      IDA=int(microinput1(12))
      EC=real(microinput1(25),4)
      HEMIS=real(microinput1(13),4)
      ALAT=real(microinput1(14),4)
      AMINUT=real(microinput1(15),4)
      ALONG=real(microinput1(16),4)
      ALMINT=real(microinput1(17),4)
      ALREF=real(microinput1(18),4)
      SLOPE=real(microinput1(19),4)
      AZMUTH=real(microinput1(20),4)
      ALTT=real(microinput1(21),4)
      CMH2O=real(microinput1(22),4)
      do 907 i=1,4
      timaxs(i)=real(timaxs1(i),4)
      timins(i)=real(timins1(i),4)
907   continue
      do 908 i=1,IDA
      julday(i)=real(julday1(i),4)
      MAXSHADES(i)=real(MAXSHADES1(i),4)
      MINSHADES(i)=real(MINSHADES1(i),4)
      RHMAXX(i)=real(RHMAXX1(i),4)
      RHMINN(i)=real(RHMINN1(i),4)
      CCMAXX(i)=real(CCMAXX1(i),4)
      CCMINN(i)=real(CCMINN1(i),4)
      WNMAXX(i)=real(WNMAXX1(i),4)
      WNMINN(i)=real(WNMINN1(i),4)
      TMAXX(i)=real(TMAXX1(i),4)
      TMINN(i)=real(TMINN1(i),4)
      SNOW(i)=real(SNOW1(i),4)
      REFLS(i)=real(REFLS1(i),4)
      PCTWET(i)=real(PCTWET1(i),4)
908   continue



C    ****     COMPUTER READ - WRITE SETUP *************

      I1=5
      I2=7
      I3=10
      I4=11
      I5=4
      I6=3
      I7=2
      I8=1
      I9=12
      I10=13
      I11=14
      I12=15
      I20=20

c    setting solrad output option(y/n) for file Solrout
      solout='N'

C    USE UNIT 5 FOR INPUT - ANY UNIT BUT 6 (CONSOLE) WILL DO.  
c      OPEN (I1,FILE = 'Microv.dat')

C    USE UNIT 7 FOR OUTPUT - ANY UNITS BUT 6 AND 5 ARE OK. 
c      OPEN (I2,FILE = 'output')

C     USE UNIT I3 for above ground micromet output when % shade < 100.
C    ALL 4 OF THE FOLLOWING FILES MUST BE OPENED BEFORE IOMET1 IS CALLED, SINCE
C    BOTH IOMET1 & IOMET2 & IOSOLR WRITE TO ALL FILES HEADER DATA
      if(writecsv.eq.1)then
      OPEN (I3, FILE = 'metout.csv') 
      write(I3,111) "JULDAY",",","TIME",",","TALOC",",","TAREF",",","RHL
     &OC",",","RH",",","VLOC",",","VREF",",","TS",",","T2",",","TDEEP"
     &,",","ZEN",",","SOLR",",","TSKYC",",","DEW",",","FROST",",","SNOWF
     &ALL",",","SNOWDEP"

C     USE UNIT 13 FOR HOUR, SOIL DEPTH & SOIL TEMPERATURE OUTPUT
      OPEN (I10, FILE = 'soil.csv')
      write(I10,112) "JULDAY",",","TIME",",","DEP1",",","DEP2",","
     &,"DEP3",",","DEP4",",","DEP5",",","DEP6",",","DEP7",",","DEP8",","
     &,"DEP9",",","DEP10" 
      OPEN (I1, FILE = 'soil_properties.csv')
      write(I1,113) "JULDAY",",","TIME",",","DEN1",",","DEN2",","
     &,"DEN3",",","DEN4",",","SPH1",",","SPH2",",","SPH3",",","SPH4",","
     &,"COND1",",","COND2",",","COND3",",","COND4"  

C     USE UNIT I13 FOR above ground micromet OUTPUT when % shade = 100.
      OPEN (I12, FILE = 'shadmet.csv') 
      write(I12,111)"JULDAY",",","TIME",",","TALOC",",","TAREF",",","RHL
     &OC",",","RH",",","VLOC",",","VREF",",","TS",",","T2",",","TDEEP"
     &,",","ZEN",",","SOLR",",","TSKYC",",","DEW",",","FROST",",","SNOWF
     &ALL",",","SNOWDEP"
        
C     USE UNIT 14 FOR HOUR, SOIL DEPTH & SOIL TEMPERATURE OUTPUT when % shade = 100.
      OPEN (I11, FILE = 'shadsoil.csv') 
      write(I11,112) "JULDAY",",","TIME",",","DEP1",",","DEP2",","
     &,"DEP3",",","DEP4",",","DEP5",",","DEP6",",","DEP7",",","DEP8"
     &,",","DEP9",",","DEP10" 
      endif

111   format(A8,A1,A8,A1,A8,A1,A8,A1,A8,A1,A8,A1,A8,A1,A8,A1,A8,A1,A8,A1
     &,A8,A1,A8,A1,A8,A1,A8,A1,A8,A1,A8,A1,A8,A1,A8,A1)
112   format(A7,A1,A7,A1,A7,A1,A7,A1,A7,A1,A7,A1,A7,A1,A7,A1,A7,A1,A7,A1
     &,A7,A1,A7,A1)
113   format(A7,A1,A7,A1,A7,A1,A7,A1,A7,A1,A7,A1,A7,A1,A7,A1,A7,A1,A7,A1
     &,A7,A1,A7,A1)


C    USE UNIT 1 FOR INPUT - ANY UNIT BUT 0 (CONSOLE) WILL DO.  
c      OPEN (I8, FILE='solar.dat')   

C    USE UNIT 12 FOR SOLAR OUTPUT - FILE 6 WILL BE THE CONSOLE
      If ((solout .eq. 'Y').or.(solout .eq. 'Y'))then    
        OPEN (I9, FILE = 'solrout')
      Endif
      
C    ******* END MICRO READ - WRITE SETUP *************
      
c     Defining maximum number of iteration days for the integrator 
c     (REPEATS OF A DAY TO GET a STEADY PERIODIC OF THE DAY).
      NDMAX=4      
      
C    QUERY FOR KEYBOARD INPUT
1030  CONTINUE
C    WRITE(6,*)'Do you want to input data for MICROMET from the ',
C     &  'keyboard (Y/N)?'
C    READ(6,110)ANS20
      ANS20 = 'Y'
      IF ((ANS20 .EQ. 'Y') .OR. (ANS20 .EQ. 'y')) THEN
C      OPEN OUTPUT FILE TO DATAKY.DAT FOR KEYBOARD INPUT
C      CREATED BY SUBROUTINES IOMET, IOTCON, IOCONS
c      OPEN (I4,FILE = 'dataky.dat')

C      *** Start read of user input (from 'Microv.dat') 
C       SET up first part of MICROMET Program INPUT AS 'DATAKY.DAT'
C      SET UP PARAMETER VALUES FOR MICROMET

        CALL Iomet1
c    burn in for daily sims
      if(microdaily.eq.1)then
          if(moy.eq.1)then
      ND=3
      endif
      endif

C      SET UP SOIL NODES, DEPTHS, AIR NODES, HEIGHTS FOR MICROMET
        CALL IOMET2

C      SET UP TIME DEPENDENT VARIABLES
c      WRITE(0,*)'CALLING SOLRAD'.  SOLRAD CALLS Iosolr
        CALL SOLRAD 
C       **** End user input (from 'Microv.dat') *****

c       Checking on maximum iteration days
        if (ND .gt. NDMAX) then
          ND=NDMAX
        endif

c      CLOSE (I4,STATUS = 'KEEP')
c        WRITE(0,*)'End of input calculations.  Micromet starting.'
C      OPEN READ FILE FOR IMPUT TO MICROMET
c      OPEN (I4,FILE = 'dataky.dat')
       ELSE
        IF ((ANS20 .EQ. 'N') .OR. (ANS20 .EQ. 'n')) THEN
C        FILE INPUT IS USER SUPPLIED FILE
102       WRITE(*,*)'Please enter data file name.'
          WRITE(*,*)' '
          READ(*,420,ERR = 102)FNAMEM
          WRITE(*,421)FNAMEM
420       FORMAT(1A12)
421       FORMAT(1X,1A12)
c        OPEN(I1,FILE = fnamem)
          WRITE(*,*)'Passed the open command'
         ELSE
C        ERROR CHECKING: NO CORRECT RESPONSE
          GO TO 1030
        ENDIF
      ENDIF


C    DEFINING A DEBUG PRINTOUT OPTION FOR MAIN (0 = NO PRINTOUT)
      IDMAIN = 0
        
      DO 10 I=1,23  
10    ITEST(I)=0
      NOUT=6
C       INSERTING DEFAULT OUTPUT VARIABLES TO BE PRINTED    
      DO 12 I=1,6   
 12   IOUT(I)=I 
      NDEP=10   
C    ZEROING WORK, DEPTH AND OUTPUT ARRAYS 
      DO 24 I=1,560     
 24   WORK(I)=0.
      DO 25 I = 1,100   
   25   OUT(I) = 0.0 
c    now using the default depths in Blkdata.for      
c      DO 26 I = 1,30
c    DEP(I) = 0.0   
c26    CONTINUE  
      IPRINT=1   

C    ***********************************************************   
C            START OF LOOP FOR READING ALL INPUT FROM FILE 'DATAKY.DAT' 

200   CONTINUE  
c    goto 1111
      TD(10)=TDSS(MOY)
      TD(11)=TDSS(MOY)
      TI(10)=0.
      TI(11)=1440.

      MOYS=(MOY)*24-23
      MOYF=MOY*24
      TD(12:35)=real(TAIRhr1(MOYS:MOYF),4)
      TD(37:60)=real(RHhr1(MOYS:MOYF),4)
      TD(62:85)=real(CLDhr1(MOYS:MOYF),4)
      TD(87:110)=real(WNhr1(MOYS:MOYF),4)*6000.
      TD(112:135)=real(SOLRhr1(MOYS:MOYF),4)/ 4.185 / 10000. * 60.
      TD(36)=real(TAIRhr1(MOYF),4)
      TD(61)=real(RHhr1(MOYF),4)
      TD(86)=real(CLDhr1(MOYF),4)
      TD(111)=real(WNhr1(MOYF),4)*6000.
      TD(136)=real(SOLRhr1(MOYF),4)/ 4.185 / 10000. * 60.
      MOYS=(MOY)*25-24
      MOYF=MOY*25
c      TD(12:36)=TARS(MOYS:MOYF)
c      TD(37:61)=RELS(MOYS:MOYF)
c      TD(62:86)=CLDS(MOYS:MOYF)
c      TD(87:111)=VELS(MOYS:MOYF)
c      TD(112:136)=SOLS(MOYS:MOYF)
      TD(137:161)=ZENS(MOYS:MOYF)
      TD(162:186)=ZSLS(MOYS:MOYF)
      TI(12:36)=minutes
      TI(37:61)=minutes
      TI(62:86)=minutes
      TI(87:111)=minutes
      TI(112:136)=minutes
      TI(137:161)=minutes
      TI(162:186)=minutes
      if(microdaily.eq.1)then
      do 344 i=1,10
      T(i)=real(SOILINIT1(i),4)
344   continue
       else
      if(ifinal.eq.1)then
       T(1:10)=TINS(1:10,moy)
      endif

      endif 
      DEP(1:13)=DEPS(1:13)
      NDEP=13
      goto 1000
c1111  continue

C       CLEARING MA CHARACTER ARRAY 
      MA=IBLK   
C     READ CONTROL CARD 
      CALL RDCTRL(INPUT,M,MA,RWORK) 
C     DEBUG OUTPUT IF IDMAIN SET > 0
c      write(0,*) M
      IF (IDMAIN .GT. 0) THEN
C    DEBUG
c    WRITE(i2,*)'MAIN(RDCTRL:):INPUT,M,MA = ',INPUT,M,MA
       ELSE
      CONTINUE
      ENDIF
      M=IABS(M) 
C     CHOOSE RESPONSE TO FIRST ENTRY NAME ON CONTROL CARD   
      DO 199 K=1,23     
      IF(INPUT .EQ. SYMBOL(K)) THEN 
        GO TO 203 
       ELSE   
      ENDIF 
 199  CONTINUE  
c      WRITE(0,220) INPUT
c220   FORMAT('0**INCORRECT CONTROL CARD,',A3,' ENCOUNTERED.  JOB TERMINA
c     1TED.***') 
      STOP  

203   ITEST(K)=1
      IF(K.GT.14) GO TO 204 
c 201  READ (I4,*) PAR(K)     
      GO TO 200 

204   IF(K.EQ.18) STOP  
      IF(K.EQ.17) GO TO 1000
c      IF(K.EQ.16) GO TO 201 
c      IF(K.EQ.15) GO TO 324 
      J=K-18
c      GO TO(319,320,321,322,323),J  
c319   READ (I4,*) (T  (I),I=1,M)
c    Kearney changed this so that the initial temperatures are the previous day's last set
c    if(microdaily.eq.1)then
c    do 344 i=1,M
c    T(i)=SOILINIT1(i)
c344   continue
c       else
c     T(1:10)=TINS(1:10)
c    endif 
      GO TO 200 
c    Depths are no longer read from Dataky.dat
c320   READ (I4,*) (DEP(I),I=1,M) 
      NDEP=M
c    DEP(1:13)=DEPS(1:13)
c    NDEP=13
      GO TO 200 

C        READ OUTPUT VARIABLE INTEGER CODES TO BE PRINTED   
c 321  READ (I4,*) (IOUT(I),I=1,M)    
      NOUT=M
      GO TO 200 

      IF(M.EQ.0) M=1    
C     TITLE CARD (TTL)
      DO 3220 J=1,M     
c      READ(I4,1)(AWORK(I),I=1,20)    
c1     FORMAT(20A4)  
c2     FORMAT(1X,20A4)   
C      WRITE TITLE CARD TO FILE METOUT FOR LABEL OF EACH DAY'S OUTPUT   
C      FROM MICROMET TO BE USED IN ANIMAL ENERGY BALANCE PROGRAM
      IF (INPUT .EQ. 'TTL') THEN
C     CHECK IF REPLICATE DAY   
         IF (IFINAL .EQ. 1) THEN
C         LABEL IT    
          DO 1234 I = 1,20
           TTLABL(I) = AWORK(I)
1234      CONTINUE
c          WRITE(0,2) (TTLABL(I),I=1,20)  
         ELSE
c         Print to console iteration day label         
c          WRITE(0,1)(AWORK(I),I=1,20)           
          IF (IFINAL .EQ. ND) THEN
C          IDENTIFY SITE IN FILE METOUT & SOIL 
c            write (i2,*) ' ' 
c            write(0,*)' ' 
            if(shayd.lt.MAXSHD)then
c            write output to file metout
c            WRITE(I3,2)(TTLABL(I),I=1,20)
c            write output to file soil
c              WRITE(I10,2)(TTLABL(I),I=1,20)
            else
c            MAXIMUM SHADE
C            USE UNIT I13 FOR above ground micromet OUTPUT when % shade = 100.
c            CLOSE (I3, STATUS = 'KEEP')
c            CLOSE (I10, STATUS = 'KEEP')
c            write output to shadmet
c            WRITE(I12,2)(TTLABL(I),I=1,20)
c            write output to shadsoil 
c            (so both files can be available for animals seeking deep shade)
c            WRITE(I11,2)(TTLABL(I),I=1,20)
            endif
C          LABEL LAST REPEATED DAY'S OUTPUT WITH DAY NUMBER in 
C           files METOUT & SOIL
            if(SHAYD.LT.MAXSHD)then
c            output to metout
c            WRITE(I3,2)(AWORK(I),I=1,19)
c            output to soil
c              WRITE(I10,2)(AWORK(I),I=1,19)
             else
c            100% SHADE
c            output to shadmet
c            WRITE(I12,2)(AWORK(I),I=1,19)
c            output to shadsoil
c              WRITE(I11,2)(AWORK(I),I=1,19)
            endif
          ENDIF
        ENDIF  
      ENDIF 
c     End of title display from TTL line in file DATAKY.DAT
      
3220  continue
c    WRITE(i2,2)(AWORK(I),I=1,20)   
      GO TO 200 

C     READ A TABLE (PAIRS OF DATA; INDEPENDENT, DEPENDENT VALUE)
c323   CONTINUE  
      CALL RDTABL(M,MA) 
c      write(*,*) M
      IF(M.LT.0) ITEST(1)=-1
      GO TO 200 

c324   READ(I4,*)PAR(K)   
      GO TO 200 

C     NOTE THAT 'NON',PAR(14), SPECIFIES # OF SOIL NODES; THEY SHOULD   
C     BE THE SAME AS THE NUMBER SPECIFIED IN 'DEP', WHERE ACTUAL SOIL   
C     DEPTHS ARE SPECIFIED  
1000  N=int(PAR(14))
      MM=N-1
      IF(ALIZ)1001,1001,1002
 1002 N=N+1 
      MM=MM+1   
 1001 CONTINUE  
      IF(IOUT(1).LT.0) IPRINT=0     
      ERR1=PAR(10)  
      DTAU=PAR(9)   
      TIMEF=PAR(11)     
      TIME=PAR(7)   
      DO 325 I=1,N  
 325  WORK(I+520)=T(I)  
      IF(IPRINT.EQ.0  ) THEN
C      SKIP ANY KIND OF OUTPUT
        GO TO 3000  
       ELSE
        CONTINUE
      ENDIF
      IF (IFINAL .EQ. 1) THEN
C      PRINT OUT PARAMETER VALUES FOR THE FIRST DAY
c      WRITE(i2,2002)(PAR(I),I=1,7),PAR(11),PAR(9),PAR(10),PAR(12),   
c     *PAR(14),PAR(15)       
c 2002 FORMAT(   
c     1'0STEFAN-BOLTZMAN CONSTANT                 (SIG)=',E13.4/ 
c     2' DENSITY TIMES HEAT CAPACITY              (RCS)=',F9.4/  
c     3' SOIL THERMAL CONDUCTIVITY                (STC)=',F9.4/  
c     4' SOIL SOLAR ABSORPTANCE                   (SSA)=',F9.4/  
c     5' HEIGHT FOR REFERENCE                     (HGT)=',F9.4/  
c     6' SOIL SURFACE ROUGHNESS                   (RUF)=',F9.4/  
c     7' BEGINNING OF TIME                        (BEG)=',F9.4/  
c     8' END OF TIME                              (END)=',F9.4/  
c     /' PRINT INTERVAL                           (PRT)=',F9.4/  
c     1' ERROR TOLERANCE                          (ERR)=',F9.4/  
c     2' SOIL LONGWAVELENGTH EMISSIVITY           (SLE)=',F9.4/  
c     3' NUMBER OF SOIL NODES                     (NON)=',F5.0/  
c     4' PERCENT SUN                              (SUN)=',F5.2/) 
c 2000 FORMAT(6(1X,F11.4))   
c 2001 FORMAT(18I4)  
c      WRITE(i2,2003)     
c      WRITE(i2,2000) (T  (I),I=1,N)  
c      WRITE(i2,2004)     
c      WRITE(i2,2000) (DEP(I),I=1,NDEP)   
c      WRITE(i2,2005)     
c      WRITE(i2,2001) (IOUT(I),I=1,NOUT)  
c 2003 FORMAT(/' INITIAL SOIL TEMPERATURES')     
c 2004 FORMAT(/' DEPTH FOR SOIL TEMPS AND HEIGHTS FOR AIR TEMPS.  (+=SOIL
c     1, -=AIR)')
c 2005 FORMAT(/' OUTPUT VARIABLES TO BE PRINTED. (SEE KEY IN MANUAL)')   
       ELSE
C    REPLICATE DAY'S DATA NOT PRINTED
        CONTINUE
      ENDIF

 3000 CONTINUE  
C    DAY COUNTER TO DELETE TABLE OUTPUT FOR REPLICATE DAYS 
      IF (IFINAL .EQ. 1) THEN
C      PRINT TABLES
          CALL PTTABL(INAME,ILOCT,TI,TD,AWORK)    
         ELSE
C      SKIP OUTPUT
        CONTINUE   
      ENDIF  

C    INTERNAL LOOP IN MICROCLIMATE PROGRAM TO RUN 2 SUCCESSIVE IDENTICAL DAYS 
C    EXCEPT FOR A CHANGE IN THE AMOUNT OF SHADE. THE MONTHLY CHANGES IN SHADE
C    ARE DONE IN SOLRAD.  THE VARIABLE SHAYD IS CHANGED MONTHLY AND ALL THE
C    MINIMUMS ARE RUN FIRST FOR MAXIMUM SUN CONDITION, THEN THE WHOLE YEAR IS
C    REPEATED (NUMRUN = 2) FOR THE MINIMUM SUN CONDITION.  10/11/04  W. PORTER

c500   CONTINUE
 
C    SETTING THIS MONTH'S SHADE VALUES FOR CALCULATIONS IN DSUB
C    THIS DEPENDS ON WHETHER IT IS THE FIRST RUN (MINIMUM SHADE YEAR SIMULATION)
C    OR THE 2ND RUN (MAXIMUM SHADE FOR THE SAME YEAR SIMULATED)
      IF(IFINAL.EQ.1)THEN
        IF(NUMRUN.EQ.1)THEN
          SHAYD = MINSHADES(MOY)
          MAXSHD = MAXSHADES(MOY)
C        FILL IN SOIL NODE VARIABLE PROPERTIES AND PUT IN COMMON FOR DSUB'S USE
c         CALL SOYLNODS(MOY)
         ELSE
C        IT'S THE SECOND RUN WHERE THE VARIABLE SHAYD IS THE MAXIMUM VALUE FOR THE 
C        MAX. SHADE BOUNDING CONDITION
          SHAYD = MAXSHADES(MOY)
          MAXSHD = MAXSHADES(MOY)
        ENDIF
      ENDIF 
C     INCREMENT DAY COUNTER TO GET STEADY PERIODIC 
      IFINAL = IFINAL + 1  

C     CHECK FOR RESET OF IFINAL, IF # OF DAYS TO BE REPEATED, ND,   
C    HAS BEEN EXCEEDED, THEN QUIT THIS DAY'S SIMULATION
      IF (IFINAL .GT. ND) THEN  
        IFINAL = 1
C      NEED SOME RESET OF NUMRUN VALUE TO EITHER DO REPEAT DAY WITH NEW SHADE VALUE OR QUIT  
      ENDIF 
      IF(ITEST(1).EQ.-1) STOP 

C    CALL THE PREDICTOR-CORRECTOR NUMBERICAL INTEGRATOR TO DO EACH MONTH OF THE YEAR FOR SET VALUES
      CALL SFODE
      if(microdaily.eq.1)then
          ND=1
      do 101 i=1,10
        soilinit1(i)=work(520+i)
101    continue
      endif

C    LOOPING FOR THE SECOND DAY WITH MAX SHADE
      IF(NUMRUN.EQ.2)THEN
        IF(MOY.LE.JULNUM)THEN
          GO TO 200
         ELSE
C        SAVE SHADMET AND SHADSOIL
c        CLOSE (I11, STATUS = 'KEEP')
c        CLOSE (I12, STATUS = 'KEEP')

c    allocate ( metout1(25*JULNUM,13 )   )    
c    allocate ( shadmet1(25*JULNUM,13 )   )
c    allocate ( soil1(25*JULNUM,12 )   )
c    allocate ( shadsoil1(25*JULNUM,12 )   )

       do 910 j=1,18
           do 909 i=1,24*julnum
        metout1(i,j)=metout(i,j)
        shadmet1(i,j)=shadmet(i,j)
909   continue
      i=1
910   continue
       do 912 j=1,12
           do 911 i=1,24*julnum
        soil1(i,j)=soil(i,j)
        shadsoil1(i,j)=shadsoil(i,j)
911   continue
      i=1
912   continue
c        was STOP
          RETURN
c        STOP
        ENDIF
      ENDIF

C    CHECK FOR THE END OF A YEAR, MONTH COUNTER INCREMENTED IN OUTPUT SUBROUTINE OSUB
      ENDMON = JULNUM + 1
      IF (MOY.EQ.ENDMON)THEN
C      SAVE METOUT AND SOIL SO NOT DELETED IN SECOND RUN WITH MAX SHADE
c      CLOSE (I3, STATUS = 'KEEP')
c      CLOSE (I10, STATUS = 'KEEP')
C      CLOSE & REOPEN DATAKY.DAT TO GET THE PROGRAM TO START AT THE TOP & REREAD THE 'TRUE' INPUT DATA FILE
c      CLOSE (I4, STATUS = 'KEEP')
c      OPEN (I4,FILE = 'dataky.dat')
C      SET UP A SECOND ANNUAL RUN WITH MAX SHADE, INSTEAD OF MIN SHADE FOR THIS LOCATION
        NUMRUN = 2
        MOY = 1
        GO TO 200
      ENDIF
      
c      WRITE(i2,3001)     
c 3001 FORMAT('1')   
      DO 3002 I=1,N     
 3002 T(I)=WORK(I+520)
 
C    READ ANOTHER DAY'S DATA FROM INPUT FILE DATAKY.DAT
      GO TO 200

c110   FORMAT(1A1)
c      CLOSE (I1, STATUS = 'KEEP')
C     CLOSE (I2, STATUS = 'KEEP')
C    CLOSE (I5, STATUS = 'KEEP')
C     CLOSE (I6, STATUS = 'KEEP')
C     CLOSE (I7, STATUS = 'KEEP')
c      CLOSE (I8)
C     CLOSE (I9, STATUS = 'KEEP')
c5005  CALL Error(LINE)
c    CLOSE (I4)
      close (i3)
      close (i12)
      close (i10)
      close (i11)
      RETURN
      END