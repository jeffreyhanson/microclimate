      program Microclimate
     
      IMPLICIT NONE
      double precision, allocatable, dimension(:,:) :: NODES2
      
      REAL ALIZ,ALPLIZ,ARLIZ,BLIZ,C,DENSITYS,DEP,DTAU
      REAL EPSLIZ,ERR1,H,HLIZ,KSOYL,OUT
      REAL PAR,PTWET,SABNEW,SPHEATS
      Real T,TD,THCONDS,TI,TIME,TIMEF,TMAX,TMIN,WLIZ,WORK 
      Real shayd,altt,MAXSHD,MAXSHADES,MINSHADES,WC,JULDAY,viewf
      real itair,icld,iwind,irelhum,sles,rainfall


      INTEGER I,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12
      Integer IDAY,IFINAL,ILOCT,IOUT
      INTEGER INTRVLS,IPCH,IPRINT
      Integer ISHADE,ITEST,J,JULNUM
      Integer MM,MONLY,MOY,N,ND,NDUM1,NKHT,NLIZ
      Integer NOCON,NODES,NOPRNT,NOSUM,NOTRAN
      INTEGER NOUT,NPOS,NUMINT,NUMRUN,NUMTYPS
      
      CHARACTER*80 LABL1,LABL2,LABL3
      CHARACTER*3 IBLK,INAME,SYMBOL    
      CHARACTER*1 solout,SINE,ANS14,SNO,SNSLOP
      CHARACTER*12 FNAME
      CHARACTER*80 LABEL

      DIMENSION BLIZ(8) 
      DIMENSION MAXSHADES(7300),MINSHADES(7300),JULDAY(7300)
      DIMENSION Nodes(10,7300)
      DIMENSION Intrvls(7300),KSOYL(10)
      
    
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


c    adding in for NicheMapR
      REAL RUF,SLE,ERR,Usrhyt,Z01,Z02,ZH1,ZH2
      REAL ALAT,ALMINT,ALONC,ALONG,ALREF
      REAL AMINUT,AMULT,AZMUTH
      REAL CCMAXX,CCMINN,CMH2O,DAY
      REAL HEMIS,soilprop,moists
      REAL PRESS,PUNSH,REFL,RHMAXX,RHMINN
      REAL SLOPE,REFLS
      REAL TANNUL,TIMCOR,TIMAXS,TIMINS,TMINN,TMAXX
      REAL TSRHR,TSNHR,WNMAXX,WNMINN,PCTWET,SNOW
      REAL metout,shadmet,soil,shadsoil,moist,snowhr
      REAL tannul2,hori,azi,tai,ec,rain,tannulrun
      real snowtemp,snowdens,snowmelt

      INTEGER IALT,IEND,IEP,IPINT,ISTART
      INTEGER IUV,NOSCAT,IDA,IDAYST,julstnd
      INTEGER microdaily,numyear,NN

      double precision microinput2,julday2,DEP2,SLES2,
     &Intrvls2,MAXSHADES2,MINSHADES2,TIMAXS2,TIMINS2,
     &RHMAXX2,RHMINN2,CCMAXX2,CCMINN2,WNMAXX2,WNMINN2,SNOW2,REFLS2,
     &PCTWET2,soilinit2,hori2,TAI2,TMAXX2,TMINN2,metout2,shadmet2,soil2
     &,shadsoil2,moists2,soilprop2,
     &rain2,tannulrun2

      DIMENSION CCMAXX(7300),CCMINN(7300)
      DIMENSION RHMAXX(7300),RHMINN(7300),TIMINS(4),TIMAXS(4)
      DIMENSION TMINN(7300),TMAXX(7300),WNMAXX(7300),
     &    WNMINN(7300),SNOWHR(25*7300)
      DIMENSION SNOW(7300),REFLS(7300),PCTWET(7300),tai(111)
      DIMENSION microinput2(34)
      DIMENSION soilprop(10,6),moists(10,7300),
     &moists2(10,7300),soilprop2(10,6)

      DIMENSION CCMAXX2(7300),CCMINN2(7300)
      DIMENSION RHMAXX2(7300),RHMINN2(7300),TIMINS2(4),TIMAXS2(4)
      DIMENSION TMINN2(7300),TMAXX2(7300),WNMAXX2(7300),
     &    WNMINN2(7300)
      DIMENSION SNOW2(7300),REFLS2(7300),PCTWET2(7300),tai2(111),
     &    soilinit2(10),hori2(24),SLES2(7300)
      DIMENSION Intrvls2(7300),Dep2(10)
      DIMENSION MAXSHADES2(7300),MINSHADES2(7300),JULDAY2(7300)

      DIMENSION SLES(7300)
      DIMENSION rain2(7300),rain(7300),tannulrun2(7300)

      DIMENSION DAY(7300),tannulrun(7300)
      DIMENSION julstnd(2)

      DIMENSION METOUT(24*7300,18),SHADMET(24*7300,18)
      DIMENSION SOIL(24*7300,12),SHADSOIL(24*7300,12)

      DIMENSION METOUT2(24*7300,18),SHADMET2(24*7300,18)
      DIMENSION SOIL2(24*7300,12),SHADSOIL2(24*7300,12)

      DIMENSION hori(24),azi(24)


c      double precision, allocatable,  :: METOUT5(:,:),
c     &SHADMET5(:,:),SOIL5(:,:),SHADSOIL5(:,:)          

      

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
c      DATA MOY/1/

      numyear=2
      NN=1
c      allocate(metout5(24*NN*365,18))
      allocate(nodes2(10,24*numyear*365))
      OPEN(1,FILE='microinput.csv')
      read(1,*)LABEL
      DO 11 i=1,34
      read(1,*)label,microinput2(i)
11    continue
      close(1)

      OPEN(1,FILE='julday.csv')
      read(1,*)LABEL
      do 13 i=1,365*numyear
      read(1,*)label,julday2(i)
13    continue
      close(1)

      OPEN(1,FILE='SLES.csv')
      read(1,*)LABEL
      do 14 i=1,365*numyear
      read(1,*)label,SLES2(i)
14    continue
      close(1)

      OPEN(1,FILE='DEP.csv')
      read(1,*)LABEL
      do 15 i=1,10
      read(1,*)label,DEP2(i)
15    continue
      close(1)

c    OPEN(1,FILE='Thconds.csv')
c    read(1,*)LABEL
c    read(1,*)label,Thconds2
c    close(1)

c    OPEN(1,FILE='Spheats.csv')
c    read(1,*)LABEL
c    read(1,*)label,Spheats2
c    close(1)

c    OPEN(1,FILE='Densitys.csv')
c    read(1,*)LABEL
c    read(1,*)label,Densitys2
c    close(1)

      OPEN(1,FILE='Intrvls.csv')
      read(1,*)LABEL
      do 19 i=1,365*numyear
      read(1,*)label,Intrvls2(i)
19    continue
      close(1)

      OPEN(1,FILE='Nodes.csv')
      read(1,*)LABEL
      do 20 i=1,10
      read(1,*)label,(Nodes2(i,j),j=1,365*20)
20    continue
      close(1)

      OPEN(1,FILE='Maxshades.csv')
      read(1,*)LABEL
      do 21 i=1,365*numyear
      read(1,*)label,Maxshades2(i)
21    continue
      close(1)

      OPEN(1,FILE='Minshades.csv')
      read(1,*)LABEL
      do 22 i=1,365*numyear
      read(1,*)label,Minshades2(i)
22    continue
      close(1)

      OPEN(1,FILE='TIMAXS.csv')
      read(1,*)LABEL
      do 23 i=1,4
      read(1,*)label,TIMAXS2(i)
23    continue
      close(1)

      OPEN(1,FILE='TIMINS.csv')
      read(1,*)LABEL
      do 24 i=1,4
      read(1,*)label,TIMINS2(i)
24    continue
      close(1)

      OPEN(1,FILE='TMAXX.csv')
      read(1,*)LABEL
      do 25 i=1,365*numyear
      read(1,*)label,TMAXX2(i)
25    continue
      close(1)

      OPEN(1,FILE='TMINN.csv')
      read(1,*)LABEL
      do 26 i=1,365*numyear
      read(1,*)label,TMINN2(i)
26    continue
      close(1)

      OPEN(1,FILE='RHMAXX.csv')
      read(1,*)LABEL
      do 27 i=1,365*numyear
      read(1,*)label,RHMAXX2(i)
27    continue
      close(1)

      OPEN(1,FILE='RHMINN.csv')
      read(1,*)LABEL
      do 28 i=1,365*numyear
      read(1,*)label,RHMINN2(i)
28    continue
      close(1)

      OPEN(1,FILE='CCMAXX.csv')
      read(1,*)LABEL
      do 29 i=1,365*numyear
      read(1,*)label,CCMAXX2(i)
29    continue
      close(1)

      OPEN(1,FILE='CCMINN.csv')
      read(1,*)LABEL
      do 30 i=1,365*numyear
      read(1,*)label,CCMINN2(i)
30    continue
      close(1)

      OPEN(1,FILE='WNMAXX.csv')
      read(1,*)LABEL
      do 31 i=1,365*numyear
      read(1,*)label,WNMAXX2(i)
31    continue
      close(1)

      OPEN(1,FILE='WNMINN.csv')
      read(1,*)LABEL
      do 32 i=1,365*numyear
      read(1,*)label,WNMINN2(i)
32    continue
      close(1)

      OPEN(1,FILE='SNOW.csv')
      read(1,*)LABEL
      do 33 i=1,365*numyear
      read(1,*)label,SNOW2(i)
33    continue
      close(1)

      OPEN(1,FILE='REFLS.csv')
      read(1,*)LABEL
      do 34 i=1,365*numyear
      read(1,*)label,REFLS2(i)
34    continue
      close(1)

      OPEN(1,FILE='PCTWET.csv')
      read(1,*)LABEL
      do 35 i=1,365*numyear
      read(1,*)label,PCTWET2(i)
35    continue
      close(1)

      OPEN(1,FILE='soilinit.csv')
      read(1,*)LABEL
      do 36 i=1,10
      read(1,*)label,soilinit2(i)
36    continue
      close(1)

      OPEN(1,FILE='hori.csv')
      read(1,*)LABEL
      do 37 i=1,24
      read(1,*)label,hori2(i)
37    continue
      close(1)

      OPEN(1,FILE='TAI.csv')
      read(1,*)LABEL
      do 38 i=1,111
      read(1,*)label,TAI2(i)
38    continue
      close(1)

      OPEN(1,FILE='soilprop.csv')
      read(1,*)LABEL
      do 39 i=1,10
      read(1,*)label,(soilprop2(i,j),j=1,6)
39    continue
      close(1)

      OPEN(1,FILE='moists.csv')
      read(1,*)LABEL
      do 40 i=1,10
      read(1,*)label,(moists2(i,j),j=1,365*numyear)
40    continue
      close(1)

      OPEN(1,FILE='rain.csv')
      read(1,*)LABEL
      do 41 i=1,365*numyear
      read(1,*)label,rain2(i)
41    continue
      close(1)

      OPEN(1,FILE='tannulrun.csv')
      read(1,*)LABEL
      do 42 i=1,365*numyear
      read(1,*)label,tannulrun2(i)
42    continue
      close(1)

c    call micr2011b(julnum1,julday1,RUF1,SLES1,ERR2,
c     &Usrhyt1,DEP1,numtyps1,numint1,Thconds1,Densitys1,Spheats1,
c     &Intrvls1,maxshades1,minshades1,Nodes1,Z011,Z021,ZH11,ZH21,idayst1,
c     &ida1,hemis1,alat1,aminut1,along1,almint1,alref1,slope1,
c     &azmuth1,altt1,cmh2o1,timaxs1,timins1,RHMAXX1,RHMINN1,CCMAXX1,
c     &CCMINN1,WNMAXX1,WNMINN1,TMAXX1,TMINN1,SNOW1,REFLS1,PCTWET1,
c     &soilinit1,microdaily1,tannul1,hori1,tai1,ec1,viewf1,metout1,soil1,
c     &shadmet1,shadsoil1)


      call micr2014(microinput2,julday2,SLES2,DEP2,
     &Intrvls2,maxshades2,minshades2,Nodes2,timaxs2,timins2,
     &RHMAXX2,RHMINN2,CCMAXX2,CCMINN2,WNMAXX2,WNMINN2,TMAXX2,TMINN2
     &,SNOW2,REFLS2,PCTWET2,soilinit2,hori2,tai2,soilprop2,moists2,
     &rain2,tannulrun2,metout2,soil2,shadmet2,shadsoil2)

c    OPEN(1,FILE='metout.csv')
c    write(1,*)metout2
      stop
      end
