      Subroutine RelHumLocal

c    This program uses 2 known temperatures and one known humidity to compute the other humidity.
c    The temperatures are the 2m reference air temperature, the local animal's air temperature and the 2m humidity to compute the local humidity.

      implicit none
      external INITDATA
      real ALT,ALTT,BP,CP,DB,DENAIR,DP,E,ESAT,PATMOS,DENSTY,DIFVPR       
      real RH,RHLOCL,THCOND,HTOVPR,TCOEFF,GGROUP,WB
      real SHAYD,SIOUT,MAXSHD,SABNEW,PTWET
      REAL T,WC,C,DEP,OUT,RW,TVINC,TVIR,VAPREF,VD,VISDYN,VISKIN
      REAL WTRPOT,rainfall
      
      INTEGER I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,ITEST,IOUT
      integer I91,I92,I93,I94,I95,I96 
      COMMON/GROUND/SHAYD,ALTT,MAXSHD,SABNEW,PTWET,rainfall
      COMMON/SIUNIT/SIOUT(10)
      COMMON/LOCLHUM/RHLOCL
      COMMON/ARRAY/T(30),WC(20),C(20),DEP(30),IOUT(100),        
     1 OUT(101),ITEST(23)
      COMMON/WMAIN/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I91,I92,I93
     & ,I94,I95,I96 

C    TIME     TA(LOC)  TA(2M) RH       V(LOC)   TS        T2      TDEEP    ZEN      SOLR     TSKY(C)  ELEV(M) 
C    SIOUT(1),SIOUT(2),OUT(2),SIOUT(3),SIOUT(4),SIOUT(5),SIOUT(6),SIOUT(7),SIOUT(8),SIOUT(9),SIOUT(10),ALTT,

C     ABSOIL %SHAD %WET  TANNUL
C     SABNEW,SHAYD,PTWET,TANNUL
c    setting variable values for sub's. dryair, wetair
      WB = 0.
      DP = 999.
c    altitude, alt, known, but not barometric pressure, bp
      ALT = ALTT
      BP = 0.
c    Dry bulb temperature is the 2m Tair
      DB = OUT(2)

c    call dryair to get atmospheric pressure, patmos, from altitude, alt, etc. for reference height
      call DRYAIR(DB,BP,ALT,PATMOS,DENSTY,VISDYN,VISKIN,DIFVPR,       
     *THCOND,HTOVPR,TCOEFF,GGROUP)

      RH = SIOUT(3)
      if(RH.gt.100.)then
            RH= 100.
      endif
c    get the vapor pressure, e, at Ta, 2m
      call WETAIR(DB,WB,RH,DP,BP,E,ESAT,VD,RW,TVIR,TVINC,DENAIR,CP,
     &  WTRPOT)
      vapref = e 
c    get the saturation vapor pressure, esat, at Tlocal
      RH = 100.
      DB = SIOUT(2)
      call WETAIR(DB,WB,RH,DP,BP,E,ESAT,VD,RW,TVIR,TVINC,DENAIR,CP,  
     *  WTRPOT)
c    Definition of relative humidity using the vapor density at reference height
      RHLOCL = (vapref/esat)* 100.

      if(RHLOCL.gt.100.)then
        RHLOCL = 100.
      endif
      if(RHLOCL.lt.0.000)then
        RHLOCL = 0.01
      endif
       
      return
      end