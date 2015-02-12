       SUBROUTINE iomet1
       
C     COPYRIGHT WARREN P. PORTER  2004 ALL RIGHTS RESERVED       

C    PART 1
C    THIS SUBROUTINE SETS UP TIME INDEPENDENT PARAMETERS FOR
C        I/O FOR PROGRAM MICROMET, THEN CALLS SUB. IOTCON

C    VERSION 1.0, 24 MAY, 1989
C    There is a variable, SUN, which was used in the Nebraska
C    simulations, to allow for vegetation to reduce incoming 
C    sunlight.  1.0 = full sun, 0.0 = full shade.  This has not
C    been implemented yet in this version, 1.0, of IOMET.

      IMPLICIT none 

      REAL ALAT,ALONC,AMULT,AZMUTH
      REAL C,CMH2O,DAY,DENSITYS,DEP,ERR,Hemis
      REAL OUT,julday,julstnd,KSOYL
      REAL PAR,PRESS,PUNSH,REFL,RUF
      REAL SLE,SLOPE,SPHEATS,sles
      REAL T,Tannul,THCONDS,TIMCOR,TMINN,TMAXX,TSRHR,TSNHR,Usrhyt
      REAL WC,Z01,Z02,ZH1,ZH2
      REAL tannul2
      REAL soilprop,moists,moist,surflux,ep

      INTEGER cons,i,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I91,I92,I93
     & ,I94,I95,I96   
      Integer IALT,IEND,IEP,IOUT,IDA,IDAYST
      INTEGER INTRVLS,IPINT,ISTART,ITEST,IUV
      INTEGER julnum,MOY,ND,NODES,NOSCAT,NUMINT,NUMTYPS
      INTEGER microdaily,NON

      CHARACTER*1 ANS14,SINE,SNSLOP
      CHARACTER*80 LABL1,LABL2,LABL3
      CHARACTER*12 FNAME
        
      DIMENSION DAY(7300),TMINN(7300),TMAXX(7300),julday(7300)
     &    ,julstnd(2)
      DIMENSION Nodes(10,7300)
      DIMENSION Intrvls(7300),sles(7300)
      DIMENSION soilprop(10,6),moists(10,7300),moist(10)

      COMMON/ARRAY/T(30),WC(20),C(20),DEP(30),IOUT(100),        
     & OUT(101),ITEST(23)
      COMMON/LABEL/LABL1,LABL2,LABL3,FNAME,SINE,ANS14,SNSLOP
      COMMON/ENDS/JULSTND
      COMMON/DAYS/DAY,TMINN,TMAXX,Tannul
      COMMON/DAYJUL/JULDAY,JULNUM,MOY
      COMMON/WIOCONS/IPINT,NOSCAT,IUV,PUNSH,IALT,ALAT,AMULT,PRESS,
     * CMH2O,REFL,ALONC,IDAYST,IDA,TIMCOR,AZMUTH,SLOPE,TSNHR,TSRHR,IEP,
     * ISTART,IEND,Hemis 
      COMMON/NDAY/ND
      COMMON/PAR/PAR(18)
      COMMON/SOILND/NON
      COMMON/WMAIN/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I91,I92,I93
     & ,I94,I95,I96   
      COMMON/WIOMT2/RUF 
      Common/Hyte/Usrhyt
      COMMON/DMYCRO/Z01,Z02,ZH1,ZH2 
      COMMON/SOYVAR1/Numtyps,Numint,Intrvls
      COMMON/SOYVAR2/Thconds,Densitys,Spheats,Nodes,KSOYL
      COMMON/NICHEMAPRIO/SLE,ERR,SLES,soilprop,moists,surflux
      common/moistcom/moist,ep
      COMMON/DAILY/microdaily,tannul2

c     Defining console output number       
       cons = 0
C     ***************************************************************
C     CALCULATION - OUTPUT OPTIONS
c     write(cons,*)'               PROGRAM MICROMET '

c     write(cons,*)'       FOR HORIZONTAL AND SLOPING SURFACES    '

c       write(cons,*)'      COPYRIGHT  2000   ALL RIGHTS RESERVED  '
c     write(cons,*)'                                '
c     write(cons,*)'               Warren P. Porter'
c     write(cons,*)'                                '
c     write(cons,*)'       U. of Wisconsin Dept. of Zoology'
c     write(cons,*)'                                '
c     write(cons,*)'           Madison, Wisconsin 53706 '
c     write(cons,*)'                                '

c     write(cons,*)'                                '

C    DEFINING THE NUMBER OF DAYS TO REPEAT TO GET A STEADY PERIODIC
c    Kearney changed this for daily simulations
      if(microdaily.eq.1)then
          if(moy.eq.1)then
           ND = 3
           else
           ND = 1
          endif
      else
           ND = 3
      endif
C    WRITE(6,*)'1) Please input a one line title for your site'
C    WRITE(6,*)'e.g., Madison, WI.,  43 n lat, 90 w long'
C    WRITE(6,*)' '

c      READ(I1,189) LABL1
c      WRITE (I2,190)LABL1 
c      READ(I1,189) LABL2
c      WRITE (I2,190)LABL2         
c      READ(I1,189)LABL3
c      WRITE(I2,190)LABL3 
c    write site labels to metout,soil, shadmet,shadsoil
C    These writes have been moved to Micr2004, contingent on shade status
C    write(i3,190)labl
C    write(i10,190)labl
C    write(i11,190)labl
C    write(i12,190)labl

C    Specify the # of Julian days that will be run, the starting & ending Julian days
c    do 9 i=1,3
c      READ(I1,189) LABL
c        WRITE (I2,190)LABL
c9    continue
c    read(i1,*)julnum,(julstnd(i),i=1,2)
c    write(i2,*)julnum,(julstnd(i),i=1,2)
c    error check
c    if(julnum.gt.365)then
c      write(0,*)'Maximum # of julian days is 52 currently.'
c      write(0,*)'Please reduce the number of days you are running.'
c      stop
c    endif

c    Read the label lines before reading Julian days to simulate
c    do 10 i=1,3
c      READ(I1,189) LABL
c        WRITE (I2,190)LABL
c10    continue
c    read(i1,*)(julday(i),i=1,julnum)
c    write(i2,*)(julday(i),i=1,julnum)

C 2002    write(cons,*)'How many repeats of each day do you want? (1-5)'
C    write(cons,*)'To establish a steady periodic, each day is '
C    write(cons,*)'"repeated" with the soil temperatures at the'
C    write(cons,*)'end of a day acting as the initial temperatures'
C    write(cons,*)'for the start of the next day.  Three repeats of a'
C    write(cons,*)'day are usually sufficient to get good profiles.'
C    write(cons,*)' '
C    READ(6,*)ND
      IF (ND .LT. 1) THEN
        write(cons,*)'The number of repeat days must be 1 or more.'
        GO TO 400
       ELSE 
        IF (ND .GT. 5) THEN
          write(cons,*)'The number of days must be 5 or less.'
          GO TO 400
         ELSE
        ENDIF
      ENDIF 

C    START WRITE TO DATA INPUT FILE FOR PROGRAM MICROMET
c    WRITE(I4,*)'TTL 1'
c    WRITE(I4,*)'----------------------------------------------------'
C    SUPPLY OUTPUT OPTIONS AUTOMATICALLY AT THIS TIME
c    WRITE(I4,*)'IOT 12'
c    WRITE(I4,*)' 1 2 3 4 5 6 14 15 16 17 18 19'

C    INPUT THE ROUGHNESS PARAMETER, Zo
C 1    write(cons,*)'2) Please input a roughness height (cm) for the type'
C    write(cons,*)'of substrate, e.g. sand is 0.05, grass may be 2.0'
C    write(cons,*)'Current allowed range: 0.001 (snow) - 2.0 cm.'
C    write(cons,*)'This value is obtained by extrapolating to zero wind'
C    write(cons,*)'speed the velocity profile for your site.  Use the '
C    write(cons,*)'axes "velocity" and "natural log of height".  Then'
C    write(cons,*)'take the antilog of the height at the zero velocity',
C     +  ' intercept.'
C    write(cons,*)' '

C    READING FROM MASTER DATA FILE FOR MICROMET (1ST 3 ENTRIES) &
C    FOR SOLRAD
c    DO 100 I=1,4
c       READ(I1,189) LABL
c     WRITE (I2,190)LABL         
c100    CONTINUE 
C    Read roughness height, soil thermal conductivity, soil reflectivity
C    substrate density, substrate specific heat, long wavelength IR emissivity
C    error level for integrator, height of animal/object which specifies 'local'
C    air temperature and wind speed at that height in output (file metout).            
c    READ(I1,*)RUF,SLE,ERR,Usrhyt
c    WRITE(I2,*)RUF,SLE,ERR,Usrhyt   

1     CONTINUE
      IF (RUF .LT. 0.0001) THEN
        write(cons,*)'The roughness height is too small ( < 0.0001).'
        write(cons,*)'Please enter a larger value.'
       READ(6,*,ERR=1)RUF
        GO TO 1
       ELSE
        IF (RUF .GT. 2.0) THEN
           write(cons,*)'The roughness height is too large (>2.0).'
           write(cons,*)'Please enter a smaller value.'
           READ(6,*,ERR=1)RUF
           GO TO 1
          ELSE
         ENDIF
       ENDIF
C     Converting RUF (m) to cm for Microclimate calculations
      RUF = RUF * 100. 
      par(6)=RUF       
C     WRITING TO DATA INPUT FILE FOR MICROMET
c    WRITE(I4,*)'RUF 1'
c    WRITE(I4,*)RUF

c    Read user supplied node depths
c    DO 103 I=1,4
c       READ(I1,189) LABL
c     WRITE (I2,190)LABL         
c103    CONTINUE 
C    Read soil depth (cm) data 
c    READ(I1,*)(DEP(I), I=1,10)
c    Do error check to make sure the data are increasing in value
      Do 409 I=1,9
        IF(DEP(1).NE.0.0)THEN
          WRITE(CONS,*)'First soil node must = 0 cm. Please correct'
c        Pause
          Stop
        ENDIF
        IF(I.GT.1)THEN
          IF(DEP(I+1).LE.DEP(I))THEN
            WRITE(CONS,*)'Soil depth is not in ascending size'
c          Pause
            Stop
          ENDIF
        ENDIF
409   CONTINUE
c    WRITE(I2,*)(DEP(I), I=1,10)

C    READING SOIL PROPERTIES VARYING IN TIME AND SPACE
c    Read in user-supplied # of soil types, # of time intervals. Format like Iosolr.for
c    Do 401 i = 1,3
c     MSG = 'substrate types & time intervals labels'
c       READ(I1,189,Err=1000) LNAME
c       WRITE(I2,189)  LNAME 
c401    continue
c    MSG = 'Reading substrate types, intervals'
c    Read(I1,*,Err=1000)Numtyps,Numint
c    Write(I2,*)Numtyps,Numint

c    Read in user-supplied substrate thermal conductivities
c    Do 402 i = 1,5
c     MSG = 'soil thermal conductivity labels'
c       READ(I1,189,Err=1000) LNAME
c       WRITE(I2,189)  LNAME 
c402    continue
c    MSG = 'substrate thermal conductivities'
c    Read(I1,*,Err=1000)(Thconds(i),i=1,numtyps)
c    Write(I2,*)(Thconds(i),i=1,numtyps)

c    Read in user-supplied substrate densities
c    Do 403 i = 1,5
c     MSG = 'substrate density labels'
c       READ(I1,189,Err=1000) LNAME
c       WRITE(I2,189)  LNAME 
c403    continue
c    MSG = 'Reading substrate densities'
c    READ(I1,*,Err=1000)(Densitys(i),i=1,numtyps)
c    Write(I2,*)(Densitys(i),i=1,numtyps)

c    Read in user-supplied substrate specific heats
c    Do 404 i = 1,5
c     MSG = 'soil specific heat labels'
c       READ(I1,189,Err=1000) LNAME
c       WRITE(I2,189)  LNAME 
c404    continue
c    MSG = 'Reading substrate specific heats'
c    READ(I1,*,Err=1000)(Spheats(i),i=1,numtyps)
c    Write(I2,*)(Spheats(i),i=1,numtyps)

c    Read in user-supplied last Julian day in each time interval sequence,
c    Do 405 i = 1,5
c     MSG = 'last Julian day interval labels'
c       READ(I1,189,Err=1000) LNAME
c       WRITE(I2,189)  LNAME 
c405    continue
c    MSG = 'Reading last Julian day for each interval'
c    READ(I1,*,Err=1000)(Intrvls(i),i=1,numint)
c    Write(I2,*)(Intrvls(i),i=1,numint)

c    Read all the deep nodes for substrate type 1, then type 2, etc.
c    Do 406 i = 1,5
c     MSG = 'LNAME dealing with deepest node for substrate type'
c       READ(I1,189,Err=1000) LNAME
c       WRITE(I2,189)  LNAME 
c406    continue
c    MSG = 'Reading deepest node for each substrate type'
c    Do 407 j=1,numtyps
c      READ(I1,*,Err=1000)(Nodes(j,i),i=1,numint)
c      Write(I2,*)(Nodes(j,i),i=1,numint)
c407    Continue
C    END OF VARIABLE SUBSTRATE INPUTS

C    Process variable soil data in SOYLFIL out of SOLRAD for each simulation day.
c    Filling in soil properties for a simulation day
C     Density-specific heat product computation 
C     NOTE:  This product should be of the order of magnitude of 2.0e+06
c    Do 408 j=1,numtyps
c     Do 1408 k=1,numint
c     Convert densities from kg/m3 to g/cm3 for DSUB'S Microclimate calculations
C     1 kg/m3 * 1000g/1 kg * 1 m3/1000000.
c     Densitys = Densitys/1.0E+3
c     Convert specific heats from J/kg-K to cal/g-K for DSUB'S Microclimate calculations
c     Spheats=Spheats/4185.
c     Convert thermal conductivities from W/m-K to cal/min-cm-K for DSUB'S Microclimate calculations
c     Thconds=(Thconds/418.5)*60.
c      write(cons,*) j,' ',
c     &      k,' ',Thconds(j,k)
c     Do error checking
c       RCS = Densitys*Spheats  
c     IF (Thconds .LT. 0.000) THEN
c      write(cons,*)'Thermal conductivity is negative.',numtyps,' ',
c     &      numint,' ',Thconds
c      write(cons,*)'Please input a positive value.'
c      Pause
c      Stop
c    ENDIF
c1408    continue
c408    Continue
c    PAR(2) = RCS

c    End processing variable soil data for each simulation day.


C    INPUT THE SOIL THERMAL CONDUCTIVITY, STC
C 2    write(cons,*)'3) Please input the soil thermal conductivity (W/mC)'
C    write(cons,*)'A typical value for sand would be 0.35 '
C        write(cons,*)'allowed range: 0.04 (dry moorland) - 0.46 (rock)'
C    write(cons,*)'from Geiger, The Climate Near the Ground, p.29, 1965'
C    write(cons,*)' '
C    READ(6,*,ERR=2)STC

C    INSERTING IN DATA FILE DATAKY.DAT FOR MICROMET
c    WRITE(I4,*)'STC 2'
c    WRITE(I4,*)(Thconds(j),j=1,numtyps)

C    INPUT THE SOIL SOLAR ABSORPTIVITY
C 12      write(cons,*)'8) What is the ground reflectivity? '
C    write(cons,*)'Allowable range is 0.05 - 0.95 . Choose a value '
C        write(cons,*)'from the values listed below, if you do not know.'
C    write(cons,102)
c102     FORMAT(1X,'   Dark soil:     .05 -.15',
c     *  /4X,'Green meadows: .10 -.20',
c     *  /4X,'Forest:        .10 -.20',
c     *  /4X,'Dry sand:      .35 -.45',
c     *  /4X,'Old snow:      .40 -.70',
c     *  /4X,'New snow:      .75 -.95 (Sellers, Physical Climatology,',
c     *      ' 1965)')
C    write(cons,*)' '
C     READ(6,*,ERR=12)REFL
c12     CONTINUE
c    Deleted single reflectivity input. Time dependent values now read near the end of Iosolr.for

13     CONTINUE
         IF ((SLE .LT. 0.05) .OR. (SLE .GT. 1.00)) THEN
         write(cons,*)'Emissivity ',SLE,' is out of bounds'
         write(cons,*)'Please enter a correct value (0.05 - 1.00).'
           READ(6,*,ERR=13)SLE
         GO TO 13
        ELSE
C          ALL'S WELL
       ENDIF
C    WRITE TO DATA INPUT FILE
c    WRITE(I4,*)'SLE 1'
c    WRITE(I4,*)SLE
      PAR(12) = SLE

14    CONTINUE
      IF (ERR .LT. 0.00) THEN
         write(cons,*)'Error bound ',ERR,' is too small'
         write(cons,*)'Please enter a correct value (> 0.00).'
           READ(6,*,ERR=14)ERR
         GO TO 14
        ELSE
C          ALL'S WELL
      ENDIF
C    WRITE TO DATA INPUT FILE
c    WRITE(I4,*)'ERR 1'
c    WRITE(I4,*)ERR    
      PAR(10) = ERR

c    DO 101 I=1,5
c       READ(I1,189) LABL
c     WRITE (I2,190)LABL         
c101    CONTINUE 
C    Read data for segmented velocity profiles
c    READ(I1,*)Z01,Z02,ZH1,ZH2
c    WRITE(I2,*)Z01,Z02,ZH1,ZH2
      
C    Convert to cm, since program Micr2007 computes in cm, min, cal
      Z01 = Z01*100.
      Z02 = Z02*100.
      ZH1 = ZH1*100.
      ZH2 = ZH2*100.

c110   FORMAT(1A1)
c130   FORMAT(5I5)
c150   FORMAT(8F9.2)
c170   FORMAT(16F5.0)
c180   FORMAT(1X,1I3,2F7.2)
c189   FORMAT(1A80)
c190   FORMAT(1A80)
c191   FORMAT(1X,1F7.4)
c220   FORMAT(1A12)

400   RETURN
c1000  call error(MSG)    
      END  
