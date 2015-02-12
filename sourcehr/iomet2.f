       SUBROUTINE IOMET2  
       
C    COPYRIGHT WARREN P. PORTER  1997 ALL RIGHTS RESERVED       

C    PART 2
C    THIS SUBROUTINE SETS UP TIME INDEPENDENT PARAMETERS FOR
C        I/O FOR PROGRAM MICROPRO, 

C    VERSION 1.0, 24 MAY, 1989
C    There is a variable, SUN, which was used in the Nebraska
C    simulations, to allow for vegetation to reduce incoming 
C    sunlight.  1.0 = full sun, 0.0 = full shade.  This has not
C    been implemented yet in this version, 

      IMPLICIT LOGICAL (A-Z)  
c      IMPLICIT NONE
      REAL AIRDP,ALAT,ALONC,AMULT,AZMUTH,CMH2O,DAY
      REAL PRESS,PUNSH,REFL,RUF,SLOPE,SOILDP
      REAL Tannul,TEST,TIMCOR,TMINN,TMAXX,TSRHR,TSNHR,Usrhyt
      REAL T,WC,C,DEP,OUT,HEMIS
      REAL DEPS,TDSS,TINS,TARS,RELS,CLDS,VELS,SOLS,ZENS,ZSLS
          
      INTEGER cons,I,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12 
      INTEGER  IALT,IDA,IEND,IPINT,ISTART,IUV,IOUT,ITEST
      INTEGER  MA,MM,NAN,ND,NDEP,NON,NOSCAT,cnt,idayst,iep,STARTHOUR

      CHARACTER*1 ANS1,ANS3,ANS14,SINE,SNSLOP
      CHARACTER*80 LABL1,LABL2,LABL3
      CHARACTER*12 FNAME

      DIMENSION DAY(7300),TMINN(7300),TMAXX(7300)
      DIMENSION SOILDP(20),AIRDP(6)
      DIMENSION DEPS(13),TDSS(7300),TINS(10,7300),TARS(25*7300),
     &RELS(25*7300),CLDS(25*7300),VELS(25*7300),SOLS(25*7300),
     &ZENS(25*7300),ZSLS(25*7300)

      COMMON/LABEL/LABL1,LABL2,LABL3,FNAME,SINE,ANS14,SNSLOP
      COMMON/DAYS/DAY,TMINN,TMAXX,Tannul
      COMMON/WIOCONS/IPINT,NOSCAT,IUV,PUNSH,IALT,ALAT,AMULT,PRESS,
     * CMH2O,REFL,ALONC,IDAYST,IDA,TIMCOR,AZMUTH,SLOPE,TSNHR,TSRHR,IEP,
     * ISTART,IEND,Hemis,STARTHOUR 
      COMMON/ARRAY/T(30),WC(20),C(20),DEP(30),IOUT(100),        
     1 OUT(101),ITEST(23)
      COMMON/NDAY/ND
      COMMON/SOILND/NON
      COMMON/WMAIN/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12 
      COMMON/WIOMT2/RUF
      Common/Hyte/Usrhyt
      COMMON/dataky/DEPS,TDSS,TINS,TARS,RELS,CLDS,VELS,SOLS,ZENS,CNT,
     &ZSLS

C     Setting console value
      cons = 0
      
C    SETTING DEFAULT SOIL (NON) AND AIR NODES (NAN) HEIGHTS & DEPTHS
      NON = 10
      NAN = 3
      
C    INPUT NUMBER OF SOIL NODES 
C 4    write(cons,*)'How many soil nodes (depths) do you want to compute?'
C    write(cons,*)'Try to bound the problem, i.e. a surface, a deep '
C    write(cons,*)'soil and some intermediates, preferably in a log '
C    write(cons,*)'depth profile, since most rapid changes in '
C    write(cons,*)'temperature occur near the surface.'
C    write(cons,*)'A typical number of nodes is 10.  Max. number is 20.'
C    write(cons,*)' '
C    READ(6,*,ERR=4)NON
      IF (NON .LT. 4) THEN
        write(cons,*)'At least four nodes are needed'
        GO TO 400
       ELSE
        IF (NON .GT. 20) THEN
          write(cons,*)'Too many nodes.  20 is max.'
          GO TO 400
         ELSE
        ENDIF
      ENDIF

C    INPUT SOIL DEPTHS
C 5    write(cons,*)'Please supply depths (cm) where soil temperatures'
C    write(cons,*)'will be calculated.  Enter depths from the surface'
C    write(cons,*)'(at 0 cm depth) sequentially downward.' 
C    write(cons,*)'60 to 100 cm is typically a good deepest soil depth.'
C    write(cons,*)'Typical values for 10 nodes: ',
C     &  '0 2.5 5 10 15 20 30 40 50 60'
C    write(cons,*)'Do you want to have these depths supplied? (Y/N)'
C    write(cons,*)' '

C    READ(6,110)ANS3
      ANS3 = 'Y'
      IF ((ANS3 .EQ. 'Y') .OR. (ANS3 .EQ. 'y')) THEN
        IF (NON .NE. 10) THEN
          NON = 10
          write(cons,*)'The number of soil nodes specified earlier has'
          write(cons,*)'been changed to 10.'
        ENDIF
c      Assign depth values to soil depth array as specified in BLKDATA.FOR.  NOTE: THE USER NEEDS ACCESS TO NODE SPECIFICATION IN MICROv.DAT 3/12/11
        DO 300 I=1,NON
          SOILDP(I)=DEP(I)
300     CONTINUE
       ELSE
        IF ((ANS3 .EQ. 'N') .OR. (ANS3 .EQ. 'n')) THEN
         write(cons,*)'Please enter desired depths (cm) top->bottom'
         write(cons,*)'starting with 0 depth Be sure to insert a space'
         write(cons,*)'or comma between numbers. '
         READ(6,*)(SOILDP(I),I=1,NON)
         write(cons,150)(SOILDP(I),I=1,NON)
         write(cons,*)'Is this correct? (Y/N)'
         READ (6,110)ANS1
           IF ((ANS1 .EQ. 'Y') .OR. (ANS1 .EQ. 'y')) THEN
           CONTINUE
          ELSE
             IF ((ANS1 .EQ. 'N') .OR. (ANS1 .EQ. 'n')) THEN
             GO TO 400
              ELSE
C              ERROR TRAPPING...THE WRONG KEY WAS STRUCK
             write(cons,*)'Please try again'
             GO TO 400
             ENDIF
         ENDIF
          ELSE
C          ERROR TRAPPING...THE WRONG KEY WAS STRUCK
         write(cons,*)'Please try again'
         GO TO 400
         ENDIF
C     ERROR CHECKING THE SOIL NODE INPUTS
C     SURFACE NODE
       IF (SOILDP(1) .EQ. 0.000) THEN
         CONTINUE
        ELSE
         write(cons,*)'The first node must be at 0.00 cm depth.'
         GO TO 400
       ENDIF
C     DEEP SOIL NODE
       IF (SOILDP(NON) .GT. 500.) THEN
         write(cons,*)'Deep soil value is excessively large.'
         GO TO 400
        ELSE
         CONTINUE
       ENDIF    
C      CHECK FOR DEEPER & DEEPER SEQUENCE
       MM = NON - 1
       DO 50 I = 1,MM
         TEST = SOILDP(I) - SOILDP(I+1)
         IF (TEST .LT. 0.0000) THEN
           CONTINUE
          ELSE
           write(cons,*)'Soil depths not in top->bottom order.'
           GO TO 400
         ENDIF
50     CONTINUE
      ENDIF

C    INPUT AIR TEMPERATURE NODES
C 6    write(cons,*)'How many air nodes (heights) do you want to compute?'
C    write(cons,*)'These nodes are used to compute profiles of '
C    write(cons,*)'temperature, wind speed and relative humidity'
C    write(cons,*)'from reference (highest) height values.'
C    write(cons,*)'Try to bound the problem, i.e. a max. height, '
C    write(cons,*)'where you might measure air temperature and wind, '
C    write(cons,*)'and some intermediate heights in a log profile '
C    write(cons,*)'approaching the surface, since most rapid changes in'
C    write(cons,*)'the profiles occur near the surface.'
C    write(cons,*)'A typical number of nodes is 4.  Max. number is 6.'
C    write(cons,*)' '
C    READ(6,*,ERR=6)NAN
      IF (NAN .LT. 2) THEN
        write(cons,*)'At least two nodes are needed'
        GO TO 400
       ELSE
        IF (NAN .GT. 6) THEN
          write(cons,*)'Too many nodes.  6 is max.'
          GO TO 400
         ELSE
          CONTINUE
        ENDIF
      ENDIF
      GO TO 8

C    INPUT AIR TEMPERATURE HEIGHTS
C    write(cons,*)'For each of the air nodes just specified, please'
C7    write(cons,*)'input their air height values (cm) from the highest'
C    write(cons,*)'sequentially downward, but do not include the '
C    write(cons,*)'surface. Insert a space or comma between numbers.'
C    write(cons,*)'200 cm is typically a good highest height.'
C    write(cons,*)'PICK AS YOUR LOWEST HEIGHT THE AVERAGE HEIGHT OF'
C    write(cons,*)'YOUR ANIMAL OR OBJECT OF INTEREST.  THAT HEIGHT '
C    write(cons,*)'WILL BE USED TO COMPUTE THE AIR TEMPERATURE, WIND'
C    write(cons,*)'SPEED AND RELATIVE HUMIDITY FOR YOUR ANIMAL (OBJECT)'
C    write(cons,*)'Typical values (cm) for 4 air nodes for a lizard: ',
C     &  '200  20  6  1.5'
C    write(cons,*)' '
C    READ(6,*)(AIRDP(I),I=1,NAN)
8     CONTINUE
C    DEFINE AIR HEIGHTS (cm)
      AIRDP(1) = 200.
c    AirDP(2) now provided by user  6/27/98
      AIRDP(2) = Usrhyt
      if(RUF.gt.0.5)then
      AIRDP(3) = RUF+.5  
      else
      AIRDP(3) = 0.5
      endif
c      write(cons,150)(AIRDP(I),I=1,NAN)
C    write(cons,*)'Is this correct? (Y/N)'
C     READ(6,110)ANS2
C         IF ((ANS2 .EQ. 'Y') .OR. (ANS2 .EQ. 'y')) THEN
C       CONTINUE
C      ELSE
C           IF ((ANS2 .EQ. 'N') .OR. (ANS2 .EQ. 'n')) THEN
C         GO TO 7
C            ELSE
C            ERROR TRAPPING...THE WRONG KEY WAS STRUCK
C         write(cons,*)'Please try again'
C         GO TO 7
C           ENDIF
C     ENDIF
C    ERROR CHECKING THE AIR NODE INPUTS
C    SURFACE NODE
      IF (AIRDP(1) .LT. 1000.) THEN
        CONTINUE
       ELSE
        write(cons,*)'Max. height of 1000 cm allowed'
        GO TO 400
      ENDIF
C    NEAR SURFACE NODE
      IF (AIRDP(NAN) .LT. RUF) THEN
        write(cons,*)'Lowest air node smaller than roughness height,',
     &   RUF 
        write(cons,*)'Please use a larger height above the surface.'
        GO TO 400
       ELSE
        CONTINUE
      ENDIF    
C     CHECK FOR DEEPER & DEEPER SEQUENCE
      MA = NAN - 1
      DO 51 I = 1,MA
        TEST = AIRDP(I) - AIRDP(I+1)
        IF (TEST .GE. 0.0000) THEN
         ELSE
          write(cons,*)'Air heights not in descending (to soil) order.'
          GO TO 400
        ENDIF
51     CONTINUE    
C    SETTING UP THE NODE NUMBERS AND VALUES FOR THE DEP ARRAY
      NDEP = NON + NAN
      DO 52 I = 1,NAN
        AIRDP(I) = (-1.)*AIRDP(I)
52    CONTINUE
C    WRITING HEIGHTS & DEPTHS TO DATA INPUT FILE
c    WRITE(I4,*)'DEP ',NDEP
c    WRITE(I4,150)(AIRDP(I),I=1,NAN),(SOILDP(I),I=1,NON)
      do 53 i=1,3
       DEPS(i)=AIRDP(i)
53    continue
      do 54 i=1,10
       DEPS(i+3)=SOILDP(i)
54    continue    
c    WRITE(I4,*)'NON 1'
c    WRITE(I4,*)NON

C    INITIAL SOIL TEMPERATURES FOR EACH SOIL NODE 
C    WILL BE WRITTEN FROM SUBROUTINE SINEC

110   FORMAT(1A1)
c130   FORMAT(5I5)
150   FORMAT(8F9.2)
c170     FORMAT(16F5.0)
c180   FORMAT(1X,1I3,2F7.2)
c189   FORMAT(A60)
c190   FORMAT(1X,A60)
c191   FORMAT(1X,1F7.4)
c220   FORMAT(12A1)

400     RETURN
      END