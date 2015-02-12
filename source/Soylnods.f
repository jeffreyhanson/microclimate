      SUBROUTINE SOYLNODS

C    This subroutine fills in soil node properties that change in time.
C    The properties are soil density, specific heat and thermal conductivity.
C    This allows for snow layers to be added to the soil or for variable thermal conductivities
C       with depth as in sandy beaches, or other purposes.

      IMPLICIT NONE
      
      REAL THCONDS,DENSITYS,SPHEATS,DENDAY,SPDAY,TKDAY,JULDAY
      
      INTEGER DAYCT,INTCT,INTRVLS,NSTRT,NEND,I,J,K
      INTEGER JULNUM,KSOYL,MOY,NODES,Numtyps,Numint,NODBOTM
      INTEGER ITEST,NON
      INTEGER I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I91,I92,I93
     & ,I94,I95,I96 

C    Day's soil properties    
      DIMENSION DENDAY(10),SPDAY(10),TKDAY(10),KSOYL(10)    
      DIMENSION Nodes(10,7300)
      DIMENSION Intrvls(7300) 

      COMMON/SOYVAR1/Numtyps,Numint,Intrvls
      COMMON/SOYVAR2/Thconds,Densitys,Spheats,Nodes,KSOYL
      COMMON/SOYFILS/DENDAY,SPDAY,TKDAY
      COMMON/SOILND/NON
      COMMON/DAYJUL/JULDAY,JULNUM,MOY
      COMMON/WMAIN/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I91,I92,I93
     & ,I94,I95,I96 

      DATA DAYCT/1/

C    VARIABLE DEFINITIONS
C    NODES(10,10) CONTAINS THE # OF SOIL TYPES, INTERVAL #, e.g. there are 4 time intervals in the following example
C    assuming soil type 1 is snow, there 
C    NODES(1,1) = 0
C    NODES(1,2) = 3
C    NODES(1,3) = 5
C    NODES(1,4) = 0
C    NODES(2,1) = 10
C    NODES(2,2) = 10
C    NODES(2,3) = 10
C    NODES(2,4) = 10
      NODBOTM=0
c    What day is it in the intervals defined by the user?
      DO 1 I=1,NUMINT
        INTCT = INTRVLS(I)
        IF(DAYCT.LE.INTCT)THEN
C        In the interval, proceed
C        Fill in soil properties for today
c        Work through all the soil nodes for today for each substrate type
          DO 2 J=1,NUMTYPS
           ITEST=NODES(J,I)
            IF (ITEST.LT.1)THEN
C            Substrate type absent, go to next substrate type
              NSTRT = 1
              NEND = 1
              NODBOTM = 0
            ENDIF    
            IF(ITEST.GT.0)THEN
C            This soil type is present. Where does it start, where does it end?
              IF(NODBOTM.EQ.0)THEN
C              Set the soil node counter going from top to bottom
                NSTRT = 1
                NEND = ITEST
               ELSE              
C              Set the soil node counter going from top to bottom
                NSTRT = NEND +1
C              Set end (deepest) node number
                NEND = NODES(J,I)
              ENDIF
C            Fill in values for nodes with this substrate type
              DO 3  K= NSTRT,NEND
                DENDAY(K)=DENSITYS
                SPDAY(K)=SPHEATS
                TKDAY(K)=THCONDS
                NODBOTM = NEND 
3             CONTINUE
            ENDIF
            IF(NEND.EQ.NON)THEN
              GO TO 100
            ENDIF
2         CONTINUE
         ELSE
C        Not in the interval, keep looking
          NODBOTM = 0
        ENDIF
C      End of all days in this interval
1     CONTINUE
      
100   continue
c    WRITE(I2,*)'Day',DAYCT,' densities (g/cm3)',(DENDAY(I),I=1,10)
c    WRITE(I2,*)'Day',DAYCT,' specific heats (cal/g-C)',
c     &    (SPDAY(I),I=1,10)
c    WRITE(I2,*)'Day',DAYCT,' thermal conductivities (cal/min-cm-C)',
c     &    (TKDAY(I),I=1,10)
      DAYCT = DAYCT+1

      RETURN
      end