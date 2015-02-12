      SUBROUTINE VSINE(IVAR,VMIN,VMAX,TIMIN,TIMAX)
      implicit none
C    COPYRIGHT 1997  WARREN P. PORTER  ALL RIGHTS RESERVED
C    VERSION 29 DEC. 1988

C     SUB. SINE PREDICTS VARIABLE ON THE HOUR GIVEN THE MAXIMUM   
C     AND MINIMUM VALUES FOR A SPECIFIED DAY. 

C  ******THIS PROGRAM COMPUTES TIME AND VALUES FOR MICROMET. ***** 

      REAL TIMARY,TIMSR,TIMSS,TIMAX,TIMIN,tmin2,tmax2,TIMTMX,time,
     &  VMAX,VMIN,XA,YA,itair,icld,iwind,irelhum,vinit,tmin,tmax
      REAL DEPS,TDSS,TINS,TARS,RELS,CLDS,VELS,SOLS,ZENS,ZSLS,vave
      real vsmin,vsmax,slope1,slope2,slope3,frmin,timec
      INTEGER I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,IVAR,jsum,itest1,i
      integer microdaily,cnt,non,iday,itest2,itime,jct,I91,I92,I93
     & ,I94,I95,I96 

      DIMENSION TIMARY (25),XA(25),YA(25)
      DIMENSION DEPS(13),TDSS(7300),TINS(10,7300),TARS(25*7300),
     &RELS(25*7300),CLDS(25*7300),VELS(25*7300),SOLS(25*7300),
     &ZENS(25*7300),ZSLS(25*7300)

      COMMON /WSINE/TIMSR,TIMSS,TIMTMX,TMIN,TMAX,TMIN2,TMAX2
      COMMON /SOILND/NON
      COMMON/WMAIN/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I91,I92,I93
     & ,I94,I95,I96  
      common/init/itair,icld,iwind,irelhum,iday
      COMMON/DAILY/microdaily
      COMMON/dataky/DEPS,TDSS,TINS,TARS,RELS,CLDS,VELS,SOLS,ZENS,CNT
     &,ZSLS 
     
C     THIS LOOP READS IN THE LABEL AND APPROPRIATE DATA FOR EACH DAY.   
C ****  TIME IS READ IN IN HUNDRED HOURS  EG. 2:45 PM = 1445 HOURS.     
C          VMIN - MINIMUM VALUE
C          VMAX - MAXIMUM VALUE     
C          TIMSR - TIME OF SUNRISE (FROM SOLRAD)   
C          TIMSS - TIME OF SUNSET (FROM SOLRAD)
C       TIMIN - TIME OF MINIMUM.  USUALLY ABOUT SUNRISE
C          TIMAX - TIME OF MAXIMUM.  USUALLY 1300 HOURS    
C        IF IN THE SAME TIME ZONE AS THE REFERENCE MERIDIAN
      vinit=0
C    TIMSR,TIMSS,TIMAX PROVIDED BY SOLRAD 
      VAVE=(VMAX+VMIN)/2   
      if((microdaily.eq.1).and.(iday.gt.1))then
       IF (IVAR .EQ. 1) THEN
        vinit=irelhum
       Endif
       IF (IVAR .EQ. 2) THEN
        vinit=icld
       Endif
       IF (IVAR .EQ. 3) THEN
        vinit=iwind
       ENDIF
      endif

C    X = TIME, Y = DEPENDENT VARIABLE, E.G. TEMPERATURE

C    SETTING UP X,Y ARRAYS
      VSMIN = VMIN + 0.01*VAVE
      VSMAX = VMAX - 0.01*VAVE  
      if (timin .lt. timax) then
c       morning minimum, afternoon maximum 
        ITEST1 = int(TIMIN)/100
        ITEST2 = int(TIMAX)/100
C       Slope from midnight to morning minimum       
        SLOPE1 = (VAVE - VSMIN)/(100.- TIMIN)
C       Slope from morning minimum to aft. maximum        
        SLOPE2 = (VSMIN - VSMAX)/(TIMIN - TIMAX) 
C       Slope from aft. max. to midnight ave. for the day        
        SLOPE3 = (VSMAX - VAVE)/(TIMAX - 2400.)
       else
c       morning maximum, afternoon minimum 
        ITEST1 = int(TIMAX)/100
        ITEST2 = int(TIMIN)/100
C       Slope from midnight to morning maximum         
        SLOPE1 = (VAVE - VSMAX)/(100. - TIMAX)
C       Slope from morning maximum to aft. minimum           
        SLOPE2 = (VSMAX - VSMIN)/(TIMAX - TIMIN) 
C       Slope from aft. min. to midnight ave. for the day        
        SLOPE3 = (VSMIN - VAVE)/(TIMIN - 2400.) 
      endif  

      DO 12 I = 1,25
C       FINDING THE INDEPENDENT VALUE, X      
        XA(I) = FLOAT(I)*100. - 100.
C       CONVERTING FROM MILITARY TIME TO BIOME TIME   
        TIME = XA(I)
        ITIME=int(TIME/100.)
        FRMIN=(TIME/100)-ITIME     
        ITIME=ITIME*60
        FRMIN=FRMIN*100.   
        TIMEC=ITIME+FRMIN  
        TIMARY(I) = TIMEC  

C       FINDING THE DEPENDENT VALUE, Y
        IF (I .EQ. 1) THEN
         if((microdaily.eq.1).and.(iday.gt.1))then
          YA(I) = VINIT
         else
          YA(I) = VAVE 
         endif
          GO TO 12
        ENDIF
         
        IF (I .LT. ITEST1) THEN
C         ON FIRST SLOPE: y2 = y1 - m * (x1-x2)
          YA(I) = YA(1) - (SLOPE1 * (XA(1) -XA(I)))
          GO TO 12
        ENDIF

C       AT FIRST MINIMUM OR MAXIMUM?
        IF (I .EQ. ITEST1) THEN
C         AT MINIMUM OR MAXIMUM
          if (timin .lt. timax) then
            YA(I) = VSMIN 
           else
             YA(I) = VSMAX
          endif   
          GO TO 12
        ENDIF
         
        IF (I .LT. ITEST2) THEN 
C         ON SECOND SLOPE 
          if (timin .lt. timax) then
            YA(I) = VSMIN - (SLOPE2 * (TIMIN - XA(I)))
           else 
            YA(I) = VSMAX - (SLOPE2 * (TIMAX - XA(I)))   
C           Correcting for occasional overshoot when times not exactly 
C           on the hour
            if (ya(i) .gt. vsmax) then
              ya(i) = vsmax
            endif              
          endif   
          GO TO 12
        ENDIF

C       AT SECOND MIN OR MAX?
        IF (I .EQ. ITEST2) THEN
          if (timin .lt. timax) then
            YA(I) = VSMAX 
           else
            YA(I) = VSMIN
          endif
          GO TO 12
        ENDIF
        
        IF (I .GT. ITEST2) THEN
C         ON SLOPE 3 
          if (timin .lt. timax) then
            YA(I) = VSMAX - (SLOPE3 * (TIMAX - XA(I)))  
           else 
            YA(I) = VSMIN - (SLOPE3 * (TIMIN - XA(I)))  
          endif  
        ENDIF 

12    CONTINUE 
C     END OF LOOP LOOKING FOR DEPENDENT VALUE

C     WRITE CONTROL CARD FOR MICROMET ARRAY INPUT   
      IF (IVAR .EQ. 1) THEN
c        WRITE(I4,*)'TAB 25 REL'
      Endif
      IF (IVAR .EQ. 2) THEN
c        WRITE(I4,*)'TAB 25 CLD'
      Endif
      IF (IVAR .EQ. 3) THEN
c        WRITE(I4,*)'TAB 25 VEL'
      ENDIF

C     SET POSSIBLE NEG. VALUES TO POSITIVE VALUES
      jsum = 0
Cc    Making sure that the first and last values are positive
C     if(ya(1) .lt. 0.0)then
C       ya(jct) = abs(ya(jct))
C     endif
C     if(ya(25) .lt. 0.0)then
C       ya(jct) = abs(ya(jct))
C     endif
c    Checking for negative values at inflection points
      DO 100 JCT = 1,25
        if(ya(jct).lt.0.0)then
          jsum = jsum + 1
          if(ya(jct+1).gt.0.0)then
c          correct the neg. value using prior and post pos. values
            ya(jct) = (ya(jct-1) + ya(jct+1))/2.
           else
c          more than 1 negative value sequentially
            ya(jct) = abs(ya(jct))
          endif
        endif
100   CONTINUE

c    Error message if many problem data
      if(jsum .gt. 1)then
        if(ivar .eq. 1)then
c        write(0,*)jsum,' negative humidities in source data'
        endif
        if(ivar .eq. 2)then
c        write(0,*)jsum,' negative cloud values in source data'
        endif
        if(ivar .eq. 3)then
c        write(0,*)jsum,' negative wind values in source data'
        endif
      endif

C     WRITE TIME, VALUE ARRAY OUTPUT FOR MICROMET
c      WRITE(I4,300)(TIMARY(I),YA(I),I=1,25) 

        if(ivar .eq. 1)then
          irelhum=YA(25)
          do 2011 i=1,25
           RELS((CNT-1)*25+i)=YA(i)
2011      continue
        endif
        if(ivar .eq. 2)then
          icld=YA(25)
          do 2012 i=1,25
           CLDS((CNT-1)*25+i)=YA(i)
2012      continue
        endif
        if(ivar .eq. 3)then
          iwind=YA(25)
          do 2013 i=1,25
           VELS((CNT-1)*25+i)=YA(i)
2013      continue

        endif

c  198 FORMAT (80A1)     
c  199 FORMAT (1X,80A1)  
c  200 FORMAT (2F10.2,6F10.0)
c  201 FORMAT (1X,8E10.4)
c  202 FORMAT (8F10.2)   
c  203 FORMAT (10F7.2)
c  300 FORMAT (2E13.3)   
c  310 FORMAT ('TAB 25 TAR') 
c  320 FORMAT (1X,2I3,6E12.3)

      RETURN
      END   
