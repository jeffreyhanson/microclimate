      SUBROUTINE SINEC  
      IMPLICIT NONE
C    COPYRIGHT 1997  WARREN P. PORTER  ALL RIGHTS RESERVED
C    VERSION 29 DEC. 1988

C     SUB. SINEC IS CALLED BY SUB. SOLRAD.
C     SINEC PREDICTS TEMPERATURE ON THE HOUR GIVEN THE AVERAGE MAXIMUM   
C     AND AVERAGE MINIMUM TEMPERATURES ** IN DEG. C ** FOR A SPECIFIED DAY. 

C  ******THIS PROGRAM WRITES TIME AND TEMPERATURE FOR MICROMET. ***** 
C      IT WRITES DATA FOR USE IN MICROMET TO DATA INPUT FILE "DATAKY.DAT"    

      REAL DAY,TAIRRY,TIMARY,TIMSR,TIMSS,TIMTMX,TMAX,TMIN,TMAXX,TMINN
      REAL TANNUL,itair,icld,iwind,irelhum,tmin2,tmax2
      REAL DEPS,TDSS,TINS,TARS,RELS,CLDS,VELS,SOLS,ZENS,ZSLS,DUMAIR
      real A,TSR,TREF,SS,SY,ZS,TSS,TAU,T,TI,E,X,Y,Z,FRMIN,TIMEC,TASUM
      real TIN,TDS

      INTEGER I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I,J,TIME,ITIME
      integer microdaily,cnt,IAIR,NON,iday

      DIMENSION DUMAIR(7300),TIMARY (7300),TAIRRY (7300)
      DIMENSION DAY(7300),TMAXX(7300),TMINN(7300)  
      DIMENSION DEPS(13),TDSS(7300),TINS(10,7300),TARS(25*7300),
     &RELS(25*7300),CLDS(25*7300),VELS(25*7300),SOLS(25*7300),
     &ZENS(25*7300),ZSLS(25*7300)

      COMMON/dataky/DEPS,TDSS,TINS,TARS,RELS,CLDS,VELS,SOLS,ZENS,CNT,
     &ZSLS 
      COMMON/WSINE/TIMSR,TIMSS,TIMTMX,TMIN,TMAX,TMIN2,TMAX2
      COMMON/SOILND/NON
      COMMON/WMAIN/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12
      COMMON/DAYS/DAY,TMINN,TMAXX,Tannul
      common/init/itair,icld,iwind,irelhum,iday
      COMMON/DAILY/microdaily

C     THIS LOOP READS IN THE LABEL AND APPROPRIATE DATA FOR EACH DAY.   
C ****  TIME IS READ IN IN HUNDRED HOURS  EG. 2:45 PM = 1445 HOURS.     
C          TMIN - MINIMUM TEMPERATURE   (C)     
C          TMAX - MAXIMUM TEMPERATURE   (C)     
C          TIMSR - TIME OF SUNRISE (FROM SOLRAD) = TIMIN   
C          TIMSS - TIME OF SUNSET (FROM SOLRAD)
C          TIMTMX - TIME OF MAXIMUM TEMPERATURE.  USUALLY 1300 HOURS = TIMAX   
C        IF IN THE SAME TIME ZONE AS THE REFERENCE MERIDIAN

C    TIMSR,TIMSS,TIMTMX PROVIDED BY SOLRAD 
      TMIN=TMIN+273.    
      TMAX=TMAX+273.
      TMIN2=TMIN2+273.
      TMAX2=TMAX2+273.    
      A=(TMAX-TMIN)/2.   
      TSR=TMIN
      TREF=(TIMTMX-TIMSR)/2.+TIMSR  
      SS=360.*(TIMSS-TREF)/(2.*(TIMTMX-TIMSR))  
      SY=SS/57.29577    
      ZS=SIN(SY)
      TSS=A*ZS+TMIN+A   
      TAU=3./((2400.-TIMSS)+TIMSR)  

C         THIS LOOP COMPUTES TEMPERATURE EACH HOUR OF THE DAY   
      DO 50 I=1,24  
         J=I+1  
         TIME=I*100
         IF (TIME-TIMSR) 20,21,11   
c          sunrise
   21      T=TMIN 
           T=T-273.   
           GO TO 10   
   11    IF (TIME-TIMSS) 15,15,13 
   20       TI=(2400.-TIMSS)+TIME
         if((microdaily.eq.1).and.(iday.gt.1))then
          T=((TMIN-273-ITAIR)/TIMSR)*TIME+ITAIR
         else
          E=TI*TAU   
          T=(TSS-TSR)/EXP(E)+TSR     
          T=T-273.
         endif       
         GO TO 10   
c        after sunset
   13    TI=(2400.-TIMSS)-(2400.-TIME)  
          E=TI*TAU   
           if(microdaily.eq.1)then
           T=(TSS-TMIN2)/EXP(E)+TMIN2 
           else
            T=(TSS-TSR)/EXP(E)+TSR  
           endif 
           T=T-273.   
           GO TO 10   
c        before or at sunset
   15    X=360.*(TIME-TREF)/(2.*(TIMTMX-TIMSR)) 
         Y=X/57.29577   
         Z=SIN(Y)   
         T=A*Z+TMIN+A   
         T=T-273.   
C         CONVERTING FROM MILITARY TIME TO BIOME TIME   
   10    ITIME=int(TIME)/100
         FRMIN=(TIME/100.)-ITIME     
         ITIME=ITIME*60
         FRMIN=FRMIN*100.   
         TIMEC=ITIME+FRMIN  
         TIMARY(J) = TIMEC  
         TAIRRY(J) = T  
50    CONTINUE  

C         WRITE START "CARD" FOR MICROMET DATA T(0)=T(1440)    
      TIMARY(1) = 0.0 
      if((microdaily.eq.1).and.(iday.gt.1))then 
       TAIRRY(1) = itair 
      else
       TAIRRY(1) = T
      endif     

C     PROCESSING AIR TEMPERATURES FOR SOIL INITIAL TEMPERATURES
      TASUM = 0.000
      DO 2005 IAIR = 1,25
       TASUM = TAIRRY(IAIR) + TASUM
2005  CONTINUE
C    MONTHLY AVERAGE
      TIN = TASUM/25.
C    ANNUAL TEMPERATURE AVERAGE
      TDS = TANNUL
 
c     writing initial soil temperature array from average of air temps
      DO 2010 IAIR = 1,NON
       DUMAIR(IAIR) = TIN
       TINS(IAIR,cnt)=TIN
2010  CONTINUE

C     WRITE CONTROL CARD FOR MICROMET DEEP SOIL TEMPERATURES
c      WRITE(I4,*)'TAB 2 TDS'
c      WRITE(I4,203)TIMARY(1),TDS
c      WRITE(I4,203)TIMARY(25),TDS
      TDSS(CNT)=TDS


C     WRITE CONTROL CARD FOR MICROMET INITIAL SOIL TEMP ARRAY    
c      WRITE(I4,*)'TIN',NON
c      WRITE(I4,203)(DUMAIR(IAIR),IAIR=1,NON)

C     WRITE CONTROL CARD FOR MICROMET AIR TEMP ARRAY INPUT   
c      WRITE(I4,310) 
c     write air temperatures as a function of time        
c      WRITE(I4,300)(TIMARY(I),TAIRRY(I),I=1,25) 
      do 2011 i=1,25
       TARS((CNT-1)*25+i)=TAIRRY(i)
2011  continue

c    get initial air temp for next day if doing 365 days of the year (microday=1)
      ITAIR=TAIRRY(25)

c  198 FORMAT (80A1)     
c  199 FORMAT (1X,80A1)  
c  200 FORMAT (2F10.2,6F10.0)
c  201 FORMAT (1X,8E10.4)
c  202 FORMAT (8F10.2)   
c  203 FORMAT (10F7.2)
c  300 FORMAT (2E13.3)   
c  310 FORMAT ('TAB 25 TAR') 
c  900 CONTINUE  

      RETURN
      END   
