      SUBROUTINE PTTABL(INAME,ILOCT,TI,TD,AWORK)
      IMPLICIT NONE
C    COPYRIGHT 1997 WARREN P. PORTER,  ALL RIGHTS RESERVED



      REAL TD,TI

      INTEGER I,IA,II,IJ,IK,IEND,ILEFT,ILOCT,ILTH,IPLACE,   
     *ISTART,ISTOR,ISTOR1,ISTOR2,ISTRT,ISUM,ITAB,ITAB1,ITEST,J, 
     *NMAX,ICK,KK,JJ
      INTEGER I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12 

      CHARACTER*6 AWORK(1000),FOR(9)
      CHARACTER*3 INAME 
      
      DIMENSION ICK(3),INAME(20),ILOCT(21),TI(200),TD(200)  
     1,ILTH(5),ISTRT(5) 
     
      COMMON/WMAIN/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12
      
      DATA ICK/3*0/ 
      DATA FOR/'   IND','EPENDE','NT DEP','ENDENT', 
     1 '   VAR','IABLE','   VAR','IABLE','      '/  

C     DAY COUNTER TO DELETE OUTPUT FOR REPLICATE DAYS   
C     NEEDED TO ESTABLISH SOIL STEADY PERIODICS 
      ILEFT=0
      ITAB = 3  
      ITAB1=ITAB-1  
      DO 5 I=1,200  
    5 AWORK(I)=FOR(9)   
C         COUNT TABLES  
      DO 10 I=2,21  
      IF(ILOCT(I).EQ.0) GOTO 15     
      ILEFT=I-1 
   10 CONTINUE  
   15 CONTINUE  
c      write(i2,6) ILEFT  
c    6 FORMAT('0  THE',I3,' TABLE(S)'/)  
C        ICOUNT = NUMBER OF ENTRIES TO WORK     
C?         ISTART = CURRENT TABLE BEING WORKED ON   
      ISTART=0  
      IEND=0
   50 CONTINUE  
      DO 55 I=1,5   
      ILTH(I)=0 
   55 ISTRT(I)=0
      ISTART=IEND+1     
      IF(ILEFT.LE.ITAB) GO TO 100   
      IEND=ISTART+ITAB1 
      GOTO 150  
  100 IEND=ISTART+ILEFT-1   
  150 CONTINUE  
c      write(i2,151)(INAME(I),I=ISTART,IEND)  
c  151 FORMAT(13X,A3,4(21X,A3))  
c      write(i2,152)  
c  152 FORMAT(' ')   
C         PLACE HEADINGS OF FOR INTO WORK   
      IPLACE=0  
      DO 170 I=ISTART,IEND  
      DO 170 J=1,4  
      IPLACE=IPLACE+1   
      AWORK(IPLACE)=FOR(J)  
      AWORK(IPLACE+20)=FOR(J+4)     
  170 CONTINUE  
      IPLACE=(IEND-ISTART+1)*4  
c      write(i2,171)(AWORK(I),I=1,IPLACE) 
      IPLACE=IPLACE+20  
c      write(i2,171)(AWORK(I),I= 21,IPLACE)   
c  171 FORMAT(20A6)  
C         BEGIN THE TABLES  
      NMAX=0
      IA=0  
      DO 200 I=ISTART,IEND  
       IPLACE=ILOCT(I+1)-ILOCT(I)   
      IA=IA+1   
      ILTH(IA)=IPLACE   
      ISTRT(IA)=ILOCT(I)
      IF(IPLACE.GT.NMAX) NMAX=IPLACE
  200 CONTINUE  

C    ***************************************************************   
C        TABLE OUTPUT   

C     FIRST CHECK FOR TOO MANY ENTRIES  
      IF (NMAX .GT. 200) THEN   
c         write(i2,*)NMAX,' IS TOO MANY PAIR OF TABLE ENTRIES. MAX = 200. 
c     *SEE SUB. PTTABL'  
         GO TO 251  
        ELSE
         CONTINUE   
      ENDIF 

c      write(i2,205)  
c  205 FORMAT(' ')   
      DO 250 I=1,NMAX   
      IA=ISTRT(1)-1+I   
C      DON'T WRITE IF I EXCEEDS TABLE 1 LENGTH 
      IF(I.GT.ILTH(1)) THEN 
          ICK(1)=0  
          GO TO 210 
        ELSE 
          ICK(1)=1  
          II=IA 
      ENDIF  
c  208 FORMAT(1X,E10.4,2X,E10.4) 
  210 IF(ISTRT(2).EQ.0) GOTO 250    
      IA=ISTRT(2)-1+I   
C      DON'T WRITE IF I EXCEEDS TABLE 2 LENGTH 
      IF(I.GT.ILTH(2)) THEN 
          ICK(2)=0  
          GO TO 220 
         ELSE
          ICK(2)=1  
          IJ=IA 
      ENDIF  
c  218 FORMAT(23X,E10.4,2X,E10.4)
  220 IF(ISTRT(3).EQ.0) GOTO 250    
      IA=ISTRT(3)-1+I   
C      DON'T WRITE IF I EXCEEDS TABLE 3 LENGTH 
      IF(I.GT.ILTH(3)) THEN 
          ICK(3)=0  
          GO TO 230 
         ELSE
          ICK(3)=1  
          IK=IA 
      ENDIF  
c  228 FORMAT(49X,E10.4,2X,E10.4)
  230 CONTINUE  

C      LOGIC FOR PRINTING OUT TI,TD
      ITEST=ICK(1)+ICK(2)+ICK(3) 
      ISUM=0 
      IF (ITEST - 2) 301,303,309 
  301      DO 302 JJ=1,3
          ISUM=ISUM+ICK(JJ) 
          IF (ISUM .EQ. 1) THEN 
C    ONLY 1 TABLE TO PRINT 
              IF (JJ - 2) 311,312,313  
311               continue            
c    write(i2,208)TI(II),TD(II)
                  GO TO 330   
312               continue
c    write(i2,218)TI(IJ),TD(IJ)
                  GO TO 330   
313               continue
c    write(i2,228)TI(IK),TD(IK)
                  GO TO 330   
             ELSE   
                  CONTINUE
          ENDIF 
302       CONTINUE
C    TWO TABLES TO PRINT   
303   DO 307 KK=1,3   
          ISUM=ISUM+ICK(KK) 
          IF (ISUM - 1) 304,305,306 
C                PRINT 2ND AND 3RD TABLE VALUES
304           continue
c    write(i2,405)TI(IJ),TD(IJ),TI(IK),TD(IK)   
              GO TO 330
305           ISTOR1=KK 
              GO TO 307
306           ISTOR2=KK 
              GO TO 308
307   CONTINUE
308   CONTINUE
      ISTOR=ISTOR1+ISTOR2
      IF (ISTOR .EQ. 3) THEN 
C          PRINT 1ST AND 2ND TABLE VALUES 
c        write(i2,404)TI(II),TD(II),TI(IJ),TD(IJ)   
          GO TO 330 
        ELSE 
C          PRINT 1ST AND 3RD TABLE VALUES 
c        write(i2,406)TI(II),TD(II),TI(IK),TD(IK)   
          GO TO 330 
      ENDIF  
C          PRINT ALL 3 TABLE VALUES   
  309 continue
c      write(i2,407)TI(II),TD(II),TI(IJ),TD(IJ),TI(IK),TD(IK) 
  330 CONTINUE  

c  404 FORMAT(1X,E10.4,2X,E10.4,2X,E10.4,2X,E10.4)   
c  405 FORMAT(25X,E10.4,2X,E10.4,2X,E10.4,2X,E10.4)  
c  406 FORMAT(1X,E10.4,2X,E10.4,26X,E10.4,2X,E10.4)  
c  407 FORMAT(1X,E10.4,2X,E10.4,2X,E10.4,2X,E10.4,2X,E10.4,2X,E10.4) 

  250 CONTINUE  
      IF(ILEFT.LE.ITAB) RETURN  
      ILEFT=ILEFT-ITAB  
      GOTO 50   
  251 RETURN
      END   
