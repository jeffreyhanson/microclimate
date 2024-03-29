      SUBROUTINE RDCTRL(INPUT,M,MA,RWORK)   
      IMPLICIT NONE
C    COPYRIGHT 1997  WARREN P. PORTER,  ALL RIGHTS RESERVED

      INTEGER I,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12
      Integer IA,IB,IC,ICOUNT,M   
      CHARACTER*1 BLANK,COMMA   
      CHARACTER*3 INPUT,MA  
      CHARACTER*80 RWORK

      COMMON/WMAIN/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12 

C    INPUT = NAME FOR TYPE OF INPUT, E.G. PAR (PARAMETER), 
C            OR TAB (TABLE)
C    M     = NUMBER OF PAIRS OF TABLE VARIABLES, IF 'TAB' IS FOUND,
C               OR THE NUMBER OF PARAMETER VALUES, IF 'PAR' IS FOUND
C    MA    = TABLE NAME FOR TYPE OF TIME DEPENDENT VARIABLE, E.G.
C            'SOL' FOR SOLAR, 'TAR' FOR AIR TEMPERATURE, ETC.
C    RWORK = 80 CHARACTER ARRAY (ONE LINE) TO BE SEARCHED FOR THE
C            3 VARIABLES ABOVE

      DATA BLANK,COMMA/' ',','/     
c    2 READ(I4,1) RWORK       
c    1 FORMAT(A80)   
C   
C    VERSION 31 AUGUST 1984  W.P. PORTER   

C    RDCTRL HAS BEEN REDONE TO USE CHARACTER SUBSTRINGS
C    RWORK HAS BEEN DIMENSIONED TO 80 CHARACTERS (A LINE OF INPUT) 
C    THIS MAKES SUBSTRING MANIPULATION EASY AND MAKES THIS FORM OF 
C    CHARACTER MANIPULATION TRANSPORTABLE FROM ONE MACHINE TO ANOTHER  
C    (AS LONG AS BOTH MACHINES ARE USING FORTRAN 77 (ASCII FORTRAN).   
C    THE FORM OF THE SUBSTRING NAME IS 
C        CVAR(START : FINISH)  
C    THUS THE LINE OF 80 CHARACTERS, RWORK, CAN HAVE ANY SUBSTRING 
C    IN THAT LINE IDENTIFIED, E.G. RWORK(1:1) IS THE FIRST CHARACTER   
C    IN THE LINE (OR) RWORK(1:3) ARE THE FIRST THREE CHARACTERS IN THE LINE

      DO 5 I=1,80   
        IF(RWORK(I:I) .EQ. COMMA) THEN  
          RWORK(I:I) = BLANK
        ENDIF  
5     CONTINUE  

      M=0   
      ICOUNT=0  
C     FIND THE FIRST BLANK   
      DO 10 I=1,80     
        IA=I   
        IF(ICOUNT .EQ. 0 .AND. RWORK(I:I) .NE. BLANK) THEN 
          GO TO 11  
         ELSE  
        ENDIF 
   10 CONTINUE  
c      GOTO 2
   11 ICOUNT=1  
      IB=IA+2   
      INPUT = RWORK(IA:IA+2)   

C     FIND THE NEXT NON-BLANK    
      IB=IB+1   
      DO 20 I=IB,80     
        IA=I  
        IF(RWORK(I:I).NE.BLANK) GOTO 21   
   20 CONTINUE  
C     ONLY A PARAMETER NAME WAS FOUND ABOVE, NOT A TABLE    
      RETURN
C    ************************************************************  

C     CONTINUING WITH ARRAY INFORMATION (NUMBER OF ENTRIES, NAME OF ARRAY)  
21    DO 25 I=IA,80 
      IB=I  
      IF(RWORK(I:I).EQ.BLANK) GOTO 26   
   25 CONTINUE  
   26 IB=IB-1   
        M = 0   
C     ICHAR CONVERTS FROM CHARACTER TO OCTAL ONLY, IN FORTRAN 77 (DEC '83)  
C     TO CONVERT FROM OCTAL TO INTEGER SUBTRACT 48 (ICHAR('0')) 
C     SO THE FOLLOWING STATEMENT COULD READ M=ICHAR(RWORK(IA))-ICHAR('0')   
        M = ICHAR(RWORK(IA:IA))-48 
c        write(*,*) M
        IC = IA + 1     
        DO 27 I = IC,IB 
            IF (RWORK(I:I) .EQ. BLANK) GO TO 27     
C     MAKING POWERS OF TEN ADJUSTMENT AS MORE DIGITS ARE FOUND FOR M    
            M = ICHAR(RWORK(I:I))-48 + M*10  
c            write(*,*) M
   27   CONTINUE
      IB=IB+1   
C         FIND THE NEXT NON-BLANK   
      DO 30 I=IB,80     
       IA=I 
      IF(RWORK(I:I).NE.BLANK) GOTO 31   
   30 CONTINUE  
C    NUMBER OF ENTRIES OF VARIABLE FOUND   
      RETURN
C    ************************************************************  

C       CHECK FOR THREE CHARACTERS (ESTABLISH TABLE NAME)   
   31 IB=IA+2   
      IF(RWORK(I:I).EQ.BLANK) GOTO 33   
c   32 CONTINUE  
       MA = RWORK(IA:IA+2)  
      RETURN

   33 WRITE(6,34) RWORK     
   34 FORMAT('0****INCORRECT FORMAT ENCOUNTERED ON FOLLOWING COMMAND CAR
     1D -- ',A80,/'     PROGRAM TERMINATED.****')   
      STOP  
      END   
