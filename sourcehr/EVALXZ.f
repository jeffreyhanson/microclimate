      SUBROUTINE EVALXZ
      IMPLICIT NONE
C     COPYRIGHT 1997  WARREN P. PORTER,  ALL RIGHTS RESERVED
C     SUBROUTINE EVAL EVALUATES THE VALUE OF THE FUNCTION DTEMP/DTIME   
C     FORM THE RUNGA KUTTA ALOGRITHM FOR SOLVING ORDINARY DIFFERENTIAL  
C     EQUATIONS SUBROUTINES TITLED DSUB IS CONSULTED.  THE MAXIMUM     
C     NUMBER OF DIFFERENTIAL EQUATIONS ALLOWED IS 40.   
      DIMENSION YII(40) 

      REAL G,H,X,Y,YC,YII,XMAX,ERR,DX,YP,YI,TNEW,TERR,ERROR,f,dummy  
      INTEGER I,N,NN,NFACTR,NCOND,IPRINT

      COMMON/NONSCR/N,NN,X,XMAX,DX,ERR,H,NCOND,NFACTR,IPRINT
      COMMON/WORK/ERROR(40),TERR(40),TNEW(40),YI(40),YP(40),YC(40),     
     1 F(6,40),G(40),Y(40)  
     2  ,DUMMY(1160)    
C          SAVE THE INITIAL VALUES READ INTO EVAL   
      DO 5 I=1,N
    5  YII(I)=Y(I)  
C          FIND THE DERVATIVEA AT XOAND T(I)    
      CALL DSUB(X,Y,G)  
      DO 10 I=1,N   
      YC(I) = Y(I) +(H*G(I))/6.0    
   10 Y(I) = YII(I) + (H*G(I))/2.0  
      X =X +H/2.0   
      CALL DSUB(X,Y,G)  
      DO 11 I=1,N   
      YC(I)= YC(I)+(H*G(I))/3.0     
   11  Y(I) = YII(I) + (H*G(I))/2.0 
      CALL DSUB(X,Y,G)  
      DO 12 I=1,N   
      YC(I) = YC(I)+(H*G(I))/3.0    
   12  Y(I) = YII(I) +  H*G(I)  
      X =X +H/2.0   
      CALL DSUB(X,Y,G)  
      DO 13 I=1,N   
   13 YC(I) = YC(I) + (H*G(I))/6.0  
      CALL DSUB(X,YC,G) 
      RETURN
      END