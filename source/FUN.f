      FUNCTION FUN (X)  
C      Estimating Cosine of zenith angle to get air mass before sunrise
C     VERSION 13 May 1998
C     Copyright 1998 W.P. PORTER, All rights reserved.
      IMPLICIT NONE
      REAL  Airms,AirmSR,Avemas,CZ,Y,X,Fun

C    Bringing in prior hour's air mass           
      Common/wfun/airms,cz

      X=CZ
             
C    Air mass at sunrise
      AirmSR = 10.0 
      Avemas = (Airms  + AirmSR)/2.

C     Air mass equation
      Y = Avemas - 1.0/(CZ+(0.025*EXP(-11.*CZ)))                 
      FUN=Y 
    
      RETURN
      END   

