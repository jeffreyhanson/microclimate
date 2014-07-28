      FUNCTION DEXPI (X)  
      IMPLICIT NONE
C     Copyright Warren P. Porter 1997  All rights reserved
      
C     COMPUTATIONS OF EXPONENTIAL INTEGRAL WITH FIFTEEN SIGNIFICANT     
C     FIGURE ACCURACY.  
C     RANGE 1 GREATER THAN -1.0D-20. GAMMA + DLOG( ABS OF X )   
C             FOR NEGATIVE VALUES OF THE ARGUMENT $ 
C     RANGE 2 -1.0D-20 TO -1.5, 3 POINT GAUSSIAN QUADRATURE.
C     RANGE 3 -1.5 TO -4.65, RATIO OF 2 POLYNOMIALS EACH WITH 7 TERMS.  
C     RANGE 4 -4.65 TO -12.0, RATIO OF 2 POLYNOMIALS EACH WITH 6 TERMS. 
C     RANGE 5 -12.0 TO -170.0, 12 POINT GAUSS-LAGUERRE QUADRATURE.  
C                FOR POSITIVE VALUES OF THE ARGUMENT $  
C     RANGE 1 LESS THAN 1.0D-20 , GAMMA + DLOG(X)   
C     RANGE 2 1.0D-20 TO 40.0,12 POINT GAUSSIAN QUADRATURE. 
C     RANGE 3 40.0 TO 173.0,12 POINT GAUSS-LAGUERRE QUADRATURE. 
      DOUBLE PRECISION A1(3),B1(3),A2(7),B2(7),A3(6),B3(6),A4(12),B4(12)
     1,A5(12),B5(12),SUMN,SUMD,SUMT,YY,YZ,AX,X,DEXPI
      INTEGER J
 10   FORMAT( T10,' THE ARGUMENT OF DEXPI IS VERY CLOSE TO ZERO. IT     
     1 IS',D25.16//)    
 20   FORMAT( T10,' THE ARGUMENT OF DEXPI IS VERY LARGE. IT IS',D25.16//
     1)     
      DATA A1 / 0.1193095930415985D+0,  0.3306046932331323D+0,  
     1 0.4662347571015760D+0/   
      DATA B1/ 0.4679139345726910D+0,  0.3607615730481386D+0,   
     1  0.1713244923791703D+0/  
      DATA A2/ 0.2823912701457739D-1,  0.3052042817823067D+1,   
     1 0.2158885931211323D+2,  0.4104611319636983D+2,   
     2 0.2785527592726121D+2,  0.7133086969436196D+1,   
     3 0.5758931590224375D+0/   
      DATA B2/ 0.1000000000000000D+1,  0.1383869728490638D+2,   
     1 0.4880858183073600D+2,  0.6348804630786363D+2,   
     2 0.3441289899236299D+2,  0.7708964199043784D+1,   
     3 0.5758934565014882D+0/   
      DATA A3/ 0.7630772325814641D-1,  0.2123699219410890D+1,   
     1 0.4745350785776186D+1,  0.2966421696379266D+1,   
     2 0.6444800036068992D+0,  0.4295808082119383D-1/   
      DATA B3/ 0.1000000000000000D+1,  0.5278950049492932D+1,   
     1 0.7196111390658517D+1,  0.3567945294128107D+1,   
     2 0.6874380519301884D+0,  0.4295808112146861D-1/   
      DATA A4/ 0.1157221173580207D+0,  0.6117574845151307D+0,   
     1 0.1512610269776419D+1,  0.2833751337743507D+1,   
     2 0.4599227639418348D+1,  0.6844525453115177D+1,   
     3 0.9621316842456867D+1,  0.1300605499330635D+2,   
     4 0.1711685518746226D+2,  0.2215109037939701D+2,   
     5 0.2848796725098400D+2,  0.3709912104446692D+2/   
      DATA B4/ 0.2647313710554432D+0,  0.3777592758731380D+0,   
     1 0.2440820113198776D+0,  0.9044922221168093D-1,   
     2 0.2010238115463410D-1,  0.2663973541865316D-2,   
     3 0.2032315926629994D-3,  0.8365055856819799D-5,   
     4 0.1668493876540910D-6,  0.1342391030515004D-8,   
     5 0.3061601635035021D-11,  0.8148077467426242D-15/     
      DATA A5 / 0.3202844643130281D-1,  0.9555943373680816D-1,  
     1 0.1575213398480817D+0,  0.2168967538130226D+0,   
     2 0.2727107356944198D+0,  0.3240468259684878D+0,   
     3 0.3700620957892772D+0,  0.4100009929869515D+0,   
     4 0.4432077635022005D+0,  0.4691372760013664D+0,   
     5 0.4873642779856547D+0,  0.4975936099985107D+0/   
      DATA B5/ 0.1279381953467522D+0,  0.1258374563468283D+0,   
     1 0.1216704729278034D+0,  0.1155056680537256D+0,   
     2 0.1074442701159656D+0,  0.9761865210411389D-1,   
     3 0.8619016153195328D-1,  0.7334648141108031D-1,   
     4 0.5929858491543678D-1,  0.4427743881741981D-1,   
     5 0.2853138862893366D-1,  0.1234122979998720D-1/   
      DEXPI=0.0D0   
      IF(X)200,100,300  
 100  WRITE(6,10)X  
      STOP  
 200  AX=DABS(X)
      IF(X.GT.-1.0D-20 )GO TO 201   
      IF(X.GT.-1.5D0)GO TO 205  
      IF(X.GT.-4.65D0)GO TO 215     
      IF(X.GT.-12.0D0)GO TO 225     
      IF(X.GT.-170.0D0)GO TO 235    
      RETURN
 201  DEXPI=DLOG(AX)+0.57721566490153286
      RETURN
 205  YY=DEXP(-0.5D0*AX)
      YZ=DEXP(A1(1)*AX) 
      SUMN=(1.0D0-YY/YZ)/(0.5D0+A1(1))+(1.0D0-YY*YZ)/(0.5D0-A1(1))  
      YZ=DEXP(A1(2)*AX) 
      SUMD=(1.0D0-YY/YZ)/(0.5D0+A1(2))+(1.0D0-YY*YZ)/(0.5D0-A1(2))  
      YZ=DEXP(A1(3)*AX) 
      SUMT=(1.0D0-YY/YZ)/(0.5D0+A1(3))+(1.0D0-YY*YZ)/(0.5D0-A1(3))  
      DEXPI=-0.5D0*(B1(1)*SUMN+B1(2)*SUMD+B1(3)*SUMT)   
      DEXPI=DEXPI+DLOG(AX)+0.57721566490153286  
      RETURN
 215  SUMN=(((((A2(7)*AX+A2(6))*AX+A2(5))*AX+A2(4))*AX+A2(3))*AX+A2(2)) 
     1*AX+A2(1) 
      SUMD=(((((B2(7)*AX+B2(6))*AX+B2(5))*AX+B2(4))*AX+B2(3))*AX+B2(2)) 
     1*AX+B2(1) 
 218  DEXPI=SUMN/(SUMD*X)   
      DEXPI=DEXPI*DEXP(X)   
      RETURN
 225  SUMN=((((A3(6)*AX+A3(5))*AX+A3(4))*AX+A3(3))*AX+A3(2))*AX+A3(1)   
      SUMD=((((B3(6)*AX+B3(5))*AX+B3(4))*AX+B3(3))*AX+B3(2))*AX+B3(1)   
      GO TO 218 
 235  DO 238 J=1,12     
 238  DEXPI = DEXPI + B4(J)/(1.0D0 + A4(J)/AX)  
      DEXPI= ( DEXP(X)/AX)*(-DEXPI) 
      RETURN
 300  IF(X.LE.1.0D-20)GO TO 301     
      IF(X.LE.40.0D0)GO TO 305  
      IF(X.LE.173.0D0)GO TO 335     
      WRITE(6,20)X  
      STOP  
 301  DEXPI=DLOG(X)+0.57721566490153286 
      RETURN
 305  YY=DEXP(0.5D0*X)  
      DO 310 J=1,12     
      YZ=DEXP(-A5(J)*X) 
      DEXPI=((1.0D0-YY/YZ)/(0.5D0+A5(J))+(1.0D0-YY*YZ)/(0.5D0-A5(J)))*  
     1B5(J)+DEXPI   
 310  CONTINUE  
      DEXPI=-0.5D0*DEXPI+DLOG(X)+0.57721566490153286
      RETURN
 335  DO 338 J=1,12     
 338  DEXPI=DEXPI+B4(J)/(1.0D0-A4(J)/X) 
      DEXPI=(DEXP(X)/X)*DEXPI   
      RETURN
      END   
