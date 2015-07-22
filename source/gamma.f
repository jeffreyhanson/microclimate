      SUBROUTINE GAMMA(TAU1,GAMR,GAML,SBAR)
      IMPLICIT NONE
      external INITDATA
C     Copyright Warren P. Porter 1997  All rights reserved.      
           
C     COMPUTATIONS OF THE RADIATION SCATTERED BY A PLANE PARALLEL   
C     HOMOGENEOUS ATMOSPHERE USING THE METHOD OF THE X AND Y FUNCTIONS. 
C     ALL COMPUTATI6NS ARE PERFORMED IN DOUBLE PRECISION.....   
C     NPNCH SHOULD BE NON-ZERO IF THE SCATTERING FUNCTIONS ARE  
C     REQUIRED 
c   10 FORMAT(D15.5,I5)  
c   20 FORMAT(1H1)   
c   30 FORMAT(//T5,'SCATTERING FUNCTIONS FOR THE AZIMUTH INDEPENDENT TERM
c     1 FOR TAU SUB   = ',F10.5//)   
c   40 FORMAT(T5,'NU 1 =',F12.7,T25,'NU 2 =',F12.7,T45,'NU 3 =',F12.7,   
c     1T65,'NU 4 =',F12.7,T85,' U 3 =',F12.7,T105,'U 4 =',F12.7//)   
c   50 FORMAT(T5,' Q = ',F12.7,T25,'S BAR = ',F12.7//)   
c   60 FORMAT(T8,'AMU',T18,'PSI',T32,'PHI',T46,'CHI',T60,'ZETA',T74,     
c     1'XI',T88,'ETA',T102,'SIGMA',T116,'THETA'//)   
c   70 FORMAT(0PF10.2,1P8E14.5)  
c   80 FORMAT(//T5,'SCATTERING FUNCTIONS FOR THE AZIMUTH DEPENDENT TERMS 
c     1 +  GROUND REFLECTION EFFECT TERMS FOR TAU 1 =',F10.5//)  
c   90 FORMAT(T8,'AMU',T18,'X SUB 1',T32,'Y SUB 1',T46,'X SUB 2',T60,    
c     1'Y SUB 2',T74,'GAMA SUB L',T88,' GAMA SUB R'//)   
c  100 FORMAT(0PF10.2,1P6E14.5)  
c  150 FORMAT (2I5,2D12.4,I5,2D12.4,I5)  
c  300 FORMAT(T2,'TAU SUB 1 =',F10.5,T30,'CON.S BAR =',F14.8)
c  310 FORMAT(  5F12.8,  F5.2,  F7.4,T73,'  PSI   ') 
c  320 FORMAT(  5F12.8,  F5.2,  F7.4,T73,'  PHI   ') 
c  330 FORMAT(  5F12.8,  F5.2,  F7.4,T73,'  CHI   ') 
c  340 FORMAT(  5F12.8,  F5.2,  F7.4,T73,'  ZETA  ') 
c  350 FORMAT(  5F12.8,  F5.2,  F7.4,T73,'  XI    ') 
c  360 FORMAT(  5F12.8,  F5.2,  F7.4,T73,'  ETA   ') 
c  370 FORMAT(  5F12.8,  F5.2,  F7.4,T73,'  SIGMA ') 
c  380 FORMAT(  5F12.8,  F5.2,  F7.4,T73,' THETA  ') 
c  390 FORMAT(  5F12.8,  F5.2,  F7.4,T73,' X SUB 1') 
c  400 FORMAT(  5F12.8,  F5.2,  F7.4,T73,'Y SUB 1 ') 
c  410 FORMAT(  5F12.8,  F5.2,  F7.4,T73,'X SUB 2 ') 
c  420 FORMAT(  5F12.8,  F5.2,  F7.4,T73,'Y SUB 2 ') 
c  430 FORMAT(  5F12.8,  F5.2,  F7.4,T73,' GAMA L ') 
c  440 FORMAT(  5F12.8,  F5.2,  F7.4,T73,' GAMA R ') 
      REAL * 8 CNU1,CNU2,CNU3,CNU4,CU3,CU4,SBAR,
     1TAU1,CHX(101),CHY(101),CFA(3),AMU(101),GAML(101),GAMR(101),   
     2X1(101),Y1(101),X2(101),Y2(101),AIL(101),AI(101),XA(101),XB(101) 
      INTEGER I,NST,NTR
      AMU(1) = 0.0D0    
      DO 500 I = 2,101  
      AMU(I) = 0.01D0 * ( I-1)  
  500 CONTINUE  
C     COMPUTATIONS OF X SUB L,Y SUB L, X SUB R , Y SUB R .......
      CFA(1) = 0.75D0   
      CFA(2) = -0.75D0  
      CFA(3) = 0.0D0    
      NST = 111 
      CALL DCHXY(TAU1,CFA,NST,CHX,CHY,NTR)  
      DO 520 I = 1,101  
      X1(I) = CHX(I)    
      Y1(I) = CHY(I)    
  520 CONTINUE  
      CFA(1) = 0.375D0  
      CFA(2) = -0.375D0 
      NST = 0   
      CALL DCHXY(TAU1,CFA,NST,CHX,CHY,NTR)  
      DO 540 I=1,101    
      X2(I) = CHX(I)    
      Y2(I) = CHY(I)    
  540 CONTINUE  
C     COMPUTATIONS OF THE MOMENTS AND THE CONSTANTS....     
      AIL(1) = 0.01D0/3.0D0 
      CNU1 = 4.0D0 * AIL(1) 
      CNU2 = 2.0D0 * AIL(1) 
      DO 580 I = 2,100,2
      AIL(I) = CNU1     
      AIL(I+1) = CNU2   
  580 CONTINUE  
      AIL(101) = AIL(1) 
      DO 590 I = 1,101  
      XA(I) = 0.0D0     
      XB(I) = 0.0D0     
  590 CONTINUE  
      DO 630 I = 1,101  
      CNU1 = AIL(I) * X1(I) * AMU(I)
      XA(1) = XA(1) + CNU1  
      XA(2) = XA(2) + CNU1 * AMU(I) 
      CNU1 = AIL(I) * Y1(I) * AMU(I)
      XA(3) = XA(3) + CNU1  
      XA(4) = XA(4) + CNU1 * AMU(I) 
      CNU1 = AIL(I) * X2(I) 
      XB(1) = XB(1) + CNU1  
      CNU1 = CNU1 * AMU(I)  
      XB(2) = XB(2) + CNU1  
      CNU1 = CNU1 * AMU(I)  
      XB(3) = XB(3) + CNU1  
      XB(4) = XB(4) + CNU1 * AMU(I) 
      CNU1 = AIL(I) * Y2(I) 
      XB(5) = XB(5) + CNU1  
      CNU1 = CNU1 * AMU(I)  
      XB(6) = XB(6) + CNU1  
      CNU1 = CNU1 * AMU(I)  
      XB(7) = XB(7) + CNU1  
      XB(8) = XB(8) + CNU1 * AMU(I) 
  630 CONTINUE  
      AI(1) = XB(1) + XB(5) - 8.0D0/3.0D0   
      AI(2) = XB(2) + XB(6) 
      AI(3) = XB(3) + XB(7) 
      AI(4) = XB(1) - XB(5) - 8.0D0/3.0D0   
      AI(5) = XB(2) - XB(6) 
      AI(6) = XB(3) - XB(7) 
      AI(7) = XB(4) - XB(8) 
      AI(8) = XA(1) + XA(3) 
      AI(9) = XA(2) + XA(4) 
      AI(10) = XA(1) - XA(3)
      AI(11) = XA(2) - XA(4)
      AI(12) = (AI(1)-AI(3))/((AI(4)-AI(6))*TAU1+2.0D0*(AI(5)-AI(7)))   
      AI(13)=1.0D0/(AI(4)*AI(10) - AI(5) * AI(11))  
      AI(14)=1.0D0/(AI(1)*AI(8)-AI(2)*AI(9)-2.0D0*AI(12)*(AI(5)*AI(8)-  
     1AI(4)*AI(9)))     
      AI(15)=2.0D0 * (AI(8) * AI(10) - AI(9) *AI(11))   
      AI(16) = AI(13) * AI(15)  
      AI(17) = AI(14) * AI(15)  
      CNU1 = 0.5D0*(AI(16) - AI(17))
      CNU2 = 0.5D0*( AI(16) + AI(17))   
      AI(15) = AI(13)*(AI(5)*AI(8)-AI(4)*AI(9)) 
      AI(16)=AI(14)*(AI(2)*AI(10)-AI(1)*AI(11)-2.0D0*AI(12)*(AI(4)*AI(10
     1)-AI(5)*AI(11)))  
      CNU3 = 0.5D0*(AI(15)-AI(16))  
      CNU4 = 0.5D0*(AI(15)+AI(16))  
      AI(15) = AI(13)*(AI(2)*AI(10)-AI(1)*AI(11))   
      AI(16) = AI(14)*(AI(5)*AI(8)-AI(4)*AI(9)) 
      CU3 = 0.5D0*(AI(15)-AI(16))   
      CU4 = 0.5D0*(AI(15)+AI(16))   
      AI(15) = AI(14)*(AI(1)*AI(8)-AI(2)*AI(9)) 
      SBAR=1.0D0-0.375D0 * AI(12) * (AI(4)-AI(6)) * 
     1((CNU2 - CNU1 ) * AI(8) + (CU4 - CU3 ) *AI(2) -AI(15) * AI(6))    
      AI(20)=0.375D0*AI(12)*(CNU2-CNU1)*(AI(4)-AI(6))   
      AI(21) =0.375D0 * AI(12)*(AI(4)-AI(6))    
      AI(22) = AI(21) * ( CU4 - CU3 )   
      AI(23) = AI(21)*AI(15)
      DO 680 I = 1,101  
      GAML(I) = AI(20)*(X1(I) + Y1(I))  
      GAMR(I) =AI(22)*(X2(I) + Y2(I)) -AMU(I)*AI(23)*(X2(I)-Y2(I))  
  680 CONTINUE  
      RETURN
      END   