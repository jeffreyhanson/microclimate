      SUBROUTINE infil(ha,moistt,ET,temp,depth,fl,sw,humid,potent,dt)
      
C     Computes water infiltration and redistribution with evaporation, from 
c     a bare soil surface, based on program 9.1 of Campbell 1985         

C     27th Jan 2015
      IMPLICIT NONE
      external INITDATA
      REAL A,B,C,F,P,Z,V,DP,W,WN,K,CP,KS,PE,BB,BD,WS,B1,N,N1
      REAL WD,GR,IM,SE,SW,FL,moistt,H,JV,DJ,PP,EP,MW,T,R,HA,DV
     &,VP,KV,temp,depth,humid,potent
      REAL RR,L,RS,PR,BZ,RW,PC,RL,PI,SP,R1,ET,TP,PB,RB,LAI
      double precision SL,PL,FF,E,XP,TR
      Integer M,I,count,maxcount,j,DT
      
      DIMENSION A(19),B(19),C(19),F(19),P(19),Z(19),V(19),DP(19),W(19)
      DIMENSION WN(19),K(19),CP(19),H(19),JV(19),DJ(19),temp(10)
     &,moistt(18),T(19),depth(10),humid(18),potent(18),PE(19),
     &KS(19),BB(19),PP(19),B1(19),N(19),N1(19),WS(19),DV(19)
      DIMENSION RR(19),L(19),E(19),RS(19),PR(19),BZ(19),BD(19)
      common/campbell/PE,KS,BB,BD,L,LAI

c      P matric potential J/kg
c      Z depth nodes
c      W water content m3/m3
c      WN water content m3/m3
c      K hydraulic conductivity, kg s/m3
c      M number of elements   
c      BD soil bulk density, Mg/m3 
c      KS saturated conductivity, kg s/m3
c      PE air entry potential J/kg     
c      BB soil 'b' parameter  
      A(1:19)=0
      B=A
      C=A
      F=A
      P=A
      Z=A
      V=A
      DP=A
      W=A
      WN=A
      K=A
      CP=A
      H=A
      JV=A
      DJ=A
      B1=A
      N=A
      WS=A
      DV=A
      RR=A
c     L=A
      E=A
      RS=A
      PR=A
      BZ=A
      E=A
      M=18 
c      PE(1:19)=1.5
c      KS(1:19)=0.00072
c      BB(1:19)=3.1
c      PE(12:19)=2.6
c      KS(12:19)=0.00064
c      BB(12:19)=5.2
c      PE(1:19)=PE1
c      KS(1:19)=KS1
c      BB(1:19)=BB1
c     # air entry potential J/kg      
      PE=ABS(PE)*(-1)
c     initial water potential J/kg 
      PP=PE
      PP=ABS(PP)*(-1)
c     # saturation water content m3/m3, appears to assume max density of 2.6 Mg/m3      
      WS=1-BD/2.6 
c     # depth to lower boundary, m      
      Z(M+1)=2 
c     # time step, s      
c      DT=60*60.
      B1=1/BB
      N=2+3/BB
      N1=1-N
      WD=1000.
c     # molar mass of water, kg/mol      
      MW=0.018
c     # temperature of soil, K      
c      T=temp+273
      j=2
      do 121 I=3,18
       if(MOD(I, 2).ne.0)then
           Z(I)=depth(j)/100
           j=j+1
       else
c           Z(I)=Z(I-1)+0.005
           Z(I)=Z(I-1)+(depth(J)/100-Z(I-1))/2
       endif
121   continue
      j=1
      do 120 I=1,19
       if(MOD(I, 2).ne.0)then
           T(I)=temp(j)
           j=j+1
       else
c           T(I)=T(I-1)
           T(I)=T(I-1)+(temp(j)-T(I-1))/2
       endif
120   continue  
      T=T+273
c     # gas constant, J/mol/K      
      R=8.310001
      Z(2)=0
      Z(1)=0

      do 2 I=2,M
c     # setting initial water content, m3/m3      
      WN(I)=moistt(i-1)
c      if(WN(I).lt.0.0000001)then
c          WN(I)=0.0000001
c      endif
      P(I)=PE(I)*(WS(I)/WN(I))**BB(I) 
      H(I)=exp(MW*P(I)/(R*T(I-1)))
      K(I)=KS(I)*(PE(I)/P(I))**N(I)
      W(I)=WN(I)
2     continue

c      Z(3:10)=depth(2:9)/100
      do 22 I=2,M
          V(I)=WD*(Z(I+1)-Z(I-1))/2
22    continue      
c     # lower boundary potential set to saturated (stays constant)
      P(M+1)=PE(I)*(WS(M+I)/WS(M+I))**BB(I) 
      H(M+1)=1
      W(M+1)=WS(M+1)
      WN(M+1)=WS(M+1)
      Z(1)=-1E10
      Z(M+1)=1E20

c     # lower boundary conductivity setting (stays constant)      
      K(M+1)=KS(I)*(PE(I)/P(M+1))**N(M+1) 
      
c     Start subroutine to initialize root water uptake variables
      RW=2.5E+10
      PC=-1500.
      RL=2000000.
      PI=3.14159
      SP=10.
      R1=0.001
c      DATA L/0,4,4,4,1.9,1.9,0.8,0.8,0.8,0.4,0.4,0.4,0.2,0.2,0.1,0.1
c     & ,0.,0./
c      L(1:2)=0.
c      L(3:5)=4.
c      L(6:7)=1.9
c      L(8:9)=0.8
c      L(10:12)=0.4
c      L(13:14)=0.2
c      L(15:16)=0.1
c      L(17:19)=0.

c     rooting density (m/m3)      
C     L(1:2)=0.
C     L(3:11)=4.
C     L(12)=1.83
C     L(13)=0.946
C     L(14)=0.635
C     L(15)=0.804
C     L(16)=0.435
C     L(17)=0.366
C     L(18:19)=0.
C     L=10000*L
      do 98 I=2,M
          if(L(I).gt.0)then
           RR(I)=2*RW/(L(I)*(Z(I+1)-Z(I-1)))
           BZ(I)=(1-M)*LOG(PI*R1*R1*L(I))/(2*PI*L(I)*(Z(I+1)-Z(I-1)))
          else
           RR(I)=1E+20
           BZ(I)=0.
          endif
98      continue
c     End subroutine to initialize root water uptake variables
            
      
c     # gravitational constant      
      GR=9.8 
c     # maximum overall mass balance error allowed      
      IM=0.000001
      DV=0.000024
      VP=0.017
      P(1)=P(2)
      K(1)=0 
     
c     Start evapotranspiration subroutine
c      EP=0.1*ET
c      LAI=0
      EP=exp(-0.82*LAI)*ET
      TP=ET-EP
c     End of evapotranspiration subroutine

c     Start of plant water uptake subroutine
      PB=0.
      RB=0.
      PL=0.
      do 99 i=2,M
          RS(I)=BZ(I)/K(I)
          PB=PB+P(I)/(RR(I)+RS(I))
          RB=RB+1/(RS(I)+RR(I))
99      continue
      PB=PB/RB
      RB=1/RB
      maxcount=500
      count=0
2080  continue      
      IF(PL.gt.PB)then
          PL=PB-TP*(RL+RB)
      ENDIF
      XP=(PL/PC)**SP
      SL=TP*(RL+RB)*SP*XP/(PL*(1+XP)*(1+XP))-1.
      FF=PB-PL-TP*(RL+RB)/(1+XP)
      PL=PL-FF/SL
      count=count+1
      if((ABS(FF).gt.10).and.(count.lt.maxcount))then
          goto 2080
      endif
      TR=TP/(1+XP)
      do 100 I=2,M
          E(I)=(P(I)-PL-RL*TR)/(RR(I)+RS(I))
C          if(e(I).gt.0)then
C              E(I)=0
C          endif
100      continue
      count=0
c     start of convergence loop 
11    SE=0
      maxcount=500
      count=count+1
      do 3 I=2,M
c     # conductivities for each node          
      K(I)=KS(I)*(PE(I)/P(I))**N(I)
3     continue
      JV(1)=EP*(H(2)-HA)/(1-HA)
      DJ(1)=EP*MW*H(2)/(R*T(I-1)*(1-HA))
      do 4 I=2,M
      KV=0.66*DV(I)*VP*(WS(I)-(WN(I)+WN(I+1))/2)/(Z(I+1)-Z(I))
      JV(I)=KV*(H(I+1)-H(I))
      DJ(I)=MW*H(I)*KV/(R*T(I-1))    
      CP(I)=-1*V(I)*WN(I)/(BB(I)*P(I)*real(DT,4))
c     # Jacobian components     
      A(I)=-1*K(I-1)/(Z(I)-Z(I-1))+GR*N(I)*K(I-1)/P(I-1) 
      C(I)=-1*K(I+1)/(Z(I+1)-Z(I)) 
      B(I)=K(I)/(Z(I)-Z(I-1))+K(I)/(Z(I+1)-Z(I))+CP(I)-GR*N(I)*K(I)/P(I)
     & +DJ(I-1)+DJ(I)
c     # mass balance      
      F(I)=((P(I)*K(I)-P(I-1)*K(I-1))/(Z(I)-Z(I-1))-(P(I+1)*K(I+1)-P(I)
     & *K(I))/(Z(I+1)-Z(I)))/N1(I)+V(I)*(WN(I)-W(I))/real(DT,4)-GR*(K(I-
     &1)-K(I))+JV(I-1)-JV(I)+ real(E(I),4)
c       if(W(2).ge.WS)then
c        if(I.gt.2)then
c       SE=SE+abs(F(I))
c        endif
c       else
       SE=SE+abs(F(I))
c       endif
4     continue
      
c     this ensures that water potential at the second node stays constant
c     during infiltration (Campbell 1985 p. 85, para 1)
c      if(W(2).ge.WS)then
c     F(2)=0
c     C(2)=0
c      endif
      
c     # Thomas algorithm (Gauss elimination)     
      do 5 I=2,(M-1)
      C(I)=C(I)/B(I)
      F(I)=F(I)/B(I)
      B(I+1)=B(I+1)-A(I+1)*C(I)
      F(I+1)=F(I+1)-A(I+1)*F(I)
5     continue

      DP(M)=F(M)/B(M)
      P(M)=P(M)-DP(M)
      if(P(M).gt.PE(M))then
      P(M)=PE(M)
      endif
      
      do 6 I=(M-1),2,-1
      DP(I)=F(I)-C(I)*DP(I+1)
c     # matric potential J/kg      
      P(I)=P(I)-DP(I) 
c     # check that water potential doesn't become too large      
       if(P(I).gt.PE(I))then 
        P(I)=(P(I)+DP(I)+PE(I))/2
       endif
6     continue
      do 7 I=2,M
c     # new water balance at end of the time step          
      WN(I)=WS(I)*(PE(I)/P(I))**B1(I)
      H(I)=EXP(MW*P(I)/(R*T(I-1)))
7     continue   
      H(M+1)=H(M)

c     loop until convergence     
      if((SE.gt.IM).and.(count.lt.maxcount))then
       goto 11
      endif

c     flux into soil, mm/m2/s (kg/m2/s)
      SW=((P(2)*K(2)-P(3)*K(3))/(N1(2)*(Z(3)-Z(2)))+GR*K(2)+real(TR,4))*
     & real(DT,4)
c      SW=((P(2)*K(2)-P(3)*K(3))/(N1(2)*(Z(3)-Z(2)))+GR*K(2))*real(DT,4)
c      SW=0.
c      SW=V(2)*(WN(2)-W(3))

c      do 103 I=2,M
c          SW=SW+V(I)*(WN(I)-W(I))
c103   continue
c      SW=SW*real(DT,4)
      W=WN

      do 9 I=2,M+1
      moistt(I-1)=WN(I)
9     continue 

c     surface evaporation, mm/h 
C     if(H(2).gt.1)then
C         H(2)=1
C     endif
      FL=(EP*(H(2)-HA)/(1-HA))*real(DT,4)
      humid(1:18)=h(2:19)
      potent(1:18)=P(2:19)
      RETURN
      END   
  
