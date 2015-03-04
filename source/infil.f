      SUBROUTINE infil(ha,moistt,EP,temp,depth,fl,sw,humid,potent,dt)
      
C     Computes water infiltration and redistribution with evaporation, from 
c     a bare soil surface, based on program 9.1 of Campbell 1985         

C     27th Jan 2015
      IMPLICIT NONE

      REAL A,B,C,F,P,Z,V,DP,W,WN,K,CP,KS,PE,BB,BD,WS,DZ,DT,B1,N,N1
      REAL WD,GR,IM,SE,SW,FL,moistt,H,JV,DJ,PP,EP,MW,T,R,HA,DV
     &,VP,KV,temp,depth,humid,potent
      Integer M,X,I,AA,count,maxcount
      
      DIMENSION A(11),B(11),C(11),F(11),P(11),Z(11),V(11),DP(11),W(11)
      DIMENSION WN(11),K(11),CP(11),H(11),JV(11),DJ(11),temp(10)
     &,moistt(10),T(10),depth(10),humid(10),potent(10)
      common/campbell/PE,KS,BB,BD
      
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
      A(1:11)=0
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
      M=10 
      X=11 
      AA=0
      DO 1 I=1,M
       AA=AA+I*I
1     continue
c     # air entry potential J/kg      
      PE=ABS(PE)*(-1)
c     initial water potential J/kg 
      PP=PE 
      PP=ABS(PP)*(-1)
c     # saturation water content m3/m3, appears to assume max density of 2.6 Mg/m3      
      WS=1-BD/2.6 
c     # depth to lower boundary, m      
      Z(M+1)=2 
      DZ=Z(M+1)/AA
c     # time step, s      
c      DT=60*60.
      B1=1/BB
      N=2+3/BB
      N1=1-N
      WD=1000.
c     # molar mass of water, kg/mol      
      MW=0.018
c     # temperature of soil, K      
      T=temp+273
c     # gas constant, J/mol/K      
      R=8.310001
      Z(2)=0
      Z(1)=0
      do 2 I=2,M
c     # setting initial water content, m3/m3      
      WN(I)=moistt(i-1)
      if(WN(I).lt.0.0001)then
          WN(I)=0.0001
      endif
      P(I)=PE*(WS/WN(I))**BB 
      H(I)=exp(MW*P(I)/(R*T(I-1)))
      W(I)=WN(I)
2     continue

      Z(3:10)=depth(2:9)/100
      do 22 I=2,M
          V(I)=WD*(Z(I+1)-Z(I-1))/2
22    continue      
c     # lower boundary potential set to initial water potential (stays constant)
      P(M+1)=P(M) 
      H(M+1)=H(M)
      W(M+1)=WS
      WN(M+1)=WS
      Z(1)=-1E20
c      Z(M+1)=1E20

c     # lower boundary conductivity setting (stays constant)      
      K(M+1)=KS*(PE/P(M+1))**N 
c     # gravitational constant      
      GR=9.8 
c     # maximum overall mass balance error allowed      
      IM=0.000001
      DV=0.000024
      VP=0.017
c     # potential boundary set to air entry potential
      P(1)=P(2)
c     # potential boundary set to air entry potential      
c      P(1)=PE 
      K(1)=0 
      count=0
c     start of convergence loop 
11    SE=0
      maxcount=2000
      count=count+1
      do 3 I=2,M
c     # conductivities for each node          
      K(I)=KS*(PE/P(I))**N 
3     continue
      JV(1)=EP*(H(2)-HA)/(1-HA)
      DJ(1)=EP*MW*H(2)/(R*T(I-1)*(1-HA))
      do 4 I=2,M
      KV=0.66*DV*VP*(WS-(WN(I)+WN(I+1))/2)/(Z(I+1)-Z(I))
      JV(I)=KV*(H(I+1)-H(I))
      DJ(I)=MW*H(I)*KV/(R*T(I-1))    
      CP(I)=-1*V(I)*WN(I)/(BB*P(I)*DT)
c     # Jacobian components     
      A(I)=-1*K(I-1)/(Z(I)-Z(I-1))+GR*N*K(I-1)/P(I-1) 
      C(I)=-1*K(I+1)/(Z(I+1)-Z(I)) 
      B(I)=K(I)/(Z(I)-Z(I-1))+K(I)/(Z(I+1)-Z(I))+CP(I)-GR*N*K(I)/P(I)
     & +DJ(I-1)+DJ(I)
c     # mass balance      
      F(I)=((P(I)*K(I)-P(I-1)*K(I-1))/(Z(I)-Z(I-1))-(P(I+1)*K(I+1)-P(I)
     & *K(I))/(Z(I+1)-Z(I)))/N1+V(I)*(WN(I)-W(I))/DT-GR*(K(I-1)-K(I))
     &+JV(I-1)-JV(I)
       SE=SE+abs(F(I))
4     continue

c     # Thomas algorithm      
      do 5 I=2,(M-1)
      C(I)=C(I)/B(I)
      F(I)=F(I)/B(I)
      B(I+1)=B(I+1)-A(I+1)*C(I)
      F(I+1)=F(I+1)-A(I+1)*F(I)
5     continue

      DP(M)=F(M)/B(M)
      P(M)=P(M)-DP(M)
      if(P(M).gt.PE)then
      P(M)=PE
      endif
      
      do 6 I=(M-1),2,-1
      DP(I)=F(I)-C(I)*DP(I+1)
c     # matric potential J/kg      
      P(I)=P(I)-DP(I) 
c     # check that water potential doesn't become too large      
       if(P(I).gt.PE)then 
        P(I)=(P(I)+DP(I)+PE)/2
       endif
6     continue
      do 7 I=2,M
c     # new water balance at end of the time step          
      WN(I)=WS*(PE/P(I))**B1
      H(I)=EXP(MW*P(I)/(R*T(I-1)))
7     continue   
      H(M+1)=H(M)

c     loop until convergence     
      if((SE.gt.IM).and.(count.lt.maxcount))then
       goto 11
      endif
      
      W=WN

      do 9 I=2,M+1
      moistt(I-1)=WN(I)
9     continue 
c     flux into soil, mm/m2 (kg/m2)
      SW=(P(2)*K(2)-P(3)*K(3))/(N1*(Z(3)-Z(2)))+GR*K(2)
c     surface evaporation, mm/h      
      FL=EP*(H(2)-HA)/(1-HA)*dt
      humid(1:10)=h(2:11)
      potent(1:10)=P(2:11)
      RETURN
      END   
  
