      subroutine soilprops(TT,ALTT,soilprop,moistt,Thconduct,
     &    Density,Spheat)  

      IMPLICIT NONE
      EXTERNAL WETAIR 

C    Michael Kearney 2012
C     Computes variable thermal conductivity with temperature and water content
c     based on Campbell, G. S., J. D. J. Jungbauer, W. R. Bidlake, and R. D. Hungerford. 1994. 
c     Predicting the effect of temperature on soil thermal conductivity. Soil Science 158:307-313.

      real TT,ALTT,drydensity,bar,clay,moistt,Thconduct,Density,Spheat
      real p_a0,PATMOS,BP,T_K,k_a,k_w,k_m,p_a,hr,D_v0,rho_hat0,D_v
      real rho_hat,lambda
      real WB,RH,DP,ESAT,VD,RW,TVIR,TVINC,DENAIR,
     &      CP,WTRPOT,q
      real e_a,E,deltax,theta_0,phi_m,theta,phi_g,f_w,k_g,k_f,g_a,g_c
      real epsilon_g,epsilon_w,epsilon_m,bA,weA,wmA,e_a1,e_a2
      real soilprop,dens,spht,HTOFN

      REAL THCONDS,DENSITYS,SPHEATS,DENDAY,SPDAY,TKDAY,JULDAY
      
      INTEGER DAYCT,INTRVLS,I,J
      INTEGER JULNUM,KSOYL,MOY,NODES,Numtyps,Numint
      INTEGER NON
      INTEGER I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I91,I92,I93
     & ,I94,I95,I96 

C    Day's soil properties    
      DIMENSION DENDAY(10),SPDAY(10),TKDAY(10),KSOYL(10)    
      DIMENSION Nodes(10,7300),julday(7300)
      DIMENSION Intrvls(7300) 
      dimension soilprop(10,6),TT(10),moistt(10)
      DIMENSION Thconduct(10),Density(10),Spheat(10)
      dimension drydensity(10),clay(10),bar(10),k_m(10)
      dimension spht(10),dens(10)

      COMMON/SOYVAR1/Numtyps,Numint,Intrvls
      COMMON/SOYVAR2/Thconds,Densitys,Spheats,Nodes,KSOYL
      COMMON/SOYFILS/DENDAY,SPDAY,TKDAY
      COMMON/SOILND/NON
      COMMON/DAYJUL/JULDAY,JULNUM,MOY
      COMMON/WMAIN/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I91,I92,I93
     & ,I94,I95,I96 

      DATA DAYCT/1/
      HTOFN=333500

c    do 1 i=1,NON
c    TT(i)=20.
c1    continue    

      do 3 j=1,numtyps
      drydensity(j)=soilprop(j,1)
      bar(j)=soilprop(j,2)
      clay(j)=soilprop(j,3)
      k_m(j)=soilprop(j,4)
      spht(j)=soilprop(j,5)
      dens(j)=soilprop(j,6)
3     continue

      j=1
      do 2 i=1,NON

      if(i.ge.nodes(j,moy))then
      j=j+1
       if(j.gt.numtyps)then
        j=numtyps
       endif
      endif
c    don't make it volumetric, but rather mass-specific, so don't multiply by kg/m3 converters (constants from Campbell and Norman 1998, Table 8.2)
      if((tt(i).gt.-0.45).and.(tt(i).le.0.4))then
      Spheat(i)=drydensity(j)/dens(j)*spht(j)+bar(j)*moistt(i)*(4180.
     & +HTOFN)
       else
       Spheat(i)=drydensity(j)/dens(j)*spht(j)+bar(j)*moistt(i)*4180.
      endif
c    constants from Campbell and Norman 1998, Table 8.2
      Density(i)=bar(j)*moistt(i)*1000+drydensity(j)/dens(j)*
     &    dens(j)*1000 

c    # standard sea level air pressure, Pa
      p_a0=101325
      PATMOS=p_a0*((1.-(0.0065*ALTT/288.))**(1./0.190284)) 
      BP = PATMOS
      
c    # deg K
      T_K=TT(i)+273.15
c    # W/mC thermal conductitvity of dry air (equation 9 in Campbell et al. 1994)
      k_a=0.024+7.73e-5*TT(i)-2.6e-8*TT(i)**2 
c    # W/mC thermal conductitvity of water (equation 8 in Campbell et al. 1994)
      k_w=0.554+2.24e-3*TT(i)-9.87e-6*TT(i)**2 
c    # W/mC thermal conductitvity of minerals (value of 2.5 suggested from Campbell and Norman 1998, Table 8.2)
c      k_m= 2.5 


c    # air pressure
      p_a=BP
c    # relative humidity
      hr=1.0 
c    # vapour diffusivity in air (m2/s), standard value at 0 deg C and sea level pressure (Campbell et al. 1994)      
      D_v0=2.12e-5 
c    # molar density of air (mol/m3), standard value at 0 deg C and sea level pressure (Campbell et al. 1994)
      rho_hat0=44.65 
c    # temperature/pressure-corrected vapour diffusivity in air (m2/s) (p. 309 in Campbell et al. 1994)
      D_v=D_v0*(p_a0/p_a)*(T_K/273.15)**1.75 
c    # temperature/pressure-corrected molar density of air (mol/m3) (p. 309 in Campbell et al. 1994)
      rho_hat=rho_hat0*(p_a/p_a0)*(273.15/T_K)
c    # J/mol latent heat of vaporization (Cambell et al. 1994, p. 309)     
      lambda=45144-48*TT(i) 
c      # vapour pressure at three temps, from which slope of vapour pressure function is calculated, assuming near 100% RH

        RH = 99.
        WB = 0.
        DP = 999.
      
      CALL WETAIR (TT,WB,RH,DP,BP,E,ESAT,VD,RW,TVIR,TVINC,DENAIR,
     &      CP,WTRPOT)
      e_a=E
      CALL WETAIR (TT-1,WB,RH,DP,BP,E,ESAT,VD,RW,TVIR,TVINC,DENAIR,
     &      CP,WTRPOT)

      e_a1=E
      CALL WETAIR (TT+1,WB,RH,DP,BP,E,ESAT,VD,RW,TVIR,TVINC,DENAIR,
     &      CP,WTRPOT)
      e_a2=E

c    # slope of the vapour pressure function centred at focal temperature
      deltax=(e_a2-e_a1)/2 

c     # these could vary with soil texture but the relationship isn't strong
c    # power for liquid recirculation, mean in Table 2 of Campell et al. 1994, excluding peat moss value
c      q_0=4
c      q=q_0*(T_K/303.)**2.    
c     using a typical value for 'q' of 4 - program becomes unstable if this is temperature dependent
      q=4.

c    # mean in Table 2 of Campell et al. 1994, excluding peat moss value
      theta_0=0.162 

c    # volume fraction of minerals
      phi_m=drydensity(j)/dens(j) 
c    # volume fraction of water
      theta=moistt(i)*bar(j)

c     # # volume fraction of gas      
      phi_g=1-theta-phi_m
      if(phi_g.lt.0)then
       phi_g=0
      endif
c     # eq 8.17 Campbell and Norman
      f_w=1/(1+(theta/theta_0)**(-4.))
c     # eq 8.17 Campbell and Norman, using temperature-specific q
      f_w=1/(1+(theta/theta_0)**(-1.*q))
c    # eq 8.18 Campbell and Norman
      k_g=k_a+lambda*deltax*hr*f_w*rho_hat*D_v/(p_a-e_a) 
c     # eq 8.19 Campbell and Norman
      k_f=k_g+f_w*(k_w-k_g)
c    # 0.1 for mineral soils, 0.33 for organic, p 125, Campbell and Norman
      g_a=0.1 
c     # p 125, Campbell and Norman
      g_c=1-2*g_a
c     # equation 8.20 in Campbell and Norman 1988
      epsilon_g=2/(3*(1+g_a*(k_g/k_f-1)))+1/(3*(1+g_c*(k_g/k_f-1)))
c    # equation 8.20 in Campbell and Norman 1988
      epsilon_w=2/(3*(1+g_a*(k_w/k_f-1)))+1/(3*(1+g_c*(k_w/k_f-1)))
c    # equation 8.20 in Campbell and Norman 1988     
      epsilon_m=2/(3*(1+g_a*(k_m(j)/k_f-1)))+
     &1/(3*(1+g_c*(k_m(j)/k_f-1))) 
c    # equation 8.13 in Campbell and Norman 1988
      Thconduct(i)=(theta*epsilon_w*k_w+phi_m*epsilon_m*k_m(j)+phi_g*
     &epsilon_g*k_g)/(theta*epsilon_w+phi_m*epsilon_m+phi_g*epsilon_g) 
      
c    soil water potential level A
c    regression of b on proportion clay, from Table 9.1 Campbell and Norman
      bA=10.742*clay(j)/100. + 2.0031 
c    regression of air-entry water potential on proportion clay, from Table 9.1 Campbell and Norman
      weA=-5.5461*clay(j)/100. - 0.8005 
c    matric water potential J/kg = kpa = mbar/10
      wmA=weA*moistt(i)**(-bA)

c     Convert thermal conductivities from W/m-K to cal/min-cm-K for DSUB'S Microclimate calculations
      Thconduct(i)=(Thconduct(i)/418.5)*60.
c     Convert specific heats from J/kg-K to cal/g-K for DSUB'S Microclimate calculations
      Spheat(i)=Spheat(i)/4185.
c     Convert densities from kg/m3 to g/cm3 for DSUB'S Microclimate calculations
C     1 kg/m3 * 1000g/1 kg * 1 m3/1000000.
      Density(i)=Density(i)/1.0E+3
2     continue

c  900 CONTINUE  

      RETURN
      END   
