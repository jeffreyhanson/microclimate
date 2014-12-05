# R Implementation of the microclimate model of Warren Porter's Niche Mapper system 
# Michael Kearney and Warren Porter 28 July 2014

# This setup is for testing purposes
# It runs the model for the middle day of each month, and the input data is for Madison WI, USA

# compile the Fortran code (creates micr2014.dll or micr2014.so pc/mac, the executable subroutine called by R)
setwd("source/") # set the working directory where the fortran source code is, assuming you are starting out in the /microclimate directory
cmnd<- "rcmd SHLIB micr2014.f BLKDATA.f dchxy.f dexpi.f DRYAIR.f DSUB.f error.f EVALXZ.f EVAP.f FUN.f gamma.f iomet1.f iomet2.f iosolr.f JREAD.f Micro.f MicroSegmt.f Osub.f Pttabl.f Rdctrl.f Rdtabl.f RelHumLocal.f Sfode.f sinec.f soilprops.f solrad.f Soylnods.f Tab.f VAPPRS.f vsine.f WETAIR.f ZBRAC.f ZBRENT.f"
# line below is for mac
#cmnd<- "R CMD SHLIB micr2014.f BLKDATA.f dchxy.f dexpi.f DRYAIR.f DSUB.f error.f EVALXZ.f EVAP.f FUN.f gamma.f iomet1.f iomet2.f iosolr.f JREAD.f Micro.f MicroSegmt.f Osub.f Pttabl.f Rdctrl.f Rdtabl.f RelHumLocal.f Sfode.f sinec.f soilprops.f solrad.f Soylnods.f Tab.f VAPPRS.f vsine.f WETAIR.f ZBRAC.f ZBRENT.f"
system(cmnd) # run the compilation
setwd("..") # return to base directory

######################### times and location info #######################################################
mac<-0 # choose mac (1) or pc (0)
longlat<-c(-89.40123,43.07305) # type a long/lat here in decimal degrees
julnum<-12 # number of time intervals to generate predictions for over a year (must be 12 <= x <=365)
julday<-c(15.,46.,74.,105.,135.,166.,196.,227.,258.,288.,319.,349.) # middle day of each month
idayst <- 1 # start month
ida<-julnum # end month
if(julnum<365){
  microdaily<-0 # run microclimate model as normal, where each day is iterated 3 times starting with the initial condition of uniform soil temp at mean monthly temperature
}else{
  microdaily<-1 # run microclimate model where one iteration of each day occurs and last day gives initial conditions for present day
}
HEMIS <- ifelse(longlat[2]<0,2.,1.) # chose hemisphere based on latitude
ALAT <- abs(trunc(longlat[2])) # degrees latitude
AMINUT <- (abs(longlat[2])-ALAT)*60 # minutes latitude
ALONG <- abs(trunc(longlat[1])) # degrees longitude
ALMINT <- (abs(longlat[1])-ALONG)*60 # minutes latitude
ALREF <- ALONG # reference longitude for time zone
#########################################################################################################

############################### microclimate model parameters ###########################################
EC <- 0.0167238 # Eccenricity of the earth's orbit (current value 0.0167238, ranges between 0.0034 to 0.058)
RUF <- 0.004 # Roughness height (m), , e.g. sand is 0.05, grass may be 2.0, current allowed range: 0.001 (snow) - 2.0 cm.
# Next four parameters are segmented velocity profiles due to bushes, rocks etc. on the surface
#IF NO EXPERIMENTAL WIND PROFILE DATA SET ALL THESE TO ZERO! (then roughness height is based on the parameter RUF)
Z01 <- 0. # Top (1st) segment roughness height(m)
Z02 <- 0. # 2nd segment roughness height(m)
ZH1 <- 0. # Top of (1st) segment, height above surface(m)
ZH2 <- 0. # 2nd segment, height above surface(m)  
SLE <- 0.96 # Substrate longwave IR emissivity (decimal %), typically close to 1
ERR <- 2.0 # Integrator error for soil temperature calculations
DEP <- c(0., 2.5,  5.,  10.,  15.,  20.,  30.,  50.,  100.,  200.) # Soil nodes (cm) - keep spacing close near the surface, last value is where it is assumed that the soil temperature is at the annual mean air temperature
Thcond <- 2.5 # soil minerals thermal conductivity (W/mC)
Density <- 2560. # soil minerals density (kg/m3)
SpecHeat <- 870. # soil minerals specific heat (J/kg-K)
BulkDensity <- 1300 # soil bulk density (kg/m3)
SatWater <- 0.26 # volumetric water content at saturation (0.1 bar matric potential) (m3/m3)
Clay <- 20 # clay content for matric potential calculations (%)
SoilMoist <- 0 # fractional soil moisture (decimal %)
REFL<-0.10 # soil reflectance (decimal %)
ALTT<-226 # altitude (m)
slope<-0. # slope (degrees, range 0-90)
azmuth<-180. # aspect (degrees, 0 = North, range 0-360)
hori<-rep(0,24) # enter the horizon angles (degrees) so that they go from 0 degrees azimuth (north) clockwise in 15 degree intervals
VIEWF <- 1-sum(sin(hori*pi/180))/length(hori) # convert horizon angles to radians and calc view factor(s)
PCTWET<-0 # percentage of surface area acting as a free water surface (%)
SNOW <- rep(0,julnum) # indicates if snow is on the surface (1 is yes, 0 is no), will remove this ultimately
CMH2O <- 1. # precipitable cm H2O in air column, 0.1 = VERY DRY; 1.0 = MOIST AIR CONDITIONS; 2.0 = HUMID, TROPICAL CONDITIONS (note this is for the whole atmospheric profile, not just near the ground)  
TIMAXS <- c(1.0, 1.0, 0.0, 0.0)   # Time of Maximums for Air Wind RelHum Cloud (h), air & Wind max's relative to solar noon, humidity and cloud cover max's relative to sunrise    															
TIMINS <- c(0.0, 0.0, 1.0, 1.0)   # Time of Minimums for Air Wind RelHum Cloud (h), air & Wind min's relative to sunrise, humidity and cloud cover min's relative to solar noon
minshade<-0. # minimum available shade (%)
maxshade<-90. # maximum available shade (%)
runshade<-1 # run the model twice, once for each shade level (1) or just for the first shade level (0)?
grasshade<-0 # this drives min shade value by the relative soil moisture multiplied by the maxshade parameter, above
Usrhyt <- 1# local height (cm) at which air temperature, relative humidity and wind speed calculatinos will be made 
# Aerosol profile
# the original profile from Elterman, L. 1970. Vertical-attenuation model with eight surface meteorological ranges 2 to 13 kilometers. U. S. Airforce Cambridge Research Laboratory, Bedford, Mass.
#TAI<-c(0.0670358341290886,0.0662612704779235,0.065497075238002,0.0647431301168489,0.0639993178022531,0.0632655219571553,0.0625416272145492,0.0611230843885423,0.0597427855962549,0.0583998423063099,0.0570933810229656,0.0558225431259535,0.0545864847111214,0.0533843764318805,0.0522154033414562,0.0499736739981675,0.047855059159556,0.0458535417401334,0.0439633201842001,0.0421788036108921,0.0404946070106968,0.0389055464934382,0.0374066345877315,0.0359930755919066,0.0346602609764008,0.0334037648376212,0.0322193394032758,0.0311029105891739,0.0300505736074963,0.0290585886265337,0.0281233764818952,0.0272415144391857,0.0264097320081524,0.0256249068083005,0.0248840604859789,0.0241843546829336,0.0235230870563317,0.0228976873502544,0.0223057135186581,0.0217448478998064,0.0212128934421699,0.0207077699817964,0.0202275105711489,0.0197702578594144,0.0193342605242809,0.0189178697551836,0.0177713140039894,0.0174187914242432,0.0170790495503944,0.0167509836728154,0.0164335684174899,0.0161258546410128,0.0158269663770596,0.0155360978343254,0.0152525104459325,0.0149755299703076,0.0147045436435285,0.0144389973831391,0.0141783930434343,0.0134220329447663,0.0131772403830191,0.0129356456025128,0.0126970313213065,0.0124612184223418,0.0122280636204822,0.01199745718102,0.0115436048739351,0.0110993711778668,0.0108808815754663,0.0106648652077878,0.0104513876347606,0.0102405315676965,0.00982708969547694,0.00962473896278535,0.00903679230300494,0.00884767454432418,0.0083031278398166,0.00796072474935954,0.00755817587626185,0.00718610751850881,0.00704629977586921,0.00684663903049612,0.00654155580333479,0.00642947339729728,0.00627223096874308,0.00603955966866779,0.00580920937536261,0.00568506186880564,0.00563167068287251,0.00556222005081865,0.00550522989971023,0.00547395763028062,0.0054478983436216,0.00541823364504573,0.00539532163908382,0.00539239864119488,0.00541690124712384,0.00551525885358836,0.00564825853509463,0.00577220185074264,0.00584222986640171,0.00581645238345584,0.00566088137411449,0.00535516862329704,0.00489914757707667,0.00432017939770409,0.0036813032251836,0.00309019064543606,0.00270890436501562,0.00276446109239711,0.00356019862584603)
# the values extracted from GADS for Madison
TAI<-c(0.269904738,0.266147825,0.262442906,0.258789404,0.255186744,0.251634356,0.248131676,0.2412732,0.234606887,0.228128378,0.221833385,0.215717692,0.20977715,0.204007681,0.198405272,0.187685927,0.177588357,0.168082846,0.159140695,0.150734206,0.142836655,0.135422274,0.128466227,0.12194459,0.115834329,0.110113284,0.104760141,0.099754417,0.09507644,0.090707328,0.086628967,0.082823998,0.07927579,0.075968428,0.072886691,0.070016034,0.067342571,0.064853053,0.062534858,0.060375964,0.058364941,0.056490925,0.054743609,0.053113222,0.051590514,0.050166738,0.046408775,0.045302803,0.044259051,0.043271471,0.042334415,0.041442618,0.040591184,0.039775572,0.038991583,0.038235345,0.037503301,0.036792197,0.036099067,0.034101935,0.033456388,0.032817888,0.032184949,0.031556287,0.030930816,0.030307633,0.029065372,0.027825562,0.027205981,0.026586556,0.025967391,0.025348692,0.024114005,0.023498886,0.021669152,0.021066668,0.019292088,0.018144698,0.016762709,0.015451481,0.014949794,0.014224263,0.013093462,0.012670686,0.012070223,0.011164062,0.010241734,0.009731103,0.009507687,0.009212683,0.008965785,0.008827751,0.008710756,0.008574128,0.008462605,0.008446967,0.008539475,0.009015237,0.009748444,0.010586023,0.011359647,0.011901268,0.012062153,0.011735443,0.010882215,0.009561062,0.007961182,0.006438984,0.005558204,0.006133532,0.009277754)
###################################################################################################################

######################### Time varying environmental data ##########################
TMAXX<-c(-3.2,0.1,6.8,14.6,21.3,26.4,29,27.7,23.3,16.6,7.8,-0.4) # maximum air temperatures (deg C)
TMINN<-c(-14.3,-12.1,-5.1,1.2,6.9,12.3,15.2,13.6,8.9,3,-3.2,-10.6) # minimum air temperatures (deg C)
RAINFALL<-c(28,28.2,54.6,79.7,81.3,100.1,101.3,102.5,89.7,62.4,54.9,41.2) # monthly mean rainfall (mm)
CCMAXX<-c(50.3,47,48.2,47.5,40.9,35.7,34.1,36.6,42.6,48.4,61.1,60.1) # max cloud cover (%)
CCMINN<-c(50.3,47,48.2,47.5,40.9,35.7,34.1,36.6,42.6,48.4,61.1,60.1) # min cloud cover (%)
WNMAXX<-c(4.9,4.8,5.2,5.3,4.6,4.3,3.8,3.7,4,4.6,4.9,4.8) # max wind speed (m/s)
WNMINN<-c(4.9,4.8,5.2,5.3,4.6,4.3,3.8,3.7,4,4.6,4.9,4.8) # min wind speed (m/s)
RHMAXX<-c(100,100,100,100,100,100,100,100,100,100,100,100) # max relative humidity (%)
RHMINN<-c(50.2,48.4,48.7,40.8,40,42.1,45.5,47.3,47.6,45,51.3,52.8) # min relative humidity (%)
tannul<-mean(c(TMAXX,TMINN)) # annual mean temperature for getting monthly deep soil temperature (deg C)
tannulrun<-rep(tannul,julnum) # monthly deep soil temperature (2m) (deg C)
SoilMoist<-c(0.42,0.42,0.42,0.43,0.44,0.44,0.43,0.42,0.41,0.42,0.42,0.43) # soil moisture (decimal %)
# creating the arrays of environmental variables that are assumed not to change with month for this simulation 
MAXSHADES <- rep(maxshade,julnum) # daily max shade (%)
MINSHADES <- rep(minshade,julnum) # daily min shade (%)
SLES <- rep(SLE,julnum) # ground emissivities
REFLS<-rep(REFL,julnum) # soil reflectances
PCTWET<-rep(PCTWET,julnum) # soil wetness
####################################################################################

################ soil properties  ################################################## 
# set up a profile of soil properites with depth for each day to be run
Intrvls <-(1:julnum) # user-supplied last Julian day in each time interval sequence
Numint <- julnum  # number of time intervals
Numtyps <- 2 # number of soil types
Nodes <- matrix(data = 0, nrow = 10, ncol = 7300) # array of all possible soil nodes for max time span of 20 years
Nodes[1,1:julnum]<-3 # deepest node for first substrate type
Nodes[2,1:julnum]<-9 # deepest node for second substrate type
#SoilMoist<-rep(SoilMoist,timeinterval) # soil moisture
Density<-Density/1000 # density of minerals - convert to Mg/m3
BulkDensity<-BulkDensity/1000 # density of minerals - convert to Mg/m3
moists2<-matrix(nrow=10, ncol = julnum, data=0) # set up an empty vector for soil moisture values through time
moists2[1,]<-SoilMoist # fill the first row with monthly soil moisture values
moists2[2,]<-moists2[1,] # make this row same as first row
moists2[3,]<-moists2[1,] # make this row same as first row
moists2[4,]<-moists2[1,] # make this row same as first row
moists<-moists2 # final soil moisture vector
# now make the soil properties matrix
# columns are: 
#1) bulk density (Mg/m3)
#2) volumetric water content at saturation (0.1 bar matric potential) (m3/m3)
#3) clay content (%)
#4) thermal conductivity (W/mK)
#5) specific heat capacity (J/kg-K)
#6) mineral density (Mg/m3)
soilprops<-matrix(data = 0, nrow = 10, ncol = 6) # create an empty soil properties matrix
soilprops[1,1]<-BulkDensity # insert soil bulk density to profile 1
soilprops[2,1]<-BulkDensity # insert soil bulk density to profile 2
soilprops[1,2]<-SatWater # insert saturated water content to profile 1
soilprops[2,2]<-SatWater # insert saturated water content to profile 2
soilprops[1,3]<-Clay     # insert percent clay to profile 1
soilprops[2,3]<-Clay     # insertpercent clay to profile 2
soilprops[1,4]<-Thcond # insert thermal conductivity to profile 1
soilprops[2,4]<-Thcond # insert thermal conductivity to profile 2
soilprops[1,5]<-SpecHeat # insert specific heat to profile 1
soilprops[2,5]<-SpecHeat # insert specific heat to profile 2
soilprops[1,6]<-Density # insert mineral density to profile 1
soilprops[2,6]<-Density # insert mineral density to profile 2
soilinit<-rep(tannul,length(DEP)) # make iniital soil temps equal to mean annual
#########################################################################################  

# surface soil moisture parameters
fieldcap<-30
wilting<-9
rainmult<-0.5


####ignore these for now, they are currently needed as input but are only for the snow version ##########
snowtemp<--100.5 # temperature at which precipitation falls as snow (used for snow model)
snowdens<-0.4 # snow density (mg/m3)
snowmelt<-1. # proportion of calculated snowmelt that doesn't refreeze
undercatch<-1. # undercatch multipier for converting rainfall to snow
rainmelt<-0.016 # paramter in equation that melts snow with rainfall as a function of air temp
#########################################################################################################  

# microclimate input parameters list
microinput<-c(julnum,RUF,ERR,Usrhyt,Numtyps,Numint,Z01,Z02,ZH1,ZH2,idayst,ida,HEMIS,ALAT,AMINUT,ALONG,ALMINT,ALREF,slope,azmuth,ALTT,CMH2O,microdaily,tannul,EC,VIEWF,snowtemp,snowdens,snowmelt,undercatch,fieldcap,wilting,rainmult,runshade,grasshade)

# all microclimate data input list - all these variables are expected by the input argument of the fortran micro2014 subroutine
micro<-list(microinput=microinput,julday=julday,SLES=SLES,DEP=DEP,Intrvls=Intrvls,Nodes=Nodes,MAXSHADES=MAXSHADES,MINSHADES=MINSHADES,TIMAXS=TIMAXS,TIMINS=TIMINS,TMAXX=TMAXX,TMINN=TMINN,RHMAXX=RHMAXX,RHMINN=RHMINN,CCMAXX=CCMAXX,CCMINN=CCMINN,WNMAXX=WNMAXX,WNMINN=WNMINN,SNOW=SNOW,REFLS=REFLS,PCTWET=PCTWET,soilinit=soilinit,hori=hori,TAI=TAI,soilprops=soilprops,moists=moists,RAINFALL=RAINFALL,tannulrun=tannulrun)

# write all input to csv files in their own folder - these can then be used as input for debugging the model in a Fortran IDE
# to do this, you compile with the 'input.for' file as the main program, which reads these csv files and passes them to the micro2014.f subroutine
setwd('csvinput/')
write.table(as.matrix(microinput), file = "microinput.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(julday, file = "julday.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(SLES, file = "SLES.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(DEP, file = "DEP.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(Intrvls, file = "Intrvls.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(Nodes, file = "Nodes.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(MAXSHADES, file = "Maxshades.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(MINSHADES, file = "Minshades.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(TIMAXS, file = "TIMAXS.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(TIMINS, file = "TIMINS.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(TMAXX, file = "TMAXX.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(TMINN, file = "TMINN.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(RHMAXX, file = "RHMAXX.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(RHMINN, file = "RHMINN.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(CCMAXX, file = "CCMAXX.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(CCMINN, file = "CCMINN.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(WNMAXX, file = "WNMAXX.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(WNMINN, file = "WNMINN.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(SNOW, file = "SNOW.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(REFLS, file = "REFLS.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(PCTWET, file = "PCTWET.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(soilinit, file = "soilinit.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(hori, file = "hori.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(TAI, file = "TAI.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(soilprops, file="soilprop.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(moists,file="moists.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(RAINFALL,file="rain.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(tannulrun,file="tannulrun.csv", sep = ",", col.names = NA, qmethod = "double")  
setwd('..')

if(mac==1){
  source('microrun_mac.R') # Fortran wrapper for the microclimate model  
}else{
 source('microrun.R') # Fortran wrapper for the microclimate model  
}
microut<-microclimate(micro) # run the model in Fortran

metout<-as.data.frame(microut$metout[1:(julnum*24),]) # retrieve above ground microclimatic conditions, min shade
shadmet<-as.data.frame(microut$shadmet[1:(julnum*24),]) # retrieve above ground microclimatic conditions, max shade
soil<-as.data.frame(microut$soil[1:(julnum*24),]) # retrieve soil temperatures, minimum shade
shadsoil<-as.data.frame(microut$shadsoil[1:(julnum*24),]) # retrieve soil temperatures, maximum shade

# write output to csv files
write.csv(metout,'metout.csv')
write.csv(shadmet,'shadmet.csv')
write.csv(soil,'soil.csv')
write.csv(shadsoil,'shadsoil.csv')

# metout/shadmet variables:
# 1 JULDAY - day of year
# 2 TIME - time of day (mins)
# 3 TALOC - air temperature (deg C) at local height (specified by 'Usrhyt' variable)
# 4 TAREF - air temperature (deg C) at reference height (1.2m)
# 5 RHLOC - relative humidity (%) at local height (specified by 'Usrhyt' variable)
# 6 RH  - relative humidity (%) at reference height (1.2m)
# 7 VLOC - wind speed (m/s) at local height (specified by 'Usrhyt' variable)
# 8 VREF - wind speed (m/s) at reference height (1.2m)
# 9 ZEN - zenith angle of sun (degrees - 90 = below the horizon)
# 10 SOLR - solar radiation (W/m2)
# 11 TSKYC - sky radiant temperature (deg C)
# 12 SNOWFALL - snow predicted to have fallen (mm)
# 13 SNOWDEP - predicted snow depth (cm)

# soil and shadsoil variables:
# 1 JULDAY - day of year
# 2 TIME - time of day (mins)
# 3-12 D0cm ... - soil temperatures at each of the 10 specified depths
###################plots#################################################
library(lattice) # library to do lattice plots

# plotting above-ground conditions in minimum shade
with(metout,{xyplot(TALOC + TAREF ~ TIME | as.factor(JULDAY),xlab = "Time of Day (min)", ylab = "Air Temperature (deg C)", auto.key=list(columns = 2), as.table = TRUE, type = "l")})
with(metout,{xyplot(RHLOC + RH ~ TIME | as.factor(JULDAY),xlab = "Time of Day (min)", ylab = "Relative Humidity (%)", auto.key=list(columns = 2), as.table = TRUE, type = "l")})
with(metout,{xyplot(VLOC ~ TIME | as.factor(JULDAY),xlab = "Time of Day (min)", ylab = "Wind Speed (m/s)", as.table = TRUE, type = "l")})
with(metout,{xyplot(ZEN ~ TIME | as.factor(JULDAY),xlab = "Time of Day (min)", ylab = "Zenith Angle of Sun (deg)", as.table = TRUE, type = "l")})
with(metout,{xyplot(SOLR ~ TIME | as.factor(JULDAY),xlab = "Time of Day (min)", ylab = "Solar Radiation (W/m2)", as.table = TRUE, type = "l")})
with(metout,{xyplot(TSKYC ~ TIME | as.factor(JULDAY),xlab = "Time of Day (min)", ylab = "Sky Temperature (deg C)", as.table = TRUE, type = "l")})
with(metout,{xyplot(SOILMOIST ~ TIME | as.factor(JULDAY),xlab = "Time of Day (min)", ylab = "surface soil moisture, mm", as.table = TRUE, type = "l")})

# plotting above-ground conditions in maximum shade
shadmet<-as.data.frame(shadmet)
with(shadmet,{xyplot(TALOC + TAREF ~ TIME | as.factor(JULDAY),xlab = "Time of Day (min)", ylab = "Air Temperature (deg C)", auto.key=list(columns = 2), as.table = TRUE, type = "l")})
with(shadmet,{xyplot(RHLOC + RH ~ TIME | as.factor(JULDAY),xlab = "Time of Day (min)", ylab = "Relative Humidity (%)", auto.key=list(columns = 2), as.table = TRUE, type = "l")})
with(shadmet,{xyplot(VLOC ~ TIME | as.factor(JULDAY),xlab = "Time of Day (min)", ylab = "Wind Speed (m/s)", as.table = TRUE, type = "l")})
with(shadmet,{xyplot(ZEN ~ TIME | as.factor(JULDAY),xlab = "Time of Day (min)", ylab = "Zenith Angle of Sun (deg)", as.table = TRUE, type = "l")})
with(shadmet,{xyplot(SOLR ~ TIME | as.factor(JULDAY),xlab = "Time of Day (min)", ylab = "Solar Radiation (W/m2)", as.table = TRUE, type = "l")})
with(shadmet,{xyplot(TSKYC ~ TIME | as.factor(JULDAY),xlab = "Time of Day (min)", ylab = "Sky Temperature (deg C)", as.table = TRUE, type = "l")})

# plotting soil for minimum shade
soil.names<-paste("D",DEP[1],"cm + D",DEP[2],"cm + D",DEP[3],"cm + D",DEP[4],"cm + D",DEP[5],"cm + D",DEP[6],"cm + D",DEP[7],"cm + D",DEP[8],"cm + D",DEP[9],"cm + D",DEP[10],"cm",sep="")
soil<-as.data.frame(soil)
with(soil,{xyplot(D0cm + D2.5cm + D5cm + D10cm + D15cm + D20cm + D30cm + D50cm + D100cm + D200cm ~ TIME | as.factor(JULDAY),xlab = "Time of Day (min)", ylab = "Soil Temperature (deg C)", auto.key=list(columns = 5), as.table = TRUE, type = "l")})

# plotting soil for maximum shade
shadsoil<-as.data.frame(shadsoil)
with(shadsoil,{xyplot(D0cm + D2.5cm + D5cm + D10cm + D15cm + D20cm + D30cm + D50cm + D100cm + D200cm ~ TIME | as.factor(JULDAY),xlab = "Time of Day (min)", ylab = "Soil Temperature (deg C)", auto.key=list(columns = 5), as.table = TRUE, type = "l")})
