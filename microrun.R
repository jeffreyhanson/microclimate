microclimate <- function(micro) {
  # If the library hasn't been loaded yet, load it
  setwd('source/')
  if (!is.loaded('micr2014')) {
    dyn.load('micr2014.dll')
  } 
  
  julnum <- as.double(micro$julnum)  
  a <- .Fortran("micr2014", 
    as.double(micro$microinput), 
    as.double(micro$julday), 
    as.double(micro$SLES), 
    as.double(micro$DEP), 
    as.double(micro$Intrvls), 
    as.double(micro$MAXSHADES), 
    as.double(micro$MINSHADES), 
    as.double(micro$Nodes), 
    as.double(micro$TIMAXS), 
    as.double(micro$TIMINS), 
    as.double(micro$RHMAXX), 
    as.double(micro$RHMINN), 
    as.double(micro$CCMAXX), 
    as.double(micro$CCMINN), 
    as.double(micro$WNMAXX), 
    as.double(micro$WNMINN), 
    as.double(micro$TMAXX), 
    as.double(micro$TMINN), 
    as.double(micro$SNOW), 
    as.double(micro$REFLS), 
    as.double(micro$PCTWET), 
    as.double(micro$soilinit),  
    as.double(micro$hori),
    as.double(micro$TAI),
    as.double(micro$soilprop),
    as.double(micro$moists),
    as.double(micro$RAINFALL),
    as.double(micro$tannulrun),
    as.double(micro$tides),
    as.double(micro$PE),
    as.double(micro$KS),
    as.double(micro$BB),
    as.double(micro$BD),
    as.double(micro$L),
    as.double(micro$LAI),

    metout=matrix(data = 0., nrow = 24*7300, ncol = 18), 
    soil=matrix(data = 0., nrow = 24*7300, ncol = 12), 
    shadmet=matrix(data = 0., nrow = 24*7300, ncol = 18),
    shadsoil=matrix(data = 0., nrow = 24*7300, ncol = 12),
    soilmoist=matrix(data = 0., nrow = 24*7300, ncol = 12),
    shadmoist=matrix(data = 0., nrow = 24*7300, ncol = 12),
    humid=matrix(data = 0., nrow = 24*7300, ncol = 12),
    shadhumid=matrix(data = 0., nrow = 24*7300, ncol = 12),
    soilpot=matrix(data = 0., nrow = 24*7300, ncol = 12),
    shadpot=matrix(data = 0., nrow = 24*7300, ncol = 12))
  dyn.unload("micr2014.dll") 
  
  metout <- matrix(data = 0., nrow = 24*7300, ncol = 18)
  shadmet <- matrix(data = 0., nrow = 24*7300, ncol = 18)
  soil <- matrix(data = 0., nrow = 24*7300, ncol = 12)
  shadsoil <- matrix(data = 0., nrow = 24*7300, ncol = 12)
  soilmoist <- matrix(data = 0., nrow = 24*7300, ncol = 12)
  shadmoist <- matrix(data = 0., nrow = 24*7300, ncol = 12)
  humid <- matrix(data = 0., nrow = 24*7300, ncol = 12)
  shadhumid <- matrix(data = 0., nrow = 24*7300, ncol = 12)
  soilpot <- matrix(data = 0., nrow = 24*7300, ncol = 12)
  shadpot <- matrix(data = 0., nrow = 24*7300, ncol = 12)
  storage.mode(metout)<-"double"
  storage.mode(shadmet)<-"double"
  storage.mode(soil)<-"double"
  storage.mode(shadsoil)<-"double"
  storage.mode(soilmoist)<-"double"
  storage.mode(shadmoist)<-"double"
  storage.mode(humid)<-"double"
  storage.mode(shadhumid)<-"double"
  storage.mode(soilpot)<-"double"
  storage.mode(shadpot)<-"double"
  metout<-a$metout
  shadmet<-a$shadmet
  soil<-a$soil
  shadsoil<-a$shadsoil
  soilmoist<-a$soilmoist
  shadmoist<-a$shadmoist
  humid<-a$humid
  shadhumid<-a$shadhumid
  soilpot<-a$soilpot
  shadpot<-a$shadpot
  metout.names<-c("JULDAY","TIME","TALOC","TAREF","RHLOC","RH","VLOC","VREF","SOILMOIST3","POOLDEP","TDEEP","ZEN","SOLR","TSKYC","DEW","FROST","SNOWFALL","SNOWDEP")
  colnames(metout)<-metout.names
  colnames(shadmet)<-metout.names
  soil.names<-c("JULDAY","TIME",paste("D",DEP,"cm", sep = ""))
  colnames(soil)<-soil.names
  colnames(shadsoil)<-soil.names
  moist.names<-c("JULDAY","TIME",paste("WC",DEP,"cm", sep = ""))
  humid.names<-c("JULDAY","TIME",paste("RH",DEP,"cm", sep = ""))
  pot.names<-c("JULDAY","TIME",paste("PT",DEP,"cm", sep = ""))
  colnames(soilmoist)<-moist.names
  colnames(shadmoist)<-moist.names
  colnames(humid)<-humid.names
  colnames(shadhumid)<-humid.names
  colnames(soilpot)<-pot.names
  colnames(shadpot)<-pot.names
  setwd("..") # return to base directory
  return (list(metout=metout, soil=soil, shadmet=shadmet, shadsoil=shadsoil, soilmoist=soilmoist, shadmoist=shadmoist, humid=humid, shadhumid=shadhumid, soilpot=soilpot, shadpot=shadpot))
}