################################################################
## Script by T. Vu & N. Holford
## Companion Script: nmvpc_PKPD_Functions.R
## Last updated: 30 March 2012
## Support for different percentiles for PI and BI plus binning of CI
## Options for changing TIME variable name and prediction MDV variable name
################################################################

isNEW= T # Initialize for new model fit file
isBIG= F # if isbig=T then then re-read simulation file each time to use less memory

#############################################
## Read NONMEM simulation file
#############################################

if (isNEW | isBIG) {
   rm(list=ls())                    # Clean up workspace
   path = "."                       # Usually start in current directory
   setwd(path)                      # set working directory to find files
   source("nmvpc_functions.R")      # set up functions
   # reset values because workspace hase been cleaned up
   isNEW= T # Initialize for new model fit file
   isBIG= F # if isbig=T then then re-read simulation file each time to use less memory

   ###  NMTBL and NMDIR are changed by nmvpc.bat using WFN environment variables
   ISWFN     = T # NONMEM simulation table file: T= Look in Wings for NONMEM run directory; F = Look in current directory
   NMTBL     = ".fit" # NONMEM table file extension 
   if (ISWFN) NMDIR=".reg"

   #Read Simulation File
   modelName = "ka1_to_emax1_simln"       # No file extension

   #Expects NONMEM simulation table file in a WFN directory
   #;Simulation Start
   #;At start of $PK or $PRED save the original observation
   #OBS=DV
   #;At end of $PK or $PRED save the replication number
   #REP=IREP
   #;must produce NONMEM table file with REP ID TIME DVID DV MDV PRED OBS
   #$SIM (20120401) ONLYSIM NSUB=100
   #$TABLE REP ID TIME DVID DV MDV PRED OBS ; TAD MDVP DOSE STDY SPEC
   #NOAPPEND ONEHEADER NOPRINT FILE=run.fit
   #;Simulation End

   if (isBIG) {
      simFile <- getfromNMtable(modelName,ISWFN=ISWFN,NMDIR=NMDIR,NMTBL=NMTBL)
   } else {
      fitFile <- getfromNMtable(modelName,ISWFN=ISWFN,NMDIR=NMDIR,NMTBL=NMTBL)
   }
   #write.csv(simFile,"simfile1.csv") # for debugging
}

#############################################
## Set VPC Options
#############################################

### The following items may be modifed by nmvpc.bat

PIpercentile = 0.9  # Percentile for prediction intervals
plotPI    = T # T if plot prediction intervals
CIpercentile = 0.95  # Percentile for confidence intervals
plotCI    = T # T if plot confidence intervals

logaxis = "" # choose axis for log scale (use "x", "y", "xy" or "")

# bin simulated values by time intervals based on nominal binning times (only used if binsim=T)
binTimes = c(c(seq(0,10,1),seq(12,144,12)))
Xlabel  = "Hour" # X-axis label on graph
Xmin    =   0 # minimum scale for x-axis
Xmax    = 144 # maximum scale for x-axis
Xtick   =  24 # ticks on x-axis at these intervals for linear scale

thisDVID=  1 # #identify observation type using the DVID variable in the simulation and observation data files
hasDVID =  T # T if observation and simulation data file have DVID data item (otherwise no selection of DVID)
hasMDV  =  T # T if observation and simulation data file have MDV data item (otherwise all records are valid DV)
Ylabel  = "mg/L" # Y-axis label on graph
Ymin    =  0 # minimum scale for y-axis
Ymax    = 20 # maximum scale for y-axis
Ytick   = 5 # ticks on y-axis at these intervals for linear scale
LLOQ    = 0 # lower limit of quantitation to apply to predicted values
pdfTxt  = "CP" # pdf file name identifier e.g. use with select

figOutputDir = "vpc_CP.pdf/" # directory for VPC pdf and csv files
timeScale    = 1 # use this to scale TIME variable (e.g. 52 to scale years to weeks)
isSTD        = T # create standard VPC
isPC         = T # create pred-corrected VPC
isCSV        = F # if iscsv=T then write csv files with numerical values used for plots

###  The following items may only be changed here in the R script
binsim    = T # T if simulation times are not the same for every subject (Otherwise use times in simulation file 'as is')
hasATIM   = F # T if simulation data file has an actual observation time item in the simulation file (otherwise ATIM=TIME)
hasLLOQ   = T # T if values less than LLOQ should be ignored
addLegend = T # T if add a legend to the plot
omitNeg   = T # T if omit negative simulated values

#output options
isPDF        = T # T if generate PDF output otherwise display plots on screen
#pCols       = c(obscol='gray50',simcol='black',pici='gray30')
pCols        = c(obscol='red',simcol='black',pici='gray90') # 0 is black; 100 is white


if (logaxis=="y" || logaxis=="xy") {
   if (Ymin<=0) {
       Ymin=Ymax/100000 # User may want to change the divisor from 1000 to change logaxis lower bound
   }
   min.powy=floor(log10(Ymin)) ; max.powy=ceiling(log10(Ymax)) #set minimum & maximum power for Y-axis (e.g., 10^-1 to 10^3)
}
if (logaxis=="x" || logaxis=="xy") {
   if (Xmin<=0) {
       Xmin=Xmax/1000 # User may want to change the divisor from 1000 to change logaxis lower bound
   }
   min.powx=floor(log10(Xmin)) ; max.powx=ceiling(log10(Xmax)) #set minimum & maximum power for X-axis (e.g., 10^-1 to 10^3)
}

########################################################
#### TYPICAL USE DOES NOT REQUIRE ANY CHANGES BELOW HERE
########################################################
pdfName      <- paste(modelName,"_DVID=",thisDVID,"_",PIpercentile*100,"logaxis",logaxis,"_",pdfTxt,sep='')

if (isBIG) {
   # over-write simFile to save memory
   if (hasDVID) {
      simFile <- simFile[simFile$DVID==thisDVID,]
   }
} else {
   # make a copy of the fit file for this particular set of VPCs
   if (hasDVID) {
      simFile <- fitFile[fitFile$DVID==thisDVID,]
   } else {
      simFile=fitFile
   }
}
# simfile TIME may be changed by nmvpc.bat to TIMENAME argument
simFile$TIME=simFile$TIME*timeScale

if (!hasATIM) { #Generate an ATIM item from the simulation TIME if there is not an ATIM item in the simulation file
   simFile$ATIM=simFile$TIME
} else {
   simFile$ATIM=as.numeric(simFile$ATIM)
}

obsFile=simFile[simFile$REP==1,]
obsFile$DV=obsFile$OBS


#Do not change the next line!
#SELECT specific observations e.g. study number
#obsFile = obsFile[obsFile$SDY==1,]
#simFile = simFile[simFile$SDY==1,]

#write.csv(simFile,"simfile2.csv") # for debugging

#Replicate all original DV into simFile
#simFile$OBS=obsFile$DV
simFile$OBS=simFile$OBS[simFile$REP==1]

#write.csv(obsFile,"obsfile1.csv") # for debugging

# simFile MDV may be changed by nmvpc.bat to MDVPNAME argument
if (hasMDV) {
   simFile <- simFile[simFile$MDV==0,]
   obsFile <- obsFile[obsFile$MDV==0,]
}

if (hasLLOQ) {
   obsFile <- obsFile[obsFile$DV>=LLOQ,]
   simFile <- simFile[simFile$DV>=LLOQ,]
}


#### GENERATE PLOTS
## Set nominal time points for binning
if (!binsim) { binTimes <- sort(unique(simFile$TIME)) }

if (isSTD) { ## Standard VPC with CI around PI
   #Compute PI of observed values
   obsPI     <- getObsPI(dat=obsFile,dvCol='DV',idvCol='TIME')
   #Compute CI of PI for each replication
   simCIPI   <- getSimCIPI(dat=simFile,dvCol='DV',idvCol='TIME')

   if (isCSV) {
      write.csv(obsPI,paste(figOutputDir,"obsPI",pdfName,".csv",sep=""))
      write.csv(simCIPI,paste(figOutputDir,"predPI",pdfName,".csv",sep=""))
   }

   if (isPDF) {
      pdfFileName <- paste(pdfName,".pdf",sep='')
      pdf(paste(figOutputDir,pdfFileName,sep=''),height=6,width=10)
   }
   par(las=1,pin=c(4,3),mfrow=c(1,2),omi=c(rep(0.5,2),0.1,0),xpd=F)
   plotVPCs(obsFile=obsFile,simQuant=simCIPI,obsQuant=obsPI,logaxis=logaxis,pub=T)
   if (isPDF) dev.off()
}
if (isPC) { ## PRED-corrected VPC with CI around PI
   pcobs       <- getPCObs(simFile,dvCol='OBS') 
   pcsim       <- getPCSim(simFile,dvCol='DV')
   pcobsPI     <- getObsPI(dat=pcobs,dvCol="PCObs",idvCol='TIME') 
   pcsimCIPI   <- getSimCIPI(dat=pcsim,dvCol='PCSim',idvCol='TIME')
   if (isCSV) {
      write.csv(pcobsPI,paste(figOutputDir,"PC_obsPI",pdfName,".csv",sep=""))
      write.csv(pcsimCIPI,paste(figOutputDir,"PC_predPI",pdfName,".csv",sep=""))
   }

   if (isPDF) {
      pdfFileName <- paste(pdfName,"PC.pdf",sep='')
      pdf(paste(figOutputDir,pdfFileName,sep=''),height=6,width=10)
   }
   par(las=1,pin=c(4,3),mfrow=c(1,2),omi=c(rep(0.5,2),0.1,0),xpd=F)
   plotVPCs(obsFile=obsFile,simQuant=pcsimCIPI,obsQuant=pcobsPI,logaxis=logaxis,pub=T)
   if (isPDF) dev.off()
}
