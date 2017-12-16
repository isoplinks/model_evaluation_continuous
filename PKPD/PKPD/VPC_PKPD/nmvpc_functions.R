################################################################
## Script by T. Vu and N. Holford 
## Last updated: 11Nov2012
## Support for different percentiles for PI and BI plus binning of CI
## Version notes:
## 11/11/2012  Added getQuant()
################################################################

calcQuant <- function(subd,dvCol,idvCol,includeRep) { 
################################################################################
## Function:   calcQuant
## Purpose:    to calculate quantiles for visual predictive check
## Arguments:  DV data subsetted by TIME intervals around the nominal time points
##             in the simulation data set
## Returns:    a data frame with time, median, lower and upper quantiles
################################################################################
   dv    <-  subd[,dvCol]
   idv   <-  subd[,idvCol]
   lowerp <- (1-PIpercentile)/2
   upperp <- PIpercentile+lowerp
   quant <- quantile(dv, probs=c(lowerp,.5,upperp), type=4, names=F, na.rm=T)
   nobs <- length(dv)
   tquant <- median(idv,na.rm=T)
   if(includeRep) {
      return(data.frame(REP=unique(subd$REP),TIME=tquant,N=nobs,
                        LOWPI=quant[1],MEDIAN=quant[2],UPPERPI=quant[3]))
   } else {
      return(data.frame(TIME=tquant,N=nobs,
             LOWPI=quant[1],MEDIAN=quant[2],UPPERPI=quant[3]))
   }
}

getQuant <- function(dat,X,Y,bins) {
################################################################################
## Function: getQuant
## Purpose: Obtain median and percentiles for a data set with X & Y values
## Returns: quantiles for the Y values according to binning of X values
################################################################################
   return(do.call(rbind,by(d,cut(d[,X],br=bins,right=T,include.lowest=T),function(subd) {
	   calcQuant(subd,dvCol=Y,idvCol=X,includeRep=F)} ))  )
}

getSimPI <- function(simFile,dvCol,idvCol) {
################################################################################
## Function: getSimPI
## Purpose: Obtain median and percentiles for simulated data
## Returns: simPI - quantiles for simulated values
################################################################################
   simPI <- do.call (rbind, by(simFile, list(simFile[,idvCol]),
               function(subdat) {calcQuant(subdat,dvCol,idvCol,includeRep=F)} ))
   simPI$TIME <- as.numeric(row.names(simPI))
   return(simPI)
}

getbinTimes <- function (binList) {
################################################################################
## Function:   getbinTimes(binList)
## Purpose:    Obtain midpoints for nominal times, 
##             e.g., binning observations by simulated time points (simTimes)
## Returns:    time points for binning
################################################################################
   binList <- sort(binList)
   binTimes <- NULL
   binTimes <- sapply(1:length(binList), function(i) {
                  curint <- binList[i] 
                  nxtint <- binList[i+1]
                  if(i==length(binList))  {
                     midint <- curint
                  } else {
                     midint <- curint+diff(range(curint,nxtint))/2 # select mid of intervals
                  }
                  c(binTimes,midint)
               })
   return(binTimes)
}

getSimCIPI <- function(dat,dvCol,idvCol) {
################################################################################
## Function: getSimCIPI
## Purpose: Obtain median and percentiles for simulated and observed data
## Returns: simCIPI - percentiles of median & percentiles from each replication
################################################################################
   binTimes <- getbinTimes(binTimes)
   if(binsim) { #option for computing quantiles of simulated values by time intervals
      repPI    <- do.call(rbind,by(dat,dat$REP, function(subrep) {
                     do.call (rbind, by(subrep, cut(subrep[,idvCol],br=c(0,binTimes),right=F,include.lowest=T), 
                        function(subdat) {calcQuant(subdat,dvCol,idvCol,includeRep=T)} ))
                }))
   } else { #option for computing quantiles of simulated values at exact simulation times, no binning
      repPI <- do.call(rbind,by(dat,dat$REP, function(subrep) {
                  do.call (rbind, by(subrep, list(subrep[,idvCol]), 
                     function(subdat) {calcQuant(subdat,dvCol,idvCol,includeRep=T)} ))
                }))
   }
   if(binsim) {simCIPI <-  do.call(rbind,by(repPI,cut(repPI[,idvCol],br=c(0,binTimes),right=F,include.lowest=T),calcCI))
        } else { simCIPI <- do.call(rbind,by(repPI,list(repPI[,idvCol]),calcCI)) }

       return(simCIPI)
}



calcCI <- function(subdat) {
################################################################################
## Function: calcCI
## Purpose: Obtain median and confidence intervals of the prediction intervals
## Returns: simCIPI - CI for the PI
################################################################################
      lowerp   <- (1-CIpercentile)/2
      upperp   <- CIpercentile+lowerp
      qlopi    <- quantile(subdat$LOWPI, probs=c(lowerp,.5,upperp), type=8, names=F, na.rm=T)
      qmedpi   <- quantile(subdat$MEDIAN, probs=c(lowerp,.5,upperp), type=8, names=F, na.rm=T)
      qhipi    <- quantile(subdat$UPPERPI, probs=c(lowerp,.5,upperp), type=8, names=F, na.rm=T)
      tquant   <- median(subdat$TIME)
      nobs     <- nrow(subdat)
      data.frame(TIME=tquant,N=nobs,
         LO_LOPI=qlopi[1],MED_LOPI=qlopi[2],HI_LOPI=qlopi[3],
         LO_MEDPI=qmedpi[1],MED_MEDPI=qmedpi[2],HI_MEDPI=qmedpi[3],
         LO_HIPI=qhipi[1],MED_HIPI=qhipi[2],HI_HIPI=qhipi[3])
}

getObsPI <- function(dat,dvCol,idvCol) {
################################################################################
## Function: getObsPI
## Purpose: Obtain median and percentiles for observed data
## Returns: obsPI - quantiles for observed values
################################################################################
   binTimes <- getbinTimes(binTimes)
   obsPI <- do.call (rbind, by(dat, cut(dat[,idvCol],br=c(0,binTimes),right=F,include.lowest=T),
                  function(dSub) {
                     # dSub <- subset(dat,dat[,idvCol]>0 & dat[,idvCol]<binTimes[2])
                     #to make sure each subj counts only once & 1st obs (for duplicated obs within each subj) is at the latest time point
                     #obsDataNoDup <- dataSub[order(dataSub$ID,dataSub[,idvCol],decreasing=F),] 
                     #remove the duplicated obs and keep only the obs from last time point
                     #obsDataNoDup <- obsDataNoDup[!duplicated(obsDataNoDup$ID),]
                     #obsDataNoDup <- dSub
                     if(nrow(dSub)>0) {calcQuant(dSub,dvCol,idvCol,includeRep=F)}
                  }
               ))
   #Match quantile obs time same with quantile simulated data
   if(binsim) {
      obsPI$TIME2 <- obsPI$TIME
   } else {
      obsPI$TIME2 <- binTimes[1:nrow(obsPI)] 
   }
   return(obsPI)
}

getPCObs <- function(simFile,dvCol) {
################################################################################
## Function:   getPCObs
## Purpose:    Obtain prediction-corrected observed DV 
## Returns:    same input file with medPRED, PCObs
################################################################################
   d1 <- subset(simFile,REP==1 & PRED!=0) #select 1 replication & only if predictions are available
   binTimes <- getbinTimes(binTimes)
   PCObs <- do.call (rbind, by(d1, cut(d1$ATIM,br=c(0,binTimes),right=F,include.lowest=T),
                  function(temp) {
                     # d1 <- subset(simFile,ATIM>=0 & ATIM<binTimes[1]) #binTimes[0]
                     temp$mPRED <-  median(temp$PRED)
                     temp$PCObs <-  temp[,dvCol]*(temp$mPRED/temp$PRED)
                     return(temp)
                  }
               ))
   return(PCObs)
}

getPCSim <- function(simFile,dvCol) {
################################################################################
## Function: getPCSim
## Purpose: Obtain prediction-corrected simulated DV 
## Returns: same input file with medPRED, PCObs
################################################################################
   d1 <- simFile[simFile$PRED!=0,]
   binTimes <- getbinTimes(binTimes)
   PCSim <- do.call (rbind, by(d1, cut(d1$ATIM,br=c(0,binTimes),right=F,include.lowest=T),
                  function(temp) {
                     # d1 <- subset(simFile,ATIM>=0 & ATIM<binTimes[1]) #binTimes[0]
                     temp$mPRED <-  median(temp$PRED)
                     temp$PCSim <-  temp[,dvCol]*(temp$mPRED/temp$PRED)
                     return(temp)
                  }
               ))
   return(PCSim)
}

setRawPlot <- function(xvar,yvar) {
################################################################################
## Function: setRawPlot
## Purpose: set up plot panel for raw scale
## Returns: 
################################################################################
   nxtick      <- (length(seq(Xmin,Xmax,by=Xtick))-1)
   nytick      <- (length(seq(Ymin,Ymax,by=Ytick))-1)
   plot(xvar,yvar,type='n',
            tcl=-.25, #tick length
            xlab="",ylab="",
            ylim=c(Ymin,Ymax), yaxp=c(Ymin,Ymax,nytick),
            xlim=c(Xmin,Xmax), xaxp=c(Xmin,Xmax,nxtick),
            mgp=c(5,.5, 0))
}

setPlotType <- function(xvar,yvar,logaxis=F) {
################################################################################
## Function: setPlotType
## Purpose: set up plot panel for logaxis scale
## Returns: 
################################################################################
   if (logaxis=='xy') {
      plot(xvar,yvar, 
         type='n',
         
   #      log='y',yaxt='n',
         log='xy',xaxt='n',yaxt='n',
         tcl=-.25, #tick length
         xlab="",ylab="",
         ylim=c(10^min.powy,10^max.powy),  
         xlim=c(10^min.powx,10^max.powx),  
   #      xlim=c(Xmin,Xmax), xaxp=c(Xmin,Xmax,nxtick),
         mgp=c(5,.5, 0)
      )
      axis.at  <- 10^c(min.powx:max.powx)
      axis(1,at = axis.at,tcl=-0.5,labels=parse(text = ifelse(axis.at<10^2,paste(axis.at),paste("10^", sort(min.powx:max.powx), sep = "")))) 
      axis(1,at = 1:10*rep(axis.at[-1]/10,each = 10),tcl = -0.25, labels = FALSE) 
      axis.at  <- 10^c(min.powy:max.powy)
      axis(2,at = axis.at,tcl=-0.5,labels=parse(text = ifelse(axis.at<10^2,paste(axis.at),paste("10^", sort(min.powy:max.powy), sep = "")))) 
      axis(2,at = 1:10*rep(axis.at[-1]/10,each = 10),tcl = -0.25, labels = FALSE) 
   } else if (logaxis=='y') {
      nxtick      <- (length(seq(Xmin,Xmax,by=Xtick))-1)
      plot(xvar,yvar, 
         type='n',
         
         log='y',yaxt='n',
         tcl=-.25, #tick length
         xlab="",ylab="",
         ylim=c(10^min.powy,10^max.powy),  
   #      xlim=c(10^min.powx,10^max.powx),  
         xlim=c(Xmin,Xmax), xaxp=c(Xmin,Xmax,nxtick),
         mgp=c(5,.5, 0)
      )
      axis.at  <- 10^c(min.powy:max.powy)
      axis(2,at = axis.at,tcl=-0.5,labels=parse(text = ifelse(axis.at<10^2,paste(axis.at),paste("10^", sort(min.powy:max.powy), sep = "")))) 
      axis(2,at = 1:10*rep(axis.at[-1]/10,each = 10),tcl = -0.25, labels = FALSE) 
   } else if (logaxis=='x') {
      nytick      <- (length(seq(Ymin,Ymax,by=Ytick))-1)
      plot(xvar,yvar, 
            type='n',
            log='x',xaxt='n',
            tcl=-.25, #tick length
            xlab="",ylab="",
            ylim=c(Ymin,Ymax), yaxp=c(Ymin,Ymax,nytick),
            xlim=c(10^min.powx,10^max.powx),  
            mgp=c(5,.5, 0)
      )
      axis.at  <- 10^c(min.powx:max.powx)
      axis(1,at = axis.at,tcl=-0.5,labels=parse(text = ifelse(axis.at<10^2,paste(axis.at),paste("10^", sort(min.powx:max.powx), sep = "")))) 
      axis(1,at = 1:10*rep(axis.at[-1]/10,each = 10),tcl = -0.25, labels = FALSE) 
   } else { # linear axes
      nxtick      <- (length(seq(Xmin,Xmax,by=Xtick))-1)
      nytick      <- (length(seq(Ymin,Ymax,by=Ytick))-1)
      plot(xvar,yvar,type='n',
               tcl=-.25, #tick length
               xlab="",ylab="",
               ylim=c(Ymin,Ymax), yaxp=c(Ymin,Ymax,nytick),
               xlim=c(Xmin,Xmax), xaxp=c(Xmin,Xmax,nxtick),
               mgp=c(5,.5, 0)
      )
   }
} 


xylabels <- function(Xlabel, Ylabel) {
################################################################################
## Function: xylabel
## Purpose: Consistent method for axis labels
## Returns: 
################################################################################
# increase line= to move label further from axis line
   mtext(side=1, outer=F,line=2.5,Xlabel,cex=1)
   mtext(side=2, outer=F,las=0,line=2.5,Ylabel,cex=1)
}

################################################################################
## Function: plotVPCs
## Purpose: Produce 2 VPC plots
## Returns: Scatterplot VPC and percentile VPC
################################################################################
plotVPCs <- function(obsFile,simQuant,obsQuant,logaxis=F,pub,isOBS=T) {

if (isOBS) { # Do this only if observations requested

# This is a scatterplot VPC of observations with observed median and PI (left hand plot on pdf)

   par(xpd=F)
   if(logaxis!="") {
      setPlotType(obsFile$TIME,obsFile$DV,logaxis)
   } else {
      setRawPlot(obsFile$TIME,obsFile$DV)
   }
   points (obsFile$TIME, obsFile$DV, pch=19, cex=.7, col=pCols['obscol'])


# This adds observaed median and PI to observation scatterplot
   if (plotPI) {
      lines(obsQuant$TIME2,obsQuant$LOWPI, lty=2, lwd=2, col=pCols['obscol'])
      lines(obsQuant$TIME2,obsQuant$UPPERPI, lty=2, lwd=2, col=pCols['obscol'])
   }
   lines(obsQuant$TIME2,obsQuant$MEDIAN, lty=1, lwd=2, col=pCols['obscol'])

# This adds simulated median and PI to observation scatterplot
#   if (plotPI) {
#     lines(simQuant$TIME,simQuant$MED_LOPI, lty=2, lwd=2, col=pCols['simcol'])
#     lines(simQuant$TIME,simQuant$MED_HIPI, lty=2, lwd=2, col=pCols['simcol'])
#   }
#   lines(simQuant$TIME,simQuant$MED_MEDPI, lty=1, lwd=2, col=pCols['simcol'])

   xylabels(Xlabel,Ylabel)
   if(!pub) {
   par(xpd=T) 
   legend (x=0,y=Ymax*1.3, ncol=2,
      legend=c("Obs",'Sim MedLo','Sim MedMedian','Sim MedHi'),
      x.intersp=.5,
      lty=c(0,2,1,2), pch=c(19,rep(NA,times=3)), pt.cex=.5,
      bty='n', lwd=2,   
      col=c(pCols['obscol'],rep(pCols['simcol'],3)))
   }
} # end of isOBS

# This is a percentile VPC of observated and simulated median and PI (right hand plot on pdf)

   par(xpd=F)
   setPlotType(obsFile$TIME,obsFile$DV,logaxis)

   times <- simQuant$TIME
   xx <- c(times,rev(times))
   lopiyy <- c(simQuant$LO_LOPI,rev(simQuant$HI_LOPI))
   medpiyy <- c(simQuant$LO_MEDPI,rev(simQuant$HI_MEDPI))
   hipiyy <- c(simQuant$LO_HIPI,rev(simQuant$HI_HIPI))
   
   if (plotCI) {
      polygon(xx,lopiyy, col=pCols['pici'],border=NA)
      polygon(xx,medpiyy, col=pCols['pici'],border=NA)
      polygon(xx,hipiyy, col=pCols['pici'],border=NA)
   }
   if (plotPI) {
      lines(simQuant$TIME,simQuant$MED_LOPI, lty=2, lwd=2, col=pCols['simcol'])
      lines(simQuant$TIME,simQuant$MED_HIPI, lty=2, lwd=2, col=pCols['simcol'])
      if (isOBS) {
         lines(obsQuant$TIME2,obsQuant$LOWPI, lty=2, lwd=2, col=pCols['obscol'])
         lines(obsQuant$TIME2,obsQuant$UPPERPI, lty=2, lwd=2, col=pCols['obscol'])
         points(obsQuant$TIME2,obsQuant$LOWPI, cex=.9, pch=16, col=pCols['obscol'])
         points(obsQuant$TIME2,obsQuant$UPPERPI,cex=.9, pch=16, col=pCols['obscol'])
      }
   }
   lines(simQuant$TIME,simQuant$MED_MEDPI, lty=1, lwd=2, col=pCols['simcol'])
   if (isOBS) {
      lines(obsQuant$TIME2,obsQuant$MEDIAN, lty=1, lwd=2, col=pCols['obscol'])
      points(obsQuant$TIME2,obsQuant$MEDIAN, cex=.9,pch=16, col=pCols['obscol'])
   }
   xylabels(Xlabel,Ylabel)
   if(!pub) {
      par(xpd=T) 
      legend (x=0,y=Ymax*1.3, ncol=2,
         legend=c('Sim MedLo','Sim MedMedian','Sim MedHi', 'ObsLo','ObsMedian','ObsHi'),
         x.intersp=.5,bty='n', 
         lty=rep(c(2,1,2),times=2), lwd=2,  
         pch=c(rep(NA,times=3),rep(16,times=3)), pt.cex=0.8,
         col=c(rep(pCols['simcol'],3),rep(pCols['obscol'],3)))
   }
   mtext(side=4, outer=T,las=0,line=-1,cex=0.5,adj=.5,
#   paste("obsFile:",obsFileName,"\n simFile:",modelName,sep=''))
   paste("simFile:",modelName,sep=''))
}


readData <- function(FileName,FilePath=NULL,FileType="csv",na.string=".") {
####################################################################################
## Function:	readData
## Purpose:		read in simulated and observed data
## Arguments:	 
## Returns:		
####################################################################################
   fileName <- paste(FilePath,FileName,sep='')
   if (FileType!="csv") {
      #reads NONMEM output table which has been written with headers. Headers are required to identify data items!
      #Skips first TABLE record then uses second record to get data item names
      dat  <- read.csv(fileName,sep="",header=T,skip=1,stringsAsFactors=F)
      #remove TABLE and data item header from subsequent sub-problems
      dat <- dat[dat[,1]!="TABLE"&dat[,1]!=names(dat[1]),]
      dat <- as.data.frame(apply(dat,2,as.numeric))
      return(dat)
   } else {
      return(read.csv(fileName,sep=',',header=T,na.string=na.string,comment.char=''))
   }
}   

####################################################################################
## Function:	getfromNMtable
## Purpose:		read data from a NONMEM table file or csv file
## Arguments:	ISWFN=data file is NONMEM table file, NMDIR=WFN directory extension, NMTBL=table file extension
## Returns:	data frame
## Author:  Nick Holford November 2008
####################################################################################
getfromNMtable<-function(runName,reps="", ISWFN=F, NMDIR=".", NMTBL=".fit") {
   if (ISWFN) fileName=paste(runName,NMDIR,"/",runName,reps,NMTBL,sep="")
   if (NMTBL!=".csv") {
      #reads NONMEM output table which has been written with headers. Headers are required to identify data items!
      #Skips first TABLE record then uses second record to get data item names
      dat=read.csv(fileName,sep="",header=T,skip=1,stringsAsFactors=F)
      #remove TABLE and data item header from subsequent sub-problems
      temp <- dat[dat[,1]!="TABLE"&dat[,1]!=names(dat[1]),]
      temp <- as.data.frame(apply(temp,2,as.numeric))
      return(temp)
   } else {
      return(read.csv(fileName,sep=',',header=T,na.string=na.string,comment.char=''))
   }
}

doseHxDist <- function(px,py,plotMean=F,xlab="",keyloc="topright") {
####################################################################################
## Function:	doseHxDist
## Purpose:		plot distributions of observed and simulated dosing histories for 
##             adaptive design
## Arguments:	 
##		       
## Returns:		distribution plots
####################################################################################
   hist(px,density=10,col='red',freq=F,
        main='',xlab=xlab)
   box(lty=1,col=1)
   hist(py,add=T,density=10,col='purple',angle=-45,freq=F)
   if(plotMean) {
      mpx 	<- signif(mean(na.omit(px)),d=3)
      mpy 	<- signif(mean(py),d=3)
      mpxtxt <- paste("(mean=",mpx,")",sep="")
      mpytxt <- paste("(mean=",mpy,")",sep="")
      abline(v=mpx,lwd=2,col='red')
      abline(v=mpy,lwd=2,col='purple')
   } else {mpxtxt <- mpytxt <- NULL }
   legend(keyloc,c(paste("Observed",mpxtxt),
         paste("Simulated",mpytxt)),
            cex=1,angle=c(45,-45),density=20,bty='n',
            fill=c('red','purple'))
}

