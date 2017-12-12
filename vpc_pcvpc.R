vpc <- function (obs, sim, bin,percentile,alpha,log.y)
{
  nrep <- dim(sim)[1]/dim(obs)[1]
  int.pred1 <- alpha/2
  int.pred2 <- 1-alpha/2
  #--create a cut (time intervals)
  obs$cut <- cut(obs$Time, breaks=bin) 
  sim$cut <- cut(sim$Time, breaks=bin)
  obsper <- ddply(obs, .(cut), summarize, lowerPI=quantile(Conc,percentile[1]),
                  medianPI=quantile(Conc,percentile[2]),upperPI=quantile(Conc,percentile[3]))
  theoper <- ddply(sim, .(cut), summarize, lowerPI=quantile(Conc,percentile[1]),medianPI=quantile(Conc,percentile[2]),upperPI=quantile(Conc,percentile[3]))
  simper <- ddply(sim, .(REPL,cut), summarize, lower=quantile(Conc,percentile[1]),median=quantile(Conc,percentile[2]),upper=quantile(Conc,percentile[3]))
  PIlower <- ddply(simper, .(cut), summarize, CIlower=quantile(lower,int.pred1),CIupper=quantile(lower,int.pred2))
  PImedian <- ddply(simper, .(cut), summarize, CIlower=quantile(median,int.pred1),CIupper=quantile(median,int.pred2))
  PIupper <- ddply(simper, .(cut), summarize, CIlower=quantile(upper,int.pred1),CIupper=quantile(upper,int.pred2))
  PIlower$median <- obsper$lowerPI
  PImedian$median <- obsper$medianPI
  PIupper$median <- obsper$upperPI
  PIlower$theomedian <- theoper$lowerPI
  PImedian$theomedian <- theoper$medianPI
  PIupper$theomedian <- theoper$upperPI
  PIlower$PI <- "5% percentile"
  PIupper$PI <- "95% percentile"
  PImedian$PI <- "50% percentile"
  PI <- plyr::rbind.fill(PIlower,PImedian,PIupper)
  tab <- ddply(obs, .(cut), summarize, meantime=round(mean(Time, na.rm=T),2))
  PI <- merge(PI, tab, by=c("cut"), all.x=T)
  theoper <- merge(theoper, tab, by=c("cut"), all.x=T)
  if (log.y) PI[,2:5] <- log10(PI[,2:5])
  return(list(PI=PI,obs=obs,theoper=theoper))
}


pcvpc <- function (obs, sim, bin,percentile,alpha,log.y)
{
  nrep <- dim(sim)[1]/dim(obs)[1]
  int.pred1 <- alpha/2
  int.pred2 <- 1-alpha/2
  obs$cut <- cut(obs$Time, breaks=bin) 
  sim$cut <- cut(sim$Time, breaks=bin)
  meansim <- ddply(sim, .(ID,Time), summarize, predij=mean(Conc))
  obsx <- merge(obs, meansim, by=c("ID","Time"), all.x=T)
  simx <- merge(sim, meansim, by=c("ID","Time"), all.x=T)
  meansim$cut <- obs$cut
  predbin <- ddply(meansim, .(cut), summarize, predbin=median(predij))
  obsx <- merge(obsx, predbin, by=c("cut"), all.x=T)
  simx <- merge(simx, predbin, by=c("cut"), all.x=T)
  obsx$pcConc <- obsx$Conc*obsx$predbin/obsx$predij
  simx$pcConc <- simx$Conc*simx$predbin/simx$predij
  
  obsper <- ddply(obsx, .(cut), summarize, lowerPI=quantile(pcConc,percentile[1]),
                  medianPI=quantile(pcConc,percentile[2]),upperPI=quantile(pcConc,percentile[3]))
  theoper <- ddply(simx, .(cut), summarize, lowerPI=quantile(pcConc,percentile[1]),medianPI=quantile(pcConc,percentile[2]),upperPI=quantile(pcConc,percentile[3]))
  simper <- ddply(simx, .(REPL,cut), summarize, lower=quantile(pcConc,percentile[1]),median=quantile(pcConc,percentile[2]),upper=quantile(pcConc,percentile[3]))
  PIlower <- ddply(simper, .(cut), summarize, CIlower=quantile(lower,int.pred1),CIupper=quantile(lower,int.pred2))
  PImedian <- ddply(simper, .(cut), summarize, CIlower=quantile(median,int.pred1),CIupper=quantile(median,int.pred2))
  PIupper <- ddply(simper, .(cut), summarize, CIlower=quantile(upper,int.pred1),CIupper=quantile(upper,int.pred2))
  PIlower$median <- obsper$lowerPI
  PImedian$median <- obsper$medianPI
  PIupper$median <- obsper$upperPI
  PIlower$theomedian <- theoper$lowerPI
  PImedian$theomedian <- theoper$medianPI
  PIupper$theomedian <- theoper$upperPI
  PIlower$PI <- "5% percentile"
  PIupper$PI <- "95% percentile"
  PImedian$PI <- "50% percentile"
  PI <- plyr::rbind.fill(PIlower,PImedian,PIupper)
  tab <- ddply(obs, .(cut), summarize, meantime=round(mean(Time, na.rm=T),2))
  PI <- merge(PI, tab, by=c("cut"), all.x=T)
  theoper <- merge(theoper, tab, by=c("cut"), all.x=T)
  if (log.y) PI[,2:5] <- log10(PI[,2:5])
  return(list(PI=PI,obs=obsx,theoper=theoper))
}
