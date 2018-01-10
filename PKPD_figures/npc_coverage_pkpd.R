# npc computations provided by Tram
sim <- ALLPCDATA1
obs <-datafit[!is.na(datafit$PD),]
head(sim)
head(obs)

percentiles <- c(50-PI/2,(50+PI/2))
nobs <- nrow(obs)
nrep <- nrow(sim)/nobs

PI <- c(0,20,40,50,60,80,90,95)
npc <- function (obs, sim,PI){
  nobs <- dim(obs)[1]
  nrep <- dim(sim)[1]/nobs
  percentiles <- c(50-PI/2,(50+PI/2))
  
  matsimfull <- matrix(sim$DV, ncol = nrep)
  matsimper <- t(apply(matsimfull,1,quantile,percentiles/100))
  matsimper_lower <- matsimper[,1:length(PI)]
  matsimper_upper <- matsimper[,(length(PI)+1):length(percentiles)]
  sim_outlower <- c()
  sim_outupper <- c()
  for (i in 1:length(PI)) {
    matsim_outlower <- ifelse(matsimfull<matsimper_lower[,i],1,0)
    sim_outlower <- cbind(sim_outlower,quantile(apply(matsim_outlower,2,mean),c(0.025,0.5,0.975)))
    matsim_outupper<- ifelse(matsimfull>matsimper_upper[,i],1,0)
    sim_outupper <- cbind(sim_outupper,quantile(apply(matsim_outupper,2,mean),c(0.025,0.5,0.975)))}
  yobs_lower <- yobs_upper <- matrix(rep(obs$PD,length(PI)),ncol=length(PI))
  yobs_lower <- ifelse(yobs_lower<matsimper_lower,1,0)
  yobs_upper <- ifelse(yobs_upper>matsimper_upper,1,0)
  obsper_outlower <- apply(yobs_lower,2,mean)
  obsper_outupper <- apply(yobs_upper,2,mean)
  lower_cover <- obsper_outlower/sim_outlower[2,]
  CI_lower_cover <- t(t(sim_outlower[c(1,3),])/sim_outlower[2,])
  upper_cover <- obsper_outupper/sim_outupper[2,]
  CI_upper_cover <- t(t(sim_outupper[c(1,3),])/sim_outupper[2,])
  lower_out <- (lower_cover<CI_lower_cover[1,])+(lower_cover>CI_lower_cover[2,])
  upper_out <- (upper_cover<CI_upper_cover[1,])+(upper_cover>CI_upper_cover[2,])
  cover <- as.data.frame(cbind(rep(PI,2),c(lower_cover,upper_cover),c(lower_out,upper_out),rbind(t(CI_lower_cover),t(CI_upper_cover))),row.names=F)
  cover$Type <- rep(c("Lower","Upper"),each=length(PI))
  colnames(cover) <- c("PI","Ratio","Outliers","lwr","upr","Type")
  return(cover)}

npccal <- npc(obs,sim,PI)
npccal$Outliers <- factor(ifelse(npccal$Outliers==1,"Yes","No"))

npccal1<- npccal
  
sim <- ALLPCDATA2
obs <-datafit[!is.na(datafit$PD),]
head(sim)
head(obs)

percentiles <- c(50-PI/2,(50+PI/2))
nobs <- nrow(obs)
nrep <- nrow(sim)/nobs

PI <- c(0,20,40,50,60,80,90,95)
npc <- function (obs, sim,PI){
  nobs <- dim(obs)[1]
  nrep <- dim(sim)[1]/nobs
  percentiles <- c(50-PI/2,(50+PI/2))
  
  matsimfull <- matrix(sim$DV, ncol = nrep)
  matsimper <- t(apply(matsimfull,1,quantile,percentiles/100))
  matsimper_lower <- matsimper[,1:length(PI)]
  matsimper_upper <- matsimper[,(length(PI)+1):length(percentiles)]
  sim_outlower <- c()
  sim_outupper <- c()
  for (i in 1:length(PI)) {
    matsim_outlower <- ifelse(matsimfull<matsimper_lower[,i],1,0)
    sim_outlower <- cbind(sim_outlower,quantile(apply(matsim_outlower,2,mean),c(0.025,0.5,0.975)))
    matsim_outupper<- ifelse(matsimfull>matsimper_upper[,i],1,0)
    sim_outupper <- cbind(sim_outupper,quantile(apply(matsim_outupper,2,mean),c(0.025,0.5,0.975)))}
  yobs_lower <- yobs_upper <- matrix(rep(obs$PD,length(PI)),ncol=length(PI))
  yobs_lower <- ifelse(yobs_lower<matsimper_lower,1,0)
  yobs_upper <- ifelse(yobs_upper>matsimper_upper,1,0)
  obsper_outlower <- apply(yobs_lower,2,mean)
  obsper_outupper <- apply(yobs_upper,2,mean)
  lower_cover <- obsper_outlower/sim_outlower[2,]
  CI_lower_cover <- t(t(sim_outlower[c(1,3),])/sim_outlower[2,])
  upper_cover <- obsper_outupper/sim_outupper[2,]
  CI_upper_cover <- t(t(sim_outupper[c(1,3),])/sim_outupper[2,])
  lower_out <- (lower_cover<CI_lower_cover[1,])+(lower_cover>CI_lower_cover[2,])
  upper_out <- (upper_cover<CI_upper_cover[1,])+(upper_cover>CI_upper_cover[2,])
  cover <- as.data.frame(cbind(rep(PI,2),c(lower_cover,upper_cover),c(lower_out,upper_out),rbind(t(CI_lower_cover),t(CI_upper_cover))),row.names=F)
  cover$Type <- rep(c("Lower","Upper"),each=length(PI))
  colnames(cover) <- c("PI","Ratio","Outliers","lwr","upr","Type")
  return(cover)}

npccal <- npc(obs,sim,PI)
npccal$Outliers <- factor(ifelse(npccal$Outliers==1,"Yes","No"))
npccal2<- npccal


sim <- ALLPCDATA3
obs <-datafit[!is.na(datafit$PD),]
head(sim)
head(obs)

percentiles <- c(50-PI/2,(50+PI/2))
nobs <- nrow(obs)
nrep <- nrow(sim)/nobs

PI <- c(0,20,40,50,60,80,90,95)
npc <- function (obs, sim,PI){
  nobs <- dim(obs)[1]
  nrep <- dim(sim)[1]/nobs
  percentiles <- c(50-PI/2,(50+PI/2))
  
  matsimfull <- matrix(sim$DV, ncol = nrep)
  matsimper <- t(apply(matsimfull,1,quantile,percentiles/100))
  matsimper_lower <- matsimper[,1:length(PI)]
  matsimper_upper <- matsimper[,(length(PI)+1):length(percentiles)]
  sim_outlower <- c()
  sim_outupper <- c()
  for (i in 1:length(PI)) {
    matsim_outlower <- ifelse(matsimfull<matsimper_lower[,i],1,0)
    sim_outlower <- cbind(sim_outlower,quantile(apply(matsim_outlower,2,mean),c(0.025,0.5,0.975)))
    matsim_outupper<- ifelse(matsimfull>matsimper_upper[,i],1,0)
    sim_outupper <- cbind(sim_outupper,quantile(apply(matsim_outupper,2,mean),c(0.025,0.5,0.975)))}
  yobs_lower <- yobs_upper <- matrix(rep(obs$PD,length(PI)),ncol=length(PI))
  yobs_lower <- ifelse(yobs_lower<matsimper_lower,1,0)
  yobs_upper <- ifelse(yobs_upper>matsimper_upper,1,0)
  obsper_outlower <- apply(yobs_lower,2,mean)
  obsper_outupper <- apply(yobs_upper,2,mean)
  lower_cover <- obsper_outlower/sim_outlower[2,]
  CI_lower_cover <- t(t(sim_outlower[c(1,3),])/sim_outlower[2,])
  upper_cover <- obsper_outupper/sim_outupper[2,]
  CI_upper_cover <- t(t(sim_outupper[c(1,3),])/sim_outupper[2,])
  lower_out <- (lower_cover<CI_lower_cover[1,])+(lower_cover>CI_lower_cover[2,])
  upper_out <- (upper_cover<CI_upper_cover[1,])+(upper_cover>CI_upper_cover[2,])
  cover <- as.data.frame(cbind(rep(PI,2),c(lower_cover,upper_cover),c(lower_out,upper_out),rbind(t(CI_lower_cover),t(CI_upper_cover))),row.names=F)
  cover$Type <- rep(c("Lower","Upper"),each=length(PI))
  colnames(cover) <- c("PI","Ratio","Outliers","lwr","upr","Type")
  return(cover)}

npccal <- npc(obs,sim,PI)
npccal$Outliers <- factor(ifelse(npccal$Outliers==1,"Yes","No"))
npccal3<- npccal


sim <- ALLPCDATA4
obs <-datafit[!is.na(datafit$PD),]
head(sim)
head(obs)

percentiles <- c(50-PI/2,(50+PI/2))
nobs <- nrow(obs)
nrep <- nrow(sim)/nobs

PI <- c(0,20,40,50,60,80,90,95)
npc <- function (obs, sim,PI){
  nobs <- dim(obs)[1]
  nrep <- dim(sim)[1]/nobs
  percentiles <- c(50-PI/2,(50+PI/2))
  
  matsimfull <- matrix(sim$DV, ncol = nrep)
  matsimper <- t(apply(matsimfull,1,quantile,percentiles/100))
  matsimper_lower <- matsimper[,1:length(PI)]
  matsimper_upper <- matsimper[,(length(PI)+1):length(percentiles)]
  sim_outlower <- c()
  sim_outupper <- c()
  for (i in 1:length(PI)) {
    matsim_outlower <- ifelse(matsimfull<matsimper_lower[,i],1,0)
    sim_outlower <- cbind(sim_outlower,quantile(apply(matsim_outlower,2,mean),c(0.025,0.5,0.975)))
    matsim_outupper<- ifelse(matsimfull>matsimper_upper[,i],1,0)
    sim_outupper <- cbind(sim_outupper,quantile(apply(matsim_outupper,2,mean),c(0.025,0.5,0.975)))}
  yobs_lower <- yobs_upper <- matrix(rep(obs$PD,length(PI)),ncol=length(PI))
  yobs_lower <- ifelse(yobs_lower<matsimper_lower,1,0)
  yobs_upper <- ifelse(yobs_upper>matsimper_upper,1,0)
  obsper_outlower <- apply(yobs_lower,2,mean)
  obsper_outupper <- apply(yobs_upper,2,mean)
  lower_cover <- obsper_outlower/sim_outlower[2,]
  CI_lower_cover <- t(t(sim_outlower[c(1,3),])/sim_outlower[2,])
  upper_cover <- obsper_outupper/sim_outupper[2,]
  CI_upper_cover <- t(t(sim_outupper[c(1,3),])/sim_outupper[2,])
  lower_out <- (lower_cover<CI_lower_cover[1,])+(lower_cover>CI_lower_cover[2,])
  upper_out <- (upper_cover<CI_upper_cover[1,])+(upper_cover>CI_upper_cover[2,])
  cover <- as.data.frame(cbind(rep(PI,2),c(lower_cover,upper_cover),c(lower_out,upper_out),rbind(t(CI_lower_cover),t(CI_upper_cover))),row.names=F)
  cover$Type <- rep(c("Lower","Upper"),each=length(PI))
  colnames(cover) <- c("PI","Ratio","Outliers","lwr","upr","Type")
  return(cover)}

npccal <- npc(obs,sim,PI)
npccal$Outliers <- factor(ifelse(npccal$Outliers==1,"Yes","No"))
npccal4<- npccal

npccal1$ModelName <- "d.True model\n Turnover PD"
npccal2$ModelName <- "a.Misspecified delay\n Immediate effect"
npccal3$ModelName <- "b.Misspecified delay\n Effect compartment"
npccal4$ModelName <- "c.Misspecified delay and correlation\n Effect compartment, Full Omega"


npccal <- rbind(npccal1,npccal2,npccal3,npccal4 )
npccal$Type <- ifelse(npccal$Type=="Lower","Lower PI Limit","Upper PI Limit") 
write.csv(npccal,"npccal.csv")



