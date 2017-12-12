Supplementary R Codes


################
## PK Figures ##
################

require(reshape2)
require(dplyr)
require(ggplot2)
require(tidyr)
require(boot)
require(npde)
require(gridExtra)
require(proto)
require(plyr)
require(scales)
setwd("./PK_figures/")
source('./stat_plsmo.R')

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
  return(list(PI=PI,obs=obsx,theoper=theoper))
}


pcvpc <- function (obs, sim, bin,percentile,alpha,log.y)
{
  nrep <- dim(sim)[1]/dim(obs)[1]
  int.pred1 <- alpha/2
  int.pred2 <- 1-alpha/2
  #--create a cut (time intervals)
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

npc <- function (obs, sim,PI){
  nobs <- dim(obs)[1]
  nrep <- dim(sim)[1]/nobs
  percentiles <- c(50-PI/2,(50+PI/2))
  
  matsimfull <- matrix(sim$Conc, ncol = nrep)
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
  yobs_lower <- yobs_upper <- matrix(rep(obs$Conc,length(PI)),ncol=length(PI))
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
  cover$Type <- rep(c("Lower PI Limit","Upper PI Limit"),each=length(PI))
  colnames(cover) <- c("PI","Ratio","Outliers","lwr","upr","Type")
  return(cover)}


setwd(".\\PK_figures")


obs <- read.table("pkdata_5pt_180.csv",sep=";",header=T)
obs_1pt <- read.table("pkdata_1pt_180.csv",sep=";",header=T)
obs_2pt <- read.table("pkdata_2pt_180.csv",sep=";",header=T)
obs <- obs[-which(obs$AMT!="."),]
obs$AMT <- rep(rep(c(10,100,1000),each=5),each=60)
obs$Conc <- as.numeric(as.character(obs$Conc))
obs_1pt <- obs_1pt[-which(obs_1pt$AMT!="."),]
obs_1pt$AMT <- rep(rep(c(10,100,1000),each=1),each=60)
obs_1pt$Conc <- as.numeric(as.character(obs_1pt$Conc))
obs_2pt <- obs_2pt[-which(obs_2pt$AMT!="."),]
obs_2pt$AMT <- rep(rep(c(10,100,1000),each=2),each=60)
obs_2pt$Conc <- as.numeric(as.character(obs_2pt$Conc))


sim1 <- read.table(".\\True\\sim.txt",header=T,sep=";")
sim2 <- read.table(".\\False_Struc\\sim.txt",header=T,sep=";")
sim3 <- read.table(".\\False_Cov\\sim.txt",header=T,sep=";")
sim4 <- read.table(".\\False_Corr\\sim.txt",header=T,sep=";")
sim5 <- read.table(".\\False_Cons\\sim.txt",header=T,sep=";")
sim6 <- read.table(".\\False_Prop\\sim.txt",header=T,sep=";")

sim1 <- sim1[-which(sim1$AMT!=" . "),]
sim1$AMT <- rep(obs$AMT,1000)
sim1$Conc <- as.numeric(as.character(sim1$Conc))

sim2 <- sim2[-which(sim2$AMT!=" . "),]
sim2$AMT <- rep(obs$AMT,1000)
sim2$Conc <- as.numeric(as.character(sim2$Conc))

sim3 <- sim3[-which(sim3$AMT!=" . "),]
sim3$AMT <- rep(obs$AMT,1000)
sim3$Conc <- as.numeric(as.character(sim3$Conc))

sim4 <- sim4[-which(sim4$AMT!=" . "),]
sim4$AMT <- rep(obs$AMT,1000)
sim4$Conc <- as.numeric(as.character(sim4$Conc))

sim5 <- sim5[-which(sim5$AMT!=" . "),]
sim5$AMT <- rep(obs$AMT,1000)
sim5$Conc <- as.numeric(as.character(sim5$Conc))

sim6 <- sim6[-which(sim6$AMT!=" . "),]
sim6$AMT <- rep(obs$AMT,1000)
sim6$Conc <- as.numeric(as.character(sim6$Conc))


sim1 <- sim1[,c(2:8,1)]
sim2 <- sim2[,c(2:8,1)]
sim3 <- sim3[,c(2:8,1)]
sim4 <- sim4[,c(2:8,1)]
sim5 <- sim5[,c(2:8,1)]
sim6 <- sim6[,c(2:8,1)]

res1 <- read.table(".\\True\\Residuals.txt",header=T)
res2 <- read.table(".\\False_Struc\\Residuals.txt",header=T)
res3 <- read.table(".\\False_Cov\\Residuals.txt",header=T)
res4 <- read.table(".\\False_Corr\\Residuals.txt",header=T)
res5 <- read.table(".\\False_Cons\\Residuals.txt",header=T)
res6 <- read.table(".\\False_Prop\\Residuals.txt",header=T)
res1_1 <- read.table(".\\True_1pt\\Residuals.txt",header=T)
res1_2 <- read.table(".\\True_2pt\\Residuals.txt",header=T)
res2_1 <- read.table(".\\False_Struc_1pt\\Residuals.txt",header=T)
res2_2 <- read.table(".\\False_Struc_2pt\\Residuals.txt",header=T)
res3_1 <- read.table(".\\False_Cov_1pt\\Residuals.txt",header=T)
res3_2 <- read.table(".\\False_Cov_2pt\\Residuals.txt",header=T)


res <- plyr::rbind.fill(res1,res2,res3,res4,res5,res6)
res$Model <- rep(c("True model","Misspecified structural model","Misspecified covariate model",
                   "Misspecified correlation model","Constant error model",
                   "Proportional error model"),each=900)
res$Model <- as.factor(res$Model)
neworder <- c("True model","Misspecified structural model","Misspecified covariate model",
              "Misspecified correlation model","Constant error model",
              "Proportional error model")
res <- arrange(transform(res,Model=factor(Model,levels=neworder)),Model)
res$Obs <- rep(obs$Conc,6)
res$Sex[res$Sex==0] <- "Male"
res$Sex[res$Sex==1] <- "Female"




eta1 <- read.table(".\\True\\EBE.txt",header=T)
eta2 <- read.table(".\\False_Struc\\EBE.txt",header=T)
eta3 <- read.table(".\\False_Cov\\EBE.txt",header=T)
eta4 <- read.table(".\\False_Corr\\EBE.txt",header=T)
eta5 <- read.table(".\\False_Cons\\EBE.txt",header=T)
eta6 <- read.table(".\\False_Prop\\EBE.txt",header=T)
eta1_1 <- read.table(".\\True_1pt\\EBE.txt",header=T)
eta1_2 <- read.table(".\\True_2pt\\EBE.txt",header=T)
eta6_1 <- read.table(".\\False_Struc_1pt\\EBE.txt",header=T)
eta6_2 <- read.table(".\\False_Struc_2pt\\EBE.txt",header=T)
eta3_1 <- read.table(".\\False_Cov_1pt\\EBE.txt",header=T)
eta3_2 <- read.table(".\\False_Cov_2pt\\EBE.txt",header=T)

indivpar1 <- read.table(".\\True\\Indiv_parameters.txt",header=T)
indivpar2 <- read.table(".\\False_Struc\\Indiv_parameters.txt",header=T)
indivpar3 <- read.table(".\\False_Cov\\Indiv_parameters.txt",header=T)
indivpar4 <- read.table(".\\False_Corr\\Indiv_parameters.txt",header=T)
indivpar5 <- read.table(".\\False_Cons\\Indiv_parameters.txt",header=T)
indivpar6 <- read.table(".\\False_Prop\\Indiv_parameters.txt",header=T)
indivpar1_1 <- read.table(".\\True_1pt\\Indiv_parameters.txt",header=T)
indivpar1_2 <- read.table(".\\True_2pt\\Indiv_parameters.txt",header=T)
indivpar2_1 <- read.table(".\\False_Struc_1pt\\Indiv_parameters.txt",header=T)
indivpar2_2 <- read.table(".\\False_Struc_2pt\\Indiv_parameters.txt",header=T)
indivpar3_1 <- read.table(".\\False_Cov_1pt\\Indiv_parameters.txt",header=T)
indivpar3_2 <- read.table(".\\False_Cov_2pt\\Indiv_parameters.txt",header=T)



#### Figure 1 #####


datafig1 <- res[res$Model %in% c("True model","Misspecified structural model"),]

Figure_1A <- ggplot(datafig1,aes(x=PPRED,y=Obs))+geom_point(colour="blue")+
  geom_abline(intercept=0,slope=1,colour = "black", size = 1)+
  labs(x="Population predictions",y="Observations")+
  facet_wrap (~Model,ncol=2)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))
Figure_1B <- ggplot(datafig1,aes(x=IPRED,y=Obs))+geom_point(colour="blue")+
  geom_abline(intercept=0,slope=1,colour = "black", size = 1)+
  labs(x="Individual Predictions",y="Observations")+
  facet_wrap (~Model,ncol=2)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))

#ggsave(".\\Figure_1A.pdf",Figure_1A,width=30,height=12,units="cm",dpi=300)
#ggsave(".\\Figure_1B.pdf",Figure_1B,width=30,height=12,units="cm",dpi=300)
p <- arrangeGrob(Figure_1A,Figure_1B,ncol=1)
grid.draw(p)
ggsave(".\\Figure_1.pdf",p,width=20,height=20,units="cm",dpi=300)


##### Figure 2 #####

Fig2A_1 <- ggplot(res[res$Model %in% c("True model","Misspecified structural model"),],aes(x=Time,y=PWRES))+
  geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x="Time",y="PWRES")+
  facet_wrap (~Model)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))

Fig2A_2 <- ggplot(res[res$Model %in% c("True model","Misspecified structural model"),],aes(x=PPRED,y=PWRES))+
  geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x="Population prediction",y="PWRES")+
  facet_wrap (~Model)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))


Fig2B_1 <- ggplot(res[res$Model %in% c("Constant error model","Proportional error model"),],aes(x=Time,y=PWRES))+
  geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x="Time",y="PWRES")+
  facet_wrap (~Model)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))

Fig2B_2 <- ggplot(res[res$Model %in% c("Constant error model","Proportional error model"),],aes(x=PPRED,y=PWRES))+
  geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x="Population prediction",y="PWRES")+
  facet_wrap (~Model)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))



Figure_2C <- ggplot(res[res$Model %in% c("Misspecified covariate model","True model"),],aes(x=Time,y=PWRES))+
  geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x="Time",y="PWRES")+
  facet_grid(Sex ~ Model,labeller=mf_labeller)+
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(1,1,1.5,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(C)")+
  theme(plot.title = element_text(size=16, face="bold"))



p <- arrangeGrob(Fig2A_1,Fig2A_2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_2A.pdf",p,width=20,height=20,units="cm",dpi=300)

p <- arrangeGrob(Fig2B_1,Fig2B_2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_2B.pdf",p,width=20,height=20,units="cm",dpi=300)

Figure_2C
ggsave(".\\Figure_2C.pdf",Figure_2C,width=20,height=20,units="cm",dpi=300)

#### Figure 3 ####


Fig3A_1 <- ggplot(res[res$Model %in% c("True model","Misspecified structural model"),],aes(x=Time,y=IWRES))+
  geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x="Time",y="IWRES")+
  facet_wrap (~Model)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))

Fig3A_2 <- ggplot(res[res$Model %in% c("True model","Misspecified structural model"),],aes(x=IPRED,y=IWRES))+
  geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x="Individual prediction",y="IWRES")+
  facet_wrap (~Model)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))


Fig3B_1 <- ggplot(res[res$Model %in% c("Constant error model","Proportional error model"),],aes(x=Time,y=IWRES))+
  geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x="Time",y="IWRES")+
  facet_wrap (~Model)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))

Fig3B_2 <- ggplot(res[res$Model %in% c("Constant error model","Proportional error model"),],aes(x=IPRED,y=IWRES))+
  geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x="Individual prediction",y="IWRES")+
  facet_wrap (~Model)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))


p <- arrangeGrob(Fig3A_1,Fig3A_2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_3A.pdf",p,width=20,height=20,units="cm",dpi=300)

p <- arrangeGrob(Fig3B_1,Fig3B_2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_3B.pdf",p,width=20,height=20,units="cm",dpi=300)

#### Figure 4#####


bin <- c(0.25,0.75,2.5,8,18,30)
alpha <- 0.05
percentile <- c(0.1,0.5,0.9)
int.pred1 <- alpha/2
int.pred2 <- 1-alpha/2


vpc1 <- pcvpc(obs,sim1,bin,percentile,alpha,log.y=F)
vpc2 <- pcvpc(obs,sim2,bin,percentile,alpha,log.y=F)
vpc3 <- pcvpc(obs,sim3,bin,percentile,alpha,log.y=F)
vpc4 <- pcvpc(obs,sim4,bin,percentile,alpha,log.y=F)
vpc5 <- pcvpc(obs,sim5,bin,percentile,alpha,log.y=F)
vpc6 <- pcvpc(obs,sim6,bin,percentile,alpha,log.y=F)

vpcobs <- plyr::rbind.fill(vpc1$obs,vpc2$obs,vpc3$obs,vpc4$obs,vpc5$obs,vpc6$obs)

vpcobs$Model <- rep(neworder,each=dim(obs)[1]) 
vpcobs$Model <- as.factor(vpcobs$Model)
vpcobs <- arrange(transform(vpcobs,Model=factor(Model,levels=neworder)),Model)

vpcpi <- plyr::rbind.fill(vpc1$PI,vpc2$PI,vpc3$PI,vpc4$PI,vpc5$PI,vpc6$PI)
vpcpi$Model <- rep(neworder,each=length(unique(obs$Time))*3) 
vpcpi$Model <- as.factor(vpcpi$Model)
vpcpi <- arrange(transform(vpcpi,Model=factor(Model,levels=neworder)),Model)


Figure_4A <- ggplot(vpcpi[vpcpi$Model %in% c("True model","Misspecified structural model",
                                        "Constant error model",
                                        "Proportional error model"),] ,aes(x=meantime,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  geom_line(aes(y=theomedian,group=PI),linetype="dashed",color="black") +
  geom_point(data=vpcobs[vpcobs$Model %in% c("True model","Misspecified structural model",
                                               "Constant error model",
                                               "Proportional error model"),],aes(x=Time,y=pcConc,col=as.factor(AMT)),shape=1)+
  facet_wrap(~Model,ncol=2)+
  scale_colour_manual(values=c("#2c7fb8","#df65b0","#2ca65f","#2c7fb8","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 80% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("10% percentile","50% percentile","90% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("Prediction corrected concentration")+ xlab("Time")+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)),limits=c(0.1, 2700))+
  scale_x_continuous(breaks=seq(0,25,5))+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

ggsave(".\\Figure_4A.pdf",Figure_4A,width=20,height=20,units="cm",dpi=300)


vpc1_Sex0 <- pcvpc(obs[obs$Sex==0,],sim1[sim1$Sex==0,],bin,percentile,alpha,log.y=F)
vpc1_Sex1 <- pcvpc(obs[obs$Sex==1,],sim1[sim1$Sex==1,],bin,percentile,alpha,log.y=F)
vpc3_Sex0 <- pcvpc(obs[obs$Sex==0,],sim3[sim3$Sex==0,],bin,percentile,alpha,log.y=F)
vpc3_Sex1 <- pcvpc(obs[obs$Sex==1,],sim3[sim3$Sex==1,],bin,percentile,alpha,log.y=F)

vpcobssex <- plyr::rbind.fill(vpc1_Sex0$obs,vpc1_Sex1$obs,vpc3_Sex0$obs,vpc3_Sex1$obs)
neworder1 <- c("True model","Misspecified covariate model")
vpcobssex$Model <- rep(neworder1,each=dim(obs)[1]) 
vpcobssex$Model <- as.factor(vpcobssex$Model)
vpcobssex <- arrange(transform(vpcobssex,Model=factor(Model,levels=neworder)),Model)
vpcobssex$Sex[vpcobssex$Sex==0] <- "Male"
vpcobssex$Sex[vpcobssex$Sex==1] <- "Female"


vpcpisex <- plyr::rbind.fill(vpc1_Sex0$PI,vpc1_Sex1$PI,vpc3_Sex0$PI,vpc3_Sex1$PI)
vpcpisex$Model <- rep(neworder1,each=length(unique(obs$Time))*3*2) 
vpcpisex$Model <- as.factor(vpcpisex$Model)
vpcpisex <- arrange(transform(vpcpisex,Model=factor(Model,levels=neworder)),Model)
vpcpisex$Sex <- rep(c("Male","Female","Male","Female"),each=length(unique(obs$Time))*3) 


Figure_4B <- ggplot(vpcpisex ,aes(x=meantime,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  geom_line(aes(y=theomedian,group=PI),linetype="dashed",color="black") +
  geom_point(data=vpcobssex,aes(x=Time,y=pcConc,col=as.factor(AMT)),shape=1)+
  facet_grid(Sex~Model)+
  scale_colour_manual(values=c("#2c7fb8","#df65b0","#2ca65f","#2c7fb8","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 80% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("10% percentile","50% percentile","90% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("Prediction corrected concentration")+ xlab("Time")+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)),limits=c(0.1, 600))+
  scale_x_continuous(breaks=seq(0,25,5))+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

ggsave(".\\Figure_4B.pdf",Figure_4B,width=20,height=20,units="cm",dpi=300)



#### Figure 5 ####

source("./npde_2_modified/global.R")

# Classes
source("./npde_2_modified/NpdeData.R")
source("./npde_2_modified/NpdeRes.R")
source("./npde_2_modified/NpdeObject.R")

# Main function
source("./npde_2_modified/func_methods.R")
source("./npde_2_modified/func_plots_npd.R")
source("./npde_2_modified/main.R")

## Compute npd ##


npde1 <- autonpde(obs,sim1,iid = 1,ix = 2, iy = 3)
npde2 <- autonpde(obs,sim2,iid = 1,ix = 2, iy = 3)
npde3 <- autonpde(obs,sim3,iid = 1,ix = 2, iy = 3)
npde4 <- autonpde(obs,sim4,iid = 1,ix = 2, iy = 3)
npde5 <- autonpde(obs,sim5,iid = 1,ix = 2, iy = 3)
npde6 <- autonpde(obs,sim6,iid = 1,ix = 2, iy = 3)

### npd vs Time

npde1["prefs"]$vpc.interval <- 0.8
a1 <- npde.plot.meanprofile(npde1)
npdex1 <- data.frame(Time=rep(a1$xcalsim$bnds$xcent,3),CIlower=matrix(a1$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(a1$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a1$xcalsim$bnds$bmed,ncol=1),
                     median=matrix(a1$xcalobs$percobs,ncol=1))
npdex1$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdex1)[1]/3)
obsnpde1 <- a1$xcalobs$plmat

npde2["prefs"]$vpc.interval <- 0.8
a2 <- npde.plot.meanprofile(npde2)
npdex2 <- data.frame(Time=rep(a2$xcalsim$bnds$xcent,3),CIlower=matrix(a2$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(a2$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a2$xcalsim$bnds$bmed,ncol=1),
                     median=matrix(a2$xcalobs$percobs,ncol=1))
npdex2$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdex2)[1]/3)
obsnpde2 <- a2$xcalobs$plmat

npde3["prefs"]$vpc.interval <- 0.8
a3 <- npde.plot.meanprofile(npde3)
npdex3 <- data.frame(Time=rep(a3$xcalsim$bnds$xcent,3),CIlower=matrix(a3$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(a3$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a3$xcalsim$bnds$bmed,ncol=1),
                     median=matrix(a3$xcalobs$percobs,ncol=1))
npdex3$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdex3)[1]/3)
obsnpde3 <- a3$xcalobs$plmat

npde4["prefs"]$vpc.interval <- 0.8
a4 <- npde.plot.meanprofile(npde4)
npdex4 <- data.frame(Time=rep(a4$xcalsim$bnds$xcent,3),CIlower=matrix(a4$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(a4$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a4$xcalsim$bnds$bmed,ncol=1),
                     median=matrix(a4$xcalobs$percobs,ncol=1))
npdex4$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdex4)[1]/3)
obsnpde4 <- a4$xcalobs$plmat


npde5["prefs"]$vpc.interval <- 0.8
a5 <- npde.plot.meanprofile(npde5)
npdex5 <- data.frame(Time=rep(a5$xcalsim$bnds$xcent,3),CIlower=matrix(a5$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(a5$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a5$xcalsim$bnds$bmed,ncol=1),
                     median=matrix(a5$xcalobs$percobs,ncol=1))
npdex5$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdex5)[1]/3)
obsnpde5 <- a5$xcalobs$plmat


npde6["prefs"]$vpc.interval <- 0.8
a6 <- npde.plot.meanprofile(npde6)
npdex6 <- data.frame(Time=rep(a6$xcalsim$bnds$xcent,3),CIlower=matrix(a6$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(a6$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a6$xcalsim$bnds$bmed,ncol=1))
npdex6$median <-  matrix(a6$xcalobs$percobs,ncol=1)
npdex6$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdex6)[1]/3)
obsnpde6 <- a6$xcalobs$plmat


npdex1$Model <- "True model"
npdex2$Model <- "Misspecified structural model"
npdex3$Model <- "Misspecified covariate model"
npdex4$Model <- "Misspecified correlation model"
npdex5$Model <- "Constant error model"
npdex6$Model <- "Proportional error model"

obsnpde1$Model <- "True model"
obsnpde2$Model <- "Misspecified structural model"
obsnpde3$Model <- "Misspecified covariate model"
obsnpde4$Model <- "Misspecified correlation model"
obsnpde5$Model <- "Constant error model"
obsnpde6$Model <- "Proportional error model"
obsnpde1$AMT <- obsnpde2$AMT <- obsnpde3$AMT <- obsnpde4$AMT <- obsnpde5$AMT <- obsnpde6$AMT <- obs$AMT

npdex <- plyr::rbind.fill(npdex1,npdex2,npdex3,npdex4,npdex5,npdex6)
obsnpde<- plyr::rbind.fill(obsnpde1,obsnpde2,obsnpde3,obsnpde4,obsnpde5,obsnpde6)

npdex <- arrange(transform(npdex,Model=factor(Model,levels=neworder)),Model)
obsnpde <- arrange(transform(obsnpde,Model=factor(Model,levels=neworder)),Model)


#### npd versus prediction ####

npdepred1 <- npde1
npdepred1@data@data$Time <- res1$PPRED
a1 <- npde.plot.meanprofile(npdepred1)
obsnpdepred1 <- a1$xcalobs$plmat
npdepredx1 <- data.frame(Time=rep(c(a1$xcalsim$bnds$xcent,max(obsnpdepred1$x)),3),
                         CIlower=matrix(rbind(a1$xcalsim$bnds$binf,a1$xcalsim$bnds$binf[dim(a1$xcalsim$bnds$binf)[1],]),ncol=1),
                         CIupper=matrix(rbind(a1$xcalsim$bnds$bsup,a1$xcalsim$bnds$bsup[dim(a1$xcalsim$bnds$bsup)[1],]),ncol=1),
                         theomedian=matrix(rbind(a1$xcalsim$bnds$bmed,a1$xcalsim$bnds$bmed[dim(a1$xcalsim$bnds$bmed)[1],]),ncol=1),
                         median=matrix(rbind(a1$xcalobs$percobs,a1$xcalobs$percobs[dim(a1$xcalobs$percobs)[1],]),ncol=1))
npdepredx1$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdepredx1)[1]/3)


npdepred2 <- npde2
npdepred2@data@data$Time <- res2$PPRED
a2 <- npde.plot.meanprofile(npdepred2)
obsnpdepred2 <- a2$xcalobs$plmat
npdepredx2 <- data.frame(Time=rep(c(a2$xcalsim$bnds$xcent,max(obsnpdepred2$x)),3),
                         CIlower=matrix(rbind(a2$xcalsim$bnds$binf,a2$xcalsim$bnds$binf[dim(a2$xcalsim$bnds$binf)[1],]),ncol=1),
                         CIupper=matrix(rbind(a2$xcalsim$bnds$bsup,a2$xcalsim$bnds$bsup[dim(a2$xcalsim$bnds$bsup)[1],]),ncol=1),
                         theomedian=matrix(rbind(a2$xcalsim$bnds$bmed,a2$xcalsim$bnds$bmed[dim(a2$xcalsim$bnds$bmed)[1],]),ncol=1),
                         median=matrix(rbind(a2$xcalobs$percobs,a2$xcalobs$percobs[dim(a2$xcalobs$percobs)[1],]),ncol=1))
npdepredx2$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdepredx1)[1]/3)


npdepred3 <- npde3
npdepred3@data@data$Time <- res3$PPRED
a3 <- npde.plot.meanprofile(npdepred3)
obsnpdepred3 <- a3$xcalobs$plmat
npdepredx3 <- data.frame(Time=rep(c(a3$xcalsim$bnds$xcent,max(obsnpdepred3$x)),3),
                         CIlower=matrix(rbind(a3$xcalsim$bnds$binf,a3$xcalsim$bnds$binf[dim(a3$xcalsim$bnds$binf)[1],]),ncol=1),
                         CIupper=matrix(rbind(a3$xcalsim$bnds$bsup,a3$xcalsim$bnds$bsup[dim(a3$xcalsim$bnds$bsup)[1],]),ncol=1),
                         theomedian=matrix(rbind(a3$xcalsim$bnds$bmed,a3$xcalsim$bnds$bmed[dim(a3$xcalsim$bnds$bmed)[1],]),ncol=1),
                         median=matrix(rbind(a3$xcalobs$percobs,a3$xcalobs$percobs[dim(a3$xcalobs$percobs)[1],]),ncol=1))
npdepredx3$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdepredx1)[1]/3)

npdepred4 <- npde4
npdepred4@data@data$Time <- res4$PPRED
a4 <- npde.plot.meanprofile(npdepred4)
obsnpdepred4 <- a4$xcalobs$plmat
npdepredx4 <- data.frame(Time=rep(c(a4$xcalsim$bnds$xcent,max(obsnpdepred4$x)),3),
                         CIlower=matrix(rbind(a4$xcalsim$bnds$binf,a4$xcalsim$bnds$binf[dim(a4$xcalsim$bnds$binf)[1],]),ncol=1),
                         CIupper=matrix(rbind(a4$xcalsim$bnds$bsup,a4$xcalsim$bnds$bsup[dim(a4$xcalsim$bnds$bsup)[1],]),ncol=1),
                         theomedian=matrix(rbind(a4$xcalsim$bnds$bmed,a4$xcalsim$bnds$bmed[dim(a4$xcalsim$bnds$bmed)[1],]),ncol=1),
                         median=matrix(rbind(a4$xcalobs$percobs,a4$xcalobs$percobs[dim(a4$xcalobs$percobs)[1],]),ncol=1))
npdepredx4$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdepredx1)[1]/3)

npdepred5 <- npde5
npdepred5@data@data$Time <- res5$PPRED
a5 <- npde.plot.meanprofile(npdepred5)
obsnpdepred5 <- a5$xcalobs$plmat
npdepredx5 <- data.frame(Time=rep(c(a5$xcalsim$bnds$xcent,max(obsnpdepred5$x)),3),
                         CIlower=matrix(rbind(a5$xcalsim$bnds$binf,a5$xcalsim$bnds$binf[dim(a5$xcalsim$bnds$binf)[1],]),ncol=1),
                         CIupper=matrix(rbind(a5$xcalsim$bnds$bsup,a5$xcalsim$bnds$bsup[dim(a5$xcalsim$bnds$bsup)[1],]),ncol=1),
                         theomedian=matrix(rbind(a5$xcalsim$bnds$bmed,a5$xcalsim$bnds$bmed[dim(a5$xcalsim$bnds$bmed)[1],]),ncol=1),
                         median=matrix(rbind(a5$xcalobs$percobs,a5$xcalobs$percobs[dim(a5$xcalobs$percobs)[1],]),ncol=1))
npdepredx5$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdepredx1)[1]/3)

npdepred6 <- npde6
npdepred6@data@data$Time <- res6$PPRED
a6 <- npde.plot.meanprofile(npdepred6)
obsnpdepred6 <- a6$xcalobs$plmat
npdepredx6 <- data.frame(Time=rep(c(a6$xcalsim$bnds$xcent,max(obsnpdepred6$x)),3),
                         CIlower=matrix(rbind(a6$xcalsim$bnds$binf,a6$xcalsim$bnds$binf[dim(a6$xcalsim$bnds$binf)[1],]),ncol=1),
                         CIupper=matrix(rbind(a6$xcalsim$bnds$bsup,a6$xcalsim$bnds$bsup[dim(a6$xcalsim$bnds$bsup)[1],]),ncol=1),
                         theomedian=matrix(rbind(a6$xcalsim$bnds$bmed,a6$xcalsim$bnds$bmed[dim(a6$xcalsim$bnds$bmed)[1],]),ncol=1),
                         median=matrix(rbind(a6$xcalobs$percobs,a6$xcalobs$percobs[dim(a6$xcalobs$percobs)[1],]),ncol=1))
npdepredx6$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdepredx1)[1]/3)

npdepredx1$Model <- "True model"
npdepredx2$Model <- "Misspecified structural model"
npdepredx3$Model <- "Misspecified covariate model"
npdepredx4$Model <- "Misspecified correlation model"
npdepredx5$Model <- "Constant error model"
npdepredx6$Model <- "Proportional error model"

obsnpdepred1$Model <- "True model"
obsnpdepred2$Model <- "Misspecified structural model"
obsnpdepred3$Model <- "Misspecified covariate model"
obsnpdepred4$Model <- "Misspecified correlation model"
obsnpdepred5$Model <- "Constant error model"
obsnpdepred6$Model <- "Proportional error model"

obsnpdepred1$AMT <- obsnpdepred2$AMT <- obsnpdepred3$AMT <- obsnpdepred4$AMT <- obsnpdepred5$AMT <- obsnpdepred6$AMT <- obs$AMT

npdepredx <- plyr::rbind.fill(npdepredx1,npdepredx2,npdepredx3,npdepredx4,npdepredx5,npdepredx6)
obsnpdepred<- plyr::rbind.fill(obsnpdepred1,obsnpdepred2,obsnpdepred3,obsnpdepred4,obsnpdepred5,obsnpdepred6)

npdepredx <- arrange(transform(npdepredx,Model=factor(Model,levels=neworder)),Model)
obsnpdepred <- arrange(transform(obsnpdepred,Model=factor(Model,levels=neworder)),Model)



### Compute npd for two groups of Sex

obs_Sex0 <- obs[obs$Sex==0,]
obs_Sex1 <- obs[obs$Sex==1,]
sim1_Sex0 <- sim1[sim1$Sex==0,]
sim1_Sex1 <- sim1[sim1$Sex==1,]
sim3_Sex0 <- sim3[sim3$Sex==0,]
sim3_Sex1 <- sim3[sim3$Sex==1,]

npde1_Sex0 <- autonpde(obs_Sex0,sim1_Sex0,iid = 1,ix = 2, iy = 3)
npde1_Sex1 <- autonpde(obs_Sex1,sim1_Sex1,iid = 1,ix = 2, iy = 3)
npde3_Sex0 <- autonpde(obs_Sex0,sim3_Sex0,iid = 1,ix = 2, iy = 3)
npde3_Sex1 <- autonpde(obs_Sex1,sim3_Sex1,iid = 1,ix = 2, iy = 3)


### npd stratifying by Sex vs Time

npde1_Sex0["prefs"]$vpc.interval <- 0.8
a1 <- npde.plot.meanprofile(npde1_Sex0)
npdex1_Sex0 <- data.frame(Time=rep(a1$xcalsim$bnds$xcent,3),CIlower=matrix(a1$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(a1$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a1$xcalsim$bnds$bmed,ncol=1),
                     median=matrix(a1$xcalobs$percobs,ncol=1))
npdex1_Sex0$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdex1_Sex0)[1]/3)
obsnpde1_Sex0 <- a1$xcalobs$plmat


npde1_Sex1["prefs"]$vpc.interval <- 0.8
a1 <- npde.plot.meanprofile(npde1_Sex1)
npdex1_Sex1 <- data.frame(Time=rep(a1$xcalsim$bnds$xcent,3),CIlower=matrix(a1$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(a1$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a1$xcalsim$bnds$bmed,ncol=1),
                     median=matrix(a1$xcalobs$percobs,ncol=1))
npdex1_Sex1$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdex1_Sex0)[1]/3)
obsnpde1_Sex1 <- a1$xcalobs$plmat

npde3_Sex0["prefs"]$vpc.interval <- 0.8
a3 <- npde.plot.meanprofile(npde3_Sex0)
npdex3_Sex0 <- data.frame(Time=rep(a3$xcalsim$bnds$xcent,3),CIlower=matrix(a3$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(a3$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a3$xcalsim$bnds$bmed,ncol=1),
                     median=matrix(a3$xcalobs$percobs,ncol=1))
npdex3_Sex0$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdex3_Sex0)[1]/3)
obsnpde3_Sex0 <- a3$xcalobs$plmat

npde3_Sex1["prefs"]$vpc.interval <- 0.8
a3 <- npde.plot.meanprofile(npde3_Sex1)
npdex3_Sex1 <- data.frame(Time=rep(a3$xcalsim$bnds$xcent,3),CIlower=matrix(a3$xcalsim$bnds$binf,ncol=1),
                          CIupper=matrix(a3$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a3$xcalsim$bnds$bmed,ncol=1),
                          median=matrix(a3$xcalobs$percobs,ncol=1))
npdex3_Sex1$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdex3_Sex1)[1]/3)
obsnpde3_Sex1 <- a3$xcalobs$plmat

npdex1_Sex0$Model <- "True model"
npdex1_Sex1$Model <- "True model"
npdex3_Sex0$Model <- "Misspecified covariate model"
npdex3_Sex1$Model <- "Misspecified covariate model"


obsnpde1_Sex0$Model <- "True model"
obsnpde1_Sex1$Model <- "True model"
obsnpde3_Sex0$Model <- "Misspecified covariate model"
obsnpde3_Sex1$Model <- "Misspecified covariate model"

obsnpde1_Sex0$Sex <- obsnpde3_Sex0$Sex <- npdex1_Sex0$Sex <-npdex3_Sex0$Sex <-"Male"
obsnpde1_Sex1$Sex <- obsnpde3_Sex1$Sex <- npdex1_Sex1$Sex <-npdex3_Sex1$Sex <-"Female"
 

obsnpde1_Sex0$AMT <- obsnpde3_Sex0$AMT  <- obs_Sex0$AMT
obsnpde1_Sex1$AMT <- obsnpde3_Sex1$AMT  <- obs_Sex1$AMT

npdex_Sex <- plyr::rbind.fill(npdex1_Sex0,npdex1_Sex1,npdex3_Sex0,npdex3_Sex1)
obsnpde_Sex<- plyr::rbind.fill(obsnpde1_Sex0,obsnpde1_Sex1,obsnpde3_Sex0,obsnpde3_Sex1)

npdex_Sex <- arrange(transform(npdex_Sex,Model=factor(Model,levels=c("True model","Misspecified covariate model"))),Model)
obsnpde_Sex <- arrange(transform(obsnpde_Sex,Model=factor(Model,levels=c("True model","Misspecified covariate model"))),Model)


source("./npde_2_modified/func_plots.R")


### npde vs Time

a1 <- npde.plot.meanprofile(npde1)
rnpdex1 <- data.frame(Time=rep(a1$xcalsim$bnds$xcent,3),CIlower=matrix(a1$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(a1$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a1$xcalsim$bnds$bmed,ncol=1),
                     median=matrix(a1$xcalobs$percobs,ncol=1))
rnpdex1$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdex1)[1]/3)
robsnpde1 <- a1$xcalobs$plmat


a2 <- npde.plot.meanprofile(npde2)
rnpdex2 <- data.frame(Time=rep(a2$xcalsim$bnds$xcent,3),CIlower=matrix(a2$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(a2$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a2$xcalsim$bnds$bmed,ncol=1),
                     median=matrix(a2$xcalobs$percobs,ncol=1))
rnpdex2$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(rnpdex2)[1]/3)
robsnpde2 <- a2$xcalobs$plmat


a5 <- npde.plot.meanprofile(npde5)
rnpdex5 <- data.frame(Time=rep(a5$xcalsim$bnds$xcent,3),CIlower=matrix(a5$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(a5$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a5$xcalsim$bnds$bmed,ncol=1),
                     median=matrix(a5$xcalobs$percobs,ncol=1))
rnpdex5$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(rnpdex5)[1]/3)
robsnpde5 <- a5$xcalobs$plmat


a6 <- npde.plot.meanprofile(npde6)
rnpdex6 <- data.frame(Time=rep(a6$xcalsim$bnds$xcent,3),CIlower=matrix(a6$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(a6$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a6$xcalsim$bnds$bmed,ncol=1))
rnpdex6$median <-  matrix(a6$xcalobs$percobs,ncol=1)
rnpdex6$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(rnpdex6)[1]/3)
robsnpde6 <- a6$xcalobs$plmat


rnpdex1$Model <- "True model"
rnpdex2$Model <- "Misspecified structural model"
rnpdex5$Model <- "Constant error model"
rnpdex6$Model <- "Proportional error model"

robsnpde1$Model <- "True model"
robsnpde2$Model <- "Misspecified structural model"
robsnpde5$Model <- "Constant error model"
robsnpde6$Model <- "Proportional error model"
robsnpde1$AMT <- robsnpde2$AMT <-robsnpde5$AMT <- robsnpde6$AMT <- obs$AMT

rnpdex <- plyr::rbind.fill(rnpdex1,rnpdex2,rnpdex5,rnpdex6)
robsnpde<- plyr::rbind.fill(robsnpde1,robsnpde2,robsnpde5,robsnpde6)

rnpdex <- arrange(transform(rnpdex,Model=factor(Model,levels=c("True model","Misspecified structural model",
                                                               "Constant error model","Proportional error model"))),Model)
robsnpde <- arrange(transform(robsnpde,Model=factor(Model,levels=c("True model","Misspecified structural model",
                                                                   "Constant error model","Proportional error model"))),Model)

### npde vs Time (stratifying by covariate)

a1 <- npde.plot.meanprofile(npde1_Sex0)
rnpdex1_Sex0 <- data.frame(Time=rep(a1$xcalsim$bnds$xcent,3),CIlower=matrix(a1$xcalsim$bnds$binf,ncol=1),
                          CIupper=matrix(a1$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a1$xcalsim$bnds$bmed,ncol=1),
                          median=matrix(a1$xcalobs$percobs,ncol=1))
rnpdex1_Sex0$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdex1_Sex0)[1]/3)
robsnpde1_Sex0 <- a1$xcalobs$plmat


a1 <- npde.plot.meanprofile(npde1_Sex1)
rnpdex1_Sex1 <- data.frame(Time=rep(a1$xcalsim$bnds$xcent,3),CIlower=matrix(a1$xcalsim$bnds$binf,ncol=1),
                          CIupper=matrix(a1$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a1$xcalsim$bnds$bmed,ncol=1),
                          median=matrix(a1$xcalobs$percobs,ncol=1))
rnpdex1_Sex1$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdex1_Sex0)[1]/3)
robsnpde1_Sex1 <- a1$xcalobs$plmat

a3 <- npde.plot.meanprofile(npde3_Sex0)
rnpdex3_Sex0 <- data.frame(Time=rep(a3$xcalsim$bnds$xcent,3),CIlower=matrix(a3$xcalsim$bnds$binf,ncol=1),
                          CIupper=matrix(a3$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a3$xcalsim$bnds$bmed,ncol=1),
                          median=matrix(a3$xcalobs$percobs,ncol=1))
rnpdex3_Sex0$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdex3_Sex0)[1]/3)
robsnpde3_Sex0 <- a3$xcalobs$plmat


a3 <- npde.plot.meanprofile(npde3_Sex1)
rnpdex3_Sex1 <- data.frame(Time=rep(a3$xcalsim$bnds$xcent,3),CIlower=matrix(a3$xcalsim$bnds$binf,ncol=1),
                          CIupper=matrix(a3$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a3$xcalsim$bnds$bmed,ncol=1),
                          median=matrix(a3$xcalobs$percobs,ncol=1))
rnpdex3_Sex1$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(npdex3_Sex1)[1]/3)
robsnpde3_Sex1 <- a3$xcalobs$plmat

rnpdex1_Sex0$Model <- "True model"
rnpdex1_Sex1$Model <- "True model"
rnpdex3_Sex0$Model <- "Misspecified covariate model"
rnpdex3_Sex1$Model <- "Misspecified covariate model"


robsnpde1_Sex0$Model <- "True model"
robsnpde1_Sex1$Model <- "True model"
robsnpde3_Sex0$Model <- "Misspecified covariate model"
robsnpde3_Sex1$Model <- "Misspecified covariate model"

robsnpde1_Sex0$Sex <- robsnpde3_Sex0$Sex <- rnpdex1_Sex0$Sex <-rnpdex3_Sex0$Sex <-"Male"
robsnpde1_Sex1$Sex <- robsnpde3_Sex1$Sex <- rnpdex1_Sex1$Sex <-rnpdex3_Sex1$Sex <-"Female"


robsnpde1_Sex0$AMT <- robsnpde3_Sex0$AMT  <- obs_Sex0$AMT
robsnpde1_Sex1$AMT <- robsnpde3_Sex1$AMT  <- obs_Sex1$AMT

rnpdex_Sex <- plyr::rbind.fill(rnpdex1_Sex0,rnpdex1_Sex1,rnpdex3_Sex0,rnpdex3_Sex1)
robsnpde_Sex<- plyr::rbind.fill(robsnpde1_Sex0,robsnpde1_Sex1,robsnpde3_Sex0,robsnpde3_Sex1)

rnpdex_Sex <- arrange(transform(rnpdex_Sex,Model=factor(Model,levels=c("True model","Misspecified covariate model"))),Model)
robsnpde_Sex <- arrange(transform(robsnpde_Sex,Model=factor(Model,levels=c("True model","Misspecified covariate model"))),Model)


### graphs

Figure_5A1 <- ggplot(npdex[npdex$Model %in% c("True model","Misspecified structural model"),] ,aes(x=Time,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  facet_wrap(~Model,ncol=2)+
  geom_point(data=obsnpde[obsnpde$Model %in% c("True model","Misspecified structural model"),],aes(x=x,y=y,col=as.factor(AMT)),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#2c7fb8","#df65b0","#2ca65f","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("10% percentile","50% percentile","90% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("npd")+ xlab("Time")+
  scale_y_continuous(breaks=seq(-3.5,3.5,1))+scale_x_continuous(breaks=seq(0,25,5))+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

Figure_5A2 <- ggplot(npdepredx[npdepredx$Model %in% c("True model","Misspecified structural model"),] ,aes(x=Time,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  facet_wrap(~Model,ncol=2)+
  geom_point(data=obsnpdepred[obsnpdepred$Model %in% c("True model","Misspecified structural model"),],aes(x=x,y=y,col=as.factor(AMT)),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#2c7fb8","#df65b0","#2ca65f","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("5% percentile","50% percentile","95% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("npd")+ xlab("Population predictions")+
  scale_y_continuous(breaks=seq(-3.5,3.5,1))+ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)


p <- arrangeGrob(Figure_5A1,Figure_5A2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_5A.pdf",p,width=24,height=24,units="cm",dpi=300)


Figure_5B1 <- ggplot(npdex[npdex$Model %in% c("Constant error model","Proportional error model"),] ,aes(x=Time,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  facet_wrap(~Model,ncol=2)+
  geom_point(data=obsnpde[obsnpde$Model %in% c("Constant error model","Proportional error model"),],aes(x=x,y=y,col=as.factor(AMT)),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#2c7fb8","#df65b0","#2ca65f","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("5% percentile","50% percentile","95% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("npd")+ xlab("Time")+
  scale_y_continuous(breaks=seq(-3.5,3.5,1))+scale_x_continuous(breaks=seq(0,25,5))+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

Figure_5B2 <- ggplot(npdepredx[npdepredx$Model %in% c("Constant error model","Proportional error model"),] ,aes(x=Time,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  facet_wrap(~Model,ncol=2)+
  geom_point(data=obsnpdepred[obsnpdepred$Model %in% c("Constant error model","Proportional error model"),],aes(x=x,y=y,col=as.factor(AMT)),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#2c7fb8","#df65b0","#2ca65f","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("5% percentile","50% percentile","95% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("npd")+ xlab("Population predictions")+
  scale_y_continuous(breaks=seq(-3.5,3.5,1))+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)


p <- arrangeGrob(Figure_5B1,Figure_5B2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_5B.pdf",p,width=24,height=24,units="cm",dpi=300)


Figure_5C <- ggplot(npdex_Sex ,aes(x=Time,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  facet_grid(Sex~Model)+
  #geom_line(aes(y=theomedian,group=PI),linetype="dashed",color="black") +
  geom_point(data=obsnpde_Sex,aes(x=x,y=y,col=as.factor(AMT)),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#2c7fb8","#df65b0","#2ca65f","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("5% percentile","50% percentile","95% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("npd")+ xlab("Time")+
  scale_y_continuous(breaks=seq(-3.5,3.5,1))+scale_x_continuous(breaks=seq(0,25,5))+
  ggtitle("(C)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

ggsave(".\\Figure_5C.pdf",Figure_5C,width=24,height=24,units="cm",dpi=300)



Figure_5D <- ggplot(rnpdex ,aes(x=Time,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  facet_wrap(~Model)+
  #geom_line(aes(y=theomedian,group=PI),linetype="dashed",color="black") +
  geom_point(data=robsnpde,aes(x=x,y=y,col=as.factor(AMT)),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#2c7fb8","#df65b0","#2ca65f","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("5% percentile","50% percentile","95% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("npde")+ xlab("Time")+
  scale_y_continuous(breaks=seq(-3.5,3.5,1))+scale_x_continuous(breaks=seq(0,25,5))+
  ggtitle("(D)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

ggsave(".\\Figure_5D.pdf",Figure_5D,width=24,height=24,units="cm",dpi=300)



Figure_5E <- ggplot(rnpdex_Sex ,aes(x=Time,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  facet_grid(Sex~Model)+
  #geom_line(aes(y=theomedian,group=PI),linetype="dashed",color="black") +
  geom_point(data=robsnpde_Sex,aes(x=x,y=y,col=as.factor(AMT)),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#2c7fb8","#df65b0","#2ca65f","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("5% percentile","50% percentile","95% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("npde")+ xlab("Time")+
  scale_y_continuous(breaks=seq(-3.5,3.5,1))+scale_x_continuous(breaks=seq(0,25,5))+
  ggtitle("(E)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

ggsave(".\\Figure_5E.pdf",Figure_5E,width=24,height=24,units="cm",dpi=300)

#### Supplementary figures
# Figure S1

simtrue<-read.table("simtrue.txt",header=T)
simfstruc<-read.table("simfstruc.txt",header=T)
simfcov<-read.table("simfcov.txt",header=T)
simfcorr<-read.table("simfcorr.txt",header=T)
simfconst<-read.table("simfconst.txt",header=T)
simfprop<-read.table("simfprop.txt",header=T)
simfstruc <- simfstruc[,-3]
colnames(simfstruc) <- c("ID","Time","Conc")
simfstruc$Dose <- simtrue$Dose
simfcov$Dose <- simtrue$Dose
simfcorr$Dose <- simtrue$Dose
simfconst$Dose <- simtrue$Dose
simfprop$Dose <- simtrue$Dose

Dose1 <- c(apply(matrix(simtrue$Conc[simtrue$Dose==10],ncol=6000),1,median),
           apply(matrix(simfstruc$Conc[simfstruc$Dose==10],ncol=6000),1,median),
           apply(matrix(simfcov$Conc[simfcov$Dose==10],ncol=6000),1,median),
           apply(matrix(simfcorr$Conc[simfcorr$Dose==10],ncol=6000),1,median),
           apply(matrix(simfconst$Conc[simfconst$Dose==10],ncol=6000),1,median),
           apply(matrix(simfprop$Conc[simfprop$Dose==10],ncol=6000),1,median))
Dose2 <- c(apply(matrix(simtrue$Conc[simtrue$Dose==100],ncol=6000),1,median),
           apply(matrix(simfstruc$Conc[simfstruc$Dose==100],ncol=6000),1,median),
           apply(matrix(simfcov$Conc[simfcov$Dose==100],ncol=6000),1,median),
           apply(matrix(simfcorr$Conc[simfcorr$Dose==100],ncol=6000),1,median),
           apply(matrix(simfconst$Conc[simfconst$Dose==100],ncol=6000),1,median),
           apply(matrix(simfprop$Conc[simfprop$Dose==100],ncol=6000),1,median))
Dose3 <- c(apply(matrix(simtrue$Conc[simtrue$Dose==1000],ncol=6000),1,median),
           apply(matrix(simfstruc$Conc[simfstruc$Dose==1000],ncol=6000),1,median),
           apply(matrix(simfcov$Conc[simfcov$Dose==1000],ncol=6000),1,median),
           apply(matrix(simfcorr$Conc[simfcorr$Dose==1000],ncol=6000),1,median),
           apply(matrix(simfconst$Conc[simfconst$Dose==1000],ncol=6000),1,median),
           apply(matrix(simfprop$Conc[simfprop$Dose==1000],ncol=6000),1,median))
Dose1 <- data.frame(Time=seq(0,24,1),Value=Dose1,Model=rep(neworder,each=25))
Dose1$AMT1 <- "Dose = 10 mg"
Dose2 <- data.frame(Time=seq(0,24,1),Value=Dose2,Model=rep(neworder,each=25))
Dose2$AMT1 <- "Dose = 100 mg"
Dose3 <- data.frame(Time=seq(0,24,1),Value=Dose3,Model=rep(neworder,each=25))
Dose3$AMT1 <- "Dose = 1000 mg"

sim3dose <- plyr::rbind.fill(Dose1,Dose2,Dose3)
sim3dose <- arrange(transform(sim3dose,Model=factor(Model,levels=neworder)),Model)
obs$AMT1 <- "Dose = 10 mg"
obs$AMT1[obs$AMT==100] <- "Dose = 100 mg"
obs$AMT1[obs$AMT==1000] <- "Dose = 1000 mg"

Figure_S1 <- ggplot(obs,aes(x=Time,y=Conc,group=ID))+geom_line(colour="gray60")+scale_y_log10()+
  geom_line(data=sim3dose,aes(x=Time,y=Value,group=Model,colour=Model),lwd=1,show_guide=F)+
  labs(x="Time",y="Concentration")+
  facet_wrap(~AMT1)+
  scale_colour_manual(values = c("Blue","Red","Green","Orange","Magenta","Black"),
                     # breaks=c("A","B","C","D","E","F"),
                      labels=c("True model","Misspecified structural model","Misspecified covariate model",
                               "Misspecified correlation model","Constant error model",
                               "Proportional error model"))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),strip.text = element_text(size=14))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  #theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))

ggsave(".\\Figure_S1.pdf",Figure_S1,width=30,height=12,units="cm",dpi=300)

### Figure_S3


library(deSolve)
pk2comp <- function(t,y,p) {
  k <- p[1]/p[2]
  V <- p[2]
  k12 <-p[3]/V
  k21 <- p[3]/p[4]
  dy1 <- -k12*y[1]+k21*y[2]-k*y[1]
  dy2 <- -k21*y[2]+k12*y[1]
  Cc <- y[1]/V 
  list(c(dy1,dy2),c(Cc))}

pk1comp <- function(t,y,p) {
  k <- p[2]/p[1]
  V <- p[1]
  dy1 <- -k*y[1]
  Cc <- y[1]/V 
  list(c(dy1),c(Cc))}

t <- seq(0,25,0.1)
id <- 3
condinit<- c(y1=10,y2=0)

ipred1 <-lsoda(func=pk2comp,times=t,y=condinit,parms=indivpar1[indivpar1$ID==id,2:5],hmax=0.1)[,c(1,4)]
ipred2 <-lsoda(func=pk1comp,times=t,y=c(y1=10),parms=indivpar2[indivpar2$ID==id,2:3],hmax=0.1)[,c(1,3)]
ipred3 <-lsoda(func=pk2comp,times=t,y=condinit,parms=indivpar3[indivpar3$ID==id,2:5],hmax=0.1)[,c(1,4)]
ipred4 <-lsoda(func=pk2comp,times=t,y=condinit,parms=indivpar4[indivpar4$ID==id,2:5],hmax=0.1)[,c(1,4)]
ipred5 <-lsoda(func=pk2comp,times=t,y=condinit,parms=indivpar5[indivpar5$ID==id,2:5],hmax=0.1)[,c(1,4)]
ipred6 <-lsoda(func=pk2comp,times=t,y=condinit,parms=indivpar6[indivpar6$ID==id,2:5],hmax=0.1)[,c(1,4)]

colnames(ipred2) <- colnames(ipred1)
ipred <- data.frame(rbind(ipred1,ipred2,ipred3,ipred4,ipred5,ipred6))
ipred$Model <- rep(c("True model","Misspecified structural model","Misspecified covariate model",
                     "Misspecified correlation model","Constant error model",
                     "Proportional error model"),each=length(t))
ipred$Model <- as.factor(ipred$Model)
ipred <- arrange(transform(ipred,Model=factor(Model,levels=neworder)),Model)

iobs <- obs[obs$ID==id,]

Figure_S3 <- ggplot(ipred[ipred$Model %in% c("True model","Misspecified structural model","Constant error model",
                                        "Proportional error model"),],aes(x=time,y=V1))+
  geom_line(colour="green")+
  labs(x="Time",y="Concentration")+
  geom_point(data=iobs,aes(x=Time,y=Conc),color="blue")+
  facet_wrap (~Model,ncol=2)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

ggsave(".\\Figure_S3.pdf",Figure_S3,width=30,height=24,units="cm",dpi=300)


#### Figure_S4

# Variance - covariance matrix of random effects of the true model
corr_true <- diag(c(0.448,0.482,0.473,0.413)^2) 
corr_true[1,2] <- corr_true[2,1] <- 0.438*0.448*0.482
# Variance - covariance matrix of random effects of the false random effect model
corr_wrong <- diag(c(0.45,0.478,0.473,0.403)^2)

### Decorrelate random effects
var.eig <- eigen(corr_true)
xmat <- var.eig$vectors %*% diag(sqrt(var.eig$values)) %*% solve(var.eig$vectors)
ymat<-try(solve(xmat))
deceta1<- as.data.frame(as.matrix(eta1[,2:5]) %*% ymat)

var.eig <- eigen(corr_wrong)
xmat <- var.eig$vectors %*% diag(sqrt(var.eig$values)) %*% solve(var.eig$vectors)
ymat<-try(solve(xmat))
deceta4<- as.data.frame(as.matrix(eta4[,2:5]) %*% ymat)

colnames(deceta1) <- colnames(deceta4) <-c("CL","V1","Q","V2")
neworder <- c("True model","Misspecified correlation model")

eta<- plyr::rbind.fill(eta1,eta4)
eta$Model <- rep(c("True model","Misspecified correlation model"),each=dim(eta1)[1])
eta$Model <- as.factor(eta$Model)
eta <- arrange(transform(eta,Model=factor(Model,levels=neworder)),Model)
slope <- data.frame(cor=c(0.438,0),Model=c("True model","Misspecified correlation model"))
eta <- merge(eta,slope,by="Model")
eta$Model <- as.factor(eta$Model)
eta <- arrange(transform(eta,Model=factor(Model,levels=neworder)),Model)

deceta <- plyr::rbind.fill(deceta1,deceta4)
deceta$Model <- rep(c("True model","Misspecified correlation model"),each=dim(eta1)[1])
deceta$Model <- as.factor(deceta$Model)
deceta <- arrange(transform(deceta,Model=factor(Model,levels=neworder)),Model)

Figure_S4_1 <- ggplot(eta,aes(x=CL,y=V1))+geom_point(colour="blue")+
  geom_abline(aes(intercept=0,slope=cor),colour = "black", size = 1)+
  labs(x=expression(eta[CL]),y=expression(eta[V[1]]))+
  facet_wrap (~Model,ncol=2)+
  stat_smooth(method="lm",se=F,size=2,color="red",linetype="dashed")+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))

Figure_S4_2 <- ggplot(deceta,aes(x=CL,y=V1))+geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x=expression(paste("Decorrelated ",eta[CL])),y=expression(paste("Decorrelated ",eta[V[1]])))+
  facet_wrap (~Model,ncol=2)+
  stat_smooth(method="lm",se=F,size=2,color="red",linetype="dashed")+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))

p <- arrangeGrob(Figure_S4_1,Figure_S4_2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S4.pdf",p,width=20,height=20,units="cm",dpi=300)

#### Figure_S5

eta<- plyr::rbind.fill(eta1,eta3)
eta$Model <- rep(c("True model","Misspecified covariate model"),each=dim(eta1)[1])
eta$Model <- as.factor(eta$Model)
eta <- arrange(transform(eta,Model=factor(Model,levels=c("True model","Misspecified covariate model"))),Model)

indivpar<- plyr::rbind.fill(indivpar1,indivpar3)
indivpar$Model <- rep(c("True model","Misspecified covariate model"),each=dim(eta1)[1])
indivpar$Model <- as.factor(indivpar$Model)
indivpar <- arrange(transform(indivpar,Model=factor(Model,levels=c("True model","Misspecified covariate model"))),Model)
indivpar[,2:5] <- log(indivpar[,2:5])

Figure_S5_A1 <- ggplot(indivpar, aes(x=as.factor(Sex), y=CL)) + geom_boxplot(color="blue")+
  labs(x="Sex",y="log(CL)")+
  facet_wrap (~Model,ncol=2)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))


Figure_S5_A2 <- ggplot(indivpar, aes(x=T_Weight, y=V1)) + geom_point(colour="blue")+
  labs(x="T_Weight",y=expression(bold("log("~V[1]~")")))+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  facet_wrap (~Model,ncol=2)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))


Figure_S5_B1 <- ggplot(eta, aes(x=as.factor(Sex), y=CL)) + geom_boxplot(color="blue")+
  labs(x="Sex",y=expression(eta[CL]))+
  geom_hline(yintercept=0,colour = "black", size = 1)+
  facet_wrap (~Model,ncol=2)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))

Figure_S5_B2 <- ggplot(eta, aes(x=T_Weight, y=V1)) + geom_point(colour="blue")+
  labs(x="T_Weight",y=expression(bold("log("~eta[V[1]]~")")))+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  facet_wrap (~Model,ncol=2)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))

p <- arrangeGrob(Figure_S5_A1,Figure_S5_A2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S5A.pdf",p,width=20,height=20,units="cm",dpi=300)
p <- arrangeGrob(Figure_S5_B1,Figure_S5_B2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S5B.pdf",p,width=20,height=20,units="cm",dpi=300)



#### Figure S6

etatruedesign<- plyr::rbind.fill(eta1,eta1_2,eta1_1)
etatruedesign$Design <- rep(c("A","B","C"),each=dim(eta1)[1])

mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="Design") { 
    value[value=="A"] <- paste("Standard design\n","\u03B7","-Shrinkage (CL) = 9%\n","\u03B7","-Shrinkage (V1) = 11%",sep="")
    value[value=="B"] <- paste("Sparse design\n","\u03B7","-Shrinkage (CL) = 26%\n","\u03B7","-Shrinkage (V1) = 39%",sep="")
    value[value=="C"] <- paste("Very sparse design\n","\u03B7","-Shrinkage (CL) = 64%\n","\u03B7","-Shrinkage (V1) = 61%",sep="")
  }
  return(value)
}
Figure_S6A <- ggplot(etatruedesign, aes(x=CL, y=V1)) + geom_point(colour="blue")+
  labs(x=expression(eta[CL]),y=expression(eta[V[1]]))+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  facet_grid(~Design,labeller=mf_labeller)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))
ggsave(".\\Figure_S6A.pdf",Figure_S6A,width=30,height=12,units="cm",dpi=300)

etafalsedesign<- plyr::rbind.fill(eta3,eta3_2,eta3_1)
etafalsedesign$Design <- rep(c("A","B","C"),each=dim(eta3)[1])

mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="Design") { 
    value[value=="A"] <- paste("Standard design\n","\u03B7","-Shrinkage (CL) = 8%\n","\u03B7","-Shrinkage (V1) = 11%",sep="")
    value[value=="B"] <- paste("Sparse design\n","\u03B7","-Shrinkage (CL) = 23%\n","\u03B7","-Shrinkage (V1) = 35%",sep="")
    value[value=="C"] <- paste("Very sparse design\n","\u03B7","-Shrinkage (CL) = 60%\n","\u03B7","-Shrinkage (V1) = 69%",sep="")
  }
  return(value)
}
Figure_S6_B1 <- ggplot(etafalsedesign, aes(x=as.factor(Sex), y=CL)) + 
  geom_boxplot(colour="blue",show_guide = F)+
  labs(x="Sex",y=expression(eta[CL]))+
  geom_hline(yintercept=0,colour = "black", size = 1)+
  facet_grid(~Design,labeller=mf_labeller)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))

Figure_S6_B2 <- ggplot(etafalsedesign, aes(x=T_Weight, y=V1)) + geom_point(colour="blue")+
  labs(x="T_Weight",y=expression(eta[V[1]]))+
  geom_hline(yintercept=0,colour = "black", size = 1)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  facet_grid(~Design,labeller=mf_labeller)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))

p <- arrangeGrob(Figure_S6_B1,Figure_S6_B2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S6B.pdf",p,width=20,height=20,units="cm",dpi=300)



#### Figure S7

res2$Design <- "A"
res2_2$Design <- "B"
res2_1$Design <- "C"

res<- plyr::rbind.fill(res2,res2_2,res2_1)

mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="Design") { 
    value[value=="A"] <- paste("Standard design\n","\u03B5","-Shrinkage = 56%",sep="")
    value[value=="B"] <- paste("Sparse design\n","\u03B5","-Shrinkage = 72%",sep="")
    value[value=="C"] <- paste("Very sparse design\n","\u03B5","-Shrinkage = 80%",sep="")
  }
  return(value)
}
OBS <- c(obs$Conc,obs_2pt$Conc,obs_1pt$Conc)
res$Obs <- OBS

Figure_S7_A1 <- ggplot(res, aes(x=IPRED, y=Obs)) + geom_point(colour="blue")+
  labs(x="Individual predictions",y="Observations")+
  geom_abline(intercept=0,slope=1,color="black",size=1)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  facet_grid(~Design,labeller=mf_labeller)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))


Figure_S7_A2 <- ggplot(res, aes(x=Time, y=IWRES)) + geom_point(colour="blue")+
  labs(x="Time",y="IWRES")+
  geom_abline(intercept=0,slope=0,color="black",size=1)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  facet_grid(~Design,labeller=mf_labeller)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))


Figure_S7_B1 <- ggplot(res, aes(x=PPRED, y=Obs)) + geom_point(colour="blue")+
  labs(x="Population predictions",y="Observations")+
  geom_abline(intercept=0,slope=1,color="black",size=1)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  facet_grid(~Design,labeller=mf_labeller)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))


Figure_S7_B2 <- ggplot(res, aes(x=Time, y=PWRES)) + geom_point(colour="blue")+
  labs(x="Time",y="PWRES")+
  geom_abline(intercept=0,slope=0,color="black",size=1)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  facet_grid(~Design,labeller=mf_labeller)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))


p <- arrangeGrob(Figure_S7_A1,Figure_S7_A2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S7A.pdf",p,width=20,height=20,units="cm",dpi=300)


p <- arrangeGrob(Figure_S7_B1,Figure_S7_B2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S7B.pdf",p,width=20,height=20,units="cm",dpi=300)

#### Figure S8

pcvpc1 <- pcvpc(obs,sim1,bin,percentile,alpha,log.y=F)
pcobsx <- pcvpc1$obs
pcvpc_classic1 <- pcvpc1$theoper
pcvpc1 <- pcvpc1$PI


vpc1 <- vpc(obs,sim1,bin,percentile,alpha,log.y=F)
obsx <- vpc1$obs
vpc_classic1 <- vpc1$theoper
vpc1 <- vpc1$PI

Figure_S8A <- ggplot(data=obsx ,aes(x=Time,y=Conc))+ geom_point(aes(colour=as.factor(AMT)),shape=1)+
  geom_ribbon(data=vpc_classic1, aes(x=meantime,y=medianPI,ymin=lowerPI,ymax=upperPI,fill="red"),alpha=0.1,col=NA,show_guide=F) +
  geom_line(data=vpc_classic1, aes(x=meantime,y=medianPI),col="black",size=1,linetype="dashed") +
  geom_line(data=vpc_classic1, aes(x=meantime,y=lowerPI),col="black",linetype="dashed") +
  geom_line(data=vpc_classic1, aes(x=meantime,y=upperPI),col="black",linetype="dashed") +
  scale_colour_manual(values=c("#2c7fb8","#df65b0","#2ca25f"),guide=F)+
  ylab("Concentration")+ xlab("Time")+
  scale_y_continuous(breaks=seq(-2,3.5,0.5))+scale_x_continuous(breaks=seq(0,25,5))+
  scale_y_log10()+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

### with observations

Figure_S8B <- ggplot(vpc1 ,aes(x=meantime,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  geom_line(aes(y=theomedian,group=PI),linetype="dashed",color="black") +
  geom_point(data=obsx,aes(x=Time,y=Conc,col=as.factor(AMT)),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#df65b0","#2ca65f","#2c7fb8","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("10% percentile","50% percentile","90% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("Concentration")+ xlab("Time")+
  scale_y_continuous(breaks=seq(0,3.5,0.5))+scale_x_continuous(breaks=seq(0,25,5))+
  scale_y_log10()+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)



Figure_S8C <- ggplot(data=pcobsx ,aes(x=Time,y=pcConc))+ geom_point(aes(colour=as.factor(AMT)),shape=1)+
  geom_ribbon(data=pcvpc_classic1, aes(x=meantime,y=medianPI,ymin=lowerPI,ymax=upperPI,fill="red"),alpha=0.1,col=NA,show_guide=F) +
  geom_line(data=pcvpc_classic1, aes(x=meantime,y=medianPI),col="black",size=1,linetype="dashed") +
  geom_line(data=pcvpc_classic1, aes(x=meantime,y=lowerPI),col="black",linetype="dashed") +
  geom_line(data=pcvpc_classic1, aes(x=meantime,y=upperPI),col="black",linetype="dashed") +
  scale_colour_manual(values=c("#2c7fb8","#df65b0","#2ca25f"),guide=F)+
  ylab("Prediction corrected concentration")+ xlab("Time")+
  scale_x_continuous(breaks=seq(0,25,5))+
  scale_y_log10()+
  ggtitle("(C)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

### with observations

Figure_S8D <- ggplot(pcvpc1 ,aes(x=meantime,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  geom_line(aes(y=theomedian,group=PI),linetype="dashed",color="black") +
  geom_point(data=pcobsx,aes(x=Time,y=pcConc,col=as.factor(AMT)),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#df65b0","#2ca65f","#2c7fb8","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("10% percentile","50% percentile","90% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("Prediction corrected concentration")+ xlab("Time")+
  scale_x_continuous(breaks=seq(0,25,5))+
  scale_y_log10()+
  ggtitle("(D)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)


p <- arrangeGrob(Figure_S8A,Figure_S8B,Figure_S8C,Figure_S8D,ncol=2)
grid.draw(p)
ggsave(".\\Figure_S8.pdf",p,width=24,height=24,units="cm",dpi=300)

#### Figure S9

PI <- c(0,20,40,50,60,80,90,95)
npccal1 <- npc(obs,sim1,PI)
npccal2 <- npc(obs,sim2,PI)
npccal3 <- npc(obs,sim3,PI)
npccal4 <- npc(obs,sim4,PI)
npccal5 <- npc(obs,sim5,PI)
npccal6 <- npc(obs,sim6,PI)
npccal <- plyr::rbind.fill(npccal1,npccal2,npccal3,npccal4,npccal5,npccal6)
npccal$Model <- rep(neworder,each=length(PI)*2) 
npccal$Model <- as.factor(npccal$Model)
npccal <- arrange(transform(npccal,Model=factor(Model,levels=neworder)),Model)

npccal1_Sex0 <- npc(obs[obs$Sex==0,],sim1[sim1$Sex==0,],PI)
npccal1_Sex1 <- npc(obs[obs$Sex==1,],sim2[sim1$Sex==1,],PI)
npccal3_Sex0 <- npc(obs[obs$Sex==0,],sim3[sim3$Sex==0,],PI)
npccal3_Sex1 <- npc(obs[obs$Sex==1,],sim3[sim3$Sex==1,],PI)

npccalSex <- plyr::rbind.fill(npccal1_Sex0,npccal1_Sex1,npccal3_Sex0,npccal3_Sex1)
npccalSex$Model <- rep(c("True model\n Sex = 0","True model\n Sex = 1",
                         "Misspecified covariate model\n Sex = 0",
                         "Misspecified covariate model\n Sex = 1"),each=length(PI)*2) 
npccalSex$Model <- as.factor(npccalSex$Model)
npccalSex <- arrange(transform(npccalSex,Model=factor(Model,levels=c("True model\n Sex = 0",
                                                                     "True model\n Sex = 1",
                                                                     "Misspecified covariate model\n Sex = 0",
                                                                     "Misspecified covariate model\n Sex = 1"))),Model)
npccalSex$Sex <- rep(c(0,1),each=length(PI)*2)

Figure_S9_A1 <- ggplot(npccal[npccal$Model %in% c("True model", "Misspecified structural model"),], aes(PI,Ratio))+
  geom_point(aes(col=as.factor(Outliers),pch=as.factor(Outliers)),size=3)+geom_line()+
  geom_ribbon(aes(ymin=lwr,ymax=upr),fill="red",alpha=0.1)+
  geom_hline(yintercept=1,linetype="dashed")+
  facet_grid(Type~Model)+
  scale_colour_manual("Outside the CI",breaks=c("No","Yes"),values=c("black","red"))+
  scale_shape_manual("Outside the CI",breaks=c("No","Yes"),values=c(1,8))+
  ylab("Observed/Expected")+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)


Figure_S9_A2 <- ggplot(npccal[npccal$Model %in% c("Constant error model",
                                                  "Proportional error model"),],aes(PI,Ratio))+
  geom_point(aes(col=as.factor(Outliers),pch=as.factor(Outliers)),size=3)+geom_line()+
  geom_ribbon(aes(ymin=lwr,ymax=upr),fill="red",alpha=0.1)+
  geom_hline(yintercept=1,linetype="dashed")+
  facet_grid(Type~Model)+
  scale_colour_manual("Outside the CI",breaks=c("No","Yes"),values=c("black","red"))+
  scale_shape_manual("Outside the CI",breaks=c("No","Yes"),values=c(1,8))+
  ylab("Observed/Expected")+
  ggtitle("")+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)


Figure_S9_B1 <- ggplot(npccalSex[npccalSex$Sex==0,],aes(PI,Ratio))+
  geom_point(aes(col=as.factor(Outliers),pch=as.factor(Outliers)),size=3)+geom_line()+
  geom_ribbon(aes(ymin=lwr,ymax=upr),fill="red",alpha=0.1)+
  geom_hline(yintercept=1,linetype="dashed")+
  facet_grid(Type~Model)+
  scale_colour_manual("Outside the CI",breaks=c("No","Yes"),values=c("black","red"))+
  scale_shape_manual("Outside the CI",breaks=c("No","Yes"),values=c(1,8))+
  ylab("Observed/Expected")+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

Figure_S9_B2 <- ggplot(npccalSex[npccalSex$Sex==1,],aes(PI,Ratio))+
  geom_point(aes(col=as.factor(Outliers),pch=as.factor(Outliers)),size=3)+geom_line()+
  geom_ribbon(aes(ymin=lwr,ymax=upr),fill="red",alpha=0.1)+
  geom_hline(yintercept=1,linetype="dashed")+
  facet_grid(Type~Model)+
  scale_colour_manual("Outside the CI",breaks=c("No","Yes"),values=c("black","red"))+
  scale_shape_manual("Outside the CI",breaks=c("No","Yes"),values=c(1,8))+
  ylab("Observed/Expected")+
  ggtitle("")+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)


p <- arrangeGrob(Figure_S9_A1,Figure_S9_A2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S9A.pdf",p,width=18,height=32,units="cm",dpi=300)

p <- arrangeGrob(Figure_S9_B1,Figure_S9_B2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S9B.pdf",p,width=18,height=32,units="cm",dpi=300)


### Figure S10
source("./npde_2_modified/func_plots_npd.R")
ta1 <- npde.plot.meanprofile(npde1,ref.prof=list(ID=100),xscale=T)
tnpdex1 <- data.frame(Time=rep(ta1$xcalsim$bnds$xcent,3),CIlower=matrix(ta1$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(ta1$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(ta1$xcalsim$bnds$bmed,ncol=1),
                     median=matrix(ta1$xcalobs$percobs,ncol=1))
tnpdex1$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(tnpdex1)[1]/3)
tobsnpde1 <- ta1$xcalobs$plmat
tobsnpde1$AMT <- obs$AMT

obsnpde1$AMT <- obs$AMT

Figure_S10A <- ggplot(npdex1,aes(x=Time,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  geom_point(data=obsnpde1,aes(x=x,y=y,col=as.factor(AMT)),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#2c7fb8","#df65b0","#2ca65f","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("10% percentile","50% percentile","90% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("npd")+ xlab("Time")+
  scale_y_continuous(breaks=seq(-3.5,3.5,1))+scale_x_continuous(breaks=seq(0,25,5))+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

Figure_S10B <- ggplot(tnpdex1 ,aes(x=Time,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  geom_point(data=tobsnpde1,aes(x=x,y=ty,col=as.factor(AMT)),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#2c7fb8","#df65b0","#2ca65f","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("10% percentile","50% percentile","90% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("Transformed npd")+ xlab("Time")+
  scale_x_continuous(breaks=seq(0,25,5))+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

p <- arrangeGrob(Figure_S10A,Figure_S10B,ncol=2)
grid.draw(p)
ggsave(".\\Figure_S10.pdf",p,width=30,height=12,units="cm",dpi=300)



###########################
## PKPD warfarin example ##
###########################



setwd(".\\R")

suppressMessages ( require(grid))
suppressMessages ( require(reshape2))
suppressMessages ( require(dplyr))
suppressMessages ( require(ggplot2))
suppressMessages ( require(tidyr))
suppressMessages ( require(boot))
#suppressMessages ( require(npde))
suppressMessages ( require(gpairs))
require(gridExtra)

#hack for npde
# Defining generic functions
#source("./npde_2_modified/global.R")

# Classes
#source("./npde_2_modified/NpdeData.R")
#source("./npde_2_modified/NpdeRes.R")
#source("./npde_2_modified/NpdeObject.R")

# Main function
#source("./npde_2_modified/func_methods.R")
#source("./npde_2_modified/func_plots_npd.R")
#source("./npde_2_modified/main.R")



datafit <- read.csv("datafit.csv")

gof1 <- read.csv(".\\1\\Residuals.csv")
gof2 <- read.csv(".\\2\\Residuals.csv")
gof3 <- read.csv(".\\3\\Residuals.csv")
gof4 <- read.csv(".\\4\\Residuals.csv")

gof1$ModelName <- "d.True model\n Turnover PD"
gof2$ModelName <- "a.Misspecified delay\n Immediate effect"
gof3$ModelName <- "b.Misspecified delay\n Effect compartment"
gof4$ModelName <- "c.Misspecified delay and correlation\n Effect compartment, Full Omega"
gof<- plyr::rbind.fill(gof1,gof2,gof3,gof4)
gof$ModelName <- as.factor(gof$ModelName)


eta1 <- read.csv(".\\1\\Etacov.csv")
eta2 <- read.csv(".\\2\\Etacov.csv")
eta3 <- read.csv(".\\3\\Etacov.csv")
eta4 <- read.csv(".\\4\\Etacov.csv")

eta1$ModelName <- "d.True model\n Turnover PD"
eta2$ModelName <- "a.Misspecified delay\n Immediate effect"
eta3$ModelName <- "b.Misspecified delay\n Effect compartment"
eta4$ModelName <- "c.Misspecified delay and correlation\n Effect compartment, Full Omega"

eta<-  plyr::rbind.fill(eta1,eta2,eta3,eta4)
eta$ModelName <- as.factor(eta$ModelName)

omega1 <- read.csv(".\\1\\omega.csv")
omega2 <- read.csv(".\\2\\omega.csv")
omega3 <- read.csv(".\\3\\omega.csv")
omega4 <- read.csv(".\\4\\omega.csv")

originaldata<- read.csv("ka1_to_emax1_simlnnlme.csv")
covdata <- originaldata %>%
  filter(!duplicated(X.ID))%>%
  select(X.ID,AGE,WT,SEX)
names(covdata) <- c("ID","AGE","WT","SEX")
covdata$SEX <- ifelse(covdata$SEX==1,"M","F")
covdata$SEX <- factor(covdata$SEX)
eta1 <- left_join(eta1,covdata)
eta2 <- left_join(eta2,covdata)
eta3 <- left_join(eta3,covdata)
eta4 <- left_join(eta4,covdata)

PCDATA1 <- read.table(".\\1\\PredCheck_ObsQ_SimQCI.csv", header = TRUE, sep = ",", nrows = -1, skip = 0, na.strings = "N/A")
PCDATA1NOPRED <- read.table(".\\1\\PredCheck_ObsQ_SimQCI_nopred.csv", header = TRUE, sep = ",", nrows = -1, skip = 0, na.strings = "N/A")
PCDATA2 <- read.table(".\\2\\PredCheck_ObsQ_SimQCI.csv", header = TRUE, sep = ",", nrows = -1, skip = 0, na.strings = "N/A")
PCDATA2NOPRED <- read.table(".\\2\\PredCheck_ObsQ_SimQCI_nopred.csv", header = TRUE, sep = ",", nrows = -1, skip = 0, na.strings = "N/A")
PCDATA3 <- read.table(".\\3\\PredCheck_ObsQ_SimQCI.csv", header = TRUE, sep = ",", nrows = -1, skip = 0, na.strings = "N/A")
PCDATA3NOPRED <- read.table(".\\3\\PredCheck_ObsQ_SimQCI_nopred.csv", header = TRUE, sep = ",", nrows = -1, skip = 0, na.strings = "N/A")
PCDATA4 <- read.table(".\\4\\PredCheck_ObsQ_SimQCI.csv", header = TRUE, sep = ",", nrows = -1, skip = 0, na.strings = "N/A")
PCDATA4NOPRED <- read.table(".\\4\\PredCheck_ObsQ_SimQCI_nopred.csv", header = TRUE, sep = ",", nrows = -1, skip = 0, na.strings = "N/A")


PCDATA1$ModelName <- "d.True model\n Turnover PD"
PCDATA2$ModelName <- "a.Misspecified delay\n Immediate effect"
PCDATA3$ModelName <- "b.Misspecified delay\n Effect compartment"
PCDATA4$ModelName <- "c.Misspecified delay and correlation\n Effect compartment, Full Omega"

PCDATA1NOPRED$ModelName <- "d.True model\n Turnover PD"
PCDATA2NOPRED$ModelName <- "a.Misspecified delay\n Immediate effect"
PCDATA3NOPRED$ModelName <- "b.Misspecified delay\n Effect compartment"
PCDATA4NOPRED$ModelName <- "c.Misspecified delay and correlation\n Effect compartment, Full Omega"


VPCDATA<- rbind(PCDATA1,PCDATA2,PCDATA3,PCDATA4)
VPCDATANOPRED<- rbind(PCDATA1NOPRED,PCDATA2NOPRED,PCDATA3NOPRED,PCDATA4NOPRED)

VPCDATA$DVPLOT <- with(VPCDATA,ifelse( is.na(DV),DV0,DV))
VPCDATA<- VPCDATA[,  c("t","DVPLOT","QI","QE","ObsName","ModelName")]
PCDATAm <- melt(VPCDATA,id=c("t","QE","QI","ObsName","ModelName"))
PCDATAm <- dcast ( PCDATAm , ModelName+ ObsName   +t +QI~ QE +variable)
PCDATAm <- PCDATAm[PCDATAm$ObsName=="EObs",]
PCDATAm$observed <- "Observed"

VPCDATANOPRED$DVPLOT <- with(VPCDATANOPRED,ifelse( is.na(DV),DV0,DV))
VPCDATANOPRED<- VPCDATANOPRED[,  c("t","DVPLOT","QI","QE","ObsName","ModelName")]
PCDATAmNOPRED <- melt(VPCDATANOPRED,id=c("t","QE","QI","ObsName","ModelName"))
PCDATAmNOPRED <- dcast ( PCDATAmNOPRED , ModelName+ ObsName   +t +QI~ QE +variable)
PCDATAmNOPRED <- PCDATAmNOPRED[PCDATAmNOPRED$ObsName=="EObs",]
PCDATAmNOPRED$observed <- "Observed"

##########NPDE using raw simulations tables provided
# when pred correction is used replicate 0 has the pred data

originaldata<- read.csv("ka1_to_emax1_simlnnlme.csv")
originaldata <- originaldata %>%
  filter(DVID==2)%>%
  filter(!is.na(DV))%>%
  filter(MDV==0)%>%
  select(X.ID,TIME,DV)

names(originaldata) <- c("ID","TIME","DV")

ALLPCDATA1 <- read.table(".\\1\\AllPCData_csv.csv", header = TRUE, sep = ",", nrows = -1, skip = 0, na.strings = "N/A")
ALLPCDATA2 <- read.table(".\\2\\AllPCData_csv.csv", header = TRUE, sep = ",", nrows = -1, skip = 0, na.strings = "N/A")
ALLPCDATA3 <- read.table(".\\3\\AllPCData_csv.csv", header = TRUE, sep = ",", nrows = -1, skip = 0, na.strings = "N/A")
ALLPCDATA4 <- read.table(".\\4\\AllPCData_csv.csv", header = TRUE, sep = ",", nrows = -1, skip = 0, na.strings = "N/A")

ALLPCDATA1 <- ALLPCDATA1 %>%
  filter(OBSNAME=="EObs")%>%
  select(ID5,IVAR,DV,REPLICATE)

ALLPCDATA2 <- ALLPCDATA2 %>%
  filter(OBSNAME=="EObs")%>%
  select(ID5,IVAR,DV,REPLICATE)

ALLPCDATA3 <- ALLPCDATA3 %>%
  filter(OBSNAME=="EObs")%>%
  select(ID5,IVAR,DV,REPLICATE)

ALLPCDATA4 <- ALLPCDATA4 %>%
  filter(OBSNAME=="EObs")%>%
  select(ID5,IVAR,DV,REPLICATE)

names(ALLPCDATA1) <-  c( names(originaldata), "REPLICATE")
names(ALLPCDATA2) <-  c( names(originaldata), "REPLICATE")
names(ALLPCDATA3) <-  c( names(originaldata), "REPLICATE")
names(ALLPCDATA4) <-  c( names(originaldata), "REPLICATE")

ALLPCDATA1$REPLICATE<- c(  rep( -1,232) ,ALLPCDATA1$REPLICATE[233:length(ALLPCDATA1$REPLICATE)] )
ALLPCDATA2$REPLICATE<- c(  rep( -1,232) ,ALLPCDATA2$REPLICATE[233:length(ALLPCDATA2$REPLICATE)] )
ALLPCDATA3$REPLICATE<- c(  rep( -1,232) ,ALLPCDATA3$REPLICATE[233:length(ALLPCDATA3$REPLICATE)] )
ALLPCDATA4$REPLICATE<- c(  rep( -1,232) ,ALLPCDATA4$REPLICATE[233:length(ALLPCDATA4$REPLICATE)] )

ALLPCDATA1 <- ALLPCDATA1 %>%
  filter(REPLICATE>=0)
ALLPCDATA2 <- ALLPCDATA2 %>%
  filter(REPLICATE>=0)
ALLPCDATA3 <- ALLPCDATA3 %>%
  filter(REPLICATE>=0)
ALLPCDATA4 <- ALLPCDATA4 %>%
  filter(REPLICATE>=0)
npccal <- read.csv("npccal.csv")# this is generated using another script




# MAIN FIGURES

#Figure 6a: (DV vs PRED) Observed PCA versus Poplation Prediction PRED



DVvsPRED <- ggplot(gof[gof$ObsName!="CObs",] ,aes(PRED,DV))+
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=1,size=1)+
  facet_grid (~ModelName)+
  stat_smooth(method="gam",se=F,size=2,color="red",linetype="dashed")+
  ylab("Observed PCA")+ xlab("Population Prediction PCA")+
  scale_y_continuous(breaks=seq(0,120,20))+scale_x_continuous(breaks=seq(0,120,20))+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(A)")


#Figure 6b: (DV vs IPRED) Observed PCA versus Individual Predictions IPRED

DVvsIPRED <-ggplot(gof ,aes((IPRED),(DV) ) )+
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=1,size=1)+
  facet_grid (~ModelName)+
  stat_smooth(method="loess",se=F,size=2,color="red",linetype="dashed")+
  ylab("Observed PCA")+ xlab("Individual Prediction PCA")+
  scale_y_continuous(breaks=seq(0,120,20))+scale_x_continuous(breaks=seq(0,120,20))+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(B)")




#Figure 7AB: CWRES Residuals versus Time and PRED



gof<-  gof %>%
  filter(ObsName=="EObs")
gofcwres <- gather(gof[,c("ModelName","ID","IVAR","PRED","CWRES")], variable, value, -IVAR,-PRED, -ID, -ModelName)
CWRESvsPRED<- ggplot(gofcwres ,aes(PRED,value))+
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=0,size=1)+
  facet_grid (~ModelName)+
  stat_smooth(method="loess",se=F,size=2,color="red",linetype="dashed")+
  ylab("CWRES")+ xlab("Population Prediction PCA")+
  scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,120,20))+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(D)")


CWRESvsTIME <-ggplot(gofcwres ,aes(IVAR,value))+
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=0,size=1)+
  facet_grid (~ModelName)+
  stat_smooth(method="loess",se=F,size=2,color="red",linetype="dashed")+
  ylab("CWRES")+ xlab("Time (h)")+
  scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,144,24))+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)+
  ggtitle("(C)")


p <- arrangeGrob(DVvsPRED,DVvsIPRED,CWRESvsTIME,CWRESvsPRED,ncol=1)
grid.draw(p)
ggsave(".\\Figure_6.pdf",p,width=35,height=45,units="cm",dpi=300)



#Figure 7A: VPC and NPD

vpcplotmain<-
  ggplot(PCDATAmNOPRED ,aes(t,`50%_DVPLOT`,ymin=`2.5%_DVPLOT`,ymax=`97.5%_DVPLOT`,fill=QI,col=QI))+
  facet_grid(~ModelName)+
  geom_ribbon(alpha=0.1,col=NA ) +
  geom_line( ,size=1) +
  geom_line(aes(y=`--_DVPLOT`,linetype=observed),color="black") +
  scale_linetype_manual(  name="Observerd (dashed lines)",breaks=c("Observed"),values=c("dashed"),guide=F)+
  scale_fill_manual  (name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                      breaks=c("05%","50%", "95%"),values=c("blue","red","blue"),guide=F)+
  ylab("Simulated and Observed PCA Quantiles")+ xlab("Time (h)")+
  scale_y_continuous(breaks=seq(0,120,20))+scale_x_continuous(breaks=seq(0,144,24))+
  scale_colour_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                      breaks=c("05%","50%","95%"),values=c("#2c7fb8","#df65b0","#2c7fb8"),guide=F)+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)


#Figure VPC2: PRED corrected VPC


pcvpcplotmain<-
  ggplot(PCDATAm ,aes(t,`50%_DVPLOT`,ymin=`2.5%_DVPLOT`,ymax=`97.5%_DVPLOT`,fill=QI,col=QI))+
  facet_grid(~ModelName)+
  geom_ribbon(alpha=0.1,col=NA ) +
  geom_line(size=1) +
  geom_line(aes(y=`--_DVPLOT`,linetype=observed),color="black") +
  scale_linetype_manual(  name="Observerd (dashed lines)",breaks=c("Observed"),values=c("dashed"),guide=F)+
  scale_fill_manual  (name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                      breaks=c("05%","50%", "95%"),values=c("blue","red","blue"),guide=F)+
  ylab("Simulated and Observed PCA Quantiles")+ xlab("Time (h)")+
  scale_y_continuous(breaks=seq(0,120,20))+scale_x_continuous(breaks=seq(0,144,24))+
  scale_colour_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                      breaks=c("05%","50%","95%"),values=c("#2c7fb8","#df65b0","#2c7fb8"),guide=F)+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)



#Figure 8B: NPD


npde1 <- autonpde(originaldata,ALLPCDATA1,iid = 1,ix = 2, iy = 3)
a1 <- npde.plot.meanprofile(npde1)
npdex1 <- data.frame(Time=rep(a1$xcalsim$bnds$xcent,3),CIlower=matrix(a1$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(a1$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a1$xcalsim$bnds$bmed,ncol=1))
colnames(npdex1) <- c("Time","CIlower","CIupper","theomedian")
rownames(npdex1) <- NULL
npdex1 <- as.data.frame(npdex1)
npdex1$median <-  matrix(a1$xcalobs$percobs,ncol=1)
npdex1$PI <- rep(c("5% percentile","50% percentile","95% percentile"),each=dim(npdex1)[1]/3)
obsnpde1 <- a1$xcalobs$plmat

a1t <- npde.plot.meanprofile(npde1,xscale=TRUE)
npdex1t <- data.frame(Time=rep(a1t$xcalsim$bnds$xcent,3),CIlower=matrix(a1t$xcalsim$bnds$binf,ncol=1),
                      CIupper=matrix(a1t$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a1t$xcalsim$bnds$bmed,ncol=1))
colnames(npdex1t) <- c("Time","CIlower","CIupper","theomedian")
rownames(npdex1t) <- NULL
npdex1t <- as.data.frame(npdex1t)
npdex1t$median <-  matrix(a1t$xcalobs$percobs,ncol=1)
npdex1t$PI <- rep(c("5% percentile","50% percentile","95% percentile"),each=dim(npdex1t)[1]/3)
obsnpde1t <- a1t$xcalobs$plmat


npde2 <- autonpde(originaldata,ALLPCDATA2,iid = 1,ix = 2, iy = 3)
a2 <- npde.plot.meanprofile(npde2)
npdex2 <- data.frame(Time=rep(a2$xcalsim$bnds$xcent,3),CIlower=matrix(a2$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(a2$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a2$xcalsim$bnds$bmed,ncol=1))
colnames(npdex2) <- c("Time","CIlower","CIupper","theomedian")
rownames(npdex2) <- NULL
npdex2 <- as.data.frame(npdex2)
npdex2$median <-  matrix(a2$xcalobs$percobs,ncol=1)
npdex2$PI <- rep(c("5% percentile","50% percentile","95% percentile"),each=dim(npdex2)[1]/3)
obsnpde2 <- a2$xcalobs$plmat


a2t <- npde.plot.meanprofile(npde2,xscale=TRUE)
npdex2t <- data.frame(Time=rep(a2t$xcalsim$bnds$xcent,3),CIlower=matrix(a2t$xcalsim$bnds$binf,ncol=1),
                      CIupper=matrix(a2t$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a2t$xcalsim$bnds$bmed,ncol=1))
colnames(npdex2t) <- c("Time","CIlower","CIupper","theomedian")
rownames(npdex2t) <- NULL
npdex2t <- as.data.frame(npdex2t)
npdex2t$median <-  matrix(a2t$xcalobs$percobs,ncol=1)
npdex2t$PI <- rep(c("5% percentile","50% percentile","95% percentile"),each=dim(npdex2t)[1]/3)
obsnpde2t <- a2t$xcalobs$plmat


npde3 <- autonpde(originaldata,ALLPCDATA3,iid = 1,ix = 2, iy = 3)
a3 <- npde.plot.meanprofile(npde3)
npdex3 <- data.frame(Time=rep(a3$xcalsim$bnds$xcent,3),CIlower=matrix(a3$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(a3$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a3$xcalsim$bnds$bmed,ncol=1))
colnames(npdex3) <- c("Time","CIlower","CIupper","theomedian")
rownames(npdex3) <- NULL
npdex3 <- as.data.frame(npdex3)
npdex3$median <-  matrix(a3$xcalobs$percobs,ncol=1)
npdex3$PI <- rep(c("5% percentile","50% percentile","95% percentile"),each=dim(npdex3)[1]/3)
obsnpde3 <- a3$xcalobs$plmat


a3t <- npde.plot.meanprofile(npde3,xscale=TRUE)
npdex3t <- data.frame(Time=rep(a3t$xcalsim$bnds$xcent,3),CIlower=matrix(a3t$xcalsim$bnds$binf,ncol=1),
                      CIupper=matrix(a3t$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a3t$xcalsim$bnds$bmed,ncol=1))
colnames(npdex3t) <- c("Time","CIlower","CIupper","theomedian")
rownames(npdex3t) <- NULL
npdex3t <- as.data.frame(npdex3t)
npdex3t$median <-  matrix(a3t$xcalobs$percobs,ncol=1)
npdex3t$PI <- rep(c("5% percentile","50% percentile","95% percentile"),each=dim(npdex3t)[1]/3)
obsnpde3t <- a3t$xcalobs$plmat



npde4 <- autonpde(originaldata,ALLPCDATA4,iid = 1,ix = 2, iy = 3)
a4 <- npde.plot.meanprofile(npde4)
npdex4 <- data.frame(Time=rep(a4$xcalsim$bnds$xcent,3),CIlower=matrix(a4$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(a4$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a4$xcalsim$bnds$bmed,ncol=1))
colnames(npdex4) <- c("Time","CIlower","CIupper","theomedian")
rownames(npdex4) <- NULL
npdex4 <- as.data.frame(npdex4)
npdex4$median <-  matrix(a4$xcalobs$percobs,ncol=1)
npdex4$PI <- rep(c("5% percentile","50% percentile","95% percentile"),each=dim(npdex4)[1]/3)
obsnpde4 <- a4$xcalobs$plmat


a4t <- npde.plot.meanprofile(npde4,xscale=TRUE)
npdex4t <- data.frame(Time=rep(a4t$xcalsim$bnds$xcent,3),CIlower=matrix(a4t$xcalsim$bnds$binf,ncol=1),
                      CIupper=matrix(a4t$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(a4t$xcalsim$bnds$bmed,ncol=1))
colnames(npdex4t) <- c("Time","CIlower","CIupper","theomedian")
rownames(npdex4t) <- NULL
npdex4t <- as.data.frame(npdex4t)
npdex4t$median <-  matrix(a4t$xcalobs$percobs,ncol=1)
npdex4t$PI <- rep(c("5% percentile","50% percentile","95% percentile"),each=dim(npdex4t)[1]/3)
obsnpde4t <- a4t$xcalobs$plmat




npdex1$ModelName <- "d.True model\n Turnover PD"
npdex2$ModelName <- "a.Misspecified delay\n Immediate effect"
npdex3$ModelName <- "b.Misspecified delay\n Effect compartment"
npdex4$ModelName <- "c.Misspecified delay and correlation\n Effect compartment, Full Omega"
npdex1t$ModelName <- "d.True model\n Turnover PD"
npdex2t$ModelName <- "a.Misspecified delay\n Immediate effect"
npdex3t$ModelName <- "b.Misspecified delay\n Effect compartment"
npdex4t$ModelName <- "c.Misspecified delay and correlation\n Effect compartment, Full Omega"


npdex<- plyr::rbind.fill(npdex1,npdex2,npdex3,npdex4)
npdext<- plyr::rbind.fill(npdex1t,npdex2t,npdex3t,npdex4t)

obsnpde<- plyr::rbind.fill(obsnpde1,obsnpde2,obsnpde3,obsnpde4)
obsnpdet<- plyr::rbind.fill(obsnpde1t,obsnpde2t,obsnpde3t,obsnpde4t)


NPDETRUE<- ggplot(data.frame(npdex)
                  ,aes(x=Time,y=median))+
  facet_grid(~ModelName) +
  geom_line(aes(y=theomedian,group=PI,col=PI),size=1.5) +
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(group=PI),linetype="dashed",color="black") +
  geom_point(data=obsnpde,aes(x=x,y=ty),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#df65b0","#2c7fb8","blue","red","blue"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("5% percentile","50% percentile","95% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("npd")+ xlab("Time (h)")+
  scale_y_continuous(breaks=seq(-3.5,3.5,1))+scale_x_continuous(breaks=seq(0,144,24))+
  ggtitle("(C)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)


NPDETRUESCALE <- ggplot(data.frame(npdext)
                        ,aes(x=Time,y=median))+
  facet_grid(~ModelName)+
  geom_line(aes(y=theomedian,group=PI,col=PI),size=1.5) +
  
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(group=PI),linetype="dashed",color="black") +
  geom_point(data=obsnpde1t,aes(x=x,y=ty),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#df65b0","#2c7fb8","blue","red","blue"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("5% percentile","50% percentile","95% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("Transformed npd")+ xlab("Time (h)")+
  scale_y_continuous(breaks=seq(0,100,20))+scale_x_continuous(breaks=seq(0,144,24))+
  ggtitle("(D)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

NPDETRUE
NPDETRUESCALE
p <- arrangeGrob(vpcplotmain,pcvpcplotmain,NPDETRUE,NPDETRUESCALE,ncol=1)
grid.draw(p)
ggsave(".\\Figure_7.pdf",p,width=35,height=45,units="cm",dpi=300)



# Supplemental

#Figure S11: Spaghetti plot of PCS with Median Pred from the various models

gofmedianpred<- gof %>%
  filter(ObsName=="EObs")  %>%
  group_by (ModelName,IVAR,ObsName) %>%
  summarise( MEDIANPRED=  median(PRED))
FIGS11 <- ggplot(datafit[!is.na(datafit$PD),] ,aes(TIME,PD))+
  geom_line(aes(group=ID,color="e.Simulated"))+
  ylab("Observed PCA")+ xlab("Time (h)")+
  geom_line(data=gofmedianpred,aes(IVAR,MEDIANPRED,col=ModelName ),size=1.2 )+
  # facet_grid(~ObsName)+
  scale_x_continuous(breaks=seq(0,144,24))+
  scale_y_continuous(breaks=seq(0,120,20))+
  # guides(col = guide_legend(reverse = TRUE))+
  scale_colour_manual(values =  c("red","blue","green", "black","gray"),guide=F
                      # labels=c(levels(gofmedianpred$ModelName),"Simulated")
  ) +
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)


FIGS11
ggsave(".\\Figure_S11.pdf",FIGS11,width=15,height=15,units="cm",dpi=300)


#Figure S12: Simulation Based Residuals versus Time and PRED
#PCWRES is similar to WRES and CWRES, but computed with a different covariance matrix. The covariance matrix for PCWRES is computed from simulations using the time points in the data.


gof<-  gof %>%
  filter(ObsName=="EObs")

gofcwres <- gather(gof[,c("ModelName","ID","IVAR","PRED","PCWRES","CWRES","WRES")], variable, value, -IVAR,-PRED, -ID, -ModelName)
gofcwres <- gather(gof[,c("ModelName","ID","IVAR","PRED","PCWRES")], variable, value, -IVAR,-PRED, -ID, -ModelName)
PCWRESvsPRED <- ggplot(gofcwres ,aes(PRED,value))+
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=0,size=1)+
  facet_grid (~ModelName)+
  stat_smooth(method="loess",se=F,size=2,color="red",linetype="dashed")+
  ylab("PWRES")+ xlab("Population Prediction PCA")+
  scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,120,20))+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)


PCWRESvsTIME<- ggplot(gofcwres ,aes(IVAR,value))+
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=0,size=1)+
  facet_grid (~ModelName)+
  stat_smooth(method="loess",se=F,size=2,color="red",linetype="dashed")+
  ylab("PWRES")+ xlab("Time (h)")+
  scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,144,24))+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)



PCWRESvsTIME

p <- arrangeGrob(PCWRESvsTIME,PCWRESvsPRED,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S12.pdf",p,width=35,height=25,units="cm",dpi=300)







#Figure S13: Example of an Individual fit
gof<-  gof %>%
  filter(ObsName=="EObs")
gofID <- gof[gof$ID==19, ]
individualfitexample<- ggplot(gofID ,aes(IVAR,DV))+
  geom_point(color="blue",size=2)+
  geom_line(aes(y=IPRED),col="green")+
  facet_grid (~ModelName,scales="free_y")+
  scale_x_continuous(breaks=seq(0,144,24))+
  ylab("PCA")+xlab("Time (h)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

individualfitexample

ggsave(".\\Figure_S13.pdf",individualfitexample,width=35,height=12,units="cm",dpi=300)




#Figure S14: (IWRES vs PRED and vs TIME)

gof<-  gof %>%
  filter(ObsName=="EObs")
gofcwres <- gather(gof[,c("ModelName","ID","IVAR","IPRED","IWRES")], variable, value, -IVAR,-IPRED, -ID, -ModelName)

IWRESvsPRED <- ggplot(gofcwres ,aes(IPRED,value))+
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=0,size=1)+
  facet_grid (~ModelName)+
  stat_smooth(method="loess",se=F,size=2,color="red",linetype="dashed")+
  ylab("IWRES")+ xlab("Individual Prediction PCA")+
  scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,120,20))+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

IWRESvsTIME <- ggplot(gofcwres ,aes(IVAR,value))+
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=0,size=1)+
  facet_grid (~ModelName)+
  stat_smooth(method="loess",se=F,size=2,color="red",linetype="dashed")+
  ylab("IWRES")+ xlab("Time (h)")+
  scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,144,24))+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

p <- arrangeGrob(IWRESvsTIME,IWRESvsPRED,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S14.pdf",p,width=35,height=25,units="cm",dpi=300)



#FigureS15 Etas: Eta pairs plots
pdf("Figure_S15D.pdf" , width = 10, height = 7)
gpairs( eta1[,c("nV","nCl","nTabs","nTlag","nBaseline","nTeq","nEC50")],
        upper.pars = list(conditional = 'boxplot', scatter = 'loess'),
        lower.pars = list(scatter = 'stats',conditional = "barcode"),
        diag.pars = list(fontsize = 9, show.hist = TRUE, hist.color = "gray"),
        stat.pars =list(fontsize = 12, signif = 0.05, verbose = FALSE, use.color = TRUE, missing = 'missing', just = 'centre'),
        scatter.pars = list(pch = 20))
dev.off()


#Eta2
pdf("Figure_S15A.pdf" , width = 10, height = 7)
gpairs( eta2[,c("nV","nCl","nTabs","nTlag","nE0","nEmax","nEC50")],
        upper.pars = list(conditional = 'boxplot', scatter = 'loess'),
        lower.pars = list(scatter = 'stats',conditional = "barcode"),
        diag.pars = list(fontsize = 9, show.hist = TRUE, hist.color = "gray"),
        stat.pars =list(fontsize = 12, signif = 0.05, verbose = FALSE, use.color = TRUE, missing = 'missing', just = 'centre'),
        scatter.pars = list(pch = 20))
dev.off()


#FigureS15 Etas3: Eta pairs plots 3

pdf("Figure_S15B.pdf" , width = 10, height = 7)
gpairs( eta3[,c("nV","nCl","nTabs","nTlag","nE0","nTeq","nEC50")],
        upper.pars = list(conditional = 'boxplot', scatter = 'loess'),
        lower.pars = list(scatter = 'stats',conditional = "barcode"),
        diag.pars = list(fontsize = 9, show.hist = TRUE, hist.color = "gray"),
        stat.pars =list(fontsize = 12, signif = 0.05, verbose = FALSE, use.color = TRUE, missing = 'missing', just = 'centre'),
        scatter.pars = list(pch = 20))
dev.off()


#FigureS15 Etas4: Eta pairs plots 4


pdf("Figure_S15C.pdf" , width = 10, height = 7)
gpairs( eta4[,c("nV","nCl","nTabs","nTlag","nE0","nTeq","nEC50")],
        upper.pars = list(conditional = 'boxplot', scatter = 'loess'),
        lower.pars = list(scatter = 'stats',conditional = "barcode"),
        diag.pars = list(fontsize = 9, show.hist = TRUE, hist.color = "gray"),
        stat.pars =list(fontsize = 12, signif = 0.05, verbose = FALSE, use.color = TRUE, missing = 'missing', just = 'centre'),
        scatter.pars = list(pch = 20))
dev.off()



#FigureS16  Etas: Eta plots vs  continuous covariates



etaplot <- gather(eta1[,c("nV","nCl","nTabs","nTlag","WT","AGE")], EtaName, Eta, -AGE,-WT)
etaplot <- gather(etaplot, CovrName, Covr,-EtaName,-Eta)

#shrinkageinfo1 <- omega1 %>%
#  filter(Label=="Shrinkage") %>%
#  select(nV:nTlag)%>%
#  gather(CovrName,shrinkage)

#etaplot <- left_join( etaplot,shrinkageinfo1 )
#etaplot$CovrName<- paste(etaplot$CovrName,"Shrinkage",
#round(100*etaplot$shrinkage,0),"%"  )


etacontinuous<- ggplot(etaplot , aes(x =Eta , y =  Covr)) +
  geom_point(col="blue",size=2)+
  geom_smooth(,method="gam",se=F,size=1.5,n=500,col="red")+
  #geom_hline(yintercept=0)+
  facet_wrap(CovrName~EtaName,scales = "free",ncol=4)+
  ylab("Individual Random Effects")+
  xlab("")+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)
etacontinuous

ggsave(".\\Figure_S16A.pdf",etacontinuous,width=25,height=15,units="cm",dpi=300)
#FigureS15B Etas: Eta plots vs  categorical covariates


etaplot <- gather(eta1[,c("nV","nCl","nTabs","nTlag","SEX")], CovrName, Covr, -SEX)
etaplot <- gather(eta1[,c("nV","nCl","nTabs","nTlag","SEX")], CovrName, Covr, -SEX)

etaplot <- gather(etaplot, EtaName, Eta,-CovrName,-Covr)

etacat<- ggplot(etaplot  , aes(x =Eta, y =  Covr)) +
  geom_boxplot(col="black",alpha=0.2)+
  geom_jitter(,col="blue")+
  stat_summary(fun.y=median, geom="line", aes(group=1),col="red")  +
  facet_wrap(~CovrName,scales = "free",ncol=4)+
  geom_hline(yintercept=0)+
  ylab("Individual Random Effects")+
  xlab("")+theme_bw()+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

ggsave(".\\Figure_S16B.pdf",etacat,width=25,height=8,units="cm",dpi=300)
#FigureS17 Numerical NPC

NPCPLOT<- ggplot(npccal, aes(PI,Ratio))+
  geom_hline(yint=1,linetype="dashed")+
  geom_point(aes(col=Outliers,pch=Outliers),size=5)+geom_line()+
  geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.1,fill="red")+
  facet_grid( Type ~ ModelName)+
  scale_colour_manual("Outside the CI",breaks=c("No","Yes"),values=c("black","red"),guide=F)+
  scale_shape_manual("Outside the CI",breaks=c("No","Yes"),values=c(1,8),guide=F)+
  ylab("Observed/Expected")+
  theme(legend.position="bottom")+
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=0, vjust=1))+
  coord_cartesian(ylim=c(-0.5,3.5),xlim=c(-5,100))+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)

ggsave(".\\Figure_S17.pdf",NPCPLOT,width=35,height=20,units="cm",dpi=300)




#Figures not used  VPC1: VPC


ggplot(PCDATAmNOPRED ,aes(t,`50%_DVPLOT`,ymin=`2.5%_DVPLOT`,ymax=`97.5%_DVPLOT`,fill=QI,col=QI))+
  facet_grid(~ModelName)+
  geom_ribbon(alpha=0.1,col=NA ) +
  geom_line( ) +
  geom_line(aes(y=`--_DVPLOT`,linetype=observed),color="black") +
  scale_linetype_manual(  name="Observerd (dashed lines)",breaks=c("Observed"),values=c("dashed"))+
  scale_colour_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                      breaks=c("05%","50%","95%"),values=c("red","blue","red"))+
  scale_fill_manual  (name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                      breaks=c("05%","50%", "95%"),values=c("transparent","transparent","transparent"))+
  ylab("Simulated and Observed PCA Quantiles")+ xlab("Time (h)")+
  scale_y_continuous(breaks=seq(0,120,20))+scale_x_continuous(breaks=seq(0,144,24))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))



#Figure NPDE: NPDE

####################
##"to_emax1_est"
####################
npde1 <- autonpde(originaldata,ALLPCDATA1,iid = 1,ix = 2, iy = 3,units =list(x="h",y="PCA") )
####################
##"im_emax_est"
####################
npde2 <- autonpde(originaldata,ALLPCDATA2,iid = 1,ix = 2, iy = 3,units =list(x="h",y="PCA") )
####################
##"ce_emax_est"
####################
npde3 <- autonpde(originaldata,ALLPCDATA3,iid = 1,ix = 2, iy = 3,units =list(x="h",y="PCA") )
####################
##"ce_emax_est_BLK2"
####################
npde4 <- autonpde(originaldata,ALLPCDATA4,iid = 1,ix = 2, iy = 3,units =list(x="h",y="PCA") )

par(mfrow=c(2,2))
plot(npde1,new=F,plot.type=c( "pred.scatter"),main="to_emax1_est" ,cex.axis=1.6,xlim=c(20,99),cex.lab=1.6)
plot(npde2,new=F,plot.type=c( "pred.scatter"),main="im_emax_est" ,cex.axis=1.6,xlim=c(20,99),cex.lab=1.6)
plot(npde3,new=F,plot.type=c( "pred.scatter"),main="ce_emax_est" ,cex.axis=1.6,xlim=c(20,99),cex.lab=1.6)
plot(npde4,new=F,plot.type=c( "pred.scatter"),main="ce_emax_est_BLK2" ,cex.axis=1.6,xlim=c(20,99),cex.lab=1.6)


par(mfrow=c(2,2))
plot(npde1,new=F,plot.type=c( "x.scatter"),main="to_emax1_est" ,cex.axis=1.6,xlim=c(0,144),cex.lab=1.6)
plot(npde2,new=F,plot.type=c( "x.scatter"),main="im_emax_est" ,cex.axis=1.6,xlim=c(0,144),cex.lab=1.6)
plot(npde3,new=F,plot.type=c( "x.scatter"),main="ce_emax_est" ,cex.axis=1.6,xlim=c(0,144),cex.lab=1.6)
plot(npde4,new=F,plot.type=c( "x.scatter"),main="ce_emax_est_BLK2" ,cex.axis=1.6,xlim=c(0,144),cex.lab=1.6)


par(mfrow=c(2,2))
plot(npde1,new=F,plot.type=c( "vpc"),main="to_emax1_est" ,cex.axis=1.6,xlim=c(20,99),cex.lab=1.6)
plot(npde2,new=F,plot.type=c( "vpc"),main="im_emax_est" ,cex.axis=1.6,xlim=c(20,99),cex.lab=1.6)
plot(npde3,new=F,plot.type=c( "vpc"),main="ce_emax_est" ,cex.axis=1.6,xlim=c(20,99),cex.lab=1.6)
plot(npde4,new=F,plot.type=c( "vpc"),main="ce_emax_est_BLK2" ,cex.axis=1.6,xlim=c(20,99),cex.lab=1.6)




