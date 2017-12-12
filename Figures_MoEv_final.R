# Supplementary R Codes

# Codes to generate the graphs found in the tutorial (including main figures and supplementary figures)

# This script is divided into two parts:
  # + The first part contains R codes to generates figures of the PK examples 
  # + The second part contains R codes to generates figures of the PKPD examples

# Make sure that you have all required packages. 
# The order of loading packages is important as some will mask functions from others.
# All codes are compatible with the indicated versions of necessary R packages
# Further updates of the concerned packages may introduce breaks in R codes

require(reshape2) # version 1.4.1
require(dplyr)    # version 0.4.3
require(ggplot2)  # version 2.0.0
require(tidyr)    # version 0.4.1
#require(boot)
require(grid)     # version 3.1.2
require(gridExtra)# version 2.2.1
require(proto)    # version 0.3-10
require(plyr)     # version 1.8.3
require(scales)   # version 0.4.0


# PK examaples
setwd("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/")


# function for Lowess regression for trend lines 
source('./stat_plsmo_new.R')
# functions for Visual Predictive Check (VPC) and Prediction Corrected VPC graphs
source('./vpc_pcvpc.R')
# function for NPC graph
source('./npc.R')

setwd("./PK_figures/") 

# Datasets

obs <- read.table("pkdata_5pt_180.csv",sep=";",header=T)
obs_1pt <- read.table("pkdata_1pt_180.csv",sep=";",header=T)
obs_2pt <- read.table("pkdata_2pt_180.csv",sep=";",header=T)

# remove rows corresponding to dose
obs <- obs[-which(obs$AMT!="."),]
obs$AMT <- rep(rep(c(10,100,1000),each=5),each=60)
obs$Conc <- as.numeric(as.character(obs$Conc))
obs_1pt <- obs_1pt[-which(obs_1pt$AMT!="."),]
obs_1pt$AMT <- rep(rep(c(10,100,1000),each=1),each=60)
obs_1pt$Conc <- as.numeric(as.character(obs_1pt$Conc))
obs_2pt <- obs_2pt[-which(obs_2pt$AMT!="."),]
obs_2pt$AMT <- rep(rep(c(10,100,1000),each=2),each=60)
obs_2pt$Conc <- as.numeric(as.character(obs_2pt$Conc))


# Simulated datasets under different models
# 1: True model
# 2: Misspecified structural model
# 3: Lacking Covariate model
# 4: Lacking Correlation between EBE model
# 5: False residual error model (Proportional error)
# 6: False residual error model (Constant error)

sim1 <- read.table(".\\True\\sim.txt",header=T,sep=";")
sim2 <- read.table(".\\False_Struc\\sim.txt",header=T,sep=";")
sim3 <- read.table(".\\False_Cov\\sim.txt",header=T,sep=";")
sim4 <- read.table(".\\False_Corr\\sim.txt",header=T,sep=";")
sim5 <- read.table(".\\False_Cons\\sim.txt",header=T,sep=";")
sim6 <- read.table(".\\False_Prop\\sim.txt",header=T,sep=";")

# remove rows corresponding to dose

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


# Read residuals results of different models

res1 <- read.table(".\\True\\Residuals_all.txt",header=T)
res2 <- read.table(".\\False_Struc\\Residuals_all.txt",header=T)
res3 <- read.table(".\\False_Cov\\Residuals_all.txt",header=T)
res4 <- read.table(".\\False_Corr\\Residuals_all.txt",header=T)
res5 <- read.table(".\\False_Cons\\Residuals_all.txt",header=T)
res6 <- read.table(".\\False_Prop\\Residuals_all.txt",header=T)
res1_1 <- read.table(".\\True_1pt\\Residuals.txt",header=T)
res1_2 <- read.table(".\\True_2pt\\Residuals.txt",header=T)
res2_1 <- read.table(".\\False_Struc_1pt\\Residuals.txt",header=T)
res2_2 <- read.table(".\\False_Struc_2pt\\Residuals.txt",header=T)
res3_1 <- read.table(".\\False_Cov_1pt\\Residuals.txt",header=T)
res3_2 <- read.table(".\\False_Cov_2pt\\Residuals.txt",header=T)

# Combine residual results of different models in one dataframe
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
res$Genotype[res$Genotype==0] <- "Genotype A"
res$Genotype[res$Genotype==1] <- "Genotype B"


# Read EBE results of different models and designs

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

# Read individual parameters of different models and designs

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



#### Figure 1 - Observations versus Population and individual predictions for true and misspecified stuctural model #####

# Choose data from true and misspecified structural model
datafig1 <- res[res$Model %in% c("True model","Misspecified structural model",
                                 "Constant error model","Proportional error model" ),]

# Observations versus CPRED
Figure_1Aa <- ggplot(datafig1,aes(x=CPRED,y=Obs))+geom_point(colour="blue")+
  geom_abline(intercept=0,slope=1,colour = "black", size = 1)+
  labs(x="CPRED",y="OBS")+
  facet_wrap (~Model,ncol=4)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))

# Observations versus Population predictions for true and misspecified structural model 
Figure_1Ab <- ggplot(datafig1,aes(x=PPRED,y=Obs))+geom_point(colour="blue")+
  geom_abline(intercept=0,slope=1,colour = "black", size = 1)+
  labs(x="PPRED",y="OBS")+
  facet_wrap (~Model,ncol=4)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))

# Observations versus Population predictions for true and misspecified structural model 
Figure_1Ac <- ggplot(datafig1,aes(x=IPRED,y=Obs))+geom_point(colour="blue")+
  geom_abline(intercept=0,slope=1,colour = "black", size = 1)+
  labs(x="IPRED",y="OBS")+
  facet_wrap (~Model,ncol=4)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))

p1 <- arrangeGrob(Figure_1Aa,Figure_1Ab,Figure_1Ac,ncol=1)
grid.draw(p1)
ggsave(".\\Figure_1A.tiff",p1,width=10,height=10,dpi=300,compression="lzw")


##### Figure 2 #####
# Weighted residuals versus Time and Population Predictions for different models

# Weighted residuals versus Time for True and Misspecified structural model

Figure_1Ba <- ggplot(res[res$Model %in% c("True model","Misspecified structural model",
                                          "Constant error model","Proportional error model"),],aes(x=Time,y=CWRES))+
  geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x="Time",y="CWRES")+
  facet_wrap(~Model,ncol=4,scales="free")+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))

Figure_1Bb <- ggplot(res[res$Model %in% c("True model","Misspecified structural model",
                                       "Constant error model","Proportional error model"),],aes(x=Time,y=PWRES))+
  geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x="Time",y="PWRES")+
  facet_wrap(~Model,ncol=4,scales="free")+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))

Figure_1Bc <- ggplot(res[res$Model %in% c("True model","Misspecified structural model",
                                          "Constant error model","Proportional error model"),],aes(x=Time,y=IWRES))+
  geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x="Time",y="IWRES")+
  facet_wrap(~Model,ncol=4,scales="free")+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))

p2 <- arrangeGrob(Figure_1Ba,Figure_1Bb,Figure_1Bc,ncol=1)
grid.draw(p2)
ggsave(".\\Figure_1B.tiff",p2,width=10,height=10,dpi=300,compression="lzw")



# Weighted residuals versus Population predictions for True and Misspecified structural model
Figure_1Ca <- ggplot(res[res$Model %in% c("True model","Misspecified structural model",
                                          "Constant error model","Proportional error model"),],aes(x=CPRED,y=CWRES))+
  geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x="CPRED",y="CWRES")+
  facet_wrap (~Model,ncol=4,scales="free")+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("(C)")+
  theme(plot.title = element_text(size=16, face="bold"))
Figure_1Cb <- ggplot(res[res$Model %in% c("True model","Misspecified structural model",
                                          "Constant error model","Proportional error model"),],aes(x=PPRED,y=PWRES))+
  geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x="PPRED",y="PWRES")+
  facet_wrap (~Model,ncol=4,scales="free")+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))

Figure_1Cc <- ggplot(res[res$Model %in% c("True model","Misspecified structural model",
                                          "Constant error model","Proportional error model"),],aes(x=IPRED,y=IWRES))+
  geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x="IPRED",y="IWRES")+
  facet_wrap (~Model,ncol=4,scales="free")+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))


p3 <- arrangeGrob(Figure_1Ca,Figure_1Cb,Figure_1Cc,ncol=1)
grid.draw(p3)
ggsave(".\\Figure_1C.tiff",p3,width=10,height=10,dpi=300,compression="lzw")

# Weighted residuals versus Time for True and Misspecified covariate models
labels <- c("0" = "No co-administration\n with drug X", "1" = "Co-administration\n with drug X")
Figure_1Da <- ggplot(res[res$Model %in% c("Misspecified covariate model","True model"),],aes(x=Time,y=CWRES))+
  geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x="Time",y="CWRES")+
  facet_wrap(Model ~ Sex,ncol=4,labeller=labeller(Sex = labels))+
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("(D)")+
  theme(plot.title = element_text(size=16, face="bold"))

Figure_1Db <- ggplot(res[res$Model %in% c("Misspecified covariate model","True model"),],aes(x=Time,y=PWRES))+
  geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x="Time",y="PWRES")+
  facet_wrap(Model ~ Sex,ncol=4,labeller=labeller(Sex = labels))+
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))

Figure_1Dc <- ggplot(res[res$Model %in% c("Misspecified covariate model","True model"),],aes(x=Time,y=IWRES))+
  geom_point(colour="blue")+
  geom_abline(intercept=0,slope=0,colour = "black", size = 1)+
  labs(x="Time",y="IWRES")+
  facet_wrap(Model ~ Sex,ncol=4,labeller=labeller(Sex = labels))+
  coord_cartesian(ylim = c(-3.1, 3.1)) +
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))


p4 <- arrangeGrob(Figure_1Da,Figure_1Db,Figure_1Dc, ncol=1)
grid.draw(p4)
ggsave(".\\Figure_1D_test.tiff",p4,width=10,height=12,dpi=300,compression="lzw")

#### Figure 2#####

# Define timepoints that separate each bin
bin <- c(0.25,0.75,2.5,8,18,30)
# Define the 95% prediction interval
alpha <- 0.05
int.pred1 <- alpha/2
int.pred2 <- 1-alpha/2
# Define the percentiles to be computed
percentile <- c(0.1,0.5,0.9)

# Calculate pcVPC for different models

vpc1 <- pcvpc(obs,sim1,bin,percentile,alpha,log.y=F)
vpc2 <- pcvpc(obs,sim2,bin,percentile,alpha,log.y=F)
vpc3 <- pcvpc(obs,sim3,bin,percentile,alpha,log.y=F)
vpc4 <- pcvpc(obs,sim4,bin,percentile,alpha,log.y=F)
vpc5 <- pcvpc(obs,sim5,bin,percentile,alpha,log.y=F)
vpc6 <- pcvpc(obs,sim6,bin,percentile,alpha,log.y=F)

# Combine all pcVPC results
# Observations
vpcobs <- plyr::rbind.fill(vpc1$obs,vpc2$obs,vpc3$obs,vpc4$obs,vpc5$obs,vpc6$obs)

vpcobs$Model <- rep(neworder,each=dim(obs)[1]) 
vpcobs$Model <- as.factor(vpcobs$Model)
vpcobs <- arrange(transform(vpcobs,Model=factor(Model,levels=neworder)),Model)
# Prediction intervals
vpcpi <- plyr::rbind.fill(vpc1$PI,vpc2$PI,vpc3$PI,vpc4$PI,vpc5$PI,vpc6$PI)
vpcpi$Model <- rep(neworder,each=length(unique(obs$Time))*3) 
vpcpi$Model <- as.factor(vpcpi$Model)
vpcpi <- arrange(transform(vpcpi,Model=factor(Model,levels=neworder)),Model)

# Figure 4A: pcVPC for different models
Figure_2A <- ggplot(vpcpi[vpcpi$Model %in% c("True model","Misspecified structural model",
                                        "Constant error model",
                                        "Proportional error model"),] ,aes(x=meantime,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  geom_line(aes(y=theomedian,group=PI),linetype="dashed",color="black") +
  geom_point(data=vpcobs[vpcobs$Model %in% c("True model","Misspecified structural model",
                                               "Constant error model",
                                               "Proportional error model"),],aes(x=Time,y=pcConc,col=as.factor(AMT)),shape=1)+
  facet_wrap(~Model,ncol=4)+
  scale_colour_manual(values=c("#2c7fb8","#df65b0","#2ca65f","#2c7fb8","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 80% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("10% percentile","50% percentile","90% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("Prediction corrected \n observations")+ xlab("Time")+
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
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

# Compute pcVPC for True and Misspecified Covariate models by stratifying by Sex
vpc1_Sex0 <- pcvpc(obs[obs$Sex==0,],sim1[sim1$Sex==0,],bin,percentile,alpha,log.y=F)
vpc1_Sex1 <- pcvpc(obs[obs$Sex==1,],sim1[sim1$Sex==1,],bin,percentile,alpha,log.y=F)
vpc3_Sex0 <- pcvpc(obs[obs$Sex==0,],sim3[sim3$Sex==0,],bin,percentile,alpha,log.y=F)
vpc3_Sex1 <- pcvpc(obs[obs$Sex==1,],sim3[sim3$Sex==1,],bin,percentile,alpha,log.y=F)

# Combine pcVPC stratified by Sex
vpcobssex <- plyr::rbind.fill(vpc1_Sex0$obs,vpc1_Sex1$obs,vpc3_Sex0$obs,vpc3_Sex1$obs)
neworder1 <- c("True model","Misspecified covariate model")
vpcobssex$Model <- rep(neworder1,each=dim(obs)[1]) 
vpcobssex$Model <- as.factor(vpcobssex$Model)
vpcobssex <- arrange(transform(vpcobssex,Model=factor(Model,levels=neworder)),Model)



vpcpisex <- plyr::rbind.fill(vpc1_Sex0$PI,vpc1_Sex1$PI,vpc3_Sex0$PI,vpc3_Sex1$PI)
vpcpisex$Model <- rep(neworder1,each=length(unique(obs$Time))*3*2) 
vpcpisex$Model <- as.factor(vpcpisex$Model)
vpcpisex <- arrange(transform(vpcpisex,Model=factor(Model,levels=neworder)),Model)
vpcpisex$Sex <- rep(c("0","1","0","1"),each=length(unique(obs$Time))*3) 


Figure_2B <- ggplot(vpcpisex ,aes(x=meantime,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  geom_line(aes(y=theomedian,group=PI),linetype="dashed",color="black") +
  geom_point(data=vpcobssex,aes(x=Time,y=pcConc,col=as.factor(AMT)),shape=1)+
  facet_wrap(Model~Sex,ncol=4,labeller=labeller(Sex = labels))+
  scale_colour_manual(values=c("#2c7fb8","#df65b0","#2ca65f","#2c7fb8","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 80% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("10% percentile","50% percentile","90% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("Prediction corrected\n observations")+ xlab("Time")+
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
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

p <- arrangeGrob(Figure_2A,Figure_2B,ncol=1)
grid.draw(p)
ggsave(".\\Figure_2.tiff",p,width=10,height=7.75,dpi=300,compression="lzw")



#### Figure 3 ####
# The document npde_2_modified contains R codes that have been modified from the original codes of 
# the npde 2.0 package written by Emmanuelle Comets (released in 2012) in order to extract the npde results
# to produce graphs with ggplot

source("./npde_2_modified/global.R")

# Classes
source("./npde_2_modified/NpdeData.R")
source("./npde_2_modified/NpdeRes.R")
source("./npde_2_modified/NpdeObject.R")

# Main function
source("./npde_2_modified/func_methods.R")
source("./npde_2_modified/func_plots_npd.R") # modified functions to extract npd results - Original functions return only npde
source("./npde_2_modified/main.R")

## Compute npd for different models ##
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

obsnpde1_Sex0$Sex <- obsnpde3_Sex0$Sex <- npdex1_Sex0$Sex <-npdex3_Sex0$Sex <-"0"
obsnpde1_Sex1$Sex <- obsnpde3_Sex1$Sex <- npdex1_Sex1$Sex <-npdex3_Sex1$Sex <-"1"
 

obsnpde1_Sex0$AMT <- obsnpde3_Sex0$AMT  <- obs_Sex0$AMT
obsnpde1_Sex1$AMT <- obsnpde3_Sex1$AMT  <- obs_Sex1$AMT

npdex_Sex <- plyr::rbind.fill(npdex1_Sex0,npdex1_Sex1,npdex3_Sex0,npdex3_Sex1)
obsnpde_Sex<- plyr::rbind.fill(obsnpde1_Sex0,obsnpde1_Sex1,obsnpde3_Sex0,obsnpde3_Sex1)

npdex_Sex <- arrange(transform(npdex_Sex,Model=factor(Model,levels=c("True model","Misspecified covariate model"))),Model)
obsnpde_Sex <- arrange(transform(obsnpde_Sex,Model=factor(Model,levels=c("True model","Misspecified covariate model"))),Model)

# Source the modified functions to extract npde (not npd) results 
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

### npde vs Time for True model and Misspecified Covariate model (stratifying by covariate)

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

robsnpde1_Sex0$Sex <- robsnpde3_Sex0$Sex <- rnpdex1_Sex0$Sex <-rnpdex3_Sex0$Sex <-"0"
robsnpde1_Sex1$Sex <- robsnpde3_Sex1$Sex <- rnpdex1_Sex1$Sex <-rnpdex3_Sex1$Sex <-"1"


robsnpde1_Sex0$AMT <- robsnpde3_Sex0$AMT  <- obs_Sex0$AMT
robsnpde1_Sex1$AMT <- robsnpde3_Sex1$AMT  <- obs_Sex1$AMT

rnpdex_Sex <- plyr::rbind.fill(rnpdex1_Sex0,rnpdex1_Sex1,rnpdex3_Sex0,rnpdex3_Sex1)
robsnpde_Sex<- plyr::rbind.fill(robsnpde1_Sex0,robsnpde1_Sex1,robsnpde3_Sex0,robsnpde3_Sex1)

rnpdex_Sex <- arrange(transform(rnpdex_Sex,Model=factor(Model,levels=c("True model","Misspecified covariate model"))),Model)
robsnpde_Sex <- arrange(transform(robsnpde_Sex,Model=factor(Model,levels=c("True model","Misspecified covariate model"))),Model)


# Figure 3A: npd versus Time / Population predictions for True and Misspecified structural model

Figure_3A <- ggplot(npdex[npdex$Model %in% c("True model","Misspecified structural model",
                                              "Constant error model","Proportional error model"),] ,aes(x=Time,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  facet_wrap(~Model,ncol=4)+
  geom_point(data=obsnpde[obsnpde$Model %in% c("True model","Misspecified structural model",
                                               "Constant error model","Proportional error model"),],aes(x=x,y=y,col=as.factor(AMT)),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#2c7fb8","#df65b0","#2ca65f","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("10% percentile","50% percentile","90% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("npd")+ xlab("Time")+
  scale_y_continuous(breaks=seq(-3.5,3.5,1))+scale_x_continuous(breaks=seq(0,25,5))+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

Figure_3B <- ggplot(npdepredx[npdepredx$Model %in% c("True model","Misspecified structural model",
                                                     "Constant error model","Proportional error model"),] ,aes(x=Time,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  facet_wrap(~Model,ncol=4)+
  geom_point(data=obsnpdepred[obsnpdepred$Model %in% c("True model","Misspecified structural model",
                                                       "Constant error model","Proportional error model"),],aes(x=x,y=y,col=as.factor(AMT)),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#2c7fb8","#df65b0","#2ca65f","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("5% percentile","50% percentile","95% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("npd")+ xlab("Population predictions")+
  scale_y_continuous(breaks=seq(-3.5,3.5,1))+ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("(B)")

# Figure 5C: npd versus Time / Population predictions for True and Misspecified Covariate model, stratifying by Sex
Figure_3C <- ggplot(npdex_Sex ,aes(x=Time,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  facet_wrap(Model~Sex,ncol=4,labeller=labeller(Sex = labels))+
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
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

Figure_3D <- ggplot(rnpdex[rnpdex$Model %in% c("True model","Misspecified structural model",
                                             "Constant error model","Proportional error model"),] ,aes(x=Time,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  facet_wrap(~Model,ncol=4)+
  geom_point(data=robsnpde[robsnpde$Model %in% c("True model","Misspecified structural model",
                                               "Constant error model","Proportional error model"),],aes(x=x,y=y,col=as.factor(AMT)),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#2c7fb8","#df65b0","#2ca65f","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("10% percentile","50% percentile","90% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("npde")+ xlab("Time")+
  scale_y_continuous(breaks=seq(-3.5,3.5,1))+scale_x_continuous(breaks=seq(0,25,5))+
  ggtitle("(D)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)


# Figure 5D: npde versus Time / Population predictions for different models
Figure_3E <- ggplot(rnpdex_Sex ,aes(x=Time,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  facet_wrap(Model~Sex,ncol=4,labeller=labeller(Sex = labels))+
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
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)


p <- arrangeGrob(Figure_3A,Figure_3B,Figure_3C,Figure_3D,Figure_3E,ncol=1)
grid.draw(p)
ggsave(".\\Figure_3.tiff",p,width=10,height=19.5,dpi=300,compression="lzw")



#### Supplementary figures
# Figure S1
# Read data simulated from different model to obtain the median profiles
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

# Median predicted profiles for different models for the dose of 10 mg
Dose1 <- c(apply(matrix(simtrue$Conc[simtrue$Dose==10],ncol=6000),1,median),
           apply(matrix(simfstruc$Conc[simfstruc$Dose==10],ncol=6000),1,median),
           apply(matrix(simfcov$Conc[simfcov$Dose==10],ncol=6000),1,median),
           apply(matrix(simfcorr$Conc[simfcorr$Dose==10],ncol=6000),1,median),
           apply(matrix(simfconst$Conc[simfconst$Dose==10],ncol=6000),1,median),
           apply(matrix(simfprop$Conc[simfprop$Dose==10],ncol=6000),1,median))
# Median predicted profiles for different models for the dose of 100 mg
Dose2 <- c(apply(matrix(simtrue$Conc[simtrue$Dose==100],ncol=6000),1,median),
           apply(matrix(simfstruc$Conc[simfstruc$Dose==100],ncol=6000),1,median),
           apply(matrix(simfcov$Conc[simfcov$Dose==100],ncol=6000),1,median),
           apply(matrix(simfcorr$Conc[simfcorr$Dose==100],ncol=6000),1,median),
           apply(matrix(simfconst$Conc[simfconst$Dose==100],ncol=6000),1,median),
           apply(matrix(simfprop$Conc[simfprop$Dose==100],ncol=6000),1,median))
# Median predicted profiles for different models for the dose of 1000 mg
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

# Combine the median predicted profiles for 3 doses
sim3dose <- plyr::rbind.fill(Dose1,Dose2,Dose3)
sim3dose <- arrange(transform(sim3dose,Model=factor(Model,levels=neworder)),Model)
obs$AMT1 <- "Dose = 10 mg"
obs$AMT1[obs$AMT==100] <- "Dose = 100 mg"
obs$AMT1[obs$AMT==1000] <- "Dose = 1000 mg"

# Figure S1: Spaghettiplot of data and the median predicted profiles of different models
Figure_S1 <- ggplot(obs,aes(x=Time,y=Conc,group=ID))+geom_line(colour="gray60")+scale_y_log10()+
  geom_line(data=sim3dose,aes(x=Time,y=Value,group=Model,colour=Model),lwd=1,show.legend=F)+
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
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))

ggsave(".\\Figure_S1.tiff",Figure_S1,width=10,height=3.5,dpi=300,compression="lzw")

### Figure_S3

# ODE systems for two-compartment PK model
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

# ODE systems for one-compartment PK model (false structural model)
pk1comp <- function(t,y,p) {
  k <- p[2]/p[1]
  V <- p[1]
  dy1 <- -k*y[1]
  Cc <- y[1]/V 
  list(c(dy1),c(Cc))}

t <- seq(0,25,0.1)
id <- 61
condinit<- c(y1=100,y2=0)

# Simulate individual profiles for individual #id = 3
ipred1 <-lsoda(func=pk2comp,times=t,y=condinit,parms=indivpar1[indivpar1$ID==id,2:5],hmax=0.1)[,c(1,4)]
ipred2 <-lsoda(func=pk1comp,times=t,y=c(y1=100),parms=indivpar2[indivpar2$ID==id,2:3],hmax=0.1)[,c(1,3)]
ipred3 <-lsoda(func=pk2comp,times=t,y=condinit,parms=indivpar3[indivpar3$ID==id,2:5],hmax=0.1)[,c(1,4)]
ipred4 <-lsoda(func=pk2comp,times=t,y=condinit,parms=indivpar4[indivpar4$ID==id,2:5],hmax=0.1)[,c(1,4)]
ipred5 <-lsoda(func=pk2comp,times=t,y=condinit,parms=indivpar5[indivpar5$ID==id,2:5],hmax=0.1)[,c(1,4)]
ipred6 <-lsoda(func=pk2comp,times=t,y=condinit,parms=indivpar6[indivpar6$ID==id,2:5],hmax=0.1)[,c(1,4)]

colnames(ipred2) <- colnames(ipred1)

# Combine all individual profiles obtained in individual #3 under different models
ipred <- data.frame(rbind(ipred1,ipred2,ipred3,ipred4,ipred5,ipred6))
ipred$Model <- rep(c("True model","Misspecified structural model","Misspecified covariate model",
                     "Misspecified correlation model","Constant error model",
                     "Proportional error model"),each=length(t))
ipred$Model <- as.factor(ipred$Model)
ipred <- arrange(transform(ipred,Model=factor(Model,levels=neworder)),Model)

# Select observed data for individual #61
iobs <- obs[obs$ID==id,]

# Simulate PRED using covariates' values of individual 61 and fixed effect parameters
pipar1 <- c(0.0977*exp(0.519*indivpar1[indivpar1$ID==id,6]),0.518*exp(0.787*indivpar1[indivpar1$ID==id,7]),0.113,1.01)
pipar2 <- c(0.862*exp(0.259*indivpar1[indivpar1$ID==id,7]),0.1*exp(0.277*indivpar1[indivpar1$ID==id,6]))
pipar3 <- c(0.127,0.508,0.113,0.985)
pipar4 <- c(0.0963*exp(0.533*indivpar1[indivpar1$ID==id,6]),0.519*exp(0.798*indivpar1[indivpar1$ID==id,7]),0.112,1.02)
pipar5 <- c(0.111*exp(0.483*indivpar1[indivpar1$ID==id,6]),0.539*exp(0.755*indivpar1[indivpar1$ID==id,7]),0.103,0.824)
pipar6 <- c(0.098*exp(0.462*indivpar1[indivpar1$ID==id,6]),0.542*exp(0.751*indivpar1[indivpar1$ID==id,7]),0.121,1.06)

pipred1 <-lsoda(func=pk2comp,times=t,y=condinit,parms=pipar1,hmax=0.1)[,c(1,4)]
pipred2 <-lsoda(func=pk1comp,times=t,y=c(y1=100),parms=pipar2,hmax=0.1)[,c(1,3)]
pipred3 <-lsoda(func=pk2comp,times=t,y=condinit,parms=pipar3,hmax=0.1)[,c(1,4)]
pipred4 <-lsoda(func=pk2comp,times=t,y=condinit,parms=pipar4,hmax=0.1)[,c(1,4)]
pipred5 <-lsoda(func=pk2comp,times=t,y=condinit,parms=pipar5,hmax=0.1)[,c(1,4)]
pipred6 <-lsoda(func=pk2comp,times=t,y=condinit,parms=pipar6,hmax=0.1)[,c(1,4)]

colnames(pipred2) <- colnames(pipred1)

# Combine all individual profiles obtained in individual #3 under different models
pipred <- data.frame(rbind(pipred1,pipred2,pipred3,pipred4,pipred5,pipred6))
pipred$Model <- rep(c("True model","Misspecified structural model","Misspecified covariate model",
                     "Misspecified correlation model","Constant error model",
                     "Proportional error model"),each=length(t))
pipred$Model <- as.factor(pipred$Model)
pipred <- arrange(transform(pipred,Model=factor(Model,levels=neworder)),Model)

# Figure S3: Individual fits for individual #3 of different models
Figure_S3 <- ggplot(ipred[ipred$Model %in% c("True model","Misspecified structural model","Constant error model",
                                        "Proportional error model"),],aes(x=time,y=V1))+
  geom_line(colour="green")+
  geom_line(data=pipred[pipred$Model %in% c("True model","Misspecified structural model","Constant error model",
                                          "Proportional error model"),],aes(x=time,y=y1),col="red")+
  labs(x="Time",y="Concentration")+
  geom_point(data=iobs,aes(x=Time,y=Conc),color="blue")+
  facet_wrap (~Model,ncol=4)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

ggsave(".\\Figure_S3.tiff",Figure_S3,width=10,height=3.5,dpi=300,compression="lzw")


#### Figure_S4

# Variance - covariance matrix of random effects of the true model
corr_true <- diag(c(0.448,0.482,0.473,0.413)^2) 
corr_true[1,2] <- corr_true[2,1] <- 0.438*0.448*0.482
# Variance - covariance matrix of random effects of the Misspecified correlation (between random effects) model
corr_wrong <- diag(c(0.45,0.478,0.473,0.403)^2)

### Decorrelate random effects for true model
var.eig <- eigen(corr_true)
xmat <- var.eig$vectors %*% diag(sqrt(var.eig$values)) %*% solve(var.eig$vectors)
ymat<-try(solve(xmat))
deceta1<- as.data.frame(as.matrix(eta1[,2:5]) %*% ymat)

### Decorrelate random effects for Misspecified correlation (between random effects) model 
var.eig <- eigen(corr_wrong)
xmat <- var.eig$vectors %*% diag(sqrt(var.eig$values)) %*% solve(var.eig$vectors)
ymat<-try(solve(xmat))
deceta4<- as.data.frame(as.matrix(eta4[,2:5]) %*% ymat)

colnames(deceta1) <- colnames(deceta4) <-c("CL","V1","Q","V2")
neworder1 <- c("True model","Misspecified correlation model")

# Combine EBE for True and Misspecified correlation model
eta14<- plyr::rbind.fill(eta1,eta4)
eta14$Model <- rep(c("True model","Misspecified correlation model"),each=dim(eta1)[1])
eta14$Model <- as.factor(eta14$Model)
eta14 <- arrange(transform(eta14,Model=factor(Model,levels=neworder1)),Model)
slope <- data.frame(cor=c(0.438,0),Model=c("True model","Misspecified correlation model"))
eta14 <- merge(eta14,slope,by="Model")
eta14$Model <- as.factor(eta14$Model)
eta14 <- arrange(transform(eta14,Model=factor(Model,levels=neworder)),Model)

# Combine decorrelated EBE for True and Misspecified correlation model
deceta <- plyr::rbind.fill(deceta1,deceta4)
deceta$Model <- rep(c("True model","Misspecified correlation model"),each=dim(eta1)[1])
deceta$Model <- as.factor(deceta$Model)
deceta <- arrange(transform(deceta,Model=factor(Model,levels=neworder)),Model)

# Figure S4: EBE and decorrelated EBE of Clearance versus those of Volume of the Central compartment
Figure_S4_1 <- ggplot(eta14,aes(x=CL,y=V1))+geom_point(colour="blue")+
  geom_abline(aes(intercept=0,slope=cor),colour = "black", size = 1)+
  labs(x=expression(eta[CL]),y=expression(eta[V[1]]))+
  facet_wrap (~Model,ncol=2)+
  stat_smooth(method="lm",se=F,size=2,color="red",linetype="dashed")+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
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
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))

p <- arrangeGrob(Figure_S4_1,Figure_S4_2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S4.tiff",p,width=6,height=7,dpi=300,compression="lzw")

#### Figure_S5

eta13<- plyr::rbind.fill(eta1,eta3)
eta13$Model <- rep(c("True model","Misspecified covariate model"),each=dim(eta1)[1])
eta13$Model <- as.factor(eta13$Model)
eta13 <- arrange(transform(eta13,Model=factor(Model,levels=c("True model","Misspecified covariate model"))),Model)

indivpar13<- plyr::rbind.fill(indivpar1,indivpar3)
indivpar13$Model <- rep(c("True model","Misspecified covariate model"),each=dim(eta1)[1])
indivpar13$Model <- as.factor(indivpar13$Model)
indivpar13 <- arrange(transform(indivpar13,Model=factor(Model,levels=c("True model","Misspecified covariate model"))),Model)
indivpar13[,2:5] <- log(indivpar13[,2:5])

Figure_S5_A1 <- ggplot(indivpar13, aes(x=as.factor(Sex), y=CL)) + geom_boxplot(color="blue")+
  labs(x="Concomitant treatment",y="log(CL)")+
  scale_x_discrete(labels=c("0"="No","1"="Yes"))+
  facet_wrap (~Model,ncol=2)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))


Figure_S5_A2 <- ggplot(indivpar13, aes(x=T_Weight, y=V1)) + geom_point(colour="blue")+
  labs(x="T_Weight",y=expression("log("~V["1"]~")"))+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  facet_wrap (~Model,ncol=2)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))


Figure_S5_B1 <- ggplot(eta13, aes(x=as.factor(Sex), y=CL)) + geom_boxplot(color="blue")+
  labs(x="Concomitant treatment",y=expression(eta[CL]))+
  scale_x_discrete(labels=c("0"="No","1"="Yes"))+
  geom_hline(yintercept=0,colour = "black", size = 1)+
  facet_wrap (~Model,ncol=2)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))

Figure_S5_B2 <- ggplot(eta13, aes(x=T_Weight, y=V1)) + geom_point(colour="blue")+
  labs(x="T_Weight",y=expression(eta[V["1"]]))+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  facet_wrap (~Model,ncol=2)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))

p <- arrangeGrob(Figure_S5_A1,Figure_S5_A2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S5A.tiff",p,width=6,height=7,dpi=300,compression="lzw")
p <- arrangeGrob(Figure_S5_B1,Figure_S5_B2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S5B.tiff",p,width=6,height=7,dpi=300,compression="lzw")



#### Figure S6

# Figure S6A: Correlation betwen EBE_CL and EBE_V1 for the true models and different designs
etatruedesign<- plyr::rbind.fill(eta1,eta1_2,eta1_1)
etatruedesign$Design <- rep(c("A","B","C"),each=dim(eta1)[1])

mf_labeller <- as_labeller(c("A"=paste("Standard design\n","\u03B7","-Shrinkage (CL) = 9%\n","\u03B7","-Shrinkage (V1) = 11%",sep=""),
                             "B"=paste("Sparse design\n","\u03B7","-Shrinkage (CL) = 26%\n","\u03B7","-Shrinkage (V1) = 39%",sep=""),
                             "C"=paste("Very sparse design\n","\u03B7","-Shrinkage (CL) = 64%\n","\u03B7","-Shrinkage (V1) = 61%",sep="")))

Figure_S6A <- ggplot(etatruedesign, aes(x=CL, y=V1)) + geom_point(colour="blue")+
  labs(x=expression(eta[CL]),y=expression(eta[V[1]]))+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  facet_grid(~Design,labeller=mf_labeller)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))
ggsave(".\\Figure_S6A.tiff",Figure_S6A,width=10,height=5,dpi=300,compression="lzw")


# Figure S6B: EBE_CL versus Covariates for the Misspecified Covariate models and different designs
etafalsedesign<- plyr::rbind.fill(eta3,eta3_2,eta3_1)
etafalsedesign$Design <- rep(c("A","B","C"),each=dim(eta3)[1])

mf_labeller <- as_labeller(c("A"=paste("Standard design\n","\u03B7","-Shrinkage (CL) = 8%\n","\u03B7","-Shrinkage (V1) = 11%",sep=""),
                             "B"=paste("Sparse design\n","\u03B7","-Shrinkage (CL) = 23%\n","\u03B7","-Shrinkage (V1) = 35%",sep=""),
                             "C"=paste("Very sparse design\n","\u03B7","-Shrinkage (CL) = 60%\n","\u03B7","-Shrinkage (V1) = 69%",sep="")))


Figure_S6_B1 <- ggplot(etafalsedesign, aes(x=as.factor(Sex), y=CL)) + 
  geom_boxplot(colour="blue",show.legend = F)+
  labs(x="Concomitant treatment",y=expression(eta[CL]))+
  scale_x_discrete(labels=c("0"="No","1"="Yes"))+
  geom_hline(yintercept=0,colour = "black", size = 1)+
  facet_grid(~Design,labeller=mf_labeller)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
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
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))

p <- arrangeGrob(Figure_S6_B1,Figure_S6_B2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S6B.tiff",p,width=10,height=10,dpi=300,compression="lzw")



#### Figure S7

res2$Design <- "A"
res2_2$Design <- "B"
res2_1$Design <- "C"

res<- plyr::rbind.fill(res2,res2_2,res2_1)
mf_labeller <- as_labeller(c("A"=paste("Standard design\n","\u03B5","-Shrinkage = 56%",sep=""),
                             "B"=paste("Sparse design\n","\u03B5","-Shrinkage = 72%",sep=""),
                             "C"=paste("Very sparse design\n","\u03B5","-Shrinkage = 80%",sep="")))
OBS <- c(obs$Conc,obs_2pt$Conc,obs_1pt$Conc)
res$Obs <- OBS
# Figure S7A1: Observations versus Individual predictions for misspecified structural model
Figure_S7_A1 <- ggplot(res, aes(x=IPRED, y=Obs)) + geom_point(colour="blue")+
  labs(x="IPRED",y="OBS")+
  geom_abline(intercept=0,slope=1,color="black",size=1)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  facet_grid(~Design,labeller=mf_labeller)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))

# Figure S7A2: IWRES versus Time for misspecified structural model
Figure_S7_A2 <- ggplot(res, aes(x=Time, y=IWRES)) + geom_point(colour="blue")+
  labs(x="Time",y="IWRES")+
  geom_abline(intercept=0,slope=0,color="black",size=1)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  facet_grid(~Design,labeller=mf_labeller)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))

# Figure S7B1: Observations versus Population predictions for misspecified structural model
Figure_S7_B1 <- ggplot(res, aes(x=PPRED, y=Obs)) + geom_point(colour="blue")+
  labs(x="PPRED",y="OBS")+
  geom_abline(intercept=0,slope=1,color="black",size=1)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  facet_grid(~Design,labeller=mf_labeller)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))

# Figure S7B2: PWRES versus Time for misspecified structural model
Figure_S7_B2 <- ggplot(res, aes(x=Time, y=PWRES)) + geom_point(colour="blue")+
  labs(x="Time",y="PWRES")+
  geom_abline(intercept=0,slope=0,color="black",size=1)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  facet_grid(~Design,labeller=mf_labeller)+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("")+
  theme(plot.title = element_text(size=16, face="bold"))


p <- arrangeGrob(Figure_S7_A1,Figure_S7_A2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S7A.tiff",p,width=10,height=8,dpi=300,compression="lzw")


p <- arrangeGrob(Figure_S7_B1,Figure_S7_B2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S7B.tiff",p,width=10,height=8,dpi=300,compression="lzw")

#### Figure S8: Different types of vpc graphs
# Compute pcVPC for true model
pcvpc1 <- pcvpc(obs,sim1,bin,percentile,alpha,log.y=F)
pcobsx <- pcvpc1$obs
pcvpc_classic1 <- pcvpc1$theoper
pcvpc1 <- pcvpc1$PI

# Compute VPC for true model
vpc1 <- vpc(obs,sim1,bin,percentile,alpha,log.y=F)
obsx <- vpc1$obs
vpc_classic1 <- vpc1$theoper
vpc1 <- vpc1$PI

# Scatter VPC for the True model
Figure_S8A <- ggplot(data=obsx ,aes(x=Time,y=Conc))+ geom_point(aes(colour=as.factor(AMT)),shape=1)+
  geom_ribbon(data=vpc_classic1, aes(x=meantime,y=medianPI,ymin=lowerPI,ymax=upperPI,fill="red"),alpha=0.1,col=NA,show.legend=F) +
  geom_line(data=vpc_classic1, aes(x=meantime,y=medianPI),col="black",size=1,linetype="dashed") +
  geom_line(data=vpc_classic1, aes(x=meantime,y=lowerPI),col="black",linetype="dashed") +
  geom_line(data=vpc_classic1, aes(x=meantime,y=upperPI),col="black",linetype="dashed") +
  scale_colour_manual(values=c("#2c7fb8","#df65b0","#2ca25f"),guide=F)+
  ylab("Observations")+ xlab("Time")+
  scale_x_continuous(breaks=seq(0,25,5))+
  scale_y_log10()+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

### Confidence interval VPC 

Figure_S8B <- ggplot(vpc1 ,aes(x=meantime,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  geom_line(aes(y=theomedian,group=PI),linetype="dashed",color="black") +
  geom_point(data=obsx,aes(x=Time,y=Conc,col=as.factor(AMT)),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#df65b0","#2ca65f","#2c7fb8","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("10% percentile","50% percentile","90% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("Observations")+ xlab("Time")+
  scale_x_continuous(breaks=seq(0,25,5))+
  scale_y_log10()+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

# Scatter pcVPC for True model

Figure_S8C <- ggplot(data=pcobsx ,aes(x=Time,y=pcConc))+ geom_point(aes(colour=as.factor(AMT)),shape=1)+
  geom_ribbon(data=pcvpc_classic1, aes(x=meantime,y=medianPI,ymin=lowerPI,ymax=upperPI,fill="red"),alpha=0.1,col=NA,show.legend=F) +
  geom_line(data=pcvpc_classic1, aes(x=meantime,y=medianPI),col="black",size=1,linetype="dashed") +
  geom_line(data=pcvpc_classic1, aes(x=meantime,y=lowerPI),col="black",linetype="dashed") +
  geom_line(data=pcvpc_classic1, aes(x=meantime,y=upperPI),col="black",linetype="dashed") +
  scale_colour_manual(values=c("#2c7fb8","#df65b0","#2ca25f"),guide=F)+
  ylab("Prediction corrected Observations")+ xlab("Time")+
  scale_x_continuous(breaks=seq(0,25,5))+
  scale_y_log10()+
  ggtitle("(C)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

# Confidence Interval pcVPC for True model

Figure_S8D <- ggplot(pcvpc1 ,aes(x=meantime,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(col=PI),size=1) +
  geom_line(aes(y=theomedian,group=PI),linetype="dashed",color="black") +
  geom_point(data=pcobsx,aes(x=Time,y=pcConc,col=as.factor(AMT)),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#df65b0","#2ca65f","#2c7fb8","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("10% percentile","50% percentile","90% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("Prediction corrected Observations")+ xlab("Time")+
  scale_x_continuous(breaks=seq(0,25,5))+
  scale_y_log10()+
  ggtitle("(D)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)


p <- arrangeGrob(Figure_S8A,Figure_S8B,Figure_S8C,Figure_S8D,ncol=2)
grid.draw(p)
ggsave(".\\Figure_S8.tiff",p,width=7,height=7,dpi=300,compression="lzw")

#### Figure S9

# Define different Prediction intervals for npc plots
PI <- c(0,20,40,50,60,80,90,95)
# Compute npc for different models
npccal1 <- npc(obs,sim1,PI)
npccal2 <- npc(obs,sim2,PI)
npccal3 <- npc(obs,sim3,PI)
npccal4 <- npc(obs,sim4,PI)
npccal5 <- npc(obs,sim5,PI)
npccal6 <- npc(obs,sim6,PI)

# Combine npc results
npccal <- plyr::rbind.fill(npccal1,npccal2,npccal3,npccal4,npccal5,npccal6)
npccal$Model <- rep(neworder,each=length(PI)*2) 
npccal$Model <- as.factor(npccal$Model)
npccal <- arrange(transform(npccal,Model=factor(Model,levels=neworder)),Model)

# Compute npc for true and misspecified covariate models, stratifying by Sex
npccal1_Sex0 <- npc(obs[obs$Sex==0,],sim1[sim1$Sex==0,],PI)
npccal1_Sex1 <- npc(obs[obs$Sex==1,],sim2[sim1$Sex==1,],PI)
npccal3_Sex0 <- npc(obs[obs$Sex==0,],sim3[sim3$Sex==0,],PI)
npccal3_Sex1 <- npc(obs[obs$Sex==1,],sim3[sim3$Sex==1,],PI)

# Combine npc results stratified by Sex
npccalSex <- plyr::rbind.fill(npccal1_Sex0,npccal1_Sex1,npccal3_Sex0,npccal3_Sex1)
npccalSex$Model <- rep(c("True model\n Concomitant treatment \n No",
                         "True model\n Concomitant treatment \n Yes",
                         "Misspecified covariate model\n Concomitant treatment \n No",
                         "Misspecified covariate model\n Concomitant treatment \n Yes"),each=length(PI)*2) 
npccalSex$Model <- as.factor(npccalSex$Model)
npccalSex <- arrange(transform(npccalSex,Model=factor(Model,levels=c("True model\n Concomitant treatment \n No",
                                                                     "True model\n Concomitant treatment \n Yes",
                                                                     "Misspecified covariate model\n Concomitant treatment \n No",
                                                                     "Misspecified covariate model\n Concomitant treatment \n Yes"))),Model)
npccalSex$Sex <- rep(c(0,1),each=length(PI)*2)

# Figure S9: Numerical Predictive Check for different models

Figure_S9_A <- ggplot(npccal[npccal$Model %in% c("True model", "Misspecified structural model","Constant error model",
                                                  "Proportional error model"),], aes(PI,Ratio))+
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
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=0.5)


Figure_S9_B <- ggplot(npccalSex,aes(PI,Ratio))+
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
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=0.5)


p <- arrangeGrob(Figure_S9_A,Figure_S9_B,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S9.tiff",p,width=10,height=7.5,dpi=300,compression="lzw")


### Figure S10: npd and transformed npd for True model
source("./npde_2_modified/func_plots_npd.R")
# Compute tnpd for true model
ta1 <- npde.plot.meanprofile(npde1,ref.prof=list(ID=100),xscale=T)
tnpdex1 <- data.frame(Time=rep(ta1$xcalsim$bnds$xcent,3),CIlower=matrix(ta1$xcalsim$bnds$binf,ncol=1),
                     CIupper=matrix(ta1$xcalsim$bnds$bsup,ncol=1),theomedian=matrix(ta1$xcalsim$bnds$bmed,ncol=1),
                     median=matrix(ta1$xcalobs$percobs,ncol=1))
tnpdex1$PI <- rep(c("10% percentile","50% percentile","90% percentile"),each=dim(tnpdex1)[1]/3)
tobsnpde1 <- ta1$xcalobs$plmat
tobsnpde1$AMT <- obs$AMT

obsnpde1$AMT <- obs$AMT

# npd for True model
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
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

# tnpd for True model
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
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

p <- arrangeGrob(Figure_S10A,Figure_S10B,ncol=2)
grid.draw(p)
ggsave(".\\Figure_S10.tiff",p,width=7,height=4,dpi=300,compression="lzw")



###########################
## PKPD warfarin example ##
###########################

setwd("C:/Users/Tram/Google Drive/ISOP MOEV/Submission/R script_Data/")


# function for Lowess regression for trend lines 
source('./stat_plsmo_new.R')

setwd("./PKPD_figures")

# Make sure that you have all required packages. 
# The order of loading packages is important as some will mask functions from others.
# All codes are compatible with the indicated versions of necessary R packages
# Further updates of the concerned packages may introduce breaks in R codes

suppressMessages(require(grid))       # version 3.1.2
suppressMessages(require(reshape2))   # version 1.4.1
suppressMessages(require(dplyr))      # version 0.4.3
suppressMessages(require(ggplot2))    # version 2.0.0
suppressMessages(require(tidyr))      # version 0.4.1
#suppressMessages(require(boot))
#suppressMessages(require(npde))
suppressMessages(require(gpairs))     # version 1.2
suppressMessages(require(gridExtra))  # version 2.2.1

# hack for npde
# Defining generic functions
# source("./npde_2_modified/global.R")

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
#covdata$SEX <- ifelse(covdata$SEX==1,"M","F")
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
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  #stat_smooth(method="gam",se=F,size=2,color="red",linetype="dashed")+
  ylab("Observed PCA")+ xlab("Population Prediction PCA")+
  scale_y_continuous(breaks=seq(0,120,20))+scale_x_continuous(breaks=seq(0,120,20))+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="none",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("(A)")


#Figure 6b: (DV vs IPRED) Observed PCA versus Individual Predictions IPRED

DVvsIPRED <-ggplot(gof ,aes((IPRED),(DV) ) )+
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=1,size=1)+
  facet_grid (~ModelName)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  #stat_smooth(method="loess",se=F,size=2,color="red",linetype="dashed")+
  ylab("Observed PCA")+ xlab("Individual Prediction PCA")+
  scale_y_continuous(breaks=seq(0,120,20))+scale_x_continuous(breaks=seq(0,120,20))+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="nonne",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("(B)")




#Figure 6cd: CWRES Residuals versus Time and PRED



gof<-  gof %>%
  filter(ObsName=="EObs")
gofcwres <- gather(gof[,c("ModelName","ID","IVAR","PRED","CWRES")], variable, value, -IVAR,-PRED, -ID, -ModelName)

CWRESvsTIME <- ggplot(gofcwres ,aes(IVAR,value))+
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=0,size=1)+
  facet_grid (~ModelName)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  #stat_smooth(method="loess",se=F,size=2,color="red",linetype="dashed")+
  ylab("CWRES")+ xlab("Time (h)")+
  scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,144,24))+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="nonne",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  #theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("(C)")

CWRESvsPRED<- ggplot(gofcwres ,aes(PRED,value))+
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=0,size=1)+
  facet_grid (~ModelName)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  #stat_smooth(method="loess",se=F,size=2,color="red",linetype="dashed")+
  ylab("CWRES")+ xlab("Population Prediction PCA")+
  scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,120,20))+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="nonne",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  #theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  ggtitle("(D)")

p <- arrangeGrob(DVvsPRED,DVvsIPRED,CWRESvsTIME,CWRESvsPRED,ncol=1)
grid.draw(p)
ggsave(".\\Figure_6.pdf",p,width=35,height=45,units="cm",dpi=300)



#Figure 7A: VPC and NPD

vpcplotmain<- ggplot(PCDATAmNOPRED ,aes(t,`50%_DVPLOT`,ymin=`2.5%_DVPLOT`,ymax=`97.5%_DVPLOT`,fill=QI))+
  facet_grid(~ModelName)+
  geom_ribbon(alpha=0.1,col=NA) +
  geom_line(col="black",linetype="dashed") +
  geom_line(aes(y=`--_DVPLOT`,linetype=observed,color=QI),size=1) +
  scale_linetype_manual(name="Observed (dashed lines)",breaks=c("Observed"),values=c("solid"))+
  scale_fill_manual (name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                     breaks=c("05%","50%", "95%"),values=c("blue","red","blue"))+
  ylab("\n Simulated and Observed PCA Quantiles")+ xlab("Time (h)")+
  scale_y_continuous(breaks=seq(0,120,20))+scale_x_continuous(breaks=seq(0,144,24))+
  scale_colour_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                      breaks=c("05%","50%","95%"),values=c("#2c7fb8","#df65b0","#2c7fb8"))+
  ggtitle("(A)")+
  theme(legend.position="none",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+#,plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  theme(plot.title = element_text(size=16, face="bold"))


#Figure VPC2: PRED corrected VPC


vpcplotmain2 <- ggplot(PCDATAm ,aes(t,`50%_DVPLOT`,ymin=`2.5%_DVPLOT`,ymax=`97.5%_DVPLOT`,fill=QI))+
  facet_grid(~ModelName)+
  geom_ribbon(alpha=0.1,col=NA ) +
  geom_line(linetype="dashed") +
  geom_line(aes(y=`--_DVPLOT`,linetype=observed,col=QI),size=1) +
  scale_linetype_manual(name="Observerd (dashed lines)",breaks=c("Observed"),values=c("solid"))+
  scale_colour_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                      breaks=c("05%","50%","95%"),values=c("#2c7fb8","#df65b0","#2c7fb8"))+
  scale_fill_manual  (name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                      breaks=c("05%","50%", "95%"),values=c("blue","red","blue"))+
  ylab("Prediction Corrected\n Simulated and Observed PCA Quantiles")+ xlab("Time (h)")+
  scale_y_continuous(breaks=seq(0,120,20))+scale_x_continuous(breaks=seq(0,144,24))+
  ggtitle("(B)")+
  theme(legend.position="none",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+#,plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  #theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  theme(plot.title = element_text(size=16, face="bold"))



#Figure 7C & 7D: NPD & tranformed NPD
# Defining generic functions
source("./npde_2_modified/global.R")

# Classes
source("./npde_2_modified/NpdeData.R")
source("./npde_2_modified/NpdeRes.R")
source("./npde_2_modified/NpdeObject.R")

# Main function
source("./npde_2_modified/func_methods.R")
source("./npde_2_modified/func_plots_npd.R")
source("./npde_2_modified/main.R")


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

# npd vs Time
NPDETRUE<- ggplot(data.frame(npdex),aes(x=Time,y=median))+
  facet_grid(~ModelName) +
  geom_line(aes(y=theomedian,group=PI),color="black",linetype="dashed") +
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(group=PI,col=PI),size=1) +
  geom_point(data=obsnpde,aes(x=x,y=ty),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#df65b0","#2c7fb8","blue","red","blue"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("5% percentile","50% percentile","95% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("\n npd")+ xlab("Time (h)")+
  scale_y_continuous(breaks=seq(-3.5,3.5,1))+scale_x_continuous(breaks=seq(0,144,24))+
  ggtitle("(C)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

# tnpd vs Time
NPDETRUESCALE <- ggplot(data.frame(npdext),aes(x=Time,y=median))+
  facet_grid(~ModelName)+
  geom_line(aes(y=theomedian,group=PI),linetype="dashed",color="black") +
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_line(aes(group=PI,col=PI),size=1) +
  geom_point(data=obsnpde1t,aes(x=x,y=ty),shape=1)+
  scale_colour_manual(values=c("#2c7fb8","#df65b0","#2c7fb8","blue","red","blue"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("5% percentile","50% percentile","95% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("\n Transformed npd")+ xlab("Time (h)")+
  scale_y_continuous(breaks=seq(0,100,20))+scale_x_continuous(breaks=seq(0,144,24))+
  ggtitle("(D)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

NPDETRUE
NPDETRUESCALE
p <- arrangeGrob(vpcplotmain,vpcplotmain2,NPDETRUE,NPDETRUESCALE,ncol=1)
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
  scale_x_continuous(breaks=seq(0,144,24))+
  scale_y_continuous(breaks=seq(0,120,20))+
  scale_colour_manual(values =  c("red","blue","green", "black","gray")) +
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="none",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)


FIGS11
ggsave(".\\Figure_S11.pdf",FIGS11,width=15,height=15,units="cm",dpi=300)


#Figure S12: Simulation Based Residuals versus Time and PRED
#PCWRES is similar to WRES and CWRES, but computed with a different covariance matrix. The covariance matrix for PCWRES is computed from simulations using the time points in the data.


gof<-  gof %>%
  filter(ObsName=="EObs")

gofcwres <- gather(gof[,c("ModelName","ID","IVAR","PRED","PCWRES","CWRES","WRES")], variable, value, -IVAR,-PRED, -ID, -ModelName)
gofcwres <- gather(gof[,c("ModelName","ID","IVAR","PRED","PCWRES")], variable, value, -IVAR,-PRED, -ID, -ModelName)
PCWRESvsTIME<- ggplot(gofcwres ,aes(IVAR,value))+
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=0,size=1)+
  facet_grid (~ModelName)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  #stat_smooth(method="loess",se=F,size=2,color="red",linetype="dashed")+
  ylab("PWRES")+ xlab("Time (h)")+
  scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,144,24))+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)


PCWRESvsPRED <- ggplot(gofcwres ,aes(PRED,value))+
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=0,size=1)+
  facet_grid (~ModelName)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  #stat_smooth(method="loess",se=F,size=2,color="red",linetype="dashed")+
  ylab("PWRES")+ xlab("Population Prediction PCA")+
  scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,120,20))+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)


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
  theme(legend.position="none",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

individualfitexample

ggsave(".\\Figure_S13.pdf",individualfitexample,width=35,height=12,units="cm",dpi=300)




#Figure S14: (IWRES vs PRED and vs TIME)

gof<-  gof %>%
  filter(ObsName=="EObs")
gofcwres <- gather(gof[,c("ModelName","ID","IVAR","IPRED","IWRES")], variable, value, -IVAR,-IPRED, -ID, -ModelName)

IWRESvsTIME <- ggplot(gofcwres ,aes(IVAR,value))+
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=0,size=1)+
  facet_grid (~ModelName)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  #stat_smooth(method="loess",se=F,size=2,color="red",linetype="dashed")+
  ylab("IWRES")+ xlab("Time (h)")+
  scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,144,24))+
  ggtitle("(A)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray",linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"), aspect.ratio=1)

IWRESvsPRED <- ggplot(gofcwres ,aes(PRED,value))+
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=0,size=1)+
  facet_grid (~ModelName)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  #stat_smooth(method="loess",se=F,size=2,color="red",linetype="dashed")+
  ylab("IWRES")+ xlab("Population Prediction PCA")+
  scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,120,20))+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

p <- arrangeGrob(IWRESvsTIME,IWRESvsPRED,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S14.pdf",p,width=35,height=25,units="cm",dpi=300)



#Figure S15 Etas: Eta pairs plots
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

pdf("Figure_S15D.pdf" , width = 10, height = 7)
gpairs( eta1[,c("nV","nCl","nTabs","nTlag","nBaseline","nTeq","nEC50")],
        upper.pars = list(conditional = 'boxplot', scatter = 'loess'),
        lower.pars = list(scatter = 'stats',conditional = "barcode"),
        diag.pars = list(fontsize = 9, show.hist = TRUE, hist.color = "gray"),
        stat.pars =list(fontsize = 12, signif = 0.05, verbose = FALSE, use.color = TRUE, missing = 'missing', just = 'centre'),
        scatter.pars = list(pch = 20))
dev.off()



#Figure S16A  Etas: Eta plots vs  continuous covariates

etaplot <- gather(eta1[,c("nV","nCl","nTabs","nTlag","WT","AGE")], EtaName, Eta, -AGE,-WT)
etaplot <- gather(etaplot, CovrName, Covr,-EtaName,-Eta)

etacontinuous_1<- ggplot(etaplot[etaplot$CovrName=="WT",] , aes(x =Covr, y =  Eta)) +
  geom_point(col="blue",size=2)+
  geom_smooth(method = "gam", method.args = list(se = F), col="red",size=1.5,se=F)+
  geom_hline(yintercept=0)+
  facet_wrap(~EtaName,scales="free_y",ncol=4)+
  ylab("Individual Random Effects")+
  xlab("Weight")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),legend.title = element_blank(),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  ggtitle("(A)")+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text.x = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  theme(text = element_text(size=20)) +
  guides(col = guide_legend(nrow = 2))#+theme_bw()
etacontinuous_2<- ggplot(etaplot[etaplot$CovrName=="AGE",] , aes(x =Covr, y =  Eta)) +
  geom_point(col="blue",size=2)+
  geom_smooth(method = "gam", method.args = list(se = F), col="red",size=1.5,se=F)+
  geom_hline(yintercept=0)+
  facet_wrap(~EtaName,scales="free_y",ncol=4)+
  ylab("Individual Random Effects")+
  xlab("Age")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),legend.title = element_blank(),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text.x = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  theme(text = element_text(size=20)) +
  guides(col = guide_legend(nrow = 2))

p <- arrangeGrob(etacontinuous_1,etacontinuous_2,ncol=1)
grid.draw(p)
ggsave(".\\Figure_S16A.pdf",p,width=20,height=20,units="cm",dpi=300)




#FigureS16B Etas: Eta plots vs  categorical covariates

etaplot <- gather(eta1[,c("nV","nCl","nTabs","nTlag","SEX")], EtaName, Eta, -SEX)
etaplot <- gather(etaplot, CovrName, Covr,-EtaName,-Eta)

etacat<- ggplot(etaplot  , aes(x =Covr, y =  Eta)) +
  geom_boxplot(col="black",alpha=0.2)+
  geom_jitter(col="blue")+
  stat_summary(fun.y=median, geom="line", aes(group=1),col="red")  +
  facet_wrap(~EtaName,scales = "free",ncol=4)+
  geom_hline(yintercept=0)+
  ylab("Individual Random Effects")+
  xlab("")+theme_bw()+
  ggtitle("(B)")+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="bottom",axis.text=element_text(size=12),
        axis.title=element_text(size=14),plot.margin=unit(c(0,1,0,1), "cm"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

ggsave(".\\Figure_S16B.pdf",etacat,width=25,height=8,units="cm",dpi=300)


# Figure S17 Numerical Predictive Checks for different models

NPCPLOT<- ggplot(npccal, aes(PI,Ratio))+
  geom_hline(yintercept=1,linetype="dashed")+
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
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

ggsave(".\\Figure_S17.pdf",NPCPLOT,width=35,height=20,units="cm",dpi=300)



