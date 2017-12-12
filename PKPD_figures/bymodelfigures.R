require(export)
# export is not available for R 3.3.2
require(ggplot2)
require(grid)
require(gridExtra)
require(gpairs)
#[1] d.True model\n Turnover PD                                           
#[2] a.Misspecified delay\n Immediate effect                              
#[3] b.Misspecified delay\n Effect compartment                            
#[4] c.Misspecified delay and correlation\n Effect compartment, Full Omega
setwd("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/")
npdex <- read.csv("npdex.csv")
npdepredx <- read.csv("npdepredx.csv")
obsnpde <- read.csv("obsnpde.csv")
npccal <- read.csv("npccal.csv")
obs <- read.csv("obs.csv")

gof <- read.csv("gof.csv")

PCDATAmNOPRED <- read.csv("PCDATAmNOPRED.csv")

source("stat_plsmo_new.R")

richpredipred <- read.csv("richpredipred.csv")
names(richpredipred) <- c("ID","IVAR","IPRED","DV","PRED","ModelName")
eta1 <- read.csv(".\\1\\Etacov.csv")
eta2 <- read.csv(".\\2\\Etacov.csv")
eta3 <- read.csv(".\\3\\Etacov.csv")
eta4 <- read.csv(".\\4\\Etacov.csv")

individualfitsplot2 <- function (richpredipred=richpredipred,ModelName="All", ...)
{
  ModelName=ModelName
  if(ModelName=="All") {
    gofID <- richpredipred[
      richpredipred$ID%in%c(0,21,15), ]
  }
  if(ModelName!="All") {
    gofID <- richpredipred[
      richpredipred$ID%in%c(0,21,15), ]
    gofID<- gofID[is.element(gofID$ModelName,ModelName ),]
    
  }
  
  individualfitexample<- ggplot(gofID ,aes(IVAR,DV))+
    geom_point(color="blue",size=2)+
    geom_line(aes(y=IPRED),col="green")+
    geom_line(aes(y=PRED),col="red")+
    facet_grid (~ID)+
    scale_x_continuous(breaks=seq(0,144,24))+
    ylab("PCA")+xlab("Time")+
    theme(legend.position="none",axis.text=element_text(size=12),
          axis.title=element_text(size=14))+
    theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
    theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
    theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
    theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)
  
  individualfitexample
  if(ModelName=="All")
    individualfitexample <- individualfitexample+
    facet_grid (ModelName~ID)
  individualfitexample <- arrangeGrob(individualfitexample,ncol=1)
  grid.draw(individualfitexample)
  return(individualfitexample)
}



#ModelName="All"
basiggofplot1 <- function (gofdata=gof,ModelName="All", ...)
{
 # gofdata<- gof
 # ModelName="All"
  ModelName=ModelName
  
if(ModelName=="All") {
  gofplot <- gofdata[gofdata$ObsName!="CObs",]
  
}
  # ModelName="d.True model\n Turnover PD"
  
  if(ModelName!="All") {
    gofplot <- gofdata[gofdata$ObsName!="CObs"&
                         is.element(gofdata$ModelName,ModelName ),]
    
  }
  
  
fig1a <- ggplot(gofplot,aes(PRED,DV))+
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=1,size=1)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  ylab("OBS")+ xlab("PPRED")+
  scale_y_continuous(breaks=seq(0,120,20))+scale_x_continuous(breaks=seq(0,120,20))+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="nonne",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

if(ModelName=="All")
  fig1a <- fig1a+
  facet_grid (~ModelName)+
  ggtitle("(A)")
  
fig1d   <-  ggplot(gofplot ,aes(IPRED,DV))+
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=1,size=1)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  ylab("OBS")+ xlab("IPRED")+
  scale_y_continuous(breaks=seq(0,120,20))+scale_x_continuous(breaks=seq(0,120,20))+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="nonne",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1) 

if(ModelName=="All")
  fig1d <- fig1d+
  facet_grid (~ModelName)+
  ggtitle("  ")

fig1 <- arrangeGrob(fig1a,fig1d,ncol=1)
grid.draw(fig1)
return(fig1)
}

basiggofplot2 <- function (gofdata=gof,ModelName="All", ...)
{
  
  ModelName=ModelName
  
  if(ModelName=="All") {
    gofplot <- gofdata[gofdata$ObsName!="CObs",]
    
  }
  if(ModelName!="All") {
    gofplot <- gofdata[gofdata$ObsName!="CObs"&
                         is.element(gofdata$ModelName,ModelName ),]
    
  }
  
  

fig1c   <- ggplot(gofplot ,aes(IVAR,PCWRES))+ 
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=0,size=1)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  ylab("PWRES")+ xlab("Time")+
  scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,144,24))+
  
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="nonne",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

if(ModelName=="All")
  fig1c <- fig1c+
  facet_grid (~ModelName)+
  ggtitle("(B)")



fig1f <- ggplot(gofplot,aes(IVAR,IWRES))+
  geom_point(color="blue")+
  geom_abline(intercept=0,slope=0,size=1)+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  ylab("IWRES")+ xlab("Time")+
  scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,144,24))+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="nonne",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

if(ModelName=="All")
  fig1f <- fig1f+
  facet_grid (~ModelName)+
  ggtitle(" ")
fig1 <- arrangeGrob(fig1c,fig1f,ncol=1)
grid.draw(fig1)
return(fig1)
}


basiggofplot3 <- function (gofdata=gof,ModelName="All",logx=FALSE, ...)
{
  
  ModelName=ModelName
  
  if(ModelName=="All") {
    gofplot <- gofdata[gofdata$ObsName!="CObs",]
    
  }
  if(ModelName!="All") {
    gofplot <- gofdata[gofdata$ObsName!="CObs"&
                         is.element(gofdata$ModelName,ModelName ),]
    
  }
  



fig1b <-  ggplot(gofplot ,aes(PRED,PCWRES))+
  geom_point(color="blue")
if(!logx)  {
  fig1b <-fig1b+
    geom_abline(intercept=0,slope=0,size=1)
}
  if(logx)  {
    fig1b <-fig1b+
      geom_abline(intercept=0,slope=0,size=1)
  }
  
  fig1b <- fig1b+
    stat_plsmo(color="red",size=1.5,linetype="dashed")+
    ylab("PWRES")+ xlab("PPRED")+
  scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,120,20))+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="nonne",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)
if(ModelName=="All")
  fig1b <- fig1b+
  facet_grid (~ModelName)+
  ggtitle("(C)")

  if(logx)  {
    fig1b <-fig1b+
      scale_x_log10()+
      xlab("log(PPRED)")
  }

fig1e   <-  ggplot(gofplot ,aes(IPRED,IWRES))+
  geom_point(color="blue")
if(!logx)  {
  fig1e <-fig1e+
    geom_abline(intercept=0,slope=0,size=1)
}
if(logx)  {
  fig1e <-fig1e+
    geom_abline(intercept=0,slope=0,size=1)
}

fig1e <- fig1e+
  stat_plsmo(color="red",size=1.5,linetype="dashed")+
  ylab("IWRES")+ xlab("IPRED")+
  scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,100,20))+
  theme(plot.title = element_text(size=16, face="bold"))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="nonne",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

if(logx)  {
  fig1e <-fig1e+
    scale_x_log10()+
    xlab("log(IPRED)")
}

if(ModelName=="All")
  fig1e <- fig1e+
  facet_grid (~ModelName)+
  ggtitle("  ")


fig1 <- arrangeGrob(fig1b,fig1e,ncol=1)
grid.draw(fig1)
return(fig1)
}

vpcplotpredcorr <- function (PCDATAm=PCDATAm,ModelName="All", ...)
{
  ModelName=ModelName
    if(ModelName=="All") {
    PCDATAmplot <- PCDATAm[PCDATAm$ObsName=="EObs",]
  }
  if(ModelName!="All") {
    PCDATAmplot <- PCDATAm[PCDATAm$ObsName=="EObs"&
                         is.element(PCDATAm$ModelName,ModelName ),]
    
  }
  
vpcplot<- ggplot(PCDATAmplot ,
       aes(t,`X50._DVPLOT`,ymin=`X2.5._DVPLOT`,ymax=`X97.5._DVPLOT`,
           fill=QI,col=QI))+
  geom_ribbon(alpha=0.1,col=NA ) +
  geom_point(data=obs[obs$ObsName=="EObs",],aes(x=t,y=DV ),color="blue",inherit.aes = FALSE,shape=1)+
  geom_line(linetype="dashed",color="black") +
  geom_line(aes(y=`X.._DVPLOT`,color=QI),size=1.5) +
  scale_linetype_manual(  name="Observerd (dashed lines)",breaks=c("Observed"),values=c("dashed"))+
  scale_colour_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                      breaks=c("05%","50%","95%"),values=c("#2c7fb8","#df65b0","#2c7fb8"))+
  scale_fill_manual  (name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                      breaks=c("05%","50%", "95%"),values=c("blue","red","blue"))+
  ylab("PCA")+ xlab("Time")+
  scale_y_continuous(breaks=seq(0,120,20))+scale_x_continuous(breaks=seq(0,144,24))+
  theme(legend.position="none",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)+
  theme(plot.title = element_text(size=16, face="bold"))

if(ModelName=="All")
  vpcplot <- vpcplot+
  facet_grid (~ModelName)+
  ggtitle("(A)")

vpcplot <- arrangeGrob(vpcplot,ncol=1)
grid.draw(vpcplot)
return(vpcplot)
}

npdeplot1 <- function (npdex=npdex,ModelName="All", ...)
{
  ModelName=ModelName
  if(ModelName=="All") {
    npdexplot <- npdex
  }
  if(ModelName!="All") {
    npdexplot <- npdex[is.element(npdex$ModelName,ModelName ),]
  }
   
  
npdeplotobj <-  ggplot(npdexplot ,aes(x=Time,y=median))+
  geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
  geom_point(data=obsnpde,aes(x=x,y=y),shape=1,col="blue")+
  geom_line(aes(y=theomedian,group=PI),linetype="dashed",color="black") +
  geom_line(aes(group=PI,col=PI),size=1.5) +
  scale_colour_manual(values=c("#2c7fb8","#df65b0","#2c7fb8"),guide=F)+
  scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                    breaks=c("10% percentile","50% percentile","90% percentile"),values=c("blue","red","blue"),guide=F)+
  ylab("npd")+ xlab("Time")+
  scale_y_continuous(breaks=c(0,seq(-3.5,3.5,1)) ) +
  scale_x_continuous(breaks=seq(0,144,24) )+
  theme(legend.position="none",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
    theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)
theme(plot.title = element_text(size=16, face="bold"))

if(ModelName=="All")
  npdeplotobj <- npdeplotobj+
  facet_grid (~ModelName)+
  ggtitle("(A)")

npdeplot <- arrangeGrob(npdeplotobj,ncol=1)
grid.draw(npdeplot)
return(npdeplot)
}




npdeplot2 <- function (npdepredx=npdepredx,ModelName="All", logx=FALSE,...)
{
  ModelName=ModelName
  if(ModelName=="All") {
    npdepredxplot <- npdepredx
  }
  if(ModelName!="All") {
    npdepredxplot <- npdepredx[is.element(npdepredx$ModelName,ModelName ),]
  }
  npdepredplotobj<-  ggplot(npdepredxplot ,aes(x=Time,y=median))+
    geom_ribbon(aes(ymin=CIlower,ymax=CIupper,fill=PI,col=PI),alpha=0.1,col=NA)+
    geom_point(data=obsnpde,aes(x=PRED,y= y),shape=1,color="blue",inherit.aes = FALSE)+
    
    geom_line(aes(y=theomedian,group=PI),linetype="dashed",color="black") +
    geom_line(aes(group=PI,col=PI),size=1.5) +
    scale_colour_manual(values=c("#2c7fb8","#df65b0","#2c7fb8","blue","red","blue"),guide=F)+
    scale_fill_manual(name="Prediction Intervals 95% CI (ribbons)\n Median Predictions (solid lines)",
                      breaks=c("5% percentile","50% percentile","95% percentile"),values=c("blue","red","blue"),guide=F)+
    ylab("npd")+ xlab("PPRED")+
    scale_y_continuous(breaks=c(0,seq(-3.5,3.5,1)) ) +
    scale_x_continuous(breaks=seq(0,100,20) ) +
    theme(legend.position="none",axis.text=element_text(size=12),
          axis.title=element_text(size=14))+
    theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
    theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
    theme(strip.text = element_text(size = 14, colour = "black"),aspect.ratio=1)
  theme(plot.title = element_text(size=16, face="bold"))
  
  if(logx)  {
    npdepredplotobj <-npdepredplotobj+
      scale_x_log10()+
      xlab("log(PPRED)")
    }

  if(ModelName=="All")
    npdepredplotobj <- npdepredplotobj+
    facet_grid (~ModelName)+
    ggtitle("(B)")
  npdeplot <- arrangeGrob(npdepredplotobj,ncol=1)
  grid.draw(npdeplot)
  return(npdeplot)
}

individualfitsplot <- function (gof=gof,ModelName="All", ...)
{
  ModelName=ModelName
  if(ModelName=="All") {
    gofID <- gof[
      gof$ObsName=="EObs"&
        gof$ID%in%c(0,21,15), ]
    }
  if(ModelName!="All") {
    gofID <- gof[
      gof$ObsName=="EObs"&
        gof$ID%in%c(0,21,15), ]
    gofID<- gofID[is.element(gofID$ModelName,ModelName ),]
    
    }

individualfitexample<- ggplot(gofID ,aes(IVAR,DV))+
  geom_point(color="blue",size=2)+
  geom_line(aes(y=IPRED),col="green")+
  geom_line(aes(y=PRED),col="red")+
  facet_grid (~ID)+
  scale_x_continuous(breaks=seq(0,144,24))+
  ylab("OBS/IPRED/PRED")+xlab("Time")+
  theme(legend.position="none",axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)

individualfitexample
if(ModelName=="All")
  individualfitexample <- individualfitexample+
  facet_grid (ModelName~ID)
individualfitexample <- arrangeGrob(individualfitexample,ncol=1)
grid.draw(individualfitexample)
return(individualfitexample)
}

#ggsave(".\\figgoftrue.pdf",figtrue,width=25,height=16.7,units="cm",dpi=300)

#tmat <- matrix(unlist(goftrueETA) ,ncol=3,byrow=FALSE ) #individual etas
#var <- diag(c(0.001978949,0.013822067	,0.13725865))
#var.eig <- eigen(var)
#xmat <- var.eig$vectors %*% diag(sqrt(var.eig$values)) %*% solve(var.eig$vectors)
#ymat<-try(solve(xmat))
#deceta<- as.data.frame(tmat %*% ymat)
#colnames(deceta) <- c("E0","EC50","TEQ")


#basiggofplot1(ModelName ="All" )
#basiggofplot2(ModelName ="All" )
#basiggofplot3(ModelName ="All" )
#vpcplotpredcorr( PCDATAm=PCDATAm,ModelName ="All" )
#npdeplot2( npdepredx=npdepredx,ModelName ="All" )
#npdeplot( npdex=npdex,npdepredx=npdepredx,ModelName ="All" )

a<- basiggofplot1(ModelName ="d.True model\n Turnover PD" )
b<- basiggofplot2(ModelName ="d.True model\n Turnover PD" )
c<- basiggofplot3(ModelName ="d.True model\n Turnover PD" ,logx=FALSE)

Fig7A <- grid.arrange(a,b,c,ncol=3, top=textGrob("(A) Basic Goodness-of-fit Plots (True model - Turn-over model)",gp=gpar(fontsize=16,fontface="bold")))
ggsave("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_7A.tiff",Fig7A,compression="lzw",width=10,height=7,dpi=300)

d<- vpcplotpredcorr( PCDATAm=PCDATAmNOPRED,ModelName ="d.True model\n Turnover PD" )
e<- npdeplot1( npdex=npdex,ModelName ="d.True model\n Turnover PD" )
f<- npdeplot2(npdepredx=npdepredx,ModelName ="d.True model\n Turnover PD",logx=FALSE )

Fig7D <- grid.arrange(d,e,f,ncol=3, top=textGrob("(D) Simulation-based Goodness-of-fit Plots (True model - Turn-over model)",gp=gpar(fontsize=16,fontface="bold")))
ggsave("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_7D.tiff",Fig7D,compression="lzw",width=10,height=3.5,dpi=300)

#j<- individualfitsplot(gof=gof,ModelName ="d.True model\n Turnover PD")
h<- individualfitsplot2(richpredipred=richpredipred,
                        ModelName ="d.True model\\n Turnover PD")
Fig7B <- grid.arrange(h,ncol=1, top=textGrob("(B) Representative Individual Fits (True model - Turn-over model)",gp=gpar(fontsize=16,fontface="bold")))
ggsave("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_7B.tiff",Fig7B,compression="lzw",width=10,height=3.5,dpi=300)


a<- basiggofplot1(ModelName ="a.Misspecified delay\n Immediate effect" )
b<- basiggofplot2(ModelName ="a.Misspecified delay\n Immediate effect" )
c<- basiggofplot3(ModelName ="a.Misspecified delay\n Immediate effect",logx=FALSE )
d<- vpcplotpredcorr( PCDATAm=PCDATAmNOPRED,ModelName ="a.Misspecified delay\n Immediate effect" )
e<- npdeplot1( npdex=npdex,ModelName ="a.Misspecified delay\n Immediate effect" )
f<- npdeplot2(npdepredx=npdepredx,ModelName ="a.Misspecified delay\n Immediate effect" ,logx=FALSE )
#j<- individualfitsplot(gof=gof,ModelName ="a.Misspecified delay\n Immediate effect")
h<- individualfitsplot2(richpredipred=richpredipred,
                        ModelName ="a.Misspecified delay\\n Immediate effect")

Fig4A <- grid.arrange(a,b,c,ncol=3, top=textGrob("(A) Basic Goodness-of-fit Plots (Misspecified delay, Immediate effect)",gp=gpar(fontsize=16,fontface="bold")))
ggsave("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_4A.tiff",Fig4A,compression="lzw",width=10,height=7,dpi=300)
Fig4D <- grid.arrange(d,e,f,ncol=3, top=textGrob("(D) Simulation-based Goodness-of-fit Plots (Misspecified delay, Immediate effect)",gp=gpar(fontsize=16,fontface="bold")))
ggsave("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_4D.tiff",Fig4D,compression="lzw",width=10,height=3.5,dpi=300)
Fig4B <- grid.arrange(h,ncol=1, top=textGrob("(B) Representative Individual Fits (Misspecified delay, Immediate effect)",gp=gpar(fontsize=16,fontface="bold")))
ggsave("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_4B.tiff",Fig4B,compression="lzw",width=10,height=3.5,dpi=300)


a<- basiggofplot1(ModelName ="b.Misspecified delay\n Effect compartment" )
b<- basiggofplot2(ModelName ="b.Misspecified delay\n Effect compartment" )
c<- basiggofplot3(ModelName ="b.Misspecified delay\n Effect compartment" ,logx=FALSE)
d<- vpcplotpredcorr( PCDATAm=PCDATAmNOPRED,ModelName ="b.Misspecified delay\n Effect compartment" )
e<- npdeplot1( npdex=npdex,ModelName ="b.Misspecified delay\n Effect compartment" )
f<- npdeplot2(npdepredx=npdepredx,ModelName ="b.Misspecified delay\n Effect compartment",logx=FALSE  )
#j<- individualfitsplot(gof=gof,ModelName ="b.Misspecified delay\n Effect compartment")
h<- individualfitsplot2(richpredipred=richpredipred,
                        ModelName ="b.Misspecified delay\\n Effect compartment")


Fig5A <- grid.arrange(a,b,c,ncol=3, top=textGrob("(A) Basic Goodness-of-fit Plots (Misspecified delay, Effect compartment)",gp=gpar(fontsize=16,fontface="bold")))
ggsave("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_5A.tiff",Fig5A,compression="lzw",width=10,height=7,dpi=300)
Fig5D <- grid.arrange(d,e,f,ncol=3, top=textGrob("(D) Simulation-based Goodness-of-fit Plots (Misspecified delay, Effect compartment)",gp=gpar(fontsize=16,fontface="bold")))
ggsave("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_5D.tiff",Fig5D,compression="lzw",width=10,height=3.5,dpi=300)
Fig5B <- grid.arrange(h,ncol=1, top=textGrob("(B) Representative Individual Fits (Misspecified delay, Effect compartment)",gp=gpar(fontsize=16,fontface="bold")))
ggsave("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_5B.tiff",Fig5B,compression="lzw",width=10,height=3.5,dpi=300)


a<- basiggofplot1(ModelName ="c.Misspecified delay and correlation\n Effect compartment, Full Omega" )
b<- basiggofplot2(ModelName ="c.Misspecified delay and correlation\n Effect compartment, Full Omega" )
c<- basiggofplot3(ModelName ="c.Misspecified delay and correlation\n Effect compartment, Full Omega" ,logx=FALSE)
d<- vpcplotpredcorr( PCDATAm=PCDATAmNOPRED,ModelName ="c.Misspecified delay and correlation\n Effect compartment, Full Omega" )
e<- npdeplot1( npdex=npdex,ModelName ="c.Misspecified delay and correlation\n Effect compartment, Full Omega" )
f<- npdeplot2(npdepredx=npdepredx,ModelName ="c.Misspecified delay and correlation\n Effect compartment, Full Omega" ,logx=FALSE )
#j<- individualfitsplot(gof=gof,ModelName ="c.Misspecified delay and correlation\n Effect compartment, Full Omega")
h<- individualfitsplot2(richpredipred=richpredipred,
                        ModelName ="c.Misspecified delay and correlation\\n Effect compartment, Full Omega")



Fig6A <- grid.arrange(a,b,c,ncol=3, top=textGrob("(A) Basic Goodness-of-fit Plots (Misspecified delay and correlation)",gp=gpar(fontsize=16,fontface="bold")))
ggsave("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_6A.tiff",Fig6A,compression="lzw",width=10,height=7,dpi=300)
Fig6D <- grid.arrange(d,e,f,ncol=3, top=textGrob("(D) Simulation-based Goodness-of-fit Plots (Misspecified delay and correlation)",gp=gpar(fontsize=16,fontface="bold")))
ggsave("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_6D.tiff",Fig6D,compression="lzw",width=10,height=3.5,dpi=300)
Fig6B <- grid.arrange(h,ncol=1, top=textGrob("(B) Representative Individual Fits (Misspecified delay and correlation)",gp=gpar(fontsize=16,fontface="bold")))
ggsave("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_6B.tiff",Fig6B,compression="lzw",width=10,height=3.5,dpi=300)


npccal$Type <- factor(npccal$Type,
                      levels=c("Upper PI Limit","Lower PI Limit"))
NPCPLOT<- ggplot(npccal, aes(PI,Ratio))+
  geom_hline(yintercept=1,linetype="dashed")+
  geom_point(aes(col=Outliers,pch=Outliers),size=5)+geom_line()+
  geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.1,fill="red")+
  facet_grid( Type ~ ModelName)+
  scale_colour_manual("Outside the CI",breaks=c("No","Yes"),values=c("black","red"))+
  scale_shape_manual("Outside the CI",breaks=c("No","Yes"),values=c(1,8))+
  ylab("Observed/Expected")+
  xlab("Predictions Intervals")+
  theme(legend.position="bottom")+
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=0, vjust=1))+
  coord_cartesian(ylim=c(-0.5,3.5),xlim=c(-5,100))+
  theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
  theme(legend.position="none",axis.text=element_text(size=18),legend.title = element_blank(),
        axis.title=element_text(size=18,face="bold"))+
  theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
  theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
  theme(strip.text = element_text(size = 10, colour = "black"),aspect.ratio=0.5)+
  theme(text = element_text(size=20)) +
  theme(panel.margin.x = grid::unit(1, "lines"))

ggsave("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_S12.tiff",NPCPLOT,compression="lzw",width=12.5,height=3.5,dpi=300)

tiff("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_7C.tiff",width=16,height=11,unit="cm",res=300)
gpairs( eta1[,c("nBaseline","nTeq","nEC50")], 
        upper.pars = list(conditional = 'boxplot', scatter = 'loess'),
        lower.pars = list(scatter = 'stats',conditional = "barcode"),
        diag.pars = list(fontsize = 9, show.hist = TRUE, hist.color = "gray"),
        stat.pars =list(fontsize = 12, signif = 0.05, verbose = FALSE, use.color = TRUE, missing = 'missing', just = 'centre'),
        scatter.pars = list(pch = 20), outer.margins = list(bottom = unit(2, "lines"), 
                                                            left = unit(2, "lines"), 
                                                            top = unit(3.5, "lines"), 
                                                            right = unit(2, "lines")))
grid.text('(C) Correlations, histogram of EBE (True model - Turn-over model)', x=.5, y=.95,gp=gpar(fontsize=12,fontface="bold"))

dev.off()



eta2$nBaseline <- eta2$nE0
tiff("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_4C.tiff",width=16,height=11,unit="cm",res=300)
gpairs( eta2[,c("nBaseline","nEmax","nEC50")], 
        upper.pars = list(conditional = 'boxplot', scatter = 'loess'),
        lower.pars = list(scatter = 'stats',conditional = "barcode"),
        diag.pars = list(fontsize = 9, show.hist = TRUE, hist.color = "gray"),
        stat.pars =list(fontsize = 12, signif = 0.05, verbose = FALSE, use.color = TRUE, missing = 'missing', just = 'centre'),
        scatter.pars = list(pch = 20),outer.margins = c(2,2,3.5,2))
grid.text('(C) Correlations, histogram of EBE (Misspecified delay, Immediate effect)', x=.5, y=.95,gp=gpar(fontsize=12,fontface="bold"))
dev.off()




eta3$nBaseline <- eta3$nE0
tiff("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_5C.tiff",width=16,height=11,unit="cm",res=300)
gpairs( eta3[,c("nBaseline","nTeq","nEC50")], 
        upper.pars = list(conditional = 'boxplot', scatter = 'loess'),
        lower.pars = list(scatter = 'stats',conditional = "barcode"),
        diag.pars = list(fontsize = 9, show.hist = TRUE, hist.color = "gray"),
        stat.pars =list(fontsize = 12, signif = 0.05, verbose = FALSE, use.color = TRUE, missing = 'missing', just = 'centre'),
        scatter.pars = list(pch = 20),outer.margins = c(2,2,3.5,2))
grid.text('(C) Correlations, histogram of EBE (Misspecified delay, Effect compartment)', x=.5, y=.95,gp=gpar(fontsize=12,fontface="bold"))
dev.off()


eta4$nBaseline <- eta4$nE0
tiff("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_6C.tiff",width=16,height=11,unit="cm",res=300)
gpairs( eta4[,c("nBaseline","nTeq","nEC50")], 
        upper.pars = list(conditional = 'boxplot', scatter = 'loess'),
        lower.pars = list(scatter = 'stats',conditional = "barcode"),
        diag.pars = list(fontsize = 9, show.hist = TRUE, hist.color = "gray"),
        stat.pars =list(fontsize = 12, signif = 0.05, verbose = FALSE, use.color = TRUE, missing = 'missing', just = 'centre'),
        scatter.pars = list(pch = 20),outer.margins = c(2,2,3.5,2))
grid.text('(C) Correlations, histogram of EBE (Misspecified delay and correlation)', x=.5, y=.95,gp=gpar(fontsize=12,fontface="bold"))
dev.off()




## we still need an augmented gof where we simulate ipred and pred at a grid of time points



#unique(gof$ModelName)

setwd("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/")
#[1] d.True model\n Turnover PD                                           
#[2] a.Misspecified delay\n Immediate effect                              
#[3] b.Misspecified delay\n Effect compartment                            
#[4] c.Misspecified delay and correlation\n Effect compartment, Full Omega
obs <- read.csv("obs.csv")
obs <- obs[obs$ObsName=="EObs",]
obs <- obs[obs$ModelName=="d.True model\n Turnover PD",]
gof <- read.csv("gof.csv")
gof <- gof[gof$ObsName=="EObs",]
gof$PPRED <- NA
gof$PWRES <- NA

sim <- read.csv("ALLPCDATA1.csv")
matsim <- matrix(sim$DV,ncol=1000)
PPRED <- apply(matsim,1,mean)
PPRED <- cbind(sim[1:232,],PPRED)
PPRED$DV <- obs$DV
PWRES <-c()
for (i in unique(PPRED$ID)) {
  iwhich <- which(PPRED$ID==i)
  idata <- PPRED[iwhich,]
  iRES <- idata$DV-idata$PPRED
  imatsim <- matsim[iwhich,]
  ivar <- var(t(imatsim))
  xmat <- try(chol(ivar)) 
  if (is.numeric(xmat)){
    ymat <- try(solve(xmat))
    ipwres <- t(ymat)%*%iRES}
  PWRES <- c(PWRES,ipwres)
  }
gof$PPRED[gof$ModelName == "d.True model\n Turnover PD"] <- PPRED$PPRED
gof$PWRES[gof$ModelName == "d.True model\n Turnover PD"] <- PWRES


sim <- read.csv("ALLPCDATA2.csv")
matsim <- matrix(sim$DV,ncol=1000)
PPRED <- apply(matsim,1,mean)
PPRED <- cbind(sim[1:232,],PPRED)
PPRED$DV <- obs$DV
PWRES <-c()
for (i in unique(PPRED$ID)) {
  iwhich <- which(PPRED$ID==i)
  idata <- PPRED[iwhich,]
  iRES <- idata$DV-idata$PPRED
  imatsim <- matsim[iwhich,]
  ivar <- var(t(imatsim))
  xmat <- try(chol(ivar)) 
  if (is.numeric(xmat)){
    ymat <- try(solve(xmat))
    ipwres <- t(ymat)%*%iRES}
  PWRES <- c(PWRES,ipwres)
}
gof$PPRED[gof$ModelName == "a.Misspecified delay\n Immediate effect"] <- PPRED$PPRED
gof$PWRES[gof$ModelName == "a.Misspecified delay\n Immediate effect"] <- PWRES


sim <- read.csv("ALLPCDATA3.csv")
matsim <- matrix(sim$DV,ncol=1000)
PPRED <- apply(matsim,1,mean)
PPRED <- cbind(sim[1:232,],PPRED)
PPRED$DV <- obs$DV
PWRES <-c()
for (i in unique(PPRED$ID)) {
  iwhich <- which(PPRED$ID==i)
  idata <- PPRED[iwhich,]
  iRES <- idata$DV-idata$PPRED
  imatsim <- matsim[iwhich,]
  ivar <- var(t(imatsim))
  xmat <- try(chol(ivar)) 
  if (is.numeric(xmat)){
    ymat <- try(solve(xmat))
    ipwres <- t(ymat)%*%iRES}
  PWRES <- c(PWRES,ipwres)
}
gof$PPRED[gof$ModelName == "b.Misspecified delay\n Effect compartment"] <- PPRED$PPRED
gof$PWRES[gof$ModelName == "b.Misspecified delay\n Effect compartment"] <- PWRES



sim <- read.csv("ALLPCDATA4.csv")
matsim <- matrix(sim$DV,ncol=1000)
PPRED <- apply(matsim,1,mean)
PPRED <- cbind(sim[1:232,],PPRED)
PPRED$DV <- obs$DV
PWRES <-c()
for (i in unique(PPRED$ID)) {
  iwhich <- which(PPRED$ID==i)
  idata <- PPRED[iwhich,]
  iRES <- idata$DV-idata$PPRED
  imatsim <- matsim[iwhich,]
  ivar <- var(t(imatsim))
  xmat <- try(chol(ivar)) 
  if (is.numeric(xmat)){
    ymat <- try(solve(xmat))
    ipwres <- t(ymat)%*%iRES}
  PWRES <- c(PWRES,ipwres)
}
gof$PPRED[gof$ModelName == "c.Misspecified delay and correlation\n Effect compartment, Full Omega"] <- PPRED$PPRED
gof$PWRES[gof$ModelName == "c.Misspecified delay and correlation\n Effect compartment, Full Omega"] <- PWRES

gof <- gof[,c(1:19,21:22,20)]
write.table(gof,"gof_ppred_pwres.csv",row.names=F,col.names = T,quote=T,sep=",")



gof1 <- read.csv("gof_ppred_pwres.csv")



#ModelName="All"
basiggofplot1 <- function (gofdata=gof1,ModelName="All", ...)
{
  # gofdata<- gof
  # ModelName="All"
  ModelName=ModelName
  
  if(ModelName=="All") {
    gofplot <- gofdata[gofdata$ObsName!="CObs",]
    
  }
  # ModelName="d.True model\n Turnover PD"
  
  if(ModelName!="All") {
    gofplot <- gofdata[gofdata$ObsName!="CObs"&
                         is.element(gofdata$ModelName,ModelName ),]
    
  }
  
  
  fig1a <- ggplot(gofplot,aes(PPRED,DV))+
    geom_point(color="blue")+
    geom_abline(intercept=0,slope=1,size=1)+
    stat_plsmo(color="red",size=1.5,linetype="dashed")+
    ylab("OBS")+ xlab("PPRED")+
    scale_y_continuous(breaks=seq(0,120,20))+scale_x_continuous(breaks=seq(0,120,20))+
    theme(plot.title = element_text(size=16, face="bold"))+
    theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
    theme(legend.position="nonne",axis.text=element_text(size=12),
          axis.title=element_text(size=14))+
    theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
    theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
    theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)
  
  if(ModelName=="All")
    fig1a <- fig1a+
    facet_grid (~ModelName)+
    ggtitle("(A)")
  
  fig1d   <-  ggplot(gofplot ,aes(IPRED,DV))+
    geom_point(color="blue")+
    geom_abline(intercept=0,slope=1,size=1)+
    stat_plsmo(color="red",size=1.5,linetype="dashed")+
    ylab("OBS")+ xlab("IPRED")+
    scale_y_continuous(breaks=seq(0,120,20))+scale_x_continuous(breaks=seq(0,120,20))+
    theme(plot.title = element_text(size=16, face="bold"))+
    theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
    theme(legend.position="nonne",axis.text=element_text(size=12),
          axis.title=element_text(size=14))+
    theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
    theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
    theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1) 
  
  if(ModelName=="All")
    fig1d <- fig1d+
    facet_grid (~ModelName)+
    ggtitle("  ")
  
  fig1 <- arrangeGrob(fig1a,fig1d,ncol=1)
  grid.draw(fig1)
  return(fig1)
}

basiggofplot2 <- function (gofdata=gof1,ModelName="All", ...)
{
  
  ModelName=ModelName
  
  if(ModelName=="All") {
    gofplot <- gofdata[gofdata$ObsName!="CObs",]
    
  }
  if(ModelName!="All") {
    gofplot <- gofdata[gofdata$ObsName!="CObs"&
                         is.element(gofdata$ModelName,ModelName ),]
    
  }
  
  
  
  fig1c   <- ggplot(gofplot ,aes(IVAR,PWRES))+ 
    geom_point(color="blue")+
    geom_abline(intercept=0,slope=0,size=1)+
    stat_plsmo(color="red",size=1.5,linetype="dashed")+
    ylab("PWRES")+ xlab("Time")+
    scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,144,24))+
    
    theme(plot.title = element_text(size=16, face="bold"))+
    theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
    theme(legend.position="nonne",axis.text=element_text(size=12),
          axis.title=element_text(size=14))+
    theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
    theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
    theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)
  
  if(ModelName=="All")
    fig1c <- fig1c+
    facet_grid (~ModelName)+
    ggtitle("(B)")
  
  
  
  fig1f <- ggplot(gofplot,aes(IVAR,IWRES))+
    geom_point(color="blue")+
    geom_abline(intercept=0,slope=0,size=1)+
    stat_plsmo(color="red",size=1.5,linetype="dashed")+
    ylab("IWRES")+ xlab("Time")+
    scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,144,24))+
    theme(plot.title = element_text(size=16, face="bold"))+
    theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
    theme(legend.position="nonne",axis.text=element_text(size=12),
          axis.title=element_text(size=14))+
    theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
    theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
    theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)
  
  if(ModelName=="All")
    fig1f <- fig1f+
    facet_grid (~ModelName)+
    ggtitle(" ")
  fig1 <- arrangeGrob(fig1c,fig1f,ncol=1)
  grid.draw(fig1)
  return(fig1)
}


basiggofplot3 <- function (gofdata=gof1,ModelName="All",logx=FALSE, ...)
{
  
  ModelName=ModelName
  
  if(ModelName=="All") {
    gofplot <- gofdata[gofdata$ObsName!="CObs",]
    
  }
  if(ModelName!="All") {
    gofplot <- gofdata[gofdata$ObsName!="CObs"&
                         is.element(gofdata$ModelName,ModelName ),]
    
  }
  
  
  
  
  fig1b <-  ggplot(gofplot ,aes(PPRED,PWRES))+
    geom_point(color="blue")
  if(!logx)  {
    fig1b <-fig1b+
      geom_abline(intercept=0,slope=0,size=1)
  }
  if(logx)  {
    fig1b <-fig1b+
      geom_abline(intercept=0,slope=0,size=1)
  }
  
  fig1b <- fig1b+
    stat_plsmo(color="red",size=1.5,linetype="dashed")+
    ylab("PWRES")+ xlab("PPRED")+
    scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,120,20))+
    theme(plot.title = element_text(size=16, face="bold"))+
    theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
    theme(legend.position="nonne",axis.text=element_text(size=12),
          axis.title=element_text(size=14))+
    theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
    theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
    theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)
  if(ModelName=="All")
    fig1b <- fig1b+
    facet_grid (~ModelName)+
    ggtitle("(C)")
  
  if(logx)  {
    fig1b <-fig1b+
      scale_x_log10()+
      xlab("log(PPRED)")
  }
  
  fig1e   <-  ggplot(gofplot ,aes(IPRED,IWRES))+
    geom_point(color="blue")
  if(!logx)  {
    fig1e <-fig1e+
      geom_abline(intercept=0,slope=0,size=1)
  }
  if(logx)  {
    fig1e <-fig1e+
      geom_abline(intercept=0,slope=0,size=1)
  }
  
  fig1e <- fig1e+
    stat_plsmo(color="red",size=1.5,linetype="dashed")+
    ylab("IWRES")+ xlab("IPRED")+
    scale_y_continuous(breaks=seq(-5,5,1))+scale_x_continuous(breaks=seq(0,100,20))+
    theme(plot.title = element_text(size=16, face="bold"))+
    theme(panel.background= element_rect(fill = 'white', colour = 'black'))+
    theme(legend.position="nonne",axis.text=element_text(size=12),
          axis.title=element_text(size=14))+
    theme(panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))+
    theme(panel.grid.major= element_line(colour = "gray", linetype = "solid"))+
    theme(strip.text = element_text(size = 12, colour = "black"),aspect.ratio=1)
  
  if(logx)  {
    fig1e <-fig1e+
      scale_x_log10()+
      xlab("log(IPRED)")
  }
  
  if(ModelName=="All")
    fig1e <- fig1e+
    facet_grid (~ModelName)+
    ggtitle("  ")
  
  
  fig1 <- arrangeGrob(fig1b,fig1e,ncol=1)
  grid.draw(fig1)
  return(fig1)
}

