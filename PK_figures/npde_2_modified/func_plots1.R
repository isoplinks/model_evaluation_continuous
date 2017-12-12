##################################################################################
#' Select plot for a NpdeObject object
#' 
#' Select plot for a NpdeObject object
#' 
#' @usage npde.plot.select(npdeObject,data=FALSE,ecdf=FALSE,qqplot=FALSE, histogram=FALSE,x.scatter=FALSE,pred.scatter=FALSE,x.box=FALSE,pred.box=FALSE, cov.x.scatter=FALSE,cov.pred.scatter=FALSE,cov.x.box=FALSE,cov.pred.box=FALSE, cov.ecdf=FALSE, vpc=FALSE,...)
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param data boolean, whether to produce a plot of the data
#' @param ecdf boolean, whether to produce a distribution plot of the empirical distribution function
#' @param qqplot boolean, whether to produce a QQ-plot of the empirical distribution function
#' @param histogram boolean, whether to produce a histogram of the metric
#' @param x.scatter boolean, whether to produce a scatterplot of the metric as a function of X
#' @param pred.scatter boolean, whether to produce a scatterplot of the metric as a function of predictions
#' @param x.box boolean, whether to produce whisker plots of the metric as a function of X
#' @param pred.box boolean, whether to produce whisker plots of the metric as a function of predictions
#' @param cov.x.scatter boolean, whether to produce a scatterplot of the metric as a function of X, split by covariate(s)
#' @param cov.pred.scatter boolean, whether to produce a scatterplot of the metric as a function of predictions, split by covariate(s)
#' @param cov.x.box boolean, whether to produce whisker plots of the metric as a function of X, split by covariate(s)
#' @param cov.pred.box boolean, whether to produce whisker plots of the metric as a function of predictions, split by covariate(s)
#' @param cov.ecdf boolean, whether to produce a distribution plot of the empirical distribution function, split by covariate(s)
#' @param vpc boolean, whether to produce a VPC
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}, \code{\link{set.plotoptions}}
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @keywords plot internal

npde.plot.select<-function(npdeObject,data=FALSE,ecdf=FALSE,qqplot=FALSE, histogram=FALSE,x.scatter=FALSE,pred.scatter=FALSE,x.box=FALSE,pred.box=FALSE, cov.x.scatter=FALSE,cov.pred.scatter=FALSE,cov.x.box=FALSE,cov.pred.box=FALSE, cov.ecdf=FALSE, vpc=FALSE,...) {
  # Function selecting which plots are to be drawn
  namObj<-deparse(substitute(npdeObject))
  interactive<-npdeObject["prefs"]$interactive
  # ECO TODO: replace with partial matching
  if(data) plot(npdeObject,plot.type="data",...)
  if(ecdf) plot(npdeObject,plot.type="ecdf",...)
  if(qqplot) plot(npdeObject,plot.type="qqplot",...)
  if(histogram) plot(npdeObject,plot.type="histogram",...)
  if(x.scatter) plot(npdeObject,plot.type="x.scatter",...)
  if(pred.box) plot(npdeObject,plot.type="pred.scatter",box=TRUE,...)
  if(x.box) plot(npdeObject,plot.type="x.scatter",box=TRUE,...)
  if(pred.scatter) plot(npdeObject,plot.type="pred.scatter",...)
  if(cov.x.scatter) plot(npdeObject,plot.type="cov.x.scatter",...)
  if(cov.pred.scatter) plot(npdeObject,plot.type="cov.pred.scatter",...)
  if(cov.ecdf) plot(npdeObject,plot.type="cov.ecdf",...)
  if(cov.x.box) plot(npdeObject,plot.type="cov.x.scatter",box=TRUE,...)
  if(cov.pred.box) plot(npdeObject,plot.type="cov.pred.scatter",box=TRUE,...)
  if(vpc) plot(npdeObject,plot.type="vpc",...)
}

#' Default plots for a NpdeObject object
#' 
#' Default plots for a NpdeObject object
#' 
#' @usage default.npde.plots(npdeObject, ...)
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' @keywords plot internal

#### Meta-niveau
default.npde.plots<-function(npdeObject,...) {
  # When plot(npdeObject) is called without plot.type  
  par(mfrow=c(2,2),ask=npdeObject["prefs"]$ask)
  npde.plot.select(npdeObject,qqplot=TRUE,histogram=TRUE, x.scatter=TRUE,pred.scatter=TRUE,new=FALSE,...)
}

#' Covariate plots for a NpdeObject object
#' 
#' Covariate plots for a NpdeObject object
#' 
#' @usage npde.plot.covariates(npdeObject, which="x", ...)
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param which one of "x" (scatterplots of the metric versus X), "pred" (scatterplots of the metric versus predictions) or "ecdf" (empirical distribution function)
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' @keywords plot internal

npde.plot.covariates<-function(npdeObject,which="x",...) {
  # Parameters or random effects versus covariates
  if(which=="x") {
    plot(npdeObject,plot.type="cov.x.scatter",...)
  }
  if(which=="pred") {
    plot(npdeObject,plot.type="cov.pred.scatter",...)
  }
  if(which=="ecdf") {
    plot(npdeObject,plot.type="cov.ecdf",...)
  }
}

#' Plots for pd and npde
#' 
#' Plots for pd and npde
#' 
#' @aliases npde.plot.pd npde.plot.npde
#' @usage npde.plot.pd(npdeObject, ...)
#' @usage npde.plot.npde(npdeObject, ...)
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' @keywords plot internal

npde.plot.pd<-function(npdeObject,...) {
  # Advanced goodness of fit plots
  if(npdeObject@options$verbose) cat("Plots for pd\n")
  default.npde.plots(npdeObject,which="pd",...)
}

npde.plot.npde<-function(npdeObject,...) {
  # Advanced goodness of fit plots
  if(npdeObject@options$verbose) cat("Plots for npde\n")
  default.npde.plots(npdeObject,...)
}

################################    Data    #####################################


#' Plot functions for NpdeObjects
#' 
#' Workhorse functions for plots (internal)
#' 
#' @aliases npde.plot.data npde.plot.dist npde.plot.scatter npde.plot.vpc npde.plot.loq aux.scatter.box aux.scatter compute.bands.true compute.bands aux.npdeplot.meanprof aux.npdeplot.computepi aux.npdeplot.transform aux.npdeplot.plot aux.npdeplot.main npde.plot.meanprofile
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' @keywords plot internal

npde.plot.data<-function(npdeObject,...) {
  if(length(npdeObject["data"]["icens"])>0) {
    has.cens<-TRUE
    icens<-npdeObject["data"]["icens"]
    is.cens<-npdeObject["data"]["data"]$cens==1
  } else has.cens<-FALSE
  plot.opt<-npdeObject["prefs"]
  plot.opt$xlab<-paste(npdeObject["data"]["name.predictor"]," (",npdeObject["data"]["units"]$x,")",sep="")
  plot.opt$ylab<-paste(npdeObject["data"]["name.response"]," (",npdeObject["data"]["units"]$y,")",sep="")
  plot.opt<-replace.plotoptions(plot.opt,...)
  if(plot.opt$new) {
    mfrow<-plot.opt$mfrow
    if(length(mfrow)==0) mfrow<-c(1,1)
    par(mfrow=mfrow,ask=plot.opt$ask)
  }
  logtyp<-""
  if(plot.opt$xlog) logtyp<-paste(logtyp,"x",sep="")
  if(plot.opt$ylog) logtyp<-paste(logtyp,"y",sep="")
  plot.type<-plot.opt$type
  x<-npdeObject["data"]["data"][,npdeObject["data"]["name.predictor"]]
  y<-npdeObject["data"]["data"][,npdeObject["data"]["name.response"]]
  id<-(npdeObject["data"]["data"]$index %in% plot.opt$ilist)
  if(plot.opt$impute.loq & length(npdeObject["results"]["res"]$ycomp)>0 & npdeObject["options"]["cens.method"]!="omit")
    y<-npdeObject["results"]["res"]$ycomp
  if(is.null(plot.opt$xlim) & !plot.opt$xlog) plot.opt$xlim<-c(min(x,na.rm=T), max(x,na.rm=T))
  if(is.null(plot.opt$ylim) & !plot.opt$ylog) plot.opt$ylim<-c(min(y,na.rm=T), max(y,na.rm=T))
  if(plot.type=="p" | plot.type=="b") {
    if(has.cens) {
      plot(x[id],y[id],xlab=plot.opt$xlab,ylab=plot.opt$ylab,main=plot.opt$main,sub=plot.opt$sub, col=plot.opt$col,pch=plot.opt$pch,log=logtyp,xlim=plot.opt$xlim,ylim=plot.opt$ylim, cex=plot.opt$cex,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main, cex.lab=plot.opt$cex.lab,type="n", xaxt=plot.opt$xaxt, yaxt=plot.opt$yaxt,axes=plot.opt$axes,frame.plot=plot.opt$frame.plot) 
      points(x[id & !is.cens],y[id & !is.cens],col=plot.opt$col.pobs,pch=plot.opt$pch.pobs, cex=plot.opt$cex) 
      if(plot.opt$plot.loq) points(x[id & is.cens],y[id & is.cens],col=plot.opt$col.pcens,pch=plot.opt$pch.pcens, cex=plot.opt$cex) 
    } else plot(x[id],y[id],xlab=plot.opt$xlab,ylab=plot.opt$ylab,log=logtyp, xlim=plot.opt$xlim,ylim=plot.opt$ylim,col=plot.opt$col.pobs, pch=plot.opt$pch, main=plot.opt$main,sub=plot.opt$sub,cex=plot.opt$cex,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main, cex.lab=plot.opt$cex.lab,xaxt=plot.opt$xaxt, yaxt=plot.opt$yaxt,axes=plot.opt$axes,frame.plot=plot.opt$frame.plot) 
  }
  if(plot.type=="l") {
    plot(x[id],y[id],xlab=plot.opt$xlab,ylab=plot.opt$ylab,log=logtyp,xlim=plot.opt$xlim, ylim=plot.opt$ylim,main=plot.opt$main,sub=plot.opt$sub, col=plot.opt$col,lty=plot.opt$lty,lwd=plot.opt$lwd,type="n",  cex=plot.opt$cex,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main, cex.lab=plot.opt$cex.lab,xaxt=plot.opt$xaxt, yaxt=plot.opt$yaxt,axes=plot.opt$axes,frame.plot=plot.opt$frame.plot)
  }
  if(plot.type=="l" | plot.type=="b") {
    for(isuj in unique(plot.opt$ilist)) {
      if(has.cens & !plot.opt$plot.loq) idx<-which(npdeObject["data"]["data"]$index==isuj & !is.cens) else idx<-which(npdeObject["data"]["data"]$index==isuj)
      lines(x[idx], y[idx],col=plot.opt$col.lobs,lty=plot.opt$lty.lobs,lwd=plot.opt$lwd.lobs)
    }
    # ECO: uncomment here if we want the LOQ points to appear even on the plot with only the lines
    #       if(has.cens & plot.opt$plot.loq) {
    #         points(x[id & is.cens],y[id & is.cens],col=plot.opt$col.pcens,pch=plot.opt$pch.pcens,cex=plot.opt$cex) 
    #       }    
  }
  if(plot.opt$line.loq & length(npdeObject["data"]["loq"])>0) abline(h=npdeObject["data"]["loq"],lty=plot.opt$lty.abline, col=plot.opt$col.abline, lwd=plot.opt$lwd.abline)
}

#################    	     Distributions - npde/pd          ###################

npde.plot.dist<-function(npdeObject,which="npde",dist.type="ecdf",covsplit=FALSE, ...) {
  plot.opt.defaults<-function(plot.opt,which="npde",dist.type="ecdf") {
    plot.opt$new<-TRUE
    plot.opt$main<-""
    plot.opt$ask<-FALSE
    if(dist.type=="ecdf") {
      plot.opt$ylab<-"Empirical cdf"
      plot.opt$ylim<-c(0,1)
      plot.opt$xlab<-switch(which,pd="Sample quantiles (pd)",npd="Sample quantiles (npd)",npde="Sample quantiles (npde)")
    }
    if(dist.type=="qqplot") {
      plot.opt$ylab<-"Theoretical Quantiles"
      plot.opt$xlab<-switch(which,pd="Sample quantiles (pd)",npd="Sample quantiles (npd)",npde="Sample quantiles (npde)")
      if(which %in% c("npd","npde")) {
        plot.opt$main<-paste("Q-Q plot versus N(0,1) for ",which)
      } else {
        plot.opt$main<-"Q-Q plot versus U(0,1) for pd"
      }
    }
    if(dist.type=="hist") {
      plot.opt$ylab<-"Frequency"
      plot.opt$xlab<-switch(which,pd="Sample quantiles (pd)",npd="Sample quantiles (npd)",npde="Sample quantiles (npde)")
      plot.opt$xlim<-NULL
      plot.opt$ylim<-NULL
    }	
    return(plot.opt)
  }
  args1<-match.call(expand.dots=TRUE)
  i1<-match("main",names(args1))
  if(!is.na(i1)) {
    change.main<-TRUE
  } else change.main<-FALSE
  i1<-match("ncat",names(args1))
  if(!is.na(i1)) {
    change.ncat<-TRUE
  } else change.ncat<-FALSE
  
  if(match(which,c("npde","pd","npd"),nomatch=0)==0) {
    cat("Option which=",which,"not recognised\n")
    return()
  }
  if(match(dist.type,c("ecdf","qqplot","hist"),nomatch=0)==0) {
    cat("Option dist.type=",dist.type,"not recognised\n")
    return()
  }
  if(which=="npde" & length(npdeObject["results"]["res"]$npde)==0)  {
    cat("    Missing npde object to plot.\n")
    return()
  }
  if(which %in% c("pd","npd") & length(npdeObject["results"]["res"]$pd)==0) {
    cat("    Missing pd object to plot.\n")
    return()
  }
  if(which=="pd") distrib<-"unif" else distrib<-"norm"
  if(length(npdeObject["data"]["icens"])>0) has.cens<-TRUE else has.cens<-FALSE
  plot.opt<-npdeObject["prefs"]
  plot.opt<-plot.opt.defaults(plot.opt,which=which,dist.type=dist.type)
  plot.opt<-replace.plotoptions(plot.opt,...)
  if(covsplit & length(npdeObject["data"]["name.covariates"])==0) {
    if(npdeObject@options$verbose) cat("No covariates in the dataset\n")
    covsplit<-FALSE
  }
  if(covsplit) {
    if(plot.opt$which.cov=="") plot.opt$which.cov<-"all"
    if(is.numeric(plot.opt$which.cov)) plot.opt$which.cov<-npdeObject["data"]["name.covariates"][plot.opt$which.cov]
    if(plot.opt$which.cov=="all") lcov<-npdeObject["data"]["name.covariates"] else {
      icov<-match(plot.opt$which.cov,npdeObject["data"]["name.covariates"])
      lcov<-npdeObject["data"]["name.covariates"][icov]
    }
    ncov<-length(lcov)
    if(ncov==0) {
      if(npdeObject@options$verbose) cat("Cannot find covariate(s)", plot.opt$which.cov,"in the dataset\n")
      covsplit<-FALSE
    }
  } else ncov<-0
  if(!covsplit & plot.opt$new) {
    mfrow<-plot.opt$mfrow
    if(length(mfrow)==0) mfrow=c(1,1)
    par(mfrow=mfrow,ask=plot.opt$ask)
  }
  logtyp<-""
  if(plot.opt$xlog) logtyp<-paste(logtyp,"x",sep="")
  if(plot.opt$ylog) logtyp<-paste(logtyp,"y",sep="")
  plot.opt$logtyp<-logtyp
  sim.ypl<-NULL
  if(!plot.opt$approx.pi) {
    if(which %in% c("pd","npd")) {
      if(length(npdeObject["results"]["pd.sim"])==0) {
        cat("You have requested to use simulations to provide the prediction intervals, but the simulated pd are not present.\n")
        plot.opt$approx.pi<-TRUE
      } else sim.ypl<-npdeObject["results"]["pd.sim"]
    }
    if(which=="npde") {
      if(length(npdeObject["results"]["npde.sim"])==0) {
        cat("You have requested to use simulations to provide the prediction intervals, but the simulated npde are not present.\n")
        plot.opt$approx.pi<-TRUE
      } else sim.ypl<-npdeObject["results"]["npde.sim"]
    }
  }
  if(which=="npde") ypl<-npdeObject["results"]["res"]$npde
  if(which %in% c("pd","npd")) ypl<-npdeObject["results"]["res"]$pd    
  if(which=="npd") ypl<-qnorm(ypl)
  if(has.cens) plmat<-data.frame(xpd=ypl, cens=npdeObject["data"]["data"][,npdeObject["data"]["name.cens"]]) else plmat<-data.frame(xpd=ypl,cens=rep(0,length(ypl)))
  keep<-npdeObject["data"]["not.miss"]
  if(npdeObject["options"]["cens.method"]=="omit" & has.cens) keep<-(npdeObject["data"]["not.miss"] & npdeObject["data"]["data"][,npdeObject["data"]["name.cens"]]==0)
  idobs<-npdeObject["data"]["data"][,npdeObject["data"]["name.group"]]
  idobs<-idobs[keep]
  plmat<-plmat[keep,]
  
  if(covsplit) {
    for(icov in 1:ncov) {
      namcov<-lcov[icov]
      zecov<-npdeObject["data"]["data"][keep,namcov]
      idobs<-npdeObject["data"]["data"][keep,npdeObject["data"]["name.group"]]
      ucov<-zecov[match(unique(idobs),idobs)]
      if(!is.numeric(ucov) & length(unique(ucov))<=4) {
        covcont<-FALSE
        ncat<-length(unique(ucov))
        namcat<-paste(namcov,sort(unique(zecov)),sep="=")
      } else {
        covcont<-TRUE
        if(change.ncat | plot.opt$ncat!=3) {
          ncat<-plot.opt$ncat
          seqcat<-seq(0,1,length.out=(ncat+1))
          zecov<-cut(zecov,breaks=quantile(ucov,seqcat), include.lowest=TRUE, ordered_result=TRUE)
          nam1<-paste("q",format(seqcat[-(ncat+1)],digits=2),"-q",format(seqcat[-1],digits=2),sep="")
          namcat<-paste(namcov,nam1,sep=": ")
        } else {
          zecov<-cut(zecov,breaks=quantile(ucov,c(0,0.25,0.75,1)), include.lowest=TRUE, ordered_result=TRUE)
          ncat<-3
          namcat<-paste(namcov,c("<Q1","Q1-Q3",">Q3"),sep=": ")
        }
      }
      if(plot.opt$new) {
        mfrow<-plot.opt$mfrow
        if(length(mfrow)==0) {
          if(ncat<=3) mfrow=c(1,ncat) else {
            n1<-round(sqrt(ncat))
            n2<-ceiling(ncat/n1)
            mfrow<-c(n1,n2)
          }
        }
        par(mfrow=mfrow,ask=plot.opt$ask)
      }
      zecat<-sort(unique(zecov))
      plmat1<-data.frame(plmat,cov=zecov)
      for(ic in 1:length(zecat)) {
        if(!change.main) plot.opt$main<-namcat[ic]
        if(dist.type=="hist") 
          aux.plot.hist(plmat1[plmat1$cov==zecat[ic],], plot.opt,distrib=distrib, nclass=plot.opt$vpc.bin, sim.ypl=sim.ypl[plmat1$cov==zecat[ic],]) else
            aux.plot.dist(plmat1[plmat1$cov==zecat[ic],],plot.opt, dist.type=dist.type,distrib=distrib,nrep=npdeObject["sim.data"]["nrep"], ties=npdeObject["options"]$ties, sim.ypl=sim.ypl[plmat1$cov==zecat[ic],])
      }
    }
  } else {
    if(dist.type=="hist") aux.plot.hist(plmat,plot.opt,distrib=distrib, nclass=plot.opt$vpc.bin,sim.ypl=sim.ypl) else aux.plot.dist(plmat,plot.opt, dist.type=dist.type,distrib=distrib,nrep=npdeObject["sim.data"]["nrep"], ties=npdeObject["options"]$ties, sim.ypl=sim.ypl)
  }
}

#################    	     Scatterplots - npde/pd           ###################

npde.plot.scatter<-function(npdeObject,which="npde",xaxis="x",covsplit=FALSE, ...) {
  plot.opt.defaults<-function(plot.opt,which="npde") {
    plot.opt$new<-TRUE
    plot.opt$xlab<-switch(xaxis, x=paste(npdeObject["data"]["name.predictor"]," (", npdeObject["data"]["units"]$x,")",sep=""), pred=paste("Predicted ", npdeObject["data"]["name.response"]," (", npdeObject["data"]["units"]$y,")",sep=""), cov="")
    plot.opt$ylab<-switch(which,pd="pd",npd="npd",npde="npde")
    plot.opt$main<-""	
    return(plot.opt)
  }
  
  if(match(which,c("npde","pd","npd"),nomatch=0)==0) {
    cat("Option which=",which,"not recognised\n")
    return()
  }
  if(xaxis=="cov" & length(npdeObject["data"]["name.covariates"])==0) {
    cat("Option xaxis set to 'cov' but no covariate in dataset\n")
    return()
  }
  if(which=="npde" & length(npdeObject["results"]["res"]$npde)==0) {
    cat("    Missing npde object to plot.\n")
    return()
  }
  if(which %in% c("pd","npd") & length(npdeObject["results"]["res"]$pd)==0)  {
    cat("    Missing pd object to plot.\n")
    return()
  }    
  if(length(npdeObject["data"]["icens"])>0) has.cens<-TRUE else has.cens<-FALSE
  args1<-match.call(expand.dots=TRUE)
  i1<-match("xlab",names(args1))
  if(!is.na(i1)) {
    change.xlab<-TRUE
  } else change.xlab<-FALSE
  i1<-match("ncat",names(args1))
  if(!is.na(i1)) {
    change.ncat<-TRUE
  } else change.ncat<-FALSE
  plot.opt<-npdeObject["prefs"]
  plot.opt<-plot.opt.defaults(plot.opt,which=which)
  plot.opt<-replace.plotoptions(plot.opt,...)
  ask<-plot.opt$ask
  
  if(!covsplit & plot.opt$new) {
    mfrow<-plot.opt$mfrow
    if(length(mfrow)==0) mfrow=c(1,1)
    par(mfrow=mfrow,ask=ask)
  }
  if(xaxis=="cov") {
    # 		plot.opt$box<-TRUE
    if(!covsplit & npdeObject@options$verbose) cat("    graph versus covariates requested, setting covsplit to TRUE\n")
    covsplit<-TRUE
  }
  if(covsplit & length(npdeObject["data"]["name.covariates"])==0) {
    if(npdeObject@options$verbose) cat("No covariates in the dataset\n")
    covsplit<-FALSE
  }
  if(which=="pd") plot.opt$ylim<-c(0,1) else plot.opt$ylim<-NULL
  if(which=="pd") distrib<-"unif" else distrib<-"norm"
  if(covsplit) {
    if(plot.opt$which.cov=="") plot.opt$which.cov<-"all"
    if(is.numeric(plot.opt$which.cov)) plot.opt$which.cov<-npdeObject["data"]["name.covariates"][plot.opt$which.cov]
    if(plot.opt$which.cov=="all") {
      lcov<-npdeObject["data"]["name.covariates"]
      lunit<-npdeObject["data"]["units"]$covariates
    } else {
      icov<-match(plot.opt$which.cov,npdeObject["data"]["name.covariates"])
      lcov<-npdeObject["data"]["name.covariates"][icov]
      lunit<-npdeObject["data"]["units"]$covariates[icov]
    }
    ncov<-length(lcov)
    if(ncov==0) {
      cat("Cannot find covariate",plot.opt$which.cov,"in the dataset\n")
      covsplit<-FALSE
    }
  } else ncov<-0
  if(xaxis=="cov" & length(plot.opt$mfrow)==0 & plot.opt$new) {
    n1<-round(sqrt(ncov))
    n2<-ceiling(ncov/n1)
    plot.opt$mfrow<-c(n1,n2)
    par(mfrow=plot.opt$mfrow,ask=ask)
  } 
  logtyp<-""
  if(plot.opt$xlog) logtyp<-paste(logtyp,"x",sep="")
  if(plot.opt$ylog) logtyp<-paste(logtyp,"y",sep="")
  plot.opt$logtyp<-logtyp
  sim.ypl<-NULL
  if(!plot.opt$approx.pi) {
    if(which %in% c("pd","npd")) {
      if(length(npdeObject["results"]["pd.sim"])==0) {
        cat("You have requested to use simulations to provide the prediction intervals, but the simulated pd are not present.\n")
        plot.opt$approx.pi<-TRUE
      } else sim.ypl<-npdeObject["results"]["pd.sim"]
    }
    if(which=="npde") {
      if(length(npdeObject["results"]["npde.sim"])==0) {
        cat("You have requested to use simulations to provide the prediction intervals, but the simulated npde are not present.\n")
        plot.opt$approx.pi<-TRUE
      } else sim.ypl<-npdeObject["results"]["npde.sim"]
    }
  }
  if(which=="npde") ypl<-npdeObject["results"]["res"]$npde
  if(which %in% c("pd","npd")) ypl<-npdeObject["results"]["res"]$pd
  if(which=="npd") {
    ypl<-qnorm(ypl)
    if(!plot.opt$approx.pi) sim.ypl<-qnorm(sim.ypl)
  }
  if(xaxis=="x") xpl<-npdeObject["data"]["data"][, npdeObject["data"]["name.predictor"]]
  idobs<-npdeObject["data"]["data"][npdeObject["data"]["not.miss"], npdeObject["data"]["name.group"]]
  if(xaxis=="cov") xpl<-rep(1,length(ypl))
  if(xaxis=="pred") xpl<-npdeObject["results"]["res"]$ypred
  if(has.cens) plmat<-matrix(c(xpl,npdeObject["data"]["data"][, npdeObject["data"]["name.cens"]],ypl),ncol=3) else plmat<-matrix(c(xpl,rep(0,length(xpl)),ypl),ncol=3)
  plmat<-plmat[npdeObject["data"]["not.miss"],]
  if(which %in% c("npd","npde")) {
    x1<-abs(qnorm((1-plot.opt$pi.size)/2))
    dotline<-c(-x1,0,x1)
  }
  if(which=="pd") dotline<-c((1-plot.opt$pi.size)/2,0.5,1-(1-plot.opt$pi.size)/2)
  if(covsplit) {
    # Same limits for the different plots
    if(is.null(plot.opt$xlim) & xaxis!="cov") plot.opt$xlim<-c(min(xpl,na.rm=T), max(xpl,na.rm=T))
    if(is.null(plot.opt$ylim) & xaxis!="cov") plot.opt$ylim<-c(min(ypl,na.rm=T), max(ypl,na.rm=T))
    for(icov in 1:ncov) {
      namcov<-lcov[icov]
      namunit<-lunit[icov]
      zecov<-npdeObject["data"]["data"][npdeObject["data"]["not.miss"],namcov]
      if(xaxis=="cov") plmat[,1]<-zecov
      ucov<-zecov[match(unique(idobs),idobs)]
      # ECO: SECURISER 
      if(!is.numeric(ucov)) {
        if(length(unique(ucov))>4) cat("Too many categories, the plot will not be informative.\n")
        plot.box<-TRUE
        covcont<-FALSE
        ncat<-length(unique(ucov))
        if(xaxis=="cov") namcat<-sort(unique(zecov)) else namcat<-paste(namcov,sort(unique(zecov)),sep="=")
      } else {
        plot.box<-plot.opt$box
        if(change.ncat | plot.opt$ncat!=3) {
          ncat<-plot.opt$ncat
          seqcat<-seq(0,1,length.out=(ncat+1))
          zecov<-cut(zecov,breaks=quantile(ucov,seqcat), include.lowest=TRUE, ordered_result=TRUE)
          nam1<-paste("q",format(seqcat[-(ncat+1)],digits=2),"-q",format(seqcat[-1],digits=2),sep="")
          namcat<-paste(namcov,nam1,sep=": ")
        } else {
          zecov<-cut(zecov,breaks=quantile(ucov,c(0,0.25,0.75,1)), include.lowest=TRUE, ordered_result=TRUE)
          ncat<-3
          namcat<-paste(namcov,c("<Q1","Q1-Q3",">Q3"),sep=": ")
        }
        covcont<-TRUE
      }
      if(plot.opt$new & xaxis!="cov") {
        mfrow<-plot.opt$mfrow
        if(length(mfrow)==0) {
          if(ncat<=3) mfrow=c(1,ncat) else {
            n1<-round(sqrt(ncat))
            n2<-ceiling(ncat/n1)
            mfrow<-c(n1,n2)
          }
        }
        par(mfrow=mfrow,ask=ask)
      }
      zecat<-sort(unique(zecov))
      plmat1<-data.frame(plmat,zecov)
      if(xaxis=="cov") {
        if(!change.xlab) {
          plot.opt$xlab<-paste(namcov," (",namunit,")",sep="")
          if(ncat<3) plot.opt$xlab<-""
        }
        if(plot.box) {
          plmat1<-plmat1[,c(4,2,3)]
          aux.scatter.box(plmat1,plot.opt,dotline,main=NULL,namcat=namcat)
        } else {
          aux.scatter(plmat,plot.opt,dotline,distrib=distrib,main=NULL, sim.ypl=sim.ypl)					
        }
      } else {
        for(ic in 1:length(zecat)) {
          if(plot.opt$box) 
            aux.scatter.box(plmat1[plmat1[,4]==zecat[ic],], plot.opt,dotline, main=namcat[ic]) else 
              aux.scatter(plmat1[plmat1[,4]==zecat[ic],],plot.opt,dotline, distrib=distrib,main=namcat[ic], sim.ypl=sim.ypl[plmat1[,4]==zecat[ic],])
        }
      }
    }
  } else {
    if(plot.opt$box) aux.scatter.box(plmat,plot.opt,dotline,main=NULL) else aux.scatter(plmat,plot.opt,dotline,distrib=distrib,main=NULL,sim.ypl=sim.ypl)
  }
}

######################	 Computational functions ################################

# Compute PI bands
compute.bands<-function(nsamp,nseuil=200,quant=c(0.025,0.5,0.975),distrib="norm", alpha=0.95) {
  # Compute a prediction interval around selected quantiles of the normal or uniform distribution, for different sizes of samples by randomly sampling from N(0,1) or U(0,1)
  ### nsamp: a vector giving the sizes of the samples
  ### size: alpha (defaults to a 95% PI)
  ### quantile: quant (defaults to 5th, 50th (median) and 95th percentiles)
  ### distribution: normal (default) or uniform
  # When the number of samples isamp is larger than 200, the PI is computed for n=200 and the size of the PI is then adjusted through sqrt(200/isamp)
  
  # Returns
  ### binf: lower bounds (as many columns as elements in quant) for the PI with level alpha
  ### bmed: median
  ### bsup: upper bounds
  #  msim<-10000
  msim<-1000 # number of replications used to compute the prediction interval
  idx1<-which(nsamp>=nseuil)
  quant.pi<-c((1-alpha)/2,0.5,1-(1-alpha)/2) 
  if(length(idx1)>0) {
    xsamp<-matrix(switch(distrib,norm=rnorm(msim*nseuil),unif=runif(msim*nseuil)), ncol=msim)
    mat<-apply(xsamp,2,quantile,quant)
    xseuil<-apply(mat,1,quantile,quant.pi)
    demi<-(xseuil[3,]-xseuil[1,])/2
  }
  binf<-bsup<-bmed<-matrix(nrow=length(nsamp),ncol=length(quant), dimnames=list(nsamp,quant))
  for(isamp in unique(nsamp)) {
    if(isamp<nseuil) {
      xsamp<-matrix(switch(distrib,norm=rnorm(msim*isamp),unif=runif(msim*isamp)), ncol=msim)
      mat<-apply(xsamp,2,quantile,quant)
      xtab<-apply(mat,1,quantile,quant.pi)
    } else {
      xtab<-matrix(nrow=3,ncol=length(quant))
      xtab[2,]<-switch(distrib,norm=qnorm(quant),unif=quant)
      xtab[1,]<-xtab[2,]-demi*sqrt(nseuil/isamp)
      xtab[3,]<-xtab[2,]+demi*sqrt(nseuil/isamp)
    }
    for(i in which(nsamp==isamp)) {
      binf[i,]<-xtab[1,]
      bmed[i,]<-xtab[2,]
      bsup[i,]<-xtab[3,]
    }
  }
  return(list(binf=binf,bsup=bsup,bmed=bmed))
}

compute.bands.true<-function(sim.ypl,xlab,quant=c(0.025,0.5,0.975), alpha=0.95) {
  # Compute a prediction interval around selected quantiles of the normal or uniform distribution, for different sizes of samples by using the data
  quant.pi<-c((1-alpha)/2,0.5,1-(1-alpha)/2)
  grp<-unique(sim.ypl$grp)
  binf<-bsup<-bmed<-matrix(nrow=length(grp),ncol=length(quant), dimnames=list(grp,quant))
  for(igrp in grp) {
    mat<-apply(sim.ypl$ypl[sim.ypl$grp==igrp,],2,quantile,quant)
    xtab<-apply(mat,1,quantile,quant.pi)
    binf[grp==igrp,]<-xtab[1,]
    bmed[grp==igrp,]<-xtab[2,]
    bsup[grp==igrp,]<-xtab[3,]
  }
  binf<-binf[match(xlab,rownames(binf)),]
  bmed<-bmed[match(xlab,rownames(bmed)),]
  bsup<-bsup[match(xlab,rownames(bsup)),]
  return(list(binf=binf,bsup=bsup,bmed=bmed))
}

# boxplot
aux.scatter.box<-function(plmat,plot.opt,dotline,distrib="norm",main=NULL,namcat=NULL,alpha=0.95) {
  # Auxiliary function used to plot boxplots
  if(!is.null(namcat)) xname<-namcat
  if(is.null(main)) main<-plot.opt$main
  if(is.null(dotline)) dotline<-c((1-alpha)/2,0.5,1-(1-alpha)/2)
  if(is.numeric(plmat$x)) {
    plot.opt$xlim<-c(min(plmat$x),max(plmat$x))
    xbin<-npde.binning(plmat$x,plot.opt,verbose=plot.opt$interactive)
    plmat<-data.frame(grp=xbin$xgrp,plmat)
    xcent<-xbin$xat
    if(is.null(namcat)) xname<-format(xbin$xat,digits=2)
  } else {
    xcent<-1:length(unique(plmat$x))
    plmat<-data.frame(grp=as.factor(plmat$x),plmat)
    xname<-unique(plmat$x)
  }
  boxplot(plmat$y~plmat$grp,at=xcent,main=main,sub=plot.opt$sub, xlim=plot.opt$xlim, ylim=plot.opt$ylim,xlab=plot.opt$xlab,ylab=plot.opt$ylab, cex=plot.opt$cex,cex.lab=plot.opt$cex.lab,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main,pch=plot.opt$pch, varwidth=plot.opt$varwidth,boxwex=plot.opt$boxwex, col=plot.opt$col.fillpi, names=xname)	
  if(length(dotline)>0) {
    for(i in dotline) abline(h=i,lty=plot.opt$lty.abline,lwd=plot.opt$lwd.abline, col=plot.opt$col.abline)
  }
}

# Scatterplot
aux.scatter<-function(plmat,plot.opt,dotline,distrib="norm",main=NULL,namcat=NULL,sim.ypl=NULL) {
  # Auxiliary function used to plot scatterplots
  alpha<-(1-plot.opt$vpc.interval)/2
  if(is.null(main)) main<-plot.opt$main
  if(is.null(dotline)) dotline<-c((1-alpha)/2,0.5,1-(1-alpha)/2)
  if(plot.opt$bands) {
    if(is.numeric(plmat[,1])) {
      # ECO TODO: should binning be done on full data or after censoring ?
      xbin<-npde.binning(plmat[,1],plot.opt,verbose=plot.opt$interactive)
      plmat<-data.frame(grp=xbin$xgrp,plmat)
      xcent<-xbin$xat
    } else {
      plmat<-data.frame(grp=plmat[,1],plmat)
      xcent<-unique(plmat[,2])
    }
    nseuil<-200
    alp.pi<-plot.opt$pi.size
    if(alp.pi<0.5) alp.pi<-(1-alp.pi)
    quant<-c(alpha,0.5,1-alpha)
    if(plot.opt$approx.pi) {
      nobs<-tapply(plmat$grp,plmat$grp,length)
      bnds<-compute.bands(nobs,nseuil,quant,distrib,alp.pi)
    } else {
      sim.ypl<-data.frame(grp=plmat$grp,cens=plmat[,3],sim.ypl)
      bnds<-compute.bands.true(sim.ypl,quant,alpha=alp.pi)
    }
    binf<-bnds$binf
    bsup<-bnds$bsup
    bmed<-bnds$bmed
    if(is.null(plot.opt$ylim)) plot.opt$ylim<-c(min(plmat[,4],binf,na.rm=T), max(plmat[,4],bsup,na.rm=T))
    plot(plmat[,2],plmat[,4],type="n",xlab=plot.opt$xlab,ylab=plot.opt$ylab, main=main,sub=plot.opt$sub,col=plot.opt$col,pch=plot.opt$pch, cex=plot.opt$cex, cex.lab=plot.opt$cex.lab,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main,xlim=plot.opt$xlim,ylim=plot.opt$ylim, xaxt=plot.opt$xaxt, yaxt=plot.opt$yaxt,axes=plot.opt$axes,frame.plot=plot.opt$frame.plot)
    for(icol in c(1,3)) polygon(c(xcent,rev(xcent)),c(binf[,icol],rev(bsup[,icol])), col=plot.opt$col.fillpi,lty=plot.opt$lty.lpi,lwd=plot.opt$lwd.lpi,border=plot.opt$col.lpi)
    polygon(c(xcent,rev(xcent)),c(binf[,2],rev(bsup[,2])), col=plot.opt$col.fillmed,lty=plot.opt$lty.lmed,lwd=plot.opt$lwd.lmed,border=plot.opt$col.lmed)
    if(length(dotline)==0) {
      for(icol in c(1,3)) lines(xcent,bmed[,icol],lty=plot.opt$lty.lpi, lwd=plot.opt$lwd.lpi,col=plot.opt$col.lpi)
      lines(xcent,bmed[,2],lty=plot.opt$lty.lmed, lwd=plot.opt$lwd.lmed,col=plot.opt$col.lmed)
    }
    percobs<-matrix(unlist(tapply(plmat[,4],plmat$grp,quantile,quant,na.rm=T)), ncol=3,byrow=T)
    for(icol in c(1,3)) lines(xcent,percobs[,icol],lty=plot.opt$lty.lobs, lwd=plot.opt$lwd.lobs, col=plot.opt$col.lobs)
    lines(xcent,percobs[,2],lty=plot.opt$lty.lobs,lwd=plot.opt$lwd.lobs, col=plot.opt$col.lmed)
  } else {
    plmat<-cbind(grp=plmat[,1],plmat)
    if(is.null(plot.opt$ylim)) plot.opt$ylim<-c(min(plmat[,4],na.rm=T), max(plmat[,4],na.rm=T))
    plot(plmat[,2],plmat[,4],type="n",xlab=plot.opt$xlab,ylab=plot.opt$ylab, main=main,sub=plot.opt$sub,cex=plot.opt$cex,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main,cex.lab=plot.opt$cex.lab, xlim=plot.opt$xlim,ylim=plot.opt$ylim,xaxt=plot.opt$xaxt, yaxt=plot.opt$yaxt,axes=plot.opt$axes,frame.plot=plot.opt$frame.plot)
  }  
  if(plot.opt$plot.obs) {
    points(plmat[plmat[,3]==0,2],plmat[plmat[,3]==0,4],col=plot.opt$col.pobs, pch=plot.opt$pch,cex=plot.opt$cex)
    if(plot.opt$plot.loq) points(plmat[plmat[,3]==1,2],plmat[plmat[,3]==1,4], col=plot.opt$col.pcens, pch=plot.opt$pch.pcens,cex=plot.opt$cex) 
  }
  if(length(dotline)>0) {
    if(length(dotline)==3) {
      for(i in c(1,3)) abline(h=dotline[i],lty=plot.opt$lty.lpi,lwd=plot.opt$lwd.lpi, col=plot.opt$col.lpi)
      abline(h=dotline[2],lty=plot.opt$lty.lmed,lwd=plot.opt$lwd.lmed, col=plot.opt$col.lmed)
    } else for(i in dotline) abline(h=i,lty=plot.opt$lty.abline,lwd=plot.opt$lwd.abline, col=plot.opt$col.abline)
  }
return(list(plmat=plmat,bnds=bnds))
  }

# histogram
aux.plot.hist<-function(plmat,plot.opt,distrib="norm", nclass=10,sim.ypl=NULL) {
  ndat<-dim(plmat)[1]
  plot.bands<-FALSE
  if(plot.opt$bands) {
    if(!plot.opt$approx.pi & length(sim.ypl)>0) plot.bands<-TRUE
    if(plot.opt$approx.pi) {
      xsamp<-switch(distrib,norm=rnorm(ndat*100),unif=runif(ndat*100))
      sim.ypl<-matrix(xsamp,nrow=ndat)
      plot.bands<-TRUE
    }
  }	
  if(plot.bands) {
    x<-hist(plmat$xpd,breaks=nclass,plot=FALSE)
    alpha<-plot.opt$pi.size
    if(alpha>0.5) alpha<-1-alpha
    tmat<-matrix(nrow=length(x$breaks)-1,ncol=dim(sim.ypl)[2])
    nB<-length(x$breaks)
    for(j in 1:dim(sim.ypl)[2]) {
      xvec<-cut(sim.ypl[,j],breaks=x$breaks,include.lowest=TRUE, ordered_result=TRUE)
      tmat[,j]<-table(xvec)
    }
    row.names(tmat)<-names(table(xvec))
    bnds<-apply(tmat,1,quantile,c(alpha/2,0.5,1-alpha/2))
    if(is.null(plot.opt$xlim)) plot.opt$xlim<-c(min(x$breaks),max(x$breaks))
    if(is.null(plot.opt$ylim)) plot.opt$ylim<-c(0,max(c(bnds,x$counts)))
    plot(x$breaks[-nB],x$counts,xlim=plot.opt$xlim,ylim=plot.opt$ylim, type="n",xlab=plot.opt$xlab,ylab=plot.opt$ylab,main=plot.opt$main,sub=plot.opt$sub, cex.lab=plot.opt$cex.lab,cex=plot.opt$cex,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main,xaxt=plot.opt$xaxt, yaxt=plot.opt$yaxt,axes=plot.opt$axes,frame.plot=plot.opt$frame.plot)
    # PI on counts
    rect(x$breaks[-nB],bnds[1,],x$breaks[-1],bnds[3,],lwd=plot.opt$lwd.lpi, lty=plot.opt$lty.lpi, border=plot.opt$col.lpi,col=plot.opt$col.fillpi)
    segments(x$breaks[-nB],bnds[2,],x$breaks[-1],bnds[2,], lty=plot.opt$lty.lmed,lwd=plot.opt$lwd.lmed, col=plot.opt$col.lmed)		
    segments(x$breaks[-c(1,nB)],bnds[2,-1],x$breaks[-c(1,nB)], bnds[2,-(nB-1)],lty=plot.opt$lty.lmed,lwd=plot.opt$lwd.lmed, col=plot.opt$col.lmed)
    # Observed data		
    segments(x$breaks[-nB],x$counts,x$breaks[-1],x$counts,lwd=plot.opt$lwd.lobs+1, lty=plot.opt$lty.lobs,col=plot.opt$col.lobs)		
    #		segments(x$breaks[-c(1,nB)],x$counts[-1],x$breaks[-c(1,nB)], x$counts[-(nB-1)],lwd=plot.opt$lwd+1,lty=plot.opt$lty, col=plot.opt$col)
    rect(x$breaks[-nB],0,x$breaks[-1],x$counts,lwd=plot.opt$lwd.lobs+1, lty=plot.opt$lty.lobs,border=plot.opt$col.lobs)
  } else {
    x<-hist(plmat$xpd,breaks=nclass,xlab=plot.opt$xlab,ylab=plot.opt$ylab, main=plot.opt$main,sub=plot.opt$sub,lwd=plot.opt$lwd.lobs,lty=plot.opt$lty.lobs, border=plot.opt$col.lobs,col=plot.opt$col.fillpi, cex.lab=plot.opt$cex.lab,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main,cex=plot.opt$cex,xaxt=plot.opt$xaxt, yaxt=plot.opt$yaxt,axes=plot.opt$axes)
    if(distrib=="norm") {
      xpl<-min(plmat$xpd)+c(0:100)/100*(max(plmat$xpd)-min(plmat$xpd))
      ypl<-dnorm(xpl)
      ypl<-ypl/max(ypl)*max(x$counts)
      lines(xpl,ypl,lty=plot.opt$lty.abline,lwd=plot.opt$lwd.abline,col=plot.opt$col.abline)  
    }
    if(distrib=="unif") {
      abline(h=ndat/nclass,lty=plot.opt$lty.abline,lwd=plot.opt$lwd.abline,col=plot.opt$col.abline)
    }
  }
}

# Empirical cdf/QQplot
aux.plot.dist<-function(plmat,plot.opt,dist.type="ecdf",distrib="norm",nrep=0, nclass=0,ties=TRUE,sim.ypl=NULL,verbose=FALSE) {
  if(nclass==0) binning<-FALSE else {
    binning<-TRUE
    nbin<-plot.opt$vpc.bin
  }
  ndat<-dim(plmat)[1]
  plot.bands<-FALSE
  if(plot.opt$bands) {
    if(!plot.opt$approx.pi & length(sim.ypl)>0) plot.bands<-TRUE
    if(plot.opt$approx.pi) {
      xsamp<-switch(distrib,norm=rnorm(ndat*100),unif=runif(ndat*100))
      sim.ypl<-matrix(xsamp,nrow=ndat)
      plot.bands<-TRUE
    }
  }
  if(ties & nrep>0) yq<-seq(1/(2*nrep),1-1/(2*nrep),length.out=ndat) else  yq<-seq(0,1,length.out=ndat)
  if(distrib=="norm") yq<-qnorm(yq)		
  if(dist.type=="ecdf") {
    theo.samp<-yq
    yq<-seq(1/ndat,1,length.out=ndat)
  }
  ymat<-plmat[order(plmat$xpd),]
  ymat<-cbind(ymat,ecdf=yq)
  if(dist.type=="ecdf") {
    ysh<-data.frame(x=plmat$xpd,y=yq)
    yobs<-data.frame(x=ymat$xpd,y=ymat$ecdf,cens=ymat$cens)
  } else {
    ysh<-data.frame(x=yq,y=plmat$xpd)
    yobs<-data.frame(x=ymat$ecdf,y=ymat$xpd,cens=ymat$cens)
  }
  if(!binning) {
    xvec<-ysh$x
    yvec<-ysh$y
    if(plot.bands) {
      alpha<-plot.opt$pi.size
      if(alpha>0.5) alpha<-1-alpha
      sim.sort<-colsort(sim.ypl)
      bnds<-apply(sim.sort,1,quantile,c(alpha/2,0.5,1-alpha/2))
      if(dist.type=="ecdf") {
        xvec<-c(xvec,c(bnds))
        yvec<-c(yvec,yq)
      }
      if(dist.type=="qqplot") {
        xvec<-c(xvec,yq)
        yvec<-c(yvec,c(bnds))
      }
      if(plot.opt$xlog) xvec<-xvec[!is.na(xvec) & xvec>0]
      if(plot.opt$ylog) yvec<-yvec[!is.na(yvec) & yvec>0]
    }
    if(is.null(plot.opt$xlim))
      plot.opt$xlim<-c(min(xvec,na.rm=TRUE),max(xvec,na.rm=TRUE))
    if(is.null(plot.opt$ylim))
      plot.opt$ylim<-c(min(yvec,na.rm=TRUE),max(yvec,na.rm=TRUE))
    plot(ysh$x,ysh$y,xlim=plot.opt$xlim,ylim=plot.opt$ylim,type="n", xlab=plot.opt$xlab,ylab=plot.opt$ylab,main=plot.opt$main,sub=plot.opt$sub, cex.lab=plot.opt$cex.lab,cex=plot.opt$cex,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main,log=plot.opt$logtyp, xaxt=plot.opt$xaxt, yaxt=plot.opt$yaxt,axes=plot.opt$axes,frame.plot=plot.opt$frame.plot)
    if(plot.bands) {
      if(dist.type=="ecdf") {
        polygon(c(bnds[1,],rev(bnds[3,])),c(yq,rev(yq)),col=plot.opt$col.fillpi, lwd=plot.opt$lwd.lpi,border=plot.opt$col.lpi,lty=plot.opt$lty.lpi)
        lines(bnds[2,],yq,lty=plot.opt$lty.lmed, col=plot.opt$col.lmed, lwd=plot.opt$lwd.lmed)
      } else { 
        polygon(c(yq,rev(yq)),c(bnds[1,],rev(bnds[3,])),col=plot.opt$col.fillpi, lwd=plot.opt$lwd.lpi,border=plot.opt$col.lpi,lty=plot.opt$lty.lpi)
        lines(yq,bnds[2,],lty=plot.opt$lty.lmed, col=plot.opt$col.lmed, lwd=plot.opt$lwd.lmed)
      }
    }
    lines(yobs$x,yobs$y,lty=plot.opt$lty,col=plot.opt$col.lobs,lwd=plot.opt$lwd)
    if(plot.opt$type=="p" | plot.opt$type=="b") {
      points(yobs$x[yobs$cens==0],yobs$y[yobs$cens==0], col=plot.opt$col.pobs, pch=plot.opt$pch.pobs,cex=plot.opt$cex)
      if(plot.opt$plot.loq) points(yobs$x[yobs$cens==1],yobs$y[yobs$cens==1], col=plot.opt$col.pcens, pch=plot.opt$pch.pcens,cex=plot.opt$cex)
    }
    if(!plot.bands) { # theoretical distribution
      if(dist.type=="qqplot" | distrib=="unif") abline(0,1,lty=plot.opt$lty.abline, lwd=plot.opt$lwd.abline,col=plot.opt$col.abline) else lines(theo.samp,c(1:ndat)/ndat, lty=plot.opt$lty.abline, lwd=plot.opt$lwd.abline,col=plot.opt$col.abline)
    }
  } else {
    if(verbose) cat("Binning not implemented yet\n")
  }
}

# Binning the X data
npde.binning<-function(xvec,plot.opt,verbose=FALSE) {
  xvec1<-xvec
  xvec<-xvec[!is.na(xvec)]
  if(is.na(pmatch(plot.opt$vpc.method,c("optimal","width","user","equal")))) {
    if(verbose) cat("Binning method",plot.opt$vpc.method,"not found, reverting to equal binning\n")
    plot.opt$vpc.method<-"equal"
  }
  if(!is.na(pmatch(plot.opt$vpc.method,"optimal")) & !"mclust"%in%.packages(all.available = TRUE)) {
    if(verbose) cat("mclust library not installed, reverting to equal binning\n")
    plot.opt$vpc.method<-"equal"
  }
  if(!is.na(pmatch(plot.opt$vpc.method,"user")) & is.null(plot.opt$vpc.breaks)) {
    if(verbose) cat("User-defined method specified, but vpc.breaks is empty; reverting to equal binning\n")
    plot.opt$vpc.method<-"equal"
  }
  if(!is.na(pmatch(plot.opt$vpc.method,c("equal","width"))) & is.null(plot.opt$vpc.bin)) {
    plot.opt$vpc.bin<-10
  }
  nbin<-plot.opt$vpc.bin
  if(length(unique(xvec))<=nbin) {
    xgrp<-match(xvec,sort(unique(xvec)))
    xpl<-tapply(xvec,xgrp,mean)
  } else {  	
    if(!is.na(pmatch(plot.opt$vpc.method,"user"))) {
      bnds<-plot.opt$vpc.breaks
      if(min(bnds)>=min(xvec)) bnds<-c(min(xvec)-1,bnds)
      if(max(bnds)<max(xvec)) bnds<-c(bnds,max(xvec))
    }
    if(!is.na(pmatch(plot.opt$vpc.method,"equal"))) {
      xvec2<-xvec;xvec2[xvec2==min(xvec)]<-min(xvec)-1      
      if(!is.null(plot.opt$vpc.extreme) & length(plot.opt$vpc.extreme)==2) {
        xq<-plot.opt$vpc.extreme
        xquant<-c(0,seq(xq[1],xq[2],length.out=(nbin-1)),1)
      } else xquant<-(0:nbin)/nbin
      bnds<-unique(quantile(xvec2,xquant,type=8))
    }
    if(!is.na(pmatch(plot.opt$vpc.method,"width"))) {
      if(plot.opt$xlog) xvec2<-log(xvec[xvec>0]) else xvec2<-xvec
      if(!is.null(plot.opt$vpc.extreme) & length(plot.opt$vpc.extreme)==2) {
        xq<-plot.opt$vpc.extreme
        xq1<-quantile(xvec2,xq,type=8)
        bnds<-c(min(xvec2),seq(xq1[1],xq1[2],length.out=(nbin-1)),max(xvec2))
      } else bnds<-seq(min(xvec2),max(xvec2),length.out=(nbin+1))
      if(plot.opt$xlog) {
        bnds<-exp(bnds)
        bnds[length(bnds)]<-bnds[length(bnds)]+1
        if(sum(xvec<=0)>0) bnds<-c(min(xvec),bnds)
      } 
      bnds[1]<-bnds[1]-1
    }
    if(!is.na(pmatch(plot.opt$vpc.method,"optimal"))) {
      yfit<-Mclust(xvec,G=((nbin-5):(nbin+5)))
      xgrp<-yfit$classification
      xpl<-yfit$parameters$mean
      xpl<-xpl[match(names(table(xgrp)),names(xpl))]
      names(xpl)<-paste("[",tapply(xvec,xgrp,min),"-",tapply(xvec,xgrp,max),"]", sep="")
    } else {
      xgrp<-factor(cut(xvec,bnds,include.lowest=F))
      xpl<-tapply(xvec,xgrp,mean)
    }
  }
  nbin<-length(unique(xgrp))
  npl<-tapply(xvec,xgrp,length)
  tab<-cbind(Interval=names(xpl),Centered.On=format(xpl,digits=2),Nb.obs=npl)
  row.names(tab)<-1:dim(tab)[1]
  if(verbose) {
    xnam<-switch(EXPR=plot.opt$vpc.method,equal="by quantiles on X", width="equal sized intervals",user="user-defined bins",optimal="clustering algorithm")
    cat("Method used for binning:",xnam,", dividing into the following",nbin,"intervals\n")
    print(tab,quote=F)
  }
  xgrp2<-rep(NA,length(xvec1))
  xgrp2[!is.na(xvec1)]<-xgrp
  return(list(xgrp=xgrp2,xat=xpl))
}

# Prediction intervals, vpc
compute.vpc.pi<-function(ysim,xgrp,idrep,nbin,vpc.pi=0.95) {
  nsim<-length(unique(idrep))
  sim.pi.low<-sim.pi.med<-sim.pi.up<-matrix(0,nrow=nbin,ncol=nsim)
  alpha<-(1-vpc.pi)/2
  i0<-1
  for(irep in unique(idrep)) {
    ysim1<-ysim[idrep==irep]
    l1<-unlist(tapply(ysim1,xgrp,function(vec) quantile(vec,c(alpha,0.5,1-alpha),na.rm=TRUE)))
    l1<-matrix(l1,ncol=3,byrow=TRUE)
    sim.pi.low[,i0]<-l1[,1]
    sim.pi.med[,i0]<-l1[,2]
    sim.pi.up[,i0]<-l1[,3]
    i0<-i0+1
  }
  return(list(sim.pi.low=sim.pi.low,sim.pi.med=sim.pi.med,sim.pi.up=sim.pi.up))
}

###############################	   VPC	 ########################################

npde.plot.vpc<-function(npdeObject,npc=FALSE,...) {
  npde.plot.meanprofile(npdeObject,which="yobs",...)
  npc.stat<-c()
  if(npc==TRUE) {
    # ECO TODO: compute NPC - interpolation ? 
  }
  invisible(list(npc=npc.stat))
}

################################  p_LOQ  ########################################

npde.plot.loq<-function(npdeObject,xaxis="x",nsim=200,...) {
  # Plot of the probability of an observation being LOQ versus X or predictions, overlaying
  ### the predicted probability (according to the model)
  ### the observed probability
  ### a prediction band obtained using the simulated data
  args1<-match.call(expand.dots=TRUE)
  i1<-match("loq",names(args1))
  if(!is.na(i1)) {
    loq<-as.numeric(args1[[i1]])
  } else {
    if(length(npdeObject["data"]["loq"])==0) {
      ploq<-c()
      yobs<-npdeObject["data"]["data"][,npdeObject["data"]["name.response"]]
      if(length(npdeObject["data"]["loq"])>0) {
        loq<-npdeObject["data"]["loq"]
        if(npdeObject@options$verbose) cat("Computing p(y<LOQ) using LOQ=",loq,"\n")
      } else {
        yloq<-yobs[npdeObject["data"]["icens"]]
        if(length(unique(yloq))==1) {
          if(npdeObject@options$verbose) cat("Same LOQ for all missing data, loq=",loq,"\n")
          loq<-unique(yloq)
        } else {
          loq<-min(unique(yloq))
          if(npdeObject@options$verbose) cat("Computing p(y<LOQ) for the lowest LOQ, loq=",loq,"\n")
        }
        npdeObject["data"]["loq"]<-loq
      }
      if(is.infinite(npdeObject["data"]["loq"])) {
        if(npdeObject@options$verbose) cat("No loq defined in the data, and no censored data to define it, please call npde.plot.loq with the option loq=XXX where XXX is the value of the LOQ.\n")
        return()
      }
    } else loq<-npdeObject["data"]["loq"]
  }
  has.cens<-FALSE
  if(length(npdeObject["data"]["icens"])>0) {
    has.cens<-TRUE
    icens<-npdeObject["data"]["icens"] #ECO TODO: icens jamais utilise
  }
  plot.opt<-npdeObject["prefs"]
  plot.opt$main<-"Probability of being under the LOQ"
  plot.opt$ylab<-"Pr(Y<LOQ)"
  plot.opt<-replace.plotoptions(plot.opt,...)
  if(plot.opt$new) {
    mfrow<-plot.opt$mfrow
    if(length(mfrow)==0) mfrow<-c(1,1)
    par(mfrow=mfrow,ask=plot.opt$ask)
  }
  logtyp<-""
  if(plot.opt$xlog) logtyp<-paste(logtyp,"x",sep="")
  if(plot.opt$ylog) logtyp<-paste(logtyp,"y",sep="")  
  nsim<-min(nsim,npdeObject["sim.data"]["nrep"])
  
  # Binning
  xvec<-switch(xaxis, x=npdeObject["data"]["data"][,npdeObject["data"]["name.predictor"]], pred=npdeObject["results"]["res"]$ypred, cov="Not implemented yet")
  if(!is.numeric(xvec)) {
    if(npdeObject@options$verbose) cat(xvec,"\n")
    return()
  }
  if(has.cens) ydat<-npdeObject["data"]["data"][,npdeObject["data"]["name.cens"]] else ydat<-rep(0,length(xvec))
  xbin<-npde.binning(xvec,plot.opt,verbose=plot.opt$interactive)
  xgrp<-xbin$xgrp
  xpl<-xbin$xat
  nbin<-length(unique(xgrp))
  isamp<-sample(1:npdeObject["sim.data"]["nrep"],nsim)
  ysim<-npdeObject["sim.data"]["datsim"]$ysim
  xtab<-matrix(nrow=nsim,ncol=nbin)
  for(i in 1:nsim) 
    xtab[i,]<-tapply(ysim[npdeObject["sim.data"]["datsim"]$irsim==isamp[i]] < loq,xgrp,mean)
  alpha<-(1-plot.opt$vpc.interval)/2
  quant<-c(alpha,0.5,1-alpha)
  ypl<-apply(xtab,2,quantile,quant)
  xobs<-tapply(ydat,xgrp,mean)
  if(is.null(plot.opt$ylim)) plot.opt$ylim<-c(0,max(c(xtab,xobs),na.rm=T))
  plot(xpl,ypl[1,],type="n",xlim=plot.opt$xlim,ylim=plot.opt$ylim,xlab=plot.opt$xlab, ylab=plot.opt$ylab,main=plot.opt$main,sub=plot.opt$sub,log=logtyp,cex=plot.opt$cex, cex.lab=plot.opt$cex.lab,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main,xaxt=plot.opt$xaxt, yaxt=plot.opt$yaxt,axes=plot.opt$axes,frame.plot=plot.opt$frame.plot)
  polygon(c(xpl,rev(xpl)),c(ypl[1,],rev(ypl[3,])),col=plot.opt$col.fillpi, lty=plot.opt$lty.lpi,lwd=plot.opt$lwd.lpi, border=plot.opt$col.lpi)
  lines(xpl,ypl[2,],lty=plot.opt$lty.lmed,col=plot.opt$col.lmed, lwd=plot.opt$lwd.lmed)
  lines(xpl,xobs,lty=plot.opt$lty.lobs,col=plot.opt$col.pobs, lwd=plot.opt$lwd.lobs)
}

#####################  Plots with reference profile  #############################
aux.npdeplot.meanprof<-function(mbin,msim) {
  # Compute a reference profile based on simulations from the model
  # mbin : matrix with columns xat (centre of the bins) and xgrp (bin number/group)
  # msim : matrix with simulations, 2 columns used (grp=which bin, ysim=simulated value)
  ymed<-tapply(msim$ysim,msim$grp,mean)
  sdmed<-tapply(msim$ysim,msim$grp,sd)
  ymed<-ymed[match(mbin$xlab,names(ymed),nomatch=0)]
  sdmed<-sdmed[match(mbin$xlab,names(sdmed),nomatch=0)]
  #	ymed<-ymed[order(names(ymed))]
  #	sdmed<-sdmed[order(names(sdmed))]
  if(length(ymed)<dim(mbin)[1]) {
    # Linear interpolation
    #				ypmed<-approx(xcent,ymed,xout=mbin$xat,rule=2)$y
    # Spline interpolation
    cat("Not all time points/bins are represented in the subset used for the reference profile: spline interpolation will be used to predict the entire profile, but this may distort the aspect of the plot significantly; we advise using another reference profile.\n")
    xcent<-mbin$xat[mbin$xlab %in% names(ymed)]
    ypmed<-spline(xcent,ymed,xout=mbin$xat)$y
    spmed<-spline(xcent,sdmed,xout=mbin$xat)$y
    iint<-1-as.integer(mbin$xlab %in% names(ymed))
    mpref<-data.frame(xat=mbin$xat,grp=mbin$grp,mean=ypmed,sd=spmed,int=iint,xlab=mbin$xlab)	
  } else mpref<-data.frame(xat=mbin$xat,grp=mbin$grp,mean=ymed,sd=sdmed,int=rep(0,length(sdmed)),xlab=mbin$xlab)	
  return(mpref)
}

aux.npdeplot.computepi<-function(plmat,plot.opt,xlab,xat,mpref=NULL,dotline=NULL,sim.ypl=NULL,distrib="norm",onlog=FALSE) {
  # Compute prediction interval for the observed data, the size of which depends on the number of observations in each bin
  # Input
  # plmat: matrix of values to plot
  # plot.opt:
  # xat: center of bins
  # xlab: group tag
  # mpref: reference profile
  # dotline: 
  # sim.ypl:
  # distrib: reference distribution
  # onlog: whether E and SD are computed on the observed value or after log transformation	
  # Output
  # bnds: boundaries of the prediction intervals for each bin present in plmat [may be <nbin]
  xinf<-sqrt(12)
  alpha<-(1-plot.opt$vpc.interval)/2
  nseuil<-200
  alp.pi<-plot.opt$pi.size
  if(alp.pi<0.5) alp.pi<-(1-alp.pi)
  quant<-c(alpha,0.5,1-alpha)
  if(!plot.opt$approx.pi & !is.null(sim.ypl)) {
    nrep<-length(sim.ypl)/dim(plmat)[1]
    yprov<-list(grp=plmat$grp,cens=plmat$cens,ypl=matrix(sim.ypl,ncol=nrep))
    bnds<-compute.bands.true(yprov,quant,xlab=xlab,alpha=alp.pi)
    bnds$xcent<-xat
  } else {
    nobs<-tapply(plmat$grp,plmat$grp,length)
    bnds<-compute.bands(nobs,nseuil,quant,distrib,alp.pi)
    bnds$xcent<-xat[match(names(nobs),xlab)]
  }
  # Transforming the boundaries if reference profile
  if(!is.null(mpref)) {
    # 		idx<-(names(nobs) %in% mpref$xlab)
    # 		mpref<-mpref[idx,]
    #			for(i in 1:3) bnds[[i]]<-bnds[[i]][unique(xbin$xgrp) %in% names(ymed),]
    if(onlog) {
      if(distrib=="unif") {
        for(i in 1:3) bnds$binf[,i]<-exp((bnds$binf[,i]-0.5)*mpref$sd*xinf+mpref$mean)
        for(i in 1:3) bnds$bmed[,i]<-exp((bnds$bmed[,i]-0.5)*mpref$sd*xinf+mpref$mean)
        for(i in 1:3) bnds$bsup[,i]<-exp((bnds$bsup[,i]-0.5)*mpref$sd*xinf+mpref$mean)
      } else {
        for(i in 1:3) bnds$binf[,i]<-exp(bnds$binf[,i]*mpref$sd+mpref$mean)
        for(i in 1:3) bnds$bmed[,i]<-exp(bnds$bmed[,i]*mpref$sd+mpref$mean)
        for(i in 1:3) bnds$bsup[,i]<-exp(bnds$bsup[,i]*mpref$sd+mpref$mean)
      }								
    } else {
      if(distrib=="unif") {
        for(i in 1:3) bnds$binf[,i]<-(bnds$binf[,i]-0.5)*mpref$sd*xinf+mpref$mean
        for(i in 1:3) bnds$bmed[,i]<-(bnds$bmed[,i]-0.5)*mpref$sd*xinf+mpref$mean
        for(i in 1:3) bnds$bsup[,i]<-(bnds$bsup[,i]-0.5)*mpref$sd*xinf+mpref$mean
      } else {
        for(i in 1:3) bnds$binf[,i]<-bnds$binf[,i]*mpref$sd+mpref$mean
        for(i in 1:3) bnds$bmed[,i]<-bnds$bmed[,i]*mpref$sd+mpref$mean
        for(i in 1:3) bnds$bsup[,i]<-bnds$bsup[,i]*mpref$sd+mpref$mean
      }				
    }
    if(is.null(dotline)) dline<-NULL else {
      dline<-data.frame(xat=mpref$xat)
      for(i in 1:length(dotline)) {
        if(onlog) {
          if(distrib=="unif") x1<-exp((dotline[i]-0.5)*mpref$sd*xinf+mpref$mean) else x1<-exp(dotline[i]*mpref$sd+mpref$mean)
        } else {
          if(distrib=="unif") x1<-(dotline[i]-0.5)*mpref$sd*xinf+mpref$mean else x1<-dotline[i]*mpref$sd+mpref$mean
        }
        dline<-cbind(dline,x1)
      }
    }
  }	else {
    if(is.numeric(plmat$x) & !is.null(dotline)) {
      dline<-data.frame(xat=seq(min(plmat$x,na.rm=TRUE),max(plmat$x,na.rm=TRUE),length.out=100))
      for(i in 1:length(dotline))
        dline<-cbind(dline,rep(dotline[i],100))
    } else dline<-NULL		
  }
  return(list(bnds=bnds,dline=dline))
}

aux.npdeplot.transform<-function(plmat,plot.opt,xat,mpref=NULL,distrib="norm",onlog=FALSE) {
  # Input
  # plmat: values to plot
  # plot.opt: preferences for plots
  # mpref: reference profile
  # distrib: reference distribution
  # onlog: whether E and SD are computed on the observed value or after log transformation
  # Output
  # plmat: updated with a ty column containing the data to plot
  # percobs: percentile of the observed distribution, used in the plots
  # dotprof: dotline profile
  xinf<-sqrt(12) # used only if distrib is "unif"
  alpha<-(1-plot.opt$vpc.interval)/2
  quant<-c(alpha,0.5,1-alpha)
  if(is.null(mpref)) {
    ty<-plmat$y
  } else {
    mpr<-mpref[mpref$int==0,] # use only points not interpolated initially, to avoid double interpolation
    yfmed<-spline(mpr$xat,mpr$mean,xout=plmat$x)$y
    sfmed<-spline(mpr$xat,mpr$sd,xout=plmat$x)$y
    if(onlog) {
      if(distrib=="unif") ty<-exp((plmat$y-0.5)*sfmed*xinf+yfmed) else ty<-exp(plmat$y*sfmed+yfmed)
    } else {
      if(distrib=="unif") ty<-(plmat$y-0.5)*sfmed*xinf+yfmed else ty<-plmat$y*sfmed+yfmed				
    }			
  }
  percobs<-matrix(unlist(tapply(ty,plmat$grp,quantile,quant,na.rm=T)),ncol=3,byrow=T)
  row.names(percobs)<-xat
  plmat<-cbind(plmat,ty=ty)
  return(list(plmat=plmat,percobs=percobs))
}

aux.npdeplot.plot<-function(plmat,percobs,plot.opt,main=NULL,bnds=NULL,dotline=NULL) {
  # Input
  # plmat: values to plot
  # plot.opt: preferences for plots
  if(is.null(plot.opt$ylim)) {
    if(length(grep("y",plot.opt$logtyp))==0) plot.opt$ylim<-c(min(c(plmat$y,c(percobs),bnds$binf[,1]),na.rm=T),max(c(plmat$y,c(percobs),bnds$bsup[,3]),na.rm=T)) else {
      vec1<-c(plmat$y,c(percobs),bnds$binf[,1])
      vec2<-c(plmat$y,c(percobs),bnds$bsup[,3])
      plot.opt$ylim<-c(min(vec1[!is.na(vec1) & vec1>0]),max(vec2[!is.na(vec2) & vec2>0]))
    }
  }
  if(is.null(plot.opt$logtyp)) plot.opt$logtyp<-""
  plot(plmat$x,plmat$ty,type="n",xlab=plot.opt$xlab,ylab=plot.opt$ylab,main=main,sub=plot.opt$sub,col=plot.opt$col,pch=plot.opt$pch, cex=plot.opt$cex, cex.lab=plot.opt$cex.lab,cex.axis=plot.opt$cex.axis,cex.main=plot.opt$cex.main,xlim=plot.opt$xlim,ylim=plot.opt$ylim, xaxt=plot.opt$xaxt, yaxt=plot.opt$yaxt,axes=plot.opt$axes,frame.plot=plot.opt$frame.plot,log=plot.opt$logtyp)
  # Plot prediction intervals
  if(plot.opt$bands & !is.null(bnds)) {
    xcent<-bnds[[4]]
    binf<-bnds$binf
    bsup<-bnds$bsup
    bmed<-bnds$bmed
    for(icol in c(1,3)) polygon(c(xcent,rev(xcent)),c(binf[,icol],rev(bsup[,icol])), col=plot.opt$col.fillpi,lty=plot.opt$lty.lpi,lwd=plot.opt$lwd.lpi,border=plot.opt$col.lpi)
    polygon(c(xcent,rev(xcent)),c(binf[,2],rev(bsup[,2])), col=plot.opt$col.fillmed,lty=plot.opt$lty.lmed,lwd=plot.opt$lwd.lmed,border=plot.opt$col.lmed)		
    if(is.null(dotline)) {
      for(icol in c(1,3)) lines(xcent,bmed[,icol],lty=plot.opt$lty.lpi, lwd=plot.opt$lwd.lpi,col=plot.opt$col.lpi)
      lines(xcent,bmed[,2],lty=plot.opt$lty.lmed, lwd=plot.opt$lwd.lmed,col=plot.opt$col.lmed)
    } else {
      if(dim(dotline)[2]==4) {
        for(i in c(2,4)) lines(dotline[,1],dotline[,i],lty=plot.opt$lty.lpi,lwd=plot.opt$lwd.lpi, col=plot.opt$col.lpi)
        lines(dotline[,1],dotline[,3],lty=plot.opt$lty.lmed,lwd=plot.opt$lwd.lmed, col=plot.opt$col.lmed)					
      } else {
        for(i in 2:dim(dotline)[2]) lines(dotline[,1],dotline[,i],lty=plot.opt$lty.abline,lwd=plot.opt$lwd.abline, col=plot.opt$col.abline)					
      }
    }
  } else xcent<-as.double(row.names(percobs))
  # Plot percentiles of observed data	
  for(icol in c(1,3)) lines(xcent,percobs[,icol],lty=plot.opt$lty.lobs, lwd=plot.opt$lwd.lobs, col=plot.opt$col.lobs)
  lines(xcent,percobs[,2],lty=plot.opt$lty.lobs,lwd=plot.opt$lwd.lobs, col=plot.opt$col.lmed)
  # Plot observed data
  if(plot.opt$plot.obs) {
    points(plmat$x[plmat$cens==0],plmat$ty[plmat$cens==0],col=plot.opt$col.pobs, pch=plot.opt$pch,cex=plot.opt$cex)
    if(plot.opt$plot.loq) points(plmat$x[plmat$cens==1],plmat$ty[plmat$cens==1], col=plot.opt$col.pcens, pch=plot.opt$pch.pcens,cex=plot.opt$cex) 
  }
  return(list(plmat=plmat,bnds=bnds))
}

aux.npdeplot.main<-function(plmat,plot.opt,namcat=NULL,msim=NULL,ref.prof=NULL,sim.ypl=NULL, dotline=NULL, distrib="norm",onlog=FALSE) {
  if(is.numeric(plmat$x)) {
    # ECO TODO: should binning be done on full data or after censoring ?
    xbin<-npde.binning(plmat$x,plot.opt,verbose=plot.opt$interactive)
    plmat<-data.frame(grp=xbin$xgrp,plmat)
    xcent<-xbin$xat
  } else {
    plmat<-data.frame(grp=plmat$x,plmat)
    xcent<-unique(plmat$x)
  }
  xlab<-as.character(1:length(xcent))
  if(!is.null(msim)) {
    nrep<-dim(msim)[1]/dim(plmat)[1]
    msim<-cbind(msim,grp=rep(xbin$xgrp,nrep))
    mbin<-data.frame(xat=xbin$xat,grp=names(xbin$xat),xlab=xlab)
  } else {
    if(!is.null(sim.ypl)) nrep<-length(sim.ypl)/dim(plmat)[1]
  }
  zecat<-unique(plmat$cov)
  ncat<-length(zecat)
  if(ncat==1) covsplit<-FALSE else covsplit<-TRUE
  if(!is.null(msim)) mpref<-aux.npdeplot.meanprof(mbin,msim[msim$use==1,]) else mpref<-NULL
  for(icat in 1:ncat) {
    tit<-plot.opt$main
    if(tit=="") tit<-namcat[icat]
    if(covsplit & is.null(ref.prof)) {
      is.cat<-(plmat$cov==zecat[icat])
      if(!is.null(msim)) mpref<-aux.npdeplot.meanprof(mbin,msim[rep(is.cat,nrep),])
    } else {
      is.cat<-rep(TRUE,dim(plmat)[1])
      if(plot.opt$main=="" & !is.null(ref.prof)) {
        if(icat==1) {
          cat("The plot uses a reference profile\n")
          if(covsplit) tit<-paste("Reference profile:",tit,sep="")
        }
      }
    }
    plmat1<-plmat[is.cat,]
    xcal1<-aux.npdeplot.transform(plmat1,plot.opt,xat=xcent[xlab%in%plmat1$grp],mpref=mpref,distrib=distrib,onlog=onlog)
    plmat1<-xcal1$plmat # now includes a ty column containing the data to plot
    #		cat(covsplit,ncat,dim(plmat),"-",dim(plmat1),"-",sum(is.cat),"\n")
    percobs<-xcal1$percobs
    if(!is.null(mpref)) xat<-mpref$xat[xlab%in%plmat1$grp] else xat<-xcent[xlab%in%plmat1$grp]
    if(!is.null(sim.ypl)) sim.ypl1<-sim.ypl[rep(is.cat,nrep)] else sim.ypl1<-NULL
    xcal<-aux.npdeplot.computepi(plmat1,plot.opt,xlab=xlab,xat=xcent,mpref=mpref,dotline=dotline,sim.ypl=sim.ypl1,distrib=distrib,onlog=onlog)
    aux.npdeplot.plot(plmat1,percobs,plot.opt,main=tit,bnds=xcal$bnds,dotline=xcal$dline)	
  }
  return(list(xcalobs=xcal1,xcalsim=xcal))
}

npde.plot.meanprofile<-function(npdeObject,which="npde",xaxis="x",covsplit=FALSE, xscale=FALSE, onlog=FALSE, ref.prof=NULL, ...) {
  plot.opt.defaults<-function(plot.opt,which="npde") {
    plot.opt$new<-TRUE
    plot.opt$xlab<-switch(xaxis, x=paste(npdeObject["data"]["name.predictor"]," (", npdeObject["data"]["units"]$x,")",sep=""), pred=paste("Predicted ", npdeObject["data"]["name.response"]," (", npdeObject["data"]["units"]$y,")",sep=""), cov="")
    plot.opt$ylab<-switch(which,pd="pd",npd="npd",npde="npde",yobs=paste("Predicted ", npdeObject["data"]["name.response"]," (", npdeObject["data"]["units"]$y,")",sep=""))
    plot.opt$main<-""	
    return(plot.opt)
  }
  
  if(match(which,c("npde","pd","npd","yobs"),nomatch=0)==0) {
    cat("Option which=",which,"not recognised\n")
    return()
  }
  if(match(xaxis,c("x","pred","cov"),nomatch=0)==0) {
    cat("Option xaxis=",xaxis,"not recognised\n")
    return()
  }
  if(xaxis=="cov" & length(npdeObject["data"]["name.covariates"])==0) {
    cat("Option xaxis set to 'cov' but no covariate in dataset\n")
    return()
  }
  # checks on presence of the value to plot (no need to check when which is yobs, the data is always there)
  if(which=="npde" & length(npdeObject["results"]["res"]$npde)==0) {
    cat("    Missing npde object to plot.\n")
    return()
  }
  if(which %in% c("pd","npd") & length(npdeObject["results"]["res"]$pd)==0)  {
    cat("    Missing pd object to plot.\n")
    return()
  }
  if(which=="yobs") {
    xaxis<-"x"
    xscale<-FALSE
  }
  if(xaxis=="pred") xscale<-FALSE
  if(!is.null(ref.prof)) xscale<-TRUE # If a reference profile is given, we assume xscale should be true
  if(!xscale) ref.prof<-NULL
  if(length(npdeObject["data"]["icens"])>0) has.cens<-TRUE else has.cens<-FALSE
  args1<-match.call(expand.dots=TRUE)
  i1<-match("xlab",names(args1))
  if(!is.na(i1)) {
    change.xlab<-TRUE
  } else change.xlab<-FALSE
  i1<-match("ncat",names(args1))
  if(!is.na(i1)) {
    change.ncat<-TRUE
  } else change.ncat<-FALSE
  plot.opt<-npdeObject["prefs"]
  plot.opt<-plot.opt.defaults(plot.opt,which=which)
  plot.opt<-replace.plotoptions(plot.opt,...)
  ask<-plot.opt$ask
  # Checking format for ref.prof is appropriate
  if(xscale & !is.null(ref.prof)) {
    if(!is.list(ref.prof)) {
      cat("The reference profile must be entered as a named list, eg list(ID=c(1,5)) to select subjects with ID=1 and 5 as reference; names should refer to columns in the data file.\n")
      ref.prof<-NULL
    }
  }
  
  if(!covsplit & plot.opt$new) {
    mfrow<-plot.opt$mfrow
    if(length(mfrow)==0) mfrow=c(1,1)
    par(mfrow=mfrow,ask=ask)
  }
  if(xaxis=="cov") {
    # 		plot.opt$box<-TRUE
    if(covsplit & npdeObject@options$verbose) cat("    graph versus covariates requested, setting covsplit to FALSE\n")
    covsplit<-FALSE
  }
  if(covsplit & length(npdeObject["data"]["name.covariates"])==0) {
    if(npdeObject@options$verbose) cat("No covariates in the dataset\n")
    covsplit<-FALSE
  }
  #	if(which=="pd") plot.opt$ylim<-c(0,1) else plot.opt$ylim<-NULL
  if(which=="pd") distrib<-"unif" else distrib<-"norm"
  if(covsplit | xaxis=="cov") {
    if(plot.opt$which.cov=="") plot.opt$which.cov<-"all"
    if(is.numeric(plot.opt$which.cov)) plot.opt$which.cov<-npdeObject["data"]["name.covariates"][plot.opt$which.cov]
    if(plot.opt$which.cov=="all") {
      lcov<-npdeObject["data"]["name.covariates"]
      lunit<-npdeObject["data"]["units"]$covariates
    } else {
      icov<-match(plot.opt$which.cov,npdeObject["data"]["name.covariates"])
      lcov<-npdeObject["data"]["name.covariates"][icov]
      lunit<-npdeObject["data"]["units"]$covariates[icov]
    }
    ncov<-length(lcov)
    if(ncov==0) {
      cat("Cannot find covariate",plot.opt$which.cov,"in the dataset\n")
      covsplit<-FALSE
      if(xaxis=="cov") invisible(return()) # can't plot
    }
  } else ncov<-0
  if(xaxis=="cov" & length(plot.opt$mfrow)==0 & plot.opt$new) {
    n1<-round(sqrt(ncov))
    n2<-ceiling(ncov/n1)
    plot.opt$mfrow<-c(n1,n2)
    par(mfrow=plot.opt$mfrow,ask=ask)
  } 
  logtyp<-""
  if(plot.opt$xlog) logtyp<-paste(logtyp,"x",sep="")
  if(plot.opt$ylog) logtyp<-paste(logtyp,"y",sep="")
  plot.opt$logtyp<-logtyp
  sim.ypl<-NULL
  if(!plot.opt$approx.pi) {
    if(which %in% c("pd","npd")) {
      if(length(npdeObject["results"]["pd.sim"])==0) {
        cat("You have requested to use simulations to provide the prediction intervals, but the simulated pd are not present.\n")
        plot.opt$approx.pi<-TRUE
      } else sim.ypl<-npdeObject["results"]["pd.sim"]
    }
    if(which=="npde") {
      if(length(npdeObject["results"]["npde.sim"])==0) {
        cat("You have requested to use simulations to provide the prediction intervals, but the simulated npde are not present.\n")
        plot.opt$approx.pi<-TRUE
      } else sim.ypl<-npdeObject["results"]["npde.sim"]
    }
  }
  if(which=="yobs" & plot.opt$bands) {
    sim.ypl<-npdeObject["sim.data"]["datsim"]$ysim
    plot.opt$approx.pi<-FALSE
  }
  if(which=="npde") ypl<-npdeObject["results"]["res"]$npde
  if(which %in% c("pd","npd")) ypl<-npdeObject["results"]["res"]$pd
  if(which=="yobs") ypl<-npdeObject["data"]["data"][,npdeObject["data"]["name.response"]]
  if(which=="npd") {
    ypl<-qnorm(ypl)
    if(!plot.opt$approx.pi) sim.ypl<-qnorm(sim.ypl)
  }
  if(xaxis=="x") xpl<-npdeObject["data"]["data"][, npdeObject["data"]["name.predictor"]]
  idobs<-npdeObject["data"]["data"][npdeObject["data"]["not.miss"], npdeObject["data"]["name.group"]]
  if(xaxis=="cov") xpl<-rep(1,length(ypl))
  if(xaxis=="pred") xpl<-npdeObject["results"]["res"]$ypred
  if(xscale) msim<-npdeObject["sim.data"]["datsim"]$ysim else msim<-NULL
  if(has.cens) plmat<-data.frame(x=xpl,cens=npdeObject["data"]["data"][, npdeObject["data"]["name.cens"]],y=ypl) else plmat<-data.frame(x=xpl,cens=rep(0,length(xpl)),y=ypl)
  plmat<-plmat[npdeObject["data"]["not.miss"],]
  if(xscale) msim<-msim[rep(npdeObject["data"]["not.miss"],npdeObject["sim.data"]["nrep"])]
  if(!is.null(sim.ypl) & which=="yobs") sim.ypl<-sim.ypl[rep(npdeObject["data"]["not.miss"],npdeObject["sim.data"]["nrep"])] # for which="pd" or "npde", MDV lines have already been removed
  # Reference profile: extracting matrix msim corresponding to the reference profile
  if(!is.null(msim)) {
    #	idx<-rep(1:dim(plmat)[1],npdeObject["sim.data"]["nrep"])
    if(!is.null(ref.prof) & xscale) {
      iuse<-rep(0,dim(plmat)[1])
      dat1<-npdeObject["data"]["data"][npdeObject["data"]["not.miss"],]
      # 			if(is.logical(ref.prof)) {
      # 				dat1<-cbind(my.indx=1:dim(dat1)[1],dat1)
      # 				dat1<-subset(dat1,ref.prof)
      # 				iuse<-rep(dat1[,1],npdeObject["sim.data"]["nrep"])
      # 			} else {
      for(iref in 1:length(ref.prof)) {
        i<-names(ref.prof)[iref]
        i1<-which(dat1[,i] %in% ref.prof[[iref]])
        if(iref==1) idx1<-i1 else idx1<-intersect(idx1,i1)
      }
      iuse[idx1]<-1
      iuse<-rep(iuse,npdeObject["sim.data"]["nrep"])
      #			}
    } else iuse<-rep(1,length(msim))
    msim<-data.frame(ysim=msim,use=iuse)	
  }
  if(which=="yobs") dotline<-NULL
  if(which %in% c("npd","npde")) {
    x1<-abs(qnorm((1-plot.opt$pi.size)/2))
    dotline<-c(-x1,0,x1)
  }
  if(which=="pd") dotline<-c((1-plot.opt$pi.size)/2,0.5,1-(1-plot.opt$pi.size)/2)
  if(covsplit | xaxis=="cov") {
    # Same limits for the different plots; can't be defined here, too narrow (or add an arbitrary factor to reflect the PI, which may not work with model misspecification)
    if(FALSE) {
      if(!xscale) {
        if(is.null(plot.opt$xlim) & xaxis!="cov") plot.opt$xlim<-c(min(xpl,na.rm=T), max(xpl,na.rm=T))
        if(is.null(plot.opt$ylim) & xaxis!="cov") plot.opt$ylim<-c(min(ypl,na.rm=T), max(ypl,na.rm=T))			
      }			
    }
    plmat<-cbind(plmat,cov=rep(1,dim(plmat)[1]))		
    for(icov in 1:ncov) {
      namcov<-lcov[icov]
      namunit<-lunit[icov]
      zecov<-npdeObject["data"]["data"][npdeObject["data"]["not.miss"],namcov]
      if(xaxis=="cov") plmat$x<-zecov
      ucov<-zecov[match(unique(idobs),idobs)]
      plot.box<-plot.opt$box
      # ECO: SECURISER 
      # detect type of covariate (continuous or )
      if(!is.numeric(ucov) | plot.opt$ncat>=length(unique(ucov))) {
        if(length(unique(ucov))>4) cat("Too many categories, the plot will not be informative.\n")
        if(!is.numeric(ucov) & xaxis=="cov") plot.box<-TRUE
        covcont<-FALSE
        ncat<-length(unique(ucov))
        if(xaxis=="cov") namcat<-sort(unique(zecov)) else namcat<-paste(namcov,sort(unique(zecov)),sep="=")
      } else {
        if(change.ncat | plot.opt$ncat!=3) {
          ncat<-plot.opt$ncat
          seqcat<-seq(0,1,length.out=(ncat+1))
          zecov<-cut(zecov,breaks=quantile(ucov,seqcat), include.lowest=TRUE, ordered_result=TRUE)						
          nam1<-paste("q",format(seqcat[-(ncat+1)],digits=2),"-q",format(seqcat[-1],digits=2),sep="")
          namcat<-paste(namcov,nam1,sep=": ")
        } else {
          zecov<-cut(zecov,breaks=quantile(ucov,c(0,0.25,0.75,1)), include.lowest=TRUE, ordered_result=TRUE)
          ncat<-3
          namcat<-paste(namcov,c("<Q1","Q1-Q3",">Q3"),sep=": ")
        }
        covcont<-TRUE
        if(plot.box) plmat$x<-zecov
      }
      if(plot.opt$new & xaxis!="cov") {
        mfrow<-plot.opt$mfrow
        if(length(mfrow)==0) {
          if(ncat<=3) mfrow=c(1,ncat) else {
            n1<-round(sqrt(ncat))
            n2<-ceiling(ncat/n1)
            mfrow<-c(n1,n2)
          }
        }
        par(mfrow=mfrow,ask=ask)
      }
      zecat<-sort(unique(zecov))
      if(xaxis=="cov") {
        plmat$cov<-rep(1,dim(plmat)[1])
        namcat<-""
        if(!change.xlab) plot.opt$xlab<-namcov
        if(plot.box) {
          aux.scatter.box(plmat,plot.opt,dotline=dotline)
        }
      } else {
        plmat$cov<-zecov
        if(plot.box) {
          for(ic in 1:length(zecat)) aux.scatter.box(plmat[plmat$cov==zecat[ic],], plot.opt,dotline=dotline,main=namcat[ic])
        }
      }
      if(!plot.box) {
        aux.npdeplot.main(plmat,plot.opt,namcat=namcat,msim=msim,ref.prof=ref.prof,sim.ypl=sim.ypl, dotline=dotline,distrib=distrib,onlog=onlog)
        gdata <- aux.npdeplot.main(plmat,plot.opt,namcat=namcat,msim=msim,ref.prof=ref.prof,sim.ypl=sim.ypl, dotline=dotline,distrib=distrib,onlog=onlog)}
    }
  } else {
    plmat<-cbind(plmat,cov=rep(1,dim(plmat)[1]))
    if(plot.opt$box) aux.scatter.box(plmat,plot.opt,dotline=dotline,main=NULL) else aux.npdeplot.main(plmat,plot.opt,namcat=NULL,msim=msim,ref.prof=ref.prof,sim.ypl=sim.ypl,dotline=dotline,distrib=distrib,onlog=onlog)
  }
}
