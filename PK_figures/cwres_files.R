

res1 <- read.table(".\\True\\Residuals.txt",header=T)
res2 <- read.table(".\\False_Struc\\Residuals.txt",header=T)
res3 <- read.table(".\\False_Cov\\Residuals.txt",header=T)
res4 <- read.table(".\\False_Corr\\Residuals.txt",header=T)
res5 <- read.table(".\\False_Cons\\Residuals.txt",header=T)
res6 <- read.table(".\\False_Prop\\Residuals.txt",header=T)

resN1 <- read.table("C:/Users/Tram/Documents/Working/CPTPSP_MoEv/Examples/NONMEM/resnonmem_true.txt",header=T)
resN2 <- read.table("C:/Users/Tram/Documents/Working/CPTPSP_MoEv/Examples/NONMEM/resnonmem_Fstruc.txt",header=T)
resN3 <- read.table("C:/Users/Tram/Documents/Working/CPTPSP_MoEv/Examples/NONMEM/resnonmem_Fcov.txt",header=T)
resN4 <- read.table("C:/Users/Tram/Documents/Working/CPTPSP_MoEv/Examples/NONMEM/resnonmem_Fintervar.txt",header=T)
resN5 <- read.table("C:/Users/Tram/Documents/Working/CPTPSP_MoEv/Examples/NONMEM/resnonmem_Fconst.txt",header=T)
resN6 <- read.table("C:/Users/Tram/Documents/Working/CPTPSP_MoEv/Examples/NONMEM/resnonmem_Fprop.txt",header=T)

resN1 <- resN1[-which(resN1$TIME==0),]
resN1 <- resN1[,c(10,12)]
resN2 <- resN2[-which(resN2$TIME==0),]
resN2 <- resN2[,c(10,12)]
resN3 <- resN3[-which(resN3$TIME==0),]
resN3 <- resN3[,c(10,12)]
resN4 <- resN4[-which(resN4$TIME==0),]
resN4 <- resN4[,c(10,12)]
resN5 <- resN5[-which(resN5$TIME==0),]
resN5 <- resN5[,c(10,12)]
resN6 <- resN6[-which(resN6$TIME==0),]
resN6 <- resN6[,c(10,12)]

res1 <- cbind.data.frame(res1,resN1)
res2 <- cbind.data.frame(res2,resN2)
res3 <- cbind.data.frame(res3,resN3)
res4 <- cbind.data.frame(res4,resN4)
res5 <- cbind.data.frame(res5,resN5)
res6 <- cbind.data.frame(res6,resN6)
#colnames(res1) <- colnames(res2) <- colnames(res3) <-
#  colnames(res4) <-colnames(res5) <-colnames(res6) <- c("Time",  "PPRED", "IPRED", "PWRES", "IWRES", "NPDE",  "Treatment",   "CPRED", "CWRES")
write.table(res1,".\\True\\Residuals_all.txt",col.names = T,row.names = F,quote=F)
write.table(res2,".\\False_Struc\\Residuals_all.txt",col.names = T,row.names = F,quote=F)
write.table(res3,".\\False_Cov\\Residuals_all.txt",col.names = T,row.names = F,quote=F)
write.table(res4,".\\False_Corr\\Residuals_all.txt",col.names = T,row.names = F,quote=F)
write.table(res5,".\\False_Cons\\Residuals_all.txt",col.names = T,row.names = F,quote=F)
write.table(res6,".\\False_Prop\\Residuals_all.txt",col.names = T,row.names = F,quote=F)

