pdf("Figure_1ABC.pdf",width=10,height=10)
grid.draw(p1)
frame()
grid.draw(p2)
frame()
grid.draw(p3)
dev.off()

pdf("Figure_1D.pdf",width=10,height=12)
grid.draw(p4)
dev.off()


pdf("Figure_7A.pdf",width=10,height=7)
grid.draw(Fig7A)
dev.off()
pdf("Figure_7B.pdf",width=10,height=3.5)
grid.draw(Fig7B)
dev.off()
pdf("Figure_7D.pdf",width=10,height=3.5,useDingbats=FALSE)
grid.draw(Fig7D)
dev.off()
pdf("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_7C.pdf",width=6,height=4)
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




pdf("Figure_4A.pdf",width=10,height=7)
grid.draw(Fig4A)
dev.off()
pdf("Figure_4B.pdf",width=10,height=3.5)
grid.draw(Fig4B)
dev.off()
pdf("Figure_4D.pdf",width=10,height=3.5,useDingbats=FALSE)
grid.draw(Fig4D)
dev.off()
eta2$nBaseline <- eta2$nE0
pdf("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_4C.pdf",width=6,height=4)
gpairs( eta2[,c("nBaseline","nEmax","nEC50")], 
        upper.pars = list(conditional = 'boxplot', scatter = 'loess'),
        lower.pars = list(scatter = 'stats',conditional = "barcode"),
        diag.pars = list(fontsize = 9, show.hist = TRUE, hist.color = "gray"),
        stat.pars =list(fontsize = 12, signif = 0.05, verbose = FALSE, use.color = TRUE, missing = 'missing', just = 'centre'),
        scatter.pars = list(pch = 20),outer.margins = c(2,2,3.5,2))
grid.text('(C) Correlations, histogram of EBE (Misspecified delay, Immediate effect)', x=.5, y=.95,gp=gpar(fontsize=12,fontface="bold"))
dev.off()



pdf("Figure_5A.pdf",width=10,height=7)
grid.draw(Fig5A)
dev.off()
pdf("Figure_5B.pdf",width=10,height=3.5)
grid.draw(Fig5B)
dev.off()
pdf("Figure_5D.pdf",width=10,height=3.5,useDingbats=FALSE)
grid.draw(Fig5D)
dev.off()
eta3$nBaseline <- eta3$nE0
pdf("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_5C.pdf",width=6,height=4)
gpairs( eta3[,c("nBaseline","nTeq","nEC50")], 
        upper.pars = list(conditional = 'boxplot', scatter = 'loess'),
        lower.pars = list(scatter = 'stats',conditional = "barcode"),
        diag.pars = list(fontsize = 9, show.hist = TRUE, hist.color = "gray"),
        stat.pars =list(fontsize = 12, signif = 0.05, verbose = FALSE, use.color = TRUE, missing = 'missing', just = 'centre'),
        scatter.pars = list(pch = 20),outer.margins = c(2,2,3.5,2))
grid.text('(C) Correlations, histogram of EBE (Misspecified delay, Effect compartment)', x=.5, y=.95,gp=gpar(fontsize=12,fontface="bold"))
dev.off()


pdf("Figure_6A.pdf",width=10,height=7)
grid.draw(Fig6A)
dev.off()
pdf("Figure_6B.pdf",width=10,height=3.5)
grid.draw(Fig6B)
dev.off()
pdf("Figure_6D.pdf",width=10,height=3.5,useDingbats=FALSE)
grid.draw(Fig6D)
dev.off()
eta4$nBaseline <- eta4$nE0
pdf("C:/Users/Tram/Google Drive/ISOP MOEV/Revision/R script_Data/tram/Figure_6C.pdf",width=6,height=4)
gpairs( eta4[,c("nBaseline","nTeq","nEC50")], 
        upper.pars = list(conditional = 'boxplot', scatter = 'loess'),
        lower.pars = list(scatter = 'stats',conditional = "barcode"),
        diag.pars = list(fontsize = 9, show.hist = TRUE, hist.color = "gray"),
        stat.pars =list(fontsize = 12, signif = 0.05, verbose = FALSE, use.color = TRUE, missing = 'missing', just = 'centre'),
        scatter.pars = list(pch = 20),outer.margins = c(2,2,3.5,2))
grid.text('(C) Correlations, histogram of EBE (Misspecified delay and correlation)', x=.5, y=.95,gp=gpar(fontsize=12,fontface="bold"))
dev.off()