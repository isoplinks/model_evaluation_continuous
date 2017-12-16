#nmvpc2r
BEGIN {
   if (ARGS!="") {
      ok=getline < ARGS
      gsub(/\\/,"/")
      WFNHOME=$0
      close(ARGS)
   }
   DVID=DVID+0
   hasdvid=(DVID>0)
   if (NMCTL=="") NMCTL=".ctl"
}
#/source\(\"nmvpc_PKPD_Functions\.R\"/ {
#  print "source(\"" WFNHOME "/bin/nmvpc_PKPD_Functions.R" "\") ## set up functions"
#  next
#}

/if \(ISWFN\) NMDIR/ && NMDIR != "" {
   print "if (ISWFN) NMDIR=\"" NMDIR "\""
   next
}

/^ *NMDIR/ && NMDIR != "" {
   print "NMDIR=\"" NMDIR "\""
   next
}

/^ *NMTBL/ && NMTBL != "" {
   print "NMTBL=\"" NMTBL "\""
   next
}

/^ *modelName/ {
   i=index(MODEL,NMCTL)
   if (i) MODEL=substr(MODEL,1,i-1)
   print "modelName = \"" MODEL "\""
   next
}
/^ *obsFileName/ {
   i=index(OBSFILE,NMCTL)
   if (i) OBSFILE=substr(OBSFILE,1,i-1)
   print "obsFileName = \"" OBSFILE "\""
   next
}
/^ *thisDVID/ {
   print "thisDVID = " DVID
   next
}

/^ *hasDVID/ {
   if (hasdvid)
      print "hasDVID = T"
   else
      print "hasDVID = F"
   next
}

/^ *logaxis/ {
   print "logaxis = \"" LOGAXIS "\""
   next
}
/^ *Ymin/ && !/Ymax/{
   print "Ymin = " YMIN
   next
}
/^ *LLOQ/ {
   if (LLOQ==".") LLOQ="\".\""
   print "LLOQ = " LLOQ
   next
}

/^ *Ymax/ {
   print "Ymax = " YMAX
   next
}
/^ *Ytick/ {
   print "Ytick = " YTICK
   next
}
/^ *Ylabel/ && YLABEL!="" {
   gsub(/#/," ",YLABEL)
   print "Ylabel = \"" YLABEL "\""
   next
}
/^ *Xmin/ && !/Xmax/ {
   print "Xmin = " XMIN
   next
}
/^ *Xmax/ {
   print "Xmax = " XMAX
   next
}
/^ *Xtick/ {
   print "Xtick = " XTICK
   next
}
/^ *Xlabel/ && XLABEL!="" {
   gsub(/#/," ",XLABEL)
   print "Xlabel = \"" XLABEL "\""
   next
}
/^ *binTimes/ {
   print "binTimes = c(" BINTIMES")"
   next
}

#Do not change the next line!
#SELECT specific observations e.g. study number
#obsFile = obsFile[obsFile$SDY==1,]
#simFile = simFile[simFile$SDY==1,]

/^#SELECT/ && SELECT!="" {
#print "SELECT=|"SELECT"|"
   iand= match(toupper(SELECT),/\.AND\./)
   if (iand)
      SELECTOBS=substr(SELECT,1,iand-1) " & obsFile$"  substr(SELECT,iand+5)
   else
      SELECTOBS=SELECT
   print "obsFile = obsFile[obsFile$"SELECTOBS",]"
   if (iand) 
      SELECTSIM=substr(SELECT,1,iand-1) " & simFile$"  substr(SELECT,iand+5)
   else
      SELECTSIM=SELECT
   print "simFile = simFile[simFile$"SELECTSIM",]"
   next
}

/^ *pdfTxt/ && PDFTXT!="" {
   gsub(/#/,"_",PDFTXT)
   print "pdfTxt = \"" PDFTXT "\""
   next
}

/^ *figOutputDir/ && FIGDIR!="" {
   print "figOutputDir = \"" FIGDIR ".pdf/\""
   next
}

/^ *isCSV/ && ISCSV!="" {
   if (index(toupper(ISCSV),"Y"))
      isCSV="T"
   else if (ISCSV+0>0)
      isCSV="T"
   else
      isCSV="F"
   print "isCSV = "isCSV
   next
}

/^ *isPC/ && ISPC!="" {
   if (index(toupper(ISPC),"Y"))
      isPC="T"
   else if (ISPC+0>0)
      isPC="T"
   else
      isPC="F"
   print "isPC = "isPC
   next
}

/^ *isOBS/ && ISOBS!="" {
   if (index(toupper(ISOBS),"Y"))
      isOBS="T"
   else if (ISOBS+0>0)
      isOBS="T"
   else
      isOBS="F"
   print "isOBS = "isOBS
   next
}

/^ *CIpercentile/ {
   print "CIpercentile = " CIPCT
   next
}
/^ *plotCI/ && PLOTCI!="" {
   if (index(toupper(PLOTCI),"N"))
      plotCI="F"
   else if (PLOTCI+0>0)
      plotCI="T"
   else
      plotCI="T"
   print "plotCI = "plotCI
   next
}

/^ *PIpercentile/ {
   print "PIpercentile = " PIPCT
   next
}
/^ *plotPI/ && PLOTPI!="" {
   if (index(toupper(PLOTPI),"N"))
      plotPI="F"
   else if (PLOTPI+0>0)
      plotPI="T"
   else
      plotPI="T"
   print "plotPI = "plotPI
   next
}

/^ *isSTD/ && ISSTD!="" {
   if (index(toupper(ISSTD),"N"))
      isSTD="F"
   else if (ISSTD+0>0)
      isSTD="T"
   else
      isSTD="T"
   print "isSTD = "isSTD
   next
}

/^ *isNEW/ && ISNEW!="" {
   if (index(toupper(ISNEW),"Y"))
      isNEW="T"
   else if (ISNEW+0>0)
      isNEW="T"
   else
      isNEW="F"
   print "isNEW = "isNEW
   next
}

/^ *isBIG/ && ISBIG!="" {
   if (index(toupper(ISBIG),"Y"))
      isBIG="T"
   else if (ISBIG+0>0)
      isBIG="T"
   else
      isBIG="F"
   print "isBIG = "isBIG
   next
}

/^ *timeScale / && TIMESCALE!="" {
   print "timeScale = "TIMESCALE
   next
}

/^ *simFile\$TIME\=simFile\$TIME\*timeScale/ && TIMENAME!="" {
   print "   simFile$TIME=simFile$"TIMENAME"*timeScale"
   next
}

/^ *hasMDV/ && HASMDV!="" {
   if (index(toupper(HASMDV),"Y"))
      hasMDV="T"
   else if (HASMDV+0>0)
      hasMDV="T"
   else
      hasMDV="F"
   print "hasMDV = "hasMDV
   next
}

/simFile\$MDV/ && MDVPNAME!="" {
   gsub(/simFile\$MDV/,"simFile$"MDVPNAME)
   print
   next
}


{print}

