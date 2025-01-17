$PROB Warfarin PKPD
;
;O'Reilly RA, Aggeler PM. Studies on coumarin anticoagulant drugs
;Initiation of warfarin therapy without a loading dose.
;Circulation 1968;38:169-177
;
;O'Reilly RA, Aggeler PM, Leong LS. Studies of the coumarin anticoagulant
;drugs: The pharmacodynamics of warfarin in man.
;Journal of Clinical Investigation 1963;42(10):1542-1551
;
$INPUT id time wt age sex dvid dv mdv evid cmt amt
$DATA ka1_to_emax1_simln.csv
IGNORE (DVID.EQ.2)

;$EST MAX=9990 NSIG=3 SIGL=9 PRINT=1
;METHOD=COND INTER ABORT

$THETA
(0.01,0.131,1) ; POP_CL L/h/70kg
(0.01,8.17,20) ; POP_V L/70kg
(0.01,0.792,24) ; POP_TABS h
(0.01,0.96,24) ; POP_LAG h
$OMEGA BLOCK(4)
0.0666 ; PPV_CL
-0.00236 0.0185 ; PPV_V
0.00923 -0.0159 0.66 ; PPV_TABS
-0.00508 0.065 -0.348 0.376 ; PPV_LAG
$SIGMA 0.00619 ; RUV_CV
$SIGMA 0.127 ; RUV_SD mg/L

$SUBR ADVAN2 TRAN2

$PK
OBS=DV
   IF (NEWIND.EQ.0) LN2=LOG(2)

   FSZV=WT/70
   FSZCL=FSZV**0.75

   CL=FSZCL*POP_CL*EXP(PPV_CL)
   V=FSZV*POP_V*EXP(PPV_V)
   TABS=POP_TABS*EXP(PPV_TABS)
   TLAG=POP_LAG*EXP(PPV_LAG)

   KA=LN2/TABS
   ALAG1=TLAG
   S2=V

$ERROR

   CP=A(2)/S2
   Y=CP*(1+RUV_CV) + RUV_SD

; Simulation start
REP=IREP
$TABLE REP ID TIME DV PRED OBS MDV DVID WT AGE SEX
NOAPPEND ONEHEADER NOPRINT FILE=vpc.fit
$SIM (20150330) ONLYSIM NSUB=100
; Simulation end
