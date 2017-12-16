$PROB WARFARIN PKPD
;
;O'Reilly RA, Aggeler PM. Studies on coumarin anticoagulant drugs
;Initiation of warfarin therapy without a loading dose.
;Circulation 1968;38:169-177
;
;O'Reilly RA, Aggeler PM, Leong LS. Studies of the coumarin anticoagulant
;drugs: The pharmacodynamics of warfarin in man.
;Journal of Clinical Investigation 1963;42(10):1542-1551
;
$INPUT ID TIME WT AGE SEX DVID DV MDV EVID CMT AMT
$DATA ka1_to_emax1_simln.csv
IGNORE (DVID.EQ.2)

$EST MAX=9990 NSIG=3 SIGL=9 PRINT=1
METHOD=COND INTER NOABORT

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

   IF (NEWIND.EQ.0) LN2=LOG(2)

   FSZV=WT/70
   FSZCL=FSZV**0.75

   CL=FSZCL*THETA(1)*EXP(ETA(1))
   V=FSZV*THETA(2)*EXP(ETA(2))
   TABS=THETA(3)*EXP(ETA(3))
   TLAG=THETA(4)*EXP(ETA(4))

   KA=LN2/TABS
   ALAG1=TLAG
   S2=V

$ERROR

   CP=A(2)/S2
   Y=CP*(1+ERR(1)) + ERR(2)

$TABLE ID TIME DVID Y
ONEHEADER NOPRINT FILE=pk.fit



