$PROB WARFARIN PKPD

;O'Reilly RA, Aggeler PM. Studies on coumarin anticoagulant drugs
;Initiation of warfarin therapy without a loading dose.
;Circulation 1968;38:169-177
;
;O'Reilly RA, Aggeler PM, Leong LS. Studies of the coumarin anticoagulant
;drugs: The pharmacodynamics of warfarin in man.
;Journal of Clinical Investigation 1963;42(10):1542-1551

$INPUT ID TIME WT AGE SEX DVID DV MDV EVID CMTX AMT
$DATA ka1_to_emax1_simln.csv

$EST MAX=9990 NSIG=3 SIGL=9 PRINT=1
METHOD=COND INTER NOABORT
$COV

$THETA
(0.01,0.131,1) FIX ; POP_CL L/h/70kg
(0.01,8.17,20) FIX ; POP_V L/70kg
(0.01,0.792,24) FIX ; POP_TABS h
(0.01,0.96,24) FIX ; POP_LAG h
$OMEGA BLOCK(4)
0.0666 ; PPV_CL
-0.00236 0.0185 ; PPV_V
0.00923 -0.0159 0.66 ; PPV_TABS
-0.00508 0.065 -0.348 0.376 FIX ; PPV_LAG
$SIGMA 0.00619 FIX ; RUV_CV
$SIGMA 0.127 FIX ; RUV_SD mg/L

$THETA
(0.01,96.2,200) ; POP_E0
(-INF,-162.,0) ; POP_EMAX
(0.01,5.42,100) ; POP_C50
(0.01,40.1,100) ; POP_TEQ
$OMEGA BLOCK(3)
0.00185 ; PPV_E0
-0.00155 0.0487 ; PPV_C50
-0.00218 -0.0449 0.0615 ; PPV_TEQ
$OMEGA 0 FIX ; PPV_EMAX
$SIGMA 15.3 ; RUV_FX

$SUBR ADVAN4 TRAN4

$PK

   IF (NEWIND.EQ.0) LN2=LOG(2)

   FSZV=WT/70
   FSZCL=FSZV**0.75

   CL=FSZCL*THETA(1)*EXP(ETA(1))
   V=FSZV*THETA(2)*EXP(ETA(2))
   TABS=THETA(3)*EXP(ETA(3))
   TLAG=THETA(4)*EXP(ETA(4))

   E0=THETA(5)*EXP(ETA(5))
   EMAX=THETA(6)*EXP(ETA(8))
   C50=THETA(7)*EXP(ETA(6))
   TEQ=THETA(8)*EXP(ETA(7))

   KEQ=LN2/TEQ
   KA=LN2/TABS
   ALAG1=TLAG
   S2=V
   V2=V
   V3=V2*0.0001
   Q=V3*KEQ


$ERROR

   CP=A(2)/S2
   CE=A(3)/V3
   PCA=E0 + EMAX*CE/(C50+CE)
   IF (DVID.LE.1) THEN
      Y=CP*(1+ERR(1)) + ERR(2)
   ENDIF
   IF (DVID.EQ.2) THEN
      Y=PCA + ERR(3)
   ENDIF

$TABLE ID TIME DVID CP CE PCA Y
ETA1 ETA2 ETA3 ETA4 ETA5 ETA6 ETA7 ETA8
ONEHEADER NOPRINT FILE=pkpd.fit



