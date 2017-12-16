$PROB Warfarin PKPD

;O'Reilly RA, Aggeler PM. Studies on coumarin anticoagulant drugs
;Initiation of warfarin therapy without a loading dose.
;Circulation 1968;38:169-177
;
;O'Reilly RA, Aggeler PM, Leong LS. Studies of the coumarin anticoagulant
;drugs: The pharmacodynamics of warfarin in man.
;Journal of Clinical Investigation 1963;42(10):1542-1551

$INPUT id time wt age sex dvid dv mdv evid cmtx amt
$DATA ka1_to_emax1_simln.csv

;$EST MAX=9990 NSIG=3 SIGL=9 PRINT=1
;METHOD=COND INTER NOABORT
;$COV

$THETA
(0.01,0.131,1) FIX ; POP_CL L/h/70kg
(0.01,8.06,20) FIX ; POP_V L/70kg
(0.01,0.944,24) FIX ; POP_TABS h
(0.01,1.03,24) FIX ; POP_LAG h
$OMEGA
0.0649 FIX ; PPV_CL
0.0201 FIX ; PPV_V
0.447 FIX ; PPV_TABS
0.311 FIX ; PPV_LAG
$SIGMA 0.00478 FIX ; RUV_CV
$SIGMA 0.135 FIX ; RUV_SD mg/L

$THETA
(0.01,95.9,200) ; POP_E0
-1. FIX ; POP_EMAX
(0.01,1.17,10) ; POP_C50
(0.01,13.3,100) ; POP_TEQ
$OMEGA
0.00199 ; PPV_E0
0 FIX ; PPV_EMAX
0.136 ; PPV_C50
0.0143 ; PPV_TEQ
$SIGMA
13.9 ; RUV_FX

$SUBR ADVAN13 TOL=9
$MODEL
   COMP=GUT
   COMP=CENTRAL
   COMP=PCA
$PK
OBS=DV
   IF (NEWIND.EQ.0) LN2=LOG(2)

   FSZV=WT/70
   FSZCL=FSZV**0.75

   CL=FSZCL*POP_CL*EXP(PPV_CL)
   V=FSZV*POP_V*EXP(PPV_V)
   TABS=POP_TABS*EXP(PPV_TABS)
   TLAG=POP_LAG*EXP(PPV_LAG)

   E0=POP_E0*EXP(PPV_E0)
   EMAX=POP_EMAX*EXP(PPV_EMAX)
   C50=POP_C50*EXP(PPV_C50)
   TEQ=POP_TEQ*EXP(PPV_TEQ)

   KPCA=LN2/TEQ
   RPCA=E0*KPCA

   KA=LN2/TABS
   ALAG1=TLAG
   S2=V

   A_0(3)=E0
 
$DES
   RATEIN=KA*A(1)
   DCP=A(2)/S2
   DPCA=A(3)
   PD=1+EMAX*DCP/(C50+DCP)

   DADT(1)=-RATEIN
   DADT(2)=RATEIN - CL*DCP
   DADT(3)=RPCA*PD - KPCA*DPCA

$ERROR

   CP=A(2)/S2
   PCA=A(3)
   IF (DVID.LE.1) THEN
      Y=CP*(1+RUV_CV) + RUV_SD
   ENDIF
   IF (DVID.EQ.2) THEN
      Y=PCA + RUV_FX
   ENDIF
   CE=CP

; Simulation start
REP=IREP
$TABLE REP ID TIME DV PRED OBS MDV DVID WT AGE SEX
NOAPPEND ONEHEADER NOPRINT FILE=vpc.fit
$SIM (20150330) ONLYSIM NSUB=100
; Simulation end
