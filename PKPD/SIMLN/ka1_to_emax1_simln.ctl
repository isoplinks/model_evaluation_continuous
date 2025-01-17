$PROB Warfarin PKPD

;O'Reilly RA, Aggeler PM. Studies on coumarin anticoagulant drugs
;Initiation of warfarin therapy without a loading dose.
;Circulation 1968;38:169-177
;
;O'Reilly RA, Aggeler PM, Leong LS. Studies of the coumarin anticoagulant
;drugs: The pharmacodynamics of warfarin in man.
;Journal of Clinical Investigation 1963;42(10):1542-1551

$INPUT id time wt age sex dvid dv mdv evid cmt amt
$DATA ka1_to_emax1_simln.csv

$SIM (20080422) ONLYSIM NSUB=1

;$EST MAX=0 SIG=3 PRINT=1
;METHOD=COND INTER NOABORT

$THETA
(0.01,0.135,1) ; POP_CL L/h/70kg
(0.01,8.03,20) ; POP_V L/70kg
(0.01,0.588,24) ; POP_TABS h
(0.01,0.842,24) ; POP_LAG h
(0.01,96.6,200) ; POP_E0
-1 FIX ; POP_EMAX
(0.01,1.17,10) ; POP_C50
(0.01,13.,100) ; POP_TEQ

$OMEGA
0.0703 ; PPV_CL
0.0195 ; PPV_V
0.551 ; PPV_TABS
0.347 ; PPV_LAG
0.00286 ; PPV_E0
0 FIX ; PPV_EMAX
0.2 ; PPV_C50
0.0114 ; PPV_TEQ

$SIGMA
0.00677 ; RUV_CV
0.094 ; RUV_SD mg/L
14.6 ; RUV_FX

$SUBR ADVAN6 TOL=3
$MODEL
   COMP=GUT
   COMP=CENTRAL
   COMP=PCA
$PK

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

$TABLE id time wt age sex dvid dv mdv evid cmt amt
NOAPPEND ONEHEADER NOPRINT FILE=pkpd.fit



