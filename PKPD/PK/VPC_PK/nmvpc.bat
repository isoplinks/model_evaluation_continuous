@echo off
if not '%1==' goto %1

:nonmem
rem set runNONMEM=y to execute nmgo for each model
set runNONMEM=y

rem Set list of models to be simulated e.g. set models=mdl1 mdl2 mdl3
set models=ka1 ka1_BLK

rem Names of variable in NONMEM table file to be used for the VPC x-axis
rem This can be used for evaluating continuous covariates e.g.
rem set xnames=TIME TAD WEIGHT for total time, time after dose and weight
set xnames=TIME WT AGE

rem Set list of names according to observation type e.g. set obsnames=CP PCA
set obsnames=CP

goto models

rem *************************
rem OBSERVATION TYPES SECTION
rem *************************
:obstype
rem Each observation type may have its own properties
rem The dvid variable is required to distinguish types

rem Define R script variables for each observation type
rem No spaces are allowed in variable values.
rem Use '#' which will be replaced by a blank in xlabel and ylabel values

rem **** COMMON ******
rem Variables common to all observation types
rem Any of these variables may be observation (obsname) specific

rem List of times for binning observed and predicted values

set logaxis=
set lloq=0

if '%xname%=='WT goto setWT
if '%xname%=='AGE goto setAGE

set bintimes=c(seq(0,10,1),seq(12,144,12))
set xlabel=Hour
set xmin=0
set xmax=144
set xtick=12
goto %obsname%

:setWT
set bintimes=c(seq(40,110,5))
set xlabel=WT#Kg
set xmin=40
set xmax=110
set xtick=10
goto %obsname%

:setAGE
set bintimes=c(seq(20,70,5))
set xlabel=AGE#y
set xmin=20
set xmax=70
set xtick=10
goto %obsname%

rem **** OBSERVATION SPECIFIC ******
rem user defined obsname labels identify variables for each observation type

:CP
set dvid=1
set ylabel=%obsname%#mg/L
set ymin=0
set ymax=20
set ytick=5

goto select

:PCA
set dvid=2
set ylabel=%obsname%#%
set ymin=0
set ymax=120
set ytick=20

goto select


rem ******************************
rem CATEGORICAL COVARIATES SECTION
rem ******************************

:covariate
rem Covariate selection is optional. For VPC without covariates: set covariates= 
rem Set list of categorical covariates (upto 3)  e.g. set covariates=SEX SIZE
rem Names in the covariates list must match exactly the names in the simulation table file
set covariates=SEX
set covariates=
rem Each covariate name must be matched with a list of numeric values for the covariate
rem which will be used to create VPCs for each value
rem select on covariate 1 e.g. sex values 0 1
set covlist1=0 1
rem select on covariate 2 e.g. size values 1 2
set covlist2=1 2
rem select on covariate 3 e.g. race values 1 2 3
set covlist3=

goto gotcov

rem **********************
rem MODELS SECTION
rem **********************
:models

rem Some miscellaneous variables that are rarely changed

rem Percentile range for prediction and confidence intervals
set PIpercentile=0.9
set CIpercentile=0.95
rem if isstd=y then create standard VPCs
set isstd=y
rem if ispc=y then create pred-corrected VPCs
set ispc=y
rem if isobs=y then show observation percentiles on VPCs
set isobs=y
rem if iscsv=y then write csv files with numerical values used for plots
set iscsv=n
rem if isbig=y then re-read simulation file each time to use less memory
set isbig=n
rem use this to scale TIME variable (e.g. timescale=52 to scale years to weeks)
set timescale=1
rem if hasmdv=y then use MDV data item to select valid observations otherwise all records are valid observations
set hasmdv=y
rem Name for MDV item for predictions. If blank then item name will be the same as for observations (MDV).
set mdvpname=

rem name of R script for VPC (without extension)
set orgR=vpc
rem No user changes needed past here
set rscript=%orgR%.R
set _tmpR=tmp_%orgR%

rem Loop through Models
for %%m in (%models%) do call %0 mdl %%m
goto over

:mdl
rem Run NONMEM simulation 
set model=%2%NMCTL%

rem R must read the fit file
set isnew=y
@echo on
if '%runNONMEM%=='y call nmgo %model%
@echo off
if exist %_tmpR%.R del %_tmpR%.R

rem Loop through VPCs

for %%x in (%xnames%) do call %0 xname %%x

rem Run R to create VPC plot
rem R.exe must be in the environment PATH
@echo on
R CMD BATCH %_tmpR%.R
@echo off
rem clean up after R (%_tmpR%.R)
if '%nmclean%==' for %%r in ( %_tmpRargs% .Rdata) do if exist %%r del %%r
goto over

:xname
set xname=%2
for %%v in (%obsnames%) do call %0 obstypes %%v
goto over

:obstypes
set obsname=%2
goto obstype

:select
set figdir=vpc_%obsname%
set orgdir=%figdir%
set pdftxt=%obsname%_%xname%_%xmax%
rem @echo on
rem Get user specified list of covariates
goto covariate
:gotcov
rem loop over covariates
set select=
set covset=
for %%c in (%covariates%) do call %0 covselect %%c
if '%select%==' goto makeR
goto over

:covselect
rem Upto three covariate sets are supported
set covname=%2
set figdir=%orgdir%_%covname%
if '%covset%=='3 set covset=
if '%covset%=='2 set covset=3
if '%covset%=='1 set covset=2
if '%covset%==' set covset=1
goto loopcov%covset%

:loopcov1
rem loop over covariate 1 e.g. sex values 0 1
rem User must set the values in this list:
for %%t in (%covlist1%) do call %0 covnum %%t
rem pause 
goto over

:loopcov2
rem loop over covariate 2 e.g. size group 1 2
rem User must set the values in this list:
for %%t in (%covlist2%) do call %0 covnum %%t
rem pause 
goto over

:loopcov3
rem loop over covariate 3 e.g. race type 1 2 3
rem User must set the values in this list:
for %%t in (%covlist3%) do call %0 covnum %%t
rem pause 
goto over

:covnum
set covnum=%2
rem creates select variable of the form SEX==1 when covname=SEX and covnum=1
set select=%covname%==%covnum%
set pdftxt=%obsname%_%covname%%covnum%
goto makeR

:makeR
if not '%figdir%==' if not '%figdir%=='. if not exist %figdir%.pdf\nul md %figdir%.pdf
set _tmpRargs=_tmp_VPC_args.txt
echo %wfnhome%>%_tmpRargs%
rem Edit R script to pass argeuments from nmvpc.bat
@echo on
%nmawk% -f nmvpc2R.awk -v ARGS=%_tmpRargs% -v NMDIR=%NMDIR% -v NMTBL=%NMTBL% -v DVID=%dvid% -v MODEL=%model% -v NMCTL=%NMCTL% -v OBSFILE=%obsfile% -v SELECT=%select% -v PDFTXT=%pdftxt% -v PIPCT=%PIpercentile% -v CIPCT=%CIpercentile% -v LOGAXIS=%logaxis% -v MDVPNAME=%mdvpname% -v HASMDV=%hasmdv% -v LLOQ=%lloq% -v ISNEW=%isnew% -v ISSTD=%isstd% -v ISPC=%ispc% -v ISOBS=%isobs% -v ISCSV=%iscsv%  -v ISBIG=%isbig% -v TIMESCALE=%timescale% -v TIMENAME=%xname% -v YMIN=%ymin% -v YMAX=%ymax% -v YTICK=%ytick% -v YLABEL=%ylabel% -v XMIN=%xmin% -v XMAX=%xmax% -v XTICK=%xtick% -v XLABEL=%xlabel% -v BINTIMES=%bintimes% -v FIGDIR=%figdir% %rscript% >> %_tmpR%.R
@echo off
rem No need for R to re-read fit file
set isnew=n
rem Return to end of obsnames loop
goto over

:over

