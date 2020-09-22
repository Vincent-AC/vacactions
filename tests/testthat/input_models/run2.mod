;;Based on: 1
$PROBLEM  PHENOBARB SIMPLE MODEL
$INPUT  ID TIME AMT WGT APGR DV
$DATA  pheno.dta IGNORE=@
;;Based on: 1
$SUBROUTINE  ADVAN1 TRANS2
$PK

      TVCL=THETA(1)
      TVV=THETA(2)
      CL=TVCL*EXP(ETA(1))
      V=TVV*EXP(ETA(2))
      S1=V
$ERROR

      W=F
      Y=F+W*EPS(1)

      IPRED=F         ;  individual-specific prediction
      IRES=DV-IPRED   ;  individual-specific residual
      IWRES=IRES/W    ;  individual-specific weighted residual

$THETA  (0,0.1)    ; CL 

$THETA  (0,3)    ; V

$OMEGA            .2  ; IVCL
                  .5 ; IVV

$SIGMA            .06                         

$ESTIMATION  MAXEVALS=9997 SIGDIGITS=4 POSTHOC MSFO=phenomsf
$COVARIANCE  PRINT=E
$TABLE ID TIME DV NOPRINT ONEHEADER FILE=sdtab2
$TABLE ID TVCL TVV APGR NOPRINT ONEHEADER FILE=patab2