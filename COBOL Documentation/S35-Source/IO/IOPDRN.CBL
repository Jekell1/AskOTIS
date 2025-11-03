       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOPDRN.
       AUTHOR.        BAH
       DATE-WRITTEN.  04-15-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOPDRN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : PDFILE 
      * TABLE DESC    : SCRATCH PAD FILE
      * OUTPUT FILE   : XPC/IOPDRN.PRE
      * USED FOR      : LIBLP/LPWSPD.CPY  --- QPD-REC
      *                 LIBLP/LPPD1R.CPY  --- READ
      *                 LIBLP/LPPD1RN.CPY --- READ/READ-NEXT/START
      *                 LIBLP/LPLP2RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0415 ***NEW***
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)IO/IOPDRN.CBL S34 09/08/21-14:30:07 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".

      *=================================================================
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LPWSPD.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QPD1-WHERE-BEG-KEY.
           03  QPD1-WBEG-ID               PIC X.
           03  QPD1-WBEG-BRANCH           PIC 9(04).
           03  QPD1-WBEG-SOC-SEC          PIC 9(10).
 
       01  QPD1-WHERE-END-KEY.
           03  QPD1-WEND-ID               PIC X.
           03  QPD1-WEND-BRANCH           PIC 9(04).
           03  QPD1-WEND-SOC-SEC          PIC 9(10).

       01  QPD2-WHERE-BEG-KEY.
           03  QPD2-WBEG-ID               PIC X.
           03  QPD2-WBEG-BRANCH           PIC 9(04).
           03  QPD2-WBEG-BWNAME           PIC X(33).
 
       01  QPD2-WHERE-END-KEY.
           03  QPD2-WEND-ID               PIC X.
           03  QPD2-WEND-BRANCH           PIC 9(04).
           03  QPD2-WEND-BWNAME           PIC X(33).
 
       01  QPD-REC.
           03  QPD-ID               PIC X.
           03  QPD-BRANCH           PIC 9(4).
           03  QPD-SOC-SEC          PIC 9(10).
           03  QPD-OWNBR            PIC 9(4).
           03  QPD-ACCTNO           PIC 9(8).
           03  QPD-CLASS            PIC 9(3).
           03  QPD-SUBCLASS         PIC 99.
           03  QPD-LAWCODE          PIC 99.
           03  QPD-SSNO1            PIC 9(9).
           03  QPD-MAKERCD1         PIC X.
           03  QPD-SSNO2            PIC 9(9).
           03  QPD-MAKERCD2         PIC X.
           03  QPD-SSNO3            PIC 9(9).
           03  QPD-MAKERCD3         PIC X.
           03  QPD-SSNO4            PIC 9(9).
           03  QPD-MAKERCD4         PIC X.
           03  QPD-ORGST            PIC XX.
           03  QPD-ORGBR            PIC 9(4).
           03  QPD-OWNST            PIC XX.
           03  QPD-SPRCLASS         PIC 9(3).
           03  QPD-CONVERCD         PIC X.
           03  QPD-MIDSTREAM        PIC X.
           03  QPD-PURCD            PIC XXX.
           03  QPD-SRCD             PIC XX.
           03  QPD-COLLCD           PIC XX.
           03  QPD-COLLID           PIC X(20).
           03  QPD-POCD             PIC XX.
           03  QPD-PLCD             PIC X.
           03  QPD-PLTYPE           PIC X.
           03  QPD-CONFIDENTCD      PIC X.
           03  QPD-ALLOTCD          PIC X.
           03  QPD-RECASTCD         PIC X.
           03  QPD-SKIP-TRW         PIC X.
           03  QPD-STATUSFG         PIC X.
           03  QPD-SMALLOAN         PIC X.
           03  QPD-SALEFIN          PIC X.
           03  QPD-SERVACCT         PIC X.
           03  QPD-LOANTYPE         PIC X.
           03  QPD-ADDON-FG         PIC X.
           03  QPD-PAY-SCHLD-FG     PIC X.
           03  QPD-LC-CODE          PIC 99.
           03  QPD-CAN-LOAN-NO      PIC XX.
           03  QPD-STATFLAGS        PIC X(20).
           03  QPD-ENTDATE          PIC X(10).
           03  QPD-LOANDATE         PIC X(10).
           03  QPD-PURDATE          PIC X(10).
           03  QPD-INTDATE          PIC X(10).
           03  QPD-CASHDATE         PIC X(10).
           03  QPD-FILEDATE         PIC X(10).
           03  QPD-RESCISSION-DATE    PIC X(10).
           03  QPD-PART-RECOURSE-DATE PIC X(10).
           03  QPD-ORIG-1STPYDATE     PIC X(10).
           03  QPD-1STPYDATE        PIC X(10).
           03  QPD-JDDATE           PIC X(10).
           03  QPD-PLDATE           PIC X(10).
           03  QPD-PL-TRDATE        PIC X(10).
           03  QPD-ACCELDATE        PIC X(10).
           03  QPD-REDUDATE         PIC X(10).
           03  QPD-POFFDATE         PIC X(10).
           03  QPD-PO-TRDATE        PIC X(10).
           03  QPD-LEGALDATE        PIC X(10).
           03  QPD-BNKRPTDATE       PIC X(10).
           03  QPD-BNKRPT-DISMISS-DATE   PIC X(10).
           03  QPD-BNKRPT-DISCHARGE-DATE PIC X(10).
           03  QPD-BNKRPT-REAFFIRM-DATE  PIC X(10).
           03  QPD-DEBTAIDATE            PIC X(10).
           03  QPD-REPODATE              PIC X(10).
           03  QPD-REDEEMDATE            PIC X(10).
           03  QPD-SOLDDATE              PIC X(10).
           03  QPD-INS-CONFIRM-DATE      PIC X(10).
           03  QPD-PRIOR-RN-FOR10-DATE   PIC X(10).
           03  QPD-LAST-RN-FOR10-DATE    PIC X(10).
           03  QPD-TRW-DOFD-DATE         PIC X(10).
           03  QPD-LAST-PYPA-DATE        PIC X(10).
           03  QPD-BT-TRDATE             PIC X(10).
           03  QPD-METRO2-LAST-PMT-DATE  PIC X(10).
           03  QPD-FOLLOWUP-DATE         PIC X(10).
           03  QPD-LAST-ACCRUAL-DATE     PIC X(10).
           03  QPD-APRATE           PIC 999V999.
           03  QPD-EFFRATE          PIC 999V999.
           03  QPD-SMPRATE          PIC 999V999.
           03  QPD-YLDRATE          PIC 999V999.
           03  QPD-OVRDRATE         PIC 999V999.
           03  QPD-ENDORATE         PIC 999V999.
           03  QPD-JDRATE           PIC 999V999.
           03  QPD-ACCRATE          PIC 999V999.
           03  QPD-REDURATE         PIC 999V999.
           03  QPD-CONRATE          PIC 999V999.
           03  QPD-SERVPER          PIC 9(3)V99.
           03  QPD-SELLPR           PIC 9(7)V99.
           03  QPD-DOWNPY           PIC 9(7)V99.
           03  QPD-TRADEIN          PIC S9(7)V99.
           03  QPD-LNAMT            PIC 9(7)V99.
           03  QPD-FINCHG           PIC 9(7)V99.
           03  QPD-INTCHG           PIC 9(7)V99.
           03  QPD-SERCHG           PIC 9(5)V99.
           03  QPD-EXTCHG           PIC 9(5)V99.
           03  QPD-MAINTFEE         PIC 9(5)V99.
           03  QPD-POINTS           PIC 99V99.
           03  QPD-PROCEEDS         PIC S9(7)V99.
           03  QPD-ORGTERM          PIC 9(3).
           03  QPD-1STPYAMT         PIC 9(7)V99.
           03  QPD-NOREGPY          PIC 9(3).
           03  QPD-REGPYAMT         PIC 9(7)V99.
           03  QPD-LASTPYAMT        PIC S9(7)V99.
           03  QPD-PROCASH          PIC S9(7)V99.
           03  QPD-TILLNO           PIC 9(2).
           03  QPD-STATE-FLAG       PIC X.
           03  QPD-STATE-WORKER     PIC S9(7)V99.
           03  QPD-STATE-WORKER2    PIC S9(5)V99.
           03  QPD-FEECODE-1          PIC XX.
           03  QPD-FEEAMT-1           PIC 9(5)V99.
           03  QPD-FEEOURS-1          PIC X.
           03  QPD-FEEFINCHG-1        PIC X.
           03  QPD-FEECODE-2          PIC XX.
           03  QPD-FEEAMT-2           PIC 9(5)V99.
           03  QPD-FEEOURS-2          PIC X.
           03  QPD-FEEFINCHG-2        PIC X.
           03  QPD-FEECODE-3          PIC XX.
           03  QPD-FEEAMT-3           PIC 9(5)V99.
           03  QPD-FEEOURS-3          PIC X.
           03  QPD-FEEFINCHG-3        PIC X.
           03  QPD-FEECODE-4          PIC XX.
           03  QPD-FEEAMT-4           PIC 9(5)V99.
           03  QPD-FEEOURS-4          PIC X.
           03  QPD-FEEFINCHG-4        PIC X.
           03  QPD-FEECODE-5          PIC XX.
           03  QPD-FEEAMT-5           PIC 9(5)V99.
           03  QPD-FEEOURS-5          PIC X.
           03  QPD-FEEFINCHG-5        PIC X.
           03  QPD-FEECODE-6          PIC XX.
           03  QPD-FEEAMT-6           PIC 9(5)V99.
           03  QPD-FEEOURS-6          PIC X.
           03  QPD-FEEFINCHG-6        PIC X.
           03  QPD-FEECODE-7          PIC XX.
           03  QPD-FEEAMT-7           PIC 9(5)V99.
           03  QPD-FEEOURS-7          PIC X.
           03  QPD-FEEFINCHG-7        PIC X.
           03  QPD-FEECODE-8          PIC XX.
           03  QPD-FEEAMT-8           PIC 9(5)V99.
           03  QPD-FEEOURS-8          PIC X.
           03  QPD-FEEFINCHG-8        PIC X.
           03  QPD-FEECODE-9          PIC XX.
           03  QPD-FEEAMT-9           PIC 9(5)V99.
           03  QPD-FEEOURS-9          PIC X.
           03  QPD-FEEFINCHG-9        PIC X.
           03  QPD-FEECODE-10         PIC XX.
           03  QPD-FEEAMT-10          PIC 9(5)V99.
           03  QPD-FEEOURS-10         PIC X.
           03  QPD-FEEFINCHG-10       PIC X.
           03  QPD-FEECODE-11          PIC XX.
           03  QPD-FEEAMT-11           PIC 9(5)V99.
           03  QPD-FEEOURS-11          PIC X.
           03  QPD-FEEFINCHG-11        PIC X.
           03  QPD-FEECODE-12          PIC XX.
           03  QPD-FEEAMT-12           PIC 9(5)V99.
           03  QPD-FEEOURS-12          PIC X.
           03  QPD-FEEFINCHG-12        PIC X.
           03  QPD-FEECODE-13          PIC XX.
           03  QPD-FEEAMT-13           PIC 9(5)V99.
           03  QPD-FEEOURS-13          PIC X.
           03  QPD-FEEFINCHG-13        PIC X.
           03  QPD-FEECODE-14          PIC XX.
           03  QPD-FEEAMT-14           PIC 9(5)V99.
           03  QPD-FEEOURS-14          PIC X.
           03  QPD-FEEFINCHG-14        PIC X.
           03  QPD-FEECODE-15          PIC XX.
           03  QPD-FEEAMT-15           PIC 9(5)V99.
           03  QPD-FEEOURS-15          PIC X.
           03  QPD-FEEFINCHG-15        PIC X.
           03  QPD-FEECODE-16          PIC XX.
           03  QPD-FEEAMT-16           PIC 9(5)V99.
           03  QPD-FEEOURS-16          PIC X.
           03  QPD-FEEFINCHG-16        PIC X.
           03  QPD-FEECODE-17          PIC XX.
           03  QPD-FEEAMT-17           PIC 9(5)V99.
           03  QPD-FEEOURS-17          PIC X.
           03  QPD-FEEFINCHG-17        PIC X.
           03  QPD-FEECODE-18          PIC XX.
           03  QPD-FEEAMT-18           PIC 9(5)V99.
           03  QPD-FEEOURS-18          PIC X.
           03  QPD-FEEFINCHG-18        PIC X.
           03  QPD-FEECODE-19          PIC XX.
           03  QPD-FEEAMT-19           PIC 9(5)V99.
           03  QPD-FEEOURS-19          PIC X.
           03  QPD-FEEFINCHG-19        PIC X.
           03  QPD-FEECODE-20         PIC XX.
           03  QPD-FEEAMT-20          PIC 9(5)V99.
           03  QPD-FEEOURS-20         PIC X.
           03  QPD-FEEFINCHG-20       PIC X.
           03  QPD-INSURED      PIC X.
           03  QPD-INSTYPES     PIC X.
           03  QPD-INSELIGIBLE  PIC X.
           03  QPD-INSCOMP-1         PIC 99.
           03  QPD-INSOURS-1         PIC X.
           03  QPD-INSEFF-DATE-1     PIC X(10).
           03  QPD-INSEXP-DATE-1     PIC X(10).
           03  QPD-INSPREM-1         PIC 9(5)V99.
           03  QPD-INSCOVR-1         PIC 9(7)V99.
           03  QPD-INSCOMM-1         PIC 9(5)V99.
           03  QPD-INSANTICERN-1     PIC S9(7)V99.
           03  QPD-INSANTICADJ-1     PIC S9(7)V99.
           03  QPD-INSUFERN-1        PIC S9(7)V99.
           03  QPD-INSACCERN-1       PIC S9(7)V99.
           03  QPD-INSEARN-ORDER-1   PIC 99.
           03  QPD-INSFINCHG-1       PIC X.
           03  QPD-INSCOMP-2         PIC 99.
           03  QPD-INSOURS-2         PIC X.
           03  QPD-INSEFF-DATE-2     PIC X(10).
           03  QPD-INSEXP-DATE-2     PIC X(10).
           03  QPD-INSPREM-2         PIC 9(5)V99.
           03  QPD-INSCOVR-2         PIC 9(7)V99.
           03  QPD-INSCOMM-2         PIC 9(5)V99.
           03  QPD-INSANTICERN-2     PIC S9(7)V99.
           03  QPD-INSANTICADJ-2     PIC S9(7)V99.
           03  QPD-INSUFERN-2        PIC S9(7)V99.
           03  QPD-INSACCERN-2       PIC S9(7)V99.
           03  QPD-INSEARN-ORDER-2   PIC 99.
           03  QPD-INSFINCHG-2       PIC X.
           03  QPD-INSCOMP-3         PIC 99.
           03  QPD-INSOURS-3         PIC X.
           03  QPD-INSEFF-DATE-3     PIC X(10).
           03  QPD-INSEXP-DATE-3     PIC X(10).
           03  QPD-INSPREM-3         PIC 9(5)V99.
           03  QPD-INSCOVR-3         PIC 9(7)V99.
           03  QPD-INSCOMM-3         PIC 9(5)V99.
           03  QPD-INSANTICERN-3     PIC S9(7)V99.
           03  QPD-INSANTICADJ-3     PIC S9(7)V99.
           03  QPD-INSUFERN-3        PIC S9(7)V99.
           03  QPD-INSACCERN-3       PIC S9(7)V99.
           03  QPD-INSEARN-ORDER-3   PIC 99.
           03  QPD-INSFINCHG-3       PIC X.
           03  QPD-INSCOMP-4         PIC 99.
           03  QPD-INSOURS-4         PIC X.
           03  QPD-INSEFF-DATE-4     PIC X(10).
           03  QPD-INSEXP-DATE-4     PIC X(10).
           03  QPD-INSPREM-4         PIC 9(5)V99.
           03  QPD-INSCOVR-4         PIC 9(7)V99.
           03  QPD-INSCOMM-4         PIC 9(5)V99.
           03  QPD-INSANTICERN-4     PIC S9(7)V99.
           03  QPD-INSANTICADJ-4     PIC S9(7)V99.
           03  QPD-INSUFERN-4        PIC S9(7)V99.
           03  QPD-INSACCERN-4       PIC S9(7)V99.
           03  QPD-INSEARN-ORDER-4   PIC 99.
           03  QPD-INSFINCHG-4       PIC X.
           03  QPD-INSCOMP-5         PIC 99.
           03  QPD-INSOURS-5         PIC X.
           03  QPD-INSEFF-DATE-5     PIC X(10).
           03  QPD-INSEXP-DATE-5     PIC X(10).
           03  QPD-INSPREM-5         PIC 9(5)V99.
           03  QPD-INSCOVR-5         PIC 9(7)V99.
           03  QPD-INSCOMM-5         PIC 9(5)V99.
           03  QPD-INSANTICERN-5     PIC S9(7)V99.
           03  QPD-INSANTICADJ-5     PIC S9(7)V99.
           03  QPD-INSUFERN-5        PIC S9(7)V99.
           03  QPD-INSACCERN-5       PIC S9(7)V99.
           03  QPD-INSEARN-ORDER-5   PIC 99.
           03  QPD-INSFINCHG-5       PIC X.
           03  QPD-INSCOMP-6         PIC 99.
           03  QPD-INSOURS-6         PIC X.
           03  QPD-INSEFF-DATE-6     PIC X(10).
           03  QPD-INSEXP-DATE-6     PIC X(10).
           03  QPD-INSPREM-6         PIC 9(5)V99.
           03  QPD-INSCOVR-6         PIC 9(7)V99.
           03  QPD-INSCOMM-6         PIC 9(5)V99.
           03  QPD-INSANTICERN-6     PIC S9(7)V99.
           03  QPD-INSANTICADJ-6     PIC S9(7)V99.
           03  QPD-INSUFERN-6        PIC S9(7)V99.
           03  QPD-INSACCERN-6       PIC S9(7)V99.
           03  QPD-INSEARN-ORDER-6   PIC 99.
           03  QPD-INSFINCHG-6       PIC X.
           03  QPD-INSCOMP-7         PIC 99.
           03  QPD-INSOURS-7         PIC X.
           03  QPD-INSEFF-DATE-7     PIC X(10).
           03  QPD-INSEXP-DATE-7     PIC X(10).
           03  QPD-INSPREM-7         PIC 9(5)V99.
           03  QPD-INSCOVR-7         PIC 9(7)V99.
           03  QPD-INSCOMM-7         PIC 9(5)V99.
           03  QPD-INSANTICERN-7     PIC S9(7)V99.
           03  QPD-INSANTICADJ-7     PIC S9(7)V99.
           03  QPD-INSUFERN-7        PIC S9(7)V99.
           03  QPD-INSACCERN-7       PIC S9(7)V99.
           03  QPD-INSEARN-ORDER-7   PIC 99.
           03  QPD-INSFINCHG-7       PIC X.
           03  QPD-INSCOMP-8         PIC 99.
           03  QPD-INSOURS-8         PIC X.
           03  QPD-INSEFF-DATE-8     PIC X(10).
           03  QPD-INSEXP-DATE-8     PIC X(10).
           03  QPD-INSPREM-8         PIC 9(5)V99.
           03  QPD-INSCOVR-8         PIC 9(7)V99.
           03  QPD-INSCOMM-8         PIC 9(5)V99.
           03  QPD-INSANTICERN-8     PIC S9(7)V99.
           03  QPD-INSANTICADJ-8     PIC S9(7)V99.
           03  QPD-INSUFERN-8        PIC S9(7)V99.
           03  QPD-INSACCERN-8       PIC S9(7)V99.
           03  QPD-INSEARN-ORDER-8   PIC 99.
           03  QPD-INSFINCHG-8       PIC X.
           03  QPD-INSCOMP-9         PIC 99.
           03  QPD-INSOURS-9         PIC X.
           03  QPD-INSEFF-DATE-9     PIC X(10).
           03  QPD-INSEXP-DATE-9     PIC X(10).
           03  QPD-INSPREM-9         PIC 9(5)V99.
           03  QPD-INSCOVR-9         PIC 9(7)V99.
           03  QPD-INSCOMM-9         PIC 9(5)V99.
           03  QPD-INSANTICERN-9     PIC S9(7)V99.
           03  QPD-INSANTICADJ-9     PIC S9(7)V99.
           03  QPD-INSUFERN-9        PIC S9(7)V99.
           03  QPD-INSACCERN-9       PIC S9(7)V99.
           03  QPD-INSEARN-ORDER-9   PIC 99.
           03  QPD-INSFINCHG-9       PIC X.
           03  QPD-INSCOMP-10        PIC 99.
           03  QPD-INSOURS-10        PIC X.
           03  QPD-INSEFF-DATE-10    PIC X(10).
           03  QPD-INSEXP-DATE-10    PIC X(10).
           03  QPD-INSPREM-10        PIC 9(5)V99.
           03  QPD-INSCOVR-10        PIC 9(7)V99.
           03  QPD-INSCOMM-10        PIC 9(5)V99.
           03  QPD-INSANTICERN-10    PIC S9(7)V99.
           03  QPD-INSANTICADJ-10    PIC S9(7)V99.
           03  QPD-INSUFERN-10       PIC S9(7)V99.
           03  QPD-INSACCERN-10      PIC S9(7)V99.
           03  QPD-INSEARN-ORDER-10  PIC 99.
           03  QPD-INSFINCHG-10      PIC X.
           03  QPD-PY-YYYYMM-RECDEL    PIC 9(6).
           03  QPD-MTD-PAYS-RECDEL     PIC S9(7)V99.
           03  QPD-GARNISH-EXTR        PIC X.
           03  QPD-SHIP-SAME-FG        PIC X.
           03  QPD-AD-D-INS-COVR       PIC 9(7).
           03  QPD-TOT-PARTIAL-CHGOFF  PIC S9(7)V99.
           03  QPD-LPO                 PIC X(4).
           03  QPD-REP-CD              PIC X(8).
           03  QPD-EMP-ID              PIC 9(8).
           03  QPD-PROCESS-CD          PIC X.
           03  QPD-WCBC-CREDITLIM      PIC S9(5).
           03  QPD-FOLLOWUP-CD         PIC X(4).
           03  QPD-MIP-FG              PIC X.
           03  QPD-MIP-MTD-CLAHPAID    PIC S9(5)V99.
           03  QPD-MIP-CLPDTH-DATE     PIC X(10).
           03  QPD-MIP-TOT-CLPAID      PIC S9(7)V99.
           03  QPD-MIP-AHPDTH-DATE     PIC X(10).
           03  QPD-MIP-TOT-AHPAID      PIC S9(7)V99.
           03  QPD-IL-GROSS-INCOME     PIC S9(7)V99.
           03  QPD-IL-GROSS-INCOME2    PIC S9(7)V99.
           03  QPD-RESCISSION          PIC X(1).
           03  QPD-INTERIM-TRW         PIC X(1).
           03  QPD-DLNO             PIC 9(4)V99.
           03  QPD-DLPOOL           PIC 9(5).
           03  QPD-DLRECOURSE       PIC X.
           03  QPD-DLPARTRCAMT      PIC 9(7)V99.
           03  QPD-ORIGPURAMT       PIC 9(7)V99.
           03  QPD-PURBAL           PIC S9(7)V99.
           03  QPD-DLRVPRIN-1       PIC 9(7)V99.
           03  QPD-DLRVPRIN-2       PIC 9(7)V99.
           03  QPD-DLRVPRIN-3       PIC 9(7)V99.
           03  QPD-DLRVCHGS-1       PIC 9(7)V99.
           03  QPD-DLRVCHGS-2       PIC 9(7)V99.
           03  QPD-DLRVCHGS-3       PIC 9(7)V99.
           03  QPD-DLPARVHELD       PIC 9(7)V99.
           03  QPD-DLPARVPAID       PIC 9(7)V99.
           03  QPD-DLDISC           PIC 9(7)V99.
           03  QPD-LCPDTH-DATE      PIC X(10).
           03  QPD-INTPDTH-DATE     PIC X(10).
           03  QPD-CURBAL           PIC S9(7)V99.
           03  QPD-INTBAL           PIC S9(7)V99.
           03  QPD-LCBAL            PIC S9(7)V99.
           03  QPD-OTHBAL           PIC S9(7)V99.
           03  QPD-OT2BAL           PIC S9(7)V99.
           03  QPD-LCPAID           PIC S9(7)V99.
           03  QPD-LCPARTIALS       PIC S9(7)V99.
           03  QPD-STMT-COUNTER     PIC 9(5).
           03  QPD-PRIOR-STMT-COUNTER PIC 9(5).
           03  QPD-FROZEN-REASON    PIC XX.
           03  QPD-COUPON-STATEMENT PIC X.
           03  QPD-MONTHEND-BAL     PIC 9(7)V99.
           03  QPD-ANTICTERM        PIC 9(3).
           03  QPD-ANTICERN-1     PIC S9(7)V99.
           03  QPD-ANTICADJ-1     PIC S9(7)V99.
           03  QPD-UFERN-1        PIC S9(7)V99.
           03  QPD-ACCERN-1       PIC S9(7)V99.
           03  QPD-EARN-ORDER-1   PIC 99.
           03  QPD-ANTICERN-2     PIC S9(7)V99.
           03  QPD-ANTICADJ-2     PIC S9(7)V99.
           03  QPD-UFERN-2        PIC S9(7)V99.
           03  QPD-ACCERN-2       PIC S9(7)V99.
           03  QPD-EARN-ORDER-2   PIC 99.
           03  QPD-ANTICERN-3     PIC S9(7)V99.
           03  QPD-ANTICADJ-3     PIC S9(7)V99.
           03  QPD-UFERN-3        PIC S9(7)V99.
           03  QPD-ACCERN-3       PIC S9(7)V99.
           03  QPD-EARN-ORDER-3   PIC 99.
           03  QPD-ANTICERN-4     PIC S9(7)V99.
           03  QPD-ANTICADJ-4     PIC S9(7)V99.
           03  QPD-UFERN-4        PIC S9(7)V99.
           03  QPD-ACCERN-4       PIC S9(7)V99.
           03  QPD-EARN-ORDER-4   PIC 99.
           03  QPD-ANTICERN-5     PIC S9(7)V99.
           03  QPD-ANTICADJ-5     PIC S9(7)V99.
           03  QPD-UFERN-5        PIC S9(7)V99.
           03  QPD-ACCERN-5       PIC S9(7)V99.
           03  QPD-EARN-ORDER-5   PIC 99.
           03  QPD-ANTICERN-6     PIC S9(7)V99.
           03  QPD-ANTICADJ-6     PIC S9(7)V99.
           03  QPD-UFERN-6        PIC S9(7)V99.
           03  QPD-ACCERN-6       PIC S9(7)V99.
           03  QPD-EARN-ORDER-6   PIC 99.
           03  QPD-ANTICERN-7     PIC S9(7)V99.
           03  QPD-ANTICADJ-7     PIC S9(7)V99.
           03  QPD-UFERN-7        PIC S9(7)V99.
           03  QPD-ACCERN-7       PIC S9(7)V99.
           03  QPD-EARN-ORDER-7   PIC 99.
           03  QPD-ANTICERN-8     PIC S9(7)V99.
           03  QPD-ANTICADJ-8     PIC S9(7)V99.
           03  QPD-UFERN-8        PIC S9(7)V99.
           03  QPD-ACCERN-8       PIC S9(7)V99.
           03  QPD-EARN-ORDER-8   PIC 99.
           03  QPD-ANTICERN-9     PIC S9(7)V99.
           03  QPD-ANTICADJ-9     PIC S9(7)V99.
           03  QPD-UFERN-9        PIC S9(7)V99.
           03  QPD-ACCERN-9       PIC S9(7)V99.
           03  QPD-EARN-ORDER-9   PIC 99.
           03  QPD-ANTICERN-10    PIC S9(7)V99.
           03  QPD-ANTICADJ-10    PIC S9(7)V99.
           03  QPD-UFERN-10       PIC S9(7)V99.
           03  QPD-ACCERN-10      PIC S9(7)V99.
           03  QPD-EARN-ORDER-10  PIC 99.
           03  QPD-LMO-PRIOR-INTEREST PIC S9(7)V99.
           03  QPD-PRIOR-UNEARNED     PIC S9(7)V99.
           03  QPD-ERNDELQ-STOP       PIC X.
           03  QPD-NONACRERN          PIC S9(7)V99.
           03  QPD-PRIOR-NONACRERN    PIC S9(7)V99.
           03  QPD-ERN-IB-PDTH-DATE   PIC X(10).
           03  QPD-ERN-IB-PRINBAL     PIC S9(7)V99.
           03  QPD-ERN-IB-INTOWE      PIC S9(7)V99.
           03  QPD-TOTERN             PIC S9(7)V99.
           03  QPD-TOTOTH             PIC S9(7)V99.
           03  QPD-TOTINT             PIC S9(7)V99.
           03  QPD-TOTLCHG          PIC S9(7)V99.
           03  QPD-TOTDEF           PIC S9(7)V99.
           03  QPD-TOTSERACCT       PIC S9(7)V99.
           03  QPD-LCWAIVERS        PIC S9(5)V99.
           03  QPD-INTWAIVERS       PIC S9(5)V99.
           03  QPD-TOTPAYMNTD       PIC S9(7)V99.
           03  QPD-TOTEXCPAYMNTD    PIC S9(7)V99.
           03  QPD-TOTNODEF         PIC S99.
           03  QPD-YTDNODEF         PIC S99.
           03  QPD-TOTNODEFACT      PIC S99.
           03  QPD-YTDNODEFACT      PIC S99.
           03  QPD-TOTNOALDEL       PIC S99.
           03  QPD-YTDNOALDEL       PIC S99.
           03  QPD-YTDINTPD         PIC S9(7)V99.
           03  QPD-LYRINTPD         PIC S9(7)V99.
           03  QPD-TOTINTPD         PIC S9(7)V99.
           03  QPD-RECASTINT        PIC 9(7)V99.
           03  QPD-RECASTPRIN       PIC 9(7)V99.
           03  QPD-CL-REBATE    PIC X.
           03  QPD-AH-REBATE    PIC X.
           03  QPD-PP-REBATE    PIC X.
           03  QPD-INT-REBATE   PIC X.
           03  QPD-SC-REBATE    PIC X.
           03  QPD-MF-REBATE    PIC X.
           03  QPD-DF-REBATE    PIC X.
           03  QPD-EX-REBATE    PIC X.
           03  QPD-O1-REBATE    PIC X.
           03  QPD-O2-REBATE    PIC X.
           03  QPD-O3-REBATE    PIC X.
           03  QPD-O4-REBATE    PIC X.
           03  QPD-O5-REBATE    PIC X.
           03  QPD-O6-REBATE    PIC X.
           03  QPD-O7-REBATE    PIC X.
           03  QPD-UN-O7-REB    PIC X.
           03  QPD-UN-O8-REB    PIC X.
           03  QPD-UN-O9-REB    PIC X.
           03  QPD-UN-O10-REB   PIC X.
           03  QPD-PLREASON         PIC XX.
           03  QPD-COLLECTOR        PIC X(3).
           03  QPD-ACTIONCD         PIC X(4).
           03  QPD-ACTIONCD2        PIC X(4).
           03  QPD-ATCODE           PIC X(3).
           03  QPD-RECODE           PIC X(3).
           03  QPD-ESCROW-FG        PIC X.
           03  QPD-PROMDAYS-PASTDUE PIC S9(3).
           03  QPD-PROMID           PIC X(3).
           03  QPD-PROMDATE         PIC X(10).
           03  QPD-PROMAMT          PIC 9(7)V99.
           03  QPD-PROMDATE2        PIC X(10).
           03  QPD-DATE-PAID-LAST   PIC X(10).
           03  QPD-AMT-PAID-LAST    PIC 9(7)V99.
           03  QPD-LASTCFORMNO      PIC 9(5).
           03  QPD-LASTCFORMDATE    PIC X(10).
           03  QPD-DELCNT           PIC 99.
           03  QPD-DELCNT2          PIC 99.
           03  QPD-CREDITLIM        PIC 9(7).
           03  QPD-CRSCORE          PIC S9(3).
           03  QPD-LASTSFORMNO      PIC 9(5).
           03  QPD-LASTSFORMDATE    PIC X(10).
           03  QPD-SOLCNT           PIC 99.
           03  QPD-MESSAGE          PIC X(30).
           03  QPD-OLDACCTNO        PIC 9(8).
           03  QPD-PRLNNO           PIC 9(8).
           03  QPD-PRBAL            PIC S9(7)V99.
           03  QPD-PRINTDUE         PIC S9(7)V99.
           03  QPD-PRFINCHGRB       PIC S9(7)V99.
           03  QPD-PRINSRB          PIC S9(7)V99.
           03  QPD-PRPOCD           PIC XX.
           03  QPD-PRPODATE         PIC X(10).
           03  QPD-PRDELINQAMT      PIC S9(7)V99.
           03  QPD-PRLNAMT          PIC 9(7)V99.
           03  QPD-PRPDUE-DAYS      PIC S9(5).
           03  QPD-PRDLDISC-UNEARN  PIC S9(7)V99.
           03  QPD-PRCONPROFILE     PIC 9(12).
           03  QPD-SUM-ORIGPURAMT   PIC 9(7)V99.
           03  QPD-SUM-LNAMT        PIC 9(7)V99.
           03  QPD-SUM-FINCHG       PIC 9(7)V99.
           03  QPD-SUM-INTCHG       PIC 9(7)V99.
           03  QPD-SUM-ORGTERM      PIC 9(3).
           03  QPD-SUM-REGPYAMT     PIC 9(7)V99.
           03  QPD-SUM-EXTCHG       PIC 9(5)V99.
           03  QPD-SUM-SERCHG       PIC 9(5)V99.
           03  QPD-SUM-MAINTFEE     PIC 9(7)V99.
           03  QPD-UNITPER-CD       PIC X.
           03  QPD-UNITPER-FREQ     PIC 999.
           03  QPD-DAYS-IN-UNITPER   PIC 9(3).
           03  QPD-UNITPER-PER-YEAR  PIC 9(3)V9(12).
           03  QPD-REPOCD            PIC X.
           03  QPD-BR-TFR-FG         PIC X.
           03  QPD-O3INS-DLPROC      PIC 9(7)V99.
           03  QPD-INSPREM-DLADJ     PIC S9(5)V99.
           03  QPD-PLAMT             PIC 9(7)V99.
           03  QPD-MTD-PRIN          PIC S9(7)V99.
           03  QPD-MTD-LC            PIC S9(5)V99.
           03  QPD-MTD-INT           PIC S9(5)V99.
           03  QPD-MTD-OT            PIC S9(5)V99.
           03  QPD-MTD-EX            PIC S9(7)V99.
           03  QPD-BNKRPT-CHAPTER    PIC 9(2).
           03  QPD-BMO-CON-STATUS    PIC X.
           03  QPD-BMO-REC-STATUS    PIC X.
           03  QPD-APP-NUMBER        PIC 9(09).
           03  QPD-GIFTNET           PIC X.
           03  QPD-GIFTNET-OUR-PO    PIC 9(7).
           03  QPD-PROC-DISBURSED    PIC X.
           03  QPD-NOODDPY           PIC 9.
           03  QPD-CONPROFILE-1      PIC 9(12).
           03  QPD-CONPROFILE-2      PIC 9(12).
           03  QPD-CONPROFILE-3      PIC 9(12).
           03  QPD-CONPROFILE-4      PIC 9(12).
           03  QPD-CONPROFILE-5      PIC 9(12).
           03  QPD-RECPROFILE-1      PIC 9(12).
           03  QPD-RECPROFILE-2      PIC 9(12).
           03  QPD-RECPROFILE-3      PIC 9(12).
           03  QPD-RECPROFILE-4      PIC 9(12).
           03  QPD-RECPROFILE-5      PIC 9(12).
           03  QPD-TRW-STATUS        PIC XX.
           03  QPD-ORIGIN            PIC X(4).
           03  QPD-PO-TRCD           PIC XX.
           03  QPD-NEXIS-CRSCORE      PIC 9(3).
           03  QPD-TRW-B-CONSUMER-IND PIC X(2).
           03  QPD-PARVHELD-PAID-FG   PIC X.
           03  QPD-ACCT-FROZEN        PIC X.
           03  QPD-TRW-SPECIAL-CODE   PIC X(2).
           03  QPD-CREATION-STATUS    PIC X(1).
           03  QPD-TOTNOLCHG                PIC S9(3).
           03  QPD-LBOX                     PIC X.
           03  QPD-EXTRA-PRIN-LFP           PIC S9(7)V99.
           03  QPD-REMAIN-INT-SINCE-LFP     PIC S9(7)V99.
           03  QPD-REMAIN-PRIN-SINCE-LFP    PIC S9(7)V99.
           03  QPD-INTERIM-COUPON           PIC X.
           03  QPD-DLCOMM-O3-CMSI           PIC 9(5)V99.
           03  QPD-DLCOMM-PP-CMSI           PIC 9(5)V99.
           03  QPD-TRW-J-CONSUMER-IND       PIC X(2).
           03  QPD-ACH                      PIC X.
           03  QPD-1099-C-FG                PIC X.
           03  QPD-TRW-COMPLIANCE-CODE      PIC XX.
           03  QPD-UNWIND-REASON-CODE       PIC X(2).
           03  QPD-PBI-PBD-FG               PIC X.
           03  QPD-OLD-ACCT-FORMAT-FG       PIC X.
           03  QPD-APPROVED-BY-USER         PIC X(10).
           03  QPD-APPROVED-BY-CODE         PIC X(3).
           03  QPD-APPROVED-BY-DATE         PIC X(10).
           03  QPD-APPROVED-BY-TIME         PIC 9(6).
           03  QPD-LTOUCH-DATE              PIC X(10).
           03  QPD-LTOUCH-TIME              PIC 9(6).
           03  QPD-HOLD-COLLATERAL      PIC X(60).
           03  QPD-DLPAID               PIC X.
           03  QPD-CKNO                 PIC 9(6).
           03  QPD-FREE1                PIC X(70).
           03  QPD-FREE2                PIC X(70).
           03  QPD-FREE-SPACE-FILLER    PIC X(44).
           03  QPD-POSTING-BR           PIC 9(4).
           03  QPD-OTHBR-FG             PIC X.
           03  QPD-APPROVAL-LIMIT       PIC 9(7).
           03  QPD-APPROVAL-USERID      PIC X(3).
           03  QPD-MIN-CRED-SCORE       PIC 9(4).
           03  QPD-CRED-SCORE-USERID    PIC X(3).
           03  QPD-FINAMT               PIC 9(7)V99.
           03  QPD-PROCEEDS-FG      PIC X.
           03  QPD-POINTS-FG        PIC X.
           03  QPD-OVRDRATE-FG      PIC X.
           03  QPD-INS-FG-1         PIC X.
           03  QPD-INS-FG-2         PIC X.
           03  QPD-INS-FG-3         PIC X.
           03  QPD-INS-FG-4         PIC X.
           03  QPD-INS-FG-5         PIC X.
           03  QPD-INS-FG-6         PIC X.
           03  QPD-INS-FG-7         PIC X.
           03  QPD-INS-FG-8         PIC X.
           03  QPD-INS-FG-9         PIC X.
           03  QPD-INS-FG-10        PIC X.
           03  QPD-SERCHG-FG        PIC X.
           03  QPD-MAINTFEE-FG      PIC X.
           03  QPD-EXTCHG-FG        PIC X.
           03  QPD-REGPYAMT-FG      PIC X.
           03  QPD-FEECD-1          PIC XX.
           03  QPD-FEECD-2          PIC XX.
           03  QPD-FEECD-3          PIC XX.
           03  QPD-FEECD-4          PIC XX.
           03  QPD-FEECD-5          PIC XX.
           03  QPD-FEECD-6          PIC XX.
           03  QPD-FEECD-7          PIC XX.
           03  QPD-FEECD-8          PIC XX.
           03  QPD-FEECD-9          PIC XX.
           03  QPD-FEECD-10         PIC XX.
           03  QPD-FEECD-11          PIC XX.
           03  QPD-FEECD-12          PIC XX.
           03  QPD-FEECD-13          PIC XX.
           03  QPD-FEECD-14          PIC XX.
           03  QPD-FEECD-15          PIC XX.
           03  QPD-FEECD-16          PIC XX.
           03  QPD-FEECD-17          PIC XX.
           03  QPD-FEECD-18          PIC XX.
           03  QPD-FEECD-19          PIC XX.
           03  QPD-FEECD-20          PIC XX.
           03  QPD-PRPAID-AHEAD     PIC S9(7)V99.
           03  QPD-RESTRICT-ADJ     PIC S9(7)V99.
           03  QPD-DUAL-LOAN-ADJ    PIC S9(7)V99.
           03  QPD-CAN-LOAN-MAX     PIC S9(7)V99.
           03  QPD-PERDOWN          PIC 999V99.
           03  QPD-SELLPR-FG        PIC X.
           03  QPD-DOWNPY-FG        PIC X.
           03  QPD-PERDOWN-FG       PIC X.
           03  QPD-SELL-DOWN-FG     PIC X.
           03  QPD-BALLOON-FG       PIC X.
           03  QPD-BALLOON-LAST-FG  PIC X.
           03  QPD-FEECD-FG-1        PIC X.
           03  QPD-FEECD-FG-2        PIC X.
           03  QPD-INS-TERM-SW-1    PIC X.
           03  QPD-COVERAGE-SW-1    PIC X.
           03  QPD-PREMIUM-SW-1     PIC X.
           03  QPD-INS-TERM-SW-2    PIC X.
           03  QPD-COVERAGE-SW-2    PIC X.
           03  QPD-PREMIUM-SW-2     PIC X.
           03  QPD-INS-TERM-SW-3    PIC X.
           03  QPD-COVERAGE-SW-3    PIC X.
           03  QPD-PREMIUM-SW-3     PIC X.
           03  QPD-INS-TERM-SW-4    PIC X.
           03  QPD-COVERAGE-SW-4    PIC X.
           03  QPD-PREMIUM-SW-4     PIC X.
           03  QPD-INS-TERM-SW-5    PIC X.
           03  QPD-COVERAGE-SW-5    PIC X.
           03  QPD-PREMIUM-SW-5     PIC X.
           03  QPD-INS-TERM-SW-6    PIC X.
           03  QPD-COVERAGE-SW-6    PIC X.
           03  QPD-PREMIUM-SW-6     PIC X.
           03  QPD-INS-TERM-SW-7    PIC X.
           03  QPD-COVERAGE-SW-7    PIC X.
           03  QPD-PREMIUM-SW-7     PIC X.
           03  QPD-INS-TERM-SW-8    PIC X.
           03  QPD-COVERAGE-SW-8    PIC X.
           03  QPD-PREMIUM-SW-8     PIC X.
           03  QPD-INS-TERM-SW-9    PIC X.
           03  QPD-COVERAGE-SW-9    PIC X.
           03  QPD-PREMIUM-SW-9     PIC X.
           03  QPD-INS-TERM-SW-10   PIC X.
           03  QPD-COVERAGE-SW-10   PIC X.
           03  QPD-PREMIUM-SW-10    PIC X.
           03  QPD-DLDISC-FG        PIC X.
           03  QPD-DLRVPRIN-FG-1    PIC X.
           03  QPD-DLRVPRIN-FG-2    PIC X.
           03  QPD-DLRVPRIN-FG-3    PIC X.
           03  QPD-DLRVCHGS-FG-1    PIC X.
           03  QPD-DLRVCHGS-FG-2    PIC X.
           03  QPD-DLRVCHGS-FG-3    PIC X.
           03  QPD-DLPARVHELD-FG    PIC X.
           03  QPD-DLPARVPAID-FG    PIC X.
           03  QPD-ONE-MTH-NO-DAY-EXT-SW        PIC X.
           03  QPD-ASSIGN-RN-ACCTNO             PIC X.
           03  QPD-APR-FINAMT        PIC S9(7)V99.
           03  QPD-DLCOMM-PP-CMSI-FG PIC X.
           03  QPD-DLCOMM-O3-CMSI-FG PIC X.
           03  QPD-FILLER-TO-MATCH-LN   PIC X(90).
           03  QPD2-ID                      PIC X.
           03  QPD2-BRANCH                  PIC 9(4).
           03  QPD2-BWNAME                  PIC X(33).
       EXEC SQL END DECLARE SECTION END-EXEC.
      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  RECORDS-FOUND               PIC X(01)     VALUE "X".
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".

       01  RECORDS-READ                PIC 9(11)     VALUE ZEROES.

       01  SUBSCRIPT-WORKERS.
           03  SUB                     PIC 9(09)     VALUE ZEROES.
           03  S1                      PIC 9(09)     VALUE ZEROES.
           03  S2                      PIC 9(09)     VALUE ZEROES.
           03  S3                      PIC 9(09)     VALUE ZEROES.

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(77)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP-ARROW, (".
           03  FILLER PIC X(30) VALUE "Y)ES OR (N)O TO ACCEPT        ".
           03  FILLER PIC X(17) VALUE "   ACCEPT (Y/N): ".

       01  LEGEND-PRINTER-OPTIONS.
           03  FILLER PIC X(30) VALUE "F7-EXIT, ENTER PRINTER OPTIONS".

       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING BRANCH NO. (F3-ALL) ".

       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING BRANCH NO. ".

       01  LEGEND-PROCESS-KEY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER NU".
           03  FILLER PIC X(30) VALUE "MBER OF FILE KEY TO PROCESS   ".

       01  LEGEND-BEG-END-RANGE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER RA".
           03  FILLER PIC X(30) VALUE "ANGE VALUE                    ".

      ******************************************************************
      *
      *    P A C E - W O R K E R S
      *
      ******************************************************************
       78  PACE-COUNTER-MAX                          VALUE 1000.
       77  PACE-COUNTER                PIC 9(6)      VALUE ZEROES.
       77  PACE-DISPLAY-MAX            PIC ZZZ,ZZ9   VALUE ZEROES.
       78  PACE-MAX                                  VALUE 15.
       77  PACE-LEV                    PIC 9(2)      VALUE ZEROES.

       01  PACE-HEAD-LINE.
           03  FILLER PIC X(28) VALUE "TIME        KEY             ".
           03  PACE-HEAD-MAX    PIC ZZZ,ZZ9 VALUE ZEROES.
           03  FILLER PIC X(05) VALUE " RECS".

       01  PACE-TIME.
           03  PACE-TIME-HH            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-MM            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-SS            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ".".
           03  PACE-TIME-HD            PIC 99.

       01  PACE-DETAIL.
           03  PACE-DETAIL-TIME        PIC 99/99/99/99.
           03  FILLER                  PIC X(1).
           03  PACE-DETAIL-KEY         PIC X(28).

       01  PACE-BUF.
           03  PACE-ELE                PIC X(40)     OCCURS PACE-MAX.

       01  PACE-LIST.
           03  FILLER PIC X(24) VALUE "PACE-01 PACE-02 PACE-03 ".
           03  FILLER PIC X(24) VALUE "PACE-04 PACE-05 PACE-06 ".
           03  FILLER PIC X(24) VALUE "PACE-07 PACE-08 PACE-09 ".
           03  FILLER PIC X(24) VALUE "PACE-10 PACE-11 PACE-12 ".
           03  FILLER PIC X(24) VALUE "PACE-13 PACE-14 PACE-15.".

      ******************************************************************
      *
      *    P A R A M E T E R - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  GROUP-FLAG              PIC X(01).
           03  ENT-GROUP               PIC X(04).

           03  PROCESS-KEY             PIC 9(01).
           03  BEG-ID                  PIC X.
           03  END-ID                  PIC X.
           03  BEG-SSNO                PIC 9(09).
           03  END-SSNO                PIC 9(09).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

       01  SAVE-SCREEN-RANGES.
           03  SV-BEG-BRANCH           PIC 9(04)     VALUE 9999.
           03  SV-END-BRANCH           PIC 9(04)     VALUE 9999.

           03  SV-GROUP-OPTION.
               05  SV-GROUP-FLAG       PIC X(01).
               05  SV-ENT-GROUP        PIC X(04).

      ******************************************************************
      *
      *    R E P O R T - W O R K E R S
      *
      ******************************************************************
       01  PRINT-LINE-WORKERS.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-FEED                 PIC 9(02)     VALUE ZEROES.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.
           03  PAGE-NUMBER             PIC 9(04)     VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.

      *=================================================================
       01  HEADER-LINE-01.
           03  H01-DATE           PIC X(08) VALUE SPACES.
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H01-HOUR           PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(01) VALUE ":".
           03  H01-MINUTE         PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(30) VALUE SPACES.
           03  H01-NAME           PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(37) VALUE SPACES.
           03  FILLER             PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE           PIC 9(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER             PIC X(10) VALUE "USER ID:".
           03  H02-USERID         PIC X(10) VALUE SPACES.
           03  H02-TITLE          PIC X(92) VALUE SPACES.
           03  FILLER             PIC X(20) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-03.
           03  FILLER PIC X(30) VALUE "BRNO  ID    SSNO       CLASS S".
           03  FILLER PIC X(30) VALUE "RCD    LNAMT".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-PD-BRANCH                  PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-PD-ID                      PIC X.
           03  FILLER                         PIC X(03).
           03  D01-PD-SOC-SEC                 PIC 999/99/9999.
           03  FILLER                         PIC X(03).
           03  D01-PD-CLASS                   PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-PD-SRCD                    PIC X(02).
           03  FILLER                         PIC X(03).
           03  D01-PD-LNAMT                   PIC Z(6)9.99.

      *-------------------------------------------------------{ RANGES }
       01  RANGE-LINE-01                      REDEFINES DETAIL-LINE.
           03  R01-PROMPT                     PIC X(32).
           03  R01-FIELD                      PIC X(100).
           03  R01-DATE                       REDEFINES R01-FIELD
                                              PIC 99/99/99.
           03  R01-NUMERIC                    REDEFINES R01-FIELD
                                              PIC Z(10)9.
           03  R01-TEXT                       REDEFINES R01-FIELD
                                              PIC X(100).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
      *COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
      *-----------------------------------------------------------------
       COPY "LIBIO/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBIO/IOPDRN_DEF.CPY".
       COPY "LIBIO/IOPDRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOPDRN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE.

       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *=================================================================
       MAIN-PROGRAM-NEXT-GROUP.

           IF ( GROUP-FLAG = "N" )
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = ZEROES )
              GO TO MAIN-PROGRAM-END.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO MAIN-PROGRAM-PROCESS-BRNO.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM READ-BR-FILE-NEXT.

           IF ( IO-BAD             ) OR
              ( BR-NO > END-BRANCH )
              GO TO MAIN-PROGRAM-END.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-PROCESS-BRNO.

           PERFORM PROCESS-BRANCH-MODULE.

           GO TO MAIN-PROGRAM-NEXT-GROUP.

      *=================================================================
       MAIN-PROGRAM-END.

           PERFORM WRITE-SUMMARY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DISPLAY SUMMARY TO SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM GET-TIME.
           MOVE SPACES TO SENDMESS-HEAD.

           STRING "PROGRAM COMPLETED AT ", TIME-EDIT
                  DELIMITED BY SIZE INTO SENDMESS-HEAD.

           MOVE SPACES TO SENDMESS-MSG1.
           MOVE SPACES TO SENDMESS-MSG2.
           STRING "RECORDS READ: ", RECORDS-READ
                  DELIMITED BY SIZE INTO SENDMESS-MSG2.
           DISPLAY MESSAGE BOX SENDMESS-HEAD, X"0A"
                               SENDMESS-MSG1, X"0A"
                               SENDMESS-MSG2.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
      D    STOP FORM-PATHNAME.

           MOVE "X"               TO RECORDS-FOUND.
           MOVE FORM-PATHNAME     TO FORM-PATH.
           MOVE "IOPDRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOPDRN-QUEUE-POS  TO Q-POS.
           MOVE IOPDRN-COPIES-POS TO C-POS.
           MOVE IOPDRN-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              PERFORM GET-TIME
              MOVE SPACES TO LEGEND
              STRING "STARTING TIME: ", TIME-EDIT
                     DELIMITED BY SIZE INTO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

           IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
              MOVE BEG-BRANCH TO SV-BEG-BRANCH
              MOVE END-BRANCH TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC  TO BEG-BRANCH
                                           END-BRANCH.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH         TO SV-BEG-BRANCH
              MOVE END-BRANCH         TO SV-END-BRANCH
              MOVE GROUP-FLAG         TO SV-GROUP-FLAG
              MOVE ENT-GROUP          TO SV-ENT-GROUP
              MOVE EXT-EOBUF-GROUP        TO ENT-GROUP
              MOVE ZEROES             TO BEG-BRANCH
              MOVE 9999               TO END-BRANCH
              MOVE "Y"                TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE.

           PERFORM GET-GPENV.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.

           PERFORM ENABLE-INTERRUPT.
           PERFORM SEND-PACE-HEAD.

           MOVE ZEROES TO PACE-LEV.
           MOVE ZEROES TO RECORDS-READ.

           PERFORM GET-TIME.
           MOVE "N"       TO RECORDS-FOUND.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE "R" TO TIMER-TYPE.
           PERFORM TIMER-START.
           MOVE TIMER-LINK-BUF TO P-TIMER-LINK-BUF.

      *=================================================================
       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.

           IF ( UP-KEY )
              GO TO ENTER-END-SSNO.

      *=================================================================
       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              MOVE ZEROES TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES               TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"                  TO GROUP-FLAG
              MOVE ZEROES               TO BEG-BRANCH
              MOVE 9999                 TO END-BRANCH
              GO TO CHECK-BR-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.

           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.

           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE LEGEND-END-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           GO TO CHECK-BR-SECURITY.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "N"                    TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BR-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" ) GO TO ENTER-BEG-BRANCH.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 = ZEROES ) OR
              ( VDUNUM0 > 1      ) 
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-BEG-ID.

           MOVE "E  A010BEG-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-ID.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-ID.

      *-----------------------------------------------------------------
       ENTER-END-ID.

           MOVE "E  A010END-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-ID.

           IF ( BEG-ID > END-ID )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ID.

           GO TO ENTER-BEG-SSNO.

      *-----------------------------------------------------------------
       ENTER-ALL-ID.

           MOVE "S  A010BEG-ID." TO VDU.
           MOVE SPACES           TO VDUBUF  BEG-ID.
           PERFORM SCREENIO.

           MOVE "S  A010END-ID." TO VDU.
           MOVE ALL "Z"              TO VDUBUF  END-ID.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SSNO.

           MOVE "ERNX090BEG-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SSNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SSNO.

      *-----------------------------------------------------------------
       ENTER-END-SSNO.

           MOVE "ERNX090END-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SSNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SSNO.

           IF ( BEG-SSNO > END-SSNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SSNO.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-SSNO.

           MOVE "SRNX090BEG-SSNO." TO VDU.
           MOVE ZEROES             TO VDU-SSNO-EDIT BEG-SSNO.
           PERFORM SCREENIO.

           MOVE "SRNX090END-SSNO." TO VDU.
           MOVE 999999999          TO VDU-SSNO-EDIT END-SSNO.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    D I S P L A Y - P A R A M E T E R S
      *
      *****************************************************************
       DISPLAY-PARAMETERS SECTION.

           IF ( GROUP-FLAG = "Y" )
              MOVE "S  A000BEG-BRANCH."   TO VDU
              MOVE ENT-GROUP              TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH."   TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH."   TO VDU
              MOVE BEG-BRANCH             TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH."   TO VDU
              MOVE END-BRANCH             TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "S  A010BEG-ID."       TO VDU.
           MOVE BEG-ID                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010END-ID."       TO VDU.
           MOVE END-ID                 TO VDUBUF.
           PERFORM SCREENIO.

            MOVE "SRNX090BEG-SSNO."        TO VDU.
           MOVE BEG-SSNO                  TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

            MOVE "SRNX090END-SSNO."        TO VDU.
           MOVE END-SSNO                  TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

      *=================================================================
       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    E O - E X E C - A U D I T
      *
      *****************************************************************
       EO-EXEC-AUDIT SECTION.

      *    MOVE EXT-EOBUF-DATE1 TO ???.
      *    MOVE EXT-EOBUF-DATE2 TO ???.

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S C A N - G R 1 - F I L E
      *
      ******************************************************************
       SCAN-GR1-FILE SECTION.

           MOVE GRSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF   TO GRSCAN-BUF.

           IF ( SCAN-STATUS NOT = "00"   ) OR
              ( GRSCAN-KEY      = SPACES )
              MOVE "XX" TO VDUSTATUS
              GO TO SCAN-GR1-FILE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - -{ VALID SELECTION  }
           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE GRSCAN-KEY TO VDUBUF.
           MOVE "00"       TO VDUSTATUS.

      *=================================================================
       SCAN-GR1-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - B R A N C H - M O D U L E
      *
      ******************************************************************
       PROCESS-BRANCH-MODULE SECTION.

           MOVE PACE-COUNTER-MAX TO PACE-COUNTER.

      *- - - - - - - - - - - - - - - - - -{ VERIFY 'LOCAL' FILE PATHS  }
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO PD-PATH-OVERRIDE.

           MOVE BR-NO     TO QPD1-WBEG-BRANCH
                              QPD2-WBEG-BRANCH.
           MOVE BR-NO     TO QPD1-WEND-BRANCH
                              QPD2-WEND-BRANCH.

           MOVE BEG-ID  TO QPD1-WBEG-ID
                           QPD2-WBEG-ID.
           MOVE END-ID  TO QPD1-WEND-ID
                           QPD2-WEND-ID.

           MOVE BEG-SSNO  TO QPD1-WBEG-SOC-SEC.
           MOVE END-SSNO  TO QPD1-WEND-SOC-SEC.

           MOVE SPACES    TO QPD2-WBEG-BWNAME.
           MOVE ALL "Z"   TO QPD2-WEND-BWNAME.


      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-PD1-FILE
              PERFORM START-PD1-FILE
           ELSE
              PERFORM OPEN-PD2-FILE
              PERFORM START-PD2-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-PD1-FILE-NEXT
           ELSE
              PERFORM READ-PD2-FILE-NEXT.
           IF ( IO-BAD ) 
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.


           IF (PROCESS-KEY = 1)
              IF ( PD-ID < BEG-ID )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( PD-ID > END-ID )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF PD-BRANCH < BEG-BRANCH OR > END-BRANCH
                    GO TO PROCESS-BRANCH-NEXT
                 ELSE
                 IF PD-SOC-SEC < BEG-SSNO OR > END-SSNO
                    GO TO PROCESS-BRANCH-NEXT.

           IF (PROCESS-KEY = 2)
              IF ( PD2-ID < BEG-ID )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( PD2-ID > END-ID )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF PD2-BRANCH < BEG-BRANCH OR > END-BRANCH
                    GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE PD-BRANCH                    TO D01-PD-BRANCH.
           MOVE PD-ID                  TO D01-PD-ID.
           MOVE PD-SOC-SEC                   TO D01-PD-SOC-SEC.
           MOVE PD-CLASS                    TO D01-PD-CLASS.
           MOVE PD-SRCD                    TO D01-PD-SRCD.
           MOVE PD-LNAMT                    TO D01-PD-LNAMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           IF PROCESS-KEY = 1
              PERFORM CLOSE-PD1-FILE
           ELSE
              PERFORM CLOSE-PD2-FILE.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P A C E - R O U T I N E
      *
      *=================================================================
      * DESC: DISPLAY THE PACE OF THE EXECUTION OF THE PROGRAM.
      ******************************************************************
       PACE-ROUTINE SECTION.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO PACE-ROUTINE-EXIT.

           ADD 01 TO PACE-COUNTER.

           IF ( PACE-COUNTER <= PACE-COUNTER-MAX )
              GO TO PACE-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ADD 01 TO PACE-LEV.

           IF ( PACE-LEV <= PACE-MAX )
              GO TO PACE-ROUTINE-UPDATE.

           PERFORM VARYING SUB FROM 2 BY 1 UNTIL SUB > PACE-MAX
              MOVE PACE-ELE(SUB) TO PACE-ELE(SUB - 1)
           END-PERFORM.

           MOVE PACE-MAX TO PACE-LEV.
           MOVE SPACES   TO PACE-ELE(PACE-LEV).

      *-----------------------------------------------------------------
       PACE-ROUTINE-UPDATE.

           MOVE SPACES TO PACE-DETAIL.

           PERFORM GET-TIME.
           MOVE CUR-HH    TO PACE-TIME-HH.
           MOVE CUR-MM    TO PACE-TIME-MM.
           MOVE CUR-SS    TO PACE-TIME-SS.
           MOVE CUR-HD    TO PACE-TIME-HD.
           MOVE PACE-TIME TO PACE-DETAIL-TIME.

           MOVE PD1-KEY TO PACE-DETAIL-KEY.

           MOVE PACE-DETAIL TO PACE-ELE(PACE-LEV).
           MOVE PACE-BUF    TO WR-BUF.
           MOVE PACE-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ZEROES TO PACE-COUNTER.

      *=================================================================
       PACE-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - S U M M A R Y
      *
      ******************************************************************
       WRITE-SUMMARY SECTION.

           IF ( NO-RECORDS-FOUND )
              GO TO WRITE-SUMMARY-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 99     TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GROUP-FLAG = "Y" )
              MOVE "GROUP ID NAME                 : " TO R01-PROMPT
              MOVE ENT-GROUP                          TO R01-TEXT
              MOVE 2                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH NUMBER       : " TO R01-PROMPT
              MOVE BEG-BRANCH                         TO R01-NUMERIC
              MOVE 2                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING    BRANCH NUMBER       : " TO R01-PROMPT
              MOVE END-BRANCH                         TO R01-NUMERIC
              MOVE 1                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING ID                  : "    TO R01-PROMPT.
           MOVE BEG-ID                                TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ID                  : "    TO R01-PROMPT.
           MOVE END-ID                                TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SSNO                : "    TO R01-PROMPT.
           MOVE BEG-SSNO                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SSNO                : "    TO R01-PROMPT.
           MOVE END-SSNO                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME                 : "    TO R01-PROMPT.
           MOVE STARTING-TIME                         TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME                 : "    TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *=================================================================
       WRITE-SUMMARY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-PAGE-HEADER.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *=================================================================
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - P A G E - H E A D E R
      *
      ******************************************************************
       WRITE-PAGE-HEADER SECTION.

           MOVE 01 TO LN-COUNT.
           ADD  01 TO PAGE-NUMBER.
           PERFORM GET-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{01}
           MOVE EXT-CONAME-SYSDATE               TO H01-DATE.
           MOVE T-HH                             TO H01-HOUR.
           MOVE T-MM                             TO H01-MINUTE.
           MOVE EXT-CONAME-CONAME                TO H01-NAME.
           CALL "C$JUSTIFY"                   USING H01-NAME "C".
           MOVE PAGE-NUMBER                      TO H01-PAGE.

           MOVE HEADER-LINE-01                   TO PR-REC.
           MOVE 99                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{02}
           MOVE EXT-CONAME-USER-ID               TO H02-USERID.
           MOVE "SQL TABLE IO UTILITY - PDFILE"  TO H02-TITLE.
           CALL "C$JUSTIFY"                   USING H02-TITLE "C".

           MOVE HEADER-LINE-02                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{03}
           MOVE HEADER-LINE-03                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 01 TO LN-FEED.

      *=================================================================
       WRITE-PAGE-HEADER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    F O R M - R E S E T
      *
      ******************************************************************
       FORM-RESET SECTION.

           MOVE EXT-CONAME-CONAME TO IOPDRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOPDRN-SYSDATE.
           DISPLAY IOPDRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOPDRN-FORM-FIELDS TO WR-BUF.
           MOVE IOPDRN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *=================================================================
       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       SETUP-LINK-INFO SECTION.

           MOVE IOPDRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOPDRN-HELP--DIR  TO HELP-LINK-DIR.

      *=================================================================
       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.

           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBIO/IOPDRN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *=================================================================
       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBWI/PFSWINDOWS.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      *COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *COPY "LIBGB/RMFILE.CPY".
      *=================================================================
       SEND-LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-PACE-HEAD.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE PACE-COUNTER-MAX    TO PACE-HEAD-MAX
              MOVE "S  A400PACE-HEAD." TO VDU
              MOVE PACE-HEAD-LINE      TO VDUBUF
              PERFORM SCREENIO.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBIO/CONNECT_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      *-----------------------------------------------------------------
      *    LOAD PD-REC FROM QPD-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-PD-FIELDS.
           MOVE QPD-ID TO PD-ID.
           MOVE QPD-BRANCH TO PD-BRANCH.
           MOVE QPD-SOC-SEC TO PD-SOC-SEC.
           MOVE QPD-OWNBR TO PD-OWNBR.
           MOVE QPD-ACCTNO TO PD-ACCTNO.
           MOVE QPD-CLASS TO PD-CLASS.
           MOVE QPD-SUBCLASS TO PD-SUBCLASS.
           MOVE QPD-LAWCODE TO PD-LAWCODE.
           MOVE QPD-SSNO1 TO PD-SSNO1.
           MOVE QPD-MAKERCD1 TO PD-MAKERCD1.
           MOVE QPD-SSNO2 TO PD-SSNO2.
           MOVE QPD-MAKERCD2 TO PD-MAKERCD2.
           MOVE QPD-SSNO3 TO PD-SSNO3.
           MOVE QPD-MAKERCD3 TO PD-MAKERCD3.
           MOVE QPD-SSNO4 TO PD-SSNO4.
           MOVE QPD-MAKERCD4 TO PD-MAKERCD4.
           MOVE QPD-ORGST TO PD-ORGST.
           MOVE QPD-ORGBR TO PD-ORGBR.
           MOVE QPD-OWNST TO PD-OWNST.
           MOVE QPD-SPRCLASS TO PD-SPRCLASS.
           MOVE QPD-CONVERCD TO PD-CONVERCD.
           MOVE QPD-MIDSTREAM TO PD-MIDSTREAM.
           MOVE QPD-PURCD TO PD-PURCD.
           MOVE QPD-SRCD TO PD-SRCD.
           MOVE QPD-COLLCD TO PD-COLLCD.
           MOVE QPD-COLLID TO PD-COLLID.
           MOVE QPD-POCD TO PD-POCD.
           MOVE QPD-PLCD TO PD-PLCD.
           MOVE QPD-PLTYPE TO PD-PLTYPE.
           MOVE QPD-CONFIDENTCD TO PD-CONFIDENTCD.
           MOVE QPD-ALLOTCD TO PD-ALLOTCD.
           MOVE QPD-RECASTCD TO PD-RECASTCD.
           MOVE QPD-SKIP-TRW TO PD-SKIP-TRW.
           MOVE QPD-STATUSFG TO PD-STATUSFG.
           MOVE QPD-SMALLOAN TO PD-SMALLOAN.
           MOVE QPD-SALEFIN TO PD-SALEFIN.
           MOVE QPD-SERVACCT TO PD-SERVACCT.
           MOVE QPD-LOANTYPE TO PD-LOANTYPE.
           MOVE QPD-ADDON-FG TO PD-ADDON-FG.
           MOVE QPD-PAY-SCHLD-FG TO PD-PAY-SCHLD-FG.
           MOVE QPD-LC-CODE TO PD-LC-CODE.
           MOVE QPD-CAN-LOAN-NO TO PD-CAN-LOAN-NO.
           MOVE QPD-STATFLAGS TO PD-STATFLAGS.

           MOVE QPD-ENTDATE TO SQL-DATE-YYYY-MM-DD.
           PERFORM SQL-GET-DATE.
           MOVE SQL-DATE-YYYYMMDD TO PD-ENTDATE.

           MOVE QPD-LOANDATE TO SQL-DATE-YYYY-MM-DD.
           PERFORM SQL-GET-DATE.
           MOVE SQL-DATE-YYYYMMDD TO PD-LOANDATE.

           MOVE QPD-PURDATE TO SQL-DATE-YYYY-MM-DD
           PERFORM SQL-GET-DATE
           MOVE SQL-DATE-YYYYMMDD TO PD-PURDATE.

           MOVE QPD-INTDATE TO SQL-DATE-YYYY-MM-DD
           PERFORM SQL-GET-DATE
           MOVE SQL-DATE-YYYYMMDD TO PD-INTDATE.

           MOVE QPD-CASHDATE TO SQL-DATE-YYYY-MM-DD
           PERFORM SQL-GET-DATE
           MOVE SQL-DATE-YYYYMMDD TO PD-CASHDATE.


           MOVE QPD-FILEDATE TO SQL-DATE-YYYY-MM-DD
           PERFORM SQL-GET-DATE
           MOVE SQL-DATE-YYYYMMDD TO PD-FILEDATE.


           MOVE QPD-RESCISSION-DATE TO SQL-DATE-YYYY-MM-DD
           PERFORM SQL-GET-DATE
           MOVE SQL-DATE-YYYYMMDD TO PD-RESCISSION-DATE.


           IF ( QPD-PART-RECOURSE-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-PART-RECOURSE-DATE
           ELSE
              MOVE QPD-PART-RECOURSE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-PART-RECOURSE-DATE.


           IF ( QPD-ORIG-1STPYDATE = "1900-01-01" )
              MOVE ZEROES TO PD-ORIG-1STPYDATE
           ELSE
              MOVE QPD-ORIG-1STPYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-ORIG-1STPYDATE.


           IF ( QPD-1STPYDATE = "1900-01-01" )
              MOVE ZEROES TO PD-1STPYDATE
           ELSE
              MOVE QPD-1STPYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-1STPYDATE.


           IF ( QPD-JDDATE = "1900-01-01" )
              MOVE ZEROES TO PD-JDDATE
           ELSE
              MOVE QPD-JDDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-JDDATE.


           IF ( QPD-PLDATE = "1900-01-01" )
              MOVE ZEROES TO PD-PLDATE
           ELSE
              MOVE QPD-PLDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-PLDATE.


           IF ( QPD-PL-TRDATE = "1900-01-01" )
              MOVE ZEROES TO PD-PL-TRDATE
           ELSE
              MOVE QPD-PL-TRDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-PL-TRDATE.


           IF ( QPD-ACCELDATE = "1900-01-01" )
              MOVE ZEROES TO PD-ACCELDATE
           ELSE
              MOVE QPD-ACCELDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-ACCELDATE.


           IF ( QPD-REDUDATE = "1900-01-01" )
              MOVE ZEROES TO PD-REDUDATE
           ELSE
              MOVE QPD-REDUDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-REDUDATE.


           IF ( QPD-POFFDATE = "1900-01-01" )
              MOVE ZEROES TO PD-POFFDATE
           ELSE
              MOVE QPD-POFFDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-POFFDATE.


           IF ( QPD-PO-TRDATE = "1900-01-01" )
              MOVE ZEROES TO PD-PO-TRDATE
           ELSE
              MOVE QPD-PO-TRDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-PO-TRDATE.


           IF ( QPD-LEGALDATE = "1900-01-01" )
              MOVE ZEROES TO PD-LEGALDATE
           ELSE
              MOVE QPD-LEGALDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-LEGALDATE.


           IF ( QPD-BNKRPTDATE = "1900-01-01" )
              MOVE ZEROES TO PD-BNKRPTDATE
           ELSE
              MOVE QPD-BNKRPTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-BNKRPTDATE.


           IF ( QPD-BNKRPT-DISMISS-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-BNKRPT-DISMISS-DATE
           ELSE
              MOVE QPD-BNKRPT-DISMISS-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-BNKRPT-DISMISS-DATE.


           IF ( QPD-BNKRPT-DISCHARGE-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-BNKRPT-DISCHARGE-DATE
           ELSE
              MOVE QPD-BNKRPT-DISCHARGE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-BNKRPT-DISCHARGE-DATE.


           IF ( QPD-BNKRPT-REAFFIRM-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-BNKRPT-REAFFIRM-DATE
           ELSE
              MOVE QPD-BNKRPT-REAFFIRM-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-BNKRPT-REAFFIRM-DATE.


           IF ( QPD-DEBTAIDATE = "1900-01-01" )
              MOVE ZEROES TO PD-DEBTAIDATE
           ELSE
              MOVE QPD-DEBTAIDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-DEBTAIDATE.


           IF ( QPD-REPODATE = "1900-01-01" )
              MOVE ZEROES TO PD-REPODATE
           ELSE
              MOVE QPD-REPODATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-REPODATE.


           IF ( QPD-REDEEMDATE = "1900-01-01" )
              MOVE ZEROES TO PD-REDEEMDATE
           ELSE
              MOVE QPD-REDEEMDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-REDEEMDATE.


           IF ( QPD-SOLDDATE = "1900-01-01" )
              MOVE ZEROES TO PD-SOLDDATE
           ELSE
              MOVE QPD-SOLDDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-SOLDDATE.


           IF ( QPD-INS-CONFIRM-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-INS-CONFIRM-DATE
           ELSE
              MOVE QPD-INS-CONFIRM-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-INS-CONFIRM-DATE.


           IF ( QPD-PRIOR-RN-FOR10-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-PRIOR-RN-FOR10-DATE
           ELSE
              MOVE QPD-PRIOR-RN-FOR10-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-PRIOR-RN-FOR10-DATE.


           IF ( QPD-LAST-RN-FOR10-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-LAST-RN-FOR10-DATE
           ELSE
              MOVE QPD-LAST-RN-FOR10-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-LAST-RN-FOR10-DATE.


           IF ( QPD-TRW-DOFD-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-TRW-DOFD-DATE
           ELSE
              MOVE QPD-TRW-DOFD-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-TRW-DOFD-DATE.


           IF ( QPD-LAST-PYPA-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-LAST-PYPA-DATE
           ELSE
              MOVE QPD-LAST-PYPA-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-LAST-PYPA-DATE.


           IF ( QPD-BT-TRDATE = "1900-01-01" )
              MOVE ZEROES TO PD-BT-TRDATE
           ELSE
              MOVE QPD-BT-TRDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-BT-TRDATE.


           IF ( QPD-METRO2-LAST-PMT-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-METRO2-LAST-PMT-DATE
           ELSE
              MOVE QPD-METRO2-LAST-PMT-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-METRO2-LAST-PMT-DATE.


           IF ( QPD-FOLLOWUP-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-FOLLOWUP-DATE
           ELSE
              MOVE QPD-FOLLOWUP-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-FOLLOWUP-DATE.


           IF ( QPD-LAST-ACCRUAL-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-LAST-ACCRUAL-DATE
           ELSE
              MOVE QPD-LAST-ACCRUAL-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-LAST-ACCRUAL-DATE.

           MOVE QPD-APRATE TO PD-APRATE.
           MOVE QPD-EFFRATE TO PD-EFFRATE.
           MOVE QPD-SMPRATE TO PD-SMPRATE.
           MOVE QPD-YLDRATE TO PD-YLDRATE.
           MOVE QPD-OVRDRATE TO PD-OVRDRATE.
           MOVE QPD-ENDORATE TO PD-ENDORATE.
           MOVE QPD-JDRATE TO PD-JDRATE.
           MOVE QPD-ACCRATE TO PD-ACCRATE.
           MOVE QPD-REDURATE TO PD-REDURATE.
           MOVE QPD-CONRATE TO PD-CONRATE.
           MOVE QPD-SERVPER TO PD-SERVPER.
           MOVE QPD-SELLPR TO PD-SELLPR.
           MOVE QPD-DOWNPY TO PD-DOWNPY.
           MOVE QPD-TRADEIN TO PD-TRADEIN.
           MOVE QPD-LNAMT TO PD-LNAMT.
           MOVE QPD-FINCHG TO PD-FINCHG.
           MOVE QPD-INTCHG TO PD-INTCHG.
           MOVE QPD-SERCHG TO PD-SERCHG.
           MOVE QPD-EXTCHG TO PD-EXTCHG.
           MOVE QPD-MAINTFEE TO PD-MAINTFEE.
           MOVE QPD-POINTS TO PD-POINTS.
           MOVE QPD-PROCEEDS TO PD-PROCEEDS.
           MOVE QPD-ORGTERM TO PD-ORGTERM.
           MOVE QPD-1STPYAMT TO PD-1STPYAMT.
           MOVE QPD-NOREGPY TO PD-NOREGPY.
           MOVE QPD-REGPYAMT TO PD-REGPYAMT.
           MOVE QPD-LASTPYAMT TO PD-LASTPYAMT.
           MOVE QPD-PROCASH TO PD-PROCASH.
           MOVE QPD-TILLNO TO PD-TILLNO.
           MOVE QPD-STATE-FLAG TO PD-STATE-FLAG.
           MOVE QPD-STATE-WORKER TO PD-STATE-WORKER.
           MOVE QPD-STATE-WORKER2 TO PD-STATE-WORKER2.
           MOVE QPD-FEECODE-1 TO PD-FEECODE(1).
           MOVE QPD-FEEAMT-1 TO PD-FEEAMT(1).
           MOVE QPD-FEEOURS-1 TO PD-FEEOURS(1).
           MOVE QPD-FEEFINCHG-1 TO PD-FEEFINCHG(1).
           MOVE QPD-FEECODE-2 TO PD-FEECODE(2).
           MOVE QPD-FEEAMT-2 TO PD-FEEAMT(2).
           MOVE QPD-FEEOURS-2 TO PD-FEEOURS(2).
           MOVE QPD-FEEFINCHG-2 TO PD-FEEFINCHG(2).
           MOVE QPD-FEECODE-3 TO PD-FEECODE(3).
           MOVE QPD-FEEAMT-3 TO PD-FEEAMT(3).
           MOVE QPD-FEEOURS-3 TO PD-FEEOURS(3).
           MOVE QPD-FEEFINCHG-3 TO PD-FEEFINCHG(3).
           MOVE QPD-FEECODE-4 TO PD-FEECODE(4).
           MOVE QPD-FEEAMT-4 TO PD-FEEAMT(4).
           MOVE QPD-FEEOURS-4 TO PD-FEEOURS(4).
           MOVE QPD-FEEFINCHG-4 TO PD-FEEFINCHG(4).
           MOVE QPD-FEECODE-5 TO PD-FEECODE(5).
           MOVE QPD-FEEAMT-5 TO PD-FEEAMT(5).
           MOVE QPD-FEEOURS-5 TO PD-FEEOURS(5).
           MOVE QPD-FEEFINCHG-5 TO PD-FEEFINCHG(5).
           MOVE QPD-FEECODE-6 TO PD-FEECODE(6).
           MOVE QPD-FEEAMT-6 TO PD-FEEAMT(6).
           MOVE QPD-FEEOURS-6 TO PD-FEEOURS(6).
           MOVE QPD-FEEFINCHG-6 TO PD-FEEFINCHG(6).
           MOVE QPD-FEECODE-7 TO PD-FEECODE(7).
           MOVE QPD-FEEAMT-7 TO PD-FEEAMT(7).
           MOVE QPD-FEEOURS-7 TO PD-FEEOURS(7).
           MOVE QPD-FEEFINCHG-7 TO PD-FEEFINCHG(7).
           MOVE QPD-FEECODE-8 TO PD-FEECODE(8).
           MOVE QPD-FEEAMT-8 TO PD-FEEAMT(8).
           MOVE QPD-FEEOURS-8 TO PD-FEEOURS(8).
           MOVE QPD-FEEFINCHG-8 TO PD-FEEFINCHG(8).
           MOVE QPD-FEECODE-9 TO PD-FEECODE(9).
           MOVE QPD-FEEAMT-9 TO PD-FEEAMT(9).
           MOVE QPD-FEEOURS-9 TO PD-FEEOURS(9).
           MOVE QPD-FEEFINCHG-9 TO PD-FEEFINCHG(9).
           MOVE QPD-FEECODE-10 TO PD-FEECODE(10).
           MOVE QPD-FEEAMT-10 TO PD-FEEAMT(10).
           MOVE QPD-FEEOURS-10 TO PD-FEEOURS(10).
           MOVE QPD-FEEFINCHG-10 TO PD-FEEFINCHG(10).
           MOVE QPD-FEECODE-11 TO PD-FEECODE(11).
           MOVE QPD-FEEAMT-11 TO PD-FEEAMT(11).
           MOVE QPD-FEEOURS-11 TO PD-FEEOURS(11).
           MOVE QPD-FEEFINCHG-11 TO PD-FEEFINCHG(11).
           MOVE QPD-FEECODE-12 TO PD-FEECODE(12).
           MOVE QPD-FEEAMT-12 TO PD-FEEAMT(12).
           MOVE QPD-FEEOURS-12 TO PD-FEEOURS(12).
           MOVE QPD-FEEFINCHG-12 TO PD-FEEFINCHG(12).
           MOVE QPD-FEECODE-13 TO PD-FEECODE(13).
           MOVE QPD-FEEAMT-13 TO PD-FEEAMT(13).
           MOVE QPD-FEEOURS-13 TO PD-FEEOURS(13).
           MOVE QPD-FEEFINCHG-13 TO PD-FEEFINCHG(13).
           MOVE QPD-FEECODE-14 TO PD-FEECODE(14).
           MOVE QPD-FEEAMT-14 TO PD-FEEAMT(14).
           MOVE QPD-FEEOURS-14 TO PD-FEEOURS(14).
           MOVE QPD-FEEFINCHG-14 TO PD-FEEFINCHG(14).
           MOVE QPD-FEECODE-15 TO PD-FEECODE(15).
           MOVE QPD-FEEAMT-15 TO PD-FEEAMT(15).
           MOVE QPD-FEEOURS-15 TO PD-FEEOURS(15).
           MOVE QPD-FEEFINCHG-15 TO PD-FEEFINCHG(15).
           MOVE QPD-FEECODE-16 TO PD-FEECODE(16).
           MOVE QPD-FEEAMT-16 TO PD-FEEAMT(16).
           MOVE QPD-FEEOURS-16 TO PD-FEEOURS(16).
           MOVE QPD-FEEFINCHG-16 TO PD-FEEFINCHG(16).
           MOVE QPD-FEECODE-17 TO PD-FEECODE(17).
           MOVE QPD-FEEAMT-17 TO PD-FEEAMT(17).
           MOVE QPD-FEEOURS-17 TO PD-FEEOURS(17).
           MOVE QPD-FEEFINCHG-17 TO PD-FEEFINCHG(17).
           MOVE QPD-FEECODE-18 TO PD-FEECODE(18).
           MOVE QPD-FEEAMT-18 TO PD-FEEAMT(18).
           MOVE QPD-FEEOURS-18 TO PD-FEEOURS(18).
           MOVE QPD-FEEFINCHG-18 TO PD-FEEFINCHG(18).
           MOVE QPD-FEECODE-19 TO PD-FEECODE(19).
           MOVE QPD-FEEAMT-19 TO PD-FEEAMT(19).
           MOVE QPD-FEEOURS-19 TO PD-FEEOURS(19).
           MOVE QPD-FEEFINCHG-19 TO PD-FEEFINCHG(19).
           MOVE QPD-FEECODE-20 TO PD-FEECODE(20).
           MOVE QPD-FEEAMT-20 TO PD-FEEAMT(20).
           MOVE QPD-FEEOURS-20 TO PD-FEEOURS(20).
           MOVE QPD-FEEFINCHG-20 TO PD-FEEFINCHG(20).
           MOVE QPD-INSURED TO PD-INSURED.
           MOVE QPD-INSTYPES TO PD-INSTYPES.
           MOVE QPD-INSELIGIBLE TO PD-INSELIGIBLE.
           MOVE QPD-INSCOMP-1 TO PD-INSCOMP(1).
           MOVE QPD-INSOURS-1 TO PD-INSOURS(1).

           MOVE QPD-INSEFF-DATE-1 TO PD-INSEFF-DATE(1).

           IF ( QPD-INSEXP-DATE-1 = "1900-01-01" )
              MOVE ZEROES TO PD-INSEXP-DATE(1)
           ELSE
              MOVE QPD-INSEXP-DATE-1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-INSEXP-DATE(1).

           MOVE QPD-INSPREM-1 TO PD-INSPREM(1).
           MOVE QPD-INSCOVR-1 TO PD-INSCOVR(1).
           MOVE QPD-INSCOMM-1 TO PD-INSCOMM(1).
           MOVE QPD-INSANTICERN-1 TO PD-INSANTICERN(1).
           MOVE QPD-INSANTICADJ-1 TO PD-INSANTICADJ(1).
           MOVE QPD-INSUFERN-1 TO PD-INSUFERN(1).
           MOVE QPD-INSACCERN-1 TO PD-INSACCERN(1).
           MOVE QPD-INSEARN-ORDER-1 TO PD-INSEARN-ORDER(1).
           MOVE QPD-INSFINCHG-1 TO PD-INSFINCHG(1).
           MOVE QPD-INSCOMP-2 TO PD-INSCOMP(2).
           MOVE QPD-INSOURS-2 TO PD-INSOURS(2).

           MOVE QPD-INSEFF-DATE-2 TO PD-INSEFF-DATE(2).

           IF ( QPD-INSEXP-DATE-2 = "1900-01-01" )
              MOVE ZEROES TO PD-INSEXP-DATE(2)
           ELSE
              MOVE QPD-INSEXP-DATE-2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-INSEXP-DATE(2).

           MOVE QPD-INSPREM-2 TO PD-INSPREM(2).
           MOVE QPD-INSCOVR-2 TO PD-INSCOVR(2).
           MOVE QPD-INSCOMM-2 TO PD-INSCOMM(2).
           MOVE QPD-INSANTICERN-2 TO PD-INSANTICERN(2).
           MOVE QPD-INSANTICADJ-2 TO PD-INSANTICADJ(2).
           MOVE QPD-INSUFERN-2 TO PD-INSUFERN(2).
           MOVE QPD-INSACCERN-2 TO PD-INSACCERN(2).
           MOVE QPD-INSEARN-ORDER-2 TO PD-INSEARN-ORDER(2).
           MOVE QPD-INSFINCHG-2 TO PD-INSFINCHG(2).
           MOVE QPD-INSCOMP-3 TO PD-INSCOMP(3).
           MOVE QPD-INSOURS-3 TO PD-INSOURS(3).

           MOVE QPD-INSEFF-DATE-3 TO PD-INSEFF-DATE(3).

           IF ( QPD-INSEXP-DATE-3 = "1900-01-01" )
              MOVE ZEROES TO PD-INSEXP-DATE(3)
           ELSE
              MOVE QPD-INSEXP-DATE-3 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-INSEXP-DATE(3).

           MOVE QPD-INSPREM-3 TO PD-INSPREM(3).
           MOVE QPD-INSCOVR-3 TO PD-INSCOVR(3).
           MOVE QPD-INSCOMM-3 TO PD-INSCOMM(3).
           MOVE QPD-INSANTICERN-3 TO PD-INSANTICERN(3).
           MOVE QPD-INSANTICADJ-3 TO PD-INSANTICADJ(3).
           MOVE QPD-INSUFERN-3 TO PD-INSUFERN(3).
           MOVE QPD-INSACCERN-3 TO PD-INSACCERN(3).
           MOVE QPD-INSEARN-ORDER-3 TO PD-INSEARN-ORDER(3).
           MOVE QPD-INSFINCHG-3 TO PD-INSFINCHG(3).
           MOVE QPD-INSCOMP-4 TO PD-INSCOMP(4).
           MOVE QPD-INSOURS-4 TO PD-INSOURS(4).

           MOVE QPD-INSEFF-DATE-4 TO PD-INSEFF-DATE(4).

           IF ( QPD-INSEXP-DATE-4 = "1900-01-01" )
              MOVE ZEROES TO PD-INSEXP-DATE(4)
           ELSE
              MOVE QPD-INSEXP-DATE-4 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-INSEXP-DATE(4).

           MOVE QPD-INSPREM-4 TO PD-INSPREM(4).
           MOVE QPD-INSCOVR-4 TO PD-INSCOVR(4).
           MOVE QPD-INSCOMM-4 TO PD-INSCOMM(4).
           MOVE QPD-INSANTICERN-4 TO PD-INSANTICERN(4).
           MOVE QPD-INSANTICADJ-4 TO PD-INSANTICADJ(4).
           MOVE QPD-INSUFERN-4 TO PD-INSUFERN(4).
           MOVE QPD-INSACCERN-4 TO PD-INSACCERN(4).
           MOVE QPD-INSEARN-ORDER-4 TO PD-INSEARN-ORDER(4).
           MOVE QPD-INSFINCHG-4 TO PD-INSFINCHG(4).
           MOVE QPD-INSCOMP-5 TO PD-INSCOMP(5).
           MOVE QPD-INSOURS-5 TO PD-INSOURS(5).

           MOVE QPD-INSEFF-DATE-5 TO PD-INSEFF-DATE(5).

           IF ( QPD-INSEXP-DATE-5 = "1900-01-01" )
              MOVE ZEROES TO PD-INSEXP-DATE(5)
           ELSE
              MOVE QPD-INSEXP-DATE-5 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-INSEXP-DATE(5).

           MOVE QPD-INSPREM-5 TO PD-INSPREM(5).
           MOVE QPD-INSCOVR-5 TO PD-INSCOVR(5).
           MOVE QPD-INSCOMM-5 TO PD-INSCOMM(5).
           MOVE QPD-INSANTICERN-5 TO PD-INSANTICERN(5).
           MOVE QPD-INSANTICADJ-5 TO PD-INSANTICADJ(5).
           MOVE QPD-INSUFERN-5 TO PD-INSUFERN(5).
           MOVE QPD-INSACCERN-5 TO PD-INSACCERN(5).
           MOVE QPD-INSEARN-ORDER-5 TO PD-INSEARN-ORDER(5).
           MOVE QPD-INSFINCHG-5 TO PD-INSFINCHG(5).
           MOVE QPD-INSCOMP-6 TO PD-INSCOMP(6).
           MOVE QPD-INSOURS-6 TO PD-INSOURS(6).

           MOVE QPD-INSEFF-DATE-6 TO PD-INSEFF-DATE(6).

           IF ( QPD-INSEXP-DATE-6 = "1900-01-01" )
              MOVE ZEROES TO PD-INSEXP-DATE(6)
           ELSE
              MOVE QPD-INSEXP-DATE-6 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-INSEXP-DATE(6).

           MOVE QPD-INSPREM-6 TO PD-INSPREM(6).
           MOVE QPD-INSCOVR-6 TO PD-INSCOVR(6).
           MOVE QPD-INSCOMM-6 TO PD-INSCOMM(6).
           MOVE QPD-INSANTICERN-6 TO PD-INSANTICERN(6).
           MOVE QPD-INSANTICADJ-6 TO PD-INSANTICADJ(6).
           MOVE QPD-INSUFERN-6 TO PD-INSUFERN(6).
           MOVE QPD-INSACCERN-6 TO PD-INSACCERN(6).
           MOVE QPD-INSEARN-ORDER-6 TO PD-INSEARN-ORDER(6).
           MOVE QPD-INSFINCHG-6 TO PD-INSFINCHG(6).
           MOVE QPD-INSCOMP-7 TO PD-INSCOMP(7).
           MOVE QPD-INSOURS-7 TO PD-INSOURS(7).

           MOVE QPD-INSEFF-DATE-7 TO PD-INSEFF-DATE(7).

           IF ( QPD-INSEXP-DATE-7 = "1900-01-01" )
              MOVE ZEROES TO PD-INSEXP-DATE(7)
           ELSE
              MOVE QPD-INSEXP-DATE-7 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-INSEXP-DATE(7).

           MOVE QPD-INSPREM-7 TO PD-INSPREM(7).
           MOVE QPD-INSCOVR-7 TO PD-INSCOVR(7).
           MOVE QPD-INSCOMM-7 TO PD-INSCOMM(7).
           MOVE QPD-INSANTICERN-7 TO PD-INSANTICERN(7).
           MOVE QPD-INSANTICADJ-7 TO PD-INSANTICADJ(7).
           MOVE QPD-INSUFERN-7 TO PD-INSUFERN(7).
           MOVE QPD-INSACCERN-7 TO PD-INSACCERN(7).
           MOVE QPD-INSEARN-ORDER-7 TO PD-INSEARN-ORDER(7).
           MOVE QPD-INSFINCHG-7 TO PD-INSFINCHG(7).
           MOVE QPD-INSCOMP-8 TO PD-INSCOMP(8).
           MOVE QPD-INSOURS-8 TO PD-INSOURS(8).

           MOVE QPD-INSEFF-DATE-8 TO PD-INSEFF-DATE(8).

           IF ( QPD-INSEXP-DATE-8 = "1900-01-01" )
              MOVE ZEROES TO PD-INSEXP-DATE(8)
           ELSE
              MOVE QPD-INSEXP-DATE-8 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-INSEXP-DATE(8).

           MOVE QPD-INSPREM-8 TO PD-INSPREM(8).
           MOVE QPD-INSCOVR-8 TO PD-INSCOVR(8).
           MOVE QPD-INSCOMM-8 TO PD-INSCOMM(8).
           MOVE QPD-INSANTICERN-8 TO PD-INSANTICERN(8).
           MOVE QPD-INSANTICADJ-8 TO PD-INSANTICADJ(8).
           MOVE QPD-INSUFERN-8 TO PD-INSUFERN(8).
           MOVE QPD-INSACCERN-8 TO PD-INSACCERN(8).
           MOVE QPD-INSEARN-ORDER-8 TO PD-INSEARN-ORDER(8).
           MOVE QPD-INSFINCHG-8 TO PD-INSFINCHG(8).
           MOVE QPD-INSCOMP-9 TO PD-INSCOMP(9).
           MOVE QPD-INSOURS-9 TO PD-INSOURS(9).

           MOVE QPD-INSEFF-DATE-9 TO PD-INSEFF-DATE(9).

           IF ( QPD-INSEXP-DATE-9 = "1900-01-01" )
              MOVE ZEROES TO PD-INSEXP-DATE(9)
           ELSE
              MOVE QPD-INSEXP-DATE-9 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-INSEXP-DATE(9).

           MOVE QPD-INSPREM-9 TO PD-INSPREM(9).
           MOVE QPD-INSCOVR-9 TO PD-INSCOVR(9).
           MOVE QPD-INSCOMM-9 TO PD-INSCOMM(9).
           MOVE QPD-INSANTICERN-9 TO PD-INSANTICERN(9).
           MOVE QPD-INSANTICADJ-9 TO PD-INSANTICADJ(9).
           MOVE QPD-INSUFERN-9 TO PD-INSUFERN(9).
           MOVE QPD-INSACCERN-9 TO PD-INSACCERN(9).
           MOVE QPD-INSEARN-ORDER-9 TO PD-INSEARN-ORDER(9).
           MOVE QPD-INSFINCHG-9 TO PD-INSFINCHG(9).
           MOVE QPD-INSCOMP-10 TO PD-INSCOMP(10).
           MOVE QPD-INSOURS-10 TO PD-INSOURS(10).

           MOVE QPD-INSEFF-DATE-10 TO PD-INSEFF-DATE(10).

           IF ( QPD-INSEXP-DATE-10 = "1900-01-01" )
              MOVE ZEROES TO PD-INSEXP-DATE(10)
           ELSE
              MOVE QPD-INSEXP-DATE-10 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-INSEXP-DATE(10).

           MOVE QPD-INSPREM-10 TO PD-INSPREM(10).
           MOVE QPD-INSCOVR-10 TO PD-INSCOVR(10).
           MOVE QPD-INSCOMM-10 TO PD-INSCOMM(10).
           MOVE QPD-INSANTICERN-10 TO PD-INSANTICERN(10).
           MOVE QPD-INSANTICADJ-10 TO PD-INSANTICADJ(10).
           MOVE QPD-INSUFERN-10 TO PD-INSUFERN(10).
           MOVE QPD-INSACCERN-10 TO PD-INSACCERN(10).
           MOVE QPD-INSEARN-ORDER-10 TO PD-INSEARN-ORDER(10).
           MOVE QPD-INSFINCHG-10 TO PD-INSFINCHG(10).
           MOVE QPD-PY-YYYYMM-RECDEL TO PD-PY-YYYYMM-RECDEL.
           MOVE QPD-MTD-PAYS-RECDEL TO PD-MTD-PAYS-RECDEL.
           MOVE QPD-GARNISH-EXTR TO PD-GARNISH-EXTR.
           MOVE QPD-SHIP-SAME-FG TO PD-SHIP-SAME-FG.
           MOVE QPD-AD-D-INS-COVR TO PD-AD-D-INS-COVR.
           MOVE QPD-TOT-PARTIAL-CHGOFF TO PD-TOT-PARTIAL-CHGOFF.
           MOVE QPD-LPO TO PD-LPO.
           MOVE QPD-REP-CD TO PD-REP-CD.
           MOVE QPD-EMP-ID TO PD-EMP-ID.
           MOVE QPD-PROCESS-CD TO PD-PROCESS-CD.
           MOVE QPD-WCBC-CREDITLIM TO PD-WCBC-CREDITLIM.
           MOVE QPD-FOLLOWUP-CD TO PD-FOLLOWUP-CD.
           MOVE QPD-MIP-FG TO PD-MIP-FG.
           MOVE QPD-MIP-MTD-CLAHPAID TO PD-MIP-MTD-CLAHPAID.

           IF ( QPD-MIP-CLPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-MIP-CLPDTH-DATE
           ELSE
              MOVE QPD-MIP-CLPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-MIP-CLPDTH-DATE.

           MOVE QPD-MIP-TOT-CLPAID TO PD-MIP-TOT-CLPAID.

           IF ( QPD-MIP-AHPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-MIP-AHPDTH-DATE
           ELSE
              MOVE QPD-MIP-AHPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-MIP-AHPDTH-DATE.

           MOVE QPD-MIP-TOT-AHPAID TO PD-MIP-TOT-AHPAID.
           MOVE QPD-IL-GROSS-INCOME TO PD-IL-GROSS-INCOME.
           MOVE QPD-IL-GROSS-INCOME2 TO PD-IL-GROSS-INCOME2.
           MOVE QPD-RESCISSION TO PD-RESCISSION.
           MOVE QPD-INTERIM-TRW TO PD-INTERIM-TRW.
           MOVE QPD-DLNO TO PD-DLNO.
           MOVE QPD-DLPOOL TO PD-DLPOOL.
           MOVE QPD-DLRECOURSE TO PD-DLRECOURSE.
           MOVE QPD-DLPARTRCAMT TO PD-DLPARTRCAMT.
           MOVE QPD-ORIGPURAMT TO PD-ORIGPURAMT.
           MOVE QPD-PURBAL TO PD-PURBAL.
           MOVE QPD-DLRVPRIN-1 TO PD-DLRVPRIN(1).
           MOVE QPD-DLRVPRIN-2 TO PD-DLRVPRIN(2).
           MOVE QPD-DLRVPRIN-3 TO PD-DLRVPRIN(3).
           MOVE QPD-DLRVCHGS-1 TO PD-DLRVCHGS(1).
           MOVE QPD-DLRVCHGS-2 TO PD-DLRVCHGS(2).
           MOVE QPD-DLRVCHGS-3 TO PD-DLRVCHGS(3).
           MOVE QPD-DLPARVHELD TO PD-DLPARVHELD.
           MOVE QPD-DLPARVPAID TO PD-DLPARVPAID.
           MOVE QPD-DLDISC TO PD-DLDISC.

           IF ( QPD-LCPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-LCPDTH-DATE
           ELSE
              MOVE QPD-LCPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-LCPDTH-DATE.


           IF ( QPD-INTPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-INTPDTH-DATE
           ELSE
              MOVE QPD-INTPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-INTPDTH-DATE.

           MOVE QPD-CURBAL TO PD-CURBAL.
           MOVE QPD-INTBAL TO PD-INTBAL.
           MOVE QPD-LCBAL TO PD-LCBAL.
           MOVE QPD-OTHBAL TO PD-OTHBAL.
           MOVE QPD-OT2BAL TO PD-OT2BAL.
           MOVE QPD-LCPAID TO PD-LCPAID.
           MOVE QPD-LCPARTIALS TO PD-LCPARTIALS.
           MOVE QPD-STMT-COUNTER TO PD-STMT-COUNTER.
           MOVE QPD-PRIOR-STMT-COUNTER TO PD-PRIOR-STMT-COUNTER.
           MOVE QPD-FROZEN-REASON TO PD-FROZEN-REASON.
           MOVE QPD-COUPON-STATEMENT TO PD-COUPON-STATEMENT.
           MOVE QPD-MONTHEND-BAL TO PD-MONTHEND-BAL.
           MOVE QPD-ANTICTERM TO PD-ANTICTERM.
           MOVE QPD-ANTICERN-1 TO PD-ANTICERN(1).
           MOVE QPD-ANTICADJ-1 TO PD-ANTICADJ(1).
           MOVE QPD-UFERN-1 TO PD-UFERN(1).
           MOVE QPD-ACCERN-1 TO PD-ACCERN(1).
           MOVE QPD-EARN-ORDER-1 TO PD-EARN-ORDER(1).
           MOVE QPD-ANTICERN-2 TO PD-ANTICERN(2).
           MOVE QPD-ANTICADJ-2 TO PD-ANTICADJ(2).
           MOVE QPD-UFERN-2 TO PD-UFERN(2).
           MOVE QPD-ACCERN-2 TO PD-ACCERN(2).
           MOVE QPD-EARN-ORDER-2 TO PD-EARN-ORDER(2).
           MOVE QPD-ANTICERN-3 TO PD-ANTICERN(3).
           MOVE QPD-ANTICADJ-3 TO PD-ANTICADJ(3).
           MOVE QPD-UFERN-3 TO PD-UFERN(3).
           MOVE QPD-ACCERN-3 TO PD-ACCERN(3).
           MOVE QPD-EARN-ORDER-3 TO PD-EARN-ORDER(3).
           MOVE QPD-ANTICERN-4 TO PD-ANTICERN(4).
           MOVE QPD-ANTICADJ-4 TO PD-ANTICADJ(4).
           MOVE QPD-UFERN-4 TO PD-UFERN(4).
           MOVE QPD-ACCERN-4 TO PD-ACCERN(4).
           MOVE QPD-EARN-ORDER-4 TO PD-EARN-ORDER(4).
           MOVE QPD-ANTICERN-5 TO PD-ANTICERN(5).
           MOVE QPD-ANTICADJ-5 TO PD-ANTICADJ(5).
           MOVE QPD-UFERN-5 TO PD-UFERN(5).
           MOVE QPD-ACCERN-5 TO PD-ACCERN(5).
           MOVE QPD-EARN-ORDER-5 TO PD-EARN-ORDER(5).
           MOVE QPD-ANTICERN-6 TO PD-ANTICERN(6).
           MOVE QPD-ANTICADJ-6 TO PD-ANTICADJ(6).
           MOVE QPD-UFERN-6 TO PD-UFERN(6).
           MOVE QPD-ACCERN-6 TO PD-ACCERN(6).
           MOVE QPD-EARN-ORDER-6 TO PD-EARN-ORDER(6).
           MOVE QPD-ANTICERN-7 TO PD-ANTICERN(7).
           MOVE QPD-ANTICADJ-7 TO PD-ANTICADJ(7).
           MOVE QPD-UFERN-7 TO PD-UFERN(7).
           MOVE QPD-ACCERN-7 TO PD-ACCERN(7).
           MOVE QPD-EARN-ORDER-7 TO PD-EARN-ORDER(7).
           MOVE QPD-ANTICERN-8 TO PD-ANTICERN(8).
           MOVE QPD-ANTICADJ-8 TO PD-ANTICADJ(8).
           MOVE QPD-UFERN-8 TO PD-UFERN(8).
           MOVE QPD-ACCERN-8 TO PD-ACCERN(8).
           MOVE QPD-EARN-ORDER-8 TO PD-EARN-ORDER(8).
           MOVE QPD-ANTICERN-9 TO PD-ANTICERN(9).
           MOVE QPD-ANTICADJ-9 TO PD-ANTICADJ(9).
           MOVE QPD-UFERN-9 TO PD-UFERN(9).
           MOVE QPD-ACCERN-9 TO PD-ACCERN(9).
           MOVE QPD-EARN-ORDER-9 TO PD-EARN-ORDER(9).
           MOVE QPD-ANTICERN-10 TO PD-ANTICERN(10).
           MOVE QPD-ANTICADJ-10 TO PD-ANTICADJ(10).
           MOVE QPD-UFERN-10 TO PD-UFERN(10).
           MOVE QPD-ACCERN-10 TO PD-ACCERN(10).
           MOVE QPD-EARN-ORDER-10 TO PD-EARN-ORDER(10).
           MOVE QPD-LMO-PRIOR-INTEREST TO PD-LMO-PRIOR-INTEREST.
           MOVE QPD-PRIOR-UNEARNED TO PD-PRIOR-UNEARNED.
           MOVE QPD-ERNDELQ-STOP TO PD-ERNDELQ-STOP.
           MOVE QPD-NONACRERN TO PD-NONACRERN.
           MOVE QPD-PRIOR-NONACRERN TO PD-PRIOR-NONACRERN.

           IF ( QPD-ERN-IB-PDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-ERN-IB-PDTH-DATE
           ELSE
              MOVE QPD-ERN-IB-PDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-ERN-IB-PDTH-DATE.

           MOVE QPD-ERN-IB-PRINBAL TO PD-ERN-IB-PRINBAL.
           MOVE QPD-ERN-IB-INTOWE TO PD-ERN-IB-INTOWE.
           MOVE QPD-TOTERN TO PD-TOTERN.
           MOVE QPD-TOTOTH TO PD-TOTOTH.
           MOVE QPD-TOTINT TO PD-TOTINT.
           MOVE QPD-TOTLCHG TO PD-TOTLCHG.
           MOVE QPD-TOTDEF TO PD-TOTDEF.
           MOVE QPD-TOTSERACCT TO PD-TOTSERACCT.
           MOVE QPD-LCWAIVERS TO PD-LCWAIVERS.
           MOVE QPD-INTWAIVERS TO PD-INTWAIVERS.
           MOVE QPD-TOTPAYMNTD TO PD-TOTPAYMNTD.
           MOVE QPD-TOTEXCPAYMNTD TO PD-TOTEXCPAYMNTD.
           MOVE QPD-TOTNODEF TO PD-TOTNODEF.
           MOVE QPD-YTDNODEF TO PD-YTDNODEF.
           MOVE QPD-TOTNODEFACT TO PD-TOTNODEFACT.
           MOVE QPD-YTDNODEFACT TO PD-YTDNODEFACT.
           MOVE QPD-TOTNOALDEL TO PD-TOTNOALDEL.
           MOVE QPD-YTDNOALDEL TO PD-YTDNOALDEL.
           MOVE QPD-YTDINTPD TO PD-YTDINTPD.
           MOVE QPD-LYRINTPD TO PD-LYRINTPD.
           MOVE QPD-TOTINTPD TO PD-TOTINTPD.
           MOVE QPD-RECASTINT TO PD-RECASTINT.
           MOVE QPD-RECASTPRIN TO PD-RECASTPRIN.
           MOVE QPD-CL-REBATE TO PD-CL-REBATE.
           MOVE QPD-AH-REBATE TO PD-AH-REBATE.
           MOVE QPD-PP-REBATE TO PD-PP-REBATE.
           MOVE QPD-INT-REBATE TO PD-INT-REBATE.
           MOVE QPD-SC-REBATE TO PD-SC-REBATE.
           MOVE QPD-MF-REBATE TO PD-MF-REBATE.
           MOVE QPD-DF-REBATE TO PD-DF-REBATE.
           MOVE QPD-EX-REBATE TO PD-EX-REBATE.
           MOVE QPD-O1-REBATE TO PD-O1-REBATE.
           MOVE QPD-O2-REBATE TO PD-O2-REBATE.
           MOVE QPD-O3-REBATE TO PD-O3-REBATE.
           MOVE QPD-O4-REBATE TO PD-O4-REBATE.
           MOVE QPD-O5-REBATE TO PD-O5-REBATE.
           MOVE QPD-O6-REBATE TO PD-O6-REBATE.
           MOVE QPD-O7-REBATE TO PD-O7-REBATE.
           MOVE QPD-UN-O7-REB TO PD-UN-O7-REB.
           MOVE QPD-UN-O8-REB TO PD-UN-O8-REB.
           MOVE QPD-UN-O9-REB TO PD-UN-O9-REB.
           MOVE QPD-UN-O10-REB TO PD-UN-O10-REB.
           MOVE QPD-PLREASON TO PD-PLREASON.
           MOVE QPD-COLLECTOR TO PD-COLLECTOR.
           MOVE QPD-ACTIONCD TO PD-ACTIONCD.
           MOVE QPD-ACTIONCD2 TO PD-ACTIONCD2.
           MOVE QPD-ATCODE TO PD-ATCODE.
           MOVE QPD-RECODE TO PD-RECODE.
           MOVE QPD-ESCROW-FG TO PD-ESCROW-FG.
           MOVE QPD-PROMDAYS-PASTDUE TO PD-PROMDAYS-PASTDUE.
           MOVE QPD-PROMID TO PD-PROMID.

           IF ( QPD-PROMDATE = "1900-01-01" )
              MOVE ZEROES TO PD-PROMDATE
           ELSE
              MOVE QPD-PROMDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-PROMDATE.

           MOVE QPD-PROMAMT TO PD-PROMAMT.

           IF ( QPD-PROMDATE2 = "1900-01-01" )
              MOVE ZEROES TO PD-PROMDATE2
           ELSE
              MOVE QPD-PROMDATE2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-PROMDATE2.


           IF ( QPD-DATE-PAID-LAST = "1900-01-01" )
              MOVE ZEROES TO PD-DATE-PAID-LAST
           ELSE
              MOVE QPD-DATE-PAID-LAST TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-DATE-PAID-LAST.

           MOVE QPD-AMT-PAID-LAST TO PD-AMT-PAID-LAST.
           MOVE QPD-LASTCFORMNO TO PD-LASTCFORMNO.

           IF ( QPD-LASTCFORMDATE = "1900-01-01" )
              MOVE ZEROES TO PD-LASTCFORMDATE
           ELSE
              MOVE QPD-LASTCFORMDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-LASTCFORMDATE.

           MOVE QPD-DELCNT TO PD-DELCNT.
           MOVE QPD-DELCNT2 TO PD-DELCNT2.
           MOVE QPD-CREDITLIM TO PD-CREDITLIM.
           MOVE QPD-CRSCORE TO PD-CRSCORE.
           MOVE QPD-LASTSFORMNO TO PD-LASTSFORMNO.

           IF ( QPD-LASTSFORMDATE = "1900-01-01" )
              MOVE ZEROES TO PD-LASTSFORMDATE
           ELSE
              MOVE QPD-LASTSFORMDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-LASTSFORMDATE.

           MOVE QPD-SOLCNT TO PD-SOLCNT.
           MOVE QPD-MESSAGE TO PD-MESSAGE.
           MOVE QPD-OLDACCTNO TO PD-OLDACCTNO.
           MOVE QPD-PRLNNO TO PD-PRLNNO.
           MOVE QPD-PRBAL TO PD-PRBAL.
           MOVE QPD-PRINTDUE TO PD-PRINTDUE.
           MOVE QPD-PRFINCHGRB TO PD-PRFINCHGRB.
           MOVE QPD-PRINSRB TO PD-PRINSRB.
           MOVE QPD-PRPOCD TO PD-PRPOCD.

           IF ( QPD-PRPODATE = "1900-01-01" )
              MOVE ZEROES TO PD-PRPODATE
           ELSE
              MOVE QPD-PRPODATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-PRPODATE.

           MOVE QPD-PRDELINQAMT TO PD-PRDELINQAMT.
           MOVE QPD-PRLNAMT TO PD-PRLNAMT.
           MOVE QPD-PRPDUE-DAYS TO PD-PRPDUE-DAYS.
           MOVE QPD-PRDLDISC-UNEARN TO PD-PRDLDISC-UNEARN.
           MOVE QPD-PRCONPROFILE TO PD-PRCONPROFILE.
           MOVE QPD-SUM-ORIGPURAMT TO PD-SUM-ORIGPURAMT.
           MOVE QPD-SUM-LNAMT TO PD-SUM-LNAMT.
           MOVE QPD-SUM-FINCHG TO PD-SUM-FINCHG.
           MOVE QPD-SUM-INTCHG TO PD-SUM-INTCHG.
           MOVE QPD-SUM-ORGTERM TO PD-SUM-ORGTERM.
           MOVE QPD-SUM-REGPYAMT TO PD-SUM-REGPYAMT.
           MOVE QPD-SUM-EXTCHG TO PD-SUM-EXTCHG.
           MOVE QPD-SUM-SERCHG TO PD-SUM-SERCHG.
           MOVE QPD-SUM-MAINTFEE TO PD-SUM-MAINTFEE.
           MOVE QPD-UNITPER-CD TO PD-UNITPER-CD.
           MOVE QPD-UNITPER-FREQ TO PD-UNITPER-FREQ.
           MOVE QPD-DAYS-IN-UNITPER TO PD-DAYS-IN-UNITPER.
           MOVE QPD-UNITPER-PER-YEAR TO PD-UNITPER-PER-YEAR.
           MOVE QPD-REPOCD TO PD-REPOCD.
           MOVE QPD-BR-TFR-FG TO PD-BR-TFR-FG.
           MOVE QPD-O3INS-DLPROC TO PD-O3INS-DLPROC.
           MOVE QPD-INSPREM-DLADJ TO PD-INSPREM-DLADJ.
           MOVE QPD-PLAMT TO PD-PLAMT.
           MOVE QPD-MTD-PRIN TO PD-MTD-PRIN.
           MOVE QPD-MTD-LC TO PD-MTD-LC.
           MOVE QPD-MTD-INT TO PD-MTD-INT.
           MOVE QPD-MTD-OT TO PD-MTD-OT.
           MOVE QPD-MTD-EX TO PD-MTD-EX.
           MOVE QPD-BNKRPT-CHAPTER TO PD-BNKRPT-CHAPTER.
           MOVE QPD-BMO-CON-STATUS TO PD-BMO-CON-STATUS.
           MOVE QPD-BMO-REC-STATUS TO PD-BMO-REC-STATUS.
           MOVE QPD-APP-NUMBER TO PD-APP-NUMBER.
           MOVE QPD-GIFTNET TO PD-GIFTNET.
           MOVE QPD-GIFTNET-OUR-PO TO PD-GIFTNET-OUR-PO.
           MOVE QPD-PROC-DISBURSED TO PD-PROC-DISBURSED.
           MOVE QPD-NOODDPY TO PD-NOODDPY.
           MOVE QPD-CONPROFILE-1 TO PD-CONPROFILE(1).
           MOVE QPD-CONPROFILE-2 TO PD-CONPROFILE(2).
           MOVE QPD-CONPROFILE-3 TO PD-CONPROFILE(3).
           MOVE QPD-CONPROFILE-4 TO PD-CONPROFILE(4).
           MOVE QPD-CONPROFILE-5 TO PD-CONPROFILE(5).
           MOVE QPD-RECPROFILE-1 TO PD-RECPROFILE(1).
           MOVE QPD-RECPROFILE-2 TO PD-RECPROFILE(2).
           MOVE QPD-RECPROFILE-3 TO PD-RECPROFILE(3).
           MOVE QPD-RECPROFILE-4 TO PD-RECPROFILE(4).
           MOVE QPD-RECPROFILE-5 TO PD-RECPROFILE(5).
           MOVE QPD-TRW-STATUS TO PD-TRW-STATUS.
           MOVE QPD-ORIGIN TO PD-ORIGIN.
           MOVE QPD-PO-TRCD TO PD-PO-TRCD.
           MOVE QPD-NEXIS-CRSCORE TO PD-NEXIS-CRSCORE.
           MOVE QPD-TRW-B-CONSUMER-IND TO PD-TRW-B-CONSUMER-IND.
           MOVE QPD-PARVHELD-PAID-FG TO PD-PARVHELD-PAID-FG.
           MOVE QPD-ACCT-FROZEN TO PD-ACCT-FROZEN.
           MOVE QPD-TRW-SPECIAL-CODE TO PD-TRW-SPECIAL-CODE.
           MOVE QPD-CREATION-STATUS TO PD-CREATION-STATUS.
           MOVE QPD-TOTNOLCHG TO PD-TOTNOLCHG.
           MOVE QPD-LBOX TO PD-LBOX.
           MOVE QPD-EXTRA-PRIN-LFP TO PD-EXTRA-PRIN-LFP.
           MOVE QPD-REMAIN-INT-SINCE-LFP TO PD-REMAIN-INT-SINCE-LFP.
           MOVE QPD-REMAIN-PRIN-SINCE-LFP TO PD-REMAIN-PRIN-SINCE-LFP.
           MOVE QPD-INTERIM-COUPON TO PD-INTERIM-COUPON.
           MOVE QPD-DLCOMM-O3-CMSI TO PD-DLCOMM-O3-CMSI.
           MOVE QPD-DLCOMM-PP-CMSI TO PD-DLCOMM-PP-CMSI.
           MOVE QPD-TRW-J-CONSUMER-IND TO PD-TRW-J-CONSUMER-IND.
           MOVE QPD-ACH TO PD-ACH.
           MOVE QPD-1099-C-FG TO PD-1099-C-FG.
           MOVE QPD-TRW-COMPLIANCE-CODE TO PD-TRW-COMPLIANCE-CODE.
           MOVE QPD-UNWIND-REASON-CODE TO PD-UNWIND-REASON-CODE.
           MOVE QPD-PBI-PBD-FG TO PD-PBI-PBD-FG.
           MOVE QPD-OLD-ACCT-FORMAT-FG TO PD-OLD-ACCT-FORMAT-FG.
           MOVE QPD-APPROVED-BY-USER TO PD-APPROVED-BY-USER.
           MOVE QPD-APPROVED-BY-CODE TO PD-APPROVED-BY-CODE.

           IF ( QPD-APPROVED-BY-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-APPROVED-BY-DATE
           ELSE
              MOVE QPD-APPROVED-BY-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-APPROVED-BY-DATE.

           MOVE QPD-APPROVED-BY-TIME TO PD-APPROVED-BY-TIME.

           IF ( QPD-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO PD-LTOUCH-DATE
           ELSE
              MOVE QPD-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PD-LTOUCH-DATE.

           MOVE QPD-LTOUCH-TIME TO PD-LTOUCH-TIME.
           MOVE QPD-HOLD-COLLATERAL TO PD-HOLD-COLLATERAL.
           MOVE QPD-DLPAID TO PD-DLPAID.
           MOVE QPD-CKNO TO PD-CKNO.
           MOVE QPD-FREE1 TO PD-FREE1.
           MOVE QPD-FREE2 TO PD-FREE2.
           MOVE QPD-FREE-SPACE-FILLER TO PD-FREE-SPACE-FILLER.
           MOVE QPD-POSTING-BR TO PD-POSTING-BR.
           MOVE QPD-OTHBR-FG TO PD-OTHBR-FG.
           MOVE QPD-APPROVAL-LIMIT TO PD-APPROVAL-LIMIT.
           MOVE QPD-APPROVAL-USERID TO PD-APPROVAL-USERID.
           MOVE QPD-MIN-CRED-SCORE TO PD-MIN-CRED-SCORE.
           MOVE QPD-CRED-SCORE-USERID TO PD-CRED-SCORE-USERID.
           MOVE QPD-FINAMT TO PD-FINAMT.
           MOVE QPD-PROCEEDS-FG TO PD-PROCEEDS-FG.
           MOVE QPD-POINTS-FG TO PD-POINTS-FG.
           MOVE QPD-OVRDRATE-FG TO PD-OVRDRATE-FG.
           MOVE QPD-INS-FG-1 TO PD-INS-FG(1).
           MOVE QPD-INS-FG-2 TO PD-INS-FG(2).
           MOVE QPD-INS-FG-3 TO PD-INS-FG(3).
           MOVE QPD-INS-FG-4 TO PD-INS-FG(4).
           MOVE QPD-INS-FG-5 TO PD-INS-FG(5).
           MOVE QPD-INS-FG-6 TO PD-INS-FG(6).
           MOVE QPD-INS-FG-7 TO PD-INS-FG(7).
           MOVE QPD-INS-FG-8 TO PD-INS-FG(8).
           MOVE QPD-INS-FG-9 TO PD-INS-FG(9).
           MOVE QPD-INS-FG-10 TO PD-INS-FG(10).
           MOVE QPD-SERCHG-FG TO PD-SERCHG-FG.
           MOVE QPD-MAINTFEE-FG TO PD-MAINTFEE-FG.
           MOVE QPD-EXTCHG-FG TO PD-EXTCHG-FG.
           MOVE QPD-REGPYAMT-FG TO PD-REGPYAMT-FG.
           MOVE QPD-FEECD-1 TO PD-FEECD(1).
           MOVE QPD-FEECD-2 TO PD-FEECD(2).
           MOVE QPD-FEECD-3 TO PD-FEECD(3).
           MOVE QPD-FEECD-4 TO PD-FEECD(4).
           MOVE QPD-FEECD-5 TO PD-FEECD(5).
           MOVE QPD-FEECD-6 TO PD-FEECD(6).
           MOVE QPD-FEECD-7 TO PD-FEECD(7).
           MOVE QPD-FEECD-8 TO PD-FEECD(8).
           MOVE QPD-FEECD-9 TO PD-FEECD(9).
           MOVE QPD-FEECD-10 TO PD-FEECD(10).
           MOVE QPD-FEECD-11 TO PD-FEECD(11).
           MOVE QPD-FEECD-12 TO PD-FEECD(12).
           MOVE QPD-FEECD-13 TO PD-FEECD(13).
           MOVE QPD-FEECD-14 TO PD-FEECD(14).
           MOVE QPD-FEECD-15 TO PD-FEECD(15).
           MOVE QPD-FEECD-16 TO PD-FEECD(16).
           MOVE QPD-FEECD-17 TO PD-FEECD(17).
           MOVE QPD-FEECD-18 TO PD-FEECD(18).
           MOVE QPD-FEECD-19 TO PD-FEECD(19).
           MOVE QPD-FEECD-20 TO PD-FEECD(20).
           MOVE QPD-PRPAID-AHEAD TO PD-PRPAID-AHEAD.
           MOVE QPD-RESTRICT-ADJ TO PD-RESTRICT-ADJ.
           MOVE QPD-DUAL-LOAN-ADJ TO PD-DUAL-LOAN-ADJ.
           MOVE QPD-CAN-LOAN-MAX TO PD-CAN-LOAN-MAX.
           MOVE QPD-PERDOWN TO PD-PERDOWN.
           MOVE QPD-SELLPR-FG TO PD-SELLPR-FG.
           MOVE QPD-DOWNPY-FG TO PD-DOWNPY-FG.
           MOVE QPD-PERDOWN-FG TO PD-PERDOWN-FG.
           MOVE QPD-SELL-DOWN-FG TO PD-SELL-DOWN-FG.
           MOVE QPD-BALLOON-FG TO PD-BALLOON-FG.
           MOVE QPD-BALLOON-LAST-FG TO PD-BALLOON-LAST-FG.
           MOVE QPD-FEECD-FG-1 TO PD-FEECD-FG(1).
           MOVE QPD-FEECD-FG-2 TO PD-FEECD-FG(2).
           MOVE QPD-INS-TERM-SW-1 TO PD-INS-TERM-SW(1).
           MOVE QPD-COVERAGE-SW-1 TO PD-COVERAGE-SW(1).
           MOVE QPD-PREMIUM-SW-1 TO PD-PREMIUM-SW(1).
           MOVE QPD-INS-TERM-SW-2 TO PD-INS-TERM-SW(2).
           MOVE QPD-COVERAGE-SW-2 TO PD-COVERAGE-SW(2).
           MOVE QPD-PREMIUM-SW-2 TO PD-PREMIUM-SW(2).
           MOVE QPD-INS-TERM-SW-3 TO PD-INS-TERM-SW(3).
           MOVE QPD-COVERAGE-SW-3 TO PD-COVERAGE-SW(3).
           MOVE QPD-PREMIUM-SW-3 TO PD-PREMIUM-SW(3).
           MOVE QPD-INS-TERM-SW-4 TO PD-INS-TERM-SW(4).
           MOVE QPD-COVERAGE-SW-4 TO PD-COVERAGE-SW(4).
           MOVE QPD-PREMIUM-SW-4 TO PD-PREMIUM-SW(4).
           MOVE QPD-INS-TERM-SW-5 TO PD-INS-TERM-SW(5).
           MOVE QPD-COVERAGE-SW-5 TO PD-COVERAGE-SW(5).
           MOVE QPD-PREMIUM-SW-5 TO PD-PREMIUM-SW(5).
           MOVE QPD-INS-TERM-SW-6 TO PD-INS-TERM-SW(6).
           MOVE QPD-COVERAGE-SW-6 TO PD-COVERAGE-SW(6).
           MOVE QPD-PREMIUM-SW-6 TO PD-PREMIUM-SW(6).
           MOVE QPD-INS-TERM-SW-7 TO PD-INS-TERM-SW(7).
           MOVE QPD-COVERAGE-SW-7 TO PD-COVERAGE-SW(7).
           MOVE QPD-PREMIUM-SW-7 TO PD-PREMIUM-SW(7).
           MOVE QPD-INS-TERM-SW-8 TO PD-INS-TERM-SW(8).
           MOVE QPD-COVERAGE-SW-8 TO PD-COVERAGE-SW(8).
           MOVE QPD-PREMIUM-SW-8 TO PD-PREMIUM-SW(8).
           MOVE QPD-INS-TERM-SW-9 TO PD-INS-TERM-SW(9).
           MOVE QPD-COVERAGE-SW-9 TO PD-COVERAGE-SW(9).
           MOVE QPD-PREMIUM-SW-9 TO PD-PREMIUM-SW(9).
           MOVE QPD-INS-TERM-SW-10 TO PD-INS-TERM-SW(10).
           MOVE QPD-COVERAGE-SW-10 TO PD-COVERAGE-SW(10).
           MOVE QPD-PREMIUM-SW-10 TO PD-PREMIUM-SW(10).
           MOVE QPD-DLDISC-FG TO PD-DLDISC-FG.
           MOVE QPD-DLRVPRIN-FG-1 TO PD-DLRVPRIN-FG(1).
           MOVE QPD-DLRVPRIN-FG-2 TO PD-DLRVPRIN-FG(2).
           MOVE QPD-DLRVPRIN-FG-3 TO PD-DLRVPRIN-FG(3).
           MOVE QPD-DLRVCHGS-FG-1 TO PD-DLRVCHGS-FG(1).
           MOVE QPD-DLRVCHGS-FG-2 TO PD-DLRVCHGS-FG(2).
           MOVE QPD-DLRVCHGS-FG-3 TO PD-DLRVCHGS-FG(3).
           MOVE QPD-DLPARVHELD-FG TO PD-DLPARVHELD-FG.
           MOVE QPD-DLPARVPAID-FG TO PD-DLPARVPAID-FG.
           MOVE QPD-ONE-MTH-NO-DAY-EXT-SW TO PD-ONE-MTH-NO-DAY-EXT-SW.
           MOVE QPD-ASSIGN-RN-ACCTNO TO PD-ASSIGN-RN-ACCTNO.
           MOVE QPD-APR-FINAMT TO PD-APR-FINAMT.
           MOVE QPD-DLCOMM-PP-CMSI-FG TO PD-DLCOMM-PP-CMSI-FG.
           MOVE QPD-DLCOMM-O3-CMSI-FG TO PD-DLCOMM-O3-CMSI-FG.
           MOVE QPD-FILLER-TO-MATCH-LN TO PD-FILLER-TO-MATCH-LN.
           MOVE QPD2-ID TO PD2-ID.
           MOVE QPD2-BRANCH TO PD2-BRANCH.
           MOVE QPD2-BWNAME TO PD2-BWNAME.
 
      *-----------------------------------------------------------------
      *    LOAD QPD-REC FROM PD-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-PD-FIELDS.
           MOVE PD-ID TO QPD-ID.
           MOVE PD-BRANCH TO QPD-BRANCH.
           MOVE PD-SOC-SEC TO QPD-SOC-SEC.
           MOVE PD-OWNBR TO QPD-OWNBR.
           MOVE PD-ACCTNO TO QPD-ACCTNO.
           MOVE PD-CLASS TO QPD-CLASS.
           MOVE PD-SUBCLASS TO QPD-SUBCLASS.
           MOVE PD-LAWCODE TO QPD-LAWCODE.
           MOVE PD-SSNO1 TO QPD-SSNO1.
           MOVE PD-MAKERCD1 TO QPD-MAKERCD1.
           MOVE PD-SSNO2 TO QPD-SSNO2.
           MOVE PD-MAKERCD2 TO QPD-MAKERCD2.
           MOVE PD-SSNO3 TO QPD-SSNO3.
           MOVE PD-MAKERCD3 TO QPD-MAKERCD3.
           MOVE PD-SSNO4 TO QPD-SSNO4.
           MOVE PD-MAKERCD4 TO QPD-MAKERCD4.
           MOVE PD-ORGST TO QPD-ORGST.
           MOVE PD-ORGBR TO QPD-ORGBR.
           MOVE PD-OWNST TO QPD-OWNST.
           MOVE PD-SPRCLASS TO QPD-SPRCLASS.
           MOVE PD-CONVERCD TO QPD-CONVERCD.
           MOVE PD-MIDSTREAM TO QPD-MIDSTREAM.
           MOVE PD-PURCD TO QPD-PURCD.
           MOVE PD-SRCD TO QPD-SRCD.
           MOVE PD-COLLCD TO QPD-COLLCD.
           MOVE PD-COLLID TO QPD-COLLID.
           MOVE PD-POCD TO QPD-POCD.
           MOVE PD-PLCD TO QPD-PLCD.
           MOVE PD-PLTYPE TO QPD-PLTYPE.
           MOVE PD-CONFIDENTCD TO QPD-CONFIDENTCD.
           MOVE PD-ALLOTCD TO QPD-ALLOTCD.
           MOVE PD-RECASTCD TO QPD-RECASTCD.
           MOVE PD-SKIP-TRW TO QPD-SKIP-TRW.
           MOVE PD-STATUSFG TO QPD-STATUSFG.
           MOVE PD-SMALLOAN TO QPD-SMALLOAN.
           MOVE PD-SALEFIN TO QPD-SALEFIN.
           MOVE PD-SERVACCT TO QPD-SERVACCT.
           MOVE PD-LOANTYPE TO QPD-LOANTYPE.
           MOVE PD-ADDON-FG TO QPD-ADDON-FG.
           MOVE PD-PAY-SCHLD-FG TO QPD-PAY-SCHLD-FG.
           MOVE PD-LC-CODE TO QPD-LC-CODE.
           MOVE PD-CAN-LOAN-NO TO QPD-CAN-LOAN-NO.
           MOVE PD-STATFLAGS TO QPD-STATFLAGS.

           IF ( PD-ENTDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-ENTDATE
           ELSE
              MOVE PD-ENTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-ENTDATE.


           IF ( PD-LOANDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-LOANDATE
           ELSE
              MOVE PD-LOANDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-LOANDATE.


           IF ( PD-PURDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-PURDATE
           ELSE
              MOVE PD-PURDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-PURDATE.


           IF ( PD-INTDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-INTDATE
           ELSE
              MOVE PD-INTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-INTDATE.


           IF ( PD-CASHDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-CASHDATE
           ELSE
              MOVE PD-CASHDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-CASHDATE.


           IF ( PD-FILEDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-FILEDATE
           ELSE
              MOVE PD-FILEDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-FILEDATE.


           IF ( PD-RESCISSION-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-RESCISSION-DATE
           ELSE
              MOVE PD-RESCISSION-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-RESCISSION-DATE.


           IF ( PD-PART-RECOURSE-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-PART-RECOURSE-DATE
           ELSE
              MOVE PD-PART-RECOURSE-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-PART-RECOURSE-DATE.


           IF ( PD-ORIG-1STPYDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-ORIG-1STPYDATE
           ELSE
              MOVE PD-ORIG-1STPYDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-ORIG-1STPYDATE.


           IF ( PD-1STPYDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-1STPYDATE
           ELSE
              MOVE PD-1STPYDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-1STPYDATE.


           IF ( PD-JDDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-JDDATE
           ELSE
              MOVE PD-JDDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-JDDATE.


           IF ( PD-PLDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-PLDATE
           ELSE
              MOVE PD-PLDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-PLDATE.


           IF ( PD-PL-TRDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-PL-TRDATE
           ELSE
              MOVE PD-PL-TRDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-PL-TRDATE.


           IF ( PD-ACCELDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-ACCELDATE
           ELSE
              MOVE PD-ACCELDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-ACCELDATE.


           IF ( PD-REDUDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-REDUDATE
           ELSE
              MOVE PD-REDUDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-REDUDATE.


           IF ( PD-POFFDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-POFFDATE
           ELSE
              MOVE PD-POFFDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-POFFDATE.


           IF ( PD-PO-TRDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-PO-TRDATE
           ELSE
              MOVE PD-PO-TRDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-PO-TRDATE.


           IF ( PD-LEGALDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-LEGALDATE
           ELSE
              MOVE PD-LEGALDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-LEGALDATE.


           IF ( PD-BNKRPTDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-BNKRPTDATE
           ELSE
              MOVE PD-BNKRPTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-BNKRPTDATE.


           IF ( PD-BNKRPT-DISMISS-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-BNKRPT-DISMISS-DATE
           ELSE
              MOVE PD-BNKRPT-DISMISS-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-BNKRPT-DISMISS-DATE.


           IF ( PD-BNKRPT-DISCHARGE-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-BNKRPT-DISCHARGE-DATE
           ELSE
              MOVE PD-BNKRPT-DISCHARGE-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-BNKRPT-DISCHARGE-DATE.


           IF ( PD-BNKRPT-REAFFIRM-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-BNKRPT-REAFFIRM-DATE
           ELSE
              MOVE PD-BNKRPT-REAFFIRM-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-BNKRPT-REAFFIRM-DATE.


           IF ( PD-DEBTAIDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-DEBTAIDATE
           ELSE
              MOVE PD-DEBTAIDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-DEBTAIDATE.


           IF ( PD-REPODATE = ZEROES )
              MOVE "1900-01-01" TO QPD-REPODATE
           ELSE
              MOVE PD-REPODATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-REPODATE.


           IF ( PD-REDEEMDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-REDEEMDATE
           ELSE
              MOVE PD-REDEEMDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-REDEEMDATE.


           IF ( PD-SOLDDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-SOLDDATE
           ELSE
              MOVE PD-SOLDDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-SOLDDATE.


           IF ( PD-INS-CONFIRM-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-INS-CONFIRM-DATE
           ELSE
              MOVE PD-INS-CONFIRM-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-INS-CONFIRM-DATE.


           IF ( PD-PRIOR-RN-FOR10-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-PRIOR-RN-FOR10-DATE
           ELSE
              MOVE PD-PRIOR-RN-FOR10-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-PRIOR-RN-FOR10-DATE.


           IF ( PD-LAST-RN-FOR10-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-LAST-RN-FOR10-DATE
           ELSE
              MOVE PD-LAST-RN-FOR10-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-LAST-RN-FOR10-DATE.


           IF ( PD-TRW-DOFD-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-TRW-DOFD-DATE
           ELSE
              MOVE PD-TRW-DOFD-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-TRW-DOFD-DATE.


           IF ( PD-LAST-PYPA-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-LAST-PYPA-DATE
           ELSE
              MOVE PD-LAST-PYPA-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-LAST-PYPA-DATE.


           IF ( PD-BT-TRDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-BT-TRDATE
           ELSE
              MOVE PD-BT-TRDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-BT-TRDATE.


           IF ( PD-METRO2-LAST-PMT-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-METRO2-LAST-PMT-DATE
           ELSE
              MOVE PD-METRO2-LAST-PMT-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-METRO2-LAST-PMT-DATE.


           IF ( PD-FOLLOWUP-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-FOLLOWUP-DATE
           ELSE
              MOVE PD-FOLLOWUP-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-FOLLOWUP-DATE.


           IF ( PD-LAST-ACCRUAL-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-LAST-ACCRUAL-DATE
           ELSE
              MOVE PD-LAST-ACCRUAL-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-LAST-ACCRUAL-DATE.

           MOVE PD-APRATE TO QPD-APRATE.
           MOVE PD-EFFRATE TO QPD-EFFRATE.
           MOVE PD-SMPRATE TO QPD-SMPRATE.
           MOVE PD-YLDRATE TO QPD-YLDRATE.
           MOVE PD-OVRDRATE TO QPD-OVRDRATE.
           MOVE PD-ENDORATE TO QPD-ENDORATE.
           MOVE PD-JDRATE TO QPD-JDRATE.
           MOVE PD-ACCRATE TO QPD-ACCRATE.
           MOVE PD-REDURATE TO QPD-REDURATE.
           MOVE PD-CONRATE TO QPD-CONRATE.
           MOVE PD-SERVPER TO QPD-SERVPER.
           MOVE PD-SELLPR TO QPD-SELLPR.
           MOVE PD-DOWNPY TO QPD-DOWNPY.
           MOVE PD-TRADEIN TO QPD-TRADEIN.
           MOVE PD-LNAMT TO QPD-LNAMT.
           MOVE PD-FINCHG TO QPD-FINCHG.
           MOVE PD-INTCHG TO QPD-INTCHG.
           MOVE PD-SERCHG TO QPD-SERCHG.
           MOVE PD-EXTCHG TO QPD-EXTCHG.
           MOVE PD-MAINTFEE TO QPD-MAINTFEE.
           MOVE PD-POINTS TO QPD-POINTS.
           MOVE PD-PROCEEDS TO QPD-PROCEEDS.
           MOVE PD-ORGTERM TO QPD-ORGTERM.
           MOVE PD-1STPYAMT TO QPD-1STPYAMT.
           MOVE PD-NOREGPY TO QPD-NOREGPY.
           MOVE PD-REGPYAMT TO QPD-REGPYAMT.
           MOVE PD-LASTPYAMT TO QPD-LASTPYAMT.
           MOVE PD-PROCASH TO QPD-PROCASH.
           MOVE PD-TILLNO TO QPD-TILLNO.
           MOVE PD-STATE-FLAG TO QPD-STATE-FLAG.
           MOVE PD-STATE-WORKER TO QPD-STATE-WORKER.
           MOVE PD-STATE-WORKER2 TO QPD-STATE-WORKER2.
           MOVE PD-FEECODE(1) TO QPD-FEECODE-1.
           MOVE PD-FEEAMT(1) TO QPD-FEEAMT-1.
           MOVE PD-FEEOURS(1) TO QPD-FEEOURS-1.
           MOVE PD-FEEFINCHG(1) TO QPD-FEEFINCHG-1.
           MOVE PD-FEECODE(2) TO QPD-FEECODE-2.
           MOVE PD-FEEAMT(2) TO QPD-FEEAMT-2.
           MOVE PD-FEEOURS(2) TO QPD-FEEOURS-2.
           MOVE PD-FEEFINCHG(2) TO QPD-FEEFINCHG-2.
           MOVE PD-FEECODE(3) TO QPD-FEECODE-3.
           MOVE PD-FEEAMT(3) TO QPD-FEEAMT-3.
           MOVE PD-FEEOURS(3) TO QPD-FEEOURS-3.
           MOVE PD-FEEFINCHG(3) TO QPD-FEEFINCHG-3.
           MOVE PD-FEECODE(4) TO QPD-FEECODE-4.
           MOVE PD-FEEAMT(4) TO QPD-FEEAMT-4.
           MOVE PD-FEEOURS(4) TO QPD-FEEOURS-4.
           MOVE PD-FEEFINCHG(4) TO QPD-FEEFINCHG-4.
           MOVE PD-FEECODE(5) TO QPD-FEECODE-5.
           MOVE PD-FEEAMT(5) TO QPD-FEEAMT-5.
           MOVE PD-FEEOURS(5) TO QPD-FEEOURS-5.
           MOVE PD-FEEFINCHG(5) TO QPD-FEEFINCHG-5.
           MOVE PD-FEECODE(6) TO QPD-FEECODE-6.
           MOVE PD-FEEAMT(6) TO QPD-FEEAMT-6.
           MOVE PD-FEEOURS(6) TO QPD-FEEOURS-6.
           MOVE PD-FEEFINCHG(6) TO QPD-FEEFINCHG-6.
           MOVE PD-FEECODE(7) TO QPD-FEECODE-7.
           MOVE PD-FEEAMT(7) TO QPD-FEEAMT-7.
           MOVE PD-FEEOURS(7) TO QPD-FEEOURS-7.
           MOVE PD-FEEFINCHG(7) TO QPD-FEEFINCHG-7.
           MOVE PD-FEECODE(8) TO QPD-FEECODE-8.
           MOVE PD-FEEAMT(8) TO QPD-FEEAMT-8.
           MOVE PD-FEEOURS(8) TO QPD-FEEOURS-8.
           MOVE PD-FEEFINCHG(8) TO QPD-FEEFINCHG-8.
           MOVE PD-FEECODE(9) TO QPD-FEECODE-9.
           MOVE PD-FEEAMT(9) TO QPD-FEEAMT-9.
           MOVE PD-FEEOURS(9) TO QPD-FEEOURS-9.
           MOVE PD-FEEFINCHG(9) TO QPD-FEEFINCHG-9.
           MOVE PD-FEECODE(10) TO QPD-FEECODE-10.
           MOVE PD-FEEAMT(10) TO QPD-FEEAMT-10.
           MOVE PD-FEEOURS(10) TO QPD-FEEOURS-10.
           MOVE PD-FEEFINCHG(10) TO QPD-FEEFINCHG-10.
           MOVE PD-FEECODE(11) TO QPD-FEECODE-11.
           MOVE PD-FEEAMT(11) TO QPD-FEEAMT-11.
           MOVE PD-FEEOURS(11) TO QPD-FEEOURS-11.
           MOVE PD-FEEFINCHG(11) TO QPD-FEEFINCHG-11.
           MOVE PD-FEECODE(12) TO QPD-FEECODE-12.
           MOVE PD-FEEAMT(12) TO QPD-FEEAMT-12.
           MOVE PD-FEEOURS(12) TO QPD-FEEOURS-12.
           MOVE PD-FEEFINCHG(12) TO QPD-FEEFINCHG-12.
           MOVE PD-FEECODE(13) TO QPD-FEECODE-13.
           MOVE PD-FEEAMT(13) TO QPD-FEEAMT-13.
           MOVE PD-FEEOURS(13) TO QPD-FEEOURS-13.
           MOVE PD-FEEFINCHG(13) TO QPD-FEEFINCHG-13.
           MOVE PD-FEECODE(14) TO QPD-FEECODE-14.
           MOVE PD-FEEAMT(14) TO QPD-FEEAMT-14.
           MOVE PD-FEEOURS(14) TO QPD-FEEOURS-14.
           MOVE PD-FEEFINCHG(14) TO QPD-FEEFINCHG-14.
           MOVE PD-FEECODE(15) TO QPD-FEECODE-15.
           MOVE PD-FEEAMT(15) TO QPD-FEEAMT-15.
           MOVE PD-FEEOURS(15) TO QPD-FEEOURS-15.
           MOVE PD-FEEFINCHG(15) TO QPD-FEEFINCHG-15.
           MOVE PD-FEECODE(16) TO QPD-FEECODE-16.
           MOVE PD-FEEAMT(16) TO QPD-FEEAMT-16.
           MOVE PD-FEEOURS(16) TO QPD-FEEOURS-16.
           MOVE PD-FEEFINCHG(16) TO QPD-FEEFINCHG-16.
           MOVE PD-FEECODE(17) TO QPD-FEECODE-17.
           MOVE PD-FEEAMT(17) TO QPD-FEEAMT-17.
           MOVE PD-FEEOURS(17) TO QPD-FEEOURS-17.
           MOVE PD-FEEFINCHG(17) TO QPD-FEEFINCHG-17.
           MOVE PD-FEECODE(18) TO QPD-FEECODE-18.
           MOVE PD-FEEAMT(18) TO QPD-FEEAMT-18.
           MOVE PD-FEEOURS(18) TO QPD-FEEOURS-18.
           MOVE PD-FEEFINCHG(18) TO QPD-FEEFINCHG-18.
           MOVE PD-FEECODE(19) TO QPD-FEECODE-19.
           MOVE PD-FEEAMT(19) TO QPD-FEEAMT-19.
           MOVE PD-FEEOURS(19) TO QPD-FEEOURS-19.
           MOVE PD-FEEFINCHG(19) TO QPD-FEEFINCHG-19.
           MOVE PD-FEECODE(20) TO QPD-FEECODE-20.
           MOVE PD-FEEAMT(20) TO QPD-FEEAMT-20.
           MOVE PD-FEEOURS(20) TO QPD-FEEOURS-20.
           MOVE PD-FEEFINCHG(20) TO QPD-FEEFINCHG-20.
           MOVE PD-INSURED TO QPD-INSURED.
           MOVE PD-INSTYPES TO QPD-INSTYPES.
           MOVE PD-INSELIGIBLE TO QPD-INSELIGIBLE.
           MOVE PD-INSCOMP(1) TO QPD-INSCOMP-1.
           MOVE PD-INSOURS(1) TO QPD-INSOURS-1.

           MOVE PD-INSEFF-DATE(1) TO QPD-INSEFF-DATE-1.

           IF ( PD-INSEXP-DATE(1) = ZEROES )
              MOVE "1900-01-01" TO QPD-INSEXP-DATE-1
           ELSE
              MOVE PD-INSEXP-DATE(1) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-INSEXP-DATE-1.

           MOVE PD-INSPREM(1) TO QPD-INSPREM-1.
           MOVE PD-INSCOVR(1) TO QPD-INSCOVR-1.
           MOVE PD-INSCOMM(1) TO QPD-INSCOMM-1.
           MOVE PD-INSANTICERN(1) TO QPD-INSANTICERN-1.
           MOVE PD-INSANTICADJ(1) TO QPD-INSANTICADJ-1.
           MOVE PD-INSUFERN(1) TO QPD-INSUFERN-1.
           MOVE PD-INSACCERN(1) TO QPD-INSACCERN-1.
           MOVE PD-INSEARN-ORDER(1) TO QPD-INSEARN-ORDER-1.
           MOVE PD-INSFINCHG(1) TO QPD-INSFINCHG-1.
           MOVE PD-INSCOMP(2) TO QPD-INSCOMP-2.
           MOVE PD-INSOURS(2) TO QPD-INSOURS-2.

           MOVE PD-INSEFF-DATE(2) TO QPD-INSEFF-DATE-2.

           IF ( PD-INSEXP-DATE(2) = ZEROES )
              MOVE "1900-01-01" TO QPD-INSEXP-DATE-2
           ELSE
              MOVE PD-INSEXP-DATE(2) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-INSEXP-DATE-2.

           MOVE PD-INSPREM(2) TO QPD-INSPREM-2.
           MOVE PD-INSCOVR(2) TO QPD-INSCOVR-2.
           MOVE PD-INSCOMM(2) TO QPD-INSCOMM-2.
           MOVE PD-INSANTICERN(2) TO QPD-INSANTICERN-2.
           MOVE PD-INSANTICADJ(2) TO QPD-INSANTICADJ-2.
           MOVE PD-INSUFERN(2) TO QPD-INSUFERN-2.
           MOVE PD-INSACCERN(2) TO QPD-INSACCERN-2.
           MOVE PD-INSEARN-ORDER(2) TO QPD-INSEARN-ORDER-2.
           MOVE PD-INSFINCHG(2) TO QPD-INSFINCHG-2.
           MOVE PD-INSCOMP(3) TO QPD-INSCOMP-3.
           MOVE PD-INSOURS(3) TO QPD-INSOURS-3.

           MOVE PD-INSEFF-DATE(3) TO QPD-INSEFF-DATE-3.

           IF ( PD-INSEXP-DATE(3) = ZEROES )
              MOVE "1900-01-01" TO QPD-INSEXP-DATE-3
           ELSE
              MOVE PD-INSEXP-DATE(3) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-INSEXP-DATE-3.

           MOVE PD-INSPREM(3) TO QPD-INSPREM-3.
           MOVE PD-INSCOVR(3) TO QPD-INSCOVR-3.
           MOVE PD-INSCOMM(3) TO QPD-INSCOMM-3.
           MOVE PD-INSANTICERN(3) TO QPD-INSANTICERN-3.
           MOVE PD-INSANTICADJ(3) TO QPD-INSANTICADJ-3.
           MOVE PD-INSUFERN(3) TO QPD-INSUFERN-3.
           MOVE PD-INSACCERN(3) TO QPD-INSACCERN-3.
           MOVE PD-INSEARN-ORDER(3) TO QPD-INSEARN-ORDER-3.
           MOVE PD-INSFINCHG(3) TO QPD-INSFINCHG-3.
           MOVE PD-INSCOMP(4) TO QPD-INSCOMP-4.
           MOVE PD-INSOURS(4) TO QPD-INSOURS-4.

           MOVE PD-INSEFF-DATE(4) TO QPD-INSEFF-DATE-4.

           IF ( PD-INSEXP-DATE(4) = ZEROES )
              MOVE "1900-01-01" TO QPD-INSEXP-DATE-4
           ELSE
              MOVE PD-INSEXP-DATE(4) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-INSEXP-DATE-4.

           MOVE PD-INSPREM(4) TO QPD-INSPREM-4.
           MOVE PD-INSCOVR(4) TO QPD-INSCOVR-4.
           MOVE PD-INSCOMM(4) TO QPD-INSCOMM-4.
           MOVE PD-INSANTICERN(4) TO QPD-INSANTICERN-4.
           MOVE PD-INSANTICADJ(4) TO QPD-INSANTICADJ-4.
           MOVE PD-INSUFERN(4) TO QPD-INSUFERN-4.
           MOVE PD-INSACCERN(4) TO QPD-INSACCERN-4.
           MOVE PD-INSEARN-ORDER(4) TO QPD-INSEARN-ORDER-4.
           MOVE PD-INSFINCHG(4) TO QPD-INSFINCHG-4.
           MOVE PD-INSCOMP(5) TO QPD-INSCOMP-5.
           MOVE PD-INSOURS(5) TO QPD-INSOURS-5.

           MOVE PD-INSEFF-DATE(5) TO QPD-INSEFF-DATE-5.

           IF ( PD-INSEXP-DATE(5) = ZEROES )
              MOVE "1900-01-01" TO QPD-INSEXP-DATE-5
           ELSE
              MOVE PD-INSEXP-DATE(5) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-INSEXP-DATE-5.

           MOVE PD-INSPREM(5) TO QPD-INSPREM-5.
           MOVE PD-INSCOVR(5) TO QPD-INSCOVR-5.
           MOVE PD-INSCOMM(5) TO QPD-INSCOMM-5.
           MOVE PD-INSANTICERN(5) TO QPD-INSANTICERN-5.
           MOVE PD-INSANTICADJ(5) TO QPD-INSANTICADJ-5.
           MOVE PD-INSUFERN(5) TO QPD-INSUFERN-5.
           MOVE PD-INSACCERN(5) TO QPD-INSACCERN-5.
           MOVE PD-INSEARN-ORDER(5) TO QPD-INSEARN-ORDER-5.
           MOVE PD-INSFINCHG(5) TO QPD-INSFINCHG-5.
           MOVE PD-INSCOMP(6) TO QPD-INSCOMP-6.
           MOVE PD-INSOURS(6) TO QPD-INSOURS-6.

           MOVE PD-INSEFF-DATE(6) TO QPD-INSEFF-DATE-6.

           IF ( PD-INSEXP-DATE(6) = ZEROES )
              MOVE "1900-01-01" TO QPD-INSEXP-DATE-6
           ELSE
              MOVE PD-INSEXP-DATE(6) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-INSEXP-DATE-6.

           MOVE PD-INSPREM(6) TO QPD-INSPREM-6.
           MOVE PD-INSCOVR(6) TO QPD-INSCOVR-6.
           MOVE PD-INSCOMM(6) TO QPD-INSCOMM-6.
           MOVE PD-INSANTICERN(6) TO QPD-INSANTICERN-6.
           MOVE PD-INSANTICADJ(6) TO QPD-INSANTICADJ-6.
           MOVE PD-INSUFERN(6) TO QPD-INSUFERN-6.
           MOVE PD-INSACCERN(6) TO QPD-INSACCERN-6.
           MOVE PD-INSEARN-ORDER(6) TO QPD-INSEARN-ORDER-6.
           MOVE PD-INSFINCHG(6) TO QPD-INSFINCHG-6.
           MOVE PD-INSCOMP(7) TO QPD-INSCOMP-7.
           MOVE PD-INSOURS(7) TO QPD-INSOURS-7.

           MOVE PD-INSEFF-DATE(7) TO QPD-INSEFF-DATE-7.

           IF ( PD-INSEXP-DATE(7) = ZEROES )
              MOVE "1900-01-01" TO QPD-INSEXP-DATE-7
           ELSE
              MOVE PD-INSEXP-DATE(7) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-INSEXP-DATE-7.

           MOVE PD-INSPREM(7) TO QPD-INSPREM-7.
           MOVE PD-INSCOVR(7) TO QPD-INSCOVR-7.
           MOVE PD-INSCOMM(7) TO QPD-INSCOMM-7.
           MOVE PD-INSANTICERN(7) TO QPD-INSANTICERN-7.
           MOVE PD-INSANTICADJ(7) TO QPD-INSANTICADJ-7.
           MOVE PD-INSUFERN(7) TO QPD-INSUFERN-7.
           MOVE PD-INSACCERN(7) TO QPD-INSACCERN-7.
           MOVE PD-INSEARN-ORDER(7) TO QPD-INSEARN-ORDER-7.
           MOVE PD-INSFINCHG(7) TO QPD-INSFINCHG-7.
           MOVE PD-INSCOMP(8) TO QPD-INSCOMP-8.
           MOVE PD-INSOURS(8) TO QPD-INSOURS-8.

           MOVE PD-INSEFF-DATE(8) TO QPD-INSEFF-DATE-8.

           IF ( PD-INSEXP-DATE(8) = ZEROES )
              MOVE "1900-01-01" TO QPD-INSEXP-DATE-8
           ELSE
              MOVE PD-INSEXP-DATE(8) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-INSEXP-DATE-8.

           MOVE PD-INSPREM(8) TO QPD-INSPREM-8.
           MOVE PD-INSCOVR(8) TO QPD-INSCOVR-8.
           MOVE PD-INSCOMM(8) TO QPD-INSCOMM-8.
           MOVE PD-INSANTICERN(8) TO QPD-INSANTICERN-8.
           MOVE PD-INSANTICADJ(8) TO QPD-INSANTICADJ-8.
           MOVE PD-INSUFERN(8) TO QPD-INSUFERN-8.
           MOVE PD-INSACCERN(8) TO QPD-INSACCERN-8.
           MOVE PD-INSEARN-ORDER(8) TO QPD-INSEARN-ORDER-8.
           MOVE PD-INSFINCHG(8) TO QPD-INSFINCHG-8.
           MOVE PD-INSCOMP(9) TO QPD-INSCOMP-9.
           MOVE PD-INSOURS(9) TO QPD-INSOURS-9.

           MOVE PD-INSEFF-DATE(9) TO QPD-INSEFF-DATE-9.

           IF ( PD-INSEXP-DATE(9) = ZEROES )
              MOVE "1900-01-01" TO QPD-INSEXP-DATE-9
           ELSE
              MOVE PD-INSEXP-DATE(9) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-INSEXP-DATE-9.

           MOVE PD-INSPREM(9) TO QPD-INSPREM-9.
           MOVE PD-INSCOVR(9) TO QPD-INSCOVR-9.
           MOVE PD-INSCOMM(9) TO QPD-INSCOMM-9.
           MOVE PD-INSANTICERN(9) TO QPD-INSANTICERN-9.
           MOVE PD-INSANTICADJ(9) TO QPD-INSANTICADJ-9.
           MOVE PD-INSUFERN(9) TO QPD-INSUFERN-9.
           MOVE PD-INSACCERN(9) TO QPD-INSACCERN-9.
           MOVE PD-INSEARN-ORDER(9) TO QPD-INSEARN-ORDER-9.
           MOVE PD-INSFINCHG(9) TO QPD-INSFINCHG-9.
           MOVE PD-INSCOMP(10) TO QPD-INSCOMP-10.
           MOVE PD-INSOURS(10) TO QPD-INSOURS-10.

           MOVE PD-INSEFF-DATE(10) TO QPD-INSEFF-DATE-10.

           IF ( PD-INSEXP-DATE(10) = ZEROES )
              MOVE "1900-01-01" TO QPD-INSEXP-DATE-10
           ELSE
              MOVE PD-INSEXP-DATE(10) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-INSEXP-DATE-10.

           MOVE PD-INSPREM(10) TO QPD-INSPREM-10.
           MOVE PD-INSCOVR(10) TO QPD-INSCOVR-10.
           MOVE PD-INSCOMM(10) TO QPD-INSCOMM-10.
           MOVE PD-INSANTICERN(10) TO QPD-INSANTICERN-10.
           MOVE PD-INSANTICADJ(10) TO QPD-INSANTICADJ-10.
           MOVE PD-INSUFERN(10) TO QPD-INSUFERN-10.
           MOVE PD-INSACCERN(10) TO QPD-INSACCERN-10.
           MOVE PD-INSEARN-ORDER(10) TO QPD-INSEARN-ORDER-10.
           MOVE PD-INSFINCHG(10) TO QPD-INSFINCHG-10.
           MOVE PD-PY-YYYYMM-RECDEL TO QPD-PY-YYYYMM-RECDEL.
           MOVE PD-MTD-PAYS-RECDEL TO QPD-MTD-PAYS-RECDEL.
           MOVE PD-GARNISH-EXTR TO QPD-GARNISH-EXTR.
           MOVE PD-SHIP-SAME-FG TO QPD-SHIP-SAME-FG.
           MOVE PD-AD-D-INS-COVR TO QPD-AD-D-INS-COVR.
           MOVE PD-TOT-PARTIAL-CHGOFF TO QPD-TOT-PARTIAL-CHGOFF.
           MOVE PD-LPO TO QPD-LPO.
           MOVE PD-REP-CD TO QPD-REP-CD.
           MOVE PD-EMP-ID TO QPD-EMP-ID.
           MOVE PD-PROCESS-CD TO QPD-PROCESS-CD.
           MOVE PD-WCBC-CREDITLIM TO QPD-WCBC-CREDITLIM.
           MOVE PD-FOLLOWUP-CD TO QPD-FOLLOWUP-CD.
           MOVE PD-MIP-FG TO QPD-MIP-FG.
           MOVE PD-MIP-MTD-CLAHPAID TO QPD-MIP-MTD-CLAHPAID.

           IF ( PD-MIP-CLPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-MIP-CLPDTH-DATE
           ELSE
              MOVE PD-MIP-CLPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-MIP-CLPDTH-DATE.

           MOVE PD-MIP-TOT-CLPAID TO QPD-MIP-TOT-CLPAID.

           IF ( PD-MIP-AHPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-MIP-AHPDTH-DATE
           ELSE
              MOVE PD-MIP-AHPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-MIP-AHPDTH-DATE.

           MOVE PD-MIP-TOT-AHPAID TO QPD-MIP-TOT-AHPAID.
           MOVE PD-IL-GROSS-INCOME TO QPD-IL-GROSS-INCOME.
           MOVE PD-IL-GROSS-INCOME2 TO QPD-IL-GROSS-INCOME2.
           MOVE PD-RESCISSION TO QPD-RESCISSION.
           MOVE PD-INTERIM-TRW TO QPD-INTERIM-TRW.
           MOVE PD-DLNO TO QPD-DLNO.
           MOVE PD-DLPOOL TO QPD-DLPOOL.
           MOVE PD-DLRECOURSE TO QPD-DLRECOURSE.
           MOVE PD-DLPARTRCAMT TO QPD-DLPARTRCAMT.
           MOVE PD-ORIGPURAMT TO QPD-ORIGPURAMT.
           MOVE PD-PURBAL TO QPD-PURBAL.
           MOVE PD-DLRVPRIN(1) TO QPD-DLRVPRIN-1.
           MOVE PD-DLRVPRIN(2) TO QPD-DLRVPRIN-2.
           MOVE PD-DLRVPRIN(3) TO QPD-DLRVPRIN-3.
           MOVE PD-DLRVCHGS(1) TO QPD-DLRVCHGS-1.
           MOVE PD-DLRVCHGS(2) TO QPD-DLRVCHGS-2.
           MOVE PD-DLRVCHGS(3) TO QPD-DLRVCHGS-3.
           MOVE PD-DLPARVHELD TO QPD-DLPARVHELD.
           MOVE PD-DLPARVPAID TO QPD-DLPARVPAID.
           MOVE PD-DLDISC TO QPD-DLDISC.

           IF ( PD-LCPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-LCPDTH-DATE
           ELSE
              MOVE PD-LCPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-LCPDTH-DATE.


           IF ( PD-INTPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-INTPDTH-DATE
           ELSE
              MOVE PD-INTPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-INTPDTH-DATE.

           MOVE PD-CURBAL TO QPD-CURBAL.
           MOVE PD-INTBAL TO QPD-INTBAL.
           MOVE PD-LCBAL TO QPD-LCBAL.
           MOVE PD-OTHBAL TO QPD-OTHBAL.
           MOVE PD-OT2BAL TO QPD-OT2BAL.
           MOVE PD-LCPAID TO QPD-LCPAID.
           MOVE PD-LCPARTIALS TO QPD-LCPARTIALS.
           MOVE PD-STMT-COUNTER TO QPD-STMT-COUNTER.
           MOVE PD-PRIOR-STMT-COUNTER TO QPD-PRIOR-STMT-COUNTER.
           MOVE PD-FROZEN-REASON TO QPD-FROZEN-REASON.
           MOVE PD-COUPON-STATEMENT TO QPD-COUPON-STATEMENT.
           MOVE PD-MONTHEND-BAL TO QPD-MONTHEND-BAL.
           MOVE PD-ANTICTERM TO QPD-ANTICTERM.
           MOVE PD-ANTICERN(1) TO QPD-ANTICERN-1.
           MOVE PD-ANTICADJ(1) TO QPD-ANTICADJ-1.
           MOVE PD-UFERN(1) TO QPD-UFERN-1.
           MOVE PD-ACCERN(1) TO QPD-ACCERN-1.
           MOVE PD-EARN-ORDER(1) TO QPD-EARN-ORDER-1.
           MOVE PD-ANTICERN(2) TO QPD-ANTICERN-2.
           MOVE PD-ANTICADJ(2) TO QPD-ANTICADJ-2.
           MOVE PD-UFERN(2) TO QPD-UFERN-2.
           MOVE PD-ACCERN(2) TO QPD-ACCERN-2.
           MOVE PD-EARN-ORDER(2) TO QPD-EARN-ORDER-2.
           MOVE PD-ANTICERN(3) TO QPD-ANTICERN-3.
           MOVE PD-ANTICADJ(3) TO QPD-ANTICADJ-3.
           MOVE PD-UFERN(3) TO QPD-UFERN-3.
           MOVE PD-ACCERN(3) TO QPD-ACCERN-3.
           MOVE PD-EARN-ORDER(3) TO QPD-EARN-ORDER-3.
           MOVE PD-ANTICERN(4) TO QPD-ANTICERN-4.
           MOVE PD-ANTICADJ(4) TO QPD-ANTICADJ-4.
           MOVE PD-UFERN(4) TO QPD-UFERN-4.
           MOVE PD-ACCERN(4) TO QPD-ACCERN-4.
           MOVE PD-EARN-ORDER(4) TO QPD-EARN-ORDER-4.
           MOVE PD-ANTICERN(5) TO QPD-ANTICERN-5.
           MOVE PD-ANTICADJ(5) TO QPD-ANTICADJ-5.
           MOVE PD-UFERN(5) TO QPD-UFERN-5.
           MOVE PD-ACCERN(5) TO QPD-ACCERN-5.
           MOVE PD-EARN-ORDER(5) TO QPD-EARN-ORDER-5.
           MOVE PD-ANTICERN(6) TO QPD-ANTICERN-6.
           MOVE PD-ANTICADJ(6) TO QPD-ANTICADJ-6.
           MOVE PD-UFERN(6) TO QPD-UFERN-6.
           MOVE PD-ACCERN(6) TO QPD-ACCERN-6.
           MOVE PD-EARN-ORDER(6) TO QPD-EARN-ORDER-6.
           MOVE PD-ANTICERN(7) TO QPD-ANTICERN-7.
           MOVE PD-ANTICADJ(7) TO QPD-ANTICADJ-7.
           MOVE PD-UFERN(7) TO QPD-UFERN-7.
           MOVE PD-ACCERN(7) TO QPD-ACCERN-7.
           MOVE PD-EARN-ORDER(7) TO QPD-EARN-ORDER-7.
           MOVE PD-ANTICERN(8) TO QPD-ANTICERN-8.
           MOVE PD-ANTICADJ(8) TO QPD-ANTICADJ-8.
           MOVE PD-UFERN(8) TO QPD-UFERN-8.
           MOVE PD-ACCERN(8) TO QPD-ACCERN-8.
           MOVE PD-EARN-ORDER(8) TO QPD-EARN-ORDER-8.
           MOVE PD-ANTICERN(9) TO QPD-ANTICERN-9.
           MOVE PD-ANTICADJ(9) TO QPD-ANTICADJ-9.
           MOVE PD-UFERN(9) TO QPD-UFERN-9.
           MOVE PD-ACCERN(9) TO QPD-ACCERN-9.
           MOVE PD-EARN-ORDER(9) TO QPD-EARN-ORDER-9.
           MOVE PD-ANTICERN(10) TO QPD-ANTICERN-10.
           MOVE PD-ANTICADJ(10) TO QPD-ANTICADJ-10.
           MOVE PD-UFERN(10) TO QPD-UFERN-10.
           MOVE PD-ACCERN(10) TO QPD-ACCERN-10.
           MOVE PD-EARN-ORDER(10) TO QPD-EARN-ORDER-10.
           MOVE PD-LMO-PRIOR-INTEREST TO QPD-LMO-PRIOR-INTEREST.
           MOVE PD-PRIOR-UNEARNED TO QPD-PRIOR-UNEARNED.
           MOVE PD-ERNDELQ-STOP TO QPD-ERNDELQ-STOP.
           MOVE PD-NONACRERN TO QPD-NONACRERN.
           MOVE PD-PRIOR-NONACRERN TO QPD-PRIOR-NONACRERN.

           IF ( PD-ERN-IB-PDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-ERN-IB-PDTH-DATE
           ELSE
              MOVE PD-ERN-IB-PDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-ERN-IB-PDTH-DATE.

           MOVE PD-ERN-IB-PRINBAL TO QPD-ERN-IB-PRINBAL.
           MOVE PD-ERN-IB-INTOWE TO QPD-ERN-IB-INTOWE.
           MOVE PD-TOTERN TO QPD-TOTERN.
           MOVE PD-TOTOTH TO QPD-TOTOTH.
           MOVE PD-TOTINT TO QPD-TOTINT.
           MOVE PD-TOTLCHG TO QPD-TOTLCHG.
           MOVE PD-TOTDEF TO QPD-TOTDEF.
           MOVE PD-TOTSERACCT TO QPD-TOTSERACCT.
           MOVE PD-LCWAIVERS TO QPD-LCWAIVERS.
           MOVE PD-INTWAIVERS TO QPD-INTWAIVERS.
           MOVE PD-TOTPAYMNTD TO QPD-TOTPAYMNTD.
           MOVE PD-TOTEXCPAYMNTD TO QPD-TOTEXCPAYMNTD.
           MOVE PD-TOTNODEF TO QPD-TOTNODEF.
           MOVE PD-YTDNODEF TO QPD-YTDNODEF.
           MOVE PD-TOTNODEFACT TO QPD-TOTNODEFACT.
           MOVE PD-YTDNODEFACT TO QPD-YTDNODEFACT.
           MOVE PD-TOTNOALDEL TO QPD-TOTNOALDEL.
           MOVE PD-YTDNOALDEL TO QPD-YTDNOALDEL.
           MOVE PD-YTDINTPD TO QPD-YTDINTPD.
           MOVE PD-LYRINTPD TO QPD-LYRINTPD.
           MOVE PD-TOTINTPD TO QPD-TOTINTPD.
           MOVE PD-RECASTINT TO QPD-RECASTINT.
           MOVE PD-RECASTPRIN TO QPD-RECASTPRIN.
           MOVE PD-CL-REBATE TO QPD-CL-REBATE.
           MOVE PD-AH-REBATE TO QPD-AH-REBATE.
           MOVE PD-PP-REBATE TO QPD-PP-REBATE.
           MOVE PD-INT-REBATE TO QPD-INT-REBATE.
           MOVE PD-SC-REBATE TO QPD-SC-REBATE.
           MOVE PD-MF-REBATE TO QPD-MF-REBATE.
           MOVE PD-DF-REBATE TO QPD-DF-REBATE.
           MOVE PD-EX-REBATE TO QPD-EX-REBATE.
           MOVE PD-O1-REBATE TO QPD-O1-REBATE.
           MOVE PD-O2-REBATE TO QPD-O2-REBATE.
           MOVE PD-O3-REBATE TO QPD-O3-REBATE.
           MOVE PD-O4-REBATE TO QPD-O4-REBATE.
           MOVE PD-O5-REBATE TO QPD-O5-REBATE.
           MOVE PD-O6-REBATE TO QPD-O6-REBATE.
           MOVE PD-O7-REBATE TO QPD-O7-REBATE.
           MOVE PD-UN-O7-REB TO QPD-UN-O7-REB.
           MOVE PD-UN-O8-REB TO QPD-UN-O8-REB.
           MOVE PD-UN-O9-REB TO QPD-UN-O9-REB.
           MOVE PD-UN-O10-REB TO QPD-UN-O10-REB.
           MOVE PD-PLREASON TO QPD-PLREASON.
           MOVE PD-COLLECTOR TO QPD-COLLECTOR.
           MOVE PD-ACTIONCD TO QPD-ACTIONCD.
           MOVE PD-ACTIONCD2 TO QPD-ACTIONCD2.
           MOVE PD-ATCODE TO QPD-ATCODE.
           MOVE PD-RECODE TO QPD-RECODE.
           MOVE PD-ESCROW-FG TO QPD-ESCROW-FG.
           MOVE PD-PROMDAYS-PASTDUE TO QPD-PROMDAYS-PASTDUE.
           MOVE PD-PROMID TO QPD-PROMID.

           IF ( PD-PROMDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-PROMDATE
           ELSE
              MOVE PD-PROMDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-PROMDATE.

           MOVE PD-PROMAMT TO QPD-PROMAMT.

           IF ( PD-PROMDATE2 = ZEROES )
              MOVE "1900-01-01" TO QPD-PROMDATE2
           ELSE
              MOVE PD-PROMDATE2 TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-PROMDATE2.


           IF ( PD-DATE-PAID-LAST = ZEROES )
              MOVE "1900-01-01" TO QPD-DATE-PAID-LAST
           ELSE
              MOVE PD-DATE-PAID-LAST TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-DATE-PAID-LAST.

           MOVE PD-AMT-PAID-LAST TO QPD-AMT-PAID-LAST.
           MOVE PD-LASTCFORMNO TO QPD-LASTCFORMNO.

           IF ( PD-LASTCFORMDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-LASTCFORMDATE
           ELSE
              MOVE PD-LASTCFORMDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-LASTCFORMDATE.

           MOVE PD-DELCNT TO QPD-DELCNT.
           MOVE PD-DELCNT2 TO QPD-DELCNT2.
           MOVE PD-CREDITLIM TO QPD-CREDITLIM.
           MOVE PD-CRSCORE TO QPD-CRSCORE.
           MOVE PD-LASTSFORMNO TO QPD-LASTSFORMNO.

           IF ( PD-LASTSFORMDATE = ZEROES )
              MOVE "1900-01-01" TO QPD-LASTSFORMDATE
           ELSE
              MOVE PD-LASTSFORMDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-LASTSFORMDATE.

           MOVE PD-SOLCNT TO QPD-SOLCNT.
           MOVE PD-MESSAGE TO QPD-MESSAGE.
           MOVE PD-OLDACCTNO TO QPD-OLDACCTNO.
           MOVE PD-PRLNNO TO QPD-PRLNNO.
           MOVE PD-PRBAL TO QPD-PRBAL.
           MOVE PD-PRINTDUE TO QPD-PRINTDUE.
           MOVE PD-PRFINCHGRB TO QPD-PRFINCHGRB.
           MOVE PD-PRINSRB TO QPD-PRINSRB.
           MOVE PD-PRPOCD TO QPD-PRPOCD.

           IF ( PD-PRPODATE = ZEROES )
              MOVE "1900-01-01" TO QPD-PRPODATE
           ELSE
              MOVE PD-PRPODATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-PRPODATE.

           MOVE PD-PRDELINQAMT TO QPD-PRDELINQAMT.
           MOVE PD-PRLNAMT TO QPD-PRLNAMT.
           MOVE PD-PRPDUE-DAYS TO QPD-PRPDUE-DAYS.
           MOVE PD-PRDLDISC-UNEARN TO QPD-PRDLDISC-UNEARN.
           MOVE PD-PRCONPROFILE TO QPD-PRCONPROFILE.
           MOVE PD-SUM-ORIGPURAMT TO QPD-SUM-ORIGPURAMT.
           MOVE PD-SUM-LNAMT TO QPD-SUM-LNAMT.
           MOVE PD-SUM-FINCHG TO QPD-SUM-FINCHG.
           MOVE PD-SUM-INTCHG TO QPD-SUM-INTCHG.
           MOVE PD-SUM-ORGTERM TO QPD-SUM-ORGTERM.
           MOVE PD-SUM-REGPYAMT TO QPD-SUM-REGPYAMT.
           MOVE PD-SUM-EXTCHG TO QPD-SUM-EXTCHG.
           MOVE PD-SUM-SERCHG TO QPD-SUM-SERCHG.
           MOVE PD-SUM-MAINTFEE TO QPD-SUM-MAINTFEE.
           MOVE PD-UNITPER-CD TO QPD-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO QPD-UNITPER-FREQ.
           MOVE PD-DAYS-IN-UNITPER TO QPD-DAYS-IN-UNITPER.
           MOVE PD-UNITPER-PER-YEAR TO QPD-UNITPER-PER-YEAR.
           MOVE PD-REPOCD TO QPD-REPOCD.
           MOVE PD-BR-TFR-FG TO QPD-BR-TFR-FG.
           MOVE PD-O3INS-DLPROC TO QPD-O3INS-DLPROC.
           MOVE PD-INSPREM-DLADJ TO QPD-INSPREM-DLADJ.
           MOVE PD-PLAMT TO QPD-PLAMT.
           MOVE PD-MTD-PRIN TO QPD-MTD-PRIN.
           MOVE PD-MTD-LC TO QPD-MTD-LC.
           MOVE PD-MTD-INT TO QPD-MTD-INT.
           MOVE PD-MTD-OT TO QPD-MTD-OT.
           MOVE PD-MTD-EX TO QPD-MTD-EX.
           MOVE PD-BNKRPT-CHAPTER TO QPD-BNKRPT-CHAPTER.
           MOVE PD-BMO-CON-STATUS TO QPD-BMO-CON-STATUS.
           MOVE PD-BMO-REC-STATUS TO QPD-BMO-REC-STATUS.
           MOVE PD-APP-NUMBER TO QPD-APP-NUMBER.
           MOVE PD-GIFTNET TO QPD-GIFTNET.
           MOVE PD-GIFTNET-OUR-PO TO QPD-GIFTNET-OUR-PO.
           MOVE PD-PROC-DISBURSED TO QPD-PROC-DISBURSED.
           MOVE PD-NOODDPY TO QPD-NOODDPY.
           MOVE PD-CONPROFILE(1) TO QPD-CONPROFILE-1.
           MOVE PD-CONPROFILE(2) TO QPD-CONPROFILE-2.
           MOVE PD-CONPROFILE(3) TO QPD-CONPROFILE-3.
           MOVE PD-CONPROFILE(4) TO QPD-CONPROFILE-4.
           MOVE PD-CONPROFILE(5) TO QPD-CONPROFILE-5.
           MOVE PD-RECPROFILE(1) TO QPD-RECPROFILE-1.
           MOVE PD-RECPROFILE(2) TO QPD-RECPROFILE-2.
           MOVE PD-RECPROFILE(3) TO QPD-RECPROFILE-3.
           MOVE PD-RECPROFILE(4) TO QPD-RECPROFILE-4.
           MOVE PD-RECPROFILE(5) TO QPD-RECPROFILE-5.
           MOVE PD-TRW-STATUS TO QPD-TRW-STATUS.
           MOVE PD-ORIGIN TO QPD-ORIGIN.
           MOVE PD-PO-TRCD TO QPD-PO-TRCD.
           MOVE PD-NEXIS-CRSCORE TO QPD-NEXIS-CRSCORE.
           MOVE PD-TRW-B-CONSUMER-IND TO QPD-TRW-B-CONSUMER-IND.
           MOVE PD-PARVHELD-PAID-FG TO QPD-PARVHELD-PAID-FG.
           MOVE PD-ACCT-FROZEN TO QPD-ACCT-FROZEN.
           MOVE PD-TRW-SPECIAL-CODE TO QPD-TRW-SPECIAL-CODE.
           MOVE PD-CREATION-STATUS TO QPD-CREATION-STATUS.
           MOVE PD-TOTNOLCHG TO QPD-TOTNOLCHG.
           MOVE PD-LBOX TO QPD-LBOX.
           MOVE PD-EXTRA-PRIN-LFP TO QPD-EXTRA-PRIN-LFP.
           MOVE PD-REMAIN-INT-SINCE-LFP TO QPD-REMAIN-INT-SINCE-LFP.
           MOVE PD-REMAIN-PRIN-SINCE-LFP TO QPD-REMAIN-PRIN-SINCE-LFP.
           MOVE PD-INTERIM-COUPON TO QPD-INTERIM-COUPON.
           MOVE PD-DLCOMM-O3-CMSI TO QPD-DLCOMM-O3-CMSI.
           MOVE PD-DLCOMM-PP-CMSI TO QPD-DLCOMM-PP-CMSI.
           MOVE PD-TRW-J-CONSUMER-IND TO QPD-TRW-J-CONSUMER-IND.
           MOVE PD-ACH TO QPD-ACH.
           MOVE PD-1099-C-FG TO QPD-1099-C-FG.
           MOVE PD-TRW-COMPLIANCE-CODE TO QPD-TRW-COMPLIANCE-CODE.
           MOVE PD-UNWIND-REASON-CODE TO QPD-UNWIND-REASON-CODE.
           MOVE PD-PBI-PBD-FG TO QPD-PBI-PBD-FG.
           MOVE PD-OLD-ACCT-FORMAT-FG TO QPD-OLD-ACCT-FORMAT-FG.
           MOVE PD-APPROVED-BY-USER TO QPD-APPROVED-BY-USER.
           MOVE PD-APPROVED-BY-CODE TO QPD-APPROVED-BY-CODE.

           IF ( PD-APPROVED-BY-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-APPROVED-BY-DATE
           ELSE
              MOVE PD-APPROVED-BY-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-APPROVED-BY-DATE.

           MOVE PD-APPROVED-BY-TIME TO QPD-APPROVED-BY-TIME.

           IF ( PD-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QPD-LTOUCH-DATE
           ELSE
              MOVE PD-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPD-LTOUCH-DATE.

           MOVE PD-LTOUCH-TIME TO QPD-LTOUCH-TIME.
           MOVE PD-HOLD-COLLATERAL TO QPD-HOLD-COLLATERAL.
           MOVE PD-DLPAID TO QPD-DLPAID.
           MOVE PD-CKNO TO QPD-CKNO.
           MOVE PD-FREE1 TO QPD-FREE1.
           MOVE PD-FREE2 TO QPD-FREE2.
           MOVE PD-FREE-SPACE-FILLER TO QPD-FREE-SPACE-FILLER.
           MOVE PD-POSTING-BR TO QPD-POSTING-BR.
           MOVE PD-OTHBR-FG TO QPD-OTHBR-FG.
           MOVE PD-APPROVAL-LIMIT TO QPD-APPROVAL-LIMIT.
           MOVE PD-APPROVAL-USERID TO QPD-APPROVAL-USERID.
           MOVE PD-MIN-CRED-SCORE TO QPD-MIN-CRED-SCORE.
           MOVE PD-CRED-SCORE-USERID TO QPD-CRED-SCORE-USERID.
           MOVE PD-FINAMT TO QPD-FINAMT.
           MOVE PD-PROCEEDS-FG TO QPD-PROCEEDS-FG.
           MOVE PD-POINTS-FG TO QPD-POINTS-FG.
           MOVE PD-OVRDRATE-FG TO QPD-OVRDRATE-FG.
           MOVE PD-INS-FG(1) TO QPD-INS-FG-1.
           MOVE PD-INS-FG(2) TO QPD-INS-FG-2.
           MOVE PD-INS-FG(3) TO QPD-INS-FG-3.
           MOVE PD-INS-FG(4) TO QPD-INS-FG-4.
           MOVE PD-INS-FG(5) TO QPD-INS-FG-5.
           MOVE PD-INS-FG(6) TO QPD-INS-FG-6.
           MOVE PD-INS-FG(7) TO QPD-INS-FG-7.
           MOVE PD-INS-FG(8) TO QPD-INS-FG-8.
           MOVE PD-INS-FG(9) TO QPD-INS-FG-9.
           MOVE PD-INS-FG(10) TO QPD-INS-FG-10.
           MOVE PD-SERCHG-FG TO QPD-SERCHG-FG.
           MOVE PD-MAINTFEE-FG TO QPD-MAINTFEE-FG.
           MOVE PD-EXTCHG-FG TO QPD-EXTCHG-FG.
           MOVE PD-REGPYAMT-FG TO QPD-REGPYAMT-FG.
           MOVE PD-FEECD(1) TO QPD-FEECD-1.
           MOVE PD-FEECD(2) TO QPD-FEECD-2.
           MOVE PD-FEECD(3) TO QPD-FEECD-3.
           MOVE PD-FEECD(4) TO QPD-FEECD-4.
           MOVE PD-FEECD(5) TO QPD-FEECD-5.
           MOVE PD-FEECD(6) TO QPD-FEECD-6.
           MOVE PD-FEECD(7) TO QPD-FEECD-7.
           MOVE PD-FEECD(8) TO QPD-FEECD-8.
           MOVE PD-FEECD(9) TO QPD-FEECD-9.
           MOVE PD-FEECD(10) TO QPD-FEECD-10.
           MOVE PD-FEECD(11) TO QPD-FEECD-11.
           MOVE PD-FEECD(12) TO QPD-FEECD-12.
           MOVE PD-FEECD(13) TO QPD-FEECD-13.
           MOVE PD-FEECD(14) TO QPD-FEECD-14.
           MOVE PD-FEECD(15) TO QPD-FEECD-15.
           MOVE PD-FEECD(16) TO QPD-FEECD-16.
           MOVE PD-FEECD(17) TO QPD-FEECD-17.
           MOVE PD-FEECD(18) TO QPD-FEECD-18.
           MOVE PD-FEECD(19) TO QPD-FEECD-19.
           MOVE PD-FEECD(20) TO QPD-FEECD-20.
           MOVE PD-PRPAID-AHEAD TO QPD-PRPAID-AHEAD.
           MOVE PD-RESTRICT-ADJ TO QPD-RESTRICT-ADJ.
           MOVE PD-DUAL-LOAN-ADJ TO QPD-DUAL-LOAN-ADJ.
           MOVE PD-CAN-LOAN-MAX TO QPD-CAN-LOAN-MAX.
           MOVE PD-PERDOWN TO QPD-PERDOWN.
           MOVE PD-SELLPR-FG TO QPD-SELLPR-FG.
           MOVE PD-DOWNPY-FG TO QPD-DOWNPY-FG.
           MOVE PD-PERDOWN-FG TO QPD-PERDOWN-FG.
           MOVE PD-SELL-DOWN-FG TO QPD-SELL-DOWN-FG.
           MOVE PD-BALLOON-FG TO QPD-BALLOON-FG.
           MOVE PD-BALLOON-LAST-FG TO QPD-BALLOON-LAST-FG.
           MOVE PD-FEECD-FG(1) TO QPD-FEECD-FG-1.
           MOVE PD-FEECD-FG(2) TO QPD-FEECD-FG-2.
           MOVE PD-INS-TERM-SW(1) TO QPD-INS-TERM-SW-1.
           MOVE PD-COVERAGE-SW(1) TO QPD-COVERAGE-SW-1.
           MOVE PD-PREMIUM-SW(1) TO QPD-PREMIUM-SW-1.
           MOVE PD-INS-TERM-SW(2) TO QPD-INS-TERM-SW-2.
           MOVE PD-COVERAGE-SW(2) TO QPD-COVERAGE-SW-2.
           MOVE PD-PREMIUM-SW(2) TO QPD-PREMIUM-SW-2.
           MOVE PD-INS-TERM-SW(3) TO QPD-INS-TERM-SW-3.
           MOVE PD-COVERAGE-SW(3) TO QPD-COVERAGE-SW-3.
           MOVE PD-PREMIUM-SW(3) TO QPD-PREMIUM-SW-3.
           MOVE PD-INS-TERM-SW(4) TO QPD-INS-TERM-SW-4.
           MOVE PD-COVERAGE-SW(4) TO QPD-COVERAGE-SW-4.
           MOVE PD-PREMIUM-SW(4) TO QPD-PREMIUM-SW-4.
           MOVE PD-INS-TERM-SW(5) TO QPD-INS-TERM-SW-5.
           MOVE PD-COVERAGE-SW(5) TO QPD-COVERAGE-SW-5.
           MOVE PD-PREMIUM-SW(5) TO QPD-PREMIUM-SW-5.
           MOVE PD-INS-TERM-SW(6) TO QPD-INS-TERM-SW-6.
           MOVE PD-COVERAGE-SW(6) TO QPD-COVERAGE-SW-6.
           MOVE PD-PREMIUM-SW(6) TO QPD-PREMIUM-SW-6.
           MOVE PD-INS-TERM-SW(7) TO QPD-INS-TERM-SW-7.
           MOVE PD-COVERAGE-SW(7) TO QPD-COVERAGE-SW-7.
           MOVE PD-PREMIUM-SW(7) TO QPD-PREMIUM-SW-7.
           MOVE PD-INS-TERM-SW(8) TO QPD-INS-TERM-SW-8.
           MOVE PD-COVERAGE-SW(8) TO QPD-COVERAGE-SW-8.
           MOVE PD-PREMIUM-SW(8) TO QPD-PREMIUM-SW-8.
           MOVE PD-INS-TERM-SW(9) TO QPD-INS-TERM-SW-9.
           MOVE PD-COVERAGE-SW(9) TO QPD-COVERAGE-SW-9.
           MOVE PD-PREMIUM-SW(9) TO QPD-PREMIUM-SW-9.
           MOVE PD-INS-TERM-SW(10) TO QPD-INS-TERM-SW-10.
           MOVE PD-COVERAGE-SW(10) TO QPD-COVERAGE-SW-10.
           MOVE PD-PREMIUM-SW(10) TO QPD-PREMIUM-SW-10.
           MOVE PD-DLDISC-FG TO QPD-DLDISC-FG.
           MOVE PD-DLRVPRIN-FG(1) TO QPD-DLRVPRIN-FG-1.
           MOVE PD-DLRVPRIN-FG(2) TO QPD-DLRVPRIN-FG-2.
           MOVE PD-DLRVPRIN-FG(3) TO QPD-DLRVPRIN-FG-3.
           MOVE PD-DLRVCHGS-FG(1) TO QPD-DLRVCHGS-FG-1.
           MOVE PD-DLRVCHGS-FG(2) TO QPD-DLRVCHGS-FG-2.
           MOVE PD-DLRVCHGS-FG(3) TO QPD-DLRVCHGS-FG-3.
           MOVE PD-DLPARVHELD-FG TO QPD-DLPARVHELD-FG.
           MOVE PD-DLPARVPAID-FG TO QPD-DLPARVPAID-FG.
           MOVE PD-ONE-MTH-NO-DAY-EXT-SW TO QPD-ONE-MTH-NO-DAY-EXT-SW.
           MOVE PD-ASSIGN-RN-ACCTNO TO QPD-ASSIGN-RN-ACCTNO.
           MOVE PD-APR-FINAMT TO QPD-APR-FINAMT.
           MOVE PD-DLCOMM-PP-CMSI-FG TO QPD-DLCOMM-PP-CMSI-FG.
           MOVE PD-DLCOMM-O3-CMSI-FG TO QPD-DLCOMM-O3-CMSI-FG.
           MOVE PD-FILLER-TO-MATCH-LN TO QPD-FILLER-TO-MATCH-LN.
           MOVE PD2-ID TO QPD2-ID.
           MOVE PD2-BRANCH TO QPD2-BRANCH.
           MOVE PD2-BWNAME TO QPD2-BWNAME.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-PD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( PD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( PD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO PD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO PD-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO PD-ALL-DATA
              MOVE PD-ALL-PATH             TO PD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( PD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO PD-ALL-BASE
              MOVE FILPATH-MACHINE         TO PD-ALL-MACHINE
              MOVE FILPATH-DATA            TO PD-ALL-DATA
              MOVE PD-ALL-PATH             TO PD-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-PDFILE NOT = "S" )
              MOVE PD-PATH-BEG             TO PD-SELECT-PATH-BEG
              MOVE PD-PATH-FILE            TO PD-SELECT-PATH-FILE
           ELSE
              MOVE PD-PATH-SQL             TO PD-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-PD1-FILE.
           PERFORM LOAD-PD1-FILE.
           PERFORM OPEN-IT.
           MOVE PD-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-PD1-FILE.
           PERFORM START-IT.
           MOVE PD-PATH-OWNBR  TO PD-BRANCH.
           MOVE PD-SELECT-PATH TO E-FILE.
           MOVE PD1-KEY        TO E-KEYX.

           IF ( PD1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-PD1-FILE.

           IF ( QPD1-WEND-ID = SPACES )
              MOVE PD-PATH-OWNBR        TO QPD1-WBEG-BRANCH
              MOVE PD-PATH-OWNBR        TO QPD1-WEND-BRANCH
              MOVE PD-ID                TO QPD1-WBEG-ID
              MOVE ALL "Z"              TO QPD1-WEND-ID
              MOVE PD-SOC-SEC           TO QPD1-WBEG-SOC-SEC
              MOVE ALL "9"              TO QPD1-WEND-SOC-SEC.

           EXEC SQL
            DECLARE PD1_CURSOR CURSOR FOR
            SELECT ISNULL(PDFILE.PD_ID,''),
                   PDFILE.PD_BRANCH,
                   PDFILE.PD_SOC_SEC,
                   PDFILE.PD_OWNBR,
                   PDFILE.PD_ACCTNO,
                   PDFILE.PD_CLASS,
                   PDFILE.PD_SUBCLASS,
                   PDFILE.PD_LAWCODE,
                   PDFILE.PD_SSNO1,
                   ISNULL(PDFILE.PD_MAKERCD1,''),
                   PDFILE.PD_SSNO2,
                   ISNULL(PDFILE.PD_MAKERCD2,''),
                   PDFILE.PD_SSNO3,
                   ISNULL(PDFILE.PD_MAKERCD3,''),
                   PDFILE.PD_SSNO4,
                   ISNULL(PDFILE.PD_MAKERCD4,''),
                   ISNULL(PDFILE.PD_ORGST,''),
                   PDFILE.PD_ORGBR,
                   ISNULL(PDFILE.PD_OWNST,''),
                   PDFILE.PD_SPRCLASS,
                   ISNULL(PDFILE.PD_CONVERCD,''),
                   ISNULL(PDFILE.PD_MIDSTREAM,''),
                   ISNULL(PDFILE.PD_PURCD,''),
                   ISNULL(PDFILE.PD_SRCD,''),
                   ISNULL(PDFILE.PD_COLLCD,''),
                   ISNULL(PDFILE.PD_COLLID,''),
                   ISNULL(PDFILE.PD_POCD,''),
                   ISNULL(PDFILE.PD_PLCD,''),
                   ISNULL(PDFILE.PD_PLTYPE,''),
                   ISNULL(PDFILE.PD_CONFIDENTCD,''),
                   ISNULL(PDFILE.PD_ALLOTCD,''),
                   ISNULL(PDFILE.PD_RECASTCD,''),
                   ISNULL(PDFILE.PD_SKIP_TRW,''),
                   ISNULL(PDFILE.PD_STATUSFG,''),
                   ISNULL(PDFILE.PD_SMALLOAN,''),
                   ISNULL(PDFILE.PD_SALEFIN,''),
                   ISNULL(PDFILE.PD_SERVACCT,''),
                   ISNULL(PDFILE.PD_LOANTYPE,''),
                   ISNULL(PDFILE.PD_ADDON_FG,''),
                   ISNULL(PDFILE.PD_PAY_SCHLD_FG,''),
                   PDFILE.PD_LC_CODE,
                   ISNULL(PDFILE.PD_CAN_LOAN_NO,''),
                   ISNULL(PDFILE.PD_STATFLAGS,''),
                   ISNULL(CAST(PDFILE.PD_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PURDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_CASHDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_FILEDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_JDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PLDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_REDUDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_REPODATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_SOLDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_APRATE,
                   PDFILE.PD_EFFRATE,
                   PDFILE.PD_SMPRATE,
                   PDFILE.PD_YLDRATE,
                   PDFILE.PD_OVRDRATE,
                   PDFILE.PD_ENDORATE,
                   PDFILE.PD_JDRATE,
                   PDFILE.PD_ACCRATE,
                   PDFILE.PD_REDURATE,
                   PDFILE.PD_CONRATE,
                   PDFILE.PD_SERVPER,
                   PDFILE.PD_SELLPR,
                   PDFILE.PD_DOWNPY,
                   PDFILE.PD_TRADEIN,
                   PDFILE.PD_LNAMT,
                   PDFILE.PD_FINCHG,
                   PDFILE.PD_INTCHG,
                   PDFILE.PD_SERCHG,
                   PDFILE.PD_EXTCHG,
                   PDFILE.PD_MAINTFEE,
                   PDFILE.PD_POINTS,
                   PDFILE.PD_PROCEEDS,
                   PDFILE.PD_ORGTERM,
                   PDFILE.PD_1STPYAMT,
                   PDFILE.PD_NOREGPY,
                   PDFILE.PD_REGPYAMT,
                   PDFILE.PD_LASTPYAMT,
                   PDFILE.PD_PROCASH,
                   PDFILE.PD_TILLNO,
                   ISNULL(PDFILE.PD_STATE_FLAG,''),
                   PDFILE.PD_STATE_WORKER,
                   PDFILE.PD_STATE_WORKER2,
                   ISNULL(PDFILE.PD_FEECODE_1,''),
                   PDFILE.PD_FEEAMT_1,
                   ISNULL(PDFILE.PD_FEEOURS_1,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_1,''),
                   ISNULL(PDFILE.PD_FEECODE_2,''),
                   PDFILE.PD_FEEAMT_2,
                   ISNULL(PDFILE.PD_FEEOURS_2,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_2,''),
                   ISNULL(PDFILE.PD_FEECODE_3,''),
                   PDFILE.PD_FEEAMT_3,
                   ISNULL(PDFILE.PD_FEEOURS_3,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_3,''),
                   ISNULL(PDFILE.PD_FEECODE_4,''),
                   PDFILE.PD_FEEAMT_4,
                   ISNULL(PDFILE.PD_FEEOURS_4,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_4,''),
                   ISNULL(PDFILE.PD_FEECODE_5,''),
                   PDFILE.PD_FEEAMT_5,
                   ISNULL(PDFILE.PD_FEEOURS_5,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_5,''),
                   ISNULL(PDFILE.PD_FEECODE_6,''),
                   PDFILE.PD_FEEAMT_6,
                   ISNULL(PDFILE.PD_FEEOURS_6,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_6,''),
                   ISNULL(PDFILE.PD_FEECODE_7,''),
                   PDFILE.PD_FEEAMT_7,
                   ISNULL(PDFILE.PD_FEEOURS_7,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_7,''),
                   ISNULL(PDFILE.PD_FEECODE_8,''),
                   PDFILE.PD_FEEAMT_8,
                   ISNULL(PDFILE.PD_FEEOURS_8,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_8,''),
                   ISNULL(PDFILE.PD_FEECODE_9,''),
                   PDFILE.PD_FEEAMT_9,
                   ISNULL(PDFILE.PD_FEEOURS_9,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_9,''),
                   ISNULL(PDFILE.PD_FEECODE_10,''),
                   PDFILE.PD_FEEAMT_10,
                   ISNULL(PDFILE.PD_FEEOURS_10,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_10,''),
                   ISNULL(PDFILE.PD_FEECODE_11,''),
                   PDFILE.PD_FEEAMT_11,
                   ISNULL(PDFILE.PD_FEEOURS_11,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_11,''),
                   ISNULL(PDFILE.PD_FEECODE_12,''),
                   PDFILE.PD_FEEAMT_12,
                   ISNULL(PDFILE.PD_FEEOURS_12,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_12,''),
                   ISNULL(PDFILE.PD_FEECODE_13,''),
                   PDFILE.PD_FEEAMT_13,
                   ISNULL(PDFILE.PD_FEEOURS_13,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_13,''),
                   ISNULL(PDFILE.PD_FEECODE_14,''),
                   PDFILE.PD_FEEAMT_14,
                   ISNULL(PDFILE.PD_FEEOURS_14,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_14,''),
                   ISNULL(PDFILE.PD_FEECODE_15,''),
                   PDFILE.PD_FEEAMT_15,
                   ISNULL(PDFILE.PD_FEEOURS_15,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_15,''),
                   ISNULL(PDFILE.PD_FEECODE_16,''),
                   PDFILE.PD_FEEAMT_16,
                   ISNULL(PDFILE.PD_FEEOURS_16,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_16,''),
                   ISNULL(PDFILE.PD_FEECODE_17,''),
                   PDFILE.PD_FEEAMT_17,
                   ISNULL(PDFILE.PD_FEEOURS_17,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_17,''),
                   ISNULL(PDFILE.PD_FEECODE_18,''),
                   PDFILE.PD_FEEAMT_18,
                   ISNULL(PDFILE.PD_FEEOURS_18,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_18,''),
                   ISNULL(PDFILE.PD_FEECODE_19,''),
                   PDFILE.PD_FEEAMT_19,
                   ISNULL(PDFILE.PD_FEEOURS_19,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_19,''),
                   ISNULL(PDFILE.PD_FEECODE_20,''),
                   PDFILE.PD_FEEAMT_20,
                   ISNULL(PDFILE.PD_FEEOURS_20,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_20,''),
                   ISNULL(PDFILE.PD_INSURED,''),
                   ISNULL(PDFILE.PD_INSTYPES,''),
                   ISNULL(PDFILE.PD_INSELIGIBLE,''),
                   PDFILE.PD_INSCOMP_1,
                   ISNULL(PDFILE.PD_INSOURS_1,''),
                   PDFILE.PD_INSEFF_DATE_1,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_1,
                   PDFILE.PD_INSCOVR_1,
                   PDFILE.PD_INSCOMM_1,
                   PDFILE.PD_INSANTICERN_1,
                   PDFILE.PD_INSANTICADJ_1,
                   PDFILE.PD_INSUFERN_1,
                   PDFILE.PD_INSACCERN_1,
                   PDFILE.PD_INSEARN_ORDER_1,
                   ISNULL(PDFILE.PD_INSFINCHG_1,''),
                   PDFILE.PD_INSCOMP_2,
                   ISNULL(PDFILE.PD_INSOURS_2,''),
                   PDFILE.PD_INSEFF_DATE_2,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_2,
                   PDFILE.PD_INSCOVR_2,
                   PDFILE.PD_INSCOMM_2,
                   PDFILE.PD_INSANTICERN_2,
                   PDFILE.PD_INSANTICADJ_2,
                   PDFILE.PD_INSUFERN_2,
                   PDFILE.PD_INSACCERN_2,
                   PDFILE.PD_INSEARN_ORDER_2,
                   ISNULL(PDFILE.PD_INSFINCHG_2,''),
                   PDFILE.PD_INSCOMP_3,
                   ISNULL(PDFILE.PD_INSOURS_3,''),
                   PDFILE.PD_INSEFF_DATE_3,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_3,
                   PDFILE.PD_INSCOVR_3,
                   PDFILE.PD_INSCOMM_3,
                   PDFILE.PD_INSANTICERN_3,
                   PDFILE.PD_INSANTICADJ_3,
                   PDFILE.PD_INSUFERN_3,
                   PDFILE.PD_INSACCERN_3,
                   PDFILE.PD_INSEARN_ORDER_3,
                   ISNULL(PDFILE.PD_INSFINCHG_3,''),
                   PDFILE.PD_INSCOMP_4,
                   ISNULL(PDFILE.PD_INSOURS_4,''),
                   PDFILE.PD_INSEFF_DATE_4,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_4,
                   PDFILE.PD_INSCOVR_4,
                   PDFILE.PD_INSCOMM_4,
                   PDFILE.PD_INSANTICERN_4,
                   PDFILE.PD_INSANTICADJ_4,
                   PDFILE.PD_INSUFERN_4,
                   PDFILE.PD_INSACCERN_4,
                   PDFILE.PD_INSEARN_ORDER_4,
                   ISNULL(PDFILE.PD_INSFINCHG_4,''),
                   PDFILE.PD_INSCOMP_5,
                   ISNULL(PDFILE.PD_INSOURS_5,''),
                   PDFILE.PD_INSEFF_DATE_5,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_5,
                   PDFILE.PD_INSCOVR_5,
                   PDFILE.PD_INSCOMM_5,
                   PDFILE.PD_INSANTICERN_5,
                   PDFILE.PD_INSANTICADJ_5,
                   PDFILE.PD_INSUFERN_5,
                   PDFILE.PD_INSACCERN_5,
                   PDFILE.PD_INSEARN_ORDER_5,
                   ISNULL(PDFILE.PD_INSFINCHG_5,''),
                   PDFILE.PD_INSCOMP_6,
                   ISNULL(PDFILE.PD_INSOURS_6,''),
                   PDFILE.PD_INSEFF_DATE_6,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_6,
                   PDFILE.PD_INSCOVR_6,
                   PDFILE.PD_INSCOMM_6,
                   PDFILE.PD_INSANTICERN_6,
                   PDFILE.PD_INSANTICADJ_6,
                   PDFILE.PD_INSUFERN_6,
                   PDFILE.PD_INSACCERN_6,
                   PDFILE.PD_INSEARN_ORDER_6,
                   ISNULL(PDFILE.PD_INSFINCHG_6,''),
                   PDFILE.PD_INSCOMP_7,
                   ISNULL(PDFILE.PD_INSOURS_7,''),
                   PDFILE.PD_INSEFF_DATE_7,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_7,
                   PDFILE.PD_INSCOVR_7,
                   PDFILE.PD_INSCOMM_7,
                   PDFILE.PD_INSANTICERN_7,
                   PDFILE.PD_INSANTICADJ_7,
                   PDFILE.PD_INSUFERN_7,
                   PDFILE.PD_INSACCERN_7,
                   PDFILE.PD_INSEARN_ORDER_7,
                   ISNULL(PDFILE.PD_INSFINCHG_7,''),
                   PDFILE.PD_INSCOMP_8,
                   ISNULL(PDFILE.PD_INSOURS_8,''),
                   PDFILE.PD_INSEFF_DATE_8,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_8,
                   PDFILE.PD_INSCOVR_8,
                   PDFILE.PD_INSCOMM_8,
                   PDFILE.PD_INSANTICERN_8,
                   PDFILE.PD_INSANTICADJ_8,
                   PDFILE.PD_INSUFERN_8,
                   PDFILE.PD_INSACCERN_8,
                   PDFILE.PD_INSEARN_ORDER_8,
                   ISNULL(PDFILE.PD_INSFINCHG_8,''),
                   PDFILE.PD_INSCOMP_9,
                   ISNULL(PDFILE.PD_INSOURS_9,''),
                   PDFILE.PD_INSEFF_DATE_9,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_9,
                   PDFILE.PD_INSCOVR_9,
                   PDFILE.PD_INSCOMM_9,
                   PDFILE.PD_INSANTICERN_9,
                   PDFILE.PD_INSANTICADJ_9,
                   PDFILE.PD_INSUFERN_9,
                   PDFILE.PD_INSACCERN_9,
                   PDFILE.PD_INSEARN_ORDER_9,
                   ISNULL(PDFILE.PD_INSFINCHG_9,''),
                   PDFILE.PD_INSCOMP_10,
                   ISNULL(PDFILE.PD_INSOURS_10,''),
                   PDFILE.PD_INSEFF_DATE_10,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_10,
                   PDFILE.PD_INSCOVR_10,
                   PDFILE.PD_INSCOMM_10,
                   PDFILE.PD_INSANTICERN_10,
                   PDFILE.PD_INSANTICADJ_10,
                   PDFILE.PD_INSUFERN_10,
                   PDFILE.PD_INSACCERN_10,
                   PDFILE.PD_INSEARN_ORDER_10,
                   ISNULL(PDFILE.PD_INSFINCHG_10,''),
                   PDFILE.PD_PY_YYYYMM_RECDEL,
                   PDFILE.PD_MTD_PAYS_RECDEL,
                   ISNULL(PDFILE.PD_GARNISH_EXTR,''),
                   ISNULL(PDFILE.PD_SHIP_SAME_FG,''),
                   PDFILE.PD_AD_D_INS_COVR,
                   PDFILE.PD_TOT_PARTIAL_CHGOFF,
                   ISNULL(PDFILE.PD_LPO,''),
                   ISNULL(PDFILE.PD_REP_CD,''),
                   PDFILE.PD_EMP_ID,
                   ISNULL(PDFILE.PD_PROCESS_CD,''),
                   PDFILE.PD_WCBC_CREDITLIM,
                   ISNULL(PDFILE.PD_FOLLOWUP_CD,''),
                   ISNULL(PDFILE.PD_MIP_FG,''),
                   PDFILE.PD_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(PDFILE.PD_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_MIP_TOT_CLPAID,
                   ISNULL(CAST(PDFILE.PD_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_MIP_TOT_AHPAID,
                   PDFILE.PD_IL_GROSS_INCOME,
                   PDFILE.PD_IL_GROSS_INCOME2,
                   ISNULL(PDFILE.PD_RESCISSION,''),
                   ISNULL(PDFILE.PD_INTERIM_TRW,''),
                   PDFILE.PD_DLNO,
                   PDFILE.PD_DLPOOL,
                   ISNULL(PDFILE.PD_DLRECOURSE,''),
                   PDFILE.PD_DLPARTRCAMT,
                   PDFILE.PD_ORIGPURAMT,
                   PDFILE.PD_PURBAL,
                   PDFILE.PD_DLRVPRIN_1,
                   PDFILE.PD_DLRVPRIN_2,
                   PDFILE.PD_DLRVPRIN_3,
                   PDFILE.PD_DLRVCHGS_1,
                   PDFILE.PD_DLRVCHGS_2,
                   PDFILE.PD_DLRVCHGS_3,
                   PDFILE.PD_DLPARVHELD,
                   PDFILE.PD_DLPARVPAID,
                   PDFILE.PD_DLDISC,
                   ISNULL(CAST(PDFILE.PD_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_CURBAL,
                   PDFILE.PD_INTBAL,
                   PDFILE.PD_LCBAL,
                   PDFILE.PD_OTHBAL,
                   PDFILE.PD_OT2BAL,
                   PDFILE.PD_LCPAID,
                   PDFILE.PD_LCPARTIALS,
                   PDFILE.PD_STMT_COUNTER,
                   PDFILE.PD_PRIOR_STMT_COUNTER,
                   ISNULL(PDFILE.PD_FROZEN_REASON,''),
                   ISNULL(PDFILE.PD_COUPON_STATEMENT,''),
                   PDFILE.PD_MONTHEND_BAL,
                   PDFILE.PD_ANTICTERM,
                   PDFILE.PD_ANTICERN_1,
                   PDFILE.PD_ANTICADJ_1,
                   PDFILE.PD_UFERN_1,
                   PDFILE.PD_ACCERN_1,
                   PDFILE.PD_EARN_ORDER_1,
                   PDFILE.PD_ANTICERN_2,
                   PDFILE.PD_ANTICADJ_2,
                   PDFILE.PD_UFERN_2,
                   PDFILE.PD_ACCERN_2,
                   PDFILE.PD_EARN_ORDER_2,
                   PDFILE.PD_ANTICERN_3,
                   PDFILE.PD_ANTICADJ_3,
                   PDFILE.PD_UFERN_3,
                   PDFILE.PD_ACCERN_3,
                   PDFILE.PD_EARN_ORDER_3,
                   PDFILE.PD_ANTICERN_4,
                   PDFILE.PD_ANTICADJ_4,
                   PDFILE.PD_UFERN_4,
                   PDFILE.PD_ACCERN_4,
                   PDFILE.PD_EARN_ORDER_4,
                   PDFILE.PD_ANTICERN_5,
                   PDFILE.PD_ANTICADJ_5,
                   PDFILE.PD_UFERN_5,
                   PDFILE.PD_ACCERN_5,
                   PDFILE.PD_EARN_ORDER_5,
                   PDFILE.PD_ANTICERN_6,
                   PDFILE.PD_ANTICADJ_6,
                   PDFILE.PD_UFERN_6,
                   PDFILE.PD_ACCERN_6,
                   PDFILE.PD_EARN_ORDER_6,
                   PDFILE.PD_ANTICERN_7,
                   PDFILE.PD_ANTICADJ_7,
                   PDFILE.PD_UFERN_7,
                   PDFILE.PD_ACCERN_7,
                   PDFILE.PD_EARN_ORDER_7,
                   PDFILE.PD_ANTICERN_8,
                   PDFILE.PD_ANTICADJ_8,
                   PDFILE.PD_UFERN_8,
                   PDFILE.PD_ACCERN_8,
                   PDFILE.PD_EARN_ORDER_8,
                   PDFILE.PD_ANTICERN_9,
                   PDFILE.PD_ANTICADJ_9,
                   PDFILE.PD_UFERN_9,
                   PDFILE.PD_ACCERN_9,
                   PDFILE.PD_EARN_ORDER_9,
                   PDFILE.PD_ANTICERN_10,
                   PDFILE.PD_ANTICADJ_10,
                   PDFILE.PD_UFERN_10,
                   PDFILE.PD_ACCERN_10,
                   PDFILE.PD_EARN_ORDER_10,
                   PDFILE.PD_LMO_PRIOR_INTEREST,
                   PDFILE.PD_PRIOR_UNEARNED,
                   ISNULL(PDFILE.PD_ERNDELQ_STOP,''),
                   PDFILE.PD_NONACRERN,
                   PDFILE.PD_PRIOR_NONACRERN,
                   ISNULL(CAST(PDFILE.PD_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_ERN_IB_PRINBAL,
                   PDFILE.PD_ERN_IB_INTOWE,
                   PDFILE.PD_TOTERN,
                   PDFILE.PD_TOTOTH,
                   PDFILE.PD_TOTINT,
                   PDFILE.PD_TOTLCHG,
                   PDFILE.PD_TOTDEF,
                   PDFILE.PD_TOTSERACCT,
                   PDFILE.PD_LCWAIVERS,
                   PDFILE.PD_INTWAIVERS,
                   PDFILE.PD_TOTPAYMNTD,
                   PDFILE.PD_TOTEXCPAYMNTD,
                   PDFILE.PD_TOTNODEF,
                   PDFILE.PD_YTDNODEF,
                   PDFILE.PD_TOTNODEFACT,
                   PDFILE.PD_YTDNODEFACT,
                   PDFILE.PD_TOTNOALDEL,
                   PDFILE.PD_YTDNOALDEL,
                   PDFILE.PD_YTDINTPD,
                   PDFILE.PD_LYRINTPD,
                   PDFILE.PD_TOTINTPD,
                   PDFILE.PD_RECASTINT,
                   PDFILE.PD_RECASTPRIN,
                   ISNULL(PDFILE.PD_CL_REBATE,''),
                   ISNULL(PDFILE.PD_AH_REBATE,''),
                   ISNULL(PDFILE.PD_PP_REBATE,''),
                   ISNULL(PDFILE.PD_INT_REBATE,''),
                   ISNULL(PDFILE.PD_SC_REBATE,''),
                   ISNULL(PDFILE.PD_MF_REBATE,''),
                   ISNULL(PDFILE.PD_DF_REBATE,''),
                   ISNULL(PDFILE.PD_EX_REBATE,''),
                   ISNULL(PDFILE.PD_O1_REBATE,''),
                   ISNULL(PDFILE.PD_O2_REBATE,''),
                   ISNULL(PDFILE.PD_O3_REBATE,''),
                   ISNULL(PDFILE.PD_O4_REBATE,''),
                   ISNULL(PDFILE.PD_O5_REBATE,''),
                   ISNULL(PDFILE.PD_O6_REBATE,''),
                   ISNULL(PDFILE.PD_O7_REBATE,''),
                   ISNULL(PDFILE.PD_UN_O7_REB,''),
                   ISNULL(PDFILE.PD_UN_O8_REB,''),
                   ISNULL(PDFILE.PD_UN_O9_REB,''),
                   ISNULL(PDFILE.PD_UN_O10_REB,''),
                   ISNULL(PDFILE.PD_PLREASON,''),
                   ISNULL(PDFILE.PD_COLLECTOR,''),
                   ISNULL(PDFILE.PD_ACTIONCD,''),
                   ISNULL(PDFILE.PD_ACTIONCD2,''),
                   ISNULL(PDFILE.PD_ATCODE,''),
                   ISNULL(PDFILE.PD_RECODE,''),
                   ISNULL(PDFILE.PD_ESCROW_FG,''),
                   PDFILE.PD_PROMDAYS_PASTDUE,
                   ISNULL(PDFILE.PD_PROMID,''),
                   ISNULL(CAST(PDFILE.PD_PROMDATE AS VARCHAR(10)),'0'),
                   PDFILE.PD_PROMAMT,
                   ISNULL(CAST(PDFILE.PD_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_AMT_PAID_LAST,
                   PDFILE.PD_LASTCFORMNO,
                   ISNULL(CAST(PDFILE.PD_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_DELCNT,
                   PDFILE.PD_DELCNT2,
                   PDFILE.PD_CREDITLIM,
                   PDFILE.PD_CRSCORE,
                   PDFILE.PD_LASTSFORMNO,
                   ISNULL(CAST(PDFILE.PD_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_SOLCNT,
                   ISNULL(PDFILE.PD_MESSAGE,''),
                   PDFILE.PD_OLDACCTNO,
                   PDFILE.PD_PRLNNO,
                   PDFILE.PD_PRBAL,
                   PDFILE.PD_PRINTDUE,
                   PDFILE.PD_PRFINCHGRB,
                   PDFILE.PD_PRINSRB,
                   ISNULL(PDFILE.PD_PRPOCD,''),
                   ISNULL(CAST(PDFILE.PD_PRPODATE AS VARCHAR(10)),'0'),
                   PDFILE.PD_PRDELINQAMT,
                   PDFILE.PD_PRLNAMT,
                   PDFILE.PD_PRPDUE_DAYS,
                   PDFILE.PD_PRDLDISC_UNEARN,
                   PDFILE.PD_PRCONPROFILE,
                   PDFILE.PD_SUM_ORIGPURAMT,
                   PDFILE.PD_SUM_LNAMT,
                   PDFILE.PD_SUM_FINCHG,
                   PDFILE.PD_SUM_INTCHG,
                   PDFILE.PD_SUM_ORGTERM,
                   PDFILE.PD_SUM_REGPYAMT,
                   PDFILE.PD_SUM_EXTCHG,
                   PDFILE.PD_SUM_SERCHG,
                   PDFILE.PD_SUM_MAINTFEE,
                   ISNULL(PDFILE.PD_UNITPER_CD,''),
                   PDFILE.PD_UNITPER_FREQ,
                   PDFILE.PD_DAYS_IN_UNITPER,
                   PDFILE.PD_UNITPER_PER_YEAR,
                   ISNULL(PDFILE.PD_REPOCD,''),
                   ISNULL(PDFILE.PD_BR_TFR_FG,''),
                   PDFILE.PD_O3INS_DLPROC,
                   PDFILE.PD_INSPREM_DLADJ,
                   PDFILE.PD_PLAMT,
                   PDFILE.PD_MTD_PRIN,
                   PDFILE.PD_MTD_LC,
                   PDFILE.PD_MTD_INT,
                   PDFILE.PD_MTD_OT,
                   PDFILE.PD_MTD_EX,
                   PDFILE.PD_BNKRPT_CHAPTER,
                   ISNULL(PDFILE.PD_BMO_CON_STATUS,''),
                   ISNULL(PDFILE.PD_BMO_REC_STATUS,''),
                   PDFILE.PD_APP_NUMBER,
                   ISNULL(PDFILE.PD_GIFTNET,''),
                   PDFILE.PD_GIFTNET_OUR_PO,
                   ISNULL(PDFILE.PD_PROC_DISBURSED,''),
                   PDFILE.PD_NOODDPY,
                   PDFILE.PD_CONPROFILE_1,
                   PDFILE.PD_CONPROFILE_2,
                   PDFILE.PD_CONPROFILE_3,
                   PDFILE.PD_CONPROFILE_4,
                   PDFILE.PD_CONPROFILE_5,
                   PDFILE.PD_RECPROFILE_1,
                   PDFILE.PD_RECPROFILE_2,
                   PDFILE.PD_RECPROFILE_3,
                   PDFILE.PD_RECPROFILE_4,
                   PDFILE.PD_RECPROFILE_5,
                   ISNULL(PDFILE.PD_TRW_STATUS,''),
                   ISNULL(PDFILE.PD_ORIGIN,''),
                   ISNULL(PDFILE.PD_PO_TRCD,''),
                   PDFILE.PD_NEXIS_CRSCORE,
                   ISNULL(PDFILE.PD_TRW_B_CONSUMER_IND,''),
                   ISNULL(PDFILE.PD_PARVHELD_PAID_FG,''),
                   ISNULL(PDFILE.PD_ACCT_FROZEN,''),
                   ISNULL(PDFILE.PD_TRW_SPECIAL_CODE,''),
                   ISNULL(PDFILE.PD_CREATION_STATUS,''),
                   PDFILE.PD_TOTNOLCHG,
                   ISNULL(PDFILE.PD_LBOX,''),
                   PDFILE.PD_EXTRA_PRIN_LFP,
                   PDFILE.PD_REMAIN_INT_SINCE_LFP,
                   PDFILE.PD_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(PDFILE.PD_INTERIM_COUPON,''),
                   PDFILE.PD_DLCOMM_O3_CMSI,
                   PDFILE.PD_DLCOMM_PP_CMSI,
                   ISNULL(PDFILE.PD_TRW_J_CONSUMER_IND,''),
                   ISNULL(PDFILE.PD_ACH,''),
                   ISNULL(PDFILE.PD_1099_C_FG,''),
                   ISNULL(PDFILE.PD_TRW_COMPLIANCE_CODE,''),
                   ISNULL(PDFILE.PD_UNWIND_REASON_CODE,''),
                   ISNULL(PDFILE.PD_PBI_PBD_FG,''),
                   ISNULL(PDFILE.PD_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(PDFILE.PD_APPROVED_BY_USER,''),
                   ISNULL(PDFILE.PD_APPROVED_BY_CODE,''),
                   ISNULL(CAST(PDFILE.PD_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_APPROVED_BY_TIME,
                   ISNULL(CAST(PDFILE.PD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_LTOUCH_TIME,
                   ISNULL(PDFILE.PD_HOLD_COLLATERAL,''),
                   ISNULL(PDFILE.PD_DLPAID,''),
                   PDFILE.PD_CKNO,
                   ISNULL(PDFILE.PD_FREE1,''),
                   ISNULL(PDFILE.PD_FREE2,''),
                   ISNULL(PDFILE.PD_FREE_SPACE_FILLER,''),
                   PDFILE.PD_POSTING_BR,
                   ISNULL(PDFILE.PD_OTHBR_FG,''),
                   PDFILE.PD_APPROVAL_LIMIT,
                   ISNULL(PDFILE.PD_APPROVAL_USERID,''),
                   PDFILE.PD_MIN_CRED_SCORE,
                   ISNULL(PDFILE.PD_CRED_SCORE_USERID,''),
                   PDFILE.PD_FINAMT,
                   ISNULL(PDFILE.PD_PROCEEDS_FG,''),
                   ISNULL(PDFILE.PD_POINTS_FG,''),
                   ISNULL(PDFILE.PD_OVRDRATE_FG,''),
                   ISNULL(PDFILE.PD_INS_FG_1,''),
                   ISNULL(PDFILE.PD_INS_FG_2,''),
                   ISNULL(PDFILE.PD_INS_FG_3,''),
                   ISNULL(PDFILE.PD_INS_FG_4,''),
                   ISNULL(PDFILE.PD_INS_FG_5,''),
                   ISNULL(PDFILE.PD_INS_FG_6,''),
                   ISNULL(PDFILE.PD_INS_FG_7,''),
                   ISNULL(PDFILE.PD_INS_FG_8,''),
                   ISNULL(PDFILE.PD_INS_FG_9,''),
                   ISNULL(PDFILE.PD_INS_FG_10,''),
                   ISNULL(PDFILE.PD_SERCHG_FG,''),
                   ISNULL(PDFILE.PD_MAINTFEE_FG,''),
                   ISNULL(PDFILE.PD_EXTCHG_FG,''),
                   ISNULL(PDFILE.PD_REGPYAMT_FG,''),
                   ISNULL(PDFILE.PD_FEECD_1,''),
                   ISNULL(PDFILE.PD_FEECD_2,''),
                   ISNULL(PDFILE.PD_FEECD_3,''),
                   ISNULL(PDFILE.PD_FEECD_4,''),
                   ISNULL(PDFILE.PD_FEECD_5,''),
                   ISNULL(PDFILE.PD_FEECD_6,''),
                   ISNULL(PDFILE.PD_FEECD_7,''),
                   ISNULL(PDFILE.PD_FEECD_8,''),
                   ISNULL(PDFILE.PD_FEECD_9,''),
                   ISNULL(PDFILE.PD_FEECD_10,''),
                   ISNULL(PDFILE.PD_FEECD_11,''),
                   ISNULL(PDFILE.PD_FEECD_12,''),
                   ISNULL(PDFILE.PD_FEECD_13,''),
                   ISNULL(PDFILE.PD_FEECD_14,''),
                   ISNULL(PDFILE.PD_FEECD_15,''),
                   ISNULL(PDFILE.PD_FEECD_16,''),
                   ISNULL(PDFILE.PD_FEECD_17,''),
                   ISNULL(PDFILE.PD_FEECD_18,''),
                   ISNULL(PDFILE.PD_FEECD_19,''),
                   ISNULL(PDFILE.PD_FEECD_20,''),
                   PDFILE.PD_PRPAID_AHEAD,
                   PDFILE.PD_RESTRICT_ADJ,
                   PDFILE.PD_DUAL_LOAN_ADJ,
                   PDFILE.PD_CAN_LOAN_MAX,
                   PDFILE.PD_PERDOWN,
                   ISNULL(PDFILE.PD_SELLPR_FG,''),
                   ISNULL(PDFILE.PD_DOWNPY_FG,''),
                   ISNULL(PDFILE.PD_PERDOWN_FG,''),
                   ISNULL(PDFILE.PD_SELL_DOWN_FG,''),
                   ISNULL(PDFILE.PD_BALLOON_FG,''),
                   ISNULL(PDFILE.PD_BALLOON_LAST_FG,''),
                   ISNULL(PDFILE.PD_FEECD_FG_1,''),
                   ISNULL(PDFILE.PD_FEECD_FG_2,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_1,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_1,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_1,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_2,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_2,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_2,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_3,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_3,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_3,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_4,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_4,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_4,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_5,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_5,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_5,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_6,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_6,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_6,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_7,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_7,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_7,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_8,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_8,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_8,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_9,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_9,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_9,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_10,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_10,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_10,''),
                   ISNULL(PDFILE.PD_DLDISC_FG,''),
                   ISNULL(PDFILE.PD_DLRVPRIN_FG_1,''),
                   ISNULL(PDFILE.PD_DLRVPRIN_FG_2,''),
                   ISNULL(PDFILE.PD_DLRVPRIN_FG_3,''),
                   ISNULL(PDFILE.PD_DLRVCHGS_FG_1,''),
                   ISNULL(PDFILE.PD_DLRVCHGS_FG_2,''),
                   ISNULL(PDFILE.PD_DLRVCHGS_FG_3,''),
                   ISNULL(PDFILE.PD_DLPARVHELD_FG,''),
                   ISNULL(PDFILE.PD_DLPARVPAID_FG,''),
                   ISNULL(PDFILE.PD_ONE_MTH_NO_DAY_EXT_SW,''),
                   ISNULL(PDFILE.PD_ASSIGN_RN_ACCTNO,''),
                   PDFILE.PD_APR_FINAMT,
                   ISNULL(PDFILE.PD_DLCOMM_PP_CMSI_FG,''),
                   ISNULL(PDFILE.PD_DLCOMM_O3_CMSI_FG,''),
                   ISNULL(PDFILE.PD_FILLER_TO_MATCH_LN,''),
                   ISNULL(PDFILE.PD2_ID,''),
                   PDFILE.PD2_BRANCH,
                   ISNULL(PDFILE.PD2_BWNAME,'')
            FROM DBO.PDFILE
            WHERE PDFILE.PD_ID >= :QPD1-WBEG-ID
              AND PDFILE.PD_ID <= :QPD1-WEND-ID
              AND PDFILE.PD_BRANCH >= :QPD1-WBEG-BRANCH
              AND PDFILE.PD_BRANCH <= :QPD1-WEND-BRANCH
              AND PDFILE.PD_SOC_SEC >= :QPD1-WBEG-SOC-SEC
              AND PDFILE.PD_SOC_SEC <= :QPD1-WEND-SOC-SEC
            ORDER BY PDFILE.PD_ID,
                     PDFILE.PD_BRANCH,
                     PDFILE.PD_SOC_SEC
           END-EXEC.

           EXEC SQL
               OPEN PD1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QPD1-WBEG-BRANCH
                          QPD1-WEND-BRANCH
                          QPD1-WBEG-SOC-SEC
                          QPD1-WEND-SOC-SEC.

           MOVE SPACES TO QPD1-WBEG-ID
                          QPD1-WEND-ID.

           MOVE STAT---GOOD TO PD1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-PD1-FILE.
           PERFORM READ-IT.
           MOVE PD-PATH-OWNBR  TO PD-BRANCH. 
           MOVE PD-SELECT-PATH TO E-FILE.
           MOVE PD1-KEY        TO E-KEYX.

           MOVE PD-ID     TO QPD-ID.
           MOVE PD-BRANCH TO QPD-BRANCH.
           MOVE PD-SOC-SEC   TO QPD-SOC-SEC.

           EXEC SQL
            SELECT ISNULL(PDFILE.PD_ID,''),
                   PDFILE.PD_BRANCH,
                   PDFILE.PD_SOC_SEC,
                   PDFILE.PD_OWNBR,
                   PDFILE.PD_ACCTNO,
                   PDFILE.PD_CLASS,
                   PDFILE.PD_SUBCLASS,
                   PDFILE.PD_LAWCODE,
                   PDFILE.PD_SSNO1,
                   ISNULL(PDFILE.PD_MAKERCD1,''),
                   PDFILE.PD_SSNO2,
                   ISNULL(PDFILE.PD_MAKERCD2,''),
                   PDFILE.PD_SSNO3,
                   ISNULL(PDFILE.PD_MAKERCD3,''),
                   PDFILE.PD_SSNO4,
                   ISNULL(PDFILE.PD_MAKERCD4,''),
                   ISNULL(PDFILE.PD_ORGST,''),
                   PDFILE.PD_ORGBR,
                   ISNULL(PDFILE.PD_OWNST,''),
                   PDFILE.PD_SPRCLASS,
                   ISNULL(PDFILE.PD_CONVERCD,''),
                   ISNULL(PDFILE.PD_MIDSTREAM,''),
                   ISNULL(PDFILE.PD_PURCD,''),
                   ISNULL(PDFILE.PD_SRCD,''),
                   ISNULL(PDFILE.PD_COLLCD,''),
                   ISNULL(PDFILE.PD_COLLID,''),
                   ISNULL(PDFILE.PD_POCD,''),
                   ISNULL(PDFILE.PD_PLCD,''),
                   ISNULL(PDFILE.PD_PLTYPE,''),
                   ISNULL(PDFILE.PD_CONFIDENTCD,''),
                   ISNULL(PDFILE.PD_ALLOTCD,''),
                   ISNULL(PDFILE.PD_RECASTCD,''),
                   ISNULL(PDFILE.PD_SKIP_TRW,''),
                   ISNULL(PDFILE.PD_STATUSFG,''),
                   ISNULL(PDFILE.PD_SMALLOAN,''),
                   ISNULL(PDFILE.PD_SALEFIN,''),
                   ISNULL(PDFILE.PD_SERVACCT,''),
                   ISNULL(PDFILE.PD_LOANTYPE,''),
                   ISNULL(PDFILE.PD_ADDON_FG,''),
                   ISNULL(PDFILE.PD_PAY_SCHLD_FG,''),
                   PDFILE.PD_LC_CODE,
                   ISNULL(PDFILE.PD_CAN_LOAN_NO,''),
                   ISNULL(PDFILE.PD_STATFLAGS,''),
                   ISNULL(CAST(PDFILE.PD_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PURDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_CASHDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_FILEDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_JDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PLDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_REDUDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_REPODATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_SOLDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_APRATE,
                   PDFILE.PD_EFFRATE,
                   PDFILE.PD_SMPRATE,
                   PDFILE.PD_YLDRATE,
                   PDFILE.PD_OVRDRATE,
                   PDFILE.PD_ENDORATE,
                   PDFILE.PD_JDRATE,
                   PDFILE.PD_ACCRATE,
                   PDFILE.PD_REDURATE,
                   PDFILE.PD_CONRATE,
                   PDFILE.PD_SERVPER,
                   PDFILE.PD_SELLPR,
                   PDFILE.PD_DOWNPY,
                   PDFILE.PD_TRADEIN,
                   PDFILE.PD_LNAMT,
                   PDFILE.PD_FINCHG,
                   PDFILE.PD_INTCHG,
                   PDFILE.PD_SERCHG,
                   PDFILE.PD_EXTCHG,
                   PDFILE.PD_MAINTFEE,
                   PDFILE.PD_POINTS,
                   PDFILE.PD_PROCEEDS,
                   PDFILE.PD_ORGTERM,
                   PDFILE.PD_1STPYAMT,
                   PDFILE.PD_NOREGPY,
                   PDFILE.PD_REGPYAMT,
                   PDFILE.PD_LASTPYAMT,
                   PDFILE.PD_PROCASH,
                   PDFILE.PD_TILLNO,
                   ISNULL(PDFILE.PD_STATE_FLAG,''),
                   PDFILE.PD_STATE_WORKER,
                   PDFILE.PD_STATE_WORKER2,
                   ISNULL(PDFILE.PD_FEECODE_1,''),
                   PDFILE.PD_FEEAMT_1,
                   ISNULL(PDFILE.PD_FEEOURS_1,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_1,''),
                   ISNULL(PDFILE.PD_FEECODE_2,''),
                   PDFILE.PD_FEEAMT_2,
                   ISNULL(PDFILE.PD_FEEOURS_2,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_2,''),
                   ISNULL(PDFILE.PD_FEECODE_3,''),
                   PDFILE.PD_FEEAMT_3,
                   ISNULL(PDFILE.PD_FEEOURS_3,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_3,''),
                   ISNULL(PDFILE.PD_FEECODE_4,''),
                   PDFILE.PD_FEEAMT_4,
                   ISNULL(PDFILE.PD_FEEOURS_4,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_4,''),
                   ISNULL(PDFILE.PD_FEECODE_5,''),
                   PDFILE.PD_FEEAMT_5,
                   ISNULL(PDFILE.PD_FEEOURS_5,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_5,''),
                   ISNULL(PDFILE.PD_FEECODE_6,''),
                   PDFILE.PD_FEEAMT_6,
                   ISNULL(PDFILE.PD_FEEOURS_6,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_6,''),
                   ISNULL(PDFILE.PD_FEECODE_7,''),
                   PDFILE.PD_FEEAMT_7,
                   ISNULL(PDFILE.PD_FEEOURS_7,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_7,''),
                   ISNULL(PDFILE.PD_FEECODE_8,''),
                   PDFILE.PD_FEEAMT_8,
                   ISNULL(PDFILE.PD_FEEOURS_8,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_8,''),
                   ISNULL(PDFILE.PD_FEECODE_9,''),
                   PDFILE.PD_FEEAMT_9,
                   ISNULL(PDFILE.PD_FEEOURS_9,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_9,''),
                   ISNULL(PDFILE.PD_FEECODE_10,''),
                   PDFILE.PD_FEEAMT_10,
                   ISNULL(PDFILE.PD_FEEOURS_10,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_10,''),
                   ISNULL(PDFILE.PD_FEECODE_11,''),
                   PDFILE.PD_FEEAMT_11,
                   ISNULL(PDFILE.PD_FEEOURS_11,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_11,''),
                   ISNULL(PDFILE.PD_FEECODE_12,''),
                   PDFILE.PD_FEEAMT_12,
                   ISNULL(PDFILE.PD_FEEOURS_12,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_12,''),
                   ISNULL(PDFILE.PD_FEECODE_13,''),
                   PDFILE.PD_FEEAMT_13,
                   ISNULL(PDFILE.PD_FEEOURS_13,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_13,''),
                   ISNULL(PDFILE.PD_FEECODE_14,''),
                   PDFILE.PD_FEEAMT_14,
                   ISNULL(PDFILE.PD_FEEOURS_14,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_14,''),
                   ISNULL(PDFILE.PD_FEECODE_15,''),
                   PDFILE.PD_FEEAMT_15,
                   ISNULL(PDFILE.PD_FEEOURS_15,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_15,''),
                   ISNULL(PDFILE.PD_FEECODE_16,''),
                   PDFILE.PD_FEEAMT_16,
                   ISNULL(PDFILE.PD_FEEOURS_16,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_16,''),
                   ISNULL(PDFILE.PD_FEECODE_17,''),
                   PDFILE.PD_FEEAMT_17,
                   ISNULL(PDFILE.PD_FEEOURS_17,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_17,''),
                   ISNULL(PDFILE.PD_FEECODE_18,''),
                   PDFILE.PD_FEEAMT_18,
                   ISNULL(PDFILE.PD_FEEOURS_18,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_18,''),
                   ISNULL(PDFILE.PD_FEECODE_19,''),
                   PDFILE.PD_FEEAMT_19,
                   ISNULL(PDFILE.PD_FEEOURS_19,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_19,''),
                   ISNULL(PDFILE.PD_FEECODE_20,''),
                   PDFILE.PD_FEEAMT_20,
                   ISNULL(PDFILE.PD_FEEOURS_20,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_20,''),
                   ISNULL(PDFILE.PD_INSURED,''),
                   ISNULL(PDFILE.PD_INSTYPES,''),
                   ISNULL(PDFILE.PD_INSELIGIBLE,''),
                   PDFILE.PD_INSCOMP_1,
                   ISNULL(PDFILE.PD_INSOURS_1,''),
                   PDFILE.PD_INSEFF_DATE_1,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_1,
                   PDFILE.PD_INSCOVR_1,
                   PDFILE.PD_INSCOMM_1,
                   PDFILE.PD_INSANTICERN_1,
                   PDFILE.PD_INSANTICADJ_1,
                   PDFILE.PD_INSUFERN_1,
                   PDFILE.PD_INSACCERN_1,
                   PDFILE.PD_INSEARN_ORDER_1,
                   ISNULL(PDFILE.PD_INSFINCHG_1,''),
                   PDFILE.PD_INSCOMP_2,
                   ISNULL(PDFILE.PD_INSOURS_2,''),
                   PDFILE.PD_INSEFF_DATE_2,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_2,
                   PDFILE.PD_INSCOVR_2,
                   PDFILE.PD_INSCOMM_2,
                   PDFILE.PD_INSANTICERN_2,
                   PDFILE.PD_INSANTICADJ_2,
                   PDFILE.PD_INSUFERN_2,
                   PDFILE.PD_INSACCERN_2,
                   PDFILE.PD_INSEARN_ORDER_2,
                   ISNULL(PDFILE.PD_INSFINCHG_2,''),
                   PDFILE.PD_INSCOMP_3,
                   ISNULL(PDFILE.PD_INSOURS_3,''),
                   PDFILE.PD_INSEFF_DATE_3,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_3,
                   PDFILE.PD_INSCOVR_3,
                   PDFILE.PD_INSCOMM_3,
                   PDFILE.PD_INSANTICERN_3,
                   PDFILE.PD_INSANTICADJ_3,
                   PDFILE.PD_INSUFERN_3,
                   PDFILE.PD_INSACCERN_3,
                   PDFILE.PD_INSEARN_ORDER_3,
                   ISNULL(PDFILE.PD_INSFINCHG_3,''),
                   PDFILE.PD_INSCOMP_4,
                   ISNULL(PDFILE.PD_INSOURS_4,''),
                   PDFILE.PD_INSEFF_DATE_4,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_4,
                   PDFILE.PD_INSCOVR_4,
                   PDFILE.PD_INSCOMM_4,
                   PDFILE.PD_INSANTICERN_4,
                   PDFILE.PD_INSANTICADJ_4,
                   PDFILE.PD_INSUFERN_4,
                   PDFILE.PD_INSACCERN_4,
                   PDFILE.PD_INSEARN_ORDER_4,
                   ISNULL(PDFILE.PD_INSFINCHG_4,''),
                   PDFILE.PD_INSCOMP_5,
                   ISNULL(PDFILE.PD_INSOURS_5,''),
                   PDFILE.PD_INSEFF_DATE_5,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_5,
                   PDFILE.PD_INSCOVR_5,
                   PDFILE.PD_INSCOMM_5,
                   PDFILE.PD_INSANTICERN_5,
                   PDFILE.PD_INSANTICADJ_5,
                   PDFILE.PD_INSUFERN_5,
                   PDFILE.PD_INSACCERN_5,
                   PDFILE.PD_INSEARN_ORDER_5,
                   ISNULL(PDFILE.PD_INSFINCHG_5,''),
                   PDFILE.PD_INSCOMP_6,
                   ISNULL(PDFILE.PD_INSOURS_6,''),
                   PDFILE.PD_INSEFF_DATE_6,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_6,
                   PDFILE.PD_INSCOVR_6,
                   PDFILE.PD_INSCOMM_6,
                   PDFILE.PD_INSANTICERN_6,
                   PDFILE.PD_INSANTICADJ_6,
                   PDFILE.PD_INSUFERN_6,
                   PDFILE.PD_INSACCERN_6,
                   PDFILE.PD_INSEARN_ORDER_6,
                   ISNULL(PDFILE.PD_INSFINCHG_6,''),
                   PDFILE.PD_INSCOMP_7,
                   ISNULL(PDFILE.PD_INSOURS_7,''),
                   PDFILE.PD_INSEFF_DATE_7,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_7,
                   PDFILE.PD_INSCOVR_7,
                   PDFILE.PD_INSCOMM_7,
                   PDFILE.PD_INSANTICERN_7,
                   PDFILE.PD_INSANTICADJ_7,
                   PDFILE.PD_INSUFERN_7,
                   PDFILE.PD_INSACCERN_7,
                   PDFILE.PD_INSEARN_ORDER_7,
                   ISNULL(PDFILE.PD_INSFINCHG_7,''),
                   PDFILE.PD_INSCOMP_8,
                   ISNULL(PDFILE.PD_INSOURS_8,''),
                   PDFILE.PD_INSEFF_DATE_8,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_8,
                   PDFILE.PD_INSCOVR_8,
                   PDFILE.PD_INSCOMM_8,
                   PDFILE.PD_INSANTICERN_8,
                   PDFILE.PD_INSANTICADJ_8,
                   PDFILE.PD_INSUFERN_8,
                   PDFILE.PD_INSACCERN_8,
                   PDFILE.PD_INSEARN_ORDER_8,
                   ISNULL(PDFILE.PD_INSFINCHG_8,''),
                   PDFILE.PD_INSCOMP_9,
                   ISNULL(PDFILE.PD_INSOURS_9,''),
                   PDFILE.PD_INSEFF_DATE_9,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_9,
                   PDFILE.PD_INSCOVR_9,
                   PDFILE.PD_INSCOMM_9,
                   PDFILE.PD_INSANTICERN_9,
                   PDFILE.PD_INSANTICADJ_9,
                   PDFILE.PD_INSUFERN_9,
                   PDFILE.PD_INSACCERN_9,
                   PDFILE.PD_INSEARN_ORDER_9,
                   ISNULL(PDFILE.PD_INSFINCHG_9,''),
                   PDFILE.PD_INSCOMP_10,
                   ISNULL(PDFILE.PD_INSOURS_10,''),
                   PDFILE.PD_INSEFF_DATE_10,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_10,
                   PDFILE.PD_INSCOVR_10,
                   PDFILE.PD_INSCOMM_10,
                   PDFILE.PD_INSANTICERN_10,
                   PDFILE.PD_INSANTICADJ_10,
                   PDFILE.PD_INSUFERN_10,
                   PDFILE.PD_INSACCERN_10,
                   PDFILE.PD_INSEARN_ORDER_10,
                   ISNULL(PDFILE.PD_INSFINCHG_10,''),
                   PDFILE.PD_PY_YYYYMM_RECDEL,
                   PDFILE.PD_MTD_PAYS_RECDEL,
                   ISNULL(PDFILE.PD_GARNISH_EXTR,''),
                   ISNULL(PDFILE.PD_SHIP_SAME_FG,''),
                   PDFILE.PD_AD_D_INS_COVR,
                   PDFILE.PD_TOT_PARTIAL_CHGOFF,
                   ISNULL(PDFILE.PD_LPO,''),
                   ISNULL(PDFILE.PD_REP_CD,''),
                   PDFILE.PD_EMP_ID,
                   ISNULL(PDFILE.PD_PROCESS_CD,''),
                   PDFILE.PD_WCBC_CREDITLIM,
                   ISNULL(PDFILE.PD_FOLLOWUP_CD,''),
                   ISNULL(PDFILE.PD_MIP_FG,''),
                   PDFILE.PD_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(PDFILE.PD_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_MIP_TOT_CLPAID,
                   ISNULL(CAST(PDFILE.PD_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_MIP_TOT_AHPAID,
                   PDFILE.PD_IL_GROSS_INCOME,
                   PDFILE.PD_IL_GROSS_INCOME2,
                   ISNULL(PDFILE.PD_RESCISSION,''),
                   ISNULL(PDFILE.PD_INTERIM_TRW,''),
                   PDFILE.PD_DLNO,
                   PDFILE.PD_DLPOOL,
                   ISNULL(PDFILE.PD_DLRECOURSE,''),
                   PDFILE.PD_DLPARTRCAMT,
                   PDFILE.PD_ORIGPURAMT,
                   PDFILE.PD_PURBAL,
                   PDFILE.PD_DLRVPRIN_1,
                   PDFILE.PD_DLRVPRIN_2,
                   PDFILE.PD_DLRVPRIN_3,
                   PDFILE.PD_DLRVCHGS_1,
                   PDFILE.PD_DLRVCHGS_2,
                   PDFILE.PD_DLRVCHGS_3,
                   PDFILE.PD_DLPARVHELD,
                   PDFILE.PD_DLPARVPAID,
                   PDFILE.PD_DLDISC,
                   ISNULL(CAST(PDFILE.PD_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_CURBAL,
                   PDFILE.PD_INTBAL,
                   PDFILE.PD_LCBAL,
                   PDFILE.PD_OTHBAL,
                   PDFILE.PD_OT2BAL,
                   PDFILE.PD_LCPAID,
                   PDFILE.PD_LCPARTIALS,
                   PDFILE.PD_STMT_COUNTER,
                   PDFILE.PD_PRIOR_STMT_COUNTER,
                   ISNULL(PDFILE.PD_FROZEN_REASON,''),
                   ISNULL(PDFILE.PD_COUPON_STATEMENT,''),
                   PDFILE.PD_MONTHEND_BAL,
                   PDFILE.PD_ANTICTERM,
                   PDFILE.PD_ANTICERN_1,
                   PDFILE.PD_ANTICADJ_1,
                   PDFILE.PD_UFERN_1,
                   PDFILE.PD_ACCERN_1,
                   PDFILE.PD_EARN_ORDER_1,
                   PDFILE.PD_ANTICERN_2,
                   PDFILE.PD_ANTICADJ_2,
                   PDFILE.PD_UFERN_2,
                   PDFILE.PD_ACCERN_2,
                   PDFILE.PD_EARN_ORDER_2,
                   PDFILE.PD_ANTICERN_3,
                   PDFILE.PD_ANTICADJ_3,
                   PDFILE.PD_UFERN_3,
                   PDFILE.PD_ACCERN_3,
                   PDFILE.PD_EARN_ORDER_3,
                   PDFILE.PD_ANTICERN_4,
                   PDFILE.PD_ANTICADJ_4,
                   PDFILE.PD_UFERN_4,
                   PDFILE.PD_ACCERN_4,
                   PDFILE.PD_EARN_ORDER_4,
                   PDFILE.PD_ANTICERN_5,
                   PDFILE.PD_ANTICADJ_5,
                   PDFILE.PD_UFERN_5,
                   PDFILE.PD_ACCERN_5,
                   PDFILE.PD_EARN_ORDER_5,
                   PDFILE.PD_ANTICERN_6,
                   PDFILE.PD_ANTICADJ_6,
                   PDFILE.PD_UFERN_6,
                   PDFILE.PD_ACCERN_6,
                   PDFILE.PD_EARN_ORDER_6,
                   PDFILE.PD_ANTICERN_7,
                   PDFILE.PD_ANTICADJ_7,
                   PDFILE.PD_UFERN_7,
                   PDFILE.PD_ACCERN_7,
                   PDFILE.PD_EARN_ORDER_7,
                   PDFILE.PD_ANTICERN_8,
                   PDFILE.PD_ANTICADJ_8,
                   PDFILE.PD_UFERN_8,
                   PDFILE.PD_ACCERN_8,
                   PDFILE.PD_EARN_ORDER_8,
                   PDFILE.PD_ANTICERN_9,
                   PDFILE.PD_ANTICADJ_9,
                   PDFILE.PD_UFERN_9,
                   PDFILE.PD_ACCERN_9,
                   PDFILE.PD_EARN_ORDER_9,
                   PDFILE.PD_ANTICERN_10,
                   PDFILE.PD_ANTICADJ_10,
                   PDFILE.PD_UFERN_10,
                   PDFILE.PD_ACCERN_10,
                   PDFILE.PD_EARN_ORDER_10,
                   PDFILE.PD_LMO_PRIOR_INTEREST,
                   PDFILE.PD_PRIOR_UNEARNED,
                   ISNULL(PDFILE.PD_ERNDELQ_STOP,''),
                   PDFILE.PD_NONACRERN,
                   PDFILE.PD_PRIOR_NONACRERN,
                   ISNULL(CAST(PDFILE.PD_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_ERN_IB_PRINBAL,
                   PDFILE.PD_ERN_IB_INTOWE,
                   PDFILE.PD_TOTERN,
                   PDFILE.PD_TOTOTH,
                   PDFILE.PD_TOTINT,
                   PDFILE.PD_TOTLCHG,
                   PDFILE.PD_TOTDEF,
                   PDFILE.PD_TOTSERACCT,
                   PDFILE.PD_LCWAIVERS,
                   PDFILE.PD_INTWAIVERS,
                   PDFILE.PD_TOTPAYMNTD,
                   PDFILE.PD_TOTEXCPAYMNTD,
                   PDFILE.PD_TOTNODEF,
                   PDFILE.PD_YTDNODEF,
                   PDFILE.PD_TOTNODEFACT,
                   PDFILE.PD_YTDNODEFACT,
                   PDFILE.PD_TOTNOALDEL,
                   PDFILE.PD_YTDNOALDEL,
                   PDFILE.PD_YTDINTPD,
                   PDFILE.PD_LYRINTPD,
                   PDFILE.PD_TOTINTPD,
                   PDFILE.PD_RECASTINT,
                   PDFILE.PD_RECASTPRIN,
                   ISNULL(PDFILE.PD_CL_REBATE,''),
                   ISNULL(PDFILE.PD_AH_REBATE,''),
                   ISNULL(PDFILE.PD_PP_REBATE,''),
                   ISNULL(PDFILE.PD_INT_REBATE,''),
                   ISNULL(PDFILE.PD_SC_REBATE,''),
                   ISNULL(PDFILE.PD_MF_REBATE,''),
                   ISNULL(PDFILE.PD_DF_REBATE,''),
                   ISNULL(PDFILE.PD_EX_REBATE,''),
                   ISNULL(PDFILE.PD_O1_REBATE,''),
                   ISNULL(PDFILE.PD_O2_REBATE,''),
                   ISNULL(PDFILE.PD_O3_REBATE,''),
                   ISNULL(PDFILE.PD_O4_REBATE,''),
                   ISNULL(PDFILE.PD_O5_REBATE,''),
                   ISNULL(PDFILE.PD_O6_REBATE,''),
                   ISNULL(PDFILE.PD_O7_REBATE,''),
                   ISNULL(PDFILE.PD_UN_O7_REB,''),
                   ISNULL(PDFILE.PD_UN_O8_REB,''),
                   ISNULL(PDFILE.PD_UN_O9_REB,''),
                   ISNULL(PDFILE.PD_UN_O10_REB,''),
                   ISNULL(PDFILE.PD_PLREASON,''),
                   ISNULL(PDFILE.PD_COLLECTOR,''),
                   ISNULL(PDFILE.PD_ACTIONCD,''),
                   ISNULL(PDFILE.PD_ACTIONCD2,''),
                   ISNULL(PDFILE.PD_ATCODE,''),
                   ISNULL(PDFILE.PD_RECODE,''),
                   ISNULL(PDFILE.PD_ESCROW_FG,''),
                   PDFILE.PD_PROMDAYS_PASTDUE,
                   ISNULL(PDFILE.PD_PROMID,''),
                   ISNULL(CAST(PDFILE.PD_PROMDATE AS VARCHAR(10)),'0'),
                   PDFILE.PD_PROMAMT,
                   ISNULL(CAST(PDFILE.PD_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_AMT_PAID_LAST,
                   PDFILE.PD_LASTCFORMNO,
                   ISNULL(CAST(PDFILE.PD_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_DELCNT,
                   PDFILE.PD_DELCNT2,
                   PDFILE.PD_CREDITLIM,
                   PDFILE.PD_CRSCORE,
                   PDFILE.PD_LASTSFORMNO,
                   ISNULL(CAST(PDFILE.PD_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_SOLCNT,
                   ISNULL(PDFILE.PD_MESSAGE,''),
                   PDFILE.PD_OLDACCTNO,
                   PDFILE.PD_PRLNNO,
                   PDFILE.PD_PRBAL,
                   PDFILE.PD_PRINTDUE,
                   PDFILE.PD_PRFINCHGRB,
                   PDFILE.PD_PRINSRB,
                   ISNULL(PDFILE.PD_PRPOCD,''),
                   ISNULL(CAST(PDFILE.PD_PRPODATE AS VARCHAR(10)),'0'),
                   PDFILE.PD_PRDELINQAMT,
                   PDFILE.PD_PRLNAMT,
                   PDFILE.PD_PRPDUE_DAYS,
                   PDFILE.PD_PRDLDISC_UNEARN,
                   PDFILE.PD_PRCONPROFILE,
                   PDFILE.PD_SUM_ORIGPURAMT,
                   PDFILE.PD_SUM_LNAMT,
                   PDFILE.PD_SUM_FINCHG,
                   PDFILE.PD_SUM_INTCHG,
                   PDFILE.PD_SUM_ORGTERM,
                   PDFILE.PD_SUM_REGPYAMT,
                   PDFILE.PD_SUM_EXTCHG,
                   PDFILE.PD_SUM_SERCHG,
                   PDFILE.PD_SUM_MAINTFEE,
                   ISNULL(PDFILE.PD_UNITPER_CD,''),
                   PDFILE.PD_UNITPER_FREQ,
                   PDFILE.PD_DAYS_IN_UNITPER,
                   PDFILE.PD_UNITPER_PER_YEAR,
                   ISNULL(PDFILE.PD_REPOCD,''),
                   ISNULL(PDFILE.PD_BR_TFR_FG,''),
                   PDFILE.PD_O3INS_DLPROC,
                   PDFILE.PD_INSPREM_DLADJ,
                   PDFILE.PD_PLAMT,
                   PDFILE.PD_MTD_PRIN,
                   PDFILE.PD_MTD_LC,
                   PDFILE.PD_MTD_INT,
                   PDFILE.PD_MTD_OT,
                   PDFILE.PD_MTD_EX,
                   PDFILE.PD_BNKRPT_CHAPTER,
                   ISNULL(PDFILE.PD_BMO_CON_STATUS,''),
                   ISNULL(PDFILE.PD_BMO_REC_STATUS,''),
                   PDFILE.PD_APP_NUMBER,
                   ISNULL(PDFILE.PD_GIFTNET,''),
                   PDFILE.PD_GIFTNET_OUR_PO,
                   ISNULL(PDFILE.PD_PROC_DISBURSED,''),
                   PDFILE.PD_NOODDPY,
                   PDFILE.PD_CONPROFILE_1,
                   PDFILE.PD_CONPROFILE_2,
                   PDFILE.PD_CONPROFILE_3,
                   PDFILE.PD_CONPROFILE_4,
                   PDFILE.PD_CONPROFILE_5,
                   PDFILE.PD_RECPROFILE_1,
                   PDFILE.PD_RECPROFILE_2,
                   PDFILE.PD_RECPROFILE_3,
                   PDFILE.PD_RECPROFILE_4,
                   PDFILE.PD_RECPROFILE_5,
                   ISNULL(PDFILE.PD_TRW_STATUS,''),
                   ISNULL(PDFILE.PD_ORIGIN,''),
                   ISNULL(PDFILE.PD_PO_TRCD,''),
                   PDFILE.PD_NEXIS_CRSCORE,
                   ISNULL(PDFILE.PD_TRW_B_CONSUMER_IND,''),
                   ISNULL(PDFILE.PD_PARVHELD_PAID_FG,''),
                   ISNULL(PDFILE.PD_ACCT_FROZEN,''),
                   ISNULL(PDFILE.PD_TRW_SPECIAL_CODE,''),
                   ISNULL(PDFILE.PD_CREATION_STATUS,''),
                   PDFILE.PD_TOTNOLCHG,
                   ISNULL(PDFILE.PD_LBOX,''),
                   PDFILE.PD_EXTRA_PRIN_LFP,
                   PDFILE.PD_REMAIN_INT_SINCE_LFP,
                   PDFILE.PD_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(PDFILE.PD_INTERIM_COUPON,''),
                   PDFILE.PD_DLCOMM_O3_CMSI,
                   PDFILE.PD_DLCOMM_PP_CMSI,
                   ISNULL(PDFILE.PD_TRW_J_CONSUMER_IND,''),
                   ISNULL(PDFILE.PD_ACH,''),
                   ISNULL(PDFILE.PD_1099_C_FG,''),
                   ISNULL(PDFILE.PD_TRW_COMPLIANCE_CODE,''),
                   ISNULL(PDFILE.PD_UNWIND_REASON_CODE,''),
                   ISNULL(PDFILE.PD_PBI_PBD_FG,''),
                   ISNULL(PDFILE.PD_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(PDFILE.PD_APPROVED_BY_USER,''),
                   ISNULL(PDFILE.PD_APPROVED_BY_CODE,''),
                   ISNULL(CAST(PDFILE.PD_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_APPROVED_BY_TIME,
                   ISNULL(CAST(PDFILE.PD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_LTOUCH_TIME,
                   ISNULL(PDFILE.PD_HOLD_COLLATERAL,''),
                   ISNULL(PDFILE.PD_DLPAID,''),
                   PDFILE.PD_CKNO,
                   ISNULL(PDFILE.PD_FREE1,''),
                   ISNULL(PDFILE.PD_FREE2,''),
                   ISNULL(PDFILE.PD_FREE_SPACE_FILLER,''),
                   PDFILE.PD_POSTING_BR,
                   ISNULL(PDFILE.PD_OTHBR_FG,''),
                   PDFILE.PD_APPROVAL_LIMIT,
                   ISNULL(PDFILE.PD_APPROVAL_USERID,''),
                   PDFILE.PD_MIN_CRED_SCORE,
                   ISNULL(PDFILE.PD_CRED_SCORE_USERID,''),
                   PDFILE.PD_FINAMT,
                   ISNULL(PDFILE.PD_PROCEEDS_FG,''),
                   ISNULL(PDFILE.PD_POINTS_FG,''),
                   ISNULL(PDFILE.PD_OVRDRATE_FG,''),
                   ISNULL(PDFILE.PD_INS_FG_1,''),
                   ISNULL(PDFILE.PD_INS_FG_2,''),
                   ISNULL(PDFILE.PD_INS_FG_3,''),
                   ISNULL(PDFILE.PD_INS_FG_4,''),
                   ISNULL(PDFILE.PD_INS_FG_5,''),
                   ISNULL(PDFILE.PD_INS_FG_6,''),
                   ISNULL(PDFILE.PD_INS_FG_7,''),
                   ISNULL(PDFILE.PD_INS_FG_8,''),
                   ISNULL(PDFILE.PD_INS_FG_9,''),
                   ISNULL(PDFILE.PD_INS_FG_10,''),
                   ISNULL(PDFILE.PD_SERCHG_FG,''),
                   ISNULL(PDFILE.PD_MAINTFEE_FG,''),
                   ISNULL(PDFILE.PD_EXTCHG_FG,''),
                   ISNULL(PDFILE.PD_REGPYAMT_FG,''),
                   ISNULL(PDFILE.PD_FEECD_1,''),
                   ISNULL(PDFILE.PD_FEECD_2,''),
                   ISNULL(PDFILE.PD_FEECD_3,''),
                   ISNULL(PDFILE.PD_FEECD_4,''),
                   ISNULL(PDFILE.PD_FEECD_5,''),
                   ISNULL(PDFILE.PD_FEECD_6,''),
                   ISNULL(PDFILE.PD_FEECD_7,''),
                   ISNULL(PDFILE.PD_FEECD_8,''),
                   ISNULL(PDFILE.PD_FEECD_9,''),
                   ISNULL(PDFILE.PD_FEECD_10,''),
                   ISNULL(PDFILE.PD_FEECD_11,''),
                   ISNULL(PDFILE.PD_FEECD_12,''),
                   ISNULL(PDFILE.PD_FEECD_13,''),
                   ISNULL(PDFILE.PD_FEECD_14,''),
                   ISNULL(PDFILE.PD_FEECD_15,''),
                   ISNULL(PDFILE.PD_FEECD_16,''),
                   ISNULL(PDFILE.PD_FEECD_17,''),
                   ISNULL(PDFILE.PD_FEECD_18,''),
                   ISNULL(PDFILE.PD_FEECD_19,''),
                   ISNULL(PDFILE.PD_FEECD_20,''),
                   PDFILE.PD_PRPAID_AHEAD,
                   PDFILE.PD_RESTRICT_ADJ,
                   PDFILE.PD_DUAL_LOAN_ADJ,
                   PDFILE.PD_CAN_LOAN_MAX,
                   PDFILE.PD_PERDOWN,
                   ISNULL(PDFILE.PD_SELLPR_FG,''),
                   ISNULL(PDFILE.PD_DOWNPY_FG,''),
                   ISNULL(PDFILE.PD_PERDOWN_FG,''),
                   ISNULL(PDFILE.PD_SELL_DOWN_FG,''),
                   ISNULL(PDFILE.PD_BALLOON_FG,''),
                   ISNULL(PDFILE.PD_BALLOON_LAST_FG,''),
                   ISNULL(PDFILE.PD_FEECD_FG_1,''),
                   ISNULL(PDFILE.PD_FEECD_FG_2,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_1,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_1,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_1,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_2,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_2,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_2,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_3,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_3,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_3,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_4,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_4,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_4,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_5,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_5,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_5,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_6,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_6,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_6,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_7,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_7,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_7,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_8,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_8,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_8,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_9,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_9,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_9,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_10,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_10,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_10,''),
                   ISNULL(PDFILE.PD_DLDISC_FG,''),
                   ISNULL(PDFILE.PD_DLRVPRIN_FG_1,''),
                   ISNULL(PDFILE.PD_DLRVPRIN_FG_2,''),
                   ISNULL(PDFILE.PD_DLRVPRIN_FG_3,''),
                   ISNULL(PDFILE.PD_DLRVCHGS_FG_1,''),
                   ISNULL(PDFILE.PD_DLRVCHGS_FG_2,''),
                   ISNULL(PDFILE.PD_DLRVCHGS_FG_3,''),
                   ISNULL(PDFILE.PD_DLPARVHELD_FG,''),
                   ISNULL(PDFILE.PD_DLPARVPAID_FG,''),
                   ISNULL(PDFILE.PD_ONE_MTH_NO_DAY_EXT_SW,''),
                   ISNULL(PDFILE.PD_ASSIGN_RN_ACCTNO,''),
                   PDFILE.PD_APR_FINAMT,
                   ISNULL(PDFILE.PD_DLCOMM_PP_CMSI_FG,''),
                   ISNULL(PDFILE.PD_DLCOMM_O3_CMSI_FG,''),
                   ISNULL(PDFILE.PD_FILLER_TO_MATCH_LN,''),
                   ISNULL(PDFILE.PD2_ID,''),
                   PDFILE.PD2_BRANCH,
                   ISNULL(PDFILE.PD2_BWNAME,'')
            INTO :QPD-REC
            FROM DBO.PDFILE
            WHERE PDFILE.PD_ID = :QPD-ID
              AND PDFILE.PD_BRANCH = :QPD-BRANCH
              AND PDFILE.PD_SOC_SEC = :QPD-SOC-SEC
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PD1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-PD-FIELDS.

      *-----------------------------------------------------------------
       READ-PD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE PD-PATH-OWNBR  TO PD-BRANCH.
           MOVE PD-SELECT-PATH TO E-FILE.
           MOVE PD1-KEY        TO E-KEYX.
           INITIALIZE QPD-REC.

           IF ( PD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH PD1_CURSOR INTO :QPD-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PD1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-PD-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-PD1-FILE.

           IF ( PD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE PD1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO PD1-CURSOR-STAT.

      *-----------------------------------------------------------------
       LOAD-PD2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( PD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( PD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO PD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO PD-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO PD-ALL-DATA
              MOVE PD-ALL-PATH             TO PD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( PD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO PD-ALL-BASE
              MOVE FILPATH-MACHINE         TO PD-ALL-MACHINE
              MOVE FILPATH-DATA            TO PD-ALL-DATA
              MOVE PD-ALL-PATH             TO PD-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-PDFILE NOT = "S" )
              MOVE PD-PATH-BEG             TO PD-SELECT-PATH-BEG
              MOVE PD-PATH-FILE            TO PD-SELECT-PATH-FILE
           ELSE
              MOVE PD-PATH-SQL             TO PD-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-PD2-FILE.
           PERFORM LOAD-PD2-FILE.
           PERFORM OPEN-IT.
           MOVE PD-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-PD2-FILE.
           PERFORM START-IT.
           MOVE PD-PATH-OWNBR  TO PD-BRANCH.
           MOVE PD-SELECT-PATH TO E-FILE.
           MOVE PD2-KEY        TO E-KEYX.

           IF ( PD2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-PD2-FILE.

           IF ( QPD2-WEND-ID = SPACES )
              MOVE PD-PATH-OWNBR        TO QPD2-WBEG-BRANCH
              MOVE PD-PATH-OWNBR        TO QPD2-WEND-BRANCH
              MOVE PD2-ID               TO QPD2-WBEG-ID
              MOVE ALL "Z"              TO QPD2-WEND-ID
              MOVE PD2-BWNAME           TO QPD2-WBEG-BWNAME
              MOVE ALL "Z"              TO QPD2-WEND-BWNAME.

           EXEC SQL
            DECLARE PD2_CURSOR CURSOR FOR
            SELECT ISNULL(PDFILE.PD_ID,''),
                   PDFILE.PD_BRANCH,
                   PDFILE.PD_SOC_SEC,
                   PDFILE.PD_OWNBR,
                   PDFILE.PD_ACCTNO,
                   PDFILE.PD_CLASS,
                   PDFILE.PD_SUBCLASS,
                   PDFILE.PD_LAWCODE,
                   PDFILE.PD_SSNO1,
                   ISNULL(PDFILE.PD_MAKERCD1,''),
                   PDFILE.PD_SSNO2,
                   ISNULL(PDFILE.PD_MAKERCD2,''),
                   PDFILE.PD_SSNO3,
                   ISNULL(PDFILE.PD_MAKERCD3,''),
                   PDFILE.PD_SSNO4,
                   ISNULL(PDFILE.PD_MAKERCD4,''),
                   ISNULL(PDFILE.PD_ORGST,''),
                   PDFILE.PD_ORGBR,
                   ISNULL(PDFILE.PD_OWNST,''),
                   PDFILE.PD_SPRCLASS,
                   ISNULL(PDFILE.PD_CONVERCD,''),
                   ISNULL(PDFILE.PD_MIDSTREAM,''),
                   ISNULL(PDFILE.PD_PURCD,''),
                   ISNULL(PDFILE.PD_SRCD,''),
                   ISNULL(PDFILE.PD_COLLCD,''),
                   ISNULL(PDFILE.PD_COLLID,''),
                   ISNULL(PDFILE.PD_POCD,''),
                   ISNULL(PDFILE.PD_PLCD,''),
                   ISNULL(PDFILE.PD_PLTYPE,''),
                   ISNULL(PDFILE.PD_CONFIDENTCD,''),
                   ISNULL(PDFILE.PD_ALLOTCD,''),
                   ISNULL(PDFILE.PD_RECASTCD,''),
                   ISNULL(PDFILE.PD_SKIP_TRW,''),
                   ISNULL(PDFILE.PD_STATUSFG,''),
                   ISNULL(PDFILE.PD_SMALLOAN,''),
                   ISNULL(PDFILE.PD_SALEFIN,''),
                   ISNULL(PDFILE.PD_SERVACCT,''),
                   ISNULL(PDFILE.PD_LOANTYPE,''),
                   ISNULL(PDFILE.PD_ADDON_FG,''),
                   ISNULL(PDFILE.PD_PAY_SCHLD_FG,''),
                   PDFILE.PD_LC_CODE,
                   ISNULL(PDFILE.PD_CAN_LOAN_NO,''),
                   ISNULL(PDFILE.PD_STATFLAGS,''),
                   ISNULL(CAST(PDFILE.PD_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PURDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_CASHDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_FILEDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_JDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PLDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_REDUDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_REPODATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_SOLDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_APRATE,
                   PDFILE.PD_EFFRATE,
                   PDFILE.PD_SMPRATE,
                   PDFILE.PD_YLDRATE,
                   PDFILE.PD_OVRDRATE,
                   PDFILE.PD_ENDORATE,
                   PDFILE.PD_JDRATE,
                   PDFILE.PD_ACCRATE,
                   PDFILE.PD_REDURATE,
                   PDFILE.PD_CONRATE,
                   PDFILE.PD_SERVPER,
                   PDFILE.PD_SELLPR,
                   PDFILE.PD_DOWNPY,
                   PDFILE.PD_TRADEIN,
                   PDFILE.PD_LNAMT,
                   PDFILE.PD_FINCHG,
                   PDFILE.PD_INTCHG,
                   PDFILE.PD_SERCHG,
                   PDFILE.PD_EXTCHG,
                   PDFILE.PD_MAINTFEE,
                   PDFILE.PD_POINTS,
                   PDFILE.PD_PROCEEDS,
                   PDFILE.PD_ORGTERM,
                   PDFILE.PD_1STPYAMT,
                   PDFILE.PD_NOREGPY,
                   PDFILE.PD_REGPYAMT,
                   PDFILE.PD_LASTPYAMT,
                   PDFILE.PD_PROCASH,
                   PDFILE.PD_TILLNO,
                   ISNULL(PDFILE.PD_STATE_FLAG,''),
                   PDFILE.PD_STATE_WORKER,
                   PDFILE.PD_STATE_WORKER2,
                   ISNULL(PDFILE.PD_FEECODE_1,''),
                   PDFILE.PD_FEEAMT_1,
                   ISNULL(PDFILE.PD_FEEOURS_1,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_1,''),
                   ISNULL(PDFILE.PD_FEECODE_2,''),
                   PDFILE.PD_FEEAMT_2,
                   ISNULL(PDFILE.PD_FEEOURS_2,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_2,''),
                   ISNULL(PDFILE.PD_FEECODE_3,''),
                   PDFILE.PD_FEEAMT_3,
                   ISNULL(PDFILE.PD_FEEOURS_3,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_3,''),
                   ISNULL(PDFILE.PD_FEECODE_4,''),
                   PDFILE.PD_FEEAMT_4,
                   ISNULL(PDFILE.PD_FEEOURS_4,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_4,''),
                   ISNULL(PDFILE.PD_FEECODE_5,''),
                   PDFILE.PD_FEEAMT_5,
                   ISNULL(PDFILE.PD_FEEOURS_5,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_5,''),
                   ISNULL(PDFILE.PD_FEECODE_6,''),
                   PDFILE.PD_FEEAMT_6,
                   ISNULL(PDFILE.PD_FEEOURS_6,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_6,''),
                   ISNULL(PDFILE.PD_FEECODE_7,''),
                   PDFILE.PD_FEEAMT_7,
                   ISNULL(PDFILE.PD_FEEOURS_7,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_7,''),
                   ISNULL(PDFILE.PD_FEECODE_8,''),
                   PDFILE.PD_FEEAMT_8,
                   ISNULL(PDFILE.PD_FEEOURS_8,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_8,''),
                   ISNULL(PDFILE.PD_FEECODE_9,''),
                   PDFILE.PD_FEEAMT_9,
                   ISNULL(PDFILE.PD_FEEOURS_9,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_9,''),
                   ISNULL(PDFILE.PD_FEECODE_10,''),
                   PDFILE.PD_FEEAMT_10,
                   ISNULL(PDFILE.PD_FEEOURS_10,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_10,''),
                   ISNULL(PDFILE.PD_FEECODE_11,''),
                   PDFILE.PD_FEEAMT_11,
                   ISNULL(PDFILE.PD_FEEOURS_11,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_11,''),
                   ISNULL(PDFILE.PD_FEECODE_12,''),
                   PDFILE.PD_FEEAMT_12,
                   ISNULL(PDFILE.PD_FEEOURS_12,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_12,''),
                   ISNULL(PDFILE.PD_FEECODE_13,''),
                   PDFILE.PD_FEEAMT_13,
                   ISNULL(PDFILE.PD_FEEOURS_13,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_13,''),
                   ISNULL(PDFILE.PD_FEECODE_14,''),
                   PDFILE.PD_FEEAMT_14,
                   ISNULL(PDFILE.PD_FEEOURS_14,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_14,''),
                   ISNULL(PDFILE.PD_FEECODE_15,''),
                   PDFILE.PD_FEEAMT_15,
                   ISNULL(PDFILE.PD_FEEOURS_15,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_15,''),
                   ISNULL(PDFILE.PD_FEECODE_16,''),
                   PDFILE.PD_FEEAMT_16,
                   ISNULL(PDFILE.PD_FEEOURS_16,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_16,''),
                   ISNULL(PDFILE.PD_FEECODE_17,''),
                   PDFILE.PD_FEEAMT_17,
                   ISNULL(PDFILE.PD_FEEOURS_17,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_17,''),
                   ISNULL(PDFILE.PD_FEECODE_18,''),
                   PDFILE.PD_FEEAMT_18,
                   ISNULL(PDFILE.PD_FEEOURS_18,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_18,''),
                   ISNULL(PDFILE.PD_FEECODE_19,''),
                   PDFILE.PD_FEEAMT_19,
                   ISNULL(PDFILE.PD_FEEOURS_19,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_19,''),
                   ISNULL(PDFILE.PD_FEECODE_20,''),
                   PDFILE.PD_FEEAMT_20,
                   ISNULL(PDFILE.PD_FEEOURS_20,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_20,''),
                   ISNULL(PDFILE.PD_INSURED,''),
                   ISNULL(PDFILE.PD_INSTYPES,''),
                   ISNULL(PDFILE.PD_INSELIGIBLE,''),
                   PDFILE.PD_INSCOMP_1,
                   ISNULL(PDFILE.PD_INSOURS_1,''),
                   PDFILE.PD_INSEFF_DATE_1,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_1,
                   PDFILE.PD_INSCOVR_1,
                   PDFILE.PD_INSCOMM_1,
                   PDFILE.PD_INSANTICERN_1,
                   PDFILE.PD_INSANTICADJ_1,
                   PDFILE.PD_INSUFERN_1,
                   PDFILE.PD_INSACCERN_1,
                   PDFILE.PD_INSEARN_ORDER_1,
                   ISNULL(PDFILE.PD_INSFINCHG_1,''),
                   PDFILE.PD_INSCOMP_2,
                   ISNULL(PDFILE.PD_INSOURS_2,''),
                   PDFILE.PD_INSEFF_DATE_2,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_2,
                   PDFILE.PD_INSCOVR_2,
                   PDFILE.PD_INSCOMM_2,
                   PDFILE.PD_INSANTICERN_2,
                   PDFILE.PD_INSANTICADJ_2,
                   PDFILE.PD_INSUFERN_2,
                   PDFILE.PD_INSACCERN_2,
                   PDFILE.PD_INSEARN_ORDER_2,
                   ISNULL(PDFILE.PD_INSFINCHG_2,''),
                   PDFILE.PD_INSCOMP_3,
                   ISNULL(PDFILE.PD_INSOURS_3,''),
                   PDFILE.PD_INSEFF_DATE_3,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_3,
                   PDFILE.PD_INSCOVR_3,
                   PDFILE.PD_INSCOMM_3,
                   PDFILE.PD_INSANTICERN_3,
                   PDFILE.PD_INSANTICADJ_3,
                   PDFILE.PD_INSUFERN_3,
                   PDFILE.PD_INSACCERN_3,
                   PDFILE.PD_INSEARN_ORDER_3,
                   ISNULL(PDFILE.PD_INSFINCHG_3,''),
                   PDFILE.PD_INSCOMP_4,
                   ISNULL(PDFILE.PD_INSOURS_4,''),
                   PDFILE.PD_INSEFF_DATE_4,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_4,
                   PDFILE.PD_INSCOVR_4,
                   PDFILE.PD_INSCOMM_4,
                   PDFILE.PD_INSANTICERN_4,
                   PDFILE.PD_INSANTICADJ_4,
                   PDFILE.PD_INSUFERN_4,
                   PDFILE.PD_INSACCERN_4,
                   PDFILE.PD_INSEARN_ORDER_4,
                   ISNULL(PDFILE.PD_INSFINCHG_4,''),
                   PDFILE.PD_INSCOMP_5,
                   ISNULL(PDFILE.PD_INSOURS_5,''),
                   PDFILE.PD_INSEFF_DATE_5,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_5,
                   PDFILE.PD_INSCOVR_5,
                   PDFILE.PD_INSCOMM_5,
                   PDFILE.PD_INSANTICERN_5,
                   PDFILE.PD_INSANTICADJ_5,
                   PDFILE.PD_INSUFERN_5,
                   PDFILE.PD_INSACCERN_5,
                   PDFILE.PD_INSEARN_ORDER_5,
                   ISNULL(PDFILE.PD_INSFINCHG_5,''),
                   PDFILE.PD_INSCOMP_6,
                   ISNULL(PDFILE.PD_INSOURS_6,''),
                   PDFILE.PD_INSEFF_DATE_6,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_6,
                   PDFILE.PD_INSCOVR_6,
                   PDFILE.PD_INSCOMM_6,
                   PDFILE.PD_INSANTICERN_6,
                   PDFILE.PD_INSANTICADJ_6,
                   PDFILE.PD_INSUFERN_6,
                   PDFILE.PD_INSACCERN_6,
                   PDFILE.PD_INSEARN_ORDER_6,
                   ISNULL(PDFILE.PD_INSFINCHG_6,''),
                   PDFILE.PD_INSCOMP_7,
                   ISNULL(PDFILE.PD_INSOURS_7,''),
                   PDFILE.PD_INSEFF_DATE_7,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_7,
                   PDFILE.PD_INSCOVR_7,
                   PDFILE.PD_INSCOMM_7,
                   PDFILE.PD_INSANTICERN_7,
                   PDFILE.PD_INSANTICADJ_7,
                   PDFILE.PD_INSUFERN_7,
                   PDFILE.PD_INSACCERN_7,
                   PDFILE.PD_INSEARN_ORDER_7,
                   ISNULL(PDFILE.PD_INSFINCHG_7,''),
                   PDFILE.PD_INSCOMP_8,
                   ISNULL(PDFILE.PD_INSOURS_8,''),
                   PDFILE.PD_INSEFF_DATE_8,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_8,
                   PDFILE.PD_INSCOVR_8,
                   PDFILE.PD_INSCOMM_8,
                   PDFILE.PD_INSANTICERN_8,
                   PDFILE.PD_INSANTICADJ_8,
                   PDFILE.PD_INSUFERN_8,
                   PDFILE.PD_INSACCERN_8,
                   PDFILE.PD_INSEARN_ORDER_8,
                   ISNULL(PDFILE.PD_INSFINCHG_8,''),
                   PDFILE.PD_INSCOMP_9,
                   ISNULL(PDFILE.PD_INSOURS_9,''),
                   PDFILE.PD_INSEFF_DATE_9,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_9,
                   PDFILE.PD_INSCOVR_9,
                   PDFILE.PD_INSCOMM_9,
                   PDFILE.PD_INSANTICERN_9,
                   PDFILE.PD_INSANTICADJ_9,
                   PDFILE.PD_INSUFERN_9,
                   PDFILE.PD_INSACCERN_9,
                   PDFILE.PD_INSEARN_ORDER_9,
                   ISNULL(PDFILE.PD_INSFINCHG_9,''),
                   PDFILE.PD_INSCOMP_10,
                   ISNULL(PDFILE.PD_INSOURS_10,''),
                   PDFILE.PD_INSEFF_DATE_10,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_10,
                   PDFILE.PD_INSCOVR_10,
                   PDFILE.PD_INSCOMM_10,
                   PDFILE.PD_INSANTICERN_10,
                   PDFILE.PD_INSANTICADJ_10,
                   PDFILE.PD_INSUFERN_10,
                   PDFILE.PD_INSACCERN_10,
                   PDFILE.PD_INSEARN_ORDER_10,
                   ISNULL(PDFILE.PD_INSFINCHG_10,''),
                   PDFILE.PD_PY_YYYYMM_RECDEL,
                   PDFILE.PD_MTD_PAYS_RECDEL,
                   ISNULL(PDFILE.PD_GARNISH_EXTR,''),
                   ISNULL(PDFILE.PD_SHIP_SAME_FG,''),
                   PDFILE.PD_AD_D_INS_COVR,
                   PDFILE.PD_TOT_PARTIAL_CHGOFF,
                   ISNULL(PDFILE.PD_LPO,''),
                   ISNULL(PDFILE.PD_REP_CD,''),
                   PDFILE.PD_EMP_ID,
                   ISNULL(PDFILE.PD_PROCESS_CD,''),
                   PDFILE.PD_WCBC_CREDITLIM,
                   ISNULL(PDFILE.PD_FOLLOWUP_CD,''),
                   ISNULL(PDFILE.PD_MIP_FG,''),
                   PDFILE.PD_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(PDFILE.PD_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_MIP_TOT_CLPAID,
                   ISNULL(CAST(PDFILE.PD_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_MIP_TOT_AHPAID,
                   PDFILE.PD_IL_GROSS_INCOME,
                   PDFILE.PD_IL_GROSS_INCOME2,
                   ISNULL(PDFILE.PD_RESCISSION,''),
                   ISNULL(PDFILE.PD_INTERIM_TRW,''),
                   PDFILE.PD_DLNO,
                   PDFILE.PD_DLPOOL,
                   ISNULL(PDFILE.PD_DLRECOURSE,''),
                   PDFILE.PD_DLPARTRCAMT,
                   PDFILE.PD_ORIGPURAMT,
                   PDFILE.PD_PURBAL,
                   PDFILE.PD_DLRVPRIN_1,
                   PDFILE.PD_DLRVPRIN_2,
                   PDFILE.PD_DLRVPRIN_3,
                   PDFILE.PD_DLRVCHGS_1,
                   PDFILE.PD_DLRVCHGS_2,
                   PDFILE.PD_DLRVCHGS_3,
                   PDFILE.PD_DLPARVHELD,
                   PDFILE.PD_DLPARVPAID,
                   PDFILE.PD_DLDISC,
                   ISNULL(CAST(PDFILE.PD_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_CURBAL,
                   PDFILE.PD_INTBAL,
                   PDFILE.PD_LCBAL,
                   PDFILE.PD_OTHBAL,
                   PDFILE.PD_OT2BAL,
                   PDFILE.PD_LCPAID,
                   PDFILE.PD_LCPARTIALS,
                   PDFILE.PD_STMT_COUNTER,
                   PDFILE.PD_PRIOR_STMT_COUNTER,
                   ISNULL(PDFILE.PD_FROZEN_REASON,''),
                   ISNULL(PDFILE.PD_COUPON_STATEMENT,''),
                   PDFILE.PD_MONTHEND_BAL,
                   PDFILE.PD_ANTICTERM,
                   PDFILE.PD_ANTICERN_1,
                   PDFILE.PD_ANTICADJ_1,
                   PDFILE.PD_UFERN_1,
                   PDFILE.PD_ACCERN_1,
                   PDFILE.PD_EARN_ORDER_1,
                   PDFILE.PD_ANTICERN_2,
                   PDFILE.PD_ANTICADJ_2,
                   PDFILE.PD_UFERN_2,
                   PDFILE.PD_ACCERN_2,
                   PDFILE.PD_EARN_ORDER_2,
                   PDFILE.PD_ANTICERN_3,
                   PDFILE.PD_ANTICADJ_3,
                   PDFILE.PD_UFERN_3,
                   PDFILE.PD_ACCERN_3,
                   PDFILE.PD_EARN_ORDER_3,
                   PDFILE.PD_ANTICERN_4,
                   PDFILE.PD_ANTICADJ_4,
                   PDFILE.PD_UFERN_4,
                   PDFILE.PD_ACCERN_4,
                   PDFILE.PD_EARN_ORDER_4,
                   PDFILE.PD_ANTICERN_5,
                   PDFILE.PD_ANTICADJ_5,
                   PDFILE.PD_UFERN_5,
                   PDFILE.PD_ACCERN_5,
                   PDFILE.PD_EARN_ORDER_5,
                   PDFILE.PD_ANTICERN_6,
                   PDFILE.PD_ANTICADJ_6,
                   PDFILE.PD_UFERN_6,
                   PDFILE.PD_ACCERN_6,
                   PDFILE.PD_EARN_ORDER_6,
                   PDFILE.PD_ANTICERN_7,
                   PDFILE.PD_ANTICADJ_7,
                   PDFILE.PD_UFERN_7,
                   PDFILE.PD_ACCERN_7,
                   PDFILE.PD_EARN_ORDER_7,
                   PDFILE.PD_ANTICERN_8,
                   PDFILE.PD_ANTICADJ_8,
                   PDFILE.PD_UFERN_8,
                   PDFILE.PD_ACCERN_8,
                   PDFILE.PD_EARN_ORDER_8,
                   PDFILE.PD_ANTICERN_9,
                   PDFILE.PD_ANTICADJ_9,
                   PDFILE.PD_UFERN_9,
                   PDFILE.PD_ACCERN_9,
                   PDFILE.PD_EARN_ORDER_9,
                   PDFILE.PD_ANTICERN_10,
                   PDFILE.PD_ANTICADJ_10,
                   PDFILE.PD_UFERN_10,
                   PDFILE.PD_ACCERN_10,
                   PDFILE.PD_EARN_ORDER_10,
                   PDFILE.PD_LMO_PRIOR_INTEREST,
                   PDFILE.PD_PRIOR_UNEARNED,
                   ISNULL(PDFILE.PD_ERNDELQ_STOP,''),
                   PDFILE.PD_NONACRERN,
                   PDFILE.PD_PRIOR_NONACRERN,
                   ISNULL(CAST(PDFILE.PD_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_ERN_IB_PRINBAL,
                   PDFILE.PD_ERN_IB_INTOWE,
                   PDFILE.PD_TOTERN,
                   PDFILE.PD_TOTOTH,
                   PDFILE.PD_TOTINT,
                   PDFILE.PD_TOTLCHG,
                   PDFILE.PD_TOTDEF,
                   PDFILE.PD_TOTSERACCT,
                   PDFILE.PD_LCWAIVERS,
                   PDFILE.PD_INTWAIVERS,
                   PDFILE.PD_TOTPAYMNTD,
                   PDFILE.PD_TOTEXCPAYMNTD,
                   PDFILE.PD_TOTNODEF,
                   PDFILE.PD_YTDNODEF,
                   PDFILE.PD_TOTNODEFACT,
                   PDFILE.PD_YTDNODEFACT,
                   PDFILE.PD_TOTNOALDEL,
                   PDFILE.PD_YTDNOALDEL,
                   PDFILE.PD_YTDINTPD,
                   PDFILE.PD_LYRINTPD,
                   PDFILE.PD_TOTINTPD,
                   PDFILE.PD_RECASTINT,
                   PDFILE.PD_RECASTPRIN,
                   ISNULL(PDFILE.PD_CL_REBATE,''),
                   ISNULL(PDFILE.PD_AH_REBATE,''),
                   ISNULL(PDFILE.PD_PP_REBATE,''),
                   ISNULL(PDFILE.PD_INT_REBATE,''),
                   ISNULL(PDFILE.PD_SC_REBATE,''),
                   ISNULL(PDFILE.PD_MF_REBATE,''),
                   ISNULL(PDFILE.PD_DF_REBATE,''),
                   ISNULL(PDFILE.PD_EX_REBATE,''),
                   ISNULL(PDFILE.PD_O1_REBATE,''),
                   ISNULL(PDFILE.PD_O2_REBATE,''),
                   ISNULL(PDFILE.PD_O3_REBATE,''),
                   ISNULL(PDFILE.PD_O4_REBATE,''),
                   ISNULL(PDFILE.PD_O5_REBATE,''),
                   ISNULL(PDFILE.PD_O6_REBATE,''),
                   ISNULL(PDFILE.PD_O7_REBATE,''),
                   ISNULL(PDFILE.PD_UN_O7_REB,''),
                   ISNULL(PDFILE.PD_UN_O8_REB,''),
                   ISNULL(PDFILE.PD_UN_O9_REB,''),
                   ISNULL(PDFILE.PD_UN_O10_REB,''),
                   ISNULL(PDFILE.PD_PLREASON,''),
                   ISNULL(PDFILE.PD_COLLECTOR,''),
                   ISNULL(PDFILE.PD_ACTIONCD,''),
                   ISNULL(PDFILE.PD_ACTIONCD2,''),
                   ISNULL(PDFILE.PD_ATCODE,''),
                   ISNULL(PDFILE.PD_RECODE,''),
                   ISNULL(PDFILE.PD_ESCROW_FG,''),
                   PDFILE.PD_PROMDAYS_PASTDUE,
                   ISNULL(PDFILE.PD_PROMID,''),
                   ISNULL(CAST(PDFILE.PD_PROMDATE AS VARCHAR(10)),'0'),
                   PDFILE.PD_PROMAMT,
                   ISNULL(CAST(PDFILE.PD_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_AMT_PAID_LAST,
                   PDFILE.PD_LASTCFORMNO,
                   ISNULL(CAST(PDFILE.PD_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_DELCNT,
                   PDFILE.PD_DELCNT2,
                   PDFILE.PD_CREDITLIM,
                   PDFILE.PD_CRSCORE,
                   PDFILE.PD_LASTSFORMNO,
                   ISNULL(CAST(PDFILE.PD_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_SOLCNT,
                   ISNULL(PDFILE.PD_MESSAGE,''),
                   PDFILE.PD_OLDACCTNO,
                   PDFILE.PD_PRLNNO,
                   PDFILE.PD_PRBAL,
                   PDFILE.PD_PRINTDUE,
                   PDFILE.PD_PRFINCHGRB,
                   PDFILE.PD_PRINSRB,
                   ISNULL(PDFILE.PD_PRPOCD,''),
                   ISNULL(CAST(PDFILE.PD_PRPODATE AS VARCHAR(10)),'0'),
                   PDFILE.PD_PRDELINQAMT,
                   PDFILE.PD_PRLNAMT,
                   PDFILE.PD_PRPDUE_DAYS,
                   PDFILE.PD_PRDLDISC_UNEARN,
                   PDFILE.PD_PRCONPROFILE,
                   PDFILE.PD_SUM_ORIGPURAMT,
                   PDFILE.PD_SUM_LNAMT,
                   PDFILE.PD_SUM_FINCHG,
                   PDFILE.PD_SUM_INTCHG,
                   PDFILE.PD_SUM_ORGTERM,
                   PDFILE.PD_SUM_REGPYAMT,
                   PDFILE.PD_SUM_EXTCHG,
                   PDFILE.PD_SUM_SERCHG,
                   PDFILE.PD_SUM_MAINTFEE,
                   ISNULL(PDFILE.PD_UNITPER_CD,''),
                   PDFILE.PD_UNITPER_FREQ,
                   PDFILE.PD_DAYS_IN_UNITPER,
                   PDFILE.PD_UNITPER_PER_YEAR,
                   ISNULL(PDFILE.PD_REPOCD,''),
                   ISNULL(PDFILE.PD_BR_TFR_FG,''),
                   PDFILE.PD_O3INS_DLPROC,
                   PDFILE.PD_INSPREM_DLADJ,
                   PDFILE.PD_PLAMT,
                   PDFILE.PD_MTD_PRIN,
                   PDFILE.PD_MTD_LC,
                   PDFILE.PD_MTD_INT,
                   PDFILE.PD_MTD_OT,
                   PDFILE.PD_MTD_EX,
                   PDFILE.PD_BNKRPT_CHAPTER,
                   ISNULL(PDFILE.PD_BMO_CON_STATUS,''),
                   ISNULL(PDFILE.PD_BMO_REC_STATUS,''),
                   PDFILE.PD_APP_NUMBER,
                   ISNULL(PDFILE.PD_GIFTNET,''),
                   PDFILE.PD_GIFTNET_OUR_PO,
                   ISNULL(PDFILE.PD_PROC_DISBURSED,''),
                   PDFILE.PD_NOODDPY,
                   PDFILE.PD_CONPROFILE_1,
                   PDFILE.PD_CONPROFILE_2,
                   PDFILE.PD_CONPROFILE_3,
                   PDFILE.PD_CONPROFILE_4,
                   PDFILE.PD_CONPROFILE_5,
                   PDFILE.PD_RECPROFILE_1,
                   PDFILE.PD_RECPROFILE_2,
                   PDFILE.PD_RECPROFILE_3,
                   PDFILE.PD_RECPROFILE_4,
                   PDFILE.PD_RECPROFILE_5,
                   ISNULL(PDFILE.PD_TRW_STATUS,''),
                   ISNULL(PDFILE.PD_ORIGIN,''),
                   ISNULL(PDFILE.PD_PO_TRCD,''),
                   PDFILE.PD_NEXIS_CRSCORE,
                   ISNULL(PDFILE.PD_TRW_B_CONSUMER_IND,''),
                   ISNULL(PDFILE.PD_PARVHELD_PAID_FG,''),
                   ISNULL(PDFILE.PD_ACCT_FROZEN,''),
                   ISNULL(PDFILE.PD_TRW_SPECIAL_CODE,''),
                   ISNULL(PDFILE.PD_CREATION_STATUS,''),
                   PDFILE.PD_TOTNOLCHG,
                   ISNULL(PDFILE.PD_LBOX,''),
                   PDFILE.PD_EXTRA_PRIN_LFP,
                   PDFILE.PD_REMAIN_INT_SINCE_LFP,
                   PDFILE.PD_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(PDFILE.PD_INTERIM_COUPON,''),
                   PDFILE.PD_DLCOMM_O3_CMSI,
                   PDFILE.PD_DLCOMM_PP_CMSI,
                   ISNULL(PDFILE.PD_TRW_J_CONSUMER_IND,''),
                   ISNULL(PDFILE.PD_ACH,''),
                   ISNULL(PDFILE.PD_1099_C_FG,''),
                   ISNULL(PDFILE.PD_TRW_COMPLIANCE_CODE,''),
                   ISNULL(PDFILE.PD_UNWIND_REASON_CODE,''),
                   ISNULL(PDFILE.PD_PBI_PBD_FG,''),
                   ISNULL(PDFILE.PD_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(PDFILE.PD_APPROVED_BY_USER,''),
                   ISNULL(PDFILE.PD_APPROVED_BY_CODE,''),
                   ISNULL(CAST(PDFILE.PD_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_APPROVED_BY_TIME,
                   ISNULL(CAST(PDFILE.PD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_LTOUCH_TIME,
                   ISNULL(PDFILE.PD_HOLD_COLLATERAL,''),
                   ISNULL(PDFILE.PD_DLPAID,''),
                   PDFILE.PD_CKNO,
                   ISNULL(PDFILE.PD_FREE1,''),
                   ISNULL(PDFILE.PD_FREE2,''),
                   ISNULL(PDFILE.PD_FREE_SPACE_FILLER,''),
                   PDFILE.PD_POSTING_BR,
                   ISNULL(PDFILE.PD_OTHBR_FG,''),
                   PDFILE.PD_APPROVAL_LIMIT,
                   ISNULL(PDFILE.PD_APPROVAL_USERID,''),
                   PDFILE.PD_MIN_CRED_SCORE,
                   ISNULL(PDFILE.PD_CRED_SCORE_USERID,''),
                   PDFILE.PD_FINAMT,
                   ISNULL(PDFILE.PD_PROCEEDS_FG,''),
                   ISNULL(PDFILE.PD_POINTS_FG,''),
                   ISNULL(PDFILE.PD_OVRDRATE_FG,''),
                   ISNULL(PDFILE.PD_INS_FG_1,''),
                   ISNULL(PDFILE.PD_INS_FG_2,''),
                   ISNULL(PDFILE.PD_INS_FG_3,''),
                   ISNULL(PDFILE.PD_INS_FG_4,''),
                   ISNULL(PDFILE.PD_INS_FG_5,''),
                   ISNULL(PDFILE.PD_INS_FG_6,''),
                   ISNULL(PDFILE.PD_INS_FG_7,''),
                   ISNULL(PDFILE.PD_INS_FG_8,''),
                   ISNULL(PDFILE.PD_INS_FG_9,''),
                   ISNULL(PDFILE.PD_INS_FG_10,''),
                   ISNULL(PDFILE.PD_SERCHG_FG,''),
                   ISNULL(PDFILE.PD_MAINTFEE_FG,''),
                   ISNULL(PDFILE.PD_EXTCHG_FG,''),
                   ISNULL(PDFILE.PD_REGPYAMT_FG,''),
                   ISNULL(PDFILE.PD_FEECD_1,''),
                   ISNULL(PDFILE.PD_FEECD_2,''),
                   ISNULL(PDFILE.PD_FEECD_3,''),
                   ISNULL(PDFILE.PD_FEECD_4,''),
                   ISNULL(PDFILE.PD_FEECD_5,''),
                   ISNULL(PDFILE.PD_FEECD_6,''),
                   ISNULL(PDFILE.PD_FEECD_7,''),
                   ISNULL(PDFILE.PD_FEECD_8,''),
                   ISNULL(PDFILE.PD_FEECD_9,''),
                   ISNULL(PDFILE.PD_FEECD_10,''),
                   ISNULL(PDFILE.PD_FEECD_11,''),
                   ISNULL(PDFILE.PD_FEECD_12,''),
                   ISNULL(PDFILE.PD_FEECD_13,''),
                   ISNULL(PDFILE.PD_FEECD_14,''),
                   ISNULL(PDFILE.PD_FEECD_15,''),
                   ISNULL(PDFILE.PD_FEECD_16,''),
                   ISNULL(PDFILE.PD_FEECD_17,''),
                   ISNULL(PDFILE.PD_FEECD_18,''),
                   ISNULL(PDFILE.PD_FEECD_19,''),
                   ISNULL(PDFILE.PD_FEECD_20,''),
                   PDFILE.PD_PRPAID_AHEAD,
                   PDFILE.PD_RESTRICT_ADJ,
                   PDFILE.PD_DUAL_LOAN_ADJ,
                   PDFILE.PD_CAN_LOAN_MAX,
                   PDFILE.PD_PERDOWN,
                   ISNULL(PDFILE.PD_SELLPR_FG,''),
                   ISNULL(PDFILE.PD_DOWNPY_FG,''),
                   ISNULL(PDFILE.PD_PERDOWN_FG,''),
                   ISNULL(PDFILE.PD_SELL_DOWN_FG,''),
                   ISNULL(PDFILE.PD_BALLOON_FG,''),
                   ISNULL(PDFILE.PD_BALLOON_LAST_FG,''),
                   ISNULL(PDFILE.PD_FEECD_FG_1,''),
                   ISNULL(PDFILE.PD_FEECD_FG_2,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_1,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_1,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_1,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_2,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_2,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_2,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_3,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_3,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_3,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_4,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_4,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_4,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_5,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_5,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_5,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_6,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_6,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_6,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_7,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_7,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_7,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_8,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_8,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_8,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_9,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_9,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_9,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_10,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_10,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_10,''),
                   ISNULL(PDFILE.PD_DLDISC_FG,''),
                   ISNULL(PDFILE.PD_DLRVPRIN_FG_1,''),
                   ISNULL(PDFILE.PD_DLRVPRIN_FG_2,''),
                   ISNULL(PDFILE.PD_DLRVPRIN_FG_3,''),
                   ISNULL(PDFILE.PD_DLRVCHGS_FG_1,''),
                   ISNULL(PDFILE.PD_DLRVCHGS_FG_2,''),
                   ISNULL(PDFILE.PD_DLRVCHGS_FG_3,''),
                   ISNULL(PDFILE.PD_DLPARVHELD_FG,''),
                   ISNULL(PDFILE.PD_DLPARVPAID_FG,''),
                   ISNULL(PDFILE.PD_ONE_MTH_NO_DAY_EXT_SW,''),
                   ISNULL(PDFILE.PD_ASSIGN_RN_ACCTNO,''),
                   PDFILE.PD_APR_FINAMT,
                   ISNULL(PDFILE.PD_DLCOMM_PP_CMSI_FG,''),
                   ISNULL(PDFILE.PD_DLCOMM_O3_CMSI_FG,''),
                   ISNULL(PDFILE.PD_FILLER_TO_MATCH_LN,''),
                   ISNULL(PDFILE.PD2_ID,''),
                   PDFILE.PD2_BRANCH,
                   ISNULL(PDFILE.PD2_BWNAME,'')
            FROM DBO.PDFILE
            WHERE PDFILE.PD2_ID >= :QPD2-WBEG-ID
              AND PDFILE.PD2_ID <= :QPD2-WEND-ID
              AND PDFILE.PD2_BRANCH >= :QPD2-WBEG-BRANCH
              AND PDFILE.PD2_BRANCH <= :QPD2-WEND-BRANCH
              AND PDFILE.PD2_BWNAME >= :QPD2-WBEG-BWNAME
              AND PDFILE.PD2_BWNAME <= :QPD2-WEND-BWNAME
            ORDER BY PDFILE.PD2_ID,
                     PDFILE.PD2_BRANCH,
                     PDFILE.PD2_BWNAME
           END-EXEC.

           EXEC SQL
               OPEN PD2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QPD2-WBEG-BRANCH
                          QPD2-WEND-BRANCH.

           MOVE SPACES TO QPD2-WBEG-ID
                          QPD2-WEND-ID
                          QPD2-WBEG-BWNAME
                          QPD2-WEND-BWNAME.

           MOVE STAT---GOOD TO PD2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-PD2-FILE.
           PERFORM READ-IT.
           MOVE PD-PATH-OWNBR  TO PD-BRANCH. 
           MOVE PD-SELECT-PATH TO E-FILE.
           MOVE PD2-KEY        TO E-KEYX.

           MOVE PD2-ID     TO QPD2-ID.
           MOVE PD2-BRANCH TO QPD2-BRANCH.
           MOVE PD2-BWNAME TO QPD2-BWNAME.

           EXEC SQL
            SELECT ISNULL(PDFILE.PD_ID,''),
                   PDFILE.PD_BRANCH,
                   PDFILE.PD_SOC_SEC,
                   PDFILE.PD_OWNBR,
                   PDFILE.PD_ACCTNO,
                   PDFILE.PD_CLASS,
                   PDFILE.PD_SUBCLASS,
                   PDFILE.PD_LAWCODE,
                   PDFILE.PD_SSNO1,
                   ISNULL(PDFILE.PD_MAKERCD1,''),
                   PDFILE.PD_SSNO2,
                   ISNULL(PDFILE.PD_MAKERCD2,''),
                   PDFILE.PD_SSNO3,
                   ISNULL(PDFILE.PD_MAKERCD3,''),
                   PDFILE.PD_SSNO4,
                   ISNULL(PDFILE.PD_MAKERCD4,''),
                   ISNULL(PDFILE.PD_ORGST,''),
                   PDFILE.PD_ORGBR,
                   ISNULL(PDFILE.PD_OWNST,''),
                   PDFILE.PD_SPRCLASS,
                   ISNULL(PDFILE.PD_CONVERCD,''),
                   ISNULL(PDFILE.PD_MIDSTREAM,''),
                   ISNULL(PDFILE.PD_PURCD,''),
                   ISNULL(PDFILE.PD_SRCD,''),
                   ISNULL(PDFILE.PD_COLLCD,''),
                   ISNULL(PDFILE.PD_COLLID,''),
                   ISNULL(PDFILE.PD_POCD,''),
                   ISNULL(PDFILE.PD_PLCD,''),
                   ISNULL(PDFILE.PD_PLTYPE,''),
                   ISNULL(PDFILE.PD_CONFIDENTCD,''),
                   ISNULL(PDFILE.PD_ALLOTCD,''),
                   ISNULL(PDFILE.PD_RECASTCD,''),
                   ISNULL(PDFILE.PD_SKIP_TRW,''),
                   ISNULL(PDFILE.PD_STATUSFG,''),
                   ISNULL(PDFILE.PD_SMALLOAN,''),
                   ISNULL(PDFILE.PD_SALEFIN,''),
                   ISNULL(PDFILE.PD_SERVACCT,''),
                   ISNULL(PDFILE.PD_LOANTYPE,''),
                   ISNULL(PDFILE.PD_ADDON_FG,''),
                   ISNULL(PDFILE.PD_PAY_SCHLD_FG,''),
                   PDFILE.PD_LC_CODE,
                   ISNULL(PDFILE.PD_CAN_LOAN_NO,''),
                   ISNULL(PDFILE.PD_STATFLAGS,''),
                   ISNULL(CAST(PDFILE.PD_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PURDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_CASHDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_FILEDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_JDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PLDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_REDUDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_REPODATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_SOLDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_APRATE,
                   PDFILE.PD_EFFRATE,
                   PDFILE.PD_SMPRATE,
                   PDFILE.PD_YLDRATE,
                   PDFILE.PD_OVRDRATE,
                   PDFILE.PD_ENDORATE,
                   PDFILE.PD_JDRATE,
                   PDFILE.PD_ACCRATE,
                   PDFILE.PD_REDURATE,
                   PDFILE.PD_CONRATE,
                   PDFILE.PD_SERVPER,
                   PDFILE.PD_SELLPR,
                   PDFILE.PD_DOWNPY,
                   PDFILE.PD_TRADEIN,
                   PDFILE.PD_LNAMT,
                   PDFILE.PD_FINCHG,
                   PDFILE.PD_INTCHG,
                   PDFILE.PD_SERCHG,
                   PDFILE.PD_EXTCHG,
                   PDFILE.PD_MAINTFEE,
                   PDFILE.PD_POINTS,
                   PDFILE.PD_PROCEEDS,
                   PDFILE.PD_ORGTERM,
                   PDFILE.PD_1STPYAMT,
                   PDFILE.PD_NOREGPY,
                   PDFILE.PD_REGPYAMT,
                   PDFILE.PD_LASTPYAMT,
                   PDFILE.PD_PROCASH,
                   PDFILE.PD_TILLNO,
                   ISNULL(PDFILE.PD_STATE_FLAG,''),
                   PDFILE.PD_STATE_WORKER,
                   PDFILE.PD_STATE_WORKER2,
                   ISNULL(PDFILE.PD_FEECODE_1,''),
                   PDFILE.PD_FEEAMT_1,
                   ISNULL(PDFILE.PD_FEEOURS_1,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_1,''),
                   ISNULL(PDFILE.PD_FEECODE_2,''),
                   PDFILE.PD_FEEAMT_2,
                   ISNULL(PDFILE.PD_FEEOURS_2,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_2,''),
                   ISNULL(PDFILE.PD_FEECODE_3,''),
                   PDFILE.PD_FEEAMT_3,
                   ISNULL(PDFILE.PD_FEEOURS_3,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_3,''),
                   ISNULL(PDFILE.PD_FEECODE_4,''),
                   PDFILE.PD_FEEAMT_4,
                   ISNULL(PDFILE.PD_FEEOURS_4,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_4,''),
                   ISNULL(PDFILE.PD_FEECODE_5,''),
                   PDFILE.PD_FEEAMT_5,
                   ISNULL(PDFILE.PD_FEEOURS_5,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_5,''),
                   ISNULL(PDFILE.PD_FEECODE_6,''),
                   PDFILE.PD_FEEAMT_6,
                   ISNULL(PDFILE.PD_FEEOURS_6,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_6,''),
                   ISNULL(PDFILE.PD_FEECODE_7,''),
                   PDFILE.PD_FEEAMT_7,
                   ISNULL(PDFILE.PD_FEEOURS_7,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_7,''),
                   ISNULL(PDFILE.PD_FEECODE_8,''),
                   PDFILE.PD_FEEAMT_8,
                   ISNULL(PDFILE.PD_FEEOURS_8,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_8,''),
                   ISNULL(PDFILE.PD_FEECODE_9,''),
                   PDFILE.PD_FEEAMT_9,
                   ISNULL(PDFILE.PD_FEEOURS_9,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_9,''),
                   ISNULL(PDFILE.PD_FEECODE_10,''),
                   PDFILE.PD_FEEAMT_10,
                   ISNULL(PDFILE.PD_FEEOURS_10,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_10,''),
                   ISNULL(PDFILE.PD_FEECODE_11,''),
                   PDFILE.PD_FEEAMT_11,
                   ISNULL(PDFILE.PD_FEEOURS_11,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_11,''),
                   ISNULL(PDFILE.PD_FEECODE_12,''),
                   PDFILE.PD_FEEAMT_12,
                   ISNULL(PDFILE.PD_FEEOURS_12,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_12,''),
                   ISNULL(PDFILE.PD_FEECODE_13,''),
                   PDFILE.PD_FEEAMT_13,
                   ISNULL(PDFILE.PD_FEEOURS_13,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_13,''),
                   ISNULL(PDFILE.PD_FEECODE_14,''),
                   PDFILE.PD_FEEAMT_14,
                   ISNULL(PDFILE.PD_FEEOURS_14,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_14,''),
                   ISNULL(PDFILE.PD_FEECODE_15,''),
                   PDFILE.PD_FEEAMT_15,
                   ISNULL(PDFILE.PD_FEEOURS_15,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_15,''),
                   ISNULL(PDFILE.PD_FEECODE_16,''),
                   PDFILE.PD_FEEAMT_16,
                   ISNULL(PDFILE.PD_FEEOURS_16,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_16,''),
                   ISNULL(PDFILE.PD_FEECODE_17,''),
                   PDFILE.PD_FEEAMT_17,
                   ISNULL(PDFILE.PD_FEEOURS_17,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_17,''),
                   ISNULL(PDFILE.PD_FEECODE_18,''),
                   PDFILE.PD_FEEAMT_18,
                   ISNULL(PDFILE.PD_FEEOURS_18,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_18,''),
                   ISNULL(PDFILE.PD_FEECODE_19,''),
                   PDFILE.PD_FEEAMT_19,
                   ISNULL(PDFILE.PD_FEEOURS_19,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_19,''),
                   ISNULL(PDFILE.PD_FEECODE_20,''),
                   PDFILE.PD_FEEAMT_20,
                   ISNULL(PDFILE.PD_FEEOURS_20,''),
                   ISNULL(PDFILE.PD_FEEFINCHG_20,''),
                   ISNULL(PDFILE.PD_INSURED,''),
                   ISNULL(PDFILE.PD_INSTYPES,''),
                   ISNULL(PDFILE.PD_INSELIGIBLE,''),
                   PDFILE.PD_INSCOMP_1,
                   ISNULL(PDFILE.PD_INSOURS_1,''),
                   PDFILE.PD_INSEFF_DATE_1,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_1,
                   PDFILE.PD_INSCOVR_1,
                   PDFILE.PD_INSCOMM_1,
                   PDFILE.PD_INSANTICERN_1,
                   PDFILE.PD_INSANTICADJ_1,
                   PDFILE.PD_INSUFERN_1,
                   PDFILE.PD_INSACCERN_1,
                   PDFILE.PD_INSEARN_ORDER_1,
                   ISNULL(PDFILE.PD_INSFINCHG_1,''),
                   PDFILE.PD_INSCOMP_2,
                   ISNULL(PDFILE.PD_INSOURS_2,''),
                   PDFILE.PD_INSEFF_DATE_2,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_2,
                   PDFILE.PD_INSCOVR_2,
                   PDFILE.PD_INSCOMM_2,
                   PDFILE.PD_INSANTICERN_2,
                   PDFILE.PD_INSANTICADJ_2,
                   PDFILE.PD_INSUFERN_2,
                   PDFILE.PD_INSACCERN_2,
                   PDFILE.PD_INSEARN_ORDER_2,
                   ISNULL(PDFILE.PD_INSFINCHG_2,''),
                   PDFILE.PD_INSCOMP_3,
                   ISNULL(PDFILE.PD_INSOURS_3,''),
                   PDFILE.PD_INSEFF_DATE_3,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_3,
                   PDFILE.PD_INSCOVR_3,
                   PDFILE.PD_INSCOMM_3,
                   PDFILE.PD_INSANTICERN_3,
                   PDFILE.PD_INSANTICADJ_3,
                   PDFILE.PD_INSUFERN_3,
                   PDFILE.PD_INSACCERN_3,
                   PDFILE.PD_INSEARN_ORDER_3,
                   ISNULL(PDFILE.PD_INSFINCHG_3,''),
                   PDFILE.PD_INSCOMP_4,
                   ISNULL(PDFILE.PD_INSOURS_4,''),
                   PDFILE.PD_INSEFF_DATE_4,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_4,
                   PDFILE.PD_INSCOVR_4,
                   PDFILE.PD_INSCOMM_4,
                   PDFILE.PD_INSANTICERN_4,
                   PDFILE.PD_INSANTICADJ_4,
                   PDFILE.PD_INSUFERN_4,
                   PDFILE.PD_INSACCERN_4,
                   PDFILE.PD_INSEARN_ORDER_4,
                   ISNULL(PDFILE.PD_INSFINCHG_4,''),
                   PDFILE.PD_INSCOMP_5,
                   ISNULL(PDFILE.PD_INSOURS_5,''),
                   PDFILE.PD_INSEFF_DATE_5,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_5,
                   PDFILE.PD_INSCOVR_5,
                   PDFILE.PD_INSCOMM_5,
                   PDFILE.PD_INSANTICERN_5,
                   PDFILE.PD_INSANTICADJ_5,
                   PDFILE.PD_INSUFERN_5,
                   PDFILE.PD_INSACCERN_5,
                   PDFILE.PD_INSEARN_ORDER_5,
                   ISNULL(PDFILE.PD_INSFINCHG_5,''),
                   PDFILE.PD_INSCOMP_6,
                   ISNULL(PDFILE.PD_INSOURS_6,''),
                   PDFILE.PD_INSEFF_DATE_6,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_6,
                   PDFILE.PD_INSCOVR_6,
                   PDFILE.PD_INSCOMM_6,
                   PDFILE.PD_INSANTICERN_6,
                   PDFILE.PD_INSANTICADJ_6,
                   PDFILE.PD_INSUFERN_6,
                   PDFILE.PD_INSACCERN_6,
                   PDFILE.PD_INSEARN_ORDER_6,
                   ISNULL(PDFILE.PD_INSFINCHG_6,''),
                   PDFILE.PD_INSCOMP_7,
                   ISNULL(PDFILE.PD_INSOURS_7,''),
                   PDFILE.PD_INSEFF_DATE_7,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_7,
                   PDFILE.PD_INSCOVR_7,
                   PDFILE.PD_INSCOMM_7,
                   PDFILE.PD_INSANTICERN_7,
                   PDFILE.PD_INSANTICADJ_7,
                   PDFILE.PD_INSUFERN_7,
                   PDFILE.PD_INSACCERN_7,
                   PDFILE.PD_INSEARN_ORDER_7,
                   ISNULL(PDFILE.PD_INSFINCHG_7,''),
                   PDFILE.PD_INSCOMP_8,
                   ISNULL(PDFILE.PD_INSOURS_8,''),
                   PDFILE.PD_INSEFF_DATE_8,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_8,
                   PDFILE.PD_INSCOVR_8,
                   PDFILE.PD_INSCOMM_8,
                   PDFILE.PD_INSANTICERN_8,
                   PDFILE.PD_INSANTICADJ_8,
                   PDFILE.PD_INSUFERN_8,
                   PDFILE.PD_INSACCERN_8,
                   PDFILE.PD_INSEARN_ORDER_8,
                   ISNULL(PDFILE.PD_INSFINCHG_8,''),
                   PDFILE.PD_INSCOMP_9,
                   ISNULL(PDFILE.PD_INSOURS_9,''),
                   PDFILE.PD_INSEFF_DATE_9,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_9,
                   PDFILE.PD_INSCOVR_9,
                   PDFILE.PD_INSCOMM_9,
                   PDFILE.PD_INSANTICERN_9,
                   PDFILE.PD_INSANTICADJ_9,
                   PDFILE.PD_INSUFERN_9,
                   PDFILE.PD_INSACCERN_9,
                   PDFILE.PD_INSEARN_ORDER_9,
                   ISNULL(PDFILE.PD_INSFINCHG_9,''),
                   PDFILE.PD_INSCOMP_10,
                   ISNULL(PDFILE.PD_INSOURS_10,''),
                   PDFILE.PD_INSEFF_DATE_10,
                   ISNULL(CAST(PDFILE.PD_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_INSPREM_10,
                   PDFILE.PD_INSCOVR_10,
                   PDFILE.PD_INSCOMM_10,
                   PDFILE.PD_INSANTICERN_10,
                   PDFILE.PD_INSANTICADJ_10,
                   PDFILE.PD_INSUFERN_10,
                   PDFILE.PD_INSACCERN_10,
                   PDFILE.PD_INSEARN_ORDER_10,
                   ISNULL(PDFILE.PD_INSFINCHG_10,''),
                   PDFILE.PD_PY_YYYYMM_RECDEL,
                   PDFILE.PD_MTD_PAYS_RECDEL,
                   ISNULL(PDFILE.PD_GARNISH_EXTR,''),
                   ISNULL(PDFILE.PD_SHIP_SAME_FG,''),
                   PDFILE.PD_AD_D_INS_COVR,
                   PDFILE.PD_TOT_PARTIAL_CHGOFF,
                   ISNULL(PDFILE.PD_LPO,''),
                   ISNULL(PDFILE.PD_REP_CD,''),
                   PDFILE.PD_EMP_ID,
                   ISNULL(PDFILE.PD_PROCESS_CD,''),
                   PDFILE.PD_WCBC_CREDITLIM,
                   ISNULL(PDFILE.PD_FOLLOWUP_CD,''),
                   ISNULL(PDFILE.PD_MIP_FG,''),
                   PDFILE.PD_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(PDFILE.PD_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_MIP_TOT_CLPAID,
                   ISNULL(CAST(PDFILE.PD_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_MIP_TOT_AHPAID,
                   PDFILE.PD_IL_GROSS_INCOME,
                   PDFILE.PD_IL_GROSS_INCOME2,
                   ISNULL(PDFILE.PD_RESCISSION,''),
                   ISNULL(PDFILE.PD_INTERIM_TRW,''),
                   PDFILE.PD_DLNO,
                   PDFILE.PD_DLPOOL,
                   ISNULL(PDFILE.PD_DLRECOURSE,''),
                   PDFILE.PD_DLPARTRCAMT,
                   PDFILE.PD_ORIGPURAMT,
                   PDFILE.PD_PURBAL,
                   PDFILE.PD_DLRVPRIN_1,
                   PDFILE.PD_DLRVPRIN_2,
                   PDFILE.PD_DLRVPRIN_3,
                   PDFILE.PD_DLRVCHGS_1,
                   PDFILE.PD_DLRVCHGS_2,
                   PDFILE.PD_DLRVCHGS_3,
                   PDFILE.PD_DLPARVHELD,
                   PDFILE.PD_DLPARVPAID,
                   PDFILE.PD_DLDISC,
                   ISNULL(CAST(PDFILE.PD_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_CURBAL,
                   PDFILE.PD_INTBAL,
                   PDFILE.PD_LCBAL,
                   PDFILE.PD_OTHBAL,
                   PDFILE.PD_OT2BAL,
                   PDFILE.PD_LCPAID,
                   PDFILE.PD_LCPARTIALS,
                   PDFILE.PD_STMT_COUNTER,
                   PDFILE.PD_PRIOR_STMT_COUNTER,
                   ISNULL(PDFILE.PD_FROZEN_REASON,''),
                   ISNULL(PDFILE.PD_COUPON_STATEMENT,''),
                   PDFILE.PD_MONTHEND_BAL,
                   PDFILE.PD_ANTICTERM,
                   PDFILE.PD_ANTICERN_1,
                   PDFILE.PD_ANTICADJ_1,
                   PDFILE.PD_UFERN_1,
                   PDFILE.PD_ACCERN_1,
                   PDFILE.PD_EARN_ORDER_1,
                   PDFILE.PD_ANTICERN_2,
                   PDFILE.PD_ANTICADJ_2,
                   PDFILE.PD_UFERN_2,
                   PDFILE.PD_ACCERN_2,
                   PDFILE.PD_EARN_ORDER_2,
                   PDFILE.PD_ANTICERN_3,
                   PDFILE.PD_ANTICADJ_3,
                   PDFILE.PD_UFERN_3,
                   PDFILE.PD_ACCERN_3,
                   PDFILE.PD_EARN_ORDER_3,
                   PDFILE.PD_ANTICERN_4,
                   PDFILE.PD_ANTICADJ_4,
                   PDFILE.PD_UFERN_4,
                   PDFILE.PD_ACCERN_4,
                   PDFILE.PD_EARN_ORDER_4,
                   PDFILE.PD_ANTICERN_5,
                   PDFILE.PD_ANTICADJ_5,
                   PDFILE.PD_UFERN_5,
                   PDFILE.PD_ACCERN_5,
                   PDFILE.PD_EARN_ORDER_5,
                   PDFILE.PD_ANTICERN_6,
                   PDFILE.PD_ANTICADJ_6,
                   PDFILE.PD_UFERN_6,
                   PDFILE.PD_ACCERN_6,
                   PDFILE.PD_EARN_ORDER_6,
                   PDFILE.PD_ANTICERN_7,
                   PDFILE.PD_ANTICADJ_7,
                   PDFILE.PD_UFERN_7,
                   PDFILE.PD_ACCERN_7,
                   PDFILE.PD_EARN_ORDER_7,
                   PDFILE.PD_ANTICERN_8,
                   PDFILE.PD_ANTICADJ_8,
                   PDFILE.PD_UFERN_8,
                   PDFILE.PD_ACCERN_8,
                   PDFILE.PD_EARN_ORDER_8,
                   PDFILE.PD_ANTICERN_9,
                   PDFILE.PD_ANTICADJ_9,
                   PDFILE.PD_UFERN_9,
                   PDFILE.PD_ACCERN_9,
                   PDFILE.PD_EARN_ORDER_9,
                   PDFILE.PD_ANTICERN_10,
                   PDFILE.PD_ANTICADJ_10,
                   PDFILE.PD_UFERN_10,
                   PDFILE.PD_ACCERN_10,
                   PDFILE.PD_EARN_ORDER_10,
                   PDFILE.PD_LMO_PRIOR_INTEREST,
                   PDFILE.PD_PRIOR_UNEARNED,
                   ISNULL(PDFILE.PD_ERNDELQ_STOP,''),
                   PDFILE.PD_NONACRERN,
                   PDFILE.PD_PRIOR_NONACRERN,
                   ISNULL(CAST(PDFILE.PD_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_ERN_IB_PRINBAL,
                   PDFILE.PD_ERN_IB_INTOWE,
                   PDFILE.PD_TOTERN,
                   PDFILE.PD_TOTOTH,
                   PDFILE.PD_TOTINT,
                   PDFILE.PD_TOTLCHG,
                   PDFILE.PD_TOTDEF,
                   PDFILE.PD_TOTSERACCT,
                   PDFILE.PD_LCWAIVERS,
                   PDFILE.PD_INTWAIVERS,
                   PDFILE.PD_TOTPAYMNTD,
                   PDFILE.PD_TOTEXCPAYMNTD,
                   PDFILE.PD_TOTNODEF,
                   PDFILE.PD_YTDNODEF,
                   PDFILE.PD_TOTNODEFACT,
                   PDFILE.PD_YTDNODEFACT,
                   PDFILE.PD_TOTNOALDEL,
                   PDFILE.PD_YTDNOALDEL,
                   PDFILE.PD_YTDINTPD,
                   PDFILE.PD_LYRINTPD,
                   PDFILE.PD_TOTINTPD,
                   PDFILE.PD_RECASTINT,
                   PDFILE.PD_RECASTPRIN,
                   ISNULL(PDFILE.PD_CL_REBATE,''),
                   ISNULL(PDFILE.PD_AH_REBATE,''),
                   ISNULL(PDFILE.PD_PP_REBATE,''),
                   ISNULL(PDFILE.PD_INT_REBATE,''),
                   ISNULL(PDFILE.PD_SC_REBATE,''),
                   ISNULL(PDFILE.PD_MF_REBATE,''),
                   ISNULL(PDFILE.PD_DF_REBATE,''),
                   ISNULL(PDFILE.PD_EX_REBATE,''),
                   ISNULL(PDFILE.PD_O1_REBATE,''),
                   ISNULL(PDFILE.PD_O2_REBATE,''),
                   ISNULL(PDFILE.PD_O3_REBATE,''),
                   ISNULL(PDFILE.PD_O4_REBATE,''),
                   ISNULL(PDFILE.PD_O5_REBATE,''),
                   ISNULL(PDFILE.PD_O6_REBATE,''),
                   ISNULL(PDFILE.PD_O7_REBATE,''),
                   ISNULL(PDFILE.PD_UN_O7_REB,''),
                   ISNULL(PDFILE.PD_UN_O8_REB,''),
                   ISNULL(PDFILE.PD_UN_O9_REB,''),
                   ISNULL(PDFILE.PD_UN_O10_REB,''),
                   ISNULL(PDFILE.PD_PLREASON,''),
                   ISNULL(PDFILE.PD_COLLECTOR,''),
                   ISNULL(PDFILE.PD_ACTIONCD,''),
                   ISNULL(PDFILE.PD_ACTIONCD2,''),
                   ISNULL(PDFILE.PD_ATCODE,''),
                   ISNULL(PDFILE.PD_RECODE,''),
                   ISNULL(PDFILE.PD_ESCROW_FG,''),
                   PDFILE.PD_PROMDAYS_PASTDUE,
                   ISNULL(PDFILE.PD_PROMID,''),
                   ISNULL(CAST(PDFILE.PD_PROMDATE AS VARCHAR(10)),'0'),
                   PDFILE.PD_PROMAMT,
                   ISNULL(CAST(PDFILE.PD_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PDFILE.PD_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_AMT_PAID_LAST,
                   PDFILE.PD_LASTCFORMNO,
                   ISNULL(CAST(PDFILE.PD_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_DELCNT,
                   PDFILE.PD_DELCNT2,
                   PDFILE.PD_CREDITLIM,
                   PDFILE.PD_CRSCORE,
                   PDFILE.PD_LASTSFORMNO,
                   ISNULL(CAST(PDFILE.PD_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_SOLCNT,
                   ISNULL(PDFILE.PD_MESSAGE,''),
                   PDFILE.PD_OLDACCTNO,
                   PDFILE.PD_PRLNNO,
                   PDFILE.PD_PRBAL,
                   PDFILE.PD_PRINTDUE,
                   PDFILE.PD_PRFINCHGRB,
                   PDFILE.PD_PRINSRB,
                   ISNULL(PDFILE.PD_PRPOCD,''),
                   ISNULL(CAST(PDFILE.PD_PRPODATE AS VARCHAR(10)),'0'),
                   PDFILE.PD_PRDELINQAMT,
                   PDFILE.PD_PRLNAMT,
                   PDFILE.PD_PRPDUE_DAYS,
                   PDFILE.PD_PRDLDISC_UNEARN,
                   PDFILE.PD_PRCONPROFILE,
                   PDFILE.PD_SUM_ORIGPURAMT,
                   PDFILE.PD_SUM_LNAMT,
                   PDFILE.PD_SUM_FINCHG,
                   PDFILE.PD_SUM_INTCHG,
                   PDFILE.PD_SUM_ORGTERM,
                   PDFILE.PD_SUM_REGPYAMT,
                   PDFILE.PD_SUM_EXTCHG,
                   PDFILE.PD_SUM_SERCHG,
                   PDFILE.PD_SUM_MAINTFEE,
                   ISNULL(PDFILE.PD_UNITPER_CD,''),
                   PDFILE.PD_UNITPER_FREQ,
                   PDFILE.PD_DAYS_IN_UNITPER,
                   PDFILE.PD_UNITPER_PER_YEAR,
                   ISNULL(PDFILE.PD_REPOCD,''),
                   ISNULL(PDFILE.PD_BR_TFR_FG,''),
                   PDFILE.PD_O3INS_DLPROC,
                   PDFILE.PD_INSPREM_DLADJ,
                   PDFILE.PD_PLAMT,
                   PDFILE.PD_MTD_PRIN,
                   PDFILE.PD_MTD_LC,
                   PDFILE.PD_MTD_INT,
                   PDFILE.PD_MTD_OT,
                   PDFILE.PD_MTD_EX,
                   PDFILE.PD_BNKRPT_CHAPTER,
                   ISNULL(PDFILE.PD_BMO_CON_STATUS,''),
                   ISNULL(PDFILE.PD_BMO_REC_STATUS,''),
                   PDFILE.PD_APP_NUMBER,
                   ISNULL(PDFILE.PD_GIFTNET,''),
                   PDFILE.PD_GIFTNET_OUR_PO,
                   ISNULL(PDFILE.PD_PROC_DISBURSED,''),
                   PDFILE.PD_NOODDPY,
                   PDFILE.PD_CONPROFILE_1,
                   PDFILE.PD_CONPROFILE_2,
                   PDFILE.PD_CONPROFILE_3,
                   PDFILE.PD_CONPROFILE_4,
                   PDFILE.PD_CONPROFILE_5,
                   PDFILE.PD_RECPROFILE_1,
                   PDFILE.PD_RECPROFILE_2,
                   PDFILE.PD_RECPROFILE_3,
                   PDFILE.PD_RECPROFILE_4,
                   PDFILE.PD_RECPROFILE_5,
                   ISNULL(PDFILE.PD_TRW_STATUS,''),
                   ISNULL(PDFILE.PD_ORIGIN,''),
                   ISNULL(PDFILE.PD_PO_TRCD,''),
                   PDFILE.PD_NEXIS_CRSCORE,
                   ISNULL(PDFILE.PD_TRW_B_CONSUMER_IND,''),
                   ISNULL(PDFILE.PD_PARVHELD_PAID_FG,''),
                   ISNULL(PDFILE.PD_ACCT_FROZEN,''),
                   ISNULL(PDFILE.PD_TRW_SPECIAL_CODE,''),
                   ISNULL(PDFILE.PD_CREATION_STATUS,''),
                   PDFILE.PD_TOTNOLCHG,
                   ISNULL(PDFILE.PD_LBOX,''),
                   PDFILE.PD_EXTRA_PRIN_LFP,
                   PDFILE.PD_REMAIN_INT_SINCE_LFP,
                   PDFILE.PD_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(PDFILE.PD_INTERIM_COUPON,''),
                   PDFILE.PD_DLCOMM_O3_CMSI,
                   PDFILE.PD_DLCOMM_PP_CMSI,
                   ISNULL(PDFILE.PD_TRW_J_CONSUMER_IND,''),
                   ISNULL(PDFILE.PD_ACH,''),
                   ISNULL(PDFILE.PD_1099_C_FG,''),
                   ISNULL(PDFILE.PD_TRW_COMPLIANCE_CODE,''),
                   ISNULL(PDFILE.PD_UNWIND_REASON_CODE,''),
                   ISNULL(PDFILE.PD_PBI_PBD_FG,''),
                   ISNULL(PDFILE.PD_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(PDFILE.PD_APPROVED_BY_USER,''),
                   ISNULL(PDFILE.PD_APPROVED_BY_CODE,''),
                   ISNULL(CAST(PDFILE.PD_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_APPROVED_BY_TIME,
                   ISNULL(CAST(PDFILE.PD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   PDFILE.PD_LTOUCH_TIME,
                   ISNULL(PDFILE.PD_HOLD_COLLATERAL,''),
                   ISNULL(PDFILE.PD_DLPAID,''),
                   PDFILE.PD_CKNO,
                   ISNULL(PDFILE.PD_FREE1,''),
                   ISNULL(PDFILE.PD_FREE2,''),
                   ISNULL(PDFILE.PD_FREE_SPACE_FILLER,''),
                   PDFILE.PD_POSTING_BR,
                   ISNULL(PDFILE.PD_OTHBR_FG,''),
                   PDFILE.PD_APPROVAL_LIMIT,
                   ISNULL(PDFILE.PD_APPROVAL_USERID,''),
                   PDFILE.PD_MIN_CRED_SCORE,
                   ISNULL(PDFILE.PD_CRED_SCORE_USERID,''),
                   PDFILE.PD_FINAMT,
                   ISNULL(PDFILE.PD_PROCEEDS_FG,''),
                   ISNULL(PDFILE.PD_POINTS_FG,''),
                   ISNULL(PDFILE.PD_OVRDRATE_FG,''),
                   ISNULL(PDFILE.PD_INS_FG_1,''),
                   ISNULL(PDFILE.PD_INS_FG_2,''),
                   ISNULL(PDFILE.PD_INS_FG_3,''),
                   ISNULL(PDFILE.PD_INS_FG_4,''),
                   ISNULL(PDFILE.PD_INS_FG_5,''),
                   ISNULL(PDFILE.PD_INS_FG_6,''),
                   ISNULL(PDFILE.PD_INS_FG_7,''),
                   ISNULL(PDFILE.PD_INS_FG_8,''),
                   ISNULL(PDFILE.PD_INS_FG_9,''),
                   ISNULL(PDFILE.PD_INS_FG_10,''),
                   ISNULL(PDFILE.PD_SERCHG_FG,''),
                   ISNULL(PDFILE.PD_MAINTFEE_FG,''),
                   ISNULL(PDFILE.PD_EXTCHG_FG,''),
                   ISNULL(PDFILE.PD_REGPYAMT_FG,''),
                   ISNULL(PDFILE.PD_FEECD_1,''),
                   ISNULL(PDFILE.PD_FEECD_2,''),
                   ISNULL(PDFILE.PD_FEECD_3,''),
                   ISNULL(PDFILE.PD_FEECD_4,''),
                   ISNULL(PDFILE.PD_FEECD_5,''),
                   ISNULL(PDFILE.PD_FEECD_6,''),
                   ISNULL(PDFILE.PD_FEECD_7,''),
                   ISNULL(PDFILE.PD_FEECD_8,''),
                   ISNULL(PDFILE.PD_FEECD_9,''),
                   ISNULL(PDFILE.PD_FEECD_10,''),
                   ISNULL(PDFILE.PD_FEECD_11,''),
                   ISNULL(PDFILE.PD_FEECD_12,''),
                   ISNULL(PDFILE.PD_FEECD_13,''),
                   ISNULL(PDFILE.PD_FEECD_14,''),
                   ISNULL(PDFILE.PD_FEECD_15,''),
                   ISNULL(PDFILE.PD_FEECD_16,''),
                   ISNULL(PDFILE.PD_FEECD_17,''),
                   ISNULL(PDFILE.PD_FEECD_18,''),
                   ISNULL(PDFILE.PD_FEECD_19,''),
                   ISNULL(PDFILE.PD_FEECD_20,''),
                   PDFILE.PD_PRPAID_AHEAD,
                   PDFILE.PD_RESTRICT_ADJ,
                   PDFILE.PD_DUAL_LOAN_ADJ,
                   PDFILE.PD_CAN_LOAN_MAX,
                   PDFILE.PD_PERDOWN,
                   ISNULL(PDFILE.PD_SELLPR_FG,''),
                   ISNULL(PDFILE.PD_DOWNPY_FG,''),
                   ISNULL(PDFILE.PD_PERDOWN_FG,''),
                   ISNULL(PDFILE.PD_SELL_DOWN_FG,''),
                   ISNULL(PDFILE.PD_BALLOON_FG,''),
                   ISNULL(PDFILE.PD_BALLOON_LAST_FG,''),
                   ISNULL(PDFILE.PD_FEECD_FG_1,''),
                   ISNULL(PDFILE.PD_FEECD_FG_2,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_1,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_1,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_1,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_2,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_2,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_2,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_3,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_3,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_3,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_4,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_4,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_4,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_5,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_5,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_5,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_6,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_6,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_6,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_7,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_7,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_7,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_8,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_8,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_8,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_9,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_9,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_9,''),
                   ISNULL(PDFILE.PD_INS_TERM_SW_10,''),
                   ISNULL(PDFILE.PD_COVERAGE_SW_10,''),
                   ISNULL(PDFILE.PD_PREMIUM_SW_10,''),
                   ISNULL(PDFILE.PD_DLDISC_FG,''),
                   ISNULL(PDFILE.PD_DLRVPRIN_FG_1,''),
                   ISNULL(PDFILE.PD_DLRVPRIN_FG_2,''),
                   ISNULL(PDFILE.PD_DLRVPRIN_FG_3,''),
                   ISNULL(PDFILE.PD_DLRVCHGS_FG_1,''),
                   ISNULL(PDFILE.PD_DLRVCHGS_FG_2,''),
                   ISNULL(PDFILE.PD_DLRVCHGS_FG_3,''),
                   ISNULL(PDFILE.PD_DLPARVHELD_FG,''),
                   ISNULL(PDFILE.PD_DLPARVPAID_FG,''),
                   ISNULL(PDFILE.PD_ONE_MTH_NO_DAY_EXT_SW,''),
                   ISNULL(PDFILE.PD_ASSIGN_RN_ACCTNO,''),
                   PDFILE.PD_APR_FINAMT,
                   ISNULL(PDFILE.PD_DLCOMM_PP_CMSI_FG,''),
                   ISNULL(PDFILE.PD_DLCOMM_O3_CMSI_FG,''),
                   ISNULL(PDFILE.PD_FILLER_TO_MATCH_LN,''),
                   ISNULL(PDFILE.PD2_ID,''),
                   PDFILE.PD2_BRANCH,
                   ISNULL(PDFILE.PD2_BWNAME,'')
            INTO :QPD-REC
            FROM DBO.PDFILE
            WHERE PDFILE.PD2_ID = :QPD2-ID
              AND PDFILE.PD2_BRANCH = :QPD2-BRANCH
              AND PDFILE.PD2_BWNAME = :QPD2-BWNAME
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PD2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-PD-FIELDS.

      *-----------------------------------------------------------------
       READ-PD2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE PD-PATH-OWNBR  TO PD-BRANCH.
           MOVE PD-SELECT-PATH TO E-FILE.
           MOVE PD2-KEY        TO E-KEYX.
           INITIALIZE QPD-REC.

           IF ( PD2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH PD2_CURSOR INTO :QPD-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PD2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-PD-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-PD2-FILE.

           IF ( PD2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE PD2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO PD2-CURSOR-STAT.

      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
 
      *=================================================================
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
      *-----------------------------------------------------------------
       END-RUN.

           PERFORM CLOSE-FILES.

      *    MOVE WK1-PATH TO ACCESS-BUF.
      *    PERFORM REMOVE-WORKFILE.

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND         ) AND
              ( EXT-ROUTE-BUF = "00" OR "70" )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.

           IF ( SV-BEG-BRANCH NOT = 9999 )
              IF ( EXT-EOBUF-GROUP-FG = "Y"  )
                 MOVE SV-BEG-BRANCH     TO BEG-BRANCH
                 MOVE SV-END-BRANCH     TO END-BRANCH
                 MOVE SV-GROUP-FLAG     TO GROUP-FLAG
                 MOVE SV-ENT-GROUP      TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BRANCH TO BEG-BRANCH
                 MOVE SV-END-BRANCH TO END-BRANCH.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IOPDRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
