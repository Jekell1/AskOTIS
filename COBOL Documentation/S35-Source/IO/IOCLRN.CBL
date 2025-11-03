       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOCLRN.
       AUTHOR.        BAH
       DATE-WRITTEN.  03-19-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOCLRN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : CLFILE 
      * TABLE DESC    : CAN LOAN FILE
      * OUTPUT FILE   : XPC/IOCLRN.PRE
      * USED FOR      : LIBLP/LPWSCL.CPY  --- QCL-REC
      *                 LIBLP/LPCL1R.CPY  --- READ
      *                 LIBLP/LPCL1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0319 ***NEW***
      * JKC 2021-1206 MADE A CHANGE FOR THE INS EFFECTIVE DATE TO BE
      *          A STRAIGHT MOVE ACROSS SINCE IT IS NOT A TRUE DATE
      *          BECAUSE IT COULD BE A TERM:
      *          MOVE QCL-PD-INSEFF-DATE-?? TO CL-PD-INSEFF-DATE(??).
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
           "@(#)IO/IOCLRN.CBL S35 12/06/21-15:21:45 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01CL.CPY".
       COPY "LIBLP/LPWSCL.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QCL1-WHERE-BEG-KEY.
           03  QCL1-WBEG-GROUP            PIC X(04).
           03  QCL1-WBEG-CAN-NO           PIC X(02).
 
       01  QCL1-WHERE-END-KEY.
           03  QCL1-WEND-GROUP            PIC X(04).
           03  QCL1-WEND-CAN-NO           PIC X(02).

       01  QCL2-WHERE-BEG-KEY.
           03  QCL2-WBEG-GROUP            PIC X(04).
           03  QCL2-WBEG-DESC             PIC X(33).
 
       01  QCL2-WHERE-END-KEY.
           03  QCL2-WEND-GROUP            PIC X(04).
           03  QCL2-WEND-DESC             PIC X(33).
 
 
       01  QCL-REC.
           03  QCL-GROUP-ID             PIC X(4).
           03  QCL-CAN-NO               PIC XX.
           03  QCL-LTOUCH-DATE          PIC X(10).
           03  QCL-DESC                 PIC X(33).
           03  QCL-GIFTNET              PIC X.
           03  QCL-PD-ID                PIC X.
           03  QCL-PD-BRANCH            PIC 9(4).
           03  QCL-PD-SOC-SEC           PIC 9(10).
           03  QCL-PD-OWNBR             PIC 9(4).
           03  QCL-PD-ACCTNO            PIC 9(8).
           03  QCL-PD-CLASS             PIC 9(3).
           03  QCL-PD-SUBCLASS          PIC 99.
           03  QCL-PD-LAWCODE           PIC 99.
           03  QCL-PD-SSNO1             PIC 9(9).
           03  QCL-PD-MAKERCD1          PIC X.
           03  QCL-PD-SSNO2             PIC 9(9).
           03  QCL-PD-MAKERCD2          PIC X.
           03  QCL-PD-SSNO3             PIC 9(9).
           03  QCL-PD-MAKERCD3          PIC X.
           03  QCL-PD-SSNO4             PIC 9(9).
           03  QCL-PD-MAKERCD4          PIC X.
           03  QCL-PD-ORGST            PIC XX.
           03  QCL-PD-ORGBR            PIC 9(4).
           03  QCL-PD-OWNST            PIC XX.
           03  QCL-PD-SPRCLASS         PIC 9(3).
           03  QCL-PD-CONVERCD         PIC X.
           03  QCL-PD-MIDSTREAM        PIC X.
           03  QCL-PD-PURCD            PIC XXX.
           03  QCL-PD-SRCD             PIC XX.
           03  QCL-PD-COLLCD           PIC XX.
           03  QCL-PD-COLLID           PIC X(20).
           03  QCL-PD-POCD             PIC XX.
           03  QCL-PD-PLCD             PIC X.
           03  QCL-PD-PLTYPE           PIC X.
           03  QCL-PD-CONFIDENTCD      PIC X.
           03  QCL-PD-ALLOTCD          PIC X.
           03  QCL-PD-RECASTCD         PIC X.
           03  QCL-PD-SKIP-TRW         PIC X.
           03  QCL-PD-STATUSFG         PIC X.
           03  QCL-PD-SMALLOAN         PIC X.
           03  QCL-PD-SALEFIN          PIC X.
           03  QCL-PD-SERVACCT         PIC X.
           03  QCL-PD-LOANTYPE         PIC X.
           03  QCL-PD-ADDON-FG         PIC X.
           03  QCL-PD-PAY-SCHLD-FG     PIC X.
           03  QCL-PD-LC-CODE          PIC 99.
           03  QCL-PD-CAN-LOAN-NO      PIC XX.
           03  QCL-PD-STATFLAGS        PIC X(20).
           03  QCL-PD-ENTDATE          PIC X(10).
           03  QCL-PD-LOANDATE         PIC X(10).
           03  QCL-PD-PURDATE          PIC X(10).
           03  QCL-PD-INTDATE          PIC X(10).
           03  QCL-PD-CASHDATE         PIC X(10).
           03  QCL-PD-FILEDATE         PIC X(10).
           03  QCL-PD-RESCISSION-DATE    PIC X(10).
           03  QCL-PD-PART-RECOURSE-DATE PIC X(10).
           03  QCL-PD-ORIG-1STPYDATE     PIC X(10).
           03  QCL-PD-1STPYDATE        PIC X(10).
           03  QCL-PD-JDDATE           PIC X(10).
           03  QCL-PD-PLDATE           PIC X(10).
           03  QCL-PD-PL-TRDATE        PIC X(10).
           03  QCL-PD-ACCELDATE        PIC X(10).
           03  QCL-PD-REDUDATE         PIC X(10).
           03  QCL-PD-POFFDATE         PIC X(10).
           03  QCL-PD-PO-TRDATE        PIC X(10).
           03  QCL-PD-LEGALDATE        PIC X(10).
           03  QCL-PD-BNKRPTDATE       PIC X(10).
           03  QCL-PD-BNKRPT-DISMISS-DATE   PIC X(10).
           03  QCL-PD-BNKRPT-DISCHARGE-DATE PIC X(10).
           03  QCL-PD-BNKRPT-REAFFIRM-DATE  PIC X(10).
           03  QCL-PD-DEBTAIDATE            PIC X(10).
           03  QCL-PD-REPODATE              PIC X(10).
           03  QCL-PD-REDEEMDATE            PIC X(10).
           03  QCL-PD-SOLDDATE              PIC X(10).
           03  QCL-PD-INS-CONFIRM-DATE     PIC X(10).
           03  QCL-PD-PRIOR-RN-FOR10-DATE  PIC X(10).
           03  QCL-PD-LAST-RN-FOR10-DATE   PIC X(10).
           03  QCL-PD-TRW-DOFD-DATE        PIC X(10).
           03  QCL-PD-LAST-PYPA-DATE       PIC X(10).
           03  QCL-PD-BT-TRDATE            PIC X(10).
           03  QCL-PD-METRO2-LAST-PMT-DATE PIC X(10).
           03  QCL-PD-FOLLOWUP-DATE        PIC X(10).
           03  QCL-PD-LAST-ACCRUAL-DATE    PIC X(10).
           03  QCL-PD-APRATE           PIC 999V999.
           03  QCL-PD-EFFRATE          PIC 999V999.
           03  QCL-PD-SMPRATE          PIC 999V999.
           03  QCL-PD-YLDRATE          PIC 999V999.
           03  QCL-PD-OVRDRATE         PIC 999V999.
           03  QCL-PD-ENDORATE         PIC 999V999.
           03  QCL-PD-JDRATE           PIC 999V999.
           03  QCL-PD-ACCRATE          PIC 999V999.
           03  QCL-PD-REDURATE         PIC 999V999.
           03  QCL-PD-CONRATE          PIC 999V999.
           03  QCL-PD-SERVPER          PIC 9(3)V99.
           03  QCL-PD-SELLPR           PIC 9(7)V99.
           03  QCL-PD-DOWNPY           PIC 9(7)V99.
           03  QCL-PD-TRADEIN          PIC S9(7)V99.
           03  QCL-PD-LNAMT            PIC 9(7)V99.
           03  QCL-PD-FINCHG           PIC 9(7)V99.
           03  QCL-PD-INTCHG           PIC 9(7)V99.
           03  QCL-PD-SERCHG           PIC 9(5)V99.
           03  QCL-PD-EXTCHG           PIC 9(5)V99.
           03  QCL-PD-MAINTFEE         PIC 9(5)V99.
           03  QCL-PD-POINTS           PIC 99V99.
           03  QCL-PD-PROCEEDS         PIC S9(7)V99.
           03  QCL-PD-ORGTERM          PIC 9(3).
           03  QCL-PD-1STPYAMT         PIC 9(7)V99.
           03  QCL-PD-NOREGPY          PIC 9(3).
           03  QCL-PD-REGPYAMT         PIC 9(7)V99.
           03  QCL-PD-LASTPYAMT        PIC S9(7)V99.
           03  QCL-PD-PROCASH          PIC S9(7)V99.
           03  QCL-PD-TILLNO           PIC 9(2).
           03  QCL-PD-STATE-FLAG       PIC X.
           03  QCL-PD-STATE-WORKER     PIC S9(7)V99.
           03  QCL-PD-STATE-WORKER2    PIC S9(5)V99.
           03  QCL-PD-FEECODE-1      PIC XX.
           03  QCL-PD-FEEAMT-1       PIC 9(5)V99.
           03  QCL-PD-FEEOURS-1      PIC X.
           03  QCL-PD-FEEFINCHG-1    PIC X.
           03  QCL-PD-FEECODE-2      PIC XX.
           03  QCL-PD-FEEAMT-2       PIC 9(5)V99.
           03  QCL-PD-FEEOURS-2      PIC X.
           03  QCL-PD-FEEFINCHG-2    PIC X.
           03  QCL-PD-FEECODE-3      PIC XX.
           03  QCL-PD-FEEAMT-3       PIC 9(5)V99.
           03  QCL-PD-FEEOURS-3      PIC X.
           03  QCL-PD-FEEFINCHG-3    PIC X.
           03  QCL-PD-FEECODE-4      PIC XX.
           03  QCL-PD-FEEAMT-4       PIC 9(5)V99.
           03  QCL-PD-FEEOURS-4      PIC X.
           03  QCL-PD-FEEFINCHG-4    PIC X.
           03  QCL-PD-FEECODE-5      PIC XX.
           03  QCL-PD-FEEAMT-5       PIC 9(5)V99.
           03  QCL-PD-FEEOURS-5      PIC X.
           03  QCL-PD-FEEFINCHG-5    PIC X.
           03  QCL-PD-FEECODE-6      PIC XX.
           03  QCL-PD-FEEAMT-6       PIC 9(5)V99.
           03  QCL-PD-FEEOURS-6      PIC X.
           03  QCL-PD-FEEFINCHG-6    PIC X.
           03  QCL-PD-FEECODE-7      PIC XX.
           03  QCL-PD-FEEAMT-7       PIC 9(5)V99.
           03  QCL-PD-FEEOURS-7      PIC X.
           03  QCL-PD-FEEFINCHG-7    PIC X.
           03  QCL-PD-FEECODE-8      PIC XX.
           03  QCL-PD-FEEAMT-8       PIC 9(5)V99.
           03  QCL-PD-FEEOURS-8      PIC X.
           03  QCL-PD-FEEFINCHG-8    PIC X.
           03  QCL-PD-FEECODE-9      PIC XX.
           03  QCL-PD-FEEAMT-9       PIC 9(5)V99.
           03  QCL-PD-FEEOURS-9      PIC X.
           03  QCL-PD-FEEFINCHG-9    PIC X.
           03  QCL-PD-FEECODE-10     PIC XX.
           03  QCL-PD-FEEAMT-10      PIC 9(5)V99.
           03  QCL-PD-FEEOURS-10     PIC X.
           03  QCL-PD-FEEFINCHG-10   PIC X.
           03  QCL-PD-FEECODE-11     PIC XX.
           03  QCL-PD-FEEAMT-11      PIC 9(5)V99.
           03  QCL-PD-FEEOURS-11     PIC X.
           03  QCL-PD-FEEFINCHG-11   PIC X.
           03  QCL-PD-FEECODE-12     PIC XX.
           03  QCL-PD-FEEAMT-12      PIC 9(5)V99.
           03  QCL-PD-FEEOURS-12     PIC X.
           03  QCL-PD-FEEFINCHG-12   PIC X.
           03  QCL-PD-FEECODE-13     PIC XX.
           03  QCL-PD-FEEAMT-13      PIC 9(5)V99.
           03  QCL-PD-FEEOURS-13     PIC X.
           03  QCL-PD-FEEFINCHG-13   PIC X.
           03  QCL-PD-FEECODE-14     PIC XX.
           03  QCL-PD-FEEAMT-14      PIC 9(5)V99.
           03  QCL-PD-FEEOURS-14     PIC X.
           03  QCL-PD-FEEFINCHG-14   PIC X.
           03  QCL-PD-FEECODE-15     PIC XX.
           03  QCL-PD-FEEAMT-15      PIC 9(5)V99.
           03  QCL-PD-FEEOURS-15     PIC X.
           03  QCL-PD-FEEFINCHG-15   PIC X.
           03  QCL-PD-FEECODE-16     PIC XX.
           03  QCL-PD-FEEAMT-16      PIC 9(5)V99.
           03  QCL-PD-FEEOURS-16     PIC X.
           03  QCL-PD-FEEFINCHG-16   PIC X.
           03  QCL-PD-FEECODE-17     PIC XX.
           03  QCL-PD-FEEAMT-17      PIC 9(5)V99.
           03  QCL-PD-FEEOURS-17     PIC X.
           03  QCL-PD-FEEFINCHG-17   PIC X.
           03  QCL-PD-FEECODE-18     PIC XX.
           03  QCL-PD-FEEAMT-18      PIC 9(5)V99.
           03  QCL-PD-FEEOURS-18     PIC X.
           03  QCL-PD-FEEFINCHG-18   PIC X.
           03  QCL-PD-FEECODE-19     PIC XX.
           03  QCL-PD-FEEAMT-19      PIC 9(5)V99.
           03  QCL-PD-FEEOURS-19     PIC X.
           03  QCL-PD-FEEFINCHG-19   PIC X.
           03  QCL-PD-FEECODE-20     PIC XX.
           03  QCL-PD-FEEAMT-20      PIC 9(5)V99.
           03  QCL-PD-FEEOURS-20     PIC X.
           03  QCL-PD-FEEFINCHG-20   PIC X.
           03  QCL-PD-INSURED        PIC X.
           03  QCL-PD-INSTYPES       PIC X.
           03  QCL-PD-INSELIGIBLE    PIC X.
           03  QCL-PD-INSCOMP-1         PIC 99.
           03  QCL-PD-INSOURS-1         PIC X.
           03  QCL-PD-INSEFF-DATE-1     PIC X(10).
           03  QCL-PD-INSEXP-DATE-1     PIC X(10).
           03  QCL-PD-INSPREM-1         PIC 9(5)V99.
           03  QCL-PD-INSCOVR-1         PIC 9(7)V99.
           03  QCL-PD-INSCOMM-1         PIC 9(5)V99.
           03  QCL-PD-INSANTICERN-1     PIC S9(7)V99.
           03  QCL-PD-INSANTICADJ-1     PIC S9(7)V99.
           03  QCL-PD-INSUFERN-1        PIC S9(7)V99.
           03  QCL-PD-INSACCERN-1       PIC S9(7)V99.
           03  QCL-PD-INSEARN-ORDER-1   PIC 99.
           03  QCL-PD-INSFINCHG-1       PIC X.
           03  QCL-PD-INSCOMP-2         PIC 99.
           03  QCL-PD-INSOURS-2         PIC X.
           03  QCL-PD-INSEFF-DATE-2     PIC X(10).
           03  QCL-PD-INSEXP-DATE-2     PIC X(10).
           03  QCL-PD-INSPREM-2         PIC 9(5)V99.
           03  QCL-PD-INSCOVR-2         PIC 9(7)V99.
           03  QCL-PD-INSCOMM-2         PIC 9(5)V99.
           03  QCL-PD-INSANTICERN-2     PIC S9(7)V99.
           03  QCL-PD-INSANTICADJ-2     PIC S9(7)V99.
           03  QCL-PD-INSUFERN-2        PIC S9(7)V99.
           03  QCL-PD-INSACCERN-2       PIC S9(7)V99.
           03  QCL-PD-INSEARN-ORDER-2   PIC 99.
           03  QCL-PD-INSFINCHG-2       PIC X.
           03  QCL-PD-INSCOMP-3         PIC 99.
           03  QCL-PD-INSOURS-3         PIC X.
           03  QCL-PD-INSEFF-DATE-3     PIC X(10).
           03  QCL-PD-INSEXP-DATE-3     PIC X(10).
           03  QCL-PD-INSPREM-3         PIC 9(5)V99.
           03  QCL-PD-INSCOVR-3         PIC 9(7)V99.
           03  QCL-PD-INSCOMM-3         PIC 9(5)V99.
           03  QCL-PD-INSANTICERN-3     PIC S9(7)V99.
           03  QCL-PD-INSANTICADJ-3     PIC S9(7)V99.
           03  QCL-PD-INSUFERN-3        PIC S9(7)V99.
           03  QCL-PD-INSACCERN-3       PIC S9(7)V99.
           03  QCL-PD-INSEARN-ORDER-3   PIC 99.
           03  QCL-PD-INSFINCHG-3       PIC X.
           03  QCL-PD-INSCOMP-4         PIC 99.
           03  QCL-PD-INSOURS-4         PIC X.
           03  QCL-PD-INSEFF-DATE-4     PIC X(10).
           03  QCL-PD-INSEXP-DATE-4     PIC X(10).
           03  QCL-PD-INSPREM-4         PIC 9(5)V99.
           03  QCL-PD-INSCOVR-4         PIC 9(7)V99.
           03  QCL-PD-INSCOMM-4         PIC 9(5)V99.
           03  QCL-PD-INSANTICERN-4     PIC S9(7)V99.
           03  QCL-PD-INSANTICADJ-4     PIC S9(7)V99.
           03  QCL-PD-INSUFERN-4        PIC S9(7)V99.
           03  QCL-PD-INSACCERN-4       PIC S9(7)V99.
           03  QCL-PD-INSEARN-ORDER-4   PIC 99.
           03  QCL-PD-INSFINCHG-4       PIC X.
           03  QCL-PD-INSCOMP-5         PIC 99.
           03  QCL-PD-INSOURS-5         PIC X.
           03  QCL-PD-INSEFF-DATE-5     PIC X(10).
           03  QCL-PD-INSEXP-DATE-5     PIC X(10).
           03  QCL-PD-INSPREM-5         PIC 9(5)V99.
           03  QCL-PD-INSCOVR-5         PIC 9(7)V99.
           03  QCL-PD-INSCOMM-5         PIC 9(5)V99.
           03  QCL-PD-INSANTICERN-5     PIC S9(7)V99.
           03  QCL-PD-INSANTICADJ-5     PIC S9(7)V99.
           03  QCL-PD-INSUFERN-5        PIC S9(7)V99.
           03  QCL-PD-INSACCERN-5       PIC S9(7)V99.
           03  QCL-PD-INSEARN-ORDER-5   PIC 99.
           03  QCL-PD-INSFINCHG-5       PIC X.
           03  QCL-PD-INSCOMP-6         PIC 99.
           03  QCL-PD-INSOURS-6         PIC X.
           03  QCL-PD-INSEFF-DATE-6     PIC X(10).
           03  QCL-PD-INSEXP-DATE-6     PIC X(10).
           03  QCL-PD-INSPREM-6         PIC 9(5)V99.
           03  QCL-PD-INSCOVR-6         PIC 9(7)V99.
           03  QCL-PD-INSCOMM-6         PIC 9(5)V99.
           03  QCL-PD-INSANTICERN-6     PIC S9(7)V99.
           03  QCL-PD-INSANTICADJ-6     PIC S9(7)V99.
           03  QCL-PD-INSUFERN-6        PIC S9(7)V99.
           03  QCL-PD-INSACCERN-6       PIC S9(7)V99.
           03  QCL-PD-INSEARN-ORDER-6   PIC 99.
           03  QCL-PD-INSFINCHG-6       PIC X.
           03  QCL-PD-INSCOMP-7         PIC 99.
           03  QCL-PD-INSOURS-7         PIC X.
           03  QCL-PD-INSEFF-DATE-7     PIC X(10).
           03  QCL-PD-INSEXP-DATE-7     PIC X(10).
           03  QCL-PD-INSPREM-7         PIC 9(5)V99.
           03  QCL-PD-INSCOVR-7         PIC 9(7)V99.
           03  QCL-PD-INSCOMM-7         PIC 9(5)V99.
           03  QCL-PD-INSANTICERN-7     PIC S9(7)V99.
           03  QCL-PD-INSANTICADJ-7     PIC S9(7)V99.
           03  QCL-PD-INSUFERN-7        PIC S9(7)V99.
           03  QCL-PD-INSACCERN-7       PIC S9(7)V99.
           03  QCL-PD-INSEARN-ORDER-7   PIC 99.
           03  QCL-PD-INSFINCHG-7       PIC X.
           03  QCL-PD-INSCOMP-8         PIC 99.
           03  QCL-PD-INSOURS-8         PIC X.
           03  QCL-PD-INSEFF-DATE-8     PIC X(10).
           03  QCL-PD-INSEXP-DATE-8     PIC X(10).
           03  QCL-PD-INSPREM-8         PIC 9(5)V99.
           03  QCL-PD-INSCOVR-8         PIC 9(7)V99.
           03  QCL-PD-INSCOMM-8         PIC 9(5)V99.
           03  QCL-PD-INSANTICERN-8     PIC S9(7)V99.
           03  QCL-PD-INSANTICADJ-8     PIC S9(7)V99.
           03  QCL-PD-INSUFERN-8        PIC S9(7)V99.
           03  QCL-PD-INSACCERN-8       PIC S9(7)V99.
           03  QCL-PD-INSEARN-ORDER-8   PIC 99.
           03  QCL-PD-INSFINCHG-8       PIC X.
           03  QCL-PD-INSCOMP-9         PIC 99.
           03  QCL-PD-INSOURS-9         PIC X.
           03  QCL-PD-INSEFF-DATE-9     PIC X(10).
           03  QCL-PD-INSEXP-DATE-9     PIC X(10).
           03  QCL-PD-INSPREM-9         PIC 9(5)V99.
           03  QCL-PD-INSCOVR-9         PIC 9(7)V99.
           03  QCL-PD-INSCOMM-9         PIC 9(5)V99.
           03  QCL-PD-INSANTICERN-9     PIC S9(7)V99.
           03  QCL-PD-INSANTICADJ-9     PIC S9(7)V99.
           03  QCL-PD-INSUFERN-9        PIC S9(7)V99.
           03  QCL-PD-INSACCERN-9       PIC S9(7)V99.
           03  QCL-PD-INSEARN-ORDER-9   PIC 99.
           03  QCL-PD-INSFINCHG-9       PIC X.
           03  QCL-PD-INSCOMP-10        PIC 99.
           03  QCL-PD-INSOURS-10        PIC X.
           03  QCL-PD-INSEFF-DATE-10    PIC X(10).
           03  QCL-PD-INSEXP-DATE-10    PIC X(10).
           03  QCL-PD-INSPREM-10        PIC 9(5)V99.
           03  QCL-PD-INSCOVR-10        PIC 9(7)V99.
           03  QCL-PD-INSCOMM-10        PIC 9(5)V99.
           03  QCL-PD-INSANTICERN-10    PIC S9(7)V99.
           03  QCL-PD-INSANTICADJ-10    PIC S9(7)V99.
           03  QCL-PD-INSUFERN-10       PIC S9(7)V99.
           03  QCL-PD-INSACCERN-10      PIC S9(7)V99.
           03  QCL-PD-INSEARN-ORDER-10  PIC 99.
           03  QCL-PD-INSFINCHG-10      PIC X.
           03  QCL-PD-PY-YYYYMM-RECDEL    PIC 9(6).
           03  QCL-PD-MTD-PAYS-RECDEL     PIC S9(7)V99.
           03  QCL-PD-GARNISH-EXTR        PIC X.
           03  QCL-PD-SHIP-SAME-FG        PIC X.
           03  QCL-PD-AD-D-INS-COVR       PIC 9(7).
           03  QCL-PD-TOT-PARTIAL-CHGOFF  PIC S9(7)V99.
           03  QCL-PD-LPO                 PIC X(4).
           03  QCL-PD-REP-CD              PIC X(8).
           03  QCL-PD-EMP-ID              PIC 9(8).
           03  QCL-PD-PROCESS-CD          PIC X.
           03  QCL-PD-WCBC-CREDITLIM      PIC S9(5).
           03  QCL-PD-FOLLOWUP-CD         PIC X(4).
           03  QCL-PD-MIP-FG              PIC X.
           03  QCL-PD-MIP-MTD-CLAHPAID    PIC S9(5)V99.
           03  QCL-PD-MIP-CLPDTH-DATE     PIC X(10).
           03  QCL-PD-MIP-TOT-CLPAID      PIC S9(7)V99.
           03  QCL-PD-MIP-AHPDTH-DATE     PIC X(10).
           03  QCL-PD-MIP-TOT-AHPAID      PIC S9(7)V99.
           03  QCL-PD-IL-GROSS-INCOME     PIC S9(7)V99.
           03  QCL-PD-IL-GROSS-INCOME2    PIC S9(7)V99.
           03  QCL-PD-RESCISSION          PIC X(1).
           03  QCL-PD-INTERIM-TRW         PIC X(1).
           03  QCL-PD-DLNO                PIC 9(4)V99.
           03  QCL-PD-DLPOOL              PIC 9(5).
           03  QCL-PD-DLRECOURSE          PIC X.
           03  QCL-PD-DLPARTRCAMT         PIC 9(7)V99.
           03  QCL-PD-ORIGPURAMT          PIC 9(7)V99.
           03  QCL-PD-PURBAL              PIC S9(7)V99.
           03  QCL-PD-DLRVPRIN-1          PIC 9(7)V99.
           03  QCL-PD-DLRVPRIN-2          PIC 9(7)V99.
           03  QCL-PD-DLRVPRIN-3          PIC 9(7)V99.
           03  QCL-PD-DLRVCHGS-1          PIC 9(7)V99.
           03  QCL-PD-DLRVCHGS-2          PIC 9(7)V99.
           03  QCL-PD-DLRVCHGS-3          PIC 9(7)V99.
           03  QCL-PD-DLPARVHELD       PIC 9(7)V99.
           03  QCL-PD-DLPARVPAID       PIC 9(7)V99.
           03  QCL-PD-DLDISC           PIC 9(7)V99.
           03  QCL-PD-LCPDTH-DATE      PIC X(10).
           03  QCL-PD-INTPDTH-DATE     PIC X(10).
           03  QCL-PD-CURBAL           PIC S9(7)V99.
           03  QCL-PD-INTBAL           PIC S9(7)V99.
           03  QCL-PD-LCBAL            PIC S9(7)V99.
           03  QCL-PD-OTHBAL           PIC S9(7)V99.
           03  QCL-PD-OT2BAL           PIC S9(7)V99.
           03  QCL-PD-LCPAID           PIC S9(7)V99.
           03  QCL-PD-LCPARTIALS       PIC S9(7)V99.
           03  QCL-PD-STMT-COUNTER     PIC 9(5).
           03  QCL-PD-PRIOR-STMT-COUNTER PIC 9(5).
           03  QCL-PD-FROZEN-REASON    PIC XX.
           03  QCL-PD-COUPON-STATEMENT PIC X.
           03  QCL-PD-MONTHEND-BAL     PIC 9(7)V99.
           03  QCL-PD-ANTICTERM        PIC 9(3).
           03  QCL-PD-ANTICERN-1     PIC S9(7)V99.
           03  QCL-PD-ANTICADJ-1     PIC S9(7)V99.
           03  QCL-PD-UFERN-1        PIC S9(7)V99.
           03  QCL-PD-ACCERN-1       PIC S9(7)V99.
           03  QCL-PD-EARN-ORDER-1   PIC 99.
           03  QCL-PD-ANTICERN-2     PIC S9(7)V99.
           03  QCL-PD-ANTICADJ-2     PIC S9(7)V99.
           03  QCL-PD-UFERN-2        PIC S9(7)V99.
           03  QCL-PD-ACCERN-2       PIC S9(7)V99.
           03  QCL-PD-EARN-ORDER-2   PIC 99.
           03  QCL-PD-ANTICERN-3     PIC S9(7)V99.
           03  QCL-PD-ANTICADJ-3     PIC S9(7)V99.
           03  QCL-PD-UFERN-3        PIC S9(7)V99.
           03  QCL-PD-ACCERN-3       PIC S9(7)V99.
           03  QCL-PD-EARN-ORDER-3   PIC 99.
           03  QCL-PD-ANTICERN-4     PIC S9(7)V99.
           03  QCL-PD-ANTICADJ-4     PIC S9(7)V99.
           03  QCL-PD-UFERN-4        PIC S9(7)V99.
           03  QCL-PD-ACCERN-4       PIC S9(7)V99.
           03  QCL-PD-EARN-ORDER-4   PIC 99.
           03  QCL-PD-ANTICERN-5     PIC S9(7)V99.
           03  QCL-PD-ANTICADJ-5     PIC S9(7)V99.
           03  QCL-PD-UFERN-5        PIC S9(7)V99.
           03  QCL-PD-ACCERN-5       PIC S9(7)V99.
           03  QCL-PD-EARN-ORDER-5   PIC 99.
           03  QCL-PD-ANTICERN-6     PIC S9(7)V99.
           03  QCL-PD-ANTICADJ-6     PIC S9(7)V99.
           03  QCL-PD-UFERN-6        PIC S9(7)V99.
           03  QCL-PD-ACCERN-6       PIC S9(7)V99.
           03  QCL-PD-EARN-ORDER-6   PIC 99.
           03  QCL-PD-ANTICERN-7     PIC S9(7)V99.
           03  QCL-PD-ANTICADJ-7     PIC S9(7)V99.
           03  QCL-PD-UFERN-7        PIC S9(7)V99.
           03  QCL-PD-ACCERN-7       PIC S9(7)V99.
           03  QCL-PD-EARN-ORDER-7   PIC 99.
           03  QCL-PD-ANTICERN-8     PIC S9(7)V99.
           03  QCL-PD-ANTICADJ-8     PIC S9(7)V99.
           03  QCL-PD-UFERN-8        PIC S9(7)V99.
           03  QCL-PD-ACCERN-8       PIC S9(7)V99.
           03  QCL-PD-EARN-ORDER-8   PIC 99.
           03  QCL-PD-ANTICERN-9     PIC S9(7)V99.
           03  QCL-PD-ANTICADJ-9     PIC S9(7)V99.
           03  QCL-PD-UFERN-9        PIC S9(7)V99.
           03  QCL-PD-ACCERN-9       PIC S9(7)V99.
           03  QCL-PD-EARN-ORDER-9   PIC 99.
           03  QCL-PD-ANTICERN-10    PIC S9(7)V99.
           03  QCL-PD-ANTICADJ-10    PIC S9(7)V99.
           03  QCL-PD-UFERN-10       PIC S9(7)V99.
           03  QCL-PD-ACCERN-10      PIC S9(7)V99.
           03  QCL-PD-EARN-ORDER-10  PIC 99.
           03  QCL-PD-LMO-PRIOR-INTEREST PIC S9(7)V99.
           03  QCL-PD-PRIOR-UNEARNED     PIC S9(7)V99.
           03  QCL-PD-ERNDELQ-STOP       PIC X.
           03  QCL-PD-NONACRERN          PIC S9(7)V99.
           03  QCL-PD-PRIOR-NONACRERN    PIC S9(7)V99.
           03  QCL-PD-ERN-IB-PDTH-DATE   PIC X(10).
           03  QCL-PD-ERN-IB-PRINBAL   PIC S9(7)V99.
           03  QCL-PD-ERN-IB-INTOWE    PIC S9(7)V99.
           03  QCL-PD-TOTERN           PIC S9(7)V99.
           03  QCL-PD-TOTOTH           PIC S9(7)V99.
           03  QCL-PD-TOTINT           PIC S9(7)V99.
           03  QCL-PD-TOTLCHG          PIC S9(7)V99.
           03  QCL-PD-TOTDEF           PIC S9(7)V99.
           03  QCL-PD-TOTSERACCT       PIC S9(7)V99.
           03  QCL-PD-LCWAIVERS        PIC S9(5)V99.
           03  QCL-PD-INTWAIVERS       PIC S9(5)V99.
           03  QCL-PD-TOTPAYMNTD       PIC S9(7)V99.
           03  QCL-PD-TOTEXCPAYMNTD    PIC S9(7)V99.
           03  QCL-PD-TOTNODEF         PIC S99.
           03  QCL-PD-YTDNODEF         PIC S99.
           03  QCL-PD-TOTNODEFACT      PIC S99.
           03  QCL-PD-YTDNODEFACT      PIC S99.
           03  QCL-PD-TOTNOALDEL       PIC S99.
           03  QCL-PD-YTDNOALDEL       PIC S99.
           03  QCL-PD-YTDINTPD         PIC S9(7)V99.
           03  QCL-PD-LYRINTPD         PIC S9(7)V99.
           03  QCL-PD-TOTINTPD         PIC S9(7)V99.
           03  QCL-PD-RECASTINT        PIC 9(7)V99.
           03  QCL-PD-RECASTPRIN       PIC 9(7)V99.
           03  QCL-PD-CL-REBATE    PIC X.
           03  QCL-PD-AH-REBATE    PIC X.
           03  QCL-PD-PP-REBATE    PIC X.
           03  QCL-PD-INT-REBATE   PIC X.
           03  QCL-PD-SC-REBATE    PIC X.
           03  QCL-PD-MF-REBATE    PIC X.
           03  QCL-PD-DF-REBATE    PIC X.
           03  QCL-PD-EX-REBATE    PIC X.
           03  QCL-PD-O1-REBATE    PIC X.
           03  QCL-PD-O2-REBATE    PIC X.
           03  QCL-PD-O3-REBATE    PIC X.
           03  QCL-PD-O4-REBATE    PIC X.
           03  QCL-PD-O5-REBATE    PIC X.
           03  QCL-PD-O6-REBATE    PIC X.
           03  QCL-PD-O7-REBATE    PIC X.
           03  QCL-PD-UN-O7-REB    PIC X.
           03  QCL-PD-UN-O8-REB    PIC X.
           03  QCL-PD-UN-O9-REB    PIC X.
           03  QCL-PD-UN-O10-REB   PIC X.
           03  QCL-PD-PLREASON         PIC XX.
           03  QCL-PD-COLLECTOR        PIC X(3).
           03  QCL-PD-ACTIONCD         PIC X(4).
           03  QCL-PD-ACTIONCD2        PIC X(4).
           03  QCL-PD-ATCODE           PIC X(3).
           03  QCL-PD-RECODE           PIC X(3).
           03  QCL-PD-ESCROW-FG        PIC X.
           03  QCL-PD-PROMDAYS-PASTDUE PIC S9(3).
           03  QCL-PD-PROMID           PIC X(3).
           03  QCL-PD-PROMDATE         PIC X(10).
           03  QCL-PD-PROMAMT          PIC 9(7)V99.
           03  QCL-PD-PROMDATE2        PIC X(10).
           03  QCL-PD-DATE-PAID-LAST   PIC X(10).
           03  QCL-PD-AMT-PAID-LAST    PIC 9(7)V99.
           03  QCL-PD-LASTCFORMNO      PIC 9(5).
           03  QCL-PD-LASTCFORMDATE    PIC X(10).
           03  QCL-PD-DELCNT           PIC 99.
           03  QCL-PD-DELCNT2          PIC 99.
           03  QCL-PD-CREDITLIM        PIC 9(7).
           03  QCL-PD-CRSCORE          PIC S9(3).
           03  QCL-PD-LASTSFORMNO      PIC 9(5).
           03  QCL-PD-LASTSFORMDATE    PIC X(10).
           03  QCL-PD-SOLCNT           PIC 99.
           03  QCL-PD-MESSAGE          PIC X(30).
           03  QCL-PD-OLDACCTNO        PIC 9(8).
           03  QCL-PD-PRLNNO           PIC 9(8).
           03  QCL-PD-PRBAL            PIC S9(7)V99.
           03  QCL-PD-PRINTDUE         PIC S9(7)V99.
           03  QCL-PD-PRFINCHGRB       PIC S9(7)V99.
           03  QCL-PD-PRINSRB          PIC S9(7)V99.
           03  QCL-PD-PRPOCD           PIC XX.
           03  QCL-PD-PRPODATE         PIC X(10).
           03  QCL-PD-PRDELINQAMT      PIC S9(7)V99.
           03  QCL-PD-PRLNAMT          PIC 9(7)V99.
           03  QCL-PD-PRPDUE-DAYS      PIC S9(5).
           03  QCL-PD-PRDLDISC-UNEARN  PIC S9(7)V99.
           03  QCL-PD-PRCONPROFILE     PIC 9(12).
           03  QCL-PD-SUM-ORIGPURAMT   PIC 9(7)V99.
           03  QCL-PD-SUM-LNAMT        PIC 9(7)V99.
           03  QCL-PD-SUM-FINCHG       PIC 9(7)V99.
           03  QCL-PD-SUM-INTCHG       PIC 9(7)V99.
           03  QCL-PD-SUM-ORGTERM      PIC 9(3).
           03  QCL-PD-SUM-REGPYAMT     PIC 9(7)V99.
           03  QCL-PD-SUM-EXTCHG       PIC 9(5)V99.
           03  QCL-PD-SUM-SERCHG       PIC 9(5)V99.
           03  QCL-PD-SUM-MAINTFEE     PIC 9(7)V99.
           03  QCL-PD-UNITPER-CD       PIC X.
           03  QCL-PD-UNITPER-FREQ     PIC 999.
           03  QCL-PD-DAYS-IN-UNITPER   PIC 9(3).
           03  QCL-PD-UNITPER-PER-YEAR  PIC 9(3)V9(12).
           03  QCL-PD-REPOCD            PIC X.
           03  QCL-PD-BR-TFR-FG         PIC X.
           03  QCL-PD-O3INS-DLPROC      PIC 9(7)V99.
           03  QCL-PD-INSPREM-DLADJ     PIC S9(5)V99.
           03  QCL-PD-PLAMT             PIC 9(7)V99.
           03  QCL-PD-MTD-PRIN          PIC S9(7)V99.
           03  QCL-PD-MTD-LC            PIC S9(5)V99.
           03  QCL-PD-MTD-INT           PIC S9(5)V99.
           03  QCL-PD-MTD-OT            PIC S9(5)V99.
           03  QCL-PD-MTD-EX            PIC S9(7)V99.
           03  QCL-PD-BNKRPT-CHAPTER    PIC 9(2).
           03  QCL-PD-BMO-CON-STATUS    PIC X.
           03  QCL-PD-BMO-REC-STATUS    PIC X.
           03  QCL-PD-APP-NUMBER        PIC 9(09).
           03  QCL-PD-GIFTNET           PIC X.
           03  QCL-PD-GIFTNET-OUR-PO    PIC 9(7).
           03  QCL-PD-PROC-DISBURSED    PIC X.
           03  QCL-PD-NOODDPY           PIC 9.
           03  QCL-PD-CONPROFILE-1      PIC 9(12).
           03  QCL-PD-CONPROFILE-2      PIC 9(12).
           03  QCL-PD-CONPROFILE-3      PIC 9(12).
           03  QCL-PD-CONPROFILE-4      PIC 9(12).
           03  QCL-PD-CONPROFILE-5      PIC 9(12).
           03  QCL-PD-RECPROFILE-1      PIC 9(12).
           03  QCL-PD-RECPROFILE-2      PIC 9(12).
           03  QCL-PD-RECPROFILE-3      PIC 9(12).
           03  QCL-PD-RECPROFILE-4      PIC 9(12).
           03  QCL-PD-RECPROFILE-5      PIC 9(12).
           03  QCL-PD-TRW-STATUS            PIC XX.
           03  QCL-PD-ORIGIN                PIC X(4).
           03  QCL-PD-PO-TRCD               PIC XX.
           03  QCL-PD-NEXIS-CRSCORE         PIC 9(3).
           03  QCL-PD-TRW-B-CONSUMER-IND    PIC X(2).
           03  QCL-PD-PARVHELD-PAID-FG      PIC X.
           03  QCL-PD-ACCT-FROZEN           PIC X.
           03  QCL-PD-TRW-SPECIAL-CODE      PIC X(2).
           03  QCL-PD-CREATION-STATUS       PIC X(1).
           03  QCL-PD-TOTNOLCHG             PIC S9(3).
           03  QCL-PD-LBOX                  PIC X(01).
           03  QCL-PD-EXTRA-PRIN-LFP        PIC S9(7)V99.
           03  QCL-PD-REMAIN-INT-SINCE-LFP  PIC S9(7)V99.
           03  QCL-PD-REMAIN-PRIN-SINCE-LFP PIC S9(7)V99.
           03  QCL-PD-INTERIM-COUPON        PIC X.
           03  QCL-PD-DLCOMM-O3-CMSI        PIC 9(5)V99.
           03  QCL-PD-DLCOMM-PP-CMSI        PIC 9(5)V99.
           03  QCL-PD-TRW-J-CONSUMER-IND    PIC X(2).
           03  QCL-PD-ACH                   PIC X.
           03  QCL-PD-1099-C-FG             PIC X.
           03  QCL-PD-TRW-COMPLIANCE-CODE   PIC XX.
           03  QCL-PD-UNWIND-REASON-CODE    PIC X(2).
           03  QCL-PD-PBI-PBD-FG            PIC X.
           03  QCL-PD-OLD-ACCT-FORMAT-FG    PIC X.
           03  QCL-PD-APPROVED-BY-USER      PIC X(10).
           03  QCL-PD-APPROVED-BY-CODE      PIC X(3).
           03  QCL-PD-APPROVED-BY-DATE      PIC X(10).
           03  QCL-PD-APPROVED-BY-TIME      PIC 9(6).
           03  QCL-PD-LTOUCH-DATE           PIC X(10).
           03  QCL-PD-LTOUCH-TIME           PIC 9(6).
           03  QCL-PD-HOLD-COLLATERAL         PIC X(60).
           03  QCL-PD-DLPAID                  PIC X.
           03  QCL-PD-CKNO                    PIC 9(6).
           03  QCL-PD-FREE1                   PIC X(70).
           03  QCL-PD-FREE2                   PIC X(70).
           03  QCL-PD-FREE-SPACE-FILLER       PIC X(44).
           03  QCL-PD-POSTING-BR              PIC 9(4).
           03  QCL-PD-OTHBR-FG                PIC X.
           03  QCL-PD-APPROVAL-LIMIT          PIC 9(7).
           03  QCL-PD-APPROVAL-USERID         PIC X(3).
           03  QCL-PD-MIN-CRED-SCORE          PIC 9(4).
           03  QCL-PD-CRED-SCORE-USERID       PIC X(3).
           03  QCL-PD-FINAMT                  PIC 9(7)V99.
           03  QCL-PD-PROCEEDS-FG             PIC X.
           03  QCL-PD-POINTS-FG               PIC X.
           03  QCL-PD-OVRDRATE-FG             PIC X.
           03  QCL-PD-INS-FG-1                PIC X.
           03  QCL-PD-INS-FG-2                PIC X.
           03  QCL-PD-INS-FG-3                PIC X.
           03  QCL-PD-INS-FG-4                PIC X.
           03  QCL-PD-INS-FG-5                PIC X.
           03  QCL-PD-INS-FG-6                PIC X.
           03  QCL-PD-INS-FG-7                PIC X.
           03  QCL-PD-INS-FG-8                PIC X.
           03  QCL-PD-INS-FG-9                PIC X.
           03  QCL-PD-INS-FG-10               PIC X.
           03  QCL-PD-SERCHG-FG               PIC X.
           03  QCL-PD-MAINTFEE-FG             PIC X.
           03  QCL-PD-EXTCHG-FG               PIC X.
           03  QCL-PD-REGPYAMT-FG             PIC X.
           03  QCL-PD-FEECD-1                 PIC XX.
           03  QCL-PD-FEECD-2                 PIC XX.
           03  QCL-PD-FEECD-3                 PIC XX.
           03  QCL-PD-FEECD-4                 PIC XX.
           03  QCL-PD-FEECD-5                 PIC XX.
           03  QCL-PD-FEECD-6                 PIC XX.
           03  QCL-PD-FEECD-7                 PIC XX.
           03  QCL-PD-FEECD-8                 PIC XX.
           03  QCL-PD-FEECD-9                 PIC XX.
           03  QCL-PD-FEECD-10                PIC XX.
           03  QCL-PD-FEECD-11                PIC XX.
           03  QCL-PD-FEECD-12                PIC XX.
           03  QCL-PD-FEECD-13                PIC XX.
           03  QCL-PD-FEECD-14                PIC XX.
           03  QCL-PD-FEECD-15                PIC XX.
           03  QCL-PD-FEECD-16                PIC XX.
           03  QCL-PD-FEECD-17                PIC XX.
           03  QCL-PD-FEECD-18                PIC XX.
           03  QCL-PD-FEECD-19                PIC XX.
           03  QCL-PD-FEECD-20                PIC XX.
           03  QCL-PD-PRPAID-AHEAD          PIC S9(7)V99.
           03  QCL-PD-RESTRICT-ADJ          PIC S9(7)V99.
           03  QCL-PD-DUAL-LOAN-ADJ         PIC S9(7)V99.
           03  QCL-PD-CAN-LOAN-MAX          PIC S9(7)V99.
           03  QCL-PD-PERDOWN               PIC 999V99.
           03  QCL-PD-SELLPR-FG             PIC X.
           03  QCL-PD-DOWNPY-FG             PIC X.
           03  QCL-PD-PERDOWN-FG            PIC X.
           03  QCL-PD-SELL-DOWN-FG          PIC X.
           03  QCL-PD-BALLOON-FG            PIC X.
           03  QCL-PD-BALLOON-LAST-FG       PIC X.
           03  QCL-PD-FEECD-FG-1            PIC X.
           03  QCL-PD-FEECD-FG-2            PIC X.
           03  QCL-PD-INS-TERM-SW-1          PIC X.
           03  QCL-PD-COVERAGE-SW-1          PIC X.
           03  QCL-PD-PREMIUM-SW-1           PIC X.
           03  QCL-PD-INS-TERM-SW-2          PIC X.
           03  QCL-PD-COVERAGE-SW-2          PIC X.
           03  QCL-PD-PREMIUM-SW-2           PIC X.
           03  QCL-PD-INS-TERM-SW-3          PIC X.
           03  QCL-PD-COVERAGE-SW-3          PIC X.
           03  QCL-PD-PREMIUM-SW-3           PIC X.
           03  QCL-PD-INS-TERM-SW-4          PIC X.
           03  QCL-PD-COVERAGE-SW-4          PIC X.
           03  QCL-PD-PREMIUM-SW-4           PIC X.
           03  QCL-PD-INS-TERM-SW-5          PIC X.
           03  QCL-PD-COVERAGE-SW-5          PIC X.
           03  QCL-PD-PREMIUM-SW-5           PIC X.
           03  QCL-PD-INS-TERM-SW-6          PIC X.
           03  QCL-PD-COVERAGE-SW-6          PIC X.
           03  QCL-PD-PREMIUM-SW-6           PIC X.
           03  QCL-PD-INS-TERM-SW-7          PIC X.
           03  QCL-PD-COVERAGE-SW-7          PIC X.
           03  QCL-PD-PREMIUM-SW-7           PIC X.
           03  QCL-PD-INS-TERM-SW-8          PIC X.
           03  QCL-PD-COVERAGE-SW-8          PIC X.
           03  QCL-PD-PREMIUM-SW-8           PIC X.
           03  QCL-PD-INS-TERM-SW-9          PIC X.
           03  QCL-PD-COVERAGE-SW-9          PIC X.
           03  QCL-PD-PREMIUM-SW-9           PIC X.
           03  QCL-PD-INS-TERM-SW-10         PIC X.
           03  QCL-PD-COVERAGE-SW-10         PIC X.
           03  QCL-PD-PREMIUM-SW-10          PIC X.
           03  QCL-PD-DLDISC-FG              PIC X.
           03  QCL-PD-DLRVPRIN-FG-1          PIC X.
           03  QCL-PD-DLRVPRIN-FG-2          PIC X.
           03  QCL-PD-DLRVPRIN-FG-3          PIC X.
           03  QCL-PD-DLRVCHGS-FG-1          PIC X.
           03  QCL-PD-DLRVCHGS-FG-2          PIC X.
           03  QCL-PD-DLRVCHGS-FG-3          PIC X.
           03  QCL-PD-DLPARVHELD-FG          PIC X.
           03  QCL-PD-DLPARVPAID-FG          PIC X.
           03  QCL-PD-ONE-MTH-NO-DAY-EXT-SW  PIC X.
           03  QCL-PD-ASSIGN-RN-ACCTNO       PIC X.
           03  QCL-PD-APR-FINAMT             PIC S9(7)V99.
           03  QCL-PD-DLCOMM-PP-CMSI-FG      PIC X.
           03  QCL-PD-DLCOMM-O3-CMSI-FG      PIC X.
           03  QCL-PD-FILLER-TO-MATCH-LN     PIC X(90).
           03  QCL-PD2-ID                    PIC X.
           03  QCL-PD2-BRANCH                PIC 9(4).
           03  QCL-PD2-BWNAME                PIC X(33).

       EXEC SQL END DECLARE SECTION END-EXEC.
      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S WS COPY MEMBERS
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

       01  LEGEND-BEG-GROUP. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING GROUP      (F3-ALL) ".

       01  LEGEND-END-GROUP.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING GROUP      ".

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
           03  BEG-GROUP               PIC X(04).
           03  END-GROUP               PIC X(04).
           03  BEG-CAN-NO              PIC X(02).
           03  END-CAN-NO              PIC X(02).
           03  BEG-DESC                PIC X(33).
           03  END-DESC                PIC X(33).

           03  PROCESS-KEY             PIC 9(01).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

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
           03  FILLER PIC X(38) VALUE "GROUP CAN   DESC".
           03  FILLER PIC X(30) VALUE "CLASS   PUR  TERM        LNAMT".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-CL-GROUP                   PIC X(4).
           03  FILLER                         PIC X(03).
           03  D01-CL-CAN-NO                  PIC X(2).
           03  FILLER                         PIC X(03).
           03  D01-CL-DESC                    PIC X(25).
           03  FILLER                         PIC X(03).
           03  D01-CL-PD-CLASS                PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-CL-PD-PURCD                PIC X(03).
           03  FILLER                         PIC X(03).
           03  D01-CL-PD-ORGTERM              PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-CL-PD-LNAMT                PIC 9(7).99.

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
      *COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
      *COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
      *COPY "LIBGB/GRWORKW.CPY".
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
      *COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBIO/IOCLRN_DEF.CPY".
       COPY "LIBIO/IOCLRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOCLRN_SCN.CPY".

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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      D    STOP FORM-PATHNAME.
           MOVE "B"                  TO CL-PATH-OVERRIDE.

           MOVE BEG-GROUP            TO QCL1-WBEG-GROUP
                                        QCL2-WBEG-GROUP.
           MOVE BEG-CAN-NO           TO QCL1-WBEG-CAN-NO.
           MOVE BEG-DESC             TO QCL2-WBEG-DESC.
      
           MOVE END-GROUP            TO QCL1-WEND-GROUP
                                        QCL2-WEND-GROUP.
           MOVE END-CAN-NO           TO QCL1-WEND-CAN-NO.
           MOVE END-DESC             TO QCL2-WEND-DESC.


           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-CL1-FILE
              PERFORM START-CL1-FILE
           ELSE
              PERFORM OPEN-CL2-FILE
              PERFORM START-CL2-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-CL1-FILE-NEXT
           ELSE
              PERFORM READ-CL2-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( CL-GROUP-ID < BEG-GROUP )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( CL-GROUP-ID > END-GROUP )
              GO TO MAIN-PROGRAM-END.

           IF ( PROCESS-KEY = 1 )
              IF CL-CAN-NO < BEG-CAN-NO OR > END-CAN-NO
                 GO TO MAIN-PROGRAM-NEXT.
           IF ( PROCESS-KEY = 2 )
              IF CL-DESC < BEG-DESC OR > END-DESC
                 GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE CL-GROUP-ID                TO D01-CL-GROUP.
           MOVE CL-CAN-NO                  TO D01-CL-CAN-NO.
           MOVE CL-DESC                    TO D01-CL-DESC.
           MOVE CL-PD-CLASS                TO D01-CL-PD-CLASS.
           MOVE CL-PD-PURCD                TO D01-CL-PD-PURCD.
           MOVE CL-PD-ORGTERM              TO D01-CL-PD-ORGTERM.
           MOVE CL-PD-LNAMT                TO D01-CL-PD-LNAMT.

           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.

           GO TO MAIN-PROGRAM-NEXT.

      *=================================================================
       MAIN-PROGRAM-END.

           PERFORM WRITE-SUMMARY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DISPLAY SUMMARY TO SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROGRAM COMPLETED!!!" TO SENDMESS-HEAD.
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
           MOVE "IOCLRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOCLRN-QUEUE-POS  TO Q-POS.
           MOVE IOCLRN-COPIES-POS TO C-POS.
           MOVE IOCLRN-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROCESSING..." TO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

           PERFORM GET-GPENV.
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
              GO TO ENTER-PROCESS-KEY.

      *=================================================================
       ENTER-BEG-GROUP.

           MOVE LEGEND-BEG-GROUP TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040BEG-GROUP." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-GROUP.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-GROUP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-GROUP.

      *-----------------------------------------------------------------
       ENTER-END-GROUP.

           MOVE LEGEND-END-GROUP TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040END-GROUP." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-GROUP.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-GROUP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-GROUP.

           IF ( BEG-GROUP > END-GROUP )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-GROUP.

           GO TO ENTER-BEG-CAN.

      *-----------------------------------------------------------------
       ENTER-ALL-GROUP.

           MOVE "S  A040BEG-GROUP." TO VDU.
           MOVE SPACES               TO BEG-GROUP VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040END-GROUP." TO VDU.
           MOVE ALL "Z"              TO END-GROUP VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-BEG-CAN.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A020BEG-CAN." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-GROUP.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CAN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CAN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-CAN-NO.

      *-----------------------------------------------------------------
       ENTER-END-CAN.

           MOVE "E  A020END-CAN." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CAN.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CAN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-CAN-NO.

           IF ( BEG-CAN-NO > END-CAN-NO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CAN.

           GO TO ENTER-BEG-DESC.

       ENTER-ALL-CAN.

           MOVE "S  A020BEG-CAN." TO VDU.
           MOVE SPACES               TO BEG-CAN-NO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020END-CAN." TO VDU.
           MOVE ALL "Z"              TO END-CAN-NO VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-DESC.

           MOVE "E  A330BEG-DESC." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CAN.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DESC.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DESC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-DESC.

      *-----------------------------------------------------------------
       ENTER-END-DESC.

           MOVE "E  A330END-DESC." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DESC.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DESC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-DESC.

           IF ( BEG-DESC > END-DESC )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DESC.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-DESC.

           MOVE "S  A330BEG-DESC." TO VDU.
           MOVE SPACES               TO BEG-DESC VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A330END-DESC." TO VDU.
           MOVE ALL "Z"              TO END-DESC VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DESC.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 < ZEROES ) OR
              ( VDUNUM0 > 2      ) 
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

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

           MOVE "S  A000BEG-GROUP."    TO VDU.
           MOVE BEG-GROUP              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-GROUP."    TO VDU.
           MOVE END-GROUP              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-CAN."    TO VDU.
           MOVE BEG-CAN-NO           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-CAN."    TO VDU.
           MOVE END-CAN-NO           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-DESC."    TO VDU.
           MOVE BEG-DESC              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-DESC."    TO VDU.
           MOVE END-DESC              TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN010PROCESS-KEY." TO VDU
           MOVE PROCESS-KEY           TO VDUNUM0.
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

           IF ( PROCESS-KEY = 1 )
              MOVE CL1-KEY TO PACE-DETAIL-KEY
           ELSE
              MOVE SPACES  TO PACE-DETAIL-KEY
              STRING CL-GROUP-ID,CL-CAN-NO DELIMITED BY "   "
                           INTO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING GROUP               : "    TO R01-PROMPT.
           MOVE BEG-GROUP                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    GROUP               : "    TO R01-PROMPT.
           MOVE END-GROUP                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CAN-NO               : "    TO R01-PROMPT.
           MOVE BEG-CAN-NO                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CAN-NO               : "    TO R01-PROMPT.
           MOVE END-CAN-NO                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING DESC               : "    TO R01-PROMPT.
           MOVE BEG-DESC                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DESC               : "    TO R01-PROMPT.
           MOVE END-DESC                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
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
           MOVE "SQL TABLE IO UTILITY - CLFILE"  TO H02-TITLE.
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{04}
           MOVE ALL "="                          TO PR-REC.
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

           MOVE EXT-CONAME-CONAME TO IOCLRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOCLRN-SYSDATE.
           DISPLAY IOCLRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOCLRN-FORM-FIELDS TO WR-BUF.
           MOVE IOCLRN-FORM-LIST TO WR-LIST.
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

           MOVE IOCLRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOCLRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOCLRN_EVA.CPY".
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
      *COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
      *COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".
      *COPY "LIBGB/GRSERCH.CPY".
      *COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBWI/PFSWINDOWS.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
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
       COPY "LIBGB/GBPRINT.CPY".

      *-----------------------------------------------------------------
      *    LOAD CL-REC FROM QCL-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CL-FIELDS.
           MOVE QCL-GROUP-ID TO CL-GROUP-ID.
           MOVE QCL-CAN-NO TO CL-CAN-NO.

           IF ( QCL-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-LTOUCH-DATE
           ELSE
              MOVE QCL-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-LTOUCH-DATE.

           MOVE QCL-DESC TO CL-DESC.
           MOVE QCL-GIFTNET TO CL-GIFTNET.
           MOVE QCL-PD-ID TO CL-PD-ID.
           MOVE QCL-PD-BRANCH TO CL-PD-BRANCH.
           MOVE QCL-PD-SOC-SEC TO CL-PD-SOC-SEC.
           MOVE QCL-PD-OWNBR TO CL-PD-OWNBR.
           MOVE QCL-PD-ACCTNO TO CL-PD-ACCTNO.
           MOVE QCL-PD-CLASS TO CL-PD-CLASS.
           MOVE QCL-PD-SUBCLASS TO CL-PD-SUBCLASS.
           MOVE QCL-PD-LAWCODE TO CL-PD-LAWCODE.
           MOVE QCL-PD-SSNO1 TO CL-PD-SSNO1.
           MOVE QCL-PD-MAKERCD1 TO CL-PD-MAKERCD1.
           MOVE QCL-PD-SSNO2 TO CL-PD-SSNO2.
           MOVE QCL-PD-MAKERCD2 TO CL-PD-MAKERCD2.
           MOVE QCL-PD-SSNO3 TO CL-PD-SSNO3.
           MOVE QCL-PD-MAKERCD3 TO CL-PD-MAKERCD3.
           MOVE QCL-PD-SSNO4 TO CL-PD-SSNO4.
           MOVE QCL-PD-MAKERCD4 TO CL-PD-MAKERCD4.
           MOVE QCL-PD-ORGST TO CL-PD-ORGST.
           MOVE QCL-PD-ORGBR TO CL-PD-ORGBR.
           MOVE QCL-PD-OWNST TO CL-PD-OWNST.
           MOVE QCL-PD-SPRCLASS TO CL-PD-SPRCLASS.
           MOVE QCL-PD-CONVERCD TO CL-PD-CONVERCD.
           MOVE QCL-PD-MIDSTREAM TO CL-PD-MIDSTREAM.
           MOVE QCL-PD-PURCD TO CL-PD-PURCD.
           MOVE QCL-PD-SRCD TO CL-PD-SRCD.
           MOVE QCL-PD-COLLCD TO CL-PD-COLLCD.
           MOVE QCL-PD-COLLID TO CL-PD-COLLID.
           MOVE QCL-PD-POCD TO CL-PD-POCD.
           MOVE QCL-PD-PLCD TO CL-PD-PLCD.
           MOVE QCL-PD-PLTYPE TO CL-PD-PLTYPE.
           MOVE QCL-PD-CONFIDENTCD TO CL-PD-CONFIDENTCD.
           MOVE QCL-PD-ALLOTCD TO CL-PD-ALLOTCD.
           MOVE QCL-PD-RECASTCD TO CL-PD-RECASTCD.
           MOVE QCL-PD-SKIP-TRW TO CL-PD-SKIP-TRW.
           MOVE QCL-PD-STATUSFG TO CL-PD-STATUSFG.
           MOVE QCL-PD-SMALLOAN TO CL-PD-SMALLOAN.
           MOVE QCL-PD-SALEFIN TO CL-PD-SALEFIN.
           MOVE QCL-PD-SERVACCT TO CL-PD-SERVACCT.
           MOVE QCL-PD-LOANTYPE TO CL-PD-LOANTYPE.
           MOVE QCL-PD-ADDON-FG TO CL-PD-ADDON-FG.
           MOVE QCL-PD-PAY-SCHLD-FG TO CL-PD-PAY-SCHLD-FG.
           MOVE QCL-PD-LC-CODE TO CL-PD-LC-CODE.
           MOVE QCL-PD-CAN-LOAN-NO TO CL-PD-CAN-LOAN-NO.
           MOVE QCL-PD-STATFLAGS TO CL-PD-STATFLAGS.

           IF ( QCL-PD-ENTDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-ENTDATE
           ELSE
              MOVE QCL-PD-ENTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-ENTDATE.


           IF ( QCL-PD-LOANDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-LOANDATE
           ELSE
              MOVE QCL-PD-LOANDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-LOANDATE.


           IF ( QCL-PD-PURDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-PURDATE
           ELSE
              MOVE QCL-PD-PURDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-PURDATE.


           IF ( QCL-PD-INTDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-INTDATE
           ELSE
              MOVE QCL-PD-INTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-INTDATE.


           IF ( QCL-PD-CASHDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-CASHDATE
           ELSE
              MOVE QCL-PD-CASHDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-CASHDATE.


           IF ( QCL-PD-FILEDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-FILEDATE
           ELSE
              MOVE QCL-PD-FILEDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-FILEDATE.


           IF ( QCL-PD-RESCISSION-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-RESCISSION-DATE
           ELSE
              MOVE QCL-PD-RESCISSION-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-RESCISSION-DATE.


           IF ( QCL-PD-PART-RECOURSE-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-PART-RECOURSE-DATE
           ELSE
              MOVE QCL-PD-PART-RECOURSE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-PART-RECOURSE-DATE.


           IF ( QCL-PD-ORIG-1STPYDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-ORIG-1STPYDATE
           ELSE
              MOVE QCL-PD-ORIG-1STPYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-ORIG-1STPYDATE.


           IF ( QCL-PD-1STPYDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-1STPYDATE
           ELSE
              MOVE QCL-PD-1STPYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-1STPYDATE.


           IF ( QCL-PD-JDDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-JDDATE
           ELSE
              MOVE QCL-PD-JDDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-JDDATE.


           IF ( QCL-PD-PLDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-PLDATE
           ELSE
              MOVE QCL-PD-PLDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-PLDATE.


           IF ( QCL-PD-PL-TRDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-PL-TRDATE
           ELSE
              MOVE QCL-PD-PL-TRDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-PL-TRDATE.


           IF ( QCL-PD-ACCELDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-ACCELDATE
           ELSE
              MOVE QCL-PD-ACCELDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-ACCELDATE.


           IF ( QCL-PD-REDUDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-REDUDATE
           ELSE
              MOVE QCL-PD-REDUDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-REDUDATE.


           IF ( QCL-PD-POFFDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-POFFDATE
           ELSE
              MOVE QCL-PD-POFFDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-POFFDATE.


           IF ( QCL-PD-PO-TRDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-PO-TRDATE
           ELSE
              MOVE QCL-PD-PO-TRDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-PO-TRDATE.


           IF ( QCL-PD-LEGALDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-LEGALDATE
           ELSE
              MOVE QCL-PD-LEGALDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-LEGALDATE.


           IF ( QCL-PD-BNKRPTDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-BNKRPTDATE
           ELSE
              MOVE QCL-PD-BNKRPTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-BNKRPTDATE.


           IF ( QCL-PD-BNKRPT-DISMISS-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-BNKRPT-DISMISS-DATE
           ELSE
              MOVE QCL-PD-BNKRPT-DISMISS-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-BNKRPT-DISMISS-DATE.


           IF ( QCL-PD-BNKRPT-DISCHARGE-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-BNKRPT-DISCHARGE-DATE
           ELSE
              MOVE QCL-PD-BNKRPT-DISCHARGE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-BNKRPT-DISCHARGE-DATE.


           IF ( QCL-PD-BNKRPT-REAFFIRM-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-BNKRPT-REAFFIRM-DATE
           ELSE
              MOVE QCL-PD-BNKRPT-REAFFIRM-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-BNKRPT-REAFFIRM-DATE.


           IF ( QCL-PD-DEBTAIDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-DEBTAIDATE
           ELSE
              MOVE QCL-PD-DEBTAIDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-DEBTAIDATE.


           IF ( QCL-PD-REPODATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-REPODATE
           ELSE
              MOVE QCL-PD-REPODATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-REPODATE.


           IF ( QCL-PD-REDEEMDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-REDEEMDATE
           ELSE
              MOVE QCL-PD-REDEEMDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-REDEEMDATE.


           IF ( QCL-PD-SOLDDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-SOLDDATE
           ELSE
              MOVE QCL-PD-SOLDDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-SOLDDATE.


           IF ( QCL-PD-INS-CONFIRM-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-INS-CONFIRM-DATE
           ELSE
              MOVE QCL-PD-INS-CONFIRM-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-INS-CONFIRM-DATE.


           IF ( QCL-PD-PRIOR-RN-FOR10-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-PRIOR-RN-FOR10-DATE
           ELSE
              MOVE QCL-PD-PRIOR-RN-FOR10-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-PRIOR-RN-FOR10-DATE.


           IF ( QCL-PD-LAST-RN-FOR10-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-LAST-RN-FOR10-DATE
           ELSE
              MOVE QCL-PD-LAST-RN-FOR10-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-LAST-RN-FOR10-DATE.


           IF ( QCL-PD-TRW-DOFD-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-TRW-DOFD-DATE
           ELSE
              MOVE QCL-PD-TRW-DOFD-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-TRW-DOFD-DATE.


           IF ( QCL-PD-LAST-PYPA-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-LAST-PYPA-DATE
           ELSE
              MOVE QCL-PD-LAST-PYPA-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-LAST-PYPA-DATE.


           IF ( QCL-PD-BT-TRDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-BT-TRDATE
           ELSE
              MOVE QCL-PD-BT-TRDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-BT-TRDATE.


           IF ( QCL-PD-METRO2-LAST-PMT-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-METRO2-LAST-PMT-DATE
           ELSE
              MOVE QCL-PD-METRO2-LAST-PMT-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-METRO2-LAST-PMT-DATE.


           IF ( QCL-PD-FOLLOWUP-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-FOLLOWUP-DATE
           ELSE
              MOVE QCL-PD-FOLLOWUP-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-FOLLOWUP-DATE.


           IF ( QCL-PD-LAST-ACCRUAL-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-LAST-ACCRUAL-DATE
           ELSE
              MOVE QCL-PD-LAST-ACCRUAL-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-LAST-ACCRUAL-DATE.

           MOVE QCL-PD-APRATE TO CL-PD-APRATE.
           MOVE QCL-PD-EFFRATE TO CL-PD-EFFRATE.
           MOVE QCL-PD-SMPRATE TO CL-PD-SMPRATE.
           MOVE QCL-PD-YLDRATE TO CL-PD-YLDRATE.
           MOVE QCL-PD-OVRDRATE TO CL-PD-OVRDRATE.
           MOVE QCL-PD-ENDORATE TO CL-PD-ENDORATE.
           MOVE QCL-PD-JDRATE TO CL-PD-JDRATE.
           MOVE QCL-PD-ACCRATE TO CL-PD-ACCRATE.
           MOVE QCL-PD-REDURATE TO CL-PD-REDURATE.
           MOVE QCL-PD-CONRATE TO CL-PD-CONRATE.
           MOVE QCL-PD-SERVPER TO CL-PD-SERVPER.
           MOVE QCL-PD-SELLPR TO CL-PD-SELLPR.
           MOVE QCL-PD-DOWNPY TO CL-PD-DOWNPY.
           MOVE QCL-PD-TRADEIN TO CL-PD-TRADEIN.
           MOVE QCL-PD-LNAMT TO CL-PD-LNAMT.
           MOVE QCL-PD-FINCHG TO CL-PD-FINCHG.
           MOVE QCL-PD-INTCHG TO CL-PD-INTCHG.
           MOVE QCL-PD-SERCHG TO CL-PD-SERCHG.
           MOVE QCL-PD-EXTCHG TO CL-PD-EXTCHG.
           MOVE QCL-PD-MAINTFEE TO CL-PD-MAINTFEE.
           MOVE QCL-PD-POINTS TO CL-PD-POINTS.
           MOVE QCL-PD-PROCEEDS TO CL-PD-PROCEEDS.
           MOVE QCL-PD-ORGTERM TO CL-PD-ORGTERM.
           MOVE QCL-PD-1STPYAMT TO CL-PD-1STPYAMT.
           MOVE QCL-PD-NOREGPY TO CL-PD-NOREGPY.
           MOVE QCL-PD-REGPYAMT TO CL-PD-REGPYAMT.
           MOVE QCL-PD-LASTPYAMT TO CL-PD-LASTPYAMT.
           MOVE QCL-PD-PROCASH TO CL-PD-PROCASH.
           MOVE QCL-PD-TILLNO TO CL-PD-TILLNO.
           MOVE QCL-PD-STATE-FLAG TO CL-PD-STATE-FLAG.
           MOVE QCL-PD-STATE-WORKER TO CL-PD-STATE-WORKER.
           MOVE QCL-PD-STATE-WORKER2 TO CL-PD-STATE-WORKER2.
           MOVE QCL-PD-FEECODE-1 TO CL-PD-FEECODE(1).
           MOVE QCL-PD-FEEAMT-1 TO CL-PD-FEEAMT(1).
           MOVE QCL-PD-FEEOURS-1 TO CL-PD-FEEOURS(1).
           MOVE QCL-PD-FEEFINCHG-1 TO CL-PD-FEEFINCHG(1).
           MOVE QCL-PD-FEECODE-2 TO CL-PD-FEECODE(2).
           MOVE QCL-PD-FEEAMT-2 TO CL-PD-FEEAMT(2).
           MOVE QCL-PD-FEEOURS-2 TO CL-PD-FEEOURS(2).
           MOVE QCL-PD-FEEFINCHG-2 TO CL-PD-FEEFINCHG(2).
           MOVE QCL-PD-FEECODE-3 TO CL-PD-FEECODE(3).
           MOVE QCL-PD-FEEAMT-3 TO CL-PD-FEEAMT(3).
           MOVE QCL-PD-FEEOURS-3 TO CL-PD-FEEOURS(3).
           MOVE QCL-PD-FEEFINCHG-3 TO CL-PD-FEEFINCHG(3).
           MOVE QCL-PD-FEECODE-4 TO CL-PD-FEECODE(4).
           MOVE QCL-PD-FEEAMT-4 TO CL-PD-FEEAMT(4).
           MOVE QCL-PD-FEEOURS-4 TO CL-PD-FEEOURS(4).
           MOVE QCL-PD-FEEFINCHG-4 TO CL-PD-FEEFINCHG(4).
           MOVE QCL-PD-FEECODE-5 TO CL-PD-FEECODE(5).
           MOVE QCL-PD-FEEAMT-5 TO CL-PD-FEEAMT(5).
           MOVE QCL-PD-FEEOURS-5 TO CL-PD-FEEOURS(5).
           MOVE QCL-PD-FEEFINCHG-5 TO CL-PD-FEEFINCHG(5).
           MOVE QCL-PD-FEECODE-6 TO CL-PD-FEECODE(6).
           MOVE QCL-PD-FEEAMT-6 TO CL-PD-FEEAMT(6).
           MOVE QCL-PD-FEEOURS-6 TO CL-PD-FEEOURS(6).
           MOVE QCL-PD-FEEFINCHG-6 TO CL-PD-FEEFINCHG(6).
           MOVE QCL-PD-FEECODE-7 TO CL-PD-FEECODE(7).
           MOVE QCL-PD-FEEAMT-7 TO CL-PD-FEEAMT(7).
           MOVE QCL-PD-FEEOURS-7 TO CL-PD-FEEOURS(7).
           MOVE QCL-PD-FEEFINCHG-7 TO CL-PD-FEEFINCHG(7).
           MOVE QCL-PD-FEECODE-8 TO CL-PD-FEECODE(8).
           MOVE QCL-PD-FEEAMT-8 TO CL-PD-FEEAMT(8).
           MOVE QCL-PD-FEEOURS-8 TO CL-PD-FEEOURS(8).
           MOVE QCL-PD-FEEFINCHG-8 TO CL-PD-FEEFINCHG(8).
           MOVE QCL-PD-FEECODE-9 TO CL-PD-FEECODE(9).
           MOVE QCL-PD-FEEAMT-9 TO CL-PD-FEEAMT(9).
           MOVE QCL-PD-FEEOURS-9 TO CL-PD-FEEOURS(9).
           MOVE QCL-PD-FEEFINCHG-9 TO CL-PD-FEEFINCHG(9).
           MOVE QCL-PD-FEECODE-10 TO CL-PD-FEECODE(10).
           MOVE QCL-PD-FEEAMT-10 TO CL-PD-FEEAMT(10).
           MOVE QCL-PD-FEEOURS-10 TO CL-PD-FEEOURS(10).
           MOVE QCL-PD-FEEFINCHG-10 TO CL-PD-FEEFINCHG(10).
           MOVE QCL-PD-FEECODE-11 TO CL-PD-FEECODE(11).
           MOVE QCL-PD-FEEAMT-11 TO CL-PD-FEEAMT(11).
           MOVE QCL-PD-FEEOURS-11 TO CL-PD-FEEOURS(11).
           MOVE QCL-PD-FEEFINCHG-11 TO CL-PD-FEEFINCHG(11).
           MOVE QCL-PD-FEECODE-12 TO CL-PD-FEECODE(12).
           MOVE QCL-PD-FEEAMT-12 TO CL-PD-FEEAMT(12).
           MOVE QCL-PD-FEEOURS-12 TO CL-PD-FEEOURS(12).
           MOVE QCL-PD-FEEFINCHG-12 TO CL-PD-FEEFINCHG(12).
           MOVE QCL-PD-FEECODE-13 TO CL-PD-FEECODE(13).
           MOVE QCL-PD-FEEAMT-13 TO CL-PD-FEEAMT(13).
           MOVE QCL-PD-FEEOURS-13 TO CL-PD-FEEOURS(13).
           MOVE QCL-PD-FEEFINCHG-13 TO CL-PD-FEEFINCHG(13).
           MOVE QCL-PD-FEECODE-14 TO CL-PD-FEECODE(14).
           MOVE QCL-PD-FEEAMT-14 TO CL-PD-FEEAMT(14).
           MOVE QCL-PD-FEEOURS-14 TO CL-PD-FEEOURS(14).
           MOVE QCL-PD-FEEFINCHG-14 TO CL-PD-FEEFINCHG(14).
           MOVE QCL-PD-FEECODE-15 TO CL-PD-FEECODE(15).
           MOVE QCL-PD-FEEAMT-15 TO CL-PD-FEEAMT(15).
           MOVE QCL-PD-FEEOURS-15 TO CL-PD-FEEOURS(15).
           MOVE QCL-PD-FEEFINCHG-15 TO CL-PD-FEEFINCHG(15).
           MOVE QCL-PD-FEECODE-16 TO CL-PD-FEECODE(16).
           MOVE QCL-PD-FEEAMT-16 TO CL-PD-FEEAMT(16).
           MOVE QCL-PD-FEEOURS-16 TO CL-PD-FEEOURS(16).
           MOVE QCL-PD-FEEFINCHG-16 TO CL-PD-FEEFINCHG(16).
           MOVE QCL-PD-FEECODE-17 TO CL-PD-FEECODE(17).
           MOVE QCL-PD-FEEAMT-17 TO CL-PD-FEEAMT(17).
           MOVE QCL-PD-FEEOURS-17 TO CL-PD-FEEOURS(17).
           MOVE QCL-PD-FEEFINCHG-17 TO CL-PD-FEEFINCHG(17).
           MOVE QCL-PD-FEECODE-18 TO CL-PD-FEECODE(18).
           MOVE QCL-PD-FEEAMT-18 TO CL-PD-FEEAMT(18).
           MOVE QCL-PD-FEEOURS-18 TO CL-PD-FEEOURS(18).
           MOVE QCL-PD-FEEFINCHG-18 TO CL-PD-FEEFINCHG(18).
           MOVE QCL-PD-FEECODE-19 TO CL-PD-FEECODE(19).
           MOVE QCL-PD-FEEAMT-19 TO CL-PD-FEEAMT(19).
           MOVE QCL-PD-FEEOURS-19 TO CL-PD-FEEOURS(19).
           MOVE QCL-PD-FEEFINCHG-19 TO CL-PD-FEEFINCHG(19).
           MOVE QCL-PD-FEECODE-20 TO CL-PD-FEECODE(20).
           MOVE QCL-PD-FEEAMT-20 TO CL-PD-FEEAMT(20).
           MOVE QCL-PD-FEEOURS-20 TO CL-PD-FEEOURS(20).
           MOVE QCL-PD-FEEFINCHG-20 TO CL-PD-FEEFINCHG(20).
           MOVE QCL-PD-INSURED TO CL-PD-INSURED.
           MOVE QCL-PD-INSTYPES TO CL-PD-INSTYPES.
           MOVE QCL-PD-INSELIGIBLE TO CL-PD-INSELIGIBLE.
           MOVE QCL-PD-INSCOMP-1 TO CL-PD-INSCOMP(1).
           MOVE QCL-PD-INSOURS-1 TO CL-PD-INSOURS(1).

           MOVE QCL-PD-INSEFF-DATE-1 TO CL-PD-INSEFF-DATE(1).

           IF ( QCL-PD-INSEXP-DATE-1 = "1900-01-01" )
              MOVE ZEROES TO CL-PD-INSEXP-DATE(1)
           ELSE
              MOVE QCL-PD-INSEXP-DATE-1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-INSEXP-DATE(1).

           MOVE QCL-PD-INSPREM-1 TO CL-PD-INSPREM(1).
           MOVE QCL-PD-INSCOVR-1 TO CL-PD-INSCOVR(1).
           MOVE QCL-PD-INSCOMM-1 TO CL-PD-INSCOMM(1).
           MOVE QCL-PD-INSANTICERN-1 TO CL-PD-INSANTICERN(1).
           MOVE QCL-PD-INSANTICADJ-1 TO CL-PD-INSANTICADJ(1).
           MOVE QCL-PD-INSUFERN-1 TO CL-PD-INSUFERN(1).
           MOVE QCL-PD-INSACCERN-1 TO CL-PD-INSACCERN(1).
           MOVE QCL-PD-INSEARN-ORDER-1 TO CL-PD-INSEARN-ORDER(1).
           MOVE QCL-PD-INSFINCHG-1 TO CL-PD-INSFINCHG(1).
           MOVE QCL-PD-INSCOMP-2 TO CL-PD-INSCOMP(2).
           MOVE QCL-PD-INSOURS-2 TO CL-PD-INSOURS(2).

           MOVE QCL-PD-INSEFF-DATE-2 TO CL-PD-INSEFF-DATE(2).

           IF ( QCL-PD-INSEXP-DATE-2 = "1900-01-01" )
              MOVE ZEROES TO CL-PD-INSEXP-DATE(2)
           ELSE
              MOVE QCL-PD-INSEXP-DATE-2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-INSEXP-DATE(2).

           MOVE QCL-PD-INSPREM-2 TO CL-PD-INSPREM(2).
           MOVE QCL-PD-INSCOVR-2 TO CL-PD-INSCOVR(2).
           MOVE QCL-PD-INSCOMM-2 TO CL-PD-INSCOMM(2).
           MOVE QCL-PD-INSANTICERN-2 TO CL-PD-INSANTICERN(2).
           MOVE QCL-PD-INSANTICADJ-2 TO CL-PD-INSANTICADJ(2).
           MOVE QCL-PD-INSUFERN-2 TO CL-PD-INSUFERN(2).
           MOVE QCL-PD-INSACCERN-2 TO CL-PD-INSACCERN(2).
           MOVE QCL-PD-INSEARN-ORDER-2 TO CL-PD-INSEARN-ORDER(2).
           MOVE QCL-PD-INSFINCHG-2 TO CL-PD-INSFINCHG(2).
           MOVE QCL-PD-INSCOMP-3 TO CL-PD-INSCOMP(3).
           MOVE QCL-PD-INSOURS-3 TO CL-PD-INSOURS(3).

           MOVE QCL-PD-INSEFF-DATE-3 TO CL-PD-INSEFF-DATE(3).

           IF ( QCL-PD-INSEXP-DATE-3 = "1900-01-01" )
              MOVE ZEROES TO CL-PD-INSEXP-DATE(3)
           ELSE
              MOVE QCL-PD-INSEXP-DATE-3 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-INSEXP-DATE(3).

           MOVE QCL-PD-INSPREM-3 TO CL-PD-INSPREM(3).
           MOVE QCL-PD-INSCOVR-3 TO CL-PD-INSCOVR(3).
           MOVE QCL-PD-INSCOMM-3 TO CL-PD-INSCOMM(3).
           MOVE QCL-PD-INSANTICERN-3 TO CL-PD-INSANTICERN(3).
           MOVE QCL-PD-INSANTICADJ-3 TO CL-PD-INSANTICADJ(3).
           MOVE QCL-PD-INSUFERN-3 TO CL-PD-INSUFERN(3).
           MOVE QCL-PD-INSACCERN-3 TO CL-PD-INSACCERN(3).
           MOVE QCL-PD-INSEARN-ORDER-3 TO CL-PD-INSEARN-ORDER(3).
           MOVE QCL-PD-INSFINCHG-3 TO CL-PD-INSFINCHG(3).
           MOVE QCL-PD-INSCOMP-4 TO CL-PD-INSCOMP(4).
           MOVE QCL-PD-INSOURS-4 TO CL-PD-INSOURS(4).

           MOVE QCL-PD-INSEFF-DATE-4 TO CL-PD-INSEFF-DATE(4).

           IF ( QCL-PD-INSEXP-DATE-4 = "1900-01-01" )
              MOVE ZEROES TO CL-PD-INSEXP-DATE(4)
           ELSE
              MOVE QCL-PD-INSEXP-DATE-4 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-INSEXP-DATE(4).

           MOVE QCL-PD-INSPREM-4 TO CL-PD-INSPREM(4).
           MOVE QCL-PD-INSCOVR-4 TO CL-PD-INSCOVR(4).
           MOVE QCL-PD-INSCOMM-4 TO CL-PD-INSCOMM(4).
           MOVE QCL-PD-INSANTICERN-4 TO CL-PD-INSANTICERN(4).
           MOVE QCL-PD-INSANTICADJ-4 TO CL-PD-INSANTICADJ(4).
           MOVE QCL-PD-INSUFERN-4 TO CL-PD-INSUFERN(4).
           MOVE QCL-PD-INSACCERN-4 TO CL-PD-INSACCERN(4).
           MOVE QCL-PD-INSEARN-ORDER-4 TO CL-PD-INSEARN-ORDER(4).
           MOVE QCL-PD-INSFINCHG-4 TO CL-PD-INSFINCHG(4).
           MOVE QCL-PD-INSCOMP-5 TO CL-PD-INSCOMP(5).
           MOVE QCL-PD-INSOURS-5 TO CL-PD-INSOURS(5).

           MOVE QCL-PD-INSEFF-DATE-5 TO CL-PD-INSEFF-DATE(5).

           IF ( QCL-PD-INSEXP-DATE-5 = "1900-01-01" )
              MOVE ZEROES TO CL-PD-INSEXP-DATE(5)
           ELSE
              MOVE QCL-PD-INSEXP-DATE-5 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-INSEXP-DATE(5).

           MOVE QCL-PD-INSPREM-5 TO CL-PD-INSPREM(5).
           MOVE QCL-PD-INSCOVR-5 TO CL-PD-INSCOVR(5).
           MOVE QCL-PD-INSCOMM-5 TO CL-PD-INSCOMM(5).
           MOVE QCL-PD-INSANTICERN-5 TO CL-PD-INSANTICERN(5).
           MOVE QCL-PD-INSANTICADJ-5 TO CL-PD-INSANTICADJ(5).
           MOVE QCL-PD-INSUFERN-5 TO CL-PD-INSUFERN(5).
           MOVE QCL-PD-INSACCERN-5 TO CL-PD-INSACCERN(5).
           MOVE QCL-PD-INSEARN-ORDER-5 TO CL-PD-INSEARN-ORDER(5).
           MOVE QCL-PD-INSFINCHG-5 TO CL-PD-INSFINCHG(5).
           MOVE QCL-PD-INSCOMP-6 TO CL-PD-INSCOMP(6).
           MOVE QCL-PD-INSOURS-6 TO CL-PD-INSOURS(6).

           MOVE QCL-PD-INSEFF-DATE-6 TO CL-PD-INSEFF-DATE(6).

           IF ( QCL-PD-INSEXP-DATE-6 = "1900-01-01" )
              MOVE ZEROES TO CL-PD-INSEXP-DATE(6)
           ELSE
              MOVE QCL-PD-INSEXP-DATE-6 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-INSEXP-DATE(6).

           MOVE QCL-PD-INSPREM-6 TO CL-PD-INSPREM(6).
           MOVE QCL-PD-INSCOVR-6 TO CL-PD-INSCOVR(6).
           MOVE QCL-PD-INSCOMM-6 TO CL-PD-INSCOMM(6).
           MOVE QCL-PD-INSANTICERN-6 TO CL-PD-INSANTICERN(6).
           MOVE QCL-PD-INSANTICADJ-6 TO CL-PD-INSANTICADJ(6).
           MOVE QCL-PD-INSUFERN-6 TO CL-PD-INSUFERN(6).
           MOVE QCL-PD-INSACCERN-6 TO CL-PD-INSACCERN(6).
           MOVE QCL-PD-INSEARN-ORDER-6 TO CL-PD-INSEARN-ORDER(6).
           MOVE QCL-PD-INSFINCHG-6 TO CL-PD-INSFINCHG(6).
           MOVE QCL-PD-INSCOMP-7 TO CL-PD-INSCOMP(7).
           MOVE QCL-PD-INSOURS-7 TO CL-PD-INSOURS(7).

           MOVE QCL-PD-INSEFF-DATE-7 TO CL-PD-INSEFF-DATE(7).

           IF ( QCL-PD-INSEXP-DATE-7 = "1900-01-01" )
              MOVE ZEROES TO CL-PD-INSEXP-DATE(7)
           ELSE
              MOVE QCL-PD-INSEXP-DATE-7 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-INSEXP-DATE(7).

           MOVE QCL-PD-INSPREM-7 TO CL-PD-INSPREM(7).
           MOVE QCL-PD-INSCOVR-7 TO CL-PD-INSCOVR(7).
           MOVE QCL-PD-INSCOMM-7 TO CL-PD-INSCOMM(7).
           MOVE QCL-PD-INSANTICERN-7 TO CL-PD-INSANTICERN(7).
           MOVE QCL-PD-INSANTICADJ-7 TO CL-PD-INSANTICADJ(7).
           MOVE QCL-PD-INSUFERN-7 TO CL-PD-INSUFERN(7).
           MOVE QCL-PD-INSACCERN-7 TO CL-PD-INSACCERN(7).
           MOVE QCL-PD-INSEARN-ORDER-7 TO CL-PD-INSEARN-ORDER(7).
           MOVE QCL-PD-INSFINCHG-7 TO CL-PD-INSFINCHG(7).
           MOVE QCL-PD-INSCOMP-8 TO CL-PD-INSCOMP(8).
           MOVE QCL-PD-INSOURS-8 TO CL-PD-INSOURS(8).

           MOVE QCL-PD-INSEFF-DATE-8 TO CL-PD-INSEFF-DATE(8).

           IF ( QCL-PD-INSEXP-DATE-8 = "1900-01-01" )
              MOVE ZEROES TO CL-PD-INSEXP-DATE(8)
           ELSE
              MOVE QCL-PD-INSEXP-DATE-8 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-INSEXP-DATE(8).

           MOVE QCL-PD-INSPREM-8 TO CL-PD-INSPREM(8).
           MOVE QCL-PD-INSCOVR-8 TO CL-PD-INSCOVR(8).
           MOVE QCL-PD-INSCOMM-8 TO CL-PD-INSCOMM(8).
           MOVE QCL-PD-INSANTICERN-8 TO CL-PD-INSANTICERN(8).
           MOVE QCL-PD-INSANTICADJ-8 TO CL-PD-INSANTICADJ(8).
           MOVE QCL-PD-INSUFERN-8 TO CL-PD-INSUFERN(8).
           MOVE QCL-PD-INSACCERN-8 TO CL-PD-INSACCERN(8).
           MOVE QCL-PD-INSEARN-ORDER-8 TO CL-PD-INSEARN-ORDER(8).
           MOVE QCL-PD-INSFINCHG-8 TO CL-PD-INSFINCHG(8).
           MOVE QCL-PD-INSCOMP-9 TO CL-PD-INSCOMP(9).
           MOVE QCL-PD-INSOURS-9 TO CL-PD-INSOURS(9).

           MOVE QCL-PD-INSEFF-DATE-9 TO CL-PD-INSEFF-DATE(9).

           IF ( QCL-PD-INSEXP-DATE-9 = "1900-01-01" )
              MOVE ZEROES TO CL-PD-INSEXP-DATE(9)
           ELSE
              MOVE QCL-PD-INSEXP-DATE-9 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-INSEXP-DATE(9).

           MOVE QCL-PD-INSPREM-9 TO CL-PD-INSPREM(9).
           MOVE QCL-PD-INSCOVR-9 TO CL-PD-INSCOVR(9).
           MOVE QCL-PD-INSCOMM-9 TO CL-PD-INSCOMM(9).
           MOVE QCL-PD-INSANTICERN-9 TO CL-PD-INSANTICERN(9).
           MOVE QCL-PD-INSANTICADJ-9 TO CL-PD-INSANTICADJ(9).
           MOVE QCL-PD-INSUFERN-9 TO CL-PD-INSUFERN(9).
           MOVE QCL-PD-INSACCERN-9 TO CL-PD-INSACCERN(9).
           MOVE QCL-PD-INSEARN-ORDER-9 TO CL-PD-INSEARN-ORDER(9).
           MOVE QCL-PD-INSFINCHG-9 TO CL-PD-INSFINCHG(9).
           MOVE QCL-PD-INSCOMP-10 TO CL-PD-INSCOMP(10).
           MOVE QCL-PD-INSOURS-10 TO CL-PD-INSOURS(10).

           MOVE QCL-PD-INSEFF-DATE-10 TO CL-PD-INSEFF-DATE(10).

           IF ( QCL-PD-INSEXP-DATE-10 = "1900-01-01" )
              MOVE ZEROES TO CL-PD-INSEXP-DATE(10)
           ELSE
              MOVE QCL-PD-INSEXP-DATE-10 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-INSEXP-DATE(10).

           MOVE QCL-PD-INSPREM-10 TO CL-PD-INSPREM(10).
           MOVE QCL-PD-INSCOVR-10 TO CL-PD-INSCOVR(10).
           MOVE QCL-PD-INSCOMM-10 TO CL-PD-INSCOMM(10).
           MOVE QCL-PD-INSANTICERN-10 TO CL-PD-INSANTICERN(10).
           MOVE QCL-PD-INSANTICADJ-10 TO CL-PD-INSANTICADJ(10).
           MOVE QCL-PD-INSUFERN-10 TO CL-PD-INSUFERN(10).
           MOVE QCL-PD-INSACCERN-10 TO CL-PD-INSACCERN(10).
           MOVE QCL-PD-INSEARN-ORDER-10 TO CL-PD-INSEARN-ORDER(10).
           MOVE QCL-PD-INSFINCHG-10 TO CL-PD-INSFINCHG(10).
           MOVE QCL-PD-PY-YYYYMM-RECDEL TO CL-PD-PY-YYYYMM-RECDEL.
           MOVE QCL-PD-MTD-PAYS-RECDEL TO CL-PD-MTD-PAYS-RECDEL.
           MOVE QCL-PD-GARNISH-EXTR TO CL-PD-GARNISH-EXTR.
           MOVE QCL-PD-SHIP-SAME-FG TO CL-PD-SHIP-SAME-FG.
           MOVE QCL-PD-AD-D-INS-COVR TO CL-PD-AD-D-INS-COVR.
           MOVE QCL-PD-TOT-PARTIAL-CHGOFF TO CL-PD-TOT-PARTIAL-CHGOFF.
           MOVE QCL-PD-LPO TO CL-PD-LPO.
           MOVE QCL-PD-REP-CD TO CL-PD-REP-CD.
           MOVE QCL-PD-EMP-ID TO CL-PD-EMP-ID.
           MOVE QCL-PD-PROCESS-CD TO CL-PD-PROCESS-CD.
           MOVE QCL-PD-WCBC-CREDITLIM TO CL-PD-WCBC-CREDITLIM.
           MOVE QCL-PD-FOLLOWUP-CD TO CL-PD-FOLLOWUP-CD.
           MOVE QCL-PD-MIP-FG TO CL-PD-MIP-FG.
           MOVE QCL-PD-MIP-MTD-CLAHPAID TO CL-PD-MIP-MTD-CLAHPAID.

           IF ( QCL-PD-MIP-CLPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-MIP-CLPDTH-DATE
           ELSE
              MOVE QCL-PD-MIP-CLPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-MIP-CLPDTH-DATE.

           MOVE QCL-PD-MIP-TOT-CLPAID TO CL-PD-MIP-TOT-CLPAID.

           IF ( QCL-PD-MIP-AHPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-MIP-AHPDTH-DATE
           ELSE
              MOVE QCL-PD-MIP-AHPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-MIP-AHPDTH-DATE.

           MOVE QCL-PD-MIP-TOT-AHPAID TO CL-PD-MIP-TOT-AHPAID.
           MOVE QCL-PD-IL-GROSS-INCOME TO CL-PD-IL-GROSS-INCOME.
           MOVE QCL-PD-IL-GROSS-INCOME2 TO CL-PD-IL-GROSS-INCOME2.
           MOVE QCL-PD-RESCISSION TO CL-PD-RESCISSION.
           MOVE QCL-PD-INTERIM-TRW TO CL-PD-INTERIM-TRW.
           MOVE QCL-PD-DLNO TO CL-PD-DLNO.
           MOVE QCL-PD-DLPOOL TO CL-PD-DLPOOL.
           MOVE QCL-PD-DLRECOURSE TO CL-PD-DLRECOURSE.
           MOVE QCL-PD-DLPARTRCAMT TO CL-PD-DLPARTRCAMT.
           MOVE QCL-PD-ORIGPURAMT TO CL-PD-ORIGPURAMT.
           MOVE QCL-PD-PURBAL TO CL-PD-PURBAL.
           MOVE QCL-PD-DLRVPRIN-1 TO CL-PD-DLRVPRIN(1).
           MOVE QCL-PD-DLRVPRIN-2 TO CL-PD-DLRVPRIN(2).
           MOVE QCL-PD-DLRVPRIN-3 TO CL-PD-DLRVPRIN(3).
           MOVE QCL-PD-DLRVCHGS-1 TO CL-PD-DLRVCHGS(1).
           MOVE QCL-PD-DLRVCHGS-2 TO CL-PD-DLRVCHGS(2).
           MOVE QCL-PD-DLRVCHGS-3 TO CL-PD-DLRVCHGS(3).
           MOVE QCL-PD-DLPARVHELD TO CL-PD-DLPARVHELD.
           MOVE QCL-PD-DLPARVPAID TO CL-PD-DLPARVPAID.
           MOVE QCL-PD-DLDISC TO CL-PD-DLDISC.

           IF ( QCL-PD-LCPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-LCPDTH-DATE
           ELSE
              MOVE QCL-PD-LCPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-LCPDTH-DATE.


           IF ( QCL-PD-INTPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-INTPDTH-DATE
           ELSE
              MOVE QCL-PD-INTPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-INTPDTH-DATE.

           MOVE QCL-PD-CURBAL TO CL-PD-CURBAL.
           MOVE QCL-PD-INTBAL TO CL-PD-INTBAL.
           MOVE QCL-PD-LCBAL TO CL-PD-LCBAL.
           MOVE QCL-PD-OTHBAL TO CL-PD-OTHBAL.
           MOVE QCL-PD-OT2BAL TO CL-PD-OT2BAL.
           MOVE QCL-PD-LCPAID TO CL-PD-LCPAID.
           MOVE QCL-PD-LCPARTIALS TO CL-PD-LCPARTIALS.
           MOVE QCL-PD-STMT-COUNTER TO CL-PD-STMT-COUNTER.
           MOVE QCL-PD-PRIOR-STMT-COUNTER TO CL-PD-PRIOR-STMT-COUNTER.
           MOVE QCL-PD-FROZEN-REASON TO CL-PD-FROZEN-REASON.
           MOVE QCL-PD-COUPON-STATEMENT TO CL-PD-COUPON-STATEMENT.
           MOVE QCL-PD-MONTHEND-BAL TO CL-PD-MONTHEND-BAL.
           MOVE QCL-PD-ANTICTERM TO CL-PD-ANTICTERM.
           MOVE QCL-PD-ANTICERN-1 TO CL-PD-ANTICERN(1).
           MOVE QCL-PD-ANTICADJ-1 TO CL-PD-ANTICADJ(1).
           MOVE QCL-PD-UFERN-1 TO CL-PD-UFERN(1).
           MOVE QCL-PD-ACCERN-1 TO CL-PD-ACCERN(1).
           MOVE QCL-PD-EARN-ORDER-1 TO CL-PD-EARN-ORDER(1).
           MOVE QCL-PD-ANTICERN-2 TO CL-PD-ANTICERN(2).
           MOVE QCL-PD-ANTICADJ-2 TO CL-PD-ANTICADJ(2).
           MOVE QCL-PD-UFERN-2 TO CL-PD-UFERN(2).
           MOVE QCL-PD-ACCERN-2 TO CL-PD-ACCERN(2).
           MOVE QCL-PD-EARN-ORDER-2 TO CL-PD-EARN-ORDER(2).
           MOVE QCL-PD-ANTICERN-3 TO CL-PD-ANTICERN(3).
           MOVE QCL-PD-ANTICADJ-3 TO CL-PD-ANTICADJ(3).
           MOVE QCL-PD-UFERN-3 TO CL-PD-UFERN(3).
           MOVE QCL-PD-ACCERN-3 TO CL-PD-ACCERN(3).
           MOVE QCL-PD-EARN-ORDER-3 TO CL-PD-EARN-ORDER(3).
           MOVE QCL-PD-ANTICERN-4 TO CL-PD-ANTICERN(4).
           MOVE QCL-PD-ANTICADJ-4 TO CL-PD-ANTICADJ(4).
           MOVE QCL-PD-UFERN-4 TO CL-PD-UFERN(4).
           MOVE QCL-PD-ACCERN-4 TO CL-PD-ACCERN(4).
           MOVE QCL-PD-EARN-ORDER-4 TO CL-PD-EARN-ORDER(4).
           MOVE QCL-PD-ANTICERN-5 TO CL-PD-ANTICERN(5).
           MOVE QCL-PD-ANTICADJ-5 TO CL-PD-ANTICADJ(5).
           MOVE QCL-PD-UFERN-5 TO CL-PD-UFERN(5).
           MOVE QCL-PD-ACCERN-5 TO CL-PD-ACCERN(5).
           MOVE QCL-PD-EARN-ORDER-5 TO CL-PD-EARN-ORDER(5).
           MOVE QCL-PD-ANTICERN-6 TO CL-PD-ANTICERN(6).
           MOVE QCL-PD-ANTICADJ-6 TO CL-PD-ANTICADJ(6).
           MOVE QCL-PD-UFERN-6 TO CL-PD-UFERN(6).
           MOVE QCL-PD-ACCERN-6 TO CL-PD-ACCERN(6).
           MOVE QCL-PD-EARN-ORDER-6 TO CL-PD-EARN-ORDER(6).
           MOVE QCL-PD-ANTICERN-7 TO CL-PD-ANTICERN(7).
           MOVE QCL-PD-ANTICADJ-7 TO CL-PD-ANTICADJ(7).
           MOVE QCL-PD-UFERN-7 TO CL-PD-UFERN(7).
           MOVE QCL-PD-ACCERN-7 TO CL-PD-ACCERN(7).
           MOVE QCL-PD-EARN-ORDER-7 TO CL-PD-EARN-ORDER(7).
           MOVE QCL-PD-ANTICERN-8 TO CL-PD-ANTICERN(8).
           MOVE QCL-PD-ANTICADJ-8 TO CL-PD-ANTICADJ(8).
           MOVE QCL-PD-UFERN-8 TO CL-PD-UFERN(8).
           MOVE QCL-PD-ACCERN-8 TO CL-PD-ACCERN(8).
           MOVE QCL-PD-EARN-ORDER-8 TO CL-PD-EARN-ORDER(8).
           MOVE QCL-PD-ANTICERN-9 TO CL-PD-ANTICERN(9).
           MOVE QCL-PD-ANTICADJ-9 TO CL-PD-ANTICADJ(9).
           MOVE QCL-PD-UFERN-9 TO CL-PD-UFERN(9).
           MOVE QCL-PD-ACCERN-9 TO CL-PD-ACCERN(9).
           MOVE QCL-PD-EARN-ORDER-9 TO CL-PD-EARN-ORDER(9).
           MOVE QCL-PD-ANTICERN-10 TO CL-PD-ANTICERN(10).
           MOVE QCL-PD-ANTICADJ-10 TO CL-PD-ANTICADJ(10).
           MOVE QCL-PD-UFERN-10 TO CL-PD-UFERN(10).
           MOVE QCL-PD-ACCERN-10 TO CL-PD-ACCERN(10).
           MOVE QCL-PD-EARN-ORDER-10 TO CL-PD-EARN-ORDER(10).
           MOVE QCL-PD-LMO-PRIOR-INTEREST TO CL-PD-LMO-PRIOR-INTEREST.
           MOVE QCL-PD-PRIOR-UNEARNED TO CL-PD-PRIOR-UNEARNED.
           MOVE QCL-PD-ERNDELQ-STOP TO CL-PD-ERNDELQ-STOP.
           MOVE QCL-PD-NONACRERN TO CL-PD-NONACRERN.
           MOVE QCL-PD-PRIOR-NONACRERN TO CL-PD-PRIOR-NONACRERN.

           IF ( QCL-PD-ERN-IB-PDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-ERN-IB-PDTH-DATE
           ELSE
              MOVE QCL-PD-ERN-IB-PDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-ERN-IB-PDTH-DATE.

           MOVE QCL-PD-ERN-IB-PRINBAL TO CL-PD-ERN-IB-PRINBAL.
           MOVE QCL-PD-ERN-IB-INTOWE TO CL-PD-ERN-IB-INTOWE.
           MOVE QCL-PD-TOTERN TO CL-PD-TOTERN.
           MOVE QCL-PD-TOTOTH TO CL-PD-TOTOTH.
           MOVE QCL-PD-TOTINT TO CL-PD-TOTINT.
           MOVE QCL-PD-TOTLCHG TO CL-PD-TOTLCHG.
           MOVE QCL-PD-TOTDEF TO CL-PD-TOTDEF.
           MOVE QCL-PD-TOTSERACCT TO CL-PD-TOTSERACCT.
           MOVE QCL-PD-LCWAIVERS TO CL-PD-LCWAIVERS.
           MOVE QCL-PD-INTWAIVERS TO CL-PD-INTWAIVERS.
           MOVE QCL-PD-TOTPAYMNTD TO CL-PD-TOTPAYMNTD.
           MOVE QCL-PD-TOTEXCPAYMNTD TO CL-PD-TOTEXCPAYMNTD.
           MOVE QCL-PD-TOTNODEF TO CL-PD-TOTNODEF.
           MOVE QCL-PD-YTDNODEF TO CL-PD-YTDNODEF.
           MOVE QCL-PD-TOTNODEFACT TO CL-PD-TOTNODEFACT.
           MOVE QCL-PD-YTDNODEFACT TO CL-PD-YTDNODEFACT.
           MOVE QCL-PD-TOTNOALDEL TO CL-PD-TOTNOALDEL.
           MOVE QCL-PD-YTDNOALDEL TO CL-PD-YTDNOALDEL.
           MOVE QCL-PD-YTDINTPD TO CL-PD-YTDINTPD.
           MOVE QCL-PD-LYRINTPD TO CL-PD-LYRINTPD.
           MOVE QCL-PD-TOTINTPD TO CL-PD-TOTINTPD.
           MOVE QCL-PD-RECASTINT TO CL-PD-RECASTINT.
           MOVE QCL-PD-RECASTPRIN TO CL-PD-RECASTPRIN.
           MOVE QCL-PD-CL-REBATE TO CL-PD-CL-REBATE.
           MOVE QCL-PD-AH-REBATE TO CL-PD-AH-REBATE.
           MOVE QCL-PD-PP-REBATE TO CL-PD-PP-REBATE.
           MOVE QCL-PD-INT-REBATE TO CL-PD-INT-REBATE.
           MOVE QCL-PD-SC-REBATE TO CL-PD-SC-REBATE.
           MOVE QCL-PD-MF-REBATE TO CL-PD-MF-REBATE.
           MOVE QCL-PD-DF-REBATE TO CL-PD-DF-REBATE.
           MOVE QCL-PD-EX-REBATE TO CL-PD-EX-REBATE.
           MOVE QCL-PD-O1-REBATE TO CL-PD-O1-REBATE.
           MOVE QCL-PD-O2-REBATE TO CL-PD-O2-REBATE.
           MOVE QCL-PD-O3-REBATE TO CL-PD-O3-REBATE.
           MOVE QCL-PD-O4-REBATE TO CL-PD-O4-REBATE.
           MOVE QCL-PD-O5-REBATE TO CL-PD-O5-REBATE.
           MOVE QCL-PD-O6-REBATE TO CL-PD-O6-REBATE.
           MOVE QCL-PD-O7-REBATE TO CL-PD-O7-REBATE.
           MOVE QCL-PD-UN-O7-REB TO CL-PD-UN-O7-REB.
           MOVE QCL-PD-UN-O8-REB TO CL-PD-UN-O8-REB.
           MOVE QCL-PD-UN-O9-REB TO CL-PD-UN-O9-REB.
           MOVE QCL-PD-UN-O10-REB TO CL-PD-UN-O10-REB.
           MOVE QCL-PD-PLREASON TO CL-PD-PLREASON.
           MOVE QCL-PD-COLLECTOR TO CL-PD-COLLECTOR.
           MOVE QCL-PD-ACTIONCD TO CL-PD-ACTIONCD.
           MOVE QCL-PD-ACTIONCD2 TO CL-PD-ACTIONCD2.
           MOVE QCL-PD-ATCODE TO CL-PD-ATCODE.
           MOVE QCL-PD-RECODE TO CL-PD-RECODE.
           MOVE QCL-PD-ESCROW-FG TO CL-PD-ESCROW-FG.
           MOVE QCL-PD-PROMDAYS-PASTDUE TO CL-PD-PROMDAYS-PASTDUE.
           MOVE QCL-PD-PROMID TO CL-PD-PROMID.

           IF ( QCL-PD-PROMDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-PROMDATE
           ELSE
              MOVE QCL-PD-PROMDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-PROMDATE.

           MOVE QCL-PD-PROMAMT TO CL-PD-PROMAMT.

           IF ( QCL-PD-PROMDATE2 = "1900-01-01" )
              MOVE ZEROES TO CL-PD-PROMDATE2
           ELSE
              MOVE QCL-PD-PROMDATE2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-PROMDATE2.


           IF ( QCL-PD-DATE-PAID-LAST = "1900-01-01" )
              MOVE ZEROES TO CL-PD-DATE-PAID-LAST
           ELSE
              MOVE QCL-PD-DATE-PAID-LAST TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-DATE-PAID-LAST.

           MOVE QCL-PD-AMT-PAID-LAST TO CL-PD-AMT-PAID-LAST.
           MOVE QCL-PD-LASTCFORMNO TO CL-PD-LASTCFORMNO.

           IF ( QCL-PD-LASTCFORMDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-LASTCFORMDATE
           ELSE
              MOVE QCL-PD-LASTCFORMDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-LASTCFORMDATE.

           MOVE QCL-PD-DELCNT TO CL-PD-DELCNT.
           MOVE QCL-PD-DELCNT2 TO CL-PD-DELCNT2.
           MOVE QCL-PD-CREDITLIM TO CL-PD-CREDITLIM.
           MOVE QCL-PD-CRSCORE TO CL-PD-CRSCORE.
           MOVE QCL-PD-LASTSFORMNO TO CL-PD-LASTSFORMNO.

           IF ( QCL-PD-LASTSFORMDATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-LASTSFORMDATE
           ELSE
              MOVE QCL-PD-LASTSFORMDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-LASTSFORMDATE.

           MOVE QCL-PD-SOLCNT TO CL-PD-SOLCNT.
           MOVE QCL-PD-MESSAGE TO CL-PD-MESSAGE.
           MOVE QCL-PD-OLDACCTNO TO CL-PD-OLDACCTNO.
           MOVE QCL-PD-PRLNNO TO CL-PD-PRLNNO.
           MOVE QCL-PD-PRBAL TO CL-PD-PRBAL.
           MOVE QCL-PD-PRINTDUE TO CL-PD-PRINTDUE.
           MOVE QCL-PD-PRFINCHGRB TO CL-PD-PRFINCHGRB.
           MOVE QCL-PD-PRINSRB TO CL-PD-PRINSRB.
           MOVE QCL-PD-PRPOCD TO CL-PD-PRPOCD.

           IF ( QCL-PD-PRPODATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-PRPODATE
           ELSE
              MOVE QCL-PD-PRPODATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-PRPODATE.

           MOVE QCL-PD-PRDELINQAMT TO CL-PD-PRDELINQAMT.
           MOVE QCL-PD-PRLNAMT TO CL-PD-PRLNAMT.
           MOVE QCL-PD-PRPDUE-DAYS TO CL-PD-PRPDUE-DAYS.
           MOVE QCL-PD-PRDLDISC-UNEARN TO CL-PD-PRDLDISC-UNEARN.
           MOVE QCL-PD-PRCONPROFILE TO CL-PD-PRCONPROFILE.
           MOVE QCL-PD-SUM-ORIGPURAMT TO CL-PD-SUM-ORIGPURAMT.
           MOVE QCL-PD-SUM-LNAMT TO CL-PD-SUM-LNAMT.
           MOVE QCL-PD-SUM-FINCHG TO CL-PD-SUM-FINCHG.
           MOVE QCL-PD-SUM-INTCHG TO CL-PD-SUM-INTCHG.
           MOVE QCL-PD-SUM-ORGTERM TO CL-PD-SUM-ORGTERM.
           MOVE QCL-PD-SUM-REGPYAMT TO CL-PD-SUM-REGPYAMT.
           MOVE QCL-PD-SUM-EXTCHG TO CL-PD-SUM-EXTCHG.
           MOVE QCL-PD-SUM-SERCHG TO CL-PD-SUM-SERCHG.
           MOVE QCL-PD-SUM-MAINTFEE TO CL-PD-SUM-MAINTFEE.
           MOVE QCL-PD-UNITPER-CD TO CL-PD-UNITPER-CD.
           MOVE QCL-PD-UNITPER-FREQ TO CL-PD-UNITPER-FREQ.
           MOVE QCL-PD-DAYS-IN-UNITPER TO CL-PD-DAYS-IN-UNITPER.
           MOVE QCL-PD-UNITPER-PER-YEAR TO CL-PD-UNITPER-PER-YEAR.
           MOVE QCL-PD-REPOCD TO CL-PD-REPOCD.
           MOVE QCL-PD-BR-TFR-FG TO CL-PD-BR-TFR-FG.
           MOVE QCL-PD-O3INS-DLPROC TO CL-PD-O3INS-DLPROC.
           MOVE QCL-PD-INSPREM-DLADJ TO CL-PD-INSPREM-DLADJ.
           MOVE QCL-PD-PLAMT TO CL-PD-PLAMT.
           MOVE QCL-PD-MTD-PRIN TO CL-PD-MTD-PRIN.
           MOVE QCL-PD-MTD-LC TO CL-PD-MTD-LC.
           MOVE QCL-PD-MTD-INT TO CL-PD-MTD-INT.
           MOVE QCL-PD-MTD-OT TO CL-PD-MTD-OT.
           MOVE QCL-PD-MTD-EX TO CL-PD-MTD-EX.
           MOVE QCL-PD-BNKRPT-CHAPTER TO CL-PD-BNKRPT-CHAPTER.
           MOVE QCL-PD-BMO-CON-STATUS TO CL-PD-BMO-CON-STATUS.
           MOVE QCL-PD-BMO-REC-STATUS TO CL-PD-BMO-REC-STATUS.
           MOVE QCL-PD-APP-NUMBER TO CL-PD-APP-NUMBER.
           MOVE QCL-PD-GIFTNET TO CL-PD-GIFTNET.
           MOVE QCL-PD-GIFTNET-OUR-PO TO CL-PD-GIFTNET-OUR-PO.
           MOVE QCL-PD-PROC-DISBURSED TO CL-PD-PROC-DISBURSED.
           MOVE QCL-PD-NOODDPY TO CL-PD-NOODDPY.
           MOVE QCL-PD-CONPROFILE-1 TO CL-PD-CONPROFILE(1).
           MOVE QCL-PD-CONPROFILE-2 TO CL-PD-CONPROFILE(2).
           MOVE QCL-PD-CONPROFILE-3 TO CL-PD-CONPROFILE(3).
           MOVE QCL-PD-CONPROFILE-4 TO CL-PD-CONPROFILE(4).
           MOVE QCL-PD-CONPROFILE-5 TO CL-PD-CONPROFILE(5).
           MOVE QCL-PD-RECPROFILE-1 TO CL-PD-RECPROFILE(1).
           MOVE QCL-PD-RECPROFILE-2 TO CL-PD-RECPROFILE(2).
           MOVE QCL-PD-RECPROFILE-3 TO CL-PD-RECPROFILE(3).
           MOVE QCL-PD-RECPROFILE-4 TO CL-PD-RECPROFILE(4).
           MOVE QCL-PD-RECPROFILE-5 TO CL-PD-RECPROFILE(5).
           MOVE QCL-PD-TRW-STATUS TO CL-PD-TRW-STATUS.
           MOVE QCL-PD-ORIGIN TO CL-PD-ORIGIN.
           MOVE QCL-PD-PO-TRCD TO CL-PD-PO-TRCD.
           MOVE QCL-PD-NEXIS-CRSCORE TO CL-PD-NEXIS-CRSCORE.
           MOVE QCL-PD-TRW-B-CONSUMER-IND TO CL-PD-TRW-B-CONSUMER-IND.
           MOVE QCL-PD-PARVHELD-PAID-FG TO CL-PD-PARVHELD-PAID-FG.
           MOVE QCL-PD-ACCT-FROZEN TO CL-PD-ACCT-FROZEN.
           MOVE QCL-PD-TRW-SPECIAL-CODE TO CL-PD-TRW-SPECIAL-CODE.
           MOVE QCL-PD-CREATION-STATUS TO CL-PD-CREATION-STATUS.
           MOVE QCL-PD-TOTNOLCHG TO CL-PD-TOTNOLCHG.
           MOVE QCL-PD-LBOX TO CL-PD-LBOX.
           MOVE QCL-PD-EXTRA-PRIN-LFP TO CL-PD-EXTRA-PRIN-LFP.
           MOVE QCL-PD-REMAIN-INT-SINCE-LFP
              TO CL-PD-REMAIN-INT-SINCE-LFP.
           MOVE QCL-PD-REMAIN-PRIN-SINCE-LFP
              TO CL-PD-REMAIN-PRIN-SINCE-LFP.
           MOVE QCL-PD-INTERIM-COUPON TO CL-PD-INTERIM-COUPON.
           MOVE QCL-PD-DLCOMM-O3-CMSI TO CL-PD-DLCOMM-O3-CMSI.
           MOVE QCL-PD-DLCOMM-PP-CMSI TO CL-PD-DLCOMM-PP-CMSI.
           MOVE QCL-PD-TRW-J-CONSUMER-IND TO CL-PD-TRW-J-CONSUMER-IND.
           MOVE QCL-PD-ACH TO CL-PD-ACH.
           MOVE QCL-PD-1099-C-FG TO CL-PD-1099-C-FG.
           MOVE QCL-PD-TRW-COMPLIANCE-CODE
              TO CL-PD-TRW-COMPLIANCE-CODE.
           MOVE QCL-PD-UNWIND-REASON-CODE TO CL-PD-UNWIND-REASON-CODE.
           MOVE QCL-PD-PBI-PBD-FG TO CL-PD-PBI-PBD-FG.
           MOVE QCL-PD-OLD-ACCT-FORMAT-FG TO CL-PD-OLD-ACCT-FORMAT-FG.
           MOVE QCL-PD-APPROVED-BY-USER TO CL-PD-APPROVED-BY-USER.
           MOVE QCL-PD-APPROVED-BY-CODE TO CL-PD-APPROVED-BY-CODE.

           IF ( QCL-PD-APPROVED-BY-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-APPROVED-BY-DATE
           ELSE
              MOVE QCL-PD-APPROVED-BY-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-APPROVED-BY-DATE.

           MOVE QCL-PD-APPROVED-BY-TIME TO CL-PD-APPROVED-BY-TIME.

           IF ( QCL-PD-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CL-PD-LTOUCH-DATE
           ELSE
              MOVE QCL-PD-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CL-PD-LTOUCH-DATE.

           MOVE QCL-PD-LTOUCH-TIME TO CL-PD-LTOUCH-TIME.
           MOVE QCL-PD-HOLD-COLLATERAL TO CL-PD-HOLD-COLLATERAL.
           MOVE QCL-PD-DLPAID TO CL-PD-DLPAID.
           MOVE QCL-PD-CKNO TO CL-PD-CKNO.
           MOVE QCL-PD-FREE1 TO CL-PD-FREE1.
           MOVE QCL-PD-FREE2 TO CL-PD-FREE2.
           MOVE QCL-PD-FREE-SPACE-FILLER TO CL-PD-FREE-SPACE-FILLER.
           MOVE QCL-PD-POSTING-BR TO CL-PD-POSTING-BR.
           MOVE QCL-PD-OTHBR-FG TO CL-PD-OTHBR-FG.
           MOVE QCL-PD-APPROVAL-LIMIT TO CL-PD-APPROVAL-LIMIT.
           MOVE QCL-PD-APPROVAL-USERID TO CL-PD-APPROVAL-USERID.
           MOVE QCL-PD-MIN-CRED-SCORE TO CL-PD-MIN-CRED-SCORE.
           MOVE QCL-PD-CRED-SCORE-USERID TO CL-PD-CRED-SCORE-USERID.
           MOVE QCL-PD-FINAMT TO CL-PD-FINAMT.
           MOVE QCL-PD-PROCEEDS-FG TO CL-PD-PROCEEDS-FG.
           MOVE QCL-PD-POINTS-FG TO CL-PD-POINTS-FG.
           MOVE QCL-PD-OVRDRATE-FG TO CL-PD-OVRDRATE-FG.
           MOVE QCL-PD-INS-FG-1 TO CL-PD-INS-FG(1).
           MOVE QCL-PD-INS-FG-2 TO CL-PD-INS-FG(2).
           MOVE QCL-PD-INS-FG-3 TO CL-PD-INS-FG(3).
           MOVE QCL-PD-INS-FG-4 TO CL-PD-INS-FG(4).
           MOVE QCL-PD-INS-FG-5 TO CL-PD-INS-FG(5).
           MOVE QCL-PD-INS-FG-6 TO CL-PD-INS-FG(6).
           MOVE QCL-PD-INS-FG-7 TO CL-PD-INS-FG(7).
           MOVE QCL-PD-INS-FG-8 TO CL-PD-INS-FG(8).
           MOVE QCL-PD-INS-FG-9 TO CL-PD-INS-FG(9).
           MOVE QCL-PD-INS-FG-10 TO CL-PD-INS-FG(10).
           MOVE QCL-PD-SERCHG-FG TO CL-PD-SERCHG-FG.
           MOVE QCL-PD-MAINTFEE-FG TO CL-PD-MAINTFEE-FG.
           MOVE QCL-PD-EXTCHG-FG TO CL-PD-EXTCHG-FG.
           MOVE QCL-PD-REGPYAMT-FG TO CL-PD-REGPYAMT-FG.
           MOVE QCL-PD-FEECD-1 TO CL-PD-FEECD(1).
           MOVE QCL-PD-FEECD-2 TO CL-PD-FEECD(2).
           MOVE QCL-PD-FEECD-3 TO CL-PD-FEECD(3).
           MOVE QCL-PD-FEECD-4 TO CL-PD-FEECD(4).
           MOVE QCL-PD-FEECD-5 TO CL-PD-FEECD(5).
           MOVE QCL-PD-FEECD-6 TO CL-PD-FEECD(6).
           MOVE QCL-PD-FEECD-7 TO CL-PD-FEECD(7).
           MOVE QCL-PD-FEECD-8 TO CL-PD-FEECD(8).
           MOVE QCL-PD-FEECD-9 TO CL-PD-FEECD(9).
           MOVE QCL-PD-FEECD-10 TO CL-PD-FEECD(10).
           MOVE QCL-PD-FEECD-11 TO CL-PD-FEECD(11).
           MOVE QCL-PD-FEECD-12 TO CL-PD-FEECD(12).
           MOVE QCL-PD-FEECD-13 TO CL-PD-FEECD(13).
           MOVE QCL-PD-FEECD-14 TO CL-PD-FEECD(14).
           MOVE QCL-PD-FEECD-15 TO CL-PD-FEECD(15).
           MOVE QCL-PD-FEECD-16 TO CL-PD-FEECD(16).
           MOVE QCL-PD-FEECD-17 TO CL-PD-FEECD(17).
           MOVE QCL-PD-FEECD-18 TO CL-PD-FEECD(18).
           MOVE QCL-PD-FEECD-19 TO CL-PD-FEECD(19).
           MOVE QCL-PD-FEECD-20 TO CL-PD-FEECD(20).
           MOVE QCL-PD-PRPAID-AHEAD TO CL-PD-PRPAID-AHEAD.
           MOVE QCL-PD-RESTRICT-ADJ TO CL-PD-RESTRICT-ADJ.
           MOVE QCL-PD-DUAL-LOAN-ADJ TO CL-PD-DUAL-LOAN-ADJ.
           MOVE QCL-PD-CAN-LOAN-MAX TO CL-PD-CAN-LOAN-MAX.
           MOVE QCL-PD-PERDOWN TO CL-PD-PERDOWN.
           MOVE QCL-PD-SELLPR-FG TO CL-PD-SELLPR-FG.
           MOVE QCL-PD-DOWNPY-FG TO CL-PD-DOWNPY-FG.
           MOVE QCL-PD-PERDOWN-FG TO CL-PD-PERDOWN-FG.
           MOVE QCL-PD-SELL-DOWN-FG TO CL-PD-SELL-DOWN-FG.
           MOVE QCL-PD-BALLOON-FG TO CL-PD-BALLOON-FG.
           MOVE QCL-PD-BALLOON-LAST-FG TO CL-PD-BALLOON-LAST-FG.
           MOVE QCL-PD-FEECD-FG-1 TO CL-PD-FEECD-FG(1).
           MOVE QCL-PD-FEECD-FG-2 TO CL-PD-FEECD-FG(2).
           MOVE QCL-PD-INS-TERM-SW-1 TO CL-PD-INS-TERM-SW(1).
           MOVE QCL-PD-COVERAGE-SW-1 TO CL-PD-COVERAGE-SW(1).
           MOVE QCL-PD-PREMIUM-SW-1 TO CL-PD-PREMIUM-SW(1).
           MOVE QCL-PD-INS-TERM-SW-2 TO CL-PD-INS-TERM-SW(2).
           MOVE QCL-PD-COVERAGE-SW-2 TO CL-PD-COVERAGE-SW(2).
           MOVE QCL-PD-PREMIUM-SW-2 TO CL-PD-PREMIUM-SW(2).
           MOVE QCL-PD-INS-TERM-SW-3 TO CL-PD-INS-TERM-SW(3).
           MOVE QCL-PD-COVERAGE-SW-3 TO CL-PD-COVERAGE-SW(3).
           MOVE QCL-PD-PREMIUM-SW-3 TO CL-PD-PREMIUM-SW(3).
           MOVE QCL-PD-INS-TERM-SW-4 TO CL-PD-INS-TERM-SW(4).
           MOVE QCL-PD-COVERAGE-SW-4 TO CL-PD-COVERAGE-SW(4).
           MOVE QCL-PD-PREMIUM-SW-4 TO CL-PD-PREMIUM-SW(4).
           MOVE QCL-PD-INS-TERM-SW-5 TO CL-PD-INS-TERM-SW(5).
           MOVE QCL-PD-COVERAGE-SW-5 TO CL-PD-COVERAGE-SW(5).
           MOVE QCL-PD-PREMIUM-SW-5 TO CL-PD-PREMIUM-SW(5).
           MOVE QCL-PD-INS-TERM-SW-6 TO CL-PD-INS-TERM-SW(6).
           MOVE QCL-PD-COVERAGE-SW-6 TO CL-PD-COVERAGE-SW(6).
           MOVE QCL-PD-PREMIUM-SW-6 TO CL-PD-PREMIUM-SW(6).
           MOVE QCL-PD-INS-TERM-SW-7 TO CL-PD-INS-TERM-SW(7).
           MOVE QCL-PD-COVERAGE-SW-7 TO CL-PD-COVERAGE-SW(7).
           MOVE QCL-PD-PREMIUM-SW-7 TO CL-PD-PREMIUM-SW(7).
           MOVE QCL-PD-INS-TERM-SW-8 TO CL-PD-INS-TERM-SW(8).
           MOVE QCL-PD-COVERAGE-SW-8 TO CL-PD-COVERAGE-SW(8).
           MOVE QCL-PD-PREMIUM-SW-8 TO CL-PD-PREMIUM-SW(8).
           MOVE QCL-PD-INS-TERM-SW-9 TO CL-PD-INS-TERM-SW(9).
           MOVE QCL-PD-COVERAGE-SW-9 TO CL-PD-COVERAGE-SW(9).
           MOVE QCL-PD-PREMIUM-SW-9 TO CL-PD-PREMIUM-SW(9).
           MOVE QCL-PD-INS-TERM-SW-10 TO CL-PD-INS-TERM-SW(10).
           MOVE QCL-PD-COVERAGE-SW-10 TO CL-PD-COVERAGE-SW(10).
           MOVE QCL-PD-PREMIUM-SW-10 TO CL-PD-PREMIUM-SW(10).
           MOVE QCL-PD-DLDISC-FG TO CL-PD-DLDISC-FG.
           MOVE QCL-PD-DLRVPRIN-FG-1 TO CL-PD-DLRVPRIN-FG(1).
           MOVE QCL-PD-DLRVPRIN-FG-2 TO CL-PD-DLRVPRIN-FG(2).
           MOVE QCL-PD-DLRVPRIN-FG-3 TO CL-PD-DLRVPRIN-FG(3).
           MOVE QCL-PD-DLRVCHGS-FG-1 TO CL-PD-DLRVCHGS-FG(1).
           MOVE QCL-PD-DLRVCHGS-FG-2 TO CL-PD-DLRVCHGS-FG(2).
           MOVE QCL-PD-DLRVCHGS-FG-3 TO CL-PD-DLRVCHGS-FG(3).
           MOVE QCL-PD-DLPARVHELD-FG TO CL-PD-DLPARVHELD-FG.
           MOVE QCL-PD-DLPARVPAID-FG TO CL-PD-DLPARVPAID-FG.
           MOVE QCL-PD-ONE-MTH-NO-DAY-EXT-SW
              TO CL-PD-ONE-MTH-NO-DAY-EXT-SW.
           MOVE QCL-PD-ASSIGN-RN-ACCTNO TO CL-PD-ASSIGN-RN-ACCTNO.
           MOVE QCL-PD-APR-FINAMT TO CL-PD-APR-FINAMT.
           MOVE QCL-PD-DLCOMM-PP-CMSI-FG TO CL-PD-DLCOMM-PP-CMSI-FG.
           MOVE QCL-PD-DLCOMM-O3-CMSI-FG TO CL-PD-DLCOMM-O3-CMSI-FG.
           MOVE QCL-PD-FILLER-TO-MATCH-LN TO CL-PD-FILLER-TO-MATCH-LN.
           MOVE QCL-PD2-ID TO CL-PD2-ID.
           MOVE QCL-PD2-BRANCH TO CL-PD2-BRANCH.
           MOVE QCL-PD2-BWNAME TO CL-PD2-BWNAME.
 
      *-----------------------------------------------------------------
      *    LOAD QCL-REC FROM CL-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CL-FIELDS.
           MOVE CL-GROUP-ID TO QCL-GROUP-ID.
           MOVE CL-CAN-NO TO QCL-CAN-NO.

           IF ( CL-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-LTOUCH-DATE
           ELSE
              MOVE CL-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-LTOUCH-DATE.

           MOVE CL-DESC TO QCL-DESC.
           MOVE CL-GIFTNET TO QCL-GIFTNET.
           MOVE CL-PD-ID TO QCL-PD-ID.
           MOVE CL-PD-BRANCH TO QCL-PD-BRANCH.
           MOVE CL-PD-SOC-SEC TO QCL-PD-SOC-SEC.
           MOVE CL-PD-OWNBR TO QCL-PD-OWNBR.
           MOVE CL-PD-ACCTNO TO QCL-PD-ACCTNO.
           MOVE CL-PD-CLASS TO QCL-PD-CLASS.
           MOVE CL-PD-SUBCLASS TO QCL-PD-SUBCLASS.
           MOVE CL-PD-LAWCODE TO QCL-PD-LAWCODE.
           MOVE CL-PD-SSNO1 TO QCL-PD-SSNO1.
           MOVE CL-PD-MAKERCD1 TO QCL-PD-MAKERCD1.
           MOVE CL-PD-SSNO2 TO QCL-PD-SSNO2.
           MOVE CL-PD-MAKERCD2 TO QCL-PD-MAKERCD2.
           MOVE CL-PD-SSNO3 TO QCL-PD-SSNO3.
           MOVE CL-PD-MAKERCD3 TO QCL-PD-MAKERCD3.
           MOVE CL-PD-SSNO4 TO QCL-PD-SSNO4.
           MOVE CL-PD-MAKERCD4 TO QCL-PD-MAKERCD4.
           MOVE CL-PD-ORGST TO QCL-PD-ORGST.
           MOVE CL-PD-ORGBR TO QCL-PD-ORGBR.
           MOVE CL-PD-OWNST TO QCL-PD-OWNST.
           MOVE CL-PD-SPRCLASS TO QCL-PD-SPRCLASS.
           MOVE CL-PD-CONVERCD TO QCL-PD-CONVERCD.
           MOVE CL-PD-MIDSTREAM TO QCL-PD-MIDSTREAM.
           MOVE CL-PD-PURCD TO QCL-PD-PURCD.
           MOVE CL-PD-SRCD TO QCL-PD-SRCD.
           MOVE CL-PD-COLLCD TO QCL-PD-COLLCD.
           MOVE CL-PD-COLLID TO QCL-PD-COLLID.
           MOVE CL-PD-POCD TO QCL-PD-POCD.
           MOVE CL-PD-PLCD TO QCL-PD-PLCD.
           MOVE CL-PD-PLTYPE TO QCL-PD-PLTYPE.
           MOVE CL-PD-CONFIDENTCD TO QCL-PD-CONFIDENTCD.
           MOVE CL-PD-ALLOTCD TO QCL-PD-ALLOTCD.
           MOVE CL-PD-RECASTCD TO QCL-PD-RECASTCD.
           MOVE CL-PD-SKIP-TRW TO QCL-PD-SKIP-TRW.
           MOVE CL-PD-STATUSFG TO QCL-PD-STATUSFG.
           MOVE CL-PD-SMALLOAN TO QCL-PD-SMALLOAN.
           MOVE CL-PD-SALEFIN TO QCL-PD-SALEFIN.
           MOVE CL-PD-SERVACCT TO QCL-PD-SERVACCT.
           MOVE CL-PD-LOANTYPE TO QCL-PD-LOANTYPE.
           MOVE CL-PD-ADDON-FG TO QCL-PD-ADDON-FG.
           MOVE CL-PD-PAY-SCHLD-FG TO QCL-PD-PAY-SCHLD-FG.
           MOVE CL-PD-LC-CODE TO QCL-PD-LC-CODE.
           MOVE CL-PD-CAN-LOAN-NO TO QCL-PD-CAN-LOAN-NO.
           MOVE CL-PD-STATFLAGS TO QCL-PD-STATFLAGS.

           IF ( CL-PD-ENTDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-ENTDATE
           ELSE
              MOVE CL-PD-ENTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-ENTDATE.


           IF ( CL-PD-LOANDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-LOANDATE
           ELSE
              MOVE CL-PD-LOANDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-LOANDATE.


           IF ( CL-PD-PURDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-PURDATE
           ELSE
              MOVE CL-PD-PURDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-PURDATE.


           IF ( CL-PD-INTDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-INTDATE
           ELSE
              MOVE CL-PD-INTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-INTDATE.


           IF ( CL-PD-CASHDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-CASHDATE
           ELSE
              MOVE CL-PD-CASHDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-CASHDATE.


           IF ( CL-PD-FILEDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-FILEDATE
           ELSE
              MOVE CL-PD-FILEDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-FILEDATE.


           IF ( CL-PD-RESCISSION-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-RESCISSION-DATE
           ELSE
              MOVE CL-PD-RESCISSION-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-RESCISSION-DATE.


           IF ( CL-PD-PART-RECOURSE-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-PART-RECOURSE-DATE
           ELSE
              MOVE CL-PD-PART-RECOURSE-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-PART-RECOURSE-DATE.


           IF ( CL-PD-ORIG-1STPYDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-ORIG-1STPYDATE
           ELSE
              MOVE CL-PD-ORIG-1STPYDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-ORIG-1STPYDATE.


           IF ( CL-PD-1STPYDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-1STPYDATE
           ELSE
              MOVE CL-PD-1STPYDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-1STPYDATE.


           IF ( CL-PD-JDDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-JDDATE
           ELSE
              MOVE CL-PD-JDDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-JDDATE.


           IF ( CL-PD-PLDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-PLDATE
           ELSE
              MOVE CL-PD-PLDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-PLDATE.


           IF ( CL-PD-PL-TRDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-PL-TRDATE
           ELSE
              MOVE CL-PD-PL-TRDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-PL-TRDATE.


           IF ( CL-PD-ACCELDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-ACCELDATE
           ELSE
              MOVE CL-PD-ACCELDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-ACCELDATE.


           IF ( CL-PD-REDUDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-REDUDATE
           ELSE
              MOVE CL-PD-REDUDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-REDUDATE.


           IF ( CL-PD-POFFDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-POFFDATE
           ELSE
              MOVE CL-PD-POFFDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-POFFDATE.


           IF ( CL-PD-PO-TRDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-PO-TRDATE
           ELSE
              MOVE CL-PD-PO-TRDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-PO-TRDATE.


           IF ( CL-PD-LEGALDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-LEGALDATE
           ELSE
              MOVE CL-PD-LEGALDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-LEGALDATE.


           IF ( CL-PD-BNKRPTDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-BNKRPTDATE
           ELSE
              MOVE CL-PD-BNKRPTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-BNKRPTDATE.


           IF ( CL-PD-BNKRPT-DISMISS-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-BNKRPT-DISMISS-DATE
           ELSE
              MOVE CL-PD-BNKRPT-DISMISS-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-BNKRPT-DISMISS-DATE.


           IF ( CL-PD-BNKRPT-DISCHARGE-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-BNKRPT-DISCHARGE-DATE
           ELSE
              MOVE CL-PD-BNKRPT-DISCHARGE-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-BNKRPT-DISCHARGE-DATE.


           IF ( CL-PD-BNKRPT-REAFFIRM-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-BNKRPT-REAFFIRM-DATE
           ELSE
              MOVE CL-PD-BNKRPT-REAFFIRM-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-BNKRPT-REAFFIRM-DATE.


           IF ( CL-PD-DEBTAIDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-DEBTAIDATE
           ELSE
              MOVE CL-PD-DEBTAIDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-DEBTAIDATE.


           IF ( CL-PD-REPODATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-REPODATE
           ELSE
              MOVE CL-PD-REPODATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-REPODATE.


           IF ( CL-PD-REDEEMDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-REDEEMDATE
           ELSE
              MOVE CL-PD-REDEEMDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-REDEEMDATE.


           IF ( CL-PD-SOLDDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-SOLDDATE
           ELSE
              MOVE CL-PD-SOLDDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-SOLDDATE.


           IF ( CL-PD-INS-CONFIRM-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-INS-CONFIRM-DATE
           ELSE
              MOVE CL-PD-INS-CONFIRM-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-INS-CONFIRM-DATE.


           IF ( CL-PD-PRIOR-RN-FOR10-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-PRIOR-RN-FOR10-DATE
           ELSE
              MOVE CL-PD-PRIOR-RN-FOR10-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-PRIOR-RN-FOR10-DATE.


           IF ( CL-PD-LAST-RN-FOR10-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-LAST-RN-FOR10-DATE
           ELSE
              MOVE CL-PD-LAST-RN-FOR10-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-LAST-RN-FOR10-DATE.


           IF ( CL-PD-TRW-DOFD-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-TRW-DOFD-DATE
           ELSE
              MOVE CL-PD-TRW-DOFD-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-TRW-DOFD-DATE.


           IF ( CL-PD-LAST-PYPA-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-LAST-PYPA-DATE
           ELSE
              MOVE CL-PD-LAST-PYPA-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-LAST-PYPA-DATE.


           IF ( CL-PD-BT-TRDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-BT-TRDATE
           ELSE
              MOVE CL-PD-BT-TRDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-BT-TRDATE.


           IF ( CL-PD-METRO2-LAST-PMT-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-METRO2-LAST-PMT-DATE
           ELSE
              MOVE CL-PD-METRO2-LAST-PMT-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-METRO2-LAST-PMT-DATE.


           IF ( CL-PD-FOLLOWUP-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-FOLLOWUP-DATE
           ELSE
              MOVE CL-PD-FOLLOWUP-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-FOLLOWUP-DATE.


           IF ( CL-PD-LAST-ACCRUAL-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-LAST-ACCRUAL-DATE
           ELSE
              MOVE CL-PD-LAST-ACCRUAL-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-LAST-ACCRUAL-DATE.

           MOVE CL-PD-APRATE TO QCL-PD-APRATE.
           MOVE CL-PD-EFFRATE TO QCL-PD-EFFRATE.
           MOVE CL-PD-SMPRATE TO QCL-PD-SMPRATE.
           MOVE CL-PD-YLDRATE TO QCL-PD-YLDRATE.
           MOVE CL-PD-OVRDRATE TO QCL-PD-OVRDRATE.
           MOVE CL-PD-ENDORATE TO QCL-PD-ENDORATE.
           MOVE CL-PD-JDRATE TO QCL-PD-JDRATE.
           MOVE CL-PD-ACCRATE TO QCL-PD-ACCRATE.
           MOVE CL-PD-REDURATE TO QCL-PD-REDURATE.
           MOVE CL-PD-CONRATE TO QCL-PD-CONRATE.
           MOVE CL-PD-SERVPER TO QCL-PD-SERVPER.
           MOVE CL-PD-SELLPR TO QCL-PD-SELLPR.
           MOVE CL-PD-DOWNPY TO QCL-PD-DOWNPY.
           MOVE CL-PD-TRADEIN TO QCL-PD-TRADEIN.
           MOVE CL-PD-LNAMT TO QCL-PD-LNAMT.
           MOVE CL-PD-FINCHG TO QCL-PD-FINCHG.
           MOVE CL-PD-INTCHG TO QCL-PD-INTCHG.
           MOVE CL-PD-SERCHG TO QCL-PD-SERCHG.
           MOVE CL-PD-EXTCHG TO QCL-PD-EXTCHG.
           MOVE CL-PD-MAINTFEE TO QCL-PD-MAINTFEE.
           MOVE CL-PD-POINTS TO QCL-PD-POINTS.
           MOVE CL-PD-PROCEEDS TO QCL-PD-PROCEEDS.
           MOVE CL-PD-ORGTERM TO QCL-PD-ORGTERM.
           MOVE CL-PD-1STPYAMT TO QCL-PD-1STPYAMT.
           MOVE CL-PD-NOREGPY TO QCL-PD-NOREGPY.
           MOVE CL-PD-REGPYAMT TO QCL-PD-REGPYAMT.
           MOVE CL-PD-LASTPYAMT TO QCL-PD-LASTPYAMT.
           MOVE CL-PD-PROCASH TO QCL-PD-PROCASH.
           MOVE CL-PD-TILLNO TO QCL-PD-TILLNO.
           MOVE CL-PD-STATE-FLAG TO QCL-PD-STATE-FLAG.
           MOVE CL-PD-STATE-WORKER TO QCL-PD-STATE-WORKER.
           MOVE CL-PD-STATE-WORKER2 TO QCL-PD-STATE-WORKER2.
           MOVE CL-PD-FEECODE(1) TO QCL-PD-FEECODE-1.
           MOVE CL-PD-FEEAMT(1) TO QCL-PD-FEEAMT-1.
           MOVE CL-PD-FEEOURS(1) TO QCL-PD-FEEOURS-1.
           MOVE CL-PD-FEEFINCHG(1) TO QCL-PD-FEEFINCHG-1.
           MOVE CL-PD-FEECODE(2) TO QCL-PD-FEECODE-2.
           MOVE CL-PD-FEEAMT(2) TO QCL-PD-FEEAMT-2.
           MOVE CL-PD-FEEOURS(2) TO QCL-PD-FEEOURS-2.
           MOVE CL-PD-FEEFINCHG(2) TO QCL-PD-FEEFINCHG-2.
           MOVE CL-PD-FEECODE(3) TO QCL-PD-FEECODE-3.
           MOVE CL-PD-FEEAMT(3) TO QCL-PD-FEEAMT-3.
           MOVE CL-PD-FEEOURS(3) TO QCL-PD-FEEOURS-3.
           MOVE CL-PD-FEEFINCHG(3) TO QCL-PD-FEEFINCHG-3.
           MOVE CL-PD-FEECODE(4) TO QCL-PD-FEECODE-4.
           MOVE CL-PD-FEEAMT(4) TO QCL-PD-FEEAMT-4.
           MOVE CL-PD-FEEOURS(4) TO QCL-PD-FEEOURS-4.
           MOVE CL-PD-FEEFINCHG(4) TO QCL-PD-FEEFINCHG-4.
           MOVE CL-PD-FEECODE(5) TO QCL-PD-FEECODE-5.
           MOVE CL-PD-FEEAMT(5) TO QCL-PD-FEEAMT-5.
           MOVE CL-PD-FEEOURS(5) TO QCL-PD-FEEOURS-5.
           MOVE CL-PD-FEEFINCHG(5) TO QCL-PD-FEEFINCHG-5.
           MOVE CL-PD-FEECODE(6) TO QCL-PD-FEECODE-6.
           MOVE CL-PD-FEEAMT(6) TO QCL-PD-FEEAMT-6.
           MOVE CL-PD-FEEOURS(6) TO QCL-PD-FEEOURS-6.
           MOVE CL-PD-FEEFINCHG(6) TO QCL-PD-FEEFINCHG-6.
           MOVE CL-PD-FEECODE(7) TO QCL-PD-FEECODE-7.
           MOVE CL-PD-FEEAMT(7) TO QCL-PD-FEEAMT-7.
           MOVE CL-PD-FEEOURS(7) TO QCL-PD-FEEOURS-7.
           MOVE CL-PD-FEEFINCHG(7) TO QCL-PD-FEEFINCHG-7.
           MOVE CL-PD-FEECODE(8) TO QCL-PD-FEECODE-8.
           MOVE CL-PD-FEEAMT(8) TO QCL-PD-FEEAMT-8.
           MOVE CL-PD-FEEOURS(8) TO QCL-PD-FEEOURS-8.
           MOVE CL-PD-FEEFINCHG(8) TO QCL-PD-FEEFINCHG-8.
           MOVE CL-PD-FEECODE(9) TO QCL-PD-FEECODE-9.
           MOVE CL-PD-FEEAMT(9) TO QCL-PD-FEEAMT-9.
           MOVE CL-PD-FEEOURS(9) TO QCL-PD-FEEOURS-9.
           MOVE CL-PD-FEEFINCHG(9) TO QCL-PD-FEEFINCHG-9.
           MOVE CL-PD-FEECODE(10) TO QCL-PD-FEECODE-10.
           MOVE CL-PD-FEEAMT(10) TO QCL-PD-FEEAMT-10.
           MOVE CL-PD-FEEOURS(10) TO QCL-PD-FEEOURS-10.
           MOVE CL-PD-FEEFINCHG(10) TO QCL-PD-FEEFINCHG-10.
           MOVE CL-PD-FEECODE(11) TO QCL-PD-FEECODE-11.
           MOVE CL-PD-FEEAMT(11) TO QCL-PD-FEEAMT-11.
           MOVE CL-PD-FEEOURS(11) TO QCL-PD-FEEOURS-11.
           MOVE CL-PD-FEEFINCHG(11) TO QCL-PD-FEEFINCHG-11.
           MOVE CL-PD-FEECODE(12) TO QCL-PD-FEECODE-12.
           MOVE CL-PD-FEEAMT(12) TO QCL-PD-FEEAMT-12.
           MOVE CL-PD-FEEOURS(12) TO QCL-PD-FEEOURS-12.
           MOVE CL-PD-FEEFINCHG(12) TO QCL-PD-FEEFINCHG-12.
           MOVE CL-PD-FEECODE(13) TO QCL-PD-FEECODE-13.
           MOVE CL-PD-FEEAMT(13) TO QCL-PD-FEEAMT-13.
           MOVE CL-PD-FEEOURS(13) TO QCL-PD-FEEOURS-13.
           MOVE CL-PD-FEEFINCHG(13) TO QCL-PD-FEEFINCHG-13.
           MOVE CL-PD-FEECODE(14) TO QCL-PD-FEECODE-14.
           MOVE CL-PD-FEEAMT(14) TO QCL-PD-FEEAMT-14.
           MOVE CL-PD-FEEOURS(14) TO QCL-PD-FEEOURS-14.
           MOVE CL-PD-FEEFINCHG(14) TO QCL-PD-FEEFINCHG-14.
           MOVE CL-PD-FEECODE(15) TO QCL-PD-FEECODE-15.
           MOVE CL-PD-FEEAMT(15) TO QCL-PD-FEEAMT-15.
           MOVE CL-PD-FEEOURS(15) TO QCL-PD-FEEOURS-15.
           MOVE CL-PD-FEEFINCHG(15) TO QCL-PD-FEEFINCHG-15.
           MOVE CL-PD-FEECODE(16) TO QCL-PD-FEECODE-16.
           MOVE CL-PD-FEEAMT(16) TO QCL-PD-FEEAMT-16.
           MOVE CL-PD-FEEOURS(16) TO QCL-PD-FEEOURS-16.
           MOVE CL-PD-FEEFINCHG(16) TO QCL-PD-FEEFINCHG-16.
           MOVE CL-PD-FEECODE(17) TO QCL-PD-FEECODE-17.
           MOVE CL-PD-FEEAMT(17) TO QCL-PD-FEEAMT-17.
           MOVE CL-PD-FEEOURS(17) TO QCL-PD-FEEOURS-17.
           MOVE CL-PD-FEEFINCHG(17) TO QCL-PD-FEEFINCHG-17.
           MOVE CL-PD-FEECODE(18) TO QCL-PD-FEECODE-18.
           MOVE CL-PD-FEEAMT(18) TO QCL-PD-FEEAMT-18.
           MOVE CL-PD-FEEOURS(18) TO QCL-PD-FEEOURS-18.
           MOVE CL-PD-FEEFINCHG(18) TO QCL-PD-FEEFINCHG-18.
           MOVE CL-PD-FEECODE(19) TO QCL-PD-FEECODE-19.
           MOVE CL-PD-FEEAMT(19) TO QCL-PD-FEEAMT-19.
           MOVE CL-PD-FEEOURS(19) TO QCL-PD-FEEOURS-19.
           MOVE CL-PD-FEEFINCHG(19) TO QCL-PD-FEEFINCHG-19.
           MOVE CL-PD-FEECODE(20) TO QCL-PD-FEECODE-20.
           MOVE CL-PD-FEEAMT(20) TO QCL-PD-FEEAMT-20.
           MOVE CL-PD-FEEOURS(20) TO QCL-PD-FEEOURS-20.
           MOVE CL-PD-FEEFINCHG(20) TO QCL-PD-FEEFINCHG-20.
           MOVE CL-PD-INSURED TO QCL-PD-INSURED.
           MOVE CL-PD-INSTYPES TO QCL-PD-INSTYPES.
           MOVE CL-PD-INSELIGIBLE TO QCL-PD-INSELIGIBLE.
           MOVE CL-PD-INSCOMP(1) TO QCL-PD-INSCOMP-1.
           MOVE CL-PD-INSOURS(1) TO QCL-PD-INSOURS-1.

           MOVE CL-PD-INSEFF-DATE(1) TO QCL-PD-INSEFF-DATE-1.

           IF ( CL-PD-INSEXP-DATE(1) = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-INSEXP-DATE-1
           ELSE
              MOVE CL-PD-INSEXP-DATE(1) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-INSEXP-DATE-1.

           MOVE CL-PD-INSPREM(1) TO QCL-PD-INSPREM-1.
           MOVE CL-PD-INSCOVR(1) TO QCL-PD-INSCOVR-1.
           MOVE CL-PD-INSCOMM(1) TO QCL-PD-INSCOMM-1.
           MOVE CL-PD-INSANTICERN(1) TO QCL-PD-INSANTICERN-1.
           MOVE CL-PD-INSANTICADJ(1) TO QCL-PD-INSANTICADJ-1.
           MOVE CL-PD-INSUFERN(1) TO QCL-PD-INSUFERN-1.
           MOVE CL-PD-INSACCERN(1) TO QCL-PD-INSACCERN-1.
           MOVE CL-PD-INSEARN-ORDER(1) TO QCL-PD-INSEARN-ORDER-1.
           MOVE CL-PD-INSFINCHG(1) TO QCL-PD-INSFINCHG-1.
           MOVE CL-PD-INSCOMP(2) TO QCL-PD-INSCOMP-2.
           MOVE CL-PD-INSOURS(2) TO QCL-PD-INSOURS-2.

           MOVE CL-PD-INSEFF-DATE(2) TO QCL-PD-INSEFF-DATE-2.

           IF ( CL-PD-INSEXP-DATE(2) = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-INSEXP-DATE-2
           ELSE
              MOVE CL-PD-INSEXP-DATE(2) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-INSEXP-DATE-2.

           MOVE CL-PD-INSPREM(2) TO QCL-PD-INSPREM-2.
           MOVE CL-PD-INSCOVR(2) TO QCL-PD-INSCOVR-2.
           MOVE CL-PD-INSCOMM(2) TO QCL-PD-INSCOMM-2.
           MOVE CL-PD-INSANTICERN(2) TO QCL-PD-INSANTICERN-2.
           MOVE CL-PD-INSANTICADJ(2) TO QCL-PD-INSANTICADJ-2.
           MOVE CL-PD-INSUFERN(2) TO QCL-PD-INSUFERN-2.
           MOVE CL-PD-INSACCERN(2) TO QCL-PD-INSACCERN-2.
           MOVE CL-PD-INSEARN-ORDER(2) TO QCL-PD-INSEARN-ORDER-2.
           MOVE CL-PD-INSFINCHG(2) TO QCL-PD-INSFINCHG-2.
           MOVE CL-PD-INSCOMP(3) TO QCL-PD-INSCOMP-3.
           MOVE CL-PD-INSOURS(3) TO QCL-PD-INSOURS-3.

           MOVE CL-PD-INSEFF-DATE(3) TO QCL-PD-INSEFF-DATE-3.

           IF ( CL-PD-INSEXP-DATE(3) = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-INSEXP-DATE-3
           ELSE
              MOVE CL-PD-INSEXP-DATE(3) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-INSEXP-DATE-3.

           MOVE CL-PD-INSPREM(3) TO QCL-PD-INSPREM-3.
           MOVE CL-PD-INSCOVR(3) TO QCL-PD-INSCOVR-3.
           MOVE CL-PD-INSCOMM(3) TO QCL-PD-INSCOMM-3.
           MOVE CL-PD-INSANTICERN(3) TO QCL-PD-INSANTICERN-3.
           MOVE CL-PD-INSANTICADJ(3) TO QCL-PD-INSANTICADJ-3.
           MOVE CL-PD-INSUFERN(3) TO QCL-PD-INSUFERN-3.
           MOVE CL-PD-INSACCERN(3) TO QCL-PD-INSACCERN-3.
           MOVE CL-PD-INSEARN-ORDER(3) TO QCL-PD-INSEARN-ORDER-3.
           MOVE CL-PD-INSFINCHG(3) TO QCL-PD-INSFINCHG-3.
           MOVE CL-PD-INSCOMP(4) TO QCL-PD-INSCOMP-4.
           MOVE CL-PD-INSOURS(4) TO QCL-PD-INSOURS-4.

           MOVE CL-PD-INSEFF-DATE(4) TO QCL-PD-INSEFF-DATE-4.

           IF ( CL-PD-INSEXP-DATE(4) = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-INSEXP-DATE-4
           ELSE
              MOVE CL-PD-INSEXP-DATE(4) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-INSEXP-DATE-4.

           MOVE CL-PD-INSPREM(4) TO QCL-PD-INSPREM-4.
           MOVE CL-PD-INSCOVR(4) TO QCL-PD-INSCOVR-4.
           MOVE CL-PD-INSCOMM(4) TO QCL-PD-INSCOMM-4.
           MOVE CL-PD-INSANTICERN(4) TO QCL-PD-INSANTICERN-4.
           MOVE CL-PD-INSANTICADJ(4) TO QCL-PD-INSANTICADJ-4.
           MOVE CL-PD-INSUFERN(4) TO QCL-PD-INSUFERN-4.
           MOVE CL-PD-INSACCERN(4) TO QCL-PD-INSACCERN-4.
           MOVE CL-PD-INSEARN-ORDER(4) TO QCL-PD-INSEARN-ORDER-4.
           MOVE CL-PD-INSFINCHG(4) TO QCL-PD-INSFINCHG-4.
           MOVE CL-PD-INSCOMP(5) TO QCL-PD-INSCOMP-5.
           MOVE CL-PD-INSOURS(5) TO QCL-PD-INSOURS-5.

           MOVE CL-PD-INSEFF-DATE(5) TO QCL-PD-INSEFF-DATE-5.

           IF ( CL-PD-INSEXP-DATE(5) = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-INSEXP-DATE-5
           ELSE
              MOVE CL-PD-INSEXP-DATE(5) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-INSEXP-DATE-5.

           MOVE CL-PD-INSPREM(5) TO QCL-PD-INSPREM-5.
           MOVE CL-PD-INSCOVR(5) TO QCL-PD-INSCOVR-5.
           MOVE CL-PD-INSCOMM(5) TO QCL-PD-INSCOMM-5.
           MOVE CL-PD-INSANTICERN(5) TO QCL-PD-INSANTICERN-5.
           MOVE CL-PD-INSANTICADJ(5) TO QCL-PD-INSANTICADJ-5.
           MOVE CL-PD-INSUFERN(5) TO QCL-PD-INSUFERN-5.
           MOVE CL-PD-INSACCERN(5) TO QCL-PD-INSACCERN-5.
           MOVE CL-PD-INSEARN-ORDER(5) TO QCL-PD-INSEARN-ORDER-5.
           MOVE CL-PD-INSFINCHG(5) TO QCL-PD-INSFINCHG-5.
           MOVE CL-PD-INSCOMP(6) TO QCL-PD-INSCOMP-6.
           MOVE CL-PD-INSOURS(6) TO QCL-PD-INSOURS-6.

           MOVE CL-PD-INSEFF-DATE(6) TO QCL-PD-INSEFF-DATE-6.

           IF ( CL-PD-INSEXP-DATE(6) = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-INSEXP-DATE-6
           ELSE
              MOVE CL-PD-INSEXP-DATE(6) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-INSEXP-DATE-6.

           MOVE CL-PD-INSPREM(6) TO QCL-PD-INSPREM-6.
           MOVE CL-PD-INSCOVR(6) TO QCL-PD-INSCOVR-6.
           MOVE CL-PD-INSCOMM(6) TO QCL-PD-INSCOMM-6.
           MOVE CL-PD-INSANTICERN(6) TO QCL-PD-INSANTICERN-6.
           MOVE CL-PD-INSANTICADJ(6) TO QCL-PD-INSANTICADJ-6.
           MOVE CL-PD-INSUFERN(6) TO QCL-PD-INSUFERN-6.
           MOVE CL-PD-INSACCERN(6) TO QCL-PD-INSACCERN-6.
           MOVE CL-PD-INSEARN-ORDER(6) TO QCL-PD-INSEARN-ORDER-6.
           MOVE CL-PD-INSFINCHG(6) TO QCL-PD-INSFINCHG-6.
           MOVE CL-PD-INSCOMP(7) TO QCL-PD-INSCOMP-7.
           MOVE CL-PD-INSOURS(7) TO QCL-PD-INSOURS-7.

           MOVE CL-PD-INSEFF-DATE(7) TO QCL-PD-INSEFF-DATE-7.

           IF ( CL-PD-INSEXP-DATE(7) = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-INSEXP-DATE-7
           ELSE
              MOVE CL-PD-INSEXP-DATE(7) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-INSEXP-DATE-7.

           MOVE CL-PD-INSPREM(7) TO QCL-PD-INSPREM-7.
           MOVE CL-PD-INSCOVR(7) TO QCL-PD-INSCOVR-7.
           MOVE CL-PD-INSCOMM(7) TO QCL-PD-INSCOMM-7.
           MOVE CL-PD-INSANTICERN(7) TO QCL-PD-INSANTICERN-7.
           MOVE CL-PD-INSANTICADJ(7) TO QCL-PD-INSANTICADJ-7.
           MOVE CL-PD-INSUFERN(7) TO QCL-PD-INSUFERN-7.
           MOVE CL-PD-INSACCERN(7) TO QCL-PD-INSACCERN-7.
           MOVE CL-PD-INSEARN-ORDER(7) TO QCL-PD-INSEARN-ORDER-7.
           MOVE CL-PD-INSFINCHG(7) TO QCL-PD-INSFINCHG-7.
           MOVE CL-PD-INSCOMP(8) TO QCL-PD-INSCOMP-8.
           MOVE CL-PD-INSOURS(8) TO QCL-PD-INSOURS-8.

           MOVE CL-PD-INSEFF-DATE(8) TO QCL-PD-INSEFF-DATE-8.

           IF ( CL-PD-INSEXP-DATE(8) = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-INSEXP-DATE-8
           ELSE
              MOVE CL-PD-INSEXP-DATE(8) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-INSEXP-DATE-8.

           MOVE CL-PD-INSPREM(8) TO QCL-PD-INSPREM-8.
           MOVE CL-PD-INSCOVR(8) TO QCL-PD-INSCOVR-8.
           MOVE CL-PD-INSCOMM(8) TO QCL-PD-INSCOMM-8.
           MOVE CL-PD-INSANTICERN(8) TO QCL-PD-INSANTICERN-8.
           MOVE CL-PD-INSANTICADJ(8) TO QCL-PD-INSANTICADJ-8.
           MOVE CL-PD-INSUFERN(8) TO QCL-PD-INSUFERN-8.
           MOVE CL-PD-INSACCERN(8) TO QCL-PD-INSACCERN-8.
           MOVE CL-PD-INSEARN-ORDER(8) TO QCL-PD-INSEARN-ORDER-8.
           MOVE CL-PD-INSFINCHG(8) TO QCL-PD-INSFINCHG-8.
           MOVE CL-PD-INSCOMP(9) TO QCL-PD-INSCOMP-9.
           MOVE CL-PD-INSOURS(9) TO QCL-PD-INSOURS-9.

           MOVE CL-PD-INSEFF-DATE(9) TO QCL-PD-INSEFF-DATE-9.

           IF ( CL-PD-INSEXP-DATE(9) = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-INSEXP-DATE-9
           ELSE
              MOVE CL-PD-INSEXP-DATE(9) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-INSEXP-DATE-9.

           MOVE CL-PD-INSPREM(9) TO QCL-PD-INSPREM-9.
           MOVE CL-PD-INSCOVR(9) TO QCL-PD-INSCOVR-9.
           MOVE CL-PD-INSCOMM(9) TO QCL-PD-INSCOMM-9.
           MOVE CL-PD-INSANTICERN(9) TO QCL-PD-INSANTICERN-9.
           MOVE CL-PD-INSANTICADJ(9) TO QCL-PD-INSANTICADJ-9.
           MOVE CL-PD-INSUFERN(9) TO QCL-PD-INSUFERN-9.
           MOVE CL-PD-INSACCERN(9) TO QCL-PD-INSACCERN-9.
           MOVE CL-PD-INSEARN-ORDER(9) TO QCL-PD-INSEARN-ORDER-9.
           MOVE CL-PD-INSFINCHG(9) TO QCL-PD-INSFINCHG-9.
           MOVE CL-PD-INSCOMP(10) TO QCL-PD-INSCOMP-10.
           MOVE CL-PD-INSOURS(10) TO QCL-PD-INSOURS-10.

           MOVE CL-PD-INSEFF-DATE(10) TO QCL-PD-INSEFF-DATE-10.

           IF ( CL-PD-INSEXP-DATE(10) = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-INSEXP-DATE-10
           ELSE
              MOVE CL-PD-INSEXP-DATE(10) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-INSEXP-DATE-10.

           MOVE CL-PD-INSPREM(10) TO QCL-PD-INSPREM-10.
           MOVE CL-PD-INSCOVR(10) TO QCL-PD-INSCOVR-10.
           MOVE CL-PD-INSCOMM(10) TO QCL-PD-INSCOMM-10.
           MOVE CL-PD-INSANTICERN(10) TO QCL-PD-INSANTICERN-10.
           MOVE CL-PD-INSANTICADJ(10) TO QCL-PD-INSANTICADJ-10.
           MOVE CL-PD-INSUFERN(10) TO QCL-PD-INSUFERN-10.
           MOVE CL-PD-INSACCERN(10) TO QCL-PD-INSACCERN-10.
           MOVE CL-PD-INSEARN-ORDER(10) TO QCL-PD-INSEARN-ORDER-10.
           MOVE CL-PD-INSFINCHG(10) TO QCL-PD-INSFINCHG-10.
           MOVE CL-PD-PY-YYYYMM-RECDEL TO QCL-PD-PY-YYYYMM-RECDEL.
           MOVE CL-PD-MTD-PAYS-RECDEL TO QCL-PD-MTD-PAYS-RECDEL.
           MOVE CL-PD-GARNISH-EXTR TO QCL-PD-GARNISH-EXTR.
           MOVE CL-PD-SHIP-SAME-FG TO QCL-PD-SHIP-SAME-FG.
           MOVE CL-PD-AD-D-INS-COVR TO QCL-PD-AD-D-INS-COVR.
           MOVE CL-PD-TOT-PARTIAL-CHGOFF TO QCL-PD-TOT-PARTIAL-CHGOFF.
           MOVE CL-PD-LPO TO QCL-PD-LPO.
           MOVE CL-PD-REP-CD TO QCL-PD-REP-CD.
           MOVE CL-PD-EMP-ID TO QCL-PD-EMP-ID.
           MOVE CL-PD-PROCESS-CD TO QCL-PD-PROCESS-CD.
           MOVE CL-PD-WCBC-CREDITLIM TO QCL-PD-WCBC-CREDITLIM.
           MOVE CL-PD-FOLLOWUP-CD TO QCL-PD-FOLLOWUP-CD.
           MOVE CL-PD-MIP-FG TO QCL-PD-MIP-FG.
           MOVE CL-PD-MIP-MTD-CLAHPAID TO QCL-PD-MIP-MTD-CLAHPAID.

           IF ( CL-PD-MIP-CLPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-MIP-CLPDTH-DATE
           ELSE
              MOVE CL-PD-MIP-CLPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-MIP-CLPDTH-DATE.

           MOVE CL-PD-MIP-TOT-CLPAID TO QCL-PD-MIP-TOT-CLPAID.

           IF ( CL-PD-MIP-AHPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-MIP-AHPDTH-DATE
           ELSE
              MOVE CL-PD-MIP-AHPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-MIP-AHPDTH-DATE.

           MOVE CL-PD-MIP-TOT-AHPAID TO QCL-PD-MIP-TOT-AHPAID.
           MOVE CL-PD-IL-GROSS-INCOME TO QCL-PD-IL-GROSS-INCOME.
           MOVE CL-PD-IL-GROSS-INCOME2 TO QCL-PD-IL-GROSS-INCOME2.
           MOVE CL-PD-RESCISSION TO QCL-PD-RESCISSION.
           MOVE CL-PD-INTERIM-TRW TO QCL-PD-INTERIM-TRW.
           MOVE CL-PD-DLNO TO QCL-PD-DLNO.
           MOVE CL-PD-DLPOOL TO QCL-PD-DLPOOL.
           MOVE CL-PD-DLRECOURSE TO QCL-PD-DLRECOURSE.
           MOVE CL-PD-DLPARTRCAMT TO QCL-PD-DLPARTRCAMT.
           MOVE CL-PD-ORIGPURAMT TO QCL-PD-ORIGPURAMT.
           MOVE CL-PD-PURBAL TO QCL-PD-PURBAL.
           MOVE CL-PD-DLRVPRIN(1) TO QCL-PD-DLRVPRIN-1.
           MOVE CL-PD-DLRVPRIN(2) TO QCL-PD-DLRVPRIN-2.
           MOVE CL-PD-DLRVPRIN(3) TO QCL-PD-DLRVPRIN-3.
           MOVE CL-PD-DLRVCHGS(1) TO QCL-PD-DLRVCHGS-1.
           MOVE CL-PD-DLRVCHGS(2) TO QCL-PD-DLRVCHGS-2.
           MOVE CL-PD-DLRVCHGS(3) TO QCL-PD-DLRVCHGS-3.
           MOVE CL-PD-DLPARVHELD TO QCL-PD-DLPARVHELD.
           MOVE CL-PD-DLPARVPAID TO QCL-PD-DLPARVPAID.
           MOVE CL-PD-DLDISC TO QCL-PD-DLDISC.

           IF ( CL-PD-LCPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-LCPDTH-DATE
           ELSE
              MOVE CL-PD-LCPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-LCPDTH-DATE.


           IF ( CL-PD-INTPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-INTPDTH-DATE
           ELSE
              MOVE CL-PD-INTPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-INTPDTH-DATE.

           MOVE CL-PD-CURBAL TO QCL-PD-CURBAL.
           MOVE CL-PD-INTBAL TO QCL-PD-INTBAL.
           MOVE CL-PD-LCBAL TO QCL-PD-LCBAL.
           MOVE CL-PD-OTHBAL TO QCL-PD-OTHBAL.
           MOVE CL-PD-OT2BAL TO QCL-PD-OT2BAL.
           MOVE CL-PD-LCPAID TO QCL-PD-LCPAID.
           MOVE CL-PD-LCPARTIALS TO QCL-PD-LCPARTIALS.
           MOVE CL-PD-STMT-COUNTER TO QCL-PD-STMT-COUNTER.
           MOVE CL-PD-PRIOR-STMT-COUNTER TO QCL-PD-PRIOR-STMT-COUNTER.
           MOVE CL-PD-FROZEN-REASON TO QCL-PD-FROZEN-REASON.
           MOVE CL-PD-COUPON-STATEMENT TO QCL-PD-COUPON-STATEMENT.
           MOVE CL-PD-MONTHEND-BAL TO QCL-PD-MONTHEND-BAL.
           MOVE CL-PD-ANTICTERM TO QCL-PD-ANTICTERM.
           MOVE CL-PD-ANTICERN(1) TO QCL-PD-ANTICERN-1.
           MOVE CL-PD-ANTICADJ(1) TO QCL-PD-ANTICADJ-1.
           MOVE CL-PD-UFERN(1) TO QCL-PD-UFERN-1.
           MOVE CL-PD-ACCERN(1) TO QCL-PD-ACCERN-1.
           MOVE CL-PD-EARN-ORDER(1) TO QCL-PD-EARN-ORDER-1.
           MOVE CL-PD-ANTICERN(2) TO QCL-PD-ANTICERN-2.
           MOVE CL-PD-ANTICADJ(2) TO QCL-PD-ANTICADJ-2.
           MOVE CL-PD-UFERN(2) TO QCL-PD-UFERN-2.
           MOVE CL-PD-ACCERN(2) TO QCL-PD-ACCERN-2.
           MOVE CL-PD-EARN-ORDER(2) TO QCL-PD-EARN-ORDER-2.
           MOVE CL-PD-ANTICERN(3) TO QCL-PD-ANTICERN-3.
           MOVE CL-PD-ANTICADJ(3) TO QCL-PD-ANTICADJ-3.
           MOVE CL-PD-UFERN(3) TO QCL-PD-UFERN-3.
           MOVE CL-PD-ACCERN(3) TO QCL-PD-ACCERN-3.
           MOVE CL-PD-EARN-ORDER(3) TO QCL-PD-EARN-ORDER-3.
           MOVE CL-PD-ANTICERN(4) TO QCL-PD-ANTICERN-4.
           MOVE CL-PD-ANTICADJ(4) TO QCL-PD-ANTICADJ-4.
           MOVE CL-PD-UFERN(4) TO QCL-PD-UFERN-4.
           MOVE CL-PD-ACCERN(4) TO QCL-PD-ACCERN-4.
           MOVE CL-PD-EARN-ORDER(4) TO QCL-PD-EARN-ORDER-4.
           MOVE CL-PD-ANTICERN(5) TO QCL-PD-ANTICERN-5.
           MOVE CL-PD-ANTICADJ(5) TO QCL-PD-ANTICADJ-5.
           MOVE CL-PD-UFERN(5) TO QCL-PD-UFERN-5.
           MOVE CL-PD-ACCERN(5) TO QCL-PD-ACCERN-5.
           MOVE CL-PD-EARN-ORDER(5) TO QCL-PD-EARN-ORDER-5.
           MOVE CL-PD-ANTICERN(6) TO QCL-PD-ANTICERN-6.
           MOVE CL-PD-ANTICADJ(6) TO QCL-PD-ANTICADJ-6.
           MOVE CL-PD-UFERN(6) TO QCL-PD-UFERN-6.
           MOVE CL-PD-ACCERN(6) TO QCL-PD-ACCERN-6.
           MOVE CL-PD-EARN-ORDER(6) TO QCL-PD-EARN-ORDER-6.
           MOVE CL-PD-ANTICERN(7) TO QCL-PD-ANTICERN-7.
           MOVE CL-PD-ANTICADJ(7) TO QCL-PD-ANTICADJ-7.
           MOVE CL-PD-UFERN(7) TO QCL-PD-UFERN-7.
           MOVE CL-PD-ACCERN(7) TO QCL-PD-ACCERN-7.
           MOVE CL-PD-EARN-ORDER(7) TO QCL-PD-EARN-ORDER-7.
           MOVE CL-PD-ANTICERN(8) TO QCL-PD-ANTICERN-8.
           MOVE CL-PD-ANTICADJ(8) TO QCL-PD-ANTICADJ-8.
           MOVE CL-PD-UFERN(8) TO QCL-PD-UFERN-8.
           MOVE CL-PD-ACCERN(8) TO QCL-PD-ACCERN-8.
           MOVE CL-PD-EARN-ORDER(8) TO QCL-PD-EARN-ORDER-8.
           MOVE CL-PD-ANTICERN(9) TO QCL-PD-ANTICERN-9.
           MOVE CL-PD-ANTICADJ(9) TO QCL-PD-ANTICADJ-9.
           MOVE CL-PD-UFERN(9) TO QCL-PD-UFERN-9.
           MOVE CL-PD-ACCERN(9) TO QCL-PD-ACCERN-9.
           MOVE CL-PD-EARN-ORDER(9) TO QCL-PD-EARN-ORDER-9.
           MOVE CL-PD-ANTICERN(10) TO QCL-PD-ANTICERN-10.
           MOVE CL-PD-ANTICADJ(10) TO QCL-PD-ANTICADJ-10.
           MOVE CL-PD-UFERN(10) TO QCL-PD-UFERN-10.
           MOVE CL-PD-ACCERN(10) TO QCL-PD-ACCERN-10.
           MOVE CL-PD-EARN-ORDER(10) TO QCL-PD-EARN-ORDER-10.
           MOVE CL-PD-LMO-PRIOR-INTEREST TO QCL-PD-LMO-PRIOR-INTEREST.
           MOVE CL-PD-PRIOR-UNEARNED TO QCL-PD-PRIOR-UNEARNED.
           MOVE CL-PD-ERNDELQ-STOP TO QCL-PD-ERNDELQ-STOP.
           MOVE CL-PD-NONACRERN TO QCL-PD-NONACRERN.
           MOVE CL-PD-PRIOR-NONACRERN TO QCL-PD-PRIOR-NONACRERN.

           IF ( CL-PD-ERN-IB-PDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-ERN-IB-PDTH-DATE
           ELSE
              MOVE CL-PD-ERN-IB-PDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-ERN-IB-PDTH-DATE.

           MOVE CL-PD-ERN-IB-PRINBAL TO QCL-PD-ERN-IB-PRINBAL.
           MOVE CL-PD-ERN-IB-INTOWE TO QCL-PD-ERN-IB-INTOWE.
           MOVE CL-PD-TOTERN TO QCL-PD-TOTERN.
           MOVE CL-PD-TOTOTH TO QCL-PD-TOTOTH.
           MOVE CL-PD-TOTINT TO QCL-PD-TOTINT.
           MOVE CL-PD-TOTLCHG TO QCL-PD-TOTLCHG.
           MOVE CL-PD-TOTDEF TO QCL-PD-TOTDEF.
           MOVE CL-PD-TOTSERACCT TO QCL-PD-TOTSERACCT.
           MOVE CL-PD-LCWAIVERS TO QCL-PD-LCWAIVERS.
           MOVE CL-PD-INTWAIVERS TO QCL-PD-INTWAIVERS.
           MOVE CL-PD-TOTPAYMNTD TO QCL-PD-TOTPAYMNTD.
           MOVE CL-PD-TOTEXCPAYMNTD TO QCL-PD-TOTEXCPAYMNTD.
           MOVE CL-PD-TOTNODEF TO QCL-PD-TOTNODEF.
           MOVE CL-PD-YTDNODEF TO QCL-PD-YTDNODEF.
           MOVE CL-PD-TOTNODEFACT TO QCL-PD-TOTNODEFACT.
           MOVE CL-PD-YTDNODEFACT TO QCL-PD-YTDNODEFACT.
           MOVE CL-PD-TOTNOALDEL TO QCL-PD-TOTNOALDEL.
           MOVE CL-PD-YTDNOALDEL TO QCL-PD-YTDNOALDEL.
           MOVE CL-PD-YTDINTPD TO QCL-PD-YTDINTPD.
           MOVE CL-PD-LYRINTPD TO QCL-PD-LYRINTPD.
           MOVE CL-PD-TOTINTPD TO QCL-PD-TOTINTPD.
           MOVE CL-PD-RECASTINT TO QCL-PD-RECASTINT.
           MOVE CL-PD-RECASTPRIN TO QCL-PD-RECASTPRIN.
           MOVE CL-PD-CL-REBATE TO QCL-PD-CL-REBATE.
           MOVE CL-PD-AH-REBATE TO QCL-PD-AH-REBATE.
           MOVE CL-PD-PP-REBATE TO QCL-PD-PP-REBATE.
           MOVE CL-PD-INT-REBATE TO QCL-PD-INT-REBATE.
           MOVE CL-PD-SC-REBATE TO QCL-PD-SC-REBATE.
           MOVE CL-PD-MF-REBATE TO QCL-PD-MF-REBATE.
           MOVE CL-PD-DF-REBATE TO QCL-PD-DF-REBATE.
           MOVE CL-PD-EX-REBATE TO QCL-PD-EX-REBATE.
           MOVE CL-PD-O1-REBATE TO QCL-PD-O1-REBATE.
           MOVE CL-PD-O2-REBATE TO QCL-PD-O2-REBATE.
           MOVE CL-PD-O3-REBATE TO QCL-PD-O3-REBATE.
           MOVE CL-PD-O4-REBATE TO QCL-PD-O4-REBATE.
           MOVE CL-PD-O5-REBATE TO QCL-PD-O5-REBATE.
           MOVE CL-PD-O6-REBATE TO QCL-PD-O6-REBATE.
           MOVE CL-PD-O7-REBATE TO QCL-PD-O7-REBATE.
           MOVE CL-PD-UN-O7-REB TO QCL-PD-UN-O7-REB.
           MOVE CL-PD-UN-O8-REB TO QCL-PD-UN-O8-REB.
           MOVE CL-PD-UN-O9-REB TO QCL-PD-UN-O9-REB.
           MOVE CL-PD-UN-O10-REB TO QCL-PD-UN-O10-REB.
           MOVE CL-PD-PLREASON TO QCL-PD-PLREASON.
           MOVE CL-PD-COLLECTOR TO QCL-PD-COLLECTOR.
           MOVE CL-PD-ACTIONCD TO QCL-PD-ACTIONCD.
           MOVE CL-PD-ACTIONCD2 TO QCL-PD-ACTIONCD2.
           MOVE CL-PD-ATCODE TO QCL-PD-ATCODE.
           MOVE CL-PD-RECODE TO QCL-PD-RECODE.
           MOVE CL-PD-ESCROW-FG TO QCL-PD-ESCROW-FG.
           MOVE CL-PD-PROMDAYS-PASTDUE TO QCL-PD-PROMDAYS-PASTDUE.
           MOVE CL-PD-PROMID TO QCL-PD-PROMID.

           IF ( CL-PD-PROMDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-PROMDATE
           ELSE
              MOVE CL-PD-PROMDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-PROMDATE.

           MOVE CL-PD-PROMAMT TO QCL-PD-PROMAMT.

           IF ( CL-PD-PROMDATE2 = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-PROMDATE2
           ELSE
              MOVE CL-PD-PROMDATE2 TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-PROMDATE2.


           IF ( CL-PD-DATE-PAID-LAST = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-DATE-PAID-LAST
           ELSE
              MOVE CL-PD-DATE-PAID-LAST TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-DATE-PAID-LAST.

           MOVE CL-PD-AMT-PAID-LAST TO QCL-PD-AMT-PAID-LAST.
           MOVE CL-PD-LASTCFORMNO TO QCL-PD-LASTCFORMNO.

           IF ( CL-PD-LASTCFORMDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-LASTCFORMDATE
           ELSE
              MOVE CL-PD-LASTCFORMDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-LASTCFORMDATE.

           MOVE CL-PD-DELCNT TO QCL-PD-DELCNT.
           MOVE CL-PD-DELCNT2 TO QCL-PD-DELCNT2.
           MOVE CL-PD-CREDITLIM TO QCL-PD-CREDITLIM.
           MOVE CL-PD-CRSCORE TO QCL-PD-CRSCORE.
           MOVE CL-PD-LASTSFORMNO TO QCL-PD-LASTSFORMNO.

           IF ( CL-PD-LASTSFORMDATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-LASTSFORMDATE
           ELSE
              MOVE CL-PD-LASTSFORMDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-LASTSFORMDATE.

           MOVE CL-PD-SOLCNT TO QCL-PD-SOLCNT.
           MOVE CL-PD-MESSAGE TO QCL-PD-MESSAGE.
           MOVE CL-PD-OLDACCTNO TO QCL-PD-OLDACCTNO.
           MOVE CL-PD-PRLNNO TO QCL-PD-PRLNNO.
           MOVE CL-PD-PRBAL TO QCL-PD-PRBAL.
           MOVE CL-PD-PRINTDUE TO QCL-PD-PRINTDUE.
           MOVE CL-PD-PRFINCHGRB TO QCL-PD-PRFINCHGRB.
           MOVE CL-PD-PRINSRB TO QCL-PD-PRINSRB.
           MOVE CL-PD-PRPOCD TO QCL-PD-PRPOCD.

           IF ( CL-PD-PRPODATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-PRPODATE
           ELSE
              MOVE CL-PD-PRPODATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-PRPODATE.

           MOVE CL-PD-PRDELINQAMT TO QCL-PD-PRDELINQAMT.
           MOVE CL-PD-PRLNAMT TO QCL-PD-PRLNAMT.
           MOVE CL-PD-PRPDUE-DAYS TO QCL-PD-PRPDUE-DAYS.
           MOVE CL-PD-PRDLDISC-UNEARN TO QCL-PD-PRDLDISC-UNEARN.
           MOVE CL-PD-PRCONPROFILE TO QCL-PD-PRCONPROFILE.
           MOVE CL-PD-SUM-ORIGPURAMT TO QCL-PD-SUM-ORIGPURAMT.
           MOVE CL-PD-SUM-LNAMT TO QCL-PD-SUM-LNAMT.
           MOVE CL-PD-SUM-FINCHG TO QCL-PD-SUM-FINCHG.
           MOVE CL-PD-SUM-INTCHG TO QCL-PD-SUM-INTCHG.
           MOVE CL-PD-SUM-ORGTERM TO QCL-PD-SUM-ORGTERM.
           MOVE CL-PD-SUM-REGPYAMT TO QCL-PD-SUM-REGPYAMT.
           MOVE CL-PD-SUM-EXTCHG TO QCL-PD-SUM-EXTCHG.
           MOVE CL-PD-SUM-SERCHG TO QCL-PD-SUM-SERCHG.
           MOVE CL-PD-SUM-MAINTFEE TO QCL-PD-SUM-MAINTFEE.
           MOVE CL-PD-UNITPER-CD TO QCL-PD-UNITPER-CD.
           MOVE CL-PD-UNITPER-FREQ TO QCL-PD-UNITPER-FREQ.
           MOVE CL-PD-DAYS-IN-UNITPER TO QCL-PD-DAYS-IN-UNITPER.
           MOVE CL-PD-UNITPER-PER-YEAR TO QCL-PD-UNITPER-PER-YEAR.
           MOVE CL-PD-REPOCD TO QCL-PD-REPOCD.
           MOVE CL-PD-BR-TFR-FG TO QCL-PD-BR-TFR-FG.
           MOVE CL-PD-O3INS-DLPROC TO QCL-PD-O3INS-DLPROC.
           MOVE CL-PD-INSPREM-DLADJ TO QCL-PD-INSPREM-DLADJ.
           MOVE CL-PD-PLAMT TO QCL-PD-PLAMT.
           MOVE CL-PD-MTD-PRIN TO QCL-PD-MTD-PRIN.
           MOVE CL-PD-MTD-LC TO QCL-PD-MTD-LC.
           MOVE CL-PD-MTD-INT TO QCL-PD-MTD-INT.
           MOVE CL-PD-MTD-OT TO QCL-PD-MTD-OT.
           MOVE CL-PD-MTD-EX TO QCL-PD-MTD-EX.
           MOVE CL-PD-BNKRPT-CHAPTER TO QCL-PD-BNKRPT-CHAPTER.
           MOVE CL-PD-BMO-CON-STATUS TO QCL-PD-BMO-CON-STATUS.
           MOVE CL-PD-BMO-REC-STATUS TO QCL-PD-BMO-REC-STATUS.
           MOVE CL-PD-APP-NUMBER TO QCL-PD-APP-NUMBER.
           MOVE CL-PD-GIFTNET TO QCL-PD-GIFTNET.
           MOVE CL-PD-GIFTNET-OUR-PO TO QCL-PD-GIFTNET-OUR-PO.
           MOVE CL-PD-PROC-DISBURSED TO QCL-PD-PROC-DISBURSED.
           MOVE CL-PD-NOODDPY TO QCL-PD-NOODDPY.
           MOVE CL-PD-CONPROFILE(1) TO QCL-PD-CONPROFILE-1.
           MOVE CL-PD-CONPROFILE(2) TO QCL-PD-CONPROFILE-2.
           MOVE CL-PD-CONPROFILE(3) TO QCL-PD-CONPROFILE-3.
           MOVE CL-PD-CONPROFILE(4) TO QCL-PD-CONPROFILE-4.
           MOVE CL-PD-CONPROFILE(5) TO QCL-PD-CONPROFILE-5.
           MOVE CL-PD-RECPROFILE(1) TO QCL-PD-RECPROFILE-1.
           MOVE CL-PD-RECPROFILE(2) TO QCL-PD-RECPROFILE-2.
           MOVE CL-PD-RECPROFILE(3) TO QCL-PD-RECPROFILE-3.
           MOVE CL-PD-RECPROFILE(4) TO QCL-PD-RECPROFILE-4.
           MOVE CL-PD-RECPROFILE(5) TO QCL-PD-RECPROFILE-5.
           MOVE CL-PD-TRW-STATUS TO QCL-PD-TRW-STATUS.
           MOVE CL-PD-ORIGIN TO QCL-PD-ORIGIN.
           MOVE CL-PD-PO-TRCD TO QCL-PD-PO-TRCD.
           MOVE CL-PD-NEXIS-CRSCORE TO QCL-PD-NEXIS-CRSCORE.
           MOVE CL-PD-TRW-B-CONSUMER-IND TO QCL-PD-TRW-B-CONSUMER-IND.
           MOVE CL-PD-PARVHELD-PAID-FG TO QCL-PD-PARVHELD-PAID-FG.
           MOVE CL-PD-ACCT-FROZEN TO QCL-PD-ACCT-FROZEN.
           MOVE CL-PD-TRW-SPECIAL-CODE TO QCL-PD-TRW-SPECIAL-CODE.
           MOVE CL-PD-CREATION-STATUS TO QCL-PD-CREATION-STATUS.
           MOVE CL-PD-TOTNOLCHG TO QCL-PD-TOTNOLCHG.
           MOVE CL-PD-LBOX TO QCL-PD-LBOX.
           MOVE CL-PD-EXTRA-PRIN-LFP TO QCL-PD-EXTRA-PRIN-LFP.
           MOVE CL-PD-REMAIN-INT-SINCE-LFP
            TO QCL-PD-REMAIN-INT-SINCE-LFP.
           MOVE CL-PD-REMAIN-PRIN-SINCE-LFP
            TO QCL-PD-REMAIN-PRIN-SINCE-LFP.
           MOVE CL-PD-INTERIM-COUPON TO QCL-PD-INTERIM-COUPON.
           MOVE CL-PD-DLCOMM-O3-CMSI TO QCL-PD-DLCOMM-O3-CMSI.
           MOVE CL-PD-DLCOMM-PP-CMSI TO QCL-PD-DLCOMM-PP-CMSI.
           MOVE CL-PD-TRW-J-CONSUMER-IND TO QCL-PD-TRW-J-CONSUMER-IND.
           MOVE CL-PD-ACH TO QCL-PD-ACH.
           MOVE CL-PD-1099-C-FG TO QCL-PD-1099-C-FG.
           MOVE CL-PD-TRW-COMPLIANCE-CODE
            TO QCL-PD-TRW-COMPLIANCE-CODE.
           MOVE CL-PD-UNWIND-REASON-CODE TO QCL-PD-UNWIND-REASON-CODE.
           MOVE CL-PD-PBI-PBD-FG TO QCL-PD-PBI-PBD-FG.
           MOVE CL-PD-OLD-ACCT-FORMAT-FG TO QCL-PD-OLD-ACCT-FORMAT-FG.
           MOVE CL-PD-APPROVED-BY-USER TO QCL-PD-APPROVED-BY-USER.
           MOVE CL-PD-APPROVED-BY-CODE TO QCL-PD-APPROVED-BY-CODE.

           IF ( CL-PD-APPROVED-BY-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-APPROVED-BY-DATE
           ELSE
              MOVE CL-PD-APPROVED-BY-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-APPROVED-BY-DATE.

           MOVE CL-PD-APPROVED-BY-TIME TO QCL-PD-APPROVED-BY-TIME.

           IF ( CL-PD-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCL-PD-LTOUCH-DATE
           ELSE
              MOVE CL-PD-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCL-PD-LTOUCH-DATE.

           MOVE CL-PD-LTOUCH-TIME TO QCL-PD-LTOUCH-TIME.
           MOVE CL-PD-HOLD-COLLATERAL TO QCL-PD-HOLD-COLLATERAL.
           MOVE CL-PD-DLPAID TO QCL-PD-DLPAID.
           MOVE CL-PD-CKNO TO QCL-PD-CKNO.
           MOVE CL-PD-FREE1 TO QCL-PD-FREE1.
           MOVE CL-PD-FREE2 TO QCL-PD-FREE2.
           MOVE CL-PD-FREE-SPACE-FILLER TO QCL-PD-FREE-SPACE-FILLER.
           MOVE CL-PD-POSTING-BR TO QCL-PD-POSTING-BR.
           MOVE CL-PD-OTHBR-FG TO QCL-PD-OTHBR-FG.
           MOVE CL-PD-APPROVAL-LIMIT TO QCL-PD-APPROVAL-LIMIT.
           MOVE CL-PD-APPROVAL-USERID TO QCL-PD-APPROVAL-USERID.
           MOVE CL-PD-MIN-CRED-SCORE TO QCL-PD-MIN-CRED-SCORE.
           MOVE CL-PD-CRED-SCORE-USERID TO QCL-PD-CRED-SCORE-USERID.
           MOVE CL-PD-FINAMT TO QCL-PD-FINAMT.
           MOVE CL-PD-PROCEEDS-FG TO QCL-PD-PROCEEDS-FG.
           MOVE CL-PD-POINTS-FG TO QCL-PD-POINTS-FG.
           MOVE CL-PD-OVRDRATE-FG TO QCL-PD-OVRDRATE-FG.
           MOVE CL-PD-INS-FG(1) TO QCL-PD-INS-FG-1.
           MOVE CL-PD-INS-FG(2) TO QCL-PD-INS-FG-2.
           MOVE CL-PD-INS-FG(3) TO QCL-PD-INS-FG-3.
           MOVE CL-PD-INS-FG(4) TO QCL-PD-INS-FG-4.
           MOVE CL-PD-INS-FG(5) TO QCL-PD-INS-FG-5.
           MOVE CL-PD-INS-FG(6) TO QCL-PD-INS-FG-6.
           MOVE CL-PD-INS-FG(7) TO QCL-PD-INS-FG-7.
           MOVE CL-PD-INS-FG(8) TO QCL-PD-INS-FG-8.
           MOVE CL-PD-INS-FG(9) TO QCL-PD-INS-FG-9.
           MOVE CL-PD-INS-FG(10) TO QCL-PD-INS-FG-10.
           MOVE CL-PD-SERCHG-FG TO QCL-PD-SERCHG-FG.
           MOVE CL-PD-MAINTFEE-FG TO QCL-PD-MAINTFEE-FG.
           MOVE CL-PD-EXTCHG-FG TO QCL-PD-EXTCHG-FG.
           MOVE CL-PD-REGPYAMT-FG TO QCL-PD-REGPYAMT-FG.
           MOVE CL-PD-FEECD(1) TO QCL-PD-FEECD-1.
           MOVE CL-PD-FEECD(2) TO QCL-PD-FEECD-2.
           MOVE CL-PD-FEECD(3) TO QCL-PD-FEECD-3.
           MOVE CL-PD-FEECD(4) TO QCL-PD-FEECD-4.
           MOVE CL-PD-FEECD(5) TO QCL-PD-FEECD-5.
           MOVE CL-PD-FEECD(6) TO QCL-PD-FEECD-6.
           MOVE CL-PD-FEECD(7) TO QCL-PD-FEECD-7.
           MOVE CL-PD-FEECD(8) TO QCL-PD-FEECD-8.
           MOVE CL-PD-FEECD(9) TO QCL-PD-FEECD-9.
           MOVE CL-PD-FEECD(10) TO QCL-PD-FEECD-10.
           MOVE CL-PD-FEECD(11) TO QCL-PD-FEECD-11.
           MOVE CL-PD-FEECD(12) TO QCL-PD-FEECD-12.
           MOVE CL-PD-FEECD(13) TO QCL-PD-FEECD-13.
           MOVE CL-PD-FEECD(14) TO QCL-PD-FEECD-14.
           MOVE CL-PD-FEECD(15) TO QCL-PD-FEECD-15.
           MOVE CL-PD-FEECD(16) TO QCL-PD-FEECD-16.
           MOVE CL-PD-FEECD(17) TO QCL-PD-FEECD-17.
           MOVE CL-PD-FEECD(18) TO QCL-PD-FEECD-18.
           MOVE CL-PD-FEECD(19) TO QCL-PD-FEECD-19.
           MOVE CL-PD-FEECD(20) TO QCL-PD-FEECD-20.
           MOVE CL-PD-PRPAID-AHEAD TO QCL-PD-PRPAID-AHEAD.
           MOVE CL-PD-RESTRICT-ADJ TO QCL-PD-RESTRICT-ADJ.
           MOVE CL-PD-DUAL-LOAN-ADJ TO QCL-PD-DUAL-LOAN-ADJ.
           MOVE CL-PD-CAN-LOAN-MAX TO QCL-PD-CAN-LOAN-MAX.
           MOVE CL-PD-PERDOWN TO QCL-PD-PERDOWN.
           MOVE CL-PD-SELLPR-FG TO QCL-PD-SELLPR-FG.
           MOVE CL-PD-DOWNPY-FG TO QCL-PD-DOWNPY-FG.
           MOVE CL-PD-PERDOWN-FG TO QCL-PD-PERDOWN-FG.
           MOVE CL-PD-SELL-DOWN-FG TO QCL-PD-SELL-DOWN-FG.
           MOVE CL-PD-BALLOON-FG TO QCL-PD-BALLOON-FG.
           MOVE CL-PD-BALLOON-LAST-FG TO QCL-PD-BALLOON-LAST-FG.
           MOVE CL-PD-FEECD-FG(1) TO QCL-PD-FEECD-FG-1.
           MOVE CL-PD-FEECD-FG(2) TO QCL-PD-FEECD-FG-2.
           MOVE CL-PD-INS-TERM-SW(1) TO QCL-PD-INS-TERM-SW-1.
           MOVE CL-PD-COVERAGE-SW(1) TO QCL-PD-COVERAGE-SW-1.
           MOVE CL-PD-PREMIUM-SW(1) TO QCL-PD-PREMIUM-SW-1.
           MOVE CL-PD-INS-TERM-SW(2) TO QCL-PD-INS-TERM-SW-2.
           MOVE CL-PD-COVERAGE-SW(2) TO QCL-PD-COVERAGE-SW-2.
           MOVE CL-PD-PREMIUM-SW(2) TO QCL-PD-PREMIUM-SW-2.
           MOVE CL-PD-INS-TERM-SW(3) TO QCL-PD-INS-TERM-SW-3.
           MOVE CL-PD-COVERAGE-SW(3) TO QCL-PD-COVERAGE-SW-3.
           MOVE CL-PD-PREMIUM-SW(3) TO QCL-PD-PREMIUM-SW-3.
           MOVE CL-PD-INS-TERM-SW(4) TO QCL-PD-INS-TERM-SW-4.
           MOVE CL-PD-COVERAGE-SW(4) TO QCL-PD-COVERAGE-SW-4.
           MOVE CL-PD-PREMIUM-SW(4) TO QCL-PD-PREMIUM-SW-4.
           MOVE CL-PD-INS-TERM-SW(5) TO QCL-PD-INS-TERM-SW-5.
           MOVE CL-PD-COVERAGE-SW(5) TO QCL-PD-COVERAGE-SW-5.
           MOVE CL-PD-PREMIUM-SW(5) TO QCL-PD-PREMIUM-SW-5.
           MOVE CL-PD-INS-TERM-SW(6) TO QCL-PD-INS-TERM-SW-6.
           MOVE CL-PD-COVERAGE-SW(6) TO QCL-PD-COVERAGE-SW-6.
           MOVE CL-PD-PREMIUM-SW(6) TO QCL-PD-PREMIUM-SW-6.
           MOVE CL-PD-INS-TERM-SW(7) TO QCL-PD-INS-TERM-SW-7.
           MOVE CL-PD-COVERAGE-SW(7) TO QCL-PD-COVERAGE-SW-7.
           MOVE CL-PD-PREMIUM-SW(7) TO QCL-PD-PREMIUM-SW-7.
           MOVE CL-PD-INS-TERM-SW(8) TO QCL-PD-INS-TERM-SW-8.
           MOVE CL-PD-COVERAGE-SW(8) TO QCL-PD-COVERAGE-SW-8.
           MOVE CL-PD-PREMIUM-SW(8) TO QCL-PD-PREMIUM-SW-8.
           MOVE CL-PD-INS-TERM-SW(9) TO QCL-PD-INS-TERM-SW-9.
           MOVE CL-PD-COVERAGE-SW(9) TO QCL-PD-COVERAGE-SW-9.
           MOVE CL-PD-PREMIUM-SW(9) TO QCL-PD-PREMIUM-SW-9.
           MOVE CL-PD-INS-TERM-SW(10) TO QCL-PD-INS-TERM-SW-10.
           MOVE CL-PD-COVERAGE-SW(10) TO QCL-PD-COVERAGE-SW-10.
           MOVE CL-PD-PREMIUM-SW(10) TO QCL-PD-PREMIUM-SW-10.
           MOVE CL-PD-DLDISC-FG TO QCL-PD-DLDISC-FG.
           MOVE CL-PD-DLRVPRIN-FG(1) TO QCL-PD-DLRVPRIN-FG-1.
           MOVE CL-PD-DLRVPRIN-FG(2) TO QCL-PD-DLRVPRIN-FG-2.
           MOVE CL-PD-DLRVPRIN-FG(3) TO QCL-PD-DLRVPRIN-FG-3.
           MOVE CL-PD-DLRVCHGS-FG(1) TO QCL-PD-DLRVCHGS-FG-1.
           MOVE CL-PD-DLRVCHGS-FG(2) TO QCL-PD-DLRVCHGS-FG-2.
           MOVE CL-PD-DLRVCHGS-FG(3) TO QCL-PD-DLRVCHGS-FG-3.
           MOVE CL-PD-DLPARVHELD-FG TO QCL-PD-DLPARVHELD-FG.
           MOVE CL-PD-DLPARVPAID-FG TO QCL-PD-DLPARVPAID-FG.
           MOVE CL-PD-ONE-MTH-NO-DAY-EXT-SW
            TO QCL-PD-ONE-MTH-NO-DAY-EXT-SW.
           MOVE CL-PD-ASSIGN-RN-ACCTNO TO QCL-PD-ASSIGN-RN-ACCTNO.
           MOVE CL-PD-APR-FINAMT TO QCL-PD-APR-FINAMT.
           MOVE CL-PD-DLCOMM-PP-CMSI-FG TO QCL-PD-DLCOMM-PP-CMSI-FG.
           MOVE CL-PD-DLCOMM-O3-CMSI-FG TO QCL-PD-DLCOMM-O3-CMSI-FG.
           MOVE CL-PD-FILLER-TO-MATCH-LN TO QCL-PD-FILLER-TO-MATCH-LN.
           MOVE CL-PD2-ID TO QCL-PD2-ID.
           MOVE CL-PD2-BRANCH TO QCL-PD2-BRANCH.
           MOVE CL-PD2-BWNAME TO QCL-PD2-BWNAME.
 

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBGB/GB%%1RN
       LOAD-CL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CL-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CL-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO CL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CL-ALL-MACHINE
              MOVE CL-ALL-PATH             TO CL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CL-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO CL-ALL-BASE
              MOVE FILPATH-MACHINE         TO CL-ALL-MACHINE
              MOVE CL-ALL-PATH             TO CL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-CLFILE NOT = "S" )
              MOVE CL-PATH-BEG             TO CL-SELECT-PATH-BEG
              MOVE CL-PATH-FILE            TO CL-SELECT-PATH-FILE
           ELSE
              MOVE CL-PATH-SQL             TO CL-SELECT-PATH.


      *-----------------------------------------------------------------
       OPEN-CL1-FILE.
           PERFORM LOAD-CL1-FILE.
           PERFORM OPEN-IT.
           MOVE CL-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CL1-FILE.
           PERFORM START-IT.
           MOVE CL-SELECT-PATH TO E-FILE.
           MOVE CL1-KEY TO E-KEYX.

           IF ( CL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CL1-FILE.

           IF ( QCL1-WEND-CAN-NO = SPACES )
              MOVE CL-GROUP-ID          TO QCL1-WBEG-GROUP
              MOVE ALL "Z"              TO QCL1-WEND-GROUP
              MOVE CL-CAN-NO            TO QCL1-WBEG-CAN-NO
              MOVE ALL "Z"              TO QCL1-WEND-CAN-NO.

           EXEC SQL
            DECLARE CL1_CURSOR CURSOR FOR
            SELECT ISNULL(CLFILE.CL_GROUP_ID,''),
                   ISNULL(CLFILE.CL_CAN_NO,''),
                   ISNULL(CAST(CLFILE.CL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CLFILE.CL_DESC,''),
                   ISNULL(CLFILE.CL_GIFTNET,''),
                   ISNULL(CLFILE.CL_PD_ID,''),
                   CLFILE.CL_PD_BRANCH,
                   CLFILE.CL_PD_SOC_SEC,
                   CLFILE.CL_PD_OWNBR,
                   CLFILE.CL_PD_ACCTNO,
                   CLFILE.CL_PD_CLASS,
                   CLFILE.CL_PD_SUBCLASS,
                   CLFILE.CL_PD_LAWCODE,
                   CLFILE.CL_PD_SSNO1,
                   ISNULL(CLFILE.CL_PD_MAKERCD1,''),
                   CLFILE.CL_PD_SSNO2,
                   ISNULL(CLFILE.CL_PD_MAKERCD2,''),
                   CLFILE.CL_PD_SSNO3,
                   ISNULL(CLFILE.CL_PD_MAKERCD3,''),
                   CLFILE.CL_PD_SSNO4,
                   ISNULL(CLFILE.CL_PD_MAKERCD4,''),
                   ISNULL(CLFILE.CL_PD_ORGST,''),
                   CLFILE.CL_PD_ORGBR,
                   ISNULL(CLFILE.CL_PD_OWNST,''),
                   CLFILE.CL_PD_SPRCLASS,
                   ISNULL(CLFILE.CL_PD_CONVERCD,''),
                   ISNULL(CLFILE.CL_PD_MIDSTREAM,''),
                   ISNULL(CLFILE.CL_PD_PURCD,''),
                   ISNULL(CLFILE.CL_PD_SRCD,''),
                   ISNULL(CLFILE.CL_PD_COLLCD,''),
                   ISNULL(CLFILE.CL_PD_COLLID,''),
                   ISNULL(CLFILE.CL_PD_POCD,''),
                   ISNULL(CLFILE.CL_PD_PLCD,''),
                   ISNULL(CLFILE.CL_PD_PLTYPE,''),
                   ISNULL(CLFILE.CL_PD_CONFIDENTCD,''),
                   ISNULL(CLFILE.CL_PD_ALLOTCD,''),
                   ISNULL(CLFILE.CL_PD_RECASTCD,''),
                   ISNULL(CLFILE.CL_PD_SKIP_TRW,''),
                   ISNULL(CLFILE.CL_PD_STATUSFG,''),
                   ISNULL(CLFILE.CL_PD_SMALLOAN,''),
                   ISNULL(CLFILE.CL_PD_SALEFIN,''),
                   ISNULL(CLFILE.CL_PD_SERVACCT,''),
                   ISNULL(CLFILE.CL_PD_LOANTYPE,''),
                   ISNULL(CLFILE.CL_PD_ADDON_FG,''),
                   ISNULL(CLFILE.CL_PD_PAY_SCHLD_FG,''),
                   CLFILE.CL_PD_LC_CODE,
                   ISNULL(CLFILE.CL_PD_CAN_LOAN_NO,''),
                   ISNULL(CLFILE.CL_PD_STATFLAGS,''),
                   ISNULL(CAST(CLFILE.CL_PD_ENTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PURDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_CASHDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_FILEDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_JDDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PLDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_REDUDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_POFFDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_REPODATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_SOLDDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_APRATE,
                   CLFILE.CL_PD_EFFRATE,
                   CLFILE.CL_PD_SMPRATE,
                   CLFILE.CL_PD_YLDRATE,
                   CLFILE.CL_PD_OVRDRATE,
                   CLFILE.CL_PD_ENDORATE,
                   CLFILE.CL_PD_JDRATE,
                   CLFILE.CL_PD_ACCRATE,
                   CLFILE.CL_PD_REDURATE,
                   CLFILE.CL_PD_CONRATE,
                   CLFILE.CL_PD_SERVPER,
                   CLFILE.CL_PD_SELLPR,
                   CLFILE.CL_PD_DOWNPY,
                   CLFILE.CL_PD_TRADEIN,
                   CLFILE.CL_PD_LNAMT,
                   CLFILE.CL_PD_FINCHG,
                   CLFILE.CL_PD_INTCHG,
                   CLFILE.CL_PD_SERCHG,
                   CLFILE.CL_PD_EXTCHG,
                   CLFILE.CL_PD_MAINTFEE,
                   CLFILE.CL_PD_POINTS,
                   CLFILE.CL_PD_PROCEEDS,
                   CLFILE.CL_PD_ORGTERM,
                   CLFILE.CL_PD_1STPYAMT,
                   CLFILE.CL_PD_NOREGPY,
                   CLFILE.CL_PD_REGPYAMT,
                   CLFILE.CL_PD_LASTPYAMT,
                   CLFILE.CL_PD_PROCASH,
                   CLFILE.CL_PD_TILLNO,
                   ISNULL(CLFILE.CL_PD_STATE_FLAG,''),
                   CLFILE.CL_PD_STATE_WORKER,
                   CLFILE.CL_PD_STATE_WORKER2,
                   ISNULL(CLFILE.CL_PD_FEECODE_1,''),
                   CLFILE.CL_PD_FEEAMT_1,
                   ISNULL(CLFILE.CL_PD_FEEOURS_1,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_1,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_2,''),
                   CLFILE.CL_PD_FEEAMT_2,
                   ISNULL(CLFILE.CL_PD_FEEOURS_2,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_2,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_3,''),
                   CLFILE.CL_PD_FEEAMT_3,
                   ISNULL(CLFILE.CL_PD_FEEOURS_3,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_3,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_4,''),
                   CLFILE.CL_PD_FEEAMT_4,
                   ISNULL(CLFILE.CL_PD_FEEOURS_4,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_4,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_5,''),
                   CLFILE.CL_PD_FEEAMT_5,
                   ISNULL(CLFILE.CL_PD_FEEOURS_5,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_5,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_6,''),
                   CLFILE.CL_PD_FEEAMT_6,
                   ISNULL(CLFILE.CL_PD_FEEOURS_6,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_6,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_7,''),
                   CLFILE.CL_PD_FEEAMT_7,
                   ISNULL(CLFILE.CL_PD_FEEOURS_7,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_7,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_8,''),
                   CLFILE.CL_PD_FEEAMT_8,
                   ISNULL(CLFILE.CL_PD_FEEOURS_8,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_8,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_9,''),
                   CLFILE.CL_PD_FEEAMT_9,
                   ISNULL(CLFILE.CL_PD_FEEOURS_9,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_9,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_10,''),
                   CLFILE.CL_PD_FEEAMT_10,
                   ISNULL(CLFILE.CL_PD_FEEOURS_10,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_10,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_11,''),
                   CLFILE.CL_PD_FEEAMT_11,
                   ISNULL(CLFILE.CL_PD_FEEOURS_11,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_11,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_12,''),
                   CLFILE.CL_PD_FEEAMT_12,
                   ISNULL(CLFILE.CL_PD_FEEOURS_12,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_12,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_13,''),
                   CLFILE.CL_PD_FEEAMT_13,
                   ISNULL(CLFILE.CL_PD_FEEOURS_13,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_13,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_14,''),
                   CLFILE.CL_PD_FEEAMT_14,
                   ISNULL(CLFILE.CL_PD_FEEOURS_14,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_14,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_15,''),
                   CLFILE.CL_PD_FEEAMT_15,
                   ISNULL(CLFILE.CL_PD_FEEOURS_15,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_15,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_16,''),
                   CLFILE.CL_PD_FEEAMT_16,
                   ISNULL(CLFILE.CL_PD_FEEOURS_16,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_16,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_17,''),
                   CLFILE.CL_PD_FEEAMT_17,
                   ISNULL(CLFILE.CL_PD_FEEOURS_17,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_17,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_18,''),
                   CLFILE.CL_PD_FEEAMT_18,
                   ISNULL(CLFILE.CL_PD_FEEOURS_18,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_18,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_19,''),
                   CLFILE.CL_PD_FEEAMT_19,
                   ISNULL(CLFILE.CL_PD_FEEOURS_19,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_19,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_20,''),
                   CLFILE.CL_PD_FEEAMT_20,
                   ISNULL(CLFILE.CL_PD_FEEOURS_20,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_20,''),
                   ISNULL(CLFILE.CL_PD_INSURED,''),
                   ISNULL(CLFILE.CL_PD_INSTYPES,''),
                   ISNULL(CLFILE.CL_PD_INSELIGIBLE,''),
                   CLFILE.CL_PD_INSCOMP_1,
                   ISNULL(CLFILE.CL_PD_INSOURS_1,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_1,
                   CLFILE.CL_PD_INSCOVR_1,
                   CLFILE.CL_PD_INSCOMM_1,
                   CLFILE.CL_PD_INSANTICERN_1,
                   CLFILE.CL_PD_INSANTICADJ_1,
                   CLFILE.CL_PD_INSUFERN_1,
                   CLFILE.CL_PD_INSACCERN_1,
                   CLFILE.CL_PD_INSEARN_ORDER_1,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_1,''),
                   CLFILE.CL_PD_INSCOMP_2,
                   ISNULL(CLFILE.CL_PD_INSOURS_2,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_2,
                   CLFILE.CL_PD_INSCOVR_2,
                   CLFILE.CL_PD_INSCOMM_2,
                   CLFILE.CL_PD_INSANTICERN_2,
                   CLFILE.CL_PD_INSANTICADJ_2,
                   CLFILE.CL_PD_INSUFERN_2,
                   CLFILE.CL_PD_INSACCERN_2,
                   CLFILE.CL_PD_INSEARN_ORDER_2,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_2,''),
                   CLFILE.CL_PD_INSCOMP_3,
                   ISNULL(CLFILE.CL_PD_INSOURS_3,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_3,
                   CLFILE.CL_PD_INSCOVR_3,
                   CLFILE.CL_PD_INSCOMM_3,
                   CLFILE.CL_PD_INSANTICERN_3,
                   CLFILE.CL_PD_INSANTICADJ_3,
                   CLFILE.CL_PD_INSUFERN_3,
                   CLFILE.CL_PD_INSACCERN_3,
                   CLFILE.CL_PD_INSEARN_ORDER_3,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_3,''),
                   CLFILE.CL_PD_INSCOMP_4,
                   ISNULL(CLFILE.CL_PD_INSOURS_4,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_4,
                   CLFILE.CL_PD_INSCOVR_4,
                   CLFILE.CL_PD_INSCOMM_4,
                   CLFILE.CL_PD_INSANTICERN_4,
                   CLFILE.CL_PD_INSANTICADJ_4,
                   CLFILE.CL_PD_INSUFERN_4,
                   CLFILE.CL_PD_INSACCERN_4,
                   CLFILE.CL_PD_INSEARN_ORDER_4,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_4,''),
                   CLFILE.CL_PD_INSCOMP_5,
                   ISNULL(CLFILE.CL_PD_INSOURS_5,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_5,
                   CLFILE.CL_PD_INSCOVR_5,
                   CLFILE.CL_PD_INSCOMM_5,
                   CLFILE.CL_PD_INSANTICERN_5,
                   CLFILE.CL_PD_INSANTICADJ_5,
                   CLFILE.CL_PD_INSUFERN_5,
                   CLFILE.CL_PD_INSACCERN_5,
                   CLFILE.CL_PD_INSEARN_ORDER_5,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_5,''),
                   CLFILE.CL_PD_INSCOMP_6,
                   ISNULL(CLFILE.CL_PD_INSOURS_6,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_6,
                   CLFILE.CL_PD_INSCOVR_6,
                   CLFILE.CL_PD_INSCOMM_6,
                   CLFILE.CL_PD_INSANTICERN_6,
                   CLFILE.CL_PD_INSANTICADJ_6,
                   CLFILE.CL_PD_INSUFERN_6,
                   CLFILE.CL_PD_INSACCERN_6,
                   CLFILE.CL_PD_INSEARN_ORDER_6,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_6,''),
                   CLFILE.CL_PD_INSCOMP_7,
                   ISNULL(CLFILE.CL_PD_INSOURS_7,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_7,
                   CLFILE.CL_PD_INSCOVR_7,
                   CLFILE.CL_PD_INSCOMM_7,
                   CLFILE.CL_PD_INSANTICERN_7,
                   CLFILE.CL_PD_INSANTICADJ_7,
                   CLFILE.CL_PD_INSUFERN_7,
                   CLFILE.CL_PD_INSACCERN_7,
                   CLFILE.CL_PD_INSEARN_ORDER_7,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_7,''),
                   CLFILE.CL_PD_INSCOMP_8,
                   ISNULL(CLFILE.CL_PD_INSOURS_8,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_8,
                   CLFILE.CL_PD_INSCOVR_8,
                   CLFILE.CL_PD_INSCOMM_8,
                   CLFILE.CL_PD_INSANTICERN_8,
                   CLFILE.CL_PD_INSANTICADJ_8,
                   CLFILE.CL_PD_INSUFERN_8,
                   CLFILE.CL_PD_INSACCERN_8,
                   CLFILE.CL_PD_INSEARN_ORDER_8,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_8,''),
                   CLFILE.CL_PD_INSCOMP_9,
                   ISNULL(CLFILE.CL_PD_INSOURS_9,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_9,
                   CLFILE.CL_PD_INSCOVR_9,
                   CLFILE.CL_PD_INSCOMM_9,
                   CLFILE.CL_PD_INSANTICERN_9,
                   CLFILE.CL_PD_INSANTICADJ_9,
                   CLFILE.CL_PD_INSUFERN_9,
                   CLFILE.CL_PD_INSACCERN_9,
                   CLFILE.CL_PD_INSEARN_ORDER_9,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_9,''),
                   CLFILE.CL_PD_INSCOMP_10,
                   ISNULL(CLFILE.CL_PD_INSOURS_10,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_10,
                   CLFILE.CL_PD_INSCOVR_10,
                   CLFILE.CL_PD_INSCOMM_10,
                   CLFILE.CL_PD_INSANTICERN_10,
                   CLFILE.CL_PD_INSANTICADJ_10,
                   CLFILE.CL_PD_INSUFERN_10,
                   CLFILE.CL_PD_INSACCERN_10,
                   CLFILE.CL_PD_INSEARN_ORDER_10,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_10,''),
                   CLFILE.CL_PD_PY_YYYYMM_RECDEL,
                   CLFILE.CL_PD_MTD_PAYS_RECDEL,
                   ISNULL(CLFILE.CL_PD_GARNISH_EXTR,''),
                   ISNULL(CLFILE.CL_PD_SHIP_SAME_FG,''),
                   CLFILE.CL_PD_AD_D_INS_COVR,
                   CLFILE.CL_PD_TOT_PARTIAL_CHGOFF,
                   ISNULL(CLFILE.CL_PD_LPO,''),
                   ISNULL(CLFILE.CL_PD_REP_CD,''),
                   CLFILE.CL_PD_EMP_ID,
                   ISNULL(CLFILE.CL_PD_PROCESS_CD,''),
                   CLFILE.CL_PD_WCBC_CREDITLIM,
                   ISNULL(CLFILE.CL_PD_FOLLOWUP_CD,''),
                   ISNULL(CLFILE.CL_PD_MIP_FG,''),
                   CLFILE.CL_PD_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(CLFILE.CL_PD_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_MIP_TOT_CLPAID,
                   ISNULL(CAST(CLFILE.CL_PD_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_MIP_TOT_AHPAID,
                   CLFILE.CL_PD_IL_GROSS_INCOME,
                   CLFILE.CL_PD_IL_GROSS_INCOME2,
                   ISNULL(CLFILE.CL_PD_RESCISSION,''),
                   ISNULL(CLFILE.CL_PD_INTERIM_TRW,''),
                   CLFILE.CL_PD_DLNO,
                   CLFILE.CL_PD_DLPOOL,
                   ISNULL(CLFILE.CL_PD_DLRECOURSE,''),
                   CLFILE.CL_PD_DLPARTRCAMT,
                   CLFILE.CL_PD_ORIGPURAMT,
                   CLFILE.CL_PD_PURBAL,
                   CLFILE.CL_PD_DLRVPRIN_1,
                   CLFILE.CL_PD_DLRVPRIN_2,
                   CLFILE.CL_PD_DLRVPRIN_3,
                   CLFILE.CL_PD_DLRVCHGS_1,
                   CLFILE.CL_PD_DLRVCHGS_2,
                   CLFILE.CL_PD_DLRVCHGS_3,
                   CLFILE.CL_PD_DLPARVHELD,
                   CLFILE.CL_PD_DLPARVPAID,
                   CLFILE.CL_PD_DLDISC,
                   ISNULL(CAST(CLFILE.CL_PD_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_CURBAL,
                   CLFILE.CL_PD_INTBAL,
                   CLFILE.CL_PD_LCBAL,
                   CLFILE.CL_PD_OTHBAL,
                   CLFILE.CL_PD_OT2BAL,
                   CLFILE.CL_PD_LCPAID,
                   CLFILE.CL_PD_LCPARTIALS,
                   CLFILE.CL_PD_STMT_COUNTER,
                   CLFILE.CL_PD_PRIOR_STMT_COUNTER,
                   ISNULL(CLFILE.CL_PD_FROZEN_REASON,''),
                   ISNULL(CLFILE.CL_PD_COUPON_STATEMENT,''),
                   CLFILE.CL_PD_MONTHEND_BAL,
                   CLFILE.CL_PD_ANTICTERM,
                   CLFILE.CL_PD_ANTICERN_1,
                   CLFILE.CL_PD_ANTICADJ_1,
                   CLFILE.CL_PD_UFERN_1,
                   CLFILE.CL_PD_ACCERN_1,
                   CLFILE.CL_PD_EARN_ORDER_1,
                   CLFILE.CL_PD_ANTICERN_2,
                   CLFILE.CL_PD_ANTICADJ_2,
                   CLFILE.CL_PD_UFERN_2,
                   CLFILE.CL_PD_ACCERN_2,
                   CLFILE.CL_PD_EARN_ORDER_2,
                   CLFILE.CL_PD_ANTICERN_3,
                   CLFILE.CL_PD_ANTICADJ_3,
                   CLFILE.CL_PD_UFERN_3,
                   CLFILE.CL_PD_ACCERN_3,
                   CLFILE.CL_PD_EARN_ORDER_3,
                   CLFILE.CL_PD_ANTICERN_4,
                   CLFILE.CL_PD_ANTICADJ_4,
                   CLFILE.CL_PD_UFERN_4,
                   CLFILE.CL_PD_ACCERN_4,
                   CLFILE.CL_PD_EARN_ORDER_4,
                   CLFILE.CL_PD_ANTICERN_5,
                   CLFILE.CL_PD_ANTICADJ_5,
                   CLFILE.CL_PD_UFERN_5,
                   CLFILE.CL_PD_ACCERN_5,
                   CLFILE.CL_PD_EARN_ORDER_5,
                   CLFILE.CL_PD_ANTICERN_6,
                   CLFILE.CL_PD_ANTICADJ_6,
                   CLFILE.CL_PD_UFERN_6,
                   CLFILE.CL_PD_ACCERN_6,
                   CLFILE.CL_PD_EARN_ORDER_6,
                   CLFILE.CL_PD_ANTICERN_7,
                   CLFILE.CL_PD_ANTICADJ_7,
                   CLFILE.CL_PD_UFERN_7,
                   CLFILE.CL_PD_ACCERN_7,
                   CLFILE.CL_PD_EARN_ORDER_7,
                   CLFILE.CL_PD_ANTICERN_8,
                   CLFILE.CL_PD_ANTICADJ_8,
                   CLFILE.CL_PD_UFERN_8,
                   CLFILE.CL_PD_ACCERN_8,
                   CLFILE.CL_PD_EARN_ORDER_8,
                   CLFILE.CL_PD_ANTICERN_9,
                   CLFILE.CL_PD_ANTICADJ_9,
                   CLFILE.CL_PD_UFERN_9,
                   CLFILE.CL_PD_ACCERN_9,
                   CLFILE.CL_PD_EARN_ORDER_9,
                   CLFILE.CL_PD_ANTICERN_10,
                   CLFILE.CL_PD_ANTICADJ_10,
                   CLFILE.CL_PD_UFERN_10,
                   CLFILE.CL_PD_ACCERN_10,
                   CLFILE.CL_PD_EARN_ORDER_10,
                   CLFILE.CL_PD_LMO_PRIOR_INTEREST,
                   CLFILE.CL_PD_PRIOR_UNEARNED,
                   ISNULL(CLFILE.CL_PD_ERNDELQ_STOP,''),
                   CLFILE.CL_PD_NONACRERN,
                   CLFILE.CL_PD_PRIOR_NONACRERN,
                   ISNULL(CAST(CLFILE.CL_PD_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_ERN_IB_PRINBAL,
                   CLFILE.CL_PD_ERN_IB_INTOWE,
                   CLFILE.CL_PD_TOTERN,
                   CLFILE.CL_PD_TOTOTH,
                   CLFILE.CL_PD_TOTINT,
                   CLFILE.CL_PD_TOTLCHG,
                   CLFILE.CL_PD_TOTDEF,
                   CLFILE.CL_PD_TOTSERACCT,
                   CLFILE.CL_PD_LCWAIVERS,
                   CLFILE.CL_PD_INTWAIVERS,
                   CLFILE.CL_PD_TOTPAYMNTD,
                   CLFILE.CL_PD_TOTEXCPAYMNTD,
                   CLFILE.CL_PD_TOTNODEF,
                   CLFILE.CL_PD_YTDNODEF,
                   CLFILE.CL_PD_TOTNODEFACT,
                   CLFILE.CL_PD_YTDNODEFACT,
                   CLFILE.CL_PD_TOTNOALDEL,
                   CLFILE.CL_PD_YTDNOALDEL,
                   CLFILE.CL_PD_YTDINTPD,
                   CLFILE.CL_PD_LYRINTPD,
                   CLFILE.CL_PD_TOTINTPD,
                   CLFILE.CL_PD_RECASTINT,
                   CLFILE.CL_PD_RECASTPRIN,
                   ISNULL(CLFILE.CL_PD_CL_REBATE,''),
                   ISNULL(CLFILE.CL_PD_AH_REBATE,''),
                   ISNULL(CLFILE.CL_PD_PP_REBATE,''),
                   ISNULL(CLFILE.CL_PD_INT_REBATE,''),
                   ISNULL(CLFILE.CL_PD_SC_REBATE,''),
                   ISNULL(CLFILE.CL_PD_MF_REBATE,''),
                   ISNULL(CLFILE.CL_PD_DF_REBATE,''),
                   ISNULL(CLFILE.CL_PD_EX_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O1_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O2_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O3_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O4_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O5_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O6_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O7_REBATE,''),
                   ISNULL(CLFILE.CL_PD_UN_O7_REB,''),
                   ISNULL(CLFILE.CL_PD_UN_O8_REB,''),
                   ISNULL(CLFILE.CL_PD_UN_O9_REB,''),
                   ISNULL(CLFILE.CL_PD_UN_O10_REB,''),
                   ISNULL(CLFILE.CL_PD_PLREASON,''),
                   ISNULL(CLFILE.CL_PD_COLLECTOR,''),
                   ISNULL(CLFILE.CL_PD_ACTIONCD,''),
                   ISNULL(CLFILE.CL_PD_ACTIONCD2,''),
                   ISNULL(CLFILE.CL_PD_ATCODE,''),
                   ISNULL(CLFILE.CL_PD_RECODE,''),
                   ISNULL(CLFILE.CL_PD_ESCROW_FG,''),
                   CLFILE.CL_PD_PROMDAYS_PASTDUE,
                   ISNULL(CLFILE.CL_PD_PROMID,''),
                   ISNULL(CAST(CLFILE.CL_PD_PROMDATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_PROMAMT,
                   ISNULL(CAST(CLFILE.CL_PD_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_AMT_PAID_LAST,
                   CLFILE.CL_PD_LASTCFORMNO,
                   ISNULL(CAST(CLFILE.CL_PD_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_DELCNT,
                   CLFILE.CL_PD_DELCNT2,
                   CLFILE.CL_PD_CREDITLIM,
                   CLFILE.CL_PD_CRSCORE,
                   CLFILE.CL_PD_LASTSFORMNO,
                   ISNULL(CAST(CLFILE.CL_PD_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_SOLCNT,
                   ISNULL(CLFILE.CL_PD_MESSAGE,''),
                   CLFILE.CL_PD_OLDACCTNO,
                   CLFILE.CL_PD_PRLNNO,
                   CLFILE.CL_PD_PRBAL,
                   CLFILE.CL_PD_PRINTDUE,
                   CLFILE.CL_PD_PRFINCHGRB,
                   CLFILE.CL_PD_PRINSRB,
                   ISNULL(CLFILE.CL_PD_PRPOCD,''),
                   ISNULL(CAST(CLFILE.CL_PD_PRPODATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_PRDELINQAMT,
                   CLFILE.CL_PD_PRLNAMT,
                   CLFILE.CL_PD_PRPDUE_DAYS,
                   CLFILE.CL_PD_PRDLDISC_UNEARN,
                   CLFILE.CL_PD_PRCONPROFILE,
                   CLFILE.CL_PD_SUM_ORIGPURAMT,
                   CLFILE.CL_PD_SUM_LNAMT,
                   CLFILE.CL_PD_SUM_FINCHG,
                   CLFILE.CL_PD_SUM_INTCHG,
                   CLFILE.CL_PD_SUM_ORGTERM,
                   CLFILE.CL_PD_SUM_REGPYAMT,
                   CLFILE.CL_PD_SUM_EXTCHG,
                   CLFILE.CL_PD_SUM_SERCHG,
                   CLFILE.CL_PD_SUM_MAINTFEE,
                   ISNULL(CLFILE.CL_PD_UNITPER_CD,''),
                   CLFILE.CL_PD_UNITPER_FREQ,
                   CLFILE.CL_PD_DAYS_IN_UNITPER,
                   CLFILE.CL_PD_UNITPER_PER_YEAR,
                   ISNULL(CLFILE.CL_PD_REPOCD,''),
                   ISNULL(CLFILE.CL_PD_BR_TFR_FG,''),
                   CLFILE.CL_PD_O3INS_DLPROC,
                   CLFILE.CL_PD_INSPREM_DLADJ,
                   CLFILE.CL_PD_PLAMT,
                   CLFILE.CL_PD_MTD_PRIN,
                   CLFILE.CL_PD_MTD_LC,
                   CLFILE.CL_PD_MTD_INT,
                   CLFILE.CL_PD_MTD_OT,
                   CLFILE.CL_PD_MTD_EX,
                   CLFILE.CL_PD_BNKRPT_CHAPTER,
                   ISNULL(CLFILE.CL_PD_BMO_CON_STATUS,''),
                   ISNULL(CLFILE.CL_PD_BMO_REC_STATUS,''),
                   CLFILE.CL_PD_APP_NUMBER,
                   ISNULL(CLFILE.CL_PD_GIFTNET,''),
                   CLFILE.CL_PD_GIFTNET_OUR_PO,
                   ISNULL(CLFILE.CL_PD_PROC_DISBURSED,''),
                   CLFILE.CL_PD_NOODDPY,
                   CLFILE.CL_PD_CONPROFILE_1,
                   CLFILE.CL_PD_CONPROFILE_2,
                   CLFILE.CL_PD_CONPROFILE_3,
                   CLFILE.CL_PD_CONPROFILE_4,
                   CLFILE.CL_PD_CONPROFILE_5,
                   CLFILE.CL_PD_RECPROFILE_1,
                   CLFILE.CL_PD_RECPROFILE_2,
                   CLFILE.CL_PD_RECPROFILE_3,
                   CLFILE.CL_PD_RECPROFILE_4,
                   CLFILE.CL_PD_RECPROFILE_5,
                   ISNULL(CLFILE.CL_PD_TRW_STATUS,''),
                   ISNULL(CLFILE.CL_PD_ORIGIN,''),
                   ISNULL(CLFILE.CL_PD_PO_TRCD,''),
                   CLFILE.CL_PD_NEXIS_CRSCORE,
                   ISNULL(CLFILE.CL_PD_TRW_B_CONSUMER_IND,''),
                   ISNULL(CLFILE.CL_PD_PARVHELD_PAID_FG,''),
                   ISNULL(CLFILE.CL_PD_ACCT_FROZEN,''),
                   ISNULL(CLFILE.CL_PD_TRW_SPECIAL_CODE,''),
                   ISNULL(CLFILE.CL_PD_CREATION_STATUS,''),
                   CLFILE.CL_PD_TOTNOLCHG,
                   ISNULL(CLFILE.CL_PD_LBOX,''),
                   CLFILE.CL_PD_EXTRA_PRIN_LFP,
                   CLFILE.CL_PD_REMAIN_INT_SINCE_LFP,
                   CLFILE.CL_PD_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(CLFILE.CL_PD_INTERIM_COUPON,''),
                   CLFILE.CL_PD_DLCOMM_O3_CMSI,
                   CLFILE.CL_PD_DLCOMM_PP_CMSI,
                   ISNULL(CLFILE.CL_PD_TRW_J_CONSUMER_IND,''),
                   ISNULL(CLFILE.CL_PD_ACH,''),
                   ISNULL(CLFILE.CL_PD_1099_C_FG,''),
                   ISNULL(CLFILE.CL_PD_TRW_COMPLIANCE_CODE,''),
                   ISNULL(CLFILE.CL_PD_UNWIND_REASON_CODE,''),
                   ISNULL(CLFILE.CL_PD_PBI_PBD_FG,''),
                   ISNULL(CLFILE.CL_PD_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(CLFILE.CL_PD_APPROVED_BY_USER,''),
                   ISNULL(CLFILE.CL_PD_APPROVED_BY_CODE,''),
                   ISNULL(CAST(CLFILE.CL_PD_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_APPROVED_BY_TIME,
                   ISNULL(CAST(CLFILE.CL_PD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_LTOUCH_TIME,
                   ISNULL(CLFILE.CL_PD_HOLD_COLLATERAL,''),
                   ISNULL(CLFILE.CL_PD_DLPAID,''),
                   CLFILE.CL_PD_CKNO,
                   ISNULL(CLFILE.CL_PD_FREE1,''),
                   ISNULL(CLFILE.CL_PD_FREE2,''),
                   ISNULL(CLFILE.CL_PD_FREE_SPACE_FILLER,''),
                   CLFILE.CL_PD_POSTING_BR,
                   ISNULL(CLFILE.CL_PD_OTHBR_FG,''),
                   CLFILE.CL_PD_APPROVAL_LIMIT,
                   ISNULL(CLFILE.CL_PD_APPROVAL_USERID,''),
                   CLFILE.CL_PD_MIN_CRED_SCORE,
                   ISNULL(CLFILE.CL_PD_CRED_SCORE_USERID,''),
                   CLFILE.CL_PD_FINAMT,
                   ISNULL(CLFILE.CL_PD_PROCEEDS_FG,''),
                   ISNULL(CLFILE.CL_PD_POINTS_FG,''),
                   ISNULL(CLFILE.CL_PD_OVRDRATE_FG,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_1,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_2,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_3,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_4,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_5,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_6,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_7,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_8,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_9,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_10,''),
                   ISNULL(CLFILE.CL_PD_SERCHG_FG,''),
                   ISNULL(CLFILE.CL_PD_MAINTFEE_FG,''),
                   ISNULL(CLFILE.CL_PD_EXTCHG_FG,''),
                   ISNULL(CLFILE.CL_PD_REGPYAMT_FG,''),
                   ISNULL(CLFILE.CL_PD_FEECD_1,''),
                   ISNULL(CLFILE.CL_PD_FEECD_2,''),
                   ISNULL(CLFILE.CL_PD_FEECD_3,''),
                   ISNULL(CLFILE.CL_PD_FEECD_4,''),
                   ISNULL(CLFILE.CL_PD_FEECD_5,''),
                   ISNULL(CLFILE.CL_PD_FEECD_6,''),
                   ISNULL(CLFILE.CL_PD_FEECD_7,''),
                   ISNULL(CLFILE.CL_PD_FEECD_8,''),
                   ISNULL(CLFILE.CL_PD_FEECD_9,''),
                   ISNULL(CLFILE.CL_PD_FEECD_10,''),
                   ISNULL(CLFILE.CL_PD_FEECD_11,''),
                   ISNULL(CLFILE.CL_PD_FEECD_12,''),
                   ISNULL(CLFILE.CL_PD_FEECD_13,''),
                   ISNULL(CLFILE.CL_PD_FEECD_14,''),
                   ISNULL(CLFILE.CL_PD_FEECD_15,''),
                   ISNULL(CLFILE.CL_PD_FEECD_16,''),
                   ISNULL(CLFILE.CL_PD_FEECD_17,''),
                   ISNULL(CLFILE.CL_PD_FEECD_18,''),
                   ISNULL(CLFILE.CL_PD_FEECD_19,''),
                   ISNULL(CLFILE.CL_PD_FEECD_20,''),
                   CLFILE.CL_PD_PRPAID_AHEAD,
                   CLFILE.CL_PD_RESTRICT_ADJ,
                   CLFILE.CL_PD_DUAL_LOAN_ADJ,
                   CLFILE.CL_PD_CAN_LOAN_MAX,
                   CLFILE.CL_PD_PERDOWN,
                   ISNULL(CLFILE.CL_PD_SELLPR_FG,''),
                   ISNULL(CLFILE.CL_PD_DOWNPY_FG,''),
                   ISNULL(CLFILE.CL_PD_PERDOWN_FG,''),
                   ISNULL(CLFILE.CL_PD_SELL_DOWN_FG,''),
                   ISNULL(CLFILE.CL_PD_BALLOON_FG,''),
                   ISNULL(CLFILE.CL_PD_BALLOON_LAST_FG,''),
                   ISNULL(CLFILE.CL_PD_FEECD_FG_1,''),
                   ISNULL(CLFILE.CL_PD_FEECD_FG_2,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_1,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_1,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_1,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_2,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_2,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_2,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_3,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_3,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_3,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_4,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_4,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_4,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_5,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_5,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_5,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_6,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_6,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_6,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_7,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_7,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_7,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_8,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_8,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_8,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_9,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_9,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_9,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_10,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_10,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_10,''),
                   ISNULL(CLFILE.CL_PD_DLDISC_FG,''),
                   ISNULL(CLFILE.CL_PD_DLRVPRIN_FG_1,''),
                   ISNULL(CLFILE.CL_PD_DLRVPRIN_FG_2,''),
                   ISNULL(CLFILE.CL_PD_DLRVPRIN_FG_3,''),
                   ISNULL(CLFILE.CL_PD_DLRVCHGS_FG_1,''),
                   ISNULL(CLFILE.CL_PD_DLRVCHGS_FG_2,''),
                   ISNULL(CLFILE.CL_PD_DLRVCHGS_FG_3,''),
                   ISNULL(CLFILE.CL_PD_DLPARVHELD_FG,''),
                   ISNULL(CLFILE.CL_PD_DLPARVPAID_FG,''),
                   ISNULL(CLFILE.CL_PD_ONE_MTH_NO_DAY_EXT_SW,''),
                   ISNULL(CLFILE.CL_PD_ASSIGN_RN_ACCTNO,''),
                   CLFILE.CL_PD_APR_FINAMT,
                   ISNULL(CLFILE.CL_PD_DLCOMM_PP_CMSI_FG,''),
                   ISNULL(CLFILE.CL_PD_DLCOMM_O3_CMSI_FG,''),
                   ISNULL(CLFILE.CL_PD_FILLER_TO_MATCH_LN,''),
                   ISNULL(CLFILE.CL_PD2_ID,''),
                   CLFILE.CL_PD2_BRANCH,
                   ISNULL(CLFILE.CL_PD2_BWNAME,'')
            FROM DBO.CLFILE
            WHERE CLFILE.CL_GROUP_ID >= :QCL1-WBEG-GROUP
              AND CLFILE.CL_GROUP_ID <= :QCL1-WEND-GROUP
              AND CLFILE.CL_CAN_NO >= :QCL1-WBEG-CAN-NO
              AND CLFILE.CL_CAN_NO <= :QCL1-WEND-CAN-NO
              ORDER BY CLFILE.CL_GROUP_ID,
                       CLFILE.CL_CAN_NO
           END-EXEC.

           EXEC SQL
               OPEN CL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QCL1-WBEG-GROUP
                          QCL1-WEND-GROUP
                          QCL1-WBEG-CAN-NO
                          QCL1-WEND-CAN-NO.

           MOVE STAT---GOOD TO CL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CL1-FILE.
           PERFORM READ-IT.
           MOVE CL-SELECT-PATH TO E-FILE.
           MOVE CL1-KEY TO E-KEYX.

           MOVE CL-GROUP-ID    TO QCL-GROUP-ID. 
           MOVE CL-CAN-NO      TO QCL-CAN-NO.

           EXEC SQL
            SELECT ISNULL(CLFILE.CL_GROUP_ID,''),
                   ISNULL(CLFILE.CL_CAN_NO,''),
                   ISNULL(CAST(CLFILE.CL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CLFILE.CL_DESC,''),
                   ISNULL(CLFILE.CL_GIFTNET,''),
                   ISNULL(CLFILE.CL_PD_ID,''),
                   CLFILE.CL_PD_BRANCH,
                   CLFILE.CL_PD_SOC_SEC,
                   CLFILE.CL_PD_OWNBR,
                   CLFILE.CL_PD_ACCTNO,
                   CLFILE.CL_PD_CLASS,
                   CLFILE.CL_PD_SUBCLASS,
                   CLFILE.CL_PD_LAWCODE,
                   CLFILE.CL_PD_SSNO1,
                   ISNULL(CLFILE.CL_PD_MAKERCD1,''),
                   CLFILE.CL_PD_SSNO2,
                   ISNULL(CLFILE.CL_PD_MAKERCD2,''),
                   CLFILE.CL_PD_SSNO3,
                   ISNULL(CLFILE.CL_PD_MAKERCD3,''),
                   CLFILE.CL_PD_SSNO4,
                   ISNULL(CLFILE.CL_PD_MAKERCD4,''),
                   ISNULL(CLFILE.CL_PD_ORGST,''),
                   CLFILE.CL_PD_ORGBR,
                   ISNULL(CLFILE.CL_PD_OWNST,''),
                   CLFILE.CL_PD_SPRCLASS,
                   ISNULL(CLFILE.CL_PD_CONVERCD,''),
                   ISNULL(CLFILE.CL_PD_MIDSTREAM,''),
                   ISNULL(CLFILE.CL_PD_PURCD,''),
                   ISNULL(CLFILE.CL_PD_SRCD,''),
                   ISNULL(CLFILE.CL_PD_COLLCD,''),
                   ISNULL(CLFILE.CL_PD_COLLID,''),
                   ISNULL(CLFILE.CL_PD_POCD,''),
                   ISNULL(CLFILE.CL_PD_PLCD,''),
                   ISNULL(CLFILE.CL_PD_PLTYPE,''),
                   ISNULL(CLFILE.CL_PD_CONFIDENTCD,''),
                   ISNULL(CLFILE.CL_PD_ALLOTCD,''),
                   ISNULL(CLFILE.CL_PD_RECASTCD,''),
                   ISNULL(CLFILE.CL_PD_SKIP_TRW,''),
                   ISNULL(CLFILE.CL_PD_STATUSFG,''),
                   ISNULL(CLFILE.CL_PD_SMALLOAN,''),
                   ISNULL(CLFILE.CL_PD_SALEFIN,''),
                   ISNULL(CLFILE.CL_PD_SERVACCT,''),
                   ISNULL(CLFILE.CL_PD_LOANTYPE,''),
                   ISNULL(CLFILE.CL_PD_ADDON_FG,''),
                   ISNULL(CLFILE.CL_PD_PAY_SCHLD_FG,''),
                   CLFILE.CL_PD_LC_CODE,
                   ISNULL(CLFILE.CL_PD_CAN_LOAN_NO,''),
                   ISNULL(CLFILE.CL_PD_STATFLAGS,''),
                   ISNULL(CAST(CLFILE.CL_PD_ENTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PURDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_CASHDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_FILEDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_JDDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PLDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_REDUDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_POFFDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_REPODATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_SOLDDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_APRATE,
                   CLFILE.CL_PD_EFFRATE,
                   CLFILE.CL_PD_SMPRATE,
                   CLFILE.CL_PD_YLDRATE,
                   CLFILE.CL_PD_OVRDRATE,
                   CLFILE.CL_PD_ENDORATE,
                   CLFILE.CL_PD_JDRATE,
                   CLFILE.CL_PD_ACCRATE,
                   CLFILE.CL_PD_REDURATE,
                   CLFILE.CL_PD_CONRATE,
                   CLFILE.CL_PD_SERVPER,
                   CLFILE.CL_PD_SELLPR,
                   CLFILE.CL_PD_DOWNPY,
                   CLFILE.CL_PD_TRADEIN,
                   CLFILE.CL_PD_LNAMT,
                   CLFILE.CL_PD_FINCHG,
                   CLFILE.CL_PD_INTCHG,
                   CLFILE.CL_PD_SERCHG,
                   CLFILE.CL_PD_EXTCHG,
                   CLFILE.CL_PD_MAINTFEE,
                   CLFILE.CL_PD_POINTS,
                   CLFILE.CL_PD_PROCEEDS,
                   CLFILE.CL_PD_ORGTERM,
                   CLFILE.CL_PD_1STPYAMT,
                   CLFILE.CL_PD_NOREGPY,
                   CLFILE.CL_PD_REGPYAMT,
                   CLFILE.CL_PD_LASTPYAMT,
                   CLFILE.CL_PD_PROCASH,
                   CLFILE.CL_PD_TILLNO,
                   ISNULL(CLFILE.CL_PD_STATE_FLAG,''),
                   CLFILE.CL_PD_STATE_WORKER,
                   CLFILE.CL_PD_STATE_WORKER2,
                   ISNULL(CLFILE.CL_PD_FEECODE_1,''),
                   CLFILE.CL_PD_FEEAMT_1,
                   ISNULL(CLFILE.CL_PD_FEEOURS_1,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_1,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_2,''),
                   CLFILE.CL_PD_FEEAMT_2,
                   ISNULL(CLFILE.CL_PD_FEEOURS_2,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_2,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_3,''),
                   CLFILE.CL_PD_FEEAMT_3,
                   ISNULL(CLFILE.CL_PD_FEEOURS_3,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_3,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_4,''),
                   CLFILE.CL_PD_FEEAMT_4,
                   ISNULL(CLFILE.CL_PD_FEEOURS_4,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_4,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_5,''),
                   CLFILE.CL_PD_FEEAMT_5,
                   ISNULL(CLFILE.CL_PD_FEEOURS_5,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_5,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_6,''),
                   CLFILE.CL_PD_FEEAMT_6,
                   ISNULL(CLFILE.CL_PD_FEEOURS_6,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_6,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_7,''),
                   CLFILE.CL_PD_FEEAMT_7,
                   ISNULL(CLFILE.CL_PD_FEEOURS_7,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_7,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_8,''),
                   CLFILE.CL_PD_FEEAMT_8,
                   ISNULL(CLFILE.CL_PD_FEEOURS_8,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_8,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_9,''),
                   CLFILE.CL_PD_FEEAMT_9,
                   ISNULL(CLFILE.CL_PD_FEEOURS_9,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_9,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_10,''),
                   CLFILE.CL_PD_FEEAMT_10,
                   ISNULL(CLFILE.CL_PD_FEEOURS_10,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_10,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_11,''),
                   CLFILE.CL_PD_FEEAMT_11,
                   ISNULL(CLFILE.CL_PD_FEEOURS_11,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_11,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_12,''),
                   CLFILE.CL_PD_FEEAMT_12,
                   ISNULL(CLFILE.CL_PD_FEEOURS_12,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_12,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_13,''),
                   CLFILE.CL_PD_FEEAMT_13,
                   ISNULL(CLFILE.CL_PD_FEEOURS_13,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_13,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_14,''),
                   CLFILE.CL_PD_FEEAMT_14,
                   ISNULL(CLFILE.CL_PD_FEEOURS_14,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_14,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_15,''),
                   CLFILE.CL_PD_FEEAMT_15,
                   ISNULL(CLFILE.CL_PD_FEEOURS_15,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_15,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_16,''),
                   CLFILE.CL_PD_FEEAMT_16,
                   ISNULL(CLFILE.CL_PD_FEEOURS_16,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_16,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_17,''),
                   CLFILE.CL_PD_FEEAMT_17,
                   ISNULL(CLFILE.CL_PD_FEEOURS_17,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_17,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_18,''),
                   CLFILE.CL_PD_FEEAMT_18,
                   ISNULL(CLFILE.CL_PD_FEEOURS_18,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_18,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_19,''),
                   CLFILE.CL_PD_FEEAMT_19,
                   ISNULL(CLFILE.CL_PD_FEEOURS_19,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_19,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_20,''),
                   CLFILE.CL_PD_FEEAMT_20,
                   ISNULL(CLFILE.CL_PD_FEEOURS_20,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_20,''),
                   ISNULL(CLFILE.CL_PD_INSURED,''),
                   ISNULL(CLFILE.CL_PD_INSTYPES,''),
                   ISNULL(CLFILE.CL_PD_INSELIGIBLE,''),
                   CLFILE.CL_PD_INSCOMP_1,
                   ISNULL(CLFILE.CL_PD_INSOURS_1,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_1,
                   CLFILE.CL_PD_INSCOVR_1,
                   CLFILE.CL_PD_INSCOMM_1,
                   CLFILE.CL_PD_INSANTICERN_1,
                   CLFILE.CL_PD_INSANTICADJ_1,
                   CLFILE.CL_PD_INSUFERN_1,
                   CLFILE.CL_PD_INSACCERN_1,
                   CLFILE.CL_PD_INSEARN_ORDER_1,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_1,''),
                   CLFILE.CL_PD_INSCOMP_2,
                   ISNULL(CLFILE.CL_PD_INSOURS_2,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_2,
                   CLFILE.CL_PD_INSCOVR_2,
                   CLFILE.CL_PD_INSCOMM_2,
                   CLFILE.CL_PD_INSANTICERN_2,
                   CLFILE.CL_PD_INSANTICADJ_2,
                   CLFILE.CL_PD_INSUFERN_2,
                   CLFILE.CL_PD_INSACCERN_2,
                   CLFILE.CL_PD_INSEARN_ORDER_2,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_2,''),
                   CLFILE.CL_PD_INSCOMP_3,
                   ISNULL(CLFILE.CL_PD_INSOURS_3,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_3,
                   CLFILE.CL_PD_INSCOVR_3,
                   CLFILE.CL_PD_INSCOMM_3,
                   CLFILE.CL_PD_INSANTICERN_3,
                   CLFILE.CL_PD_INSANTICADJ_3,
                   CLFILE.CL_PD_INSUFERN_3,
                   CLFILE.CL_PD_INSACCERN_3,
                   CLFILE.CL_PD_INSEARN_ORDER_3,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_3,''),
                   CLFILE.CL_PD_INSCOMP_4,
                   ISNULL(CLFILE.CL_PD_INSOURS_4,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_4,
                   CLFILE.CL_PD_INSCOVR_4,
                   CLFILE.CL_PD_INSCOMM_4,
                   CLFILE.CL_PD_INSANTICERN_4,
                   CLFILE.CL_PD_INSANTICADJ_4,
                   CLFILE.CL_PD_INSUFERN_4,
                   CLFILE.CL_PD_INSACCERN_4,
                   CLFILE.CL_PD_INSEARN_ORDER_4,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_4,''),
                   CLFILE.CL_PD_INSCOMP_5,
                   ISNULL(CLFILE.CL_PD_INSOURS_5,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_5,
                   CLFILE.CL_PD_INSCOVR_5,
                   CLFILE.CL_PD_INSCOMM_5,
                   CLFILE.CL_PD_INSANTICERN_5,
                   CLFILE.CL_PD_INSANTICADJ_5,
                   CLFILE.CL_PD_INSUFERN_5,
                   CLFILE.CL_PD_INSACCERN_5,
                   CLFILE.CL_PD_INSEARN_ORDER_5,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_5,''),
                   CLFILE.CL_PD_INSCOMP_6,
                   ISNULL(CLFILE.CL_PD_INSOURS_6,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_6,
                   CLFILE.CL_PD_INSCOVR_6,
                   CLFILE.CL_PD_INSCOMM_6,
                   CLFILE.CL_PD_INSANTICERN_6,
                   CLFILE.CL_PD_INSANTICADJ_6,
                   CLFILE.CL_PD_INSUFERN_6,
                   CLFILE.CL_PD_INSACCERN_6,
                   CLFILE.CL_PD_INSEARN_ORDER_6,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_6,''),
                   CLFILE.CL_PD_INSCOMP_7,
                   ISNULL(CLFILE.CL_PD_INSOURS_7,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_7,
                   CLFILE.CL_PD_INSCOVR_7,
                   CLFILE.CL_PD_INSCOMM_7,
                   CLFILE.CL_PD_INSANTICERN_7,
                   CLFILE.CL_PD_INSANTICADJ_7,
                   CLFILE.CL_PD_INSUFERN_7,
                   CLFILE.CL_PD_INSACCERN_7,
                   CLFILE.CL_PD_INSEARN_ORDER_7,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_7,''),
                   CLFILE.CL_PD_INSCOMP_8,
                   ISNULL(CLFILE.CL_PD_INSOURS_8,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_8,
                   CLFILE.CL_PD_INSCOVR_8,
                   CLFILE.CL_PD_INSCOMM_8,
                   CLFILE.CL_PD_INSANTICERN_8,
                   CLFILE.CL_PD_INSANTICADJ_8,
                   CLFILE.CL_PD_INSUFERN_8,
                   CLFILE.CL_PD_INSACCERN_8,
                   CLFILE.CL_PD_INSEARN_ORDER_8,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_8,''),
                   CLFILE.CL_PD_INSCOMP_9,
                   ISNULL(CLFILE.CL_PD_INSOURS_9,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_9,
                   CLFILE.CL_PD_INSCOVR_9,
                   CLFILE.CL_PD_INSCOMM_9,
                   CLFILE.CL_PD_INSANTICERN_9,
                   CLFILE.CL_PD_INSANTICADJ_9,
                   CLFILE.CL_PD_INSUFERN_9,
                   CLFILE.CL_PD_INSACCERN_9,
                   CLFILE.CL_PD_INSEARN_ORDER_9,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_9,''),
                   CLFILE.CL_PD_INSCOMP_10,
                   ISNULL(CLFILE.CL_PD_INSOURS_10,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_10,
                   CLFILE.CL_PD_INSCOVR_10,
                   CLFILE.CL_PD_INSCOMM_10,
                   CLFILE.CL_PD_INSANTICERN_10,
                   CLFILE.CL_PD_INSANTICADJ_10,
                   CLFILE.CL_PD_INSUFERN_10,
                   CLFILE.CL_PD_INSACCERN_10,
                   CLFILE.CL_PD_INSEARN_ORDER_10,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_10,''),
                   CLFILE.CL_PD_PY_YYYYMM_RECDEL,
                   CLFILE.CL_PD_MTD_PAYS_RECDEL,
                   ISNULL(CLFILE.CL_PD_GARNISH_EXTR,''),
                   ISNULL(CLFILE.CL_PD_SHIP_SAME_FG,''),
                   CLFILE.CL_PD_AD_D_INS_COVR,
                   CLFILE.CL_PD_TOT_PARTIAL_CHGOFF,
                   ISNULL(CLFILE.CL_PD_LPO,''),
                   ISNULL(CLFILE.CL_PD_REP_CD,''),
                   CLFILE.CL_PD_EMP_ID,
                   ISNULL(CLFILE.CL_PD_PROCESS_CD,''),
                   CLFILE.CL_PD_WCBC_CREDITLIM,
                   ISNULL(CLFILE.CL_PD_FOLLOWUP_CD,''),
                   ISNULL(CLFILE.CL_PD_MIP_FG,''),
                   CLFILE.CL_PD_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(CLFILE.CL_PD_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_MIP_TOT_CLPAID,
                   ISNULL(CAST(CLFILE.CL_PD_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_MIP_TOT_AHPAID,
                   CLFILE.CL_PD_IL_GROSS_INCOME,
                   CLFILE.CL_PD_IL_GROSS_INCOME2,
                   ISNULL(CLFILE.CL_PD_RESCISSION,''),
                   ISNULL(CLFILE.CL_PD_INTERIM_TRW,''),
                   CLFILE.CL_PD_DLNO,
                   CLFILE.CL_PD_DLPOOL,
                   ISNULL(CLFILE.CL_PD_DLRECOURSE,''),
                   CLFILE.CL_PD_DLPARTRCAMT,
                   CLFILE.CL_PD_ORIGPURAMT,
                   CLFILE.CL_PD_PURBAL,
                   CLFILE.CL_PD_DLRVPRIN_1,
                   CLFILE.CL_PD_DLRVPRIN_2,
                   CLFILE.CL_PD_DLRVPRIN_3,
                   CLFILE.CL_PD_DLRVCHGS_1,
                   CLFILE.CL_PD_DLRVCHGS_2,
                   CLFILE.CL_PD_DLRVCHGS_3,
                   CLFILE.CL_PD_DLPARVHELD,
                   CLFILE.CL_PD_DLPARVPAID,
                   CLFILE.CL_PD_DLDISC,
                   ISNULL(CAST(CLFILE.CL_PD_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_CURBAL,
                   CLFILE.CL_PD_INTBAL,
                   CLFILE.CL_PD_LCBAL,
                   CLFILE.CL_PD_OTHBAL,
                   CLFILE.CL_PD_OT2BAL,
                   CLFILE.CL_PD_LCPAID,
                   CLFILE.CL_PD_LCPARTIALS,
                   CLFILE.CL_PD_STMT_COUNTER,
                   CLFILE.CL_PD_PRIOR_STMT_COUNTER,
                   ISNULL(CLFILE.CL_PD_FROZEN_REASON,''),
                   ISNULL(CLFILE.CL_PD_COUPON_STATEMENT,''),
                   CLFILE.CL_PD_MONTHEND_BAL,
                   CLFILE.CL_PD_ANTICTERM,
                   CLFILE.CL_PD_ANTICERN_1,
                   CLFILE.CL_PD_ANTICADJ_1,
                   CLFILE.CL_PD_UFERN_1,
                   CLFILE.CL_PD_ACCERN_1,
                   CLFILE.CL_PD_EARN_ORDER_1,
                   CLFILE.CL_PD_ANTICERN_2,
                   CLFILE.CL_PD_ANTICADJ_2,
                   CLFILE.CL_PD_UFERN_2,
                   CLFILE.CL_PD_ACCERN_2,
                   CLFILE.CL_PD_EARN_ORDER_2,
                   CLFILE.CL_PD_ANTICERN_3,
                   CLFILE.CL_PD_ANTICADJ_3,
                   CLFILE.CL_PD_UFERN_3,
                   CLFILE.CL_PD_ACCERN_3,
                   CLFILE.CL_PD_EARN_ORDER_3,
                   CLFILE.CL_PD_ANTICERN_4,
                   CLFILE.CL_PD_ANTICADJ_4,
                   CLFILE.CL_PD_UFERN_4,
                   CLFILE.CL_PD_ACCERN_4,
                   CLFILE.CL_PD_EARN_ORDER_4,
                   CLFILE.CL_PD_ANTICERN_5,
                   CLFILE.CL_PD_ANTICADJ_5,
                   CLFILE.CL_PD_UFERN_5,
                   CLFILE.CL_PD_ACCERN_5,
                   CLFILE.CL_PD_EARN_ORDER_5,
                   CLFILE.CL_PD_ANTICERN_6,
                   CLFILE.CL_PD_ANTICADJ_6,
                   CLFILE.CL_PD_UFERN_6,
                   CLFILE.CL_PD_ACCERN_6,
                   CLFILE.CL_PD_EARN_ORDER_6,
                   CLFILE.CL_PD_ANTICERN_7,
                   CLFILE.CL_PD_ANTICADJ_7,
                   CLFILE.CL_PD_UFERN_7,
                   CLFILE.CL_PD_ACCERN_7,
                   CLFILE.CL_PD_EARN_ORDER_7,
                   CLFILE.CL_PD_ANTICERN_8,
                   CLFILE.CL_PD_ANTICADJ_8,
                   CLFILE.CL_PD_UFERN_8,
                   CLFILE.CL_PD_ACCERN_8,
                   CLFILE.CL_PD_EARN_ORDER_8,
                   CLFILE.CL_PD_ANTICERN_9,
                   CLFILE.CL_PD_ANTICADJ_9,
                   CLFILE.CL_PD_UFERN_9,
                   CLFILE.CL_PD_ACCERN_9,
                   CLFILE.CL_PD_EARN_ORDER_9,
                   CLFILE.CL_PD_ANTICERN_10,
                   CLFILE.CL_PD_ANTICADJ_10,
                   CLFILE.CL_PD_UFERN_10,
                   CLFILE.CL_PD_ACCERN_10,
                   CLFILE.CL_PD_EARN_ORDER_10,
                   CLFILE.CL_PD_LMO_PRIOR_INTEREST,
                   CLFILE.CL_PD_PRIOR_UNEARNED,
                   ISNULL(CLFILE.CL_PD_ERNDELQ_STOP,''),
                   CLFILE.CL_PD_NONACRERN,
                   CLFILE.CL_PD_PRIOR_NONACRERN,
                   ISNULL(CAST(CLFILE.CL_PD_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_ERN_IB_PRINBAL,
                   CLFILE.CL_PD_ERN_IB_INTOWE,
                   CLFILE.CL_PD_TOTERN,
                   CLFILE.CL_PD_TOTOTH,
                   CLFILE.CL_PD_TOTINT,
                   CLFILE.CL_PD_TOTLCHG,
                   CLFILE.CL_PD_TOTDEF,
                   CLFILE.CL_PD_TOTSERACCT,
                   CLFILE.CL_PD_LCWAIVERS,
                   CLFILE.CL_PD_INTWAIVERS,
                   CLFILE.CL_PD_TOTPAYMNTD,
                   CLFILE.CL_PD_TOTEXCPAYMNTD,
                   CLFILE.CL_PD_TOTNODEF,
                   CLFILE.CL_PD_YTDNODEF,
                   CLFILE.CL_PD_TOTNODEFACT,
                   CLFILE.CL_PD_YTDNODEFACT,
                   CLFILE.CL_PD_TOTNOALDEL,
                   CLFILE.CL_PD_YTDNOALDEL,
                   CLFILE.CL_PD_YTDINTPD,
                   CLFILE.CL_PD_LYRINTPD,
                   CLFILE.CL_PD_TOTINTPD,
                   CLFILE.CL_PD_RECASTINT,
                   CLFILE.CL_PD_RECASTPRIN,
                   ISNULL(CLFILE.CL_PD_CL_REBATE,''),
                   ISNULL(CLFILE.CL_PD_AH_REBATE,''),
                   ISNULL(CLFILE.CL_PD_PP_REBATE,''),
                   ISNULL(CLFILE.CL_PD_INT_REBATE,''),
                   ISNULL(CLFILE.CL_PD_SC_REBATE,''),
                   ISNULL(CLFILE.CL_PD_MF_REBATE,''),
                   ISNULL(CLFILE.CL_PD_DF_REBATE,''),
                   ISNULL(CLFILE.CL_PD_EX_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O1_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O2_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O3_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O4_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O5_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O6_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O7_REBATE,''),
                   ISNULL(CLFILE.CL_PD_UN_O7_REB,''),
                   ISNULL(CLFILE.CL_PD_UN_O8_REB,''),
                   ISNULL(CLFILE.CL_PD_UN_O9_REB,''),
                   ISNULL(CLFILE.CL_PD_UN_O10_REB,''),
                   ISNULL(CLFILE.CL_PD_PLREASON,''),
                   ISNULL(CLFILE.CL_PD_COLLECTOR,''),
                   ISNULL(CLFILE.CL_PD_ACTIONCD,''),
                   ISNULL(CLFILE.CL_PD_ACTIONCD2,''),
                   ISNULL(CLFILE.CL_PD_ATCODE,''),
                   ISNULL(CLFILE.CL_PD_RECODE,''),
                   ISNULL(CLFILE.CL_PD_ESCROW_FG,''),
                   CLFILE.CL_PD_PROMDAYS_PASTDUE,
                   ISNULL(CLFILE.CL_PD_PROMID,''),
                   ISNULL(CAST(CLFILE.CL_PD_PROMDATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_PROMAMT,
                   ISNULL(CAST(CLFILE.CL_PD_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_AMT_PAID_LAST,
                   CLFILE.CL_PD_LASTCFORMNO,
                   ISNULL(CAST(CLFILE.CL_PD_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_DELCNT,
                   CLFILE.CL_PD_DELCNT2,
                   CLFILE.CL_PD_CREDITLIM,
                   CLFILE.CL_PD_CRSCORE,
                   CLFILE.CL_PD_LASTSFORMNO,
                   ISNULL(CAST(CLFILE.CL_PD_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_SOLCNT,
                   ISNULL(CLFILE.CL_PD_MESSAGE,''),
                   CLFILE.CL_PD_OLDACCTNO,
                   CLFILE.CL_PD_PRLNNO,
                   CLFILE.CL_PD_PRBAL,
                   CLFILE.CL_PD_PRINTDUE,
                   CLFILE.CL_PD_PRFINCHGRB,
                   CLFILE.CL_PD_PRINSRB,
                   ISNULL(CLFILE.CL_PD_PRPOCD,''),
                   ISNULL(CAST(CLFILE.CL_PD_PRPODATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_PRDELINQAMT,
                   CLFILE.CL_PD_PRLNAMT,
                   CLFILE.CL_PD_PRPDUE_DAYS,
                   CLFILE.CL_PD_PRDLDISC_UNEARN,
                   CLFILE.CL_PD_PRCONPROFILE,
                   CLFILE.CL_PD_SUM_ORIGPURAMT,
                   CLFILE.CL_PD_SUM_LNAMT,
                   CLFILE.CL_PD_SUM_FINCHG,
                   CLFILE.CL_PD_SUM_INTCHG,
                   CLFILE.CL_PD_SUM_ORGTERM,
                   CLFILE.CL_PD_SUM_REGPYAMT,
                   CLFILE.CL_PD_SUM_EXTCHG,
                   CLFILE.CL_PD_SUM_SERCHG,
                   CLFILE.CL_PD_SUM_MAINTFEE,
                   ISNULL(CLFILE.CL_PD_UNITPER_CD,''),
                   CLFILE.CL_PD_UNITPER_FREQ,
                   CLFILE.CL_PD_DAYS_IN_UNITPER,
                   CLFILE.CL_PD_UNITPER_PER_YEAR,
                   ISNULL(CLFILE.CL_PD_REPOCD,''),
                   ISNULL(CLFILE.CL_PD_BR_TFR_FG,''),
                   CLFILE.CL_PD_O3INS_DLPROC,
                   CLFILE.CL_PD_INSPREM_DLADJ,
                   CLFILE.CL_PD_PLAMT,
                   CLFILE.CL_PD_MTD_PRIN,
                   CLFILE.CL_PD_MTD_LC,
                   CLFILE.CL_PD_MTD_INT,
                   CLFILE.CL_PD_MTD_OT,
                   CLFILE.CL_PD_MTD_EX,
                   CLFILE.CL_PD_BNKRPT_CHAPTER,
                   ISNULL(CLFILE.CL_PD_BMO_CON_STATUS,''),
                   ISNULL(CLFILE.CL_PD_BMO_REC_STATUS,''),
                   CLFILE.CL_PD_APP_NUMBER,
                   ISNULL(CLFILE.CL_PD_GIFTNET,''),
                   CLFILE.CL_PD_GIFTNET_OUR_PO,
                   ISNULL(CLFILE.CL_PD_PROC_DISBURSED,''),
                   CLFILE.CL_PD_NOODDPY,
                   CLFILE.CL_PD_CONPROFILE_1,
                   CLFILE.CL_PD_CONPROFILE_2,
                   CLFILE.CL_PD_CONPROFILE_3,
                   CLFILE.CL_PD_CONPROFILE_4,
                   CLFILE.CL_PD_CONPROFILE_5,
                   CLFILE.CL_PD_RECPROFILE_1,
                   CLFILE.CL_PD_RECPROFILE_2,
                   CLFILE.CL_PD_RECPROFILE_3,
                   CLFILE.CL_PD_RECPROFILE_4,
                   CLFILE.CL_PD_RECPROFILE_5,
                   ISNULL(CLFILE.CL_PD_TRW_STATUS,''),
                   ISNULL(CLFILE.CL_PD_ORIGIN,''),
                   ISNULL(CLFILE.CL_PD_PO_TRCD,''),
                   CLFILE.CL_PD_NEXIS_CRSCORE,
                   ISNULL(CLFILE.CL_PD_TRW_B_CONSUMER_IND,''),
                   ISNULL(CLFILE.CL_PD_PARVHELD_PAID_FG,''),
                   ISNULL(CLFILE.CL_PD_ACCT_FROZEN,''),
                   ISNULL(CLFILE.CL_PD_TRW_SPECIAL_CODE,''),
                   ISNULL(CLFILE.CL_PD_CREATION_STATUS,''),
                   CLFILE.CL_PD_TOTNOLCHG,
                   ISNULL(CLFILE.CL_PD_LBOX,''),
                   CLFILE.CL_PD_EXTRA_PRIN_LFP,
                   CLFILE.CL_PD_REMAIN_INT_SINCE_LFP,
                   CLFILE.CL_PD_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(CLFILE.CL_PD_INTERIM_COUPON,''),
                   CLFILE.CL_PD_DLCOMM_O3_CMSI,
                   CLFILE.CL_PD_DLCOMM_PP_CMSI,
                   ISNULL(CLFILE.CL_PD_TRW_J_CONSUMER_IND,''),
                   ISNULL(CLFILE.CL_PD_ACH,''),
                   ISNULL(CLFILE.CL_PD_1099_C_FG,''),
                   ISNULL(CLFILE.CL_PD_TRW_COMPLIANCE_CODE,''),
                   ISNULL(CLFILE.CL_PD_UNWIND_REASON_CODE,''),
                   ISNULL(CLFILE.CL_PD_PBI_PBD_FG,''),
                   ISNULL(CLFILE.CL_PD_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(CLFILE.CL_PD_APPROVED_BY_USER,''),
                   ISNULL(CLFILE.CL_PD_APPROVED_BY_CODE,''),
                   ISNULL(CAST(CLFILE.CL_PD_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_APPROVED_BY_TIME,
                   ISNULL(CAST(CLFILE.CL_PD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_LTOUCH_TIME,
                   ISNULL(CLFILE.CL_PD_HOLD_COLLATERAL,''),
                   ISNULL(CLFILE.CL_PD_DLPAID,''),
                   CLFILE.CL_PD_CKNO,
                   ISNULL(CLFILE.CL_PD_FREE1,''),
                   ISNULL(CLFILE.CL_PD_FREE2,''),
                   ISNULL(CLFILE.CL_PD_FREE_SPACE_FILLER,''),
                   CLFILE.CL_PD_POSTING_BR,
                   ISNULL(CLFILE.CL_PD_OTHBR_FG,''),
                   CLFILE.CL_PD_APPROVAL_LIMIT,
                   ISNULL(CLFILE.CL_PD_APPROVAL_USERID,''),
                   CLFILE.CL_PD_MIN_CRED_SCORE,
                   ISNULL(CLFILE.CL_PD_CRED_SCORE_USERID,''),
                   CLFILE.CL_PD_FINAMT,
                   ISNULL(CLFILE.CL_PD_PROCEEDS_FG,''),
                   ISNULL(CLFILE.CL_PD_POINTS_FG,''),
                   ISNULL(CLFILE.CL_PD_OVRDRATE_FG,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_1,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_2,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_3,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_4,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_5,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_6,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_7,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_8,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_9,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_10,''),
                   ISNULL(CLFILE.CL_PD_SERCHG_FG,''),
                   ISNULL(CLFILE.CL_PD_MAINTFEE_FG,''),
                   ISNULL(CLFILE.CL_PD_EXTCHG_FG,''),
                   ISNULL(CLFILE.CL_PD_REGPYAMT_FG,''),
                   ISNULL(CLFILE.CL_PD_FEECD_1,''),
                   ISNULL(CLFILE.CL_PD_FEECD_2,''),
                   ISNULL(CLFILE.CL_PD_FEECD_3,''),
                   ISNULL(CLFILE.CL_PD_FEECD_4,''),
                   ISNULL(CLFILE.CL_PD_FEECD_5,''),
                   ISNULL(CLFILE.CL_PD_FEECD_6,''),
                   ISNULL(CLFILE.CL_PD_FEECD_7,''),
                   ISNULL(CLFILE.CL_PD_FEECD_8,''),
                   ISNULL(CLFILE.CL_PD_FEECD_9,''),
                   ISNULL(CLFILE.CL_PD_FEECD_10,''),
                   ISNULL(CLFILE.CL_PD_FEECD_11,''),
                   ISNULL(CLFILE.CL_PD_FEECD_12,''),
                   ISNULL(CLFILE.CL_PD_FEECD_13,''),
                   ISNULL(CLFILE.CL_PD_FEECD_14,''),
                   ISNULL(CLFILE.CL_PD_FEECD_15,''),
                   ISNULL(CLFILE.CL_PD_FEECD_16,''),
                   ISNULL(CLFILE.CL_PD_FEECD_17,''),
                   ISNULL(CLFILE.CL_PD_FEECD_18,''),
                   ISNULL(CLFILE.CL_PD_FEECD_19,''),
                   ISNULL(CLFILE.CL_PD_FEECD_20,''),
                   CLFILE.CL_PD_PRPAID_AHEAD,
                   CLFILE.CL_PD_RESTRICT_ADJ,
                   CLFILE.CL_PD_DUAL_LOAN_ADJ,
                   CLFILE.CL_PD_CAN_LOAN_MAX,
                   CLFILE.CL_PD_PERDOWN,
                   ISNULL(CLFILE.CL_PD_SELLPR_FG,''),
                   ISNULL(CLFILE.CL_PD_DOWNPY_FG,''),
                   ISNULL(CLFILE.CL_PD_PERDOWN_FG,''),
                   ISNULL(CLFILE.CL_PD_SELL_DOWN_FG,''),
                   ISNULL(CLFILE.CL_PD_BALLOON_FG,''),
                   ISNULL(CLFILE.CL_PD_BALLOON_LAST_FG,''),
                   ISNULL(CLFILE.CL_PD_FEECD_FG_1,''),
                   ISNULL(CLFILE.CL_PD_FEECD_FG_2,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_1,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_1,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_1,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_2,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_2,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_2,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_3,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_3,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_3,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_4,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_4,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_4,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_5,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_5,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_5,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_6,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_6,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_6,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_7,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_7,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_7,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_8,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_8,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_8,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_9,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_9,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_9,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_10,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_10,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_10,''),
                   ISNULL(CLFILE.CL_PD_DLDISC_FG,''),
                   ISNULL(CLFILE.CL_PD_DLRVPRIN_FG_1,''),
                   ISNULL(CLFILE.CL_PD_DLRVPRIN_FG_2,''),
                   ISNULL(CLFILE.CL_PD_DLRVPRIN_FG_3,''),
                   ISNULL(CLFILE.CL_PD_DLRVCHGS_FG_1,''),
                   ISNULL(CLFILE.CL_PD_DLRVCHGS_FG_2,''),
                   ISNULL(CLFILE.CL_PD_DLRVCHGS_FG_3,''),
                   ISNULL(CLFILE.CL_PD_DLPARVHELD_FG,''),
                   ISNULL(CLFILE.CL_PD_DLPARVPAID_FG,''),
                   ISNULL(CLFILE.CL_PD_ONE_MTH_NO_DAY_EXT_SW,''),
                   ISNULL(CLFILE.CL_PD_ASSIGN_RN_ACCTNO,''),
                   CLFILE.CL_PD_APR_FINAMT,
                   ISNULL(CLFILE.CL_PD_DLCOMM_PP_CMSI_FG,''),
                   ISNULL(CLFILE.CL_PD_DLCOMM_O3_CMSI_FG,''),
                   ISNULL(CLFILE.CL_PD_FILLER_TO_MATCH_LN,''),
                   ISNULL(CLFILE.CL_PD2_ID,''),
                   CLFILE.CL_PD2_BRANCH,
                   ISNULL(CLFILE.CL_PD2_BWNAME,'')
            INTO :QCL-REC
            FROM DBO.CLFILE
            WHERE CLFILE.CL_GROUP_ID = :QCL-GROUP-ID
              AND CLFILE.CL_CAN_NO = :QCL-CAN-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CL-FIELDS.

      *-----------------------------------------------------------------
       READ-CL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CL-SELECT-PATH TO E-FILE.
           MOVE CL1-KEY TO E-KEYX.
           INITIALIZE QCL-REC.

           IF ( CL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CL1_CURSOR INTO :QCL-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CL-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CL1-FILE.
           IF ( CL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       LOAD-CL2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CL-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CL-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO CL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CL-ALL-MACHINE
              MOVE CL-ALL-PATH             TO CL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CL-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO CL-ALL-BASE
              MOVE FILPATH-MACHINE         TO CL-ALL-MACHINE
              MOVE CL-ALL-PATH             TO CL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-CLFILE NOT = "S" )
              MOVE CL-PATH-BEG             TO CL-SELECT-PATH-BEG
              MOVE CL-PATH-FILE            TO CL-SELECT-PATH-FILE
           ELSE
              MOVE CL-PATH-SQL             TO CL-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-CL2-FILE.
           PERFORM LOAD-CL2-FILE.
           PERFORM OPEN-IT.
           MOVE CL-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CL2-FILE.
           PERFORM START-IT.
           MOVE CL-SELECT-PATH TO E-FILE.
           MOVE SPACES TO E-KEYX.
           STRING CL-GROUP-ID,CL-CAN-NO DELIMITED BY "   "
                          INTO E-KEYX.

           IF ( CL2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CL2-FILE.

           IF ( QCL2-WEND-GROUP = SPACES )
              MOVE CL-GROUP-ID          TO QCL2-WBEG-GROUP
              MOVE ALL "Z"              TO QCL2-WEND-GROUP
              MOVE CL-DESC              TO QCL2-WBEG-DESC
              MOVE ALL "Z"              TO QCL2-WEND-DESC.

           EXEC SQL
            DECLARE CL2_CURSOR CURSOR FOR
            SELECT ISNULL(CLFILE.CL_GROUP_ID,''),
                   ISNULL(CLFILE.CL_CAN_NO,''),
                   ISNULL(CAST(CLFILE.CL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CLFILE.CL_DESC,''),
                   ISNULL(CLFILE.CL_GIFTNET,''),
                   ISNULL(CLFILE.CL_PD_ID,''),
                   CLFILE.CL_PD_BRANCH,
                   CLFILE.CL_PD_SOC_SEC,
                   CLFILE.CL_PD_OWNBR,
                   CLFILE.CL_PD_ACCTNO,
                   CLFILE.CL_PD_CLASS,
                   CLFILE.CL_PD_SUBCLASS,
                   CLFILE.CL_PD_LAWCODE,
                   CLFILE.CL_PD_SSNO1,
                   ISNULL(CLFILE.CL_PD_MAKERCD1,''),
                   CLFILE.CL_PD_SSNO2,
                   ISNULL(CLFILE.CL_PD_MAKERCD2,''),
                   CLFILE.CL_PD_SSNO3,
                   ISNULL(CLFILE.CL_PD_MAKERCD3,''),
                   CLFILE.CL_PD_SSNO4,
                   ISNULL(CLFILE.CL_PD_MAKERCD4,''),
                   ISNULL(CLFILE.CL_PD_ORGST,''),
                   CLFILE.CL_PD_ORGBR,
                   ISNULL(CLFILE.CL_PD_OWNST,''),
                   CLFILE.CL_PD_SPRCLASS,
                   ISNULL(CLFILE.CL_PD_CONVERCD,''),
                   ISNULL(CLFILE.CL_PD_MIDSTREAM,''),
                   ISNULL(CLFILE.CL_PD_PURCD,''),
                   ISNULL(CLFILE.CL_PD_SRCD,''),
                   ISNULL(CLFILE.CL_PD_COLLCD,''),
                   ISNULL(CLFILE.CL_PD_COLLID,''),
                   ISNULL(CLFILE.CL_PD_POCD,''),
                   ISNULL(CLFILE.CL_PD_PLCD,''),
                   ISNULL(CLFILE.CL_PD_PLTYPE,''),
                   ISNULL(CLFILE.CL_PD_CONFIDENTCD,''),
                   ISNULL(CLFILE.CL_PD_ALLOTCD,''),
                   ISNULL(CLFILE.CL_PD_RECASTCD,''),
                   ISNULL(CLFILE.CL_PD_SKIP_TRW,''),
                   ISNULL(CLFILE.CL_PD_STATUSFG,''),
                   ISNULL(CLFILE.CL_PD_SMALLOAN,''),
                   ISNULL(CLFILE.CL_PD_SALEFIN,''),
                   ISNULL(CLFILE.CL_PD_SERVACCT,''),
                   ISNULL(CLFILE.CL_PD_LOANTYPE,''),
                   ISNULL(CLFILE.CL_PD_ADDON_FG,''),
                   ISNULL(CLFILE.CL_PD_PAY_SCHLD_FG,''),
                   CLFILE.CL_PD_LC_CODE,
                   ISNULL(CLFILE.CL_PD_CAN_LOAN_NO,''),
                   ISNULL(CLFILE.CL_PD_STATFLAGS,''),
                   ISNULL(CAST(CLFILE.CL_PD_ENTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PURDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_CASHDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_FILEDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_JDDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PLDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_REDUDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_POFFDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_REPODATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_SOLDDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_APRATE,
                   CLFILE.CL_PD_EFFRATE,
                   CLFILE.CL_PD_SMPRATE,
                   CLFILE.CL_PD_YLDRATE,
                   CLFILE.CL_PD_OVRDRATE,
                   CLFILE.CL_PD_ENDORATE,
                   CLFILE.CL_PD_JDRATE,
                   CLFILE.CL_PD_ACCRATE,
                   CLFILE.CL_PD_REDURATE,
                   CLFILE.CL_PD_CONRATE,
                   CLFILE.CL_PD_SERVPER,
                   CLFILE.CL_PD_SELLPR,
                   CLFILE.CL_PD_DOWNPY,
                   CLFILE.CL_PD_TRADEIN,
                   CLFILE.CL_PD_LNAMT,
                   CLFILE.CL_PD_FINCHG,
                   CLFILE.CL_PD_INTCHG,
                   CLFILE.CL_PD_SERCHG,
                   CLFILE.CL_PD_EXTCHG,
                   CLFILE.CL_PD_MAINTFEE,
                   CLFILE.CL_PD_POINTS,
                   CLFILE.CL_PD_PROCEEDS,
                   CLFILE.CL_PD_ORGTERM,
                   CLFILE.CL_PD_1STPYAMT,
                   CLFILE.CL_PD_NOREGPY,
                   CLFILE.CL_PD_REGPYAMT,
                   CLFILE.CL_PD_LASTPYAMT,
                   CLFILE.CL_PD_PROCASH,
                   CLFILE.CL_PD_TILLNO,
                   ISNULL(CLFILE.CL_PD_STATE_FLAG,''),
                   CLFILE.CL_PD_STATE_WORKER,
                   CLFILE.CL_PD_STATE_WORKER2,
                   ISNULL(CLFILE.CL_PD_FEECODE_1,''),
                   CLFILE.CL_PD_FEEAMT_1,
                   ISNULL(CLFILE.CL_PD_FEEOURS_1,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_1,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_2,''),
                   CLFILE.CL_PD_FEEAMT_2,
                   ISNULL(CLFILE.CL_PD_FEEOURS_2,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_2,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_3,''),
                   CLFILE.CL_PD_FEEAMT_3,
                   ISNULL(CLFILE.CL_PD_FEEOURS_3,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_3,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_4,''),
                   CLFILE.CL_PD_FEEAMT_4,
                   ISNULL(CLFILE.CL_PD_FEEOURS_4,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_4,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_5,''),
                   CLFILE.CL_PD_FEEAMT_5,
                   ISNULL(CLFILE.CL_PD_FEEOURS_5,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_5,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_6,''),
                   CLFILE.CL_PD_FEEAMT_6,
                   ISNULL(CLFILE.CL_PD_FEEOURS_6,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_6,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_7,''),
                   CLFILE.CL_PD_FEEAMT_7,
                   ISNULL(CLFILE.CL_PD_FEEOURS_7,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_7,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_8,''),
                   CLFILE.CL_PD_FEEAMT_8,
                   ISNULL(CLFILE.CL_PD_FEEOURS_8,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_8,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_9,''),
                   CLFILE.CL_PD_FEEAMT_9,
                   ISNULL(CLFILE.CL_PD_FEEOURS_9,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_9,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_10,''),
                   CLFILE.CL_PD_FEEAMT_10,
                   ISNULL(CLFILE.CL_PD_FEEOURS_10,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_10,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_11,''),
                   CLFILE.CL_PD_FEEAMT_11,
                   ISNULL(CLFILE.CL_PD_FEEOURS_11,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_11,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_12,''),
                   CLFILE.CL_PD_FEEAMT_12,
                   ISNULL(CLFILE.CL_PD_FEEOURS_12,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_12,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_13,''),
                   CLFILE.CL_PD_FEEAMT_13,
                   ISNULL(CLFILE.CL_PD_FEEOURS_13,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_13,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_14,''),
                   CLFILE.CL_PD_FEEAMT_14,
                   ISNULL(CLFILE.CL_PD_FEEOURS_14,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_14,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_15,''),
                   CLFILE.CL_PD_FEEAMT_15,
                   ISNULL(CLFILE.CL_PD_FEEOURS_15,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_15,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_16,''),
                   CLFILE.CL_PD_FEEAMT_16,
                   ISNULL(CLFILE.CL_PD_FEEOURS_16,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_16,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_17,''),
                   CLFILE.CL_PD_FEEAMT_17,
                   ISNULL(CLFILE.CL_PD_FEEOURS_17,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_17,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_18,''),
                   CLFILE.CL_PD_FEEAMT_18,
                   ISNULL(CLFILE.CL_PD_FEEOURS_18,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_18,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_19,''),
                   CLFILE.CL_PD_FEEAMT_19,
                   ISNULL(CLFILE.CL_PD_FEEOURS_19,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_19,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_20,''),
                   CLFILE.CL_PD_FEEAMT_20,
                   ISNULL(CLFILE.CL_PD_FEEOURS_20,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_20,''),
                   ISNULL(CLFILE.CL_PD_INSURED,''),
                   ISNULL(CLFILE.CL_PD_INSTYPES,''),
                   ISNULL(CLFILE.CL_PD_INSELIGIBLE,''),
                   CLFILE.CL_PD_INSCOMP_1,
                   ISNULL(CLFILE.CL_PD_INSOURS_1,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_1,
                   CLFILE.CL_PD_INSCOVR_1,
                   CLFILE.CL_PD_INSCOMM_1,
                   CLFILE.CL_PD_INSANTICERN_1,
                   CLFILE.CL_PD_INSANTICADJ_1,
                   CLFILE.CL_PD_INSUFERN_1,
                   CLFILE.CL_PD_INSACCERN_1,
                   CLFILE.CL_PD_INSEARN_ORDER_1,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_1,''),
                   CLFILE.CL_PD_INSCOMP_2,
                   ISNULL(CLFILE.CL_PD_INSOURS_2,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_2,
                   CLFILE.CL_PD_INSCOVR_2,
                   CLFILE.CL_PD_INSCOMM_2,
                   CLFILE.CL_PD_INSANTICERN_2,
                   CLFILE.CL_PD_INSANTICADJ_2,
                   CLFILE.CL_PD_INSUFERN_2,
                   CLFILE.CL_PD_INSACCERN_2,
                   CLFILE.CL_PD_INSEARN_ORDER_2,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_2,''),
                   CLFILE.CL_PD_INSCOMP_3,
                   ISNULL(CLFILE.CL_PD_INSOURS_3,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_3,
                   CLFILE.CL_PD_INSCOVR_3,
                   CLFILE.CL_PD_INSCOMM_3,
                   CLFILE.CL_PD_INSANTICERN_3,
                   CLFILE.CL_PD_INSANTICADJ_3,
                   CLFILE.CL_PD_INSUFERN_3,
                   CLFILE.CL_PD_INSACCERN_3,
                   CLFILE.CL_PD_INSEARN_ORDER_3,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_3,''),
                   CLFILE.CL_PD_INSCOMP_4,
                   ISNULL(CLFILE.CL_PD_INSOURS_4,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_4,
                   CLFILE.CL_PD_INSCOVR_4,
                   CLFILE.CL_PD_INSCOMM_4,
                   CLFILE.CL_PD_INSANTICERN_4,
                   CLFILE.CL_PD_INSANTICADJ_4,
                   CLFILE.CL_PD_INSUFERN_4,
                   CLFILE.CL_PD_INSACCERN_4,
                   CLFILE.CL_PD_INSEARN_ORDER_4,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_4,''),
                   CLFILE.CL_PD_INSCOMP_5,
                   ISNULL(CLFILE.CL_PD_INSOURS_5,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_5,
                   CLFILE.CL_PD_INSCOVR_5,
                   CLFILE.CL_PD_INSCOMM_5,
                   CLFILE.CL_PD_INSANTICERN_5,
                   CLFILE.CL_PD_INSANTICADJ_5,
                   CLFILE.CL_PD_INSUFERN_5,
                   CLFILE.CL_PD_INSACCERN_5,
                   CLFILE.CL_PD_INSEARN_ORDER_5,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_5,''),
                   CLFILE.CL_PD_INSCOMP_6,
                   ISNULL(CLFILE.CL_PD_INSOURS_6,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_6,
                   CLFILE.CL_PD_INSCOVR_6,
                   CLFILE.CL_PD_INSCOMM_6,
                   CLFILE.CL_PD_INSANTICERN_6,
                   CLFILE.CL_PD_INSANTICADJ_6,
                   CLFILE.CL_PD_INSUFERN_6,
                   CLFILE.CL_PD_INSACCERN_6,
                   CLFILE.CL_PD_INSEARN_ORDER_6,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_6,''),
                   CLFILE.CL_PD_INSCOMP_7,
                   ISNULL(CLFILE.CL_PD_INSOURS_7,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_7,
                   CLFILE.CL_PD_INSCOVR_7,
                   CLFILE.CL_PD_INSCOMM_7,
                   CLFILE.CL_PD_INSANTICERN_7,
                   CLFILE.CL_PD_INSANTICADJ_7,
                   CLFILE.CL_PD_INSUFERN_7,
                   CLFILE.CL_PD_INSACCERN_7,
                   CLFILE.CL_PD_INSEARN_ORDER_7,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_7,''),
                   CLFILE.CL_PD_INSCOMP_8,
                   ISNULL(CLFILE.CL_PD_INSOURS_8,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_8,
                   CLFILE.CL_PD_INSCOVR_8,
                   CLFILE.CL_PD_INSCOMM_8,
                   CLFILE.CL_PD_INSANTICERN_8,
                   CLFILE.CL_PD_INSANTICADJ_8,
                   CLFILE.CL_PD_INSUFERN_8,
                   CLFILE.CL_PD_INSACCERN_8,
                   CLFILE.CL_PD_INSEARN_ORDER_8,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_8,''),
                   CLFILE.CL_PD_INSCOMP_9,
                   ISNULL(CLFILE.CL_PD_INSOURS_9,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_9,
                   CLFILE.CL_PD_INSCOVR_9,
                   CLFILE.CL_PD_INSCOMM_9,
                   CLFILE.CL_PD_INSANTICERN_9,
                   CLFILE.CL_PD_INSANTICADJ_9,
                   CLFILE.CL_PD_INSUFERN_9,
                   CLFILE.CL_PD_INSACCERN_9,
                   CLFILE.CL_PD_INSEARN_ORDER_9,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_9,''),
                   CLFILE.CL_PD_INSCOMP_10,
                   ISNULL(CLFILE.CL_PD_INSOURS_10,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_10,
                   CLFILE.CL_PD_INSCOVR_10,
                   CLFILE.CL_PD_INSCOMM_10,
                   CLFILE.CL_PD_INSANTICERN_10,
                   CLFILE.CL_PD_INSANTICADJ_10,
                   CLFILE.CL_PD_INSUFERN_10,
                   CLFILE.CL_PD_INSACCERN_10,
                   CLFILE.CL_PD_INSEARN_ORDER_10,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_10,''),
                   CLFILE.CL_PD_PY_YYYYMM_RECDEL,
                   CLFILE.CL_PD_MTD_PAYS_RECDEL,
                   ISNULL(CLFILE.CL_PD_GARNISH_EXTR,''),
                   ISNULL(CLFILE.CL_PD_SHIP_SAME_FG,''),
                   CLFILE.CL_PD_AD_D_INS_COVR,
                   CLFILE.CL_PD_TOT_PARTIAL_CHGOFF,
                   ISNULL(CLFILE.CL_PD_LPO,''),
                   ISNULL(CLFILE.CL_PD_REP_CD,''),
                   CLFILE.CL_PD_EMP_ID,
                   ISNULL(CLFILE.CL_PD_PROCESS_CD,''),
                   CLFILE.CL_PD_WCBC_CREDITLIM,
                   ISNULL(CLFILE.CL_PD_FOLLOWUP_CD,''),
                   ISNULL(CLFILE.CL_PD_MIP_FG,''),
                   CLFILE.CL_PD_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(CLFILE.CL_PD_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_MIP_TOT_CLPAID,
                   ISNULL(CAST(CLFILE.CL_PD_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_MIP_TOT_AHPAID,
                   CLFILE.CL_PD_IL_GROSS_INCOME,
                   CLFILE.CL_PD_IL_GROSS_INCOME2,
                   ISNULL(CLFILE.CL_PD_RESCISSION,''),
                   ISNULL(CLFILE.CL_PD_INTERIM_TRW,''),
                   CLFILE.CL_PD_DLNO,
                   CLFILE.CL_PD_DLPOOL,
                   ISNULL(CLFILE.CL_PD_DLRECOURSE,''),
                   CLFILE.CL_PD_DLPARTRCAMT,
                   CLFILE.CL_PD_ORIGPURAMT,
                   CLFILE.CL_PD_PURBAL,
                   CLFILE.CL_PD_DLRVPRIN_1,
                   CLFILE.CL_PD_DLRVPRIN_2,
                   CLFILE.CL_PD_DLRVPRIN_3,
                   CLFILE.CL_PD_DLRVCHGS_1,
                   CLFILE.CL_PD_DLRVCHGS_2,
                   CLFILE.CL_PD_DLRVCHGS_3,
                   CLFILE.CL_PD_DLPARVHELD,
                   CLFILE.CL_PD_DLPARVPAID,
                   CLFILE.CL_PD_DLDISC,
                   ISNULL(CAST(CLFILE.CL_PD_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_CURBAL,
                   CLFILE.CL_PD_INTBAL,
                   CLFILE.CL_PD_LCBAL,
                   CLFILE.CL_PD_OTHBAL,
                   CLFILE.CL_PD_OT2BAL,
                   CLFILE.CL_PD_LCPAID,
                   CLFILE.CL_PD_LCPARTIALS,
                   CLFILE.CL_PD_STMT_COUNTER,
                   CLFILE.CL_PD_PRIOR_STMT_COUNTER,
                   ISNULL(CLFILE.CL_PD_FROZEN_REASON,''),
                   ISNULL(CLFILE.CL_PD_COUPON_STATEMENT,''),
                   CLFILE.CL_PD_MONTHEND_BAL,
                   CLFILE.CL_PD_ANTICTERM,
                   CLFILE.CL_PD_ANTICERN_1,
                   CLFILE.CL_PD_ANTICADJ_1,
                   CLFILE.CL_PD_UFERN_1,
                   CLFILE.CL_PD_ACCERN_1,
                   CLFILE.CL_PD_EARN_ORDER_1,
                   CLFILE.CL_PD_ANTICERN_2,
                   CLFILE.CL_PD_ANTICADJ_2,
                   CLFILE.CL_PD_UFERN_2,
                   CLFILE.CL_PD_ACCERN_2,
                   CLFILE.CL_PD_EARN_ORDER_2,
                   CLFILE.CL_PD_ANTICERN_3,
                   CLFILE.CL_PD_ANTICADJ_3,
                   CLFILE.CL_PD_UFERN_3,
                   CLFILE.CL_PD_ACCERN_3,
                   CLFILE.CL_PD_EARN_ORDER_3,
                   CLFILE.CL_PD_ANTICERN_4,
                   CLFILE.CL_PD_ANTICADJ_4,
                   CLFILE.CL_PD_UFERN_4,
                   CLFILE.CL_PD_ACCERN_4,
                   CLFILE.CL_PD_EARN_ORDER_4,
                   CLFILE.CL_PD_ANTICERN_5,
                   CLFILE.CL_PD_ANTICADJ_5,
                   CLFILE.CL_PD_UFERN_5,
                   CLFILE.CL_PD_ACCERN_5,
                   CLFILE.CL_PD_EARN_ORDER_5,
                   CLFILE.CL_PD_ANTICERN_6,
                   CLFILE.CL_PD_ANTICADJ_6,
                   CLFILE.CL_PD_UFERN_6,
                   CLFILE.CL_PD_ACCERN_6,
                   CLFILE.CL_PD_EARN_ORDER_6,
                   CLFILE.CL_PD_ANTICERN_7,
                   CLFILE.CL_PD_ANTICADJ_7,
                   CLFILE.CL_PD_UFERN_7,
                   CLFILE.CL_PD_ACCERN_7,
                   CLFILE.CL_PD_EARN_ORDER_7,
                   CLFILE.CL_PD_ANTICERN_8,
                   CLFILE.CL_PD_ANTICADJ_8,
                   CLFILE.CL_PD_UFERN_8,
                   CLFILE.CL_PD_ACCERN_8,
                   CLFILE.CL_PD_EARN_ORDER_8,
                   CLFILE.CL_PD_ANTICERN_9,
                   CLFILE.CL_PD_ANTICADJ_9,
                   CLFILE.CL_PD_UFERN_9,
                   CLFILE.CL_PD_ACCERN_9,
                   CLFILE.CL_PD_EARN_ORDER_9,
                   CLFILE.CL_PD_ANTICERN_10,
                   CLFILE.CL_PD_ANTICADJ_10,
                   CLFILE.CL_PD_UFERN_10,
                   CLFILE.CL_PD_ACCERN_10,
                   CLFILE.CL_PD_EARN_ORDER_10,
                   CLFILE.CL_PD_LMO_PRIOR_INTEREST,
                   CLFILE.CL_PD_PRIOR_UNEARNED,
                   ISNULL(CLFILE.CL_PD_ERNDELQ_STOP,''),
                   CLFILE.CL_PD_NONACRERN,
                   CLFILE.CL_PD_PRIOR_NONACRERN,
                   ISNULL(CAST(CLFILE.CL_PD_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_ERN_IB_PRINBAL,
                   CLFILE.CL_PD_ERN_IB_INTOWE,
                   CLFILE.CL_PD_TOTERN,
                   CLFILE.CL_PD_TOTOTH,
                   CLFILE.CL_PD_TOTINT,
                   CLFILE.CL_PD_TOTLCHG,
                   CLFILE.CL_PD_TOTDEF,
                   CLFILE.CL_PD_TOTSERACCT,
                   CLFILE.CL_PD_LCWAIVERS,
                   CLFILE.CL_PD_INTWAIVERS,
                   CLFILE.CL_PD_TOTPAYMNTD,
                   CLFILE.CL_PD_TOTEXCPAYMNTD,
                   CLFILE.CL_PD_TOTNODEF,
                   CLFILE.CL_PD_YTDNODEF,
                   CLFILE.CL_PD_TOTNODEFACT,
                   CLFILE.CL_PD_YTDNODEFACT,
                   CLFILE.CL_PD_TOTNOALDEL,
                   CLFILE.CL_PD_YTDNOALDEL,
                   CLFILE.CL_PD_YTDINTPD,
                   CLFILE.CL_PD_LYRINTPD,
                   CLFILE.CL_PD_TOTINTPD,
                   CLFILE.CL_PD_RECASTINT,
                   CLFILE.CL_PD_RECASTPRIN,
                   ISNULL(CLFILE.CL_PD_CL_REBATE,''),
                   ISNULL(CLFILE.CL_PD_AH_REBATE,''),
                   ISNULL(CLFILE.CL_PD_PP_REBATE,''),
                   ISNULL(CLFILE.CL_PD_INT_REBATE,''),
                   ISNULL(CLFILE.CL_PD_SC_REBATE,''),
                   ISNULL(CLFILE.CL_PD_MF_REBATE,''),
                   ISNULL(CLFILE.CL_PD_DF_REBATE,''),
                   ISNULL(CLFILE.CL_PD_EX_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O1_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O2_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O3_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O4_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O5_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O6_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O7_REBATE,''),
                   ISNULL(CLFILE.CL_PD_UN_O7_REB,''),
                   ISNULL(CLFILE.CL_PD_UN_O8_REB,''),
                   ISNULL(CLFILE.CL_PD_UN_O9_REB,''),
                   ISNULL(CLFILE.CL_PD_UN_O10_REB,''),
                   ISNULL(CLFILE.CL_PD_PLREASON,''),
                   ISNULL(CLFILE.CL_PD_COLLECTOR,''),
                   ISNULL(CLFILE.CL_PD_ACTIONCD,''),
                   ISNULL(CLFILE.CL_PD_ACTIONCD2,''),
                   ISNULL(CLFILE.CL_PD_ATCODE,''),
                   ISNULL(CLFILE.CL_PD_RECODE,''),
                   ISNULL(CLFILE.CL_PD_ESCROW_FG,''),
                   CLFILE.CL_PD_PROMDAYS_PASTDUE,
                   ISNULL(CLFILE.CL_PD_PROMID,''),
                   ISNULL(CAST(CLFILE.CL_PD_PROMDATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_PROMAMT,
                   ISNULL(CAST(CLFILE.CL_PD_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_AMT_PAID_LAST,
                   CLFILE.CL_PD_LASTCFORMNO,
                   ISNULL(CAST(CLFILE.CL_PD_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_DELCNT,
                   CLFILE.CL_PD_DELCNT2,
                   CLFILE.CL_PD_CREDITLIM,
                   CLFILE.CL_PD_CRSCORE,
                   CLFILE.CL_PD_LASTSFORMNO,
                   ISNULL(CAST(CLFILE.CL_PD_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_SOLCNT,
                   ISNULL(CLFILE.CL_PD_MESSAGE,''),
                   CLFILE.CL_PD_OLDACCTNO,
                   CLFILE.CL_PD_PRLNNO,
                   CLFILE.CL_PD_PRBAL,
                   CLFILE.CL_PD_PRINTDUE,
                   CLFILE.CL_PD_PRFINCHGRB,
                   CLFILE.CL_PD_PRINSRB,
                   ISNULL(CLFILE.CL_PD_PRPOCD,''),
                   ISNULL(CAST(CLFILE.CL_PD_PRPODATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_PRDELINQAMT,
                   CLFILE.CL_PD_PRLNAMT,
                   CLFILE.CL_PD_PRPDUE_DAYS,
                   CLFILE.CL_PD_PRDLDISC_UNEARN,
                   CLFILE.CL_PD_PRCONPROFILE,
                   CLFILE.CL_PD_SUM_ORIGPURAMT,
                   CLFILE.CL_PD_SUM_LNAMT,
                   CLFILE.CL_PD_SUM_FINCHG,
                   CLFILE.CL_PD_SUM_INTCHG,
                   CLFILE.CL_PD_SUM_ORGTERM,
                   CLFILE.CL_PD_SUM_REGPYAMT,
                   CLFILE.CL_PD_SUM_EXTCHG,
                   CLFILE.CL_PD_SUM_SERCHG,
                   CLFILE.CL_PD_SUM_MAINTFEE,
                   ISNULL(CLFILE.CL_PD_UNITPER_CD,''),
                   CLFILE.CL_PD_UNITPER_FREQ,
                   CLFILE.CL_PD_DAYS_IN_UNITPER,
                   CLFILE.CL_PD_UNITPER_PER_YEAR,
                   ISNULL(CLFILE.CL_PD_REPOCD,''),
                   ISNULL(CLFILE.CL_PD_BR_TFR_FG,''),
                   CLFILE.CL_PD_O3INS_DLPROC,
                   CLFILE.CL_PD_INSPREM_DLADJ,
                   CLFILE.CL_PD_PLAMT,
                   CLFILE.CL_PD_MTD_PRIN,
                   CLFILE.CL_PD_MTD_LC,
                   CLFILE.CL_PD_MTD_INT,
                   CLFILE.CL_PD_MTD_OT,
                   CLFILE.CL_PD_MTD_EX,
                   CLFILE.CL_PD_BNKRPT_CHAPTER,
                   ISNULL(CLFILE.CL_PD_BMO_CON_STATUS,''),
                   ISNULL(CLFILE.CL_PD_BMO_REC_STATUS,''),
                   CLFILE.CL_PD_APP_NUMBER,
                   ISNULL(CLFILE.CL_PD_GIFTNET,''),
                   CLFILE.CL_PD_GIFTNET_OUR_PO,
                   ISNULL(CLFILE.CL_PD_PROC_DISBURSED,''),
                   CLFILE.CL_PD_NOODDPY,
                   CLFILE.CL_PD_CONPROFILE_1,
                   CLFILE.CL_PD_CONPROFILE_2,
                   CLFILE.CL_PD_CONPROFILE_3,
                   CLFILE.CL_PD_CONPROFILE_4,
                   CLFILE.CL_PD_CONPROFILE_5,
                   CLFILE.CL_PD_RECPROFILE_1,
                   CLFILE.CL_PD_RECPROFILE_2,
                   CLFILE.CL_PD_RECPROFILE_3,
                   CLFILE.CL_PD_RECPROFILE_4,
                   CLFILE.CL_PD_RECPROFILE_5,
                   ISNULL(CLFILE.CL_PD_TRW_STATUS,''),
                   ISNULL(CLFILE.CL_PD_ORIGIN,''),
                   ISNULL(CLFILE.CL_PD_PO_TRCD,''),
                   CLFILE.CL_PD_NEXIS_CRSCORE,
                   ISNULL(CLFILE.CL_PD_TRW_B_CONSUMER_IND,''),
                   ISNULL(CLFILE.CL_PD_PARVHELD_PAID_FG,''),
                   ISNULL(CLFILE.CL_PD_ACCT_FROZEN,''),
                   ISNULL(CLFILE.CL_PD_TRW_SPECIAL_CODE,''),
                   ISNULL(CLFILE.CL_PD_CREATION_STATUS,''),
                   CLFILE.CL_PD_TOTNOLCHG,
                   ISNULL(CLFILE.CL_PD_LBOX,''),
                   CLFILE.CL_PD_EXTRA_PRIN_LFP,
                   CLFILE.CL_PD_REMAIN_INT_SINCE_LFP,
                   CLFILE.CL_PD_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(CLFILE.CL_PD_INTERIM_COUPON,''),
                   CLFILE.CL_PD_DLCOMM_O3_CMSI,
                   CLFILE.CL_PD_DLCOMM_PP_CMSI,
                   ISNULL(CLFILE.CL_PD_TRW_J_CONSUMER_IND,''),
                   ISNULL(CLFILE.CL_PD_ACH,''),
                   ISNULL(CLFILE.CL_PD_1099_C_FG,''),
                   ISNULL(CLFILE.CL_PD_TRW_COMPLIANCE_CODE,''),
                   ISNULL(CLFILE.CL_PD_UNWIND_REASON_CODE,''),
                   ISNULL(CLFILE.CL_PD_PBI_PBD_FG,''),
                   ISNULL(CLFILE.CL_PD_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(CLFILE.CL_PD_APPROVED_BY_USER,''),
                   ISNULL(CLFILE.CL_PD_APPROVED_BY_CODE,''),
                   ISNULL(CAST(CLFILE.CL_PD_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_APPROVED_BY_TIME,
                   ISNULL(CAST(CLFILE.CL_PD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_LTOUCH_TIME,
                   ISNULL(CLFILE.CL_PD_HOLD_COLLATERAL,''),
                   ISNULL(CLFILE.CL_PD_DLPAID,''),
                   CLFILE.CL_PD_CKNO,
                   ISNULL(CLFILE.CL_PD_FREE1,''),
                   ISNULL(CLFILE.CL_PD_FREE2,''),
                   ISNULL(CLFILE.CL_PD_FREE_SPACE_FILLER,''),
                   CLFILE.CL_PD_POSTING_BR,
                   ISNULL(CLFILE.CL_PD_OTHBR_FG,''),
                   CLFILE.CL_PD_APPROVAL_LIMIT,
                   ISNULL(CLFILE.CL_PD_APPROVAL_USERID,''),
                   CLFILE.CL_PD_MIN_CRED_SCORE,
                   ISNULL(CLFILE.CL_PD_CRED_SCORE_USERID,''),
                   CLFILE.CL_PD_FINAMT,
                   ISNULL(CLFILE.CL_PD_PROCEEDS_FG,''),
                   ISNULL(CLFILE.CL_PD_POINTS_FG,''),
                   ISNULL(CLFILE.CL_PD_OVRDRATE_FG,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_1,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_2,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_3,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_4,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_5,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_6,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_7,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_8,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_9,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_10,''),
                   ISNULL(CLFILE.CL_PD_SERCHG_FG,''),
                   ISNULL(CLFILE.CL_PD_MAINTFEE_FG,''),
                   ISNULL(CLFILE.CL_PD_EXTCHG_FG,''),
                   ISNULL(CLFILE.CL_PD_REGPYAMT_FG,''),
                   ISNULL(CLFILE.CL_PD_FEECD_1,''),
                   ISNULL(CLFILE.CL_PD_FEECD_2,''),
                   ISNULL(CLFILE.CL_PD_FEECD_3,''),
                   ISNULL(CLFILE.CL_PD_FEECD_4,''),
                   ISNULL(CLFILE.CL_PD_FEECD_5,''),
                   ISNULL(CLFILE.CL_PD_FEECD_6,''),
                   ISNULL(CLFILE.CL_PD_FEECD_7,''),
                   ISNULL(CLFILE.CL_PD_FEECD_8,''),
                   ISNULL(CLFILE.CL_PD_FEECD_9,''),
                   ISNULL(CLFILE.CL_PD_FEECD_10,''),
                   ISNULL(CLFILE.CL_PD_FEECD_11,''),
                   ISNULL(CLFILE.CL_PD_FEECD_12,''),
                   ISNULL(CLFILE.CL_PD_FEECD_13,''),
                   ISNULL(CLFILE.CL_PD_FEECD_14,''),
                   ISNULL(CLFILE.CL_PD_FEECD_15,''),
                   ISNULL(CLFILE.CL_PD_FEECD_16,''),
                   ISNULL(CLFILE.CL_PD_FEECD_17,''),
                   ISNULL(CLFILE.CL_PD_FEECD_18,''),
                   ISNULL(CLFILE.CL_PD_FEECD_19,''),
                   ISNULL(CLFILE.CL_PD_FEECD_20,''),
                   CLFILE.CL_PD_PRPAID_AHEAD,
                   CLFILE.CL_PD_RESTRICT_ADJ,
                   CLFILE.CL_PD_DUAL_LOAN_ADJ,
                   CLFILE.CL_PD_CAN_LOAN_MAX,
                   CLFILE.CL_PD_PERDOWN,
                   ISNULL(CLFILE.CL_PD_SELLPR_FG,''),
                   ISNULL(CLFILE.CL_PD_DOWNPY_FG,''),
                   ISNULL(CLFILE.CL_PD_PERDOWN_FG,''),
                   ISNULL(CLFILE.CL_PD_SELL_DOWN_FG,''),
                   ISNULL(CLFILE.CL_PD_BALLOON_FG,''),
                   ISNULL(CLFILE.CL_PD_BALLOON_LAST_FG,''),
                   ISNULL(CLFILE.CL_PD_FEECD_FG_1,''),
                   ISNULL(CLFILE.CL_PD_FEECD_FG_2,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_1,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_1,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_1,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_2,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_2,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_2,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_3,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_3,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_3,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_4,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_4,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_4,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_5,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_5,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_5,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_6,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_6,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_6,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_7,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_7,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_7,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_8,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_8,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_8,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_9,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_9,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_9,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_10,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_10,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_10,''),
                   ISNULL(CLFILE.CL_PD_DLDISC_FG,''),
                   ISNULL(CLFILE.CL_PD_DLRVPRIN_FG_1,''),
                   ISNULL(CLFILE.CL_PD_DLRVPRIN_FG_2,''),
                   ISNULL(CLFILE.CL_PD_DLRVPRIN_FG_3,''),
                   ISNULL(CLFILE.CL_PD_DLRVCHGS_FG_1,''),
                   ISNULL(CLFILE.CL_PD_DLRVCHGS_FG_2,''),
                   ISNULL(CLFILE.CL_PD_DLRVCHGS_FG_3,''),
                   ISNULL(CLFILE.CL_PD_DLPARVHELD_FG,''),
                   ISNULL(CLFILE.CL_PD_DLPARVPAID_FG,''),
                   ISNULL(CLFILE.CL_PD_ONE_MTH_NO_DAY_EXT_SW,''),
                   ISNULL(CLFILE.CL_PD_ASSIGN_RN_ACCTNO,''),
                   CLFILE.CL_PD_APR_FINAMT,
                   ISNULL(CLFILE.CL_PD_DLCOMM_PP_CMSI_FG,''),
                   ISNULL(CLFILE.CL_PD_DLCOMM_O3_CMSI_FG,''),
                   ISNULL(CLFILE.CL_PD_FILLER_TO_MATCH_LN,''),
                   ISNULL(CLFILE.CL_PD2_ID,''),
                   CLFILE.CL_PD2_BRANCH,
                   ISNULL(CLFILE.CL_PD2_BWNAME,'')
            FROM DBO.CLFILE
            WHERE CLFILE.CL_GROUP_ID >= :QCL2-WBEG-GROUP
              AND CLFILE.CL_GROUP_ID <= :QCL2-WEND-GROUP
              AND CLFILE.CL_DESC >= :QCL2-WBEG-DESC
              AND CLFILE.CL_DESC <= :QCL2-WEND-DESC
              ORDER BY CLFILE.CL_GROUP_ID,
                       CLFILE.CL_DESC,
                       CLFILE.CL_CAN_NO
           END-EXEC.

           EXEC SQL
               OPEN CL2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QCL2-WBEG-GROUP
                          QCL2-WEND-GROUP
                          QCL2-WBEG-DESC
                          QCL2-WEND-DESC.

           MOVE STAT---GOOD TO CL2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CL2-FILE.
           PERFORM READ-IT.
           MOVE CL-SELECT-PATH TO E-FILE.
           MOVE SPACES TO E-KEYX.
           STRING CL-GROUP-ID,CL-CAN-NO DELIMITED BY "   "
                          INTO E-KEYX.

           MOVE CL-GROUP-ID    TO QCL-GROUP-ID. 
           MOVE CL-DESC        TO QCL-DESC.

           EXEC SQL
            SELECT ISNULL(CLFILE.CL_GROUP_ID,''),
                   ISNULL(CLFILE.CL_CAN_NO,''),
                   ISNULL(CAST(CLFILE.CL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CLFILE.CL_DESC,''),
                   ISNULL(CLFILE.CL_GIFTNET,''),
                   ISNULL(CLFILE.CL_PD_ID,''),
                   CLFILE.CL_PD_BRANCH,
                   CLFILE.CL_PD_SOC_SEC,
                   CLFILE.CL_PD_OWNBR,
                   CLFILE.CL_PD_ACCTNO,
                   CLFILE.CL_PD_CLASS,
                   CLFILE.CL_PD_SUBCLASS,
                   CLFILE.CL_PD_LAWCODE,
                   CLFILE.CL_PD_SSNO1,
                   ISNULL(CLFILE.CL_PD_MAKERCD1,''),
                   CLFILE.CL_PD_SSNO2,
                   ISNULL(CLFILE.CL_PD_MAKERCD2,''),
                   CLFILE.CL_PD_SSNO3,
                   ISNULL(CLFILE.CL_PD_MAKERCD3,''),
                   CLFILE.CL_PD_SSNO4,
                   ISNULL(CLFILE.CL_PD_MAKERCD4,''),
                   ISNULL(CLFILE.CL_PD_ORGST,''),
                   CLFILE.CL_PD_ORGBR,
                   ISNULL(CLFILE.CL_PD_OWNST,''),
                   CLFILE.CL_PD_SPRCLASS,
                   ISNULL(CLFILE.CL_PD_CONVERCD,''),
                   ISNULL(CLFILE.CL_PD_MIDSTREAM,''),
                   ISNULL(CLFILE.CL_PD_PURCD,''),
                   ISNULL(CLFILE.CL_PD_SRCD,''),
                   ISNULL(CLFILE.CL_PD_COLLCD,''),
                   ISNULL(CLFILE.CL_PD_COLLID,''),
                   ISNULL(CLFILE.CL_PD_POCD,''),
                   ISNULL(CLFILE.CL_PD_PLCD,''),
                   ISNULL(CLFILE.CL_PD_PLTYPE,''),
                   ISNULL(CLFILE.CL_PD_CONFIDENTCD,''),
                   ISNULL(CLFILE.CL_PD_ALLOTCD,''),
                   ISNULL(CLFILE.CL_PD_RECASTCD,''),
                   ISNULL(CLFILE.CL_PD_SKIP_TRW,''),
                   ISNULL(CLFILE.CL_PD_STATUSFG,''),
                   ISNULL(CLFILE.CL_PD_SMALLOAN,''),
                   ISNULL(CLFILE.CL_PD_SALEFIN,''),
                   ISNULL(CLFILE.CL_PD_SERVACCT,''),
                   ISNULL(CLFILE.CL_PD_LOANTYPE,''),
                   ISNULL(CLFILE.CL_PD_ADDON_FG,''),
                   ISNULL(CLFILE.CL_PD_PAY_SCHLD_FG,''),
                   CLFILE.CL_PD_LC_CODE,
                   ISNULL(CLFILE.CL_PD_CAN_LOAN_NO,''),
                   ISNULL(CLFILE.CL_PD_STATFLAGS,''),
                   ISNULL(CAST(CLFILE.CL_PD_ENTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PURDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_CASHDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_FILEDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_JDDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PLDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_REDUDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_POFFDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_REPODATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_SOLDDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_APRATE,
                   CLFILE.CL_PD_EFFRATE,
                   CLFILE.CL_PD_SMPRATE,
                   CLFILE.CL_PD_YLDRATE,
                   CLFILE.CL_PD_OVRDRATE,
                   CLFILE.CL_PD_ENDORATE,
                   CLFILE.CL_PD_JDRATE,
                   CLFILE.CL_PD_ACCRATE,
                   CLFILE.CL_PD_REDURATE,
                   CLFILE.CL_PD_CONRATE,
                   CLFILE.CL_PD_SERVPER,
                   CLFILE.CL_PD_SELLPR,
                   CLFILE.CL_PD_DOWNPY,
                   CLFILE.CL_PD_TRADEIN,
                   CLFILE.CL_PD_LNAMT,
                   CLFILE.CL_PD_FINCHG,
                   CLFILE.CL_PD_INTCHG,
                   CLFILE.CL_PD_SERCHG,
                   CLFILE.CL_PD_EXTCHG,
                   CLFILE.CL_PD_MAINTFEE,
                   CLFILE.CL_PD_POINTS,
                   CLFILE.CL_PD_PROCEEDS,
                   CLFILE.CL_PD_ORGTERM,
                   CLFILE.CL_PD_1STPYAMT,
                   CLFILE.CL_PD_NOREGPY,
                   CLFILE.CL_PD_REGPYAMT,
                   CLFILE.CL_PD_LASTPYAMT,
                   CLFILE.CL_PD_PROCASH,
                   CLFILE.CL_PD_TILLNO,
                   ISNULL(CLFILE.CL_PD_STATE_FLAG,''),
                   CLFILE.CL_PD_STATE_WORKER,
                   CLFILE.CL_PD_STATE_WORKER2,
                   ISNULL(CLFILE.CL_PD_FEECODE_1,''),
                   CLFILE.CL_PD_FEEAMT_1,
                   ISNULL(CLFILE.CL_PD_FEEOURS_1,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_1,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_2,''),
                   CLFILE.CL_PD_FEEAMT_2,
                   ISNULL(CLFILE.CL_PD_FEEOURS_2,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_2,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_3,''),
                   CLFILE.CL_PD_FEEAMT_3,
                   ISNULL(CLFILE.CL_PD_FEEOURS_3,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_3,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_4,''),
                   CLFILE.CL_PD_FEEAMT_4,
                   ISNULL(CLFILE.CL_PD_FEEOURS_4,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_4,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_5,''),
                   CLFILE.CL_PD_FEEAMT_5,
                   ISNULL(CLFILE.CL_PD_FEEOURS_5,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_5,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_6,''),
                   CLFILE.CL_PD_FEEAMT_6,
                   ISNULL(CLFILE.CL_PD_FEEOURS_6,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_6,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_7,''),
                   CLFILE.CL_PD_FEEAMT_7,
                   ISNULL(CLFILE.CL_PD_FEEOURS_7,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_7,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_8,''),
                   CLFILE.CL_PD_FEEAMT_8,
                   ISNULL(CLFILE.CL_PD_FEEOURS_8,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_8,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_9,''),
                   CLFILE.CL_PD_FEEAMT_9,
                   ISNULL(CLFILE.CL_PD_FEEOURS_9,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_9,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_10,''),
                   CLFILE.CL_PD_FEEAMT_10,
                   ISNULL(CLFILE.CL_PD_FEEOURS_10,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_10,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_11,''),
                   CLFILE.CL_PD_FEEAMT_11,
                   ISNULL(CLFILE.CL_PD_FEEOURS_11,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_11,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_12,''),
                   CLFILE.CL_PD_FEEAMT_12,
                   ISNULL(CLFILE.CL_PD_FEEOURS_12,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_12,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_13,''),
                   CLFILE.CL_PD_FEEAMT_13,
                   ISNULL(CLFILE.CL_PD_FEEOURS_13,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_13,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_14,''),
                   CLFILE.CL_PD_FEEAMT_14,
                   ISNULL(CLFILE.CL_PD_FEEOURS_14,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_14,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_15,''),
                   CLFILE.CL_PD_FEEAMT_15,
                   ISNULL(CLFILE.CL_PD_FEEOURS_15,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_15,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_16,''),
                   CLFILE.CL_PD_FEEAMT_16,
                   ISNULL(CLFILE.CL_PD_FEEOURS_16,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_16,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_17,''),
                   CLFILE.CL_PD_FEEAMT_17,
                   ISNULL(CLFILE.CL_PD_FEEOURS_17,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_17,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_18,''),
                   CLFILE.CL_PD_FEEAMT_18,
                   ISNULL(CLFILE.CL_PD_FEEOURS_18,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_18,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_19,''),
                   CLFILE.CL_PD_FEEAMT_19,
                   ISNULL(CLFILE.CL_PD_FEEOURS_19,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_19,''),
                   ISNULL(CLFILE.CL_PD_FEECODE_20,''),
                   CLFILE.CL_PD_FEEAMT_20,
                   ISNULL(CLFILE.CL_PD_FEEOURS_20,''),
                   ISNULL(CLFILE.CL_PD_FEEFINCHG_20,''),
                   ISNULL(CLFILE.CL_PD_INSURED,''),
                   ISNULL(CLFILE.CL_PD_INSTYPES,''),
                   ISNULL(CLFILE.CL_PD_INSELIGIBLE,''),
                   CLFILE.CL_PD_INSCOMP_1,
                   ISNULL(CLFILE.CL_PD_INSOURS_1,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_1,
                   CLFILE.CL_PD_INSCOVR_1,
                   CLFILE.CL_PD_INSCOMM_1,
                   CLFILE.CL_PD_INSANTICERN_1,
                   CLFILE.CL_PD_INSANTICADJ_1,
                   CLFILE.CL_PD_INSUFERN_1,
                   CLFILE.CL_PD_INSACCERN_1,
                   CLFILE.CL_PD_INSEARN_ORDER_1,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_1,''),
                   CLFILE.CL_PD_INSCOMP_2,
                   ISNULL(CLFILE.CL_PD_INSOURS_2,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_2,
                   CLFILE.CL_PD_INSCOVR_2,
                   CLFILE.CL_PD_INSCOMM_2,
                   CLFILE.CL_PD_INSANTICERN_2,
                   CLFILE.CL_PD_INSANTICADJ_2,
                   CLFILE.CL_PD_INSUFERN_2,
                   CLFILE.CL_PD_INSACCERN_2,
                   CLFILE.CL_PD_INSEARN_ORDER_2,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_2,''),
                   CLFILE.CL_PD_INSCOMP_3,
                   ISNULL(CLFILE.CL_PD_INSOURS_3,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_3,
                   CLFILE.CL_PD_INSCOVR_3,
                   CLFILE.CL_PD_INSCOMM_3,
                   CLFILE.CL_PD_INSANTICERN_3,
                   CLFILE.CL_PD_INSANTICADJ_3,
                   CLFILE.CL_PD_INSUFERN_3,
                   CLFILE.CL_PD_INSACCERN_3,
                   CLFILE.CL_PD_INSEARN_ORDER_3,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_3,''),
                   CLFILE.CL_PD_INSCOMP_4,
                   ISNULL(CLFILE.CL_PD_INSOURS_4,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_4,
                   CLFILE.CL_PD_INSCOVR_4,
                   CLFILE.CL_PD_INSCOMM_4,
                   CLFILE.CL_PD_INSANTICERN_4,
                   CLFILE.CL_PD_INSANTICADJ_4,
                   CLFILE.CL_PD_INSUFERN_4,
                   CLFILE.CL_PD_INSACCERN_4,
                   CLFILE.CL_PD_INSEARN_ORDER_4,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_4,''),
                   CLFILE.CL_PD_INSCOMP_5,
                   ISNULL(CLFILE.CL_PD_INSOURS_5,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_5,
                   CLFILE.CL_PD_INSCOVR_5,
                   CLFILE.CL_PD_INSCOMM_5,
                   CLFILE.CL_PD_INSANTICERN_5,
                   CLFILE.CL_PD_INSANTICADJ_5,
                   CLFILE.CL_PD_INSUFERN_5,
                   CLFILE.CL_PD_INSACCERN_5,
                   CLFILE.CL_PD_INSEARN_ORDER_5,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_5,''),
                   CLFILE.CL_PD_INSCOMP_6,
                   ISNULL(CLFILE.CL_PD_INSOURS_6,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_6,
                   CLFILE.CL_PD_INSCOVR_6,
                   CLFILE.CL_PD_INSCOMM_6,
                   CLFILE.CL_PD_INSANTICERN_6,
                   CLFILE.CL_PD_INSANTICADJ_6,
                   CLFILE.CL_PD_INSUFERN_6,
                   CLFILE.CL_PD_INSACCERN_6,
                   CLFILE.CL_PD_INSEARN_ORDER_6,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_6,''),
                   CLFILE.CL_PD_INSCOMP_7,
                   ISNULL(CLFILE.CL_PD_INSOURS_7,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_7,
                   CLFILE.CL_PD_INSCOVR_7,
                   CLFILE.CL_PD_INSCOMM_7,
                   CLFILE.CL_PD_INSANTICERN_7,
                   CLFILE.CL_PD_INSANTICADJ_7,
                   CLFILE.CL_PD_INSUFERN_7,
                   CLFILE.CL_PD_INSACCERN_7,
                   CLFILE.CL_PD_INSEARN_ORDER_7,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_7,''),
                   CLFILE.CL_PD_INSCOMP_8,
                   ISNULL(CLFILE.CL_PD_INSOURS_8,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_8,
                   CLFILE.CL_PD_INSCOVR_8,
                   CLFILE.CL_PD_INSCOMM_8,
                   CLFILE.CL_PD_INSANTICERN_8,
                   CLFILE.CL_PD_INSANTICADJ_8,
                   CLFILE.CL_PD_INSUFERN_8,
                   CLFILE.CL_PD_INSACCERN_8,
                   CLFILE.CL_PD_INSEARN_ORDER_8,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_8,''),
                   CLFILE.CL_PD_INSCOMP_9,
                   ISNULL(CLFILE.CL_PD_INSOURS_9,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_9,
                   CLFILE.CL_PD_INSCOVR_9,
                   CLFILE.CL_PD_INSCOMM_9,
                   CLFILE.CL_PD_INSANTICERN_9,
                   CLFILE.CL_PD_INSANTICADJ_9,
                   CLFILE.CL_PD_INSUFERN_9,
                   CLFILE.CL_PD_INSACCERN_9,
                   CLFILE.CL_PD_INSEARN_ORDER_9,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_9,''),
                   CLFILE.CL_PD_INSCOMP_10,
                   ISNULL(CLFILE.CL_PD_INSOURS_10,''),
                   ISNULL(CAST(CLFILE.CL_PD_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_INSPREM_10,
                   CLFILE.CL_PD_INSCOVR_10,
                   CLFILE.CL_PD_INSCOMM_10,
                   CLFILE.CL_PD_INSANTICERN_10,
                   CLFILE.CL_PD_INSANTICADJ_10,
                   CLFILE.CL_PD_INSUFERN_10,
                   CLFILE.CL_PD_INSACCERN_10,
                   CLFILE.CL_PD_INSEARN_ORDER_10,
                   ISNULL(CLFILE.CL_PD_INSFINCHG_10,''),
                   CLFILE.CL_PD_PY_YYYYMM_RECDEL,
                   CLFILE.CL_PD_MTD_PAYS_RECDEL,
                   ISNULL(CLFILE.CL_PD_GARNISH_EXTR,''),
                   ISNULL(CLFILE.CL_PD_SHIP_SAME_FG,''),
                   CLFILE.CL_PD_AD_D_INS_COVR,
                   CLFILE.CL_PD_TOT_PARTIAL_CHGOFF,
                   ISNULL(CLFILE.CL_PD_LPO,''),
                   ISNULL(CLFILE.CL_PD_REP_CD,''),
                   CLFILE.CL_PD_EMP_ID,
                   ISNULL(CLFILE.CL_PD_PROCESS_CD,''),
                   CLFILE.CL_PD_WCBC_CREDITLIM,
                   ISNULL(CLFILE.CL_PD_FOLLOWUP_CD,''),
                   ISNULL(CLFILE.CL_PD_MIP_FG,''),
                   CLFILE.CL_PD_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(CLFILE.CL_PD_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_MIP_TOT_CLPAID,
                   ISNULL(CAST(CLFILE.CL_PD_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_MIP_TOT_AHPAID,
                   CLFILE.CL_PD_IL_GROSS_INCOME,
                   CLFILE.CL_PD_IL_GROSS_INCOME2,
                   ISNULL(CLFILE.CL_PD_RESCISSION,''),
                   ISNULL(CLFILE.CL_PD_INTERIM_TRW,''),
                   CLFILE.CL_PD_DLNO,
                   CLFILE.CL_PD_DLPOOL,
                   ISNULL(CLFILE.CL_PD_DLRECOURSE,''),
                   CLFILE.CL_PD_DLPARTRCAMT,
                   CLFILE.CL_PD_ORIGPURAMT,
                   CLFILE.CL_PD_PURBAL,
                   CLFILE.CL_PD_DLRVPRIN_1,
                   CLFILE.CL_PD_DLRVPRIN_2,
                   CLFILE.CL_PD_DLRVPRIN_3,
                   CLFILE.CL_PD_DLRVCHGS_1,
                   CLFILE.CL_PD_DLRVCHGS_2,
                   CLFILE.CL_PD_DLRVCHGS_3,
                   CLFILE.CL_PD_DLPARVHELD,
                   CLFILE.CL_PD_DLPARVPAID,
                   CLFILE.CL_PD_DLDISC,
                   ISNULL(CAST(CLFILE.CL_PD_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_CURBAL,
                   CLFILE.CL_PD_INTBAL,
                   CLFILE.CL_PD_LCBAL,
                   CLFILE.CL_PD_OTHBAL,
                   CLFILE.CL_PD_OT2BAL,
                   CLFILE.CL_PD_LCPAID,
                   CLFILE.CL_PD_LCPARTIALS,
                   CLFILE.CL_PD_STMT_COUNTER,
                   CLFILE.CL_PD_PRIOR_STMT_COUNTER,
                   ISNULL(CLFILE.CL_PD_FROZEN_REASON,''),
                   ISNULL(CLFILE.CL_PD_COUPON_STATEMENT,''),
                   CLFILE.CL_PD_MONTHEND_BAL,
                   CLFILE.CL_PD_ANTICTERM,
                   CLFILE.CL_PD_ANTICERN_1,
                   CLFILE.CL_PD_ANTICADJ_1,
                   CLFILE.CL_PD_UFERN_1,
                   CLFILE.CL_PD_ACCERN_1,
                   CLFILE.CL_PD_EARN_ORDER_1,
                   CLFILE.CL_PD_ANTICERN_2,
                   CLFILE.CL_PD_ANTICADJ_2,
                   CLFILE.CL_PD_UFERN_2,
                   CLFILE.CL_PD_ACCERN_2,
                   CLFILE.CL_PD_EARN_ORDER_2,
                   CLFILE.CL_PD_ANTICERN_3,
                   CLFILE.CL_PD_ANTICADJ_3,
                   CLFILE.CL_PD_UFERN_3,
                   CLFILE.CL_PD_ACCERN_3,
                   CLFILE.CL_PD_EARN_ORDER_3,
                   CLFILE.CL_PD_ANTICERN_4,
                   CLFILE.CL_PD_ANTICADJ_4,
                   CLFILE.CL_PD_UFERN_4,
                   CLFILE.CL_PD_ACCERN_4,
                   CLFILE.CL_PD_EARN_ORDER_4,
                   CLFILE.CL_PD_ANTICERN_5,
                   CLFILE.CL_PD_ANTICADJ_5,
                   CLFILE.CL_PD_UFERN_5,
                   CLFILE.CL_PD_ACCERN_5,
                   CLFILE.CL_PD_EARN_ORDER_5,
                   CLFILE.CL_PD_ANTICERN_6,
                   CLFILE.CL_PD_ANTICADJ_6,
                   CLFILE.CL_PD_UFERN_6,
                   CLFILE.CL_PD_ACCERN_6,
                   CLFILE.CL_PD_EARN_ORDER_6,
                   CLFILE.CL_PD_ANTICERN_7,
                   CLFILE.CL_PD_ANTICADJ_7,
                   CLFILE.CL_PD_UFERN_7,
                   CLFILE.CL_PD_ACCERN_7,
                   CLFILE.CL_PD_EARN_ORDER_7,
                   CLFILE.CL_PD_ANTICERN_8,
                   CLFILE.CL_PD_ANTICADJ_8,
                   CLFILE.CL_PD_UFERN_8,
                   CLFILE.CL_PD_ACCERN_8,
                   CLFILE.CL_PD_EARN_ORDER_8,
                   CLFILE.CL_PD_ANTICERN_9,
                   CLFILE.CL_PD_ANTICADJ_9,
                   CLFILE.CL_PD_UFERN_9,
                   CLFILE.CL_PD_ACCERN_9,
                   CLFILE.CL_PD_EARN_ORDER_9,
                   CLFILE.CL_PD_ANTICERN_10,
                   CLFILE.CL_PD_ANTICADJ_10,
                   CLFILE.CL_PD_UFERN_10,
                   CLFILE.CL_PD_ACCERN_10,
                   CLFILE.CL_PD_EARN_ORDER_10,
                   CLFILE.CL_PD_LMO_PRIOR_INTEREST,
                   CLFILE.CL_PD_PRIOR_UNEARNED,
                   ISNULL(CLFILE.CL_PD_ERNDELQ_STOP,''),
                   CLFILE.CL_PD_NONACRERN,
                   CLFILE.CL_PD_PRIOR_NONACRERN,
                   ISNULL(CAST(CLFILE.CL_PD_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_ERN_IB_PRINBAL,
                   CLFILE.CL_PD_ERN_IB_INTOWE,
                   CLFILE.CL_PD_TOTERN,
                   CLFILE.CL_PD_TOTOTH,
                   CLFILE.CL_PD_TOTINT,
                   CLFILE.CL_PD_TOTLCHG,
                   CLFILE.CL_PD_TOTDEF,
                   CLFILE.CL_PD_TOTSERACCT,
                   CLFILE.CL_PD_LCWAIVERS,
                   CLFILE.CL_PD_INTWAIVERS,
                   CLFILE.CL_PD_TOTPAYMNTD,
                   CLFILE.CL_PD_TOTEXCPAYMNTD,
                   CLFILE.CL_PD_TOTNODEF,
                   CLFILE.CL_PD_YTDNODEF,
                   CLFILE.CL_PD_TOTNODEFACT,
                   CLFILE.CL_PD_YTDNODEFACT,
                   CLFILE.CL_PD_TOTNOALDEL,
                   CLFILE.CL_PD_YTDNOALDEL,
                   CLFILE.CL_PD_YTDINTPD,
                   CLFILE.CL_PD_LYRINTPD,
                   CLFILE.CL_PD_TOTINTPD,
                   CLFILE.CL_PD_RECASTINT,
                   CLFILE.CL_PD_RECASTPRIN,
                   ISNULL(CLFILE.CL_PD_CL_REBATE,''),
                   ISNULL(CLFILE.CL_PD_AH_REBATE,''),
                   ISNULL(CLFILE.CL_PD_PP_REBATE,''),
                   ISNULL(CLFILE.CL_PD_INT_REBATE,''),
                   ISNULL(CLFILE.CL_PD_SC_REBATE,''),
                   ISNULL(CLFILE.CL_PD_MF_REBATE,''),
                   ISNULL(CLFILE.CL_PD_DF_REBATE,''),
                   ISNULL(CLFILE.CL_PD_EX_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O1_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O2_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O3_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O4_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O5_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O6_REBATE,''),
                   ISNULL(CLFILE.CL_PD_O7_REBATE,''),
                   ISNULL(CLFILE.CL_PD_UN_O7_REB,''),
                   ISNULL(CLFILE.CL_PD_UN_O8_REB,''),
                   ISNULL(CLFILE.CL_PD_UN_O9_REB,''),
                   ISNULL(CLFILE.CL_PD_UN_O10_REB,''),
                   ISNULL(CLFILE.CL_PD_PLREASON,''),
                   ISNULL(CLFILE.CL_PD_COLLECTOR,''),
                   ISNULL(CLFILE.CL_PD_ACTIONCD,''),
                   ISNULL(CLFILE.CL_PD_ACTIONCD2,''),
                   ISNULL(CLFILE.CL_PD_ATCODE,''),
                   ISNULL(CLFILE.CL_PD_RECODE,''),
                   ISNULL(CLFILE.CL_PD_ESCROW_FG,''),
                   CLFILE.CL_PD_PROMDAYS_PASTDUE,
                   ISNULL(CLFILE.CL_PD_PROMID,''),
                   ISNULL(CAST(CLFILE.CL_PD_PROMDATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_PROMAMT,
                   ISNULL(CAST(CLFILE.CL_PD_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(CLFILE.CL_PD_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_AMT_PAID_LAST,
                   CLFILE.CL_PD_LASTCFORMNO,
                   ISNULL(CAST(CLFILE.CL_PD_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_DELCNT,
                   CLFILE.CL_PD_DELCNT2,
                   CLFILE.CL_PD_CREDITLIM,
                   CLFILE.CL_PD_CRSCORE,
                   CLFILE.CL_PD_LASTSFORMNO,
                   ISNULL(CAST(CLFILE.CL_PD_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_SOLCNT,
                   ISNULL(CLFILE.CL_PD_MESSAGE,''),
                   CLFILE.CL_PD_OLDACCTNO,
                   CLFILE.CL_PD_PRLNNO,
                   CLFILE.CL_PD_PRBAL,
                   CLFILE.CL_PD_PRINTDUE,
                   CLFILE.CL_PD_PRFINCHGRB,
                   CLFILE.CL_PD_PRINSRB,
                   ISNULL(CLFILE.CL_PD_PRPOCD,''),
                   ISNULL(CAST(CLFILE.CL_PD_PRPODATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_PRDELINQAMT,
                   CLFILE.CL_PD_PRLNAMT,
                   CLFILE.CL_PD_PRPDUE_DAYS,
                   CLFILE.CL_PD_PRDLDISC_UNEARN,
                   CLFILE.CL_PD_PRCONPROFILE,
                   CLFILE.CL_PD_SUM_ORIGPURAMT,
                   CLFILE.CL_PD_SUM_LNAMT,
                   CLFILE.CL_PD_SUM_FINCHG,
                   CLFILE.CL_PD_SUM_INTCHG,
                   CLFILE.CL_PD_SUM_ORGTERM,
                   CLFILE.CL_PD_SUM_REGPYAMT,
                   CLFILE.CL_PD_SUM_EXTCHG,
                   CLFILE.CL_PD_SUM_SERCHG,
                   CLFILE.CL_PD_SUM_MAINTFEE,
                   ISNULL(CLFILE.CL_PD_UNITPER_CD,''),
                   CLFILE.CL_PD_UNITPER_FREQ,
                   CLFILE.CL_PD_DAYS_IN_UNITPER,
                   CLFILE.CL_PD_UNITPER_PER_YEAR,
                   ISNULL(CLFILE.CL_PD_REPOCD,''),
                   ISNULL(CLFILE.CL_PD_BR_TFR_FG,''),
                   CLFILE.CL_PD_O3INS_DLPROC,
                   CLFILE.CL_PD_INSPREM_DLADJ,
                   CLFILE.CL_PD_PLAMT,
                   CLFILE.CL_PD_MTD_PRIN,
                   CLFILE.CL_PD_MTD_LC,
                   CLFILE.CL_PD_MTD_INT,
                   CLFILE.CL_PD_MTD_OT,
                   CLFILE.CL_PD_MTD_EX,
                   CLFILE.CL_PD_BNKRPT_CHAPTER,
                   ISNULL(CLFILE.CL_PD_BMO_CON_STATUS,''),
                   ISNULL(CLFILE.CL_PD_BMO_REC_STATUS,''),
                   CLFILE.CL_PD_APP_NUMBER,
                   ISNULL(CLFILE.CL_PD_GIFTNET,''),
                   CLFILE.CL_PD_GIFTNET_OUR_PO,
                   ISNULL(CLFILE.CL_PD_PROC_DISBURSED,''),
                   CLFILE.CL_PD_NOODDPY,
                   CLFILE.CL_PD_CONPROFILE_1,
                   CLFILE.CL_PD_CONPROFILE_2,
                   CLFILE.CL_PD_CONPROFILE_3,
                   CLFILE.CL_PD_CONPROFILE_4,
                   CLFILE.CL_PD_CONPROFILE_5,
                   CLFILE.CL_PD_RECPROFILE_1,
                   CLFILE.CL_PD_RECPROFILE_2,
                   CLFILE.CL_PD_RECPROFILE_3,
                   CLFILE.CL_PD_RECPROFILE_4,
                   CLFILE.CL_PD_RECPROFILE_5,
                   ISNULL(CLFILE.CL_PD_TRW_STATUS,''),
                   ISNULL(CLFILE.CL_PD_ORIGIN,''),
                   ISNULL(CLFILE.CL_PD_PO_TRCD,''),
                   CLFILE.CL_PD_NEXIS_CRSCORE,
                   ISNULL(CLFILE.CL_PD_TRW_B_CONSUMER_IND,''),
                   ISNULL(CLFILE.CL_PD_PARVHELD_PAID_FG,''),
                   ISNULL(CLFILE.CL_PD_ACCT_FROZEN,''),
                   ISNULL(CLFILE.CL_PD_TRW_SPECIAL_CODE,''),
                   ISNULL(CLFILE.CL_PD_CREATION_STATUS,''),
                   CLFILE.CL_PD_TOTNOLCHG,
                   ISNULL(CLFILE.CL_PD_LBOX,''),
                   CLFILE.CL_PD_EXTRA_PRIN_LFP,
                   CLFILE.CL_PD_REMAIN_INT_SINCE_LFP,
                   CLFILE.CL_PD_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(CLFILE.CL_PD_INTERIM_COUPON,''),
                   CLFILE.CL_PD_DLCOMM_O3_CMSI,
                   CLFILE.CL_PD_DLCOMM_PP_CMSI,
                   ISNULL(CLFILE.CL_PD_TRW_J_CONSUMER_IND,''),
                   ISNULL(CLFILE.CL_PD_ACH,''),
                   ISNULL(CLFILE.CL_PD_1099_C_FG,''),
                   ISNULL(CLFILE.CL_PD_TRW_COMPLIANCE_CODE,''),
                   ISNULL(CLFILE.CL_PD_UNWIND_REASON_CODE,''),
                   ISNULL(CLFILE.CL_PD_PBI_PBD_FG,''),
                   ISNULL(CLFILE.CL_PD_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(CLFILE.CL_PD_APPROVED_BY_USER,''),
                   ISNULL(CLFILE.CL_PD_APPROVED_BY_CODE,''),
                   ISNULL(CAST(CLFILE.CL_PD_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_APPROVED_BY_TIME,
                   ISNULL(CAST(CLFILE.CL_PD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CLFILE.CL_PD_LTOUCH_TIME,
                   ISNULL(CLFILE.CL_PD_HOLD_COLLATERAL,''),
                   ISNULL(CLFILE.CL_PD_DLPAID,''),
                   CLFILE.CL_PD_CKNO,
                   ISNULL(CLFILE.CL_PD_FREE1,''),
                   ISNULL(CLFILE.CL_PD_FREE2,''),
                   ISNULL(CLFILE.CL_PD_FREE_SPACE_FILLER,''),
                   CLFILE.CL_PD_POSTING_BR,
                   ISNULL(CLFILE.CL_PD_OTHBR_FG,''),
                   CLFILE.CL_PD_APPROVAL_LIMIT,
                   ISNULL(CLFILE.CL_PD_APPROVAL_USERID,''),
                   CLFILE.CL_PD_MIN_CRED_SCORE,
                   ISNULL(CLFILE.CL_PD_CRED_SCORE_USERID,''),
                   CLFILE.CL_PD_FINAMT,
                   ISNULL(CLFILE.CL_PD_PROCEEDS_FG,''),
                   ISNULL(CLFILE.CL_PD_POINTS_FG,''),
                   ISNULL(CLFILE.CL_PD_OVRDRATE_FG,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_1,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_2,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_3,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_4,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_5,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_6,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_7,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_8,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_9,''),
                   ISNULL(CLFILE.CL_PD_INS_FG_10,''),
                   ISNULL(CLFILE.CL_PD_SERCHG_FG,''),
                   ISNULL(CLFILE.CL_PD_MAINTFEE_FG,''),
                   ISNULL(CLFILE.CL_PD_EXTCHG_FG,''),
                   ISNULL(CLFILE.CL_PD_REGPYAMT_FG,''),
                   ISNULL(CLFILE.CL_PD_FEECD_1,''),
                   ISNULL(CLFILE.CL_PD_FEECD_2,''),
                   ISNULL(CLFILE.CL_PD_FEECD_3,''),
                   ISNULL(CLFILE.CL_PD_FEECD_4,''),
                   ISNULL(CLFILE.CL_PD_FEECD_5,''),
                   ISNULL(CLFILE.CL_PD_FEECD_6,''),
                   ISNULL(CLFILE.CL_PD_FEECD_7,''),
                   ISNULL(CLFILE.CL_PD_FEECD_8,''),
                   ISNULL(CLFILE.CL_PD_FEECD_9,''),
                   ISNULL(CLFILE.CL_PD_FEECD_10,''),
                   ISNULL(CLFILE.CL_PD_FEECD_11,''),
                   ISNULL(CLFILE.CL_PD_FEECD_12,''),
                   ISNULL(CLFILE.CL_PD_FEECD_13,''),
                   ISNULL(CLFILE.CL_PD_FEECD_14,''),
                   ISNULL(CLFILE.CL_PD_FEECD_15,''),
                   ISNULL(CLFILE.CL_PD_FEECD_16,''),
                   ISNULL(CLFILE.CL_PD_FEECD_17,''),
                   ISNULL(CLFILE.CL_PD_FEECD_18,''),
                   ISNULL(CLFILE.CL_PD_FEECD_19,''),
                   ISNULL(CLFILE.CL_PD_FEECD_20,''),
                   CLFILE.CL_PD_PRPAID_AHEAD,
                   CLFILE.CL_PD_RESTRICT_ADJ,
                   CLFILE.CL_PD_DUAL_LOAN_ADJ,
                   CLFILE.CL_PD_CAN_LOAN_MAX,
                   CLFILE.CL_PD_PERDOWN,
                   ISNULL(CLFILE.CL_PD_SELLPR_FG,''),
                   ISNULL(CLFILE.CL_PD_DOWNPY_FG,''),
                   ISNULL(CLFILE.CL_PD_PERDOWN_FG,''),
                   ISNULL(CLFILE.CL_PD_SELL_DOWN_FG,''),
                   ISNULL(CLFILE.CL_PD_BALLOON_FG,''),
                   ISNULL(CLFILE.CL_PD_BALLOON_LAST_FG,''),
                   ISNULL(CLFILE.CL_PD_FEECD_FG_1,''),
                   ISNULL(CLFILE.CL_PD_FEECD_FG_2,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_1,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_1,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_1,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_2,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_2,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_2,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_3,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_3,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_3,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_4,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_4,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_4,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_5,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_5,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_5,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_6,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_6,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_6,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_7,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_7,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_7,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_8,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_8,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_8,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_9,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_9,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_9,''),
                   ISNULL(CLFILE.CL_PD_INS_TERM_SW_10,''),
                   ISNULL(CLFILE.CL_PD_COVERAGE_SW_10,''),
                   ISNULL(CLFILE.CL_PD_PREMIUM_SW_10,''),
                   ISNULL(CLFILE.CL_PD_DLDISC_FG,''),
                   ISNULL(CLFILE.CL_PD_DLRVPRIN_FG_1,''),
                   ISNULL(CLFILE.CL_PD_DLRVPRIN_FG_2,''),
                   ISNULL(CLFILE.CL_PD_DLRVPRIN_FG_3,''),
                   ISNULL(CLFILE.CL_PD_DLRVCHGS_FG_1,''),
                   ISNULL(CLFILE.CL_PD_DLRVCHGS_FG_2,''),
                   ISNULL(CLFILE.CL_PD_DLRVCHGS_FG_3,''),
                   ISNULL(CLFILE.CL_PD_DLPARVHELD_FG,''),
                   ISNULL(CLFILE.CL_PD_DLPARVPAID_FG,''),
                   ISNULL(CLFILE.CL_PD_ONE_MTH_NO_DAY_EXT_SW,''),
                   ISNULL(CLFILE.CL_PD_ASSIGN_RN_ACCTNO,''),
                   CLFILE.CL_PD_APR_FINAMT,
                   ISNULL(CLFILE.CL_PD_DLCOMM_PP_CMSI_FG,''),
                   ISNULL(CLFILE.CL_PD_DLCOMM_O3_CMSI_FG,''),
                   ISNULL(CLFILE.CL_PD_FILLER_TO_MATCH_LN,''),
                   ISNULL(CLFILE.CL_PD2_ID,''),
                   CLFILE.CL_PD2_BRANCH,
                   ISNULL(CLFILE.CL_PD2_BWNAME,'')
            INTO :QCL-REC
            FROM DBO.CLFILE
            WHERE CLFILE.CL_GROUP_ID = :QCL-GROUP-ID
              AND CLFILE.CL_DESC = :QCL-DESC
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CL2-FILE.

           PERFORM GET-CL-FIELDS.

      *-----------------------------------------------------------------
       READ-CL2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CL-SELECT-PATH TO E-FILE.
           MOVE SPACES TO E-KEYX.
           STRING CL-GROUP-ID,CL-CAN-NO DELIMITED BY "   "
                          INTO E-KEYX.
           INITIALIZE QCL-REC.

           IF ( CL2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CL2_CURSOR INTO :QCL-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CL2-FILE-NEXT.

           PERFORM GET-CL-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CL2-FILE.
           IF ( CL2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CL2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CL2-CURSOR-STAT.

      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################

      *=================================================================
      *=================================================================
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

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IOCLRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
