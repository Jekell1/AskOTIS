      * COPYMEMBER: LIBEX/EX01XTRB
      **************************************************************************
      * BULK TRANSFER TRANSACTION EXTRACTION FILE      ???? BYTES
      *
      * REGIONAL ACCEPTANCE, DATA WAREHOUSE EXTRACTION, REGACC PR# 1675
      *
      * REV:
      *    CS  040907 ADD XTRB-REGION-NUMBER [PR#76 REGACC]
      *   BAH  20220421 ADD XTRB-ACTIONCD2, SECURITIZATION #1551
      **************************************************************************
       01  XTRB-REC.
           03  XTRB-BRACCTNO             PIC 9(10).
           03  XTRB-BRNO                 PIC 9(04).
           03  XTRB-ACCTNO               PIC 9(06).
           03  XTRB-TRANS-DATE           PIC 9(08).
           03  XTRB-CODE                 PIC X(01).
           03  XTRB-CLASS                PIC 9(02).
           03  XTRB-LINE                 PIC 9(02).
           03  XTRB-LAWCODE              PIC 9(02).
           03  XTRB-SSNO1                PIC 9(09).
           03  XTRB-MAKERCD1             PIC X(01).
           03  XTRB-SSNO2                PIC 9(09).
           03  XTRB-MAKERCD2             PIC X(01).
           03  XTRB-SSNO3                PIC 9(09).
           03  XTRB-MAKERCD3             PIC X(01).
           03  XTRB-SSNO4                PIC 9(09).
           03  XTRB-MAKERCD4             PIC X(01).
           03  XTRB-REF-FRTO             PIC X(01).
           03  XTRB-REFBR                PIC 9(04).
           03  XTRB-PURCD                PIC X(03).
           03  XTRB-SRCD                 PIC X(02).
           03  XTRB-COLLCD               PIC X(02).
           03  XTRB-SERVACCT             PIC X(01).
           03  XTRB-LOANTYPE             PIC X(01).
           03  XTRB-LOANDATE             PIC 9(08).
           03  XTRB-PROCEEDS             PIC +9(07).99.
           03  XTRB-LNAMT                PIC +9(07).99.
           03  XTRB-FINCHG               PIC +9(07).99.
           03  XTRB-INTCHG               PIC +9(07).99.
           03  XTRB-SERCHG               PIC 9(05).99.
           03  XTRB-EXTCHG               PIC 9(05).99.
           03  XTRB-MAINTFEE             PIC 9(03).99.
           03  XTRB-ORGTERM              PIC 9(03).
           03  XTRB-FEE-TAB.
               05  XTRB-FEECODE1         PIC X(02).
               05  XTRB-FEEAMT1          PIC +9(05).99.
               05  XTRB-FEEOURS1         PIC X(01).
               05  XTRB-FEECODE2         PIC X(02).
               05  XTRB-FEEAMT2          PIC +9(05).99.
               05  XTRB-FEEOURS2         PIC X(01).
               05  XTRB-FEECODE3         PIC X(02).
               05  XTRB-FEEAMT3          PIC +9(05).99.
               05  XTRB-FEEOURS3         PIC X(01).
               05  XTRB-FEECODE4         PIC X(02).
               05  XTRB-FEEAMT4          PIC +9(05).99.
               05  XTRB-FEEOURS4         PIC X(01).
               05  XTRB-FEECODE5         PIC X(02).
               05  XTRB-FEEAMT5          PIC +9(05).99.
               05  XTRB-FEEOURS5         PIC X(01).
               05  XTRB-FEECODE6         PIC X(02).
               05  XTRB-FEEAMT6          PIC +9(05).99.
               05  XTRB-FEEOURS6         PIC X(01).
           03  XTRB-FEETBL REDEFINES XTRB-FEE-TAB OCCURS 6 TIMES.
               05  XTRB-FEECODE          PIC X(02).
               05  XTRB-FEEAMT           PIC +9(05).99.
               05  XTRB-FEEOURS          PIC X(01).
           03  XTRB-INSURED              PIC X(01).
           03  XTRB-INSTYPES             PIC X(01).
           03  XTRB-INSELIGIBLE          PIC X(01).   
           03  XTRB-INS-TAB.
               05  XTRB-INSCOMP1         PIC 9(02).
               05  XTRB-INSOURS1         PIC X(01).
               05  XTRB-INSEFF1          PIC 9(08).
               05  XTRB-INSEXP1          PIC 9(08).
               05  XTRB-INSPREM1         PIC +9(05).99.
               05  XTRB-INSCOMM1         PIC +9(06).99.
               05  XTRB-INSCOMP2         PIC 9(02).
               05  XTRB-INSOURS2         PIC X(01).
               05  XTRB-INSEFF2          PIC 9(08).
               05  XTRB-INSEXP2          PIC 9(08).
               05  XTRB-INSPREM2         PIC +9(05).99.
               05  XTRB-INSCOMM2         PIC +9(06).99.
               05  XTRB-INSCOMP3         PIC 9(02).
               05  XTRB-INSOURS3         PIC X(01).
               05  XTRB-INSEFF3          PIC 9(08).
               05  XTRB-INSEXP3          PIC 9(08).
               05  XTRB-INSPREM3         PIC +9(05).99.
               05  XTRB-INSCOMM3         PIC +9(06).99.
               05  XTRB-INSCOMP4         PIC 9(02).
               05  XTRB-INSOURS4         PIC X(01).
               05  XTRB-INSEFF4          PIC 9(08).
               05  XTRB-INSEXP4          PIC 9(08).
               05  XTRB-INSPREM4         PIC +9(05).99.
               05  XTRB-INSCOMM4         PIC +9(06).99.
               05  XTRB-INSCOMP5         PIC 9(02).
               05  XTRB-INSOURS5         PIC X(01).
               05  XTRB-INSEFF5          PIC 9(08).
               05  XTRB-INSEXP5          PIC 9(08).
               05  XTRB-INSPREM5         PIC +9(05).99.
               05  XTRB-INSCOMM5         PIC +9(06).99.
           03  XTRB-INSTBL REDEFINES XTRB-INS-TAB OCCURS 5 TIMES.
               05  XTRB-INSCOMP          PIC 9(02).
               05  XTRB-INSOURS          PIC X(01).
               05  XTRB-INSEFF           PIC 9(08).
               05  XTRB-INSEXP           PIC 9(08).
               05  XTRB-INSPREM          PIC +9(05).99.
               05  XTRB-INSCOMM          PIC +9(06).99.
           03  XTRB-DLNO                 PIC 9(4).99.
           03  XTRB-DLRV                 PIC +9(07).99.
           03  XTRB-DLSPRV               PIC +9(07).99.
           03  XTRB-DLDISC               PIC +9(07).99.
           03  XTRB-DLPARV               PIC +9(07).99.
           03  XTRB-DLRECOURSE           PIC X(01).
           03  XTRB-DLPARTRCAMT          PIC 9(06).99.
           03  XTRB-PURBAL               PIC +9(07).99.

           03  XTRB-UNEARNED-TAB.
               05  XTRB-UNEARN1              PIC +9(07).99.
               05  XTRB-UNEARN2              PIC +9(07).99.
               05  XTRB-UNEARN3              PIC +9(07).99.
               05  XTRB-UNEARN4              PIC +9(07).99.
               05  XTRB-UNEARN5              PIC +9(07).99.
               05  XTRB-UNEARN6              PIC +9(07).99.
           03  XTRB-UNEARN REDEFINES XTRB-UNEARNED-TAB OCCURS 6 TIMES
                                             PIC +9(07).99.
           03  XTRB-REF-ACCTNO               PIC 9(08).
           03  XTRB-DLRVPRIN1                PIC +9(08).99.
           03  XTRB-DLRVPRIN2                PIC +9(08).99.
           03  XTRB-DLRVPRIN3                PIC +9(08).99.
           03  XTRB-DLRVCHGS1                PIC +9(08).99.
           03  XTRB-DLRVCHGS2                PIC +9(08).99.
           03  XTRB-DLRVCHGS3                PIC +9(08).99.
           03  XTRB-DLPARVHELD               PIC +9(08).99.
           03  XTRB-DLPARVPAID               PIC +9(08).99.
           03  XTRB-USERID                   PIC X(10).
           03  XTRB-POSTDATE                 PIC 9(08).
           03  XTRB-POSTTIME                 PIC 9(06).
           03  XTRB-PLCD                     PIC X(01).
           03  XTRB-PLTYPE                   PIC X(01).
           03  XTRB-REPOCD                   PIC X(01).
           03  XTRB-REGION-NUMBER            PIC 9(2).
           03  XTRB-ACTIONCD2                PIC X(4).
           03  FILLER                        PIC X(94).
           03  XTRB-CR                       PIC X(01).
           03  XTRB-LF                       PIC X(01).
