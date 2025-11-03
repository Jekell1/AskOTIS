      * COPYMEMBER: LIBEX/EX01XTRK
      **************************************************************************
      * CONVERSION TRANSACTION EXTRACTION FILE      ???? BYTES
      *
      * REGIONAL ACCEPTANCE, DATA WAREHOUSE EXTRACTION, REGACC PR# 1675
      *
      * REV:
      *     CS  040907 ADD XTRK-REGION-NUMBER [PR#76 REGACC]
      **************************************************************************
       01  XTRK-REC.
           03  XTRK-BRACCTNO             PIC 9(10).
           03  XTRK-BRNO                 PIC 9(04).
           03  XTRK-ACCTNO               PIC 9(06).
           03  XTRK-TRANS-DATE           PIC 9(08).
           03  XTRK-CODE                 PIC X(01).
           03  XTRK-CLASS                PIC 9(02).
           03  XTRK-LINE                 PIC 9(02).
           03  XTRK-LAWCODE              PIC 9(02).
           03  XTRK-SSNO1                PIC 9(09).
           03  XTRK-MAKERCD1             PIC X(01).
           03  XTRK-SSNO2                PIC 9(09).
           03  XTRK-MAKERCD2             PIC X(01).
           03  XTRK-SSNO3                PIC 9(09).
           03  XTRK-MAKERCD3             PIC X(01).
           03  XTRK-SSNO4                PIC 9(09).
           03  XTRK-MAKERCD4             PIC X(01).
           03  XTRK-REF-FRTO             PIC X(01).
           03  XTRK-REFBR                PIC 9(04).
           03  XTRK-PURCD                PIC X(03).
           03  XTRK-SRCD                 PIC X(02).
           03  XTRK-COLLCD               PIC X(02).
           03  XTRK-SERVACCT             PIC X(01).
           03  XTRK-LOANTYPE             PIC X(01).
           03  XTRK-REPOCD               PIC X(01).
           03  XTRK-PLCD                 PIC X(01).
           03  XTRK-PLTYPE               PIC X(01).
           03  XTRK-LOANDATE             PIC 9(08).
           03  XTRK-PROCEEDS             PIC +9(07).99.
           03  XTRK-LNAMT                PIC +9(07).99.
           03  XTRK-FINCHG               PIC +9(07).99.
           03  XTRK-INTCHG               PIC 9(07).99.
           03  XTRK-SERCHG               PIC 9(05).99.
           03  XTRK-EXTCHG               PIC 9(05).99.
           03  XTRK-MAINTFEE             PIC 9(03).99.
           03  XTRK-ORGTERM              PIC 9(03).
           03  XTRK-FEE-TAB.
               05  XTRK-FEECODE1         PIC X(02).
               05  XTRK-FEEAMT1          PIC +9(05).99.
               05  XTRK-FEEOURS1         PIC X(01).
               05  XTRK-FEECODE2         PIC X(02).
               05  XTRK-FEEAMT2          PIC +9(05).99.
               05  XTRK-FEEOURS2         PIC X(01).
               05  XTRK-FEECODE3         PIC X(02).
               05  XTRK-FEEAMT3          PIC +9(05).99.
               05  XTRK-FEEOURS3         PIC X(01).
               05  XTRK-FEECODE4         PIC X(02).
               05  XTRK-FEEAMT4          PIC +9(05).99.
               05  XTRK-FEEOURS4         PIC X(01).
               05  XTRK-FEECODE5         PIC X(02).
               05  XTRK-FEEAMT5          PIC +9(05).99.
               05  XTRK-FEEOURS5         PIC X(01).
               05  XTRK-FEECODE6         PIC X(02).
               05  XTRK-FEEAMT6          PIC +9(05).99.
               05  XTRK-FEEOURS6         PIC X(01).
           03  XTRK-FEETBL REDEFINES XTRK-FEE-TAB OCCURS 6 TIMES.
               05  XTRK-FEECODE          PIC X(02).
               05  XTRK-FEEAMT           PIC +9(05).99.
               05  XTRK-FEEOURS          PIC X(01).
           03  XTRK-INSURED              PIC X(01).
           03  XTRK-INSTYPES             PIC X(01).
           03  XTRK-INSELIGIBLE          PIC X(01).   
           03  XTRK-INS-TAB.
               05  XTRK-INSCOMP1         PIC 9(02).
               05  XTRK-INSOURS1         PIC X(01).
               05  XTRK-INSEFF1          PIC 9(08).
               05  XTRK-INSEXP1          PIC 9(08).
               05  XTRK-INSPREM1         PIC +9(05).99.
               05  XTRK-INSCOMM1         PIC +9(06).99.
               05  XTRK-INSCOMP2         PIC 9(02).
               05  XTRK-INSOURS2         PIC X(01).
               05  XTRK-INSEFF2          PIC 9(08).
               05  XTRK-INSEXP2          PIC 9(08).
               05  XTRK-INSPREM2         PIC +9(05).99.
               05  XTRK-INSCOMM2         PIC +9(06).99.
               05  XTRK-INSCOMP3         PIC 9(02).
               05  XTRK-INSOURS3         PIC X(01).
               05  XTRK-INSEFF3          PIC 9(08).
               05  XTRK-INSEXP3          PIC 9(08).
               05  XTRK-INSPREM3         PIC +9(05).99.
               05  XTRK-INSCOMM3         PIC +9(06).99.
               05  XTRK-INSCOMP4         PIC 9(02).
               05  XTRK-INSOURS4         PIC X(01).
               05  XTRK-INSEFF4          PIC 9(08).
               05  XTRK-INSEXP4          PIC 9(08).
               05  XTRK-INSPREM4         PIC +9(05).99.
               05  XTRK-INSCOMM4         PIC +9(06).99.
               05  XTRK-INSCOMP5         PIC 9(02).
               05  XTRK-INSOURS5         PIC X(01).
               05  XTRK-INSEFF5          PIC 9(08).
               05  XTRK-INSEXP5          PIC 9(08).
               05  XTRK-INSPREM5         PIC +9(05).99.
               05  XTRK-INSCOMM5         PIC +9(06).99.
           03  XTRK-INSTBL REDEFINES XTRK-INS-TAB OCCURS 5 TIMES.
               05  XTRK-INSCOMP          PIC 9(02).
               05  XTRK-INSOURS          PIC X(01).
               05  XTRK-INSEFF           PIC 9(08).
               05  XTRK-INSEXP           PIC 9(08).
               05  XTRK-INSPREM          PIC +9(05).99.
               05  XTRK-INSCOMM          PIC +9(06).99.
           03  XTRK-DLNO                 PIC 9(4).99.
           03  XTRK-DLRV                 PIC +9(07).99.
           03  XTRK-DLSPRV               PIC +9(07).99.
           03  XTRK-DLDISC               PIC +9(07).99.
           03  XTRK-DLPARV               PIC +9(07).99.
           03  XTRK-DLRECOURSE           PIC X(01).
           03  XTRK-DLPARTRCAMT          PIC 9(06).99.
           03  XTRK-PURBAL               PIC +9(07).99.

           03  XTRK-UNEARNED-TAB.
               05  XTRK-UNEARN1              PIC +9(07).99.
               05  XTRK-UNEARN2              PIC +9(07).99.
               05  XTRK-UNEARN3              PIC +9(07).99.
               05  XTRK-UNEARN4              PIC +9(07).99.
               05  XTRK-UNEARN5              PIC +9(07).99.
               05  XTRK-UNEARN6              PIC +9(07).99.
           03  XTRK-UNEARN REDEFINES XTRK-UNEARNED-TAB OCCURS 6 TIMES
                                             PIC +9(07).99.
           03  XTRK-PRLNNO                   PIC 9(08).
           03  XTRK-DLRVPRIN1                PIC +9(08).99.
           03  XTRK-DLRVPRIN2                PIC +9(08).99.
           03  XTRK-DLRVPRIN3                PIC +9(08).99.
           03  XTRK-DLRVCHGS1                PIC +9(08).99.
           03  XTRK-DLRVCHGS2                PIC +9(08).99.
           03  XTRK-DLRVCHGS3                PIC +9(08).99.
           03  XTRK-DLPARVPAID               PIC +9(08).99.
           03  XTRK-DLPARVHELD               PIC +9(08).99.
           03  XTRK-USERID                   PIC X(10).
           03  XTRK-POSTDATE                 PIC 9(08).
           03  XTRK-POSTTIME                 PIC 9(06).
           03  XTRK-REGION-NUMBER            PIC 9(2).
           03  FILLER                        PIC X(98).
           03  XTRK-CR                       PIC X(01).
           03  XTRK-LF                       PIC X(01).

