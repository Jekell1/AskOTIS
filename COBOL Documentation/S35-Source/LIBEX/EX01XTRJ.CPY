      * COPYMEMBER: LIBEX/EX01XTRJ
      **************************************************************************
      * CLASS TRANSFER TRANSACTION EXTRACTION FILE      ???? BYTES
      *
      * REGIONAL ACCEPTANCE, DATA WAREHOUSE EXTRACTION, REGACC PR# 1675
      *
      * REV:
      *     040907  ADD XTRJ-REGION-NUMBER [PR#76 REGACC]
      **************************************************************************
       01  XTRJ-REC.
           03  XTRJ-BRACCTNO             PIC 9(10).
           03  XTRJ-BRNO                 PIC 9(04).
           03  XTRJ-ACCTNO               PIC 9(06).
           03  XTRJ-TRANS-DATE           PIC 9(08).
           03  XTRJ-CODE                 PIC X(01).
           03  XTRJ-CLASS                PIC 9(02).
           03  XTRJ-LINE                 PIC 9(02).
           03  XTRJ-LAWCODE              PIC 9(02).
           03  XTRJ-SSNO1                PIC 9(09).
           03  XTRJ-MAKERCD1             PIC X(01).
           03  XTRJ-SSNO2                PIC 9(09).
           03  XTRJ-MAKERCD2             PIC X(01).
           03  XTRJ-SSNO3                PIC 9(09).
           03  XTRJ-MAKERCD3             PIC X(01).
           03  XTRJ-SSNO4                PIC 9(09).
           03  XTRJ-MAKERCD4             PIC X(01).
           03  XTRJ-REF-FRLC             PIC 9(02).
           03  XTRJ-REF-X                PIC X(01).
           03  XTRJ-REF-TOLC             PIC 9(02).
           03  XTRJ-PURCD                PIC X(03).
           03  XTRJ-SRCD                 PIC X(02).
           03  XTRJ-COLLCD               PIC X(02).
           03  XTRJ-SERVACCT             PIC X(01).
           03  XTRJ-LOANTYPE             PIC X(01).
           03  XTRJ-LOANDATE             PIC 9(08).
           03  XTRJ-PROCEEDS             PIC +9(07).99.
           03  XTRJ-LNAMT                PIC +9(07).99.
           03  XTRJ-FINCHG               PIC +9(07).99.
           03  XTRJ-INTCHG               PIC 9(07).99.
           03  XTRJ-SERCHG               PIC 9(05).99.
           03  XTRJ-EXTCHG               PIC 9(05).99.
           03  XTRJ-MAINTFEE             PIC 9(03).99.
           03  XTRJ-ORGTERM              PIC 9(03).
           03  XTRJ-FEE-TAB.
               05  XTRJ-FEECODE1         PIC X(02).
               05  XTRJ-FEEAMT1          PIC +9(05).99.
               05  XTRJ-FEEOURS1         PIC X(01).
               05  XTRJ-FEECODE2         PIC X(02).
               05  XTRJ-FEEAMT2          PIC +9(05).99.
               05  XTRJ-FEEOURS2         PIC X(01).
               05  XTRJ-FEECODE3         PIC X(02).
               05  XTRJ-FEEAMT3          PIC +9(05).99.
               05  XTRJ-FEEOURS3         PIC X(01).
               05  XTRJ-FEECODE4         PIC X(02).
               05  XTRJ-FEEAMT4          PIC +9(05).99.
               05  XTRJ-FEEOURS4         PIC X(01).
               05  XTRJ-FEECODE5         PIC X(02).
               05  XTRJ-FEEAMT5          PIC +9(05).99.
               05  XTRJ-FEEOURS5         PIC X(01).
               05  XTRJ-FEECODE6         PIC X(02).
               05  XTRJ-FEEAMT6          PIC +9(05).99.
               05  XTRJ-FEEOURS6         PIC X(01).
           03  XTRJ-FEETBL REDEFINES XTRJ-FEE-TAB OCCURS 6 TIMES.
               05  XTRJ-FEECODE          PIC X(02).
               05  XTRJ-FEEAMT           PIC +9(05).99.
               05  XTRJ-FEEOURS          PIC X(01).
           03  XTRJ-INSURED              PIC X(01).
           03  XTRJ-INSTYPES             PIC X(01).
           03  XTRJ-INSELIGIBLE          PIC X(01).   
           03  XTRJ-INS-TAB.
               05  XTRJ-INSCOMP1         PIC 9(02).
               05  XTRJ-INSOURS1         PIC X(01).
               05  XTRJ-INSEFF1          PIC 9(08).
               05  XTRJ-INSEXP1          PIC 9(08).
               05  XTRJ-INSPREM1         PIC +9(05).99.
               05  XTRJ-INSCOMM1         PIC +9(06).99.
               05  XTRJ-INSCOMP2         PIC 9(02).
               05  XTRJ-INSOURS2         PIC X(01).
               05  XTRJ-INSEFF2          PIC 9(08).
               05  XTRJ-INSEXP2          PIC 9(08).
               05  XTRJ-INSPREM2         PIC +9(05).99.
               05  XTRJ-INSCOMM2         PIC +9(06).99.
               05  XTRJ-INSCOMP3         PIC 9(02).
               05  XTRJ-INSOURS3         PIC X(01).
               05  XTRJ-INSEFF3          PIC 9(08).
               05  XTRJ-INSEXP3          PIC 9(08).
               05  XTRJ-INSPREM3         PIC +9(05).99.
               05  XTRJ-INSCOMM3         PIC +9(06).99.
               05  XTRJ-INSCOMP4         PIC 9(02).
               05  XTRJ-INSOURS4         PIC X(01).
               05  XTRJ-INSEFF4          PIC 9(08).
               05  XTRJ-INSEXP4          PIC 9(08).
               05  XTRJ-INSPREM4         PIC +9(05).99.
               05  XTRJ-INSCOMM4         PIC +9(06).99.
               05  XTRJ-INSCOMP5         PIC 9(02).
               05  XTRJ-INSOURS5         PIC X(01).
               05  XTRJ-INSEFF5          PIC 9(08).
               05  XTRJ-INSEXP5          PIC 9(08).
               05  XTRJ-INSPREM5         PIC +9(05).99.
               05  XTRJ-INSCOMM5         PIC +9(06).99.
           03  XTRJ-INSTBL REDEFINES XTRJ-INS-TAB OCCURS 5 TIMES.
               05  XTRJ-INSCOMP          PIC 9(02).
               05  XTRJ-INSOURS          PIC X(01).
               05  XTRJ-INSEFF           PIC 9(08).
               05  XTRJ-INSEXP           PIC 9(08).
               05  XTRJ-INSPREM          PIC +9(05).99.
               05  XTRJ-INSCOMM          PIC +9(06).99.
           03  XTRJ-DLNO                 PIC 9(4).99.
           03  XTRJ-DLRV                 PIC +9(07).99.
           03  XTRJ-DLSPRV               PIC +9(07).99.
           03  XTRJ-DLDISC               PIC +9(07).99.
           03  XTRJ-DLPARV               PIC +9(07).99.
           03  XTRJ-DLRECOURSE           PIC X(01).
           03  XTRJ-DLPARTRCAMT          PIC 9(06).99.
           03  XTRJ-PURBAL               PIC +9(07).99.

           03  XTRJ-UNEARNED-TAB.
               05  XTRJ-UNEARN1              PIC +9(07).99.
               05  XTRJ-UNEARN2              PIC +9(07).99.
               05  XTRJ-UNEARN3              PIC +9(07).99.
               05  XTRJ-UNEARN4              PIC +9(07).99.
               05  XTRJ-UNEARN5              PIC +9(07).99.
               05  XTRJ-UNEARN6              PIC +9(07).99.
           03  XTRJ-UNEARN REDEFINES XTRJ-UNEARNED-TAB OCCURS 6 TIMES
                                             PIC +9(07).99.
           03  XTRJ-PRLNNO                   PIC 9(08).
           03  XTRJ-DLRVPRIN1                PIC +9(08).99.
           03  XTRJ-DLRVPRIN2                PIC +9(08).99.
           03  XTRJ-DLRVPRIN3                PIC +9(08).99.
           03  XTRJ-DLRVCHGS1                PIC +9(08).99.
           03  XTRJ-DLRVCHGS2                PIC +9(08).99.
           03  XTRJ-DLRVCHGS3                PIC +9(08).99.
           03  XTRJ-DLPARVHELD               PIC +9(08).99.
           03  XTRJ-DLPARVPAID               PIC +9(08).99.
           03  XTRJ-USERID                   PIC X(10).
           03  XTRJ-POSTDATE                 PIC 9(08).
           03  XTRJ-POSTTIME                 PIC 9(06).
           03  XTRJ-PLCD                     PIC X(01).
           03  XTRJ-PLTYPE                   PIC X(01).
           03  XTRJ-REPOCD                   PIC X(01).
           03  XTRJ-REGION-NUMBER            PIC 9(2).
           03  FILLER                        PIC X(98).
           03  XTRJ-CR                       PIC X(01).
           03  XTRJ-LF                       PIC X(01).

