      * COPYMEMBER: LIBEX/EX01XTRX
      **************************************************************************
      * TRANSFER TO SERVICE TRANSACTION EXTRACTION FILE      ???? BYTES
      *
      * REGIONAL ACCEPTANCE, DATA WAREHOUSE EXTRACTION, REGACC PR# 1675
      *
      * REV:
      *     CS  040907 ADD XTRX-REGION-NUMBER [PR#76 REGACC]
      **************************************************************************
       01  XTRX-REC.
           03  XTRX-BRACCTNO             PIC 9(10).
           03  XTRX-BRNO                 PIC 9(04).
           03  XTRX-ACCTNO               PIC 9(06).
           03  XTRX-TRANS-DATE           PIC 9(08).
           03  XTRX-CODE                 PIC X(01).
           03  XTRX-CLASS                PIC 9(02).
           03  XTRX-LINE                 PIC 9(02).
           03  XTRX-LAWCODE              PIC 9(02).
           03  XTRX-SSNO1                PIC 9(09).
           03  XTRX-MAKERCD1             PIC X(01).
           03  XTRX-SSNO2                PIC 9(09).
           03  XTRX-MAKERCD2             PIC X(01).
           03  XTRX-SSNO3                PIC 9(09).
           03  XTRX-MAKERCD3             PIC X(01).
           03  XTRX-SSNO4                PIC 9(09).
           03  XTRX-MAKERCD4             PIC X(01).
           03  XTRX-PURCD                PIC X(03).
           03  XTRX-SRCD                 PIC X(02).
           03  XTRX-COLLCD               PIC X(02).
           03  XTRX-SERVACCT             PIC X(01).
           03  XTRX-LOANTYPE             PIC X(01).
           03  XTRX-LOANDATE             PIC 9(08).
           03  XTRX-PROCEEDS             PIC +9(07).99.
           03  XTRX-LNAMT                PIC +9(07).99.
           03  XTRX-FINCHG               PIC +9(07).99.
           03  XTRX-INTCHG               PIC 9(07).99.
           03  XTRX-SERCHG               PIC 9(05).99.
           03  XTRX-EXTCHG               PIC 9(05).99.
           03  XTRX-MAINTFEE             PIC 9(03).99.
           03  XTRX-ORGTERM              PIC 9(03).
           03  XTRX-FEE-TAB.
               05  XTRX-FEECODE1         PIC X(02).
               05  XTRX-FEEAMT1          PIC +9(05).99.
               05  XTRX-FEEOURS1         PIC X(01).
               05  XTRX-FEECODE2         PIC X(02).
               05  XTRX-FEEAMT2          PIC +9(05).99.
               05  XTRX-FEEOURS2         PIC X(01).
               05  XTRX-FEECODE3         PIC X(02).
               05  XTRX-FEEAMT3          PIC +9(05).99.
               05  XTRX-FEEOURS3         PIC X(01).
               05  XTRX-FEECODE4         PIC X(02).
               05  XTRX-FEEAMT4          PIC +9(05).99.
               05  XTRX-FEEOURS4         PIC X(01).
               05  XTRX-FEECODE5         PIC X(02).
               05  XTRX-FEEAMT5          PIC +9(05).99.
               05  XTRX-FEEOURS5         PIC X(01).
               05  XTRX-FEECODE6         PIC X(02).
               05  XTRX-FEEAMT6          PIC +9(05).99.
               05  XTRX-FEEOURS6         PIC X(01).
               05  XTRX-FEECODE7         PIC X(02).
               05  XTRX-FEEAMT7          PIC +9(05).99.
               05  XTRX-FEEOURS7         PIC X(01).
               05  XTRX-FEECODE8         PIC X(02).
               05  XTRX-FEEAMT8          PIC +9(05).99.
               05  XTRX-FEEOURS8         PIC X(01).
               05  XTRX-FEECODE9         PIC X(02).
               05  XTRX-FEEAMT9          PIC +9(05).99.
               05  XTRX-FEEOURS9         PIC X(01).
               05  XTRX-FEECODE10        PIC X(02).
               05  XTRX-FEEAMT10         PIC +9(05).99.
               05  XTRX-FEEOURS10        PIC X(01).
           03  XTRX-FEETBL REDEFINES XTRX-FEE-TAB OCCURS 10 TIMES.
               05  XTRX-FEECODE          PIC X(02).
               05  XTRX-FEEAMT           PIC +9(05).99.
               05  XTRX-FEEOURS          PIC X(01).
           03  XTRX-INSURED              PIC X(01).
           03  XTRX-INSTYPES             PIC X(01).
           03  XTRX-INSELIGIBLE          PIC X(01).   
           03  XTRX-INS-TAB.
               05  XTRX-INSCOMP1         PIC 9(02).
               05  XTRX-INSOURS1         PIC X(01).
               05  XTRX-INSEFF1          PIC 9(08).
               05  XTRX-INSEXP1          PIC 9(08).
               05  XTRX-INSPREM1         PIC +9(05).99.
               05  XTRX-INSCOMM1         PIC +9(06).99.
               05  XTRX-INSCOMP2         PIC 9(02).
               05  XTRX-INSOURS2         PIC X(01).
               05  XTRX-INSEFF2          PIC 9(08).
               05  XTRX-INSEXP2          PIC 9(08).
               05  XTRX-INSPREM2         PIC +9(05).99.
               05  XTRX-INSCOMM2         PIC +9(06).99.
               05  XTRX-INSCOMP3         PIC 9(02).
               05  XTRX-INSOURS3         PIC X(01).
               05  XTRX-INSEFF3          PIC 9(08).
               05  XTRX-INSEXP3          PIC 9(08).
               05  XTRX-INSPREM3         PIC +9(05).99.
               05  XTRX-INSCOMM3         PIC +9(06).99.
               05  XTRX-INSCOMP4         PIC 9(02).
               05  XTRX-INSOURS4         PIC X(01).
               05  XTRX-INSEFF4          PIC 9(08).
               05  XTRX-INSEXP4          PIC 9(08).
               05  XTRX-INSPREM4         PIC +9(05).99.
               05  XTRX-INSCOMM4         PIC +9(06).99.
               05  XTRX-INSCOMP5         PIC 9(02).
               05  XTRX-INSOURS5         PIC X(01).
               05  XTRX-INSEFF5          PIC 9(08).
               05  XTRX-INSEXP5          PIC 9(08).
               05  XTRX-INSPREM5         PIC +9(05).99.
               05  XTRX-INSCOMM5         PIC +9(06).99.
               05  XTRX-INSCOMP6         PIC 9(02).
               05  XTRX-INSOURS6         PIC X(01).
               05  XTRX-INSEFF6          PIC 9(08).
               05  XTRX-INSEXP6          PIC 9(08).
               05  XTRX-INSPREM6         PIC +9(05).99.
               05  XTRX-INSCOMM6         PIC +9(06).99.
               05  XTRX-INSCOMP7         PIC 9(02).
               05  XTRX-INSOURS7         PIC X(01).
               05  XTRX-INSEFF7          PIC 9(08).
               05  XTRX-INSEXP7          PIC 9(08).
               05  XTRX-INSPREM7         PIC +9(05).99.
               05  XTRX-INSCOMM7         PIC +9(06).99.
               05  XTRX-INSCOMP8         PIC 9(02).
               05  XTRX-INSOURS8         PIC X(01).
               05  XTRX-INSEFF8          PIC 9(08).
               05  XTRX-INSEXP8          PIC 9(08).
               05  XTRX-INSPREM8         PIC +9(05).99.
               05  XTRX-INSCOMM8         PIC +9(06).99.
               05  XTRX-INSCOMP9         PIC 9(02).
               05  XTRX-INSOURS9         PIC X(01).
               05  XTRX-INSEFF9          PIC 9(08).
               05  XTRX-INSEXP9          PIC 9(08).
               05  XTRX-INSPREM9         PIC +9(05).99.
               05  XTRX-INSCOMM9         PIC +9(06).99.
               05  XTRX-INSCOMP10        PIC 9(02).
               05  XTRX-INSOURS10        PIC X(01).
               05  XTRX-INSEFF10         PIC 9(08).
               05  XTRX-INSEXP10         PIC 9(08).
               05  XTRX-INSPREM10        PIC +9(05).99.
               05  XTRX-INSCOMM10        PIC +9(06).99.
           03  XTRX-INSTBL REDEFINES XTRX-INS-TAB OCCURS 10 TIMES.
               05  XTRX-INSCOMP          PIC 9(02).
               05  XTRX-INSOURS          PIC X(01).
               05  XTRX-INSEFF           PIC 9(08).
               05  XTRX-INSEXP           PIC 9(08).
               05  XTRX-INSPREM          PIC +9(05).99.
               05  XTRX-INSCOMM          PIC +9(06).99.
           03  XTRX-DLNO                 PIC 9(4).99.
           03  XTRX-DLRECOURSE           PIC X(01).
           03  XTRX-DLPARTRCAMT          PIC 9(06).99.
           03  XTRX-PURBAL               PIC +9(07).99.
           03  XTRX-DLRVPRIN1            PIC +9(07).99.
           03  XTRX-DLRVPRIN2            PIC +9(07).99.
           03  XTRX-DLRVPRIN3            PIC +9(07).99.
           03  XTRX-DLRVCHGS1            PIC +9(06).99.
           03  XTRX-DLRVCHGS2            PIC +9(06).99.
           03  XTRX-DLRVCHGS3            PIC +9(06).99.
           03  XTRX-DLPARVHELD           PIC +9(07).99.
           03  XTRX-DLPARVPAID           PIC +9(06).99.
           03  XTRX-DLDISC               PIC +9(07).99.
           03  XTRX-ANTICTERM            PIC 9(03).
           03  XTRX-ANTICERN-TAB.
               05  XTRX-ANTICERN1            PIC +9(07).99.
               05  XTRX-ANTICERN2            PIC +9(07).99.
               05  XTRX-ANTICERN3            PIC +9(07).99.
               05  XTRX-ANTICERN4            PIC +9(07).99.
               05  XTRX-ANTICERN5            PIC +9(07).99.
               05  XTRX-ANTICERN6            PIC +9(07).99.
           03  XTRX-ANTICERN REDEFINES XTRX-ANTICERN-TAB OCCURS 6 TIMES
                                             PIC +9(07).99.
           03  XTRX-EARNINGS-TAB.
               05  XTRX-EARNINGS1            PIC +9(07).99.
               05  XTRX-EARNINGS2            PIC +9(07).99.
               05  XTRX-EARNINGS3            PIC +9(07).99.
               05  XTRX-EARNINGS4            PIC +9(07).99.
               05  XTRX-EARNINGS5            PIC +9(07).99.
               05  XTRX-EARNINGS6            PIC +9(07).99.
           03  XTRX-EARNINGS REDEFINES XTRX-EARNINGS-TAB OCCURS 6 TIMES
                                             PIC +9(07).99.
           03  XTRX-PRLNNO                   PIC 9(08).
           03  XTRX-PRBAL                    PIC +9(07).99.
           03  XTRX-TFRBAL                   PIC +9(07).99.
           03  XTRX-SPRCLASS                 PIC 9(02).
           03  XTRX-USERID                   PIC X(10).
           03  XTRX-POSTDATE                 PIC 9(08).
           03  XTRX-POSTTIME                 PIC 9(06).
           03  XTRX-REGION-NUMBER            PIC 9(2).
           03  FILLER                        PIC X(98).
           03  XTRX-CR                       PIC X(01).
           03  XTRX-LF                       PIC X(01).

