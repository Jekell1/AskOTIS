      * COPYMEMBER: LIBEX/EX01XTRS
      **************************************************************************
      * SALES FINANCE TRANSACTION EXTRACTION FILE      ???? BYTES
      *
      * REGIONAL ACCEPTANCE, DATA WAREHOUSE EXTRACTION, REGACC PR# 1675
      *
      * REV:
      * BLV 021016 ADD CSMI O3 & PP COMMISSION FIELDS.....IN THE TRFILE JOHN
      *            TRASHED THE LAST 2 OCCURANCES OF THE INSURANCE TABLE TO ADD
      *            THESE; KEVIN TOLD ME TO LEAVE INSURANCE OCCURANCES 9 & 10
      *            IN THIS FILE AS 0 & JUST ADD THE NEW FIELDS TO THE END
      * BLV 021017 ADD MIP-FLAG AND DLPAID
      *  CS 040907 ADD XTRS-REGION-NUMBER [PR#76 REGACC]
      * BAH 20220421 ADD XTRS-ACTIONCD2, SECURITIZATION #1551
      **************************************************************************
       01  XTRS-REC.
           03  XTRS-BRACCTNO             PIC 9(10).
           03  XTRS-BRNO                 PIC 9(04).
           03  XTRS-ACCTNO               PIC 9(06).
           03  XTRS-TRANS-DATE           PIC 9(08).
           03  XTRS-CODE                 PIC X(01).
           03  XTRS-CLASS                PIC 9(02).
           03  XTRS-LINE                 PIC 9(02).
           03  XTRS-LAWCODE              PIC 9(02).
           03  XTRS-SSNO1                PIC 9(09).
           03  XTRS-MAKERCD1             PIC X(01).
           03  XTRS-SSNO2                PIC 9(09).
           03  XTRS-MAKERCD2             PIC X(01).
           03  XTRS-SSNO3                PIC 9(09).
           03  XTRS-MAKERCD3             PIC X(01).
           03  XTRS-SSNO4                PIC 9(09).
           03  XTRS-MAKERCD4             PIC X(01).
           03  XTRS-PURCD                PIC X(03).
           03  XTRS-SRCD                 PIC X(02).
           03  XTRS-COLLCD               PIC X(02).
           03  XTRS-SERVACCT             PIC X(01).
           03  XTRS-LOANTYPE             PIC X(01).
           03  XTRS-LOANDATE             PIC 9(08).
           03  XTRS-INTDATE              PIC 9(08).
           03  XTRS-PROCEEDS             PIC +9(07).99.
           03  XTRS-LNAMT                PIC +9(07).99.
           03  XTRS-FINCHG               PIC +9(07).99.
           03  XTRS-INTCHG               PIC +9(07).99.
           03  XTRS-SERCHG               PIC +9(05).99.
           03  XTRS-EXTCHG               PIC +9(05).99.
           03  XTRS-MAINTFEE             PIC +9(03).99.
           03  XTRS-ORGTERM              PIC 9(03).
           03  XTRS-1STPYAMT             PIC +9(07).99.
           03  XTRS-NOREGPY              PIC 9(03).
           03  XTRS-REGPYAMT             PIC +9(07).99.
           03  XTRS-LASTPYAMT            PIC +9(07).99.
           03  XTRS-FEE-TAB.
               05  XTRS-FEECODE1         PIC X(02).
               05  XTRS-FEEAMT1          PIC +9(05).99.
               05  XTRS-FEEOURS1         PIC X(01).
               05  XTRS-FEECODE2         PIC X(02).
               05  XTRS-FEEAMT2          PIC +9(05).99.
               05  XTRS-FEEOURS2         PIC X(01).
               05  XTRS-FEECODE3         PIC X(02).
               05  XTRS-FEEAMT3          PIC +9(05).99.
               05  XTRS-FEEOURS3         PIC X(01).
               05  XTRS-FEECODE4         PIC X(02).
               05  XTRS-FEEAMT4          PIC +9(05).99.
               05  XTRS-FEEOURS4         PIC X(01).
               05  XTRS-FEECODE5         PIC X(02).
               05  XTRS-FEEAMT5          PIC +9(05).99.
               05  XTRS-FEEOURS5         PIC X(01).
               05  XTRS-FEECODE6         PIC X(02).
               05  XTRS-FEEAMT6          PIC +9(05).99.
               05  XTRS-FEEOURS6         PIC X(01).
               05  XTRS-FEECODE7         PIC X(02).
               05  XTRS-FEEAMT7          PIC +9(05).99.
               05  XTRS-FEEOURS7         PIC X(01).
               05  XTRS-FEECODE8         PIC X(02).
               05  XTRS-FEEAMT8          PIC +9(05).99.
               05  XTRS-FEEOURS8         PIC X(01).
               05  XTRS-FEECODE9         PIC X(02).
               05  XTRS-FEEAMT9          PIC +9(05).99.
               05  XTRS-FEEOURS9         PIC X(01).
               05  XTRS-FEECODE10        PIC X(02).
               05  XTRS-FEEAMT10         PIC +9(05).99.
               05  XTRS-FEEOURS10        PIC X(01).
           03  XTRS-FEETBL REDEFINES XTRS-FEE-TAB OCCURS 10 TIMES.
               05  XTRS-FEECODE          PIC X(02).
               05  XTRS-FEEAMT           PIC +9(05).99.
               05  XTRS-FEEOURS          PIC X(01).
           03  XTRS-INSURED              PIC X(01).
           03  XTRS-INSTYPES             PIC X(01).
           03  XTRS-INSELIGIBLE          PIC X(01).   
           03  XTRS-INS-TAB.
               05  XTRS-INSCOMP1         PIC 9(02).
               05  XTRS-INSOURS1         PIC X(01).
               05  XTRS-INSEFF1          PIC 9(08).
               05  XTRS-INSEXP1          PIC 9(08).
               05  XTRS-INSPREM1         PIC +9(05).99.
               05  XTRS-INSCOMM1         PIC +9(06).99.
               05  XTRS-INSCOMP2         PIC 9(02).
               05  XTRS-INSOURS2         PIC X(01).
               05  XTRS-INSEFF2          PIC 9(08).
               05  XTRS-INSEXP2          PIC 9(08).
               05  XTRS-INSPREM2         PIC +9(05).99.
               05  XTRS-INSCOMM2         PIC +9(06).99.
               05  XTRS-INSCOMP3         PIC 9(02).
               05  XTRS-INSOURS3         PIC X(01).
               05  XTRS-INSEFF3          PIC 9(08).
               05  XTRS-INSEXP3          PIC 9(08).
               05  XTRS-INSPREM3         PIC +9(05).99.
               05  XTRS-INSCOMM3         PIC +9(06).99.
               05  XTRS-INSCOMP4         PIC 9(02).
               05  XTRS-INSOURS4         PIC X(01).
               05  XTRS-INSEFF4          PIC 9(08).
               05  XTRS-INSEXP4          PIC 9(08).
               05  XTRS-INSPREM4         PIC +9(05).99.
               05  XTRS-INSCOMM4         PIC +9(06).99.
               05  XTRS-INSCOMP5         PIC 9(02).
               05  XTRS-INSOURS5         PIC X(01).
               05  XTRS-INSEFF5          PIC 9(08).
               05  XTRS-INSEXP5          PIC 9(08).
               05  XTRS-INSPREM5         PIC +9(05).99.
               05  XTRS-INSCOMM5         PIC +9(06).99.
               05  XTRS-INSCOMP6         PIC 9(02).
               05  XTRS-INSOURS6         PIC X(01).
               05  XTRS-INSEFF6          PIC 9(08).
               05  XTRS-INSEXP6          PIC 9(08).
               05  XTRS-INSPREM6         PIC +9(05).99.
               05  XTRS-INSCOMM6         PIC +9(06).99.
               05  XTRS-INSCOMP7         PIC 9(02).
               05  XTRS-INSOURS7         PIC X(01).
               05  XTRS-INSEFF7          PIC 9(08).
               05  XTRS-INSEXP7          PIC 9(08).
               05  XTRS-INSPREM7         PIC +9(05).99.
               05  XTRS-INSCOMM7         PIC +9(06).99.
               05  XTRS-INSCOMP8         PIC 9(02).
               05  XTRS-INSOURS8         PIC X(01).
               05  XTRS-INSEFF8          PIC 9(08).
               05  XTRS-INSEXP8          PIC 9(08).
               05  XTRS-INSPREM8         PIC +9(05).99.
               05  XTRS-INSCOMM8         PIC +9(06).99.
               05  XTRS-INSCOMP9         PIC 9(02).
               05  XTRS-INSOURS9         PIC X(01).
               05  XTRS-INSEFF9          PIC 9(08).
               05  XTRS-INSEXP9          PIC 9(08).
               05  XTRS-INSPREM9         PIC +9(05).99.
               05  XTRS-INSCOMM9         PIC +9(06).99.
               05  XTRS-INSCOMP10        PIC 9(02).
               05  XTRS-INSOURS10        PIC X(01).
               05  XTRS-INSEFF10         PIC 9(08).
               05  XTRS-INSEXP10         PIC 9(08).
               05  XTRS-INSPREM10        PIC +9(05).99.
               05  XTRS-INSCOMM10        PIC +9(06).99.
           03  XTRS-INSTBL REDEFINES XTRS-INS-TAB OCCURS 10 TIMES.
               05  XTRS-INSCOMP          PIC 9(02).
               05  XTRS-INSOURS          PIC X(01).
               05  XTRS-INSEFF           PIC 9(08).
               05  XTRS-INSEXP           PIC 9(08).
               05  XTRS-INSPREM          PIC +9(05).99.
               05  XTRS-INSCOMM          PIC +9(06).99.
           03  XTRS-DLNO                 PIC 9(4).99.
           03  XTRS-DLRECOURSE           PIC X(01).
           03  XTRS-DLPARTRCAMT          PIC +9(07).99.
           03  XTRS-PURBAL               PIC +9(07).99.
           03  XTRS-DLRVPRIN1            PIC +9(07).99.
           03  XTRS-DLRVPRIN2            PIC +9(07).99.
           03  XTRS-DLRVPRIN3            PIC +9(07).99.
           03  XTRS-DLRVCHGS1            PIC +9(06).99.
           03  XTRS-DLRVCHGS2            PIC +9(06).99.
           03  XTRS-DLRVCHGS3            PIC +9(06).99.
           03  XTRS-DLPARVHELD           PIC +9(07).99.
           03  XTRS-DLPARVPAID           PIC +9(06).99.
           03  XTRS-DLDISC               PIC +9(07).99.
           03  XTRS-ANTICTERM            PIC 9(03).
           03  XTRS-ANTICERN-TAB.
               05  XTRS-ANTICERN1            PIC +9(07).99.
               05  XTRS-ANTICERN2            PIC +9(07).99.
               05  XTRS-ANTICERN3            PIC +9(07).99.
               05  XTRS-ANTICERN4            PIC +9(07).99.
               05  XTRS-ANTICERN5            PIC +9(07).99.
               05  XTRS-ANTICERN6            PIC +9(07).99.
           03  XTRS-ANTICERN REDEFINES XTRS-ANTICERN-TAB OCCURS 6 TIMES
                                             PIC +9(07).99.
           03  XTRS-EARNINGS-TAB.
               05  XTRS-EARNINGS1            PIC +9(07).99.
               05  XTRS-EARNINGS2            PIC +9(07).99.
               05  XTRS-EARNINGS3            PIC +9(07).99.
               05  XTRS-EARNINGS4            PIC +9(07).99.
               05  XTRS-EARNINGS5            PIC +9(07).99.
               05  XTRS-EARNINGS6            PIC +9(07).99.
           03  XTRS-EARNINGS REDEFINES XTRS-EARNINGS-TAB OCCURS 6 TIMES
                                             PIC +9(07).99.
           03  XTRS-PRLNNO                   PIC 9(08).
           03  XTRS-INSPREM-DLADJ            PIC +9(05).99.
           03  XTRS-PRDLDISC-UNEARN          PIC +9(07).99.
           03  XTRS-SP-LNCOST-FRMLA          PIC X(01).
           03  XTRS-DL-RVCHGS-CODE1          PIC X(01).
           03  XTRS-DL-RVCHGS-CODE2          PIC X(01).
           03  XTRS-DL-RVCHGS-CODE3          PIC X(01).
           03  XTRS-DL-PARVHELD-CODE         PIC X(01).
           03  XTRS-DL-PARVPAID-CODE         PIC X(01).
           03  XTRS-SPRCLASS                 PIC 9(02).
           03  XTRS-O3INS-DLPROC             PIC +9(06).99.
           03  XTRS-USERID                   PIC X(10).
           03  XTRS-POSTDATE                 PIC 9(08).
           03  XTRS-POSTTIME                 PIC 9(06).
           03  XTRS-DLCOMM-PP-CMSI           PIC +9(4).99.
           03  XTRS-DLCOMM-O3-CMSI           PIC +9(4).99.
           03  XTRS-DLPAID                   PIC X(01).
           03  XTRS-MIP-FG                   PIC X(01).
           03  XTRS-REGION-NUMBER            PIC 9(2).
           03  XTRS-ACTIONCD2                PIC X(4).
           03  FILLER                        PIC X(76).
           03  XTRS-CR                       PIC X(01).
           03  XTRS-LF                       PIC X(01).

