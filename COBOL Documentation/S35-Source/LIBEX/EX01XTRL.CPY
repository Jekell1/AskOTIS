      * COPYMEMBER: LIBEX/EX01XTRL
      **************************************************************************
      * LOAN TRANSACTION EXTRACTION FILE      ???? BYTES
      *
      * REGIONAL ACCEPTANCE, DATA WAREHOUSE EXTRACTION, REGACC PR# 1675
      *
      * REV:
      *      CS  040907  ADD XTRL-REGION-NUMBER [PR#76 REGACC]
      *     BAH  180306 ADDED XTRL-MIP-FG THRU XTRL-DEBIT-CARD-AMT #1306 WORLD
      *     BAH  20220421 ADDED XTRL-ACTIONCD2, SECURITIZATION #1551
      **************************************************************************
       01  XTRL-REC.
           03  XTRL-BRACCTNO             PIC 9(10).
           03  XTRL-BRNO                 PIC 9(04).
           03  XTRL-ACCTNO               PIC 9(06).
           03  XTRL-TRANS-DATE           PIC 9(08).
           03  XTRL-CODE                 PIC X(01).
           03  XTRL-CLASS                PIC 9(02).
           03  XTRL-LINE                 PIC 9(02).
           03  XTRL-LAWCODE              PIC 9(02).
           03  XTRL-SSNO1                PIC 9(09).
           03  XTRL-MAKERCD1             PIC X(01).
           03  XTRL-SSNO2                PIC 9(09).
           03  XTRL-MAKERCD2             PIC X(01).
           03  XTRL-SSNO3                PIC 9(09).
           03  XTRL-MAKERCD3             PIC X(01).
           03  XTRL-SSNO4                PIC 9(09).
           03  XTRL-MAKERCD4             PIC X(01).
           03  XTRL-PURCD                PIC X(03).
           03  XTRL-SRCD                 PIC X(02).
           03  XTRL-COLLCD               PIC X(02).
           03  XTRL-SERVACCT             PIC X(01).
           03  XTRL-LOANTYPE             PIC X(01).
           03  XTRL-LOANDATE             PIC 9(08).
           03  XTRL-INTDATE              PIC 9(08).
           03  XTRL-PROCEEDS             PIC +9(07).99.
           03  XTRL-LNAMT                PIC +9(07).99.
           03  XTRL-FINCHG               PIC +9(07).99.
           03  XTRL-INTCHG               PIC +9(07).99.
           03  XTRL-SERCHG               PIC +9(05).99.
           03  XTRL-EXTCHG               PIC +9(05).99.
           03  XTRL-MAINTFEE             PIC +9(03).99.
           03  XTRL-ORGTERM              PIC 9(03).
           03  XTRL-1STPYAMT             PIC +9(07).99.
           03  XTRL-NOREGPY              PIC 9(03).
           03  XTRL-REGPYAMT             PIC +9(07).99.
           03  XTRL-LASTPYAMT            PIC +9(07).99.
           03  XTRL-PROCASH              PIC +9(07).99.
           03  XTRL-TILLNO               PIC X(01).
           03  XTRL-FEE-TAB.
               05  XTRL-FEECODE1         PIC X(02).
               05  XTRL-FEEAMT1          PIC +9(05).99.
               05  XTRL-FEEOURS1         PIC X(01).
               05  XTRL-FEECODE2         PIC X(02).
               05  XTRL-FEEAMT2          PIC +9(05).99.
               05  XTRL-FEEOURS2         PIC X(01).
               05  XTRL-FEECODE3         PIC X(02).
               05  XTRL-FEEAMT3          PIC +9(05).99.
               05  XTRL-FEEOURS3         PIC X(01).
               05  XTRL-FEECODE4         PIC X(02).
               05  XTRL-FEEAMT4          PIC +9(05).99.
               05  XTRL-FEEOURS4         PIC X(01).
               05  XTRL-FEECODE5         PIC X(02).
               05  XTRL-FEEAMT5          PIC +9(05).99.
               05  XTRL-FEEOURS5         PIC X(01).
               05  XTRL-FEECODE6         PIC X(02).
               05  XTRL-FEEAMT6          PIC +9(05).99.
               05  XTRL-FEEOURS6         PIC X(01).
               05  XTRL-FEECODE7         PIC X(02).
               05  XTRL-FEEAMT7          PIC +9(05).99.
               05  XTRL-FEEOURS7         PIC X(01).
               05  XTRL-FEECODE8         PIC X(02).
               05  XTRL-FEEAMT8          PIC +9(05).99.
               05  XTRL-FEEOURS8         PIC X(01).
               05  XTRL-FEECODE9         PIC X(02).
               05  XTRL-FEEAMT9          PIC +9(05).99.
               05  XTRL-FEEOURS9         PIC X(01).
               05  XTRL-FEECODE10        PIC X(02).
               05  XTRL-FEEAMT10         PIC +9(05).99.
               05  XTRL-FEEOURS10        PIC X(01).
           03  XTRL-FEETBL REDEFINES XTRL-FEE-TAB OCCURS 10 TIMES.
               05  XTRL-FEECODE          PIC X(02).
               05  XTRL-FEEAMT           PIC +9(05).99.
               05  XTRL-FEEOURS          PIC X(01).
           03  XTRL-INSURED              PIC X(01).
           03  XTRL-INSTYPES             PIC X(01).
           03  XTRL-INSELIGIBLE          PIC X(01).   
           03  XTRL-INS-TAB.
               05  XTRL-INSCOMP1         PIC 9(02).
               05  XTRL-INSOURS1         PIC X(01).
               05  XTRL-INSEFF1          PIC 9(08).
               05  XTRL-INSEXP1          PIC 9(08).
               05  XTRL-INSPREM1         PIC +9(05).99.
               05  XTRL-INSCOMM1         PIC +9(06).99.
               05  XTRL-INSCOMP2         PIC 9(02).
               05  XTRL-INSOURS2         PIC X(01).
               05  XTRL-INSEFF2          PIC 9(08).
               05  XTRL-INSEXP2          PIC 9(08).
               05  XTRL-INSPREM2         PIC +9(05).99.
               05  XTRL-INSCOMM2         PIC +9(06).99.
               05  XTRL-INSCOMP3         PIC 9(02).
               05  XTRL-INSOURS3         PIC X(01).
               05  XTRL-INSEFF3          PIC 9(08).
               05  XTRL-INSEXP3          PIC 9(08).
               05  XTRL-INSPREM3         PIC +9(05).99.
               05  XTRL-INSCOMM3         PIC +9(06).99.
               05  XTRL-INSCOMP4         PIC 9(02).
               05  XTRL-INSOURS4         PIC X(01).
               05  XTRL-INSEFF4          PIC 9(08).
               05  XTRL-INSEXP4          PIC 9(08).
               05  XTRL-INSPREM4         PIC +9(05).99.
               05  XTRL-INSCOMM4         PIC +9(06).99.
               05  XTRL-INSCOMP5         PIC 9(02).
               05  XTRL-INSOURS5         PIC X(01).
               05  XTRL-INSEFF5          PIC 9(08).
               05  XTRL-INSEXP5          PIC 9(08).
               05  XTRL-INSPREM5         PIC +9(05).99.
               05  XTRL-INSCOMM5         PIC +9(06).99.
               05  XTRL-INSCOMP6         PIC 9(02).
               05  XTRL-INSOURS6         PIC X(01).
               05  XTRL-INSEFF6          PIC 9(08).
               05  XTRL-INSEXP6          PIC 9(08).
               05  XTRL-INSPREM6         PIC +9(05).99.
               05  XTRL-INSCOMM6         PIC +9(06).99.
               05  XTRL-INSCOMP7         PIC 9(02).
               05  XTRL-INSOURS7         PIC X(01).
               05  XTRL-INSEFF7          PIC 9(08).
               05  XTRL-INSEXP7          PIC 9(08).
               05  XTRL-INSPREM7         PIC +9(05).99.
               05  XTRL-INSCOMM7         PIC +9(06).99.
               05  XTRL-INSCOMP8         PIC 9(02).
               05  XTRL-INSOURS8         PIC X(01).
               05  XTRL-INSEFF8          PIC 9(08).
               05  XTRL-INSEXP8          PIC 9(08).
               05  XTRL-INSPREM8         PIC +9(05).99.
               05  XTRL-INSCOMM8         PIC +9(06).99.
               05  XTRL-INSCOMP9         PIC 9(02).
               05  XTRL-INSOURS9         PIC X(01).
               05  XTRL-INSEFF9          PIC 9(08).
               05  XTRL-INSEXP9          PIC 9(08).
               05  XTRL-INSPREM9         PIC +9(05).99.
               05  XTRL-INSCOMM9         PIC +9(06).99.
               05  XTRL-INSCOMP10        PIC 9(02).
               05  XTRL-INSOURS10        PIC X(01).
               05  XTRL-INSEFF10         PIC 9(08).
               05  XTRL-INSEXP10         PIC 9(08).
               05  XTRL-INSPREM10        PIC +9(05).99.
               05  XTRL-INSCOMM10        PIC +9(06).99.
           03  XTRL-INSTBL REDEFINES XTRL-INS-TAB OCCURS 10 TIMES.
               05  XTRL-INSCOMP          PIC 9(02).
               05  XTRL-INSOURS          PIC X(01).
               05  XTRL-INSEFF           PIC 9(08).
               05  XTRL-INSEXP           PIC 9(08).
               05  XTRL-INSPREM          PIC +9(05).99.
               05  XTRL-INSCOMM          PIC +9(06).99.
           03  XTRL-DLNO                 PIC 9(4).99.
           03  XTRL-EARNINGS-TAB.
               05  XTRL-EARNINGS1            PIC +9(07).99.
               05  XTRL-EARNINGS2            PIC +9(07).99.
               05  XTRL-EARNINGS3            PIC +9(07).99.
               05  XTRL-EARNINGS4            PIC +9(07).99.
               05  XTRL-EARNINGS5            PIC +9(07).99.
               05  XTRL-EARNINGS6            PIC +9(07).99.
           03  XTRL-EARNINGS REDEFINES XTRL-EARNINGS-TAB OCCURS 6 TIMES
                                             PIC +9(07).99.
           03  XTRL-ANTICERN-TAB.
               05  XTRL-ANTICERN1            PIC +9(07).99.
               05  XTRL-ANTICERN2            PIC +9(07).99.
               05  XTRL-ANTICERN3            PIC +9(07).99.
               05  XTRL-ANTICERN4            PIC +9(07).99.
               05  XTRL-ANTICERN5            PIC +9(07).99.
               05  XTRL-ANTICERN6            PIC +9(07).99.
           03  XTRL-ANTICERN REDEFINES XTRL-ANTICERN-TAB OCCURS 6 TIMES
                                             PIC +9(07).99.
           03  XTRL-PRLNNO                   PIC 9(08).
           03  XTRL-PRBAL                    PIC +9(06).99.
           03  XTRL-PRINTDUE                 PIC +9(05).99.
           03  XTRL-PRFINCHGRB               PIC +9(05).99.
           03  XTRL-PRINSRB                  PIC +9(05).99.
           03  XTRL-LPEOD5-FG                PIC X(01).
           03  XTRL-STATE-WORKER             PIC +9(07).99.
           03  XTRL-RESTRICTD-ACCTNO         PIC 9(08).
           03  XTRL-SPRCLASS                 PIC 9(02).
           03  XTRL-USERID                   PIC X(10).
           03  XTRL-POSTDATE                 PIC 9(08).
           03  XTRL-POSTTIME                 PIC 9(06).
           03  XTRL-GIFTNET                  PIC X(01).
           03  XTRL-APR                      PIC 9(03).999.
           03  XTRL-SP-LNCOST-FRMLA          PIC X(01).
           03  XTRL-REGION-NUMBER            PIC 9(2).
           03  XTRL-MIP-FG                   PIC X.
           03  XTRL-INS-TAX-COUNTY-CD        PIC X(3).
           03  XTRL-INS-TAX-CITY-CD          PIC X(3).
           03  XTRL-PRDLDISC-UNEARN          PIC +9(7).99.
           03  XTRL-POINTS                   PIC 99.99.
           03  XTRL-DEBIT-CARD-AMT           PIC +9(7).99.
           03  XTRL-ACTIONCD2                PIC X(4).

           03  FILLER                        PIC X(60).
           03  XTRL-CR                       PIC X(01).
           03  XTRL-LF                       PIC X(01).
