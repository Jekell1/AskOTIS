      * COPYMEMBER: LIBLP/LP01TRL_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DAILY TRANSACTION FILE - DIRECT LOANS
      *
      *=================================================================
      * REV:
      * BLM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  TRL1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRL1-CURSOR-STAT-GOOD             VALUE "00".
           88  TRL1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                        "01" THRU "~~".
       01  TRL2-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRL2-CURSOR-STAT-GOOD             VALUE "00".
           88  TRL2-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRL1-CURSOR-SC-STAT          PIC X(2) VALUE SPACES.
           88  TRL1-CURSOR-SC-STAT-GOOD          VALUE "00".
           88  TRL1-CURSOR-SC-STAT-BAD           VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  TRL1-RETURN-CODE                PIC S9(4)   COMP-5.
       01  TRL2-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QTRL1-WHERE-BEG-KEY.
           03  QTRL1-WBEG-BRNO             PIC 9(04).
           03  QTRL1-WBEG-DATE             PIC X(10).
           03  QTRL1-WBEG-CLASS            PIC 9(03).
           03  QTRL1-WBEG-ACCTNO           PIC 9(08).
           03  QTRL1-WBEG-LAWCODE          PIC 9(02).
           03  QTRL1-WBEG-LINE             PIC 9(03).

       01  QTRL1-WHERE-END-KEY.
           03  QTRL1-WEND-BRNO             PIC 9(04).
           03  QTRL1-WEND-DATE             PIC X(10).
           03  QTRL1-WEND-CLASS            PIC 9(03).
           03  QTRL1-WEND-ACCTNO           PIC 9(08).
           03  QTRL1-WEND-LAWCODE          PIC 9(02).
           03  QTRL1-WEND-LINE             PIC 9(03).

       01  QTRL1-WHERE-SCAN-BEG-KEY.
           03  QTRL1-WSBEG-DATE            PIC X(10).
           03  QTRL1-WSBEG-CLASS           PIC 9(03).
           03  QTRL1-WSBEG-ACCTNO          PIC 9(08).
           03  QTRL1-WSBEG-LAWCODE         PIC 9(02).
 
       01  QTRL2-WHERE-BEG-KEY.
           03  QTRL2-WBEG-DATE             PIC X(10).
           03  QTRL2-WBEG-BRNO             PIC 9(04).
           03  QTRL2-WBEG-CLASS            PIC 9(03).
           03  QTRL2-WBEG-ACCTNO           PIC 9(08).
           03  QTRL2-WBEG-LAWCODE          PIC 9(02).
           03  QTRL2-WBEG-LINE             PIC 9(03).

       01  QTRL2-WHERE-END-KEY.
           03  QTRL2-WEND-DATE             PIC X(10).
           03  QTRL2-WEND-BRNO             PIC 9(04).
           03  QTRL2-WEND-CLASS            PIC 9(03).
           03  QTRL2-WEND-ACCTNO           PIC 9(08).
           03  QTRL2-WEND-LAWCODE          PIC 9(02).
           03  QTRL2-WEND-LINE             PIC 9(03).
 
       01  QTRL-REC.
           03  QTL-BRNO             PIC 9(4).
           03  QTL-DATE             PIC X(10).
           03  QTL-CLASS            PIC 9(3).
           03  QTL-ACCTNO           PIC 9(8).
           03  QTL-LAWCODE          PIC 99.
           03  QTL-LINE             PIC 9(3).
           03  QTL-LTOUCH-DATE      PIC X(10).
           03  QTL-SSNO-1           PIC 9(9).
           03  QTL-MAKERCD-1        PIC X.
           03  QTL-SSNO-2           PIC 9(9).
           03  QTL-MAKERCD-2        PIC X.
           03  QTL-SSNO-3           PIC 9(9).
           03  QTL-MAKERCD-3        PIC X.
           03  QTL-SSNO-4           PIC 9(9).
           03  QTL-MAKERCD-4        PIC X.
           03  QTL-PURCD            PIC XXX.
           03  QTL-SRCD             PIC XX.
           03  QTL-COLLCD           PIC XX.
           03  QTL-SERVACCT         PIC X.
           03  QTL-LOANTYPE         PIC X.
           03  QTL-SUBCLASS         PIC 99.
           03  QTL-LOANDATE         PIC X(10).
           03  QTL-INTDATE          PIC X(10).
           03  QTL-PROCEEDS         PIC S9(7)V99.
           03  QTL-LNAMT            PIC S9(7)V99.
           03  QTL-FINCHG           PIC S9(7)V99.
           03  QTL-INTCHG           PIC S9(7)V99.
           03  QTL-SERCHG           PIC S9(5)V99.
           03  QTL-EXTCHG           PIC S9(5)V99.
           03  QTL-MAINTFEE         PIC S9(5)V99.
           03  QTL-ORGTERM          PIC 9(3).
           03  QTL-1STPYAMT         PIC S9(7)V99.
           03  QTL-NOREGPY          PIC 9(3).
           03  QTL-REGPYAMT         PIC S9(7)V99.
           03  QTL-LASTPYAMT        PIC S9(7)V99.
           03  QTL-PROCASH          PIC S9(7)V99.
           03  QTL-TILLNO           PIC 99.
           03  QTL-FEECODE-1        PIC XX.
           03  QTL-FEEAMT-1         PIC S9(5)V99.
           03  QTL-FEECODE-2        PIC XX.
           03  QTL-FEEAMT-2         PIC S9(5)V99.
           03  QTL-FEECODE-3        PIC XX.
           03  QTL-FEEAMT-3         PIC S9(5)V99.
           03  QTL-FEECODE-4        PIC XX.
           03  QTL-FEEAMT-4         PIC S9(5)V99.
           03  QTL-FEECODE-5        PIC XX.
           03  QTL-FEEAMT-5         PIC S9(5)V99.
           03  QTL-FEECODE-6        PIC XX.
           03  QTL-FEEAMT-6         PIC S9(5)V99.
           03  QTL-FEECODE-7        PIC XX.
           03  QTL-FEEAMT-7         PIC S9(5)V99.
           03  QTL-FEECODE-8        PIC XX.
           03  QTL-FEEAMT-8         PIC S9(5)V99.
           03  QTL-FEECODE-9        PIC XX.
           03  QTL-FEEAMT-9         PIC S9(5)V99.
           03  QTL-FEECODE-10       PIC XX.
           03  QTL-FEEAMT-10        PIC S9(5)V99.
           03  QTL-FEECODE-11       PIC XX.
           03  QTL-FEEAMT-11        PIC S9(5)V99.
           03  QTL-FEECODE-12       PIC XX.
           03  QTL-FEEAMT-12        PIC S9(5)V99.
           03  QTL-FEECODE-13       PIC XX.
           03  QTL-FEEAMT-13        PIC S9(5)V99.
           03  QTL-FEECODE-14       PIC XX.
           03  QTL-FEEAMT-14        PIC S9(5)V99.
           03  QTL-FEECODE-15       PIC XX.
           03  QTL-FEEAMT-15        PIC S9(5)V99.
           03  QTL-FEECODE-16       PIC XX.
           03  QTL-FEEAMT-16        PIC S9(5)V99.
           03  QTL-FEECODE-17       PIC XX.
           03  QTL-FEEAMT-17        PIC S9(5)V99.
           03  QTL-FEECODE-18       PIC XX.
           03  QTL-FEEAMT-18        PIC S9(5)V99.
           03  QTL-FEECODE-19       PIC XX.
           03  QTL-FEEAMT-19        PIC S9(5)V99.
           03  QTL-FEECODE-20       PIC XX.
           03  QTL-FEEAMT-20        PIC S9(5)V99.
           03  QTL-INSURED          PIC X.
           03  QTL-INSTYPES         PIC X.
           03  QTL-INSELIGIBLE  PIC X.
           03  QTL-INSCOMP-1          PIC 99.
           03  QTL-INSOURS-1          PIC X.
           03  QTL-INSEFF-DATE-1      PIC X(10).
           03  QTL-INSEXP-DATE-1      PIC X(10).
           03  QTL-INSPREM-1          PIC S9(5)V99.
           03  QTL-INSCOMM-1          PIC S9(5)V99.
           03  QTL-INSEARNINGS-1      PIC S9(7)V99.
           03  QTL-INSANTICERN-1      PIC S9(7)V99.
           03  QTL-INSCOMP-2          PIC 99.
           03  QTL-INSOURS-2          PIC X.
           03  QTL-INSEFF-DATE-2      PIC X(10).
           03  QTL-INSEXP-DATE-2      PIC X(10).
           03  QTL-INSPREM-2          PIC S9(5)V99.
           03  QTL-INSCOMM-2          PIC S9(5)V99.
           03  QTL-INSEARNINGS-2      PIC S9(7)V99.
           03  QTL-INSANTICERN-2      PIC S9(7)V99.
           03  QTL-INSCOMP-3          PIC 99.
           03  QTL-INSOURS-3          PIC X.
           03  QTL-INSEFF-DATE-3      PIC X(10).
           03  QTL-INSEXP-DATE-3      PIC X(10).
           03  QTL-INSPREM-3          PIC S9(5)V99.
           03  QTL-INSCOMM-3          PIC S9(5)V99.
           03  QTL-INSEARNINGS-3      PIC S9(7)V99.
           03  QTL-INSANTICERN-3      PIC S9(7)V99.
           03  QTL-INSCOMP-4          PIC 99.
           03  QTL-INSOURS-4          PIC X.
           03  QTL-INSEFF-DATE-4      PIC X(10).
           03  QTL-INSEXP-DATE-4      PIC X(10).
           03  QTL-INSPREM-4          PIC S9(5)V99.
           03  QTL-INSCOMM-4          PIC S9(5)V99.
           03  QTL-INSEARNINGS-4      PIC S9(7)V99.
           03  QTL-INSANTICERN-4      PIC S9(7)V99.
           03  QTL-INSCOMP-5          PIC 99.
           03  QTL-INSOURS-5          PIC X.
           03  QTL-INSEFF-DATE-5      PIC X(10).
           03  QTL-INSEXP-DATE-5      PIC X(10).
           03  QTL-INSPREM-5          PIC S9(5)V99.
           03  QTL-INSCOMM-5          PIC S9(5)V99.
           03  QTL-INSEARNINGS-5      PIC S9(7)V99.
           03  QTL-INSANTICERN-5      PIC S9(7)V99.
           03  QTL-INSCOMP-6          PIC 99.
           03  QTL-INSOURS-6          PIC X.
           03  QTL-INSEFF-DATE-6      PIC X(10).
           03  QTL-INSEXP-DATE-6      PIC X(10).
           03  QTL-INSPREM-6          PIC S9(5)V99.
           03  QTL-INSCOMM-6          PIC S9(5)V99.
           03  QTL-INSEARNINGS-6      PIC S9(7)V99.
           03  QTL-INSANTICERN-6      PIC S9(7)V99.
           03  QTL-INSCOMP-7          PIC 99.
           03  QTL-INSOURS-7          PIC X.
           03  QTL-INSEFF-DATE-7      PIC X(10).
           03  QTL-INSEXP-DATE-7      PIC X(10).
           03  QTL-INSPREM-7          PIC S9(5)V99.
           03  QTL-INSCOMM-7          PIC S9(5)V99.
           03  QTL-INSEARNINGS-7      PIC S9(7)V99.
           03  QTL-INSANTICERN-7      PIC S9(7)V99.
           03  QTL-INSCOMP-8          PIC 99.
           03  QTL-INSOURS-8          PIC X.
           03  QTL-INSEFF-DATE-8      PIC X(10).
           03  QTL-INSEXP-DATE-8      PIC X(10).
           03  QTL-INSPREM-8          PIC S9(5)V99.
           03  QTL-INSCOMM-8          PIC S9(5)V99.
           03  QTL-INSEARNINGS-8      PIC S9(7)V99.
           03  QTL-INSANTICERN-8      PIC S9(7)V99.
           03  QTL-INSCOMP-9          PIC 99.
           03  QTL-INSOURS-9          PIC X.
           03  QTL-INSEFF-DATE-9      PIC X(10).
           03  QTL-INSEXP-DATE-9      PIC X(10).
           03  QTL-INSPREM-9          PIC S9(5)V99.
           03  QTL-INSCOMM-9          PIC S9(5)V99.
           03  QTL-INSEARNINGS-9      PIC S9(7)V99.
           03  QTL-INSANTICERN-9      PIC S9(7)V99.
           03  QTL-INSCOMP-10         PIC 99.
           03  QTL-INSOURS-10         PIC X.
           03  QTL-INSEFF-DATE-10     PIC X(10).
           03  QTL-INSEXP-DATE-10     PIC X(10).
           03  QTL-INSPREM-10         PIC S9(5)V99.
           03  QTL-INSCOMM-10         PIC S9(5)V99.
           03  QTL-INSEARNINGS-10     PIC S9(7)V99.
           03  QTL-INSANTICERN-10     PIC S9(7)V99.
           03  QTL-DLNO               PIC 9(4)V99.
           03  QTL-EARNINGS-1         PIC S9(7)V99.
           03  QTL-EARNINGS-2         PIC S9(7)V99.
           03  QTL-EARNINGS-3         PIC S9(7)V99.
           03  QTL-EARNINGS-4         PIC S9(7)V99.
           03  QTL-EARNINGS-5         PIC S9(7)V99.
           03  QTL-EARNINGS-6         PIC S9(7)V99.
           03  QTL-EARNINGS-7         PIC S9(7)V99.
           03  QTL-EARNINGS-8         PIC S9(7)V99.
           03  QTL-EARNINGS-9         PIC S9(7)V99.
           03  QTL-EARNINGS-10        PIC S9(7)V99.
           03  QTL-ANTICERN-1         PIC S9(7)V99.
           03  QTL-ANTICERN-2         PIC S9(7)V99.
           03  QTL-ANTICERN-3         PIC S9(7)V99.
           03  QTL-ANTICERN-4         PIC S9(7)V99.
           03  QTL-ANTICERN-5         PIC S9(7)V99.
           03  QTL-ANTICERN-6         PIC S9(7)V99.
           03  QTL-ANTICERN-7         PIC S9(7)V99.
           03  QTL-ANTICERN-8         PIC S9(7)V99.
           03  QTL-ANTICERN-9         PIC S9(7)V99.
           03  QTL-ANTICERN-10        PIC S9(7)V99.
           03  QTL-PRLNNO             PIC 9(8).
           03  QTL-PRBAL            PIC S9(7)V99.
           03  QTL-PRINTDUE         PIC S9(7)V99.
           03  QTL-PRFINCHGRB       PIC S9(7)V99.
           03  QTL-PRINSRB          PIC S9(7)V99.
           03  QTL-LPEOD5-FG        PIC X.
           03  QTL-STATE-WORKER     PIC S9(7)V99.
           03  QTL-RESTRICTD-ACCTNO PIC 9(8).
           03  QTL-SPRCLASS         PIC 999.
           03  QTL-USERID           PIC X(10).
           03  QTL-POSTDATE         PIC X(10).
           03  QTL-POSTTIME         PIC  9(6).
           03  QTL-GIFTNET          PIC X.
           03  QTL-APR              PIC 9(3)V999.
           03  QTL-SP-LNCOST-FRMLA  PIC X.
           03  QTL-MIP-FG           PIC X.
           03  QTL-INS-TAX-COUNTY-CD PIC X(3).
           03  QTL-INS-TAX-CITY-CD   PIC X(3).
           03  QTL-PRDLDISC-UNEARN   PIC S9(7)V99.
           03  QTL-POINTS            PIC 99V99.
           03  QTL-TAX-AMT           PIC S9(7)V99.
           03  QTL-PBI-PBD-FG        PIC X.
           03  QTL-APPROVED-BY-USER  PIC X(10).
           03  QTL-APPROVED-BY-CODE  PIC X(3).
           03  QTL-APPROVED-BY-DATE  PIC X(10).
           03  QTL-APPROVED-BY-TIME  PIC 9(6).
           03  QTL-DEBIT-CARD-AMT    PIC S9(7)V99.
           03  QTL-ACTIONCD2         PIC X(4).

       EXEC SQL END DECLARE SECTION END-EXEC.
