       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOTRLI.
       AUTHOR.        BAH
       DATE-WRITTEN.  04-13-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOTRLI.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : TRLFILE 
      * TABLE DESC    : TRANSACTION LOAN FILE
      * OUTPUT FILE   : XPC/IOTRLI.PRE
      * USED FOR      : LIBLP/LPWSTRL.CPY  --- QTRL-REC
      *                 LIBLP/LPTRL1R.CPY  --- READ
      *                 LIBLP/LPTRL1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0413 ***NEW***
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
           "@(#)IO/IOTRLI.CBL S34 05/10/21-15:00:52 >".

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
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
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
           03  BEG-DATE                PIC 9(08).
           03  END-DATE                PIC 9(08).
           03  BEG-CLASS               PIC 9(03).
           03  END-CLASS               PIC 9(03).
           03  BEG-ACCTNO              PIC 9(08).
           03  END-ACCTNO              PIC 9(08).
           03  BEG-LAWCODE             PIC 9(02).
           03  END-LAWCODE             PIC 9(02).
           03  BEG-LINE                PIC 9(03).
           03  END-LINE                PIC 9(03).


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
           03  FILLER PIC X(30) VALUE "BRNO     DATE     CLASS   ACCT".
           03  FILLER PIC X(29) VALUE "NO   LAW  LINE    SSNO       ".
           03  FILLER PIC X(30) VALUE "LOANDATE".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-TL-BRNO                   PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-TL-DATE                   PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-TL-CLASS                  PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-TL-ACCTNO                 PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-TL-LAWCODE                PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-TL-LINE                   PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-TL-SSNO1                  PIC 9(09).
           03  FILLER                         PIC X(03).
           03  D01-TL-LOANDATE               PIC 9999/99/99.


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
       COPY "LIBIO/IOTRLI_DEF.CPY".
       COPY "LIBIO/IOTRLI_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOTRLI_SCN.CPY".

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
           MOVE "IOTRLI"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOTRLI-QUEUE-POS  TO Q-POS.
           MOVE IOTRLI-COPIES-POS TO C-POS.
           MOVE IOTRLI-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-LINE.

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
              ( VDUNUM0 > 2      ) 
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-BEG-DATE.

           MOVE SPACES  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           MOVE "ERNM060END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF ( BEG-DATE > END-DATE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "S  8080BEG-DATE." TO VDU.
           MOVE JULIAN-BEG         TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE." TO VDU.
           MOVE JULIAN-END         TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-CLASS.

           MOVE "ENNN030BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-END-CLASS.

           MOVE "ENNN030END-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-CLASS.

           IF ( BEG-CLASS > END-CLASS )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           GO TO ENTER-BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-ALL-CLASS.

           MOVE "SNNN030BEG-CLASS." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-CLASS.
           PERFORM SCREENIO.

           MOVE "SNNN030END-CLASS." TO VDU.
           MOVE 999                 TO VDUNUM0  END-CLASS.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-ACCTNO.

           MOVE "ENNN080BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-ACCTNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-END-ACCTNO.

           MOVE "ENNN080END-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-ACCTNO.

           IF ( BEG-ACCTNO > END-ACCTNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ACCTNO.

           GO TO ENTER-BEG-LAWCODE.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN080BEG-ACCTNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-ACCTNO.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO." TO VDU.
           MOVE 99999999            TO VDUNUM0  END-ACCTNO.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-LAWCODE.

           MOVE "ENNN020BEG-LAWCODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-LAWCODE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-LAWCODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-LAWCODE.

      *-----------------------------------------------------------------
       ENTER-END-LAWCODE.

           MOVE "ENNN020END-LAWCODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LAWCODE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-LAWCODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-LAWCODE.

           IF ( BEG-LAWCODE > END-LAWCODE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LAWCODE.

           GO TO ENTER-BEG-LINE.

      *-----------------------------------------------------------------
       ENTER-ALL-LAWCODE.

           MOVE "SNNN020BEG-LAWCODE." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-LAWCODE.
           PERFORM SCREENIO.

           MOVE "SNNN020END-LAWCODE." TO VDU.
           MOVE 99                    TO VDUNUM0  END-LAWCODE.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-LINE.

           MOVE "ENNN030BEG-LINE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LAWCODE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-LINE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-LINE.

      *-----------------------------------------------------------------
       ENTER-END-LINE.

           MOVE "ENNN030END-LINE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LINE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-LINE.

           IF ( BEG-LINE > END-LINE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LINE.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-LINE.

           MOVE "SNNN030BEG-LINE." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-LINE.
           PERFORM SCREENIO.

           MOVE "SNNN030END-LINE." TO VDU.
           MOVE 999                 TO VDUNUM0  END-LINE.
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

           MOVE "S  A000BEG-DATE."    TO VDU.
           MOVE BEG-DATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A000END-DATE."    TO VDU.
           MOVE END-DATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "SNNN030BEG-CLASS."       TO VDU.
           MOVE BEG-CLASS                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-CLASS."       TO VDU.
           MOVE END-CLASS                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080BEG-ACCTNO."      TO VDU.
           MOVE BEG-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO."      TO VDU.
           MOVE END-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020BEG-LAWCODE."       TO VDU.
           MOVE BEG-LAWCODE                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-LAWCODE."       TO VDU.
           MOVE END-LAWCODE                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030BEG-LINE."       TO VDU.
           MOVE BEG-LINE                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-LINE."       TO VDU.
           MOVE END-LINE                 TO VDUNUM0.
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

           MOVE "B"                        TO TRL-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QTRL1-WBEG-BRNO.
           MOVE BR-NO     TO QTRL1-WEND-BRNO.

           MOVE BEG-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WBEG-DATE.
           MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WEND-DATE.

           MOVE BEG-CLASS  TO QTRL1-WBEG-CLASS.
           MOVE END-CLASS  TO QTRL1-WEND-CLASS.

           MOVE BEG-ACCTNO TO QTRL1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QTRL1-WEND-ACCTNO.
           MOVE BEG-LAWCODE TO QTRL1-WBEG-LAWCODE.
           MOVE END-LAWCODE TO QTRL1-WEND-LAWCODE.
           MOVE BEG-LINE TO QTRL1-WBEG-LINE.
           MOVE END-LINE TO QTRL1-WEND-LINE.


      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-TRL1-FILE
           PERFORM START-TRL1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-TRL1-FILE-NEXT.
           IF ( IO-BAD ) 
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

              IF ( TL-BRNO < BEG-BRANCH )
                  GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( TL-BRNO > END-BRANCH )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF TL-DATE < BEG-DATE OR > END-DATE
                    GO TO PROCESS-BRANCH-NEXT.

           IF TL-CLASS < BEG-CLASS OR > END-CLASS
              GO TO PROCESS-BRANCH-NEXT.
           IF TL-ACCTNO < BEG-ACCTNO OR > END-ACCTNO
              GO TO PROCESS-BRANCH-NEXT.
           IF TL-LAWCODE < BEG-LAWCODE OR > END-LAWCODE
              GO TO PROCESS-BRANCH-NEXT.
           IF TL-LINE < BEG-LINE OR > END-LINE
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE TL-BRNO                    TO D01-TL-BRNO.
           MOVE TL-DATE                    TO D01-TL-DATE.
           MOVE TL-CLASS                   TO D01-TL-CLASS.
           MOVE TL-ACCTNO                  TO D01-TL-ACCTNO.
           MOVE TL-LAWCODE                 TO D01-TL-LAWCODE.
           MOVE TL-LINE                    TO D01-TL-LINE.
           MOVE TL-SSNO(1)                 TO D01-TL-SSNO1.
           MOVE TL-LOANDATE                TO D01-TL-LOANDATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-TRL1-FILE.

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

           IF PROCESS-KEY = 1
              MOVE TRL1-KEY TO PACE-DETAIL-KEY
           ELSE
           STRING TL-DATE, TL-BRNO, TL-CLASS, TL-ACCTNO, TL-LAWCODE,
                  TL-LINE DELIMITED BY SIZE INTO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING DATE               : "    TO R01-PROMPT.
           MOVE BEG-DATE                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE               : "    TO R01-PROMPT.
           MOVE END-DATE                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS               : "    TO R01-PROMPT.
           MOVE BEG-CLASS                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CLASS               : "    TO R01-PROMPT.
           MOVE END-CLASS                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING ACCTNO              : "    TO R01-PROMPT.
           MOVE BEG-ACCTNO                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ACCTNO              : "    TO R01-PROMPT.
           MOVE END-ACCTNO                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING LAWCODE              : "    TO R01-PROMPT.
           MOVE BEG-LAWCODE                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    LAWCODE              : "    TO R01-PROMPT.
           MOVE END-LAWCODE                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE "BEGINNING LINE                : "    TO R01-PROMPT.
           MOVE BEG-LINE                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    LINE                : "    TO R01-PROMPT.
           MOVE END-LINE                              TO R01-NUMERIC.
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
           MOVE "SQL TABLE IO UTILITY - TRLFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOTRLI-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOTRLI-SYSDATE.
           DISPLAY IOTRLI-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOTRLI-FORM-FIELDS TO WR-BUF.
           MOVE IOTRLI-FORM-LIST TO WR-LIST.
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

           MOVE IOTRLI-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOTRLI-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOTRLI_EVA.CPY".
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
      *    LOAD TRL-REC FROM QTRL-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-TRL-FIELDS.
           MOVE QTL-BRNO TO TL-BRNO.

           IF ( QTL-DATE = "1900-01-01" )
              MOVE ZEROES TO TL-DATE
           ELSE
              MOVE QTL-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-DATE.

           MOVE QTL-CLASS TO TL-CLASS.
           MOVE QTL-ACCTNO TO TL-ACCTNO.
           MOVE QTL-LAWCODE TO TL-LAWCODE.
           MOVE QTL-LINE TO TL-LINE.

           IF ( QTL-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO TL-LTOUCH-DATE
           ELSE
              MOVE QTL-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-LTOUCH-DATE.

           MOVE QTL-SSNO-1 TO TL-SSNO(1).
           MOVE QTL-MAKERCD-1 TO TL-MAKERCD(1).
           MOVE QTL-SSNO-2 TO TL-SSNO(2).
           MOVE QTL-MAKERCD-2 TO TL-MAKERCD(2).
           MOVE QTL-SSNO-3 TO TL-SSNO(3).
           MOVE QTL-MAKERCD-3 TO TL-MAKERCD(3).
           MOVE QTL-SSNO-4 TO TL-SSNO(4).
           MOVE QTL-MAKERCD-4 TO TL-MAKERCD(4).
           MOVE QTL-PURCD TO TL-PURCD.
           MOVE QTL-SRCD TO TL-SRCD.
           MOVE QTL-COLLCD TO TL-COLLCD.
           MOVE QTL-SERVACCT TO TL-SERVACCT.
           MOVE QTL-LOANTYPE TO TL-LOANTYPE.
           MOVE QTL-SUBCLASS TO TL-SUBCLASS.

           IF ( QTL-LOANDATE = "1900-01-01" )
              MOVE ZEROES TO TL-LOANDATE
           ELSE
              MOVE QTL-LOANDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-LOANDATE.


           IF ( QTL-INTDATE = "1900-01-01" )
              MOVE ZEROES TO TL-INTDATE
           ELSE
              MOVE QTL-INTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INTDATE.

           MOVE QTL-PROCEEDS TO TL-PROCEEDS.
           MOVE QTL-LNAMT TO TL-LNAMT.
           MOVE QTL-FINCHG TO TL-FINCHG.
           MOVE QTL-INTCHG TO TL-INTCHG.
           MOVE QTL-SERCHG TO TL-SERCHG.
           MOVE QTL-EXTCHG TO TL-EXTCHG.
           MOVE QTL-MAINTFEE TO TL-MAINTFEE.
           MOVE QTL-ORGTERM TO TL-ORGTERM.
           MOVE QTL-1STPYAMT TO TL-1STPYAMT.
           MOVE QTL-NOREGPY TO TL-NOREGPY.
           MOVE QTL-REGPYAMT TO TL-REGPYAMT.
           MOVE QTL-LASTPYAMT TO TL-LASTPYAMT.
           MOVE QTL-PROCASH TO TL-PROCASH.
           MOVE QTL-TILLNO TO TL-TILLNO.
           MOVE QTL-FEECODE-1 TO TL-FEECODE(1).
           MOVE QTL-FEEAMT-1 TO TL-FEEAMT(1).
           MOVE QTL-FEECODE-2 TO TL-FEECODE(2).
           MOVE QTL-FEEAMT-2 TO TL-FEEAMT(2).
           MOVE QTL-FEECODE-3 TO TL-FEECODE(3).
           MOVE QTL-FEEAMT-3 TO TL-FEEAMT(3).
           MOVE QTL-FEECODE-4 TO TL-FEECODE(4).
           MOVE QTL-FEEAMT-4 TO TL-FEEAMT(4).
           MOVE QTL-FEECODE-5 TO TL-FEECODE(5).
           MOVE QTL-FEEAMT-5 TO TL-FEEAMT(5).
           MOVE QTL-FEECODE-6 TO TL-FEECODE(6).
           MOVE QTL-FEEAMT-6 TO TL-FEEAMT(6).
           MOVE QTL-FEECODE-7 TO TL-FEECODE(7).
           MOVE QTL-FEEAMT-7 TO TL-FEEAMT(7).
           MOVE QTL-FEECODE-8 TO TL-FEECODE(8).
           MOVE QTL-FEEAMT-8 TO TL-FEEAMT(8).
           MOVE QTL-FEECODE-9 TO TL-FEECODE(9).
           MOVE QTL-FEEAMT-9 TO TL-FEEAMT(9).
           MOVE QTL-FEECODE-10 TO TL-FEECODE(10).
           MOVE QTL-FEEAMT-10 TO TL-FEEAMT(10).
           MOVE QTL-FEECODE-11 TO TL-FEECODE(11).
           MOVE QTL-FEEAMT-11 TO TL-FEEAMT(11).
           MOVE QTL-FEECODE-12 TO TL-FEECODE(12).
           MOVE QTL-FEEAMT-12 TO TL-FEEAMT(12).
           MOVE QTL-FEECODE-13 TO TL-FEECODE(13).
           MOVE QTL-FEEAMT-13 TO TL-FEEAMT(13).
           MOVE QTL-FEECODE-14 TO TL-FEECODE(14).
           MOVE QTL-FEEAMT-14 TO TL-FEEAMT(14).
           MOVE QTL-FEECODE-15 TO TL-FEECODE(15).
           MOVE QTL-FEEAMT-15 TO TL-FEEAMT(15).
           MOVE QTL-FEECODE-16 TO TL-FEECODE(16).
           MOVE QTL-FEEAMT-16 TO TL-FEEAMT(16).
           MOVE QTL-FEECODE-17 TO TL-FEECODE(17).
           MOVE QTL-FEEAMT-17 TO TL-FEEAMT(17).
           MOVE QTL-FEECODE-18 TO TL-FEECODE(18).
           MOVE QTL-FEEAMT-18 TO TL-FEEAMT(18).
           MOVE QTL-FEECODE-19 TO TL-FEECODE(19).
           MOVE QTL-FEEAMT-19 TO TL-FEEAMT(19).
           MOVE QTL-FEECODE-20 TO TL-FEECODE(20).
           MOVE QTL-FEEAMT-20 TO TL-FEEAMT(20).
           MOVE QTL-INSURED TO TL-INSURED.
           MOVE QTL-INSTYPES TO TL-INSTYPES.
           MOVE QTL-INSELIGIBLE TO TL-INSELIGIBLE.
           MOVE QTL-INSCOMP-1 TO TL-INSCOMP(1).
           MOVE QTL-INSOURS-1 TO TL-INSOURS(1).

           IF ( QTL-INSEFF-DATE-1 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEFF-DATE(1)
           ELSE
              MOVE QTL-INSEFF-DATE-1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEFF-DATE(1).


           IF ( QTL-INSEXP-DATE-1 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEXP-DATE(1)
           ELSE
              MOVE QTL-INSEXP-DATE-1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEXP-DATE(1).

           MOVE QTL-INSPREM-1 TO TL-INSPREM(1).
           MOVE QTL-INSCOMM-1 TO TL-INSCOMM(1).
           MOVE QTL-INSEARNINGS-1 TO TL-INSEARNINGS(1).
           MOVE QTL-INSANTICERN-1 TO TL-INSANTICERN(1).
           MOVE QTL-INSCOMP-2 TO TL-INSCOMP(2).
           MOVE QTL-INSOURS-2 TO TL-INSOURS(2).

           IF ( QTL-INSEFF-DATE-2 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEFF-DATE(2)
           ELSE
              MOVE QTL-INSEFF-DATE-2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEFF-DATE(2).


           IF ( QTL-INSEXP-DATE-2 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEXP-DATE(2)
           ELSE
              MOVE QTL-INSEXP-DATE-2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEXP-DATE(2).

           MOVE QTL-INSPREM-2 TO TL-INSPREM(2).
           MOVE QTL-INSCOMM-2 TO TL-INSCOMM(2).
           MOVE QTL-INSEARNINGS-2 TO TL-INSEARNINGS(2).
           MOVE QTL-INSANTICERN-2 TO TL-INSANTICERN(2).
           MOVE QTL-INSCOMP-3 TO TL-INSCOMP(3).
           MOVE QTL-INSOURS-3 TO TL-INSOURS(3).

           IF ( QTL-INSEFF-DATE-3 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEFF-DATE(3)
           ELSE
              MOVE QTL-INSEFF-DATE-3 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEFF-DATE(3).


           IF ( QTL-INSEXP-DATE-3 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEXP-DATE(3)
           ELSE
              MOVE QTL-INSEXP-DATE-3 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEXP-DATE(3).

           MOVE QTL-INSPREM-3 TO TL-INSPREM(3).
           MOVE QTL-INSCOMM-3 TO TL-INSCOMM(3).
           MOVE QTL-INSEARNINGS-3 TO TL-INSEARNINGS(3).
           MOVE QTL-INSANTICERN-3 TO TL-INSANTICERN(3).
           MOVE QTL-INSCOMP-4 TO TL-INSCOMP(4).
           MOVE QTL-INSOURS-4 TO TL-INSOURS(4).

           IF ( QTL-INSEFF-DATE-4 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEFF-DATE(4)
           ELSE
              MOVE QTL-INSEFF-DATE-4 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEFF-DATE(4).


           IF ( QTL-INSEXP-DATE-4 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEXP-DATE(4)
           ELSE
              MOVE QTL-INSEXP-DATE-4 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEXP-DATE(4).

           MOVE QTL-INSPREM-4 TO TL-INSPREM(4).
           MOVE QTL-INSCOMM-4 TO TL-INSCOMM(4).
           MOVE QTL-INSEARNINGS-4 TO TL-INSEARNINGS(4).
           MOVE QTL-INSANTICERN-4 TO TL-INSANTICERN(4).
           MOVE QTL-INSCOMP-5 TO TL-INSCOMP(5).
           MOVE QTL-INSOURS-5 TO TL-INSOURS(5).

           IF ( QTL-INSEFF-DATE-5 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEFF-DATE(5)
           ELSE
              MOVE QTL-INSEFF-DATE-5 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEFF-DATE(5).


           IF ( QTL-INSEXP-DATE-5 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEXP-DATE(5)
           ELSE
              MOVE QTL-INSEXP-DATE-5 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEXP-DATE(5).

           MOVE QTL-INSPREM-5 TO TL-INSPREM(5).
           MOVE QTL-INSCOMM-5 TO TL-INSCOMM(5).
           MOVE QTL-INSEARNINGS-5 TO TL-INSEARNINGS(5).
           MOVE QTL-INSANTICERN-5 TO TL-INSANTICERN(5).
           MOVE QTL-INSCOMP-6 TO TL-INSCOMP(6).
           MOVE QTL-INSOURS-6 TO TL-INSOURS(6).

           IF ( QTL-INSEFF-DATE-6 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEFF-DATE(6)
           ELSE
              MOVE QTL-INSEFF-DATE-6 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEFF-DATE(6).


           IF ( QTL-INSEXP-DATE-6 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEXP-DATE(6)
           ELSE
              MOVE QTL-INSEXP-DATE-6 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEXP-DATE(6).

           MOVE QTL-INSPREM-6 TO TL-INSPREM(6).
           MOVE QTL-INSCOMM-6 TO TL-INSCOMM(6).
           MOVE QTL-INSEARNINGS-6 TO TL-INSEARNINGS(6).
           MOVE QTL-INSANTICERN-6 TO TL-INSANTICERN(6).
           MOVE QTL-INSCOMP-7 TO TL-INSCOMP(7).
           MOVE QTL-INSOURS-7 TO TL-INSOURS(7).

           IF ( QTL-INSEFF-DATE-7 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEFF-DATE(7)
           ELSE
              MOVE QTL-INSEFF-DATE-7 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEFF-DATE(7).


           IF ( QTL-INSEXP-DATE-7 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEXP-DATE(7)
           ELSE
              MOVE QTL-INSEXP-DATE-7 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEXP-DATE(7).

           MOVE QTL-INSPREM-7 TO TL-INSPREM(7).
           MOVE QTL-INSCOMM-7 TO TL-INSCOMM(7).
           MOVE QTL-INSEARNINGS-7 TO TL-INSEARNINGS(7).
           MOVE QTL-INSANTICERN-7 TO TL-INSANTICERN(7).
           MOVE QTL-INSCOMP-8 TO TL-INSCOMP(8).
           MOVE QTL-INSOURS-8 TO TL-INSOURS(8).

           IF ( QTL-INSEFF-DATE-8 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEFF-DATE(8)
           ELSE
              MOVE QTL-INSEFF-DATE-8 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEFF-DATE(8).


           IF ( QTL-INSEXP-DATE-8 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEXP-DATE(8)
           ELSE
              MOVE QTL-INSEXP-DATE-8 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEXP-DATE(8).

           MOVE QTL-INSPREM-8 TO TL-INSPREM(8).
           MOVE QTL-INSCOMM-8 TO TL-INSCOMM(8).
           MOVE QTL-INSEARNINGS-8 TO TL-INSEARNINGS(8).
           MOVE QTL-INSANTICERN-8 TO TL-INSANTICERN(8).
           MOVE QTL-INSCOMP-9 TO TL-INSCOMP(9).
           MOVE QTL-INSOURS-9 TO TL-INSOURS(9).

           IF ( QTL-INSEFF-DATE-9 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEFF-DATE(9)
           ELSE
              MOVE QTL-INSEFF-DATE-9 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEFF-DATE(9).


           IF ( QTL-INSEXP-DATE-9 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEXP-DATE(9)
           ELSE
              MOVE QTL-INSEXP-DATE-9 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEXP-DATE(9).

           MOVE QTL-INSPREM-9 TO TL-INSPREM(9).
           MOVE QTL-INSCOMM-9 TO TL-INSCOMM(9).
           MOVE QTL-INSEARNINGS-9 TO TL-INSEARNINGS(9).
           MOVE QTL-INSANTICERN-9 TO TL-INSANTICERN(9).
           MOVE QTL-INSCOMP-10 TO TL-INSCOMP(10).
           MOVE QTL-INSOURS-10 TO TL-INSOURS(10).

           IF ( QTL-INSEFF-DATE-10 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEFF-DATE(10)
           ELSE
              MOVE QTL-INSEFF-DATE-10 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEFF-DATE(10).


           IF ( QTL-INSEXP-DATE-10 = "1900-01-01" )
              MOVE ZEROES TO TL-INSEXP-DATE(10)
           ELSE
              MOVE QTL-INSEXP-DATE-10 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-INSEXP-DATE(10).

           MOVE QTL-INSPREM-10 TO TL-INSPREM(10).
           MOVE QTL-INSCOMM-10 TO TL-INSCOMM(10).
           MOVE QTL-INSEARNINGS-10 TO TL-INSEARNINGS(10).
           MOVE QTL-INSANTICERN-10 TO TL-INSANTICERN(10).

           MOVE QTL-DLNO TO TL-DLNO.
           MOVE QTL-EARNINGS-1 TO TL-EARNINGS(1).
           MOVE QTL-EARNINGS-2 TO TL-EARNINGS(2).
           MOVE QTL-EARNINGS-3 TO TL-EARNINGS(3).
           MOVE QTL-EARNINGS-4 TO TL-EARNINGS(4).
           MOVE QTL-EARNINGS-5 TO TL-EARNINGS(5).
           MOVE QTL-EARNINGS-6 TO TL-EARNINGS(6).
           MOVE QTL-EARNINGS-7 TO TL-EARNINGS(7).
           MOVE QTL-EARNINGS-8 TO TL-EARNINGS(8).
           MOVE QTL-EARNINGS-9 TO TL-EARNINGS(9).
           MOVE QTL-EARNINGS-10 TO TL-EARNINGS(10).
           MOVE QTL-ANTICERN-1 TO TL-ANTICERN(1).
           MOVE QTL-ANTICERN-2 TO TL-ANTICERN(2).
           MOVE QTL-ANTICERN-3 TO TL-ANTICERN(3).
           MOVE QTL-ANTICERN-4 TO TL-ANTICERN(4).
           MOVE QTL-ANTICERN-5 TO TL-ANTICERN(5).
           MOVE QTL-ANTICERN-6 TO TL-ANTICERN(6).
           MOVE QTL-ANTICERN-7 TO TL-ANTICERN(7).
           MOVE QTL-ANTICERN-8 TO TL-ANTICERN(8).
           MOVE QTL-ANTICERN-9 TO TL-ANTICERN(9).
           MOVE QTL-ANTICERN-10 TO TL-ANTICERN(10).
           MOVE QTL-PRLNNO TO TL-PRLNNO.
           MOVE QTL-PRBAL TO TL-PRBAL.
           MOVE QTL-PRINTDUE TO TL-PRINTDUE.
           MOVE QTL-PRFINCHGRB TO TL-PRFINCHGRB.
           MOVE QTL-PRINSRB TO TL-PRINSRB.
           MOVE QTL-LPEOD5-FG TO TL-LPEOD5-FG.
           MOVE QTL-STATE-WORKER TO TL-STATE-WORKER.
           MOVE QTL-RESTRICTD-ACCTNO TO TL-RESTRICTD-ACCTNO.
           MOVE QTL-SPRCLASS TO TL-SPRCLASS.
           MOVE QTL-USERID TO TL-USERID.

           IF ( QTL-POSTDATE = "1900-01-01" )
              MOVE ZEROES TO TL-POSTDATE
           ELSE
              MOVE QTL-POSTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-POSTDATE.

           MOVE QTL-POSTTIME TO TL-POSTTIME.
           MOVE QTL-GIFTNET TO TL-GIFTNET.
           MOVE QTL-APR TO TL-APR.
           MOVE QTL-SP-LNCOST-FRMLA TO TL-SP-LNCOST-FRMLA.
           MOVE QTL-MIP-FG TO TL-MIP-FG.
           MOVE QTL-INS-TAX-COUNTY-CD TO TL-INS-TAX-COUNTY-CD.
           MOVE QTL-INS-TAX-CITY-CD TO TL-INS-TAX-CITY-CD.
           MOVE QTL-PRDLDISC-UNEARN TO TL-PRDLDISC-UNEARN.
           MOVE QTL-POINTS TO TL-POINTS.
           MOVE QTL-TAX-AMT TO TL-TAX-AMT.
           MOVE QTL-PBI-PBD-FG TO TL-PBI-PBD-FG.
           MOVE QTL-APPROVED-BY-USER TO TL-APPROVED-BY-USER.
           MOVE QTL-APPROVED-BY-CODE TO TL-APPROVED-BY-CODE.

           IF ( QTL-APPROVED-BY-DATE = "1900-01-01" )
              MOVE ZEROES TO TL-APPROVED-BY-DATE
           ELSE
              MOVE QTL-APPROVED-BY-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TL-APPROVED-BY-DATE.

           MOVE QTL-APPROVED-BY-TIME TO TL-APPROVED-BY-TIME.
           MOVE QTL-DEBIT-CARD-AMT TO TL-DEBIT-CARD-AMT.
 
      *-----------------------------------------------------------------
      *    LOAD QTRL-REC FROM TRL-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-TRL-FIELDS.
           MOVE TL-BRNO TO QTL-BRNO.

           IF ( TL-DATE = ZEROES )
              MOVE "1900-01-01" TO QTL-DATE
           ELSE
              MOVE TL-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-DATE.

           MOVE TL-CLASS TO QTL-CLASS.
           MOVE TL-ACCTNO TO QTL-ACCTNO.
           MOVE TL-LAWCODE TO QTL-LAWCODE.
           MOVE TL-LINE TO QTL-LINE.

           IF ( TL-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTL-LTOUCH-DATE
           ELSE
              MOVE TL-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-LTOUCH-DATE.

           MOVE TL-SSNO(1) TO QTL-SSNO-1.
           MOVE TL-MAKERCD(1) TO QTL-MAKERCD-1.
           MOVE TL-SSNO(2) TO QTL-SSNO-2.
           MOVE TL-MAKERCD(2) TO QTL-MAKERCD-2.
           MOVE TL-SSNO(3) TO QTL-SSNO-3.
           MOVE TL-MAKERCD(3) TO QTL-MAKERCD-3.
           MOVE TL-SSNO(4) TO QTL-SSNO-4.
           MOVE TL-MAKERCD(4) TO QTL-MAKERCD-4.
           MOVE TL-PURCD TO QTL-PURCD.
           MOVE TL-SRCD TO QTL-SRCD.
           MOVE TL-COLLCD TO QTL-COLLCD.
           MOVE TL-SERVACCT TO QTL-SERVACCT.
           MOVE TL-LOANTYPE TO QTL-LOANTYPE.
           MOVE TL-SUBCLASS TO QTL-SUBCLASS.

           IF ( TL-LOANDATE = ZEROES )
              MOVE "1900-01-01" TO QTL-LOANDATE
           ELSE
              MOVE TL-LOANDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-LOANDATE.


           IF ( TL-INTDATE = ZEROES )
              MOVE "1900-01-01" TO QTL-INTDATE
           ELSE
              MOVE TL-INTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INTDATE.

           MOVE TL-PROCEEDS TO QTL-PROCEEDS.
           MOVE TL-LNAMT TO QTL-LNAMT.
           MOVE TL-FINCHG TO QTL-FINCHG.
           MOVE TL-INTCHG TO QTL-INTCHG.
           MOVE TL-SERCHG TO QTL-SERCHG.
           MOVE TL-EXTCHG TO QTL-EXTCHG.
           MOVE TL-MAINTFEE TO QTL-MAINTFEE.
           MOVE TL-ORGTERM TO QTL-ORGTERM.
           MOVE TL-1STPYAMT TO QTL-1STPYAMT.
           MOVE TL-NOREGPY TO QTL-NOREGPY.
           MOVE TL-REGPYAMT TO QTL-REGPYAMT.
           MOVE TL-LASTPYAMT TO QTL-LASTPYAMT.
           MOVE TL-PROCASH TO QTL-PROCASH.
           MOVE TL-TILLNO TO QTL-TILLNO.
           MOVE TL-FEECODE(1) TO QTL-FEECODE-1.
           MOVE TL-FEEAMT(1) TO QTL-FEEAMT-1.
           MOVE TL-FEECODE(2) TO QTL-FEECODE-2.
           MOVE TL-FEEAMT(2) TO QTL-FEEAMT-2.
           MOVE TL-FEECODE(3) TO QTL-FEECODE-3.
           MOVE TL-FEEAMT(3) TO QTL-FEEAMT-3.
           MOVE TL-FEECODE(4) TO QTL-FEECODE-4.
           MOVE TL-FEEAMT(4) TO QTL-FEEAMT-4.
           MOVE TL-FEECODE(5) TO QTL-FEECODE-5.
           MOVE TL-FEEAMT(5) TO QTL-FEEAMT-5.
           MOVE TL-FEECODE(6) TO QTL-FEECODE-6.
           MOVE TL-FEEAMT(6) TO QTL-FEEAMT-6.
           MOVE TL-FEECODE(7) TO QTL-FEECODE-7.
           MOVE TL-FEEAMT(7) TO QTL-FEEAMT-7.
           MOVE TL-FEECODE(8) TO QTL-FEECODE-8.
           MOVE TL-FEEAMT(8) TO QTL-FEEAMT-8.
           MOVE TL-FEECODE(9) TO QTL-FEECODE-9.
           MOVE TL-FEEAMT(9) TO QTL-FEEAMT-9.
           MOVE TL-FEECODE(10) TO QTL-FEECODE-10.
           MOVE TL-FEEAMT(10) TO QTL-FEEAMT-10.
           MOVE TL-FEECODE(11) TO QTL-FEECODE-11.
           MOVE TL-FEEAMT(11) TO QTL-FEEAMT-11.
           MOVE TL-FEECODE(12) TO QTL-FEECODE-12.
           MOVE TL-FEEAMT(12) TO QTL-FEEAMT-12.
           MOVE TL-FEECODE(13) TO QTL-FEECODE-13.
           MOVE TL-FEEAMT(13) TO QTL-FEEAMT-13.
           MOVE TL-FEECODE(14) TO QTL-FEECODE-14.
           MOVE TL-FEEAMT(14) TO QTL-FEEAMT-14.
           MOVE TL-FEECODE(15) TO QTL-FEECODE-15.
           MOVE TL-FEEAMT(15) TO QTL-FEEAMT-15.
           MOVE TL-FEECODE(16) TO QTL-FEECODE-16.
           MOVE TL-FEEAMT(16) TO QTL-FEEAMT-16.
           MOVE TL-FEECODE(17) TO QTL-FEECODE-17.
           MOVE TL-FEEAMT(17) TO QTL-FEEAMT-17.
           MOVE TL-FEECODE(18) TO QTL-FEECODE-18.
           MOVE TL-FEEAMT(18) TO QTL-FEEAMT-18.
           MOVE TL-FEECODE(19) TO QTL-FEECODE-19.
           MOVE TL-FEEAMT(19) TO QTL-FEEAMT-19.
           MOVE TL-FEECODE(20) TO QTL-FEECODE-20.
           MOVE TL-FEEAMT(20) TO QTL-FEEAMT-20.
           MOVE TL-INSURED TO QTL-INSURED.
           MOVE TL-INSTYPES TO QTL-INSTYPES.
           MOVE TL-INSELIGIBLE TO QTL-INSELIGIBLE.

           MOVE TL-INSCOMP(1) TO QTL-INSCOMP-1.
           MOVE TL-INSOURS(1) TO QTL-INSOURS-1.
           IF ( TL-INSEFF-DATE(1) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEFF-DATE-1
           ELSE
              MOVE TL-INSEFF-DATE(1) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEFF-DATE-1.
           IF ( TL-INSEXP-DATE(1) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEXP-DATE-1
           ELSE
              MOVE TL-INSEXP-DATE(1) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEXP-DATE-1.
           MOVE TL-INSPREM(1) TO QTL-INSPREM-1.
           MOVE TL-INSCOMM(1) TO QTL-INSCOMM-1.
           MOVE TL-INSEARNINGS(1) TO QTL-INSEARNINGS-1.
           MOVE TL-INSANTICERN(1) TO QTL-INSANTICERN-1.

           MOVE TL-INSCOMP(2) TO QTL-INSCOMP-2.
           MOVE TL-INSOURS(2) TO QTL-INSOURS-2.
           IF ( TL-INSEFF-DATE(2) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEFF-DATE-2
           ELSE
              MOVE TL-INSEFF-DATE(2) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEFF-DATE-2.
           IF ( TL-INSEXP-DATE(2) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEXP-DATE-2
           ELSE
              MOVE TL-INSEXP-DATE(2) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEXP-DATE-2.
           MOVE TL-INSPREM(2) TO QTL-INSPREM-2.
           MOVE TL-INSCOMM(2) TO QTL-INSCOMM-2.
           MOVE TL-INSEARNINGS(2) TO QTL-INSEARNINGS-2.
           MOVE TL-INSANTICERN(2) TO QTL-INSANTICERN-2.

           MOVE TL-INSCOMP(3) TO QTL-INSCOMP-3.
           MOVE TL-INSOURS(3) TO QTL-INSOURS-3.
           IF ( TL-INSEFF-DATE(3) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEFF-DATE-3
           ELSE
              MOVE TL-INSEFF-DATE(3) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEFF-DATE-3.
           IF ( TL-INSEXP-DATE(3) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEXP-DATE-3
           ELSE
              MOVE TL-INSEXP-DATE(3) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEXP-DATE-3.
           MOVE TL-INSPREM(3) TO QTL-INSPREM-3.
           MOVE TL-INSCOMM(3) TO QTL-INSCOMM-3.
           MOVE TL-INSEARNINGS(3) TO QTL-INSEARNINGS-3.
           MOVE TL-INSANTICERN(3) TO QTL-INSANTICERN-3.

           MOVE TL-INSCOMP(4) TO QTL-INSCOMP-4.
           MOVE TL-INSOURS(4) TO QTL-INSOURS-4.
           IF ( TL-INSEFF-DATE(4) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEFF-DATE-4
           ELSE
              MOVE TL-INSEFF-DATE(4) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEFF-DATE-4.
           IF ( TL-INSEXP-DATE(4) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEXP-DATE-4
           ELSE
              MOVE TL-INSEXP-DATE(4) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEXP-DATE-4.
           MOVE TL-INSPREM(4) TO QTL-INSPREM-4.
           MOVE TL-INSCOMM(4) TO QTL-INSCOMM-4.
           MOVE TL-INSEARNINGS(4) TO QTL-INSEARNINGS-4.
           MOVE TL-INSANTICERN(4) TO QTL-INSANTICERN-4.

           MOVE TL-INSCOMP(5) TO QTL-INSCOMP-5.
           MOVE TL-INSOURS(5) TO QTL-INSOURS-5.
           IF ( TL-INSEFF-DATE(5) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEFF-DATE-5
           ELSE
              MOVE TL-INSEFF-DATE(5) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEFF-DATE-5.
           IF ( TL-INSEXP-DATE(5) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEXP-DATE-5
           ELSE
              MOVE TL-INSEXP-DATE(5) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEXP-DATE-5.
           MOVE TL-INSPREM(5) TO QTL-INSPREM-5.
           MOVE TL-INSCOMM(5) TO QTL-INSCOMM-5.
           MOVE TL-INSEARNINGS(5) TO QTL-INSEARNINGS-5.
           MOVE TL-INSANTICERN(5) TO QTL-INSANTICERN-5.

           MOVE TL-INSCOMP(6) TO QTL-INSCOMP-6.
           MOVE TL-INSOURS(6) TO QTL-INSOURS-6.
           IF ( TL-INSEFF-DATE(6) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEFF-DATE-6
           ELSE
              MOVE TL-INSEFF-DATE(6) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEFF-DATE-6.
           IF ( TL-INSEXP-DATE(6) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEXP-DATE-6
           ELSE
              MOVE TL-INSEXP-DATE(6) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEXP-DATE-6.
           MOVE TL-INSPREM(6) TO QTL-INSPREM-6.
           MOVE TL-INSCOMM(6) TO QTL-INSCOMM-6.
           MOVE TL-INSEARNINGS(6) TO QTL-INSEARNINGS-6.
           MOVE TL-INSANTICERN(6) TO QTL-INSANTICERN-6.

           MOVE TL-INSCOMP(7) TO QTL-INSCOMP-7.
           MOVE TL-INSOURS(7) TO QTL-INSOURS-7.
           IF ( TL-INSEFF-DATE(7) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEFF-DATE-7
           ELSE
              MOVE TL-INSEFF-DATE(7) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEFF-DATE-7.
           IF ( TL-INSEXP-DATE(7) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEXP-DATE-7
           ELSE
              MOVE TL-INSEXP-DATE(7) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEXP-DATE-7.
           MOVE TL-INSPREM(7) TO QTL-INSPREM-7.
           MOVE TL-INSCOMM(7) TO QTL-INSCOMM-7.
           MOVE TL-INSEARNINGS(7) TO QTL-INSEARNINGS-7.
           MOVE TL-INSANTICERN(7) TO QTL-INSANTICERN-7.

           MOVE TL-INSCOMP(8) TO QTL-INSCOMP-8.
           MOVE TL-INSOURS(8) TO QTL-INSOURS-8.
           IF ( TL-INSEFF-DATE(8) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEFF-DATE-8
           ELSE
              MOVE TL-INSEFF-DATE(8) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEFF-DATE-8.
           IF ( TL-INSEXP-DATE(8) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEXP-DATE-8
           ELSE
              MOVE TL-INSEXP-DATE(8) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEXP-DATE-8.
           MOVE TL-INSPREM(8) TO QTL-INSPREM-8.
           MOVE TL-INSCOMM(8) TO QTL-INSCOMM-8.
           MOVE TL-INSEARNINGS(8) TO QTL-INSEARNINGS-8.
           MOVE TL-INSANTICERN(8) TO QTL-INSANTICERN-8.

           MOVE TL-INSCOMP(9) TO QTL-INSCOMP-9.
           MOVE TL-INSOURS(9) TO QTL-INSOURS-9.
           IF ( TL-INSEFF-DATE(9) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEFF-DATE-9
           ELSE
              MOVE TL-INSEFF-DATE(9) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEFF-DATE-9.
           IF ( TL-INSEXP-DATE(9) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEXP-DATE-9
           ELSE
              MOVE TL-INSEXP-DATE(9) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEXP-DATE-9.
           MOVE TL-INSPREM(9) TO QTL-INSPREM-9.
           MOVE TL-INSCOMM(9) TO QTL-INSCOMM-9.
           MOVE TL-INSEARNINGS(9) TO QTL-INSEARNINGS-9.
           MOVE TL-INSANTICERN(9) TO QTL-INSANTICERN-9.

           MOVE TL-INSCOMP(10) TO QTL-INSCOMP-10.
           MOVE TL-INSOURS(10) TO QTL-INSOURS-10.
           IF ( TL-INSEFF-DATE(10) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEFF-DATE-10
           ELSE
              MOVE TL-INSEFF-DATE(10) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEFF-DATE-10.
           IF ( TL-INSEXP-DATE(10) = ZEROES )
              MOVE "1900-01-01" TO QTL-INSEXP-DATE-10
           ELSE
              MOVE TL-INSEXP-DATE(10) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-INSEXP-DATE-10.
           MOVE TL-INSPREM(10) TO QTL-INSPREM-10.
           MOVE TL-INSCOMM(10) TO QTL-INSCOMM-10.
           MOVE TL-INSEARNINGS(10) TO QTL-INSEARNINGS-10.
           MOVE TL-INSANTICERN(10) TO QTL-INSANTICERN-10.

           MOVE TL-DLNO TO QTL-DLNO.

           MOVE TL-EARNINGS(1) TO QTL-EARNINGS-1.
           MOVE TL-EARNINGS(2) TO QTL-EARNINGS-2.
           MOVE TL-EARNINGS(3) TO QTL-EARNINGS-3.
           MOVE TL-EARNINGS(4) TO QTL-EARNINGS-4.
           MOVE TL-EARNINGS(5) TO QTL-EARNINGS-5.
           MOVE TL-EARNINGS(6) TO QTL-EARNINGS-6.
           MOVE TL-EARNINGS(7) TO QTL-EARNINGS-7.
           MOVE TL-EARNINGS(8) TO QTL-EARNINGS-8.
           MOVE TL-EARNINGS(9) TO QTL-EARNINGS-9.
           MOVE TL-EARNINGS(10) TO QTL-EARNINGS-10.
           MOVE TL-ANTICERN(1) TO QTL-ANTICERN-1.
           MOVE TL-ANTICERN(2) TO QTL-ANTICERN-2.
           MOVE TL-ANTICERN(3) TO QTL-ANTICERN-3.
           MOVE TL-ANTICERN(4) TO QTL-ANTICERN-4.
           MOVE TL-ANTICERN(5) TO QTL-ANTICERN-5.
           MOVE TL-ANTICERN(6) TO QTL-ANTICERN-6.
           MOVE TL-ANTICERN(7) TO QTL-ANTICERN-7.
           MOVE TL-ANTICERN(8) TO QTL-ANTICERN-8.
           MOVE TL-ANTICERN(9) TO QTL-ANTICERN-9.
           MOVE TL-ANTICERN(10) TO QTL-ANTICERN-10.
           MOVE TL-PRLNNO TO QTL-PRLNNO.
           MOVE TL-PRBAL TO QTL-PRBAL.
           MOVE TL-PRINTDUE TO QTL-PRINTDUE.
           MOVE TL-PRFINCHGRB TO QTL-PRFINCHGRB.
           MOVE TL-PRINSRB TO QTL-PRINSRB.
           MOVE TL-LPEOD5-FG TO QTL-LPEOD5-FG.
           MOVE TL-STATE-WORKER TO QTL-STATE-WORKER.
           MOVE TL-RESTRICTD-ACCTNO TO QTL-RESTRICTD-ACCTNO.
           MOVE TL-SPRCLASS TO QTL-SPRCLASS.
           MOVE TL-USERID TO QTL-USERID.

           IF ( TL-POSTDATE = ZEROES )
              MOVE "1900-01-01" TO QTL-POSTDATE
           ELSE
              MOVE TL-POSTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-POSTDATE.

           MOVE TL-POSTTIME TO QTL-POSTTIME.
           MOVE TL-GIFTNET TO QTL-GIFTNET.
           MOVE TL-APR TO QTL-APR.
           MOVE TL-SP-LNCOST-FRMLA TO QTL-SP-LNCOST-FRMLA.
           MOVE TL-MIP-FG TO QTL-MIP-FG.
           MOVE TL-INS-TAX-COUNTY-CD TO QTL-INS-TAX-COUNTY-CD.
           MOVE TL-INS-TAX-CITY-CD TO QTL-INS-TAX-CITY-CD.
           MOVE TL-PRDLDISC-UNEARN TO QTL-PRDLDISC-UNEARN.
           MOVE TL-POINTS TO QTL-POINTS.
           MOVE TL-TAX-AMT TO QTL-TAX-AMT.
           MOVE TL-PBI-PBD-FG TO QTL-PBI-PBD-FG.
           MOVE TL-APPROVED-BY-USER TO QTL-APPROVED-BY-USER.
           MOVE TL-APPROVED-BY-CODE TO QTL-APPROVED-BY-CODE.

           IF ( TL-APPROVED-BY-DATE = ZEROES )
              MOVE "1900-01-01" TO QTL-APPROVED-BY-DATE
           ELSE
              MOVE TL-APPROVED-BY-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTL-APPROVED-BY-DATE.

           MOVE TL-APPROVED-BY-TIME TO QTL-APPROVED-BY-TIME.
           MOVE TL-DEBIT-CARD-AMT TO QTL-DEBIT-CARD-AMT.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-TRL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRL-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRL-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRLFILE NOT = "S" )
              MOVE TRL-PATH-BEG             TO TRL-SELECT-PATH-BEG
              MOVE TRL-PATH-FILE            TO TRL-SELECT-PATH-FILE
           ELSE
              MOVE TRL-PATH-SQL             TO TRL-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRL1-FILE.
           PERFORM LOAD-TRL1-FILE.
           PERFORM OPEN-IT.
           MOVE TRL-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRL1-FILE.
           PERFORM START-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-SELECT-PATH TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           IF ( TRL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRL1-FILE.

           IF ( QTRL1-WEND-LINE = ZEROES    ) OR
              ( QTRL1-WEND-LINE NOT NUMERIC )
              MOVE TRL-PATH-OWNBR        TO QTRL1-WBEG-BRNO
              MOVE TRL-PATH-OWNBR        TO QTRL1-WEND-BRNO
              MOVE TL-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WEND-DATE
              MOVE TL-CLASS             TO QTRL1-WBEG-CLASS
              MOVE TL-ACCTNO            TO QTRL1-WBEG-ACCTNO
              MOVE TL-LAWCODE           TO QTRL1-WBEG-LAWCODE
              MOVE TL-LINE              TO QTRL1-WBEG-LINE
              MOVE ALL "9"              TO QTRL1-WEND-CLASS
                                           QTRL1-WEND-ACCTNO
                                           QTRL1-WEND-LAWCODE
                                           QTRL1-WEND-LINE.


           EXEC SQL
            DECLARE TRL1_CURSOR CURSOR FOR
            SELECT TRLFILE.TL_BRNO,
                   ISNULL(CAST(TRLFILE.TL_DATE AS VARCHAR(10)),'0'),
                   TRLFILE.TL_CLASS,
                   TRLFILE.TL_ACCTNO,
                   TRLFILE.TL_LAWCODE,
                   TRLFILE.TL_LINE,
                   ISNULL(CAST(TRLFILE.TL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_SSNO_1,
                   ISNULL(TRLFILE.TL_MAKERCD_1,''),
                   TRLFILE.TL_SSNO_2,
                   ISNULL(TRLFILE.TL_MAKERCD_2,''),
                   TRLFILE.TL_SSNO_3,
                   ISNULL(TRLFILE.TL_MAKERCD_3,''),
                   TRLFILE.TL_SSNO_4,
                   ISNULL(TRLFILE.TL_MAKERCD_4,''),
                   ISNULL(TRLFILE.TL_PURCD,''),
                   ISNULL(TRLFILE.TL_SRCD,''),
                   ISNULL(TRLFILE.TL_COLLCD,''),
                   ISNULL(TRLFILE.TL_SERVACCT,''),
                   ISNULL(TRLFILE.TL_LOANTYPE,''),
                   TRLFILE.TL_SUBCLASS,
                   ISNULL(CAST(TRLFILE.TL_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INTDATE AS VARCHAR(10)),'0'),
                   TRLFILE.TL_PROCEEDS,
                   TRLFILE.TL_LNAMT,
                   TRLFILE.TL_FINCHG,
                   TRLFILE.TL_INTCHG,
                   TRLFILE.TL_SERCHG,
                   TRLFILE.TL_EXTCHG,
                   TRLFILE.TL_MAINTFEE,
                   TRLFILE.TL_ORGTERM,
                   TRLFILE.TL_1STPYAMT,
                   TRLFILE.TL_NOREGPY,
                   TRLFILE.TL_REGPYAMT,
                   TRLFILE.TL_LASTPYAMT,
                   TRLFILE.TL_PROCASH,
                   TRLFILE.TL_TILLNO,
                   ISNULL(TRLFILE.TL_FEECODE_1,''),
                   TRLFILE.TL_FEEAMT_1,
                   ISNULL(TRLFILE.TL_FEECODE_2,''),
                   TRLFILE.TL_FEEAMT_2,
                   ISNULL(TRLFILE.TL_FEECODE_3,''),
                   TRLFILE.TL_FEEAMT_3,
                   ISNULL(TRLFILE.TL_FEECODE_4,''),
                   TRLFILE.TL_FEEAMT_4,
                   ISNULL(TRLFILE.TL_FEECODE_5,''),
                   TRLFILE.TL_FEEAMT_5,
                   ISNULL(TRLFILE.TL_FEECODE_6,''),
                   TRLFILE.TL_FEEAMT_6,
                   ISNULL(TRLFILE.TL_FEECODE_7,''),
                   TRLFILE.TL_FEEAMT_7,
                   ISNULL(TRLFILE.TL_FEECODE_8,''),
                   TRLFILE.TL_FEEAMT_8,
                   ISNULL(TRLFILE.TL_FEECODE_9,''),
                   TRLFILE.TL_FEEAMT_9,
                   ISNULL(TRLFILE.TL_FEECODE_10,''),
                   TRLFILE.TL_FEEAMT_10,
                   ISNULL(TRLFILE.TL_FEECODE_11,''),
                   TRLFILE.TL_FEEAMT_11,
                   ISNULL(TRLFILE.TL_FEECODE_12,''),
                   TRLFILE.TL_FEEAMT_12,
                   ISNULL(TRLFILE.TL_FEECODE_13,''),
                   TRLFILE.TL_FEEAMT_13,
                   ISNULL(TRLFILE.TL_FEECODE_14,''),
                   TRLFILE.TL_FEEAMT_14,
                   ISNULL(TRLFILE.TL_FEECODE_15,''),
                   TRLFILE.TL_FEEAMT_15,
                   ISNULL(TRLFILE.TL_FEECODE_16,''),
                   TRLFILE.TL_FEEAMT_16,
                   ISNULL(TRLFILE.TL_FEECODE_17,''),
                   TRLFILE.TL_FEEAMT_17,
                   ISNULL(TRLFILE.TL_FEECODE_18,''),
                   TRLFILE.TL_FEEAMT_18,
                   ISNULL(TRLFILE.TL_FEECODE_19,''),
                   TRLFILE.TL_FEEAMT_19,
                   ISNULL(TRLFILE.TL_FEECODE_20,''),
                   TRLFILE.TL_FEEAMT_20,
                   ISNULL(TRLFILE.TL_INSURED,''),
                   ISNULL(TRLFILE.TL_INSTYPES,''),
                   ISNULL(TRLFILE.TL_INSELIGIBLE,''),
                   TRLFILE.TL_INSCOMP_1,
                   ISNULL(TRLFILE.TL_INSOURS_1,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_1,
                   TRLFILE.TL_INSCOMM_1,
                   TRLFILE.TL_INSEARNINGS_1,
                   TRLFILE.TL_INSANTICERN_1,
                   TRLFILE.TL_INSCOMP_2,
                   ISNULL(TRLFILE.TL_INSOURS_2,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_2,
                   TRLFILE.TL_INSCOMM_2,
                   TRLFILE.TL_INSEARNINGS_2,
                   TRLFILE.TL_INSANTICERN_2,
                   TRLFILE.TL_INSCOMP_3,
                   ISNULL(TRLFILE.TL_INSOURS_3,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_3,
                   TRLFILE.TL_INSCOMM_3,
                   TRLFILE.TL_INSEARNINGS_3,
                   TRLFILE.TL_INSANTICERN_3,
                   TRLFILE.TL_INSCOMP_4,
                   ISNULL(TRLFILE.TL_INSOURS_4,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_4,
                   TRLFILE.TL_INSCOMM_4,
                   TRLFILE.TL_INSEARNINGS_4,
                   TRLFILE.TL_INSANTICERN_4,
                   TRLFILE.TL_INSCOMP_5,
                   ISNULL(TRLFILE.TL_INSOURS_5,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_5,
                   TRLFILE.TL_INSCOMM_5,
                   TRLFILE.TL_INSEARNINGS_5,
                   TRLFILE.TL_INSANTICERN_5,
                   TRLFILE.TL_INSCOMP_6,
                   ISNULL(TRLFILE.TL_INSOURS_6,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_6,
                   TRLFILE.TL_INSCOMM_6,
                   TRLFILE.TL_INSEARNINGS_6,
                   TRLFILE.TL_INSANTICERN_6,
                   TRLFILE.TL_INSCOMP_7,
                   ISNULL(TRLFILE.TL_INSOURS_7,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_7,
                   TRLFILE.TL_INSCOMM_7,
                   TRLFILE.TL_INSEARNINGS_7,
                   TRLFILE.TL_INSANTICERN_7,
                   TRLFILE.TL_INSCOMP_8,
                   ISNULL(TRLFILE.TL_INSOURS_8,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_8,
                   TRLFILE.TL_INSCOMM_8,
                   TRLFILE.TL_INSEARNINGS_8,
                   TRLFILE.TL_INSANTICERN_8,
                   TRLFILE.TL_INSCOMP_9,
                   ISNULL(TRLFILE.TL_INSOURS_9,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_9,
                   TRLFILE.TL_INSCOMM_9,
                   TRLFILE.TL_INSEARNINGS_9,
                   TRLFILE.TL_INSANTICERN_9,
                   TRLFILE.TL_INSCOMP_10,
                   ISNULL(TRLFILE.TL_INSOURS_10,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_10,
                   TRLFILE.TL_INSCOMM_10,
                   TRLFILE.TL_INSEARNINGS_10,
                   TRLFILE.TL_INSANTICERN_10,
                   TRLFILE.TL_DLNO,
                   TRLFILE.TL_EARNINGS_1,
                   TRLFILE.TL_EARNINGS_2,
                   TRLFILE.TL_EARNINGS_3,
                   TRLFILE.TL_EARNINGS_4,
                   TRLFILE.TL_EARNINGS_5,
                   TRLFILE.TL_EARNINGS_6,
                   TRLFILE.TL_EARNINGS_7,
                   TRLFILE.TL_EARNINGS_8,
                   TRLFILE.TL_EARNINGS_9,
                   TRLFILE.TL_EARNINGS_10,
                   TRLFILE.TL_ANTICERN_1,
                   TRLFILE.TL_ANTICERN_2,
                   TRLFILE.TL_ANTICERN_3,
                   TRLFILE.TL_ANTICERN_4,
                   TRLFILE.TL_ANTICERN_5,
                   TRLFILE.TL_ANTICERN_6,
                   TRLFILE.TL_ANTICERN_7,
                   TRLFILE.TL_ANTICERN_8,
                   TRLFILE.TL_ANTICERN_9,
                   TRLFILE.TL_ANTICERN_10,
                   TRLFILE.TL_PRLNNO,
                   TRLFILE.TL_PRBAL,
                   TRLFILE.TL_PRINTDUE,
                   TRLFILE.TL_PRFINCHGRB,
                   TRLFILE.TL_PRINSRB,
                   ISNULL(TRLFILE.TL_LPEOD5_FG,''),
                   TRLFILE.TL_STATE_WORKER,
                   TRLFILE.TL_RESTRICTD_ACCTNO,
                   TRLFILE.TL_SPRCLASS,
                   ISNULL(TRLFILE.TL_USERID,''),
                   ISNULL(CAST(TRLFILE.TL_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_POSTTIME,
                   ISNULL(TRLFILE.TL_GIFTNET,''),
                   TRLFILE.TL_APR,
                   ISNULL(TRLFILE.TL_SP_LNCOST_FRMLA,''),
                   ISNULL(TRLFILE.TL_MIP_FG,''),
                   ISNULL(TRLFILE.TL_INS_TAX_COUNTY_CD,''),
                   ISNULL(TRLFILE.TL_INS_TAX_CITY_CD,''),
                   TRLFILE.TL_PRDLDISC_UNEARN,
                   TRLFILE.TL_POINTS,
                   TRLFILE.TL_TAX_AMT,
                   ISNULL(TRLFILE.TL_PBI_PBD_FG,''),
                   ISNULL(TRLFILE.TL_APPROVED_BY_USER,''),
                   ISNULL(TRLFILE.TL_APPROVED_BY_CODE,''),
                   ISNULL(CAST(TRLFILE.TL_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_APPROVED_BY_TIME,
                   TRLFILE.TL_DEBIT_CARD_AMT
            FROM DBO.TRLFILE
            WHERE TRLFILE.TL_BRNO >= :QTRL1-WBEG-BRNO
              AND TRLFILE.TL_BRNO <= :QTRL1-WEND-BRNO
              AND CAST(TRLFILE.TL_DATE AS VARCHAR(10))
                          >= :QTRL1-WBEG-DATE
              AND CAST(TRLFILE.TL_DATE AS VARCHAR(10))
                          <= :QTRL1-WEND-DATE
              AND TRLFILE.TL_CLASS >= :QTRL1-WBEG-CLASS
              AND TRLFILE.TL_CLASS <= :QTRL1-WEND-CLASS
              AND TRLFILE.TL_ACCTNO >= :QTRL1-WBEG-ACCTNO
              AND TRLFILE.TL_ACCTNO <= :QTRL1-WEND-ACCTNO
              AND TRLFILE.TL_LAWCODE >= :QTRL1-WBEG-LAWCODE
              AND TRLFILE.TL_LAWCODE <= :QTRL1-WEND-LAWCODE
              AND TRLFILE.TL_LINE >= :QTRL1-WBEG-LINE
              AND TRLFILE.TL_LINE <= :QTRL1-WEND-LINE
            ORDER BY TRLFILE.TL_BRNO,
                     TRLFILE.TL_DATE,
                     TRLFILE.TL_CLASS,
                     TRLFILE.TL_ACCTNO,
                     TRLFILE.TL_LAWCODE,
                     TRLFILE.TL_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRL1-WBEG-BRNO
                          QTRL1-WEND-BRNO
                          QTRL1-WBEG-CLASS
                          QTRL1-WEND-CLASS
                          QTRL1-WBEG-ACCTNO
                          QTRL1-WEND-ACCTNO
                          QTRL1-WBEG-LAWCODE
                          QTRL1-WEND-LAWCODE
                          QTRL1-WBEG-LINE
                          QTRL1-WEND-LINE.
           MOVE SPACES TO QTRL1-WBEG-DATE
                          QTRL1-WEND-DATE.


           MOVE STAT---GOOD TO TRL1-CURSOR-STAT.


      *-----------------------------------------------------------------
       READ-TRL1-FILE.
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO. 
           MOVE TRL-SELECT-PATH TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           MOVE TL-BRNO             TO QTL-BRNO.
           MOVE TL-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTL-DATE.
           MOVE TL-CLASS            TO QTL-CLASS.
           MOVE TL-ACCTNO           TO QTL-ACCTNO.
           MOVE TL-LAWCODE          TO QTL-LAWCODE.
           MOVE TL-LINE             TO QTL-LINE.

           EXEC SQL
            SELECT TRLFILE.TL_BRNO,
                   ISNULL(CAST(TRLFILE.TL_DATE AS VARCHAR(10)),'0'),
                   TRLFILE.TL_CLASS,
                   TRLFILE.TL_ACCTNO,
                   TRLFILE.TL_LAWCODE,
                   TRLFILE.TL_LINE,
                   ISNULL(CAST(TRLFILE.TL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_SSNO_1,
                   ISNULL(TRLFILE.TL_MAKERCD_1,''),
                   TRLFILE.TL_SSNO_2,
                   ISNULL(TRLFILE.TL_MAKERCD_2,''),
                   TRLFILE.TL_SSNO_3,
                   ISNULL(TRLFILE.TL_MAKERCD_3,''),
                   TRLFILE.TL_SSNO_4,
                   ISNULL(TRLFILE.TL_MAKERCD_4,''),
                   ISNULL(TRLFILE.TL_PURCD,''),
                   ISNULL(TRLFILE.TL_SRCD,''),
                   ISNULL(TRLFILE.TL_COLLCD,''),
                   ISNULL(TRLFILE.TL_SERVACCT,''),
                   ISNULL(TRLFILE.TL_LOANTYPE,''),
                   TRLFILE.TL_SUBCLASS,
                   ISNULL(CAST(TRLFILE.TL_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INTDATE AS VARCHAR(10)),'0'),
                   TRLFILE.TL_PROCEEDS,
                   TRLFILE.TL_LNAMT,
                   TRLFILE.TL_FINCHG,
                   TRLFILE.TL_INTCHG,
                   TRLFILE.TL_SERCHG,
                   TRLFILE.TL_EXTCHG,
                   TRLFILE.TL_MAINTFEE,
                   TRLFILE.TL_ORGTERM,
                   TRLFILE.TL_1STPYAMT,
                   TRLFILE.TL_NOREGPY,
                   TRLFILE.TL_REGPYAMT,
                   TRLFILE.TL_LASTPYAMT,
                   TRLFILE.TL_PROCASH,
                   TRLFILE.TL_TILLNO,
                   ISNULL(TRLFILE.TL_FEECODE_1,''),
                   TRLFILE.TL_FEEAMT_1,
                   ISNULL(TRLFILE.TL_FEECODE_2,''),
                   TRLFILE.TL_FEEAMT_2,
                   ISNULL(TRLFILE.TL_FEECODE_3,''),
                   TRLFILE.TL_FEEAMT_3,
                   ISNULL(TRLFILE.TL_FEECODE_4,''),
                   TRLFILE.TL_FEEAMT_4,
                   ISNULL(TRLFILE.TL_FEECODE_5,''),
                   TRLFILE.TL_FEEAMT_5,
                   ISNULL(TRLFILE.TL_FEECODE_6,''),
                   TRLFILE.TL_FEEAMT_6,
                   ISNULL(TRLFILE.TL_FEECODE_7,''),
                   TRLFILE.TL_FEEAMT_7,
                   ISNULL(TRLFILE.TL_FEECODE_8,''),
                   TRLFILE.TL_FEEAMT_8,
                   ISNULL(TRLFILE.TL_FEECODE_9,''),
                   TRLFILE.TL_FEEAMT_9,
                   ISNULL(TRLFILE.TL_FEECODE_10,''),
                   TRLFILE.TL_FEEAMT_10,
                   ISNULL(TRLFILE.TL_FEECODE_11,''),
                   TRLFILE.TL_FEEAMT_11,
                   ISNULL(TRLFILE.TL_FEECODE_12,''),
                   TRLFILE.TL_FEEAMT_12,
                   ISNULL(TRLFILE.TL_FEECODE_13,''),
                   TRLFILE.TL_FEEAMT_13,
                   ISNULL(TRLFILE.TL_FEECODE_14,''),
                   TRLFILE.TL_FEEAMT_14,
                   ISNULL(TRLFILE.TL_FEECODE_15,''),
                   TRLFILE.TL_FEEAMT_15,
                   ISNULL(TRLFILE.TL_FEECODE_16,''),
                   TRLFILE.TL_FEEAMT_16,
                   ISNULL(TRLFILE.TL_FEECODE_17,''),
                   TRLFILE.TL_FEEAMT_17,
                   ISNULL(TRLFILE.TL_FEECODE_18,''),
                   TRLFILE.TL_FEEAMT_18,
                   ISNULL(TRLFILE.TL_FEECODE_19,''),
                   TRLFILE.TL_FEEAMT_19,
                   ISNULL(TRLFILE.TL_FEECODE_20,''),
                   TRLFILE.TL_FEEAMT_20,
                   ISNULL(TRLFILE.TL_INSURED,''),
                   ISNULL(TRLFILE.TL_INSTYPES,''),
                   ISNULL(TRLFILE.TL_INSELIGIBLE,''),
                   TRLFILE.TL_INSCOMP_1,
                   ISNULL(TRLFILE.TL_INSOURS_1,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_1,
                   TRLFILE.TL_INSCOMM_1,
                   TRLFILE.TL_INSEARNINGS_1,
                   TRLFILE.TL_INSANTICERN_1,
                   TRLFILE.TL_INSCOMP_2,
                   ISNULL(TRLFILE.TL_INSOURS_2,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_2,
                   TRLFILE.TL_INSCOMM_2,
                   TRLFILE.TL_INSEARNINGS_2,
                   TRLFILE.TL_INSANTICERN_2,
                   TRLFILE.TL_INSCOMP_3,
                   ISNULL(TRLFILE.TL_INSOURS_3,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_3,
                   TRLFILE.TL_INSCOMM_3,
                   TRLFILE.TL_INSEARNINGS_3,
                   TRLFILE.TL_INSANTICERN_3,
                   TRLFILE.TL_INSCOMP_4,
                   ISNULL(TRLFILE.TL_INSOURS_4,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_4,
                   TRLFILE.TL_INSCOMM_4,
                   TRLFILE.TL_INSEARNINGS_4,
                   TRLFILE.TL_INSANTICERN_4,
                   TRLFILE.TL_INSCOMP_5,
                   ISNULL(TRLFILE.TL_INSOURS_5,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_5,
                   TRLFILE.TL_INSCOMM_5,
                   TRLFILE.TL_INSEARNINGS_5,
                   TRLFILE.TL_INSANTICERN_5,
                   TRLFILE.TL_INSCOMP_6,
                   ISNULL(TRLFILE.TL_INSOURS_6,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_6,
                   TRLFILE.TL_INSCOMM_6,
                   TRLFILE.TL_INSEARNINGS_6,
                   TRLFILE.TL_INSANTICERN_6,
                   TRLFILE.TL_INSCOMP_7,
                   ISNULL(TRLFILE.TL_INSOURS_7,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_7,
                   TRLFILE.TL_INSCOMM_7,
                   TRLFILE.TL_INSEARNINGS_7,
                   TRLFILE.TL_INSANTICERN_7,
                   TRLFILE.TL_INSCOMP_8,
                   ISNULL(TRLFILE.TL_INSOURS_8,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_8,
                   TRLFILE.TL_INSCOMM_8,
                   TRLFILE.TL_INSEARNINGS_8,
                   TRLFILE.TL_INSANTICERN_8,
                   TRLFILE.TL_INSCOMP_9,
                   ISNULL(TRLFILE.TL_INSOURS_9,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_9,
                   TRLFILE.TL_INSCOMM_9,
                   TRLFILE.TL_INSEARNINGS_9,
                   TRLFILE.TL_INSANTICERN_9,
                   TRLFILE.TL_INSCOMP_10,
                   ISNULL(TRLFILE.TL_INSOURS_10,''),
                   ISNULL(CAST(TRLFILE.TL_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRLFILE.TL_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_INSPREM_10,
                   TRLFILE.TL_INSCOMM_10,
                   TRLFILE.TL_INSEARNINGS_10,
                   TRLFILE.TL_INSANTICERN_10,
                   TRLFILE.TL_DLNO,
                   TRLFILE.TL_EARNINGS_1,
                   TRLFILE.TL_EARNINGS_2,
                   TRLFILE.TL_EARNINGS_3,
                   TRLFILE.TL_EARNINGS_4,
                   TRLFILE.TL_EARNINGS_5,
                   TRLFILE.TL_EARNINGS_6,
                   TRLFILE.TL_EARNINGS_7,
                   TRLFILE.TL_EARNINGS_8,
                   TRLFILE.TL_EARNINGS_9,
                   TRLFILE.TL_EARNINGS_10,
                   TRLFILE.TL_ANTICERN_1,
                   TRLFILE.TL_ANTICERN_2,
                   TRLFILE.TL_ANTICERN_3,
                   TRLFILE.TL_ANTICERN_4,
                   TRLFILE.TL_ANTICERN_5,
                   TRLFILE.TL_ANTICERN_6,
                   TRLFILE.TL_ANTICERN_7,
                   TRLFILE.TL_ANTICERN_8,
                   TRLFILE.TL_ANTICERN_9,
                   TRLFILE.TL_ANTICERN_10,
                   TRLFILE.TL_PRLNNO,
                   TRLFILE.TL_PRBAL,
                   TRLFILE.TL_PRINTDUE,
                   TRLFILE.TL_PRFINCHGRB,
                   TRLFILE.TL_PRINSRB,
                   ISNULL(TRLFILE.TL_LPEOD5_FG,''),
                   TRLFILE.TL_STATE_WORKER,
                   TRLFILE.TL_RESTRICTD_ACCTNO,
                   TRLFILE.TL_SPRCLASS,
                   ISNULL(TRLFILE.TL_USERID,''),
                   ISNULL(CAST(TRLFILE.TL_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_POSTTIME,
                   ISNULL(TRLFILE.TL_GIFTNET,''),
                   TRLFILE.TL_APR,
                   ISNULL(TRLFILE.TL_SP_LNCOST_FRMLA,''),
                   ISNULL(TRLFILE.TL_MIP_FG,''),
                   ISNULL(TRLFILE.TL_INS_TAX_COUNTY_CD,''),
                   ISNULL(TRLFILE.TL_INS_TAX_CITY_CD,''),
                   TRLFILE.TL_PRDLDISC_UNEARN,
                   TRLFILE.TL_POINTS,
                   TRLFILE.TL_TAX_AMT,
                   ISNULL(TRLFILE.TL_PBI_PBD_FG,''),
                   ISNULL(TRLFILE.TL_APPROVED_BY_USER,''),
                   ISNULL(TRLFILE.TL_APPROVED_BY_CODE,''),
                   ISNULL(CAST(TRLFILE.TL_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRLFILE.TL_APPROVED_BY_TIME,
                   TRLFILE.TL_DEBIT_CARD_AMT
            INTO :QTRL-REC
            FROM DBO.TRLFILE
            WHERE TRLFILE.TL_BRNO = :QTL-BRNO
              AND TRLFILE.TL_DATE = :QTL-DATE
              AND TRLFILE.TL_CLASS = :QTL-CLASS
              AND TRLFILE.TL_ACCTNO = :QTL-ACCTNO
              AND TRLFILE.TL_LAWCODE = :QTL-LAWCODE
              AND TRLFILE.TL_LINE = :QTL-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRL-FIELDS.

      *-----------------------------------------------------------------
       READ-TRL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-SELECT-PATH TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           INITIALIZE QTRL-REC.

           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRL1_CURSOR INTO :QTRL-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRL-FIELDS.

      *-----------------------------------------------------------------
       WRITE-TRL1-FILE.

           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           ACCEPT TL-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TRL-SELECT-PATH TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           PERFORM SET-TRL-FIELDS.

           EXEC SQL
            INSERT INTO DBO.TRLFILE
             (TRLFILE.TL_BRNO,
              TRLFILE.TL_DATE,
              TRLFILE.TL_CLASS,
              TRLFILE.TL_ACCTNO,
              TRLFILE.TL_LAWCODE,
              TRLFILE.TL_LINE,
              TRLFILE.TL_LTOUCH_DATE,
              TRLFILE.TL_SSNO_1,
              TRLFILE.TL_MAKERCD_1,
              TRLFILE.TL_SSNO_2,
              TRLFILE.TL_MAKERCD_2,
              TRLFILE.TL_SSNO_3,
              TRLFILE.TL_MAKERCD_3,
              TRLFILE.TL_SSNO_4,
              TRLFILE.TL_MAKERCD_4,
              TRLFILE.TL_PURCD,
              TRLFILE.TL_SRCD,
              TRLFILE.TL_COLLCD,
              TRLFILE.TL_SERVACCT,
              TRLFILE.TL_LOANTYPE,
              TRLFILE.TL_SUBCLASS,
              TRLFILE.TL_LOANDATE,
              TRLFILE.TL_INTDATE,
              TRLFILE.TL_PROCEEDS,
              TRLFILE.TL_LNAMT,
              TRLFILE.TL_FINCHG,
              TRLFILE.TL_INTCHG,
              TRLFILE.TL_SERCHG,
              TRLFILE.TL_EXTCHG,
              TRLFILE.TL_MAINTFEE,
              TRLFILE.TL_ORGTERM,
              TRLFILE.TL_1STPYAMT,
              TRLFILE.TL_NOREGPY,
              TRLFILE.TL_REGPYAMT,
              TRLFILE.TL_LASTPYAMT,
              TRLFILE.TL_PROCASH,
              TRLFILE.TL_TILLNO,
              TRLFILE.TL_FEECODE_1,
              TRLFILE.TL_FEEAMT_1,
              TRLFILE.TL_FEECODE_2,
              TRLFILE.TL_FEEAMT_2,
              TRLFILE.TL_FEECODE_3,
              TRLFILE.TL_FEEAMT_3,
              TRLFILE.TL_FEECODE_4,
              TRLFILE.TL_FEEAMT_4,
              TRLFILE.TL_FEECODE_5,
              TRLFILE.TL_FEEAMT_5,
              TRLFILE.TL_FEECODE_6,
              TRLFILE.TL_FEEAMT_6,
              TRLFILE.TL_FEECODE_7,
              TRLFILE.TL_FEEAMT_7,
              TRLFILE.TL_FEECODE_8,
              TRLFILE.TL_FEEAMT_8,
              TRLFILE.TL_FEECODE_9,
              TRLFILE.TL_FEEAMT_9,
              TRLFILE.TL_FEECODE_10,
              TRLFILE.TL_FEEAMT_10,
              TRLFILE.TL_FEECODE_11,
              TRLFILE.TL_FEEAMT_11,
              TRLFILE.TL_FEECODE_12,
              TRLFILE.TL_FEEAMT_12,
              TRLFILE.TL_FEECODE_13,
              TRLFILE.TL_FEEAMT_13,
              TRLFILE.TL_FEECODE_14,
              TRLFILE.TL_FEEAMT_14,
              TRLFILE.TL_FEECODE_15,
              TRLFILE.TL_FEEAMT_15,
              TRLFILE.TL_FEECODE_16,
              TRLFILE.TL_FEEAMT_16,
              TRLFILE.TL_FEECODE_17,
              TRLFILE.TL_FEEAMT_17,
              TRLFILE.TL_FEECODE_18,
              TRLFILE.TL_FEEAMT_18,
              TRLFILE.TL_FEECODE_19,
              TRLFILE.TL_FEEAMT_19,
              TRLFILE.TL_FEECODE_20,
              TRLFILE.TL_FEEAMT_20,
              TRLFILE.TL_INSURED,
              TRLFILE.TL_INSTYPES,
              TRLFILE.TL_INSELIGIBLE,
              TRLFILE.TL_INSCOMP_1,
              TRLFILE.TL_INSOURS_1,
              TRLFILE.TL_INSEFF_DATE_1,
              TRLFILE.TL_INSEXP_DATE_1,
              TRLFILE.TL_INSPREM_1,
              TRLFILE.TL_INSCOMM_1,
              TRLFILE.TL_INSEARNINGS_1,
              TRLFILE.TL_INSANTICERN_1,
              TRLFILE.TL_INSCOMP_2,
              TRLFILE.TL_INSOURS_2,
              TRLFILE.TL_INSEFF_DATE_2,
              TRLFILE.TL_INSEXP_DATE_2,
              TRLFILE.TL_INSPREM_2,
              TRLFILE.TL_INSCOMM_2,
              TRLFILE.TL_INSEARNINGS_2,
              TRLFILE.TL_INSANTICERN_2,
              TRLFILE.TL_INSCOMP_3,
              TRLFILE.TL_INSOURS_3,
              TRLFILE.TL_INSEFF_DATE_3,
              TRLFILE.TL_INSEXP_DATE_3,
              TRLFILE.TL_INSPREM_3,
              TRLFILE.TL_INSCOMM_3,
              TRLFILE.TL_INSEARNINGS_3,
              TRLFILE.TL_INSANTICERN_3,
              TRLFILE.TL_INSCOMP_4,
              TRLFILE.TL_INSOURS_4,
              TRLFILE.TL_INSEFF_DATE_4,
              TRLFILE.TL_INSEXP_DATE_4,
              TRLFILE.TL_INSPREM_4,
              TRLFILE.TL_INSCOMM_4,
              TRLFILE.TL_INSEARNINGS_4,
              TRLFILE.TL_INSANTICERN_4,
              TRLFILE.TL_INSCOMP_5,
              TRLFILE.TL_INSOURS_5,
              TRLFILE.TL_INSEFF_DATE_5,
              TRLFILE.TL_INSEXP_DATE_5,
              TRLFILE.TL_INSPREM_5,
              TRLFILE.TL_INSCOMM_5,
              TRLFILE.TL_INSEARNINGS_5,
              TRLFILE.TL_INSANTICERN_5,
              TRLFILE.TL_INSCOMP_6,
              TRLFILE.TL_INSOURS_6,
              TRLFILE.TL_INSEFF_DATE_6,
              TRLFILE.TL_INSEXP_DATE_6,
              TRLFILE.TL_INSPREM_6,
              TRLFILE.TL_INSCOMM_6,
              TRLFILE.TL_INSEARNINGS_6,
              TRLFILE.TL_INSANTICERN_6,
              TRLFILE.TL_INSCOMP_7,
              TRLFILE.TL_INSOURS_7,
              TRLFILE.TL_INSEFF_DATE_7,
              TRLFILE.TL_INSEXP_DATE_7,
              TRLFILE.TL_INSPREM_7,
              TRLFILE.TL_INSCOMM_7,
              TRLFILE.TL_INSEARNINGS_7,
              TRLFILE.TL_INSANTICERN_7,
              TRLFILE.TL_INSCOMP_8,
              TRLFILE.TL_INSOURS_8,
              TRLFILE.TL_INSEFF_DATE_8,
              TRLFILE.TL_INSEXP_DATE_8,
              TRLFILE.TL_INSPREM_8,
              TRLFILE.TL_INSCOMM_8,
              TRLFILE.TL_INSEARNINGS_8,
              TRLFILE.TL_INSANTICERN_8,
              TRLFILE.TL_INSCOMP_9,
              TRLFILE.TL_INSOURS_9,
              TRLFILE.TL_INSEFF_DATE_9,
              TRLFILE.TL_INSEXP_DATE_9,
              TRLFILE.TL_INSPREM_9,
              TRLFILE.TL_INSCOMM_9,
              TRLFILE.TL_INSEARNINGS_9,
              TRLFILE.TL_INSANTICERN_9,
              TRLFILE.TL_INSCOMP_10,
              TRLFILE.TL_INSOURS_10,
              TRLFILE.TL_INSEFF_DATE_10,
              TRLFILE.TL_INSEXP_DATE_10,
              TRLFILE.TL_INSPREM_10,
              TRLFILE.TL_INSCOMM_10,
              TRLFILE.TL_INSEARNINGS_10,
              TRLFILE.TL_INSANTICERN_10,
              TRLFILE.TL_DLNO,
              TRLFILE.TL_EARNINGS_1,
              TRLFILE.TL_EARNINGS_2,
              TRLFILE.TL_EARNINGS_3,
              TRLFILE.TL_EARNINGS_4,
              TRLFILE.TL_EARNINGS_5,
              TRLFILE.TL_EARNINGS_6,
              TRLFILE.TL_EARNINGS_7,
              TRLFILE.TL_EARNINGS_8,
              TRLFILE.TL_EARNINGS_9,
              TRLFILE.TL_EARNINGS_10,
              TRLFILE.TL_ANTICERN_1,
              TRLFILE.TL_ANTICERN_2,
              TRLFILE.TL_ANTICERN_3,
              TRLFILE.TL_ANTICERN_4,
              TRLFILE.TL_ANTICERN_5,
              TRLFILE.TL_ANTICERN_6,
              TRLFILE.TL_ANTICERN_7,
              TRLFILE.TL_ANTICERN_8,
              TRLFILE.TL_ANTICERN_9,
              TRLFILE.TL_ANTICERN_10,
              TRLFILE.TL_PRLNNO,
              TRLFILE.TL_PRBAL,
              TRLFILE.TL_PRINTDUE,
              TRLFILE.TL_PRFINCHGRB,
              TRLFILE.TL_PRINSRB,
              TRLFILE.TL_LPEOD5_FG,
              TRLFILE.TL_STATE_WORKER,
              TRLFILE.TL_RESTRICTD_ACCTNO,
              TRLFILE.TL_SPRCLASS,
              TRLFILE.TL_USERID,
              TRLFILE.TL_POSTDATE,
              TRLFILE.TL_POSTTIME,
              TRLFILE.TL_GIFTNET,
              TRLFILE.TL_APR,
              TRLFILE.TL_SP_LNCOST_FRMLA,
              TRLFILE.TL_MIP_FG,
              TRLFILE.TL_INS_TAX_COUNTY_CD,
              TRLFILE.TL_INS_TAX_CITY_CD,
              TRLFILE.TL_PRDLDISC_UNEARN,
              TRLFILE.TL_POINTS,
              TRLFILE.TL_TAX_AMT,
              TRLFILE.TL_PBI_PBD_FG,
              TRLFILE.TL_APPROVED_BY_USER,
              TRLFILE.TL_APPROVED_BY_CODE,
              TRLFILE.TL_APPROVED_BY_DATE,
              TRLFILE.TL_APPROVED_BY_TIME,
              TRLFILE.TL_DEBIT_CARD_AMT)
            VALUES
             (:QTL-BRNO,
              NULLIF(:QTL-DATE,'1900-01-01'),
              :QTL-CLASS,
              :QTL-ACCTNO,
              :QTL-LAWCODE,
              :QTL-LINE,
              NULLIF(:QTL-LTOUCH-DATE,'1900-01-01'),
              :QTL-SSNO-1,
              :QTL-MAKERCD-1,
              :QTL-SSNO-2,
              :QTL-MAKERCD-2,
              :QTL-SSNO-3,
              :QTL-MAKERCD-3,
              :QTL-SSNO-4,
              :QTL-MAKERCD-4,
              :QTL-PURCD,
              :QTL-SRCD,
              :QTL-COLLCD,
              :QTL-SERVACCT,
              :QTL-LOANTYPE,
              :QTL-SUBCLASS,
              NULLIF(:QTL-LOANDATE,'1900-01-01'),
              NULLIF(:QTL-INTDATE,'1900-01-01'),
              :QTL-PROCEEDS,
              :QTL-LNAMT,
              :QTL-FINCHG,
              :QTL-INTCHG,
              :QTL-SERCHG,
              :QTL-EXTCHG,
              :QTL-MAINTFEE,
              :QTL-ORGTERM,
              :QTL-1STPYAMT,
              :QTL-NOREGPY,
              :QTL-REGPYAMT,
              :QTL-LASTPYAMT,
              :QTL-PROCASH,
              :QTL-TILLNO,
              :QTL-FEECODE-1,
              :QTL-FEEAMT-1,
              :QTL-FEECODE-2,
              :QTL-FEEAMT-2,
              :QTL-FEECODE-3,
              :QTL-FEEAMT-3,
              :QTL-FEECODE-4,
              :QTL-FEEAMT-4,
              :QTL-FEECODE-5,
              :QTL-FEEAMT-5,
              :QTL-FEECODE-6,
              :QTL-FEEAMT-6,
              :QTL-FEECODE-7,
              :QTL-FEEAMT-7,
              :QTL-FEECODE-8,
              :QTL-FEEAMT-8,
              :QTL-FEECODE-9,
              :QTL-FEEAMT-9,
              :QTL-FEECODE-10,
              :QTL-FEEAMT-10,
              :QTL-FEECODE-11,
              :QTL-FEEAMT-11,
              :QTL-FEECODE-12,
              :QTL-FEEAMT-12,
              :QTL-FEECODE-13,
              :QTL-FEEAMT-13,
              :QTL-FEECODE-14,
              :QTL-FEEAMT-14,
              :QTL-FEECODE-15,
              :QTL-FEEAMT-15,
              :QTL-FEECODE-16,
              :QTL-FEEAMT-16,
              :QTL-FEECODE-17,
              :QTL-FEEAMT-17,
              :QTL-FEECODE-18,
              :QTL-FEEAMT-18,
              :QTL-FEECODE-19,
              :QTL-FEEAMT-19,
              :QTL-FEECODE-20,
              :QTL-FEEAMT-20,
              :QTL-INSURED,
              :QTL-INSTYPES,
              :QTL-INSELIGIBLE,
              :QTL-INSCOMP-1,
              :QTL-INSOURS-1,
              NULLIF(:QTL-INSEFF-DATE-1,'1900-01-01'),
              NULLIF(:QTL-INSEXP-DATE-1,'1900-01-01'),
              :QTL-INSPREM-1,
              :QTL-INSCOMM-1,
              :QTL-INSEARNINGS-1,
              :QTL-INSANTICERN-1,
              :QTL-INSCOMP-2,
              :QTL-INSOURS-2,
              NULLIF(:QTL-INSEFF-DATE-2,'1900-01-01'),
              NULLIF(:QTL-INSEXP-DATE-2,'1900-01-01'),
              :QTL-INSPREM-2,
              :QTL-INSCOMM-2,
              :QTL-INSEARNINGS-2,
              :QTL-INSANTICERN-2,
              :QTL-INSCOMP-3,
              :QTL-INSOURS-3,
              NULLIF(:QTL-INSEFF-DATE-3,'1900-01-01'),
              NULLIF(:QTL-INSEXP-DATE-3,'1900-01-01'),
              :QTL-INSPREM-3,
              :QTL-INSCOMM-3,
              :QTL-INSEARNINGS-3,
              :QTL-INSANTICERN-3,
              :QTL-INSCOMP-4,
              :QTL-INSOURS-4,
              NULLIF(:QTL-INSEFF-DATE-4,'1900-01-01'),
              NULLIF(:QTL-INSEXP-DATE-4,'1900-01-01'),
              :QTL-INSPREM-4,
              :QTL-INSCOMM-4,
              :QTL-INSEARNINGS-4,
              :QTL-INSANTICERN-4,
              :QTL-INSCOMP-5,
              :QTL-INSOURS-5,
              NULLIF(:QTL-INSEFF-DATE-5,'1900-01-01'),
              NULLIF(:QTL-INSEXP-DATE-5,'1900-01-01'),
              :QTL-INSPREM-5,
              :QTL-INSCOMM-5,
              :QTL-INSEARNINGS-5,
              :QTL-INSANTICERN-5,
              :QTL-INSCOMP-6,
              :QTL-INSOURS-6,
              NULLIF(:QTL-INSEFF-DATE-6,'1900-01-01'),
              NULLIF(:QTL-INSEXP-DATE-6,'1900-01-01'),
              :QTL-INSPREM-6,
              :QTL-INSCOMM-6,
              :QTL-INSEARNINGS-6,
              :QTL-INSANTICERN-6,
              :QTL-INSCOMP-7,
              :QTL-INSOURS-7,
              NULLIF(:QTL-INSEFF-DATE-7,'1900-01-01'),
              NULLIF(:QTL-INSEXP-DATE-7,'1900-01-01'),
              :QTL-INSPREM-7,
              :QTL-INSCOMM-7,
              :QTL-INSEARNINGS-7,
              :QTL-INSANTICERN-7,
              :QTL-INSCOMP-8,
              :QTL-INSOURS-8,
              NULLIF(:QTL-INSEFF-DATE-8,'1900-01-01'),
              NULLIF(:QTL-INSEXP-DATE-8,'1900-01-01'),
              :QTL-INSPREM-8,
              :QTL-INSCOMM-8,
              :QTL-INSEARNINGS-8,
              :QTL-INSANTICERN-8,
              :QTL-INSCOMP-9,
              :QTL-INSOURS-9,
              NULLIF(:QTL-INSEFF-DATE-9,'1900-01-01'),
              NULLIF(:QTL-INSEXP-DATE-9,'1900-01-01'),
              :QTL-INSPREM-9,
              :QTL-INSCOMM-9,
              :QTL-INSEARNINGS-9,
              :QTL-INSANTICERN-9,
              :QTL-INSCOMP-10,
              :QTL-INSOURS-10,
              NULLIF(:QTL-INSEFF-DATE-10,'1900-01-01'),
              NULLIF(:QTL-INSEXP-DATE-10,'1900-01-01'),
              :QTL-INSPREM-10,
              :QTL-INSCOMM-10,
              :QTL-INSEARNINGS-10,
              :QTL-INSANTICERN-10,
              :QTL-DLNO,
              :QTL-EARNINGS-1,
              :QTL-EARNINGS-2,
              :QTL-EARNINGS-3,
              :QTL-EARNINGS-4,
              :QTL-EARNINGS-5,
              :QTL-EARNINGS-6,
              :QTL-EARNINGS-7,
              :QTL-EARNINGS-8,
              :QTL-EARNINGS-9,
              :QTL-EARNINGS-10,
              :QTL-ANTICERN-1,
              :QTL-ANTICERN-2,
              :QTL-ANTICERN-3,
              :QTL-ANTICERN-4,
              :QTL-ANTICERN-5,
              :QTL-ANTICERN-6,
              :QTL-ANTICERN-7,
              :QTL-ANTICERN-8,
              :QTL-ANTICERN-9,
              :QTL-ANTICERN-10,
              :QTL-PRLNNO,
              :QTL-PRBAL,
              :QTL-PRINTDUE,
              :QTL-PRFINCHGRB,
              :QTL-PRINSRB,
              :QTL-LPEOD5-FG,
              :QTL-STATE-WORKER,
              :QTL-RESTRICTD-ACCTNO,
              :QTL-SPRCLASS,
              :QTL-USERID,
              NULLIF(:QTL-POSTDATE,'1900-01-01'),
              :QTL-POSTTIME,
              :QTL-GIFTNET,
              :QTL-APR,
              :QTL-SP-LNCOST-FRMLA,
              :QTL-MIP-FG,
              :QTL-INS-TAX-COUNTY-CD,
              :QTL-INS-TAX-CITY-CD,
              :QTL-PRDLDISC-UNEARN,
              :QTL-POINTS,
              :QTL-TAX-AMT,
              :QTL-PBI-PBD-FG,
              :QTL-APPROVED-BY-USER,
              :QTL-APPROVED-BY-CODE,
              NULLIF(:QTL-APPROVED-BY-DATE,'1900-01-01'),
              :QTL-APPROVED-BY-TIME,
              :QTL-DEBIT-CARD-AMT)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TRL1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-TRL1-FILE.

           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           ACCEPT TL-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TRL-SELECT-PATH TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           PERFORM SET-TRL-FIELDS.

           EXEC SQL
            UPDATE DBO.TRLFILE
             SET
              TRLFILE.TL_BRNO = :QTL-BRNO,
              TRLFILE.TL_DATE =
                 NULLIF(:QTL-DATE,'1900-01-01'),
              TRLFILE.TL_CLASS = :QTL-CLASS,
              TRLFILE.TL_ACCTNO = :QTL-ACCTNO,
              TRLFILE.TL_LAWCODE = :QTL-LAWCODE,
              TRLFILE.TL_LINE = :QTL-LINE,
              TRLFILE.TL_LTOUCH_DATE =
                 NULLIF(:QTL-LTOUCH-DATE,'1900-01-01'),
              TRLFILE.TL_SSNO_1 = :QTL-SSNO-1,
              TRLFILE.TL_MAKERCD_1 = :QTL-MAKERCD-1,
              TRLFILE.TL_SSNO_2 = :QTL-SSNO-2,
              TRLFILE.TL_MAKERCD_2 = :QTL-MAKERCD-2,
              TRLFILE.TL_SSNO_3 = :QTL-SSNO-3,
              TRLFILE.TL_MAKERCD_3 = :QTL-MAKERCD-3,
              TRLFILE.TL_SSNO_4 = :QTL-SSNO-4,
              TRLFILE.TL_MAKERCD_4 = :QTL-MAKERCD-4,
              TRLFILE.TL_PURCD = :QTL-PURCD,
              TRLFILE.TL_SRCD = :QTL-SRCD,
              TRLFILE.TL_COLLCD = :QTL-COLLCD,
              TRLFILE.TL_SERVACCT = :QTL-SERVACCT,
              TRLFILE.TL_LOANTYPE = :QTL-LOANTYPE,
              TRLFILE.TL_SUBCLASS = :QTL-SUBCLASS,
              TRLFILE.TL_LOANDATE =
                 NULLIF(:QTL-LOANDATE,'1900-01-01'),
              TRLFILE.TL_INTDATE =
                 NULLIF(:QTL-INTDATE,'1900-01-01'),
              TRLFILE.TL_PROCEEDS = :QTL-PROCEEDS,
              TRLFILE.TL_LNAMT = :QTL-LNAMT,
              TRLFILE.TL_FINCHG = :QTL-FINCHG,
              TRLFILE.TL_INTCHG = :QTL-INTCHG,
              TRLFILE.TL_SERCHG = :QTL-SERCHG,
              TRLFILE.TL_EXTCHG = :QTL-EXTCHG,
              TRLFILE.TL_MAINTFEE = :QTL-MAINTFEE,
              TRLFILE.TL_ORGTERM = :QTL-ORGTERM,
              TRLFILE.TL_1STPYAMT = :QTL-1STPYAMT,
              TRLFILE.TL_NOREGPY = :QTL-NOREGPY,
              TRLFILE.TL_REGPYAMT = :QTL-REGPYAMT,
              TRLFILE.TL_LASTPYAMT = :QTL-LASTPYAMT,
              TRLFILE.TL_PROCASH = :QTL-PROCASH,
              TRLFILE.TL_TILLNO = :QTL-TILLNO,
              TRLFILE.TL_FEECODE_1 = :QTL-FEECODE-1,
              TRLFILE.TL_FEEAMT_1 = :QTL-FEEAMT-1,
              TRLFILE.TL_FEECODE_2 = :QTL-FEECODE-2,
              TRLFILE.TL_FEEAMT_2 = :QTL-FEEAMT-2,
              TRLFILE.TL_FEECODE_3 = :QTL-FEECODE-3,
              TRLFILE.TL_FEEAMT_3 = :QTL-FEEAMT-3,
              TRLFILE.TL_FEECODE_4 = :QTL-FEECODE-4,
              TRLFILE.TL_FEEAMT_4 = :QTL-FEEAMT-4,
              TRLFILE.TL_FEECODE_5 = :QTL-FEECODE-5,
              TRLFILE.TL_FEEAMT_5 = :QTL-FEEAMT-5,
              TRLFILE.TL_FEECODE_6 = :QTL-FEECODE-6,
              TRLFILE.TL_FEEAMT_6 = :QTL-FEEAMT-6,
              TRLFILE.TL_FEECODE_7 = :QTL-FEECODE-7,
              TRLFILE.TL_FEEAMT_7 = :QTL-FEEAMT-7,
              TRLFILE.TL_FEECODE_8 = :QTL-FEECODE-8,
              TRLFILE.TL_FEEAMT_8 = :QTL-FEEAMT-8,
              TRLFILE.TL_FEECODE_9 = :QTL-FEECODE-9,
              TRLFILE.TL_FEEAMT_9 = :QTL-FEEAMT-9,
              TRLFILE.TL_FEECODE_10 = :QTL-FEECODE-10,
              TRLFILE.TL_FEEAMT_10 = :QTL-FEEAMT-10,
              TRLFILE.TL_FEECODE_11 = :QTL-FEECODE-11,
              TRLFILE.TL_FEEAMT_11 = :QTL-FEEAMT-11,
              TRLFILE.TL_FEECODE_12 = :QTL-FEECODE-12,
              TRLFILE.TL_FEEAMT_12 = :QTL-FEEAMT-12,
              TRLFILE.TL_FEECODE_13 = :QTL-FEECODE-13,
              TRLFILE.TL_FEEAMT_13 = :QTL-FEEAMT-13,
              TRLFILE.TL_FEECODE_14 = :QTL-FEECODE-14,
              TRLFILE.TL_FEEAMT_14 = :QTL-FEEAMT-14,
              TRLFILE.TL_FEECODE_15 = :QTL-FEECODE-15,
              TRLFILE.TL_FEEAMT_15 = :QTL-FEEAMT-15,
              TRLFILE.TL_FEECODE_16 = :QTL-FEECODE-16,
              TRLFILE.TL_FEEAMT_16 = :QTL-FEEAMT-16,
              TRLFILE.TL_FEECODE_17 = :QTL-FEECODE-17,
              TRLFILE.TL_FEEAMT_17 = :QTL-FEEAMT-17,
              TRLFILE.TL_FEECODE_18 = :QTL-FEECODE-18,
              TRLFILE.TL_FEEAMT_18 = :QTL-FEEAMT-18,
              TRLFILE.TL_FEECODE_19 = :QTL-FEECODE-19,
              TRLFILE.TL_FEEAMT_19 = :QTL-FEEAMT-19,
              TRLFILE.TL_FEECODE_20 = :QTL-FEECODE-20,
              TRLFILE.TL_FEEAMT_20 = :QTL-FEEAMT-20,
              TRLFILE.TL_INSURED = :QTL-INSURED,
              TRLFILE.TL_INSTYPES = :QTL-INSTYPES,
              TRLFILE.TL_INSELIGIBLE = :QTL-INSELIGIBLE,
              TRLFILE.TL_INSCOMP_1 = :QTL-INSCOMP-1,
              TRLFILE.TL_INSOURS_1 = :QTL-INSOURS-1,
              TRLFILE.TL_INSEFF_DATE_1 =
                 NULLIF(:QTL-INSEFF-DATE-1,'1900-01-01'),
              TRLFILE.TL_INSEXP_DATE_1 =
                 NULLIF(:QTL-INSEXP-DATE-1,'1900-01-01'),
              TRLFILE.TL_INSPREM_1 = :QTL-INSPREM-1,
              TRLFILE.TL_INSCOMM_1 = :QTL-INSCOMM-1,
              TRLFILE.TL_INSEARNINGS_1 = :QTL-INSEARNINGS-1,
              TRLFILE.TL_INSANTICERN_1 = :QTL-INSANTICERN-1,
              TRLFILE.TL_INSCOMP_2 = :QTL-INSCOMP-2,
              TRLFILE.TL_INSOURS_2 = :QTL-INSOURS-2,
              TRLFILE.TL_INSEFF_DATE_2 =
                 NULLIF(:QTL-INSEFF-DATE-2,'1900-01-01'),
              TRLFILE.TL_INSEXP_DATE_2 =
                 NULLIF(:QTL-INSEXP-DATE-2,'1900-01-01'),
              TRLFILE.TL_INSPREM_2 = :QTL-INSPREM-2,
              TRLFILE.TL_INSCOMM_2 = :QTL-INSCOMM-2,
              TRLFILE.TL_INSEARNINGS_2 = :QTL-INSEARNINGS-2,
              TRLFILE.TL_INSANTICERN_2 = :QTL-INSANTICERN-2,
              TRLFILE.TL_INSCOMP_3 = :QTL-INSCOMP-3,
              TRLFILE.TL_INSOURS_3 = :QTL-INSOURS-3,
              TRLFILE.TL_INSEFF_DATE_3 =
                 NULLIF(:QTL-INSEFF-DATE-3,'1900-01-01'),
              TRLFILE.TL_INSEXP_DATE_3 =
                 NULLIF(:QTL-INSEXP-DATE-3,'1900-01-01'),
              TRLFILE.TL_INSPREM_3 = :QTL-INSPREM-3,
              TRLFILE.TL_INSCOMM_3 = :QTL-INSCOMM-3,
              TRLFILE.TL_INSEARNINGS_3 = :QTL-INSEARNINGS-3,
              TRLFILE.TL_INSANTICERN_3 = :QTL-INSANTICERN-3,
              TRLFILE.TL_INSCOMP_4 = :QTL-INSCOMP-4,
              TRLFILE.TL_INSOURS_4 = :QTL-INSOURS-4,
              TRLFILE.TL_INSEFF_DATE_4 =
                 NULLIF(:QTL-INSEFF-DATE-4,'1900-01-01'),
              TRLFILE.TL_INSEXP_DATE_4 =
                 NULLIF(:QTL-INSEXP-DATE-4,'1900-01-01'),
              TRLFILE.TL_INSPREM_4 = :QTL-INSPREM-4,
              TRLFILE.TL_INSCOMM_4 = :QTL-INSCOMM-4,
              TRLFILE.TL_INSEARNINGS_4 = :QTL-INSEARNINGS-4,
              TRLFILE.TL_INSANTICERN_4 = :QTL-INSANTICERN-4,
              TRLFILE.TL_INSCOMP_5 = :QTL-INSCOMP-5,
              TRLFILE.TL_INSOURS_5 = :QTL-INSOURS-5,
              TRLFILE.TL_INSEFF_DATE_5 =
                 NULLIF(:QTL-INSEFF-DATE-5,'1900-01-01'),
              TRLFILE.TL_INSEXP_DATE_5 =
                 NULLIF(:QTL-INSEXP-DATE-5,'1900-01-01'),
              TRLFILE.TL_INSPREM_5 = :QTL-INSPREM-5,
              TRLFILE.TL_INSCOMM_5 = :QTL-INSCOMM-5,
              TRLFILE.TL_INSEARNINGS_5 = :QTL-INSEARNINGS-5,
              TRLFILE.TL_INSANTICERN_5 = :QTL-INSANTICERN-5,
              TRLFILE.TL_INSCOMP_6 = :QTL-INSCOMP-6,
              TRLFILE.TL_INSOURS_6 = :QTL-INSOURS-6,
              TRLFILE.TL_INSEFF_DATE_6 =
                 NULLIF(:QTL-INSEFF-DATE-6,'1900-01-01'),
              TRLFILE.TL_INSEXP_DATE_6 =
                 NULLIF(:QTL-INSEXP-DATE-6,'1900-01-01'),
              TRLFILE.TL_INSPREM_6 = :QTL-INSPREM-6,
              TRLFILE.TL_INSCOMM_6 = :QTL-INSCOMM-6,
              TRLFILE.TL_INSEARNINGS_6 = :QTL-INSEARNINGS-6,
              TRLFILE.TL_INSANTICERN_6 = :QTL-INSANTICERN-6,
              TRLFILE.TL_INSCOMP_7 = :QTL-INSCOMP-7,
              TRLFILE.TL_INSOURS_7 = :QTL-INSOURS-7,
              TRLFILE.TL_INSEFF_DATE_7 =
                 NULLIF(:QTL-INSEFF-DATE-7,'1900-01-01'),
              TRLFILE.TL_INSEXP_DATE_7 =
                 NULLIF(:QTL-INSEXP-DATE-7,'1900-01-01'),
              TRLFILE.TL_INSPREM_7 = :QTL-INSPREM-7,
              TRLFILE.TL_INSCOMM_7 = :QTL-INSCOMM-7,
              TRLFILE.TL_INSEARNINGS_7 = :QTL-INSEARNINGS-7,
              TRLFILE.TL_INSANTICERN_7 = :QTL-INSANTICERN-7,
              TRLFILE.TL_INSCOMP_8 = :QTL-INSCOMP-8,
              TRLFILE.TL_INSOURS_8 = :QTL-INSOURS-8,
              TRLFILE.TL_INSEFF_DATE_8 =
                 NULLIF(:QTL-INSEFF-DATE-8,'1900-01-01'),
              TRLFILE.TL_INSEXP_DATE_8 =
                 NULLIF(:QTL-INSEXP-DATE-8,'1900-01-01'),
              TRLFILE.TL_INSPREM_8 = :QTL-INSPREM-8,
              TRLFILE.TL_INSCOMM_8 = :QTL-INSCOMM-8,
              TRLFILE.TL_INSEARNINGS_8 = :QTL-INSEARNINGS-8,
              TRLFILE.TL_INSANTICERN_8 = :QTL-INSANTICERN-8,
              TRLFILE.TL_INSCOMP_9 = :QTL-INSCOMP-9,
              TRLFILE.TL_INSOURS_9 = :QTL-INSOURS-9,
              TRLFILE.TL_INSEFF_DATE_9 =
                 NULLIF(:QTL-INSEFF-DATE-9,'1900-01-01'),
              TRLFILE.TL_INSEXP_DATE_9 =
                 NULLIF(:QTL-INSEXP-DATE-9,'1900-01-01'),
              TRLFILE.TL_INSPREM_9 = :QTL-INSPREM-9,
              TRLFILE.TL_INSCOMM_9 = :QTL-INSCOMM-9,
              TRLFILE.TL_INSEARNINGS_9 = :QTL-INSEARNINGS-9,
              TRLFILE.TL_INSANTICERN_9 = :QTL-INSANTICERN-9,
              TRLFILE.TL_INSCOMP_10 = :QTL-INSCOMP-10,
              TRLFILE.TL_INSOURS_10 = :QTL-INSOURS-10,
              TRLFILE.TL_INSEFF_DATE_10 =
                 NULLIF(:QTL-INSEFF-DATE-10,'1900-01-01'),
              TRLFILE.TL_INSEXP_DATE_10 =
                 NULLIF(:QTL-INSEXP-DATE-10,'1900-01-01'),
              TRLFILE.TL_INSPREM_10 = :QTL-INSPREM-10,
              TRLFILE.TL_INSCOMM_10 = :QTL-INSCOMM-10,
              TRLFILE.TL_INSEARNINGS_10 = :QTL-INSEARNINGS-10,
              TRLFILE.TL_INSANTICERN_10 = :QTL-INSANTICERN-10,
              TRLFILE.TL_DLNO = :QTL-DLNO,
              TRLFILE.TL_EARNINGS_1 = :QTL-EARNINGS-1,
              TRLFILE.TL_EARNINGS_2 = :QTL-EARNINGS-2,
              TRLFILE.TL_EARNINGS_3 = :QTL-EARNINGS-3,
              TRLFILE.TL_EARNINGS_4 = :QTL-EARNINGS-4,
              TRLFILE.TL_EARNINGS_5 = :QTL-EARNINGS-5,
              TRLFILE.TL_EARNINGS_6 = :QTL-EARNINGS-6,
              TRLFILE.TL_EARNINGS_7 = :QTL-EARNINGS-7,
              TRLFILE.TL_EARNINGS_8 = :QTL-EARNINGS-8,
              TRLFILE.TL_EARNINGS_9 = :QTL-EARNINGS-9,
              TRLFILE.TL_EARNINGS_10 = :QTL-EARNINGS-10,
              TRLFILE.TL_ANTICERN_1 = :QTL-ANTICERN-1,
              TRLFILE.TL_ANTICERN_2 = :QTL-ANTICERN-2,
              TRLFILE.TL_ANTICERN_3 = :QTL-ANTICERN-3,
              TRLFILE.TL_ANTICERN_4 = :QTL-ANTICERN-4,
              TRLFILE.TL_ANTICERN_5 = :QTL-ANTICERN-5,
              TRLFILE.TL_ANTICERN_6 = :QTL-ANTICERN-6,
              TRLFILE.TL_ANTICERN_7 = :QTL-ANTICERN-7,
              TRLFILE.TL_ANTICERN_8 = :QTL-ANTICERN-8,
              TRLFILE.TL_ANTICERN_9 = :QTL-ANTICERN-9,
              TRLFILE.TL_ANTICERN_10 = :QTL-ANTICERN-10,
              TRLFILE.TL_PRLNNO = :QTL-PRLNNO,
              TRLFILE.TL_PRBAL = :QTL-PRBAL,
              TRLFILE.TL_PRINTDUE = :QTL-PRINTDUE,
              TRLFILE.TL_PRFINCHGRB = :QTL-PRFINCHGRB,
              TRLFILE.TL_PRINSRB = :QTL-PRINSRB,
              TRLFILE.TL_LPEOD5_FG = :QTL-LPEOD5-FG,
              TRLFILE.TL_STATE_WORKER = :QTL-STATE-WORKER,
              TRLFILE.TL_RESTRICTD_ACCTNO = :QTL-RESTRICTD-ACCTNO,
              TRLFILE.TL_SPRCLASS = :QTL-SPRCLASS,
              TRLFILE.TL_USERID = :QTL-USERID,
              TRLFILE.TL_POSTDATE =
                 NULLIF(:QTL-POSTDATE,'1900-01-01'),
              TRLFILE.TL_POSTTIME = :QTL-POSTTIME,
              TRLFILE.TL_GIFTNET = :QTL-GIFTNET,
              TRLFILE.TL_APR = :QTL-APR,
              TRLFILE.TL_SP_LNCOST_FRMLA = :QTL-SP-LNCOST-FRMLA,
              TRLFILE.TL_MIP_FG = :QTL-MIP-FG,
              TRLFILE.TL_INS_TAX_COUNTY_CD =
                   :QTL-INS-TAX-COUNTY-CD,
              TRLFILE.TL_INS_TAX_CITY_CD = :QTL-INS-TAX-CITY-CD,
              TRLFILE.TL_PRDLDISC_UNEARN = :QTL-PRDLDISC-UNEARN,
              TRLFILE.TL_POINTS = :QTL-POINTS,
              TRLFILE.TL_TAX_AMT = :QTL-TAX-AMT,
              TRLFILE.TL_PBI_PBD_FG = :QTL-PBI-PBD-FG,
              TRLFILE.TL_APPROVED_BY_USER = :QTL-APPROVED-BY-USER,
              TRLFILE.TL_APPROVED_BY_CODE = :QTL-APPROVED-BY-CODE,
              TRLFILE.TL_APPROVED_BY_DATE =
                 NULLIF(:QTL-APPROVED-BY-DATE,'1900-01-01'),
              TRLFILE.TL_APPROVED_BY_TIME = :QTL-APPROVED-BY-TIME,
              TRLFILE.TL_DEBIT_CARD_AMT = :QTL-DEBIT-CARD-AMT
            WHERE TRLFILE.TL_BRNO = :QTL-BRNO
              AND TRLFILE.TL_DATE = :QTL-DATE
              AND TRLFILE.TL_CLASS = :QTL-CLASS
              AND TRLFILE.TL_ACCTNO = :QTL-ACCTNO
              AND TRLFILE.TL_LAWCODE = :QTL-LAWCODE
              AND TRLFILE.TL_LINE = :QTL-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TRL1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-TRL1-FILE.

           MOVE TRL1-KEY        TO E-KEYX.
           PERFORM DELETE-IT.
           MOVE TRL-SELECT-PATH TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           PERFORM SET-TRL-FIELDS.

           EXEC SQL
            DELETE FROM DBO.TRLFILE
            WHERE TRLFILE.TL_BRNO = :QTL-BRNO
              AND TRLFILE.TL_DATE = :QTL-DATE
              AND TRLFILE.TL_CLASS = :QTL-CLASS
              AND TRLFILE.TL_ACCTNO = :QTL-ACCTNO
              AND TRLFILE.TL_LAWCODE = :QTL-LAWCODE
              AND TRLFILE.TL_LINE = :QTL-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRL1-FILE.

      *-----------------------------------------------------------------
       CLOSE-TRL1-FILE.

           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRL1-CURSOR-STAT.

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
              DESTROY IOTRLI-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
