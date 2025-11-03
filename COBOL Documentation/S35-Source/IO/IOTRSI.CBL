       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOTRSR.
       AUTHOR.        BAH.
       DATE-WRITTEN.  04-14-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOTRSR.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : TRSFILE 
      * TABLE DESC    : TRANSACTION SALES FINANCE FILE
      * OUTPUT FILE   : XPC/IOTRSR.PRE
      * USED FOR      : LIBLP/LPWSTRS.CPY  --- QTRS-REC
      *                 LIBLP/LPTRS1R.CPY  --- READ
      *                 LIBLP/LPTRS1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0414 ***NEW***
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./IO/IOTRSR.CBL S34 05/05/21-16:11:55 >".

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
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LPWSTRS.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QTRS1-WHERE-BEG-KEY.
           03  QTRS1-WBEG-BRNO             PIC 9(04).
           03  QTRS1-WBEG-DATE             PIC X(10).
           03  QTRS1-WBEG-CLASS            PIC 9(03).
           03  QTRS1-WBEG-ACCTNO           PIC 9(08).
           03  QTRS1-WBEG-LAWCODE          PIC 9(02).
           03  QTRS1-WBEG-LINE             PIC 9(03).

       01  QTRS1-WHERE-END-KEY.
           03  QTRS1-WEND-BRNO             PIC 9(04).
           03  QTRS1-WEND-DATE             PIC X(10).
           03  QTRS1-WEND-CLASS            PIC 9(03).
           03  QTRS1-WEND-ACCTNO           PIC 9(08).
           03  QTRS1-WEND-LAWCODE          PIC 9(02).
           03  QTRS1-WEND-LINE             PIC 9(03).

       01  QTRS-REC.
           03  QTS-BRNO             PIC 9(4).
           03  QTS-DATE             PIC X(10).
           03  QTS-CLASS            PIC 9(3).
           03  QTS-ACCTNO           PIC 9(8).
           03  QTS-LAWCODE          PIC 99.
           03  QTS-LINE             PIC 9(3).
           03  QTS-LTOUCH-DATE      PIC X(10).
           03  QTS-SSNO-1         PIC 9(9).
           03  QTS-MAKERCD-1      PIC X.
           03  QTS-SSNO-2         PIC 9(9).
           03  QTS-MAKERCD-2      PIC X.
           03  QTS-SSNO-3         PIC 9(9).
           03  QTS-MAKERCD-3      PIC X.
           03  QTS-SSNO-4         PIC 9(9).
           03  QTS-MAKERCD-4      PIC X.
           03  QTS-PURCD            PIC XXX.
           03  QTS-SRCD             PIC XX.
           03  QTS-COLLCD           PIC XX.
           03  QTS-SERVACCT         PIC X.
           03  QTS-LOANTYPE         PIC X.
           03  QTS-SUBCLASS         PIC 9(2).
           03  QTS-LOANDATE         PIC X(10).
           03  QTS-INTDATE          PIC X(10).
           03  QTS-PROCEEDS         PIC S9(7)V99.
           03  QTS-LNAMT            PIC S9(7)V99.
           03  QTS-FINCHG           PIC S9(7)V99.
           03  QTS-INTCHG           PIC S9(7)V99.
           03  QTS-SERCHG           PIC S9(5)V99.
           03  QTS-EXTCHG           PIC S9(5)V99.
           03  QTS-MAINTFEE         PIC S9(5)V99.
           03  QTS-ORGTERM          PIC 9(3).
           03  QTS-1STPYAMT         PIC S9(7)V99.
           03  QTS-NOREGPY          PIC 9(3).
           03  QTS-REGPYAMT         PIC S9(7)V99.
           03  QTS-LASTPYAMT        PIC S9(7)V99.
           03  QTS-FEECODE-1      PIC XX.
           03  QTS-FEEAMT-1       PIC S9(5)V99.
           03  QTS-FEEOURS-1      PIC X.
           03  QTS-FEECODE-2      PIC XX.
           03  QTS-FEEAMT-2       PIC S9(5)V99.
           03  QTS-FEEOURS-2      PIC X.
           03  QTS-FEECODE-3      PIC XX.
           03  QTS-FEEAMT-3       PIC S9(5)V99.
           03  QTS-FEEOURS-3      PIC X.
           03  QTS-FEECODE-4      PIC XX.
           03  QTS-FEEAMT-4       PIC S9(5)V99.
           03  QTS-FEEOURS-4      PIC X.
           03  QTS-FEECODE-5      PIC XX.
           03  QTS-FEEAMT-5       PIC S9(5)V99.
           03  QTS-FEEOURS-5      PIC X.
           03  QTS-FEECODE-6      PIC XX.
           03  QTS-FEEAMT-6       PIC S9(5)V99.
           03  QTS-FEEOURS-6      PIC X.
           03  QTS-FEECODE-7      PIC XX.
           03  QTS-FEEAMT-7       PIC S9(5)V99.
           03  QTS-FEEOURS-7      PIC X.
           03  QTS-FEECODE-8      PIC XX.
           03  QTS-FEEAMT-8       PIC S9(5)V99.
           03  QTS-FEEOURS-8      PIC X.
           03  QTS-FEECODE-9      PIC XX.
           03  QTS-FEEAMT-9       PIC S9(5)V99.
           03  QTS-FEEOURS-9      PIC X.
           03  QTS-FEECODE-10     PIC XX.
           03  QTS-FEEAMT-10      PIC S9(5)V99.
           03  QTS-FEEOURS-10     PIC X.
           03  QTS-FEECODE-11      PIC XX.
           03  QTS-FEEAMT-11       PIC S9(5)V99.
           03  QTS-FEEOURS-11      PIC X.
           03  QTS-FEECODE-12      PIC XX.
           03  QTS-FEEAMT-12       PIC S9(5)V99.
           03  QTS-FEEOURS-12      PIC X.
           03  QTS-FEECODE-13      PIC XX.
           03  QTS-FEEAMT-13       PIC S9(5)V99.
           03  QTS-FEEOURS-13      PIC X.
           03  QTS-FEECODE-14      PIC XX.
           03  QTS-FEEAMT-14       PIC S9(5)V99.
           03  QTS-FEEOURS-14      PIC X.
           03  QTS-FEECODE-15      PIC XX.
           03  QTS-FEEAMT-15       PIC S9(5)V99.
           03  QTS-FEEOURS-15      PIC X.
           03  QTS-FEECODE-16      PIC XX.
           03  QTS-FEEAMT-16       PIC S9(5)V99.
           03  QTS-FEEOURS-16      PIC X.
           03  QTS-FEECODE-17      PIC XX.
           03  QTS-FEEAMT-17       PIC S9(5)V99.
           03  QTS-FEEOURS-17      PIC X.
           03  QTS-FEECODE-18      PIC XX.
           03  QTS-FEEAMT-18       PIC S9(5)V99.
           03  QTS-FEEOURS-18      PIC X.
           03  QTS-FEECODE-19      PIC XX.
           03  QTS-FEEAMT-19       PIC S9(5)V99.
           03  QTS-FEEOURS-19      PIC X.
           03  QTS-FEECODE-20      PIC XX.
           03  QTS-FEEAMT-20       PIC S9(5)V99.
           03  QTS-FEEOURS-20      PIC X.
           03  QTS-INSURED      PIC X.
           03  QTS-INSTYPES     PIC X.
           03  QTS-INSELIGIBLE  PIC X.
           03  QTS-INSCOMP-1      PIC 99.
           03  QTS-INSOURS-1      PIC X.
           03  QTS-INSEFF-DATE-1  PIC X(10).
           03  QTS-INSEXP-DATE-1  PIC X(10).
           03  QTS-INSPREM-1      PIC S9(5)V99.
           03  QTS-INSCOMM-1      PIC S9(5)V99.
           03  QTS-INSANTICERN-1  PIC S9(7)V99.
           03  QTS-INSEARNINGS-1  PIC S9(7)V99.
           03  QTS-INSCOMP-2      PIC 99.
           03  QTS-INSOURS-2      PIC X.
           03  QTS-INSEFF-DATE-2  PIC X(10).
           03  QTS-INSEXP-DATE-2  PIC X(10).
           03  QTS-INSPREM-2      PIC S9(5)V99.
           03  QTS-INSCOMM-2      PIC S9(5)V99.
           03  QTS-INSANTICERN-2  PIC S9(7)V99.
           03  QTS-INSEARNINGS-2  PIC S9(7)V99.
           03  QTS-INSCOMP-3      PIC 99.
           03  QTS-INSOURS-3      PIC X.
           03  QTS-INSEFF-DATE-3  PIC X(10).
           03  QTS-INSEXP-DATE-3  PIC X(10).
           03  QTS-INSPREM-3      PIC S9(5)V99.
           03  QTS-INSCOMM-3      PIC S9(5)V99.
           03  QTS-INSANTICERN-3  PIC S9(7)V99.
           03  QTS-INSEARNINGS-3  PIC S9(7)V99.
           03  QTS-INSCOMP-4      PIC 99.
           03  QTS-INSOURS-4      PIC X.
           03  QTS-INSEFF-DATE-4  PIC X(10).
           03  QTS-INSEXP-DATE-4  PIC X(10).
           03  QTS-INSPREM-4      PIC S9(5)V99.
           03  QTS-INSCOMM-4      PIC S9(5)V99.
           03  QTS-INSANTICERN-4  PIC S9(7)V99.
           03  QTS-INSEARNINGS-4  PIC S9(7)V99.
           03  QTS-INSCOMP-5      PIC 99.
           03  QTS-INSOURS-5      PIC X.
           03  QTS-INSEFF-DATE-5  PIC X(10).
           03  QTS-INSEXP-DATE-5  PIC X(10).
           03  QTS-INSPREM-5      PIC S9(5)V99.
           03  QTS-INSCOMM-5      PIC S9(5)V99.
           03  QTS-INSANTICERN-5  PIC S9(7)V99.
           03  QTS-INSEARNINGS-5  PIC S9(7)V99.
           03  QTS-INSCOMP-6      PIC 99.
           03  QTS-INSOURS-6      PIC X.
           03  QTS-INSEFF-DATE-6  PIC X(10).
           03  QTS-INSEXP-DATE-6  PIC X(10).
           03  QTS-INSPREM-6      PIC S9(5)V99.
           03  QTS-INSCOMM-6      PIC S9(5)V99.
           03  QTS-INSANTICERN-6  PIC S9(7)V99.
           03  QTS-INSEARNINGS-6  PIC S9(7)V99.
           03  QTS-INSCOMP-7      PIC 99.
           03  QTS-INSOURS-7      PIC X.
           03  QTS-INSEFF-DATE-7  PIC X(10).
           03  QTS-INSEXP-DATE-7  PIC X(10).
           03  QTS-INSPREM-7      PIC S9(5)V99.
           03  QTS-INSCOMM-7      PIC S9(5)V99.
           03  QTS-INSANTICERN-7  PIC S9(7)V99.
           03  QTS-INSEARNINGS-7  PIC S9(7)V99.
           03  QTS-INSCOMP-8      PIC 99.
           03  QTS-INSOURS-8      PIC X.
           03  QTS-INSEFF-DATE-8  PIC X(10).
           03  QTS-INSEXP-DATE-8  PIC X(10).
           03  QTS-INSPREM-8      PIC S9(5)V99.
           03  QTS-INSCOMM-8      PIC S9(5)V99.
           03  QTS-INSANTICERN-8  PIC S9(7)V99.
           03  QTS-INSEARNINGS-8  PIC S9(7)V99.
           03  QTS-INSCOMP-9      PIC 99.
           03  QTS-INSOURS-9      PIC X.
           03  QTS-INSEFF-DATE-9  PIC X(10).
           03  QTS-INSEXP-DATE-9  PIC X(10).
           03  QTS-INSPREM-9      PIC S9(5)V99.
           03  QTS-INSCOMM-9      PIC S9(5)V99.
           03  QTS-INSANTICERN-9  PIC S9(7)V99.
           03  QTS-INSEARNINGS-9  PIC S9(7)V99.
           03  QTS-INSCOMP-10     PIC 99.
           03  QTS-INSOURS-10     PIC X.
           03  QTS-INSEFF-DATE-10 PIC X(10).
           03  QTS-INSEXP-DATE-10 PIC X(10).
           03  QTS-INSPREM-10     PIC S9(5)V99.
           03  QTS-INSCOMM-10     PIC S9(5)V99.
           03  QTS-INSANTICERN-10 PIC S9(7)V99.
           03  QTS-INSEARNINGS-10 PIC S9(7)V99.
           03  QTS-DLCOMM-PP-CMSI     PIC S9(5)V99.
           03  QTS-DLCOMM-O3-CMSI     PIC S9(5)V99.
           03  QTS-INS-TAX-COUNTY-CD  PIC X(3).
           03  QTS-INS-TAX-CITY-CD    PIC X(3).
           03  QTS-TILLNO             PIC 9(2).
           03  QTS-UNWIND-REASON-CODE PIC XX.
           03  QTS-DLNO             PIC 9(4)V99.
           03  QTS-DLRECOURSE       PIC X.
           03  QTS-DLPARTRCAMT      PIC S9(7)V99.
           03  QTS-PURBAL           PIC S9(7)V99.
           03  QTS-DLRVPRIN-1       PIC S9(7)V99.
           03  QTS-DLRVPRIN-2       PIC S9(7)V99.
           03  QTS-DLRVPRIN-3       PIC S9(7)V99.
           03  QTS-DLRVCHGS-1       PIC S9(7)V99.
           03  QTS-DLRVCHGS-2       PIC S9(7)V99.
           03  QTS-DLRVCHGS-3       PIC S9(7)V99.
           03  QTS-DL-RVCHGS-CODE-1 PIC X.
           03  QTS-DL-RVCHGS-CODE-2 PIC X.
           03  QTS-DL-RVCHGS-CODE-3 PIC X.
           03  QTS-DL-PARVHELD-CODE PIC X.
           03  QTS-DL-PARVPAID-CODE PIC X.
           03  QTS-DLPARVHELD       PIC S9(7)V99.
           03  QTS-DLPARVPAID       PIC S9(7)V99.
           03  QTS-DLDISC           PIC S9(7)V99.
           03  QTS-ANTICTERM        PIC 9(3).
           03  QTS-ANTICERN-1       PIC S9(7)V99.
           03  QTS-ANTICERN-2       PIC S9(7)V99.
           03  QTS-ANTICERN-3       PIC S9(7)V99.
           03  QTS-ANTICERN-4       PIC S9(7)V99.
           03  QTS-ANTICERN-5       PIC S9(7)V99.
           03  QTS-ANTICERN-6       PIC S9(7)V99.
           03  QTS-ANTICERN-7       PIC S9(7)V99.
           03  QTS-ANTICERN-8       PIC S9(7)V99.
           03  QTS-ANTICERN-9       PIC S9(7)V99.
           03  QTS-ANTICERN-10      PIC S9(7)V99.
           03  QTS-EARNINGS-1       PIC S9(7)V99.
           03  QTS-EARNINGS-2       PIC S9(7)V99.
           03  QTS-EARNINGS-3       PIC S9(7)V99.
           03  QTS-EARNINGS-4       PIC S9(7)V99.
           03  QTS-EARNINGS-5       PIC S9(7)V99.
           03  QTS-EARNINGS-6       PIC S9(7)V99.
           03  QTS-EARNINGS-7       PIC S9(7)V99.
           03  QTS-EARNINGS-8       PIC S9(7)V99.
           03  QTS-EARNINGS-9       PIC S9(7)V99.
           03  QTS-EARNINGS-10      PIC S9(7)V99.
           03  QTS-PRLNNO           PIC 9(8).
           03  QTS-INSPREM-DLADJ    PIC S9(5)V99.
           03  QTS-PRDLDISC-UNEARN  PIC S9(7)V99.
           03  QTS-SP-LNCOST-FRMLA  PIC X.
           03  QTS-DLPAID           PIC X.
           03  QTS-MIP-FG           PIC X.
           03  QTS-SPRCLASS         PIC 999.
           03  QTS-O3INS-DLPROC     PIC S9(7)V99.
           03  QTS-USERID           PIC X(10).
           03  QTS-POSTDATE         PIC X(10).
           03  QTS-POSTTIME         PIC 9(6).
           03  QTS-TAX-AMT          PIC S9(7)V99.
           03  QTS-APPROVED-BY-USER PIC X(10).
           03  QTS-APPROVED-BY-CODE PIC X(3).
           03  QTS-APPROVED-BY-DATE PIC X(10).
           03  QTS-APPROVED-BY-TIME PIC 9(6).
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

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-TS-BRNO                   PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-TS-DATE                   PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-TS-CLASS                  PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-TS-ACCTNO                 PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-TS-LAWCODE                PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-TS-LINE                   PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-TS-SSNO1                  PIC 9(09).
           03  FILLER                         PIC X(03).
           03  D01-TS-LOANDATE               PIC 9999/99/99.

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
       COPY "LIBIO/IOTRSR_DEF.CPY".
       COPY "LIBIO/IOTRSR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOTRSR_SCN.CPY".

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
           MOVE "IOTRSR"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOTRSR-QUEUE-POS  TO Q-POS.
           MOVE IOTRSR-COPIES-POS TO C-POS.
           MOVE IOTRSR-PRU-POS    TO P-POS.
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
              ( VDUNUM0 > 1      ) 
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

           MOVE "B"                        TO TRS-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }

           MOVE BR-NO     TO QTRS1-WBEG-BRNO.
           MOVE BR-NO     TO QTRS1-WEND-BRNO.

           MOVE BEG-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRS1-WBEG-DATE.
           MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRS1-WEND-DATE.

           MOVE BEG-CLASS  TO QTRS1-WBEG-CLASS.
           MOVE END-CLASS  TO QTRS1-WEND-CLASS.

           MOVE BEG-ACCTNO TO QTRS1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QTRS1-WEND-ACCTNO.
           MOVE BEG-LAWCODE TO QTRS1-WBEG-LAWCODE.
           MOVE END-LAWCODE TO QTRS1-WEND-LAWCODE.
           MOVE BEG-LINE TO QTRS1-WBEG-LINE.
           MOVE END-LINE TO QTRS1-WEND-LINE.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }

           PERFORM OPEN-TRS1-FILE
           PERFORM START-TRS1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-TRS1-FILE-NEXT.
           IF ( IO-BAD ) 
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( TS-BRNO < BEG-BRANCH )
                  GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( TS-BRNO > END-BRANCH )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF TS-DATE < BEG-DATE OR > END-DATE
                    GO TO PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 2 )
              IF ( TS-DATE < BEG-DATE )
                  GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( TS-DATE > END-DATE )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF TS-BRNO < BEG-BRANCH OR > END-BRANCH
                    GO TO PROCESS-BRANCH-NEXT.

           IF TS-CLASS < BEG-CLASS OR > END-CLASS
              GO TO PROCESS-BRANCH-NEXT.
           IF TS-ACCTNO < BEG-ACCTNO OR > END-ACCTNO
              GO TO PROCESS-BRANCH-NEXT.
           IF TS-LAWCODE < BEG-LAWCODE OR > END-LAWCODE
              GO TO PROCESS-BRANCH-NEXT.
           IF TS-LINE < BEG-LINE OR > END-LINE
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE TS-BRNO                    TO D01-TS-BRNO.
           MOVE TS-DATE                    TO D01-TS-DATE.
           MOVE TS-CLASS                   TO D01-TS-CLASS.
           MOVE TS-ACCTNO                  TO D01-TS-ACCTNO.
           MOVE TS-LAWCODE                 TO D01-TS-LAWCODE.
           MOVE TS-LINE                    TO D01-TS-LINE.
           MOVE TS-SSNO(1)                 TO D01-TS-SSNO1.
           MOVE TS-LOANDATE                TO D01-TS-LOANDATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-TRS1-FILE.

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
              MOVE TRS1-KEY TO PACE-DETAIL-KEY
           ELSE
              STRING TS-DATE, TS-BRNO, TS-CLASS, TS-ACCTNO, TS-LAWCODE,
                     TS-LINE DELIMITED BY SIZE INTO PACE-DETAIL-KEY.

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
           MOVE "SQL TABLE IO UTILITY - TRSFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOTRSR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOTRSR-SYSDATE.
           DISPLAY IOTRSR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOTRSR-FORM-FIELDS TO WR-BUF.
           MOVE IOTRSR-FORM-LIST TO WR-LIST.
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

           MOVE IOTRSR-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOTRSR-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOTRSR_EVA.CPY".
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
      *    LOAD TRS-REC FROM QTRS-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-TRS-FIELDS.
           MOVE QTS-BRNO TO TS-BRNO.

           IF ( QTS-DATE = "1900-01-01" )
              MOVE ZEROES TO TS-DATE
           ELSE
              MOVE QTS-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-DATE.

           MOVE QTS-CLASS TO TS-CLASS.
           MOVE QTS-ACCTNO TO TS-ACCTNO.
           MOVE QTS-LAWCODE TO TS-LAWCODE.
           MOVE QTS-LINE TO TS-LINE.

           IF ( QTS-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO TS-LTOUCH-DATE
           ELSE
              MOVE QTS-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-LTOUCH-DATE.

           MOVE QTS-SSNO-1 TO TS-SSNO(1).
           MOVE QTS-MAKERCD-1 TO TS-MAKERCD(1).
           MOVE QTS-SSNO-2 TO TS-SSNO(2).
           MOVE QTS-MAKERCD-2 TO TS-MAKERCD(2).
           MOVE QTS-SSNO-3 TO TS-SSNO(3).
           MOVE QTS-MAKERCD-3 TO TS-MAKERCD(3).
           MOVE QTS-SSNO-4 TO TS-SSNO(4).
           MOVE QTS-MAKERCD-4 TO TS-MAKERCD(4).
           MOVE QTS-PURCD TO TS-PURCD.
           MOVE QTS-SRCD TO TS-SRCD.
           MOVE QTS-COLLCD TO TS-COLLCD.
           MOVE QTS-SERVACCT TO TS-SERVACCT.
           MOVE QTS-LOANTYPE TO TS-LOANTYPE.
           MOVE QTS-SUBCLASS TO TS-SUBCLASS.

           IF ( QTS-LOANDATE = "1900-01-01" )
              MOVE ZEROES TO TS-LOANDATE
           ELSE
              MOVE QTS-LOANDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-LOANDATE.


           IF ( QTS-INTDATE = "1900-01-01" )
              MOVE ZEROES TO TS-INTDATE
           ELSE
              MOVE QTS-INTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INTDATE.

           MOVE QTS-PROCEEDS TO TS-PROCEEDS.
           MOVE QTS-LNAMT TO TS-LNAMT.
           MOVE QTS-FINCHG TO TS-FINCHG.
           MOVE QTS-INTCHG TO TS-INTCHG.
           MOVE QTS-SERCHG TO TS-SERCHG.
           MOVE QTS-EXTCHG TO TS-EXTCHG.
           MOVE QTS-MAINTFEE TO TS-MAINTFEE.
           MOVE QTS-ORGTERM TO TS-ORGTERM.
           MOVE QTS-1STPYAMT TO TS-1STPYAMT.
           MOVE QTS-NOREGPY TO TS-NOREGPY.
           MOVE QTS-REGPYAMT TO TS-REGPYAMT.
           MOVE QTS-LASTPYAMT TO TS-LASTPYAMT.
           MOVE QTS-FEECODE-1 TO TS-FEECODE(1).
           MOVE QTS-FEEAMT-1 TO TS-FEEAMT(1).
           MOVE QTS-FEEOURS-1 TO TS-FEEOURS(1).
           MOVE QTS-FEECODE-2 TO TS-FEECODE(2).
           MOVE QTS-FEEAMT-2 TO TS-FEEAMT(2).
           MOVE QTS-FEEOURS-2 TO TS-FEEOURS(2).
           MOVE QTS-FEECODE-3 TO TS-FEECODE(3).
           MOVE QTS-FEEAMT-3 TO TS-FEEAMT(3).
           MOVE QTS-FEEOURS-3 TO TS-FEEOURS(3).
           MOVE QTS-FEECODE-4 TO TS-FEECODE(4).
           MOVE QTS-FEEAMT-4 TO TS-FEEAMT(4).
           MOVE QTS-FEEOURS-4 TO TS-FEEOURS(4).
           MOVE QTS-FEECODE-5 TO TS-FEECODE(5).
           MOVE QTS-FEEAMT-5 TO TS-FEEAMT(5).
           MOVE QTS-FEEOURS-5 TO TS-FEEOURS(5).
           MOVE QTS-FEECODE-6 TO TS-FEECODE(6).
           MOVE QTS-FEEAMT-6 TO TS-FEEAMT(6).
           MOVE QTS-FEEOURS-6 TO TS-FEEOURS(6).
           MOVE QTS-FEECODE-7 TO TS-FEECODE(7).
           MOVE QTS-FEEAMT-7 TO TS-FEEAMT(7).
           MOVE QTS-FEEOURS-7 TO TS-FEEOURS(7).
           MOVE QTS-FEECODE-8 TO TS-FEECODE(8).
           MOVE QTS-FEEAMT-8 TO TS-FEEAMT(8).
           MOVE QTS-FEEOURS-8 TO TS-FEEOURS(8).
           MOVE QTS-FEECODE-9 TO TS-FEECODE(9).
           MOVE QTS-FEEAMT-9 TO TS-FEEAMT(9).
           MOVE QTS-FEEOURS-9 TO TS-FEEOURS(9).
           MOVE QTS-FEECODE-10 TO TS-FEECODE(10).
           MOVE QTS-FEEAMT-10 TO TS-FEEAMT(10).
           MOVE QTS-FEEOURS-10 TO TS-FEEOURS(10).
           MOVE QTS-FEECODE-11 TO TS-FEECODE(11).
           MOVE QTS-FEEAMT-11 TO TS-FEEAMT(11).
           MOVE QTS-FEEOURS-11 TO TS-FEEOURS(11).
           MOVE QTS-FEECODE-12 TO TS-FEECODE(12).
           MOVE QTS-FEEAMT-12 TO TS-FEEAMT(12).
           MOVE QTS-FEEOURS-12 TO TS-FEEOURS(12).
           MOVE QTS-FEECODE-13 TO TS-FEECODE(13).
           MOVE QTS-FEEAMT-13 TO TS-FEEAMT(13).
           MOVE QTS-FEEOURS-13 TO TS-FEEOURS(13).
           MOVE QTS-FEECODE-14 TO TS-FEECODE(14).
           MOVE QTS-FEEAMT-14 TO TS-FEEAMT(14).
           MOVE QTS-FEEOURS-14 TO TS-FEEOURS(14).
           MOVE QTS-FEECODE-15 TO TS-FEECODE(15).
           MOVE QTS-FEEAMT-15 TO TS-FEEAMT(15).
           MOVE QTS-FEEOURS-15 TO TS-FEEOURS(15).
           MOVE QTS-FEECODE-16 TO TS-FEECODE(16).
           MOVE QTS-FEEAMT-16 TO TS-FEEAMT(16).
           MOVE QTS-FEEOURS-16 TO TS-FEEOURS(16).
           MOVE QTS-FEECODE-17 TO TS-FEECODE(17).
           MOVE QTS-FEEAMT-17 TO TS-FEEAMT(17).
           MOVE QTS-FEEOURS-17 TO TS-FEEOURS(17).
           MOVE QTS-FEECODE-18 TO TS-FEECODE(18).
           MOVE QTS-FEEAMT-18 TO TS-FEEAMT(18).
           MOVE QTS-FEEOURS-18 TO TS-FEEOURS(18).
           MOVE QTS-FEECODE-19 TO TS-FEECODE(19).
           MOVE QTS-FEEAMT-19 TO TS-FEEAMT(19).
           MOVE QTS-FEEOURS-19 TO TS-FEEOURS(19).
           MOVE QTS-FEECODE-20 TO TS-FEECODE(20).
           MOVE QTS-FEEAMT-20 TO TS-FEEAMT(20).
           MOVE QTS-FEEOURS-20 TO TS-FEEOURS(20).
           MOVE QTS-INSURED TO TS-INSURED.
           MOVE QTS-INSTYPES TO TS-INSTYPES.
           MOVE QTS-INSELIGIBLE TO TS-INSELIGIBLE.

           MOVE QTS-INSCOMP-1 TO TS-INSCOMP(1).
           MOVE QTS-INSOURS-1 TO TS-INSOURS(1).
           IF ( QTS-INSEFF-DATE-1 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEFF-DATE(1)
           ELSE
              MOVE QTS-INSEFF-DATE-1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEFF-DATE(1).
           IF ( QTS-INSEXP-DATE-1 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEXP-DATE(1)
           ELSE
              MOVE QTS-INSEXP-DATE-1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEXP-DATE(1).
           MOVE QTS-INSPREM-1 TO TS-INSPREM(1).
           MOVE QTS-INSCOMM-1 TO TS-INSCOMM(1).
           MOVE QTS-INSANTICERN-1 TO TS-INSANTICERN(1).
           MOVE QTS-INSEARNINGS-1 TO TS-INSEARNINGS(1).

           MOVE QTS-INSCOMP-2 TO TS-INSCOMP(2).
           MOVE QTS-INSOURS-2 TO TS-INSOURS(2).

           IF ( QTS-INSEFF-DATE-2 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEFF-DATE(2)
           ELSE
              MOVE QTS-INSEFF-DATE-2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEFF-DATE(2).


           IF ( QTS-INSEXP-DATE-2 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEXP-DATE(2)
           ELSE
              MOVE QTS-INSEXP-DATE-2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEXP-DATE(2).

           MOVE QTS-INSPREM-2 TO TS-INSPREM(2).
           MOVE QTS-INSCOMM-2 TO TS-INSCOMM(2).
           MOVE QTS-INSANTICERN-2 TO TS-INSANTICERN(2).
           MOVE QTS-INSEARNINGS-2 TO TS-INSEARNINGS(2).

           MOVE QTS-INSCOMP-3 TO TS-INSCOMP(3).
           MOVE QTS-INSOURS-3 TO TS-INSOURS(3).

           IF ( QTS-INSEFF-DATE-3 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEFF-DATE(3)
           ELSE
              MOVE QTS-INSEFF-DATE-3 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEFF-DATE(3).


           IF ( QTS-INSEXP-DATE-3 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEXP-DATE(3)
           ELSE
              MOVE QTS-INSEXP-DATE-3 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEXP-DATE(3).

           MOVE QTS-INSPREM-3 TO TS-INSPREM(3).
           MOVE QTS-INSCOMM-3 TO TS-INSCOMM(3).
           MOVE QTS-INSANTICERN-3 TO TS-INSANTICERN(3).
           MOVE QTS-INSEARNINGS-3 TO TS-INSEARNINGS(3).

           MOVE QTS-INSCOMP-4 TO TS-INSCOMP(4).
           MOVE QTS-INSOURS-4 TO TS-INSOURS(4).

           IF ( QTS-INSEFF-DATE-4 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEFF-DATE(4)
           ELSE
              MOVE QTS-INSEFF-DATE-4 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEFF-DATE(4).


           IF ( QTS-INSEXP-DATE-4 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEXP-DATE(4)
           ELSE
              MOVE QTS-INSEXP-DATE-4 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEXP-DATE(4).

           MOVE QTS-INSPREM-4 TO TS-INSPREM(4).
           MOVE QTS-INSCOMM-4 TO TS-INSCOMM(4).
           MOVE QTS-INSANTICERN-4 TO TS-INSANTICERN(4).
           MOVE QTS-INSEARNINGS-4 TO TS-INSEARNINGS(4).
           MOVE QTS-INSCOMP-5 TO TS-INSCOMP(5).
           MOVE QTS-INSOURS-5 TO TS-INSOURS(5).

           IF ( QTS-INSEFF-DATE-5 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEFF-DATE(5)
           ELSE
              MOVE QTS-INSEFF-DATE-5 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEFF-DATE(5).
           IF ( QTS-INSEXP-DATE-5 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEXP-DATE(5)
           ELSE
              MOVE QTS-INSEXP-DATE-5 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEXP-DATE(5).

           MOVE QTS-INSPREM-5 TO TS-INSPREM(5).
           MOVE QTS-INSCOMM-5 TO TS-INSCOMM(5).
           MOVE QTS-INSANTICERN-5 TO TS-INSANTICERN(5).
           MOVE QTS-INSEARNINGS-5 TO TS-INSEARNINGS(5).
           MOVE QTS-INSCOMP-6 TO TS-INSCOMP(6).
           MOVE QTS-INSOURS-6 TO TS-INSOURS(6).

           IF ( QTS-INSEFF-DATE-6 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEFF-DATE(6)
           ELSE
              MOVE QTS-INSEFF-DATE-6 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEFF-DATE(6).

           IF ( QTS-INSEXP-DATE-6 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEXP-DATE(6)
           ELSE
              MOVE QTS-INSEXP-DATE-6 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEXP-DATE(6).

           MOVE QTS-INSPREM-6 TO TS-INSPREM(6).
           MOVE QTS-INSCOMM-6 TO TS-INSCOMM(6).
           MOVE QTS-INSANTICERN-6 TO TS-INSANTICERN(6).
           MOVE QTS-INSEARNINGS-6 TO TS-INSEARNINGS(6).
           MOVE QTS-INSCOMP-7 TO TS-INSCOMP(7).
           MOVE QTS-INSOURS-7 TO TS-INSOURS(7).

           IF ( QTS-INSEFF-DATE-7 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEFF-DATE(7)
           ELSE
              MOVE QTS-INSEFF-DATE-7 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEFF-DATE(7).


           IF ( QTS-INSEXP-DATE-7 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEXP-DATE(7)
           ELSE
              MOVE QTS-INSEXP-DATE-7 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEXP-DATE(7).

           MOVE QTS-INSPREM-7 TO TS-INSPREM(7).
           MOVE QTS-INSCOMM-7 TO TS-INSCOMM(7).
           MOVE QTS-INSANTICERN-7 TO TS-INSANTICERN(7).
           MOVE QTS-INSEARNINGS-7 TO TS-INSEARNINGS(7).
           MOVE QTS-INSCOMP-8 TO TS-INSCOMP(8).
           MOVE QTS-INSOURS-8 TO TS-INSOURS(8).

           IF ( QTS-INSEFF-DATE-8 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEFF-DATE(8)
           ELSE
              MOVE QTS-INSEFF-DATE-8 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEFF-DATE(8).


           IF ( QTS-INSEXP-DATE-8 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEXP-DATE(8)
           ELSE
              MOVE QTS-INSEXP-DATE-8 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEXP-DATE(8).

           MOVE QTS-INSPREM-8 TO TS-INSPREM(8).
           MOVE QTS-INSCOMM-8 TO TS-INSCOMM(8).
           MOVE QTS-INSANTICERN-8 TO TS-INSANTICERN(8).
           MOVE QTS-INSEARNINGS-8 TO TS-INSEARNINGS(8).
           MOVE QTS-INSCOMP-9 TO TS-INSCOMP(9).
           MOVE QTS-INSOURS-9 TO TS-INSOURS(9).

           IF ( QTS-INSEFF-DATE-9 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEFF-DATE(9)
           ELSE
              MOVE QTS-INSEFF-DATE-9 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEFF-DATE(9).

           IF ( QTS-INSEXP-DATE-9 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEXP-DATE(9)
           ELSE
              MOVE QTS-INSEXP-DATE-9 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEXP-DATE(9).

           MOVE QTS-INSPREM-9 TO TS-INSPREM(9).
           MOVE QTS-INSCOMM-9 TO TS-INSCOMM(9).
           MOVE QTS-INSANTICERN-9 TO TS-INSANTICERN(9).
           MOVE QTS-INSEARNINGS-9 TO TS-INSEARNINGS(9).
           MOVE QTS-INSCOMP-10 TO TS-INSCOMP(10).
           MOVE QTS-INSOURS-10 TO TS-INSOURS(10).

           IF ( QTS-INSEFF-DATE-10 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEFF-DATE(10)
           ELSE
              MOVE QTS-INSEFF-DATE-10 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEFF-DATE(10).
           IF ( QTS-INSEXP-DATE-10 = "1900-01-01" )
              MOVE ZEROES TO TS-INSEXP-DATE(10)
           ELSE
              MOVE QTS-INSEXP-DATE-10 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-INSEXP-DATE(10).

           MOVE QTS-INSPREM-10 TO TS-INSPREM(10).
           MOVE QTS-INSCOMM-10 TO TS-INSCOMM(10).
           MOVE QTS-INSANTICERN-10 TO TS-INSANTICERN(10).
           MOVE QTS-INSEARNINGS-10 TO TS-INSEARNINGS(10).
           MOVE QTS-DLCOMM-PP-CMSI TO TS-DLCOMM-PP-CMSI.
           MOVE QTS-DLCOMM-O3-CMSI TO TS-DLCOMM-O3-CMSI.
           MOVE QTS-INS-TAX-COUNTY-CD TO TS-INS-TAX-COUNTY-CD.
           MOVE QTS-INS-TAX-CITY-CD TO TS-INS-TAX-CITY-CD.
           MOVE QTS-TILLNO TO TS-TILLNO.
           MOVE QTS-UNWIND-REASON-CODE TO TS-UNWIND-REASON-CODE.
           MOVE QTS-DLNO TO TS-DLNO.
           MOVE QTS-DLRECOURSE TO TS-DLRECOURSE.
           MOVE QTS-DLPARTRCAMT TO TS-DLPARTRCAMT.
           MOVE QTS-PURBAL TO TS-PURBAL.
           MOVE QTS-DLRVPRIN-1 TO TS-DLRVPRIN(1).
           MOVE QTS-DLRVPRIN-2 TO TS-DLRVPRIN(2).
           MOVE QTS-DLRVPRIN-3 TO TS-DLRVPRIN(3).
           MOVE QTS-DLRVCHGS-1 TO TS-DLRVCHGS(1).
           MOVE QTS-DLRVCHGS-2 TO TS-DLRVCHGS(2).
           MOVE QTS-DLRVCHGS-3 TO TS-DLRVCHGS(3).
           MOVE QTS-DL-RVCHGS-CODE-1 TO TS-DL-RVCHGS-CODE(1).
           MOVE QTS-DL-RVCHGS-CODE-2 TO TS-DL-RVCHGS-CODE(2).
           MOVE QTS-DL-RVCHGS-CODE-3 TO TS-DL-RVCHGS-CODE(3).
           MOVE QTS-DL-PARVHELD-CODE TO TS-DL-PARVHELD-CODE.
           MOVE QTS-DL-PARVPAID-CODE TO TS-DL-PARVPAID-CODE.
           MOVE QTS-DLPARVHELD TO TS-DLPARVHELD.
           MOVE QTS-DLPARVPAID TO TS-DLPARVPAID.
           MOVE QTS-DLDISC TO TS-DLDISC.
           MOVE QTS-ANTICTERM TO TS-ANTICTERM.
           MOVE QTS-ANTICERN-1 TO TS-ANTICERN(1).
           MOVE QTS-ANTICERN-2 TO TS-ANTICERN(2).
           MOVE QTS-ANTICERN-3 TO TS-ANTICERN(3).
           MOVE QTS-ANTICERN-4 TO TS-ANTICERN(4).
           MOVE QTS-ANTICERN-5 TO TS-ANTICERN(5).
           MOVE QTS-ANTICERN-6 TO TS-ANTICERN(6).
           MOVE QTS-ANTICERN-7 TO TS-ANTICERN(7).
           MOVE QTS-ANTICERN-8 TO TS-ANTICERN(8).
           MOVE QTS-ANTICERN-9 TO TS-ANTICERN(9).
           MOVE QTS-ANTICERN-10 TO TS-ANTICERN(10).
           MOVE QTS-EARNINGS-1 TO TS-EARNINGS(1).
           MOVE QTS-EARNINGS-2 TO TS-EARNINGS(2).
           MOVE QTS-EARNINGS-3 TO TS-EARNINGS(3).
           MOVE QTS-EARNINGS-4 TO TS-EARNINGS(4).
           MOVE QTS-EARNINGS-5 TO TS-EARNINGS(5).
           MOVE QTS-EARNINGS-6 TO TS-EARNINGS(6).
           MOVE QTS-EARNINGS-7 TO TS-EARNINGS(7).
           MOVE QTS-EARNINGS-8 TO TS-EARNINGS(8).
           MOVE QTS-EARNINGS-9 TO TS-EARNINGS(9).
           MOVE QTS-EARNINGS-10 TO TS-EARNINGS(10).
           MOVE QTS-PRLNNO TO TS-PRLNNO.
           MOVE QTS-INSPREM-DLADJ TO TS-INSPREM-DLADJ.
           MOVE QTS-PRDLDISC-UNEARN TO TS-PRDLDISC-UNEARN.
           MOVE QTS-SP-LNCOST-FRMLA TO TS-SP-LNCOST-FRMLA.
           MOVE QTS-DLPAID TO TS-DLPAID.
           MOVE QTS-MIP-FG TO TS-MIP-FG.
           MOVE QTS-SPRCLASS TO TS-SPRCLASS.
           MOVE QTS-O3INS-DLPROC TO TS-O3INS-DLPROC.
           MOVE QTS-USERID TO TS-USERID.

           IF ( QTS-POSTDATE = "1900-01-01" )
              MOVE ZEROES TO TS-POSTDATE
           ELSE
              MOVE QTS-POSTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-POSTDATE.

           MOVE QTS-POSTTIME TO TS-POSTTIME.
           MOVE QTS-TAX-AMT TO TS-TAX-AMT.
           MOVE QTS-APPROVED-BY-USER TO TS-APPROVED-BY-USER.
           MOVE QTS-APPROVED-BY-CODE TO TS-APPROVED-BY-CODE.

           IF ( QTS-APPROVED-BY-DATE = "1900-01-01" )
              MOVE ZEROES TO TS-APPROVED-BY-DATE
           ELSE
              MOVE QTS-APPROVED-BY-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TS-APPROVED-BY-DATE.

           MOVE QTS-APPROVED-BY-TIME TO TS-APPROVED-BY-TIME.
 
      *-----------------------------------------------------------------
      *    LOAD QTRS-REC FROM TRS-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-TRS-FIELDS.
           MOVE TS-BRNO TO QTS-BRNO.

           IF ( TS-DATE = ZEROES )
              MOVE "1900-01-01" TO QTS-DATE
           ELSE
              MOVE TS-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-DATE.

           MOVE TS-CLASS TO QTS-CLASS.
           MOVE TS-ACCTNO TO QTS-ACCTNO.
           MOVE TS-LAWCODE TO QTS-LAWCODE.
           MOVE TS-LINE TO QTS-LINE.

           IF ( TS-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTS-LTOUCH-DATE
           ELSE
              MOVE TS-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-LTOUCH-DATE.

           MOVE TS-SSNO(1) TO QTS-SSNO-1.
           MOVE TS-MAKERCD(1) TO QTS-MAKERCD-1.
           MOVE TS-SSNO(2) TO QTS-SSNO-2.
           MOVE TS-MAKERCD(2) TO QTS-MAKERCD-2.
           MOVE TS-SSNO(3) TO QTS-SSNO-3.
           MOVE TS-MAKERCD(3) TO QTS-MAKERCD-3.
           MOVE TS-SSNO(4) TO QTS-SSNO-4.
           MOVE TS-MAKERCD(4) TO QTS-MAKERCD-4.
           MOVE TS-PURCD TO QTS-PURCD.
           MOVE TS-SRCD TO QTS-SRCD.
           MOVE TS-COLLCD TO QTS-COLLCD.
           MOVE TS-SERVACCT TO QTS-SERVACCT.
           MOVE TS-LOANTYPE TO QTS-LOANTYPE.
           MOVE TS-SUBCLASS TO QTS-SUBCLASS.

           IF ( TS-LOANDATE = ZEROES )
              MOVE "1900-01-01" TO QTS-LOANDATE
           ELSE
              MOVE TS-LOANDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-LOANDATE.


           IF ( TS-INTDATE = ZEROES )
              MOVE "1900-01-01" TO QTS-INTDATE
           ELSE
              MOVE TS-INTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INTDATE.

           MOVE TS-PROCEEDS TO QTS-PROCEEDS.
           MOVE TS-LNAMT TO QTS-LNAMT.
           MOVE TS-FINCHG TO QTS-FINCHG.
           MOVE TS-INTCHG TO QTS-INTCHG.
           MOVE TS-SERCHG TO QTS-SERCHG.
           MOVE TS-EXTCHG TO QTS-EXTCHG.
           MOVE TS-MAINTFEE TO QTS-MAINTFEE.
           MOVE TS-ORGTERM TO QTS-ORGTERM.
           MOVE TS-1STPYAMT TO QTS-1STPYAMT.
           MOVE TS-NOREGPY TO QTS-NOREGPY.
           MOVE TS-REGPYAMT TO QTS-REGPYAMT.
           MOVE TS-LASTPYAMT TO QTS-LASTPYAMT.
           MOVE TS-FEECODE(1) TO QTS-FEECODE-1.
           MOVE TS-FEEAMT(1) TO QTS-FEEAMT-1.
           MOVE TS-FEEOURS(1) TO QTS-FEEOURS-1.
           MOVE TS-FEECODE(2) TO QTS-FEECODE-2.
           MOVE TS-FEEAMT(2) TO QTS-FEEAMT-2.
           MOVE TS-FEEOURS(2) TO QTS-FEEOURS-2.
           MOVE TS-FEECODE(3) TO QTS-FEECODE-3.
           MOVE TS-FEEAMT(3) TO QTS-FEEAMT-3.
           MOVE TS-FEEOURS(3) TO QTS-FEEOURS-3.
           MOVE TS-FEECODE(4) TO QTS-FEECODE-4.
           MOVE TS-FEEAMT(4) TO QTS-FEEAMT-4.
           MOVE TS-FEEOURS(4) TO QTS-FEEOURS-4.
           MOVE TS-FEECODE(5) TO QTS-FEECODE-5.
           MOVE TS-FEEAMT(5) TO QTS-FEEAMT-5.
           MOVE TS-FEEOURS(5) TO QTS-FEEOURS-5.
           MOVE TS-FEECODE(6) TO QTS-FEECODE-6.
           MOVE TS-FEEAMT(6) TO QTS-FEEAMT-6.
           MOVE TS-FEEOURS(6) TO QTS-FEEOURS-6.
           MOVE TS-FEECODE(7) TO QTS-FEECODE-7.
           MOVE TS-FEEAMT(7) TO QTS-FEEAMT-7.
           MOVE TS-FEEOURS(7) TO QTS-FEEOURS-7.
           MOVE TS-FEECODE(8) TO QTS-FEECODE-8.
           MOVE TS-FEEAMT(8) TO QTS-FEEAMT-8.
           MOVE TS-FEEOURS(8) TO QTS-FEEOURS-8.
           MOVE TS-FEECODE(9) TO QTS-FEECODE-9.
           MOVE TS-FEEAMT(9) TO QTS-FEEAMT-9.
           MOVE TS-FEEOURS(9) TO QTS-FEEOURS-9.
           MOVE TS-FEECODE(10) TO QTS-FEECODE-10.
           MOVE TS-FEEAMT(10) TO QTS-FEEAMT-10.
           MOVE TS-FEEOURS(10) TO QTS-FEEOURS-10.
           MOVE TS-FEECODE(11) TO QTS-FEECODE-11.
           MOVE TS-FEEAMT(11) TO QTS-FEEAMT-11.
           MOVE TS-FEEOURS(11) TO QTS-FEEOURS-11.
           MOVE TS-FEECODE(12) TO QTS-FEECODE-12.
           MOVE TS-FEEAMT(12) TO QTS-FEEAMT-12.
           MOVE TS-FEEOURS(12) TO QTS-FEEOURS-12.
           MOVE TS-FEECODE(13) TO QTS-FEECODE-13.
           MOVE TS-FEEAMT(13) TO QTS-FEEAMT-13.
           MOVE TS-FEEOURS(13) TO QTS-FEEOURS-13.
           MOVE TS-FEECODE(14) TO QTS-FEECODE-14.
           MOVE TS-FEEAMT(14) TO QTS-FEEAMT-14.
           MOVE TS-FEEOURS(14) TO QTS-FEEOURS-14.
           MOVE TS-FEECODE(15) TO QTS-FEECODE-15.
           MOVE TS-FEEAMT(15) TO QTS-FEEAMT-15.
           MOVE TS-FEEOURS(15) TO QTS-FEEOURS-15.
           MOVE TS-FEECODE(16) TO QTS-FEECODE-16.
           MOVE TS-FEEAMT(16) TO QTS-FEEAMT-16.
           MOVE TS-FEEOURS(16) TO QTS-FEEOURS-16.
           MOVE TS-FEECODE(17) TO QTS-FEECODE-17.
           MOVE TS-FEEAMT(17) TO QTS-FEEAMT-17.
           MOVE TS-FEEOURS(17) TO QTS-FEEOURS-17.
           MOVE TS-FEECODE(18) TO QTS-FEECODE-18.
           MOVE TS-FEEAMT(18) TO QTS-FEEAMT-18.
           MOVE TS-FEEOURS(18) TO QTS-FEEOURS-18.
           MOVE TS-FEECODE(19) TO QTS-FEECODE-19.
           MOVE TS-FEEAMT(19) TO QTS-FEEAMT-19.
           MOVE TS-FEEOURS(19) TO QTS-FEEOURS-19.
           MOVE TS-FEECODE(20) TO QTS-FEECODE-20.
           MOVE TS-FEEAMT(20) TO QTS-FEEAMT-20.
           MOVE TS-FEEOURS(20) TO QTS-FEEOURS-20.
           MOVE TS-INSURED TO QTS-INSURED.
           MOVE TS-INSTYPES TO QTS-INSTYPES.
           MOVE TS-INSELIGIBLE TO QTS-INSELIGIBLE.
           MOVE TS-INSCOMP(1) TO QTS-INSCOMP-1.
           MOVE TS-INSOURS(1) TO QTS-INSOURS-1.

           IF ( TS-INSEFF-DATE(1) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEFF-DATE-1
           ELSE
              MOVE TS-INSEFF-DATE(1) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEFF-DATE-1.


           IF ( TS-INSEXP-DATE(1) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEXP-DATE-1
           ELSE
              MOVE TS-INSEXP-DATE(1) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEXP-DATE-1.

           MOVE TS-INSPREM(1) TO QTS-INSPREM-1.
           MOVE TS-INSCOMM(1) TO QTS-INSCOMM-1.
           MOVE TS-INSANTICERN(1) TO QTS-INSANTICERN-1.
           MOVE TS-INSEARNINGS(1) TO QTS-INSEARNINGS-1.

           MOVE TS-INSCOMP(2) TO QTS-INSCOMP-2.
           MOVE TS-INSOURS(2) TO QTS-INSOURS-2.

           IF ( TS-INSEFF-DATE(2) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEFF-DATE-2
           ELSE
              MOVE TS-INSEFF-DATE(2) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEFF-DATE-2.


           IF ( TS-INSEXP-DATE(2) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEXP-DATE-2
           ELSE
              MOVE TS-INSEXP-DATE(2) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEXP-DATE-2.

           MOVE TS-INSPREM(2) TO QTS-INSPREM-2.
           MOVE TS-INSCOMM(2) TO QTS-INSCOMM-2.
           MOVE TS-INSANTICERN(2) TO QTS-INSANTICERN-2.
           MOVE TS-INSEARNINGS(2) TO QTS-INSEARNINGS-2.
           MOVE TS-INSCOMP(3) TO QTS-INSCOMP-3.
           MOVE TS-INSOURS(3) TO QTS-INSOURS-3.

           IF ( TS-INSEFF-DATE(3) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEFF-DATE-3
           ELSE
              MOVE TS-INSEFF-DATE(3) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEFF-DATE-3.


           IF ( TS-INSEXP-DATE(3) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEXP-DATE-3
           ELSE
              MOVE TS-INSEXP-DATE(3) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEXP-DATE-3.

           MOVE TS-INSPREM(3) TO QTS-INSPREM-3.
           MOVE TS-INSCOMM(3) TO QTS-INSCOMM-3.
           MOVE TS-INSANTICERN(3) TO QTS-INSANTICERN-3.
           MOVE TS-INSEARNINGS(3) TO QTS-INSEARNINGS-3.
           MOVE TS-INSCOMP(4) TO QTS-INSCOMP-4.
           MOVE TS-INSOURS(4) TO QTS-INSOURS-4.

           IF ( TS-INSEFF-DATE(4) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEFF-DATE-4
           ELSE
              MOVE TS-INSEFF-DATE(4) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEFF-DATE-4.


           IF ( TS-INSEXP-DATE(4) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEXP-DATE-4
           ELSE
              MOVE TS-INSEXP-DATE(4) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEXP-DATE-4.

           MOVE TS-INSPREM(4) TO QTS-INSPREM-4.
           MOVE TS-INSCOMM(4) TO QTS-INSCOMM-4.
           MOVE TS-INSANTICERN(4) TO QTS-INSANTICERN-4.
           MOVE TS-INSEARNINGS(4) TO QTS-INSEARNINGS-4.
           MOVE TS-INSCOMP(5) TO QTS-INSCOMP-5.
           MOVE TS-INSOURS(5) TO QTS-INSOURS-5.

           IF ( TS-INSEFF-DATE(5) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEFF-DATE-5
           ELSE
              MOVE TS-INSEFF-DATE(5) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEFF-DATE-5.


           IF ( TS-INSEXP-DATE(5) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEXP-DATE-5
           ELSE
              MOVE TS-INSEXP-DATE(5) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEXP-DATE-5.

           MOVE TS-INSPREM(5) TO QTS-INSPREM-5.
           MOVE TS-INSCOMM(5) TO QTS-INSCOMM-5.
           MOVE TS-INSANTICERN(5) TO QTS-INSANTICERN-5.
           MOVE TS-INSEARNINGS(5) TO QTS-INSEARNINGS-5.
           MOVE TS-INSCOMP(6) TO QTS-INSCOMP-6.
           MOVE TS-INSOURS(6) TO QTS-INSOURS-6.

           IF ( TS-INSEFF-DATE(6) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEFF-DATE-6
           ELSE
              MOVE TS-INSEFF-DATE(6) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEFF-DATE-6.


           IF ( TS-INSEXP-DATE(6) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEXP-DATE-6
           ELSE
              MOVE TS-INSEXP-DATE(6) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEXP-DATE-6.

           MOVE TS-INSPREM(6) TO QTS-INSPREM-6.
           MOVE TS-INSCOMM(6) TO QTS-INSCOMM-6.
           MOVE TS-INSANTICERN(6) TO QTS-INSANTICERN-6.
           MOVE TS-INSEARNINGS(6) TO QTS-INSEARNINGS-6.
           MOVE TS-INSCOMP(7) TO QTS-INSCOMP-7.
           MOVE TS-INSOURS(7) TO QTS-INSOURS-7.

           IF ( TS-INSEFF-DATE(7) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEFF-DATE-7
           ELSE
              MOVE TS-INSEFF-DATE(7) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEFF-DATE-7.


           IF ( TS-INSEXP-DATE(7) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEXP-DATE-7
           ELSE
              MOVE TS-INSEXP-DATE(7) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEXP-DATE-7.

           MOVE TS-INSPREM(7) TO QTS-INSPREM-7.
           MOVE TS-INSCOMM(7) TO QTS-INSCOMM-7.
           MOVE TS-INSANTICERN(7) TO QTS-INSANTICERN-7.
           MOVE TS-INSEARNINGS(7) TO QTS-INSEARNINGS-7.
           MOVE TS-INSCOMP(8) TO QTS-INSCOMP-8.
           MOVE TS-INSOURS(8) TO QTS-INSOURS-8.

           IF ( TS-INSEFF-DATE(8) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEFF-DATE-8
           ELSE
              MOVE TS-INSEFF-DATE(8) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEFF-DATE-8.


           IF ( TS-INSEXP-DATE(8) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEXP-DATE-8
           ELSE
              MOVE TS-INSEXP-DATE(8) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEXP-DATE-8.

           MOVE TS-INSPREM(8) TO QTS-INSPREM-8.
           MOVE TS-INSCOMM(8) TO QTS-INSCOMM-8.
           MOVE TS-INSANTICERN(8) TO QTS-INSANTICERN-8.
           MOVE TS-INSEARNINGS(8) TO QTS-INSEARNINGS-8.
           MOVE TS-INSCOMP(9) TO QTS-INSCOMP-9.
           MOVE TS-INSOURS(9) TO QTS-INSOURS-9.

           IF ( TS-INSEFF-DATE(9) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEFF-DATE-9
           ELSE
              MOVE TS-INSEFF-DATE(9) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEFF-DATE-9.


           IF ( TS-INSEXP-DATE(9) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEXP-DATE-9
           ELSE
              MOVE TS-INSEXP-DATE(9) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEXP-DATE-9.

           MOVE TS-INSPREM(9) TO QTS-INSPREM-9.
           MOVE TS-INSCOMM(9) TO QTS-INSCOMM-9.
           MOVE TS-INSANTICERN(9) TO QTS-INSANTICERN-9.
           MOVE TS-INSEARNINGS(9) TO QTS-INSEARNINGS-9.
           MOVE TS-INSCOMP(10) TO QTS-INSCOMP-10.
           MOVE TS-INSOURS(10) TO QTS-INSOURS-10.

           IF ( TS-INSEFF-DATE(10) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEFF-DATE-10
           ELSE
              MOVE TS-INSEFF-DATE(10) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEFF-DATE-10.


           IF ( TS-INSEXP-DATE(10) = ZEROES )
              MOVE "1900-01-01" TO QTS-INSEXP-DATE-10
           ELSE
              MOVE TS-INSEXP-DATE(10) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-INSEXP-DATE-10.

           MOVE TS-INSPREM(10) TO QTS-INSPREM-10.
           MOVE TS-INSCOMM(10) TO QTS-INSCOMM-10.
           MOVE TS-INSANTICERN(10) TO QTS-INSANTICERN-10.
           MOVE TS-INSEARNINGS(10) TO QTS-INSEARNINGS-10.

           MOVE TS-DLCOMM-PP-CMSI TO QTS-DLCOMM-PP-CMSI.
           MOVE TS-DLCOMM-O3-CMSI TO QTS-DLCOMM-O3-CMSI.
           MOVE TS-INS-TAX-COUNTY-CD TO QTS-INS-TAX-COUNTY-CD.
           MOVE TS-INS-TAX-CITY-CD TO QTS-INS-TAX-CITY-CD.
           MOVE TS-TILLNO TO QTS-TILLNO.
           MOVE TS-UNWIND-REASON-CODE TO QTS-UNWIND-REASON-CODE.
           MOVE TS-DLNO TO QTS-DLNO.
           MOVE TS-DLRECOURSE TO QTS-DLRECOURSE.
           MOVE TS-DLPARTRCAMT TO QTS-DLPARTRCAMT.
           MOVE TS-PURBAL TO QTS-PURBAL.
           MOVE TS-DLRVPRIN(1) TO QTS-DLRVPRIN-1.
           MOVE TS-DLRVPRIN(2) TO QTS-DLRVPRIN-2.
           MOVE TS-DLRVPRIN(3) TO QTS-DLRVPRIN-3.
           MOVE TS-DLRVCHGS(1) TO QTS-DLRVCHGS-1.
           MOVE TS-DLRVCHGS(2) TO QTS-DLRVCHGS-2.
           MOVE TS-DLRVCHGS(3) TO QTS-DLRVCHGS-3.
           MOVE TS-DL-RVCHGS-CODE(1) TO QTS-DL-RVCHGS-CODE-1.
           MOVE TS-DL-RVCHGS-CODE(2) TO QTS-DL-RVCHGS-CODE-2.
           MOVE TS-DL-RVCHGS-CODE(3) TO QTS-DL-RVCHGS-CODE-3.
           MOVE TS-DL-PARVHELD-CODE TO QTS-DL-PARVHELD-CODE.
           MOVE TS-DL-PARVPAID-CODE TO QTS-DL-PARVPAID-CODE.
           MOVE TS-DLPARVHELD TO QTS-DLPARVHELD.
           MOVE TS-DLPARVPAID TO QTS-DLPARVPAID.
           MOVE TS-DLDISC TO QTS-DLDISC.
           MOVE TS-ANTICTERM TO QTS-ANTICTERM.
           MOVE TS-ANTICERN(1) TO QTS-ANTICERN-1.
           MOVE TS-ANTICERN(2) TO QTS-ANTICERN-2.
           MOVE TS-ANTICERN(3) TO QTS-ANTICERN-3.
           MOVE TS-ANTICERN(4) TO QTS-ANTICERN-4.
           MOVE TS-ANTICERN(5) TO QTS-ANTICERN-5.
           MOVE TS-ANTICERN(6) TO QTS-ANTICERN-6.
           MOVE TS-ANTICERN(7) TO QTS-ANTICERN-7.
           MOVE TS-ANTICERN(8) TO QTS-ANTICERN-8.
           MOVE TS-ANTICERN(9) TO QTS-ANTICERN-9.
           MOVE TS-ANTICERN(10) TO QTS-ANTICERN-10.
           MOVE TS-EARNINGS(1) TO QTS-EARNINGS-1.
           MOVE TS-EARNINGS(2) TO QTS-EARNINGS-2.
           MOVE TS-EARNINGS(3) TO QTS-EARNINGS-3.
           MOVE TS-EARNINGS(4) TO QTS-EARNINGS-4.
           MOVE TS-EARNINGS(5) TO QTS-EARNINGS-5.
           MOVE TS-EARNINGS(6) TO QTS-EARNINGS-6.
           MOVE TS-EARNINGS(7) TO QTS-EARNINGS-7.
           MOVE TS-EARNINGS(8) TO QTS-EARNINGS-8.
           MOVE TS-EARNINGS(9) TO QTS-EARNINGS-9.
           MOVE TS-EARNINGS(10) TO QTS-EARNINGS-10.
           MOVE TS-PRLNNO TO QTS-PRLNNO.
           MOVE TS-INSPREM-DLADJ TO QTS-INSPREM-DLADJ.
           MOVE TS-PRDLDISC-UNEARN TO QTS-PRDLDISC-UNEARN.
           MOVE TS-SP-LNCOST-FRMLA TO QTS-SP-LNCOST-FRMLA.
           MOVE TS-DLPAID TO QTS-DLPAID.
           MOVE TS-MIP-FG TO QTS-MIP-FG.
           MOVE TS-SPRCLASS TO QTS-SPRCLASS.
           MOVE TS-O3INS-DLPROC TO QTS-O3INS-DLPROC.
           MOVE TS-USERID TO QTS-USERID.

           IF ( TS-POSTDATE = ZEROES )
              MOVE "1900-01-01" TO QTS-POSTDATE
           ELSE
              MOVE TS-POSTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-POSTDATE.

           MOVE TS-POSTTIME TO QTS-POSTTIME.
           MOVE TS-TAX-AMT TO QTS-TAX-AMT.
           MOVE TS-APPROVED-BY-USER TO QTS-APPROVED-BY-USER.
           MOVE TS-APPROVED-BY-CODE TO QTS-APPROVED-BY-CODE.

           IF ( TS-APPROVED-BY-DATE = ZEROES )
              MOVE "1900-01-01" TO QTS-APPROVED-BY-DATE
           ELSE
              MOVE TS-APPROVED-BY-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTS-APPROVED-BY-DATE.

           MOVE TS-APPROVED-BY-TIME TO QTS-APPROVED-BY-TIME.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-TRS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRS-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRS-ALL-DATA
              MOVE TRS-ALL-PATH             TO TRS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRS-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRS-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRS-ALL-DATA
              MOVE TRS-ALL-PATH             TO TRS-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRSFILE NOT = "S" )
              MOVE TRS-PATH-BEG             TO TRS-SELECT-PATH-BEG
              MOVE TRS-PATH-FILE            TO TRS-SELECT-PATH-FILE
           ELSE
              MOVE TRS-PATH-SQL             TO TRS-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRS1-FILE.
           PERFORM LOAD-TRS1-FILE.
           PERFORM OPEN-IT.
           MOVE TRS-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRS1-FILE.
           PERFORM START-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-SELECT-PATH TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.

           IF ( TRS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRS1-FILE.

           IF ( QTRS1-WEND-LINE = ZEROES    ) OR
              ( QTRS1-WEND-LINE NOT NUMERIC )
              MOVE TRS-PATH-OWNBR        TO QTRS1-WBEG-BRNO
              MOVE TRS-PATH-OWNBR        TO QTRS1-WEND-BRNO
              MOVE TS-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRS1-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRS1-WEND-DATE
              MOVE TS-CLASS             TO QTRS1-WBEG-CLASS
              MOVE TS-ACCTNO            TO QTRS1-WBEG-ACCTNO
              MOVE TS-LAWCODE           TO QTRS1-WBEG-LAWCODE
              MOVE TS-LINE              TO QTRS1-WBEG-LINE
              MOVE ALL "9"              TO QTRS1-WEND-CLASS
                                           QTRS1-WEND-ACCTNO
                                           QTRS1-WEND-LAWCODE
                                           QTRS1-WEND-LINE.

           EXEC SQL
            DECLARE TRS1_CURSOR CURSOR FOR
            SELECT TRSFILE.TS_BRNO,
                   ISNULL(CAST(TRSFILE.TS_DATE AS VARCHAR(10)),'0'),
                   TRSFILE.TS_CLASS,
                   TRSFILE.TS_ACCTNO,
                   TRSFILE.TS_LAWCODE,
                   TRSFILE.TS_LINE,
                   ISNULL(CAST(TRSFILE.TS_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_SSNO_1,
                   ISNULL(TRSFILE.TS_MAKERCD_1,''),
                   TRSFILE.TS_SSNO_2,
                   ISNULL(TRSFILE.TS_MAKERCD_2,''),
                   TRSFILE.TS_SSNO_3,
                   ISNULL(TRSFILE.TS_MAKERCD_3,''),
                   TRSFILE.TS_SSNO_4,
                   ISNULL(TRSFILE.TS_MAKERCD_4,''),
                   ISNULL(TRSFILE.TS_PURCD,''),
                   ISNULL(TRSFILE.TS_SRCD,''),
                   ISNULL(TRSFILE.TS_COLLCD,''),
                   ISNULL(TRSFILE.TS_SERVACCT,''),
                   ISNULL(TRSFILE.TS_LOANTYPE,''),
                   TRSFILE.TS_SUBCLASS,
                   ISNULL(CAST(TRSFILE.TS_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INTDATE AS VARCHAR(10)),'0'),
                   TRSFILE.TS_PROCEEDS,
                   TRSFILE.TS_LNAMT,
                   TRSFILE.TS_FINCHG,
                   TRSFILE.TS_INTCHG,
                   TRSFILE.TS_SERCHG,
                   TRSFILE.TS_EXTCHG,
                   TRSFILE.TS_MAINTFEE,
                   TRSFILE.TS_ORGTERM,
                   TRSFILE.TS_1STPYAMT,
                   TRSFILE.TS_NOREGPY,
                   TRSFILE.TS_REGPYAMT,
                   TRSFILE.TS_LASTPYAMT,
                   ISNULL(TRSFILE.TS_FEECODE_1,''),
                   TRSFILE.TS_FEEAMT_1,
                   ISNULL(TRSFILE.TS_FEEOURS_1,''),
                   ISNULL(TRSFILE.TS_FEECODE_2,''),
                   TRSFILE.TS_FEEAMT_2,
                   ISNULL(TRSFILE.TS_FEEOURS_2,''),
                   ISNULL(TRSFILE.TS_FEECODE_3,''),
                   TRSFILE.TS_FEEAMT_3,
                   ISNULL(TRSFILE.TS_FEEOURS_3,''),
                   ISNULL(TRSFILE.TS_FEECODE_4,''),
                   TRSFILE.TS_FEEAMT_4,
                   ISNULL(TRSFILE.TS_FEEOURS_4,''),
                   ISNULL(TRSFILE.TS_FEECODE_5,''),
                   TRSFILE.TS_FEEAMT_5,
                   ISNULL(TRSFILE.TS_FEEOURS_5,''),
                   ISNULL(TRSFILE.TS_FEECODE_6,''),
                   TRSFILE.TS_FEEAMT_6,
                   ISNULL(TRSFILE.TS_FEEOURS_6,''),
                   ISNULL(TRSFILE.TS_FEECODE_7,''),
                   TRSFILE.TS_FEEAMT_7,
                   ISNULL(TRSFILE.TS_FEEOURS_7,''),
                   ISNULL(TRSFILE.TS_FEECODE_8,''),
                   TRSFILE.TS_FEEAMT_8,
                   ISNULL(TRSFILE.TS_FEEOURS_8,''),
                   ISNULL(TRSFILE.TS_FEECODE_9,''),
                   TRSFILE.TS_FEEAMT_9,
                   ISNULL(TRSFILE.TS_FEEOURS_9,''),
                   ISNULL(TRSFILE.TS_FEECODE_10,''),
                   TRSFILE.TS_FEEAMT_10,
                   ISNULL(TRSFILE.TS_FEEOURS_10,''),
                   ISNULL(TRSFILE.TS_FEECODE_11,''),
                   TRSFILE.TS_FEEAMT_11,
                   ISNULL(TRSFILE.TS_FEEOURS_11,''),
                   ISNULL(TRSFILE.TS_FEECODE_12,''),
                   TRSFILE.TS_FEEAMT_12,
                   ISNULL(TRSFILE.TS_FEEOURS_12,''),
                   ISNULL(TRSFILE.TS_FEECODE_13,''),
                   TRSFILE.TS_FEEAMT_13,
                   ISNULL(TRSFILE.TS_FEEOURS_13,''),
                   ISNULL(TRSFILE.TS_FEECODE_14,''),
                   TRSFILE.TS_FEEAMT_14,
                   ISNULL(TRSFILE.TS_FEEOURS_14,''),
                   ISNULL(TRSFILE.TS_FEECODE_15,''),
                   TRSFILE.TS_FEEAMT_15,
                   ISNULL(TRSFILE.TS_FEEOURS_15,''),
                   ISNULL(TRSFILE.TS_FEECODE_16,''),
                   TRSFILE.TS_FEEAMT_16,
                   ISNULL(TRSFILE.TS_FEEOURS_16,''),
                   ISNULL(TRSFILE.TS_FEECODE_17,''),
                   TRSFILE.TS_FEEAMT_17,
                   ISNULL(TRSFILE.TS_FEEOURS_17,''),
                   ISNULL(TRSFILE.TS_FEECODE_18,''),
                   TRSFILE.TS_FEEAMT_18,
                   ISNULL(TRSFILE.TS_FEEOURS_18,''),
                   ISNULL(TRSFILE.TS_FEECODE_19,''),
                   TRSFILE.TS_FEEAMT_19,
                   ISNULL(TRSFILE.TS_FEEOURS_19,''),
                   ISNULL(TRSFILE.TS_FEECODE_20,''),
                   TRSFILE.TS_FEEAMT_20,
                   ISNULL(TRSFILE.TS_FEEOURS_20,''),
                   ISNULL(TRSFILE.TS_INSURED,''),
                   ISNULL(TRSFILE.TS_INSTYPES,''),
                   ISNULL(TRSFILE.TS_INSELIGIBLE,''),
                   TRSFILE.TS_INSCOMP_1,
                   ISNULL(TRSFILE.TS_INSOURS_1,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_1,
                   TRSFILE.TS_INSCOMM_1,
                   TRSFILE.TS_INSANTICERN_1,
                   TRSFILE.TS_INSEARNINGS_1,
                   TRSFILE.TS_INSCOMP_2,
                   ISNULL(TRSFILE.TS_INSOURS_2,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_2,
                   TRSFILE.TS_INSCOMM_2,
                   TRSFILE.TS_INSANTICERN_2,
                   TRSFILE.TS_INSEARNINGS_2,
                   TRSFILE.TS_INSCOMP_3,
                   ISNULL(TRSFILE.TS_INSOURS_3,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_3,
                   TRSFILE.TS_INSCOMM_3,
                   TRSFILE.TS_INSANTICERN_3,
                   TRSFILE.TS_INSEARNINGS_3,
                   TRSFILE.TS_INSCOMP_4,
                   ISNULL(TRSFILE.TS_INSOURS_4,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_4,
                   TRSFILE.TS_INSCOMM_4,
                   TRSFILE.TS_INSANTICERN_4,
                   TRSFILE.TS_INSEARNINGS_4,
                   TRSFILE.TS_INSCOMP_5,
                   ISNULL(TRSFILE.TS_INSOURS_5,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_5,
                   TRSFILE.TS_INSCOMM_5,
                   TRSFILE.TS_INSANTICERN_5,
                   TRSFILE.TS_INSEARNINGS_5,
                   TRSFILE.TS_INSCOMP_6,
                   ISNULL(TRSFILE.TS_INSOURS_6,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_6,
                   TRSFILE.TS_INSCOMM_6,
                   TRSFILE.TS_INSANTICERN_6,
                   TRSFILE.TS_INSEARNINGS_6,
                   TRSFILE.TS_INSCOMP_7,
                   ISNULL(TRSFILE.TS_INSOURS_7,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_7,
                   TRSFILE.TS_INSCOMM_7,
                   TRSFILE.TS_INSANTICERN_7,
                   TRSFILE.TS_INSEARNINGS_7,
                   TRSFILE.TS_INSCOMP_8,
                   ISNULL(TRSFILE.TS_INSOURS_8,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_8,
                   TRSFILE.TS_INSCOMM_8,
                   TRSFILE.TS_INSANTICERN_8,
                   TRSFILE.TS_INSEARNINGS_8,
                   TRSFILE.TS_INSCOMP_9,
                   ISNULL(TRSFILE.TS_INSOURS_9,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_9,
                   TRSFILE.TS_INSCOMM_9,
                   TRSFILE.TS_INSANTICERN_9,
                   TRSFILE.TS_INSEARNINGS_9,
                   TRSFILE.TS_INSCOMP_10,
                   ISNULL(TRSFILE.TS_INSOURS_10,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_10,
                   TRSFILE.TS_INSCOMM_10,
                   TRSFILE.TS_INSANTICERN_10,
                   TRSFILE.TS_INSEARNINGS_10,
                   TRSFILE.TS_DLCOMM_PP_CMSI,
                   TRSFILE.TS_DLCOMM_O3_CMSI,
                   ISNULL(TRSFILE.TS_INS_TAX_COUNTY_CD,''),
                   ISNULL(TRSFILE.TS_INS_TAX_CITY_CD,''),
                   TRSFILE.TS_TILLNO,
                   ISNULL(TRSFILE.TS_UNWIND_REASON_CODE,''),
                   TRSFILE.TS_DLNO,
                   ISNULL(TRSFILE.TS_DLRECOURSE,''),
                   TRSFILE.TS_DLPARTRCAMT,
                   TRSFILE.TS_PURBAL,
                   TRSFILE.TS_DLRVPRIN_1,
                   TRSFILE.TS_DLRVPRIN_2,
                   TRSFILE.TS_DLRVPRIN_3,
                   TRSFILE.TS_DLRVCHGS_1,
                   TRSFILE.TS_DLRVCHGS_2,
                   TRSFILE.TS_DLRVCHGS_3,
                   ISNULL(TRSFILE.TS_DL_RVCHGS_CODE_1,''),
                   ISNULL(TRSFILE.TS_DL_RVCHGS_CODE_2,''),
                   ISNULL(TRSFILE.TS_DL_RVCHGS_CODE_3,''),
                   ISNULL(TRSFILE.TS_DL_PARVHELD_CODE,''),
                   ISNULL(TRSFILE.TS_DL_PARVPAID_CODE,''),
                   TRSFILE.TS_DLPARVHELD,
                   TRSFILE.TS_DLPARVPAID,
                   TRSFILE.TS_DLDISC,
                   TRSFILE.TS_ANTICTERM,
                   TRSFILE.TS_ANTICERN_1,
                   TRSFILE.TS_ANTICERN_2,
                   TRSFILE.TS_ANTICERN_3,
                   TRSFILE.TS_ANTICERN_4,
                   TRSFILE.TS_ANTICERN_5,
                   TRSFILE.TS_ANTICERN_6,
                   TRSFILE.TS_ANTICERN_7,
                   TRSFILE.TS_ANTICERN_8,
                   TRSFILE.TS_ANTICERN_9,
                   TRSFILE.TS_ANTICERN_10,
                   TRSFILE.TS_EARNINGS_1,
                   TRSFILE.TS_EARNINGS_2,
                   TRSFILE.TS_EARNINGS_3,
                   TRSFILE.TS_EARNINGS_4,
                   TRSFILE.TS_EARNINGS_5,
                   TRSFILE.TS_EARNINGS_6,
                   TRSFILE.TS_EARNINGS_7,
                   TRSFILE.TS_EARNINGS_8,
                   TRSFILE.TS_EARNINGS_9,
                   TRSFILE.TS_EARNINGS_10,
                   TRSFILE.TS_PRLNNO,
                   TRSFILE.TS_INSPREM_DLADJ,
                   TRSFILE.TS_PRDLDISC_UNEARN,
                   ISNULL(TRSFILE.TS_SP_LNCOST_FRMLA,''),
                   ISNULL(TRSFILE.TS_DLPAID,''),
                   ISNULL(TRSFILE.TS_MIP_FG,''),
                   TRSFILE.TS_SPRCLASS,
                   TRSFILE.TS_O3INS_DLPROC,
                   ISNULL(TRSFILE.TS_USERID,''),
                   ISNULL(CAST(TRSFILE.TS_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_POSTTIME,
                   TRSFILE.TS_TAX_AMT,
                   ISNULL(TRSFILE.TS_APPROVED_BY_USER,''),
                   ISNULL(TRSFILE.TS_APPROVED_BY_CODE,''),
                   ISNULL(CAST(TRSFILE.TS_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_APPROVED_BY_TIME
            FROM DBO.TRSFILE
            WHERE TRSFILE.TS_BRNO >= :QTRS1-WBEG-BRNO
              AND TRSFILE.TS_BRNO <= :QTRS1-WEND-BRNO
              AND CAST(TRSFILE.TS_DATE AS VARCHAR(10))
                          >= :QTRS1-WBEG-DATE
              AND CAST(TRSFILE.TS_DATE AS VARCHAR(10))
                          <= :QTRS1-WEND-DATE
              AND TRSFILE.TS_CLASS >= :QTRS1-WBEG-CLASS
              AND TRSFILE.TS_CLASS <= :QTRS1-WEND-CLASS
              AND TRSFILE.TS_ACCTNO >= :QTRS1-WBEG-ACCTNO
              AND TRSFILE.TS_ACCTNO <= :QTRS1-WEND-ACCTNO
              AND TRSFILE.TS_LAWCODE >= :QTRS1-WBEG-LAWCODE
              AND TRSFILE.TS_LAWCODE <= :QTRS1-WEND-LAWCODE
              AND TRSFILE.TS_LINE >= :QTRS1-WBEG-LINE
              AND TRSFILE.TS_LINE <= :QTRS1-WEND-LINE
            ORDER BY TRSFILE.TS_BRNO,
                     TRSFILE.TS_DATE,
                     TRSFILE.TS_CLASS,
                     TRSFILE.TS_ACCTNO,
                     TRSFILE.TS_LAWCODE,
                     TRSFILE.TS_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRS1-WBEG-BRNO
                          QTRS1-WEND-BRNO
                          QTRS1-WBEG-CLASS
                          QTRS1-WEND-CLASS
                          QTRS1-WBEG-ACCTNO
                          QTRS1-WEND-ACCTNO
                          QTRS1-WBEG-LAWCODE
                          QTRS1-WEND-LAWCODE
                          QTRS1-WBEG-LINE
                          QTRS1-WEND-LINE.
           MOVE SPACES TO QTRS1-WBEG-DATE
                          QTRS1-WEND-DATE.


           MOVE STAT---GOOD TO TRS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRS1-FILE.
           PERFORM READ-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO. 
           MOVE TRS-SELECT-PATH TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.

           MOVE TS-BRNO             TO QTS-BRNO.
           MOVE TS-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTS-DATE.
           MOVE TS-CLASS            TO QTS-CLASS.
           MOVE TS-ACCTNO           TO QTS-ACCTNO.
           MOVE TS-LAWCODE          TO QTS-LAWCODE.
           MOVE TS-LINE             TO QTS-LINE.


           EXEC SQL
            SELECT TRSFILE.TS_BRNO,
                   ISNULL(CAST(TRSFILE.TS_DATE AS VARCHAR(10)),'0'),
                   TRSFILE.TS_CLASS,
                   TRSFILE.TS_ACCTNO,
                   TRSFILE.TS_LAWCODE,
                   TRSFILE.TS_LINE,
                   ISNULL(CAST(TRSFILE.TS_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_SSNO_1,
                   ISNULL(TRSFILE.TS_MAKERCD_1,''),
                   TRSFILE.TS_SSNO_2,
                   ISNULL(TRSFILE.TS_MAKERCD_2,''),
                   TRSFILE.TS_SSNO_3,
                   ISNULL(TRSFILE.TS_MAKERCD_3,''),
                   TRSFILE.TS_SSNO_4,
                   ISNULL(TRSFILE.TS_MAKERCD_4,''),
                   ISNULL(TRSFILE.TS_PURCD,''),
                   ISNULL(TRSFILE.TS_SRCD,''),
                   ISNULL(TRSFILE.TS_COLLCD,''),
                   ISNULL(TRSFILE.TS_SERVACCT,''),
                   ISNULL(TRSFILE.TS_LOANTYPE,''),
                   TRSFILE.TS_SUBCLASS,
                   ISNULL(CAST(TRSFILE.TS_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INTDATE AS VARCHAR(10)),'0'),
                   TRSFILE.TS_PROCEEDS,
                   TRSFILE.TS_LNAMT,
                   TRSFILE.TS_FINCHG,
                   TRSFILE.TS_INTCHG,
                   TRSFILE.TS_SERCHG,
                   TRSFILE.TS_EXTCHG,
                   TRSFILE.TS_MAINTFEE,
                   TRSFILE.TS_ORGTERM,
                   TRSFILE.TS_1STPYAMT,
                   TRSFILE.TS_NOREGPY,
                   TRSFILE.TS_REGPYAMT,
                   TRSFILE.TS_LASTPYAMT,
                   ISNULL(TRSFILE.TS_FEECODE_1,''),
                   TRSFILE.TS_FEEAMT_1,
                   ISNULL(TRSFILE.TS_FEEOURS_1,''),
                   ISNULL(TRSFILE.TS_FEECODE_2,''),
                   TRSFILE.TS_FEEAMT_2,
                   ISNULL(TRSFILE.TS_FEEOURS_2,''),
                   ISNULL(TRSFILE.TS_FEECODE_3,''),
                   TRSFILE.TS_FEEAMT_3,
                   ISNULL(TRSFILE.TS_FEEOURS_3,''),
                   ISNULL(TRSFILE.TS_FEECODE_4,''),
                   TRSFILE.TS_FEEAMT_4,
                   ISNULL(TRSFILE.TS_FEEOURS_4,''),
                   ISNULL(TRSFILE.TS_FEECODE_5,''),
                   TRSFILE.TS_FEEAMT_5,
                   ISNULL(TRSFILE.TS_FEEOURS_5,''),
                   ISNULL(TRSFILE.TS_FEECODE_6,''),
                   TRSFILE.TS_FEEAMT_6,
                   ISNULL(TRSFILE.TS_FEEOURS_6,''),
                   ISNULL(TRSFILE.TS_FEECODE_7,''),
                   TRSFILE.TS_FEEAMT_7,
                   ISNULL(TRSFILE.TS_FEEOURS_7,''),
                   ISNULL(TRSFILE.TS_FEECODE_8,''),
                   TRSFILE.TS_FEEAMT_8,
                   ISNULL(TRSFILE.TS_FEEOURS_8,''),
                   ISNULL(TRSFILE.TS_FEECODE_9,''),
                   TRSFILE.TS_FEEAMT_9,
                   ISNULL(TRSFILE.TS_FEEOURS_9,''),
                   ISNULL(TRSFILE.TS_FEECODE_10,''),
                   TRSFILE.TS_FEEAMT_10,
                   ISNULL(TRSFILE.TS_FEEOURS_10,''),
                   ISNULL(TRSFILE.TS_FEECODE_11,''),
                   TRSFILE.TS_FEEAMT_11,
                   ISNULL(TRSFILE.TS_FEEOURS_11,''),
                   ISNULL(TRSFILE.TS_FEECODE_12,''),
                   TRSFILE.TS_FEEAMT_12,
                   ISNULL(TRSFILE.TS_FEEOURS_12,''),
                   ISNULL(TRSFILE.TS_FEECODE_13,''),
                   TRSFILE.TS_FEEAMT_13,
                   ISNULL(TRSFILE.TS_FEEOURS_13,''),
                   ISNULL(TRSFILE.TS_FEECODE_14,''),
                   TRSFILE.TS_FEEAMT_14,
                   ISNULL(TRSFILE.TS_FEEOURS_14,''),
                   ISNULL(TRSFILE.TS_FEECODE_15,''),
                   TRSFILE.TS_FEEAMT_15,
                   ISNULL(TRSFILE.TS_FEEOURS_15,''),
                   ISNULL(TRSFILE.TS_FEECODE_16,''),
                   TRSFILE.TS_FEEAMT_16,
                   ISNULL(TRSFILE.TS_FEEOURS_16,''),
                   ISNULL(TRSFILE.TS_FEECODE_17,''),
                   TRSFILE.TS_FEEAMT_17,
                   ISNULL(TRSFILE.TS_FEEOURS_17,''),
                   ISNULL(TRSFILE.TS_FEECODE_18,''),
                   TRSFILE.TS_FEEAMT_18,
                   ISNULL(TRSFILE.TS_FEEOURS_18,''),
                   ISNULL(TRSFILE.TS_FEECODE_19,''),
                   TRSFILE.TS_FEEAMT_19,
                   ISNULL(TRSFILE.TS_FEEOURS_19,''),
                   ISNULL(TRSFILE.TS_FEECODE_20,''),
                   TRSFILE.TS_FEEAMT_20,
                   ISNULL(TRSFILE.TS_FEEOURS_20,''),
                   ISNULL(TRSFILE.TS_INSURED,''),
                   ISNULL(TRSFILE.TS_INSTYPES,''),
                   ISNULL(TRSFILE.TS_INSELIGIBLE,''),
                   TRSFILE.TS_INSCOMP_1,
                   ISNULL(TRSFILE.TS_INSOURS_1,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_1,
                   TRSFILE.TS_INSCOMM_1,
                   TRSFILE.TS_INSANTICERN_1,
                   TRSFILE.TS_INSEARNINGS_1,
                   TRSFILE.TS_INSCOMP_2,
                   ISNULL(TRSFILE.TS_INSOURS_2,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_2,
                   TRSFILE.TS_INSCOMM_2,
                   TRSFILE.TS_INSANTICERN_2,
                   TRSFILE.TS_INSEARNINGS_2,
                   TRSFILE.TS_INSCOMP_3,
                   ISNULL(TRSFILE.TS_INSOURS_3,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_3,
                   TRSFILE.TS_INSCOMM_3,
                   TRSFILE.TS_INSANTICERN_3,
                   TRSFILE.TS_INSEARNINGS_3,
                   TRSFILE.TS_INSCOMP_4,
                   ISNULL(TRSFILE.TS_INSOURS_4,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_4,
                   TRSFILE.TS_INSCOMM_4,
                   TRSFILE.TS_INSANTICERN_4,
                   TRSFILE.TS_INSEARNINGS_4,
                   TRSFILE.TS_INSCOMP_5,
                   ISNULL(TRSFILE.TS_INSOURS_5,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_5,
                   TRSFILE.TS_INSCOMM_5,
                   TRSFILE.TS_INSANTICERN_5,
                   TRSFILE.TS_INSEARNINGS_5,
                   TRSFILE.TS_INSCOMP_6,
                   ISNULL(TRSFILE.TS_INSOURS_6,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_6,
                   TRSFILE.TS_INSCOMM_6,
                   TRSFILE.TS_INSANTICERN_6,
                   TRSFILE.TS_INSEARNINGS_6,
                   TRSFILE.TS_INSCOMP_7,
                   ISNULL(TRSFILE.TS_INSOURS_7,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_7,
                   TRSFILE.TS_INSCOMM_7,
                   TRSFILE.TS_INSANTICERN_7,
                   TRSFILE.TS_INSEARNINGS_7,
                   TRSFILE.TS_INSCOMP_8,
                   ISNULL(TRSFILE.TS_INSOURS_8,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_8,
                   TRSFILE.TS_INSCOMM_8,
                   TRSFILE.TS_INSANTICERN_8,
                   TRSFILE.TS_INSEARNINGS_8,
                   TRSFILE.TS_INSCOMP_9,
                   ISNULL(TRSFILE.TS_INSOURS_9,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_9,
                   TRSFILE.TS_INSCOMM_9,
                   TRSFILE.TS_INSANTICERN_9,
                   TRSFILE.TS_INSEARNINGS_9,
                   TRSFILE.TS_INSCOMP_10,
                   ISNULL(TRSFILE.TS_INSOURS_10,''),
                   ISNULL(CAST(TRSFILE.TS_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRSFILE.TS_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_INSPREM_10,
                   TRSFILE.TS_INSCOMM_10,
                   TRSFILE.TS_INSANTICERN_10,
                   TRSFILE.TS_INSEARNINGS_10,
                   TRSFILE.TS_DLCOMM_PP_CMSI,
                   TRSFILE.TS_DLCOMM_O3_CMSI,
                   ISNULL(TRSFILE.TS_INS_TAX_COUNTY_CD,''),
                   ISNULL(TRSFILE.TS_INS_TAX_CITY_CD,''),
                   TRSFILE.TS_TILLNO,
                   ISNULL(TRSFILE.TS_UNWIND_REASON_CODE,''),
                   TRSFILE.TS_DLNO,
                   ISNULL(TRSFILE.TS_DLRECOURSE,''),
                   TRSFILE.TS_DLPARTRCAMT,
                   TRSFILE.TS_PURBAL,
                   TRSFILE.TS_DLRVPRIN_1,
                   TRSFILE.TS_DLRVPRIN_2,
                   TRSFILE.TS_DLRVPRIN_3,
                   TRSFILE.TS_DLRVCHGS_1,
                   TRSFILE.TS_DLRVCHGS_2,
                   TRSFILE.TS_DLRVCHGS_3,
                   ISNULL(TRSFILE.TS_DL_RVCHGS_CODE_1,''),
                   ISNULL(TRSFILE.TS_DL_RVCHGS_CODE_2,''),
                   ISNULL(TRSFILE.TS_DL_RVCHGS_CODE_3,''),
                   ISNULL(TRSFILE.TS_DL_PARVHELD_CODE,''),
                   ISNULL(TRSFILE.TS_DL_PARVPAID_CODE,''),
                   TRSFILE.TS_DLPARVHELD,
                   TRSFILE.TS_DLPARVPAID,
                   TRSFILE.TS_DLDISC,
                   TRSFILE.TS_ANTICTERM,
                   TRSFILE.TS_ANTICERN_1,
                   TRSFILE.TS_ANTICERN_2,
                   TRSFILE.TS_ANTICERN_3,
                   TRSFILE.TS_ANTICERN_4,
                   TRSFILE.TS_ANTICERN_5,
                   TRSFILE.TS_ANTICERN_6,
                   TRSFILE.TS_ANTICERN_7,
                   TRSFILE.TS_ANTICERN_8,
                   TRSFILE.TS_ANTICERN_9,
                   TRSFILE.TS_ANTICERN_10,
                   TRSFILE.TS_EARNINGS_1,
                   TRSFILE.TS_EARNINGS_2,
                   TRSFILE.TS_EARNINGS_3,
                   TRSFILE.TS_EARNINGS_4,
                   TRSFILE.TS_EARNINGS_5,
                   TRSFILE.TS_EARNINGS_6,
                   TRSFILE.TS_EARNINGS_7,
                   TRSFILE.TS_EARNINGS_8,
                   TRSFILE.TS_EARNINGS_9,
                   TRSFILE.TS_EARNINGS_10,
                   TRSFILE.TS_PRLNNO,
                   TRSFILE.TS_INSPREM_DLADJ,
                   TRSFILE.TS_PRDLDISC_UNEARN,
                   ISNULL(TRSFILE.TS_SP_LNCOST_FRMLA,''),
                   ISNULL(TRSFILE.TS_DLPAID,''),
                   ISNULL(TRSFILE.TS_MIP_FG,''),
                   TRSFILE.TS_SPRCLASS,
                   TRSFILE.TS_O3INS_DLPROC,
                   ISNULL(TRSFILE.TS_USERID,''),
                   ISNULL(CAST(TRSFILE.TS_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_POSTTIME,
                   TRSFILE.TS_TAX_AMT,
                   ISNULL(TRSFILE.TS_APPROVED_BY_USER,''),
                   ISNULL(TRSFILE.TS_APPROVED_BY_CODE,''),
                   ISNULL(CAST(TRSFILE.TS_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRSFILE.TS_APPROVED_BY_TIME
            INTO :QTRS-REC
            FROM DBO.TRSFILE
            WHERE TRSFILE.TS_BRNO = :QTS-BRNO
              AND TRSFILE.TS_DATE = :QTS-DATE
              AND TRSFILE.TS_CLASS = :QTS-CLASS
              AND TRSFILE.TS_ACCTNO = :QTS-ACCTNO
              AND TRSFILE.TS_LAWCODE = :QTS-LAWCODE
              AND TRSFILE.TS_LINE = :QTS-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRS1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRS-FIELDS.

      *-----------------------------------------------------------------
       READ-TRS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-SELECT-PATH TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.
           INITIALIZE QTRS-REC.

           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRS1_CURSOR INTO :QTRS-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRS-FIELDS.

      *-----------------------------------------------------------------
       WRITE-TRS1-FILE.

           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           ACCEPT TS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TRS-SELECT-PATH TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.
           PERFORM SET-TRS-FIELDS.

           EXEC SQL
            INSERT INTO DBO.TRSFILE
             (TRSFILE.TS_BRNO,
              TRSFILE.TS_DATE,
              TRSFILE.TS_CLASS,
              TRSFILE.TS_ACCTNO,
              TRSFILE.TS_LAWCODE,
              TRSFILE.TS_LINE,
              TRSFILE.TS_LTOUCH_DATE,
              TRSFILE.TS_SSNO_1,
              TRSFILE.TS_MAKERCD_1,
              TRSFILE.TS_SSNO_2,
              TRSFILE.TS_MAKERCD_2,
              TRSFILE.TS_SSNO_3,
              TRSFILE.TS_MAKERCD_3,
              TRSFILE.TS_SSNO_4,
              TRSFILE.TS_MAKERCD_4,
              TRSFILE.TS_PURCD,
              TRSFILE.TS_SRCD,
              TRSFILE.TS_COLLCD,
              TRSFILE.TS_SERVACCT,
              TRSFILE.TS_LOANTYPE,
              TRSFILE.TS_SUBCLASS,
              TRSFILE.TS_LOANDATE,
              TRSFILE.TS_INTDATE,
              TRSFILE.TS_PROCEEDS,
              TRSFILE.TS_LNAMT,
              TRSFILE.TS_FINCHG,
              TRSFILE.TS_INTCHG,
              TRSFILE.TS_SERCHG,
              TRSFILE.TS_EXTCHG,
              TRSFILE.TS_MAINTFEE,
              TRSFILE.TS_ORGTERM,
              TRSFILE.TS_1STPYAMT,
              TRSFILE.TS_NOREGPY,
              TRSFILE.TS_REGPYAMT,
              TRSFILE.TS_LASTPYAMT,
              TRSFILE.TS_FEECODE_1,
              TRSFILE.TS_FEEAMT_1,
              TRSFILE.TS_FEEOURS_1,
              TRSFILE.TS_FEECODE_2,
              TRSFILE.TS_FEEAMT_2,
              TRSFILE.TS_FEEOURS_2,
              TRSFILE.TS_FEECODE_3,
              TRSFILE.TS_FEEAMT_3,
              TRSFILE.TS_FEEOURS_3,
              TRSFILE.TS_FEECODE_4,
              TRSFILE.TS_FEEAMT_4,
              TRSFILE.TS_FEEOURS_4,
              TRSFILE.TS_FEECODE_5,
              TRSFILE.TS_FEEAMT_5,
              TRSFILE.TS_FEEOURS_5,
              TRSFILE.TS_FEECODE_6,
              TRSFILE.TS_FEEAMT_6,
              TRSFILE.TS_FEEOURS_6,
              TRSFILE.TS_FEECODE_7,
              TRSFILE.TS_FEEAMT_7,
              TRSFILE.TS_FEEOURS_7,
              TRSFILE.TS_FEECODE_8,
              TRSFILE.TS_FEEAMT_8,
              TRSFILE.TS_FEEOURS_8,
              TRSFILE.TS_FEECODE_9,
              TRSFILE.TS_FEEAMT_9,
              TRSFILE.TS_FEEOURS_9,
              TRSFILE.TS_FEECODE_10,
              TRSFILE.TS_FEEAMT_10,
              TRSFILE.TS_FEEOURS_10,
              TRSFILE.TS_FEECODE_11,
              TRSFILE.TS_FEEAMT_11,
              TRSFILE.TS_FEEOURS_11,
              TRSFILE.TS_FEECODE_12,
              TRSFILE.TS_FEEAMT_12,
              TRSFILE.TS_FEEOURS_12,
              TRSFILE.TS_FEECODE_13,
              TRSFILE.TS_FEEAMT_13,
              TRSFILE.TS_FEEOURS_13,
              TRSFILE.TS_FEECODE_14,
              TRSFILE.TS_FEEAMT_14,
              TRSFILE.TS_FEEOURS_14,
              TRSFILE.TS_FEECODE_15,
              TRSFILE.TS_FEEAMT_15,
              TRSFILE.TS_FEEOURS_15,
              TRSFILE.TS_FEECODE_16,
              TRSFILE.TS_FEEAMT_16,
              TRSFILE.TS_FEEOURS_16,
              TRSFILE.TS_FEECODE_17,
              TRSFILE.TS_FEEAMT_17,
              TRSFILE.TS_FEEOURS_17,
              TRSFILE.TS_FEECODE_18,
              TRSFILE.TS_FEEAMT_18,
              TRSFILE.TS_FEEOURS_18,
              TRSFILE.TS_FEECODE_19,
              TRSFILE.TS_FEEAMT_19,
              TRSFILE.TS_FEEOURS_19,
              TRSFILE.TS_FEECODE_20,
              TRSFILE.TS_FEEAMT_20,
              TRSFILE.TS_FEEOURS_20,
              TRSFILE.TS_INSURED,
              TRSFILE.TS_INSTYPES,
              TRSFILE.TS_INSELIGIBLE,
              TRSFILE.TS_INSCOMP_1,
              TRSFILE.TS_INSOURS_1,
              TRSFILE.TS_INSEFF_DATE_1,
              TRSFILE.TS_INSEXP_DATE_1,
              TRSFILE.TS_INSPREM_1,
              TRSFILE.TS_INSCOMM_1,
              TRSFILE.TS_INSANTICERN_1,
              TRSFILE.TS_INSEARNINGS_1,
              TRSFILE.TS_INSCOMP_2,
              TRSFILE.TS_INSOURS_2,
              TRSFILE.TS_INSEFF_DATE_2,
              TRSFILE.TS_INSEXP_DATE_2,
              TRSFILE.TS_INSPREM_2,
              TRSFILE.TS_INSCOMM_2,
              TRSFILE.TS_INSANTICERN_2,
              TRSFILE.TS_INSEARNINGS_2,
              TRSFILE.TS_INSCOMP_3,
              TRSFILE.TS_INSOURS_3,
              TRSFILE.TS_INSEFF_DATE_3,
              TRSFILE.TS_INSEXP_DATE_3,
              TRSFILE.TS_INSPREM_3,
              TRSFILE.TS_INSCOMM_3,
              TRSFILE.TS_INSANTICERN_3,
              TRSFILE.TS_INSEARNINGS_3,
              TRSFILE.TS_INSCOMP_4,
              TRSFILE.TS_INSOURS_4,
              TRSFILE.TS_INSEFF_DATE_4,
              TRSFILE.TS_INSEXP_DATE_4,
              TRSFILE.TS_INSPREM_4,
              TRSFILE.TS_INSCOMM_4,
              TRSFILE.TS_INSANTICERN_4,
              TRSFILE.TS_INSEARNINGS_4,
              TRSFILE.TS_INSCOMP_5,
              TRSFILE.TS_INSOURS_5,
              TRSFILE.TS_INSEFF_DATE_5,
              TRSFILE.TS_INSEXP_DATE_5,
              TRSFILE.TS_INSPREM_5,
              TRSFILE.TS_INSCOMM_5,
              TRSFILE.TS_INSANTICERN_5,
              TRSFILE.TS_INSEARNINGS_5,
              TRSFILE.TS_INSCOMP_6,
              TRSFILE.TS_INSOURS_6,
              TRSFILE.TS_INSEFF_DATE_6,
              TRSFILE.TS_INSEXP_DATE_6,
              TRSFILE.TS_INSPREM_6,
              TRSFILE.TS_INSCOMM_6,
              TRSFILE.TS_INSANTICERN_6,
              TRSFILE.TS_INSEARNINGS_6,
              TRSFILE.TS_INSCOMP_7,
              TRSFILE.TS_INSOURS_7,
              TRSFILE.TS_INSEFF_DATE_7,
              TRSFILE.TS_INSEXP_DATE_7,
              TRSFILE.TS_INSPREM_7,
              TRSFILE.TS_INSCOMM_7,
              TRSFILE.TS_INSANTICERN_7,
              TRSFILE.TS_INSEARNINGS_7,
              TRSFILE.TS_INSCOMP_8,
              TRSFILE.TS_INSOURS_8,
              TRSFILE.TS_INSEFF_DATE_8,
              TRSFILE.TS_INSEXP_DATE_8,
              TRSFILE.TS_INSPREM_8,
              TRSFILE.TS_INSCOMM_8,
              TRSFILE.TS_INSANTICERN_8,
              TRSFILE.TS_INSEARNINGS_8,
              TRSFILE.TS_INSCOMP_9,
              TRSFILE.TS_INSOURS_9,
              TRSFILE.TS_INSEFF_DATE_9,
              TRSFILE.TS_INSEXP_DATE_9,
              TRSFILE.TS_INSPREM_9,
              TRSFILE.TS_INSCOMM_9,
              TRSFILE.TS_INSANTICERN_9,
              TRSFILE.TS_INSEARNINGS_9,
              TRSFILE.TS_INSCOMP_10,
              TRSFILE.TS_INSOURS_10,
              TRSFILE.TS_INSEFF_DATE_10,
              TRSFILE.TS_INSEXP_DATE_10,
              TRSFILE.TS_INSPREM_10,
              TRSFILE.TS_INSCOMM_10,
              TRSFILE.TS_INSANTICERN_10,
              TRSFILE.TS_INSEARNINGS_10,
              TRSFILE.TS_DLCOMM_PP_CMSI,
              TRSFILE.TS_DLCOMM_O3_CMSI,
              TRSFILE.TS_INS_TAX_COUNTY_CD,
              TRSFILE.TS_INS_TAX_CITY_CD,
              TRSFILE.TS_TILLNO,
              TRSFILE.TS_UNWIND_REASON_CODE,
              TRSFILE.TS_DLNO,
              TRSFILE.TS_DLRECOURSE,
              TRSFILE.TS_DLPARTRCAMT,
              TRSFILE.TS_PURBAL,
              TRSFILE.TS_DLRVPRIN_1,
              TRSFILE.TS_DLRVPRIN_2,
              TRSFILE.TS_DLRVPRIN_3,
              TRSFILE.TS_DLRVCHGS_1,
              TRSFILE.TS_DLRVCHGS_2,
              TRSFILE.TS_DLRVCHGS_3,
              TRSFILE.TS_DL_RVCHGS_CODE_1,
              TRSFILE.TS_DL_RVCHGS_CODE_2,
              TRSFILE.TS_DL_RVCHGS_CODE_3,
              TRSFILE.TS_DL_PARVHELD_CODE,
              TRSFILE.TS_DL_PARVPAID_CODE,
              TRSFILE.TS_DLPARVHELD,
              TRSFILE.TS_DLPARVPAID,
              TRSFILE.TS_DLDISC,
              TRSFILE.TS_ANTICTERM,
              TRSFILE.TS_ANTICERN_1,
              TRSFILE.TS_ANTICERN_2,
              TRSFILE.TS_ANTICERN_3,
              TRSFILE.TS_ANTICERN_4,
              TRSFILE.TS_ANTICERN_5,
              TRSFILE.TS_ANTICERN_6,
              TRSFILE.TS_ANTICERN_7,
              TRSFILE.TS_ANTICERN_8,
              TRSFILE.TS_ANTICERN_9,
              TRSFILE.TS_ANTICERN_10,
              TRSFILE.TS_EARNINGS_1,
              TRSFILE.TS_EARNINGS_2,
              TRSFILE.TS_EARNINGS_3,
              TRSFILE.TS_EARNINGS_4,
              TRSFILE.TS_EARNINGS_5,
              TRSFILE.TS_EARNINGS_6,
              TRSFILE.TS_EARNINGS_7,
              TRSFILE.TS_EARNINGS_8,
              TRSFILE.TS_EARNINGS_9,
              TRSFILE.TS_EARNINGS_10,
              TRSFILE.TS_PRLNNO,
              TRSFILE.TS_INSPREM_DLADJ,
              TRSFILE.TS_PRDLDISC_UNEARN,
              TRSFILE.TS_SP_LNCOST_FRMLA,
              TRSFILE.TS_DLPAID,
              TRSFILE.TS_MIP_FG,
              TRSFILE.TS_SPRCLASS,
              TRSFILE.TS_O3INS_DLPROC,
              TRSFILE.TS_USERID,
              TRSFILE.TS_POSTDATE,
              TRSFILE.TS_POSTTIME,
              TRSFILE.TS_TAX_AMT,
              TRSFILE.TS_APPROVED_BY_USER,
              TRSFILE.TS_APPROVED_BY_CODE,
              TRSFILE.TS_APPROVED_BY_DATE,
              TRSFILE.TS_APPROVED_BY_TIME)
            VALUES
             (:QTS-BRNO,
              NULLIF(:QTS-DATE,'1900-01-01'),
              :QTS-CLASS,
              :QTS-ACCTNO,
              :QTS-LAWCODE,
              :QTS-LINE,
              NULLIF(:QTS-LTOUCH-DATE,'1900-01-01'),
              :QTS-SSNO-1,
              :QTS-MAKERCD-1,
              :QTS-SSNO-2,
              :QTS-MAKERCD-2,
              :QTS-SSNO-3,
              :QTS-MAKERCD-3,
              :QTS-SSNO-4,
              :QTS-MAKERCD-4,
              :QTS-PURCD,
              :QTS-SRCD,
              :QTS-COLLCD,
              :QTS-SERVACCT,
              :QTS-LOANTYPE,
              :QTS-SUBCLASS,
              NULLIF(:QTS-LOANDATE,'1900-01-01'),
              NULLIF(:QTS-INTDATE,'1900-01-01'),
              :QTS-PROCEEDS,
              :QTS-LNAMT,
              :QTS-FINCHG,
              :QTS-INTCHG,
              :QTS-SERCHG,
              :QTS-EXTCHG,
              :QTS-MAINTFEE,
              :QTS-ORGTERM,
              :QTS-1STPYAMT,
              :QTS-NOREGPY,
              :QTS-REGPYAMT,
              :QTS-LASTPYAMT,
              :QTS-FEECODE-1,
              :QTS-FEEAMT-1,
              :QTS-FEEOURS-1,
              :QTS-FEECODE-2,
              :QTS-FEEAMT-2,
              :QTS-FEEOURS-2,
              :QTS-FEECODE-3,
              :QTS-FEEAMT-3,
              :QTS-FEEOURS-3,
              :QTS-FEECODE-4,
              :QTS-FEEAMT-4,
              :QTS-FEEOURS-4,
              :QTS-FEECODE-5,
              :QTS-FEEAMT-5,
              :QTS-FEEOURS-5,
              :QTS-FEECODE-6,
              :QTS-FEEAMT-6,
              :QTS-FEEOURS-6,
              :QTS-FEECODE-7,
              :QTS-FEEAMT-7,
              :QTS-FEEOURS-7,
              :QTS-FEECODE-8,
              :QTS-FEEAMT-8,
              :QTS-FEEOURS-8,
              :QTS-FEECODE-9,
              :QTS-FEEAMT-9,
              :QTS-FEEOURS-9,
              :QTS-FEECODE-10,
              :QTS-FEEAMT-10,
              :QTS-FEEOURS-10,
              :QTS-FEECODE-11,
              :QTS-FEEAMT-11,
              :QTS-FEEOURS-11,
              :QTS-FEECODE-12,
              :QTS-FEEAMT-12,
              :QTS-FEEOURS-12,
              :QTS-FEECODE-13,
              :QTS-FEEAMT-13,
              :QTS-FEEOURS-13,
              :QTS-FEECODE-14,
              :QTS-FEEAMT-14,
              :QTS-FEEOURS-14,
              :QTS-FEECODE-15,
              :QTS-FEEAMT-15,
              :QTS-FEEOURS-15,
              :QTS-FEECODE-16,
              :QTS-FEEAMT-16,
              :QTS-FEEOURS-16,
              :QTS-FEECODE-17,
              :QTS-FEEAMT-17,
              :QTS-FEEOURS-17,
              :QTS-FEECODE-18,
              :QTS-FEEAMT-18,
              :QTS-FEEOURS-18,
              :QTS-FEECODE-19,
              :QTS-FEEAMT-19,
              :QTS-FEEOURS-19,
              :QTS-FEECODE-20,
              :QTS-FEEAMT-20,
              :QTS-FEEOURS-20,
              :QTS-INSURED,
              :QTS-INSTYPES,
              :QTS-INSELIGIBLE,
              :QTS-INSCOMP-1,
              :QTS-INSOURS-1,
              NULLIF(:QTS-INSEFF-DATE-1,'1900-01-01'),
              NULLIF(:QTS-INSEXP-DATE-1,'1900-01-01'),
              :QTS-INSPREM-1,
              :QTS-INSCOMM-1,
              :QTS-INSANTICERN-1,
              :QTS-INSEARNINGS-1,
              :QTS-INSCOMP-2,
              :QTS-INSOURS-2,
              NULLIF(:QTS-INSEFF-DATE-2,'1900-01-01'),
              NULLIF(:QTS-INSEXP-DATE-2,'1900-01-01'),
              :QTS-INSPREM-2,
              :QTS-INSCOMM-2,
              :QTS-INSANTICERN-2,
              :QTS-INSEARNINGS-2,
              :QTS-INSCOMP-3,
              :QTS-INSOURS-3,
              NULLIF(:QTS-INSEFF-DATE-3,'1900-01-01'),
              NULLIF(:QTS-INSEXP-DATE-3,'1900-01-01'),
              :QTS-INSPREM-3,
              :QTS-INSCOMM-3,
              :QTS-INSANTICERN-3,
              :QTS-INSEARNINGS-3,
              :QTS-INSCOMP-4,
              :QTS-INSOURS-4,
              NULLIF(:QTS-INSEFF-DATE-4,'1900-01-01'),
              NULLIF(:QTS-INSEXP-DATE-4,'1900-01-01'),
              :QTS-INSPREM-4,
              :QTS-INSCOMM-4,
              :QTS-INSANTICERN-4,
              :QTS-INSEARNINGS-4,
              :QTS-INSCOMP-5,
              :QTS-INSOURS-5,
              NULLIF(:QTS-INSEFF-DATE-5,'1900-01-01'),
              NULLIF(:QTS-INSEXP-DATE-5,'1900-01-01'),
              :QTS-INSPREM-5,
              :QTS-INSCOMM-5,
              :QTS-INSANTICERN-5,
              :QTS-INSEARNINGS-5,
              :QTS-INSCOMP-6,
              :QTS-INSOURS-6,
              NULLIF(:QTS-INSEFF-DATE-6,'1900-01-01'),
              NULLIF(:QTS-INSEXP-DATE-6,'1900-01-01'),
              :QTS-INSPREM-6,
              :QTS-INSCOMM-6,
              :QTS-INSANTICERN-6,
              :QTS-INSEARNINGS-6,
              :QTS-INSCOMP-7,
              :QTS-INSOURS-7,
              NULLIF(:QTS-INSEFF-DATE-7,'1900-01-01'),
              NULLIF(:QTS-INSEXP-DATE-7,'1900-01-01'),
              :QTS-INSPREM-7,
              :QTS-INSCOMM-7,
              :QTS-INSANTICERN-7,
              :QTS-INSEARNINGS-7,
              :QTS-INSCOMP-8,
              :QTS-INSOURS-8,
              NULLIF(:QTS-INSEFF-DATE-8,'1900-01-01'),
              NULLIF(:QTS-INSEXP-DATE-8,'1900-01-01'),
              :QTS-INSPREM-8,
              :QTS-INSCOMM-8,
              :QTS-INSANTICERN-8,
              :QTS-INSEARNINGS-8,
              :QTS-INSCOMP-9,
              :QTS-INSOURS-9,
              NULLIF(:QTS-INSEFF-DATE-9,'1900-01-01'),
              NULLIF(:QTS-INSEXP-DATE-9,'1900-01-01'),
              :QTS-INSPREM-9,
              :QTS-INSCOMM-9,
              :QTS-INSANTICERN-9,
              :QTS-INSEARNINGS-9,
              :QTS-INSCOMP-10,
              :QTS-INSOURS-10,
              NULLIF(:QTS-INSEFF-DATE-10,'1900-01-01'),
              NULLIF(:QTS-INSEXP-DATE-10,'1900-01-01'),
              :QTS-INSPREM-10,
              :QTS-INSCOMM-10,
              :QTS-INSANTICERN-10,
              :QTS-INSEARNINGS-10,
              :QTS-DLCOMM-PP-CMSI,
              :QTS-DLCOMM-O3-CMSI,
              :QTS-INS-TAX-COUNTY-CD,
              :QTS-INS-TAX-CITY-CD,
              :QTS-TILLNO,
              :QTS-UNWIND-REASON-CODE,
              :QTS-DLNO,
              :QTS-DLRECOURSE,
              :QTS-DLPARTRCAMT,
              :QTS-PURBAL,
              :QTS-DLRVPRIN-1,
              :QTS-DLRVPRIN-2,
              :QTS-DLRVPRIN-3,
              :QTS-DLRVCHGS-1,
              :QTS-DLRVCHGS-2,
              :QTS-DLRVCHGS-3,
              :QTS-DL-RVCHGS-CODE-1,
              :QTS-DL-RVCHGS-CODE-2,
              :QTS-DL-RVCHGS-CODE-3,
              :QTS-DL-PARVHELD-CODE,
              :QTS-DL-PARVPAID-CODE,
              :QTS-DLPARVHELD,
              :QTS-DLPARVPAID,
              :QTS-DLDISC,
              :QTS-ANTICTERM,
              :QTS-ANTICERN-1,
              :QTS-ANTICERN-2,
              :QTS-ANTICERN-3,
              :QTS-ANTICERN-4,
              :QTS-ANTICERN-5,
              :QTS-ANTICERN-6,
              :QTS-ANTICERN-7,
              :QTS-ANTICERN-8,
              :QTS-ANTICERN-9,
              :QTS-ANTICERN-10,
              :QTS-EARNINGS-1,
              :QTS-EARNINGS-2,
              :QTS-EARNINGS-3,
              :QTS-EARNINGS-4,
              :QTS-EARNINGS-5,
              :QTS-EARNINGS-6,
              :QTS-EARNINGS-7,
              :QTS-EARNINGS-8,
              :QTS-EARNINGS-9,
              :QTS-EARNINGS-10,
              :QTS-PRLNNO,
              :QTS-INSPREM-DLADJ,
              :QTS-PRDLDISC-UNEARN,
              :QTS-SP-LNCOST-FRMLA,
              :QTS-DLPAID,
              :QTS-MIP-FG,
              :QTS-SPRCLASS,
              :QTS-O3INS-DLPROC,
              :QTS-USERID,
              NULLIF(:QTS-POSTDATE,'1900-01-01'),
              :QTS-POSTTIME,
              :QTS-TAX-AMT,
              :QTS-APPROVED-BY-USER,
              :QTS-APPROVED-BY-CODE,
              NULLIF(:QTS-APPROVED-BY-DATE,'1900-01-01'),
              :QTS-APPROVED-BY-TIME)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TRS1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-TRS1-FILE.

           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           ACCEPT TS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TRS-SELECT-PATH TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.
           PERFORM SET-TRS-FIELDS.

           EXEC SQL
            UPDATE DBO.TRWFILE
             SET
              TRSFILE.TS_BRNO = :QTS-BRNO,
              TRSFILE.TS_DATE =
                 NULLIF(:QTS-DATE,'1900-01-01'),
              TRSFILE.TS_CLASS = :QTS-CLASS,
              TRSFILE.TS_ACCTNO = :QTS-ACCTNO,
              TRSFILE.TS_LAWCODE = :QTS-LAWCODE,
              TRSFILE.TS_LINE = :QTS-LINE,
              TRSFILE.TS_LTOUCH_DATE =
                 NULLIF(:QTS-LTOUCH-DATE,'1900-01-01'),
              TRSFILE.TS_SSNO_1 = :QTS-SSNO-1,
              TRSFILE.TS_MAKERCD_1 = :QTS-MAKERCD-1,
              TRSFILE.TS_SSNO_2 = :QTS-SSNO-2,
              TRSFILE.TS_MAKERCD_2 = :QTS-MAKERCD-2,
              TRSFILE.TS_SSNO_3 = :QTS-SSNO-3,
              TRSFILE.TS_MAKERCD_3 = :QTS-MAKERCD-3,
              TRSFILE.TS_SSNO_4 = :QTS-SSNO-4,
              TRSFILE.TS_MAKERCD_4 = :QTS-MAKERCD-4,
              TRSFILE.TS_PURCD = :QTS-PURCD,
              TRSFILE.TS_SRCD = :QTS-SRCD,
              TRSFILE.TS_COLLCD = :QTS-COLLCD,
              TRSFILE.TS_SERVACCT = :QTS-SERVACCT,
              TRSFILE.TS_LOANTYPE = :QTS-LOANTYPE,
              TRSFILE.TS_SUBCLASS = :QTS-SUBCLASS,
              TRSFILE.TS_LOANDATE =
                 NULLIF(:QTS-LOANDATE,'1900-01-01'),
              TRSFILE.TS_INTDATE =
                 NULLIF(:QTS-INTDATE,'1900-01-01'),
              TRSFILE.TS_PROCEEDS = :QTS-PROCEEDS,
              TRSFILE.TS_LNAMT = :QTS-LNAMT,
              TRSFILE.TS_FINCHG = :QTS-FINCHG,
              TRSFILE.TS_INTCHG = :QTS-INTCHG,
              TRSFILE.TS_SERCHG = :QTS-SERCHG,
              TRSFILE.TS_EXTCHG = :QTS-EXTCHG,
              TRSFILE.TS_MAINTFEE = :QTS-MAINTFEE,
              TRSFILE.TS_ORGTERM = :QTS-ORGTERM,
              TRSFILE.TS_1STPYAMT = :QTS-1STPYAMT,
              TRSFILE.TS_NOREGPY = :QTS-NOREGPY,
              TRSFILE.TS_REGPYAMT = :QTS-REGPYAMT,
              TRSFILE.TS_LASTPYAMT = :QTS-LASTPYAMT,
              TRSFILE.TS_FEECODE_1 = :QTS-FEECODE-1,
              TRSFILE.TS_FEEAMT_1 = :QTS-FEEAMT-1,
              TRSFILE.TS_FEEOURS_1 = :QTS-FEEOURS-1,
              TRSFILE.TS_FEECODE_2 = :QTS-FEECODE-2,
              TRSFILE.TS_FEEAMT_2 = :QTS-FEEAMT-2,
              TRSFILE.TS_FEEOURS_2 = :QTS-FEEOURS-2,
              TRSFILE.TS_FEECODE_3 = :QTS-FEECODE-3,
              TRSFILE.TS_FEEAMT_3 = :QTS-FEEAMT-3,
              TRSFILE.TS_FEEOURS_3 = :QTS-FEEOURS-3,
              TRSFILE.TS_FEECODE_4 = :QTS-FEECODE-4,
              TRSFILE.TS_FEEAMT_4 = :QTS-FEEAMT-4,
              TRSFILE.TS_FEEOURS_4 = :QTS-FEEOURS-4,
              TRSFILE.TS_FEECODE_5 = :QTS-FEECODE-5,
              TRSFILE.TS_FEEAMT_5 = :QTS-FEEAMT-5,
              TRSFILE.TS_FEEOURS_5 = :QTS-FEEOURS-5,
              TRSFILE.TS_FEECODE_6 = :QTS-FEECODE-6,
              TRSFILE.TS_FEEAMT_6 = :QTS-FEEAMT-6,
              TRSFILE.TS_FEEOURS_6 = :QTS-FEEOURS-6,
              TRSFILE.TS_FEECODE_7 = :QTS-FEECODE-7,
              TRSFILE.TS_FEEAMT_7 = :QTS-FEEAMT-7,
              TRSFILE.TS_FEEOURS_7 = :QTS-FEEOURS-7,
              TRSFILE.TS_FEECODE_8 = :QTS-FEECODE-8,
              TRSFILE.TS_FEEAMT_8 = :QTS-FEEAMT-8,
              TRSFILE.TS_FEEOURS_8 = :QTS-FEEOURS-8,
              TRSFILE.TS_FEECODE_9 = :QTS-FEECODE-9,
              TRSFILE.TS_FEEAMT_9 = :QTS-FEEAMT-9,
              TRSFILE.TS_FEEOURS_9 = :QTS-FEEOURS-9,
              TRSFILE.TS_FEECODE_10 = :QTS-FEECODE-10,
              TRSFILE.TS_FEEAMT_10 = :QTS-FEEAMT-10,
              TRSFILE.TS_FEEOURS_10 = :QTS-FEEOURS-10,
              TRSFILE.TS_FEECODE_11 = :QTS-FEECODE-11,
              TRSFILE.TS_FEEAMT_11 = :QTS-FEEAMT-11,
              TRSFILE.TS_FEEOURS_11 = :QTS-FEEOURS-11,
              TRSFILE.TS_FEECODE_12 = :QTS-FEECODE-12,
              TRSFILE.TS_FEEAMT_12 = :QTS-FEEAMT-12,
              TRSFILE.TS_FEEOURS_12 = :QTS-FEEOURS-12,
              TRSFILE.TS_FEECODE_13 = :QTS-FEECODE-13,
              TRSFILE.TS_FEEAMT_13 = :QTS-FEEAMT-13,
              TRSFILE.TS_FEEOURS_13 = :QTS-FEEOURS-13,
              TRSFILE.TS_FEECODE_14 = :QTS-FEECODE-14,
              TRSFILE.TS_FEEAMT_14 = :QTS-FEEAMT-14,
              TRSFILE.TS_FEEOURS_14 = :QTS-FEEOURS-14,
              TRSFILE.TS_FEECODE_15 = :QTS-FEECODE-15,
              TRSFILE.TS_FEEAMT_15 = :QTS-FEEAMT-15,
              TRSFILE.TS_FEEOURS_15 = :QTS-FEEOURS-15,
              TRSFILE.TS_FEECODE_16 = :QTS-FEECODE-16,
              TRSFILE.TS_FEEAMT_16 = :QTS-FEEAMT-16,
              TRSFILE.TS_FEEOURS_16 = :QTS-FEEOURS-16,
              TRSFILE.TS_FEECODE_17 = :QTS-FEECODE-17,
              TRSFILE.TS_FEEAMT_17 = :QTS-FEEAMT-17,
              TRSFILE.TS_FEEOURS_17 = :QTS-FEEOURS-17,
              TRSFILE.TS_FEECODE_18 = :QTS-FEECODE-18,
              TRSFILE.TS_FEEAMT_18 = :QTS-FEEAMT-18,
              TRSFILE.TS_FEEOURS_18 = :QTS-FEEOURS-18,
              TRSFILE.TS_FEECODE_19 = :QTS-FEECODE-19,
              TRSFILE.TS_FEEAMT_19 = :QTS-FEEAMT-19,
              TRSFILE.TS_FEEOURS_19 = :QTS-FEEOURS-19,
              TRSFILE.TS_FEECODE_20 = :QTS-FEECODE-20,
              TRSFILE.TS_FEEAMT_20 = :QTS-FEEAMT-20,
              TRSFILE.TS_FEEOURS_20 = :QTS-FEEOURS-20,
              TRSFILE.TS_INSURED = :QTS-INSURED,
              TRSFILE.TS_INSTYPES = :QTS-INSTYPES,
              TRSFILE.TS_INSELIGIBLE = :QTS-INSELIGIBLE,
              TRSFILE.TS_INSCOMP_1 = :QTS-INSCOMP-1,
              TRSFILE.TS_INSOURS_1 = :QTS-INSOURS-1,
              TRSFILE.TS_INSEFF_DATE_1 =
                 NULLIF(:QTS-INSEFF-DATE-1,'1900-01-01'),
              TRSFILE.TS_INSEXP_DATE_1 =
                 NULLIF(:QTS-INSEXP-DATE-1,'1900-01-01'),
              TRSFILE.TS_INSPREM_1 = :QTS-INSPREM-1,
              TRSFILE.TS_INSCOMM_1 = :QTS-INSCOMM-1,
              TRSFILE.TS_INSANTICERN_1 = :QTS-INSANTICERN-1,
              TRSFILE.TS_INSEARNINGS_1 = :QTS-INSEARNINGS-1,
              TRSFILE.TS_INSCOMP_2 = :QTS-INSCOMP-2,
              TRSFILE.TS_INSOURS_2 = :QTS-INSOURS-2,
              TRSFILE.TS_INSEFF_DATE_2 =
                 NULLIF(:QTS-INSEFF-DATE-2,'1900-01-01'),
              TRSFILE.TS_INSEXP_DATE_2 =
                 NULLIF(:QTS-INSEXP-DATE-2,'1900-01-01'),
              TRSFILE.TS_INSPREM_2 = :QTS-INSPREM-2,
              TRSFILE.TS_INSCOMM_2 = :QTS-INSCOMM-2,
              TRSFILE.TS_INSANTICERN_2 = :QTS-INSANTICERN-2,
              TRSFILE.TS_INSEARNINGS_2 = :QTS-INSEARNINGS-2,
              TRSFILE.TS_INSCOMP_3 = :QTS-INSCOMP-3,
              TRSFILE.TS_INSOURS_3 = :QTS-INSOURS-3,
              TRSFILE.TS_INSEFF_DATE_3 =
                 NULLIF(:QTS-INSEFF-DATE-3,'1900-01-01'),
              TRSFILE.TS_INSEXP_DATE_3 =
                 NULLIF(:QTS-INSEXP-DATE-3,'1900-01-01'),
              TRSFILE.TS_INSPREM_3 = :QTS-INSPREM-3,
              TRSFILE.TS_INSCOMM_3 = :QTS-INSCOMM-3,
              TRSFILE.TS_INSANTICERN_3 = :QTS-INSANTICERN-3,
              TRSFILE.TS_INSEARNINGS_3 = :QTS-INSEARNINGS-3,
              TRSFILE.TS_INSCOMP_4 = :QTS-INSCOMP-4,
              TRSFILE.TS_INSOURS_4 = :QTS-INSOURS-4,
              TRSFILE.TS_INSEFF_DATE_4 =
                 NULLIF(:QTS-INSEFF-DATE-4,'1900-01-01'),
              TRSFILE.TS_INSEXP_DATE_4 =
                 NULLIF(:QTS-INSEXP-DATE-4,'1900-01-01'),
              TRSFILE.TS_INSPREM_4 = :QTS-INSPREM-4,
              TRSFILE.TS_INSCOMM_4 = :QTS-INSCOMM-4,
              TRSFILE.TS_INSANTICERN_4 = :QTS-INSANTICERN-4,
              TRSFILE.TS_INSEARNINGS_4 = :QTS-INSEARNINGS-4,
              TRSFILE.TS_INSCOMP_5 = :QTS-INSCOMP-5,
              TRSFILE.TS_INSOURS_5 = :QTS-INSOURS-5,
              TRSFILE.TS_INSEFF_DATE_5 =
                 NULLIF(:QTS-INSEFF-DATE-5,'1900-01-01'),
              TRSFILE.TS_INSEXP_DATE_5 =
                 NULLIF(:QTS-INSEXP-DATE-5,'1900-01-01'),
              TRSFILE.TS_INSPREM_5 = :QTS-INSPREM-5,
              TRSFILE.TS_INSCOMM_5 = :QTS-INSCOMM-5,
              TRSFILE.TS_INSANTICERN_5 = :QTS-INSANTICERN-5,
              TRSFILE.TS_INSEARNINGS_5 = :QTS-INSEARNINGS-5,
              TRSFILE.TS_INSCOMP_6 = :QTS-INSCOMP-6,
              TRSFILE.TS_INSOURS_6 = :QTS-INSOURS-6,
              TRSFILE.TS_INSEFF_DATE_6 =
                 NULLIF(:QTS-INSEFF-DATE-6,'1900-01-01'),
              TRSFILE.TS_INSEXP_DATE_6 =
                 NULLIF(:QTS-INSEXP-DATE-6,'1900-01-01'),
              TRSFILE.TS_INSPREM_6 = :QTS-INSPREM-6,
              TRSFILE.TS_INSCOMM_6 = :QTS-INSCOMM-6,
              TRSFILE.TS_INSANTICERN_6 = :QTS-INSANTICERN-6,
              TRSFILE.TS_INSEARNINGS_6 = :QTS-INSEARNINGS-6,
              TRSFILE.TS_INSCOMP_7 = :QTS-INSCOMP-7,
              TRSFILE.TS_INSOURS_7 = :QTS-INSOURS-7,
              TRSFILE.TS_INSEFF_DATE_7 =
                 NULLIF(:QTS-INSEFF-DATE-7,'1900-01-01'),
              TRSFILE.TS_INSEXP_DATE_7 =
                 NULLIF(:QTS-INSEXP-DATE-7,'1900-01-01'),
              TRSFILE.TS_INSPREM_7 = :QTS-INSPREM-7,
              TRSFILE.TS_INSCOMM_7 = :QTS-INSCOMM-7,
              TRSFILE.TS_INSANTICERN_7 = :QTS-INSANTICERN-7,
              TRSFILE.TS_INSEARNINGS_7 = :QTS-INSEARNINGS-7,
              TRSFILE.TS_INSCOMP_8 = :QTS-INSCOMP-8,
              TRSFILE.TS_INSOURS_8 = :QTS-INSOURS-8,
              TRSFILE.TS_INSEFF_DATE_8 =
                 NULLIF(:QTS-INSEFF-DATE-8,'1900-01-01'),
              TRSFILE.TS_INSEXP_DATE_8 =
                 NULLIF(:QTS-INSEXP-DATE-8,'1900-01-01'),
              TRSFILE.TS_INSPREM_8 = :QTS-INSPREM-8,
              TRSFILE.TS_INSCOMM_8 = :QTS-INSCOMM-8,
              TRSFILE.TS_INSANTICERN_8 = :QTS-INSANTICERN-8,
              TRSFILE.TS_INSEARNINGS_8 = :QTS-INSEARNINGS-8,
              TRSFILE.TS_INSCOMP_9 = :QTS-INSCOMP-9,
              TRSFILE.TS_INSOURS_9 = :QTS-INSOURS-9,
              TRSFILE.TS_INSEFF_DATE_9 =
                 NULLIF(:QTS-INSEFF-DATE-9,'1900-01-01'),
              TRSFILE.TS_INSEXP_DATE_9 =
                 NULLIF(:QTS-INSEXP-DATE-9,'1900-01-01'),
              TRSFILE.TS_INSPREM_9 = :QTS-INSPREM-9,
              TRSFILE.TS_INSCOMM_9 = :QTS-INSCOMM-9,
              TRSFILE.TS_INSANTICERN_9 = :QTS-INSANTICERN-9,
              TRSFILE.TS_INSEARNINGS_9 = :QTS-INSEARNINGS-9,
              TRSFILE.TS_INSCOMP_10 = :QTS-INSCOMP-10,
              TRSFILE.TS_INSOURS_10 = :QTS-INSOURS-10,
              TRSFILE.TS_INSEFF_DATE_10 =
                 NULLIF(:QTS-INSEFF-DATE-10,'1900-01-01'),
              TRSFILE.TS_INSEXP_DATE_10 =
                 NULLIF(:QTS-INSEXP-DATE-10,'1900-01-01'),
              TRSFILE.TS_INSPREM_10 = :QTS-INSPREM-10,
              TRSFILE.TS_INSCOMM_10 = :QTS-INSCOMM-10,
              TRSFILE.TS_INSANTICERN_10 = :QTS-INSANTICERN-10,
              TRSFILE.TS_INSEARNINGS_10 = :QTS-INSEARNINGS-10,
              TRSFILE.TS_DLCOMM_PP_CMSI = :QTS-DLCOMM-PP-CMSI,
              TRSFILE.TS_DLCOMM_O3_CMSI = :QTS-DLCOMM-O3-CMSI,
              TRSFILE.TS_INS_TAX_COUNTY_CD =
                   :QTS-INS-TAX-COUNTY-CD,
              TRSFILE.TS_INS_TAX_CITY_CD = :QTS-INS-TAX-CITY-CD,
              TRSFILE.TS_TILLNO = :QTS-TILLNO,
              TRSFILE.TS_UNWIND_REASON_CODE =
                   :QTS-UNWIND-REASON-CODE,
              TRSFILE.TS_DLNO = :QTS-DLNO,
              TRSFILE.TS_DLRECOURSE = :QTS-DLRECOURSE,
              TRSFILE.TS_DLPARTRCAMT = :QTS-DLPARTRCAMT,
              TRSFILE.TS_PURBAL = :QTS-PURBAL,
              TRSFILE.TS_DLRVPRIN_1 = :QTS-DLRVPRIN-1,
              TRSFILE.TS_DLRVPRIN_2 = :QTS-DLRVPRIN-2,
              TRSFILE.TS_DLRVPRIN_3 = :QTS-DLRVPRIN-3,
              TRSFILE.TS_DLRVCHGS_1 = :QTS-DLRVCHGS-1,
              TRSFILE.TS_DLRVCHGS_2 = :QTS-DLRVCHGS-2,
              TRSFILE.TS_DLRVCHGS_3 = :QTS-DLRVCHGS-3,
              TRSFILE.TS_DL_RVCHGS_CODE_1 = :QTS-DL-RVCHGS-CODE-1,
              TRSFILE.TS_DL_RVCHGS_CODE_2 = :QTS-DL-RVCHGS-CODE-2,
              TRSFILE.TS_DL_RVCHGS_CODE_3 = :QTS-DL-RVCHGS-CODE-3,
              TRSFILE.TS_DL_PARVHELD_CODE = :QTS-DL-PARVHELD-CODE,
              TRSFILE.TS_DL_PARVPAID_CODE = :QTS-DL-PARVPAID-CODE,
              TRSFILE.TS_DLPARVHELD = :QTS-DLPARVHELD,
              TRSFILE.TS_DLPARVPAID = :QTS-DLPARVPAID,
              TRSFILE.TS_DLDISC = :QTS-DLDISC,
              TRSFILE.TS_ANTICTERM = :QTS-ANTICTERM,
              TRSFILE.TS_ANTICERN_1 = :QTS-ANTICERN-1,
              TRSFILE.TS_ANTICERN_2 = :QTS-ANTICERN-2,
              TRSFILE.TS_ANTICERN_3 = :QTS-ANTICERN-3,
              TRSFILE.TS_ANTICERN_4 = :QTS-ANTICERN-4,
              TRSFILE.TS_ANTICERN_5 = :QTS-ANTICERN-5,
              TRSFILE.TS_ANTICERN_6 = :QTS-ANTICERN-6,
              TRSFILE.TS_ANTICERN_7 = :QTS-ANTICERN-7,
              TRSFILE.TS_ANTICERN_8 = :QTS-ANTICERN-8,
              TRSFILE.TS_ANTICERN_9 = :QTS-ANTICERN-9,
              TRSFILE.TS_ANTICERN_10 = :QTS-ANTICERN-10,
              TRSFILE.TS_EARNINGS_1 = :QTS-EARNINGS-1,
              TRSFILE.TS_EARNINGS_2 = :QTS-EARNINGS-2,
              TRSFILE.TS_EARNINGS_3 = :QTS-EARNINGS-3,
              TRSFILE.TS_EARNINGS_4 = :QTS-EARNINGS-4,
              TRSFILE.TS_EARNINGS_5 = :QTS-EARNINGS-5,
              TRSFILE.TS_EARNINGS_6 = :QTS-EARNINGS-6,
              TRSFILE.TS_EARNINGS_7 = :QTS-EARNINGS-7,
              TRSFILE.TS_EARNINGS_8 = :QTS-EARNINGS-8,
              TRSFILE.TS_EARNINGS_9 = :QTS-EARNINGS-9,
              TRSFILE.TS_EARNINGS_10 = :QTS-EARNINGS-10,
              TRSFILE.TS_PRLNNO = :QTS-PRLNNO,
              TRSFILE.TS_INSPREM_DLADJ = :QTS-INSPREM-DLADJ,
              TRSFILE.TS_PRDLDISC_UNEARN = :QTS-PRDLDISC-UNEARN,
              TRSFILE.TS_SP_LNCOST_FRMLA = :QTS-SP-LNCOST-FRMLA,
              TRSFILE.TS_DLPAID = :QTS-DLPAID,
              TRSFILE.TS_MIP_FG = :QTS-MIP-FG,
              TRSFILE.TS_SPRCLASS = :QTS-SPRCLASS,
              TRSFILE.TS_O3INS_DLPROC = :QTS-O3INS-DLPROC,
              TRSFILE.TS_USERID = :QTS-USERID,
              TRSFILE.TS_POSTDATE =
                 NULLIF(:QTS-POSTDATE,'1900-01-01'),
              TRSFILE.TS_POSTTIME = :QTS-POSTTIME,
              TRSFILE.TS_TAX_AMT = :QTS-TAX-AMT,
              TRSFILE.TS_APPROVED_BY_USER = :QTS-APPROVED-BY-USER,
              TRSFILE.TS_APPROVED_BY_CODE = :QTS-APPROVED-BY-CODE,
              TRSFILE.TS_APPROVED_BY_DATE =
                 NULLIF(:QTS-APPROVED-BY-DATE,'1900-01-01'),
              TRSFILE.TS_APPROVED_BY_TIME = :QTS-APPROVED-BY-TIME
            WHERE TRSFILE.TS_BRNO = :QTS-BRNO
              AND TRSFILE.TS_DATE = :QTS-DATE
              AND TRSFILE.TS_CLASS = :QTS-CLASS
              AND TRSFILE.TS_ACCTNO = :QTS-ACCTNO
              AND TRSFILE.TS_LAWCODE = :QTS-LAWCODE
              AND TRSFILE.TS_LINE = :QTS-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TRS1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-TRS1-FILE.

           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           PERFORM DELETE-IT.
           MOVE TRS-SELECT-PATH TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.
           PERFORM SET-TRS-FIELDS.

           EXEC SQL
            DELETE FROM DBO.TRSFILE
            WHERE TRSFILE.TS_BRNO = :QTS-BRNO
              AND TRSFILE.TS_DATE = :QTS-DATE
              AND TRSFILE.TS_CLASS = :QTS-CLASS
              AND TRSFILE.TS_ACCTNO = :QTS-ACCTNO
              AND TRSFILE.TS_LAWCODE = :QTS-LAWCODE
              AND TRSFILE.TS_LINE = :QTS-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRS1-FILE.

      *-----------------------------------------------------------------
       CLOSE-TRS1-FILE.

           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRS1-CURSOR-STAT.

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
              DESTROY IOTRSR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
