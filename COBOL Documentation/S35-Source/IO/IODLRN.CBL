       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IODLRN.
       AUTHOR.        BAH.
       DATE-WRITTEN.  04-02-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IODLRN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : DLFILE 
      * TABLE DESC    : DEALER MASTER FILE
      * OUTPUT FILE   : XPC/IODLRN.PRE
      * USED FOR      : LIBLP/LPWSDL.CPY  --- QDL-REC
      *                 LIBLP/LPDL1R.CPY  --- READ
      *                 LIBLP/LPDL1RN.CPY --- READ/READ-NEXT/START
      *                 LIBLP/LPDL2RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0402 ***NEW***
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
           "@(#)IO/IODLRN.CBL S34 05/20/21-08:54:31 >".

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
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LPWSDL.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QDL1-WHERE-BEG-KEY.
           03  QDL1-WBEG-BRNO             PIC 9(4).
           03  QDL1-WBEG-NUM             PIC 9(6).
 
       01  QDL1-WHERE-END-KEY.
           03  QDL1-WEND-BRNO             PIC 9(4).
           03  QDL1-WEND-NUM             PIC 9(6).

       01  QDL2-WHERE-BEG-KEY.
           03  QDL2-WBEG-BRNO             PIC 9(4).
           03  QDL2-WBEG-SHORT             PIC X(16).
 
       01  QDL2-WHERE-END-KEY.
           03  QDL2-WEND-BRNO             PIC 9(4).
           03  QDL2-WEND-SHORT             PIC X(16).
 
 
       01  QDL-REC.
           03  QDL-BRNO                    PIC 9(4).
           03  QDL-NUM                     PIC 9(6).
           03  QDL-LTOUCH-DATE             PIC X(10).
           03  QDL-NAME                    PIC X(25).
           03  QDL-ADR1                    PIC X(25).
           03  QDL-ADR2                    PIC X(25).
           03  QDL-CITY                    PIC X(15).
           03  QDL-STATE                   PIC XX.
           03  QDL-ZIP                     PIC X(10).
           03  QDL-SHORT                   PIC X(16).
           03  QDL-PHONE                   PIC 9(10).
           03  QDL-PURPOSE                 PIC X(3).
           03  QDL-MFGCODE                 PIC X(3).
           03  QDL-ORIGDATE                PIC X(10).
           03  QDL-TERMDATE                PIC X(10).
           03  QDL-LC-CODE                 PIC 99.
           03  QDL-FULLCEIL                PIC S9(7)V99.
           03  QDL-PARTCEIL                PIC S9(7)V99.
           03  QDL-NONCEIL                 PIC S9(7)V99.
           03  QDL-SIGN1                   PIC X(25).
           03  QDL-SIGN2                   PIC X(25).
           03  QDL-SIGN3                   PIC X(25).
           03  QDL-MAXPURTRM               PIC 9(4).
           03  QDL-MAXPURPER               PIC 9(4).
           03  QDL-EARNCD                  PIC 99.
           03  QDL-EARNFAC                 PIC 9(4).
           03  QDL-DISC-FRMLA              PIC 99.
           03  QDL-DISC-CODE               PIC X.
           03  QDL-DISC-TERM-1             PIC 9(3).
           03  QDL-DISC-RATE-1             PIC 99V9(4).
           03  QDL-DISC-TERM-2             PIC 9(3).
           03  QDL-DISC-RATE-2             PIC 99V9(4).
           03  QDL-DISC-TERM-3             PIC 9(3).
           03  QDL-DISC-RATE-3             PIC 99V9(4).
           03  QDL-DISC-TERM-4             PIC 9(3).
           03  QDL-DISC-RATE-4             PIC 99V9(4).
           03  QDL-DISC-TERM-5             PIC 9(3).
           03  QDL-DISC-RATE-5             PIC 99V9(4).
           03  QDL-DISC-TERM-6             PIC 9(3).
           03  QDL-DISC-RATE-6             PIC 99V9(4).
           03  QDL-RVPRIN-FRMLA-1     PIC 99.
           03  QDL-RVPRIN-CODE-1      PIC X.
           03  QDL-RVPRIN-ADDON-1     PIC S9(5)V99.
           03  QDL-RVPRIN-TERM-1-1    PIC 9(3).
           03  QDL-RVPRIN-RATE-1-1    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-1-2    PIC 9(3).
           03  QDL-RVPRIN-RATE-1-2    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-1-3    PIC 9(3).
           03  QDL-RVPRIN-RATE-1-3    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-1-4    PIC 9(3).
           03  QDL-RVPRIN-RATE-1-4    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-1-5    PIC 9(3).
           03  QDL-RVPRIN-RATE-1-5    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-1-6    PIC 9(3).
           03  QDL-RVPRIN-RATE-1-6    PIC 99V9(4).
           03  QDL-RVPRIN-FRMLA-2     PIC 99.
           03  QDL-RVPRIN-CODE-2      PIC X.
           03  QDL-RVPRIN-ADDON-2     PIC S9(5)V99.
           03  QDL-RVPRIN-TERM-2-1    PIC 9(3).
           03  QDL-RVPRIN-RATE-2-1    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-2-2    PIC 9(3).
           03  QDL-RVPRIN-RATE-2-2    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-2-3    PIC 9(3).
           03  QDL-RVPRIN-RATE-2-3    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-2-4    PIC 9(3).
           03  QDL-RVPRIN-RATE-2-4    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-2-5    PIC 9(3).
           03  QDL-RVPRIN-RATE-2-5    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-2-6    PIC 9(3).
           03  QDL-RVPRIN-RATE-2-6    PIC 99V9(4).
           03  QDL-RVPRIN-FRMLA-3     PIC 99.
           03  QDL-RVPRIN-CODE-3      PIC X.
           03  QDL-RVPRIN-ADDON-3     PIC S9(5)V99.
           03  QDL-RVPRIN-TERM-3-1    PIC 9(3).
           03  QDL-RVPRIN-RATE-3-1    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-3-2    PIC 9(3).
           03  QDL-RVPRIN-RATE-3-2    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-3-3    PIC 9(3).
           03  QDL-RVPRIN-RATE-3-3    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-3-4    PIC 9(3).
           03  QDL-RVPRIN-RATE-3-4    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-3-5    PIC 9(3).
           03  QDL-RVPRIN-RATE-3-5    PIC 99V9(4).
           03  QDL-RVPRIN-TERM-3-6    PIC 9(3).
           03  QDL-RVPRIN-RATE-3-6    PIC 99V9(4).
           03  QDL-RVCHGS-FRMLA-1     PIC 99.
           03  QDL-RVCHGS-CODE-1      PIC X.
           03  QDL-RVCHGS-ADDON-1     PIC S9(5)V99.
           03  QDL-RVCHGS-TERM-1-1    PIC 9(3).
           03  QDL-RVCHGS-RATE-1-1    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-1-2    PIC 9(3).
           03  QDL-RVCHGS-RATE-1-2    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-1-3    PIC 9(3).
           03  QDL-RVCHGS-RATE-1-3    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-1-4    PIC 9(3).
           03  QDL-RVCHGS-RATE-1-4    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-1-5    PIC 9(3).
           03  QDL-RVCHGS-RATE-1-5    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-1-6    PIC 9(3).
           03  QDL-RVCHGS-RATE-1-6    PIC 99V9(4).
           03  QDL-RVCHGS-FRMLA-2     PIC 99.
           03  QDL-RVCHGS-CODE-2      PIC X.
           03  QDL-RVCHGS-ADDON-2     PIC S9(5)V99.
           03  QDL-RVCHGS-TERM-2-1    PIC 9(3).
           03  QDL-RVCHGS-RATE-2-1    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-2-2    PIC 9(3).
           03  QDL-RVCHGS-RATE-2-2    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-2-3    PIC 9(3).
           03  QDL-RVCHGS-RATE-2-3    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-2-4    PIC 9(3).
           03  QDL-RVCHGS-RATE-2-4    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-2-5    PIC 9(3).
           03  QDL-RVCHGS-RATE-2-5    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-2-6    PIC 9(3).
           03  QDL-RVCHGS-RATE-2-6    PIC 99V9(4).
           03  QDL-RVCHGS-FRMLA-3     PIC 99.
           03  QDL-RVCHGS-CODE-3      PIC X.
           03  QDL-RVCHGS-ADDON-3     PIC S9(5)V99.
           03  QDL-RVCHGS-TERM-3-1    PIC 9(3).
           03  QDL-RVCHGS-RATE-3-1    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-3-2    PIC 9(3).
           03  QDL-RVCHGS-RATE-3-2    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-3-3    PIC 9(3).
           03  QDL-RVCHGS-RATE-3-3    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-3-4    PIC 9(3).
           03  QDL-RVCHGS-RATE-3-4    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-3-5    PIC 9(3).
           03  QDL-RVCHGS-RATE-3-5    PIC 99V9(4).
           03  QDL-RVCHGS-TERM-3-6    PIC 9(3).
           03  QDL-RVCHGS-RATE-3-6    PIC 99V9(4).
           03  QDL-PARVHELD-FRMLA      PIC 99.
           03  QDL-PARVHELD-CODE       PIC X.
           03  QDL-PARVHELD-ADDON      PIC S9(5)V99.
           03  QDL-PARVHELD-TERM-1    PIC 9(3).
           03  QDL-PARVHELD-RATE-1    PIC 99V9(4).
           03  QDL-PARVHELD-TERM-2    PIC 9(3).
           03  QDL-PARVHELD-RATE-2    PIC 99V9(4).
           03  QDL-PARVHELD-TERM-3    PIC 9(3).
           03  QDL-PARVHELD-RATE-3    PIC 99V9(4).
           03  QDL-PARVHELD-TERM-4    PIC 9(3).
           03  QDL-PARVHELD-RATE-4    PIC 99V9(4).
           03  QDL-PARVHELD-TERM-5    PIC 9(3).
           03  QDL-PARVHELD-RATE-5    PIC 99V9(4).
           03  QDL-PARVHELD-TERM-6    PIC 9(3).
           03  QDL-PARVHELD-RATE-6    PIC 99V9(4).
           03  QDL-PARVPAID-FRMLA     PIC 99.
           03  QDL-PARVPAID-CODE      PIC X.
           03  QDL-PARVPAID-ADDON     PIC S9(5)V99.
           03  QDL-PARVPAID-TERM-1    PIC 9(3).
           03  QDL-PARVPAID-RATE-1    PIC 99V9(4).
           03  QDL-PARVPAID-TERM-2    PIC 9(3).
           03  QDL-PARVPAID-RATE-2    PIC 99V9(4).
           03  QDL-PARVPAID-TERM-3    PIC 9(3).
           03  QDL-PARVPAID-RATE-3    PIC 99V9(4).
           03  QDL-PARVPAID-TERM-4    PIC 9(3).
           03  QDL-PARVPAID-RATE-4    PIC 99V9(4).
           03  QDL-PARVPAID-TERM-5    PIC 9(3).
           03  QDL-PARVPAID-RATE-5    PIC 99V9(4).
           03  QDL-PARVPAID-TERM-6    PIC 9(3).
           03  QDL-PARVPAID-RATE-6    PIC 99V9(4).
           03  QDL-SVACCT-FRMLA            PIC 99.
           03  QDL-SVCODE                  PIC X.
           03  QDL-SVACCT-AMT              PIC S9(5)V99.
           03  QDL-SVACCT-RATE             PIC 999V99.
           03  QDL-SVACCT-LCHGPER          PIC 999.
           03  QDL-SVACCT-INTPER           PIC 999.
           03  QDL-SVACCT-DEFPER           PIC 999.
           03  QDL-FULLCEIL-BAL            PIC S9(9)V99.
           03  QDL-PARTCEIL-BAL            PIC S9(9)V99.
           03  QDL-NONCEIL-BAL             PIC S9(9)V99.
           03  QDL-AFP-DEALER              PIC X.
           03  QDL-RECOURSE-FRMLA          PIC X.
           03  QDL-FAX-NUMBER              PIC 9(10).
           03  QDL-PARVPAID-FRMLA-IB       PIC X.
           03  QDL-PARVPAID-CODE-IB        PIC X.
           03  QDL-LPO                     PIC X(4).
           03  QDL-ACH-FG                  PIC X.
           03  QDL-BANK-ROUTING-NO         PIC 9(9).
           03  QDL-BANK-ACCTNO             PIC X(17).
           03  QDL-ACH-PRENOTE-FG          PIC X.
           03  QDL-SAL-MISC-PCT            PIC 99V99.
           03  QDL-SAL-MISC-PCT2           PIC 99V99.
           03  QDL-WAIVE-ACH-FEE           PIC X.
           03  QDL-TRACK-CHANGE-FLAG       PIC X.
           03  QDL-DBA-NAME                PIC X(40).

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
           03  BEG-NUM               PIC 9(06).
           03  END-NUM               PIC 9(06).
           03  BEG-SHORT              PIC X(15).
           03  END-SHORT              PIC X(15).

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
           03  FILLER PIC X(35) VALUE "BRNO    DNO    SHORT".
           03  FILLER PIC X(31) VALUE "CITY              ORIGDATE     ".
           03  FILLER PIC X(30) VALUE "BANK ACCT".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-DL-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-DL-NUM                    PIC 9(06).
           03  FILLER                         PIC X(03).
           03  D01-DL-SHORT                   PIC X(16).
           03  FILLER                         PIC X(03).
           03  D01-DL-CITY                    PIC X(15).
           03  FILLER                         PIC X(03).
           03  D01-DL-ORIGDATE                PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-DL-BANK-ACCTNO             PIC X(17).

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
       COPY "LIBIO/IODLRN_DEF.CPY".
       COPY "LIBIO/IODLRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IODLRN_SCN.CPY".

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
           MOVE "IODLRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IODLRN-QUEUE-POS  TO Q-POS.
           MOVE IODLRN-COPIES-POS TO C-POS.
           MOVE IODLRN-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-SHORT.

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
       ENTER-BEG-NUM.

           MOVE "ENNN060BEG-NUM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DLNUM.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-NUM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-NUM.

      *-----------------------------------------------------------------
       ENTER-END-NUM.

           MOVE "ENNN060END-NUM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-NUM.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-NUM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-NUM.

           IF ( BEG-NUM > END-NUM )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-NUM.

           GO TO ENTER-BEG-SHORT.

      *-----------------------------------------------------------------
       ENTER-ALL-DLNUM.

           MOVE "SNNN060BEG-NUM." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-NUM.
           PERFORM SCREENIO.

           MOVE "SNNN060END-NUM." TO VDU.
           MOVE 999999               TO VDUNUM0  END-NUM.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SHORT.

           MOVE "E  A150BEG-SHORT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-NUM.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SHORT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SHORT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-SHORT.

      *-----------------------------------------------------------------
       ENTER-END-SHORT.

           MOVE "E  A150END-SHORT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SHORT.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SHORT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-SHORT.

           IF ( BEG-SHORT > END-SHORT )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SHORT.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-SHORT.

           MOVE "S  A000BEG-SHORT." TO VDU.
           MOVE SPACES              TO VDUBUF  BEG-SHORT.
           PERFORM SCREENIO.

           MOVE "SNNN080END-SHORT." TO VDU.
           MOVE ALL "Z"             TO VDUBUF  END-SHORT.
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

           MOVE "SNNN060BEG-NUM."       TO VDU.
           MOVE BEG-NUM                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-NUM."       TO VDU.
           MOVE END-NUM                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-SHORT."      TO VDU.
           MOVE BEG-SHORT                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-SHORT."      TO VDU.
           MOVE END-SHORT                TO VDUBUF.
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

           MOVE "B"                        TO DL-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QDL1-WBEG-BRNO 
                              QDL2-WBEG-BRNO.

           MOVE BR-NO     TO QDL1-WEND-BRNO
                              QDL2-WEND-BRNO.

           MOVE BEG-NUM  TO QDL1-WBEG-NUM.
           MOVE END-NUM  TO QDL1-WEND-NUM.

           MOVE BEG-SHORT TO QDL2-WBEG-SHORT.
           MOVE END-SHORT TO QDL2-WEND-SHORT.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-DL1-FILE
              PERFORM START-DL1-FILE
           ELSE
              PERFORM OPEN-DL2-FILE
              PERFORM START-DL2-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-DL1-FILE-NEXT
           ELSE
              PERFORM READ-DL2-FILE-NEXT.
           IF ( IO-BAD ) OR ( DL-BRNO NOT = DL-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( DL-NUM < BEG-NUM )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( DL-NUM > END-NUM )
                 GO TO PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 2 )
              IF ( DL-SHORT  < BEG-SHORT  )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( DL-SHORT  > END-SHORT  )
                 GO TO PROCESS-BRANCH-MODULE-END.



      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE DL-BRNO                    TO D01-DL-BRNO.
           MOVE DL-NUM                    TO D01-DL-NUM.
           MOVE DL-SHORT                   TO D01-DL-SHORT.
           MOVE DL-CITY                    TO D01-DL-CITY.
           MOVE DL-ORIGDATE                TO D01-DL-ORIGDATE.
           MOVE DL-BANK-ACCTNO             TO D01-DL-BANK-ACCTNO.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 1 )
              PERFORM CLOSE-DL1-FILE
           ELSE
              PERFORM CLOSE-DL2-FILE.

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

           IF ( PROCESS-KEY = 1 )
              MOVE DL1-KEY TO PACE-DETAIL-KEY
           ELSE
           IF ( PROCESS-KEY = 2 )
              STRING DL-BRNO, DL-SHORT
                     DELIMITED BY SIZE INTO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING DLNUM               : "    TO R01-PROMPT.
           MOVE BEG-NUM                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DLNUM               : "    TO R01-PROMPT.
           MOVE END-NUM                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SHORT              : "    TO R01-PROMPT.
           MOVE BEG-SHORT                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SHORT              : "    TO R01-PROMPT.
           MOVE END-SHORT                            TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - DLFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IODLRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IODLRN-SYSDATE.
           DISPLAY IODLRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IODLRN-FORM-FIELDS TO WR-BUF.
           MOVE IODLRN-FORM-LIST TO WR-LIST.
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

           MOVE IODLRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IODLRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IODLRN_EVA.CPY".
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
      *    LOAD DL-REC FROM QDL-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-DL-FIELDS.
           MOVE QDL-BRNO TO DL-BRNO.
           MOVE QDL-NUM TO DL-NUM.

           IF ( QDL-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO DL-LTOUCH-DATE
           ELSE
              MOVE QDL-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO DL-LTOUCH-DATE.

           MOVE QDL-NAME TO DL-NAME.
           MOVE QDL-ADR1 TO DL-ADR1.
           MOVE QDL-ADR2 TO DL-ADR2.
           MOVE QDL-CITY TO DL-CITY.
           MOVE QDL-STATE TO DL-STATE.
           MOVE QDL-ZIP TO DL-ZIP.
           MOVE QDL-SHORT TO DL-SHORT.
           MOVE QDL-PHONE TO DL-PHONE.
           MOVE QDL-PURPOSE TO DL-PURPOSE.
           MOVE QDL-MFGCODE TO DL-MFGCODE.

           IF ( QDL-ORIGDATE = "1900-01-01" )
              MOVE ZEROES TO DL-ORIGDATE
           ELSE
              MOVE QDL-ORIGDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO DL-ORIGDATE.


           IF ( QDL-TERMDATE = "1900-01-01" )
              MOVE ZEROES TO DL-TERMDATE
           ELSE
              MOVE QDL-TERMDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO DL-TERMDATE.

           MOVE QDL-LC-CODE TO DL-LC-CODE.
           MOVE QDL-FULLCEIL TO DL-FULLCEIL.
           MOVE QDL-PARTCEIL TO DL-PARTCEIL.
           MOVE QDL-NONCEIL TO DL-NONCEIL.
           MOVE QDL-SIGN1 TO DL-SIGN1.
           MOVE QDL-SIGN2 TO DL-SIGN2.
           MOVE QDL-SIGN3 TO DL-SIGN3.
           MOVE QDL-MAXPURTRM TO DL-MAXPURTRM.
           MOVE QDL-MAXPURPER TO DL-MAXPURPER.
           MOVE QDL-EARNCD TO DL-EARNCD.
           MOVE QDL-EARNFAC TO DL-EARNFAC.
           MOVE QDL-DISC-FRMLA TO DL-DISC-FRMLA.
           MOVE QDL-DISC-CODE TO DL-DISC-CODE.
           MOVE QDL-DISC-TERM-1 TO DL-DISC-TERM(1).
           MOVE QDL-DISC-RATE-1 TO DL-DISC-RATE(1).
           MOVE QDL-DISC-TERM-2 TO DL-DISC-TERM(2).
           MOVE QDL-DISC-RATE-2 TO DL-DISC-RATE(2).
           MOVE QDL-DISC-TERM-3 TO DL-DISC-TERM(3).
           MOVE QDL-DISC-RATE-3 TO DL-DISC-RATE(3).
           MOVE QDL-DISC-TERM-4 TO DL-DISC-TERM(4).
           MOVE QDL-DISC-RATE-4 TO DL-DISC-RATE(4).
           MOVE QDL-DISC-TERM-5 TO DL-DISC-TERM(5).
           MOVE QDL-DISC-RATE-5 TO DL-DISC-RATE(5).
           MOVE QDL-DISC-TERM-6 TO DL-DISC-TERM(6).
           MOVE QDL-DISC-RATE-6 TO DL-DISC-RATE(6).
           MOVE QDL-RVPRIN-FRMLA-1 TO DL-RVPRIN-FRMLA(1).
           MOVE QDL-RVPRIN-CODE-1 TO DL-RVPRIN-CODE(1).
           MOVE QDL-RVPRIN-ADDON-1 TO DL-RVPRIN-ADDON(1).
           MOVE QDL-RVPRIN-TERM-1-1 TO DL-RVPRIN-TERM(1 1).
           MOVE QDL-RVPRIN-RATE-1-1 TO DL-RVPRIN-RATE(1 1).
           MOVE QDL-RVPRIN-TERM-1-2 TO DL-RVPRIN-TERM(1 2).
           MOVE QDL-RVPRIN-RATE-1-2 TO DL-RVPRIN-RATE(1 2).
           MOVE QDL-RVPRIN-TERM-1-3 TO DL-RVPRIN-TERM(1 3).
           MOVE QDL-RVPRIN-RATE-1-3 TO DL-RVPRIN-RATE(1 3).
           MOVE QDL-RVPRIN-TERM-1-4 TO DL-RVPRIN-TERM(1 4).
           MOVE QDL-RVPRIN-RATE-1-4 TO DL-RVPRIN-RATE(1 4).
           MOVE QDL-RVPRIN-TERM-1-5 TO DL-RVPRIN-TERM(1 5).
           MOVE QDL-RVPRIN-RATE-1-5 TO DL-RVPRIN-RATE(1 5).
           MOVE QDL-RVPRIN-TERM-1-6 TO DL-RVPRIN-TERM(1 6).
           MOVE QDL-RVPRIN-RATE-1-6 TO DL-RVPRIN-RATE(1 6).
           MOVE QDL-RVPRIN-FRMLA-2 TO DL-RVPRIN-FRMLA(2).
           MOVE QDL-RVPRIN-CODE-2 TO DL-RVPRIN-CODE(2).
           MOVE QDL-RVPRIN-ADDON-2 TO DL-RVPRIN-ADDON(2).
           MOVE QDL-RVPRIN-TERM-2-1 TO DL-RVPRIN-TERM(2 1).
           MOVE QDL-RVPRIN-RATE-2-1 TO DL-RVPRIN-RATE(2 1).
           MOVE QDL-RVPRIN-TERM-2-2 TO DL-RVPRIN-TERM(2 2).
           MOVE QDL-RVPRIN-RATE-2-2 TO DL-RVPRIN-RATE(2 2).
           MOVE QDL-RVPRIN-TERM-2-3 TO DL-RVPRIN-TERM(2 3).
           MOVE QDL-RVPRIN-RATE-2-3 TO DL-RVPRIN-RATE(2 3).
           MOVE QDL-RVPRIN-TERM-2-4 TO DL-RVPRIN-TERM(2 4).
           MOVE QDL-RVPRIN-RATE-2-4 TO DL-RVPRIN-RATE(2 4).
           MOVE QDL-RVPRIN-TERM-2-5 TO DL-RVPRIN-TERM(2 5).
           MOVE QDL-RVPRIN-RATE-2-5 TO DL-RVPRIN-RATE(2 5).
           MOVE QDL-RVPRIN-TERM-2-6 TO DL-RVPRIN-TERM(2 6).
           MOVE QDL-RVPRIN-RATE-2-6 TO DL-RVPRIN-RATE(2 6).
           MOVE QDL-RVPRIN-FRMLA-3 TO DL-RVPRIN-FRMLA(3).
           MOVE QDL-RVPRIN-CODE-3 TO DL-RVPRIN-CODE(3).
           MOVE QDL-RVPRIN-ADDON-3 TO DL-RVPRIN-ADDON(3).
           MOVE QDL-RVPRIN-TERM-3-1 TO DL-RVPRIN-TERM(3 1).
           MOVE QDL-RVPRIN-RATE-3-1 TO DL-RVPRIN-RATE(3 1).
           MOVE QDL-RVPRIN-TERM-3-2 TO DL-RVPRIN-TERM(3 2).
           MOVE QDL-RVPRIN-RATE-3-2 TO DL-RVPRIN-RATE(3 2).
           MOVE QDL-RVPRIN-TERM-3-3 TO DL-RVPRIN-TERM(3 3).
           MOVE QDL-RVPRIN-RATE-3-3 TO DL-RVPRIN-RATE(3 3).
           MOVE QDL-RVPRIN-TERM-3-4 TO DL-RVPRIN-TERM(3 4).
           MOVE QDL-RVPRIN-RATE-3-4 TO DL-RVPRIN-RATE(3 4).
           MOVE QDL-RVPRIN-TERM-3-5 TO DL-RVPRIN-TERM(3 5).
           MOVE QDL-RVPRIN-RATE-3-5 TO DL-RVPRIN-RATE(3 5).
           MOVE QDL-RVPRIN-TERM-3-6 TO DL-RVPRIN-TERM(3 6).
           MOVE QDL-RVPRIN-RATE-3-6 TO DL-RVPRIN-RATE(3 6).
           MOVE QDL-RVCHGS-FRMLA-1 TO DL-RVCHGS-FRMLA(1).
           MOVE QDL-RVCHGS-CODE-1 TO DL-RVCHGS-CODE(1).
           MOVE QDL-RVCHGS-ADDON-1 TO DL-RVCHGS-ADDON(1).
           MOVE QDL-RVCHGS-TERM-1-1 TO DL-RVCHGS-TERM(1 1).
           MOVE QDL-RVCHGS-RATE-1-1 TO DL-RVCHGS-RATE(1 1).
           MOVE QDL-RVCHGS-TERM-1-2 TO DL-RVCHGS-TERM(1 2).
           MOVE QDL-RVCHGS-RATE-1-2 TO DL-RVCHGS-RATE(1 2).
           MOVE QDL-RVCHGS-TERM-1-3 TO DL-RVCHGS-TERM(1 3).
           MOVE QDL-RVCHGS-RATE-1-3 TO DL-RVCHGS-RATE(1 3).
           MOVE QDL-RVCHGS-TERM-1-4 TO DL-RVCHGS-TERM(1 4).
           MOVE QDL-RVCHGS-RATE-1-4 TO DL-RVCHGS-RATE(1 4).
           MOVE QDL-RVCHGS-TERM-1-5 TO DL-RVCHGS-TERM(1 5).
           MOVE QDL-RVCHGS-RATE-1-5 TO DL-RVCHGS-RATE(1 5).
           MOVE QDL-RVCHGS-TERM-1-6 TO DL-RVCHGS-TERM(1 6).
           MOVE QDL-RVCHGS-RATE-1-6 TO DL-RVCHGS-RATE(1 6).
           MOVE QDL-RVCHGS-FRMLA-2 TO DL-RVCHGS-FRMLA(2).
           MOVE QDL-RVCHGS-CODE-2 TO DL-RVCHGS-CODE(2).
           MOVE QDL-RVCHGS-ADDON-2 TO DL-RVCHGS-ADDON(2).
           MOVE QDL-RVCHGS-TERM-2-1 TO DL-RVCHGS-TERM(2 1).
           MOVE QDL-RVCHGS-RATE-2-1 TO DL-RVCHGS-RATE(2 1).
           MOVE QDL-RVCHGS-TERM-2-2 TO DL-RVCHGS-TERM(2 2).
           MOVE QDL-RVCHGS-RATE-2-2 TO DL-RVCHGS-RATE(2 2).
           MOVE QDL-RVCHGS-TERM-2-3 TO DL-RVCHGS-TERM(2 3).
           MOVE QDL-RVCHGS-RATE-2-3 TO DL-RVCHGS-RATE(2 3).
           MOVE QDL-RVCHGS-TERM-2-4 TO DL-RVCHGS-TERM(2 4).
           MOVE QDL-RVCHGS-RATE-2-4 TO DL-RVCHGS-RATE(2 4).
           MOVE QDL-RVCHGS-TERM-2-5 TO DL-RVCHGS-TERM(2 5).
           MOVE QDL-RVCHGS-RATE-2-5 TO DL-RVCHGS-RATE(2 5).
           MOVE QDL-RVCHGS-TERM-2-6 TO DL-RVCHGS-TERM(2 6).
           MOVE QDL-RVCHGS-RATE-2-6 TO DL-RVCHGS-RATE(2 6).
           MOVE QDL-RVCHGS-FRMLA-3 TO DL-RVCHGS-FRMLA(3).
           MOVE QDL-RVCHGS-CODE-3 TO DL-RVCHGS-CODE(3).
           MOVE QDL-RVCHGS-ADDON-3 TO DL-RVCHGS-ADDON(3).
           MOVE QDL-RVCHGS-TERM-3-1 TO DL-RVCHGS-TERM(3 1).
           MOVE QDL-RVCHGS-RATE-3-1 TO DL-RVCHGS-RATE(3 1).
           MOVE QDL-RVCHGS-TERM-3-2 TO DL-RVCHGS-TERM(3 2).
           MOVE QDL-RVCHGS-RATE-3-2 TO DL-RVCHGS-RATE(3 2).
           MOVE QDL-RVCHGS-TERM-3-3 TO DL-RVCHGS-TERM(3 3).
           MOVE QDL-RVCHGS-RATE-3-3 TO DL-RVCHGS-RATE(3 3).
           MOVE QDL-RVCHGS-TERM-3-4 TO DL-RVCHGS-TERM(3 4).
           MOVE QDL-RVCHGS-RATE-3-4 TO DL-RVCHGS-RATE(3 4).
           MOVE QDL-RVCHGS-TERM-3-5 TO DL-RVCHGS-TERM(3 5).
           MOVE QDL-RVCHGS-RATE-3-5 TO DL-RVCHGS-RATE(3 5).
           MOVE QDL-RVCHGS-TERM-3-6 TO DL-RVCHGS-TERM(3 6).
           MOVE QDL-RVCHGS-RATE-3-6 TO DL-RVCHGS-RATE(3 6).
           MOVE QDL-PARVHELD-FRMLA TO DL-PARVHELD-FRMLA.
           MOVE QDL-PARVHELD-CODE TO DL-PARVHELD-CODE.
           MOVE QDL-PARVHELD-ADDON TO DL-PARVHELD-ADDON.
           MOVE QDL-PARVHELD-TERM-1 TO DL-PARVHELD-TERM(1).
           MOVE QDL-PARVHELD-RATE-1 TO DL-PARVHELD-RATE(1).
           MOVE QDL-PARVHELD-TERM-2 TO DL-PARVHELD-TERM(2).
           MOVE QDL-PARVHELD-RATE-2 TO DL-PARVHELD-RATE(2).
           MOVE QDL-PARVHELD-TERM-3 TO DL-PARVHELD-TERM(3).
           MOVE QDL-PARVHELD-RATE-3 TO DL-PARVHELD-RATE(3).
           MOVE QDL-PARVHELD-TERM-4 TO DL-PARVHELD-TERM(4).
           MOVE QDL-PARVHELD-RATE-4 TO DL-PARVHELD-RATE(4).
           MOVE QDL-PARVHELD-TERM-5 TO DL-PARVHELD-TERM(5).
           MOVE QDL-PARVHELD-RATE-5 TO DL-PARVHELD-RATE(5).
           MOVE QDL-PARVHELD-TERM-6 TO DL-PARVHELD-TERM(6).
           MOVE QDL-PARVHELD-RATE-6 TO DL-PARVHELD-RATE(6).
           MOVE QDL-PARVPAID-FRMLA TO DL-PARVPAID-FRMLA.
           MOVE QDL-PARVPAID-CODE TO DL-PARVPAID-CODE.
           MOVE QDL-PARVPAID-ADDON TO DL-PARVPAID-ADDON.
           MOVE QDL-PARVPAID-TERM-1 TO DL-PARVPAID-TERM(1).
           MOVE QDL-PARVPAID-RATE-1 TO DL-PARVPAID-RATE(1).
           MOVE QDL-PARVPAID-TERM-2 TO DL-PARVPAID-TERM(2).
           MOVE QDL-PARVPAID-RATE-2 TO DL-PARVPAID-RATE(2).
           MOVE QDL-PARVPAID-TERM-3 TO DL-PARVPAID-TERM(3).
           MOVE QDL-PARVPAID-RATE-3 TO DL-PARVPAID-RATE(3).
           MOVE QDL-PARVPAID-TERM-4 TO DL-PARVPAID-TERM(4).
           MOVE QDL-PARVPAID-RATE-4 TO DL-PARVPAID-RATE(4).
           MOVE QDL-PARVPAID-TERM-5 TO DL-PARVPAID-TERM(5).
           MOVE QDL-PARVPAID-RATE-5 TO DL-PARVPAID-RATE(5).
           MOVE QDL-PARVPAID-TERM-6 TO DL-PARVPAID-TERM(6).
           MOVE QDL-PARVPAID-RATE-6 TO DL-PARVPAID-RATE(6).
           MOVE QDL-SVACCT-FRMLA TO DL-SVACCT-FRMLA.
           MOVE QDL-SVCODE TO DL-SVCODE.
           MOVE QDL-SVACCT-AMT TO DL-SVACCT-AMT.
           MOVE QDL-SVACCT-RATE TO DL-SVACCT-RATE.
           MOVE QDL-SVACCT-LCHGPER TO DL-SVACCT-LCHGPER.
           MOVE QDL-SVACCT-INTPER TO DL-SVACCT-INTPER.
           MOVE QDL-SVACCT-DEFPER TO DL-SVACCT-DEFPER.
           MOVE QDL-FULLCEIL-BAL TO DL-FULLCEIL-BAL.
           MOVE QDL-PARTCEIL-BAL TO DL-PARTCEIL-BAL.
           MOVE QDL-NONCEIL-BAL TO DL-NONCEIL-BAL.
           MOVE QDL-AFP-DEALER TO DL-AFP-DEALER.
           MOVE QDL-RECOURSE-FRMLA TO DL-RECOURSE-FRMLA.
           MOVE QDL-FAX-NUMBER TO DL-FAX-NUMBER.
           MOVE QDL-PARVPAID-FRMLA-IB TO DL-PARVPAID-FRMLA-IB.
           MOVE QDL-PARVPAID-CODE-IB TO DL-PARVPAID-CODE-IB.
           MOVE QDL-LPO TO DL-LPO.
           MOVE QDL-ACH-FG TO DL-ACH-FG.
           MOVE QDL-BANK-ROUTING-NO TO DL-BANK-ROUTING-NO.
           MOVE QDL-BANK-ACCTNO TO DL-BANK-ACCTNO.
           MOVE QDL-ACH-PRENOTE-FG TO DL-ACH-PRENOTE-FG.
           MOVE QDL-SAL-MISC-PCT TO DL-SAL-MISC-PCT.
           MOVE QDL-SAL-MISC-PCT2 TO DL-SAL-MISC-PCT2.
           MOVE QDL-WAIVE-ACH-FEE TO DL-WAIVE-ACH-FEE.
           MOVE QDL-TRACK-CHANGE-FLAG TO DL-TRACK-CHANGE-FLAG.
           MOVE QDL-DBA-NAME TO DL-DBA-NAME.
 
      *-----------------------------------------------------------------
      *    LOAD QDL-REC FROM DL-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-DL-FIELDS.
           MOVE DL-BRNO TO QDL-BRNO.
           MOVE DL-NUM TO QDL-NUM.

           IF ( DL-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QDL-LTOUCH-DATE
           ELSE
              MOVE DL-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QDL-LTOUCH-DATE.

           MOVE DL-NAME TO QDL-NAME.
           MOVE DL-ADR1 TO QDL-ADR1.
           MOVE DL-ADR2 TO QDL-ADR2.
           MOVE DL-CITY TO QDL-CITY.
           MOVE DL-STATE TO QDL-STATE.
           MOVE DL-ZIP TO QDL-ZIP.
           MOVE DL-SHORT TO QDL-SHORT.
           MOVE DL-PHONE TO QDL-PHONE.
           MOVE DL-PURPOSE TO QDL-PURPOSE.
           MOVE DL-MFGCODE TO QDL-MFGCODE.

           IF ( DL-ORIGDATE = ZEROES )
              MOVE "1900-01-01" TO QDL-ORIGDATE
           ELSE
              MOVE DL-ORIGDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QDL-ORIGDATE.


           IF ( DL-TERMDATE = ZEROES )
              MOVE "1900-01-01" TO QDL-TERMDATE
           ELSE
              MOVE DL-TERMDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QDL-TERMDATE.

           MOVE DL-LC-CODE TO QDL-LC-CODE.
           MOVE DL-FULLCEIL TO QDL-FULLCEIL.
           MOVE DL-PARTCEIL TO QDL-PARTCEIL.
           MOVE DL-NONCEIL TO QDL-NONCEIL.
           MOVE DL-SIGN1 TO QDL-SIGN1.
           MOVE DL-SIGN2 TO QDL-SIGN2.
           MOVE DL-SIGN3 TO QDL-SIGN3.
           MOVE DL-MAXPURTRM TO QDL-MAXPURTRM.
           MOVE DL-MAXPURPER TO QDL-MAXPURPER.
           MOVE DL-EARNCD TO QDL-EARNCD.
           MOVE DL-EARNFAC TO QDL-EARNFAC.
           MOVE DL-DISC-FRMLA TO QDL-DISC-FRMLA.
           MOVE DL-DISC-CODE TO QDL-DISC-CODE.
           MOVE DL-DISC-TERM(1) TO QDL-DISC-TERM-1.
           MOVE DL-DISC-RATE(1) TO QDL-DISC-RATE-1.
           MOVE DL-DISC-TERM(2) TO QDL-DISC-TERM-2.
           MOVE DL-DISC-RATE(2) TO QDL-DISC-RATE-2.
           MOVE DL-DISC-TERM(3) TO QDL-DISC-TERM-3.
           MOVE DL-DISC-RATE(3) TO QDL-DISC-RATE-3.
           MOVE DL-DISC-TERM(4) TO QDL-DISC-TERM-4.
           MOVE DL-DISC-RATE(4) TO QDL-DISC-RATE-4.
           MOVE DL-DISC-TERM(5) TO QDL-DISC-TERM-5.
           MOVE DL-DISC-RATE(5) TO QDL-DISC-RATE-5.
           MOVE DL-DISC-TERM(6) TO QDL-DISC-TERM-6.
           MOVE DL-DISC-RATE(6) TO QDL-DISC-RATE-6.
           MOVE DL-RVPRIN-FRMLA(1) TO QDL-RVPRIN-FRMLA-1.
           MOVE DL-RVPRIN-CODE(1) TO QDL-RVPRIN-CODE-1.
           MOVE DL-RVPRIN-ADDON(1) TO QDL-RVPRIN-ADDON-1.
           MOVE DL-RVPRIN-TERM(1 1) TO QDL-RVPRIN-TERM-1-1.
           MOVE DL-RVPRIN-RATE(1 1) TO QDL-RVPRIN-RATE-1-1.
           MOVE DL-RVPRIN-TERM(1 2) TO QDL-RVPRIN-TERM-1-2.
           MOVE DL-RVPRIN-RATE(1 2) TO QDL-RVPRIN-RATE-1-2.
           MOVE DL-RVPRIN-TERM(1 3) TO QDL-RVPRIN-TERM-1-3.
           MOVE DL-RVPRIN-RATE(1 3) TO QDL-RVPRIN-RATE-1-3.
           MOVE DL-RVPRIN-TERM(1 4) TO QDL-RVPRIN-TERM-1-4.
           MOVE DL-RVPRIN-RATE(1 4) TO QDL-RVPRIN-RATE-1-4.
           MOVE DL-RVPRIN-TERM(1 5) TO QDL-RVPRIN-TERM-1-5.
           MOVE DL-RVPRIN-RATE(1 5) TO QDL-RVPRIN-RATE-1-5.
           MOVE DL-RVPRIN-TERM(1 6) TO QDL-RVPRIN-TERM-1-6.
           MOVE DL-RVPRIN-RATE(1 6) TO QDL-RVPRIN-RATE-1-6.
           MOVE DL-RVPRIN-FRMLA(2) TO QDL-RVPRIN-FRMLA-2.
           MOVE DL-RVPRIN-CODE(2) TO QDL-RVPRIN-CODE-2.
           MOVE DL-RVPRIN-ADDON(2) TO QDL-RVPRIN-ADDON-2.
           MOVE DL-RVPRIN-TERM(2 1) TO QDL-RVPRIN-TERM-2-1.
           MOVE DL-RVPRIN-RATE(2 1) TO QDL-RVPRIN-RATE-2-1.
           MOVE DL-RVPRIN-TERM(2 2) TO QDL-RVPRIN-TERM-2-2.
           MOVE DL-RVPRIN-RATE(2 2) TO QDL-RVPRIN-RATE-2-2.
           MOVE DL-RVPRIN-TERM(2 3) TO QDL-RVPRIN-TERM-2-3.
           MOVE DL-RVPRIN-RATE(2 3) TO QDL-RVPRIN-RATE-2-3.
           MOVE DL-RVPRIN-TERM(2 4) TO QDL-RVPRIN-TERM-2-4.
           MOVE DL-RVPRIN-RATE(2 4) TO QDL-RVPRIN-RATE-2-4.
           MOVE DL-RVPRIN-TERM(2 5) TO QDL-RVPRIN-TERM-2-5.
           MOVE DL-RVPRIN-RATE(2 5) TO QDL-RVPRIN-RATE-2-5.
           MOVE DL-RVPRIN-TERM(2 6) TO QDL-RVPRIN-TERM-2-6.
           MOVE DL-RVPRIN-RATE(2 6) TO QDL-RVPRIN-RATE-2-6.
           MOVE DL-RVPRIN-FRMLA(3) TO QDL-RVPRIN-FRMLA-3.
           MOVE DL-RVPRIN-CODE(3) TO QDL-RVPRIN-CODE-3.
           MOVE DL-RVPRIN-ADDON(3) TO QDL-RVPRIN-ADDON-3.
           MOVE DL-RVPRIN-TERM(3 1) TO QDL-RVPRIN-TERM-3-1.
           MOVE DL-RVPRIN-RATE(3 1) TO QDL-RVPRIN-RATE-3-1.
           MOVE DL-RVPRIN-TERM(3 2) TO QDL-RVPRIN-TERM-3-2.
           MOVE DL-RVPRIN-RATE(3 2) TO QDL-RVPRIN-RATE-3-2.
           MOVE DL-RVPRIN-TERM(3 3) TO QDL-RVPRIN-TERM-3-3.
           MOVE DL-RVPRIN-RATE(3 3) TO QDL-RVPRIN-RATE-3-3.
           MOVE DL-RVPRIN-TERM(3 4) TO QDL-RVPRIN-TERM-3-4.
           MOVE DL-RVPRIN-RATE(3 4) TO QDL-RVPRIN-RATE-3-4.
           MOVE DL-RVPRIN-TERM(3 5) TO QDL-RVPRIN-TERM-3-5.
           MOVE DL-RVPRIN-RATE(3 5) TO QDL-RVPRIN-RATE-3-5.
           MOVE DL-RVPRIN-TERM(3 6) TO QDL-RVPRIN-TERM-3-6.
           MOVE DL-RVPRIN-RATE(3 6) TO QDL-RVPRIN-RATE-3-6.
           MOVE DL-RVCHGS-FRMLA(1) TO QDL-RVCHGS-FRMLA-1.
           MOVE DL-RVCHGS-CODE(1) TO QDL-RVCHGS-CODE-1.
           MOVE DL-RVCHGS-ADDON(1) TO QDL-RVCHGS-ADDON-1.
           MOVE DL-RVCHGS-TERM(1 1) TO QDL-RVCHGS-TERM-1-1.
           MOVE DL-RVCHGS-RATE(1 1) TO QDL-RVCHGS-RATE-1-1.
           MOVE DL-RVCHGS-TERM(1 2) TO QDL-RVCHGS-TERM-1-2.
           MOVE DL-RVCHGS-RATE(1 2) TO QDL-RVCHGS-RATE-1-2.
           MOVE DL-RVCHGS-TERM(1 3) TO QDL-RVCHGS-TERM-1-3.
           MOVE DL-RVCHGS-RATE(1 3) TO QDL-RVCHGS-RATE-1-3.
           MOVE DL-RVCHGS-TERM(1 4) TO QDL-RVCHGS-TERM-1-4.
           MOVE DL-RVCHGS-RATE(1 4) TO QDL-RVCHGS-RATE-1-4.
           MOVE DL-RVCHGS-TERM(1 5) TO QDL-RVCHGS-TERM-1-5.
           MOVE DL-RVCHGS-RATE(1 5) TO QDL-RVCHGS-RATE-1-5.
           MOVE DL-RVCHGS-TERM(1 6) TO QDL-RVCHGS-TERM-1-6.
           MOVE DL-RVCHGS-RATE(1 6) TO QDL-RVCHGS-RATE-1-6.
           MOVE DL-RVCHGS-FRMLA(2) TO QDL-RVCHGS-FRMLA-2.
           MOVE DL-RVCHGS-CODE(2) TO QDL-RVCHGS-CODE-2.
           MOVE DL-RVCHGS-ADDON(2) TO QDL-RVCHGS-ADDON-2.
           MOVE DL-RVCHGS-TERM(2 1) TO QDL-RVCHGS-TERM-2-1.
           MOVE DL-RVCHGS-RATE(2 1) TO QDL-RVCHGS-RATE-2-1.
           MOVE DL-RVCHGS-TERM(2 2) TO QDL-RVCHGS-TERM-2-2.
           MOVE DL-RVCHGS-RATE(2 2) TO QDL-RVCHGS-RATE-2-2.
           MOVE DL-RVCHGS-TERM(2 3) TO QDL-RVCHGS-TERM-2-3.
           MOVE DL-RVCHGS-RATE(2 3) TO QDL-RVCHGS-RATE-2-3.
           MOVE DL-RVCHGS-TERM(2 4) TO QDL-RVCHGS-TERM-2-4.
           MOVE DL-RVCHGS-RATE(2 4) TO QDL-RVCHGS-RATE-2-4.
           MOVE DL-RVCHGS-TERM(2 5) TO QDL-RVCHGS-TERM-2-5.
           MOVE DL-RVCHGS-RATE(2 5) TO QDL-RVCHGS-RATE-2-5.
           MOVE DL-RVCHGS-TERM(2 6) TO QDL-RVCHGS-TERM-2-6.
           MOVE DL-RVCHGS-RATE(2 6) TO QDL-RVCHGS-RATE-2-6.
           MOVE DL-RVCHGS-FRMLA(3) TO QDL-RVCHGS-FRMLA-3.
           MOVE DL-RVCHGS-CODE(3) TO QDL-RVCHGS-CODE-3.
           MOVE DL-RVCHGS-ADDON(3) TO QDL-RVCHGS-ADDON-3.
           MOVE DL-RVCHGS-TERM(3 1) TO QDL-RVCHGS-TERM-3-1.
           MOVE DL-RVCHGS-RATE(3 1) TO QDL-RVCHGS-RATE-3-1.
           MOVE DL-RVCHGS-TERM(3 2) TO QDL-RVCHGS-TERM-3-2.
           MOVE DL-RVCHGS-RATE(3 2) TO QDL-RVCHGS-RATE-3-2.
           MOVE DL-RVCHGS-TERM(3 3) TO QDL-RVCHGS-TERM-3-3.
           MOVE DL-RVCHGS-RATE(3 3) TO QDL-RVCHGS-RATE-3-3.
           MOVE DL-RVCHGS-TERM(3 4) TO QDL-RVCHGS-TERM-3-4.
           MOVE DL-RVCHGS-RATE(3 4) TO QDL-RVCHGS-RATE-3-4.
           MOVE DL-RVCHGS-TERM(3 5) TO QDL-RVCHGS-TERM-3-5.
           MOVE DL-RVCHGS-RATE(3 5) TO QDL-RVCHGS-RATE-3-5.
           MOVE DL-RVCHGS-TERM(3 6) TO QDL-RVCHGS-TERM-3-6.
           MOVE DL-RVCHGS-RATE(3 6) TO QDL-RVCHGS-RATE-3-6.
           MOVE DL-PARVHELD-FRMLA TO QDL-PARVHELD-FRMLA.
           MOVE DL-PARVHELD-CODE TO QDL-PARVHELD-CODE.
           MOVE DL-PARVHELD-ADDON TO QDL-PARVHELD-ADDON.
           MOVE DL-PARVHELD-TERM(1) TO QDL-PARVHELD-TERM-1.
           MOVE DL-PARVHELD-RATE(1) TO QDL-PARVHELD-RATE-1.
           MOVE DL-PARVHELD-TERM(2) TO QDL-PARVHELD-TERM-2.
           MOVE DL-PARVHELD-RATE(2) TO QDL-PARVHELD-RATE-2.
           MOVE DL-PARVHELD-TERM(3) TO QDL-PARVHELD-TERM-3.
           MOVE DL-PARVHELD-RATE(3) TO QDL-PARVHELD-RATE-3.
           MOVE DL-PARVHELD-TERM(4) TO QDL-PARVHELD-TERM-4.
           MOVE DL-PARVHELD-RATE(4) TO QDL-PARVHELD-RATE-4.
           MOVE DL-PARVHELD-TERM(5) TO QDL-PARVHELD-TERM-5.
           MOVE DL-PARVHELD-RATE(5) TO QDL-PARVHELD-RATE-5.
           MOVE DL-PARVHELD-TERM(6) TO QDL-PARVHELD-TERM-6.
           MOVE DL-PARVHELD-RATE(6) TO QDL-PARVHELD-RATE-6.
           MOVE DL-PARVPAID-FRMLA TO QDL-PARVPAID-FRMLA.
           MOVE DL-PARVPAID-CODE TO QDL-PARVPAID-CODE.
           MOVE DL-PARVPAID-ADDON TO QDL-PARVPAID-ADDON.
           MOVE DL-PARVPAID-TERM(1) TO QDL-PARVPAID-TERM-1.
           MOVE DL-PARVPAID-RATE(1) TO QDL-PARVPAID-RATE-1.
           MOVE DL-PARVPAID-TERM(2) TO QDL-PARVPAID-TERM-2.
           MOVE DL-PARVPAID-RATE(2) TO QDL-PARVPAID-RATE-2.
           MOVE DL-PARVPAID-TERM(3) TO QDL-PARVPAID-TERM-3.
           MOVE DL-PARVPAID-RATE(3) TO QDL-PARVPAID-RATE-3.
           MOVE DL-PARVPAID-TERM(4) TO QDL-PARVPAID-TERM-4.
           MOVE DL-PARVPAID-RATE(4) TO QDL-PARVPAID-RATE-4.
           MOVE DL-PARVPAID-TERM(5) TO QDL-PARVPAID-TERM-5.
           MOVE DL-PARVPAID-RATE(5) TO QDL-PARVPAID-RATE-5.
           MOVE DL-PARVPAID-TERM(6) TO QDL-PARVPAID-TERM-6.
           MOVE DL-PARVPAID-RATE(6) TO QDL-PARVPAID-RATE-6.
           MOVE DL-SVACCT-FRMLA TO QDL-SVACCT-FRMLA.
           MOVE DL-SVCODE TO QDL-SVCODE.
           MOVE DL-SVACCT-AMT TO QDL-SVACCT-AMT.
           MOVE DL-SVACCT-RATE TO QDL-SVACCT-RATE.
           MOVE DL-SVACCT-LCHGPER TO QDL-SVACCT-LCHGPER.
           MOVE DL-SVACCT-INTPER TO QDL-SVACCT-INTPER.
           MOVE DL-SVACCT-DEFPER TO QDL-SVACCT-DEFPER.
           MOVE DL-FULLCEIL-BAL TO QDL-FULLCEIL-BAL.
           MOVE DL-PARTCEIL-BAL TO QDL-PARTCEIL-BAL.
           MOVE DL-NONCEIL-BAL TO QDL-NONCEIL-BAL.
           MOVE DL-AFP-DEALER TO QDL-AFP-DEALER.
           MOVE DL-RECOURSE-FRMLA TO QDL-RECOURSE-FRMLA.
           MOVE DL-FAX-NUMBER TO QDL-FAX-NUMBER.
           MOVE DL-PARVPAID-FRMLA-IB TO QDL-PARVPAID-FRMLA-IB.
           MOVE DL-PARVPAID-CODE-IB TO QDL-PARVPAID-CODE-IB.
           MOVE DL-LPO TO QDL-LPO.
           MOVE DL-ACH-FG TO QDL-ACH-FG.
           MOVE DL-BANK-ROUTING-NO TO QDL-BANK-ROUTING-NO.
           MOVE DL-BANK-ACCTNO TO QDL-BANK-ACCTNO.
           MOVE DL-ACH-PRENOTE-FG TO QDL-ACH-PRENOTE-FG.
           MOVE DL-SAL-MISC-PCT TO QDL-SAL-MISC-PCT.
           MOVE DL-SAL-MISC-PCT2 TO QDL-SAL-MISC-PCT2.
           MOVE DL-WAIVE-ACH-FEE TO QDL-WAIVE-ACH-FEE.
           MOVE DL-TRACK-CHANGE-FLAG TO QDL-TRACK-CHANGE-FLAG.
           MOVE DL-DBA-NAME TO QDL-DBA-NAME.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-DL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( DL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( DL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO DL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO DL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO DL-ALL-DATA
              MOVE DL-ALL-PATH             TO DL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( DL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO DL-ALL-BASE
              MOVE FILPATH-MACHINE         TO DL-ALL-MACHINE
              MOVE FILPATH-DATA            TO DL-ALL-DATA
              MOVE DL-ALL-PATH             TO DL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-DLFILE NOT = "S" )
              MOVE DL-PATH-BEG             TO DL-SELECT-PATH-BEG
              MOVE DL-PATH-FILE            TO DL-SELECT-PATH-FILE
           ELSE
              MOVE DL-PATH-SQL             TO DL-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-DL1-FILE.
           PERFORM LOAD-DL1-FILE.
           PERFORM OPEN-IT.
           MOVE DL-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-DL1-FILE.
           PERFORM START-IT.
           MOVE DL-PATH-OWNBR  TO DL-BRNO.
           MOVE DL-SELECT-PATH TO E-FILE.
           MOVE DL1-KEY        TO E-KEYX.

           IF ( DL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-DL1-FILE.

           IF ( QDL1-WEND-NUM = ZEROES    ) OR
              ( QDL1-WEND-NUM NOT NUMERIC )
              MOVE DL-PATH-OWNBR        TO QDL1-WBEG-BRNO
              MOVE DL-PATH-OWNBR        TO QDL1-WEND-BRNO
              MOVE DL-NUM               TO QDL1-WBEG-NUM
              MOVE ALL "9"              TO QDL1-WEND-NUM.

           EXEC SQL
            DECLARE DL1_CURSOR CURSOR FOR
            SELECT DLFILE.DL_BRNO,
                   DLFILE.DL_NUM,
                   ISNULL(CAST(DLFILE.DL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(DLFILE.DL_NAME,''),
                   ISNULL(DLFILE.DL_ADR1,''),
                   ISNULL(DLFILE.DL_ADR2,''),
                   ISNULL(DLFILE.DL_CITY,''),
                   ISNULL(DLFILE.DL_STATE,''),
                   ISNULL(DLFILE.DL_ZIP,''),
                   ISNULL(DLFILE.DL_SHORT,''),
                   DLFILE.DL_PHONE,
                   ISNULL(DLFILE.DL_PURPOSE,''),
                   ISNULL(DLFILE.DL_MFGCODE,''),
                   ISNULL(CAST(DLFILE.DL_ORIGDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(DLFILE.DL_TERMDATE AS VARCHAR(10)),'0'),
                   DLFILE.DL_LC_CODE,
                   DLFILE.DL_FULLCEIL,
                   DLFILE.DL_PARTCEIL,
                   DLFILE.DL_NONCEIL,
                   ISNULL(DLFILE.DL_SIGN1,''),
                   ISNULL(DLFILE.DL_SIGN2,''),
                   ISNULL(DLFILE.DL_SIGN3,''),
                   DLFILE.DL_MAXPURTRM,
                   DLFILE.DL_MAXPURPER,
                   DLFILE.DL_EARNCD,
                   DLFILE.DL_EARNFAC,
                   DLFILE.DL_DISC_FRMLA,
                   ISNULL(DLFILE.DL_DISC_CODE,''),
                   DLFILE.DL_DISC_TERM_1,
                   DLFILE.DL_DISC_RATE_1,
                   DLFILE.DL_DISC_TERM_2,
                   DLFILE.DL_DISC_RATE_2,
                   DLFILE.DL_DISC_TERM_3,
                   DLFILE.DL_DISC_RATE_3,
                   DLFILE.DL_DISC_TERM_4,
                   DLFILE.DL_DISC_RATE_4,
                   DLFILE.DL_DISC_TERM_5,
                   DLFILE.DL_DISC_RATE_5,
                   DLFILE.DL_DISC_TERM_6,
                   DLFILE.DL_DISC_RATE_6,
                   DLFILE.DL_RVPRIN_FRMLA_1,
                   ISNULL(DLFILE.DL_RVPRIN_CODE_1,''),
                   DLFILE.DL_RVPRIN_ADDON_1,
                   DLFILE.DL_RVPRIN_TERM_1_1,
                   DLFILE.DL_RVPRIN_RATE_1_1,
                   DLFILE.DL_RVPRIN_TERM_1_2,
                   DLFILE.DL_RVPRIN_RATE_1_2,
                   DLFILE.DL_RVPRIN_TERM_1_3,
                   DLFILE.DL_RVPRIN_RATE_1_3,
                   DLFILE.DL_RVPRIN_TERM_1_4,
                   DLFILE.DL_RVPRIN_RATE_1_4,
                   DLFILE.DL_RVPRIN_TERM_1_5,
                   DLFILE.DL_RVPRIN_RATE_1_5,
                   DLFILE.DL_RVPRIN_TERM_1_6,
                   DLFILE.DL_RVPRIN_RATE_1_6,
                   DLFILE.DL_RVPRIN_FRMLA_2,
                   ISNULL(DLFILE.DL_RVPRIN_CODE_2,''),
                   DLFILE.DL_RVPRIN_ADDON_2,
                   DLFILE.DL_RVPRIN_TERM_2_1,
                   DLFILE.DL_RVPRIN_RATE_2_1,
                   DLFILE.DL_RVPRIN_TERM_2_2,
                   DLFILE.DL_RVPRIN_RATE_2_2,
                   DLFILE.DL_RVPRIN_TERM_2_3,
                   DLFILE.DL_RVPRIN_RATE_2_3,
                   DLFILE.DL_RVPRIN_TERM_2_4,
                   DLFILE.DL_RVPRIN_RATE_2_4,
                   DLFILE.DL_RVPRIN_TERM_2_5,
                   DLFILE.DL_RVPRIN_RATE_2_5,
                   DLFILE.DL_RVPRIN_TERM_2_6,
                   DLFILE.DL_RVPRIN_RATE_2_6,
                   DLFILE.DL_RVPRIN_FRMLA_3,
                   ISNULL(DLFILE.DL_RVPRIN_CODE_3,''),
                   DLFILE.DL_RVPRIN_ADDON_3,
                   DLFILE.DL_RVPRIN_TERM_3_1,
                   DLFILE.DL_RVPRIN_RATE_3_1,
                   DLFILE.DL_RVPRIN_TERM_3_2,
                   DLFILE.DL_RVPRIN_RATE_3_2,
                   DLFILE.DL_RVPRIN_TERM_3_3,
                   DLFILE.DL_RVPRIN_RATE_3_3,
                   DLFILE.DL_RVPRIN_TERM_3_4,
                   DLFILE.DL_RVPRIN_RATE_3_4,
                   DLFILE.DL_RVPRIN_TERM_3_5,
                   DLFILE.DL_RVPRIN_RATE_3_5,
                   DLFILE.DL_RVPRIN_TERM_3_6,
                   DLFILE.DL_RVPRIN_RATE_3_6,
                   DLFILE.DL_RVCHGS_FRMLA_1,
                   ISNULL(DLFILE.DL_RVCHGS_CODE_1,''),
                   DLFILE.DL_RVCHGS_ADDON_1,
                   DLFILE.DL_RVCHGS_TERM_1_1,
                   DLFILE.DL_RVCHGS_RATE_1_1,
                   DLFILE.DL_RVCHGS_TERM_1_2,
                   DLFILE.DL_RVCHGS_RATE_1_2,
                   DLFILE.DL_RVCHGS_TERM_1_3,
                   DLFILE.DL_RVCHGS_RATE_1_3,
                   DLFILE.DL_RVCHGS_TERM_1_4,
                   DLFILE.DL_RVCHGS_RATE_1_4,
                   DLFILE.DL_RVCHGS_TERM_1_5,
                   DLFILE.DL_RVCHGS_RATE_1_5,
                   DLFILE.DL_RVCHGS_TERM_1_6,
                   DLFILE.DL_RVCHGS_RATE_1_6,
                   DLFILE.DL_RVCHGS_FRMLA_2,
                   ISNULL(DLFILE.DL_RVCHGS_CODE_2,''),
                   DLFILE.DL_RVCHGS_ADDON_2,
                   DLFILE.DL_RVCHGS_TERM_2_1,
                   DLFILE.DL_RVCHGS_RATE_2_1,
                   DLFILE.DL_RVCHGS_TERM_2_2,
                   DLFILE.DL_RVCHGS_RATE_2_2,
                   DLFILE.DL_RVCHGS_TERM_2_3,
                   DLFILE.DL_RVCHGS_RATE_2_3,
                   DLFILE.DL_RVCHGS_TERM_2_4,
                   DLFILE.DL_RVCHGS_RATE_2_4,
                   DLFILE.DL_RVCHGS_TERM_2_5,
                   DLFILE.DL_RVCHGS_RATE_2_5,
                   DLFILE.DL_RVCHGS_TERM_2_6,
                   DLFILE.DL_RVCHGS_RATE_2_6,
                   DLFILE.DL_RVCHGS_FRMLA_3,
                   ISNULL(DLFILE.DL_RVCHGS_CODE_3,''),
                   DLFILE.DL_RVCHGS_ADDON_3,
                   DLFILE.DL_RVCHGS_TERM_3_1,
                   DLFILE.DL_RVCHGS_RATE_3_1,
                   DLFILE.DL_RVCHGS_TERM_3_2,
                   DLFILE.DL_RVCHGS_RATE_3_2,
                   DLFILE.DL_RVCHGS_TERM_3_3,
                   DLFILE.DL_RVCHGS_RATE_3_3,
                   DLFILE.DL_RVCHGS_TERM_3_4,
                   DLFILE.DL_RVCHGS_RATE_3_4,
                   DLFILE.DL_RVCHGS_TERM_3_5,
                   DLFILE.DL_RVCHGS_RATE_3_5,
                   DLFILE.DL_RVCHGS_TERM_3_6,
                   DLFILE.DL_RVCHGS_RATE_3_6,
                   DLFILE.DL_PARVHELD_FRMLA,
                   ISNULL(DLFILE.DL_PARVHELD_CODE,''),
                   DLFILE.DL_PARVHELD_ADDON,
                   DLFILE.DL_PARVHELD_TERM_1,
                   DLFILE.DL_PARVHELD_RATE_1,
                   DLFILE.DL_PARVHELD_TERM_2,
                   DLFILE.DL_PARVHELD_RATE_2,
                   DLFILE.DL_PARVHELD_TERM_3,
                   DLFILE.DL_PARVHELD_RATE_3,
                   DLFILE.DL_PARVHELD_TERM_4,
                   DLFILE.DL_PARVHELD_RATE_4,
                   DLFILE.DL_PARVHELD_TERM_5,
                   DLFILE.DL_PARVHELD_RATE_5,
                   DLFILE.DL_PARVHELD_TERM_6,
                   DLFILE.DL_PARVHELD_RATE_6,
                   DLFILE.DL_PARVPAID_FRMLA,
                   ISNULL(DLFILE.DL_PARVPAID_CODE,''),
                   DLFILE.DL_PARVPAID_ADDON,
                   DLFILE.DL_PARVPAID_TERM_1,
                   DLFILE.DL_PARVPAID_RATE_1,
                   DLFILE.DL_PARVPAID_TERM_2,
                   DLFILE.DL_PARVPAID_RATE_2,
                   DLFILE.DL_PARVPAID_TERM_3,
                   DLFILE.DL_PARVPAID_RATE_3,
                   DLFILE.DL_PARVPAID_TERM_4,
                   DLFILE.DL_PARVPAID_RATE_4,
                   DLFILE.DL_PARVPAID_TERM_5,
                   DLFILE.DL_PARVPAID_RATE_5,
                   DLFILE.DL_PARVPAID_TERM_6,
                   DLFILE.DL_PARVPAID_RATE_6,
                   DLFILE.DL_SVACCT_FRMLA,
                   ISNULL(DLFILE.DL_SVCODE,''),
                   DLFILE.DL_SVACCT_AMT,
                   DLFILE.DL_SVACCT_RATE,
                   DLFILE.DL_SVACCT_LCHGPER,
                   DLFILE.DL_SVACCT_INTPER,
                   DLFILE.DL_SVACCT_DEFPER,
                   DLFILE.DL_FULLCEIL_BAL,
                   DLFILE.DL_PARTCEIL_BAL,
                   DLFILE.DL_NONCEIL_BAL,
                   ISNULL(DLFILE.DL_AFP_DEALER,''),
                   ISNULL(DLFILE.DL_RECOURSE_FRMLA,''),
                   DLFILE.DL_FAX_NUMBER,
                   ISNULL(DLFILE.DL_PARVPAID_FRMLA_IB,''),
                   ISNULL(DLFILE.DL_PARVPAID_CODE_IB,''),
                   ISNULL(DLFILE.DL_LPO,''),
                   ISNULL(DLFILE.DL_ACH_FG,''),
                   DLFILE.DL_BANK_ROUTING_NO,
                   ISNULL(DLFILE.DL_BANK_ACCTNO,''),
                   ISNULL(DLFILE.DL_ACH_PRENOTE_FG,''),
                   DLFILE.DL_SAL_MISC_PCT,
                   DLFILE.DL_SAL_MISC_PCT2,
                   ISNULL(DLFILE.DL_WAIVE_ACH_FEE,''),
                   ISNULL(DLFILE.DL_TRACK_CHANGE_FLAG,''),
                   ISNULL(DLFILE.DL_DBA_NAME,'')
            FROM DBO.DLFILE
            WHERE DLFILE.DL_BRNO >= :QDL1-WBEG-BRNO
              AND DLFILE.DL_BRNO <= :QDL1-WEND-BRNO
              AND DLFILE.DL_NUM >= :QDL1-WBEG-NUM
              AND DLFILE.DL_NUM <= :QDL1-WEND-NUM
            ORDER BY DLFILE.DL_BRNO,
                     DLFILE.DL_NUM
           END-EXEC.

           EXEC SQL
               OPEN DL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QDL1-WBEG-BRNO 
                          QDL1-WEND-BRNO
                          QDL1-WBEG-NUM
                          QDL1-WEND-NUM.

           MOVE STAT---GOOD TO DL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-DL1-FILE.
           PERFORM READ-IT.
           MOVE DL-PATH-OWNBR  TO DL-BRNO. 
           MOVE DL-SELECT-PATH TO E-FILE.
           MOVE DL1-KEY        TO E-KEYX.

           MOVE DL-BRNO   TO QDL-BRNO.
           MOVE DL-NUM   TO QDL-NUM.

           EXEC SQL
            SELECT DLFILE.DL_BRNO,
                   DLFILE.DL_NUM,
                   ISNULL(CAST(DLFILE.DL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(DLFILE.DL_NAME,''),
                   ISNULL(DLFILE.DL_ADR1,''),
                   ISNULL(DLFILE.DL_ADR2,''),
                   ISNULL(DLFILE.DL_CITY,''),
                   ISNULL(DLFILE.DL_STATE,''),
                   ISNULL(DLFILE.DL_ZIP,''),
                   ISNULL(DLFILE.DL_SHORT,''),
                   DLFILE.DL_PHONE,
                   ISNULL(DLFILE.DL_PURPOSE,''),
                   ISNULL(DLFILE.DL_MFGCODE,''),
                   ISNULL(CAST(DLFILE.DL_ORIGDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(DLFILE.DL_TERMDATE AS VARCHAR(10)),'0'),
                   DLFILE.DL_LC_CODE,
                   DLFILE.DL_FULLCEIL,
                   DLFILE.DL_PARTCEIL,
                   DLFILE.DL_NONCEIL,
                   ISNULL(DLFILE.DL_SIGN1,''),
                   ISNULL(DLFILE.DL_SIGN2,''),
                   ISNULL(DLFILE.DL_SIGN3,''),
                   DLFILE.DL_MAXPURTRM,
                   DLFILE.DL_MAXPURPER,
                   DLFILE.DL_EARNCD,
                   DLFILE.DL_EARNFAC,
                   DLFILE.DL_DISC_FRMLA,
                   ISNULL(DLFILE.DL_DISC_CODE,''),
                   DLFILE.DL_DISC_TERM_1,
                   DLFILE.DL_DISC_RATE_1,
                   DLFILE.DL_DISC_TERM_2,
                   DLFILE.DL_DISC_RATE_2,
                   DLFILE.DL_DISC_TERM_3,
                   DLFILE.DL_DISC_RATE_3,
                   DLFILE.DL_DISC_TERM_4,
                   DLFILE.DL_DISC_RATE_4,
                   DLFILE.DL_DISC_TERM_5,
                   DLFILE.DL_DISC_RATE_5,
                   DLFILE.DL_DISC_TERM_6,
                   DLFILE.DL_DISC_RATE_6,
                   DLFILE.DL_RVPRIN_FRMLA_1,
                   ISNULL(DLFILE.DL_RVPRIN_CODE_1,''),
                   DLFILE.DL_RVPRIN_ADDON_1,
                   DLFILE.DL_RVPRIN_TERM_1_1,
                   DLFILE.DL_RVPRIN_RATE_1_1,
                   DLFILE.DL_RVPRIN_TERM_1_2,
                   DLFILE.DL_RVPRIN_RATE_1_2,
                   DLFILE.DL_RVPRIN_TERM_1_3,
                   DLFILE.DL_RVPRIN_RATE_1_3,
                   DLFILE.DL_RVPRIN_TERM_1_4,
                   DLFILE.DL_RVPRIN_RATE_1_4,
                   DLFILE.DL_RVPRIN_TERM_1_5,
                   DLFILE.DL_RVPRIN_RATE_1_5,
                   DLFILE.DL_RVPRIN_TERM_1_6,
                   DLFILE.DL_RVPRIN_RATE_1_6,
                   DLFILE.DL_RVPRIN_FRMLA_2,
                   ISNULL(DLFILE.DL_RVPRIN_CODE_2,''),
                   DLFILE.DL_RVPRIN_ADDON_2,
                   DLFILE.DL_RVPRIN_TERM_2_1,
                   DLFILE.DL_RVPRIN_RATE_2_1,
                   DLFILE.DL_RVPRIN_TERM_2_2,
                   DLFILE.DL_RVPRIN_RATE_2_2,
                   DLFILE.DL_RVPRIN_TERM_2_3,
                   DLFILE.DL_RVPRIN_RATE_2_3,
                   DLFILE.DL_RVPRIN_TERM_2_4,
                   DLFILE.DL_RVPRIN_RATE_2_4,
                   DLFILE.DL_RVPRIN_TERM_2_5,
                   DLFILE.DL_RVPRIN_RATE_2_5,
                   DLFILE.DL_RVPRIN_TERM_2_6,
                   DLFILE.DL_RVPRIN_RATE_2_6,
                   DLFILE.DL_RVPRIN_FRMLA_3,
                   ISNULL(DLFILE.DL_RVPRIN_CODE_3,''),
                   DLFILE.DL_RVPRIN_ADDON_3,
                   DLFILE.DL_RVPRIN_TERM_3_1,
                   DLFILE.DL_RVPRIN_RATE_3_1,
                   DLFILE.DL_RVPRIN_TERM_3_2,
                   DLFILE.DL_RVPRIN_RATE_3_2,
                   DLFILE.DL_RVPRIN_TERM_3_3,
                   DLFILE.DL_RVPRIN_RATE_3_3,
                   DLFILE.DL_RVPRIN_TERM_3_4,
                   DLFILE.DL_RVPRIN_RATE_3_4,
                   DLFILE.DL_RVPRIN_TERM_3_5,
                   DLFILE.DL_RVPRIN_RATE_3_5,
                   DLFILE.DL_RVPRIN_TERM_3_6,
                   DLFILE.DL_RVPRIN_RATE_3_6,
                   DLFILE.DL_RVCHGS_FRMLA_1,
                   ISNULL(DLFILE.DL_RVCHGS_CODE_1,''),
                   DLFILE.DL_RVCHGS_ADDON_1,
                   DLFILE.DL_RVCHGS_TERM_1_1,
                   DLFILE.DL_RVCHGS_RATE_1_1,
                   DLFILE.DL_RVCHGS_TERM_1_2,
                   DLFILE.DL_RVCHGS_RATE_1_2,
                   DLFILE.DL_RVCHGS_TERM_1_3,
                   DLFILE.DL_RVCHGS_RATE_1_3,
                   DLFILE.DL_RVCHGS_TERM_1_4,
                   DLFILE.DL_RVCHGS_RATE_1_4,
                   DLFILE.DL_RVCHGS_TERM_1_5,
                   DLFILE.DL_RVCHGS_RATE_1_5,
                   DLFILE.DL_RVCHGS_TERM_1_6,
                   DLFILE.DL_RVCHGS_RATE_1_6,
                   DLFILE.DL_RVCHGS_FRMLA_2,
                   ISNULL(DLFILE.DL_RVCHGS_CODE_2,''),
                   DLFILE.DL_RVCHGS_ADDON_2,
                   DLFILE.DL_RVCHGS_TERM_2_1,
                   DLFILE.DL_RVCHGS_RATE_2_1,
                   DLFILE.DL_RVCHGS_TERM_2_2,
                   DLFILE.DL_RVCHGS_RATE_2_2,
                   DLFILE.DL_RVCHGS_TERM_2_3,
                   DLFILE.DL_RVCHGS_RATE_2_3,
                   DLFILE.DL_RVCHGS_TERM_2_4,
                   DLFILE.DL_RVCHGS_RATE_2_4,
                   DLFILE.DL_RVCHGS_TERM_2_5,
                   DLFILE.DL_RVCHGS_RATE_2_5,
                   DLFILE.DL_RVCHGS_TERM_2_6,
                   DLFILE.DL_RVCHGS_RATE_2_6,
                   DLFILE.DL_RVCHGS_FRMLA_3,
                   ISNULL(DLFILE.DL_RVCHGS_CODE_3,''),
                   DLFILE.DL_RVCHGS_ADDON_3,
                   DLFILE.DL_RVCHGS_TERM_3_1,
                   DLFILE.DL_RVCHGS_RATE_3_1,
                   DLFILE.DL_RVCHGS_TERM_3_2,
                   DLFILE.DL_RVCHGS_RATE_3_2,
                   DLFILE.DL_RVCHGS_TERM_3_3,
                   DLFILE.DL_RVCHGS_RATE_3_3,
                   DLFILE.DL_RVCHGS_TERM_3_4,
                   DLFILE.DL_RVCHGS_RATE_3_4,
                   DLFILE.DL_RVCHGS_TERM_3_5,
                   DLFILE.DL_RVCHGS_RATE_3_5,
                   DLFILE.DL_RVCHGS_TERM_3_6,
                   DLFILE.DL_RVCHGS_RATE_3_6,
                   DLFILE.DL_PARVHELD_FRMLA,
                   ISNULL(DLFILE.DL_PARVHELD_CODE,''),
                   DLFILE.DL_PARVHELD_ADDON,
                   DLFILE.DL_PARVHELD_TERM_1,
                   DLFILE.DL_PARVHELD_RATE_1,
                   DLFILE.DL_PARVHELD_TERM_2,
                   DLFILE.DL_PARVHELD_RATE_2,
                   DLFILE.DL_PARVHELD_TERM_3,
                   DLFILE.DL_PARVHELD_RATE_3,
                   DLFILE.DL_PARVHELD_TERM_4,
                   DLFILE.DL_PARVHELD_RATE_4,
                   DLFILE.DL_PARVHELD_TERM_5,
                   DLFILE.DL_PARVHELD_RATE_5,
                   DLFILE.DL_PARVHELD_TERM_6,
                   DLFILE.DL_PARVHELD_RATE_6,
                   DLFILE.DL_PARVPAID_FRMLA,
                   ISNULL(DLFILE.DL_PARVPAID_CODE,''),
                   DLFILE.DL_PARVPAID_ADDON,
                   DLFILE.DL_PARVPAID_TERM_1,
                   DLFILE.DL_PARVPAID_RATE_1,
                   DLFILE.DL_PARVPAID_TERM_2,
                   DLFILE.DL_PARVPAID_RATE_2,
                   DLFILE.DL_PARVPAID_TERM_3,
                   DLFILE.DL_PARVPAID_RATE_3,
                   DLFILE.DL_PARVPAID_TERM_4,
                   DLFILE.DL_PARVPAID_RATE_4,
                   DLFILE.DL_PARVPAID_TERM_5,
                   DLFILE.DL_PARVPAID_RATE_5,
                   DLFILE.DL_PARVPAID_TERM_6,
                   DLFILE.DL_PARVPAID_RATE_6,
                   DLFILE.DL_SVACCT_FRMLA,
                   ISNULL(DLFILE.DL_SVCODE,''),
                   DLFILE.DL_SVACCT_AMT,
                   DLFILE.DL_SVACCT_RATE,
                   DLFILE.DL_SVACCT_LCHGPER,
                   DLFILE.DL_SVACCT_INTPER,
                   DLFILE.DL_SVACCT_DEFPER,
                   DLFILE.DL_FULLCEIL_BAL,
                   DLFILE.DL_PARTCEIL_BAL,
                   DLFILE.DL_NONCEIL_BAL,
                   ISNULL(DLFILE.DL_AFP_DEALER,''),
                   ISNULL(DLFILE.DL_RECOURSE_FRMLA,''),
                   DLFILE.DL_FAX_NUMBER,
                   ISNULL(DLFILE.DL_PARVPAID_FRMLA_IB,''),
                   ISNULL(DLFILE.DL_PARVPAID_CODE_IB,''),
                   ISNULL(DLFILE.DL_LPO,''),
                   ISNULL(DLFILE.DL_ACH_FG,''),
                   DLFILE.DL_BANK_ROUTING_NO,
                   ISNULL(DLFILE.DL_BANK_ACCTNO,''),
                   ISNULL(DLFILE.DL_ACH_PRENOTE_FG,''),
                   DLFILE.DL_SAL_MISC_PCT,
                   DLFILE.DL_SAL_MISC_PCT2,
                   ISNULL(DLFILE.DL_WAIVE_ACH_FEE,''),
                   ISNULL(DLFILE.DL_TRACK_CHANGE_FLAG,''),
                   ISNULL(DLFILE.DL_DBA_NAME,'')
            INTO :QDL-REC
            FROM DBO.DLFILE
            WHERE DLFILE.DL_BRNO = :QDL-BRNO
              AND DLFILE.DL_NUM = :QDL-NUM
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-DL-FIELDS.

      *-----------------------------------------------------------------
       READ-DL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE DL-PATH-OWNBR  TO DL-BRNO.
           MOVE DL-SELECT-PATH TO E-FILE.
           MOVE DL1-KEY        TO E-KEYX.
           INITIALIZE QDL-REC.

           IF ( DL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH DL1_CURSOR INTO :QDL-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-DL-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-DL1-FILE.

           IF ( DL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE DL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO DL1-CURSOR-STAT.


      *-----------------------------------------------------------------
       LOAD-DL2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( DL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( DL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO DL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO DL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO DL-ALL-DATA
              MOVE DL-ALL-PATH             TO DL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( DL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO DL-ALL-BASE
              MOVE FILPATH-MACHINE         TO DL-ALL-MACHINE
              MOVE FILPATH-DATA            TO DL-ALL-DATA
              MOVE DL-ALL-PATH             TO DL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-DLFILE NOT = "S" )
              MOVE DL-PATH-BEG             TO DL-SELECT-PATH-BEG
              MOVE DL-PATH-FILE            TO DL-SELECT-PATH-FILE
           ELSE
              MOVE DL-PATH-SQL             TO DL-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-DL2-FILE.
           PERFORM LOAD-DL2-FILE.
           PERFORM OPEN-IT.
           MOVE DL-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-DL2-FILE.
           PERFORM START-IT.
           MOVE DL-PATH-OWNBR  TO DL-BRNO.
           MOVE DL-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING DL-BRNO, DL-SHORT DELIMITED BY SIZE INTO E-KEYX.

           IF ( DL2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-DL2-FILE.

           IF ( QDL2-WEND-SHORT = SPACES )
              MOVE DL-PATH-OWNBR        TO QDL2-WBEG-BRNO
              MOVE DL-PATH-OWNBR        TO QDL2-WEND-BRNO
              MOVE DL-SHORT             TO QDL2-WBEG-SHORT
              MOVE ALL "Z"              TO QDL2-WEND-SHORT.

           EXEC SQL
            DECLARE DL2_CURSOR CURSOR FOR
            SELECT DLFILE.DL_BRNO,
                   DLFILE.DL_NUM,
                   ISNULL(CAST(DLFILE.DL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(DLFILE.DL_NAME,''),
                   ISNULL(DLFILE.DL_ADR1,''),
                   ISNULL(DLFILE.DL_ADR2,''),
                   ISNULL(DLFILE.DL_CITY,''),
                   ISNULL(DLFILE.DL_STATE,''),
                   ISNULL(DLFILE.DL_ZIP,''),
                   ISNULL(DLFILE.DL_SHORT,''),
                   DLFILE.DL_PHONE,
                   ISNULL(DLFILE.DL_PURPOSE,''),
                   ISNULL(DLFILE.DL_MFGCODE,''),
                   ISNULL(CAST(DLFILE.DL_ORIGDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(DLFILE.DL_TERMDATE AS VARCHAR(10)),'0'),
                   DLFILE.DL_LC_CODE,
                   DLFILE.DL_FULLCEIL,
                   DLFILE.DL_PARTCEIL,
                   DLFILE.DL_NONCEIL,
                   ISNULL(DLFILE.DL_SIGN1,''),
                   ISNULL(DLFILE.DL_SIGN2,''),
                   ISNULL(DLFILE.DL_SIGN3,''),
                   DLFILE.DL_MAXPURTRM,
                   DLFILE.DL_MAXPURPER,
                   DLFILE.DL_EARNCD,
                   DLFILE.DL_EARNFAC,
                   DLFILE.DL_DISC_FRMLA,
                   ISNULL(DLFILE.DL_DISC_CODE,''),
                   DLFILE.DL_DISC_TERM_1,
                   DLFILE.DL_DISC_RATE_1,
                   DLFILE.DL_DISC_TERM_2,
                   DLFILE.DL_DISC_RATE_2,
                   DLFILE.DL_DISC_TERM_3,
                   DLFILE.DL_DISC_RATE_3,
                   DLFILE.DL_DISC_TERM_4,
                   DLFILE.DL_DISC_RATE_4,
                   DLFILE.DL_DISC_TERM_5,
                   DLFILE.DL_DISC_RATE_5,
                   DLFILE.DL_DISC_TERM_6,
                   DLFILE.DL_DISC_RATE_6,
                   DLFILE.DL_RVPRIN_FRMLA_1,
                   ISNULL(DLFILE.DL_RVPRIN_CODE_1,''),
                   DLFILE.DL_RVPRIN_ADDON_1,
                   DLFILE.DL_RVPRIN_TERM_1_1,
                   DLFILE.DL_RVPRIN_RATE_1_1,
                   DLFILE.DL_RVPRIN_TERM_1_2,
                   DLFILE.DL_RVPRIN_RATE_1_2,
                   DLFILE.DL_RVPRIN_TERM_1_3,
                   DLFILE.DL_RVPRIN_RATE_1_3,
                   DLFILE.DL_RVPRIN_TERM_1_4,
                   DLFILE.DL_RVPRIN_RATE_1_4,
                   DLFILE.DL_RVPRIN_TERM_1_5,
                   DLFILE.DL_RVPRIN_RATE_1_5,
                   DLFILE.DL_RVPRIN_TERM_1_6,
                   DLFILE.DL_RVPRIN_RATE_1_6,
                   DLFILE.DL_RVPRIN_FRMLA_2,
                   ISNULL(DLFILE.DL_RVPRIN_CODE_2,''),
                   DLFILE.DL_RVPRIN_ADDON_2,
                   DLFILE.DL_RVPRIN_TERM_2_1,
                   DLFILE.DL_RVPRIN_RATE_2_1,
                   DLFILE.DL_RVPRIN_TERM_2_2,
                   DLFILE.DL_RVPRIN_RATE_2_2,
                   DLFILE.DL_RVPRIN_TERM_2_3,
                   DLFILE.DL_RVPRIN_RATE_2_3,
                   DLFILE.DL_RVPRIN_TERM_2_4,
                   DLFILE.DL_RVPRIN_RATE_2_4,
                   DLFILE.DL_RVPRIN_TERM_2_5,
                   DLFILE.DL_RVPRIN_RATE_2_5,
                   DLFILE.DL_RVPRIN_TERM_2_6,
                   DLFILE.DL_RVPRIN_RATE_2_6,
                   DLFILE.DL_RVPRIN_FRMLA_3,
                   ISNULL(DLFILE.DL_RVPRIN_CODE_3,''),
                   DLFILE.DL_RVPRIN_ADDON_3,
                   DLFILE.DL_RVPRIN_TERM_3_1,
                   DLFILE.DL_RVPRIN_RATE_3_1,
                   DLFILE.DL_RVPRIN_TERM_3_2,
                   DLFILE.DL_RVPRIN_RATE_3_2,
                   DLFILE.DL_RVPRIN_TERM_3_3,
                   DLFILE.DL_RVPRIN_RATE_3_3,
                   DLFILE.DL_RVPRIN_TERM_3_4,
                   DLFILE.DL_RVPRIN_RATE_3_4,
                   DLFILE.DL_RVPRIN_TERM_3_5,
                   DLFILE.DL_RVPRIN_RATE_3_5,
                   DLFILE.DL_RVPRIN_TERM_3_6,
                   DLFILE.DL_RVPRIN_RATE_3_6,
                   DLFILE.DL_RVCHGS_FRMLA_1,
                   ISNULL(DLFILE.DL_RVCHGS_CODE_1,''),
                   DLFILE.DL_RVCHGS_ADDON_1,
                   DLFILE.DL_RVCHGS_TERM_1_1,
                   DLFILE.DL_RVCHGS_RATE_1_1,
                   DLFILE.DL_RVCHGS_TERM_1_2,
                   DLFILE.DL_RVCHGS_RATE_1_2,
                   DLFILE.DL_RVCHGS_TERM_1_3,
                   DLFILE.DL_RVCHGS_RATE_1_3,
                   DLFILE.DL_RVCHGS_TERM_1_4,
                   DLFILE.DL_RVCHGS_RATE_1_4,
                   DLFILE.DL_RVCHGS_TERM_1_5,
                   DLFILE.DL_RVCHGS_RATE_1_5,
                   DLFILE.DL_RVCHGS_TERM_1_6,
                   DLFILE.DL_RVCHGS_RATE_1_6,
                   DLFILE.DL_RVCHGS_FRMLA_2,
                   ISNULL(DLFILE.DL_RVCHGS_CODE_2,''),
                   DLFILE.DL_RVCHGS_ADDON_2,
                   DLFILE.DL_RVCHGS_TERM_2_1,
                   DLFILE.DL_RVCHGS_RATE_2_1,
                   DLFILE.DL_RVCHGS_TERM_2_2,
                   DLFILE.DL_RVCHGS_RATE_2_2,
                   DLFILE.DL_RVCHGS_TERM_2_3,
                   DLFILE.DL_RVCHGS_RATE_2_3,
                   DLFILE.DL_RVCHGS_TERM_2_4,
                   DLFILE.DL_RVCHGS_RATE_2_4,
                   DLFILE.DL_RVCHGS_TERM_2_5,
                   DLFILE.DL_RVCHGS_RATE_2_5,
                   DLFILE.DL_RVCHGS_TERM_2_6,
                   DLFILE.DL_RVCHGS_RATE_2_6,
                   DLFILE.DL_RVCHGS_FRMLA_3,
                   ISNULL(DLFILE.DL_RVCHGS_CODE_3,''),
                   DLFILE.DL_RVCHGS_ADDON_3,
                   DLFILE.DL_RVCHGS_TERM_3_1,
                   DLFILE.DL_RVCHGS_RATE_3_1,
                   DLFILE.DL_RVCHGS_TERM_3_2,
                   DLFILE.DL_RVCHGS_RATE_3_2,
                   DLFILE.DL_RVCHGS_TERM_3_3,
                   DLFILE.DL_RVCHGS_RATE_3_3,
                   DLFILE.DL_RVCHGS_TERM_3_4,
                   DLFILE.DL_RVCHGS_RATE_3_4,
                   DLFILE.DL_RVCHGS_TERM_3_5,
                   DLFILE.DL_RVCHGS_RATE_3_5,
                   DLFILE.DL_RVCHGS_TERM_3_6,
                   DLFILE.DL_RVCHGS_RATE_3_6,
                   DLFILE.DL_PARVHELD_FRMLA,
                   ISNULL(DLFILE.DL_PARVHELD_CODE,''),
                   DLFILE.DL_PARVHELD_ADDON,
                   DLFILE.DL_PARVHELD_TERM_1,
                   DLFILE.DL_PARVHELD_RATE_1,
                   DLFILE.DL_PARVHELD_TERM_2,
                   DLFILE.DL_PARVHELD_RATE_2,
                   DLFILE.DL_PARVHELD_TERM_3,
                   DLFILE.DL_PARVHELD_RATE_3,
                   DLFILE.DL_PARVHELD_TERM_4,
                   DLFILE.DL_PARVHELD_RATE_4,
                   DLFILE.DL_PARVHELD_TERM_5,
                   DLFILE.DL_PARVHELD_RATE_5,
                   DLFILE.DL_PARVHELD_TERM_6,
                   DLFILE.DL_PARVHELD_RATE_6,
                   DLFILE.DL_PARVPAID_FRMLA,
                   ISNULL(DLFILE.DL_PARVPAID_CODE,''),
                   DLFILE.DL_PARVPAID_ADDON,
                   DLFILE.DL_PARVPAID_TERM_1,
                   DLFILE.DL_PARVPAID_RATE_1,
                   DLFILE.DL_PARVPAID_TERM_2,
                   DLFILE.DL_PARVPAID_RATE_2,
                   DLFILE.DL_PARVPAID_TERM_3,
                   DLFILE.DL_PARVPAID_RATE_3,
                   DLFILE.DL_PARVPAID_TERM_4,
                   DLFILE.DL_PARVPAID_RATE_4,
                   DLFILE.DL_PARVPAID_TERM_5,
                   DLFILE.DL_PARVPAID_RATE_5,
                   DLFILE.DL_PARVPAID_TERM_6,
                   DLFILE.DL_PARVPAID_RATE_6,
                   DLFILE.DL_SVACCT_FRMLA,
                   ISNULL(DLFILE.DL_SVCODE,''),
                   DLFILE.DL_SVACCT_AMT,
                   DLFILE.DL_SVACCT_RATE,
                   DLFILE.DL_SVACCT_LCHGPER,
                   DLFILE.DL_SVACCT_INTPER,
                   DLFILE.DL_SVACCT_DEFPER,
                   DLFILE.DL_FULLCEIL_BAL,
                   DLFILE.DL_PARTCEIL_BAL,
                   DLFILE.DL_NONCEIL_BAL,
                   ISNULL(DLFILE.DL_AFP_DEALER,''),
                   ISNULL(DLFILE.DL_RECOURSE_FRMLA,''),
                   DLFILE.DL_FAX_NUMBER,
                   ISNULL(DLFILE.DL_PARVPAID_FRMLA_IB,''),
                   ISNULL(DLFILE.DL_PARVPAID_CODE_IB,''),
                   ISNULL(DLFILE.DL_LPO,''),
                   ISNULL(DLFILE.DL_ACH_FG,''),
                   DLFILE.DL_BANK_ROUTING_NO,
                   ISNULL(DLFILE.DL_BANK_ACCTNO,''),
                   ISNULL(DLFILE.DL_ACH_PRENOTE_FG,''),
                   DLFILE.DL_SAL_MISC_PCT,
                   DLFILE.DL_SAL_MISC_PCT2,
                   ISNULL(DLFILE.DL_WAIVE_ACH_FEE,''),
                   ISNULL(DLFILE.DL_TRACK_CHANGE_FLAG,''),
                   ISNULL(DLFILE.DL_DBA_NAME,'')
            FROM DBO.DLFILE
            WHERE DLFILE.DL_BRNO >= :QDL2-WBEG-BRNO
              AND DLFILE.DL_BRNO <= :QDL2-WEND-BRNO
              AND DLFILE.DL_SHORT >= :QDL2-WBEG-SHORT
              AND DLFILE.DL_SHORT <= :QDL2-WEND-SHORT
            ORDER BY DLFILE.DL_BRNO,
                     DLFILE.DL_SHORT,
                     DLFILE.DL_NUM
           END-EXEC.

           EXEC SQL
               OPEN DL2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QDL2-WBEG-BRNO 
                          QDL2-WEND-BRNO.
           MOVE SPACES TO QDL2-WBEG-SHORT
                          QDL2-WEND-SHORT.

           MOVE STAT---GOOD TO DL2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-DL2-FILE.
           PERFORM READ-IT.
           MOVE DL-PATH-OWNBR  TO DL-BRNO. 
           MOVE DL-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING DL-BRNO, DL-SHORT DELIMITED BY SIZE INTO E-KEYX.

           MOVE DL-BRNO   TO QDL-BRNO.
           MOVE DL-SHORT   TO QDL-SHORT.

           EXEC SQL
            SELECT DLFILE.DL_BRNO,
                   DLFILE.DL_NUM,
                   ISNULL(CAST(DLFILE.DL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(DLFILE.DL_NAME,''),
                   ISNULL(DLFILE.DL_ADR1,''),
                   ISNULL(DLFILE.DL_ADR2,''),
                   ISNULL(DLFILE.DL_CITY,''),
                   ISNULL(DLFILE.DL_STATE,''),
                   ISNULL(DLFILE.DL_ZIP,''),
                   ISNULL(DLFILE.DL_SHORT,''),
                   DLFILE.DL_PHONE,
                   ISNULL(DLFILE.DL_PURPOSE,''),
                   ISNULL(DLFILE.DL_MFGCODE,''),
                   ISNULL(CAST(DLFILE.DL_ORIGDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(DLFILE.DL_TERMDATE AS VARCHAR(10)),'0'),
                   DLFILE.DL_LC_CODE,
                   DLFILE.DL_FULLCEIL,
                   DLFILE.DL_PARTCEIL,
                   DLFILE.DL_NONCEIL,
                   ISNULL(DLFILE.DL_SIGN1,''),
                   ISNULL(DLFILE.DL_SIGN2,''),
                   ISNULL(DLFILE.DL_SIGN3,''),
                   DLFILE.DL_MAXPURTRM,
                   DLFILE.DL_MAXPURPER,
                   DLFILE.DL_EARNCD,
                   DLFILE.DL_EARNFAC,
                   DLFILE.DL_DISC_FRMLA,
                   ISNULL(DLFILE.DL_DISC_CODE,''),
                   DLFILE.DL_DISC_TERM_1,
                   DLFILE.DL_DISC_RATE_1,
                   DLFILE.DL_DISC_TERM_2,
                   DLFILE.DL_DISC_RATE_2,
                   DLFILE.DL_DISC_TERM_3,
                   DLFILE.DL_DISC_RATE_3,
                   DLFILE.DL_DISC_TERM_4,
                   DLFILE.DL_DISC_RATE_4,
                   DLFILE.DL_DISC_TERM_5,
                   DLFILE.DL_DISC_RATE_5,
                   DLFILE.DL_DISC_TERM_6,
                   DLFILE.DL_DISC_RATE_6,
                   DLFILE.DL_RVPRIN_FRMLA_1,
                   ISNULL(DLFILE.DL_RVPRIN_CODE_1,''),
                   DLFILE.DL_RVPRIN_ADDON_1,
                   DLFILE.DL_RVPRIN_TERM_1_1,
                   DLFILE.DL_RVPRIN_RATE_1_1,
                   DLFILE.DL_RVPRIN_TERM_1_2,
                   DLFILE.DL_RVPRIN_RATE_1_2,
                   DLFILE.DL_RVPRIN_TERM_1_3,
                   DLFILE.DL_RVPRIN_RATE_1_3,
                   DLFILE.DL_RVPRIN_TERM_1_4,
                   DLFILE.DL_RVPRIN_RATE_1_4,
                   DLFILE.DL_RVPRIN_TERM_1_5,
                   DLFILE.DL_RVPRIN_RATE_1_5,
                   DLFILE.DL_RVPRIN_TERM_1_6,
                   DLFILE.DL_RVPRIN_RATE_1_6,
                   DLFILE.DL_RVPRIN_FRMLA_2,
                   ISNULL(DLFILE.DL_RVPRIN_CODE_2,''),
                   DLFILE.DL_RVPRIN_ADDON_2,
                   DLFILE.DL_RVPRIN_TERM_2_1,
                   DLFILE.DL_RVPRIN_RATE_2_1,
                   DLFILE.DL_RVPRIN_TERM_2_2,
                   DLFILE.DL_RVPRIN_RATE_2_2,
                   DLFILE.DL_RVPRIN_TERM_2_3,
                   DLFILE.DL_RVPRIN_RATE_2_3,
                   DLFILE.DL_RVPRIN_TERM_2_4,
                   DLFILE.DL_RVPRIN_RATE_2_4,
                   DLFILE.DL_RVPRIN_TERM_2_5,
                   DLFILE.DL_RVPRIN_RATE_2_5,
                   DLFILE.DL_RVPRIN_TERM_2_6,
                   DLFILE.DL_RVPRIN_RATE_2_6,
                   DLFILE.DL_RVPRIN_FRMLA_3,
                   ISNULL(DLFILE.DL_RVPRIN_CODE_3,''),
                   DLFILE.DL_RVPRIN_ADDON_3,
                   DLFILE.DL_RVPRIN_TERM_3_1,
                   DLFILE.DL_RVPRIN_RATE_3_1,
                   DLFILE.DL_RVPRIN_TERM_3_2,
                   DLFILE.DL_RVPRIN_RATE_3_2,
                   DLFILE.DL_RVPRIN_TERM_3_3,
                   DLFILE.DL_RVPRIN_RATE_3_3,
                   DLFILE.DL_RVPRIN_TERM_3_4,
                   DLFILE.DL_RVPRIN_RATE_3_4,
                   DLFILE.DL_RVPRIN_TERM_3_5,
                   DLFILE.DL_RVPRIN_RATE_3_5,
                   DLFILE.DL_RVPRIN_TERM_3_6,
                   DLFILE.DL_RVPRIN_RATE_3_6,
                   DLFILE.DL_RVCHGS_FRMLA_1,
                   ISNULL(DLFILE.DL_RVCHGS_CODE_1,''),
                   DLFILE.DL_RVCHGS_ADDON_1,
                   DLFILE.DL_RVCHGS_TERM_1_1,
                   DLFILE.DL_RVCHGS_RATE_1_1,
                   DLFILE.DL_RVCHGS_TERM_1_2,
                   DLFILE.DL_RVCHGS_RATE_1_2,
                   DLFILE.DL_RVCHGS_TERM_1_3,
                   DLFILE.DL_RVCHGS_RATE_1_3,
                   DLFILE.DL_RVCHGS_TERM_1_4,
                   DLFILE.DL_RVCHGS_RATE_1_4,
                   DLFILE.DL_RVCHGS_TERM_1_5,
                   DLFILE.DL_RVCHGS_RATE_1_5,
                   DLFILE.DL_RVCHGS_TERM_1_6,
                   DLFILE.DL_RVCHGS_RATE_1_6,
                   DLFILE.DL_RVCHGS_FRMLA_2,
                   ISNULL(DLFILE.DL_RVCHGS_CODE_2,''),
                   DLFILE.DL_RVCHGS_ADDON_2,
                   DLFILE.DL_RVCHGS_TERM_2_1,
                   DLFILE.DL_RVCHGS_RATE_2_1,
                   DLFILE.DL_RVCHGS_TERM_2_2,
                   DLFILE.DL_RVCHGS_RATE_2_2,
                   DLFILE.DL_RVCHGS_TERM_2_3,
                   DLFILE.DL_RVCHGS_RATE_2_3,
                   DLFILE.DL_RVCHGS_TERM_2_4,
                   DLFILE.DL_RVCHGS_RATE_2_4,
                   DLFILE.DL_RVCHGS_TERM_2_5,
                   DLFILE.DL_RVCHGS_RATE_2_5,
                   DLFILE.DL_RVCHGS_TERM_2_6,
                   DLFILE.DL_RVCHGS_RATE_2_6,
                   DLFILE.DL_RVCHGS_FRMLA_3,
                   ISNULL(DLFILE.DL_RVCHGS_CODE_3,''),
                   DLFILE.DL_RVCHGS_ADDON_3,
                   DLFILE.DL_RVCHGS_TERM_3_1,
                   DLFILE.DL_RVCHGS_RATE_3_1,
                   DLFILE.DL_RVCHGS_TERM_3_2,
                   DLFILE.DL_RVCHGS_RATE_3_2,
                   DLFILE.DL_RVCHGS_TERM_3_3,
                   DLFILE.DL_RVCHGS_RATE_3_3,
                   DLFILE.DL_RVCHGS_TERM_3_4,
                   DLFILE.DL_RVCHGS_RATE_3_4,
                   DLFILE.DL_RVCHGS_TERM_3_5,
                   DLFILE.DL_RVCHGS_RATE_3_5,
                   DLFILE.DL_RVCHGS_TERM_3_6,
                   DLFILE.DL_RVCHGS_RATE_3_6,
                   DLFILE.DL_PARVHELD_FRMLA,
                   ISNULL(DLFILE.DL_PARVHELD_CODE,''),
                   DLFILE.DL_PARVHELD_ADDON,
                   DLFILE.DL_PARVHELD_TERM_1,
                   DLFILE.DL_PARVHELD_RATE_1,
                   DLFILE.DL_PARVHELD_TERM_2,
                   DLFILE.DL_PARVHELD_RATE_2,
                   DLFILE.DL_PARVHELD_TERM_3,
                   DLFILE.DL_PARVHELD_RATE_3,
                   DLFILE.DL_PARVHELD_TERM_4,
                   DLFILE.DL_PARVHELD_RATE_4,
                   DLFILE.DL_PARVHELD_TERM_5,
                   DLFILE.DL_PARVHELD_RATE_5,
                   DLFILE.DL_PARVHELD_TERM_6,
                   DLFILE.DL_PARVHELD_RATE_6,
                   DLFILE.DL_PARVPAID_FRMLA,
                   ISNULL(DLFILE.DL_PARVPAID_CODE,''),
                   DLFILE.DL_PARVPAID_ADDON,
                   DLFILE.DL_PARVPAID_TERM_1,
                   DLFILE.DL_PARVPAID_RATE_1,
                   DLFILE.DL_PARVPAID_TERM_2,
                   DLFILE.DL_PARVPAID_RATE_2,
                   DLFILE.DL_PARVPAID_TERM_3,
                   DLFILE.DL_PARVPAID_RATE_3,
                   DLFILE.DL_PARVPAID_TERM_4,
                   DLFILE.DL_PARVPAID_RATE_4,
                   DLFILE.DL_PARVPAID_TERM_5,
                   DLFILE.DL_PARVPAID_RATE_5,
                   DLFILE.DL_PARVPAID_TERM_6,
                   DLFILE.DL_PARVPAID_RATE_6,
                   DLFILE.DL_SVACCT_FRMLA,
                   ISNULL(DLFILE.DL_SVCODE,''),
                   DLFILE.DL_SVACCT_AMT,
                   DLFILE.DL_SVACCT_RATE,
                   DLFILE.DL_SVACCT_LCHGPER,
                   DLFILE.DL_SVACCT_INTPER,
                   DLFILE.DL_SVACCT_DEFPER,
                   DLFILE.DL_FULLCEIL_BAL,
                   DLFILE.DL_PARTCEIL_BAL,
                   DLFILE.DL_NONCEIL_BAL,
                   ISNULL(DLFILE.DL_AFP_DEALER,''),
                   ISNULL(DLFILE.DL_RECOURSE_FRMLA,''),
                   DLFILE.DL_FAX_NUMBER,
                   ISNULL(DLFILE.DL_PARVPAID_FRMLA_IB,''),
                   ISNULL(DLFILE.DL_PARVPAID_CODE_IB,''),
                   ISNULL(DLFILE.DL_LPO,''),
                   ISNULL(DLFILE.DL_ACH_FG,''),
                   DLFILE.DL_BANK_ROUTING_NO,
                   ISNULL(DLFILE.DL_BANK_ACCTNO,''),
                   ISNULL(DLFILE.DL_ACH_PRENOTE_FG,''),
                   DLFILE.DL_SAL_MISC_PCT,
                   DLFILE.DL_SAL_MISC_PCT2,
                   ISNULL(DLFILE.DL_WAIVE_ACH_FEE,''),
                   ISNULL(DLFILE.DL_TRACK_CHANGE_FLAG,''),
                   ISNULL(DLFILE.DL_DBA_NAME,'')
            INTO :QDL-REC
            FROM DBO.DLFILE
            WHERE DLFILE.DL_BRNO = :QDL-BRNO
              AND DLFILE.DL_SHORT = :QDL-SHORT
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DL2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-DL-FIELDS.

      *-----------------------------------------------------------------
       READ-DL2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE DL-PATH-OWNBR  TO DL-BRNO.
           MOVE DL-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING DL-BRNO, DL-SHORT DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QDL-REC.

           IF ( DL2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH DL2_CURSOR INTO :QDL-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DL2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-DL-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-DL2-FILE.

           IF ( DL2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE DL2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO DL2-CURSOR-STAT.

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
              DESTROY IODLRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
