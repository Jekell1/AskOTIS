       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOSSIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  03-12-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOSSIN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : SSFILE 
      * TABLE DESC    : CREDIT SCORE STATISTICS FILE
      * OUTPUT FILE   : XPC/IOSSIN.PRE
      * USED FOR      : LIBLP/LPWSSS.CPY  --- QSS-REC
      *                 LIBLP/LPSS1R.CPY  --- READ
      *                 LIBLP/LPSS1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0312 ***NEW***
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
           "@(#)IO/IOSSIN.CBL S34 05/18/21-11:31:11 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01SS.CPY".
       COPY "LIBLP/LPWSSS.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S WS COPY MEMBERS
      *#################################################################
      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QSS1-WHERE-BEG-KEY.
           03  QSS1-WBEG-DATE           PIC 9(06).
           03  QSS1-WBEG-BRNO           PIC 9(04).
           03  QSS1-WBEG-CLASS          PIC 9(03).

       01  QSS1-WHERE-END-KEY.
           03  QSS1-WEND-DATE           PIC 9(06).
           03  QSS1-WEND-BRNO           PIC 9(04).
           03  QSS1-WEND-CLASS          PIC 9(03).

       01  QSS-REC.
           03  QSS-DATE-YYYYMM         PIC 9(6).
           03  QSS-BRNO                PIC 9(4).
           03  QSS-CLASS               PIC 9(3).
           03  QSS-LTOUCH-DATE         PIC X(10).
           03  QSS-UNITS-1-1-1    PIC S9(7).
           03  QSS-OUTBAL-1-1-1   PIC S9(9)V99.
           03  QSS-UNITS-1-1-2    PIC S9(7).
           03  QSS-OUTBAL-1-1-2   PIC S9(9)V99.
           03  QSS-UNITS-1-1-3    PIC S9(7).
           03  QSS-OUTBAL-1-1-3   PIC S9(9)V99.
           03  QSS-UNITS-1-1-4    PIC S9(7).
           03  QSS-OUTBAL-1-1-4   PIC S9(9)V99.
           03  QSS-UNITS-1-1-5    PIC S9(7).
           03  QSS-OUTBAL-1-1-5   PIC S9(9)V99.
           03  QSS-NO-PL-1-1-1    PIC S9(7).
           03  QSS-AMT-PL-1-1-1   PIC S9(9)V99.
           03  QSS-NO-PL-1-1-2    PIC S9(7).
           03  QSS-AMT-PL-1-1-2   PIC S9(9)V99.
           03  QSS-UNITS-1-2-1    PIC S9(7).
           03  QSS-OUTBAL-1-2-1   PIC S9(9)V99.
           03  QSS-UNITS-1-2-2    PIC S9(7).
           03  QSS-OUTBAL-1-2-2   PIC S9(9)V99.
           03  QSS-UNITS-1-2-3    PIC S9(7).
           03  QSS-OUTBAL-1-2-3   PIC S9(9)V99.
           03  QSS-UNITS-1-2-4    PIC S9(7).
           03  QSS-OUTBAL-1-2-4   PIC S9(9)V99.
           03  QSS-UNITS-1-2-5    PIC S9(7).
           03  QSS-OUTBAL-1-2-5   PIC S9(9)V99.
           03  QSS-NO-PL-1-2-1    PIC S9(7).
           03  QSS-AMT-PL-1-2-1   PIC S9(9)V99.
           03  QSS-NO-PL-1-2-2    PIC S9(7).
           03  QSS-AMT-PL-1-2-2   PIC S9(9)V99.
           03  QSS-UNITS-2-1-1    PIC S9(7).
           03  QSS-OUTBAL-2-1-1   PIC S9(9)V99.
           03  QSS-UNITS-2-1-2    PIC S9(7).
           03  QSS-OUTBAL-2-1-2   PIC S9(9)V99.
           03  QSS-UNITS-2-1-3    PIC S9(7).
           03  QSS-OUTBAL-2-1-3   PIC S9(9)V99.
           03  QSS-UNITS-2-1-4    PIC S9(7).
           03  QSS-OUTBAL-2-1-4   PIC S9(9)V99.
           03  QSS-UNITS-2-1-5    PIC S9(7).
           03  QSS-OUTBAL-2-1-5   PIC S9(9)V99.
           03  QSS-NO-PL-2-1-1    PIC S9(7).
           03  QSS-AMT-PL-2-1-1   PIC S9(9)V99.
           03  QSS-NO-PL-2-1-2    PIC S9(7).
           03  QSS-AMT-PL-2-1-2   PIC S9(9)V99.
           03  QSS-UNITS-2-2-1    PIC S9(7).
           03  QSS-OUTBAL-2-2-1   PIC S9(9)V99.
           03  QSS-UNITS-2-2-2    PIC S9(7).
           03  QSS-OUTBAL-2-2-2   PIC S9(9)V99.
           03  QSS-UNITS-2-2-3    PIC S9(7).
           03  QSS-OUTBAL-2-2-3   PIC S9(9)V99.
           03  QSS-UNITS-2-2-4    PIC S9(7).
           03  QSS-OUTBAL-2-2-4   PIC S9(9)V99.
           03  QSS-UNITS-2-2-5    PIC S9(7).
           03  QSS-OUTBAL-2-2-5   PIC S9(9)V99.
           03  QSS-NO-PL-2-2-1    PIC S9(7).
           03  QSS-AMT-PL-2-2-1   PIC S9(9)V99.
           03  QSS-NO-PL-2-2-2    PIC S9(7).
           03  QSS-AMT-PL-2-2-2   PIC S9(9)V99.
           03  QSS-UNITS-3-1-1    PIC S9(7).
           03  QSS-OUTBAL-3-1-1   PIC S9(9)V99.
           03  QSS-UNITS-3-1-2    PIC S9(7).
           03  QSS-OUTBAL-3-1-2   PIC S9(9)V99.
           03  QSS-UNITS-3-1-3    PIC S9(7).
           03  QSS-OUTBAL-3-1-3   PIC S9(9)V99.
           03  QSS-UNITS-3-1-4    PIC S9(7).
           03  QSS-OUTBAL-3-1-4   PIC S9(9)V99.
           03  QSS-UNITS-3-1-5    PIC S9(7).
           03  QSS-OUTBAL-3-1-5   PIC S9(9)V99.
           03  QSS-NO-PL-3-1-1    PIC S9(7).
           03  QSS-AMT-PL-3-1-1   PIC S9(9)V99.
           03  QSS-NO-PL-3-1-2    PIC S9(7).
           03  QSS-AMT-PL-3-1-2   PIC S9(9)V99.
           03  QSS-UNITS-3-2-1    PIC S9(7).
           03  QSS-OUTBAL-3-2-1   PIC S9(9)V99.
           03  QSS-UNITS-3-2-2    PIC S9(7).
           03  QSS-OUTBAL-3-2-2   PIC S9(9)V99.
           03  QSS-UNITS-3-2-3    PIC S9(7).
           03  QSS-OUTBAL-3-2-3   PIC S9(9)V99.
           03  QSS-UNITS-3-2-4    PIC S9(7).
           03  QSS-OUTBAL-3-2-4   PIC S9(9)V99.
           03  QSS-UNITS-3-2-5    PIC S9(7).
           03  QSS-OUTBAL-3-2-5   PIC S9(9)V99.
           03  QSS-NO-PL-3-2-1    PIC S9(7).
           03  QSS-AMT-PL-3-2-1   PIC S9(9)V99.
           03  QSS-NO-PL-3-2-2    PIC S9(7).
           03  QSS-AMT-PL-3-2-2   PIC S9(9)V99.
           03  QSS-UNITS-4-1-1    PIC S9(7).
           03  QSS-OUTBAL-4-1-1   PIC S9(9)V99.
           03  QSS-UNITS-4-1-2    PIC S9(7).
           03  QSS-OUTBAL-4-1-2   PIC S9(9)V99.
           03  QSS-UNITS-4-1-3    PIC S9(7).
           03  QSS-OUTBAL-4-1-3   PIC S9(9)V99.
           03  QSS-UNITS-4-1-4    PIC S9(7).
           03  QSS-OUTBAL-4-1-4   PIC S9(9)V99.
           03  QSS-UNITS-4-1-5    PIC S9(7).
           03  QSS-OUTBAL-4-1-5   PIC S9(9)V99.
           03  QSS-NO-PL-4-1-1    PIC S9(7).
           03  QSS-AMT-PL-4-1-1   PIC S9(9)V99.
           03  QSS-NO-PL-4-1-2    PIC S9(7).
           03  QSS-AMT-PL-4-1-2   PIC S9(9)V99.
           03  QSS-UNITS-4-2-1    PIC S9(7).
           03  QSS-OUTBAL-4-2-1   PIC S9(9)V99.
           03  QSS-UNITS-4-2-2    PIC S9(7).
           03  QSS-OUTBAL-4-2-2   PIC S9(9)V99.
           03  QSS-UNITS-4-2-3    PIC S9(7).
           03  QSS-OUTBAL-4-2-3   PIC S9(9)V99.
           03  QSS-UNITS-4-2-4    PIC S9(7).
           03  QSS-OUTBAL-4-2-4   PIC S9(9)V99.
           03  QSS-UNITS-4-2-5    PIC S9(7).
           03  QSS-OUTBAL-4-2-5   PIC S9(9)V99.
           03  QSS-NO-PL-4-2-1    PIC S9(7).
           03  QSS-AMT-PL-4-2-1   PIC S9(9)V99.
           03  QSS-NO-PL-4-2-2    PIC S9(7).
           03  QSS-AMT-PL-4-2-2   PIC S9(9)V99.
           03  QSS-UNITS-5-1-1    PIC S9(7).
           03  QSS-OUTBAL-5-1-1   PIC S9(9)V99.
           03  QSS-UNITS-5-1-2    PIC S9(7).
           03  QSS-OUTBAL-5-1-2   PIC S9(9)V99.
           03  QSS-UNITS-5-1-3    PIC S9(7).
           03  QSS-OUTBAL-5-1-3   PIC S9(9)V99.
           03  QSS-UNITS-5-1-4    PIC S9(7).
           03  QSS-OUTBAL-5-1-4   PIC S9(9)V99.
           03  QSS-UNITS-5-1-5    PIC S9(7).
           03  QSS-OUTBAL-5-1-5   PIC S9(9)V99.
           03  QSS-NO-PL-5-1-1    PIC S9(7).
           03  QSS-AMT-PL-5-1-1   PIC S9(9)V99.
           03  QSS-NO-PL-5-1-2    PIC S9(7).
           03  QSS-AMT-PL-5-1-2   PIC S9(9)V99.
           03  QSS-UNITS-5-2-1    PIC S9(7).
           03  QSS-OUTBAL-5-2-1   PIC S9(9)V99.
           03  QSS-UNITS-5-2-2    PIC S9(7).
           03  QSS-OUTBAL-5-2-2   PIC S9(9)V99.
           03  QSS-UNITS-5-2-3    PIC S9(7).
           03  QSS-OUTBAL-5-2-3   PIC S9(9)V99.
           03  QSS-UNITS-5-2-4    PIC S9(7).
           03  QSS-OUTBAL-5-2-4   PIC S9(9)V99.
           03  QSS-UNITS-5-2-5    PIC S9(7).
           03  QSS-OUTBAL-5-2-5   PIC S9(9)V99.
           03  QSS-NO-PL-5-2-1    PIC S9(7).
           03  QSS-AMT-PL-5-2-1   PIC S9(9)V99.
           03  QSS-NO-PL-5-2-2    PIC S9(7).
           03  QSS-AMT-PL-5-2-2   PIC S9(9)V99.
           03  QSS-UNITS-6-1-1    PIC S9(7).
           03  QSS-OUTBAL-6-1-1   PIC S9(9)V99.
           03  QSS-UNITS-6-1-2    PIC S9(7).
           03  QSS-OUTBAL-6-1-2   PIC S9(9)V99.
           03  QSS-UNITS-6-1-3    PIC S9(7).
           03  QSS-OUTBAL-6-1-3   PIC S9(9)V99.
           03  QSS-UNITS-6-1-4    PIC S9(7).
           03  QSS-OUTBAL-6-1-4   PIC S9(9)V99.
           03  QSS-UNITS-6-1-5    PIC S9(7).
           03  QSS-OUTBAL-6-1-5   PIC S9(9)V99.
           03  QSS-NO-PL-6-1-1    PIC S9(7).
           03  QSS-AMT-PL-6-1-1   PIC S9(9)V99.
           03  QSS-NO-PL-6-1-2    PIC S9(7).
           03  QSS-AMT-PL-6-1-2   PIC S9(9)V99.
           03  QSS-UNITS-6-2-1    PIC S9(7).
           03  QSS-OUTBAL-6-2-1   PIC S9(9)V99.
           03  QSS-UNITS-6-2-2    PIC S9(7).
           03  QSS-OUTBAL-6-2-2   PIC S9(9)V99.
           03  QSS-UNITS-6-2-3    PIC S9(7).
           03  QSS-OUTBAL-6-2-3   PIC S9(9)V99.
           03  QSS-UNITS-6-2-4    PIC S9(7).
           03  QSS-OUTBAL-6-2-4   PIC S9(9)V99.
           03  QSS-UNITS-6-2-5    PIC S9(7).
           03  QSS-OUTBAL-6-2-5   PIC S9(9)V99.
           03  QSS-NO-PL-6-2-1    PIC S9(7).
           03  QSS-AMT-PL-6-2-1   PIC S9(9)V99.
           03  QSS-NO-PL-6-2-2    PIC S9(7).
           03  QSS-AMT-PL-6-2-2   PIC S9(9)V99.
           03  QSS-UNITS-7-1-1    PIC S9(7).
           03  QSS-OUTBAL-7-1-1   PIC S9(9)V99.
           03  QSS-UNITS-7-1-2    PIC S9(7).
           03  QSS-OUTBAL-7-1-2   PIC S9(9)V99.
           03  QSS-UNITS-7-1-3    PIC S9(7).
           03  QSS-OUTBAL-7-1-3   PIC S9(9)V99.
           03  QSS-UNITS-7-1-4    PIC S9(7).
           03  QSS-OUTBAL-7-1-4   PIC S9(9)V99.
           03  QSS-UNITS-7-1-5    PIC S9(7).
           03  QSS-OUTBAL-7-1-5   PIC S9(9)V99.
           03  QSS-NO-PL-7-1-1    PIC S9(7).
           03  QSS-AMT-PL-7-1-1   PIC S9(9)V99.
           03  QSS-NO-PL-7-1-2    PIC S9(7).
           03  QSS-AMT-PL-7-1-2   PIC S9(9)V99.
           03  QSS-UNITS-7-2-1    PIC S9(7).
           03  QSS-OUTBAL-7-2-1   PIC S9(9)V99.
           03  QSS-UNITS-7-2-2    PIC S9(7).
           03  QSS-OUTBAL-7-2-2   PIC S9(9)V99.
           03  QSS-UNITS-7-2-3    PIC S9(7).
           03  QSS-OUTBAL-7-2-3   PIC S9(9)V99.
           03  QSS-UNITS-7-2-4    PIC S9(7).
           03  QSS-OUTBAL-7-2-4   PIC S9(9)V99.
           03  QSS-UNITS-7-2-5    PIC S9(7).
           03  QSS-OUTBAL-7-2-5   PIC S9(9)V99.
           03  QSS-NO-PL-7-2-1    PIC S9(7).
           03  QSS-AMT-PL-7-2-1   PIC S9(9)V99.
           03  QSS-NO-PL-7-2-2    PIC S9(7).
           03  QSS-AMT-PL-7-2-2   PIC S9(9)V99.
           03  QSS-UNITS-8-1-1    PIC S9(7).
           03  QSS-OUTBAL-8-1-1   PIC S9(9)V99.
           03  QSS-UNITS-8-1-2    PIC S9(7).
           03  QSS-OUTBAL-8-1-2   PIC S9(9)V99.
           03  QSS-UNITS-8-1-3    PIC S9(7).
           03  QSS-OUTBAL-8-1-3   PIC S9(9)V99.
           03  QSS-UNITS-8-1-4    PIC S9(7).
           03  QSS-OUTBAL-8-1-4   PIC S9(9)V99.
           03  QSS-UNITS-8-1-5    PIC S9(7).
           03  QSS-OUTBAL-8-1-5   PIC S9(9)V99.
           03  QSS-NO-PL-8-1-1    PIC S9(7).
           03  QSS-AMT-PL-8-1-1   PIC S9(9)V99.
           03  QSS-NO-PL-8-1-2    PIC S9(7).
           03  QSS-AMT-PL-8-1-2   PIC S9(9)V99.
           03  QSS-UNITS-8-2-1    PIC S9(7).
           03  QSS-OUTBAL-8-2-1   PIC S9(9)V99.
           03  QSS-UNITS-8-2-2    PIC S9(7).
           03  QSS-OUTBAL-8-2-2   PIC S9(9)V99.
           03  QSS-UNITS-8-2-3    PIC S9(7).
           03  QSS-OUTBAL-8-2-3   PIC S9(9)V99.
           03  QSS-UNITS-8-2-4    PIC S9(7).
           03  QSS-OUTBAL-8-2-4   PIC S9(9)V99.
           03  QSS-UNITS-8-2-5    PIC S9(7).
           03  QSS-OUTBAL-8-2-5   PIC S9(9)V99.
           03  QSS-NO-PL-8-2-1    PIC S9(7).
           03  QSS-AMT-PL-8-2-1   PIC S9(9)V99.
           03  QSS-NO-PL-8-2-2    PIC S9(7).
           03  QSS-AMT-PL-8-2-2   PIC S9(9)V99.

       EXEC SQL END DECLARE SECTION END-EXEC.
      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S WS COPY MEMBERS
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

       01  LEGEND-BEG-DATE. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING DATE       (F3-ALL) ".

       01  LEGEND-END-DATE. 
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING DATE       ".

       01  LEGEND-BEG-BRNO. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING BRNO       (F3-ALL) ".

       01  LEGEND-END-BRNO. 
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING BRNO       ".

       01  LEGEND-BEG-CLASS.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING CLASS      (F3-ALL) ".

       01  LEGEND-END-CLASS.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING CLASS      ".

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
           03  BEG-DATE               PIC 9(06).
           03  END-DATE               PIC 9(06).
           03  BEG-BRNO               PIC 9(04).
           03  END-BRNO               PIC 9(04).
           03  BEG-CLASS              PIC 9(03).
           03  END-CLASS              PIC 9(03).

           03  PROCESS-KEY             PIC 9(01).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

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
           03  FILLER PIC X(30) VALUE "DATE  BRNO CLASS              ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-SS-DATE                    PIC 9(08).
           03  FILLER                         PIC X(01).
           03  D01-SS-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(01).
           03  D01-SS-CLASS                   PIC 9(03).
           03  FILLER                         PIC X(01).

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
      *COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
      *COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
      *COPY "LIBGB/GRWORKW.CPY".
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
      *COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBIO/IOSSIN_DEF.CPY".
       COPY "LIBIO/IOSSIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOSSIN_SCN.CPY".

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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      D    STOP FORM-PATHNAME.
           MOVE "B"                        TO SS-PATH-OVERRIDE.

           MOVE BEG-DATE                   TO QSS1-WBEG-DATE.
           MOVE BEG-BRNO                   TO QSS1-WBEG-BRNO.
           MOVE BEG-CLASS                  TO QSS1-WBEG-CLASS.

           MOVE END-DATE                   TO QSS1-WEND-DATE.
           MOVE END-BRNO                   TO QSS1-WEND-BRNO.
           MOVE END-CLASS                  TO QSS1-WEND-CLASS.

           PERFORM OPEN-SS1-FILE.
           PERFORM START-SS1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-SS1-FILE-NEXT.

           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SS-DATE-YYYYMM < BEG-DATE )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( SS-DATE-YYYYMM > END-DATE )
              GO TO MAIN-PROGRAM-END
           ELSE
           IF ( SS-BRNO < BEG-BRNO ) OR
              ( SS-BRNO > END-BRNO )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( SS-CLASS < BEG-CLASS ) OR
              ( SS-CLASS > END-CLASS )
              GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SS-DATE-YYYYMM             TO D01-SS-DATE.
           MOVE SS-BRNO                    TO D01-SS-BRNO.
           MOVE SS-CLASS                   TO D01-SS-CLASS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.

           GO TO MAIN-PROGRAM-NEXT.

      *=================================================================
       MAIN-PROGRAM-END.

           PERFORM WRITE-SUMMARY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DISPLAY SUMMARY TO SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROGRAM COMPLETED!!!" TO SENDMESS-HEAD.
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
           MOVE "IOSSIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOSSIN-QUEUE-POS  TO Q-POS.
           MOVE IOSSIN-COPIES-POS TO C-POS.
           MOVE IOSSIN-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROCESSING..." TO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

           PERFORM GET-GPENV.
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
              GO TO ENTER-PROCESS-KEY.

      *=================================================================
       ENTER-BEG-DATE.

           MOVE LEGEND-BEG-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           MOVE LEGEND-END-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-DATE.

           IF ( BEG-DATE > END-DATE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-BEG-BRNO.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "SNNN060BEG-DATE." TO VDU.
           MOVE 0                    TO BEG-DATE VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-DATE." TO VDU.
           MOVE 999999               TO END-DATE VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-BRNO.

           MOVE LEGEND-BEG-BRNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BEG-BRNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRNO.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-BRNO.

      *-----------------------------------------------------------------
       ENTER-END-BRNO.

           MOVE LEGEND-END-BRNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060END-BRNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRNO.

           IF ( BEG-BRNO > END-BRNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRNO.

           GO TO ENTER-BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-ALL-BRNO.

           MOVE "SNNN040BEG-BRNO." TO VDU.
           MOVE 0                    TO BEG-BRNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRNO." TO VDU.
           MOVE 9999                 TO END-BRNO VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-CLASS.

           MOVE LEGEND-BEG-CLASS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN030BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-END-CLASS.

           MOVE LEGEND-END-CLASS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN030END-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-CLASS.

           IF ( BEG-CLASS > END-DATE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-CLASS.

           MOVE "SNNN030BEG-CLASS." TO VDU.
           MOVE 0                    TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-CLASS." TO VDU.
           MOVE 999                  TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 < ZEROES ) OR
              ( VDUNUM0 > 1      )     
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

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

           MOVE "SNNN060BEG-DATE."    TO VDU.
           MOVE BEG-DATE              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-DATE."    TO VDU.
           MOVE END-DATE              TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN060BEG-BRNO."    TO VDU.
           MOVE BEG-BRNO              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-BRNO."    TO VDU.
           MOVE END-BRNO              TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN060BEG-CLASS."   TO VDU.
           MOVE BEG-CLASS             TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-CLASS."   TO VDU.
           MOVE END-CLASS             TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN010PROCESS-KEY." TO VDU
           MOVE PROCESS-KEY           TO VDUNUM0.
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

           MOVE SS1-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING DATE               : "    TO R01-PROMPT.
           MOVE BEG-DATE                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE               : "    TO R01-PROMPT.
           MOVE END-DATE                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "BEGINNING ACCTNO              : "    TO R01-PROMPT.
      *    MOVE BEG-ACCTNO                            TO R01-NUMERIC.
      *    MOVE 1                                     TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

      *    MOVE "ENDING    ACCTNO              : "    TO R01-PROMPT.
      *    MOVE END-ACCTNO                            TO R01-NUMERIC.
      *    MOVE 1                                     TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

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
           MOVE "SQL TABLE IO UTILITY - SSFILE"  TO H02-TITLE.
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{04}
           MOVE ALL "="                          TO PR-REC.
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

           MOVE EXT-CONAME-CONAME TO IOSSIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOSSIN-SYSDATE.
           DISPLAY IOSSIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOSSIN-FORM-FIELDS TO WR-BUF.
           MOVE IOSSIN-FORM-LIST TO WR-LIST.
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

           MOVE IOSSIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOSSIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOSSIN_EVA.CPY".
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
      *COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
      *COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".
      *COPY "LIBGB/GRSERCH.CPY".
      *COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBWI/PFSWINDOWS.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
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
       COPY "LIBGB/GBPRINT.CPY".

      *-----------------------------------------------------------------
      *    LOAD SS-REC FROM QSS-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-SS-FIELDS.
           MOVE QSS-DATE-YYYYMM TO SS-DATE-YYYYMM.
           MOVE QSS-BRNO TO SS-BRNO.
           MOVE QSS-CLASS TO SS-CLASS.

           IF ( QSS-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO SS-LTOUCH-DATE
           ELSE
              MOVE QSS-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO SS-LTOUCH-DATE.

           MOVE QSS-UNITS-1-1-1 TO SS-UNITS(1 1 1).
           MOVE QSS-OUTBAL-1-1-1 TO SS-OUTBAL(1 1 1).
           MOVE QSS-UNITS-1-1-2 TO SS-UNITS(1 1 2).
           MOVE QSS-OUTBAL-1-1-2 TO SS-OUTBAL(1 1 2).
           MOVE QSS-UNITS-1-1-3 TO SS-UNITS(1 1 3).
           MOVE QSS-OUTBAL-1-1-3 TO SS-OUTBAL(1 1 3).
           MOVE QSS-UNITS-1-1-4 TO SS-UNITS(1 1 4).
           MOVE QSS-OUTBAL-1-1-4 TO SS-OUTBAL(1 1 4).
           MOVE QSS-UNITS-1-1-5 TO SS-UNITS(1 1 5).
           MOVE QSS-OUTBAL-1-1-5 TO SS-OUTBAL(1 1 5).
           MOVE QSS-NO-PL-1-1-1 TO SS-NO-PL(1 1 1).
           MOVE QSS-AMT-PL-1-1-1 TO SS-AMT-PL(1 1 1).
           MOVE QSS-NO-PL-1-1-2 TO SS-NO-PL(1 1 2).
           MOVE QSS-AMT-PL-1-1-2 TO SS-AMT-PL(1 1 2).
           MOVE QSS-UNITS-1-2-1 TO SS-UNITS(1 2 1).
           MOVE QSS-OUTBAL-1-2-1 TO SS-OUTBAL(1 2 1).
           MOVE QSS-UNITS-1-2-2 TO SS-UNITS(1 2 2).
           MOVE QSS-OUTBAL-1-2-2 TO SS-OUTBAL(1 2 2).
           MOVE QSS-UNITS-1-2-3 TO SS-UNITS(1 2 3).
           MOVE QSS-OUTBAL-1-2-3 TO SS-OUTBAL(1 2 3).
           MOVE QSS-UNITS-1-2-4 TO SS-UNITS(1 2 4).
           MOVE QSS-OUTBAL-1-2-4 TO SS-OUTBAL(1 2 4).
           MOVE QSS-UNITS-1-2-5 TO SS-UNITS(1 2 5).
           MOVE QSS-OUTBAL-1-2-5 TO SS-OUTBAL(1 2 5).
           MOVE QSS-NO-PL-1-2-1 TO SS-NO-PL(1 2 1).
           MOVE QSS-AMT-PL-1-2-1 TO SS-AMT-PL(1 2 1).
           MOVE QSS-NO-PL-1-2-2 TO SS-NO-PL(1 2 2).
           MOVE QSS-AMT-PL-1-2-2 TO SS-AMT-PL(1 2 2).
           MOVE QSS-UNITS-2-1-1 TO SS-UNITS(2 1 1).
           MOVE QSS-OUTBAL-2-1-1 TO SS-OUTBAL(2 1 1).
           MOVE QSS-UNITS-2-1-2 TO SS-UNITS(2 1 2).
           MOVE QSS-OUTBAL-2-1-2 TO SS-OUTBAL(2 1 2).
           MOVE QSS-UNITS-2-1-3 TO SS-UNITS(2 1 3).
           MOVE QSS-OUTBAL-2-1-3 TO SS-OUTBAL(2 1 3).
           MOVE QSS-UNITS-2-1-4 TO SS-UNITS(2 1 4).
           MOVE QSS-OUTBAL-2-1-4 TO SS-OUTBAL(2 1 4).
           MOVE QSS-UNITS-2-1-5 TO SS-UNITS(2 1 5).
           MOVE QSS-OUTBAL-2-1-5 TO SS-OUTBAL(2 1 5).
           MOVE QSS-NO-PL-2-1-1 TO SS-NO-PL(2 1 1).
           MOVE QSS-AMT-PL-2-1-1 TO SS-AMT-PL(2 1 1).
           MOVE QSS-NO-PL-2-1-2 TO SS-NO-PL(2 1 2).
           MOVE QSS-AMT-PL-2-1-2 TO SS-AMT-PL(2 1 2).
           MOVE QSS-UNITS-2-2-1 TO SS-UNITS(2 2 1).
           MOVE QSS-OUTBAL-2-2-1 TO SS-OUTBAL(2 2 1).
           MOVE QSS-UNITS-2-2-2 TO SS-UNITS(2 2 2).
           MOVE QSS-OUTBAL-2-2-2 TO SS-OUTBAL(2 2 2).
           MOVE QSS-UNITS-2-2-3 TO SS-UNITS(2 2 3).
           MOVE QSS-OUTBAL-2-2-3 TO SS-OUTBAL(2 2 3).
           MOVE QSS-UNITS-2-2-4 TO SS-UNITS(2 2 4).
           MOVE QSS-OUTBAL-2-2-4 TO SS-OUTBAL(2 2 4).
           MOVE QSS-UNITS-2-2-5 TO SS-UNITS(2 2 5).
           MOVE QSS-OUTBAL-2-2-5 TO SS-OUTBAL(2 2 5).
           MOVE QSS-NO-PL-2-2-1 TO SS-NO-PL(2 2 1).
           MOVE QSS-AMT-PL-2-2-1 TO SS-AMT-PL(2 2 1).
           MOVE QSS-NO-PL-2-2-2 TO SS-NO-PL(2 2 2).
           MOVE QSS-AMT-PL-2-2-2 TO SS-AMT-PL(2 2 2).
           MOVE QSS-UNITS-3-1-1 TO SS-UNITS(3 1 1).
           MOVE QSS-OUTBAL-3-1-1 TO SS-OUTBAL(3 1 1).
           MOVE QSS-UNITS-3-1-2 TO SS-UNITS(3 1 2).
           MOVE QSS-OUTBAL-3-1-2 TO SS-OUTBAL(3 1 2).
           MOVE QSS-UNITS-3-1-3 TO SS-UNITS(3 1 3).
           MOVE QSS-OUTBAL-3-1-3 TO SS-OUTBAL(3 1 3).
           MOVE QSS-UNITS-3-1-4 TO SS-UNITS(3 1 4).
           MOVE QSS-OUTBAL-3-1-4 TO SS-OUTBAL(3 1 4).
           MOVE QSS-UNITS-3-1-5 TO SS-UNITS(3 1 5).
           MOVE QSS-OUTBAL-3-1-5 TO SS-OUTBAL(3 1 5).
           MOVE QSS-NO-PL-3-1-1 TO SS-NO-PL(3 1 1).
           MOVE QSS-AMT-PL-3-1-1 TO SS-AMT-PL(3 1 1).
           MOVE QSS-NO-PL-3-1-2 TO SS-NO-PL(3 1 2).
           MOVE QSS-AMT-PL-3-1-2 TO SS-AMT-PL(3 1 2).
           MOVE QSS-UNITS-3-2-1 TO SS-UNITS(3 2 1).
           MOVE QSS-OUTBAL-3-2-1 TO SS-OUTBAL(3 2 1).
           MOVE QSS-UNITS-3-2-2 TO SS-UNITS(3 2 2).
           MOVE QSS-OUTBAL-3-2-2 TO SS-OUTBAL(3 2 2).
           MOVE QSS-UNITS-3-2-3 TO SS-UNITS(3 2 3).
           MOVE QSS-OUTBAL-3-2-3 TO SS-OUTBAL(3 2 3).
           MOVE QSS-UNITS-3-2-4 TO SS-UNITS(3 2 4).
           MOVE QSS-OUTBAL-3-2-4 TO SS-OUTBAL(3 2 4).
           MOVE QSS-UNITS-3-2-5 TO SS-UNITS(3 2 5).
           MOVE QSS-OUTBAL-3-2-5 TO SS-OUTBAL(3 2 5).
           MOVE QSS-NO-PL-3-2-1 TO SS-NO-PL(3 2 1).
           MOVE QSS-AMT-PL-3-2-1 TO SS-AMT-PL(3 2 1).
           MOVE QSS-NO-PL-3-2-2 TO SS-NO-PL(3 2 2).
           MOVE QSS-AMT-PL-3-2-2 TO SS-AMT-PL(3 2 2).
           MOVE QSS-UNITS-4-1-1 TO SS-UNITS(4 1 1).
           MOVE QSS-OUTBAL-4-1-1 TO SS-OUTBAL(4 1 1).
           MOVE QSS-UNITS-4-1-2 TO SS-UNITS(4 1 2).
           MOVE QSS-OUTBAL-4-1-2 TO SS-OUTBAL(4 1 2).
           MOVE QSS-UNITS-4-1-3 TO SS-UNITS(4 1 3).
           MOVE QSS-OUTBAL-4-1-3 TO SS-OUTBAL(4 1 3).
           MOVE QSS-UNITS-4-1-4 TO SS-UNITS(4 1 4).
           MOVE QSS-OUTBAL-4-1-4 TO SS-OUTBAL(4 1 4).
           MOVE QSS-UNITS-4-1-5 TO SS-UNITS(4 1 5).
           MOVE QSS-OUTBAL-4-1-5 TO SS-OUTBAL(4 1 5).
           MOVE QSS-NO-PL-4-1-1 TO SS-NO-PL(4 1 1).
           MOVE QSS-AMT-PL-4-1-1 TO SS-AMT-PL(4 1 1).
           MOVE QSS-NO-PL-4-1-2 TO SS-NO-PL(4 1 2).
           MOVE QSS-AMT-PL-4-1-2 TO SS-AMT-PL(4 1 2).
           MOVE QSS-UNITS-4-2-1 TO SS-UNITS(4 2 1).
           MOVE QSS-OUTBAL-4-2-1 TO SS-OUTBAL(4 2 1).
           MOVE QSS-UNITS-4-2-2 TO SS-UNITS(4 2 2).
           MOVE QSS-OUTBAL-4-2-2 TO SS-OUTBAL(4 2 2).
           MOVE QSS-UNITS-4-2-3 TO SS-UNITS(4 2 3).
           MOVE QSS-OUTBAL-4-2-3 TO SS-OUTBAL(4 2 3).
           MOVE QSS-UNITS-4-2-4 TO SS-UNITS(4 2 4).
           MOVE QSS-OUTBAL-4-2-4 TO SS-OUTBAL(4 2 4).
           MOVE QSS-UNITS-4-2-5 TO SS-UNITS(4 2 5).
           MOVE QSS-OUTBAL-4-2-5 TO SS-OUTBAL(4 2 5).
           MOVE QSS-NO-PL-4-2-1 TO SS-NO-PL(4 2 1).
           MOVE QSS-AMT-PL-4-2-1 TO SS-AMT-PL(4 2 1).
           MOVE QSS-NO-PL-4-2-2 TO SS-NO-PL(4 2 2).
           MOVE QSS-AMT-PL-4-2-2 TO SS-AMT-PL(4 2 2).
           MOVE QSS-UNITS-5-1-1 TO SS-UNITS(5 1 1).
           MOVE QSS-OUTBAL-5-1-1 TO SS-OUTBAL(5 1 1).
           MOVE QSS-UNITS-5-1-2 TO SS-UNITS(5 1 2).
           MOVE QSS-OUTBAL-5-1-2 TO SS-OUTBAL(5 1 2).
           MOVE QSS-UNITS-5-1-3 TO SS-UNITS(5 1 3).
           MOVE QSS-OUTBAL-5-1-3 TO SS-OUTBAL(5 1 3).
           MOVE QSS-UNITS-5-1-4 TO SS-UNITS(5 1 4).
           MOVE QSS-OUTBAL-5-1-4 TO SS-OUTBAL(5 1 4).
           MOVE QSS-UNITS-5-1-5 TO SS-UNITS(5 1 5).
           MOVE QSS-OUTBAL-5-1-5 TO SS-OUTBAL(5 1 5).
           MOVE QSS-NO-PL-5-1-1 TO SS-NO-PL(5 1 1).
           MOVE QSS-AMT-PL-5-1-1 TO SS-AMT-PL(5 1 1).
           MOVE QSS-NO-PL-5-1-2 TO SS-NO-PL(5 1 2).
           MOVE QSS-AMT-PL-5-1-2 TO SS-AMT-PL(5 1 2).
           MOVE QSS-UNITS-5-2-1 TO SS-UNITS(5 2 1).
           MOVE QSS-OUTBAL-5-2-1 TO SS-OUTBAL(5 2 1).
           MOVE QSS-UNITS-5-2-2 TO SS-UNITS(5 2 2).
           MOVE QSS-OUTBAL-5-2-2 TO SS-OUTBAL(5 2 2).
           MOVE QSS-UNITS-5-2-3 TO SS-UNITS(5 2 3).
           MOVE QSS-OUTBAL-5-2-3 TO SS-OUTBAL(5 2 3).
           MOVE QSS-UNITS-5-2-4 TO SS-UNITS(5 2 4).
           MOVE QSS-OUTBAL-5-2-4 TO SS-OUTBAL(5 2 4).
           MOVE QSS-UNITS-5-2-5 TO SS-UNITS(5 2 5).
           MOVE QSS-OUTBAL-5-2-5 TO SS-OUTBAL(5 2 5).
           MOVE QSS-NO-PL-5-2-1 TO SS-NO-PL(5 2 1).
           MOVE QSS-AMT-PL-5-2-1 TO SS-AMT-PL(5 2 1).
           MOVE QSS-NO-PL-5-2-2 TO SS-NO-PL(5 2 2).
           MOVE QSS-AMT-PL-5-2-2 TO SS-AMT-PL(5 2 2).
           MOVE QSS-UNITS-6-1-1 TO SS-UNITS(6 1 1).
           MOVE QSS-OUTBAL-6-1-1 TO SS-OUTBAL(6 1 1).
           MOVE QSS-UNITS-6-1-2 TO SS-UNITS(6 1 2).
           MOVE QSS-OUTBAL-6-1-2 TO SS-OUTBAL(6 1 2).
           MOVE QSS-UNITS-6-1-3 TO SS-UNITS(6 1 3).
           MOVE QSS-OUTBAL-6-1-3 TO SS-OUTBAL(6 1 3).
           MOVE QSS-UNITS-6-1-4 TO SS-UNITS(6 1 4).
           MOVE QSS-OUTBAL-6-1-4 TO SS-OUTBAL(6 1 4).
           MOVE QSS-UNITS-6-1-5 TO SS-UNITS(6 1 5).
           MOVE QSS-OUTBAL-6-1-5 TO SS-OUTBAL(6 1 5).
           MOVE QSS-NO-PL-6-1-1 TO SS-NO-PL(6 1 1).
           MOVE QSS-AMT-PL-6-1-1 TO SS-AMT-PL(6 1 1).
           MOVE QSS-NO-PL-6-1-2 TO SS-NO-PL(6 1 2).
           MOVE QSS-AMT-PL-6-1-2 TO SS-AMT-PL(6 1 2).
           MOVE QSS-UNITS-6-2-1 TO SS-UNITS(6 2 1).
           MOVE QSS-OUTBAL-6-2-1 TO SS-OUTBAL(6 2 1).
           MOVE QSS-UNITS-6-2-2 TO SS-UNITS(6 2 2).
           MOVE QSS-OUTBAL-6-2-2 TO SS-OUTBAL(6 2 2).
           MOVE QSS-UNITS-6-2-3 TO SS-UNITS(6 2 3).
           MOVE QSS-OUTBAL-6-2-3 TO SS-OUTBAL(6 2 3).
           MOVE QSS-UNITS-6-2-4 TO SS-UNITS(6 2 4).
           MOVE QSS-OUTBAL-6-2-4 TO SS-OUTBAL(6 2 4).
           MOVE QSS-UNITS-6-2-5 TO SS-UNITS(6 2 5).
           MOVE QSS-OUTBAL-6-2-5 TO SS-OUTBAL(6 2 5).
           MOVE QSS-NO-PL-6-2-1 TO SS-NO-PL(6 2 1).
           MOVE QSS-AMT-PL-6-2-1 TO SS-AMT-PL(6 2 1).
           MOVE QSS-NO-PL-6-2-2 TO SS-NO-PL(6 2 2).
           MOVE QSS-AMT-PL-6-2-2 TO SS-AMT-PL(6 2 2).
           MOVE QSS-UNITS-7-1-1 TO SS-UNITS(7 1 1).
           MOVE QSS-OUTBAL-7-1-1 TO SS-OUTBAL(7 1 1).
           MOVE QSS-UNITS-7-1-2 TO SS-UNITS(7 1 2).
           MOVE QSS-OUTBAL-7-1-2 TO SS-OUTBAL(7 1 2).
           MOVE QSS-UNITS-7-1-3 TO SS-UNITS(7 1 3).
           MOVE QSS-OUTBAL-7-1-3 TO SS-OUTBAL(7 1 3).
           MOVE QSS-UNITS-7-1-4 TO SS-UNITS(7 1 4).
           MOVE QSS-OUTBAL-7-1-4 TO SS-OUTBAL(7 1 4).
           MOVE QSS-UNITS-7-1-5 TO SS-UNITS(7 1 5).
           MOVE QSS-OUTBAL-7-1-5 TO SS-OUTBAL(7 1 5).
           MOVE QSS-NO-PL-7-1-1 TO SS-NO-PL(7 1 1).
           MOVE QSS-AMT-PL-7-1-1 TO SS-AMT-PL(7 1 1).
           MOVE QSS-NO-PL-7-1-2 TO SS-NO-PL(7 1 2).
           MOVE QSS-AMT-PL-7-1-2 TO SS-AMT-PL(7 1 2).
           MOVE QSS-UNITS-7-2-1 TO SS-UNITS(7 2 1).
           MOVE QSS-OUTBAL-7-2-1 TO SS-OUTBAL(7 2 1).
           MOVE QSS-UNITS-7-2-2 TO SS-UNITS(7 2 2).
           MOVE QSS-OUTBAL-7-2-2 TO SS-OUTBAL(7 2 2).
           MOVE QSS-UNITS-7-2-3 TO SS-UNITS(7 2 3).
           MOVE QSS-OUTBAL-7-2-3 TO SS-OUTBAL(7 2 3).
           MOVE QSS-UNITS-7-2-4 TO SS-UNITS(7 2 4).
           MOVE QSS-OUTBAL-7-2-4 TO SS-OUTBAL(7 2 4).
           MOVE QSS-UNITS-7-2-5 TO SS-UNITS(7 2 5).
           MOVE QSS-OUTBAL-7-2-5 TO SS-OUTBAL(7 2 5).
           MOVE QSS-NO-PL-7-2-1 TO SS-NO-PL(7 2 1).
           MOVE QSS-AMT-PL-7-2-1 TO SS-AMT-PL(7 2 1).
           MOVE QSS-NO-PL-7-2-2 TO SS-NO-PL(7 2 2).
           MOVE QSS-AMT-PL-7-2-2 TO SS-AMT-PL(7 2 2).
           MOVE QSS-UNITS-8-1-1 TO SS-UNITS(8 1 1).
           MOVE QSS-OUTBAL-8-1-1 TO SS-OUTBAL(8 1 1).
           MOVE QSS-UNITS-8-1-2 TO SS-UNITS(8 1 2).
           MOVE QSS-OUTBAL-8-1-2 TO SS-OUTBAL(8 1 2).
           MOVE QSS-UNITS-8-1-3 TO SS-UNITS(8 1 3).
           MOVE QSS-OUTBAL-8-1-3 TO SS-OUTBAL(8 1 3).
           MOVE QSS-UNITS-8-1-4 TO SS-UNITS(8 1 4).
           MOVE QSS-OUTBAL-8-1-4 TO SS-OUTBAL(8 1 4).
           MOVE QSS-UNITS-8-1-5 TO SS-UNITS(8 1 5).
           MOVE QSS-OUTBAL-8-1-5 TO SS-OUTBAL(8 1 5).
           MOVE QSS-NO-PL-8-1-1 TO SS-NO-PL(8 1 1).
           MOVE QSS-AMT-PL-8-1-1 TO SS-AMT-PL(8 1 1).
           MOVE QSS-NO-PL-8-1-2 TO SS-NO-PL(8 1 2).
           MOVE QSS-AMT-PL-8-1-2 TO SS-AMT-PL(8 1 2).
           MOVE QSS-UNITS-8-2-1 TO SS-UNITS(8 2 1).
           MOVE QSS-OUTBAL-8-2-1 TO SS-OUTBAL(8 2 1).
           MOVE QSS-UNITS-8-2-2 TO SS-UNITS(8 2 2).
           MOVE QSS-OUTBAL-8-2-2 TO SS-OUTBAL(8 2 2).
           MOVE QSS-UNITS-8-2-3 TO SS-UNITS(8 2 3).
           MOVE QSS-OUTBAL-8-2-3 TO SS-OUTBAL(8 2 3).
           MOVE QSS-UNITS-8-2-4 TO SS-UNITS(8 2 4).
           MOVE QSS-OUTBAL-8-2-4 TO SS-OUTBAL(8 2 4).
           MOVE QSS-UNITS-8-2-5 TO SS-UNITS(8 2 5).
           MOVE QSS-OUTBAL-8-2-5 TO SS-OUTBAL(8 2 5).
           MOVE QSS-NO-PL-8-2-1 TO SS-NO-PL(8 2 1).
           MOVE QSS-AMT-PL-8-2-1 TO SS-AMT-PL(8 2 1).
           MOVE QSS-NO-PL-8-2-2 TO SS-NO-PL(8 2 2).
           MOVE QSS-AMT-PL-8-2-2 TO SS-AMT-PL(8 2 2).
 
      *-----------------------------------------------------------------
      *    LOAD QSS-REC FROM SS-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-SS-FIELDS.
           MOVE SS-DATE-YYYYMM TO QSS-DATE-YYYYMM.
           MOVE SS-BRNO TO QSS-BRNO.
           MOVE SS-CLASS TO QSS-CLASS.

           IF ( SS-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QSS-LTOUCH-DATE
           ELSE
              MOVE SS-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QSS-LTOUCH-DATE.

           MOVE SS-UNITS(1 1 1) TO QSS-UNITS-1-1-1.
           MOVE SS-OUTBAL(1 1 1) TO QSS-OUTBAL-1-1-1.
           MOVE SS-UNITS(1 1 2) TO QSS-UNITS-1-1-2.
           MOVE SS-OUTBAL(1 1 2) TO QSS-OUTBAL-1-1-2.
           MOVE SS-UNITS(1 1 3) TO QSS-UNITS-1-1-3.
           MOVE SS-OUTBAL(1 1 3) TO QSS-OUTBAL-1-1-3.
           MOVE SS-UNITS(1 1 4) TO QSS-UNITS-1-1-4.
           MOVE SS-OUTBAL(1 1 4) TO QSS-OUTBAL-1-1-4.
           MOVE SS-UNITS(1 1 5) TO QSS-UNITS-1-1-5.
           MOVE SS-OUTBAL(1 1 5) TO QSS-OUTBAL-1-1-5.
           MOVE SS-NO-PL(1 1 1) TO QSS-NO-PL-1-1-1.
           MOVE SS-AMT-PL(1 1 1) TO QSS-AMT-PL-1-1-1.
           MOVE SS-NO-PL(1 1 2) TO QSS-NO-PL-1-1-2.
           MOVE SS-AMT-PL(1 1 2) TO QSS-AMT-PL-1-1-2.
           MOVE SS-UNITS(1 2 1) TO QSS-UNITS-1-2-1.
           MOVE SS-OUTBAL(1 2 1) TO QSS-OUTBAL-1-2-1.
           MOVE SS-UNITS(1 2 2) TO QSS-UNITS-1-2-2.
           MOVE SS-OUTBAL(1 2 2) TO QSS-OUTBAL-1-2-2.
           MOVE SS-UNITS(1 2 3) TO QSS-UNITS-1-2-3.
           MOVE SS-OUTBAL(1 2 3) TO QSS-OUTBAL-1-2-3.
           MOVE SS-UNITS(1 2 4) TO QSS-UNITS-1-2-4.
           MOVE SS-OUTBAL(1 2 4) TO QSS-OUTBAL-1-2-4.
           MOVE SS-UNITS(1 2 5) TO QSS-UNITS-1-2-5.
           MOVE SS-OUTBAL(1 2 5) TO QSS-OUTBAL-1-2-5.
           MOVE SS-NO-PL(1 2 1) TO QSS-NO-PL-1-2-1.
           MOVE SS-AMT-PL(1 2 1) TO QSS-AMT-PL-1-2-1.
           MOVE SS-NO-PL(1 2 2) TO QSS-NO-PL-1-2-2.
           MOVE SS-AMT-PL(1 2 2) TO QSS-AMT-PL-1-2-2.
           MOVE SS-UNITS(2 1 1) TO QSS-UNITS-2-1-1.
           MOVE SS-OUTBAL(2 1 1) TO QSS-OUTBAL-2-1-1.
           MOVE SS-UNITS(2 1 2) TO QSS-UNITS-2-1-2.
           MOVE SS-OUTBAL(2 1 2) TO QSS-OUTBAL-2-1-2.
           MOVE SS-UNITS(2 1 3) TO QSS-UNITS-2-1-3.
           MOVE SS-OUTBAL(2 1 3) TO QSS-OUTBAL-2-1-3.
           MOVE SS-UNITS(2 1 4) TO QSS-UNITS-2-1-4.
           MOVE SS-OUTBAL(2 1 4) TO QSS-OUTBAL-2-1-4.
           MOVE SS-UNITS(2 1 5) TO QSS-UNITS-2-1-5.
           MOVE SS-OUTBAL(2 1 5) TO QSS-OUTBAL-2-1-5.
           MOVE SS-NO-PL(2 1 1) TO QSS-NO-PL-2-1-1.
           MOVE SS-AMT-PL(2 1 1) TO QSS-AMT-PL-2-1-1.
           MOVE SS-NO-PL(2 1 2) TO QSS-NO-PL-2-1-2.
           MOVE SS-AMT-PL(2 1 2) TO QSS-AMT-PL-2-1-2.
           MOVE SS-UNITS(2 2 1) TO QSS-UNITS-2-2-1.
           MOVE SS-OUTBAL(2 2 1) TO QSS-OUTBAL-2-2-1.
           MOVE SS-UNITS(2 2 2) TO QSS-UNITS-2-2-2.
           MOVE SS-OUTBAL(2 2 2) TO QSS-OUTBAL-2-2-2.
           MOVE SS-UNITS(2 2 3) TO QSS-UNITS-2-2-3.
           MOVE SS-OUTBAL(2 2 3) TO QSS-OUTBAL-2-2-3.
           MOVE SS-UNITS(2 2 4) TO QSS-UNITS-2-2-4.
           MOVE SS-OUTBAL(2 2 4) TO QSS-OUTBAL-2-2-4.
           MOVE SS-UNITS(2 2 5) TO QSS-UNITS-2-2-5.
           MOVE SS-OUTBAL(2 2 5) TO QSS-OUTBAL-2-2-5.
           MOVE SS-NO-PL(2 2 1) TO QSS-NO-PL-2-2-1.
           MOVE SS-AMT-PL(2 2 1) TO QSS-AMT-PL-2-2-1.
           MOVE SS-NO-PL(2 2 2) TO QSS-NO-PL-2-2-2.
           MOVE SS-AMT-PL(2 2 2) TO QSS-AMT-PL-2-2-2.
           MOVE SS-UNITS(3 1 1) TO QSS-UNITS-3-1-1.
           MOVE SS-OUTBAL(3 1 1) TO QSS-OUTBAL-3-1-1.
           MOVE SS-UNITS(3 1 2) TO QSS-UNITS-3-1-2.
           MOVE SS-OUTBAL(3 1 2) TO QSS-OUTBAL-3-1-2.
           MOVE SS-UNITS(3 1 3) TO QSS-UNITS-3-1-3.
           MOVE SS-OUTBAL(3 1 3) TO QSS-OUTBAL-3-1-3.
           MOVE SS-UNITS(3 1 4) TO QSS-UNITS-3-1-4.
           MOVE SS-OUTBAL(3 1 4) TO QSS-OUTBAL-3-1-4.
           MOVE SS-UNITS(3 1 5) TO QSS-UNITS-3-1-5.
           MOVE SS-OUTBAL(3 1 5) TO QSS-OUTBAL-3-1-5.
           MOVE SS-NO-PL(3 1 1) TO QSS-NO-PL-3-1-1.
           MOVE SS-AMT-PL(3 1 1) TO QSS-AMT-PL-3-1-1.
           MOVE SS-NO-PL(3 1 2) TO QSS-NO-PL-3-1-2.
           MOVE SS-AMT-PL(3 1 2) TO QSS-AMT-PL-3-1-2.
           MOVE SS-UNITS(3 2 1) TO QSS-UNITS-3-2-1.
           MOVE SS-OUTBAL(3 2 1) TO QSS-OUTBAL-3-2-1.
           MOVE SS-UNITS(3 2 2) TO QSS-UNITS-3-2-2.
           MOVE SS-OUTBAL(3 2 2) TO QSS-OUTBAL-3-2-2.
           MOVE SS-UNITS(3 2 3) TO QSS-UNITS-3-2-3.
           MOVE SS-OUTBAL(3 2 3) TO QSS-OUTBAL-3-2-3.
           MOVE SS-UNITS(3 2 4) TO QSS-UNITS-3-2-4.
           MOVE SS-OUTBAL(3 2 4) TO QSS-OUTBAL-3-2-4.
           MOVE SS-UNITS(3 2 5) TO QSS-UNITS-3-2-5.
           MOVE SS-OUTBAL(3 2 5) TO QSS-OUTBAL-3-2-5.
           MOVE SS-NO-PL(3 2 1) TO QSS-NO-PL-3-2-1.
           MOVE SS-AMT-PL(3 2 1) TO QSS-AMT-PL-3-2-1.
           MOVE SS-NO-PL(3 2 2) TO QSS-NO-PL-3-2-2.
           MOVE SS-AMT-PL(3 2 2) TO QSS-AMT-PL-3-2-2.
           MOVE SS-UNITS(4 1 1) TO QSS-UNITS-4-1-1.
           MOVE SS-OUTBAL(4 1 1) TO QSS-OUTBAL-4-1-1.
           MOVE SS-UNITS(4 1 2) TO QSS-UNITS-4-1-2.
           MOVE SS-OUTBAL(4 1 2) TO QSS-OUTBAL-4-1-2.
           MOVE SS-UNITS(4 1 3) TO QSS-UNITS-4-1-3.
           MOVE SS-OUTBAL(4 1 3) TO QSS-OUTBAL-4-1-3.
           MOVE SS-UNITS(4 1 4) TO QSS-UNITS-4-1-4.
           MOVE SS-OUTBAL(4 1 4) TO QSS-OUTBAL-4-1-4.
           MOVE SS-UNITS(4 1 5) TO QSS-UNITS-4-1-5.
           MOVE SS-OUTBAL(4 1 5) TO QSS-OUTBAL-4-1-5.
           MOVE SS-NO-PL(4 1 1) TO QSS-NO-PL-4-1-1.
           MOVE SS-AMT-PL(4 1 1) TO QSS-AMT-PL-4-1-1.
           MOVE SS-NO-PL(4 1 2) TO QSS-NO-PL-4-1-2.
           MOVE SS-AMT-PL(4 1 2) TO QSS-AMT-PL-4-1-2.
           MOVE SS-UNITS(4 2 1) TO QSS-UNITS-4-2-1.
           MOVE SS-OUTBAL(4 2 1) TO QSS-OUTBAL-4-2-1.
           MOVE SS-UNITS(4 2 2) TO QSS-UNITS-4-2-2.
           MOVE SS-OUTBAL(4 2 2) TO QSS-OUTBAL-4-2-2.
           MOVE SS-UNITS(4 2 3) TO QSS-UNITS-4-2-3.
           MOVE SS-OUTBAL(4 2 3) TO QSS-OUTBAL-4-2-3.
           MOVE SS-UNITS(4 2 4) TO QSS-UNITS-4-2-4.
           MOVE SS-OUTBAL(4 2 4) TO QSS-OUTBAL-4-2-4.
           MOVE SS-UNITS(4 2 5) TO QSS-UNITS-4-2-5.
           MOVE SS-OUTBAL(4 2 5) TO QSS-OUTBAL-4-2-5.
           MOVE SS-NO-PL(4 2 1) TO QSS-NO-PL-4-2-1.
           MOVE SS-AMT-PL(4 2 1) TO QSS-AMT-PL-4-2-1.
           MOVE SS-NO-PL(4 2 2) TO QSS-NO-PL-4-2-2.
           MOVE SS-AMT-PL(4 2 2) TO QSS-AMT-PL-4-2-2.
           MOVE SS-UNITS(5 1 1) TO QSS-UNITS-5-1-1.
           MOVE SS-OUTBAL(5 1 1) TO QSS-OUTBAL-5-1-1.
           MOVE SS-UNITS(5 1 2) TO QSS-UNITS-5-1-2.
           MOVE SS-OUTBAL(5 1 2) TO QSS-OUTBAL-5-1-2.
           MOVE SS-UNITS(5 1 3) TO QSS-UNITS-5-1-3.
           MOVE SS-OUTBAL(5 1 3) TO QSS-OUTBAL-5-1-3.
           MOVE SS-UNITS(5 1 4) TO QSS-UNITS-5-1-4.
           MOVE SS-OUTBAL(5 1 4) TO QSS-OUTBAL-5-1-4.
           MOVE SS-UNITS(5 1 5) TO QSS-UNITS-5-1-5.
           MOVE SS-OUTBAL(5 1 5) TO QSS-OUTBAL-5-1-5.
           MOVE SS-NO-PL(5 1 1) TO QSS-NO-PL-5-1-1.
           MOVE SS-AMT-PL(5 1 1) TO QSS-AMT-PL-5-1-1.
           MOVE SS-NO-PL(5 1 2) TO QSS-NO-PL-5-1-2.
           MOVE SS-AMT-PL(5 1 2) TO QSS-AMT-PL-5-1-2.
           MOVE SS-UNITS(5 2 1) TO QSS-UNITS-5-2-1.
           MOVE SS-OUTBAL(5 2 1) TO QSS-OUTBAL-5-2-1.
           MOVE SS-UNITS(5 2 2) TO QSS-UNITS-5-2-2.
           MOVE SS-OUTBAL(5 2 2) TO QSS-OUTBAL-5-2-2.
           MOVE SS-UNITS(5 2 3) TO QSS-UNITS-5-2-3.
           MOVE SS-OUTBAL(5 2 3) TO QSS-OUTBAL-5-2-3.
           MOVE SS-UNITS(5 2 4) TO QSS-UNITS-5-2-4.
           MOVE SS-OUTBAL(5 2 4) TO QSS-OUTBAL-5-2-4.
           MOVE SS-UNITS(5 2 5) TO QSS-UNITS-5-2-5.
           MOVE SS-OUTBAL(5 2 5) TO QSS-OUTBAL-5-2-5.
           MOVE SS-NO-PL(5 2 1) TO QSS-NO-PL-5-2-1.
           MOVE SS-AMT-PL(5 2 1) TO QSS-AMT-PL-5-2-1.
           MOVE SS-NO-PL(5 2 2) TO QSS-NO-PL-5-2-2.
           MOVE SS-AMT-PL(5 2 2) TO QSS-AMT-PL-5-2-2.
           MOVE SS-UNITS(6 1 1) TO QSS-UNITS-6-1-1.
           MOVE SS-OUTBAL(6 1 1) TO QSS-OUTBAL-6-1-1.
           MOVE SS-UNITS(6 1 2) TO QSS-UNITS-6-1-2.
           MOVE SS-OUTBAL(6 1 2) TO QSS-OUTBAL-6-1-2.
           MOVE SS-UNITS(6 1 3) TO QSS-UNITS-6-1-3.
           MOVE SS-OUTBAL(6 1 3) TO QSS-OUTBAL-6-1-3.
           MOVE SS-UNITS(6 1 4) TO QSS-UNITS-6-1-4.
           MOVE SS-OUTBAL(6 1 4) TO QSS-OUTBAL-6-1-4.
           MOVE SS-UNITS(6 1 5) TO QSS-UNITS-6-1-5.
           MOVE SS-OUTBAL(6 1 5) TO QSS-OUTBAL-6-1-5.
           MOVE SS-NO-PL(6 1 1) TO QSS-NO-PL-6-1-1.
           MOVE SS-AMT-PL(6 1 1) TO QSS-AMT-PL-6-1-1.
           MOVE SS-NO-PL(6 1 2) TO QSS-NO-PL-6-1-2.
           MOVE SS-AMT-PL(6 1 2) TO QSS-AMT-PL-6-1-2.
           MOVE SS-UNITS(6 2 1) TO QSS-UNITS-6-2-1.
           MOVE SS-OUTBAL(6 2 1) TO QSS-OUTBAL-6-2-1.
           MOVE SS-UNITS(6 2 2) TO QSS-UNITS-6-2-2.
           MOVE SS-OUTBAL(6 2 2) TO QSS-OUTBAL-6-2-2.
           MOVE SS-UNITS(6 2 3) TO QSS-UNITS-6-2-3.
           MOVE SS-OUTBAL(6 2 3) TO QSS-OUTBAL-6-2-3.
           MOVE SS-UNITS(6 2 4) TO QSS-UNITS-6-2-4.
           MOVE SS-OUTBAL(6 2 4) TO QSS-OUTBAL-6-2-4.
           MOVE SS-UNITS(6 2 5) TO QSS-UNITS-6-2-5.
           MOVE SS-OUTBAL(6 2 5) TO QSS-OUTBAL-6-2-5.
           MOVE SS-NO-PL(6 2 1) TO QSS-NO-PL-6-2-1.
           MOVE SS-AMT-PL(6 2 1) TO QSS-AMT-PL-6-2-1.
           MOVE SS-NO-PL(6 2 2) TO QSS-NO-PL-6-2-2.
           MOVE SS-AMT-PL(6 2 2) TO QSS-AMT-PL-6-2-2.
           MOVE SS-UNITS(7 1 1) TO QSS-UNITS-7-1-1.
           MOVE SS-OUTBAL(7 1 1) TO QSS-OUTBAL-7-1-1.
           MOVE SS-UNITS(7 1 2) TO QSS-UNITS-7-1-2.
           MOVE SS-OUTBAL(7 1 2) TO QSS-OUTBAL-7-1-2.
           MOVE SS-UNITS(7 1 3) TO QSS-UNITS-7-1-3.
           MOVE SS-OUTBAL(7 1 3) TO QSS-OUTBAL-7-1-3.
           MOVE SS-UNITS(7 1 4) TO QSS-UNITS-7-1-4.
           MOVE SS-OUTBAL(7 1 4) TO QSS-OUTBAL-7-1-4.
           MOVE SS-UNITS(7 1 5) TO QSS-UNITS-7-1-5.
           MOVE SS-OUTBAL(7 1 5) TO QSS-OUTBAL-7-1-5.
           MOVE SS-NO-PL(7 1 1) TO QSS-NO-PL-7-1-1.
           MOVE SS-AMT-PL(7 1 1) TO QSS-AMT-PL-7-1-1.
           MOVE SS-NO-PL(7 1 2) TO QSS-NO-PL-7-1-2.
           MOVE SS-AMT-PL(7 1 2) TO QSS-AMT-PL-7-1-2.
           MOVE SS-UNITS(7 2 1) TO QSS-UNITS-7-2-1.
           MOVE SS-OUTBAL(7 2 1) TO QSS-OUTBAL-7-2-1.
           MOVE SS-UNITS(7 2 2) TO QSS-UNITS-7-2-2.
           MOVE SS-OUTBAL(7 2 2) TO QSS-OUTBAL-7-2-2.
           MOVE SS-UNITS(7 2 3) TO QSS-UNITS-7-2-3.
           MOVE SS-OUTBAL(7 2 3) TO QSS-OUTBAL-7-2-3.
           MOVE SS-UNITS(7 2 4) TO QSS-UNITS-7-2-4.
           MOVE SS-OUTBAL(7 2 4) TO QSS-OUTBAL-7-2-4.
           MOVE SS-UNITS(7 2 5) TO QSS-UNITS-7-2-5.
           MOVE SS-OUTBAL(7 2 5) TO QSS-OUTBAL-7-2-5.
           MOVE SS-NO-PL(7 2 1) TO QSS-NO-PL-7-2-1.
           MOVE SS-AMT-PL(7 2 1) TO QSS-AMT-PL-7-2-1.
           MOVE SS-NO-PL(7 2 2) TO QSS-NO-PL-7-2-2.
           MOVE SS-AMT-PL(7 2 2) TO QSS-AMT-PL-7-2-2.
           MOVE SS-UNITS(8 1 1) TO QSS-UNITS-8-1-1.
           MOVE SS-OUTBAL(8 1 1) TO QSS-OUTBAL-8-1-1.
           MOVE SS-UNITS(8 1 2) TO QSS-UNITS-8-1-2.
           MOVE SS-OUTBAL(8 1 2) TO QSS-OUTBAL-8-1-2.
           MOVE SS-UNITS(8 1 3) TO QSS-UNITS-8-1-3.
           MOVE SS-OUTBAL(8 1 3) TO QSS-OUTBAL-8-1-3.
           MOVE SS-UNITS(8 1 4) TO QSS-UNITS-8-1-4.
           MOVE SS-OUTBAL(8 1 4) TO QSS-OUTBAL-8-1-4.
           MOVE SS-UNITS(8 1 5) TO QSS-UNITS-8-1-5.
           MOVE SS-OUTBAL(8 1 5) TO QSS-OUTBAL-8-1-5.
           MOVE SS-NO-PL(8 1 1) TO QSS-NO-PL-8-1-1.
           MOVE SS-AMT-PL(8 1 1) TO QSS-AMT-PL-8-1-1.
           MOVE SS-NO-PL(8 1 2) TO QSS-NO-PL-8-1-2.
           MOVE SS-AMT-PL(8 1 2) TO QSS-AMT-PL-8-1-2.
           MOVE SS-UNITS(8 2 1) TO QSS-UNITS-8-2-1.
           MOVE SS-OUTBAL(8 2 1) TO QSS-OUTBAL-8-2-1.
           MOVE SS-UNITS(8 2 2) TO QSS-UNITS-8-2-2.
           MOVE SS-OUTBAL(8 2 2) TO QSS-OUTBAL-8-2-2.
           MOVE SS-UNITS(8 2 3) TO QSS-UNITS-8-2-3.
           MOVE SS-OUTBAL(8 2 3) TO QSS-OUTBAL-8-2-3.
           MOVE SS-UNITS(8 2 4) TO QSS-UNITS-8-2-4.
           MOVE SS-OUTBAL(8 2 4) TO QSS-OUTBAL-8-2-4.
           MOVE SS-UNITS(8 2 5) TO QSS-UNITS-8-2-5.
           MOVE SS-OUTBAL(8 2 5) TO QSS-OUTBAL-8-2-5.
           MOVE SS-NO-PL(8 2 1) TO QSS-NO-PL-8-2-1.
           MOVE SS-AMT-PL(8 2 1) TO QSS-AMT-PL-8-2-1.
           MOVE SS-NO-PL(8 2 2) TO QSS-NO-PL-8-2-2.
           MOVE SS-AMT-PL(8 2 2) TO QSS-AMT-PL-8-2-2.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBGB/GBSS1RN
       LOAD-SS1-FILE.

      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO SS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SS-ALL-MACHINE
              MOVE SS-ALL-PATH             TO SS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO SS-ALL-BASE
              MOVE FILPATH-MACHINE         TO SS-ALL-MACHINE
              MOVE SS-ALL-PATH             TO SS-PATH.
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*G2G  *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-SSFILE NOT = "S" )
*G2G          MOVE SS-PATH-BEG             TO SS-SELECT-PATH-BEG
*G2G          MOVE SS-PATH-FILE            TO SS-SELECT-PATH-FILE
*G2G       ELSE
*G2G          MOVE SS-PATH-SQL             TO SS-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-SS1-FILE.
           PERFORM LOAD-SS1-FILE.
           PERFORM OPEN-IT.
           MOVE SS-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-SS1-FILE.
           PERFORM START-IT.
           MOVE SS-SELECT-PATH TO E-FILE.
           MOVE SS1-KEY TO E-KEYX.

           IF ( SS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SS1-FILE.



           IF ( QSS1-WEND-CLASS = ZEROES    ) OR
              ( QSS1-WEND-CLASS NOT NUMERIC )
              MOVE SS-DATE-YYYYMM TO QSS1-WBEG-DATE
              MOVE ALL "9"        TO QSS1-WEND-DATE
              MOVE SS-BRNO        TO QSS1-WBEG-BRNO
              MOVE ALL "9"        TO QSS1-WEND-BRNO
              MOVE SS-CLASS       TO QSS1-WBEG-CLASS
              MOVE ALL "9"        TO QSS1-WEND-CLASS.

           EXEC SQL
            DECLARE SS1_CURSOR CURSOR FOR
            SELECT SSFILE.SS_DATE_YYYYMM,
                   SSFILE.SS_BRNO,
                   SSFILE.SS_CLASS,
                   ISNULL(CAST(SSFILE.SS_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   SSFILE.SS_UNITS_1_1_1,
                   SSFILE.SS_OUTBAL_1_1_1,
                   SSFILE.SS_UNITS_1_1_2,
                   SSFILE.SS_OUTBAL_1_1_2,
                   SSFILE.SS_UNITS_1_1_3,
                   SSFILE.SS_OUTBAL_1_1_3,
                   SSFILE.SS_UNITS_1_1_4,
                   SSFILE.SS_OUTBAL_1_1_4,
                   SSFILE.SS_UNITS_1_1_5,
                   SSFILE.SS_OUTBAL_1_1_5,
                   SSFILE.SS_NO_PL_1_1_1,
                   SSFILE.SS_AMT_PL_1_1_1,
                   SSFILE.SS_NO_PL_1_1_2,
                   SSFILE.SS_AMT_PL_1_1_2,
                   SSFILE.SS_UNITS_1_2_1,
                   SSFILE.SS_OUTBAL_1_2_1,
                   SSFILE.SS_UNITS_1_2_2,
                   SSFILE.SS_OUTBAL_1_2_2,
                   SSFILE.SS_UNITS_1_2_3,
                   SSFILE.SS_OUTBAL_1_2_3,
                   SSFILE.SS_UNITS_1_2_4,
                   SSFILE.SS_OUTBAL_1_2_4,
                   SSFILE.SS_UNITS_1_2_5,
                   SSFILE.SS_OUTBAL_1_2_5,
                   SSFILE.SS_NO_PL_1_2_1,
                   SSFILE.SS_AMT_PL_1_2_1,
                   SSFILE.SS_NO_PL_1_2_2,
                   SSFILE.SS_AMT_PL_1_2_2,
                   SSFILE.SS_UNITS_2_1_1,
                   SSFILE.SS_OUTBAL_2_1_1,
                   SSFILE.SS_UNITS_2_1_2,
                   SSFILE.SS_OUTBAL_2_1_2,
                   SSFILE.SS_UNITS_2_1_3,
                   SSFILE.SS_OUTBAL_2_1_3,
                   SSFILE.SS_UNITS_2_1_4,
                   SSFILE.SS_OUTBAL_2_1_4,
                   SSFILE.SS_UNITS_2_1_5,
                   SSFILE.SS_OUTBAL_2_1_5,
                   SSFILE.SS_NO_PL_2_1_1,
                   SSFILE.SS_AMT_PL_2_1_1,
                   SSFILE.SS_NO_PL_2_1_2,
                   SSFILE.SS_AMT_PL_2_1_2,
                   SSFILE.SS_UNITS_2_2_1,
                   SSFILE.SS_OUTBAL_2_2_1,
                   SSFILE.SS_UNITS_2_2_2,
                   SSFILE.SS_OUTBAL_2_2_2,
                   SSFILE.SS_UNITS_2_2_3,
                   SSFILE.SS_OUTBAL_2_2_3,
                   SSFILE.SS_UNITS_2_2_4,
                   SSFILE.SS_OUTBAL_2_2_4,
                   SSFILE.SS_UNITS_2_2_5,
                   SSFILE.SS_OUTBAL_2_2_5,
                   SSFILE.SS_NO_PL_2_2_1,
                   SSFILE.SS_AMT_PL_2_2_1,
                   SSFILE.SS_NO_PL_2_2_2,
                   SSFILE.SS_AMT_PL_2_2_2,
                   SSFILE.SS_UNITS_3_1_1,
                   SSFILE.SS_OUTBAL_3_1_1,
                   SSFILE.SS_UNITS_3_1_2,
                   SSFILE.SS_OUTBAL_3_1_2,
                   SSFILE.SS_UNITS_3_1_3,
                   SSFILE.SS_OUTBAL_3_1_3,
                   SSFILE.SS_UNITS_3_1_4,
                   SSFILE.SS_OUTBAL_3_1_4,
                   SSFILE.SS_UNITS_3_1_5,
                   SSFILE.SS_OUTBAL_3_1_5,
                   SSFILE.SS_NO_PL_3_1_1,
                   SSFILE.SS_AMT_PL_3_1_1,
                   SSFILE.SS_NO_PL_3_1_2,
                   SSFILE.SS_AMT_PL_3_1_2,
                   SSFILE.SS_UNITS_3_2_1,
                   SSFILE.SS_OUTBAL_3_2_1,
                   SSFILE.SS_UNITS_3_2_2,
                   SSFILE.SS_OUTBAL_3_2_2,
                   SSFILE.SS_UNITS_3_2_3,
                   SSFILE.SS_OUTBAL_3_2_3,
                   SSFILE.SS_UNITS_3_2_4,
                   SSFILE.SS_OUTBAL_3_2_4,
                   SSFILE.SS_UNITS_3_2_5,
                   SSFILE.SS_OUTBAL_3_2_5,
                   SSFILE.SS_NO_PL_3_2_1,
                   SSFILE.SS_AMT_PL_3_2_1,
                   SSFILE.SS_NO_PL_3_2_2,
                   SSFILE.SS_AMT_PL_3_2_2,
                   SSFILE.SS_UNITS_4_1_1,
                   SSFILE.SS_OUTBAL_4_1_1,
                   SSFILE.SS_UNITS_4_1_2,
                   SSFILE.SS_OUTBAL_4_1_2,
                   SSFILE.SS_UNITS_4_1_3,
                   SSFILE.SS_OUTBAL_4_1_3,
                   SSFILE.SS_UNITS_4_1_4,
                   SSFILE.SS_OUTBAL_4_1_4,
                   SSFILE.SS_UNITS_4_1_5,
                   SSFILE.SS_OUTBAL_4_1_5,
                   SSFILE.SS_NO_PL_4_1_1,
                   SSFILE.SS_AMT_PL_4_1_1,
                   SSFILE.SS_NO_PL_4_1_2,
                   SSFILE.SS_AMT_PL_4_1_2,
                   SSFILE.SS_UNITS_4_2_1,
                   SSFILE.SS_OUTBAL_4_2_1,
                   SSFILE.SS_UNITS_4_2_2,
                   SSFILE.SS_OUTBAL_4_2_2,
                   SSFILE.SS_UNITS_4_2_3,
                   SSFILE.SS_OUTBAL_4_2_3,
                   SSFILE.SS_UNITS_4_2_4,
                   SSFILE.SS_OUTBAL_4_2_4,
                   SSFILE.SS_UNITS_4_2_5,
                   SSFILE.SS_OUTBAL_4_2_5,
                   SSFILE.SS_NO_PL_4_2_1,
                   SSFILE.SS_AMT_PL_4_2_1,
                   SSFILE.SS_NO_PL_4_2_2,
                   SSFILE.SS_AMT_PL_4_2_2,
                   SSFILE.SS_UNITS_5_1_1,
                   SSFILE.SS_OUTBAL_5_1_1,
                   SSFILE.SS_UNITS_5_1_2,
                   SSFILE.SS_OUTBAL_5_1_2,
                   SSFILE.SS_UNITS_5_1_3,
                   SSFILE.SS_OUTBAL_5_1_3,
                   SSFILE.SS_UNITS_5_1_4,
                   SSFILE.SS_OUTBAL_5_1_4,
                   SSFILE.SS_UNITS_5_1_5,
                   SSFILE.SS_OUTBAL_5_1_5,
                   SSFILE.SS_NO_PL_5_1_1,
                   SSFILE.SS_AMT_PL_5_1_1,
                   SSFILE.SS_NO_PL_5_1_2,
                   SSFILE.SS_AMT_PL_5_1_2,
                   SSFILE.SS_UNITS_5_2_1,
                   SSFILE.SS_OUTBAL_5_2_1,
                   SSFILE.SS_UNITS_5_2_2,
                   SSFILE.SS_OUTBAL_5_2_2,
                   SSFILE.SS_UNITS_5_2_3,
                   SSFILE.SS_OUTBAL_5_2_3,
                   SSFILE.SS_UNITS_5_2_4,
                   SSFILE.SS_OUTBAL_5_2_4,
                   SSFILE.SS_UNITS_5_2_5,
                   SSFILE.SS_OUTBAL_5_2_5,
                   SSFILE.SS_NO_PL_5_2_1,
                   SSFILE.SS_AMT_PL_5_2_1,
                   SSFILE.SS_NO_PL_5_2_2,
                   SSFILE.SS_AMT_PL_5_2_2,
                   SSFILE.SS_UNITS_6_1_1,
                   SSFILE.SS_OUTBAL_6_1_1,
                   SSFILE.SS_UNITS_6_1_2,
                   SSFILE.SS_OUTBAL_6_1_2,
                   SSFILE.SS_UNITS_6_1_3,
                   SSFILE.SS_OUTBAL_6_1_3,
                   SSFILE.SS_UNITS_6_1_4,
                   SSFILE.SS_OUTBAL_6_1_4,
                   SSFILE.SS_UNITS_6_1_5,
                   SSFILE.SS_OUTBAL_6_1_5,
                   SSFILE.SS_NO_PL_6_1_1,
                   SSFILE.SS_AMT_PL_6_1_1,
                   SSFILE.SS_NO_PL_6_1_2,
                   SSFILE.SS_AMT_PL_6_1_2,
                   SSFILE.SS_UNITS_6_2_1,
                   SSFILE.SS_OUTBAL_6_2_1,
                   SSFILE.SS_UNITS_6_2_2,
                   SSFILE.SS_OUTBAL_6_2_2,
                   SSFILE.SS_UNITS_6_2_3,
                   SSFILE.SS_OUTBAL_6_2_3,
                   SSFILE.SS_UNITS_6_2_4,
                   SSFILE.SS_OUTBAL_6_2_4,
                   SSFILE.SS_UNITS_6_2_5,
                   SSFILE.SS_OUTBAL_6_2_5,
                   SSFILE.SS_NO_PL_6_2_1,
                   SSFILE.SS_AMT_PL_6_2_1,
                   SSFILE.SS_NO_PL_6_2_2,
                   SSFILE.SS_AMT_PL_6_2_2,
                   SSFILE.SS_UNITS_7_1_1,
                   SSFILE.SS_OUTBAL_7_1_1,
                   SSFILE.SS_UNITS_7_1_2,
                   SSFILE.SS_OUTBAL_7_1_2,
                   SSFILE.SS_UNITS_7_1_3,
                   SSFILE.SS_OUTBAL_7_1_3,
                   SSFILE.SS_UNITS_7_1_4,
                   SSFILE.SS_OUTBAL_7_1_4,
                   SSFILE.SS_UNITS_7_1_5,
                   SSFILE.SS_OUTBAL_7_1_5,
                   SSFILE.SS_NO_PL_7_1_1,
                   SSFILE.SS_AMT_PL_7_1_1,
                   SSFILE.SS_NO_PL_7_1_2,
                   SSFILE.SS_AMT_PL_7_1_2,
                   SSFILE.SS_UNITS_7_2_1,
                   SSFILE.SS_OUTBAL_7_2_1,
                   SSFILE.SS_UNITS_7_2_2,
                   SSFILE.SS_OUTBAL_7_2_2,
                   SSFILE.SS_UNITS_7_2_3,
                   SSFILE.SS_OUTBAL_7_2_3,
                   SSFILE.SS_UNITS_7_2_4,
                   SSFILE.SS_OUTBAL_7_2_4,
                   SSFILE.SS_UNITS_7_2_5,
                   SSFILE.SS_OUTBAL_7_2_5,
                   SSFILE.SS_NO_PL_7_2_1,
                   SSFILE.SS_AMT_PL_7_2_1,
                   SSFILE.SS_NO_PL_7_2_2,
                   SSFILE.SS_AMT_PL_7_2_2,
                   SSFILE.SS_UNITS_8_1_1,
                   SSFILE.SS_OUTBAL_8_1_1,
                   SSFILE.SS_UNITS_8_1_2,
                   SSFILE.SS_OUTBAL_8_1_2,
                   SSFILE.SS_UNITS_8_1_3,
                   SSFILE.SS_OUTBAL_8_1_3,
                   SSFILE.SS_UNITS_8_1_4,
                   SSFILE.SS_OUTBAL_8_1_4,
                   SSFILE.SS_UNITS_8_1_5,
                   SSFILE.SS_OUTBAL_8_1_5,
                   SSFILE.SS_NO_PL_8_1_1,
                   SSFILE.SS_AMT_PL_8_1_1,
                   SSFILE.SS_NO_PL_8_1_2,
                   SSFILE.SS_AMT_PL_8_1_2,
                   SSFILE.SS_UNITS_8_2_1,
                   SSFILE.SS_OUTBAL_8_2_1,
                   SSFILE.SS_UNITS_8_2_2,
                   SSFILE.SS_OUTBAL_8_2_2,
                   SSFILE.SS_UNITS_8_2_3,
                   SSFILE.SS_OUTBAL_8_2_3,
                   SSFILE.SS_UNITS_8_2_4,
                   SSFILE.SS_OUTBAL_8_2_4,
                   SSFILE.SS_UNITS_8_2_5,
                   SSFILE.SS_OUTBAL_8_2_5,
                   SSFILE.SS_NO_PL_8_2_1,
                   SSFILE.SS_AMT_PL_8_2_1,
                   SSFILE.SS_NO_PL_8_2_2,
                   SSFILE.SS_AMT_PL_8_2_2
            FROM DBO.SSFILE
            WHERE SSFILE.SS_DATE_YYYYMM >= :QSS1-WBEG-DATE
              AND SSFILE.SS_DATE_YYYYMM <= :QSS1-WEND-DATE
              AND SSFILE.SS_BRNO >= :QSS1-WBEG-BRNO
              AND SSFILE.SS_BRNO <= :QSS1-WEND-BRNO
              AND SSFILE.SS_CLASS >= :QSS1-WBEG-CLASS
              AND SSFILE.SS_CLASS <= :QSS1-WEND-CLASS
            ORDER BY SSFILE.SS_DATE_YYYYMM,
                     SSFILE.SS_BRNO,
                     SSFILE.SS_CLASS
           END-EXEC.

           EXEC SQL
               OPEN SS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QSS1-WBEG-DATE
                          QSS1-WEND-DATE
                          QSS1-WBEG-BRNO
                          QSS1-WEND-BRNO
                          QSS1-WBEG-CLASS
                          QSS1-WEND-CLASS.

           MOVE STAT---GOOD TO SS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SS1-FILE.
           PERFORM READ-IT.
           MOVE SS-SELECT-PATH TO E-FILE.
           MOVE SS1-KEY TO E-KEYX.

           MOVE SS-DATE-YYYYMM TO QSS-DATE-YYYYMM.
           MOVE SS-BRNO        TO QSS-BRNO.
           MOVE SS-CLASS       TO QSS-CLASS.

           EXEC SQL
            SELECT SSFILE.SS_DATE_YYYYMM,
                   SSFILE.SS_BRNO,
                   SSFILE.SS_CLASS,
                   ISNULL(CAST(SSFILE.SS_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   SSFILE.SS_UNITS_1_1_1,
                   SSFILE.SS_OUTBAL_1_1_1,
                   SSFILE.SS_UNITS_1_1_2,
                   SSFILE.SS_OUTBAL_1_1_2,
                   SSFILE.SS_UNITS_1_1_3,
                   SSFILE.SS_OUTBAL_1_1_3,
                   SSFILE.SS_UNITS_1_1_4,
                   SSFILE.SS_OUTBAL_1_1_4,
                   SSFILE.SS_UNITS_1_1_5,
                   SSFILE.SS_OUTBAL_1_1_5,
                   SSFILE.SS_NO_PL_1_1_1,
                   SSFILE.SS_AMT_PL_1_1_1,
                   SSFILE.SS_NO_PL_1_1_2,
                   SSFILE.SS_AMT_PL_1_1_2,
                   SSFILE.SS_UNITS_1_2_1,
                   SSFILE.SS_OUTBAL_1_2_1,
                   SSFILE.SS_UNITS_1_2_2,
                   SSFILE.SS_OUTBAL_1_2_2,
                   SSFILE.SS_UNITS_1_2_3,
                   SSFILE.SS_OUTBAL_1_2_3,
                   SSFILE.SS_UNITS_1_2_4,
                   SSFILE.SS_OUTBAL_1_2_4,
                   SSFILE.SS_UNITS_1_2_5,
                   SSFILE.SS_OUTBAL_1_2_5,
                   SSFILE.SS_NO_PL_1_2_1,
                   SSFILE.SS_AMT_PL_1_2_1,
                   SSFILE.SS_NO_PL_1_2_2,
                   SSFILE.SS_AMT_PL_1_2_2,
                   SSFILE.SS_UNITS_2_1_1,
                   SSFILE.SS_OUTBAL_2_1_1,
                   SSFILE.SS_UNITS_2_1_2,
                   SSFILE.SS_OUTBAL_2_1_2,
                   SSFILE.SS_UNITS_2_1_3,
                   SSFILE.SS_OUTBAL_2_1_3,
                   SSFILE.SS_UNITS_2_1_4,
                   SSFILE.SS_OUTBAL_2_1_4,
                   SSFILE.SS_UNITS_2_1_5,
                   SSFILE.SS_OUTBAL_2_1_5,
                   SSFILE.SS_NO_PL_2_1_1,
                   SSFILE.SS_AMT_PL_2_1_1,
                   SSFILE.SS_NO_PL_2_1_2,
                   SSFILE.SS_AMT_PL_2_1_2,
                   SSFILE.SS_UNITS_2_2_1,
                   SSFILE.SS_OUTBAL_2_2_1,
                   SSFILE.SS_UNITS_2_2_2,
                   SSFILE.SS_OUTBAL_2_2_2,
                   SSFILE.SS_UNITS_2_2_3,
                   SSFILE.SS_OUTBAL_2_2_3,
                   SSFILE.SS_UNITS_2_2_4,
                   SSFILE.SS_OUTBAL_2_2_4,
                   SSFILE.SS_UNITS_2_2_5,
                   SSFILE.SS_OUTBAL_2_2_5,
                   SSFILE.SS_NO_PL_2_2_1,
                   SSFILE.SS_AMT_PL_2_2_1,
                   SSFILE.SS_NO_PL_2_2_2,
                   SSFILE.SS_AMT_PL_2_2_2,
                   SSFILE.SS_UNITS_3_1_1,
                   SSFILE.SS_OUTBAL_3_1_1,
                   SSFILE.SS_UNITS_3_1_2,
                   SSFILE.SS_OUTBAL_3_1_2,
                   SSFILE.SS_UNITS_3_1_3,
                   SSFILE.SS_OUTBAL_3_1_3,
                   SSFILE.SS_UNITS_3_1_4,
                   SSFILE.SS_OUTBAL_3_1_4,
                   SSFILE.SS_UNITS_3_1_5,
                   SSFILE.SS_OUTBAL_3_1_5,
                   SSFILE.SS_NO_PL_3_1_1,
                   SSFILE.SS_AMT_PL_3_1_1,
                   SSFILE.SS_NO_PL_3_1_2,
                   SSFILE.SS_AMT_PL_3_1_2,
                   SSFILE.SS_UNITS_3_2_1,
                   SSFILE.SS_OUTBAL_3_2_1,
                   SSFILE.SS_UNITS_3_2_2,
                   SSFILE.SS_OUTBAL_3_2_2,
                   SSFILE.SS_UNITS_3_2_3,
                   SSFILE.SS_OUTBAL_3_2_3,
                   SSFILE.SS_UNITS_3_2_4,
                   SSFILE.SS_OUTBAL_3_2_4,
                   SSFILE.SS_UNITS_3_2_5,
                   SSFILE.SS_OUTBAL_3_2_5,
                   SSFILE.SS_NO_PL_3_2_1,
                   SSFILE.SS_AMT_PL_3_2_1,
                   SSFILE.SS_NO_PL_3_2_2,
                   SSFILE.SS_AMT_PL_3_2_2,
                   SSFILE.SS_UNITS_4_1_1,
                   SSFILE.SS_OUTBAL_4_1_1,
                   SSFILE.SS_UNITS_4_1_2,
                   SSFILE.SS_OUTBAL_4_1_2,
                   SSFILE.SS_UNITS_4_1_3,
                   SSFILE.SS_OUTBAL_4_1_3,
                   SSFILE.SS_UNITS_4_1_4,
                   SSFILE.SS_OUTBAL_4_1_4,
                   SSFILE.SS_UNITS_4_1_5,
                   SSFILE.SS_OUTBAL_4_1_5,
                   SSFILE.SS_NO_PL_4_1_1,
                   SSFILE.SS_AMT_PL_4_1_1,
                   SSFILE.SS_NO_PL_4_1_2,
                   SSFILE.SS_AMT_PL_4_1_2,
                   SSFILE.SS_UNITS_4_2_1,
                   SSFILE.SS_OUTBAL_4_2_1,
                   SSFILE.SS_UNITS_4_2_2,
                   SSFILE.SS_OUTBAL_4_2_2,
                   SSFILE.SS_UNITS_4_2_3,
                   SSFILE.SS_OUTBAL_4_2_3,
                   SSFILE.SS_UNITS_4_2_4,
                   SSFILE.SS_OUTBAL_4_2_4,
                   SSFILE.SS_UNITS_4_2_5,
                   SSFILE.SS_OUTBAL_4_2_5,
                   SSFILE.SS_NO_PL_4_2_1,
                   SSFILE.SS_AMT_PL_4_2_1,
                   SSFILE.SS_NO_PL_4_2_2,
                   SSFILE.SS_AMT_PL_4_2_2,
                   SSFILE.SS_UNITS_5_1_1,
                   SSFILE.SS_OUTBAL_5_1_1,
                   SSFILE.SS_UNITS_5_1_2,
                   SSFILE.SS_OUTBAL_5_1_2,
                   SSFILE.SS_UNITS_5_1_3,
                   SSFILE.SS_OUTBAL_5_1_3,
                   SSFILE.SS_UNITS_5_1_4,
                   SSFILE.SS_OUTBAL_5_1_4,
                   SSFILE.SS_UNITS_5_1_5,
                   SSFILE.SS_OUTBAL_5_1_5,
                   SSFILE.SS_NO_PL_5_1_1,
                   SSFILE.SS_AMT_PL_5_1_1,
                   SSFILE.SS_NO_PL_5_1_2,
                   SSFILE.SS_AMT_PL_5_1_2,
                   SSFILE.SS_UNITS_5_2_1,
                   SSFILE.SS_OUTBAL_5_2_1,
                   SSFILE.SS_UNITS_5_2_2,
                   SSFILE.SS_OUTBAL_5_2_2,
                   SSFILE.SS_UNITS_5_2_3,
                   SSFILE.SS_OUTBAL_5_2_3,
                   SSFILE.SS_UNITS_5_2_4,
                   SSFILE.SS_OUTBAL_5_2_4,
                   SSFILE.SS_UNITS_5_2_5,
                   SSFILE.SS_OUTBAL_5_2_5,
                   SSFILE.SS_NO_PL_5_2_1,
                   SSFILE.SS_AMT_PL_5_2_1,
                   SSFILE.SS_NO_PL_5_2_2,
                   SSFILE.SS_AMT_PL_5_2_2,
                   SSFILE.SS_UNITS_6_1_1,
                   SSFILE.SS_OUTBAL_6_1_1,
                   SSFILE.SS_UNITS_6_1_2,
                   SSFILE.SS_OUTBAL_6_1_2,
                   SSFILE.SS_UNITS_6_1_3,
                   SSFILE.SS_OUTBAL_6_1_3,
                   SSFILE.SS_UNITS_6_1_4,
                   SSFILE.SS_OUTBAL_6_1_4,
                   SSFILE.SS_UNITS_6_1_5,
                   SSFILE.SS_OUTBAL_6_1_5,
                   SSFILE.SS_NO_PL_6_1_1,
                   SSFILE.SS_AMT_PL_6_1_1,
                   SSFILE.SS_NO_PL_6_1_2,
                   SSFILE.SS_AMT_PL_6_1_2,
                   SSFILE.SS_UNITS_6_2_1,
                   SSFILE.SS_OUTBAL_6_2_1,
                   SSFILE.SS_UNITS_6_2_2,
                   SSFILE.SS_OUTBAL_6_2_2,
                   SSFILE.SS_UNITS_6_2_3,
                   SSFILE.SS_OUTBAL_6_2_3,
                   SSFILE.SS_UNITS_6_2_4,
                   SSFILE.SS_OUTBAL_6_2_4,
                   SSFILE.SS_UNITS_6_2_5,
                   SSFILE.SS_OUTBAL_6_2_5,
                   SSFILE.SS_NO_PL_6_2_1,
                   SSFILE.SS_AMT_PL_6_2_1,
                   SSFILE.SS_NO_PL_6_2_2,
                   SSFILE.SS_AMT_PL_6_2_2,
                   SSFILE.SS_UNITS_7_1_1,
                   SSFILE.SS_OUTBAL_7_1_1,
                   SSFILE.SS_UNITS_7_1_2,
                   SSFILE.SS_OUTBAL_7_1_2,
                   SSFILE.SS_UNITS_7_1_3,
                   SSFILE.SS_OUTBAL_7_1_3,
                   SSFILE.SS_UNITS_7_1_4,
                   SSFILE.SS_OUTBAL_7_1_4,
                   SSFILE.SS_UNITS_7_1_5,
                   SSFILE.SS_OUTBAL_7_1_5,
                   SSFILE.SS_NO_PL_7_1_1,
                   SSFILE.SS_AMT_PL_7_1_1,
                   SSFILE.SS_NO_PL_7_1_2,
                   SSFILE.SS_AMT_PL_7_1_2,
                   SSFILE.SS_UNITS_7_2_1,
                   SSFILE.SS_OUTBAL_7_2_1,
                   SSFILE.SS_UNITS_7_2_2,
                   SSFILE.SS_OUTBAL_7_2_2,
                   SSFILE.SS_UNITS_7_2_3,
                   SSFILE.SS_OUTBAL_7_2_3,
                   SSFILE.SS_UNITS_7_2_4,
                   SSFILE.SS_OUTBAL_7_2_4,
                   SSFILE.SS_UNITS_7_2_5,
                   SSFILE.SS_OUTBAL_7_2_5,
                   SSFILE.SS_NO_PL_7_2_1,
                   SSFILE.SS_AMT_PL_7_2_1,
                   SSFILE.SS_NO_PL_7_2_2,
                   SSFILE.SS_AMT_PL_7_2_2,
                   SSFILE.SS_UNITS_8_1_1,
                   SSFILE.SS_OUTBAL_8_1_1,
                   SSFILE.SS_UNITS_8_1_2,
                   SSFILE.SS_OUTBAL_8_1_2,
                   SSFILE.SS_UNITS_8_1_3,
                   SSFILE.SS_OUTBAL_8_1_3,
                   SSFILE.SS_UNITS_8_1_4,
                   SSFILE.SS_OUTBAL_8_1_4,
                   SSFILE.SS_UNITS_8_1_5,
                   SSFILE.SS_OUTBAL_8_1_5,
                   SSFILE.SS_NO_PL_8_1_1,
                   SSFILE.SS_AMT_PL_8_1_1,
                   SSFILE.SS_NO_PL_8_1_2,
                   SSFILE.SS_AMT_PL_8_1_2,
                   SSFILE.SS_UNITS_8_2_1,
                   SSFILE.SS_OUTBAL_8_2_1,
                   SSFILE.SS_UNITS_8_2_2,
                   SSFILE.SS_OUTBAL_8_2_2,
                   SSFILE.SS_UNITS_8_2_3,
                   SSFILE.SS_OUTBAL_8_2_3,
                   SSFILE.SS_UNITS_8_2_4,
                   SSFILE.SS_OUTBAL_8_2_4,
                   SSFILE.SS_UNITS_8_2_5,
                   SSFILE.SS_OUTBAL_8_2_5,
                   SSFILE.SS_NO_PL_8_2_1,
                   SSFILE.SS_AMT_PL_8_2_1,
                   SSFILE.SS_NO_PL_8_2_2,
                   SSFILE.SS_AMT_PL_8_2_2
            INTO :QSS-REC
            FROM DBO.SSFILE
            WHERE SSFILE.SS_DATE_YYYYMM = :QSS-DATE-YYYYMM
              AND SSFILE.SS_BRNO = :QSS-BRNO
              AND SSFILE.SS_CLASS = :QSS-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SS1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SS-FIELDS.

      *-----------------------------------------------------------------
       READ-SS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SS-SELECT-PATH TO E-FILE.
           MOVE SS1-KEY TO E-KEYX.
           INITIALIZE QSS-REC.

           IF ( SS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SS1_CURSOR INTO :QSS-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-SS-FIELDS.

      *-----------------------------------------------------------------
       WRITE-SS1-FILE.

           ACCEPT SS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE SS-SELECT-PATH TO E-FILE.
           MOVE SS1-KEY TO E-KEYX.
           PERFORM SET-SS-FIELDS.

           EXEC SQL
            INSERT INTO DBO.SSFILE
             (SSFILE.SS_DATE_YYYYMM,
              SSFILE.SS_BRNO,
              SSFILE.SS_CLASS,
              SSFILE.SS_LTOUCH_DATE,
              SSFILE.SS_UNITS_1_1_1,
              SSFILE.SS_OUTBAL_1_1_1,
              SSFILE.SS_UNITS_1_1_2,
              SSFILE.SS_OUTBAL_1_1_2,
              SSFILE.SS_UNITS_1_1_3,
              SSFILE.SS_OUTBAL_1_1_3,
              SSFILE.SS_UNITS_1_1_4,
              SSFILE.SS_OUTBAL_1_1_4,
              SSFILE.SS_UNITS_1_1_5,
              SSFILE.SS_OUTBAL_1_1_5,
              SSFILE.SS_NO_PL_1_1_1,
              SSFILE.SS_AMT_PL_1_1_1,
              SSFILE.SS_NO_PL_1_1_2,
              SSFILE.SS_AMT_PL_1_1_2,
              SSFILE.SS_UNITS_1_2_1,
              SSFILE.SS_OUTBAL_1_2_1,
              SSFILE.SS_UNITS_1_2_2,
              SSFILE.SS_OUTBAL_1_2_2,
              SSFILE.SS_UNITS_1_2_3,
              SSFILE.SS_OUTBAL_1_2_3,
              SSFILE.SS_UNITS_1_2_4,
              SSFILE.SS_OUTBAL_1_2_4,
              SSFILE.SS_UNITS_1_2_5,
              SSFILE.SS_OUTBAL_1_2_5,
              SSFILE.SS_NO_PL_1_2_1,
              SSFILE.SS_AMT_PL_1_2_1,
              SSFILE.SS_NO_PL_1_2_2,
              SSFILE.SS_AMT_PL_1_2_2,
              SSFILE.SS_UNITS_2_1_1,
              SSFILE.SS_OUTBAL_2_1_1,
              SSFILE.SS_UNITS_2_1_2,
              SSFILE.SS_OUTBAL_2_1_2,
              SSFILE.SS_UNITS_2_1_3,
              SSFILE.SS_OUTBAL_2_1_3,
              SSFILE.SS_UNITS_2_1_4,
              SSFILE.SS_OUTBAL_2_1_4,
              SSFILE.SS_UNITS_2_1_5,
              SSFILE.SS_OUTBAL_2_1_5,
              SSFILE.SS_NO_PL_2_1_1,
              SSFILE.SS_AMT_PL_2_1_1,
              SSFILE.SS_NO_PL_2_1_2,
              SSFILE.SS_AMT_PL_2_1_2,
              SSFILE.SS_UNITS_2_2_1,
              SSFILE.SS_OUTBAL_2_2_1,
              SSFILE.SS_UNITS_2_2_2,
              SSFILE.SS_OUTBAL_2_2_2,
              SSFILE.SS_UNITS_2_2_3,
              SSFILE.SS_OUTBAL_2_2_3,
              SSFILE.SS_UNITS_2_2_4,
              SSFILE.SS_OUTBAL_2_2_4,
              SSFILE.SS_UNITS_2_2_5,
              SSFILE.SS_OUTBAL_2_2_5,
              SSFILE.SS_NO_PL_2_2_1,
              SSFILE.SS_AMT_PL_2_2_1,
              SSFILE.SS_NO_PL_2_2_2,
              SSFILE.SS_AMT_PL_2_2_2,
              SSFILE.SS_UNITS_3_1_1,
              SSFILE.SS_OUTBAL_3_1_1,
              SSFILE.SS_UNITS_3_1_2,
              SSFILE.SS_OUTBAL_3_1_2,
              SSFILE.SS_UNITS_3_1_3,
              SSFILE.SS_OUTBAL_3_1_3,
              SSFILE.SS_UNITS_3_1_4,
              SSFILE.SS_OUTBAL_3_1_4,
              SSFILE.SS_UNITS_3_1_5,
              SSFILE.SS_OUTBAL_3_1_5,
              SSFILE.SS_NO_PL_3_1_1,
              SSFILE.SS_AMT_PL_3_1_1,
              SSFILE.SS_NO_PL_3_1_2,
              SSFILE.SS_AMT_PL_3_1_2,
              SSFILE.SS_UNITS_3_2_1,
              SSFILE.SS_OUTBAL_3_2_1,
              SSFILE.SS_UNITS_3_2_2,
              SSFILE.SS_OUTBAL_3_2_2,
              SSFILE.SS_UNITS_3_2_3,
              SSFILE.SS_OUTBAL_3_2_3,
              SSFILE.SS_UNITS_3_2_4,
              SSFILE.SS_OUTBAL_3_2_4,
              SSFILE.SS_UNITS_3_2_5,
              SSFILE.SS_OUTBAL_3_2_5,
              SSFILE.SS_NO_PL_3_2_1,
              SSFILE.SS_AMT_PL_3_2_1,
              SSFILE.SS_NO_PL_3_2_2,
              SSFILE.SS_AMT_PL_3_2_2,
              SSFILE.SS_UNITS_4_1_1,
              SSFILE.SS_OUTBAL_4_1_1,
              SSFILE.SS_UNITS_4_1_2,
              SSFILE.SS_OUTBAL_4_1_2,
              SSFILE.SS_UNITS_4_1_3,
              SSFILE.SS_OUTBAL_4_1_3,
              SSFILE.SS_UNITS_4_1_4,
              SSFILE.SS_OUTBAL_4_1_4,
              SSFILE.SS_UNITS_4_1_5,
              SSFILE.SS_OUTBAL_4_1_5,
              SSFILE.SS_NO_PL_4_1_1,
              SSFILE.SS_AMT_PL_4_1_1,
              SSFILE.SS_NO_PL_4_1_2,
              SSFILE.SS_AMT_PL_4_1_2,
              SSFILE.SS_UNITS_4_2_1,
              SSFILE.SS_OUTBAL_4_2_1,
              SSFILE.SS_UNITS_4_2_2,
              SSFILE.SS_OUTBAL_4_2_2,
              SSFILE.SS_UNITS_4_2_3,
              SSFILE.SS_OUTBAL_4_2_3,
              SSFILE.SS_UNITS_4_2_4,
              SSFILE.SS_OUTBAL_4_2_4,
              SSFILE.SS_UNITS_4_2_5,
              SSFILE.SS_OUTBAL_4_2_5,
              SSFILE.SS_NO_PL_4_2_1,
              SSFILE.SS_AMT_PL_4_2_1,
              SSFILE.SS_NO_PL_4_2_2,
              SSFILE.SS_AMT_PL_4_2_2,
              SSFILE.SS_UNITS_5_1_1,
              SSFILE.SS_OUTBAL_5_1_1,
              SSFILE.SS_UNITS_5_1_2,
              SSFILE.SS_OUTBAL_5_1_2,
              SSFILE.SS_UNITS_5_1_3,
              SSFILE.SS_OUTBAL_5_1_3,
              SSFILE.SS_UNITS_5_1_4,
              SSFILE.SS_OUTBAL_5_1_4,
              SSFILE.SS_UNITS_5_1_5,
              SSFILE.SS_OUTBAL_5_1_5,
              SSFILE.SS_NO_PL_5_1_1,
              SSFILE.SS_AMT_PL_5_1_1,
              SSFILE.SS_NO_PL_5_1_2,
              SSFILE.SS_AMT_PL_5_1_2,
              SSFILE.SS_UNITS_5_2_1,
              SSFILE.SS_OUTBAL_5_2_1,
              SSFILE.SS_UNITS_5_2_2,
              SSFILE.SS_OUTBAL_5_2_2,
              SSFILE.SS_UNITS_5_2_3,
              SSFILE.SS_OUTBAL_5_2_3,
              SSFILE.SS_UNITS_5_2_4,
              SSFILE.SS_OUTBAL_5_2_4,
              SSFILE.SS_UNITS_5_2_5,
              SSFILE.SS_OUTBAL_5_2_5,
              SSFILE.SS_NO_PL_5_2_1,
              SSFILE.SS_AMT_PL_5_2_1,
              SSFILE.SS_NO_PL_5_2_2,
              SSFILE.SS_AMT_PL_5_2_2,
              SSFILE.SS_UNITS_6_1_1,
              SSFILE.SS_OUTBAL_6_1_1,
              SSFILE.SS_UNITS_6_1_2,
              SSFILE.SS_OUTBAL_6_1_2,
              SSFILE.SS_UNITS_6_1_3,
              SSFILE.SS_OUTBAL_6_1_3,
              SSFILE.SS_UNITS_6_1_4,
              SSFILE.SS_OUTBAL_6_1_4,
              SSFILE.SS_UNITS_6_1_5,
              SSFILE.SS_OUTBAL_6_1_5,
              SSFILE.SS_NO_PL_6_1_1,
              SSFILE.SS_AMT_PL_6_1_1,
              SSFILE.SS_NO_PL_6_1_2,
              SSFILE.SS_AMT_PL_6_1_2,
              SSFILE.SS_UNITS_6_2_1,
              SSFILE.SS_OUTBAL_6_2_1,
              SSFILE.SS_UNITS_6_2_2,
              SSFILE.SS_OUTBAL_6_2_2,
              SSFILE.SS_UNITS_6_2_3,
              SSFILE.SS_OUTBAL_6_2_3,
              SSFILE.SS_UNITS_6_2_4,
              SSFILE.SS_OUTBAL_6_2_4,
              SSFILE.SS_UNITS_6_2_5,
              SSFILE.SS_OUTBAL_6_2_5,
              SSFILE.SS_NO_PL_6_2_1,
              SSFILE.SS_AMT_PL_6_2_1,
              SSFILE.SS_NO_PL_6_2_2,
              SSFILE.SS_AMT_PL_6_2_2,
              SSFILE.SS_UNITS_7_1_1,
              SSFILE.SS_OUTBAL_7_1_1,
              SSFILE.SS_UNITS_7_1_2,
              SSFILE.SS_OUTBAL_7_1_2,
              SSFILE.SS_UNITS_7_1_3,
              SSFILE.SS_OUTBAL_7_1_3,
              SSFILE.SS_UNITS_7_1_4,
              SSFILE.SS_OUTBAL_7_1_4,
              SSFILE.SS_UNITS_7_1_5,
              SSFILE.SS_OUTBAL_7_1_5,
              SSFILE.SS_NO_PL_7_1_1,
              SSFILE.SS_AMT_PL_7_1_1,
              SSFILE.SS_NO_PL_7_1_2,
              SSFILE.SS_AMT_PL_7_1_2,
              SSFILE.SS_UNITS_7_2_1,
              SSFILE.SS_OUTBAL_7_2_1,
              SSFILE.SS_UNITS_7_2_2,
              SSFILE.SS_OUTBAL_7_2_2,
              SSFILE.SS_UNITS_7_2_3,
              SSFILE.SS_OUTBAL_7_2_3,
              SSFILE.SS_UNITS_7_2_4,
              SSFILE.SS_OUTBAL_7_2_4,
              SSFILE.SS_UNITS_7_2_5,
              SSFILE.SS_OUTBAL_7_2_5,
              SSFILE.SS_NO_PL_7_2_1,
              SSFILE.SS_AMT_PL_7_2_1,
              SSFILE.SS_NO_PL_7_2_2,
              SSFILE.SS_AMT_PL_7_2_2,
              SSFILE.SS_UNITS_8_1_1,
              SSFILE.SS_OUTBAL_8_1_1,
              SSFILE.SS_UNITS_8_1_2,
              SSFILE.SS_OUTBAL_8_1_2,
              SSFILE.SS_UNITS_8_1_3,
              SSFILE.SS_OUTBAL_8_1_3,
              SSFILE.SS_UNITS_8_1_4,
              SSFILE.SS_OUTBAL_8_1_4,
              SSFILE.SS_UNITS_8_1_5,
              SSFILE.SS_OUTBAL_8_1_5,
              SSFILE.SS_NO_PL_8_1_1,
              SSFILE.SS_AMT_PL_8_1_1,
              SSFILE.SS_NO_PL_8_1_2,
              SSFILE.SS_AMT_PL_8_1_2,
              SSFILE.SS_UNITS_8_2_1,
              SSFILE.SS_OUTBAL_8_2_1,
              SSFILE.SS_UNITS_8_2_2,
              SSFILE.SS_OUTBAL_8_2_2,
              SSFILE.SS_UNITS_8_2_3,
              SSFILE.SS_OUTBAL_8_2_3,
              SSFILE.SS_UNITS_8_2_4,
              SSFILE.SS_OUTBAL_8_2_4,
              SSFILE.SS_UNITS_8_2_5,
              SSFILE.SS_OUTBAL_8_2_5,
              SSFILE.SS_NO_PL_8_2_1,
              SSFILE.SS_AMT_PL_8_2_1,
              SSFILE.SS_NO_PL_8_2_2,
              SSFILE.SS_AMT_PL_8_2_2)
            VALUES
             (:QSS-DATE-YYYYMM,
              :QSS-BRNO,
              :QSS-CLASS,
              NULLIF(:QSS-LTOUCH-DATE,'1900-01-01'),
              :QSS-UNITS-1-1-1,
              :QSS-OUTBAL-1-1-1,
              :QSS-UNITS-1-1-2,
              :QSS-OUTBAL-1-1-2,
              :QSS-UNITS-1-1-3,
              :QSS-OUTBAL-1-1-3,
              :QSS-UNITS-1-1-4,
              :QSS-OUTBAL-1-1-4,
              :QSS-UNITS-1-1-5,
              :QSS-OUTBAL-1-1-5,
              :QSS-NO-PL-1-1-1,
              :QSS-AMT-PL-1-1-1,
              :QSS-NO-PL-1-1-2,
              :QSS-AMT-PL-1-1-2,
              :QSS-UNITS-1-2-1,
              :QSS-OUTBAL-1-2-1,
              :QSS-UNITS-1-2-2,
              :QSS-OUTBAL-1-2-2,
              :QSS-UNITS-1-2-3,
              :QSS-OUTBAL-1-2-3,
              :QSS-UNITS-1-2-4,
              :QSS-OUTBAL-1-2-4,
              :QSS-UNITS-1-2-5,
              :QSS-OUTBAL-1-2-5,
              :QSS-NO-PL-1-2-1,
              :QSS-AMT-PL-1-2-1,
              :QSS-NO-PL-1-2-2,
              :QSS-AMT-PL-1-2-2,
              :QSS-UNITS-2-1-1,
              :QSS-OUTBAL-2-1-1,
              :QSS-UNITS-2-1-2,
              :QSS-OUTBAL-2-1-2,
              :QSS-UNITS-2-1-3,
              :QSS-OUTBAL-2-1-3,
              :QSS-UNITS-2-1-4,
              :QSS-OUTBAL-2-1-4,
              :QSS-UNITS-2-1-5,
              :QSS-OUTBAL-2-1-5,
              :QSS-NO-PL-2-1-1,
              :QSS-AMT-PL-2-1-1,
              :QSS-NO-PL-2-1-2,
              :QSS-AMT-PL-2-1-2,
              :QSS-UNITS-2-2-1,
              :QSS-OUTBAL-2-2-1,
              :QSS-UNITS-2-2-2,
              :QSS-OUTBAL-2-2-2,
              :QSS-UNITS-2-2-3,
              :QSS-OUTBAL-2-2-3,
              :QSS-UNITS-2-2-4,
              :QSS-OUTBAL-2-2-4,
              :QSS-UNITS-2-2-5,
              :QSS-OUTBAL-2-2-5,
              :QSS-NO-PL-2-2-1,
              :QSS-AMT-PL-2-2-1,
              :QSS-NO-PL-2-2-2,
              :QSS-AMT-PL-2-2-2,
              :QSS-UNITS-3-1-1,
              :QSS-OUTBAL-3-1-1,
              :QSS-UNITS-3-1-2,
              :QSS-OUTBAL-3-1-2,
              :QSS-UNITS-3-1-3,
              :QSS-OUTBAL-3-1-3,
              :QSS-UNITS-3-1-4,
              :QSS-OUTBAL-3-1-4,
              :QSS-UNITS-3-1-5,
              :QSS-OUTBAL-3-1-5,
              :QSS-NO-PL-3-1-1,
              :QSS-AMT-PL-3-1-1,
              :QSS-NO-PL-3-1-2,
              :QSS-AMT-PL-3-1-2,
              :QSS-UNITS-3-2-1,
              :QSS-OUTBAL-3-2-1,
              :QSS-UNITS-3-2-2,
              :QSS-OUTBAL-3-2-2,
              :QSS-UNITS-3-2-3,
              :QSS-OUTBAL-3-2-3,
              :QSS-UNITS-3-2-4,
              :QSS-OUTBAL-3-2-4,
              :QSS-UNITS-3-2-5,
              :QSS-OUTBAL-3-2-5,
              :QSS-NO-PL-3-2-1,
              :QSS-AMT-PL-3-2-1,
              :QSS-NO-PL-3-2-2,
              :QSS-AMT-PL-3-2-2,
              :QSS-UNITS-4-1-1,
              :QSS-OUTBAL-4-1-1,
              :QSS-UNITS-4-1-2,
              :QSS-OUTBAL-4-1-2,
              :QSS-UNITS-4-1-3,
              :QSS-OUTBAL-4-1-3,
              :QSS-UNITS-4-1-4,
              :QSS-OUTBAL-4-1-4,
              :QSS-UNITS-4-1-5,
              :QSS-OUTBAL-4-1-5,
              :QSS-NO-PL-4-1-1,
              :QSS-AMT-PL-4-1-1,
              :QSS-NO-PL-4-1-2,
              :QSS-AMT-PL-4-1-2,
              :QSS-UNITS-4-2-1,
              :QSS-OUTBAL-4-2-1,
              :QSS-UNITS-4-2-2,
              :QSS-OUTBAL-4-2-2,
              :QSS-UNITS-4-2-3,
              :QSS-OUTBAL-4-2-3,
              :QSS-UNITS-4-2-4,
              :QSS-OUTBAL-4-2-4,
              :QSS-UNITS-4-2-5,
              :QSS-OUTBAL-4-2-5,
              :QSS-NO-PL-4-2-1,
              :QSS-AMT-PL-4-2-1,
              :QSS-NO-PL-4-2-2,
              :QSS-AMT-PL-4-2-2,
              :QSS-UNITS-5-1-1,
              :QSS-OUTBAL-5-1-1,
              :QSS-UNITS-5-1-2,
              :QSS-OUTBAL-5-1-2,
              :QSS-UNITS-5-1-3,
              :QSS-OUTBAL-5-1-3,
              :QSS-UNITS-5-1-4,
              :QSS-OUTBAL-5-1-4,
              :QSS-UNITS-5-1-5,
              :QSS-OUTBAL-5-1-5,
              :QSS-NO-PL-5-1-1,
              :QSS-AMT-PL-5-1-1,
              :QSS-NO-PL-5-1-2,
              :QSS-AMT-PL-5-1-2,
              :QSS-UNITS-5-2-1,
              :QSS-OUTBAL-5-2-1,
              :QSS-UNITS-5-2-2,
              :QSS-OUTBAL-5-2-2,
              :QSS-UNITS-5-2-3,
              :QSS-OUTBAL-5-2-3,
              :QSS-UNITS-5-2-4,
              :QSS-OUTBAL-5-2-4,
              :QSS-UNITS-5-2-5,
              :QSS-OUTBAL-5-2-5,
              :QSS-NO-PL-5-2-1,
              :QSS-AMT-PL-5-2-1,
              :QSS-NO-PL-5-2-2,
              :QSS-AMT-PL-5-2-2,
              :QSS-UNITS-6-1-1,
              :QSS-OUTBAL-6-1-1,
              :QSS-UNITS-6-1-2,
              :QSS-OUTBAL-6-1-2,
              :QSS-UNITS-6-1-3,
              :QSS-OUTBAL-6-1-3,
              :QSS-UNITS-6-1-4,
              :QSS-OUTBAL-6-1-4,
              :QSS-UNITS-6-1-5,
              :QSS-OUTBAL-6-1-5,
              :QSS-NO-PL-6-1-1,
              :QSS-AMT-PL-6-1-1,
              :QSS-NO-PL-6-1-2,
              :QSS-AMT-PL-6-1-2,
              :QSS-UNITS-6-2-1,
              :QSS-OUTBAL-6-2-1,
              :QSS-UNITS-6-2-2,
              :QSS-OUTBAL-6-2-2,
              :QSS-UNITS-6-2-3,
              :QSS-OUTBAL-6-2-3,
              :QSS-UNITS-6-2-4,
              :QSS-OUTBAL-6-2-4,
              :QSS-UNITS-6-2-5,
              :QSS-OUTBAL-6-2-5,
              :QSS-NO-PL-6-2-1,
              :QSS-AMT-PL-6-2-1,
              :QSS-NO-PL-6-2-2,
              :QSS-AMT-PL-6-2-2,
              :QSS-UNITS-7-1-1,
              :QSS-OUTBAL-7-1-1,
              :QSS-UNITS-7-1-2,
              :QSS-OUTBAL-7-1-2,
              :QSS-UNITS-7-1-3,
              :QSS-OUTBAL-7-1-3,
              :QSS-UNITS-7-1-4,
              :QSS-OUTBAL-7-1-4,
              :QSS-UNITS-7-1-5,
              :QSS-OUTBAL-7-1-5,
              :QSS-NO-PL-7-1-1,
              :QSS-AMT-PL-7-1-1,
              :QSS-NO-PL-7-1-2,
              :QSS-AMT-PL-7-1-2,
              :QSS-UNITS-7-2-1,
              :QSS-OUTBAL-7-2-1,
              :QSS-UNITS-7-2-2,
              :QSS-OUTBAL-7-2-2,
              :QSS-UNITS-7-2-3,
              :QSS-OUTBAL-7-2-3,
              :QSS-UNITS-7-2-4,
              :QSS-OUTBAL-7-2-4,
              :QSS-UNITS-7-2-5,
              :QSS-OUTBAL-7-2-5,
              :QSS-NO-PL-7-2-1,
              :QSS-AMT-PL-7-2-1,
              :QSS-NO-PL-7-2-2,
              :QSS-AMT-PL-7-2-2,
              :QSS-UNITS-8-1-1,
              :QSS-OUTBAL-8-1-1,
              :QSS-UNITS-8-1-2,
              :QSS-OUTBAL-8-1-2,
              :QSS-UNITS-8-1-3,
              :QSS-OUTBAL-8-1-3,
              :QSS-UNITS-8-1-4,
              :QSS-OUTBAL-8-1-4,
              :QSS-UNITS-8-1-5,
              :QSS-OUTBAL-8-1-5,
              :QSS-NO-PL-8-1-1,
              :QSS-AMT-PL-8-1-1,
              :QSS-NO-PL-8-1-2,
              :QSS-AMT-PL-8-1-2,
              :QSS-UNITS-8-2-1,
              :QSS-OUTBAL-8-2-1,
              :QSS-UNITS-8-2-2,
              :QSS-OUTBAL-8-2-2,
              :QSS-UNITS-8-2-3,
              :QSS-OUTBAL-8-2-3,
              :QSS-UNITS-8-2-4,
              :QSS-OUTBAL-8-2-4,
              :QSS-UNITS-8-2-5,
              :QSS-OUTBAL-8-2-5,
              :QSS-NO-PL-8-2-1,
              :QSS-AMT-PL-8-2-1,
              :QSS-NO-PL-8-2-2,
              :QSS-AMT-PL-8-2-2)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-SS1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-SS1-FILE.

           ACCEPT SS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE SS-SELECT-PATH TO E-FILE.
           MOVE SS1-KEY TO E-KEYX.
           PERFORM SET-SS-FIELDS.

           EXEC SQL
            UPDATE DBO.SSFILE
             SET
              SSFILE.SS_DATE_YYYYMM = :QSS-DATE-YYYYMM,
              SSFILE.SS_BRNO = :QSS-BRNO,
              SSFILE.SS_CLASS = :QSS-CLASS,
              SSFILE.SS_LTOUCH_DATE =
                 NULLIF(:QSS-LTOUCH-DATE,'1900-01-01'),
              SSFILE.SS_UNITS_1_1_1 = :QSS-UNITS-1-1-1,
              SSFILE.SS_OUTBAL_1_1_1 = :QSS-OUTBAL-1-1-1,
              SSFILE.SS_UNITS_1_1_2 = :QSS-UNITS-1-1-2,
              SSFILE.SS_OUTBAL_1_1_2 = :QSS-OUTBAL-1-1-2,
              SSFILE.SS_UNITS_1_1_3 = :QSS-UNITS-1-1-3,
              SSFILE.SS_OUTBAL_1_1_3 = :QSS-OUTBAL-1-1-3,
              SSFILE.SS_UNITS_1_1_4 = :QSS-UNITS-1-1-4,
              SSFILE.SS_OUTBAL_1_1_4 = :QSS-OUTBAL-1-1-4,
              SSFILE.SS_UNITS_1_1_5 = :QSS-UNITS-1-1-5,
              SSFILE.SS_OUTBAL_1_1_5 = :QSS-OUTBAL-1-1-5,
              SSFILE.SS_NO_PL_1_1_1 = :QSS-NO-PL-1-1-1,
              SSFILE.SS_AMT_PL_1_1_1 = :QSS-AMT-PL-1-1-1,
              SSFILE.SS_NO_PL_1_1_2 = :QSS-NO-PL-1-1-2,
              SSFILE.SS_AMT_PL_1_1_2 = :QSS-AMT-PL-1-1-2,
              SSFILE.SS_UNITS_1_2_1 = :QSS-UNITS-1-2-1,
              SSFILE.SS_OUTBAL_1_2_1 = :QSS-OUTBAL-1-2-1,
              SSFILE.SS_UNITS_1_2_2 = :QSS-UNITS-1-2-2,
              SSFILE.SS_OUTBAL_1_2_2 = :QSS-OUTBAL-1-2-2,
              SSFILE.SS_UNITS_1_2_3 = :QSS-UNITS-1-2-3,
              SSFILE.SS_OUTBAL_1_2_3 = :QSS-OUTBAL-1-2-3,
              SSFILE.SS_UNITS_1_2_4 = :QSS-UNITS-1-2-4,
              SSFILE.SS_OUTBAL_1_2_4 = :QSS-OUTBAL-1-2-4,
              SSFILE.SS_UNITS_1_2_5 = :QSS-UNITS-1-2-5,
              SSFILE.SS_OUTBAL_1_2_5 = :QSS-OUTBAL-1-2-5,
              SSFILE.SS_NO_PL_1_2_1 = :QSS-NO-PL-1-2-1,
              SSFILE.SS_AMT_PL_1_2_1 = :QSS-AMT-PL-1-2-1,
              SSFILE.SS_NO_PL_1_2_2 = :QSS-NO-PL-1-2-2,
              SSFILE.SS_AMT_PL_1_2_2 = :QSS-AMT-PL-1-2-2,
              SSFILE.SS_UNITS_2_1_1 = :QSS-UNITS-2-1-1,
              SSFILE.SS_OUTBAL_2_1_1 = :QSS-OUTBAL-2-1-1,
              SSFILE.SS_UNITS_2_1_2 = :QSS-UNITS-2-1-2,
              SSFILE.SS_OUTBAL_2_1_2 = :QSS-OUTBAL-2-1-2,
              SSFILE.SS_UNITS_2_1_3 = :QSS-UNITS-2-1-3,
              SSFILE.SS_OUTBAL_2_1_3 = :QSS-OUTBAL-2-1-3,
              SSFILE.SS_UNITS_2_1_4 = :QSS-UNITS-2-1-4,
              SSFILE.SS_OUTBAL_2_1_4 = :QSS-OUTBAL-2-1-4,
              SSFILE.SS_UNITS_2_1_5 = :QSS-UNITS-2-1-5,
              SSFILE.SS_OUTBAL_2_1_5 = :QSS-OUTBAL-2-1-5,
              SSFILE.SS_NO_PL_2_1_1 = :QSS-NO-PL-2-1-1,
              SSFILE.SS_AMT_PL_2_1_1 = :QSS-AMT-PL-2-1-1,
              SSFILE.SS_NO_PL_2_1_2 = :QSS-NO-PL-2-1-2,
              SSFILE.SS_AMT_PL_2_1_2 = :QSS-AMT-PL-2-1-2,
              SSFILE.SS_UNITS_2_2_1 = :QSS-UNITS-2-2-1,
              SSFILE.SS_OUTBAL_2_2_1 = :QSS-OUTBAL-2-2-1,
              SSFILE.SS_UNITS_2_2_2 = :QSS-UNITS-2-2-2,
              SSFILE.SS_OUTBAL_2_2_2 = :QSS-OUTBAL-2-2-2,
              SSFILE.SS_UNITS_2_2_3 = :QSS-UNITS-2-2-3,
              SSFILE.SS_OUTBAL_2_2_3 = :QSS-OUTBAL-2-2-3,
              SSFILE.SS_UNITS_2_2_4 = :QSS-UNITS-2-2-4,
              SSFILE.SS_OUTBAL_2_2_4 = :QSS-OUTBAL-2-2-4,
              SSFILE.SS_UNITS_2_2_5 = :QSS-UNITS-2-2-5,
              SSFILE.SS_OUTBAL_2_2_5 = :QSS-OUTBAL-2-2-5,
              SSFILE.SS_NO_PL_2_2_1 = :QSS-NO-PL-2-2-1,
              SSFILE.SS_AMT_PL_2_2_1 = :QSS-AMT-PL-2-2-1,
              SSFILE.SS_NO_PL_2_2_2 = :QSS-NO-PL-2-2-2,
              SSFILE.SS_AMT_PL_2_2_2 = :QSS-AMT-PL-2-2-2,
              SSFILE.SS_UNITS_3_1_1 = :QSS-UNITS-3-1-1,
              SSFILE.SS_OUTBAL_3_1_1 = :QSS-OUTBAL-3-1-1,
              SSFILE.SS_UNITS_3_1_2 = :QSS-UNITS-3-1-2,
              SSFILE.SS_OUTBAL_3_1_2 = :QSS-OUTBAL-3-1-2,
              SSFILE.SS_UNITS_3_1_3 = :QSS-UNITS-3-1-3,
              SSFILE.SS_OUTBAL_3_1_3 = :QSS-OUTBAL-3-1-3,
              SSFILE.SS_UNITS_3_1_4 = :QSS-UNITS-3-1-4,
              SSFILE.SS_OUTBAL_3_1_4 = :QSS-OUTBAL-3-1-4,
              SSFILE.SS_UNITS_3_1_5 = :QSS-UNITS-3-1-5,
              SSFILE.SS_OUTBAL_3_1_5 = :QSS-OUTBAL-3-1-5,
              SSFILE.SS_NO_PL_3_1_1 = :QSS-NO-PL-3-1-1,
              SSFILE.SS_AMT_PL_3_1_1 = :QSS-AMT-PL-3-1-1,
              SSFILE.SS_NO_PL_3_1_2 = :QSS-NO-PL-3-1-2,
              SSFILE.SS_AMT_PL_3_1_2 = :QSS-AMT-PL-3-1-2,
              SSFILE.SS_UNITS_3_2_1 = :QSS-UNITS-3-2-1,
              SSFILE.SS_OUTBAL_3_2_1 = :QSS-OUTBAL-3-2-1,
              SSFILE.SS_UNITS_3_2_2 = :QSS-UNITS-3-2-2,
              SSFILE.SS_OUTBAL_3_2_2 = :QSS-OUTBAL-3-2-2,
              SSFILE.SS_UNITS_3_2_3 = :QSS-UNITS-3-2-3,
              SSFILE.SS_OUTBAL_3_2_3 = :QSS-OUTBAL-3-2-3,
              SSFILE.SS_UNITS_3_2_4 = :QSS-UNITS-3-2-4,
              SSFILE.SS_OUTBAL_3_2_4 = :QSS-OUTBAL-3-2-4,
              SSFILE.SS_UNITS_3_2_5 = :QSS-UNITS-3-2-5,
              SSFILE.SS_OUTBAL_3_2_5 = :QSS-OUTBAL-3-2-5,
              SSFILE.SS_NO_PL_3_2_1 = :QSS-NO-PL-3-2-1,
              SSFILE.SS_AMT_PL_3_2_1 = :QSS-AMT-PL-3-2-1,
              SSFILE.SS_NO_PL_3_2_2 = :QSS-NO-PL-3-2-2,
              SSFILE.SS_AMT_PL_3_2_2 = :QSS-AMT-PL-3-2-2,
              SSFILE.SS_UNITS_4_1_1 = :QSS-UNITS-4-1-1,
              SSFILE.SS_OUTBAL_4_1_1 = :QSS-OUTBAL-4-1-1,
              SSFILE.SS_UNITS_4_1_2 = :QSS-UNITS-4-1-2,
              SSFILE.SS_OUTBAL_4_1_2 = :QSS-OUTBAL-4-1-2,
              SSFILE.SS_UNITS_4_1_3 = :QSS-UNITS-4-1-3,
              SSFILE.SS_OUTBAL_4_1_3 = :QSS-OUTBAL-4-1-3,
              SSFILE.SS_UNITS_4_1_4 = :QSS-UNITS-4-1-4,
              SSFILE.SS_OUTBAL_4_1_4 = :QSS-OUTBAL-4-1-4,
              SSFILE.SS_UNITS_4_1_5 = :QSS-UNITS-4-1-5,
              SSFILE.SS_OUTBAL_4_1_5 = :QSS-OUTBAL-4-1-5,
              SSFILE.SS_NO_PL_4_1_1 = :QSS-NO-PL-4-1-1,
              SSFILE.SS_AMT_PL_4_1_1 = :QSS-AMT-PL-4-1-1,
              SSFILE.SS_NO_PL_4_1_2 = :QSS-NO-PL-4-1-2,
              SSFILE.SS_AMT_PL_4_1_2 = :QSS-AMT-PL-4-1-2,
              SSFILE.SS_UNITS_4_2_1 = :QSS-UNITS-4-2-1,
              SSFILE.SS_OUTBAL_4_2_1 = :QSS-OUTBAL-4-2-1,
              SSFILE.SS_UNITS_4_2_2 = :QSS-UNITS-4-2-2,
              SSFILE.SS_OUTBAL_4_2_2 = :QSS-OUTBAL-4-2-2,
              SSFILE.SS_UNITS_4_2_3 = :QSS-UNITS-4-2-3,
              SSFILE.SS_OUTBAL_4_2_3 = :QSS-OUTBAL-4-2-3,
              SSFILE.SS_UNITS_4_2_4 = :QSS-UNITS-4-2-4,
              SSFILE.SS_OUTBAL_4_2_4 = :QSS-OUTBAL-4-2-4,
              SSFILE.SS_UNITS_4_2_5 = :QSS-UNITS-4-2-5,
              SSFILE.SS_OUTBAL_4_2_5 = :QSS-OUTBAL-4-2-5,
              SSFILE.SS_NO_PL_4_2_1 = :QSS-NO-PL-4-2-1,
              SSFILE.SS_AMT_PL_4_2_1 = :QSS-AMT-PL-4-2-1,
              SSFILE.SS_NO_PL_4_2_2 = :QSS-NO-PL-4-2-2,
              SSFILE.SS_AMT_PL_4_2_2 = :QSS-AMT-PL-4-2-2,
              SSFILE.SS_UNITS_5_1_1 = :QSS-UNITS-5-1-1,
              SSFILE.SS_OUTBAL_5_1_1 = :QSS-OUTBAL-5-1-1,
              SSFILE.SS_UNITS_5_1_2 = :QSS-UNITS-5-1-2,
              SSFILE.SS_OUTBAL_5_1_2 = :QSS-OUTBAL-5-1-2,
              SSFILE.SS_UNITS_5_1_3 = :QSS-UNITS-5-1-3,
              SSFILE.SS_OUTBAL_5_1_3 = :QSS-OUTBAL-5-1-3,
              SSFILE.SS_UNITS_5_1_4 = :QSS-UNITS-5-1-4,
              SSFILE.SS_OUTBAL_5_1_4 = :QSS-OUTBAL-5-1-4,
              SSFILE.SS_UNITS_5_1_5 = :QSS-UNITS-5-1-5,
              SSFILE.SS_OUTBAL_5_1_5 = :QSS-OUTBAL-5-1-5,
              SSFILE.SS_NO_PL_5_1_1 = :QSS-NO-PL-5-1-1,
              SSFILE.SS_AMT_PL_5_1_1 = :QSS-AMT-PL-5-1-1,
              SSFILE.SS_NO_PL_5_1_2 = :QSS-NO-PL-5-1-2,
              SSFILE.SS_AMT_PL_5_1_2 = :QSS-AMT-PL-5-1-2,
              SSFILE.SS_UNITS_5_2_1 = :QSS-UNITS-5-2-1,
              SSFILE.SS_OUTBAL_5_2_1 = :QSS-OUTBAL-5-2-1,
              SSFILE.SS_UNITS_5_2_2 = :QSS-UNITS-5-2-2,
              SSFILE.SS_OUTBAL_5_2_2 = :QSS-OUTBAL-5-2-2,
              SSFILE.SS_UNITS_5_2_3 = :QSS-UNITS-5-2-3,
              SSFILE.SS_OUTBAL_5_2_3 = :QSS-OUTBAL-5-2-3,
              SSFILE.SS_UNITS_5_2_4 = :QSS-UNITS-5-2-4,
              SSFILE.SS_OUTBAL_5_2_4 = :QSS-OUTBAL-5-2-4,
              SSFILE.SS_UNITS_5_2_5 = :QSS-UNITS-5-2-5,
              SSFILE.SS_OUTBAL_5_2_5 = :QSS-OUTBAL-5-2-5,
              SSFILE.SS_NO_PL_5_2_1 = :QSS-NO-PL-5-2-1,
              SSFILE.SS_AMT_PL_5_2_1 = :QSS-AMT-PL-5-2-1,
              SSFILE.SS_NO_PL_5_2_2 = :QSS-NO-PL-5-2-2,
              SSFILE.SS_AMT_PL_5_2_2 = :QSS-AMT-PL-5-2-2,
              SSFILE.SS_UNITS_6_1_1 = :QSS-UNITS-6-1-1,
              SSFILE.SS_OUTBAL_6_1_1 = :QSS-OUTBAL-6-1-1,
              SSFILE.SS_UNITS_6_1_2 = :QSS-UNITS-6-1-2,
              SSFILE.SS_OUTBAL_6_1_2 = :QSS-OUTBAL-6-1-2,
              SSFILE.SS_UNITS_6_1_3 = :QSS-UNITS-6-1-3,
              SSFILE.SS_OUTBAL_6_1_3 = :QSS-OUTBAL-6-1-3,
              SSFILE.SS_UNITS_6_1_4 = :QSS-UNITS-6-1-4,
              SSFILE.SS_OUTBAL_6_1_4 = :QSS-OUTBAL-6-1-4,
              SSFILE.SS_UNITS_6_1_5 = :QSS-UNITS-6-1-5,
              SSFILE.SS_OUTBAL_6_1_5 = :QSS-OUTBAL-6-1-5,
              SSFILE.SS_NO_PL_6_1_1 = :QSS-NO-PL-6-1-1,
              SSFILE.SS_AMT_PL_6_1_1 = :QSS-AMT-PL-6-1-1,
              SSFILE.SS_NO_PL_6_1_2 = :QSS-NO-PL-6-1-2,
              SSFILE.SS_AMT_PL_6_1_2 = :QSS-AMT-PL-6-1-2,
              SSFILE.SS_UNITS_6_2_1 = :QSS-UNITS-6-2-1,
              SSFILE.SS_OUTBAL_6_2_1 = :QSS-OUTBAL-6-2-1,
              SSFILE.SS_UNITS_6_2_2 = :QSS-UNITS-6-2-2,
              SSFILE.SS_OUTBAL_6_2_2 = :QSS-OUTBAL-6-2-2,
              SSFILE.SS_UNITS_6_2_3 = :QSS-UNITS-6-2-3,
              SSFILE.SS_OUTBAL_6_2_3 = :QSS-OUTBAL-6-2-3,
              SSFILE.SS_UNITS_6_2_4 = :QSS-UNITS-6-2-4,
              SSFILE.SS_OUTBAL_6_2_4 = :QSS-OUTBAL-6-2-4,
              SSFILE.SS_UNITS_6_2_5 = :QSS-UNITS-6-2-5,
              SSFILE.SS_OUTBAL_6_2_5 = :QSS-OUTBAL-6-2-5,
              SSFILE.SS_NO_PL_6_2_1 = :QSS-NO-PL-6-2-1,
              SSFILE.SS_AMT_PL_6_2_1 = :QSS-AMT-PL-6-2-1,
              SSFILE.SS_NO_PL_6_2_2 = :QSS-NO-PL-6-2-2,
              SSFILE.SS_AMT_PL_6_2_2 = :QSS-AMT-PL-6-2-2,
              SSFILE.SS_UNITS_7_1_1 = :QSS-UNITS-7-1-1,
              SSFILE.SS_OUTBAL_7_1_1 = :QSS-OUTBAL-7-1-1,
              SSFILE.SS_UNITS_7_1_2 = :QSS-UNITS-7-1-2,
              SSFILE.SS_OUTBAL_7_1_2 = :QSS-OUTBAL-7-1-2,
              SSFILE.SS_UNITS_7_1_3 = :QSS-UNITS-7-1-3,
              SSFILE.SS_OUTBAL_7_1_3 = :QSS-OUTBAL-7-1-3,
              SSFILE.SS_UNITS_7_1_4 = :QSS-UNITS-7-1-4,
              SSFILE.SS_OUTBAL_7_1_4 = :QSS-OUTBAL-7-1-4,
              SSFILE.SS_UNITS_7_1_5 = :QSS-UNITS-7-1-5,
              SSFILE.SS_OUTBAL_7_1_5 = :QSS-OUTBAL-7-1-5,
              SSFILE.SS_NO_PL_7_1_1 = :QSS-NO-PL-7-1-1,
              SSFILE.SS_AMT_PL_7_1_1 = :QSS-AMT-PL-7-1-1,
              SSFILE.SS_NO_PL_7_1_2 = :QSS-NO-PL-7-1-2,
              SSFILE.SS_AMT_PL_7_1_2 = :QSS-AMT-PL-7-1-2,
              SSFILE.SS_UNITS_7_2_1 = :QSS-UNITS-7-2-1,
              SSFILE.SS_OUTBAL_7_2_1 = :QSS-OUTBAL-7-2-1,
              SSFILE.SS_UNITS_7_2_2 = :QSS-UNITS-7-2-2,
              SSFILE.SS_OUTBAL_7_2_2 = :QSS-OUTBAL-7-2-2,
              SSFILE.SS_UNITS_7_2_3 = :QSS-UNITS-7-2-3,
              SSFILE.SS_OUTBAL_7_2_3 = :QSS-OUTBAL-7-2-3,
              SSFILE.SS_UNITS_7_2_4 = :QSS-UNITS-7-2-4,
              SSFILE.SS_OUTBAL_7_2_4 = :QSS-OUTBAL-7-2-4,
              SSFILE.SS_UNITS_7_2_5 = :QSS-UNITS-7-2-5,
              SSFILE.SS_OUTBAL_7_2_5 = :QSS-OUTBAL-7-2-5,
              SSFILE.SS_NO_PL_7_2_1 = :QSS-NO-PL-7-2-1,
              SSFILE.SS_AMT_PL_7_2_1 = :QSS-AMT-PL-7-2-1,
              SSFILE.SS_NO_PL_7_2_2 = :QSS-NO-PL-7-2-2,
              SSFILE.SS_AMT_PL_7_2_2 = :QSS-AMT-PL-7-2-2,
              SSFILE.SS_UNITS_8_1_1 = :QSS-UNITS-8-1-1,
              SSFILE.SS_OUTBAL_8_1_1 = :QSS-OUTBAL-8-1-1,
              SSFILE.SS_UNITS_8_1_2 = :QSS-UNITS-8-1-2,
              SSFILE.SS_OUTBAL_8_1_2 = :QSS-OUTBAL-8-1-2,
              SSFILE.SS_UNITS_8_1_3 = :QSS-UNITS-8-1-3,
              SSFILE.SS_OUTBAL_8_1_3 = :QSS-OUTBAL-8-1-3,
              SSFILE.SS_UNITS_8_1_4 = :QSS-UNITS-8-1-4,
              SSFILE.SS_OUTBAL_8_1_4 = :QSS-OUTBAL-8-1-4,
              SSFILE.SS_UNITS_8_1_5 = :QSS-UNITS-8-1-5,
              SSFILE.SS_OUTBAL_8_1_5 = :QSS-OUTBAL-8-1-5,
              SSFILE.SS_NO_PL_8_1_1 = :QSS-NO-PL-8-1-1,
              SSFILE.SS_AMT_PL_8_1_1 = :QSS-AMT-PL-8-1-1,
              SSFILE.SS_NO_PL_8_1_2 = :QSS-NO-PL-8-1-2,
              SSFILE.SS_AMT_PL_8_1_2 = :QSS-AMT-PL-8-1-2,
              SSFILE.SS_UNITS_8_2_1 = :QSS-UNITS-8-2-1,
              SSFILE.SS_OUTBAL_8_2_1 = :QSS-OUTBAL-8-2-1,
              SSFILE.SS_UNITS_8_2_2 = :QSS-UNITS-8-2-2,
              SSFILE.SS_OUTBAL_8_2_2 = :QSS-OUTBAL-8-2-2,
              SSFILE.SS_UNITS_8_2_3 = :QSS-UNITS-8-2-3,
              SSFILE.SS_OUTBAL_8_2_3 = :QSS-OUTBAL-8-2-3,
              SSFILE.SS_UNITS_8_2_4 = :QSS-UNITS-8-2-4,
              SSFILE.SS_OUTBAL_8_2_4 = :QSS-OUTBAL-8-2-4,
              SSFILE.SS_UNITS_8_2_5 = :QSS-UNITS-8-2-5,
              SSFILE.SS_OUTBAL_8_2_5 = :QSS-OUTBAL-8-2-5,
              SSFILE.SS_NO_PL_8_2_1 = :QSS-NO-PL-8-2-1,
              SSFILE.SS_AMT_PL_8_2_1 = :QSS-AMT-PL-8-2-1,
              SSFILE.SS_NO_PL_8_2_2 = :QSS-NO-PL-8-2-2,
              SSFILE.SS_AMT_PL_8_2_2 = :QSS-AMT-PL-8-2-2
            WHERE SSFILE.SS_DATE_YYYYMM = :QSS-DATE-YYYYMM
              AND SSFILE.SS_BRNO = :QSS-BRNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-SS1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-SS1-FILE.

           PERFORM DELETE-IT.
           MOVE SS-SELECT-PATH TO E-FILE.
           MOVE SS1-KEY TO E-KEYX.
           PERFORM SET-SS-FIELDS.

           EXEC SQL
            DELETE FROM DBO.SSFILE
            WHERE SSFILE.SS_DATE_YYYYMM = :QSS-DATE-YYYYMM
              AND SSFILE.SS_BRNO = :QSS-BRNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-SS1-FILE.

      *-----------------------------------------------------------------
       CLOSE-SS1-FILE.
           IF ( SS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SS1-CURSOR-STAT.

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

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IOSSIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
