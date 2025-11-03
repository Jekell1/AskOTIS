       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOEARN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  03-12-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOEARN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : EAFILE 
      * TABLE DESC    : EXTRACTION/ACTION FILE
      * OUTPUT FILE   : XPC/IOEARN.PRE
      * USED FOR      : LIBLP/LPWSEA.CPY  --- QEA-REC
      *                 LIBLP/LPEA1R.CPY  --- READ
      *                 LIBLP/LPEA1RN.CPY --- READ/READ-NEXT/START
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
           "@(#)IO/IOEARN.CBL S34 05/17/21-16:41:48 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01EA.CPY".
       COPY "LIBLP/LPWSEA.CPY".

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
       01  QEA1-WHERE-BEG-KEY.
           03  QEA1-WBEG-BRANCH         PIC 9(04).
           03  QEA1-WBEG-CLASS          PIC 9(03).
           03  QEA1-WBEG-GROUP-ID       PIC X(06).
           03  QEA1-WBEG-SEQNO          PIC 99.

       01  QEA1-WHERE-END-KEY.
           03  QEA1-WEND-BRANCH         PIC 9(04).
           03  QEA1-WEND-CLASS          PIC 9(03).
           03  QEA1-WEND-GROUP-ID       PIC X(06).
           03  QEA1-WEND-SEQNO          PIC 99.

       01  QEA-REC.
           03  QEA-BRANCH              PIC 9(4).
           03  QEA-CLASS               PIC 9(3).
           03  QEA-GROUP-ID            PIC X(6).
           03  QEA-SEQNO               PIC 99.
           03  QEA-LTOUCH-DATE         PIC X(10).
           03  QEA-DESC                PIC X(60).
           03  QEA-PASTDUE-DAY1        PIC 9(4).
           03  QEA-PASTDUE-DAY2        PIC 9(4).
           03  QEA-PASTDUE-AMT1        PIC 9(4)V99.
           03  QEA-PASTDUE-AMT2        PIC 9(4)V99.
           03  QEA-ACCT-BAL1           PIC 9(6)V99.
           03  QEA-ACCT-BAL2           PIC 9(6)V99.
           03  QEA-MATURED             PIC X.
           03  QEA-PROMISED            PIC X.
           03  QEA-ACTION              PIC X(4).
           03  QEA-LSTFM-DATE1         PIC X(10).
           03  QEA-LSTFM-DATE2         PIC X(10).
           03  QEA-LSTFM-NO1           PIC 9(4).
           03  QEA-LSTFM-NO2           PIC 9(4).
           03  QEA-PASTPROM-DAY1       PIC S9(5).
           03  QEA-PASTPROM-DAY2       PIC S9(5).
           03  QEA-RECENCY-DAY1        PIC 9(4).
           03  QEA-RECENCY-DAY2        PIC 9(4).
           03  QEA-ALPNAME1            PIC X(4).
           03  QEA-ALPNAME2            PIC X(4).
           03  QEA-RECENCYDATE         PIC X.
           03  QEA-DLNO1               PIC 9(4)V99.
           03  QEA-DLNO2               PIC 9(4)V99.
           03  QEA-CONTRACTUAL-DAY1    PIC S9(5).
           03  QEA-CONTRACTUAL-DAY2    PIC S9(5).
           03  QEA-SETFM-NO            PIC 9(4).
           03  QEA-SETACT-CD           PIC X(4).
           03  QEA-INC-CNT             PIC X.
           03  QEA-INC-PROMISED        PIC 9(2).
           03  QEA-SETFM-1             PIC 9(4).
           03  QEA-SETFM-2             PIC 9(4).
           03  QEA-SETFM-3             PIC 9(4).
           03  QEA-SET-LN-COL          PIC X.
           03  QEA-LN-COL-ID           PIC XXX.
           03  QEA-CQ-FG               PIC X.
           03  QEA-COL-POOL            PIC X(3).
           03  QEA-CPOTS               PIC X.
           03  QEA-REPO                PIC X.
           03  QEA-PL                  PIC X.
           03  QEA-BANKRUPT            PIC X.
           03  QEA-PL-IN-ACTIVE-BOTH   PIC X.
           03  QEA-CONFIDENTIAL-FG     PIC X.
           03  QEA-1ST-PMT-MISSES      PIC X.
           03  QEA-LEGAL               PIC X.
           03  QEA-DEBTAID             PIC X.
           03  QEA-JUDGEMENT           PIC X.
           03  QEA-BROKEN-PROM-FG      PIC X.
           03  QEA-PROM-BREAK1         PIC S9(5).
           03  QEA-PROM-BREAK2         PIC S9(5).
           03  QEA-ORIGIN              PIC X(4).
           03  QEA-PL-BEG-DATE         PIC X(10).
           03  QEA-PL-END-DATE         PIC X(10).
           03  QEA-RPOTS               PIC X.
           03  QEA-MISPAYS             PIC X.
           03  QEA-ALLOTMENTS          PIC X.
           03  QEA-LNDATE-DAYS1        PIC 9(4).
           03  QEA-LNDATE-DAYS2        PIC 9(4).
           03  QEA-MATURE-DAYS1        PIC 9(4).
           03  QEA-MATURE-DAYS2        PIC 9(4).
           03  QEA-DAY-OF-MO-DUE1      PIC 9(2).
           03  QEA-DAY-OF-MO-DUE2      PIC 9(2).
           03  QEA-INCL-COLLECTOR      PIC X(3).
           03  QEA-BORR-STATE          PIC X(2).

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

       01  LEGEND-BEG-BRANCH. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING BRANCH     (F3-ALL) ".

       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING BRANCH     ".

       01  LEGEND-BEG-CLASS. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING CLASS      (F3-ALL) ".

       01  LEGEND-END-CLASS.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING CLASS      ".

       01  LEGEND-BEG-GROUP-ID. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING GROUP-ID   (F3-ALL) ".

       01  LEGEND-END-GROUP-ID.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING GROUP-ID   ".

       01  LEGEND-BEG-SEQNO.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING SEQNO      (F3-ALL) ".

       01  LEGEND-END-SEQNO.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING SEQNO      ".

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
           03  BEG-CLASS               PIC 9(03).
           03  END-CLASS               PIC 9(03).
           03  BEG-GROUP-ID            PIC X(06).
           03  END-GROUP-ID            PIC X(06).
           03  BEG-SEQNO               PIC 9(02).
           03  END-SEQNO               PIC 9(02).

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
           03  FILLER PIC X(30) VALUE "BRANCH CLASS GROUP ID SEQNO   ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-EA-BRANCH                  PIC 9(04).
           03  FILLER                         PIC X(01).
           03  D01-EA-CLASS                   PIC 9(02).
           03  FILLER                         PIC X(01).
           03  D01-EA-GROUP-ID                PIC X(06).
           03  FILLER                         PIC X(01).
           03  D01-EA-SEQNO                   PIC 9(02).
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
       COPY "LIBIO/IOEARN_DEF.CPY".
       COPY "LIBIO/IOEARN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOEARN_SCN.CPY".

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
           MOVE "B"                         TO EA-PATH-OVERRIDE.

           MOVE BEG-BRANCH                  TO QEA1-WBEG-BRANCH.
           MOVE BEG-CLASS                   TO QEA1-WBEG-CLASS.
           MOVE BEG-GROUP-ID                TO QEA1-WBEG-GROUP-ID.
           MOVE BEG-SEQNO                   TO QEA1-WBEG-SEQNO.

           MOVE END-BRANCH                  TO QEA1-WEND-BRANCH.
           MOVE END-CLASS                   TO QEA1-WEND-CLASS.
           MOVE END-GROUP-ID                TO QEA1-WEND-GROUP-ID.
           MOVE END-SEQNO                   TO QEA1-WEND-SEQNO.

           PERFORM OPEN-EA1-FILE.
           PERFORM START-EA1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-EA1-FILE-NEXT.

           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EA-BRANCH < BEG-BRANCH )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( EA-BRANCH > END-BRANCH )
              GO TO MAIN-PROGRAM-END
           ELSE
           IF ( EA-CLASS < BEG-CLASS ) OR
              ( EA-CLASS > END-CLASS )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( EA-GROUP-ID < BEG-GROUP-ID ) OR
              ( EA-GROUP-ID > END-GROUP-ID )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( EA-SEQNO < BEG-SEQNO ) OR
              ( EA-SEQNO > END-SEQNO )
              GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EA-BRANCH                  TO D01-EA-BRANCH.
           MOVE EA-CLASS                   TO D01-EA-CLASS.
           MOVE EA-GROUP-ID                TO D01-EA-GROUP-ID.
           MOVE EA-SEQNO                   TO D01-EA-SEQNO.
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
           MOVE "IOEARN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOEARN-QUEUE-POS  TO Q-POS.
           MOVE IOEARN-COPIES-POS TO C-POS.
           MOVE IOEARN-PRU-POS    TO P-POS.
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
       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-BRANCH.

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
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           GO TO ENTER-BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE 0                    TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-CLASS.

           MOVE LEGEND-BEG-CLASS  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN030BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

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

           IF ( BEG-CLASS > END-CLASS )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           GO TO ENTER-BEG-GROUP-ID.

      *-----------------------------------------------------------------
       ENTER-ALL-CLASS.

           MOVE "SNNN030BEG-CLASS." TO VDU.
           MOVE 0                    TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-CLASS." TO VDU.
           MOVE 999                  TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-GROUP-ID.

           MOVE LEGEND-BEG-GROUP-ID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A060BEG-GROUP-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-GROUP-ID.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-GROUP-ID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-GROUP-ID.

      *-----------------------------------------------------------------
       ENTER-END-GROUP-ID.

           MOVE LEGEND-END-GROUP-ID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A060END-GROUP-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-GROUP-ID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-GROUP-ID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-GROUP-ID.

           IF ( BEG-GROUP-ID > END-GROUP-ID )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-GROUP-ID.

           GO TO ENTER-BEG-SEQNO.

      *-----------------------------------------------------------------
       ENTER-ALL-GROUP-ID.

           MOVE "S  A060BEG-GROUP-ID." TO VDU.
           MOVE SPACES               TO BEG-GROUP-ID VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A060END-GROUP-ID." TO VDU.
           MOVE ALL "Z"              TO END-GROUP-ID VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SEQNO.

           MOVE LEGEND-BEG-SEQNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020BEG-SEQNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-GROUP-ID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SEQNO.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SEQNO.

      *-----------------------------------------------------------------
       ENTER-END-SEQNO.

           MOVE LEGEND-END-SEQNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020END-SEQNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SEQNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SEQNO.

           IF ( BEG-SEQNO > END-SEQNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SEQNO.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-SEQNO.

           MOVE "SNNN020BEG-SEQNO." TO VDU.
           MOVE 0                    TO BEG-SEQNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-SEQNO." TO VDU.
           MOVE 99                   TO END-SEQNO VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SEQNO.
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

           MOVE "SNNN040BEG-BRANCH."    TO VDU.
           MOVE BEG-BRANCH              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH."    TO VDU.
           MOVE END-BRANCH              TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN030BEG-CLASS."    TO VDU.
           MOVE BEG-CLASS              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-CLASS."    TO VDU.
           MOVE END-CLASS              TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A060BEG-GROUP-ID."    TO VDU.
           MOVE BEG-GROUP-ID              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A060END-GROUP-ID."    TO VDU.
           MOVE END-GROUP-ID              TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020BEG-SEQNO."     TO VDU.
           MOVE BEG-SEQNO               TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-SEQNO."     TO VDU.
           MOVE END-SEQNO               TO VDUNUM0.
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

           MOVE EA1-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING BRANCH               : "    TO R01-PROMPT.
           MOVE BEG-BRANCH                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    BRANCH               : "    TO R01-PROMPT.
           MOVE END-BRANCH                             TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - EAFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOEARN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOEARN-SYSDATE.
           DISPLAY IOEARN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOEARN-FORM-FIELDS TO WR-BUF.
           MOVE IOEARN-FORM-LIST TO WR-LIST.
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

           MOVE IOEARN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOEARN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOEARN_EVA.CPY".
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
      *    LOAD EA-REC FROM QEA-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-EA-FIELDS.
           MOVE QEA-BRANCH TO EA-BRANCH.
           MOVE QEA-CLASS TO EA-CLASS.
           MOVE QEA-GROUP-ID TO EA-GROUP-ID.
           MOVE QEA-SEQNO TO EA-SEQNO.

           IF ( QEA-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO EA-LTOUCH-DATE
           ELSE
              MOVE QEA-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EA-LTOUCH-DATE.

           MOVE QEA-DESC TO EA-DESC.
           MOVE QEA-PASTDUE-DAY1 TO EA-PASTDUE-DAY1.
           MOVE QEA-PASTDUE-DAY2 TO EA-PASTDUE-DAY2.
           MOVE QEA-PASTDUE-AMT1 TO EA-PASTDUE-AMT1.
           MOVE QEA-PASTDUE-AMT2 TO EA-PASTDUE-AMT2.
           MOVE QEA-ACCT-BAL1 TO EA-ACCT-BAL1.
           MOVE QEA-ACCT-BAL2 TO EA-ACCT-BAL2.
           MOVE QEA-MATURED TO EA-MATURED.
           MOVE QEA-PROMISED TO EA-PROMISED.
           MOVE QEA-ACTION TO EA-ACTION.

           IF ( QEA-LSTFM-DATE1 = "1900-01-01" )
              MOVE ZEROES TO EA-LSTFM-DATE1
           ELSE
              MOVE QEA-LSTFM-DATE1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EA-LSTFM-DATE1.


           IF ( QEA-LSTFM-DATE2 = "1900-01-01" )
              MOVE ZEROES TO EA-LSTFM-DATE2
           ELSE
              MOVE QEA-LSTFM-DATE2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EA-LSTFM-DATE2.

           MOVE QEA-LSTFM-NO1 TO EA-LSTFM-NO1.
           MOVE QEA-LSTFM-NO2 TO EA-LSTFM-NO2.
           MOVE QEA-PASTPROM-DAY1 TO EA-PASTPROM-DAY1.
           MOVE QEA-PASTPROM-DAY2 TO EA-PASTPROM-DAY2.
           MOVE QEA-RECENCY-DAY1 TO EA-RECENCY-DAY1.
           MOVE QEA-RECENCY-DAY2 TO EA-RECENCY-DAY2.
           MOVE QEA-ALPNAME1 TO EA-ALPNAME1.
           MOVE QEA-ALPNAME2 TO EA-ALPNAME2.
           MOVE QEA-RECENCYDATE TO EA-RECENCYDATE.
           MOVE QEA-DLNO1 TO EA-DLNO1.
           MOVE QEA-DLNO2 TO EA-DLNO2.
           MOVE QEA-CONTRACTUAL-DAY1 TO EA-CONTRACTUAL-DAY1.
           MOVE QEA-CONTRACTUAL-DAY2 TO EA-CONTRACTUAL-DAY2.
           MOVE QEA-SETFM-NO TO EA-SETFM-NO.
           MOVE QEA-SETACT-CD TO EA-SETACT-CD.
           MOVE QEA-INC-CNT TO EA-INC-CNT.
           MOVE QEA-INC-PROMISED TO EA-INC-PROMISED.
           MOVE QEA-SETFM-1 TO EA-SETFM(1).
           MOVE QEA-SETFM-2 TO EA-SETFM(2).
           MOVE QEA-SETFM-3 TO EA-SETFM(3).
           MOVE QEA-SET-LN-COL TO EA-SET-LN-COL.
           MOVE QEA-LN-COL-ID TO EA-LN-COL-ID.
           MOVE QEA-CQ-FG TO EA-CQ-FG.
           MOVE QEA-COL-POOL TO EA-COL-POOL.
           MOVE QEA-CPOTS TO EA-CPOTS.
           MOVE QEA-REPO TO EA-REPO.
           MOVE QEA-PL TO EA-PL.
           MOVE QEA-BANKRUPT TO EA-BANKRUPT.
           MOVE QEA-PL-IN-ACTIVE-BOTH TO EA-PL-IN-ACTIVE-BOTH.
           MOVE QEA-CONFIDENTIAL-FG TO EA-CONFIDENTIAL-FG.
           MOVE QEA-1ST-PMT-MISSES TO EA-1ST-PMT-MISSES.
           MOVE QEA-LEGAL TO EA-LEGAL.
           MOVE QEA-DEBTAID TO EA-DEBTAID.
           MOVE QEA-JUDGEMENT TO EA-JUDGEMENT.
           MOVE QEA-BROKEN-PROM-FG TO EA-BROKEN-PROM-FG.
           MOVE QEA-PROM-BREAK1 TO EA-PROM-BREAK1.
           MOVE QEA-PROM-BREAK2 TO EA-PROM-BREAK2.
           MOVE QEA-ORIGIN TO EA-ORIGIN.

           IF ( QEA-PL-BEG-DATE = "1900-01-01" )
              MOVE ZEROES TO EA-PL-BEG-DATE
           ELSE
              MOVE QEA-PL-BEG-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EA-PL-BEG-DATE.


           IF ( QEA-PL-END-DATE = "1900-01-01" )
              MOVE ZEROES TO EA-PL-END-DATE
           ELSE
              MOVE QEA-PL-END-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EA-PL-END-DATE.

           MOVE QEA-RPOTS TO EA-RPOTS.
           MOVE QEA-MISPAYS TO EA-MISPAYS.
           MOVE QEA-ALLOTMENTS TO EA-ALLOTMENTS.
           MOVE QEA-LNDATE-DAYS1 TO EA-LNDATE-DAYS1.
           MOVE QEA-LNDATE-DAYS2 TO EA-LNDATE-DAYS2.
           MOVE QEA-MATURE-DAYS1 TO EA-MATURE-DAYS1.
           MOVE QEA-MATURE-DAYS2 TO EA-MATURE-DAYS2.
           MOVE QEA-DAY-OF-MO-DUE1 TO EA-DAY-OF-MO-DUE1.
           MOVE QEA-DAY-OF-MO-DUE2 TO EA-DAY-OF-MO-DUE2.
           MOVE QEA-INCL-COLLECTOR TO EA-INCL-COLLECTOR.
           MOVE QEA-BORR-STATE TO EA-BORR-STATE.
 
      *-----------------------------------------------------------------
      *    LOAD QEA-REC FROM EA-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-EA-FIELDS.
           MOVE EA-BRANCH TO QEA-BRANCH.
           MOVE EA-CLASS TO QEA-CLASS.
           MOVE EA-GROUP-ID TO QEA-GROUP-ID.
           MOVE EA-SEQNO TO QEA-SEQNO.

           IF ( EA-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QEA-LTOUCH-DATE
           ELSE
              MOVE EA-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEA-LTOUCH-DATE.

           MOVE EA-DESC TO QEA-DESC.
           MOVE EA-PASTDUE-DAY1 TO QEA-PASTDUE-DAY1.
           MOVE EA-PASTDUE-DAY2 TO QEA-PASTDUE-DAY2.
           MOVE EA-PASTDUE-AMT1 TO QEA-PASTDUE-AMT1.
           MOVE EA-PASTDUE-AMT2 TO QEA-PASTDUE-AMT2.
           MOVE EA-ACCT-BAL1 TO QEA-ACCT-BAL1.
           MOVE EA-ACCT-BAL2 TO QEA-ACCT-BAL2.
           MOVE EA-MATURED TO QEA-MATURED.
           MOVE EA-PROMISED TO QEA-PROMISED.
           MOVE EA-ACTION TO QEA-ACTION.

           IF ( EA-LSTFM-DATE1 = ZEROES )
              MOVE "1900-01-01" TO QEA-LSTFM-DATE1
           ELSE
              MOVE EA-LSTFM-DATE1 TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEA-LSTFM-DATE1.


           IF ( EA-LSTFM-DATE2 = ZEROES )
              MOVE "1900-01-01" TO QEA-LSTFM-DATE2
           ELSE
              MOVE EA-LSTFM-DATE2 TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEA-LSTFM-DATE2.

           MOVE EA-LSTFM-NO1 TO QEA-LSTFM-NO1.
           MOVE EA-LSTFM-NO2 TO QEA-LSTFM-NO2.
           MOVE EA-PASTPROM-DAY1 TO QEA-PASTPROM-DAY1.
           MOVE EA-PASTPROM-DAY2 TO QEA-PASTPROM-DAY2.
           MOVE EA-RECENCY-DAY1 TO QEA-RECENCY-DAY1.
           MOVE EA-RECENCY-DAY2 TO QEA-RECENCY-DAY2.
           MOVE EA-ALPNAME1 TO QEA-ALPNAME1.
           MOVE EA-ALPNAME2 TO QEA-ALPNAME2.
           MOVE EA-RECENCYDATE TO QEA-RECENCYDATE.
           MOVE EA-DLNO1 TO QEA-DLNO1.
           MOVE EA-DLNO2 TO QEA-DLNO2.
           MOVE EA-CONTRACTUAL-DAY1 TO QEA-CONTRACTUAL-DAY1.
           MOVE EA-CONTRACTUAL-DAY2 TO QEA-CONTRACTUAL-DAY2.
           MOVE EA-SETFM-NO TO QEA-SETFM-NO.
           MOVE EA-SETACT-CD TO QEA-SETACT-CD.
           MOVE EA-INC-CNT TO QEA-INC-CNT.
           MOVE EA-INC-PROMISED TO QEA-INC-PROMISED.
           MOVE EA-SETFM(1) TO QEA-SETFM-1.
           MOVE EA-SETFM(2) TO QEA-SETFM-2.
           MOVE EA-SETFM(3) TO QEA-SETFM-3.
           MOVE EA-SET-LN-COL TO QEA-SET-LN-COL.
           MOVE EA-LN-COL-ID TO QEA-LN-COL-ID.
           MOVE EA-CQ-FG TO QEA-CQ-FG.
           MOVE EA-COL-POOL TO QEA-COL-POOL.
           MOVE EA-CPOTS TO QEA-CPOTS.
           MOVE EA-REPO TO QEA-REPO.
           MOVE EA-PL TO QEA-PL.
           MOVE EA-BANKRUPT TO QEA-BANKRUPT.
           MOVE EA-PL-IN-ACTIVE-BOTH TO QEA-PL-IN-ACTIVE-BOTH.
           MOVE EA-CONFIDENTIAL-FG TO QEA-CONFIDENTIAL-FG.
           MOVE EA-1ST-PMT-MISSES TO QEA-1ST-PMT-MISSES.
           MOVE EA-LEGAL TO QEA-LEGAL.
           MOVE EA-DEBTAID TO QEA-DEBTAID.
           MOVE EA-JUDGEMENT TO QEA-JUDGEMENT.
           MOVE EA-BROKEN-PROM-FG TO QEA-BROKEN-PROM-FG.
           MOVE EA-PROM-BREAK1 TO QEA-PROM-BREAK1.
           MOVE EA-PROM-BREAK2 TO QEA-PROM-BREAK2.
           MOVE EA-ORIGIN TO QEA-ORIGIN.

           IF ( EA-PL-BEG-DATE = ZEROES )
              MOVE "1900-01-01" TO QEA-PL-BEG-DATE
           ELSE
              MOVE EA-PL-BEG-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEA-PL-BEG-DATE.


           IF ( EA-PL-END-DATE = ZEROES )
              MOVE "1900-01-01" TO QEA-PL-END-DATE
           ELSE
              MOVE EA-PL-END-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEA-PL-END-DATE.

           MOVE EA-RPOTS TO QEA-RPOTS.
           MOVE EA-MISPAYS TO QEA-MISPAYS.
           MOVE EA-ALLOTMENTS TO QEA-ALLOTMENTS.
           MOVE EA-LNDATE-DAYS1 TO QEA-LNDATE-DAYS1.
           MOVE EA-LNDATE-DAYS2 TO QEA-LNDATE-DAYS2.
           MOVE EA-MATURE-DAYS1 TO QEA-MATURE-DAYS1.
           MOVE EA-MATURE-DAYS2 TO QEA-MATURE-DAYS2.
           MOVE EA-DAY-OF-MO-DUE1 TO QEA-DAY-OF-MO-DUE1.
           MOVE EA-DAY-OF-MO-DUE2 TO QEA-DAY-OF-MO-DUE2.
           MOVE EA-INCL-COLLECTOR TO QEA-INCL-COLLECTOR.
           MOVE EA-BORR-STATE TO QEA-BORR-STATE.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBGB/GBEA1RN
       LOAD-EA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( EA-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( EA-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO EA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO EA-ALL-MACHINE
              MOVE EA-ALL-PATH             TO EA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( EA-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO EA-ALL-BASE
              MOVE FILPATH-MACHINE         TO EA-ALL-MACHINE
              MOVE EA-ALL-PATH             TO EA-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-EAFILE NOT = "S" )
              MOVE EA-PATH-BEG             TO EA-SELECT-PATH-BEG
              MOVE EA-PATH-FILE            TO EA-SELECT-PATH-FILE
           ELSE
              MOVE EA-PATH-SQL             TO EA-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-EA1-FILE.
           PERFORM LOAD-EA1-FILE.
           PERFORM OPEN-IT.
           MOVE EA-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-EA1-FILE.
           PERFORM START-IT.
           MOVE EA-SELECT-PATH TO E-FILE.
           MOVE EA1-KEY TO E-KEYX.

           IF ( EA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-EA1-FILE.

           IF ( QEA1-WEND-SEQNO = ZEROES    ) OR
              ( QEA1-WEND-SEQNO NOT NUMERIC )
              MOVE EA-BRANCH      TO QEA1-WBEG-BRANCH
              MOVE ALL "9"        TO QEA1-WEND-BRANCH
              MOVE EA-CLASS       TO QEA1-WBEG-CLASS
              MOVE ALL "9"        TO QEA1-WEND-CLASS
              MOVE EA-GROUP-ID    TO QEA1-WBEG-GROUP-ID
              MOVE ALL "Z"        TO QEA1-WEND-GROUP-ID
              MOVE EA-SEQNO       TO QEA1-WBEG-SEQNO
              MOVE ALL "9"        TO QEA1-WEND-SEQNO.

           EXEC SQL
            DECLARE EA1_CURSOR CURSOR FOR
            SELECT EAFILE.EA_BRANCH,
                   EAFILE.EA_CLASS,
                   ISNULL(EAFILE.EA_GROUP_ID,''),
                   EAFILE.EA_SEQNO,
                   ISNULL(CAST(EAFILE.EA_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(EAFILE.EA_DESC,''),
                   EAFILE.EA_PASTDUE_DAY1,
                   EAFILE.EA_PASTDUE_DAY2,
                   EAFILE.EA_PASTDUE_AMT1,
                   EAFILE.EA_PASTDUE_AMT2,
                   EAFILE.EA_ACCT_BAL1,
                   EAFILE.EA_ACCT_BAL2,
                   ISNULL(EAFILE.EA_MATURED,''),
                   ISNULL(EAFILE.EA_PROMISED,''),
                   ISNULL(EAFILE.EA_ACTION,''),
                   ISNULL(CAST(EAFILE.EA_LSTFM_DATE1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(EAFILE.EA_LSTFM_DATE2
                                                  AS VARCHAR(10)),'0'),
                   EAFILE.EA_LSTFM_NO1,
                   EAFILE.EA_LSTFM_NO2,
                   EAFILE.EA_PASTPROM_DAY1,
                   EAFILE.EA_PASTPROM_DAY2,
                   EAFILE.EA_RECENCY_DAY1,
                   EAFILE.EA_RECENCY_DAY2,
                   ISNULL(EAFILE.EA_ALPNAME1,''),
                   ISNULL(EAFILE.EA_ALPNAME2,''),
                   ISNULL(EAFILE.EA_RECENCYDATE,''),
                   EAFILE.EA_DLNO1,
                   EAFILE.EA_DLNO2,
                   EAFILE.EA_CONTRACTUAL_DAY1,
                   EAFILE.EA_CONTRACTUAL_DAY2,
                   EAFILE.EA_SETFM_NO,
                   ISNULL(EAFILE.EA_SETACT_CD,''),
                   ISNULL(EAFILE.EA_INC_CNT,''),
                   EAFILE.EA_INC_PROMISED,
                   EAFILE.EA_SETFM_1,
                   EAFILE.EA_SETFM_2,
                   EAFILE.EA_SETFM_3,
                   ISNULL(EAFILE.EA_SET_LN_COL,''),
                   ISNULL(EAFILE.EA_LN_COL_ID,''),
                   ISNULL(EAFILE.EA_CQ_FG,''),
                   ISNULL(EAFILE.EA_COL_POOL,''),
                   ISNULL(EAFILE.EA_CPOTS,''),
                   ISNULL(EAFILE.EA_REPO,''),
                   ISNULL(EAFILE.EA_PL,''),
                   ISNULL(EAFILE.EA_BANKRUPT,''),
                   ISNULL(EAFILE.EA_PL_IN_ACTIVE_BOTH,''),
                   ISNULL(EAFILE.EA_CONFIDENTIAL_FG,''),
                   ISNULL(EAFILE.EA_1ST_PMT_MISSES,''),
                   ISNULL(EAFILE.EA_LEGAL,''),
                   ISNULL(EAFILE.EA_DEBTAID,''),
                   ISNULL(EAFILE.EA_JUDGEMENT,''),
                   ISNULL(EAFILE.EA_BROKEN_PROM_FG,''),
                   EAFILE.EA_PROM_BREAK1,
                   EAFILE.EA_PROM_BREAK2,
                   ISNULL(EAFILE.EA_ORIGIN,''),
                   ISNULL(CAST(EAFILE.EA_PL_BEG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(EAFILE.EA_PL_END_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(EAFILE.EA_RPOTS,''),
                   ISNULL(EAFILE.EA_MISPAYS,''),
                   ISNULL(EAFILE.EA_ALLOTMENTS,''),
                   EAFILE.EA_LNDATE_DAYS1,
                   EAFILE.EA_LNDATE_DAYS2,
                   EAFILE.EA_MATURE_DAYS1,
                   EAFILE.EA_MATURE_DAYS2,
                   EAFILE.EA_DAY_OF_MO_DUE1,
                   EAFILE.EA_DAY_OF_MO_DUE2,
                   ISNULL(EAFILE.EA_INCL_COLLECTOR,''),
                   ISNULL(EAFILE.EA_BORR_STATE,'')
            FROM DBO.EAFILE
            WHERE EAFILE.EA_BRANCH >= :QEA1-WBEG-BRANCH
              AND EAFILE.EA_BRANCH <= :QEA1-WEND-BRANCH
              AND EAFILE.EA_CLASS >= :QEA1-WBEG-CLASS
              AND EAFILE.EA_CLASS <= :QEA1-WEND-CLASS
              AND EAFILE.EA_GROUP_ID >= :QEA1-WBEG-GROUP-ID
              AND EAFILE.EA_GROUP_ID <= :QEA1-WEND-GROUP-ID
              AND EAFILE.EA_SEQNO >= :QEA1-WBEG-SEQNO
              AND EAFILE.EA_SEQNO <= :QEA1-WEND-SEQNO
            ORDER BY EAFILE.EA_BRANCH,
                     EAFILE.EA_CLASS,
                     EAFILE.EA_GROUP_ID,
                     EAFILE.EA_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN EA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QEA1-WBEG-GROUP-ID
                          QEA1-WEND-GROUP-ID.

           MOVE ZEROES TO QEA1-WBEG-BRANCH
                          QEA1-WEND-BRANCH
                          QEA1-WEND-CLASS
                          QEA1-WEND-CLASS
                          QEA1-WEND-SEQNO
                          QEA1-WEND-SEQNO.

           MOVE STAT---GOOD TO EA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-EA1-FILE.
           PERFORM READ-IT.
           MOVE EA-SELECT-PATH TO E-FILE.
           MOVE EA1-KEY TO E-KEYX.

           MOVE EA-BRANCH     TO QEA-BRANCH.
           MOVE EA-CLASS      TO QEA-CLASS.
           MOVE EA-GROUP-ID   TO QEA-GROUP-ID.
           MOVE EA-SEQNO      TO QEA-SEQNO.

           EXEC SQL
            SELECT EAFILE.EA_BRANCH,
                   EAFILE.EA_CLASS,
                   ISNULL(EAFILE.EA_GROUP_ID,''),
                   EAFILE.EA_SEQNO,
                   ISNULL(CAST(EAFILE.EA_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(EAFILE.EA_DESC,''),
                   EAFILE.EA_PASTDUE_DAY1,
                   EAFILE.EA_PASTDUE_DAY2,
                   EAFILE.EA_PASTDUE_AMT1,
                   EAFILE.EA_PASTDUE_AMT2,
                   EAFILE.EA_ACCT_BAL1,
                   EAFILE.EA_ACCT_BAL2,
                   ISNULL(EAFILE.EA_MATURED,''),
                   ISNULL(EAFILE.EA_PROMISED,''),
                   ISNULL(EAFILE.EA_ACTION,''),
                   ISNULL(CAST(EAFILE.EA_LSTFM_DATE1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(EAFILE.EA_LSTFM_DATE2
                                                  AS VARCHAR(10)),'0'),
                   EAFILE.EA_LSTFM_NO1,
                   EAFILE.EA_LSTFM_NO2,
                   EAFILE.EA_PASTPROM_DAY1,
                   EAFILE.EA_PASTPROM_DAY2,
                   EAFILE.EA_RECENCY_DAY1,
                   EAFILE.EA_RECENCY_DAY2,
                   ISNULL(EAFILE.EA_ALPNAME1,''),
                   ISNULL(EAFILE.EA_ALPNAME2,''),
                   ISNULL(EAFILE.EA_RECENCYDATE,''),
                   EAFILE.EA_DLNO1,
                   EAFILE.EA_DLNO2,
                   EAFILE.EA_CONTRACTUAL_DAY1,
                   EAFILE.EA_CONTRACTUAL_DAY2,
                   EAFILE.EA_SETFM_NO,
                   ISNULL(EAFILE.EA_SETACT_CD,''),
                   ISNULL(EAFILE.EA_INC_CNT,''),
                   EAFILE.EA_INC_PROMISED,
                   EAFILE.EA_SETFM_1,
                   EAFILE.EA_SETFM_2,
                   EAFILE.EA_SETFM_3,
                   ISNULL(EAFILE.EA_SET_LN_COL,''),
                   ISNULL(EAFILE.EA_LN_COL_ID,''),
                   ISNULL(EAFILE.EA_CQ_FG,''),
                   ISNULL(EAFILE.EA_COL_POOL,''),
                   ISNULL(EAFILE.EA_CPOTS,''),
                   ISNULL(EAFILE.EA_REPO,''),
                   ISNULL(EAFILE.EA_PL,''),
                   ISNULL(EAFILE.EA_BANKRUPT,''),
                   ISNULL(EAFILE.EA_PL_IN_ACTIVE_BOTH,''),
                   ISNULL(EAFILE.EA_CONFIDENTIAL_FG,''),
                   ISNULL(EAFILE.EA_1ST_PMT_MISSES,''),
                   ISNULL(EAFILE.EA_LEGAL,''),
                   ISNULL(EAFILE.EA_DEBTAID,''),
                   ISNULL(EAFILE.EA_JUDGEMENT,''),
                   ISNULL(EAFILE.EA_BROKEN_PROM_FG,''),
                   EAFILE.EA_PROM_BREAK1,
                   EAFILE.EA_PROM_BREAK2,
                   ISNULL(EAFILE.EA_ORIGIN,''),
                   ISNULL(CAST(EAFILE.EA_PL_BEG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(EAFILE.EA_PL_END_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(EAFILE.EA_RPOTS,''),
                   ISNULL(EAFILE.EA_MISPAYS,''),
                   ISNULL(EAFILE.EA_ALLOTMENTS,''),
                   EAFILE.EA_LNDATE_DAYS1,
                   EAFILE.EA_LNDATE_DAYS2,
                   EAFILE.EA_MATURE_DAYS1,
                   EAFILE.EA_MATURE_DAYS2,
                   EAFILE.EA_DAY_OF_MO_DUE1,
                   EAFILE.EA_DAY_OF_MO_DUE2,
                   ISNULL(EAFILE.EA_INCL_COLLECTOR,''),
                   ISNULL(EAFILE.EA_BORR_STATE,'')
            INTO :QEA-REC
            FROM DBO.EAFILE
            WHERE EAFILE.EA_BRANCH = :QEA-BRANCH
              AND EAFILE.EA_CLASS = :QEA-CLASS
              AND EAFILE.EA_GROUP_ID = :QEA-GROUP-ID
              AND EAFILE.EA_SEQNO = :QEA-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-EA1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-EA-FIELDS.

      *-----------------------------------------------------------------
       READ-EA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE EA-SELECT-PATH TO E-FILE.
           MOVE EA1-KEY TO E-KEYX.
           INITIALIZE QEA-REC.

           IF ( EA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH EA1_CURSOR INTO :QEA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-EA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-EA-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-EA1-FILE.
           IF ( EA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE EA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO EA1-CURSOR-STAT.

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
              DESTROY IOEARN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
