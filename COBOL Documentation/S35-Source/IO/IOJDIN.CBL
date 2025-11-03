       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOJDIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-11-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOJDIN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : JDFILE
      * TABLE DESC    : GENERAL LEDGER JOURNAL DETAIL LINES
      * OUTPUT FILE   : XPC/IOJDIN.PRE
      * USED FOR      : LIBGL/GLWSJD.CPY  --- QJD-REC
      *                 LIBGL/GLJD1R.CPY  --- READ
      *                 LIBGL/GLJD1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0511 ***NEW***
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
           "@(#)IO/IOJDIN.CBL S34 05/18/21-10:34:20 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGL/GL01JD.CPY".
       COPY "LIBGL/GLWSJD.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QJD1-WHERE-BEG-KEY.
           03  QJD1-WBEG-DATE              PIC X(10).
           03  QJD1-WBEG-ID                PIC X(08).
           03  QJD1-WBEG-SEQ               PIC 9(02).
           03  QJD1-WBEG-LINE              PIC 9(04).
 
       01  QJD1-WHERE-END-KEY.
           03  QJD1-WEND-DATE              PIC X(10).
           03  QJD1-WEND-ID                PIC X(08).
           03  QJD1-WEND-SEQ               PIC 9(02).
           03  QJD1-WEND-LINE              PIC 9(04).
 
       01  QJD-REC.
           03  QJD-DATE                         PIC X(10).
           03  QJD-ID                           PIC X(8).
           03  QJD-SEQ                          PIC 9(2).
           03  QJD-LINE                         PIC 9(4).
           03  QJD-LTOUCH-DATE                  PIC X(10).
           03  QJD-GROUP-FG                     PIC X.
           03  QJD-BR-ACCOUNT                   PIC 9(11).
           03  QJD-OFFSET-GROUP-FG              PIC X.
           03  QJD-OFFSET-BR-ACCOUNT            PIC 9(11).
           03  QJD-AMT                          PIC S9(9)V99.
           03  QJD-EXPL                         PIC X(32).
           03  QJD-DRCR-FG                      PIC X.
           03  QJD-STATUS                       PIC X.
           03  QJD-ORIG-LINE                    PIC 9(4).
           03  QJD-PARENT-GROUP                 PIC X(4).

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
           03  BEG-DATE               PIC 9(08).
           03  END-DATE               PIC 9(08).
           03  BEG-ID                 PIC X(08).
           03  END-ID                 PIC X(08).
           03  BEG-SEQ                PIC 9(02).
           03  END-SEQ                PIC 9(02).
           03  BEG-LINE               PIC 9(04).
           03  END-LINE               PIC 9(04).

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
           03  FILLER PIC X(28) VALUE "DATE         ID        SEQ  ".
           03  FILLER PIC X(37) VALUE "LINE   BR/ACCOUNT    AMOUNT   ".
           03  FILLER PIC X(30) VALUE "EXPL".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-JD-DATE                    PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-JD-ID                      PIC X(8).
           03  FILLER                         PIC X(03).
           03  D01-JD-SEQ                     PIC 9(2).
           03  FILLER                         PIC X(03).
           03  D01-JD-LINE                    PIC 9(4).
           03  FILLER                         PIC X(03).
           03  D01-JD-BR-ACCOUNT              PIC 9(11).
           03  FILLER                         PIC X(03).
           03  D01-JD-AMT                     PIC Z(8)9.99-.
           03  FILLER                         PIC X(03).
           03  D01-JD-EXPL                    PIC X(32).

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
       COPY "LIBIO/IOJDIN_DEF.CPY".
       COPY "LIBIO/IOJDIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOJDIN_SCN.CPY".

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
           MOVE "B"                  TO JD-PATH-OVERRIDE.

           MOVE BEG-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QJD1-WBEG-DATE.
           MOVE BEG-ID               TO QJD1-WBEG-ID.
           MOVE BEG-SEQ              TO QJD1-WBEG-SEQ.
           MOVE BEG-LINE             TO QJD1-WBEG-LINE.

           MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QJD1-WEND-DATE.
           MOVE END-ID               TO QJD1-WEND-ID.
           MOVE END-SEQ              TO QJD1-WEND-SEQ.
           MOVE END-LINE             TO QJD1-WEND-LINE.

           PERFORM OPEN-JD1-FILE.
           PERFORM START-JD1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-JD1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( JD-DATE < BEG-DATE )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( JD-DATE > END-DATE )
              GO TO MAIN-PROGRAM-END.
 
           IF JD-ID < BEG-ID OR > END-ID
              GO TO MAIN-PROGRAM-NEXT.
           IF JD-SEQ < BEG-SEQ OR > END-SEQ
              GO TO MAIN-PROGRAM-NEXT.
           IF JD-LINE < BEG-LINE OR > END-LINE
              GO TO MAIN-PROGRAM-NEXT.

      *-------------------------------------------------------{ RANGES }
           MOVE JD-DATE                    TO D01-JD-DATE.
           MOVE JD-ID                      TO D01-JD-ID.
           MOVE JD-SEQ                     TO D01-JD-SEQ.
           MOVE JD-LINE                    TO D01-JD-LINE.
           MOVE JD-BR-ACCOUNT              TO D01-JD-BR-ACCOUNT.
           MOVE JD-AMT                     TO D01-JD-AMT.
           MOVE JD-EXPL                    TO D01-JD-EXPL.
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
           MOVE "IOJDIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOJDIN-QUEUE-POS  TO Q-POS.
           MOVE IOJDIN-COPIES-POS TO C-POS.
           MOVE IOJDIN-PRU-POS    TO P-POS.
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


           MOVE "ERNM060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATES.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           MOVE LEGEND-END-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF ( BEG-DATE > END-DATE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-BEG-ID.

      *-----------------------------------------------------------------
       ENTER-ALL-DATES.
           MOVE "S  8080BEG-DATE." TO VDU.
           MOVE JULIAN-BEG         TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE." TO VDU.
           MOVE JULIAN-END         TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-ID.

           MOVE "E  A080BEG-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-ID.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ID.

           MOVE VDUBUF TO BEG-ID.

      *-----------------------------------------------------------------
       ENTER-END-ID.

           MOVE "E  A080END-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ID.

           MOVE VDUBUF TO END-ID.

           IF ( BEG-ID > END-ID )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ID.

           GO TO ENTER-BEG-SEQ.

      *-----------------------------------------------------------------
       ENTER-ALL-ID.

           MOVE "S  A080BEG-ID." TO VDU.
           MOVE SPACES               TO BEG-ID VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A080END-ID." TO VDU.
           MOVE ALL "Z"               TO END-ID VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SEQ.

           MOVE "EN N020BEG-SEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SEQ.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SEQ.

           MOVE VDUNUM0 TO BEG-SEQ.

      *-----------------------------------------------------------------
       ENTER-END-SEQ.

           MOVE "EN N020END-SEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SEQ.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SEQ.

           MOVE VDUNUM0 TO END-SEQ.

           IF ( BEG-SEQ > END-SEQ )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SEQ.

           GO TO ENTER-BEG-LINE.

      *-----------------------------------------------------------------
       ENTER-ALL-SEQ.

           MOVE "SN N020BEG-SEQ." TO VDU.
           MOVE 0                 TO BEG-SEQ VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N020END-SEQ." TO VDU.
           MOVE 99                TO END-SEQ VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-LINE.

           MOVE "EN N040BEG-LINE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SEQ.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-LINE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-LINE.

           MOVE VDUNUM0 TO BEG-LINE.

      *-----------------------------------------------------------------
       ENTER-END-LINE.

           MOVE "EN N040END-LINE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LINE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-LINE.

           MOVE VDUNUM0 TO END-LINE.

           IF ( BEG-LINE > END-LINE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LINE.

           GO TO ENTER-BEG-LINE.

      *-----------------------------------------------------------------
       ENTER-ALL-LINE.

           MOVE "SN N040BEG-LINE." TO VDU.
           MOVE 0                 TO BEG-LINE VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N040END-LINE." TO VDU.
           MOVE 9999               TO END-LINE VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-LINE.
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

           MOVE "S  A000BEG-DATE."    TO VDU.
           MOVE BEG-DATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A000END-DATE."    TO VDU.
           MOVE END-DATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-ID."    TO VDU.
           MOVE BEG-ID              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-ID."    TO VDU.
           MOVE END-ID              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SN N020BEG-SEQ."    TO VDU.
           MOVE BEG-SEQ              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N020END-SEQ."    TO VDU.
           MOVE END-SEQ              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N040BEG-LINE."    TO VDU.
           MOVE BEG-LINE              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N040END-LINE."    TO VDU.
           MOVE END-LINE              TO VDUNUM0.
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

           MOVE JD1-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING ID               : "    TO R01-PROMPT.
           MOVE BEG-ID                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ID               : "    TO R01-PROMPT.
           MOVE END-ID                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SEQ               : "    TO R01-PROMPT.
           MOVE BEG-SEQ                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SEQ               : "    TO R01-PROMPT.
           MOVE END-SEQ                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING LINE               : "    TO R01-PROMPT.
           MOVE BEG-LINE                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    LINE               : "    TO R01-PROMPT.
           MOVE END-LINE                             TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - JDFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOJDIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOJDIN-SYSDATE.
           DISPLAY IOJDIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOJDIN-FORM-FIELDS TO WR-BUF.
           MOVE IOJDIN-FORM-LIST TO WR-LIST.
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

           MOVE IOJDIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOJDIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOJDIN_EVA.CPY".
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
      *    LOAD JD-REC FROM QJD-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-JD-FIELDS.

           IF ( QJD-DATE = "1900-01-01" )
              MOVE ZEROES TO JD-DATE
           ELSE
              MOVE QJD-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO JD-DATE.

           MOVE QJD-ID TO JD-ID.
           MOVE QJD-SEQ TO JD-SEQ.
           MOVE QJD-LINE TO JD-LINE.

           IF ( QJD-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO JD-LTOUCH-DATE
           ELSE
              MOVE QJD-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO JD-LTOUCH-DATE.

           MOVE QJD-GROUP-FG TO JD-GROUP-FG.
           MOVE QJD-BR-ACCOUNT TO JD-BR-ACCOUNT.
           MOVE QJD-OFFSET-GROUP-FG TO JD-OFFSET-GROUP-FG.
           MOVE QJD-OFFSET-BR-ACCOUNT TO JD-OFFSET-BR-ACCOUNT.
           MOVE QJD-AMT TO JD-AMT.
           MOVE QJD-EXPL TO JD-EXPL.
           MOVE QJD-DRCR-FG TO JD-DRCR-FG.
           MOVE QJD-STATUS TO JD-STATUS.
           MOVE QJD-ORIG-LINE TO JD-ORIG-LINE.
           MOVE QJD-PARENT-GROUP TO JD-PARENT-GROUP.
 
      *-----------------------------------------------------------------
      *    LOAD QJD-REC FROM JD-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-JD-FIELDS.

           IF ( JD-DATE = ZEROES )
              MOVE "1900-01-01" TO QJD-DATE
           ELSE
              MOVE JD-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QJD-DATE.

           MOVE JD-ID TO QJD-ID.
           MOVE JD-SEQ TO QJD-SEQ.
           MOVE JD-LINE TO QJD-LINE.

           IF ( JD-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QJD-LTOUCH-DATE
           ELSE
              MOVE JD-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QJD-LTOUCH-DATE.

           MOVE JD-GROUP-FG TO QJD-GROUP-FG.
           MOVE JD-BR-ACCOUNT TO QJD-BR-ACCOUNT.
           MOVE JD-OFFSET-GROUP-FG TO QJD-OFFSET-GROUP-FG.
           MOVE JD-OFFSET-BR-ACCOUNT TO QJD-OFFSET-BR-ACCOUNT.
           MOVE JD-AMT TO QJD-AMT.
           MOVE JD-EXPL TO QJD-EXPL.
           MOVE JD-DRCR-FG TO QJD-DRCR-FG.
           MOVE JD-STATUS TO QJD-STATUS.
           MOVE JD-ORIG-LINE TO QJD-ORIG-LINE.
           MOVE JD-PARENT-GROUP TO QJD-PARENT-GROUP.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
       LOAD-JD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( JD-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( JD-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO JD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO JD-ALL-MACHINE
              MOVE JD-ALL-PATH             TO JD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( JD-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO JD-ALL-BASE
              MOVE FILPATH-MACHINE         TO JD-ALL-MACHINE
              MOVE JD-ALL-PATH             TO JD-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-JDFILE NOT = "S" )
              MOVE JD-PATH-BEG             TO JD-SELECT-PATH-BEG
              MOVE JD-PATH-FILE            TO JD-SELECT-PATH-FILE
           ELSE
              MOVE JD-PATH-SQL             TO JD-SELECT-PATH.


      *-----------------------------------------------------------------
       OPEN-JD1-FILE.
           PERFORM LOAD-JD1-FILE.
           PERFORM OPEN-IT.
           MOVE JD-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-JD1-FILE.
           PERFORM START-IT.
           MOVE JD-SELECT-PATH TO E-FILE.
           MOVE JD1-KEY TO E-KEYX.

           IF ( JD1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-JD1-FILE.

           IF ( QJD1-WEND-DATE = SPACES )
              MOVE JD-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QJD1-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QJD1-WEND-DATE
              MOVE JD-ID                TO QJD1-WBEG-ID
              MOVE ALL "Z"              TO QJD1-WEND-ID
              MOVE JD-SEQ               TO QJD1-WBEG-SEQ
              MOVE 99                   TO QJD1-WEND-SEQ
              MOVE JD-LINE              TO QJD1-WBEG-LINE
              MOVE 9999                 TO QJD1-WEND-LINE.

           EXEC SQL
            DECLARE JD1_CURSOR CURSOR FOR
            SELECT ISNULL(CAST(JDFILE.JD_DATE AS VARCHAR(10)),'0'),
                   ISNULL(JDFILE.JD_ID,''),
                   JDFILE.JD_SEQ,
                   JDFILE.JD_LINE,
                   ISNULL(CAST(JDFILE.JD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(JDFILE.JD_GROUP_FG,''),
                   JDFILE.JD_BR_ACCOUNT,
                   ISNULL(JDFILE.JD_OFFSET_GROUP_FG,''),
                   JDFILE.JD_OFFSET_BR_ACCOUNT,
                   JDFILE.JD_AMT,
                   ISNULL(JDFILE.JD_EXPL,''),
                   ISNULL(JDFILE.JD_DRCR_FG,''),
                   ISNULL(JDFILE.JD_STATUS,''),
                   JDFILE.JD_ORIG_LINE,
                   ISNULL(JDFILE.JD_PARENT_GROUP,'')
            FROM DBO.JDFILE
            WHERE CAST(JDFILE.JD_DATE AS VARCHAR(10))
                           >= :QJD1-WBEG-DATE
              AND CAST(JDFILE.JD_DATE AS VARCHAR(10))
                           <= :QJD1-WEND-DATE
              AND JDFILE.JD_ID >= :QJD1-WBEG-ID
              AND JDFILE.JD_ID <= :QJD1-WEND-ID
              AND JDFILE.JD_SEQ >= :QJD1-WBEG-SEQ
              AND JDFILE.JD_SEQ <= :QJD1-WEND-SEQ
              AND JDFILE.JD_LINE >= :QJD1-WBEG-LINE
              AND JDFILE.JD_LINE <= :QJD1-WEND-LINE
            ORDER BY JDFILE.JD_DATE,
                     JDFILE.JD_ID,
                     JDFILE.JD_SEQ,
                     JDFILE.JD_LINE
           END-EXEC.

           EXEC SQL
               OPEN JD1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QJD1-WBEG-ID
                          QJD1-WEND-ID
                          QJD1-WBEG-DATE
                          QJD1-WEND-DATE.

           MOVE ZEROES TO QJD1-WBEG-SEQ
                          QJD1-WBEG-LINE
                          QJD1-WEND-SEQ
                          QJD1-WEND-LINE.

           MOVE STAT---GOOD TO JD1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-JD1-FILE.
           PERFORM READ-IT.
           MOVE JD-SELECT-PATH TO E-FILE.
           MOVE JD1-KEY TO E-KEYX.

           MOVE JD-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QJD-DATE. 
           MOVE JD-ID               TO QJD-ID.
           MOVE JD-SEQ              TO QJD-SEQ.
           MOVE JD-LINE             TO QJD-LINE.

           EXEC SQL
            SELECT ISNULL(CAST(JDFILE.JD_DATE AS VARCHAR(10)),'0'),
                   ISNULL(JDFILE.JD_ID,''),
                   JDFILE.JD_SEQ,
                   JDFILE.JD_LINE,
                   ISNULL(CAST(JDFILE.JD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(JDFILE.JD_GROUP_FG,''),
                   JDFILE.JD_BR_ACCOUNT,
                   ISNULL(JDFILE.JD_OFFSET_GROUP_FG,''),
                   JDFILE.JD_OFFSET_BR_ACCOUNT,
                   JDFILE.JD_AMT,
                   ISNULL(JDFILE.JD_EXPL,''),
                   ISNULL(JDFILE.JD_DRCR_FG,''),
                   ISNULL(JDFILE.JD_STATUS,''),
                   JDFILE.JD_ORIG_LINE,
                   ISNULL(JDFILE.JD_PARENT_GROUP,'')
            INTO :QJD-REC
            FROM DBO.JDFILE
            WHERE JDFILE.JD_DATE = :QJD-DATE
              AND JDFILE.JD_ID = :QJD-ID
              AND JDFILE.JD_SEQ = :QJD-SEQ
              AND JDFILE.JD_LINE = :QJD-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-JD1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-JD-FIELDS.

      *-----------------------------------------------------------------
       READ-JD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE JD-SELECT-PATH TO E-FILE.
           MOVE JD1-KEY TO E-KEYX.
           INITIALIZE QJD-REC.

           IF ( JD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH JD1_CURSOR INTO :QJD-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-JD1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-JD-FIELDS.

      *-----------------------------------------------------------------
       WRITE-JD1-FILE.

           ACCEPT JD-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE JD-SELECT-PATH TO E-FILE.
           MOVE JD1-KEY TO E-KEYX.
           PERFORM SET-JD-FIELDS.

           EXEC SQL
            INSERT INTO DBO.JDFILE
             (JDFILE.JD_DATE,
              JDFILE.JD_ID,
              JDFILE.JD_SEQ,
              JDFILE.JD_LINE,
              JDFILE.JD_LTOUCH_DATE,
              JDFILE.JD_GROUP_FG,
              JDFILE.JD_BR_ACCOUNT,
              JDFILE.JD_OFFSET_GROUP_FG,
              JDFILE.JD_OFFSET_BR_ACCOUNT,
              JDFILE.JD_AMT,
              JDFILE.JD_EXPL,
              JDFILE.JD_DRCR_FG,
              JDFILE.JD_STATUS,
              JDFILE.JD_ORIG_LINE,
              JDFILE.JD_PARENT_GROUP)
            VALUES
             (NULLIF(:QJD-DATE,'1900-01-01'),
              :QJD-ID,
              :QJD-SEQ,
              :QJD-LINE,
              NULLIF(:QJD-LTOUCH-DATE,'1900-01-01'),
              :QJD-GROUP-FG,
              :QJD-BR-ACCOUNT,
              :QJD-OFFSET-GROUP-FG,
              :QJD-OFFSET-BR-ACCOUNT,
              :QJD-AMT,
              :QJD-EXPL,
              :QJD-DRCR-FG,
              :QJD-STATUS,
              :QJD-ORIG-LINE,
              :QJD-PARENT-GROUP)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-JD1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-JD1-FILE.

           ACCEPT JD-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE JD-SELECT-PATH TO E-FILE.
           MOVE JD1-KEY TO E-KEYX.
           PERFORM SET-JD-FIELDS.

           EXEC SQL
            UPDATE DBO.JDFILE
             SET
              JDFILE.JD_DATE =
                 NULLIF(:QJD-DATE,'1900-01-01'),
              JDFILE.JD_ID = :QJD-ID,
              JDFILE.JD_SEQ = :QJD-SEQ,
              JDFILE.JD_LINE = :QJD-LINE,
              JDFILE.JD_LTOUCH_DATE =
                 NULLIF(:QJD-LTOUCH-DATE,'1900-01-01'),
              JDFILE.JD_GROUP_FG = :QJD-GROUP-FG,
              JDFILE.JD_BR_ACCOUNT = :QJD-BR-ACCOUNT,
              JDFILE.JD_OFFSET_GROUP_FG = :QJD-OFFSET-GROUP-FG,
              JDFILE.JD_OFFSET_BR_ACCOUNT =
                   :QJD-OFFSET-BR-ACCOUNT,
              JDFILE.JD_AMT = :QJD-AMT,
              JDFILE.JD_EXPL = :QJD-EXPL,
              JDFILE.JD_DRCR_FG = :QJD-DRCR-FG,
              JDFILE.JD_STATUS = :QJD-STATUS,
              JDFILE.JD_ORIG_LINE = :QJD-ORIG-LINE,
              JDFILE.JD_PARENT_GROUP = :QJD-PARENT-GROUP
             WHERE JDFILE.JD_DATE = :QJD-DATE
               AND JDFILE.JD_ID = :QJD-ID
               AND JDFILE.JD_SEQ = :QJD-SEQ
               AND JDFILE.JD_LINE = :QJD-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-JD1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-JD1-FILE.

           PERFORM DELETE-IT.
           MOVE JD-SELECT-PATH TO E-FILE.
           MOVE JD1-KEY TO E-KEYX.
           PERFORM SET-JD-FIELDS.

           EXEC SQL
            DELETE FROM DBO.JDFILE
             WHERE JDFILE.JD_DATE = :QJD-DATE
               AND JDFILE.JD_ID = :QJD-ID
               AND JDFILE.JD_SEQ = :QJD-SEQ
               AND JDFILE.JD_LINE = :QJD-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-JD1-FILE.

      *-----------------------------------------------------------------
       CLOSE-JD1-FILE.
           IF ( JD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE JD1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO JD1-CURSOR-STAT.

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
              DESTROY IOJDIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
