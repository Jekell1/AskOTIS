       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOEOIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-11-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOEOIN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : EOFILE 
      * TABLE DESC    : BATCH REPORT PROCESSING
      * OUTPUT FILE   : XPC/IOEOIN.PRE
      * USED FOR      : LIBGB/GBWSEO.CPY  --- QEO-REC
      *                 LIBGB/GBEO1IN.CPY --- READ/READ-NEXT/START
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
           "@(#)IO/IOEOIN.CBL S34 05/17/21-16:44:43 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01EO.CPY".
       COPY "LIBGB/GBWSEO.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QEO1-WHERE-BEG-KEY.
           03  QEO1-WBEG-PCODE            PIC X(3).
           03  QEO1-WBEG-STEP             PIC 9(4).
 
       01  QEO1-WHERE-END-KEY.
           03  QEO1-WEND-PCODE            PIC X(3).
           03  QEO1-WEND-STEP             PIC 9(4).
 
       01  QEO-REC.
           03  QEO-PCODE                        PIC X(3).
           03  QEO-STEP                         PIC 9999.
           03  QEO-LTOUCH-DATE                  PIC X(10).
           03  QEO-DIR                          PIC XX.
           03  QEO-PROGRAM                      PIC X(6).
           03  QEO-COPIES                       PIC 9.
           03  QEO-COMMENT                      PIC X(30).
           03  QEO-MESS                         PIC X(60).
           03  QEO-START-DATE                   PIC X(10).
           03  QEO-START-TIME                   PIC 9(6).
           03  QEO-COMPLETE-DATE                PIC X(10).
           03  QEO-COMPLETE-TIME                PIC 9(6).
           03  QEO-COMPLETE-STAT                PIC X(6).
           03  QEO-PARAMETERS                   PIC X(600).
           03  QEO-ERROR-STEP-X                 PIC XXXX.
           03  QEO-TRANSFER                     PIC X.
           03  QEO-LEVEL                        PIC 9.

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

       01  LEGEND-BEG-PCODE. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING PCODE      (F3-ALL) ".

       01  LEGEND-END-PCODE. 
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING PCODE      ".

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
           03  BEG-PCODE               PIC X(03).
           03  END-PCODE               PIC X(03).
           03  BEG-STEP                PIC 9(04).
           03  END-STEP                PIC 9(04).

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
           03  FILLER PIC X(28) VALUE "CODE    STEP     PROG       ".
           03  FILLER PIC X(30) VALUE "LTOUCH       COPIES  COMMENT".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-EO-PCODE                   PIC X(3).
           03  FILLER                         PIC X(05).
           03  D01-EO-STEP                    PIC 9(4).
           03  FILLER                         PIC X(05).
           03  D01-EO-PROGRAM                 PIC X(6).
           03  FILLER                         PIC X(05).
           03  D01-EO-LTOUCH-DATE             PIC 9999/99/99.
           03  FILLER                         PIC X(05).
           03  D01-EO-COPIES                  PIC 9.
           03  FILLER                         PIC X(05).
           03  D01-EO-COMMENT                 PIC X(20).

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
       COPY "LIBIO/IOEOIN_DEF.CPY".
       COPY "LIBIO/IOEOIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOEOIN_SCN.CPY".

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
           MOVE "B"                  TO EO-PATH-OVERRIDE.

           MOVE BEG-PCODE            TO QEO1-WBEG-PCODE.
           MOVE BEG-STEP             TO QEO1-WBEG-STEP.

           MOVE END-PCODE            TO QEO1-WEND-PCODE.
           MOVE END-STEP             TO QEO1-WEND-STEP.

           PERFORM OPEN-EO1-FILE.
           PERFORM START-EO1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-EO1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EO-PCODE < BEG-PCODE )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( EO-PCODE > END-PCODE )
              GO TO MAIN-PROGRAM-END.

      *-------------------------------------------------------{ RANGES }
           MOVE EO-PCODE                  TO D01-EO-PCODE.
           MOVE EO-STEP                   TO D01-EO-STEP.
           MOVE EO-PROGRAM                TO D01-EO-PROGRAM.
           MOVE EO-LTOUCH-DATE            TO D01-EO-LTOUCH-DATE.
           MOVE EO-COPIES                 TO D01-EO-COPIES.
           MOVE EO-COMMENT                TO D01-EO-COMMENT.
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
           MOVE "IOEOIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOEOIN-QUEUE-POS  TO Q-POS.
           MOVE IOEOIN-COPIES-POS TO C-POS.
           MOVE IOEOIN-PRU-POS    TO P-POS.
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
       ENTER-BEG-PCODE.

           MOVE LEGEND-BEG-PCODE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A030BEG-PCODE." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-PCODE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-PCODE.

           MOVE VDUBUF TO BEG-PCODE.

      *-----------------------------------------------------------------
       ENTER-END-PCODE.

           MOVE LEGEND-END-PCODE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A030END-PCODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-PCODE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-PCODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-PCODE.

           IF ( BEG-PCODE > END-PCODE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-PCODE.

           GO TO ENTER-BEG-STEP.

      *-----------------------------------------------------------------
       ENTER-ALL-PCODE.

           MOVE "S  A030BEG-PCODE." TO VDU.
           MOVE SPACES               TO BEG-PCODE VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A030END-PCODE." TO VDU.
           MOVE ALL "Z"              TO END-PCODE VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------

       ENTER-BEG-STEP.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "EN N040BEG-STEP." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-PCODE.
           IF (        F1-KEY ) GO TO ENTER-BEG-PCODE.
           IF (        F3-KEY ) GO TO ENTER-ALL-STEP.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-STEP.

           MOVE VDUNUM0 TO BEG-STEP.

      *-----------------------------------------------------------------
       ENTER-END-STEP.

           MOVE "EN N040END-STEP." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-STEP.
           IF (        F1-KEY ) GO TO ENTER-BEG-PCODE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-STEP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-STEP.

           IF ( BEG-STEP > END-STEP )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-STEP.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-STEP.

           MOVE "SN N040BEG-STEP." TO VDU.
           MOVE 0                  TO BEG-STEP VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N040END-STEP." TO VDU.
           MOVE 9999               TO END-STEP VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-PCODE.
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

           MOVE "S  A000BEG-PCODE."    TO VDU.
           MOVE BEG-PCODE              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-PCODE."    TO VDU.
           MOVE END-PCODE              TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A000BEG-STEP."    TO VDU.
           MOVE BEG-STEP              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-STEP."    TO VDU.
           MOVE END-STEP              TO VDUBUF.
           PERFORM SCREENIO.

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

           MOVE EO1-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING PCODE               : "    TO R01-PROMPT.
           MOVE BEG-PCODE                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    PCODE               : "    TO R01-PROMPT.
           MOVE END-PCODE                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING STEP               : "    TO R01-PROMPT.
           MOVE BEG-STEP                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    STEP               : "    TO R01-PROMPT.
           MOVE END-STEP                             TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - EOFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOEOIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOEOIN-SYSDATE.
           DISPLAY IOEOIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOEOIN-FORM-FIELDS TO WR-BUF.
           MOVE IOEOIN-FORM-LIST TO WR-LIST.
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

           MOVE IOEOIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOEOIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOEOIN_EVA.CPY".
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
      *    LOAD EO-REC FROM QEO-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-EO-FIELDS.
           MOVE QEO-PCODE TO EO-PCODE.
           MOVE QEO-STEP TO EO-STEP.

           IF ( QEO-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO EO-LTOUCH-DATE
           ELSE
              MOVE QEO-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EO-LTOUCH-DATE.

           MOVE QEO-DIR TO EO-DIR.
           MOVE QEO-PROGRAM TO EO-PROGRAM.
           MOVE QEO-COPIES TO EO-COPIES.
           MOVE QEO-COMMENT TO EO-COMMENT.
           MOVE QEO-MESS TO EO-MESS.

           IF ( QEO-START-DATE = "1900-01-01" )
              MOVE ZEROES TO EO-START-DATE
           ELSE
              MOVE QEO-START-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EO-START-DATE.

           MOVE QEO-START-TIME TO EO-START-TIME.

           IF ( QEO-COMPLETE-DATE = "1900-01-01" )
              MOVE ZEROES TO EO-COMPLETE-DATE
           ELSE
              MOVE QEO-COMPLETE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EO-COMPLETE-DATE.

           MOVE QEO-COMPLETE-TIME TO EO-COMPLETE-TIME.
           MOVE QEO-COMPLETE-STAT TO EO-COMPLETE-STAT.
           MOVE QEO-PARAMETERS TO EO-PARAMETERS.
           MOVE QEO-ERROR-STEP-X TO EO-ERROR-STEP-X.
           MOVE QEO-TRANSFER TO EO-TRANSFER.
           MOVE QEO-LEVEL TO EO-LEVEL.
 
      *-----------------------------------------------------------------
      *    LOAD QEO-REC FROM EO-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-EO-FIELDS.
           MOVE EO-PCODE TO QEO-PCODE.
           MOVE EO-STEP TO QEO-STEP.

           IF ( EO-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QEO-LTOUCH-DATE
           ELSE
              MOVE EO-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEO-LTOUCH-DATE.

           MOVE EO-DIR TO QEO-DIR.
           MOVE EO-PROGRAM TO QEO-PROGRAM.
           MOVE EO-COPIES TO QEO-COPIES.
           MOVE EO-COMMENT TO QEO-COMMENT.
           MOVE EO-MESS TO QEO-MESS.

           IF ( EO-START-DATE = ZEROES )
              MOVE "1900-01-01" TO QEO-START-DATE
           ELSE
              MOVE EO-START-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEO-START-DATE.

           MOVE EO-START-TIME TO QEO-START-TIME.

           IF ( EO-COMPLETE-DATE = ZEROES )
              MOVE "1900-01-01" TO QEO-COMPLETE-DATE
           ELSE
              MOVE EO-COMPLETE-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEO-COMPLETE-DATE.

           MOVE EO-COMPLETE-TIME TO QEO-COMPLETE-TIME.
           MOVE EO-COMPLETE-STAT TO QEO-COMPLETE-STAT.
           MOVE EO-PARAMETERS TO QEO-PARAMETERS.
           MOVE EO-ERROR-STEP-X TO QEO-ERROR-STEP-X.
           MOVE EO-TRANSFER TO QEO-TRANSFER.
           MOVE EO-LEVEL TO QEO-LEVEL.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
       LOAD-EO1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( EO-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( EO-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO EO-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO EO-ALL-MACHINE
              MOVE EO-ALL-PATH             TO EO-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( EO-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO EO-ALL-BASE
              MOVE FILPATH-MACHINE         TO EO-ALL-MACHINE
              MOVE EO-ALL-PATH             TO EO-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-EOFILE NOT = "S" )
              MOVE EO-PATH-BEG             TO EO-SELECT-PATH-BEG
              MOVE EO-PATH-FILE            TO EO-SELECT-PATH-FILE
           ELSE
              MOVE EO-PATH-SQL             TO EO-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-EO1-FILE.
           PERFORM LOAD-EO1-FILE.
           PERFORM OPEN-IT.
           MOVE EO-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-EO1-FILE.
           PERFORM START-IT.
           MOVE EO-SELECT-PATH TO E-FILE.
           MOVE EO1-KEY TO E-KEYX.

           IF ( EO1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-EO1-FILE.

           IF ( QEO1-WEND-STEP = ZEROES    ) OR
              ( QEO1-WEND-STEP NOT NUMERIC )
              MOVE EO-PCODE             TO QEO1-WBEG-PCODE
              MOVE ALL "Z"              TO QEO1-WEND-PCODE
              MOVE EO-STEP              TO QEO1-WBEG-STEP
              MOVE ALL "9"              TO QEO1-WEND-STEP.

           EXEC SQL
            DECLARE EO1_CURSOR CURSOR FOR
            SELECT ISNULL(EOFILE.EO_PCODE,''),
                   EOFILE.EO_STEP,
                   ISNULL(CAST(EOFILE.EO_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(EOFILE.EO_DIR,''),
                   ISNULL(EOFILE.EO_PROGRAM,''),
                   EOFILE.EO_COPIES,
                   ISNULL(EOFILE.EO_COMMENT,''),
                   ISNULL(EOFILE.EO_MESS,''),
                   ISNULL(CAST(EOFILE.EO_START_DATE
                                                  AS VARCHAR(10)),'0'),
                   EOFILE.EO_START_TIME,
                   ISNULL(CAST(EOFILE.EO_COMPLETE_DATE
                                                  AS VARCHAR(10)),'0'),
                   EOFILE.EO_COMPLETE_TIME,
                   ISNULL(EOFILE.EO_COMPLETE_STAT,''),
                   ISNULL(EOFILE.EO_PARAMETERS,''),
                   ISNULL(EOFILE.EO_ERROR_STEP_X,''),
                   ISNULL(EOFILE.EO_TRANSFER,''),
                   EOFILE.EO_LEVEL
            FROM DBO.EOFILE
            WHERE EOFILE.EO_PCODE >= :QEO1-WBEG-PCODE
              AND EOFILE.EO_PCODE <= :QEO1-WEND-PCODE
              AND EOFILE.EO_STEP >= :QEO1-WBEG-STEP
              AND EOFILE.EO_STEP <= :QEO1-WEND-STEP
            ORDER BY EOFILE.EO_PCODE,
                     EOFILE.EO_STEP
           END-EXEC.

           EXEC SQL
               OPEN EO1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QEO1-WBEG-PCODE
                          QEO1-WEND-PCODE.

           MOVE ZEROES TO QEO1-WBEG-STEP
                          QEO1-WEND-STEP.

           MOVE STAT---GOOD TO EO1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-EO1-FILE-GREATER.
           PERFORM START-IT.
           MOVE EO-SELECT-PATH TO E-FILE.
           MOVE EO1-KEY TO E-KEYX.

           IF ( EO1-GR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-EO1-FILE.

           IF ( QEO1-WEND-STEP = ZEROES    ) OR
              ( QEO1-WEND-STEP NOT NUMERIC )
              MOVE EO-PCODE             TO QEO1-WBEG-PCODE
              MOVE ALL "Z"              TO QEO1-WEND-PCODE
              MOVE EO-STEP              TO QEO1-WBEG-STEP
              MOVE ALL "9"              TO QEO1-WEND-STEP.

           EXEC SQL
            DECLARE EO1_GR_CURSOR CURSOR FOR
            SELECT ISNULL(EOFILE.EO_PCODE,''),
                   EOFILE.EO_STEP,
                   ISNULL(CAST(EOFILE.EO_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(EOFILE.EO_DIR,''),
                   ISNULL(EOFILE.EO_PROGRAM,''),
                   EOFILE.EO_COPIES,
                   ISNULL(EOFILE.EO_COMMENT,''),
                   ISNULL(EOFILE.EO_MESS,''),
                   ISNULL(CAST(EOFILE.EO_START_DATE
                                                  AS VARCHAR(10)),'0'),
                   EOFILE.EO_START_TIME,
                   ISNULL(CAST(EOFILE.EO_COMPLETE_DATE
                                                  AS VARCHAR(10)),'0'),
                   EOFILE.EO_COMPLETE_TIME,
                   ISNULL(EOFILE.EO_COMPLETE_STAT,''),
                   ISNULL(EOFILE.EO_PARAMETERS,''),
                   ISNULL(EOFILE.EO_ERROR_STEP_X,''),
                   ISNULL(EOFILE.EO_TRANSFER,''),
                   EOFILE.EO_LEVEL
            FROM DBO.EOFILE
      * WONT WORK WITH PCODE >, HAS TO BE >=
            WHERE EOFILE.EO_PCODE >= :QEO1-WBEG-PCODE
              AND EOFILE.EO_PCODE <= :QEO1-WEND-PCODE
              AND EOFILE.EO_STEP > :QEO1-WBEG-STEP
              AND EOFILE.EO_STEP <= :QEO1-WEND-STEP
            ORDER BY EOFILE.EO_PCODE,
                     EOFILE.EO_STEP
           END-EXEC.

           EXEC SQL
               OPEN EO1_GR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QEO1-WBEG-PCODE
                          QEO1-WEND-PCODE.

           MOVE ZEROES TO QEO1-WBEG-STEP
                          QEO1-WEND-STEP.

           MOVE STAT---GOOD TO EO1-GR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-EO1-FILE.
           PERFORM READ-IT.
           MOVE EO-SELECT-PATH TO E-FILE.
           MOVE EO1-KEY TO E-KEYX.

           MOVE EO-PCODE    TO QEO-PCODE. 
           MOVE EO-STEP     TO QEO-STEP.

           EXEC SQL
            SELECT ISNULL(EOFILE.EO_PCODE,''),
                   EOFILE.EO_STEP,
                   ISNULL(CAST(EOFILE.EO_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(EOFILE.EO_DIR,''),
                   ISNULL(EOFILE.EO_PROGRAM,''),
                   EOFILE.EO_COPIES,
                   ISNULL(EOFILE.EO_COMMENT,''),
                   ISNULL(EOFILE.EO_MESS,''),
                   ISNULL(CAST(EOFILE.EO_START_DATE
                                                  AS VARCHAR(10)),'0'),
                   EOFILE.EO_START_TIME,
                   ISNULL(CAST(EOFILE.EO_COMPLETE_DATE
                                                  AS VARCHAR(10)),'0'),
                   EOFILE.EO_COMPLETE_TIME,
                   ISNULL(EOFILE.EO_COMPLETE_STAT,''),
                   ISNULL(EOFILE.EO_PARAMETERS,''),
                   ISNULL(EOFILE.EO_ERROR_STEP_X,''),
                   ISNULL(EOFILE.EO_TRANSFER,''),
                   EOFILE.EO_LEVEL
            INTO :QEO-REC
            FROM DBO.EOFILE
            WHERE EOFILE.EO_PCODE = :QEO-PCODE
              AND EOFILE.EO_STEP = :QEO-STEP
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-EO1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-EO-FIELDS.

      *-----------------------------------------------------------------
       READ-EO1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE EO-SELECT-PATH TO E-FILE.
           MOVE EO1-KEY TO E-KEYX.
           INITIALIZE QEO-REC.

           IF ( EO1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH EO1_CURSOR INTO :QEO-REC
              END-EXEC
           ELSE
           IF ( EO1-GR-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH EO1_GR_CURSOR INTO :QEO-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-EO1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-EO-FIELDS.

      *-----------------------------------------------------------------
       WRITE-EO1-FILE.

           ACCEPT EO-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE EO-SELECT-PATH TO E-FILE.
           MOVE EO1-KEY TO E-KEYX.
           PERFORM SET-EO-FIELDS.

           EXEC SQL
            INSERT INTO DBO.EOFILE
             (EOFILE.EO_PCODE,
              EOFILE.EO_STEP,
              EOFILE.EO_LTOUCH_DATE,
              EOFILE.EO_DIR,
              EOFILE.EO_PROGRAM,
              EOFILE.EO_COPIES,
              EOFILE.EO_COMMENT,
              EOFILE.EO_MESS,
              EOFILE.EO_START_DATE,
              EOFILE.EO_START_TIME,
              EOFILE.EO_COMPLETE_DATE,
              EOFILE.EO_COMPLETE_TIME,
              EOFILE.EO_COMPLETE_STAT,
              EOFILE.EO_PARAMETERS,
              EOFILE.EO_ERROR_STEP_X,
              EOFILE.EO_TRANSFER,
              EOFILE.EO_LEVEL)
            VALUES
             (:QEO-PCODE,
              :QEO-STEP,
              NULLIF(:QEO-LTOUCH-DATE,'1900-01-01'),
              :QEO-DIR,
              :QEO-PROGRAM,
              :QEO-COPIES,
              :QEO-COMMENT,
              :QEO-MESS,
              NULLIF(:QEO-START-DATE,'1900-01-01'),
              :QEO-START-TIME,
              NULLIF(:QEO-COMPLETE-DATE,'1900-01-01'),
              :QEO-COMPLETE-TIME,
              :QEO-COMPLETE-STAT,
              :QEO-PARAMETERS,
              :QEO-ERROR-STEP-X,
              :QEO-TRANSFER,
              :QEO-LEVEL)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-EO1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-EO1-FILE.

           ACCEPT EO-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE EO-SELECT-PATH TO E-FILE.
           MOVE EO1-KEY TO E-KEYX.
           PERFORM SET-EO-FIELDS.

           EXEC SQL
            UPDATE DBO.EOFILE
             SET
              EOFILE.EO_PCODE = :QEO-PCODE,
              EOFILE.EO_STEP = :QEO-STEP,
              EOFILE.EO_LTOUCH_DATE =
                 NULLIF(:QEO-LTOUCH-DATE,'1900-01-01'),
              EOFILE.EO_DIR = :QEO-DIR,
              EOFILE.EO_PROGRAM = :QEO-PROGRAM,
              EOFILE.EO_COPIES = :QEO-COPIES,
              EOFILE.EO_COMMENT = :QEO-COMMENT,
              EOFILE.EO_MESS = :QEO-MESS,
              EOFILE.EO_START_DATE =
                 NULLIF(:QEO-START-DATE,'1900-01-01'),
              EOFILE.EO_START_TIME = :QEO-START-TIME,
              EOFILE.EO_COMPLETE_DATE =
                 NULLIF(:QEO-COMPLETE-DATE,'1900-01-01'),
              EOFILE.EO_COMPLETE_TIME = :QEO-COMPLETE-TIME,
              EOFILE.EO_COMPLETE_STAT = :QEO-COMPLETE-STAT,
              EOFILE.EO_PARAMETERS = :QEO-PARAMETERS,
              EOFILE.EO_ERROR_STEP_X = :QEO-ERROR-STEP-X,
              EOFILE.EO_TRANSFER = :QEO-TRANSFER,
              EOFILE.EO_LEVEL = :QEO-LEVEL
             WHERE EOFILE.EO_PCODE = :QEO-PCODE
               AND EOFILE.EO_STEP = :QEO-STEP
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-EO1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-EO1-FILE.

           PERFORM DELETE-IT.
           MOVE EO-SELECT-PATH TO E-FILE.
           MOVE EO1-KEY TO E-KEYX.
           PERFORM SET-EO-FIELDS.

           EXEC SQL
            DELETE FROM DBO.EOFILE
             WHERE EOFILE.EO_PCODE = :QEO-PCODE
               AND EOFILE.EO_STEP = :QEO-STEP
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-EO1-FILE.

      *-----------------------------------------------------------------
       CLOSE-EO1-FILE.
           IF ( EO1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE EO1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO EO1-CURSOR-STAT.

           IF ( EO1-GR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE EO1_GR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO EO1-GR-CURSOR-STAT.

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
              DESTROY IOEOIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
