       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOGQSR.
       AUTHOR.        AMM.
       DATE-WRITTEN.  04-05-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOGQSR
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : GQSFILE 
      * TABLE DESC    : DAILY COLLECTOR QUEUE FILE
      * OUTPUT FILE   : XPC/IOGQSR.PRE
      * USED FOR      : LIBSP/SPWSGQS.CPY  --- QGQS-REC
      *                 LIBSP/SPGQS1R.CPY  --- READ
      *                 LIBSP/SPGQS1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0405 ***NEW***
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
           "@(#)IO/IOGQSR.CBL S34 05/18/21-08:29:08 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBSP/SP01GQS.CPY".
       COPY "LIBSP/SPWSGQS.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S 01 SQL COPY MEMBERS
      *#################################################################
      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIBSP/SPWSGQS.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QGQS1-WHERE-BEG-KEY.
           03  QGQS1-WBEG-COLLID      PIC X(03).
 
       01  QGQS1-WHERE-END-KEY.
           03  QGQS1-WEND-COLLID      PIC X(03).
 
       01  QGQS-REC.
           03  QGQS-COLLID                   PIC X(3).
           03  QGQS-LTOUCH-DATE              PIC X(10).
           03  QGQS-TOT-REC                  PIC 9(5).
           03  QGQS-WORKED-REC               PIC 9(5).
           03  QGQS-TIMED-REC                PIC 9(5).
           03  QGQS-GROUP-ID                 PIC X(6).
           03  QGQS-GQ-COLLID                PIC XXX.
           03  QGQS-GQ-SEQ-FG                PIC X.
           03  QGQS-GQ-DATE                  PIC X(10).
           03  QGQS-GQ-START-TIME            PIC S9(4).
           03  QGQS-GQ-REV-DEL               PIC 9(5).
           03  QGQS-GQ-MISC-KEY-FLD          PIC X(3).
           03  QGQS-GQ-REV-PDUE-DAYS         PIC 9(5).
           03  QGQS-GQ-REV-BAL               PIC 9(7).
           03  QGQS-GQ-SEQ                   PIC 9(3).
           
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

       01  LEGEND-BEG-COLLID.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING COLLID     (F3-ALL) ".

       01  LEGEND-END-COLLID.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING COLLID     ".

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
           03  BEG-COLLID              PIC X(03).
           03  END-COLLID              PIC X(03).

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
           03  FILLER PIC X(30) VALUE "COLLID                        ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-GQS-COLLID                  PIC X(03).
           03  FILLER                          PIC X(01).
           03  D01-GQS-GQ-COLLID               PIC X(03).

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
       COPY "LIBIO/IOGQSR_DEF.CPY".
       COPY "LIBIO/IOGQSR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOGQSR_SCN.CPY".

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
           MOVE "B"                  TO GQS-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BEG-COLLID           TO QGQS1-WBEG-COLLID.
           MOVE END-COLLID           TO QGQS1-WEND-COLLID.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-GQS1-FILE.
           PERFORM START-GQS1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-GQS1-FILE-NEXT.

           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PROCESS-KEY = 1 )
              IF ( GQS-COLLID < BEG-COLLID )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( GQS-COLLID > END-COLLID )
                 GO TO MAIN-PROGRAM-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE GQS-COLLID                  TO D01-GQS-COLLID.
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
           MOVE "IOGQSR"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOGQSR-QUEUE-POS  TO Q-POS.
           MOVE IOGQSR-COPIES-POS TO C-POS.
           MOVE IOGQSR-PRU-POS    TO P-POS.
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
       ENTER-BEG-COLLID.

           MOVE LEGEND-BEG-COLLID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A030BEG-COLLID." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-COLLID.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-COLLID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-COLLID.

      *-----------------------------------------------------------------
       ENTER-END-COLLID.

           MOVE LEGEND-END-COLLID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A030END-COLLID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-COLLID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-COLLID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-COLLID.

           IF ( BEG-COLLID > END-COLLID )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-COLLID.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-COLLID.

           MOVE "S   A03BEG-COLLID." TO VDU.
           MOVE SPACES               TO BEG-COLLID VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A030END-COLLID." TO VDU.
           MOVE ALL "Z"              TO END-COLLID VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-COLLID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 < ZEROES ) OR
              ( VDUNUM0 > 2      )     
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A030BEG-COLLID."   TO VDU.
           MOVE BEG-COLLID             TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A030END-COLLID."   TO VDU.
           MOVE END-COLLID             TO VDUBUF.
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
           MOVE CUR-HH      TO PACE-TIME-HH.
           MOVE CUR-MM      TO PACE-TIME-MM.
           MOVE CUR-SS      TO PACE-TIME-SS.
           MOVE CUR-HD      TO PACE-TIME-HD.
           MOVE PACE-TIME   TO PACE-DETAIL-TIME.

           MOVE GQS1-KEY    TO PACE-DETAIL-KEY.

           MOVE PACE-DETAIL TO PACE-ELE(PACE-LEV).
           MOVE PACE-BUF    TO WR-BUF.
           MOVE PACE-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ZEROES      TO PACE-COUNTER.

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
           MOVE "BEGINNING COLLID              : "    TO R01-PROMPT.
           MOVE BEG-COLLID                            TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    COLLID              : "    TO R01-PROMPT.
           MOVE END-COLLID                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
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
           MOVE "SQL TABLE IO UTILITY - GQSFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOGQSR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOGQSR-SYSDATE.
           DISPLAY IOGQSR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOGQSR-FORM-FIELDS TO WR-BUF.
           MOVE IOGQSR-FORM-LIST TO WR-LIST.
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

           MOVE IOGQSR-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOGQSR-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOGQSR_EVA.CPY".
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
      *    LOAD GQS-REC FROM QGQS-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-GQS-FIELDS.
           MOVE QGQS-COLLID TO GQS-COLLID.

           IF ( QGQS-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO GQS-LTOUCH-DATE
           ELSE
              MOVE QGQS-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GQS-LTOUCH-DATE.

           MOVE QGQS-TOT-REC TO GQS-TOT-REC.
           MOVE QGQS-WORKED-REC TO GQS-WORKED-REC.
           MOVE QGQS-TIMED-REC TO GQS-TIMED-REC.
           MOVE QGQS-GROUP-ID TO GQS-GROUP-ID.
           MOVE QGQS-GQ-COLLID TO GQS-GQ-COLLID.
           MOVE QGQS-GQ-SEQ-FG TO GQS-GQ-SEQ-FG.

           IF ( QGQS-GQ-DATE = "1900-01-01" )
              MOVE ZEROES TO GQS-GQ-DATE
           ELSE
              MOVE QGQS-GQ-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GQS-GQ-DATE.

           MOVE QGQS-GQ-START-TIME TO GQS-GQ-START-TIME.
           MOVE QGQS-GQ-REV-DEL TO GQS-GQ-REV-DEL.
           MOVE QGQS-GQ-MISC-KEY-FLD TO GQS-GQ-MISC-KEY-FLD.
           MOVE QGQS-GQ-REV-PDUE-DAYS TO GQS-GQ-REV-PDUE-DAYS.
           MOVE QGQS-GQ-REV-BAL TO GQS-GQ-REV-BAL.
           MOVE QGQS-GQ-SEQ TO GQS-GQ-SEQ.
 
      *-----------------------------------------------------------------
      *    LOAD QGQS-REC FROM GQS-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-GQS-FIELDS.
           MOVE GQS-COLLID TO QGQS-COLLID.

           IF ( GQS-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QGQS-LTOUCH-DATE
           ELSE
              MOVE GQS-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGQS-LTOUCH-DATE.

           MOVE GQS-TOT-REC TO QGQS-TOT-REC.
           MOVE GQS-WORKED-REC TO QGQS-WORKED-REC.
           MOVE GQS-TIMED-REC TO QGQS-TIMED-REC.
           MOVE GQS-GROUP-ID TO QGQS-GROUP-ID.
           MOVE GQS-GQ-COLLID TO QGQS-GQ-COLLID.
           MOVE GQS-GQ-SEQ-FG TO QGQS-GQ-SEQ-FG.

           IF ( GQS-GQ-DATE = ZEROES )
              MOVE "1900-01-01" TO QGQS-GQ-DATE
           ELSE
              MOVE GQS-GQ-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGQS-GQ-DATE.

           MOVE GQS-GQ-START-TIME TO QGQS-GQ-START-TIME.
           MOVE GQS-GQ-REV-DEL TO QGQS-GQ-REV-DEL.
           MOVE GQS-GQ-MISC-KEY-FLD TO QGQS-GQ-MISC-KEY-FLD.
           MOVE GQS-GQ-REV-PDUE-DAYS TO QGQS-GQ-REV-PDUE-DAYS.
           MOVE GQS-GQ-REV-BAL TO QGQS-GQ-REV-BAL.
           MOVE GQS-GQ-SEQ TO QGQS-GQ-SEQ.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBSP/SPGQS1RN
       LOAD-GQS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GQS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GQS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO GQS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GQS-ALL-MACHINE
              MOVE GQS-ALL-PATH            TO GQS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GQS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO GQS-ALL-BASE
              MOVE FILPATH-MACHINE         TO GQS-ALL-MACHINE
              MOVE GQS-ALL-PATH            TO GQS-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-GQSFILE NOT = "S" )
              MOVE GQS-PATH-BEG             TO GQS-SELECT-PATH-BEG
              MOVE GQS-PATH-FILE            TO GQS-SELECT-PATH-FILE
           ELSE
              MOVE GQS-PATH-SQL             TO GQS-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-GQS1-FILE.
           PERFORM LOAD-GQS1-FILE.
           PERFORM OPEN-IT.
           MOVE GQS-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GQS1-FILE.
           PERFORM START-IT.
           MOVE GQS-SELECT-PATH TO E-FILE.
           MOVE GQS1-KEY TO E-KEYX.

           IF ( GQS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GQS1-FILE.

           IF ( QGQS1-WEND-COLLID = SPACES )
              MOVE GQS-COLLID           TO QGQS1-WBEG-COLLID
              MOVE ALL "X"              TO QGQS1-WEND-COLLID.

           EXEC SQL
            DECLARE GQS1_CURSOR CURSOR FOR
            SELECT ISNULL(GQSFILE.GQS_COLLID,''),
                   ISNULL(CAST(GQSFILE.GQS_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   GQSFILE.GQS_TOT_REC,
                   GQSFILE.GQS_WORKED_REC,
                   GQSFILE.GQS_TIMED_REC,
                   ISNULL(GQSFILE.GQS_GROUP_ID,''),
                   ISNULL(GQSFILE.GQS_GQ_COLLID,''),
                   ISNULL(GQSFILE.GQS_GQ_SEQ_FG,''),
                   ISNULL(CAST(GQSFILE.GQS_GQ_DATE
                                                  AS VARCHAR(10)),'0'),
                   GQSFILE.GQS_GQ_START_TIME,
                   GQSFILE.GQS_GQ_REV_DEL,
                   ISNULL(GQSFILE.GQS_GQ_MISC_KEY_FLD,''),
                   GQSFILE.GQS_GQ_REV_PDUE_DAYS,
                   GQSFILE.GQS_GQ_REV_BAL,
                   GQSFILE.GQS_GQ_SEQ
            FROM DBO.GQSFILE
            WHERE GQSFILE.GQS_COLLID >= :QGQS1-WBEG-COLLID
              AND GQSFILE.GQS_COLLID <= :QGQS1-WEND-COLLID
            ORDER BY GQSFILE.GQS_COLLID
           END-EXEC.

           EXEC SQL
               OPEN GQS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGQS1-WBEG-COLLID
                          QGQS1-WEND-COLLID.

           MOVE STAT---GOOD TO GQS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GQS1-FILE.
           PERFORM READ-IT.
           MOVE GQS-SELECT-PATH TO E-FILE.
           MOVE GQS1-KEY TO E-KEYX.

           MOVE GQS-GQ-COLLID     TO QGQS-GQ-COLLID. 

           EXEC SQL
            SELECT ISNULL(GQSFILE.GQS_COLLID,''),
                   ISNULL(CAST(GQSFILE.GQS_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   GQSFILE.GQS_TOT_REC,
                   GQSFILE.GQS_WORKED_REC,
                   GQSFILE.GQS_TIMED_REC,
                   ISNULL(GQSFILE.GQS_GROUP_ID,''),
                   ISNULL(GQSFILE.GQS_GQ_COLLID,''),
                   ISNULL(GQSFILE.GQS_GQ_SEQ_FG,''),
                   ISNULL(CAST(GQSFILE.GQS_GQ_DATE
                                                  AS VARCHAR(10)),'0'),
                   GQSFILE.GQS_GQ_START_TIME,
                   GQSFILE.GQS_GQ_REV_DEL,
                   ISNULL(GQSFILE.GQS_GQ_MISC_KEY_FLD,''),
                   GQSFILE.GQS_GQ_REV_PDUE_DAYS,
                   GQSFILE.GQS_GQ_REV_BAL,
                   GQSFILE.GQS_GQ_SEQ
            INTO :QGQS-REC
            FROM DBO.GQSFILE
            WHERE GQSFILE.GQS_GQ_COLLID = :QGQS-GQ-COLLID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GQS1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GQS-FIELDS.

      *-----------------------------------------------------------------
       READ-GQS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GQS-SELECT-PATH TO E-FILE.
           MOVE GQS1-KEY TO E-KEYX.
           INITIALIZE QGQS-REC.

           IF ( GQS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GQS1_CURSOR INTO :QGQS-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GQS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GQS-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GQS1-FILE.
           IF ( GQS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GQS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GQS1-CURSOR-STAT.

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
              DESTROY IOGQSR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
