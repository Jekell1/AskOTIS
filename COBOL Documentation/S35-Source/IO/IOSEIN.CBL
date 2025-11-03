       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOSEIN.
       AUTHOR.        BAH.
       DATE-WRITTEN.  03-11-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOSEIN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : SEFILE 
      * TABLE DESC    : USER SECURITY 
      * OUTPUT FILE   : XPC/IOSEIN.PRE
      * USED FOR      : LIBGB/GBWSSE.CPY  --- QSE-REC
      *                 LIBGB/GBSE1R.CPY  --- READ
      *                 LIBGB/GBSE1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0311 ***NEW***
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
           "@(#)IO/IOSEIN.CBL S34 05/18/21-11:15:42 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01SE.CPY".
       COPY "LIBGB/GBWSSE.CPY".

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
       01  QSE1-WHERE-BEG-KEY.
           03  QSE1-WBEG-FORM           PIC X(6).

       01  QSE1-WHERE-END-KEY.
           03  QSE1-WEND-FORM           PIC X(6).

       01  QSE-REC.
           03  QSE-FORM                 PIC X(06).
           03  QSE-LTOUCH-DATE          PIC X(10).
           03  QSE-EXP-DATE             PIC X(10).
           03  QSE-PURCHASABLE          PIC X(01).
           03  QSE-PROF-1               PIC X.
           03  QSE-PROF-2               PIC X.
           03  QSE-PROF-3               PIC X.
           03  QSE-PROF-4               PIC X.
           03  QSE-PROF-5               PIC X.
           03  QSE-PROF-6               PIC X.
           03  QSE-PROF-7               PIC X.
           03  QSE-PROF-8               PIC X.
           03  QSE-PROF-9               PIC X.
           03  QSE-PROF-10              PIC X.
           03  QSE-PROF-11               PIC X.
           03  QSE-PROF-12               PIC X.
           03  QSE-PROF-13               PIC X.
           03  QSE-PROF-14               PIC X.
           03  QSE-PROF-15               PIC X.
           03  QSE-PROF-16               PIC X.
           03  QSE-PROF-17               PIC X.
           03  QSE-PROF-18               PIC X.
           03  QSE-PROF-19               PIC X.
           03  QSE-PROF-20               PIC X.
           03  QSE-PROF-21               PIC X.
           03  QSE-PROF-22               PIC X.
           03  QSE-PROF-23               PIC X.
           03  QSE-PROF-24               PIC X.
           03  QSE-PROF-25               PIC X.
           03  QSE-PROF-26               PIC X.
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

       01  LEGEND-BEG-PROGRAM. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING PROGRAM    (F3-ALL) ".

       01  LEGEND-END-PROGRAM.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING PROGRAM    ".

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
           03  BEG-PROGRAM               PIC X(06).
           03  END-PROGRAM               PIC X(06).

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
           03  FILLER PIC X(31) VALUE "PROGRAM  LTOUCH     EXPIRE".
           03  FILLER PIC X(30) VALUE "P   1   2   3   25  26  F ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-SE-FORM                    PIC X(6).
           03  FILLER                         PIC X(03).
           03  D01-SE-LTOUCH-DATE             PIC 9(8).
           03  FILLER                         PIC X(03).
           03  D01-SE-EXP-DATE                PIC 9(8).
           03  FILLER                         PIC X(03).
           03  D01-SE-PURCHASABLE             PIC X.
           03  FILLER                         PIC X(03).
           03  D01-SE-PROF-1                  PIC X.
           03  FILLER                         PIC X(03).
           03  D01-SE-PROF-2                  PIC X.
           03  FILLER                         PIC X(03).
           03  D01-SE-PROF-3                  PIC X.
           03  FILLER                         PIC X(03).
           03  D01-SE-PROF-25                 PIC X.
           03  FILLER                         PIC X(03).
           03  D01-SE-PROF-26                 PIC X.
           03  FILLER                         PIC X(03).

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
       COPY "LIBIO/IOSEIN_DEF.CPY".
       COPY "LIBIO/IOSEIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOSEIN_SCN.CPY".

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
           MOVE "B"                        TO SE-PATH-OVERRIDE.

           MOVE BEG-PROGRAM                TO QSE1-WBEG-FORM.
           MOVE END-PROGRAM                TO QSE1-WEND-FORM.

           PERFORM OPEN-SE1-FILE.
           PERFORM START-SE1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-SE1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SE-FORM < BEG-PROGRAM )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( SE-FORM > END-PROGRAM )
              GO TO MAIN-PROGRAM-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE SE-FORM               TO D01-SE-FORM.
           MOVE SE-LTOUCH-DATE        TO D01-SE-LTOUCH-DATE.
           MOVE SE-EXP-DATE           TO D01-SE-EXP-DATE.
           MOVE SE-PURCHASABLE        TO D01-SE-PURCHASABLE.
           MOVE SE-PROF(1)             TO D01-SE-PROF-1.
           MOVE SE-PROF(2)             TO D01-SE-PROF-2.
           MOVE SE-PROF(3)             TO D01-SE-PROF-3.
           MOVE SE-PROF(25)            TO D01-SE-PROF-25.
           MOVE SE-PROF(26)            TO D01-SE-PROF-26.
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
           MOVE "IOSEIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOSEIN-QUEUE-POS  TO Q-POS.
           MOVE IOSEIN-COPIES-POS TO C-POS.
           MOVE IOSEIN-PRU-POS    TO P-POS.
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
       ENTER-BEG-PROGRAM.

           MOVE LEGEND-BEG-PROGRAM TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A060BEG-PROGRAM." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-PROGRAM.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-PROGRAM.

           MOVE VDUBUF TO BEG-PROGRAM.

      *-----------------------------------------------------------------
       ENTER-END-PROGRAM.

           MOVE LEGEND-END-PROGRAM TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040END-PROGRAM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-PROGRAM.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-PROGRAM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-PROGRAM.

           IF ( BEG-PROGRAM > END-PROGRAM )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-PROGRAM.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-PROGRAM.

           MOVE "S  A060BEG-PROGRAM." TO VDU.
           MOVE SPACES               TO BEG-PROGRAM VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A060END-PROGRAM." TO VDU.
           MOVE ALL "Z"              TO END-PROGRAM VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-PROGRAM.
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

           MOVE "S  A000BEG-PROGRAM."    TO VDU.
           MOVE BEG-PROGRAM              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-PROGRAM."    TO VDU.
           MOVE END-PROGRAM              TO VDUBUF.
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

           MOVE SE1-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING PROGRAM               : "    TO R01-PROMPT.
           MOVE BEG-PROGRAM                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    PROGRAM               : "    TO R01-PROMPT.
           MOVE END-PROGRAM                             TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - SEFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOSEIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOSEIN-SYSDATE.
           DISPLAY IOSEIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOSEIN-FORM-FIELDS TO WR-BUF.
           MOVE IOSEIN-FORM-LIST TO WR-LIST.
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

           MOVE IOSEIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOSEIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOSEIN_EVA.CPY".
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
      *    LOAD SE-REC FROM QSE-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-SE-FIELDS.
           MOVE QSE-FORM TO SE-FORM.

           IF ( QSE-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO SE-LTOUCH-DATE
           ELSE
              MOVE QSE-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO SE-LTOUCH-DATE.


           IF ( QSE-EXP-DATE = "1900-01-01" )
              MOVE ZEROES TO SE-EXP-DATE
           ELSE
              MOVE QSE-EXP-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO SE-EXP-DATE.

           MOVE QSE-PURCHASABLE TO SE-PURCHASABLE.
           MOVE QSE-PROF-1 TO SE-PROF(1).
           MOVE QSE-PROF-2 TO SE-PROF(2).
           MOVE QSE-PROF-3 TO SE-PROF(3).
           MOVE QSE-PROF-4 TO SE-PROF(4).
           MOVE QSE-PROF-5 TO SE-PROF(5).
           MOVE QSE-PROF-6 TO SE-PROF(6).
           MOVE QSE-PROF-7 TO SE-PROF(7).
           MOVE QSE-PROF-8 TO SE-PROF(8).
           MOVE QSE-PROF-9 TO SE-PROF(9).
           MOVE QSE-PROF-10 TO SE-PROF(10).
           MOVE QSE-PROF-11 TO SE-PROF(11).
           MOVE QSE-PROF-12 TO SE-PROF(12).
           MOVE QSE-PROF-13 TO SE-PROF(13).
           MOVE QSE-PROF-14 TO SE-PROF(14).
           MOVE QSE-PROF-15 TO SE-PROF(15).
           MOVE QSE-PROF-16 TO SE-PROF(16).
           MOVE QSE-PROF-17 TO SE-PROF(17).
           MOVE QSE-PROF-18 TO SE-PROF(18).
           MOVE QSE-PROF-19 TO SE-PROF(19).
           MOVE QSE-PROF-20 TO SE-PROF(20).
           MOVE QSE-PROF-21 TO SE-PROF(21).
           MOVE QSE-PROF-22 TO SE-PROF(22).
           MOVE QSE-PROF-23 TO SE-PROF(23).
           MOVE QSE-PROF-24 TO SE-PROF(24).
           MOVE QSE-PROF-25 TO SE-PROF(25).
           MOVE QSE-PROF-26 TO SE-PROF(26).
 
      *-----------------------------------------------------------------
      *    LOAD QSE-REC FROM SE-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-SE-FIELDS.
           MOVE SE-FORM TO QSE-FORM.

           IF ( SE-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QSE-LTOUCH-DATE
           ELSE
              MOVE SE-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QSE-LTOUCH-DATE.


           IF ( SE-EXP-DATE = ZEROES )
              MOVE "1900-01-01" TO QSE-EXP-DATE
           ELSE
              MOVE SE-EXP-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QSE-EXP-DATE.

           MOVE SE-PURCHASABLE TO QSE-PURCHASABLE.
           MOVE SE-PROF(1) TO QSE-PROF-1.
           MOVE SE-PROF(2) TO QSE-PROF-2.
           MOVE SE-PROF(3) TO QSE-PROF-3.
           MOVE SE-PROF(4) TO QSE-PROF-4.
           MOVE SE-PROF(5) TO QSE-PROF-5.
           MOVE SE-PROF(6) TO QSE-PROF-6.
           MOVE SE-PROF(7) TO QSE-PROF-7.
           MOVE SE-PROF(8) TO QSE-PROF-8.
           MOVE SE-PROF(9) TO QSE-PROF-9.
           MOVE SE-PROF(10) TO QSE-PROF-10.
           MOVE SE-PROF(11) TO QSE-PROF-11.
           MOVE SE-PROF(12) TO QSE-PROF-12.
           MOVE SE-PROF(13) TO QSE-PROF-13.
           MOVE SE-PROF(14) TO QSE-PROF-14.
           MOVE SE-PROF(15) TO QSE-PROF-15.
           MOVE SE-PROF(16) TO QSE-PROF-16.
           MOVE SE-PROF(17) TO QSE-PROF-17.
           MOVE SE-PROF(18) TO QSE-PROF-18.
           MOVE SE-PROF(19) TO QSE-PROF-19.
           MOVE SE-PROF(20) TO QSE-PROF-20.
           MOVE SE-PROF(21) TO QSE-PROF-21.
           MOVE SE-PROF(22) TO QSE-PROF-22.
           MOVE SE-PROF(23) TO QSE-PROF-23.
           MOVE SE-PROF(24) TO QSE-PROF-24.
           MOVE SE-PROF(25) TO QSE-PROF-25.
           MOVE SE-PROF(26) TO QSE-PROF-26.
 

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
       LOAD-SE1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SE-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SE-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SE-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SE-ALL-MACHINE
              MOVE SE-ALL-PATH             TO SE-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SE-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SE-ALL-BASE
              MOVE FILPATH-MACHINE         TO SE-ALL-MACHINE
              MOVE SE-ALL-PATH             TO SE-PATH.
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*G2G  *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-SEFILE NOT = "S" )
*G2G          MOVE SE-PATH-BEG             TO SE-SELECT-PATH-BEG
*G2G          MOVE SE-PATH-FILE            TO SE-SELECT-PATH-FILE
*G2G       ELSE
*G2G          MOVE SE-PATH-SQL             TO SE-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-SE1-FILE.
           PERFORM LOAD-SE1-FILE.
           PERFORM OPEN-IT.
           MOVE SE-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-SE1-FILE.
           PERFORM START-IT.
           MOVE SE-SELECT-PATH TO E-FILE.
           MOVE SE1-KEY TO E-KEYX.

           IF ( SE1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SE1-FILE.

           IF ( QSE1-WEND-FORM = SPACES    )
               MOVE SE-FORM   TO QSE1-WBEG-FORM
               MOVE ALL "Z"   TO QSE1-WEND-FORM.

           EXEC SQL
            DECLARE SE1_CURSOR CURSOR FOR
            SELECT ISNULL(SEFILE.SE_FORM,''),
                   ISNULL(CAST(SEFILE.SE_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(SEFILE.SE_EXP_DATE AS VARCHAR(10)),'0'),
                   ISNULL(SEFILE.SE_PURCHASABLE,''),
                   ISNULL(SEFILE.SE_PROF_1,''),
                   ISNULL(SEFILE.SE_PROF_2,''),
                   ISNULL(SEFILE.SE_PROF_3,''),
                   ISNULL(SEFILE.SE_PROF_4,''),
                   ISNULL(SEFILE.SE_PROF_5,''),
                   ISNULL(SEFILE.SE_PROF_6,''),
                   ISNULL(SEFILE.SE_PROF_7,''),
                   ISNULL(SEFILE.SE_PROF_8,''),
                   ISNULL(SEFILE.SE_PROF_9,''),
                   ISNULL(SEFILE.SE_PROF_10,''),
                   ISNULL(SEFILE.SE_PROF_11,''),
                   ISNULL(SEFILE.SE_PROF_12,''),
                   ISNULL(SEFILE.SE_PROF_13,''),
                   ISNULL(SEFILE.SE_PROF_14,''),
                   ISNULL(SEFILE.SE_PROF_15,''),
                   ISNULL(SEFILE.SE_PROF_16,''),
                   ISNULL(SEFILE.SE_PROF_17,''),
                   ISNULL(SEFILE.SE_PROF_18,''),
                   ISNULL(SEFILE.SE_PROF_19,''),
                   ISNULL(SEFILE.SE_PROF_20,''),
                   ISNULL(SEFILE.SE_PROF_21,''),
                   ISNULL(SEFILE.SE_PROF_22,''),
                   ISNULL(SEFILE.SE_PROF_23,''),
                   ISNULL(SEFILE.SE_PROF_24,''),
                   ISNULL(SEFILE.SE_PROF_25,''),
                   ISNULL(SEFILE.SE_PROF_26,'')
            FROM DBO.SEFILE
            WHERE SEFILE.SE_FORM >= :QSE1-WBEG-FORM
              AND SEFILE.SE_FORM <= :QSE1-WEND-FORM
            ORDER BY SEFILE.SE_FORM
           END-EXEC.

           EXEC SQL
               OPEN SE1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QSE1-WBEG-FORM
                          QSE1-WEND-FORM.

           MOVE STAT---GOOD TO SE1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SE1-FILE.
           PERFORM READ-IT.
           MOVE SE-SELECT-PATH TO E-FILE.
           MOVE SE1-KEY TO E-KEYX.

           MOVE SE-FORM    TO QSE-FORM.

           EXEC SQL
            SELECT ISNULL(SEFILE.SE_FORM,''),
                   ISNULL(CAST(SEFILE.SE_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(SEFILE.SE_EXP_DATE AS VARCHAR(10)),'0'),
                   ISNULL(SEFILE.SE_PURCHASABLE,''),
                   ISNULL(SEFILE.SE_PROF_1,''),
                   ISNULL(SEFILE.SE_PROF_2,''),
                   ISNULL(SEFILE.SE_PROF_3,''),
                   ISNULL(SEFILE.SE_PROF_4,''),
                   ISNULL(SEFILE.SE_PROF_5,''),
                   ISNULL(SEFILE.SE_PROF_6,''),
                   ISNULL(SEFILE.SE_PROF_7,''),
                   ISNULL(SEFILE.SE_PROF_8,''),
                   ISNULL(SEFILE.SE_PROF_9,''),
                   ISNULL(SEFILE.SE_PROF_10,''),
                   ISNULL(SEFILE.SE_PROF_11,''),
                   ISNULL(SEFILE.SE_PROF_12,''),
                   ISNULL(SEFILE.SE_PROF_13,''),
                   ISNULL(SEFILE.SE_PROF_14,''),
                   ISNULL(SEFILE.SE_PROF_15,''),
                   ISNULL(SEFILE.SE_PROF_16,''),
                   ISNULL(SEFILE.SE_PROF_17,''),
                   ISNULL(SEFILE.SE_PROF_18,''),
                   ISNULL(SEFILE.SE_PROF_19,''),
                   ISNULL(SEFILE.SE_PROF_20,''),
                   ISNULL(SEFILE.SE_PROF_21,''),
                   ISNULL(SEFILE.SE_PROF_22,''),
                   ISNULL(SEFILE.SE_PROF_23,''),
                   ISNULL(SEFILE.SE_PROF_24,''),
                   ISNULL(SEFILE.SE_PROF_25,''),
                   ISNULL(SEFILE.SE_PROF_26,'')
            INTO :QSE-REC
            FROM DBO.SEFILE
            WHERE SEFILE.SE_FORM = :QSE-FORM
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SE1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SE-FIELDS.

      *-----------------------------------------------------------------
       READ-SE1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SE-SELECT-PATH TO E-FILE.
           MOVE SE1-KEY TO E-KEYX.
           INITIALIZE QSE-REC.

           IF ( SE1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SE1_CURSOR INTO :QSE-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SE1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-SE-FIELDS.

      *-----------------------------------------------------------------
       WRITE-SE1-FILE.

           ACCEPT SE-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE SE-SELECT-PATH TO E-FILE.
           MOVE SE1-KEY TO E-KEYX.
           PERFORM SET-SE-FIELDS.

           EXEC SQL
            INSERT INTO DBO.SEFILE
             (SEFILE.SE_FORM,
              SEFILE.SE_LTOUCH_DATE,
              SEFILE.SE_EXP_DATE,
              SEFILE.SE_PURCHASABLE,
              SEFILE.SE_PROF_1,
              SEFILE.SE_PROF_2,
              SEFILE.SE_PROF_3,
              SEFILE.SE_PROF_4,
              SEFILE.SE_PROF_5,
              SEFILE.SE_PROF_6,
              SEFILE.SE_PROF_7,
              SEFILE.SE_PROF_8,
              SEFILE.SE_PROF_9,
              SEFILE.SE_PROF_10,
              SEFILE.SE_PROF_11,
              SEFILE.SE_PROF_12,
              SEFILE.SE_PROF_13,
              SEFILE.SE_PROF_14,
              SEFILE.SE_PROF_15,
              SEFILE.SE_PROF_16,
              SEFILE.SE_PROF_17,
              SEFILE.SE_PROF_18,
              SEFILE.SE_PROF_19,
              SEFILE.SE_PROF_20,
              SEFILE.SE_PROF_21,
              SEFILE.SE_PROF_22,
              SEFILE.SE_PROF_23,
              SEFILE.SE_PROF_24,
              SEFILE.SE_PROF_25,
              SEFILE.SE_PROF_26)
            VALUES
             (:QSE-FORM,
              NULLIF(:QSE-LTOUCH-DATE,'1900-01-01'),
              NULLIF(:QSE-EXP-DATE,'1900-01-01'),
              :QSE-PURCHASABLE,
              :QSE-PROF-1,
              :QSE-PROF-2,
              :QSE-PROF-3,
              :QSE-PROF-4,
              :QSE-PROF-5,
              :QSE-PROF-6,
              :QSE-PROF-7,
              :QSE-PROF-8,
              :QSE-PROF-9,
              :QSE-PROF-10,
              :QSE-PROF-11,
              :QSE-PROF-12,
              :QSE-PROF-13,
              :QSE-PROF-14,
              :QSE-PROF-15,
              :QSE-PROF-16,
              :QSE-PROF-17,
              :QSE-PROF-18,
              :QSE-PROF-19,
              :QSE-PROF-20,
              :QSE-PROF-21,
              :QSE-PROF-22,
              :QSE-PROF-23,
              :QSE-PROF-24,
              :QSE-PROF-25,
              :QSE-PROF-26)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-SE1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-SE1-FILE.

           ACCEPT SE-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE SE-SELECT-PATH TO E-FILE.
           MOVE SE1-KEY TO E-KEYX.
           PERFORM SET-SE-FIELDS.

           EXEC SQL
            UPDATE DBO.SEFILE
             SET
              SEFILE.SE_FORM = :QSE-FORM,
              SEFILE.SE_LTOUCH_DATE =
                 NULLIF(:QSE-LTOUCH-DATE,'1900-01-01'),
              SEFILE.SE_EXP_DATE =
                 NULLIF(:QSE-EXP-DATE,'1900-01-01'),
              SEFILE.SE_PURCHASABLE = :QSE-PURCHASABLE,
              SEFILE.SE_PROF_1 = :QSE-PROF-1,
              SEFILE.SE_PROF_2 = :QSE-PROF-2,
              SEFILE.SE_PROF_3 = :QSE-PROF-3,
              SEFILE.SE_PROF_4 = :QSE-PROF-4,
              SEFILE.SE_PROF_5 = :QSE-PROF-5,
              SEFILE.SE_PROF_6 = :QSE-PROF-6,
              SEFILE.SE_PROF_7 = :QSE-PROF-7,
              SEFILE.SE_PROF_8 = :QSE-PROF-8,
              SEFILE.SE_PROF_9 = :QSE-PROF-9,
              SEFILE.SE_PROF_10 = :QSE-PROF-10,
              SEFILE.SE_PROF_11 = :QSE-PROF-11,
              SEFILE.SE_PROF_12 = :QSE-PROF-12,
              SEFILE.SE_PROF_13 = :QSE-PROF-13,
              SEFILE.SE_PROF_14 = :QSE-PROF-14,
              SEFILE.SE_PROF_15 = :QSE-PROF-15,
              SEFILE.SE_PROF_16 = :QSE-PROF-16,
              SEFILE.SE_PROF_17 = :QSE-PROF-17,
              SEFILE.SE_PROF_18 = :QSE-PROF-18,
              SEFILE.SE_PROF_19 = :QSE-PROF-19,
              SEFILE.SE_PROF_20 = :QSE-PROF-20,
              SEFILE.SE_PROF_21 = :QSE-PROF-21,
              SEFILE.SE_PROF_22 = :QSE-PROF-22,
              SEFILE.SE_PROF_23 = :QSE-PROF-23,
              SEFILE.SE_PROF_24 = :QSE-PROF-24,
              SEFILE.SE_PROF_25 = :QSE-PROF-25,
              SEFILE.SE_PROF_26 = :QSE-PROF-26
            WHERE SEFILE.SE_FORM = :QSE-FORM
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-SE1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-SE1-FILE.

           PERFORM DELETE-IT.
           MOVE SE-SELECT-PATH TO E-FILE.
           MOVE SE1-KEY TO E-KEYX.
           PERFORM SET-SE-FIELDS.

           EXEC SQL
            DELETE FROM DBO.SEFILE
            WHERE SEFILE.SE_FORM = :QSE-FORM
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-SE1-FILE.

      *-----------------------------------------------------------------
       CLOSE-SE1-FILE.
           IF ( SE1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SE1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SE1-CURSOR-STAT.

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
              DESTROY IOSEIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
