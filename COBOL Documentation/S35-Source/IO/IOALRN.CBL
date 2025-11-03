       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOALRN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  03-10-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOALRN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : ALFILE 
      * TABLE DESC    : AL FILE
      * OUTPUT FILE   : XPC/IOALRN.PRE
      * USED FOR      : LIBGL/GLWSAL.CPY  --- QAL-REC
      *                 LIBGL/GLAL1R.CPY  --- READ
      *                 LIBGL/GLAL1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0310 ***NEW***
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
           "@(#)IO/IOALRN.CBL S34 05/17/21-14:49:13 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGL/GL01AL.CPY".
       COPY "LIBGL/GLWSAL.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S WS COPY MEMBERS
      *#################################################################
      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIBGL/GLWSAL.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QAL1-WHERE-BEG-KEY.
           03  QAL1-WBEG-ID           PIC X(12).
 
       01  QAL1-WHERE-END-KEY.
           03  QAL1-WEND-ID           PIC X(12).
 
       01  QAL-REC.
           03  QAL-ID                PIC X(12).
           03  QAL-LTOUCH-DATE       PIC X(10).
           03  QAL-DESC              PIC X(30).
           03  QAL-NO-ACCTS          PIC 99.
           03  QAL-ACCT-1             PIC 9(11).
           03  QAL-PCT-1              PIC S9(3)V99.
           03  QAL-ACCT-2             PIC 9(11).
           03  QAL-PCT-2              PIC S9(3)V99.
           03  QAL-ACCT-3             PIC 9(11).
           03  QAL-PCT-3              PIC S9(3)V99.
           03  QAL-ACCT-4             PIC 9(11).
           03  QAL-PCT-4              PIC S9(3)V99.
           03  QAL-ACCT-5             PIC 9(11).
           03  QAL-PCT-5              PIC S9(3)V99.
           03  QAL-ACCT-6             PIC 9(11).
           03  QAL-PCT-6              PIC S9(3)V99.
           03  QAL-ACCT-7             PIC 9(11).
           03  QAL-PCT-7              PIC S9(3)V99.
           03  QAL-ACCT-8             PIC 9(11).
           03  QAL-PCT-8              PIC S9(3)V99.
           03  QAL-ACCT-9             PIC 9(11).
           03  QAL-PCT-9              PIC S9(3)V99.
           03  QAL-ACCT-10            PIC 9(11).
           03  QAL-PCT-10             PIC S9(3)V99.
           03  QAL-TYPE              PIC X.

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

       01  LEGEND-BEG-ID.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING ID         (F3-ALL) ".

       01  LEGEND-END-ID.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING ID         ".

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
           03  BEG-ID                  PIC X(12).
           03  END-ID                  PIC X(12).

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
           03  FILLER PIC X(30) VALUE "AL-ID LAST TOUCH DESC         ".
           03  FILLER PIC X(30) VALUE "                  TYPE        ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-AL-ID                      PIC X(12).
           03  FILLER                         PIC X(01).
           03  D01-AL-LTOUCH-DATE             PIC 9(08).
           03  FILLER                         PIC X(01).
           03  D01-AL-DESC                    PIC X(30).
           03  FILLER                         PIC X(01).
           03  D01-AL-TYPE                    PIC X(1).

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
       COPY "LIBIO/IOALRN_DEF.CPY".
       COPY "LIBIO/IOALRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOALRN_SCN.CPY".

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
           MOVE "B"                        TO AL-PATH-OVERRIDE.

           MOVE BEG-ID                     TO QAL1-WBEG-ID.
           MOVE END-ID                     TO QAL1-WEND-ID.

           PERFORM OPEN-AL1-FILE.
           PERFORM START-AL1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-AL1-FILE-NEXT.

           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( AL-ID < BEG-ID )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( AL-ID > END-ID )
              GO TO MAIN-PROGRAM-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE AL-ID                      TO D01-AL-ID.
           MOVE AL-LTOUCH-DATE             TO D01-AL-LTOUCH-DATE.
           MOVE AL-DESC                    TO D01-AL-DESC.
           MOVE AL-TYPE                    TO D01-AL-TYPE.
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
           MOVE "IOALRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOALRN-QUEUE-POS  TO Q-POS.
           MOVE IOALRN-COPIES-POS TO C-POS.
           MOVE IOALRN-PRU-POS    TO P-POS.
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
       ENTER-BEG-ID.
           MOVE LEGEND-BEG-ID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040BEG-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-ID.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-ID.

      *-----------------------------------------------------------------
       ENTER-END-ID.

           MOVE LEGEND-END-ID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040END-ID."    TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-ID.

           IF ( BEG-ID > END-ID )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ID.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-ID.

           MOVE "S  A040BEG-ID." TO VDU.
           MOVE SPACES               TO BEG-ID VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040END-ID." TO VDU.
           MOVE ALL "Z"              TO END-ID VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ID.
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

           MOVE "S  A000BEG-ID."       TO VDU.
           MOVE BEG-ID                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-I."        TO VDU.
           MOVE END-ID                 TO VDUBUF.
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

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *    IF ( PROCESS-KEY = 1 )
      *       MOVE %%1-KEY TO PACE-DETAIL-KEY
      *    ELSE
      *    IF ( PROCESS-KEY = 2 )
      *       MOVE %%2-KEY TO PACE-DETAIL-KEY
      *    ELSE
      *       MOVE %%3-KEY TO PACE-DETAIL-KEY.
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           MOVE AL1-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING ID                  : "    TO R01-PROMPT.
           MOVE BEG-ID                                TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ID                  : "    TO R01-PROMPT.
           MOVE END-ID                                TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - ALFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOALRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOALRN-SYSDATE.
           DISPLAY IOALRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOALRN-FORM-FIELDS TO WR-BUF.
           MOVE IOALRN-FORM-LIST TO WR-LIST.
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

           MOVE IOALRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOALRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOALRN_EVA.CPY".
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
      *    LOAD AL-REC FROM QAL-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-AL-FIELDS.
           MOVE QAL-ID TO AL-ID.

           IF ( QAL-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO AL-LTOUCH-DATE
           ELSE
              MOVE QAL-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO AL-LTOUCH-DATE.

           MOVE QAL-DESC TO AL-DESC.
           MOVE QAL-NO-ACCTS TO AL-NO-ACCTS.
           MOVE QAL-ACCT-1 TO AL-ACCT(1).
           MOVE QAL-PCT-1 TO AL-PCT(1).
           MOVE QAL-ACCT-2 TO AL-ACCT(2).
           MOVE QAL-PCT-2 TO AL-PCT(2).
           MOVE QAL-ACCT-3 TO AL-ACCT(3).
           MOVE QAL-PCT-3 TO AL-PCT(3).
           MOVE QAL-ACCT-4 TO AL-ACCT(4).
           MOVE QAL-PCT-4 TO AL-PCT(4).
           MOVE QAL-ACCT-5 TO AL-ACCT(5).
           MOVE QAL-PCT-5 TO AL-PCT(5).
           MOVE QAL-ACCT-6 TO AL-ACCT(6).
           MOVE QAL-PCT-6 TO AL-PCT(6).
           MOVE QAL-ACCT-7 TO AL-ACCT(7).
           MOVE QAL-PCT-7 TO AL-PCT(7).
           MOVE QAL-ACCT-8 TO AL-ACCT(8).
           MOVE QAL-PCT-8 TO AL-PCT(8).
           MOVE QAL-ACCT-9 TO AL-ACCT(9).
           MOVE QAL-PCT-9 TO AL-PCT(9).
           MOVE QAL-ACCT-10 TO AL-ACCT(10).
           MOVE QAL-PCT-10 TO AL-PCT(10).
           MOVE QAL-TYPE TO AL-TYPE.
 
      *-----------------------------------------------------------------
      *    LOAD QAL-REC FROM AL-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-AL-FIELDS.
           MOVE AL-ID TO QAL-ID.

           IF ( AL-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QAL-LTOUCH-DATE
           ELSE
              MOVE AL-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QAL-LTOUCH-DATE.

           MOVE AL-DESC TO QAL-DESC.
           MOVE AL-NO-ACCTS TO QAL-NO-ACCTS.
           MOVE AL-ACCT(1) TO QAL-ACCT-1.
           MOVE AL-PCT(1) TO QAL-PCT-1.
           MOVE AL-ACCT(2) TO QAL-ACCT-2.
           MOVE AL-PCT(2) TO QAL-PCT-2.
           MOVE AL-ACCT(3) TO QAL-ACCT-3.
           MOVE AL-PCT(3) TO QAL-PCT-3.
           MOVE AL-ACCT(4) TO QAL-ACCT-4.
           MOVE AL-PCT(4) TO QAL-PCT-4.
           MOVE AL-ACCT(5) TO QAL-ACCT-5.
           MOVE AL-PCT(5) TO QAL-PCT-5.
           MOVE AL-ACCT(6) TO QAL-ACCT-6.
           MOVE AL-PCT(6) TO QAL-PCT-6.
           MOVE AL-ACCT(7) TO QAL-ACCT-7.
           MOVE AL-PCT(7) TO QAL-PCT-7.
           MOVE AL-ACCT(8) TO QAL-ACCT-8.
           MOVE AL-PCT(8) TO QAL-PCT-8.
           MOVE AL-ACCT(9) TO QAL-ACCT-9.
           MOVE AL-PCT(9) TO QAL-PCT-9.
           MOVE AL-ACCT(10) TO QAL-ACCT-10.
           MOVE AL-PCT(10) TO QAL-PCT-10.
           MOVE AL-TYPE TO QAL-TYPE.
 
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBGL/GLAL1RN
       LOAD-AL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( AL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( AL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO AL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO AL-ALL-MACHINE
              MOVE AL-ALL-PATH             TO AL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( AL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO AL-ALL-BASE
              MOVE FILPATH-MACHINE         TO AL-ALL-MACHINE
              MOVE AL-ALL-PATH             TO AL-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-ALFILE NOT = "S" )
              MOVE AL-PATH-BEG             TO AL-SELECT-PATH-BEG
              MOVE AL-PATH-FILE            TO AL-SELECT-PATH-FILE
           ELSE
              MOVE AL-PATH-SQL             TO AL-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-AL1-FILE.
           PERFORM LOAD-AL1-FILE.
           PERFORM OPEN-IT.
           MOVE AL-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-AL1-FILE.
           PERFORM START-IT.
           MOVE AL-SELECT-PATH TO E-FILE.
           MOVE AL1-KEY TO E-KEYX.

           IF ( AL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-AL1-FILE.

           IF ( QAL1-WEND-ID = SPACES )
              MOVE AL-ID          TO QAL1-WBEG-ID
              MOVE ALL "Z"        TO QAL1-WEND-ID.

           EXEC SQL
            DECLARE AL1_CURSOR CURSOR FOR
            SELECT ISNULL(ALFILE.AL_ID,''),
                   ISNULL(CAST(ALFILE.AL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(ALFILE.AL_DESC,''),
                   ALFILE.AL_NO_ACCTS,
                   ALFILE.AL_ACCT_1,
                   ALFILE.AL_PCT_1,
                   ALFILE.AL_ACCT_2,
                   ALFILE.AL_PCT_2,
                   ALFILE.AL_ACCT_3,
                   ALFILE.AL_PCT_3,
                   ALFILE.AL_ACCT_4,
                   ALFILE.AL_PCT_4,
                   ALFILE.AL_ACCT_5,
                   ALFILE.AL_PCT_5,
                   ALFILE.AL_ACCT_6,
                   ALFILE.AL_PCT_6,
                   ALFILE.AL_ACCT_7,
                   ALFILE.AL_PCT_7,
                   ALFILE.AL_ACCT_8,
                   ALFILE.AL_PCT_8,
                   ALFILE.AL_ACCT_9,
                   ALFILE.AL_PCT_9,
                   ALFILE.AL_ACCT_10,
                   ALFILE.AL_PCT_10,
                   ISNULL(ALFILE.AL_TYPE,'')
            FROM DBO.ALFILE
            WHERE ALFILE.AL_ID >= :QAL1-WBEG-ID
              AND ALFILE.AL_ID <= :QAL1-WEND-ID
            ORDER BY ALFILE.AL_ID
           END-EXEC.

           EXEC SQL
               OPEN AL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QAL1-WBEG-ID
                          QAL1-WEND-ID.

           MOVE STAT---GOOD TO AL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-AL1-FILE.
           PERFORM READ-IT.
           MOVE AL-SELECT-PATH TO E-FILE.
           MOVE AL1-KEY TO E-KEYX.

           MOVE AL-ID     TO QAL-ID.

           EXEC SQL
            SELECT ISNULL(ALFILE.AL_ID,''),
                   ISNULL(CAST(ALFILE.AL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(ALFILE.AL_DESC,''),
                   ALFILE.AL_NO_ACCTS,
                   ALFILE.AL_ACCT_1,
                   ALFILE.AL_PCT_1,
                   ALFILE.AL_ACCT_2,
                   ALFILE.AL_PCT_2,
                   ALFILE.AL_ACCT_3,
                   ALFILE.AL_PCT_3,
                   ALFILE.AL_ACCT_4,
                   ALFILE.AL_PCT_4,
                   ALFILE.AL_ACCT_5,
                   ALFILE.AL_PCT_5,
                   ALFILE.AL_ACCT_6,
                   ALFILE.AL_PCT_6,
                   ALFILE.AL_ACCT_7,
                   ALFILE.AL_PCT_7,
                   ALFILE.AL_ACCT_8,
                   ALFILE.AL_PCT_8,
                   ALFILE.AL_ACCT_9,
                   ALFILE.AL_PCT_9,
                   ALFILE.AL_ACCT_10,
                   ALFILE.AL_PCT_10,
                   ISNULL(ALFILE.AL_TYPE,'')
            INTO :QAL-REC
            FROM DBO.ALFILE
            WHERE ALFILE.AL_ID = :QAL-ID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-AL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-AL-FIELDS.

      *-----------------------------------------------------------------
       READ-AL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE AL-SELECT-PATH TO E-FILE.
           MOVE AL1-KEY TO E-KEYX.
           INITIALIZE QAL-REC.

           IF ( AL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH AL1_CURSOR INTO :QAL-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-AL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-AL-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-AL1-FILE.
           IF ( AL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE AL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO AL1-CURSOR-STAT.

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
              DESTROY IOALRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
