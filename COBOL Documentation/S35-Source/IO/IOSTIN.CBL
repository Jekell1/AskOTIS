       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOSTIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  03-09-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOSTIN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : STFILE 
      * TABLE DESC    : STATE CODE FILE
      * OUTPUT FILE   : XPC/IOSTIN.PRE
      * USED FOR      : LIBLP/LPWSST.CPY  --- QST-REC
      *                 LIBLP/LPST1R.CPY  --- READ
      *                 LIBLP/LPST1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0309 ***NEW***
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
           "@(#)IO/IOSTIN.CBL S34 05/18/21-11:33:31 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LPWSST.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S WS COPY MEMBERS
      *#################################################################
      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIBLP/LPWSST.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QST1-WHERE-BEG-KEY.
           03  QST1-WBEG-CODE           PIC X(02).

       01  QST1-WHERE-END-KEY.
           03  QST1-WEND-CODE           PIC X(02).

       01  QST-REC.
           03  QST-CODE                      PIC X(02).
           03  QST-LTOUCH-DATE               PIC X(10).
           03  QST-DESC                      PIC X(16).
           03  QST-REINSTATE-WINDOW          PIC X(01).
           03  QST-REPO-RENOTICE             PIC X(01).
           03  QST-TITLE-RELEASE-TYPE        PIC X(01).
           03  QST-AUCTION-DATE-REQ          PIC X(01).
           03  QST-GEOGRAPHIC-CODE           PIC 9(04).
           03  QST-MAX-LOAN-AVAIL            PIC 9(6)V99.
           03  QST-MAX-TAX-LOAN-AMT          PIC S9(7)V99.
           03  QST-NSF-FEE-AMT               PIC S9(5)V99.

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

       01  LEGEND-BEG-CODE.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING CODE       (F3-ALL) ".

       01  LEGEND-END-CODE.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING CODE       ".

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
           03  BEG-CODE                PIC X(02).
           03  END-CODE                PIC X(02).

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
           03  FILLER PIC X(30) VALUE "CODE LAST TOUCH     DESC      ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(01).
           03  D01-ST-CODE                    PIC X(02).
           03  FILLER                         PIC X(03).
           03  D01-ST-LTOUCH-DATE             PIC 9(08).
           03  FILLER                         PIC X(01).
           03  D01-ST-DESC                    PIC X(16).

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
       COPY "LIBIO/IOSTIN_DEF.CPY".
       COPY "LIBIO/IOSTIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOSTIN_SCN.CPY".

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
           MOVE "B"                        TO ST-PATH-OVERRIDE.

           MOVE BEG-CODE                   TO QST1-WBEG-CODE.
           MOVE END-CODE                   TO QST1-WEND-CODE.

           PERFORM OPEN-ST1-FILE.
           PERFORM START-ST1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *    IF ( PROCESS-KEY = 1 )
      *       PERFORM READ-ST1-FILE-NEXT
      *    ELSE
      *       PERFORM READ-ST#-FILE-NEXT.
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           PERFORM READ-ST1-FILE-NEXT.

           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( ST-CODE < BEG-CODE )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( ST-CODE > END-CODE )
              GO TO MAIN-PROGRAM-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ST-CODE                    TO D01-ST-CODE.
           MOVE ST-LTOUCH-DATE             TO D01-ST-LTOUCH-DATE.
           MOVE ST-DESC                    TO D01-ST-DESC.
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
           MOVE "IOSTIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOSTIN-QUEUE-POS  TO Q-POS.
           MOVE IOSTIN-COPIES-POS TO C-POS.
           MOVE IOSTIN-PRU-POS    TO P-POS.
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
       ENTER-BEG-CODE.

           MOVE LEGEND-BEG-CODE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040BEG-CODE." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CODE.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-CODE.

      *-----------------------------------------------------------------
       ENTER-END-CODE.

           MOVE LEGEND-END-CODE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040END-CODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CODE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CODE. 

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-CODE.

           IF ( BEG-CODE > END-CODE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CODE. 

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-CODE. 

           MOVE "S  A040BEG-CODE." TO VDU.
           MOVE SPACES               TO BEG-CODE VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040END-CODE." TO VDU.
           MOVE ALL "Z"              TO END-CODE VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CODE.
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

           MOVE "S  A000BEG-CODE."    TO VDU.
           MOVE BEG-CODE              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-CODE."    TO VDU.
           MOVE END-CODE              TO VDUBUF.
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
      *       MOVE ST1-KEY TO PACE-DETAIL-KEY
      *    ELSE
      *    IF ( PROCESS-KEY = 2 )
      *       MOVE ST2-KEY TO PACE-DETAIL-KEY
      *    ELSE
      *       MOVE ST3-KEY TO PACE-DETAIL-KEY.
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           MOVE ST1-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING CODE                : "    TO R01-PROMPT.
           MOVE BEG-CODE                              TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CODE                : "    TO R01-PROMPT.
           MOVE END-CODE                              TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - STFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOSTIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOSTIN-SYSDATE.
           DISPLAY IOSTIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOSTIN-FORM-FIELDS TO WR-BUF.
           MOVE IOSTIN-FORM-LIST TO WR-LIST.
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

           MOVE IOSTIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOSTIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOSTIN_EVA.CPY".
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

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBLP/LPST1IN

       LOAD-ST1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ST-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ST-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ST-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO ST-ALL-MACHINE
              MOVE ST-ALL-PATH             TO ST-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ST-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ST-ALL-BASE
              MOVE FILPATH-MACHINE         TO ST-ALL-MACHINE
              MOVE ST-ALL-PATH             TO ST-PATH.
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*G2G  *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-STFILE NOT = "S" )
*G2G          MOVE ST-PATH-BEG             TO ST-SELECT-PATH-BEG
*G2G          MOVE ST-PATH-FILE            TO ST-SELECT-PATH-FILE
*G2G       ELSE
*G2G          MOVE ST-PATH-SQL             TO ST-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-ST1-FILE.
           PERFORM LOAD-ST1-FILE.
           PERFORM OPEN-IT.
           MOVE ST-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-ST1-FILE.
           PERFORM START-IT.
           MOVE ST-SELECT-PATH TO E-FILE.
           MOVE ST1-KEY TO E-KEYX.

           IF ( ST1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-ST1-FILE.

           IF ( QST1-WEND-CODE = SPACES )
              MOVE ST-CODE        TO QST1-WBEG-CODE.
              MOVE ALL "Z"        TO QST1-WEND-CODE.

           EXEC SQL
            DECLARE ST1_CURSOR CURSOR FOR
            SELECT ISNULL(STFILE.ST_CODE,''),
                   ISNULL(CAST(STFILE.ST_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(STFILE.ST_DESC,''),
                   ISNULL(STFILE.ST_REINSTATE_WINDOW,''),
                   ISNULL(STFILE.ST_REPO_RENOTICE,''),
                   ISNULL(STFILE.ST_TITLE_RELEASE_TYPE,''),
                   ISNULL(STFILE.ST_AUCTION_DATE_REQ,''),
                   STFILE.ST_GEOGRAPHIC_CODE,
                   STFILE.ST_MAX_LOAN_AVAIL,
                   STFILE.ST_MAX_TAX_LOAN_AMT,
                   STFILE.ST_NSF_FEE_AMT
            FROM DBO.STFILE
            WHERE STFILE.ST_CODE >= :QST1-WBEG-CODE
              AND STFILE.ST_CODE <= :QST1-WEND-CODE
            ORDER BY STFILE.ST_CODE
           END-EXEC.

           EXEC SQL
               OPEN ST1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QST1-WBEG-CODE
                          QST1-WEND-CODE.

           MOVE STAT---GOOD TO ST1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-ST1-FILE.
           PERFORM READ-IT.
           MOVE ST-SELECT-PATH TO E-FILE.
           MOVE ST1-KEY TO E-KEYX.

           MOVE ST-CODE   TO QST-CODE.

           EXEC SQL
            SELECT ISNULL(STFILE.ST_CODE,''),
                   ISNULL(CAST(STFILE.ST_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(STFILE.ST_DESC,''),
                   ISNULL(STFILE.ST_REINSTATE_WINDOW,''),
                   ISNULL(STFILE.ST_REPO_RENOTICE,''),
                   ISNULL(STFILE.ST_TITLE_RELEASE_TYPE,''),
                   ISNULL(STFILE.ST_AUCTION_DATE_REQ,''),
                   STFILE.ST_GEOGRAPHIC_CODE,
                   STFILE.ST_MAX_LOAN_AVAIL,
                   STFILE.ST_MAX_TAX_LOAN_AMT,
                   STFILE.ST_NSF_FEE_AMT
            INTO :QST-REC
            FROM DBO.STFILE
            WHERE STFILE.ST_CODE = :QST-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-ST1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-ST-FIELDS.

      *-----------------------------------------------------------------
       READ-ST1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ST-SELECT-PATH TO E-FILE.
           MOVE ST1-KEY TO E-KEYX.
           INITIALIZE QST-REC.

           IF ( ST1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH ST1_CURSOR INTO :QST-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-ST1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-ST-FIELDS.

      *-----------------------------------------------------------------
       WRITE-ST1-FILE.

           ACCEPT ST-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE ST-SELECT-PATH TO E-FILE.
           MOVE ST1-KEY TO E-KEYX.
           PERFORM SET-ST-FIELDS.

           EXEC SQL
            INSERT INTO DBO.STFILE
             (STFILE.ST_CODE,
              STFILE.ST_LTOUCH_DATE,
              STFILE.ST_DESC,
              STFILE.ST_REINSTATE_WINDOW,
              STFILE.ST_REPO_RENOTICE,
              STFILE.ST_TITLE_RELEASE_TYPE,
              STFILE.ST_AUCTION_DATE_REQ,
              STFILE.ST_GEOGRAPHIC_CODE,
              STFILE.ST_MAX_LOAN_AVAIL,
              STFILE.ST_MAX_TAX_LOAN_AMT,
              STFILE.ST_NSF_FEE_AMT)
            VALUES
             (:QST-CODE,
              NULLIF(:QST-LTOUCH-DATE,'1900-01-01'),
              :QST-DESC,
              :QST-REINSTATE-WINDOW,
              :QST-REPO-RENOTICE,
              :QST-TITLE-RELEASE-TYPE,
              :QST-AUCTION-DATE-REQ,
              :QST-GEOGRAPHIC-CODE,
              :QST-MAX-LOAN-AVAIL,
              :QST-MAX-TAX-LOAN-AMT,
              :QST-NSF-FEE-AMT)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-ST1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-ST1-FILE.

           ACCEPT ST-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE ST-SELECT-PATH TO E-FILE.
           MOVE ST1-KEY TO E-KEYX.
           PERFORM SET-ST-FIELDS.

           EXEC SQL
            UPDATE DBO.STFILE
             SET
              STFILE.ST_CODE = :QST-CODE,
              STFILE.ST_LTOUCH_DATE =
                 NULLIF(:QST-LTOUCH-DATE,'1900-01-01'),
              STFILE.ST_DESC = :QST-DESC,
              STFILE.ST_REINSTATE_WINDOW = :QST-REINSTATE-WINDOW,
              STFILE.ST_REPO_RENOTICE = :QST-REPO-RENOTICE,
              STFILE.ST_TITLE_RELEASE_TYPE =
                   :QST-TITLE-RELEASE-TYPE,
              STFILE.ST_AUCTION_DATE_REQ = :QST-AUCTION-DATE-REQ,
              STFILE.ST_GEOGRAPHIC_CODE = :QST-GEOGRAPHIC-CODE,
              STFILE.ST_MAX_LOAN_AVAIL = :QST-MAX-LOAN-AVAIL,
              STFILE.ST_MAX_TAX_LOAN_AMT = :QST-MAX-TAX-LOAN-AMT,
              STFILE.ST_NSF_FEE_AMT = :QST-NSF-FEE-AMT
            WHERE STFILE.ST_CODE = :QST-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-ST1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-ST1-FILE.

           PERFORM DELETE-IT.
           MOVE ST-SELECT-PATH TO E-FILE.
           MOVE ST1-KEY TO E-KEYX.
           PERFORM SET-ST-FIELDS.

           EXEC SQL
            DELETE FROM DBO.STFILE
            WHERE STFILE.ST_CODE = :QST-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-ST1-FILE.

      *-----------------------------------------------------------------
       CLOSE-ST1-FILE.
           IF ( ST1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE ST1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO ST1-CURSOR-STAT.

      *-----------------------------------------------------------------
      *    LOAD ST-REC FROM QST-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-ST-FIELDS.
           MOVE QST-CODE TO ST-CODE.

           MOVE QST-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD.
           PERFORM SQL-GET-DATE.
           MOVE SQL-DATE-YYYYMMDD TO ST-LTOUCH-DATE.

           MOVE QST-DESC TO ST-DESC.
           MOVE QST-REINSTATE-WINDOW TO ST-REINSTATE-WINDOW.
           MOVE QST-REPO-RENOTICE TO ST-REPO-RENOTICE.
           MOVE QST-TITLE-RELEASE-TYPE TO ST-TITLE-RELEASE-TYPE.
           MOVE QST-AUCTION-DATE-REQ TO ST-AUCTION-DATE-REQ.
           MOVE QST-GEOGRAPHIC-CODE TO ST-GEOGRAPHIC-CODE.
           MOVE QST-MAX-LOAN-AVAIL TO ST-MAX-LOAN-AVAIL.
           MOVE QST-MAX-TAX-LOAN-AMT TO ST-MAX-TAX-LOAN-AMT.
           MOVE QST-NSF-FEE-AMT TO ST-NSF-FEE-AMT.
 
      *-----------------------------------------------------------------
      *    LOAD QST-REC FROM ST-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-ST-FIELDS.
           MOVE ST-CODE TO QST-CODE.

           MOVE ST-LTOUCH-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QST-LTOUCH-DATE.

           MOVE ST-DESC TO QST-DESC.
           MOVE ST-REINSTATE-WINDOW TO QST-REINSTATE-WINDOW.
           MOVE ST-REPO-RENOTICE TO QST-REPO-RENOTICE.
           MOVE ST-TITLE-RELEASE-TYPE TO QST-TITLE-RELEASE-TYPE.
           MOVE ST-AUCTION-DATE-REQ TO QST-AUCTION-DATE-REQ.
           MOVE ST-GEOGRAPHIC-CODE TO QST-GEOGRAPHIC-CODE.
           MOVE ST-MAX-LOAN-AVAIL TO QST-MAX-LOAN-AVAIL.
           MOVE ST-MAX-TAX-LOAN-AMT TO QST-MAX-TAX-LOAN-AMT.
           MOVE ST-NSF-FEE-AMT TO QST-NSF-FEE-AMT.
 
      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################

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
              DESTROY IOSTIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
