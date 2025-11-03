       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOIIIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-11-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOIIIN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : IIFILE 
      * TABLE DESC    : IRS IDENTIFICATION FILE
      * OUTPUT FILE   : XPC/IOIIIN.PRE
      * USED FOR      : LIBSP/SPWSII.CPY  --- QII-REC
      *                 LIBSP/SPIIR.CPY  --- READ
      *                 LIBSP/SPIIRN.CPY --- READ/READ-NEXT/START
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
           "@(#)IO/IOIIIN.CBL S34 05/18/21-08:57:27 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBSP/SP01II.CPY".
       COPY "LIBSP/SPWSII.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QII-WHERE-BEG-KEY.
           03  QII-WBEG-KEY               PIC 99.
 
       01  QII-WHERE-END-KEY.
           03  QII-WEND-KEY               PIC 99.
 
       01  QII-REC.
           03  QII-KEY                    PIC 9(2).
           03  QII-LTOUCH-DATE            PIC X(10).
           03  QII-EMPLOYER-ID            PIC 9(9).
           03  QII-NAME-CONTROL-NO        PIC X(4).
           03  QII-TRANS-CONTROL-CODE     PIC X(5).
           03  QII-FOREIGN-CORP           PIC X.
           03  QII-TRANSFER-AGENT-FG      PIC X.
           03  QII-PAYER-NAME             PIC X(40).
           03  QII-SECOND-PAYER           PIC X(40).
           03  QII-PAYER-ADDRESS          PIC X(40).
           03  QII-PAYER-CITY             PIC X(29).
           03  QII-PAYER-STATE            PIC X(2).
           03  QII-PAYER-ZIP              PIC X(9).
           03  QII-PAYER-PHONE            PIC 9(10).
           03  QII-PAYER-EXT              PIC X(5).
           03  QII-CONTACT                PIC X(40).
           03  QII-CONTACT-EMAIL          PIC X(45).
           03  QII-TAX-YEAR               PIC 9(4).
           03  QII-CURRENT-LAST           PIC X.
           03  QII-LASER-FORM             PIC X.

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

       01  LEGEND-BEG-KEY.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING KEY        (F3-ALL) ".

       01  LEGEND-END-KEY. 
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING KEY        ".

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
           03  BEG-KEY               PIC 99.
           03  END-KEY               PIC 99.

           03  PROCESS-KEY           PIC 9(01).

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
           03  FILLER PIC X(48) VALUE "KEY EMPLOYER    PAYER NAME".
           03  FILLER PIC X(51) VALUE "CONTROL   CONTACT".
           03  FILLER PIC X(30) VALUE "TAX YEAR     LTOUCH".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-II-KEY                     PIC 99.
           03  FILLER                         PIC XX.
           03  D01-II-EMPLOYER-ID             PIC 9(9).
           03  FILLER                         PIC X(05).
           03  D01-II-PAYER-NAME              PIC X(25).
           03  FILLER                         PIC X(05).
           03  D01-II-TRANS-CONTROL-CODE      PIC X(5).
           03  FILLER                         PIC X(05).
           03  D01-II-CONTACT                 PIC X(40).
           03  FILLER                         PIC X(05).
           03  D01-II-TAX-YEAR                PIC 9(4).
           03  FILLER                         PIC X(05).
           03  D01-II-LTOUCH-DATE             PIC 9(8).

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
       COPY "LIBIO/IOIIIN_DEF.CPY".
       COPY "LIBIO/IOIIIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOIIIN_SCN.CPY".

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
           MOVE "B"                  TO II-PATH-OVERRIDE.

           MOVE BEG-KEY              TO QII-WBEG-KEY.
           MOVE END-KEY              TO QII-WEND-KEY.

           PERFORM OPEN-II-FILE.
           PERFORM START-II-FILE.

       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-II-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( II-KEY < BEG-KEY )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( II-KEY > END-KEY )
              GO TO MAIN-PROGRAM-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE II-KEY                     TO D01-II-KEY.
           MOVE II-EMPLOYER-ID             TO D01-II-EMPLOYER-ID.
           MOVE II-PAYER-NAME              TO D01-II-PAYER-NAME.
           MOVE II-TRANS-CONTROL-CODE      TO D01-II-TRANS-CONTROL-CODE.
           MOVE II-CONTACT                 TO D01-II-CONTACT.
           MOVE II-TAX-YEAR                TO D01-II-TAX-YEAR.
           MOVE II-LTOUCH-DATE             TO D01-II-LTOUCH-DATE.
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
           MOVE "IOIIIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOIIIN-QUEUE-POS  TO Q-POS.
           MOVE IOIIIN-COPIES-POS TO C-POS.
           MOVE IOIIIN-PRU-POS    TO P-POS.
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
       ENTER-BEG-KEY.

           MOVE LEGEND-BEG-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  N020BEG-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-KEY.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-KEY.

      *-----------------------------------------------------------------
       ENTER-END-KEY.

           MOVE LEGEND-END-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  N020END-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-KEY.

           IF ( BEG-KEY > END-KEY )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-KEY.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-KEY.

           MOVE "S  N020BEG-KEY." TO VDU.
           MOVE 0                 TO BEG-KEY VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  N020END-KEY." TO VDU.
           MOVE 99                TO END-KEY VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-KEY.
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

           MOVE "S  N020BEG-KEY."    TO VDU.
           MOVE BEG-KEY              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  N000END-KEY."    TO VDU.
           MOVE END-KEY              TO VDUNUM0.
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

           MOVE II-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING KEY                 : "    TO R01-PROMPT.
           MOVE BEG-KEY                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    KEY                 : "    TO R01-PROMPT.
           MOVE END-KEY                             TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - IIFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOIIIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOIIIN-SYSDATE.
           DISPLAY IOIIIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOIIIN-FORM-FIELDS TO WR-BUF.
           MOVE IOIIIN-FORM-LIST TO WR-LIST.
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

           MOVE IOIIIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOIIIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOIIIN_EVA.CPY".
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
      *    LOAD II-REC FROM QII-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-II-FIELDS.
           MOVE QII-KEY TO II-KEY.

           IF ( QII-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO II-LTOUCH-DATE
           ELSE
              MOVE QII-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO II-LTOUCH-DATE.

           MOVE QII-EMPLOYER-ID TO II-EMPLOYER-ID.
           MOVE QII-NAME-CONTROL-NO TO II-NAME-CONTROL-NO.
           MOVE QII-TRANS-CONTROL-CODE TO II-TRANS-CONTROL-CODE.
           MOVE QII-FOREIGN-CORP TO II-FOREIGN-CORP.
           MOVE QII-TRANSFER-AGENT-FG TO II-TRANSFER-AGENT-FG.
           MOVE QII-PAYER-NAME TO II-PAYER-NAME.
           MOVE QII-SECOND-PAYER TO II-SECOND-PAYER.
           MOVE QII-PAYER-ADDRESS TO II-PAYER-ADDRESS.
           MOVE QII-PAYER-CITY TO II-PAYER-CITY.
           MOVE QII-PAYER-STATE TO II-PAYER-STATE.
           MOVE QII-PAYER-ZIP TO II-PAYER-ZIP.
           MOVE QII-PAYER-PHONE TO II-PAYER-PHONE.
           MOVE QII-PAYER-EXT TO II-PAYER-EXT.
           MOVE QII-CONTACT TO II-CONTACT.
           MOVE QII-CONTACT-EMAIL TO II-CONTACT-EMAIL.
           MOVE QII-TAX-YEAR TO II-TAX-YEAR.
           MOVE QII-CURRENT-LAST TO II-CURRENT-LAST.
           MOVE QII-LASER-FORM TO II-LASER-FORM.
 
      *-----------------------------------------------------------------
      *    LOAD QII-REC FROM II-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-II-FIELDS.
           MOVE II-KEY TO QII-KEY.

           IF ( II-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QII-LTOUCH-DATE
           ELSE
              MOVE II-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QII-LTOUCH-DATE.

           MOVE II-EMPLOYER-ID TO QII-EMPLOYER-ID.
           MOVE II-NAME-CONTROL-NO TO QII-NAME-CONTROL-NO.
           MOVE II-TRANS-CONTROL-CODE TO QII-TRANS-CONTROL-CODE.
           MOVE II-FOREIGN-CORP TO QII-FOREIGN-CORP.
           MOVE II-TRANSFER-AGENT-FG TO QII-TRANSFER-AGENT-FG.
           MOVE II-PAYER-NAME TO QII-PAYER-NAME.
           MOVE II-SECOND-PAYER TO QII-SECOND-PAYER.
           MOVE II-PAYER-ADDRESS TO QII-PAYER-ADDRESS.
           MOVE II-PAYER-CITY TO QII-PAYER-CITY.
           MOVE II-PAYER-STATE TO QII-PAYER-STATE.
           MOVE II-PAYER-ZIP TO QII-PAYER-ZIP.
           MOVE II-PAYER-PHONE TO QII-PAYER-PHONE.
           MOVE II-PAYER-EXT TO QII-PAYER-EXT.
           MOVE II-CONTACT TO QII-CONTACT.
           MOVE II-CONTACT-EMAIL TO QII-CONTACT-EMAIL.
           MOVE II-TAX-YEAR TO QII-TAX-YEAR.
           MOVE II-CURRENT-LAST TO QII-CURRENT-LAST.
           MOVE II-LASER-FORM TO QII-LASER-FORM.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
       LOAD-II-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( II-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( II-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO II-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO II-ALL-MACHINE
              MOVE II-ALL-PATH             TO II-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( II-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO II-ALL-BASE
              MOVE FILPATH-MACHINE         TO II-ALL-MACHINE
              MOVE II-ALL-PATH             TO II-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-IIFILE NOT = "S" )
              MOVE II-PATH-BEG             TO II-SELECT-PATH-BEG
              MOVE II-PATH-FILE            TO II-SELECT-PATH-FILE
           ELSE
              MOVE II-PATH-SQL             TO II-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-II-FILE.
           PERFORM LOAD-II-FILE.
           PERFORM OPEN-IT.
           MOVE II-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-II-FILE.
           PERFORM START-IT.
           MOVE II-SELECT-PATH TO E-FILE.
           MOVE II-KEY TO E-KEYX.

           IF ( II-CURSOR-STAT-GOOD )
              PERFORM CLOSE-II-FILE.

           IF ( QII-WEND-KEY = ZEROES    ) OR
              ( QII-WEND-KEY NOT NUMERIC )
              MOVE II-KEY               TO QII-WBEG-KEY
              MOVE 99                   TO QII-WEND-KEY.

           EXEC SQL
            DECLARE II_CURSOR CURSOR FOR
            SELECT IIFILE.II_KEY,
                   ISNULL(CAST(IIFILE.II_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   IIFILE.II_EMPLOYER_ID,
                   ISNULL(IIFILE.II_NAME_CONTROL_NO,''),
                   ISNULL(IIFILE.II_TRANS_CONTROL_CODE,''),
                   ISNULL(IIFILE.II_FOREIGN_CORP,''),
                   ISNULL(IIFILE.II_TRANSFER_AGENT_FG,''),
                   ISNULL(IIFILE.II_PAYER_NAME,''),
                   ISNULL(IIFILE.II_SECOND_PAYER,''),
                   ISNULL(IIFILE.II_PAYER_ADDRESS,''),
                   ISNULL(IIFILE.II_PAYER_CITY,''),
                   ISNULL(IIFILE.II_PAYER_STATE,''),
                   ISNULL(IIFILE.II_PAYER_ZIP,''),
                   IIFILE.II_PAYER_PHONE,
                   ISNULL(IIFILE.II_PAYER_EXT,''),
                   ISNULL(IIFILE.II_CONTACT,''),
                   ISNULL(IIFILE.II_CONTACT_EMAIL,''),
                   IIFILE.II_TAX_YEAR,
                   ISNULL(IIFILE.II_CURRENT_LAST,''),
                   ISNULL(IIFILE.II_LASER_FORM,'')
            FROM DBO.IIFILE
            WHERE IIFILE.II_KEY >= :QII-WBEG-KEY
              AND IIFILE.II_KEY <= :QII-WEND-KEY
            ORDER BY IIFILE.II_KEY
           END-EXEC.

           EXEC SQL
               OPEN II_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QII-WBEG-KEY
                          QII-WEND-KEY.

           MOVE STAT---GOOD TO II-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-II-FILE.
           PERFORM READ-IT.
           MOVE II-SELECT-PATH TO E-FILE.
           MOVE II-KEY TO E-KEYX.

           MOVE II-KEY    TO QII-KEY.  

           EXEC SQL
            SELECT IIFILE.II_KEY,
                   ISNULL(CAST(IIFILE.II_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   IIFILE.II_EMPLOYER_ID,
                   ISNULL(IIFILE.II_NAME_CONTROL_NO,''),
                   ISNULL(IIFILE.II_TRANS_CONTROL_CODE,''),
                   ISNULL(IIFILE.II_FOREIGN_CORP,''),
                   ISNULL(IIFILE.II_TRANSFER_AGENT_FG,''),
                   ISNULL(IIFILE.II_PAYER_NAME,''),
                   ISNULL(IIFILE.II_SECOND_PAYER,''),
                   ISNULL(IIFILE.II_PAYER_ADDRESS,''),
                   ISNULL(IIFILE.II_PAYER_CITY,''),
                   ISNULL(IIFILE.II_PAYER_STATE,''),
                   ISNULL(IIFILE.II_PAYER_ZIP,''),
                   IIFILE.II_PAYER_PHONE,
                   ISNULL(IIFILE.II_PAYER_EXT,''),
                   ISNULL(IIFILE.II_CONTACT,''),
                   ISNULL(IIFILE.II_CONTACT_EMAIL,''),
                   IIFILE.II_TAX_YEAR,
                   ISNULL(IIFILE.II_CURRENT_LAST,''),
                   ISNULL(IIFILE.II_LASER_FORM,'')
            INTO :QII-REC
            FROM DBO.IIFILE
            WHERE IIFILE.II_KEY = :QII-KEY
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-II-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-II-FIELDS.

      *-----------------------------------------------------------------
       READ-II-FILE-NEXT.
           PERFORM READ-IT.
           MOVE II-SELECT-PATH TO E-FILE.
           MOVE II-KEY TO E-KEYX.
           INITIALIZE QII-REC.

           IF ( II-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH II_CURSOR INTO :QII-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-II-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-II-FIELDS.

      *-----------------------------------------------------------------
       WRITE-II-FILE.

           ACCEPT II-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE II-SELECT-PATH TO E-FILE.
           MOVE II-KEY TO E-KEYX.
           PERFORM SET-II-FIELDS.

           EXEC SQL
            INSERT INTO DBO.IIFILE
             (IIFILE.II_KEY,
              IIFILE.II_LTOUCH_DATE,
              IIFILE.II_EMPLOYER_ID,
              IIFILE.II_NAME_CONTROL_NO,
              IIFILE.II_TRANS_CONTROL_CODE,
              IIFILE.II_FOREIGN_CORP,
              IIFILE.II_TRANSFER_AGENT_FG,
              IIFILE.II_PAYER_NAME,
              IIFILE.II_SECOND_PAYER,
              IIFILE.II_PAYER_ADDRESS,
              IIFILE.II_PAYER_CITY,
              IIFILE.II_PAYER_STATE,
              IIFILE.II_PAYER_ZIP,
              IIFILE.II_PAYER_PHONE,
              IIFILE.II_PAYER_EXT,
              IIFILE.II_CONTACT,
              IIFILE.II_CONTACT_EMAIL,
              IIFILE.II_TAX_YEAR,
              IIFILE.II_CURRENT_LAST,
              IIFILE.II_LASER_FORM)
            VALUES
             (:QII-KEY,
              NULLIF(:QII-LTOUCH-DATE,'1900-01-01'),
              :QII-EMPLOYER-ID,
              :QII-NAME-CONTROL-NO,
              :QII-TRANS-CONTROL-CODE,
              :QII-FOREIGN-CORP,
              :QII-TRANSFER-AGENT-FG,
              :QII-PAYER-NAME,
              :QII-SECOND-PAYER,
              :QII-PAYER-ADDRESS,
              :QII-PAYER-CITY,
              :QII-PAYER-STATE,
              :QII-PAYER-ZIP,
              :QII-PAYER-PHONE,
              :QII-PAYER-EXT,
              :QII-CONTACT,
              :QII-CONTACT-EMAIL,
              :QII-TAX-YEAR,
              :QII-CURRENT-LAST,
              :QII-LASER-FORM)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-II-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-II-FILE.

           ACCEPT II-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE II-SELECT-PATH TO E-FILE.
           MOVE II-KEY TO E-KEYX.
           PERFORM SET-II-FIELDS.

           EXEC SQL
            UPDATE DBO.IIFILE
             SET
              IIFILE.II_KEY = :QII-KEY,
              IIFILE.II_LTOUCH_DATE =
                 NULLIF(:QII-LTOUCH-DATE,'1900-01-01'),
              IIFILE.II_EMPLOYER_ID = :QII-EMPLOYER-ID,
              IIFILE.II_NAME_CONTROL_NO = :QII-NAME-CONTROL-NO,
              IIFILE.II_TRANS_CONTROL_CODE =
                   :QII-TRANS-CONTROL-CODE,
              IIFILE.II_FOREIGN_CORP = :QII-FOREIGN-CORP,
              IIFILE.II_TRANSFER_AGENT_FG =
                   :QII-TRANSFER-AGENT-FG,
              IIFILE.II_PAYER_NAME = :QII-PAYER-NAME,
              IIFILE.II_SECOND_PAYER = :QII-SECOND-PAYER,
              IIFILE.II_PAYER_ADDRESS = :QII-PAYER-ADDRESS,
              IIFILE.II_PAYER_CITY = :QII-PAYER-CITY,
              IIFILE.II_PAYER_STATE = :QII-PAYER-STATE,
              IIFILE.II_PAYER_ZIP = :QII-PAYER-ZIP,
              IIFILE.II_PAYER_PHONE = :QII-PAYER-PHONE,
              IIFILE.II_PAYER_EXT = :QII-PAYER-EXT,
              IIFILE.II_CONTACT = :QII-CONTACT,
              IIFILE.II_CONTACT_EMAIL = :QII-CONTACT-EMAIL,
              IIFILE.II_TAX_YEAR = :QII-TAX-YEAR,
              IIFILE.II_CURRENT_LAST = :QII-CURRENT-LAST,
              IIFILE.II_LASER_FORM = :QII-LASER-FORM
             WHERE IIFILE.II_KEY = :QII-KEY
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-II-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-II-FILE.

           PERFORM DELETE-IT.
           MOVE II-SELECT-PATH TO E-FILE.
           MOVE II-KEY TO E-KEYX.
           PERFORM SET-II-FIELDS.

           EXEC SQL
            DELETE FROM DBO.IIFILE
             WHERE IIFILE.II_KEY = :QII-KEY
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-II-FILE.

      *-----------------------------------------------------------------
       CLOSE-II-FILE.
           IF ( II-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE II_CURSOR
              END-EXEC
              MOVE STAT---BAD TO II-CURSOR-STAT.

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
              DESTROY IOIIIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
