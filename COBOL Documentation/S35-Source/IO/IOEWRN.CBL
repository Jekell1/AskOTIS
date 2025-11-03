       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOEWRN.
       AUTHOR.        BAH.
       DATE-WRITTEN.  03-22-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOEWRN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : EWFILE 
      * TABLE DESC    : ELECTRONIC PAYMENT WORK FILE
      * OUTPUT FILE   : XPC/IOEWRN.PRE
      * USED FOR      : LIBSP/SPWSEW.CPY  --- QEW-REC
      *                 LIBSP/SPEW1R.CPY  --- READ
      *                 LIBSP/SPEW1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0322 ***NEW***
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
           "@(#)IO/IOEWRN.CBL S34 05/17/21-16:51:22 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBSP/SP01EW.CPY".
       COPY "LIBSP/SPWSEW.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QEW1-WHERE-BEG-KEY.
           03  QEW1-WBEG-BRNO              PIC 9(04).
           03  QEW1-WBEG-DATE              PIC X(10).
           03  QEW1-WBEG-ACCTNO            PIC 9(08).
           03  QEW1-WBEG-SEQ               PIC 9(02).
 
       01  QEW1-WHERE-END-KEY.
           03  QEW1-WEND-BRNO              PIC 9(04).
           03  QEW1-WEND-DATE              PIC X(10).
           03  QEW1-WEND-ACCTNO            PIC 9(08).
           03  QEW1-WEND-SEQ               PIC 9(02).

       01  QEW2-WHERE-BEG-KEY.
           03  QEW2-WBEG-DATE              PIC X(10).
 
       01  QEW2-WHERE-END-KEY.
           03  QEW2-WEND-DATE              PIC X(10).
 
       01  QEW-REC.
           03  QEW-BRNO                PIC 9(4).
           03  QEW-TRANS-DATE          PIC X(10).
           03  QEW-ACCTNO              PIC 9(8).
           03  QEW-SEQ                 PIC 9(2).
           03  QEW-LTOUCH-DATE             PIC X(10).
           03  QEW-NAME                    PIC X(22).
           03  QEW-BANK-ACCTNO             PIC X(17).
           03  QEW-BANK-ROUTING-NO         PIC 9(9).
           03  QEW-ACCT-TYPE               PIC X.
           03  QEW-PAYMENT-AMT             PIC S9(9)V99.
           03  QEW-PROGRAM                 PIC X(2).
           03  QEW-USERID                  PIC X(10).
           03  QEW-POST-DATE               PIC X(10).
           03  QEW-POST-TIME               PIC 9(6).

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
           03  BEG-BRANCH               PIC X(04).
           03  END-BRANCH               PIC X(04).
           03  BEG-TRANS-DATE           PIC 9(08).
           03  END-TRANS-DATE           PIC 9(08).
           03  BEG-ACCTNO               PIC 9(08).
           03  END-ACCTNO               PIC 9(08).
           03  BEG-SEQ                  PIC 9(02).
           03  END-SEQ                  PIC 9(02).

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
           03  FILLER PIC X(31) VALUE "BRNO   TRANSDATE    ACCOUNT    ".
           03  FILLER PIC X(37) VALUE "SEQ  NAME         BANK ACCTNO  ".
           03  FILLER PIC X(30) VALUE "PAY AMT".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.
      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-EW-BRNO                    PIC 9(4).
           03  FILLER                         PIC X(03).
           03  D01-EW-TRANS-DATE              PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-EW-ACCTNO                  PIC 9(8).
           03  FILLER                         PIC X(03).
           03  D01-EW-SEQ                     PIC 9(2).
           03  FILLER                         PIC X(03).
           03  D01-EW-NAME                    PIC X(10).
           03  FILLER                         PIC X(03).
           03  D01-EW-BANK-ACCTNO             PIC X(17).
           03  FILLER                         PIC X(03).
           03  D01-EW-PAYMENT-AMT             PIC Z(8)9.99.
 

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
       COPY "LIBIO/IOEWRN_DEF.CPY".
       COPY "LIBIO/IOEWRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOEWRN_SCN.CPY".

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
           MOVE "B"                  TO EW-PATH-OVERRIDE.

           MOVE BEG-BRANCH           TO QEW1-WBEG-BRNO.
           MOVE END-BRANCH           TO QEW1-WEND-BRNO.
           MOVE BEG-ACCTNO           TO QEW1-WBEG-ACCTNO.
           MOVE END-ACCTNO           TO QEW1-WEND-ACCTNO.
           MOVE BEG-SEQ              TO QEW1-WBEG-SEQ.
           MOVE END-SEQ              TO QEW1-WEND-SEQ.

           MOVE BEG-TRANS-DATE       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QEW1-WBEG-DATE
                                        QEW2-WBEG-DATE.

           MOVE END-TRANS-DATE       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QEW1-WEND-DATE
                                        QEW2-WEND-DATE.

           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-EW1-FILE
              PERFORM START-EW1-FILE
           ELSE
              PERFORM OPEN-EW2-FILE
              PERFORM START-EW2-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-EW1-FILE-NEXT
           ELSE
              PERFORM READ-EW2-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF PROCESS-KEY = 1
              IF ( EW-BRNO < BEG-BRANCH )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( EW-BRNO > END-BRANCH )
                 GO TO MAIN-PROGRAM-END.

           IF PROCESS-KEY = 2
              IF ( EW-TRANS-DATE < BEG-TRANS-DATE )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( EW-TRANS-DATE > END-TRANS-DATE )
                 GO TO MAIN-PROGRAM-END.

           IF PROCESS-KEY = 1
              IF EW-TRANS-DATE < BEG-TRANS-DATE OR > END-TRANS-DATE
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
                 IF EW-ACCTNO < BEG-ACCTNO OR > END-ACCTNO
                    GO TO MAIN-PROGRAM-NEXT
                 ELSE
                    IF EW-SEQ < BEG-SEQ OR > END-SEQ
                       GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE EW-BRNO                    TO D01-EW-BRNO.
           MOVE EW-TRANS-DATE              TO D01-EW-TRANS-DATE.
           MOVE EW-ACCTNO                  TO D01-EW-ACCTNO.
           MOVE EW-SEQ                     TO D01-EW-SEQ.
           MOVE EW-NAME                    TO D01-EW-NAME.
           MOVE EW-BANK-ACCTNO             TO D01-EW-BANK-ACCTNO.
           MOVE EW-PAYMENT-AMT             TO D01-EW-PAYMENT-AMT.
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
           MOVE "IOEWRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOEWRN-QUEUE-POS  TO Q-POS.
           MOVE IOEWRN-COPIES-POS TO C-POS.
           MOVE IOEWRN-PRU-POS    TO P-POS.
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

           MOVE "EN N040BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-BRANCH.

      *-----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE LEGEND-END-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "EN N040END-BRANCH." TO VDU.
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

           GO TO ENTER-BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE 0               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N040END-BRANCH." TO VDU.
           MOVE 9999            TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-BEG-DATE.

           MOVE SPACES  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-TRANS-DATE.
      *-----------------------------------------------------------------
       ENTER-END-DATE.

           MOVE "ERNM060END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

           MOVE VDU-DATE-YYYYMMDD TO END-TRANS-DATE.

           IF ( BEG-TRANS-DATE > END-TRANS-DATE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-BEG-ACCTNO.
      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "S  8080BEG-DATE." TO VDU.
           MOVE JULIAN-BEG         TO BEG-TRANS-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE." TO VDU.
           MOVE JULIAN-END         TO END-TRANS-DATE VDU-DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-ACCTNO.

           MOVE "EN N080BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-ACCTNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ACCTNO.

           MOVE VDUNUM0 TO BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-END-ACCTNO.

           MOVE "EN N080END-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-ACCTNO.

           IF ( BEG-ACCTNO > END-ACCTNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ACCTNO.

           GO TO ENTER-BEG-SEQ.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SN N080BEG-ACCTNO." TO VDU.
           MOVE 0               TO BEG-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N080END-ACCTNO." TO VDU.
           MOVE 99999999        TO END-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-BEG-SEQ.

           MOVE "EN N020BEG-SEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ACCTNO.
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SEQ.

           IF ( BEG-SEQ > END-SEQ )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SEQ.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-SEQ.

           MOVE "SN N020BEG-SEQ." TO VDU.
           MOVE 0               TO BEG-SEQ VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N020END-SEQ." TO VDU.
           MOVE 99              TO END-SEQ VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SEQ.
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

           MOVE "SN N000BEG-BRANCH."    TO VDU.
           MOVE BEG-BRANCH              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SN N000END-BRANCH."    TO VDU.
           MOVE END-BRANCH              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-DATE."    TO VDU.
           MOVE BEG-TRANS-DATE        TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A000END-DATE."    TO VDU.
           MOVE END-TRANS-DATE        TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "SN N080BEG-ACCTNO."    TO VDU.
           MOVE BEG-ACCTNO              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SN N080END-ACCTNO."    TO VDU.
           MOVE END-ACCTNO              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SN N020BEG-SEQ."    TO VDU.
           MOVE BEG-SEQ              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SN N020END-SEQ."    TO VDU.
           MOVE END-SEQ              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN010PROCESS-KEY." TO VDU
           MOVE PROCESS-KEY           TO VDUNUM0.
           PERFORM SCREENIO.

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

           IF ( PROCESS-KEY = 1 )
              MOVE EW1-KEY TO PACE-DETAIL-KEY
           ELSE
              MOVE EW2-KEY TO PACE-DETAIL-KEY.

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

           MOVE "BEGINNING DATE               : "    TO R01-PROMPT.
           MOVE BEG-TRANS-DATE                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE               : "    TO R01-PROMPT.
           MOVE END-TRANS-DATE                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING ACCTNO               : "    TO R01-PROMPT.
           MOVE BEG-ACCTNO                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ACCTNO               : "    TO R01-PROMPT.
           MOVE END-ACCTNO                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING SEQ               : "    TO R01-PROMPT.
           MOVE BEG-SEQ                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SEQ               : "    TO R01-PROMPT.
           MOVE END-SEQ                             TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - EWFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOEWRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOEWRN-SYSDATE.
           DISPLAY IOEWRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOEWRN-FORM-FIELDS TO WR-BUF.
           MOVE IOEWRN-FORM-LIST TO WR-LIST.
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

           MOVE IOEWRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOEWRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOEWRN_EVA.CPY".
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
      *    LOAD EW-REC FROM QEW-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-EW-FIELDS.
           MOVE QEW-BRNO TO EW-BRNO.

           IF ( QEW-TRANS-DATE = "1900-01-01" )
              MOVE ZEROES TO EW-TRANS-DATE
           ELSE
              MOVE QEW-TRANS-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EW-TRANS-DATE.

           MOVE QEW-ACCTNO TO EW-ACCTNO.
           MOVE QEW-SEQ TO EW-SEQ.

           IF ( QEW-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO EW-LTOUCH-DATE
           ELSE
              MOVE QEW-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EW-LTOUCH-DATE.

           MOVE QEW-NAME TO EW-NAME.
           MOVE QEW-BANK-ACCTNO TO EW-BANK-ACCTNO.
           MOVE QEW-BANK-ROUTING-NO TO EW-BANK-ROUTING-NO.
           MOVE QEW-ACCT-TYPE TO EW-ACCT-TYPE.
           MOVE QEW-PAYMENT-AMT TO EW-PAYMENT-AMT.
           MOVE QEW-PROGRAM TO EW-PROGRAM.
           MOVE QEW-USERID TO EW-USERID.

           IF ( QEW-POST-DATE = "1900-01-01" )
              MOVE ZEROES TO EW-POST-DATE
           ELSE
              MOVE QEW-POST-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EW-POST-DATE.

           MOVE QEW-POST-TIME TO EW-POST-TIME.
 
      *-----------------------------------------------------------------
      *    LOAD QEW-REC FROM EW-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-EW-FIELDS.
           MOVE EW-BRNO TO QEW-BRNO.

           IF ( EW-TRANS-DATE = ZEROES )
              MOVE "1900-01-01" TO QEW-TRANS-DATE
           ELSE
              MOVE EW-TRANS-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEW-TRANS-DATE.

           MOVE EW-ACCTNO TO QEW-ACCTNO.
           MOVE EW-SEQ TO QEW-SEQ.

           IF ( EW-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QEW-LTOUCH-DATE
           ELSE
              MOVE EW-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEW-LTOUCH-DATE.

           MOVE EW-NAME TO QEW-NAME.
           MOVE EW-BANK-ACCTNO TO QEW-BANK-ACCTNO.
           MOVE EW-BANK-ROUTING-NO TO QEW-BANK-ROUTING-NO.
           MOVE EW-ACCT-TYPE TO QEW-ACCT-TYPE.
           MOVE EW-PAYMENT-AMT TO QEW-PAYMENT-AMT.
           MOVE EW-PROGRAM TO QEW-PROGRAM.
           MOVE EW-USERID TO QEW-USERID.

           IF ( EW-POST-DATE = ZEROES )
              MOVE "1900-01-01" TO QEW-POST-DATE
           ELSE
              MOVE EW-POST-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEW-POST-DATE.

           MOVE EW-POST-TIME TO QEW-POST-TIME.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBGB/GBEW1RN
       LOAD-EW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( EW-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( EW-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO EW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO EW-ALL-MACHINE
              MOVE EW-ALL-PATH             TO EW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( EW-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO EW-ALL-BASE
              MOVE FILPATH-MACHINE         TO EW-ALL-MACHINE
              MOVE EW-ALL-PATH             TO EW-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-EWFILE NOT = "S" )
              MOVE EW-PATH-BEG             TO EW-SELECT-PATH-BEG
              MOVE EW-PATH-FILE            TO EW-SELECT-PATH-FILE
           ELSE
              MOVE EW-PATH-SQL             TO EW-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-EW1-FILE.
           PERFORM LOAD-EW1-FILE.
           PERFORM OPEN-IT.
           MOVE EW-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-EW1-FILE.
           PERFORM START-IT.
           MOVE EW-SELECT-PATH TO E-FILE.
           MOVE EW1-KEY TO E-KEYX.

           IF ( EW1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-EW1-FILE.

           IF ( QEW1-WEND-SEQ = ZEROES    ) OR
              ( QEW1-WEND-SEQ NOT NUMERIC )
              MOVE EW-BRNO              TO QEW1-WBEG-BRNO
              MOVE 9999                 TO QEW1-WEND-BRNO
              MOVE EW-ACCTNO            TO QEW1-WBEG-ACCTNO
              MOVE 99999999             TO QEW1-WEND-ACCTNO
              MOVE EW-SEQ               TO QEW1-WBEG-SEQ
              MOVE 99                   TO QEW1-WEND-SEQ
              MOVE EW-TRANS-DATE        TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QEW1-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QEW1-WEND-DATE.

           EXEC SQL
            DECLARE EW1_CURSOR CURSOR FOR
            SELECT EWFILE.EW_BRNO,
                   ISNULL(CAST(EWFILE.EW_TRANS_DATE
                                                  AS VARCHAR(10)),'0'),
                   EWFILE.EW_ACCTNO,
                   EWFILE.EW_SEQ,
                   ISNULL(CAST(EWFILE.EW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(EWFILE.EW_NAME,''),
                   ISNULL(EWFILE.EW_BANK_ACCTNO,''),
                   EWFILE.EW_BANK_ROUTING_NO,
                   ISNULL(EWFILE.EW_ACCT_TYPE,''),
                   EWFILE.EW_PAYMENT_AMT,
                   ISNULL(EWFILE.EW_PROGRAM,''),
                   ISNULL(EWFILE.EW_USERID,''),
                   ISNULL(CAST(EWFILE.EW_POST_DATE
                                                  AS VARCHAR(10)),'0'),
                   EWFILE.EW_POST_TIME
            FROM DBO.EWFILE
            WHERE EWFILE.EW_BRNO >= :QEW1-WBEG-BRNO
              AND EWFILE.EW_BRNO <= :QEW1-WEND-BRNO
              AND CAST(EWFILE.EW_TRANS_DATE AS VARCHAR(10))
                           >= :QEW1-WBEG-DATE
              AND CAST(EWFILE.EW_TRANS_DATE AS VARCHAR(10))
                           <= :QEW1-WEND-DATE
              AND EWFILE.EW_ACCTNO >= :QEW1-WBEG-ACCTNO
              AND EWFILE.EW_ACCTNO <= :QEW1-WEND-ACCTNO
              AND EWFILE.EW_SEQ >= :QEW1-WBEG-SEQ
              AND EWFILE.EW_SEQ <= :QEW1-WEND-SEQ
            ORDER BY EWFILE.EW_BRNO,
                     EWFILE.EW_TRANS_DATE,
                     EWFILE.EW_ACCTNO,
                     EWFILE.EW_SEQ
           END-EXEC.

           EXEC SQL
               OPEN EW1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QEW1-WBEG-BRNO
                          QEW1-WEND-BRNO
                          QEW1-WBEG-ACCTNO
                          QEW1-WEND-ACCTNO
                          QEW1-WBEG-SEQ
                          QEW1-WEND-SEQ.
           MOVE SPACES TO QEW1-WBEG-DATE
                          QEW1-WEND-DATE.

           MOVE STAT---GOOD TO EW1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-EW1-FILE.
           PERFORM READ-IT.
           MOVE EW-SELECT-PATH TO E-FILE.
           MOVE EW1-KEY TO E-KEYX.

           MOVE EW-BRNO             TO QEW-BRNO. 
           MOVE EW-TRANS-DATE       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QEW-TRANS-DATE.
           MOVE EW-ACCTNO           TO QEW-ACCTNO.
           MOVE EW-SEQ              TO QEW-SEQ.

           EXEC SQL
            SELECT EWFILE.EW_BRNO,
                   ISNULL(CAST(EWFILE.EW_TRANS_DATE
                                                  AS VARCHAR(10)),'0'),
                   EWFILE.EW_ACCTNO,
                   EWFILE.EW_SEQ,
                   ISNULL(CAST(EWFILE.EW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(EWFILE.EW_NAME,''),
                   ISNULL(EWFILE.EW_BANK_ACCTNO,''),
                   EWFILE.EW_BANK_ROUTING_NO,
                   ISNULL(EWFILE.EW_ACCT_TYPE,''),
                   EWFILE.EW_PAYMENT_AMT,
                   ISNULL(EWFILE.EW_PROGRAM,''),
                   ISNULL(EWFILE.EW_USERID,''),
                   ISNULL(CAST(EWFILE.EW_POST_DATE
                                                  AS VARCHAR(10)),'0'),
                   EWFILE.EW_POST_TIME
            INTO :QEW-REC
            FROM DBO.EWFILE
            WHERE EWFILE.EW_BRNO = :QEW-BRNO
              AND EWFILE.EW_TRANS_DATE = :QEW-TRANS-DATE
              AND EWFILE.EW_ACCTNO = :QEW-ACCTNO
              AND EWFILE.EW_SEQ = :QEW-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-EW1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-EW-FIELDS.

      *-----------------------------------------------------------------
       READ-EW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE EW-SELECT-PATH TO E-FILE.
           MOVE EW1-KEY TO E-KEYX.
           INITIALIZE QEW-REC.

           IF ( EW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH EW1_CURSOR INTO :QEW-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-EW1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-EW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-EW1-FILE.
           IF ( EW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE EW1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO EW1-CURSOR-STAT.

      *-----------------------------------------------------------------
       LOAD-EW2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( EW-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( EW-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO EW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO EW-ALL-MACHINE
              MOVE EW-ALL-PATH             TO EW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( EW-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO EW-ALL-BASE
              MOVE FILPATH-MACHINE         TO EW-ALL-MACHINE
              MOVE EW-ALL-PATH             TO EW-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-EWFILE NOT = "S" )
              MOVE EW-PATH-BEG             TO EW-SELECT-PATH-BEG
              MOVE EW-PATH-FILE            TO EW-SELECT-PATH-FILE
           ELSE
              MOVE EW-PATH-SQL             TO EW-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-EW2-FILE.
           PERFORM LOAD-EW2-FILE.
           PERFORM OPEN-IT.
           MOVE EW-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-EW2-FILE.
           PERFORM START-IT.
           MOVE EW-SELECT-PATH TO E-FILE.
           MOVE EW2-KEY TO E-KEYX.

           IF ( EW2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-EW2-FILE.

           IF ( QEW2-WEND-DATE = SPACES )
              MOVE EW-TRANS-DATE        TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QEW2-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QEW2-WEND-DATE.

           EXEC SQL
            DECLARE EW2_CURSOR CURSOR FOR
            SELECT EWFILE.EW_BRNO,
                   ISNULL(CAST(EWFILE.EW_TRANS_DATE
                                                  AS VARCHAR(10)),'0'),
                   EWFILE.EW_ACCTNO,
                   EWFILE.EW_SEQ,
                   ISNULL(CAST(EWFILE.EW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(EWFILE.EW_NAME,''),
                   ISNULL(EWFILE.EW_BANK_ACCTNO,''),
                   EWFILE.EW_BANK_ROUTING_NO,
                   ISNULL(EWFILE.EW_ACCT_TYPE,''),
                   EWFILE.EW_PAYMENT_AMT,
                   ISNULL(EWFILE.EW_PROGRAM,''),
                   ISNULL(EWFILE.EW_USERID,''),
                   ISNULL(CAST(EWFILE.EW_POST_DATE
                                                  AS VARCHAR(10)),'0'),
                   EWFILE.EW_POST_TIME
            FROM DBO.EWFILE
            WHERE CAST(EWFILE.EW_TRANS_DATE AS VARCHAR(10))
                           >= :QEW2-WBEG-DATE
              AND CAST(EWFILE.EW_TRANS_DATE AS VARCHAR(10))
                           <= :QEW2-WEND-DATE
            ORDER BY EWFILE.EW_TRANS_DATE,
                     EWFILE.EW_BRNO,
                     EWFILE.EW_ACCTNO,
                     EWFILE.EW_SEQ
           END-EXEC.

           EXEC SQL
               OPEN EW2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QEW2-WBEG-DATE
                          QEW2-WEND-DATE.

           MOVE STAT---GOOD TO EW2-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-EW2-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE EW-SELECT-PATH TO E-FILE.
           MOVE EW2-KEY TO E-KEYX.

           IF ( EW2-NG-CURSOR-STAT-GOOD )
              PERFORM CLOSE-EW2-FILE.

           IF ( QEW2-WEND-DATE = SPACES )
      * SET THESE BACKWARDS FOR A START NOT GREATER
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QEW2-WBEG-DATE
              MOVE EW-TRANS-DATE        TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QEW2-WEND-DATE.

           EXEC SQL
            DECLARE EW2_NG_CURSOR CURSOR FOR
            SELECT EWFILE.EW_BRNO,
                   ISNULL(CAST(EWFILE.EW_TRANS_DATE
                                                  AS VARCHAR(10)),'0'),
                   EWFILE.EW_ACCTNO,
                   EWFILE.EW_SEQ,
                   ISNULL(CAST(EWFILE.EW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(EWFILE.EW_NAME,''),
                   ISNULL(EWFILE.EW_BANK_ACCTNO,''),
                   EWFILE.EW_BANK_ROUTING_NO,
                   ISNULL(EWFILE.EW_ACCT_TYPE,''),
                   EWFILE.EW_PAYMENT_AMT,
                   ISNULL(EWFILE.EW_PROGRAM,''),
                   ISNULL(EWFILE.EW_USERID,''),
                   ISNULL(CAST(EWFILE.EW_POST_DATE
                                                  AS VARCHAR(10)),'0'),
                   EWFILE.EW_POST_TIME
            FROM DBO.EWFILE
            WHERE CAST(EWFILE.EW_TRANS_DATE AS VARCHAR(10))
                           >= :QEW2-WBEG-DATE
              AND CAST(EWFILE.EW_TRANS_DATE AS VARCHAR(10))
                           <= :QEW2-WEND-DATE
            ORDER BY EWFILE.EW_TRANS_DATE DESC
           END-EXEC.

           EXEC SQL
               OPEN EW2_NG_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QEW2-WBEG-DATE
                          QEW2-WEND-DATE.

           MOVE STAT---GOOD TO EW2-NG-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-EW2-FILE.
           PERFORM READ-IT.
           MOVE EW-SELECT-PATH TO E-FILE.
           MOVE EW2-KEY TO E-KEYX.

           MOVE EW-TRANS-DATE       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QEW-TRANS-DATE.

           EXEC SQL
            SELECT EWFILE.EW_BRNO,
                   ISNULL(CAST(EWFILE.EW_TRANS_DATE
                                                  AS VARCHAR(10)),'0'),
                   EWFILE.EW_ACCTNO,
                   EWFILE.EW_SEQ,
                   ISNULL(CAST(EWFILE.EW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(EWFILE.EW_NAME,''),
                   ISNULL(EWFILE.EW_BANK_ACCTNO,''),
                   EWFILE.EW_BANK_ROUTING_NO,
                   ISNULL(EWFILE.EW_ACCT_TYPE,''),
                   EWFILE.EW_PAYMENT_AMT,
                   ISNULL(EWFILE.EW_PROGRAM,''),
                   ISNULL(EWFILE.EW_USERID,''),
                   ISNULL(CAST(EWFILE.EW_POST_DATE
                                                  AS VARCHAR(10)),'0'),
                   EWFILE.EW_POST_TIME
            INTO :QEW-REC
            FROM DBO.EWFILE
            WHERE EWFILE.EW_TRANS_DATE = :QEW-TRANS-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-EW2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-EW-FIELDS.

      *-----------------------------------------------------------------
       READ-EW2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE EW-SELECT-PATH TO E-FILE.
           MOVE EW2-KEY TO E-KEYX.
           INITIALIZE QEW-REC.

           IF ( EW2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH EW2_CURSOR INTO :QEW-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-EW2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-EW-FIELDS.


      *-----------------------------------------------------------------
       READ-EW2-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE EW-SELECT-PATH TO E-FILE.
           MOVE EW2-KEY TO E-KEYX.
           INITIALIZE QEW-REC.

           IF ( EW2-NG-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH EW2_NG_CURSOR INTO :QEW-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-EW2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-EW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-EW2-FILE.
           IF ( EW2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE EW2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO EW2-CURSOR-STAT.

           IF ( EW2-NG-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE EW2_NG_CURSOR
              END-EXEC
              MOVE STAT---BAD TO EW2-NG-CURSOR-STAT.

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
              DESTROY IOEWRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
