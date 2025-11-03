       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IORCRN.
       AUTHOR.        BAH
       DATE-WRITTEN.  03-11-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IORCRN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : RCFILE 
      * TABLE DESC    : DAILY STATUS FILE
      * OUTPUT FILE   : XPC/IORCRN.PRE
      * USED FOR      : LIBGB/GBWSRC.CPY  --- QRC-REC
      *                 LIBGB/GBRC1R.CPY  --- READ
      *                 LIBGB/GBRC1RN.CPY --- READ/READ-NEXT/START
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
           "@(#)IO/IORCRN.CBL S34 05/18/21-11:12:44 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GBWSRC.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S 01 SQL COPY MEMBERS
      *#################################################################
      *01  QRC-DATE-EDIT               PIC 9999/99/99.
      *01  QRC-DATE-EDITX              REDEFINES QRC-DATE-EDIT
      *                                PIC X(10).

      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QRC1-WHERE-BEG-KEY.
           03  QRC1-WBEG-BRNO           PIC 9(04).
           03  QRC1-WBEG-TRANS-DATE     PIC X(10).

       01  QRC1-WHERE-END-KEY.
           03  QRC1-WEND-BRNO           PIC 9(04).
           03  QRC1-WEND-TRANS-DATE     PIC X(10).

       01  QRC2-WHERE-BEG-KEY.
           03  QRC2-WBEG-STATUS         PIC X(02).
           03  QRC2-WBEG-BRNO           PIC 9(04).
           03  QRC2-WBEG-TRANS-DATE     PIC X(10).

       01  QRC2-WHERE-END-KEY.
           03  QRC2-WEND-STATUS         PIC X(02).
           03  QRC2-WEND-BRNO           PIC 9(04).
           03  QRC2-WEND-TRANS-DATE     PIC X(10).

       01  QRC-REC.
           03  QRC-STATUS              PIC XX.
           03  QRC-BRNO                PIC 9(4).
           03  QRC-TRANS-DATE          PIC X(10).
           03  QRC-LTOUCH-DATE         PIC X(10).
           03  QRC-RECAP-FG             PIC X.
           03  QRC-GL-FG                PIC X.
           03  QRC-EOD-BATCH-FG         PIC X.
           03  QRC-NIGHTLY-FUNDS-FG     PIC X.
           03  QRC-EOM1-BATCH-FG        PIC X.
           03  QRC-EOM-UPDATE-FG        PIC X.
           03  QRC-EOM2-BATCH-FG        PIC X.
           03  QRC-BS-P2P3-INBAL-FG     PIC X.
           03  QRC-DEPOSIT              PIC S9(7)V99.
           03  QRC-OVER-SHORT           PIC S9(7)V99.
           03  QRC-CHECKS               PIC S9(7)V99.
           03  QRC-OPENED-DATE          PIC X(10).
           03  QRC-OPENED-TIME          PIC 9(6).
           03  QRC-OP-USERID            PIC X(10).
           03  QRC-CLOSED-DATE          PIC X(10).
           03  QRC-CLOSED-TIME          PIC 9(6).
           03  QRC-CL-USERID            PIC X(10).
           03  QRC-UPDATED-DATE         PIC X(10).
           03  QRC-UPDATED-TIME         PIC 9(6).
           03  QRC-UPDATED-USERID       PIC X(10).
           03  QRC-EOD-BATCH-DATE       PIC X(10).
           03  QRC-EOD-BATCH-TIME       PIC 9(6).
           03  QRC-FUNDS-DATE           PIC X(10).
           03  QRC-FUNDS-TIME           PIC 9(6).
           03  QRC-EARNINGS-DATE        PIC X(10).
           03  QRC-EARNINGS-TIME        PIC 9(6).
           03  QRC-EOM-UPDATED-DATE     PIC X(10).
           03  QRC-EOM-UPDATED-TIME     PIC 9(6).
           03  QRC-EOM2-BATCH-DATE      PIC X(10).
           03  QRC-EOM2-BATCH-TIME      PIC 9(6).
           03  QRC-CHECK-DATE           PIC X(10).
           03  QRC-CHECK-TIME           PIC 9(6).
           03  QRC-LAST-ACCRUE          PIC 9(4).
           03  QRC-ERROR-MSG            PIC X(15).
           03  QRC-BANK-BAL             PIC S9(9)V99.
           03  QRC-CHECK-DEPOSIT        PIC S9(7)V99.
       EXEC SQL END DECLARE SECTION END-EXEC.
      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S 01 SQL COPY MEMBERS
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
           03  FILLER PIC X(30) VALUE "BEGINNING BRANCH      (F3-ALL) ".

       01  LEGEND-END-BRANCH. 
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING BRANCH      ".

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
           03  BEG-BRANCH               PIC 9(04).
           03  END-BRANCH               PIC 9(04).
           03  BEG-DATE                 PIC 9(08).
           03  END-DATE                 PIC 9(08).
           03  BEG-STATUS               PIC XX.
           03  END-STATUS               PIC XX.

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
           03  FILLER PIC X(26) VALUE "BRNO       DATE  STAT     ".
           03  FILLER PIC X(33) VALUE "DEPOSIT   OP USERID   CL TIME".
           03  FILLER PIC X(30) VALUE "UPDAT DATE   BANK BAL".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.
      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-RC-BRNO                    PIC 9999.
           03  FILLER                         PIC X(01).
           03  D01-RC-DATE                    PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-RC-STATUS                  PIC XX.
           03  FILLER                         PIC X(03).
           03  D01-RC-DEPOSIT                 PIC Z(7).99.
           03  FILLER                         PIC X(03).
           03  D01-RC-OP-USERID               PIC X(10).
           03  FILLER                         PIC X(03).
           03  D01-RC-CLOSED-TIME             PIC 9(6).
           03  FILLER                         PIC X(03).
           03  D01-RC-UPDATED-DATE            PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-RC-BANK-BAL                PIC Z(7).99.

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
       COPY "LIBIO/IORCRN_DEF.CPY".
       COPY "LIBIO/IORCRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IORCRN_SCN.CPY".

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

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      D    STOP MESS.
           IF ( QRC1-WBEG-TRANS-DATE = SPACES )
              MOVE "ABC"  TO MESS.
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      D    STOP FORM-PATHNAME.
           MOVE "B"                  TO RC-PATH-OVERRIDE.

           MOVE BEG-STATUS           TO QRC2-WBEG-STATUS.
           MOVE END-STATUS           TO QRC2-WEND-STATUS.

           MOVE BEG-BRANCH           TO QRC1-WBEG-BRNO
                                        QRC2-WBEG-BRNO.
           MOVE END-BRANCH           TO QRC1-WEND-BRNO
                                        QRC2-WEND-BRNO.

      *OLD MOVE BEG-DATE             TO QRC-DATE-EDIT.
      *OLD INSPECT QRC-DATE-EDIT REPLACING ALL "/" BY "-".
      *OLD MOVE QRC-DATE-EDITX       TO QRC1-WBEG-TRANS-DATE
      *OLD                              QRC2-WBEG-TRANS-DATE.

           MOVE BEG-DATE             TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC1-WBEG-TRANS-DATE
                                        QRC2-WBEG-TRANS-DATE.

      *OLD MOVE END-DATE             TO QRC-DATE-EDIT.
      *OLD INSPECT QRC-DATE-EDIT REPLACING ALL "/" BY "-".
      *OLD MOVE QRC-DATE-EDITX       TO QRC1-WEND-TRANS-DATE
      *OLD                              QRC2-WEND-TRANS-DATE.

           MOVE END-DATE             TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC1-WEND-TRANS-DATE
                                        QRC2-WEND-TRANS-DATE.

           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-RC1-FILE
              PERFORM START-RC1-FILE
           ELSE
           IF ( PROCESS-KEY = 9 )
              PERFORM OPEN-RC1-FILE
           ELSE
              PERFORM OPEN-RC2-FILE
              PERFORM START-RC2-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-RC1-FILE-NEXT
           ELSE
      * KEY READ
           IF (PROCESS-KEY = 9)
              MOVE BEG-BRANCH TO RC-BRNO
              MOVE BEG-DATE   TO RC-TRANS-DATE
              PERFORM READ-RC1-FILE
           ELSE
              PERFORM READ-RC2-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF PROCESS-KEY = 1 OR 9
              IF ( RC-BRNO < BEG-BRANCH )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( RC-BRNO > END-BRANCH )
                 GO TO MAIN-PROGRAM-END.

           IF PROCESS-KEY = 2
              IF ( RC-STATUS < BEG-STATUS )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( RC-STATUS > END-STATUS )
                 GO TO MAIN-PROGRAM-END.

           IF PROCESS-KEY = 2
              IF RC-BRNO < BEG-BRANCH OR RC-BRNO > END-BRANCH
                 GO TO MAIN-PROGRAM-NEXT.

           IF RC-TRANS-DATE < BEG-DATE OR RC-TRANS-DATE > END-DATE
              GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE RC-BRNO                    TO D01-RC-BRNO.
           MOVE RC-TRANS-DATE              TO D01-RC-DATE.
           MOVE RC-STATUS                  TO D01-RC-STATUS.
           MOVE RC-DEPOSIT                 TO D01-RC-DEPOSIT.
           MOVE RC-OP-USERID               TO D01-RC-OP-USERID.
           MOVE RC-UPDATED-DATE            TO D01-RC-UPDATED-DATE.
           MOVE RC-BANK-BAL                    TO D01-RC-BANK-BAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.

      * KEY READ
           IF (PROCESS-KEY = 9)
              GO TO MAIN-PROGRAM-END.

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
           MOVE "IORCRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IORCRN-QUEUE-POS  TO Q-POS.
           MOVE IORCRN-COPIES-POS TO C-POS.
           MOVE IORCRN-PRU-POS    TO P-POS.
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
           MOVE 9999              TO END-BRANCH VDUNUM0.
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

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           MOVE "ERNM060END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF ( BEG-DATE > END-DATE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-BEG-STATUS.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "S  8080BEG-DATE." TO VDU.
           MOVE JULIAN-BEG         TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE." TO VDU.
           MOVE JULIAN-END         TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           PERFORM SCREENIO.

       ENTER-BEG-STATUS.

           MOVE "E  A020BEG-STATUS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-STATUS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

           MOVE VDUBUF TO BEG-STATUS.

      *-----------------------------------------------------------------
       ENTER-END-STATUS.

           MOVE "E  A020END-STATUS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-STATUS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-STATUS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-STATUS.

           IF ( BEG-STATUS > END-STATUS )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-STATUS.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-STATUS.

           MOVE "S  A020BEG-STATUS." TO VDU.
           MOVE SPACES               TO BEG-STATUS VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020END-STATUS." TO VDU.
           MOVE "ZZ"                 TO END-STATUS VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-STATUS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 < ZEROES ) OR
              ( VDUNUM0 > 2      )     
              IF VDUNUM0 NOT = 9
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

           MOVE "S  N000BEG-BRANCH."    TO VDU.
           MOVE BEG-BRANCH              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  N000END-BRANCH."    TO VDU.
           MOVE END-BRANCH              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-DATE."    TO VDU.
           MOVE BEG-DATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A000END-DATE."    TO VDU.
           MOVE END-DATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-STATUS."    TO VDU.
           MOVE BEG-STATUS              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-STATUS."    TO VDU.
           MOVE END-STATUS              TO VDUBUF.
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

           IF ( PROCESS-KEY = 1 )
              MOVE RC1-KEY TO PACE-DETAIL-KEY
           ELSE
              MOVE RC2-KEY TO PACE-DETAIL-KEY.

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
           MOVE BEG-DATE                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE               : "    TO R01-PROMPT.
           MOVE END-DATE                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


           MOVE "BEGINNING STATUS               : "    TO R01-PROMPT.
           MOVE BEG-STATUS                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    STATUS               : "    TO R01-PROMPT.
           MOVE END-STATUS                             TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - RCFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IORCRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IORCRN-SYSDATE.
           DISPLAY IORCRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IORCRN-FORM-FIELDS TO WR-BUF.
           MOVE IORCRN-FORM-LIST TO WR-LIST.
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

           MOVE IORCRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IORCRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IORCRN_EVA.CPY".
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
      *    LOAD RC-REC FROM QRC-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-RC-FIELDS.
           MOVE QRC-STATUS TO RC-STATUS.
           MOVE QRC-BRNO TO RC-BRNO.

           IF ( QRC-TRANS-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-TRANS-DATE
           ELSE
              MOVE QRC-TRANS-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-TRANS-DATE.


           IF ( QRC-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-LTOUCH-DATE
           ELSE
              MOVE QRC-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-LTOUCH-DATE.

           MOVE QRC-RECAP-FG TO RC-RECAP-FG.
           MOVE QRC-GL-FG TO RC-GL-FG.
           MOVE QRC-EOD-BATCH-FG TO RC-EOD-BATCH-FG.
           MOVE QRC-NIGHTLY-FUNDS-FG TO RC-NIGHTLY-FUNDS-FG.
           MOVE QRC-EOM1-BATCH-FG TO RC-EOM1-BATCH-FG.
           MOVE QRC-EOM-UPDATE-FG TO RC-EOM-UPDATE-FG.
           MOVE QRC-EOM2-BATCH-FG TO RC-EOM2-BATCH-FG.
           MOVE QRC-BS-P2P3-INBAL-FG TO RC-BS-P2P3-INBAL-FG.
           MOVE QRC-DEPOSIT TO RC-DEPOSIT.
           MOVE QRC-OVER-SHORT TO RC-OVER-SHORT.
           MOVE QRC-CHECKS TO RC-CHECKS.

           IF ( QRC-OPENED-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-OPENED-DATE
           ELSE
              MOVE QRC-OPENED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-OPENED-DATE.

           MOVE QRC-OPENED-TIME TO RC-OPENED-TIME.
           MOVE QRC-OP-USERID TO RC-OP-USERID.

           IF ( QRC-CLOSED-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-CLOSED-DATE
           ELSE
              MOVE QRC-CLOSED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-CLOSED-DATE.

           MOVE QRC-CLOSED-TIME TO RC-CLOSED-TIME.
           MOVE QRC-CL-USERID TO RC-CL-USERID.

           IF ( QRC-UPDATED-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-UPDATED-DATE
           ELSE
              MOVE QRC-UPDATED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-UPDATED-DATE.

           MOVE QRC-UPDATED-TIME TO RC-UPDATED-TIME.

           IF ( QRC-UPDATED-USERID = "1900-01-01" )
              MOVE ZEROES TO RC-UPDATED-USERID
           ELSE
              MOVE QRC-UPDATED-USERID TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-UPDATED-USERID.


           IF ( QRC-EOD-BATCH-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-EOD-BATCH-DATE
           ELSE
              MOVE QRC-EOD-BATCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-EOD-BATCH-DATE.

           MOVE QRC-EOD-BATCH-TIME TO RC-EOD-BATCH-TIME.

           IF ( QRC-FUNDS-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-FUNDS-DATE
           ELSE
              MOVE QRC-FUNDS-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-FUNDS-DATE.

           MOVE QRC-FUNDS-TIME TO RC-FUNDS-TIME.

           IF ( QRC-EARNINGS-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-EARNINGS-DATE
           ELSE
              MOVE QRC-EARNINGS-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-EARNINGS-DATE.

           MOVE QRC-EARNINGS-TIME TO RC-EARNINGS-TIME.

           IF ( QRC-EOM-UPDATED-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-EOM-UPDATED-DATE
           ELSE
              MOVE QRC-EOM-UPDATED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-EOM-UPDATED-DATE.

           MOVE QRC-EOM-UPDATED-TIME TO RC-EOM-UPDATED-TIME.

           IF ( QRC-EOM2-BATCH-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-EOM2-BATCH-DATE
           ELSE
              MOVE QRC-EOM2-BATCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-EOM2-BATCH-DATE.

           MOVE QRC-EOM2-BATCH-TIME TO RC-EOM2-BATCH-TIME.

           IF ( QRC-CHECK-DATE = "1900-01-01" )
              MOVE ZEROES TO RC-CHECK-DATE
           ELSE
              MOVE QRC-CHECK-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RC-CHECK-DATE.

           MOVE QRC-CHECK-TIME TO RC-CHECK-TIME.
           MOVE QRC-LAST-ACCRUE TO RC-LAST-ACCRUE.
           MOVE QRC-ERROR-MSG TO RC-ERROR-MSG.
           MOVE QRC-BANK-BAL TO RC-BANK-BAL.
           MOVE QRC-CHECK-DEPOSIT TO RC-CHECK-DEPOSIT.
 
      *-----------------------------------------------------------------
      *    LOAD QRC-REC FROM RC-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-RC-FIELDS.
           MOVE RC-STATUS TO QRC-STATUS.
           MOVE RC-BRNO TO QRC-BRNO.

           IF ( RC-TRANS-DATE = ZEROES )
              MOVE "1900-01-01" TO QRC-TRANS-DATE
           ELSE
              MOVE RC-TRANS-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRC-TRANS-DATE.


           IF ( RC-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QRC-LTOUCH-DATE
           ELSE
              MOVE RC-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRC-LTOUCH-DATE.

           MOVE RC-RECAP-FG TO QRC-RECAP-FG.
           MOVE RC-GL-FG TO QRC-GL-FG.
           MOVE RC-EOD-BATCH-FG TO QRC-EOD-BATCH-FG.
           MOVE RC-NIGHTLY-FUNDS-FG TO QRC-NIGHTLY-FUNDS-FG.
           MOVE RC-EOM1-BATCH-FG TO QRC-EOM1-BATCH-FG.
           MOVE RC-EOM-UPDATE-FG TO QRC-EOM-UPDATE-FG.
           MOVE RC-EOM2-BATCH-FG TO QRC-EOM2-BATCH-FG.
           MOVE RC-BS-P2P3-INBAL-FG TO QRC-BS-P2P3-INBAL-FG.
           MOVE RC-DEPOSIT TO QRC-DEPOSIT.
           MOVE RC-OVER-SHORT TO QRC-OVER-SHORT.
           MOVE RC-CHECKS TO QRC-CHECKS.

           IF ( RC-OPENED-DATE = ZEROES )
              MOVE "1900-01-01" TO QRC-OPENED-DATE
           ELSE
              MOVE RC-OPENED-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRC-OPENED-DATE.

           MOVE RC-OPENED-TIME TO QRC-OPENED-TIME.
           MOVE RC-OP-USERID TO QRC-OP-USERID.

           IF ( RC-CLOSED-DATE = ZEROES )
              MOVE "1900-01-01" TO QRC-CLOSED-DATE
           ELSE
              MOVE RC-CLOSED-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRC-CLOSED-DATE.

           MOVE RC-CLOSED-TIME TO QRC-CLOSED-TIME.
           MOVE RC-CL-USERID TO QRC-CL-USERID.

           IF ( RC-UPDATED-DATE = ZEROES )
              MOVE "1900-01-01" TO QRC-UPDATED-DATE
           ELSE
              MOVE RC-UPDATED-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRC-UPDATED-DATE.

           MOVE RC-UPDATED-TIME TO QRC-UPDATED-TIME.

           IF ( RC-UPDATED-USERID = ZEROES )
              MOVE "1900-01-01" TO QRC-UPDATED-USERID
           ELSE
              MOVE RC-UPDATED-USERID TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRC-UPDATED-USERID.


           IF ( RC-EOD-BATCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QRC-EOD-BATCH-DATE
           ELSE
              MOVE RC-EOD-BATCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRC-EOD-BATCH-DATE.

           MOVE RC-EOD-BATCH-TIME TO QRC-EOD-BATCH-TIME.

           IF ( RC-FUNDS-DATE = ZEROES )
              MOVE "1900-01-01" TO QRC-FUNDS-DATE
           ELSE
              MOVE RC-FUNDS-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRC-FUNDS-DATE.

           MOVE RC-FUNDS-TIME TO QRC-FUNDS-TIME.

           IF ( RC-EARNINGS-DATE = ZEROES )
              MOVE "1900-01-01" TO QRC-EARNINGS-DATE
           ELSE
              MOVE RC-EARNINGS-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRC-EARNINGS-DATE.

           MOVE RC-EARNINGS-TIME TO QRC-EARNINGS-TIME.

           IF ( RC-EOM-UPDATED-DATE = ZEROES )
              MOVE "1900-01-01" TO QRC-EOM-UPDATED-DATE
           ELSE
              MOVE RC-EOM-UPDATED-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRC-EOM-UPDATED-DATE.

           MOVE RC-EOM-UPDATED-TIME TO QRC-EOM-UPDATED-TIME.

           IF ( RC-EOM2-BATCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QRC-EOM2-BATCH-DATE
           ELSE
              MOVE RC-EOM2-BATCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRC-EOM2-BATCH-DATE.

           MOVE RC-EOM2-BATCH-TIME TO QRC-EOM2-BATCH-TIME.

           IF ( RC-CHECK-DATE = ZEROES )
              MOVE "1900-01-01" TO QRC-CHECK-DATE
           ELSE
              MOVE RC-CHECK-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRC-CHECK-DATE.

           MOVE RC-CHECK-TIME TO QRC-CHECK-TIME.
           MOVE RC-LAST-ACCRUE TO QRC-LAST-ACCRUE.
           MOVE RC-ERROR-MSG TO QRC-ERROR-MSG.
           MOVE RC-BANK-BAL TO QRC-BANK-BAL.
           MOVE RC-CHECK-DEPOSIT TO QRC-CHECK-DEPOSIT.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBGB/GBRC1RN
       LOAD-RC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( RC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( RC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO RC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO RC-ALL-MACHINE
              MOVE RC-ALL-PATH             TO RC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( RC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO RC-ALL-BASE
              MOVE FILPATH-MACHINE         TO RC-ALL-MACHINE
              MOVE RC-ALL-PATH             TO RC-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-RCFILE NOT = "S" )
              MOVE RC-PATH-BEG             TO RC-SELECT-PATH-BEG
              MOVE RC-PATH-FILE            TO RC-SELECT-PATH-FILE
           ELSE
              MOVE RC-PATH-SQL             TO RC-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-RC1-FILE.
           PERFORM LOAD-RC1-FILE.
           PERFORM OPEN-IT.
           MOVE RC-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-RC1-FILE.
           PERFORM START-IT.
           MOVE RC-SELECT-PATH TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.

           IF ( RC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-RC1-FILE.

           IF ( QRC1-WEND-BRNO = ZEROES    ) OR
              ( QRC1-WEND-BRNO NOT NUMERIC )
              MOVE RC-BRNO              TO QRC1-WBEG-BRNO
              MOVE ALL "9"              TO QRC1-WEND-BRNO

              MOVE RC-TRANS-DATE        TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRC1-WBEG-TRANS-DATE

              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRC1-WBEG-TRANS-DATE.

           EXEC SQL
            DECLARE RC1_CURSOR CURSOR FOR
            SELECT ISNULL(RCFILE.RC_STATUS,''),
                   RCFILE.RC_BRNO,
                   ISNULL(CAST(RCFILE.RC_TRANS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(RCFILE.RC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(RCFILE.RC_RECAP_FG,''),
                   ISNULL(RCFILE.RC_GL_FG,''),
                   ISNULL(RCFILE.RC_EOD_BATCH_FG,''),
                   ISNULL(RCFILE.RC_NIGHTLY_FUNDS_FG,''),
                   ISNULL(RCFILE.RC_EOM1_BATCH_FG,''),
                   ISNULL(RCFILE.RC_EOM_UPDATE_FG,''),
                   ISNULL(RCFILE.RC_EOM2_BATCH_FG,''),
                   ISNULL(RCFILE.RC_BS_P2P3_INBAL_FG,''),
                   RCFILE.RC_DEPOSIT,
                   RCFILE.RC_OVER_SHORT,
                   RCFILE.RC_CHECKS,
                   ISNULL(CAST(RCFILE.RC_OPENED_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_OPENED_TIME,
                   ISNULL(RCFILE.RC_OP_USERID,''),
                   ISNULL(CAST(RCFILE.RC_CLOSED_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_CLOSED_TIME,
                   ISNULL(RCFILE.RC_CL_USERID,''),
                   ISNULL(CAST(RCFILE.RC_UPDATED_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_UPDATED_TIME,
                   ISNULL(CAST(RCFILE.RC_UPDATED_USERID
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(RCFILE.RC_EOD_BATCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_EOD_BATCH_TIME,
                   ISNULL(CAST(RCFILE.RC_FUNDS_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_FUNDS_TIME,
                   ISNULL(CAST(RCFILE.RC_EARNINGS_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_EARNINGS_TIME,
                   ISNULL(CAST(RCFILE.RC_EOM_UPDATED_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_EOM_UPDATED_TIME,
                   ISNULL(CAST(RCFILE.RC_EOM2_BATCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_EOM2_BATCH_TIME,
                   ISNULL(CAST(RCFILE.RC_CHECK_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_CHECK_TIME,
                   RCFILE.RC_LAST_ACCRUE,
                   ISNULL(RCFILE.RC_ERROR_MSG,''),
                   RCFILE.RC_BANK_BAL,
                   RCFILE.RC_CHECK_DEPOSIT
            FROM DBO.RCFILE
            WHERE RCFILE.RC_BRNO >= :QRC1-WBEG-BRNO
              AND RCFILE.RC_BRNO <= :QRC1-WEND-BRNO
              AND CAST(RCFILE.RC_TRANS_DATE AS VARCHAR(10))
                          >= :QRC1-WBEG-TRANS-DATE
              AND CAST(RCFILE.RC_TRANS_DATE AS VARCHAR(10))
                          <= :QRC1-WEND-TRANS-DATE
            ORDER BY RCFILE.RC_BRNO,
                     RCFILE.RC_TRANS_DATE
           END-EXEC.

           EXEC SQL
               OPEN RC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QRC1-WBEG-BRNO
                          QRC1-WEND-BRNO.

           MOVE SPACES TO QRC1-WBEG-TRANS-DATE
                          QRC1-WEND-TRANS-DATE.

           MOVE STAT---GOOD TO RC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-RC1-FILE.
           PERFORM READ-IT.
           MOVE RC-SELECT-PATH TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.

           MOVE RC-BRNO          TO QRC-BRNO.  
           MOVE RC-TRANS-DATE    TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC-TRANS-DATE.
           MOVE RC-STATUS        TO QRC-STATUS.

           EXEC SQL
            SELECT ISNULL(RCFILE.RC_STATUS,''),
                   RCFILE.RC_BRNO,
                   ISNULL(CAST(RCFILE.RC_TRANS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(RCFILE.RC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(RCFILE.RC_RECAP_FG,''),
                   ISNULL(RCFILE.RC_GL_FG,''),
                   ISNULL(RCFILE.RC_EOD_BATCH_FG,''),
                   ISNULL(RCFILE.RC_NIGHTLY_FUNDS_FG,''),
                   ISNULL(RCFILE.RC_EOM1_BATCH_FG,''),
                   ISNULL(RCFILE.RC_EOM_UPDATE_FG,''),
                   ISNULL(RCFILE.RC_EOM2_BATCH_FG,''),
                   ISNULL(RCFILE.RC_BS_P2P3_INBAL_FG,''),
                   RCFILE.RC_DEPOSIT,
                   RCFILE.RC_OVER_SHORT,
                   RCFILE.RC_CHECKS,
                   ISNULL(CAST(RCFILE.RC_OPENED_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_OPENED_TIME,
                   ISNULL(RCFILE.RC_OP_USERID,''),
                   ISNULL(CAST(RCFILE.RC_CLOSED_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_CLOSED_TIME,
                   ISNULL(RCFILE.RC_CL_USERID,''),
                   ISNULL(CAST(RCFILE.RC_UPDATED_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_UPDATED_TIME,
                   ISNULL(CAST(RCFILE.RC_UPDATED_USERID
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(RCFILE.RC_EOD_BATCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_EOD_BATCH_TIME,
                   ISNULL(CAST(RCFILE.RC_FUNDS_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_FUNDS_TIME,
                   ISNULL(CAST(RCFILE.RC_EARNINGS_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_EARNINGS_TIME,
                   ISNULL(CAST(RCFILE.RC_EOM_UPDATED_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_EOM_UPDATED_TIME,
                   ISNULL(CAST(RCFILE.RC_EOM2_BATCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_EOM2_BATCH_TIME,
                   ISNULL(CAST(RCFILE.RC_CHECK_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_CHECK_TIME,
                   RCFILE.RC_LAST_ACCRUE,
                   ISNULL(RCFILE.RC_ERROR_MSG,''),
                   RCFILE.RC_BANK_BAL,
                   RCFILE.RC_CHECK_DEPOSIT
            INTO :QRC-REC
            FROM DBO.RCFILE
            WHERE RCFILE.RC_BRNO = :QRC-BRNO
              AND RCFILE.RC_TRANS_DATE = :QRC-TRANS-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-RC-FIELDS.

      *-----------------------------------------------------------------
       READ-RC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE RC-SELECT-PATH TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.
           INITIALIZE QRC-REC.

           IF ( RC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH RC1_CURSOR INTO :QRC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-RC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-RC1-FILE.
           IF ( RC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE RC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO RC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       LOAD-RC2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( RC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( RC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO RC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO RC-ALL-MACHINE
              MOVE RC-ALL-PATH             TO RC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( RC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO RC-ALL-BASE
              MOVE FILPATH-MACHINE         TO RC-ALL-MACHINE
              MOVE RC-ALL-PATH             TO RC-PATH.

*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*G2G  *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-RCFILE NOT = "S" )
*G2G          MOVE RC-PATH-BEG             TO RC-SELECT-PATH-BEG
*G2G          MOVE RC-PATH-FILE            TO RC-SELECT-PATH-FILE
*G2G       ELSE
*G2G          MOVE RC-PATH-SQL             TO RC-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-RC2-FILE.
           PERFORM LOAD-RC2-FILE.
           PERFORM OPEN-IT.
           MOVE RC-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-RC2-FILE.
           PERFORM START-IT.
           MOVE RC-SELECT-PATH TO E-FILE.
           MOVE RC2-KEY TO E-KEYX.

           IF ( RC2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-RC2-FILE.

           IF ( QRC2-WEND-STATUS = SPACES )
              MOVE RC-STATUS            TO QRC2-WBEG-STATUS
              MOVE ALL "Z"              TO QRC2-WEND-STATUS
              MOVE RC-BRNO              TO QRC2-WBEG-BRNO
              MOVE ALL "9"              TO QRC2-WEND-BRNO

              MOVE RC-TRANS-DATE        TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WBEG-TRANS-DATE

              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WEND-TRANS-DATE.

           EXEC SQL
            DECLARE RC2_CURSOR CURSOR FOR
            SELECT ISNULL(RCFILE.RC_STATUS,''),
                   RCFILE.RC_BRNO,
                   ISNULL(CAST(RCFILE.RC_TRANS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(RCFILE.RC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(RCFILE.RC_RECAP_FG,''),
                   ISNULL(RCFILE.RC_GL_FG,''),
                   ISNULL(RCFILE.RC_EOD_BATCH_FG,''),
                   ISNULL(RCFILE.RC_NIGHTLY_FUNDS_FG,''),
                   ISNULL(RCFILE.RC_EOM1_BATCH_FG,''),
                   ISNULL(RCFILE.RC_EOM_UPDATE_FG,''),
                   ISNULL(RCFILE.RC_EOM2_BATCH_FG,''),
                   ISNULL(RCFILE.RC_BS_P2P3_INBAL_FG,''),
                   RCFILE.RC_DEPOSIT,
                   RCFILE.RC_OVER_SHORT,
                   RCFILE.RC_CHECKS,
                   ISNULL(CAST(RCFILE.RC_OPENED_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_OPENED_TIME,
                   ISNULL(RCFILE.RC_OP_USERID,''),
                   ISNULL(CAST(RCFILE.RC_CLOSED_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_CLOSED_TIME,
                   ISNULL(RCFILE.RC_CL_USERID,''),
                   ISNULL(CAST(RCFILE.RC_UPDATED_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_UPDATED_TIME,
                   ISNULL(CAST(RCFILE.RC_UPDATED_USERID
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(RCFILE.RC_EOD_BATCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_EOD_BATCH_TIME,
                   ISNULL(CAST(RCFILE.RC_FUNDS_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_FUNDS_TIME,
                   ISNULL(CAST(RCFILE.RC_EARNINGS_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_EARNINGS_TIME,
                   ISNULL(CAST(RCFILE.RC_EOM_UPDATED_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_EOM_UPDATED_TIME,
                   ISNULL(CAST(RCFILE.RC_EOM2_BATCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_EOM2_BATCH_TIME,
                   ISNULL(CAST(RCFILE.RC_CHECK_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_CHECK_TIME,
                   RCFILE.RC_LAST_ACCRUE,
                   ISNULL(RCFILE.RC_ERROR_MSG,''),
                   RCFILE.RC_BANK_BAL,
                   RCFILE.RC_CHECK_DEPOSIT
            FROM DBO.RCFILE
            WHERE RCFILE.RC_STATUS >= :QRC2-WBEG-STATUS
              AND RCFILE.RC_STATUS <= :QRC2-WEND-STATUS
              AND RCFILE.RC_BRNO >= :QRC2-WBEG-BRNO
              AND RCFILE.RC_BRNO <= :QRC2-WEND-BRNO
              AND CAST(RCFILE.RC_TRANS_DATE AS VARCHAR(10))
                          >= :QRC2-WBEG-TRANS-DATE
              AND CAST(RCFILE.RC_TRANS_DATE AS VARCHAR(10))
                          <= :QRC2-WEND-TRANS-DATE
            ORDER BY RCFILE.RC_STATUS,
                     RCFILE.RC_BRNO,
                     RCFILE.RC_TRANS_DATE
           END-EXEC.

           EXEC SQL
               OPEN RC2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QRC2-WBEG-STATUS
                          QRC2-WEND-STATUS
                          QRC2-WBEG-TRANS-DATE
                          QRC2-WEND-TRANS-DATE.

           MOVE ZEROES TO QRC2-WBEG-BRNO
                          QRC2-WEND-BRNO.

           MOVE STAT---GOOD TO RC2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-RC2-FILE.
           PERFORM READ-IT.
           MOVE RC-SELECT-PATH TO E-FILE.
           MOVE RC2-KEY TO E-KEYX.

           MOVE RC-STATUS        TO QRC-STATUS.
           MOVE RC-BRNO          TO QRC-BRNO.  
           MOVE RC-TRANS-DATE    TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC-TRANS-DATE.

           EXEC SQL
            SELECT ISNULL(RCFILE.RC_STATUS,''),
                   RCFILE.RC_BRNO,
                   ISNULL(CAST(RCFILE.RC_TRANS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(RCFILE.RC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(RCFILE.RC_RECAP_FG,''),
                   ISNULL(RCFILE.RC_GL_FG,''),
                   ISNULL(RCFILE.RC_EOD_BATCH_FG,''),
                   ISNULL(RCFILE.RC_NIGHTLY_FUNDS_FG,''),
                   ISNULL(RCFILE.RC_EOM1_BATCH_FG,''),
                   ISNULL(RCFILE.RC_EOM_UPDATE_FG,''),
                   ISNULL(RCFILE.RC_EOM2_BATCH_FG,''),
                   ISNULL(RCFILE.RC_BS_P2P3_INBAL_FG,''),
                   RCFILE.RC_DEPOSIT,
                   RCFILE.RC_OVER_SHORT,
                   RCFILE.RC_CHECKS,
                   ISNULL(CAST(RCFILE.RC_OPENED_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_OPENED_TIME,
                   ISNULL(RCFILE.RC_OP_USERID,''),
                   ISNULL(CAST(RCFILE.RC_CLOSED_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_CLOSED_TIME,
                   ISNULL(RCFILE.RC_CL_USERID,''),
                   ISNULL(CAST(RCFILE.RC_UPDATED_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_UPDATED_TIME,
                   ISNULL(CAST(RCFILE.RC_UPDATED_USERID
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(RCFILE.RC_EOD_BATCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_EOD_BATCH_TIME,
                   ISNULL(CAST(RCFILE.RC_FUNDS_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_FUNDS_TIME,
                   ISNULL(CAST(RCFILE.RC_EARNINGS_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_EARNINGS_TIME,
                   ISNULL(CAST(RCFILE.RC_EOM_UPDATED_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_EOM_UPDATED_TIME,
                   ISNULL(CAST(RCFILE.RC_EOM2_BATCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_EOM2_BATCH_TIME,
                   ISNULL(CAST(RCFILE.RC_CHECK_DATE
                                                  AS VARCHAR(10)),'0'),
                   RCFILE.RC_CHECK_TIME,
                   RCFILE.RC_LAST_ACCRUE,
                   ISNULL(RCFILE.RC_ERROR_MSG,''),
                   RCFILE.RC_BANK_BAL,
                   RCFILE.RC_CHECK_DEPOSIT
            INTO :QRC-REC
            FROM DBO.RCFILE
            WHERE RCFILE.RC_STATUS = :QRC-STATUS
              AND RCFILE.RC_BRNO = :QRC-BRNO
              AND RCFILE.RC_TRANS_DATE = :QRC-TRANS-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RC2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-RC-FIELDS.

      *-----------------------------------------------------------------
       READ-RC2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE RC-SELECT-PATH TO E-FILE.
           MOVE RC2-KEY        TO E-KEYX.
           INITIALIZE QRC-REC.

           IF ( RC2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH RC2_CURSOR INTO :QRC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RC2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-RC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-RC2-FILE.
           IF ( RC2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE RC2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO RC2-CURSOR-STAT.

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
              DESTROY IORCRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
