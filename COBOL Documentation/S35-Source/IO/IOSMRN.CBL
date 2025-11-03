       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOSMRN.
       AUTHOR.        BAH
       DATE-WRITTEN.  03-15-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOSMRN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : SMFILE 
      * TABLE DESC    : STATE PARAMETER MEMO FILE
      * OUTPUT FILE   : XPC/IOSMRN.PRE
      * USED FOR      : LIBLP/LPWSSM.CPY  --- QSM-REC
      *                 LIBLP/LPSM1R.CPY  --- READ
      *                 LIBLP/LPSM1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0315 ***NEW***
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
           "@(#)IO/IOSMRN.CBL S34 05/18/21-11:18:11 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01SM.CPY".
       COPY "LIBLP/LPWSSM.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S 01 SQL COPY MEMBERS
      *#################################################################
       01  QSM-DATE-EDIT              PIC 9999/99/99.
       01  QSM-DATE-EDITX             REDEFINES QSM-DATE-EDIT
                                      PIC X(10).

      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QSM1-WHERE-BEG-KEY.
           03  QSM1-WBEG-ORGST           PIC X(02).
           03  QSM1-WBEG-SPRCLASS        PIC 9(03).
           03  QSM1-WBEG-SUBCLASS        PIC 9(02).
           03  QSM1-WBEG-LAWCODE         PIC 9(02).
           03  QSM1-WBEG-DATE-REVERSED   PIC 9(8).
           03  QSM1-WBEG-SEQ             PIC 9(03).
 
       01  QSM1-WHERE-END-KEY.
           03  QSM1-WEND-ORGST           PIC X(02).
           03  QSM1-WEND-SPRCLASS        PIC 9(03).
           03  QSM1-WEND-SUBCLASS        PIC 9(02).
           03  QSM1-WEND-LAWCODE         PIC 9(02).
           03  QSM1-WEND-DATE-REVERSED   PIC 9(8).
           03  QSM1-WEND-SEQ             PIC 9(03).
 
       01  QSM-REC.
           03  QSM-SP-ORGST        PIC X(02).
           03  QSM-SP-SPRCLASS     PIC 9(03).
           03  QSM-SP-SUBCLASS     PIC 9(02).
           03  QSM-SP-LAWCODE      PIC 9(02).
           03  QSM-DATE-REVERSED  PIC 9(08).
           03  QSM-SEQ            PIC 9(03).
           03  QSM-LTOUCH-DATE            PIC X(10).
           03  QSM-LTOUCH-TIME-HHMM       PIC 9(4).
           03  QSM-LTOUCH-USERID          PIC X(10).
           03  QSM-MEMO1                  PIC X(30).
           03  QSM-MEMO2                  PIC X(30).
           03  QSM-MEMO-TYPE              PIC XX.
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

       01  LEGEND-BEG-ORGST. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING STATE      (F3-ALL) ".

       01  LEGEND-END-ORGST. 
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING STATE      ".

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
           03  BEG-ORGST               PIC X(02).
           03  END-ORGST               PIC X(02).
           03  BEG-SPRCLASS            PIC 9(03).
           03  END-SPRCLASS            PIC 9(03).
           03  BEG-SUBCLASS            PIC 9(02).
           03  END-SUBCLASS            PIC 9(02).
           03  BEG-LAWCODE             PIC 9(02).
           03  END-LAWCODE             PIC 9(02).
           03  BEG-DATE                PIC 9(08).
           03  END-DATE                PIC 9(08).
           03  BEG-SEQ                 PIC 9(03).
           03  END-SEQ                 PIC 9(03).

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
           03  FILLER PIC X(32) VALUE "ST  SPR  SUB LAW  DATE     SEQ".
           03  FILLER PIC X(30) VALUE "MEMO".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-SM-SP-ORGST                   PIC XX.
           03  FILLER                         PIC X(02).
           03  D01-SM-SP-SPRCLASS                PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-SM-SP-SUBCLASS                PIC 9(02).
           03  FILLER                         PIC X(02).
           03  D01-SM-SP-LAWCODE                 PIC 9(02).
           03  FILLER                         PIC X(02).
           03  D01-SM-REAL-DATE               PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-SM-SEQ                     PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-SM-MEMO                    PIC X(60).
           03  FILLER                         PIC X(02).

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
       COPY "LIBIO/IOSMRN_DEF.CPY".
       COPY "LIBIO/IOSMRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOSMRN_SCN.CPY".

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
           MOVE "B"                  TO SM-PATH-OVERRIDE.

           MOVE BEG-ORGST            TO QSM1-WBEG-ORGST.
           MOVE BEG-SPRCLASS         TO QSM1-WBEG-SPRCLASS.
           MOVE BEG-SUBCLASS         TO QSM1-WBEG-SUBCLASS.
           MOVE BEG-LAWCODE          TO QSM1-WBEG-LAWCODE.
           COMPUTE QSM1-WBEG-DATE-REVERSED = 
                                     99999999 - BEG-DATE.
           MOVE BEG-SEQ              TO QSM1-WBEG-SEQ.

           MOVE END-ORGST            TO QSM1-WEND-ORGST.
           MOVE END-SPRCLASS         TO QSM1-WEND-SPRCLASS.
           MOVE END-SUBCLASS         TO QSM1-WEND-SUBCLASS.
           MOVE END-LAWCODE          TO QSM1-WEND-LAWCODE.
           COMPUTE QSM1-WEND-DATE-REVERSED = 
                                     99999999 - END-DATE.
           MOVE END-SEQ              TO QSM1-WEND-SEQ.

           PERFORM OPEN-SM1-FILE.
      D    STOP MESS.
           PERFORM START-SM1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-SM1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SM-SP-ORGST < BEG-ORGST )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( SM-SP-ORGST > END-ORGST )
              GO TO MAIN-PROGRAM-END.

           IF SM-SP-SPRCLASS < BEG-SPRCLASS OR > END-SPRCLASS
              GO TO MAIN-PROGRAM-NEXT.
           IF SM-SP-SUBCLASS < BEG-SUBCLASS OR > END-SUBCLASS
              GO TO MAIN-PROGRAM-NEXT.
           IF SM-SP-LAWCODE < BEG-LAWCODE OR > END-LAWCODE
              GO TO MAIN-PROGRAM-NEXT.
           IF SM-SEQ < BEG-SEQ OR > END-SEQ
              GO TO MAIN-PROGRAM-NEXT.

           COMPUTE DATE-YYYYMMDD = 99999999 - SM-DATE-REVERSED.
           IF DATE-YYYYMMDD < BEG-DATE OR > END-DATE
              GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE SM-SP-ORGST                   TO D01-SM-SP-ORGST.
           MOVE SM-SP-SPRCLASS                TO D01-SM-SP-SPRCLASS.
           MOVE SM-SP-SUBCLASS                TO D01-SM-SP-SUBCLASS.
           MOVE SM-SP-LAWCODE                 TO D01-SM-SP-LAWCODE.
           COMPUTE D01-SM-REAL-DATE = 99999999 - SM-DATE.
           MOVE SM-SEQ                     TO D01-SM-SEQ.
           MOVE SM-MEMO                    TO D01-SM-MEMO.
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
           MOVE "IOSMRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOSMRN-QUEUE-POS  TO Q-POS.
           MOVE IOSMRN-COPIES-POS TO C-POS.
           MOVE IOSMRN-PRU-POS    TO P-POS.
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
       ENTER-BEG-ORGST.

           MOVE LEGEND-BEG-ORGST TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A020BEG-ORGST." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-STATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ORGST.

           MOVE VDUBUF TO BEG-ORGST.

      *-----------------------------------------------------------------
       ENTER-END-ORGST.

           MOVE LEGEND-END-ORGST TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A020END-ORGST." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ORGST.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ORGST.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-ORGST.

           IF ( BEG-ORGST > END-ORGST )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ORGST.

           GO TO ENTER-BEG-SPRCLASS.

      *-----------------------------------------------------------------
       ENTER-ALL-STATE.

           MOVE "S  A020BEG-ORGST." TO VDU.
           MOVE SPACES               TO BEG-ORGST VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020END-ORGST." TO VDU.
           MOVE ALL "Z"              TO END-ORGST VDUBUF.
           PERFORM SCREENIO.

       ENTER-BEG-SPRCLASS.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "EN N030BEG-SPRCLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ORGST.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SPRCLASS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SPRCLASS.

           MOVE VDUNUM0 TO BEG-SPRCLASS.

      *-----------------------------------------------------------------
       ENTER-END-SPRCLASS.

           MOVE "EN N030END-SPRCLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SPRCLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SPRCLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SPRCLASS.

           IF ( BEG-SPRCLASS > END-SPRCLASS )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SPRCLASS.

           GO TO ENTER-BEG-SUBCLASS.

      *-----------------------------------------------------------------
       ENTER-ALL-SPRCLASS.

           MOVE "SN N030BEG-SPRCLASS." TO VDU.
           MOVE 0                      TO BEG-SPRCLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N030END-SPRCLASS." TO VDU.
           MOVE 999                    TO END-SPRCLASS VDUNUM0.
           PERFORM SCREENIO.

       ENTER-BEG-SUBCLASS.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "EN N020BEG-SUBCLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SPRCLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SUBCLASS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SUBCLASS.

           MOVE VDUNUM0 TO BEG-SUBCLASS.

      *-----------------------------------------------------------------
       ENTER-END-SUBCLASS.

           MOVE "EN N020END-SUBCLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SUBCLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SUBCLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SUBCLASS.

           IF ( BEG-SUBCLASS > END-SUBCLASS )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SUBCLASS.

           GO TO ENTER-BEG-LAWCODE.

      *-----------------------------------------------------------------
       ENTER-ALL-SUBCLASS.

           MOVE "SN N020BEG-SUBCLASS." TO VDU.
           MOVE 0                      TO BEG-SUBCLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N020END-SUBCLASS." TO VDU.
           MOVE 99                     TO END-SUBCLASS VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-BEG-LAWCODE.

           MOVE "EN N020BEG-LAWCODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SUBCLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-LAWCODE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-LAWCODE.

           MOVE VDUNUM0 TO BEG-LAWCODE.

      *-----------------------------------------------------------------
       ENTER-END-LAWCODE.

           MOVE "EN N020END-LAWCODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LAWCODE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-LAWCODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-LAWCODE.

           IF ( BEG-LAWCODE > END-LAWCODE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LAWCODE.

           GO TO ENTER-BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-ALL-LAWCODE.

           MOVE "SN N020BEG-LAWCODE." TO VDU.
           MOVE 0                      TO BEG-LAWCODE VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N020END-LAWCODE." TO VDU.
           MOVE 99                     TO END-LAWCODE VDUNUM0.
           PERFORM SCREENIO.

       ENTER-BEG-DATE.
           MOVE "ERNM060BEG-DATE." TO VDU.
           PERFORM SCREENIO.
           IF (        UP-KEY ) GO TO ENTER-END-LAWCODE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATES.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

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

           GO TO ENTER-BEG-SEQ.
       ENTER-ALL-DATES.
           MOVE "S  8080BEG-DATE." TO VDU.
           MOVE JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080END-DATE." TO VDU.
           MOVE JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       ENTER-BEG-SEQ.

           MOVE "EN N030BEG-SEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SEQ.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SEQ.

           MOVE VDUNUM0 TO BEG-SEQ.

      *-----------------------------------------------------------------
       ENTER-END-SEQ.

           MOVE "EN N030END-SEQ." TO VDU.
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

           MOVE "SN N030BEG-SEQ." TO VDU.
           MOVE 0                      TO BEG-SEQ VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N030END-SEQ." TO VDU.
           MOVE 999                    TO END-SEQ VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-LAWCODE.
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

           MOVE "S  A020BEG-ORGST."    TO VDU.
           MOVE BEG-ORGST              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-ORGST."    TO VDU.
           MOVE END-ORGST              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A030BEG-SPRCLASS."    TO VDU.
           MOVE BEG-SPRCLASS              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A030END-SPRCLASS."    TO VDU.
           MOVE END-SPRCLASS              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A020BEG-SUBCLASS."    TO VDU.
           MOVE BEG-SUBCLASS              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A020END-SUBCLASS."    TO VDU.
           MOVE END-SUBCLASS              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A020BEG-LAWCODE."    TO VDU.
           MOVE BEG-LAWCODE              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A020END-LAWCODE."    TO VDU.
           MOVE END-LAWCODE              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  8080BEG-DATE." TO VDU.
           MOVE BEG-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE." TO VDU.
           MOVE END-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  A020BEG-SEQ."    TO VDU.
           MOVE BEG-SEQ              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A020END-SEQ."    TO VDU.
           MOVE END-SEQ              TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

           MOVE SM1-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING ORGST               : "    TO R01-PROMPT.
           MOVE BEG-ORGST                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ORGST               : "    TO R01-PROMPT.
           MOVE END-ORGST                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SPRCLASS               : "    TO R01-PROMPT.
           MOVE BEG-SPRCLASS                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SPRCLASS               : "    TO R01-PROMPT.
           MOVE END-SPRCLASS                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SUBCLASS               : "    TO R01-PROMPT.
           MOVE BEG-SUBCLASS                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SUBCLASS               : "    TO R01-PROMPT.
           MOVE END-SUBCLASS                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING LAWCODE               : "    TO R01-PROMPT.
           MOVE BEG-LAWCODE                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    LAWCODE               : "    TO R01-PROMPT.
           MOVE END-LAWCODE                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
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
           MOVE "STARTING TIME                 : "    TO R01-PROMPT.
           MOVE STARTING-TIME                         TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME                 : "    TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

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
           MOVE "SQL TABLE IO UTILITY - SMFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOSMRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOSMRN-SYSDATE.
           DISPLAY IOSMRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOSMRN-FORM-FIELDS TO WR-BUF.
           MOVE IOSMRN-FORM-LIST TO WR-LIST.
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

           MOVE IOSMRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOSMRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOSMRN_EVA.CPY".
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
      *    LOAD SM-REC FROM QSM-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-SM-FIELDS.
           MOVE QSM-SP-ORGST TO SM-SP-ORGST.
           MOVE QSM-SP-SPRCLASS TO SM-SP-SPRCLASS.
           MOVE QSM-SP-SUBCLASS TO SM-SP-SUBCLASS.
           MOVE QSM-SP-LAWCODE TO SM-SP-LAWCODE.

           MOVE QSM-DATE-REVERSED TO SM-DATE-REVERSED.
           MOVE QSM-SEQ TO SM-SEQ.

           IF ( QSM-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO SM-LTOUCH-DATE
           ELSE
              MOVE QSM-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO SM-LTOUCH-DATE.

           MOVE QSM-LTOUCH-TIME-HHMM TO SM-LTOUCH-TIME-HHMM.
           MOVE QSM-LTOUCH-USERID TO SM-LTOUCH-USERID.
           MOVE QSM-MEMO1 TO SM-MEMO1.
           MOVE QSM-MEMO2 TO SM-MEMO2.
           MOVE QSM-MEMO-TYPE TO SM-MEMO-TYPE.
 
      *-----------------------------------------------------------------
      *    LOAD QSM-REC FROM SM-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-SM-FIELDS.
           MOVE SM-SP-ORGST TO QSM-SP-ORGST.
           MOVE SM-SP-SPRCLASS TO QSM-SP-SPRCLASS.
           MOVE SM-SP-SUBCLASS TO QSM-SP-SUBCLASS.
           MOVE SM-SP-LAWCODE TO QSM-SP-LAWCODE.

           MOVE SM-DATE-REVERSED TO QSM-DATE-REVERSED.
           MOVE SM-SEQ TO QSM-SEQ.

           IF ( SM-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QSM-LTOUCH-DATE
           ELSE
              MOVE SM-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QSM-LTOUCH-DATE.

           MOVE SM-LTOUCH-TIME-HHMM TO QSM-LTOUCH-TIME-HHMM.
           MOVE SM-LTOUCH-USERID TO QSM-LTOUCH-USERID.
           MOVE SM-MEMO1 TO QSM-MEMO1.
           MOVE SM-MEMO2 TO QSM-MEMO2.
           MOVE SM-MEMO-TYPE TO QSM-MEMO-TYPE.
 

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBGB/GBSM1RN
       LOAD-SM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SM-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SM-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SM-ALL-MACHINE
              MOVE SM-ALL-PATH             TO SM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SM-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SM-ALL-BASE
              MOVE FILPATH-MACHINE         TO SM-ALL-MACHINE
              MOVE SM-ALL-PATH             TO SM-PATH.
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*G2G  *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-SMFILE NOT = "S" )
*G2G          MOVE SM-PATH-BEG             TO SM-SELECT-PATH-BEG
*G2G          MOVE SM-PATH-FILE            TO SM-SELECT-PATH-FILE
*G2G       ELSE
*G2G          MOVE SM-PATH-SQL             TO SM-SELECT-PATH.


      *-----------------------------------------------------------------
       OPEN-SM1-FILE.
           PERFORM LOAD-SM1-FILE.
           PERFORM OPEN-IT.
           MOVE SM-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-SM1-FILE.
           PERFORM START-IT.
           MOVE SM-SELECT-PATH TO E-FILE.
           MOVE SM1-KEY TO E-KEYX.

           IF ( SM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SM1-FILE.

           IF ( QSM1-WEND-ORGST = SPACES  ) 
              MOVE SM-SP-ORGST          TO QSM1-WBEG-ORGST
              MOVE ALL "Z"              TO QSM1-WEND-ORGST
              MOVE SM-SP-SPRCLASS       TO QSM1-WBEG-SPRCLASS
              MOVE ALL "9"              TO QSM1-WEND-SPRCLASS
              MOVE SM-SP-SUBCLASS       TO QSM1-WBEG-SUBCLASS
              MOVE ALL "9"              TO QSM1-WEND-SUBCLASS
              MOVE SM-SP-LAWCODE        TO QSM1-WBEG-LAWCODE
              MOVE ALL "9"              TO QSM1-WEND-LAWCODE
              MOVE SM-DATE-REVERSED     TO QSM1-WBEG-DATE-REVERSED
              MOVE JULIAN-END           TO QSM1-WEND-DATE-REVERSED
              MOVE SM-SEQ               TO QSM1-WBEG-SEQ
              MOVE ALL "9"              TO QSM1-WEND-SEQ.

           EXEC SQL
            DECLARE SM1_CURSOR CURSOR FOR
            SELECT ISNULL(SMFILE.SM_SP_ORGST,''),
                   SMFILE.SM_SP_SPRCLASS,
                   SMFILE.SM_SP_SUBCLASS,
                   SMFILE.SM_SP_LAWCODE,
                   SMFILE.SM_DATE_REVERSED,
                   SMFILE.SM_SEQ,
                   ISNULL(CAST(SMFILE.SM_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   SMFILE.SM_LTOUCH_TIME_HHMM,
                   ISNULL(SMFILE.SM_LTOUCH_USERID,''),
                   ISNULL(SMFILE.SM_MEMO1,''),
                   ISNULL(SMFILE.SM_MEMO2,''),
                   ISNULL(SMFILE.SM_MEMO_TYPE,'')
            FROM DBO.SMFILE
            WHERE SMFILE.SM_SP_ORGST >= :QSM1-WBEG-ORGST
              AND SMFILE.SM_SP_ORGST <= :QSM1-WEND-ORGST
              AND SMFILE.SM_SP_SPRCLASS >= :QSM1-WBEG-SPRCLASS
              AND SMFILE.SM_SP_SPRCLASS <= :QSM1-WEND-SPRCLASS
              AND SMFILE.SM_SP_SUBCLASS >= :QSM1-WBEG-SUBCLASS
              AND SMFILE.SM_SP_SUBCLASS <= :QSM1-WEND-SUBCLASS
              AND SMFILE.SM_SP_LAWCODE >= :QSM1-WBEG-LAWCODE
              AND SMFILE.SM_SP_LAWCODE <= :QSM1-WEND-LAWCODE
              AND ( CAST(SMFILE.SM_DATE_REVERSED AS CHAR(08)) +
                    CAST (SMFILE.SM_SEQ AS CHAR(03)) )
                  >=
                  ( CAST(:QSM1-WBEG-DATE-REVERSED AS CHAR(08)) +
                    CAST (:QSM1-WBEG-SEQ AS CHAR(03)) )
              AND ( CAST(SMFILE.SM_DATE_REVERSED AS CHAR(08)) +
                    CAST (SMFILE.SM_SEQ AS CHAR(03)) )
                  <=
                  ( CAST(:QSM1-WEND-DATE-REVERSED AS CHAR(08)) +
                    CAST (:QSM1-WEND-SEQ AS CHAR(03)) )

      * THE ABOVE WORKS, GETS PAGE UP AND PAGE DOWN CORRECTLY
      * THE CONCAT REPEATS SOME OF THE MEMOS ON PAGE DOWN
      *       AND CONCAT (SMFILE.SM_DATE_REVERSED,
      *                   SMFILE.SM_SEQ)
      *           >=
      *           CONCAT (:QSM1-WBEG-DATE-REVERSED,
      *                   :QSM1-WBEG-SEQ)
      *       AND CONCAT (SMFILE.SM_DATE_REVERSED,
      *                   SMFILE.SM_SEQ)
      *           <=
      *           CONCAT (:QSM1-WEND-DATE-REVERSED,
      *                   :QSM1-WEND-SEQ)
            ORDER BY SMFILE.SM_SP_ORGST,
                     SMFILE.SM_SP_SPRCLASS,
                     SMFILE.SM_SP_SUBCLASS,
                     SMFILE.SM_SP_LAWCODE,
                     SMFILE.SM_DATE_REVERSED,
                     SMFILE.SM_SEQ
           END-EXEC.

           EXEC SQL
               OPEN SM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QSM1-WBEG-ORGST
                          QSM1-WEND-ORGST.

           MOVE ZEROES TO QSM1-WBEG-SPRCLASS
                          QSM1-WEND-SPRCLASS
                          QSM1-WBEG-SUBCLASS
                          QSM1-WEND-SUBCLASS
                          QSM1-WBEG-LAWCODE
                          QSM1-WEND-LAWCODE
                          QSM1-WBEG-DATE-REVERSED
                          QSM1-WEND-DATE-REVERSED
                          QSM1-WBEG-SEQ
                          QSM1-WEND-SEQ.

           MOVE STAT---GOOD TO SM1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SM1-FILE.
           PERFORM READ-IT.
           MOVE SM-SELECT-PATH TO E-FILE.
           MOVE SM1-KEY TO E-KEYX.

           MOVE SM-SP-ORGST            TO QSM-SP-ORGST.  
           MOVE SM-SP-SPRCLASS         TO QSM-SP-SPRCLASS.  
           MOVE SM-SP-SUBCLASS         TO QSM-SP-SUBCLASS.  
           MOVE SM-SP-LAWCODE          TO QSM-SP-LAWCODE.  
           MOVE SM-DATE-REVERSED       TO QSM-DATE-REVERSED.  
           MOVE SM-SEQ                 TO QSM-SEQ.  

           EXEC SQL
            SELECT ISNULL(SMFILE.SM_SP_ORGST,''),
                   SMFILE.SM_SP_SPRCLASS,
                   SMFILE.SM_SP_SUBCLASS,
                   SMFILE.SM_SP_LAWCODE,
                   SMFILE.SM_DATE_REVERSED,
                   SMFILE.SM_SEQ,
                   ISNULL(CAST(SMFILE.SM_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   SMFILE.SM_LTOUCH_TIME_HHMM,
                   ISNULL(SMFILE.SM_LTOUCH_USERID,''),
                   ISNULL(SMFILE.SM_MEMO1,''),
                   ISNULL(SMFILE.SM_MEMO2,''),
                   ISNULL(SMFILE.SM_MEMO_TYPE,'')
            INTO :QSM-REC
            FROM DBO.SMFILE
            WHERE SMFILE.SM_SP_ORGST = :QSM-SP-ORGST
              AND SMFILE.SM_SP_SPRCLASS = :QSM-SP-SPRCLASS
              AND SMFILE.SM_SP_SUBCLASS = :QSM-SP-SUBCLASS
              AND SMFILE.SM_SP_LAWCODE = :QSM-SP-LAWCODE
              AND SMFILE.SM_DATE_REVERSED = :QSM-DATE-REVERSED
              AND SMFILE.SM_SEQ = :QSM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SM1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SM-FIELDS.

      *-----------------------------------------------------------------
       READ-SM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SM-SELECT-PATH TO E-FILE.
           MOVE SM1-KEY TO E-KEYX.
           INITIALIZE QSM-REC.

           IF ( SM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SM1_CURSOR INTO :QSM-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-SM-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SM1-FILE.
           IF ( SM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SM1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SM1-CURSOR-STAT.

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
              DESTROY IOSMRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
