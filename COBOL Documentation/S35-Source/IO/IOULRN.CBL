       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOULRN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  04-21-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOULRN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : ULFILE 
      * TABLE DESC    : USER LOG FILE
      * OUTPUT FILE   : XPC/IOULRN.PRE
      * USED FOR      : LIBGB/GBWSUL.CPY  --- QUL-REC
      *                 LIBGB/GBUL1R.CPY  --- READ
      *                 LIBGB/GBUL1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0421 ***NEW***
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
           "@(#)IO/IOULRN.CBL S35 01/13/22-10:22:57 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01UL.CPY".
       COPY "LIBGB/GBWSUL.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S 01 SQL COPY MEMBERS
      *#################################################################
       01  QUL-DATE-EDIT              PIC 9999/99/99.
       01  QUL-DATE-EDITX             REDEFINES QUL-DATE-EDIT
                                      PIC X(10).

      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QUL1-WHERE-BEG-KEY.
           03  QUL1-WBEG-LOGIN-LETTER    PIC X.
           03  QUL1-WBEG-LOGIN-BRNO      PIC 9(4).
           03  QUL1-WBEG-LOGIN-ID        PIC X(4).
 
       01  QUL1-WHERE-END-KEY.
           03  QUL1-WEND-LOGIN-LETTER    PIC X.
           03  QUL1-WEND-LOGIN-BRNO      PIC 9(4).
           03  QUL1-WEND-LOGIN-ID        PIC X(4).
 
       01  QUL-REC.
           03  QUL-LOGIN-LETTER          PIC X.
           03  QUL-LOGIN-BRNO            PIC 9(4).
           03  QUL-LOGIN-ID              PIC X(4).
           03  QUL-RELEASE               PIC X(4).
           03  QUL-PROGRAM               PIC X(6).
           03  QUL-DATE                  PIC X(8).
           03  QUL-TIME                  PIC X(5).
           03  QUL-POST-BRANCH           PIC 9(4).
           03  QUL-INTERBR               PIC 9(4).
           03  QUL-LTOUCH-DATE           PIC X(10).

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

       01  LEGEND-BEG-LETTER.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING LETTER     (F3-ALL) ".

       01  LEGEND-END-LETTER.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING LETTER     ".

       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING BRANCH NO. (F3-ALL) ".

       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING BRANCH NO. ".

       01  LEGEND-BEG-LOGINID.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING LOGIN ID   (F3-ALL) ".

       01  LEGEND-END-LOGINID.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING LOGIN ID   ".

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
           03  BEG-LETTER              PIC X.
           03  END-LETTER              PIC X.
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  BEG-LOGINID             PIC X(04).
           03  END-LOGINID             PIC X(04).

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
           03  FILLER PIC X(30) VALUE "LETTER BRNO ID                ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-UL-LOGIN-LETTER            PIC X.
           03  FILLER                         PIC X(01).
           03  D01-UL-LOGIN-BRNO              PIC 9(04).
           03  FILLER                         PIC X(01).
           03  D01-UL-LOGIN-ID                PIC X(04).
           03  FILLER                         PIC X(01).
           03  D01-UL-PROGRAM                 PIC X(10).

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
       COPY "LIBIO/IOULRN_DEF.CPY".
       COPY "LIBIO/IOULRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOULRN_SCN.CPY".

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
           MOVE "B"                  TO UL-PATH-OVERRIDE.

           MOVE BEG-LETTER           TO QUL1-WBEG-LOGIN-LETTER.
           MOVE BEG-BRANCH           TO QUL1-WBEG-LOGIN-BRNO.
           MOVE BEG-LOGINID          TO QUL1-WBEG-LOGIN-ID.

           MOVE END-LETTER           TO QUL1-WEND-LOGIN-LETTER.
           MOVE END-BRANCH           TO QUL1-WEND-LOGIN-BRNO.
           MOVE END-LOGINID          TO QUL1-WEND-LOGIN-ID.

           PERFORM OPEN-UL1-FILE.
           PERFORM START-UL1-FILE.

      D    STOP MESS.
      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-UL1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( UL-LOGIN-LETTER < BEG-LETTER )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( UL-LOGIN-LETTER > END-LETTER )
              GO TO MAIN-PROGRAM-END
           ELSE
           IF ( UL-LOGIN-BRNO < BEG-BRANCH ) OR
              ( UL-LOGIN-BRNO > END-BRANCH )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( UL-LOGIN-ID < BEG-LOGINID ) OR
              ( UL-LOGIN-ID > END-LOGINID )
              GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE UL-LOGIN-LETTER            TO D01-UL-LOGIN-LETTER.
           MOVE UL-LOGIN-BRNO              TO D01-UL-LOGIN-BRNO.
           MOVE UL-LOGIN-ID                TO D01-UL-LOGIN-ID.
           MOVE UL-PROGRAM                 TO D01-UL-PROGRAM.
 
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
           MOVE "IOULRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOULRN-QUEUE-POS  TO Q-POS.
           MOVE IOULRN-COPIES-POS TO C-POS.
           MOVE IOULRN-PRU-POS    TO P-POS.
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
       ENTER-BEG-LETTER.

           MOVE LEGEND-BEG-LETTER TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010BEG-LETTER." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-LETTER.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-LETTER.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-LETTER.

      *-----------------------------------------------------------------
       ENTER-END-LETTER.

           MOVE LEGEND-END-LETTER TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010END-LETTER." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LETTER.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-LETTER.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-LETTER.

           IF ( BEG-LETTER > END-LETTER )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LETTER.

           GO TO ENTER-BEG-BRANCH.

      *-----------------------------------------------------------------
       ENTER-ALL-LETTER.

           MOVE "S  A010BEG-LETTER." TO VDU.
           MOVE SPACES               TO BEG-LETTER VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010END-LETTER." TO VDU.
           MOVE ALL "Z"              TO END-LETTER.
           CALL "C$TOLOWER" USING END-LETTER, VALUE 1.
           MOVE END-LETTER           TO VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-LETTER.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-BRANCH.

      *-----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE LEGEND-END-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-BRANCH." TO VDU.
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

           GO TO ENTER-BEG-LOGINID.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE ALL "9"              TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-LOGINID.

           MOVE LEGEND-BEG-LOGINID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040BEG-LOGINID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-LOGINID.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-LOGINID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-LOGINID.

      *-----------------------------------------------------------------
       ENTER-END-LOGINID.

           MOVE LEGEND-END-LOGINID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040END-LOGINID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LOGINID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-LOGINID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-LOGINID.

           IF ( BEG-LOGINID > END-LOGINID )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LOGINID.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-LOGINID.

           MOVE "S  A040BEG-LOGINID." TO VDU.
           MOVE SPACES               TO BEG-LOGINID VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040END-LOGINID." TO VDU.
           MOVE ALL "Z"               TO END-LOGINID.
           CALL "C$TOLOWER" USING END-LOGINID, VALUE 4.
           MOVE END-LETTER            TO VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-LOGINID.
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

           MOVE "S  A010BEG-LETTER."   TO VDU.
           MOVE BEG-LETTER             TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010END-LETTER."   TO VDU.
           MOVE END-LETTER             TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BEG-BRANCH."   TO VDU.
           MOVE BEG-BRANCH             TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH."   TO VDU.
           MOVE END-BRANCH             TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A040BEG-LOGINID."  TO VDU.
           MOVE BEG-LOGINID            TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040END-LOGINID."  TO VDU.
           MOVE END-LOGINID            TO VDUBUF.
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

           MOVE UL1-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING LETTER              : "    TO R01-PROMPT.
           MOVE BEG-LETTER                            TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    LETTER              : "    TO R01-PROMPT.
           MOVE END-LETTER                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING BRANCH              : "    TO R01-PROMPT.
           MOVE BEG-BRANCH                            TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    BRANCH              : "    TO R01-PROMPT.
           MOVE END-BRANCH                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING LOGINID             : "    TO R01-PROMPT.
           MOVE BEG-LOGINID                           TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    LOGINID             : "    TO R01-PROMPT.
           MOVE END-LOGINID                           TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - ULFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOULRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOULRN-SYSDATE.
           DISPLAY IOULRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOULRN-FORM-FIELDS TO WR-BUF.
           MOVE IOULRN-FORM-LIST TO WR-LIST.
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

           MOVE IOULRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOULRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOULRN_EVA.CPY".
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
      *    LOAD UL-REC FROM QUL-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-UL-FIELDS.
           MOVE QUL-LOGIN-LETTER TO UL-LOGIN-LETTER.
           MOVE QUL-LOGIN-BRNO TO UL-LOGIN-BRNO.
           MOVE QUL-LOGIN-ID TO UL-LOGIN-ID.
           MOVE QUL-RELEASE TO UL-RELEASE.
           MOVE QUL-PROGRAM TO UL-PROGRAM.
           MOVE QUL-DATE TO UL-DATE.
           MOVE QUL-TIME TO UL-TIME.
           MOVE QUL-POST-BRANCH TO UL-POST-BRANCH.
           MOVE QUL-INTERBR TO UL-INTERBR.

           IF ( QUL-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO UL-LTOUCH-DATE
           ELSE
              MOVE QUL-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO UL-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QUL-REC FROM UL-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-UL-FIELDS.
           MOVE UL-LOGIN-LETTER TO QUL-LOGIN-LETTER.
           MOVE UL-LOGIN-BRNO TO QUL-LOGIN-BRNO.
           MOVE UL-LOGIN-ID TO QUL-LOGIN-ID.
           MOVE UL-RELEASE TO QUL-RELEASE.
           MOVE UL-PROGRAM TO QUL-PROGRAM.
           MOVE UL-DATE TO QUL-DATE.
           MOVE UL-TIME TO QUL-TIME.
           MOVE UL-POST-BRANCH TO QUL-POST-BRANCH.
           MOVE UL-INTERBR TO QUL-INTERBR.

           IF ( UL-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QUL-LTOUCH-DATE
           ELSE
              MOVE UL-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QUL-LTOUCH-DATE.

 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBGB/GBUL1RN
       LOAD-UL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( UL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( UL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO UL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO UL-ALL-MACHINE
              MOVE UL-ALL-PATH             TO UL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( UL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO UL-ALL-BASE
              MOVE FILPATH-MACHINE         TO UL-ALL-MACHINE
              MOVE UL-ALL-PATH             TO UL-PATH.
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*G2G  *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-ULFILE NOT = "S" )
*G2G          MOVE UL-PATH-BEG             TO UL-SELECT-PATH-BEG
*G2G          MOVE UL-PATH-FILE            TO UL-SELECT-PATH-FILE
*G2G       ELSE
*G2G          MOVE UL-PATH-SQL             TO UL-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-UL1-FILE.
           PERFORM LOAD-UL1-FILE.
           PERFORM OPEN-IT.
           MOVE UL-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-UL1-FILE.
           PERFORM START-IT.
           MOVE UL-SELECT-PATH TO E-FILE.
           MOVE UL1-KEY TO E-KEYX.

           IF ( UL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-UL1-FILE.

           IF ( QUL1-WEND-LOGIN-LETTER = SPACES )
              MOVE UL-LOGIN-LETTER      TO QUL1-WBEG-LOGIN-LETTER
              MOVE ALL "Z"              TO QUL1-WEND-LOGIN-LETTER
              CALL "C$TOLOWER" USING QUL1-WEND-LOGIN-LETTER, VALUE 1
              MOVE UL-LOGIN-BRNO        TO QUL1-WBEG-LOGIN-BRNO
              MOVE ALL "9"              TO QUL1-WEND-LOGIN-BRNO
              MOVE UL-LOGIN-ID          TO QUL1-WBEG-LOGIN-ID
              MOVE ALL "Z"              TO QUL1-WEND-LOGIN-ID
              CALL "C$TOLOWER" USING QUL1-WEND-LOGIN-ID, VALUE 4.

           EXEC SQL
            DECLARE UL1_CURSOR CURSOR FOR
            SELECT ISNULL(ULFILE.UL_LOGIN_LETTER,''),
                   ULFILE.UL_LOGIN_BRNO,
                   ISNULL(ULFILE.UL_LOGIN_ID,''),
                   ISNULL(ULFILE.UL_RELEASE,''),
                   ISNULL(ULFILE.UL_PROGRAM,''),
                   ISNULL(ULFILE.UL_DATE,''),
                   ISNULL(ULFILE.UL_TIME,''),
                   ULFILE.UL_POST_BRANCH,
                   ULFILE.UL_INTERBR,
                   ISNULL(CAST(ULFILE.UL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            FROM DBO.ULFILE
            WHERE CONCAT (ULFILE.UL_LOGIN_LETTER,
                          ULFILE.UL_LOGIN_BRNO,
                          ULFILE.UL_LOGIN_ID)
                     >= CONCAT (:QUL1-WBEG-LOGIN-LETTER,
                                :QUL1-WBEG-LOGIN-BRNO,
                                :QUL1-WBEG-LOGIN-ID)
      *       AND CONCAT (ULFILE.UL_LOGIN_LETTER,
      *                   ULFILE.UL_LOGIN_BRNO,
      *                   ULFILE.UL_LOGIN_ID)
      *              <= CONCAT (:QUL1-WEND-LOGIN-LETTER,
      *                         :QUL1-WEND-LOGIN-BRNO,
      *                         :QUL1-WEND-LOGIN-ID)
            ORDER BY ULFILE.UL_LOGIN_LETTER,
                     ULFILE.UL_LOGIN_BRNO,
                     ULFILE.UL_LOGIN_ID
           END-EXEC.

           EXEC SQL
               OPEN UL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QUL1-WBEG-LOGIN-LETTER
                          QUL1-WEND-LOGIN-LETTER
                          QUL1-WBEG-LOGIN-ID
                          QUL1-WEND-LOGIN-ID.

           MOVE ZEROES TO QUL1-WBEG-LOGIN-BRNO
                          QUL1-WEND-LOGIN-BRNO.

           MOVE STAT---GOOD TO UL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-UL1-FILE.
           PERFORM READ-IT.
           MOVE UL-SELECT-PATH TO E-FILE.
           MOVE UL1-KEY TO E-KEYX.

           MOVE UL-LOGIN-LETTER  TO QUL-LOGIN-LETTER.
           MOVE UL-LOGIN-BRNO    TO QUL-LOGIN-BRNO.
           MOVE UL-LOGIN-ID      TO QUL-LOGIN-ID.

           EXEC SQL
            SELECT ISNULL(ULFILE.UL_LOGIN_LETTER,''),
                   ULFILE.UL_LOGIN_BRNO,
                   ISNULL(ULFILE.UL_LOGIN_ID,''),
                   ISNULL(ULFILE.UL_RELEASE,''),
                   ISNULL(ULFILE.UL_PROGRAM,''),
                   ISNULL(ULFILE.UL_DATE,''),
                   ISNULL(ULFILE.UL_TIME,''),
                   ULFILE.UL_POST_BRANCH,
                   ULFILE.UL_INTERBR,
                   ISNULL(CAST(ULFILE.UL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            INTO :QUL-REC
            FROM DBO.ULFILE
            WHERE ULFILE.UL_LOGIN_LETTER = :QUL-LOGIN-LETTER
              AND ULFILE.UL_LOGIN_BRNO = :QUL-LOGIN-BRNO
              AND ULFILE.UL_LOGIN_ID = :QUL-LOGIN-ID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-UL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-UL-FIELDS.

      *-----------------------------------------------------------------
       READ-UL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE UL-SELECT-PATH TO E-FILE.
           MOVE UL1-KEY TO E-KEYX.
           INITIALIZE QUL-REC.

           IF ( UL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH UL1_CURSOR INTO :QUL-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-UL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-UL-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-UL1-FILE.
           IF ( UL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE UL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO UL1-CURSOR-STAT.

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
              DESTROY IOULRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
