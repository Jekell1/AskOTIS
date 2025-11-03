       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOMLRN.
       AUTHOR.        BAH.
       DATE-WRITTEN.  04-14-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOMLRN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : MLFILE 
      * TABLE DESC    : MAIL LIST FILE
      * OUTPUT FILE   : XPC/IOMLRN.PRE
      * USED FOR      : LIBLP/LPWSML.CPY  --- QML-REC
      *                 LIBLP/LPML1R.CPY  --- READ
      *                 LIBLP/LPML1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0414 ***NEW***
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
           "@(#)IO/IOMLRN.CBL S34 05/10/21-14:24:45 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".

      *=================================================================
       COPY "LIBLP/LP01ML.CPY".
       COPY "LIBLP/LPWSML.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QML1-WHERE-BEG-KEY.
           03  QML1-WBEG-OWNBR             PIC 9(4).
           03  QML1-WBEG-SSNO             PIC 9(9).
       01  QML1-WHERE-END-KEY.
           03  QML1-WEND-OWNBR             PIC 9(4).
           03  QML1-WEND-SSNO             PIC 9(9).

       01  QML2-WHERE-BEG-KEY.
           03  QML2-WBEG-OWNBR             PIC 9(4).
           03  QML2-WBEG-LNAME            PIC X(35).
           03  QML2-WBEG-FNAME            PIC X(35).
       01  QML2-WHERE-END-KEY.
           03  QML2-WEND-OWNBR             PIC 9(4).
           03  QML2-WEND-LNAME            PIC X(35).
           03  QML2-WEND-FNAME            PIC X(35).

       01  QML3-WHERE-BEG-KEY.
           03  QML3-WBEG-SSNO              PIC 9(9).
       01  QML3-WHERE-END-KEY.
           03  QML3-WEND-SSNO              PIC 9(9).

       01  QML-REC.
           03  QML-OWNBR            PIC 9(4).
           03  QML-SSNO             PIC 9(9).
           03  QML-LNAME            PIC X(30).
           03  QML-FNAME            PIC X(30).
           03  QML-ADR1                 PIC X(24).
           03  QML-ADR2                 PIC X(24).
           03  QML-CITY                 PIC X(15).
           03  QML-STATE                PIC X(2).
           03  QML-ZIP                  PIC X(10).
           03  QML-REFDATE              PIC X(10).
           03  QML-SRCD                 PIC X(2).
           03  QML-SOLICIT              PIC X.
           03  QML-CONFIDENTIAL         PIC X.
           03  QML-LAST-FORM-NO         PIC 9(4).
           03  QML-LAST-FORM-DATE       PIC X(10).
           03  QML-LTOUCH-DATE          PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
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
           03  FILLER PIC X(30) VALUE "BEGINNING BRANCH NO. (F3-ALL) ".

       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING BRANCH NO. ".

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
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  GROUP-FLAG              PIC X(01).
           03  ENT-GROUP               PIC X(04).

           03  PROCESS-KEY             PIC 9(01).
           03  BEG-SSNO                PIC 9(09).
           03  END-SSNO                PIC 9(09).
           03  BEG-LNAME               PIC X(30).
           03  END-LNAME               PIC X(30).
           03  BEG-FNAME               PIC X(30).
           03  END-FNAME               PIC X(30).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

       01  SAVE-SCREEN-RANGES.
           03  SV-BEG-BRANCH           PIC 9(04)     VALUE 9999.
           03  SV-END-BRANCH           PIC 9(04)     VALUE 9999.

           03  SV-GROUP-OPTION.
               05  SV-GROUP-FLAG       PIC X(01).
               05  SV-ENT-GROUP        PIC X(04).

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
           03  FILLER PIC X(43) VALUE "BRNO     SSNO       LNAME".
           03  FILLER PIC X(30) VALUE "FNAME                  CITY".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-ML-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-ML-SSNO                    PIC 999/99/9999.
           03  FILLER                         PIC X(03).
           03  D01-ML-LNAME                   PIC X(20).
           03  FILLER                         PIC X(03).
           03  D01-ML-FNAME                   PIC X(20).
           03  FILLER                         PIC X(03).
           03  D01-ML-CITY                   PIC X(20).

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
      *COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
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
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBIO/IOMLRN_DEF.CPY".
       COPY "LIBIO/IOMLRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOMLRN_SCN.CPY".

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
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
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

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *=================================================================
       MAIN-PROGRAM-NEXT-GROUP.

           IF ( GROUP-FLAG = "N" )
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = ZEROES )
              GO TO MAIN-PROGRAM-END.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO MAIN-PROGRAM-PROCESS-BRNO.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM READ-BR-FILE-NEXT.

           IF ( IO-BAD             ) OR
              ( BR-NO > END-BRANCH )
              GO TO MAIN-PROGRAM-END.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-PROCESS-BRNO.

           PERFORM PROCESS-BRANCH-MODULE.

           GO TO MAIN-PROGRAM-NEXT-GROUP.

      *=================================================================
       MAIN-PROGRAM-END.

           PERFORM WRITE-SUMMARY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DISPLAY SUMMARY TO SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM GET-TIME.
           MOVE SPACES TO SENDMESS-HEAD.

           STRING "PROGRAM COMPLETED AT ", TIME-EDIT
                  DELIMITED BY SIZE INTO SENDMESS-HEAD.

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
           MOVE "IOMLRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOMLRN-QUEUE-POS  TO Q-POS.
           MOVE IOMLRN-COPIES-POS TO C-POS.
           MOVE IOMLRN-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              PERFORM GET-TIME
              MOVE SPACES TO LEGEND
              STRING "STARTING TIME: ", TIME-EDIT
                     DELIMITED BY SIZE INTO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

           IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
              MOVE BEG-BRANCH TO SV-BEG-BRANCH
              MOVE END-BRANCH TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC  TO BEG-BRANCH
                                           END-BRANCH.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH         TO SV-BEG-BRANCH
              MOVE END-BRANCH         TO SV-END-BRANCH
              MOVE GROUP-FLAG         TO SV-GROUP-FLAG
              MOVE ENT-GROUP          TO SV-ENT-GROUP
              MOVE EXT-EOBUF-GROUP        TO ENT-GROUP
              MOVE ZEROES             TO BEG-BRANCH
              MOVE 9999               TO END-BRANCH
              MOVE "Y"                TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE.

           PERFORM GET-GPENV.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
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
              GO TO ENTER-END-SSNO.

      *=================================================================
       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              MOVE ZEROES TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES               TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"                  TO GROUP-FLAG
              MOVE ZEROES               TO BEG-BRANCH
              MOVE 9999                 TO END-BRANCH
              GO TO CHECK-BR-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.

           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.

           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.

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
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           GO TO CHECK-BR-SECURITY.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "N"                    TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BR-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" ) GO TO ENTER-BEG-BRANCH.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 = ZEROES ) OR
              ( VDUNUM0 > 3      ) 
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-BEG-SSNO.

           MOVE "ERNX090BEG-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SSNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SSNO.

      *-----------------------------------------------------------------
       ENTER-END-SSNO.

           MOVE "ERNX090END-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SSNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SSNO.

           IF ( BEG-SSNO > END-SSNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SSNO.

           GO TO ENTER-BEG-LNAME.

      *-----------------------------------------------------------------
       ENTER-ALL-SSNO.

           MOVE "SRNX090BEG-SSNO." TO VDU.
           MOVE ZEROES             TO VDU-SSNO-EDIT BEG-SSNO.
           PERFORM SCREENIO.

           MOVE "SRNX090END-SSNO." TO VDU.
           MOVE 999999999          TO VDU-SSNO-EDIT END-SSNO.
           PERFORM SCREENIO.
      *=================================================================
       ENTER-BEG-LNAME.

           MOVE "E  A350BEG-LNAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SSNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-LNAME.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-LNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-LNAME.

      *-----------------------------------------------------------------
       ENTER-END-LNAME.

           MOVE "E  A300END-LNAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LNAME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-LNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-LNAME.

           IF ( BEG-LNAME > END-LNAME )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LNAME.

           GO TO ENTER-BEG-FNAME.

      *-----------------------------------------------------------------
       ENTER-ALL-LNAME.

           MOVE "S  A000BEG-LNAME." TO VDU.
           MOVE SPACES              TO VDUBUF  BEG-LNAME.
           PERFORM SCREENIO.

           MOVE "S  A000END-LNAME." TO VDU.
           MOVE ALL "Z"             TO VDUBUF  END-LNAME.
           PERFORM SCREENIO.
      *=================================================================
       ENTER-BEG-FNAME.

           MOVE "E  A300BEG-FNAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-LNAME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-FNAME.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-FNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-FNAME.

      *-----------------------------------------------------------------
       ENTER-END-FNAME.

           MOVE "E  A300END-FNAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-FNAME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-FNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-FNAME.

           IF ( BEG-FNAME > END-FNAME )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-FNAME.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-FNAME.

           MOVE "S  A000BEG-FNAME." TO VDU.
           MOVE SPACES              TO VDUBUF  BEG-FNAME.
           PERFORM SCREENIO.

           MOVE "S  A000END-FNAME." TO VDU.
           MOVE ALL "Z"             TO VDUBUF  END-FNAME.
           PERFORM SCREENIO.

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

           IF ( GROUP-FLAG = "Y" )
              MOVE "S  A000BEG-BRANCH."   TO VDU
              MOVE ENT-GROUP              TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH."   TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH."   TO VDU
              MOVE BEG-BRANCH             TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH."   TO VDU
              MOVE END-BRANCH             TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SRNX090BEG-SSNO."        TO VDU.
           MOVE BEG-SSNO                  TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.
           MOVE "SRNX090END-SSNO."        TO VDU.
           MOVE END-SSNO                  TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A300BEG-LNAME."        TO VDU.
           MOVE BEG-LNAME                  TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A300END-LNAME."        TO VDU.
           MOVE END-LNAME                  TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A300BEG-FNAME."        TO VDU.
           MOVE BEG-FNAME                  TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A300END-FNAME."        TO VDU.
           MOVE END-FNAME                  TO VDUBUF.
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
      *    S C A N - G R 1 - F I L E
      *
      ******************************************************************
       SCAN-GR1-FILE SECTION.

           MOVE GRSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF   TO GRSCAN-BUF.

           IF ( SCAN-STATUS NOT = "00"   ) OR
              ( GRSCAN-KEY      = SPACES )
              MOVE "XX" TO VDUSTATUS
              GO TO SCAN-GR1-FILE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - -{ VALID SELECTION  }
           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE GRSCAN-KEY TO VDUBUF.
           MOVE "00"       TO VDUSTATUS.

      *=================================================================
       SCAN-GR1-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - B R A N C H - M O D U L E
      *
      ******************************************************************
       PROCESS-BRANCH-MODULE SECTION.

           MOVE PACE-COUNTER-MAX TO PACE-COUNTER.

      *- - - - - - - - - - - - - - - - - -{ VERIFY 'LOCAL' FILE PATHS  }
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO ML-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QML1-WBEG-OWNBR
                              QML2-WBEG-OWNBR.

           MOVE BR-NO       TO QML1-WEND-OWNBR
                                QML2-WEND-OWNBR.

           MOVE BEG-SSNO     TO QML1-WBEG-SSNO
                                QML3-WBEG-SSNO.
           MOVE END-SSNO     TO QML1-WEND-SSNO
                                QML3-WEND-SSNO.

           MOVE BEG-LNAME    TO QML2-WBEG-LNAME.
           MOVE END-LNAME    TO QML2-WEND-LNAME.
           MOVE SPACES       TO QML2-WBEG-FNAME.
           MOVE ALL "Z"      TO QML2-WEND-FNAME.


      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-ML1-FILE
              PERFORM START-ML1-FILE
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM OPEN-ML2-FILE
              PERFORM START-ML2-FILE
           ELSE
              PERFORM OPEN-ML3-FILE
              PERFORM START-ML3-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-ML1-FILE-NEXT
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM READ-ML2-FILE-NEXT
           ELSE
              PERFORM READ-ML3-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-BRANCH-MODULE-END.

      * KEY 3 IS JUST SSNO, NO BRANCH
           IF (PROCESS-KEY = 1 OR 2)
             IF ( ML-OWNBR NOT = ML-PATH-OWNBR )
                 GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( ML-SSNO < BEG-SSNO )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( ML-SSNO > END-SSNO )
                 GO TO PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 2 )
              IF ( ML-LNAME  < BEG-LNAME  )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( ML-LNAME  > END-LNAME  )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF ML-FNAME < BEG-FNAME OR > END-FNAME
                    GO TO PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 3 )
              IF ( ML-SSNO < BEG-SSNO )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( ML-SSNO > END-SSNO )
                 GO TO PROCESS-BRANCH-MODULE-END.


      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE ML-OWNBR                    TO D01-ML-BRNO.
           MOVE ML-SSNO                  TO D01-ML-SSNO.
           MOVE ML-LNAME                   TO D01-ML-LNAME.
           MOVE ML-FNAME                    TO D01-ML-FNAME.
           MOVE ML-CITY                    TO D01-ML-CITY.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 1 )
              PERFORM CLOSE-ML1-FILE
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM CLOSE-ML2-FILE
           ELSE
              PERFORM CLOSE-ML3-FILE.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-MODULE-EXIT.
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
              MOVE ML1-KEY TO PACE-DETAIL-KEY
           ELSE
           IF ( PROCESS-KEY = 2 )
              STRING ML-OWNBR, ML-LNAME, ML-FNAME
                     DELIMITED BY SIZE INTO PACE-DETAIL-KEY
           ELSE
              MOVE ML3-KEY TO PACE-DETAIL-KEY.

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
           IF ( GROUP-FLAG = "Y" )
              MOVE "GROUP ID NAME                 : " TO R01-PROMPT
              MOVE ENT-GROUP                          TO R01-TEXT
              MOVE 2                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH NUMBER       : " TO R01-PROMPT
              MOVE BEG-BRANCH                         TO R01-NUMERIC
              MOVE 2                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING    BRANCH NUMBER       : " TO R01-PROMPT
              MOVE END-BRANCH                         TO R01-NUMERIC
              MOVE 1                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SSNO               : "    TO R01-PROMPT.
           MOVE BEG-SSNO                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SSNO               : "    TO R01-PROMPT.
           MOVE END-SSNO                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING LNAME               : "    TO R01-PROMPT.
           MOVE BEG-LNAME                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    LNAME               : "    TO R01-PROMPT.
           MOVE END-LNAME                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING FNAME               : "    TO R01-PROMPT.
           MOVE BEG-FNAME                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    FNAME               : "    TO R01-PROMPT.
           MOVE END-FNAME                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
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
           MOVE "SQL TABLE IO UTILITY - MLFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOMLRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOMLRN-SYSDATE.
           DISPLAY IOMLRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOMLRN-FORM-FIELDS TO WR-BUF.
           MOVE IOMLRN-FORM-LIST TO WR-LIST.
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

           MOVE IOMLRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOMLRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOMLRN_EVA.CPY".
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
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBWI/PFSWINDOWS.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      *COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *COPY "LIBGB/RMFILE.CPY".
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
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      *-----------------------------------------------------------------
      *    LOAD ML-REC FROM QML-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-ML-FIELDS.
           MOVE QML-OWNBR TO ML-OWNBR.
           MOVE QML-SSNO TO ML-SSNO.
           MOVE QML-LNAME TO ML-LNAME.
           MOVE QML-FNAME TO ML-FNAME.
           MOVE QML-ADR1 TO ML-ADR1.
           MOVE QML-ADR2 TO ML-ADR2.
           MOVE QML-CITY TO ML-CITY.
           MOVE QML-STATE TO ML-STATE.
           MOVE QML-ZIP TO ML-ZIP.

           IF ( QML-REFDATE = "1900-01-01" )
              MOVE ZEROES TO ML-REFDATE
           ELSE
              MOVE QML-REFDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ML-REFDATE.

           MOVE QML-SRCD TO ML-SRCD.
           MOVE QML-SOLICIT TO ML-SOLICIT.
           MOVE QML-CONFIDENTIAL TO ML-CONFIDENTIAL.
           MOVE QML-LAST-FORM-NO TO ML-LAST-FORM-NO.

           IF ( QML-LAST-FORM-DATE = "1900-01-01" )
              MOVE ZEROES TO ML-LAST-FORM-DATE
           ELSE
              MOVE QML-LAST-FORM-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ML-LAST-FORM-DATE.


           IF ( QML-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO ML-LTOUCH-DATE
           ELSE
              MOVE QML-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO ML-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QML-REC FROM ML-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-ML-FIELDS.
           MOVE ML-OWNBR TO QML-OWNBR.
           MOVE ML-SSNO TO QML-SSNO.
           MOVE ML-LNAME TO QML-LNAME.
           MOVE ML-FNAME TO QML-FNAME.
           MOVE ML-ADR1 TO QML-ADR1.
           MOVE ML-ADR2 TO QML-ADR2.
           MOVE ML-CITY TO QML-CITY.
           MOVE ML-STATE TO QML-STATE.
           MOVE ML-ZIP TO QML-ZIP.

           IF ( ML-REFDATE = ZEROES )
              MOVE "1900-01-01" TO QML-REFDATE
           ELSE
              MOVE ML-REFDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QML-REFDATE.

           MOVE ML-SRCD TO QML-SRCD.
           MOVE ML-SOLICIT TO QML-SOLICIT.
           MOVE ML-CONFIDENTIAL TO QML-CONFIDENTIAL.
           MOVE ML-LAST-FORM-NO TO QML-LAST-FORM-NO.

           IF ( ML-LAST-FORM-DATE = ZEROES )
              MOVE "1900-01-01" TO QML-LAST-FORM-DATE
           ELSE
              MOVE ML-LAST-FORM-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QML-LAST-FORM-DATE.


           IF ( ML-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QML-LTOUCH-DATE
           ELSE
              MOVE ML-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QML-LTOUCH-DATE.

 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-ML1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ML-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ML-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ML-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO ML-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO ML-ALL-DATA
              MOVE ML-ALL-PATH             TO ML-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ML-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ML-ALL-BASE
              MOVE FILPATH-MACHINE         TO ML-ALL-MACHINE
              MOVE FILPATH-DATA            TO ML-ALL-DATA
              MOVE ML-ALL-PATH             TO ML-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-MLFILE NOT = "S" )
              MOVE ML-PATH-BEG             TO ML-SELECT-PATH-BEG
              MOVE ML-PATH-FILE            TO ML-SELECT-PATH-FILE
           ELSE
              MOVE ML-PATH-SQL             TO ML-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-ML1-FILE.
           PERFORM LOAD-ML1-FILE.
           PERFORM OPEN-IT.
           MOVE ML-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-ML1-FILE.
           PERFORM START-IT.
           MOVE ML-PATH-OWNBR  TO ML-OWNBR.
           MOVE ML-SELECT-PATH TO E-FILE.
           MOVE ML1-KEY        TO E-KEYX.

           IF ( ML1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-ML1-FILE.

           IF ( QML1-WEND-SSNO = ZEROES    ) OR
              ( QML1-WEND-SSNO NOT NUMERIC )
              MOVE ML-OWNBR             TO QML1-WBEG-OWNBR
                                           QML1-WEND-OWNBR
              MOVE ML-SSNO              TO QML1-WBEG-SSNO
              MOVE ALL "9"              TO QML1-WEND-SSNO.


           EXEC SQL
            DECLARE ML1_CURSOR CURSOR FOR
            SELECT MLFILE.ML_OWNBR,
                   MLFILE.ML_SSNO,
                   ISNULL(MLFILE.ML_LNAME,''),
                   ISNULL(MLFILE.ML_FNAME,''),
                   ISNULL(MLFILE.ML_ADR1,''),
                   ISNULL(MLFILE.ML_ADR2,''),
                   ISNULL(MLFILE.ML_CITY,''),
                   ISNULL(MLFILE.ML_STATE,''),
                   ISNULL(MLFILE.ML_ZIP,''),
                   ISNULL(CAST(MLFILE.ML_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(MLFILE.ML_SRCD,''),
                   ISNULL(MLFILE.ML_SOLICIT,''),
                   ISNULL(MLFILE.ML_CONFIDENTIAL,''),
                   MLFILE.ML_LAST_FORM_NO,
                   ISNULL(CAST(MLFILE.ML_LAST_FORM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(MLFILE.ML_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            FROM DBO.MLFILE
            WHERE MLFILE.ML_OWNBR >= :QML1-WBEG-OWNBR
              AND MLFILE.ML_OWNBR <= :QML1-WEND-OWNBR
              AND MLFILE.ML_SSNO >= :QML1-WBEG-SSNO
              AND MLFILE.ML_SSNO <= :QML1-WEND-SSNO
            ORDER BY MLFILE.ML_OWNBR,
                     MLFILE.ML_SSNO
           END-EXEC.

           EXEC SQL
               OPEN ML1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QML1-WBEG-OWNBR
                          QML1-WEND-OWNBR
                          QML1-WBEG-SSNO
                          QML1-WEND-SSNO.

           MOVE STAT---GOOD TO ML1-CURSOR-STAT.


      *-----------------------------------------------------------------
       READ-ML1-FILE.
           PERFORM READ-IT.
           MOVE ML-PATH-OWNBR  TO ML-OWNBR. 
           MOVE ML-SELECT-PATH TO E-FILE.
           MOVE ML1-KEY        TO E-KEYX.

           MOVE ML-OWNBR   TO QML-OWNBR.
           MOVE ML-SSNO    TO QML-SSNO.

           EXEC SQL
            SELECT MLFILE.ML_OWNBR,
                   MLFILE.ML_SSNO,
                   ISNULL(MLFILE.ML_LNAME,''),
                   ISNULL(MLFILE.ML_FNAME,''),
                   ISNULL(MLFILE.ML_ADR1,''),
                   ISNULL(MLFILE.ML_ADR2,''),
                   ISNULL(MLFILE.ML_CITY,''),
                   ISNULL(MLFILE.ML_STATE,''),
                   ISNULL(MLFILE.ML_ZIP,''),
                   ISNULL(CAST(MLFILE.ML_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(MLFILE.ML_SRCD,''),
                   ISNULL(MLFILE.ML_SOLICIT,''),
                   ISNULL(MLFILE.ML_CONFIDENTIAL,''),
                   MLFILE.ML_LAST_FORM_NO,
                   ISNULL(CAST(MLFILE.ML_LAST_FORM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(MLFILE.ML_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            INTO :QML-REC
            FROM DBO.MLFILE
            WHERE MLFILE.ML_OWNBR = :QML-OWNBR
              AND MLFILE.ML_SSNO = :QML-SSNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-ML1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-ML-FIELDS.

      *-----------------------------------------------------------------
       READ-ML1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ML-PATH-OWNBR  TO ML-OWNBR.
           MOVE ML-SELECT-PATH TO E-FILE.
           MOVE ML1-KEY        TO E-KEYX.
           INITIALIZE QML-REC.

           IF ( ML1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH ML1_CURSOR INTO :QML-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-ML1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-ML-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-ML1-FILE.

           IF ( ML1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE ML1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO ML1-CURSOR-STAT.

      *#################################################################
       LOAD-ML2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ML-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ML-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ML-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO ML-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO ML-ALL-DATA
              MOVE ML-ALL-PATH             TO ML-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ML-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ML-ALL-BASE
              MOVE FILPATH-MACHINE         TO ML-ALL-MACHINE
              MOVE FILPATH-DATA            TO ML-ALL-DATA
              MOVE ML-ALL-PATH             TO ML-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-MLFILE NOT = "S" )
              MOVE ML-PATH-BEG             TO ML-SELECT-PATH-BEG
              MOVE ML-PATH-FILE            TO ML-SELECT-PATH-FILE
           ELSE
              MOVE ML-PATH-SQL             TO ML-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-ML2-FILE.
           PERFORM LOAD-ML2-FILE.
           PERFORM OPEN-IT.
           MOVE ML-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-ML2-FILE.
           PERFORM START-IT.
           MOVE ML-PATH-OWNBR  TO ML-OWNBR.
           MOVE ML-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING ML-OWNBR, ML-LNAME, ML-FNAME DELIMITED BY SIZE
                   INTO E-KEYX.

           IF ( ML2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-ML2-FILE.

           IF ( QML2-WEND-FNAME = SPACES    ) 
              MOVE ML-OWNBR             TO QML2-WBEG-OWNBR
                                           QML2-WEND-OWNBR
              MOVE ML-LNAME             TO QML2-WBEG-LNAME
              MOVE ALL "Z"              TO QML2-WEND-LNAME
              MOVE ML-FNAME             TO QML2-WBEG-FNAME
              MOVE ALL "Z"              TO QML2-WEND-FNAME.

           EXEC SQL
            DECLARE ML2_CURSOR CURSOR FOR
            SELECT MLFILE.ML_OWNBR,
                   MLFILE.ML_SSNO,
                   ISNULL(MLFILE.ML_LNAME,''),
                   ISNULL(MLFILE.ML_FNAME,''),
                   ISNULL(MLFILE.ML_ADR1,''),
                   ISNULL(MLFILE.ML_ADR2,''),
                   ISNULL(MLFILE.ML_CITY,''),
                   ISNULL(MLFILE.ML_STATE,''),
                   ISNULL(MLFILE.ML_ZIP,''),
                   ISNULL(CAST(MLFILE.ML_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(MLFILE.ML_SRCD,''),
                   ISNULL(MLFILE.ML_SOLICIT,''),
                   ISNULL(MLFILE.ML_CONFIDENTIAL,''),
                   MLFILE.ML_LAST_FORM_NO,
                   ISNULL(CAST(MLFILE.ML_LAST_FORM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(MLFILE.ML_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            FROM DBO.MLFILE
            WHERE MLFILE.ML_OWNBR >= :QML2-WBEG-OWNBR
              AND MLFILE.ML_OWNBR <= :QML2-WEND-OWNBR
              AND MLFILE.ML_LNAME >= :QML2-WBEG-LNAME
              AND MLFILE.ML_LNAME <= :QML2-WEND-LNAME
              AND MLFILE.ML_FNAME >= :QML2-WBEG-FNAME
              AND MLFILE.ML_FNAME <= :QML2-WEND-FNAME
            ORDER BY MLFILE.ML_OWNBR,
                     MLFILE.ML_LNAME,
                     MLFILE.ML_FNAME,
                     MLFILE.ML_SSNO
           END-EXEC.

           EXEC SQL
               OPEN ML2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QML2-WBEG-OWNBR
                          QML2-WEND-OWNBR.
           MOVE SPACES TO QML2-WBEG-LNAME
                          QML2-WEND-LNAME
                          QML2-WBEG-FNAME
                          QML2-WEND-FNAME.

           MOVE STAT---GOOD TO ML2-CURSOR-STAT.


      *-----------------------------------------------------------------
       READ-ML2-FILE.
           PERFORM READ-IT.
           MOVE ML-PATH-OWNBR  TO ML-OWNBR. 
           MOVE ML-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING ML-OWNBR, ML-LNAME, ML-FNAME DELIMITED BY SIZE
                               INTO E-KEYX.

           MOVE ML-OWNBR   TO QML-OWNBR.
           MOVE ML-LNAME   TO QML-LNAME.
           MOVE ML-FNAME   TO QML-FNAME.

           EXEC SQL
            SELECT MLFILE.ML_OWNBR,
                   MLFILE.ML_SSNO,
                   ISNULL(MLFILE.ML_LNAME,''),
                   ISNULL(MLFILE.ML_FNAME,''),
                   ISNULL(MLFILE.ML_ADR1,''),
                   ISNULL(MLFILE.ML_ADR2,''),
                   ISNULL(MLFILE.ML_CITY,''),
                   ISNULL(MLFILE.ML_STATE,''),
                   ISNULL(MLFILE.ML_ZIP,''),
                   ISNULL(CAST(MLFILE.ML_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(MLFILE.ML_SRCD,''),
                   ISNULL(MLFILE.ML_SOLICIT,''),
                   ISNULL(MLFILE.ML_CONFIDENTIAL,''),
                   MLFILE.ML_LAST_FORM_NO,
                   ISNULL(CAST(MLFILE.ML_LAST_FORM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(MLFILE.ML_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            INTO :QML-REC
            FROM DBO.MLFILE
            WHERE MLFILE.ML_OWNBR = :QML-OWNBR
              AND MLFILE.ML_LNAME = :QML-LNAME
              AND MLFILE.ML_FNAME = :QML-FNAME
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-ML2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-ML-FIELDS.

      *-----------------------------------------------------------------
       READ-ML2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ML-PATH-OWNBR  TO ML-OWNBR.
           MOVE ML-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING ML-OWNBR, ML-LNAME, ML-FNAME DELIMITED BY SIZE
                               INTO E-KEYX.
           INITIALIZE QML-REC.

           IF ( ML2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH ML2_CURSOR INTO :QML-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-ML2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-ML-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-ML2-FILE.

           IF ( ML2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE ML2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO ML2-CURSOR-STAT.


      *-----------------------------------------------------------------
       LOAD-ML3-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( ML-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( ML-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO ML-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO ML-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO ML-ALL-DATA
              MOVE ML-ALL-PATH             TO ML-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( ML-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO ML-ALL-BASE
              MOVE FILPATH-MACHINE         TO ML-ALL-MACHINE
              MOVE FILPATH-DATA            TO ML-ALL-DATA
              MOVE ML-ALL-PATH             TO ML-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-MLFILE NOT = "S" )
              MOVE ML-PATH-BEG             TO ML-SELECT-PATH-BEG
              MOVE ML-PATH-FILE            TO ML-SELECT-PATH-FILE
           ELSE
              MOVE ML-PATH-SQL             TO ML-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-ML3-FILE.
           PERFORM LOAD-ML3-FILE.
           PERFORM OPEN-IT.
           MOVE ML-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-ML3-FILE.
           PERFORM START-IT.
           MOVE ML-PATH-OWNBR  TO ML-OWNBR.
           MOVE ML-SELECT-PATH TO E-FILE.
           MOVE ML3-KEY        TO E-KEYX.

           IF ( ML3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-ML3-FILE.

           IF ( QML3-WEND-SSNO = ZEROES    ) OR
              ( QML3-WEND-SSNO NOT NUMERIC )
              MOVE ML-SSNO              TO QML3-WBEG-SSNO
              MOVE ALL "9"              TO QML3-WEND-SSNO.

           EXEC SQL
            DECLARE ML3_CURSOR CURSOR FOR
            SELECT MLFILE.ML_OWNBR,
                   MLFILE.ML_SSNO,
                   ISNULL(MLFILE.ML_LNAME,''),
                   ISNULL(MLFILE.ML_FNAME,''),
                   ISNULL(MLFILE.ML_ADR1,''),
                   ISNULL(MLFILE.ML_ADR2,''),
                   ISNULL(MLFILE.ML_CITY,''),
                   ISNULL(MLFILE.ML_STATE,''),
                   ISNULL(MLFILE.ML_ZIP,''),
                   ISNULL(CAST(MLFILE.ML_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(MLFILE.ML_SRCD,''),
                   ISNULL(MLFILE.ML_SOLICIT,''),
                   ISNULL(MLFILE.ML_CONFIDENTIAL,''),
                   MLFILE.ML_LAST_FORM_NO,
                   ISNULL(CAST(MLFILE.ML_LAST_FORM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(MLFILE.ML_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            FROM DBO.MLFILE
            WHERE MLFILE.ML_SSNO >= :QML3-WBEG-SSNO
              AND MLFILE.ML_SSNO <= :QML3-WEND-SSNO
            ORDER BY MLFILE.ML_SSNO,
                     MLFILE.ML_OWNBR
           END-EXEC.

           EXEC SQL
               OPEN ML3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QML3-WBEG-SSNO
                          QML3-WEND-SSNO.

           MOVE STAT---GOOD TO ML3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-ML3-FILE.
           PERFORM READ-IT.
           MOVE ML-PATH-OWNBR  TO ML-OWNBR. 
           MOVE ML-SELECT-PATH TO E-FILE.
           MOVE ML3-KEY        TO E-KEYX.

           MOVE ML-SSNO    TO QML-SSNO.

           EXEC SQL
            SELECT MLFILE.ML_OWNBR,
                   MLFILE.ML_SSNO,
                   ISNULL(MLFILE.ML_LNAME,''),
                   ISNULL(MLFILE.ML_FNAME,''),
                   ISNULL(MLFILE.ML_ADR1,''),
                   ISNULL(MLFILE.ML_ADR2,''),
                   ISNULL(MLFILE.ML_CITY,''),
                   ISNULL(MLFILE.ML_STATE,''),
                   ISNULL(MLFILE.ML_ZIP,''),
                   ISNULL(CAST(MLFILE.ML_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(MLFILE.ML_SRCD,''),
                   ISNULL(MLFILE.ML_SOLICIT,''),
                   ISNULL(MLFILE.ML_CONFIDENTIAL,''),
                   MLFILE.ML_LAST_FORM_NO,
                   ISNULL(CAST(MLFILE.ML_LAST_FORM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(MLFILE.ML_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            INTO :QML-REC
            FROM DBO.MLFILE
            WHERE MLFILE.ML_SSNO = :QML-SSNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-ML3-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-ML-FIELDS.

      *-----------------------------------------------------------------
       READ-ML3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ML-PATH-OWNBR  TO ML-OWNBR.
           MOVE ML-SELECT-PATH TO E-FILE.
           MOVE ML3-KEY        TO E-KEYX.
           INITIALIZE QML-REC.

           IF ( ML3-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH ML3_CURSOR INTO :QML-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-ML3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-ML-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-ML3-FILE.

           IF ( ML3-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE ML3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO ML3-CURSOR-STAT.


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

      *    MOVE WK1-PATH TO ACCESS-BUF.
      *    PERFORM REMOVE-WORKFILE.

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

           IF ( SV-BEG-BRANCH NOT = 9999 )
              IF ( EXT-EOBUF-GROUP-FG = "Y"  )
                 MOVE SV-BEG-BRANCH     TO BEG-BRANCH
                 MOVE SV-END-BRANCH     TO END-BRANCH
                 MOVE SV-GROUP-FLAG     TO GROUP-FLAG
                 MOVE SV-ENT-GROUP      TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BRANCH TO BEG-BRANCH
                 MOVE SV-END-BRANCH TO END-BRANCH.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IOMLRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
