       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOTRDR.
       AUTHOR.        BAH
       DATE-WRITTEN.  04-12-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOTRDR.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : TRDFILE 
      * TABLE DESC    : TRANSACTION DEPOSIT FILE
      * OUTPUT FILE   : XPC/IOTRDR.PRE
      * USED FOR      : LIBLP/LPWSTRD.CPY  --- QTRD-REC
      *                 LIBLP/LPTRD1R.CPY  --- READ
      *                 LIBLP/LPTRD1RN.CPY --- READ/READ-NEXT/START
      *                 LIBLP/LPTRD2RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0412 ***NEW***
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./IO/IOTRDR.CBL S34 05/05/21-17:02:37 >".

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
       COPY "LIBLP/LP01TRD.CPY".
       COPY "LIBLP/LPWSTRD.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QTRD1-WHERE-BEG-KEY.
           03  QTRD1-WBEG-BRNO             PIC 9(4).
           03  QTRD1-WBEG-DATE             PIC X(10).
 
       01  QTRD1-WHERE-END-KEY.
           03  QTRD1-WEND-BRNO             PIC 9(4).
           03  QTRD1-WEND-DATE             PIC X(10).

       01  QTRD2-WHERE-BEG-KEY.
           03  QTRD2-WBEG-DATE             PIC X(10).
           03  QTRD2-WBEG-BRNO             PIC 9(4).
 
       01  QTRD2-WHERE-END-KEY.
           03  QTRD2-WEND-DATE             PIC X(10).
           03  QTRD2-WEND-BRNO             PIC 9(4).
 
       01  QTRD-REC.
           03  QTD-BRNO             PIC 9(4).
           03  QTD-DATE             PIC X(10).
           03  QTD-LTOUCH-DATE      PIC X(10).
           03  QTD-DEPOSIT          PIC S9(7)V99.
           03  QTD-CHECK-DEPOSIT    PIC S9(7)V99.
           03  QTD-BANK             PIC 9(11).
           03  QTD-CASH             PIC 9(11).
           03  QTD-GLFG             PIC X.
           03  QTD-GLOVERSHORT      PIC 9(11).
           03  QTD-OVERSHORT        PIC S9(5)V99.
           03  QTD-USERID           PIC X(10).
           03  QTD-POSTDATE         PIC X(10).
           03  QTD-POSTTIME         PIC 9(6).

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
           03  BEG-DATE                PIC 9(08).
           03  END-DATE                PIC 9(08).

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
           03  FILLER PIC X(30) VALUE "BRNO     DATE        DEPOSIT  ".
           03  FILLER PIC X(30) VALUE "       BANK".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-TD-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-TD-DATE                    PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-TD-DEPOSIT                 PIC Z(6)9.99-.
           03  FILLER                         PIC X(03).
           03  D01-TD-BANK                    PIC 9(11).

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
       COPY "LIBIO/IOTRDR_DEF.CPY".
       COPY "LIBIO/IOTRDR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOTRDR_SCN.CPY".

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
           MOVE "IOTRDR"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOTRDR-QUEUE-POS  TO Q-POS.
           MOVE IOTRDR-COPIES-POS TO C-POS.
           MOVE IOTRDR-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-DATE.

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
              ( VDUNUM0 > 2      ) 
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

       ENTER-BEG-DATE.

           MOVE SPACES  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
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

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "S  8080BEG-DATE." TO VDU.
           MOVE JULIAN-BEG         TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE." TO VDU.
           MOVE JULIAN-END         TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
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

           MOVE "S  A000BEG-DATE."    TO VDU.
           MOVE BEG-DATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A000END-DATE."    TO VDU.
           MOVE END-DATE              TO VDU-DATE-EDIT.
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

           MOVE "B"                        TO TRD-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QTRD1-WBEG-BRNO
                              QTRD2-WBEG-BRNO.
           MOVE BR-NO     TO QTRD1-WEND-BRNO
                              QTRD2-WEND-BRNO.

           MOVE BEG-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRD1-WBEG-DATE
                                        QTRD2-WBEG-DATE.

           MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRD1-WEND-DATE
                                        QTRD2-WEND-DATE.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF (PROCESS-KEY = 1)
              PERFORM OPEN-TRD1-FILE
              PERFORM START-TRD1-FILE
           ELSE
              PERFORM OPEN-TRD2-FILE
              PERFORM START-TRD2-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           IF (PROCESS-KEY = 1)
              PERFORM READ-TRD1-FILE-NEXT
           ELSE
              PERFORM READ-TRD2-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( TD-BRNO < BEG-BRANCH )
                  GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( TD-BRNO > END-BRANCH )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF TD-DATE < BEG-DATE OR > END-DATE
                    GO TO PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 2 )
              IF ( TD-DATE < BEG-DATE )
                  GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( TD-DATE > END-DATE )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF TD-BRNO < BEG-BRANCH OR > END-BRANCH
                    GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE TD-BRNO                  TO D01-TD-BRNO.
           MOVE TD-DATE                  TO D01-TD-DATE.
           MOVE TD-DEPOSIT               TO D01-TD-DEPOSIT.
           MOVE TD-BANK                  TO D01-TD-BANK.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           IF PROCESS-KEY = 1
              PERFORM CLOSE-TRD1-FILE
           ELSE
              PERFORM CLOSE-TRD2-FILE.

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
              MOVE TRD1-KEY TO PACE-DETAIL-KEY
           ELSE
              STRING TD-DATE,TD-BRNO
                DELIMITED BY SIZE INTO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING DATE               : "    TO R01-PROMPT.
           MOVE BEG-DATE                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE               : "    TO R01-PROMPT.
           MOVE END-DATE                             TO R01-NUMERIC.
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
           MOVE "SQL TABLE IO UTILITY - TRDFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOTRDR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOTRDR-SYSDATE.
           DISPLAY IOTRDR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOTRDR-FORM-FIELDS TO WR-BUF.
           MOVE IOTRDR-FORM-LIST TO WR-LIST.
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

           MOVE IOTRDR-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOTRDR-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOTRDR_EVA.CPY".
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
      *    LOAD TRD-REC FROM QTRD-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-TRD-FIELDS.
           MOVE QTD-BRNO TO TD-BRNO.

           IF ( QTD-DATE = "1900-01-01" )
              MOVE ZEROES TO TD-DATE
           ELSE
              MOVE QTD-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TD-DATE.


           IF ( QTD-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO TD-LTOUCH-DATE
           ELSE
              MOVE QTD-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TD-LTOUCH-DATE.

           MOVE QTD-DEPOSIT TO TD-DEPOSIT.
           MOVE QTD-CHECK-DEPOSIT TO TD-CHECK-DEPOSIT.
           MOVE QTD-BANK TO TD-BANK.
           MOVE QTD-CASH TO TD-CASH.
           MOVE QTD-GLFG TO TD-GLFG.
           MOVE QTD-GLOVERSHORT TO TD-GLOVERSHORT.
           MOVE QTD-OVERSHORT TO TD-OVERSHORT.
           MOVE QTD-USERID TO TD-USERID.

           IF ( QTD-POSTDATE = "1900-01-01" )
              MOVE ZEROES TO TD-POSTDATE
           ELSE
              MOVE QTD-POSTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TD-POSTDATE.

           MOVE QTD-POSTTIME TO TD-POSTTIME.
 
      *-----------------------------------------------------------------
      *    LOAD QTRD-REC FROM TRD-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-TRD-FIELDS.
           MOVE TD-BRNO TO QTD-BRNO.

           IF ( TD-DATE = ZEROES )
              MOVE "1900-01-01" TO QTD-DATE
           ELSE
              MOVE TD-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTD-DATE.


           IF ( TD-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTD-LTOUCH-DATE
           ELSE
              MOVE TD-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTD-LTOUCH-DATE.

           MOVE TD-DEPOSIT TO QTD-DEPOSIT.
           MOVE TD-CHECK-DEPOSIT TO QTD-CHECK-DEPOSIT.
           MOVE TD-BANK TO QTD-BANK.
           MOVE TD-CASH TO QTD-CASH.
           MOVE TD-GLFG TO QTD-GLFG.
           MOVE TD-GLOVERSHORT TO QTD-GLOVERSHORT.
           MOVE TD-OVERSHORT TO QTD-OVERSHORT.
           MOVE TD-USERID TO QTD-USERID.

           IF ( TD-POSTDATE = ZEROES )
              MOVE "1900-01-01" TO QTD-POSTDATE
           ELSE
              MOVE TD-POSTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTD-POSTDATE.

           MOVE TD-POSTTIME TO QTD-POSTTIME.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-TRD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRD-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRD-ALL-DATA
              MOVE TRD-ALL-PATH             TO TRD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRD-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRD-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRD-ALL-DATA
              MOVE TRD-ALL-PATH             TO TRD-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRDFILE NOT = "S" )
              MOVE TRD-PATH-BEG             TO TRD-SELECT-PATH-BEG
              MOVE TRD-PATH-FILE            TO TRD-SELECT-PATH-FILE
           ELSE
              MOVE TRD-PATH-SQL             TO TRD-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRD1-FILE.
           PERFORM LOAD-TRD1-FILE.
           PERFORM OPEN-IT.
           MOVE TRD-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRD1-FILE.
           PERFORM START-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           MOVE TRD-SELECT-PATH TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.

           IF ( TRD1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRD1-FILE.

           IF ( QTRD1-WEND-BRNO = ZEROES    ) OR
              ( QTRD1-WEND-BRNO NOT NUMERIC )
              MOVE TRD-PATH-OWNBR       TO QTRD1-WBEG-BRNO
              MOVE TRD-PATH-OWNBR       TO QTRD1-WEND-BRNO
              MOVE TD-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRD1-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRD1-WEND-DATE.


           EXEC SQL
            DECLARE TRD1_CURSOR CURSOR FOR
            SELECT TRDFILE.TD_BRNO,
                   ISNULL(CAST(TRDFILE.TD_DATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRDFILE.TD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRDFILE.TD_DEPOSIT,
                   TRDFILE.TD_CHECK_DEPOSIT,
                   TRDFILE.TD_BANK,
                   TRDFILE.TD_CASH,
                   ISNULL(TRDFILE.TD_GLFG,''),
                   TRDFILE.TD_GLOVERSHORT,
                   TRDFILE.TD_OVERSHORT,
                   ISNULL(TRDFILE.TD_USERID,''),
                   ISNULL(CAST(TRDFILE.TD_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRDFILE.TD_POSTTIME
            FROM DBO.TRDFILE
            WHERE TRDFILE.TD_BRNO >= :QTRD1-WBEG-BRNO
              AND TRDFILE.TD_BRNO <= :QTRD1-WEND-BRNO
              AND CAST(TRDFILE.TD_DATE AS VARCHAR(10))
                          >= :QTRD1-WBEG-DATE
              AND CAST(TRDFILE.TD_DATE AS VARCHAR(10))
                          <= :QTRD1-WEND-DATE
            ORDER BY TRDFILE.TD_BRNO,
                     TRDFILE.TD_DATE
           END-EXEC.

           EXEC SQL
               OPEN TRD1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRD1-WBEG-BRNO 
                          QTRD1-WEND-BRNO.
 
           MOVE SPACES TO QTRD1-WBEG-DATE
                          QTRD1-WEND-DATE.

           MOVE STAT---GOOD TO TRD1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRD1-FILE.
           PERFORM READ-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO. 
           MOVE TRD-SELECT-PATH TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.

           MOVE TD-BRNO   TO QTD-BRNO.
           MOVE TD-DATE   TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTD-DATE.

           EXEC SQL
            SELECT TRDFILE.TD_BRNO,
                   ISNULL(CAST(TRDFILE.TD_DATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRDFILE.TD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRDFILE.TD_DEPOSIT,
                   TRDFILE.TD_CHECK_DEPOSIT,
                   TRDFILE.TD_BANK,
                   TRDFILE.TD_CASH,
                   ISNULL(TRDFILE.TD_GLFG,''),
                   TRDFILE.TD_GLOVERSHORT,
                   TRDFILE.TD_OVERSHORT,
                   ISNULL(TRDFILE.TD_USERID,''),
                   ISNULL(CAST(TRDFILE.TD_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRDFILE.TD_POSTTIME
            INTO :QTRD-REC
            FROM DBO.TRDFILE
            WHERE TRDFILE.TD_BRNO = :QTD-BRNO
              AND TRDFILE.TD_DATE = :QTD-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRD1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRD-FIELDS.

      *-----------------------------------------------------------------
       READ-TRD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           MOVE TRD-SELECT-PATH TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.
           INITIALIZE QTRD-REC.

           IF ( TRD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRD1_CURSOR INTO :QTRD-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRD1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRD-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRD1-FILE.

           IF ( TRD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRD1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRD1-CURSOR-STAT.

      *===============================================================
       LOAD-TRD2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRD-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRD-ALL-DATA
              MOVE TRD-ALL-PATH             TO TRD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRD-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRD-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRD-ALL-DATA
              MOVE TRD-ALL-PATH             TO TRD-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRDFILE NOT = "S" )
              MOVE TRD-PATH-BEG             TO TRD-SELECT-PATH-BEG
              MOVE TRD-PATH-FILE            TO TRD-SELECT-PATH-FILE
           ELSE
              MOVE TRD-PATH-SQL             TO TRD-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRD2-FILE.
           PERFORM LOAD-TRD2-FILE.
           PERFORM OPEN-IT.
           MOVE TRD-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRD2-FILE.
           PERFORM START-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           MOVE TRD-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING TD-DATE,TD-BRNO DELIMITED BY SIZE INTO E-KEYX.

           IF ( TRD2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRD2-FILE.

           IF ( QTRD2-WEND-BRNO = ZEROES    ) OR
              ( QTRD2-WEND-BRNO NOT NUMERIC )
              MOVE TRD-PATH-OWNBR       TO QTRD2-WBEG-BRNO
              MOVE TRD-PATH-OWNBR       TO QTRD2-WEND-BRNO
              MOVE TD-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRD2-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRD2-WEND-DATE.

           EXEC SQL
            DECLARE TRD2_CURSOR CURSOR FOR
            SELECT TRDFILE.TD_BRNO,
                   ISNULL(CAST(TRDFILE.TD_DATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRDFILE.TD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRDFILE.TD_DEPOSIT,
                   TRDFILE.TD_CHECK_DEPOSIT,
                   TRDFILE.TD_BANK,
                   TRDFILE.TD_CASH,
                   ISNULL(TRDFILE.TD_GLFG,''),
                   TRDFILE.TD_GLOVERSHORT,
                   TRDFILE.TD_OVERSHORT,
                   ISNULL(TRDFILE.TD_USERID,''),
                   ISNULL(CAST(TRDFILE.TD_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRDFILE.TD_POSTTIME
            FROM DBO.TRDFILE
            WHERE CAST(TRDFILE.TD_DATE AS VARCHAR(10))
                          >= :QTRD2-WBEG-DATE
              AND CAST(TRDFILE.TD_DATE AS VARCHAR(10))
                          <= :QTRD2-WEND-DATE
              AND TRDFILE.TD_BRNO >= :QTRD2-WBEG-BRNO
              AND TRDFILE.TD_BRNO <= :QTRD2-WEND-BRNO
            ORDER BY TRCFILE.TC_DATE,
                     TRCFILE.TC_BRNO
           END-EXEC.

           EXEC SQL
               OPEN TRD2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRD2-WBEG-BRNO 
                          QTRD2-WEND-BRNO.
 
           MOVE SPACES TO QTRD2-WBEG-DATE
                          QTRD2-WEND-DATE.

           MOVE STAT---GOOD TO TRD2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRD2-FILE.
           PERFORM READ-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO. 
           MOVE TRD-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING TD-DATE,TD-BRNO DELIMITED BY SIZE INTO E-KEYX.

           MOVE TD-BRNO   TO QTD-BRNO.
           MOVE TD-DATE   TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTD-DATE.

           EXEC SQL
            SELECT TRDFILE.TD_BRNO,
                   ISNULL(CAST(TRDFILE.TD_DATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRDFILE.TD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRDFILE.TD_DEPOSIT,
                   TRDFILE.TD_CHECK_DEPOSIT,
                   TRDFILE.TD_BANK,
                   TRDFILE.TD_CASH,
                   ISNULL(TRDFILE.TD_GLFG,''),
                   TRDFILE.TD_GLOVERSHORT,
                   TRDFILE.TD_OVERSHORT,
                   ISNULL(TRDFILE.TD_USERID,''),
                   ISNULL(CAST(TRDFILE.TD_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRDFILE.TD_POSTTIME
            INTO :QTRD-REC
            FROM DBO.TRDFILE
            WHERE TRDFILE.TD_DATE = :QTD-DATE
              AND TRDFILE.TD_BRNO = :QTD-BRNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRD2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRD-FIELDS.

      *-----------------------------------------------------------------
       READ-TRD2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           MOVE TRD-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING TD-DATE,TD-BRNO DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QTRD-REC.

           IF ( TRD2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRD2_CURSOR INTO :QTRD-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRD2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRD-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRD2-FILE.

           IF ( TRD2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRD2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRD2-CURSOR-STAT.

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
              DESTROY IOTRDR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
