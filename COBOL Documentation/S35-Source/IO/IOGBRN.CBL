       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOGBRN.
       AUTHOR.        BAH.
       DATE-WRITTEN.  04-02-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOGBRN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : GBFILE 
      * TABLE DESC    : GLOBAL FILE
      * OUTPUT FILE   : XPC/IOGBRN.PRE
      * USED FOR      : LIBGB/GBWSGB.CPY  --- QGB-REC
      *                 LIBGB/GBGBRN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0402 ***NEW***
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
           "@(#)IO/IOGBRN.CBL S34 05/20/21-09:07:21 >".

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
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GBWSGB.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QGB-WHERE-BEG-KEY.
           03  QGB-WBEG-BRNO             PIC 9(04).
           03  QGB-WBEG-NO               PIC 9.
 
       01  QGB-WHERE-END-KEY.
           03  QGB-WEND-BRNO             PIC 9(04).
           03  QGB-WEND-NO               PIC 9.
 
       01  QGB-REC.
           03  QGB-BRNO                      PIC 9(4).
           03  QGB-NO                        PIC 9.
           03  QGB-LTOUCH-DATE               PIC X(10).
           03  QGB-COMPANY-NAME              PIC X(40).
           03  QGB-ADDR-1                    PIC X(25).
           03  QGB-ADDR-2                    PIC X(25).
           03  QGB-CITY                      PIC X(15).
           03  QGB-STATE                     PIC XX.
           03  QGB-ZIP                       PIC X(10).
           03  QGB-PHONE                     PIC 9(10).
           03  QGB-NEXT-SF                   PIC 9(8).
           03  QGB-NEXT-SMALL                PIC 9(8).
           03  QGB-NEXT-OTHER                PIC 9(8).
           03  QGB-HOLIDAY-1                 PIC 9(4).
           03  QGB-HOLIDAY-2                 PIC 9(4).
           03  QGB-HOLIDAY-3                 PIC 9(4).
           03  QGB-HOLIDAY-4                 PIC 9(4).
           03  QGB-HOLIDAY-5                 PIC 9(4).
           03  QGB-HOLIDAY-6                 PIC 9(4).
           03  QGB-HOLIDAY-7                 PIC 9(4).
           03  QGB-HOLIDAY-8                 PIC 9(4).
           03  QGB-HOLIDAY-9                 PIC 9(4).
           03  QGB-HOLIDAY-10                PIC 9(4).
           03  QGB-HOLIDAY-11                PIC 9(4).
           03  QGB-HOLIDAY-12                PIC 9(4).
           03  QGB-HOLIDAY-13                PIC 9(4).
           03  QGB-HOLIDAY-14                PIC 9(4).
           03  QGB-HOLIDAY-15                PIC 9(4).
           03  QGB-HOLIDAY-16                PIC 9(4).
           03  QGB-RPT-LINES                 PIC 99.
           03  QGB-CHECK-PROG                PIC XX.
           03  QGB-OTHER-BRANCH-POST         PIC X.
           03  QGB-LAST-ACCRUE               PIC 9(4).
           03  QGB-AUTO-DEPOSIT              PIC X.
           03  QGB-UUCP-HOME                 PIC X(8).
           03  QGB-EOM-TAG-CCYYMM            PIC 9(6).
           03  QGB-EOM-UPDATE-DATE           PIC X(10).
           03  QGB-EOM-UPDATE-TIME           PIC 9(6).
           03  QGB-EOM-UPDATE-USERID         PIC X(10).
           03  QGB-FISCAL-EOY-TAG-CCYYMM     PIC 9(6).
           03  QGB-EOYF-UPDATE-DATE          PIC X(10).
           03  QGB-EOYF-UPDATE-TIME          PIC 9(6).
           03  QGB-EOYF-UPDATE-USERID        PIC X(10).
           03  QGB-CALENDAR-EOY-TAG-CCYYMM   PIC 9(6).
           03  QGB-EOYC-UPDATE-DATE          PIC X(10).
           03  QGB-EOYC-UPDATE-TIME          PIC 9(6).
           03  QGB-EOYC-UPDATE-USERID        PIC X(10).

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
           03  BEG-NO                  PIC 9.
           03  END-NO                  PIC 9.

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
           03  FILLER PIC X(28) VALUE "BRNO  NO   NAME              ".
           03  FILLER PIC X(30) VALUE "SMALL     CHECK  EOM".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-GB-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-GB-NO                      PIC 9.
           03  FILLER                         PIC X(03).
           03  D01-GB-COMPANY-NAME            PIC X(15).
           03  FILLER                         PIC X(03).
           03  D01-GB-NEXT-SMALL              PIC 9(8).
           03  FILLER                         PIC X(03).
           03  D01-GB-CHECK-PROG              PIC XX.
           03  FILLER                         PIC X(03).
           03  D01-GB-EOM-TAG-CCYYMM          PIC 9(6).
           

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
       COPY "LIBIO/IOGBRN_DEF.CPY".
       COPY "LIBIO/IOGBRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOGBRN_SCN.CPY".

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
           MOVE "IOGBRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOGBRN-QUEUE-POS  TO Q-POS.
           MOVE IOGBRN-COPIES-POS TO C-POS.
           MOVE IOGBRN-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-NO.

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
              ( VDUNUM0 > 1      ) 
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-BEG-NO.

           MOVE "ENNN010BEG-NO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-NO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-NO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-NO.

      *-----------------------------------------------------------------
       ENTER-END-NO.

           MOVE "ENNN010END-NO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-NO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-NO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-NO.

           IF ( BEG-NO > END-NO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-NO.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-NO.

           MOVE "SNNN010BEG-NO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-NO.
           PERFORM SCREENIO.

           MOVE "SNNN010END-NO." TO VDU.
           MOVE 9                TO VDUNUM0  END-NO.
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

           MOVE "SNNN010BEG-NO."       TO VDU.
           MOVE BEG-NO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN010END-NO."       TO VDU.
           MOVE END-NO                 TO VDUNUM0.
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

           MOVE "B"                        TO GB-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QGB-WBEG-BRNO.
           MOVE BR-NO     TO QGB-WEND-BRNO.

           MOVE BEG-NO  TO QGB-WBEG-NO.
           MOVE END-NO  TO QGB-WEND-NO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-GB-FILE.
           PERFORM START-GB-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-GB-FILE-NEXT.
           IF ( IO-BAD ) OR ( GB-BRNO NOT = GB-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( GB-NO < BEG-NO )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( GB-NO > END-NO )
              GO TO PROCESS-BRANCH-MODULE-END.


      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE GB-BRNO                  TO D01-GB-BRNO.
           MOVE GB-NO                    TO D01-GB-NO.
           MOVE GB-COMPANY-NAME          TO D01-GB-COMPANY-NAME.
           MOVE GB-NEXT-SMALL            TO D01-GB-NEXT-SMALL.
           MOVE GB-CHECK-PROG            TO D01-GB-CHECK-PROG.
           MOVE GB-EOM-TAG-CCYYMM        TO D01-GB-EOM-TAG-CCYYMM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-GB-FILE.

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

           MOVE GB-KEY TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING NO               : "    TO R01-PROMPT.
           MOVE BEG-NO                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    NO               : "    TO R01-PROMPT.
           MOVE END-NO                             TO R01-NUMERIC.
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
           MOVE "SQL TABLE IO UTILITY - GBFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOGBRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOGBRN-SYSDATE.
           DISPLAY IOGBRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOGBRN-FORM-FIELDS TO WR-BUF.
           MOVE IOGBRN-FORM-LIST TO WR-LIST.
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

           MOVE IOGBRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOGBRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOGBRN_EVA.CPY".
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
      *    LOAD GB-REC FROM QGB-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-GB-FIELDS.
           MOVE QGB-BRNO TO GB-BRNO.
           MOVE QGB-NO TO GB-NO.

           IF ( QGB-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO GB-LTOUCH-DATE
           ELSE
              MOVE QGB-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GB-LTOUCH-DATE.

           MOVE QGB-COMPANY-NAME TO GB-COMPANY-NAME.
           MOVE QGB-ADDR-1 TO GB-ADDR-1.
           MOVE QGB-ADDR-2 TO GB-ADDR-2.
           MOVE QGB-CITY TO GB-CITY.
           MOVE QGB-STATE TO GB-STATE.
           MOVE QGB-ZIP TO GB-ZIP.
           MOVE QGB-PHONE TO GB-PHONE.
           MOVE QGB-NEXT-SF TO GB-NEXT-SF.
           MOVE QGB-NEXT-SMALL TO GB-NEXT-SMALL.
           MOVE QGB-NEXT-OTHER TO GB-NEXT-OTHER.
           MOVE QGB-HOLIDAY-1 TO GB-HOLIDAY(1).
           MOVE QGB-HOLIDAY-2 TO GB-HOLIDAY(2).
           MOVE QGB-HOLIDAY-3 TO GB-HOLIDAY(3).
           MOVE QGB-HOLIDAY-4 TO GB-HOLIDAY(4).
           MOVE QGB-HOLIDAY-5 TO GB-HOLIDAY(5).
           MOVE QGB-HOLIDAY-6 TO GB-HOLIDAY(6).
           MOVE QGB-HOLIDAY-7 TO GB-HOLIDAY(7).
           MOVE QGB-HOLIDAY-8 TO GB-HOLIDAY(8).
           MOVE QGB-HOLIDAY-9 TO GB-HOLIDAY(9).
           MOVE QGB-HOLIDAY-10 TO GB-HOLIDAY(10).
           MOVE QGB-HOLIDAY-11 TO GB-HOLIDAY(11).
           MOVE QGB-HOLIDAY-12 TO GB-HOLIDAY(12).
           MOVE QGB-HOLIDAY-13 TO GB-HOLIDAY(13).
           MOVE QGB-HOLIDAY-14 TO GB-HOLIDAY(14).
           MOVE QGB-HOLIDAY-15 TO GB-HOLIDAY(15).
           MOVE QGB-HOLIDAY-16 TO GB-HOLIDAY(16).
           MOVE QGB-RPT-LINES TO GB-RPT-LINES.
           MOVE QGB-CHECK-PROG TO GB-CHECK-PROG.
           MOVE QGB-OTHER-BRANCH-POST TO GB-OTHER-BRANCH-POST.
           MOVE QGB-LAST-ACCRUE TO GB-LAST-ACCRUE.
           MOVE QGB-AUTO-DEPOSIT TO GB-AUTO-DEPOSIT.
           MOVE QGB-UUCP-HOME TO GB-UUCP-HOME.
           MOVE QGB-EOM-TAG-CCYYMM TO GB-EOM-TAG-CCYYMM.

           IF ( QGB-EOM-UPDATE-DATE = "1900-01-01" )
              MOVE ZEROES TO GB-EOM-UPDATE-DATE
           ELSE
              MOVE QGB-EOM-UPDATE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GB-EOM-UPDATE-DATE.

           MOVE QGB-EOM-UPDATE-TIME TO GB-EOM-UPDATE-TIME.
           MOVE QGB-EOM-UPDATE-USERID TO GB-EOM-UPDATE-USERID.

           MOVE QGB-FISCAL-EOY-TAG-CCYYMM TO GB-FISCAL-EOY-TAG-CCYYMM.

           IF ( QGB-EOYF-UPDATE-DATE = "1900-01-01" )
              MOVE ZEROES TO GB-EOYF-UPDATE-DATE
           ELSE
              MOVE QGB-EOYF-UPDATE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GB-EOYF-UPDATE-DATE.

           MOVE QGB-EOYF-UPDATE-TIME TO GB-EOYF-UPDATE-TIME.
           MOVE QGB-EOYF-UPDATE-USERID TO GB-EOYF-UPDATE-USERID.

           MOVE QGB-CALENDAR-EOY-TAG-CCYYMM
              TO GB-CALENDAR-EOY-TAG-CCYYMM.

           IF ( QGB-EOYC-UPDATE-DATE = "1900-01-01" )
              MOVE ZEROES TO GB-EOYC-UPDATE-DATE
           ELSE
              MOVE QGB-EOYC-UPDATE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GB-EOYC-UPDATE-DATE.

           MOVE QGB-EOYC-UPDATE-TIME TO GB-EOYC-UPDATE-TIME.
           MOVE QGB-EOYC-UPDATE-USERID TO GB-EOYC-UPDATE-USERID.

      *-----------------------------------------------------------------
      *    LOAD QGB-REC FROM GB-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-GB-FIELDS.
           MOVE GB-BRNO TO QGB-BRNO.
           MOVE GB-NO TO QGB-NO.

           IF ( GB-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QGB-LTOUCH-DATE
           ELSE
              MOVE GB-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGB-LTOUCH-DATE.

           MOVE GB-COMPANY-NAME TO QGB-COMPANY-NAME.
           MOVE GB-ADDR-1 TO QGB-ADDR-1.
           MOVE GB-ADDR-2 TO QGB-ADDR-2.
           MOVE GB-CITY TO QGB-CITY.
           MOVE GB-STATE TO QGB-STATE.
           MOVE GB-ZIP TO QGB-ZIP.
           MOVE GB-PHONE TO QGB-PHONE.
           MOVE GB-NEXT-SF TO QGB-NEXT-SF.
           MOVE GB-NEXT-SMALL TO QGB-NEXT-SMALL.
           MOVE GB-NEXT-OTHER TO QGB-NEXT-OTHER.
           MOVE GB-HOLIDAY(1) TO QGB-HOLIDAY-1.
           MOVE GB-HOLIDAY(2) TO QGB-HOLIDAY-2.
           MOVE GB-HOLIDAY(3) TO QGB-HOLIDAY-3.
           MOVE GB-HOLIDAY(4) TO QGB-HOLIDAY-4.
           MOVE GB-HOLIDAY(5) TO QGB-HOLIDAY-5.
           MOVE GB-HOLIDAY(6) TO QGB-HOLIDAY-6.
           MOVE GB-HOLIDAY(7) TO QGB-HOLIDAY-7.
           MOVE GB-HOLIDAY(8) TO QGB-HOLIDAY-8.
           MOVE GB-HOLIDAY(9) TO QGB-HOLIDAY-9.
           MOVE GB-HOLIDAY(10) TO QGB-HOLIDAY-10.
           MOVE GB-HOLIDAY(11) TO QGB-HOLIDAY-11.
           MOVE GB-HOLIDAY(12) TO QGB-HOLIDAY-12.
           MOVE GB-HOLIDAY(13) TO QGB-HOLIDAY-13.
           MOVE GB-HOLIDAY(14) TO QGB-HOLIDAY-14.
           MOVE GB-HOLIDAY(15) TO QGB-HOLIDAY-15.
           MOVE GB-HOLIDAY(16) TO QGB-HOLIDAY-16.
           MOVE GB-RPT-LINES TO QGB-RPT-LINES.
           MOVE GB-CHECK-PROG TO QGB-CHECK-PROG.
           MOVE GB-OTHER-BRANCH-POST TO QGB-OTHER-BRANCH-POST.
           MOVE GB-LAST-ACCRUE TO QGB-LAST-ACCRUE.
           MOVE GB-AUTO-DEPOSIT TO QGB-AUTO-DEPOSIT.
           MOVE GB-UUCP-HOME TO QGB-UUCP-HOME.
           MOVE GB-EOM-TAG-CCYYMM TO QGB-EOM-TAG-CCYYMM.

           IF ( GB-EOM-UPDATE-DATE = ZEROES )
              MOVE "1900-01-01" TO QGB-EOM-UPDATE-DATE
           ELSE
              MOVE GB-EOM-UPDATE-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGB-EOM-UPDATE-DATE.

           MOVE GB-EOM-UPDATE-TIME TO QGB-EOM-UPDATE-TIME.
           MOVE GB-EOM-UPDATE-USERID TO QGB-EOM-UPDATE-USERID.

           MOVE GB-FISCAL-EOY-TAG-CCYYMM TO QGB-FISCAL-EOY-TAG-CCYYMM.

           IF ( GB-EOYF-UPDATE-DATE = ZEROES )
              MOVE "1900-01-01" TO QGB-EOYF-UPDATE-DATE
           ELSE
              MOVE GB-EOYF-UPDATE-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGB-EOYF-UPDATE-DATE.

           MOVE GB-EOYF-UPDATE-TIME TO QGB-EOYF-UPDATE-TIME.
           MOVE GB-EOYF-UPDATE-USERID TO QGB-EOYF-UPDATE-USERID.

           MOVE GB-CALENDAR-EOY-TAG-CCYYMM
            TO QGB-CALENDAR-EOY-TAG-CCYYMM.

           IF ( GB-EOYC-UPDATE-DATE = ZEROES )
              MOVE "1900-01-01" TO QGB-EOYC-UPDATE-DATE
           ELSE
              MOVE GB-EOYC-UPDATE-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGB-EOYC-UPDATE-DATE.

           MOVE GB-EOYC-UPDATE-TIME TO QGB-EOYC-UPDATE-TIME.
           MOVE GB-EOYC-UPDATE-USERID TO QGB-EOYC-UPDATE-USERID.

 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-GB-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO GB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GB-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO GB-ALL-DATA
              MOVE GB-ALL-PATH             TO GB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO GB-ALL-BASE
              MOVE FILPATH-MACHINE         TO GB-ALL-MACHINE
              MOVE FILPATH-DATA            TO GB-ALL-DATA
              MOVE GB-ALL-PATH             TO GB-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-GBFILE NOT = "S" )
              MOVE GB-PATH-BEG             TO GB-SELECT-PATH-BEG
              MOVE GB-PATH-FILE            TO GB-SELECT-PATH-FILE
           ELSE
              MOVE GB-PATH-SQL             TO GB-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-GB-FILE.
           PERFORM LOAD-GB-FILE.
           PERFORM OPEN-IT.
           MOVE GB-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GB-FILE.
           PERFORM START-IT.
           MOVE GB-PATH-OWNBR  TO GB-BRNO.
           MOVE GB-SELECT-PATH TO E-FILE.
           MOVE GB-KEY         TO E-KEYX.

           IF ( GB-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GB-FILE.

           IF ( QGB-WEND-NO = ZEROES    ) OR
              ( QGB-WEND-NO NOT NUMERIC )
              MOVE GB-PATH-OWNBR    TO QGB-WBEG-BRNO
              MOVE GB-PATH-OWNBR    TO QGB-WEND-BRNO
              MOVE GB-NO            TO QGB-WBEG-NO
              MOVE 9                TO QGB-WEND-NO.

           EXEC SQL
            DECLARE GB_CURSOR CURSOR FOR
            SELECT GBFILE.GB_BRNO,
                   GBFILE.GB_NO,
                   ISNULL(CAST(GBFILE.GB_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(GBFILE.GB_COMPANY_NAME,''),
                   ISNULL(GBFILE.GB_ADDR_1,''),
                   ISNULL(GBFILE.GB_ADDR_2,''),
                   ISNULL(GBFILE.GB_CITY,''),
                   ISNULL(GBFILE.GB_STATE,''),
                   ISNULL(GBFILE.GB_ZIP,''),
                   GBFILE.GB_PHONE,
                   GBFILE.GB_NEXT_SF,
                   GBFILE.GB_NEXT_SMALL,
                   GBFILE.GB_NEXT_OTHER,
                   GBFILE.GB_HOLIDAY_1,
                   GBFILE.GB_HOLIDAY_2,
                   GBFILE.GB_HOLIDAY_3,
                   GBFILE.GB_HOLIDAY_4,
                   GBFILE.GB_HOLIDAY_5,
                   GBFILE.GB_HOLIDAY_6,
                   GBFILE.GB_HOLIDAY_7,
                   GBFILE.GB_HOLIDAY_8,
                   GBFILE.GB_HOLIDAY_9,
                   GBFILE.GB_HOLIDAY_10,
                   GBFILE.GB_HOLIDAY_11,
                   GBFILE.GB_HOLIDAY_12,
                   GBFILE.GB_HOLIDAY_13,
                   GBFILE.GB_HOLIDAY_14,
                   GBFILE.GB_HOLIDAY_15,
                   GBFILE.GB_HOLIDAY_16,
                   GBFILE.GB_RPT_LINES,
                   ISNULL(GBFILE.GB_CHECK_PROG,''),
                   ISNULL(GBFILE.GB_OTHER_BRANCH_POST,''),
                   GBFILE.GB_LAST_ACCRUE,
                   ISNULL(GBFILE.GB_AUTO_DEPOSIT,''),
                   ISNULL(GBFILE.GB_UUCP_HOME,''),
                   GBFILE.GB_EOM_TAG_CCYYMM,
                   ISNULL(CAST(GBFILE.GB_EOM_UPDATE_DATE
                                                  AS VARCHAR(10)),'0'),
                   GBFILE.GB_EOM_UPDATE_TIME,
                   ISNULL(GBFILE.GB_EOM_UPDATE_USERID,''),
                   GBFILE.GB_FISCAL_EOY_TAG_CCYYMM,
                   ISNULL(CAST(GBFILE.GB_EOYF_UPDATE_DATE
                                                  AS VARCHAR(10)),'0'),
                   GBFILE.GB_EOYF_UPDATE_TIME,
                   ISNULL(GBFILE.GB_EOYF_UPDATE_USERID,''),
                   GBFILE.GB_CALENDAR_EOY_TAG_CCYYMM,
                   ISNULL(CAST(GBFILE.GB_EOYC_UPDATE_DATE
                                                  AS VARCHAR(10)),'0'),
                   GBFILE.GB_EOYC_UPDATE_TIME,
                   ISNULL(GBFILE.GB_EOYC_UPDATE_USERID,'')
            FROM DBO.GBFILE
            WHERE GBFILE.GB_BRNO >= :QGB-WBEG-BRNO
              AND GBFILE.GB_BRNO <= :QGB-WEND-BRNO
              AND GBFILE.GB_NO >= :QGB-WBEG-NO
              AND GBFILE.GB_NO <= :QGB-WEND-NO
            ORDER BY GBFILE.GB_BRNO,
                     GBFILE.GB_NO
           END-EXEC.

           EXEC SQL
               OPEN GB_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QGB-WBEG-BRNO 
                          QGB-WEND-BRNO
                          QGB-WBEG-NO
                          QGB-WEND-NO.

           MOVE STAT---GOOD TO GB-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GB-FILE.
           PERFORM READ-IT.
           MOVE GB-PATH-OWNBR  TO GB-BRNO. 
           MOVE GB-SELECT-PATH TO E-FILE.
           MOVE GB-KEY         TO E-KEYX.

           MOVE GB-BRNO   TO QGB-BRNO.
           MOVE GB-NO     TO QGB-NO.

           EXEC SQL
            SELECT GBFILE.GB_BRNO,
                   GBFILE.GB_NO,
                   ISNULL(CAST(GBFILE.GB_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(GBFILE.GB_COMPANY_NAME,''),
                   ISNULL(GBFILE.GB_ADDR_1,''),
                   ISNULL(GBFILE.GB_ADDR_2,''),
                   ISNULL(GBFILE.GB_CITY,''),
                   ISNULL(GBFILE.GB_STATE,''),
                   ISNULL(GBFILE.GB_ZIP,''),
                   GBFILE.GB_PHONE,
                   GBFILE.GB_NEXT_SF,
                   GBFILE.GB_NEXT_SMALL,
                   GBFILE.GB_NEXT_OTHER,
                   GBFILE.GB_HOLIDAY_1,
                   GBFILE.GB_HOLIDAY_2,
                   GBFILE.GB_HOLIDAY_3,
                   GBFILE.GB_HOLIDAY_4,
                   GBFILE.GB_HOLIDAY_5,
                   GBFILE.GB_HOLIDAY_6,
                   GBFILE.GB_HOLIDAY_7,
                   GBFILE.GB_HOLIDAY_8,
                   GBFILE.GB_HOLIDAY_9,
                   GBFILE.GB_HOLIDAY_10,
                   GBFILE.GB_HOLIDAY_11,
                   GBFILE.GB_HOLIDAY_12,
                   GBFILE.GB_HOLIDAY_13,
                   GBFILE.GB_HOLIDAY_14,
                   GBFILE.GB_HOLIDAY_15,
                   GBFILE.GB_HOLIDAY_16,
                   GBFILE.GB_RPT_LINES,
                   ISNULL(GBFILE.GB_CHECK_PROG,''),
                   ISNULL(GBFILE.GB_OTHER_BRANCH_POST,''),
                   GBFILE.GB_LAST_ACCRUE,
                   ISNULL(GBFILE.GB_AUTO_DEPOSIT,''),
                   ISNULL(GBFILE.GB_UUCP_HOME,''),
                   GBFILE.GB_EOM_TAG_CCYYMM,
                   ISNULL(CAST(GBFILE.GB_EOM_UPDATE_DATE
                                                  AS VARCHAR(10)),'0'),
                   GBFILE.GB_EOM_UPDATE_TIME,
                   ISNULL(GBFILE.GB_EOM_UPDATE_USERID,''),
                   GBFILE.GB_FISCAL_EOY_TAG_CCYYMM,
                   ISNULL(CAST(GBFILE.GB_EOYF_UPDATE_DATE
                                                  AS VARCHAR(10)),'0'),
                   GBFILE.GB_EOYF_UPDATE_TIME,
                   ISNULL(GBFILE.GB_EOYF_UPDATE_USERID,''),
                   GBFILE.GB_CALENDAR_EOY_TAG_CCYYMM,
                   ISNULL(CAST(GBFILE.GB_EOYC_UPDATE_DATE
                                                  AS VARCHAR(10)),'0'),
                   GBFILE.GB_EOYC_UPDATE_TIME,
                   ISNULL(GBFILE.GB_EOYC_UPDATE_USERID,'')
            INTO :QGB-REC
            FROM DBO.GBFILE
            WHERE GBFILE.GB_BRNO = :QGB-BRNO
              AND GBFILE.GB_NO = :QGB-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GB-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GB-FIELDS.

      *-----------------------------------------------------------------
       READ-GB-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GB-PATH-OWNBR  TO GB-BRNO.
           MOVE GB-SELECT-PATH TO E-FILE.
           MOVE GB-KEY         TO E-KEYX.
           INITIALIZE QGB-REC.

           IF ( GB-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GB_CURSOR INTO :QGB-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GB-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GB-FILE.

           IF ( GB-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GB_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GB-CURSOR-STAT.

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
              DESTROY IOGBRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
