       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOCQRN.
       AUTHOR.        BAH
       DATE-WRITTEN.  04-16-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOCQRN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : CQFILE 
      * TABLE DESC    : COLLECTOR QUEUE FILE
      * OUTPUT FILE   : XPC/IOCQRN.PRE
      * USED FOR      : LIBSP/SPWSCQ.CPY  --- QCQ-REC
      *                 LIBSP/SPCQ1.CPY  --- READ
      *                 LIBSP/SPCQ1RN.CPY --- READ/READ-NEXT/START
      *                 LIBSP/SPCQ2RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0416 ***NEW***
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)IO/IOCQRN S34 06/30/21-10:47:38 >".

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
       COPY "LIBSP/SP01CQ.CPY".
       COPY "LIBSP/SPWSCQ.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QCQ1-WHERE-BEG-KEY.
           03  QCQ1-WBEG-BRNO             PIC 9(04).
           03  QCQ1-WBEG-ACCTNO           PIC 9(08).
 
       01  QCQ1-WHERE-END-KEY.
           03  QCQ1-WEND-BRNO             PIC 9(04).
           03  QCQ1-WEND-ACCTNO           PIC 9(08).

       01  QCQ2-WHERE-BEG-KEY.
           03  QCQ2-WBEG-BRNO              PIC 9(04).
           03  QCQ2-WBEG-COLLID            PIC X(03).
           03  QCQ2-WBEG-REV-BEHAVIOR-RISK PIC 9.
           03  QCQ2-WBEG-SEQ-FG            PIC X.
           03  QCQ2-WBEG-DATE              PIC X(10).
           03  QCQ2-WBEG-START-TIME        PIC S9(04).
           03  QCQ2-WBEG-REV-DEL           PIC 9(05).
           03  QCQ2-WBEG-MISC-KEY-FLD      PIC X(03).
           03  QCQ2-WBEG-REV-PDUE-DAYS     PIC 9(05).
           03  QCQ2-WBEG-REV-BAL           PIC 9(07).
           03  QCQ2-WBEG-SEQ               PIC 9(03).
 
       01  QCQ2-WHERE-END-KEY.
           03  QCQ2-WEND-BRNO              PIC 9(04).
           03  QCQ2-WEND-COLLID            PIC X(03).
           03  QCQ2-WEND-REV-BEHAVIOR-RISK PIC 9.
           03  QCQ2-WEND-SEQ-FG            PIC X.
           03  QCQ2-WEND-DATE              PIC X(10).
           03  QCQ2-WEND-START-TIME        PIC S9(04).
           03  QCQ2-WEND-REV-DEL           PIC 9(05).
           03  QCQ2-WEND-MISC-KEY-FLD      PIC X(03).
           03  QCQ2-WEND-REV-PDUE-DAYS     PIC 9(05).
           03  QCQ2-WEND-REV-BAL           PIC 9(07).
           03  QCQ2-WEND-SEQ               PIC 9(03).
 
       01  QCQ-REC.
           03  QCQ-BRNO              PIC 9(4).
           03  QCQ-ACCTNO            PIC 9(8).
           03  QCQ-COLLID            PIC XXX.
           03  QCQ-REV-BEHAVIOR-RISK PIC 9.
           03  QCQ-SEQ-FG            PIC X.
           03  QCQ-DATE              PIC X(10).
           03  QCQ-START-TIME        PIC S9(4).
           03  QCQ-REV-DEL           PIC 9(5).
           03  QCQ-MISC-KEY-FLD      PIC X(3).
           03  QCQ-REV-PDUE-DAYS     PIC 9(5).
           03  QCQ-REV-BAL           PIC 9(7).
           03  QCQ-SEQ               PIC 9(3).
           03  QCQ-LTOUCH-DATE       PIC X(10).
           03  QCQ-WORK-COLLID       PIC XXX.
           03  QCQ-WORKED-FG         PIC X.
           03  QCQ-IN-PROCESS        PIC X.
           03  QCQ-DATE-QUEUED       PIC X(10).
           03  QCQ-TIME-ZONE-ERROR   PIC X.

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
           03  BEG-ACCTNO              PIC 9(08).
           03  END-ACCTNO              PIC 9(08).
           03  BEG-COLLID              PIC X(03).
           03  END-COLLID              PIC X(03).
           03  BEG-BEHAVIOR            PIC 9.
           03  END-BEHAVIOR            PIC 9.
           03  BEG-SEQ-FG            PIC X.
           03  END-SEQ-FG            PIC X.
           03  BEG-DATE                PIC 9(08).
           03  END-DATE                PIC 9(08).
           03  BEG-TIME                PIC 9(04).
           03  END-TIME                PIC 9(04).

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
           03  FILLER PIC X(30) VALUE "BRNO     ACCTNO  COLLID  BEHAV".
           03  FILLER PIC X(30) VALUE "  SEQ   DATE        TIME   REV".
           03  FILLER PIC X(30) VALUE "DEL".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-CQ-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-CQ-ACCTNO                  PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-CQ-COLLID                  PIC X(03).
           03  FILLER                         PIC X(06).
           03  D01-CQ-REV-BEHAVIOR-RISK       PIC 9.
           03  FILLER                         PIC X(05).
           03  D01-CQ-SEQ-FG                  PIC X.
           03  FILLER                         PIC X(03).
           03  D01-CQ-DATE                    PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-CQ-START-TIME              PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-CQ-REV-DEL                 PIC 9(05).

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
       COPY "LIBIO/IOCQRN_DEF.CPY".
       COPY "LIBIO/IOCQRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOCQRN_SCN.CPY".

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
      D    STOP FORM-PATHNAME.

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
           MOVE "IOCQRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOCQRN-QUEUE-POS  TO Q-POS.
           MOVE IOCQRN-COPIES-POS TO C-POS.
           MOVE IOCQRN-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-TIME.

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

      *=================================================================
       ENTER-BEG-ACCTNO.

           MOVE "ENNN080BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-ACCTNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-END-ACCTNO.

           MOVE "ENNN080END-ACCTNO." TO VDU.
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

           GO TO ENTER-BEG-COLLID.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN080BEG-ACCTNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-ACCTNO.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO." TO VDU.
           MOVE 99999999            TO VDUNUM0  END-ACCTNO.
           PERFORM SCREENIO.

      *========================================================
       ENTER-BEG-COLLID.

           MOVE "E  A030BEG-COLLID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-COLLID.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-COLLID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-COLLID.

      *-----------------------------------------------------------------
       ENTER-END-COLLID.

           MOVE "E  A030END-COLLID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-COLLID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-COLLID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-COLLID.

           IF ( BEG-COLLID > END-COLLID )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-COLLID.

           GO TO ENTER-BEG-BEHAVIOR.

      *-----------------------------------------------------------------
       ENTER-ALL-COLLID.

           MOVE "S  A030BEG-COLLID." TO VDU.
           MOVE SPACES               TO VDUBUF  BEG-COLLID.
           PERFORM SCREENIO.

           MOVE "S  A030END-COLLID." TO VDU.
           MOVE ALL "Z"              TO VDUBUF  END-COLLID.
           CALL "C$TOLOWER" USING END-COLLID, VALUE 3.
           MOVE END-COLLID TO VDUBUF.
           PERFORM SCREENIO.

      *================================================
       ENTER-BEG-BEHAVIOR.

           MOVE "ENNN010BEG-BEHAVE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-COLLID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BEHAVIOR.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BEHAVIOR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-BEHAVIOR.

      *-----------------------------------------------------------------
       ENTER-END-BEHAVIOR.

           MOVE "ENNN010END-BEHAVE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BEHAVIOR.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BEHAVIOR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BEHAVIOR.

           IF ( BEG-BEHAVIOR > END-BEHAVIOR )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BEHAVIOR.

           GO TO ENTER-BEG-SEQ-FG.

      *-----------------------------------------------------------------
       ENTER-ALL-BEHAVIOR.

           MOVE "SNNN010BEG-BEHAVE." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-BEHAVIOR.
           PERFORM SCREENIO.

           MOVE "SNNN010END-BEHAVE." TO VDU.
           MOVE ALL "9"            TO VDUNUM0  END-BEHAVIOR.
           PERFORM SCREENIO.

      *========================================================
       ENTER-BEG-SEQ-FG.

           MOVE "E  A010BEG-SEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BEHAVIOR.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SEQ-FG.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SEQ-FG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-SEQ-FG.

      *-----------------------------------------------------------------
       ENTER-END-SEQ-FG.

           MOVE "E  A010END-SEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SEQ-FG.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SEQ-FG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-SEQ-FG.

           IF ( BEG-SEQ-FG > END-SEQ-FG )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SEQ-FG.

           GO TO ENTER-BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-ALL-SEQ-FG.

           MOVE "S  A010BEG-SEQ." TO VDU.
           MOVE SPACES            TO VDUBUF  BEG-SEQ-FG.
           PERFORM SCREENIO.

           MOVE "S  A010END-SEQ." TO VDU.
           MOVE ALL "Z"              TO VDUBUF  END-SEQ-FG.
           CALL "C$TOLOWER" USING END-SEQ-FG, VALUE 1.
           MOVE END-SEQ-FG TO VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-DATE.

           MOVE SPACES  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SEQ-FG.
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

           GO TO ENTER-BEG-TIME.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "S  8080BEG-DATE." TO VDU.
           MOVE JULIAN-BEG         TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE." TO VDU.
           MOVE JULIAN-END         TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           PERFORM SCREENIO.

      *================================================
       ENTER-BEG-TIME.

           MOVE "ENNN040BEG-TIME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-COLLID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-TIME.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-TIME.

      *-----------------------------------------------------------------
       ENTER-END-TIME.

           MOVE "ENNN040END-TIME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-TIME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-TIME.

           IF ( BEG-TIME > END-TIME )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-TIME.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-TIME.

           MOVE "SNNN040BEG-TIME." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-TIME.
           PERFORM SCREENIO.

           MOVE "SNNN040END-TIME." TO VDU.
           MOVE ALL "9"            TO VDUNUM0  END-TIME.
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


           MOVE "SNNN080BEG-ACCTNO."      TO VDU.
           MOVE BEG-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO."      TO VDU.
           MOVE END-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A030BEG-COLLID."      TO VDU.
           MOVE BEG-COLLID                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A030END-COLLID."      TO VDU.
           MOVE END-COLLID                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN010BEG-BEHAVE."      TO VDU.
           MOVE BEG-BEHAVIOR                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN010END-BEHAVE."      TO VDU.
           MOVE END-BEHAVIOR                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010BEG-SEQ."      TO VDU.
           MOVE BEG-SEQ-FG                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010END-SEQ."      TO VDU.
           MOVE END-SEQ-FG                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-DATE."    TO VDU.
           MOVE BEG-DATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A000END-DATE."    TO VDU.
           MOVE END-DATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "SNNN040.BEG-TIME."      TO VDU.
           MOVE BEG-TIME                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040.END-TIME."      TO VDU.
           MOVE END-TIME                TO VDUNUM0.
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

           MOVE "B"                        TO CQ-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }

           MOVE BR-NO     TO QCQ1-WBEG-BRNO 
                              QCQ2-WBEG-BRNO.
           MOVE BR-NO     TO QCQ1-WEND-BRNO
                              QCQ2-WEND-BRNO.

           MOVE BEG-ACCTNO TO QCQ1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QCQ1-WEND-ACCTNO.

           MOVE BEG-COLLID  TO QCQ2-WBEG-COLLID.
           MOVE END-COLLID  TO QCQ2-WEND-COLLID.
           MOVE BEG-BEHAVIOR TO QCQ2-WBEG-REV-BEHAVIOR-RISK.
           MOVE END-BEHAVIOR TO QCQ2-WEND-REV-BEHAVIOR-RISK.
           MOVE BEG-SEQ-FG  TO QCQ2-WBEG-SEQ-FG.
           MOVE END-SEQ-FG  TO QCQ2-WEND-SEQ-FG.

           MOVE BEG-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCQ2-WBEG-DATE.
           MOVE END-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCQ2-WEND-DATE.


           MOVE BEG-TIME  TO QCQ2-WBEG-START-TIME.
           MOVE END-TIME  TO QCQ2-WEND-START-TIME.

      * I DIDNT PUT THESE ON THE SCREEN ENTRY
           MOVE SPACES    TO QCQ2-WBEG-MISC-KEY-FLD.
           MOVE 0         TO QCQ2-WBEG-REV-DEL
                             QCQ2-WBEG-REV-PDUE-DAYS
                             QCQ2-WBEG-REV-BAL
                             QCQ2-WBEG-SEQ.
           MOVE ALL "Z"   TO QCQ2-WEND-MISC-KEY-FLD.
           MOVE ALL "9"   TO QCQ2-WEND-REV-DEL
                             QCQ2-WEND-REV-PDUE-DAYS
                             QCQ2-WEND-REV-BAL
                             QCQ2-WEND-SEQ.
                      

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-CQ1-FILE
              PERFORM START-CQ1-FILE
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM OPEN-CQ2-FILE
              PERFORM START-CQ2-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-CQ1-FILE-NEXT
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM READ-CQ2-FILE-NEXT.
           IF ( IO-BAD ) OR ( CQ-BRNO NOT = CQ-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( CQ-ACCTNO < BEG-ACCTNO )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( CQ-ACCTNO > END-ACCTNO )
                 GO TO PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 2 )
              IF ( CQ-COLLID  < BEG-COLLID  )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( CQ-COLLID  > END-COLLID  )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
              IF ( CQ-REV-BEHAVIOR-RISK < BEG-BEHAVIOR OR
                                        > END-BEHAVIOR )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( CQ-SEQ-FG < BEG-SEQ-FG OR > END-SEQ-FG )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( CQ-DATE < BEG-DATE OR > END-DATE )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( CQ-START-TIME < BEG-TIME OR > END-TIME )
                 GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE CQ-BRNO               TO D01-CQ-BRNO.
           MOVE CQ-ACCTNO             TO D01-CQ-ACCTNO.
           MOVE CQ-COLLID             TO D01-CQ-COLLID.
           MOVE CQ-REV-BEHAVIOR-RISK  TO D01-CQ-REV-BEHAVIOR-RISK.
           MOVE CQ-SEQ-FG             TO D01-CQ-SEQ-FG.
           MOVE CQ-DATE               TO D01-CQ-DATE.
           MOVE CQ-START-TIME         TO D01-CQ-START-TIME.
           MOVE CQ-REV-DEL            TO D01-CQ-REV-DEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 1 )
              PERFORM CLOSE-CQ1-FILE
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM CLOSE-CQ2-FILE.

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
              MOVE CQ1-KEY TO PACE-DETAIL-KEY
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM LOAD-CQ2-NON-COMP-KEY
              MOVE SPWSCQ-CQ2-KEY-NON-COMP TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING ACCTNO              : "    TO R01-PROMPT.
           MOVE BEG-ACCTNO                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ACCTNO              : "    TO R01-PROMPT.
           MOVE END-ACCTNO                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE "BEGINNING COLLID              : "    TO R01-PROMPT.
           MOVE BEG-COLLID                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    COLLID              : "    TO R01-PROMPT.
           MOVE END-COLLID                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING BEHAVIOR              : "    TO R01-PROMPT.
           MOVE BEG-BEHAVIOR                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    BEHAVIOR              : "    TO R01-PROMPT.
           MOVE END-BEHAVIOR                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SEQ-FG              : "    TO R01-PROMPT.
           MOVE BEG-SEQ-FG                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SEQ-FG              : "    TO R01-PROMPT.
           MOVE END-SEQ-FG                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


           MOVE "BEGINNING DATE              : "    TO R01-PROMPT.
           MOVE BEG-DATE                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE              : "    TO R01-PROMPT.
           MOVE END-DATE                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING TIME              : "    TO R01-PROMPT.
           MOVE BEG-TIME                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    TIME              : "    TO R01-PROMPT.
           MOVE END-TIME                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
           MOVE "SQL TABLE IO UTILITY - CQFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOCQRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOCQRN-SYSDATE.
           DISPLAY IOCQRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOCQRN-FORM-FIELDS TO WR-BUF.
           MOVE IOCQRN-FORM-LIST TO WR-LIST.
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

           MOVE IOCQRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOCQRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOCQRN_EVA.CPY".
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
      *    LOAD CQ-REC FROM QCQ-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CQ-FIELDS.
           MOVE QCQ-BRNO TO CQ-BRNO.
           MOVE QCQ-ACCTNO TO CQ-ACCTNO.
           MOVE QCQ-COLLID TO CQ-COLLID.
           MOVE QCQ-REV-BEHAVIOR-RISK TO CQ-REV-BEHAVIOR-RISK.
           MOVE QCQ-SEQ-FG TO CQ-SEQ-FG.

           IF ( QCQ-DATE = "1900-01-01" )
              MOVE ZEROES TO CQ-DATE
           ELSE
              MOVE QCQ-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CQ-DATE.

           MOVE QCQ-START-TIME TO CQ-START-TIME.
           MOVE QCQ-REV-DEL TO CQ-REV-DEL.
           MOVE QCQ-MISC-KEY-FLD TO CQ-MISC-KEY-FLD.
           MOVE QCQ-REV-PDUE-DAYS TO CQ-REV-PDUE-DAYS.
           MOVE QCQ-REV-BAL TO CQ-REV-BAL.
           MOVE QCQ-SEQ TO CQ-SEQ.

           IF ( QCQ-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CQ-LTOUCH-DATE
           ELSE
              MOVE QCQ-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CQ-LTOUCH-DATE.

           MOVE QCQ-WORK-COLLID TO CQ-WORK-COLLID.
           MOVE QCQ-WORKED-FG TO CQ-WORKED-FG.
           MOVE QCQ-IN-PROCESS TO CQ-IN-PROCESS.

           IF ( QCQ-DATE-QUEUED = "1900-01-01" )
              MOVE ZEROES TO CQ-DATE-QUEUED
           ELSE
              MOVE QCQ-DATE-QUEUED TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CQ-DATE-QUEUED.

           MOVE QCQ-TIME-ZONE-ERROR TO CQ-TIME-ZONE-ERROR.
 
      *-----------------------------------------------------------------
      *    LOAD QCQ-REC FROM CQ-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CQ-FIELDS.
           MOVE CQ-BRNO TO QCQ-BRNO.
           MOVE CQ-ACCTNO TO QCQ-ACCTNO.
           MOVE CQ-COLLID TO QCQ-COLLID.
           MOVE CQ-REV-BEHAVIOR-RISK TO QCQ-REV-BEHAVIOR-RISK.
           MOVE CQ-SEQ-FG TO QCQ-SEQ-FG.

           IF ( CQ-DATE = ZEROES )
              MOVE "1900-01-01" TO QCQ-DATE
           ELSE
              MOVE CQ-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCQ-DATE.

           MOVE CQ-START-TIME TO QCQ-START-TIME.
           MOVE CQ-REV-DEL TO QCQ-REV-DEL.
           MOVE CQ-MISC-KEY-FLD TO QCQ-MISC-KEY-FLD.
           MOVE CQ-REV-PDUE-DAYS TO QCQ-REV-PDUE-DAYS.
           MOVE CQ-REV-BAL TO QCQ-REV-BAL.
           MOVE CQ-SEQ TO QCQ-SEQ.

           IF ( CQ-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCQ-LTOUCH-DATE
           ELSE
              MOVE CQ-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCQ-LTOUCH-DATE.

           MOVE CQ-WORK-COLLID TO QCQ-WORK-COLLID.
           MOVE CQ-WORKED-FG TO QCQ-WORKED-FG.
           MOVE CQ-IN-PROCESS TO QCQ-IN-PROCESS.

           IF ( CQ-DATE-QUEUED = ZEROES )
              MOVE "1900-01-01" TO QCQ-DATE-QUEUED
           ELSE
              MOVE CQ-DATE-QUEUED TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCQ-DATE-QUEUED.

           MOVE CQ-TIME-ZONE-ERROR TO QCQ-TIME-ZONE-ERROR.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-CQ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CQ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CQ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CQ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CQ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CQ-ALL-DATA
              MOVE CQ-ALL-PATH             TO CQ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CQ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CQ-ALL-BASE
              MOVE FILPATH-MACHINE         TO CQ-ALL-MACHINE
              MOVE FILPATH-DATA            TO CQ-ALL-DATA
              MOVE CQ-ALL-PATH             TO CQ-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-CQFILE NOT = "S" )
              MOVE CQ-PATH-BEG             TO CQ-SELECT-PATH-BEG
              MOVE CQ-PATH-FILE            TO CQ-SELECT-PATH-FILE
           ELSE
              MOVE CQ-PATH-SQL             TO CQ-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-CQ1-FILE.
           PERFORM LOAD-CQ1-FILE.
           PERFORM OPEN-IT.
           MOVE CQ-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CQ1-FILE.
           PERFORM START-IT.
           MOVE CQ-PATH-OWNBR  TO CQ-BRNO.
           MOVE CQ-SELECT-PATH TO E-FILE.
           MOVE CQ1-KEY        TO E-KEYX.

           IF ( CQ1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CQ1-FILE.

           IF ( QCQ1-WEND-ACCTNO = ZEROES    ) OR
              ( QCQ1-WEND-ACCTNO NOT NUMERIC )
              MOVE CQ-PATH-OWNBR        TO QCQ1-WBEG-BRNO
              MOVE CQ-PATH-OWNBR        TO QCQ1-WEND-BRNO
              MOVE CQ-ACCTNO            TO QCQ1-WBEG-ACCTNO
              MOVE ALL "9"              TO QCQ1-WEND-ACCTNO.

           EXEC SQL
            DECLARE CQ1_CURSOR CURSOR FOR
            SELECT CQFILE.CQ_BRNO,
                   CQFILE.CQ_ACCTNO,
                   ISNULL(CQFILE.CQ_COLLID,''),
                   CQFILE.CQ_REV_BEHAVIOR_RISK,
                   ISNULL(CQFILE.CQ_SEQ_FG,''),
                   ISNULL(CAST(CQFILE.CQ_DATE AS VARCHAR(10)),'0'),
                   CQFILE.CQ_START_TIME,
                   CQFILE.CQ_REV_DEL,
                   ISNULL(CQFILE.CQ_MISC_KEY_FLD,''),
                   CQFILE.CQ_REV_PDUE_DAYS,
                   CQFILE.CQ_REV_BAL,
                   CQFILE.CQ_SEQ,
                   ISNULL(CAST(CQFILE.CQ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CQFILE.CQ_WORK_COLLID,''),
                   ISNULL(CQFILE.CQ_WORKED_FG,''),
                   ISNULL(CQFILE.CQ_IN_PROCESS,''),
                   ISNULL(CAST(CQFILE.CQ_DATE_QUEUED
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CQFILE.CQ_TIME_ZONE_ERROR,'')
            FROM DBO.CQFILE
            WHERE CQFILE.CQ_BRNO >= :QCQ1-WBEG-BRNO
              AND CQFILE.CQ_BRNO <= :QCQ1-WEND-BRNO
              AND CQFILE.CQ_ACCTNO >= :QCQ1-WBEG-ACCTNO
              AND CQFILE.CQ_ACCTNO <= :QCQ1-WEND-ACCTNO
            ORDER BY CQFILE.CQ_BRNO,
                     CQFILE.CQ_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN CQ1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCQ1-WBEG-BRNO 
                          QCQ1-WEND-BRNO
                          QCQ1-WBEG-ACCTNO
                          QCQ1-WEND-ACCTNO.
 
           MOVE STAT---GOOD TO CQ1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CQ1-FILE.
           PERFORM READ-IT.
           MOVE CQ-PATH-OWNBR  TO CQ-BRNO. 
           MOVE CQ-SELECT-PATH TO E-FILE.
           MOVE CQ1-KEY        TO E-KEYX.

           MOVE CQ-BRNO   TO QCQ-BRNO.
           MOVE CQ-ACCTNO TO QCQ-ACCTNO.

           EXEC SQL
            SELECT CQFILE.CQ_BRNO,
                   CQFILE.CQ_ACCTNO,
                   ISNULL(CQFILE.CQ_COLLID,''),
                   CQFILE.CQ_REV_BEHAVIOR_RISK,
                   ISNULL(CQFILE.CQ_SEQ_FG,''),
                   ISNULL(CAST(CQFILE.CQ_DATE AS VARCHAR(10)),'0'),
                   CQFILE.CQ_START_TIME,
                   CQFILE.CQ_REV_DEL,
                   ISNULL(CQFILE.CQ_MISC_KEY_FLD,''),
                   CQFILE.CQ_REV_PDUE_DAYS,
                   CQFILE.CQ_REV_BAL,
                   CQFILE.CQ_SEQ,
                   ISNULL(CAST(CQFILE.CQ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CQFILE.CQ_WORK_COLLID,''),
                   ISNULL(CQFILE.CQ_WORKED_FG,''),
                   ISNULL(CQFILE.CQ_IN_PROCESS,''),
                   ISNULL(CAST(CQFILE.CQ_DATE_QUEUED
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CQFILE.CQ_TIME_ZONE_ERROR,'')
            INTO :QCQ-REC
            FROM DBO.CQFILE
            WHERE CQFILE.CQ_BRNO = :QCQ-BRNO
              AND CQFILE.CQ_ACCTNO = :QCQ-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CQ1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CQ-FIELDS.

      *-----------------------------------------------------------------
       READ-CQ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CQ-PATH-OWNBR  TO CQ-BRNO.
           MOVE CQ-SELECT-PATH TO E-FILE.
           MOVE CQ1-KEY        TO E-KEYX.
           INITIALIZE QCQ-REC.

           IF ( CQ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CQ1_CURSOR INTO :QCQ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CQ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CQ-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CQ1-FILE.

           IF ( CQ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CQ1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CQ1-CURSOR-STAT.

      *-----------------------------------------------------------------
       LOAD-CQ2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CQ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CQ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CQ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CQ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CQ-ALL-DATA
              MOVE CQ-ALL-PATH             TO CQ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CQ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CQ-ALL-BASE
              MOVE FILPATH-MACHINE         TO CQ-ALL-MACHINE
              MOVE FILPATH-DATA            TO CQ-ALL-DATA
              MOVE CQ-ALL-PATH             TO CQ-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-CQFILE NOT = "S" )
              MOVE CQ-PATH-BEG             TO CQ-SELECT-PATH-BEG
              MOVE CQ-PATH-FILE            TO CQ-SELECT-PATH-FILE
           ELSE
              MOVE CQ-PATH-SQL             TO CQ-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-CQ2-FILE.
           PERFORM LOAD-CQ2-FILE.
           PERFORM OPEN-IT.
           MOVE CQ-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CQ2-FILE.
           PERFORM START-IT.
           MOVE CQ-PATH-OWNBR  TO CQ-BRNO.
           MOVE CQ-SELECT-PATH TO E-FILE.
           PERFORM LOAD-CQ2-NON-COMP-KEY.
           MOVE SPWSCQ-CQ2-KEY-NON-COMP TO E-KEYX.

           IF ( CQ2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CQ2-FILE.

           IF ( QCQ2-WEND-SEQ = ZEROES    ) OR
              ( QCQ2-WEND-SEQ NOT NUMERIC )
              MOVE CQ-PATH-OWNBR     TO QCQ2-WBEG-BRNO
              MOVE CQ-PATH-OWNBR     TO QCQ2-WEND-BRNO

              MOVE CQ-COLLID         TO QCQ2-WBEG-COLLID
              MOVE CQ-REV-BEHAVIOR-RISK TO
                                     QCQ2-WBEG-REV-BEHAVIOR-RISK
              MOVE CQ-SEQ-FG           TO QCQ2-WBEG-SEQ-FG
              MOVE CQ-DATE             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCQ2-WBEG-DATE
              MOVE JULIAN-END          TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCQ2-WEND-DATE
              MOVE CQ-START-TIME       TO QCQ2-WBEG-START-TIME
              MOVE CQ-REV-DEL          TO QCQ2-WBEG-REV-DEL
              MOVE CQ-MISC-KEY-FLD     TO QCQ2-WBEG-MISC-KEY-FLD
              MOVE CQ-REV-PDUE-DAYS    TO QCQ2-WBEG-REV-PDUE-DAYS
              MOVE CQ-REV-BAL          TO QCQ2-WBEG-REV-BAL
              MOVE CQ-SEQ              TO QCQ2-WBEG-SEQ

              MOVE ALL "Z"         TO QCQ2-WEND-COLLID
                                      QCQ2-WEND-SEQ-FG
                                      QCQ2-WEND-MISC-KEY-FLD
              MOVE ALL "9"         TO QCQ2-WEND-REV-BEHAVIOR-RISK
                                      QCQ2-WEND-START-TIME
                                      QCQ2-WEND-REV-DEL
                                      QCQ2-WEND-REV-PDUE-DAYS
                                      QCQ2-WEND-REV-BAL
                                      QCQ2-WEND-SEQ
              CALL "C$TOLOWER" USING QCQ2-WEND-COLLID, VALUE 3
              CALL "C$TOLOWER" USING QCQ2-WEND-SEQ-FG, VALUE 1.


           EXEC SQL
            DECLARE CQ2_CURSOR CURSOR FOR
            SELECT CQFILE.CQ_BRNO,
                   CQFILE.CQ_ACCTNO,
                   ISNULL(CQFILE.CQ_COLLID,''),
                   CQFILE.CQ_REV_BEHAVIOR_RISK,
                   ISNULL(CQFILE.CQ_SEQ_FG,''),
                   ISNULL(CAST(CQFILE.CQ_DATE AS VARCHAR(10)),'0'),
                   CQFILE.CQ_START_TIME,
                   CQFILE.CQ_REV_DEL,
                   ISNULL(CQFILE.CQ_MISC_KEY_FLD,''),
                   CQFILE.CQ_REV_PDUE_DAYS,
                   CQFILE.CQ_REV_BAL,
                   CQFILE.CQ_SEQ,
                   ISNULL(CAST(CQFILE.CQ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CQFILE.CQ_WORK_COLLID,''),
                   ISNULL(CQFILE.CQ_WORKED_FG,''),
                   ISNULL(CQFILE.CQ_IN_PROCESS,''),
                   ISNULL(CAST(CQFILE.CQ_DATE_QUEUED
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CQFILE.CQ_TIME_ZONE_ERROR,'')
            FROM DBO.CQFILE
            WHERE CQFILE.CQ_BRNO >= :QCQ2-WBEG-BRNO
              AND CQFILE.CQ_BRNO <= :QCQ2-WEND-BRNO
              AND CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND CQFILE.CQ_COLLID <= :QCQ2-WEND-COLLID
              AND CQFILE.CQ_REV_BEHAVIOR_RISK >=
                           :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND CQFILE.CQ_REV_BEHAVIOR_RISK <=
                           :QCQ2-WEND-REV-BEHAVIOR-RISK
              AND CQFILE.CQ_SEQ_FG >= :QCQ2-WBEG-SEQ-FG
              AND CQFILE.CQ_SEQ_FG <= :QCQ2-WEND-SEQ-FG
              AND CAST(CQFILE.CQ_DATE AS VARCHAR(10)) >=
                           :QCQ2-WBEG-DATE
              AND CAST(CQFILE.CQ_DATE AS VARCHAR(10)) <=
                           :QCQ2-WEND-DATE
              AND CONCAT (CQFILE.CQ_START_TIME,
                          CQFILE.CQ_REV_DEL,
                          CQFILE.CQ_MISC_KEY_FLD,
                          CQFILE.CQ_REV_PDUE_DAYS,
                          CQFILE.CQ_REV_BAL,
                          CQFILE.CQ_SEQ)
                  >=
                  CONCAT (:QCQ2-WBEG-START-TIME,
                          :QCQ2-WBEG-REV-DEL,
                          :QCQ2-WBEG-MISC-KEY-FLD,
                          :QCQ2-WBEG-REV-PDUE-DAYS,
                          :QCQ2-WBEG-REV-BAL,
                          :QCQ2-WBEG-SEQ)
              AND CONCAT (CQFILE.CQ_START_TIME,
                          CQFILE.CQ_REV_DEL,
                          CQFILE.CQ_MISC_KEY_FLD,
                          CQFILE.CQ_REV_PDUE_DAYS,
                          CQFILE.CQ_REV_BAL,
                          CQFILE.CQ_SEQ)
                  <=
                  CONCAT (:QCQ2-WEND-START-TIME,
                          :QCQ2-WEND-REV-DEL,
                          :QCQ2-WEND-MISC-KEY-FLD,
                          :QCQ2-WEND-REV-PDUE-DAYS,
                          :QCQ2-WEND-REV-BAL,
                          :QCQ2-WEND-SEQ)
            ORDER BY CQFILE.CQ_BRNO,
                     CQFILE.CQ_COLLID,
                     CQFILE.CQ_REV_BEHAVIOR_RISK,
                     CQFILE.CQ_SEQ_FG,
                     CQFILE.CQ_DATE,
                     CQFILE.CQ_START_TIME,
                     CQFILE.CQ_REV_DEL,
                     CQFILE.CQ_MISC_KEY_FLD,
                     CQFILE.CQ_REV_PDUE_DAYS,
                     CQFILE.CQ_REV_BAL,
                     CQFILE.CQ_SEQ,
                     CQFILE.CQ_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN CQ2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCQ2-WBEG-BRNO 
                          QCQ2-WEND-BRNO
                          QCQ2-WBEG-REV-BEHAVIOR-RISK
                          QCQ2-WEND-REV-BEHAVIOR-RISK
                          QCQ2-WBEG-START-TIME
                          QCQ2-WEND-START-TIME
                          QCQ2-WBEG-REV-DEL
                          QCQ2-WEND-REV-DEL
                          QCQ2-WBEG-REV-PDUE-DAYS
                          QCQ2-WEND-REV-PDUE-DAYS
                          QCQ2-WBEG-REV-BAL
                          QCQ2-WEND-REV-BAL
                          QCQ2-WBEG-SEQ
                          QCQ2-WEND-SEQ.
 
           MOVE SPACES TO QCQ2-WBEG-DATE
                          QCQ2-WEND-DATE
                          QCQ2-WBEG-COLLID
                          QCQ2-WEND-COLLID
                          QCQ2-WBEG-SEQ-FG
                          QCQ2-WEND-SEQ-FG
                          QCQ2-WBEG-MISC-KEY-FLD
                          QCQ2-WEND-MISC-KEY-FLD.

           MOVE STAT---GOOD TO CQ2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CQ2-FILE.
           PERFORM READ-IT.
           MOVE CQ-PATH-OWNBR  TO CQ-BRNO. 
           MOVE CQ-SELECT-PATH TO E-FILE.
           PERFORM LOAD-CQ2-NON-COMP-KEY.
           MOVE SPWSCQ-CQ2-KEY-NON-COMP TO E-KEYX.

           MOVE CQ-BRNO              TO QCQ-BRNO.
           MOVE CQ-COLLID            TO QCQ-COLLID.
           MOVE CQ-REV-BEHAVIOR-RISK TO QCQ-REV-BEHAVIOR-RISK.
           MOVE CQ-SEQ-FG            TO QCQ-SEQ-FG.
           MOVE CQ-DATE    TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QCQ-DATE.
           MOVE CQ-START-TIME        TO QCQ-START-TIME.
           MOVE CQ-REV-DEL           TO QCQ-REV-DEL.
           MOVE CQ-MISC-KEY-FLD      TO QCQ-MISC-KEY-FLD.
           MOVE CQ-REV-PDUE-DAYS     TO QCQ-REV-PDUE-DAYS.
           MOVE CQ-REV-BAL           TO QCQ-REV-BAL.
           MOVE CQ-SEQ               TO QCQ-SEQ.

           EXEC SQL
            SELECT CQFILE.CQ_BRNO,
                   CQFILE.CQ_ACCTNO,
                   ISNULL(CQFILE.CQ_COLLID,''),
                   CQFILE.CQ_REV_BEHAVIOR_RISK,
                   ISNULL(CQFILE.CQ_SEQ_FG,''),
                   ISNULL(CAST(CQFILE.CQ_DATE AS VARCHAR(10)),'0'),
                   CQFILE.CQ_START_TIME,
                   CQFILE.CQ_REV_DEL,
                   ISNULL(CQFILE.CQ_MISC_KEY_FLD,''),
                   CQFILE.CQ_REV_PDUE_DAYS,
                   CQFILE.CQ_REV_BAL,
                   CQFILE.CQ_SEQ,
                   ISNULL(CAST(CQFILE.CQ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CQFILE.CQ_WORK_COLLID,''),
                   ISNULL(CQFILE.CQ_WORKED_FG,''),
                   ISNULL(CQFILE.CQ_IN_PROCESS,''),
                   ISNULL(CAST(CQFILE.CQ_DATE_QUEUED
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CQFILE.CQ_TIME_ZONE_ERROR,'')
            INTO :QCQ-REC
            FROM DBO.CQFILE
            WHERE CQFILE.CQ_BRNO = :QCQ-BRNO
              AND CQFILE.CQ_COLLID = :QCQ-COLLID
              AND CQFILE.CQ_REV_BEHAVIOR_RISK = :QCQ-REV-BEHAVIOR-RISK
              AND CQFILE.CQ_SEQ_FG = :QCQ-SEQ-FG
              AND CQFILE.CQ_DATE = :QCQ-DATE
              AND CQFILE.CQ_START_TIME = :QCQ-START-TIME
              AND CQFILE.CQ_REV_DEL = :QCQ-REV-DEL
              AND CQFILE.CQ_MISC_KEY_FLD = :QCQ-MISC-KEY-FLD
              AND CQFILE.CQ_REV_PDUE_DAYS = :QCQ-REV-PDUE-DAYS
              AND CQFILE.CQ_REV_BAL = :QCQ-REV-BAL
              AND CQFILE.CQ_SEQ = :QCQ-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CQ2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CQ-FIELDS.

      *-----------------------------------------------------------------
       READ-CQ2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CQ-PATH-OWNBR  TO CQ-BRNO.
           MOVE CQ-SELECT-PATH TO E-FILE.
           PERFORM LOAD-CQ2-NON-COMP-KEY.
           MOVE SPWSCQ-CQ2-KEY-NON-COMP TO E-KEYX.
           INITIALIZE QCQ-REC.

           IF ( CQ2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CQ2_CURSOR INTO :QCQ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CQ2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CQ-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CQ2-FILE.

           IF ( CQ2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CQ2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CQ1-CURSOR-STAT.

       LOAD-CQ2-NON-COMP-KEY.
           MOVE CQ-BRNO                 TO SPWSCQ-CQ2-BRNO-NC.
           MOVE CQ-COLLID               TO SPWSCQ-CQ2-COLLID-NC.
           MOVE CQ-REV-BEHAVIOR-RISK TO SPWSCQ-CQ2-REV-BEHAVIOR-RISK-NC.
           MOVE CQ-SEQ-FG               TO SPWSCQ-CQ2-SEQ-FG-NC.
           MOVE CQ-DATE                 TO SPWSCQ-CQ2-DATE-NC.
           MOVE CQ-START-TIME           TO SPWSCQ-CQ2-START-TIME-NC.
           MOVE CQ-REV-DEL              TO SPWSCQ-CQ2-REV-DEL-NC.
           MOVE CQ-MISC-KEY-FLD         TO SPWSCQ-CQ2-MISC-KEY-FLD-NC.
           MOVE CQ-REV-PDUE-DAYS        TO SPWSCQ-CQ2-REV-PDUE-DAYS-NC.
           MOVE CQ-REV-BAL              TO SPWSCQ-CQ2-REV-BAL-NC.
           MOVE CQ-SEQ                  TO SPWSCQ-CQ2-SEQ-NC.


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
              DESTROY IOCQRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
