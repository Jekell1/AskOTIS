       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOCJRN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  04-15-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOCJRN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : CJFILE 
      * TABLE DESC    : COLLECTOR JOURNAL FILE
      * OUTPUT FILE   : XPC/IOCJRN.PRE
      * USED FOR      : LIBSP/SPWSCJ.CPY  --- QCJ-REC
      *                 LIBSP/SPCJ1R.CPY  --- READ
      *                 LIBSP/SPCJ1RN.CPY --- READ/READ-NEXT/START
      *                 LIBSP/SPCJ2RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0415 ***NEW***
      * BLM 2021-0506 REMOVED DUPLICATE PARAGRAPH FOR READ-CJ2-FILE
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
           "@(#)IO/IOCJRN.CBL S34 05/19/21-12:54:01 >".

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
       COPY "LIBSP/SP01CJ.CPY".
       COPY "LIBSP/SPWSCJ.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QCJ1-WHERE-BEG-KEY.
           03  QCJ1-WBEG-BRNO              PIC 9(04).
           03  QCJ1-WBEG-COLLID            PIC XXX.
           03  QCJ1-WBEG-DATE              PIC X(10).
           03  QCJ1-WBEG-START-TIME        PIC 9(04).
           03  QCJ1-WBEG-SEQNO             PIC 99.

       01  QCJ1-WHERE-END-KEY.
           03  QCJ1-WEND-BRNO              PIC 9(04).
           03  QCJ1-WEND-COLLID            PIC XXX.
           03  QCJ1-WEND-DATE              PIC X(10).
           03  QCJ1-WEND-START-TIME        PIC 9(04).
           03  QCJ1-WEND-SEQNO             PIC 99.
 
       01  QCJ2-WHERE-BEG-KEY.
           03  QCJ2-WBEG-BRNO              PIC 9(04).
           03  QCJ2-WBEG-ACCTNO            PIC 9(08).
           03  QCJ2-WBEG-DATE              PIC X(10).
           03  QCJ2-WBEG-START-TIME        PIC 9(04).

       01  QCJ2-WHERE-END-KEY.
           03  QCJ2-WEND-BRNO              PIC 9(04).
           03  QCJ2-WEND-ACCTNO            PIC 9(08).
           03  QCJ2-WEND-DATE              PIC X(10).
           03  QCJ2-WEND-START-TIME        PIC 9(04).
 
       01  QCJ-REC.
           03  QCJ-BRNO              PIC 9(4).
           03  QCJ-COLLID            PIC XXX.
           03  QCJ-DATE              PIC X(10).
           03  QCJ-START-TIME        PIC 9(4).
           03  QCJ-SEQNO             PIC 99.
           03  QCJ-LTOUCH-DATE       PIC X(10).
           03  QCJ-ACCTNO            PIC 9(8).
           03  QCJ-END-TIME          PIC 9(4).
           03  QCJ-ACTIVITY-CD       PIC X(3).
           03  QCJ-RESULT-CD         PIC X(3).
           03  QCJ-ACTION-CD         PIC X(4).
           03  QCJ-NEW-COLLID        PIC X(3).
           03  QCJ-CON-STAT          PIC X(3).
           03  QCJ-REC-STAT          PIC X(3).
           03  QCJ-FORM-NO           PIC 9(4).
           03  QCJ-PROMISE-DATE1     PIC X(10).
           03  QCJ-PROMISE-AMT       PIC 9(7)V99.
           03  QCJ-BEG-LM-SEQ        PIC 9(3).
           03  QCJ-END-LM-SEQ        PIC 9(3).
           03  QCJ-PROMISE-DATE2     PIC X(10).
           03  QCJ-CUR-BAL           PIC 9(7)V99.
           03  QCJ-PAST-DUE          PIC 9(7)V99.
           03  QCJ-POSTING-BR        PIC 9(4).
           03  QCJ-QUEUE             PIC X(3).

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
           03  BEG-COLLID              PIC XXX.
           03  END-COLLID              PIC XXX.
           03  BEG-DATE                PIC 9(08).
           03  END-DATE                PIC 9(08).
           03  BEG-STARTTM             PIC 9(04).
           03  END-STARTTM             PIC 9(04).
           03  BEG-ACCTNO              PIC 9(08).
           03  END-ACCTNO              PIC 9(08).
           03  BEG-SEQNO               PIC 99.
           03  END-SEQNO               PIC 99.

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
           03  FILLER PIC X(30) VALUE "BRNO COLLID DATE    START TIME".
           03  FILLER PIC X(30) VALUE "ACCTNO  SEQNO                 ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(06) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-CJ-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(01).
           03  D01-CJ-COLLID                  PIC XXX.
           03  FILLER                         PIC X(01).
           03  D01-CJ-DATE                    PIC 9(08).
           03  FILLER                         PIC X(01).
           03  D01-CJ-START-TIME              PIC 9(04).
           03  FILLER                         PIC X(01).
           03  D01-CJ-ACCTNO                  PIC 9(08).
           03  FILLER                         PIC X(01).
           03  D01-CJ-SEQNO                   PIC 99.

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
       COPY "LIBIO/IOCJRN_DEF.CPY".
       COPY "LIBIO/IOCJRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOCJRN_SCN.CPY".

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
           MOVE "IOCJRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOCJRN-QUEUE-POS  TO Q-POS.
           MOVE IOCJRN-COPIES-POS TO C-POS.
           MOVE IOCJRN-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-SEQNO.

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
       ENTER-BEG-COLLID.

           MOVE "E  A030BEG-COLLID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
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

           GO TO ENTER-BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-ALL-COLLID.

           MOVE "S  A030BEG-COLLID." TO VDU.
           MOVE SPACES                 TO VDUBUF BEG-COLLID.
           PERFORM SCREENIO.

           MOVE "S  A030END-COLLID." TO VDU.
           MOVE ALL "Z"                TO VDUBUF END-COLLID.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-DATE.

           MOVE "ENNN080BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-COLLID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           MOVE "ENNN080END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-DATE.

           IF ( BEG-DATE > END-DATE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-BEG-START-TIME.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "SNNN080BEG-DATE." TO VDU.
           MOVE ZEROES                 TO VDUNUM0 BEG-DATE.
           PERFORM SCREENIO.

           MOVE "SNNN080END-DATE." TO VDU.
           MOVE 99999999               TO VDUNUM0 END-DATE.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-START-TIME.

           MOVE "ENNN040BEG-STARTTM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SEQNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-START-TIME.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-START-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-STARTTM.

      *-----------------------------------------------------------------
       ENTER-END-START-TIME.

           MOVE "ENNN040END-STARTTM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-START-TIME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-START-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-STARTTM.

           IF ( BEG-STARTTM > END-STARTTM )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-START-TIME.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-START-TIME.

           MOVE "SNNN040BEG-STARTTM."  TO VDU.
           MOVE ZEROES                 TO VDUNUM0 BEG-STARTTM.
           PERFORM SCREENIO.

           MOVE "SNNN040END-STARTTM."  TO VDU.
           MOVE 9999                   TO VDUNUM0 END-STARTTM.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-ACCTNO.

           MOVE "ENNN080BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-START-TIME.
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

           GO TO ENTER-BEG-SEQNO.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN080BEG-ACCTNO." TO VDU.
           MOVE ZEROES               TO VDUNUM0  BEG-ACCTNO.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO." TO VDU.
           MOVE 99999999             TO VDUNUM0  END-ACCTNO.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SEQNO.

           MOVE "ENNN020BEG-SEQNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SEQNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SEQNO.

      *-----------------------------------------------------------------
       ENTER-END-SEQNO.

           MOVE "ENNN020END-SEQNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SEQNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SEQNO.

           IF ( BEG-SEQNO > END-SEQNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SEQNO.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-SEQNO.

           MOVE "SNNN020BEG-SEQNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0 BEG-SEQNO.
           PERFORM SCREENIO.

           MOVE "SNNN020END-SEQNO." TO VDU.
           MOVE 99                  TO VDUNUM0 END-SEQNO.
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

           MOVE "S  A030BEG-COLLID."      TO VDU.
           MOVE BEG-COLLID                TO VDUBUF. 
           PERFORM SCREENIO.

           MOVE "S  A030END-COLLID."      TO VDU.
           MOVE END-COLLID                TO VDUBUF. 
           PERFORM SCREENIO.

           MOVE "SNNN080BEG-DATE."        TO VDU.
           MOVE BEG-DATE                  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080END-DATE."        TO VDU.
           MOVE END-DATE                  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BEG-STARTTM."     TO VDU.
           MOVE BEG-STARTTM               TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-STARTTM."     TO VDU.
           MOVE END-STARTTM               TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080BEG-ACCTNO."      TO VDU.
           MOVE BEG-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO."      TO VDU.
           MOVE END-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020BEG-SEQNO."       TO VDU.
           MOVE BEG-SEQNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-SEQNO."       TO VDU.
           MOVE END-SEQNO                 TO VDUNUM0.
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

           MOVE "B"                        TO CJ-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO       TO QCJ1-WBEG-BRNO 
                                QCJ2-WBEG-BRNO.

           MOVE BR-NO       TO QCJ1-WEND-BRNO 
                                QCJ2-WEND-BRNO.

           MOVE BEG-COLLID   TO QCJ1-WBEG-COLLID.
           MOVE END-COLLID   TO QCJ1-WEND-COLLID.

           MOVE BEG-DATE     TO QCJ1-WBEG-DATE
                                QCJ2-WBEG-DATE.

           MOVE END-DATE     TO QCJ1-WEND-DATE
                                QCJ2-WEND-DATE.

           MOVE BEG-STARTTM  TO QCJ1-WBEG-START-TIME
                                QCJ2-WBEG-START-TIME.

           MOVE END-STARTTM  TO QCJ1-WEND-START-TIME
                                QCJ2-WEND-START-TIME.

           MOVE BEG-ACCTNO   TO QCJ2-WBEG-ACCTNO.
           MOVE END-ACCTNO   TO QCJ2-WEND-ACCTNO.

           MOVE BEG-SEQNO    TO QCJ1-WBEG-SEQNO.
           MOVE END-SEQNO    TO QCJ1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-CJ1-FILE
              PERFORM START-CJ1-FILE
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM OPEN-CJ2-FILE
              PERFORM START-CJ2-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-CJ1-FILE-NEXT
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM READ-CJ2-FILE-NEXT.

           IF ( IO-BAD ) OR ( CJ-BRNO NOT = CJ-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( CJ-COLLID < BEG-COLLID )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( CJ-COLLID > END-COLLID )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
              IF ( CJ-DATE < BEG-DATE ) OR
                 ( CJ-DATE > END-DATE )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( CJ-START-TIME < BEG-STARTTM ) OR
                 ( CJ-START-TIME > END-STARTTM )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( CJ-SEQNO < BEG-SEQNO ) OR
                 ( CJ-SEQNO > END-SEQNO )
                 GO TO PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 2 )
              IF ( CJ-ACCTNO < BEG-ACCTNO )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( CJ-ACCTNO > END-ACCTNO )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
              IF ( CJ-DATE < BEG-DATE ) OR
                 ( CJ-DATE > END-DATE )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( CJ-START-TIME < BEG-STARTTM ) OR
                 ( CJ-START-TIME > END-STARTTM )
                 GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE CJ-BRNO                    TO D01-CJ-BRNO.
           MOVE CJ-COLLID                  TO D01-CJ-COLLID.
           MOVE CJ-DATE                    TO D01-CJ-DATE.
           MOVE CJ-START-TIME              TO D01-CJ-START-TIME.
           MOVE CJ-ACCTNO                  TO D01-CJ-ACCTNO.
           MOVE CJ-SEQNO                   TO D01-CJ-SEQNO.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 1 )
              PERFORM CLOSE-CJ1-FILE
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM CLOSE-CJ2-FILE.

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
              MOVE CJ1-KEY TO PACE-DETAIL-KEY
           ELSE
           IF ( PROCESS-KEY = 2 )
              STRING CJ-BRNO, CJ-ACCTNO, CJ-DATE, CJ-START-TIME
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
           MOVE "BEGINNING COLLID              : "    TO R01-PROMPT.
           MOVE BEG-COLLID                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    COLLID              : "    TO R01-PROMPT.
           MOVE END-COLLID                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DATE                : "    TO R01-PROMPT.
           MOVE BEG-DATE                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE                : "    TO R01-PROMPT.
           MOVE END-DATE                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING START TIME          : "    TO R01-PROMPT.
           MOVE BEG-STARTTM                           TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    START TIME          : "    TO R01-PROMPT.
           MOVE END-STARTTM                           TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
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
           MOVE "BEGINNING SEQNO               : "    TO R01-PROMPT.
           MOVE BEG-SEQNO                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SEQNO               : "    TO R01-PROMPT.
           MOVE END-SEQNO                             TO R01-NUMERIC.
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
           MOVE "SQL TABLE IO UTILITY - CJFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOCJRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOCJRN-SYSDATE.
           DISPLAY IOCJRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOCJRN-FORM-FIELDS TO WR-BUF.
           MOVE IOCJRN-FORM-LIST TO WR-LIST.
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

           MOVE IOCJRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOCJRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOCJRN_EVA.CPY".
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
      *    LOAD CJ-REC FROM QCJ-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CJ-FIELDS.
           MOVE QCJ-BRNO TO CJ-BRNO.
           MOVE QCJ-COLLID TO CJ-COLLID.

           IF ( QCJ-DATE = "1900-01-01" )
              MOVE ZEROES TO CJ-DATE
           ELSE
              MOVE QCJ-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CJ-DATE.

           MOVE QCJ-START-TIME TO CJ-START-TIME.
           MOVE QCJ-SEQNO TO CJ-SEQNO.

           IF ( QCJ-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CJ-LTOUCH-DATE
           ELSE
              MOVE QCJ-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CJ-LTOUCH-DATE.

           MOVE QCJ-ACCTNO TO CJ-ACCTNO.
           MOVE QCJ-END-TIME TO CJ-END-TIME.
           MOVE QCJ-ACTIVITY-CD TO CJ-ACTIVITY-CD.
           MOVE QCJ-RESULT-CD TO CJ-RESULT-CD.
           MOVE QCJ-ACTION-CD TO CJ-ACTION-CD.
           MOVE QCJ-NEW-COLLID TO CJ-NEW-COLLID.
           MOVE QCJ-CON-STAT TO CJ-CON-STAT.
           MOVE QCJ-REC-STAT TO CJ-REC-STAT.
           MOVE QCJ-FORM-NO TO CJ-FORM-NO.

           IF ( QCJ-PROMISE-DATE1 = "1900-01-01" )
              MOVE ZEROES TO CJ-PROMISE-DATE1
           ELSE
              MOVE QCJ-PROMISE-DATE1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CJ-PROMISE-DATE1.

           MOVE QCJ-PROMISE-AMT TO CJ-PROMISE-AMT.
           MOVE QCJ-BEG-LM-SEQ TO CJ-BEG-LM-SEQ.
           MOVE QCJ-END-LM-SEQ TO CJ-END-LM-SEQ.

           IF ( QCJ-PROMISE-DATE2 = "1900-01-01" )
              MOVE ZEROES TO CJ-PROMISE-DATE2
           ELSE
              MOVE QCJ-PROMISE-DATE2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CJ-PROMISE-DATE2.

           MOVE QCJ-CUR-BAL TO CJ-CUR-BAL.
           MOVE QCJ-PAST-DUE TO CJ-PAST-DUE.
           MOVE QCJ-POSTING-BR TO CJ-POSTING-BR.
           MOVE QCJ-QUEUE TO CJ-QUEUE.
 
      *-----------------------------------------------------------------
      *    LOAD QCJ-REC FROM CJ-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CJ-FIELDS.
           MOVE CJ-BRNO TO QCJ-BRNO.
           MOVE CJ-COLLID TO QCJ-COLLID.

           IF ( CJ-DATE = ZEROES )
              MOVE "1900-01-01" TO QCJ-DATE
           ELSE
              MOVE CJ-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCJ-DATE.

           MOVE CJ-START-TIME TO QCJ-START-TIME.
           MOVE CJ-SEQNO TO QCJ-SEQNO.

           IF ( CJ-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCJ-LTOUCH-DATE
           ELSE
              MOVE CJ-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCJ-LTOUCH-DATE.

           MOVE CJ-ACCTNO TO QCJ-ACCTNO.
           MOVE CJ-END-TIME TO QCJ-END-TIME.
           MOVE CJ-ACTIVITY-CD TO QCJ-ACTIVITY-CD.
           MOVE CJ-RESULT-CD TO QCJ-RESULT-CD.
           MOVE CJ-ACTION-CD TO QCJ-ACTION-CD.
           MOVE CJ-NEW-COLLID TO QCJ-NEW-COLLID.
           MOVE CJ-CON-STAT TO QCJ-CON-STAT.
           MOVE CJ-REC-STAT TO QCJ-REC-STAT.
           MOVE CJ-FORM-NO TO QCJ-FORM-NO.

           IF ( CJ-PROMISE-DATE1 = ZEROES )
              MOVE "1900-01-01" TO QCJ-PROMISE-DATE1
           ELSE
              MOVE CJ-PROMISE-DATE1 TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCJ-PROMISE-DATE1.

           MOVE CJ-PROMISE-AMT TO QCJ-PROMISE-AMT.
           MOVE CJ-BEG-LM-SEQ TO QCJ-BEG-LM-SEQ.
           MOVE CJ-END-LM-SEQ TO QCJ-END-LM-SEQ.

           IF ( CJ-PROMISE-DATE2 = ZEROES )
              MOVE "1900-01-01" TO QCJ-PROMISE-DATE2
           ELSE
              MOVE CJ-PROMISE-DATE2 TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCJ-PROMISE-DATE2.

           MOVE CJ-CUR-BAL TO QCJ-CUR-BAL.
           MOVE CJ-PAST-DUE TO QCJ-PAST-DUE.
           MOVE CJ-POSTING-BR TO QCJ-POSTING-BR.
           MOVE CJ-QUEUE TO QCJ-QUEUE.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBSP/SPCJ1R
      * COPYMEMBER: LIBSP/SPCJ1RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-CJ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CJ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CJ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CJ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CJ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CJ-ALL-DATA
              MOVE CJ-ALL-PATH             TO CJ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CJ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CJ-ALL-BASE
              MOVE FILPATH-MACHINE         TO CJ-ALL-MACHINE
              MOVE FILPATH-DATA            TO CJ-ALL-DATA
              MOVE CJ-ALL-PATH             TO CJ-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-CJFILE NOT = "S" )
              MOVE CJ-PATH-BEG             TO CJ-SELECT-PATH-BEG
              MOVE CJ-PATH-FILE            TO CJ-SELECT-PATH-FILE
           ELSE
              MOVE CJ-PATH-SQL             TO CJ-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-CJ1-FILE.
           PERFORM LOAD-CJ1-FILE.
           PERFORM OPEN-IT.
           MOVE CJ-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CJ1-FILE.
           PERFORM START-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO.
           MOVE CJ-SELECT-PATH TO E-FILE.
           MOVE CJ1-KEY        TO E-KEYX.

           IF ( CJ1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CJ1-FILE.

           IF ( QCJ1-WEND-SEQNO = ZEROES    ) OR
              ( QCJ1-WEND-SEQNO NOT NUMERIC )
              MOVE CJ-BRNO              TO QCJ1-WBEG-BRNO
              MOVE CJ-BRNO              TO QCJ1-WEND-BRNO
              MOVE CJ-COLLID            TO QCJ1-WBEG-COLLID
              MOVE ALL "Z"              TO QCJ1-WEND-COLLID
              MOVE CJ-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCJ1-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCJ1-WEND-DATE
              MOVE CJ-START-TIME        TO QCJ1-WBEG-START-TIME
              MOVE ALL "9"              TO QCJ1-WEND-START-TIME
              MOVE CJ-SEQNO             TO QCJ1-WBEG-SEQNO
              MOVE ALL "9"              TO QCJ1-WEND-SEQNO.

           EXEC SQL
            DECLARE CJ1_CURSOR CURSOR FOR
            SELECT CJFILE.CJ_BRNO,
                   ISNULL(CJFILE.CJ_COLLID,''),
                   ISNULL(CAST(CJFILE.CJ_DATE AS VARCHAR(10)),'0'),
                   CJFILE.CJ_START_TIME,
                   CJFILE.CJ_SEQNO,
                   ISNULL(CAST(CJFILE.CJ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CJFILE.CJ_ACCTNO,
                   CJFILE.CJ_END_TIME,
                   ISNULL(CJFILE.CJ_ACTIVITY_CD,''),
                   ISNULL(CJFILE.CJ_RESULT_CD,''),
                   ISNULL(CJFILE.CJ_ACTION_CD,''),
                   ISNULL(CJFILE.CJ_NEW_COLLID,''),
                   ISNULL(CJFILE.CJ_CON_STAT,''),
                   ISNULL(CJFILE.CJ_REC_STAT,''),
                   CJFILE.CJ_FORM_NO,
                   ISNULL(CAST(CJFILE.CJ_PROMISE_DATE1
                                                  AS VARCHAR(10)),'0'),
                   CJFILE.CJ_PROMISE_AMT,
                   CJFILE.CJ_BEG_LM_SEQ,
                   CJFILE.CJ_END_LM_SEQ,
                   ISNULL(CAST(CJFILE.CJ_PROMISE_DATE2
                                                  AS VARCHAR(10)),'0'),
                   CJFILE.CJ_CUR_BAL,
                   CJFILE.CJ_PAST_DUE,
                   CJFILE.CJ_POSTING_BR,
                   ISNULL(CJFILE.CJ_QUEUE,'')
            FROM DBO.CJFILE
            WHERE CJFILE.CJ_BRNO >= :QCJ1-WBEG-BRNO 
              AND CJFILE.CJ_BRNO <= :QCJ1-WEND-BRNO 
              AND CJFILE.CJ_COLLID >= :QCJ1-WBEG-COLLID
              AND CJFILE.CJ_COLLID <= :QCJ1-WEND-COLLID
              AND CAST(CJFILE.CJ_DATE AS VARCHAR(10))
                          >= :QCJ1-WBEG-DATE
              AND CAST(CJFILE.CJ_DATE AS VARCHAR(10))
                          <= :QCJ1-WEND-DATE
              AND CJFILE.CJ_START_TIME >= :QCJ1-WBEG-START-TIME
              AND CJFILE.CJ_START_TIME <= :QCJ1-WEND-START-TIME
              AND CJFILE.CJ_SEQNO >= :QCJ1-WBEG-SEQNO
              AND CJFILE.CJ_SEQNO <= :QCJ1-WEND-SEQNO
              ORDER BY CJFILE.CJ_BRNO,
                       CJFILE.CJ_COLLID,
                       CJFILE.CJ_DATE,
                       CJFILE.CJ_START_TIME,
                       CJFILE.CJ_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN CJ1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCJ1-WBEG-BRNO 
                          QCJ1-WEND-BRNO
                          QCJ1-WBEG-START-TIME
                          QCJ1-WEND-START-TIME
                          QCJ1-WBEG-SEQNO
                          QCJ1-WEND-SEQNO.
 
           MOVE SPACES TO QCJ1-WBEG-COLLID
                          QCJ1-WEND-COLLID
                          QCJ1-WBEG-DATE
                          QCJ1-WEND-DATE.

           MOVE STAT---GOOD TO CJ1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CJ1-FILE.
           PERFORM READ-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO. 
           MOVE CJ-SELECT-PATH TO E-FILE.
           MOVE CJ1-KEY        TO E-KEYX.

           MOVE CJ-BRNO             TO QCJ-BRNO.
           MOVE CJ-COLLID           TO QCJ-COLLID.
           MOVE CJ-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCJ-DATE.
           MOVE CJ-START-TIME       TO QCJ-START-TIME.
           MOVE CJ-SEQNO            TO QCJ-SEQNO.

           EXEC SQL
            SELECT CJFILE.CJ_BRNO,
                   ISNULL(CJFILE.CJ_COLLID,''),
                   ISNULL(CAST(CJFILE.CJ_DATE AS VARCHAR(10)),'0'),
                   CJFILE.CJ_START_TIME,
                   CJFILE.CJ_SEQNO,
                   ISNULL(CAST(CJFILE.CJ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CJFILE.CJ_ACCTNO,
                   CJFILE.CJ_END_TIME,
                   ISNULL(CJFILE.CJ_ACTIVITY_CD,''),
                   ISNULL(CJFILE.CJ_RESULT_CD,''),
                   ISNULL(CJFILE.CJ_ACTION_CD,''),
                   ISNULL(CJFILE.CJ_NEW_COLLID,''),
                   ISNULL(CJFILE.CJ_CON_STAT,''),
                   ISNULL(CJFILE.CJ_REC_STAT,''),
                   CJFILE.CJ_FORM_NO,
                   ISNULL(CAST(CJFILE.CJ_PROMISE_DATE1
                                                  AS VARCHAR(10)),'0'),
                   CJFILE.CJ_PROMISE_AMT,
                   CJFILE.CJ_BEG_LM_SEQ,
                   CJFILE.CJ_END_LM_SEQ,
                   ISNULL(CAST(CJFILE.CJ_PROMISE_DATE2
                                                  AS VARCHAR(10)),'0'),
                   CJFILE.CJ_CUR_BAL,
                   CJFILE.CJ_PAST_DUE,
                   CJFILE.CJ_POSTING_BR,
                   ISNULL(CJFILE.CJ_QUEUE,'')
            INTO :QCJ-REC
            FROM DBO.CJFILE
            WHERE CJFILE.CJ_BRNO = :QCJ-BRNO
              AND CJFILE.CJ_COLLID = :QCJ-COLLID
              AND CJFILE.CJ_DATE = :QCJ-DATE
              AND CJFILE.CJ_START_TIME = :QCJ-START-TIME
              AND CJFILE.CJ_SEQNO = :QCJ-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CJ1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CJ-FIELDS.

      *-----------------------------------------------------------------
       READ-CJ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO.
           MOVE CJ-SELECT-PATH TO E-FILE.
           MOVE CJ1-KEY        TO E-KEYX.
           INITIALIZE QCJ-REC.

           IF ( CJ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CJ1_CURSOR INTO :QCJ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CJ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CJ-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CJ1-FILE.

           IF ( CJ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CJ1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CJ1-CURSOR-STAT.

      *-----------------------------------------------------------------
       LOAD-CJ2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CJ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CJ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CJ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CJ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CJ-ALL-DATA
              MOVE CJ-ALL-PATH             TO CJ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CJ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CJ-ALL-BASE
              MOVE FILPATH-MACHINE         TO CJ-ALL-MACHINE
              MOVE FILPATH-DATA            TO CJ-ALL-DATA
              MOVE CJ-ALL-PATH             TO CJ-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-CJFILE NOT = "S" )
              MOVE CJ-PATH-BEG             TO CJ-SELECT-PATH-BEG
              MOVE CJ-PATH-FILE            TO CJ-SELECT-PATH-FILE
           ELSE
              MOVE CJ-PATH-SQL             TO CJ-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-CJ2-FILE.
           PERFORM LOAD-CJ2-FILE.
           PERFORM OPEN-IT.
           MOVE CJ-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CJ2-FILE.
           PERFORM START-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO.
           MOVE CJ-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING CJ-BRNO, CJ-ACCTNO, CJ-DATE, CJ-START-TIME
              DELIMITED BY SIZE INTO E-KEYX.

           IF ( CJ2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CJ2-FILE.

           IF ( QCJ2-WEND-START-TIME = ZEROES    ) OR
              ( QCJ2-WEND-START-TIME NOT NUMERIC )
              MOVE CJ-BRNO              TO QCJ2-WBEG-BRNO
              MOVE CJ-BRNO              TO QCJ2-WEND-BRNO
              MOVE CJ-ACCTNO            TO QCJ2-WBEG-ACCTNO 
              MOVE ALL "9"              TO QCJ2-WEND-ACCTNO 
              MOVE CJ-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCJ2-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCJ2-WEND-DATE
              MOVE CJ-START-TIME        TO QCJ2-WBEG-START-TIME
              MOVE ALL "9"              TO QCJ2-WEND-START-TIME.

           EXEC SQL
            DECLARE CJ2_CURSOR CURSOR FOR
            SELECT CJFILE.CJ_BRNO,
                   ISNULL(CJFILE.CJ_COLLID,''),
                   ISNULL(CAST(CJFILE.CJ_DATE AS VARCHAR(10)),'0'),
                   CJFILE.CJ_START_TIME,
                   CJFILE.CJ_SEQNO,
                   ISNULL(CAST(CJFILE.CJ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CJFILE.CJ_ACCTNO,
                   CJFILE.CJ_END_TIME,
                   ISNULL(CJFILE.CJ_ACTIVITY_CD,''),
                   ISNULL(CJFILE.CJ_RESULT_CD,''),
                   ISNULL(CJFILE.CJ_ACTION_CD,''),
                   ISNULL(CJFILE.CJ_NEW_COLLID,''),
                   ISNULL(CJFILE.CJ_CON_STAT,''),
                   ISNULL(CJFILE.CJ_REC_STAT,''),
                   CJFILE.CJ_FORM_NO,
                   ISNULL(CAST(CJFILE.CJ_PROMISE_DATE1
                                                  AS VARCHAR(10)),'0'),
                   CJFILE.CJ_PROMISE_AMT,
                   CJFILE.CJ_BEG_LM_SEQ,
                   CJFILE.CJ_END_LM_SEQ,
                   ISNULL(CAST(CJFILE.CJ_PROMISE_DATE2
                                                  AS VARCHAR(10)),'0'),
                   CJFILE.CJ_CUR_BAL,
                   CJFILE.CJ_PAST_DUE,
                   CJFILE.CJ_POSTING_BR,
                   ISNULL(CJFILE.CJ_QUEUE,'')
            FROM DBO.CJFILE
            WHERE CJFILE.CJ_BRNO >= :QCJ2-WBEG-BRNO 
              AND CJFILE.CJ_BRNO <= :QCJ2-WEND-BRNO 
              AND CJFILE.CJ_ACCTNO >= :QCJ2-WBEG-ACCTNO
              AND CJFILE.CJ_ACCTNO <= :QCJ2-WEND-ACCTNO
              AND CAST(CJFILE.CJ_DATE AS VARCHAR(10))
                          >= :QCJ2-WBEG-DATE
              AND CAST(CJFILE.CJ_DATE AS VARCHAR(10))
                          <= :QCJ2-WEND-DATE
              AND CJFILE.CJ_START_TIME >= :QCJ2-WBEG-START-TIME
              AND CJFILE.CJ_START_TIME <= :QCJ2-WEND-START-TIME
              ORDER BY CJFILE.CJ_BRNO,
                       CJFILE.CJ_ACCTNO,
                       CJFILE.CJ_DATE,
                       CJFILE.CJ_START_TIME,
                       CJFILE.CJ_COLLID,
                       CJFILE.CJ_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN CJ2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCJ2-WBEG-BRNO 
                          QCJ2-WEND-BRNO
                          QCJ2-WBEG-ACCTNO
                          QCJ2-WEND-ACCTNO
                          QCJ2-WBEG-START-TIME
                          QCJ2-WEND-START-TIME.
 
           MOVE SPACES TO QCJ2-WBEG-DATE
                          QCJ2-WEND-DATE.

           MOVE STAT---GOOD TO CJ2-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-CJ2-FILE-GREATER.
           PERFORM START-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO.
           MOVE CJ-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING CJ-BRNO, CJ-ACCTNO, CJ-DATE, CJ-START-TIME
              DELIMITED BY SIZE INTO E-KEYX.

           IF ( CJ2-GR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CJ2-FILE.

           IF ( QCJ2-WEND-START-TIME = ZEROES    ) OR
              ( QCJ2-WEND-START-TIME NOT NUMERIC )
              MOVE CJ-BRNO              TO QCJ2-WBEG-BRNO
              MOVE CJ-BRNO              TO QCJ2-WEND-BRNO
              MOVE CJ-ACCTNO            TO QCJ2-WBEG-ACCTNO 
              MOVE ALL "9"              TO QCJ2-WEND-ACCTNO 
              MOVE CJ-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCJ2-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCJ2-WEND-DATE
              MOVE CJ-START-TIME        TO QCJ2-WBEG-START-TIME
              MOVE ALL "9"              TO QCJ2-WEND-START-TIME.

           EXEC SQL
            DECLARE CJ2_GR_CURSOR CURSOR FOR
            SELECT CJFILE.CJ_BRNO,
                   ISNULL(CJFILE.CJ_COLLID,''),
                   ISNULL(CAST(CJFILE.CJ_DATE AS VARCHAR(10)),'0'),
                   CJFILE.CJ_START_TIME,
                   CJFILE.CJ_SEQNO,
                   ISNULL(CAST(CJFILE.CJ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CJFILE.CJ_ACCTNO,
                   CJFILE.CJ_END_TIME,
                   ISNULL(CJFILE.CJ_ACTIVITY_CD,''),
                   ISNULL(CJFILE.CJ_RESULT_CD,''),
                   ISNULL(CJFILE.CJ_ACTION_CD,''),
                   ISNULL(CJFILE.CJ_NEW_COLLID,''),
                   ISNULL(CJFILE.CJ_CON_STAT,''),
                   ISNULL(CJFILE.CJ_REC_STAT,''),
                   CJFILE.CJ_FORM_NO,
                   ISNULL(CAST(CJFILE.CJ_PROMISE_DATE1
                                                  AS VARCHAR(10)),'0'),
                   CJFILE.CJ_PROMISE_AMT,
                   CJFILE.CJ_BEG_LM_SEQ,
                   CJFILE.CJ_END_LM_SEQ,
                   ISNULL(CAST(CJFILE.CJ_PROMISE_DATE2
                                                  AS VARCHAR(10)),'0'),
                   CJFILE.CJ_CUR_BAL,
                   CJFILE.CJ_PAST_DUE,
                   CJFILE.CJ_POSTING_BR,
                   ISNULL(CJFILE.CJ_QUEUE,'')
            FROM DBO.CJFILE
            WHERE CJFILE.CJ_BRNO = :QCJ2-WBEG-BRNO 
              AND CJFILE.CJ_ACCTNO > :QCJ2-WBEG-ACCTNO
              AND CJFILE.CJ_ACCTNO <= :QCJ2-WEND-ACCTNO
              AND CAST(CJFILE.CJ_DATE AS VARCHAR(10))
                          > :QCJ2-WBEG-DATE
              AND CAST(CJFILE.CJ_DATE AS VARCHAR(10))
                          <= :QCJ2-WEND-DATE
              AND CJFILE.CJ_START_TIME > :QCJ2-WBEG-START-TIME
              AND CJFILE.CJ_START_TIME <= :QCJ2-WEND-START-TIME
              ORDER BY CJFILE.CJ_BRNO,
                       CJFILE.CJ_ACCTNO,
                       CJFILE.CJ_DATE,
                       CJFILE.CJ_START_TIME,
                       CJFILE.CJ_COLLID,
                       CJFILE.CJ_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN CJ2_GR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCJ2-WBEG-BRNO 
                          QCJ2-WEND-BRNO
                          QCJ2-WBEG-ACCTNO
                          QCJ2-WEND-ACCTNO
                          QCJ2-WBEG-START-TIME
                          QCJ2-WEND-START-TIME.
 
           MOVE SPACES TO QCJ2-WBEG-DATE
                          QCJ2-WEND-DATE.

           MOVE STAT---GOOD TO CJ2-GR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CJ2-FILE.
           PERFORM READ-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO. 
           MOVE CJ-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING CJ-BRNO, CJ-ACCTNO, CJ-DATE, CJ-START-TIME
              DELIMITED BY SIZE INTO E-KEYX.

           MOVE CJ-BRNO             TO QCJ-BRNO.
           MOVE CJ-ACCTNO TO QCJ-ACCTNO.
           MOVE CJ-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCJ-DATE.
           MOVE CJ-START-TIME       TO QCJ-START-TIME.

           EXEC SQL
            SELECT CJFILE.CJ_BRNO,
                   ISNULL(CJFILE.CJ_COLLID,''),
                   ISNULL(CAST(CJFILE.CJ_DATE AS VARCHAR(10)),'0'),
                   CJFILE.CJ_START_TIME,
                   CJFILE.CJ_SEQNO,
                   ISNULL(CAST(CJFILE.CJ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CJFILE.CJ_ACCTNO,
                   CJFILE.CJ_END_TIME,
                   ISNULL(CJFILE.CJ_ACTIVITY_CD,''),
                   ISNULL(CJFILE.CJ_RESULT_CD,''),
                   ISNULL(CJFILE.CJ_ACTION_CD,''),
                   ISNULL(CJFILE.CJ_NEW_COLLID,''),
                   ISNULL(CJFILE.CJ_CON_STAT,''),
                   ISNULL(CJFILE.CJ_REC_STAT,''),
                   CJFILE.CJ_FORM_NO,
                   ISNULL(CAST(CJFILE.CJ_PROMISE_DATE1
                                                  AS VARCHAR(10)),'0'),
                   CJFILE.CJ_PROMISE_AMT,
                   CJFILE.CJ_BEG_LM_SEQ,
                   CJFILE.CJ_END_LM_SEQ,
                   ISNULL(CAST(CJFILE.CJ_PROMISE_DATE2
                                                  AS VARCHAR(10)),'0'),
                   CJFILE.CJ_CUR_BAL,
                   CJFILE.CJ_PAST_DUE,
                   CJFILE.CJ_POSTING_BR,
                   ISNULL(CJFILE.CJ_QUEUE,'')
            INTO :QCJ-REC
            FROM DBO.CJFILE
            WHERE CJFILE.CJ_BRNO = :QCJ-BRNO
              AND CJFILE.CJ_ACCTNO = :QCJ-ACCTNO
              AND CJFILE.CJ_DATE = :QCJ-DATE
              AND CJFILE.CJ_START_TIME = :QCJ-START-TIME
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CJ2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CJ-FIELDS.

      *-----------------------------------------------------------------
       READ-CJ2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO.
           MOVE CJ-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING CJ-BRNO, CJ-ACCTNO, CJ-DATE, CJ-START-TIME
              DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QCJ-REC.

           IF ( CJ2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CJ2_CURSOR INTO :QCJ-REC
              END-EXEC
           ELSE
           IF ( CJ2-GR-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CJ2_GR_CURSOR INTO :QCJ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CJ2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CJ-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CJ2-FILE.

           IF ( CJ2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CJ2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CJ2-CURSOR-STAT.

           IF ( CJ2-GR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CJ2_GR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CJ2-GR-CURSOR-STAT.

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
              DESTROY IOCJRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
