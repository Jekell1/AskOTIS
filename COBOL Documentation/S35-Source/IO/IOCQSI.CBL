       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOCQSI.
       AUTHOR.        BAH.
       DATE-WRITTEN.  04-20-2021
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOCQSI.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : CQSFILE 
      * TABLE DESC    : COLLECTOR QUEUE SUMMARY FILE
      * OUTPUT FILE   : XPC/IOCQSI.PRE
      * USED FOR      : LIBSP/SPWSCQS.CPY  --- QCQS-REC
      *                 LIBSP/SPCQS1R.CPY  --- READ
      *                 LIBSP/SPCQS1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0420 ***NEW***
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
           "@(#)IO/IOCQSI.CBL S34 05/19/21-13:16:11 >".

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
       COPY "LIBSP/SP01CQS.CPY".
       COPY "LIBSP/SPWSCQS.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QCQS1-WHERE-BEG-KEY.
           03  QCQS1-WBEG-BRNO             PIC 9(04).
           03  QCQS1-WBEG-COLLID           PIC X(03).
 
       01  QCQS1-WHERE-END-KEY.
           03  QCQS1-WEND-BRNO             PIC 9(04).
           03  QCQS1-WEND-COLLID           PIC X(03).
 
       01  QCQS-REC.
           03  QCQS-BRNO                     PIC 9(4).
           03  QCQS-COLLID                   PIC X(3).
           03  QCQS-LTOUCH-DATE              PIC X(10).
           03  QCQS-TOT-REC                  PIC 9(5).
           03  QCQS-WORKED-REC               PIC 9(5).
           03  QCQS-TIMED-REC                PIC 9(5).
           03  QCQS-CQ-BRNO                  PIC 9(4).
           03  QCQS-CQ-COLLID                PIC XXX.
           03  QCQS-CQ-REV-BEHAVIOR-RISK     PIC 9.
           03  QCQS-CQ-SEQ-FG                PIC X.
           03  QCQS-CQ-DATE                  PIC X(10).
           03  QCQS-CQ-START-TIME            PIC S9(4).
           03  QCQS-CQ-REV-DEL               PIC 9(5).
           03  QCQS-CQ-MISC-KEY-FLD          PIC X(3).
           03  QCQS-CQ-REV-PDUE-DAYS         PIC 9(5).
           03  QCQS-CQ-REV-BAL               PIC 9(7).
           03  QCQS-CQ-SEQ                   PIC 9(3).
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
           03  BEG-COLLID               PIC X(03).
           03  END-COLLID               PIC X(03).

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
           03  FILLER PIC X(31) VALUE "BRNO  COLLID     TOT     WORKED".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-CQS-BRNO                   PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-CQS-COLLID                 PIC X(03).
           03  FILLER                         PIC X(07).
           03  D01-CQS-TOT-REC                PIC 9(05).
           03  FILLER                         PIC X(05).
           03  D01-CQS-WORKED-REC             PIC 9(05).


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
       COPY "LIBIO/IOCQSI_DEF.CPY".
       COPY "LIBIO/IOCQSI_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOCQSI_SCN.CPY".

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
           MOVE "IOCQSI"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOCQSI-QUEUE-POS  TO Q-POS.
           MOVE IOCQSI-COPIES-POS TO C-POS.
           MOVE IOCQSI-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-COLLID.

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

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-COLLID.

           MOVE "S  A030BEG-COLLID." TO VDU.
           MOVE SPACES               TO VDUBUF  BEG-COLLID.
           PERFORM SCREENIO.

           MOVE "S  A030END-COLLID." TO VDU.
           MOVE ALL "Z"              TO END-COLLID.
           CALL "C$TOLOWER" USING END-COLLID, VALUE 3.
           MOVE END-COLLID TO VDUBUF.
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

           MOVE "S  A030BEG-COLLID."       TO VDU.
           MOVE BEG-COLLID                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A030END-COLLID."       TO VDU.
           MOVE END-COLLID                 TO VDUBUF.
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

           MOVE "B"                        TO CQS-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QCQS1-WBEG-BRNO.
           MOVE BR-NO     TO QCQS1-WEND-BRNO.

           MOVE BEG-COLLID  TO QCQS1-WBEG-COLLID.
           MOVE END-COLLID  TO QCQS1-WEND-COLLID.


      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-CQS1-FILE.
           PERFORM START-CQS1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-CQS1-FILE-NEXT.

           IF ( IO-BAD ) OR ( CQS-BRNO NOT = CQS-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( CQS-COLLID < BEG-COLLID )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( CQS-COLLID > END-COLLID )
              GO TO PROCESS-BRANCH-MODULE-END.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE CQS-BRNO                TO D01-CQS-BRNO.
           MOVE CQS-COLLID              TO D01-CQS-COLLID.
           MOVE CQS-TOT-REC             TO D01-CQS-TOT-REC.
           MOVE CQS-WORKED-REC          TO D01-CQS-WORKED-REC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-CQS1-FILE.

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

           MOVE CQS1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING COLLID               : "    TO R01-PROMPT.
           MOVE BEG-COLLID                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    COLLID               : "    TO R01-PROMPT.
           MOVE END-COLLID                             TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - CQSFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOCQSI-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOCQSI-SYSDATE.
           DISPLAY IOCQSI-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOCQSI-FORM-FIELDS TO WR-BUF.
           MOVE IOCQSI-FORM-LIST TO WR-LIST.
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

           MOVE IOCQSI-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOCQSI-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOCQSI_EVA.CPY".
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
      *    LOAD CQS-REC FROM QCQS-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CQS-FIELDS.
           MOVE QCQS-BRNO TO CQS-BRNO.
           MOVE QCQS-COLLID TO CQS-COLLID.

           IF ( QCQS-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CQS-LTOUCH-DATE
           ELSE
              MOVE QCQS-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CQS-LTOUCH-DATE.

           MOVE QCQS-TOT-REC TO CQS-TOT-REC.
           MOVE QCQS-WORKED-REC TO CQS-WORKED-REC.
           MOVE QCQS-TIMED-REC TO CQS-TIMED-REC.
           MOVE QCQS-CQ-BRNO TO CQS-CQ-BRNO.
           MOVE QCQS-CQ-COLLID TO CQS-CQ-COLLID.
           MOVE QCQS-CQ-REV-BEHAVIOR-RISK TO CQS-CQ-REV-BEHAVIOR-RISK.
           MOVE QCQS-CQ-SEQ-FG TO CQS-CQ-SEQ-FG.

           IF ( QCQS-CQ-DATE = "1900-01-01" )
              MOVE ZEROES TO CQS-CQ-DATE
           ELSE
              MOVE QCQS-CQ-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CQS-CQ-DATE.

           MOVE QCQS-CQ-START-TIME TO CQS-CQ-START-TIME.
           MOVE QCQS-CQ-REV-DEL TO CQS-CQ-REV-DEL.
           MOVE QCQS-CQ-MISC-KEY-FLD TO CQS-CQ-MISC-KEY-FLD.
           MOVE QCQS-CQ-REV-PDUE-DAYS TO CQS-CQ-REV-PDUE-DAYS.
           MOVE QCQS-CQ-REV-BAL TO CQS-CQ-REV-BAL.
           MOVE QCQS-CQ-SEQ TO CQS-CQ-SEQ.
 
      *-----------------------------------------------------------------
      *    LOAD QCQS-REC FROM CQS-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CQS-FIELDS.
           MOVE CQS-BRNO TO QCQS-BRNO.
           MOVE CQS-COLLID TO QCQS-COLLID.

           IF ( CQS-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCQS-LTOUCH-DATE
           ELSE
              MOVE CQS-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCQS-LTOUCH-DATE.

           MOVE CQS-TOT-REC TO QCQS-TOT-REC.
           MOVE CQS-WORKED-REC TO QCQS-WORKED-REC.
           MOVE CQS-TIMED-REC TO QCQS-TIMED-REC.
           MOVE CQS-CQ-BRNO TO QCQS-CQ-BRNO.
           MOVE CQS-CQ-COLLID TO QCQS-CQ-COLLID.
           MOVE CQS-CQ-REV-BEHAVIOR-RISK TO QCQS-CQ-REV-BEHAVIOR-RISK.
           MOVE CQS-CQ-SEQ-FG TO QCQS-CQ-SEQ-FG.

           IF ( CQS-CQ-DATE = ZEROES )
              MOVE "1900-01-01" TO QCQS-CQ-DATE
           ELSE
              MOVE CQS-CQ-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCQS-CQ-DATE.

           MOVE CQS-CQ-START-TIME TO QCQS-CQ-START-TIME.
           MOVE CQS-CQ-REV-DEL TO QCQS-CQ-REV-DEL.
           MOVE CQS-CQ-MISC-KEY-FLD TO QCQS-CQ-MISC-KEY-FLD.
           MOVE CQS-CQ-REV-PDUE-DAYS TO QCQS-CQ-REV-PDUE-DAYS.
           MOVE CQS-CQ-REV-BAL TO QCQS-CQ-REV-BAL.
           MOVE CQS-CQ-SEQ TO QCQS-CQ-SEQ.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-CQS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CQS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CQS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CQS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CQS-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CQS-ALL-DATA
              MOVE CQS-ALL-PATH             TO CQS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CQS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CQS-ALL-BASE
              MOVE FILPATH-MACHINE         TO CQS-ALL-MACHINE
              MOVE FILPATH-DATA            TO CQS-ALL-DATA
              MOVE CQS-ALL-PATH             TO CQS-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-CQSFILE NOT = "S" )
              MOVE CQS-PATH-BEG             TO CQS-SELECT-PATH-BEG
              MOVE CQS-PATH-FILE            TO CQS-SELECT-PATH-FILE
           ELSE
              MOVE CQS-PATH-SQL             TO CQS-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-CQS1-FILE.
           PERFORM LOAD-CQS1-FILE.
           PERFORM OPEN-IT.
           MOVE CQS-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CQS1-FILE.
           PERFORM START-IT.
           MOVE CQS-PATH-OWNBR  TO CQS-BRNO.
           MOVE CQS-SELECT-PATH TO E-FILE.
           MOVE CQS1-KEY        TO E-KEYX.

           IF ( CQS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CQS1-FILE.

           IF ( QCQS1-WEND-COLLID = SPACES    ) 
              MOVE CQS-PATH-OWNBR        TO QCQS1-WBEG-BRNO
              MOVE CQS-PATH-OWNBR        TO QCQS1-WEND-BRNO
              MOVE CQS-COLLID            TO QCQS1-WBEG-COLLID
              MOVE ALL "Z"               TO QCQS1-WEND-COLLID
              CALL "C$TOLOWER" USING QCQS1-WEND-COLLID, VALUE 3.

           EXEC SQL
            DECLARE CQS1_CURSOR CURSOR FOR
            SELECT CQSFILE.CQS_BRNO,
                   ISNULL(CQSFILE.CQS_COLLID,''),
                   ISNULL(CAST(CQSFILE.CQS_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CQSFILE.CQS_TOT_REC,
                   CQSFILE.CQS_WORKED_REC,
                   CQSFILE.CQS_TIMED_REC,
                   CQSFILE.CQS_CQ_BRNO,
                   ISNULL(CQSFILE.CQS_CQ_COLLID,''),
                   CQSFILE.CQS_CQ_REV_BEHAVIOR_RISK,
                   ISNULL(CQSFILE.CQS_CQ_SEQ_FG,''),
                   ISNULL(CAST(CQSFILE.CQS_CQ_DATE
                                                  AS VARCHAR(10)),'0'),
                   CQSFILE.CQS_CQ_START_TIME,
                   CQSFILE.CQS_CQ_REV_DEL,
                   ISNULL(CQSFILE.CQS_CQ_MISC_KEY_FLD,''),
                   CQSFILE.CQS_CQ_REV_PDUE_DAYS,
                   CQSFILE.CQS_CQ_REV_BAL,
                   CQSFILE.CQS_CQ_SEQ
            FROM DBO.CQSFILE
            WHERE CQSFILE.CQS_BRNO >= :QCQS1-WBEG-BRNO
              AND CQSFILE.CQS_BRNO <= :QCQS1-WEND-BRNO
              AND CQSFILE.CQS_COLLID >= :QCQS1-WBEG-COLLID
              AND CQSFILE.CQS_COLLID <= :QCQS1-WEND-COLLID
            ORDER BY CQSFILE.CQS_BRNO,
                     CQSFILE.CQS_COLLID
           END-EXEC.

           EXEC SQL
               OPEN CQS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCQS1-WBEG-BRNO 
                          QCQS1-WEND-BRNO.
 
           MOVE SPACES TO QCQS1-WBEG-COLLID
                          QCQS1-WEND-COLLID.

           MOVE STAT---GOOD TO CQS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CQS1-FILE.
           PERFORM READ-IT.
           MOVE CQS-PATH-OWNBR  TO CQS-BRNO. 
           MOVE CQS-SELECT-PATH TO E-FILE.
           MOVE CQS1-KEY        TO E-KEYX.

           MOVE CQS-BRNO   TO QCQS-BRNO.
           MOVE CQS-COLLID TO QCQS-COLLID.

           EXEC SQL
            SELECT CQSFILE.CQS_BRNO,
                   ISNULL(CQSFILE.CQS_COLLID,''),
                   ISNULL(CAST(CQSFILE.CQS_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   CQSFILE.CQS_TOT_REC,
                   CQSFILE.CQS_WORKED_REC,
                   CQSFILE.CQS_TIMED_REC,
                   CQSFILE.CQS_CQ_BRNO,
                   ISNULL(CQSFILE.CQS_CQ_COLLID,''),
                   CQSFILE.CQS_CQ_REV_BEHAVIOR_RISK,
                   ISNULL(CQSFILE.CQS_CQ_SEQ_FG,''),
                   ISNULL(CAST(CQSFILE.CQS_CQ_DATE
                                                  AS VARCHAR(10)),'0'),
                   CQSFILE.CQS_CQ_START_TIME,
                   CQSFILE.CQS_CQ_REV_DEL,
                   ISNULL(CQSFILE.CQS_CQ_MISC_KEY_FLD,''),
                   CQSFILE.CQS_CQ_REV_PDUE_DAYS,
                   CQSFILE.CQS_CQ_REV_BAL,
                   CQSFILE.CQS_CQ_SEQ
            INTO :QCQS-REC
            FROM DBO.CQSFILE
            WHERE CQSFILE.CQS_BRNO = :QCQS-BRNO
              AND CQSFILE.CQS_COLLID = :QCQS-COLLID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CQS1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CQS-FIELDS.

      *-----------------------------------------------------------------
       READ-CQS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CQS-PATH-OWNBR  TO CQS-BRNO.
           MOVE CQS-SELECT-PATH TO E-FILE.
           MOVE CQS1-KEY        TO E-KEYX.
           INITIALIZE QCQS-REC.

           IF ( CQS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CQS1_CURSOR INTO :QCQS-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CQS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CQS-FIELDS.

      *-----------------------------------------------------------------
       WRITE-CQS1-FILE.

           MOVE CQS-PATH-OWNBR  TO CQS-BRNO.
           ACCEPT CQS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE CQS-SELECT-PATH TO E-FILE.
           MOVE CQS1-KEY        TO E-KEYX.
           PERFORM SET-CQS-FIELDS.

           EXEC SQL
            INSERT INTO DBO.CQSFILE
             (CQSFILE.CQS_BRNO,
              CQSFILE.CQS_COLLID,
              CQSFILE.CQS_LTOUCH_DATE,
              CQSFILE.CQS_TOT_REC,
              CQSFILE.CQS_WORKED_REC,
              CQSFILE.CQS_TIMED_REC,
              CQSFILE.CQS_CQ_BRNO,
              CQSFILE.CQS_CQ_COLLID,
              CQSFILE.CQS_CQ_REV_BEHAVIOR_RISK,
              CQSFILE.CQS_CQ_SEQ_FG,
              CQSFILE.CQS_CQ_DATE,
              CQSFILE.CQS_CQ_START_TIME,
              CQSFILE.CQS_CQ_REV_DEL,
              CQSFILE.CQS_CQ_MISC_KEY_FLD,
              CQSFILE.CQS_CQ_REV_PDUE_DAYS,
              CQSFILE.CQS_CQ_REV_BAL,
              CQSFILE.CQS_CQ_SEQ)
            VALUES
             (:QCQS-BRNO,
              :QCQS-COLLID,
              NULLIF(:QCQS-LTOUCH-DATE,'1900-01-01'),
              :QCQS-TOT-REC,
              :QCQS-WORKED-REC,
              :QCQS-TIMED-REC,
              :QCQS-CQ-BRNO,
              :QCQS-CQ-COLLID,
              :QCQS-CQ-REV-BEHAVIOR-RISK,
              :QCQS-CQ-SEQ-FG,
              NULLIF(:QCQS-CQ-DATE,'1900-01-01'),
              :QCQS-CQ-START-TIME,
              :QCQS-CQ-REV-DEL,
              :QCQS-CQ-MISC-KEY-FLD,
              :QCQS-CQ-REV-PDUE-DAYS,
              :QCQS-CQ-REV-BAL,
              :QCQS-CQ-SEQ)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-CQS1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-CQS1-FILE.

           MOVE CQS-PATH-OWNBR  TO CQS-BRNO.
           ACCEPT CQS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE CQS-SELECT-PATH TO E-FILE.
           MOVE CQS1-KEY        TO E-KEYX.
           PERFORM SET-CQS-FIELDS.

           EXEC SQL
            UPDATE DBO.CQSFILE
             SET
              CQSFILE.CQS_BRNO = :QCQS-BRNO,
              CQSFILE.CQS_COLLID = :QCQS-COLLID,
              CQSFILE.CQS_LTOUCH_DATE =
                 NULLIF(:QCQS-LTOUCH-DATE,'1900-01-01'),
              CQSFILE.CQS_TOT_REC = :QCQS-TOT-REC,
              CQSFILE.CQS_WORKED_REC = :QCQS-WORKED-REC,
              CQSFILE.CQS_TIMED_REC = :QCQS-TIMED-REC,
              CQSFILE.CQS_CQ_BRNO = :QCQS-CQ-BRNO,
              CQSFILE.CQS_CQ_COLLID = :QCQS-CQ-COLLID,
              CQSFILE.CQS_CQ_REV_BEHAVIOR_RISK =
                   :QCQS-CQ-REV-BEHAVIOR-RISK,
              CQSFILE.CQS_CQ_SEQ_FG = :QCQS-CQ-SEQ-FG,
              CQSFILE.CQS_CQ_DATE =
                 NULLIF(:QCQS-CQ-DATE,'1900-01-01'),
              CQSFILE.CQS_CQ_START_TIME = :QCQS-CQ-START-TIME,
              CQSFILE.CQS_CQ_REV_DEL = :QCQS-CQ-REV-DEL,
              CQSFILE.CQS_CQ_MISC_KEY_FLD = :QCQS-CQ-MISC-KEY-FLD,
              CQSFILE.CQS_CQ_REV_PDUE_DAYS =
                   :QCQS-CQ-REV-PDUE-DAYS,
              CQSFILE.CQS_CQ_REV_BAL = :QCQS-CQ-REV-BAL,
              CQSFILE.CQS_CQ_SEQ = :QCQS-CQ-SEQ
             WHERE CQSFILE.CQS_BRNO = :QCQS-BRNO
               AND CQSFILE.CQS_COLLID = :QCQS-COLLID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-CQS1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-CQS1-FILE.

           MOVE CQS-PATH-OWNBR  TO CQS-BRNO.
           PERFORM DELETE-IT.
           MOVE CQS-SELECT-PATH TO E-FILE.
           MOVE CQS1-KEY        TO E-KEYX.
           PERFORM SET-CQS-FIELDS.

           EXEC SQL
            DELETE FROM DBO.CQSFILE
             WHERE CQSFILE.CQS_BRNO = :QCQS-BRNO
               AND CQSFILE.CQS_COLLID = :QCQS-COLLID
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-CQS1-FILE.

      *-----------------------------------------------------------------
       CLOSE-CQS1-FILE.

           IF ( CQS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CQS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CQS1-CURSOR-STAT.

      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
 
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
              DESTROY IOCQSI-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
