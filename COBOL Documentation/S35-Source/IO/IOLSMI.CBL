       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOLSMI.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-12-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOLSMI.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : LSMFILE 
      * TABLE DESC    : LOAN TRAILER MISC FILE
      * OUTPUT FILE   : XPC/IOLSMI.PRE
      * USED FOR      : LIBLP/LPWSLSM.CPY  --- QLSM-REC
      *                 LIBLP/LPLSM1R.CPY  --- READ
      *                 LIBLP/LPLSM1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0512 ***NEW***
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
           "@(#)IO/IOLSMI.CBL S34 09/30/21-12:58:39 >".

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
       COPY "LIBLP/LP01LSM.CPY".
       COPY "LIBLP/LPWSLSM.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QLSM1-WHERE-BEG-KEY.
           03  QLSM1-WBEG-BRNO             PIC 9(04).
           03  QLSM1-WBEG-ACCTNO           PIC 9(08).
           03  QLSM1-WBEG-SEQNO            PIC 9(02).

       01  QLSM1-WHERE-END-KEY.
           03  QLSM1-WEND-BRNO             PIC 9(04).
           03  QLSM1-WEND-ACCTNO           PIC 9(08).
           03  QLSM1-WEND-SEQNO            PIC 9(02).

       01  QLSM-REC.
           03  QLSM-BRNO               PIC 9(4).
           03  QLSM-ACCTNO             PIC 9(8).
           03  QLSM-SEQNO              PIC 99.
           03  QLSM-LTOUCH-DATE        PIC X(10).
           03  QLSM-MISC-FIELD-1       PIC X(20).
           03  QLSM-MISC-FIELD-2       PIC X(20).
           03  QLSM-MISC-FIELD-3       PIC X(20).
           03  QLSM-MISC-FIELD-4       PIC X(20).
           03  QLSM-MISC-FIELD-5       PIC X(20).
           03  QLSM-MISC-FIELD-6       PIC X(20).
           03  QLSM-MISC-FIELD-7       PIC X(20).
           03  QLSM-MISC-FIELD-8       PIC X(20).
           03  QLSM-MISC-FIELD-9       PIC X(20).
           03  QLSM-MISC-FIELD-10      PIC X(20).
           03  QLSM-MISC-FIELD-11       PIC X(20).
           03  QLSM-MISC-FIELD-12       PIC X(20).
           03  QLSM-MISC-FIELD-13       PIC X(20).
           03  QLSM-MISC-FIELD-14       PIC X(20).
           03  QLSM-MISC-FIELD-15       PIC X(20).
           03  QLSM-MISC-FIELD-16       PIC X(20).
           03  QLSM-MISC-FIELD-17       PIC X(20).
           03  QLSM-MISC-FIELD-18       PIC X(20).
           03  QLSM-MISC-FIELD-19       PIC X(20).
           03  QLSM-MISC-FIELD-20       PIC X(20).
           03  QLSM-MISC-FIELD-21       PIC X(20).
           03  QLSM-MISC-FIELD-22       PIC X(20).
           03  QLSM-MISC-FIELD-23       PIC X(20).
           03  QLSM-MISC-FIELD-24       PIC X(20).
           03  QLSM-MISC-FIELD-25       PIC X(20).
           03  QLSM-MISC-FIELD-26       PIC X(20).
           03  QLSM-MISC-FIELD-27       PIC X(20).
           03  QLSM-MISC-FIELD-28       PIC X(20).
           03  QLSM-MISC-FIELD-29       PIC X(20).
           03  QLSM-MISC-FIELD-30       PIC X(20).
           03  QLSM-MISC-FIELD-31       PIC X(20).
           03  QLSM-MISC-FIELD-32       PIC X(20).
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
           03  BEG-SEQNO               PIC 9(02).
           03  END-SEQNO               PIC 9(02).

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
           03  FILLER PIC X(26) VALUE "BRNO    ACCTNO     SEQ    ".
           03  FILLER PIC X(22) VALUE "MISC 1                ".
           03  FILLER PIC X(30) VALUE "MISC 30               ".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-LSM-BRNO                  PIC 9(04).
           03  FILLER                        PIC X(03).
           03  D01-LSM-ACCTNO                PIC 9(08).
           03  FILLER                        PIC X(03).
           03  D01-LSM-SEQNO                 PIC 9(02).
           03  FILLER                        PIC X(03).
           03  D01-LSM-MISC-FIELD1         PIC X(20).
           03  FILLER                        PIC X(03).
           03  D01-LSM-MISC-FIELD30        PIC X(20).

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
       COPY "LIBIO/IOLSMI_DEF.CPY".
       COPY "LIBIO/IOLSMI_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOLSMI_SCN.CPY".

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
           MOVE "IOLSMI"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOLSMI-QUEUE-POS  TO Q-POS.
           MOVE IOLSMI-COPIES-POS TO C-POS.
           MOVE IOLSMI-PRU-POS    TO P-POS.
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
              ( VDUNUM0 > 1      ) 
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

           GO TO ENTER-BEG-SEQNO.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN080BEG-ACCTNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-ACCTNO.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO." TO VDU.
           MOVE 99999999            TO VDUNUM0  END-ACCTNO.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SEQNO.

           MOVE "ENNN020BEG-SEQNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ACCTNO.
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
           MOVE ZEROES              TO VDUNUM0  BEG-SEQNO.
           PERFORM SCREENIO.

           MOVE "SNNN020END-SEQNO." TO VDU.
           MOVE 999                 TO VDUNUM0  END-SEQNO.
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

           MOVE "B"                        TO LSM-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }

           MOVE BR-NO     TO QLSM1-WBEG-BRNO.
           MOVE BR-NO     TO QLSM1-WEND-BRNO.

           MOVE BEG-ACCTNO TO QLSM1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QLSM1-WEND-ACCTNO.
           MOVE BEG-SEQNO  TO QLSM1-WBEG-SEQNO.
           MOVE END-SEQNO  TO QLSM1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-LSM1-FILE.
           PERFORM START-LSM1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-LSM1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LSM-BRNO NOT = LSM-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( LSM-ACCTNO < BEG-ACCTNO )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( LSM-ACCTNO > END-ACCTNO )
              GO TO PROCESS-BRANCH-MODULE-END.

           IF LSM-SEQNO < BEG-SEQNO OR > END-SEQNO
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE LSM-BRNO                    TO D01-LSM-BRNO.
           MOVE LSM-ACCTNO                  TO D01-LSM-ACCTNO.
           MOVE LSM-SEQNO                   TO D01-LSM-SEQNO.
           MOVE LSM-MISC-FIELD(1)    TO D01-LSM-MISC-FIELD1.
           MOVE LSM-MISC-FIELD(30)   TO D01-LSM-MISC-FIELD30.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-LSM1-FILE.

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

           MOVE LSM1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "SQL TABLE IO UTILITY - LSMFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOLSMI-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOLSMI-SYSDATE.
           DISPLAY IOLSMI-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOLSMI-FORM-FIELDS TO WR-BUF.
           MOVE IOLSMI-FORM-LIST TO WR-LIST.
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

           MOVE IOLSMI-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOLSMI-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOLSMI_EVA.CPY".
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
      *    LOAD LSM-REC FROM QLSM-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LSM-FIELDS.
           MOVE QLSM-BRNO TO LSM-BRNO.
           MOVE QLSM-ACCTNO TO LSM-ACCTNO.
           MOVE QLSM-SEQNO TO LSM-SEQNO.

           IF ( QLSM-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LSM-LTOUCH-DATE
           ELSE
              MOVE QLSM-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSM-LTOUCH-DATE.

           MOVE QLSM-MISC-FIELD-1 TO LSM-MISC-FIELD(1).
           MOVE QLSM-MISC-FIELD-2 TO LSM-MISC-FIELD(2).
           MOVE QLSM-MISC-FIELD-3 TO LSM-MISC-FIELD(3).
           MOVE QLSM-MISC-FIELD-4 TO LSM-MISC-FIELD(4).
           MOVE QLSM-MISC-FIELD-5 TO LSM-MISC-FIELD(5).
           MOVE QLSM-MISC-FIELD-6 TO LSM-MISC-FIELD(6).
           MOVE QLSM-MISC-FIELD-7 TO LSM-MISC-FIELD(7).
           MOVE QLSM-MISC-FIELD-8 TO LSM-MISC-FIELD(8).
           MOVE QLSM-MISC-FIELD-9 TO LSM-MISC-FIELD(9).
           MOVE QLSM-MISC-FIELD-10 TO LSM-MISC-FIELD(10).
           MOVE QLSM-MISC-FIELD-11 TO LSM-MISC-FIELD(11).
           MOVE QLSM-MISC-FIELD-12 TO LSM-MISC-FIELD(12).
           MOVE QLSM-MISC-FIELD-13 TO LSM-MISC-FIELD(13).
           MOVE QLSM-MISC-FIELD-14 TO LSM-MISC-FIELD(14).
           MOVE QLSM-MISC-FIELD-15 TO LSM-MISC-FIELD(15).
           MOVE QLSM-MISC-FIELD-16 TO LSM-MISC-FIELD(16).
           MOVE QLSM-MISC-FIELD-17 TO LSM-MISC-FIELD(17).
           MOVE QLSM-MISC-FIELD-18 TO LSM-MISC-FIELD(18).
           MOVE QLSM-MISC-FIELD-19 TO LSM-MISC-FIELD(19).
           MOVE QLSM-MISC-FIELD-20 TO LSM-MISC-FIELD(20).
           MOVE QLSM-MISC-FIELD-21 TO LSM-MISC-FIELD(21).
           MOVE QLSM-MISC-FIELD-22 TO LSM-MISC-FIELD(22).
           MOVE QLSM-MISC-FIELD-23 TO LSM-MISC-FIELD(23).
           MOVE QLSM-MISC-FIELD-24 TO LSM-MISC-FIELD(24).
           MOVE QLSM-MISC-FIELD-25 TO LSM-MISC-FIELD(25).
           MOVE QLSM-MISC-FIELD-26 TO LSM-MISC-FIELD(26).
           MOVE QLSM-MISC-FIELD-27 TO LSM-MISC-FIELD(27).
           MOVE QLSM-MISC-FIELD-28 TO LSM-MISC-FIELD(28).
           MOVE QLSM-MISC-FIELD-29 TO LSM-MISC-FIELD(29).
           MOVE QLSM-MISC-FIELD-30 TO LSM-MISC-FIELD(30).
           MOVE QLSM-MISC-FIELD-31 TO LSM-MISC-FIELD(31).
           MOVE QLSM-MISC-FIELD-32 TO LSM-MISC-FIELD(32).
 
      *-----------------------------------------------------------------
      *    LOAD QLSM-REC FROM LSM-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LSM-FIELDS.
           MOVE LSM-BRNO TO QLSM-BRNO.
           MOVE LSM-ACCTNO TO QLSM-ACCTNO.
           MOVE LSM-SEQNO TO QLSM-SEQNO.

           IF ( LSM-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSM-LTOUCH-DATE
           ELSE
              MOVE LSM-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSM-LTOUCH-DATE.

           MOVE LSM-MISC-FIELD(1) TO QLSM-MISC-FIELD-1.
           MOVE LSM-MISC-FIELD(2) TO QLSM-MISC-FIELD-2.
           MOVE LSM-MISC-FIELD(3) TO QLSM-MISC-FIELD-3.
           MOVE LSM-MISC-FIELD(4) TO QLSM-MISC-FIELD-4.
           MOVE LSM-MISC-FIELD(5) TO QLSM-MISC-FIELD-5.
           MOVE LSM-MISC-FIELD(6) TO QLSM-MISC-FIELD-6.
           MOVE LSM-MISC-FIELD(7) TO QLSM-MISC-FIELD-7.
           MOVE LSM-MISC-FIELD(8) TO QLSM-MISC-FIELD-8.
           MOVE LSM-MISC-FIELD(9) TO QLSM-MISC-FIELD-9.
           MOVE LSM-MISC-FIELD(10) TO QLSM-MISC-FIELD-10.
           MOVE LSM-MISC-FIELD(11) TO QLSM-MISC-FIELD-11.
           MOVE LSM-MISC-FIELD(12) TO QLSM-MISC-FIELD-12.
           MOVE LSM-MISC-FIELD(13) TO QLSM-MISC-FIELD-13.
           MOVE LSM-MISC-FIELD(14) TO QLSM-MISC-FIELD-14.
           MOVE LSM-MISC-FIELD(15) TO QLSM-MISC-FIELD-15.
           MOVE LSM-MISC-FIELD(16) TO QLSM-MISC-FIELD-16.
           MOVE LSM-MISC-FIELD(17) TO QLSM-MISC-FIELD-17.
           MOVE LSM-MISC-FIELD(18) TO QLSM-MISC-FIELD-18.
           MOVE LSM-MISC-FIELD(19) TO QLSM-MISC-FIELD-19.
           MOVE LSM-MISC-FIELD(20) TO QLSM-MISC-FIELD-20.
           MOVE LSM-MISC-FIELD(21) TO QLSM-MISC-FIELD-21.
           MOVE LSM-MISC-FIELD(22) TO QLSM-MISC-FIELD-22.
           MOVE LSM-MISC-FIELD(23) TO QLSM-MISC-FIELD-23.
           MOVE LSM-MISC-FIELD(24) TO QLSM-MISC-FIELD-24.
           MOVE LSM-MISC-FIELD(25) TO QLSM-MISC-FIELD-25.
           MOVE LSM-MISC-FIELD(26) TO QLSM-MISC-FIELD-26.
           MOVE LSM-MISC-FIELD(27) TO QLSM-MISC-FIELD-27.
           MOVE LSM-MISC-FIELD(28) TO QLSM-MISC-FIELD-28.
           MOVE LSM-MISC-FIELD(29) TO QLSM-MISC-FIELD-29.
           MOVE LSM-MISC-FIELD(30) TO QLSM-MISC-FIELD-30.
           MOVE LSM-MISC-FIELD(31) TO QLSM-MISC-FIELD-31.
           MOVE LSM-MISC-FIELD(32) TO QLSM-MISC-FIELD-32.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LSM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LSM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LSM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LSM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LSM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LSM-ALL-DATA
              MOVE LSM-ALL-PATH             TO LSM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LSM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LSM-ALL-BASE
              MOVE FILPATH-MACHINE         TO LSM-ALL-MACHINE
              MOVE FILPATH-DATA            TO LSM-ALL-DATA
              MOVE LSM-ALL-PATH             TO LSM-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LSMFILE NOT = "S" )
              MOVE LSM-PATH-BEG             TO LSM-SELECT-PATH-BEG
              MOVE LSM-PATH-FILE            TO LSM-SELECT-PATH-FILE
           ELSE
              MOVE LSM-PATH-SQL             TO LSM-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LSM1-FILE.
           PERFORM LOAD-LSM1-FILE.
           PERFORM OPEN-IT.
           MOVE LSM-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LSM1-FILE.
           PERFORM START-IT.
           MOVE LSM-PATH-OWNBR  TO LSM-BRNO.
           MOVE LSM-SELECT-PATH TO E-FILE.
           MOVE LSM1-KEY        TO E-KEYX.

           IF ( LSM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LSM1-FILE.

           IF ( QLSM1-WEND-BRNO = ZEROES    ) OR
              ( QLSM1-WEND-BRNO NOT NUMERIC )
              MOVE LSM-PATH-OWNBR        TO QLSM1-WBEG-BRNO
              MOVE LSM-PATH-OWNBR        TO QLSM1-WEND-BRNO
              MOVE LSM-ACCTNO            TO QLSM1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLSM1-WEND-ACCTNO
              MOVE LSM-SEQNO             TO QLSM1-WBEG-SEQNO
              MOVE ALL "9"               TO QLSM1-WEND-SEQNO.

           EXEC SQL
            DECLARE LSM1_CURSOR CURSOR FOR
            SELECT LSMFILE.LSM_BRNO,
                   LSMFILE.LSM_ACCTNO,
                   LSMFILE.LSM_SEQNO,
                   ISNULL(CAST(LSMFILE.LSM_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_1,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_2,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_3,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_4,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_5,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_6,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_7,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_8,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_9,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_10,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_11,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_12,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_13,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_14,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_15,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_16,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_17,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_18,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_19,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_20,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_21,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_22,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_23,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_24,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_25,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_26,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_27,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_28,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_29,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_30,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_31,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_32,'')
            FROM DBO.LSMFILE
            WHERE LSMFILE.LSM_BRNO >= :QLSM1-WBEG-BRNO
              AND LSMFILE.LSM_BRNO <= :QLSM1-WEND-BRNO
              AND LSMFILE.LSM_ACCTNO >= :QLSM1-WBEG-ACCTNO
              AND LSMFILE.LSM_ACCTNO <= :QLSM1-WEND-ACCTNO
              AND LSMFILE.LSM_SEQNO >= :QLSM1-WBEG-SEQNO
              AND LSMFILE.LSM_SEQNO <= :QLSM1-WEND-SEQNO
            ORDER BY LSMFILE.LSM_BRNO,
                     LSMFILE.LSM_ACCTNO,
                     LSMFILE.LSM_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LSM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLSM1-WBEG-BRNO
                          QLSM1-WEND-BRNO
                          QLSM1-WBEG-ACCTNO
                          QLSM1-WEND-ACCTNO
                          QLSM1-WBEG-SEQNO
                          QLSM1-WEND-SEQNO.

           MOVE STAT---GOOD TO LSM1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LSM1-FILE.
           PERFORM READ-IT.
           MOVE LSM-PATH-OWNBR  TO LSM-BRNO. 
           MOVE LSM-SELECT-PATH TO E-FILE.
           MOVE LSM1-KEY        TO E-KEYX.

           MOVE LSM-BRNO   TO QLSM-BRNO.
           MOVE LSM-ACCTNO TO QLSM-ACCTNO.
           MOVE LSM-SEQNO TO QLSM-SEQNO.

           EXEC SQL
            SELECT LSMFILE.LSM_BRNO,
                   LSMFILE.LSM_ACCTNO,
                   LSMFILE.LSM_SEQNO,
                   ISNULL(CAST(LSMFILE.LSM_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_1,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_2,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_3,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_4,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_5,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_6,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_7,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_8,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_9,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_10,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_11,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_12,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_13,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_14,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_15,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_16,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_17,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_18,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_19,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_20,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_21,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_22,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_23,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_24,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_25,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_26,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_27,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_28,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_29,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_30,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_31,''),
                   ISNULL(LSMFILE.LSM_MISC_FIELD_32,'')
            INTO :QLSM-REC
            FROM DBO.LSMFILE
            WHERE LSMFILE.LSM_BRNO = :QLSM-BRNO
              AND LSMFILE.LSM_ACCTNO = :QLSM-ACCTNO
              AND LSMFILE.LSM_SEQNO = :QLSM-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSM1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LSM-FIELDS.

      *-----------------------------------------------------------------
       READ-LSM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LSM-PATH-OWNBR  TO LSM-BRNO.
           MOVE LSM-SELECT-PATH TO E-FILE.
           MOVE LSM1-KEY        TO E-KEYX.
           INITIALIZE QLSM-REC.

           IF ( LSM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LSM1_CURSOR INTO :QLSM-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LSM-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LSM1-FILE.

           MOVE LSM-PATH-OWNBR  TO LSM-BRNO.
           ACCEPT LSM-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LSM-SELECT-PATH TO E-FILE.
           MOVE LSM1-KEY        TO E-KEYX.
           PERFORM SET-LSM-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LSMFILE
             (LSMFILE.LSM_BRNO,
              LSMFILE.LSM_ACCTNO,
              LSMFILE.LSM_SEQNO,
              LSMFILE.LSM_LTOUCH_DATE,
              LSMFILE.LSM_MISC_FIELD_1,
              LSMFILE.LSM_MISC_FIELD_2,
              LSMFILE.LSM_MISC_FIELD_3,
              LSMFILE.LSM_MISC_FIELD_4,
              LSMFILE.LSM_MISC_FIELD_5,
              LSMFILE.LSM_MISC_FIELD_6,
              LSMFILE.LSM_MISC_FIELD_7,
              LSMFILE.LSM_MISC_FIELD_8,
              LSMFILE.LSM_MISC_FIELD_9,
              LSMFILE.LSM_MISC_FIELD_10,
              LSMFILE.LSM_MISC_FIELD_11,
              LSMFILE.LSM_MISC_FIELD_12,
              LSMFILE.LSM_MISC_FIELD_13,
              LSMFILE.LSM_MISC_FIELD_14,
              LSMFILE.LSM_MISC_FIELD_15,
              LSMFILE.LSM_MISC_FIELD_16,
              LSMFILE.LSM_MISC_FIELD_17,
              LSMFILE.LSM_MISC_FIELD_18,
              LSMFILE.LSM_MISC_FIELD_19,
              LSMFILE.LSM_MISC_FIELD_20,
              LSMFILE.LSM_MISC_FIELD_21,
              LSMFILE.LSM_MISC_FIELD_22,
              LSMFILE.LSM_MISC_FIELD_23,
              LSMFILE.LSM_MISC_FIELD_24,
              LSMFILE.LSM_MISC_FIELD_25,
              LSMFILE.LSM_MISC_FIELD_26,
              LSMFILE.LSM_MISC_FIELD_27,
              LSMFILE.LSM_MISC_FIELD_28,
              LSMFILE.LSM_MISC_FIELD_29,
              LSMFILE.LSM_MISC_FIELD_30,
              LSMFILE.LSM_MISC_FIELD_31,
              LSMFILE.LSM_MISC_FIELD_32)
            VALUES
             (:QLSM-BRNO,
              :QLSM-ACCTNO,
              :QLSM-SEQNO,
              NULLIF(:QLSM-LTOUCH-DATE,'1900-01-01'),
              :QLSM-MISC-FIELD-1,
              :QLSM-MISC-FIELD-2,
              :QLSM-MISC-FIELD-3,
              :QLSM-MISC-FIELD-4,
              :QLSM-MISC-FIELD-5,
              :QLSM-MISC-FIELD-6,
              :QLSM-MISC-FIELD-7,
              :QLSM-MISC-FIELD-8,
              :QLSM-MISC-FIELD-9,
              :QLSM-MISC-FIELD-10,
              :QLSM-MISC-FIELD-11,
              :QLSM-MISC-FIELD-12,
              :QLSM-MISC-FIELD-13,
              :QLSM-MISC-FIELD-14,
              :QLSM-MISC-FIELD-15,
              :QLSM-MISC-FIELD-16,
              :QLSM-MISC-FIELD-17,
              :QLSM-MISC-FIELD-18,
              :QLSM-MISC-FIELD-19,
              :QLSM-MISC-FIELD-20,
              :QLSM-MISC-FIELD-21,
              :QLSM-MISC-FIELD-22,
              :QLSM-MISC-FIELD-23,
              :QLSM-MISC-FIELD-24,
              :QLSM-MISC-FIELD-25,
              :QLSM-MISC-FIELD-26,
              :QLSM-MISC-FIELD-27,
              :QLSM-MISC-FIELD-28,
              :QLSM-MISC-FIELD-29,
              :QLSM-MISC-FIELD-30,
              :QLSM-MISC-FIELD-31,
              :QLSM-MISC-FIELD-32)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LSM1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LSM1-FILE.

           MOVE LSM-PATH-OWNBR  TO LSM-BRNO.
           ACCEPT LSM-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LSM-SELECT-PATH TO E-FILE.
           MOVE LSM1-KEY        TO E-KEYX.
           PERFORM SET-LSM-FIELDS.

           EXEC SQL
            UPDATE DBO.LSMFILE
             SET
              LSMFILE.LSM_BRNO = :QLSM-BRNO,
              LSMFILE.LSM_ACCTNO = :QLSM-ACCTNO,
              LSMFILE.LSM_SEQNO = :QLSM-SEQNO,
              LSMFILE.LSM_LTOUCH_DATE =
                 NULLIF(:QLSM-LTOUCH-DATE,'1900-01-01'),
              LSMFILE.LSM_MISC_FIELD_1 = :QLSM-MISC-FIELD-1,
              LSMFILE.LSM_MISC_FIELD_2 = :QLSM-MISC-FIELD-2,
              LSMFILE.LSM_MISC_FIELD_3 = :QLSM-MISC-FIELD-3,
              LSMFILE.LSM_MISC_FIELD_4 = :QLSM-MISC-FIELD-4,
              LSMFILE.LSM_MISC_FIELD_5 = :QLSM-MISC-FIELD-5,
              LSMFILE.LSM_MISC_FIELD_6 = :QLSM-MISC-FIELD-6,
              LSMFILE.LSM_MISC_FIELD_7 = :QLSM-MISC-FIELD-7,
              LSMFILE.LSM_MISC_FIELD_8 = :QLSM-MISC-FIELD-8,
              LSMFILE.LSM_MISC_FIELD_9 = :QLSM-MISC-FIELD-9,
              LSMFILE.LSM_MISC_FIELD_10 = :QLSM-MISC-FIELD-10,
              LSMFILE.LSM_MISC_FIELD_11 = :QLSM-MISC-FIELD-11,
              LSMFILE.LSM_MISC_FIELD_12 = :QLSM-MISC-FIELD-12,
              LSMFILE.LSM_MISC_FIELD_13 = :QLSM-MISC-FIELD-13,
              LSMFILE.LSM_MISC_FIELD_14 = :QLSM-MISC-FIELD-14,
              LSMFILE.LSM_MISC_FIELD_15 = :QLSM-MISC-FIELD-15,
              LSMFILE.LSM_MISC_FIELD_16 = :QLSM-MISC-FIELD-16,
              LSMFILE.LSM_MISC_FIELD_17 = :QLSM-MISC-FIELD-17,
              LSMFILE.LSM_MISC_FIELD_18 = :QLSM-MISC-FIELD-18,
              LSMFILE.LSM_MISC_FIELD_19 = :QLSM-MISC-FIELD-19,
              LSMFILE.LSM_MISC_FIELD_20 = :QLSM-MISC-FIELD-20,
              LSMFILE.LSM_MISC_FIELD_21 = :QLSM-MISC-FIELD-21,
              LSMFILE.LSM_MISC_FIELD_22 = :QLSM-MISC-FIELD-22,
              LSMFILE.LSM_MISC_FIELD_23 = :QLSM-MISC-FIELD-23,
              LSMFILE.LSM_MISC_FIELD_24 = :QLSM-MISC-FIELD-24,
              LSMFILE.LSM_MISC_FIELD_25 = :QLSM-MISC-FIELD-25,
              LSMFILE.LSM_MISC_FIELD_26 = :QLSM-MISC-FIELD-26,
              LSMFILE.LSM_MISC_FIELD_27 = :QLSM-MISC-FIELD-27,
              LSMFILE.LSM_MISC_FIELD_28 = :QLSM-MISC-FIELD-28,
              LSMFILE.LSM_MISC_FIELD_29 = :QLSM-MISC-FIELD-29,
              LSMFILE.LSM_MISC_FIELD_30 = :QLSM-MISC-FIELD-30,
              LSMFILE.LSM_MISC_FIELD_31 = :QLSM-MISC-FIELD-31,
              LSMFILE.LSM_MISC_FIELD_32 = :QLSM-MISC-FIELD-32
             WHERE LSMFILE.LSM_BRNO = :QLSM-BRNO
               AND LSMFILE.LSM_ACCTNO = :QLSM-ACCTNO
               AND LSMFILE.LSM_SEQNO = :QLSM-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LSM1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LSM1-FILE.

           MOVE LSM-PATH-OWNBR  TO LSM-BRNO.
           PERFORM DELETE-IT.
           MOVE LSM-SELECT-PATH TO E-FILE.
           MOVE LSM1-KEY        TO E-KEYX.
           PERFORM SET-LSM-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LSMFILE
             WHERE LSMFILE.LSM_BRNO = :QLSM-BRNO
               AND LSMFILE.LSM_ACCTNO = :QLSM-ACCTNO
               AND LSMFILE.LSM_SEQNO = :QLSM-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LSM1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LSM1-FILE.

           IF ( LSM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSM1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSM1-CURSOR-STAT.

      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
 
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
              DESTROY IOLSMI-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
