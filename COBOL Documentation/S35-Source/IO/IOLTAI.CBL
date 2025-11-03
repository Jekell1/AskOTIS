       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOLTAI.
       AUTHOR.        BAH.
       DATE-WRITTEN.  04-05-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOLTAI.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : LTAFILE 
      * TABLE DESC    : LOAN TRAILER AUTO INSURANCE FILE
      * OUTPUT FILE   : XPC/IOLTAI.PRE
      * USED FOR      : LIBLP/LPWSLTA.CPY  --- QLTA-REC
      *                 LIBLP/LPLTA1R.CPY  --- READ
      *                 LIBLP/LPLTA1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0405 ***NEW***
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
           "@(#)IO/IOLTAI.CBL S34 07/21/21-13:40:58 >".

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
       COPY "LIBLP/LP01LTA.CPY".
       COPY "LIBLP/LPWSLTA.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QLTA1-WHERE-BEG-KEY.
           03  QLTA1-WBEG-BRNO             PIC 9(04).
           03  QLTA1-WBEG-ACCTNO           PIC 9(08).
           03  QLTA1-WBEG-SEQNO            PIC 9(03).

       01  QLTA1-WHERE-END-KEY.
           03  QLTA1-WEND-BRNO             PIC 9(04).
           03  QLTA1-WEND-ACCTNO           PIC 9(08).
           03  QLTA1-WEND-SEQNO            PIC 9(03).

 
       01  QLTA-REC.
           03  QLTA-BRNO              PIC 9(4).
           03  QLTA-ACCTNO            PIC 9(8).
           03  QLTA-SEQNO             PIC 999.
           03  QLTA-LTOUCH-DATE       PIC X(10).
           03  QLTA-DLR-REQ          PIC X.
           03  QLTA-INS-TYPE         PIC X.
           03  QLTA-INS-CO           PIC X(25).
           03  QLTA-POLICY-NO        PIC X(25).
           03  QLTA-AGENCY           PIC X(25).
           03  QLTA-AGENT            PIC X(25).
           03  QLTA-ADR1             PIC X(25).
           03  QLTA-ADR2             PIC X(25).
           03  QLTA-CITY             PIC X(15).
           03  QLTA-STATE            PIC XX.
           03  QLTA-ZIP              PIC X(10).
           03  QLTA-PHONE            PIC 9(10).
           03  QLTA-EFF-DATE         PIC X(10).
           03  QLTA-EXP-DATE         PIC X(10).
           03  QLTA-CAN-DATE         PIC X(10).
           03  QLTA-CONF-DATE        PIC X(10).
           03  QLTA-LAST-FORM        PIC 9(4).
           03  QLTA-COV-TYPE         PIC X.
           03  QLTA-BIL-COV          PIC S9(7)V99.
           03  QLTA-PDL-COV          PIC S9(7)V99.
           03  QLTA-PIP-COV          PIC S9(7)V99.
           03  QLTA-UMB-COV          PIC S9(7)V99.
           03  QLTA-COL-COV          PIC S9(7)V99.
           03  QLTA-LOSS-PAYEE       PIC X(25).
           03  QLTA-COM-COV          PIC S9(7)V99.
           03  QLTA-TRACK-ONLY       PIC X.
           03  QLTA-INSURANCE        PIC X.
           03  QLTA-TRACK-MONTHS     PIC X(12).
           03  QLTA-INS-MONTHS       PIC X(12).
           03  QLTA-CPP-MONTHS       PIC X(12).

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
           03  BEG-SEQNO               PIC 9(03).
           03  END-SEQNO               PIC 9(03).

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
           03  FILLER PIC X(24) VALUE "BRNO   ACCTNO     SEQ   ".
           03  FILLER PIC X(27) VALUE "POLICY NO    AGENCY        ".
           03  FILLER PIC X(30) VALUE "EFF DATE".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-LTA-BRNO                   PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-LTA-ACCTNO                 PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-LTA-SEQNO                  PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-LTA-POLICY-NO              PIC X(10).
           03  FILLER                         PIC X(03).
           03  D01-LTA-AGENCY                 PIC X(10).
           03  FILLER                         PIC X(03).
           03  D01-LTA-EFF-DATE               PIC 9999/99/99.

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
       COPY "LIBIO/IOLTAI_DEF.CPY".
       COPY "LIBIO/IOLTAI_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOLTAI_SCN.CPY".

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
           MOVE "IOLTAI"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOLTAI-QUEUE-POS  TO Q-POS.
           MOVE IOLTAI-COPIES-POS TO C-POS.
           MOVE IOLTAI-PRU-POS    TO P-POS.
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

           MOVE "ENNN030BEG-SEQNO." TO VDU.
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

           MOVE "ENNN030END-SEQNO." TO VDU.
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

           MOVE "SNNN030BEG-SEQNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-SEQNO.
           PERFORM SCREENIO.

           MOVE "SNNN030END-SEQNO." TO VDU.
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

           MOVE "SNNN030BEG-SEQNO."       TO VDU.
           MOVE BEG-SEQNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-SEQNO."       TO VDU.
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

           MOVE "B"                        TO LTA-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QLTA1-WBEG-BRNO.
           MOVE BR-NO     TO QLTA1-WEND-BRNO.

           MOVE BEG-ACCTNO TO QLTA1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QLTA1-WEND-ACCTNO.
           MOVE BEG-SEQNO  TO QLTA1-WBEG-SEQNO.
           MOVE END-SEQNO  TO QLTA1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-LTA1-FILE
           PERFORM START-LTA1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-LTA1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LTA-BRNO NOT = LTA-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( LTA-ACCTNO < BEG-ACCTNO )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( LTA-ACCTNO > END-ACCTNO )
              GO TO PROCESS-BRANCH-MODULE-END.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE LTA-BRNO                    TO D01-LTA-BRNO.
           MOVE LTA-ACCTNO                  TO D01-LTA-ACCTNO.
           MOVE LTA-SEQNO                   TO D01-LTA-SEQNO.
           MOVE LTA-POLICY-NO               TO D01-LTA-POLICY-NO.
           MOVE LTA-AGENCY                  TO D01-LTA-AGENCY.
           MOVE LTA-EFF-DATE                TO D01-LTA-EFF-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-LTA1-FILE.

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

           MOVE LTA1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "SQL TABLE IO UTILITY - LTAFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOLTAI-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOLTAI-SYSDATE.
           DISPLAY IOLTAI-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOLTAI-FORM-FIELDS TO WR-BUF.
           MOVE IOLTAI-FORM-LIST TO WR-LIST.
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

           MOVE IOLTAI-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOLTAI-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOLTAI_EVA.CPY".
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
      *    LOAD LTA-REC FROM QLTA-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LTA-FIELDS.
           MOVE QLTA-BRNO TO LTA-BRNO.
           MOVE QLTA-ACCTNO TO LTA-ACCTNO.
           MOVE QLTA-SEQNO TO LTA-SEQNO.

           IF ( QLTA-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LTA-LTOUCH-DATE
           ELSE
              MOVE QLTA-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTA-LTOUCH-DATE.

           MOVE QLTA-DLR-REQ TO LTA-DLR-REQ.
           MOVE QLTA-INS-TYPE TO LTA-INS-TYPE.
           MOVE QLTA-INS-CO TO LTA-INS-CO.
           MOVE QLTA-POLICY-NO TO LTA-POLICY-NO.
           MOVE QLTA-AGENCY TO LTA-AGENCY.
           MOVE QLTA-AGENT TO LTA-AGENT.
           MOVE QLTA-ADR1 TO LTA-ADR1.
           MOVE QLTA-ADR2 TO LTA-ADR2.
           MOVE QLTA-CITY TO LTA-CITY.
           MOVE QLTA-STATE TO LTA-STATE.
           MOVE QLTA-ZIP TO LTA-ZIP.
           MOVE QLTA-PHONE TO LTA-PHONE.

           IF ( QLTA-EFF-DATE = "1900-01-01" )
              MOVE ZEROES TO LTA-EFF-DATE
           ELSE
              MOVE QLTA-EFF-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTA-EFF-DATE.


           IF ( QLTA-EXP-DATE = "1900-01-01" )
              MOVE ZEROES TO LTA-EXP-DATE
           ELSE
              MOVE QLTA-EXP-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTA-EXP-DATE.


           IF ( QLTA-CAN-DATE = "1900-01-01" )
              MOVE ZEROES TO LTA-CAN-DATE
           ELSE
              MOVE QLTA-CAN-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTA-CAN-DATE.


           IF ( QLTA-CONF-DATE = "1900-01-01" )
              MOVE ZEROES TO LTA-CONF-DATE
           ELSE
              MOVE QLTA-CONF-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTA-CONF-DATE.

           MOVE QLTA-LAST-FORM TO LTA-LAST-FORM.
           MOVE QLTA-COV-TYPE TO LTA-COV-TYPE.
           MOVE QLTA-BIL-COV TO LTA-BIL-COV.
           MOVE QLTA-PDL-COV TO LTA-PDL-COV.
           MOVE QLTA-PIP-COV TO LTA-PIP-COV.
           MOVE QLTA-UMB-COV TO LTA-UMB-COV.
           MOVE QLTA-COL-COV TO LTA-COL-COV.
           MOVE QLTA-LOSS-PAYEE TO LTA-LOSS-PAYEE.
           MOVE QLTA-COM-COV TO LTA-COM-COV.
           MOVE QLTA-TRACK-ONLY TO LTA-TRACK-ONLY.
           MOVE QLTA-INSURANCE TO LTA-INSURANCE.
           MOVE QLTA-TRACK-MONTHS TO LTA-TRACK-MONTHS.
           MOVE QLTA-INS-MONTHS TO LTA-INS-MONTHS.
           MOVE QLTA-CPP-MONTHS TO LTA-CPP-MONTHS.
 
      *-----------------------------------------------------------------
      *    LOAD QLTA-REC FROM LTA-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LTA-FIELDS.
           MOVE LTA-BRNO TO QLTA-BRNO.
           MOVE LTA-ACCTNO TO QLTA-ACCTNO.
           MOVE LTA-SEQNO TO QLTA-SEQNO.

           IF ( LTA-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTA-LTOUCH-DATE
           ELSE
              MOVE LTA-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTA-LTOUCH-DATE.

           MOVE LTA-DLR-REQ TO QLTA-DLR-REQ.
           MOVE LTA-INS-TYPE TO QLTA-INS-TYPE.
           MOVE LTA-INS-CO TO QLTA-INS-CO.
           MOVE LTA-POLICY-NO TO QLTA-POLICY-NO.
           MOVE LTA-AGENCY TO QLTA-AGENCY.
           MOVE LTA-AGENT TO QLTA-AGENT.
           MOVE LTA-ADR1 TO QLTA-ADR1.
           MOVE LTA-ADR2 TO QLTA-ADR2.
           MOVE LTA-CITY TO QLTA-CITY.
           MOVE LTA-STATE TO QLTA-STATE.
           MOVE LTA-ZIP TO QLTA-ZIP.
           MOVE LTA-PHONE TO QLTA-PHONE.

           IF ( LTA-EFF-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTA-EFF-DATE
           ELSE
              MOVE LTA-EFF-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTA-EFF-DATE.


           IF ( LTA-EXP-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTA-EXP-DATE
           ELSE
              MOVE LTA-EXP-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTA-EXP-DATE.


           IF ( LTA-CAN-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTA-CAN-DATE
           ELSE
              MOVE LTA-CAN-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTA-CAN-DATE.


           IF ( LTA-CONF-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTA-CONF-DATE
           ELSE
              MOVE LTA-CONF-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTA-CONF-DATE.

           MOVE LTA-LAST-FORM TO QLTA-LAST-FORM.
           MOVE LTA-COV-TYPE TO QLTA-COV-TYPE.
           MOVE LTA-BIL-COV TO QLTA-BIL-COV.
           MOVE LTA-PDL-COV TO QLTA-PDL-COV.
           MOVE LTA-PIP-COV TO QLTA-PIP-COV.
           MOVE LTA-UMB-COV TO QLTA-UMB-COV.
           MOVE LTA-COL-COV TO QLTA-COL-COV.
           MOVE LTA-LOSS-PAYEE TO QLTA-LOSS-PAYEE.
           MOVE LTA-COM-COV TO QLTA-COM-COV.
           MOVE LTA-TRACK-ONLY TO QLTA-TRACK-ONLY.
           MOVE LTA-INSURANCE TO QLTA-INSURANCE.
           MOVE LTA-TRACK-MONTHS TO QLTA-TRACK-MONTHS.
           MOVE LTA-INS-MONTHS TO QLTA-INS-MONTHS.
           MOVE LTA-CPP-MONTHS TO QLTA-CPP-MONTHS.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LTA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTA-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTA-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTA-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTA-ALL-DATA
              MOVE LTA-ALL-PATH             TO LTA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTA-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTA-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTA-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTA-ALL-DATA
              MOVE LTA-ALL-PATH             TO LTA-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LTAFILE NOT = "S" )
              MOVE LTA-PATH-BEG             TO LTA-SELECT-PATH-BEG
              MOVE LTA-PATH-FILE            TO LTA-SELECT-PATH-FILE
           ELSE
              MOVE LTA-PATH-SQL             TO LTA-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LTA1-FILE.
           PERFORM LOAD-LTA1-FILE.
           PERFORM OPEN-IT.
           MOVE LTA-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTA1-FILE.
           PERFORM START-IT.
           MOVE LTA-PATH-OWNBR  TO LTA-BRNO.
           MOVE LTA-SELECT-PATH TO E-FILE.
           MOVE LTA1-KEY        TO E-KEYX.

           IF ( LTA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTA1-FILE.

           IF ( QLTA1-WEND-BRNO = ZEROES    ) OR
              ( QLTA1-WEND-BRNO NOT NUMERIC )
              MOVE LTA-PATH-OWNBR        TO QLTA1-WBEG-BRNO
              MOVE LTA-PATH-OWNBR        TO QLTA1-WEND-BRNO
              MOVE LTA-ACCTNO            TO QLTA1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTA1-WEND-ACCTNO
              MOVE LTA-SEQNO             TO QLTA1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTA1-WEND-SEQNO.

           EXEC SQL
            DECLARE LTA1_CURSOR CURSOR FOR
            SELECT LTAFILE.LTA_BRNO,
                   LTAFILE.LTA_ACCTNO,
                   LTAFILE.LTA_SEQNO,
                   ISNULL(CAST(LTAFILE.LTA_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTAFILE.LTA_DLR_REQ,''),
                   ISNULL(LTAFILE.LTA_INS_TYPE,''),
                   ISNULL(LTAFILE.LTA_INS_CO,''),
                   ISNULL(LTAFILE.LTA_POLICY_NO,''),
                   ISNULL(LTAFILE.LTA_AGENCY,''),
                   ISNULL(LTAFILE.LTA_AGENT,''),
                   ISNULL(LTAFILE.LTA_ADR1,''),
                   ISNULL(LTAFILE.LTA_ADR2,''),
                   ISNULL(LTAFILE.LTA_CITY,''),
                   ISNULL(LTAFILE.LTA_STATE,''),
                   ISNULL(LTAFILE.LTA_ZIP,''),
                   LTAFILE.LTA_PHONE,
                   ISNULL(CAST(LTAFILE.LTA_EFF_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LTAFILE.LTA_EXP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LTAFILE.LTA_CAN_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LTAFILE.LTA_CONF_DATE
                                                  AS VARCHAR(10)),'0'),
                   LTAFILE.LTA_LAST_FORM,
                   ISNULL(LTAFILE.LTA_COV_TYPE,''),
                   LTAFILE.LTA_BIL_COV,
                   LTAFILE.LTA_PDL_COV,
                   LTAFILE.LTA_PIP_COV,
                   LTAFILE.LTA_UMB_COV,
                   LTAFILE.LTA_COL_COV,
                   ISNULL(LTAFILE.LTA_LOSS_PAYEE,''),
                   LTAFILE.LTA_COM_COV,
                   ISNULL(LTAFILE.LTA_TRACK_ONLY,''),
                   ISNULL(LTAFILE.LTA_INSURANCE,''),
                   ISNULL(LTAFILE.LTA_TRACK_MONTHS,''),
                   ISNULL(LTAFILE.LTA_INS_MONTHS,''),
                   ISNULL(LTAFILE.LTA_CPP_MONTHS,'')
            FROM DBO.LTAFILE
            WHERE LTAFILE.LTA_BRNO >= :QLTA1-WBEG-BRNO
              AND LTAFILE.LTA_BRNO <= :QLTA1-WEND-BRNO
              AND LTAFILE.LTA_ACCTNO >= :QLTA1-WBEG-ACCTNO
              AND LTAFILE.LTA_ACCTNO <= :QLTA1-WEND-ACCTNO
              AND LTAFILE.LTA_SEQNO >= :QLTA1-WBEG-SEQNO
              AND LTAFILE.LTA_SEQNO <= :QLTA1-WEND-SEQNO
            ORDER BY LTAFILE.LTA_BRNO,
                     LTAFILE.LTA_ACCTNO,
                     LTAFILE.LTA_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTA1-WBEG-BRNO
                          QLTA1-WEND-BRNO
                          QLTA1-WBEG-ACCTNO
                          QLTA1-WEND-ACCTNO
                          QLTA1-WBEG-SEQNO
                          QLTA1-WEND-SEQNO.

           MOVE STAT---GOOD TO LTA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTA1-FILE.
           PERFORM READ-IT.
           MOVE LTA-PATH-OWNBR  TO LTA-BRNO. 
           MOVE LTA-SELECT-PATH TO E-FILE.
           MOVE LTA1-KEY        TO E-KEYX.

           MOVE LTA-BRNO   TO QLTA-BRNO.
           MOVE LTA-ACCTNO TO QLTA-ACCTNO.
           MOVE LTA-SEQNO  TO QLTA-SEQNO.

           EXEC SQL
            SELECT LTAFILE.LTA_BRNO,
                   LTAFILE.LTA_ACCTNO,
                   LTAFILE.LTA_SEQNO,
                   ISNULL(CAST(LTAFILE.LTA_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTAFILE.LTA_DLR_REQ,''),
                   ISNULL(LTAFILE.LTA_INS_TYPE,''),
                   ISNULL(LTAFILE.LTA_INS_CO,''),
                   ISNULL(LTAFILE.LTA_POLICY_NO,''),
                   ISNULL(LTAFILE.LTA_AGENCY,''),
                   ISNULL(LTAFILE.LTA_AGENT,''),
                   ISNULL(LTAFILE.LTA_ADR1,''),
                   ISNULL(LTAFILE.LTA_ADR2,''),
                   ISNULL(LTAFILE.LTA_CITY,''),
                   ISNULL(LTAFILE.LTA_STATE,''),
                   ISNULL(LTAFILE.LTA_ZIP,''),
                   LTAFILE.LTA_PHONE,
                   ISNULL(CAST(LTAFILE.LTA_EFF_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LTAFILE.LTA_EXP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LTAFILE.LTA_CAN_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LTAFILE.LTA_CONF_DATE
                                                  AS VARCHAR(10)),'0'),
                   LTAFILE.LTA_LAST_FORM,
                   ISNULL(LTAFILE.LTA_COV_TYPE,''),
                   LTAFILE.LTA_BIL_COV,
                   LTAFILE.LTA_PDL_COV,
                   LTAFILE.LTA_PIP_COV,
                   LTAFILE.LTA_UMB_COV,
                   LTAFILE.LTA_COL_COV,
                   ISNULL(LTAFILE.LTA_LOSS_PAYEE,''),
                   LTAFILE.LTA_COM_COV,
                   ISNULL(LTAFILE.LTA_TRACK_ONLY,''),
                   ISNULL(LTAFILE.LTA_INSURANCE,''),
                   ISNULL(LTAFILE.LTA_TRACK_MONTHS,''),
                   ISNULL(LTAFILE.LTA_INS_MONTHS,''),
                   ISNULL(LTAFILE.LTA_CPP_MONTHS,'')
            INTO :QLTA-REC
            FROM DBO.LTAFILE
            WHERE LTAFILE.LTA_BRNO = :QLTA-BRNO
              AND LTAFILE.LTA_ACCTNO = :QLTA-ACCTNO
              AND LTAFILE.LTA_SEQNO = :QLTA-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTA1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTA-FIELDS.

      *-----------------------------------------------------------------
       READ-LTA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTA-PATH-OWNBR  TO LTA-BRNO.
           MOVE LTA-SELECT-PATH TO E-FILE.
           MOVE LTA1-KEY        TO E-KEYX.
           INITIALIZE QLTA-REC.

           IF ( LTA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTA1_CURSOR INTO :QLTA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTA-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LTA1-FILE.

           MOVE LTA-PATH-OWNBR  TO LTA-BRNO.
           ACCEPT LTA-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LTA-SELECT-PATH TO E-FILE.
           MOVE LTA1-KEY        TO E-KEYX.
           PERFORM SET-LTA-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LTAFILE
             (LTAFILE.LTA_BRNO,
              LTAFILE.LTA_ACCTNO,
              LTAFILE.LTA_SEQNO,
              LTAFILE.LTA_LTOUCH_DATE,
              LTAFILE.LTA_DLR_REQ,
              LTAFILE.LTA_INS_TYPE,
              LTAFILE.LTA_INS_CO,
              LTAFILE.LTA_POLICY_NO,
              LTAFILE.LTA_AGENCY,
              LTAFILE.LTA_AGENT,
              LTAFILE.LTA_ADR1,
              LTAFILE.LTA_ADR2,
              LTAFILE.LTA_CITY,
              LTAFILE.LTA_STATE,
              LTAFILE.LTA_ZIP,
              LTAFILE.LTA_PHONE,
              LTAFILE.LTA_EFF_DATE,
              LTAFILE.LTA_EXP_DATE,
              LTAFILE.LTA_CAN_DATE,
              LTAFILE.LTA_CONF_DATE,
              LTAFILE.LTA_LAST_FORM,
              LTAFILE.LTA_COV_TYPE,
              LTAFILE.LTA_BIL_COV,
              LTAFILE.LTA_PDL_COV,
              LTAFILE.LTA_PIP_COV,
              LTAFILE.LTA_UMB_COV,
              LTAFILE.LTA_COL_COV,
              LTAFILE.LTA_LOSS_PAYEE,
              LTAFILE.LTA_COM_COV,
              LTAFILE.LTA_TRACK_ONLY,
              LTAFILE.LTA_INSURANCE,
              LTAFILE.LTA_TRACK_MONTHS,
              LTAFILE.LTA_INS_MONTHS,
              LTAFILE.LTA_CPP_MONTHS)
            VALUES
             (:QLTA-BRNO,
              :QLTA-ACCTNO,
              :QLTA-SEQNO,
              NULLIF(:QLTA-LTOUCH-DATE,'1900-01-01'),
              :QLTA-DLR-REQ,
              :QLTA-INS-TYPE,
              :QLTA-INS-CO,
              :QLTA-POLICY-NO,
              :QLTA-AGENCY,
              :QLTA-AGENT,
              :QLTA-ADR1,
              :QLTA-ADR2,
              :QLTA-CITY,
              :QLTA-STATE,
              :QLTA-ZIP,
              :QLTA-PHONE,
              NULLIF(:QLTA-EFF-DATE,'1900-01-01'),
              NULLIF(:QLTA-EXP-DATE,'1900-01-01'),
              NULLIF(:QLTA-CAN-DATE,'1900-01-01'),
              NULLIF(:QLTA-CONF-DATE,'1900-01-01'),
              :QLTA-LAST-FORM,
              :QLTA-COV-TYPE,
              :QLTA-BIL-COV,
              :QLTA-PDL-COV,
              :QLTA-PIP-COV,
              :QLTA-UMB-COV,
              :QLTA-COL-COV,
              :QLTA-LOSS-PAYEE,
              :QLTA-COM-COV,
              :QLTA-TRACK-ONLY,
              :QLTA-INSURANCE,
              :QLTA-TRACK-MONTHS,
              :QLTA-INS-MONTHS,
              :QLTA-CPP-MONTHS)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LTA1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LTA1-FILE.

           MOVE LTA-PATH-OWNBR  TO LTA-BRNO.
           ACCEPT LTA-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LTA-SELECT-PATH TO E-FILE.
           MOVE LTA1-KEY        TO E-KEYX.
           PERFORM SET-LTA-FIELDS.

           EXEC SQL
            UPDATE DBO.LTAFILE
             SET
              LTAFILE.LTA_BRNO = :QLTA-BRNO,
              LTAFILE.LTA_ACCTNO = :QLTA-ACCTNO,
              LTAFILE.LTA_SEQNO = :QLTA-SEQNO,
              LTAFILE.LTA_LTOUCH_DATE =
                 NULLIF(:QLTA-LTOUCH-DATE,'1900-01-01'),
              LTAFILE.LTA_DLR_REQ = :QLTA-DLR-REQ,
              LTAFILE.LTA_INS_TYPE = :QLTA-INS-TYPE,
              LTAFILE.LTA_INS_CO = :QLTA-INS-CO,
              LTAFILE.LTA_POLICY_NO = :QLTA-POLICY-NO,
              LTAFILE.LTA_AGENCY = :QLTA-AGENCY,
              LTAFILE.LTA_AGENT = :QLTA-AGENT,
              LTAFILE.LTA_ADR1 = :QLTA-ADR1,
              LTAFILE.LTA_ADR2 = :QLTA-ADR2,
              LTAFILE.LTA_CITY = :QLTA-CITY,
              LTAFILE.LTA_STATE = :QLTA-STATE,
              LTAFILE.LTA_ZIP = :QLTA-ZIP,
              LTAFILE.LTA_PHONE = :QLTA-PHONE,
              LTAFILE.LTA_EFF_DATE =
                 NULLIF(:QLTA-EFF-DATE,'1900-01-01'),
              LTAFILE.LTA_EXP_DATE =
                 NULLIF(:QLTA-EXP-DATE,'1900-01-01'),
              LTAFILE.LTA_CAN_DATE =
                 NULLIF(:QLTA-CAN-DATE,'1900-01-01'),
              LTAFILE.LTA_CONF_DATE =
                 NULLIF(:QLTA-CONF-DATE,'1900-01-01'),
              LTAFILE.LTA_LAST_FORM = :QLTA-LAST-FORM,
              LTAFILE.LTA_COV_TYPE = :QLTA-COV-TYPE,
              LTAFILE.LTA_BIL_COV = :QLTA-BIL-COV,
              LTAFILE.LTA_PDL_COV = :QLTA-PDL-COV,
              LTAFILE.LTA_PIP_COV = :QLTA-PIP-COV,
              LTAFILE.LTA_UMB_COV = :QLTA-UMB-COV,
              LTAFILE.LTA_COL_COV = :QLTA-COL-COV,
              LTAFILE.LTA_LOSS_PAYEE = :QLTA-LOSS-PAYEE,
              LTAFILE.LTA_COM_COV = :QLTA-COM-COV,
              LTAFILE.LTA_TRACK_ONLY = :QLTA-TRACK-ONLY,
              LTAFILE.LTA_INSURANCE = :QLTA-INSURANCE,
              LTAFILE.LTA_TRACK_MONTHS = :QLTA-TRACK-MONTHS,
              LTAFILE.LTA_INS_MONTHS = :QLTA-INS-MONTHS,
              LTAFILE.LTA_CPP_MONTHS = :QLTA-CPP-MONTHS
             WHERE LTAFILE.LTA_BRNO = :QLTA-BRNO
               AND LTAFILE.LTA_ACCTNO = :QLTA-ACCTNO
               AND LTAFILE.LTA_SEQNO = :QLTA-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LTA1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LTA1-FILE.

           MOVE LTA-PATH-OWNBR  TO LTA-BRNO.
           PERFORM DELETE-IT.
           MOVE LTA-SELECT-PATH TO E-FILE.
           MOVE LTA1-KEY        TO E-KEYX.
           PERFORM SET-LTA-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LTAFILE
             WHERE LTAFILE.LTA_BRNO = :QLTA-BRNO
               AND LTAFILE.LTA_ACCTNO = :QLTA-ACCTNO
               AND LTAFILE.LTA_SEQNO = :QLTA-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LTA1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LTA1-FILE.

           IF ( LTA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTA1-CURSOR-STAT.


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
              DESTROY IOLTAI-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
