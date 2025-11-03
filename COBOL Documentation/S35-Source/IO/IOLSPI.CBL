       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOLSPI.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-12-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOLSPI.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : LSPFILE 
      * TABLE DESC    : LOAN TRAILER PAID THRU (DEFERMENT) FILE
      * OUTPUT FILE   : XPC/IOLSPI.PRE
      * USED FOR      : LIBLP/LPWSLSP.CPY  --- QLSP-REC
      *                 LIBLP/LPLSP1R.CPY  --- READ
      *                 LIBLP/LPLSP1RN.CPY --- READ/READ-NEXT/START
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
           "@(#)IO/IOLSPI.CBL S34 05/12/21-12:21:50 >".

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
       COPY "LIBLP/LP01LSP.CPY".
       COPY "LIBLP/LPWSLSP.CPY".
      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QLSP1-WHERE-BEG-KEY.
           03  QLSP1-WBEG-BRNO             PIC 9(04).
           03  QLSP1-WBEG-ACCTNO           PIC 9(08).
           03  QLSP1-WBEG-SEQNO            PIC 9(02).

       01  QLSP1-WHERE-END-KEY.
           03  QLSP1-WEND-BRNO             PIC 9(04).
           03  QLSP1-WEND-ACCTNO           PIC 9(08).
           03  QLSP1-WEND-SEQNO            PIC 9(02).

       01  QLSP-REC.
           03  QLSP-BRNO               PIC 9(4).
           03  QLSP-ACCTNO             PIC 9(8).
           03  QLSP-SEQNO              PIC 99.
           03  QLSP-LTOUCH-DATE        PIC X(10).
           03  QLSP-REASON-CODE           PIC XX.
           03  QLSP-PERIODS-TO-DEFER      PIC 99.
           03  QLSP-REASON-FOR-DEFER      PIC X(50).
           03  QLSP-MORTGAGE-PAST-DUE     PIC X.
           03  QLSP-MORTGAGE-BALANCE      PIC S9(7)V99.
           03  QLSP-ATTEMPT-REFINANCE     PIC X(50).
           03  QLSP-INFO-DATE             PIC X(10).
           03  QLSP-INFO-USERID           PIC X(10).
           03  QLSP-BR-MANAGER-COMMENTS   PIC X(50).
           03  QLSP-BR-MANAGER-DATE       PIC X(10).
           03  QLSP-BR-MANAGER-USERID     PIC X(10).
           03  QLSP-AVP-COMMENTS          PIC X(50).
           03  QLSP-AVP-DATE              PIC X(10).
           03  QLSP-AVP-USERID            PIC X(10).
           03  QLSP-AVP-APPROVE           PIC X.
           03  QLSP-VP-COMMENTS           PIC X(50).
           03  QLSP-VP-DATE               PIC X(10).
           03  QLSP-VP-USERID             PIC X(10).
           03  QLSP-VP-APPROVE            PIC X.
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
           03  FILLER PIC X(28) VALUE "BRNO   ACCTNO    SEQ   RE   ".
           03  FILLER PIC X(30) VALUE "INFODATE    USERID".
           03  FILLER PIC X(06) VALUE "ATE CD".


      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-LSP-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-LSP-ACCTNO                  PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-LSP-SEQNO                   PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-LSP-REASON-CODE            PIC XX.
           03  FILLER                         PIC X(03).
           03  D01-LSP-INFO-DATE              PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-LSP-INFO-USERID            PIC X(10).

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
       COPY "LIBIO/IOLSPI_DEF.CPY".
       COPY "LIBIO/IOLSPI_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOLSPI_SCN.CPY".

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
           MOVE "IOLSPI"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOLSPI-QUEUE-POS  TO Q-POS.
           MOVE IOLSPI-COPIES-POS TO C-POS.
           MOVE IOLSPI-PRU-POS    TO P-POS.
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

           MOVE "B"                        TO LSP-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }

           MOVE BR-NO     TO QLSP1-WBEG-BRNO.
           MOVE BR-NO     TO QLSP1-WEND-BRNO.

           MOVE BEG-ACCTNO TO QLSP1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QLSP1-WEND-ACCTNO.
           MOVE BEG-SEQNO  TO QLSP1-WBEG-SEQNO.
           MOVE END-SEQNO  TO QLSP1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-LSP1-FILE.
           PERFORM START-LSP1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-LSP1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LSP-BRNO NOT = LSP-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( LSP-ACCTNO < BEG-ACCTNO )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( LSP-ACCTNO > END-ACCTNO )
              GO TO PROCESS-BRANCH-MODULE-END.

           IF LSP-SEQNO < BEG-SEQNO OR > END-SEQNO
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE LSP-BRNO                    TO D01-LSP-BRNO.
           MOVE LSP-ACCTNO                  TO D01-LSP-ACCTNO.
           MOVE LSP-SEQNO                   TO D01-LSP-SEQNO.
           MOVE LSP-REASON-CODE  TO D01-LSP-REASON-CODE.
           MOVE LSP-INFO-DATE    TO D01-LSP-INFO-DATE.
           MOVE LSP-INFO-USERID  TO D01-LSP-INFO-USERID.

           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-LSP1-FILE.

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

              MOVE LSP1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "SQL TABLE IO UTILITY - LSPFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOLSPI-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOLSPI-SYSDATE.
           DISPLAY IOLSPI-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOLSPI-FORM-FIELDS TO WR-BUF.
           MOVE IOLSPI-FORM-LIST TO WR-LIST.
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

           MOVE IOLSPI-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOLSPI-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOLSPI_EVA.CPY".
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
      *    LOAD LSP-REC FROM QLSP-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LSP-FIELDS.
           MOVE QLSP-BRNO TO LSP-BRNO.
           MOVE QLSP-ACCTNO TO LSP-ACCTNO.
           MOVE QLSP-SEQNO TO LSP-SEQNO.

           IF ( QLSP-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LSP-LTOUCH-DATE
           ELSE
              MOVE QLSP-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSP-LTOUCH-DATE.

           MOVE QLSP-REASON-CODE TO LSP-REASON-CODE.
           MOVE QLSP-PERIODS-TO-DEFER TO LSP-PERIODS-TO-DEFER.
           MOVE QLSP-REASON-FOR-DEFER TO LSP-REASON-FOR-DEFER.
           MOVE QLSP-MORTGAGE-PAST-DUE TO LSP-MORTGAGE-PAST-DUE.
           MOVE QLSP-MORTGAGE-BALANCE TO LSP-MORTGAGE-BALANCE.
           MOVE QLSP-ATTEMPT-REFINANCE TO LSP-ATTEMPT-REFINANCE.

           IF ( QLSP-INFO-DATE = "1900-01-01" )
              MOVE ZEROES TO LSP-INFO-DATE
           ELSE
              MOVE QLSP-INFO-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSP-INFO-DATE.

           MOVE QLSP-INFO-USERID TO LSP-INFO-USERID.
           MOVE QLSP-BR-MANAGER-COMMENTS TO LSP-BR-MANAGER-COMMENTS.

           IF ( QLSP-BR-MANAGER-DATE = "1900-01-01" )
              MOVE ZEROES TO LSP-BR-MANAGER-DATE
           ELSE
              MOVE QLSP-BR-MANAGER-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSP-BR-MANAGER-DATE.

           MOVE QLSP-BR-MANAGER-USERID TO LSP-BR-MANAGER-USERID.
           MOVE QLSP-AVP-COMMENTS TO LSP-AVP-COMMENTS.

           IF ( QLSP-AVP-DATE = "1900-01-01" )
              MOVE ZEROES TO LSP-AVP-DATE
           ELSE
              MOVE QLSP-AVP-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSP-AVP-DATE.

           MOVE QLSP-AVP-USERID TO LSP-AVP-USERID.
           MOVE QLSP-AVP-APPROVE TO LSP-AVP-APPROVE.
           MOVE QLSP-VP-COMMENTS TO LSP-VP-COMMENTS.

           IF ( QLSP-VP-DATE = "1900-01-01" )
              MOVE ZEROES TO LSP-VP-DATE
           ELSE
              MOVE QLSP-VP-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSP-VP-DATE.

           MOVE QLSP-VP-USERID TO LSP-VP-USERID.
           MOVE QLSP-VP-APPROVE TO LSP-VP-APPROVE.
 
      *-----------------------------------------------------------------
      *    LOAD QLSP-REC FROM LSP-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LSP-FIELDS.
           MOVE LSP-BRNO TO QLSP-BRNO.
           MOVE LSP-ACCTNO TO QLSP-ACCTNO.
           MOVE LSP-SEQNO TO QLSP-SEQNO.

           IF ( LSP-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSP-LTOUCH-DATE
           ELSE
              MOVE LSP-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSP-LTOUCH-DATE.

           MOVE LSP-REASON-CODE TO QLSP-REASON-CODE.
           MOVE LSP-PERIODS-TO-DEFER TO QLSP-PERIODS-TO-DEFER.
           MOVE LSP-REASON-FOR-DEFER TO QLSP-REASON-FOR-DEFER.
           MOVE LSP-MORTGAGE-PAST-DUE TO QLSP-MORTGAGE-PAST-DUE.
           MOVE LSP-MORTGAGE-BALANCE TO QLSP-MORTGAGE-BALANCE.
           MOVE LSP-ATTEMPT-REFINANCE TO QLSP-ATTEMPT-REFINANCE.

           IF ( LSP-INFO-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSP-INFO-DATE
           ELSE
              MOVE LSP-INFO-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSP-INFO-DATE.

           MOVE LSP-INFO-USERID TO QLSP-INFO-USERID.
           MOVE LSP-BR-MANAGER-COMMENTS TO QLSP-BR-MANAGER-COMMENTS.

           IF ( LSP-BR-MANAGER-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSP-BR-MANAGER-DATE
           ELSE
              MOVE LSP-BR-MANAGER-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSP-BR-MANAGER-DATE.

           MOVE LSP-BR-MANAGER-USERID TO QLSP-BR-MANAGER-USERID.
           MOVE LSP-AVP-COMMENTS TO QLSP-AVP-COMMENTS.

           IF ( LSP-AVP-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSP-AVP-DATE
           ELSE
              MOVE LSP-AVP-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSP-AVP-DATE.

           MOVE LSP-AVP-USERID TO QLSP-AVP-USERID.
           MOVE LSP-AVP-APPROVE TO QLSP-AVP-APPROVE.
           MOVE LSP-VP-COMMENTS TO QLSP-VP-COMMENTS.

           IF ( LSP-VP-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSP-VP-DATE
           ELSE
              MOVE LSP-VP-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSP-VP-DATE.

           MOVE LSP-VP-USERID TO QLSP-VP-USERID.
           MOVE LSP-VP-APPROVE TO QLSP-VP-APPROVE.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LSP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LSP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LSP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LSP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LSP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LSP-ALL-DATA
              MOVE LSP-ALL-PATH             TO LSP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LSP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LSP-ALL-BASE
              MOVE FILPATH-MACHINE         TO LSP-ALL-MACHINE
              MOVE FILPATH-DATA            TO LSP-ALL-DATA
              MOVE LSP-ALL-PATH             TO LSP-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LSPFILE NOT = "S" )
              MOVE LSP-PATH-BEG             TO LSP-SELECT-PATH-BEG
              MOVE LSP-PATH-FILE            TO LSP-SELECT-PATH-FILE
           ELSE
              MOVE LSP-PATH-SQL             TO LSP-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LSP1-FILE.
           PERFORM LOAD-LSP1-FILE.
           PERFORM OPEN-IT.
           MOVE LSP-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LSP1-FILE.
           PERFORM START-IT.
           MOVE LSP-PATH-OWNBR  TO LSP-BRNO.
           MOVE LSP-SELECT-PATH TO E-FILE.
           MOVE LSP1-KEY        TO E-KEYX.

           IF ( LSP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LSP1-FILE.

           IF ( QLSP1-WEND-BRNO = ZEROES    ) OR
              ( QLSP1-WEND-BRNO NOT NUMERIC )
              MOVE LSP-PATH-OWNBR        TO QLSP1-WBEG-BRNO
              MOVE LSP-PATH-OWNBR        TO QLSP1-WEND-BRNO
              MOVE LSP-ACCTNO            TO QLSP1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLSP1-WEND-ACCTNO
              MOVE LSP-SEQNO             TO QLSP1-WBEG-SEQNO
              MOVE ALL "9"               TO QLSP1-WEND-SEQNO.

           EXEC SQL
            DECLARE LSP1_CURSOR CURSOR FOR
            SELECT LSPFILE.LSP_BRNO,
                   LSPFILE.LSP_ACCTNO,
                   LSPFILE.LSP_SEQNO,
                   ISNULL(CAST(LSPFILE.LSP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSPFILE.LSP_REASON_CODE,''),
                   LSPFILE.LSP_PERIODS_TO_DEFER,
                   ISNULL(LSPFILE.LSP_REASON_FOR_DEFER,''),
                   ISNULL(LSPFILE.LSP_MORTGAGE_PAST_DUE,''),
                   LSPFILE.LSP_MORTGAGE_BALANCE,
                   ISNULL(LSPFILE.LSP_ATTEMPT_REFINANCE,''),
                   ISNULL(CAST(LSPFILE.LSP_INFO_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSPFILE.LSP_INFO_USERID,''),
                   ISNULL(LSPFILE.LSP_BR_MANAGER_COMMENTS,''),
                   ISNULL(CAST(LSPFILE.LSP_BR_MANAGER_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSPFILE.LSP_BR_MANAGER_USERID,''),
                   ISNULL(LSPFILE.LSP_AVP_COMMENTS,''),
                   ISNULL(CAST(LSPFILE.LSP_AVP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSPFILE.LSP_AVP_USERID,''),
                   ISNULL(LSPFILE.LSP_AVP_APPROVE,''),
                   ISNULL(LSPFILE.LSP_VP_COMMENTS,''),
                   ISNULL(CAST(LSPFILE.LSP_VP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSPFILE.LSP_VP_USERID,''),
                   ISNULL(LSPFILE.LSP_VP_APPROVE,'')
            FROM DBO.LSPFILE
            WHERE LSPFILE.LSP_BRNO >= :QLSP1-WBEG-BRNO
              AND LSPFILE.LSP_BRNO <= :QLSP1-WEND-BRNO
              AND LSPFILE.LSP_ACCTNO >= :QLSP1-WBEG-ACCTNO
              AND LSPFILE.LSP_ACCTNO <= :QLSP1-WEND-ACCTNO
              AND LSPFILE.LSP_SEQNO >= :QLSP1-WBEG-SEQNO
              AND LSPFILE.LSP_SEQNO <= :QLSP1-WEND-SEQNO
            ORDER BY LSPFILE.LSP_BRNO,
                     LSPFILE.LSP_ACCTNO,
                     LSPFILE.LSP_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LSP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLSP1-WBEG-BRNO
                          QLSP1-WEND-BRNO
                          QLSP1-WBEG-ACCTNO
                          QLSP1-WEND-ACCTNO
                          QLSP1-WBEG-SEQNO
                          QLSP1-WEND-SEQNO.

           MOVE STAT---GOOD TO LSP1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LSP1-FILE.
           PERFORM READ-IT.
           MOVE LSP-PATH-OWNBR  TO LSP-BRNO. 
           MOVE LSP-SELECT-PATH TO E-FILE.
           MOVE LSP1-KEY        TO E-KEYX.

           MOVE LSP-BRNO   TO QLSP-BRNO.
           MOVE LSP-ACCTNO TO QLSP-ACCTNO.
           MOVE LSP-SEQNO TO QLSP-SEQNO.

           EXEC SQL
            SELECT LSPFILE.LSP_BRNO,
                   LSPFILE.LSP_ACCTNO,
                   LSPFILE.LSP_SEQNO,
                   ISNULL(CAST(LSPFILE.LSP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSPFILE.LSP_REASON_CODE,''),
                   LSPFILE.LSP_PERIODS_TO_DEFER,
                   ISNULL(LSPFILE.LSP_REASON_FOR_DEFER,''),
                   ISNULL(LSPFILE.LSP_MORTGAGE_PAST_DUE,''),
                   LSPFILE.LSP_MORTGAGE_BALANCE,
                   ISNULL(LSPFILE.LSP_ATTEMPT_REFINANCE,''),
                   ISNULL(CAST(LSPFILE.LSP_INFO_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSPFILE.LSP_INFO_USERID,''),
                   ISNULL(LSPFILE.LSP_BR_MANAGER_COMMENTS,''),
                   ISNULL(CAST(LSPFILE.LSP_BR_MANAGER_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSPFILE.LSP_BR_MANAGER_USERID,''),
                   ISNULL(LSPFILE.LSP_AVP_COMMENTS,''),
                   ISNULL(CAST(LSPFILE.LSP_AVP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSPFILE.LSP_AVP_USERID,''),
                   ISNULL(LSPFILE.LSP_AVP_APPROVE,''),
                   ISNULL(LSPFILE.LSP_VP_COMMENTS,''),
                   ISNULL(CAST(LSPFILE.LSP_VP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSPFILE.LSP_VP_USERID,''),
                   ISNULL(LSPFILE.LSP_VP_APPROVE,'')
            INTO :QLSP-REC
            FROM DBO.LSPFILE
            WHERE LSPFILE.LSP_BRNO = :QLSP-BRNO
              AND LSPFILE.LSP_ACCTNO = :QLSP-ACCTNO
              AND LSPFILE.LSP_SEQNO = :QLSP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LSP-FIELDS.

      *-----------------------------------------------------------------
       READ-LSP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LSP-PATH-OWNBR  TO LSP-BRNO.
           MOVE LSP-SELECT-PATH TO E-FILE.
           MOVE LSP1-KEY        TO E-KEYX.
           INITIALIZE QLSP-REC.

           IF ( LSP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LSP1_CURSOR INTO :QLSP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LSP-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LSP1-FILE.

           MOVE LSP-PATH-OWNBR  TO LSP-BRNO.
           ACCEPT LSP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LSP-SELECT-PATH TO E-FILE.
           MOVE LSP1-KEY        TO E-KEYX.
           PERFORM SET-LSP-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LSPFILE
             (LSPFILE.LSP_BRNO,
              LSPFILE.LSP_ACCTNO,
              LSPFILE.LSP_SEQNO,
              LSPFILE.LSP_LTOUCH_DATE,
              LSPFILE.LSP_REASON_CODE,
              LSPFILE.LSP_PERIODS_TO_DEFER,
              LSPFILE.LSP_REASON_FOR_DEFER,
              LSPFILE.LSP_MORTGAGE_PAST_DUE,
              LSPFILE.LSP_MORTGAGE_BALANCE,
              LSPFILE.LSP_ATTEMPT_REFINANCE,
              LSPFILE.LSP_INFO_DATE,
              LSPFILE.LSP_INFO_USERID,
              LSPFILE.LSP_BR_MANAGER_COMMENTS,
              LSPFILE.LSP_BR_MANAGER_DATE,
              LSPFILE.LSP_BR_MANAGER_USERID,
              LSPFILE.LSP_AVP_COMMENTS,
              LSPFILE.LSP_AVP_DATE,
              LSPFILE.LSP_AVP_USERID,
              LSPFILE.LSP_AVP_APPROVE,
              LSPFILE.LSP_VP_COMMENTS,
              LSPFILE.LSP_VP_DATE,
              LSPFILE.LSP_VP_USERID,
              LSPFILE.LSP_VP_APPROVE)
            VALUES
             (:QLSP-BRNO,
              :QLSP-ACCTNO,
              :QLSP-SEQNO,
              NULLIF(:QLSP-LTOUCH-DATE,'1900-01-01'),
              :QLSP-REASON-CODE,
              :QLSP-PERIODS-TO-DEFER,
              :QLSP-REASON-FOR-DEFER,
              :QLSP-MORTGAGE-PAST-DUE,
              :QLSP-MORTGAGE-BALANCE,
              :QLSP-ATTEMPT-REFINANCE,
              NULLIF(:QLSP-INFO-DATE,'1900-01-01'),
              :QLSP-INFO-USERID,
              :QLSP-BR-MANAGER-COMMENTS,
              NULLIF(:QLSP-BR-MANAGER-DATE,'1900-01-01'),
              :QLSP-BR-MANAGER-USERID,
              :QLSP-AVP-COMMENTS,
              NULLIF(:QLSP-AVP-DATE,'1900-01-01'),
              :QLSP-AVP-USERID,
              :QLSP-AVP-APPROVE,
              :QLSP-VP-COMMENTS,
              NULLIF(:QLSP-VP-DATE,'1900-01-01'),
              :QLSP-VP-USERID,
              :QLSP-VP-APPROVE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LSP1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LSP1-FILE.

           MOVE LSP-PATH-OWNBR  TO LSP-BRNO.
           ACCEPT LSP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LSP-SELECT-PATH TO E-FILE.
           MOVE LSP1-KEY        TO E-KEYX.
           PERFORM SET-LSP-FIELDS.

           EXEC SQL
            UPDATE DBO.LSPFILE
             SET
              LSPFILE.LSP_BRNO = :QLSP-BRNO,
              LSPFILE.LSP_ACCTNO = :QLSP-ACCTNO,
              LSPFILE.LSP_SEQNO = :QLSP-SEQNO,
              LSPFILE.LSP_LTOUCH_DATE =
                 NULLIF(:QLSP-LTOUCH-DATE,'1900-01-01'),
              LSPFILE.LSP_REASON_CODE = :QLSP-REASON-CODE,
              LSPFILE.LSP_PERIODS_TO_DEFER =
                   :QLSP-PERIODS-TO-DEFER,
              LSPFILE.LSP_REASON_FOR_DEFER =
                   :QLSP-REASON-FOR-DEFER,
              LSPFILE.LSP_MORTGAGE_PAST_DUE =
                   :QLSP-MORTGAGE-PAST-DUE,
              LSPFILE.LSP_MORTGAGE_BALANCE =
                   :QLSP-MORTGAGE-BALANCE,
              LSPFILE.LSP_ATTEMPT_REFINANCE =
                   :QLSP-ATTEMPT-REFINANCE,
              LSPFILE.LSP_INFO_DATE =
                 NULLIF(:QLSP-INFO-DATE,'1900-01-01'),
              LSPFILE.LSP_INFO_USERID = :QLSP-INFO-USERID,
              LSPFILE.LSP_BR_MANAGER_COMMENTS =
                   :QLSP-BR-MANAGER-COMMENTS,
              LSPFILE.LSP_BR_MANAGER_DATE =
                 NULLIF(:QLSP-BR-MANAGER-DATE,'1900-01-01'),
              LSPFILE.LSP_BR_MANAGER_USERID =
                   :QLSP-BR-MANAGER-USERID,
              LSPFILE.LSP_AVP_COMMENTS = :QLSP-AVP-COMMENTS,
              LSPFILE.LSP_AVP_DATE =
                 NULLIF(:QLSP-AVP-DATE,'1900-01-01'),
              LSPFILE.LSP_AVP_USERID = :QLSP-AVP-USERID,
              LSPFILE.LSP_AVP_APPROVE = :QLSP-AVP-APPROVE,
              LSPFILE.LSP_VP_COMMENTS = :QLSP-VP-COMMENTS,
              LSPFILE.LSP_VP_DATE =
                 NULLIF(:QLSP-VP-DATE,'1900-01-01'),
              LSPFILE.LSP_VP_USERID = :QLSP-VP-USERID,
              LSPFILE.LSP_VP_APPROVE = :QLSP-VP-APPROVE
             WHERE LSPFILE.LSP_BRNO = :QLSP-BRNO
               AND LSPFILE.LSP_ACCTNO = :QLSP-ACCTNO
               AND LSPFILE.LSP_SEQNO = :QLSP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LSP1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LSP1-FILE.

           MOVE LSP-PATH-OWNBR  TO LSP-BRNO.
           PERFORM DELETE-IT.
           MOVE LSP-SELECT-PATH TO E-FILE.
           MOVE LSP1-KEY        TO E-KEYX.
           PERFORM SET-LSP-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LSPFILE
             WHERE LSPFILE.LSP_BRNO = :QLSP-BRNO
               AND LSPFILE.LSP_ACCTNO = :QLSP-ACCTNO
               AND LSPFILE.LSP_SEQNO = :QLSP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LSP1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LSP1-FILE.

           IF ( LSP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSP1-CURSOR-STAT.

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
              DESTROY IOLSPI-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
