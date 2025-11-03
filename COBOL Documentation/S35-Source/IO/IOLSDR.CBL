       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOLSDR.
       AUTHOR.        BAH
       DATE-WRITTEN.  04-07-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOLSDR.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : LSDFILE 
      * TABLE DESC    : LOAN TRAILER DEFICIENCY FILE
      * OUTPUT FILE   : XPC/IOLSDR.PRE
      * USED FOR      : LIBLP/LPWSLSD.CPY  --- QLSD-REC
      *                 LIBLP/LPLSD1R.CPY  --- READ
      *                 LIBLP/LPLSD1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0407 ***NEW***
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
           "@(#)IO/IOLSDR.CBL S34 07/21/21-12:19:13 >".

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
       COPY "LIBLP/LP01LSD.CPY".
       COPY "LIBLP/LPWSLSD.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QLSD1-WHERE-BEG-KEY.
           03  QLSD1-WBEG-BRNO             PIC 9(04).
           03  QLSD1-WBEG-ACCTNO           PIC 9(08).
           03  QLSD1-WBEG-SEQNO            PIC 9(02).

       01  QLSD1-WHERE-END-KEY.
           03  QLSD1-WEND-BRNO             PIC 9(04).
           03  QLSD1-WEND-ACCTNO           PIC 9(08).
           03  QLSD1-WEND-SEQNO            PIC 9(02).
 
       01  QLSD-REC.
           03  QLSD-BRNO               PIC 9(4).
           03  QLSD-ACCTNO             PIC 9(8).
           03  QLSD-SEQNO              PIC 99.
           03  QLSD-LTOUCH-DATE           PIC X(10).
           03  QLSD-STOLEN-DATE           PIC X(10).
           03  QLSD-GAP-INS-PAID          PIC S9(7)V99.
           03  QLSD-OTHER-EXPENSE         PIC S9(7)V99.
           03  QLSD-MISC-AMT-EXPENSE      PIC S9(7)V99.
           03  QLSD-MISC-DESC-EXPENSE     PIC X(15).
           03  QLSD-TOT-EXPENSE           PIC S9(7)V99.
           03  QLSD-INT-REB               PIC S9(7)V99.
           03  QLSD-CPP-REB               PIC S9(7)V99.
           03  QLSD-GAP-REB               PIC S9(7)V99.
           03  QLSD-WARRANTY-REB          PIC S9(7)V99.
           03  QLSD-MISC-AMT-CREDIT       PIC S9(7)V99.
           03  QLSD-MISC-DESC-CREDIT      PIC X(15).
           03  QLSD-TOT-CREDITS           PIC S9(7)V99.
           03  QLSD-PAID-TO-OTHER         PIC S9(7)V99.
           03  QLSD-AMT-FROM-OWING        PIC S9(7)V99.
           03  QLSD-AMT-OF-SURPLUS        PIC S9(7)V99.
           03  QLSD-AMT-OF-DEFICIENCY     PIC S9(7)V99.
           03  QLSD-SEND-FMNO             PIC 9(4).
           03  QLSD-INSURANCE-PROCEEDS    PIC S9(7)V99.
           03  QLSD-DATE-CHECK-RECV       PIC X(10).
           03  QLSD-INSURANCE-CHECK       PIC S9(7)V99.
           03  QLSD-WHO-ADJUSTED          PIC X(20).
           03  QLSD-INS-COMPANY           PIC X(20).
           03  QLSD-DEF-LETTER-SENT-DATE  PIC X(10).
           03  QLSD-SALES-DATE            PIC X(10).
           03  QLSD-AUCTION               PIC X(24).
           03  QLSD-AUCTN-TIME            PIC 9(4).
           03  QLSD-AUCTN-ADR             PIC X(24).
           03  QLSD-AUCTN-CITY            PIC X(15).
           03  QLSD-AUCTN-STATE           PIC X(2).
           03  QLSD-AUCTN-ZIP             PIC X(5).
           03  QLSD-AUCTN-PHONE           PIC 9(10).
           03  QLSD-REPO-CHARGE           PIC S9(5)V99.
           03  QLSD-STORAGE-CHARGE        PIC S9(5)V99.
           03  QLSD-REPAIR-CHARGE         PIC S9(5)V99.
           03  QLSD-OTHER-CHARGE          PIC S9(5)V99.
           03  QLSD-STORAGE-RATE          PIC S9(5)V99.
           03  QLSD-EST-REBATE            PIC S9(5)V99.
           03  QLSD-MINIMUM-BID           PIC S9(5)V99.
           03  QLSD-MAY-REINSTATE         PIC X.
           03  QLSD-MAY-NOT-REINSTATE     PIC X.
           03  QLSD-IN-PAST-12MONTHS      PIC X.
           03  QLSD-2X-DURING-CONTRACT    PIC X.
           03  QLSD-OTHER                 PIC X.
           03  QLSD-OTHER-COMMENTS1       PIC X(30).
           03  QLSD-OTHER-COMMENTS2       PIC X(30).
           03  QLSD-CA-PYMTS-AS-AGREED    PIC X.
           03  QLSD-CA-KEEP-INS           PIC X.
           03  QLSD-CA-KEEP-LIEN-FREE     PIC X.
           03  QLSD-CA-OTHER              PIC X.
           03  QLSD-CA-OTHER-DESC         PIC X(30).
           03  QLSD-CURE                  PIC X(33).
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
           03  FILLER PIC X(37) VALUE "BRNO     ACCTNO   SEQ   STOLEN".
           03  FILLER PIC X(28) VALUE "MISC AMT      REPO CHG      ".
           03  FILLER PIC X(30) VALUE "COMMENT".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-LSD-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-LSD-ACCTNO                  PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-LSD-SEQNO                   PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-LSD-STOLEN-DATE            PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-LSD-MISC-AMT-EXPENSE       PIC Z(6)9.99-.
           03  FILLER                         PIC X(03).
           03  D01-LSD-REPO-CHARGE            PIC Z(6)9.99-.
           03  FILLER                         PIC X(03).
           03  D01-LSD-OTHER-COMMENTS1       PIC X(20).

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
       COPY "LIBIO/IOLSDR_DEF.CPY".
       COPY "LIBIO/IOLSDR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOLSDR_SCN.CPY".

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
           MOVE "IOLSDR"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOLSDR-QUEUE-POS  TO Q-POS.
           MOVE IOLSDR-COPIES-POS TO C-POS.
           MOVE IOLSDR-PRU-POS    TO P-POS.
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

           MOVE "B"                        TO LSD-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }

           MOVE BR-NO     TO QLSD1-WBEG-BRNO.
           MOVE BR-NO     TO QLSD1-WEND-BRNO.

           MOVE BEG-ACCTNO TO QLSD1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QLSD1-WEND-ACCTNO.
           MOVE BEG-SEQNO  TO QLSD1-WBEG-SEQNO.
           MOVE END-SEQNO  TO QLSD1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-LSD1-FILE.
           PERFORM START-LSD1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-LSD1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LSD-BRNO NOT = LSD-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( LSD-ACCTNO < BEG-ACCTNO )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( LSD-ACCTNO > END-ACCTNO )
              GO TO PROCESS-BRANCH-MODULE-END.

           IF LSD-SEQNO < BEG-SEQNO OR > END-SEQNO
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE LSD-BRNO                    TO D01-LSD-BRNO.
           MOVE LSD-ACCTNO                  TO D01-LSD-ACCTNO.
           MOVE LSD-SEQNO                   TO D01-LSD-SEQNO.
           MOVE LSD-STOLEN-DATE       TO D01-LSD-STOLEN-DATE.
           MOVE LSD-MISC-AMT-EXPENSE  TO D01-LSD-MISC-AMT-EXPENSE.
           MOVE LSD-REPO-CHARGE       TO D01-LSD-REPO-CHARGE.
           MOVE LSD-OTHER-COMMENTS1   TO D01-LSD-OTHER-COMMENTS1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-LSD1-FILE.

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

           MOVE LSD1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "SQL TABLE IO UTILITY - LSDFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOLSDR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOLSDR-SYSDATE.
           DISPLAY IOLSDR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOLSDR-FORM-FIELDS TO WR-BUF.
           MOVE IOLSDR-FORM-LIST TO WR-LIST.
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

           MOVE IOLSDR-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOLSDR-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOLSDR_EVA.CPY".
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
      *    LOAD LSD-REC FROM QLSD-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LSD-FIELDS.
           MOVE QLSD-BRNO TO LSD-BRNO.
           MOVE QLSD-ACCTNO TO LSD-ACCTNO.
           MOVE QLSD-SEQNO TO LSD-SEQNO.

           IF ( QLSD-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LSD-LTOUCH-DATE
           ELSE
              MOVE QLSD-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSD-LTOUCH-DATE.


           IF ( QLSD-STOLEN-DATE = "1900-01-01" )
              MOVE ZEROES TO LSD-STOLEN-DATE
           ELSE
              MOVE QLSD-STOLEN-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSD-STOLEN-DATE.

           MOVE QLSD-GAP-INS-PAID TO LSD-GAP-INS-PAID.
           MOVE QLSD-OTHER-EXPENSE TO LSD-OTHER-EXPENSE.
           MOVE QLSD-MISC-AMT-EXPENSE TO LSD-MISC-AMT-EXPENSE.
           MOVE QLSD-MISC-DESC-EXPENSE TO LSD-MISC-DESC-EXPENSE.
           MOVE QLSD-TOT-EXPENSE TO LSD-TOT-EXPENSE.
           MOVE QLSD-INT-REB TO LSD-INT-REB.
           MOVE QLSD-CPP-REB TO LSD-CPP-REB.
           MOVE QLSD-GAP-REB TO LSD-GAP-REB.
           MOVE QLSD-WARRANTY-REB TO LSD-WARRANTY-REB.
           MOVE QLSD-MISC-AMT-CREDIT TO LSD-MISC-AMT-CREDIT.
           MOVE QLSD-MISC-DESC-CREDIT TO LSD-MISC-DESC-CREDIT.
           MOVE QLSD-TOT-CREDITS TO LSD-TOT-CREDITS.
           MOVE QLSD-PAID-TO-OTHER TO LSD-PAID-TO-OTHER.
           MOVE QLSD-AMT-FROM-OWING TO LSD-AMT-FROM-OWING.
           MOVE QLSD-AMT-OF-SURPLUS TO LSD-AMT-OF-SURPLUS.
           MOVE QLSD-AMT-OF-DEFICIENCY TO LSD-AMT-OF-DEFICIENCY.
           MOVE QLSD-SEND-FMNO TO LSD-SEND-FMNO.
           MOVE QLSD-INSURANCE-PROCEEDS TO LSD-INSURANCE-PROCEEDS.

           IF ( QLSD-DATE-CHECK-RECV = "1900-01-01" )
              MOVE ZEROES TO LSD-DATE-CHECK-RECV
           ELSE
              MOVE QLSD-DATE-CHECK-RECV TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSD-DATE-CHECK-RECV.

           MOVE QLSD-INSURANCE-CHECK TO LSD-INSURANCE-CHECK.
           MOVE QLSD-WHO-ADJUSTED TO LSD-WHO-ADJUSTED.
           MOVE QLSD-INS-COMPANY TO LSD-INS-COMPANY.

           IF ( QLSD-DEF-LETTER-SENT-DATE = "1900-01-01" )
              MOVE ZEROES TO LSD-DEF-LETTER-SENT-DATE
           ELSE
              MOVE QLSD-DEF-LETTER-SENT-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSD-DEF-LETTER-SENT-DATE.


           IF ( QLSD-SALES-DATE = "1900-01-01" )
              MOVE ZEROES TO LSD-SALES-DATE
           ELSE
              MOVE QLSD-SALES-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSD-SALES-DATE.

           MOVE QLSD-AUCTION TO LSD-AUCTION.
           MOVE QLSD-AUCTN-TIME TO LSD-AUCTN-TIME.
           MOVE QLSD-AUCTN-ADR TO LSD-AUCTN-ADR.
           MOVE QLSD-AUCTN-CITY TO LSD-AUCTN-CITY.
           MOVE QLSD-AUCTN-STATE TO LSD-AUCTN-STATE.
           MOVE QLSD-AUCTN-ZIP TO LSD-AUCTN-ZIP.
           MOVE QLSD-AUCTN-PHONE TO LSD-AUCTN-PHONE.
           MOVE QLSD-REPO-CHARGE TO LSD-REPO-CHARGE.
           MOVE QLSD-STORAGE-CHARGE TO LSD-STORAGE-CHARGE.
           MOVE QLSD-REPAIR-CHARGE TO LSD-REPAIR-CHARGE.
           MOVE QLSD-OTHER-CHARGE TO LSD-OTHER-CHARGE.
           MOVE QLSD-STORAGE-RATE TO LSD-STORAGE-RATE.
           MOVE QLSD-EST-REBATE TO LSD-EST-REBATE.
           MOVE QLSD-MINIMUM-BID TO LSD-MINIMUM-BID.
           MOVE QLSD-MAY-REINSTATE TO LSD-MAY-REINSTATE.
           MOVE QLSD-MAY-NOT-REINSTATE TO LSD-MAY-NOT-REINSTATE.
           MOVE QLSD-IN-PAST-12MONTHS TO LSD-IN-PAST-12MONTHS.
           MOVE QLSD-2X-DURING-CONTRACT TO LSD-2X-DURING-CONTRACT.
           MOVE QLSD-OTHER TO LSD-OTHER.
           MOVE QLSD-OTHER-COMMENTS1 TO LSD-OTHER-COMMENTS1.
           MOVE QLSD-OTHER-COMMENTS2 TO LSD-OTHER-COMMENTS2.
           MOVE QLSD-CA-PYMTS-AS-AGREED TO LSD-CA-PYMTS-AS-AGREED.
           MOVE QLSD-CA-KEEP-INS TO LSD-CA-KEEP-INS.
           MOVE QLSD-CA-KEEP-LIEN-FREE TO LSD-CA-KEEP-LIEN-FREE.
           MOVE QLSD-CA-OTHER TO LSD-CA-OTHER.
           MOVE QLSD-CA-OTHER-DESC TO LSD-CA-OTHER-DESC.
           MOVE QLSD-CURE TO LSD-CURE.
 
      *-----------------------------------------------------------------
      *    LOAD QLSD-REC FROM LSD-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LSD-FIELDS.
           MOVE LSD-BRNO TO QLSD-BRNO.
           MOVE LSD-ACCTNO TO QLSD-ACCTNO.
           MOVE LSD-SEQNO TO QLSD-SEQNO.

           IF ( LSD-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSD-LTOUCH-DATE
           ELSE
              MOVE LSD-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSD-LTOUCH-DATE.


           IF ( LSD-STOLEN-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSD-STOLEN-DATE
           ELSE
              MOVE LSD-STOLEN-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSD-STOLEN-DATE.

           MOVE LSD-GAP-INS-PAID TO QLSD-GAP-INS-PAID.
           MOVE LSD-OTHER-EXPENSE TO QLSD-OTHER-EXPENSE.
           MOVE LSD-MISC-AMT-EXPENSE TO QLSD-MISC-AMT-EXPENSE.
           MOVE LSD-MISC-DESC-EXPENSE TO QLSD-MISC-DESC-EXPENSE.
           MOVE LSD-TOT-EXPENSE TO QLSD-TOT-EXPENSE.
           MOVE LSD-INT-REB TO QLSD-INT-REB.
           MOVE LSD-CPP-REB TO QLSD-CPP-REB.
           MOVE LSD-GAP-REB TO QLSD-GAP-REB.
           MOVE LSD-WARRANTY-REB TO QLSD-WARRANTY-REB.
           MOVE LSD-MISC-AMT-CREDIT TO QLSD-MISC-AMT-CREDIT.
           MOVE LSD-MISC-DESC-CREDIT TO QLSD-MISC-DESC-CREDIT.
           MOVE LSD-TOT-CREDITS TO QLSD-TOT-CREDITS.
           MOVE LSD-PAID-TO-OTHER TO QLSD-PAID-TO-OTHER.
           MOVE LSD-AMT-FROM-OWING TO QLSD-AMT-FROM-OWING.
           MOVE LSD-AMT-OF-SURPLUS TO QLSD-AMT-OF-SURPLUS.
           MOVE LSD-AMT-OF-DEFICIENCY TO QLSD-AMT-OF-DEFICIENCY.
           MOVE LSD-SEND-FMNO TO QLSD-SEND-FMNO.
           MOVE LSD-INSURANCE-PROCEEDS TO QLSD-INSURANCE-PROCEEDS.

           IF ( LSD-DATE-CHECK-RECV = ZEROES )
              MOVE "1900-01-01" TO QLSD-DATE-CHECK-RECV
           ELSE
              MOVE LSD-DATE-CHECK-RECV TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSD-DATE-CHECK-RECV.

           MOVE LSD-INSURANCE-CHECK TO QLSD-INSURANCE-CHECK.
           MOVE LSD-WHO-ADJUSTED TO QLSD-WHO-ADJUSTED.
           MOVE LSD-INS-COMPANY TO QLSD-INS-COMPANY.

           IF ( LSD-DEF-LETTER-SENT-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSD-DEF-LETTER-SENT-DATE
           ELSE
              MOVE LSD-DEF-LETTER-SENT-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSD-DEF-LETTER-SENT-DATE.


           IF ( LSD-SALES-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSD-SALES-DATE
           ELSE
              MOVE LSD-SALES-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSD-SALES-DATE.

           MOVE LSD-AUCTION TO QLSD-AUCTION.
           MOVE LSD-AUCTN-TIME TO QLSD-AUCTN-TIME.
           MOVE LSD-AUCTN-ADR TO QLSD-AUCTN-ADR.
           MOVE LSD-AUCTN-CITY TO QLSD-AUCTN-CITY.
           MOVE LSD-AUCTN-STATE TO QLSD-AUCTN-STATE.
           MOVE LSD-AUCTN-ZIP TO QLSD-AUCTN-ZIP.
           MOVE LSD-AUCTN-PHONE TO QLSD-AUCTN-PHONE.
           MOVE LSD-REPO-CHARGE TO QLSD-REPO-CHARGE.
           MOVE LSD-STORAGE-CHARGE TO QLSD-STORAGE-CHARGE.
           MOVE LSD-REPAIR-CHARGE TO QLSD-REPAIR-CHARGE.
           MOVE LSD-OTHER-CHARGE TO QLSD-OTHER-CHARGE.
           MOVE LSD-STORAGE-RATE TO QLSD-STORAGE-RATE.
           MOVE LSD-EST-REBATE TO QLSD-EST-REBATE.
           MOVE LSD-MINIMUM-BID TO QLSD-MINIMUM-BID.
           MOVE LSD-MAY-REINSTATE TO QLSD-MAY-REINSTATE.
           MOVE LSD-MAY-NOT-REINSTATE TO QLSD-MAY-NOT-REINSTATE.
           MOVE LSD-IN-PAST-12MONTHS TO QLSD-IN-PAST-12MONTHS.
           MOVE LSD-2X-DURING-CONTRACT TO QLSD-2X-DURING-CONTRACT.
           MOVE LSD-OTHER TO QLSD-OTHER.
           MOVE LSD-OTHER-COMMENTS1 TO QLSD-OTHER-COMMENTS1.
           MOVE LSD-OTHER-COMMENTS2 TO QLSD-OTHER-COMMENTS2.
           MOVE LSD-CA-PYMTS-AS-AGREED TO QLSD-CA-PYMTS-AS-AGREED.
           MOVE LSD-CA-KEEP-INS TO QLSD-CA-KEEP-INS.
           MOVE LSD-CA-KEEP-LIEN-FREE TO QLSD-CA-KEEP-LIEN-FREE.
           MOVE LSD-CA-OTHER TO QLSD-CA-OTHER.
           MOVE LSD-CA-OTHER-DESC TO QLSD-CA-OTHER-DESC.
           MOVE LSD-CURE TO QLSD-CURE.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LSD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LSD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LSD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LSD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LSD-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LSD-ALL-DATA
              MOVE LSD-ALL-PATH             TO LSD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LSD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LSD-ALL-BASE
              MOVE FILPATH-MACHINE         TO LSD-ALL-MACHINE
              MOVE FILPATH-DATA            TO LSD-ALL-DATA
              MOVE LSD-ALL-PATH             TO LSD-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LSDFILE NOT = "S" )
              MOVE LSD-PATH-BEG             TO LSD-SELECT-PATH-BEG
              MOVE LSD-PATH-FILE            TO LSD-SELECT-PATH-FILE
           ELSE
              MOVE LSD-PATH-SQL             TO LSD-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LSD1-FILE.
           PERFORM LOAD-LSD1-FILE.
           PERFORM OPEN-IT.
           MOVE LSD-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LSD1-FILE.
           PERFORM START-IT.
           MOVE LSD-PATH-OWNBR  TO LSD-BRNO.
           MOVE LSD-SELECT-PATH TO E-FILE.
           MOVE LSD1-KEY        TO E-KEYX.

           IF ( LSD1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LSD1-FILE.

           IF ( QLSD1-WEND-BRNO = ZEROES    ) OR
              ( QLSD1-WEND-BRNO NOT NUMERIC )
              MOVE LSD-PATH-OWNBR        TO QLSD1-WBEG-BRNO
              MOVE LSD-PATH-OWNBR        TO QLSD1-WEND-BRNO
              MOVE LSD-ACCTNO            TO QLSD1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLSD1-WEND-ACCTNO
              MOVE LSD-SEQNO             TO QLSD1-WBEG-SEQNO
              MOVE ALL "9"               TO QLSD1-WEND-SEQNO.

           EXEC SQL
            DECLARE LSD1_CURSOR CURSOR FOR
            SELECT LSDFILE.LSD_BRNO,
                   LSDFILE.LSD_ACCTNO,
                   LSDFILE.LSD_SEQNO,
                   ISNULL(CAST(LSDFILE.LSD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSDFILE.LSD_STOLEN_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSDFILE.LSD_GAP_INS_PAID,
                   LSDFILE.LSD_OTHER_EXPENSE,
                   LSDFILE.LSD_MISC_AMT_EXPENSE,
                   ISNULL(LSDFILE.LSD_MISC_DESC_EXPENSE,''),
                   LSDFILE.LSD_TOT_EXPENSE,
                   LSDFILE.LSD_INT_REB,
                   LSDFILE.LSD_CPP_REB,
                   LSDFILE.LSD_GAP_REB,
                   LSDFILE.LSD_WARRANTY_REB,
                   LSDFILE.LSD_MISC_AMT_CREDIT,
                   ISNULL(LSDFILE.LSD_MISC_DESC_CREDIT,''),
                   LSDFILE.LSD_TOT_CREDITS,
                   LSDFILE.LSD_PAID_TO_OTHER,
                   LSDFILE.LSD_AMT_FROM_OWING,
                   LSDFILE.LSD_AMT_OF_SURPLUS,
                   LSDFILE.LSD_AMT_OF_DEFICIENCY,
                   LSDFILE.LSD_SEND_FMNO,
                   LSDFILE.LSD_INSURANCE_PROCEEDS,
                   ISNULL(CAST(LSDFILE.LSD_DATE_CHECK_RECV
                                                  AS VARCHAR(10)),'0'),
                   LSDFILE.LSD_INSURANCE_CHECK,
                   ISNULL(LSDFILE.LSD_WHO_ADJUSTED,''),
                   ISNULL(LSDFILE.LSD_INS_COMPANY,''),
                   ISNULL(CAST(LSDFILE.LSD_DEF_LETTER_SENT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSDFILE.LSD_SALES_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSDFILE.LSD_AUCTION,''),
                   LSDFILE.LSD_AUCTN_TIME,
                   ISNULL(LSDFILE.LSD_AUCTN_ADR,''),
                   ISNULL(LSDFILE.LSD_AUCTN_CITY,''),
                   ISNULL(LSDFILE.LSD_AUCTN_STATE,''),
                   ISNULL(LSDFILE.LSD_AUCTN_ZIP,''),
                   LSDFILE.LSD_AUCTN_PHONE,
                   LSDFILE.LSD_REPO_CHARGE,
                   LSDFILE.LSD_STORAGE_CHARGE,
                   LSDFILE.LSD_REPAIR_CHARGE,
                   LSDFILE.LSD_OTHER_CHARGE,
                   LSDFILE.LSD_STORAGE_RATE,
                   LSDFILE.LSD_EST_REBATE,
                   LSDFILE.LSD_MINIMUM_BID,
                   ISNULL(LSDFILE.LSD_MAY_REINSTATE,''),
                   ISNULL(LSDFILE.LSD_MAY_NOT_REINSTATE,''),
                   ISNULL(LSDFILE.LSD_IN_PAST_12MONTHS,''),
                   ISNULL(LSDFILE.LSD_2X_DURING_CONTRACT,''),
                   ISNULL(LSDFILE.LSD_OTHER,''),
                   ISNULL(LSDFILE.LSD_OTHER_COMMENTS1,''),
                   ISNULL(LSDFILE.LSD_OTHER_COMMENTS2,''),
                   ISNULL(LSDFILE.LSD_CA_PYMTS_AS_AGREED,''),
                   ISNULL(LSDFILE.LSD_CA_KEEP_INS,''),
                   ISNULL(LSDFILE.LSD_CA_KEEP_LIEN_FREE,''),
                   ISNULL(LSDFILE.LSD_CA_OTHER,''),
                   ISNULL(LSDFILE.LSD_CA_OTHER_DESC,''),
                   ISNULL(LSDFILE.LSD_CURE,'')
            FROM DBO.LSDFILE
            WHERE LSDFILE.LSD_BRNO >= :QLSD1-WBEG-BRNO
              AND LSDFILE.LSD_BRNO <= :QLSD1-WEND-BRNO
              AND LSDFILE.LSD_ACCTNO >= :QLSD1-WBEG-ACCTNO
              AND LSDFILE.LSD_ACCTNO <= :QLSD1-WEND-ACCTNO
              AND LSDFILE.LSD_SEQNO >= :QLSD1-WBEG-SEQNO
              AND LSDFILE.LSD_SEQNO <= :QLSD1-WEND-SEQNO
            ORDER BY LSDFILE.LSD_BRNO,
                     LSDFILE.LSD_ACCTNO,
                     LSDFILE.LSD_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LSD1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLSD1-WBEG-BRNO
                          QLSD1-WEND-BRNO
                          QLSD1-WBEG-ACCTNO
                          QLSD1-WEND-ACCTNO
                          QLSD1-WBEG-SEQNO
                          QLSD1-WEND-SEQNO.

           MOVE STAT---GOOD TO LSD1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LSD1-FILE.
           PERFORM READ-IT.
           MOVE LSD-PATH-OWNBR  TO LSD-BRNO. 
           MOVE LSD-SELECT-PATH TO E-FILE.
           MOVE LSD1-KEY        TO E-KEYX.

           MOVE LSD-BRNO   TO QLSD-BRNO.
           MOVE LSD-ACCTNO TO QLSD-ACCTNO.
           MOVE LSD-SEQNO  TO QLSD-SEQNO.

           EXEC SQL
            SELECT LSDFILE.LSD_BRNO,
                   LSDFILE.LSD_ACCTNO,
                   LSDFILE.LSD_SEQNO,
                   ISNULL(CAST(LSDFILE.LSD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSDFILE.LSD_STOLEN_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSDFILE.LSD_GAP_INS_PAID,
                   LSDFILE.LSD_OTHER_EXPENSE,
                   LSDFILE.LSD_MISC_AMT_EXPENSE,
                   ISNULL(LSDFILE.LSD_MISC_DESC_EXPENSE,''),
                   LSDFILE.LSD_TOT_EXPENSE,
                   LSDFILE.LSD_INT_REB,
                   LSDFILE.LSD_CPP_REB,
                   LSDFILE.LSD_GAP_REB,
                   LSDFILE.LSD_WARRANTY_REB,
                   LSDFILE.LSD_MISC_AMT_CREDIT,
                   ISNULL(LSDFILE.LSD_MISC_DESC_CREDIT,''),
                   LSDFILE.LSD_TOT_CREDITS,
                   LSDFILE.LSD_PAID_TO_OTHER,
                   LSDFILE.LSD_AMT_FROM_OWING,
                   LSDFILE.LSD_AMT_OF_SURPLUS,
                   LSDFILE.LSD_AMT_OF_DEFICIENCY,
                   LSDFILE.LSD_SEND_FMNO,
                   LSDFILE.LSD_INSURANCE_PROCEEDS,
                   ISNULL(CAST(LSDFILE.LSD_DATE_CHECK_RECV
                                                  AS VARCHAR(10)),'0'),
                   LSDFILE.LSD_INSURANCE_CHECK,
                   ISNULL(LSDFILE.LSD_WHO_ADJUSTED,''),
                   ISNULL(LSDFILE.LSD_INS_COMPANY,''),
                   ISNULL(CAST(LSDFILE.LSD_DEF_LETTER_SENT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSDFILE.LSD_SALES_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSDFILE.LSD_AUCTION,''),
                   LSDFILE.LSD_AUCTN_TIME,
                   ISNULL(LSDFILE.LSD_AUCTN_ADR,''),
                   ISNULL(LSDFILE.LSD_AUCTN_CITY,''),
                   ISNULL(LSDFILE.LSD_AUCTN_STATE,''),
                   ISNULL(LSDFILE.LSD_AUCTN_ZIP,''),
                   LSDFILE.LSD_AUCTN_PHONE,
                   LSDFILE.LSD_REPO_CHARGE,
                   LSDFILE.LSD_STORAGE_CHARGE,
                   LSDFILE.LSD_REPAIR_CHARGE,
                   LSDFILE.LSD_OTHER_CHARGE,
                   LSDFILE.LSD_STORAGE_RATE,
                   LSDFILE.LSD_EST_REBATE,
                   LSDFILE.LSD_MINIMUM_BID,
                   ISNULL(LSDFILE.LSD_MAY_REINSTATE,''),
                   ISNULL(LSDFILE.LSD_MAY_NOT_REINSTATE,''),
                   ISNULL(LSDFILE.LSD_IN_PAST_12MONTHS,''),
                   ISNULL(LSDFILE.LSD_2X_DURING_CONTRACT,''),
                   ISNULL(LSDFILE.LSD_OTHER,''),
                   ISNULL(LSDFILE.LSD_OTHER_COMMENTS1,''),
                   ISNULL(LSDFILE.LSD_OTHER_COMMENTS2,''),
                   ISNULL(LSDFILE.LSD_CA_PYMTS_AS_AGREED,''),
                   ISNULL(LSDFILE.LSD_CA_KEEP_INS,''),
                   ISNULL(LSDFILE.LSD_CA_KEEP_LIEN_FREE,''),
                   ISNULL(LSDFILE.LSD_CA_OTHER,''),
                   ISNULL(LSDFILE.LSD_CA_OTHER_DESC,''),
                   ISNULL(LSDFILE.LSD_CURE,'')
            INTO :QLSD-REC
            FROM DBO.LSDFILE
            WHERE LSDFILE.LSD_BRNO = :QLSD-BRNO
              AND LSDFILE.LSD_ACCTNO = :QLSD-ACCTNO
              AND LSDFILE.LSD_SEQNO = :QLSD-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSD1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LSD-FIELDS.

      *-----------------------------------------------------------------
       READ-LSD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LSD-PATH-OWNBR  TO LSD-BRNO.
           MOVE LSD-SELECT-PATH TO E-FILE.
           MOVE LSD1-KEY        TO E-KEYX.
           INITIALIZE QLSD-REC.

           IF ( LSD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LSD1_CURSOR INTO :QLSD-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSD1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LSD-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LSD1-FILE.

           IF ( LSD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSD1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSD1-CURSOR-STAT.

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
              DESTROY IOLSDR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
