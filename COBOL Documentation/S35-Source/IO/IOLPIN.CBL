       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOLPIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-12-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOLPIN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : LPFILE 
      * TABLE DESC    : LOAN PAYMENT FILE
      * OUTPUT FILE   : XPC/IOLPIN.PRE
      * USED FOR      : LIBLP/LPWSLP.CPY  --- QLP-REC
      *                 LIBLP/LPLP1R.CPY  --- READ
      *                 LIBLP/LPLP1RN.CPY --- READ/READ-NEXT/START
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
           "@(#)IO/IOLPIN.CBL S34 05/20/21-09:28:57 >".

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
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LPWSLP.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QLP1-WHERE-BEG-KEY.
           03  QLP1-WBEG-BRNO             PIC 9(04).
           03  QLP1-WBEG-ACCTNO           PIC 9(08).
           03  QLP1-WBEG-SEQNO            PIC 9(05).
 
       01  QLP1-WHERE-END-KEY.
           03  QLP1-WEND-BRNO             PIC 9(04).
           03  QLP1-WEND-ACCTNO           PIC 9(08).
           03  QLP1-WEND-SEQNO            PIC 9(05).
 
       01  QLP-REC.
           03  QLP-BRNO             PIC  9(4).
           03  QLP-ACCTNO           PIC  9(8).
           03  QLP-SEQNO            PIC  9(5).
           03  QLP-LTOUCH-DATE          PIC X(10).
           03  QLP-USERID               PIC X(10).
           03  QLP-POSTDATE             PIC X(10).
           03  QLP-POSTTIME             PIC 9(6).
           03  QLP-TRDATE               PIC X(10).
           03  QLP-PAYDATE              PIC X(10).
           03  QLP-TRCD                 PIC XX.
           03  QLP-REFNO                PIC X(5).
           03  QLP-TRAMT                PIC S9(7)V99.
           03  QLP-APOTH                PIC S9(7)V99.
           03  QLP-APOT2                PIC S9(7)V99.
           03  QLP-APINT                PIC S9(7)V99.
           03  QLP-APLC                 PIC S9(7)V99.
           03  QLP-APCUR                PIC S9(7)V99.
           03  QLP-OTHBAL               PIC S9(7)V99.
           03  QLP-OT2BAL               PIC S9(7)V99.
           03  QLP-INTBAL               PIC S9(7)V99.
           03  QLP-LCBAL                PIC S9(7)V99.
           03  QLP-CURBAL               PIC S9(7)V99.
           03  QLP-INTDUE               PIC S9(7)V99.
           03  QLP-DLPROC               PIC S9(7)V99.
           03  QLP-EARNED-1             PIC S9(7)V99.
           03  QLP-EARNED-2             PIC S9(7)V99.
           03  QLP-EARNED-3             PIC S9(7)V99.
           03  QLP-INTPAID              PIC S9(7)V99.
           03  QLP-SSNO                 PIC 9(10).
           03  QLP-PDTH-DATE            PIC X(10).
           03  QLP-LCPDTH-DATE          PIC X(10).
           03  QLP-INTPDTH-DATE         PIC X(10).
           03  QLP-TILLNO               PIC 99.
           03  QLP-APINTOWE             PIC S9(7)V99.
           03  QLP-DLPARVHELD-ADJ       PIC S9(7)V99.
           03  QLP-LT-SEQNO             PIC 9(3).
           03  QLP-IBPC                 PIC X.
           03  QLP-MANUAL-RB            PIC X.
           03  QLP-INSTYPES             PIC X.
           03  QLP-REV                  PIC X.
           03  QLP-REVTRCD              PIC XX.
           03  QLP-REVSEQNO             PIC 9(5).
           03  QLP-STATUSFG             PIC X.
           03  QLP-NEWSTAT-RATE         PIC 999V999.
           03  QLP-LCPARTIALS           PIC S9(7)V99.
           03  QLP-ADDON-ORIG-REGPY     PIC S9(7)V99.
           03  QLP-NEW1ST-PYDATE        PIC X(10).
           03  QLP-DEF-DUEDATE          PIC X(10).
           03  QLP-LCPAID               PIC S9(7)V99.
           03  QLP-OLD1ST-PYDATE        PIC X(10).
           03  QLP-DEF-TERM             PIC 9(3).
           03  QLP-ADDON-LAST-PYAMT     PIC S9(7)V99.
           03  QLP-DLRVCHGS-ADJ-1       PIC S9(7)V99.
           03  QLP-DLRVCHGS-ADJ-2       PIC S9(7)V99.
           03  QLP-DLRVCHGS-ADJ-3       PIC S9(7)V99.
           03  QLP-DLPARVPAID-ADJ       PIC S9(7)V99.
           03  QLP-DLNO                  PIC 9(4)V99.
           03  QLP-PREPAY-PENALTY        PIC S9(7)V99.
           03  QLP-PRORATED-INS-REB      PIC S9(7)V99.
           03  QLP-POSTING-BR            PIC 9(4).
           03  QLP-NOLCHG                PIC S9(3).
           03  QLP-COLLECTOR             PIC X(3).
           03  QLP-EXTRA-PRIN-LFP        PIC S9(7)V99.
           03  QLP-REMAIN-INT-SINCE-LFP  PIC S9(7)V99.
           03  QLP-REMAIN-PRIN-SINCE-LFP PIC S9(7)V99.
           03  QLP-APPLIED-TO-ESCROW     PIC S9(7)V99.
           03  QLP-ESCROW-PREPAID        PIC S9(7)V99.
           03  QLP-PRIOR-POOLID          PIC 9(5).
           03  QLP-ADDON-INTPDTH-DATE    PIC X(10).
           03  QLP-FROMCL                PIC  999.
           03  QLP-TOCL                  PIC  999.
           03  QLP-ALT-PREPAY-FG         PIC X.

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
           03  BEG-SEQNO               PIC 9(05).
           03  END-SEQNO               PIC 9(05).

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
           03  FILLER PIC X(32) VALUE "BRNO  ACCTNO    SEQ   TRDATE".
           03  FILLER PIC X(30) VALUE "TRCD  REFNO       TRAMT".
           03  FILLER PIC X(30) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-LP-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(01).
           03  D01-LP-ACCTNO                  PIC 9(08).
           03  FILLER                         PIC X(01).
           03  D01-LP-SEQNO                   PIC 9(05).
           03  FILLER                         PIC X(01).
           03  D01-LP-TRDATE                  PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-LP-TRCD                    PIC XX.
           03  FILLER                         PIC X(03).
           03  D01-LP-REFNO                   PIC X(5).
           03  FILLER                         PIC X(02).
           03  D01-LP-TRAMT                   PIC Z(6)9.99-.

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
       COPY "LIBIO/IOLPIN_DEF.CPY".
       COPY "LIBIO/IOLPIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOLPIN_SCN.CPY".

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
           MOVE "IOLPIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOLPIN-QUEUE-POS  TO Q-POS.
           MOVE IOLPIN-COPIES-POS TO C-POS.
           MOVE IOLPIN-PRU-POS    TO P-POS.
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
              ( VDUNUM0 NOT =  1 AND NOT = 9)
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

      *================================================================
       ENTER-BEG-SEQNO.

           MOVE "ENNN050BEG-SEQNO." TO VDU.
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

           MOVE "ENNN050END-SEQNO." TO VDU.
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

           GO TO ENTER-BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-ALL-SEQNO.

           MOVE "SNNN050BEG-SEQNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-SEQNO.
           PERFORM SCREENIO.

           MOVE "SNNN050END-SEQNO." TO VDU.
           MOVE 99999               TO VDUNUM0  END-SEQNO.
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

           MOVE "SNNN050BEG-SEQNO."       TO VDU.
           MOVE BEG-SEQNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN050END-SEQNO."       TO VDU.
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

           MOVE "B"                        TO LP-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QLP1-WBEG-BRNO 
                              QLP1-WEND-BRNO.
           MOVE BEG-ACCTNO TO QLP1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QLP1-WEND-ACCTNO.
           MOVE BEG-SEQNO  TO QLP1-WBEG-SEQNO.
           MOVE END-SEQNO  TO QLP1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-LP1-FILE.
           IF PROCESS-KEY = 9
              PERFORM START-LP1-FILE-NOT-GREATER
           ELSE
              PERFORM START-LP1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           IF PROCESS-KEY = 9
              PERFORM READ-LP1-FILE-PREVIOUS
           ELSE
              PERFORM READ-LP1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LP-BRNO NOT = LP-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      * READ PREVIOUS
           IF PROCESS-KEY = 9
              IF LP-ACCTNO < BEG-ACCTNO
                 GO TO PROCESS-BRANCH-MODULE-END.

           IF ( LP-ACCTNO < BEG-ACCTNO )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( LP-ACCTNO > END-ACCTNO )
              GO TO PROCESS-BRANCH-MODULE-END.

           IF LP-SEQNO < BEG-SEQNO OR > END-SEQNO
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

      *-------------------------------------------------------{ RANGES }
           MOVE LP-BRNO                    TO D01-LP-BRNO.
           MOVE LP-ACCTNO                  TO D01-LP-ACCTNO.
           MOVE LP-SEQNO                   TO D01-LP-SEQNO.
           MOVE LP-TRDATE                  TO D01-LP-TRDATE.
           MOVE LP-TRCD                    TO D01-LP-TRCD.
           MOVE LP-REFNO                   TO D01-LP-REFNO.
           MOVE LP-TRAMT                   TO D01-LP-TRAMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-LP1-FILE.
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

           MOVE LP1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "SQL TABLE IO UTILITY - LPFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOLPIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOLPIN-SYSDATE.
           DISPLAY IOLPIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOLPIN-FORM-FIELDS TO WR-BUF.
           MOVE IOLPIN-FORM-LIST TO WR-LIST.
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

           MOVE IOLPIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOLPIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOLPIN_EVA.CPY".
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
      *    LOAD LP-REC FROM QLP-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LP-FIELDS.
           MOVE QLP-BRNO TO LP-BRNO.
           MOVE QLP-ACCTNO TO LP-ACCTNO.
           MOVE QLP-SEQNO TO LP-SEQNO.

           IF ( QLP-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LP-LTOUCH-DATE
           ELSE
              MOVE QLP-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-LTOUCH-DATE.

           MOVE QLP-USERID TO LP-USERID.

           IF ( QLP-POSTDATE = "1900-01-01" )
              MOVE ZEROES TO LP-POSTDATE
           ELSE
              MOVE QLP-POSTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-POSTDATE.

           MOVE QLP-POSTTIME TO LP-POSTTIME.

           IF ( QLP-TRDATE = "1900-01-01" )
              MOVE ZEROES TO LP-TRDATE
           ELSE
              MOVE QLP-TRDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-TRDATE.


           IF ( QLP-PAYDATE = "1900-01-01" )
              MOVE ZEROES TO LP-PAYDATE
           ELSE
              MOVE QLP-PAYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-PAYDATE.

           MOVE QLP-TRCD TO LP-TRCD.
           MOVE QLP-REFNO TO LP-REFNO.
           MOVE QLP-TRAMT TO LP-TRAMT.
           MOVE QLP-APOTH TO LP-APOTH.
           MOVE QLP-APOT2 TO LP-APOT2.
           MOVE QLP-APINT TO LP-APINT.
           MOVE QLP-APLC TO LP-APLC.
           MOVE QLP-APCUR TO LP-APCUR.
           MOVE QLP-OTHBAL TO LP-OTHBAL.
           MOVE QLP-OT2BAL TO LP-OT2BAL.
           MOVE QLP-INTBAL TO LP-INTBAL.
           MOVE QLP-LCBAL TO LP-LCBAL.
           MOVE QLP-CURBAL TO LP-CURBAL.
           MOVE QLP-INTDUE TO LP-INTDUE.
           MOVE QLP-DLPROC TO LP-DLPROC.
           MOVE QLP-EARNED-1 TO LP-EARNED(1).
           MOVE QLP-EARNED-2 TO LP-EARNED(2).
           MOVE QLP-EARNED-3 TO LP-EARNED(3).
           MOVE QLP-INTPAID TO LP-INTPAID.
           MOVE QLP-SSNO TO LP-SSNO.

           IF ( QLP-PDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO LP-PDTH-DATE
           ELSE
              MOVE QLP-PDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-PDTH-DATE.


           IF ( QLP-LCPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO LP-LCPDTH-DATE
           ELSE
              MOVE QLP-LCPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-LCPDTH-DATE.


           IF ( QLP-INTPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO LP-INTPDTH-DATE
           ELSE
              MOVE QLP-INTPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-INTPDTH-DATE.

           MOVE QLP-TILLNO TO LP-TILLNO.
           MOVE QLP-APINTOWE TO LP-APINTOWE.
           MOVE QLP-DLPARVHELD-ADJ TO LP-DLPARVHELD-ADJ.
           MOVE QLP-LT-SEQNO TO LP-LT-SEQNO.
           MOVE QLP-IBPC TO LP-IBPC.
           MOVE QLP-MANUAL-RB TO LP-MANUAL-RB.
           MOVE QLP-INSTYPES TO LP-INSTYPES.
           MOVE QLP-REV TO LP-REV.
           MOVE QLP-REVTRCD TO LP-REVTRCD.
           MOVE QLP-REVSEQNO TO LP-REVSEQNO.
           MOVE QLP-STATUSFG TO LP-STATUSFG.
           MOVE QLP-NEWSTAT-RATE TO LP-NEWSTAT-RATE.
           MOVE QLP-LCPARTIALS TO LP-LCPARTIALS.
           MOVE QLP-ADDON-ORIG-REGPY TO LP-ADDON-ORIG-REGPY.

           IF ( QLP-NEW1ST-PYDATE = "1900-01-01" )
              MOVE ZEROES TO LP-NEW1ST-PYDATE
           ELSE
              MOVE QLP-NEW1ST-PYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-NEW1ST-PYDATE.


           IF ( QLP-DEF-DUEDATE = "1900-01-01" )
              MOVE ZEROES TO LP-DEF-DUEDATE
           ELSE
              MOVE QLP-DEF-DUEDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-DEF-DUEDATE.

           MOVE QLP-LCPAID TO LP-LCPAID.

           IF ( QLP-OLD1ST-PYDATE = "1900-01-01" )
              MOVE ZEROES TO LP-OLD1ST-PYDATE
           ELSE
              MOVE QLP-OLD1ST-PYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-OLD1ST-PYDATE.

           MOVE QLP-DEF-TERM TO LP-DEF-TERM.
           MOVE QLP-ADDON-LAST-PYAMT TO LP-ADDON-LAST-PYAMT.
           MOVE QLP-DLRVCHGS-ADJ-1 TO LP-DLRVCHGS-ADJ(1).
           MOVE QLP-DLRVCHGS-ADJ-2 TO LP-DLRVCHGS-ADJ(2).
           MOVE QLP-DLRVCHGS-ADJ-3 TO LP-DLRVCHGS-ADJ(3).
           MOVE QLP-DLPARVPAID-ADJ TO LP-DLPARVPAID-ADJ.
           MOVE QLP-DLNO TO LP-DLNO.
           MOVE QLP-PREPAY-PENALTY TO LP-PREPAY-PENALTY.
           MOVE QLP-PRORATED-INS-REB TO LP-PRORATED-INS-REB.
           MOVE QLP-POSTING-BR TO LP-POSTING-BR.
           MOVE QLP-NOLCHG TO LP-NOLCHG.
           MOVE QLP-COLLECTOR TO LP-COLLECTOR.
           MOVE QLP-EXTRA-PRIN-LFP TO LP-EXTRA-PRIN-LFP.
           MOVE QLP-REMAIN-INT-SINCE-LFP TO LP-REMAIN-INT-SINCE-LFP.
           MOVE QLP-REMAIN-PRIN-SINCE-LFP TO LP-REMAIN-PRIN-SINCE-LFP.
           MOVE QLP-APPLIED-TO-ESCROW TO LP-APPLIED-TO-ESCROW.
           MOVE QLP-ESCROW-PREPAID TO LP-ESCROW-PREPAID.
           MOVE QLP-PRIOR-POOLID TO LP-PRIOR-POOLID.

           IF ( QLP-ADDON-INTPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO LP-ADDON-INTPDTH-DATE
           ELSE
              MOVE QLP-ADDON-INTPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LP-ADDON-INTPDTH-DATE.

           MOVE QLP-FROMCL TO LP-FROMCL.
           MOVE QLP-TOCL TO LP-TOCL.
           MOVE QLP-ALT-PREPAY-FG TO LP-ALT-PREPAY-FG.
 
      *-----------------------------------------------------------------
      *    LOAD QLP-REC FROM LP-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LP-FIELDS.
           MOVE LP-BRNO TO QLP-BRNO.
           MOVE LP-ACCTNO TO QLP-ACCTNO.
           MOVE LP-SEQNO TO QLP-SEQNO.

           IF ( LP-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLP-LTOUCH-DATE
           ELSE
              MOVE LP-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLP-LTOUCH-DATE.

           MOVE LP-USERID TO QLP-USERID.

           IF ( LP-POSTDATE = ZEROES )
              MOVE "1900-01-01" TO QLP-POSTDATE
           ELSE
              MOVE LP-POSTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLP-POSTDATE.

           MOVE LP-POSTTIME TO QLP-POSTTIME.

           IF ( LP-TRDATE = ZEROES )
              MOVE "1900-01-01" TO QLP-TRDATE
           ELSE
              MOVE LP-TRDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLP-TRDATE.


           IF ( LP-PAYDATE = ZEROES )
              MOVE "1900-01-01" TO QLP-PAYDATE
           ELSE
              MOVE LP-PAYDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLP-PAYDATE.

           MOVE LP-TRCD TO QLP-TRCD.
           MOVE LP-REFNO TO QLP-REFNO.
           MOVE LP-TRAMT TO QLP-TRAMT.
           MOVE LP-APOTH TO QLP-APOTH.
           MOVE LP-APOT2 TO QLP-APOT2.
           MOVE LP-APINT TO QLP-APINT.
           MOVE LP-APLC TO QLP-APLC.
           MOVE LP-APCUR TO QLP-APCUR.
           MOVE LP-OTHBAL TO QLP-OTHBAL.
           MOVE LP-OT2BAL TO QLP-OT2BAL.
           MOVE LP-INTBAL TO QLP-INTBAL.
           MOVE LP-LCBAL TO QLP-LCBAL.
           MOVE LP-CURBAL TO QLP-CURBAL.
           MOVE LP-INTDUE TO QLP-INTDUE.
           MOVE LP-DLPROC TO QLP-DLPROC.
           MOVE LP-EARNED(1) TO QLP-EARNED-1.
           MOVE LP-EARNED(2) TO QLP-EARNED-2.
           MOVE LP-EARNED(3) TO QLP-EARNED-3.
           MOVE LP-INTPAID TO QLP-INTPAID.
           MOVE LP-SSNO TO QLP-SSNO.

           IF ( LP-PDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLP-PDTH-DATE
           ELSE
              MOVE LP-PDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLP-PDTH-DATE.


           IF ( LP-LCPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLP-LCPDTH-DATE
           ELSE
              MOVE LP-LCPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLP-LCPDTH-DATE.


           IF ( LP-INTPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLP-INTPDTH-DATE
           ELSE
              MOVE LP-INTPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLP-INTPDTH-DATE.

           MOVE LP-TILLNO TO QLP-TILLNO.
           MOVE LP-APINTOWE TO QLP-APINTOWE.
           MOVE LP-DLPARVHELD-ADJ TO QLP-DLPARVHELD-ADJ.
           MOVE LP-LT-SEQNO TO QLP-LT-SEQNO.
           MOVE LP-IBPC TO QLP-IBPC.
           MOVE LP-MANUAL-RB TO QLP-MANUAL-RB.
           MOVE LP-INSTYPES TO QLP-INSTYPES.
           MOVE LP-REV TO QLP-REV.
           MOVE LP-REVTRCD TO QLP-REVTRCD.
           MOVE LP-REVSEQNO TO QLP-REVSEQNO.
           MOVE LP-STATUSFG TO QLP-STATUSFG.
           MOVE LP-NEWSTAT-RATE TO QLP-NEWSTAT-RATE.
           MOVE LP-LCPARTIALS TO QLP-LCPARTIALS.
           MOVE LP-ADDON-ORIG-REGPY TO QLP-ADDON-ORIG-REGPY.

           IF ( LP-NEW1ST-PYDATE = ZEROES )
              MOVE "1900-01-01" TO QLP-NEW1ST-PYDATE
           ELSE
              MOVE LP-NEW1ST-PYDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLP-NEW1ST-PYDATE.


           IF ( LP-DEF-DUEDATE = ZEROES )
              MOVE "1900-01-01" TO QLP-DEF-DUEDATE
           ELSE
              MOVE LP-DEF-DUEDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLP-DEF-DUEDATE.

           MOVE LP-LCPAID TO QLP-LCPAID.

           IF ( LP-OLD1ST-PYDATE = ZEROES )
              MOVE "1900-01-01" TO QLP-OLD1ST-PYDATE
           ELSE
              MOVE LP-OLD1ST-PYDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLP-OLD1ST-PYDATE.

           MOVE LP-DEF-TERM TO QLP-DEF-TERM.
           MOVE LP-ADDON-LAST-PYAMT TO QLP-ADDON-LAST-PYAMT.
           MOVE LP-DLRVCHGS-ADJ(1) TO QLP-DLRVCHGS-ADJ-1.
           MOVE LP-DLRVCHGS-ADJ(2) TO QLP-DLRVCHGS-ADJ-2.
           MOVE LP-DLRVCHGS-ADJ(3) TO QLP-DLRVCHGS-ADJ-3.
           MOVE LP-DLPARVPAID-ADJ TO QLP-DLPARVPAID-ADJ.
           MOVE LP-DLNO TO QLP-DLNO.
           MOVE LP-PREPAY-PENALTY TO QLP-PREPAY-PENALTY.
           MOVE LP-PRORATED-INS-REB TO QLP-PRORATED-INS-REB.
           MOVE LP-POSTING-BR TO QLP-POSTING-BR.
           MOVE LP-NOLCHG TO QLP-NOLCHG.
           MOVE LP-COLLECTOR TO QLP-COLLECTOR.
           MOVE LP-EXTRA-PRIN-LFP TO QLP-EXTRA-PRIN-LFP.
           MOVE LP-REMAIN-INT-SINCE-LFP TO QLP-REMAIN-INT-SINCE-LFP.
           MOVE LP-REMAIN-PRIN-SINCE-LFP TO QLP-REMAIN-PRIN-SINCE-LFP.
           MOVE LP-APPLIED-TO-ESCROW TO QLP-APPLIED-TO-ESCROW.
           MOVE LP-ESCROW-PREPAID TO QLP-ESCROW-PREPAID.
           MOVE LP-PRIOR-POOLID TO QLP-PRIOR-POOLID.

           IF ( LP-ADDON-INTPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLP-ADDON-INTPDTH-DATE
           ELSE
              MOVE LP-ADDON-INTPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLP-ADDON-INTPDTH-DATE.

           MOVE LP-FROMCL TO QLP-FROMCL.
           MOVE LP-TOCL TO QLP-TOCL.
           MOVE LP-ALT-PREPAY-FG TO QLP-ALT-PREPAY-FG.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LP-ALL-BASE
              MOVE FILPATH-MACHINE         TO LP-ALL-MACHINE
              MOVE FILPATH-DATA            TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LPFILE NOT = "S" )
              MOVE LP-PATH-BEG             TO LP-SELECT-PATH-BEG
              MOVE LP-PATH-FILE            TO LP-SELECT-PATH-FILE
           ELSE
              MOVE LP-PATH-SQL             TO LP-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LP1-FILE.
           PERFORM LOAD-LP1-FILE.
           PERFORM OPEN-IT.
           MOVE LP-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LP1-FILE.
           PERFORM START-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-SELECT-PATH TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.

           IF ( LP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LP1-FILE.

           IF ( QLP1-WEND-SEQNO = ZEROES    ) OR
              ( QLP1-WEND-SEQNO NOT NUMERIC )
              MOVE LP-PATH-OWNBR        TO QLP1-WBEG-BRNO
              MOVE LP-PATH-OWNBR        TO QLP1-WEND-BRNO
              MOVE LP-ACCTNO            TO QLP1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLP1-WEND-ACCTNO
              MOVE LP-SEQNO             TO QLP1-WBEG-SEQNO
              MOVE ALL "9"              TO QLP1-WEND-SEQNO.

           EXEC SQL
            DECLARE LP1_CURSOR CURSOR FOR
            SELECT LPFILE.LP_BRNO,
                   LPFILE.LP_ACCTNO,
                   LPFILE.LP_SEQNO,
                   ISNULL(CAST(LPFILE.LP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LPFILE.LP_USERID,''),
                   ISNULL(CAST(LPFILE.LP_POSTDATE AS VARCHAR(10)),'0'),
                   LPFILE.LP_POSTTIME,
                   ISNULL(CAST(LPFILE.LP_TRDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_PAYDATE AS VARCHAR(10)),'0'),
                   ISNULL(LPFILE.LP_TRCD,''),
                   ISNULL(LPFILE.LP_REFNO,''),
                   LPFILE.LP_TRAMT,
                   LPFILE.LP_APOTH,
                   LPFILE.LP_APOT2,
                   LPFILE.LP_APINT,
                   LPFILE.LP_APLC,
                   LPFILE.LP_APCUR,
                   LPFILE.LP_OTHBAL,
                   LPFILE.LP_OT2BAL,
                   LPFILE.LP_INTBAL,
                   LPFILE.LP_LCBAL,
                   LPFILE.LP_CURBAL,
                   LPFILE.LP_INTDUE,
                   LPFILE.LP_DLPROC,
                   LPFILE.LP_EARNED_1,
                   LPFILE.LP_EARNED_2,
                   LPFILE.LP_EARNED_3,
                   LPFILE.LP_INTPAID,
                   LPFILE.LP_SSNO,
                   ISNULL(CAST(LPFILE.LP_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_TILLNO,
                   LPFILE.LP_APINTOWE,
                   LPFILE.LP_DLPARVHELD_ADJ,
                   LPFILE.LP_LT_SEQNO,
                   ISNULL(LPFILE.LP_IBPC,''),
                   ISNULL(LPFILE.LP_MANUAL_RB,''),
                   ISNULL(LPFILE.LP_INSTYPES,''),
                   ISNULL(LPFILE.LP_REV,''),
                   ISNULL(LPFILE.LP_REVTRCD,''),
                   LPFILE.LP_REVSEQNO,
                   ISNULL(LPFILE.LP_STATUSFG,''),
                   LPFILE.LP_NEWSTAT_RATE,
                   LPFILE.LP_LCPARTIALS,
                   LPFILE.LP_ADDON_ORIG_REGPY,
                   ISNULL(CAST(LPFILE.LP_NEW1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_DEF_DUEDATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_LCPAID,
                   ISNULL(CAST(LPFILE.LP_OLD1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_DEF_TERM,
                   LPFILE.LP_ADDON_LAST_PYAMT,
                   LPFILE.LP_DLRVCHGS_ADJ_1,
                   LPFILE.LP_DLRVCHGS_ADJ_2,
                   LPFILE.LP_DLRVCHGS_ADJ_3,
                   LPFILE.LP_DLPARVPAID_ADJ,
                   LPFILE.LP_DLNO,
                   LPFILE.LP_PREPAY_PENALTY,
                   LPFILE.LP_PRORATED_INS_REB,
                   LPFILE.LP_POSTING_BR,
                   LPFILE.LP_NOLCHG,
                   ISNULL(LPFILE.LP_COLLECTOR,''),
                   LPFILE.LP_EXTRA_PRIN_LFP,
                   LPFILE.LP_REMAIN_INT_SINCE_LFP,
                   LPFILE.LP_REMAIN_PRIN_SINCE_LFP,
                   LPFILE.LP_APPLIED_TO_ESCROW,
                   LPFILE.LP_ESCROW_PREPAID,
                   LPFILE.LP_PRIOR_POOLID,
                   ISNULL(CAST(LPFILE.LP_ADDON_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_FROMCL,
                   LPFILE.LP_TOCL,
                   ISNULL(LPFILE.LP_ALT_PREPAY_FG,'')
            FROM DBO.LPFILE
            WHERE LPFILE.LP_BRNO >= :QLP1-WBEG-BRNO
              AND LPFILE.LP_BRNO <= :QLP1-WEND-BRNO
              AND LPFILE.LP_ACCTNO >= :QLP1-WBEG-ACCTNO
              AND LPFILE.LP_ACCTNO <= :QLP1-WEND-ACCTNO
              AND LPFILE.LP_SEQNO >= :QLP1-WBEG-SEQNO
              AND LPFILE.LP_SEQNO <= :QLP1-WEND-SEQNO
            ORDER BY LPFILE.LP_BRNO,
                     LPFILE.LP_ACCTNO,
                     LPFILE.LP_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLP1-WBEG-BRNO 
                          QLP1-WEND-BRNO
                          QLP1-WBEG-ACCTNO
                          QLP1-WEND-ACCTNO
                          QLP1-WBEG-SEQNO
                          QLP1-WEND-SEQNO.

           MOVE STAT---GOOD TO LP1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-LP1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-SELECT-PATH TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.

           IF ( LP1-NG-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LP1-FILE.

           IF ( QLP1-WEND-SEQNO = ZEROES    ) OR
              ( QLP1-WEND-SEQNO NOT NUMERIC )
              MOVE LP-PATH-OWNBR        TO QLP1-WBEG-BRNO
              MOVE LP-PATH-OWNBR        TO QLP1-WEND-BRNO
              MOVE ALL "0"              TO QLP1-WBEG-ACCTNO
              MOVE LP-ACCTNO            TO QLP1-WEND-ACCTNO
              MOVE ALL "0"              TO QLP1-WBEG-SEQNO
              MOVE LP-SEQNO             TO QLP1-WEND-SEQNO.

           EXEC SQL
            DECLARE LP1_NG_CURSOR CURSOR FOR
            SELECT LPFILE.LP_BRNO,
                   LPFILE.LP_ACCTNO,
                   LPFILE.LP_SEQNO,
                   ISNULL(CAST(LPFILE.LP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LPFILE.LP_USERID,''),
                   ISNULL(CAST(LPFILE.LP_POSTDATE AS VARCHAR(10)),'0'),
                   LPFILE.LP_POSTTIME,
                   ISNULL(CAST(LPFILE.LP_TRDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_PAYDATE AS VARCHAR(10)),'0'),
                   ISNULL(LPFILE.LP_TRCD,''),
                   ISNULL(LPFILE.LP_REFNO,''),
                   LPFILE.LP_TRAMT,
                   LPFILE.LP_APOTH,
                   LPFILE.LP_APOT2,
                   LPFILE.LP_APINT,
                   LPFILE.LP_APLC,
                   LPFILE.LP_APCUR,
                   LPFILE.LP_OTHBAL,
                   LPFILE.LP_OT2BAL,
                   LPFILE.LP_INTBAL,
                   LPFILE.LP_LCBAL,
                   LPFILE.LP_CURBAL,
                   LPFILE.LP_INTDUE,
                   LPFILE.LP_DLPROC,
                   LPFILE.LP_EARNED_1,
                   LPFILE.LP_EARNED_2,
                   LPFILE.LP_EARNED_3,
                   LPFILE.LP_INTPAID,
                   LPFILE.LP_SSNO,
                   ISNULL(CAST(LPFILE.LP_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_TILLNO,
                   LPFILE.LP_APINTOWE,
                   LPFILE.LP_DLPARVHELD_ADJ,
                   LPFILE.LP_LT_SEQNO,
                   ISNULL(LPFILE.LP_IBPC,''),
                   ISNULL(LPFILE.LP_MANUAL_RB,''),
                   ISNULL(LPFILE.LP_INSTYPES,''),
                   ISNULL(LPFILE.LP_REV,''),
                   ISNULL(LPFILE.LP_REVTRCD,''),
                   LPFILE.LP_REVSEQNO,
                   ISNULL(LPFILE.LP_STATUSFG,''),
                   LPFILE.LP_NEWSTAT_RATE,
                   LPFILE.LP_LCPARTIALS,
                   LPFILE.LP_ADDON_ORIG_REGPY,
                   ISNULL(CAST(LPFILE.LP_NEW1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_DEF_DUEDATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_LCPAID,
                   ISNULL(CAST(LPFILE.LP_OLD1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_DEF_TERM,
                   LPFILE.LP_ADDON_LAST_PYAMT,
                   LPFILE.LP_DLRVCHGS_ADJ_1,
                   LPFILE.LP_DLRVCHGS_ADJ_2,
                   LPFILE.LP_DLRVCHGS_ADJ_3,
                   LPFILE.LP_DLPARVPAID_ADJ,
                   LPFILE.LP_DLNO,
                   LPFILE.LP_PREPAY_PENALTY,
                   LPFILE.LP_PRORATED_INS_REB,
                   LPFILE.LP_POSTING_BR,
                   LPFILE.LP_NOLCHG,
                   ISNULL(LPFILE.LP_COLLECTOR,''),
                   LPFILE.LP_EXTRA_PRIN_LFP,
                   LPFILE.LP_REMAIN_INT_SINCE_LFP,
                   LPFILE.LP_REMAIN_PRIN_SINCE_LFP,
                   LPFILE.LP_APPLIED_TO_ESCROW,
                   LPFILE.LP_ESCROW_PREPAID,
                   LPFILE.LP_PRIOR_POOLID,
                   ISNULL(CAST(LPFILE.LP_ADDON_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_FROMCL,
                   LPFILE.LP_TOCL,
                   ISNULL(LPFILE.LP_ALT_PREPAY_FG,'')
            FROM DBO.LPFILE
            WHERE LPFILE.LP_BRNO >= :QLP1-WBEG-BRNO
              AND LPFILE.LP_BRNO <= :QLP1-WEND-BRNO
              AND LPFILE.LP_ACCTNO >= :QLP1-WBEG-ACCTNO
              AND LPFILE.LP_ACCTNO <= :QLP1-WEND-ACCTNO
              AND LPFILE.LP_SEQNO >= :QLP1-WBEG-SEQNO
              AND LPFILE.LP_SEQNO <= :QLP1-WEND-SEQNO
            ORDER BY LPFILE.LP_BRNO DESC,
                     LPFILE.LP_ACCTNO DESC,
                     LPFILE.LP_SEQNO DESC
           END-EXEC.

           EXEC SQL
               OPEN LP1_NG_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLP1-WBEG-BRNO 
                          QLP1-WEND-BRNO
                          QLP1-WBEG-ACCTNO
                          QLP1-WEND-ACCTNO
                          QLP1-WBEG-SEQNO
                          QLP1-WEND-SEQNO.

           MOVE STAT---GOOD TO LP1-NG-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LP1-FILE.
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO. 
           MOVE LP-SELECT-PATH TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.

           MOVE LP-BRNO   TO QLP-BRNO.
           MOVE LP-ACCTNO TO QLP-ACCTNO.
           MOVE LP-SEQNO  TO QLP-SEQNO.

           EXEC SQL
            SELECT LPFILE.LP_BRNO,
                   LPFILE.LP_ACCTNO,
                   LPFILE.LP_SEQNO,
                   ISNULL(CAST(LPFILE.LP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LPFILE.LP_USERID,''),
                   ISNULL(CAST(LPFILE.LP_POSTDATE AS VARCHAR(10)),'0'),
                   LPFILE.LP_POSTTIME,
                   ISNULL(CAST(LPFILE.LP_TRDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_PAYDATE AS VARCHAR(10)),'0'),
                   ISNULL(LPFILE.LP_TRCD,''),
                   ISNULL(LPFILE.LP_REFNO,''),
                   LPFILE.LP_TRAMT,
                   LPFILE.LP_APOTH,
                   LPFILE.LP_APOT2,
                   LPFILE.LP_APINT,
                   LPFILE.LP_APLC,
                   LPFILE.LP_APCUR,
                   LPFILE.LP_OTHBAL,
                   LPFILE.LP_OT2BAL,
                   LPFILE.LP_INTBAL,
                   LPFILE.LP_LCBAL,
                   LPFILE.LP_CURBAL,
                   LPFILE.LP_INTDUE,
                   LPFILE.LP_DLPROC,
                   LPFILE.LP_EARNED_1,
                   LPFILE.LP_EARNED_2,
                   LPFILE.LP_EARNED_3,
                   LPFILE.LP_INTPAID,
                   LPFILE.LP_SSNO,
                   ISNULL(CAST(LPFILE.LP_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_TILLNO,
                   LPFILE.LP_APINTOWE,
                   LPFILE.LP_DLPARVHELD_ADJ,
                   LPFILE.LP_LT_SEQNO,
                   ISNULL(LPFILE.LP_IBPC,''),
                   ISNULL(LPFILE.LP_MANUAL_RB,''),
                   ISNULL(LPFILE.LP_INSTYPES,''),
                   ISNULL(LPFILE.LP_REV,''),
                   ISNULL(LPFILE.LP_REVTRCD,''),
                   LPFILE.LP_REVSEQNO,
                   ISNULL(LPFILE.LP_STATUSFG,''),
                   LPFILE.LP_NEWSTAT_RATE,
                   LPFILE.LP_LCPARTIALS,
                   LPFILE.LP_ADDON_ORIG_REGPY,
                   ISNULL(CAST(LPFILE.LP_NEW1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_DEF_DUEDATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_LCPAID,
                   ISNULL(CAST(LPFILE.LP_OLD1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_DEF_TERM,
                   LPFILE.LP_ADDON_LAST_PYAMT,
                   LPFILE.LP_DLRVCHGS_ADJ_1,
                   LPFILE.LP_DLRVCHGS_ADJ_2,
                   LPFILE.LP_DLRVCHGS_ADJ_3,
                   LPFILE.LP_DLPARVPAID_ADJ,
                   LPFILE.LP_DLNO,
                   LPFILE.LP_PREPAY_PENALTY,
                   LPFILE.LP_PRORATED_INS_REB,
                   LPFILE.LP_POSTING_BR,
                   LPFILE.LP_NOLCHG,
                   ISNULL(LPFILE.LP_COLLECTOR,''),
                   LPFILE.LP_EXTRA_PRIN_LFP,
                   LPFILE.LP_REMAIN_INT_SINCE_LFP,
                   LPFILE.LP_REMAIN_PRIN_SINCE_LFP,
                   LPFILE.LP_APPLIED_TO_ESCROW,
                   LPFILE.LP_ESCROW_PREPAID,
                   LPFILE.LP_PRIOR_POOLID,
                   ISNULL(CAST(LPFILE.LP_ADDON_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_FROMCL,
                   LPFILE.LP_TOCL,
                   ISNULL(LPFILE.LP_ALT_PREPAY_FG,'')
            INTO :QLP-REC
            FROM DBO.LPFILE
            WHERE LPFILE.LP_BRNO = :QLP-BRNO
              AND LPFILE.LP_ACCTNO = :QLP-ACCTNO
              AND LPFILE.LP_SEQNO = :QLP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.

      *-----------------------------------------------------------------
       READ-LP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-SELECT-PATH TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           INITIALIZE QLP-REC.

           IF ( LP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LP1_CURSOR INTO :QLP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.

      *-----------------------------------------------------------------
       READ-LP1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-SELECT-PATH TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           INITIALIZE QLP-REC.

           IF ( LP1-NG-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LP1_NG_CURSOR INTO :QLP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LP1-FILE.

           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           ACCEPT LP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LP-SELECT-PATH TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           PERFORM SET-LP-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LPFILE
             (LPFILE.LP_BRNO,
              LPFILE.LP_ACCTNO,
              LPFILE.LP_SEQNO,
              LPFILE.LP_LTOUCH_DATE,
              LPFILE.LP_USERID,
              LPFILE.LP_POSTDATE,
              LPFILE.LP_POSTTIME,
              LPFILE.LP_TRDATE,
              LPFILE.LP_PAYDATE,
              LPFILE.LP_TRCD,
              LPFILE.LP_REFNO,
              LPFILE.LP_TRAMT,
              LPFILE.LP_APOTH,
              LPFILE.LP_APOT2,
              LPFILE.LP_APINT,
              LPFILE.LP_APLC,
              LPFILE.LP_APCUR,
              LPFILE.LP_OTHBAL,
              LPFILE.LP_OT2BAL,
              LPFILE.LP_INTBAL,
              LPFILE.LP_LCBAL,
              LPFILE.LP_CURBAL,
              LPFILE.LP_INTDUE,
              LPFILE.LP_DLPROC,
              LPFILE.LP_EARNED_1,
              LPFILE.LP_EARNED_2,
              LPFILE.LP_EARNED_3,
              LPFILE.LP_INTPAID,
              LPFILE.LP_SSNO,
              LPFILE.LP_PDTH_DATE,
              LPFILE.LP_LCPDTH_DATE,
              LPFILE.LP_INTPDTH_DATE,
              LPFILE.LP_TILLNO,
              LPFILE.LP_APINTOWE,
              LPFILE.LP_DLPARVHELD_ADJ,
              LPFILE.LP_LT_SEQNO,
              LPFILE.LP_IBPC,
              LPFILE.LP_MANUAL_RB,
              LPFILE.LP_INSTYPES,
              LPFILE.LP_REV,
              LPFILE.LP_REVTRCD,
              LPFILE.LP_REVSEQNO,
              LPFILE.LP_STATUSFG,
              LPFILE.LP_NEWSTAT_RATE,
              LPFILE.LP_LCPARTIALS,
              LPFILE.LP_ADDON_ORIG_REGPY,
              LPFILE.LP_NEW1ST_PYDATE,
              LPFILE.LP_DEF_DUEDATE,
              LPFILE.LP_LCPAID,
              LPFILE.LP_OLD1ST_PYDATE,
              LPFILE.LP_DEF_TERM,
              LPFILE.LP_ADDON_LAST_PYAMT,
              LPFILE.LP_DLRVCHGS_ADJ_1,
              LPFILE.LP_DLRVCHGS_ADJ_2,
              LPFILE.LP_DLRVCHGS_ADJ_3,
              LPFILE.LP_DLPARVPAID_ADJ,
              LPFILE.LP_DLNO,
              LPFILE.LP_PREPAY_PENALTY,
              LPFILE.LP_PRORATED_INS_REB,
              LPFILE.LP_POSTING_BR,
              LPFILE.LP_NOLCHG,
              LPFILE.LP_COLLECTOR,
              LPFILE.LP_EXTRA_PRIN_LFP,
              LPFILE.LP_REMAIN_INT_SINCE_LFP,
              LPFILE.LP_REMAIN_PRIN_SINCE_LFP,
              LPFILE.LP_APPLIED_TO_ESCROW,
              LPFILE.LP_ESCROW_PREPAID,
              LPFILE.LP_PRIOR_POOLID,
              LPFILE.LP_ADDON_INTPDTH_DATE,
              LPFILE.LP_FROMCL,
              LPFILE.LP_TOCL,
              LPFILE.LP_ALT_PREPAY_FG)
            VALUES
             (:QLP-BRNO,
              :QLP-ACCTNO,
              :QLP-SEQNO,
              NULLIF(:QLP-LTOUCH-DATE,'1900-01-01'),
              :QLP-USERID,
              NULLIF(:QLP-POSTDATE,'1900-01-01'),
              :QLP-POSTTIME,
              NULLIF(:QLP-TRDATE,'1900-01-01'),
              NULLIF(:QLP-PAYDATE,'1900-01-01'),
              :QLP-TRCD,
              :QLP-REFNO,
              :QLP-TRAMT,
              :QLP-APOTH,
              :QLP-APOT2,
              :QLP-APINT,
              :QLP-APLC,
              :QLP-APCUR,
              :QLP-OTHBAL,
              :QLP-OT2BAL,
              :QLP-INTBAL,
              :QLP-LCBAL,
              :QLP-CURBAL,
              :QLP-INTDUE,
              :QLP-DLPROC,
              :QLP-EARNED-1,
              :QLP-EARNED-2,
              :QLP-EARNED-3,
              :QLP-INTPAID,
              :QLP-SSNO,
              NULLIF(:QLP-PDTH-DATE,'1900-01-01'),
              NULLIF(:QLP-LCPDTH-DATE,'1900-01-01'),
              NULLIF(:QLP-INTPDTH-DATE,'1900-01-01'),
              :QLP-TILLNO,
              :QLP-APINTOWE,
              :QLP-DLPARVHELD-ADJ,
              :QLP-LT-SEQNO,
              :QLP-IBPC,
              :QLP-MANUAL-RB,
              :QLP-INSTYPES,
              :QLP-REV,
              :QLP-REVTRCD,
              :QLP-REVSEQNO,
              :QLP-STATUSFG,
              :QLP-NEWSTAT-RATE,
              :QLP-LCPARTIALS,
              :QLP-ADDON-ORIG-REGPY,
              NULLIF(:QLP-NEW1ST-PYDATE,'1900-01-01'),
              NULLIF(:QLP-DEF-DUEDATE,'1900-01-01'),
              :QLP-LCPAID,
              NULLIF(:QLP-OLD1ST-PYDATE,'1900-01-01'),
              :QLP-DEF-TERM,
              :QLP-ADDON-LAST-PYAMT,
              :QLP-DLRVCHGS-ADJ-1,
              :QLP-DLRVCHGS-ADJ-2,
              :QLP-DLRVCHGS-ADJ-3,
              :QLP-DLPARVPAID-ADJ,
              :QLP-DLNO,
              :QLP-PREPAY-PENALTY,
              :QLP-PRORATED-INS-REB,
              :QLP-POSTING-BR,
              :QLP-NOLCHG,
              :QLP-COLLECTOR,
              :QLP-EXTRA-PRIN-LFP,
              :QLP-REMAIN-INT-SINCE-LFP,
              :QLP-REMAIN-PRIN-SINCE-LFP,
              :QLP-APPLIED-TO-ESCROW,
              :QLP-ESCROW-PREPAID,
              :QLP-PRIOR-POOLID,
              NULLIF(:QLP-ADDON-INTPDTH-DATE,'1900-01-01'),
              :QLP-FROMCL,
              :QLP-TOCL,
              :QLP-ALT-PREPAY-FG)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LP1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LP1-FILE.

           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           ACCEPT LP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LP-SELECT-PATH TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           PERFORM SET-LP-FIELDS.

           EXEC SQL
            UPDATE DBO.LPFILE
             SET
              LPFILE.LP_BRNO = :QLP-BRNO,
              LPFILE.LP_ACCTNO = :QLP-ACCTNO,
              LPFILE.LP_SEQNO = :QLP-SEQNO,
              LPFILE.LP_LTOUCH_DATE =
                 NULLIF(:QLP-LTOUCH-DATE,'1900-01-01'),
              LPFILE.LP_USERID = :QLP-USERID,
              LPFILE.LP_POSTDATE =
                 NULLIF(:QLP-POSTDATE,'1900-01-01'),
              LPFILE.LP_POSTTIME = :QLP-POSTTIME,
              LPFILE.LP_TRDATE =
                 NULLIF(:QLP-TRDATE,'1900-01-01'),
              LPFILE.LP_PAYDATE =
                 NULLIF(:QLP-PAYDATE,'1900-01-01'),
              LPFILE.LP_TRCD = :QLP-TRCD,
              LPFILE.LP_REFNO = :QLP-REFNO,
              LPFILE.LP_TRAMT = :QLP-TRAMT,
              LPFILE.LP_APOTH = :QLP-APOTH,
              LPFILE.LP_APOT2 = :QLP-APOT2,
              LPFILE.LP_APINT = :QLP-APINT,
              LPFILE.LP_APLC = :QLP-APLC,
              LPFILE.LP_APCUR = :QLP-APCUR,
              LPFILE.LP_OTHBAL = :QLP-OTHBAL,
              LPFILE.LP_OT2BAL = :QLP-OT2BAL,
              LPFILE.LP_INTBAL = :QLP-INTBAL,
              LPFILE.LP_LCBAL = :QLP-LCBAL,
              LPFILE.LP_CURBAL = :QLP-CURBAL,
              LPFILE.LP_INTDUE = :QLP-INTDUE,
              LPFILE.LP_DLPROC = :QLP-DLPROC,
              LPFILE.LP_EARNED_1 = :QLP-EARNED-1,
              LPFILE.LP_EARNED_2 = :QLP-EARNED-2,
              LPFILE.LP_EARNED_3 = :QLP-EARNED-3,
              LPFILE.LP_INTPAID = :QLP-INTPAID,
              LPFILE.LP_SSNO = :QLP-SSNO,
              LPFILE.LP_PDTH_DATE =
                 NULLIF(:QLP-PDTH-DATE,'1900-01-01'),
              LPFILE.LP_LCPDTH_DATE =
                 NULLIF(:QLP-LCPDTH-DATE,'1900-01-01'),
              LPFILE.LP_INTPDTH_DATE =
                 NULLIF(:QLP-INTPDTH-DATE,'1900-01-01'),
              LPFILE.LP_TILLNO = :QLP-TILLNO,
              LPFILE.LP_APINTOWE = :QLP-APINTOWE,
              LPFILE.LP_DLPARVHELD_ADJ = :QLP-DLPARVHELD-ADJ,
              LPFILE.LP_LT_SEQNO = :QLP-LT-SEQNO,
              LPFILE.LP_IBPC = :QLP-IBPC,
              LPFILE.LP_MANUAL_RB = :QLP-MANUAL-RB,
              LPFILE.LP_INSTYPES = :QLP-INSTYPES,
              LPFILE.LP_REV = :QLP-REV,
              LPFILE.LP_REVTRCD = :QLP-REVTRCD,
              LPFILE.LP_REVSEQNO = :QLP-REVSEQNO,
              LPFILE.LP_STATUSFG = :QLP-STATUSFG,
              LPFILE.LP_NEWSTAT_RATE = :QLP-NEWSTAT-RATE,
              LPFILE.LP_LCPARTIALS = :QLP-LCPARTIALS,
              LPFILE.LP_ADDON_ORIG_REGPY = :QLP-ADDON-ORIG-REGPY,
              LPFILE.LP_NEW1ST_PYDATE =
                 NULLIF(:QLP-NEW1ST-PYDATE,'1900-01-01'),
              LPFILE.LP_DEF_DUEDATE =
                 NULLIF(:QLP-DEF-DUEDATE,'1900-01-01'),
              LPFILE.LP_LCPAID = :QLP-LCPAID,
              LPFILE.LP_OLD1ST_PYDATE =
                 NULLIF(:QLP-OLD1ST-PYDATE,'1900-01-01'),
              LPFILE.LP_DEF_TERM = :QLP-DEF-TERM,
              LPFILE.LP_ADDON_LAST_PYAMT = :QLP-ADDON-LAST-PYAMT,
              LPFILE.LP_DLRVCHGS_ADJ_1 = :QLP-DLRVCHGS-ADJ-1,
              LPFILE.LP_DLRVCHGS_ADJ_2 = :QLP-DLRVCHGS-ADJ-2,
              LPFILE.LP_DLRVCHGS_ADJ_3 = :QLP-DLRVCHGS-ADJ-3,
              LPFILE.LP_DLPARVPAID_ADJ = :QLP-DLPARVPAID-ADJ,
              LPFILE.LP_DLNO = :QLP-DLNO,
              LPFILE.LP_PREPAY_PENALTY = :QLP-PREPAY-PENALTY,
              LPFILE.LP_PRORATED_INS_REB = :QLP-PRORATED-INS-REB,
              LPFILE.LP_POSTING_BR = :QLP-POSTING-BR,
              LPFILE.LP_NOLCHG = :QLP-NOLCHG,
              LPFILE.LP_COLLECTOR = :QLP-COLLECTOR,
              LPFILE.LP_EXTRA_PRIN_LFP = :QLP-EXTRA-PRIN-LFP,
              LPFILE.LP_REMAIN_INT_SINCE_LFP =
                   :QLP-REMAIN-INT-SINCE-LFP,
              LPFILE.LP_REMAIN_PRIN_SINCE_LFP =
                   :QLP-REMAIN-PRIN-SINCE-LFP,
              LPFILE.LP_APPLIED_TO_ESCROW =
                   :QLP-APPLIED-TO-ESCROW,
              LPFILE.LP_ESCROW_PREPAID = :QLP-ESCROW-PREPAID,
              LPFILE.LP_PRIOR_POOLID = :QLP-PRIOR-POOLID,
              LPFILE.LP_ADDON_INTPDTH_DATE =
                 NULLIF(:QLP-ADDON-INTPDTH-DATE,'1900-01-01'),
              LPFILE.LP_FROMCL = :QLP-FROMCL,
              LPFILE.LP_TOCL = :QLP-TOCL,
              LPFILE.LP_ALT_PREPAY_FG = :QLP-ALT-PREPAY-FG
             WHERE LPFILE.LP_BRNO = :QLP-BRNO
               AND LPFILE.LP_ACCTNO = :QLP-ACCTNO
               AND LPFILE.LP_SEQNO = :QLP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LP1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LP1-FILE.

           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           PERFORM DELETE-IT.
           MOVE LP-SELECT-PATH TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           PERFORM SET-LP-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LPFILE
             WHERE LPFILE.LP_BRNO = :QLP-BRNO
               AND LPFILE.LP_ACCTNO = :QLP-ACCTNO
               AND LPFILE.LP_SEQNO = :QLP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LP1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LP1-FILE.

           IF ( LP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LP1-CURSOR-STAT.

           IF ( LP1-NG-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LP1_NG_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LP1-NG-CURSOR-STAT.

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
              DESTROY IOLPIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
