       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOMPRN.
       AUTHOR.        BAH.
       DATE-WRITTEN.  04-14-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOMPRN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : MPFILE 
      * TABLE DESC    : MEMO PAYMENT FILE FROM A CONVERSION
      * OUTPUT FILE   : XPC/IOMPRN.PRE
      * USED FOR      : LIBLP/LPWSMP.CPY  --- QMP-REC
      *                 LIBLP/LPMP1R.CPY  --- READ
      *                 LIBLP/LPMP1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0414 ***NEW***
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
           "@(#)IO/IOMPRN.CBL S34 05/10/21-14:28:14 >".

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
       COPY "LIBLP/LP01MP.CPY".
       COPY "LIBLP/LPWSMP.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QMP1-WHERE-BEG-KEY.
           03  QMP1-WBEG-BRNO             PIC 9(04).
           03  QMP1-WBEG-ACCTNO           PIC 9(08).
           03  QMP1-WBEG-SEQNO            PIC 9(05).

       01  QMP1-WHERE-END-KEY.
           03  QMP1-WEND-BRNO             PIC 9(04).
           03  QMP1-WEND-ACCTNO           PIC 9(08).
           03  QMP1-WEND-SEQNO            PIC 9(05).
 
       01  QMP-REC.
           03  QMP-BRNO             PIC  9(4).
           03  QMP-ACCTNO           PIC  9(8).
           03  QMP-SEQNO            PIC  9(5).
           03  QMP-LTOUCH-DATE          PIC X(10).
           03  QMP-USERID               PIC X(10).
           03  QMP-POSTDATE             PIC X(10).
           03  QMP-POSTTIME             PIC 9(6).
           03  QMP-TRDATE               PIC X(10).
           03  QMP-PAYDATE              PIC X(10).
           03  QMP-TRCD                 PIC XX.
           03  QMP-REFNO                PIC X(5).
           03  QMP-TRAMT                PIC S9(7)V99.
           03  QMP-APOTH                PIC S9(7)V99.
           03  QMP-APOT2                PIC S9(7)V99.
           03  QMP-APINT                PIC S9(7)V99.
           03  QMP-APLC                 PIC S9(7)V99.
           03  QMP-APCUR                PIC S9(7)V99.
           03  QMP-OTHBAL               PIC S9(7)V99.
           03  QMP-OT2BAL               PIC S9(7)V99.
           03  QMP-INTBAL               PIC S9(7)V99.
           03  QMP-LCBAL                PIC S9(7)V99.
           03  QMP-CURBAL               PIC S9(7)V99.
           03  QMP-INTDUE               PIC S9(7)V99.
           03  QMP-DLPROC               PIC S9(7)V99.
           03  QMP-EARNED-1             PIC S9(7)V99.
           03  QMP-EARNED-2             PIC S9(7)V99.
           03  QMP-EARNED-3             PIC S9(7)V99.
           03  QMP-INTPAID              PIC S9(7)V99.
           03  QMP-SSNO                 PIC 9(10).
           03  QMP-PDTH-DATE            PIC X(10).
           03  QMP-LCPDTH-DATE          PIC X(10).
           03  QMP-INTPDTH-DATE         PIC X(10).
           03  QMP-TILLNO               PIC 99.
           03  QMP-APINTOWE             PIC S9(7)V99.
           03  QMP-DLPARVHELD-ADJ       PIC S9(7)V99.
           03  QMP-LT-SEQNO             PIC 9(3).
           03  QMP-IBPC                 PIC X.
           03  QMP-MANUAL-RB            PIC X.
           03  QMP-INSTYPES             PIC X.
           03  QMP-REV                  PIC X.
           03  QMP-REVTRCD              PIC XX.
           03  QMP-REVSEQNO             PIC 9(5).
           03  QMP-STATUSFG             PIC X.
           03  QMP-NEWSTAT-RATE         PIC 999V999.
           03  QMP-LCPARTIALS           PIC S9(7)V99.
           03  QMP-ADDON-ORIG-REGPY     PIC S9(7)V99.
           03  QMP-NEW1ST-PYDATE        PIC X(10).
           03  QMP-DEF-DUEDATE          PIC X(10).
           03  QMP-LCPAID               PIC S9(7)V99.
           03  QMP-OLD1ST-PYDATE        PIC X(10).
           03  QMP-DEF-TERM             PIC 9(3).
           03  QMP-ADDON-LAST-PYAMT     PIC S9(7)V99.
           03  QMP-DLRVCHGS-ADJ-1       PIC S9(7)V99.
           03  QMP-DLRVCHGS-ADJ-2       PIC S9(7)V99.
           03  QMP-DLRVCHGS-ADJ-3       PIC S9(7)V99.
           03  QMP-DLPARVPAID-ADJ       PIC S9(7)V99.
           03  QMP-DLNO                  PIC 9(4)V99.
           03  QMP-PREPAY-PENALTY        PIC S9(7)V99.
           03  QMP-PRORATED-INS-REB      PIC S9(7)V99.
           03  QMP-POSTING-BR            PIC 9(4).
           03  QMP-NOLCHG                PIC S9(3).
           03  QMP-COLLECTOR             PIC X(3).
           03  QMP-EXTRA-PRIN-LFP        PIC S9(7)V99.
           03  QMP-REMAIN-INT-SINCE-LFP  PIC S9(7)V99.
           03  QMP-REMAIN-PRIN-SINCE-LFP PIC S9(7)V99.
           03  QMP-APPLIED-TO-ESCROW     PIC S9(7)V99.
           03  QMP-ESCROW-PREPAID        PIC S9(7)V99.
           03  QMP-PRIOR-POOLID          PIC 9(5).
           03  QMP-ADDON-INTPDTH-DATE    PIC X(10).
           03  QMP-FROMCL                PIC  999.
           03  QMP-TOCL                  PIC  999.
           03  QMP-ALT-PREPAY-FG         PIC X.

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
           03  D01-MP-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(01).
           03  D01-MP-ACCTNO                  PIC 9(08).
           03  FILLER                         PIC X(01).
           03  D01-MP-SEQNO                   PIC 9(05).
           03  FILLER                         PIC X(01).
           03  D01-MP-TRDATE                  PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-MP-TRCD                    PIC XX.
           03  FILLER                         PIC X(03).
           03  D01-MP-REFNO                   PIC X(5).
           03  FILLER                         PIC X(02).
           03  D01-MP-TRAMT                   PIC 9(7).99.


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
       COPY "LIBIO/IOMPRN_DEF.CPY".
       COPY "LIBIO/IOMPRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOMPRN_SCN.CPY".

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
           MOVE "IOMPRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOMPRN-QUEUE-POS  TO Q-POS.
           MOVE IOMPRN-COPIES-POS TO C-POS.
           MOVE IOMPRN-PRU-POS    TO P-POS.
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

           MOVE "B"                        TO MP-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QMP1-WBEG-BRNO
                              QMP1-WEND-BRNO.
           MOVE BEG-ACCTNO TO QMP1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QMP1-WEND-ACCTNO.
           MOVE BEG-SEQNO  TO QMP1-WBEG-SEQNO.
           MOVE END-SEQNO  TO QMP1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-MP1-FILE.
           PERFORM START-MP1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-MP1-FILE-NEXT.
           IF ( IO-BAD ) OR ( MP-BRNO NOT = MP-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( MP-ACCTNO < BEG-ACCTNO )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( MP-ACCTNO > END-ACCTNO )
              GO TO PROCESS-BRANCH-MODULE-END.

           IF MP-SEQNO < BEG-SEQNO OR > END-SEQNO
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE MP-BRNO                    TO D01-MP-BRNO.
           MOVE MP-ACCTNO                  TO D01-MP-ACCTNO.
           MOVE MP-SEQNO                   TO D01-MP-SEQNO.
           MOVE MP-TRDATE                  TO D01-MP-TRDATE.
           MOVE MP-TRCD                    TO D01-MP-TRCD.
           MOVE MP-REFNO                   TO D01-MP-REFNO.
           MOVE MP-TRAMT                   TO D01-MP-TRAMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-MP1-FILE.

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

           MOVE MP1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "SQL TABLE IO UTILITY - MPFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOMPRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOMPRN-SYSDATE.
           DISPLAY IOMPRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOMPRN-FORM-FIELDS TO WR-BUF.
           MOVE IOMPRN-FORM-LIST TO WR-LIST.
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

           MOVE IOMPRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOMPRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOMPRN_EVA.CPY".
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
      *    LOAD MP-REC FROM QMP-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-MP-FIELDS.
           MOVE QMP-BRNO TO MP-BRNO.
           MOVE QMP-ACCTNO TO MP-ACCTNO.
           MOVE QMP-SEQNO TO MP-SEQNO.

           IF ( QMP-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO MP-LTOUCH-DATE
           ELSE
              MOVE QMP-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO MP-LTOUCH-DATE.

           MOVE QMP-USERID TO MP-USERID.

           IF ( QMP-POSTDATE = "1900-01-01" )
              MOVE ZEROES TO MP-POSTDATE
           ELSE
              MOVE QMP-POSTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO MP-POSTDATE.

           MOVE QMP-POSTTIME TO MP-POSTTIME.

           IF ( QMP-TRDATE = "1900-01-01" )
              MOVE ZEROES TO MP-TRDATE
           ELSE
              MOVE QMP-TRDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO MP-TRDATE.


           IF ( QMP-PAYDATE = "1900-01-01" )
              MOVE ZEROES TO MP-PAYDATE
           ELSE
              MOVE QMP-PAYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO MP-PAYDATE.

           MOVE QMP-TRCD TO MP-TRCD.
           MOVE QMP-REFNO TO MP-REFNO.
           MOVE QMP-TRAMT TO MP-TRAMT.
           MOVE QMP-APOTH TO MP-APOTH.
           MOVE QMP-APOT2 TO MP-APOT2.
           MOVE QMP-APINT TO MP-APINT.
           MOVE QMP-APLC TO MP-APLC.
           MOVE QMP-APCUR TO MP-APCUR.
           MOVE QMP-OTHBAL TO MP-OTHBAL.
           MOVE QMP-OT2BAL TO MP-OT2BAL.
           MOVE QMP-INTBAL TO MP-INTBAL.
           MOVE QMP-LCBAL TO MP-LCBAL.
           MOVE QMP-CURBAL TO MP-CURBAL.
           MOVE QMP-INTDUE TO MP-INTDUE.
           MOVE QMP-DLPROC TO MP-DLPROC.
           MOVE QMP-EARNED-1 TO MP-EARNED(1).
           MOVE QMP-EARNED-2 TO MP-EARNED(2).
           MOVE QMP-EARNED-3 TO MP-EARNED(3).
           MOVE QMP-INTPAID TO MP-INTPAID.
           MOVE QMP-SSNO TO MP-SSNO.

           IF ( QMP-PDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO MP-PDTH-DATE
           ELSE
              MOVE QMP-PDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO MP-PDTH-DATE.


           IF ( QMP-LCPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO MP-LCPDTH-DATE
           ELSE
              MOVE QMP-LCPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO MP-LCPDTH-DATE.


           IF ( QMP-INTPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO MP-INTPDTH-DATE
           ELSE
              MOVE QMP-INTPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO MP-INTPDTH-DATE.

           MOVE QMP-TILLNO TO MP-TILLNO.
           MOVE QMP-APINTOWE TO MP-APINTOWE.
           MOVE QMP-DLPARVHELD-ADJ TO MP-DLPARVHELD-ADJ.
           MOVE QMP-LT-SEQNO TO MP-LT-SEQNO.
           MOVE QMP-IBPC TO MP-IBPC.
           MOVE QMP-MANUAL-RB TO MP-MANUAL-RB.
           MOVE QMP-INSTYPES TO MP-INSTYPES.
           MOVE QMP-REV TO MP-REV.
           MOVE QMP-REVTRCD TO MP-REVTRCD.
           MOVE QMP-REVSEQNO TO MP-REVSEQNO.
           MOVE QMP-STATUSFG TO MP-STATUSFG.
           MOVE QMP-NEWSTAT-RATE TO MP-NEWSTAT-RATE.
           MOVE QMP-LCPARTIALS TO MP-LCPARTIALS.
           MOVE QMP-ADDON-ORIG-REGPY TO MP-ADDON-ORIG-REGPY.

           IF ( QMP-NEW1ST-PYDATE = "1900-01-01" )
              MOVE ZEROES TO MP-NEW1ST-PYDATE
           ELSE
              MOVE QMP-NEW1ST-PYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO MP-NEW1ST-PYDATE.


           IF ( QMP-DEF-DUEDATE = "1900-01-01" )
              MOVE ZEROES TO MP-DEF-DUEDATE
           ELSE
              MOVE QMP-DEF-DUEDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO MP-DEF-DUEDATE.

           MOVE QMP-LCPAID TO MP-LCPAID.

           IF ( QMP-OLD1ST-PYDATE = "1900-01-01" )
              MOVE ZEROES TO MP-OLD1ST-PYDATE
           ELSE
              MOVE QMP-OLD1ST-PYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO MP-OLD1ST-PYDATE.

           MOVE QMP-DEF-TERM TO MP-DEF-TERM.
           MOVE QMP-ADDON-LAST-PYAMT TO MP-ADDON-LAST-PYAMT.
           MOVE QMP-DLRVCHGS-ADJ-1 TO MP-DLRVCHGS-ADJ(1).
           MOVE QMP-DLRVCHGS-ADJ-2 TO MP-DLRVCHGS-ADJ(2).
           MOVE QMP-DLRVCHGS-ADJ-3 TO MP-DLRVCHGS-ADJ(3).
           MOVE QMP-DLPARVPAID-ADJ TO MP-DLPARVPAID-ADJ.
           MOVE QMP-DLNO TO MP-DLNO.
           MOVE QMP-PREPAY-PENALTY TO MP-PREPAY-PENALTY.
           MOVE QMP-PRORATED-INS-REB TO MP-PRORATED-INS-REB.
           MOVE QMP-POSTING-BR TO MP-POSTING-BR.
           MOVE QMP-NOLCHG TO MP-NOLCHG.
           MOVE QMP-COLLECTOR TO MP-COLLECTOR.
           MOVE QMP-EXTRA-PRIN-LFP TO MP-EXTRA-PRIN-LFP.
           MOVE QMP-REMAIN-INT-SINCE-LFP TO MP-REMAIN-INT-SINCE-LFP.
           MOVE QMP-REMAIN-PRIN-SINCE-LFP TO MP-REMAIN-PRIN-SINCE-LFP.
           MOVE QMP-APPLIED-TO-ESCROW TO MP-APPLIED-TO-ESCROW.
           MOVE QMP-ESCROW-PREPAID TO MP-ESCROW-PREPAID.
           MOVE QMP-PRIOR-POOLID TO MP-PRIOR-POOLID.

           IF ( QMP-ADDON-INTPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO MP-ADDON-INTPDTH-DATE
           ELSE
              MOVE QMP-ADDON-INTPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO MP-ADDON-INTPDTH-DATE.

           MOVE QMP-FROMCL TO MP-FROMCL.
           MOVE QMP-TOCL TO MP-TOCL.
           MOVE QMP-ALT-PREPAY-FG TO MP-ALT-PREPAY-FG.
 
      *-----------------------------------------------------------------
      *    LOAD QMP-REC FROM MP-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-MP-FIELDS.
           MOVE MP-BRNO TO QMP-BRNO.
           MOVE MP-ACCTNO TO QMP-ACCTNO.
           MOVE MP-SEQNO TO QMP-SEQNO.

           IF ( MP-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QMP-LTOUCH-DATE
           ELSE
              MOVE MP-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QMP-LTOUCH-DATE.

           MOVE MP-USERID TO QMP-USERID.

           IF ( MP-POSTDATE = ZEROES )
              MOVE "1900-01-01" TO QMP-POSTDATE
           ELSE
              MOVE MP-POSTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QMP-POSTDATE.

           MOVE MP-POSTTIME TO QMP-POSTTIME.

           IF ( MP-TRDATE = ZEROES )
              MOVE "1900-01-01" TO QMP-TRDATE
           ELSE
              MOVE MP-TRDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QMP-TRDATE.


           IF ( MP-PAYDATE = ZEROES )
              MOVE "1900-01-01" TO QMP-PAYDATE
           ELSE
              MOVE MP-PAYDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QMP-PAYDATE.

           MOVE MP-TRCD TO QMP-TRCD.
           MOVE MP-REFNO TO QMP-REFNO.
           MOVE MP-TRAMT TO QMP-TRAMT.
           MOVE MP-APOTH TO QMP-APOTH.
           MOVE MP-APOT2 TO QMP-APOT2.
           MOVE MP-APINT TO QMP-APINT.
           MOVE MP-APLC TO QMP-APLC.
           MOVE MP-APCUR TO QMP-APCUR.
           MOVE MP-OTHBAL TO QMP-OTHBAL.
           MOVE MP-OT2BAL TO QMP-OT2BAL.
           MOVE MP-INTBAL TO QMP-INTBAL.
           MOVE MP-LCBAL TO QMP-LCBAL.
           MOVE MP-CURBAL TO QMP-CURBAL.
           MOVE MP-INTDUE TO QMP-INTDUE.
           MOVE MP-DLPROC TO QMP-DLPROC.
           MOVE MP-EARNED(1) TO QMP-EARNED-1.
           MOVE MP-EARNED(2) TO QMP-EARNED-2.
           MOVE MP-EARNED(3) TO QMP-EARNED-3.
           MOVE MP-INTPAID TO QMP-INTPAID.
           MOVE MP-SSNO TO QMP-SSNO.

           IF ( MP-PDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QMP-PDTH-DATE
           ELSE
              MOVE MP-PDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QMP-PDTH-DATE.


           IF ( MP-LCPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QMP-LCPDTH-DATE
           ELSE
              MOVE MP-LCPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QMP-LCPDTH-DATE.


           IF ( MP-INTPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QMP-INTPDTH-DATE
           ELSE
              MOVE MP-INTPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QMP-INTPDTH-DATE.

           MOVE MP-TILLNO TO QMP-TILLNO.
           MOVE MP-APINTOWE TO QMP-APINTOWE.
           MOVE MP-DLPARVHELD-ADJ TO QMP-DLPARVHELD-ADJ.
           MOVE MP-LT-SEQNO TO QMP-LT-SEQNO.
           MOVE MP-IBPC TO QMP-IBPC.
           MOVE MP-MANUAL-RB TO QMP-MANUAL-RB.
           MOVE MP-INSTYPES TO QMP-INSTYPES.
           MOVE MP-REV TO QMP-REV.
           MOVE MP-REVTRCD TO QMP-REVTRCD.
           MOVE MP-REVSEQNO TO QMP-REVSEQNO.
           MOVE MP-STATUSFG TO QMP-STATUSFG.
           MOVE MP-NEWSTAT-RATE TO QMP-NEWSTAT-RATE.
           MOVE MP-LCPARTIALS TO QMP-LCPARTIALS.
           MOVE MP-ADDON-ORIG-REGPY TO QMP-ADDON-ORIG-REGPY.

           IF ( MP-NEW1ST-PYDATE = ZEROES )
              MOVE "1900-01-01" TO QMP-NEW1ST-PYDATE
           ELSE
              MOVE MP-NEW1ST-PYDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QMP-NEW1ST-PYDATE.


           IF ( MP-DEF-DUEDATE = ZEROES )
              MOVE "1900-01-01" TO QMP-DEF-DUEDATE
           ELSE
              MOVE MP-DEF-DUEDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QMP-DEF-DUEDATE.

           MOVE MP-LCPAID TO QMP-LCPAID.

           IF ( MP-OLD1ST-PYDATE = ZEROES )
              MOVE "1900-01-01" TO QMP-OLD1ST-PYDATE
           ELSE
              MOVE MP-OLD1ST-PYDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QMP-OLD1ST-PYDATE.

           MOVE MP-DEF-TERM TO QMP-DEF-TERM.
           MOVE MP-ADDON-LAST-PYAMT TO QMP-ADDON-LAST-PYAMT.
           MOVE MP-DLRVCHGS-ADJ(1) TO QMP-DLRVCHGS-ADJ-1.
           MOVE MP-DLRVCHGS-ADJ(2) TO QMP-DLRVCHGS-ADJ-2.
           MOVE MP-DLRVCHGS-ADJ(3) TO QMP-DLRVCHGS-ADJ-3.
           MOVE MP-DLPARVPAID-ADJ TO QMP-DLPARVPAID-ADJ.
           MOVE MP-DLNO TO QMP-DLNO.
           MOVE MP-PREPAY-PENALTY TO QMP-PREPAY-PENALTY.
           MOVE MP-PRORATED-INS-REB TO QMP-PRORATED-INS-REB.
           MOVE MP-POSTING-BR TO QMP-POSTING-BR.
           MOVE MP-NOLCHG TO QMP-NOLCHG.
           MOVE MP-COLLECTOR TO QMP-COLLECTOR.
           MOVE MP-EXTRA-PRIN-LFP TO QMP-EXTRA-PRIN-LFP.
           MOVE MP-REMAIN-INT-SINCE-LFP TO QMP-REMAIN-INT-SINCE-LFP.
           MOVE MP-REMAIN-PRIN-SINCE-LFP TO QMP-REMAIN-PRIN-SINCE-LFP.
           MOVE MP-APPLIED-TO-ESCROW TO QMP-APPLIED-TO-ESCROW.
           MOVE MP-ESCROW-PREPAID TO QMP-ESCROW-PREPAID.
           MOVE MP-PRIOR-POOLID TO QMP-PRIOR-POOLID.

           IF ( MP-ADDON-INTPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QMP-ADDON-INTPDTH-DATE
           ELSE
              MOVE MP-ADDON-INTPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QMP-ADDON-INTPDTH-DATE.

           MOVE MP-FROMCL TO QMP-FROMCL.
           MOVE MP-TOCL TO QMP-TOCL.
           MOVE MP-ALT-PREPAY-FG TO QMP-ALT-PREPAY-FG.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-MP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( MP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( MP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO MP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO MP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO MP-ALL-DATA
              MOVE MP-ALL-PATH             TO MP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( MP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO MP-ALL-BASE
              MOVE FILPATH-MACHINE         TO MP-ALL-MACHINE
              MOVE FILPATH-DATA            TO MP-ALL-DATA
              MOVE MP-ALL-PATH             TO MP-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-MPFILE NOT = "S" )
              MOVE MP-PATH-BEG             TO MP-SELECT-PATH-BEG
              MOVE MP-PATH-FILE            TO MP-SELECT-PATH-FILE
           ELSE
              MOVE MP-PATH-SQL             TO MP-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-MP1-FILE.
           PERFORM LOAD-MP1-FILE.
           PERFORM OPEN-IT.
           MOVE MP-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-MP1-FILE.
           PERFORM START-IT.
           MOVE MP-PATH-OWNBR  TO MP-BRNO.
           MOVE MP-SELECT-PATH TO E-FILE.
           MOVE MP1-KEY        TO E-KEYX.

           IF ( MP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-MP1-FILE.

           IF ( QMP1-WEND-SEQNO = ZEROES    ) OR
              ( QMP1-WEND-SEQNO NOT NUMERIC )
              MOVE MP-PATH-OWNBR        TO QMP1-WBEG-BRNO
              MOVE MP-PATH-OWNBR        TO QMP1-WEND-BRNO
              MOVE MP-ACCTNO            TO QMP1-WBEG-ACCTNO
              MOVE ALL "9"              TO QMP1-WEND-ACCTNO
              MOVE MP-SEQNO             TO QMP1-WBEG-SEQNO
              MOVE ALL "9"              TO QMP1-WEND-SEQNO.

           EXEC SQL
            DECLARE MP1_CURSOR CURSOR FOR
            SELECT MPFILE.MP_BRNO,
                   MPFILE.MP_ACCTNO,
                   MPFILE.MP_SEQNO,
                   ISNULL(CAST(MPFILE.MP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(MPFILE.MP_USERID,''),
                   ISNULL(CAST(MPFILE.MP_POSTDATE AS VARCHAR(10)),'0'),
                   MPFILE.MP_POSTTIME,
                   ISNULL(CAST(MPFILE.MP_TRDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(MPFILE.MP_PAYDATE AS VARCHAR(10)),'0'),
                   ISNULL(MPFILE.MP_TRCD,''),
                   ISNULL(MPFILE.MP_REFNO,''),
                   MPFILE.MP_TRAMT,
                   MPFILE.MP_APOTH,
                   MPFILE.MP_APOT2,
                   MPFILE.MP_APINT,
                   MPFILE.MP_APLC,
                   MPFILE.MP_APCUR,
                   MPFILE.MP_OTHBAL,
                   MPFILE.MP_OT2BAL,
                   MPFILE.MP_INTBAL,
                   MPFILE.MP_LCBAL,
                   MPFILE.MP_CURBAL,
                   MPFILE.MP_INTDUE,
                   MPFILE.MP_DLPROC,
                   MPFILE.MP_EARNED_1,
                   MPFILE.MP_EARNED_2,
                   MPFILE.MP_EARNED_3,
                   MPFILE.MP_INTPAID,
                   MPFILE.MP_SSNO,
                   ISNULL(CAST(MPFILE.MP_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(MPFILE.MP_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(MPFILE.MP_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   MPFILE.MP_TILLNO,
                   MPFILE.MP_APINTOWE,
                   MPFILE.MP_DLPARVHELD_ADJ,
                   MPFILE.MP_LT_SEQNO,
                   ISNULL(MPFILE.MP_IBPC,''),
                   ISNULL(MPFILE.MP_MANUAL_RB,''),
                   ISNULL(MPFILE.MP_INSTYPES,''),
                   ISNULL(MPFILE.MP_REV,''),
                   ISNULL(MPFILE.MP_REVTRCD,''),
                   MPFILE.MP_REVSEQNO,
                   ISNULL(MPFILE.MP_STATUSFG,''),
                   MPFILE.MP_NEWSTAT_RATE,
                   MPFILE.MP_LCPARTIALS,
                   MPFILE.MP_ADDON_ORIG_REGPY,
                   ISNULL(CAST(MPFILE.MP_NEW1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(MPFILE.MP_DEF_DUEDATE
                                                  AS VARCHAR(10)),'0'),
                   MPFILE.MP_LCPAID,
                   ISNULL(CAST(MPFILE.MP_OLD1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   MPFILE.MP_DEF_TERM,
                   MPFILE.MP_ADDON_LAST_PYAMT,
                   MPFILE.MP_DLRVCHGS_ADJ_1,
                   MPFILE.MP_DLRVCHGS_ADJ_2,
                   MPFILE.MP_DLRVCHGS_ADJ_3,
                   MPFILE.MP_DLPARVPAID_ADJ,
                   MPFILE.MP_DLNO,
                   MPFILE.MP_PREPAY_PENALTY,
                   MPFILE.MP_PRORATED_INS_REB,
                   MPFILE.MP_POSTING_BR,
                   MPFILE.MP_NOLCHG,
                   ISNULL(MPFILE.MP_COLLECTOR,''),
                   MPFILE.MP_EXTRA_PRIN_LFP,
                   MPFILE.MP_REMAIN_INT_SINCE_LFP,
                   MPFILE.MP_REMAIN_PRIN_SINCE_LFP,
                   MPFILE.MP_APPLIED_TO_ESCROW,
                   MPFILE.MP_ESCROW_PREPAID,
                   MPFILE.MP_PRIOR_POOLID,
                   ISNULL(CAST(MPFILE.MP_ADDON_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   MPFILE.MP_FROMCL,
                   MPFILE.MP_TOCL,
                   ISNULL(MPFILE.MP_ALT_PREPAY_FG,'')
            FROM DBO.MPFILE
            WHERE MPFILE.MP_BRNO >= :QMP1-WBEG-BRNO
              AND MPFILE.MP_BRNO <= :QMP1-WEND-BRNO
              AND MPFILE.MP_ACCTNO >= :QMP1-WBEG-ACCTNO
              AND MPFILE.MP_ACCTNO <= :QMP1-WEND-ACCTNO
              AND MPFILE.MP_SEQNO >= :QMP1-WBEG-SEQNO
              AND MPFILE.MP_SEQNO <= :QMP1-WEND-SEQNO
            ORDER BY MPFILE.MP_BRNO,
                     MPFILE.MP_ACCTNO,
                     MPFILE.MP_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN MP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QMP1-WBEG-BRNO
                          QMP1-WEND-BRNO
                          QMP1-WBEG-ACCTNO
                          QMP1-WEND-ACCTNO
                          QMP1-WBEG-SEQNO
                          QMP1-WEND-SEQNO.

           MOVE STAT---GOOD TO MP1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-MP1-FILE.
           PERFORM READ-IT.
           MOVE MP-PATH-OWNBR  TO MP-BRNO. 
           MOVE MP-SELECT-PATH TO E-FILE.
           MOVE MP1-KEY        TO E-KEYX.

           MOVE MP-BRNO   TO QMP-BRNO.
           MOVE MP-ACCTNO TO QMP-ACCTNO.
           MOVE MP-SEQNO  TO QMP-SEQNO.

           EXEC SQL
            SELECT MPFILE.MP_BRNO,
                   MPFILE.MP_ACCTNO,
                   MPFILE.MP_SEQNO,
                   ISNULL(CAST(MPFILE.MP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(MPFILE.MP_USERID,''),
                   ISNULL(CAST(MPFILE.MP_POSTDATE AS VARCHAR(10)),'0'),
                   MPFILE.MP_POSTTIME,
                   ISNULL(CAST(MPFILE.MP_TRDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(MPFILE.MP_PAYDATE AS VARCHAR(10)),'0'),
                   ISNULL(MPFILE.MP_TRCD,''),
                   ISNULL(MPFILE.MP_REFNO,''),
                   MPFILE.MP_TRAMT,
                   MPFILE.MP_APOTH,
                   MPFILE.MP_APOT2,
                   MPFILE.MP_APINT,
                   MPFILE.MP_APLC,
                   MPFILE.MP_APCUR,
                   MPFILE.MP_OTHBAL,
                   MPFILE.MP_OT2BAL,
                   MPFILE.MP_INTBAL,
                   MPFILE.MP_LCBAL,
                   MPFILE.MP_CURBAL,
                   MPFILE.MP_INTDUE,
                   MPFILE.MP_DLPROC,
                   MPFILE.MP_EARNED_1,
                   MPFILE.MP_EARNED_2,
                   MPFILE.MP_EARNED_3,
                   MPFILE.MP_INTPAID,
                   MPFILE.MP_SSNO,
                   ISNULL(CAST(MPFILE.MP_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(MPFILE.MP_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(MPFILE.MP_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   MPFILE.MP_TILLNO,
                   MPFILE.MP_APINTOWE,
                   MPFILE.MP_DLPARVHELD_ADJ,
                   MPFILE.MP_LT_SEQNO,
                   ISNULL(MPFILE.MP_IBPC,''),
                   ISNULL(MPFILE.MP_MANUAL_RB,''),
                   ISNULL(MPFILE.MP_INSTYPES,''),
                   ISNULL(MPFILE.MP_REV,''),
                   ISNULL(MPFILE.MP_REVTRCD,''),
                   MPFILE.MP_REVSEQNO,
                   ISNULL(MPFILE.MP_STATUSFG,''),
                   MPFILE.MP_NEWSTAT_RATE,
                   MPFILE.MP_LCPARTIALS,
                   MPFILE.MP_ADDON_ORIG_REGPY,
                   ISNULL(CAST(MPFILE.MP_NEW1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(MPFILE.MP_DEF_DUEDATE
                                                  AS VARCHAR(10)),'0'),
                   MPFILE.MP_LCPAID,
                   ISNULL(CAST(MPFILE.MP_OLD1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   MPFILE.MP_DEF_TERM,
                   MPFILE.MP_ADDON_LAST_PYAMT,
                   MPFILE.MP_DLRVCHGS_ADJ_1,
                   MPFILE.MP_DLRVCHGS_ADJ_2,
                   MPFILE.MP_DLRVCHGS_ADJ_3,
                   MPFILE.MP_DLPARVPAID_ADJ,
                   MPFILE.MP_DLNO,
                   MPFILE.MP_PREPAY_PENALTY,
                   MPFILE.MP_PRORATED_INS_REB,
                   MPFILE.MP_POSTING_BR,
                   MPFILE.MP_NOLCHG,
                   ISNULL(MPFILE.MP_COLLECTOR,''),
                   MPFILE.MP_EXTRA_PRIN_LFP,
                   MPFILE.MP_REMAIN_INT_SINCE_LFP,
                   MPFILE.MP_REMAIN_PRIN_SINCE_LFP,
                   MPFILE.MP_APPLIED_TO_ESCROW,
                   MPFILE.MP_ESCROW_PREPAID,
                   MPFILE.MP_PRIOR_POOLID,
                   ISNULL(CAST(MPFILE.MP_ADDON_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   MPFILE.MP_FROMCL,
                   MPFILE.MP_TOCL,
                   ISNULL(MPFILE.MP_ALT_PREPAY_FG,'')
            INTO :QMP-REC
            FROM DBO.MPFILE
            WHERE MPFILE.MP_BRNO = :QMP-BRNO
              AND MPFILE.MP_ACCTNO = :QMP-ACCTNO
              AND MPFILE.MP_SEQNO = :QMP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-MP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-MP-FIELDS.

      *-----------------------------------------------------------------
       READ-MP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE MP-PATH-OWNBR  TO MP-BRNO.
           MOVE MP-SELECT-PATH TO E-FILE.
           MOVE MP1-KEY        TO E-KEYX.
           INITIALIZE QMP-REC.

           IF ( MP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH MP1_CURSOR INTO :QMP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-MP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-MP-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-MP1-FILE.

           IF ( MP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE MP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO MP1-CURSOR-STAT.

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
              DESTROY IOMPRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
