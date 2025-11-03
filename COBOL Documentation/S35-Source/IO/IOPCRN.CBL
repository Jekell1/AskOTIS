       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOPCRN.
       AUTHOR.        BAH
       DATE-WRITTEN.  04-15-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOPCRN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : PCFILE 
      * TABLE DESC    : SCRATCH PAD FILE FOR COLLATERAL AND AUTO INSURANCE
      * OUTPUT FILE   : XPC/IOPCRN.PRE
      * USED FOR      : LIBLP/LPWSPC.CPY  --- QPC-REC
      *                 LIBLP/LPPC1R.CPY  --- READ
      *                 LIBLP/LPPC1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0415 ***NEW***
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./IO/IOPCRN.CBL S34 05/11/21-10:06:06 >".

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
       COPY "LIBLP/LP01PC.CPY".
       COPY "LIBLP/LPWSPC.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QPC1-WHERE-BEG-KEY.
           03  QPC1-WBEG-ID               PIC X.
           03  QPC1-WBEG-BRANCH           PIC 9(04).
           03  QPC1-WBEG-SSNO             PIC 9(10).
 
       01  QPC1-WHERE-END-KEY.
           03  QPC1-WEND-ID               PIC X.
           03  QPC1-WEND-BRANCH           PIC 9(04).
           03  QPC1-WEND-SSNO             PIC 9(10).

       01  QPC-REC.
           03  QPC-ID               PIC X.
           03  QPC-BRANCH           PIC 9(4).
           03  QPC-SOC-SEC          PIC 9(10).
           03  QPC-LTC-BRNO          PIC 9(4).
           03  QPC-LTC-ACCTNO        PIC 9(8).
           03  QPC-LTC-SEQNO         PIC 999.
           03  QPC-LTC-LTOUCH-DATE   PIC X(10).
           03  QPC-LTC-DESC-1        PIC X(15).
           03  QPC-LTC-MAKE-1        PIC X(15).
           03  QPC-LTC-MODEL-1       PIC X(15).
           03  QPC-LTC-YEAR-1        PIC X(4).
           03  QPC-LTC-STYLE-1       PIC X(8).
           03  QPC-LTC-CLASS-1       PIC X(8).
           03  QPC-LTC-VIN-1         PIC X(20).
           03  QPC-LTC-TITLE-NO-1    PIC X(20).
           03  QPC-LTC-MILEAGE-1     PIC S9(7).
           03  QPC-LTC-VALUE-1       PIC S9(7)V99.
           03  QPC-LTC-SELLPR-1      PIC S9(7)V99.
           03  QPC-LTC-MISC-AMT-1    PIC S9(7)V99.
           03  QPC-LTC-SENT-FLAG-1   PIC X.
           03  QPC-LTC-VIN-CHG-DATE-1 PIC X(10).
           03  QPC-LTC-DESC-2         PIC X(15).
           03  QPC-LTC-MAKE-2         PIC X(15).
           03  QPC-LTC-MODEL-2        PIC X(15).
           03  QPC-LTC-YEAR-2         PIC X(4).
           03  QPC-LTC-STYLE-2        PIC X(8).
           03  QPC-LTC-CLASS-2        PIC X(8).
           03  QPC-LTC-VIN-2          PIC X(20).
           03  QPC-LTC-TITLE-NO-2     PIC X(20).
           03  QPC-LTC-MILEAGE-2      PIC S9(7).
           03  QPC-LTC-VALUE-2        PIC S9(7)V99.
           03  QPC-LTC-SELLPR-2       PIC S9(7)V99.
           03  QPC-LTC-MISC-AMT-2     PIC S9(7)V99.
           03  QPC-LTC-SENT-FLAG-2    PIC X.
           03  QPC-LTC-VIN-CHG-DATE-2 PIC X(10).
           03  QPC-LTC-DESC-3         PIC X(15).
           03  QPC-LTC-MAKE-3         PIC X(15).
           03  QPC-LTC-MODEL-3        PIC X(15).
           03  QPC-LTC-YEAR-3         PIC X(4).
           03  QPC-LTC-STYLE-3        PIC X(8).
           03  QPC-LTC-CLASS-3        PIC X(8).
           03  QPC-LTC-VIN-3          PIC X(20).
           03  QPC-LTC-TITLE-NO-3     PIC X(20).
           03  QPC-LTC-MILEAGE-3      PIC S9(7).
           03  QPC-LTC-VALUE-3        PIC S9(7)V99.
           03  QPC-LTC-SELLPR-3       PIC S9(7)V99.
           03  QPC-LTC-MISC-AMT-3     PIC S9(7)V99.
           03  QPC-LTC-SENT-FLAG-3    PIC X.
           03  QPC-LTC-VIN-CHG-DATE-3 PIC X(10).
           03  QPC-LTC-HHGS-1         PIC X(80).
           03  QPC-LTC-HHGS-2         PIC X(80).
           03  QPC-LTC-HHGS-3         PIC X(80).
           03  QPC-LTA-BRNO           PIC 9(4).
           03  QPC-LTA-ACCTNO         PIC 9(8).
           03  QPC-LTA-SEQNO          PIC 999.
           03  QPC-LTA-LTOUCH-DATE    PIC X(10).
           03  QPC-LTA-DLR-REQ        PIC X.
           03  QPC-LTA-INS-TYPE       PIC X.
           03  QPC-LTA-INS-CO         PIC X(25).
           03  QPC-LTA-POLICY-NO      PIC X(25).
           03  QPC-LTA-AGENCY         PIC X(25).
           03  QPC-LTA-AGENT          PIC X(25).
           03  QPC-LTA-ADR1           PIC X(25).
           03  QPC-LTA-ADR2           PIC X(25).
           03  QPC-LTA-CITY           PIC X(15).
           03  QPC-LTA-STATE          PIC XX.
           03  QPC-LTA-ZIP            PIC X(10).
           03  QPC-LTA-PHONE          PIC 9(10).
           03  QPC-LTA-EFF-DATE       PIC X(10).
           03  QPC-LTA-EXP-DATE       PIC X(10).
           03  QPC-LTA-CAN-DATE       PIC X(10).
           03  QPC-LTA-CONF-DATE      PIC X(10).
           03  QPC-LTA-LAST-FORM      PIC 9(4).
           03  QPC-LTA-COV-TYPE       PIC X.
           03  QPC-LTA-BIL-COV        PIC S9(7)V99.
           03  QPC-LTA-PDL-COV        PIC S9(7)V99.
           03  QPC-LTA-PIP-COV        PIC S9(7)V99.
           03  QPC-LTA-UMB-COV        PIC S9(7)V99.
           03  QPC-LTA-COL-COV        PIC S9(7)V99.
           03  QPC-LTA-LOSS-PAYEE     PIC X(25).
           03  QPC-LTA-COM-COV        PIC S9(7)V99.
           03  QPC-LTA-TRACK-ONLY     PIC X.
           03  QPC-LTA-INSURANCE      PIC X.
           03  QPC-LTA-TRACK-MONTHS   PIC X(12).
           03  QPC-LTA-INS-MONTHS     PIC X(12).
           03  QPC-LTA-CPP-MONTHS     PIC X(12).
           03  QPC-LOANDATE             PIC X(10).
           03  QPC-ORGTERM              PIC 9(3).
           03  QPC-REGPYAMT             PIC 9(6)V99.
           03  QPC2-ID                  PIC X.
           03  QPC2-BRANCH              PIC 9(4).
           03  QPC2-BWNAME              PIC X(33).
           03  QPC-LTOUCH-DATE          PIC X(10).

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
           03  BEG-ID                  PIC X.
           03  END-ID                  PIC X.
           03  BEG-SSNO                PIC 9(09).
           03  END-SSNO                PIC 9(09).
           03  BEG-BWNAME              PIC X(33).
           03  END-BWNAME              PIC X(33).

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
           03  FILLER PIC X(30) VALUE "BRNO  ID    SSNO         BWNAM".
           03  FILLER PIC X(30) VALUE "E                 MAKE 1".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-PC-BRANCH                  PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-PC-ID                      PIC X.
           03  FILLER                         PIC X(03).
           03  D01-PC-SOC-SEC                    PIC 999/99/9999.
           03  FILLER                         PIC X(03).
           03  D01-PC-BWNAME                  PIC X(20).
           03  FILLER                         PIC X(03).
           03  D01-PC-LTC-MAKE-1              PIC X(10).

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
       COPY "LIBIO/IOPCRN_DEF.CPY".
       COPY "LIBIO/IOPCRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOPCRN_SCN.CPY".

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
           MOVE "IOPCRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOPCRN-QUEUE-POS  TO Q-POS.
           MOVE IOPCRN-COPIES-POS TO C-POS.
           MOVE IOPCRN-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-NAME.

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
               IF VDUNUM0 NOT = 9
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-BEG-ID.

           MOVE "E  A010BEG-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-ID.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-ID.

      *-----------------------------------------------------------------
       ENTER-END-ID.

           MOVE "E  A010END-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-ID.

           IF ( BEG-ID > END-ID )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ID.

           GO TO ENTER-BEG-SSNO.

      *-----------------------------------------------------------------
       ENTER-ALL-ID.

           MOVE "S  A010BEG-ID." TO VDU.
           MOVE SPACES           TO VDUBUF  BEG-ID.
           PERFORM SCREENIO.

           MOVE "S  A010END-ID." TO VDU.
           MOVE ALL "Z"              TO VDUBUF  END-ID.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SSNO.

           MOVE "ERNX090BEG-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SSNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SSNO.

      *-----------------------------------------------------------------
       ENTER-END-SSNO.

           MOVE "ERNX090END-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SSNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SSNO.

           IF ( BEG-SSNO > END-SSNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SSNO.

           GO TO ENTER-BEG-NAME.

      *-----------------------------------------------------------------
       ENTER-ALL-SSNO.

           MOVE "SRNX090BEG-SSNO." TO VDU.
           MOVE ZEROES             TO VDU-SSNO-EDIT BEG-SSNO.
           PERFORM SCREENIO.

           MOVE "SRNX090END-SSNO." TO VDU.
           MOVE 999999999          TO VDU-SSNO-EDIT END-SSNO.
           PERFORM SCREENIO.

      *======================================================
       ENTER-BEG-NAME.

           MOVE "E  A330BEG-NAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SSNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-NAME.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-NAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-BWNAME.

      *-----------------------------------------------------------------
       ENTER-END-NAME.

           MOVE "E  A330END-NAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-NAME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-NAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-BWNAME.

           IF ( BEG-BWNAME > END-BWNAME )
              MOVE "ERROR: RANGE IS INVALNAME" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-NAME.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-NAME.

           MOVE "S  A330BEG-NAME." TO VDU.
           MOVE SPACES             TO VDUBUF  BEG-BWNAME.
           PERFORM SCREENIO.

           MOVE "S  A330END-NAME." TO VDU.
           MOVE ALL "Z"              TO VDUBUF  END-BWNAME.
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

           MOVE "S  A010BEG-ID."       TO VDU.
           MOVE BEG-ID                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010END-ID."       TO VDU.
           MOVE END-ID                 TO VDUBUF.
           PERFORM SCREENIO.

            MOVE "SRNX090BEG-SSNO."        TO VDU.
           MOVE BEG-SSNO                  TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

            MOVE "SRNX090END-SSNO."        TO VDU.
           MOVE END-SSNO                  TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A330BEG-NAME."       TO VDU.
           MOVE BEG-BWNAME                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A330END-NAME."       TO VDU.
           MOVE END-BWNAME                 TO VDUBUF.
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

           MOVE "B"                        TO PC-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QPC1-WBEG-BRANCH.
           MOVE BR-NO     TO QPC1-WEND-BRANCH.

           MOVE BEG-ID  TO QPC1-WBEG-ID.
           MOVE END-ID  TO QPC1-WEND-ID.
           MOVE BEG-SSNO  TO QPC1-WBEG-SSNO.
           MOVE END-SSNO  TO QPC1-WEND-SSNO.


      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-PC1-FILE
              PERFORM START-PC1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-PC1-FILE-NEXT.
           IF ( IO-BAD ) 
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( PC-ID < BEG-ID )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( PC-ID > END-ID )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF PC-BRANCH < BEG-BRANCH OR > END-BRANCH
                    GO TO PROCESS-BRANCH-NEXT
                 ELSE
                 IF PC-SOC-SEC < BEG-SSNO OR > END-SSNO
                    GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.


           MOVE PC-BRANCH               TO D01-PC-BRANCH.
           MOVE PC-ID                   TO D01-PC-ID.
           MOVE PC-SOC-SEC                 TO D01-PC-SOC-SEC.
           MOVE PC2-BWNAME              TO D01-PC-BWNAME.
           MOVE PC-LTC-MAKE-1           TO D01-PC-LTC-MAKE-1.
           PERFORM WRITE-DETAIL-LINE.

           IF PROCESS-KEY = 9 
              GO TO PROCESS-BRANCH-MODULE-END.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-PC1-FILE.

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

           MOVE PC1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING ID                  : "    TO R01-PROMPT.
           MOVE BEG-ID                                TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ID                  : "    TO R01-PROMPT.
           MOVE END-ID                                TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SSNO                : "    TO R01-PROMPT.
           MOVE BEG-SSNO                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SSNO                : "    TO R01-PROMPT.
           MOVE END-SSNO                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE "BEGINNING BWNAME                  : "    TO R01-PROMPT.
           MOVE BEG-BWNAME                                TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    BWNAME                  : "    TO R01-PROMPT.
           MOVE END-BWNAME                                TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - PCFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOPCRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOPCRN-SYSDATE.
           DISPLAY IOPCRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOPCRN-FORM-FIELDS TO WR-BUF.
           MOVE IOPCRN-FORM-LIST TO WR-LIST.
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

           MOVE IOPCRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOPCRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOPCRN_EVA.CPY".
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
      *    LOAD PC-REC FROM QPC-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-PC-FIELDS.
           MOVE QPC-ID TO PC-ID.
           MOVE QPC-BRANCH TO PC-BRANCH.
           MOVE QPC-SOC-SEC TO PC-SOC-SEC.
           MOVE QPC-LTC-BRNO TO PC-LTC-BRNO.
           MOVE QPC-LTC-ACCTNO TO PC-LTC-ACCTNO.
           MOVE QPC-LTC-SEQNO TO PC-LTC-SEQNO.

           IF ( QPC-LTC-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO PC-LTC-LTOUCH-DATE
           ELSE
              MOVE QPC-LTC-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PC-LTC-LTOUCH-DATE.

           MOVE QPC-LTC-DESC-1 TO PC-LTC-DESC(1).
           MOVE QPC-LTC-MAKE-1 TO PC-LTC-MAKE(1).
           MOVE QPC-LTC-MODEL-1 TO PC-LTC-MODEL(1).
           MOVE QPC-LTC-YEAR-1 TO PC-LTC-YEAR(1).
           MOVE QPC-LTC-STYLE-1 TO PC-LTC-STYLE(1).
           MOVE QPC-LTC-CLASS-1 TO PC-LTC-CLASS(1).
           MOVE QPC-LTC-VIN-1 TO PC-LTC-VIN(1).
           MOVE QPC-LTC-TITLE-NO-1 TO PC-LTC-TITLE-NO(1).
           MOVE QPC-LTC-MILEAGE-1 TO PC-LTC-MILEAGE(1).
           MOVE QPC-LTC-VALUE-1 TO PC-LTC-VALUE(1).
           MOVE QPC-LTC-SELLPR-1 TO PC-LTC-SELLPR(1).
           MOVE QPC-LTC-MISC-AMT-1 TO PC-LTC-MISC-AMT(1).
           MOVE QPC-LTC-SENT-FLAG-1 TO PC-LTC-SENT-FLAG(1).

           IF ( QPC-LTC-VIN-CHG-DATE-1 = "1900-01-01" )
              MOVE ZEROES TO PC-LTC-VIN-CHG-DATE(1)
           ELSE
              MOVE QPC-LTC-VIN-CHG-DATE-1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PC-LTC-VIN-CHG-DATE(1).

           MOVE QPC-LTC-DESC-2 TO PC-LTC-DESC(2).
           MOVE QPC-LTC-MAKE-2 TO PC-LTC-MAKE(2).
           MOVE QPC-LTC-MODEL-2 TO PC-LTC-MODEL(2).
           MOVE QPC-LTC-YEAR-2 TO PC-LTC-YEAR(2).
           MOVE QPC-LTC-STYLE-2 TO PC-LTC-STYLE(2).
           MOVE QPC-LTC-CLASS-2 TO PC-LTC-CLASS(2).
           MOVE QPC-LTC-VIN-2 TO PC-LTC-VIN(2).
           MOVE QPC-LTC-TITLE-NO-2 TO PC-LTC-TITLE-NO(2).
           MOVE QPC-LTC-MILEAGE-2 TO PC-LTC-MILEAGE(2).
           MOVE QPC-LTC-VALUE-2 TO PC-LTC-VALUE(2).
           MOVE QPC-LTC-SELLPR-2 TO PC-LTC-SELLPR(2).
           MOVE QPC-LTC-MISC-AMT-2 TO PC-LTC-MISC-AMT(2).
           MOVE QPC-LTC-SENT-FLAG-2 TO PC-LTC-SENT-FLAG(2).

           IF ( QPC-LTC-VIN-CHG-DATE-2 = "1900-01-01" )
              MOVE ZEROES TO PC-LTC-VIN-CHG-DATE(2)
           ELSE
              MOVE QPC-LTC-VIN-CHG-DATE-2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PC-LTC-VIN-CHG-DATE(2).

           MOVE QPC-LTC-DESC-3 TO PC-LTC-DESC(3).
           MOVE QPC-LTC-MAKE-3 TO PC-LTC-MAKE(3).
           MOVE QPC-LTC-MODEL-3 TO PC-LTC-MODEL(3).
           MOVE QPC-LTC-YEAR-3 TO PC-LTC-YEAR(3).
           MOVE QPC-LTC-STYLE-3 TO PC-LTC-STYLE(3).
           MOVE QPC-LTC-CLASS-3 TO PC-LTC-CLASS(3).
           MOVE QPC-LTC-VIN-3 TO PC-LTC-VIN(3).
           MOVE QPC-LTC-TITLE-NO-3 TO PC-LTC-TITLE-NO(3).
           MOVE QPC-LTC-MILEAGE-3 TO PC-LTC-MILEAGE(3).
           MOVE QPC-LTC-VALUE-3 TO PC-LTC-VALUE(3).
           MOVE QPC-LTC-SELLPR-3 TO PC-LTC-SELLPR(3).
           MOVE QPC-LTC-MISC-AMT-3 TO PC-LTC-MISC-AMT(3).
           MOVE QPC-LTC-SENT-FLAG-3 TO PC-LTC-SENT-FLAG(3).

           IF ( QPC-LTC-VIN-CHG-DATE-3 = "1900-01-01" )
              MOVE ZEROES TO PC-LTC-VIN-CHG-DATE(3)
           ELSE
              MOVE QPC-LTC-VIN-CHG-DATE-3 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PC-LTC-VIN-CHG-DATE(3).

           MOVE QPC-LTC-HHGS-1 TO PC-LTC-HHGS(1).
           MOVE QPC-LTC-HHGS-2 TO PC-LTC-HHGS(2).
           MOVE QPC-LTC-HHGS-3 TO PC-LTC-HHGS(3).
           MOVE QPC-LTA-BRNO TO PC-LTA-BRNO.
           MOVE QPC-LTA-ACCTNO TO PC-LTA-ACCTNO.
           MOVE QPC-LTA-SEQNO TO PC-LTA-SEQNO.

           IF ( QPC-LTA-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO PC-LTA-LTOUCH-DATE
           ELSE
              MOVE QPC-LTA-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PC-LTA-LTOUCH-DATE.

           MOVE QPC-LTA-DLR-REQ TO PC-LTA-DLR-REQ.
           MOVE QPC-LTA-INS-TYPE TO PC-LTA-INS-TYPE.
           MOVE QPC-LTA-INS-CO TO PC-LTA-INS-CO.
           MOVE QPC-LTA-POLICY-NO TO PC-LTA-POLICY-NO.
           MOVE QPC-LTA-AGENCY TO PC-LTA-AGENCY.
           MOVE QPC-LTA-AGENT TO PC-LTA-AGENT.
           MOVE QPC-LTA-ADR1 TO PC-LTA-ADR1.
           MOVE QPC-LTA-ADR2 TO PC-LTA-ADR2.
           MOVE QPC-LTA-CITY TO PC-LTA-CITY.
           MOVE QPC-LTA-STATE TO PC-LTA-STATE.
           MOVE QPC-LTA-ZIP TO PC-LTA-ZIP.
           MOVE QPC-LTA-PHONE TO PC-LTA-PHONE.

           IF ( QPC-LTA-EFF-DATE = "1900-01-01" )
              MOVE ZEROES TO PC-LTA-EFF-DATE
           ELSE
              MOVE QPC-LTA-EFF-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PC-LTA-EFF-DATE.


           IF ( QPC-LTA-EXP-DATE = "1900-01-01" )
              MOVE ZEROES TO PC-LTA-EXP-DATE
           ELSE
              MOVE QPC-LTA-EXP-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PC-LTA-EXP-DATE.


           IF ( QPC-LTA-CAN-DATE = "1900-01-01" )
              MOVE ZEROES TO PC-LTA-CAN-DATE
           ELSE
              MOVE QPC-LTA-CAN-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PC-LTA-CAN-DATE.


           IF ( QPC-LTA-CONF-DATE = "1900-01-01" )
              MOVE ZEROES TO PC-LTA-CONF-DATE
           ELSE
              MOVE QPC-LTA-CONF-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PC-LTA-CONF-DATE.

           MOVE QPC-LTA-LAST-FORM TO PC-LTA-LAST-FORM.
           MOVE QPC-LTA-COV-TYPE TO PC-LTA-COV-TYPE.
           MOVE QPC-LTA-BIL-COV TO PC-LTA-BIL-COV.
           MOVE QPC-LTA-PDL-COV TO PC-LTA-PDL-COV.
           MOVE QPC-LTA-PIP-COV TO PC-LTA-PIP-COV.
           MOVE QPC-LTA-UMB-COV TO PC-LTA-UMB-COV.
           MOVE QPC-LTA-COL-COV TO PC-LTA-COL-COV.
           MOVE QPC-LTA-LOSS-PAYEE TO PC-LTA-LOSS-PAYEE.
           MOVE QPC-LTA-COM-COV TO PC-LTA-COM-COV.
           MOVE QPC-LTA-TRACK-ONLY TO PC-LTA-TRACK-ONLY.
           MOVE QPC-LTA-INSURANCE TO PC-LTA-INSURANCE.
           MOVE QPC-LTA-TRACK-MONTHS TO PC-LTA-TRACK-MONTHS.
           MOVE QPC-LTA-INS-MONTHS TO PC-LTA-INS-MONTHS.
           MOVE QPC-LTA-CPP-MONTHS TO PC-LTA-CPP-MONTHS.

           IF ( QPC-LOANDATE = "1900-01-01" )
              MOVE ZEROES TO PC-LOANDATE
           ELSE
              MOVE QPC-LOANDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PC-LOANDATE.

           MOVE QPC-ORGTERM TO PC-ORGTERM.
           MOVE QPC-REGPYAMT TO PC-REGPYAMT.
           MOVE QPC2-ID TO PC2-ID.
           MOVE QPC2-BRANCH TO PC2-BRANCH.
           MOVE QPC2-BWNAME TO PC2-BWNAME.

           IF ( QPC-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO PC-LTOUCH-DATE
           ELSE
              MOVE QPC-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PC-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QPC-REC FROM PC-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-PC-FIELDS.
           MOVE PC-ID TO QPC-ID.
           MOVE PC-BRANCH TO QPC-BRANCH.
           MOVE PC-SOC-SEC TO QPC-SOC-SEC.
           MOVE PC-LTC-BRNO TO QPC-LTC-BRNO.
           MOVE PC-LTC-ACCTNO TO QPC-LTC-ACCTNO.
           MOVE PC-LTC-SEQNO TO QPC-LTC-SEQNO.

           IF ( PC-LTC-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QPC-LTC-LTOUCH-DATE
           ELSE
              MOVE PC-LTC-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPC-LTC-LTOUCH-DATE.

           MOVE PC-LTC-DESC(1) TO QPC-LTC-DESC-1.
           MOVE PC-LTC-MAKE(1) TO QPC-LTC-MAKE-1.
           MOVE PC-LTC-MODEL(1) TO QPC-LTC-MODEL-1.
           MOVE PC-LTC-YEAR(1) TO QPC-LTC-YEAR-1.
           MOVE PC-LTC-STYLE(1) TO QPC-LTC-STYLE-1.
           MOVE PC-LTC-CLASS(1) TO QPC-LTC-CLASS-1.
           MOVE PC-LTC-VIN(1) TO QPC-LTC-VIN-1.
           MOVE PC-LTC-TITLE-NO(1) TO QPC-LTC-TITLE-NO-1.
           MOVE PC-LTC-MILEAGE(1) TO QPC-LTC-MILEAGE-1.
           MOVE PC-LTC-VALUE(1) TO QPC-LTC-VALUE-1.
           MOVE PC-LTC-SELLPR(1) TO QPC-LTC-SELLPR-1.
           MOVE PC-LTC-MISC-AMT(1) TO QPC-LTC-MISC-AMT-1.
           MOVE PC-LTC-SENT-FLAG(1) TO QPC-LTC-SENT-FLAG-1.

           IF ( PC-LTC-VIN-CHG-DATE(1) = ZEROES )
              MOVE "1900-01-01" TO QPC-LTC-VIN-CHG-DATE-1
           ELSE
              MOVE PC-LTC-VIN-CHG-DATE(1) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPC-LTC-VIN-CHG-DATE-1.

           MOVE PC-LTC-DESC(2) TO QPC-LTC-DESC-2.
           MOVE PC-LTC-MAKE(2) TO QPC-LTC-MAKE-2.
           MOVE PC-LTC-MODEL(2) TO QPC-LTC-MODEL-2.
           MOVE PC-LTC-YEAR(2) TO QPC-LTC-YEAR-2.
           MOVE PC-LTC-STYLE(2) TO QPC-LTC-STYLE-2.
           MOVE PC-LTC-CLASS(2) TO QPC-LTC-CLASS-2.
           MOVE PC-LTC-VIN(2) TO QPC-LTC-VIN-2.
           MOVE PC-LTC-TITLE-NO(2) TO QPC-LTC-TITLE-NO-2.
           MOVE PC-LTC-MILEAGE(2) TO QPC-LTC-MILEAGE-2.
           MOVE PC-LTC-VALUE(2) TO QPC-LTC-VALUE-2.
           MOVE PC-LTC-SELLPR(2) TO QPC-LTC-SELLPR-2.
           MOVE PC-LTC-MISC-AMT(2) TO QPC-LTC-MISC-AMT-2.
           MOVE PC-LTC-SENT-FLAG(2) TO QPC-LTC-SENT-FLAG-2.

           IF ( PC-LTC-VIN-CHG-DATE(2) = ZEROES )
              MOVE "1900-01-01" TO QPC-LTC-VIN-CHG-DATE-2
           ELSE
              MOVE PC-LTC-VIN-CHG-DATE(2) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPC-LTC-VIN-CHG-DATE-2.

           MOVE PC-LTC-DESC(3) TO QPC-LTC-DESC-3.
           MOVE PC-LTC-MAKE(3) TO QPC-LTC-MAKE-3.
           MOVE PC-LTC-MODEL(3) TO QPC-LTC-MODEL-3.
           MOVE PC-LTC-YEAR(3) TO QPC-LTC-YEAR-3.
           MOVE PC-LTC-STYLE(3) TO QPC-LTC-STYLE-3.
           MOVE PC-LTC-CLASS(3) TO QPC-LTC-CLASS-3.
           MOVE PC-LTC-VIN(3) TO QPC-LTC-VIN-3.
           MOVE PC-LTC-TITLE-NO(3) TO QPC-LTC-TITLE-NO-3.
           MOVE PC-LTC-MILEAGE(3) TO QPC-LTC-MILEAGE-3.
           MOVE PC-LTC-VALUE(3) TO QPC-LTC-VALUE-3.
           MOVE PC-LTC-SELLPR(3) TO QPC-LTC-SELLPR-3.
           MOVE PC-LTC-MISC-AMT(3) TO QPC-LTC-MISC-AMT-3.
           MOVE PC-LTC-SENT-FLAG(3) TO QPC-LTC-SENT-FLAG-3.

           IF ( PC-LTC-VIN-CHG-DATE(3) = ZEROES )
              MOVE "1900-01-01" TO QPC-LTC-VIN-CHG-DATE-3
           ELSE
              MOVE PC-LTC-VIN-CHG-DATE(3) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPC-LTC-VIN-CHG-DATE-3.

           MOVE PC-LTC-HHGS(1) TO QPC-LTC-HHGS-1.
           MOVE PC-LTC-HHGS(2) TO QPC-LTC-HHGS-2.
           MOVE PC-LTC-HHGS(3) TO QPC-LTC-HHGS-3.
           MOVE PC-LTA-BRNO TO QPC-LTA-BRNO.
           MOVE PC-LTA-ACCTNO TO QPC-LTA-ACCTNO.
           MOVE PC-LTA-SEQNO TO QPC-LTA-SEQNO.

           IF ( PC-LTA-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QPC-LTA-LTOUCH-DATE
           ELSE
              MOVE PC-LTA-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPC-LTA-LTOUCH-DATE.

           MOVE PC-LTA-DLR-REQ TO QPC-LTA-DLR-REQ.
           MOVE PC-LTA-INS-TYPE TO QPC-LTA-INS-TYPE.
           MOVE PC-LTA-INS-CO TO QPC-LTA-INS-CO.
           MOVE PC-LTA-POLICY-NO TO QPC-LTA-POLICY-NO.
           MOVE PC-LTA-AGENCY TO QPC-LTA-AGENCY.
           MOVE PC-LTA-AGENT TO QPC-LTA-AGENT.
           MOVE PC-LTA-ADR1 TO QPC-LTA-ADR1.
           MOVE PC-LTA-ADR2 TO QPC-LTA-ADR2.
           MOVE PC-LTA-CITY TO QPC-LTA-CITY.
           MOVE PC-LTA-STATE TO QPC-LTA-STATE.
           MOVE PC-LTA-ZIP TO QPC-LTA-ZIP.
           MOVE PC-LTA-PHONE TO QPC-LTA-PHONE.

           IF ( PC-LTA-EFF-DATE = ZEROES )
              MOVE "1900-01-01" TO QPC-LTA-EFF-DATE
           ELSE
              MOVE PC-LTA-EFF-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPC-LTA-EFF-DATE.


           IF ( PC-LTA-EXP-DATE = ZEROES )
              MOVE "1900-01-01" TO QPC-LTA-EXP-DATE
           ELSE
              MOVE PC-LTA-EXP-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPC-LTA-EXP-DATE.


           IF ( PC-LTA-CAN-DATE = ZEROES )
              MOVE "1900-01-01" TO QPC-LTA-CAN-DATE
           ELSE
              MOVE PC-LTA-CAN-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPC-LTA-CAN-DATE.


           IF ( PC-LTA-CONF-DATE = ZEROES )
              MOVE "1900-01-01" TO QPC-LTA-CONF-DATE
           ELSE
              MOVE PC-LTA-CONF-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPC-LTA-CONF-DATE.

           MOVE PC-LTA-LAST-FORM TO QPC-LTA-LAST-FORM.
           MOVE PC-LTA-COV-TYPE TO QPC-LTA-COV-TYPE.
           MOVE PC-LTA-BIL-COV TO QPC-LTA-BIL-COV.
           MOVE PC-LTA-PDL-COV TO QPC-LTA-PDL-COV.
           MOVE PC-LTA-PIP-COV TO QPC-LTA-PIP-COV.
           MOVE PC-LTA-UMB-COV TO QPC-LTA-UMB-COV.
           MOVE PC-LTA-COL-COV TO QPC-LTA-COL-COV.
           MOVE PC-LTA-LOSS-PAYEE TO QPC-LTA-LOSS-PAYEE.
           MOVE PC-LTA-COM-COV TO QPC-LTA-COM-COV.
           MOVE PC-LTA-TRACK-ONLY TO QPC-LTA-TRACK-ONLY.
           MOVE PC-LTA-INSURANCE TO QPC-LTA-INSURANCE.
           MOVE PC-LTA-TRACK-MONTHS TO QPC-LTA-TRACK-MONTHS.
           MOVE PC-LTA-INS-MONTHS TO QPC-LTA-INS-MONTHS.
           MOVE PC-LTA-CPP-MONTHS TO QPC-LTA-CPP-MONTHS.

           IF ( PC-LOANDATE = ZEROES )
              MOVE "1900-01-01" TO QPC-LOANDATE
           ELSE
              MOVE PC-LOANDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPC-LOANDATE.

           MOVE PC-ORGTERM TO QPC-ORGTERM.
           MOVE PC-REGPYAMT TO QPC-REGPYAMT.
           MOVE PC2-ID TO QPC2-ID.
           MOVE PC2-BRANCH TO QPC2-BRANCH.
           MOVE PC2-BWNAME TO QPC2-BWNAME.

           IF ( PC-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QPC-LTOUCH-DATE
           ELSE
              MOVE PC-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPC-LTOUCH-DATE.

 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-PC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( PC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( PC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO PC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO PC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO PC-ALL-DATA
              MOVE PC-ALL-PATH             TO PC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( PC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO PC-ALL-BASE
              MOVE FILPATH-MACHINE         TO PC-ALL-MACHINE
              MOVE FILPATH-DATA            TO PC-ALL-DATA
              MOVE PC-ALL-PATH             TO PC-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-PCFILE NOT = "S" )
              MOVE PC-PATH-BEG             TO PC-SELECT-PATH-BEG
              MOVE PC-PATH-FILE            TO PC-SELECT-PATH-FILE
           ELSE
              MOVE PC-PATH-SQL             TO PC-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-PC1-FILE.
           PERFORM LOAD-PC1-FILE.
           PERFORM OPEN-IT.
           MOVE PC-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-PC1-FILE.
           PERFORM START-IT.
           MOVE PC-PATH-OWNBR  TO PC-BRANCH.
           MOVE PC-SELECT-PATH TO E-FILE.
           MOVE PC1-KEY        TO E-KEYX.

           IF ( PC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-PC1-FILE.

           IF ( QPC1-WEND-ID = SPACES )
              MOVE PC-PATH-OWNBR        TO QPC1-WBEG-BRANCH
              MOVE PC-PATH-OWNBR        TO QPC1-WEND-BRANCH
              MOVE PC-ID                TO QPC1-WBEG-ID
              MOVE ALL "Z"              TO QPC1-WEND-ID
              MOVE PC-SOC-SEC           TO QPC1-WBEG-SSNO
              MOVE ALL "9"              TO QPC1-WEND-SSNO.


           EXEC SQL
            DECLARE PC1_CURSOR CURSOR FOR
            SELECT ISNULL(PCFILE.PC_ID,''),
                   PCFILE.PC_BRANCH,
                   PCFILE.PC_SOC_SEC,
                   PCFILE.PC_LTC_BRNO,
                   PCFILE.PC_LTC_ACCTNO,
                   PCFILE.PC_LTC_SEQNO,
                   ISNULL(CAST(PCFILE.PC_LTC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(PCFILE.PC_LTC_DESC_1,''),
                   ISNULL(PCFILE.PC_LTC_MAKE_1,''),
                   ISNULL(PCFILE.PC_LTC_MODEL_1,''),
                   ISNULL(PCFILE.PC_LTC_YEAR_1,''),
                   ISNULL(PCFILE.PC_LTC_STYLE_1,''),
                   ISNULL(PCFILE.PC_LTC_CLASS_1,''),
                   ISNULL(PCFILE.PC_LTC_VIN_1,''),
                   ISNULL(PCFILE.PC_LTC_TITLE_NO_1,''),
                   PCFILE.PC_LTC_MILEAGE_1,
                   PCFILE.PC_LTC_VALUE_1,
                   PCFILE.PC_LTC_SELLPR_1,
                   PCFILE.PC_LTC_MISC_AMT_1,
                   ISNULL(PCFILE.PC_LTC_SENT_FLAG_1,''),
                   ISNULL(CAST(PCFILE.PC_LTC_VIN_CHG_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(PCFILE.PC_LTC_DESC_2,''),
                   ISNULL(PCFILE.PC_LTC_MAKE_2,''),
                   ISNULL(PCFILE.PC_LTC_MODEL_2,''),
                   ISNULL(PCFILE.PC_LTC_YEAR_2,''),
                   ISNULL(PCFILE.PC_LTC_STYLE_2,''),
                   ISNULL(PCFILE.PC_LTC_CLASS_2,''),
                   ISNULL(PCFILE.PC_LTC_VIN_2,''),
                   ISNULL(PCFILE.PC_LTC_TITLE_NO_2,''),
                   PCFILE.PC_LTC_MILEAGE_2,
                   PCFILE.PC_LTC_VALUE_2,
                   PCFILE.PC_LTC_SELLPR_2,
                   PCFILE.PC_LTC_MISC_AMT_2,
                   ISNULL(PCFILE.PC_LTC_SENT_FLAG_2,''),
                   ISNULL(CAST(PCFILE.PC_LTC_VIN_CHG_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(PCFILE.PC_LTC_DESC_3,''),
                   ISNULL(PCFILE.PC_LTC_MAKE_3,''),
                   ISNULL(PCFILE.PC_LTC_MODEL_3,''),
                   ISNULL(PCFILE.PC_LTC_YEAR_3,''),
                   ISNULL(PCFILE.PC_LTC_STYLE_3,''),
                   ISNULL(PCFILE.PC_LTC_CLASS_3,''),
                   ISNULL(PCFILE.PC_LTC_VIN_3,''),
                   ISNULL(PCFILE.PC_LTC_TITLE_NO_3,''),
                   PCFILE.PC_LTC_MILEAGE_3,
                   PCFILE.PC_LTC_VALUE_3,
                   PCFILE.PC_LTC_SELLPR_3,
                   PCFILE.PC_LTC_MISC_AMT_3,
                   ISNULL(PCFILE.PC_LTC_SENT_FLAG_3,''),
                   ISNULL(CAST(PCFILE.PC_LTC_VIN_CHG_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(PCFILE.PC_LTC_HHGS_1,''),
                   ISNULL(PCFILE.PC_LTC_HHGS_2,''),
                   ISNULL(PCFILE.PC_LTC_HHGS_3,''),
                   PCFILE.PC_LTA_BRNO,
                   PCFILE.PC_LTA_ACCTNO,
                   PCFILE.PC_LTA_SEQNO,
                   ISNULL(CAST(PCFILE.PC_LTA_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(PCFILE.PC_LTA_DLR_REQ,''),
                   ISNULL(PCFILE.PC_LTA_INS_TYPE,''),
                   ISNULL(PCFILE.PC_LTA_INS_CO,''),
                   ISNULL(PCFILE.PC_LTA_POLICY_NO,''),
                   ISNULL(PCFILE.PC_LTA_AGENCY,''),
                   ISNULL(PCFILE.PC_LTA_AGENT,''),
                   ISNULL(PCFILE.PC_LTA_ADR1,''),
                   ISNULL(PCFILE.PC_LTA_ADR2,''),
                   ISNULL(PCFILE.PC_LTA_CITY,''),
                   ISNULL(PCFILE.PC_LTA_STATE,''),
                   ISNULL(PCFILE.PC_LTA_ZIP,''),
                   PCFILE.PC_LTA_PHONE,
                   ISNULL(CAST(PCFILE.PC_LTA_EFF_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PCFILE.PC_LTA_EXP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PCFILE.PC_LTA_CAN_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PCFILE.PC_LTA_CONF_DATE
                                                  AS VARCHAR(10)),'0'),
                   PCFILE.PC_LTA_LAST_FORM,
                   ISNULL(PCFILE.PC_LTA_COV_TYPE,''),
                   PCFILE.PC_LTA_BIL_COV,
                   PCFILE.PC_LTA_PDL_COV,
                   PCFILE.PC_LTA_PIP_COV,
                   PCFILE.PC_LTA_UMB_COV,
                   PCFILE.PC_LTA_COL_COV,
                   ISNULL(PCFILE.PC_LTA_LOSS_PAYEE,''),
                   PCFILE.PC_LTA_COM_COV,
                   ISNULL(PCFILE.PC_LTA_TRACK_ONLY,''),
                   ISNULL(PCFILE.PC_LTA_INSURANCE,''),
                   ISNULL(PCFILE.PC_LTA_TRACK_MONTHS,''),
                   ISNULL(PCFILE.PC_LTA_INS_MONTHS,''),
                   ISNULL(PCFILE.PC_LTA_CPP_MONTHS,''),
                   ISNULL(CAST(PCFILE.PC_LOANDATE AS VARCHAR(10)),'0'),
                   PCFILE.PC_ORGTERM,
                   PCFILE.PC_REGPYAMT,
                   ISNULL(PCFILE.PC2_ID,''),
                   PCFILE.PC2_BRANCH,
                   ISNULL(PCFILE.PC2_BWNAME,''),
                   ISNULL(CAST(PCFILE.PC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            FROM DBO.PCFILE
            WHERE PCFILE.PC_ID >= :QPC1-WBEG-ID
              AND PCFILE.PC_ID <= :QPC1-WEND-ID
              AND PCFILE.PC_BRANCH >= :QPC1-WBEG-BRANCH
              AND PCFILE.PC_BRANCH <= :QPC1-WEND-BRANCH
              AND PCFILE.PC_SOC_SEC >= :QPC1-WBEG-SSNO
              AND PCFILE.PC_SOC_SEC <= :QPC1-WEND-SSNO
            ORDER BY PCFILE.PC_ID,
                     PCFILE.PC_BRANCH,
                     PCFILE.PC_SOC_SEC
           END-EXEC.

           EXEC SQL
               OPEN PC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QPC1-WBEG-BRANCH 
                          QPC1-WEND-BRANCH
                          QPC1-WBEG-SSNO
                          QPC1-WEND-SSNO.
 
           MOVE SPACES TO QPC1-WBEG-ID
                          QPC1-WEND-ID.

           MOVE STAT---GOOD TO PC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-PC1-FILE.
           PERFORM READ-IT.
           MOVE PC-PATH-OWNBR  TO PC-BRANCH. 
           MOVE PC-SELECT-PATH TO E-FILE.
           MOVE PC1-KEY        TO E-KEYX.

           MOVE PC-ID     TO QPC-ID.
           MOVE PC-BRANCH TO QPC-BRANCH.
           MOVE PC-SOC-SEC   TO QPC-SOC-SEC.

           EXEC SQL
            SELECT ISNULL(PCFILE.PC_ID,''),
                   PCFILE.PC_BRANCH,
                   PCFILE.PC_SOC_SEC,
                   PCFILE.PC_LTC_BRNO,
                   PCFILE.PC_LTC_ACCTNO,
                   PCFILE.PC_LTC_SEQNO,
                   ISNULL(CAST(PCFILE.PC_LTC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(PCFILE.PC_LTC_DESC_1,''),
                   ISNULL(PCFILE.PC_LTC_MAKE_1,''),
                   ISNULL(PCFILE.PC_LTC_MODEL_1,''),
                   ISNULL(PCFILE.PC_LTC_YEAR_1,''),
                   ISNULL(PCFILE.PC_LTC_STYLE_1,''),
                   ISNULL(PCFILE.PC_LTC_CLASS_1,''),
                   ISNULL(PCFILE.PC_LTC_VIN_1,''),
                   ISNULL(PCFILE.PC_LTC_TITLE_NO_1,''),
                   PCFILE.PC_LTC_MILEAGE_1,
                   PCFILE.PC_LTC_VALUE_1,
                   PCFILE.PC_LTC_SELLPR_1,
                   PCFILE.PC_LTC_MISC_AMT_1,
                   ISNULL(PCFILE.PC_LTC_SENT_FLAG_1,''),
                   ISNULL(CAST(PCFILE.PC_LTC_VIN_CHG_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(PCFILE.PC_LTC_DESC_2,''),
                   ISNULL(PCFILE.PC_LTC_MAKE_2,''),
                   ISNULL(PCFILE.PC_LTC_MODEL_2,''),
                   ISNULL(PCFILE.PC_LTC_YEAR_2,''),
                   ISNULL(PCFILE.PC_LTC_STYLE_2,''),
                   ISNULL(PCFILE.PC_LTC_CLASS_2,''),
                   ISNULL(PCFILE.PC_LTC_VIN_2,''),
                   ISNULL(PCFILE.PC_LTC_TITLE_NO_2,''),
                   PCFILE.PC_LTC_MILEAGE_2,
                   PCFILE.PC_LTC_VALUE_2,
                   PCFILE.PC_LTC_SELLPR_2,
                   PCFILE.PC_LTC_MISC_AMT_2,
                   ISNULL(PCFILE.PC_LTC_SENT_FLAG_2,''),
                   ISNULL(CAST(PCFILE.PC_LTC_VIN_CHG_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(PCFILE.PC_LTC_DESC_3,''),
                   ISNULL(PCFILE.PC_LTC_MAKE_3,''),
                   ISNULL(PCFILE.PC_LTC_MODEL_3,''),
                   ISNULL(PCFILE.PC_LTC_YEAR_3,''),
                   ISNULL(PCFILE.PC_LTC_STYLE_3,''),
                   ISNULL(PCFILE.PC_LTC_CLASS_3,''),
                   ISNULL(PCFILE.PC_LTC_VIN_3,''),
                   ISNULL(PCFILE.PC_LTC_TITLE_NO_3,''),
                   PCFILE.PC_LTC_MILEAGE_3,
                   PCFILE.PC_LTC_VALUE_3,
                   PCFILE.PC_LTC_SELLPR_3,
                   PCFILE.PC_LTC_MISC_AMT_3,
                   ISNULL(PCFILE.PC_LTC_SENT_FLAG_3,''),
                   ISNULL(CAST(PCFILE.PC_LTC_VIN_CHG_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(PCFILE.PC_LTC_HHGS_1,''),
                   ISNULL(PCFILE.PC_LTC_HHGS_2,''),
                   ISNULL(PCFILE.PC_LTC_HHGS_3,''),
                   PCFILE.PC_LTA_BRNO,
                   PCFILE.PC_LTA_ACCTNO,
                   PCFILE.PC_LTA_SEQNO,
                   ISNULL(CAST(PCFILE.PC_LTA_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(PCFILE.PC_LTA_DLR_REQ,''),
                   ISNULL(PCFILE.PC_LTA_INS_TYPE,''),
                   ISNULL(PCFILE.PC_LTA_INS_CO,''),
                   ISNULL(PCFILE.PC_LTA_POLICY_NO,''),
                   ISNULL(PCFILE.PC_LTA_AGENCY,''),
                   ISNULL(PCFILE.PC_LTA_AGENT,''),
                   ISNULL(PCFILE.PC_LTA_ADR1,''),
                   ISNULL(PCFILE.PC_LTA_ADR2,''),
                   ISNULL(PCFILE.PC_LTA_CITY,''),
                   ISNULL(PCFILE.PC_LTA_STATE,''),
                   ISNULL(PCFILE.PC_LTA_ZIP,''),
                   PCFILE.PC_LTA_PHONE,
                   ISNULL(CAST(PCFILE.PC_LTA_EFF_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PCFILE.PC_LTA_EXP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PCFILE.PC_LTA_CAN_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(PCFILE.PC_LTA_CONF_DATE
                                                  AS VARCHAR(10)),'0'),
                   PCFILE.PC_LTA_LAST_FORM,
                   ISNULL(PCFILE.PC_LTA_COV_TYPE,''),
                   PCFILE.PC_LTA_BIL_COV,
                   PCFILE.PC_LTA_PDL_COV,
                   PCFILE.PC_LTA_PIP_COV,
                   PCFILE.PC_LTA_UMB_COV,
                   PCFILE.PC_LTA_COL_COV,
                   ISNULL(PCFILE.PC_LTA_LOSS_PAYEE,''),
                   PCFILE.PC_LTA_COM_COV,
                   ISNULL(PCFILE.PC_LTA_TRACK_ONLY,''),
                   ISNULL(PCFILE.PC_LTA_INSURANCE,''),
                   ISNULL(PCFILE.PC_LTA_TRACK_MONTHS,''),
                   ISNULL(PCFILE.PC_LTA_INS_MONTHS,''),
                   ISNULL(PCFILE.PC_LTA_CPP_MONTHS,''),
                   ISNULL(CAST(PCFILE.PC_LOANDATE AS VARCHAR(10)),'0'),
                   PCFILE.PC_ORGTERM,
                   PCFILE.PC_REGPYAMT,
                   ISNULL(PCFILE.PC2_ID,''),
                   PCFILE.PC2_BRANCH,
                   ISNULL(PCFILE.PC2_BWNAME,''),
                   ISNULL(CAST(PCFILE.PC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            INTO :QPC-REC
            FROM DBO.PCFILE
            WHERE PCFILE.PC_ID = :QPC-ID
              AND PCFILE.PC_BRANCH = :QPC-BRANCH
              AND PCFILE.PC_SOC_SEC = :QPC-SOC-SEC
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-PC-FIELDS.

      *-----------------------------------------------------------------
       READ-PC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE PC-PATH-OWNBR  TO PC-BRANCH.
           MOVE PC-SELECT-PATH TO E-FILE.
           MOVE PC1-KEY        TO E-KEYX.
           INITIALIZE QPC-REC.

           IF ( PC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH PC1_CURSOR INTO :QPC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-PC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-PC1-FILE.

           IF ( PC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE PC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO PC1-CURSOR-STAT.

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
              DESTROY IOPCRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
