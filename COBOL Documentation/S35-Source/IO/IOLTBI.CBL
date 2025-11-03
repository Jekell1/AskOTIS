       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOLTBI.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-12-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOLTBI.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : LTBFILE 
      * TABLE DESC    : LOAN TRAILER BANKRUPT FILE
      * OUTPUT FILE   : XPC/IOLTBI.PRE
      * USED FOR      : LIBLP/LPWSLTB.CPY  --- QLP-REC
      *                 LIBLP/LPLTB1R.CPY  --- READ
      *                 LIBLP/LPLTBRN.CPY --- READ/READ-NEXT/START
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
           "@(#)IO/IOLTBI.CBL S34 05/12/21-12:29:50 >".

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
       COPY "LIBLP/LP01LTB.CPY".
       COPY "LIBLP/LPWSLTB.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QLTB1-WHERE-BEG-KEY.
           03  QLTB1-WBEG-BRNO             PIC 9(04).
           03  QLTB1-WBEG-ACCTNO           PIC 9(08).
           03  QLTB1-WBEG-SEQNO            PIC 9(03).
 
       01  QLTB1-WHERE-END-KEY.
           03  QLTB1-WEND-BRNO             PIC 9(04).
           03  QLTB1-WEND-ACCTNO           PIC 9(08).
           03  QLTB1-WEND-SEQNO            PIC 9(03).
 
       01  QLTB-REC.
           03  QLTB-BRNO              PIC 9(4).
           03  QLTB-ACCTNO            PIC 9(8).
           03  QLTB-SEQNO             PIC 999.
           03  QLTB-LTOUCH-DATE       PIC X(10).
           03  QLTB-FILEDATE         PIC X(10).
           03  QLTB-CASE-NO          PIC X(9).
           03  QLTB-CHAPTER          PIC 99.
           03  QLTB-MISC1            PIC X(30).
           03  QLTB-MISC2            PIC X(30).
           03  QLTB-MISC3            PIC X(30).
           03  QLTB-BDDATE           PIC X(10).
           03  QLTB-WHO-FILED        PIC X.
           03  QLTB-ENDORSER-FILED   PIC X.
           03  QLTB-CLAIM-AMT        PIC S9(7)V99.
           03  QLTB-ACTIVE-INACTIVE  PIC X.
           03  QLTB-INT-LC-DUE       PIC S9(7)V99.
           03  QLTB-OTH-DUE          PIC S9(7)V99.
           03  QLTB-TDUEAMT          PIC S9(7)V99.
           03  QLTB-NAME-1             PIC X(24).
           03  QLTB-ADR1-1             PIC X(24).
           03  QLTB-ADR2-1             PIC X(24).
           03  QLTB-CITY-1             PIC X(15).
           03  QLTB-STATE-1            PIC XX.
           03  QLTB-ZIP-1              PIC X(10).
           03  QLTB-PHONE-1            PIC 9(10).
           03  QLTB-NAME-2             PIC X(24).
           03  QLTB-ADR1-2             PIC X(24).
           03  QLTB-ADR2-2             PIC X(24).
           03  QLTB-CITY-2             PIC X(15).
           03  QLTB-STATE-2            PIC XX.
           03  QLTB-ZIP-2              PIC X(10).
           03  QLTB-PHONE-2            PIC 9(10).
           03  QLTB-NAME-3             PIC X(24).
           03  QLTB-ADR1-3             PIC X(24).
           03  QLTB-ADR2-3             PIC X(24).
           03  QLTB-CITY-3             PIC X(15).
           03  QLTB-STATE-3            PIC XX.
           03  QLTB-ZIP-3              PIC X(10).
           03  QLTB-PHONE-3            PIC 9(10).
           03  QLTB-REAFF-DATE           PIC X(10).
           03  QLTB-REAFF-SYSDATE        PIC X(10).
           03  QLTB-REAFF-BALANCE        PIC 9(6)V99.
           03  QLTB-REAFF-PAYAMT         PIC 9(6)V99.
           03  QLTB-REAFF-NO-PAYS        PIC 9(3).
           03  QLTB-INTRATE              PIC 9(3)V9(3).
           03  QLTB-RESCIND-DATE         PIC X(10).
           03  QLTB-PARTIAL-FULL         PIC X.
           03  QLTB-DISCHARGE-DATE       PIC X(10).
           03  QLTB-DISCHARGE-SYSDATE    PIC X(10).
           03  QLTB-GROSS-BAL            PIC S9(7)V99.

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
           03  FILLER PIC X(35) VALUE
                                "BRNO   ACCTNO    SEQ    FILEDATE".
           03  FILLER PIC X(30) VALUE "CHP     CLAIM     CASE".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-LTB-BRNO                   PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-LTB-ACCTNO                 PIC 9(08).
           03  FILLER                         PIC X(02).
           03  D01-LTB-SEQNO                  PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-LTB-FILEDATE               PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-LTB-CHAPTER                PIC 99.
           03  FILLER                         PIC X(03).
           03  D01-LTB-CLAIM-AMT              PIC Z(6)9.99-.
           03  FILLER                         PIC X(03).
           03  D01-LTB-CASE-NO                PIC X(9).

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
       COPY "LIBIO/IOLTBI_DEF.CPY".
       COPY "LIBIO/IOLTBI_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOLTBI_SCN.CPY".

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
           MOVE "IOLTBI"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOLTBI-QUEUE-POS  TO Q-POS.
           MOVE IOLTBI-COPIES-POS TO C-POS.
           MOVE IOLTBI-PRU-POS    TO P-POS.
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

           MOVE "B"                        TO LTB-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QLTB1-WBEG-BRNO.
           MOVE BR-NO     TO QLTB1-WEND-BRNO.

           MOVE BEG-ACCTNO TO QLTB1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QLTB1-WEND-ACCTNO.
           MOVE BEG-SEQNO  TO QLTB1-WBEG-SEQNO.
           MOVE END-SEQNO  TO QLTB1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-LTB1-FILE.
           PERFORM START-LTB1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-LTB1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LTB-BRNO NOT = LTB-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( LTB-ACCTNO < BEG-ACCTNO )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( LTB-ACCTNO > END-ACCTNO )
              GO TO PROCESS-BRANCH-MODULE-END.

           IF LTB-SEQNO < BEG-SEQNO OR > END-SEQNO
              GO TO PROCESS-BRANCH-NEXT.


      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE LTB-BRNO                   TO D01-LTB-BRNO.
           MOVE LTB-ACCTNO                 TO D01-LTB-ACCTNO.
           MOVE LTB-SEQNO                  TO D01-LTB-SEQNO.
           MOVE LTB-FILEDATE               TO D01-LTB-FILEDATE.
           MOVE LTB-CHAPTER                TO D01-LTB-CHAPTER.
           MOVE LTB-CLAIM-AMT              TO D01-LTB-CLAIM-AMT.
           MOVE LTB-CASE-NO                TO D01-LTB-CASE-NO.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-LTB1-FILE.

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

           MOVE LTB1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "SQL TABLE IO UTILITY - LTBFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOLTBI-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOLTBI-SYSDATE.
           DISPLAY IOLTBI-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOLTBI-FORM-FIELDS TO WR-BUF.
           MOVE IOLTBI-FORM-LIST TO WR-LIST.
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

           MOVE IOLTBI-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOLTBI-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOLTBI_EVA.CPY".
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
      *    LOAD LTB-REC FROM QLTB-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LTB-FIELDS.
           MOVE QLTB-BRNO TO LTB-BRNO.
           MOVE QLTB-ACCTNO TO LTB-ACCTNO.
           MOVE QLTB-SEQNO TO LTB-SEQNO.

           IF ( QLTB-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LTB-LTOUCH-DATE
           ELSE
              MOVE QLTB-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTB-LTOUCH-DATE.


           IF ( QLTB-FILEDATE = "1900-01-01" )
              MOVE ZEROES TO LTB-FILEDATE
           ELSE
              MOVE QLTB-FILEDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTB-FILEDATE.

           MOVE QLTB-CASE-NO TO LTB-CASE-NO.
           MOVE QLTB-CHAPTER TO LTB-CHAPTER.
           MOVE QLTB-MISC1 TO LTB-MISC1.
           MOVE QLTB-MISC2 TO LTB-MISC2.
           MOVE QLTB-MISC3 TO LTB-MISC3.

           IF ( QLTB-BDDATE = "1900-01-01" )
              MOVE ZEROES TO LTB-BDDATE
           ELSE
              MOVE QLTB-BDDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTB-BDDATE.

           MOVE QLTB-WHO-FILED TO LTB-WHO-FILED.
           MOVE QLTB-ENDORSER-FILED TO LTB-ENDORSER-FILED.
           MOVE QLTB-CLAIM-AMT TO LTB-CLAIM-AMT.
           MOVE QLTB-ACTIVE-INACTIVE TO LTB-ACTIVE-INACTIVE.
           MOVE QLTB-INT-LC-DUE TO LTB-INT-LC-DUE.
           MOVE QLTB-OTH-DUE TO LTB-OTH-DUE.
           MOVE QLTB-TDUEAMT TO LTB-TDUEAMT.
           MOVE QLTB-NAME-1 TO LTB-NAME(1).
           MOVE QLTB-ADR1-1 TO LTB-ADR1(1).
           MOVE QLTB-ADR2-1 TO LTB-ADR2(1).
           MOVE QLTB-CITY-1 TO LTB-CITY(1).
           MOVE QLTB-STATE-1 TO LTB-STATE(1).
           MOVE QLTB-ZIP-1 TO LTB-ZIP(1).
           MOVE QLTB-PHONE-1 TO LTB-PHONE(1).
           MOVE QLTB-NAME-2 TO LTB-NAME(2).
           MOVE QLTB-ADR1-2 TO LTB-ADR1(2).
           MOVE QLTB-ADR2-2 TO LTB-ADR2(2).
           MOVE QLTB-CITY-2 TO LTB-CITY(2).
           MOVE QLTB-STATE-2 TO LTB-STATE(2).
           MOVE QLTB-ZIP-2 TO LTB-ZIP(2).
           MOVE QLTB-PHONE-2 TO LTB-PHONE(2).
           MOVE QLTB-NAME-3 TO LTB-NAME(3).
           MOVE QLTB-ADR1-3 TO LTB-ADR1(3).
           MOVE QLTB-ADR2-3 TO LTB-ADR2(3).
           MOVE QLTB-CITY-3 TO LTB-CITY(3).
           MOVE QLTB-STATE-3 TO LTB-STATE(3).
           MOVE QLTB-ZIP-3 TO LTB-ZIP(3).
           MOVE QLTB-PHONE-3 TO LTB-PHONE(3).

           IF ( QLTB-REAFF-DATE = "1900-01-01" )
              MOVE ZEROES TO LTB-REAFF-DATE
           ELSE
              MOVE QLTB-REAFF-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTB-REAFF-DATE.


           IF ( QLTB-REAFF-SYSDATE = "1900-01-01" )
              MOVE ZEROES TO LTB-REAFF-SYSDATE
           ELSE
              MOVE QLTB-REAFF-SYSDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTB-REAFF-SYSDATE.

           MOVE QLTB-REAFF-BALANCE TO LTB-REAFF-BALANCE.
           MOVE QLTB-REAFF-PAYAMT TO LTB-REAFF-PAYAMT.
           MOVE QLTB-REAFF-NO-PAYS TO LTB-REAFF-NO-PAYS.
           MOVE QLTB-INTRATE TO LTB-INTRATE.

           IF ( QLTB-RESCIND-DATE = "1900-01-01" )
              MOVE ZEROES TO LTB-RESCIND-DATE
           ELSE
              MOVE QLTB-RESCIND-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTB-RESCIND-DATE.

           MOVE QLTB-PARTIAL-FULL TO LTB-PARTIAL-FULL.

           IF ( QLTB-DISCHARGE-DATE = "1900-01-01" )
              MOVE ZEROES TO LTB-DISCHARGE-DATE
           ELSE
              MOVE QLTB-DISCHARGE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTB-DISCHARGE-DATE.


           IF ( QLTB-DISCHARGE-SYSDATE = "1900-01-01" )
              MOVE ZEROES TO LTB-DISCHARGE-SYSDATE
           ELSE
              MOVE QLTB-DISCHARGE-SYSDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTB-DISCHARGE-SYSDATE.

           MOVE QLTB-GROSS-BAL TO LTB-GROSS-BAL.
 
      *-----------------------------------------------------------------
      *    LOAD QLTB-REC FROM LTB-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LTB-FIELDS.
           MOVE LTB-BRNO TO QLTB-BRNO.
           MOVE LTB-ACCTNO TO QLTB-ACCTNO.
           MOVE LTB-SEQNO TO QLTB-SEQNO.

           IF ( LTB-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTB-LTOUCH-DATE
           ELSE
              MOVE LTB-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTB-LTOUCH-DATE.


           IF ( LTB-FILEDATE = ZEROES )
              MOVE "1900-01-01" TO QLTB-FILEDATE
           ELSE
              MOVE LTB-FILEDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTB-FILEDATE.

           MOVE LTB-CASE-NO TO QLTB-CASE-NO.
           MOVE LTB-CHAPTER TO QLTB-CHAPTER.
           MOVE LTB-MISC1 TO QLTB-MISC1.
           MOVE LTB-MISC2 TO QLTB-MISC2.
           MOVE LTB-MISC3 TO QLTB-MISC3.

           IF ( LTB-BDDATE = ZEROES )
              MOVE "1900-01-01" TO QLTB-BDDATE
           ELSE
              MOVE LTB-BDDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTB-BDDATE.

           MOVE LTB-WHO-FILED TO QLTB-WHO-FILED.
           MOVE LTB-ENDORSER-FILED TO QLTB-ENDORSER-FILED.
           MOVE LTB-CLAIM-AMT TO QLTB-CLAIM-AMT.
           MOVE LTB-ACTIVE-INACTIVE TO QLTB-ACTIVE-INACTIVE.
           MOVE LTB-INT-LC-DUE TO QLTB-INT-LC-DUE.
           MOVE LTB-OTH-DUE TO QLTB-OTH-DUE.
           MOVE LTB-TDUEAMT TO QLTB-TDUEAMT.
           MOVE LTB-NAME(1) TO QLTB-NAME-1.
           MOVE LTB-ADR1(1) TO QLTB-ADR1-1.
           MOVE LTB-ADR2(1) TO QLTB-ADR2-1.
           MOVE LTB-CITY(1) TO QLTB-CITY-1.
           MOVE LTB-STATE(1) TO QLTB-STATE-1.
           MOVE LTB-ZIP(1) TO QLTB-ZIP-1.
           MOVE LTB-PHONE(1) TO QLTB-PHONE-1.
           MOVE LTB-NAME(2) TO QLTB-NAME-2.
           MOVE LTB-ADR1(2) TO QLTB-ADR1-2.
           MOVE LTB-ADR2(2) TO QLTB-ADR2-2.
           MOVE LTB-CITY(2) TO QLTB-CITY-2.
           MOVE LTB-STATE(2) TO QLTB-STATE-2.
           MOVE LTB-ZIP(2) TO QLTB-ZIP-2.
           MOVE LTB-PHONE(2) TO QLTB-PHONE-2.
           MOVE LTB-NAME(3) TO QLTB-NAME-3.
           MOVE LTB-ADR1(3) TO QLTB-ADR1-3.
           MOVE LTB-ADR2(3) TO QLTB-ADR2-3.
           MOVE LTB-CITY(3) TO QLTB-CITY-3.
           MOVE LTB-STATE(3) TO QLTB-STATE-3.
           MOVE LTB-ZIP(3) TO QLTB-ZIP-3.
           MOVE LTB-PHONE(3) TO QLTB-PHONE-3.

           IF ( LTB-REAFF-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTB-REAFF-DATE
           ELSE
              MOVE LTB-REAFF-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTB-REAFF-DATE.


           IF ( LTB-REAFF-SYSDATE = ZEROES )
              MOVE "1900-01-01" TO QLTB-REAFF-SYSDATE
           ELSE
              MOVE LTB-REAFF-SYSDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTB-REAFF-SYSDATE.

           MOVE LTB-REAFF-BALANCE TO QLTB-REAFF-BALANCE.
           MOVE LTB-REAFF-PAYAMT TO QLTB-REAFF-PAYAMT.
           MOVE LTB-REAFF-NO-PAYS TO QLTB-REAFF-NO-PAYS.
           MOVE LTB-INTRATE TO QLTB-INTRATE.

           IF ( LTB-RESCIND-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTB-RESCIND-DATE
           ELSE
              MOVE LTB-RESCIND-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTB-RESCIND-DATE.

           MOVE LTB-PARTIAL-FULL TO QLTB-PARTIAL-FULL.

           IF ( LTB-DISCHARGE-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTB-DISCHARGE-DATE
           ELSE
              MOVE LTB-DISCHARGE-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTB-DISCHARGE-DATE.


           IF ( LTB-DISCHARGE-SYSDATE = ZEROES )
              MOVE "1900-01-01" TO QLTB-DISCHARGE-SYSDATE
           ELSE
              MOVE LTB-DISCHARGE-SYSDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTB-DISCHARGE-SYSDATE.

           MOVE LTB-GROSS-BAL TO QLTB-GROSS-BAL.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LTB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTB-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTB-ALL-DATA
              MOVE LTB-ALL-PATH             TO LTB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTB-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTB-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTB-ALL-DATA
              MOVE LTB-ALL-PATH             TO LTB-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LTBFILE NOT = "S" )
              MOVE LTB-PATH-BEG             TO LTB-SELECT-PATH-BEG
              MOVE LTB-PATH-FILE            TO LTB-SELECT-PATH-FILE
           ELSE
              MOVE LTB-PATH-SQL             TO LTB-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LTB1-FILE.
           PERFORM LOAD-LTB1-FILE.
           PERFORM OPEN-IT.
           MOVE LTB-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTB1-FILE.
           PERFORM START-IT.
           MOVE LTB-PATH-OWNBR  TO LTB-BRNO.
           MOVE LTB-SELECT-PATH TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.

           IF ( LTB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTB1-FILE.

           IF ( QLTB1-WEND-BRNO = ZEROES    ) OR
              ( QLTB1-WEND-BRNO NOT NUMERIC )
              MOVE LTB-PATH-OWNBR        TO QLTB1-WBEG-BRNO
              MOVE LTB-PATH-OWNBR        TO QLTB1-WEND-BRNO
              MOVE LTB-ACCTNO            TO QLTB1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTB1-WEND-ACCTNO
              MOVE LTB-SEQNO             TO QLTB1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTB1-WEND-SEQNO.


           EXEC SQL
            DECLARE LTB1_CURSOR CURSOR FOR
            SELECT LTBFILE.LTB_BRNO,
                   LTBFILE.LTB_ACCTNO,
                   LTBFILE.LTB_SEQNO,
                   ISNULL(CAST(LTBFILE.LTB_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LTBFILE.LTB_FILEDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTBFILE.LTB_CASE_NO,''),
                   LTBFILE.LTB_CHAPTER,
                   ISNULL(LTBFILE.LTB_MISC1,''),
                   ISNULL(LTBFILE.LTB_MISC2,''),
                   ISNULL(LTBFILE.LTB_MISC3,''),
                   ISNULL(CAST(LTBFILE.LTB_BDDATE AS VARCHAR(10)),'0'),
                   ISNULL(LTBFILE.LTB_WHO_FILED,''),
                   ISNULL(LTBFILE.LTB_ENDORSER_FILED,''),
                   LTBFILE.LTB_CLAIM_AMT,
                   ISNULL(LTBFILE.LTB_ACTIVE_INACTIVE,''),
                   LTBFILE.LTB_INT_LC_DUE,
                   LTBFILE.LTB_OTH_DUE,
                   LTBFILE.LTB_TDUEAMT,
                   ISNULL(LTBFILE.LTB_NAME_1,''),
                   ISNULL(LTBFILE.LTB_ADR1_1,''),
                   ISNULL(LTBFILE.LTB_ADR2_1,''),
                   ISNULL(LTBFILE.LTB_CITY_1,''),
                   ISNULL(LTBFILE.LTB_STATE_1,''),
                   ISNULL(LTBFILE.LTB_ZIP_1,''),
                   LTBFILE.LTB_PHONE_1,
                   ISNULL(LTBFILE.LTB_NAME_2,''),
                   ISNULL(LTBFILE.LTB_ADR1_2,''),
                   ISNULL(LTBFILE.LTB_ADR2_2,''),
                   ISNULL(LTBFILE.LTB_CITY_2,''),
                   ISNULL(LTBFILE.LTB_STATE_2,''),
                   ISNULL(LTBFILE.LTB_ZIP_2,''),
                   LTBFILE.LTB_PHONE_2,
                   ISNULL(LTBFILE.LTB_NAME_3,''),
                   ISNULL(LTBFILE.LTB_ADR1_3,''),
                   ISNULL(LTBFILE.LTB_ADR2_3,''),
                   ISNULL(LTBFILE.LTB_CITY_3,''),
                   ISNULL(LTBFILE.LTB_STATE_3,''),
                   ISNULL(LTBFILE.LTB_ZIP_3,''),
                   LTBFILE.LTB_PHONE_3,
                   ISNULL(CAST(LTBFILE.LTB_REAFF_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LTBFILE.LTB_REAFF_SYSDATE
                                                  AS VARCHAR(10)),'0'),
                   LTBFILE.LTB_REAFF_BALANCE,
                   LTBFILE.LTB_REAFF_PAYAMT,
                   LTBFILE.LTB_REAFF_NO_PAYS,
                   LTBFILE.LTB_INTRATE,
                   ISNULL(CAST(LTBFILE.LTB_RESCIND_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTBFILE.LTB_PARTIAL_FULL,''),
                   ISNULL(CAST(LTBFILE.LTB_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LTBFILE.LTB_DISCHARGE_SYSDATE
                                                  AS VARCHAR(10)),'0'),
                   LTBFILE.LTB_GROSS_BAL
            FROM DBO.LTBFILE
            WHERE LTBFILE.LTB_BRNO >= :QLTB1-WBEG-BRNO
              AND LTBFILE.LTB_BRNO <= :QLTB1-WEND-BRNO
              AND LTBFILE.LTB_ACCTNO >= :QLTB1-WBEG-ACCTNO
              AND LTBFILE.LTB_ACCTNO <= :QLTB1-WEND-ACCTNO
              AND LTBFILE.LTB_SEQNO >= :QLTB1-WBEG-SEQNO
              AND LTBFILE.LTB_SEQNO <= :QLTB1-WEND-SEQNO
            ORDER BY LTBFILE.LTB_BRNO,
                     LTBFILE.LTB_ACCTNO,
                     LTBFILE.LTB_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTB1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTB1-WBEG-BRNO
                          QLTB1-WEND-BRNO
                          QLTB1-WBEG-ACCTNO
                          QLTB1-WEND-ACCTNO
                          QLTB1-WBEG-SEQNO
                          QLTB1-WEND-SEQNO.

           MOVE STAT---GOOD TO LTB1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTB1-FILE.
           PERFORM READ-IT.
           MOVE LTB-PATH-OWNBR  TO LTB-BRNO. 
           MOVE LTB-SELECT-PATH TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.

           MOVE LTB-BRNO   TO QLTB-BRNO.
           MOVE LTB-ACCTNO TO QLTB-ACCTNO.
           MOVE LTB-SEQNO  TO QLTB-SEQNO.

           EXEC SQL
            SELECT LTBFILE.LTB_BRNO,
                   LTBFILE.LTB_ACCTNO,
                   LTBFILE.LTB_SEQNO,
                   ISNULL(CAST(LTBFILE.LTB_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LTBFILE.LTB_FILEDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTBFILE.LTB_CASE_NO,''),
                   LTBFILE.LTB_CHAPTER,
                   ISNULL(LTBFILE.LTB_MISC1,''),
                   ISNULL(LTBFILE.LTB_MISC2,''),
                   ISNULL(LTBFILE.LTB_MISC3,''),
                   ISNULL(CAST(LTBFILE.LTB_BDDATE AS VARCHAR(10)),'0'),
                   ISNULL(LTBFILE.LTB_WHO_FILED,''),
                   ISNULL(LTBFILE.LTB_ENDORSER_FILED,''),
                   LTBFILE.LTB_CLAIM_AMT,
                   ISNULL(LTBFILE.LTB_ACTIVE_INACTIVE,''),
                   LTBFILE.LTB_INT_LC_DUE,
                   LTBFILE.LTB_OTH_DUE,
                   LTBFILE.LTB_TDUEAMT,
                   ISNULL(LTBFILE.LTB_NAME_1,''),
                   ISNULL(LTBFILE.LTB_ADR1_1,''),
                   ISNULL(LTBFILE.LTB_ADR2_1,''),
                   ISNULL(LTBFILE.LTB_CITY_1,''),
                   ISNULL(LTBFILE.LTB_STATE_1,''),
                   ISNULL(LTBFILE.LTB_ZIP_1,''),
                   LTBFILE.LTB_PHONE_1,
                   ISNULL(LTBFILE.LTB_NAME_2,''),
                   ISNULL(LTBFILE.LTB_ADR1_2,''),
                   ISNULL(LTBFILE.LTB_ADR2_2,''),
                   ISNULL(LTBFILE.LTB_CITY_2,''),
                   ISNULL(LTBFILE.LTB_STATE_2,''),
                   ISNULL(LTBFILE.LTB_ZIP_2,''),
                   LTBFILE.LTB_PHONE_2,
                   ISNULL(LTBFILE.LTB_NAME_3,''),
                   ISNULL(LTBFILE.LTB_ADR1_3,''),
                   ISNULL(LTBFILE.LTB_ADR2_3,''),
                   ISNULL(LTBFILE.LTB_CITY_3,''),
                   ISNULL(LTBFILE.LTB_STATE_3,''),
                   ISNULL(LTBFILE.LTB_ZIP_3,''),
                   LTBFILE.LTB_PHONE_3,
                   ISNULL(CAST(LTBFILE.LTB_REAFF_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LTBFILE.LTB_REAFF_SYSDATE
                                                  AS VARCHAR(10)),'0'),
                   LTBFILE.LTB_REAFF_BALANCE,
                   LTBFILE.LTB_REAFF_PAYAMT,
                   LTBFILE.LTB_REAFF_NO_PAYS,
                   LTBFILE.LTB_INTRATE,
                   ISNULL(CAST(LTBFILE.LTB_RESCIND_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTBFILE.LTB_PARTIAL_FULL,''),
                   ISNULL(CAST(LTBFILE.LTB_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LTBFILE.LTB_DISCHARGE_SYSDATE
                                                  AS VARCHAR(10)),'0'),
                   LTBFILE.LTB_GROSS_BAL
            INTO :QLTB-REC
            FROM DBO.LTBFILE
            WHERE LTBFILE.LTB_BRNO = :QLTB-BRNO
              AND LTBFILE.LTB_ACCTNO = :QLTB-ACCTNO
              AND LTBFILE.LTB_SEQNO = :QLTB-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTB-FIELDS.

      *-----------------------------------------------------------------
       READ-LTB1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTB-PATH-OWNBR  TO LTB-BRNO.
           MOVE LTB-SELECT-PATH TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.
           INITIALIZE QLTB-REC.

           IF ( LTB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTB1_CURSOR INTO :QLTB-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTB1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTB-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LTB1-FILE.

           MOVE LTB-PATH-OWNBR  TO LTB-BRNO.
           ACCEPT LTB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LTB-SELECT-PATH TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.
           PERFORM SET-LTB-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LTBFILE
             (LTBFILE.LTB_BRNO,
              LTBFILE.LTB_ACCTNO,
              LTBFILE.LTB_SEQNO,
              LTBFILE.LTB_LTOUCH_DATE,
              LTBFILE.LTB_FILEDATE,
              LTBFILE.LTB_CASE_NO,
              LTBFILE.LTB_CHAPTER,
              LTBFILE.LTB_MISC1,
              LTBFILE.LTB_MISC2,
              LTBFILE.LTB_MISC3,
              LTBFILE.LTB_BDDATE,
              LTBFILE.LTB_WHO_FILED,
              LTBFILE.LTB_ENDORSER_FILED,
              LTBFILE.LTB_CLAIM_AMT,
              LTBFILE.LTB_ACTIVE_INACTIVE,
              LTBFILE.LTB_INT_LC_DUE,
              LTBFILE.LTB_OTH_DUE,
              LTBFILE.LTB_TDUEAMT,
              LTBFILE.LTB_NAME_1,
              LTBFILE.LTB_ADR1_1,
              LTBFILE.LTB_ADR2_1,
              LTBFILE.LTB_CITY_1,
              LTBFILE.LTB_STATE_1,
              LTBFILE.LTB_ZIP_1,
              LTBFILE.LTB_PHONE_1,
              LTBFILE.LTB_NAME_2,
              LTBFILE.LTB_ADR1_2,
              LTBFILE.LTB_ADR2_2,
              LTBFILE.LTB_CITY_2,
              LTBFILE.LTB_STATE_2,
              LTBFILE.LTB_ZIP_2,
              LTBFILE.LTB_PHONE_2,
              LTBFILE.LTB_NAME_3,
              LTBFILE.LTB_ADR1_3,
              LTBFILE.LTB_ADR2_3,
              LTBFILE.LTB_CITY_3,
              LTBFILE.LTB_STATE_3,
              LTBFILE.LTB_ZIP_3,
              LTBFILE.LTB_PHONE_3,
              LTBFILE.LTB_REAFF_DATE,
              LTBFILE.LTB_REAFF_SYSDATE,
              LTBFILE.LTB_REAFF_BALANCE,
              LTBFILE.LTB_REAFF_PAYAMT,
              LTBFILE.LTB_REAFF_NO_PAYS,
              LTBFILE.LTB_INTRATE,
              LTBFILE.LTB_RESCIND_DATE,
              LTBFILE.LTB_PARTIAL_FULL,
              LTBFILE.LTB_DISCHARGE_DATE,
              LTBFILE.LTB_DISCHARGE_SYSDATE,
              LTBFILE.LTB_GROSS_BAL)
            VALUES
             (:QLTB-BRNO,
              :QLTB-ACCTNO,
              :QLTB-SEQNO,
              NULLIF(:QLTB-LTOUCH-DATE,'1900-01-01'),
              NULLIF(:QLTB-FILEDATE,'1900-01-01'),
              :QLTB-CASE-NO,
              :QLTB-CHAPTER,
              :QLTB-MISC1,
              :QLTB-MISC2,
              :QLTB-MISC3,
              NULLIF(:QLTB-BDDATE,'1900-01-01'),
              :QLTB-WHO-FILED,
              :QLTB-ENDORSER-FILED,
              :QLTB-CLAIM-AMT,
              :QLTB-ACTIVE-INACTIVE,
              :QLTB-INT-LC-DUE,
              :QLTB-OTH-DUE,
              :QLTB-TDUEAMT,
              :QLTB-NAME-1,
              :QLTB-ADR1-1,
              :QLTB-ADR2-1,
              :QLTB-CITY-1,
              :QLTB-STATE-1,
              :QLTB-ZIP-1,
              :QLTB-PHONE-1,
              :QLTB-NAME-2,
              :QLTB-ADR1-2,
              :QLTB-ADR2-2,
              :QLTB-CITY-2,
              :QLTB-STATE-2,
              :QLTB-ZIP-2,
              :QLTB-PHONE-2,
              :QLTB-NAME-3,
              :QLTB-ADR1-3,
              :QLTB-ADR2-3,
              :QLTB-CITY-3,
              :QLTB-STATE-3,
              :QLTB-ZIP-3,
              :QLTB-PHONE-3,
              NULLIF(:QLTB-REAFF-DATE,'1900-01-01'),
              NULLIF(:QLTB-REAFF-SYSDATE,'1900-01-01'),
              :QLTB-REAFF-BALANCE,
              :QLTB-REAFF-PAYAMT,
              :QLTB-REAFF-NO-PAYS,
              :QLTB-INTRATE,
              NULLIF(:QLTB-RESCIND-DATE,'1900-01-01'),
              :QLTB-PARTIAL-FULL,
              NULLIF(:QLTB-DISCHARGE-DATE,'1900-01-01'),
              NULLIF(:QLTB-DISCHARGE-SYSDATE,'1900-01-01'),
              :QLTB-GROSS-BAL)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LTB1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LTB1-FILE.

           MOVE LTB-PATH-OWNBR  TO LTB-BRNO.
           ACCEPT LTB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LTB-SELECT-PATH TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.
           PERFORM SET-LTB-FIELDS.

           EXEC SQL
            UPDATE DBO.LTBFILE
             SET
              LTBFILE.LTB_BRNO = :QLTB-BRNO,
              LTBFILE.LTB_ACCTNO = :QLTB-ACCTNO,
              LTBFILE.LTB_SEQNO = :QLTB-SEQNO,
              LTBFILE.LTB_LTOUCH_DATE =
                 NULLIF(:QLTB-LTOUCH-DATE,'1900-01-01'),
              LTBFILE.LTB_FILEDATE =
                 NULLIF(:QLTB-FILEDATE,'1900-01-01'),
              LTBFILE.LTB_CASE_NO = :QLTB-CASE-NO,
              LTBFILE.LTB_CHAPTER = :QLTB-CHAPTER,
              LTBFILE.LTB_MISC1 = :QLTB-MISC1,
              LTBFILE.LTB_MISC2 = :QLTB-MISC2,
              LTBFILE.LTB_MISC3 = :QLTB-MISC3,
              LTBFILE.LTB_BDDATE =
                 NULLIF(:QLTB-BDDATE,'1900-01-01'),
              LTBFILE.LTB_WHO_FILED = :QLTB-WHO-FILED,
              LTBFILE.LTB_ENDORSER_FILED = :QLTB-ENDORSER-FILED,
              LTBFILE.LTB_CLAIM_AMT = :QLTB-CLAIM-AMT,
              LTBFILE.LTB_ACTIVE_INACTIVE = :QLTB-ACTIVE-INACTIVE,
              LTBFILE.LTB_INT_LC_DUE = :QLTB-INT-LC-DUE,
              LTBFILE.LTB_OTH_DUE = :QLTB-OTH-DUE,
              LTBFILE.LTB_TDUEAMT = :QLTB-TDUEAMT,
              LTBFILE.LTB_NAME_1 = :QLTB-NAME-1,
              LTBFILE.LTB_ADR1_1 = :QLTB-ADR1-1,
              LTBFILE.LTB_ADR2_1 = :QLTB-ADR2-1,
              LTBFILE.LTB_CITY_1 = :QLTB-CITY-1,
              LTBFILE.LTB_STATE_1 = :QLTB-STATE-1,
              LTBFILE.LTB_ZIP_1 = :QLTB-ZIP-1,
              LTBFILE.LTB_PHONE_1 = :QLTB-PHONE-1,
              LTBFILE.LTB_NAME_2 = :QLTB-NAME-2,
              LTBFILE.LTB_ADR1_2 = :QLTB-ADR1-2,
              LTBFILE.LTB_ADR2_2 = :QLTB-ADR2-2,
              LTBFILE.LTB_CITY_2 = :QLTB-CITY-2,
              LTBFILE.LTB_STATE_2 = :QLTB-STATE-2,
              LTBFILE.LTB_ZIP_2 = :QLTB-ZIP-2,
              LTBFILE.LTB_PHONE_2 = :QLTB-PHONE-2,
              LTBFILE.LTB_NAME_3 = :QLTB-NAME-3,
              LTBFILE.LTB_ADR1_3 = :QLTB-ADR1-3,
              LTBFILE.LTB_ADR2_3 = :QLTB-ADR2-3,
              LTBFILE.LTB_CITY_3 = :QLTB-CITY-3,
              LTBFILE.LTB_STATE_3 = :QLTB-STATE-3,
              LTBFILE.LTB_ZIP_3 = :QLTB-ZIP-3,
              LTBFILE.LTB_PHONE_3 = :QLTB-PHONE-3,
              LTBFILE.LTB_REAFF_DATE =
                 NULLIF(:QLTB-REAFF-DATE,'1900-01-01'),
              LTBFILE.LTB_REAFF_SYSDATE =
                 NULLIF(:QLTB-REAFF-SYSDATE,'1900-01-01'),
              LTBFILE.LTB_REAFF_BALANCE = :QLTB-REAFF-BALANCE,
              LTBFILE.LTB_REAFF_PAYAMT = :QLTB-REAFF-PAYAMT,
              LTBFILE.LTB_REAFF_NO_PAYS = :QLTB-REAFF-NO-PAYS,
              LTBFILE.LTB_INTRATE = :QLTB-INTRATE,
              LTBFILE.LTB_RESCIND_DATE =
                 NULLIF(:QLTB-RESCIND-DATE,'1900-01-01'),
              LTBFILE.LTB_PARTIAL_FULL = :QLTB-PARTIAL-FULL,
              LTBFILE.LTB_DISCHARGE_DATE =
                 NULLIF(:QLTB-DISCHARGE-DATE,'1900-01-01'),
              LTBFILE.LTB_DISCHARGE_SYSDATE =
                 NULLIF(:QLTB-DISCHARGE-SYSDATE,'1900-01-01'),
              LTBFILE.LTB_GROSS_BAL = :QLTB-GROSS-BAL
             WHERE LTBFILE.LTB_BRNO = :QLTB-BRNO
               AND LTBFILE.LTB_ACCTNO = :QLTB-ACCTNO
               AND LTBFILE.LTB_SEQNO = :QLTB-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LTB1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LTB1-FILE.

           MOVE LTB-PATH-OWNBR  TO LTB-BRNO.
           PERFORM DELETE-IT.
           MOVE LTB-SELECT-PATH TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.
           PERFORM SET-LTB-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LTBFILE
             WHERE LTBFILE.LTB_BRNO = :QLTB-BRNO
               AND LTBFILE.LTB_ACCTNO = :QLTB-ACCTNO
               AND LTBFILE.LTB_SEQNO = :QLTB-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LTB1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LTB1-FILE.

           IF ( LTB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTB1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTB1-CURSOR-STAT.

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
              DESTROY IOLTBI-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
