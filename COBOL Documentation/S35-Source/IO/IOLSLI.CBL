       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOLSLI.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-12-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOLSLI.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : LSLFILE 
      * TABLE DESC    : LOAN TRAILER LEGAL INFO FILE
      * OUTPUT FILE   : XPC/IOLSLI.PRE
      * USED FOR      : LIBLP/LPWSLSL.CPY  --- QLSL-REC
      *                 LIBLP/LPLSL1R.CPY  --- READ
      *                 LIBLP/LPLAL1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0511 ***NEW***
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
           "@(#)IO/IOLSLI.CBL S34 05/12/21-12:17:51 >".

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
       COPY "LIBLP/LP01LSL.CPY".
       COPY "LIBLP/LPWSLSL.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QLSL1-WHERE-BEG-KEY.
           03  QLSL1-WBEG-BRNO             PIC 9(04).
           03  QLSL1-WBEG-ACCTNO           PIC 9(08).
           03  QLSL1-WBEG-SEQNO            PIC 9(02).

       01  QLSL1-WHERE-END-KEY.
           03  QLSL1-WEND-BRNO             PIC 9(04).
           03  QLSL1-WEND-ACCTNO           PIC 9(08).
           03  QLSL1-WEND-SEQNO            PIC 9(02).

       01  QLSL-REC.
           03  QLSL-BRNO              PIC 9(4).
           03  QLSL-ACCTNO            PIC 9(8).
           03  QLSL-SEQNO             PIC 99.
           03  QLSL-LTOUCH-DATE           PIC X(10).
           03  QLSL-DATE-SUIT-AUTH        PIC X(10).
           03  QLSL-DATE-SUIT-AUTH-BY     PIC X(20).
           03  QLSL-ATTORNEY-NET-ASSIGN   PIC X(20).
           03  QLSL-LOCAL-ATTORNEY        PIC X(20).
           03  QLSL-ATTORNEY-ADDRESS      PIC X(60).
           03  QLSL-ATTORNEY-PHONE        PIC 9(10).
           03  QLSL-DATE-ASSIGNED         PIC X(10).
           03  QLSL-ATTORNEY-ACCTNO       PIC X(20).
           03  QLSL-PRIN-BAL-SENT         PIC S9(7)V99.
           03  QLSL-RECEIVED-ACKNOW-DATE       PIC X(10).
           03  QLSL-ATTORNEY-LETTER-SENT-DATE  PIC X(10).
           03  QLSL-HOLD-DATE                  PIC X(10).
           03  QLSL-RESPONSE-RECEIVED-DATE     PIC X(10).
           03  QLSL-SUIT-DATE             PIC X(10).
           03  QLSL-SUIT-FILED-DATE       PIC X(10).
           03  QLSL-SERVICE-DATE          PIC X(10).
           03  QLSL-CUSTOMER-SERVED       PIC X.
           03  QLSL-ANSWER-FILED-DATE     PIC X(10).
           03  QLSL-COURT-DATE            PIC X(10).
           03  QLSL-JUDGMENT-TYPE         PIC X(20).
           03  QLSL-JUDGMENT-OBTAINED-DATE PIC X(10).
           03  QLSL-JUDGMENT-COPY-FILED   PIC X.
           03  QLSL-JUDGMENT-AMT          PIC S9(7)V99.
           03  QLSL-JUDGMENT-PRIN         PIC S9(7)V99.
           03  QLSL-POST-JUDG-INT-RATE    PIC S9(3)V99.
           03  QLSL-JUDG-INT-AMT          PIC S9(7)V99.
           03  QLSL-ATTORNEY-FEES-ALLOWED PIC S9(7)V99.
           03  QLSL-OTHER-FEES-ALLOWED    PIC S9(7)V99.
           03  QLSL-JUDG-INT-RATE         PIC S9(3)V99.
           03  QLSL-ASSET-USED-IN-SUIT    PIC X(20).
           03  QLSL-GARN-FILED-DATE       PIC X(10).
           03  QLSL-GARN-TERM-DAYS        PIC 9(3).
           03  QLSL-INTERROGATORY-SENT-DATE PIC X(10).
           03  QLSL-INTERROGATORY-ANSW-DATE PIC X(10).
           03  QLSL-FIFA-SENT-DATE        PIC X(10).
           03  QLSL-FIFA-ANSWERED-DATE    PIC X(10).
           03  QLSL-DATE-TO-RECV-PAY      PIC X(10).
           03  QLSL-FOLLOWUP-DATE         PIC X(10).
           03  QLSL-FIFA-RENEWED-DATE     PIC X(10).
           03  QLSL-PAYS-RECV-ATTORNEY    PIC X.
           03  QLSL-DATE-CLOSED           PIC X(10).
           03  QLSL-REASON-CLOSED         PIC X(20).

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
           03  FILLER PIC X(24) VALUE "BRNO    ACCTNO    SEQ   ".
           03  FILLER PIC X(31) VALUE "ASSIGNED     ATTORNEY".
           03  FILLER PIC X(30) VALUE "COURT".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-LSL-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-LSL-ACCTNO                  PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-LSL-SEQNO                   PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-LSL-DATE-ASSIGNED          PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-LSL-LOCAL-ATTORNEY          PIC X(15).
           03  FILLER                         PIC X(03).
           03  D01-LSL-COURT-DATE             PIC 9999/99/99.

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
       COPY "LIBIO/IOLSLI_DEF.CPY".
       COPY "LIBIO/IOLSLI_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOLSLI_SCN.CPY".

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
           MOVE "IOLSLI"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOLSLI-QUEUE-POS  TO Q-POS.
           MOVE IOLSLI-COPIES-POS TO C-POS.
           MOVE IOLSLI-PRU-POS    TO P-POS.
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

           MOVE "B"                        TO LSL-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }

           MOVE BR-NO     TO QLSL1-WBEG-BRNO.
           MOVE BR-NO     TO QLSL1-WEND-BRNO.

           MOVE BEG-ACCTNO TO QLSL1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QLSL1-WEND-ACCTNO.
           MOVE BEG-SEQNO  TO QLSL1-WBEG-SEQNO.
           MOVE END-SEQNO  TO QLSL1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-LSL1-FILE.
           PERFORM START-LSL1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-LSL1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LSL-BRNO NOT = LSL-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( LSL-ACCTNO < BEG-ACCTNO )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( LSL-ACCTNO > END-ACCTNO )
              GO TO PROCESS-BRANCH-MODULE-END.

           IF LSL-SEQNO < BEG-SEQNO OR > END-SEQNO
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE LSL-BRNO                    TO D01-LSL-BRNO.
           MOVE LSL-ACCTNO                  TO D01-LSL-ACCTNO.
           MOVE LSL-SEQNO                   TO D01-LSL-SEQNO.
           MOVE LSL-DATE-ASSIGNED  TO D01-LSL-DATE-ASSIGNED.
           MOVE LSL-LOCAL-ATTORNEY TO D01-LSL-LOCAL-ATTORNEY.
           MOVE LSL-COURT-DATE     TO D01-LSL-COURT-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-LSL1-FILE.

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

           MOVE LSL1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "SQL TABLE IO UTILITY - LSLFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOLSLI-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOLSLI-SYSDATE.
           DISPLAY IOLSLI-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOLSLI-FORM-FIELDS TO WR-BUF.
           MOVE IOLSLI-FORM-LIST TO WR-LIST.
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

           MOVE IOLSLI-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOLSLI-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOLSLI_EVA.CPY".
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
      *    LOAD LSL-REC FROM QLSL-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LSL-FIELDS.
           MOVE QLSL-BRNO TO LSL-BRNO.
           MOVE QLSL-ACCTNO TO LSL-ACCTNO.
           MOVE QLSL-SEQNO TO LSL-SEQNO.

           IF ( QLSL-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LSL-LTOUCH-DATE
           ELSE
              MOVE QLSL-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-LTOUCH-DATE.


           IF ( QLSL-DATE-SUIT-AUTH = "1900-01-01" )
              MOVE ZEROES TO LSL-DATE-SUIT-AUTH
           ELSE
              MOVE QLSL-DATE-SUIT-AUTH TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-DATE-SUIT-AUTH.

           MOVE QLSL-DATE-SUIT-AUTH-BY TO LSL-DATE-SUIT-AUTH-BY.
           MOVE QLSL-ATTORNEY-NET-ASSIGN TO LSL-ATTORNEY-NET-ASSIGN.
           MOVE QLSL-LOCAL-ATTORNEY TO LSL-LOCAL-ATTORNEY.
           MOVE QLSL-ATTORNEY-ADDRESS TO LSL-ATTORNEY-ADDRESS.
           MOVE QLSL-ATTORNEY-PHONE TO LSL-ATTORNEY-PHONE.

           IF ( QLSL-DATE-ASSIGNED = "1900-01-01" )
              MOVE ZEROES TO LSL-DATE-ASSIGNED
           ELSE
              MOVE QLSL-DATE-ASSIGNED TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-DATE-ASSIGNED.

           MOVE QLSL-ATTORNEY-ACCTNO TO LSL-ATTORNEY-ACCTNO.
           MOVE QLSL-PRIN-BAL-SENT TO LSL-PRIN-BAL-SENT.

           IF ( QLSL-RECEIVED-ACKNOW-DATE = "1900-01-01" )
              MOVE ZEROES TO LSL-RECEIVED-ACKNOW-DATE
           ELSE
              MOVE QLSL-RECEIVED-ACKNOW-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-RECEIVED-ACKNOW-DATE.


           IF ( QLSL-ATTORNEY-LETTER-SENT-DATE = "1900-01-01" )
              MOVE ZEROES TO LSL-ATTORNEY-LETTER-SENT-DATE
           ELSE
              MOVE QLSL-ATTORNEY-LETTER-SENT-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-ATTORNEY-LETTER-SENT-DATE.


           IF ( QLSL-HOLD-DATE = "1900-01-01" )
              MOVE ZEROES TO LSL-HOLD-DATE
           ELSE
              MOVE QLSL-HOLD-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-HOLD-DATE.


           IF ( QLSL-RESPONSE-RECEIVED-DATE = "1900-01-01" )
              MOVE ZEROES TO LSL-RESPONSE-RECEIVED-DATE
           ELSE
              MOVE QLSL-RESPONSE-RECEIVED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-RESPONSE-RECEIVED-DATE.


           IF ( QLSL-SUIT-DATE = "1900-01-01" )
              MOVE ZEROES TO LSL-SUIT-DATE
           ELSE
              MOVE QLSL-SUIT-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-SUIT-DATE.


           IF ( QLSL-SUIT-FILED-DATE = "1900-01-01" )
              MOVE ZEROES TO LSL-SUIT-FILED-DATE
           ELSE
              MOVE QLSL-SUIT-FILED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-SUIT-FILED-DATE.


           IF ( QLSL-SERVICE-DATE = "1900-01-01" )
              MOVE ZEROES TO LSL-SERVICE-DATE
           ELSE
              MOVE QLSL-SERVICE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-SERVICE-DATE.

           MOVE QLSL-CUSTOMER-SERVED TO LSL-CUSTOMER-SERVED.

           IF ( QLSL-ANSWER-FILED-DATE = "1900-01-01" )
              MOVE ZEROES TO LSL-ANSWER-FILED-DATE
           ELSE
              MOVE QLSL-ANSWER-FILED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-ANSWER-FILED-DATE.


           IF ( QLSL-COURT-DATE = "1900-01-01" )
              MOVE ZEROES TO LSL-COURT-DATE
           ELSE
              MOVE QLSL-COURT-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-COURT-DATE.

           MOVE QLSL-JUDGMENT-TYPE TO LSL-JUDGMENT-TYPE.

           IF ( QLSL-JUDGMENT-OBTAINED-DATE = "1900-01-01" )
              MOVE ZEROES TO LSL-JUDGMENT-OBTAINED-DATE
           ELSE
              MOVE QLSL-JUDGMENT-OBTAINED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-JUDGMENT-OBTAINED-DATE.

           MOVE QLSL-JUDGMENT-COPY-FILED TO LSL-JUDGMENT-COPY-FILED.
           MOVE QLSL-JUDGMENT-AMT TO LSL-JUDGMENT-AMT.
           MOVE QLSL-JUDGMENT-PRIN TO LSL-JUDGMENT-PRIN.
           MOVE QLSL-POST-JUDG-INT-RATE TO LSL-POST-JUDG-INT-RATE.
           MOVE QLSL-JUDG-INT-AMT TO LSL-JUDG-INT-AMT.
           MOVE QLSL-ATTORNEY-FEES-ALLOWED
              TO LSL-ATTORNEY-FEES-ALLOWED.
           MOVE QLSL-OTHER-FEES-ALLOWED TO LSL-OTHER-FEES-ALLOWED.
           MOVE QLSL-JUDG-INT-RATE TO LSL-JUDG-INT-RATE.
           MOVE QLSL-ASSET-USED-IN-SUIT TO LSL-ASSET-USED-IN-SUIT.

           IF ( QLSL-GARN-FILED-DATE = "1900-01-01" )
              MOVE ZEROES TO LSL-GARN-FILED-DATE
           ELSE
              MOVE QLSL-GARN-FILED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-GARN-FILED-DATE.

           MOVE QLSL-GARN-TERM-DAYS TO LSL-GARN-TERM-DAYS.

           IF ( QLSL-INTERROGATORY-SENT-DATE = "1900-01-01" )
              MOVE ZEROES TO LSL-INTERROGATORY-SENT-DATE
           ELSE
              MOVE QLSL-INTERROGATORY-SENT-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-INTERROGATORY-SENT-DATE.


           IF ( QLSL-INTERROGATORY-ANSW-DATE = "1900-01-01" )
              MOVE ZEROES TO LSL-INTERROGATORY-ANSW-DATE
           ELSE
              MOVE QLSL-INTERROGATORY-ANSW-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-INTERROGATORY-ANSW-DATE.


           IF ( QLSL-FIFA-SENT-DATE = "1900-01-01" )
              MOVE ZEROES TO LSL-FIFA-SENT-DATE
           ELSE
              MOVE QLSL-FIFA-SENT-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-FIFA-SENT-DATE.


           IF ( QLSL-FIFA-ANSWERED-DATE = "1900-01-01" )
              MOVE ZEROES TO LSL-FIFA-ANSWERED-DATE
           ELSE
              MOVE QLSL-FIFA-ANSWERED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-FIFA-ANSWERED-DATE.


           IF ( QLSL-DATE-TO-RECV-PAY = "1900-01-01" )
              MOVE ZEROES TO LSL-DATE-TO-RECV-PAY
           ELSE
              MOVE QLSL-DATE-TO-RECV-PAY TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-DATE-TO-RECV-PAY.


           IF ( QLSL-FOLLOWUP-DATE = "1900-01-01" )
              MOVE ZEROES TO LSL-FOLLOWUP-DATE
           ELSE
              MOVE QLSL-FOLLOWUP-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-FOLLOWUP-DATE.


           IF ( QLSL-FIFA-RENEWED-DATE = "1900-01-01" )
              MOVE ZEROES TO LSL-FIFA-RENEWED-DATE
           ELSE
              MOVE QLSL-FIFA-RENEWED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-FIFA-RENEWED-DATE.

           MOVE QLSL-PAYS-RECV-ATTORNEY TO LSL-PAYS-RECV-ATTORNEY.

           IF ( QLSL-DATE-CLOSED = "1900-01-01" )
              MOVE ZEROES TO LSL-DATE-CLOSED
           ELSE
              MOVE QLSL-DATE-CLOSED TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LSL-DATE-CLOSED.

           MOVE QLSL-REASON-CLOSED TO LSL-REASON-CLOSED.
 
      *-----------------------------------------------------------------
      *    LOAD QLSL-REC FROM LSL-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LSL-FIELDS.
           MOVE LSL-BRNO TO QLSL-BRNO.
           MOVE LSL-ACCTNO TO QLSL-ACCTNO.
           MOVE LSL-SEQNO TO QLSL-SEQNO.

           IF ( LSL-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSL-LTOUCH-DATE
           ELSE
              MOVE LSL-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-LTOUCH-DATE.


           IF ( LSL-DATE-SUIT-AUTH = ZEROES )
              MOVE "1900-01-01" TO QLSL-DATE-SUIT-AUTH
           ELSE
              MOVE LSL-DATE-SUIT-AUTH TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-DATE-SUIT-AUTH.

           MOVE LSL-DATE-SUIT-AUTH-BY TO QLSL-DATE-SUIT-AUTH-BY.
           MOVE LSL-ATTORNEY-NET-ASSIGN TO QLSL-ATTORNEY-NET-ASSIGN.
           MOVE LSL-LOCAL-ATTORNEY TO QLSL-LOCAL-ATTORNEY.
           MOVE LSL-ATTORNEY-ADDRESS TO QLSL-ATTORNEY-ADDRESS.
           MOVE LSL-ATTORNEY-PHONE TO QLSL-ATTORNEY-PHONE.

           IF ( LSL-DATE-ASSIGNED = ZEROES )
              MOVE "1900-01-01" TO QLSL-DATE-ASSIGNED
           ELSE
              MOVE LSL-DATE-ASSIGNED TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-DATE-ASSIGNED.

           MOVE LSL-ATTORNEY-ACCTNO TO QLSL-ATTORNEY-ACCTNO.
           MOVE LSL-PRIN-BAL-SENT TO QLSL-PRIN-BAL-SENT.

           IF ( LSL-RECEIVED-ACKNOW-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSL-RECEIVED-ACKNOW-DATE
           ELSE
              MOVE LSL-RECEIVED-ACKNOW-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-RECEIVED-ACKNOW-DATE.


           IF ( LSL-ATTORNEY-LETTER-SENT-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSL-ATTORNEY-LETTER-SENT-DATE
           ELSE
              MOVE LSL-ATTORNEY-LETTER-SENT-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-ATTORNEY-LETTER-SENT-DATE.


           IF ( LSL-HOLD-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSL-HOLD-DATE
           ELSE
              MOVE LSL-HOLD-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-HOLD-DATE.


           IF ( LSL-RESPONSE-RECEIVED-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSL-RESPONSE-RECEIVED-DATE
           ELSE
              MOVE LSL-RESPONSE-RECEIVED-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-RESPONSE-RECEIVED-DATE.


           IF ( LSL-SUIT-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSL-SUIT-DATE
           ELSE
              MOVE LSL-SUIT-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-SUIT-DATE.


           IF ( LSL-SUIT-FILED-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSL-SUIT-FILED-DATE
           ELSE
              MOVE LSL-SUIT-FILED-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-SUIT-FILED-DATE.


           IF ( LSL-SERVICE-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSL-SERVICE-DATE
           ELSE
              MOVE LSL-SERVICE-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-SERVICE-DATE.

           MOVE LSL-CUSTOMER-SERVED TO QLSL-CUSTOMER-SERVED.

           IF ( LSL-ANSWER-FILED-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSL-ANSWER-FILED-DATE
           ELSE
              MOVE LSL-ANSWER-FILED-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-ANSWER-FILED-DATE.


           IF ( LSL-COURT-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSL-COURT-DATE
           ELSE
              MOVE LSL-COURT-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-COURT-DATE.

           MOVE LSL-JUDGMENT-TYPE TO QLSL-JUDGMENT-TYPE.

           IF ( LSL-JUDGMENT-OBTAINED-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSL-JUDGMENT-OBTAINED-DATE
           ELSE
              MOVE LSL-JUDGMENT-OBTAINED-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-JUDGMENT-OBTAINED-DATE.

           MOVE LSL-JUDGMENT-COPY-FILED TO QLSL-JUDGMENT-COPY-FILED.
           MOVE LSL-JUDGMENT-AMT TO QLSL-JUDGMENT-AMT.
           MOVE LSL-JUDGMENT-PRIN TO QLSL-JUDGMENT-PRIN.
           MOVE LSL-POST-JUDG-INT-RATE TO QLSL-POST-JUDG-INT-RATE.
           MOVE LSL-JUDG-INT-AMT TO QLSL-JUDG-INT-AMT.
           MOVE LSL-ATTORNEY-FEES-ALLOWED
            TO QLSL-ATTORNEY-FEES-ALLOWED.
           MOVE LSL-OTHER-FEES-ALLOWED TO QLSL-OTHER-FEES-ALLOWED.
           MOVE LSL-JUDG-INT-RATE TO QLSL-JUDG-INT-RATE.
           MOVE LSL-ASSET-USED-IN-SUIT TO QLSL-ASSET-USED-IN-SUIT.

           IF ( LSL-GARN-FILED-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSL-GARN-FILED-DATE
           ELSE
              MOVE LSL-GARN-FILED-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-GARN-FILED-DATE.

           MOVE LSL-GARN-TERM-DAYS TO QLSL-GARN-TERM-DAYS.

           IF ( LSL-INTERROGATORY-SENT-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSL-INTERROGATORY-SENT-DATE
           ELSE
              MOVE LSL-INTERROGATORY-SENT-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-INTERROGATORY-SENT-DATE.


           IF ( LSL-INTERROGATORY-ANSW-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSL-INTERROGATORY-ANSW-DATE
           ELSE
              MOVE LSL-INTERROGATORY-ANSW-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-INTERROGATORY-ANSW-DATE.


           IF ( LSL-FIFA-SENT-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSL-FIFA-SENT-DATE
           ELSE
              MOVE LSL-FIFA-SENT-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-FIFA-SENT-DATE.


           IF ( LSL-FIFA-ANSWERED-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSL-FIFA-ANSWERED-DATE
           ELSE
              MOVE LSL-FIFA-ANSWERED-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-FIFA-ANSWERED-DATE.


           IF ( LSL-DATE-TO-RECV-PAY = ZEROES )
              MOVE "1900-01-01" TO QLSL-DATE-TO-RECV-PAY
           ELSE
              MOVE LSL-DATE-TO-RECV-PAY TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-DATE-TO-RECV-PAY.


           IF ( LSL-FOLLOWUP-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSL-FOLLOWUP-DATE
           ELSE
              MOVE LSL-FOLLOWUP-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-FOLLOWUP-DATE.


           IF ( LSL-FIFA-RENEWED-DATE = ZEROES )
              MOVE "1900-01-01" TO QLSL-FIFA-RENEWED-DATE
           ELSE
              MOVE LSL-FIFA-RENEWED-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-FIFA-RENEWED-DATE.

           MOVE LSL-PAYS-RECV-ATTORNEY TO QLSL-PAYS-RECV-ATTORNEY.

           IF ( LSL-DATE-CLOSED = ZEROES )
              MOVE "1900-01-01" TO QLSL-DATE-CLOSED
           ELSE
              MOVE LSL-DATE-CLOSED TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLSL-DATE-CLOSED.

           MOVE LSL-REASON-CLOSED TO QLSL-REASON-CLOSED.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LSL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LSL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LSL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LSL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LSL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LSL-ALL-DATA
              MOVE LSL-ALL-PATH             TO LSL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LSL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LSL-ALL-BASE
              MOVE FILPATH-MACHINE         TO LSL-ALL-MACHINE
              MOVE FILPATH-DATA            TO LSL-ALL-DATA
              MOVE LSL-ALL-PATH             TO LSL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LSLFILE NOT = "S" )
              MOVE LSL-PATH-BEG             TO LSL-SELECT-PATH-BEG
              MOVE LSL-PATH-FILE            TO LSL-SELECT-PATH-FILE
           ELSE
              MOVE LSL-PATH-SQL             TO LSL-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LSL1-FILE.
           PERFORM LOAD-LSL1-FILE.
           PERFORM OPEN-IT.
           MOVE LSL-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LSL1-FILE.
           PERFORM START-IT.
           MOVE LSL-PATH-OWNBR  TO LSL-BRNO.
           MOVE LSL-SELECT-PATH TO E-FILE.
           MOVE LSL1-KEY        TO E-KEYX.

           IF ( LSL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LSL1-FILE.

           IF ( QLSL1-WEND-BRNO = ZEROES    ) OR
              ( QLSL1-WEND-BRNO NOT NUMERIC )
              MOVE LSL-PATH-OWNBR        TO QLSL1-WBEG-BRNO
              MOVE LSL-PATH-OWNBR        TO QLSL1-WEND-BRNO
              MOVE LSL-ACCTNO            TO QLSL1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLSL1-WEND-ACCTNO
              MOVE LSL-SEQNO             TO QLSL1-WBEG-SEQNO
              MOVE ALL "9"               TO QLSL1-WEND-SEQNO.


           EXEC SQL
            DECLARE LSL1_CURSOR CURSOR FOR
            SELECT LSLFILE.LSL_BRNO,
                   LSLFILE.LSL_ACCTNO,
                   LSLFILE.LSL_SEQNO,
                   ISNULL(CAST(LSLFILE.LSL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_DATE_SUIT_AUTH
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSLFILE.LSL_DATE_SUIT_AUTH_BY,''),
                   ISNULL(LSLFILE.LSL_ATTORNEY_NET_ASSIGN,''),
                   ISNULL(LSLFILE.LSL_LOCAL_ATTORNEY,''),
                   ISNULL(LSLFILE.LSL_ATTORNEY_ADDRESS,''),
                   LSLFILE.LSL_ATTORNEY_PHONE,
                   ISNULL(CAST(LSLFILE.LSL_DATE_ASSIGNED
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSLFILE.LSL_ATTORNEY_ACCTNO,''),
                   LSLFILE.LSL_PRIN_BAL_SENT,
                   ISNULL(CAST(LSLFILE.LSL_RECEIVED_ACKNOW_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_ATTORNEY_LETTER_SENT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_HOLD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_RESPONSE_RECEIVED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_SUIT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_SUIT_FILED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_SERVICE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSLFILE.LSL_CUSTOMER_SERVED,''),
                   ISNULL(CAST(LSLFILE.LSL_ANSWER_FILED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_COURT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSLFILE.LSL_JUDGMENT_TYPE,''),
                   ISNULL(CAST(LSLFILE.LSL_JUDGMENT_OBTAINED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSLFILE.LSL_JUDGMENT_COPY_FILED,''),
                   LSLFILE.LSL_JUDGMENT_AMT,
                   LSLFILE.LSL_JUDGMENT_PRIN,
                   LSLFILE.LSL_POST_JUDG_INT_RATE,
                   LSLFILE.LSL_JUDG_INT_AMT,
                   LSLFILE.LSL_ATTORNEY_FEES_ALLOWED,
                   LSLFILE.LSL_OTHER_FEES_ALLOWED,
                   LSLFILE.LSL_JUDG_INT_RATE,
                   ISNULL(LSLFILE.LSL_ASSET_USED_IN_SUIT,''),
                   ISNULL(CAST(LSLFILE.LSL_GARN_FILED_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSLFILE.LSL_GARN_TERM_DAYS,
                   ISNULL(CAST(LSLFILE.LSL_INTERROGATORY_SENT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_INTERROGATORY_ANSW_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_FIFA_SENT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_FIFA_ANSWERED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_DATE_TO_RECV_PAY
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_FIFA_RENEWED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSLFILE.LSL_PAYS_RECV_ATTORNEY,''),
                   ISNULL(CAST(LSLFILE.LSL_DATE_CLOSED
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSLFILE.LSL_REASON_CLOSED,'')
            FROM DBO.LSLFILE
            WHERE LSLFILE.LSL_BRNO >= :QLSL1-WBEG-BRNO
              AND LSLFILE.LSL_BRNO <= :QLSL1-WEND-BRNO
              AND LSLFILE.LSL_ACCTNO >= :QLSL1-WBEG-ACCTNO
              AND LSLFILE.LSL_ACCTNO <= :QLSL1-WEND-ACCTNO
              AND LSLFILE.LSL_SEQNO >= :QLSL1-WBEG-SEQNO
              AND LSLFILE.LSL_SEQNO <= :QLSL1-WEND-SEQNO
            ORDER BY LSLFILE.LSL_BRNO,
                     LSLFILE.LSL_ACCTNO,
                     LSLFILE.LSL_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LSL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLSL1-WBEG-BRNO
                          QLSL1-WEND-BRNO
                          QLSL1-WBEG-ACCTNO
                          QLSL1-WEND-ACCTNO
                          QLSL1-WBEG-SEQNO
                          QLSL1-WEND-SEQNO.

           MOVE STAT---GOOD TO LSL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LSL1-FILE.
           PERFORM READ-IT.
           MOVE LSL-PATH-OWNBR  TO LSL-BRNO. 
           MOVE LSL-SELECT-PATH TO E-FILE.
           MOVE LSL1-KEY        TO E-KEYX.

           MOVE LSL-BRNO   TO QLSL-BRNO.
           MOVE LSL-ACCTNO TO QLSL-ACCTNO.
           MOVE LSL-SEQNO TO QLSL-SEQNO.

           EXEC SQL
            SELECT LSLFILE.LSL_BRNO,
                   LSLFILE.LSL_ACCTNO,
                   LSLFILE.LSL_SEQNO,
                   ISNULL(CAST(LSLFILE.LSL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_DATE_SUIT_AUTH
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSLFILE.LSL_DATE_SUIT_AUTH_BY,''),
                   ISNULL(LSLFILE.LSL_ATTORNEY_NET_ASSIGN,''),
                   ISNULL(LSLFILE.LSL_LOCAL_ATTORNEY,''),
                   ISNULL(LSLFILE.LSL_ATTORNEY_ADDRESS,''),
                   LSLFILE.LSL_ATTORNEY_PHONE,
                   ISNULL(CAST(LSLFILE.LSL_DATE_ASSIGNED
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSLFILE.LSL_ATTORNEY_ACCTNO,''),
                   LSLFILE.LSL_PRIN_BAL_SENT,
                   ISNULL(CAST(LSLFILE.LSL_RECEIVED_ACKNOW_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_ATTORNEY_LETTER_SENT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_HOLD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_RESPONSE_RECEIVED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_SUIT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_SUIT_FILED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_SERVICE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSLFILE.LSL_CUSTOMER_SERVED,''),
                   ISNULL(CAST(LSLFILE.LSL_ANSWER_FILED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_COURT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSLFILE.LSL_JUDGMENT_TYPE,''),
                   ISNULL(CAST(LSLFILE.LSL_JUDGMENT_OBTAINED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSLFILE.LSL_JUDGMENT_COPY_FILED,''),
                   LSLFILE.LSL_JUDGMENT_AMT,
                   LSLFILE.LSL_JUDGMENT_PRIN,
                   LSLFILE.LSL_POST_JUDG_INT_RATE,
                   LSLFILE.LSL_JUDG_INT_AMT,
                   LSLFILE.LSL_ATTORNEY_FEES_ALLOWED,
                   LSLFILE.LSL_OTHER_FEES_ALLOWED,
                   LSLFILE.LSL_JUDG_INT_RATE,
                   ISNULL(LSLFILE.LSL_ASSET_USED_IN_SUIT,''),
                   ISNULL(CAST(LSLFILE.LSL_GARN_FILED_DATE
                                                  AS VARCHAR(10)),'0'),
                   LSLFILE.LSL_GARN_TERM_DAYS,
                   ISNULL(CAST(LSLFILE.LSL_INTERROGATORY_SENT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_INTERROGATORY_ANSW_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_FIFA_SENT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_FIFA_ANSWERED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_DATE_TO_RECV_PAY
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LSLFILE.LSL_FIFA_RENEWED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSLFILE.LSL_PAYS_RECV_ATTORNEY,''),
                   ISNULL(CAST(LSLFILE.LSL_DATE_CLOSED
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LSLFILE.LSL_REASON_CLOSED,'')
            INTO :QLSL-REC
            FROM DBO.LSLFILE
            WHERE LSLFILE.LSL_BRNO = :QLSL-BRNO
              AND LSLFILE.LSL_ACCTNO = :QLSL-ACCTNO
              AND LSLFILE.LSL_SEQNO = :QLSL-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LSL-FIELDS.

      *-----------------------------------------------------------------
       READ-LSL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LSL-PATH-OWNBR  TO LSL-BRNO.
           MOVE LSL-SELECT-PATH TO E-FILE.
           MOVE LSL1-KEY        TO E-KEYX.
           INITIALIZE QLSL-REC.

           IF ( LSL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LSL1_CURSOR INTO :QLSL-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LSL-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LSL1-FILE.

           MOVE LSL-PATH-OWNBR  TO LSL-BRNO.
           ACCEPT LSL-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LSL-SELECT-PATH TO E-FILE.
           MOVE LSL1-KEY        TO E-KEYX.
           PERFORM SET-LSL-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LSLFILE
             (LSLFILE.LSL_BRNO,
              LSLFILE.LSL_ACCTNO,
              LSLFILE.LSL_SEQNO,
              LSLFILE.LSL_LTOUCH_DATE,
              LSLFILE.LSL_DATE_SUIT_AUTH,
              LSLFILE.LSL_DATE_SUIT_AUTH_BY,
              LSLFILE.LSL_ATTORNEY_NET_ASSIGN,
              LSLFILE.LSL_LOCAL_ATTORNEY,
              LSLFILE.LSL_ATTORNEY_ADDRESS,
              LSLFILE.LSL_ATTORNEY_PHONE,
              LSLFILE.LSL_DATE_ASSIGNED,
              LSLFILE.LSL_ATTORNEY_ACCTNO,
              LSLFILE.LSL_PRIN_BAL_SENT,
              LSLFILE.LSL_RECEIVED_ACKNOW_DATE,
              LSLFILE.LSL_ATTORNEY_LETTER_SENT_DATE,
              LSLFILE.LSL_HOLD_DATE,
              LSLFILE.LSL_RESPONSE_RECEIVED_DATE,
              LSLFILE.LSL_SUIT_DATE,
              LSLFILE.LSL_SUIT_FILED_DATE,
              LSLFILE.LSL_SERVICE_DATE,
              LSLFILE.LSL_CUSTOMER_SERVED,
              LSLFILE.LSL_ANSWER_FILED_DATE,
              LSLFILE.LSL_COURT_DATE,
              LSLFILE.LSL_JUDGMENT_TYPE,
              LSLFILE.LSL_JUDGMENT_OBTAINED_DATE,
              LSLFILE.LSL_JUDGMENT_COPY_FILED,
              LSLFILE.LSL_JUDGMENT_AMT,
              LSLFILE.LSL_JUDGMENT_PRIN,
              LSLFILE.LSL_POST_JUDG_INT_RATE,
              LSLFILE.LSL_JUDG_INT_AMT,
              LSLFILE.LSL_ATTORNEY_FEES_ALLOWED,
              LSLFILE.LSL_OTHER_FEES_ALLOWED,
              LSLFILE.LSL_JUDG_INT_RATE,
              LSLFILE.LSL_ASSET_USED_IN_SUIT,
              LSLFILE.LSL_GARN_FILED_DATE,
              LSLFILE.LSL_GARN_TERM_DAYS,
              LSLFILE.LSL_INTERROGATORY_SENT_DATE,
              LSLFILE.LSL_INTERROGATORY_ANSW_DATE,
              LSLFILE.LSL_FIFA_SENT_DATE,
              LSLFILE.LSL_FIFA_ANSWERED_DATE,
              LSLFILE.LSL_DATE_TO_RECV_PAY,
              LSLFILE.LSL_FOLLOWUP_DATE,
              LSLFILE.LSL_FIFA_RENEWED_DATE,
              LSLFILE.LSL_PAYS_RECV_ATTORNEY,
              LSLFILE.LSL_DATE_CLOSED,
              LSLFILE.LSL_REASON_CLOSED)
            VALUES
             (:QLSL-BRNO,
              :QLSL-ACCTNO,
              :QLSL-SEQNO,
              NULLIF(:QLSL-LTOUCH-DATE,'1900-01-01'),
              NULLIF(:QLSL-DATE-SUIT-AUTH,'1900-01-01'),
              :QLSL-DATE-SUIT-AUTH-BY,
              :QLSL-ATTORNEY-NET-ASSIGN,
              :QLSL-LOCAL-ATTORNEY,
              :QLSL-ATTORNEY-ADDRESS,
              :QLSL-ATTORNEY-PHONE,
              NULLIF(:QLSL-DATE-ASSIGNED,'1900-01-01'),
              :QLSL-ATTORNEY-ACCTNO,
              :QLSL-PRIN-BAL-SENT,
              NULLIF(:QLSL-RECEIVED-ACKNOW-DATE,'1900-01-01'),
              NULLIF(:QLSL-ATTORNEY-LETTER-SENT-DATE,'1900-01-01'),
              NULLIF(:QLSL-HOLD-DATE,'1900-01-01'),
              NULLIF(:QLSL-RESPONSE-RECEIVED-DATE,'1900-01-01'),
              NULLIF(:QLSL-SUIT-DATE,'1900-01-01'),
              NULLIF(:QLSL-SUIT-FILED-DATE,'1900-01-01'),
              NULLIF(:QLSL-SERVICE-DATE,'1900-01-01'),
              :QLSL-CUSTOMER-SERVED,
              NULLIF(:QLSL-ANSWER-FILED-DATE,'1900-01-01'),
              NULLIF(:QLSL-COURT-DATE,'1900-01-01'),
              :QLSL-JUDGMENT-TYPE,
              NULLIF(:QLSL-JUDGMENT-OBTAINED-DATE,'1900-01-01'),
              :QLSL-JUDGMENT-COPY-FILED,
              :QLSL-JUDGMENT-AMT,
              :QLSL-JUDGMENT-PRIN,
              :QLSL-POST-JUDG-INT-RATE,
              :QLSL-JUDG-INT-AMT,
              :QLSL-ATTORNEY-FEES-ALLOWED,
              :QLSL-OTHER-FEES-ALLOWED,
              :QLSL-JUDG-INT-RATE,
              :QLSL-ASSET-USED-IN-SUIT,
              NULLIF(:QLSL-GARN-FILED-DATE,'1900-01-01'),
              :QLSL-GARN-TERM-DAYS,
              NULLIF(:QLSL-INTERROGATORY-SENT-DATE,'1900-01-01'),
              NULLIF(:QLSL-INTERROGATORY-ANSW-DATE,'1900-01-01'),
              NULLIF(:QLSL-FIFA-SENT-DATE,'1900-01-01'),
              NULLIF(:QLSL-FIFA-ANSWERED-DATE,'1900-01-01'),
              NULLIF(:QLSL-DATE-TO-RECV-PAY,'1900-01-01'),
              NULLIF(:QLSL-FOLLOWUP-DATE,'1900-01-01'),
              NULLIF(:QLSL-FIFA-RENEWED-DATE,'1900-01-01'),
              :QLSL-PAYS-RECV-ATTORNEY,
              NULLIF(:QLSL-DATE-CLOSED,'1900-01-01'),
              :QLSL-REASON-CLOSED)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LSL1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LSL1-FILE.

           MOVE LSL-PATH-OWNBR  TO LSL-BRNO.
           ACCEPT LSL-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LSL-SELECT-PATH TO E-FILE.
           MOVE LSL1-KEY        TO E-KEYX.
           PERFORM SET-LSL-FIELDS.

           EXEC SQL
            UPDATE DBO.LSLFILE
             SET
              LSLFILE.LSL_BRNO = :QLSL-BRNO,
              LSLFILE.LSL_ACCTNO = :QLSL-ACCTNO,
              LSLFILE.LSL_SEQNO = :QLSL-SEQNO,
              LSLFILE.LSL_LTOUCH_DATE =
                 NULLIF(:QLSL-LTOUCH-DATE,'1900-01-01'),
              LSLFILE.LSL_DATE_SUIT_AUTH =
                 NULLIF(:QLSL-DATE-SUIT-AUTH,'1900-01-01'),
              LSLFILE.LSL_DATE_SUIT_AUTH_BY =
                   :QLSL-DATE-SUIT-AUTH-BY,
              LSLFILE.LSL_ATTORNEY_NET_ASSIGN =
                   :QLSL-ATTORNEY-NET-ASSIGN,
              LSLFILE.LSL_LOCAL_ATTORNEY = :QLSL-LOCAL-ATTORNEY,
              LSLFILE.LSL_ATTORNEY_ADDRESS =
                   :QLSL-ATTORNEY-ADDRESS,
              LSLFILE.LSL_ATTORNEY_PHONE = :QLSL-ATTORNEY-PHONE,
              LSLFILE.LSL_DATE_ASSIGNED =
                 NULLIF(:QLSL-DATE-ASSIGNED,'1900-01-01'),
              LSLFILE.LSL_ATTORNEY_ACCTNO = :QLSL-ATTORNEY-ACCTNO,
              LSLFILE.LSL_PRIN_BAL_SENT = :QLSL-PRIN-BAL-SENT,
              LSLFILE.LSL_RECEIVED_ACKNOW_DATE =
                 NULLIF(:QLSL-RECEIVED-ACKNOW-DATE,'1900-01-01'),
              LSLFILE.LSL_ATTORNEY_LETTER_SENT_DATE =
                 NULLIF(:QLSL-ATTORNEY-LETTER-SENT-DATE,'1900-01-01'),
              LSLFILE.LSL_HOLD_DATE =
                 NULLIF(:QLSL-HOLD-DATE,'1900-01-01'),
              LSLFILE.LSL_RESPONSE_RECEIVED_DATE =
                 NULLIF(:QLSL-RESPONSE-RECEIVED-DATE,'1900-01-01'),
              LSLFILE.LSL_SUIT_DATE =
                 NULLIF(:QLSL-SUIT-DATE,'1900-01-01'),
              LSLFILE.LSL_SUIT_FILED_DATE =
                 NULLIF(:QLSL-SUIT-FILED-DATE,'1900-01-01'),
              LSLFILE.LSL_SERVICE_DATE =
                 NULLIF(:QLSL-SERVICE-DATE,'1900-01-01'),
              LSLFILE.LSL_CUSTOMER_SERVED = :QLSL-CUSTOMER-SERVED,
              LSLFILE.LSL_ANSWER_FILED_DATE =
                 NULLIF(:QLSL-ANSWER-FILED-DATE,'1900-01-01'),
              LSLFILE.LSL_COURT_DATE =
                 NULLIF(:QLSL-COURT-DATE,'1900-01-01'),
              LSLFILE.LSL_JUDGMENT_TYPE = :QLSL-JUDGMENT-TYPE,
              LSLFILE.LSL_JUDGMENT_OBTAINED_DATE =
                 NULLIF(:QLSL-JUDGMENT-OBTAINED-DATE,'1900-01-01'),
              LSLFILE.LSL_JUDGMENT_COPY_FILED =
                   :QLSL-JUDGMENT-COPY-FILED,
              LSLFILE.LSL_JUDGMENT_AMT = :QLSL-JUDGMENT-AMT,
              LSLFILE.LSL_JUDGMENT_PRIN = :QLSL-JUDGMENT-PRIN,
              LSLFILE.LSL_POST_JUDG_INT_RATE =
                   :QLSL-POST-JUDG-INT-RATE,
              LSLFILE.LSL_JUDG_INT_AMT = :QLSL-JUDG-INT-AMT,
              LSLFILE.LSL_ATTORNEY_FEES_ALLOWED =
                   :QLSL-ATTORNEY-FEES-ALLOWED,
              LSLFILE.LSL_OTHER_FEES_ALLOWED =
                   :QLSL-OTHER-FEES-ALLOWED,
              LSLFILE.LSL_JUDG_INT_RATE = :QLSL-JUDG-INT-RATE,
              LSLFILE.LSL_ASSET_USED_IN_SUIT =
                   :QLSL-ASSET-USED-IN-SUIT,
              LSLFILE.LSL_GARN_FILED_DATE =
                 NULLIF(:QLSL-GARN-FILED-DATE,'1900-01-01'),
              LSLFILE.LSL_GARN_TERM_DAYS = :QLSL-GARN-TERM-DAYS,
              LSLFILE.LSL_INTERROGATORY_SENT_DATE =
                 NULLIF(:QLSL-INTERROGATORY-SENT-DATE,'1900-01-01'),
              LSLFILE.LSL_INTERROGATORY_ANSW_DATE =
                 NULLIF(:QLSL-INTERROGATORY-ANSW-DATE,'1900-01-01'),
              LSLFILE.LSL_FIFA_SENT_DATE =
                 NULLIF(:QLSL-FIFA-SENT-DATE,'1900-01-01'),
              LSLFILE.LSL_FIFA_ANSWERED_DATE =
                 NULLIF(:QLSL-FIFA-ANSWERED-DATE,'1900-01-01'),
              LSLFILE.LSL_DATE_TO_RECV_PAY =
                 NULLIF(:QLSL-DATE-TO-RECV-PAY,'1900-01-01'),
              LSLFILE.LSL_FOLLOWUP_DATE =
                 NULLIF(:QLSL-FOLLOWUP-DATE,'1900-01-01'),
              LSLFILE.LSL_FIFA_RENEWED_DATE =
                 NULLIF(:QLSL-FIFA-RENEWED-DATE,'1900-01-01'),
              LSLFILE.LSL_PAYS_RECV_ATTORNEY =
                   :QLSL-PAYS-RECV-ATTORNEY,
              LSLFILE.LSL_DATE_CLOSED =
                 NULLIF(:QLSL-DATE-CLOSED,'1900-01-01'),
              LSLFILE.LSL_REASON_CLOSED = :QLSL-REASON-CLOSED
             WHERE LSLFILE.LSL_BRNO = :QLSL-BRNO
               AND LSLFILE.LSL_ACCTNO = :QLSL-ACCTNO
               AND LSLFILE.LSL_SEQNO = :QLSL-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LSL1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LSL1-FILE.

           MOVE LSL-PATH-OWNBR  TO LSL-BRNO.
           PERFORM DELETE-IT.
           MOVE LSL-SELECT-PATH TO E-FILE.
           MOVE LSL1-KEY        TO E-KEYX.
           PERFORM SET-LSL-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LSLFILE
             WHERE LSLFILE.LSL_BRNO = :QLSL-BRNO
               AND LSLFILE.LSL_ACCTNO = :QLSL-ACCTNO
               AND LSLFILE.LSL_SEQNO = :QLSL-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LSL1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LSL1-FILE.

           IF ( LSL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LSL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LSL1-CURSOR-STAT.

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
              DESTROY IOLSLI-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
