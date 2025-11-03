       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IONORN.
       AUTHOR.        BAH
       DATE-WRITTEN.  04-09-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IONORN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : NOFILE
      * TABLE DESC    : NOTICE FILE
      * OUTPUT FILE   : XPC/IONORN.PRE
      * USED FOR      : LIBLP/LPWSNO.CPY  --- QNO-REC
      *                 LIBLP/LPNO1R.CPY  --- READ
      *                 LIBLP/LPNO1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0409 ***NEW***
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
           "@(#)IO/IONORN.CBL S34 07/22/21-10:37:39 >".

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
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LPWSNO.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QNO1-WHERE-BEG-KEY.
           03  QNO1-WBEG-BRNO             PIC 9(04).
           03  QNO1-WBEG-ACCTNO           PIC 9(08).
           03  QNO1-WBEG-FMNO             PIC 9(04).
 
       01  QNO1-WHERE-END-KEY.
           03  QNO1-WEND-BRNO             PIC 9(04).
           03  QNO1-WEND-ACCTNO           PIC 9(08).
           03  QNO1-WEND-FMNO             PIC 9(04).
 
       01  QNO-REC.
           03  QNO-BRNO           PIC 9(4).
           03  QNO-ACCTNO         PIC 9(8).
           03  QNO-FMNO           PIC 9(4).
           03  QNO-LTOUCH-DATE        PIC X(10).
           03  QNO-TDUE-DATE          PIC X(10).
           03  QNO-TDUELC             PIC S9(7)V99.
           03  QNO-TDUEAMT            PIC S9(7)V99.
           03  QNO-TDUEPRIN           PIC S9(7)V99.
           03  QNO-TDUETBAL           PIC S9(7)V99.
           03  QNO-TDUEPOFF           PIC S9(7)V99.
           03  QNO-LDUE-DATE          PIC X(10).
           03  QNO-LDUELC             PIC S9(7)V99.
           03  QNO-LDUEAMT            PIC S9(7)V99.
           03  QNO-DATE-PAID-LAST     PIC X(10).
           03  QNO-AMT-PAID-LAST      PIC S9(7)V99.
           03  QNO-LSTLPKEY           PIC X(17).
           03  QNO-NXDUE-DATE         PIC X(10).
           03  QNO-NXDUEAMT           PIC S9(7)V99.
           03  QNO-NXDUELC            PIC S9(7)V99.
           03  QNO-TRUCONTR           PIC 9(4).
           03  QNO-RECENCY            PIC 9(4).
           03  QNO-PDTH-DATE          PIC X(10).
           03  QNO-LCPDTH-DATE        PIC X(10).
           03  QNO-PRINTED            PIC X.
           03  QNO-LCHG-INTDUE        PIC S9(7)V99.
           03  QNO-AVAILCREDIT        PIC S9(7)V99.
           03  QNO-TDUEDELINQ         PIC S9(7)V99.
           03  QNO-DEFERMENT          PIC S9(7)V99.
           03  QNO-TRUE-DUE-DATE      PIC X(10).
           03  QNO-SCHED-PAYMENT      PIC S9(7)V99.
           03  QNO-TDUECONTR          PIC S9(7)V99.
           03  QNO-LDUECONTR          PIC S9(7)V99.
           03  QNO-NXDUECONTR         PIC S9(7)V99.
           03  QNO-CURRENT-AVAIL      PIC S9(7)V99.
           03  QNO-LATE-CHG           PIC S9(7)V99.
           03  QNO-WAGE-ASSIGN-DELINQ PIC S9(7)V99.
           03  QNO-PAST-DUE-DAYS      PIC 9(4).
           03  QNO-RECDEL             PIC S9(5).

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
           03  BEG-FMNO                PIC 9(04).
           03  END-FMNO                PIC 9(04).

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
           03  FILLER PIC X(30) VALUE "BRNO    ACCTNO    FMNO    TDUE".
           03  FILLER PIC X(30) VALUE "DATE     TDUEAMT      NXDUEDAT".
           03  FILLER PIC X(30) VALUE "E    NXDUEAMT".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.
      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-NO-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-NO-ACCTNO                  PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-NO-FMNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-NO-TDUE-DATE               PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-NO-TDUEAMT                 PIC Z(6)9.99.
           03  FILLER                         PIC X(03).
           03  D01-NO-NXDUE-DATE               PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-NO-NXDUEAMT                 PIC Z(6)9.99.

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
       COPY "LIBIO/IONORN_DEF.CPY".
       COPY "LIBIO/IONORN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IONORN_SCN.CPY".

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
           MOVE "IONORN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IONORN-QUEUE-POS  TO Q-POS.
           MOVE IONORN-COPIES-POS TO C-POS.
           MOVE IONORN-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-FMNO.

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

           GO TO ENTER-BEG-FMNO.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN080BEG-ACCTNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-ACCTNO.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO." TO VDU.
           MOVE 99999999            TO VDUNUM0  END-ACCTNO.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-FMNO.

           MOVE "ENNN040BEG-FMNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-FMNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-FMNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-FMNO.

      *-----------------------------------------------------------------
       ENTER-END-FMNO.

           MOVE "ENNN040END-FMNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-FMNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-FMNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-FMNO.

           IF ( BEG-FMNO > END-FMNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-FMNO.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-FMNO.

           MOVE "SNNN040BEG-FMNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-FMNO.
           PERFORM SCREENIO.

           MOVE "SNNN040END-FMNO." TO VDU.
           MOVE 9999                TO VDUNUM0  END-FMNO.
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

           MOVE "SNNN040BEG-FMNO."       TO VDU.
           MOVE BEG-FMNO                 TO VDUNUM0.
           PERFORM SCREENIO.

            MOVE "SNNN040END-FMNO."       TO VDU.
           MOVE END-FMNO                 TO VDUNUM0.
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

           MOVE "B"                        TO NO-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QNO1-WBEG-BRNO.
           MOVE BR-NO     TO QNO1-WEND-BRNO.

           MOVE BEG-ACCTNO TO QNO1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QNO1-WEND-ACCTNO.

           MOVE BEG-FMNO  TO QNO1-WBEG-FMNO.
           MOVE END-FMNO  TO QNO1-WEND-FMNO.


      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-NO1-FILE.
           PERFORM START-NO1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-NO1-FILE-NEXT.
           IF ( IO-BAD ) OR ( NO-BRNO NOT = NO-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( NO-ACCTNO < BEG-ACCTNO )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( NO-ACCTNO > END-ACCTNO )
              GO TO PROCESS-BRANCH-MODULE-END.

           IF NO-FMNO < BEG-FMNO OR > END-FMNO
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE NO-BRNO                    TO D01-NO-BRNO.
           MOVE NO-ACCTNO                  TO D01-NO-ACCTNO.
           MOVE NO-FMNO             TO D01-NO-FMNO.
           MOVE NO-TDUE-DATE        TO D01-NO-TDUE-DATE.
           MOVE NO-TDUEAMT          TO D01-NO-TDUEAMT.
           MOVE NO-NXDUE-DATE       TO D01-NO-NXDUE-DATE.
           MOVE NO-NXDUEAMT         TO D01-NO-NXDUEAMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-NO1-FILE.

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

           MOVE NO1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING FMNO               : "    TO R01-PROMPT.
           MOVE BEG-FMNO                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    FMNO               : "    TO R01-PROMPT.
           MOVE END-FMNO                             TO R01-NUMERIC.
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
           MOVE "SQL TABLE IO UTILITY - NOFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IONORN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IONORN-SYSDATE.
           DISPLAY IONORN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IONORN-FORM-FIELDS TO WR-BUF.
           MOVE IONORN-FORM-LIST TO WR-LIST.
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

           MOVE IONORN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IONORN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IONORN_EVA.CPY".
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
      *    LOAD NO-REC FROM QNO-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-NO-FIELDS.
           MOVE QNO-BRNO TO NO-BRNO.
           MOVE QNO-ACCTNO TO NO-ACCTNO.
           MOVE QNO-FMNO TO NO-FMNO.

           IF ( QNO-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO NO-LTOUCH-DATE
           ELSE
              MOVE QNO-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO NO-LTOUCH-DATE.


           IF ( QNO-TDUE-DATE = "1900-01-01" )
              MOVE ZEROES TO NO-TDUE-DATE
           ELSE
              MOVE QNO-TDUE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO NO-TDUE-DATE.

           MOVE QNO-TDUELC TO NO-TDUELC.
           MOVE QNO-TDUEAMT TO NO-TDUEAMT.
           MOVE QNO-TDUEPRIN TO NO-TDUEPRIN.
           MOVE QNO-TDUETBAL TO NO-TDUETBAL.
           MOVE QNO-TDUEPOFF TO NO-TDUEPOFF.

           IF ( QNO-LDUE-DATE = "1900-01-01" )
              MOVE ZEROES TO NO-LDUE-DATE
           ELSE
              MOVE QNO-LDUE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO NO-LDUE-DATE.

           MOVE QNO-LDUELC TO NO-LDUELC.
           MOVE QNO-LDUEAMT TO NO-LDUEAMT.

           IF ( QNO-DATE-PAID-LAST = "1900-01-01" )
              MOVE ZEROES TO NO-DATE-PAID-LAST
           ELSE
              MOVE QNO-DATE-PAID-LAST TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO NO-DATE-PAID-LAST.

           MOVE QNO-AMT-PAID-LAST TO NO-AMT-PAID-LAST.
           MOVE QNO-LSTLPKEY TO NO-LSTLPKEY.

           IF ( QNO-NXDUE-DATE = "1900-01-01" )
              MOVE ZEROES TO NO-NXDUE-DATE
           ELSE
              MOVE QNO-NXDUE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO NO-NXDUE-DATE.

           MOVE QNO-NXDUEAMT TO NO-NXDUEAMT.
           MOVE QNO-NXDUELC TO NO-NXDUELC.
           MOVE QNO-TRUCONTR TO NO-TRUCONTR.
           MOVE QNO-RECENCY TO NO-RECENCY.

           IF ( QNO-PDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO NO-PDTH-DATE
           ELSE
              MOVE QNO-PDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO NO-PDTH-DATE.


           IF ( QNO-LCPDTH-DATE = "1900-01-01" )
              MOVE ZEROES TO NO-LCPDTH-DATE
           ELSE
              MOVE QNO-LCPDTH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO NO-LCPDTH-DATE.

           MOVE QNO-PRINTED TO NO-PRINTED.
           MOVE QNO-LCHG-INTDUE TO NO-LCHG-INTDUE.
           MOVE QNO-AVAILCREDIT TO NO-AVAILCREDIT.
           MOVE QNO-TDUEDELINQ TO NO-TDUEDELINQ.
           MOVE QNO-DEFERMENT TO NO-DEFERMENT.

           IF ( QNO-TRUE-DUE-DATE = "1900-01-01" )
              MOVE ZEROES TO NO-TRUE-DUE-DATE
           ELSE
              MOVE QNO-TRUE-DUE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO NO-TRUE-DUE-DATE.

           MOVE QNO-SCHED-PAYMENT TO NO-SCHED-PAYMENT.
           MOVE QNO-TDUECONTR TO NO-TDUECONTR.
           MOVE QNO-LDUECONTR TO NO-LDUECONTR.
           MOVE QNO-NXDUECONTR TO NO-NXDUECONTR.
           MOVE QNO-CURRENT-AVAIL TO NO-CURRENT-AVAIL.
           MOVE QNO-LATE-CHG TO NO-LATE-CHG.
           MOVE QNO-WAGE-ASSIGN-DELINQ TO NO-WAGE-ASSIGN-DELINQ.
           MOVE QNO-PAST-DUE-DAYS TO NO-PAST-DUE-DAYS.
           MOVE QNO-RECDEL TO NO-RECDEL.
 
      *-----------------------------------------------------------------
      *    LOAD QNO-REC FROM NO-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-NO-FIELDS.
           MOVE NO-BRNO TO QNO-BRNO.
           MOVE NO-ACCTNO TO QNO-ACCTNO.
           MOVE NO-FMNO TO QNO-FMNO.

           IF ( NO-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QNO-LTOUCH-DATE
           ELSE
              MOVE NO-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QNO-LTOUCH-DATE.


           IF ( NO-TDUE-DATE = ZEROES )
              MOVE "1900-01-01" TO QNO-TDUE-DATE
           ELSE
              MOVE NO-TDUE-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QNO-TDUE-DATE.

           MOVE NO-TDUELC TO QNO-TDUELC.
           MOVE NO-TDUEAMT TO QNO-TDUEAMT.
           MOVE NO-TDUEPRIN TO QNO-TDUEPRIN.
           MOVE NO-TDUETBAL TO QNO-TDUETBAL.
           MOVE NO-TDUEPOFF TO QNO-TDUEPOFF.

           IF ( NO-LDUE-DATE = ZEROES )
              MOVE "1900-01-01" TO QNO-LDUE-DATE
           ELSE
              MOVE NO-LDUE-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QNO-LDUE-DATE.

           MOVE NO-LDUELC TO QNO-LDUELC.
           MOVE NO-LDUEAMT TO QNO-LDUEAMT.

           IF ( NO-DATE-PAID-LAST = ZEROES )
              MOVE "1900-01-01" TO QNO-DATE-PAID-LAST
           ELSE
              MOVE NO-DATE-PAID-LAST TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QNO-DATE-PAID-LAST.

           MOVE NO-AMT-PAID-LAST TO QNO-AMT-PAID-LAST.
           MOVE NO-LSTLPKEY TO QNO-LSTLPKEY.

           IF ( NO-NXDUE-DATE = ZEROES )
              MOVE "1900-01-01" TO QNO-NXDUE-DATE
           ELSE
              MOVE NO-NXDUE-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QNO-NXDUE-DATE.

           MOVE NO-NXDUEAMT TO QNO-NXDUEAMT.
           MOVE NO-NXDUELC TO QNO-NXDUELC.
           MOVE NO-TRUCONTR TO QNO-TRUCONTR.
           MOVE NO-RECENCY TO QNO-RECENCY.

           IF ( NO-PDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QNO-PDTH-DATE
           ELSE
              MOVE NO-PDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QNO-PDTH-DATE.


           IF ( NO-LCPDTH-DATE = ZEROES )
              MOVE "1900-01-01" TO QNO-LCPDTH-DATE
           ELSE
              MOVE NO-LCPDTH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QNO-LCPDTH-DATE.

           MOVE NO-PRINTED TO QNO-PRINTED.
           MOVE NO-LCHG-INTDUE TO QNO-LCHG-INTDUE.
           MOVE NO-AVAILCREDIT TO QNO-AVAILCREDIT.
           MOVE NO-TDUEDELINQ TO QNO-TDUEDELINQ.
           MOVE NO-DEFERMENT TO QNO-DEFERMENT.

           IF ( NO-TRUE-DUE-DATE = ZEROES )
              MOVE "1900-01-01" TO QNO-TRUE-DUE-DATE
           ELSE
              MOVE NO-TRUE-DUE-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QNO-TRUE-DUE-DATE.

           MOVE NO-SCHED-PAYMENT TO QNO-SCHED-PAYMENT.
           MOVE NO-TDUECONTR TO QNO-TDUECONTR.
           MOVE NO-LDUECONTR TO QNO-LDUECONTR.
           MOVE NO-NXDUECONTR TO QNO-NXDUECONTR.
           MOVE NO-CURRENT-AVAIL TO QNO-CURRENT-AVAIL.
           MOVE NO-LATE-CHG TO QNO-LATE-CHG.
           MOVE NO-WAGE-ASSIGN-DELINQ TO QNO-WAGE-ASSIGN-DELINQ.
           MOVE NO-PAST-DUE-DAYS TO QNO-PAST-DUE-DAYS.
           MOVE NO-RECDEL TO QNO-RECDEL.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-NO1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( NO-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( NO-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO NO-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO NO-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO NO-ALL-DATA
              MOVE NO-ALL-PATH             TO NO-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( NO-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO NO-ALL-BASE
              MOVE FILPATH-MACHINE         TO NO-ALL-MACHINE
              MOVE FILPATH-DATA            TO NO-ALL-DATA
              MOVE NO-ALL-PATH             TO NO-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-NOFILE NOT = "S" )
              MOVE NO-PATH-BEG             TO NO-SELECT-PATH-BEG
              MOVE NO-PATH-FILE            TO NO-SELECT-PATH-FILE
           ELSE
              MOVE NO-PATH-SQL             TO NO-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-NO1-FILE.
           PERFORM LOAD-NO1-FILE.
           PERFORM OPEN-IT.
           MOVE NO-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-NO1-FILE.
           PERFORM START-IT.
           MOVE NO-PATH-OWNBR  TO NO-BRNO.
           MOVE NO-SELECT-PATH TO E-FILE.
           MOVE NO1-KEY        TO E-KEYX.

           IF ( NO1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-NO1-FILE.

           IF ( QNO1-WEND-FMNO = ZEROES    ) OR
              ( QNO1-WEND-FMNO NOT NUMERIC )
              MOVE NO-PATH-OWNBR        TO QNO1-WBEG-BRNO
              MOVE NO-PATH-OWNBR        TO QNO1-WEND-BRNO
              MOVE NO-ACCTNO            TO QNO1-WBEG-ACCTNO
              MOVE ALL "9"              TO QNO1-WEND-ACCTNO
              MOVE NO-FMNO              TO QNO1-WBEG-FMNO
              MOVE ALL "9"              TO QNO1-WEND-FMNO.

           EXEC SQL
            DECLARE NO1_CURSOR CURSOR FOR
            SELECT NOFILE.NO_BRNO,
                   NOFILE.NO_ACCTNO,
                   NOFILE.NO_FMNO,
                   ISNULL(CAST(NOFILE.NO_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(NOFILE.NO_TDUE_DATE
                                                  AS VARCHAR(10)),'0'),
                   NOFILE.NO_TDUELC,
                   NOFILE.NO_TDUEAMT,
                   NOFILE.NO_TDUEPRIN,
                   NOFILE.NO_TDUETBAL,
                   NOFILE.NO_TDUEPOFF,
                   ISNULL(CAST(NOFILE.NO_LDUE_DATE
                                                  AS VARCHAR(10)),'0'),
                   NOFILE.NO_LDUELC,
                   NOFILE.NO_LDUEAMT,
                   ISNULL(CAST(NOFILE.NO_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   NOFILE.NO_AMT_PAID_LAST,
                   ISNULL(NOFILE.NO_LSTLPKEY,''),
                   ISNULL(CAST(NOFILE.NO_NXDUE_DATE
                                                  AS VARCHAR(10)),'0'),
                   NOFILE.NO_NXDUEAMT,
                   NOFILE.NO_NXDUELC,
                   NOFILE.NO_TRUCONTR,
                   NOFILE.NO_RECENCY,
                   ISNULL(CAST(NOFILE.NO_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(NOFILE.NO_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(NOFILE.NO_PRINTED,''),
                   NOFILE.NO_LCHG_INTDUE,
                   NOFILE.NO_AVAILCREDIT,
                   NOFILE.NO_TDUEDELINQ,
                   NOFILE.NO_DEFERMENT,
                   ISNULL(CAST(NOFILE.NO_TRUE_DUE_DATE
                                                  AS VARCHAR(10)),'0'),
                   NOFILE.NO_SCHED_PAYMENT,
                   NOFILE.NO_TDUECONTR,
                   NOFILE.NO_LDUECONTR,
                   NOFILE.NO_NXDUECONTR,
                   NOFILE.NO_CURRENT_AVAIL,
                   NOFILE.NO_LATE_CHG,
                   NOFILE.NO_WAGE_ASSIGN_DELINQ,
                   NOFILE.NO_PAST_DUE_DAYS,
                   NOFILE.NO_RECDEL
            FROM DBO.NOFILE
            WHERE NOFILE.NO_BRNO >= :QNO1-WBEG-BRNO
              AND NOFILE.NO_BRNO <= :QNO1-WEND-BRNO
              AND NOFILE.NO_ACCTNO >= :QNO1-WBEG-ACCTNO
              AND NOFILE.NO_ACCTNO <= :QNO1-WEND-ACCTNO
              AND NOFILE.NO_FMNO >= :QNO1-WBEG-FMNO
              AND NOFILE.NO_FMNO <= :QNO1-WEND-FMNO
            ORDER BY NOFILE.NO_BRNO,
                     NOFILE.NO_ACCTNO,
                     NOFILE.NO_FMNO
           END-EXEC.

           EXEC SQL
               OPEN NO1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QNO1-WBEG-BRNO 
                          QNO1-WEND-BRNO
                          QNO1-WBEG-ACCTNO
                          QNO1-WEND-ACCTNO
                          QNO1-WBEG-FMNO
                          QNO1-WEND-FMNO.

           MOVE STAT---GOOD TO NO1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-NO1-FILE.
           PERFORM READ-IT.
           MOVE NO-PATH-OWNBR  TO NO-BRNO. 
           MOVE NO-SELECT-PATH TO E-FILE.
           MOVE NO1-KEY        TO E-KEYX.

           MOVE NO-BRNO   TO QNO-BRNO.
           MOVE NO-ACCTNO TO QNO-ACCTNO.
           MOVE NO-FMNO   TO QNO-FMNO.

           EXEC SQL
            SELECT NOFILE.NO_BRNO,
                   NOFILE.NO_ACCTNO,
                   NOFILE.NO_FMNO,
                   ISNULL(CAST(NOFILE.NO_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(NOFILE.NO_TDUE_DATE
                                                  AS VARCHAR(10)),'0'),
                   NOFILE.NO_TDUELC,
                   NOFILE.NO_TDUEAMT,
                   NOFILE.NO_TDUEPRIN,
                   NOFILE.NO_TDUETBAL,
                   NOFILE.NO_TDUEPOFF,
                   ISNULL(CAST(NOFILE.NO_LDUE_DATE
                                                  AS VARCHAR(10)),'0'),
                   NOFILE.NO_LDUELC,
                   NOFILE.NO_LDUEAMT,
                   ISNULL(CAST(NOFILE.NO_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   NOFILE.NO_AMT_PAID_LAST,
                   ISNULL(NOFILE.NO_LSTLPKEY,''),
                   ISNULL(CAST(NOFILE.NO_NXDUE_DATE
                                                  AS VARCHAR(10)),'0'),
                   NOFILE.NO_NXDUEAMT,
                   NOFILE.NO_NXDUELC,
                   NOFILE.NO_TRUCONTR,
                   NOFILE.NO_RECENCY,
                   ISNULL(CAST(NOFILE.NO_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(NOFILE.NO_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(NOFILE.NO_PRINTED,''),
                   NOFILE.NO_LCHG_INTDUE,
                   NOFILE.NO_AVAILCREDIT,
                   NOFILE.NO_TDUEDELINQ,
                   NOFILE.NO_DEFERMENT,
                   ISNULL(CAST(NOFILE.NO_TRUE_DUE_DATE
                                                  AS VARCHAR(10)),'0'),
                   NOFILE.NO_SCHED_PAYMENT,
                   NOFILE.NO_TDUECONTR,
                   NOFILE.NO_LDUECONTR,
                   NOFILE.NO_NXDUECONTR,
                   NOFILE.NO_CURRENT_AVAIL,
                   NOFILE.NO_LATE_CHG,
                   NOFILE.NO_WAGE_ASSIGN_DELINQ,
                   NOFILE.NO_PAST_DUE_DAYS,
                   NOFILE.NO_RECDEL
            INTO :QNO-REC
            FROM DBO.NOFILE
            WHERE NOFILE.NO_BRNO = :QNO-BRNO
              AND NOFILE.NO_ACCTNO = :QNO-ACCTNO
              AND NOFILE.NO_FMNO = :QNO-FMNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-NO1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-NO-FIELDS.

      *-----------------------------------------------------------------
       READ-NO1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE NO-PATH-OWNBR  TO NO-BRNO.
           MOVE NO-SELECT-PATH TO E-FILE.
           MOVE NO1-KEY        TO E-KEYX.
           INITIALIZE QNO-REC.

           IF ( NO1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH NO1_CURSOR INTO :QNO-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-NO1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-NO-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-NO1-FILE.

           IF ( NO1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE NO1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO NO1-CURSOR-STAT.

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
              DESTROY IONORN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
