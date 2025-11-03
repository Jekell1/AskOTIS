       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOTRWR.
       AUTHOR.        BAH
       DATE-WRITTEN.  04-14-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOTRWR.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : TRWFILE 
      * TABLE DESC    : TRANSACTION CASH BALANCING FILE
      * OUTPUT FILE   : XPC/IOTRWR.PRE
      * USED FOR      : LIBLP/LPWSTRW.CPY  --- QTRW-REC
      *                 LIBLP/LPTRW1R.CPY  --- READ
      *                 LIBLP/LPTRW1RN.CPY --- READ/READ-NEXT/START
      *                 LIBLP/LPTRW2RN.CPY --- READ/READ-NEXT/START
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./IO/IOTRWR.CBL S34 05/05/21-14:20:48 >".

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
       COPY "LIBLP/LP01TRW.CPY".
       COPY "LIBLP/LPWSTRW.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QTRW1-WHERE-BEG-KEY.
           03  QTRW1-WBEG-BRNO             PIC 9(4).
           03  QTRW1-WBEG-DATE             PIC X(10).
           03  QTRW1-WBEG-TILLNO           PIC 9(02).

       01  QTRW1-WHERE-END-KEY.
           03  QTRW1-WEND-BRNO             PIC 9(4).
           03  QTRW1-WEND-DATE             PIC X(10).
           03  QTRW1-WEND-TILLNO           PIC 9(02).

       01  QTRW2-WHERE-BEG-KEY.
           03  QTRW2-WBEG-DATE             PIC X(10).
           03  QTRW2-WBEG-BRNO             PIC 9(4).
           03  QTRW2-WBEG-TILLNO           PIC 9(02).

       01  QTRW2-WHERE-END-KEY.
           03  QTRW2-WEND-DATE             PIC X(10).
           03  QTRW2-WEND-BRNO             PIC 9(4).
           03  QTRW2-WEND-TILLNO           PIC 9(02).
 
       01  QTRW-REC.
           03  QTW-BRNO             PIC 9(4).
           03  QTW-DATE             PIC X(10).
           03  QTW-TILLNO           PIC 9(2).
           03  QTW-LTOUCH-DATE          PIC X(10).
           03  QTW-POSTNAME-1         PIC X(08).
           03  QTW-TIME-1             PIC 9(04).
           03  QTW-AMOUNT-1           PIC S9(07)V99.
           03  QTW-OVERSHORT-1        PIC S9(07)V99.
           03  QTW-POSTNAME-2         PIC X(08).
           03  QTW-TIME-2             PIC 9(04).
           03  QTW-AMOUNT-2           PIC S9(07)V99.
           03  QTW-OVERSHORT-2        PIC S9(07)V99.
           03  QTW-POSTNAME-3         PIC X(08).
           03  QTW-TIME-3             PIC 9(04).
           03  QTW-AMOUNT-3           PIC S9(07)V99.
           03  QTW-OVERSHORT-3        PIC S9(07)V99.
           03  QTW-POSTNAME-4         PIC X(08).
           03  QTW-TIME-4             PIC 9(04).
           03  QTW-AMOUNT-4           PIC S9(07)V99.
           03  QTW-OVERSHORT-4        PIC S9(07)V99.
           03  QTW-POSTNAME-5         PIC X(08).
           03  QTW-TIME-5             PIC 9(04).
           03  QTW-AMOUNT-5           PIC S9(07)V99.
           03  QTW-OVERSHORT-5        PIC S9(07)V99.
           03  QTW-POSTNAME-6         PIC X(08).
           03  QTW-TIME-6             PIC 9(04).
           03  QTW-AMOUNT-6           PIC S9(07)V99.
           03  QTW-OVERSHORT-6        PIC S9(07)V99.
           03  QTW-POSTNAME-7         PIC X(08).
           03  QTW-TIME-7             PIC 9(04).
           03  QTW-AMOUNT-7           PIC S9(07)V99.
           03  QTW-OVERSHORT-7        PIC S9(07)V99.
           03  QTW-POSTNAME-8         PIC X(08).
           03  QTW-TIME-8             PIC 9(04).
           03  QTW-AMOUNT-8           PIC S9(07)V99.
           03  QTW-OVERSHORT-8        PIC S9(07)V99.
           03  QTW-POSTNAME-9         PIC X(08).
           03  QTW-TIME-9             PIC 9(04).
           03  QTW-AMOUNT-9           PIC S9(07)V99.
           03  QTW-OVERSHORT-9        PIC S9(07)V99.
           03  QTW-POSTNAME-10        PIC X(08).
           03  QTW-TIME-10            PIC 9(04).
           03  QTW-AMOUNT-10          PIC S9(07)V99.
           03  QTW-OVERSHORT-10       PIC S9(07)V99.
           03  QTW-POSTNAME-11         PIC X(08).
           03  QTW-TIME-11             PIC 9(04).
           03  QTW-AMOUNT-11           PIC S9(07)V99.
           03  QTW-OVERSHORT-11        PIC S9(07)V99.
           03  QTW-POSTNAME-12         PIC X(08).
           03  QTW-TIME-12             PIC 9(04).
           03  QTW-AMOUNT-12           PIC S9(07)V99.
           03  QTW-OVERSHORT-12        PIC S9(07)V99.
           03  QTW-POSTNAME-13         PIC X(08).
           03  QTW-TIME-13             PIC 9(04).
           03  QTW-AMOUNT-13           PIC S9(07)V99.
           03  QTW-OVERSHORT-13        PIC S9(07)V99.
           03  QTW-POSTNAME-14         PIC X(08).
           03  QTW-TIME-14             PIC 9(04).
           03  QTW-AMOUNT-14           PIC S9(07)V99.
           03  QTW-OVERSHORT-14        PIC S9(07)V99.
           03  QTW-POSTNAME-15         PIC X(08).
           03  QTW-TIME-15             PIC 9(04).
           03  QTW-AMOUNT-15           PIC S9(07)V99.
           03  QTW-OVERSHORT-15        PIC S9(07)V99.
           03  QTW-POSTNAME-16         PIC X(08).
           03  QTW-TIME-16             PIC 9(04).
           03  QTW-AMOUNT-16           PIC S9(07)V99.
           03  QTW-OVERSHORT-16        PIC S9(07)V99.
           03  QTW-POSTNAME-17         PIC X(08).
           03  QTW-TIME-17             PIC 9(04).
           03  QTW-AMOUNT-17           PIC S9(07)V99.
           03  QTW-OVERSHORT-17        PIC S9(07)V99.
           03  QTW-POSTNAME-18         PIC X(08).
           03  QTW-TIME-18             PIC 9(04).
           03  QTW-AMOUNT-18           PIC S9(07)V99.
           03  QTW-OVERSHORT-18        PIC S9(07)V99.
           03  QTW-POSTNAME-19         PIC X(08).
           03  QTW-TIME-19             PIC 9(04).
           03  QTW-AMOUNT-19           PIC S9(07)V99.
           03  QTW-OVERSHORT-19        PIC S9(07)V99.
           03  QTW-POSTNAME-20          PIC X(08).
           03  QTW-TIME-20              PIC 9(04).
           03  QTW-AMOUNT-20            PIC S9(07)V99.
           03  QTW-OVERSHORT-20         PIC S9(07)V99.
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
           03  BEG-DATE                PIC 9(08).
           03  END-DATE                PIC 9(08).
           03  BEG-TILLNO              PIC 9(02).
           03  END-TILLNO              PIC 9(02).

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
           03  FILLER PIC X(30) VALUE "BRNO      DATE     TILL    USE".
           03  FILLER PIC X(30) VALUE "R     TIME     AMT         OVE".
           03  FILLER PIC X(30) VALUE "R/SHORT".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-TW-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-TW-DATE                    PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-TW-TILLNO                  PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-TW-POSTNAME-1              PIC X(08).
           03  FILLER                         PIC X(03).
           03  D01-TW-TIME-1                  PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-TW-AMOUNT-1                PIC Z(6)9.99-.
           03  FILLER                         PIC X(03).
           03  D01-TW-OVERSHORT-1             PIC Z(6)9.99-.

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
       COPY "LIBIO/IOTRWR_DEF.CPY".
       COPY "LIBIO/IOTRWR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOTRWR_SCN.CPY".

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
           MOVE "IOTRWR"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOTRWR-QUEUE-POS  TO Q-POS.
           MOVE IOTRWR-COPIES-POS TO C-POS.
           MOVE IOTRWR-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-TILLNO.

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
              ( VDUNUM0 > 2      ) 
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-BEG-DATE.

           MOVE SPACES  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           MOVE "ERNM060END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF ( BEG-DATE > END-DATE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-BEG-TILLNO.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "S  8080BEG-DATE." TO VDU.
           MOVE JULIAN-BEG         TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE." TO VDU.
           MOVE JULIAN-END         TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-TILLNO.

           MOVE "ENNN020BEG-TILLNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-TILLNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-TILLNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-TILLNO.

      *-----------------------------------------------------------------
       ENTER-END-TILLNO.

           MOVE "ENNN020END-TILLNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-TILLNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-TILLNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-TILLNO.

           IF ( BEG-TILLNO > END-TILLNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-TILLNO.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-TILLNO.

           MOVE "SNNN020BEG-TILLNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-TILLNO.
           PERFORM SCREENIO.

           MOVE "SNNN020END-TILLNO." TO VDU.
           MOVE ALL "9"             TO VDUNUM0  END-TILLNO.
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

           MOVE "S  A000BEG-DATE."    TO VDU.
           MOVE BEG-DATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A000END-DATE."    TO VDU.
           MOVE END-DATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "SNNN020BEG-TILLNO."       TO VDU.
           MOVE BEG-TILLNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-TILLNO."       TO VDU.
           MOVE END-TILLNO                 TO VDUNUM0.
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

           MOVE "B"                        TO TRW-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QTRW1-WBEG-BRNO
                              QTRW2-WBEG-BRNO.
           MOVE BR-NO     TO QTRW1-WEND-BRNO
                              QTRW2-WEND-BRNO.

           MOVE BEG-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRW1-WBEG-DATE
                                        QTRW2-WBEG-DATE.

           MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRW1-WEND-DATE
                                        QTRW2-WEND-DATE.

           MOVE BEG-TILLNO           TO QTRW1-WBEG-TILLNO
                                        QTRW2-WBEG-TILLNO.
           MOVE END-TILLNO           TO QTRW1-WEND-TILLNO
                                        QTRW2-WEND-TILLNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF PROCESS-KEY = 1
              PERFORM OPEN-TRW1-FILE
              PERFORM START-TRW1-FILE
           ELSE
              PERFORM OPEN-TRW2-FILE
              PERFORM START-TRW2-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           IF PROCESS-KEY = 1
              PERFORM READ-TRW1-FILE-NEXT
           ELSE
              PERFORM READ-TRW2-FILE-NEXT.
           IF ( IO-BAD ) 
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( TW-BRNO < BEG-BRANCH )
                  GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( TW-BRNO > END-BRANCH )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF TW-DATE < BEG-DATE OR > END-DATE
                    GO TO PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 2 )
              IF ( TW-DATE < BEG-DATE )
                  GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( TW-DATE > END-DATE )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF TW-BRNO < BEG-BRANCH OR > END-BRANCH
                    GO TO PROCESS-BRANCH-NEXT.

           IF TW-TILLNO < BEG-TILLNO OR > END-TILLNO
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE TW-BRNO              TO D01-TW-BRNO.
           MOVE TW-DATE              TO D01-TW-DATE.
           MOVE TW-TILLNO            TO D01-TW-TILLNO.
           MOVE TW-POSTNAME(1)       TO D01-TW-POSTNAME-1.
           MOVE TW-TIME(1)           TO D01-TW-TIME-1.
           MOVE TW-AMOUNT(1)         TO D01-TW-AMOUNT-1.
           MOVE TW-OVERSHORT(1)      TO D01-TW-OVERSHORT-1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           IF PROCESS-KEY = 1
              PERFORM CLOSE-TRW1-FILE
           ELSE
              PERFORM CLOSE-TRW2-FILE.

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

           IF PROCESS-KEY = 1
              MOVE TRW1-KEY TO PACE-DETAIL-KEY
           ELSE
              STRING TW-DATE, TW-BRNO, TW-TILLNO
                     DELIMITED BY SIZE INTO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING DATE               : "    TO R01-PROMPT.
           MOVE BEG-DATE                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE               : "    TO R01-PROMPT.
           MOVE END-DATE                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING TILLNO               : "    TO R01-PROMPT.
           MOVE BEG-TILLNO                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    TILLNO               : "    TO R01-PROMPT.
           MOVE END-TILLNO                             TO R01-NUMERIC.
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
           MOVE "SQL TABLE IO UTILITY - TRWFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOTRWR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOTRWR-SYSDATE.
           DISPLAY IOTRWR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOTRWR-FORM-FIELDS TO WR-BUF.
           MOVE IOTRWR-FORM-LIST TO WR-LIST.
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

           MOVE IOTRWR-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOTRWR-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOTRWR_EVA.CPY".
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
      *    LOAD TRW-REC FROM QTRW-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-TRW-FIELDS.
           MOVE QTW-BRNO TO TW-BRNO.

           IF ( QTW-DATE = "1900-01-01" )
              MOVE ZEROES TO TW-DATE
           ELSE
              MOVE QTW-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TW-DATE.

           MOVE QTW-TILLNO TO TW-TILLNO.

           IF ( QTW-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO TW-LTOUCH-DATE
           ELSE
              MOVE QTW-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TW-LTOUCH-DATE.

           MOVE QTW-POSTNAME-1 TO TW-POSTNAME(1).
           MOVE QTW-TIME-1 TO TW-TIME(1).
           MOVE QTW-AMOUNT-1 TO TW-AMOUNT(1).
           MOVE QTW-OVERSHORT-1 TO TW-OVERSHORT(1).
           MOVE QTW-POSTNAME-2 TO TW-POSTNAME(2).
           MOVE QTW-TIME-2 TO TW-TIME(2).
           MOVE QTW-AMOUNT-2 TO TW-AMOUNT(2).
           MOVE QTW-OVERSHORT-2 TO TW-OVERSHORT(2).
           MOVE QTW-POSTNAME-3 TO TW-POSTNAME(3).
           MOVE QTW-TIME-3 TO TW-TIME(3).
           MOVE QTW-AMOUNT-3 TO TW-AMOUNT(3).
           MOVE QTW-OVERSHORT-3 TO TW-OVERSHORT(3).
           MOVE QTW-POSTNAME-4 TO TW-POSTNAME(4).
           MOVE QTW-TIME-4 TO TW-TIME(4).
           MOVE QTW-AMOUNT-4 TO TW-AMOUNT(4).
           MOVE QTW-OVERSHORT-4 TO TW-OVERSHORT(4).
           MOVE QTW-POSTNAME-5 TO TW-POSTNAME(5).
           MOVE QTW-TIME-5 TO TW-TIME(5).
           MOVE QTW-AMOUNT-5 TO TW-AMOUNT(5).
           MOVE QTW-OVERSHORT-5 TO TW-OVERSHORT(5).
           MOVE QTW-POSTNAME-6 TO TW-POSTNAME(6).
           MOVE QTW-TIME-6 TO TW-TIME(6).
           MOVE QTW-AMOUNT-6 TO TW-AMOUNT(6).
           MOVE QTW-OVERSHORT-6 TO TW-OVERSHORT(6).
           MOVE QTW-POSTNAME-7 TO TW-POSTNAME(7).
           MOVE QTW-TIME-7 TO TW-TIME(7).
           MOVE QTW-AMOUNT-7 TO TW-AMOUNT(7).
           MOVE QTW-OVERSHORT-7 TO TW-OVERSHORT(7).
           MOVE QTW-POSTNAME-8 TO TW-POSTNAME(8).
           MOVE QTW-TIME-8 TO TW-TIME(8).
           MOVE QTW-AMOUNT-8 TO TW-AMOUNT(8).
           MOVE QTW-OVERSHORT-8 TO TW-OVERSHORT(8).
           MOVE QTW-POSTNAME-9 TO TW-POSTNAME(9).
           MOVE QTW-TIME-9 TO TW-TIME(9).
           MOVE QTW-AMOUNT-9 TO TW-AMOUNT(9).
           MOVE QTW-OVERSHORT-9 TO TW-OVERSHORT(9).
           MOVE QTW-POSTNAME-10 TO TW-POSTNAME(10).
           MOVE QTW-TIME-10 TO TW-TIME(10).
           MOVE QTW-AMOUNT-10 TO TW-AMOUNT(10).
           MOVE QTW-OVERSHORT-10 TO TW-OVERSHORT(10).
           MOVE QTW-POSTNAME-11 TO TW-POSTNAME(11).
           MOVE QTW-TIME-11 TO TW-TIME(11).
           MOVE QTW-AMOUNT-11 TO TW-AMOUNT(11).
           MOVE QTW-OVERSHORT-11 TO TW-OVERSHORT(11).
           MOVE QTW-POSTNAME-12 TO TW-POSTNAME(12).
           MOVE QTW-TIME-12 TO TW-TIME(12).
           MOVE QTW-AMOUNT-12 TO TW-AMOUNT(12).
           MOVE QTW-OVERSHORT-12 TO TW-OVERSHORT(12).
           MOVE QTW-POSTNAME-13 TO TW-POSTNAME(13).
           MOVE QTW-TIME-13 TO TW-TIME(13).
           MOVE QTW-AMOUNT-13 TO TW-AMOUNT(13).
           MOVE QTW-OVERSHORT-13 TO TW-OVERSHORT(13).
           MOVE QTW-POSTNAME-14 TO TW-POSTNAME(14).
           MOVE QTW-TIME-14 TO TW-TIME(14).
           MOVE QTW-AMOUNT-14 TO TW-AMOUNT(14).
           MOVE QTW-OVERSHORT-14 TO TW-OVERSHORT(14).
           MOVE QTW-POSTNAME-15 TO TW-POSTNAME(15).
           MOVE QTW-TIME-15 TO TW-TIME(15).
           MOVE QTW-AMOUNT-15 TO TW-AMOUNT(15).
           MOVE QTW-OVERSHORT-15 TO TW-OVERSHORT(15).
           MOVE QTW-POSTNAME-16 TO TW-POSTNAME(16).
           MOVE QTW-TIME-16 TO TW-TIME(16).
           MOVE QTW-AMOUNT-16 TO TW-AMOUNT(16).
           MOVE QTW-OVERSHORT-16 TO TW-OVERSHORT(16).
           MOVE QTW-POSTNAME-17 TO TW-POSTNAME(17).
           MOVE QTW-TIME-17 TO TW-TIME(17).
           MOVE QTW-AMOUNT-17 TO TW-AMOUNT(17).
           MOVE QTW-OVERSHORT-17 TO TW-OVERSHORT(17).
           MOVE QTW-POSTNAME-18 TO TW-POSTNAME(18).
           MOVE QTW-TIME-18 TO TW-TIME(18).
           MOVE QTW-AMOUNT-18 TO TW-AMOUNT(18).
           MOVE QTW-OVERSHORT-18 TO TW-OVERSHORT(18).
           MOVE QTW-POSTNAME-19 TO TW-POSTNAME(19).
           MOVE QTW-TIME-19 TO TW-TIME(19).
           MOVE QTW-AMOUNT-19 TO TW-AMOUNT(19).
           MOVE QTW-OVERSHORT-19 TO TW-OVERSHORT(19).
           MOVE QTW-POSTNAME-20 TO TW-POSTNAME(20).
           MOVE QTW-TIME-20 TO TW-TIME(20).
           MOVE QTW-AMOUNT-20 TO TW-AMOUNT(20).
           MOVE QTW-OVERSHORT-20 TO TW-OVERSHORT(20).
 
      *-----------------------------------------------------------------
      *    LOAD QTRW-REC FROM TRW-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-TRW-FIELDS.
           MOVE TW-BRNO TO QTW-BRNO.

           IF ( TW-DATE = ZEROES )
              MOVE "1900-01-01" TO QTW-DATE
           ELSE
              MOVE TW-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTW-DATE.

           MOVE TW-TILLNO TO QTW-TILLNO.

           IF ( TW-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTW-LTOUCH-DATE
           ELSE
              MOVE TW-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTW-LTOUCH-DATE.

           MOVE TW-POSTNAME(1) TO QTW-POSTNAME-1.
           MOVE TW-TIME(1) TO QTW-TIME-1.
           MOVE TW-AMOUNT(1) TO QTW-AMOUNT-1.
           MOVE TW-OVERSHORT(1) TO QTW-OVERSHORT-1.
           MOVE TW-POSTNAME(2) TO QTW-POSTNAME-2.
           MOVE TW-TIME(2) TO QTW-TIME-2.
           MOVE TW-AMOUNT(2) TO QTW-AMOUNT-2.
           MOVE TW-OVERSHORT(2) TO QTW-OVERSHORT-2.
           MOVE TW-POSTNAME(3) TO QTW-POSTNAME-3.
           MOVE TW-TIME(3) TO QTW-TIME-3.
           MOVE TW-AMOUNT(3) TO QTW-AMOUNT-3.
           MOVE TW-OVERSHORT(3) TO QTW-OVERSHORT-3.
           MOVE TW-POSTNAME(4) TO QTW-POSTNAME-4.
           MOVE TW-TIME(4) TO QTW-TIME-4.
           MOVE TW-AMOUNT(4) TO QTW-AMOUNT-4.
           MOVE TW-OVERSHORT(4) TO QTW-OVERSHORT-4.
           MOVE TW-POSTNAME(5) TO QTW-POSTNAME-5.
           MOVE TW-TIME(5) TO QTW-TIME-5.
           MOVE TW-AMOUNT(5) TO QTW-AMOUNT-5.
           MOVE TW-OVERSHORT(5) TO QTW-OVERSHORT-5.
           MOVE TW-POSTNAME(6) TO QTW-POSTNAME-6.
           MOVE TW-TIME(6) TO QTW-TIME-6.
           MOVE TW-AMOUNT(6) TO QTW-AMOUNT-6.
           MOVE TW-OVERSHORT(6) TO QTW-OVERSHORT-6.
           MOVE TW-POSTNAME(7) TO QTW-POSTNAME-7.
           MOVE TW-TIME(7) TO QTW-TIME-7.
           MOVE TW-AMOUNT(7) TO QTW-AMOUNT-7.
           MOVE TW-OVERSHORT(7) TO QTW-OVERSHORT-7.
           MOVE TW-POSTNAME(8) TO QTW-POSTNAME-8.
           MOVE TW-TIME(8) TO QTW-TIME-8.
           MOVE TW-AMOUNT(8) TO QTW-AMOUNT-8.
           MOVE TW-OVERSHORT(8) TO QTW-OVERSHORT-8.
           MOVE TW-POSTNAME(9) TO QTW-POSTNAME-9.
           MOVE TW-TIME(9) TO QTW-TIME-9.
           MOVE TW-AMOUNT(9) TO QTW-AMOUNT-9.
           MOVE TW-OVERSHORT(9) TO QTW-OVERSHORT-9.
           MOVE TW-POSTNAME(10) TO QTW-POSTNAME-10.
           MOVE TW-TIME(10) TO QTW-TIME-10.
           MOVE TW-AMOUNT(10) TO QTW-AMOUNT-10.
           MOVE TW-OVERSHORT(10) TO QTW-OVERSHORT-10.
           MOVE TW-POSTNAME(11) TO QTW-POSTNAME-11.
           MOVE TW-TIME(11) TO QTW-TIME-11.
           MOVE TW-AMOUNT(11) TO QTW-AMOUNT-11.
           MOVE TW-OVERSHORT(11) TO QTW-OVERSHORT-11.
           MOVE TW-POSTNAME(12) TO QTW-POSTNAME-12.
           MOVE TW-TIME(12) TO QTW-TIME-12.
           MOVE TW-AMOUNT(12) TO QTW-AMOUNT-12.
           MOVE TW-OVERSHORT(12) TO QTW-OVERSHORT-12.
           MOVE TW-POSTNAME(13) TO QTW-POSTNAME-13.
           MOVE TW-TIME(13) TO QTW-TIME-13.
           MOVE TW-AMOUNT(13) TO QTW-AMOUNT-13.
           MOVE TW-OVERSHORT(13) TO QTW-OVERSHORT-13.
           MOVE TW-POSTNAME(14) TO QTW-POSTNAME-14.
           MOVE TW-TIME(14) TO QTW-TIME-14.
           MOVE TW-AMOUNT(14) TO QTW-AMOUNT-14.
           MOVE TW-OVERSHORT(14) TO QTW-OVERSHORT-14.
           MOVE TW-POSTNAME(15) TO QTW-POSTNAME-15.
           MOVE TW-TIME(15) TO QTW-TIME-15.
           MOVE TW-AMOUNT(15) TO QTW-AMOUNT-15.
           MOVE TW-OVERSHORT(15) TO QTW-OVERSHORT-15.
           MOVE TW-POSTNAME(16) TO QTW-POSTNAME-16.
           MOVE TW-TIME(16) TO QTW-TIME-16.
           MOVE TW-AMOUNT(16) TO QTW-AMOUNT-16.
           MOVE TW-OVERSHORT(16) TO QTW-OVERSHORT-16.
           MOVE TW-POSTNAME(17) TO QTW-POSTNAME-17.
           MOVE TW-TIME(17) TO QTW-TIME-17.
           MOVE TW-AMOUNT(17) TO QTW-AMOUNT-17.
           MOVE TW-OVERSHORT(17) TO QTW-OVERSHORT-17.
           MOVE TW-POSTNAME(18) TO QTW-POSTNAME-18.
           MOVE TW-TIME(18) TO QTW-TIME-18.
           MOVE TW-AMOUNT(18) TO QTW-AMOUNT-18.
           MOVE TW-OVERSHORT(18) TO QTW-OVERSHORT-18.
           MOVE TW-POSTNAME(19) TO QTW-POSTNAME-19.
           MOVE TW-TIME(19) TO QTW-TIME-19.
           MOVE TW-AMOUNT(19) TO QTW-AMOUNT-19.
           MOVE TW-OVERSHORT(19) TO QTW-OVERSHORT-19.
           MOVE TW-POSTNAME(20) TO QTW-POSTNAME-20.
           MOVE TW-TIME(20) TO QTW-TIME-20.
           MOVE TW-AMOUNT(20) TO QTW-AMOUNT-20.
           MOVE TW-OVERSHORT(20) TO QTW-OVERSHORT-20.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-TRW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRW-ALL-DATA
              MOVE TRW-ALL-PATH             TO TRW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRW-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRW-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRW-ALL-DATA
              MOVE TRW-ALL-PATH             TO TRW-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRWFILE NOT = "S" )
              MOVE TRW-PATH-BEG             TO TRW-SELECT-PATH-BEG
              MOVE TRW-PATH-FILE            TO TRW-SELECT-PATH-FILE
           ELSE
              MOVE TRW-PATH-SQL             TO TRW-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRW1-FILE.
           PERFORM LOAD-TRW1-FILE.
           PERFORM OPEN-IT.
           MOVE TRW-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRW1-FILE.
           PERFORM START-IT.
           MOVE TRW-PATH-OWNBR  TO TW-BRNO.
           MOVE TRW-SELECT-PATH TO E-FILE.
           MOVE TRW1-KEY        TO E-KEYX.

           IF ( TRW1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRW1-FILE.

           IF ( QTRW1-WEND-TILLNO = ZEROES    ) OR
              ( QTRW1-WEND-TILLNO NOT NUMERIC )
              MOVE TRW-PATH-OWNBR        TO QTRW1-WBEG-BRNO
              MOVE TRW-PATH-OWNBR        TO QTRW1-WEND-BRNO
              MOVE TW-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRW1-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRW1-WEND-DATE
              MOVE TW-TILLNO            TO QTRW1-WBEG-TILLNO
              MOVE ALL "9"              TO QTRW1-WEND-TILLNO.


           EXEC SQL
            DECLARE TRW1_CURSOR CURSOR FOR
            SELECT TRWFILE.TW_BRNO,
                   ISNULL(CAST(TRWFILE.TW_DATE AS VARCHAR(10)),'0'),
                   TRWFILE.TW_TILLNO,
                   ISNULL(CAST(TRWFILE.TW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(TRWFILE.TW_POSTNAME_1,''),
                   TRWFILE.TW_TIME_1,
                   TRWFILE.TW_AMOUNT_1,
                   TRWFILE.TW_OVERSHORT_1,
                   ISNULL(TRWFILE.TW_POSTNAME_2,''),
                   TRWFILE.TW_TIME_2,
                   TRWFILE.TW_AMOUNT_2,
                   TRWFILE.TW_OVERSHORT_2,
                   ISNULL(TRWFILE.TW_POSTNAME_3,''),
                   TRWFILE.TW_TIME_3,
                   TRWFILE.TW_AMOUNT_3,
                   TRWFILE.TW_OVERSHORT_3,
                   ISNULL(TRWFILE.TW_POSTNAME_4,''),
                   TRWFILE.TW_TIME_4,
                   TRWFILE.TW_AMOUNT_4,
                   TRWFILE.TW_OVERSHORT_4,
                   ISNULL(TRWFILE.TW_POSTNAME_5,''),
                   TRWFILE.TW_TIME_5,
                   TRWFILE.TW_AMOUNT_5,
                   TRWFILE.TW_OVERSHORT_5,
                   ISNULL(TRWFILE.TW_POSTNAME_6,''),
                   TRWFILE.TW_TIME_6,
                   TRWFILE.TW_AMOUNT_6,
                   TRWFILE.TW_OVERSHORT_6,
                   ISNULL(TRWFILE.TW_POSTNAME_7,''),
                   TRWFILE.TW_TIME_7,
                   TRWFILE.TW_AMOUNT_7,
                   TRWFILE.TW_OVERSHORT_7,
                   ISNULL(TRWFILE.TW_POSTNAME_8,''),
                   TRWFILE.TW_TIME_8,
                   TRWFILE.TW_AMOUNT_8,
                   TRWFILE.TW_OVERSHORT_8,
                   ISNULL(TRWFILE.TW_POSTNAME_9,''),
                   TRWFILE.TW_TIME_9,
                   TRWFILE.TW_AMOUNT_9,
                   TRWFILE.TW_OVERSHORT_9,
                   ISNULL(TRWFILE.TW_POSTNAME_10,''),
                   TRWFILE.TW_TIME_10,
                   TRWFILE.TW_AMOUNT_10,
                   TRWFILE.TW_OVERSHORT_10,
                   ISNULL(TRWFILE.TW_POSTNAME_11,''),
                   TRWFILE.TW_TIME_11,
                   TRWFILE.TW_AMOUNT_11,
                   TRWFILE.TW_OVERSHORT_11,
                   ISNULL(TRWFILE.TW_POSTNAME_12,''),
                   TRWFILE.TW_TIME_12,
                   TRWFILE.TW_AMOUNT_12,
                   TRWFILE.TW_OVERSHORT_12,
                   ISNULL(TRWFILE.TW_POSTNAME_13,''),
                   TRWFILE.TW_TIME_13,
                   TRWFILE.TW_AMOUNT_13,
                   TRWFILE.TW_OVERSHORT_13,
                   ISNULL(TRWFILE.TW_POSTNAME_14,''),
                   TRWFILE.TW_TIME_14,
                   TRWFILE.TW_AMOUNT_14,
                   TRWFILE.TW_OVERSHORT_14,
                   ISNULL(TRWFILE.TW_POSTNAME_15,''),
                   TRWFILE.TW_TIME_15,
                   TRWFILE.TW_AMOUNT_15,
                   TRWFILE.TW_OVERSHORT_15,
                   ISNULL(TRWFILE.TW_POSTNAME_16,''),
                   TRWFILE.TW_TIME_16,
                   TRWFILE.TW_AMOUNT_16,
                   TRWFILE.TW_OVERSHORT_16,
                   ISNULL(TRWFILE.TW_POSTNAME_17,''),
                   TRWFILE.TW_TIME_17,
                   TRWFILE.TW_AMOUNT_17,
                   TRWFILE.TW_OVERSHORT_17,
                   ISNULL(TRWFILE.TW_POSTNAME_18,''),
                   TRWFILE.TW_TIME_18,
                   TRWFILE.TW_AMOUNT_18,
                   TRWFILE.TW_OVERSHORT_18,
                   ISNULL(TRWFILE.TW_POSTNAME_19,''),
                   TRWFILE.TW_TIME_19,
                   TRWFILE.TW_AMOUNT_19,
                   TRWFILE.TW_OVERSHORT_19,
                   ISNULL(TRWFILE.TW_POSTNAME_20,''),
                   TRWFILE.TW_TIME_20,
                   TRWFILE.TW_AMOUNT_20,
                   TRWFILE.TW_OVERSHORT_20
            FROM DBO.TRWFILE
            WHERE TRWFILE.TW_BRNO >= :QTRW1-WBEG-BRNO
              AND TRWFILE.TW_BRNO <= :QTRW1-WEND-BRNO
              AND CAST(TRWFILE.TW_DATE AS VARCHAR(10))
                          >= :QTRW1-WBEG-DATE
              AND CAST(TRWFILE.TW_DATE AS VARCHAR(10))
                          <= :QTRW1-WEND-DATE
              AND TRWFILE.TW_TILLNO >= :QTRW1-WBEG-TILLNO
              AND TRWFILE.TW_TILLNO <= :QTRW1-WEND-TILLNO
            ORDER BY TRWFILE.TW_BRNO,
                     TRWFILE.TW_DATE,
                     TRWFILE.TW_TILLNO
           END-EXEC.

           EXEC SQL
               OPEN TRW1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRW1-WBEG-BRNO
                          QTRW1-WEND-BRNO
                          QTRW1-WBEG-TILLNO
                          QTRW1-WEND-TILLNO.

           MOVE SPACES TO QTRW1-WBEG-DATE
                          QTRW1-WEND-DATE.

           MOVE STAT---GOOD TO TRW1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRW1-FILE.
           PERFORM READ-IT.
           MOVE TRW-PATH-OWNBR  TO TW-BRNO. 
           MOVE TRW-SELECT-PATH TO E-FILE.
           MOVE TRW1-KEY        TO E-KEYX.

           MOVE TW-BRNO             TO QTW-BRNO.
           MOVE TW-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTW-DATE.
           MOVE TW-TILLNO           TO QTW-TILLNO.

           EXEC SQL
            SELECT TRWFILE.TW_BRNO,
                   ISNULL(CAST(TRWFILE.TW_DATE AS VARCHAR(10)),'0'),
                   TRWFILE.TW_TILLNO,
                   ISNULL(CAST(TRWFILE.TW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(TRWFILE.TW_POSTNAME_1,''),
                   TRWFILE.TW_TIME_1,
                   TRWFILE.TW_AMOUNT_1,
                   TRWFILE.TW_OVERSHORT_1,
                   ISNULL(TRWFILE.TW_POSTNAME_2,''),
                   TRWFILE.TW_TIME_2,
                   TRWFILE.TW_AMOUNT_2,
                   TRWFILE.TW_OVERSHORT_2,
                   ISNULL(TRWFILE.TW_POSTNAME_3,''),
                   TRWFILE.TW_TIME_3,
                   TRWFILE.TW_AMOUNT_3,
                   TRWFILE.TW_OVERSHORT_3,
                   ISNULL(TRWFILE.TW_POSTNAME_4,''),
                   TRWFILE.TW_TIME_4,
                   TRWFILE.TW_AMOUNT_4,
                   TRWFILE.TW_OVERSHORT_4,
                   ISNULL(TRWFILE.TW_POSTNAME_5,''),
                   TRWFILE.TW_TIME_5,
                   TRWFILE.TW_AMOUNT_5,
                   TRWFILE.TW_OVERSHORT_5,
                   ISNULL(TRWFILE.TW_POSTNAME_6,''),
                   TRWFILE.TW_TIME_6,
                   TRWFILE.TW_AMOUNT_6,
                   TRWFILE.TW_OVERSHORT_6,
                   ISNULL(TRWFILE.TW_POSTNAME_7,''),
                   TRWFILE.TW_TIME_7,
                   TRWFILE.TW_AMOUNT_7,
                   TRWFILE.TW_OVERSHORT_7,
                   ISNULL(TRWFILE.TW_POSTNAME_8,''),
                   TRWFILE.TW_TIME_8,
                   TRWFILE.TW_AMOUNT_8,
                   TRWFILE.TW_OVERSHORT_8,
                   ISNULL(TRWFILE.TW_POSTNAME_9,''),
                   TRWFILE.TW_TIME_9,
                   TRWFILE.TW_AMOUNT_9,
                   TRWFILE.TW_OVERSHORT_9,
                   ISNULL(TRWFILE.TW_POSTNAME_10,''),
                   TRWFILE.TW_TIME_10,
                   TRWFILE.TW_AMOUNT_10,
                   TRWFILE.TW_OVERSHORT_10,
                   ISNULL(TRWFILE.TW_POSTNAME_11,''),
                   TRWFILE.TW_TIME_11,
                   TRWFILE.TW_AMOUNT_11,
                   TRWFILE.TW_OVERSHORT_11,
                   ISNULL(TRWFILE.TW_POSTNAME_12,''),
                   TRWFILE.TW_TIME_12,
                   TRWFILE.TW_AMOUNT_12,
                   TRWFILE.TW_OVERSHORT_12,
                   ISNULL(TRWFILE.TW_POSTNAME_13,''),
                   TRWFILE.TW_TIME_13,
                   TRWFILE.TW_AMOUNT_13,
                   TRWFILE.TW_OVERSHORT_13,
                   ISNULL(TRWFILE.TW_POSTNAME_14,''),
                   TRWFILE.TW_TIME_14,
                   TRWFILE.TW_AMOUNT_14,
                   TRWFILE.TW_OVERSHORT_14,
                   ISNULL(TRWFILE.TW_POSTNAME_15,''),
                   TRWFILE.TW_TIME_15,
                   TRWFILE.TW_AMOUNT_15,
                   TRWFILE.TW_OVERSHORT_15,
                   ISNULL(TRWFILE.TW_POSTNAME_16,''),
                   TRWFILE.TW_TIME_16,
                   TRWFILE.TW_AMOUNT_16,
                   TRWFILE.TW_OVERSHORT_16,
                   ISNULL(TRWFILE.TW_POSTNAME_17,''),
                   TRWFILE.TW_TIME_17,
                   TRWFILE.TW_AMOUNT_17,
                   TRWFILE.TW_OVERSHORT_17,
                   ISNULL(TRWFILE.TW_POSTNAME_18,''),
                   TRWFILE.TW_TIME_18,
                   TRWFILE.TW_AMOUNT_18,
                   TRWFILE.TW_OVERSHORT_18,
                   ISNULL(TRWFILE.TW_POSTNAME_19,''),
                   TRWFILE.TW_TIME_19,
                   TRWFILE.TW_AMOUNT_19,
                   TRWFILE.TW_OVERSHORT_19,
                   ISNULL(TRWFILE.TW_POSTNAME_20,''),
                   TRWFILE.TW_TIME_20,
                   TRWFILE.TW_AMOUNT_20,
                   TRWFILE.TW_OVERSHORT_20
            INTO :QTRW-REC
            FROM DBO.TRWFILE
            WHERE TRWFILE.TW_BRNO = :QTW-BRNO
              AND TRWFILE.TW_DATE = :QTW-DATE
              AND TRWFILE.TW_TILLNO = :QTW-TILLNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRW1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRW-FIELDS.

      *-----------------------------------------------------------------
       READ-TRW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRW-PATH-OWNBR  TO TW-BRNO.
           MOVE TRW-SELECT-PATH TO E-FILE.
           MOVE TRW1-KEY        TO E-KEYX.
           INITIALIZE QTRW-REC.

           IF ( TRW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRW1_CURSOR INTO :QTRW-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRW1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRW1-FILE.

           IF ( TRW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRW1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRW1-CURSOR-STAT.

      *==========================================================
       LOAD-TRW2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRW-ALL-DATA
              MOVE TRW-ALL-PATH             TO TRW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRW-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRW-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRW-ALL-DATA
              MOVE TRW-ALL-PATH             TO TRW-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRWFILE NOT = "S" )
              MOVE TRW-PATH-BEG             TO TRW-SELECT-PATH-BEG
              MOVE TRW-PATH-FILE            TO TRW-SELECT-PATH-FILE
           ELSE
              MOVE TRW-PATH-SQL             TO TRW-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRW2-FILE.
           PERFORM LOAD-TRW2-FILE.
           PERFORM OPEN-IT.
           MOVE TRW-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRW2-FILE.
           PERFORM START-IT.
           MOVE TRW-PATH-OWNBR  TO TW-BRNO.
           MOVE TRW-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING TW-DATE, TW-BRNO, TW-TILLNO
                  DELIMITED BY SIZE INTO E-KEYX.

           IF ( TRW2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRW2-FILE.

           IF ( QTRW2-WEND-TILLNO = ZEROES    ) OR
              ( QTRW2-WEND-TILLNO NOT NUMERIC )
              MOVE TW-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRW2-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRW2-WEND-DATE
              MOVE TRW-PATH-OWNBR       TO QTRW2-WBEG-BRNO
              MOVE TRW-PATH-OWNBR       TO QTRW2-WEND-BRNO
              MOVE TW-TILLNO            TO QTRW2-WBEG-TILLNO
              MOVE ALL "9"              TO QTRW2-WEND-TILLNO.


           EXEC SQL
            DECLARE TRW2_CURSOR CURSOR FOR
            SELECT TRWFILE.TW_BRNO,
                   ISNULL(CAST(TRWFILE.TW_DATE AS VARCHAR(10)),'0'),
                   TRWFILE.TW_TILLNO,
                   ISNULL(CAST(TRWFILE.TW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(TRWFILE.TW_POSTNAME_1,''),
                   TRWFILE.TW_TIME_1,
                   TRWFILE.TW_AMOUNT_1,
                   TRWFILE.TW_OVERSHORT_1,
                   ISNULL(TRWFILE.TW_POSTNAME_2,''),
                   TRWFILE.TW_TIME_2,
                   TRWFILE.TW_AMOUNT_2,
                   TRWFILE.TW_OVERSHORT_2,
                   ISNULL(TRWFILE.TW_POSTNAME_3,''),
                   TRWFILE.TW_TIME_3,
                   TRWFILE.TW_AMOUNT_3,
                   TRWFILE.TW_OVERSHORT_3,
                   ISNULL(TRWFILE.TW_POSTNAME_4,''),
                   TRWFILE.TW_TIME_4,
                   TRWFILE.TW_AMOUNT_4,
                   TRWFILE.TW_OVERSHORT_4,
                   ISNULL(TRWFILE.TW_POSTNAME_5,''),
                   TRWFILE.TW_TIME_5,
                   TRWFILE.TW_AMOUNT_5,
                   TRWFILE.TW_OVERSHORT_5,
                   ISNULL(TRWFILE.TW_POSTNAME_6,''),
                   TRWFILE.TW_TIME_6,
                   TRWFILE.TW_AMOUNT_6,
                   TRWFILE.TW_OVERSHORT_6,
                   ISNULL(TRWFILE.TW_POSTNAME_7,''),
                   TRWFILE.TW_TIME_7,
                   TRWFILE.TW_AMOUNT_7,
                   TRWFILE.TW_OVERSHORT_7,
                   ISNULL(TRWFILE.TW_POSTNAME_8,''),
                   TRWFILE.TW_TIME_8,
                   TRWFILE.TW_AMOUNT_8,
                   TRWFILE.TW_OVERSHORT_8,
                   ISNULL(TRWFILE.TW_POSTNAME_9,''),
                   TRWFILE.TW_TIME_9,
                   TRWFILE.TW_AMOUNT_9,
                   TRWFILE.TW_OVERSHORT_9,
                   ISNULL(TRWFILE.TW_POSTNAME_10,''),
                   TRWFILE.TW_TIME_10,
                   TRWFILE.TW_AMOUNT_10,
                   TRWFILE.TW_OVERSHORT_10,
                   ISNULL(TRWFILE.TW_POSTNAME_11,''),
                   TRWFILE.TW_TIME_11,
                   TRWFILE.TW_AMOUNT_11,
                   TRWFILE.TW_OVERSHORT_11,
                   ISNULL(TRWFILE.TW_POSTNAME_12,''),
                   TRWFILE.TW_TIME_12,
                   TRWFILE.TW_AMOUNT_12,
                   TRWFILE.TW_OVERSHORT_12,
                   ISNULL(TRWFILE.TW_POSTNAME_13,''),
                   TRWFILE.TW_TIME_13,
                   TRWFILE.TW_AMOUNT_13,
                   TRWFILE.TW_OVERSHORT_13,
                   ISNULL(TRWFILE.TW_POSTNAME_14,''),
                   TRWFILE.TW_TIME_14,
                   TRWFILE.TW_AMOUNT_14,
                   TRWFILE.TW_OVERSHORT_14,
                   ISNULL(TRWFILE.TW_POSTNAME_15,''),
                   TRWFILE.TW_TIME_15,
                   TRWFILE.TW_AMOUNT_15,
                   TRWFILE.TW_OVERSHORT_15,
                   ISNULL(TRWFILE.TW_POSTNAME_16,''),
                   TRWFILE.TW_TIME_16,
                   TRWFILE.TW_AMOUNT_16,
                   TRWFILE.TW_OVERSHORT_16,
                   ISNULL(TRWFILE.TW_POSTNAME_17,''),
                   TRWFILE.TW_TIME_17,
                   TRWFILE.TW_AMOUNT_17,
                   TRWFILE.TW_OVERSHORT_17,
                   ISNULL(TRWFILE.TW_POSTNAME_18,''),
                   TRWFILE.TW_TIME_18,
                   TRWFILE.TW_AMOUNT_18,
                   TRWFILE.TW_OVERSHORT_18,
                   ISNULL(TRWFILE.TW_POSTNAME_19,''),
                   TRWFILE.TW_TIME_19,
                   TRWFILE.TW_AMOUNT_19,
                   TRWFILE.TW_OVERSHORT_19,
                   ISNULL(TRWFILE.TW_POSTNAME_20,''),
                   TRWFILE.TW_TIME_20,
                   TRWFILE.TW_AMOUNT_20,
                   TRWFILE.TW_OVERSHORT_20
            FROM DBO.TRWFILE
            WHERE CAST(TRWFILE.TW_DATE AS VARCHAR(10))
                          >= :QTRW2-WBEG-DATE
              AND CAST(TRWFILE.TW_DATE AS VARCHAR(10))
                          <= :QTRW2-WEND-DATE
              AND TRWFILE.TW_BRNO >= :QTRW2-WBEG-BRNO
              AND TRWFILE.TW_BRNO <= :QTRW2-WEND-BRNO
              AND TRWFILE.TW_TILLNO >= :QTRW2-WBEG-TILLNO
              AND TRWFILE.TW_TILLNO <= :QTRW2-WEND-TILLNO
            ORDER BY TRWFILE.TW_DATE,
                     TRWFILE.TW_BRNO,
                     TRWFILE.TW_TILLNO
           END-EXEC.

           EXEC SQL
               OPEN TRW2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRW2-WBEG-BRNO
                          QTRW2-WEND-BRNO
                          QTRW2-WBEG-TILLNO
                          QTRW2-WEND-TILLNO.

           MOVE SPACES TO QTRW2-WBEG-DATE
                          QTRW2-WEND-DATE.

           MOVE STAT---GOOD TO TRW2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRW2-FILE.
           PERFORM READ-IT.
           MOVE TRW-PATH-OWNBR  TO TW-BRNO. 
           MOVE TRW-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING TW-DATE, TW-BRNO, TW-TILLNO
                  DELIMITED BY SIZE INTO E-KEYX.

           MOVE TW-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTW-DATE.
           MOVE TW-BRNO             TO QTW-BRNO.
           MOVE TW-TILLNO           TO QTW-TILLNO.

           EXEC SQL
            SELECT TRWFILE.TW_BRNO,
                   ISNULL(CAST(TRWFILE.TW_DATE AS VARCHAR(10)),'0'),
                   TRWFILE.TW_TILLNO,
                   ISNULL(CAST(TRWFILE.TW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(TRWFILE.TW_POSTNAME_1,''),
                   TRWFILE.TW_TIME_1,
                   TRWFILE.TW_AMOUNT_1,
                   TRWFILE.TW_OVERSHORT_1,
                   ISNULL(TRWFILE.TW_POSTNAME_2,''),
                   TRWFILE.TW_TIME_2,
                   TRWFILE.TW_AMOUNT_2,
                   TRWFILE.TW_OVERSHORT_2,
                   ISNULL(TRWFILE.TW_POSTNAME_3,''),
                   TRWFILE.TW_TIME_3,
                   TRWFILE.TW_AMOUNT_3,
                   TRWFILE.TW_OVERSHORT_3,
                   ISNULL(TRWFILE.TW_POSTNAME_4,''),
                   TRWFILE.TW_TIME_4,
                   TRWFILE.TW_AMOUNT_4,
                   TRWFILE.TW_OVERSHORT_4,
                   ISNULL(TRWFILE.TW_POSTNAME_5,''),
                   TRWFILE.TW_TIME_5,
                   TRWFILE.TW_AMOUNT_5,
                   TRWFILE.TW_OVERSHORT_5,
                   ISNULL(TRWFILE.TW_POSTNAME_6,''),
                   TRWFILE.TW_TIME_6,
                   TRWFILE.TW_AMOUNT_6,
                   TRWFILE.TW_OVERSHORT_6,
                   ISNULL(TRWFILE.TW_POSTNAME_7,''),
                   TRWFILE.TW_TIME_7,
                   TRWFILE.TW_AMOUNT_7,
                   TRWFILE.TW_OVERSHORT_7,
                   ISNULL(TRWFILE.TW_POSTNAME_8,''),
                   TRWFILE.TW_TIME_8,
                   TRWFILE.TW_AMOUNT_8,
                   TRWFILE.TW_OVERSHORT_8,
                   ISNULL(TRWFILE.TW_POSTNAME_9,''),
                   TRWFILE.TW_TIME_9,
                   TRWFILE.TW_AMOUNT_9,
                   TRWFILE.TW_OVERSHORT_9,
                   ISNULL(TRWFILE.TW_POSTNAME_10,''),
                   TRWFILE.TW_TIME_10,
                   TRWFILE.TW_AMOUNT_10,
                   TRWFILE.TW_OVERSHORT_10,
                   ISNULL(TRWFILE.TW_POSTNAME_11,''),
                   TRWFILE.TW_TIME_11,
                   TRWFILE.TW_AMOUNT_11,
                   TRWFILE.TW_OVERSHORT_11,
                   ISNULL(TRWFILE.TW_POSTNAME_12,''),
                   TRWFILE.TW_TIME_12,
                   TRWFILE.TW_AMOUNT_12,
                   TRWFILE.TW_OVERSHORT_12,
                   ISNULL(TRWFILE.TW_POSTNAME_13,''),
                   TRWFILE.TW_TIME_13,
                   TRWFILE.TW_AMOUNT_13,
                   TRWFILE.TW_OVERSHORT_13,
                   ISNULL(TRWFILE.TW_POSTNAME_14,''),
                   TRWFILE.TW_TIME_14,
                   TRWFILE.TW_AMOUNT_14,
                   TRWFILE.TW_OVERSHORT_14,
                   ISNULL(TRWFILE.TW_POSTNAME_15,''),
                   TRWFILE.TW_TIME_15,
                   TRWFILE.TW_AMOUNT_15,
                   TRWFILE.TW_OVERSHORT_15,
                   ISNULL(TRWFILE.TW_POSTNAME_16,''),
                   TRWFILE.TW_TIME_16,
                   TRWFILE.TW_AMOUNT_16,
                   TRWFILE.TW_OVERSHORT_16,
                   ISNULL(TRWFILE.TW_POSTNAME_17,''),
                   TRWFILE.TW_TIME_17,
                   TRWFILE.TW_AMOUNT_17,
                   TRWFILE.TW_OVERSHORT_17,
                   ISNULL(TRWFILE.TW_POSTNAME_18,''),
                   TRWFILE.TW_TIME_18,
                   TRWFILE.TW_AMOUNT_18,
                   TRWFILE.TW_OVERSHORT_18,
                   ISNULL(TRWFILE.TW_POSTNAME_19,''),
                   TRWFILE.TW_TIME_19,
                   TRWFILE.TW_AMOUNT_19,
                   TRWFILE.TW_OVERSHORT_19,
                   ISNULL(TRWFILE.TW_POSTNAME_20,''),
                   TRWFILE.TW_TIME_20,
                   TRWFILE.TW_AMOUNT_20,
                   TRWFILE.TW_OVERSHORT_20
            INTO :QTRW-REC
            FROM DBO.TRWFILE
            WHERE TRWFILE.TW_DATE = :QTW-DATE
              AND TRWFILE.TW_BRNO = :QTW-BRNO
              AND TRWFILE.TW_TILLNO = :QTW-TILLNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRW2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRW-FIELDS.

      *-----------------------------------------------------------------
       READ-TRW2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRW-PATH-OWNBR  TO TW-BRNO.
           MOVE TRW-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING TW-DATE, TW-BRNO, TW-TILLNO
                  DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QTRW-REC.

           IF ( TRW2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRW2_CURSOR INTO :QTRW-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRW2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRW2-FILE.

           IF ( TRW2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRW2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRW2-CURSOR-STAT.

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
              DESTROY IOTRWR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
