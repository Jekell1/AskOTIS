       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOLTPI.
       AUTHOR.        AMM
       DATE-WRITTEN.  05-12-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOLTPI.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : LTPFILE 
      * TABLE DESC    : LOAN TRAILER PAYMENT SCHEDULE FILE
      * OUTPUT FILE   : XPC/IOLTPI.PRE
      * USED FOR      : LIBLTP/LTPWSLTP.CPY  --- QLTP-REC
      *                 LIBLTP/LTPLTP1R.CPY  --- READ
      *                 LIBLTP/LTPLTP1RN.CPY --- READ/READ-NEXT/START
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
           "@(#)IO/IOLTPI.CBL S34 05/12/21-12:41:48 >".

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
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LPWSLTP.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QLTP1-WHERE-BEG-KEY.
           03  QLTP1-WBEG-BRANCH             PIC 9(04).
           03  QLTP1-WBEG-ACCTNO           PIC 9(08).
           03  QLTP1-WBEG-SEQNO            PIC 9(03).
 
       01  QLTP1-WHERE-END-KEY.
           03  QLTP1-WEND-BRANCH             PIC 9(04).
           03  QLTP1-WEND-ACCTNO           PIC 9(08).
           03  QLTP1-WEND-SEQNO            PIC 9(03).
 
       01  QLTP-REC.
           03  QLTP-BRNO              PIC 9(4).
           03  QLTP-ACCTNO            PIC 9(8).
           03  QLTP-SEQNO             PIC 999.
           03  QLTP-LTOUCH-DATE       PIC X(10).
           03  QLTP-SCHLD-TERM-1     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-1   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-2     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-2   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-3     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-3   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-4     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-4   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-5     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-5   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-6     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-6   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-7     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-7   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-8     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-8   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-9     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-9   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-10    PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-10  PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-11     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-11   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-12     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-12   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-13     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-13   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-14     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-14   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-15     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-15   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-16     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-16   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-17     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-17   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-18     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-18   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-19     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-19   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-20     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-20   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-21     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-21   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-22     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-22   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-23     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-23   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-24     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-24   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-25     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-25   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-26     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-26   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-27     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-27   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-28     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-28   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-29     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-29   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-30     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-30   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-31     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-31   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-32     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-32   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-33     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-33   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-34     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-34   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-35     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-35   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-36     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-36   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-37     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-37   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-38     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-38   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-39     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-39   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-40     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-40   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-41     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-41   PIC 9(6)V99.
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
           03  FILLER PIC X(34) VALUE "BRNO    ACCRTNO    SEQ   TERM1".
           03  FILLER PIC X(30) VALUE "PAY1      TERM2    PAY2".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-LTP-BRNO                   PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-LTP-ACCTNO                 PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-LTP-SEQNO                  PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-LTP-TERM-1                 PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-LTP-PAYMNT-1               PIC 9(06).99.
           03  FILLER                         PIC X(03).
           03  D01-LTP-TERM-2                 PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-LTP-PAYMNT-2               PIC 9(06).99.

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
       COPY "LIBIO/IOLTPI_DEF.CPY".
       COPY "LIBIO/IOLTPI_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOLTPI_SCN.CPY".

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
           MOVE "IOLTPI"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOLTPI-QUEUE-POS  TO Q-POS.
           MOVE IOLTPI-COPIES-POS TO C-POS.
           MOVE IOLTPI-PRU-POS    TO P-POS.
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

           GO TO ENTER-BEG-ACCTNO.

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

           MOVE "B"                        TO LTP-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QLTP1-WBEG-BRANCH.
           MOVE BR-NO     TO QLTP1-WEND-BRANCH.

           MOVE BEG-ACCTNO TO QLTP1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QLTP1-WEND-ACCTNO.
           MOVE BEG-SEQNO  TO QLTP1-WBEG-SEQNO.
           MOVE END-SEQNO  TO QLTP1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-LTP1-FILE.
           PERFORM START-LTP1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-LTP1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LTP-BRNO NOT = LTP-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( LTP-ACCTNO < BEG-ACCTNO )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( LTP-ACCTNO > END-ACCTNO )
              GO TO PROCESS-BRANCH-MODULE-END.

           IF LTP-SEQNO < BEG-SEQNO OR > END-SEQNO
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE LTP-BRNO                    TO D01-LTP-BRNO.
           MOVE LTP-ACCTNO                  TO D01-LTP-ACCTNO.
           MOVE LTP-SEQNO                   TO D01-LTP-SEQNO.
           MOVE LTP-SCHLD-TERM(1)           TO D01-LTP-TERM-1.
           MOVE LTP-SCHLD-PAYMNT(1)         TO D01-LTP-PAYMNT-1.
           MOVE LTP-SCHLD-TERM(2)           TO D01-LTP-TERM-2.
           MOVE LTP-SCHLD-PAYMNT(2)         TO D01-LTP-PAYMNT-2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-LTP1-FILE.

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

           MOVE LTP1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "SQL TABLE IO UTILITY - LTPFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOLTPI-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOLTPI-SYSDATE.
           DISPLAY IOLTPI-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOLTPI-FORM-FIELDS TO WR-BUF.
           MOVE IOLTPI-FORM-LIST TO WR-LIST.
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

           MOVE IOLTPI-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOLTPI-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOLTPI_EVA.CPY".
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
      *    LOAD LTP-REC FROM QLTP-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LTP-FIELDS.
           MOVE QLTP-BRNO TO LTP-BRNO.
           MOVE QLTP-ACCTNO TO LTP-ACCTNO.
           MOVE QLTP-SEQNO TO LTP-SEQNO.

           IF ( QLTP-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LTP-LTOUCH-DATE
           ELSE
              MOVE QLTP-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTP-LTOUCH-DATE.

           MOVE QLTP-SCHLD-TERM-1 TO LTP-SCHLD-TERM(1).
           MOVE QLTP-SCHLD-PAYMNT-1 TO LTP-SCHLD-PAYMNT(1).
           MOVE QLTP-SCHLD-TERM-2 TO LTP-SCHLD-TERM(2).
           MOVE QLTP-SCHLD-PAYMNT-2 TO LTP-SCHLD-PAYMNT(2).
           MOVE QLTP-SCHLD-TERM-3 TO LTP-SCHLD-TERM(3).
           MOVE QLTP-SCHLD-PAYMNT-3 TO LTP-SCHLD-PAYMNT(3).
           MOVE QLTP-SCHLD-TERM-4 TO LTP-SCHLD-TERM(4).
           MOVE QLTP-SCHLD-PAYMNT-4 TO LTP-SCHLD-PAYMNT(4).
           MOVE QLTP-SCHLD-TERM-5 TO LTP-SCHLD-TERM(5).
           MOVE QLTP-SCHLD-PAYMNT-5 TO LTP-SCHLD-PAYMNT(5).
           MOVE QLTP-SCHLD-TERM-6 TO LTP-SCHLD-TERM(6).
           MOVE QLTP-SCHLD-PAYMNT-6 TO LTP-SCHLD-PAYMNT(6).
           MOVE QLTP-SCHLD-TERM-7 TO LTP-SCHLD-TERM(7).
           MOVE QLTP-SCHLD-PAYMNT-7 TO LTP-SCHLD-PAYMNT(7).
           MOVE QLTP-SCHLD-TERM-8 TO LTP-SCHLD-TERM(8).
           MOVE QLTP-SCHLD-PAYMNT-8 TO LTP-SCHLD-PAYMNT(8).
           MOVE QLTP-SCHLD-TERM-9 TO LTP-SCHLD-TERM(9).
           MOVE QLTP-SCHLD-PAYMNT-9 TO LTP-SCHLD-PAYMNT(9).
           MOVE QLTP-SCHLD-TERM-10 TO LTP-SCHLD-TERM(10).
           MOVE QLTP-SCHLD-PAYMNT-10 TO LTP-SCHLD-PAYMNT(10).
           MOVE QLTP-SCHLD-TERM-11 TO LTP-SCHLD-TERM(11).
           MOVE QLTP-SCHLD-PAYMNT-11 TO LTP-SCHLD-PAYMNT(11).
           MOVE QLTP-SCHLD-TERM-12 TO LTP-SCHLD-TERM(12).
           MOVE QLTP-SCHLD-PAYMNT-12 TO LTP-SCHLD-PAYMNT(12).
           MOVE QLTP-SCHLD-TERM-13 TO LTP-SCHLD-TERM(13).
           MOVE QLTP-SCHLD-PAYMNT-13 TO LTP-SCHLD-PAYMNT(13).
           MOVE QLTP-SCHLD-TERM-14 TO LTP-SCHLD-TERM(14).
           MOVE QLTP-SCHLD-PAYMNT-14 TO LTP-SCHLD-PAYMNT(14).
           MOVE QLTP-SCHLD-TERM-15 TO LTP-SCHLD-TERM(15).
           MOVE QLTP-SCHLD-PAYMNT-15 TO LTP-SCHLD-PAYMNT(15).
           MOVE QLTP-SCHLD-TERM-16 TO LTP-SCHLD-TERM(16).
           MOVE QLTP-SCHLD-PAYMNT-16 TO LTP-SCHLD-PAYMNT(16).
           MOVE QLTP-SCHLD-TERM-17 TO LTP-SCHLD-TERM(17).
           MOVE QLTP-SCHLD-PAYMNT-17 TO LTP-SCHLD-PAYMNT(17).
           MOVE QLTP-SCHLD-TERM-18 TO LTP-SCHLD-TERM(18).
           MOVE QLTP-SCHLD-PAYMNT-18 TO LTP-SCHLD-PAYMNT(18).
           MOVE QLTP-SCHLD-TERM-19 TO LTP-SCHLD-TERM(19).
           MOVE QLTP-SCHLD-PAYMNT-19 TO LTP-SCHLD-PAYMNT(19).
           MOVE QLTP-SCHLD-TERM-20 TO LTP-SCHLD-TERM(20).
           MOVE QLTP-SCHLD-PAYMNT-20 TO LTP-SCHLD-PAYMNT(20).
           MOVE QLTP-SCHLD-TERM-21 TO LTP-SCHLD-TERM(21).
           MOVE QLTP-SCHLD-PAYMNT-21 TO LTP-SCHLD-PAYMNT(21).
           MOVE QLTP-SCHLD-TERM-22 TO LTP-SCHLD-TERM(22).
           MOVE QLTP-SCHLD-PAYMNT-22 TO LTP-SCHLD-PAYMNT(22).
           MOVE QLTP-SCHLD-TERM-23 TO LTP-SCHLD-TERM(23).
           MOVE QLTP-SCHLD-PAYMNT-23 TO LTP-SCHLD-PAYMNT(23).
           MOVE QLTP-SCHLD-TERM-24 TO LTP-SCHLD-TERM(24).
           MOVE QLTP-SCHLD-PAYMNT-24 TO LTP-SCHLD-PAYMNT(24).
           MOVE QLTP-SCHLD-TERM-25 TO LTP-SCHLD-TERM(25).
           MOVE QLTP-SCHLD-PAYMNT-25 TO LTP-SCHLD-PAYMNT(25).
           MOVE QLTP-SCHLD-TERM-26 TO LTP-SCHLD-TERM(26).
           MOVE QLTP-SCHLD-PAYMNT-26 TO LTP-SCHLD-PAYMNT(26).
           MOVE QLTP-SCHLD-TERM-27 TO LTP-SCHLD-TERM(27).
           MOVE QLTP-SCHLD-PAYMNT-27 TO LTP-SCHLD-PAYMNT(27).
           MOVE QLTP-SCHLD-TERM-28 TO LTP-SCHLD-TERM(28).
           MOVE QLTP-SCHLD-PAYMNT-28 TO LTP-SCHLD-PAYMNT(28).
           MOVE QLTP-SCHLD-TERM-29 TO LTP-SCHLD-TERM(29).
           MOVE QLTP-SCHLD-PAYMNT-29 TO LTP-SCHLD-PAYMNT(29).
           MOVE QLTP-SCHLD-TERM-30 TO LTP-SCHLD-TERM(30).
           MOVE QLTP-SCHLD-PAYMNT-30 TO LTP-SCHLD-PAYMNT(30).
           MOVE QLTP-SCHLD-TERM-31 TO LTP-SCHLD-TERM(31).
           MOVE QLTP-SCHLD-PAYMNT-31 TO LTP-SCHLD-PAYMNT(31).
           MOVE QLTP-SCHLD-TERM-32 TO LTP-SCHLD-TERM(32).
           MOVE QLTP-SCHLD-PAYMNT-32 TO LTP-SCHLD-PAYMNT(32).
           MOVE QLTP-SCHLD-TERM-33 TO LTP-SCHLD-TERM(33).
           MOVE QLTP-SCHLD-PAYMNT-33 TO LTP-SCHLD-PAYMNT(33).
           MOVE QLTP-SCHLD-TERM-34 TO LTP-SCHLD-TERM(34).
           MOVE QLTP-SCHLD-PAYMNT-34 TO LTP-SCHLD-PAYMNT(34).
           MOVE QLTP-SCHLD-TERM-35 TO LTP-SCHLD-TERM(35).
           MOVE QLTP-SCHLD-PAYMNT-35 TO LTP-SCHLD-PAYMNT(35).
           MOVE QLTP-SCHLD-TERM-36 TO LTP-SCHLD-TERM(36).
           MOVE QLTP-SCHLD-PAYMNT-36 TO LTP-SCHLD-PAYMNT(36).
           MOVE QLTP-SCHLD-TERM-37 TO LTP-SCHLD-TERM(37).
           MOVE QLTP-SCHLD-PAYMNT-37 TO LTP-SCHLD-PAYMNT(37).
           MOVE QLTP-SCHLD-TERM-38 TO LTP-SCHLD-TERM(38).
           MOVE QLTP-SCHLD-PAYMNT-38 TO LTP-SCHLD-PAYMNT(38).
           MOVE QLTP-SCHLD-TERM-39 TO LTP-SCHLD-TERM(39).
           MOVE QLTP-SCHLD-PAYMNT-39 TO LTP-SCHLD-PAYMNT(39).
           MOVE QLTP-SCHLD-TERM-40 TO LTP-SCHLD-TERM(40).
           MOVE QLTP-SCHLD-PAYMNT-40 TO LTP-SCHLD-PAYMNT(40).
           MOVE QLTP-SCHLD-TERM-41 TO LTP-SCHLD-TERM(41).
           MOVE QLTP-SCHLD-PAYMNT-41 TO LTP-SCHLD-PAYMNT(41).
 
      *-----------------------------------------------------------------
      *    LOAD QLTP-REC FROM LTP-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LTP-FIELDS.
           MOVE LTP-BRNO TO QLTP-BRNO.
           MOVE LTP-ACCTNO TO QLTP-ACCTNO.
           MOVE LTP-SEQNO TO QLTP-SEQNO.

           IF ( LTP-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTP-LTOUCH-DATE
           ELSE
              MOVE LTP-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTP-LTOUCH-DATE.

           MOVE LTP-SCHLD-TERM(1) TO QLTP-SCHLD-TERM-1.
           MOVE LTP-SCHLD-PAYMNT(1) TO QLTP-SCHLD-PAYMNT-1.
           MOVE LTP-SCHLD-TERM(2) TO QLTP-SCHLD-TERM-2.
           MOVE LTP-SCHLD-PAYMNT(2) TO QLTP-SCHLD-PAYMNT-2.
           MOVE LTP-SCHLD-TERM(3) TO QLTP-SCHLD-TERM-3.
           MOVE LTP-SCHLD-PAYMNT(3) TO QLTP-SCHLD-PAYMNT-3.
           MOVE LTP-SCHLD-TERM(4) TO QLTP-SCHLD-TERM-4.
           MOVE LTP-SCHLD-PAYMNT(4) TO QLTP-SCHLD-PAYMNT-4.
           MOVE LTP-SCHLD-TERM(5) TO QLTP-SCHLD-TERM-5.
           MOVE LTP-SCHLD-PAYMNT(5) TO QLTP-SCHLD-PAYMNT-5.
           MOVE LTP-SCHLD-TERM(6) TO QLTP-SCHLD-TERM-6.
           MOVE LTP-SCHLD-PAYMNT(6) TO QLTP-SCHLD-PAYMNT-6.
           MOVE LTP-SCHLD-TERM(7) TO QLTP-SCHLD-TERM-7.
           MOVE LTP-SCHLD-PAYMNT(7) TO QLTP-SCHLD-PAYMNT-7.
           MOVE LTP-SCHLD-TERM(8) TO QLTP-SCHLD-TERM-8.
           MOVE LTP-SCHLD-PAYMNT(8) TO QLTP-SCHLD-PAYMNT-8.
           MOVE LTP-SCHLD-TERM(9) TO QLTP-SCHLD-TERM-9.
           MOVE LTP-SCHLD-PAYMNT(9) TO QLTP-SCHLD-PAYMNT-9.
           MOVE LTP-SCHLD-TERM(10) TO QLTP-SCHLD-TERM-10.
           MOVE LTP-SCHLD-PAYMNT(10) TO QLTP-SCHLD-PAYMNT-10.
           MOVE LTP-SCHLD-TERM(11) TO QLTP-SCHLD-TERM-11.
           MOVE LTP-SCHLD-PAYMNT(11) TO QLTP-SCHLD-PAYMNT-11.
           MOVE LTP-SCHLD-TERM(12) TO QLTP-SCHLD-TERM-12.
           MOVE LTP-SCHLD-PAYMNT(12) TO QLTP-SCHLD-PAYMNT-12.
           MOVE LTP-SCHLD-TERM(13) TO QLTP-SCHLD-TERM-13.
           MOVE LTP-SCHLD-PAYMNT(13) TO QLTP-SCHLD-PAYMNT-13.
           MOVE LTP-SCHLD-TERM(14) TO QLTP-SCHLD-TERM-14.
           MOVE LTP-SCHLD-PAYMNT(14) TO QLTP-SCHLD-PAYMNT-14.
           MOVE LTP-SCHLD-TERM(15) TO QLTP-SCHLD-TERM-15.
           MOVE LTP-SCHLD-PAYMNT(15) TO QLTP-SCHLD-PAYMNT-15.
           MOVE LTP-SCHLD-TERM(16) TO QLTP-SCHLD-TERM-16.
           MOVE LTP-SCHLD-PAYMNT(16) TO QLTP-SCHLD-PAYMNT-16.
           MOVE LTP-SCHLD-TERM(17) TO QLTP-SCHLD-TERM-17.
           MOVE LTP-SCHLD-PAYMNT(17) TO QLTP-SCHLD-PAYMNT-17.
           MOVE LTP-SCHLD-TERM(18) TO QLTP-SCHLD-TERM-18.
           MOVE LTP-SCHLD-PAYMNT(18) TO QLTP-SCHLD-PAYMNT-18.
           MOVE LTP-SCHLD-TERM(19) TO QLTP-SCHLD-TERM-19.
           MOVE LTP-SCHLD-PAYMNT(19) TO QLTP-SCHLD-PAYMNT-19.
           MOVE LTP-SCHLD-TERM(20) TO QLTP-SCHLD-TERM-20.
           MOVE LTP-SCHLD-PAYMNT(20) TO QLTP-SCHLD-PAYMNT-20.
           MOVE LTP-SCHLD-TERM(21) TO QLTP-SCHLD-TERM-21.
           MOVE LTP-SCHLD-PAYMNT(21) TO QLTP-SCHLD-PAYMNT-21.
           MOVE LTP-SCHLD-TERM(22) TO QLTP-SCHLD-TERM-22.
           MOVE LTP-SCHLD-PAYMNT(22) TO QLTP-SCHLD-PAYMNT-22.
           MOVE LTP-SCHLD-TERM(23) TO QLTP-SCHLD-TERM-23.
           MOVE LTP-SCHLD-PAYMNT(23) TO QLTP-SCHLD-PAYMNT-23.
           MOVE LTP-SCHLD-TERM(24) TO QLTP-SCHLD-TERM-24.
           MOVE LTP-SCHLD-PAYMNT(24) TO QLTP-SCHLD-PAYMNT-24.
           MOVE LTP-SCHLD-TERM(25) TO QLTP-SCHLD-TERM-25.
           MOVE LTP-SCHLD-PAYMNT(25) TO QLTP-SCHLD-PAYMNT-25.
           MOVE LTP-SCHLD-TERM(26) TO QLTP-SCHLD-TERM-26.
           MOVE LTP-SCHLD-PAYMNT(26) TO QLTP-SCHLD-PAYMNT-26.
           MOVE LTP-SCHLD-TERM(27) TO QLTP-SCHLD-TERM-27.
           MOVE LTP-SCHLD-PAYMNT(27) TO QLTP-SCHLD-PAYMNT-27.
           MOVE LTP-SCHLD-TERM(28) TO QLTP-SCHLD-TERM-28.
           MOVE LTP-SCHLD-PAYMNT(28) TO QLTP-SCHLD-PAYMNT-28.
           MOVE LTP-SCHLD-TERM(29) TO QLTP-SCHLD-TERM-29.
           MOVE LTP-SCHLD-PAYMNT(29) TO QLTP-SCHLD-PAYMNT-29.
           MOVE LTP-SCHLD-TERM(30) TO QLTP-SCHLD-TERM-30.
           MOVE LTP-SCHLD-PAYMNT(30) TO QLTP-SCHLD-PAYMNT-30.
           MOVE LTP-SCHLD-TERM(31) TO QLTP-SCHLD-TERM-31.
           MOVE LTP-SCHLD-PAYMNT(31) TO QLTP-SCHLD-PAYMNT-31.
           MOVE LTP-SCHLD-TERM(32) TO QLTP-SCHLD-TERM-32.
           MOVE LTP-SCHLD-PAYMNT(32) TO QLTP-SCHLD-PAYMNT-32.
           MOVE LTP-SCHLD-TERM(33) TO QLTP-SCHLD-TERM-33.
           MOVE LTP-SCHLD-PAYMNT(33) TO QLTP-SCHLD-PAYMNT-33.
           MOVE LTP-SCHLD-TERM(34) TO QLTP-SCHLD-TERM-34.
           MOVE LTP-SCHLD-PAYMNT(34) TO QLTP-SCHLD-PAYMNT-34.
           MOVE LTP-SCHLD-TERM(35) TO QLTP-SCHLD-TERM-35.
           MOVE LTP-SCHLD-PAYMNT(35) TO QLTP-SCHLD-PAYMNT-35.
           MOVE LTP-SCHLD-TERM(36) TO QLTP-SCHLD-TERM-36.
           MOVE LTP-SCHLD-PAYMNT(36) TO QLTP-SCHLD-PAYMNT-36.
           MOVE LTP-SCHLD-TERM(37) TO QLTP-SCHLD-TERM-37.
           MOVE LTP-SCHLD-PAYMNT(37) TO QLTP-SCHLD-PAYMNT-37.
           MOVE LTP-SCHLD-TERM(38) TO QLTP-SCHLD-TERM-38.
           MOVE LTP-SCHLD-PAYMNT(38) TO QLTP-SCHLD-PAYMNT-38.
           MOVE LTP-SCHLD-TERM(39) TO QLTP-SCHLD-TERM-39.
           MOVE LTP-SCHLD-PAYMNT(39) TO QLTP-SCHLD-PAYMNT-39.
           MOVE LTP-SCHLD-TERM(40) TO QLTP-SCHLD-TERM-40.
           MOVE LTP-SCHLD-PAYMNT(40) TO QLTP-SCHLD-PAYMNT-40.
           MOVE LTP-SCHLD-TERM(41) TO QLTP-SCHLD-TERM-41.
           MOVE LTP-SCHLD-PAYMNT(41) TO QLTP-SCHLD-PAYMNT-41.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LTP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTP-ALL-DATA
              MOVE LTP-ALL-PATH             TO LTP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTP-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTP-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTP-ALL-DATA
              MOVE LTP-ALL-PATH             TO LTP-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LTPFILE NOT = "S" )
              MOVE LTP-PATH-BEG             TO LTP-SELECT-PATH-BEG
              MOVE LTP-PATH-FILE            TO LTP-SELECT-PATH-FILE
           ELSE
              MOVE LTP-PATH-SQL             TO LTP-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LTP1-FILE.
           PERFORM LOAD-LTP1-FILE.
           PERFORM OPEN-IT.
           MOVE LTP-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTP1-FILE.
           PERFORM START-IT.
           MOVE LTP-PATH-OWNBR  TO LTP-BRNO.
           MOVE LTP-SELECT-PATH TO E-FILE.
           MOVE LTP1-KEY        TO E-KEYX.

           IF ( LTP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTP1-FILE.

           IF ( QLTP1-WEND-BRANCH = ZEROES    ) OR
              ( QLTP1-WEND-BRANCH NOT NUMERIC )
              MOVE LTP-PATH-OWNBR        TO QLTP1-WBEG-BRANCH
              MOVE LTP-PATH-OWNBR        TO QLTP1-WEND-BRANCH
              MOVE LTP-ACCTNO            TO QLTP1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTP1-WEND-ACCTNO
              MOVE LTP-SEQNO             TO QLTP1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTP1-WEND-SEQNO.

           EXEC SQL
            DECLARE LTP1_CURSOR CURSOR FOR
            SELECT LTPFILE.LTP_BRNO,
                   LTPFILE.LTP_ACCTNO,
                   LTPFILE.LTP_SEQNO,
                   ISNULL(CAST(LTPFILE.LTP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LTPFILE.LTP_SCHLD_TERM_1,
                   LTPFILE.LTP_SCHLD_PAYMNT_1,
                   LTPFILE.LTP_SCHLD_TERM_2,
                   LTPFILE.LTP_SCHLD_PAYMNT_2,
                   LTPFILE.LTP_SCHLD_TERM_3,
                   LTPFILE.LTP_SCHLD_PAYMNT_3,
                   LTPFILE.LTP_SCHLD_TERM_4,
                   LTPFILE.LTP_SCHLD_PAYMNT_4,
                   LTPFILE.LTP_SCHLD_TERM_5,
                   LTPFILE.LTP_SCHLD_PAYMNT_5,
                   LTPFILE.LTP_SCHLD_TERM_6,
                   LTPFILE.LTP_SCHLD_PAYMNT_6,
                   LTPFILE.LTP_SCHLD_TERM_7,
                   LTPFILE.LTP_SCHLD_PAYMNT_7,
                   LTPFILE.LTP_SCHLD_TERM_8,
                   LTPFILE.LTP_SCHLD_PAYMNT_8,
                   LTPFILE.LTP_SCHLD_TERM_9,
                   LTPFILE.LTP_SCHLD_PAYMNT_9,
                   LTPFILE.LTP_SCHLD_TERM_10,
                   LTPFILE.LTP_SCHLD_PAYMNT_10,
                   LTPFILE.LTP_SCHLD_TERM_11,
                   LTPFILE.LTP_SCHLD_PAYMNT_11,
                   LTPFILE.LTP_SCHLD_TERM_12,
                   LTPFILE.LTP_SCHLD_PAYMNT_12,
                   LTPFILE.LTP_SCHLD_TERM_13,
                   LTPFILE.LTP_SCHLD_PAYMNT_13,
                   LTPFILE.LTP_SCHLD_TERM_14,
                   LTPFILE.LTP_SCHLD_PAYMNT_14,
                   LTPFILE.LTP_SCHLD_TERM_15,
                   LTPFILE.LTP_SCHLD_PAYMNT_15,
                   LTPFILE.LTP_SCHLD_TERM_16,
                   LTPFILE.LTP_SCHLD_PAYMNT_16,
                   LTPFILE.LTP_SCHLD_TERM_17,
                   LTPFILE.LTP_SCHLD_PAYMNT_17,
                   LTPFILE.LTP_SCHLD_TERM_18,
                   LTPFILE.LTP_SCHLD_PAYMNT_18,
                   LTPFILE.LTP_SCHLD_TERM_19,
                   LTPFILE.LTP_SCHLD_PAYMNT_19,
                   LTPFILE.LTP_SCHLD_TERM_20,
                   LTPFILE.LTP_SCHLD_PAYMNT_20,
                   LTPFILE.LTP_SCHLD_TERM_21,
                   LTPFILE.LTP_SCHLD_PAYMNT_21,
                   LTPFILE.LTP_SCHLD_TERM_22,
                   LTPFILE.LTP_SCHLD_PAYMNT_22,
                   LTPFILE.LTP_SCHLD_TERM_23,
                   LTPFILE.LTP_SCHLD_PAYMNT_23,
                   LTPFILE.LTP_SCHLD_TERM_24,
                   LTPFILE.LTP_SCHLD_PAYMNT_24,
                   LTPFILE.LTP_SCHLD_TERM_25,
                   LTPFILE.LTP_SCHLD_PAYMNT_25,
                   LTPFILE.LTP_SCHLD_TERM_26,
                   LTPFILE.LTP_SCHLD_PAYMNT_26,
                   LTPFILE.LTP_SCHLD_TERM_27,
                   LTPFILE.LTP_SCHLD_PAYMNT_27,
                   LTPFILE.LTP_SCHLD_TERM_28,
                   LTPFILE.LTP_SCHLD_PAYMNT_28,
                   LTPFILE.LTP_SCHLD_TERM_29,
                   LTPFILE.LTP_SCHLD_PAYMNT_29,
                   LTPFILE.LTP_SCHLD_TERM_30,
                   LTPFILE.LTP_SCHLD_PAYMNT_30,
                   LTPFILE.LTP_SCHLD_TERM_31,
                   LTPFILE.LTP_SCHLD_PAYMNT_31,
                   LTPFILE.LTP_SCHLD_TERM_32,
                   LTPFILE.LTP_SCHLD_PAYMNT_32,
                   LTPFILE.LTP_SCHLD_TERM_33,
                   LTPFILE.LTP_SCHLD_PAYMNT_33,
                   LTPFILE.LTP_SCHLD_TERM_34,
                   LTPFILE.LTP_SCHLD_PAYMNT_34,
                   LTPFILE.LTP_SCHLD_TERM_35,
                   LTPFILE.LTP_SCHLD_PAYMNT_35,
                   LTPFILE.LTP_SCHLD_TERM_36,
                   LTPFILE.LTP_SCHLD_PAYMNT_36,
                   LTPFILE.LTP_SCHLD_TERM_37,
                   LTPFILE.LTP_SCHLD_PAYMNT_37,
                   LTPFILE.LTP_SCHLD_TERM_38,
                   LTPFILE.LTP_SCHLD_PAYMNT_38,
                   LTPFILE.LTP_SCHLD_TERM_39,
                   LTPFILE.LTP_SCHLD_PAYMNT_39,
                   LTPFILE.LTP_SCHLD_TERM_40,
                   LTPFILE.LTP_SCHLD_PAYMNT_40,
                   LTPFILE.LTP_SCHLD_TERM_41,
                   LTPFILE.LTP_SCHLD_PAYMNT_41
            FROM DBO.LTPFILE
            WHERE LTPFILE.LTP_BRNO >= :QLTP1-WBEG-BRANCH
              AND LTPFILE.LTP_BRNO <= :QLTP1-WEND-BRANCH
              AND LTPFILE.LTP_ACCTNO >= :QLTP1-WBEG-ACCTNO
              AND LTPFILE.LTP_ACCTNO <= :QLTP1-WEND-ACCTNO
              AND LTPFILE.LTP_SEQNO >= :QLTP1-WEND-SEQNO
              AND LTPFILE.LTP_SEQNO <= :QLTP1-WEND-SEQNO
            ORDER BY LTPFILE.LTP_BRNO,
                     LTPFILE.LTP_ACCTNO,
                     LTPFILE.LTP_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTP1-WBEG-BRANCH 
                          QLTP1-WEND-BRANCH
                          QLTP1-WBEG-ACCTNO
                          QLTP1-WEND-ACCTNO
                          QLTP1-WBEG-SEQNO
                          QLTP1-WEND-SEQNO.

           MOVE STAT---GOOD TO LTP1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTP1-FILE.
           PERFORM READ-IT.
           MOVE LTP-PATH-OWNBR  TO LTP-BRNO. 
           MOVE LTP-SELECT-PATH TO E-FILE.
           MOVE LTP1-KEY        TO E-KEYX.

           MOVE LTP-BRNO   TO QLTP-BRNO.
           MOVE LTP-ACCTNO TO QLTP-ACCTNO.
           MOVE LTP-SEQNO  TO QLTP-SEQNO.

           EXEC SQL
            SELECT LTPFILE.LTP_BRNO,
                   LTPFILE.LTP_ACCTNO,
                   LTPFILE.LTP_SEQNO,
                   ISNULL(CAST(LTPFILE.LTP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LTPFILE.LTP_SCHLD_TERM_1,
                   LTPFILE.LTP_SCHLD_PAYMNT_1,
                   LTPFILE.LTP_SCHLD_TERM_2,
                   LTPFILE.LTP_SCHLD_PAYMNT_2,
                   LTPFILE.LTP_SCHLD_TERM_3,
                   LTPFILE.LTP_SCHLD_PAYMNT_3,
                   LTPFILE.LTP_SCHLD_TERM_4,
                   LTPFILE.LTP_SCHLD_PAYMNT_4,
                   LTPFILE.LTP_SCHLD_TERM_5,
                   LTPFILE.LTP_SCHLD_PAYMNT_5,
                   LTPFILE.LTP_SCHLD_TERM_6,
                   LTPFILE.LTP_SCHLD_PAYMNT_6,
                   LTPFILE.LTP_SCHLD_TERM_7,
                   LTPFILE.LTP_SCHLD_PAYMNT_7,
                   LTPFILE.LTP_SCHLD_TERM_8,
                   LTPFILE.LTP_SCHLD_PAYMNT_8,
                   LTPFILE.LTP_SCHLD_TERM_9,
                   LTPFILE.LTP_SCHLD_PAYMNT_9,
                   LTPFILE.LTP_SCHLD_TERM_10,
                   LTPFILE.LTP_SCHLD_PAYMNT_10,
                   LTPFILE.LTP_SCHLD_TERM_11,
                   LTPFILE.LTP_SCHLD_PAYMNT_11,
                   LTPFILE.LTP_SCHLD_TERM_12,
                   LTPFILE.LTP_SCHLD_PAYMNT_12,
                   LTPFILE.LTP_SCHLD_TERM_13,
                   LTPFILE.LTP_SCHLD_PAYMNT_13,
                   LTPFILE.LTP_SCHLD_TERM_14,
                   LTPFILE.LTP_SCHLD_PAYMNT_14,
                   LTPFILE.LTP_SCHLD_TERM_15,
                   LTPFILE.LTP_SCHLD_PAYMNT_15,
                   LTPFILE.LTP_SCHLD_TERM_16,
                   LTPFILE.LTP_SCHLD_PAYMNT_16,
                   LTPFILE.LTP_SCHLD_TERM_17,
                   LTPFILE.LTP_SCHLD_PAYMNT_17,
                   LTPFILE.LTP_SCHLD_TERM_18,
                   LTPFILE.LTP_SCHLD_PAYMNT_18,
                   LTPFILE.LTP_SCHLD_TERM_19,
                   LTPFILE.LTP_SCHLD_PAYMNT_19,
                   LTPFILE.LTP_SCHLD_TERM_20,
                   LTPFILE.LTP_SCHLD_PAYMNT_20,
                   LTPFILE.LTP_SCHLD_TERM_21,
                   LTPFILE.LTP_SCHLD_PAYMNT_21,
                   LTPFILE.LTP_SCHLD_TERM_22,
                   LTPFILE.LTP_SCHLD_PAYMNT_22,
                   LTPFILE.LTP_SCHLD_TERM_23,
                   LTPFILE.LTP_SCHLD_PAYMNT_23,
                   LTPFILE.LTP_SCHLD_TERM_24,
                   LTPFILE.LTP_SCHLD_PAYMNT_24,
                   LTPFILE.LTP_SCHLD_TERM_25,
                   LTPFILE.LTP_SCHLD_PAYMNT_25,
                   LTPFILE.LTP_SCHLD_TERM_26,
                   LTPFILE.LTP_SCHLD_PAYMNT_26,
                   LTPFILE.LTP_SCHLD_TERM_27,
                   LTPFILE.LTP_SCHLD_PAYMNT_27,
                   LTPFILE.LTP_SCHLD_TERM_28,
                   LTPFILE.LTP_SCHLD_PAYMNT_28,
                   LTPFILE.LTP_SCHLD_TERM_29,
                   LTPFILE.LTP_SCHLD_PAYMNT_29,
                   LTPFILE.LTP_SCHLD_TERM_30,
                   LTPFILE.LTP_SCHLD_PAYMNT_30,
                   LTPFILE.LTP_SCHLD_TERM_31,
                   LTPFILE.LTP_SCHLD_PAYMNT_31,
                   LTPFILE.LTP_SCHLD_TERM_32,
                   LTPFILE.LTP_SCHLD_PAYMNT_32,
                   LTPFILE.LTP_SCHLD_TERM_33,
                   LTPFILE.LTP_SCHLD_PAYMNT_33,
                   LTPFILE.LTP_SCHLD_TERM_34,
                   LTPFILE.LTP_SCHLD_PAYMNT_34,
                   LTPFILE.LTP_SCHLD_TERM_35,
                   LTPFILE.LTP_SCHLD_PAYMNT_35,
                   LTPFILE.LTP_SCHLD_TERM_36,
                   LTPFILE.LTP_SCHLD_PAYMNT_36,
                   LTPFILE.LTP_SCHLD_TERM_37,
                   LTPFILE.LTP_SCHLD_PAYMNT_37,
                   LTPFILE.LTP_SCHLD_TERM_38,
                   LTPFILE.LTP_SCHLD_PAYMNT_38,
                   LTPFILE.LTP_SCHLD_TERM_39,
                   LTPFILE.LTP_SCHLD_PAYMNT_39,
                   LTPFILE.LTP_SCHLD_TERM_40,
                   LTPFILE.LTP_SCHLD_PAYMNT_40,
                   LTPFILE.LTP_SCHLD_TERM_41,
                   LTPFILE.LTP_SCHLD_PAYMNT_41
            INTO :QLTP-REC
            FROM DBO.LTPFILE
            WHERE LTPFILE.LTP_BRNO = :QLTP-BRNO
              AND LTPFILE.LTP_ACCTNO = :QLTP-ACCTNO
              AND LTPFILE.LTP_SEQNO = :QLTP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTP-FIELDS.

      *-----------------------------------------------------------------
       READ-LTP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTP-PATH-OWNBR  TO LTP-BRNO.
           MOVE LTP-SELECT-PATH TO E-FILE.
           MOVE LTP1-KEY        TO E-KEYX.
           INITIALIZE QLTP-REC.

           IF ( LTP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTP1_CURSOR INTO :QLTP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTP-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LTP1-FILE.

           MOVE LTP-PATH-OWNBR  TO LTP-BRNO.
           ACCEPT LTP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LTP-SELECT-PATH TO E-FILE.
           MOVE LTP1-KEY        TO E-KEYX.
           PERFORM SET-LTP-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LTPFILE
             (LTPFILE.LTP_BRNO,
              LTPFILE.LTP_ACCTNO,
              LTPFILE.LTP_SEQNO,
              LTPFILE.LTP_LTOUCH_DATE,
              LTPFILE.LTP_SCHLD_TERM_1,
              LTPFILE.LTP_SCHLD_PAYMNT_1,
              LTPFILE.LTP_SCHLD_TERM_2,
              LTPFILE.LTP_SCHLD_PAYMNT_2,
              LTPFILE.LTP_SCHLD_TERM_3,
              LTPFILE.LTP_SCHLD_PAYMNT_3,
              LTPFILE.LTP_SCHLD_TERM_4,
              LTPFILE.LTP_SCHLD_PAYMNT_4,
              LTPFILE.LTP_SCHLD_TERM_5,
              LTPFILE.LTP_SCHLD_PAYMNT_5,
              LTPFILE.LTP_SCHLD_TERM_6,
              LTPFILE.LTP_SCHLD_PAYMNT_6,
              LTPFILE.LTP_SCHLD_TERM_7,
              LTPFILE.LTP_SCHLD_PAYMNT_7,
              LTPFILE.LTP_SCHLD_TERM_8,
              LTPFILE.LTP_SCHLD_PAYMNT_8,
              LTPFILE.LTP_SCHLD_TERM_9,
              LTPFILE.LTP_SCHLD_PAYMNT_9,
              LTPFILE.LTP_SCHLD_TERM_10,
              LTPFILE.LTP_SCHLD_PAYMNT_10,
              LTPFILE.LTP_SCHLD_TERM_11,
              LTPFILE.LTP_SCHLD_PAYMNT_11,
              LTPFILE.LTP_SCHLD_TERM_12,
              LTPFILE.LTP_SCHLD_PAYMNT_12,
              LTPFILE.LTP_SCHLD_TERM_13,
              LTPFILE.LTP_SCHLD_PAYMNT_13,
              LTPFILE.LTP_SCHLD_TERM_14,
              LTPFILE.LTP_SCHLD_PAYMNT_14,
              LTPFILE.LTP_SCHLD_TERM_15,
              LTPFILE.LTP_SCHLD_PAYMNT_15,
              LTPFILE.LTP_SCHLD_TERM_16,
              LTPFILE.LTP_SCHLD_PAYMNT_16,
              LTPFILE.LTP_SCHLD_TERM_17,
              LTPFILE.LTP_SCHLD_PAYMNT_17,
              LTPFILE.LTP_SCHLD_TERM_18,
              LTPFILE.LTP_SCHLD_PAYMNT_18,
              LTPFILE.LTP_SCHLD_TERM_19,
              LTPFILE.LTP_SCHLD_PAYMNT_19,
              LTPFILE.LTP_SCHLD_TERM_20,
              LTPFILE.LTP_SCHLD_PAYMNT_20,
              LTPFILE.LTP_SCHLD_TERM_21,
              LTPFILE.LTP_SCHLD_PAYMNT_21,
              LTPFILE.LTP_SCHLD_TERM_22,
              LTPFILE.LTP_SCHLD_PAYMNT_22,
              LTPFILE.LTP_SCHLD_TERM_23,
              LTPFILE.LTP_SCHLD_PAYMNT_23,
              LTPFILE.LTP_SCHLD_TERM_24,
              LTPFILE.LTP_SCHLD_PAYMNT_24,
              LTPFILE.LTP_SCHLD_TERM_25,
              LTPFILE.LTP_SCHLD_PAYMNT_25,
              LTPFILE.LTP_SCHLD_TERM_26,
              LTPFILE.LTP_SCHLD_PAYMNT_26,
              LTPFILE.LTP_SCHLD_TERM_27,
              LTPFILE.LTP_SCHLD_PAYMNT_27,
              LTPFILE.LTP_SCHLD_TERM_28,
              LTPFILE.LTP_SCHLD_PAYMNT_28,
              LTPFILE.LTP_SCHLD_TERM_29,
              LTPFILE.LTP_SCHLD_PAYMNT_29,
              LTPFILE.LTP_SCHLD_TERM_30,
              LTPFILE.LTP_SCHLD_PAYMNT_30,
              LTPFILE.LTP_SCHLD_TERM_31,
              LTPFILE.LTP_SCHLD_PAYMNT_31,
              LTPFILE.LTP_SCHLD_TERM_32,
              LTPFILE.LTP_SCHLD_PAYMNT_32,
              LTPFILE.LTP_SCHLD_TERM_33,
              LTPFILE.LTP_SCHLD_PAYMNT_33,
              LTPFILE.LTP_SCHLD_TERM_34,
              LTPFILE.LTP_SCHLD_PAYMNT_34,
              LTPFILE.LTP_SCHLD_TERM_35,
              LTPFILE.LTP_SCHLD_PAYMNT_35,
              LTPFILE.LTP_SCHLD_TERM_36,
              LTPFILE.LTP_SCHLD_PAYMNT_36,
              LTPFILE.LTP_SCHLD_TERM_37,
              LTPFILE.LTP_SCHLD_PAYMNT_37,
              LTPFILE.LTP_SCHLD_TERM_38,
              LTPFILE.LTP_SCHLD_PAYMNT_38,
              LTPFILE.LTP_SCHLD_TERM_39,
              LTPFILE.LTP_SCHLD_PAYMNT_39,
              LTPFILE.LTP_SCHLD_TERM_40,
              LTPFILE.LTP_SCHLD_PAYMNT_40,
              LTPFILE.LTP_SCHLD_TERM_41,
              LTPFILE.LTP_SCHLD_PAYMNT_41)
            VALUES
             (:QLTP-BRNO,
              :QLTP-ACCTNO,
              :QLTP-SEQNO,
              NULLIF(:QLTP-LTOUCH-DATE,'1900-01-01'),
              :QLTP-SCHLD-TERM-1,
              :QLTP-SCHLD-PAYMNT-1,
              :QLTP-SCHLD-TERM-2,
              :QLTP-SCHLD-PAYMNT-2,
              :QLTP-SCHLD-TERM-3,
              :QLTP-SCHLD-PAYMNT-3,
              :QLTP-SCHLD-TERM-4,
              :QLTP-SCHLD-PAYMNT-4,
              :QLTP-SCHLD-TERM-5,
              :QLTP-SCHLD-PAYMNT-5,
              :QLTP-SCHLD-TERM-6,
              :QLTP-SCHLD-PAYMNT-6,
              :QLTP-SCHLD-TERM-7,
              :QLTP-SCHLD-PAYMNT-7,
              :QLTP-SCHLD-TERM-8,
              :QLTP-SCHLD-PAYMNT-8,
              :QLTP-SCHLD-TERM-9,
              :QLTP-SCHLD-PAYMNT-9,
              :QLTP-SCHLD-TERM-10,
              :QLTP-SCHLD-PAYMNT-10,
              :QLTP-SCHLD-TERM-11,
              :QLTP-SCHLD-PAYMNT-11,
              :QLTP-SCHLD-TERM-12,
              :QLTP-SCHLD-PAYMNT-12,
              :QLTP-SCHLD-TERM-13,
              :QLTP-SCHLD-PAYMNT-13,
              :QLTP-SCHLD-TERM-14,
              :QLTP-SCHLD-PAYMNT-14,
              :QLTP-SCHLD-TERM-15,
              :QLTP-SCHLD-PAYMNT-15,
              :QLTP-SCHLD-TERM-16,
              :QLTP-SCHLD-PAYMNT-16,
              :QLTP-SCHLD-TERM-17,
              :QLTP-SCHLD-PAYMNT-17,
              :QLTP-SCHLD-TERM-18,
              :QLTP-SCHLD-PAYMNT-18,
              :QLTP-SCHLD-TERM-19,
              :QLTP-SCHLD-PAYMNT-19,
              :QLTP-SCHLD-TERM-20,
              :QLTP-SCHLD-PAYMNT-20,
              :QLTP-SCHLD-TERM-21,
              :QLTP-SCHLD-PAYMNT-21,
              :QLTP-SCHLD-TERM-22,
              :QLTP-SCHLD-PAYMNT-22,
              :QLTP-SCHLD-TERM-23,
              :QLTP-SCHLD-PAYMNT-23,
              :QLTP-SCHLD-TERM-24,
              :QLTP-SCHLD-PAYMNT-24,
              :QLTP-SCHLD-TERM-25,
              :QLTP-SCHLD-PAYMNT-25,
              :QLTP-SCHLD-TERM-26,
              :QLTP-SCHLD-PAYMNT-26,
              :QLTP-SCHLD-TERM-27,
              :QLTP-SCHLD-PAYMNT-27,
              :QLTP-SCHLD-TERM-28,
              :QLTP-SCHLD-PAYMNT-28,
              :QLTP-SCHLD-TERM-29,
              :QLTP-SCHLD-PAYMNT-29,
              :QLTP-SCHLD-TERM-30,
              :QLTP-SCHLD-PAYMNT-30,
              :QLTP-SCHLD-TERM-31,
              :QLTP-SCHLD-PAYMNT-31,
              :QLTP-SCHLD-TERM-32,
              :QLTP-SCHLD-PAYMNT-32,
              :QLTP-SCHLD-TERM-33,
              :QLTP-SCHLD-PAYMNT-33,
              :QLTP-SCHLD-TERM-34,
              :QLTP-SCHLD-PAYMNT-34,
              :QLTP-SCHLD-TERM-35,
              :QLTP-SCHLD-PAYMNT-35,
              :QLTP-SCHLD-TERM-36,
              :QLTP-SCHLD-PAYMNT-36,
              :QLTP-SCHLD-TERM-37,
              :QLTP-SCHLD-PAYMNT-37,
              :QLTP-SCHLD-TERM-38,
              :QLTP-SCHLD-PAYMNT-38,
              :QLTP-SCHLD-TERM-39,
              :QLTP-SCHLD-PAYMNT-39,
              :QLTP-SCHLD-TERM-40,
              :QLTP-SCHLD-PAYMNT-40,
              :QLTP-SCHLD-TERM-41,
              :QLTP-SCHLD-PAYMNT-41)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LTP1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LTP1-FILE.

           MOVE LTP-PATH-OWNBR  TO LTP-BRNO.
           ACCEPT LTP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LTP-SELECT-PATH TO E-FILE.
           MOVE LTP1-KEY        TO E-KEYX.
           PERFORM SET-LTP-FIELDS.

           EXEC SQL
            UPDATE DBO.LTPFILE
             SET
              LTPFILE.LTP_BRNO = :QLTP-BRNO,
              LTPFILE.LTP_ACCTNO = :QLTP-ACCTNO,
              LTPFILE.LTP_SEQNO = :QLTP-SEQNO,
              LTPFILE.LTP_LTOUCH_DATE =
                 NULLIF(:QLTP-LTOUCH-DATE,'1900-01-01'),
              LTPFILE.LTP_SCHLD_TERM_1 = :QLTP-SCHLD-TERM-1,
              LTPFILE.LTP_SCHLD_PAYMNT_1 = :QLTP-SCHLD-PAYMNT-1,
              LTPFILE.LTP_SCHLD_TERM_2 = :QLTP-SCHLD-TERM-2,
              LTPFILE.LTP_SCHLD_PAYMNT_2 = :QLTP-SCHLD-PAYMNT-2,
              LTPFILE.LTP_SCHLD_TERM_3 = :QLTP-SCHLD-TERM-3,
              LTPFILE.LTP_SCHLD_PAYMNT_3 = :QLTP-SCHLD-PAYMNT-3,
              LTPFILE.LTP_SCHLD_TERM_4 = :QLTP-SCHLD-TERM-4,
              LTPFILE.LTP_SCHLD_PAYMNT_4 = :QLTP-SCHLD-PAYMNT-4,
              LTPFILE.LTP_SCHLD_TERM_5 = :QLTP-SCHLD-TERM-5,
              LTPFILE.LTP_SCHLD_PAYMNT_5 = :QLTP-SCHLD-PAYMNT-5,
              LTPFILE.LTP_SCHLD_TERM_6 = :QLTP-SCHLD-TERM-6,
              LTPFILE.LTP_SCHLD_PAYMNT_6 = :QLTP-SCHLD-PAYMNT-6,
              LTPFILE.LTP_SCHLD_TERM_7 = :QLTP-SCHLD-TERM-7,
              LTPFILE.LTP_SCHLD_PAYMNT_7 = :QLTP-SCHLD-PAYMNT-7,
              LTPFILE.LTP_SCHLD_TERM_8 = :QLTP-SCHLD-TERM-8,
              LTPFILE.LTP_SCHLD_PAYMNT_8 = :QLTP-SCHLD-PAYMNT-8,
              LTPFILE.LTP_SCHLD_TERM_9 = :QLTP-SCHLD-TERM-9,
              LTPFILE.LTP_SCHLD_PAYMNT_9 = :QLTP-SCHLD-PAYMNT-9,
              LTPFILE.LTP_SCHLD_TERM_10 = :QLTP-SCHLD-TERM-10,
              LTPFILE.LTP_SCHLD_PAYMNT_10 = :QLTP-SCHLD-PAYMNT-10,
              LTPFILE.LTP_SCHLD_TERM_11 = :QLTP-SCHLD-TERM-11,
              LTPFILE.LTP_SCHLD_PAYMNT_11 = :QLTP-SCHLD-PAYMNT-11,
              LTPFILE.LTP_SCHLD_TERM_12 = :QLTP-SCHLD-TERM-12,
              LTPFILE.LTP_SCHLD_PAYMNT_12 = :QLTP-SCHLD-PAYMNT-12,
              LTPFILE.LTP_SCHLD_TERM_13 = :QLTP-SCHLD-TERM-13,
              LTPFILE.LTP_SCHLD_PAYMNT_13 = :QLTP-SCHLD-PAYMNT-13,
              LTPFILE.LTP_SCHLD_TERM_14 = :QLTP-SCHLD-TERM-14,
              LTPFILE.LTP_SCHLD_PAYMNT_14 = :QLTP-SCHLD-PAYMNT-14,
              LTPFILE.LTP_SCHLD_TERM_15 = :QLTP-SCHLD-TERM-15,
              LTPFILE.LTP_SCHLD_PAYMNT_15 = :QLTP-SCHLD-PAYMNT-15,
              LTPFILE.LTP_SCHLD_TERM_16 = :QLTP-SCHLD-TERM-16,
              LTPFILE.LTP_SCHLD_PAYMNT_16 = :QLTP-SCHLD-PAYMNT-16,
              LTPFILE.LTP_SCHLD_TERM_17 = :QLTP-SCHLD-TERM-17,
              LTPFILE.LTP_SCHLD_PAYMNT_17 = :QLTP-SCHLD-PAYMNT-17,
              LTPFILE.LTP_SCHLD_TERM_18 = :QLTP-SCHLD-TERM-18,
              LTPFILE.LTP_SCHLD_PAYMNT_18 = :QLTP-SCHLD-PAYMNT-18,
              LTPFILE.LTP_SCHLD_TERM_19 = :QLTP-SCHLD-TERM-19,
              LTPFILE.LTP_SCHLD_PAYMNT_19 = :QLTP-SCHLD-PAYMNT-19,
              LTPFILE.LTP_SCHLD_TERM_20 = :QLTP-SCHLD-TERM-20,
              LTPFILE.LTP_SCHLD_PAYMNT_20 = :QLTP-SCHLD-PAYMNT-20,
              LTPFILE.LTP_SCHLD_TERM_21 = :QLTP-SCHLD-TERM-21,
              LTPFILE.LTP_SCHLD_PAYMNT_21 = :QLTP-SCHLD-PAYMNT-21,
              LTPFILE.LTP_SCHLD_TERM_22 = :QLTP-SCHLD-TERM-22,
              LTPFILE.LTP_SCHLD_PAYMNT_22 = :QLTP-SCHLD-PAYMNT-22,
              LTPFILE.LTP_SCHLD_TERM_23 = :QLTP-SCHLD-TERM-23,
              LTPFILE.LTP_SCHLD_PAYMNT_23 = :QLTP-SCHLD-PAYMNT-23,
              LTPFILE.LTP_SCHLD_TERM_24 = :QLTP-SCHLD-TERM-24,
              LTPFILE.LTP_SCHLD_PAYMNT_24 = :QLTP-SCHLD-PAYMNT-24,
              LTPFILE.LTP_SCHLD_TERM_25 = :QLTP-SCHLD-TERM-25,
              LTPFILE.LTP_SCHLD_PAYMNT_25 = :QLTP-SCHLD-PAYMNT-25,
              LTPFILE.LTP_SCHLD_TERM_26 = :QLTP-SCHLD-TERM-26,
              LTPFILE.LTP_SCHLD_PAYMNT_26 = :QLTP-SCHLD-PAYMNT-26,
              LTPFILE.LTP_SCHLD_TERM_27 = :QLTP-SCHLD-TERM-27,
              LTPFILE.LTP_SCHLD_PAYMNT_27 = :QLTP-SCHLD-PAYMNT-27,
              LTPFILE.LTP_SCHLD_TERM_28 = :QLTP-SCHLD-TERM-28,
              LTPFILE.LTP_SCHLD_PAYMNT_28 = :QLTP-SCHLD-PAYMNT-28,
              LTPFILE.LTP_SCHLD_TERM_29 = :QLTP-SCHLD-TERM-29,
              LTPFILE.LTP_SCHLD_PAYMNT_29 = :QLTP-SCHLD-PAYMNT-29,
              LTPFILE.LTP_SCHLD_TERM_30 = :QLTP-SCHLD-TERM-30,
              LTPFILE.LTP_SCHLD_PAYMNT_30 = :QLTP-SCHLD-PAYMNT-30,
              LTPFILE.LTP_SCHLD_TERM_31 = :QLTP-SCHLD-TERM-31,
              LTPFILE.LTP_SCHLD_PAYMNT_31 = :QLTP-SCHLD-PAYMNT-31,
              LTPFILE.LTP_SCHLD_TERM_32 = :QLTP-SCHLD-TERM-32,
              LTPFILE.LTP_SCHLD_PAYMNT_32 = :QLTP-SCHLD-PAYMNT-32,
              LTPFILE.LTP_SCHLD_TERM_33 = :QLTP-SCHLD-TERM-33,
              LTPFILE.LTP_SCHLD_PAYMNT_33 = :QLTP-SCHLD-PAYMNT-33,
              LTPFILE.LTP_SCHLD_TERM_34 = :QLTP-SCHLD-TERM-34,
              LTPFILE.LTP_SCHLD_PAYMNT_34 = :QLTP-SCHLD-PAYMNT-34,
              LTPFILE.LTP_SCHLD_TERM_35 = :QLTP-SCHLD-TERM-35,
              LTPFILE.LTP_SCHLD_PAYMNT_35 = :QLTP-SCHLD-PAYMNT-35,
              LTPFILE.LTP_SCHLD_TERM_36 = :QLTP-SCHLD-TERM-36,
              LTPFILE.LTP_SCHLD_PAYMNT_36 = :QLTP-SCHLD-PAYMNT-36,
              LTPFILE.LTP_SCHLD_TERM_37 = :QLTP-SCHLD-TERM-37,
              LTPFILE.LTP_SCHLD_PAYMNT_37 = :QLTP-SCHLD-PAYMNT-37,
              LTPFILE.LTP_SCHLD_TERM_38 = :QLTP-SCHLD-TERM-38,
              LTPFILE.LTP_SCHLD_PAYMNT_38 = :QLTP-SCHLD-PAYMNT-38,
              LTPFILE.LTP_SCHLD_TERM_39 = :QLTP-SCHLD-TERM-39,
              LTPFILE.LTP_SCHLD_PAYMNT_39 = :QLTP-SCHLD-PAYMNT-39,
              LTPFILE.LTP_SCHLD_TERM_40 = :QLTP-SCHLD-TERM-40,
              LTPFILE.LTP_SCHLD_PAYMNT_40 = :QLTP-SCHLD-PAYMNT-40,
              LTPFILE.LTP_SCHLD_TERM_41 = :QLTP-SCHLD-TERM-41,
              LTPFILE.LTP_SCHLD_PAYMNT_41 = :QLTP-SCHLD-PAYMNT-41
             WHERE LTPFILE.LTP_BRNO = :QLTP-BRNO
               AND LTPFILE.LTP_ACCTNO = :QLTP-ACCTNO
               AND LTPFILE.LTP_SEQNO = :QLTP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LTP1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LTP1-FILE.

           MOVE LTP-PATH-OWNBR  TO LTP-BRNO.
           PERFORM DELETE-IT.
           MOVE LTP-SELECT-PATH TO E-FILE.
           MOVE LTP1-KEY        TO E-KEYX.
           PERFORM SET-LTP-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LTPFILE
             WHERE LTPFILE.LTP_BRNO = :QLTP-BRNO
               AND LTPFILE.LTP_ACCTNO = :QLTP-ACCTNO
               AND LTPFILE.LTP_SEQNO = :QLTP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LTP1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LTP1-FILE.

           IF ( LTP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTP1-CURSOR-STAT.

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
              DESTROY IOLTPI-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
