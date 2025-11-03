       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOTRXR.
       AUTHOR.        BAH
       DATE-WRITTEN.  04-14-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOTRXR.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : TRXFILE 
      * TABLE DESC    : TRANSACTION CLASS TRANSFER FILE
      * OUTPUT FILE   : XPC/IOTRXR.PRE
      * USED FOR      : LIBLP/LPWSTRX.CPY  --- QTRX-REC
      *                 LIBLP/LPTRX1R.CPY  --- READ
      *                 LIBLP/LPTRX1RN.CPY --- READ/READ-NEXT/START
      *                 LIBLP/LPTRX2RN.CPY --- READ/READ-NEXT/START
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
           "@(#)./IO/IOTRXR.CBL S34 05/10/21-10:31:08 >".

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
       COPY "LIBLP/LP01TRX.CPY".
       COPY "LIBLP/LPWSTRX.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QTRX1-WHERE-BEG-KEY.
           03  QTRX1-WBEG-BRNO             PIC 9(04).
           03  QTRX1-WBEG-DATE             PIC X(10).
           03  QTRX1-WBEG-CLASS            PIC 9(03).
           03  QTRX1-WBEG-ACCTNO           PIC 9(08).
           03  QTRX1-WBEG-LAWCODE          PIC 9(02).
           03  QTRX1-WBEG-LINE             PIC 9(03).

       01  QTRX1-WHERE-END-KEY.
           03  QTRX1-WEND-BRNO             PIC 9(04).
           03  QTRX1-WEND-DATE             PIC X(10).
           03  QTRX1-WEND-CLASS            PIC 9(03).
           03  QTRX1-WEND-ACCTNO           PIC 9(08).
           03  QTRX1-WEND-LAWCODE          PIC 9(02).
           03  QTRX1-WEND-LINE             PIC 9(03).
 
       01  QTRX2-WHERE-BEG-KEY.
           03  QTRX2-WBEG-DATE             PIC X(10).
           03  QTRX2-WBEG-BRNO             PIC 9(04).
           03  QTRX2-WBEG-CLASS            PIC 9(03).
           03  QTRX2-WBEG-ACCTNO           PIC 9(08).
           03  QTRX2-WBEG-LAWCODE          PIC 9(02).
           03  QTRX2-WBEG-LINE             PIC 9(03).

       01  QTRX2-WHERE-END-KEY.
           03  QTRX2-WEND-DATE             PIC X(10).
           03  QTRX2-WEND-BRNO             PIC 9(04).
           03  QTRX2-WEND-CLASS            PIC 9(03).
           03  QTRX2-WEND-ACCTNO           PIC 9(08).
           03  QTRX2-WEND-LAWCODE          PIC 9(02).
           03  QTRX2-WEND-LINE             PIC 9(03).
 
       01  QTRX-REC.
           03  QTX-BRNO             PIC 9(4).
           03  QTX-DATE             PIC X(10).
           03  QTX-CLASS            PIC 9(3).
           03  QTX-ACCTNO           PIC 9(8).
           03  QTX-LAWCODE          PIC 99.
           03  QTX-LINE             PIC 9(3).
           03  QTX-LTOUCH-DATE      PIC X(10).
           03  QTX-SSNO-1         PIC 9(9).
           03  QTX-MAKERCD-1      PIC X.
           03  QTX-SSNO-2         PIC 9(9).
           03  QTX-MAKERCD-2      PIC X.
           03  QTX-SSNO-3         PIC 9(9).
           03  QTX-MAKERCD-3      PIC X.
           03  QTX-SSNO-4         PIC 9(9).
           03  QTX-MAKERCD-4      PIC X.
           03  QTX-PURCD            PIC XXX.
           03  QTX-SRCD             PIC XX.
           03  QTX-COLLCD           PIC XX.
           03  QTX-SERVACCT         PIC X.
           03  QTX-LOANTYPE         PIC X.
           03  QTX-SUBCLASS         PIC 99.
           03  QTX-LOANDATE         PIC X(10).
           03  QTX-PROCEEDS         PIC S9(7)V99.
           03  QTX-LNAMT            PIC S9(7)V99.
           03  QTX-FINCHG           PIC S9(7)V99.
           03  QTX-INTCHG           PIC 9(7)V99.
           03  QTX-SERCHG           PIC 9(5)V99.
           03  QTX-EXTCHG           PIC 9(5)V99.
           03  QTX-MAINTFEE         PIC 9(5)V99.
           03  QTX-ORGTERM          PIC 9(3).
           03  QTX-INSURED          PIC X.
           03  QTX-INSANTICERN-1    PIC S9(7)V99.
           03  QTX-INSANTICERN-2    PIC S9(7)V99.
           03  QTX-INSANTICERN-3    PIC S9(7)V99.
           03  QTX-INSANTICERN-4    PIC S9(7)V99.
           03  QTX-INSANTICERN-5    PIC S9(7)V99.
           03  QTX-INSANTICERN-6    PIC S9(7)V99.
           03  QTX-INSANTICERN-7    PIC S9(7)V99.
           03  QTX-INSANTICERN-8    PIC S9(7)V99.
           03  QTX-INSANTICERN-9    PIC S9(7)V99.
           03  QTX-INSANTICERN-10   PIC S9(7)V99.
           03  QTX-INSEARNINGS-1    PIC S9(7)V99.
           03  QTX-INSEARNINGS-2    PIC S9(7)V99.
           03  QTX-INSEARNINGS-3    PIC S9(7)V99.
           03  QTX-INSEARNINGS-4    PIC S9(7)V99.
           03  QTX-INSEARNINGS-5    PIC S9(7)V99.
           03  QTX-INSEARNINGS-6    PIC S9(7)V99.
           03  QTX-INSEARNINGS-7    PIC S9(7)V99.
           03  QTX-INSEARNINGS-8    PIC S9(7)V99.
           03  QTX-INSEARNINGS-9    PIC S9(7)V99.
           03  QTX-INSEARNINGS-10   PIC S9(7)V99.
           03  QTX-DLNO             PIC 9(4)V99.
           03  QTX-DLRECOURSE       PIC X.
           03  QTX-DLPARTRCAMT      PIC 9(7)V99.
           03  QTX-PURBAL           PIC S9(7)V99.
           03  QTX-DLRVPRIN-1       PIC S9(7)V99.
           03  QTX-DLRVPRIN-2       PIC S9(7)V99.
           03  QTX-DLRVPRIN-3       PIC S9(7)V99.
           03  QTX-DLRVCHGS-1       PIC S9(7)V99.
           03  QTX-DLRVCHGS-2       PIC S9(7)V99.
           03  QTX-DLRVCHGS-3       PIC S9(7)V99.
           03  QTX-DLPARVHELD       PIC S9(7)V99.
           03  QTX-DLPARVPAID       PIC S9(7)V99.
           03  QTX-DLDISC           PIC S9(7)V99.
           03  QTX-ANTICTERM        PIC 9(3).
           03  QTX-ANTICERN-1       PIC S9(7)V99.
           03  QTX-ANTICERN-2       PIC S9(7)V99.
           03  QTX-ANTICERN-3       PIC S9(7)V99.
           03  QTX-ANTICERN-4       PIC S9(7)V99.
           03  QTX-ANTICERN-5       PIC S9(7)V99.
           03  QTX-ANTICERN-6       PIC S9(7)V99.
           03  QTX-ANTICERN-7       PIC S9(7)V99.
           03  QTX-ANTICERN-8       PIC S9(7)V99.
           03  QTX-ANTICERN-9       PIC S9(7)V99.
           03  QTX-ANTICERN-10      PIC S9(7)V99.
           03  QTX-EARNINGS-1       PIC S9(7)V99.
           03  QTX-EARNINGS-2       PIC S9(7)V99.
           03  QTX-EARNINGS-3       PIC S9(7)V99.
           03  QTX-EARNINGS-4       PIC S9(7)V99.
           03  QTX-EARNINGS-5       PIC S9(7)V99.
           03  QTX-EARNINGS-6       PIC S9(7)V99.
           03  QTX-EARNINGS-7       PIC S9(7)V99.
           03  QTX-EARNINGS-8       PIC S9(7)V99.
           03  QTX-EARNINGS-9       PIC S9(7)V99.
           03  QTX-EARNINGS-10      PIC S9(7)V99.
           03  QTX-PRLNNO           PIC 9(8).
           03  QTX-PRBAL            PIC S9(7)V99.
           03  QTX-TFRBAL           PIC S9(7)V99.
           03  QTX-SPRCLASS         PIC 999.
           03  QTX-USERID           PIC X(10).
           03  QTX-POSTDATE         PIC X(10).
           03  QTX-POSTTIME         PIC 9(6).
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
           03  BEG-CLASS               PIC 9(03).
           03  END-CLASS               PIC 9(03).
           03  BEG-ACCTNO              PIC 9(08).
           03  END-ACCTNO              PIC 9(08).
           03  BEG-LAWCODE             PIC 9(02).
           03  END-LAWCODE             PIC 9(02).
           03  BEG-LINE                PIC 9(03).
           03  END-LINE                PIC 9(03).

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
           03  FILLER PIC X(30) VALUE "BRNO     DATE     CLASS   ACCT".
           03  FILLER PIC X(29) VALUE "NO   LAW  LINE    SSNO       ".
           03  FILLER PIC X(30) VALUE "LOANDATE".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-TX-BRNO                   PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-TX-DATE                   PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-TX-CLASS                  PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-TX-ACCTNO                 PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-TX-LAWCODE                PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-TX-LINE                   PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-TX-SSNO1                  PIC 9(09).
           03  FILLER                         PIC X(03).
           03  D01-TX-LOANDATE               PIC 9999/99/99.

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
       COPY "LIBIO/IOTRXR_DEF.CPY".
       COPY "LIBIO/IOTRXR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOTRXR_SCN.CPY".

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
           MOVE "IOTRXR"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOTRXR-QUEUE-POS  TO Q-POS.
           MOVE IOTRXR-COPIES-POS TO C-POS.
           MOVE IOTRXR-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-LINE.

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

           GO TO ENTER-BEG-CLASS.

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
       ENTER-BEG-CLASS.

           MOVE "ENNN030BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-END-CLASS.

           MOVE "ENNN030END-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-CLASS.

           IF ( BEG-CLASS > END-CLASS )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           GO TO ENTER-BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-ALL-CLASS.

           MOVE "SNNN030BEG-CLASS." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-CLASS.
           PERFORM SCREENIO.

           MOVE "SNNN030END-CLASS." TO VDU.
           MOVE 999                 TO VDUNUM0  END-CLASS.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-ACCTNO.

           MOVE "ENNN080BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CLASS.
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

           GO TO ENTER-BEG-LAWCODE.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN080BEG-ACCTNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-ACCTNO.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO." TO VDU.
           MOVE 99999999            TO VDUNUM0  END-ACCTNO.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-LAWCODE.

           MOVE "ENNN020BEG-LAWCODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-LAWCODE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-LAWCODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-LAWCODE.

      *-----------------------------------------------------------------
       ENTER-END-LAWCODE.

           MOVE "ENNN020END-LAWCODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LAWCODE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-LAWCODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-LAWCODE.

           IF ( BEG-LAWCODE > END-LAWCODE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LAWCODE.

           GO TO ENTER-BEG-LINE.

      *-----------------------------------------------------------------
       ENTER-ALL-LAWCODE.

           MOVE "SNNN020BEG-LAWCODE." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-LAWCODE.
           PERFORM SCREENIO.

           MOVE "SNNN020END-LAWCODE." TO VDU.
           MOVE 99                    TO VDUNUM0  END-LAWCODE.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-LINE.

           MOVE "ENNN030BEG-LINE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LAWCODE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-LINE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-LINE.

      *-----------------------------------------------------------------
       ENTER-END-LINE.

           MOVE "ENNN030END-LINE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LINE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-LINE.

           IF ( BEG-LINE > END-LINE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LINE.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-LINE.

           MOVE "SNNN030BEG-LINE." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-LINE.
           PERFORM SCREENIO.

           MOVE "SNNN030END-LINE." TO VDU.
           MOVE 999                 TO VDUNUM0  END-LINE.
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

           MOVE "SNNN030BEG-CLASS."       TO VDU.
           MOVE BEG-CLASS                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-CLASS."       TO VDU.
           MOVE END-CLASS                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080BEG-ACCTNO."      TO VDU.
           MOVE BEG-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO."      TO VDU.
           MOVE END-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020BEG-LAWCODE."       TO VDU.
           MOVE BEG-LAWCODE                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-LAWCODE."       TO VDU.
           MOVE END-LAWCODE                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030BEG-LINE."       TO VDU.
           MOVE BEG-LINE                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-LINE."       TO VDU.
           MOVE END-LINE                 TO VDUNUM0.
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

           MOVE "B"                        TO TRX-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QTRX1-WBEG-BRNO
                              QTRX2-WBEG-BRNO.
           MOVE BR-NO     TO QTRX1-WEND-BRNO
                              QTRX2-WEND-BRNO.

           MOVE BEG-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRX1-WBEG-DATE
                                        QTRX2-WBEG-DATE.
           MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRX1-WEND-DATE
                                        QTRX2-WEND-DATE.

           MOVE BEG-CLASS  TO QTRX1-WBEG-CLASS
                              QTRX2-WBEG-CLASS.
           MOVE END-CLASS  TO QTRX1-WEND-CLASS
                              QTRX2-WEND-CLASS.

           MOVE BEG-ACCTNO TO QTRX1-WBEG-ACCTNO
                              QTRX2-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QTRX1-WEND-ACCTNO
                              QTRX2-WEND-ACCTNO.
           MOVE BEG-LAWCODE TO QTRX1-WBEG-LAWCODE
                               QTRX2-WBEG-LAWCODE.
           MOVE END-LAWCODE TO QTRX1-WEND-LAWCODE
                               QTRX2-WEND-LAWCODE.
           MOVE BEG-LINE TO QTRX1-WBEG-LINE
                            QTRX2-WBEG-LINE.
           MOVE END-LINE TO QTRX1-WEND-LINE
                            QTRX2-WEND-LINE.


      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF PROCESS-KEY = 1
              PERFORM OPEN-TRX1-FILE
              PERFORM START-TRX1-FILE
           ELSE
              PERFORM OPEN-TRX2-FILE
              PERFORM START-TRX2-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           IF PROCESS-KEY = 1
              PERFORM READ-TRX1-FILE-NEXT
           ELSE
              PERFORM READ-TRX2-FILE-NEXT.
           IF ( IO-BAD ) 
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( TX-BRNO < BEG-BRANCH )
                  GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( TX-BRNO > END-BRANCH )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF TX-DATE < BEG-DATE OR > END-DATE
                    GO TO PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 2 )
              IF ( TX-DATE < BEG-DATE )
                  GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( TX-DATE > END-DATE )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF TX-BRNO < BEG-BRANCH OR > END-BRANCH
                    GO TO PROCESS-BRANCH-NEXT.

           IF TX-CLASS < BEG-CLASS OR > END-CLASS
              GO TO PROCESS-BRANCH-NEXT.
           IF TX-ACCTNO < BEG-ACCTNO OR > END-ACCTNO
              GO TO PROCESS-BRANCH-NEXT.
           IF TX-LAWCODE < BEG-LAWCODE OR > END-LAWCODE
              GO TO PROCESS-BRANCH-NEXT.
           IF TX-LINE < BEG-LINE OR > END-LINE
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE TX-BRNO                    TO D01-TX-BRNO.
           MOVE TX-DATE                    TO D01-TX-DATE.
           MOVE TX-CLASS                   TO D01-TX-CLASS.
           MOVE TX-ACCTNO                  TO D01-TX-ACCTNO.
           MOVE TX-LAWCODE                 TO D01-TX-LAWCODE.
           MOVE TX-LINE                    TO D01-TX-LINE.
           MOVE TX-SSNO(1)                 TO D01-TX-SSNO1.
           MOVE TX-LOANDATE                TO D01-TX-LOANDATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           IF PROCESS-KEY = 1
              PERFORM CLOSE-TRX1-FILE
           ELSE
              PERFORM CLOSE-TRX2-FILE.

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
              MOVE TRX1-KEY TO PACE-DETAIL-KEY
           ELSE
              STRING TX-DATE, TX-BRNO, TX-CLASS, TX-ACCTNO, TX-LAWCODE,
                     TX-LINE DELIMITED BY SIZE INTO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING CLASS               : "    TO R01-PROMPT.
           MOVE BEG-CLASS                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CLASS               : "    TO R01-PROMPT.
           MOVE END-CLASS                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
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
           MOVE "BEGINNING LAWCODE              : "    TO R01-PROMPT.
           MOVE BEG-LAWCODE                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    LAWCODE              : "    TO R01-PROMPT.
           MOVE END-LAWCODE                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE "BEGINNING LINE                : "    TO R01-PROMPT.
           MOVE BEG-LINE                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    LINE                : "    TO R01-PROMPT.
           MOVE END-LINE                              TO R01-NUMERIC.
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
           MOVE "SQL TABLE IO UTILITY - TRXFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOTRXR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOTRXR-SYSDATE.
           DISPLAY IOTRXR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOTRXR-FORM-FIELDS TO WR-BUF.
           MOVE IOTRXR-FORM-LIST TO WR-LIST.
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

           MOVE IOTRXR-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOTRXR-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOTRXR_EVA.CPY".
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
      *    LOAD TRX-REC FROM QTRX-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-TRX-FIELDS.
           MOVE QTX-BRNO TO TX-BRNO.

           IF ( QTX-DATE = "1900-01-01" )
              MOVE ZEROES TO TX-DATE
           ELSE
              MOVE QTX-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TX-DATE.

           MOVE QTX-CLASS TO TX-CLASS.
           MOVE QTX-ACCTNO TO TX-ACCTNO.
           MOVE QTX-LAWCODE TO TX-LAWCODE.
           MOVE QTX-LINE TO TX-LINE.

           IF ( QTX-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO TX-LTOUCH-DATE
           ELSE
              MOVE QTX-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TX-LTOUCH-DATE.

           MOVE QTX-SSNO-1 TO TX-SSNO(1).
           MOVE QTX-MAKERCD-1 TO TX-MAKERCD(1).
           MOVE QTX-SSNO-2 TO TX-SSNO(2).
           MOVE QTX-MAKERCD-2 TO TX-MAKERCD(2).
           MOVE QTX-SSNO-3 TO TX-SSNO(3).
           MOVE QTX-MAKERCD-3 TO TX-MAKERCD(3).
           MOVE QTX-SSNO-4 TO TX-SSNO(4).
           MOVE QTX-MAKERCD-4 TO TX-MAKERCD(4).
           MOVE QTX-PURCD TO TX-PURCD.
           MOVE QTX-SRCD TO TX-SRCD.
           MOVE QTX-COLLCD TO TX-COLLCD.
           MOVE QTX-SERVACCT TO TX-SERVACCT.
           MOVE QTX-LOANTYPE TO TX-LOANTYPE.
           MOVE QTX-SUBCLASS TO TX-SUBCLASS.

           IF ( QTX-LOANDATE = "1900-01-01" )
              MOVE ZEROES TO TX-LOANDATE
           ELSE
              MOVE QTX-LOANDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TX-LOANDATE.

           MOVE QTX-PROCEEDS TO TX-PROCEEDS.
           MOVE QTX-LNAMT TO TX-LNAMT.
           MOVE QTX-FINCHG TO TX-FINCHG.
           MOVE QTX-INTCHG TO TX-INTCHG.
           MOVE QTX-SERCHG TO TX-SERCHG.
           MOVE QTX-EXTCHG TO TX-EXTCHG.
           MOVE QTX-MAINTFEE TO TX-MAINTFEE.
           MOVE QTX-ORGTERM TO TX-ORGTERM.
           MOVE QTX-INSURED TO TX-INSURED.
           MOVE QTX-INSANTICERN-1 TO TX-INSANTICERN(1).
           MOVE QTX-INSANTICERN-2 TO TX-INSANTICERN(2).
           MOVE QTX-INSANTICERN-3 TO TX-INSANTICERN(3).
           MOVE QTX-INSANTICERN-4 TO TX-INSANTICERN(4).
           MOVE QTX-INSANTICERN-5 TO TX-INSANTICERN(5).
           MOVE QTX-INSANTICERN-6 TO TX-INSANTICERN(6).
           MOVE QTX-INSANTICERN-7 TO TX-INSANTICERN(7).
           MOVE QTX-INSANTICERN-8 TO TX-INSANTICERN(8).
           MOVE QTX-INSANTICERN-9 TO TX-INSANTICERN(9).
           MOVE QTX-INSANTICERN-10 TO TX-INSANTICERN(10).
           MOVE QTX-INSEARNINGS-1 TO TX-INSEARNINGS(1).
           MOVE QTX-INSEARNINGS-2 TO TX-INSEARNINGS(2).
           MOVE QTX-INSEARNINGS-3 TO TX-INSEARNINGS(3).
           MOVE QTX-INSEARNINGS-4 TO TX-INSEARNINGS(4).
           MOVE QTX-INSEARNINGS-5 TO TX-INSEARNINGS(5).
           MOVE QTX-INSEARNINGS-6 TO TX-INSEARNINGS(6).
           MOVE QTX-INSEARNINGS-7 TO TX-INSEARNINGS(7).
           MOVE QTX-INSEARNINGS-8 TO TX-INSEARNINGS(8).
           MOVE QTX-INSEARNINGS-9 TO TX-INSEARNINGS(9).
           MOVE QTX-INSEARNINGS-10 TO TX-INSEARNINGS(10).
           MOVE QTX-DLNO TO TX-DLNO.
           MOVE QTX-DLRECOURSE TO TX-DLRECOURSE.
           MOVE QTX-DLPARTRCAMT TO TX-DLPARTRCAMT.
           MOVE QTX-PURBAL TO TX-PURBAL.
           MOVE QTX-DLRVPRIN-1 TO TX-DLRVPRIN(1).
           MOVE QTX-DLRVPRIN-2 TO TX-DLRVPRIN(2).
           MOVE QTX-DLRVPRIN-3 TO TX-DLRVPRIN(3).
           MOVE QTX-DLRVCHGS-1 TO TX-DLRVCHGS(1).
           MOVE QTX-DLRVCHGS-2 TO TX-DLRVCHGS(2).
           MOVE QTX-DLRVCHGS-3 TO TX-DLRVCHGS(3).
           MOVE QTX-DLPARVHELD TO TX-DLPARVHELD.
           MOVE QTX-DLPARVPAID TO TX-DLPARVPAID.
           MOVE QTX-DLDISC TO TX-DLDISC.
           MOVE QTX-ANTICTERM TO TX-ANTICTERM.
           MOVE QTX-ANTICERN-1 TO TX-ANTICERN(1).
           MOVE QTX-ANTICERN-2 TO TX-ANTICERN(2).
           MOVE QTX-ANTICERN-3 TO TX-ANTICERN(3).
           MOVE QTX-ANTICERN-4 TO TX-ANTICERN(4).
           MOVE QTX-ANTICERN-5 TO TX-ANTICERN(5).
           MOVE QTX-ANTICERN-6 TO TX-ANTICERN(6).
           MOVE QTX-ANTICERN-7 TO TX-ANTICERN(7).
           MOVE QTX-ANTICERN-8 TO TX-ANTICERN(8).
           MOVE QTX-ANTICERN-9 TO TX-ANTICERN(9).
           MOVE QTX-ANTICERN-10 TO TX-ANTICERN(10).
           MOVE QTX-EARNINGS-1 TO TX-EARNINGS(1).
           MOVE QTX-EARNINGS-2 TO TX-EARNINGS(2).
           MOVE QTX-EARNINGS-3 TO TX-EARNINGS(3).
           MOVE QTX-EARNINGS-4 TO TX-EARNINGS(4).
           MOVE QTX-EARNINGS-5 TO TX-EARNINGS(5).
           MOVE QTX-EARNINGS-6 TO TX-EARNINGS(6).
           MOVE QTX-EARNINGS-7 TO TX-EARNINGS(7).
           MOVE QTX-EARNINGS-8 TO TX-EARNINGS(8).
           MOVE QTX-EARNINGS-9 TO TX-EARNINGS(9).
           MOVE QTX-EARNINGS-10 TO TX-EARNINGS(10).
           MOVE QTX-PRLNNO TO TX-PRLNNO.
           MOVE QTX-PRBAL TO TX-PRBAL.
           MOVE QTX-TFRBAL TO TX-TFRBAL.
           MOVE QTX-SPRCLASS TO TX-SPRCLASS.
           MOVE QTX-USERID TO TX-USERID.

           IF ( QTX-POSTDATE = "1900-01-01" )
              MOVE ZEROES TO TX-POSTDATE
           ELSE
              MOVE QTX-POSTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TX-POSTDATE.

           MOVE QTX-POSTTIME TO TX-POSTTIME.
 
      *-----------------------------------------------------------------
      *    LOAD QTRX-REC FROM TRX-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-TRX-FIELDS.
           MOVE TX-BRNO TO QTX-BRNO.

           IF ( TX-DATE = ZEROES )
              MOVE "1900-01-01" TO QTX-DATE
           ELSE
              MOVE TX-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTX-DATE.

           MOVE TX-CLASS TO QTX-CLASS.
           MOVE TX-ACCTNO TO QTX-ACCTNO.
           MOVE TX-LAWCODE TO QTX-LAWCODE.
           MOVE TX-LINE TO QTX-LINE.

           IF ( TX-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTX-LTOUCH-DATE
           ELSE
              MOVE TX-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTX-LTOUCH-DATE.

           MOVE TX-SSNO(1) TO QTX-SSNO-1.
           MOVE TX-MAKERCD(1) TO QTX-MAKERCD-1.
           MOVE TX-SSNO(2) TO QTX-SSNO-2.
           MOVE TX-MAKERCD(2) TO QTX-MAKERCD-2.
           MOVE TX-SSNO(3) TO QTX-SSNO-3.
           MOVE TX-MAKERCD(3) TO QTX-MAKERCD-3.
           MOVE TX-SSNO(4) TO QTX-SSNO-4.
           MOVE TX-MAKERCD(4) TO QTX-MAKERCD-4.
           MOVE TX-PURCD TO QTX-PURCD.
           MOVE TX-SRCD TO QTX-SRCD.
           MOVE TX-COLLCD TO QTX-COLLCD.
           MOVE TX-SERVACCT TO QTX-SERVACCT.
           MOVE TX-LOANTYPE TO QTX-LOANTYPE.
           MOVE TX-SUBCLASS TO QTX-SUBCLASS.

           IF ( TX-LOANDATE = ZEROES )
              MOVE "1900-01-01" TO QTX-LOANDATE
           ELSE
              MOVE TX-LOANDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTX-LOANDATE.

           MOVE TX-PROCEEDS TO QTX-PROCEEDS.
           MOVE TX-LNAMT TO QTX-LNAMT.
           MOVE TX-FINCHG TO QTX-FINCHG.
           MOVE TX-INTCHG TO QTX-INTCHG.
           MOVE TX-SERCHG TO QTX-SERCHG.
           MOVE TX-EXTCHG TO QTX-EXTCHG.
           MOVE TX-MAINTFEE TO QTX-MAINTFEE.
           MOVE TX-ORGTERM TO QTX-ORGTERM.
           MOVE TX-INSURED TO QTX-INSURED.
           MOVE TX-INSANTICERN(1) TO QTX-INSANTICERN-1.
           MOVE TX-INSANTICERN(2) TO QTX-INSANTICERN-2.
           MOVE TX-INSANTICERN(3) TO QTX-INSANTICERN-3.
           MOVE TX-INSANTICERN(4) TO QTX-INSANTICERN-4.
           MOVE TX-INSANTICERN(5) TO QTX-INSANTICERN-5.
           MOVE TX-INSANTICERN(6) TO QTX-INSANTICERN-6.
           MOVE TX-INSANTICERN(7) TO QTX-INSANTICERN-7.
           MOVE TX-INSANTICERN(8) TO QTX-INSANTICERN-8.
           MOVE TX-INSANTICERN(9) TO QTX-INSANTICERN-9.
           MOVE TX-INSANTICERN(10) TO QTX-INSANTICERN-10.
           MOVE TX-INSEARNINGS(1) TO QTX-INSEARNINGS-1.
           MOVE TX-INSEARNINGS(2) TO QTX-INSEARNINGS-2.
           MOVE TX-INSEARNINGS(3) TO QTX-INSEARNINGS-3.
           MOVE TX-INSEARNINGS(4) TO QTX-INSEARNINGS-4.
           MOVE TX-INSEARNINGS(5) TO QTX-INSEARNINGS-5.
           MOVE TX-INSEARNINGS(6) TO QTX-INSEARNINGS-6.
           MOVE TX-INSEARNINGS(7) TO QTX-INSEARNINGS-7.
           MOVE TX-INSEARNINGS(8) TO QTX-INSEARNINGS-8.
           MOVE TX-INSEARNINGS(9) TO QTX-INSEARNINGS-9.
           MOVE TX-INSEARNINGS(10) TO QTX-INSEARNINGS-10.
           MOVE TX-DLNO TO QTX-DLNO.
           MOVE TX-DLRECOURSE TO QTX-DLRECOURSE.
           MOVE TX-DLPARTRCAMT TO QTX-DLPARTRCAMT.
           MOVE TX-PURBAL TO QTX-PURBAL.
           MOVE TX-DLRVPRIN(1) TO QTX-DLRVPRIN-1.
           MOVE TX-DLRVPRIN(2) TO QTX-DLRVPRIN-2.
           MOVE TX-DLRVPRIN(3) TO QTX-DLRVPRIN-3.
           MOVE TX-DLRVCHGS(1) TO QTX-DLRVCHGS-1.
           MOVE TX-DLRVCHGS(2) TO QTX-DLRVCHGS-2.
           MOVE TX-DLRVCHGS(3) TO QTX-DLRVCHGS-3.
           MOVE TX-DLPARVHELD TO QTX-DLPARVHELD.
           MOVE TX-DLPARVPAID TO QTX-DLPARVPAID.
           MOVE TX-DLDISC TO QTX-DLDISC.
           MOVE TX-ANTICTERM TO QTX-ANTICTERM.
           MOVE TX-ANTICERN(1) TO QTX-ANTICERN-1.
           MOVE TX-ANTICERN(2) TO QTX-ANTICERN-2.
           MOVE TX-ANTICERN(3) TO QTX-ANTICERN-3.
           MOVE TX-ANTICERN(4) TO QTX-ANTICERN-4.
           MOVE TX-ANTICERN(5) TO QTX-ANTICERN-5.
           MOVE TX-ANTICERN(6) TO QTX-ANTICERN-6.
           MOVE TX-ANTICERN(7) TO QTX-ANTICERN-7.
           MOVE TX-ANTICERN(8) TO QTX-ANTICERN-8.
           MOVE TX-ANTICERN(9) TO QTX-ANTICERN-9.
           MOVE TX-ANTICERN(10) TO QTX-ANTICERN-10.
           MOVE TX-EARNINGS(1) TO QTX-EARNINGS-1.
           MOVE TX-EARNINGS(2) TO QTX-EARNINGS-2.
           MOVE TX-EARNINGS(3) TO QTX-EARNINGS-3.
           MOVE TX-EARNINGS(4) TO QTX-EARNINGS-4.
           MOVE TX-EARNINGS(5) TO QTX-EARNINGS-5.
           MOVE TX-EARNINGS(6) TO QTX-EARNINGS-6.
           MOVE TX-EARNINGS(7) TO QTX-EARNINGS-7.
           MOVE TX-EARNINGS(8) TO QTX-EARNINGS-8.
           MOVE TX-EARNINGS(9) TO QTX-EARNINGS-9.
           MOVE TX-EARNINGS(10) TO QTX-EARNINGS-10.
           MOVE TX-PRLNNO TO QTX-PRLNNO.
           MOVE TX-PRBAL TO QTX-PRBAL.
           MOVE TX-TFRBAL TO QTX-TFRBAL.
           MOVE TX-SPRCLASS TO QTX-SPRCLASS.
           MOVE TX-USERID TO QTX-USERID.

           IF ( TX-POSTDATE = ZEROES )
              MOVE "1900-01-01" TO QTX-POSTDATE
           ELSE
              MOVE TX-POSTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTX-POSTDATE.

           MOVE TX-POSTTIME TO QTX-POSTTIME.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-TRX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRX-ALL-DATA
              MOVE TRX-ALL-PATH             TO TRX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRX-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRX-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRX-ALL-DATA
              MOVE TRX-ALL-PATH             TO TRX-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRXFILE NOT = "S" )
              MOVE TRX-PATH-BEG             TO TRX-SELECT-PATH-BEG
              MOVE TRX-PATH-FILE            TO TRX-SELECT-PATH-FILE
           ELSE
              MOVE TRX-PATH-SQL             TO TRX-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRX1-FILE.
           PERFORM LOAD-TRX1-FILE.
           PERFORM OPEN-IT.
           MOVE TRX-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRX1-FILE.
           PERFORM START-IT.
           MOVE TRX-PATH-OWNBR  TO TX-BRNO.
           MOVE TRX-SELECT-PATH TO E-FILE.
           MOVE TRX1-KEY        TO E-KEYX.

           IF ( TRX1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRX1-FILE.

           IF ( QTRX1-WEND-LINE = ZEROES    ) OR
              ( QTRX1-WEND-LINE NOT NUMERIC )
              MOVE TRX-PATH-OWNBR        TO QTRX1-WBEG-BRNO
              MOVE TRX-PATH-OWNBR        TO QTRX1-WEND-BRNO
              MOVE TX-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRX1-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRX1-WEND-DATE
              MOVE TX-CLASS             TO QTRX1-WBEG-CLASS
              MOVE TX-ACCTNO            TO QTRX1-WBEG-ACCTNO
              MOVE TX-LAWCODE           TO QTRX1-WBEG-LAWCODE
              MOVE TX-LINE              TO QTRX1-WBEG-LINE
              MOVE ALL "9"              TO QTRX1-WEND-CLASS
                                           QTRX1-WEND-ACCTNO
                                           QTRX1-WEND-LAWCODE
                                           QTRX1-WEND-LINE.


           EXEC SQL
            DECLARE TRX1_CURSOR CURSOR FOR
            SELECT TRXFILE.TX_BRNO,
                   ISNULL(CAST(TRXFILE.TX_DATE AS VARCHAR(10)),'0'),
                   TRXFILE.TX_CLASS,
                   TRXFILE.TX_ACCTNO,
                   TRXFILE.TX_LAWCODE,
                   TRXFILE.TX_LINE,
                   ISNULL(CAST(TRXFILE.TX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRXFILE.TX_SSNO_1,
                   ISNULL(TRXFILE.TX_MAKERCD_1,''),
                   TRXFILE.TX_SSNO_2,
                   ISNULL(TRXFILE.TX_MAKERCD_2,''),
                   TRXFILE.TX_SSNO_3,
                   ISNULL(TRXFILE.TX_MAKERCD_3,''),
                   TRXFILE.TX_SSNO_4,
                   ISNULL(TRXFILE.TX_MAKERCD_4,''),
                   ISNULL(TRXFILE.TX_PURCD,''),
                   ISNULL(TRXFILE.TX_SRCD,''),
                   ISNULL(TRXFILE.TX_COLLCD,''),
                   ISNULL(TRXFILE.TX_SERVACCT,''),
                   ISNULL(TRXFILE.TX_LOANTYPE,''),
                   TRXFILE.TX_SUBCLASS,
                   ISNULL(CAST(TRXFILE.TX_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   TRXFILE.TX_PROCEEDS,
                   TRXFILE.TX_LNAMT,
                   TRXFILE.TX_FINCHG,
                   TRXFILE.TX_INTCHG,
                   TRXFILE.TX_SERCHG,
                   TRXFILE.TX_EXTCHG,
                   TRXFILE.TX_MAINTFEE,
                   TRXFILE.TX_ORGTERM,
                   ISNULL(TRXFILE.TX_INSURED,''),
                   TRXFILE.TX_INSANTICERN_1,
                   TRXFILE.TX_INSANTICERN_2,
                   TRXFILE.TX_INSANTICERN_3,
                   TRXFILE.TX_INSANTICERN_4,
                   TRXFILE.TX_INSANTICERN_5,
                   TRXFILE.TX_INSANTICERN_6,
                   TRXFILE.TX_INSANTICERN_7,
                   TRXFILE.TX_INSANTICERN_8,
                   TRXFILE.TX_INSANTICERN_9,
                   TRXFILE.TX_INSANTICERN_10,
                   TRXFILE.TX_INSEARNINGS_1,
                   TRXFILE.TX_INSEARNINGS_2,
                   TRXFILE.TX_INSEARNINGS_3,
                   TRXFILE.TX_INSEARNINGS_4,
                   TRXFILE.TX_INSEARNINGS_5,
                   TRXFILE.TX_INSEARNINGS_6,
                   TRXFILE.TX_INSEARNINGS_7,
                   TRXFILE.TX_INSEARNINGS_8,
                   TRXFILE.TX_INSEARNINGS_9,
                   TRXFILE.TX_INSEARNINGS_10,
                   TRXFILE.TX_DLNO,
                   ISNULL(TRXFILE.TX_DLRECOURSE,''),
                   TRXFILE.TX_DLPARTRCAMT,
                   TRXFILE.TX_PURBAL,
                   TRXFILE.TX_DLRVPRIN_1,
                   TRXFILE.TX_DLRVPRIN_2,
                   TRXFILE.TX_DLRVPRIN_3,
                   TRXFILE.TX_DLRVCHGS_1,
                   TRXFILE.TX_DLRVCHGS_2,
                   TRXFILE.TX_DLRVCHGS_3,
                   TRXFILE.TX_DLPARVHELD,
                   TRXFILE.TX_DLPARVPAID,
                   TRXFILE.TX_DLDISC,
                   TRXFILE.TX_ANTICTERM,
                   TRXFILE.TX_ANTICERN_1,
                   TRXFILE.TX_ANTICERN_2,
                   TRXFILE.TX_ANTICERN_3,
                   TRXFILE.TX_ANTICERN_4,
                   TRXFILE.TX_ANTICERN_5,
                   TRXFILE.TX_ANTICERN_6,
                   TRXFILE.TX_ANTICERN_7,
                   TRXFILE.TX_ANTICERN_8,
                   TRXFILE.TX_ANTICERN_9,
                   TRXFILE.TX_ANTICERN_10,
                   TRXFILE.TX_EARNINGS_1,
                   TRXFILE.TX_EARNINGS_2,
                   TRXFILE.TX_EARNINGS_3,
                   TRXFILE.TX_EARNINGS_4,
                   TRXFILE.TX_EARNINGS_5,
                   TRXFILE.TX_EARNINGS_6,
                   TRXFILE.TX_EARNINGS_7,
                   TRXFILE.TX_EARNINGS_8,
                   TRXFILE.TX_EARNINGS_9,
                   TRXFILE.TX_EARNINGS_10,
                   TRXFILE.TX_PRLNNO,
                   TRXFILE.TX_PRBAL,
                   TRXFILE.TX_TFRBAL,
                   TRXFILE.TX_SPRCLASS,
                   ISNULL(TRXFILE.TX_USERID,''),
                   ISNULL(CAST(TRXFILE.TX_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRXFILE.TX_POSTTIME
            FROM DBO.TRXFILE
            WHERE TRXFILE.TX_BRNO >= :QTRX1-WBEG-BRNO
              AND TRXFILE.TX_BRNO <= :QTRX1-WEND-BRNO
              AND CAST(TRXFILE.TX_DATE AS VARCHAR(10))
                          >= :QTRX1-WBEG-DATE
              AND CAST(TRXFILE.TX_DATE AS VARCHAR(10))
                          <= :QTRX1-WEND-DATE
              AND TRXFILE.TX_CLASS >= :QTRX1-WBEG-CLASS
              AND TRXFILE.TX_CLASS <= :QTRX1-WEND-CLASS
              AND TRXFILE.TX_ACCTNO >= :QTRX1-WBEG-ACCTNO
              AND TRXFILE.TX_ACCTNO <= :QTRX1-WEND-ACCTNO
              AND TRXFILE.TX_LAWCODE >= :QTRX1-WBEG-LAWCODE
              AND TRXFILE.TX_LAWCODE <= :QTRX1-WEND-LAWCODE
              AND TRXFILE.TX_LINE >= :QTRX1-WBEG-LINE
              AND TRXFILE.TX_LINE <= :QTRX1-WEND-LINE
            ORDER BY TRXFILE.TX_BRNO,
                     TRXFILE.TX_DATE,
                     TRXFILE.TX_CLASS,
                     TRXFILE.TX_ACCTNO,
                     TRXFILE.TX_LAWCODE,
                     TRXFILE.TX_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRX1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRX1-WBEG-BRNO
                          QTRX1-WEND-BRNO
                          QTRX1-WBEG-CLASS
                          QTRX1-WEND-CLASS
                          QTRX1-WBEG-ACCTNO
                          QTRX1-WEND-ACCTNO
                          QTRX1-WBEG-LAWCODE
                          QTRX1-WEND-LAWCODE
                          QTRX1-WBEG-LINE
                          QTRX1-WEND-LINE.
           MOVE SPACES TO QTRX1-WBEG-DATE
                          QTRX1-WEND-DATE.


           MOVE STAT---GOOD TO TRX1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRX1-FILE.
           PERFORM READ-IT.
           MOVE TRX-PATH-OWNBR  TO TX-BRNO. 
           MOVE TRX-SELECT-PATH TO E-FILE.
           MOVE TRX1-KEY        TO E-KEYX.

           MOVE TX-BRNO             TO QTX-BRNO.
           MOVE TX-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTX-DATE.
           MOVE TX-CLASS            TO QTX-CLASS.
           MOVE TX-ACCTNO           TO QTX-ACCTNO.
           MOVE TX-LAWCODE          TO QTX-LAWCODE.
           MOVE TX-LINE             TO QTX-LINE.


           EXEC SQL
            SELECT TRXFILE.TX_BRNO,
                   ISNULL(CAST(TRXFILE.TX_DATE AS VARCHAR(10)),'0'),
                   TRXFILE.TX_CLASS,
                   TRXFILE.TX_ACCTNO,
                   TRXFILE.TX_LAWCODE,
                   TRXFILE.TX_LINE,
                   ISNULL(CAST(TRXFILE.TX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRXFILE.TX_SSNO_1,
                   ISNULL(TRXFILE.TX_MAKERCD_1,''),
                   TRXFILE.TX_SSNO_2,
                   ISNULL(TRXFILE.TX_MAKERCD_2,''),
                   TRXFILE.TX_SSNO_3,
                   ISNULL(TRXFILE.TX_MAKERCD_3,''),
                   TRXFILE.TX_SSNO_4,
                   ISNULL(TRXFILE.TX_MAKERCD_4,''),
                   ISNULL(TRXFILE.TX_PURCD,''),
                   ISNULL(TRXFILE.TX_SRCD,''),
                   ISNULL(TRXFILE.TX_COLLCD,''),
                   ISNULL(TRXFILE.TX_SERVACCT,''),
                   ISNULL(TRXFILE.TX_LOANTYPE,''),
                   TRXFILE.TX_SUBCLASS,
                   ISNULL(CAST(TRXFILE.TX_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   TRXFILE.TX_PROCEEDS,
                   TRXFILE.TX_LNAMT,
                   TRXFILE.TX_FINCHG,
                   TRXFILE.TX_INTCHG,
                   TRXFILE.TX_SERCHG,
                   TRXFILE.TX_EXTCHG,
                   TRXFILE.TX_MAINTFEE,
                   TRXFILE.TX_ORGTERM,
                   ISNULL(TRXFILE.TX_INSURED,''),
                   TRXFILE.TX_INSANTICERN_1,
                   TRXFILE.TX_INSANTICERN_2,
                   TRXFILE.TX_INSANTICERN_3,
                   TRXFILE.TX_INSANTICERN_4,
                   TRXFILE.TX_INSANTICERN_5,
                   TRXFILE.TX_INSANTICERN_6,
                   TRXFILE.TX_INSANTICERN_7,
                   TRXFILE.TX_INSANTICERN_8,
                   TRXFILE.TX_INSANTICERN_9,
                   TRXFILE.TX_INSANTICERN_10,
                   TRXFILE.TX_INSEARNINGS_1,
                   TRXFILE.TX_INSEARNINGS_2,
                   TRXFILE.TX_INSEARNINGS_3,
                   TRXFILE.TX_INSEARNINGS_4,
                   TRXFILE.TX_INSEARNINGS_5,
                   TRXFILE.TX_INSEARNINGS_6,
                   TRXFILE.TX_INSEARNINGS_7,
                   TRXFILE.TX_INSEARNINGS_8,
                   TRXFILE.TX_INSEARNINGS_9,
                   TRXFILE.TX_INSEARNINGS_10,
                   TRXFILE.TX_DLNO,
                   ISNULL(TRXFILE.TX_DLRECOURSE,''),
                   TRXFILE.TX_DLPARTRCAMT,
                   TRXFILE.TX_PURBAL,
                   TRXFILE.TX_DLRVPRIN_1,
                   TRXFILE.TX_DLRVPRIN_2,
                   TRXFILE.TX_DLRVPRIN_3,
                   TRXFILE.TX_DLRVCHGS_1,
                   TRXFILE.TX_DLRVCHGS_2,
                   TRXFILE.TX_DLRVCHGS_3,
                   TRXFILE.TX_DLPARVHELD,
                   TRXFILE.TX_DLPARVPAID,
                   TRXFILE.TX_DLDISC,
                   TRXFILE.TX_ANTICTERM,
                   TRXFILE.TX_ANTICERN_1,
                   TRXFILE.TX_ANTICERN_2,
                   TRXFILE.TX_ANTICERN_3,
                   TRXFILE.TX_ANTICERN_4,
                   TRXFILE.TX_ANTICERN_5,
                   TRXFILE.TX_ANTICERN_6,
                   TRXFILE.TX_ANTICERN_7,
                   TRXFILE.TX_ANTICERN_8,
                   TRXFILE.TX_ANTICERN_9,
                   TRXFILE.TX_ANTICERN_10,
                   TRXFILE.TX_EARNINGS_1,
                   TRXFILE.TX_EARNINGS_2,
                   TRXFILE.TX_EARNINGS_3,
                   TRXFILE.TX_EARNINGS_4,
                   TRXFILE.TX_EARNINGS_5,
                   TRXFILE.TX_EARNINGS_6,
                   TRXFILE.TX_EARNINGS_7,
                   TRXFILE.TX_EARNINGS_8,
                   TRXFILE.TX_EARNINGS_9,
                   TRXFILE.TX_EARNINGS_10,
                   TRXFILE.TX_PRLNNO,
                   TRXFILE.TX_PRBAL,
                   TRXFILE.TX_TFRBAL,
                   TRXFILE.TX_SPRCLASS,
                   ISNULL(TRXFILE.TX_USERID,''),
                   ISNULL(CAST(TRXFILE.TX_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRXFILE.TX_POSTTIME
            INTO :QTRX-REC
            FROM DBO.TRXFILE
            WHERE TRXFILE.TX_BRNO = :QTX-BRNO
              AND TRXFILE.TX_DATE = :QTX-DATE
              AND TRXFILE.TX_CLASS = :QTX-CLASS
              AND TRXFILE.TX_ACCTNO = :QTX-ACCTNO
              AND TRXFILE.TX_LAWCODE = :QTX-LAWCODE
              AND TRXFILE.TX_LINE = :QTX-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRX1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRX-FIELDS.

      *-----------------------------------------------------------------
       READ-TRX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRX-PATH-OWNBR  TO TX-BRNO.
           MOVE TRX-SELECT-PATH TO E-FILE.
           MOVE TRX1-KEY        TO E-KEYX.
           INITIALIZE QTRX-REC.

           IF ( TRX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRX1_CURSOR INTO :QTRX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRX1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRX-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRX1-FILE.

           IF ( TRX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRX1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRX1-CURSOR-STAT.


      *===========================================================
       LOAD-TRX2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRX-ALL-DATA
              MOVE TRX-ALL-PATH             TO TRX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRX-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRX-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRX-ALL-DATA
              MOVE TRX-ALL-PATH             TO TRX-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRXFILE NOT = "S" )
              MOVE TRX-PATH-BEG             TO TRX-SELECT-PATH-BEG
              MOVE TRX-PATH-FILE            TO TRX-SELECT-PATH-FILE
           ELSE
              MOVE TRX-PATH-SQL             TO TRX-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRX2-FILE.
           PERFORM LOAD-TRX2-FILE.
           PERFORM OPEN-IT.
           MOVE TRX-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRX2-FILE.
           PERFORM START-IT.
           MOVE TRX-PATH-OWNBR  TO TX-BRNO.
           MOVE TRX-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING TX-DATE, TX-BRNO, TX-CLASS, TX-ACCTNO, TX-LAWCODE,
                  TX-LINE DELIMITED BY SIZE INTO E-KEYX.

           IF ( TRX2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRX2-FILE.

           IF ( QTRX2-WEND-LINE = ZEROES    ) OR
              ( QTRX2-WEND-LINE NOT NUMERIC )
              MOVE TX-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRX2-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRX2-WEND-DATE
              MOVE TRX-PATH-OWNBR       TO QTRX2-WBEG-BRNO
              MOVE TRX-PATH-OWNBR       TO QTRX2-WEND-BRNO
              MOVE TX-CLASS             TO QTRX2-WBEG-CLASS
              MOVE TX-ACCTNO            TO QTRX2-WBEG-ACCTNO
              MOVE TX-LAWCODE           TO QTRX2-WBEG-LAWCODE
              MOVE TX-LINE              TO QTRX2-WBEG-LINE
              MOVE ALL "9"              TO QTRX2-WEND-CLASS
                                           QTRX2-WEND-ACCTNO
                                           QTRX2-WEND-LAWCODE
                                           QTRX2-WEND-LINE.

           EXEC SQL
            DECLARE TRX2_CURSOR CURSOR FOR
            SELECT TRXFILE.TX_BRNO,
                   ISNULL(CAST(TRXFILE.TX_DATE AS VARCHAR(10)),'0'),
                   TRXFILE.TX_CLASS,
                   TRXFILE.TX_ACCTNO,
                   TRXFILE.TX_LAWCODE,
                   TRXFILE.TX_LINE,
                   ISNULL(CAST(TRXFILE.TX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRXFILE.TX_SSNO_1,
                   ISNULL(TRXFILE.TX_MAKERCD_1,''),
                   TRXFILE.TX_SSNO_2,
                   ISNULL(TRXFILE.TX_MAKERCD_2,''),
                   TRXFILE.TX_SSNO_3,
                   ISNULL(TRXFILE.TX_MAKERCD_3,''),
                   TRXFILE.TX_SSNO_4,
                   ISNULL(TRXFILE.TX_MAKERCD_4,''),
                   ISNULL(TRXFILE.TX_PURCD,''),
                   ISNULL(TRXFILE.TX_SRCD,''),
                   ISNULL(TRXFILE.TX_COLLCD,''),
                   ISNULL(TRXFILE.TX_SERVACCT,''),
                   ISNULL(TRXFILE.TX_LOANTYPE,''),
                   TRXFILE.TX_SUBCLASS,
                   ISNULL(CAST(TRXFILE.TX_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   TRXFILE.TX_PROCEEDS,
                   TRXFILE.TX_LNAMT,
                   TRXFILE.TX_FINCHG,
                   TRXFILE.TX_INTCHG,
                   TRXFILE.TX_SERCHG,
                   TRXFILE.TX_EXTCHG,
                   TRXFILE.TX_MAINTFEE,
                   TRXFILE.TX_ORGTERM,
                   ISNULL(TRXFILE.TX_INSURED,''),
                   TRXFILE.TX_INSANTICERN_1,
                   TRXFILE.TX_INSANTICERN_2,
                   TRXFILE.TX_INSANTICERN_3,
                   TRXFILE.TX_INSANTICERN_4,
                   TRXFILE.TX_INSANTICERN_5,
                   TRXFILE.TX_INSANTICERN_6,
                   TRXFILE.TX_INSANTICERN_7,
                   TRXFILE.TX_INSANTICERN_8,
                   TRXFILE.TX_INSANTICERN_9,
                   TRXFILE.TX_INSANTICERN_10,
                   TRXFILE.TX_INSEARNINGS_1,
                   TRXFILE.TX_INSEARNINGS_2,
                   TRXFILE.TX_INSEARNINGS_3,
                   TRXFILE.TX_INSEARNINGS_4,
                   TRXFILE.TX_INSEARNINGS_5,
                   TRXFILE.TX_INSEARNINGS_6,
                   TRXFILE.TX_INSEARNINGS_7,
                   TRXFILE.TX_INSEARNINGS_8,
                   TRXFILE.TX_INSEARNINGS_9,
                   TRXFILE.TX_INSEARNINGS_10,
                   TRXFILE.TX_DLNO,
                   ISNULL(TRXFILE.TX_DLRECOURSE,''),
                   TRXFILE.TX_DLPARTRCAMT,
                   TRXFILE.TX_PURBAL,
                   TRXFILE.TX_DLRVPRIN_1,
                   TRXFILE.TX_DLRVPRIN_2,
                   TRXFILE.TX_DLRVPRIN_3,
                   TRXFILE.TX_DLRVCHGS_1,
                   TRXFILE.TX_DLRVCHGS_2,
                   TRXFILE.TX_DLRVCHGS_3,
                   TRXFILE.TX_DLPARVHELD,
                   TRXFILE.TX_DLPARVPAID,
                   TRXFILE.TX_DLDISC,
                   TRXFILE.TX_ANTICTERM,
                   TRXFILE.TX_ANTICERN_1,
                   TRXFILE.TX_ANTICERN_2,
                   TRXFILE.TX_ANTICERN_3,
                   TRXFILE.TX_ANTICERN_4,
                   TRXFILE.TX_ANTICERN_5,
                   TRXFILE.TX_ANTICERN_6,
                   TRXFILE.TX_ANTICERN_7,
                   TRXFILE.TX_ANTICERN_8,
                   TRXFILE.TX_ANTICERN_9,
                   TRXFILE.TX_ANTICERN_10,
                   TRXFILE.TX_EARNINGS_1,
                   TRXFILE.TX_EARNINGS_2,
                   TRXFILE.TX_EARNINGS_3,
                   TRXFILE.TX_EARNINGS_4,
                   TRXFILE.TX_EARNINGS_5,
                   TRXFILE.TX_EARNINGS_6,
                   TRXFILE.TX_EARNINGS_7,
                   TRXFILE.TX_EARNINGS_8,
                   TRXFILE.TX_EARNINGS_9,
                   TRXFILE.TX_EARNINGS_10,
                   TRXFILE.TX_PRLNNO,
                   TRXFILE.TX_PRBAL,
                   TRXFILE.TX_TFRBAL,
                   TRXFILE.TX_SPRCLASS,
                   ISNULL(TRXFILE.TX_USERID,''),
                   ISNULL(CAST(TRXFILE.TX_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRXFILE.TX_POSTTIME
            FROM DBO.TRXFILE
            WHERE CAST(TRXFILE.TX_DATE AS VARCHAR(10))
                          >= :QTRX2-WBEG-DATE
              AND CAST(TRXFILE.TX_DATE AS VARCHAR(10))
                          <= :QTRX2-WEND-DATE
              AND TRXFILE.TX_BRNO >= :QTRX2-WBEG-BRNO
              AND TRXFILE.TX_BRNO <= :QTRX2-WEND-BRNO
              AND TRXFILE.TX_CLASS >= :QTRX2-WBEG-CLASS
              AND TRXFILE.TX_CLASS <= :QTRX2-WEND-CLASS
              AND TRXFILE.TX_ACCTNO >= :QTRX2-WBEG-ACCTNO
              AND TRXFILE.TX_ACCTNO <= :QTRX2-WEND-ACCTNO
              AND TRXFILE.TX_LAWCODE >= :QTRX2-WBEG-LAWCODE
              AND TRXFILE.TX_LAWCODE <= :QTRX2-WEND-LAWCODE
              AND TRXFILE.TX_LINE >= :QTRX2-WBEG-LINE
              AND TRXFILE.TX_LINE <= :QTRX2-WEND-LINE
            ORDER BY TRXFILE.TX_DATE,
                     TRXFILE.TX_BRNO,
                     TRXFILE.TX_CLASS,
                     TRXFILE.TX_ACCTNO,
                     TRXFILE.TX_LAWCODE,
                     TRXFILE.TX_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRX2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRX2-WBEG-BRNO
                          QTRX2-WEND-BRNO
                          QTRX2-WBEG-CLASS
                          QTRX2-WEND-CLASS
                          QTRX2-WBEG-ACCTNO
                          QTRX2-WEND-ACCTNO
                          QTRX2-WBEG-LAWCODE
                          QTRX2-WEND-LAWCODE
                          QTRX2-WBEG-LINE
                          QTRX2-WEND-LINE.
           MOVE SPACES TO QTRX2-WBEG-DATE
                          QTRX2-WEND-DATE.

           MOVE STAT---GOOD TO TRX2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRX2-FILE.
           PERFORM READ-IT.
           MOVE TRX-PATH-OWNBR  TO TX-BRNO. 
           MOVE TRX-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING TX-DATE, TX-BRNO, TX-CLASS, TX-ACCTNO, TX-LAWCODE,
                  TX-LINE DELIMITED BY SIZE INTO E-KEYX.

           MOVE TX-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTX-DATE.
           MOVE TX-BRNO             TO QTX-BRNO.
           MOVE TX-CLASS            TO QTX-CLASS.
           MOVE TX-ACCTNO           TO QTX-ACCTNO.
           MOVE TX-LAWCODE          TO QTX-LAWCODE.
           MOVE TX-LINE             TO QTX-LINE.

           EXEC SQL
            SELECT TRXFILE.TX_BRNO,
                   ISNULL(CAST(TRXFILE.TX_DATE AS VARCHAR(10)),'0'),
                   TRXFILE.TX_CLASS,
                   TRXFILE.TX_ACCTNO,
                   TRXFILE.TX_LAWCODE,
                   TRXFILE.TX_LINE,
                   ISNULL(CAST(TRXFILE.TX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRXFILE.TX_SSNO_1,
                   ISNULL(TRXFILE.TX_MAKERCD_1,''),
                   TRXFILE.TX_SSNO_2,
                   ISNULL(TRXFILE.TX_MAKERCD_2,''),
                   TRXFILE.TX_SSNO_3,
                   ISNULL(TRXFILE.TX_MAKERCD_3,''),
                   TRXFILE.TX_SSNO_4,
                   ISNULL(TRXFILE.TX_MAKERCD_4,''),
                   ISNULL(TRXFILE.TX_PURCD,''),
                   ISNULL(TRXFILE.TX_SRCD,''),
                   ISNULL(TRXFILE.TX_COLLCD,''),
                   ISNULL(TRXFILE.TX_SERVACCT,''),
                   ISNULL(TRXFILE.TX_LOANTYPE,''),
                   TRXFILE.TX_SUBCLASS,
                   ISNULL(CAST(TRXFILE.TX_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   TRXFILE.TX_PROCEEDS,
                   TRXFILE.TX_LNAMT,
                   TRXFILE.TX_FINCHG,
                   TRXFILE.TX_INTCHG,
                   TRXFILE.TX_SERCHG,
                   TRXFILE.TX_EXTCHG,
                   TRXFILE.TX_MAINTFEE,
                   TRXFILE.TX_ORGTERM,
                   ISNULL(TRXFILE.TX_INSURED,''),
                   TRXFILE.TX_INSANTICERN_1,
                   TRXFILE.TX_INSANTICERN_2,
                   TRXFILE.TX_INSANTICERN_3,
                   TRXFILE.TX_INSANTICERN_4,
                   TRXFILE.TX_INSANTICERN_5,
                   TRXFILE.TX_INSANTICERN_6,
                   TRXFILE.TX_INSANTICERN_7,
                   TRXFILE.TX_INSANTICERN_8,
                   TRXFILE.TX_INSANTICERN_9,
                   TRXFILE.TX_INSANTICERN_10,
                   TRXFILE.TX_INSEARNINGS_1,
                   TRXFILE.TX_INSEARNINGS_2,
                   TRXFILE.TX_INSEARNINGS_3,
                   TRXFILE.TX_INSEARNINGS_4,
                   TRXFILE.TX_INSEARNINGS_5,
                   TRXFILE.TX_INSEARNINGS_6,
                   TRXFILE.TX_INSEARNINGS_7,
                   TRXFILE.TX_INSEARNINGS_8,
                   TRXFILE.TX_INSEARNINGS_9,
                   TRXFILE.TX_INSEARNINGS_10,
                   TRXFILE.TX_DLNO,
                   ISNULL(TRXFILE.TX_DLRECOURSE,''),
                   TRXFILE.TX_DLPARTRCAMT,
                   TRXFILE.TX_PURBAL,
                   TRXFILE.TX_DLRVPRIN_1,
                   TRXFILE.TX_DLRVPRIN_2,
                   TRXFILE.TX_DLRVPRIN_3,
                   TRXFILE.TX_DLRVCHGS_1,
                   TRXFILE.TX_DLRVCHGS_2,
                   TRXFILE.TX_DLRVCHGS_3,
                   TRXFILE.TX_DLPARVHELD,
                   TRXFILE.TX_DLPARVPAID,
                   TRXFILE.TX_DLDISC,
                   TRXFILE.TX_ANTICTERM,
                   TRXFILE.TX_ANTICERN_1,
                   TRXFILE.TX_ANTICERN_2,
                   TRXFILE.TX_ANTICERN_3,
                   TRXFILE.TX_ANTICERN_4,
                   TRXFILE.TX_ANTICERN_5,
                   TRXFILE.TX_ANTICERN_6,
                   TRXFILE.TX_ANTICERN_7,
                   TRXFILE.TX_ANTICERN_8,
                   TRXFILE.TX_ANTICERN_9,
                   TRXFILE.TX_ANTICERN_10,
                   TRXFILE.TX_EARNINGS_1,
                   TRXFILE.TX_EARNINGS_2,
                   TRXFILE.TX_EARNINGS_3,
                   TRXFILE.TX_EARNINGS_4,
                   TRXFILE.TX_EARNINGS_5,
                   TRXFILE.TX_EARNINGS_6,
                   TRXFILE.TX_EARNINGS_7,
                   TRXFILE.TX_EARNINGS_8,
                   TRXFILE.TX_EARNINGS_9,
                   TRXFILE.TX_EARNINGS_10,
                   TRXFILE.TX_PRLNNO,
                   TRXFILE.TX_PRBAL,
                   TRXFILE.TX_TFRBAL,
                   TRXFILE.TX_SPRCLASS,
                   ISNULL(TRXFILE.TX_USERID,''),
                   ISNULL(CAST(TRXFILE.TX_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRXFILE.TX_POSTTIME
            INTO :QTRX-REC
            FROM DBO.TRXFILE
            WHERE TRXFILE.TX_DATE = :QTX-DATE
              AND TRXFILE.TX_BRNO = :QTX-BRNO
              AND TRXFILE.TX_CLASS = :QTX-CLASS
              AND TRXFILE.TX_ACCTNO = :QTX-ACCTNO
              AND TRXFILE.TX_LAWCODE = :QTX-LAWCODE
              AND TRXFILE.TX_LINE = :QTX-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRX2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRX-FIELDS.

      *-----------------------------------------------------------------
       READ-TRX2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRX-PATH-OWNBR  TO TX-BRNO.
           MOVE TRX-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING TX-DATE, TX-BRNO, TX-CLASS, TX-ACCTNO, TX-LAWCODE,
                  TX-LINE DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QTRX-REC.

           IF ( TRX2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRX2_CURSOR INTO :QTRX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRX2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRX-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRX2-FILE.

           IF ( TRX2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRX2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRX2-CURSOR-STAT.

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
              DESTROY IOTRXR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
