       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOTRKI.
       AUTHOR.        BAH
       DATE-WRITTEN.  04-13-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOTRKI.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : TRKFILE 
      * TABLE DESC    : TRANSACTION CONVERSIONS FILE
      * OUTPUT FILE   : XPC/IOTRKI.PRE
      * USED FOR      : LIBLP/LPWSTRK.CPY  --- QTRK-REC
      *                 LIBLP/LPTRK1R.CPY  --- READ
      *                 LIBLP/LPTRK1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0413 ***NEW***
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
           "@(#)./IO/IOTRKI.CBL S34 05/05/21-16:33:06 >".

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
       COPY "LIBLP/LP01TRK.CPY".
       COPY "LIBLP/LPWSTRK.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QTRK1-WHERE-BEG-KEY.
           03  QTRK1-WBEG-BRNO             PIC 9(04).
           03  QTRK1-WBEG-DATE             PIC X(10).
           03  QTRK1-WBEG-CLASS            PIC 9(03).
           03  QTRK1-WBEG-ACCTNO           PIC 9(08).
           03  QTRK1-WBEG-LAWCODE          PIC 9(02).
           03  QTRK1-WBEG-LINE             PIC 9(03).

       01  QTRK1-WHERE-END-KEY.
           03  QTRK1-WEND-BRNO             PIC 9(04).
           03  QTRK1-WEND-DATE             PIC X(10).
           03  QTRK1-WEND-CLASS            PIC 9(03).
           03  QTRK1-WEND-ACCTNO           PIC 9(08).
           03  QTRK1-WEND-LAWCODE          PIC 9(02).
           03  QTRK1-WEND-LINE             PIC 9(03).


       01  QTRK-REC.
           03  QTK-BRNO             PIC 9(4).
           03  QTK-DATE             PIC X(10).
           03  QTK-CLASS            PIC 9(3).
           03  QTK-ACCTNO           PIC 9(8).
           03  QTK-LAWCODE          PIC 99.
           03  QTK-LINE             PIC 9(3).
           03  QTK-LTOUCH-DATE        PIC X(10).
           03  QTK-SSNO-1             PIC 9(9).
           03  QTK-MAKERCD-1          PIC X.
           03  QTK-SSNO-2             PIC 9(9).
           03  QTK-MAKERCD-2          PIC X.
           03  QTK-SSNO-3             PIC 9(9).
           03  QTK-MAKERCD-3          PIC X.
           03  QTK-SSNO-4             PIC 9(9).
           03  QTK-MAKERCD-4          PIC X.
           03  QTK-REF-FRTO         PIC X.
           03  QTK-REFBR            PIC 9999.
           03  QTK-PURCD                PIC XXX.
           03  QTK-SRCD                 PIC XX.
           03  QTK-COLLCD               PIC XX.
           03  QTK-SERVACCT             PIC X.
           03  QTK-LOANTYPE             PIC X.
           03  QTK-SUBCLASS             PIC 99.
           03  QTK-REPOCD               PIC X.
           03  QTK-PLCD                 PIC X.
           03  QTK-PLTYPE               PIC X.
           03  QTK-LOANDATE             PIC X(10).
           03  QTK-PROCEEDS             PIC S9(7)V99.
           03  QTK-LNAMT                PIC S9(7)V99.
           03  QTK-FINCHG               PIC S9(7)V99.
           03  QTK-INTCHG               PIC 9(7)V99.
           03  QTK-SERCHG               PIC 9(5)V99.
           03  QTK-EXTCHG               PIC 9(5)V99.
           03  QTK-MAINTFEE             PIC 9(5)V99.
           03  QTK-ORGTERM              PIC 9(3).
           03  QTK-INSURED              PIC X.
           03  QTK-INSUNEARN-1          PIC S9(7)V99.
           03  QTK-INSUNEARN-2          PIC S9(7)V99.
           03  QTK-INSUNEARN-3          PIC S9(7)V99.
           03  QTK-INSUNEARN-4          PIC S9(7)V99.
           03  QTK-INSUNEARN-5          PIC S9(7)V99.
           03  QTK-INSUNEARN-6          PIC S9(7)V99.
           03  QTK-INSUNEARN-7          PIC S9(7)V99.
           03  QTK-INSUNEARN-8          PIC S9(7)V99.
           03  QTK-INSUNEARN-9          PIC S9(7)V99.
           03  QTK-INSUNEARN-10         PIC S9(7)V99.
           03  QTK-INSEARNED-1          PIC S9(7)V99.
           03  QTK-INSEARNED-2          PIC S9(7)V99.
           03  QTK-INSEARNED-3          PIC S9(7)V99.
           03  QTK-INSEARNED-4          PIC S9(7)V99.
           03  QTK-INSEARNED-5          PIC S9(7)V99.
           03  QTK-INSEARNED-6          PIC S9(7)V99.
           03  QTK-INSEARNED-7          PIC S9(7)V99.
           03  QTK-INSEARNED-8          PIC S9(7)V99.
           03  QTK-INSEARNED-9          PIC S9(7)V99.
           03  QTK-INSEARNED-10         PIC S9(7)V99.
           03  QTK-DLNO                 PIC 9(4)V99.
           03  QTK-DLRV                 PIC S9(7)V99.
           03  QTK-DLSPRV               PIC S9(7)V99.
           03  QTK-DLDISC               PIC S9(7)V99.
           03  QTK-DLPARV               PIC S9(7)V99.
           03  QTK-DLRECOURSE           PIC X.
           03  QTK-DLPARTRCAMT          PIC 9(6)V99.
           03  QTK-PURBAL               PIC S9(7)V99.
           03  QTK-UNEARN-1             PIC S9(7)V99.
           03  QTK-UNEARN-2             PIC S9(7)V99.
           03  QTK-UNEARN-3             PIC S9(7)V99.
           03  QTK-UNEARN-4             PIC S9(7)V99.
           03  QTK-UNEARN-5             PIC S9(7)V99.
           03  QTK-UNEARN-6             PIC S9(7)V99.
           03  QTK-UNEARN-7             PIC S9(7)V99.
           03  QTK-UNEARN-8             PIC S9(7)V99.
           03  QTK-UNEARN-9             PIC S9(7)V99.
           03  QTK-UNEARN-10            PIC S9(7)V99.
           03  QTK-PRLNNO               PIC 9(8).
           03  QTK-DLRVPRIN-1           PIC S9(7)V99.
           03  QTK-DLRVPRIN-2           PIC S9(7)V99.
           03  QTK-DLRVPRIN-3           PIC S9(7)V99.
           03  QTK-DLRVCHGS-1           PIC S9(7)V99.
           03  QTK-DLRVCHGS-2           PIC S9(7)V99.
           03  QTK-DLRVCHGS-3           PIC S9(7)V99.
           03  QTK-DLPARVPAID           PIC S9(7)V99.
           03  QTK-DLPARVHELD           PIC S9(7)V99.
           03  QTK-USERID               PIC X(10).
           03  QTK-POSTDATE             PIC X(10).
           03  QTK-POSTTIME             PIC 9(6).
           03  QTK-EARNED-1             PIC S9(7)V99.
           03  QTK-EARNED-2             PIC S9(7)V99.
           03  QTK-EARNED-3             PIC S9(7)V99.
           03  QTK-EARNED-4             PIC S9(7)V99.
           03  QTK-EARNED-5             PIC S9(7)V99.
           03  QTK-EARNED-6             PIC S9(7)V99.
           03  QTK-EARNED-7             PIC S9(7)V99.
           03  QTK-EARNED-8             PIC S9(7)V99.
           03  QTK-EARNED-9             PIC S9(7)V99.
           03  QTK-EARNED-10            PIC S9(7)V99.
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
           03  D01-TK-BRNO                   PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-TK-DATE                   PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-TK-CLASS                  PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-TK-ACCTNO                 PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-TK-LAWCODE                PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-TK-LINE                   PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-TK-SSNO1                  PIC 9(09).
           03  FILLER                         PIC X(03).
           03  D01-TK-LOANDATE               PIC 9999/99/99.



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
       COPY "LIBIO/IOTRKI_DEF.CPY".
       COPY "LIBIO/IOTRKI_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOTRKI_SCN.CPY".

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
           MOVE "IOTRKI"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOTRKI-QUEUE-POS  TO Q-POS.
           MOVE IOTRKI-COPIES-POS TO C-POS.
           MOVE IOTRKI-PRU-POS    TO P-POS.
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
              ( VDUNUM0 > 1      ) 
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

           MOVE "B"                        TO TRK-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QTRK1-WBEG-BRNO.
           MOVE BR-NO     TO QTRK1-WEND-BRNO.

           MOVE BEG-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRK1-WBEG-DATE.
           MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRK1-WEND-DATE.

           MOVE BEG-CLASS  TO QTRK1-WBEG-CLASS.
           MOVE END-CLASS  TO QTRK1-WEND-CLASS.

           MOVE BEG-ACCTNO TO QTRK1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QTRK1-WEND-ACCTNO.
           MOVE BEG-LAWCODE TO QTRK1-WBEG-LAWCODE.
           MOVE END-LAWCODE TO QTRK1-WEND-LAWCODE.
           MOVE BEG-LINE TO QTRK1-WBEG-LINE.
           MOVE END-LINE TO QTRK1-WEND-LINE.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-TRK1-FILE
           PERFORM START-TRK1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-TRK1-FILE-NEXT.
           IF ( IO-BAD ) 
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( TK-BRNO < BEG-BRANCH )
               GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( TK-BRNO > END-BRANCH )
              GO TO PROCESS-BRANCH-MODULE-END
           ELSE
              IF TK-DATE < BEG-DATE OR > END-DATE
                 GO TO PROCESS-BRANCH-NEXT.

           IF TK-CLASS < BEG-CLASS OR > END-CLASS
              GO TO PROCESS-BRANCH-NEXT.
           IF TK-ACCTNO < BEG-ACCTNO OR > END-ACCTNO
              GO TO PROCESS-BRANCH-NEXT.
           IF TK-LAWCODE < BEG-LAWCODE OR > END-LAWCODE
              GO TO PROCESS-BRANCH-NEXT.
           IF TK-LINE < BEG-LINE OR > END-LINE
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.


           MOVE TK-BRNO                    TO D01-TK-BRNO.
           MOVE TK-DATE                    TO D01-TK-DATE.
           MOVE TK-CLASS                   TO D01-TK-CLASS.
           MOVE TK-ACCTNO                  TO D01-TK-ACCTNO.
           MOVE TK-LAWCODE                 TO D01-TK-LAWCODE.
           MOVE TK-LINE                    TO D01-TK-LINE.
           MOVE TK-SSNO(1)                 TO D01-TK-SSNO1.
           MOVE TK-LOANDATE                TO D01-TK-LOANDATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-TRK1-FILE.

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
              MOVE TRK1-KEY TO PACE-DETAIL-KEY
           ELSE
              STRING TK-DATE, TK-BRNO, TK-CLASS, TK-ACCTNO, TK-LAWCODE,
                     TK-LINE DELIMITED BY SIZE INTO PACE-DETAIL-KEY.

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
           MOVE "SQL TABLE IO UTILITY - TRKFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOTRKI-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOTRKI-SYSDATE.
           DISPLAY IOTRKI-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOTRKI-FORM-FIELDS TO WR-BUF.
           MOVE IOTRKI-FORM-LIST TO WR-LIST.
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

           MOVE IOTRKI-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOTRKI-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOTRKI_EVA.CPY".
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
      *    LOAD TRK-REC FROM QTRK-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-TRK-FIELDS.
           MOVE QTK-BRNO TO TK-BRNO.

           IF ( QTK-DATE = "1900-01-01" )
              MOVE ZEROES TO TK-DATE
           ELSE
              MOVE QTK-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TK-DATE.

           MOVE QTK-CLASS TO TK-CLASS.
           MOVE QTK-ACCTNO TO TK-ACCTNO.
           MOVE QTK-LAWCODE TO TK-LAWCODE.
           MOVE QTK-LINE TO TK-LINE.

           IF ( QTK-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO TK-LTOUCH-DATE
           ELSE
              MOVE QTK-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TK-LTOUCH-DATE.

           MOVE QTK-SSNO-1 TO TK-SSNO(1).
           MOVE QTK-MAKERCD-1 TO TK-MAKERCD(1).
           MOVE QTK-SSNO-2 TO TK-SSNO(2).
           MOVE QTK-MAKERCD-2 TO TK-MAKERCD(2).
           MOVE QTK-SSNO-3 TO TK-SSNO(3).
           MOVE QTK-MAKERCD-3 TO TK-MAKERCD(3).
           MOVE QTK-SSNO-4 TO TK-SSNO(4).
           MOVE QTK-MAKERCD-4 TO TK-MAKERCD(4).
           MOVE QTK-REF-FRTO TO TK-REF-FRTO.
           MOVE QTK-REFBR TO TK-REFBR.
           MOVE QTK-PURCD TO TK-PURCD.
           MOVE QTK-SRCD TO TK-SRCD.
           MOVE QTK-COLLCD TO TK-COLLCD.
           MOVE QTK-SERVACCT TO TK-SERVACCT.
           MOVE QTK-LOANTYPE TO TK-LOANTYPE.
           MOVE QTK-SUBCLASS TO TK-SUBCLASS.
           MOVE QTK-REPOCD TO TK-REPOCD.
           MOVE QTK-PLCD TO TK-PLCD.
           MOVE QTK-PLTYPE TO TK-PLTYPE.

           IF ( QTK-LOANDATE = "1900-01-01" )
              MOVE ZEROES TO TK-LOANDATE
           ELSE
              MOVE QTK-LOANDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TK-LOANDATE.

           MOVE QTK-PROCEEDS TO TK-PROCEEDS.
           MOVE QTK-LNAMT TO TK-LNAMT.
           MOVE QTK-FINCHG TO TK-FINCHG.
           MOVE QTK-INTCHG TO TK-INTCHG.
           MOVE QTK-SERCHG TO TK-SERCHG.
           MOVE QTK-EXTCHG TO TK-EXTCHG.
           MOVE QTK-MAINTFEE TO TK-MAINTFEE.
           MOVE QTK-ORGTERM TO TK-ORGTERM.
           MOVE QTK-INSURED TO TK-INSURED.
           MOVE QTK-INSUNEARN-1 TO TK-INSUNEARN(1).
           MOVE QTK-INSUNEARN-2 TO TK-INSUNEARN(2).
           MOVE QTK-INSUNEARN-3 TO TK-INSUNEARN(3).
           MOVE QTK-INSUNEARN-4 TO TK-INSUNEARN(4).
           MOVE QTK-INSUNEARN-5 TO TK-INSUNEARN(5).
           MOVE QTK-INSUNEARN-6 TO TK-INSUNEARN(6).
           MOVE QTK-INSUNEARN-7 TO TK-INSUNEARN(7).
           MOVE QTK-INSUNEARN-8 TO TK-INSUNEARN(8).
           MOVE QTK-INSUNEARN-9 TO TK-INSUNEARN(9).
           MOVE QTK-INSUNEARN-10 TO TK-INSUNEARN(10).
           MOVE QTK-INSEARNED-1 TO TK-INSEARNED(1).
           MOVE QTK-INSEARNED-2 TO TK-INSEARNED(2).
           MOVE QTK-INSEARNED-3 TO TK-INSEARNED(3).
           MOVE QTK-INSEARNED-4 TO TK-INSEARNED(4).
           MOVE QTK-INSEARNED-5 TO TK-INSEARNED(5).
           MOVE QTK-INSEARNED-6 TO TK-INSEARNED(6).
           MOVE QTK-INSEARNED-7 TO TK-INSEARNED(7).
           MOVE QTK-INSEARNED-8 TO TK-INSEARNED(8).
           MOVE QTK-INSEARNED-9 TO TK-INSEARNED(9).
           MOVE QTK-INSEARNED-10 TO TK-INSEARNED(10).
           MOVE QTK-DLNO TO TK-DLNO.
           MOVE QTK-DLRV TO TK-DLRV.
           MOVE QTK-DLSPRV TO TK-DLSPRV.
           MOVE QTK-DLDISC TO TK-DLDISC.
           MOVE QTK-DLPARV TO TK-DLPARV.
           MOVE QTK-DLRECOURSE TO TK-DLRECOURSE.
           MOVE QTK-DLPARTRCAMT TO TK-DLPARTRCAMT.
           MOVE QTK-PURBAL TO TK-PURBAL.
           MOVE QTK-UNEARN-1 TO TK-UNEARN(1).
           MOVE QTK-UNEARN-2 TO TK-UNEARN(2).
           MOVE QTK-UNEARN-3 TO TK-UNEARN(3).
           MOVE QTK-UNEARN-4 TO TK-UNEARN(4).
           MOVE QTK-UNEARN-5 TO TK-UNEARN(5).
           MOVE QTK-UNEARN-6 TO TK-UNEARN(6).
           MOVE QTK-UNEARN-7 TO TK-UNEARN(7).
           MOVE QTK-UNEARN-8 TO TK-UNEARN(8).
           MOVE QTK-UNEARN-9 TO TK-UNEARN(9).
           MOVE QTK-UNEARN-10 TO TK-UNEARN(10).
           MOVE QTK-PRLNNO TO TK-PRLNNO.
           MOVE QTK-DLRVPRIN-1 TO TK-DLRVPRIN(1).
           MOVE QTK-DLRVPRIN-2 TO TK-DLRVPRIN(2).
           MOVE QTK-DLRVPRIN-3 TO TK-DLRVPRIN(3).
           MOVE QTK-DLRVCHGS-1 TO TK-DLRVCHGS(1).
           MOVE QTK-DLRVCHGS-2 TO TK-DLRVCHGS(2).
           MOVE QTK-DLRVCHGS-3 TO TK-DLRVCHGS(3).
           MOVE QTK-DLPARVPAID TO TK-DLPARVPAID.
           MOVE QTK-DLPARVHELD TO TK-DLPARVHELD.
           MOVE QTK-USERID TO TK-USERID.

           IF ( QTK-POSTDATE = "1900-01-01" )
              MOVE ZEROES TO TK-POSTDATE
           ELSE
              MOVE QTK-POSTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TK-POSTDATE.

           MOVE QTK-POSTTIME TO TK-POSTTIME.
           MOVE QTK-EARNED-1 TO TK-EARNED(1).
           MOVE QTK-EARNED-2 TO TK-EARNED(2).
           MOVE QTK-EARNED-3 TO TK-EARNED(3).
           MOVE QTK-EARNED-4 TO TK-EARNED(4).
           MOVE QTK-EARNED-5 TO TK-EARNED(5).
           MOVE QTK-EARNED-6 TO TK-EARNED(6).
           MOVE QTK-EARNED-7 TO TK-EARNED(7).
           MOVE QTK-EARNED-8 TO TK-EARNED(8).
           MOVE QTK-EARNED-9 TO TK-EARNED(9).
           MOVE QTK-EARNED-10 TO TK-EARNED(10).
 
      *-----------------------------------------------------------------
      *    LOAD QTRK-REC FROM TRK-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-TRK-FIELDS.
           MOVE TK-BRNO TO QTK-BRNO.

           IF ( TK-DATE = ZEROES )
              MOVE "1900-01-01" TO QTK-DATE
           ELSE
              MOVE TK-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTK-DATE.

           MOVE TK-CLASS TO QTK-CLASS.
           MOVE TK-ACCTNO TO QTK-ACCTNO.
           MOVE TK-LAWCODE TO QTK-LAWCODE.
           MOVE TK-LINE TO QTK-LINE.

           IF ( TK-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTK-LTOUCH-DATE
           ELSE
              MOVE TK-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTK-LTOUCH-DATE.

           MOVE TK-SSNO(1) TO QTK-SSNO-1.
           MOVE TK-MAKERCD(1) TO QTK-MAKERCD-1.
           MOVE TK-SSNO(2) TO QTK-SSNO-2.
           MOVE TK-MAKERCD(2) TO QTK-MAKERCD-2.
           MOVE TK-SSNO(3) TO QTK-SSNO-3.
           MOVE TK-MAKERCD(3) TO QTK-MAKERCD-3.
           MOVE TK-SSNO(4) TO QTK-SSNO-4.
           MOVE TK-MAKERCD(4) TO QTK-MAKERCD-4.
           MOVE TK-REF-FRTO TO QTK-REF-FRTO.
           MOVE TK-REFBR TO QTK-REFBR.
           MOVE TK-PURCD TO QTK-PURCD.
           MOVE TK-SRCD TO QTK-SRCD.
           MOVE TK-COLLCD TO QTK-COLLCD.
           MOVE TK-SERVACCT TO QTK-SERVACCT.
           MOVE TK-LOANTYPE TO QTK-LOANTYPE.
           MOVE TK-SUBCLASS TO QTK-SUBCLASS.
           MOVE TK-REPOCD TO QTK-REPOCD.
           MOVE TK-PLCD TO QTK-PLCD.
           MOVE TK-PLTYPE TO QTK-PLTYPE.

           IF ( TK-LOANDATE = ZEROES )
              MOVE "1900-01-01" TO QTK-LOANDATE
           ELSE
              MOVE TK-LOANDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTK-LOANDATE.

           MOVE TK-PROCEEDS TO QTK-PROCEEDS.
           MOVE TK-LNAMT TO QTK-LNAMT.
           MOVE TK-FINCHG TO QTK-FINCHG.
           MOVE TK-INTCHG TO QTK-INTCHG.
           MOVE TK-SERCHG TO QTK-SERCHG.
           MOVE TK-EXTCHG TO QTK-EXTCHG.
           MOVE TK-MAINTFEE TO QTK-MAINTFEE.
           MOVE TK-ORGTERM TO QTK-ORGTERM.
           MOVE TK-INSURED TO QTK-INSURED.
           MOVE TK-INSUNEARN(1) TO QTK-INSUNEARN-1.
           MOVE TK-INSUNEARN(2) TO QTK-INSUNEARN-2.
           MOVE TK-INSUNEARN(3) TO QTK-INSUNEARN-3.
           MOVE TK-INSUNEARN(4) TO QTK-INSUNEARN-4.
           MOVE TK-INSUNEARN(5) TO QTK-INSUNEARN-5.
           MOVE TK-INSUNEARN(6) TO QTK-INSUNEARN-6.
           MOVE TK-INSUNEARN(7) TO QTK-INSUNEARN-7.
           MOVE TK-INSUNEARN(8) TO QTK-INSUNEARN-8.
           MOVE TK-INSUNEARN(9) TO QTK-INSUNEARN-9.
           MOVE TK-INSUNEARN(10) TO QTK-INSUNEARN-10.
           MOVE TK-INSEARNED(1) TO QTK-INSEARNED-1.
           MOVE TK-INSEARNED(2) TO QTK-INSEARNED-2.
           MOVE TK-INSEARNED(3) TO QTK-INSEARNED-3.
           MOVE TK-INSEARNED(4) TO QTK-INSEARNED-4.
           MOVE TK-INSEARNED(5) TO QTK-INSEARNED-5.
           MOVE TK-INSEARNED(6) TO QTK-INSEARNED-6.
           MOVE TK-INSEARNED(7) TO QTK-INSEARNED-7.
           MOVE TK-INSEARNED(8) TO QTK-INSEARNED-8.
           MOVE TK-INSEARNED(9) TO QTK-INSEARNED-9.
           MOVE TK-INSEARNED(10) TO QTK-INSEARNED-10.
           MOVE TK-DLNO TO QTK-DLNO.
           MOVE TK-DLRV TO QTK-DLRV.
           MOVE TK-DLSPRV TO QTK-DLSPRV.
           MOVE TK-DLDISC TO QTK-DLDISC.
           MOVE TK-DLPARV TO QTK-DLPARV.
           MOVE TK-DLRECOURSE TO QTK-DLRECOURSE.
           MOVE TK-DLPARTRCAMT TO QTK-DLPARTRCAMT.
           MOVE TK-PURBAL TO QTK-PURBAL.
           MOVE TK-UNEARN(1) TO QTK-UNEARN-1.
           MOVE TK-UNEARN(2) TO QTK-UNEARN-2.
           MOVE TK-UNEARN(3) TO QTK-UNEARN-3.
           MOVE TK-UNEARN(4) TO QTK-UNEARN-4.
           MOVE TK-UNEARN(5) TO QTK-UNEARN-5.
           MOVE TK-UNEARN(6) TO QTK-UNEARN-6.
           MOVE TK-UNEARN(7) TO QTK-UNEARN-7.
           MOVE TK-UNEARN(8) TO QTK-UNEARN-8.
           MOVE TK-UNEARN(9) TO QTK-UNEARN-9.
           MOVE TK-UNEARN(10) TO QTK-UNEARN-10.
           MOVE TK-PRLNNO TO QTK-PRLNNO.
           MOVE TK-DLRVPRIN(1) TO QTK-DLRVPRIN-1.
           MOVE TK-DLRVPRIN(2) TO QTK-DLRVPRIN-2.
           MOVE TK-DLRVPRIN(3) TO QTK-DLRVPRIN-3.
           MOVE TK-DLRVCHGS(1) TO QTK-DLRVCHGS-1.
           MOVE TK-DLRVCHGS(2) TO QTK-DLRVCHGS-2.
           MOVE TK-DLRVCHGS(3) TO QTK-DLRVCHGS-3.
           MOVE TK-DLPARVPAID TO QTK-DLPARVPAID.
           MOVE TK-DLPARVHELD TO QTK-DLPARVHELD.
           MOVE TK-USERID TO QTK-USERID.

           IF ( TK-POSTDATE = ZEROES )
              MOVE "1900-01-01" TO QTK-POSTDATE
           ELSE
              MOVE TK-POSTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTK-POSTDATE.

           MOVE TK-POSTTIME TO QTK-POSTTIME.
           MOVE TK-EARNED(1) TO QTK-EARNED-1.
           MOVE TK-EARNED(2) TO QTK-EARNED-2.
           MOVE TK-EARNED(3) TO QTK-EARNED-3.
           MOVE TK-EARNED(4) TO QTK-EARNED-4.
           MOVE TK-EARNED(5) TO QTK-EARNED-5.
           MOVE TK-EARNED(6) TO QTK-EARNED-6.
           MOVE TK-EARNED(7) TO QTK-EARNED-7.
           MOVE TK-EARNED(8) TO QTK-EARNED-8.
           MOVE TK-EARNED(9) TO QTK-EARNED-9.
           MOVE TK-EARNED(10) TO QTK-EARNED-10.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-TRK1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRK-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRK-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRK-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRK-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRK-ALL-DATA
              MOVE TRK-ALL-PATH             TO TRK-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRK-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRK-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRK-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRK-ALL-DATA
              MOVE TRK-ALL-PATH             TO TRK-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRKFILE NOT = "S" )
              MOVE TRK-PATH-BEG             TO TRK-SELECT-PATH-BEG
              MOVE TRK-PATH-FILE            TO TRK-SELECT-PATH-FILE
           ELSE
              MOVE TRK-PATH-SQL             TO TRK-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRK1-FILE.
           PERFORM LOAD-TRK1-FILE.
           PERFORM OPEN-IT.
           MOVE TRK-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRK1-FILE.
           PERFORM START-IT.
           MOVE TRK-PATH-OWNBR  TO TK-BRNO.
           MOVE TRK-SELECT-PATH TO E-FILE.
           MOVE TRK1-KEY        TO E-KEYX.

           IF ( TRK1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRK1-FILE.

           IF ( QTRK1-WEND-LINE = ZEROES    ) OR
              ( QTRK1-WEND-LINE NOT NUMERIC )
              MOVE TRK-PATH-OWNBR        TO QTRK1-WBEG-BRNO
              MOVE TRK-PATH-OWNBR        TO QTRK1-WEND-BRNO
              MOVE TK-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRK1-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRK1-WEND-DATE
              MOVE TK-CLASS             TO QTRK1-WBEG-CLASS
              MOVE TK-ACCTNO            TO QTRK1-WBEG-ACCTNO
              MOVE TK-LAWCODE           TO QTRK1-WBEG-LAWCODE
              MOVE TK-LINE              TO QTRK1-WBEG-LINE
              MOVE ALL "9"              TO QTRK1-WEND-CLASS
                                           QTRK1-WEND-ACCTNO
                                           QTRK1-WEND-LAWCODE
                                           QTRK1-WEND-LINE.


           EXEC SQL
            DECLARE TRK1_CURSOR CURSOR FOR
            SELECT TRKFILE.TK_BRNO,
                   ISNULL(CAST(TRKFILE.TK_DATE AS VARCHAR(10)),'0'),
                   TRKFILE.TK_CLASS,
                   TRKFILE.TK_ACCTNO,
                   TRKFILE.TK_LAWCODE,
                   TRKFILE.TK_LINE,
                   ISNULL(CAST(TRKFILE.TK_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRKFILE.TK_SSNO_1,
                   ISNULL(TRKFILE.TK_MAKERCD_1,''),
                   TRKFILE.TK_SSNO_2,
                   ISNULL(TRKFILE.TK_MAKERCD_2,''),
                   TRKFILE.TK_SSNO_3,
                   ISNULL(TRKFILE.TK_MAKERCD_3,''),
                   TRKFILE.TK_SSNO_4,
                   ISNULL(TRKFILE.TK_MAKERCD_4,''),
                   ISNULL(TRKFILE.TK_REF_FRTO,''),
                   TRKFILE.TK_REFBR,
                   ISNULL(TRKFILE.TK_PURCD,''),
                   ISNULL(TRKFILE.TK_SRCD,''),
                   ISNULL(TRKFILE.TK_COLLCD,''),
                   ISNULL(TRKFILE.TK_SERVACCT,''),
                   ISNULL(TRKFILE.TK_LOANTYPE,''),
                   TRKFILE.TK_SUBCLASS,
                   ISNULL(TRKFILE.TK_REPOCD,''),
                   ISNULL(TRKFILE.TK_PLCD,''),
                   ISNULL(TRKFILE.TK_PLTYPE,''),
                   ISNULL(CAST(TRKFILE.TK_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   TRKFILE.TK_PROCEEDS,
                   TRKFILE.TK_LNAMT,
                   TRKFILE.TK_FINCHG,
                   TRKFILE.TK_INTCHG,
                   TRKFILE.TK_SERCHG,
                   TRKFILE.TK_EXTCHG,
                   TRKFILE.TK_MAINTFEE,
                   TRKFILE.TK_ORGTERM,
                   ISNULL(TRKFILE.TK_INSURED,''),
                   TRKFILE.TK_INSUNEARN_1,
                   TRKFILE.TK_INSUNEARN_2,
                   TRKFILE.TK_INSUNEARN_3,
                   TRKFILE.TK_INSUNEARN_4,
                   TRKFILE.TK_INSUNEARN_5,
                   TRKFILE.TK_INSUNEARN_6,
                   TRKFILE.TK_INSUNEARN_7,
                   TRKFILE.TK_INSUNEARN_8,
                   TRKFILE.TK_INSUNEARN_9,
                   TRKFILE.TK_INSUNEARN_10,
                   TRKFILE.TK_INSEARNED_1,
                   TRKFILE.TK_INSEARNED_2,
                   TRKFILE.TK_INSEARNED_3,
                   TRKFILE.TK_INSEARNED_4,
                   TRKFILE.TK_INSEARNED_5,
                   TRKFILE.TK_INSEARNED_6,
                   TRKFILE.TK_INSEARNED_7,
                   TRKFILE.TK_INSEARNED_8,
                   TRKFILE.TK_INSEARNED_9,
                   TRKFILE.TK_INSEARNED_10,
                   TRKFILE.TK_DLNO,
                   TRKFILE.TK_DLRV,
                   TRKFILE.TK_DLSPRV,
                   TRKFILE.TK_DLDISC,
                   TRKFILE.TK_DLPARV,
                   ISNULL(TRKFILE.TK_DLRECOURSE,''),
                   TRKFILE.TK_DLPARTRCAMT,
                   TRKFILE.TK_PURBAL,
                   TRKFILE.TK_UNEARN_1,
                   TRKFILE.TK_UNEARN_2,
                   TRKFILE.TK_UNEARN_3,
                   TRKFILE.TK_UNEARN_4,
                   TRKFILE.TK_UNEARN_5,
                   TRKFILE.TK_UNEARN_6,
                   TRKFILE.TK_UNEARN_7,
                   TRKFILE.TK_UNEARN_8,
                   TRKFILE.TK_UNEARN_9,
                   TRKFILE.TK_UNEARN_10,
                   TRKFILE.TK_PRLNNO,
                   TRKFILE.TK_DLRVPRIN_1,
                   TRKFILE.TK_DLRVPRIN_2,
                   TRKFILE.TK_DLRVPRIN_3,
                   TRKFILE.TK_DLRVCHGS_1,
                   TRKFILE.TK_DLRVCHGS_2,
                   TRKFILE.TK_DLRVCHGS_3,
                   TRKFILE.TK_DLPARVPAID,
                   TRKFILE.TK_DLPARVHELD,
                   ISNULL(TRKFILE.TK_USERID,''),
                   ISNULL(CAST(TRKFILE.TK_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRKFILE.TK_POSTTIME,
                   TRKFILE.TK_EARNED_1,
                   TRKFILE.TK_EARNED_2,
                   TRKFILE.TK_EARNED_3,
                   TRKFILE.TK_EARNED_4,
                   TRKFILE.TK_EARNED_5,
                   TRKFILE.TK_EARNED_6,
                   TRKFILE.TK_EARNED_7,
                   TRKFILE.TK_EARNED_8,
                   TRKFILE.TK_EARNED_9,
                   TRKFILE.TK_EARNED_10
            FROM DBO.TRKFILE
            WHERE TRKFILE.TK_BRNO >= :QTRK1-WBEG-BRNO
              AND TRKFILE.TK_BRNO <= :QTRK1-WEND-BRNO
              AND CAST(TRKFILE.TK_DATE AS VARCHAR(10))
                          >= :QTRK1-WBEG-DATE
              AND CAST(TRKFILE.TK_DATE AS VARCHAR(10))
                          <= :QTRK1-WEND-DATE
              AND TRKFILE.TK_CLASS >= :QTRK1-WBEG-CLASS
              AND TRKFILE.TK_CLASS <= :QTRK1-WEND-CLASS
              AND TRKFILE.TK_ACCTNO >= :QTRK1-WBEG-ACCTNO
              AND TRKFILE.TK_ACCTNO <= :QTRK1-WEND-ACCTNO
              AND TRKFILE.TK_LAWCODE >= :QTRK1-WBEG-LAWCODE
              AND TRKFILE.TK_LAWCODE <= :QTRK1-WEND-LAWCODE
              AND TRKFILE.TK_LINE >= :QTRK1-WBEG-LINE
              AND TRKFILE.TK_LINE <= :QTRK1-WEND-LINE
            ORDER BY TRKFILE.TK_BRNO,
                     TRKFILE.TK_DATE,
                     TRKFILE.TK_CLASS,
                     TRKFILE.TK_ACCTNO,
                     TRKFILE.TK_LAWCODE,
                     TRKFILE.TK_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRK1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRK1-WBEG-BRNO
                          QTRK1-WEND-BRNO
                          QTRK1-WBEG-CLASS
                          QTRK1-WEND-CLASS
                          QTRK1-WBEG-ACCTNO
                          QTRK1-WEND-ACCTNO
                          QTRK1-WBEG-LAWCODE
                          QTRK1-WEND-LAWCODE
                          QTRK1-WBEG-LINE
                          QTRK1-WEND-LINE.
           MOVE SPACES TO QTRK1-WBEG-DATE
                          QTRK1-WEND-DATE.

           MOVE STAT---GOOD TO TRK1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRK1-FILE.
           PERFORM READ-IT.
           MOVE TRK-PATH-OWNBR  TO TK-BRNO. 
           MOVE TRK-SELECT-PATH TO E-FILE.
           MOVE TRK1-KEY        TO E-KEYX.

           MOVE TK-BRNO             TO QTK-BRNO.
           MOVE TK-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTK-DATE.
           MOVE TK-CLASS            TO QTK-CLASS.
           MOVE TK-ACCTNO           TO QTK-ACCTNO.
           MOVE TK-LAWCODE          TO QTK-LAWCODE.
           MOVE TK-LINE             TO QTK-LINE.


           EXEC SQL
            SELECT TRKFILE.TK_BRNO,
                   ISNULL(CAST(TRKFILE.TK_DATE AS VARCHAR(10)),'0'),
                   TRKFILE.TK_CLASS,
                   TRKFILE.TK_ACCTNO,
                   TRKFILE.TK_LAWCODE,
                   TRKFILE.TK_LINE,
                   ISNULL(CAST(TRKFILE.TK_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRKFILE.TK_SSNO_1,
                   ISNULL(TRKFILE.TK_MAKERCD_1,''),
                   TRKFILE.TK_SSNO_2,
                   ISNULL(TRKFILE.TK_MAKERCD_2,''),
                   TRKFILE.TK_SSNO_3,
                   ISNULL(TRKFILE.TK_MAKERCD_3,''),
                   TRKFILE.TK_SSNO_4,
                   ISNULL(TRKFILE.TK_MAKERCD_4,''),
                   ISNULL(TRKFILE.TK_REF_FRTO,''),
                   TRKFILE.TK_REFBR,
                   ISNULL(TRKFILE.TK_PURCD,''),
                   ISNULL(TRKFILE.TK_SRCD,''),
                   ISNULL(TRKFILE.TK_COLLCD,''),
                   ISNULL(TRKFILE.TK_SERVACCT,''),
                   ISNULL(TRKFILE.TK_LOANTYPE,''),
                   TRKFILE.TK_SUBCLASS,
                   ISNULL(TRKFILE.TK_REPOCD,''),
                   ISNULL(TRKFILE.TK_PLCD,''),
                   ISNULL(TRKFILE.TK_PLTYPE,''),
                   ISNULL(CAST(TRKFILE.TK_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   TRKFILE.TK_PROCEEDS,
                   TRKFILE.TK_LNAMT,
                   TRKFILE.TK_FINCHG,
                   TRKFILE.TK_INTCHG,
                   TRKFILE.TK_SERCHG,
                   TRKFILE.TK_EXTCHG,
                   TRKFILE.TK_MAINTFEE,
                   TRKFILE.TK_ORGTERM,
                   ISNULL(TRKFILE.TK_INSURED,''),
                   TRKFILE.TK_INSUNEARN_1,
                   TRKFILE.TK_INSUNEARN_2,
                   TRKFILE.TK_INSUNEARN_3,
                   TRKFILE.TK_INSUNEARN_4,
                   TRKFILE.TK_INSUNEARN_5,
                   TRKFILE.TK_INSUNEARN_6,
                   TRKFILE.TK_INSUNEARN_7,
                   TRKFILE.TK_INSUNEARN_8,
                   TRKFILE.TK_INSUNEARN_9,
                   TRKFILE.TK_INSUNEARN_10,
                   TRKFILE.TK_INSEARNED_1,
                   TRKFILE.TK_INSEARNED_2,
                   TRKFILE.TK_INSEARNED_3,
                   TRKFILE.TK_INSEARNED_4,
                   TRKFILE.TK_INSEARNED_5,
                   TRKFILE.TK_INSEARNED_6,
                   TRKFILE.TK_INSEARNED_7,
                   TRKFILE.TK_INSEARNED_8,
                   TRKFILE.TK_INSEARNED_9,
                   TRKFILE.TK_INSEARNED_10,
                   TRKFILE.TK_DLNO,
                   TRKFILE.TK_DLRV,
                   TRKFILE.TK_DLSPRV,
                   TRKFILE.TK_DLDISC,
                   TRKFILE.TK_DLPARV,
                   ISNULL(TRKFILE.TK_DLRECOURSE,''),
                   TRKFILE.TK_DLPARTRCAMT,
                   TRKFILE.TK_PURBAL,
                   TRKFILE.TK_UNEARN_1,
                   TRKFILE.TK_UNEARN_2,
                   TRKFILE.TK_UNEARN_3,
                   TRKFILE.TK_UNEARN_4,
                   TRKFILE.TK_UNEARN_5,
                   TRKFILE.TK_UNEARN_6,
                   TRKFILE.TK_UNEARN_7,
                   TRKFILE.TK_UNEARN_8,
                   TRKFILE.TK_UNEARN_9,
                   TRKFILE.TK_UNEARN_10,
                   TRKFILE.TK_PRLNNO,
                   TRKFILE.TK_DLRVPRIN_1,
                   TRKFILE.TK_DLRVPRIN_2,
                   TRKFILE.TK_DLRVPRIN_3,
                   TRKFILE.TK_DLRVCHGS_1,
                   TRKFILE.TK_DLRVCHGS_2,
                   TRKFILE.TK_DLRVCHGS_3,
                   TRKFILE.TK_DLPARVPAID,
                   TRKFILE.TK_DLPARVHELD,
                   ISNULL(TRKFILE.TK_USERID,''),
                   ISNULL(CAST(TRKFILE.TK_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRKFILE.TK_POSTTIME,
                   TRKFILE.TK_EARNED_1,
                   TRKFILE.TK_EARNED_2,
                   TRKFILE.TK_EARNED_3,
                   TRKFILE.TK_EARNED_4,
                   TRKFILE.TK_EARNED_5,
                   TRKFILE.TK_EARNED_6,
                   TRKFILE.TK_EARNED_7,
                   TRKFILE.TK_EARNED_8,
                   TRKFILE.TK_EARNED_9,
                   TRKFILE.TK_EARNED_10
            INTO :QTRK-REC
            FROM DBO.TRKFILE
            WHERE TRKFILE.TK_BRNO = :QTK-BRNO
              AND TRKFILE.TK_DATE = :QTK-DATE
              AND TRKFILE.TK_CLASS = :QTK-CLASS
              AND TRKFILE.TK_ACCTNO = :QTK-ACCTNO
              AND TRKFILE.TK_LAWCODE = :QTK-LAWCODE
              AND TRKFILE.TK_LINE = :QTK-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRK1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRK-FIELDS.

      *-----------------------------------------------------------------
       READ-TRK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRK-PATH-OWNBR  TO TK-BRNO.
           MOVE TRK-SELECT-PATH TO E-FILE.
           MOVE TRK1-KEY        TO E-KEYX.
           INITIALIZE QTRK-REC.

           IF ( TRK1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRK1_CURSOR INTO :QTRK-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRK1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRK-FIELDS.

      *-----------------------------------------------------------------
       WRITE-TRK1-FILE.

           MOVE TRK-PATH-OWNBR  TO TK-BRNO.
           ACCEPT TK-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TRK-SELECT-PATH TO E-FILE.
           MOVE TRK1-KEY        TO E-KEYX.
           PERFORM SET-TRK-FIELDS.

           EXEC SQL
            INSERT INTO DBO.TRKFILE
             (TRKFILE.TK_BRNO,
              TRKFILE.TK_DATE,
              TRKFILE.TK_CLASS,
              TRKFILE.TK_ACCTNO,
              TRKFILE.TK_LAWCODE,
              TRKFILE.TK_LINE,
              TRKFILE.TK_LTOUCH_DATE,
              TRKFILE.TK_SSNO_1,
              TRKFILE.TK_MAKERCD_1,
              TRKFILE.TK_SSNO_2,
              TRKFILE.TK_MAKERCD_2,
              TRKFILE.TK_SSNO_3,
              TRKFILE.TK_MAKERCD_3,
              TRKFILE.TK_SSNO_4,
              TRKFILE.TK_MAKERCD_4,
              TRKFILE.TK_REF_FRTO,
              TRKFILE.TK_REFBR,
              TRKFILE.TK_PURCD,
              TRKFILE.TK_SRCD,
              TRKFILE.TK_COLLCD,
              TRKFILE.TK_SERVACCT,
              TRKFILE.TK_LOANTYPE,
              TRKFILE.TK_SUBCLASS,
              TRKFILE.TK_REPOCD,
              TRKFILE.TK_PLCD,
              TRKFILE.TK_PLTYPE,
              TRKFILE.TK_LOANDATE,
              TRKFILE.TK_PROCEEDS,
              TRKFILE.TK_LNAMT,
              TRKFILE.TK_FINCHG,
              TRKFILE.TK_INTCHG,
              TRKFILE.TK_SERCHG,
              TRKFILE.TK_EXTCHG,
              TRKFILE.TK_MAINTFEE,
              TRKFILE.TK_ORGTERM,
              TRKFILE.TK_INSURED,
              TRKFILE.TK_INSUNEARN_1,
              TRKFILE.TK_INSUNEARN_2,
              TRKFILE.TK_INSUNEARN_3,
              TRKFILE.TK_INSUNEARN_4,
              TRKFILE.TK_INSUNEARN_5,
              TRKFILE.TK_INSUNEARN_6,
              TRKFILE.TK_INSUNEARN_7,
              TRKFILE.TK_INSUNEARN_8,
              TRKFILE.TK_INSUNEARN_9,
              TRKFILE.TK_INSUNEARN_10,
              TRKFILE.TK_INSEARNED_1,
              TRKFILE.TK_INSEARNED_2,
              TRKFILE.TK_INSEARNED_3,
              TRKFILE.TK_INSEARNED_4,
              TRKFILE.TK_INSEARNED_5,
              TRKFILE.TK_INSEARNED_6,
              TRKFILE.TK_INSEARNED_7,
              TRKFILE.TK_INSEARNED_8,
              TRKFILE.TK_INSEARNED_9,
              TRKFILE.TK_INSEARNED_10,
              TRKFILE.TK_DLNO,
              TRKFILE.TK_DLRV,
              TRKFILE.TK_DLSPRV,
              TRKFILE.TK_DLDISC,
              TRKFILE.TK_DLPARV,
              TRKFILE.TK_DLRECOURSE,
              TRKFILE.TK_DLPARTRCAMT,
              TRKFILE.TK_PURBAL,
              TRKFILE.TK_UNEARN_1,
              TRKFILE.TK_UNEARN_2,
              TRKFILE.TK_UNEARN_3,
              TRKFILE.TK_UNEARN_4,
              TRKFILE.TK_UNEARN_5,
              TRKFILE.TK_UNEARN_6,
              TRKFILE.TK_UNEARN_7,
              TRKFILE.TK_UNEARN_8,
              TRKFILE.TK_UNEARN_9,
              TRKFILE.TK_UNEARN_10,
              TRKFILE.TK_PRLNNO,
              TRKFILE.TK_DLRVPRIN_1,
              TRKFILE.TK_DLRVPRIN_2,
              TRKFILE.TK_DLRVPRIN_3,
              TRKFILE.TK_DLRVCHGS_1,
              TRKFILE.TK_DLRVCHGS_2,
              TRKFILE.TK_DLRVCHGS_3,
              TRKFILE.TK_DLPARVPAID,
              TRKFILE.TK_DLPARVHELD,
              TRKFILE.TK_USERID,
              TRKFILE.TK_POSTDATE,
              TRKFILE.TK_POSTTIME,
              TRKFILE.TK_EARNED_1,
              TRKFILE.TK_EARNED_2,
              TRKFILE.TK_EARNED_3,
              TRKFILE.TK_EARNED_4,
              TRKFILE.TK_EARNED_5,
              TRKFILE.TK_EARNED_6,
              TRKFILE.TK_EARNED_7,
              TRKFILE.TK_EARNED_8,
              TRKFILE.TK_EARNED_9,
              TRKFILE.TK_EARNED_10)
            VALUES
             (:QTK-BRNO,
              NULLIF(:QTK-DATE,'1900-01-01'),
              :QTK-CLASS,
              :QTK-ACCTNO,
              :QTK-LAWCODE,
              :QTK-LINE,
              NULLIF(:QTK-LTOUCH-DATE,'1900-01-01'),
              :QTK-SSNO-1,
              :QTK-MAKERCD-1,
              :QTK-SSNO-2,
              :QTK-MAKERCD-2,
              :QTK-SSNO-3,
              :QTK-MAKERCD-3,
              :QTK-SSNO-4,
              :QTK-MAKERCD-4,
              :QTK-REF-FRTO,
              :QTK-REFBR,
              :QTK-PURCD,
              :QTK-SRCD,
              :QTK-COLLCD,
              :QTK-SERVACCT,
              :QTK-LOANTYPE,
              :QTK-SUBCLASS,
              :QTK-REPOCD,
              :QTK-PLCD,
              :QTK-PLTYPE,
              NULLIF(:QTK-LOANDATE,'1900-01-01'),
              :QTK-PROCEEDS,
              :QTK-LNAMT,
              :QTK-FINCHG,
              :QTK-INTCHG,
              :QTK-SERCHG,
              :QTK-EXTCHG,
              :QTK-MAINTFEE,
              :QTK-ORGTERM,
              :QTK-INSURED,
              :QTK-INSUNEARN-1,
              :QTK-INSUNEARN-2,
              :QTK-INSUNEARN-3,
              :QTK-INSUNEARN-4,
              :QTK-INSUNEARN-5,
              :QTK-INSUNEARN-6,
              :QTK-INSUNEARN-7,
              :QTK-INSUNEARN-8,
              :QTK-INSUNEARN-9,
              :QTK-INSUNEARN-10,
              :QTK-INSEARNED-1,
              :QTK-INSEARNED-2,
              :QTK-INSEARNED-3,
              :QTK-INSEARNED-4,
              :QTK-INSEARNED-5,
              :QTK-INSEARNED-6,
              :QTK-INSEARNED-7,
              :QTK-INSEARNED-8,
              :QTK-INSEARNED-9,
              :QTK-INSEARNED-10,
              :QTK-DLNO,
              :QTK-DLRV,
              :QTK-DLSPRV,
              :QTK-DLDISC,
              :QTK-DLPARV,
              :QTK-DLRECOURSE,
              :QTK-DLPARTRCAMT,
              :QTK-PURBAL,
              :QTK-UNEARN-1,
              :QTK-UNEARN-2,
              :QTK-UNEARN-3,
              :QTK-UNEARN-4,
              :QTK-UNEARN-5,
              :QTK-UNEARN-6,
              :QTK-UNEARN-7,
              :QTK-UNEARN-8,
              :QTK-UNEARN-9,
              :QTK-UNEARN-10,
              :QTK-PRLNNO,
              :QTK-DLRVPRIN-1,
              :QTK-DLRVPRIN-2,
              :QTK-DLRVPRIN-3,
              :QTK-DLRVCHGS-1,
              :QTK-DLRVCHGS-2,
              :QTK-DLRVCHGS-3,
              :QTK-DLPARVPAID,
              :QTK-DLPARVHELD,
              :QTK-USERID,
              NULLIF(:QTK-POSTDATE,'1900-01-01'),
              :QTK-POSTTIME,
              :QTK-EARNED-1,
              :QTK-EARNED-2,
              :QTK-EARNED-3,
              :QTK-EARNED-4,
              :QTK-EARNED-5,
              :QTK-EARNED-6,
              :QTK-EARNED-7,
              :QTK-EARNED-8,
              :QTK-EARNED-9,
              :QTK-EARNED-10)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TRK1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-TRK1-FILE.

           MOVE TRK-PATH-OWNBR  TO TK-BRNO.
           ACCEPT TK-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TRK-SELECT-PATH TO E-FILE.
           MOVE TRK1-KEY        TO E-KEYX.
           PERFORM SET-TRK-FIELDS.

           EXEC SQL
            UPDATE DBO.TRKFILE
             SET
              TRKFILE.TK_BRNO = :QTK-BRNO,
              TRKFILE.TK_DATE =
                 NULLIF(:QTK-DATE,'1900-01-01'),
              TRKFILE.TK_CLASS = :QTK-CLASS,
              TRKFILE.TK_ACCTNO = :QTK-ACCTNO,
              TRKFILE.TK_LAWCODE = :QTK-LAWCODE,
              TRKFILE.TK_LINE = :QTK-LINE,
              TRKFILE.TK_LTOUCH_DATE =
                 NULLIF(:QTK-LTOUCH-DATE,'1900-01-01'),
              TRKFILE.TK_SSNO_1 = :QTK-SSNO-1,
              TRKFILE.TK_MAKERCD_1 = :QTK-MAKERCD-1,
              TRKFILE.TK_SSNO_2 = :QTK-SSNO-2,
              TRKFILE.TK_MAKERCD_2 = :QTK-MAKERCD-2,
              TRKFILE.TK_SSNO_3 = :QTK-SSNO-3,
              TRKFILE.TK_MAKERCD_3 = :QTK-MAKERCD-3,
              TRKFILE.TK_SSNO_4 = :QTK-SSNO-4,
              TRKFILE.TK_MAKERCD_4 = :QTK-MAKERCD-4,
              TRKFILE.TK_REF_FRTO = :QTK-REF-FRTO,
              TRKFILE.TK_REFBR = :QTK-REFBR,
              TRKFILE.TK_PURCD = :QTK-PURCD,
              TRKFILE.TK_SRCD = :QTK-SRCD,
              TRKFILE.TK_COLLCD = :QTK-COLLCD,
              TRKFILE.TK_SERVACCT = :QTK-SERVACCT,
              TRKFILE.TK_LOANTYPE = :QTK-LOANTYPE,
              TRKFILE.TK_SUBCLASS = :QTK-SUBCLASS,
              TRKFILE.TK_REPOCD = :QTK-REPOCD,
              TRKFILE.TK_PLCD = :QTK-PLCD,
              TRKFILE.TK_PLTYPE = :QTK-PLTYPE,
              TRKFILE.TK_LOANDATE =
                 NULLIF(:QTK-LOANDATE,'1900-01-01'),
              TRKFILE.TK_PROCEEDS = :QTK-PROCEEDS,
              TRKFILE.TK_LNAMT = :QTK-LNAMT,
              TRKFILE.TK_FINCHG = :QTK-FINCHG,
              TRKFILE.TK_INTCHG = :QTK-INTCHG,
              TRKFILE.TK_SERCHG = :QTK-SERCHG,
              TRKFILE.TK_EXTCHG = :QTK-EXTCHG,
              TRKFILE.TK_MAINTFEE = :QTK-MAINTFEE,
              TRKFILE.TK_ORGTERM = :QTK-ORGTERM,
              TRKFILE.TK_INSURED = :QTK-INSURED,
              TRKFILE.TK_INSUNEARN_1 = :QTK-INSUNEARN-1,
              TRKFILE.TK_INSUNEARN_2 = :QTK-INSUNEARN-2,
              TRKFILE.TK_INSUNEARN_3 = :QTK-INSUNEARN-3,
              TRKFILE.TK_INSUNEARN_4 = :QTK-INSUNEARN-4,
              TRKFILE.TK_INSUNEARN_5 = :QTK-INSUNEARN-5,
              TRKFILE.TK_INSUNEARN_6 = :QTK-INSUNEARN-6,
              TRKFILE.TK_INSUNEARN_7 = :QTK-INSUNEARN-7,
              TRKFILE.TK_INSUNEARN_8 = :QTK-INSUNEARN-8,
              TRKFILE.TK_INSUNEARN_9 = :QTK-INSUNEARN-9,
              TRKFILE.TK_INSUNEARN_10 = :QTK-INSUNEARN-10,
              TRKFILE.TK_INSEARNED_1 = :QTK-INSEARNED-1,
              TRKFILE.TK_INSEARNED_2 = :QTK-INSEARNED-2,
              TRKFILE.TK_INSEARNED_3 = :QTK-INSEARNED-3,
              TRKFILE.TK_INSEARNED_4 = :QTK-INSEARNED-4,
              TRKFILE.TK_INSEARNED_5 = :QTK-INSEARNED-5,
              TRKFILE.TK_INSEARNED_6 = :QTK-INSEARNED-6,
              TRKFILE.TK_INSEARNED_7 = :QTK-INSEARNED-7,
              TRKFILE.TK_INSEARNED_8 = :QTK-INSEARNED-8,
              TRKFILE.TK_INSEARNED_9 = :QTK-INSEARNED-9,
              TRKFILE.TK_INSEARNED_10 = :QTK-INSEARNED-10,
              TRKFILE.TK_DLNO = :QTK-DLNO,
              TRKFILE.TK_DLRV = :QTK-DLRV,
              TRKFILE.TK_DLSPRV = :QTK-DLSPRV,
              TRKFILE.TK_DLDISC = :QTK-DLDISC,
              TRKFILE.TK_DLPARV = :QTK-DLPARV,
              TRKFILE.TK_DLRECOURSE = :QTK-DLRECOURSE,
              TRKFILE.TK_DLPARTRCAMT = :QTK-DLPARTRCAMT,
              TRKFILE.TK_PURBAL = :QTK-PURBAL,
              TRKFILE.TK_UNEARN_1 = :QTK-UNEARN-1,
              TRKFILE.TK_UNEARN_2 = :QTK-UNEARN-2,
              TRKFILE.TK_UNEARN_3 = :QTK-UNEARN-3,
              TRKFILE.TK_UNEARN_4 = :QTK-UNEARN-4,
              TRKFILE.TK_UNEARN_5 = :QTK-UNEARN-5,
              TRKFILE.TK_UNEARN_6 = :QTK-UNEARN-6,
              TRKFILE.TK_UNEARN_7 = :QTK-UNEARN-7,
              TRKFILE.TK_UNEARN_8 = :QTK-UNEARN-8,
              TRKFILE.TK_UNEARN_9 = :QTK-UNEARN-9,
              TRKFILE.TK_UNEARN_10 = :QTK-UNEARN-10,
              TRKFILE.TK_PRLNNO = :QTK-PRLNNO,
              TRKFILE.TK_DLRVPRIN_1 = :QTK-DLRVPRIN-1,
              TRKFILE.TK_DLRVPRIN_2 = :QTK-DLRVPRIN-2,
              TRKFILE.TK_DLRVPRIN_3 = :QTK-DLRVPRIN-3,
              TRKFILE.TK_DLRVCHGS_1 = :QTK-DLRVCHGS-1,
              TRKFILE.TK_DLRVCHGS_2 = :QTK-DLRVCHGS-2,
              TRKFILE.TK_DLRVCHGS_3 = :QTK-DLRVCHGS-3,
              TRKFILE.TK_DLPARVPAID = :QTK-DLPARVPAID,
              TRKFILE.TK_DLPARVHELD = :QTK-DLPARVHELD,
              TRKFILE.TK_USERID = :QTK-USERID,
              TRKFILE.TK_POSTDATE =
                 NULLIF(:QTK-POSTDATE,'1900-01-01'),
              TRKFILE.TK_POSTTIME = :QTK-POSTTIME,
              TRKFILE.TK_EARNED_1 = :QTK-EARNED-1,
              TRKFILE.TK_EARNED_2 = :QTK-EARNED-2,
              TRKFILE.TK_EARNED_3 = :QTK-EARNED-3,
              TRKFILE.TK_EARNED_4 = :QTK-EARNED-4,
              TRKFILE.TK_EARNED_5 = :QTK-EARNED-5,
              TRKFILE.TK_EARNED_6 = :QTK-EARNED-6,
              TRKFILE.TK_EARNED_7 = :QTK-EARNED-7,
              TRKFILE.TK_EARNED_8 = :QTK-EARNED-8,
              TRKFILE.TK_EARNED_9 = :QTK-EARNED-9,
              TRKFILE.TK_EARNED_10 = :QTK-EARNED-10
            WHERE TRKFILE.TK_BRNO = :QTK-BRNO
              AND TRKFILE.TK_DATE = :QTK-DATE
              AND TRKFILE.TK_CLASS = :QTK-CLASS
              AND TRKFILE.TK_ACCTNO = :QTK-ACCTNO
              AND TRKFILE.TK_LAWCODE = :QTK-LAWCODE
              AND TRKFILE.TK_LINE = :QTK-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TRK1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-TRK1-FILE.

           MOVE TRK-PATH-OWNBR  TO TK-BRNO.
           PERFORM DELETE-IT.
           MOVE TRK-SELECT-PATH TO E-FILE.
           MOVE TRK1-KEY        TO E-KEYX.
           PERFORM SET-TRK-FIELDS.

           EXEC SQL
            DELETE FROM DBO.TRKFILE
            WHERE TRKFILE.TK_BRNO = :QTK-BRNO
              AND TRKFILE.TK_DATE = :QTK-DATE
              AND TRKFILE.TK_CLASS = :QTK-CLASS
              AND TRKFILE.TK_ACCTNO = :QTK-ACCTNO
              AND TRKFILE.TK_LAWCODE = :QTK-LAWCODE
              AND TRKFILE.TK_LINE = :QTK-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRK1-FILE.

      *-----------------------------------------------------------------
       CLOSE-TRK1-FILE.

           IF ( TRK1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRK1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRK1-CURSOR-STAT.

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
              DESTROY IOTRKI-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
