       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOTRJR.
       AUTHOR.        BAH.
       DATE-WRITTEN.  04-13-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOTRJR.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : TRJFILE 
      * TABLE DESC    : TRANSACTION CLASS TRANSFER FILE
      * OUTPUT FILE   : XPC/IOTRJR.PRE
      * USED FOR      : LIBLP/LPWSTRJ.CPY  --- QTRJ-REC
      *                 LIBLP/LPTR1JR.CPY  --- READ
      *                 LIBLP/LPTR1JRN.CPY --- READ/READ-NEXT/START
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)IO/IOTRJR.CBL S34 05/19/21-14:38:17 >".

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
       COPY "LIBLP/LP01TRJ.CPY".
       COPY "LIBLP/LPWSTRJ.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QTRJ1-WHERE-BEG-KEY.
           03  QTRJ1-WBEG-BRNO             PIC 9(04).
           03  QTRJ1-WBEG-DATE             PIC X(10).
           03  QTRJ1-WBEG-CLASS            PIC 9(03).
           03  QTRJ1-WBEG-ACCTNO           PIC 9(08).
           03  QTRJ1-WBEG-LAWCODE          PIC 9(02).
           03  QTRJ1-WBEG-LINE             PIC 9(03).

       01  QTRJ1-WHERE-END-KEY.
           03  QTRJ1-WEND-BRNO             PIC 9(04).
           03  QTRJ1-WEND-DATE             PIC X(10).
           03  QTRJ1-WEND-CLASS            PIC 9(03).
           03  QTRJ1-WEND-ACCTNO           PIC 9(08).
           03  QTRJ1-WEND-LAWCODE          PIC 9(02).
           03  QTRJ1-WEND-LINE             PIC 9(03).

       01  QTRJ2-WHERE-BEG-KEY.
           03  QTRJ2-WBEG-DATE             PIC X(10).
           03  QTRJ2-WBEG-BRNO             PIC 9(04).
           03  QTRJ2-WBEG-CLASS            PIC 9(03).
           03  QTRJ2-WBEG-ACCTNO           PIC 9(08).
           03  QTRJ2-WBEG-LAWCODE          PIC 9(02).
           03  QTRJ2-WBEG-LINE             PIC 9(03).

       01  QTRJ2-WHERE-END-KEY.
           03  QTRJ2-WEND-DATE             PIC X(10).
           03  QTRJ2-WEND-BRNO             PIC 9(04).
           03  QTRJ2-WEND-CLASS            PIC 9(03).
           03  QTRJ2-WEND-ACCTNO           PIC 9(08).
           03  QTRJ2-WEND-LAWCODE          PIC 9(02).
           03  QTRJ2-WEND-LINE             PIC 9(03).

       01  QTRJ-REC.
           03  QTJ-BRNO             PIC 9(4).
           03  QTJ-DATE             PIC X(10).
           03  QTJ-CLASS            PIC 9(3).
           03  QTJ-ACCTNO           PIC 9(8).
           03  QTJ-LAWCODE          PIC 99.
           03  QTJ-LINE             PIC 9(3).
           03  QTJ-LTOUCH-DATE      PIC X(10).
           03  QTJ-SSNO-1         PIC 9(9).
           03  QTJ-MAKERCD-1      PIC X.
           03  QTJ-SSNO-2         PIC 9(9).
           03  QTJ-MAKERCD-2      PIC X.
           03  QTJ-SSNO-3         PIC 9(9).
           03  QTJ-MAKERCD-3      PIC X.
           03  QTJ-SSNO-4         PIC 9(9).
           03  QTJ-MAKERCD-4      PIC X.
           03  QTJ-REF-FRLC     PIC 99.
           03  QTJ-REF-X        PIC X.
           03  QTJ-REF-TOLC     PIC 99.
           03  QTJ-FRLC         PIC 9(3).
           03  QTJ-TOLC         PIC 9(3).
           03  QTJ-PURCD            PIC XXX.
           03  QTJ-SRCD             PIC XX.
           03  QTJ-COLLCD           PIC XX.
           03  QTJ-SERVACCT         PIC X.
           03  QTJ-LOANTYPE         PIC X.
           03  QTJ-SUBCLASS         PIC 99.
           03  QTJ-LOANDATE         PIC X(10).
           03  QTJ-PROCEEDS         PIC S9(7)V99.
           03  QTJ-LNAMT            PIC S9(7)V99.
           03  QTJ-FINCHG           PIC S9(7)V99.
           03  QTJ-INTCHG           PIC 9(7)V99.
           03  QTJ-SERCHG           PIC 9(5)V99.
           03  QTJ-EXTCHG           PIC 9(5)V99.
           03  QTJ-MAINTFEE         PIC 9(5)V99.
           03  QTJ-ORGTERM          PIC 9(3).
           03  QTJ-INSURED          PIC X.
           03  QTJ-INSUNEARN-1      PIC S9(7)V99.
           03  QTJ-INSUNEARN-2      PIC S9(7)V99.
           03  QTJ-INSUNEARN-3      PIC S9(7)V99.
           03  QTJ-INSUNEARN-4      PIC S9(7)V99.
           03  QTJ-INSUNEARN-5      PIC S9(7)V99.
           03  QTJ-INSUNEARN-6      PIC S9(7)V99.
           03  QTJ-INSUNEARN-7      PIC S9(7)V99.
           03  QTJ-INSUNEARN-8      PIC S9(7)V99.
           03  QTJ-INSUNEARN-9      PIC S9(7)V99.
           03  QTJ-INSUNEARN-10     PIC S9(7)V99.
           03  QTJ-DLNO             PIC 9(4)V99.
           03  QTJ-DLRV             PIC S9(7)V99.
           03  QTJ-DLSPRV           PIC S9(7)V99.
           03  QTJ-DLDISC           PIC S9(7)V99.
           03  QTJ-DLPARV           PIC S9(7)V99.
           03  QTJ-DLRECOURSE       PIC X.
           03  QTJ-DLPARTRCAMT      PIC 9(6)V99.
           03  QTJ-PURBAL           PIC S9(7)V99.
           03  QTJ-UNEARN-1         PIC S9(7)V99.
           03  QTJ-UNEARN-2         PIC S9(7)V99.
           03  QTJ-UNEARN-3         PIC S9(7)V99.
           03  QTJ-UNEARN-4         PIC S9(7)V99.
           03  QTJ-UNEARN-5         PIC S9(7)V99.
           03  QTJ-UNEARN-6         PIC S9(7)V99.
           03  QTJ-UNEARN-7         PIC S9(7)V99.
           03  QTJ-UNEARN-8         PIC S9(7)V99.
           03  QTJ-UNEARN-9         PIC S9(7)V99.
           03  QTJ-UNEARN-10        PIC S9(7)V99.
           03  QTJ-PRLNNO           PIC 9(8).
           03  QTJ-DLRVPRIN-1       PIC S9(7)V99.
           03  QTJ-DLRVPRIN-2       PIC S9(7)V99.
           03  QTJ-DLRVPRIN-3       PIC S9(7)V99.
           03  QTJ-DLRVCHGS-1       PIC S9(7)V99.
           03  QTJ-DLRVCHGS-2       PIC S9(7)V99.
           03  QTJ-DLRVCHGS-3       PIC S9(7)V99.
           03  QTJ-DLPARVPAID       PIC S9(7)V99.
           03  QTJ-DLPARVHELD       PIC S9(7)V99.
           03  QTJ-USERID           PIC X(10).
           03  QTJ-POSTDATE         PIC X(10).
           03  QTJ-POSTTIME         PIC 9(6).
           03  QTJ-PLCD             PIC X.
           03  QTJ-PLTYPE           PIC X.
           03  QTJ-REPOCD           PIC X.
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
           03  D01-TJ-BRNO                   PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-TJ-DATE                   PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-TJ-CLASS                  PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-TJ-ACCTNO                 PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-TJ-LAWCODE                PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-TJ-LINE                   PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-TJ-SSNO1                  PIC 9(09).
           03  FILLER                         PIC X(03).
           03  D01-TJ-LOANDATE               PIC 9999/99/99.


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
       COPY "LIBIO/IOTRJR_DEF.CPY".
       COPY "LIBIO/IOTRJR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOTRJR_SCN.CPY".

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
           MOVE "IOTRJR"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOTRJR-QUEUE-POS  TO Q-POS.
           MOVE IOTRJR-COPIES-POS TO C-POS.
           MOVE IOTRJR-PRU-POS    TO P-POS.
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

           MOVE "B"                        TO TRJ-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QTRJ1-WBEG-BRNO
                              QTRJ2-WBEG-BRNO.
           MOVE BR-NO     TO QTRJ1-WEND-BRNO
                              QTRJ2-WEND-BRNO.

           MOVE BEG-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRJ1-WBEG-DATE
                                        QTRJ2-WBEG-DATE.
           MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRJ1-WEND-DATE
                                        QTRJ2-WEND-DATE.

           MOVE BEG-CLASS  TO QTRJ1-WBEG-CLASS
                              QTRJ2-WBEG-CLASS.
           MOVE END-CLASS  TO QTRJ1-WEND-CLASS
                              QTRJ2-WEND-CLASS.

           MOVE BEG-ACCTNO TO QTRJ1-WBEG-ACCTNO
                              QTRJ2-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QTRJ1-WEND-ACCTNO
                              QTRJ2-WEND-ACCTNO.
           MOVE BEG-LAWCODE TO QTRJ1-WBEG-LAWCODE
                               QTRJ2-WBEG-LAWCODE.
           MOVE END-LAWCODE TO QTRJ1-WEND-LAWCODE
                               QTRJ2-WEND-LAWCODE.
           MOVE BEG-LINE TO QTRJ1-WBEG-LINE
                            QTRJ2-WBEG-LINE.
           MOVE END-LINE TO QTRJ1-WEND-LINE
                            QTRJ2-WEND-LINE.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF PROCESS-KEY = 1
              PERFORM OPEN-TRJ1-FILE
              PERFORM START-TRJ1-FILE
           ELSE
              PERFORM OPEN-TRJ2-FILE
              PERFORM START-TRJ2-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           IF PROCESS-KEY = 1
              PERFORM READ-TRJ1-FILE-NEXT
           ELSE
              PERFORM READ-TRJ2-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( TJ-BRNO < BEG-BRANCH )
                  GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( TJ-BRNO > END-BRANCH )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF TJ-DATE < BEG-DATE OR > END-DATE
                    GO TO PROCESS-BRANCH-NEXT.
           IF ( PROCESS-KEY = 2 )
              IF ( TJ-DATE < BEG-DATE )
                  GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( TJ-DATE > END-DATE )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF TJ-BRNO < BEG-BRANCH OR > END-BRANCH
                    GO TO PROCESS-BRANCH-NEXT.

           IF TJ-CLASS < BEG-CLASS OR > END-CLASS
              GO TO PROCESS-BRANCH-NEXT.
           IF TJ-ACCTNO < BEG-ACCTNO OR > END-ACCTNO
              GO TO PROCESS-BRANCH-NEXT.
           IF TJ-LAWCODE < BEG-LAWCODE OR > END-LAWCODE
              GO TO PROCESS-BRANCH-NEXT.
           IF TJ-LINE < BEG-LINE OR > END-LINE
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE TJ-BRNO                    TO D01-TJ-BRNO.
           MOVE TJ-DATE                    TO D01-TJ-DATE.
           MOVE TJ-CLASS                   TO D01-TJ-CLASS.
           MOVE TJ-ACCTNO                  TO D01-TJ-ACCTNO.
           MOVE TJ-LAWCODE                 TO D01-TJ-LAWCODE.
           MOVE TJ-LINE                    TO D01-TJ-LINE.
           MOVE TJ-SSNO(1)                 TO D01-TJ-SSNO1.
           MOVE TJ-LOANDATE                TO D01-TJ-LOANDATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           IF PROCESS-KEY = 1
              PERFORM CLOSE-TRJ1-FILE
           ELSE
              PERFORM CLOSE-TRJ2-FILE.

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
              MOVE TRJ1-KEY TO PACE-DETAIL-KEY
           ELSE
              STRING TJ-DATE, TJ-BRNO, TJ-CLASS, TJ-ACCTNO, TJ-LAWCODE,
                     TJ-LINE DELIMITED BY SIZE INTO PACE-DETAIL-KEY.

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
           MOVE "SQL TABLE IO UTILITY - TRJFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOTRJR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOTRJR-SYSDATE.
           DISPLAY IOTRJR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOTRJR-FORM-FIELDS TO WR-BUF.
           MOVE IOTRJR-FORM-LIST TO WR-LIST.
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

           MOVE IOTRJR-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOTRJR-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOTRJR_EVA.CPY".
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
      *    LOAD TRJ-REC FROM QTRJ-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-TRJ-FIELDS.
           MOVE QTJ-BRNO TO TJ-BRNO.

           IF ( QTJ-DATE = "1900-01-01" )
              MOVE ZEROES TO TJ-DATE
           ELSE
              MOVE QTJ-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TJ-DATE.

           MOVE QTJ-CLASS TO TJ-CLASS.
           MOVE QTJ-ACCTNO TO TJ-ACCTNO.
           MOVE QTJ-LAWCODE TO TJ-LAWCODE.
           MOVE QTJ-LINE TO TJ-LINE.

           IF ( QTJ-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO TJ-LTOUCH-DATE
           ELSE
              MOVE QTJ-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TJ-LTOUCH-DATE.

           MOVE QTJ-SSNO-1 TO TJ-SSNO(1).
           MOVE QTJ-MAKERCD-1 TO TJ-MAKERCD(1).
           MOVE QTJ-SSNO-2 TO TJ-SSNO(2).
           MOVE QTJ-MAKERCD-2 TO TJ-MAKERCD(2).
           MOVE QTJ-SSNO-3 TO TJ-SSNO(3).
           MOVE QTJ-MAKERCD-3 TO TJ-MAKERCD(3).
           MOVE QTJ-SSNO-4 TO TJ-SSNO(4).
           MOVE QTJ-MAKERCD-4 TO TJ-MAKERCD(4).
           MOVE QTJ-REF-FRLC TO TJ-REF-FRLC.
           MOVE QTJ-REF-X TO TJ-REF-X.
           MOVE QTJ-REF-TOLC TO TJ-REF-TOLC.
           MOVE QTJ-FRLC TO TJ-FRLC.
           MOVE QTJ-TOLC TO TJ-TOLC.
           MOVE QTJ-PURCD TO TJ-PURCD.
           MOVE QTJ-SRCD TO TJ-SRCD.
           MOVE QTJ-COLLCD TO TJ-COLLCD.
           MOVE QTJ-SERVACCT TO TJ-SERVACCT.
           MOVE QTJ-LOANTYPE TO TJ-LOANTYPE.
           MOVE QTJ-SUBCLASS TO TJ-SUBCLASS.

           IF ( QTJ-LOANDATE = "1900-01-01" )
              MOVE ZEROES TO TJ-LOANDATE
           ELSE
              MOVE QTJ-LOANDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TJ-LOANDATE.

           MOVE QTJ-PROCEEDS TO TJ-PROCEEDS.
           MOVE QTJ-LNAMT TO TJ-LNAMT.
           MOVE QTJ-FINCHG TO TJ-FINCHG.
           MOVE QTJ-INTCHG TO TJ-INTCHG.
           MOVE QTJ-SERCHG TO TJ-SERCHG.
           MOVE QTJ-EXTCHG TO TJ-EXTCHG.
           MOVE QTJ-MAINTFEE TO TJ-MAINTFEE.
           MOVE QTJ-ORGTERM TO TJ-ORGTERM.
           MOVE QTJ-INSURED TO TJ-INSURED.
           MOVE QTJ-INSUNEARN-1 TO TJ-INSUNEARN(1).
           MOVE QTJ-INSUNEARN-2 TO TJ-INSUNEARN(2).
           MOVE QTJ-INSUNEARN-3 TO TJ-INSUNEARN(3).
           MOVE QTJ-INSUNEARN-4 TO TJ-INSUNEARN(4).
           MOVE QTJ-INSUNEARN-5 TO TJ-INSUNEARN(5).
           MOVE QTJ-INSUNEARN-6 TO TJ-INSUNEARN(6).
           MOVE QTJ-INSUNEARN-7 TO TJ-INSUNEARN(7).
           MOVE QTJ-INSUNEARN-8 TO TJ-INSUNEARN(8).
           MOVE QTJ-INSUNEARN-9 TO TJ-INSUNEARN(9).
           MOVE QTJ-INSUNEARN-10 TO TJ-INSUNEARN(10).
           MOVE QTJ-DLNO TO TJ-DLNO.
           MOVE QTJ-DLRV TO TJ-DLRV.
           MOVE QTJ-DLSPRV TO TJ-DLSPRV.
           MOVE QTJ-DLDISC TO TJ-DLDISC.
           MOVE QTJ-DLPARV TO TJ-DLPARV.
           MOVE QTJ-DLRECOURSE TO TJ-DLRECOURSE.
           MOVE QTJ-DLPARTRCAMT TO TJ-DLPARTRCAMT.
           MOVE QTJ-PURBAL TO TJ-PURBAL.
           MOVE QTJ-UNEARN-1 TO TJ-UNEARN(1).
           MOVE QTJ-UNEARN-2 TO TJ-UNEARN(2).
           MOVE QTJ-UNEARN-3 TO TJ-UNEARN(3).
           MOVE QTJ-UNEARN-4 TO TJ-UNEARN(4).
           MOVE QTJ-UNEARN-5 TO TJ-UNEARN(5).
           MOVE QTJ-UNEARN-6 TO TJ-UNEARN(6).
           MOVE QTJ-UNEARN-7 TO TJ-UNEARN(7).
           MOVE QTJ-UNEARN-8 TO TJ-UNEARN(8).
           MOVE QTJ-UNEARN-9 TO TJ-UNEARN(9).
           MOVE QTJ-UNEARN-10 TO TJ-UNEARN(10).
           MOVE QTJ-PRLNNO TO TJ-PRLNNO.
           MOVE QTJ-DLRVPRIN-1 TO TJ-DLRVPRIN(1).
           MOVE QTJ-DLRVPRIN-2 TO TJ-DLRVPRIN(2).
           MOVE QTJ-DLRVPRIN-3 TO TJ-DLRVPRIN(3).
           MOVE QTJ-DLRVCHGS-1 TO TJ-DLRVCHGS(1).
           MOVE QTJ-DLRVCHGS-2 TO TJ-DLRVCHGS(2).
           MOVE QTJ-DLRVCHGS-3 TO TJ-DLRVCHGS(3).
           MOVE QTJ-DLPARVPAID TO TJ-DLPARVPAID.
           MOVE QTJ-DLPARVHELD TO TJ-DLPARVHELD.
           MOVE QTJ-USERID TO TJ-USERID.

           IF ( QTJ-POSTDATE = "1900-01-01" )
              MOVE ZEROES TO TJ-POSTDATE
           ELSE
              MOVE QTJ-POSTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TJ-POSTDATE.

           MOVE QTJ-POSTTIME TO TJ-POSTTIME.
           MOVE QTJ-PLCD TO TJ-PLCD.
           MOVE QTJ-PLTYPE TO TJ-PLTYPE.
           MOVE QTJ-REPOCD TO TJ-REPOCD.
 
      *-----------------------------------------------------------------
      *    LOAD QTRJ-REC FROM TRJ-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-TRJ-FIELDS.
           MOVE TJ-BRNO TO QTJ-BRNO.

           IF ( TJ-DATE = ZEROES )
              MOVE "1900-01-01" TO QTJ-DATE
           ELSE
              MOVE TJ-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTJ-DATE.

           MOVE TJ-CLASS TO QTJ-CLASS.
           MOVE TJ-ACCTNO TO QTJ-ACCTNO.
           MOVE TJ-LAWCODE TO QTJ-LAWCODE.
           MOVE TJ-LINE TO QTJ-LINE.

           IF ( TJ-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTJ-LTOUCH-DATE
           ELSE
              MOVE TJ-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTJ-LTOUCH-DATE.

           MOVE TJ-SSNO(1) TO QTJ-SSNO-1.
           MOVE TJ-MAKERCD(1) TO QTJ-MAKERCD-1.
           MOVE TJ-SSNO(2) TO QTJ-SSNO-2.
           MOVE TJ-MAKERCD(2) TO QTJ-MAKERCD-2.
           MOVE TJ-SSNO(3) TO QTJ-SSNO-3.
           MOVE TJ-MAKERCD(3) TO QTJ-MAKERCD-3.
           MOVE TJ-SSNO(4) TO QTJ-SSNO-4.
           MOVE TJ-MAKERCD(4) TO QTJ-MAKERCD-4.
           MOVE TJ-REF-FRLC TO QTJ-REF-FRLC.
           MOVE TJ-REF-X TO QTJ-REF-X.
           MOVE TJ-REF-TOLC TO QTJ-REF-TOLC.
           MOVE TJ-FRLC TO QTJ-FRLC.
           MOVE TJ-TOLC TO QTJ-TOLC.
           MOVE TJ-PURCD TO QTJ-PURCD.
           MOVE TJ-SRCD TO QTJ-SRCD.
           MOVE TJ-COLLCD TO QTJ-COLLCD.
           MOVE TJ-SERVACCT TO QTJ-SERVACCT.
           MOVE TJ-LOANTYPE TO QTJ-LOANTYPE.
           MOVE TJ-SUBCLASS TO QTJ-SUBCLASS.

           IF ( TJ-LOANDATE = ZEROES )
              MOVE "1900-01-01" TO QTJ-LOANDATE
           ELSE
              MOVE TJ-LOANDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTJ-LOANDATE.

           MOVE TJ-PROCEEDS TO QTJ-PROCEEDS.
           MOVE TJ-LNAMT TO QTJ-LNAMT.
           MOVE TJ-FINCHG TO QTJ-FINCHG.
           MOVE TJ-INTCHG TO QTJ-INTCHG.
           MOVE TJ-SERCHG TO QTJ-SERCHG.
           MOVE TJ-EXTCHG TO QTJ-EXTCHG.
           MOVE TJ-MAINTFEE TO QTJ-MAINTFEE.
           MOVE TJ-ORGTERM TO QTJ-ORGTERM.
           MOVE TJ-INSURED TO QTJ-INSURED.
           MOVE TJ-INSUNEARN(1) TO QTJ-INSUNEARN-1.
           MOVE TJ-INSUNEARN(2) TO QTJ-INSUNEARN-2.
           MOVE TJ-INSUNEARN(3) TO QTJ-INSUNEARN-3.
           MOVE TJ-INSUNEARN(4) TO QTJ-INSUNEARN-4.
           MOVE TJ-INSUNEARN(5) TO QTJ-INSUNEARN-5.
           MOVE TJ-INSUNEARN(6) TO QTJ-INSUNEARN-6.
           MOVE TJ-INSUNEARN(7) TO QTJ-INSUNEARN-7.
           MOVE TJ-INSUNEARN(8) TO QTJ-INSUNEARN-8.
           MOVE TJ-INSUNEARN(9) TO QTJ-INSUNEARN-9.
           MOVE TJ-INSUNEARN(10) TO QTJ-INSUNEARN-10.
           MOVE TJ-DLNO TO QTJ-DLNO.
           MOVE TJ-DLRV TO QTJ-DLRV.
           MOVE TJ-DLSPRV TO QTJ-DLSPRV.
           MOVE TJ-DLDISC TO QTJ-DLDISC.
           MOVE TJ-DLPARV TO QTJ-DLPARV.
           MOVE TJ-DLRECOURSE TO QTJ-DLRECOURSE.
           MOVE TJ-DLPARTRCAMT TO QTJ-DLPARTRCAMT.
           MOVE TJ-PURBAL TO QTJ-PURBAL.
           MOVE TJ-UNEARN(1) TO QTJ-UNEARN-1.
           MOVE TJ-UNEARN(2) TO QTJ-UNEARN-2.
           MOVE TJ-UNEARN(3) TO QTJ-UNEARN-3.
           MOVE TJ-UNEARN(4) TO QTJ-UNEARN-4.
           MOVE TJ-UNEARN(5) TO QTJ-UNEARN-5.
           MOVE TJ-UNEARN(6) TO QTJ-UNEARN-6.
           MOVE TJ-UNEARN(7) TO QTJ-UNEARN-7.
           MOVE TJ-UNEARN(8) TO QTJ-UNEARN-8.
           MOVE TJ-UNEARN(9) TO QTJ-UNEARN-9.
           MOVE TJ-UNEARN(10) TO QTJ-UNEARN-10.
           MOVE TJ-PRLNNO TO QTJ-PRLNNO.
           MOVE TJ-DLRVPRIN(1) TO QTJ-DLRVPRIN-1.
           MOVE TJ-DLRVPRIN(2) TO QTJ-DLRVPRIN-2.
           MOVE TJ-DLRVPRIN(3) TO QTJ-DLRVPRIN-3.
           MOVE TJ-DLRVCHGS(1) TO QTJ-DLRVCHGS-1.
           MOVE TJ-DLRVCHGS(2) TO QTJ-DLRVCHGS-2.
           MOVE TJ-DLRVCHGS(3) TO QTJ-DLRVCHGS-3.
           MOVE TJ-DLPARVPAID TO QTJ-DLPARVPAID.
           MOVE TJ-DLPARVHELD TO QTJ-DLPARVHELD.
           MOVE TJ-USERID TO QTJ-USERID.

           IF ( TJ-POSTDATE = ZEROES )
              MOVE "1900-01-01" TO QTJ-POSTDATE
           ELSE
              MOVE TJ-POSTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTJ-POSTDATE.

           MOVE TJ-POSTTIME TO QTJ-POSTTIME.
           MOVE TJ-PLCD TO QTJ-PLCD.
           MOVE TJ-PLTYPE TO QTJ-PLTYPE.
           MOVE TJ-REPOCD TO QTJ-REPOCD.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-TRJ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRJ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRJ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRJ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRJ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRJ-ALL-DATA
              MOVE TRJ-ALL-PATH             TO TRJ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRJ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRJ-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRJ-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRJ-ALL-DATA
              MOVE TRJ-ALL-PATH             TO TRJ-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRJFILE NOT = "S" )
              MOVE TRJ-PATH-BEG             TO TRJ-SELECT-PATH-BEG
              MOVE TRJ-PATH-FILE            TO TRJ-SELECT-PATH-FILE
           ELSE
              MOVE TRJ-PATH-SQL             TO TRJ-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRJ1-FILE.
           PERFORM LOAD-TRJ1-FILE.
           PERFORM OPEN-IT.
           MOVE TRJ-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRJ1-FILE.
           PERFORM START-IT.
           MOVE TRJ-PATH-OWNBR  TO TJ-BRNO.
           MOVE TRJ-SELECT-PATH TO E-FILE.
           MOVE TRJ1-KEY        TO E-KEYX.

           IF ( TRJ1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRJ1-FILE.

           IF ( QTRJ1-WEND-LINE = ZEROES    ) OR
              ( QTRJ1-WEND-LINE NOT NUMERIC )
              MOVE TRJ-PATH-OWNBR        TO QTRJ1-WBEG-BRNO
              MOVE TRJ-PATH-OWNBR        TO QTRJ1-WEND-BRNO
              MOVE TJ-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRJ1-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRJ1-WEND-DATE
              MOVE TJ-CLASS             TO QTRJ1-WBEG-CLASS
              MOVE TJ-ACCTNO            TO QTRJ1-WBEG-ACCTNO
              MOVE TJ-LAWCODE           TO QTRJ1-WBEG-LAWCODE
              MOVE TJ-LINE              TO QTRJ1-WBEG-LINE
              MOVE ALL "9"              TO QTRJ1-WEND-CLASS
                                           QTRJ1-WEND-ACCTNO
                                           QTRJ1-WEND-LAWCODE
                                           QTRJ1-WEND-LINE.


           EXEC SQL
            DECLARE TRJ1_CURSOR CURSOR FOR
            SELECT TRJFILE.TJ_BRNO,
                   ISNULL(CAST(TRJFILE.TJ_DATE AS VARCHAR(10)),'0'),
                   TRJFILE.TJ_CLASS,
                   TRJFILE.TJ_ACCTNO,
                   TRJFILE.TJ_LAWCODE,
                   TRJFILE.TJ_LINE,
                   ISNULL(CAST(TRJFILE.TJ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRJFILE.TJ_SSNO_1,
                   ISNULL(TRJFILE.TJ_MAKERCD_1,''),
                   TRJFILE.TJ_SSNO_2,
                   ISNULL(TRJFILE.TJ_MAKERCD_2,''),
                   TRJFILE.TJ_SSNO_3,
                   ISNULL(TRJFILE.TJ_MAKERCD_3,''),
                   TRJFILE.TJ_SSNO_4,
                   ISNULL(TRJFILE.TJ_MAKERCD_4,''),
                   TRJFILE.TJ_REF_FRLC,
                   ISNULL(TRJFILE.TJ_REF_X,''),
                   TRJFILE.TJ_REF_TOLC,
                   TRJFILE.TJ_FRLC,
                   TRJFILE.TJ_TOLC,
                   ISNULL(TRJFILE.TJ_PURCD,''),
                   ISNULL(TRJFILE.TJ_SRCD,''),
                   ISNULL(TRJFILE.TJ_COLLCD,''),
                   ISNULL(TRJFILE.TJ_SERVACCT,''),
                   ISNULL(TRJFILE.TJ_LOANTYPE,''),
                   TRJFILE.TJ_SUBCLASS,
                   ISNULL(CAST(TRJFILE.TJ_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   TRJFILE.TJ_PROCEEDS,
                   TRJFILE.TJ_LNAMT,
                   TRJFILE.TJ_FINCHG,
                   TRJFILE.TJ_INTCHG,
                   TRJFILE.TJ_SERCHG,
                   TRJFILE.TJ_EXTCHG,
                   TRJFILE.TJ_MAINTFEE,
                   TRJFILE.TJ_ORGTERM,
                   ISNULL(TRJFILE.TJ_INSURED,''),
                   TRJFILE.TJ_INSUNEARN_1,
                   TRJFILE.TJ_INSUNEARN_2,
                   TRJFILE.TJ_INSUNEARN_3,
                   TRJFILE.TJ_INSUNEARN_4,
                   TRJFILE.TJ_INSUNEARN_5,
                   TRJFILE.TJ_INSUNEARN_6,
                   TRJFILE.TJ_INSUNEARN_7,
                   TRJFILE.TJ_INSUNEARN_8,
                   TRJFILE.TJ_INSUNEARN_9,
                   TRJFILE.TJ_INSUNEARN_10,
                   TRJFILE.TJ_DLNO,
                   TRJFILE.TJ_DLRV,
                   TRJFILE.TJ_DLSPRV,
                   TRJFILE.TJ_DLDISC,
                   TRJFILE.TJ_DLPARV,
                   ISNULL(TRJFILE.TJ_DLRECOURSE,''),
                   TRJFILE.TJ_DLPARTRCAMT,
                   TRJFILE.TJ_PURBAL,
                   TRJFILE.TJ_UNEARN_1,
                   TRJFILE.TJ_UNEARN_2,
                   TRJFILE.TJ_UNEARN_3,
                   TRJFILE.TJ_UNEARN_4,
                   TRJFILE.TJ_UNEARN_5,
                   TRJFILE.TJ_UNEARN_6,
                   TRJFILE.TJ_UNEARN_7,
                   TRJFILE.TJ_UNEARN_8,
                   TRJFILE.TJ_UNEARN_9,
                   TRJFILE.TJ_UNEARN_10,
                   TRJFILE.TJ_PRLNNO,
                   TRJFILE.TJ_DLRVPRIN_1,
                   TRJFILE.TJ_DLRVPRIN_2,
                   TRJFILE.TJ_DLRVPRIN_3,
                   TRJFILE.TJ_DLRVCHGS_1,
                   TRJFILE.TJ_DLRVCHGS_2,
                   TRJFILE.TJ_DLRVCHGS_3,
                   TRJFILE.TJ_DLPARVPAID,
                   TRJFILE.TJ_DLPARVHELD,
                   ISNULL(TRJFILE.TJ_USERID,''),
                   ISNULL(CAST(TRJFILE.TJ_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRJFILE.TJ_POSTTIME,
                   ISNULL(TRJFILE.TJ_PLCD,''),
                   ISNULL(TRJFILE.TJ_PLTYPE,''),
                   ISNULL(TRJFILE.TJ_REPOCD,'')
            FROM DBO.TRJFILE
            WHERE TRJFILE.TJ_BRNO >= :QTRJ1-WBEG-BRNO
              AND TRJFILE.TJ_BRNO <= :QTRJ1-WEND-BRNO
              AND CAST(TRJFILE.TJ_DATE AS VARCHAR(10))
                          >= :QTRJ1-WBEG-DATE
              AND CAST(TRJFILE.TJ_DATE AS VARCHAR(10))
                          <= :QTRJ1-WEND-DATE
              AND TRJFILE.TJ_CLASS >= :QTRJ1-WBEG-CLASS
              AND TRJFILE.TJ_CLASS <= :QTRJ1-WEND-CLASS
              AND TRJFILE.TJ_ACCTNO >= :QTRJ1-WBEG-ACCTNO
              AND TRJFILE.TJ_ACCTNO <= :QTRJ1-WEND-ACCTNO
              AND TRJFILE.TJ_LAWCODE >= :QTRJ1-WBEG-LAWCODE
              AND TRJFILE.TJ_LAWCODE <= :QTRJ1-WEND-LAWCODE
              AND TRJFILE.TJ_LINE >= :QTRJ1-WBEG-LINE
              AND TRJFILE.TJ_LINE <= :QTRJ1-WEND-LINE
            ORDER BY TRJFILE.TJ_BRNO,
                     TRJFILE.TJ_DATE,
                     TRJFILE.TJ_CLASS,
                     TRJFILE.TJ_ACCTNO,
                     TRJFILE.TJ_LAWCODE,
                     TRJFILE.TJ_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRJ1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRJ1-WBEG-BRNO
                          QTRJ1-WEND-BRNO
                          QTRJ1-WBEG-CLASS
                          QTRJ1-WEND-CLASS
                          QTRJ1-WBEG-ACCTNO
                          QTRJ1-WEND-ACCTNO
                          QTRJ1-WBEG-LAWCODE
                          QTRJ1-WEND-LAWCODE
                          QTRJ1-WBEG-LINE
                          QTRJ1-WEND-LINE.
           MOVE SPACES TO QTRJ1-WBEG-DATE
                          QTRJ1-WEND-DATE.

           MOVE STAT---GOOD TO TRJ1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRJ1-FILE.
           PERFORM READ-IT.
           MOVE TRJ-PATH-OWNBR  TO TJ-BRNO. 
           MOVE TRJ-SELECT-PATH TO E-FILE.
           MOVE TRJ1-KEY        TO E-KEYX.

           MOVE TJ-BRNO             TO QTJ-BRNO.
           MOVE TJ-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTJ-DATE.
           MOVE TJ-CLASS            TO QTJ-CLASS.
           MOVE TJ-ACCTNO           TO QTJ-ACCTNO.
           MOVE TJ-LAWCODE          TO QTJ-LAWCODE.
           MOVE TJ-LINE             TO QTJ-LINE.

           EXEC SQL
            SELECT TRJFILE.TJ_BRNO,
                   ISNULL(CAST(TRJFILE.TJ_DATE AS VARCHAR(10)),'0'),
                   TRJFILE.TJ_CLASS,
                   TRJFILE.TJ_ACCTNO,
                   TRJFILE.TJ_LAWCODE,
                   TRJFILE.TJ_LINE,
                   ISNULL(CAST(TRJFILE.TJ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRJFILE.TJ_SSNO_1,
                   ISNULL(TRJFILE.TJ_MAKERCD_1,''),
                   TRJFILE.TJ_SSNO_2,
                   ISNULL(TRJFILE.TJ_MAKERCD_2,''),
                   TRJFILE.TJ_SSNO_3,
                   ISNULL(TRJFILE.TJ_MAKERCD_3,''),
                   TRJFILE.TJ_SSNO_4,
                   ISNULL(TRJFILE.TJ_MAKERCD_4,''),
                   TRJFILE.TJ_REF_FRLC,
                   ISNULL(TRJFILE.TJ_REF_X,''),
                   TRJFILE.TJ_REF_TOLC,
                   TRJFILE.TJ_FRLC,
                   TRJFILE.TJ_TOLC,
                   ISNULL(TRJFILE.TJ_PURCD,''),
                   ISNULL(TRJFILE.TJ_SRCD,''),
                   ISNULL(TRJFILE.TJ_COLLCD,''),
                   ISNULL(TRJFILE.TJ_SERVACCT,''),
                   ISNULL(TRJFILE.TJ_LOANTYPE,''),
                   TRJFILE.TJ_SUBCLASS,
                   ISNULL(CAST(TRJFILE.TJ_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   TRJFILE.TJ_PROCEEDS,
                   TRJFILE.TJ_LNAMT,
                   TRJFILE.TJ_FINCHG,
                   TRJFILE.TJ_INTCHG,
                   TRJFILE.TJ_SERCHG,
                   TRJFILE.TJ_EXTCHG,
                   TRJFILE.TJ_MAINTFEE,
                   TRJFILE.TJ_ORGTERM,
                   ISNULL(TRJFILE.TJ_INSURED,''),
                   TRJFILE.TJ_INSUNEARN_1,
                   TRJFILE.TJ_INSUNEARN_2,
                   TRJFILE.TJ_INSUNEARN_3,
                   TRJFILE.TJ_INSUNEARN_4,
                   TRJFILE.TJ_INSUNEARN_5,
                   TRJFILE.TJ_INSUNEARN_6,
                   TRJFILE.TJ_INSUNEARN_7,
                   TRJFILE.TJ_INSUNEARN_8,
                   TRJFILE.TJ_INSUNEARN_9,
                   TRJFILE.TJ_INSUNEARN_10,
                   TRJFILE.TJ_DLNO,
                   TRJFILE.TJ_DLRV,
                   TRJFILE.TJ_DLSPRV,
                   TRJFILE.TJ_DLDISC,
                   TRJFILE.TJ_DLPARV,
                   ISNULL(TRJFILE.TJ_DLRECOURSE,''),
                   TRJFILE.TJ_DLPARTRCAMT,
                   TRJFILE.TJ_PURBAL,
                   TRJFILE.TJ_UNEARN_1,
                   TRJFILE.TJ_UNEARN_2,
                   TRJFILE.TJ_UNEARN_3,
                   TRJFILE.TJ_UNEARN_4,
                   TRJFILE.TJ_UNEARN_5,
                   TRJFILE.TJ_UNEARN_6,
                   TRJFILE.TJ_UNEARN_7,
                   TRJFILE.TJ_UNEARN_8,
                   TRJFILE.TJ_UNEARN_9,
                   TRJFILE.TJ_UNEARN_10,
                   TRJFILE.TJ_PRLNNO,
                   TRJFILE.TJ_DLRVPRIN_1,
                   TRJFILE.TJ_DLRVPRIN_2,
                   TRJFILE.TJ_DLRVPRIN_3,
                   TRJFILE.TJ_DLRVCHGS_1,
                   TRJFILE.TJ_DLRVCHGS_2,
                   TRJFILE.TJ_DLRVCHGS_3,
                   TRJFILE.TJ_DLPARVPAID,
                   TRJFILE.TJ_DLPARVHELD,
                   ISNULL(TRJFILE.TJ_USERID,''),
                   ISNULL(CAST(TRJFILE.TJ_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRJFILE.TJ_POSTTIME,
                   ISNULL(TRJFILE.TJ_PLCD,''),
                   ISNULL(TRJFILE.TJ_PLTYPE,''),
                   ISNULL(TRJFILE.TJ_REPOCD,'')
            INTO :QTRJ-REC
            FROM DBO.TRJFILE
            WHERE TRJFILE.TJ_BRNO = :QTJ-BRNO
              AND TRJFILE.TJ_DATE = :QTJ-DATE
              AND TRJFILE.TJ_CLASS = :QTJ-CLASS
              AND TRJFILE.TJ_ACCTNO = :QTJ-ACCTNO
              AND TRJFILE.TJ_LAWCODE = :QTJ-LAWCODE
              AND TRJFILE.TJ_LINE = :QTJ-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRJ1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRJ-FIELDS.

      *-----------------------------------------------------------------
       READ-TRJ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRJ-PATH-OWNBR  TO TJ-BRNO.
           MOVE TRJ-SELECT-PATH TO E-FILE.
           MOVE TRJ1-KEY        TO E-KEYX.
           INITIALIZE QTRJ-REC.

           IF ( TRJ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRJ1_CURSOR INTO :QTRJ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRJ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRJ-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRJ1-FILE.

           IF ( TRJ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRJ1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRJ1-CURSOR-STAT.

      *============================================================
       LOAD-TRJ2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRJ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRJ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRJ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRJ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRJ-ALL-DATA
              MOVE TRJ-ALL-PATH             TO TRJ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRJ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRJ-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRJ-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRJ-ALL-DATA
              MOVE TRJ-ALL-PATH             TO TRJ-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRJFILE NOT = "S" )
              MOVE TRJ-PATH-BEG             TO TRJ-SELECT-PATH-BEG
              MOVE TRJ-PATH-FILE            TO TRJ-SELECT-PATH-FILE
           ELSE
              MOVE TRJ-PATH-SQL             TO TRJ-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRJ2-FILE.
           PERFORM LOAD-TRJ2-FILE.
           PERFORM OPEN-IT.
           MOVE TRJ-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRJ2-FILE.
           PERFORM START-IT.
           MOVE TRJ-PATH-OWNBR  TO TJ-BRNO.
           MOVE TRJ-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING TJ-DATE, TJ-BRNO, TJ-CLASS, TJ-ACCTNO, TJ-LAWCODE,
                  TJ-LINE DELIMITED BY SIZE INTO E-KEYX.

           IF ( TRJ2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRJ2-FILE.

           IF ( QTRJ2-WEND-LINE = ZEROES    ) OR
              ( QTRJ2-WEND-LINE NOT NUMERIC )
              MOVE TJ-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRJ2-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRJ2-WEND-DATE
              MOVE TRJ-PATH-OWNBR       TO QTRJ2-WBEG-BRNO
              MOVE TRJ-PATH-OWNBR       TO QTRJ2-WEND-BRNO
              MOVE TJ-CLASS             TO QTRJ2-WBEG-CLASS
              MOVE TJ-ACCTNO            TO QTRJ2-WBEG-ACCTNO
              MOVE TJ-LAWCODE           TO QTRJ2-WBEG-LAWCODE
              MOVE TJ-LINE              TO QTRJ2-WBEG-LINE
              MOVE ALL "9"              TO QTRJ2-WEND-CLASS
                                           QTRJ2-WEND-ACCTNO
                                           QTRJ2-WEND-LAWCODE
                                           QTRJ2-WEND-LINE.


           EXEC SQL
            DECLARE TRJ2_CURSOR CURSOR FOR
            SELECT TRJFILE.TJ_BRNO,
                   ISNULL(CAST(TRJFILE.TJ_DATE AS VARCHAR(10)),'0'),
                   TRJFILE.TJ_CLASS,
                   TRJFILE.TJ_ACCTNO,
                   TRJFILE.TJ_LAWCODE,
                   TRJFILE.TJ_LINE,
                   ISNULL(CAST(TRJFILE.TJ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRJFILE.TJ_SSNO_1,
                   ISNULL(TRJFILE.TJ_MAKERCD_1,''),
                   TRJFILE.TJ_SSNO_2,
                   ISNULL(TRJFILE.TJ_MAKERCD_2,''),
                   TRJFILE.TJ_SSNO_3,
                   ISNULL(TRJFILE.TJ_MAKERCD_3,''),
                   TRJFILE.TJ_SSNO_4,
                   ISNULL(TRJFILE.TJ_MAKERCD_4,''),
                   TRJFILE.TJ_REF_FRLC,
                   ISNULL(TRJFILE.TJ_REF_X,''),
                   TRJFILE.TJ_REF_TOLC,
                   TRJFILE.TJ_FRLC,
                   TRJFILE.TJ_TOLC,
                   ISNULL(TRJFILE.TJ_PURCD,''),
                   ISNULL(TRJFILE.TJ_SRCD,''),
                   ISNULL(TRJFILE.TJ_COLLCD,''),
                   ISNULL(TRJFILE.TJ_SERVACCT,''),
                   ISNULL(TRJFILE.TJ_LOANTYPE,''),
                   TRJFILE.TJ_SUBCLASS,
                   ISNULL(CAST(TRJFILE.TJ_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   TRJFILE.TJ_PROCEEDS,
                   TRJFILE.TJ_LNAMT,
                   TRJFILE.TJ_FINCHG,
                   TRJFILE.TJ_INTCHG,
                   TRJFILE.TJ_SERCHG,
                   TRJFILE.TJ_EXTCHG,
                   TRJFILE.TJ_MAINTFEE,
                   TRJFILE.TJ_ORGTERM,
                   ISNULL(TRJFILE.TJ_INSURED,''),
                   TRJFILE.TJ_INSUNEARN_1,
                   TRJFILE.TJ_INSUNEARN_2,
                   TRJFILE.TJ_INSUNEARN_3,
                   TRJFILE.TJ_INSUNEARN_4,
                   TRJFILE.TJ_INSUNEARN_5,
                   TRJFILE.TJ_INSUNEARN_6,
                   TRJFILE.TJ_INSUNEARN_7,
                   TRJFILE.TJ_INSUNEARN_8,
                   TRJFILE.TJ_INSUNEARN_9,
                   TRJFILE.TJ_INSUNEARN_10,
                   TRJFILE.TJ_DLNO,
                   TRJFILE.TJ_DLRV,
                   TRJFILE.TJ_DLSPRV,
                   TRJFILE.TJ_DLDISC,
                   TRJFILE.TJ_DLPARV,
                   ISNULL(TRJFILE.TJ_DLRECOURSE,''),
                   TRJFILE.TJ_DLPARTRCAMT,
                   TRJFILE.TJ_PURBAL,
                   TRJFILE.TJ_UNEARN_1,
                   TRJFILE.TJ_UNEARN_2,
                   TRJFILE.TJ_UNEARN_3,
                   TRJFILE.TJ_UNEARN_4,
                   TRJFILE.TJ_UNEARN_5,
                   TRJFILE.TJ_UNEARN_6,
                   TRJFILE.TJ_UNEARN_7,
                   TRJFILE.TJ_UNEARN_8,
                   TRJFILE.TJ_UNEARN_9,
                   TRJFILE.TJ_UNEARN_10,
                   TRJFILE.TJ_PRLNNO,
                   TRJFILE.TJ_DLRVPRIN_1,
                   TRJFILE.TJ_DLRVPRIN_2,
                   TRJFILE.TJ_DLRVPRIN_3,
                   TRJFILE.TJ_DLRVCHGS_1,
                   TRJFILE.TJ_DLRVCHGS_2,
                   TRJFILE.TJ_DLRVCHGS_3,
                   TRJFILE.TJ_DLPARVPAID,
                   TRJFILE.TJ_DLPARVHELD,
                   ISNULL(TRJFILE.TJ_USERID,''),
                   ISNULL(CAST(TRJFILE.TJ_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRJFILE.TJ_POSTTIME,
                   ISNULL(TRJFILE.TJ_PLCD,''),
                   ISNULL(TRJFILE.TJ_PLTYPE,''),
                   ISNULL(TRJFILE.TJ_REPOCD,'')
            FROM DBO.TRJFILE
            WHERE CAST(TRJFILE.TJ_DATE AS VARCHAR(10))
                          >= :QTRJ2-WBEG-DATE
              AND CAST(TRJFILE.TJ_DATE AS VARCHAR(10))
                          <= :QTRJ2-WEND-DATE
              AND TRJFILE.TJ_BRNO >= :QTRJ2-WBEG-BRNO
              AND TRJFILE.TJ_BRNO <= :QTRJ2-WEND-BRNO
              AND TRJFILE.TJ_CLASS >= :QTRJ2-WBEG-CLASS
              AND TRJFILE.TJ_CLASS <= :QTRJ2-WEND-CLASS
              AND TRJFILE.TJ_ACCTNO >= :QTRJ2-WBEG-ACCTNO
              AND TRJFILE.TJ_ACCTNO <= :QTRJ2-WEND-ACCTNO
              AND TRJFILE.TJ_LAWCODE >= :QTRJ2-WBEG-LAWCODE
              AND TRJFILE.TJ_LAWCODE <= :QTRJ2-WEND-LAWCODE
              AND TRJFILE.TJ_LINE >= :QTRJ2-WBEG-LINE
              AND TRJFILE.TJ_LINE <= :QTRJ2-WEND-LINE
            ORDER BY TRJFILE.TJ_DATE,
                     TRJFILE.TJ_BRNO,
                     TRJFILE.TJ_CLASS,
                     TRJFILE.TJ_ACCTNO,
                     TRJFILE.TJ_LAWCODE,
                     TRJFILE.TJ_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRJ2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRJ2-WBEG-BRNO
                          QTRJ2-WEND-BRNO
                          QTRJ2-WBEG-CLASS
                          QTRJ2-WEND-CLASS
                          QTRJ2-WBEG-ACCTNO
                          QTRJ2-WEND-ACCTNO
                          QTRJ2-WBEG-LAWCODE
                          QTRJ2-WEND-LAWCODE
                          QTRJ2-WBEG-LINE
                          QTRJ2-WEND-LINE.
           MOVE SPACES TO QTRJ2-WBEG-DATE
                          QTRJ2-WEND-DATE.

           MOVE STAT---GOOD TO TRJ2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRJ2-FILE.
           PERFORM READ-IT.
           MOVE TRJ-PATH-OWNBR  TO TJ-BRNO. 
           MOVE TRJ-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING TJ-DATE, TJ-BRNO, TJ-CLASS, TJ-ACCTNO, TJ-LAWCODE,
                  TJ-LINE DELIMITED BY SIZE INTO E-KEYX.

           MOVE TJ-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTJ-DATE.
           MOVE TJ-BRNO             TO QTJ-BRNO.
           MOVE TJ-CLASS            TO QTJ-CLASS.
           MOVE TJ-ACCTNO           TO QTJ-ACCTNO.
           MOVE TJ-LAWCODE          TO QTJ-LAWCODE.
           MOVE TJ-LINE             TO QTJ-LINE.

           EXEC SQL
            SELECT TRJFILE.TJ_BRNO,
                   ISNULL(CAST(TRJFILE.TJ_DATE AS VARCHAR(10)),'0'),
                   TRJFILE.TJ_CLASS,
                   TRJFILE.TJ_ACCTNO,
                   TRJFILE.TJ_LAWCODE,
                   TRJFILE.TJ_LINE,
                   ISNULL(CAST(TRJFILE.TJ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRJFILE.TJ_SSNO_1,
                   ISNULL(TRJFILE.TJ_MAKERCD_1,''),
                   TRJFILE.TJ_SSNO_2,
                   ISNULL(TRJFILE.TJ_MAKERCD_2,''),
                   TRJFILE.TJ_SSNO_3,
                   ISNULL(TRJFILE.TJ_MAKERCD_3,''),
                   TRJFILE.TJ_SSNO_4,
                   ISNULL(TRJFILE.TJ_MAKERCD_4,''),
                   TRJFILE.TJ_REF_FRLC,
                   ISNULL(TRJFILE.TJ_REF_X,''),
                   TRJFILE.TJ_REF_TOLC,
                   TRJFILE.TJ_FRLC,
                   TRJFILE.TJ_TOLC,
                   ISNULL(TRJFILE.TJ_PURCD,''),
                   ISNULL(TRJFILE.TJ_SRCD,''),
                   ISNULL(TRJFILE.TJ_COLLCD,''),
                   ISNULL(TRJFILE.TJ_SERVACCT,''),
                   ISNULL(TRJFILE.TJ_LOANTYPE,''),
                   TRJFILE.TJ_SUBCLASS,
                   ISNULL(CAST(TRJFILE.TJ_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   TRJFILE.TJ_PROCEEDS,
                   TRJFILE.TJ_LNAMT,
                   TRJFILE.TJ_FINCHG,
                   TRJFILE.TJ_INTCHG,
                   TRJFILE.TJ_SERCHG,
                   TRJFILE.TJ_EXTCHG,
                   TRJFILE.TJ_MAINTFEE,
                   TRJFILE.TJ_ORGTERM,
                   ISNULL(TRJFILE.TJ_INSURED,''),
                   TRJFILE.TJ_INSUNEARN_1,
                   TRJFILE.TJ_INSUNEARN_2,
                   TRJFILE.TJ_INSUNEARN_3,
                   TRJFILE.TJ_INSUNEARN_4,
                   TRJFILE.TJ_INSUNEARN_5,
                   TRJFILE.TJ_INSUNEARN_6,
                   TRJFILE.TJ_INSUNEARN_7,
                   TRJFILE.TJ_INSUNEARN_8,
                   TRJFILE.TJ_INSUNEARN_9,
                   TRJFILE.TJ_INSUNEARN_10,
                   TRJFILE.TJ_DLNO,
                   TRJFILE.TJ_DLRV,
                   TRJFILE.TJ_DLSPRV,
                   TRJFILE.TJ_DLDISC,
                   TRJFILE.TJ_DLPARV,
                   ISNULL(TRJFILE.TJ_DLRECOURSE,''),
                   TRJFILE.TJ_DLPARTRCAMT,
                   TRJFILE.TJ_PURBAL,
                   TRJFILE.TJ_UNEARN_1,
                   TRJFILE.TJ_UNEARN_2,
                   TRJFILE.TJ_UNEARN_3,
                   TRJFILE.TJ_UNEARN_4,
                   TRJFILE.TJ_UNEARN_5,
                   TRJFILE.TJ_UNEARN_6,
                   TRJFILE.TJ_UNEARN_7,
                   TRJFILE.TJ_UNEARN_8,
                   TRJFILE.TJ_UNEARN_9,
                   TRJFILE.TJ_UNEARN_10,
                   TRJFILE.TJ_PRLNNO,
                   TRJFILE.TJ_DLRVPRIN_1,
                   TRJFILE.TJ_DLRVPRIN_2,
                   TRJFILE.TJ_DLRVPRIN_3,
                   TRJFILE.TJ_DLRVCHGS_1,
                   TRJFILE.TJ_DLRVCHGS_2,
                   TRJFILE.TJ_DLRVCHGS_3,
                   TRJFILE.TJ_DLPARVPAID,
                   TRJFILE.TJ_DLPARVHELD,
                   ISNULL(TRJFILE.TJ_USERID,''),
                   ISNULL(CAST(TRJFILE.TJ_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRJFILE.TJ_POSTTIME,
                   ISNULL(TRJFILE.TJ_PLCD,''),
                   ISNULL(TRJFILE.TJ_PLTYPE,''),
                   ISNULL(TRJFILE.TJ_REPOCD,'')
            INTO :QTRJ-REC
            FROM DBO.TRJFILE
            WHERE TRJFILE.TJ_DATE = :QTJ-DATE
              AND TRJFILE.TJ_BRNO = :QTJ-BRNO
              AND TRJFILE.TJ_CLASS = :QTJ-CLASS
              AND TRJFILE.TJ_ACCTNO = :QTJ-ACCTNO
              AND TRJFILE.TJ_LAWCODE = :QTJ-LAWCODE
              AND TRJFILE.TJ_LINE = :QTJ-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRJ2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRJ-FIELDS.

      *-----------------------------------------------------------------
       READ-TRJ2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRJ-PATH-OWNBR  TO TJ-BRNO.
           MOVE TRJ-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING TJ-DATE, TJ-BRNO, TJ-CLASS, TJ-ACCTNO, TJ-LAWCODE,
                  TJ-LINE DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QTRJ-REC.

           IF ( TRJ2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRJ2_CURSOR INTO :QTRJ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRJ2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRJ-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRJ2-FILE.

           IF ( TRJ2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRJ2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRJ2-CURSOR-STAT.

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
              DESTROY IOTRJR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
