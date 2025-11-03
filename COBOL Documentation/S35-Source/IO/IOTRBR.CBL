       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOTRBR.
       AUTHOR.        BAH.
       DATE-WRITTEN.  04-12-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOTRBR.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : TRBFILE 
      * TABLE DESC    : TRANSACTION BRANCH BULK TRANSFER FILE
      * OUTPUT FILE   : XPC/IOTRBR.PRE
      * USED FOR      : LIBLP/LPWSTRB.CPY  --- QTRB-REC
      *                 LIBLP/LPTRB1R.CPY  --- READ
      *                 LIBLP/LPTRB1RN.CPY --- READ/READ-NEXT/START
      *                 LIBLP/LPTRB2RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0412 ***NEW***
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
           "@(#)./IO/IOTRBR.CBL S34 05/05/21-17:10:57 >".

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
       COPY "LIBLP/LP01TRB.CPY".
       COPY "LIBLP/LPWSTRB.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QTRB1-WHERE-BEG-KEY.
           03  QTRB1-WBEG-BRNO             PIC 9(04).
           03  QTRB1-WBEG-DATE             PIC X(10).
           03  QTRB1-WBEG-CLASS            PIC 9(03).
           03  QTRB1-WBEG-ACCTNO           PIC 9(08).
           03  QTRB1-WBEG-LAWCODE          PIC 9(02).
           03  QTRB1-WBEG-LINE             PIC 9(03).
 
       01  QTRB1-WHERE-END-KEY.
           03  QTRB1-WEND-BRNO             PIC 9(04).
           03  QTRB1-WEND-DATE             PIC X(10).
           03  QTRB1-WEND-CLASS            PIC 9(03).
           03  QTRB1-WEND-ACCTNO           PIC 9(08).
           03  QTRB1-WEND-LAWCODE          PIC 9(02).
           03  QTRB1-WEND-LINE             PIC 9(03).

       01  QTRB2-WHERE-BEG-KEY.
           03  QTRB2-WBEG-DATE             PIC X(10).
           03  QTRB2-WBEG-BRNO             PIC 9(04).
           03  QTRB2-WBEG-CLASS            PIC 9(03).
           03  QTRB2-WBEG-ACCTNO           PIC 9(08).
           03  QTRB2-WBEG-LAWCODE          PIC 9(02).
           03  QTRB2-WBEG-LINE             PIC 9(03).
 
       01  QTRB2-WHERE-END-KEY.
           03  QTRB2-WEND-DATE             PIC X(10).
           03  QTRB2-WEND-BRNO             PIC 9(04).
           03  QTRB2-WEND-CLASS            PIC 9(03).
           03  QTRB2-WEND-ACCTNO           PIC 9(08).
           03  QTRB2-WEND-LAWCODE          PIC 9(02).
           03  QTRB2-WEND-LINE             PIC 9(03).
 
 
       01  QTRB-REC.
           03  QTB-BRNO             PIC 9(4).
           03  QTB-DATE             PIC X(10).
           03  QTB-CLASS            PIC 9(3).
           03  QTB-ACCTNO           PIC 9(8).
           03  QTB-LAWCODE          PIC 99.
           03  QTB-LINE             PIC 9(3).
           03  QTB-LTOUCH-DATE      PIC X(10).
           03  QTB-SSNO-1         PIC 9(9).
           03  QTB-MAKERCD-1      PIC X.
           03  QTB-SSNO-2         PIC 9(9).
           03  QTB-MAKERCD-2      PIC X.
           03  QTB-SSNO-3         PIC 9(9).
           03  QTB-MAKERCD-3      PIC X.
           03  QTB-SSNO-4         PIC 9(9).
           03  QTB-MAKERCD-4      PIC X.
           03  QTB-REF-FRTO       PIC X.
           03  QTB-REFBR          PIC 9999.
           03  QTB-PURCD            PIC XXX.
           03  QTB-SRCD             PIC XX.
           03  QTB-COLLCD           PIC XX.
           03  QTB-SERVACCT         PIC X.
           03  QTB-LOANTYPE         PIC X.
           03  QTB-SUBCLASS         PIC 99.
           03  QTB-LOANDATE         PIC X(10).
           03  QTB-PROCEEDS         PIC S9(7)V99.
           03  QTB-LNAMT            PIC S9(7)V99.
           03  QTB-FINCHG           PIC S9(7)V99.
           03  QTB-INTCHG           PIC 9(7)V99.
           03  QTB-SERCHG           PIC 9(5)V99.
           03  QTB-EXTCHG           PIC 9(5)V99.
           03  QTB-MAINTFEE         PIC 9(5)V99.
           03  QTB-ORGTERM          PIC 9(3).
           03  QTB-INSURED          PIC X.
           03  QTB-INSUNEARN-1      PIC S9(7)V99.
           03  QTB-INSUNEARN-2      PIC S9(7)V99.
           03  QTB-INSUNEARN-3      PIC S9(7)V99.
           03  QTB-INSUNEARN-4      PIC S9(7)V99.
           03  QTB-INSUNEARN-5      PIC S9(7)V99.
           03  QTB-INSUNEARN-6      PIC S9(7)V99.
           03  QTB-INSUNEARN-7      PIC S9(7)V99.
           03  QTB-INSUNEARN-8      PIC S9(7)V99.
           03  QTB-INSUNEARN-9      PIC S9(7)V99.
           03  QTB-INSUNEARN-10     PIC S9(7)V99.
           03  QTB-DLNO             PIC 9(4)V99.
           03  QTB-DLRV             PIC S9(7)V99.
           03  QTB-DLSPRV           PIC S9(7)V99.
           03  QTB-DLDISC           PIC S9(7)V99.
           03  QTB-DLPARV           PIC S9(7)V99.
           03  QTB-DLRECOURSE       PIC X.
           03  QTB-DLPARTRCAMT      PIC 9(7)V99.
           03  QTB-PURBAL           PIC S9(7)V9.
           03  QTB-UNEARN-1         PIC S9(7)V99.
           03  QTB-UNEARN-2         PIC S9(7)V99.
           03  QTB-UNEARN-3         PIC S9(7)V99.
           03  QTB-UNEARN-4         PIC S9(7)V99.
           03  QTB-UNEARN-5         PIC S9(7)V99.
           03  QTB-UNEARN-6         PIC S9(7)V99.
           03  QTB-UNEARN-7         PIC S9(7)V99.
           03  QTB-UNEARN-8         PIC S9(7)V99.
           03  QTB-UNEARN-9         PIC S9(7)V99.
           03  QTB-UNEARN-10        PIC S9(7)V99.
           03  QTB-REF-ACCTNO       PIC 9(8).
           03  QTB-DLRVPRIN-1       PIC S9(8)V99.
           03  QTB-DLRVPRIN-2       PIC S9(8)V99.
           03  QTB-DLRVPRIN-3       PIC S9(8)V99.
           03  QTB-DLRVCHGS-1       PIC S9(7)V99.
           03  QTB-DLRVCHGS-2       PIC S9(7)V99.
           03  QTB-DLRVCHGS-3       PIC S9(7)V99.
           03  QTB-DLPARVPAID       PIC S9(7)V99.
           03  QTB-DLPARVHELD       PIC S9(7)V99.
           03  QTB-USERID           PIC X(10).
           03  QTB-POSTDATE         PIC X(10).
           03  QTB-POSTTIME         PIC 9(6).
           03  QTB-PLCD             PIC X.
           03  QTB-PLTYPE           PIC X.
           03  QTB-REPOCD           PIC X.

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
           03  D01-TB-BRNO                   PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-TB-DATE                   PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-TB-CLASS                  PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-TB-ACCTNO                 PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-TB-LAWCODE                PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-TB-LINE                   PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-TB-SSNO1                  PIC 9(09).
           03  FILLER                         PIC X(03).
           03  D01-TB-LOANDATE               PIC 9999/99/99.

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
       COPY "LIBIO/IOTRBR_DEF.CPY".
       COPY "LIBIO/IOTRBR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOTRBR_SCN.CPY".

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
           MOVE "IOTRBR"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOTRBR-QUEUE-POS  TO Q-POS.
           MOVE IOTRBR-COPIES-POS TO C-POS.
           MOVE IOTRBR-PRU-POS    TO P-POS.
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

           MOVE "B"                        TO TRB-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QTRB1-WBEG-BRNO
                              QTRB2-WBEG-BRNO.
           MOVE BR-NO     TO QTRB1-WEND-BRNO
                              QTRB2-WEND-BRNO.

           MOVE BEG-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRB1-WBEG-DATE
                                        QTRB2-WBEG-DATE.
           MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRB1-WEND-DATE
                                        QTRB2-WEND-DATE.

           MOVE BEG-CLASS  TO QTRB1-WBEG-CLASS
                              QTRB2-WBEG-CLASS.
           MOVE END-CLASS  TO QTRB1-WEND-CLASS
                              QTRB2-WEND-CLASS.

           MOVE BEG-ACCTNO TO QTRB1-WBEG-ACCTNO
                              QTRB2-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QTRB1-WEND-ACCTNO
                              QTRB2-WEND-ACCTNO.
           MOVE BEG-LAWCODE TO QTRB1-WBEG-LAWCODE
                               QTRB2-WBEG-LAWCODE.
           MOVE END-LAWCODE TO QTRB1-WEND-LAWCODE
                               QTRB2-WEND-LAWCODE.
           MOVE BEG-LINE TO QTRB1-WBEG-LINE
                            QTRB2-WBEG-LINE.
           MOVE END-LINE TO QTRB1-WEND-LINE
                            QTRB2-WEND-LINE.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF PROCESS-KEY = 1
              PERFORM OPEN-TRB1-FILE
              PERFORM START-TRB1-FILE
           ELSE
              PERFORM OPEN-TRB2-FILE
              PERFORM START-TRB2-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           IF PROCESS-KEY = 1
              PERFORM READ-TRB1-FILE-NEXT
           ELSE
              PERFORM READ-TRB2-FILE-NEXT.
           IF ( IO-BAD ) 
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( TB-BRNO < BEG-BRANCH )
                  GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( TB-BRNO > END-BRANCH )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF TB-DATE < BEG-DATE OR > END-DATE
                    GO TO PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 2 )
              IF ( TB-DATE < BEG-DATE )
                  GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( TB-DATE > END-DATE )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF TB-BRNO < BEG-BRANCH OR > END-BRANCH
                    GO TO PROCESS-BRANCH-NEXT.

           IF TB-CLASS < BEG-CLASS OR > END-CLASS
              GO TO PROCESS-BRANCH-NEXT.
           IF TB-ACCTNO < BEG-ACCTNO OR > END-ACCTNO
              GO TO PROCESS-BRANCH-NEXT.
           IF TB-LAWCODE < BEG-LAWCODE OR > END-LAWCODE
              GO TO PROCESS-BRANCH-NEXT.
           IF TB-LINE < BEG-LINE OR > END-LINE
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE TB-BRNO                    TO D01-TB-BRNO.
           MOVE TB-DATE                    TO D01-TB-DATE.
           MOVE TB-CLASS                   TO D01-TB-CLASS.
           MOVE TB-ACCTNO                  TO D01-TB-ACCTNO.
           MOVE TB-LAWCODE                 TO D01-TB-LAWCODE.
           MOVE TB-LINE                    TO D01-TB-LINE.
           MOVE TB-SSNO(1)                 TO D01-TB-SSNO1.
           MOVE TB-LOANDATE                TO D01-TB-LOANDATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           IF PROCESS-KEY = 1
              PERFORM CLOSE-TRB1-FILE
           ELSE
              PERFORM CLOSE-TRB2-FILE.

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
              MOVE TRB1-KEY TO PACE-DETAIL-KEY
           ELSE
              STRING TB-DATE, TB-BRNO, TB-CLASS, TB-ACCTNO, TB-LAWCODE,
                     TB-LINE DELIMITED BY SIZE INTO PACE-DETAIL-KEY.

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
           MOVE "SQL TABLE IO UTILITY - TRBFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOTRBR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOTRBR-SYSDATE.
           DISPLAY IOTRBR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOTRBR-FORM-FIELDS TO WR-BUF.
           MOVE IOTRBR-FORM-LIST TO WR-LIST.
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

           MOVE IOTRBR-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOTRBR-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOTRBR_EVA.CPY".
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
      *    LOAD TRB-REC FROM QTRB-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-TRB-FIELDS.
           MOVE QTB-BRNO TO TB-BRNO.

           IF ( QTB-DATE = "1900-01-01" )
              MOVE ZEROES TO TB-DATE
           ELSE
              MOVE QTB-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TB-DATE.

           MOVE QTB-CLASS TO TB-CLASS.
           MOVE QTB-ACCTNO TO TB-ACCTNO.
           MOVE QTB-LAWCODE TO TB-LAWCODE.
           MOVE QTB-LINE TO TB-LINE.

           IF ( QTB-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO TB-LTOUCH-DATE
           ELSE
              MOVE QTB-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TB-LTOUCH-DATE.

           MOVE QTB-SSNO-1 TO TB-SSNO(1).
           MOVE QTB-MAKERCD-1 TO TB-MAKERCD(1).
           MOVE QTB-SSNO-2 TO TB-SSNO(2).
           MOVE QTB-MAKERCD-2 TO TB-MAKERCD(2).
           MOVE QTB-SSNO-3 TO TB-SSNO(3).
           MOVE QTB-MAKERCD-3 TO TB-MAKERCD(3).
           MOVE QTB-SSNO-4 TO TB-SSNO(4).
           MOVE QTB-MAKERCD-4 TO TB-MAKERCD(4).
           MOVE QTB-REF-FRTO TO TB-REF-FRTO.
           MOVE QTB-REFBR TO TB-REFBR.
           MOVE QTB-PURCD TO TB-PURCD.
           MOVE QTB-SRCD TO TB-SRCD.
           MOVE QTB-COLLCD TO TB-COLLCD.
           MOVE QTB-SERVACCT TO TB-SERVACCT.
           MOVE QTB-LOANTYPE TO TB-LOANTYPE.
           MOVE QTB-SUBCLASS TO TB-SUBCLASS.

           IF ( QTB-LOANDATE = "1900-01-01" )
              MOVE ZEROES TO TB-LOANDATE
           ELSE
              MOVE QTB-LOANDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TB-LOANDATE.

           MOVE QTB-PROCEEDS TO TB-PROCEEDS.
           MOVE QTB-LNAMT TO TB-LNAMT.
           MOVE QTB-FINCHG TO TB-FINCHG.
           MOVE QTB-INTCHG TO TB-INTCHG.
           MOVE QTB-SERCHG TO TB-SERCHG.
           MOVE QTB-EXTCHG TO TB-EXTCHG.
           MOVE QTB-MAINTFEE TO TB-MAINTFEE.
           MOVE QTB-ORGTERM TO TB-ORGTERM.
           MOVE QTB-INSURED TO TB-INSURED.
           MOVE QTB-INSUNEARN-1 TO TB-INSUNEARN(1).
           MOVE QTB-INSUNEARN-2 TO TB-INSUNEARN(2).
           MOVE QTB-INSUNEARN-3 TO TB-INSUNEARN(3).
           MOVE QTB-INSUNEARN-4 TO TB-INSUNEARN(4).
           MOVE QTB-INSUNEARN-5 TO TB-INSUNEARN(5).
           MOVE QTB-INSUNEARN-6 TO TB-INSUNEARN(6).
           MOVE QTB-INSUNEARN-7 TO TB-INSUNEARN(7).
           MOVE QTB-INSUNEARN-8 TO TB-INSUNEARN(8).
           MOVE QTB-INSUNEARN-9 TO TB-INSUNEARN(9).
           MOVE QTB-INSUNEARN-10 TO TB-INSUNEARN(10).
           MOVE QTB-DLNO TO TB-DLNO.
           MOVE QTB-DLRV TO TB-DLRV.
           MOVE QTB-DLSPRV TO TB-DLSPRV.
           MOVE QTB-DLDISC TO TB-DLDISC.
           MOVE QTB-DLPARV TO TB-DLPARV.
           MOVE QTB-DLRECOURSE TO TB-DLRECOURSE.
           MOVE QTB-DLPARTRCAMT TO TB-DLPARTRCAMT.
           MOVE QTB-PURBAL TO TB-PURBAL.
           MOVE QTB-UNEARN-1 TO TB-UNEARN(1).
           MOVE QTB-UNEARN-2 TO TB-UNEARN(2).
           MOVE QTB-UNEARN-3 TO TB-UNEARN(3).
           MOVE QTB-UNEARN-4 TO TB-UNEARN(4).
           MOVE QTB-UNEARN-5 TO TB-UNEARN(5).
           MOVE QTB-UNEARN-6 TO TB-UNEARN(6).
           MOVE QTB-UNEARN-7 TO TB-UNEARN(7).
           MOVE QTB-UNEARN-8 TO TB-UNEARN(8).
           MOVE QTB-UNEARN-9 TO TB-UNEARN(9).
           MOVE QTB-UNEARN-10 TO TB-UNEARN(10).
           MOVE QTB-REF-ACCTNO TO TB-REF-ACCTNO.
           MOVE QTB-DLRVPRIN-1 TO TB-DLRVPRIN(1).
           MOVE QTB-DLRVPRIN-2 TO TB-DLRVPRIN(2).
           MOVE QTB-DLRVPRIN-3 TO TB-DLRVPRIN(3).
           MOVE QTB-DLRVCHGS-1 TO TB-DLRVCHGS(1).
           MOVE QTB-DLRVCHGS-2 TO TB-DLRVCHGS(2).
           MOVE QTB-DLRVCHGS-3 TO TB-DLRVCHGS(3).
           MOVE QTB-DLPARVPAID TO TB-DLPARVPAID.
           MOVE QTB-DLPARVHELD TO TB-DLPARVHELD.
           MOVE QTB-USERID TO TB-USERID.

           IF ( QTB-POSTDATE = "1900-01-01" )
              MOVE ZEROES TO TB-POSTDATE
           ELSE
              MOVE QTB-POSTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TB-POSTDATE.

           MOVE QTB-POSTTIME TO TB-POSTTIME.
           MOVE QTB-PLCD TO TB-PLCD.
           MOVE QTB-PLTYPE TO TB-PLTYPE.
           MOVE QTB-REPOCD TO TB-REPOCD.
 
      *-----------------------------------------------------------------
      *    LOAD QTRB-REC FROM TRB-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-TRB-FIELDS.
           MOVE TB-BRNO TO QTB-BRNO.

           IF ( TB-DATE = ZEROES )
              MOVE "1900-01-01" TO QTB-DATE
           ELSE
              MOVE TB-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTB-DATE.

           MOVE TB-CLASS TO QTB-CLASS.
           MOVE TB-ACCTNO TO QTB-ACCTNO.
           MOVE TB-LAWCODE TO QTB-LAWCODE.
           MOVE TB-LINE TO QTB-LINE.

           IF ( TB-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTB-LTOUCH-DATE
           ELSE
              MOVE TB-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTB-LTOUCH-DATE.

           MOVE TB-SSNO(1) TO QTB-SSNO-1.
           MOVE TB-MAKERCD(1) TO QTB-MAKERCD-1.
           MOVE TB-SSNO(2) TO QTB-SSNO-2.
           MOVE TB-MAKERCD(2) TO QTB-MAKERCD-2.
           MOVE TB-SSNO(3) TO QTB-SSNO-3.
           MOVE TB-MAKERCD(3) TO QTB-MAKERCD-3.
           MOVE TB-SSNO(4) TO QTB-SSNO-4.
           MOVE TB-MAKERCD(4) TO QTB-MAKERCD-4.
           MOVE TB-REF-FRTO TO QTB-REF-FRTO.
           MOVE TB-REFBR TO QTB-REFBR.
           MOVE TB-PURCD TO QTB-PURCD.
           MOVE TB-SRCD TO QTB-SRCD.
           MOVE TB-COLLCD TO QTB-COLLCD.
           MOVE TB-SERVACCT TO QTB-SERVACCT.
           MOVE TB-LOANTYPE TO QTB-LOANTYPE.
           MOVE TB-SUBCLASS TO QTB-SUBCLASS.

           IF ( TB-LOANDATE = ZEROES )
              MOVE "1900-01-01" TO QTB-LOANDATE
           ELSE
              MOVE TB-LOANDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTB-LOANDATE.

           MOVE TB-PROCEEDS TO QTB-PROCEEDS.
           MOVE TB-LNAMT TO QTB-LNAMT.
           MOVE TB-FINCHG TO QTB-FINCHG.
           MOVE TB-INTCHG TO QTB-INTCHG.
           MOVE TB-SERCHG TO QTB-SERCHG.
           MOVE TB-EXTCHG TO QTB-EXTCHG.
           MOVE TB-MAINTFEE TO QTB-MAINTFEE.
           MOVE TB-ORGTERM TO QTB-ORGTERM.
           MOVE TB-INSURED TO QTB-INSURED.
           MOVE TB-INSUNEARN(1) TO QTB-INSUNEARN-1.
           MOVE TB-INSUNEARN(2) TO QTB-INSUNEARN-2.
           MOVE TB-INSUNEARN(3) TO QTB-INSUNEARN-3.
           MOVE TB-INSUNEARN(4) TO QTB-INSUNEARN-4.
           MOVE TB-INSUNEARN(5) TO QTB-INSUNEARN-5.
           MOVE TB-INSUNEARN(6) TO QTB-INSUNEARN-6.
           MOVE TB-INSUNEARN(7) TO QTB-INSUNEARN-7.
           MOVE TB-INSUNEARN(8) TO QTB-INSUNEARN-8.
           MOVE TB-INSUNEARN(9) TO QTB-INSUNEARN-9.
           MOVE TB-INSUNEARN(10) TO QTB-INSUNEARN-10.
           MOVE TB-DLNO TO QTB-DLNO.
           MOVE TB-DLRV TO QTB-DLRV.
           MOVE TB-DLSPRV TO QTB-DLSPRV.
           MOVE TB-DLDISC TO QTB-DLDISC.
           MOVE TB-DLPARV TO QTB-DLPARV.
           MOVE TB-DLRECOURSE TO QTB-DLRECOURSE.
           MOVE TB-DLPARTRCAMT TO QTB-DLPARTRCAMT.
           MOVE TB-PURBAL TO QTB-PURBAL.
           MOVE TB-UNEARN(1) TO QTB-UNEARN-1.
           MOVE TB-UNEARN(2) TO QTB-UNEARN-2.
           MOVE TB-UNEARN(3) TO QTB-UNEARN-3.
           MOVE TB-UNEARN(4) TO QTB-UNEARN-4.
           MOVE TB-UNEARN(5) TO QTB-UNEARN-5.
           MOVE TB-UNEARN(6) TO QTB-UNEARN-6.
           MOVE TB-UNEARN(7) TO QTB-UNEARN-7.
           MOVE TB-UNEARN(8) TO QTB-UNEARN-8.
           MOVE TB-UNEARN(9) TO QTB-UNEARN-9.
           MOVE TB-UNEARN(10) TO QTB-UNEARN-10.
           MOVE TB-REF-ACCTNO TO QTB-REF-ACCTNO.
           MOVE TB-DLRVPRIN(1) TO QTB-DLRVPRIN-1.
           MOVE TB-DLRVPRIN(2) TO QTB-DLRVPRIN-2.
           MOVE TB-DLRVPRIN(3) TO QTB-DLRVPRIN-3.
           MOVE TB-DLRVCHGS(1) TO QTB-DLRVCHGS-1.
           MOVE TB-DLRVCHGS(2) TO QTB-DLRVCHGS-2.
           MOVE TB-DLRVCHGS(3) TO QTB-DLRVCHGS-3.
           MOVE TB-DLPARVPAID TO QTB-DLPARVPAID.
           MOVE TB-DLPARVHELD TO QTB-DLPARVHELD.
           MOVE TB-USERID TO QTB-USERID.

           IF ( TB-POSTDATE = ZEROES )
              MOVE "1900-01-01" TO QTB-POSTDATE
           ELSE
              MOVE TB-POSTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTB-POSTDATE.

           MOVE TB-POSTTIME TO QTB-POSTTIME.
           MOVE TB-PLCD TO QTB-PLCD.
           MOVE TB-PLTYPE TO QTB-PLTYPE.
           MOVE TB-REPOCD TO QTB-REPOCD.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-TRB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRB-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRB-ALL-DATA
              MOVE TRB-ALL-PATH             TO TRB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRB-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRB-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRB-ALL-DATA
              MOVE TRB-ALL-PATH             TO TRB-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRBFILE NOT = "S" )
              MOVE TRB-PATH-BEG             TO TRB-SELECT-PATH-BEG
              MOVE TRB-PATH-FILE            TO TRB-SELECT-PATH-FILE
           ELSE
              MOVE TRB-PATH-SQL             TO TRB-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRB1-FILE.
           PERFORM LOAD-TRB1-FILE.
           PERFORM OPEN-IT.
           MOVE TRB-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRB1-FILE.
           PERFORM START-IT.
           MOVE TRB-PATH-OWNBR  TO TB-BRNO.
           MOVE TRB-SELECT-PATH TO E-FILE.
           MOVE TRB1-KEY        TO E-KEYX.

           IF ( TRB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRB1-FILE.

           IF ( QTRB1-WEND-LINE = ZEROES    ) OR
              ( QTRB1-WEND-LINE NOT NUMERIC )
              MOVE TRB-PATH-OWNBR        TO QTRB1-WBEG-BRNO
              MOVE TRB-PATH-OWNBR        TO QTRB1-WEND-BRNO
              MOVE TB-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRB1-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRB1-WEND-DATE
              MOVE TB-CLASS             TO QTRB1-WBEG-CLASS
              MOVE TB-ACCTNO            TO QTRB1-WBEG-ACCTNO
              MOVE TB-LAWCODE           TO QTRB1-WBEG-LAWCODE
              MOVE TB-LINE              TO QTRB1-WBEG-LINE
              MOVE ALL "9"              TO QTRB1-WEND-CLASS
                                           QTRB1-WEND-ACCTNO
                                           QTRB1-WEND-LAWCODE
                                           QTRB1-WEND-LINE.


           EXEC SQL
            DECLARE TRB1_CURSOR CURSOR FOR
            SELECT TRBFILE.TB_BRNO,
                   ISNULL(CAST(TRBFILE.TB_DATE AS VARCHAR(10)),'0'),
                   TRBFILE.TB_CLASS,
                   TRBFILE.TB_ACCTNO,
                   TRBFILE.TB_LAWCODE,
                   TRBFILE.TB_LINE,
                   ISNULL(CAST(TRBFILE.TB_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRBFILE.TB_SSNO_1,
                   ISNULL(TRBFILE.TB_MAKERCD_1,''),
                   TRBFILE.TB_SSNO_2,
                   ISNULL(TRBFILE.TB_MAKERCD_2,''),
                   TRBFILE.TB_SSNO_3,
                   ISNULL(TRBFILE.TB_MAKERCD_3,''),
                   TRBFILE.TB_SSNO_4,
                   ISNULL(TRBFILE.TB_MAKERCD_4,''),
                   ISNULL(TRBFILE.TB_REF_FRTO,''),
                   TRBFILE.TB_REFBR,
                   ISNULL(TRBFILE.TB_PURCD,''),
                   ISNULL(TRBFILE.TB_SRCD,''),
                   ISNULL(TRBFILE.TB_COLLCD,''),
                   ISNULL(TRBFILE.TB_SERVACCT,''),
                   ISNULL(TRBFILE.TB_LOANTYPE,''),
                   TRBFILE.TB_SUBCLASS,
                   ISNULL(CAST(TRBFILE.TB_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   TRBFILE.TB_PROCEEDS,
                   TRBFILE.TB_LNAMT,
                   TRBFILE.TB_FINCHG,
                   TRBFILE.TB_INTCHG,
                   TRBFILE.TB_SERCHG,
                   TRBFILE.TB_EXTCHG,
                   TRBFILE.TB_MAINTFEE,
                   TRBFILE.TB_ORGTERM,
                   ISNULL(TRBFILE.TB_INSURED,''),
                   TRBFILE.TB_INSUNEARN_1,
                   TRBFILE.TB_INSUNEARN_2,
                   TRBFILE.TB_INSUNEARN_3,
                   TRBFILE.TB_INSUNEARN_4,
                   TRBFILE.TB_INSUNEARN_5,
                   TRBFILE.TB_INSUNEARN_6,
                   TRBFILE.TB_INSUNEARN_7,
                   TRBFILE.TB_INSUNEARN_8,
                   TRBFILE.TB_INSUNEARN_9,
                   TRBFILE.TB_INSUNEARN_10,
                   TRBFILE.TB_DLNO,
                   TRBFILE.TB_DLRV,
                   TRBFILE.TB_DLSPRV,
                   TRBFILE.TB_DLDISC,
                   TRBFILE.TB_DLPARV,
                   ISNULL(TRBFILE.TB_DLRECOURSE,''),
                   TRBFILE.TB_DLPARTRCAMT,
                   TRBFILE.TB_PURBAL,
                   TRBFILE.TB_UNEARN_1,
                   TRBFILE.TB_UNEARN_2,
                   TRBFILE.TB_UNEARN_3,
                   TRBFILE.TB_UNEARN_4,
                   TRBFILE.TB_UNEARN_5,
                   TRBFILE.TB_UNEARN_6,
                   TRBFILE.TB_UNEARN_7,
                   TRBFILE.TB_UNEARN_8,
                   TRBFILE.TB_UNEARN_9,
                   TRBFILE.TB_UNEARN_10,
                   TRBFILE.TB_REF_ACCTNO,
                   TRBFILE.TB_DLRVPRIN_1,
                   TRBFILE.TB_DLRVPRIN_2,
                   TRBFILE.TB_DLRVPRIN_3,
                   TRBFILE.TB_DLRVCHGS_1,
                   TRBFILE.TB_DLRVCHGS_2,
                   TRBFILE.TB_DLRVCHGS_3,
                   TRBFILE.TB_DLPARVPAID,
                   TRBFILE.TB_DLPARVHELD,
                   ISNULL(TRBFILE.TB_USERID,''),
                   ISNULL(CAST(TRBFILE.TB_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRBFILE.TB_POSTTIME,
                   ISNULL(TRBFILE.TB_PLCD,''),
                   ISNULL(TRBFILE.TB_PLTYPE,''),
                   ISNULL(TRBFILE.TB_REPOCD,'')
            FROM DBO.TRBFILE
            WHERE TRBFILE.TB_BRNO >= :QTRB1-WBEG-BRNO
              AND TRBFILE.TB_BRNO <= :QTRB1-WEND-BRNO
              AND CAST(TRBFILE.TB_DATE AS VARCHAR(10))
                          >= :QTRB1-WBEG-DATE
              AND CAST(TRBFILE.TB_DATE AS VARCHAR(10))
                          <= :QTRB1-WEND-DATE
              AND TRBFILE.TB_CLASS >= :QTRB1-WBEG-CLASS
              AND TRBFILE.TB_CLASS <= :QTRB1-WEND-CLASS
              AND TRBFILE.TB_ACCTNO >= :QTRB1-WBEG-ACCTNO
              AND TRBFILE.TB_ACCTNO <= :QTRB1-WEND-ACCTNO
              AND TRBFILE.TB_LAWCODE >= :QTRB1-WBEG-LAWCODE
              AND TRBFILE.TB_LAWCODE <= :QTRB1-WEND-LAWCODE
              AND TRBFILE.TB_LINE >= :QTRB1-WBEG-LINE
              AND TRBFILE.TB_LINE <= :QTRB1-WEND-LINE
            ORDER BY TRBFILE.TB_BRNO,
                     TRBFILE.TB_DATE,
                     TRBFILE.TB_CLASS,
                     TRBFILE.TB_ACCTNO,
                     TRBFILE.TB_LAWCODE,
                     TRBFILE.TB_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRB1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRB1-WBEG-BRNO 
                          QTRB1-WEND-BRNO
                          QTRB1-WBEG-CLASS
                          QTRB1-WEND-CLASS
                          QTRB1-WBEG-ACCTNO
                          QTRB1-WEND-ACCTNO
                          QTRB1-WBEG-LAWCODE
                          QTRB1-WEND-LAWCODE
                          QTRB1-WBEG-LINE
                          QTRB1-WEND-LINE.
           MOVE SPACES TO QTRB1-WBEG-DATE
                          QTRB1-WEND-DATE.

           MOVE STAT---GOOD TO TRB1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRB1-FILE.
           PERFORM READ-IT.
           MOVE TRB-PATH-OWNBR  TO TB-BRNO. 
           MOVE TRB-SELECT-PATH TO E-FILE.
           MOVE TRB1-KEY        TO E-KEYX.

           MOVE TB-BRNO             TO QTB-BRNO.
           MOVE TB-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTB-DATE.
           MOVE TB-CLASS            TO QTB-CLASS.
           MOVE TB-ACCTNO           TO QTB-ACCTNO.
           MOVE TB-LAWCODE          TO QTB-LAWCODE.
           MOVE TB-LINE             TO QTB-LINE.

           EXEC SQL
            SELECT TRBFILE.TB_BRNO,
                   ISNULL(CAST(TRBFILE.TB_DATE AS VARCHAR(10)),'0'),
                   TRBFILE.TB_CLASS,
                   TRBFILE.TB_ACCTNO,
                   TRBFILE.TB_LAWCODE,
                   TRBFILE.TB_LINE,
                   ISNULL(CAST(TRBFILE.TB_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRBFILE.TB_SSNO_1,
                   ISNULL(TRBFILE.TB_MAKERCD_1,''),
                   TRBFILE.TB_SSNO_2,
                   ISNULL(TRBFILE.TB_MAKERCD_2,''),
                   TRBFILE.TB_SSNO_3,
                   ISNULL(TRBFILE.TB_MAKERCD_3,''),
                   TRBFILE.TB_SSNO_4,
                   ISNULL(TRBFILE.TB_MAKERCD_4,''),
                   ISNULL(TRBFILE.TB_REF_FRTO,''),
                   TRBFILE.TB_REFBR,
                   ISNULL(TRBFILE.TB_PURCD,''),
                   ISNULL(TRBFILE.TB_SRCD,''),
                   ISNULL(TRBFILE.TB_COLLCD,''),
                   ISNULL(TRBFILE.TB_SERVACCT,''),
                   ISNULL(TRBFILE.TB_LOANTYPE,''),
                   TRBFILE.TB_SUBCLASS,
                   ISNULL(CAST(TRBFILE.TB_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   TRBFILE.TB_PROCEEDS,
                   TRBFILE.TB_LNAMT,
                   TRBFILE.TB_FINCHG,
                   TRBFILE.TB_INTCHG,
                   TRBFILE.TB_SERCHG,
                   TRBFILE.TB_EXTCHG,
                   TRBFILE.TB_MAINTFEE,
                   TRBFILE.TB_ORGTERM,
                   ISNULL(TRBFILE.TB_INSURED,''),
                   TRBFILE.TB_INSUNEARN_1,
                   TRBFILE.TB_INSUNEARN_2,
                   TRBFILE.TB_INSUNEARN_3,
                   TRBFILE.TB_INSUNEARN_4,
                   TRBFILE.TB_INSUNEARN_5,
                   TRBFILE.TB_INSUNEARN_6,
                   TRBFILE.TB_INSUNEARN_7,
                   TRBFILE.TB_INSUNEARN_8,
                   TRBFILE.TB_INSUNEARN_9,
                   TRBFILE.TB_INSUNEARN_10,
                   TRBFILE.TB_DLNO,
                   TRBFILE.TB_DLRV,
                   TRBFILE.TB_DLSPRV,
                   TRBFILE.TB_DLDISC,
                   TRBFILE.TB_DLPARV,
                   ISNULL(TRBFILE.TB_DLRECOURSE,''),
                   TRBFILE.TB_DLPARTRCAMT,
                   TRBFILE.TB_PURBAL,
                   TRBFILE.TB_UNEARN_1,
                   TRBFILE.TB_UNEARN_2,
                   TRBFILE.TB_UNEARN_3,
                   TRBFILE.TB_UNEARN_4,
                   TRBFILE.TB_UNEARN_5,
                   TRBFILE.TB_UNEARN_6,
                   TRBFILE.TB_UNEARN_7,
                   TRBFILE.TB_UNEARN_8,
                   TRBFILE.TB_UNEARN_9,
                   TRBFILE.TB_UNEARN_10,
                   TRBFILE.TB_REF_ACCTNO,
                   TRBFILE.TB_DLRVPRIN_1,
                   TRBFILE.TB_DLRVPRIN_2,
                   TRBFILE.TB_DLRVPRIN_3,
                   TRBFILE.TB_DLRVCHGS_1,
                   TRBFILE.TB_DLRVCHGS_2,
                   TRBFILE.TB_DLRVCHGS_3,
                   TRBFILE.TB_DLPARVPAID,
                   TRBFILE.TB_DLPARVHELD,
                   ISNULL(TRBFILE.TB_USERID,''),
                   ISNULL(CAST(TRBFILE.TB_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRBFILE.TB_POSTTIME,
                   ISNULL(TRBFILE.TB_PLCD,''),
                   ISNULL(TRBFILE.TB_PLTYPE,''),
                   ISNULL(TRBFILE.TB_REPOCD,'')
            INTO :QTRB-REC
            FROM DBO.TRBFILE
            WHERE TRBFILE.TB_BRNO = :QTB-BRNO
              AND TRBFILE.TB_DATE = :QTB-DATE
              AND TRBFILE.TB_CLASS = :QTB-CLASS
              AND TRBFILE.TB_ACCTNO = :QTB-ACCTNO
              AND TRBFILE.TB_LAWCODE = :QTB-LAWCODE
              AND TRBFILE.TB_LINE = :QTB-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRB-FIELDS.

      *-----------------------------------------------------------------
       READ-TRB1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRB-PATH-OWNBR  TO TB-BRNO.
           MOVE TRB-SELECT-PATH TO E-FILE.
           MOVE TRB1-KEY        TO E-KEYX.
           INITIALIZE QTRB-REC.

           IF ( TRB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRB1_CURSOR INTO :QTRB-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRB1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRB1-FILE.

           IF ( TRB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRB1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRB1-CURSOR-STAT.

      *=================================================================
       LOAD-TRB2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRB-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRB-ALL-DATA
              MOVE TRB-ALL-PATH             TO TRB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRB-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRB-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRB-ALL-DATA
              MOVE TRB-ALL-PATH             TO TRB-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRBFILE NOT = "S" )
              MOVE TRB-PATH-BEG             TO TRB-SELECT-PATH-BEG
              MOVE TRB-PATH-FILE            TO TRB-SELECT-PATH-FILE
           ELSE
              MOVE TRB-PATH-SQL             TO TRB-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRB2-FILE.
           PERFORM LOAD-TRB2-FILE.
           PERFORM OPEN-IT.
           MOVE TRB-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRB2-FILE.
           PERFORM START-IT.
           MOVE TRB-PATH-OWNBR  TO TB-BRNO.
           MOVE TRB-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING TB-DATE, TB-BRNO, TB-CLASS, TB-ACCTNO, TB-LAWCODE,
                  TB-LINE DELIMITED BY SIZE INTO E-KEYX.

           IF ( TRB2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRB2-FILE.

           IF ( QTRB2-WEND-LINE = ZEROES    ) OR
              ( QTRB2-WEND-LINE NOT NUMERIC )
              MOVE TB-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRB2-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRB2-WEND-DATE
              MOVE TRB-PATH-OWNBR       TO QTRB2-WBEG-BRNO
              MOVE TRB-PATH-OWNBR       TO QTRB2-WEND-BRNO
              MOVE TB-CLASS             TO QTRB2-WBEG-CLASS
              MOVE TB-ACCTNO            TO QTRB2-WBEG-ACCTNO
              MOVE TB-LAWCODE           TO QTRB2-WBEG-LAWCODE
              MOVE TB-LINE              TO QTRB2-WBEG-LINE
              MOVE ALL "9"              TO QTRB2-WEND-CLASS
                                           QTRB2-WEND-ACCTNO
                                           QTRB2-WEND-LAWCODE
                                           QTRB2-WEND-LINE.


           EXEC SQL
            DECLARE TRB2_CURSOR CURSOR FOR
            SELECT TRBFILE.TB_BRNO,
                   ISNULL(CAST(TRBFILE.TB_DATE AS VARCHAR(10)),'0'),
                   TRBFILE.TB_CLASS,
                   TRBFILE.TB_ACCTNO,
                   TRBFILE.TB_LAWCODE,
                   TRBFILE.TB_LINE,
                   ISNULL(CAST(TRBFILE.TB_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRBFILE.TB_SSNO_1,
                   ISNULL(TRBFILE.TB_MAKERCD_1,''),
                   TRBFILE.TB_SSNO_2,
                   ISNULL(TRBFILE.TB_MAKERCD_2,''),
                   TRBFILE.TB_SSNO_3,
                   ISNULL(TRBFILE.TB_MAKERCD_3,''),
                   TRBFILE.TB_SSNO_4,
                   ISNULL(TRBFILE.TB_MAKERCD_4,''),
                   ISNULL(TRBFILE.TB_REF_FRTO,''),
                   TRBFILE.TB_REFBR,
                   ISNULL(TRBFILE.TB_PURCD,''),
                   ISNULL(TRBFILE.TB_SRCD,''),
                   ISNULL(TRBFILE.TB_COLLCD,''),
                   ISNULL(TRBFILE.TB_SERVACCT,''),
                   ISNULL(TRBFILE.TB_LOANTYPE,''),
                   TRBFILE.TB_SUBCLASS,
                   ISNULL(CAST(TRBFILE.TB_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   TRBFILE.TB_PROCEEDS,
                   TRBFILE.TB_LNAMT,
                   TRBFILE.TB_FINCHG,
                   TRBFILE.TB_INTCHG,
                   TRBFILE.TB_SERCHG,
                   TRBFILE.TB_EXTCHG,
                   TRBFILE.TB_MAINTFEE,
                   TRBFILE.TB_ORGTERM,
                   ISNULL(TRBFILE.TB_INSURED,''),
                   TRBFILE.TB_INSUNEARN_1,
                   TRBFILE.TB_INSUNEARN_2,
                   TRBFILE.TB_INSUNEARN_3,
                   TRBFILE.TB_INSUNEARN_4,
                   TRBFILE.TB_INSUNEARN_5,
                   TRBFILE.TB_INSUNEARN_6,
                   TRBFILE.TB_INSUNEARN_7,
                   TRBFILE.TB_INSUNEARN_8,
                   TRBFILE.TB_INSUNEARN_9,
                   TRBFILE.TB_INSUNEARN_10,
                   TRBFILE.TB_DLNO,
                   TRBFILE.TB_DLRV,
                   TRBFILE.TB_DLSPRV,
                   TRBFILE.TB_DLDISC,
                   TRBFILE.TB_DLPARV,
                   ISNULL(TRBFILE.TB_DLRECOURSE,''),
                   TRBFILE.TB_DLPARTRCAMT,
                   TRBFILE.TB_PURBAL,
                   TRBFILE.TB_UNEARN_1,
                   TRBFILE.TB_UNEARN_2,
                   TRBFILE.TB_UNEARN_3,
                   TRBFILE.TB_UNEARN_4,
                   TRBFILE.TB_UNEARN_5,
                   TRBFILE.TB_UNEARN_6,
                   TRBFILE.TB_UNEARN_7,
                   TRBFILE.TB_UNEARN_8,
                   TRBFILE.TB_UNEARN_9,
                   TRBFILE.TB_UNEARN_10,
                   TRBFILE.TB_REF_ACCTNO,
                   TRBFILE.TB_DLRVPRIN_1,
                   TRBFILE.TB_DLRVPRIN_2,
                   TRBFILE.TB_DLRVPRIN_3,
                   TRBFILE.TB_DLRVCHGS_1,
                   TRBFILE.TB_DLRVCHGS_2,
                   TRBFILE.TB_DLRVCHGS_3,
                   TRBFILE.TB_DLPARVPAID,
                   TRBFILE.TB_DLPARVHELD,
                   ISNULL(TRBFILE.TB_USERID,''),
                   ISNULL(CAST(TRBFILE.TB_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRBFILE.TB_POSTTIME,
                   ISNULL(TRBFILE.TB_PLCD,''),
                   ISNULL(TRBFILE.TB_PLTYPE,''),
                   ISNULL(TRBFILE.TB_REPOCD,'')
            FROM DBO.TRBFILE
            WHERE CAST(TRBFILE.TB_DATE AS VARCHAR(10))
                          >= :QTRB2-WBEG-DATE
              AND CAST(TRBFILE.TB_DATE AS VARCHAR(10))
                          <= :QTRB2-WEND-DATE
              AND TRBFILE.TB_BRNO >= :QTRB2-WBEG-BRNO
              AND TRBFILE.TB_BRNO <= :QTRB2-WEND-BRNO
              AND TRBFILE.TB_CLASS >= :QTRB2-WBEG-CLASS
              AND TRBFILE.TB_CLASS <= :QTRB2-WEND-CLASS
              AND TRBFILE.TB_ACCTNO >= :QTRB2-WBEG-ACCTNO
              AND TRBFILE.TB_ACCTNO <= :QTRB2-WEND-ACCTNO
              AND TRBFILE.TB_LAWCODE >= :QTRB2-WBEG-LAWCODE
              AND TRBFILE.TB_LAWCODE <= :QTRB2-WEND-LAWCODE
              AND TRBFILE.TB_LINE >= :QTRB2-WBEG-LINE
              AND TRBFILE.TB_LINE <= :QTRB2-WEND-LINE
            ORDER BY TRBFILE.TB_DATE,
                     TRBFILE.TB_BRNO,
                     TRBFILE.TB_CLASS,
                     TRBFILE.TB_ACCTNO,
                     TRBFILE.TB_LAWCODE,
                     TRBFILE.TB_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRB2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRB2-WBEG-BRNO 
                          QTRB2-WEND-BRNO
                          QTRB2-WBEG-CLASS
                          QTRB2-WEND-CLASS
                          QTRB2-WBEG-ACCTNO
                          QTRB2-WEND-ACCTNO
                          QTRB2-WBEG-LAWCODE
                          QTRB2-WEND-LAWCODE
                          QTRB2-WBEG-LINE
                          QTRB2-WEND-LINE.
           MOVE SPACES TO QTRB2-WBEG-DATE
                          QTRB2-WEND-DATE.

           MOVE STAT---GOOD TO TRB2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRB2-FILE.
           PERFORM READ-IT.
           MOVE TRB-PATH-OWNBR  TO TB-BRNO. 
           MOVE TRB-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING TB-DATE, TB-BRNO, TB-CLASS, TB-ACCTNO, TB-LAWCODE,
                  TB-LINE DELIMITED BY SIZE INTO E-KEYX.

           MOVE TB-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTB-DATE.
           MOVE TB-BRNO             TO QTB-BRNO.
           MOVE TB-CLASS            TO QTB-CLASS.
           MOVE TB-ACCTNO           TO QTB-ACCTNO.
           MOVE TB-LAWCODE          TO QTB-LAWCODE.
           MOVE TB-LINE             TO QTB-LINE.

           EXEC SQL
            SELECT TRBFILE.TB_BRNO,
                   ISNULL(CAST(TRBFILE.TB_DATE AS VARCHAR(10)),'0'),
                   TRBFILE.TB_CLASS,
                   TRBFILE.TB_ACCTNO,
                   TRBFILE.TB_LAWCODE,
                   TRBFILE.TB_LINE,
                   ISNULL(CAST(TRBFILE.TB_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRBFILE.TB_SSNO_1,
                   ISNULL(TRBFILE.TB_MAKERCD_1,''),
                   TRBFILE.TB_SSNO_2,
                   ISNULL(TRBFILE.TB_MAKERCD_2,''),
                   TRBFILE.TB_SSNO_3,
                   ISNULL(TRBFILE.TB_MAKERCD_3,''),
                   TRBFILE.TB_SSNO_4,
                   ISNULL(TRBFILE.TB_MAKERCD_4,''),
                   ISNULL(TRBFILE.TB_REF_FRTO,''),
                   TRBFILE.TB_REFBR,
                   ISNULL(TRBFILE.TB_PURCD,''),
                   ISNULL(TRBFILE.TB_SRCD,''),
                   ISNULL(TRBFILE.TB_COLLCD,''),
                   ISNULL(TRBFILE.TB_SERVACCT,''),
                   ISNULL(TRBFILE.TB_LOANTYPE,''),
                   TRBFILE.TB_SUBCLASS,
                   ISNULL(CAST(TRBFILE.TB_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   TRBFILE.TB_PROCEEDS,
                   TRBFILE.TB_LNAMT,
                   TRBFILE.TB_FINCHG,
                   TRBFILE.TB_INTCHG,
                   TRBFILE.TB_SERCHG,
                   TRBFILE.TB_EXTCHG,
                   TRBFILE.TB_MAINTFEE,
                   TRBFILE.TB_ORGTERM,
                   ISNULL(TRBFILE.TB_INSURED,''),
                   TRBFILE.TB_INSUNEARN_1,
                   TRBFILE.TB_INSUNEARN_2,
                   TRBFILE.TB_INSUNEARN_3,
                   TRBFILE.TB_INSUNEARN_4,
                   TRBFILE.TB_INSUNEARN_5,
                   TRBFILE.TB_INSUNEARN_6,
                   TRBFILE.TB_INSUNEARN_7,
                   TRBFILE.TB_INSUNEARN_8,
                   TRBFILE.TB_INSUNEARN_9,
                   TRBFILE.TB_INSUNEARN_10,
                   TRBFILE.TB_DLNO,
                   TRBFILE.TB_DLRV,
                   TRBFILE.TB_DLSPRV,
                   TRBFILE.TB_DLDISC,
                   TRBFILE.TB_DLPARV,
                   ISNULL(TRBFILE.TB_DLRECOURSE,''),
                   TRBFILE.TB_DLPARTRCAMT,
                   TRBFILE.TB_PURBAL,
                   TRBFILE.TB_UNEARN_1,
                   TRBFILE.TB_UNEARN_2,
                   TRBFILE.TB_UNEARN_3,
                   TRBFILE.TB_UNEARN_4,
                   TRBFILE.TB_UNEARN_5,
                   TRBFILE.TB_UNEARN_6,
                   TRBFILE.TB_UNEARN_7,
                   TRBFILE.TB_UNEARN_8,
                   TRBFILE.TB_UNEARN_9,
                   TRBFILE.TB_UNEARN_10,
                   TRBFILE.TB_REF_ACCTNO,
                   TRBFILE.TB_DLRVPRIN_1,
                   TRBFILE.TB_DLRVPRIN_2,
                   TRBFILE.TB_DLRVPRIN_3,
                   TRBFILE.TB_DLRVCHGS_1,
                   TRBFILE.TB_DLRVCHGS_2,
                   TRBFILE.TB_DLRVCHGS_3,
                   TRBFILE.TB_DLPARVPAID,
                   TRBFILE.TB_DLPARVHELD,
                   ISNULL(TRBFILE.TB_USERID,''),
                   ISNULL(CAST(TRBFILE.TB_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRBFILE.TB_POSTTIME,
                   ISNULL(TRBFILE.TB_PLCD,''),
                   ISNULL(TRBFILE.TB_PLTYPE,''),
                   ISNULL(TRBFILE.TB_REPOCD,'')
            INTO :QTRB-REC
            FROM DBO.TRBFILE
            WHERE TRBFILE.TB_DATE = :QTB-DATE
              AND TRBFILE.TB_BRNO = :QTB-BRNO
              AND TRBFILE.TB_CLASS = :QTB-CLASS
              AND TRBFILE.TB_ACCTNO = :QTB-ACCTNO
              AND TRBFILE.TB_LAWCODE = :QTB-LAWCODE
              AND TRBFILE.TB_LINE = :QTB-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRB2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRB-FIELDS.

      *-----------------------------------------------------------------
       READ-TRB2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRB-PATH-OWNBR  TO TB-BRNO.
           MOVE TRB-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING TB-DATE, TB-BRNO, TB-CLASS, TB-ACCTNO, TB-LAWCODE,
                  TB-LINE DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QTRB-REC.

           IF ( TRB2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRB2_CURSOR INTO :QTRB-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRB2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRB2-FILE.

           IF ( TRB2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRB2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRB2-CURSOR-STAT.

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
              DESTROY IOTRBR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
