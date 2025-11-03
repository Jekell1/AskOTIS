       IDENTIFICATION DIVISION.
       PROGRAM-ID.   RXREPT.
       DATE-WRITTEN. 120996.
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  RESCISSION INDEX REPORT
      *
      * REV:
      *  GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  BAH 170718 ACTIVATE 8 DIGIT DATES, PD0006
      *  AMM 190412 LOCAL TO GLOBAL CHANGES RX FILE
      *  BAH 20190930 REMOVED ACCESS-CALL RXFILE
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/RXREPT.CBL S35 03/23/22-10:47:26 >".
      *=================================================================
      *
      *   F I L E   P A T H S
      *
      *=================================================================
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01RX.CPY".
       COPY "LIBLP/LP01RX_SQL.CPY".
       COPY "LIBLP/LPWSRX.CPY".
       
       01  PR-IFN               PIC X(2)     VALUE "PR".

       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      *=================================================================
      *
      *   M I S C - W O R K E R S
      *
      *=================================================================

       01  DATE-TOTALS                  PIC X(1)    VALUE " ".
           88  DATE-TOTALS-ARREARAGE                VALUE "A".
           88  DATE-TOTALS-TODAY                    VALUE "T".
           88  DATE-TOTALS-FUTURE                   VALUE "F".
           88  DATE-TOTALS-RESCIND                  VALUE "R".
           88  DATE-TOTALS-RESCISSION               VALUE "A" "T" "F".

       01  FIRST-TIME                   PIC X       VALUE "Y".
       01  LEGEND                       PIC X(70).
       01  MESS-LIST                    PIC X(5)    VALUE "MESS.".
       01  MSEL-LIST                    PIC X(5)    VALUE "MSEL.".
       01  RECORDS-FOUND                PIC X       VALUE "Y".
           88  NO-RECORDS-FOUND                     VALUE "N".
       01  TODAY-DATE                   PIC 9(8)    VALUE ZEROES.

      *=================================================================
      *
      *    L E V E L S
      *
      *=================================================================
       01  LEVELS.
       
           03  LEV1                 PIC 9(1).
           03  LEV1-BRANCH          PIC 9(1)            VALUE 1.
           03  LEV1-GRAND           PIC 9(1)            VALUE 2.

           03  LEV2                 PIC 9(1).
           03  LEV2-ARREARAGE       PIC 9(1)            VALUE 1.
           03  LEV2-TODAY           PIC 9(1)            VALUE 2.
           03  LEV2-FUTURE          PIC 9(1)            VALUE 3.
           03  LEV2-RESCIND         PIC 9(1)            VALUE 4.
           03  LEV2-RESCISSION      PIC 9(1)            VALUE 5.

      *=================================================================
      *
      *    P R O M P T S
      *
      *=================================================================
       01  PROMPTS.
           03  PROMPT-HEAD-BRANCH          PIC X(25)   VALUE
               "B R A N C H - T O T A L S".
           03  PROMPT-HEAD-GRAND           PIC X(23)   VALUE
               "G R A N D - T O T A L S".
           03  PROMPT-SUMM-UPDATE-ARREAR   PIC X(37)   VALUE
               "ACCOUNTS DUE FOR UPDATE IN ARREARAGE:".
           03  PROMPT-SUMM-UPDATE-FUTURE   PIC X(37)   VALUE
               "ACCOUNTS DUE FOR UPDATE IN FUTURE   :".
           03  PROMPT-SUMM-UPDATE-TODAY    PIC X(37)   VALUE
               "ACCOUNTS DUE FOR UPDATE TODAY       :".
           03  PROMPT-SUMM-UPDATE-RESCIND  PIC X(37)   VALUE
               "ACCOUNTS THAT HAVE BEEN RESCINDED   :".
           03  PROMPT-SUMM-UPDATE-RESCISS  PIC X(37)   VALUE
               "TOTAL OF ACCOUNTS IN RESCISSION     :".
           03  PROMPT-RANGE-BEG-ACCTNO     PIC X(25)   VALUE
               "BEGINNING ACCOUNT NUMBER:".
           03  PROMPT-RANGE-END-ACCTNO     PIC X(25)   VALUE
               "ENDING    ACCOUNT NUMBER:".
           03  PROMPT-RANGE-BEG-BRANCH     PIC X(25)   VALUE
               "BEGINNING BRANCH NUMBER :".
           03  PROMPT-RANGE-END-BRANCH     PIC X(25)   VALUE
               "ENDING    BRANCH NUMBER :".
           03  PROMPT-RANGE-BEG-INTDATE    PIC X(25)   VALUE
               "BEGINNING INTEREST DATE :".
           03  PROMPT-RANGE-END-INTDATE    PIC X(25)   VALUE
               "ENDING    INTEREST DATE :".
           03  PROMPT-RANGE-RESCISSION     PIC X(25)   VALUE
               "RESCISSION STATUS       :".
           03  PROMPT-RANGE-PRT-DETAIL     PIC X(25)   VALUE
               "PRINT DETAIL ANALYSIS   :".
           03  PROMPT-RANGE-PRT-BRANCH     PIC X(25)   VALUE
               "PRINT BRANCH TOTALS     :".
           03  PROMPT-RANGE-PRT-GRAND      PIC X(25)   VALUE
               "PRINT GRAND TOTALS      :".

      *=================================================================
      *
      *    W S - F I E L D S
      *
      *=================================================================
       01  WS-FIELDS.
           03  WS-NEXT-RX                    PIC X(1)     VALUE SPACES.
           03  WS-TOTAL-HEAD-FG              PIC X(1)     VALUE SPACES.
               88  WS-TOTAL-HEAD-DETAIL                   VALUE "D".
               88  WS-TOTAL-HEAD-BRANCH                   VALUE "B".
               88  WS-TOTAL-HEAD-GRAND                    VALUE "G".
               88  WS-TOTAL-HEAD-RANGE                    VALUE "R".

      *=================================================================
      *
      *    T O T A L S
      *
       
      *    LEV1 --->  1 = BRANCH TOTALS
      *               2 = GRAND  TOTALS
      *
      *    LEV2 --->  1 = ARREARAGE     AMOUNTS/COUNTERS
      *               2 = TODAY         AMOUNTS/COUNTERS
      *               3 = FUTURE        AMOUNTS/COUNTERS
      *               4 = RESCIND       AMOUNTS/COUNTERS
      *               5 = IN RESCISSION AMOUNTS/COUNTERS
      *
      *=================================================================
       01  TT-TOTALS.
           03  TT-TOTALS-LEVEL            OCCURS 2.
               07  TT-TOTALS-COUNTERS     OCCURS 5.
                   09  TT-ACCTS-COUNTER   PIC 9(6)    VALUE ZEROES.
               07  TT-TOTALS-AMOUNTS      OCCURS 5.
                   09  TT-AMTS-PROCEEDS   PIC 9(9)V99 VALUE ZEROES.
                   09  TT-AMTS-LOANAMT    PIC 9(9)V99 VALUE ZEROES.

      *=================================================================
      *
      *     S C R E E N   F I E L D S
      *
      *=================================================================
       COPY "LIBGB/EOBUF_EXT.CPY".
       
           03  BEG-BRANCH          PIC 9(4).
           03  END-BRANCH          PIC 9(4).
           03  BEG-ACCTNO          PIC 9(8).
           03  END-ACCTNO          PIC 9(8).
           03  BEG-DATE            PIC 9(8).
           03  END-DATE            PIC 9(8).
           03  PRINT-DETAIL        PIC X(1).
           03  PRINT-BRANCH-TOTAL  PIC X(1).
           03  PRINT-GRAND-TOTAL   PIC X(1).
           03  RESCIND-STATUS      PIC X(1).
           03  GROUP-FLAG          PIC X(1).
           03  ENT-GROUP           PIC X(4).
           03  LOCAL-PRINT-FG      PIC X(1).

       01  VERSION-CONTROL         PIC X VALUE "A".

       01  SV-BEG-BR               PIC 9(4)  VALUE 9999.
       01  SV-END-BR               PIC 9(4)  VALUE 9999.
       01  SV-GROUP-FLAG           PIC X(1)  VALUE SPACES.
       01  SV-ENT-GROUP            PIC X(4)  VALUE SPACES.

      *****************************************************************
      *
      *  P R I N T  B U F F E R S
      *
      *****************************************************************
       01  HEAD1.
           03  H1-DATE       PIC X(8)    VALUE " ".
           03  FILLER        PIC XX      VALUE " ".
           03  H1-TIME       PIC X(8)    VALUE " ".
           03  FILLER        PIC X(28)   VALUE " ".
           03  H1-NAME       PIC X(40)   VALUE " ".
           03  FILLER        PIC X(37)   VALUE " ".
           03  FILLER        PIC X(5)    VALUE "PAGE".
           03  H1-PAGE-NO    PIC ZZZ9.

       
       01  HEAD2.
           03  FILLER        PIC X(11)   VALUE "USER ID  : ".
           03  H2-USERID     PIC X(10).
           03  FILLER        PIC X(25)   VALUE " ".
           03  FILLER        PIC X(23)   VALUE
               "RESCISSION INDEX REPORT".
           03  FILLER        PIC X(63)   VALUE " ".

       
       01  HEAD3.
           03  FILLER        PIC X(11)   VALUE "BRANCH   : ".
           03  H3-BRNO       PIC 9(4).
           03  FILLER        PIC X(2)     VALUE " ".
           03  H3-BRDESC     PIC X(40)    VALUE " ".
           03  FILLER        PIC X(75)    VALUE " ".

       
       01  HEAD4.
           03  FILLER        PIC X(50)   VALUE
               "ACCOUNT                         LOAN    INTEREST  ".
           03  FILLER        PIC X(50)   VALUE
               "                 LOAN    DEALER                   ".
           03  FILLER        PIC X(32)   VALUE
               "        RESCISSION              ".

       
       01  HEAD5.
           03  FILLER        PIC X(50)   VALUE
               "NUMBER  BORROWER NAME           DATE      DATE    ".
           03  FILLER        PIC X(50)   VALUE
               "  PROCEEDS      AMOUNT   NUMBER  DEALER NAME      ".
           03  FILLER        PIC X(32)   VALUE
               "          STATUS                ".

       
       01  HEAD-SUMMARY-1.

           03  FILLER        PIC X(10)   VALUE SPACES.
           03  H-S1-TITLE    PIC X(31)   VALUE SPACES.
           03  FILLER        PIC X(9)    VALUE "      NUM".
           03  FILLER        PIC X(50)   VALUE
               "BER OF    TOTAL AMOUNT    TOTAL AMOUNT            ".
           03  FILLER        PIC X(32)   VALUE SPACES.

       
       01  HEAD-SUMMARY-2.
           03  FILLER        PIC X(50)   VALUE
               "                                                AC".
           03  FILLER        PIC X(50)   VALUE
               "COUNTS     OF PROCEEDS         OF LOAN            ".
           03  FILLER        PIC X(32)   VALUE SPACES.

      *****************************************************************
      *
      *    D E T A I L - L I N E
      *
      *****************************************************************
       01  DETAIL-LINE      PIC X(132)   VALUE SPACES.

       
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D1-NUMBER            PIC 9(6).
           03  FILLER               PIC X(2).
           03  D1-NAME              PIC X(20).
           03  FILLER               PIC X(2).
           03  D1-LOANDATE          PIC 99/99/99.
           03  FILLER               PIC X(2).
           03  D1-INTDATE           PIC 99/99/99.
           03  FILLER               PIC X(2).
           03  D1-PROCEEDS          PIC ZZZ,ZZ9.99.
           03  FILLER               PIC X(2).
           03  D1-LOANAMT           PIC ZZZ,ZZ9.99.
           03  FILLER               PIC X(2).
           03  D1-DLNO              PIC ZZZ9.99.
           03  FILLER               PIC X(2).
           03  D1-DL-NAME           PIC X(20).
           03  FILLER               PIC X(2).
           03  D1-RESCIND-STATUS    PIC X(15).
           03  FILLER               PIC X(7).

       
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  FILLER               PIC X(10).
           03  D2-TEXT              PIC X(37).
           03  FILLER               PIC X(2).
           03  D2-COUNTERS          PIC ZZZ,ZZ9.
           03  FILLER               PIC X(2).
           03  D2-PROCEEDS          PIC ZZZ,ZZZ,ZZ9.99.
           03  FILLER               PIC X(2).
           03  D2-LOANAMT           PIC ZZZ,ZZZ,ZZ9.99.
           03  FILLER               PIC X(54).

       
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(10).
           03  R-TEXT           PIC X(25).
           03  FILLER           PIC X(1).
           03  R-NUM            PIC Z(9)9.
           03  R-CHAR           REDEFINES R-NUM
                                PIC X(10).
           03  R-DATE           REDEFINES R-NUM
                                PIC 9999/99/99.
           03  FILLER           PIC X(86).
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       
       01  LEGEND-ACCTNO-01.
           03  FILLER           PIC X(12)  VALUE "F7-EXIT, ^, ".
           03  FILLER           PIC X      VALUE X'76'.
           03  FILLER           PIC X(43)  VALUE 
               ", ENTER BEGINNING ACCOUNT NUMBER, (F3-ALL):".

       01  LEGEND-ACCTNO-02.
           03  FILLER           PIC X(4)   VALUE " ^, ".
           03  FILLER           PIC X      VALUE X'76'.
           03  FILLER           PIC X(30)  VALUE 
               ", ENTER ENDING ACCOUNT NUMBER:".
       
       01  LEGEND-BR-01.
           03  FILLER         PIC X(21)  VALUE "F1-PRINTER, F7-EXIT, ".
           03  FILLER         PIC X      VALUE X'76'.
           03  FILLER         PIC X(37)  VALUE 
               ", ENTER BEGINNING BRANCH NO (F3-ALL):".

       01  LEGEND-BR-01-A.
           03  FILLER         PIC X(21)  VALUE "F1-PRINTER, F7-EXIT, ".
           03  FILLER         PIC X      VALUE X'76'.
           03  FILLER         PIC X(29)  VALUE 
               ", ENTER BEGINNING BRANCH NO.:".

       01  LEGEND-BR-02.
           03  FILLER           PIC X(3)  VALUE "^, ".
           03  FILLER           PIC X      VALUE X'76'.
           03  FILLER           PIC X(27)  VALUE 
               ", ENTER ENDING BRANCH NO. :".
       
       01  LEGEND-CLASS-01.
           03  FILLER           PIC X(13)  VALUE " F7-EXIT, ^, ".
           03  FILLER           PIC X      VALUE X'76'.
           03  FILLER           PIC X(38)  VALUE 
               ", ENTER BEGINNING LOAN CLASS (F3-ALL):".

       01  LEGEND-CLASS-02.
           03  FILLER           PIC X(3)  VALUE "^, ".
           03  FILLER           PIC X      VALUE X'76'.
           03  FILLER           PIC X(27)  VALUE
               ", ENTER ENDING LOAN CLASS :".
       
       01  LEGEND-DATE-01.
           03  FILLER           PIC X(13)  VALUE " F7-EXIT, ^, ".
           03  FILLER           PIC X      VALUE X'76'.
           03  FILLER           PIC X(32)  VALUE
               ", ENTER BEGINNING INTEREST DATE:".

       01  LEGEND-DATE-02.
           03  FILLER           PIC X(3)  VALUE "^, ".
           03  FILLER           PIC X      VALUE X'76'.
           03  FILLER           PIC X(30)  VALUE
               ", ENTER ENDING INTEREST DATE :".

       
       01  LEGEND-RESCIND-STATUS.
           03  FILLER           PIC X(3)  VALUE "^, ".
           03  FILLER           PIC X      VALUE X'76'.
           03  FILLER           PIC X(44)  VALUE
               ", ENTER (A)LL, (I)N RESCISSION OR (R)ESCIND:".

       
       01  LEGEND-PRINT-DETAIL.
           03  FILLER           PIC X(3)  VALUE "^, ".
           03  FILLER           PIC X      VALUE X'76'.
           03  FILLER           PIC X(47)  VALUE
               ", ENTER (Y)ES OR (N)O TO PRINT DETAIL ANALYSIS:".

       01  LEGEND-PRINT-BRANCH-TOTAL.
           03  FILLER           PIC X(3)  VALUE "^, ".
           03  FILLER           PIC X      VALUE X'76'.
           03  FILLER           PIC X(45)  VALUE
               ", ENTER (Y)ES OR (N)O TO PRINT BRANCH TOTALS:".


       01  LEGEND-PRINT-CORP-TOTAL.
           03  FILLER           PIC X(3)  VALUE "^, ".
           03  FILLER           PIC X      VALUE X'76'.
           03  FILLER           PIC X(48)  VALUE
               ", ENTER (Y)ES OR (N)O TO PRINT CORPORATE TOTALS:".


       01  LEGEND-NO-PRINTING.
           03  FILLER           PIC X(39)  VALUE
               "THIS SETUP DOES NOT PRINT ANY REPORT!!!".

       01  LEGEND-NO-CORP-TOTAL.
           03  FILLER           PIC X(49)  VALUE
               "CORPORATE TOTALS NOT ALLOWED ON LOCAL-SPOOLING!!!".

       01  LEGEND-LOCAL-SPOOL.
           03  FILLER           PIC X(31)  VALUE
               "SPOOL TO LOCAL DIRECTORY (Y/N):".

       01  LEGEND-ACCEPT.
           03  FILLER           PIC X(40)  VALUE
               "F7-EXIT, F1, ^, ACCEPT (Y/N):".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/RXREPT_DEF.CPY".
       COPY "LIBLP/RXREPT_WKS.CPY".
      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/RXREPT_SCN.CPY".
       
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-RX1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "RXREPT" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO END-PROGRAM.

           MOVE "N" TO RECORDS-FOUND.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       
       MOVE-BRANCH-PATH-INFO.
           IF LOCAL-PRINT-FG = "Y"
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 GO TO NEXT-GROUP-LOC.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO DL-PATH-OVERRIDE
                                              RX-PATH-OVERRIDE.

           MOVE 99 TO LN-COUNT.
           PERFORM SET-BRANCH-HEADINGS.
           PERFORM OPEN-DL1-FILE.

           PERFORM OPEN-RX1-FILE.

           MOVE RX-PATH-OWNBR        TO RX-BRNO
                                        QRX1-WBEG-BRNO
                                        QRX1-WEND-BRNO.
           MOVE BEG-ACCTNO           TO RX-ACCTNO  
                                        QRX1-WBEG-ACCTNO.
           MOVE END-ACCTNO           TO QRX1-WEND-ACCTNO.

           PERFORM START-RX1-FILE.

       NEXT-RX-REPORT.
           PERFORM READ-RX1-FILE-NEXT.
           IF ( IO-FG = 9 ) OR ( RX-BRNO NOT = RX-PATH-OWNBR )
              GO TO END-OF-GROUP.

           PERFORM SEND-PROGRESS-LINE.
           PERFORM CHECK-SET-PARAMETERS.

           IF ( WS-NEXT-RX = "X" )
              GO TO END-OF-GROUP.

           IF ( WS-NEXT-RX = "Y" )
              GO TO NEXT-RX-REPORT.

       
       PROCESS-VALID-RECORD.

           MOVE 1 TO LEV1.
           PERFORM COMPUTE-TOTALS-TABLE.

           IF ( FIRST-TIME = "Y" )
              MOVE "N"          TO FIRST-TIME
              MOVE "Y"          TO RECORDS-FOUND
              MOVE 1            TO LN-FEED
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

           IF ( PRINT-DETAIL = "Y" )
              PERFORM PRINT-DETAIL-LINE.

           GO TO NEXT-RX-REPORT.

      *=================================================================
      *
      *    END OF THE BRANCH AND/OR GROUP.
      *
      *=================================================================
       END-OF-GROUP.

           IF ( PRINT-BRANCH-TOTAL = "Y" )
              MOVE 1 TO LEV1
              PERFORM PRINT-TOTALS.

           PERFORM CLEARING-TOTALS.

           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-RX1-FILE.

           IF ( LOCAL-PRINT-FG = "Y" )
              MOVE 0 TO PAGE-NUMBER.

           MOVE "Y" TO FIRST-TIME.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           IF ( PRINT-GRAND-TOTAL = "Y" )
              MOVE 2 TO LEV1
              PERFORM PRINT-TOTALS.

           IF ( LOCAL-PRINT-FG = "N" )
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE RXREPT-QUEUE-POS TO Q-POS.
           MOVE RXREPT-COPIES-POS TO C-POS.
           MOVE RXREPT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE 0 TO PAGE-NUMBER.

           MOVE EXT-CONAME-CONAME TO H1-NAME.
           MOVE EXT-CONAME-SYSDATE TO H1-DATE.
           MOVE EXT-CONAME-USER-ID TO H2-USERID.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                   END-BRANCH.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-GROUP TO ENT-GROUP
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              MOVE "Y" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TODAY-DATE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "RESCISSION INDEX REPORT IN PROGRESS..." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF ( LOCAL-PRINT-FG NOT = "Y" )
              PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF ( VDUSTATUS = "1U" )
              GO TO PRINT-CORP-TOTAL-YN.

       BEGIN-BRANCH.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF ( VDUSTATUS = "1<" )
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BEGIN-BRANCH
              ELSE
                 MOVE "S  A000BEG-BRANCH." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGIN-BRANCH.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-BRANCH.

           IF WGR-TYPE = "A"
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

       
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

       
       ENDING-BRANCH.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGIN-BRANCH.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDING-BRANCH.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-BRANCH.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.

       
       ALL-BR.
           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BEGIN-BRANCH.

       
       BEGIN-ACCTNO.
           MOVE LEGEND-ACCTNO-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ACCTNO.
           IF VDUSTATUS = "1U" GO TO ENDING-BRANCH.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGIN-ACCTNO.
           MOVE VDUNUM0 TO BEG-ACCTNO.

       
       ENDING-ACCTNO.
           MOVE LEGEND-ACCTNO-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060END-ACCTNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGIN-ACCTNO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDING-ACCTNO.
           MOVE VDUNUM0 TO END-ACCTNO.

           IF ( BEG-ACCTNO > END-ACCTNO )
              MOVE "INVALID ACCOUNT NUMBER RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-ACCTNO.

           GO TO BEGIN-DATE.

       
       ALL-ACCTNO.
           MOVE "SNNN060BEG-ACCTNO." TO VDU.
           MOVE ZEROES               TO BEG-ACCTNO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060END-ACCTNO." TO VDU.
           MOVE 999999               TO END-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

       
       BEGIN-DATE.
           IF ( EXT-ROUTE-BUF NOT = "00" ) AND ( VDUSTATUS = "1U" )
              GO TO ENDING-ACCTNO.

           IF ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO PRINT-DETAIL-YN.

           MOVE LEGEND-DATE-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNM080BEG-DATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U" GO TO ENDING-ACCTNO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEGIN-DATE.

           IF ( VDUNUM0 = ZEROES )
              GO TO BEGIN-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

       
       ENDING-DATE.
           IF EXT-ROUTE-BUF NOT = "00" AND VDUSTATUS = "1U"
              GO TO ENDING-ACCTNO.

           MOVE LEGEND-DATE-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNM080END-DATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U" GO TO BEGIN-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENDING-DATE.

           IF ( VDUNUM0 = ZEROES )
              GO TO ENDING-DATE.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-DATE.

       
       ENTER-RESCISSION-STATUS.
           MOVE LEGEND-RESCIND-STATUS TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010RESC-STATUS." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO ENDING-DATE.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS NOT = "00" AND NOT "1D" )
              GO TO ENTER-RESCISSION-STATUS.

           IF NOT ( VDUBUF = "A" OR "I" OR "R" )
              MOVE "ENTER ONLY: 'A' 'I' OR 'R'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-RESCISSION-STATUS.

           MOVE VDUBUF TO RESCIND-STATUS.

       
       PRINT-DETAIL-YN.
           MOVE LEGEND-PRINT-DETAIL TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010PRT-DET-ANAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-RESCISSION-STATUS.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT "1D" GO TO PRINT-DETAIL-YN.

           IF ( VDUBUF NOT = "Y" )
              IF ( VDUBUF NOT = "N" )
                 GO TO PRINT-DETAIL-YN.

           MOVE VDUBUF TO PRINT-DETAIL.

       
       PRINT-BRANCH-TOTAL-YN.
           MOVE LEGEND-PRINT-BRANCH-TOTAL TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010BRANCH-TOTAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PRINT-DETAIL-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF ( VDUSTATUS NOT = "00" AND NOT "1D" )
              GO TO PRINT-BRANCH-TOTAL-YN.

           IF ( VDUBUF NOT = "Y" )
              IF ( VDUBUF NOT = "N" )
                 GO TO PRINT-BRANCH-TOTAL-YN.

           MOVE VDUBUF TO PRINT-BRANCH-TOTAL.

       
       PRINT-CORP-TOTAL-YN.
           MOVE LEGEND-PRINT-CORP-TOTAL TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010CORP-TOTAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PRINT-BRANCH-TOTAL-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF ( VDUSTATUS NOT = "00" AND NOT "1D" )
              GO TO PRINT-CORP-TOTAL-YN.

           IF ( VDUBUF NOT = "Y" )
              IF ( VDUBUF NOT = "N" )
                 GO TO PRINT-CORP-TOTAL-YN.

           MOVE VDUBUF TO PRINT-GRAND-TOTAL.

       
       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO END-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO END-PARM.

       
       ASK-LOCAL.
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "S  A250SPOOL-PROMPT."         TO VDU.
           MOVE "SPOOL TO LOCAL?  (Y/N)  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PRINT-CORP-TOTAL-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

      *=================================================================
       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

       
      *    A CHECK TO SEE IF LOCAL-SPOOLING AND CORPORATE TOTALS ARE
      *    BOTH ON.  CORPORATE TOTALS CANNOT PRINT IF THERE IS LOCAL-SP.
       
       CHECK-LOCAL-TO-CORP-TOTALS.
           IF ( LOCAL-PRINT-FG = "Y" ) AND
              ( PRINT-GRAND-TOTAL = "Y" )
              MOVE "S  A010CORP-TOTAL." TO VDU
              MOVE "N" TO VDUBUF PRINT-GRAND-TOTAL
              PERFORM SCREENIO
              MOVE LEGEND-NO-CORP-TOTAL TO MESS
              PERFORM SEND-MESS.

       
      *    CHECKING TO SEE IF THERE IS ANY TYPE OF PRINTING.
      *    IF NOT, THEY MUST RE-DO THE SETUP.
       
       CHECK-IF-PRINTING.
           IF ( PRINT-DETAIL       = "N" ) AND
              ( PRINT-BRANCH-TOTAL = "N" ) AND
              ( PRINT-GRAND-TOTAL  = "N" )
              MOVE LEGEND-NO-PRINTING TO MESS
              PERFORM SEND-MESS
              GO TO PRINT-DETAIL-YN.

       
       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
       
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BEG-BRANCH." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
       
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.
       
           MOVE "S  A060BEG-ACCTNO." TO VDU.
           MOVE BEG-ACCTNO           TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A060END-ACCTNO." TO VDU.
           MOVE END-ACCTNO           TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A010RESC-STATUS." TO VDU.
           MOVE RESCIND-STATUS        TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A010PRT-DET-ANAL." TO VDU.
           MOVE PRINT-DETAIL           TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A010BRANCH-TOTAL." TO VDU.
           MOVE PRINT-BRANCH-TOTAL     TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A010CORP-TOTAL." TO VDU.
           MOVE PRINT-GRAND-TOTAL    TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A250SPOOL-PROMPT."         TO VDU.
           MOVE "SPOOL TO LOCAL?  (Y/N)  :"    TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.
       
      ******************************************************************
       CHECK-SET-PARAMETERS SECTION.
      ******************************************************************

           MOVE "N" TO WS-NEXT-RX.
       
           IF ( RX-ACCTNO < BEG-ACCTNO )
              MOVE "Y" TO WS-NEXT-RX
              GO TO EXIT-CHECK-SET-PARM.

           IF ( RX-ACCTNO > END-ACCTNO )
              MOVE "X" TO WS-NEXT-RX
              GO TO EXIT-CHECK-SET-PARM.

           IF RX-INTDATE < BEG-DATE
              MOVE "Y" TO WS-NEXT-RX
              GO TO EXIT-CHECK-SET-PARM.

           IF RX-INTDATE > END-DATE
              MOVE "Y" TO WS-NEXT-RX
              GO TO EXIT-CHECK-SET-PARM.

       
           IF ( RESCIND-STATUS NOT = "A" )
              IF ( RESCIND-STATUS NOT = RX-STATUS )
                 MOVE "Y" TO WS-NEXT-RX
                 GO TO EXIT-CHECK-SET-PARM.


       EXIT-CHECK-SET-PARM.
           EXIT.

      ******************************************************************
       COMPUTE-TOTALS-TABLE SECTION.
      ******************************************************************
       
           PERFORM FIND-DATE-STATUS.

           IF ( DATE-TOTALS-ARREARAGE )
              ADD 1 TO
                     TT-ACCTS-COUNTER(LEV1-BRANCH, LEV2-ARREARAGE )
                     TT-ACCTS-COUNTER(LEV1-BRANCH, LEV2-RESCISSION)
                     TT-ACCTS-COUNTER(LEV1-GRAND , LEV2-ARREARAGE )
                     TT-ACCTS-COUNTER(LEV1-GRAND , LEV2-RESCISSION)

              ADD RX-PROCEEDS TO
                     TT-AMTS-PROCEEDS(LEV1-BRANCH, LEV2-ARREARAGE )
                     TT-AMTS-PROCEEDS(LEV1-BRANCH, LEV2-RESCISSION)
                     TT-AMTS-PROCEEDS(LEV1-GRAND , LEV2-ARREARAGE )
                     TT-AMTS-PROCEEDS(LEV1-GRAND , LEV2-RESCISSION)

              ADD RX-LOANAMT TO
                     TT-AMTS-LOANAMT (LEV1-BRANCH, LEV2-ARREARAGE )
                     TT-AMTS-LOANAMT (LEV1-BRANCH, LEV2-RESCISSION)
                     TT-AMTS-LOANAMT (LEV1-GRAND , LEV2-ARREARAGE )
                     TT-AMTS-LOANAMT (LEV1-GRAND , LEV2-RESCISSION)
       
           ELSE
           IF ( DATE-TOTALS-TODAY )
              ADD 1 TO
                     TT-ACCTS-COUNTER(LEV1-BRANCH, LEV2-TODAY     )
                     TT-ACCTS-COUNTER(LEV1-BRANCH, LEV2-RESCISSION)
                     TT-ACCTS-COUNTER(LEV1-GRAND , LEV2-TODAY     )
                     TT-ACCTS-COUNTER(LEV1-GRAND , LEV2-RESCISSION)

              ADD RX-PROCEEDS TO
                     TT-AMTS-PROCEEDS(LEV1-BRANCH, LEV2-TODAY     )
                     TT-AMTS-PROCEEDS(LEV1-BRANCH, LEV2-RESCISSION)
                     TT-AMTS-PROCEEDS(LEV1-GRAND , LEV2-TODAY     )
                     TT-AMTS-PROCEEDS(LEV1-GRAND , LEV2-RESCISSION)

              ADD RX-LOANAMT TO
                     TT-AMTS-LOANAMT (LEV1-BRANCH, LEV2-TODAY     )
                     TT-AMTS-LOANAMT (LEV1-BRANCH, LEV2-RESCISSION)
                     TT-AMTS-LOANAMT (LEV1-GRAND , LEV2-TODAY     )
                     TT-AMTS-LOANAMT (LEV1-GRAND , LEV2-RESCISSION)
       
           ELSE
           IF ( DATE-TOTALS-FUTURE )
              ADD 1 TO
                     TT-ACCTS-COUNTER(LEV1-BRANCH, LEV2-FUTURE    )
                     TT-ACCTS-COUNTER(LEV1-BRANCH, LEV2-RESCISSION)
                     TT-ACCTS-COUNTER(LEV1-GRAND , LEV2-FUTURE    )
                     TT-ACCTS-COUNTER(LEV1-GRAND , LEV2-RESCISSION)

              ADD RX-PROCEEDS TO
                     TT-AMTS-PROCEEDS(LEV1-BRANCH, LEV2-FUTURE    )
                     TT-AMTS-PROCEEDS(LEV1-BRANCH, LEV2-RESCISSION)
                     TT-AMTS-PROCEEDS(LEV1-GRAND , LEV2-FUTURE    )
                     TT-AMTS-PROCEEDS(LEV1-GRAND , LEV2-RESCISSION)

              ADD RX-LOANAMT  TO
                     TT-AMTS-LOANAMT (LEV1-BRANCH, LEV2-FUTURE    )
                     TT-AMTS-LOANAMT (LEV1-BRANCH, LEV2-RESCISSION)
                     TT-AMTS-LOANAMT (LEV1-GRAND , LEV2-FUTURE    )
                     TT-AMTS-LOANAMT (LEV1-GRAND , LEV2-RESCISSION)
       
           ELSE
           IF ( DATE-TOTALS-RESCIND )
              ADD 1 TO
                     TT-ACCTS-COUNTER(LEV1-BRANCH, LEV2-RESCIND)
                     TT-ACCTS-COUNTER(LEV1-GRAND , LEV2-RESCIND)

              ADD RX-PROCEEDS TO
                     TT-AMTS-PROCEEDS(LEV1-BRANCH, LEV2-RESCIND)
                     TT-AMTS-PROCEEDS(LEV1-GRAND , LEV2-RESCIND)

              ADD RX-LOANAMT  TO
                     TT-AMTS-LOANAMT (LEV1-BRANCH, LEV2-RESCIND)
                     TT-AMTS-LOANAMT (LEV1-GRAND , LEV2-RESCIND).

       EXIT-COMPUTE-TOTALS-TABLE.
           EXIT.

      ******************************************************************
       CLEARING-TOTALS SECTION.
      ******************************************************************

           PERFORM VARYING LEV2 FROM 1 BY 1 UNTIL LEV2 > 5

              MOVE 0 TO TT-ACCTS-COUNTER( LEV1-BRANCH, LEV2 )
                        TT-AMTS-PROCEEDS( LEV1-BRANCH, LEV2 )
                        TT-AMTS-LOANAMT ( LEV1-BRANCH, LEV2 )
           END-PERFORM.

       EXIT-CLEARING-TOTALS.
           EXIT.

      ******************************************************************
       SET-BRANCH-HEADINGS SECTION.
      ******************************************************************
           MOVE BR-NO     TO H3-BRNO.
           MOVE BR-NAME   TO H3-BRDESC.

      ******************************************************************
       PRINT-DETAIL-LINE SECTION.
      ******************************************************************
       
           MOVE "D"           TO WS-TOTAL-HEAD-FG.
           MOVE RX-ACCTNO TO D1-NUMBER.

           STRING RX-LNAME,", ",RX-FNAME
                  DELIMITED BY "   " INTO D1-NAME.

           MOVE RX-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D1-LOANDATE.

           MOVE RX-INTDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D1-INTDATE.

           MOVE RX-PROCEEDS TO D1-PROCEEDS.
           MOVE RX-LOANAMT  TO D1-LOANAMT.
           MOVE RX-DLNO     TO D1-DLNO.
           PERFORM GET-DEALER.
           MOVE DL-NAME     TO D1-DL-NAME.

           IF ( RX-STATUS = "I" )
              MOVE "   RESCISSION  " TO D1-RESCIND-STATUS
           ELSE
              MOVE "***RESCINDED***" TO D1-RESCIND-STATUS.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
       EXIT-PRINT-DETAIL-LINE.
           EXIT.

      ******************************************************************
       PRINT-TOTALS SECTION.
      ******************************************************************

           IF ( LEV1 = 1 )
              MOVE "B" TO WS-TOTAL-HEAD-FG
           ELSE
           IF ( LEV1 = 2 )
              MOVE "G" TO WS-TOTAL-HEAD-FG.

           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

       
           MOVE PROMPT-SUMM-UPDATE-ARREAR              TO D2-TEXT.
           MOVE TT-ACCTS-COUNTER(LEV1, LEV2-ARREARAGE) TO D2-COUNTERS.
           MOVE TT-AMTS-PROCEEDS(LEV1, LEV2-ARREARAGE) TO D2-PROCEEDS.
           MOVE TT-AMTS-LOANAMT (LEV1, LEV2-ARREARAGE) TO D2-LOANAMT.
           MOVE 1                                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE PROMPT-SUMM-UPDATE-TODAY           TO D2-TEXT.
           MOVE TT-ACCTS-COUNTER(LEV1, LEV2-TODAY) TO D2-COUNTERS.
           MOVE TT-AMTS-PROCEEDS(LEV1, LEV2-TODAY) TO D2-PROCEEDS.
           MOVE TT-AMTS-LOANAMT (LEV1, LEV2-TODAY) TO D2-LOANAMT.
           MOVE 1                                  TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE PROMPT-SUMM-UPDATE-FUTURE           TO D2-TEXT.
           MOVE TT-ACCTS-COUNTER(LEV1, LEV2-FUTURE) TO D2-COUNTERS.
           MOVE TT-AMTS-PROCEEDS(LEV1, LEV2-FUTURE) TO D2-PROCEEDS.
           MOVE TT-AMTS-LOANAMT (LEV1, LEV2-FUTURE) TO D2-LOANAMT.
           MOVE 1                                   TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE PROMPT-SUMM-UPDATE-RESCISS              TO D2-TEXT.
           MOVE TT-ACCTS-COUNTER(LEV1, LEV2-RESCISSION) TO D2-COUNTERS.
           MOVE TT-AMTS-PROCEEDS(LEV1, LEV2-RESCISSION) TO D2-PROCEEDS.
           MOVE TT-AMTS-LOANAMT (LEV1, LEV2-RESCISSION) TO D2-LOANAMT.
           MOVE 2                                       TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE PROMPT-SUMM-UPDATE-RESCIND           TO D2-TEXT.
           MOVE TT-ACCTS-COUNTER(LEV1, LEV2-RESCIND) TO D2-COUNTERS.
           MOVE TT-AMTS-PROCEEDS(LEV1, LEV2-RESCIND) TO D2-PROCEEDS.
           MOVE TT-AMTS-LOANAMT (LEV1, LEV2-RESCIND) TO D2-LOANAMT.
           MOVE 2                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-TOTALS.
           MOVE "D" TO WS-TOTAL-HEAD-FG.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
       
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF ( LN-WK GREATER THAN EXT-CONAME-RPT-LINES )
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H1-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT   TO H1-TIME.
           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE HEAD2 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE HEAD3 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           IF ( WS-TOTAL-HEAD-DETAIL ) OR ( WS-TOTAL-HEAD-RANGE )
              MOVE HEAD4 TO PR-REC
           ELSE
           IF ( WS-TOTAL-HEAD-BRANCH )
              MOVE PROMPT-HEAD-BRANCH TO H-S1-TITLE
              MOVE HEAD-SUMMARY-1     TO PR-REC
           ELSE
           IF ( WS-TOTAL-HEAD-GRAND )
              MOVE PROMPT-HEAD-GRAND TO H-S1-TITLE
              MOVE HEAD-SUMMARY-1    TO PR-REC
           ELSE
              MOVE SPACES            TO PR-REC.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           IF ( WS-TOTAL-HEAD-DETAIL ) OR ( WS-TOTAL-HEAD-RANGE )
              MOVE HEAD5 TO PR-REC
           ELSE
           IF ( WS-TOTAL-HEAD-BRANCH ) OR ( WS-TOTAL-HEAD-GRAND )
              MOVE HEAD-SUMMARY-2 TO PR-REC
           ELSE
              MOVE SPACES         TO PR-REC.

           MOVE 1 TO LN-FEED
           PERFORM WRITE-PR-REC.
       
           MOVE ALL "-" TO PR-REC.
           MOVE 1       TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE 8 TO LN-COUNT.
           MOVE 1 TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE "R"    TO WS-TOTAL-HEAD-FG.
           MOVE 99     TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
       
           IF ( GROUP-FLAG = "Y" )
              MOVE PROMPT-RANGE-BEG-BRANCH TO R-TEXT
              MOVE ENT-GROUP               TO R-NUM
              MOVE 4                       TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE PROMPT-RANGE-BEG-BRANCH TO R-TEXT
              MOVE BEG-BRANCH              TO R-NUM
              MOVE 4                       TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE PROMPT-RANGE-END-BRANCH TO R-TEXT
              MOVE END-BRANCH              TO R-NUM
              MOVE 1                       TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.
       
           MOVE PROMPT-RANGE-BEG-ACCTNO TO R-TEXT.
           MOVE BEG-ACCTNO              TO R-NUM.
           MOVE 2                       TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE PROMPT-RANGE-END-ACCTNO TO R-TEXT.
           MOVE END-ACCTNO              TO R-NUM.
           MOVE 1                       TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE PROMPT-RANGE-BEG-INTDATE TO R-TEXT.
           MOVE BEG-DATE                 TO R-DATE.
           MOVE 2                        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE PROMPT-RANGE-END-INTDATE TO R-TEXT.
           MOVE END-DATE                 TO R-DATE.
           MOVE 1                        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE PROMPT-RANGE-RESCISSION TO R-TEXT.
           MOVE RESCIND-STATUS          TO R-CHAR.
           MOVE 2                       TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE PROMPT-RANGE-PRT-DETAIL TO R-TEXT.
           MOVE PRINT-DETAIL            TO R-CHAR.
           MOVE 2                       TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE PROMPT-RANGE-PRT-BRANCH TO R-TEXT.
           MOVE PRINT-BRANCH-TOTAL      TO R-CHAR.
           MOVE 1                       TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE PROMPT-RANGE-PRT-GRAND TO R-TEXT.
           MOVE PRINT-GRAND-TOTAL      TO R-CHAR.
           MOVE 1                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       

      ********************************************
       FORM-RESET SECTION.
      ********************************************
           MOVE EXT-CONAME-CONAME TO RXREPT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO RXREPT-SYSDATE.
           DISPLAY RXREPT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE RXREPT-FORM-FIELDS TO WR-BUF.
           MOVE RXREPT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ********************************************
       SETUP-LINK-INFO SECTION.
      ********************************************
           MOVE RXREPT-HELP--FILE TO HELP-LINK-FILE.
           MOVE RXREPT-HELP--DIR  TO HELP-LINK-DIR.

      ********************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/RXREPT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       
       FIND-DATE-STATUS.

           IF RX-INTDATE < TODAY-DATE
              MOVE "A" TO DATE-TOTALS
           ELSE
           IF RX-INTDATE > TODAY-DATE
              MOVE "F" TO DATE-TOTALS
           ELSE
              MOVE "T" TO DATE-TOTALS.

           IF ( RX-STATUS = "R" )
              MOVE "R" TO DATE-TOTALS.

       
       GET-DEALER.
           MOVE RX-DLNO TO DL-DLNO.
           PERFORM READ-DL1-FILE.

           IF ( IO-FG NOT = ZEROES )
              MOVE "*** NOT FOUND ***" TO DL-NAME.

      *=================================================================
       SEND-PROGRESS-LINE.
           IF EXT-ROUTE-BUF = "00"
              STRING "RESCISSION INDEX REPORT IN PROGRESS..... ",
                  BR-NO,"-",RX-ACCTNO DELIMITED BY "  " INTO MESS
              MOVE MESS TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

       
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPRXGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPRX1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
             MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       
       END-RUN.
           PERFORM CLOSE-FILES.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND )
              IF ( EXT-ROUTE-BUF = "00" OR "70" )
                 MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
                 PERFORM SEND-MESS.

       
       EXIT-PROG.
           IF ( SV-BEG-BR NOT = 9999 )
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH
              MOVE SV-GROUP-FLAG TO GROUP-FLAG
              MOVE SV-ENT-GROUP TO ENT-GROUP.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY RXREPT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
