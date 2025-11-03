       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CONVAL.
       DATE-WRITTEN.  060113.
      ******************************************************************
      *
      * IDENTIFICATION:  LP/CONVAL.C
      *
      * DESCRIPTION   :  LOAN CONVERSION REPORT - ALPHA ORDER, NO BREAKS
      *                  FOR LN-CONVERCD = "Y" ACCOUNTS ONLY
      *    DOES NOT CROSS PATHS
      *    ALPHA ORDER
      *
      * THIS REPORT IS SUPPOSE TO BE A SNAPSHOT OF HOW THE ACCOUNTS LOOKED
      * WHEN THEY WERE CONVERTED....USE THE TR"K" RECORD FROM CONVERSION
      * BUT THE LOAN FILE IS THE DRIVER...
      *
      * 7/26/05  LOOKED AT A PROBLEM AT WORLD, ACCT WAS CONVERTED, THEN
      * DELETED & REENTERED WITH AN OLD TRDATE, IN A DIFFERENT CLASS
      * AS A P&L, DIDN'T SHOW IN P&L TOTALS.
      *
      * REV:
      *060113 BAH 060113 NEW FOR WORLD PR# 451
      *090812 CS JIM CALLED REGARDING 'NO BF' BUT THERE WAS UNEARNED ON
      *          REPORT. ALSO IF THERE WAS A BF & THEN A REVERSAL, ONLY THE
      *          REVERSAL WAS ADDED TO TOTALS, BUT UNEARNED ON DETAIL LINE
      *          WAS NOT SIGNED AND ALWAYS APPEARED POSITIVE.  IF THERE WAS
      *          NO 'BF' ON THE ACCOUNT, GOT THE UNEARNED FROM THE LOAN REC.
      *          WE FIXED THIS IN 2006 ON CONVRP... SAME COMPLAINT. STILL
      *          HAVE 'BAD' NUMBERS IN LN-ENTRY DATE IN ONE MONTH & BALFWD
      *          IN ANOTHER   WORLD PR#662
      *090819 CS USE THE LN-ENTRYDATE RANGE FOR BOTH LOAN AND TRFILE. ALLOW A
      *          SPAN OF MONTHS FOR THE TRFILE.  IF AN ACCOUNT HAS A BF BUT
      *          THE BF DID NOT HAPPPEN WITH THE DATE RANGE ON THE SCREEN PRINT
      *          A MESSAGE THAT 'BF OUT OF DATE RANGE'AND EXCLUDE FROM BALANCE
      *          AND UNEARNED TOTALS WORLD PR#662
      * BAH 160125 REMOVED LTFILE PD0003
      * BAH 170517 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181205 GLOBAL LNFILE, REMOVED ARCHIVE
      * BAH 190116 GLOBAL LXFILE
      * BAH 20190920 REMOVED ACCESS-CALL ON TRKFILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-LNAME         PIC X(16).
               05  WK-FNAME         PIC X(16).
               05  WK-ACCTNO        PIC 9(8).

           03  WK-OWNBR             PIC 99.
           03  WK-STAR              PIC X.
           03  WK-LOAN-NUM          PIC X(13).
           03  WK-ORIGBAL           PIC S9(7)V99 COMP-3.
           03  WK-CURBAL            PIC S9(7)V99 COMP-3.
           03  WK-PURBAL            PIC S9(7)V99 COMP-3.
           03  WK-UNEARN-TABLE.
               05  WK-UNEARN        PIC S9(7)V99 OCCURS 7.
           03  WK-UNEARNX REDEFINES WK-UNEARN-TABLE.
               05  WK-UNEARN1           PIC S9(7)V99.
               05  WK-UNEARN2           PIC S9(7)V99.
               05  WK-UNEARN3           PIC S9(7)V99.
               05  WK-UNEARN4           PIC S9(7)V99.
               05  WK-UNEARN5           PIC S9(7)V99.
               05  WK-UNEARN6           PIC S9(7)V99.
               05  WK-UNEARN7           PIC S9(7)V99.
           03  WK-PLCD              PIC X.
           03  WK-USED-LP-REC       PIC X(01).
           03  WK-MISSING-BF        PIC X(01).

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/CONVAL.CBL J35 12/21/23-08:58:44 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01TRK.CPY".
       COPY "LIBLP/LP01TRK_SQL.CPY".
       COPY "LIBLP/LPWSTRK.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LXE.CPY".
       COPY "LIBLP/LP01LXE_SQL.CPY".
       COPY "LIBLP/LPWSLXE.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
      *-----------------------------------------------------------------
       01  WK-IFN                      PIC XX        VALUE "WK".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  FIRST-TIME                  PIC X(01)     VALUE "Y".
       01  FIRST-TIME-BRANCH           PIC X(01)     VALUE "Y".
       01  LEGEND                      PIC X(80)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "MESS:B.".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LN-FEED                 PIC 9(02)     VALUE ZEROES.
       01  LN-COUNT                PIC 9(03)     VALUE 99.
       01  PAGE-NUMBER             PIC 9(04)     VALUE ZEROES.

       01  RECORDS-FOUND               PIC X(01)     VALUE "X".
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.

       01  SUMMARY-FG                  PIC X(01)     VALUE "N".

       78  CU-MAX                                    VALUE 7.
       77  CU-SUB                      PIC 9(01)     VALUE ZEROES.
       01  FOUND-TK                    PIC X VALUE "N".

      *-----------------------------------------------------------------
       01  CURRENT-CURBAL          PIC S9(9)V99.

       01  CURRENT-LOAN-LC-ACCTNO  PIC X(16).
       01  FILLER                  REDEFINES CURRENT-LOAN-LC-ACCTNO.
           03  CURRENT-LOAN-LC     PIC X(03).
           03  CURRENT-LOAN-ACCTNO PIC X(13).

       01  CURRENT-ORIGBAL         PIC S9(7)V99.
       01  CURRENT-PURBAL          PIC S9(7)V99.
       01  CURRENT-UNEARN          PIC S9(7)V99 OCCURS CU-MAX TIMES.
       01  CURRENT-PLCD            PIC X.
       01  CURRENT-MISSING-BF      PIC X.

      *=================================================================
      *
      *    T O T A L - W O R K E R S
      *
      *-----------------------------------------------------------------
      *    TT = TOTALS
      *    TU = TOTAL UNEARN  (1-6 IS TRK UNEARN; 7 IS TOTAL OF 1-6)
      *=================================================================
       78  TT-MAX                                    VALUE 2.
       77  TT-SUB                      PIC 9(01)     VALUE ZEROES.
       78  TU-MAX                                    VALUE 7.
       77  TU-SUB                      PIC 9(01)     VALUE ZEROES.

       01  TOTAL-MISSING-BF            PIC 9(7) VALUE 0.
      *-----------------------------------------------------------------
       01  ACTIVE-TABLE                OCCURS TT-MAX TIMES.
           03  A-NO                    PIC S9(5).
           03  A-CURBAL                PIC S9(9)V99.
           03  A-PURBAL                PIC S9(7)V99.
           03  A-ORIGBAL               PIC S9(7)V99.
           03  A-UNEARN                PIC S9(7)V99 OCCURS TU-MAX TIMES.

       01  PL-TABLE                    OCCURS TT-MAX TIMES.
           03  P-NO                    PIC S9(5).
           03  P-CURBAL                PIC S9(9)V99.
           03  P-PURBAL                PIC S9(7)V99.
           03  P-ORIGBAL               PIC S9(7)V99.
           03  P-UNEARN                PIC S9(7)V99 OCCURS TU-MAX TIMES.

       01  TOTAL-TABLE                 OCCURS TT-MAX TIMES.
           03  T-NO                    PIC S9(5).
           03  T-CURBAL                PIC S9(9)V99.
           03  T-PURBAL                PIC S9(7)V99.
           03  T-ORIGBAL               PIC S9(7)V99.
           03  T-UNEARN                PIC S9(7)V99 OCCURS TU-MAX TIMES.

      ******************************************************************
      *
      *    S C R E E N - R A N G E S - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-DATE                PIC 9(08).
           03  END-DATE                PIC 9(08).
           03  ZBAL-ISO                PIC X(01).
               88  ZBAL-ISO-INCLUDE        VALUE "I".
               88  ZBAL-ISO-SKIP           VALUE "S".
               88  ZBAL-ISO-ONLY           VALUE "O".
               88  ZBAL-ISO-VALID          VALUE "I" "S" "O".
           03  SUM-ONLY                PIC X(01).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

      ******************************************************************
      *
      *    H E A D E R - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-01.
           03  H01-DATE                PIC X(08)     VALUE SPACES.
           03  FILLER                  PIC X(02)     VALUE SPACES.
           03  H01-HR                  PIC 9(02)     VALUE ZEROES.
           03  FILLER                  PIC X(01)     VALUE ":".
           03  H01-MIN                 PIC 9(02)     VALUE ZEROES.
           03  FILLER                  PIC X(30)     VALUE SPACES.
           03  H01-NAME                PIC X(40)     VALUE SPACES.
           03  FILLER                  PIC X(37)     VALUE SPACES.
           03  FILLER                  PIC X(05)     VALUE "PAGE ".
           03  H01-PAGE-NO             PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10)     VALUE "USER ID:".
           03  H02-USERID              PIC X(12)     VALUE SPACES.
           03  FILLER                  PIC X(34)     VALUE SPACES.
           03  FILLER                  PIC X(18)     VALUE
                                                 "CONVERTED ACCOUNTS".

      *--------------------------------------------------{ DETAIL LINE }
       01  HEADER-LINE-D01.
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(30) VALUE "     ORIGINAL BALANCE AT    CU".
           03  FILLER PIC X(30) VALUE "RRENT   INTEREST   SER. CHG  M".
           03  FILLER PIC X(30) VALUE "AINT. FEE  DL. DISC     OTHER ".
           03  FILLER PIC X(12) VALUE "      TOTAL ".

      *--------------------------------------------------{ DETAIL LINE }
       01  HEADER-LINE-D02.
           03  FILLER PIC X(30) VALUE "ACCOUNT       NAME            ".
           03  FILLER PIC X(30) VALUE "         LOAN   PURCHASE    BA".
           03  FILLER PIC X(30) VALUE "LANCE   UNEARNED   UNEARNED   ".
           03  FILLER PIC X(30) VALUE "UNEARNED   UNEARNED   UNEARNED".
           03  FILLER PIC X(12) VALUE "   UNEARNED ".

      *-------------------------------------------------{ SUMMARY LINE }
       01  HEADER-LINE-S01.
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(30) VALUE "     ORIGINAL BALANCE AT    CU".
           03  FILLER PIC X(30) VALUE "RRENT   INTEREST   SER. CHG  M".
           03  FILLER PIC X(30) VALUE "AINT. FEE  DL. DISC     OTHER ".
           03  FILLER PIC X(12) VALUE "      TOTAL ".

      *-----------------------------------------------------------------
       01  HEADER-LINE-S02.
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(30) VALUE "         LOAN   PURCHASE    BA".
           03  FILLER PIC X(30) VALUE "LANCE   UNEARNED   UNEARNED   ".
           03  FILLER PIC X(30) VALUE "UNEARNED   UNEARNED   UNEARNED".
           03  FILLER PIC X(12) VALUE "   UNEARNED ".

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE                 PIC X(132)    VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01              REDEFINES DETAIL-LINE.
           03  D01-LOAN-NUM            PIC X(13).
           03  FILLER                  PIC X(01).
           03  D01-LNAME               PIC X(18).
           03  D01-STAR                PIC X(01).
           03  D01-ORIGBAL             PIC ZZZZZZ9.99.
           03  D01-BALANCES            PIC X(23).
           03  D01-BAL REDEFINES D01-BALANCES.
               05  FILLER              PIC X(01).
               05  D01-CURBAL          PIC ZZZZZZ9.99.
               05  D01-CURBALX REDEFINES D01-CURBAL PIC X(10).
               05  FILLER              PIC X(01).
               05  D01-PURBAL          PIC ZZZZZZ9.99-.

           03  D01-UNEARN-1            PIC ZZZZZZ9.99-.
           03  D01-UNEARN-3            PIC ZZZZZZ9.99-.
           03  D01-UNEARN-4            PIC ZZZZZZ9.99-.
           03  D01-UNEARN-5            PIC ZZZZZZ9.99-.
           03  D01-UNEARN-6            PIC ZZZZZZ9.99-.
           03  D01-UNEARN-TOTAL        PIC ZZZZZZ9.99-.
           03  D01-USED-LP-REC         PIC X(01).

      *-----------------------------------------------------------------
       01  SUMMARY-LINE-01             REDEFINES DETAIL-LINE.
           03  S01-SUMNAME             PIC X(16).
           03  S01-COUNT               PIC Z(06).
           03  FILLER                  PIC X(11).
           03  S01-ORIGBAL             PIC ZZZZZZ9.99.
           03  FILLER                  PIC X(01).
           03  S01-CURBAL              PIC ZZZZZZ9.99.
           03  FILLER                  PIC X(01).
           03  S01-PURBAL              PIC ZZZZZZ9.99.
           03  FILLER                  PIC X(01).
           03  S01-UNEARN-1            PIC ZZZZZZ9.99.
           03  FILLER                  PIC X(01).
           03  S01-UNEARN-3            PIC ZZZZZZ9.99.
           03  FILLER                  PIC X(01).
           03  S01-UNEARN-4            PIC ZZZZZZ9.99.
           03  FILLER                  PIC X(01).
           03  S01-UNEARN-5            PIC ZZZZZZ9.99.
           03  FILLER                  PIC X(01).
           03  S01-UNEARN-6            PIC ZZZZZZ9.99.
           03  FILLER                  PIC X(01).
           03  S01-UNEARN-TOTAL        PIC ZZZZZZ9.99.
           03  FILLER                  PIC X(01).

      *-----------------------------------------------------------------
       01  RANGE-LINE-01               REDEFINES DETAIL-LINE.
           03  R01-PROMPT              PIC X(32).
           03  R01-TEXT                PIC X(100).
           03  R01-DATE                REDEFINES R01-TEXT
                                       PIC 9999/99/99.
           03  R01-BRNO                REDEFINES R01-TEXT
                                       PIC 9(4).
      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".

       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CONVAL_DEF.CPY".
       COPY "LIBLP/CONVAL_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/CONVAL_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRK1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

      D    STOP MESS.
           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO END-PROGRAM.

           PERFORM PRIMARY-DRIVER.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CONVAL" TO VDU-FORM-NAME.

           MOVE CONVAL-QUEUE-POS TO Q-POS.
           MOVE CONVAL-COPIES-POS TO C-POS.
           MOVE CONVAL-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE "N" TO RECORDS-FOUND.

           IF EXT-ROUTE-BUF = "00"
              MOVE "CONVERTED ACCOUNTS IN PROGRESS......" TO LEGEND
              PERFORM SEND-LEGEND.

           PERFORM INITIAL-PROGRESS-INDICATOR.

           PERFORM CLEAR-ALL-TOTALS.

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE EXT-CONAME-CONAME  TO H01-NAME.
           MOVE EXT-CONAME-SYSDATE TO H01-DATE.
           MOVE EXT-CONAME-USER-ID TO H02-USERID.

           PERFORM OPEN-PR-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

           IF ( UP-KEY )
              GO TO ENTER-SUM-ONLY.

      *=================================================================
       ENTER-BEG-DATE.

           IF ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO ENTER-ZBAL-ISO.

           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           IF ( UP-KEY                   ) AND
              ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO ENTER-ZBAL-ISO.

           MOVE "ERNM060DT2." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

           MOVE VDU-DATE-YYYYMMDD  TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-ZBAL-ISO.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "S  8080DT1." TO VDU.
           MOVE EXT-JULIAN-BEG    TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080DT2." TO VDU.
           MOVE EXT-JULIAN-END    TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-ZBAL-ISO.

           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-ZBAL-ISO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO ZBAL-ISO.

           IF NOT ( ZBAL-ISO-VALID )
              GO TO ENTER-ZBAL-ISO.

      *-----------------------------------------------------------------
       ENTER-SUM-ONLY.

           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-ZBAL-ISO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-SUM-ONLY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF NOT ( VDUBUF = "Y" OR "N" )
              GO TO ENTER-SUM-ONLY.

           MOVE VDUBUF TO SUM-ONLY.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           MOVE "S  8080DT1." TO VDU.
           MOVE BEG-DATE      TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080DT2." TO VDU.
           MOVE END-DATE      TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL-ISO       TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY          TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
       PRIMARY-DRIVER SECTION.
      ******************************************************************

           PERFORM PROCESS-BRANCH.

           PERFORM PROCESS-WORK-FILE.

       PRIMARY-DRIVER-END.

           IF ( NO-RECORDS-FOUND )
              GO TO PRIMARY-DRIVER-EXIT.

           PERFORM PRINT-GRAND-TOTALS.
           PERFORM PRINT-RANGE-LINES.

       PRIMARY-DRIVER-EXIT.
           EXIT.

      ***************************
       PROCESS-BRANCH SECTION.
      ***************************

           MOVE 9999         TO PROGID-SLEEP-COUNTER.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-LXE1-FILE.
           PERFORM OPEN-LN3-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-CLASS LN-ACCTNO.

           PERFORM START-LN3-FILE.

       NEXT-LN3-RECORD.

           PERFORM READ-LN3-FILE-NEXT.
           IF ( IO-BAD ) OR ( LN-OWNBR NOT = LN-PATH-OWNBR )
              GO TO PROCESS-BRANCH-END.

           PERFORM SEND-PROGRESS-INDICATOR.

           IF ( LN-CONVERCD NOT = "Y"  ) OR
              ( LN-POCD         = "**" )
              GO TO NEXT-LN3-RECORD.

           IF ( ZBAL-ISO-ONLY )
              IF ( LN-CURBAL NOT = ZEROES ) OR
                 ( LN-INTBAL NOT = ZEROES ) OR
                 ( LN-LCBAL  NOT = ZEROES ) OR
                 ( LN-OTHBAL NOT = ZEROES )
                 GO TO NEXT-LN3-RECORD.

           IF ( ZBAL-ISO-SKIP      ) AND
              ( LN-CURBAL = ZEROES ) AND
              ( LN-INTBAL = ZEROES ) AND
              ( LN-LCBAL  = ZEROES ) AND
              ( LN-OTHBAL = ZEROES )
              GO TO NEXT-LN3-RECORD.

      * VERIFY BETWEEN BEGINNING AND ENDING DATE
           IF LN-ENTDATE < BEG-DATE OR LN-ENTDATE > END-DATE
              GO TO NEXT-LN3-RECORD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO WK-REC.
           MOVE 0 TO WK-ACCTNO WK-ORIGBAL WK-OWNBR
                     WK-ORIGBAL WK-CURBAL WK-PURBAL
                     WK-UNEARN1 WK-UNEARN3 WK-UNEARN4
                     WK-UNEARN5 WK-UNEARN6 WK-UNEARN7.

           PERFORM GET-BR-RECORD.
           PERFORM GET-LP1-RECORD.

           IF WK-MISSING-BF = "Y"
              PERFORM VARYING CU-SUB FROM 1 BY 1 UNTIL CU-SUB > 7
                 MOVE 0 TO CURRENT-UNEARN(CU-SUB)
              END-PERFORM
              GO TO MISSING-BF-SKIP.

           PERFORM GET-LXE1-RECORD.
           PERFORM GET-SP1-RECORD.
           PERFORM GET-TRK-RECORD.

       MISSING-BF-SKIP.
      *%FOUND A BF BUT NOT WITHIN DATE RANGE
           IF FOUND-TK = "N"
              IF NOT (WK-MISSING-BF = "Y")
                 MOVE "*"       TO WK-STAR.

           PERFORM LOAD-CURRENT-INFORMATION.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF ( IO-BAD )
               MOVE "***NOT ON FILE***"    TO WK-LNAME
               MOVE SPACES TO WK-FNAME
           ELSE
               MOVE BW-FNAME TO WK-FNAME
               MOVE BW-LNAME               TO WK-LNAME.

           MOVE LN-ACCTNO           TO WK-ACCTNO.
           MOVE CURRENT-LOAN-ACCTNO TO WK-LOAN-NUM.
           MOVE LN-OWNBR TO WK-OWNBR.


           MOVE CURRENT-ORIGBAL TO WK-ORIGBAL.
           MOVE CURRENT-CURBAL TO WK-CURBAL.
           MOVE CURRENT-PURBAL TO WK-PURBAL.
           MOVE CURRENT-UNEARN(1) TO WK-UNEARN1.
           MOVE CURRENT-UNEARN(3) TO WK-UNEARN3.
           MOVE CURRENT-UNEARN(4) TO WK-UNEARN4.
           MOVE CURRENT-UNEARN(5) TO WK-UNEARN5.
           MOVE CURRENT-UNEARN(6) TO WK-UNEARN6.
           MOVE CURRENT-UNEARN(7) TO WK-UNEARN7.

           MOVE CURRENT-PLCD TO WK-PLCD.

           PERFORM WRITE-WK-FILE.

           GO TO NEXT-LN3-RECORD.


       PROCESS-BRANCH-END.

           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LXE1-FILE.

       PROCESS-BRANCH-EXIT.
           EXIT.

      **************************
       PROCESS-WORK-FILE SECTION.
      **************************

           MOVE "Y"          TO FIRST-TIME-BRANCH.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           PERFORM CLOSE-WK-FILE.

           PERFORM OPEN-WK-FILE.
           MOVE SPACES TO WK-LNAME WK-FNAME.
           MOVE 0      TO WK-ACCTNO.
           PERFORM START-WK-FILE.
       PROCESS-NEXT-WK.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO PROCESS-WORK-EXIT.

           IF ( FIRST-TIME = "Y" )
              MOVE WK-OWNBR TO BR-NO
              PERFORM GET-BR-RECORD
              IF ( IO-BAD )
                 GO TO PROCESS-NEXT-WK.

           PERFORM PRINT-DETAIL-LINE.
           PERFORM UPDATE-TOTALS.

           MOVE "Y" TO RECORDS-FOUND.

           GO TO PROCESS-NEXT-WK.

       PROCESS-WORK-EXIT.
           EXIT.

      ******************************************************************
       GET-TRK-RECORD SECTION.
      ******************************************************************
           MOVE "N" TO FOUND-TK.

           PERFORM VARYING CU-SUB FROM 1 BY 1 UNTIL CU-SUB > 7
               MOVE 0 TO CURRENT-UNEARN(CU-SUB)
           END-PERFORM.
           MOVE SPACES TO CURRENT-PLCD.

           PERFORM OPEN-TRK1-FILE.

           MOVE LN-OWNBR            TO TK-BRNO
                                       QTRK1-WBEG-BRNO
                                       QTRK1-WEND-BRNO.
           MOVE LN-ENTDATE          TO TK-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRK1-WBEG-DATE.
           MOVE END-DATE            TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRK1-WEND-DATE.
           MOVE LN-CLASS            TO TK-CLASS
                                       QTRK1-WBEG-CLASS.
           MOVE ALL "9"             TO QTRK1-WEND-CLASS.
           MOVE LN-ACCTNO           TO TK-ACCTNO
                                       QTRK1-WBEG-ACCTNO
                                       QTRK1-WEND-ACCTNO.

           MOVE 0                   TO TK-LAWCODE TK-LINE
                                       QTRK1-WBEG-LAWCODE
                                       QTRK1-WBEG-LINE.
           MOVE ALL "9"             TO QTRK1-WEND-LAWCODE
                                       QTRK1-WEND-LINE.

           PERFORM START-TRK1-FILE.

       GET-TRK-RECORD-NEXT.
           PERFORM READ-TRK1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRK-PATH-OWNBR NOT = TK-BRNO)
              IF FOUND-TK = "N"
                 PERFORM CLEAR-TRK-FILE
                 GO TO GET-TRK-RECORD-EXIT
              ELSE
      * IF END OF FILE, BUT YOU HAD A GOOD TRK RECORD, DONT CLEAR
      * THE ACCOUNT NUMBER OUT OF THE TK OR IT WILL GO USE LOAN RECORD
      * FOR CURRENT INFO....
                 GO TO GET-TRK-RECORD-EXIT.

           IF ( TK-BRNO   NOT = LN-OWNBR  ) OR
              ( TK-ACCTNO NOT = LN-ACCTNO )
              GO TO GET-TRK-RECORD-NEXT.

           IF TK-DATE > END-DATE
              IF FOUND-TK = "N"
                 PERFORM CLEAR-TRK-FILE
                 GO TO GET-TRK-RECORD-EXIT
              ELSE
                 GO TO GET-TRK-RECORD-EXIT.

           MOVE "Y" TO FOUND-TK.

      * VALID 'TRK' RECORD

           PERFORM VARYING CU-SUB FROM 1 BY 1 UNTIL CU-SUB > 6
                   ADD TK-UNEARN(CU-SUB) TO CURRENT-UNEARN(CU-SUB)
                   ADD TK-UNEARN(CU-SUB) TO CURRENT-UNEARN(CU-MAX)
           END-PERFORM.

           MOVE TK-PLCD TO CURRENT-PLCD.

      * MIGHT BE A REVERAL, NEW, REVERSAL, NEW, ETC...KEEP READING
           GO TO GET-TRK-RECORD-NEXT.

      * WHEN YOU END, YOU WILL HAVE THE MOST CURRENT TRK

       GET-TRK-RECORD-EXIT.
           PERFORM CLOSE-TRK1-FILE.

      ******************************************************************
       LOAD-CURRENT-INFORMATION SECTION.
      ******************************************************************

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO          TO CURRENT-LOAN-LC-ACCTNO.
           MOVE LN-LNAMT                  TO CURRENT-ORIGBAL.
           IF WK-STAR  = "*"
               MOVE ZEROS                 TO CURRENT-PURBAL
                                             CURRENT-CURBAL
           ELSE
               MOVE LN-CURBAL             TO CURRENT-PURBAL
               MOVE LP-CURBAL             TO CURRENT-CURBAL.


           GO TO LOAD-CURRENT-INFO-EXIT.

      *=================================================================
      *    THIS IS THE DEFAULT IF UNABLE TO GET THE 'TRK' RECORD.
      *=================================================================
       LOAD-CURRENT-INFO-DEFAULT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    COMPUTE UNEARNED AT TIME OF PURCHASE:
      *     UNEARNED = ANTICERN + ANTICADG - EARNED
      *     NOTE:
      *         COMPUTATION OF THE UNEARNED IS ONLY CORRECT IF THE LOAN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *CS  PERFORM VARYING CU-SUB FROM 1 BY 1 UNTIL CU-SUB > CU-MAX
      *CS          MOVE ZEROES TO CURRENT-UNEARN(CU-SUB)
      *CS  END-PERFORM.

      *CS  MOVE LN-PLCD TO CURRENT-PLCD.

      *CS  PERFORM VARYING CU-SUB FROM 1 BY 1 UNTIL CU-SUB > 6

      *CS        ADD LN-ANTICERN(CU-SUB) TO CURRENT-UNEARN(CU-SUB)
      *CS                                   CURRENT-UNEARN(CU-MAX)

      *CS        ADD LN-ANTICADJ(CU-SUB) TO CURRENT-UNEARN(CU-SUB)
      *CS                                   CURRENT-UNEARN(CU-MAX)

      *CS        IF ( LN-EARN-ORDER(CU-SUB) > 3 )
      *CS           COMPUTE SUBX = LN-EARN-ORDER(CU-SUB) - 3
      *CS           SUBTRACT LXE-EARN(SUBX) FROM CURRENT-UNEARN(CU-SUB)
      *CS           SUBTRACT LXE-EARN(SUBX) FROM CURRENT-UNEARN(CU-MAX)
      *CS        ELSE
      * LN-EARN-ORDER IS 0 IF BF HASNT BEEN DONE YET !
      *CS           IF LN-EARN-ORDER(CU-SUB) NOT = 0
      *CS              MOVE LN-EARN-ORDER(CU-SUB) TO SUBX
      *CS              SUBTRACT LP-EARNED(SUBX) FROM
      *CS                                        CURRENT-UNEARN(CU-SUB)
      *CS              SUBTRACT LP-EARNED(SUBX) FROM
      *CS                                        CURRENT-UNEARN(CU-MAX)
      *CS           END-IF
      *CS        END-IF

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * JTG 000614 CORRECTED LOGIC TO CORRECT EARNINGS FOR A COMPONET OF
      *             INCOME FROM LP-REC / LXE-REC                   #1203
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *            IF ( SUB > 3 )
      *               SUBTRACT LXE-EARN(SUB - 3) FROM HOLD-UNEARN
      *            ELSE
      *               SUBTRACT LP-EARNED(SUB)    FROM HOLD-UNEARN
      *JTG-------> END-IF

      *CS  END-PERFORM.

       LOAD-CURRENT-INFO-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-TOTALS SECTION.
      ******************************************************************

           MOVE ZEROES TO TT-SUB.

       UPDATE-TOTALS-NEXT.

           ADD 1 TO TT-SUB.

           IF ( TT-SUB > TT-MAX )
              GO TO UPDATE-TOTALS-EXIT.

           IF WK-PLCD NOT = SPACES
              GO TO UPDATE-TOTALS-PL
           ELSE
              GO TO UPDATE-TOTALS-ACTIVE.

      *---------------------------------------------{ RECORD IS ACTIVE }
       UPDATE-TOTALS-ACTIVE.

           ADD 1                            TO A-NO     (TT-SUB).
           ADD WK-ORIGBAL              TO A-ORIGBAL(TT-SUB).
           ADD WK-CURBAL               TO A-CURBAL (TT-SUB).
           ADD WK-PURBAL               TO A-PURBAL (TT-SUB).

           PERFORM VARYING TU-SUB FROM 1 BY 1 UNTIL TU-SUB > TU-MAX
                 ADD WK-UNEARN(TU-SUB) TO A-UNEARN (TT-SUB TU-SUB)
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           GO TO UPDATE-TOTALS-TOTAL.

      *------------------------------------------------{ RECORD IS P/L }
       UPDATE-TOTALS-PL.

           ADD 1                            TO P-NO     (TT-SUB).
           ADD WK-ORIGBAL              TO P-ORIGBAL(TT-SUB).
           ADD WK-CURBAL               TO P-CURBAL (TT-SUB).
           ADD WK-PURBAL               TO P-PURBAL (TT-SUB).

           PERFORM VARYING TU-SUB FROM 1 BY 1 UNTIL TU-SUB > TU-MAX
                 ADD WK-UNEARN(TU-SUB) TO P-UNEARN (TT-SUB TU-SUB)
           END-PERFORM.

      *-----------------------------------------------------------------
       UPDATE-TOTALS-TOTAL.

           ADD 1                            TO T-NO     (TT-SUB).
           ADD WK-ORIGBAL              TO T-ORIGBAL(TT-SUB).
           ADD WK-CURBAL               TO T-CURBAL (TT-SUB).
           ADD WK-PURBAL               TO T-PURBAL (TT-SUB).

           PERFORM VARYING TU-SUB FROM 1 BY 1 UNTIL TU-SUB > TU-MAX
                 ADD WK-UNEARN(TU-SUB) TO T-UNEARN (TT-SUB TU-SUB)
           END-PERFORM.

           GO TO UPDATE-TOTALS-NEXT.

       UPDATE-TOTALS-EXIT.
           EXIT.

      ******************************************************************
       PRINT-DETAIL-LINE SECTION.
      ******************************************************************

           IF ( SUM-ONLY = "Y" )
              GO TO PRINT-DETAIL-LINE-EXIT.

           MOVE WK-LOAN-NUM TO D01-LOAN-NUM.
           IF WK-PLCD = "P"
              INSPECT D01-LOAN-NUM REPLACING FIRST " " BY "P".

           STRING WK-LNAME,", ",WK-FNAME DELIMITED BY
                   "   " INTO D01-LNAME.

           MOVE WK-STAR               TO D01-STAR.
           MOVE WK-ORIGBAL            TO D01-ORIGBAL.
           MOVE WK-CURBAL             TO D01-CURBAL.
           IF WK-MISSING-BF = "Y"
             MOVE "* NO BF * " TO D01-CURBALX
           ELSE
           IF WK-STAR = "*"
             MOVE " *BF DATE OUT OF RANGE*"  TO D01-BALANCES.

           IF WK-STAR NOT  = "*"
           MOVE WK-PURBAL             TO D01-PURBAL.

           MOVE WK-UNEARN(1)          TO D01-UNEARN-1.
           MOVE WK-UNEARN(3)          TO D01-UNEARN-3.
           MOVE WK-UNEARN(4)          TO D01-UNEARN-4.
           MOVE WK-UNEARN(5)          TO D01-UNEARN-5.
           MOVE WK-UNEARN(6)          TO D01-UNEARN-6.

           MOVE WK-UNEARN(7)          TO D01-UNEARN-TOTAL.

           IF ( WK-USED-LP-REC = "Y")
              MOVE "*"                TO D01-USED-LP-REC.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-DETAIL-LINE-EXIT.
           EXIT.

      *************************************
       PRINT-GRAND-TOTALS SECTION.
      *************************************

           MOVE "Y" TO SUMMARY-FG.
           MOVE 2   TO TT-SUB.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DO NOT LET TOTALS WRAP END OF PAGE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF (( LN-COUNT + 10 ) > EXT-CONAME-RPT-LINES )
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "GRAND TOTALS:"            TO DETAIL-LINE.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE " # P&L    ACCTS:"         TO S01-SUMNAME.
           MOVE P-NO     (TT-SUB)          TO S01-COUNT.
           MOVE P-ORIGBAL(TT-SUB)          TO S01-ORIGBAL.
           MOVE P-CURBAL (TT-SUB)          TO S01-CURBAL.
           MOVE P-PURBAL (TT-SUB)          TO S01-PURBAL.
           MOVE P-UNEARN (TT-SUB 1)        TO S01-UNEARN-1.
           MOVE P-UNEARN (TT-SUB 3)        TO S01-UNEARN-3.
           MOVE P-UNEARN (TT-SUB 4)        TO S01-UNEARN-4.
           MOVE P-UNEARN (TT-SUB 5)        TO S01-UNEARN-5.
           MOVE P-UNEARN (TT-SUB 6)        TO S01-UNEARN-6.
           MOVE P-UNEARN (TT-SUB TU-MAX)   TO S01-UNEARN-TOTAL.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE " # ACTIVE ACCTS:"         TO S01-SUMNAME.
           MOVE A-NO     (TT-SUB)          TO S01-COUNT.
           MOVE A-ORIGBAL(TT-SUB)          TO S01-ORIGBAL.
           MOVE A-CURBAL (TT-SUB)          TO S01-CURBAL.
           MOVE A-PURBAL (TT-SUB)          TO S01-PURBAL.
           MOVE A-UNEARN (TT-SUB 1)        TO S01-UNEARN-1.
           MOVE A-UNEARN (TT-SUB 3)        TO S01-UNEARN-3.
           MOVE A-UNEARN (TT-SUB 4)        TO S01-UNEARN-4.
           MOVE A-UNEARN (TT-SUB 5)        TO S01-UNEARN-5.
           MOVE A-UNEARN (TT-SUB 6)        TO S01-UNEARN-6.
           MOVE A-UNEARN (TT-SUB TU-MAX)   TO S01-UNEARN-TOTAL.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE " TOTALS        :"         TO S01-SUMNAME.
           MOVE T-NO     (TT-SUB)          TO S01-COUNT.
           MOVE T-ORIGBAL(TT-SUB)          TO S01-ORIGBAL.
           MOVE T-CURBAL (TT-SUB)          TO S01-CURBAL.
           MOVE T-PURBAL (TT-SUB)          TO S01-PURBAL.
           MOVE T-UNEARN (TT-SUB 1)        TO S01-UNEARN-1.
           MOVE T-UNEARN (TT-SUB 3)        TO S01-UNEARN-3.
           MOVE T-UNEARN (TT-SUB 4)        TO S01-UNEARN-4.
           MOVE T-UNEARN (TT-SUB 5)        TO S01-UNEARN-5.
           MOVE T-UNEARN (TT-SUB 6)        TO S01-UNEARN-6.
           MOVE T-UNEARN (TT-SUB TU-MAX)   TO S01-UNEARN-TOTAL.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " MISSING BF    :"         TO S01-SUMNAME.
           MOVE TOTAL-MISSING-BF           TO S01-COUNT.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM CLEAR-GRAND-TOTALS.

       PRINT-GRAND-TOTALS-EXIT.
           EXIT.

      ******************************************************************
       PRINT-HEADER SECTION.
      ******************************************************************

           ADD 1 TO PAGE-NUMBER.
           PERFORM GET-TIME.

           MOVE PAGE-NUMBER    TO H01-PAGE-NO.
           MOVE T-HH           TO H01-HR.
           MOVE T-MM           TO H01-MIN.
           MOVE HEADER-LINE-01 TO PR-REC.
           MOVE 99             TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-LINE-02 TO PR-REC.
           MOVE 1              TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE SPACES TO PR-REC.
           MOVE 1              TO LN-FEED.
           PERFORM WRITE-PR-REC.


           IF ( SUMMARY-FG = "Y" )
              GO TO PRINT-HEADER-SUMMARY.

      *-----------------------------------------------------------------
       PRINT-HEADER-DETAIL.

           MOVE HEADER-LINE-D01 TO PR-REC.
           MOVE 1               TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-LINE-D02 TO PR-REC.
           MOVE 1               TO LN-FEED.
           PERFORM WRITE-PR-REC.

           GO TO PRINT-HEADER-END.

      *-----------------------------------------------------------------
       PRINT-HEADER-SUMMARY.

           MOVE HEADER-LINE-S01 TO PR-REC.
           MOVE 1               TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-LINE-S02 TO PR-REC.
           MOVE 1               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *=================================================================
       PRINT-HEADER-END.

           MOVE ALL "=" TO PR-REC.
           MOVE 1       TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 7 TO LN-COUNT.
           MOVE 1 TO LN-FEED.

       PRINT-HEADER-EXIT.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************

           MOVE "BRANCH                        : " TO R01-PROMPT.
           MOVE BR-NO TO R01-BRNO.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING DATE                : " TO R01-PROMPT.
           MOVE BEG-DATE                           TO R01-DATE.
           MOVE 2                                  TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE                : " TO R01-PROMPT.
           MOVE END-DATE                           TO R01-DATE.
           MOVE 1                                  TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "INCLUDE ZERO BALANCES (I/S/O) : " TO R01-PROMPT.
           MOVE ZBAL-ISO                           TO R01-TEXT.
           MOVE 2                                  TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SUMMARY ONLY            (Y/N) : " TO R01-PROMPT.
           MOVE SUM-ONLY                           TO R01-TEXT.
           MOVE 2                                  TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME                 : " TO R01-PROMPT.
           MOVE STARTING-TIME                      TO R01-TEXT.
           MOVE 2                                  TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM GET-TIME.
           MOVE "ENDING   TIME                 : " TO R01-PROMPT.
           MOVE TIME-EDIT                          TO R01-TEXT.
           MOVE 1                                  TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2                                  TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM PRINT-HEADER.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPTRKCL.CPY".


      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO CONVAL-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CONVAL-SYSDATE.
           DISPLAY CONVAL-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CONVAL-FORM-FIELDS TO WR-BUF.
           MOVE CONVAL-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CONVAL-HELP--FILE TO HELP-LINK-FILE.
           MOVE CONVAL-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CONVAL_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
      *-----------------------------------------------------------------
       CLEAR-ALL-TOTALS.

           PERFORM VARYING TT-SUB FROM 1 BY 1 UNTIL TT-SUB > TT-MAX

                MOVE ZEROES         TO A-NO     (TT-SUB)
                                       A-CURBAL (TT-SUB)
                                       A-PURBAL (TT-SUB)
                                       A-ORIGBAL(TT-SUB)

                MOVE ZEROES         TO P-NO     (TT-SUB)
                                       P-CURBAL (TT-SUB)
                                       P-PURBAL (TT-SUB)
                                       P-ORIGBAL(TT-SUB)

                MOVE ZEROES         TO T-NO     (TT-SUB)
                                       T-CURBAL (TT-SUB)
                                       T-PURBAL (TT-SUB)
                                       T-ORIGBAL(TT-SUB)

                PERFORM VARYING TU-SUB FROM 1 BY 1 UNTIL TU-SUB > TU-MAX
                        MOVE ZEROES TO A-UNEARN (TT-SUB TU-SUB)
                                       P-UNEARN (TT-SUB TU-SUB)
                                       T-UNEARN (TT-SUB TU-SUB)
                END-PERFORM
           END-PERFORM.

      *-----------------------------------------------------------------
       CLEAR-CLASS-TOTALS.

           MOVE 1      TO TT-SUB.
           MOVE ZEROES TO A-NO     (TT-SUB)
                          A-CURBAL (TT-SUB)
                          A-PURBAL (TT-SUB)
                          A-ORIGBAL(TT-SUB).

           MOVE ZEROES TO P-NO     (TT-SUB)
                          P-CURBAL (TT-SUB)
                          P-PURBAL (TT-SUB)
                          P-ORIGBAL(TT-SUB).

           MOVE ZEROES TO T-NO     (TT-SUB)
                          T-CURBAL (TT-SUB)
                          T-PURBAL (TT-SUB)
                          T-ORIGBAL(TT-SUB).

           PERFORM VARYING TU-SUB FROM 1 BY 1 UNTIL TU-SUB > TU-MAX
                   MOVE ZEROES TO A-UNEARN (TT-SUB TU-SUB)
                                  P-UNEARN (TT-SUB TU-SUB)
                                  T-UNEARN (TT-SUB TU-SUB)
           END-PERFORM.

      *-----------------------------------------------------------------
       CLEAR-GRAND-TOTALS.

           MOVE 2      TO TT-SUB.
           MOVE ZEROES TO A-NO     (TT-SUB)
                          A-CURBAL (TT-SUB)
                          A-PURBAL (TT-SUB)
                          A-ORIGBAL(TT-SUB).

           MOVE ZEROES TO P-NO     (TT-SUB)
                          P-CURBAL (TT-SUB)
                          P-PURBAL (TT-SUB)
                          P-ORIGBAL(TT-SUB).

           MOVE ZEROES TO T-NO     (TT-SUB)
                          T-CURBAL (TT-SUB)
                          T-PURBAL (TT-SUB)
                          T-ORIGBAL(TT-SUB).

           PERFORM VARYING TU-SUB FROM 1 BY 1 UNTIL TU-SUB > TU-MAX
                   MOVE ZEROES TO A-UNEARN (TT-SUB TU-SUB)
                                  P-UNEARN (TT-SUB TU-SUB)
                                  T-UNEARN (TT-SUB TU-SUB)
           END-PERFORM.

       GET-BR-RECORD.
           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

      *--------------------------------------------
       GET-LP1-RECORD.
           MOVE "N" TO CURRENT-MISSING-BF.
           MOVE LN-OWNBR  TO LP-BRNO.
           MOVE LN-ACCTNO TO LP-ACCTNO.
           MOVE 1         TO LP-SEQNO.
           PERFORM READ-LP1-FILE.

           IF ( IO-BAD )
              ADD 1 TO TOTAL-MISSING-BF
              MOVE "Y" TO WK-MISSING-BF
              MOVE 0.00      TO LP-CURBAL
              MOVE 0         TO LP-EARNED(1)
                                LP-EARNED(2)
                                LP-EARNED(3).

      *--------------------------------------------
       GET-LXE1-RECORD.
           MOVE LN-OWNBR  TO LXE-BRNO.
           MOVE LN-ACCTNO TO LXE-ACCTNO.
           MOVE 1         TO LXE-SEQNO.
           PERFORM READ-LXE1-FILE.
           IF ( IO-BAD )
              MOVE ZEROES TO LXE-EARN(1)
                             LXE-EARN(2)
                             LXE-EARN(3).

      *-----------------------------------------------------------------
       GET-SP1-RECORD.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF ( LPWSSP-SP1-KEY NOT = SP1-KEY )
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM OPEN-SP1-FILE
              PERFORM READ-SP1-FILE
              IF ( IO-BAD )
                 GO TO IO-ERROR
              ELSE
                 PERFORM CLOSE-SP1-FILE.

      *=================================================================
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *=======================================================[ PROGID ]
       INITIAL-PROGRESS-INDICATOR.

           MOVE ZEROES                 TO PROGID-BRANCH.
           MOVE LEGEND-LIST            TO PROGID-LINE-LIST.
           MOVE PRU-LP                 TO PROGID-PRU-LP.
           MOVE EXT-ROUTE-BUF              TO PROGID-ROUTE-BUF.
           MOVE 9999                   TO PROGID-SLEEP-COUNTER.
           MOVE 250                    TO PROGID-SLEEP-COUNTER-MAX.

           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO PROGID-TIME.

           MOVE "S"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

      *-----------------------------------------------------------------
       SEND-PROGRESS-INDICATOR.

      *KC->MOVE BR-NO     TO PROGID-BRANCH.

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO PROGID-TIME.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PROGID-DISPLAY.
      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRKGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLXEGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLXE1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPTRK1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.

           PERFORM CLOSE-FILES.

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "LP/LPCVMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY CONVAL-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
