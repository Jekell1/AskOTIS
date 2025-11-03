       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CONVRP.
       DATE-WRITTEN.  04-14-1994.
      ******************************************************************
      *
      * IDENTIFICATION:  LP/CONVRP.C
      *
      * DESCRIPTION   :  LOAN CONVERSION REPORT
      *                  FOR LN-CONVERCD = "Y" ACCOUNTS ONLY
      *    DOES NOT CROSS PATHS
      *
      * !!!!!PLEASE NOTE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      * 12/8/06
      * TODAY JIM CALLED & SAID WORLD WAS HAVING A HARD TIME BALANCING
      * THE UNEARNED FOR ACCOUNTS MANUALLY CONVERTED ON.  HE SPECIFICALLY
      * WAS QUESTIONING WHERE HE SAW ACCOUNTS WHERE THE DETAIL LINE
      * SAID "* NO BF *" BUT THERE WAS STILL AN UNEARNED AMOUNT &
      * THOUGHT WE SHOULD FIX THIS - IF NO BF UNEARNED SHOULD BE 0.
      *
      * CINDY & I LOOKED AT THE ROUTINE THAT ACCUMULATED THE UNEARNED;
      * IN 2000 KEN HAD CHANGED THIS REPORT TO READ THE TRK RECORD.
      * AS OF 11/29/05 THE PROGRAM LOOPED THROUGH ALL THE TR'S FOR
      * EACH ACCT, STARTING WITH LOAN ENTRY DATE, BUT ONLY TOOK THE
      * UNEARNED FROM THE LAST TR FOUND, SO IF THERE WAS A TRK FOR
      * THE BF & THEN A REVERSAL, ONLY THE REVERSAL WAS ADDED TO THE
      * TOTALS.  THE TOTALS WERE SIGNED BUT THE UNEARNED ON THE LINE
      * WAS NOT SIGNED, SO IF THERE WAS A REVERSAL, IT WOULD ADD
      * THE NEGATIVE TO THE TOTALS BUT APPEAR AS A POSITIVE ON THE
      * LINE, SO IF YOU ADDED THE UNEARNED ON THE LINES IT WOULDN'T
      * MATCH THE TOTAL ON THE REPORT.  THIS REALLY SHOULD HAVE
      * NETTED TO AN UNEARNED OF 0.
      *
      * CINDY & I CHANGED THE REPORT TO ADD TO THE UNEARNED BUCKET
      * AND THE TOTAL UNEARNED (INSTEAD OF MOVING) SO THE REVERSAL
      * SITUATIONS WOULD NET TO ZERO.  ALSO CHANGED THE UNEARNED
      * PRINT WORKERS TO BE SIGNED.
      *
      * WHEN WE WENT TO TEST THIS VERSION, THE TOTAL CHANGED
      * DRAMATICALLY & WE TRACKED IT TO A BUNCH OF ACCOUNTS
      * WHICH WERE ENTERED IN 10/06, WITH A TRK & REVERSAL IN
      * 10/06, & THEN THE BF RE-ENTERED THE NEXT MONTH.  NOW
      * THESE ALL NETTED TO 0 ON THE OCTOBER REPORT, BUT WOULD
      * NOT EVEN APPEAR ON THE NEXT MONTH'S REPORT BECAUSE
      * IT ONLY SEARCHES THE TRFILE FOR THE MONTH OF THE LOAN
      * ENTRY DATE.
      *
      * WORLD HAS ALWAYS USED THE UNEARNED TOTAL FROM THIS
      * REPORT FOR THE JOURNAL ENTRIES FOR THE OPENING BALANCE &
      * UNEARNED FOR THEIR CONVERSIONS.  I TOLD JIM I DIDN'T
      * KNOW WHY THEY WERE NOT USING THE CONVERSION TRANSACTION
      * REGISTER...THIS SHOULD ELIMINATE THE PROBLEM OF LOAN
      * ENTRY DATES VS. TRANS DATES.  I THINK AFTER TODAYS
      * CHANGES THIS REPORT MAY BE OK IF THE CONVERSIONS ARE
      * ALL IN THE SAME MONTH...IF CUSTOMER IS NOT SURE I STILL
      * THINK THE CONVERSION REGISTER (EMTKRG) IS BETTER.
      *
      * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      *
      *
      * THIS REPORT IS SUPPOSE TO BE A SNAPSHOT OF HOW THE ACCOUNTS LOOKED
      * WHEN THEY WERE CONVERTED....USE THE TR"K" RECORD FROM CONVERSION
      * BUT THE LOAN FILE IS THE DRIVER...
      *
      * 7/26/05  LOOKED AT A PROBLEM AT WORLD, ACCT WAS CONVERTED, THEN
      * DELETED & REENTERED WITH AN OLD TRDATE, IN A DIFFERENT CLASS
      * AS A P&L, DIDN'T SHOW IN P&L TOTALS.
      * RMZ 20240116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
      *
      *=================================================================
      * REV:
      * BAH 940418 ADDED ORIGINAL BALANCE CONVERTED WITH
      * BAH 950911 ADDED DATE RANGE FOR NEW CONVERTED ACCOUNTS
      * KEC 960910 FIXED PRINTING "NO REPORT GENERATED" WHEN EXITING.
      * GLF 980212 ADDED SEPARATE TOTALS FOR PL ACCOUNTS AND OPEN
      *             ACCOUNTS
      * MJD 000124 ADDED CLASS BREAK/TOTALS
      * MJD 000215 CHANGED TO PRINT ORIG. LOAN AMT, BAL AT PURCHASE,
      *             CURBAL, AND UNEARNED (WORLD PR#223)
      * BLV 000612 CHANGED TO MOVE "E" TO LX-CODE INSTEAD OF "G"
      * JTG 000614 CORRECTED LOGIC TO CORRECT EARNINGS FOR A COMPONET OF
      *             INCOME FROM LP-REC / LXE-REC                   #1203
      * KEC 000714 {PR#00000} WORLD ACCEPTANCE [JOHN]
      *             REVISED THE REPORT TO HAVE A DIFFERENT DETAIL-LINE
      *             AND TO USE 'TRK' FILE.
      *             THIS WAS DONE IN 'A10' AND COPIED DIRECTLY FROM.
      * KEC 000717 {PR#00000} WORLD ACCEPTANCE [JIM ROSENAUER]
      *             NEEDED TO FIX THE TOTALS ROUTINE. WAS COUNTING THE
      *             PL'S AS ACTIVE'S AND ACTIVE'S AS PL'S.
      * BAH 031216 FIXED SOME STUFF WITH TRK RECORDS, ETC
      * BAH 040302 IF NO "BF" LP RECORD, WAS MOVING 999999.99 TO LP-CURBAL
      *            CAUSING BOGUS TOTALS, CHANGED TO MOVE 0
      * BAH 051129 LN-EARN-ORDER() IS 0 IF BF HAS NOT BEEN DONE, SO DONT
      *            USE IT IF 0 !!!!!!
      * BLF 061208 WORLD HAVING BIG PROBLEMS BALANCING UNEARNED FOR
      *            CONVERSIONS.  ADDED SIGNS TO ALL UNEARNED AMTS ON
      *            DETAIL LINE.  CHANGED ROUTINE WHICH LOOPS READING
      *            TRK FOR UNEARNED: DON'T ZERO OUT CURRENT-UNEARN(6)
      *            AND ADD TO UNEARNED BUCKET INSTEAD OF MOVING IN AMT;
      *            PLEASE REFER TO NOTES AT TOP OF THIS PROGRAM!
      *090819 CS USE THE LN-ENTRYDATE RANGE FOR BOTH LOAN AND TRFILE. ALLOW A
      *          SPAN OF MONTHS FOR THE TRFILE.  IF AN ACCOUNT HAS A BF BUT
      *          THE BF DID NOT HAPPPEN WITH THE DATE RANGE ON THE SCREEN PRINT
      *          A MESSAGE THAT 'BF OUT OF DATE RANGE'AND EXCLUDE FROM BALANCE
      *          AND UNEARNED TOTALS WORLD PR#662
      * BAH 160125 REMOVED LTFILE PD0003
      * BAH 170517 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2017.0605 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 181205 GLOBAL LNFILE
      * BAH 190116 GLOBAL LXFILE, SPLIT
      * BAH 20190919 REMOVED ACCESS-CALL ON TRKFILE
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
           "@(#)LP/CONVRP.CBL S35 02/20/24-15:26:55 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01TRK.CPY".
       COPY "LIBLP/LP01TRK_SQL.CPY".
       COPY "LIBLP/LPWSTRK.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
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
       01  W-STAR                      PIC X         VALUE SPACES.

      *=================================================================
      *
      *    H O L D - W O R K E R S
      *
      *=================================================================
       01  HOLD-LN-OWNBR               PIC 9(04)     VALUE ZEROES.
       01  HOLD-LN-CLASS               PIC 9(03)     VALUE ZEROES.

      *=================================================================
      *
      *    C U R R E N T - I N F O R M A T I O N - W O R K E R S
      *
      *-----------------------------------------------------------------
      *    STORE INFORMATION FOR CURRENT RECORD
      *=================================================================
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

      *-----------------------------------------------------------------
       01  HEADER-LINE-03.
           03  FILLER                  PIC X(08)     VALUE "CLASS  :".
           03  FILLER                  PIC X(02)     VALUE SPACES.
           03  H03-LCNO                PIC 9(02).
           03  H03-LCNOX               REDEFINES H03-LCNO
                                       PIC X(02).
           03  FILLER                  PIC X(01)    VALUE SPACES.
           03  H03-LCDESC              PIC X(20)    VALUE SPACES.

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
               05  FILLER                  PIC X(01).
               05  D01-CURBAL              PIC ZZZZZZ9.99.
               05  D01-CURBALX REDEFINES D01-CURBAL PIC X(10).
               05  FILLER                  PIC X(01).
               05  D01-PURBAL              PIC ZZZZZZ9.99-.
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
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CONVRP_DEF.CPY".
       COPY "LIBLP/CONVRP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/CONVRP_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRK1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
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
              GO TO END-PROGRAM.

           PERFORM PRIMARY-DRIVER.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.


      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CONVRP" TO VDU-FORM-NAME.

           MOVE CONVRP-QUEUE-POS TO Q-POS.
           MOVE CONVRP-COPIES-POS TO C-POS.
           MOVE CONVRP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
      *-----------------------------------------------------------------
       COPY "LIBGB/EOINIT.CPY".
      *-----------------------------------------------------------------
       INITIALIZATION-CONTINUE.

           MOVE "N" TO RECORDS-FOUND.

           IF EXT-ROUTE-BUF = "00"
              MOVE "CONVERTED ACCOUNTS IN PROGRESS......" TO LEGEND
              PERFORM SEND-LEGEND.

           PERFORM INITIAL-PROGRESS-INDICATOR.
           PERFORM CLEAR-ALL-TOTALS.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

      *- - - - - - - - - - - - - - - - - - - - - -{ SET HEADER FIELDS  }
           MOVE EXT-CONAME-CONAME  TO H01-NAME.
           MOVE EXT-CONAME-SYSDATE TO H01-DATE.
           MOVE EXT-CONAME-USER-ID TO H02-USERID.

           IF BEG-DATE = 0
              MOVE 19000101 TO BEG-DATE.

           PERFORM OPEN-PR-FILE.

           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-BR-FILE.
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
           MOVE EXT-JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080DT2." TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
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

       PRIMARY-DRIVER-END.

           IF ( NO-RECORDS-FOUND )
              GO TO PRIMARY-DRIVER-EXIT.

           PERFORM PRINT-GRAND-TOTALS.
           PERFORM PRINT-RANGE-LINES.

       PRIMARY-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       PROCESS-BRANCH SECTION.
      ******************************************************************

           MOVE "Y"          TO FIRST-TIME-BRANCH.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
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

      *- - - - - - - - - - - - - - - - - - - - - -{ ZERO BALANCE (ISO) }
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    VERIFY BETWEEN BEGINNING AND ENDING DATE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF LN-ENTDATE < BEG-DATE OR LN-ENTDATE > END-DATE
              GO TO NEXT-LN3-RECORD.

           IF ( FIRST-TIME = "Y" )
              MOVE LN-CLASS TO HOLD-LN-CLASS
              MOVE ZEROES   TO LC-CODE
              PERFORM SET-CLASS-HEADINGS
              MOVE LN-OWNBR TO HOLD-LN-OWNBR
              PERFORM GET-BR-RECORD
              IF ( IO-BAD )
                 GO TO IO-ERROR.

           IF ( LN-OWNBR NOT = HOLD-LN-OWNBR )
              PERFORM GET-BR-RECORD
              IF ( IO-BAD )
                 GO TO IO-ERROR.

           IF ( LN-CLASS NOT = HOLD-LN-CLASS )
              PERFORM PRINT-CLASS-TOTALS
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS.

           PERFORM GET-LP1-RECORD.


           MOVE " " TO W-STAR.

           IF CURRENT-MISSING-BF = "Y"
              PERFORM VARYING CU-SUB FROM 1 BY 1 UNTIL CU-SUB > 7
                 MOVE 0 TO CURRENT-UNEARN(CU-SUB)
              END-PERFORM
              GO TO MISSING-BF-SKIP.


           PERFORM GET-LX1-RECORD.
           PERFORM GET-SP1-RECORD.
           PERFORM GET-TRK-RECORD.

       MISSING-BF-SKIP.
           IF FOUND-TK = "N"
              IF NOT (CURRENT-MISSING-BF = "Y")
                 MOVE "*"   TO W-STAR.

           PERFORM LOAD-CURRENT-INFORMATION.
           PERFORM UPDATE-TOTALS.
           PERFORM PRINT-DETAIL-LINE.

           IF ( FIRST-TIME-BRANCH = "Y" )
              MOVE "Y" TO RECORDS-FOUND
              MOVE "N" TO FIRST-TIME
                          FIRST-TIME-BRANCH.

           GO TO NEXT-LN3-RECORD.

       PROCESS-BRANCH-END.

           IF ( FIRST-TIME-BRANCH NOT = "Y" )
              PERFORM PRINT-CLASS-TOTALS.

           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LXE1-FILE.

       PROCESS-BRANCH-EXIT.
           EXIT.

      ******************************************************************
       GET-TRK-RECORD SECTION.
      ******************************************************************

           MOVE "N" TO FOUND-TK.

           PERFORM VARYING CU-SUB FROM 1 BY 1 UNTIL CU-SUB > 7
               MOVE 0 TO CURRENT-UNEARN(CU-SUB)
           END-PERFORM.

           MOVE SPACES TO CURRENT-PLCD.

           MOVE "B"                        TO TRK-PATH-OVERRIDE.

           PERFORM OPEN-TRK1-FILE.

           MOVE LN-OWNBR            TO TK-BRNO
                                       QTRK1-WBEG-BRNO
                                       QTRK1-WEND-BRNO.
           MOVE BEG-DATE            TO TK-DATE SQL-DATE-YYYYMMDD.
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
              GO TO GET-TRK-RECORD-EXIT.

           MOVE "Y" TO FOUND-TK.

      * VALID 'TRK' RECORD
      *    MOVE 0 TO CURRENT-UNEARN(CU-MAX).
           PERFORM VARYING CU-SUB FROM 1 BY 1 UNTIL CU-SUB > 6
                   ADD TK-UNEARN(CU-SUB) TO CURRENT-UNEARN(CU-SUB)
                   ADD TK-UNEARN(CU-SUB) TO CURRENT-UNEARN(CU-MAX)
           END-PERFORM.

           MOVE TK-PLCD TO CURRENT-PLCD.

      * MIGHT BE A REVERAL, NEW, REVERSAL, NEW, ETC...KEEP READING
           GO TO GET-TRK-RECORD-NEXT.

      * WHEN YOU END, YOU WILL HAVE THE MOST CURRENT TRK

       GET-TRK-RECORD-EXIT.
           EXIT.

      ******************************************************************
       LOAD-CURRENT-INFORMATION SECTION.
      ******************************************************************

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO          TO CURRENT-LOAN-LC-ACCTNO.
           MOVE LN-LNAMT                  TO CURRENT-ORIGBAL.
           IF W-STAR = "*"
              MOVE ZEROS                  TO CURRENT-PURBAL
                                             CURRENT-CURBAL
           ELSE
              MOVE LP-CURBAL                 TO CURRENT-CURBAL
              MOVE LN-CURBAL                 TO CURRENT-PURBAL.

       LOAD-CURRENT-INFO-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-TOTALS SECTION.
      ******************************************************************

           MOVE ZEROES TO TT-SUB.

      *=================================================================
       UPDATE-TOTALS-NEXT.

           ADD 1 TO TT-SUB.

           IF ( TT-SUB > TT-MAX )
              GO TO UPDATE-TOTALS-EXIT.

           IF CURRENT-PLCD NOT = SPACES
              GO TO UPDATE-TOTALS-PL
           ELSE
              GO TO UPDATE-TOTALS-ACTIVE.

      *---------------------------------------------{ RECORD IS ACTIVE }
       UPDATE-TOTALS-ACTIVE.

           ADD 1                            TO A-NO     (TT-SUB).
           ADD CURRENT-ORIGBAL              TO A-ORIGBAL(TT-SUB).
           ADD CURRENT-CURBAL               TO A-CURBAL (TT-SUB).
           ADD CURRENT-PURBAL               TO A-PURBAL (TT-SUB).

           PERFORM VARYING TU-SUB FROM 1 BY 1 UNTIL TU-SUB > TU-MAX
                 ADD CURRENT-UNEARN(TU-SUB) TO A-UNEARN (TT-SUB TU-SUB)
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           GO TO UPDATE-TOTALS-TOTAL.

      *------------------------------------------------{ RECORD IS P/L }
       UPDATE-TOTALS-PL.

           ADD 1                            TO P-NO     (TT-SUB).
           ADD CURRENT-ORIGBAL              TO P-ORIGBAL(TT-SUB).
           ADD CURRENT-CURBAL               TO P-CURBAL (TT-SUB).
           ADD CURRENT-PURBAL               TO P-PURBAL (TT-SUB).

           PERFORM VARYING TU-SUB FROM 1 BY 1 UNTIL TU-SUB > TU-MAX
                 ADD CURRENT-UNEARN(TU-SUB) TO P-UNEARN (TT-SUB TU-SUB)
           END-PERFORM.

      *-----------------------------------------------------------------
       UPDATE-TOTALS-TOTAL.

           ADD 1                            TO T-NO     (TT-SUB).
           ADD CURRENT-ORIGBAL              TO T-ORIGBAL(TT-SUB).
           ADD CURRENT-CURBAL               TO T-CURBAL (TT-SUB).
           ADD CURRENT-PURBAL               TO T-PURBAL (TT-SUB).

           PERFORM VARYING TU-SUB FROM 1 BY 1 UNTIL TU-SUB > TU-MAX
                 ADD CURRENT-UNEARN(TU-SUB) TO T-UNEARN (TT-SUB TU-SUB)
           END-PERFORM.

           GO TO UPDATE-TOTALS-NEXT.

       UPDATE-TOTALS-EXIT.
           EXIT.

      ******************************************************************
       PRINT-DETAIL-LINE SECTION.
      ******************************************************************

           IF ( SUM-ONLY = "Y" )
              GO TO PRINT-DETAIL-LINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE CURRENT-LOAN-ACCTNO        TO D01-LOAN-NUM.
           IF CURRENT-PLCD = "P"
              INSPECT D01-LOAN-NUM REPLACING FIRST " " BY "P".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.

           IF ( IO-BAD )
               MOVE "***NOT ON FILE***"    TO D01-LNAME
           ELSE
               MOVE BW-LNAME               TO D01-LNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE W-STAR                     TO D01-STAR.
           MOVE CURRENT-ORIGBAL            TO D01-ORIGBAL.
           MOVE CURRENT-CURBAL             TO D01-CURBAL.
           IF CURRENT-MISSING-BF = "Y"
             MOVE "* NO BF * " TO D01-CURBALX
           ELSE
           IF W-STAR = "*"
              MOVE " *BF DATE OUT OF RANGE*" TO D01-BALANCES.

           IF W-STAR NOT = "*"
           MOVE CURRENT-PURBAL             TO D01-PURBAL.

           MOVE CURRENT-UNEARN(1)          TO D01-UNEARN-1.
           MOVE CURRENT-UNEARN(3)          TO D01-UNEARN-3.
           MOVE CURRENT-UNEARN(4)          TO D01-UNEARN-4.
           MOVE CURRENT-UNEARN(5)          TO D01-UNEARN-5.
           MOVE CURRENT-UNEARN(6)          TO D01-UNEARN-6.
           MOVE CURRENT-UNEARN(7)          TO D01-UNEARN-TOTAL.

           IF ( TK-ACCTNO = ZEROES )
              MOVE "*"                     TO D01-USED-LP-REC.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
       SET-CLASS-HEADINGS SECTION.
      ******************************************************************

           MOVE HOLD-LN-CLASS TO H03-LCNO.

           IF ( HOLD-LN-CLASS NOT = LC-CODE )
              PERFORM GET-LC1-RECORD.

           MOVE LC-DESC      TO H03-LCDESC.
           MOVE "N"          TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

       SET-CLASS-HEADINGS-EXIT.
           EXIT.

      ******************************************************************
       PRINT-CLASS-TOTALS SECTION.
      ******************************************************************

           MOVE "Y" TO SUMMARY-FG.
           MOVE 1   TO TT-SUB.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DO NOT LET TOTALS WRAP END OF PAGE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF (( LN-COUNT + 10 ) > EXT-CONAME-RPT-LINES )
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "CLASS TOTALS:"            TO DETAIL-LINE.

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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "N" TO SUMMARY-FG.
           PERFORM CLEAR-CLASS-TOTALS.

       PRINT-CLASS-TOTALS-EXIT.
           EXIT.

      ******************************************************************
       PRINT-GRAND-TOTALS SECTION.
      ******************************************************************

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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           PERFORM CLEAR-GRAND-TOTALS.

       PRINT-GRAND-TOTALS-EXIT.
           EXIT.

      ******************************************************************
       PRINT-HEADER SECTION.
      ******************************************************************

           ADD 1 TO PAGE-NUMBER.
           PERFORM GET-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{01}
           MOVE PAGE-NUMBER    TO H01-PAGE-NO.
           MOVE T-HH           TO H01-HR.
           MOVE T-MM           TO H01-MIN.
           MOVE HEADER-LINE-01 TO PR-REC.
           MOVE 99             TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{02}
           MOVE HEADER-LINE-02 TO PR-REC.
           MOVE 1              TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{03}
           MOVE HEADER-LINE-03 TO PR-REC.
           MOVE 1              TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
           MOVE EXT-CONAME-CONAME TO CONVRP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CONVRP-SYSDATE.
           DISPLAY CONVRP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CONVRP-FORM-FIELDS TO WR-BUF.
           MOVE CONVRP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CONVRP-HELP--FILE TO HELP-LINK-FILE.
           MOVE CONVRP-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CONVRP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
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

      *=================================================================
       GET-BR-RECORD.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM READ-BR-FILE.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

      *-----------------------------------------------------------------
       GET-LC1-RECORD.

           MOVE HOLD-LN-CLASS TO LC-CODE.
           PERFORM READ-LC1-FILE.
           IF ( IO-BAD )
              MOVE "INVALID" TO LC-DESC.

      *-----------------------------------{ GET BALANCE FORWARD RECORD }
       GET-LP1-RECORD.

           MOVE "N" TO CURRENT-MISSING-BF.
           MOVE LN-OWNBR  TO LP-BRNO.
           MOVE LN-ACCTNO TO LP-ACCTNO.
           MOVE 1         TO LP-SEQNO.
           PERFORM READ-LP1-FILE.

           IF ( IO-BAD )
              ADD 1 TO TOTAL-MISSING-BF
              MOVE "Y" TO CURRENT-MISSING-BF
              MOVE 0.00      TO LP-CURBAL
              MOVE 0         TO LP-EARNED(1)
                                LP-EARNED(2)
                                LP-EARNED(3).

      *-----------------------------------{ GET BALANCE FORWARD RECORD }
       GET-LX1-RECORD.

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
              PERFORM READ-SP1-FILE
              IF ( IO-BAD )
                 GO TO IO-ERROR.

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

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO PROGID-TIME.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PROGID-DISPLAY.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRKGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLXEGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLXE1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPTRK1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "LP/LPCVMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY CONVRP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
