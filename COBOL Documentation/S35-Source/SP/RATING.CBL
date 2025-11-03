       IDENTIFICATION DIVISION.
       PROGRAM-ID.      RATING.
      ******************************************************************
      *
      *                      (SP)
      *    DESCRIPTION:  RATING SHEET LISTING
      *
      *    IF "Y" TO EXTRACT FILE, A FILE IS CREATED IN
      *    /USR/EXTRACT/OPS/RATING (OPS & RATING ARE IN CAPS)
      *    FILE IS PIPE-DELIMITED & CONTAINS BRANCH #, RATING
      *    SHEET FIELDS, AND 3 BONUS AMTS.  ALL FIELDS (EXCEPT 
      *    BRANCH NUMBER) ARE FORMATTED AS AMOUNTS SO THE
      *    DOLLAR AMTS WON'T MATCH THE AMTS ON THE REPORT (SINCE
      *    THE REPORT DOESN'T PRINT DECIMALS).  AMTS ARE LEFT
      *    JUSTIFIED WITH NO LEADING ZEROS & LEADING MINUS SIGN
      *    IF NEGATIVE.
      *
      * REV:
      * BLV 031030 DON'T SET WR-ALL-PATH (LEAVE RELATIVE), MOVE GRTOTL
      *            OUT OF MISC SECTION
      * BLV 031210 CHANGE ALL FIELD NAMES IN DETAIL LINE & TOTAL TABLE,
      *            CUR WAS REALLY LAST YEAR MTD, YTD WAS REALLY LAST
      *            YEAR YTD, TOTALLY CONFUSING!!!
      * BAH 031211 PROGRAM CLEAN UP.
      * BLV 031217 CHANGE TO GET YTD PCTS FROM WR-RECORD INTEAD OF
      *            COMPUTING...THESE ARE ACCUMLULATED YTD STATS
      * BLV 040112 REMOVE DECIMALS ON VARIANCES FOR NUMBER FIELD (NOT
      *            AMOUNTS).  CHANGE LINE 41 FROM % NON BKRUP NF CLAIMS
      *            TO PRETAX INCOME TO BEG LEDGER
      * BLV 040112 CHANGED TO COMPUTE YTD FOR RATING SHEET LINES 30 & 32;
      *            THESE FIELDS WERE DELETED SINCE THEY ARE COMPUTED
      *            PERCENTS, NOT ACCUMULATED
      * BLV 040113 ADD "ROUNDED" TO ALL COMPUTES FOR PERCENTS AND
      *            AVERAGES
      * BLV 040113 FIX DELINQUENCY PERCENTAGES - WERE DIVIDING BY
      *            WR OUTSTANDING (LAST RECORD READ) INSTEAD OF TOTAL
      * BLV 040127 FIX DATE IN HEADING, WAS PRINTING GARBAGE WHEN
      *            RUN IN BATCH
      * GLB 040407 BROUGHT OVER VERSION THAT ALLOWS ENTERING 1 OR MORE
      *            LINE NUMBERS
      * CS  040408 ADDED PRINTING OF CASHIER, ASST. MGR. & MGR BONUSES
      * BLV 040409 FOR NOW, REMOVE MULTIPLE LINE OPTIONS & PAGE ON
      *            BRANCH, JUST STARRED OFF.  ALSO, FIXED LAST YEAR
      *            ON LINE 23, LINE 56
      * BLV 040422 FIX YTD PERCENTS ON LINES 20, 22, 24, 26, 29, 37,
      *            39, 41, & 44 TO COMPUTE YTD PERCENTS FOR GROUP
      *            TOTALS
      * BLV 040430 FIX PRINTING INCORRECT GROUP IN HEADING & PRINT
      *            CORRECT BRANCH NAME ON LINE FOR 1 LINE FORMAT
      * GLB 040506 TEST MONTH BETWEEN 1-12
      *  CS 040507 FIX MISSING FIELDS
      * JTG 040510 INCREASED BONUS PRINTS TP Z,ZZZ.99         WORLD 040510
      *            03  D-BONUS2            PIC Z,ZZ9.99.
      * BLV 040514 FIX SO LINE # & DESC WILL PRINT IN HEADING IF RUNNING
      *            FOR A GROUP
      * BLV 040521 FIX NOT PRINTING LYR CUR FOR LINE 22, RS INC TO
      *            BEG LEDGER
      * BLV 040521 ALSO FIX SEVERAL PERCENTAGE CALCULATIONS, LINES
      *            26, 37, 39, 41
      * BLV 040602 T-COUNT WASN'T COMP, CAUSING NON-NUMERIC ERRORS,
      *            USED IN LINE 10, PERCENT TURNOVER
      * BLV 041213 ADD LIFE OF THE SOUTH NON-FILE CLAIMS TO LINE 40, 42
      * BLF 050505 INCREASED BONUS PRINTS TP ZZ,ZZZ.99         WORLD
      * BLF 050912 ADD MINIMUM & MAXIMUM PARAMETERS TO RATING SHEET IF
      *            PRINTING 1 LINE, WORLD PR# 421
      * BLF 060616 FIX 1 LINE REPORT PRINTING DIVIDING LINES IN THE
      *            WRONG PLACES, WORLD PR# 462
      * BLF 061115 EXPANDED ALL GROUP SUBSCRIPTS TO ACCOMODATE
      *            EXPANDED GROUP SEARCH TABLES, WORLD PR# 502
      * BLF 070717 MOVE SPACES TO DETAIL-LINE AT END OF PRINT-LINE-
      *            LEVEL-HEAD; SOME LEFTOVER DASHES WERE PRINTING NEXT
      *            TO BRANCH, WORLD, PER JR
      * BLF 080108 CHANGE ALL DOLLAR AMTS TO PRINT AS WHOLE NUMBERS,
      *            LEAVE ALL PERCENTAGES PRINTING WITH 2 PLACES,
      *            WORLD PR# 560
      * BLM 090209 REPLACE LINE 38 (NF CLAIMS-BANKRUPT) WITH LINE 37
      *            FROM SALES FINANCE RATING SHEET, SALES FIN PROFIT.
      *            REPLACE LINE 39(%BNKRPT NF CLMS) WITH LINE 35 FROM
      *            SALES FINANCE RATING SHEET, SALES FIN BAD DEBT EXPENSE.
      *            BECAUSE SO THESE 2 LINES FROM LS/WRWCBC USED OTHER LINES
      *            WHICH IN TURN USED EVEN MORE LINES, I PRETTY MUCH HAD
      *            TO ADD IN THE WHOLE WCBC TOTAL TABLE.  TO KEEP IT A
      *            LITTLE CLEAN, I EXPANDED THE TOTAL TABLE A BUNCH &
      *            PREFIXED THE WCBC ITEMS WITH A 1, SO LINE 1 ON WRWCBC IS
      *            LINE 101 IN TOTAL TABLE.  SO THERE ARE A FEW BLANKS IN
      *            IN THE TOTAL TABLE BUT I THOUGHT THIS WAS CLEARER.
      *            WORLD PR# 625
      * BLM 090427 COUPLE OF PROBLEMS WITH CHANGE ABOVE (PR# 625). CHANGED
      *            LINE-DESC FOR LINE 38 & 39 SO CORRECT DESCRIPTION
      *            DISPLAYS WHEN SELECTING 1 LINE TO PRINT.  THEN FIXED
      *            WRITE-DETAIL ROUTINE TO FIX PRINTING ONE 1 LINE  -
      *            LINE 38 OR 39 WERE NOT PRINTING IF 1 LINE SELECTED.
      *            MUST REMEMBER I AM STILL PRINTING THESE GUYS AS LINE
      *            38 OR 39, BUT THEY ARE REALLY IN THE TOTAL TABLE AS
      *            LINES 137 AND 135.  ALSO HAD TO TWEAK THE MIN-MAX
      *            CHECKS FOR THESE LINES.  WORLD PR# 625.
      * BLM 140815 INCLUDE WCBC ELM OUTSTANDING (LINE 101 IN TOTAL-TABLE) IN
      *            COMPUTATION OF MTD & YTD PROFIT PERCENTS, WORLD PR# 992B
      * BLM 140917 IF WR-MGR-PENALTY-FG = "*", PRINT * NEXT TO MGR BONUS,
      *            WORLD PR# 1019
      * BLM 150519 CHANGE TO NOT INCLUDE WCBC IN CALCULATOINS OF PROFIT 
      *            PERCENTS.  WORLD PR# 1078.
      * BLM 160125 ADD OPTION TO EXTRACT A FILE WITH RATING SHEET FIELDS,
      *            ONLY BRANCH, NO GROUP TOTALS, WORLD PR# 1131
      * BLM 160127 FIX PROBLEM RUNNING BY GROUP, WORLD PR# 1131
      * BLM 160309 FIX PROBLEM RUNNING IN GROUP BATCH EXECUTION DRIVER,
      *            WORLD PR# 1143	
      * BLM 160616 CHANGE TO PRINT WR-BSR-BONUS(1) INSTEAD OF CASHIER &
      *            AM BONUSES, STILL PRINT CASHIER & AM ON LAST LINE IF
      *            NON-ZERO (FOR OLD DATE RANGE). DON'T PRINT PENALTY
      *            FLAG.  CHANGE TO EXTRACT BSR-BONUS & MGR-BONUS, BOTH
      *            9(5)V99, DON'T EXTRACT CSR/AM BONUSES, WORLD PR# 1163
      * BAH 170216 STOP LINE 15 FROM PRINTING AND STOP BONUS LINES FROM 
      *            PRINTING (LINES 57 AND 58), WORLD PR#1228
      * 20171013 BAH ACTIVATE 8 DIGIT DATES, PD0006
      *   190523  CS DONT PRINT LINES 12 -37,LINE 43 WORLD PR#1373
      *              AND LINE 36/37 ARE UPDATED VIA THE GL RATING SHEET
      *              UPDATE SO SKIP THEM, SKIP EXTRACT, UP-ARROW
      *              TO ACCESS.     
      * CS 20190820 ADD LINE 34 CHARGE-OFFS BACK TO RATING SHEET PR#1386
      * BAH 20200831 REMOVED RESET ON BR1 
      * RMZ 20230830 PERFORMANCE TUNING - CHANGED WRFILE 'READ NEXT' TO 
      *                                   'READ'. 
      *********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT RATING-FILE ASSIGN OUTPUT RATING-PATH
                          ORGANIZATION LINE SEQUENTIAL
                          LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                          FILE STATUS FILE-STAT.
      /
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPRL.CPY".
      *****************************************************************
      *    RATING EXTRACT FILE
      *****************************************************************
       FD  RATING-FILE
           RECORD IS VARYING IN SIZE FROM 10 TO 4500 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  RATING-REC.
           03  RATING-BYTE             PIC X OCCURS 4500 TIMES.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/RATING.CBL S35 11/22/23-14:19:58 >".

      ******************************************************************
      *     F I L E   I F N
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBSP/SP01WR.CPY".
       COPY "LIBSP/SP01WR_SQL.CPY".
       COPY "LIBSP/SPWSWR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       01  RATING-PATH.
           03  RATING-PATH1.
               05  RATING-PATH-EXTRACT   PIC X(12) VALUE
                   "/USR/EXTRACT".
               05  FILLER                PIC X(5)  VALUE
                   "/OPS/".
           03  FILLER                PIC X(6)  VALUE
               "RATING".
      *-----------------------------------------------------------------
       01  PR-IFN               PIC X(2)     VALUE "PR".

      ******************************************************************
      *   LINE WORKERS
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999   COMP   VALUE 1.
           03  LN-COUNT         PIC 999   COMP   VALUE 99.
           03  LN-HEAD          PIC 999   COMP   VALUE 99.
           03  LN-WK            PIC 999   COMP   VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)  COMP   VALUE 0.

      ******************************************************************
      *   MISC WORKERS
      ******************************************************************
       01  SUB                  PIC 999.
       01  WORK-SUB             PIC 999.
       01  TOT-SUB              PIC 99.
       01  LEVEL-SUB            PIC 9999.
       01  GROUP-SUB            PIC 9999.
       01  PREV-SUB             PIC 9999.
       01  WORK-AMT             PIC S9(9)V99.
       01  WORK1                PIC S9(9)V99.
       01  WORK2                PIC S9(9)V99.
       01  HOLD-LEVEL           PIC 9.

       01  PRINT-THE-LINE       PIC X.


       01  LAST-YYYYMM-DATE     PIC 9(6).
       01  FILLER               REDEFINES LAST-YYYYMM-DATE.
           03  LAST-YYYY        PIC 9(4).
           03  LAST-MM          PIC 99.

       01  CUR-YYYYMM-DATE      PIC 9(6).
       01  FILLER               REDEFINES CUR-YYYYMM-DATE.
           03  CURRENT-YYYY     PIC 9(4).
           03  CURRENT-MM       PIC 99.

       01  FIRST-TIME-BRNO-SW   PIC X.

      * LAST YEAR RATING SHEET RECORD
       COPY "LIBSP/SP01WR.CPY" REPLACING LEADING "WR" BY "WRLY".

      * THIS MONTH RATING SHEET RECORD
       COPY "LIBSP/SP01WR.CPY" REPLACING LEADING "WR" BY "WRTM".

      ******************************************************************
      *    T O T A L   T A B L E
      ******************************************************************
       01  TOTAL-TABLE.
           03  TOTAL-TAB          OCCURS 10.
               05  T-LINE         OCCURS 137.
                   07  T-MTD      PIC S9(13)V99 COMP.
                   07  T-YTD      PIC S9(13)V99 COMP.
                   07  T-LYR-MTH  PIC S9(13)V99 COMP.
                   07  T-LYR-YTD  PIC S9(13)V99 COMP.
               05  T-COUNT        PIC 9(5)      COMP.
      *        05  T-PENALTY-FG   PIC X.

       01  PREV-SCURR-TABLE.
           03  PREV-SCURR       OCCURS 15
                                PIC 9999     COMP.

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  FIRST-LEVEL          PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  DISPLAY-DATE             PIC 9(4).
       01  DISPLAY-DATEX            REDEFINES DISPLAY-DATE.
           03  DISPLAY-MO           PIC 99.
           03  DISPLAY-YR           PIC 99.

       01  LINE-TABLE.
           03  LINE-RPT             PIC 99 OCCURS 10.

       01  LINE-DESC-TABLE.
           03  LINE-FILLER.
               05  FILLER          PIC X(23) VALUE
               "OPEN CUSTOMERS         ".
               05  FILLER          PIC X(23) VALUE
               "CUSTOMER GAIN          ".
               05  FILLER          PIC X(23) VALUE
               "LEDGER BALANCE         ".
               05  FILLER          PIC X(23) VALUE
               "LEDGER GAIN            ".
               05  FILLER          PIC X(23) VALUE
               "NO. NEW CUSTOMERS      ".
               05  FILLER          PIC X(23) VALUE
               "NO. FORMER CUSTOMERS   ".
               05  FILLER          PIC X(23) VALUE
               "NO. RENEWAL CUSTOMERS  ".
               05  FILLER          PIC X(23) VALUE
               "TOTAL # LOANS MADE     ".
               05  FILLER          PIC X(23) VALUE
               "GROSS LOAN VOLUME      ".
               05  FILLER          PIC X(23) VALUE
               "PERCENT TURNOVER       ".
               05  FILLER          PIC X(23) VALUE
               "AVG UNPAID BALANCE     ".
               05  FILLER          PIC X(23) VALUE
               "PRE-TAX PROFIT         ".
               05  FILLER          PIC X(23) VALUE
               "WCBC PROFIT            ".
               05  FILLER          PIC X(23) VALUE
               "INTEREST ALLOCATION    ".
               05  FILLER          PIC X(23) VALUE
               "SUPERVISION            ".
               05  FILLER          PIC X(23) VALUE
               "GENERAL RESERVE ADJ    ".
               05  FILLER          PIC X(23) VALUE
               "INTANGIBLE AMORTIZATION".
               05  FILLER          PIC X(23) VALUE
               "ROBBERIES              ".
               05  FILLER          PIC X(23) VALUE
               "RATING SHEET PROFIT    ".
               05  FILLER          PIC X(23) VALUE
               "% RATING SHEET PROFIT  ".
               05  FILLER          PIC X(23) VALUE
               "RATING SHEET INCOME    ".
               05  FILLER          PIC X(23) VALUE
               "% RS INC TO BEG LEDGER ".
               05  FILLER          PIC X(23) VALUE
               "INTEREST & FEE INCOME  ".
               05  FILLER          PIC X(23) VALUE
               "% INT & FEE INCOME     ".
               05  FILLER          PIC X(23) VALUE
               "INSURANCE INCOME       ".
               05  FILLER          PIC X(23) VALUE
               "% INSURANCE INCOME /BEG".
               05  FILLER          PIC X(23) VALUE
               "RATING SHEET EXPENSES  ".
               05  FILLER          PIC X(23) VALUE
               "EXPENSE PER LOAN MADE  ".
               05  FILLER          PIC X(23) VALUE
               "% EXPENSES TO BEG LEDGR".
               05  FILLER          PIC X(23) VALUE
               "EXPENSES AS A % INCOME ".
               05  FILLER          PIC X(23) VALUE
               "TOTAL PERSONNEL EXPENSE".
               05  FILLER          PIC X(23) VALUE
               "PERSONNEL AS A % INC  E".
               05  FILLER          PIC X(23) VALUE
               "CHARGE OFF EXPENSE     ".
               05  FILLER          PIC X(23) VALUE
               "NUMBER OF CHARGE-OFFS  ".
               05  FILLER          PIC X(23) VALUE
               "RECOVERIES             ".
               05  FILLER          PIC X(23) VALUE
               "BAD DEBT INCREASE      ".
               05  FILLER          PIC X(23) VALUE
               "% BAD DEBT TO BEG LEDGR".
               05  FILLER          PIC X(23) VALUE
               "WCBC RS PROFIT         ".
               05  FILLER          PIC X(23) VALUE
               "WCBC BAD DEBT EXPENSE  ".
               05  FILLER          PIC X(23) VALUE
               "NF CLAIMS-NON-BANKRUPT ".
               05  FILLER          PIC X(23) VALUE
               "% PRETAX INC TO BEG LED".
               05  FILLER          PIC X(23) VALUE
               "TOTAL NF CLAIMS        ".
               05  FILLER          PIC X(23) VALUE
               "TOTAL BAD DEBT + NF    ".
               05  FILLER          PIC X(23) VALUE
               "% TO BEGINNING LEDGER  ".
               05  FILLER          PIC X(23) VALUE
               "NO. DEL - 30 DAYS      ".
               05  FILLER          PIC X(23) VALUE
               "AMT DEL  - 30 DAYS     ".
               05  FILLER          PIC X(23) VALUE
               "% AMT DEL - 30 DAYS    ".
               05  FILLER          PIC X(23) VALUE
               "NO. DEL - 60 DAYS      ".
               05  FILLER          PIC X(23) VALUE
               "AMT DEL  - 60 DAYS     ".
               05  FILLER          PIC X(23) VALUE
               "% AMT DEL - 60 DAYS    ".
               05  FILLER          PIC X(23) VALUE
               "NO. DEL - 90 DAYS      ".
               05  FILLER          PIC X(23) VALUE
               "AMT DEL  - 90 DAYS     ".
               05  FILLER          PIC X(23) VALUE
               "% AMT DEL - 90 DAYS    ".
               05  FILLER          PIC X(23) VALUE
               "NO. DEL - TOTAL        ".
               05  FILLER          PIC X(23) VALUE
               "AMT DEL  - TOTAL       ".
               05  FILLER          PIC X(23) VALUE
               "% AMT DEL - TOTAL      ".
               05  FILLER          PIC X(23) VALUE
               "        BSR BONUS      ".
               05  FILLER          PIC X(23) VALUE
               "        MANAGER BONUS  ".
               05  FILLER          PIC X(23) VALUE
               "        CASHIER BONUS  ".
               05  FILLER          PIC X(23) VALUE
               "      ASSISTANT BONUS  ".

       01  LINE-DESC-TABLE2 REDEFINES LINE-DESC-TABLE.
           03  LINE-DESC       PIC X(23) OCCURS 60.

       01  TEST-MTD-AMT        PIC S9(9)V99.
       01  TEST-YTD-AMT        PIC S9(9)V99.
       01  WITHIN-MIN-MAX      PIC X.

       01  EXTRACT-LINE             PIC X(74) OCCURS 56.
       01  EXTRACT-NO-LINE OCCURS 56 REDEFINES EXTRACT-LINE.
           03  EXT-MTD-NO           PIC --------9.99.
           03  EXT-YTD-NO           PIC --------9.99.
           03  EXT-LYR-MTD-NO       PIC --------9.99.
           03  EXT-LYR-YTD-NO       PIC --------9.99.
           03  EXT-MTD-VAR-NO       PIC --------9.99.
           03  EXT-YTD-VAR-NO       PIC --------9.99.
           03  EXT-MTD-VAR-PCT      PIC ----.-9.
           03  EXT-YTD-VAR-PCT      PIC ----.-9.
       01  EXTRACT-PCT-LINE OCCURS 56 REDEFINES EXTRACT-LINE.
           03  EXT-PCT-MTD          PIC --------9.99.
           03  EXT-PCT-YTD          PIC --------9.99.
           03  EXT-PCT-LYR-MTD      PIC --------9.99.
           03  EXT-PCT-LYR-YTD      PIC --------9.99.
           03  EXT-PCT-MTD-VAR      PIC --------9.99.
           03  EXT-PCT-YTD-VAR      PIC --------9.99.
           03  EXT-PCT-MTD-VAR-PCT  PIC ----.-9.
           03  EXT-PCT-YTD-VAR-PCT  PIC ----.-9.
       01  EXTRACT-BONUS-LINE.
           03  EXTRACT-BSR         PIC -----9.99.
           03  EXTRACT-MGR         PIC -----9.99.
       01  STRING-LINE             PIC X(94) OCCURS 57.
       01  HOLD-REC1               PIC X(2055).
       01  HOLD-REC2               PIC X(2624).
       01  HOLD-BRNO               PIC 9(4).

      ******************************************************************
      *     S C R E E N   F I E L D S
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  GROUP-FLAG            PIC X.
           03  ENT-GROUP             PIC X(4).
           03  BEG-BRANCH            PIC 9(4).
           03  END-BRANCH            PIC 9(4).
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE PIC X OCCURS 10.
           03  LINES-PER-PAGE        PIC 99.
           03  PRINT-1-LINE-FG       PIC X.
           03  PRINT-LINE-TABLE.
               05  PRINT-LINE-RPT    PIC 99 OCCURS 56.
           03  MIN-AMT               PIC S9(9)V99.
           03  MAX-AMT               PIC S9(9)V99.
           03  MIN-MAX-PERIOD        PIC X.
           03  MIN-MAX-FG            PIC X.
           03  EXTRACT-FG            PIC X.
           03  PRINT-BONUS           PIC X.

       01  VERSION-CONTROL     PIC X     VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ******************************************************************
      *     P R I N T   B U F F E R S
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(36)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZZ9.
           03  FILLER           PIC X(2)     VALUE " ".

      *----------------------------------------------------------------
       01  HEAD2.
           03  HEAD2-TYPE       PIC X(8)  VALUE "BRANCH:".
           03  H-GRID           PIC X(4).
           03  H-BRNO          REDEFINES H-GRID PIC Z(4).
           03  FILLER           PIC XX VALUE "  ".
           03  H-GRNAME         PIC X(31) VALUE " ".
           03  FILLER           PIC X(8)  VALUE " ".
           03  FILLER           PIC X(26) VALUE
           "- - -  RATING SHEET  - - -".
           03  FILLER           PIC X(2)  VALUE " ".
           03  H-MMYY           PIC 99/99.
           03  FILLER           PIC X(32) VALUE " ".
           03  FILLER           PIC X(6)  VALUE "USER:".
           03  H-USERID         PIC X(8).

      *----------------------------------------------------------------
       01  HEAD3.
           03  H-PGDESC         PIC X(26) VALUE SPACES.
           03  FILLER           PIC X(31) VALUE
               "       CUR MTH        CUR YTD ".
           03  FILLER           PIC X(31) VALUE
               "      LYR CUR        LYR YTD  ".
           03  FILLER           PIC X(31) VALUE
               "     CUR VAR        YTD VAR   ".
           03  FILLER           PIC X(13) VALUE
               "CUR%    YTD% ".


      ******************************************************************
      *    D E T A I L - L I N E
      ******************************************************************
       01  DETAIL-LINE            PIC X(132) VALUE " ".
       01  DETAIL1                REDEFINES DETAIL-LINE.
           03  D-DETAIL-AREA.
               05  FILLER             PIC X(26).
           03  D-DETAIL-AREA1 REDEFINES D-DETAIL-AREA.
               05  D-LINENO           PIC 99.
               05  FILLER             PIC X.
               05  D-BRNO             PIC 9(4).
               05  FILLER             PIC X.
               05  D-DESC             PIC X(18).
           03  D-DETAIL-AREA2 REDEFINES D-DETAIL-AREA.
               05  D-GRID             PIC X(4).
               05  D-GRDASH1          PIC X(4).
               05  D-GRNAME           PIC X(18).

           03  D-MTD              PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  D-MTD-NO           REDEFINES D-MTD
                                  PIC ZZ,ZZZ,ZZZ,ZZZ-.
           03  D-YTD              PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  D-YTD-NO           REDEFINES D-YTD
                                  PIC ZZ,ZZZ,ZZZ,ZZZ-.
           03  D-LYR-MTD          PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  D-LYR-MTD-NO       REDEFINES D-LYR-MTD
                                  PIC ZZ,ZZZ,ZZZ,ZZZ-.
           03  D-LYR-YTD          PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  D-LYR-YTD-NO       REDEFINES D-LYR-YTD
                                  PIC ZZ,ZZZ,ZZZ,ZZZ-.
           03  D-MTD-VAR          PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  D-MTD-VAR-NO       REDEFINES D-MTD-VAR
                                  PIC ZZ,ZZZ,ZZZ,ZZZ-.
           03  D-YTD-VAR          PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  D-YTD-VAR-NO       REDEFINES D-YTD-VAR
                                  PIC ZZ,ZZZ,ZZZ,ZZZ-.
           03  D-MTD-VAR-PCT      PIC ZZZZ.ZZ-.
           03  D-YTD-VAR-PCT      PIC ZZZZ.ZZ-.

       01  DETAIL2                REDEFINES DETAIL-LINE.
          03  FILLER              PIC X(3).
          03  D-BONUS-LINE1       PIC X(23).
          03  FILLER              PIC X(5).
          03  D-BONUS1            PIC ZZ,ZZ9.99.
          03  D-BONUS-LINE1B      PIC X(21).
          03  D-BONUS2            PIC ZZ,ZZ9.99.

       01  DETAIL3                REDEFINES DETAIL-LINE.
           03  D-LEVEL-DESC       PIC X(132).

       01  RANGE-LINE            REDEFINES DETAIL-LINE.
           03  RANGE-DESC        PIC X(20).
           03  FILLER            PIC X(2).
           03  RANGE-LOW-AREA.
               05  RANGE-LOW     PIC X(15).
           03  RANGE-LOW-AREAN   REDEFINES RANGE-LOW-AREA.
               05  RANGE-LOWN    PIC Z(15).
           03  RANGE-LOW-DEC     REDEFINES RANGE-LOW-AREA.
               05  RANGE-LOWDEC  PIC ZZZ,ZZZ,ZZZ.99-.
           03  RANGE-HIGH-AREA.
               05  RANGE-HIGH    PIC X(15).
           03  RANGE-HIGH-AREAN  REDEFINES RANGE-HIGH-AREA.
               05  RANGE-HIGHN   PIC Z(15).
           03  RANGE-HIGH-DEC    REDEFINES RANGE-HIGH-AREA.
               05  RANGE-HIGHDEC PIC ZZZ,ZZZ,ZZZ.99-.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/RATING_DEF.CPY".
       COPY "LIBSP/RATING_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/RATING_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE RATING-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-WR1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               RATING-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "RATING" TO VDU-FORM-NAME.

      * NOW THAT THESE ARE EXTERNAL AND NOT SAVED IN THE FORM LIKE A15,
      * WE CAN NOT RESET THEM!
      *    MOVE " "        TO TOTAL-LEV-TABLE.
      *    MOVE "Y"        TO TOTAL-LEVEL-TABLE(9).
           MOVE LOW-VALUES TO TOTAL-TABLE.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

      *----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.
           PERFORM SEARCH-GR-NEXT.

      * LEVELS START AT THE SAME POINT (THE SELECTED PARENT GROUP)

           IF WGR-SRESULT = 0
              MOVE ZEROS TO WGR-SCURR-TABLE
              MOVE 15 TO WGR-TPLUS
              PERFORM TOTAL-LEVEL-CHECK
              GO TO END-ROUTINE.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE WGR-SCURR-TABLE TO PREV-SCURR-TABLE  
           ELSE
              PERFORM TOTAL-LEVEL-CHECK.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              PERFORM PRINT-ONE-BRANCH-GRAND
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "REPORT IN PROGRESS...............", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.


           PERFORM OPEN-WR1-FILE.

           MOVE CUR-YYYYMM-DATE TO WR-YYYYMM.

           IF GROUP-FLAG = "Y"
              MOVE WGR-SRESULT TO WR-BRNO
           ELSE
              MOVE BR-NO       TO WR-BRNO.

           MOVE "Y"            TO FIRST-TIME-BRNO-SW.

      *    PERFORM START-WR1-FILE.

      *----------------------------------------------------------------
       NEXT-WR1.
      *    PERFORM READ-WR1-FILE-NEXT.


      *   BECAUSE THERE IS ONLY 1 ROW ON THE WRFILE TABLE FOR A GIVEN
      * BRANCH/MONTH-YEAR, CHANGED TO PERFORM A READ, NOT A READ-NEXT.
      * BUT SINCE THIS WAS A LOOP PROCESS, ADDED FIRST-TIME-BRNO-SW,
      * TO CONTINUE THE LOOP PROCESS AS IF IT WERE A READ-NEXT PROCESS.
      * IT IS SET TO "Y" BEFORE THE SINGLE READ, AND "N" RIGHT AFTER,
      * BUT THE LOOP PROCESSING STAYS AS IT WAS BEFORE. 

           IF FIRST-TIME-BRNO-SW = "N"
              IF GROUP-FLAG = "Y"
                 GO TO END-OF-GROUP
              ELSE
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-OF-GROUP.

           PERFORM READ-WR1-FILE.
           MOVE "N"        TO FIRST-TIME-BRNO-SW.

           IF IO-FG NOT     = 0
              IF GROUP-FLAG = "Y"
                 GO TO END-OF-GROUP
              ELSE
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-OF-GROUP.

           IF WR-YYYYMM NOT = CUR-YYYYMM-DATE
              IF GROUP-FLAG = "Y"
                 GO TO END-OF-GROUP
              ELSE
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-OF-GROUP.

           IF GROUP-FLAG = "Y"
              IF WR-BRNO NOT = WGR-SRESULT
                 GO TO END-OF-GROUP.

           IF GROUP-FLAG NOT = "Y"
              IF WR-BRNO NOT = BR-NO
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-OF-GROUP.

      *----------------------------------------------------------------
      *    OK-TO-PROCESS
      *----------------------------------------------------------------
           MOVE "N" TO FIRST-TIME.
           PERFORM CHECK-ADD-TOTALS.
           GO TO NEXT-WR1.

      *----------------------------------------------------------------
       END-OF-GROUP.
      *    PERFORM CLOSE-WR1-FILE.
           IF GROUP-FLAG = "Y"
              GO TO NEXT-GROUP-LOC
           ELSE
              GO TO NEXT-BRANCH-NO.
      *----------------------------------------------------------------
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           IF EXT-EOBUF-GROUP-FG NOT = "Y"
              PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ******************************************************************
       CHECK-ADD-TOTALS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "N"
              MOVE 9 TO TOT-SUB
              PERFORM ADD-TOTALS
              MOVE 1 TO TOT-SUB
              PERFORM ADD-TOTALS
              GO TO EXIT-CHECK-TOTALS.

           MOVE WGR-TPLUS TO GROUP-SUB.

       CHECK-TOTAL-LOOP.
           IF GROUP-SUB = 0
              GO TO EXIT-CHECK-TOTALS.

           MOVE WGR-SCURR(GROUP-SUB) TO LEVEL-SUB.

           IF WGR-SID(LEVEL-SUB) IS NUMERIC
              MOVE 9 TO TOT-SUB
              GO TO CHECK-TOTAL-FLAG.

           MOVE WGR-SID(LEVEL-SUB) TO GR-ID.
           PERFORM READ-GR1-FILE.
           MOVE GR-LEVEL TO TOT-SUB.

      *----------------------------------------------------------------
       CHECK-TOTAL-FLAG.
           IF TOTAL-LEVEL-TABLE(TOT-SUB) NOT = "Y"
              GO TO CONTINUE-CHECK-TOTALS.

           PERFORM ADD-TOTALS.

      *----------------------------------------------------------------
       CONTINUE-CHECK-TOTALS.
           SUBTRACT 1 FROM GROUP-SUB.

           IF GROUP-SUB > 0
              GO TO CHECK-TOTAL-LOOP.

       EXIT-CHECK-TOTALS.
           EXIT.

      ******************************************************************
       ADD-TOTALS SECTION.
      ******************************************************************

      * MOVE CURRENT WR-REC TO THIS MONTH WRTM RECORD
      *   - AND READ LAST YEAR  WRLY-REC
      *   - PROCESS
      * RESTORE THIS MONTH WRTM RECORD AND RESTART FILE:

      * MOVE CURRENT WR-REC TO THIS MONTH WRTM RECORD

           MOVE WR-REC TO WRTM-REC.

      * READ LAST YEAR WRLY-REC:
           MOVE LAST-YYYYMM-DATE TO WR-YYYYMM.
           PERFORM READ-WR1-FILE.
           IF IO-FG = 9
              PERFORM CLEAR-WR-FILE.
           MOVE WR-REC TO WRLY-REC.


           ADD 1 TO T-COUNT(TOT-SUB).
      
      * LINE# 01: OPEN CUSTOMERS
      
           MOVE 01 TO SUB.
           ADD WRTM-CUR-NO-CUST TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-CUR-NO-CUST TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-CUR-NO-CUST TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-CUR-NO-CUST TO T-LYR-YTD(TOT-SUB,SUB).

      * LINE# 02: CUSTOMER BALANCE
      
           MOVE 02 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) = T-MTD(TOT-SUB,SUB)
                   + (WRTM-CUR-NO-CUST - WRTM-ELM-NO-CUST).
           COMPUTE T-YTD(TOT-SUB,SUB) = T-YTD(TOT-SUB,SUB)
                   + (WRTM-CUR-NO-CUST - WRTM-ELY-NO-CUST).
           COMPUTE T-LYR-MTH(TOT-SUB,SUB) = T-LYR-MTH(TOT-SUB,SUB)
                   + (WRLY-CUR-NO-CUST - WRLY-ELM-NO-CUST).
           COMPUTE T-LYR-YTD(TOT-SUB,SUB) = T-LYR-YTD(TOT-SUB,SUB)
                   + (WRLY-CUR-NO-CUST - WRLY-ELY-NO-CUST).

      * LINE# 03: LEDGER BALANCE
      
           MOVE 03 TO SUB.
           ADD WRTM-OUTS-BALANCE TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-OUTS-BALANCE TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-OUTS-BALANCE TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-OUTS-BALANCE TO T-LYR-YTD(TOT-SUB,SUB).

      * LINE# 04: LEDGER GAIN
      
           MOVE 04 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) = T-MTD(TOT-SUB,SUB)
                   + (WRTM-OUTS-BALANCE - WRTM-ELM-OUTS-BALANCE).
           COMPUTE T-YTD(TOT-SUB,SUB) = T-YTD(TOT-SUB,SUB)
                   + (WRTM-OUTS-BALANCE - WRTM-ELY-OUTS-BALANCE).
           COMPUTE T-LYR-MTH(TOT-SUB,SUB) = T-LYR-MTH(TOT-SUB,SUB)
                   + (WRLY-OUTS-BALANCE - WRLY-ELM-OUTS-BALANCE).
           COMPUTE T-LYR-YTD(TOT-SUB,SUB) = T-LYR-YTD(TOT-SUB,SUB)
                   + (WRLY-OUTS-BALANCE - WRLY-ELY-OUTS-BALANCE).

      * LINE# 05: NO. NEW CUSTOMERS
      
           MOVE 05 TO SUB.
           ADD WRTM-NO-NB(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-NO-NB(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-NO-NB(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-NO-NB(2) TO T-LYR-YTD(TOT-SUB,SUB).

      * LINE# 06: NO. FORMER CUSTOMERS
      
           MOVE 06 TO SUB.
           ADD WRTM-NO-FB(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-NO-FB(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-NO-FB(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-NO-FB(2) TO T-LYR-YTD(TOT-SUB,SUB).

      * LINE# 07: NO. RENEWAL CUSTOMERS
      
           MOVE 07 TO SUB.
           ADD WRTM-NO-PB(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-NO-PB(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-NO-PB(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-NO-PB(2) TO T-LYR-YTD(TOT-SUB,SUB).

      * LINE# 08: TOTAL # LOANS MADE
      
           MOVE 08 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) =
                   T-MTD(TOT-SUB,5) + T-MTD(TOT-SUB,6)
                                       + T-MTD(TOT-SUB,7).
           COMPUTE T-YTD(TOT-SUB,SUB) =
                   T-YTD(TOT-SUB,5) + T-YTD(TOT-SUB,6)
                                       + T-YTD(TOT-SUB,7).
           COMPUTE T-LYR-MTH(TOT-SUB,SUB) =
                   T-LYR-MTH(TOT-SUB,5) + T-LYR-MTH(TOT-SUB,6)
                                    + T-LYR-MTH(TOT-SUB,7).
           COMPUTE T-LYR-YTD(TOT-SUB,SUB) =
                   T-LYR-YTD(TOT-SUB,5) + T-LYR-YTD(TOT-SUB,6)
                                    + T-LYR-YTD(TOT-SUB,7).

      * LINE# 09: GROSS LOAN VOLUME
      
           MOVE 09 TO SUB.
           ADD WRTM-GROSS-VOLUME(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-GROSS-VOLUME(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-GROSS-VOLUME(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-GROSS-VOLUME(2) TO T-LYR-YTD(TOT-SUB,SUB).

      * LINE# 10: PERCENT TURNOVER
      
           MOVE 10 TO SUB.
           COMPUTE WORK-AMT = T-MTD(TOT-SUB,3) - T-MTD(TOT-SUB,4).
           IF WORK-AMT NOT = 0
              COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
                    100 * T-MTD(TOT-SUB,9) / WORK-AMT.

           ADD WRTM-YTD-PCT-TURNOVER TO T-YTD(TOT-SUB, SUB).

           COMPUTE WORK-AMT =
                      T-LYR-MTH(TOT-SUB,3) - T-LYR-MTH(TOT-SUB,4).
           IF WORK-AMT NOT = 0
              COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
                    100 * T-LYR-MTH(TOT-SUB,9) / WORK-AMT.

           ADD WRLY-YTD-PCT-TURNOVER TO T-LYR-YTD(TOT-SUB, SUB).

      * LINE# 11: AVG UNPAID BALANCE
      
           MOVE 11 TO SUB.
           IF T-MTD(TOT-SUB,1) NOT = 0
              COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
                      T-MTD(TOT-SUB,3) / T-MTD(TOT-SUB,1).
           IF T-YTD(TOT-SUB,1) NOT = 0
              COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
                      T-YTD(TOT-SUB,3) / T-YTD(TOT-SUB,1).
           IF T-LYR-MTH(TOT-SUB,1) NOT = 0
              COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
                      T-LYR-MTH(TOT-SUB,3) / T-LYR-MTH(TOT-SUB,1).
           IF T-LYR-YTD(TOT-SUB,1) NOT = 0
              COMPUTE T-LYR-YTD(TOT-SUB,SUB) ROUNDED =
                      T-LYR-YTD(TOT-SUB,3) / T-LYR-YTD(TOT-SUB,1).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 12: PRE-TAX PROFIT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 12 TO SUB.
      *    ADD WRTM-PRETAX-PROFIT(1) TO T-MTD(TOT-SUB,SUB).
      *    ADD WRTM-PRETAX-PROFIT(2) TO T-YTD(TOT-SUB,SUB).
      *    ADD WRLY-PRETAX-PROFIT(1) TO T-LYR-MTH(TOT-SUB,SUB).
      *    ADD WRLY-PRETAX-PROFIT(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 13: WCBC PROFIT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 13 TO SUB.
      *    ADD 0 TO T-MTD(TOT-SUB,SUB).
      *    ADD 0 TO T-YTD(TOT-SUB,SUB).
      *    ADD 0 TO T-LYR-MTH(TOT-SUB,SUB).
      *    ADD 0 TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 14: INTEREST ALLOCATION
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 14 TO SUB.
      *    ADD WRTM-INTEREST-ALLOCATION(1) TO T-MTD(TOT-SUB,SUB).
      *    ADD WRTM-INTEREST-ALLOCATION(2) TO T-YTD(TOT-SUB,SUB).
      *    ADD WRLY-INTEREST-ALLOCATION(1) TO T-LYR-MTH(TOT-SUB,SUB).
      *    ADD WRLY-INTEREST-ALLOCATION(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 15: SUPERVISION
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 15 TO SUB.
      *    ADD WRTM-SUPERVISION(1) TO T-MTD(TOT-SUB,SUB).
      *    ADD WRTM-SUPERVISION(2) TO T-YTD(TOT-SUB,SUB).
      *    ADD WRLY-SUPERVISION(1) TO T-LYR-MTH(TOT-SUB,SUB).
      *    ADD WRLY-SUPERVISION(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 16: GENERAL RESERVE ADJUSTMENT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 16 TO SUB.
      *    ADD WRTM-GEN-RESERVE-ADJ(1) TO T-MTD(TOT-SUB,SUB).
      *    ADD WRTM-GEN-RESERVE-ADJ(2) TO T-YTD(TOT-SUB,SUB).
      *    ADD WRLY-GEN-RESERVE-ADJ(1) TO T-LYR-MTH(TOT-SUB,SUB).
      *    ADD WRLY-GEN-RESERVE-ADJ(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 17: INTANGIBLE AMORTIZATION
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 17 TO SUB.
      *    ADD WRTM-INTANGIBLE-AMORT(1) TO T-MTD(TOT-SUB,SUB).
      *    ADD WRTM-INTANGIBLE-AMORT(2) TO T-YTD(TOT-SUB,SUB).
      *    ADD WRLY-INTANGIBLE-AMORT(1) TO T-LYR-MTH(TOT-SUB,SUB).
      *    ADD WRLY-INTANGIBLE-AMORT(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 18: ROBBERIES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 18 TO SUB.
      *    ADD WRTM-ROBBERIES(1) TO T-MTD(TOT-SUB,SUB).
      *    ADD WRTM-ROBBERIES(2) TO T-YTD(TOT-SUB,SUB).
      *    ADD WRLY-ROBBERIES(1) TO T-LYR-MTH(TOT-SUB,SUB).
      *    ADD WRLY-ROBBERIES(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 19: RATING SHEET PROFIT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 19 TO SUB.
      *    COMPUTE T-MTD(TOT-SUB,SUB) =
      *            T-MTD(TOT-SUB,12) + T-MTD(TOT-SUB,13)
      *          + T-MTD(TOT-SUB,14) + T-MTD(TOT-SUB,15)
      *          + T-MTD(TOT-SUB,16) + T-MTD(TOT-SUB,17)
      *          + T-MTD(TOT-SUB,18).
      *    COMPUTE T-YTD(TOT-SUB,SUB) =
      *            T-YTD(TOT-SUB,12) + T-YTD(TOT-SUB,13)
      *          + T-YTD(TOT-SUB,14) + T-YTD(TOT-SUB,15)
      *          + T-YTD(TOT-SUB,16) + T-YTD(TOT-SUB,17)
      *          + T-YTD(TOT-SUB,18).
      *    COMPUTE T-LYR-MTH(TOT-SUB,SUB) =
      *            T-LYR-MTH(TOT-SUB,12) + T-LYR-MTH(TOT-SUB,13)
      *          + T-LYR-MTH(TOT-SUB,14) + T-LYR-MTH(TOT-SUB,15)
      *          + T-LYR-MTH(TOT-SUB,16) + T-LYR-MTH(TOT-SUB,17)
      *          + T-LYR-MTH(TOT-SUB,18).
      *    COMPUTE T-LYR-YTD(TOT-SUB,SUB) =
      *            T-LYR-YTD(TOT-SUB,12) + T-LYR-YTD(TOT-SUB,13)
      *          + T-LYR-YTD(TOT-SUB,14) + T-LYR-YTD(TOT-SUB,15)
      *          + T-LYR-YTD(TOT-SUB,16) + T-LYR-YTD(TOT-SUB,17)
      *          + T-LYR-YTD(TOT-SUB,18).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 20: % RATING SHEET PROFIT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 20 TO SUB.
      *    COMPUTE WORK-AMT =
      *               T-MTD(TOT-SUB,3) - T-MTD(TOT-SUB,4).
      *    IF WORK-AMT NOT = 0
      *       COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
      *              100 * T-MTD(TOT-SUB,19)
      *            / WORK-AMT.

      *    IF TOT-SUB = 9
      *       ADD WRTM-YTD-PCT-RATE-SHT-PROFIT TO T-YTD(TOT-SUB, SUB)
      *    ELSE
      *       COMPUTE WORK-AMT =
      *               T-YTD(TOT-SUB,3) - T-YTD(TOT-SUB,4)
      *       IF WORK-AMT NOT = 0
      *          COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
      *                 100 * T-YTD(TOT-SUB,19)
      *               / WORK-AMT.

      *    COMPUTE WORK-AMT =
      *               T-LYR-MTH(TOT-SUB,3) - T-LYR-MTH(TOT-SUB,4).
      *    IF WORK-AMT NOT = 0
      *       COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
      *                 100 * T-LYR-MTH(TOT-SUB,19)
      *               / WORK-AMT.

      *    COMPUTE WORK-AMT =
      *                  T-LYR-YTD(TOT-SUB,3) - T-LYR-YTD(TOT-SUB,4).
      *    IF TOT-SUB = 9
      *      ADD WRLY-YTD-PCT-RATE-SHT-PROFIT TO T-LYR-YTD(TOT-SUB, SUB)
      *    ELSE
      *       IF WORK-AMT NOT = 0
      *          COMPUTE T-LYR-YTD(TOT-SUB,SUB) ROUNDED =
      *                    100 * T-LYR-YTD(TOT-SUB,19)
      *                  / WORK-AMT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 21: RATING SHEET INCOME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 21 TO SUB.
      *    ADD WRTM-RATING-SHEET-INCOME(1) TO T-MTD(TOT-SUB,SUB).
      *    ADD WRTM-RATING-SHEET-INCOME(2) TO T-YTD(TOT-SUB,SUB).
      *    ADD WRLY-RATING-SHEET-INCOME(1) TO T-LYR-MTH(TOT-SUB,SUB).
      *    ADD WRLY-RATING-SHEET-INCOME(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 22: % RS INC TO BEG LEDGER
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 22 TO SUB.
      *    COMPUTE WORK-AMT =
      *              T-MTD(TOT-SUB,3) - T-MTD(TOT-SUB,4).
      *    IF WORK-AMT NOT = 0
      *       COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
      *                 100 * T-MTD(TOT-SUB,21)
      *               / WORK-AMT.

      *     COMPUTE WORK-AMT =
      *                 T-YTD(TOT-SUB,3) - T-YTD(TOT-SUB,4).
      *    IF TOT-SUB = 9
      *       ADD WRTM-YTD-PCT-RS-INC-LEDGER TO T-YTD(TOT-SUB, SUB)
      *    ELSE
      *        IF WORK-AMT NOT = 0
      *           COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
      *                 100 * T-YTD(TOT-SUB,21)
      *                / WORK-AMT.

      *    COMPUTE WORK-AMT =
      *              (T-LYR-MTH(TOT-SUB,3) - T-LYR-MTH(TOT-SUB,4)).
      *    IF WORK-AMT NOT = 0
      *       COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
      *              100 * T-LYR-MTH(TOT-SUB,21) / WORK-AMT.

      *    IF TOT-SUB = 9
      *       ADD WRLY-YTD-PCT-RS-INC-LEDGER TO T-LYR-YTD(TOT-SUB, SUB)
      *    ELSE
      *       COMPUTE WORK-AMT =
      *                  T-LYR-YTD(TOT-SUB,3) - T-LYR-YTD(TOT-SUB,4)
      *       IF WORK-AMT NOT = 0
      *          COMPUTE T-LYR-YTD(TOT-SUB,SUB) ROUNDED =
      *                 100 * T-LYR-YTD(TOT-SUB,21)
      *               / WORK-AMT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 23: INTEREST & FEE INCOME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 23 TO SUB.
      *    ADD WRTM-INT-FEE-INCOME(1) TO T-MTD(TOT-SUB,SUB).
      *    ADD WRTM-INT-FEE-INCOME(2) TO T-YTD(TOT-SUB,SUB).
      *    ADD WRLY-INT-FEE-INCOME(1) TO T-LYR-MTH(TOT-SUB,SUB).
      *    ADD WRLY-INT-FEE-INCOME(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 24: % INT & FEE INCOME PER BEGIN LEDGER
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 24 TO SUB.
      *    COMPUTE WORK-AMT =
      *               T-MTD(TOT-SUB,3) - T-MTD(TOT-SUB,4).
      *    IF WORK-AMT NOT = 0
      *       COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
      *              100 * T-MTD(TOT-SUB,23)
      *             / WORK-AMT.

      *    COMPUTE WORK-AMT =
      *                T-YTD(TOT-SUB,3) - T-YTD(TOT-SUB,4).
      *    IF TOT-SUB = 9
      *       ADD WRTM-YTD-PCT-INT-FEE-LEDGER TO T-YTD(TOT-SUB, SUB)
      *    ELSE
      *       IF WORK-AMT NOT = 0
      *          COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
      *                 100 * T-YTD(TOT-SUB,23)
      *                / WORK-AMT.

      *    COMPUTE WORK-AMT =
      *                T-LYR-MTH(TOT-SUB,3) - T-LYR-MTH(TOT-SUB,4).
      *    IF WORK-AMT NOT = 0
      *       COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
      *              100 * T-LYR-MTH(TOT-SUB,23)
      *             / WORK-AMT.

      *    COMPUTE WORK-AMT =
      *                 T-LYR-YTD(TOT-SUB,3) - T-LYR-YTD(TOT-SUB,4).
      *    IF TOT-SUB = 9
      *       ADD WRLY-YTD-PCT-INT-FEE-LEDGER TO T-LYR-YTD(TOT-SUB, SUB)
      *    ELSE
      *       IF WORK-AMT NOT = 0
      *          COMPUTE T-LYR-YTD(TOT-SUB,SUB) ROUNDED =
      *                 100 * T-LYR-YTD(TOT-SUB,23)
      *                / WORK-AMT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 25: INSURANCE INCOME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 25 TO SUB.
      *    ADD WRTM-INSURANCE-INCOME(1) TO T-MTD(TOT-SUB,SUB).
      *    ADD WRTM-INSURANCE-INCOME(2) TO T-YTD(TOT-SUB,SUB).
      *    ADD WRLY-INSURANCE-INCOME(1) TO T-LYR-MTH(TOT-SUB,SUB).
      *    ADD WRLY-INSURANCE-INCOME(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 26: % INSURANCE INCOME TO BEGIN LEDGER
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 26 TO SUB.
      *    COMPUTE WORK-AMT =
      *              T-MTD(TOT-SUB,3) - T-MTD(TOT-SUB,4).
      *    IF WORK-AMT NOT = 0
      *       COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
      *               100 * T-MTD(TOT-SUB,25) / WORK-AMT.

      *    IF TOT-SUB = 9
      *       ADD WRTM-YTD-PCT-INS-INC-LEDGER TO T-YTD(TOT-SUB, SUB)
      *    ELSE
      *       COMPUTE WORK-AMT =
      *              T-YTD(TOT-SUB,3) - T-YTD(TOT-SUB,4)
      *       IF WORK-AMT NOT = 0
      *          COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
      *                  100 * T-YTD(TOT-SUB,25)
      *                / WORK-AMT.

      *    COMPUTE WORK-AMT =
      *               T-LYR-MTH(TOT-SUB,3) - T-LYR-MTH(TOT-SUB,4).
      *    IF WORK-AMT NOT = 0
      *       COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
      *               100 * T-LYR-MTH(TOT-SUB,25) / WORK-AMT.

      *    IF TOT-SUB = 9
      *       ADD WRLY-YTD-PCT-INS-INC-LEDGER TO T-LYR-YTD(TOT-SUB, SUB)
      *    ELSE
      *       COMPUTE WORK-AMT =
      *               T-LYR-YTD(TOT-SUB,3) - T-LYR-YTD(TOT-SUB,4)
      *       IF WORK-AMT NOT = 0
      *          COMPUTE T-LYR-YTD(TOT-SUB,SUB) ROUNDED =
      *                  100 * T-LYR-YTD(TOT-SUB,25)
      *                / WORK-AMT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 27: RATING SHEET EXPENSES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 27 TO SUB.
      *    ADD WRTM-RATING-SHEET-EXPENSES(1) TO T-MTD(TOT-SUB,SUB).
      *    ADD WRTM-RATING-SHEET-EXPENSES(2) TO T-YTD(TOT-SUB,SUB).
      *    ADD WRLY-RATING-SHEET-EXPENSES(1) TO T-LYR-MTH(TOT-SUB,SUB).
      *    ADD WRLY-RATING-SHEET-EXPENSES(2) TO T-LYR-YTD(TOT-SUB,SUB).
 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 28: EXPENSE PER LOAN MADE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 28 TO SUB.
      *    IF T-MTD(TOT-SUB,8) NOT = 0
      *       COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
      *            T-MTD(TOT-SUB,27) / T-MTD(TOT-SUB,8).
      *    IF T-YTD(TOT-SUB,8) NOT = 0
      *       COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
      *            T-YTD(TOT-SUB,27) / T-YTD(TOT-SUB,8).
      *    IF T-LYR-MTH(TOT-SUB,8) NOT = 0
      *       COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
      *            T-LYR-MTH(TOT-SUB,27) / T-LYR-MTH(TOT-SUB,8).
      *    IF T-LYR-YTD(TOT-SUB,8) NOT = 0
      *       COMPUTE T-LYR-YTD(TOT-SUB,SUB) ROUNDED =
      *            T-LYR-YTD(TOT-SUB,27) / T-LYR-YTD(TOT-SUB,8).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 29: % EXPENSES TO BEGINNING OF LEDGER
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 29 TO SUB.
      *    COMPUTE WORK-AMT =
      *               T-MTD(TOT-SUB,3) - T-MTD(TOT-SUB,4).
      *    IF WORK-AMT NOT = 0
      *       COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
      *              100 * T-MTD(TOT-SUB,27) / WORK-AMT.
      *    COMPUTE WORK-AMT =
      *               T-YTD(TOT-SUB,3) - T-YTD(TOT-SUB,4).
      *    IF TOT-SUB = 9
      *       ADD WRTM-YTD-PCT-EXPENSE-LEDGER TO T-YTD(TOT-SUB, SUB)
      *    ELSE
      *       IF WORK-AMT NOT = 0
      *          COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
      *                 100 * T-YTD(TOT-SUB,27) / WORK-AMT.
 
      *    COMPUTE WORK-AMT =
      *               T-LYR-MTH(TOT-SUB,3) - T-LYR-MTH(TOT-SUB,4).
      *    IF WORK-AMT NOT = 0
      *       COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
      *              100 * T-LYR-MTH(TOT-SUB,27) / WORK-AMT.

      *    COMPUTE WORK-AMT =
      *                 T-LYR-YTD(TOT-SUB,3) - T-LYR-YTD(TOT-SUB,4).
      *    IF TOT-SUB = 9
      *       ADD WRLY-YTD-PCT-EXPENSE-LEDGER TO T-LYR-YTD(TOT-SUB, SUB)
      *    ELSE
      *       IF WORK-AMT NOT = 0
      *          COMPUTE T-LYR-YTD(TOT-SUB,SUB) ROUNDED =
      *                 100 * T-LYR-YTD(TOT-SUB,27)
      *                 / WORK-AMT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 30: EXPENSES AS A PERCENT OF INCOME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 30 TO SUB.
      *    IF T-MTD(TOT-SUB,21) NOT = 0
      *       COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
      *           100 * T-MTD(TOT-SUB,27) / T-MTD(TOT-SUB,21).
      *    IF T-YTD(TOT-SUB,21) NOT = 0
      *       COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
      *           100 * T-YTD(TOT-SUB,27) / T-YTD(TOT-SUB,21).
      *    IF T-LYR-MTH(TOT-SUB,21) NOT = 0
      *       COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
      *           100 * T-LYR-MTH(TOT-SUB,27) / T-LYR-MTH(TOT-SUB,21).
      *    IF T-LYR-YTD(TOT-SUB,21) NOT = 0
      *       COMPUTE T-LYR-YTD(TOT-SUB,SUB) ROUNDED =
      *           100 * T-LYR-YTD(TOT-SUB,27) / T-LYR-YTD(TOT-SUB,21).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 31: PERSONNEL EXPENSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 31 TO SUB.
      *    ADD WRTM-PERSONNEL-EXPENSE(1) TO T-MTD(TOT-SUB,SUB).
      *    ADD WRTM-PERSONNEL-EXPENSE(2) TO T-YTD(TOT-SUB,SUB).
      *    ADD WRLY-PERSONNEL-EXPENSE(1) TO T-LYR-MTH(TOT-SUB,SUB).
      *    ADD WRLY-PERSONNEL-EXPENSE(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 32: PERSONNEL AS A PERCENT OF INCOME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 32 TO SUB.
      *    IF T-MTD(TOT-SUB,21) NOT = 0
      *       COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
      *           100 * T-MTD(TOT-SUB,31) / T-MTD(TOT-SUB,21).
      *    IF T-YTD(TOT-SUB,21) NOT = 0
      *       COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
      *           100 * T-YTD(TOT-SUB,31) / T-YTD(TOT-SUB,21).
      *    IF T-LYR-MTH(TOT-SUB,21) NOT = 0
      *       COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
      *           100 * T-LYR-MTH(TOT-SUB,31) / T-LYR-MTH(TOT-SUB,21).
      *    IF T-LYR-YTD(TOT-SUB,21) NOT = 0
      *       COMPUTE T-LYR-YTD(TOT-SUB,SUB) ROUNDED =
      *           100 * T-LYR-YTD(TOT-SUB,31) / T-LYR-YTD(TOT-SUB,21).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 33: CHARGEOFF EXPENSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 33 TO SUB
      *    ADD WRTM-CHARGEOFF-EXPENSE(1) TO T-MTD(TOT-SUB,SUB).
      *    ADD WRTM-CHARGEOFF-EXPENSE(2) TO T-YTD(TOT-SUB,SUB).
      *    ADD WRLY-CHARGEOFF-EXPENSE(1) TO T-LYR-MTH(TOT-SUB,SUB).
      *    ADD WRLY-CHARGEOFF-EXPENSE(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 34: NUMBER OF CHARGEOFFS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 34 TO SUB.
           ADD WRTM-NO-CHARGEOFFS(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-NO-CHARGEOFFS(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-NO-CHARGEOFFS(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-NO-CHARGEOFFS(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 35: RECOVERY INCOME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 35 TO SUB.
      *    ADD WRTM-RECOVERY-INCOME(1) TO T-MTD(TOT-SUB,SUB).
      *    ADD WRTM-RECOVERY-INCOME(2) TO T-YTD(TOT-SUB,SUB).
      *    ADD WRLY-RECOVERY-INCOME(1) TO T-LYR-MTH(TOT-SUB,SUB).
      *    ADD WRLY-RECOVERY-INCOME(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 36: BAD DEBT INCREASE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 36 TO SUB.
      *    ADD WRTM-BAD-DEBT-INCREASE(1) TO T-MTD(TOT-SUB,SUB).
      *    ADD WRTM-BAD-DEBT-INCREASE(2) TO T-YTD(TOT-SUB,SUB).
      *    ADD WRLY-BAD-DEBT-INCREASE(1) TO T-LYR-MTH(TOT-SUB,SUB).
      *    ADD WRLY-BAD-DEBT-INCREASE(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 37: % BAD DEBT TO BEGINNING OF LEDGER BALANCE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 37 TO SUB
      *    COMPUTE WORK-AMT =
      *               T-MTD(TOT-SUB,3) - T-MTD(TOT-SUB,4).
      *    IF WORK-AMT NOT = 0
      *       COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
      *              100 * T-MTD(TOT-SUB,36) / WORK-AMT.

      *    IF TOT-SUB = 9
      *       ADD WRTM-YTD-PCT-BAD-DEBT-LEDGER TO T-YTD(TOT-SUB, SUB)
      *    ELSE
      *       COMPUTE WORK-AMT =
      *               T-YTD(TOT-SUB,3) - T-YTD(TOT-SUB,4)
      *       IF WORK-AMT NOT = 0
      *          COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
      *                 100 * T-YTD(TOT-SUB,36)
      *                 / WORK-AMT.

      *    COMPUTE WORK-AMT =
      *            T-LYR-MTH(TOT-SUB,3) - T-LYR-MTH(TOT-SUB,4).
      *    IF WORK-AMT NOT = 0
      *       COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
      *              100 * T-LYR-MTH(TOT-SUB,36) / WORK-AMT.

      *    COMPUTE WORK-AMT =
      *                  T-LYR-YTD(TOT-SUB,3) - T-LYR-YTD(TOT-SUB,4).
      *    IF TOT-SUB = 9
      *      ADD WRLY-YTD-PCT-BAD-DEBT-LEDGER TO T-LYR-YTD(TOT-SUB, SUB)
      *    ELSE
      *       IF WORK-AMT NOT = 0
      *          COMPUTE T-LYR-YTD(TOT-SUB,SUB) ROUNDED =
      *                 100 * T-LYR-YTD(TOT-SUB,36)
      *                 / WORK-AMT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 38: NF CLAIMS-BANKRUPT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 38 TO SUB.
           ADD WRTM-NF-BK-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-NF-BK-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-NF-BK-CLAIMS(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-NF-BK-CLAIMS(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 39: % BANKRUPT NF CLAIMS TO BEGINNING OF LEDGER BALANCE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 39 TO SUB.
           COMPUTE WORK-AMT =
                      T-MTD(TOT-SUB,3) - T-MTD(TOT-SUB,4).
           IF WORK-AMT NOT = 0
              COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
                     100 * T-MTD(TOT-SUB,38) / WORK-AMT.

           IF TOT-SUB = 9
              ADD WRTM-YTD-PCT-NF-BNK-LEDGER TO T-YTD(TOT-SUB, SUB)
           ELSE
              COMPUTE WORK-AMT =
                      T-YTD(TOT-SUB,3) - T-YTD(TOT-SUB,4)
              IF WORK-AMT NOT = 0
                 COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
                        100 * T-YTD(TOT-SUB,38)
                        / WORK-AMT.

           COMPUTE WORK-AMT =
                      T-LYR-MTH(TOT-SUB,3) - T-LYR-MTH(TOT-SUB,4).
           IF WORK-AMT NOT = 0
              COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
                     100 * T-LYR-MTH(TOT-SUB,38) / WORK-AMT.

           COMPUTE WORK-AMT =
                         T-LYR-YTD(TOT-SUB,3) - T-LYR-YTD(TOT-SUB,4).
           IF TOT-SUB = 9
              ADD WRLY-YTD-PCT-NF-BNK-LEDGER TO T-LYR-YTD(TOT-SUB, SUB)
           ELSE
              IF WORK-AMT NOT = 0
                 COMPUTE T-LYR-YTD(TOT-SUB,SUB) ROUNDED =
                        100 * T-LYR-YTD(TOT-SUB,38)
                       / WORK-AMT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 40: NF CLAIMS-NON-BANKRUPT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 40 TO SUB.
           ADD WRTM-NF-NON-BK-CLAIMS(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-LOTS-NF-CLAIMS(1)   TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-NF-NON-BK-CLAIMS(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRTM-LOTS-NF-CLAIMS(2)   TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-NF-NON-BK-CLAIMS(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-LOTS-NF-CLAIMS(1)   TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-NF-NON-BK-CLAIMS(2) TO T-LYR-YTD(TOT-SUB,SUB).
           ADD WRLY-LOTS-NF-CLAIMS(2)   TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 41: % PRETAX INCOME TO BEGINNING OF LEDGER BALANCE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 41 TO SUB.
      *    COMPUTE WORK-AMT =
      *              T-MTD(TOT-SUB,3) - T-MTD(TOT-SUB,4).
      *    IF WORK-AMT NOT = 0
      *       COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
      *                 100 * T-MTD(TOT-SUB,12) / WORK-AMT.

      *    IF TOT-SUB = 9
      *       ADD WRTM-YTD-PCT-PRETAX-LEDGER TO T-YTD(TOT-SUB, SUB)
      *    ELSE
      *       COMPUTE WORK-AMT =
      *               T-YTD(TOT-SUB,3) - T-YTD(TOT-SUB,4)
      *       IF WORK-AMT NOT = 0
      *          COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
      *                 100 * T-YTD(TOT-SUB,12)
      *                / WORK-AMT.

      *    COMPUTE WORK-AMT =
      *             T-LYR-MTH(TOT-SUB,3) - T-LYR-MTH(TOT-SUB,4).
      *    IF WORK-AMT NOT = 0
      *       COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
      *                100 * T-LYR-MTH(TOT-SUB,12) / WORK-AMT.

      *    COMPUTE WORK-AMT =
      *                  T-LYR-YTD(TOT-SUB,3) - T-LYR-YTD(TOT-SUB,4).
      *    IF TOT-SUB = 9
      *       ADD WRLY-YTD-PCT-PRETAX-LEDGER TO T-LYR-YTD(TOT-SUB, SUB)
      *    ELSE
      *       IF WORK-AMT NOT = 0
      *          COMPUTE T-LYR-YTD(TOT-SUB,SUB) ROUNDED =
      *                100 * T-LYR-YTD(TOT-SUB,12)
      *               / WORK-AMT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 42: TOTAL NF CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 42 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) =
                   T-MTD(TOT-SUB,38) + T-MTD(TOT-SUB,40).
           COMPUTE T-YTD(TOT-SUB,SUB) =
                   T-YTD(TOT-SUB,38) + T-YTD(TOT-SUB,40).
           COMPUTE T-LYR-MTH(TOT-SUB,SUB) =
                   T-LYR-MTH(TOT-SUB,38) + T-LYR-MTH(TOT-SUB,40).
           COMPUTE T-LYR-YTD(TOT-SUB,SUB) =
                   T-LYR-YTD(TOT-SUB,38) + T-LYR-YTD(TOT-SUB,40).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 43: TOTAL BAD DEBT + TOTAL NF CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 43 TO SUB.
      *    COMPUTE T-MTD(TOT-SUB,SUB) =
      *            T-MTD(TOT-SUB,36) + T-MTD(TOT-SUB,42).
      *    COMPUTE T-YTD(TOT-SUB,SUB) =
      *            T-YTD(TOT-SUB,36) + T-YTD(TOT-SUB,42).
      *    COMPUTE T-LYR-MTH(TOT-SUB,SUB) =
      *            T-LYR-MTH(TOT-SUB,36) + T-LYR-MTH(TOT-SUB,42).
      *    COMPUTE T-LYR-YTD(TOT-SUB,SUB) =
      *            T-LYR-YTD(TOT-SUB,36) + T-LYR-YTD(TOT-SUB,42).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 44: % TOTAL TO BEGINNING OF LEDGER BALANCE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 44 TO SUB.
      *    COMPUTE WORK-AMT =
      *              T-MTD(TOT-SUB,3) - T-MTD(TOT-SUB,4).
      *    IF WORK-AMT NOT = 0
      *       COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
      *              100 * T-MTD(TOT-SUB,43)
      *             / WORK-AMT.

      *    COMPUTE WORK-AMT =
      *                T-YTD(TOT-SUB,3) - T-YTD(TOT-SUB,4).
      *    IF TOT-SUB = 9
      *       ADD WRTM-YTD-PCT-DEBT-NF-LEDGER TO T-YTD(TOT-SUB, SUB)
      *    ELSE
      *       IF WORK-AMT NOT = 0
      *          COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
      *                 100 * T-YTD(TOT-SUB,43)
      *                / WORK-AMT.

      *    COMPUTE WORK-AMT =
      *             T-LYR-MTH(TOT-SUB,3) - T-LYR-MTH(TOT-SUB,4).
      *    IF WORK-AMT NOT = 0
      *       COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
      *              100 * T-LYR-MTH(TOT-SUB,43)
      *             / WORK-AMT.

      *    COMPUTE WORK-AMT =
      *                T-LYR-YTD(TOT-SUB,3) - T-LYR-YTD(TOT-SUB,4).
      *    IF TOT-SUB = 9
      *       ADD WRLY-YTD-PCT-DEBT-NF-LEDGER TO T-LYR-YTD(TOT-SUB, SUB)
      *    ELSE
      *       IF WORK-AMT NOT = 0
      *          COMPUTE T-LYR-YTD(TOT-SUB,SUB) ROUNDED =
      *                 100 * T-LYR-YTD(TOT-SUB,43)
      *                / WORK-AMT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DETAIL MEMO "-DELINQUENCIES"
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 45: NO OF 30 DAY ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 45 TO SUB.
           ADD WRTM-NO-30-DAY-ACCTS TO T-MTD(TOT-SUB,SUB).
           ADD WRLY-NO-30-DAY-ACCTS TO T-LYR-MTH(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 46: AMT OF 30 DAY ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 46 TO SUB.
           ADD WRTM-AMT-30-DAY-ACCTS TO T-MTD(TOT-SUB,SUB).
           ADD WRLY-AMT-30-DAY-ACCTS TO T-LYR-MTH(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 47: % AMT OF 30 DAY ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 47 TO SUB.
           IF T-MTD(TOT-SUB,3) NOT = 0
              COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
                 100 * T-MTD(TOT-SUB,46) / T-MTD(TOT-SUB,3).
           IF T-LYR-MTH(TOT-SUB,3) NOT = 0
              COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
                 100 * T-LYR-MTH(TOT-SUB,46) / T-LYR-MTH(TOT-SUB,3).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 48: NO OF 60 DAY ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 48 TO SUB.
           ADD WRTM-NO-60-DAY-ACCTS TO T-MTD(TOT-SUB,SUB).
           ADD WRLY-NO-60-DAY-ACCTS TO T-LYR-MTH(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 49: AMT OF 60 DAY ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 49 TO SUB.
           ADD WRTM-AMT-60-DAY-ACCTS TO T-MTD(TOT-SUB,SUB).
           ADD WRLY-AMT-60-DAY-ACCTS TO T-LYR-MTH(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 50: % AMT OF 60 DAY ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 50 TO SUB.
           IF T-MTD(TOT-SUB,3) NOT = 0
              COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
                 100 * T-MTD(TOT-SUB,49) / T-MTD(TOT-SUB,3).
           IF T-LYR-MTH(TOT-SUB,3) NOT = 0
              COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
                 100 * T-LYR-MTH(TOT-SUB,49) / T-LYR-MTH(TOT-SUB,3).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 51: NO OF 90 DAY ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 51 TO SUB.
           ADD WRTM-NO-90-DAY-ACCTS TO T-MTD(TOT-SUB,SUB).
           ADD WRLY-NO-90-DAY-ACCTS TO T-LYR-MTH(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 52: AMT OF 90 DAY ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 52 TO SUB.
           ADD WRTM-AMT-90-DAY-ACCTS TO T-MTD(TOT-SUB,SUB).
           ADD WRLY-AMT-90-DAY-ACCTS TO T-LYR-MTH(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 53: % AMT OF 90 DAY ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 53 TO SUB.
           IF T-MTD(TOT-SUB,3) NOT = 0
              COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
                 100 * T-MTD(TOT-SUB,52) / T-MTD(TOT-SUB,3).
           IF T-LYR-MTH(TOT-SUB,3) NOT = 0
              COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
                 100 * T-LYR-MTH(TOT-SUB,52) / T-LYR-MTH(TOT-SUB,3).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 54: TOTAL NO. OF DELINQ ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 54 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) =
                   T-MTD(TOT-SUB,45) + T-MTD(TOT-SUB,48)
                                        + T-MTD(TOT-SUB,51).
           COMPUTE T-LYR-MTH(TOT-SUB,SUB) =
                   T-LYR-MTH(TOT-SUB,45) + T-LYR-MTH(TOT-SUB,48)
                                     + T-LYR-MTH(TOT-SUB,51).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 55: TOTAL AMT OF DELINQ ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 55 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) =
                   T-MTD(TOT-SUB,46) + T-MTD(TOT-SUB,49)
                                        + T-MTD(TOT-SUB,52).
           COMPUTE T-LYR-MTH(TOT-SUB,SUB) =
                   T-LYR-MTH(TOT-SUB,46) + T-LYR-MTH(TOT-SUB,49)
                                     + T-LYR-MTH(TOT-SUB,52).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 56: TOTAL % AMT OF DELINQ ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 56 TO SUB.
           IF T-MTD(TOT-SUB,3) NOT = 0
              COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
                  100 * T-MTD(TOT-SUB,55) / T-MTD(TOT-SUB,3).
           IF T-LYR-MTH(TOT-SUB,3) NOT = 0
              COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
                  100 * T-LYR-MTH(TOT-SUB,55) / T-LYR-MTH(TOT-SUB,3).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 57  (SUB 57 AND 58)  BSR/MGR BONUS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 57 TO SUB.
           ADD WRTM-BSR-BONUS(1) TO T-MTD(TOT-SUB,SUB).
 
           MOVE 58 TO SUB.
           ADD WRTM-MGR-BONUS TO T-MTD(TOT-SUB,SUB).
      *    MOVE WRTM-MGR-PENALTY-FG TO T-PENALTY-FG(TOT-SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      *    LINE# 58  (SUB 59 AND 60)  CSR/AM BONUS
      *    NOT USED, WILL PRINT FOR OLD DATES IF NON-ZERO
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE 59 TO SUB.
           ADD WRTM-CASHIER-BONUS TO T-MTD(TOT-SUB,SUB).

           MOVE 60 TO SUB.
           ADD WRTM-ASST-MGR-BONUS TO T-MTD(TOT-SUB,SUB).

      **************************************************************
      * PR# 625, ADDED WCBC LINES TO RATING SHEET.  USED SAME LINE
      * NUMBERS AS LS/WRWCBC BUT ADDED 100, JUST TO KEEP IT EASIER
      * TO KEEP TRACK OF.  BARB
      **************************************************************

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      *    LINE# 101: NOT USED, WILL USE TO ACCUMULATE WCBC ELM-OUTS-BAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
           MOVE 101 TO SUB.
           ADD WRTM-WCBC-ELM-OUTS-BAL TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-WCBC-ELY-OUTS-BAL TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-WCBC-ELM-OUTS-BAL TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-WCBC-ELY-OUTS-BAL TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 102: OPEN CUSTOMERS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 102 TO SUB.
           ADD WRTM-WCBC-CUR-NO-CUST TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-WCBC-CUR-NO-CUST TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-WCBC-CUR-NO-CUST TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-WCBC-CUR-NO-CUST TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 103: CUSTOMER GAIN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 103 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) = T-MTD(TOT-SUB,SUB)
                   + (WRTM-WCBC-CUR-NO-CUST - WRTM-WCBC-ELM-NO-CUST).
           COMPUTE T-YTD(TOT-SUB,SUB) = T-YTD(TOT-SUB,SUB)
                   + (WRTM-WCBC-CUR-NO-CUST - WRTM-WCBC-ELY-NO-CUST).
           COMPUTE T-LYR-MTH(TOT-SUB,SUB) = T-LYR-MTH(TOT-SUB,SUB)
                   + (WRLY-WCBC-CUR-NO-CUST - WRLY-WCBC-ELM-NO-CUST).
           COMPUTE T-LYR-YTD(TOT-SUB,SUB) = T-LYR-YTD(TOT-SUB,SUB)
                   + (WRLY-WCBC-CUR-NO-CUST - WRLY-WCBC-ELY-NO-CUST).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 104: LEDGER BALANCE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 104 TO SUB.
           ADD WRTM-WCBC-OUTS-BALANCE TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-WCBC-OUTS-BALANCE TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-WCBC-OUTS-BALANCE TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-WCBC-OUTS-BALANCE TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 105: LEDGER GAIN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 105 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) = T-MTD(TOT-SUB,SUB)
               + (WRTM-WCBC-OUTS-BALANCE - WRTM-WCBC-ELM-OUTS-BAL).
           COMPUTE T-YTD(TOT-SUB,SUB) = T-YTD(TOT-SUB,SUB)
               + (WRTM-WCBC-OUTS-BALANCE - WRTM-WCBC-ELY-OUTS-BAL).
           COMPUTE T-LYR-MTH(TOT-SUB,SUB) = T-LYR-MTH(TOT-SUB,SUB)
               + (WRLY-WCBC-OUTS-BALANCE - WRLY-WCBC-ELM-OUTS-BAL).
           COMPUTE T-LYR-YTD(TOT-SUB,SUB) = T-LYR-YTD(TOT-SUB,SUB)
               + (WRLY-WCBC-OUTS-BALANCE - WRLY-WCBC-ELY-OUTS-BAL).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 106: NO. NEW CUSTOMERS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 106 TO SUB.
           ADD WRTM-WCBC-NO-NB(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-WCBC-NO-NB(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-WCBC-NO-NB(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-WCBC-NO-NB(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 107: NO. FORMER CUSTOMERS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 107 TO SUB.
           ADD WRTM-WCBC-NO-FB(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-WCBC-NO-FB(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-WCBC-NO-FB(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-WCBC-NO-FB(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 108: NO. RENEWAL CUSTOMERS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 108 TO SUB.
           ADD WRTM-WCBC-NO-PB(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-WCBC-NO-PB(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-WCBC-NO-PB(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-WCBC-NO-PB(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 109: N/A
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# L11: GROSS LOAN VOLUME
      *    DO LINE 111 BEFORE LINE 110, NEED VOLUME FOR LINE 110
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 111 TO SUB.
           ADD WRTM-WCBC-GROSS-VOLUME(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-WCBC-GROSS-VOLUME(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-WCBC-GROSS-VOLUME(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-WCBC-GROSS-VOLUME(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 110: PERCENT VOLUME  ?? ASK BARB
      *              KEYPUNCH / LINE# 4 PREV MONTH (REG RATING OR LINE
      *              1110 COMP OPER PRIOR MONTH)
      *              ACCUMULATE YTD, COMPUTE MTD
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 110 TO SUB.
           IF T-MTD(TOT-SUB,101) NOT = 0
              COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
                 T-MTD(TOT-SUB,111) / T-MTD(TOT-SUB,101)
                     * 100.
           ADD WRTM-WCBC-PCT-VOLUME TO T-YTD(TOT-SUB,SUB).

           IF T-LYR-MTH(TOT-SUB,101) NOT = 0
              COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
                 T-LYR-MTH(TOT-SUB,111) / T-LYR-MTH(TOT-SUB,101)
                 * 100.
           ADD WRLY-WCBC-PCT-VOLUME TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 112: N/A
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 113: AVG UNPAID BALANCE   LINE 104 / LINE 102;  YTD N/A
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 113 TO SUB.
           MOVE 0 TO T-MTD(TOT-SUB,SUB).
           IF T-MTD(TOT-SUB,102)  NOT = 0
              COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
                   T-MTD(TOT-SUB, 104) / T-MTD(TOT-SUB,102).
           MOVE 0 TO T-LYR-MTH(TOT-SUB,SUB).
           IF T-LYR-MTH(TOT-SUB,102)  NOT = 0
              COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
                   T-LYR-MTH(TOT-SUB, 104) / T-LYR-MTH(TOT-SUB,102).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 114: DELINQUENT CHARGES  ASK BARB??  G/L 5111
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 114 TO SUB.
           ADD WRTM-WCBC-LATE-CHGS(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-WCBC-LATE-CHGS(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-WCBC-LATE-CHGS(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-WCBC-LATE-CHGS(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 115: N/A
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 116: N/A
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 117: FINANCE CHARGES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 117 TO SUB.
           ADD WRTM-WCBC-NET-FIN-CHGS(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-WCBC-NET-FIN-CHGS(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-WCBC-NET-FIN-CHGS(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-WCBC-NET-FIN-CHGS(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 118: N/A
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 119: SALES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 119 TO SUB.
           ADD WRTM-WCBC-SALES(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-WCBC-SALES(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-WCBC-SALES(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-WCBC-SALES(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 120: COST OF SALES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 120 TO SUB.
           ADD WRTM-WCBC-COST-OF-SALES(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-WCBC-COST-OF-SALES(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-WCBC-COST-OF-SALES(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-WCBC-COST-OF-SALES(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 121: EXPENSES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 121 TO SUB.
           ADD WRTM-WCBC-EXPENSES(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-WCBC-EXPENSES(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-WCBC-EXPENSES(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-WCBC-EXPENSES(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 122: N/A
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 123: N/A
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 124: N/A
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 125: N/A
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 126: TOTAL INCOME  LINE 114 - LINE 119 ADD; -LINE 120,121
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 126 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) =
                   T-MTD(TOT-SUB,114)
                 + T-MTD(TOT-SUB,117)
                 + T-MTD(TOT-SUB,119)
                 - T-MTD(TOT-SUB,120)
                 - T-MTD(TOT-SUB,121).
           COMPUTE T-YTD(TOT-SUB,SUB) =
                   T-YTD(TOT-SUB,114)
                 + T-YTD(TOT-SUB,117)
                 + T-YTD(TOT-SUB,119)
                 - T-YTD(TOT-SUB,120)
                 - T-YTD(TOT-SUB,121).
           COMPUTE T-LYR-MTH(TOT-SUB,SUB) =
                   T-LYR-MTH(TOT-SUB,114)
                 + T-LYR-MTH(TOT-SUB,117)
                 + T-LYR-MTH(TOT-SUB,119)
                 - T-LYR-MTH(TOT-SUB,120)
                 - T-LYR-MTH(TOT-SUB,121).
           COMPUTE T-LYR-YTD(TOT-SUB,SUB) =
                   T-LYR-YTD(TOT-SUB,114)
                 + T-LYR-YTD(TOT-SUB,117)
                 + T-LYR-YTD(TOT-SUB,119)
                 - T-LYR-YTD(TOT-SUB,120)
                 - T-LYR-YTD(TOT-SUB,121).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 127: GROSS PROFIT PER LOAN
      *              LINE 119 + LINE 120 / LINE 106 + LINE 107 + LINE 108
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 127 TO SUB.
           COMPUTE WORK1 = T-MTD(TOT-SUB,119)
                         - T-MTD(TOT-SUB,120).
           COMPUTE WORK2 = T-MTD(TOT-SUB,106)
                         + T-MTD(TOT-SUB,107)
                         + T-MTD(TOT-SUB,108).
           MOVE 0 TO T-MTD(TOT-SUB,SUB).
           IF WORK2 NOT = 0
              COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
                   WORK1 / WORK2.

      *
           COMPUTE WORK1 = T-YTD(TOT-SUB,119)
                         - T-YTD(TOT-SUB,120).
           COMPUTE WORK2 = T-YTD(TOT-SUB,106)
                         + T-YTD(TOT-SUB,107)
                         + T-YTD(TOT-SUB,108).
           MOVE 0 TO T-YTD(TOT-SUB,SUB).
           IF WORK2 NOT = 0
              COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
                   WORK1 / WORK2.

      *
           COMPUTE WORK1 = T-LYR-MTH(TOT-SUB,119)
                         - T-LYR-MTH(TOT-SUB,120).
           COMPUTE WORK2 = T-LYR-MTH(TOT-SUB,106)
                         + T-LYR-MTH(TOT-SUB,107)
                         + T-LYR-MTH(TOT-SUB,108).
           MOVE 0 TO T-LYR-MTH(TOT-SUB,SUB).
           IF WORK2 NOT = 0
              COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
                   WORK1 / WORK2.

      *
           COMPUTE WORK1 = T-LYR-YTD(TOT-SUB,119)
                         - T-LYR-YTD(TOT-SUB,120).
           COMPUTE WORK2 = T-LYR-YTD(TOT-SUB,106)
                         + T-LYR-YTD(TOT-SUB,107)
                         + T-LYR-YTD(TOT-SUB,108).
           MOVE 0 TO T-LYR-YTD(TOT-SUB,SUB).
           IF WORK2 NOT = 0
              COMPUTE T-LYR-YTD(TOT-SUB,SUB) ROUNDED =
                   WORK1 / WORK2.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 128: GROSS PROFIT % PER LOAN
      *              LINE 119 + LINE 120 / LINE 111
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 128 TO SUB.
           COMPUTE WORK1 = T-MTD(TOT-SUB,119)
                         - T-MTD(TOT-SUB,120).
           COMPUTE WORK2 = T-MTD(TOT-SUB,119).
           IF WORK2 NOT = 0
              COMPUTE T-MTD(TOT-SUB,SUB) ROUNDED =
                   WORK1 / WORK2 * 100.
      *
           COMPUTE WORK1 = T-YTD(TOT-SUB,119)
                         - T-YTD(TOT-SUB,120).
           COMPUTE WORK2 = T-YTD(TOT-SUB,119).
           IF WORK2 NOT = 0
              COMPUTE T-YTD(TOT-SUB,SUB) ROUNDED =
                   WORK1 / WORK2 * 100.
      *
           COMPUTE WORK1 = T-LYR-MTH(TOT-SUB,119)
                         - T-LYR-MTH(TOT-SUB,120).
           COMPUTE WORK2 = T-LYR-MTH(TOT-SUB,119).
           IF WORK2 NOT = 0
              COMPUTE T-LYR-MTH(TOT-SUB,SUB) ROUNDED =
                   WORK1 / WORK2 * 100.
      *
           COMPUTE WORK1 = T-LYR-YTD(TOT-SUB,119)
                         - T-LYR-YTD(TOT-SUB,120).
           COMPUTE WORK2 = T-LYR-YTD(TOT-SUB,119).
           IF WORK2 NOT = 0
              COMPUTE T-LYR-YTD(TOT-SUB,SUB) ROUNDED =
                   WORK1 / WORK2 * 100.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 129: RECOVERY-INCOME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 129 TO SUB.
           ADD WRTM-WCBC-RECOVERY-INCOME(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-WCBC-RECOVERY-INCOME(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-WCBC-RECOVERY-INCOME(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-WCBC-RECOVERY-INCOME(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 130: INCR/DECR 90 DAYS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 130 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB)     = T-MTD(TOT-SUB,SUB) +
              (WRTM-WCBC-AMT-90-DAY-ACCTS - WRTM-WCBC-ELM-AMT-90-DAYS).
           COMPUTE T-YTD(TOT-SUB,SUB)     = T-YTD(TOT-SUB,SUB) +
              (WRTM-WCBC-AMT-90-DAY-ACCTS - WRTM-WCBC-ELY-AMT-90-DAYS).
           COMPUTE T-LYR-MTH(TOT-SUB,SUB) = T-LYR-MTH(TOT-SUB,SUB) +
              (WRLY-WCBC-AMT-90-DAY-ACCTS - WRLY-WCBC-ELM-AMT-90-DAYS).
           COMPUTE T-LYR-YTD(TOT-SUB,SUB) = T-LYR-YTD(TOT-SUB,SUB) +
              (WRLY-WCBC-AMT-90-DAY-ACCTS - WRLY-WCBC-ELY-AMT-90-DAYS).


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 131: CHARGE OFFS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 131 TO SUB.
           ADD WRTM-WCBC-CHARGEOFF-EXPENSE(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-WCBC-CHARGEOFF-EXPENSE(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-WCBC-CHARGEOFF-EXPENSE(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-WCBC-CHARGEOFF-EXPENSE(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 132: CHARGE OFF EXPENSE/BANKRUPTCIES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 132 TO SUB.
           ADD WRTM-WCBC-BAD-DEBT-BNKRPCY(1) TO T-MTD(TOT-SUB,SUB).
           ADD WRTM-WCBC-BAD-DEBT-BNKRPCY(2) TO T-YTD(TOT-SUB,SUB).
           ADD WRLY-WCBC-BAD-DEBT-BNKRPCY(1) TO T-LYR-MTH(TOT-SUB,SUB).
           ADD WRLY-WCBC-BAD-DEBT-BNKRPCY(2) TO T-LYR-YTD(TOT-SUB,SUB).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 133: N/A
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 134: N/A
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 135: BAD DEBT  LINE 130 + LINE 131 + LINE 132
      *                      + LINE 133 + LINE 134 - LINE 129
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 135 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) = T-MTD(TOT-SUB, 129) * -1
                 + T-MTD(TOT-SUB, 130) + T-MTD(TOT-SUB, 131)
                 + T-MTD(TOT-SUB, 132) + T-MTD(TOT-SUB, 133)
                 + T-MTD(TOT-SUB, 134).
           COMPUTE T-YTD(TOT-SUB,SUB) = T-YTD(TOT-SUB, 129) * -1
                 + T-YTD(TOT-SUB, 130) + T-YTD(TOT-SUB, 131)
                 + T-YTD(TOT-SUB, 132) + T-YTD(TOT-SUB, 133)
                 + T-YTD(TOT-SUB, 134).
           COMPUTE T-LYR-MTH(TOT-SUB,SUB) = T-LYR-MTH(TOT-SUB, 129) * -1
                 + T-LYR-MTH(TOT-SUB, 130) + T-LYR-MTH(TOT-SUB, 131)
                 + T-LYR-MTH(TOT-SUB, 132) + T-LYR-MTH(TOT-SUB, 133)
                 + T-LYR-MTH(TOT-SUB, 134).
           COMPUTE T-LYR-YTD(TOT-SUB,SUB) = T-LYR-YTD(TOT-SUB, 129) * -1
                 + T-LYR-YTD(TOT-SUB, 130) + T-LYR-YTD(TOT-SUB, 131)
                 + T-LYR-YTD(TOT-SUB, 132) + T-LYR-YTD(TOT-SUB, 133)
                 + T-LYR-YTD(TOT-SUB, 134).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 136: N/A
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 137: SALES FINANCE PROFIT LINE 126 - LINE 135 BAD DEBT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 137 TO SUB.
           COMPUTE T-MTD(TOT-SUB,SUB) = T-MTD(TOT-SUB, 126)
                 - T-MTD(TOT-SUB, 135).
           COMPUTE T-YTD(TOT-SUB,SUB) = T-YTD(TOT-SUB, 126)
                 - T-YTD(TOT-SUB, 135).
           COMPUTE T-LYR-MTH(TOT-SUB,SUB) = T-LYR-MTH(TOT-SUB, 126)
                 - T-LYR-MTH(TOT-SUB, 135).
           COMPUTE T-LYR-YTD(TOT-SUB,SUB) = T-LYR-YTD(TOT-SUB, 126)
                 - T-LYR-YTD(TOT-SUB, 135).

       EXIT-ADD-TOTALS.
      * RESTORE THIS MONTH RECORD AND RESTART FILE:
           MOVE WRTM-REC TO WR-REC.
      *    PERFORM START-WR1-FILE.
      *    PERFORM READ-WR1-FILE-NEXT.

      ******************************************************************
       CREATE-EXTRACT-REC SECTION.
      ******************************************************************
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 56
              CALL "C$JUSTIFY" USING EXT-MTD-NO(SUB), "L"
              CALL "C$JUSTIFY" USING EXT-YTD-NO(SUB), "L"
              CALL "C$JUSTIFY" USING EXT-LYR-MTD-NO(SUB), "L"
              CALL "C$JUSTIFY" USING EXT-LYR-YTD-NO(SUB), "L"
              CALL "C$JUSTIFY" USING EXT-MTD-VAR-NO(SUB), "L"
              CALL "C$JUSTIFY" USING EXT-YTD-VAR-NO(SUB), "L"
              CALL "C$JUSTIFY" USING EXT-MTD-VAR-PCT(SUB), "L"
              CALL "C$JUSTIFY" USING EXT-YTD-VAR-PCT(SUB), "L"
              MOVE SPACES TO STRING-LINE(SUB)
              STRING EXT-MTD-NO(SUB), "|",
                     EXT-YTD-NO(SUB), "|",
                     EXT-LYR-MTD-NO(SUB), "|",
                     EXT-LYR-YTD-NO(SUB), "|",
                     EXT-MTD-VAR-NO(SUB), "|",
                     EXT-YTD-VAR-NO(SUB), "|",
                     EXT-MTD-VAR-PCT(SUB), "|",
                     EXT-YTD-VAR-PCT(SUB), "|",
                DELIMITED BY " " INTO STRING-LINE(SUB)
           END-PERFORM.
           MOVE SPACES TO STRING-LINE(57).
           MOVE WRTM-BSR-BONUS(1)  TO EXTRACT-BSR.
           MOVE WRTM-MGR-BONUS      TO EXTRACT-MGR.
           CALL "C$JUSTIFY" USING EXTRACT-BSR, "L".
           CALL "C$JUSTIFY" USING EXTRACT-MGR, "L".
           STRING EXTRACT-BSR, "|",
                  EXTRACT-MGR
             DELIMITED BY " " INTO STRING-LINE(57).
           MOVE SPACES TO HOLD-REC1 HOLD-REC2 RATING-REC.
           STRING HOLD-BRNO, "|",
                  STRING-LINE(1),
                  STRING-LINE(2),
                  STRING-LINE(3),
                  STRING-LINE(4),
                  STRING-LINE(5),
                  STRING-LINE(6),
                  STRING-LINE(7),
                  STRING-LINE(8),
                  STRING-LINE(9),
                  STRING-LINE(10),
                  STRING-LINE(11),
                  STRING-LINE(12),
                  STRING-LINE(14),
                  STRING-LINE(15),
                  STRING-LINE(16),
                  STRING-LINE(17),
                  STRING-LINE(18),
                  STRING-LINE(19),
                  STRING-LINE(20),
                  STRING-LINE(21),
                  STRING-LINE(22),
                  STRING-LINE(23),
                  STRING-LINE(24),
                  STRING-LINE(25),
                DELIMITED BY " " INTO HOLD-REC1.
           STRING STRING-LINE(26),
                  STRING-LINE(27),
                  STRING-LINE(28),
                  STRING-LINE(29),
                  STRING-LINE(30),
                  STRING-LINE(31),
                  STRING-LINE(32),
                  STRING-LINE(33),
                  STRING-LINE(34),
                  STRING-LINE(35),
                  STRING-LINE(36),
                  STRING-LINE(37),
                  STRING-LINE(38),
                  STRING-LINE(39),
                  STRING-LINE(40),
                  STRING-LINE(41),
                  STRING-LINE(42),
                  STRING-LINE(43),
                  STRING-LINE(44),
                  STRING-LINE(45),
                  STRING-LINE(46),
                  STRING-LINE(47),
                  STRING-LINE(48),
                  STRING-LINE(49),
                  STRING-LINE(50),
                  STRING-LINE(51),
                  STRING-LINE(52),
                  STRING-LINE(53),
                  STRING-LINE(54),
                  STRING-LINE(55),
                  STRING-LINE(56),
                  STRING-LINE(57) 
                DELIMITED BY " " INTO HOLD-REC2.
                  
           STRING HOLD-REC1, HOLD-REC2 
                DELIMITED BY " " INTO RATING-REC.

           PERFORM WRITE-RATING-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE RATING-QUEUE-POS TO Q-POS.
           MOVE RATING-COPIES-POS TO C-POS.
           MOVE RATING-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".
      
           PERFORM OPEN-BR-FILE.
           IF EXTRACT-FG = "Y"
              CALL "C$TOLOWER" USING RATING-PATH-EXTRACT, VALUE 12
              MOVE RATING-PATH1 TO ACCESS-BUF
              PERFORM ACCESS-CALL
              IF STAT-BAD
                 CALL "C$MAKEDIR" USING ACCESS-BUF GIVING STAT-99
              END-IF
              PERFORM OPEN-RATING-FILE-OUTPUT.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID  TO H-USERID.
           IF PRINT-1-LINE-FG = "Y"
               MOVE "LN BR # BRANCH NAME       " TO H-PGDESC
           ELSE
               MOVE "LN BR # RATING LINE DESC  " TO H-PGDESC.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH     TO SV-BEG-BR
              MOVE END-BRANCH     TO SV-END-BR
              MOVE GROUP-FLAG     TO SV-GROUP-FLAG
              MOVE ENT-GROUP      TO SV-ENT-GROUP
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
              IF EXT-EOBUF-BRNO NUMERIC
                 MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
                 MOVE "N" TO GROUP-FLAG
              ELSE
                 MOVE EXT-EOBUF-GROUP TO ENT-GROUP
                 MOVE 0 TO BEG-BRANCH
                 MOVE 9999 TO END-BRANCH
                 MOVE "Y" TO GROUP-FLAG
              END-IF
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           MOVE DISPLAY-DATE TO H-MMYY.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS..............." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           IF LINES-PER-PAGE NOT = 99
              MOVE LINES-PER-PAGE TO EXT-CONAME-RPT-LINES.

           IF GROUP-FLAG = "Y"
              PERFORM OPEN-GR1-FILE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              GO TO EXIT-INITIALIZE.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM     TO CUR-YYYYMM-DATE.

      * SETUP LAST YEAR DATE:

           MOVE EXT-EOBUF-DATE2 TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD   TO NDTE-DATE.
           MOVE 1               TO NDTE-DD.
           MOVE -1              TO NDTE-HOLD.
           PERFORM INCREMENT-YEARS.
           MOVE NDTE-DATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM     TO LAST-YYYYMM-DATE.

           MOVE PRINT-LINE-TABLE TO LINE-TABLE.

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
           IF VDUSTATUS = "1U" GO TO PRINT-BONUS-YN.

           PERFORM GET-TODAYS-DATE.

      *----------------------------------------------------------------
       ENTER-DATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.
           MOVE "EN N040MMYY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DATE.

           IF VDUNUM0 = 0
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              MOVE ALP-MO TO DISPLAY-MO
              MOVE ALP-YR TO DISPLAY-YR
              MOVE "SN N040MMYY." TO VDU
              MOVE DISPLAY-DATE TO VDUNUM0
              PERFORM SCREENIO
              GO TO TEST-WR-DATE.

           MOVE VDUNUM0 TO DISPLAY-DATE.

      *----------------------------------------------------------------
       TEST-WR-DATE.
           IF DISPLAY-MO < 1 OR DISPLAY-MO > 12
              MOVE "MONTH ENTRY ERROR " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-DATE.

      *----------------------------------------------------------------
       SETUP-WR-DATE.
           MOVE DISPLAY-MO      TO DATE-MMDDYY-MM.
           MOVE 1               TO DATE-MMDDYY-DD.
           MOVE DISPLAY-YR      TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM     TO CUR-YYYYMM-DATE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SETUP LAST YEAR DATE:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DISPLAY-MO      TO DATE-MMDDYY-MM.
           MOVE 1               TO DATE-MMDDYY-DD.
           MOVE DISPLAY-YR      TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD   TO NDTE-DATE.
           MOVE -1              TO NDTE-HOLD.
           PERFORM INCREMENT-YEARS.
           MOVE NDTE-DATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM     TO LAST-YYYYMM-DATE.
      *----------------------------------------------------------------

       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO ENTER-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.

       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       START-PRINT-LINES.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              MOVE 0 TO PRINT-LINE-RPT(SUB) LINE-RPT(SUB)
           END-PERFORM.

           MOVE 0 TO SUB.
       PRINT-LINES-LOOP.
           ADD 1 TO SUB.
      *    IF SUB > 10
           IF SUB > 1
             GO TO E-LINES-PAGE.
       E-PRINT-LINES.
           MOVE "ENNN020010." TO VDU.
           MOVE SUB TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16"
              MOVE "SN N020010." TO VDU
              MOVE 0 TO VDUNUM0 PRINT-LINE-RPT(1)
              PERFORM SCREENIO
              MOVE "S  A000LNDESC." TO VDU
              MOVE "PRINT COMPLETE RATING SHEET" TO VDUBUF
              PERFORM SCREENIO
              MOVE "N" TO PRINT-1-LINE-FG
              GO TO E-LINES-PAGE.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUNUM0 =  0
              IF SUB = 1
                 GO TO E-PRINT-LINES.

           IF VDUNUM0 > 56 GO TO E-PRINT-LINES.
           IF VDUNUM0 = 0
              GO TO E-LINES-PAGE.
           MOVE VDUNUM0 TO PRINT-LINE-RPT(SUB) LINE-RPT(SUB)
                           WORK-SUB.
           MOVE "S  A000LNDESC." TO VDU.
           MOVE LINE-DESC(WORK-SUB) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "Y" TO PRINT-1-LINE-FG.
           GO TO PRINT-LINES-LOOP.

       E-LINES-PAGE.
      *    MOVE "SN N020PGLN." TO VDU.
      *    MOVE EXT-CONAME-RPT-LINES TO VDUNUM0 LINES-PER-PAGE.
      *    PERFORM SCREENIO.
           MOVE EXT-CONAME-RPT-LINES TO LINES-PER-PAGE.
           GO TO E-MIN-AMT.

      *    MOVE "EN N020PGLN." TO VDU.
      *    PERFORM SCREENIO.
      *    IF VDUSTATUS = "10" GO TO EXIT-PARM.
      *    IF VDUSTATUS = "16"
      *       MOVE "SN N020PGLN." TO VDU
      *       MOVE 99 TO LINES-PER-PAGE VDUNUM0
      *       PERFORM SCREENIO
      *       GO TO E-PRINT-LINES.
      *    IF VDUSTATUS = "1U" GO TO START-PRINT-LINES.
      *    IF VDUSTATUS = "1>" GO TO EXIT-PARM.
      *    MOVE VDUNUM0 TO LINES-PER-PAGE.

       E-MIN-AMT.
           MOVE "ENNS092MIN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16"
              GO TO ALL-AMTS.
           IF VDUSTATUS = "1U" GO TO START-PRINT-LINES.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           MOVE VDUNUM2 TO MIN-AMT.

       E-MAX-AMT.
           MOVE "ENNS092MAX." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO E-MIN-AMT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           MOVE VDUNUM2 TO MAX-AMT.
           GO TO E-CURRENT-LASTYR.

       ALL-AMTS.
           MOVE "SNNN092MIN." TO VDU.
           MOVE 0 TO MIN-AMT VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SNNN072MAX." TO VDU.
           MOVE 999999999.99 TO MAX-AMT VDUNUM2.
           PERFORM SCREENIO.

       E-CURRENT-LASTYR.
           IF MIN-AMT = 0 AND MAX-AMT = 999999999.99
              MOVE "S  A010CURLASTFG." TO VDU
              MOVE SPACES TO MIN-MAX-PERIOD VDUBUF
                             MIN-MAX-FG
              PERFORM SCREENIO
              GO TO PRINT-BONUS-YN.
           MOVE "E  A010CURLASTFG." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO E-MAX-AMT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           MOVE VDUBUF TO MIN-MAX-PERIOD.
           IF MIN-MAX-PERIOD NOT = "C"
              IF MIN-MAX-PERIOD NOT = "L"
                 MOVE "ENTER `C' FOR CURRENT OR `L' FOR LAST YEAR"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO E-CURRENT-LASTYR.

       E-MTD-YTD-BOTH.
           IF MIN-AMT = 0 AND MAX-AMT = 999999999.99
              MOVE "S  A010MINMAXFG." TO VDU
              MOVE SPACES TO MIN-MAX-PERIOD
                             MIN-MAX-FG VDUBUF
              PERFORM SCREENIO
              GO TO PRINT-BONUS-YN.
           MOVE "E  A010MINMAXFG." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO E-CURRENT-LASTYR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           MOVE VDUBUF TO MIN-MAX-FG.
           IF MIN-MAX-FG NOT = "M"
              IF MIN-MAX-FG NOT = "Y"
                 IF MIN-MAX-FG NOT = "B"
                    MOVE "ENTER `M' FOR MTD, `Y' FOR YTD, `B' FOR BOTH"
                       TO MESS
                    PERFORM SEND-MESS
                    GO TO E-MTD-YTD-BOTH.
            GO TO PRINT-BONUS-YN.
      *----------------------------------------------------------------
       EXTRACT-FILE-YN.
           MOVE "E  A010EXTRACT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO E-MTD-YTD-BOTH.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           MOVE VDUBUF TO EXTRACT-FG.
           IF EXTRACT-FG NOT = "Y"
              IF EXTRACT-FG NOT = "N"
                 MOVE "PLEASE ENTER 'Y' OR 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO EXTRACT-FILE-YN.
      *----------------------------------------------------------------
       PRINT-BONUS-YN.
           MOVE "E  A010BONUS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO EXTRACT-FILE-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           MOVE VDUBUF TO PRINT-BONUS.
           IF PRINT-BONUS NOT = "Y" AND NOT = "N"
              MOVE "PLEASE ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO PRINT-BONUS-YN.
      *----------------------------------------------------------------
       CHECK-LEVEL-PRINTS.

           IF ( GROUP-FLAG = "Y" )
              PERFORM OPEN-GR1-FILE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y" TO TOTAL-LEVEL-TABLE(9).

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           MOVE SPACES TO VDUBUF.
           MOVE "S  A040MMYY." TO VDU.
           PERFORM SCREENIO.

           IF GROUP-FLAG = "Y"
              MOVE "S  A040BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF GTT-SV-ENT-GROUP
              PERFORM SCREENIO
           ELSE
              MOVE "SN N040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SN N040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              MOVE PRINT-LINE-RPT(SUB) TO LINE-RPT(SUB)
           END-PERFORM.

           MOVE PRINT-LINE-RPT(1) TO LINE-RPT(1), WORK-SUB.

           MOVE "SNNN020010." TO VDU.
           MOVE PRINT-LINE-RPT(1) TO VDUNUM0.
           PERFORM SCREENIO.


           MOVE "S  A000LNDESC." TO VDU.
           IF PRINT-LINE-RPT(1) = 0
              MOVE "PRINT COMPLETE RATING SHEET" TO VDUBUF
           ELSE
              MOVE LINE-DESC(WORK-SUB) TO VDUBUF.
           PERFORM SCREENIO.

      * FUTURE USE TO ALLOW CUSTOMER TO CHOOSE LINES
      *    MOVE "SN N020PGLN." TO VDU.
      *    MOVE LINES-PER-PAGE TO VDUNUM0.
      *    PERFORM SCREENIO.

           MOVE "S  S082MIN." TO VDU.
           MOVE MIN-AMT TO VDUNUM2.
           PERFORM SCREENIO.

           MOVE "S  S082MAX." TO VDU.
           MOVE MAX-AMT TO VDUNUM2.
           PERFORM SCREENIO.

           MOVE "S  A000CURLASTFG." TO VDU.
           MOVE MIN-MAX-PERIOD TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000MINMAXFG." TO VDU.
           MOVE MIN-MAX-FG TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000EXTRACT." TO VDU.
           MOVE EXTRACT-FG TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000BONUS." TO VDU.
           MOVE PRINT-BONUS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
      *   CHECK FOR LEVEL SUMMARY TOTALS
      *   WGR-SCURR IS TABLE OF POINTERS FOR CURRENT GROUP
      *   WHICH POINT TO WGR-SID TABLE TO SHOW CURRENT
      *   GROUP HIERARCHY:  WGR-SCURR(1) POINTS TO WGR-SID
      *   OF LOW LEVEL (SELECTED GROUP); WGR-SCURR(WGR-TPLUS)
      *   POINTS TO THE WGR-SID OF THE HIGHEST LEVEL WHICH
      *   IS THE BRANCH NUMBER CURRENTLY BEING PROCESSED.
      *   THIS ROUTINE COMPARES THE WGR-SCURR TABLE TO THE
      *   TABLE SAVED FROM THE PREVIOUS GROUP, PRINTING &
      *   ZEROING TOTALS FOR EACH LEVEL WHICH HAS CHANGED
      *   STARTING WITH THE HIGHEST (BRANCH) LEVEL & WORKING
      *   THROUGH HIERARCHY.  GRAND TOTALS ARE FORCED BY
      *   MOVING 0 TO WGR-SCURR TABLE & 15 TO WGR-TPLUS.
      ******************************************************************
       TOTAL-LEVEL-CHECK SECTION.
      ******************************************************************
           MOVE 15 TO GROUP-SUB.

       LEVEL-TOTAL-LOOP.
           IF GROUP-SUB = 0
              GO TO EXIT-TOTAL-LEVEL-CHECK.

           IF WGR-SCURR(GROUP-SUB) = PREV-SCURR(GROUP-SUB)
              GO TO CONTINUE-LOOP.

           MOVE PREV-SCURR(GROUP-SUB) TO PREV-SUB.

           IF PREV-SUB = 0
              GO TO CONTINUE-LOOP.

           IF WGR-SID(PREV-SUB) NUMERIC
              GO TO CHECK-BRANCH-SID.

           MOVE WGR-SID(PREV-SUB) TO GR-ID.
           PERFORM READ-GR1-FILE.

           IF TOTAL-LEVEL-TABLE(GR-LEVEL) NOT = "Y"
              GO TO CONTINUE-LOOP.

           MOVE GR-LEVEL TO TOT-SUB.

      *----------------------------------------------------------------
       DO-GROUP-TOTALS.
           MOVE "GROUP: "    TO HEAD2-TYPE.
           MOVE GR-ID        TO H-GRID.
           MOVE GR-NAME      TO H-GRNAME.
           IF PRINT-1-LINE-FG = "N"
              PERFORM HEAD-PAGE.

           PERFORM PRINT-TOTALS.

           GO TO CONTINUE-LOOP.

      *----------------------------------------------------------------
       CHECK-BRANCH-SID.
           IF TOTAL-LEVEL-TABLE(9) NOT = "Y"
              GO TO CONTINUE-LOOP.

           MOVE WGR-SID(PREV-SUB) TO WGR-WORD.
           MOVE WGR-NUMB TO BR-NO.
           PERFORM READ-BR-FILE.
           MOVE 9 TO TOT-SUB.
           MOVE "BRANCH:"      TO HEAD2-TYPE.
           MOVE BR-NO          TO H-BRNO HOLD-BRNO.
           MOVE BR-NAME        TO H-GRNAME.
           IF PRINT-1-LINE-FG = "N"
              PERFORM HEAD-PAGE.

           PERFORM PRINT-TOTALS.

      *----------------------------------------------------------------
       CONTINUE-LOOP.
           SUBTRACT 1 FROM GROUP-SUB.

           IF GROUP-SUB < WGR-SSTABLE
              GO TO EXIT-TOTAL-LEVEL-CHECK.

           GO TO LEVEL-TOTAL-LOOP.

       EXIT-TOTAL-LEVEL-CHECK.
           MOVE WGR-SCURR-TABLE TO PREV-SCURR-TABLE.

      ******************************************************************
       PRINT-ONE-BRANCH-TOTALS SECTION.
      ******************************************************************
           MOVE 9 TO TOT-SUB.

           MOVE "BRANCH:"     TO HEAD2-TYPE.
           MOVE BR-NO         TO H-BRNO HOLD-BRNO.
           MOVE BR-NAME       TO H-GRNAME.
           IF PRINT-1-LINE-FG = "N"
              PERFORM HEAD-PAGE.

           PERFORM PRINT-TOTALS.

           MOVE "N" TO FIRST-TIME.

       EXIT-PRINT-ONE-BRANCH.
           EXIT.

      ******************************************************************
       PRINT-ONE-BRANCH-GRAND SECTION.
      ******************************************************************
           IF BEG-BRANCH = END-BRANCH
              GO TO EXIT-PRINT-ONE-GRAND.

           MOVE 1 TO TOT-SUB.

           MOVE 0               TO H-BRNO.

           PERFORM HEAD-PAGE.

           MOVE "GRAND :"      TO HEAD2-TYPE.

           PERFORM PRINT-TOTALS.

       EXIT-PRINT-ONE-GRAND.
           EXIT.

      ******************************************************************
       PRINT-TOTALS SECTION.
      ******************************************************************
           IF FIRST-LEVEL = "Y"
              MOVE TOT-SUB TO HOLD-LEVEL
              MOVE "N" TO FIRST-LEVEL.

           IF PRINT-1-LINE-FG = "Y"
              IF TOT-SUB = 9
                 IF NOT (MIN-AMT = 0 AND MAX-AMT = 999999999.99)
                    PERFORM CHECK-MIN-MAX
                    IF WITHIN-MIN-MAX = "N"
                       GO TO EXIT-PRINT-TOTALS.

           IF TOT-SUB NOT = 9
              IF PRINT-1-LINE-FG = "Y"
                 IF TOT-SUB < HOLD-LEVEL
                    PERFORM PRINT-LINE-LEVEL-HEAD.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 56
              MOVE SPACES TO EXTRACT-LINE(SUB)
           END-PERFORM.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 01: OPEN CUSTOMERS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 01               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 02: CUSTOMER BALANCE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 02               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 03: LEDGER BALANCE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 03               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 04: LEDGER GAIN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 04               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 05: NO. NEW CUSTOMERS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 05               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 06: NO. FORMER CUSTOMERS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 06               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 07: NO. RENEWAL CUSTOMERS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 07               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 08: TOTAL # LOANS MADE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 08               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 09: GROSS LOAN VOLUME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 09               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 10: PERCENT TURNOVER
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 10               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           COMPUTE D-MTD = T-MTD(TOT-SUB,SUB).
           MOVE 0 TO D-YTD.
           IF T-COUNT(TOT-SUB) NOT = 0
              COMPUTE D-YTD = T-YTD(TOT-SUB,SUB) / T-COUNT(TOT-SUB).
           COMPUTE D-LYR-MTD    = T-LYR-MTH(TOT-SUB,SUB).
           MOVE 0 TO D-LYR-YTD.
           IF T-COUNT(TOT-SUB) NOT = 0
              COMPUTE D-LYR-YTD    = T-LYR-YTD(TOT-SUB,SUB) /
                                  T-COUNT(TOT-SUB).

           IF (EXTRACT-FG = "Y" AND TOT-SUB = 9)
              COMPUTE EXT-PCT-MTD(SUB) = T-MTD(TOT-SUB,SUB)
              MOVE 0 TO EXT-PCT-YTD(SUB)
              IF T-COUNT(TOT-SUB) NOT = 0
                 COMPUTE EXT-PCT-YTD(SUB) = T-YTD(TOT-SUB,SUB)
                                          / T-COUNT(TOT-SUB)
              END-IF
              COMPUTE EXT-PCT-LYR-MTD(SUB) = T-LYR-MTH(TOT-SUB,SUB)
              MOVE 0 TO EXT-PCT-LYR-YTD(SUB)
              IF T-COUNT(TOT-SUB) NOT = 0
                 COMPUTE EXT-PCT-LYR-YTD(SUB) = T-LYR-YTD(TOT-SUB,SUB)
                                              / T-COUNT(TOT-SUB).
      *    PERFORM MOVE-DOLLARS.
           PERFORM COMPUTE-PERCENT-VARIANCES.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 11: AVG UNPAID BALANCE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 11               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 12: PRE-TAX PROFIT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 12               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-DOLLARS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES-NO.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 13: WCBC PROFIT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 13               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.

      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 14: INTEREST ALLOCATION
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 14               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-DOLLARS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES-NO.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 15: SUPERVISION
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 15               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-DOLLARS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES-NO.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 16: GENERAL RESERVE ADJUSTMENT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 16               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-DOLLARS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES-NO.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 17: INTANGIBLE AMORTIZATION
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 17               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-DOLLARS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES-NO.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 18: ROBBERIES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 18               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-DOLLARS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES-NO.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 19: RATING SHEET PROFIT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 19               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-DOLLARS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES-NO.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 20: % RATING SHEET PROFIT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 20               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-PERCENTS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 21: RATING SHEET INCOME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 21               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-DOLLARS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES-NO.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 22: % RS INC TO BEG LEDGER
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 22               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-PERCENTS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 23: INTEREST & FEE INCOME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 23               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-DOLLARS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES-NO.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 24: % INT & FEE INCOME PER BEGIN LEDGER
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 24               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-PERCENTS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 25: INSURANCE INCOME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 25               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-DOLLARS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES-NO.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 26: % INSURANCE INCOME TO BEGIN LEDGER
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 26               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-PERCENTS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 27: RATING SHEET EXPENSES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 27               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-DOLLARS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES-NO.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 28: EXPENSE PER LOAN MADE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 28               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-DOLLARS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES-NO.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 29: % EXPENSES TO BEGINNING OF LEDGER
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 29               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-PERCENTS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 30: EXPENSES AS A PERCENT OF INCOME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 30               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-PERCENTS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 31: PERSONNEL EXPENSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 31               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-DOLLARS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES-NO.
      *    PERFORM WRITE-DETAIL-LINE.
 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 32: PERSONNEL AS A PERCENT OF INCOME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 32               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-PERCENTS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 33: CHARGEOFF EXPENSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 33               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-DOLLARS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES-NO.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 34: NUMBER OF CHARGEOFFS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 34               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 35: RECOVERY INCOME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 35               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-DOLLARS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES-NO.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 36: BAD DEBT INCREASE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 36               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-DOLLARS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES-NO.
      *    PERFORM WRITE-DETAIL-LINE.
 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 37: % BAD DEBT TO BEGINNING OF LEDGER BALANCE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 37               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-PERCENTS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 38: NF CLAIMS-BANKRUPT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 38               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-DOLLARS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES-NO.
      *    PERFORM WRITE-DETAIL-LINE.
      **************************************************************
      *    2/9/9 PR#625, REPLACE NF CLMS BNKRPT WITH WCBC RS PROFIT
      **************************************************************
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 38: SALES FINANCE PROFIT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 38                         TO D-LINENO.
           MOVE 137                        TO SUB.
           MOVE "WCBC RS PROFIT    "  TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 39: % BANKRUPT NF CLAIMS TO BEGINNING OF LEDGER BALANCE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 39               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-PERCENTS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES.
      *    PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 39: BAD DEBT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 39                     TO D-LINENO.
           MOVE 135                    TO SUB.
           MOVE "WCBC BAD DEBT EXP "  TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 40: NF CLAIMS-NON-BANKRUPT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 40               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 41: % NON BANKRUPT NF CLAIMS TO BEGINNING OF LEDGER BALANCE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 41               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-PERCENTS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 42: TOTAL NF CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 42               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 43: TOTAL BAD DEBT + TOTAL NF CLAIMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 43               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-DOLLARS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES-NO.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 44: % TOTAL TO BEGINNING OF LEDGER BALANCE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE 44               TO SUB D-LINENO.
      *    MOVE LINE-DESC(SUB)   TO D-DESC.
      *    PERFORM MOVE-PERCENTS.
      *    PERFORM COMPUTE-PERCENT-VARIANCES.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DETAIL MEMO "-DELINQUENCIES"
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "-DELINQUENCIES" TO DETAIL-LINE.

      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 45: NO OF 30 DAY ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 45               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 46: AMT OF 30 DAY ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 46               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 47: % AMT OF 30 DAY ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 47               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-PERCENTS.
           PERFORM COMPUTE-PERCENT-VARIANCES.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 48: NO OF 60 DAY ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 48               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 49: AMT OF 60 DAY ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 49               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 50: % AMT OF 60 DAY ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 50               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-PERCENTS.
           PERFORM COMPUTE-PERCENT-VARIANCES.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 51: NO OF 90 DAY ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 51               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 52: AMT OF 90 DAY ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 52               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 53: % AMT OF 90 DAY ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 53               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-PERCENTS.
           PERFORM COMPUTE-PERCENT-VARIANCES.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 54: TOTAL NO. OF DELINQ ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 54               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-NUMBERS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 55: TOTAL AMT OF DELINQ ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 55               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-DOLLARS.
           PERFORM COMPUTE-PERCENT-VARIANCES-NO.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 56: TOTAL % AMT OF DELINQ ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 56               TO SUB D-LINENO.
           MOVE LINE-DESC(SUB)   TO D-DESC.
           PERFORM MOVE-PERCENTS.
           PERFORM COMPUTE-PERCENT-VARIANCES.
           PERFORM WRITE-DETAIL-LINE.

           IF TOT-SUB NOT = 9
              GO TO EXIT-PRINT-TOTALS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 57  (SUB 57 AND 58)  BSR/AM BONUS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF PRINT-BONUS = "Y"
              MOVE 57               TO SUB
              MOVE LINE-DESC(SUB)   TO D-BONUS-LINE1
              MOVE T-MTD(TOT-SUB,SUB) TO D-BONUS1
              MOVE 58               TO SUB
              MOVE LINE-DESC(SUB)   TO D-BONUS-LINE1B
              MOVE T-MTD(TOT-SUB,SUB) TO D-BONUS2
              PERFORM  WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LINE# 58  (SUB 59 AND 60)  CSR/AM BONUS
      * 6/16/16 NO LONGER USED, BUT PRINT IF RUNNING FOR OLDER DATE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 59               TO SUB.
           IF T-MTD(TOT-SUB,SUB) NOT = 0
              MOVE LINE-DESC(SUB)   TO D-BONUS-LINE1
              MOVE T-MTD(TOT-SUB,SUB) TO D-BONUS1.
           MOVE 60               TO SUB.
           IF T-MTD(TOT-SUB,SUB) NOT = 0
              MOVE LINE-DESC(SUB)   TO D-BONUS-LINE1B
              MOVE T-MTD(TOT-SUB,SUB) TO D-BONUS2.
           PERFORM  WRITE-DETAIL-LINE.
       EXIT-PRINT-TOTALS.
           MOVE LOW-VALUES TO TOTAL-TAB(TOT-SUB).

           IF PRINT-1-LINE-FG = "Y"
              IF TOT-SUB < HOLD-LEVEL
                 IF TOT-SUB NOT = 9
                    PERFORM PRINT-LINE-LEVEL-HEAD.

           IF PRINT-1-LINE-FG = "Y"
              IF TOT-SUB NOT = HOLD-LEVEL
                 MOVE TOT-SUB TO HOLD-LEVEL.

           IF EXTRACT-FG = "Y"
              IF TOT-SUB = 9
                 PERFORM CREATE-EXTRACT-REC.

      *****************************************
       CHECK-MIN-MAX SECTION.
      *****************************************

           MOVE "Y" TO WITHIN-MIN-MAX.
           MOVE PRINT-LINE-RPT(1) TO WORK-SUB.
           IF PRINT-LINE-RPT(1) = 38
              MOVE 137 TO WORK-SUB.
           IF PRINT-LINE-RPT(1) = 39
              MOVE 135 TO WORK-SUB.
           IF MIN-MAX-PERIOD = "C"
              MOVE T-MTD(TOT-SUB, WORK-SUB) TO TEST-MTD-AMT
              MOVE T-YTD(TOT-SUB, WORK-SUB) TO TEST-YTD-AMT
           ELSE
              MOVE T-LYR-MTH(TOT-SUB, WORK-SUB) TO TEST-MTD-AMT
              MOVE T-LYR-YTD(TOT-SUB, WORK-SUB) TO TEST-YTD-AMT.

           IF MIN-MAX-FG = "M"
              IF TEST-MTD-AMT <MIN-AMT OR TEST-MTD-AMT > MAX-AMT
                 MOVE "N" TO WITHIN-MIN-MAX
                 GO TO EXIT-CHECK-MIN-MAX.

           IF MIN-MAX-FG = "Y"
              IF TEST-YTD-AMT <MIN-AMT OR TEST-YTD-AMT > MAX-AMT
                 MOVE "N" TO WITHIN-MIN-MAX
                 GO TO EXIT-CHECK-MIN-MAX.

           IF MIN-MAX-FG = "B"
              IF (TEST-MTD-AMT <MIN-AMT OR TEST-MTD-AMT > MAX-AMT)
              OR (TEST-YTD-AMT <MIN-AMT OR TEST-YTD-AMT > MAX-AMT)
                 MOVE "N" TO WITHIN-MIN-MAX
                 GO TO EXIT-CHECK-MIN-MAX.

       EXIT-CHECK-MIN-MAX.
           EXIT.

      *****************************************
       PRINT-LINE-LEVEL-HEAD SECTION.
      *****************************************

           MOVE ALL "-" TO D-LEVEL-DESC.
           COMPUTE LN-WK = LN-FEED + LN-COUNT.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE SPACES TO DETAIL-LINE.
           ADD LN-FEED TO LN-COUNT.

      *****************************************
       COMPUTE-PERCENT-VARIANCES SECTION.
      *****************************************
           COMPUTE D-MTD-VAR =
                   T-MTD(TOT-SUB,SUB) - T-LYR-MTH(TOT-SUB,SUB).
           COMPUTE D-YTD-VAR =
                   T-YTD(TOT-SUB,SUB) - T-LYR-YTD(TOT-SUB,SUB).
           MOVE 0 TO D-MTD-VAR-PCT.
           IF T-LYR-MTH(TOT-SUB,SUB) NOT = 0
              COMPUTE D-MTD-VAR-PCT ROUNDED =
                   (T-MTD(TOT-SUB,SUB) - T-LYR-MTH(TOT-SUB,SUB))
                   / T-LYR-MTH(TOT-SUB,SUB) * 100.
           MOVE 0 TO D-YTD-VAR-PCT.
           IF T-LYR-YTD(TOT-SUB,SUB) NOT = 0
              COMPUTE D-YTD-VAR-PCT ROUNDED =
                   (T-YTD(TOT-SUB,SUB) - T-LYR-YTD(TOT-SUB,SUB))
                   / T-LYR-YTD(TOT-SUB,SUB) * 100.
           IF (EXTRACT-FG = "Y" AND TOT-SUB = 9 AND SUB < 57)
              COMPUTE EXT-PCT-MTD-VAR(SUB) =
                   T-MTD(TOT-SUB,SUB) - T-LYR-MTH(TOT-SUB,SUB)
              COMPUTE EXT-PCT-YTD-VAR(SUB) =
                   T-YTD(TOT-SUB,SUB) - T-LYR-YTD(TOT-SUB,SUB)
              MOVE 0 TO EXT-PCT-MTD-VAR-PCT(SUB)
              IF T-LYR-MTH(TOT-SUB,SUB) NOT = 0
                 COMPUTE EXT-PCT-MTD-VAR-PCT(SUB) ROUNDED =
                   (T-MTD(TOT-SUB,SUB) - T-LYR-MTH(TOT-SUB,SUB))
                   / T-LYR-MTH(TOT-SUB,SUB) * 100
              END-IF
              MOVE 0 TO EXT-PCT-YTD-VAR-PCT(SUB)
              IF T-LYR-YTD(TOT-SUB,SUB) NOT = 0
                 COMPUTE EXT-PCT-YTD-VAR-PCT(SUB) ROUNDED =
                   (T-YTD(TOT-SUB,SUB) - T-LYR-YTD(TOT-SUB,SUB))
                   / T-LYR-YTD(TOT-SUB,SUB) * 100.

      *****************************************
       COMPUTE-PERCENT-VARIANCES-NO SECTION.
      *****************************************
           COMPUTE D-MTD-VAR-NO ROUNDED =
                   T-MTD(TOT-SUB,SUB) - T-LYR-MTH(TOT-SUB,SUB).
           COMPUTE D-YTD-VAR-NO ROUNDED =
                   T-YTD(TOT-SUB,SUB) - T-LYR-YTD(TOT-SUB,SUB).
           MOVE 0  TO D-MTD-VAR-PCT.
           IF T-LYR-MTH(TOT-SUB,SUB) NOT = 0
              COMPUTE D-MTD-VAR-PCT ROUNDED =
                   (T-MTD(TOT-SUB,SUB) - T-LYR-MTH(TOT-SUB,SUB))
                   / T-LYR-MTH(TOT-SUB,SUB) * 100.
           MOVE 0  TO D-YTD-VAR-PCT.
           IF T-LYR-YTD(TOT-SUB,SUB) NOT = 0
           COMPUTE D-YTD-VAR-PCT ROUNDED =
                   (T-YTD(TOT-SUB,SUB) - T-LYR-YTD(TOT-SUB,SUB))
                   / T-LYR-YTD(TOT-SUB,SUB) * 100.
           IF (EXTRACT-FG = "Y" AND TOT-SUB = 9 AND SUB < 57)
              COMPUTE EXT-MTD-VAR-NO(SUB) ROUNDED =
                   T-MTD(TOT-SUB,SUB) - T-LYR-MTH(TOT-SUB,SUB)
              COMPUTE EXT-YTD-VAR-NO(SUB) ROUNDED =
                   T-YTD(TOT-SUB,SUB) - T-LYR-YTD(TOT-SUB,SUB)
              MOVE 0  TO EXT-MTD-VAR-PCT(SUB)
              IF T-LYR-MTH(TOT-SUB,SUB) NOT = 0
                 COMPUTE EXT-MTD-VAR-PCT(SUB) ROUNDED =
                   (T-MTD(TOT-SUB,SUB) - T-LYR-MTH(TOT-SUB,SUB))
                   / T-LYR-MTH(TOT-SUB,SUB) * 100
              END-IF
              MOVE 0  TO EXT-YTD-VAR-PCT(SUB)
              IF T-LYR-YTD(TOT-SUB,SUB) NOT = 0
              COMPUTE EXT-YTD-VAR-PCT(SUB) ROUNDED =
                   (T-YTD(TOT-SUB,SUB) - T-LYR-YTD(TOT-SUB,SUB))
                   / T-LYR-YTD(TOT-SUB,SUB) * 100.
           IF (EXTRACT-FG = "Y" AND TOT-SUB = 9 AND SUB = 137)
              COMPUTE EXT-MTD-VAR-NO(38) ROUNDED =
                   T-MTD(TOT-SUB,SUB) - T-LYR-MTH(TOT-SUB,SUB)
              COMPUTE EXT-YTD-VAR-NO(38) ROUNDED =
                   T-YTD(TOT-SUB,SUB) - T-LYR-YTD(TOT-SUB,SUB)
              MOVE 0  TO EXT-MTD-VAR-PCT(38)
              IF T-LYR-MTH(TOT-SUB,SUB) NOT = 0
                 COMPUTE EXT-MTD-VAR-PCT(38) ROUNDED =
                   (T-MTD(TOT-SUB,SUB) - T-LYR-MTH(TOT-SUB,SUB))
                   / T-LYR-MTH(TOT-SUB,SUB) * 100
              END-IF
              MOVE 0  TO EXT-YTD-VAR-PCT(38)
              IF T-LYR-YTD(TOT-SUB,SUB) NOT = 0
              COMPUTE EXT-YTD-VAR-PCT(38) ROUNDED =
                   (T-YTD(TOT-SUB,SUB) - T-LYR-YTD(TOT-SUB,SUB))
                   / T-LYR-YTD(TOT-SUB,SUB) * 100.
           IF (EXTRACT-FG = "Y" AND TOT-SUB = 9 AND SUB = 135)
              COMPUTE EXT-MTD-VAR-NO(39) ROUNDED =
                   T-MTD(TOT-SUB,SUB) - T-LYR-MTH(TOT-SUB,SUB)
              COMPUTE EXT-YTD-VAR-NO(39) ROUNDED =
                   T-YTD(TOT-SUB,SUB) - T-LYR-YTD(TOT-SUB,SUB)
              MOVE 0  TO EXT-MTD-VAR-PCT(39)
              IF T-LYR-MTH(TOT-SUB,SUB) NOT = 0
                 COMPUTE EXT-MTD-VAR-PCT(39) ROUNDED =
                   (T-MTD(TOT-SUB,SUB) - T-LYR-MTH(TOT-SUB,SUB))
                   / T-LYR-MTH(TOT-SUB,SUB) * 100
              END-IF
              MOVE 0  TO EXT-YTD-VAR-PCT(39)
              IF T-LYR-YTD(TOT-SUB,SUB) NOT = 0
              COMPUTE EXT-YTD-VAR-PCT(39) ROUNDED =
                   (T-YTD(TOT-SUB,SUB) - T-LYR-YTD(TOT-SUB,SUB))
                   / T-LYR-YTD(TOT-SUB,SUB) * 100.


      *****************************************
       WRITE-DETAIL-LINE SECTION.
      *****************************************

           IF PRINT-LINE-RPT(1) = 0
              GO TO SKIP-LINE-PRINT-TEST.

           MOVE "N" TO PRINT-THE-LINE.
           PERFORM VARYING WORK-SUB FROM 1 BY 1 UNTIL WORK-SUB > 10
              IF PRINT-LINE-RPT(WORK-SUB) = SUB
                 MOVE "Y" TO PRINT-THE-LINE
              END-IF
              IF (PRINT-LINE-RPT(WORK-SUB) = 38 AND SUB = 137)
                 MOVE "Y" TO PRINT-THE-LINE
              END-IF
              IF (PRINT-LINE-RPT(WORK-SUB) = 39 AND SUB = 135)
                 MOVE "Y" TO PRINT-THE-LINE
              END-IF
           END-PERFORM.
           IF PRINT-THE-LINE = "N"
              GO TO CONTINUE-WRITE-DET.

       SKIP-LINE-PRINT-TEST.
      *    IF D-MTD NOT = SPACES
      *    IF D-YTD  = SPACES
      *       GO TO CONTINUE-WRITE-DET.

           COMPUTE LN-WK = LN-FEED + LN-COUNT.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.


           IF PRINT-1-LINE-FG = "Y"
              IF TOT-SUB = 9
                 MOVE BR-NAME      TO H-GRNAME
                 MOVE BR-NO       TO D-BRNO
                 MOVE H-GRNAME     TO D-DESC
              ELSE
                 MOVE GR-ID     TO D-GRID
                 MOVE "--> "    TO D-GRDASH1
                 MOVE GR-NAME   TO D-GRNAME
              END-IF
              IF HEAD2-TYPE  = "GRAND :"
                 MOVE SPACES    TO D-GRID
                 MOVE "--> "    TO D-GRDASH1
                 MOVE "GRAND TOTALS :" TO D-GRNAME.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.

       CONTINUE-WRITE-DET.
           MOVE SPACES TO DETAIL-LINE.

       WRITE-DETAIL-EXIT.
           EXIT.

      ***************************************************
       HEAD-PAGE SECTION.
      ***************************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT   TO H-TIME.

           MOVE HEAD1 TO PR-REC.
           MOVE 99    TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF PRINT-1-LINE-FG = "Y"
              MOVE "LINE #: "             TO HEAD2-TYPE
              MOVE LINE-RPT (1)           TO H-BRNO WORK-SUB
              MOVE LINE-DESC(WORK-SUB)    TO H-GRNAME.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE SPACES TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 3 TO LN-COUNT LN-HEAD.
           MOVE 1 TO LN-FEED.

      ***************************************************
       PRINT-RANGE-LINES SECTION.
      ***************************************************
           MOVE 99 TO LN-FEED.
           MOVE "REPORT SELECTION:" TO H-GRNAME.

           MOVE SPACES                 TO RANGE-LINE.
           IF GROUP-FLAG = "Y"
              MOVE "GROUP SELECTION    :" TO RANGE-DESC
              MOVE ENT-GROUP              TO RANGE-LOW
              MOVE RANGE-LINE             TO PR-REC
              PERFORM WRITE-PR-REC
           ELSE
              MOVE "BRANCH RANGE       :" TO RANGE-DESC
              MOVE BEG-BRANCH             TO RANGE-LOWN
              MOVE END-BRANCH             TO RANGE-HIGHN
              MOVE RANGE-LINE             TO PR-REC
              PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                 TO RANGE-LINE.
           MOVE "LEVEL TOTALS       :" TO RANGE-DESC.
           MOVE TOTAL-LEV-TABLE        TO RANGE-LOW.
           MOVE RANGE-LINE             TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                 TO RANGE-LINE.
           MOVE "#LINES PER PAGE    :" TO RANGE-DESC.
           MOVE LINES-PER-PAGE         TO RANGE-LOWN.
      *    IF LINES-PER-PAGE = 99
      *       MOVE "DEFAULT"           TO RANGE-LOW.
           MOVE RANGE-LINE             TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                 TO RANGE-LINE.
           MOVE "PRINT 1 LINE ONLY ?:" TO RANGE-DESC.
           MOVE PRINT-1-LINE-FG        TO RANGE-LOW.
           MOVE RANGE-LINE             TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                 TO RANGE-LINE.
           MOVE "RATING LINES       :" TO RANGE-DESC.
           IF LINE-RPT(1) = 0
              MOVE "ALL LINES" TO RANGE-LOW
           ELSE
              MOVE LINE-RPT(1) TO RANGE-LOWN.
           MOVE RANGE-LINE             TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                 TO RANGE-LINE.
           MOVE "MINIMUM/MAXIMUM AMT:" TO RANGE-DESC.
           MOVE MIN-AMT                TO RANGE-LOWDEC.
           MOVE MAX-AMT                TO RANGE-HIGHDEC.
           MOVE RANGE-LINE             TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                 TO RANGE-LINE.
           MOVE "CURRENT OR LAST YR :" TO RANGE-DESC.
           MOVE MIN-MAX-PERIOD         TO RANGE-LOW.
           MOVE RANGE-LINE             TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                 TO RANGE-LINE.
           MOVE "MTD, YTD, OR BOTH  :" TO RANGE-DESC.
           MOVE MIN-MAX-FG             TO RANGE-LOW.
           MOVE RANGE-LINE             TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                 TO RANGE-LINE.
           MOVE "EXTRACT FILE       :" TO RANGE-DESC.
           MOVE EXTRACT-FG             TO RANGE-LOW.
           MOVE RANGE-LINE             TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.


      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBWI/GRTOTL.CPY".


      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO RATING-CONAME.
           MOVE EXT-CONAME-SYSDATE TO RATING-SYSDATE.
           DISPLAY RATING-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE RATING-FORM-FIELDS TO WR-BUF.
           MOVE RATING-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE RATING-HELP--FILE TO HELP-LINK-FILE.
           MOVE RATING-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/RATING_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************************************
       MISC-PARA SECTION.
      ***************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
       GET-TODAYS-DATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           MOVE ALP-MO TO DISPLAY-MO.
           MOVE ALP-YR TO DISPLAY-YR.
       MOVE-NUMBERS.
           COMPUTE D-MTD-NO = T-MTD(TOT-SUB,SUB).
           COMPUTE D-YTD-NO = T-YTD(TOT-SUB,SUB).
           COMPUTE D-LYR-MTD-NO    = T-LYR-MTH(TOT-SUB,SUB).
           COMPUTE D-LYR-YTD-NO    = T-LYR-YTD(TOT-SUB,SUB).
           IF (EXTRACT-FG = "Y" AND TOT-SUB = 9 AND SUB < 57)
              COMPUTE EXT-MTD-NO(SUB) = T-MTD(TOT-SUB,SUB)
              COMPUTE EXT-YTD-NO(SUB) = T-YTD(TOT-SUB,SUB)
              COMPUTE EXT-LYR-MTD-NO (SUB)= T-LYR-MTH(TOT-SUB,SUB)
              COMPUTE EXT-LYR-YTD-NO(SUB) = T-LYR-YTD(TOT-SUB,SUB).
       MOVE-PERCENTS.
           COMPUTE D-MTD = T-MTD(TOT-SUB,SUB).
           COMPUTE D-YTD = T-YTD(TOT-SUB,SUB).
           COMPUTE D-LYR-MTD    = T-LYR-MTH(TOT-SUB,SUB).
           COMPUTE D-LYR-YTD    = T-LYR-YTD(TOT-SUB,SUB).
           IF (EXTRACT-FG = "Y" AND TOT-SUB = 9 AND SUB < 57)
              COMPUTE EXT-PCT-MTD(SUB) = T-MTD(TOT-SUB,SUB)
              COMPUTE EXT-PCT-YTD(SUB) = T-YTD(TOT-SUB,SUB)
              COMPUTE EXT-PCT-LYR-MTD(SUB) = T-LYR-MTH(TOT-SUB,SUB)
              COMPUTE EXT-PCT-LYR-YTD(SUB) = T-LYR-YTD(TOT-SUB,SUB).
       MOVE-DOLLARS.
           COMPUTE D-MTD-NO      ROUNDED = T-MTD(TOT-SUB,SUB) * 1.
           COMPUTE D-YTD-NO      ROUNDED = T-YTD(TOT-SUB,SUB) * 1.
           COMPUTE D-LYR-MTD-NO  ROUNDED = T-LYR-MTH(TOT-SUB,SUB) * 1.
           COMPUTE D-LYR-YTD-NO  ROUNDED = T-LYR-YTD(TOT-SUB,SUB) * 1.
           IF (EXTRACT-FG = "Y" AND TOT-SUB = 9 AND SUB < 57)
              COMPUTE EXT-MTD-NO(SUB) ROUNDED = T-MTD(TOT-SUB,SUB) * 1
              COMPUTE EXT-YTD-NO(SUB) ROUNDED = T-YTD(TOT-SUB,SUB) * 1
              COMPUTE EXT-LYR-MTD-NO(SUB) ROUNDED =
                 T-LYR-MTH(TOT-SUB,SUB) * 1
              COMPUTE EXT-LYR-YTD-NO(SUB) ROUNDED =
                 T-LYR-YTD(TOT-SUB,SUB) * 1.
           IF (EXTRACT-FG = "Y" AND TOT-SUB = 9 AND SUB = 137)
              COMPUTE EXT-MTD-NO(38) ROUNDED = T-MTD(TOT-SUB,SUB) * 1
              COMPUTE EXT-YTD-NO(38) ROUNDED = T-YTD(TOT-SUB,SUB) * 1
              COMPUTE EXT-LYR-MTD-NO(38) ROUNDED =
                 T-LYR-MTH(TOT-SUB,SUB) * 1
              COMPUTE EXT-LYR-YTD-NO(38) ROUNDED =
                 T-LYR-YTD(TOT-SUB,SUB) * 1.
           IF (EXTRACT-FG = "Y" AND TOT-SUB = 9 AND SUB = 135)
              COMPUTE EXT-MTD-NO(39) ROUNDED = T-MTD(TOT-SUB,SUB) * 1
              COMPUTE EXT-YTD-NO(39) ROUNDED = T-YTD(TOT-SUB,SUB) * 1
              COMPUTE EXT-LYR-MTD-NO(39) ROUNDED =
                 T-LYR-MTH(TOT-SUB,SUB) * 1
              COMPUTE EXT-LYR-YTD-NO(39) ROUNDED =
                 T-LYR-YTD(TOT-SUB,SUB) * 1.

       COPY "LIBSP/SPWRCL.CPY".

      ***************************************************
       IO-ROUTINES SECTION.
      ***************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPWRGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBSP/SPWR1RN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       OPEN-RATING-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE RATING-PATH TO E-FILE.
           OPEN OUTPUT RATING-FILE.
           IF IO-FG = 8
              GO TO OPEN-RATING-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE RATING-FILE
              GO TO OPEN-RATING-FILE-OUTPUT.
       READ-RATING-FILE.
           PERFORM READ-IT.
           MOVE RATING-PATH TO E-FILE.
           READ RATING-FILE.
           IF IO-FG = 8
              GO TO READ-RATING-FILE.
       WRITE-RATING-FILE.
           PERFORM WRITE-IT.
           MOVE RATING-PATH TO E-FILE.
           WRITE RATING-REC.
           IF IO-FG = 8
              GO TO WRITE-RATING-FILE.
       CLOSE-RATING-FILE.
           PERFORM CLOSE-IT.
           CLOSE RATING-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ***************************************************
       END-PROGRAM SECTION.
      ***************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF EXT-EOBUF-GROUP-FG = "Y"
              IF SV-BEG-BR NOT = 9999
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY RATING-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
