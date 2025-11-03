       IDENTIFICATION DIVISION.
       PROGRAM-ID.      OPREPT.
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  WORLD OPERATIONS REPORT
      *
      * REV:
      *  BLK 042694 FIX CHECK-ADD-TOTALS TO SEARCH HIERARCHY
      *  JTG 061496 CHANGED 'INCREASE REMAIN' FOR NO ACCOUNTS
      *             CALCULATION.
      *  KEC 090396 ADDED 'LOCAL-SPOOLING' LOGIC.
      *  KEC 091096 FIXED PRINTING "NO REPORT GENERATED" WHEN EXITING.
      *  KEC 091196 FIXED 'LOCAL-SPOOLING' LOGIC TO WORK WITH
      *             'GROUP-TOTALS' LOGIC. WITH "Y" TO LOCAL-PRINT-FG,
      *             ALLOW ONLY "Y" TO ONLY GROUP-TOTAL-LEVEL OF '9'.
      *  BLV 122396 REVISED LOCAL SPOOLING LOGIC
      *  JTG 030397 CHANGED 'INCREASE REMAIN' FOR NO ACCOUNTS CALCULATION
      *             TO INCLUDE 180+ ACCOUNTS
      *  BLV 072397 FIX TODAYS TOTAL ADDING TO BEG MO GAIN/LOSS &
      *             DELINQUENCY
      *  BLV 022398 FIX PREV-SCURR TABLE TO BE COMP
      *  MJD 041798 NOT CHECKING EXT-ROUTE-BUF BEFORE CALCULATING DATE.
      *             DISPLAYING "(P2)" IN DATE FIELD WHEN IN EOMAIN.
      *  BLV 042098 FIX BEG MONTH DELINQUENCY ROUTINE
      *  BLV 042798 CHANGE BEG MONTH DELINQUENCY OPTION TO GATHER
      *             BEGMO & BEGMO FROM NEXT MONTHS STATS (NOT CUR & BEGMO)
      *  BLV 043098 DISPLAY DEL-FLAG IN DISPLAY-PARAMETERS
      *  BLV 051198 FIX TY-NO GETTING ADDED INSTEAD OF TY-NO-PL
      *  BLV 052998 FIXED TOTAL LEVEL FLAGS NOT DISPLAYING IN EOMAIN
      *  BLV 060298 FIX TO USE TY-ALL-PATH; USE RC FIRST-OPEN-DAY LOGIC; FIX
      *             TO SKIP TODAYS TOTALS IF NOT RUNNING FOR CURRENT MONTH
      *  BLV 000207 CHANGE FIRST-OPEN-DATE-YYYYMMDD FROM 6 DIGITS TO 7, TODAYS
      *             TOTALS WERE NOT INCLUDED SINCE 1/1/00
      *  BLV 000807 FIX GROUP PROBLEM IF INCONSISTENT LEVELS IN GROUP
      *             HIERARCHY, SEE GROUP.BUG IN /USR/A0102B/WORLD
      *  BLV 021125 SUBTRACT P2'S FROM P&L ACCUMULATIONS FOR WORLD -
      *             THEY'RE USING P2'S FOR NON-FILE PMTS, THIS ALSO FIXES
      *             INCREASE REMAINING CALCULATION TO NOT INCLUDE P2'S
      *  BAH 051222 H-TITLE PRINT EO-COMMENT WHICH IS PASSED THRU EXT-EOBUF-MESS
      *             FROM EOEXEC, WORLD PR# 449
      *  BLF 060112 IF GP-NO-SC-USE-DM = "Y", MOVE "DM" TO SOURCE DESCRIPTION
      *             INSTEAD OF "SC", WORLD PR# 453
      *  BLF 061114 EXPANDED ALL GROUP SUBSCRIPTS TO ACCOMODATE
      *             EXPANDED GROUP SEARCH TABLES, WORLD PR# 502
      *  BLM 111118 FIX YTD VOLUME TO PRINT A BILLION, WORLD PR# 780
      *  BLM 111121 ALSO INCREASE GAIN-LOSS WORKERS & DELINQUENCY
      *             WORKERS TO HANDLE A BILLION, WORLD PR# 780
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *  BLM 160314 FIX FOR NEW BY-DATE FORMAT, PD0003
      *
      * KEC 2016.0929 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED BY-DATE TO BY-DATE-CCYYMM FOR CONSISTENCY.
      *
      *  BAH 170707 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181018 GLOBAL TYFILE
      * KEC 190212 A32 F2SL - REVISED FOR SPLITTING THE BYFILE.
      *            REMOVED LOAN APP FIELDS IN BY & TY.
      * BAH 2019-0922 REMOVED ACCESS-CALL RCFILE, TYFILE
      * CS 2021.0114 ALLOW FOR CLASS TO HAVE ALPHA INPUT OF 'S' SALES
      *              'L'LOAN CLASSES,'A'WORLD SMALL CLASSES, 'W'WCBC,
      *              'O' WORLD OTHER CLASSES,'F' WORLD SF CLASSES, 'M'
      *               SMALL-LOAN-CLASSES, 'R' NON SMALL LN-CLASSES,
      *              'I' IB CLASSES PR# 1485
      * RMZ 20230808 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      * RMZ 20240116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
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
      *     W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/OPREPT S35 05/09/24-08:34:07 >".

      *=================================================================
      *
      *     F I L E - I F N
      *
      *=================================================================
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01BY.CPY".
       COPY "LIBLP/LP01BYA.CPY".
       COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LP01BY_SQL.CPY".
       COPY "LIBLP/LPWSBY.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".

      *=================================================================
      *
      *    LINE WORKERS
      *
      *=================================================================
       01  LN-WORKERS.
           03  LN-FEED          PIC 999   COMP   VALUE 0.
           03  LN-COUNT         PIC 999   COMP   VALUE 99.
           03  LN-WK            PIC 999   COMP   VALUE 0.

       01  PAGE-NUMBER          PIC 9(4)  COMP   VALUE 0.
       01  BY1-CONN-OPEN-SW     PIC X            VALUE "N".

      *=================================================================
      *
      *    MISC WORKERS
      *
      *=================================================================
       01  LEVEL-SUB            PIC 9999.
       01  TOT-SUB              PIC 99.
       01  GROUP-SUB            PIC 9999.
       01  PREV-SUB             PIC 9999.
       01  TYPE-SUB             PIC 99.
       01  DEL-SUB              PIC 99.
       01  PER-SUB              PIC 99.
       01  PER-SUB1             PIC 99.
       01  PER-SUB2             PIC 99.
       01  AGE-SUB              PIC 99.
       01  AGE-IN               PIC 99.
       01  AGE-OUT              PIC 99.
       01  REC-SUB              PIC 99.
       01  INS-SUB              PIC 99.
       01  PL-SUB               PIC 99.

       01  ENTER-CLASS-WK       PIC 99.
       01  ENTER-CLASS-ALPHA REDEFINES ENTER-CLASS-WK.
           03  CLASS-WK1        PIC X.
           03  CLASS-WK2        PIC X.

       01  CORRECT-CLASS        PIC X VALUE "N".

       01  ALL-CLASSES-FLAG         PIC X    VALUE "N".
           88  ONE-CLASS                     VALUE "1".
           88  ALL-CLASSES                   VALUE "Y".
           88  SF-CLASSES                    VALUE "S".
           88  LN-CLASSES                    VALUE "L".
           88  WORLD-SMALL-CLASSES           VALUE "A".
           88  WCBC-CLASSES                  VALUE "W".
           88  WORLD-OTHER-CLASSES           VALUE "O".
           88  WORLD-SF-CLASSES              VALUE "F".
           88  SMALL-LN-CLASSES              VALUE "M".
           88  NON-SMALL-LN-CLASSES          VALUE "R".
           88  IB-LN-CLASSES                 VALUE "I".

       01  HOLD-BY1-KEY.
           03  HOLD-BY-DATE-CCYYMM  PIC 9(6).
           03  HOLD-BY-BRNO         PIC 9(4).
           03  HOLD-BY-CLASS        PIC 9(3).

       01  CUR-MO-BY-DATE-CCYYMM    PIC 9(6).
       01  NEXT-MO-BY-DATE-CCYYMM      PIC 9(6).

       01  SOURCE-VALUES.
           03  FILLER           PIC X(27) VALUE
               "NEW      REFERRALSFORMER   ".
           03  FILLER           PIC X(27) VALUE
               "PRESENT  PB DELNQ PB INCRS ".
           03  FILLER           PIC X(18) VALUE
               "RENEWALS SC       ".
           03  FILLER           PIC X(18) VALUE
               "OTHER    TOTAL    ".

       01  SOURCE-TABLE REDEFINES SOURCE-VALUES.
           03  SOURCE-ELE       PIC X(9) OCCURS 10 TIMES.

       01  BUCKET-VALUES.
           03  FILLER           PIC X(28) VALUE
               "30     60     90-179 180+   ".
           03  FILLER           PIC X(28) VALUE
               "DEL    MISPAYSPOTS   SLOW   ".

       01  BUCKET-TABLE REDEFINES BUCKET-VALUES.
           03  BUCKET-ELE       PIC X(7) OCCURS 8 TIMES.

       01  TOTAL-TABLE.
           03  TOTAL-TAB        OCCURS 10.
      *  SECTION 1 - GAIN/LOSS
               05  GAIN-LOSS-NO-CUR     PIC S9(8).
               05  GAIN-LOSS-AMT-CUR    PIC S9(11)V99.
               05  GAIN-LOSS-NO-MTD     PIC S9(8).
               05  GAIN-LOSS-AMT-MTD    PIC S9(11)V99.
               05  GAIN-LOSS-NO-YTD     PIC S9(8).
               05  GAIN-LOSS-AMT-YTD    PIC S9(11)V99.
      *  SECTION 2 -  VOLUME (SUB2: 1= MTD, 2= YTD)
      *                      (SUB3: 1-9=TYPE, 10 = TOTAL)
               05  VOLUME-TOTS OCCURS 2.
                   07  VOLUME-TYPES OCCURS 10.
                       09  VOL-NO-APPS      PIC S9(8).
                       09  VOL-NO           PIC S9(8).
                       09  VOL-AMT          PIC S9(11)V99.
      *  SECTION 3 - DELINQUENCY-RECENCY
      *                      (SUB 2: 1=REC, 2= CONTRACTUAL)
      *                      (SUB 3: 1=30, 2=60, 3=90+120+150, 4=180+210
      *                              5=DEL SUB-TOTAL(TOT OF 1-4), 6=MISPAYS,
      *                              7= 30 DAY POTS, 8=TOTAL SLOW
               05  DELINQUINCY-TOTS OCCURS 2.
                   07  DELINQUINCY-TYPES OCCURS 8.
                       09  DEL-NO-CUR        PIC S9(8).
                       09  DEL-AMT-CUR       PIC S9(11)V99.
                       09  DEL-NO-MTD        PIC S9(8).
                       09  DEL-AMT-MTD       PIC S9(11)V99.
      *  SECTION 4 - CHARGE-OFFS
               05  PL-AMT-MTD                PIC S9(9)V99.
               05  PL-NO-MTD                 PIC S9(8).
               05  PL-AMT-YTD                PIC S9(9)V99.
               05  PL-NO-YTD                 PIC S9(8).
      *  SECTION 5 - INSURANCE
               05  INS-TABLE                 OCCURS 6.
                   07  INS-NO-MTD            PIC S9(8).

       01  PREV-SCURR-TABLE.
           03  PREV-SCURR       PIC 9999 COMP OCCURS 15.

       01  FIRST-TIME           PIC X        VALUE "Y".

       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
           88  SKIP-RECORDS-FOUND            VALUE " ".

       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  DISPLAY-DATE             PIC 9(4).
       01  DISPLAY-DATEX            REDEFINES DISPLAY-DATE.
           03  DISPLAY-MO           PIC 99.
           03  DISPLAY-YR           PIC 99.
       01  DATE-LIST                PIC X(5)  VALUE "MMYY.".
       01  FIRST-OPEN-DATE-YYYYMMDD PIC 9(8)  VALUE 0.

       01  NO-WORK              PIC S9(7)     VALUE +0.
       01  AMT-WORK             PIC S9(10)V99 VALUE +0.
       01  AVG-WORK             PIC S9(10)V99 VALUE +0.
       01  PCT-WORK             PIC S9(3)V9   VALUE +0.

       01  STAR-LINE.
           03  FILLER          PIC X(40) VALUE
           "________________________________________".
           03  FILLER          PIC X(40) VALUE
           "________________________________________".


      *=================================================================
      *
      *     S C R E E N - F I E L D S
      *
      *=================================================================
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  STORE-DATE      PIC 9(4).
           03  STORE-DATEX     REDEFINES STORE-DATE.
               05  STORE-MO    PIC 9(2).
               05  STORE-YR    PIC 9(2).
           03  BRANCH          PIC 9999.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  BEG-CLASS       PIC 9(3).
           03  BEG-CLASSX REDEFINES BEG-CLASS PIC X(3).
           03  END-CLASS       PIC 9(3).
           03  END-CLASSX REDEFINES END-CLASS PIC X(3).
           03  DEL-FLAG        PIC X.
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE  PIC X OCCURS 10.

       01  VERSION-CONTROL     PIC X     VALUE "D".
       01  SV-BRANCH           PIC 9999 VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      *=================================================================
      *
      *     P R I N T   B U F F E R S
      *
      *=================================================================
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(5)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(11)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X        VALUE " ".
           03  H-TITLE          PIC X(27) VALUE
           "- -  OPERATIONS REPORT  - -".
           03  FILLER           PIC X(2)     VALUE " ".
           03  H-MMYY           PIC 99/99.

       01  HEAD3.
           03  HEAD3-TYPE       PIC X(10).
           03  H-GRID           PIC X(4).
           03  H-BRNO REDEFINES H-GRID PIC ZZ99.
           03  FILLER           PIC X VALUE " ".
           03  H-GRNAME         PIC X(20) VALUE SPACES.

       01  DETAIL1-HEAD1.
           03  FILLER           PIC X(35) VALUE SPACES.
           03  FILLER           PIC X(9)  VALUE "GAIN/LOSS".
           03  FILLER           PIC X(36) VALUE SPACES.

       01  DETAIL1-HEAD2.
           03  FILLER           PIC X(33) VALUE
               "CURRENT OUTSTANDING".
           03  FILLER           PIC X(13) VALUE "MONTH-TO-DATE".
           03  FILLER           PIC X(34) VALUE SPACES.

       01  DETAIL1-HEAD3.
           03  FILLER           PIC X(33) VALUE
               " # ACCTS            DOLLARS      ".
           03  FILLER           PIC X(39) VALUE
               "# ACCTS      %           DOLLARS      %".
           03  FILLER           PIC X(8)  VALUE SPACES.

       01  DETAIL1-HEAD4.
           03  FILLER           PIC X(34) VALUE SPACES.
           03  FILLER           PIC X(12) VALUE "YEAR-TO-DATE".
           03  FILLER           PIC X(34) VALUE SPACES.

       01  DETAIL1-HEAD5.
           03  FILLER           PIC X(33) VALUE SPACES.
           03  FILLER           PIC X(39) VALUE
               "# ACCTS      %           DOLLARS      %".
           03  FILLER           PIC X(8)  VALUE SPACES.

       01  DETAIL2-HEAD1.
           03  FILLER           PIC X(37) VALUE SPACES.
           03  FILLER           PIC X(6)  VALUE "VOLUME".
           03  FILLER           PIC X(37) VALUE SPACES.

       01  DETAIL2-HEAD2.
           03  FILLER           PIC X(33) VALUE SPACES.
           03  FILLER           PIC X(13) VALUE "MONTH-TO-DATE".
           03  FILLER           PIC X(34) VALUE SPACES.

       01  DETAIL2-HEAD2-YTD.
           03  FILLER           PIC X(34) VALUE SPACES.
           03  FILLER           PIC X(12) VALUE "YEAR-TO-DATE".
           03  FILLER           PIC X(34) VALUE SPACES.

       01  DETAIL2-HEAD3.
           03  FILLER           PIC X(12) VALUE SPACES.
           03  FILLER           PIC X(29) VALUE
               "# APPS   # MADE        %     ".
           03  FILLER           PIC X(39) VALUE
               "        DOLLARS        %       AVG LOAN".

       01  DETAIL3-HEAD1.
           03  FILLER           PIC X(29) VALUE SPACES.
           03  FILLER           PIC X(21)  VALUE
               "DELINQUENCY - RECENCY".
           03  FILLER           PIC X(30) VALUE SPACES.

       01  DETAIL3-HEAD2.
           03  FILLER           PIC X(18) VALUE SPACES.
           03  FILLER           PIC X(16) VALUE
               "CURRENT POSITION".
           03  FILLER           PIC X(20) VALUE SPACES.
           03  FILLER           PIC X(24) VALUE
               "NET CHANGE MONTH-TO-DATE".
           03  FILLER           PIC X(2) VALUE SPACES.

       01  DETAIL3-HEAD3.
           03  FILLER           PIC X(16) VALUE SPACES.
           03  FILLER           PIC X(34) VALUE
               "#          $ AMOUNT      %      ".
           03  FILLER           PIC X(30) VALUE
               "  #          $ AMOUNT      %  ".

       01  DETAIL3-HEAD1-CONTRACT.
           03  FILLER           PIC X(29) VALUE SPACES.
           03  FILLER           PIC X(22)  VALUE
               "DELINQUENCY - CONTRACT".
           03  FILLER           PIC X(29) VALUE SPACES.

       01  DETAIL3-HEAD2-CONTRACT.
           03  FILLER           PIC X(21) VALUE SPACES.
           03  FILLER           PIC X(9) VALUE
               "REMAINING".
           03  FILLER           PIC X(24) VALUE SPACES.
           03  FILLER           PIC X(24) VALUE
               "NET CHANGE MONTH-TO-DATE".
           03  FILLER           PIC X(2) VALUE SPACES.

       01  DETAIL4-HEAD1.
           03  FILLER           PIC X(35) VALUE SPACES.
           03  FILLER           PIC X(10)  VALUE
               "CHARGE OFF".
           03  FILLER           PIC X(35) VALUE SPACES.

       01  DETAIL4-HEAD2.
           03  FILLER           PIC X(19) VALUE SPACES.
           03  FILLER           PIC X(13) VALUE
               "MONTH-TO-DATE".
           03  FILLER           PIC X(22) VALUE SPACES.
           03  FILLER           PIC X(12) VALUE
               "YEAR-TO-DATE".
           03  FILLER           PIC X(14) VALUE SPACES.

       01  DETAIL4-HEAD3.
           03  FILLER           PIC X(16) VALUE SPACES.
           03  FILLER           PIC X(34) VALUE
               "#          $ AMOUNT      %        ".
           03  FILLER           PIC X(30) VALUE
               "  #          $ AMOUNT      %  ".

       01  DETAIL5-HEAD1.
           03  FILLER           PIC X(35) VALUE SPACES.
           03  FILLER           PIC X(9)  VALUE
               "INSURANCE".
           03  FILLER           PIC X(36) VALUE SPACES.

       01  DETAIL5-HEAD2.
           03  FILLER           PIC X(16) VALUE
               " LIFE POLICIES  ".
           03  FILLER           PIC X(16) VALUE
               "  A&H POLICIES  ".
           03  FILLER           PIC X(16) VALUE
               "PROPERTY POLICIE".
           03  FILLER           PIC X(16) VALUE
               "S  OTHER-1      ".
           03  FILLER           PIC X(16) VALUE
               "   OTHER-2      ".
           03  FILLER           PIC X(16) VALUE
               "   OTHER-3      ".

       01  DETAIL5-HEAD3.
           03  FILLER           PIC X(48) VALUE
               "# WRITTEN    %  # WRITTEN    %  # WRITTEN    %  ".
           03  FILLER           PIC X(48) VALUE
               "# WRITTEN    %  # WRITTEN    %  # WRITTEN    %  ".

      *=================================================================
      *
      *    D E T A I L - L I N E
      *
      *=================================================================
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D1-GAIN-LOSS-NO1      PIC ZZZZ,ZZ9-         BLANK ZERO.
           03  FILLER                PIC X.
           03  D1-GAIN-LOSS-AMT1     PIC ZZ,ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  FILLER                PIC X(4).
           03  D1-GAIN-LOSS-NO2      PIC ZZZZ,ZZ9-         BLANK ZERO.
           03  D1-GAIN-LOSS-PCT1     PIC ZZZZ9.9-          BLANK ZERO.
           03  D1-GAIN-LOSS-AMT2     PIC Z,ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  D1-GAIN-LOSS-PCT2     PIC ZZZZ9.9-          BLANK ZERO.
           03  FILLER                PIC X(6).

       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  D2-SOURCE             PIC X(10).
           03  D2-VOL-NO-APPS        PIC ZZZZ,ZZ9-         BLANK ZERO.
           03  D2-VOL-NO             PIC ZZZZ,ZZ9-         BLANK ZERO.
           03  FILLER                PIC X(2).
           03  D2-VOL-PCT1           PIC ZZZZ9.9-         BLANK ZERO.
           03  FILLER                PIC X.
           03  D2-VOL-AMT            PIC ZZ,ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  FILLER                PIC X(2).
           03  D2-VOL-PCT2           PIC ZZZZ9.9-         BLANK ZERO.
           03  FILLER                PIC X(2).
           03  D2-VOL-AVG-LOAN       PIC ZZZZ,ZZ9.99-      BLANK ZERO.

       01  DETAIL3 REDEFINES DETAIL-LINE.
           03  D3-BUCKET             PIC X(10).
           03  D3-DEL-NO-CUR         PIC ZZZZ,ZZ9-         BLANK ZERO.
           03  FILLER                PIC X(1).
           03  D3-DEL-AMT-CUR        PIC ZZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  FILLER                PIC X(1).
           03  D3-DEL-PCT1           PIC ZZZ9.9-         BLANK ZERO.
           03  FILLER                PIC X(2).
           03  D3-DEL-NO-MTD         PIC ZZZZ,ZZ9-         BLANK ZERO.
           03  FILLER                PIC X(1).
           03  D3-DEL-AMT-MTD        PIC ZZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  FILLER                PIC X(1).
           03  D3-DEL-PCT2           PIC ZZZ9.9-         BLANK ZERO.

       01  DETAIL4 REDEFINES DETAIL-LINE.
           03  D4-TYPE               PIC X(10).
           03  D4-PL-NO-MTD          PIC ZZZZ,ZZ9-         BLANK ZERO.
           03  FILLER                PIC X(1).
           03  D4-PL-AMT-MTD         PIC ZZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  FILLER                PIC X(1).
           03  D4-PL-PCT1            PIC ZZZ9.9-         BLANK ZERO.
           03  FILLER                PIC X(2).
           03  D4-PL-NO-YTD          PIC ZZZZ,ZZ9-         BLANK ZERO.
           03  FILLER                PIC X(1).
           03  D4-PL-AMT-YTD         PIC ZZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  FILLER                PIC X(1).
           03  D4-PL-PCT2            PIC ZZZ9.9-         BLANK ZERO.

       01  DETAIL5 REDEFINES DETAIL-LINE.
           03  D5-INS                OCCURS 6.
               05  D5-INS-NO         PIC ZZZZ,ZZ9-        BLANK ZERO.
               05  D5-INS-PCT        PIC ZZZ9.9-          BLANK ZERO.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER                PIC X(30).
           03  RANGE-SEL             PIC X(6).

      *=================================================================
      *    E X I T - P A T H
      *=================================================================
      *01  EXIT-PATH-LIST  PIC X(9)   VALUE "EXITPATH.".
      *01  EXIT-PATH       PIC X(9)   VALUE " ".

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/OPREPT_DEF.CPY".
       COPY "LIBLP/OPREPT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".


      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/OPREPT_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TY1-FILE.
      *    PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BY1-FILE.
           PERFORM CLOSE-BY1-CONNECTION.
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *     M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "OPREPT"      TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.

           IF GROUP-FLAG = "N"
              MOVE BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

       NEXT-GROUP-LOC.

           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

      * AT END OF FILE, FORCE WGR-SCURR -TABLE & TPLUS TO CHANGE FROM
      * PREVIOUS SO GRAND TOTALS WILL PRINT - MUST FORCE BECAUSE ALL
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

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO NOT = BRANCH
              PERFORM PRINT-ONE-BRANCH-TOTALS
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "REPORT IN PROGRESS................", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BY-PATH-OVERRIDE
                                              RC-PATH-OVERRIDE
                                              TY-PATH-OVERRIDE.

           PERFORM GET-FIRST-OPEN-DATE.
           PERFORM OPEN-BY1-FILE.

           MOVE CUR-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM
                                         QBY1-WBEG-DATE-CCYYMM
                                         QBY1-WEND-DATE-CCYYMM.
           IF GROUP-FLAG = "Y"
              MOVE WGR-SRESULT TO BY-BRNO
           ELSE
              MOVE BRANCH TO BY-BRNO.
           MOVE BY-BRNO              TO QBY1-WBEG-BRNO
                                        QBY1-WEND-BRNO.

           IF BEG-CLASS NUMERIC
              MOVE BEG-CLASS TO BY-CLASS
           ELSE
              MOVE 1         TO BY-CLASS
              MOVE 99        TO END-CLASS.

           MOVE BY-CLASS             TO QBY1-WBEG-CLASS.
           MOVE END-CLASS            TO QBY1-WEND-CLASS.

           PERFORM START-BY1-FILE.

       NEXT-BY1.

           PERFORM READ-BY1-FILE-NEXT.
           IF GROUP-FLAG = "Y"
              IF (IO-FG NOT = 0) OR
                  (BY-BRNO NOT = WGR-SRESULT) OR
                   (BY-DATE-CCYYMM NOT = CUR-MO-BY-DATE-CCYYMM) OR
                    ( BY-CLASS > END-CLASS)
                 GO TO NEXT-GROUP-LOC.

           IF GROUP-FLAG NOT = "Y"
              IF (IO-FG NOT = 0) OR
                  (BY-BRNO NOT = BRANCH) OR
                   (BY-DATE-CCYYMM NOT = CUR-MO-BY-DATE-CCYYMM) OR
                    (BY-CLASS > END-CLASS)
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-ROUTINE.

           IF BEG-CLASS NOT NUMERIC
              PERFORM TEST-CLASS 
                IF CORRECT-CLASS = "N"
                   GO TO NEXT-BY1.

           MOVE "N" TO FIRST-TIME.

       OK-TO-PROCESS.

           PERFORM CHECK-ADD-TOTALS.
           GO TO NEXT-BY1.

       END-ROUTINE.

           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM
           ELSE
              MOVE "Y" TO RECORDS-FOUND.

           PERFORM RANGE-LINES.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE OPREPT-QUEUE-POS TO Q-POS.
           MOVE OPREPT-COPIES-POS TO C-POS.
           MOVE OPREPT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-BR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF DEL-FLAG = "Y"
              MOVE "MONTH-END OPERATIONS REPORT" TO H-TITLE.
           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.
           MOVE STORE-DATE TO H-MMYY.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS................" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE ZEROS TO TOTAL-TABLE.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BRANCH TO SV-BRANCH
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF GROUP-FLAG = "N"
                 IF BRANCH = 0
                    MOVE BRANCH TO SV-BRANCH
                    MOVE EXT-CONAME-USER-LOC TO BRANCH.

           IF GP-NO-SC-USE-DM = "Y"
              MOVE "DM" TO SOURCE-ELE(8).

           PERFORM OPEN-PR-FILE.

           IF EXT-ROUTE-BUF = "00"
              GO TO EXIT-INITIALIZE.

           IF GROUP-FLAG = "Y"
              PERFORM OPEN-GR1-FILE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              GO TO EXIT-INITIALIZE
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y" TO TOTAL-LEVEL-TABLE(9).

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO DATE-YYYYMMDD NDTE-DATE.
           MOVE DATE-YYYYMMDD-MM TO STORE-MO.
           MOVE DATE-YYYYMMDD-YY TO STORE-YR.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO CUR-MO-BY-DATE-CCYYMM.

           IF DEL-FLAG = "N"
              GO TO EXIT-EO-EXEC-AUDIT.

      * SETUP NEXT MONTH DATE:

           MOVE 1 TO NDTE-HOLD.
           MOVE 1 TO NDTE-DD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO NEXT-MO-BY-DATE-CCYYMM.

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

           IF VDUSTATUS = "1U" GO TO ENTER-DEL-FLAG.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-GROUP.

           PERFORM GET-TODAYS-DATE.

           GO TO SETUP-BY-DATE.

       ENTER-DATE.
           MOVE "EN N040MMYY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DATE.

           MOVE VDUNUM0 TO STORE-DATE DISPLAY-DATE.

       SETUP-BY-DATE.
           MOVE STORE-MO TO DATE-MMDDYY-MM.
           MOVE 1        TO DATE-MMDDYY-DD.
           MOVE STORE-YR TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO CUR-MO-BY-DATE-CCYYMM.

       ENTER-GROUP.

           MOVE "E  A040GROUP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "40" OR "50"
                 GO TO ENTER-GROUP
              ELSE
                 GO TO ENTER-DATE.

           IF VDUSTATUS = "16"
              MOVE 0 TO BRANCH
              MOVE "N" TO GROUP-FLAG
              GO TO SKIP-PASSWD-CHECK.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO ENTER-GROUP
              ELSE
                 MOVE "S  A000GROUP." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-GROUP.

           MOVE VDUBUF TO WGR-WORD
                          ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO BRANCH
              MOVE "N" TO GROUP-FLAG
              GO TO SKIP-PASSWD-CHECK.


           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
                   AND WGR-ERR NOT = 1
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-GROUP.

           IF WGR-TYPE NOT = "A"
              MOVE WGR-SRESULT TO BRANCH
              GO TO SKIP-PASSWD-CHECK.

           PERFORM VERIFY-PASSWORD.

           IF PASSWDW-STAT NOT = "OK"
              MOVE
           "VIEWING THESE BRANCHES IS NOT ALLOWED WITHOUT PASSWORD!!!"
                                                TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-GROUP.

           MOVE WGR-WORD TO ENT-GROUP.
           MOVE "Y" TO GROUP-FLAG.
           GO TO BEGCLS.

       SKIP-PASSWD-CHECK.

           MOVE "N" TO GROUP-FLAG.
           MOVE "SNNN040GROUP." TO VDU.
           MOVE BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

       BEGCLS.

      *    MOVE "ENNN020CLASS1." TO VDU.
           MOVE "E  A020CLASS1." TO VDU.

           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLCLS.
           IF VDUSTATUS = "1U" GO TO ENTER-GROUP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGCLS.

           MOVE VDUBUF TO ENTER-CLASS-ALPHA.

           IF ENTER-CLASS-ALPHA NUMERIC
              GO TO NUMERIC-CLASS-ENTERED.

      *CLASS-WK1 IS A DIGIT AND CLASS-WK2 IS A SPACE SO IT FAILS
      *TEST ON IS IT A NUMERIC.
           IF CLASS-WK1 NUMERIC AND CLASS-WK2 = " "
              MOVE CLASS-WK1 TO CLASS-WK2
              MOVE 0 TO CLASS-WK1
              MOVE "SN N020CLASS1." TO VDU
              PERFORM SCREENIO
              GO TO NUMERIC-CLASS-ENTERED.

           IF CLASS-WK1 = "S"
              MOVE "S" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "SALES FINANCE" TO VDUBUF
              PERFORM SCREENIO.

           IF CLASS-WK1 = "L"
              MOVE "L" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "DIRECT LOANS" TO VDUBUF
              PERFORM SCREENIO.

           IF CLASS-WK1 = "A"
              MOVE "A" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "CLASS 1-4 & 22 LOANS" TO VDUBUF
              PERFORM SCREENIO.

           IF CLASS-WK1 = "W"
              MOVE "W" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "WCBC LOANS" TO VDUBUF
             PERFORM SCREENIO.

           IF CLASS-WK1 = "M"
              MOVE "M" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "SMALL LOANS" TO VDUBUF
              PERFORM SCREENIO.

           IF CLASS-WK1 = "R"
              MOVE "R" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "REAL ESTATE CLASSES" TO VDUBUF
              PERFORM SCREENIO.

           IF CLASS-WK1 = "I"
              MOVE "I" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "I.B. LOANS" TO VDUBUF
              PERFORM SCREENIO.

           IF CLASS-WK1 = "O"
              MOVE "O" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "CLASS 5 - 21 LOANS" TO VDUBUF

              PERFORM SCREENIO.

           IF CLASS-WK1 = "F"
              MOVE "F" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "WCBC & CLASS 30" TO VDUBUF
              PERFORM SCREENIO.

           IF NOT( CLASS-WK1 = "L" OR "A" OR "W" OR "M" OR "R"
                             OR "I" OR "O" OR "F" OR "S")
                        GO TO BEGCLS.

      * IF ALPHA MOVE SPACES TO END-CLASS
           MOVE "S  A020CLASS1." TO VDU
           MOVE ENTER-CLASS-ALPHA TO BEG-CLASSX VDUBUF
           PERFORM SCREENIO
           MOVE "S  A020CLASS2." TO VDU
           MOVE SPACES TO VDUBUF END-CLASSX
           PERFORM SCREENIO.

           GO TO ENTER-DEL-FLAG.

       NUMERIC-CLASS-ENTERED.
           MOVE "SNNN020CLASS1." TO VDU
           MOVE ENTER-CLASS-WK TO BEG-CLASS LC-CODE VDUNUM0
           PERFORM SCREENIO.

           IF BEG-CLASS = 0
              GO TO BEGCLS.

           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.

           IF IO-FG = 9
              MOVE "INVALID LOAN CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO BEGCLS.

       ENDCLS.

           MOVE "ENNN020CLASS2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLCLS.
           IF VDUSTATUS = "1U" GO TO BEGCLS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDCLS.

           MOVE VDUNUM0 TO END-CLASS LC-CODE.

           IF END-CLASS = 0
              GO TO ENDCLS.

           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.

            IF IO-FG = 9
              MOVE "INVALID LOAN CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO ENDCLS.

           GO TO ENTER-DEL-FLAG.

       ALLCLS.

           MOVE "SNNN020CLASS1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CLASS2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

       ENTER-DEL-FLAG.
           MOVE "E  A010DELFLAG." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENDCLS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-DEL-FLAG.

           MOVE VDUBUF TO DEL-FLAG.

           IF DEL-FLAG NOT = "Y" AND NOT = "N"
              GO TO ENTER-DEL-FLAG.

           IF DEL-FLAG = "N"
              GO TO CHECK-LEVEL-PRINTS.

      * SETUP NEXT MONTH DATE:

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO CHECK-LEVEL-PRINTS.

           MOVE STORE-MO TO DATE-MMDDYY-MM.
           MOVE 1        TO DATE-MMDDYY-DD.
           MOVE STORE-YR TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           MOVE 1             TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO NEXT-MO-BY-DATE-CCYYMM.
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

           IF GROUP-FLAG = "Y"
              MOVE "S  A040GROUP." TO VDU
              MOVE ENT-GROUP TO VDUBUF GTT-SV-ENT-GROUP
              PERFORM SCREENIO
           ELSE
              MOVE "SN N040GROUP." TO VDU
              MOVE BRANCH TO VDUNUM0
              PERFORM SCREENIO.

           IF BEG-CLASS NUMERIC
              MOVE "SNNN020CLASS1." TO VDU
              MOVE BEG-CLASS TO VDUNUM0 BEG-CLASS
              PERFORM SCREENIO
           ELSE
              MOVE "S  A020CLASS1." TO VDU
              MOVE BEG-CLASSX TO VDUBUF
              PERFORM SCREENIO.

           IF END-CLASS NUMERIC
              MOVE "SNNN020CLASS2." TO VDU
              MOVE END-CLASS TO VDUNUM0
              PERFORM SCREENIO
           ELSE
              MOVE "S  A020CLASS2." TO VDU
              MOVE END-CLASSX TO VDUBUF
              PERFORM SCREENIO.

           MOVE "S  A010DELFLAG." TO VDU.
           MOVE DEL-FLAG TO VDUBUF.
           PERFORM SCREENIO.

      **********************************************
       TEST-CLASS SECTION.
      **********************************************

           MOVE "N" TO CORRECT-CLASS.

           MOVE BY-CLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-BAD
              GO TO EXIT-TEST-CLASS.

           MOVE BEG-CLASSX TO ALL-CLASSES-FLAG.

           IF SF-CLASSES
              IF LC-SALEFIN = "Y"
                 IF LC-AFP-CLASS NOT = "Y"
                    MOVE "Y" TO CORRECT-CLASS.
      * CLASS 1, 2, 3, 4 AND 22
           IF WORLD-SMALL-CLASSES
              IF (LC-CODE < 5 OR LC-CODE = 22)
                 MOVE "Y" TO CORRECT-CLASS.
           IF LN-CLASSES
              IF LC-SALEFIN = "N" AND LC-GIFTNET NOT = "Y"
                 MOVE "Y" TO CORRECT-CLASS.
           IF SMALL-LN-CLASSES
              IF LC-SMALLOAN = "Y"
                 MOVE "Y" TO CORRECT-CLASS.
           IF NON-SMALL-LN-CLASSES
              IF LC-SMALLOAN = "N" AND LC-SALEFIN = "N"
                 MOVE "Y" TO CORRECT-CLASS.
           IF IB-LN-CLASSES
              IF LC-LOANTYPE = "I" AND LC-SALEFIN NOT = "Y"
                 MOVE "Y" TO CORRECT-CLASS.
           IF WCBC-CLASSES
              IF LC-GIFTNET = "Y"
                 MOVE "Y" TO CORRECT-CLASS.
      * CLASSES 5 - 21
           IF WORLD-OTHER-CLASSES
              IF LC-CODE > 4
                 IF LC-CODE < 22
                    MOVE "Y" TO CORRECT-CLASS.
           IF WORLD-SF-CLASSES
              IF LC-GIFTNET = "Y" OR LC-CODE = 30
                 MOVE "Y" TO CORRECT-CLASS.
           IF ALL-CLASSES
              MOVE "Y" TO CORRECT-CLASS.
           IF LC-SERVACCT = "Y"
              MOVE "N" TO CORRECT-CLASS.
       EXIT-TEST-CLASS.
           EXIT.


      ******************************************************************
      *
      *     T O T A L - L E V E L - C H E C K
      *
      *   CHECK FOR LEVEL SUMMARY TOTALS:
      *   WGR-SCURR IS TABLE OF POINTERS FOR CURRENT GROUP WHICH POINT
      *   TO WGR-SID TABLE TO SHOW CURRENT GROUP HIERARCHY:
      *       WGR-SCURR(1) POINTS TO WGR-SID OF LOW LEVEL
      *       (SELECTED GROUP);
      *       WGR-SCURR(WGR-TPLUS) POINTS TO THE WGR-SID OF THE
      *       HIGHEST LEVEL WHICH IS THE BRANCH NUMBER CURRENTLY
      *       BEING PROCESSED.
      *   THIS ROUTINE COMPARES THE WGR-SCURR TABLE TO THE TABLE SAVED
      *   FROM THE PREVIOUS GROUP, PRINTING & ZEROING TOTALS FOR EACH
      *   LEVEL WHICH HAS CHANGED STARTING WITH THE HIGHEST (BRANCH)
      *   LEVEL & WORKING THROUGH HIERARCHY.  GRAND TOTALS ARE FORCED BY
      *   MOVING 0 TO WGR-SCURR TABLE & 15 TO WGR-TPLUS.
      ******************************************************************
       TOTAL-LEVEL-CHECK SECTION.

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

       DO-GROUP-TOTALS.

           MOVE "GROUP: " TO HEAD3-TYPE.
           MOVE GR-ID TO H-GRID.
           MOVE GR-NAME TO H-GRNAME.
           PERFORM HEAD-PAGE.
           PERFORM PRINT-TOTALS.
           GO TO CONTINUE-LOOP.

       CHECK-BRANCH-SID.

           IF TOTAL-LEVEL-TABLE(9) NOT = "Y"
              GO TO CONTINUE-LOOP.

           MOVE WGR-SID(PREV-SUB) TO WGR-WORD.
           MOVE WGR-NUMB TO BR-NO.
           PERFORM READ-BR-FILE.
           MOVE 9 TO TOT-SUB.
           MOVE "BRANCH: " TO HEAD3-TYPE.
           MOVE BR-NO TO H-BRNO.
           MOVE BR-NAME TO H-GRNAME.
           PERFORM HEAD-PAGE.
           PERFORM PRINT-TOTALS.

       CONTINUE-LOOP.

           SUBTRACT 1 FROM GROUP-SUB.

           IF GROUP-SUB < WGR-SSTABLE
              GO TO EXIT-TOTAL-LEVEL-CHECK.

           GO TO LEVEL-TOTAL-LOOP.

       EXIT-TOTAL-LEVEL-CHECK.
           MOVE WGR-SCURR-TABLE TO PREV-SCURR-TABLE.

      ******************************************************************
       CHECK-ADD-TOTALS SECTION.
      ******************************************************************

           PERFORM GET-TODAYS-TOTALS.

           IF GROUP-FLAG = "N"
              MOVE 9 TO TOT-SUB
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

       CHECK-TOTAL-FLAG.

           IF TOTAL-LEVEL-TABLE(TOT-SUB) NOT = "Y"
              GO TO CONTINUE-CHECK-TOTALS.

           PERFORM ADD-TOTALS.

       CONTINUE-CHECK-TOTALS.

           SUBTRACT 1 FROM GROUP-SUB.

           IF GROUP-SUB > 0
              GO TO CHECK-TOTAL-LOOP.

       EXIT-CHECK-TOTALS.
           EXIT.

      ******************************************************************
       PRINT-ONE-BRANCH-TOTALS SECTION.
      ******************************************************************

      * PRINT FOR 1 NUMERIC BRANCH SELECTED

           IF FIRST-TIME = "Y"
              GO TO EXIT-PRINT-ONE-BRANCH.

           MOVE BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           MOVE 9          TO TOT-SUB.
           MOVE "BRANCH: " TO HEAD3-TYPE.
           MOVE BR-NO      TO H-BRNO.
           MOVE BR-NAME    TO H-GRNAME.
           PERFORM HEAD-PAGE.
           PERFORM PRINT-TOTALS.

       EXIT-PRINT-ONE-BRANCH.
           EXIT.

      ******************************************************************
       ADD-TOTALS SECTION.
      ******************************************************************
           MOVE 1 TO TYPE-SUB.

       TYPE-LOOP.

      * SECTION 1: GAIN/LOSS
           ADD BY-NO(1, 1, TYPE-SUB) TO GAIN-LOSS-NO-CUR(TOT-SUB).
           ADD BY-AMT(1, 1, TYPE-SUB) TO GAIN-LOSS-AMT-CUR(TOT-SUB).
           ADD BY-NO(2, 1, TYPE-SUB) TO GAIN-LOSS-NO-MTD(TOT-SUB).
           ADD BY-AMT(2, 1, TYPE-SUB) TO GAIN-LOSS-AMT-MTD(TOT-SUB).
           ADD BY-NO(3, 1, TYPE-SUB) TO GAIN-LOSS-NO-YTD(TOT-SUB).
           ADD BY-AMT(3, 1, TYPE-SUB) TO GAIN-LOSS-AMT-YTD(TOT-SUB).

      * SECTION 2: MTD & YTD VOLUME
           ADD BY-NO(1, 2, TYPE-SUB) TO VOL-NO(TOT-SUB, 1, TYPE-SUB).
           ADD BY-AMT(1, 2, TYPE-SUB) TO VOL-AMT(TOT-SUB, 1, TYPE-SUB).
           ADD BY-NO(2, 2, TYPE-SUB) TO VOL-NO(TOT-SUB, 2, TYPE-SUB).
           ADD BY-AMT(2, 2, TYPE-SUB) TO VOL-AMT(TOT-SUB, 2, TYPE-SUB).

      * 10TH OCCURENCE = TOTAL OF ALL TYPES
           ADD BY-NO(1, 2, TYPE-SUB) TO VOL-NO(TOT-SUB, 1, 10).
           ADD BY-AMT(1, 2, TYPE-SUB) TO VOL-AMT(TOT-SUB, 1, 10).
           ADD BY-NO(2, 2, TYPE-SUB) TO VOL-NO(TOT-SUB, 2, 10).
           ADD BY-AMT(2, 2, TYPE-SUB) TO VOL-AMT(TOT-SUB, 2, 10).

      * ACCUMULATE TODAY'S TOTALS
      * SECTION 1: GAIN/LOSS
           ADD TY-NO(1, TYPE-SUB) TO GAIN-LOSS-NO-CUR(TOT-SUB).
           ADD TY-AMT(1, TYPE-SUB) TO GAIN-LOSS-AMT-CUR(TOT-SUB).

      * SECTION 2: MTD & YTD VOLUME
           ADD TY-NO(2, TYPE-SUB) TO VOL-NO(TOT-SUB, 1, TYPE-SUB).
           ADD TY-AMT(2, TYPE-SUB) TO VOL-AMT(TOT-SUB, 1, TYPE-SUB).
           ADD TY-NO(2, TYPE-SUB) TO VOL-NO(TOT-SUB, 2, TYPE-SUB).
           ADD TY-AMT(2, TYPE-SUB) TO VOL-AMT(TOT-SUB, 2, TYPE-SUB).

      * 10TH OCCURENCE = TOTAL OF ALL TYPES
           ADD TY-NO(2, TYPE-SUB) TO VOL-NO(TOT-SUB, 1, 10).
           ADD TY-AMT(2, TYPE-SUB) TO VOL-AMT(TOT-SUB, 1, 10).
           ADD TY-NO(2, TYPE-SUB) TO VOL-NO(TOT-SUB, 2, 10).
           ADD TY-AMT(2, TYPE-SUB) TO VOL-AMT(TOT-SUB, 2, 10).

           ADD 1 TO TYPE-SUB.

           IF TYPE-SUB < 11
              GO TO TYPE-LOOP.

           PERFORM VARYING PL-SUB FROM 1 BY 2 UNTIL PL-SUB > 9
              ADD BY-NO-PL(1,2,PL-SUB) TO PL-NO-MTD(TOT-SUB)
              ADD BY-AMT-PL(1,2,PL-SUB) TO PL-AMT-MTD(TOT-SUB)
              ADD BY-NO-PL(2,2,PL-SUB) TO PL-NO-YTD(TOT-SUB)
              ADD BY-AMT-PL(2,2,PL-SUB) TO PL-AMT-YTD(TOT-SUB)
              ADD TY-AMT-PL(2,PL-SUB) TO PL-AMT-MTD(TOT-SUB)
              ADD TY-NO-PL(2,PL-SUB) TO PL-NO-MTD(TOT-SUB)
              ADD TY-AMT-PL(2,PL-SUB) TO PL-AMT-YTD(TOT-SUB)
              ADD TY-NO-PL(2,PL-SUB) TO PL-NO-YTD(TOT-SUB)
           END-PERFORM.

           SUBTRACT BY-NO-PL(1,2,3) FROM PL-NO-MTD(TOT-SUB).
           SUBTRACT BY-AMT-PL(1,2,3) FROM PL-AMT-MTD(TOT-SUB).
           SUBTRACT BY-NO-PL(2,2,3) FROM PL-NO-YTD(TOT-SUB).
           SUBTRACT BY-AMT-PL(2,2,3) FROM PL-AMT-YTD(TOT-SUB).
           SUBTRACT TY-AMT-PL(2,3) FROM PL-AMT-MTD(TOT-SUB).
           SUBTRACT TY-NO-PL(2,3) FROM PL-NO-MTD(TOT-SUB).
           SUBTRACT TY-AMT-PL(2,3) FROM PL-AMT-YTD(TOT-SUB).
           SUBTRACT TY-NO-PL(2,3) FROM PL-NO-YTD(TOT-SUB).

      * SECTION 3: DELINQUENCY RECENCY & CONTRACT

           IF DEL-FLAG = "Y"
              MOVE BY1-KEY TO HOLD-BY1-KEY
              MOVE NEXT-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM
              PERFORM READ-BY1-FILE
              IF IO-FG NOT = 0
                 GO TO RESTORE-CUR-MONTH-BY.

      * IF BEG MO DELINQUENCY, GET NEXT MO BY & USE PERIOD 2
      * (BEG MO DEL) & PERIOD 3 (ELM DEL)
      * OTHERWISE, USE 1 (CUR) VS. 2 (BEGMO)

           IF DEL-FLAG = "Y"
              MOVE 2 TO PER-SUB1
              MOVE 2 TO PER-SUB2
           ELSE
              MOVE 1 TO PER-SUB1
              MOVE 2 TO PER-SUB2.
           MOVE 1 TO AGE-IN.
           MOVE 1 TO REC-SUB.
           MOVE 2 TO DEL-SUB.

       AGE-LOOP.

      * OUTPUT AGING BUCKETS   1=30, 2=60, 3=90+120+150, 4=180+210
      *                        5=DEL SUB-TOTAL (TOTAL OF BUCKETS 1-4)
      *                        6=MISPAYS, 7=30 DAY POTS, 7=TOTAL SLOW

           IF      AGE-IN = 1 MOVE 1 TO AGE-OUT
           ELSE IF AGE-IN = 2 MOVE 2 TO AGE-OUT
           ELSE IF AGE-IN = 3 MOVE 3 TO AGE-OUT
           ELSE IF AGE-IN = 4 MOVE 3 TO AGE-OUT
           ELSE IF AGE-IN = 5 MOVE 3 TO AGE-OUT
           ELSE IF AGE-IN = 6 MOVE 4 TO AGE-OUT
           ELSE IF AGE-IN = 7 MOVE 4 TO AGE-OUT
           ELSE IF AGE-IN = 8 MOVE 6 TO AGE-OUT
           ELSE IF AGE-IN = 9 MOVE 7 TO AGE-OUT.

      * FORCE 2 IN RECENCY SUB (FOR CONTRACT) IF OCCURANCE 8 (MISPAYS):

           IF AGE-IN = 8
              MOVE 2 TO REC-SUB
           ELSE
              MOVE 1 TO REC-SUB.

      * ALWAYS ADD TO AGE-OUT BUCKET & TOTAL DEL (BUCKET 8)
      * ADD TO SLOW (BUCKET 5) IF 30 TO 210

           ADD BY-UNITS(PER-SUB1, REC-SUB, AGE-IN) TO
               DEL-NO-CUR(TOT-SUB, 1, AGE-OUT)
               DEL-NO-CUR(TOT-SUB, 1, 8).
           IF AGE-IN < 8
               ADD BY-UNITS(PER-SUB1, REC-SUB, AGE-IN) TO
               DEL-NO-CUR(TOT-SUB, 1, 5).

           ADD BY-OUTBAL(PER-SUB1, REC-SUB, AGE-IN) TO
               DEL-AMT-CUR(TOT-SUB, 1, AGE-OUT)
               DEL-AMT-CUR(TOT-SUB, 1, 8).
           IF AGE-IN < 8
               ADD BY-OUTBAL(PER-SUB1, REC-SUB, AGE-IN) TO
               DEL-AMT-CUR(TOT-SUB, 1, 5).

           ADD BY-UNITS(PER-SUB1, DEL-SUB, AGE-IN) TO
               DEL-NO-CUR(TOT-SUB, 2, AGE-OUT)
               DEL-NO-CUR(TOT-SUB, 2, 8).
           IF AGE-IN < 8
               ADD BY-UNITS(PER-SUB1, DEL-SUB, AGE-IN) TO
               DEL-NO-CUR(TOT-SUB, 2, 5).

           ADD BY-OUTBAL(PER-SUB1, DEL-SUB, AGE-IN) TO
               DEL-AMT-CUR(TOT-SUB, 2, AGE-OUT)
               DEL-AMT-CUR(TOT-SUB, 2, 8).
           IF AGE-IN < 8
               ADD BY-OUTBAL(PER-SUB1, DEL-SUB, AGE-IN) TO
               DEL-AMT-CUR(TOT-SUB, 2, 5).

           ADD BY-UNITS(PER-SUB2, REC-SUB, AGE-IN) TO
               DEL-NO-MTD(TOT-SUB, 1, AGE-OUT)
               DEL-NO-MTD(TOT-SUB, 1, 8).
           IF AGE-IN < 8
               ADD BY-UNITS(PER-SUB2, REC-SUB, AGE-IN) TO
               DEL-NO-MTD(TOT-SUB, 1, 5).

           ADD BY-OUTBAL(PER-SUB2, REC-SUB, AGE-IN) TO
               DEL-AMT-MTD(TOT-SUB, 1, AGE-OUT)
               DEL-AMT-MTD(TOT-SUB, 1, 8).
           IF AGE-IN < 8
               ADD BY-OUTBAL(PER-SUB2, REC-SUB, AGE-IN) TO
               DEL-AMT-MTD(TOT-SUB, 1, 5).

           ADD BY-UNITS(PER-SUB2, DEL-SUB, AGE-IN) TO
               DEL-NO-MTD(TOT-SUB, 2, AGE-OUT)
               DEL-NO-MTD(TOT-SUB, 2, 8).
           IF AGE-IN < 8
               ADD BY-UNITS(PER-SUB2, DEL-SUB, AGE-IN) TO
               DEL-NO-MTD(TOT-SUB, 2, 5).

           ADD BY-OUTBAL(PER-SUB2, DEL-SUB, AGE-IN) TO
               DEL-AMT-MTD(TOT-SUB, 2, AGE-OUT)
               DEL-AMT-MTD(TOT-SUB, 2, 8).
           IF AGE-IN < 8
               ADD BY-OUTBAL(PER-SUB2, DEL-SUB, AGE-IN) TO
               DEL-AMT-MTD(TOT-SUB, 2, 5).

      * IF BEG-MO DELINQUENCY, DON'T ADD TODAY'S TOTALS:
           IF DEL-FLAG = "Y"
              GO TO INCREMENT-AGE-SUB.

      * ADD TODAY' TOTALS

           ADD TY-UNITS(REC-SUB, AGE-IN) TO
               DEL-NO-CUR(TOT-SUB, 1, AGE-OUT)
               DEL-NO-CUR(TOT-SUB, 1, 8).
           IF AGE-IN < 8
               ADD TY-UNITS(REC-SUB, AGE-IN) TO
               DEL-NO-CUR(TOT-SUB, 1, 5).

           ADD TY-OUTBAL(REC-SUB, AGE-IN) TO
               DEL-AMT-CUR(TOT-SUB, 1, AGE-OUT)
               DEL-AMT-CUR(TOT-SUB, 1, 8).
           IF AGE-IN < 8
               ADD TY-OUTBAL(REC-SUB, AGE-IN) TO
               DEL-AMT-CUR(TOT-SUB, 1, 5).

           ADD TY-UNITS(DEL-SUB, AGE-IN) TO
               DEL-NO-CUR(TOT-SUB, 2, AGE-OUT)
               DEL-NO-CUR(TOT-SUB, 2, 8).
           IF AGE-IN < 8
               ADD TY-UNITS(DEL-SUB, AGE-IN) TO
               DEL-NO-CUR(TOT-SUB, 2, 5).

           ADD TY-OUTBAL(DEL-SUB, AGE-IN) TO
               DEL-AMT-CUR(TOT-SUB, 2, AGE-OUT)
               DEL-AMT-CUR(TOT-SUB, 2, 8).
           IF AGE-IN < 8
               ADD TY-OUTBAL(DEL-SUB, AGE-IN) TO
               DEL-AMT-CUR(TOT-SUB, 2, 5).

       INCREMENT-AGE-SUB.
           ADD 1 TO AGE-IN.

           IF AGE-IN < 10
              GO TO AGE-LOOP.

       RESTORE-CUR-MONTH-BY.
           IF DEL-FLAG = "Y"
              MOVE HOLD-BY1-KEY TO BY1-KEY
              MOVE BY-DATE-CCYYMM       TO QBY1-WBEG-DATE-CCYYMM
                                           QBY1-WEND-DATE-CCYYMM
              MOVE BY-BRNO              TO QBY1-WBEG-BRNO
                                           QBY1-WEND-BRNO
              MOVE BY-CLASS             TO QBY1-WBEG-CLASS
              MOVE END-CLASS            TO QBY1-WEND-CLASS
              PERFORM START-BY1-FILE
              PERFORM READ-BY1-FILE-NEXT.

      * SECTION 5: INSURANCE

            ADD BY-INS-NO-WROTE(1,1) TO INS-NO-MTD(TOT-SUB, 1).
            ADD BY-INS-NO-WROTE(1,2) TO INS-NO-MTD(TOT-SUB, 2).
            ADD BY-INS-NO-WROTE(1,3) TO INS-NO-MTD(TOT-SUB, 3).
            ADD BY-INS-NO-WROTE(1,4) TO INS-NO-MTD(TOT-SUB, 4).
            ADD BY-INS-NO-WROTE(1,5) TO INS-NO-MTD(TOT-SUB, 5).
            ADD BY-INS-NO-WROTE(1,6) TO INS-NO-MTD(TOT-SUB, 6).

      * TODAY'S TOTALS
            ADD TY-INS-NO-WROTE(1) TO INS-NO-MTD(TOT-SUB, 1).
            ADD TY-INS-NO-WROTE(2) TO INS-NO-MTD(TOT-SUB, 2).
            ADD TY-INS-NO-WROTE(3) TO INS-NO-MTD(TOT-SUB, 3).
            ADD TY-INS-NO-WROTE(4) TO INS-NO-MTD(TOT-SUB, 4).
            ADD TY-INS-NO-WROTE(5) TO INS-NO-MTD(TOT-SUB, 5).
            ADD TY-INS-NO-WROTE(6) TO INS-NO-MTD(TOT-SUB, 6).

       EXIT-ADD-TOTALS.
           EXIT.

      ******************************************************************
       PRINT-TOTALS SECTION.
      ******************************************************************

           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           MOVE DETAIL1-HEAD1 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DETAIL1-HEAD2 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DETAIL1-HEAD3 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE GAIN-LOSS-NO-CUR(TOT-SUB) TO D1-GAIN-LOSS-NO1.
           MOVE GAIN-LOSS-AMT-CUR(TOT-SUB) TO D1-GAIN-LOSS-AMT1.
           COMPUTE NO-WORK = GAIN-LOSS-NO-CUR(TOT-SUB)
                           - GAIN-LOSS-NO-MTD(TOT-SUB).
           MOVE NO-WORK TO D1-GAIN-LOSS-NO2.

           IF GAIN-LOSS-NO-CUR(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                   NO-WORK / GAIN-LOSS-NO-CUR(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.

           MOVE PCT-WORK TO D1-GAIN-LOSS-PCT1.
           COMPUTE AMT-WORK = GAIN-LOSS-AMT-CUR(TOT-SUB)
                            - GAIN-LOSS-AMT-MTD(TOT-SUB).
           MOVE AMT-WORK TO D1-GAIN-LOSS-AMT2.

           IF GAIN-LOSS-AMT-CUR(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                   AMT-WORK / GAIN-LOSS-AMT-CUR(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.

           MOVE PCT-WORK TO D1-GAIN-LOSS-PCT2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "AVG BALANCE" TO DETAIL-LINE.

           IF GAIN-LOSS-NO-CUR(TOT-SUB) NOT = 0
              COMPUTE AVG-WORK ROUNDED =
                GAIN-LOSS-AMT-CUR(TOT-SUB) / GAIN-LOSS-NO-CUR(TOT-SUB)
           ELSE
              MOVE 0 TO AVG-WORK.

           MOVE AVG-WORK TO D1-GAIN-LOSS-AMT1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DETAIL1-HEAD4 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DETAIL1-HEAD5 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           COMPUTE NO-WORK = GAIN-LOSS-NO-CUR(TOT-SUB)
                           - GAIN-LOSS-NO-YTD(TOT-SUB).

           MOVE NO-WORK TO D1-GAIN-LOSS-NO2.

           IF GAIN-LOSS-NO-CUR(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                   NO-WORK / GAIN-LOSS-NO-CUR(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.

           MOVE PCT-WORK TO D1-GAIN-LOSS-PCT1.

           COMPUTE AMT-WORK = GAIN-LOSS-AMT-CUR(TOT-SUB)
                            - GAIN-LOSS-AMT-YTD(TOT-SUB).

           MOVE AMT-WORK TO D1-GAIN-LOSS-AMT2.

           IF GAIN-LOSS-AMT-CUR(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                   AMT-WORK / GAIN-LOSS-AMT-CUR(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.

           MOVE PCT-WORK TO D1-GAIN-LOSS-PCT2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO PER-SUB.

       PRINT-DETAIL2.

           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO LN-FEED.
           MOVE DETAIL2-HEAD1 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           IF PER-SUB = 1
              MOVE DETAIL2-HEAD2 TO DETAIL-LINE
           ELSE
              MOVE DETAIL2-HEAD2-YTD TO DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

           MOVE DETAIL2-HEAD3 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO TYPE-SUB.

       DETAIL2-TYPE-LOOP.

           MOVE VOL-NO-APPS(TOT-SUB, PER-SUB, TYPE-SUB) TO
                            D2-VOL-NO-APPS.

           MOVE VOL-NO(TOT-SUB, PER-SUB, TYPE-SUB) TO D2-VOL-NO.

           IF GAIN-LOSS-NO-CUR(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 VOL-NO(TOT-SUB, PER-SUB, TYPE-SUB) /
                 GAIN-LOSS-NO-CUR(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.

           MOVE PCT-WORK TO D2-VOL-PCT1.
           MOVE VOL-AMT(TOT-SUB, PER-SUB, TYPE-SUB) TO D2-VOL-AMT.

           IF GAIN-LOSS-AMT-CUR(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 VOL-AMT(TOT-SUB, PER-SUB, TYPE-SUB) /
                 GAIN-LOSS-AMT-CUR(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.

           MOVE PCT-WORK TO D2-VOL-PCT2.

           IF VOL-NO(TOT-SUB, PER-SUB, TYPE-SUB) NOT = 0
              COMPUTE AVG-WORK ROUNDED =
                 VOL-AMT(TOT-SUB, PER-SUB, TYPE-SUB) /
                 VOL-NO(TOT-SUB, PER-SUB, TYPE-SUB)
           ELSE
              MOVE 0 TO AVG-WORK.

           MOVE AVG-WORK TO D2-VOL-AVG-LOAN.
           MOVE SOURCE-ELE(TYPE-SUB) TO D2-SOURCE.

           IF TYPE-SUB = 10
              MOVE 2 TO LN-FEED.

           PERFORM WRITE-DETAIL-LINE.

           ADD 1 TO TYPE-SUB.

           IF TYPE-SUB < 11
              GO TO DETAIL2-TYPE-LOOP.

           IF PER-SUB = 1
              ADD 1 TO PER-SUB
              GO TO PRINT-DETAIL2.

           MOVE 1 TO REC-SUB.

       PRINT-DETAIL3.

           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.

           IF REC-SUB = 1
              MOVE DETAIL3-HEAD1 TO DETAIL-LINE
           ELSE
              MOVE DETAIL3-HEAD1-CONTRACT TO DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

           IF REC-SUB = 1
              MOVE DETAIL3-HEAD2 TO DETAIL-LINE
           ELSE
              MOVE DETAIL3-HEAD2-CONTRACT TO DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

           MOVE DETAIL3-HEAD3 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO AGE-SUB.

       DETAIL3-AGE-LOOP.

           MOVE BUCKET-ELE(AGE-SUB) TO D3-BUCKET.
           MOVE DEL-NO-CUR(TOT-SUB, REC-SUB, AGE-SUB) TO
              D3-DEL-NO-CUR.

           MOVE DEL-AMT-CUR(TOT-SUB, REC-SUB, AGE-SUB) TO
              D3-DEL-AMT-CUR.

           IF GAIN-LOSS-AMT-CUR(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                   DEL-AMT-CUR(TOT-SUB, REC-SUB, AGE-SUB) /
                   GAIN-LOSS-AMT-CUR(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.

           MOVE PCT-WORK TO D3-DEL-PCT1.

           COMPUTE NO-WORK = DEL-NO-CUR(TOT-SUB, REC-SUB, AGE-SUB) -
                             DEL-NO-MTD(TOT-SUB, REC-SUB, AGE-SUB).

           MOVE NO-WORK TO D3-DEL-NO-MTD.

           COMPUTE AMT-WORK = DEL-AMT-CUR(TOT-SUB, REC-SUB, AGE-SUB) -
                             DEL-AMT-MTD(TOT-SUB, REC-SUB, AGE-SUB).

           MOVE AMT-WORK TO D3-DEL-AMT-MTD.

           IF DEL-AMT-MTD(TOT-SUB, REC-SUB, AGE-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                AMT-WORK / DEL-AMT-MTD(TOT-SUB, REC-SUB, AGE-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.

           MOVE PCT-WORK TO D3-DEL-PCT2.

           IF AGE-SUB = 8
              MOVE 2 TO LN-FEED.

           PERFORM WRITE-DETAIL-LINE.

       CONTINUE-AGE-LOOP.

           ADD 1 TO AGE-SUB.

           IF AGE-SUB < 9
              GO TO DETAIL3-AGE-LOOP.

           IF REC-SUB = 2
              GO TO PRINT-DETAIL4.

           MOVE 2 TO LN-FEED.
           MOVE "REMAIN INC" TO D3-BUCKET.

      * INCREASE REMAIN = MTD P&L + CUR60 + CUR90 + CUR 180+
      *                                   - BEG 90 - BEG 180+

           COMPUTE NO-WORK ROUNDED =
                     PL-NO-MTD(TOT-SUB)
                      + DEL-NO-CUR(TOT-SUB, 1, 2)
                       + DEL-NO-CUR(TOT-SUB, 1, 3)
                       + DEL-NO-CUR(TOT-SUB, 1, 4)
                        - DEL-NO-MTD(TOT-SUB, 1, 3)
                        - DEL-NO-MTD(TOT-SUB, 1, 4).

           MOVE NO-WORK TO D3-DEL-NO-CUR.

           COMPUTE AMT-WORK ROUNDED =
                     PL-AMT-MTD(TOT-SUB)
                      + DEL-AMT-CUR(TOT-SUB, 1, 2)
                       + DEL-AMT-CUR(TOT-SUB, 1, 3)
                       + DEL-AMT-CUR(TOT-SUB, 1, 4)
                        - DEL-AMT-MTD(TOT-SUB, 1, 3)
                        - DEL-AMT-MTD(TOT-SUB, 1, 4).

           MOVE AMT-WORK TO D3-DEL-AMT-CUR.
           PERFORM WRITE-DETAIL-LINE.

           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           ADD 1 TO REC-SUB.
           PERFORM HEAD-PAGE.
           GO TO PRINT-DETAIL3.

       PRINT-DETAIL4.

           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           MOVE DETAIL4-HEAD1 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DETAIL4-HEAD2 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DETAIL4-HEAD3 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PL" TO D4-TYPE.
           MOVE PL-NO-MTD(TOT-SUB) TO D4-PL-NO-MTD.
           MOVE PL-AMT-MTD(TOT-SUB) TO D4-PL-AMT-MTD.

           IF GAIN-LOSS-AMT-CUR(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                   PL-AMT-MTD(TOT-SUB) /
                   GAIN-LOSS-AMT-CUR(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.

           MOVE PCT-WORK TO D4-PL-PCT1.
           MOVE PL-NO-YTD(TOT-SUB) TO D4-PL-NO-YTD.
           MOVE PL-AMT-YTD(TOT-SUB) TO D4-PL-AMT-YTD.

           IF GAIN-LOSS-AMT-CUR(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                   PL-AMT-YTD(TOT-SUB) /
                   GAIN-LOSS-AMT-CUR(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.

           MOVE PCT-WORK TO D4-PL-PCT2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-DETAIL5.

           MOVE 2 TO LN-FEED.
           MOVE DETAIL5-HEAD1 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DETAIL5-HEAD2 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DETAIL5-HEAD3 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           MOVE 1 TO INS-SUB.

       PRINT-INS-LOOP.

           MOVE INS-NO-MTD(TOT-SUB, INS-SUB) TO D5-INS-NO(INS-SUB).

           IF GAIN-LOSS-NO-CUR(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                   INS-NO-MTD(TOT-SUB, INS-SUB) /
                   GAIN-LOSS-NO-CUR(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.

           MOVE PCT-WORK TO D5-INS-PCT(INS-SUB).
           ADD 1 TO INS-SUB.
           IF INS-SUB < 7 GO TO PRINT-INS-LOOP.
           PERFORM WRITE-DETAIL-LINE.

           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

       ZERO-TOTALS.
           MOVE ZEROS TO TOTAL-TAB(TOT-SUB).

       EXIT-PRINT-TOTALS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-CONDITIONAL.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

       WRITE-DETAIL-EXIT.
           EXIT.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.

           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-CONDITIONAL.

       END-HEAD-PAGE.
           MOVE 3 TO LN-COUNT.
           MOVE 1 TO LN-FEED.

      ******************************************************************
       RANGE-LINES SECTION.
      ******************************************************************
           MOVE 99 TO LN-FEED.

           MOVE "GROUP " TO RANGE-LINE.
           MOVE ENT-GROUP TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       WRITE-PR-CONDITIONAL SECTION.
      ******************************************************************
           PERFORM WRITE-PR-REC.

       WRITE-PR-COND-EXIT.
           EXIT.

      *************************************
       GET-FIRST-OPEN-DATE SECTION.
      *************************************
           MOVE 0 TO FIRST-OPEN-DATE-YYYYMMDD.

      * FIND THE FIRST DAY OPEN
           PERFORM OPEN-RC2-FILE.

           MOVE "A1"     TO RC-STATUS
                            QRC2-WBEG-STATUS
                            QRC2-WEND-STATUS.
           MOVE BR-NO    TO RC-BRNO
                            QRC2-WBEG-BRNO
                            QRC2-WEND-BRNO.
           MOVE 19000101 TO RC-TRANS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WBEG-TRANS-DATE.
           MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WEND-TRANS-DATE.

           PERFORM START-RC2-FILE. *> EMBEDDED - RCFILE
           PERFORM READ-RC2-FILE-NEXT. *> EMBEDDED - RCFILE
           IF IO-FG = 0
             IF (RC-BRNO = BR-NO)
                IF (RC-STATUS = "A1")
                  MOVE RC-TRANS-DATE TO FIRST-OPEN-DATE-YYYYMMDD.

           PERFORM CLOSE-RC2-FILE.
       EXIT-GET-FIRST-OPEN.
           EXIT.

      ******************************************************************
       GET-TODAYS-TOTALS SECTION.
      ******************************************************************

           IF FIRST-OPEN-DATE-YYYYMMDD = 0
              PERFORM CLEAR-TY-FILE
              GO TO GET-TODAYS-TOTALS-EXIT.

           MOVE BY-BRNO                  TO TY-BRNO.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           IF NOT (STORE-MO = ALP-MO AND STORE-YR = ALP-YR)
              PERFORM CLEAR-TY-FILE
              GO TO GET-TODAYS-TOTALS-EXIT.

           MOVE FIRST-OPEN-DATE-YYYYMMDD TO TY-DATE.
           MOVE BY-CLASS                 TO TY-CLASS.

           PERFORM OPEN-TY1-FILE.
           PERFORM READ-TY1-FILE. *> EMBEDDED - TYFILE
           IF IO-FG = 9 OR TY-BRFG = "Y"
              PERFORM CLEAR-TY-FILE.

           PERFORM CLOSE-TY1-FILE.

       GET-TODAYS-TOTALS-EXIT.
           EXIT.

      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "BYINQ" TO PASSWDW-PGM.
           MOVE WGR-LEVEL TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "TO REPORT ON THESE BRANCHES:" TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ********************************************
       FORM-RESET SECTION.
      ********************************************
           MOVE EXT-CONAME-CONAME TO OPREPT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO OPREPT-SYSDATE.
           DISPLAY OPREPT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE OPREPT-FORM-FIELDS TO WR-BUF.
           MOVE OPREPT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ********************************************
       SETUP-LINK-INFO SECTION.
      ********************************************
           MOVE OPREPT-HELP--FILE TO HELP-LINK-FILE.
           MOVE OPREPT-HELP--DIR  TO HELP-LINK-DIR.

      ********************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/OPREPT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GPENV.CPY".

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETPR.CPY".
       GET-TODAYS-DATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           MOVE ALP-MO TO STORE-MO DISPLAY-MO.
           MOVE ALP-YR TO STORE-YR DISPLAY-YR.
           MOVE DISPLAY-DATE TO WR-BUF.
           MOVE DATE-LIST    TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBYGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".


      * COPY "LIBLP/LPBY1RN.CPY".
      * COPYMEMBER: LIBLP/LPBY1RN
      ******************************************************************
      *
      * DESCRIPTION: BRANCH STATISTICS FILE - IO ROUTINES - A THRU B
      *
      * FILE KEY   : #1 = DATE-CCYYMM, BRNO, CLASS
      *
      * FILE PATH  : /USR/DATA/R#/LP/BYAFILE
      *              /USR/DATA/R#/LP/BYBFILE
      *
      * ATTENTION  : BY-PATH-OVERRIDE CONTROLS ALL FILES - A THRU B
      *              ^^^^^^^^^^^^^^^^
      *              THERE IS NO BYA-PATH-OVERRIDE OR BYB-PATH-OVERRIDE
      *
      *=================================================================
      * REV:
      * KEC 2019-0205 ***NEW*** TO A32; MODIFIED FROM A30
      * KEC 2021-0524 ADDED ACUSQL VERSION
      ******************************************************************
      *-----------------------------------------------------------------
       SET-ALL-TO-BY1.
           MOVE BYA1-KEY   TO BY1-KEY.
           MOVE BYA-AREA-A TO BY-AREA-A.
           MOVE BYB-AREA-B TO BY-AREA-B.

      *-----------------------------------------------------------------
       SET-BY1-TO-ALL.
           MOVE BY1-KEY    TO BYA1-KEY BYB1-KEY.
           MOVE BY-AREA-A  TO BYA-AREA-A.
           MOVE BY-AREA-B  TO BYB-AREA-B.

      *-----------------------------------------------------------------
       LOAD-BY1-FILE.
           PERFORM LOAD-BYA1-FILE.
 
      *-----------------------------------------------------------------
       OPEN-BY1-FILE.
 
           PERFORM OPEN-BYA1-FILE.
           IF ( IO-FG  = 0 )
              MOVE " " TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       START-BY1-FILE.
 
           PERFORM SET-BY1-TO-ALL.
           PERFORM START-BYA1-FILE. *> SSP - BYA,BYB,LC
           IF ( IO-FG  = 0 )
              MOVE " " TO IO-MULTI-FG 
           ELSE                     
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       READ-BY1-FILE.

           PERFORM SET-BY1-TO-ALL.
           PERFORM READ-BYA1-FILE. *> EMBEDDED - BYA,BYB,LC
           IF ( IO-FG  = 0 )
              PERFORM SET-ALL-TO-BY1
           ELSE 
              MOVE "A" TO IO-MULTI-FG.
          
      *-----------------------------------------------------------------
       READ-BY1-FILE-NEXT.
     
           PERFORM SET-BY1-TO-ALL.
           PERFORM READ-BYA1-FILE-NEXT.
           IF ( IO-FG = 0 )
              PERFORM SET-ALL-TO-BY1
           ELSE
              MOVE "A" TO IO-MULTI-FG.

             
      *-----------------------------------------------------------------
       CLOSE-BY1-FILE.

           PERFORM CLOSE-BYA1-FILE.

      *=================================================================
       LOAD-BYA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BY-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BYA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BYA-ALL-MACHINE
              MOVE BYA-ALL-PATH            TO BYA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BYA-ALL-BASE
              MOVE FILPATH-MACHINE         TO BYA-ALL-MACHINE
              MOVE BYA-ALL-PATH            TO BYA-PATH.

      *-----------------------------------------------------------------
       OPEN-BYA1-FILE.
           PERFORM LOAD-BYA1-FILE.
           PERFORM OPEN-IT.
           MOVE BYA-PATH-SQL TO E-FILE.

           IF BY1-CONN-OPEN-SW = "N"
              MOVE "Y" TO BY1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BY1_CONNECT
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.
		   
      *-----------------------------------------------------------------
       START-BYA1-FILE. *> SSP - BYA,BYB,LC
           PERFORM START-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.

           IF ( BYA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BYA1-FILE.

           IF ( QBY1-WEND-DATE-CCYYMM = ZEROES ) OR
              ( QBY1-WEND-DATE-CCYYMM NOT NUMERIC )
              MOVE BY-DATE-CCYYMM       TO QBY1-WBEG-DATE-CCYYMM
              MOVE ALL "9"              TO QBY1-WEND-DATE-CCYYMM 
              MOVE BY-BRNO              TO QBY1-WBEG-BRNO
              MOVE ALL "9"              TO QBY1-WEND-BRNO
              MOVE BY-CLASS             TO QBY1-WBEG-CLASS
              MOVE ALL "9"              TO QBY1-WEND-CLASS.

      * STORED PROCEDURE SELECT STATEMENT: 
      *
      *   SELECT BYAFILE.BYA_DATE_CCYYMM,
      *          BYAFILE.BYA_BRNO,
      *          BYAFILE.BYA_CLASS,
      *          BYAFILE.BYA_NO_1_1_1,
      *          BYAFILE.BYA_NO_1_1_2,
      *          BYAFILE.BYA_NO_1_1_3,
      *          BYAFILE.BYA_NO_1_1_4,
      *          BYAFILE.BYA_NO_1_1_5,
      *          BYAFILE.BYA_NO_1_1_6,
      *          BYAFILE.BYA_NO_1_1_7,
      *          BYAFILE.BYA_NO_1_1_8,
      *          BYAFILE.BYA_NO_1_1_9,
      *          BYAFILE.BYA_NO_1_1_10,
      *          BYAFILE.BYA_NO_1_1_11,
      *          BYAFILE.BYA_NO_1_2_1,
      *          BYAFILE.BYA_NO_1_2_2,
      *          BYAFILE.BYA_NO_1_2_3,
      *          BYAFILE.BYA_NO_1_2_4,
      *          BYAFILE.BYA_NO_1_2_5,
      *          BYAFILE.BYA_NO_1_2_6,
      *          BYAFILE.BYA_NO_1_2_7,
      *          BYAFILE.BYA_NO_1_2_8,
      *          BYAFILE.BYA_NO_1_2_9,
      *          BYAFILE.BYA_NO_1_2_10,
      *          BYAFILE.BYA_NO_1_2_11,
      *          BYAFILE.BYA_NO_2_1_1,
      *          BYAFILE.BYA_NO_2_1_2,
      *          BYAFILE.BYA_NO_2_1_3,
      *          BYAFILE.BYA_NO_2_1_4,
      *          BYAFILE.BYA_NO_2_1_5,
      *          BYAFILE.BYA_NO_2_1_6,
      *          BYAFILE.BYA_NO_2_1_7,
      *          BYAFILE.BYA_NO_2_1_8,
      *          BYAFILE.BYA_NO_2_1_9,
      *          BYAFILE.BYA_NO_2_1_10,
      *          BYAFILE.BYA_NO_2_1_11,
      *          BYAFILE.BYA_NO_2_2_1,
      *          BYAFILE.BYA_NO_2_2_2,
      *          BYAFILE.BYA_NO_2_2_3,
      *          BYAFILE.BYA_NO_2_2_4,
      *          BYAFILE.BYA_NO_2_2_5,
      *          BYAFILE.BYA_NO_2_2_6,
      *          BYAFILE.BYA_NO_2_2_7,
      *          BYAFILE.BYA_NO_2_2_8,
      *          BYAFILE.BYA_NO_2_2_9,
      *          BYAFILE.BYA_NO_2_2_10,
      *          BYAFILE.BYA_NO_2_2_11,
      *          BYAFILE.BYA_NO_3_1_1,
      *          BYAFILE.BYA_NO_3_1_2,
      *          BYAFILE.BYA_NO_3_1_3,
      *          BYAFILE.BYA_NO_3_1_4,
      *          BYAFILE.BYA_NO_3_1_5,
      *          BYAFILE.BYA_NO_3_1_6,
      *          BYAFILE.BYA_NO_3_1_7,
      *          BYAFILE.BYA_NO_3_1_8,
      *          BYAFILE.BYA_NO_3_1_9,
      *          BYAFILE.BYA_NO_3_1_10,
      *          BYAFILE.BYA_NO_3_1_11,
      *          BYAFILE.BYA_NO_3_2_1,
      *          BYAFILE.BYA_NO_3_2_2,
      *          BYAFILE.BYA_NO_3_2_3,
      *          BYAFILE.BYA_NO_3_2_4,
      *          BYAFILE.BYA_NO_3_2_5,
      *          BYAFILE.BYA_NO_3_2_6,
      *          BYAFILE.BYA_NO_3_2_7,
      *          BYAFILE.BYA_NO_3_2_8,
      *          BYAFILE.BYA_NO_3_2_9,
      *          BYAFILE.BYA_NO_3_2_10,
      *          BYAFILE.BYA_NO_3_2_11,
      *          BYAFILE.BYA_AMT_1_1_1,
      *          BYAFILE.BYA_AMT_1_1_2,
      *          BYAFILE.BYA_AMT_1_1_3,
      *          BYAFILE.BYA_AMT_1_1_4,
      *          BYAFILE.BYA_AMT_1_1_5,
      *          BYAFILE.BYA_AMT_1_1_6,
      *          BYAFILE.BYA_AMT_1_1_7,
      *          BYAFILE.BYA_AMT_1_1_8,
      *          BYAFILE.BYA_AMT_1_1_9,
      *          BYAFILE.BYA_AMT_1_1_10,
      *          BYAFILE.BYA_AMT_1_1_11,
      *          BYAFILE.BYA_AMT_1_2_1,
      *          BYAFILE.BYA_AMT_1_2_2,
      *          BYAFILE.BYA_AMT_1_2_3,
      *          BYAFILE.BYA_AMT_1_2_4,
      *          BYAFILE.BYA_AMT_1_2_5,
      *          BYAFILE.BYA_AMT_1_2_6,
      *          BYAFILE.BYA_AMT_1_2_7,
      *          BYAFILE.BYA_AMT_1_2_8,
      *          BYAFILE.BYA_AMT_1_2_9,
      *          BYAFILE.BYA_AMT_1_2_10,
      *          BYAFILE.BYA_AMT_1_2_11,
      *          BYAFILE.BYA_AMT_2_1_1,
      *          BYAFILE.BYA_AMT_2_1_2,
      *          BYAFILE.BYA_AMT_2_1_3,
      *          BYAFILE.BYA_AMT_2_1_4,
      *          BYAFILE.BYA_AMT_2_1_5,
      *          BYAFILE.BYA_AMT_2_1_6,
      *          BYAFILE.BYA_AMT_2_1_7,
      *          BYAFILE.BYA_AMT_2_1_8,
      *          BYAFILE.BYA_AMT_2_1_9,
      *          BYAFILE.BYA_AMT_2_1_10,
      *          BYAFILE.BYA_AMT_2_1_11,
      *          BYAFILE.BYA_AMT_2_2_1,
      *          BYAFILE.BYA_AMT_2_2_2,
      *          BYAFILE.BYA_AMT_2_2_3,
      *          BYAFILE.BYA_AMT_2_2_4,
      *          BYAFILE.BYA_AMT_2_2_5,
      *          BYAFILE.BYA_AMT_2_2_6,
      *          BYAFILE.BYA_AMT_2_2_7,
      *          BYAFILE.BYA_AMT_2_2_8,
      *          BYAFILE.BYA_AMT_2_2_9,
      *          BYAFILE.BYA_AMT_2_2_10,
      *          BYAFILE.BYA_AMT_2_2_11,
      *          BYAFILE.BYA_AMT_3_1_1,
      *          BYAFILE.BYA_AMT_3_1_2,
      *          BYAFILE.BYA_AMT_3_1_3,
      *          BYAFILE.BYA_AMT_3_1_4,
      *          BYAFILE.BYA_AMT_3_1_5,
      *          BYAFILE.BYA_AMT_3_1_6,
      *          BYAFILE.BYA_AMT_3_1_7,
      *          BYAFILE.BYA_AMT_3_1_8,
      *          BYAFILE.BYA_AMT_3_1_9,
      *          BYAFILE.BYA_AMT_3_1_10,
      *          BYAFILE.BYA_AMT_3_1_11,
      *          BYAFILE.BYA_AMT_3_2_1,
      *          BYAFILE.BYA_AMT_3_2_2,
      *          BYAFILE.BYA_AMT_3_2_3,
      *          BYAFILE.BYA_AMT_3_2_4,
      *          BYAFILE.BYA_AMT_3_2_5,
      *          BYAFILE.BYA_AMT_3_2_6,
      *          BYAFILE.BYA_AMT_3_2_7,
      *          BYAFILE.BYA_AMT_3_2_8,
      *          BYAFILE.BYA_AMT_3_2_9,
      *          BYAFILE.BYA_AMT_3_2_10,
      *          BYAFILE.BYA_AMT_3_2_11,
      *          BYAFILE.BYA_NO_PL_1_1_1,
      *          BYAFILE.BYA_NO_PL_1_1_2,
      *          BYAFILE.BYA_NO_PL_1_1_3,
      *          BYAFILE.BYA_NO_PL_1_1_4,
      *          BYAFILE.BYA_NO_PL_1_1_5,
      *          BYAFILE.BYA_NO_PL_1_1_6,
      *          BYAFILE.BYA_NO_PL_1_1_7,
      *          BYAFILE.BYA_NO_PL_1_1_8,
      *          BYAFILE.BYA_NO_PL_1_1_9,
      *          BYAFILE.BYA_NO_PL_1_1_10,
      *          BYAFILE.BYA_NO_PL_1_2_1,
      *          BYAFILE.BYA_NO_PL_1_2_2,
      *          BYAFILE.BYA_NO_PL_1_2_3,
      *          BYAFILE.BYA_NO_PL_1_2_4,
      *          BYAFILE.BYA_NO_PL_1_2_5,
      *          BYAFILE.BYA_NO_PL_1_2_6,
      *          BYAFILE.BYA_NO_PL_1_2_7,
      *          BYAFILE.BYA_NO_PL_1_2_8,
      *          BYAFILE.BYA_NO_PL_1_2_9,
      *          BYAFILE.BYA_NO_PL_1_2_10,
      *          BYAFILE.BYA_NO_PL_2_1_1,
      *          BYAFILE.BYA_NO_PL_2_1_2,
      *          BYAFILE.BYA_NO_PL_2_1_3,
      *          BYAFILE.BYA_NO_PL_2_1_4,
      *          BYAFILE.BYA_NO_PL_2_1_5,
      *          BYAFILE.BYA_NO_PL_2_1_6,
      *          BYAFILE.BYA_NO_PL_2_1_7,
      *          BYAFILE.BYA_NO_PL_2_1_8,
      *          BYAFILE.BYA_NO_PL_2_1_9,
      *          BYAFILE.BYA_NO_PL_2_1_10,
      *          BYAFILE.BYA_NO_PL_2_2_1,
      *          BYAFILE.BYA_NO_PL_2_2_2,
      *          BYAFILE.BYA_NO_PL_2_2_3,
      *          BYAFILE.BYA_NO_PL_2_2_4,
      *          BYAFILE.BYA_NO_PL_2_2_5,
      *          BYAFILE.BYA_NO_PL_2_2_6,
      *          BYAFILE.BYA_NO_PL_2_2_7,
      *          BYAFILE.BYA_NO_PL_2_2_8,
      *          BYAFILE.BYA_NO_PL_2_2_9,
      *          BYAFILE.BYA_NO_PL_2_2_10,
      *          BYAFILE.BYA_NO_PL_3_1_1,
      *          BYAFILE.BYA_NO_PL_3_1_2,
      *          BYAFILE.BYA_NO_PL_3_1_3,
      *          BYAFILE.BYA_NO_PL_3_1_4,
      *          BYAFILE.BYA_NO_PL_3_1_5,
      *          BYAFILE.BYA_NO_PL_3_1_6,
      *          BYAFILE.BYA_NO_PL_3_1_7,
      *          BYAFILE.BYA_NO_PL_3_1_8,
      *          BYAFILE.BYA_NO_PL_3_1_9,
      *          BYAFILE.BYA_NO_PL_3_1_10,
      *          BYAFILE.BYA_NO_PL_3_2_1,
      *          BYAFILE.BYA_NO_PL_3_2_2,
      *          BYAFILE.BYA_NO_PL_3_2_3,
      *          BYAFILE.BYA_NO_PL_3_2_4,
      *          BYAFILE.BYA_NO_PL_3_2_5,
      *          BYAFILE.BYA_NO_PL_3_2_6,
      *          BYAFILE.BYA_NO_PL_3_2_7,
      *          BYAFILE.BYA_NO_PL_3_2_8,
      *          BYAFILE.BYA_NO_PL_3_2_9,
      *          BYAFILE.BYA_NO_PL_3_2_10,
      *          BYAFILE.BYA_AMT_PL_1_1_1,
      *          BYAFILE.BYA_AMT_PL_1_1_2,
      *          BYAFILE.BYA_AMT_PL_1_1_3,
      *          BYAFILE.BYA_AMT_PL_1_1_4,
      *          BYAFILE.BYA_AMT_PL_1_1_5,
      *          BYAFILE.BYA_AMT_PL_1_1_6,
      *          BYAFILE.BYA_AMT_PL_1_1_7,
      *          BYAFILE.BYA_AMT_PL_1_1_8,
      *          BYAFILE.BYA_AMT_PL_1_1_9,
      *          BYAFILE.BYA_AMT_PL_1_1_10,
      *          BYAFILE.BYA_AMT_PL_1_2_1,
      *          BYAFILE.BYA_AMT_PL_1_2_2,
      *          BYAFILE.BYA_AMT_PL_1_2_3,
      *          BYAFILE.BYA_AMT_PL_1_2_4,
      *          BYAFILE.BYA_AMT_PL_1_2_5,
      *          BYAFILE.BYA_AMT_PL_1_2_6,
      *          BYAFILE.BYA_AMT_PL_1_2_7,
      *          BYAFILE.BYA_AMT_PL_1_2_8,
      *          BYAFILE.BYA_AMT_PL_1_2_9,
      *          BYAFILE.BYA_AMT_PL_1_2_10,
      *          BYAFILE.BYA_AMT_PL_2_1_1,
      *          BYAFILE.BYA_AMT_PL_2_1_2,
      *          BYAFILE.BYA_AMT_PL_2_1_3,
      *          BYAFILE.BYA_AMT_PL_2_1_4,
      *          BYAFILE.BYA_AMT_PL_2_1_5,
      *          BYAFILE.BYA_AMT_PL_2_1_6,
      *          BYAFILE.BYA_AMT_PL_2_1_7,
      *          BYAFILE.BYA_AMT_PL_2_1_8,
      *          BYAFILE.BYA_AMT_PL_2_1_9,
      *          BYAFILE.BYA_AMT_PL_2_1_10,
      *          BYAFILE.BYA_AMT_PL_2_2_1,
      *          BYAFILE.BYA_AMT_PL_2_2_2,
      *          BYAFILE.BYA_AMT_PL_2_2_3,
      *          BYAFILE.BYA_AMT_PL_2_2_4,
      *          BYAFILE.BYA_AMT_PL_2_2_5,
      *          BYAFILE.BYA_AMT_PL_2_2_6,
      *          BYAFILE.BYA_AMT_PL_2_2_7,
      *          BYAFILE.BYA_AMT_PL_2_2_8,
      *          BYAFILE.BYA_AMT_PL_2_2_9,
      *          BYAFILE.BYA_AMT_PL_2_2_10,
      *          BYAFILE.BYA_AMT_PL_3_1_1,
      *          BYAFILE.BYA_AMT_PL_3_1_2,
      *          BYAFILE.BYA_AMT_PL_3_1_3,
      *          BYAFILE.BYA_AMT_PL_3_1_4,
      *          BYAFILE.BYA_AMT_PL_3_1_5,
      *          BYAFILE.BYA_AMT_PL_3_1_6,
      *          BYAFILE.BYA_AMT_PL_3_1_7,
      *          BYAFILE.BYA_AMT_PL_3_1_8,
      *          BYAFILE.BYA_AMT_PL_3_1_9,
      *          BYAFILE.BYA_AMT_PL_3_1_10,
      *          BYAFILE.BYA_AMT_PL_3_2_1,
      *          BYAFILE.BYA_AMT_PL_3_2_2,
      *          BYAFILE.BYA_AMT_PL_3_2_3,
      *          BYAFILE.BYA_AMT_PL_3_2_4,
      *          BYAFILE.BYA_AMT_PL_3_2_5,
      *          BYAFILE.BYA_AMT_PL_3_2_6,
      *          BYAFILE.BYA_AMT_PL_3_2_7,
      *          BYAFILE.BYA_AMT_PL_3_2_8,
      *          BYAFILE.BYA_AMT_PL_3_2_9,
      *          BYAFILE.BYA_AMT_PL_3_2_10,
      *          BYBFILE.BYB_INS_NO_WROTE_1_1,
      *          BYBFILE.BYB_INS_NO_WROTE_1_2,
      *          BYBFILE.BYB_INS_NO_WROTE_1_3,
      *          BYBFILE.BYB_INS_NO_WROTE_1_4,
      *          BYBFILE.BYB_INS_NO_WROTE_1_5,
      *          BYBFILE.BYB_INS_NO_WROTE_1_6,
      *          BYBFILE.BYB_INS_NO_WROTE_1_7,
      *          BYBFILE.BYB_INS_NO_WROTE_1_8,
      *          BYBFILE.BYB_INS_NO_WROTE_1_9,
      *          BYBFILE.BYB_INS_NO_WROTE_1_10,
      *          BYBFILE.BYB_INS_NO_WROTE_2_1,
      *          BYBFILE.BYB_INS_NO_WROTE_2_2,
      *          BYBFILE.BYB_INS_NO_WROTE_2_3,
      *          BYBFILE.BYB_INS_NO_WROTE_2_4,
      *          BYBFILE.BYB_INS_NO_WROTE_2_5,
      *          BYBFILE.BYB_INS_NO_WROTE_2_6,
      *          BYBFILE.BYB_INS_NO_WROTE_2_7,
      *          BYBFILE.BYB_INS_NO_WROTE_2_8,
      *          BYBFILE.BYB_INS_NO_WROTE_2_9,
      *          BYBFILE.BYB_INS_NO_WROTE_2_10,
      *          BYBFILE.BYB_UNITS_1_1_1,
      *          BYBFILE.BYB_UNITS_1_1_2,
      *          BYBFILE.BYB_UNITS_1_1_3,
      *          BYBFILE.BYB_UNITS_1_1_4,
      *          BYBFILE.BYB_UNITS_1_1_5,
      *          BYBFILE.BYB_UNITS_1_1_6,
      *          BYBFILE.BYB_UNITS_1_1_7,
      *          BYBFILE.BYB_UNITS_1_1_8,
      *          BYBFILE.BYB_UNITS_1_1_9,
      *          BYBFILE.BYB_UNITS_1_1_10,
      *          BYBFILE.BYB_UNITS_1_2_1,
      *          BYBFILE.BYB_UNITS_1_2_2,
      *          BYBFILE.BYB_UNITS_1_2_3,
      *          BYBFILE.BYB_UNITS_1_2_4,
      *          BYBFILE.BYB_UNITS_1_2_5,
      *          BYBFILE.BYB_UNITS_1_2_6,
      *          BYBFILE.BYB_UNITS_1_2_7,
      *          BYBFILE.BYB_UNITS_1_2_8,
      *          BYBFILE.BYB_UNITS_1_2_9,
      *          BYBFILE.BYB_UNITS_1_2_10,
      *          BYBFILE.BYB_UNITS_2_1_1,
      *          BYBFILE.BYB_UNITS_2_1_2,
      *          BYBFILE.BYB_UNITS_2_1_3,
      *          BYBFILE.BYB_UNITS_2_1_4,
      *          BYBFILE.BYB_UNITS_2_1_5,
      *          BYBFILE.BYB_UNITS_2_1_6,
      *          BYBFILE.BYB_UNITS_2_1_7,
      *          BYBFILE.BYB_UNITS_2_1_8,
      *          BYBFILE.BYB_UNITS_2_1_9,
      *          BYBFILE.BYB_UNITS_2_1_10,
      *          BYBFILE.BYB_UNITS_2_2_1,
      *          BYBFILE.BYB_UNITS_2_2_2,
      *          BYBFILE.BYB_UNITS_2_2_3,
      *          BYBFILE.BYB_UNITS_2_2_4,
      *          BYBFILE.BYB_UNITS_2_2_5,
      *          BYBFILE.BYB_UNITS_2_2_6,
      *          BYBFILE.BYB_UNITS_2_2_7,
      *          BYBFILE.BYB_UNITS_2_2_8,
      *          BYBFILE.BYB_UNITS_2_2_9,
      *          BYBFILE.BYB_UNITS_2_2_10,
      *          BYBFILE.BYB_UNITS_3_1_1,
      *          BYBFILE.BYB_UNITS_3_1_2,
      *          BYBFILE.BYB_UNITS_3_1_3,
      *          BYBFILE.BYB_UNITS_3_1_4,
      *          BYBFILE.BYB_UNITS_3_1_5,
      *          BYBFILE.BYB_UNITS_3_1_6,
      *          BYBFILE.BYB_UNITS_3_1_7,
      *          BYBFILE.BYB_UNITS_3_1_8,
      *          BYBFILE.BYB_UNITS_3_1_9,
      *          BYBFILE.BYB_UNITS_3_1_10,
      *          BYBFILE.BYB_UNITS_3_2_1,
      *          BYBFILE.BYB_UNITS_3_2_2,
      *          BYBFILE.BYB_UNITS_3_2_3,
      *          BYBFILE.BYB_UNITS_3_2_4,
      *          BYBFILE.BYB_UNITS_3_2_5,
      *          BYBFILE.BYB_UNITS_3_2_6,
      *          BYBFILE.BYB_UNITS_3_2_7,
      *          BYBFILE.BYB_UNITS_3_2_8,
      *          BYBFILE.BYB_UNITS_3_2_9,
      *          BYBFILE.BYB_UNITS_3_2_10,
      *          BYBFILE.BYB_OUTBAL_1_1_1,
      *          BYBFILE.BYB_OUTBAL_1_1_2,
      *          BYBFILE.BYB_OUTBAL_1_1_3,
      *          BYBFILE.BYB_OUTBAL_1_1_4,
      *          BYBFILE.BYB_OUTBAL_1_1_5,
      *          BYBFILE.BYB_OUTBAL_1_1_6,
      *          BYBFILE.BYB_OUTBAL_1_1_7,
      *          BYBFILE.BYB_OUTBAL_1_1_8,
      *          BYBFILE.BYB_OUTBAL_1_1_9,
      *          BYBFILE.BYB_OUTBAL_1_1_10,
      *          BYBFILE.BYB_OUTBAL_1_2_1,
      *          BYBFILE.BYB_OUTBAL_1_2_2,
      *          BYBFILE.BYB_OUTBAL_1_2_3,
      *          BYBFILE.BYB_OUTBAL_1_2_4,
      *          BYBFILE.BYB_OUTBAL_1_2_5,
      *          BYBFILE.BYB_OUTBAL_1_2_6,
      *          BYBFILE.BYB_OUTBAL_1_2_7,
      *          BYBFILE.BYB_OUTBAL_1_2_8,
      *          BYBFILE.BYB_OUTBAL_1_2_9,
      *          BYBFILE.BYB_OUTBAL_1_2_10,
      *          BYBFILE.BYB_OUTBAL_2_1_1,
      *          BYBFILE.BYB_OUTBAL_2_1_2,
      *          BYBFILE.BYB_OUTBAL_2_1_3,
      *          BYBFILE.BYB_OUTBAL_2_1_4,
      *          BYBFILE.BYB_OUTBAL_2_1_5,
      *          BYBFILE.BYB_OUTBAL_2_1_6,
      *          BYBFILE.BYB_OUTBAL_2_1_7,
      *          BYBFILE.BYB_OUTBAL_2_1_8,
      *          BYBFILE.BYB_OUTBAL_2_1_9,
      *          BYBFILE.BYB_OUTBAL_2_1_10,
      *          BYBFILE.BYB_OUTBAL_2_2_1,
      *          BYBFILE.BYB_OUTBAL_2_2_2,
      *          BYBFILE.BYB_OUTBAL_2_2_3,
      *          BYBFILE.BYB_OUTBAL_2_2_4,
      *          BYBFILE.BYB_OUTBAL_2_2_5,
      *          BYBFILE.BYB_OUTBAL_2_2_6,
      *          BYBFILE.BYB_OUTBAL_2_2_7,
      *          BYBFILE.BYB_OUTBAL_2_2_8,
      *          BYBFILE.BYB_OUTBAL_2_2_9,
      *          BYBFILE.BYB_OUTBAL_2_2_10,
      *          BYBFILE.BYB_OUTBAL_3_1_1,
      *          BYBFILE.BYB_OUTBAL_3_1_2,
      *          BYBFILE.BYB_OUTBAL_3_1_3,
      *          BYBFILE.BYB_OUTBAL_3_1_4,
      *          BYBFILE.BYB_OUTBAL_3_1_5,
      *          BYBFILE.BYB_OUTBAL_3_1_6,
      *          BYBFILE.BYB_OUTBAL_3_1_7,
      *          BYBFILE.BYB_OUTBAL_3_1_8,
      *          BYBFILE.BYB_OUTBAL_3_1_9,
      *          BYBFILE.BYB_OUTBAL_3_1_10,
      *          BYBFILE.BYB_OUTBAL_3_2_1,
      *          BYBFILE.BYB_OUTBAL_3_2_2,
      *          BYBFILE.BYB_OUTBAL_3_2_3,
      *          BYBFILE.BYB_OUTBAL_3_2_4,
      *          BYBFILE.BYB_OUTBAL_3_2_5,
      *          BYBFILE.BYB_OUTBAL_3_2_6,
      *          BYBFILE.BYB_OUTBAL_3_2_7,
      *          BYBFILE.BYB_OUTBAL_3_2_8,
      *          BYBFILE.BYB_OUTBAL_3_2_9,
      *          BYBFILE.BYB_OUTBAL_3_2_10,
      *          LCFILE.LC_CODE,
      *          ISNULL(LCFILE.LC_SMALLOAN,' '),
      *          ISNULL(LCFILE.LC_SALEFIN,' '),
      *          ISNULL(LCFILE.LC_SERVACCT,' '),
      *          ISNULL(LCFILE.LC_LOANTYPE,' '),
      *          ISNULL(LCFILE.LC_AFP_CLASS,' '),
      *          ISNULL(LCFILE.LC_GIFTNET,' ')
      *    FROM DBO.BYAFILE BYAFILE
      *        ,DBO.BYBFILE BYBFILE
      *    LEFT OUTER JOIN
      *         DBO.LCFILE LCFILE
      *      ON ( LCFILE.LC_CODE = BYBFILE.BYB_CLASS ) 
      *   WHERE ( BYAFILE.BYA_DATE_CCYYMM BETWEEN @QBY1_WBEG_DATE_CCYYMM
      *                                       AND @QBY1_WEND_DATE_CCYYMM
      *     AND   BYAFILE.BYA_BRNO             >= @QBY1_WBEG_BRNO
      *     AND   BYAFILE.BYA_CLASS       BETWEEN @QBY1_WBEG_CLASS
      *                                       AND @QBY1_WEND_CLASS )
      *	 AND ( BYBFILE.BYB_DATE_CCYYMM       = BYAFILE.BYA_DATE_CCYYMM 
      *     AND   BYBFILE.BYB_BRNO              = BYAFILE.BYA_BRNO 
      *     AND   BYBFILE.BYB_CLASS             = BYAFILE.BYA_CLASS ) 
      *   ORDER BY BYAFILE.BYA_DATE_CCYYMM,
      *            BYAFILE.BYA_BRNO,
      *            BYAFILE.BYA_CLASS;


           EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BY1_CURSOR CURSOR FOR
                :BY1-RETURN-CODE = EXEC SSP_OPREPT_BYFILE_SELECT 
              ( :QBY1-WBEG-DATE-CCYYMM, 
                :QBY1-WEND-DATE-CCYYMM,  
                :QBY1-WBEG-BRNO, 
                :QBY1-WBEG-CLASS,
                :QBY1-WEND-CLASS )
           END-EXEC.

           EXEC SQL
               OPEN BY1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBY1-WBEG-DATE-CCYYMM
                          QBY1-WEND-DATE-CCYYMM
                          QBY1-WBEG-BRNO
                          QBY1-WEND-BRNO
                          QBY1-WBEG-CLASS
                          QBY1-WEND-CLASS.

           MOVE STAT---GOOD TO BYA1-CURSOR-STAT.
   
           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-BYA1-FILE. *> EMBEDDED - BYA,BYB
           PERFORM READ-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.

           MOVE BYA-DATE-CCYYMM      TO QBYA-DATE-CCYYMM.
           MOVE BYA-BRNO             TO QBYA-BRNO.
           MOVE BYA-CLASS            TO QBYA-CLASS.

           EXEC SQL
               SELECT BYAFILE.BYA_DATE_CCYYMM,
                      BYAFILE.BYA_BRNO,
                      BYAFILE.BYA_CLASS,
                      BYAFILE.BYA_NO_1_1_1,
                      BYAFILE.BYA_NO_1_1_2,
                      BYAFILE.BYA_NO_1_1_3,
                      BYAFILE.BYA_NO_1_1_4,
                      BYAFILE.BYA_NO_1_1_5,
                      BYAFILE.BYA_NO_1_1_6,
                      BYAFILE.BYA_NO_1_1_7,
                      BYAFILE.BYA_NO_1_1_8,
                      BYAFILE.BYA_NO_1_1_9,
                      BYAFILE.BYA_NO_1_1_10,
                      BYAFILE.BYA_NO_1_1_11,
                      BYAFILE.BYA_NO_1_2_1,
                      BYAFILE.BYA_NO_1_2_2,
                      BYAFILE.BYA_NO_1_2_3,
                      BYAFILE.BYA_NO_1_2_4,
                      BYAFILE.BYA_NO_1_2_5,
                      BYAFILE.BYA_NO_1_2_6,
                      BYAFILE.BYA_NO_1_2_7,
                      BYAFILE.BYA_NO_1_2_8,
                      BYAFILE.BYA_NO_1_2_9,
                      BYAFILE.BYA_NO_1_2_10,
                      BYAFILE.BYA_NO_1_2_11,
                      BYAFILE.BYA_NO_2_1_1,
                      BYAFILE.BYA_NO_2_1_2,
                      BYAFILE.BYA_NO_2_1_3,
                      BYAFILE.BYA_NO_2_1_4,
                      BYAFILE.BYA_NO_2_1_5,
                      BYAFILE.BYA_NO_2_1_6,
                      BYAFILE.BYA_NO_2_1_7,
                      BYAFILE.BYA_NO_2_1_8,
                      BYAFILE.BYA_NO_2_1_9,
                      BYAFILE.BYA_NO_2_1_10,
                      BYAFILE.BYA_NO_2_1_11,
                      BYAFILE.BYA_NO_2_2_1,
                      BYAFILE.BYA_NO_2_2_2,
                      BYAFILE.BYA_NO_2_2_3,
                      BYAFILE.BYA_NO_2_2_4,
                      BYAFILE.BYA_NO_2_2_5,
                      BYAFILE.BYA_NO_2_2_6,
                      BYAFILE.BYA_NO_2_2_7,
                      BYAFILE.BYA_NO_2_2_8,
                      BYAFILE.BYA_NO_2_2_9,
                      BYAFILE.BYA_NO_2_2_10,
                      BYAFILE.BYA_NO_2_2_11,
                      BYAFILE.BYA_NO_3_1_1,
                      BYAFILE.BYA_NO_3_1_2,
                      BYAFILE.BYA_NO_3_1_3,
                      BYAFILE.BYA_NO_3_1_4,
                      BYAFILE.BYA_NO_3_1_5,
                      BYAFILE.BYA_NO_3_1_6,
                      BYAFILE.BYA_NO_3_1_7,
                      BYAFILE.BYA_NO_3_1_8,
                      BYAFILE.BYA_NO_3_1_9,
                      BYAFILE.BYA_NO_3_1_10,
                      BYAFILE.BYA_NO_3_1_11,
                      BYAFILE.BYA_NO_3_2_1,
                      BYAFILE.BYA_NO_3_2_2,
                      BYAFILE.BYA_NO_3_2_3,
                      BYAFILE.BYA_NO_3_2_4,
                      BYAFILE.BYA_NO_3_2_5,
                      BYAFILE.BYA_NO_3_2_6,
                      BYAFILE.BYA_NO_3_2_7,
                      BYAFILE.BYA_NO_3_2_8,
                      BYAFILE.BYA_NO_3_2_9,
                      BYAFILE.BYA_NO_3_2_10,
                      BYAFILE.BYA_NO_3_2_11,
                      BYAFILE.BYA_AMT_1_1_1,
                      BYAFILE.BYA_AMT_1_1_2,
                      BYAFILE.BYA_AMT_1_1_3,
                      BYAFILE.BYA_AMT_1_1_4,
                      BYAFILE.BYA_AMT_1_1_5,
                      BYAFILE.BYA_AMT_1_1_6,
                      BYAFILE.BYA_AMT_1_1_7,
                      BYAFILE.BYA_AMT_1_1_8,
                      BYAFILE.BYA_AMT_1_1_9,
                      BYAFILE.BYA_AMT_1_1_10,
                      BYAFILE.BYA_AMT_1_1_11,
                      BYAFILE.BYA_AMT_1_2_1,
                      BYAFILE.BYA_AMT_1_2_2,
                      BYAFILE.BYA_AMT_1_2_3,
                      BYAFILE.BYA_AMT_1_2_4,
                      BYAFILE.BYA_AMT_1_2_5,
                      BYAFILE.BYA_AMT_1_2_6,
                      BYAFILE.BYA_AMT_1_2_7,
                      BYAFILE.BYA_AMT_1_2_8,
                      BYAFILE.BYA_AMT_1_2_9,
                      BYAFILE.BYA_AMT_1_2_10,
                      BYAFILE.BYA_AMT_1_2_11,
                      BYAFILE.BYA_AMT_2_1_1,
                      BYAFILE.BYA_AMT_2_1_2,
                      BYAFILE.BYA_AMT_2_1_3,
                      BYAFILE.BYA_AMT_2_1_4,
                      BYAFILE.BYA_AMT_2_1_5,
                      BYAFILE.BYA_AMT_2_1_6,
                      BYAFILE.BYA_AMT_2_1_7,
                      BYAFILE.BYA_AMT_2_1_8,
                      BYAFILE.BYA_AMT_2_1_9,
                      BYAFILE.BYA_AMT_2_1_10,
                      BYAFILE.BYA_AMT_2_1_11,
                      BYAFILE.BYA_AMT_2_2_1,
                      BYAFILE.BYA_AMT_2_2_2,
                      BYAFILE.BYA_AMT_2_2_3,
                      BYAFILE.BYA_AMT_2_2_4,
                      BYAFILE.BYA_AMT_2_2_5,
                      BYAFILE.BYA_AMT_2_2_6,
                      BYAFILE.BYA_AMT_2_2_7,
                      BYAFILE.BYA_AMT_2_2_8,
                      BYAFILE.BYA_AMT_2_2_9,
                      BYAFILE.BYA_AMT_2_2_10,
                      BYAFILE.BYA_AMT_2_2_11,
                      BYAFILE.BYA_AMT_3_1_1,
                      BYAFILE.BYA_AMT_3_1_2,
                      BYAFILE.BYA_AMT_3_1_3,
                      BYAFILE.BYA_AMT_3_1_4,
                      BYAFILE.BYA_AMT_3_1_5,
                      BYAFILE.BYA_AMT_3_1_6,
                      BYAFILE.BYA_AMT_3_1_7,
                      BYAFILE.BYA_AMT_3_1_8,
                      BYAFILE.BYA_AMT_3_1_9,
                      BYAFILE.BYA_AMT_3_1_10,
                      BYAFILE.BYA_AMT_3_1_11,
                      BYAFILE.BYA_AMT_3_2_1,
                      BYAFILE.BYA_AMT_3_2_2,
                      BYAFILE.BYA_AMT_3_2_3,
                      BYAFILE.BYA_AMT_3_2_4,
                      BYAFILE.BYA_AMT_3_2_5,
                      BYAFILE.BYA_AMT_3_2_6,
                      BYAFILE.BYA_AMT_3_2_7,
                      BYAFILE.BYA_AMT_3_2_8,
                      BYAFILE.BYA_AMT_3_2_9,
                      BYAFILE.BYA_AMT_3_2_10,
                      BYAFILE.BYA_AMT_3_2_11,
                      BYAFILE.BYA_NO_PL_1_1_1,
                      BYAFILE.BYA_NO_PL_1_1_2,
                      BYAFILE.BYA_NO_PL_1_1_3,
                      BYAFILE.BYA_NO_PL_1_1_4,
                      BYAFILE.BYA_NO_PL_1_1_5,
                      BYAFILE.BYA_NO_PL_1_1_6,
                      BYAFILE.BYA_NO_PL_1_1_7,
                      BYAFILE.BYA_NO_PL_1_1_8,
                      BYAFILE.BYA_NO_PL_1_1_9,
                      BYAFILE.BYA_NO_PL_1_1_10,
                      BYAFILE.BYA_NO_PL_1_2_1,
                      BYAFILE.BYA_NO_PL_1_2_2,
                      BYAFILE.BYA_NO_PL_1_2_3,
                      BYAFILE.BYA_NO_PL_1_2_4,
                      BYAFILE.BYA_NO_PL_1_2_5,
                      BYAFILE.BYA_NO_PL_1_2_6,
                      BYAFILE.BYA_NO_PL_1_2_7,
                      BYAFILE.BYA_NO_PL_1_2_8,
                      BYAFILE.BYA_NO_PL_1_2_9,
                      BYAFILE.BYA_NO_PL_1_2_10,
                      BYAFILE.BYA_NO_PL_2_1_1,
                      BYAFILE.BYA_NO_PL_2_1_2,
                      BYAFILE.BYA_NO_PL_2_1_3,
                      BYAFILE.BYA_NO_PL_2_1_4,
                      BYAFILE.BYA_NO_PL_2_1_5,
                      BYAFILE.BYA_NO_PL_2_1_6,
                      BYAFILE.BYA_NO_PL_2_1_7,
                      BYAFILE.BYA_NO_PL_2_1_8,
                      BYAFILE.BYA_NO_PL_2_1_9,
                      BYAFILE.BYA_NO_PL_2_1_10,
                      BYAFILE.BYA_NO_PL_2_2_1,
                      BYAFILE.BYA_NO_PL_2_2_2,
                      BYAFILE.BYA_NO_PL_2_2_3,
                      BYAFILE.BYA_NO_PL_2_2_4,
                      BYAFILE.BYA_NO_PL_2_2_5,
                      BYAFILE.BYA_NO_PL_2_2_6,
                      BYAFILE.BYA_NO_PL_2_2_7,
                      BYAFILE.BYA_NO_PL_2_2_8,
                      BYAFILE.BYA_NO_PL_2_2_9,
                      BYAFILE.BYA_NO_PL_2_2_10,
                      BYAFILE.BYA_NO_PL_3_1_1,
                      BYAFILE.BYA_NO_PL_3_1_2,
                      BYAFILE.BYA_NO_PL_3_1_3,
                      BYAFILE.BYA_NO_PL_3_1_4,
                      BYAFILE.BYA_NO_PL_3_1_5,
                      BYAFILE.BYA_NO_PL_3_1_6,
                      BYAFILE.BYA_NO_PL_3_1_7,
                      BYAFILE.BYA_NO_PL_3_1_8,
                      BYAFILE.BYA_NO_PL_3_1_9,
                      BYAFILE.BYA_NO_PL_3_1_10,
                      BYAFILE.BYA_NO_PL_3_2_1,
                      BYAFILE.BYA_NO_PL_3_2_2,
                      BYAFILE.BYA_NO_PL_3_2_3,
                      BYAFILE.BYA_NO_PL_3_2_4,
                      BYAFILE.BYA_NO_PL_3_2_5,
                      BYAFILE.BYA_NO_PL_3_2_6,
                      BYAFILE.BYA_NO_PL_3_2_7,
                      BYAFILE.BYA_NO_PL_3_2_8,
                      BYAFILE.BYA_NO_PL_3_2_9,
                      BYAFILE.BYA_NO_PL_3_2_10,
                      BYAFILE.BYA_AMT_PL_1_1_1,
                      BYAFILE.BYA_AMT_PL_1_1_2,
                      BYAFILE.BYA_AMT_PL_1_1_3,
                      BYAFILE.BYA_AMT_PL_1_1_4,
                      BYAFILE.BYA_AMT_PL_1_1_5,
                      BYAFILE.BYA_AMT_PL_1_1_6,
                      BYAFILE.BYA_AMT_PL_1_1_7,
                      BYAFILE.BYA_AMT_PL_1_1_8,
                      BYAFILE.BYA_AMT_PL_1_1_9,
                      BYAFILE.BYA_AMT_PL_1_1_10,
                      BYAFILE.BYA_AMT_PL_1_2_1,
                      BYAFILE.BYA_AMT_PL_1_2_2,
                      BYAFILE.BYA_AMT_PL_1_2_3,
                      BYAFILE.BYA_AMT_PL_1_2_4,
                      BYAFILE.BYA_AMT_PL_1_2_5,
                      BYAFILE.BYA_AMT_PL_1_2_6,
                      BYAFILE.BYA_AMT_PL_1_2_7,
                      BYAFILE.BYA_AMT_PL_1_2_8,
                      BYAFILE.BYA_AMT_PL_1_2_9,
                      BYAFILE.BYA_AMT_PL_1_2_10,
                      BYAFILE.BYA_AMT_PL_2_1_1,
                      BYAFILE.BYA_AMT_PL_2_1_2,
                      BYAFILE.BYA_AMT_PL_2_1_3,
                      BYAFILE.BYA_AMT_PL_2_1_4,
                      BYAFILE.BYA_AMT_PL_2_1_5,
                      BYAFILE.BYA_AMT_PL_2_1_6,
                      BYAFILE.BYA_AMT_PL_2_1_7,
                      BYAFILE.BYA_AMT_PL_2_1_8,
                      BYAFILE.BYA_AMT_PL_2_1_9,
                      BYAFILE.BYA_AMT_PL_2_1_10,
                      BYAFILE.BYA_AMT_PL_2_2_1,
                      BYAFILE.BYA_AMT_PL_2_2_2,
                      BYAFILE.BYA_AMT_PL_2_2_3,
                      BYAFILE.BYA_AMT_PL_2_2_4,
                      BYAFILE.BYA_AMT_PL_2_2_5,
                      BYAFILE.BYA_AMT_PL_2_2_6,
                      BYAFILE.BYA_AMT_PL_2_2_7,
                      BYAFILE.BYA_AMT_PL_2_2_8,
                      BYAFILE.BYA_AMT_PL_2_2_9,
                      BYAFILE.BYA_AMT_PL_2_2_10,
                      BYAFILE.BYA_AMT_PL_3_1_1,
                      BYAFILE.BYA_AMT_PL_3_1_2,
                      BYAFILE.BYA_AMT_PL_3_1_3,
                      BYAFILE.BYA_AMT_PL_3_1_4,
                      BYAFILE.BYA_AMT_PL_3_1_5,
                      BYAFILE.BYA_AMT_PL_3_1_6,
                      BYAFILE.BYA_AMT_PL_3_1_7,
                      BYAFILE.BYA_AMT_PL_3_1_8,
                      BYAFILE.BYA_AMT_PL_3_1_9,
                      BYAFILE.BYA_AMT_PL_3_1_10,
                      BYAFILE.BYA_AMT_PL_3_2_1,
                      BYAFILE.BYA_AMT_PL_3_2_2,
                      BYAFILE.BYA_AMT_PL_3_2_3,
                      BYAFILE.BYA_AMT_PL_3_2_4,
                      BYAFILE.BYA_AMT_PL_3_2_5,
                      BYAFILE.BYA_AMT_PL_3_2_6,
                      BYAFILE.BYA_AMT_PL_3_2_7,
                      BYAFILE.BYA_AMT_PL_3_2_8,
                      BYAFILE.BYA_AMT_PL_3_2_9,
                      BYAFILE.BYA_AMT_PL_3_2_10,
                      BYBFILE.BYB_INS_NO_WROTE_1_1,
                      BYBFILE.BYB_INS_NO_WROTE_1_2,
                      BYBFILE.BYB_INS_NO_WROTE_1_3,
                      BYBFILE.BYB_INS_NO_WROTE_1_4,
                      BYBFILE.BYB_INS_NO_WROTE_1_5,
                      BYBFILE.BYB_INS_NO_WROTE_1_6,
                      BYBFILE.BYB_INS_NO_WROTE_1_7,
                      BYBFILE.BYB_INS_NO_WROTE_1_8,
                      BYBFILE.BYB_INS_NO_WROTE_1_9,
                      BYBFILE.BYB_INS_NO_WROTE_1_10,
                      BYBFILE.BYB_INS_NO_WROTE_2_1,
                      BYBFILE.BYB_INS_NO_WROTE_2_2,
                      BYBFILE.BYB_INS_NO_WROTE_2_3,
                      BYBFILE.BYB_INS_NO_WROTE_2_4,
                      BYBFILE.BYB_INS_NO_WROTE_2_5,
                      BYBFILE.BYB_INS_NO_WROTE_2_6,
                      BYBFILE.BYB_INS_NO_WROTE_2_7,
                      BYBFILE.BYB_INS_NO_WROTE_2_8,
                      BYBFILE.BYB_INS_NO_WROTE_2_9,
                      BYBFILE.BYB_INS_NO_WROTE_2_10,
                      BYBFILE.BYB_UNITS_1_1_1,
                      BYBFILE.BYB_UNITS_1_1_2,
                      BYBFILE.BYB_UNITS_1_1_3,
                      BYBFILE.BYB_UNITS_1_1_4,
                      BYBFILE.BYB_UNITS_1_1_5,
                      BYBFILE.BYB_UNITS_1_1_6,
                      BYBFILE.BYB_UNITS_1_1_7,
                      BYBFILE.BYB_UNITS_1_1_8,
                      BYBFILE.BYB_UNITS_1_1_9,
                      BYBFILE.BYB_UNITS_1_1_10,
                      BYBFILE.BYB_UNITS_1_2_1,
                      BYBFILE.BYB_UNITS_1_2_2,
                      BYBFILE.BYB_UNITS_1_2_3,
                      BYBFILE.BYB_UNITS_1_2_4,
                      BYBFILE.BYB_UNITS_1_2_5,
                      BYBFILE.BYB_UNITS_1_2_6,
                      BYBFILE.BYB_UNITS_1_2_7,
                      BYBFILE.BYB_UNITS_1_2_8,
                      BYBFILE.BYB_UNITS_1_2_9,
                      BYBFILE.BYB_UNITS_1_2_10,
                      BYBFILE.BYB_UNITS_2_1_1,
                      BYBFILE.BYB_UNITS_2_1_2,
                      BYBFILE.BYB_UNITS_2_1_3,
                      BYBFILE.BYB_UNITS_2_1_4,
                      BYBFILE.BYB_UNITS_2_1_5,
                      BYBFILE.BYB_UNITS_2_1_6,
                      BYBFILE.BYB_UNITS_2_1_7,
                      BYBFILE.BYB_UNITS_2_1_8,
                      BYBFILE.BYB_UNITS_2_1_9,
                      BYBFILE.BYB_UNITS_2_1_10,
                      BYBFILE.BYB_UNITS_2_2_1,
                      BYBFILE.BYB_UNITS_2_2_2,
                      BYBFILE.BYB_UNITS_2_2_3,
                      BYBFILE.BYB_UNITS_2_2_4,
                      BYBFILE.BYB_UNITS_2_2_5,
                      BYBFILE.BYB_UNITS_2_2_6,
                      BYBFILE.BYB_UNITS_2_2_7,
                      BYBFILE.BYB_UNITS_2_2_8,
                      BYBFILE.BYB_UNITS_2_2_9,
                      BYBFILE.BYB_UNITS_2_2_10,
                      BYBFILE.BYB_UNITS_3_1_1,
                      BYBFILE.BYB_UNITS_3_1_2,
                      BYBFILE.BYB_UNITS_3_1_3,
                      BYBFILE.BYB_UNITS_3_1_4,
                      BYBFILE.BYB_UNITS_3_1_5,
                      BYBFILE.BYB_UNITS_3_1_6,
                      BYBFILE.BYB_UNITS_3_1_7,
                      BYBFILE.BYB_UNITS_3_1_8,
                      BYBFILE.BYB_UNITS_3_1_9,
                      BYBFILE.BYB_UNITS_3_1_10,
                      BYBFILE.BYB_UNITS_3_2_1,
                      BYBFILE.BYB_UNITS_3_2_2,
                      BYBFILE.BYB_UNITS_3_2_3,
                      BYBFILE.BYB_UNITS_3_2_4,
                      BYBFILE.BYB_UNITS_3_2_5,
                      BYBFILE.BYB_UNITS_3_2_6,
                      BYBFILE.BYB_UNITS_3_2_7,
                      BYBFILE.BYB_UNITS_3_2_8,
                      BYBFILE.BYB_UNITS_3_2_9,
                      BYBFILE.BYB_UNITS_3_2_10,
                      BYBFILE.BYB_OUTBAL_1_1_1,
                      BYBFILE.BYB_OUTBAL_1_1_2,
                      BYBFILE.BYB_OUTBAL_1_1_3,
                      BYBFILE.BYB_OUTBAL_1_1_4,
                      BYBFILE.BYB_OUTBAL_1_1_5,
                      BYBFILE.BYB_OUTBAL_1_1_6,
                      BYBFILE.BYB_OUTBAL_1_1_7,
                      BYBFILE.BYB_OUTBAL_1_1_8,
                      BYBFILE.BYB_OUTBAL_1_1_9,
                      BYBFILE.BYB_OUTBAL_1_1_10,
                      BYBFILE.BYB_OUTBAL_1_2_1,
                      BYBFILE.BYB_OUTBAL_1_2_2,
                      BYBFILE.BYB_OUTBAL_1_2_3,
                      BYBFILE.BYB_OUTBAL_1_2_4,
                      BYBFILE.BYB_OUTBAL_1_2_5,
                      BYBFILE.BYB_OUTBAL_1_2_6,
                      BYBFILE.BYB_OUTBAL_1_2_7,
                      BYBFILE.BYB_OUTBAL_1_2_8,
                      BYBFILE.BYB_OUTBAL_1_2_9,
                      BYBFILE.BYB_OUTBAL_1_2_10,
                      BYBFILE.BYB_OUTBAL_2_1_1,
                      BYBFILE.BYB_OUTBAL_2_1_2,
                      BYBFILE.BYB_OUTBAL_2_1_3,
                      BYBFILE.BYB_OUTBAL_2_1_4,
                      BYBFILE.BYB_OUTBAL_2_1_5,
                      BYBFILE.BYB_OUTBAL_2_1_6,
                      BYBFILE.BYB_OUTBAL_2_1_7,
                      BYBFILE.BYB_OUTBAL_2_1_8,
                      BYBFILE.BYB_OUTBAL_2_1_9,
                      BYBFILE.BYB_OUTBAL_2_1_10,
                      BYBFILE.BYB_OUTBAL_2_2_1,
                      BYBFILE.BYB_OUTBAL_2_2_2,
                      BYBFILE.BYB_OUTBAL_2_2_3,
                      BYBFILE.BYB_OUTBAL_2_2_4,
                      BYBFILE.BYB_OUTBAL_2_2_5,
                      BYBFILE.BYB_OUTBAL_2_2_6,
                      BYBFILE.BYB_OUTBAL_2_2_7,
                      BYBFILE.BYB_OUTBAL_2_2_8,
                      BYBFILE.BYB_OUTBAL_2_2_9,
                      BYBFILE.BYB_OUTBAL_2_2_10,
                      BYBFILE.BYB_OUTBAL_3_1_1,
                      BYBFILE.BYB_OUTBAL_3_1_2,
                      BYBFILE.BYB_OUTBAL_3_1_3,
                      BYBFILE.BYB_OUTBAL_3_1_4,
                      BYBFILE.BYB_OUTBAL_3_1_5,
                      BYBFILE.BYB_OUTBAL_3_1_6,
                      BYBFILE.BYB_OUTBAL_3_1_7,
                      BYBFILE.BYB_OUTBAL_3_1_8,
                      BYBFILE.BYB_OUTBAL_3_1_9,
                      BYBFILE.BYB_OUTBAL_3_1_10,
                      BYBFILE.BYB_OUTBAL_3_2_1,
                      BYBFILE.BYB_OUTBAL_3_2_2,
                      BYBFILE.BYB_OUTBAL_3_2_3,
                      BYBFILE.BYB_OUTBAL_3_2_4,
                      BYBFILE.BYB_OUTBAL_3_2_5,
                      BYBFILE.BYB_OUTBAL_3_2_6,
                      BYBFILE.BYB_OUTBAL_3_2_7,
                      BYBFILE.BYB_OUTBAL_3_2_8,
                      BYBFILE.BYB_OUTBAL_3_2_9,
                      BYBFILE.BYB_OUTBAL_3_2_10 
                 INTO :QBYA-DATE-CCYYMM,
                      :QBYA-BRNO,
                      :QBYA-CLASS,
                      :QBYA-NO-1-1-1,
                      :QBYA-NO-1-1-2,
                      :QBYA-NO-1-1-3,
                      :QBYA-NO-1-1-4,
                      :QBYA-NO-1-1-5,
                      :QBYA-NO-1-1-6,
                      :QBYA-NO-1-1-7,
                      :QBYA-NO-1-1-8,
                      :QBYA-NO-1-1-9,
                      :QBYA-NO-1-1-10,
                      :QBYA-NO-1-1-11,
                      :QBYA-NO-1-2-1,
                      :QBYA-NO-1-2-2,
                      :QBYA-NO-1-2-3,
                      :QBYA-NO-1-2-4,
                      :QBYA-NO-1-2-5,
                      :QBYA-NO-1-2-6,
                      :QBYA-NO-1-2-7,
                      :QBYA-NO-1-2-8,
                      :QBYA-NO-1-2-9,
                      :QBYA-NO-1-2-10,
                      :QBYA-NO-1-2-11,
                      :QBYA-NO-2-1-1,
                      :QBYA-NO-2-1-2,
                      :QBYA-NO-2-1-3,
                      :QBYA-NO-2-1-4,
                      :QBYA-NO-2-1-5,
                      :QBYA-NO-2-1-6,
                      :QBYA-NO-2-1-7,
                      :QBYA-NO-2-1-8,
                      :QBYA-NO-2-1-9,
                      :QBYA-NO-2-1-10,
                      :QBYA-NO-2-1-11,
                      :QBYA-NO-2-2-1,
                      :QBYA-NO-2-2-2,
                      :QBYA-NO-2-2-3,
                      :QBYA-NO-2-2-4,
                      :QBYA-NO-2-2-5,
                      :QBYA-NO-2-2-6,
                      :QBYA-NO-2-2-7,
                      :QBYA-NO-2-2-8,
                      :QBYA-NO-2-2-9,
                      :QBYA-NO-2-2-10,
                      :QBYA-NO-2-2-11,
                      :QBYA-NO-3-1-1,
                      :QBYA-NO-3-1-2,
                      :QBYA-NO-3-1-3,
                      :QBYA-NO-3-1-4,
                      :QBYA-NO-3-1-5,
                      :QBYA-NO-3-1-6,
                      :QBYA-NO-3-1-7,
                      :QBYA-NO-3-1-8,
                      :QBYA-NO-3-1-9,
                      :QBYA-NO-3-1-10,
                      :QBYA-NO-3-1-11,
                      :QBYA-NO-3-2-1,
                      :QBYA-NO-3-2-2,
                      :QBYA-NO-3-2-3,
                      :QBYA-NO-3-2-4,
                      :QBYA-NO-3-2-5,
                      :QBYA-NO-3-2-6,
                      :QBYA-NO-3-2-7,
                      :QBYA-NO-3-2-8,
                      :QBYA-NO-3-2-9,
                      :QBYA-NO-3-2-10,
                      :QBYA-NO-3-2-11,
                      :QBYA-AMT-1-1-1,
                      :QBYA-AMT-1-1-2,
                      :QBYA-AMT-1-1-3,
                      :QBYA-AMT-1-1-4,
                      :QBYA-AMT-1-1-5,
                      :QBYA-AMT-1-1-6,
                      :QBYA-AMT-1-1-7,
                      :QBYA-AMT-1-1-8,
                      :QBYA-AMT-1-1-9,
                      :QBYA-AMT-1-1-10,
                      :QBYA-AMT-1-1-11,
                      :QBYA-AMT-1-2-1,
                      :QBYA-AMT-1-2-2,
                      :QBYA-AMT-1-2-3,
                      :QBYA-AMT-1-2-4,
                      :QBYA-AMT-1-2-5,
                      :QBYA-AMT-1-2-6,
                      :QBYA-AMT-1-2-7,
                      :QBYA-AMT-1-2-8,
                      :QBYA-AMT-1-2-9,
                      :QBYA-AMT-1-2-10,
                      :QBYA-AMT-1-2-11,
                      :QBYA-AMT-2-1-1,
                      :QBYA-AMT-2-1-2,
                      :QBYA-AMT-2-1-3,
                      :QBYA-AMT-2-1-4,
                      :QBYA-AMT-2-1-5,
                      :QBYA-AMT-2-1-6,
                      :QBYA-AMT-2-1-7,
                      :QBYA-AMT-2-1-8,
                      :QBYA-AMT-2-1-9,
                      :QBYA-AMT-2-1-10,
                      :QBYA-AMT-2-1-11,
                      :QBYA-AMT-2-2-1,
                      :QBYA-AMT-2-2-2,
                      :QBYA-AMT-2-2-3,
                      :QBYA-AMT-2-2-4,
                      :QBYA-AMT-2-2-5,
                      :QBYA-AMT-2-2-6,
                      :QBYA-AMT-2-2-7,
                      :QBYA-AMT-2-2-8,
                      :QBYA-AMT-2-2-9,
                      :QBYA-AMT-2-2-10,
                      :QBYA-AMT-2-2-11,
                      :QBYA-AMT-3-1-1,
                      :QBYA-AMT-3-1-2,
                      :QBYA-AMT-3-1-3,
                      :QBYA-AMT-3-1-4,
                      :QBYA-AMT-3-1-5,
                      :QBYA-AMT-3-1-6,
                      :QBYA-AMT-3-1-7,
                      :QBYA-AMT-3-1-8,
                      :QBYA-AMT-3-1-9,
                      :QBYA-AMT-3-1-10,
                      :QBYA-AMT-3-1-11,
                      :QBYA-AMT-3-2-1,
                      :QBYA-AMT-3-2-2,
                      :QBYA-AMT-3-2-3,
                      :QBYA-AMT-3-2-4,
                      :QBYA-AMT-3-2-5,
                      :QBYA-AMT-3-2-6,
                      :QBYA-AMT-3-2-7,
                      :QBYA-AMT-3-2-8,
                      :QBYA-AMT-3-2-9,
                      :QBYA-AMT-3-2-10,
                      :QBYA-AMT-3-2-11,
                      :QBYA-NO-PL-1-1-1,
                      :QBYA-NO-PL-1-1-2,
                      :QBYA-NO-PL-1-1-3,
                      :QBYA-NO-PL-1-1-4,
                      :QBYA-NO-PL-1-1-5,
                      :QBYA-NO-PL-1-1-6,
                      :QBYA-NO-PL-1-1-7,
                      :QBYA-NO-PL-1-1-8,
                      :QBYA-NO-PL-1-1-9,
                      :QBYA-NO-PL-1-1-10,
                      :QBYA-NO-PL-1-2-1,
                      :QBYA-NO-PL-1-2-2,
                      :QBYA-NO-PL-1-2-3,
                      :QBYA-NO-PL-1-2-4,
                      :QBYA-NO-PL-1-2-5,
                      :QBYA-NO-PL-1-2-6,
                      :QBYA-NO-PL-1-2-7,
                      :QBYA-NO-PL-1-2-8,
                      :QBYA-NO-PL-1-2-9,
                      :QBYA-NO-PL-1-2-10,
                      :QBYA-NO-PL-2-1-1,
                      :QBYA-NO-PL-2-1-2,
                      :QBYA-NO-PL-2-1-3,
                      :QBYA-NO-PL-2-1-4,
                      :QBYA-NO-PL-2-1-5,
                      :QBYA-NO-PL-2-1-6,
                      :QBYA-NO-PL-2-1-7,
                      :QBYA-NO-PL-2-1-8,
                      :QBYA-NO-PL-2-1-9,
                      :QBYA-NO-PL-2-1-10,
                      :QBYA-NO-PL-2-2-1,
                      :QBYA-NO-PL-2-2-2,
                      :QBYA-NO-PL-2-2-3,
                      :QBYA-NO-PL-2-2-4,
                      :QBYA-NO-PL-2-2-5,
                      :QBYA-NO-PL-2-2-6,
                      :QBYA-NO-PL-2-2-7,
                      :QBYA-NO-PL-2-2-8,
                      :QBYA-NO-PL-2-2-9,
                      :QBYA-NO-PL-2-2-10,
                      :QBYA-NO-PL-3-1-1,
                      :QBYA-NO-PL-3-1-2,
                      :QBYA-NO-PL-3-1-3,
                      :QBYA-NO-PL-3-1-4,
                      :QBYA-NO-PL-3-1-5,
                      :QBYA-NO-PL-3-1-6,
                      :QBYA-NO-PL-3-1-7,
                      :QBYA-NO-PL-3-1-8,
                      :QBYA-NO-PL-3-1-9,
                      :QBYA-NO-PL-3-1-10,
                      :QBYA-NO-PL-3-2-1,
                      :QBYA-NO-PL-3-2-2,
                      :QBYA-NO-PL-3-2-3,
                      :QBYA-NO-PL-3-2-4,
                      :QBYA-NO-PL-3-2-5,
                      :QBYA-NO-PL-3-2-6,
                      :QBYA-NO-PL-3-2-7,
                      :QBYA-NO-PL-3-2-8,
                      :QBYA-NO-PL-3-2-9,
                      :QBYA-NO-PL-3-2-10,
                      :QBYA-AMT-PL-1-1-1,
                      :QBYA-AMT-PL-1-1-2,
                      :QBYA-AMT-PL-1-1-3,
                      :QBYA-AMT-PL-1-1-4,
                      :QBYA-AMT-PL-1-1-5,
                      :QBYA-AMT-PL-1-1-6,
                      :QBYA-AMT-PL-1-1-7,
                      :QBYA-AMT-PL-1-1-8,
                      :QBYA-AMT-PL-1-1-9,
                      :QBYA-AMT-PL-1-1-10,
                      :QBYA-AMT-PL-1-2-1,
                      :QBYA-AMT-PL-1-2-2,
                      :QBYA-AMT-PL-1-2-3,
                      :QBYA-AMT-PL-1-2-4,
                      :QBYA-AMT-PL-1-2-5,
                      :QBYA-AMT-PL-1-2-6,
                      :QBYA-AMT-PL-1-2-7,
                      :QBYA-AMT-PL-1-2-8,
                      :QBYA-AMT-PL-1-2-9,
                      :QBYA-AMT-PL-1-2-10,
                      :QBYA-AMT-PL-2-1-1,
                      :QBYA-AMT-PL-2-1-2,
                      :QBYA-AMT-PL-2-1-3,
                      :QBYA-AMT-PL-2-1-4,
                      :QBYA-AMT-PL-2-1-5,
                      :QBYA-AMT-PL-2-1-6,
                      :QBYA-AMT-PL-2-1-7,
                      :QBYA-AMT-PL-2-1-8,
                      :QBYA-AMT-PL-2-1-9,
                      :QBYA-AMT-PL-2-1-10,
                      :QBYA-AMT-PL-2-2-1,
                      :QBYA-AMT-PL-2-2-2,
                      :QBYA-AMT-PL-2-2-3,
                      :QBYA-AMT-PL-2-2-4,
                      :QBYA-AMT-PL-2-2-5,
                      :QBYA-AMT-PL-2-2-6,
                      :QBYA-AMT-PL-2-2-7,
                      :QBYA-AMT-PL-2-2-8,
                      :QBYA-AMT-PL-2-2-9,
                      :QBYA-AMT-PL-2-2-10,
                      :QBYA-AMT-PL-3-1-1,
                      :QBYA-AMT-PL-3-1-2,
                      :QBYA-AMT-PL-3-1-3,
                      :QBYA-AMT-PL-3-1-4,
                      :QBYA-AMT-PL-3-1-5,
                      :QBYA-AMT-PL-3-1-6,
                      :QBYA-AMT-PL-3-1-7,
                      :QBYA-AMT-PL-3-1-8,
                      :QBYA-AMT-PL-3-1-9,
                      :QBYA-AMT-PL-3-1-10,
                      :QBYA-AMT-PL-3-2-1,
                      :QBYA-AMT-PL-3-2-2,
                      :QBYA-AMT-PL-3-2-3,
                      :QBYA-AMT-PL-3-2-4,
                      :QBYA-AMT-PL-3-2-5,
                      :QBYA-AMT-PL-3-2-6,
                      :QBYA-AMT-PL-3-2-7,
                      :QBYA-AMT-PL-3-2-8,
                      :QBYA-AMT-PL-3-2-9,
                      :QBYA-AMT-PL-3-2-10,
                      :QBYB-INS-NO-WROTE-1-1,
                      :QBYB-INS-NO-WROTE-1-2,
                      :QBYB-INS-NO-WROTE-1-3,
                      :QBYB-INS-NO-WROTE-1-4,
                      :QBYB-INS-NO-WROTE-1-5,
                      :QBYB-INS-NO-WROTE-1-6,
                      :QBYB-INS-NO-WROTE-1-7,
                      :QBYB-INS-NO-WROTE-1-8,
                      :QBYB-INS-NO-WROTE-1-9,
                      :QBYB-INS-NO-WROTE-1-10,
                      :QBYB-INS-NO-WROTE-2-1,
                      :QBYB-INS-NO-WROTE-2-2,
                      :QBYB-INS-NO-WROTE-2-3,
                      :QBYB-INS-NO-WROTE-2-4,
                      :QBYB-INS-NO-WROTE-2-5,
                      :QBYB-INS-NO-WROTE-2-6,
                      :QBYB-INS-NO-WROTE-2-7,
                      :QBYB-INS-NO-WROTE-2-8,
                      :QBYB-INS-NO-WROTE-2-9,
                      :QBYB-INS-NO-WROTE-2-10,
                      :QBYB-UNITS-1-1-1,
                      :QBYB-UNITS-1-1-2,
                      :QBYB-UNITS-1-1-3,
                      :QBYB-UNITS-1-1-4,
                      :QBYB-UNITS-1-1-5,
                      :QBYB-UNITS-1-1-6,
                      :QBYB-UNITS-1-1-7,
                      :QBYB-UNITS-1-1-8,
                      :QBYB-UNITS-1-1-9,
                      :QBYB-UNITS-1-1-10,
                      :QBYB-UNITS-1-2-1,
                      :QBYB-UNITS-1-2-2,
                      :QBYB-UNITS-1-2-3,
                      :QBYB-UNITS-1-2-4,
                      :QBYB-UNITS-1-2-5,
                      :QBYB-UNITS-1-2-6,
                      :QBYB-UNITS-1-2-7,
                      :QBYB-UNITS-1-2-8,
                      :QBYB-UNITS-1-2-9,
                      :QBYB-UNITS-1-2-10,
                      :QBYB-UNITS-2-1-1,
                      :QBYB-UNITS-2-1-2,
                      :QBYB-UNITS-2-1-3,
                      :QBYB-UNITS-2-1-4,
                      :QBYB-UNITS-2-1-5,
                      :QBYB-UNITS-2-1-6,
                      :QBYB-UNITS-2-1-7,
                      :QBYB-UNITS-2-1-8,
                      :QBYB-UNITS-2-1-9,
                      :QBYB-UNITS-2-1-10,
                      :QBYB-UNITS-2-2-1,
                      :QBYB-UNITS-2-2-2,
                      :QBYB-UNITS-2-2-3,
                      :QBYB-UNITS-2-2-4,
                      :QBYB-UNITS-2-2-5,
                      :QBYB-UNITS-2-2-6,
                      :QBYB-UNITS-2-2-7,
                      :QBYB-UNITS-2-2-8,
                      :QBYB-UNITS-2-2-9,
                      :QBYB-UNITS-2-2-10,
                      :QBYB-UNITS-3-1-1,
                      :QBYB-UNITS-3-1-2,
                      :QBYB-UNITS-3-1-3,
                      :QBYB-UNITS-3-1-4,
                      :QBYB-UNITS-3-1-5,
                      :QBYB-UNITS-3-1-6,
                      :QBYB-UNITS-3-1-7,
                      :QBYB-UNITS-3-1-8,
                      :QBYB-UNITS-3-1-9,
                      :QBYB-UNITS-3-1-10,
                      :QBYB-UNITS-3-2-1,
                      :QBYB-UNITS-3-2-2,
                      :QBYB-UNITS-3-2-3,
                      :QBYB-UNITS-3-2-4,
                      :QBYB-UNITS-3-2-5,
                      :QBYB-UNITS-3-2-6,
                      :QBYB-UNITS-3-2-7,
                      :QBYB-UNITS-3-2-8,
                      :QBYB-UNITS-3-2-9,
                      :QBYB-UNITS-3-2-10,
                      :QBYB-OUTBAL-1-1-1,
                      :QBYB-OUTBAL-1-1-2,
                      :QBYB-OUTBAL-1-1-3,
                      :QBYB-OUTBAL-1-1-4,
                      :QBYB-OUTBAL-1-1-5,
                      :QBYB-OUTBAL-1-1-6,
                      :QBYB-OUTBAL-1-1-7,
                      :QBYB-OUTBAL-1-1-8,
                      :QBYB-OUTBAL-1-1-9,
                      :QBYB-OUTBAL-1-1-10,
                      :QBYB-OUTBAL-1-2-1,
                      :QBYB-OUTBAL-1-2-2,
                      :QBYB-OUTBAL-1-2-3,
                      :QBYB-OUTBAL-1-2-4,
                      :QBYB-OUTBAL-1-2-5,
                      :QBYB-OUTBAL-1-2-6,
                      :QBYB-OUTBAL-1-2-7,
                      :QBYB-OUTBAL-1-2-8,
                      :QBYB-OUTBAL-1-2-9,
                      :QBYB-OUTBAL-1-2-10,
                      :QBYB-OUTBAL-2-1-1,
                      :QBYB-OUTBAL-2-1-2,
                      :QBYB-OUTBAL-2-1-3,
                      :QBYB-OUTBAL-2-1-4,
                      :QBYB-OUTBAL-2-1-5,
                      :QBYB-OUTBAL-2-1-6,
                      :QBYB-OUTBAL-2-1-7,
                      :QBYB-OUTBAL-2-1-8,
                      :QBYB-OUTBAL-2-1-9,
                      :QBYB-OUTBAL-2-1-10,
                      :QBYB-OUTBAL-2-2-1,
                      :QBYB-OUTBAL-2-2-2,
                      :QBYB-OUTBAL-2-2-3,
                      :QBYB-OUTBAL-2-2-4,
                      :QBYB-OUTBAL-2-2-5,
                      :QBYB-OUTBAL-2-2-6,
                      :QBYB-OUTBAL-2-2-7,
                      :QBYB-OUTBAL-2-2-8,
                      :QBYB-OUTBAL-2-2-9,
                      :QBYB-OUTBAL-2-2-10,
                      :QBYB-OUTBAL-3-1-1,
                      :QBYB-OUTBAL-3-1-2,
                      :QBYB-OUTBAL-3-1-3,
                      :QBYB-OUTBAL-3-1-4,
                      :QBYB-OUTBAL-3-1-5,
                      :QBYB-OUTBAL-3-1-6,
                      :QBYB-OUTBAL-3-1-7,
                      :QBYB-OUTBAL-3-1-8,
                      :QBYB-OUTBAL-3-1-9,
                      :QBYB-OUTBAL-3-1-10,
                      :QBYB-OUTBAL-3-2-1,
                      :QBYB-OUTBAL-3-2-2,
                      :QBYB-OUTBAL-3-2-3,
                      :QBYB-OUTBAL-3-2-4,
                      :QBYB-OUTBAL-3-2-5,
                      :QBYB-OUTBAL-3-2-6,
                      :QBYB-OUTBAL-3-2-7,
                      :QBYB-OUTBAL-3-2-8,
                      :QBYB-OUTBAL-3-2-9,
                      :QBYB-OUTBAL-3-2-10 
                 FROM DBO.BYAFILE BYAFILE,
                      DBO.BYBFILE BYBFILE 
                 WHERE (BYAFILE.BYA_DATE_CCYYMM  = :QBYA-DATE-CCYYMM
                   AND  BYAFILE.BYA_BRNO         = :QBYA-BRNO 
                   AND  BYAFILE.BYA_CLASS        = :QBYA-CLASS)
                   AND (BYBFILE.BYB_DATE_CCYYMM  =
                                             BYAFILE.BYA_DATE_CCYYMM 
                   AND  BYBFILE.BYB_BRNO         = BYAFILE.BYA_BRNO 
                   AND  BYBFILE.BYB_CLASS        = BYAFILE.BYA_CLASS) 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYA1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BYA-FIELDS
              PERFORM GET-BYB-FIELDS.

      *-----------------------------------------------------------------
       READ-BYA1-FILE-NEXT. *> EMBEDDED - BYA,BYB,LC
           PERFORM READ-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           INITIALIZE QBYA-REC.
           INITIALIZE QBYB-REC.
           INITIALIZE QLC-REC.

           EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC.

           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BY1_CURSOR INTO  
                      :QBYA-DATE-CCYYMM,
                      :QBYA-BRNO,
                      :QBYA-CLASS,
                      :QBYA-NO-1-1-1,
                      :QBYA-NO-1-1-2,
                      :QBYA-NO-1-1-3,
                      :QBYA-NO-1-1-4,
                      :QBYA-NO-1-1-5,
                      :QBYA-NO-1-1-6,
                      :QBYA-NO-1-1-7,
                      :QBYA-NO-1-1-8,
                      :QBYA-NO-1-1-9,
                      :QBYA-NO-1-1-10,
                      :QBYA-NO-1-1-11,
                      :QBYA-NO-1-2-1,
                      :QBYA-NO-1-2-2,
                      :QBYA-NO-1-2-3,
                      :QBYA-NO-1-2-4,
                      :QBYA-NO-1-2-5,
                      :QBYA-NO-1-2-6,
                      :QBYA-NO-1-2-7,
                      :QBYA-NO-1-2-8,
                      :QBYA-NO-1-2-9,
                      :QBYA-NO-1-2-10,
                      :QBYA-NO-1-2-11,
                      :QBYA-NO-2-1-1,
                      :QBYA-NO-2-1-2,
                      :QBYA-NO-2-1-3,
                      :QBYA-NO-2-1-4,
                      :QBYA-NO-2-1-5,
                      :QBYA-NO-2-1-6,
                      :QBYA-NO-2-1-7,
                      :QBYA-NO-2-1-8,
                      :QBYA-NO-2-1-9,
                      :QBYA-NO-2-1-10,
                      :QBYA-NO-2-1-11,
                      :QBYA-NO-2-2-1,
                      :QBYA-NO-2-2-2,
                      :QBYA-NO-2-2-3,
                      :QBYA-NO-2-2-4,
                      :QBYA-NO-2-2-5,
                      :QBYA-NO-2-2-6,
                      :QBYA-NO-2-2-7,
                      :QBYA-NO-2-2-8,
                      :QBYA-NO-2-2-9,
                      :QBYA-NO-2-2-10,
                      :QBYA-NO-2-2-11,
                      :QBYA-NO-3-1-1,
                      :QBYA-NO-3-1-2,
                      :QBYA-NO-3-1-3,
                      :QBYA-NO-3-1-4,
                      :QBYA-NO-3-1-5,
                      :QBYA-NO-3-1-6,
                      :QBYA-NO-3-1-7,
                      :QBYA-NO-3-1-8,
                      :QBYA-NO-3-1-9,
                      :QBYA-NO-3-1-10,
                      :QBYA-NO-3-1-11,
                      :QBYA-NO-3-2-1,
                      :QBYA-NO-3-2-2,
                      :QBYA-NO-3-2-3,
                      :QBYA-NO-3-2-4,
                      :QBYA-NO-3-2-5,
                      :QBYA-NO-3-2-6,
                      :QBYA-NO-3-2-7,
                      :QBYA-NO-3-2-8,
                      :QBYA-NO-3-2-9,
                      :QBYA-NO-3-2-10,
                      :QBYA-NO-3-2-11,
                      :QBYA-AMT-1-1-1,
                      :QBYA-AMT-1-1-2,
                      :QBYA-AMT-1-1-3,
                      :QBYA-AMT-1-1-4,
                      :QBYA-AMT-1-1-5,
                      :QBYA-AMT-1-1-6,
                      :QBYA-AMT-1-1-7,
                      :QBYA-AMT-1-1-8,
                      :QBYA-AMT-1-1-9,
                      :QBYA-AMT-1-1-10,
                      :QBYA-AMT-1-1-11,
                      :QBYA-AMT-1-2-1,
                      :QBYA-AMT-1-2-2,
                      :QBYA-AMT-1-2-3,
                      :QBYA-AMT-1-2-4,
                      :QBYA-AMT-1-2-5,
                      :QBYA-AMT-1-2-6,
                      :QBYA-AMT-1-2-7,
                      :QBYA-AMT-1-2-8,
                      :QBYA-AMT-1-2-9,
                      :QBYA-AMT-1-2-10,
                      :QBYA-AMT-1-2-11,
                      :QBYA-AMT-2-1-1,
                      :QBYA-AMT-2-1-2,
                      :QBYA-AMT-2-1-3,
                      :QBYA-AMT-2-1-4,
                      :QBYA-AMT-2-1-5,
                      :QBYA-AMT-2-1-6,
                      :QBYA-AMT-2-1-7,
                      :QBYA-AMT-2-1-8,
                      :QBYA-AMT-2-1-9,
                      :QBYA-AMT-2-1-10,
                      :QBYA-AMT-2-1-11,
                      :QBYA-AMT-2-2-1,
                      :QBYA-AMT-2-2-2,
                      :QBYA-AMT-2-2-3,
                      :QBYA-AMT-2-2-4,
                      :QBYA-AMT-2-2-5,
                      :QBYA-AMT-2-2-6,
                      :QBYA-AMT-2-2-7,
                      :QBYA-AMT-2-2-8,
                      :QBYA-AMT-2-2-9,
                      :QBYA-AMT-2-2-10,
                      :QBYA-AMT-2-2-11,
                      :QBYA-AMT-3-1-1,
                      :QBYA-AMT-3-1-2,
                      :QBYA-AMT-3-1-3,
                      :QBYA-AMT-3-1-4,
                      :QBYA-AMT-3-1-5,
                      :QBYA-AMT-3-1-6,
                      :QBYA-AMT-3-1-7,
                      :QBYA-AMT-3-1-8,
                      :QBYA-AMT-3-1-9,
                      :QBYA-AMT-3-1-10,
                      :QBYA-AMT-3-1-11,
                      :QBYA-AMT-3-2-1,
                      :QBYA-AMT-3-2-2,
                      :QBYA-AMT-3-2-3,
                      :QBYA-AMT-3-2-4,
                      :QBYA-AMT-3-2-5,
                      :QBYA-AMT-3-2-6,
                      :QBYA-AMT-3-2-7,
                      :QBYA-AMT-3-2-8,
                      :QBYA-AMT-3-2-9,
                      :QBYA-AMT-3-2-10,
                      :QBYA-AMT-3-2-11,
                      :QBYA-NO-PL-1-1-1,
                      :QBYA-NO-PL-1-1-2,
                      :QBYA-NO-PL-1-1-3,
                      :QBYA-NO-PL-1-1-4,
                      :QBYA-NO-PL-1-1-5,
                      :QBYA-NO-PL-1-1-6,
                      :QBYA-NO-PL-1-1-7,
                      :QBYA-NO-PL-1-1-8,
                      :QBYA-NO-PL-1-1-9,
                      :QBYA-NO-PL-1-1-10,
                      :QBYA-NO-PL-1-2-1,
                      :QBYA-NO-PL-1-2-2,
                      :QBYA-NO-PL-1-2-3,
                      :QBYA-NO-PL-1-2-4,
                      :QBYA-NO-PL-1-2-5,
                      :QBYA-NO-PL-1-2-6,
                      :QBYA-NO-PL-1-2-7,
                      :QBYA-NO-PL-1-2-8,
                      :QBYA-NO-PL-1-2-9,
                      :QBYA-NO-PL-1-2-10,
                      :QBYA-NO-PL-2-1-1,
                      :QBYA-NO-PL-2-1-2,
                      :QBYA-NO-PL-2-1-3,
                      :QBYA-NO-PL-2-1-4,
                      :QBYA-NO-PL-2-1-5,
                      :QBYA-NO-PL-2-1-6,
                      :QBYA-NO-PL-2-1-7,
                      :QBYA-NO-PL-2-1-8,
                      :QBYA-NO-PL-2-1-9,
                      :QBYA-NO-PL-2-1-10,
                      :QBYA-NO-PL-2-2-1,
                      :QBYA-NO-PL-2-2-2,
                      :QBYA-NO-PL-2-2-3,
                      :QBYA-NO-PL-2-2-4,
                      :QBYA-NO-PL-2-2-5,
                      :QBYA-NO-PL-2-2-6,
                      :QBYA-NO-PL-2-2-7,
                      :QBYA-NO-PL-2-2-8,
                      :QBYA-NO-PL-2-2-9,
                      :QBYA-NO-PL-2-2-10,
                      :QBYA-NO-PL-3-1-1,
                      :QBYA-NO-PL-3-1-2,
                      :QBYA-NO-PL-3-1-3,
                      :QBYA-NO-PL-3-1-4,
                      :QBYA-NO-PL-3-1-5,
                      :QBYA-NO-PL-3-1-6,
                      :QBYA-NO-PL-3-1-7,
                      :QBYA-NO-PL-3-1-8,
                      :QBYA-NO-PL-3-1-9,
                      :QBYA-NO-PL-3-1-10,
                      :QBYA-NO-PL-3-2-1,
                      :QBYA-NO-PL-3-2-2,
                      :QBYA-NO-PL-3-2-3,
                      :QBYA-NO-PL-3-2-4,
                      :QBYA-NO-PL-3-2-5,
                      :QBYA-NO-PL-3-2-6,
                      :QBYA-NO-PL-3-2-7,
                      :QBYA-NO-PL-3-2-8,
                      :QBYA-NO-PL-3-2-9,
                      :QBYA-NO-PL-3-2-10,
                      :QBYA-AMT-PL-1-1-1,
                      :QBYA-AMT-PL-1-1-2,
                      :QBYA-AMT-PL-1-1-3,
                      :QBYA-AMT-PL-1-1-4,
                      :QBYA-AMT-PL-1-1-5,
                      :QBYA-AMT-PL-1-1-6,
                      :QBYA-AMT-PL-1-1-7,
                      :QBYA-AMT-PL-1-1-8,
                      :QBYA-AMT-PL-1-1-9,
                      :QBYA-AMT-PL-1-1-10,
                      :QBYA-AMT-PL-1-2-1,
                      :QBYA-AMT-PL-1-2-2,
                      :QBYA-AMT-PL-1-2-3,
                      :QBYA-AMT-PL-1-2-4,
                      :QBYA-AMT-PL-1-2-5,
                      :QBYA-AMT-PL-1-2-6,
                      :QBYA-AMT-PL-1-2-7,
                      :QBYA-AMT-PL-1-2-8,
                      :QBYA-AMT-PL-1-2-9,
                      :QBYA-AMT-PL-1-2-10,
                      :QBYA-AMT-PL-2-1-1,
                      :QBYA-AMT-PL-2-1-2,
                      :QBYA-AMT-PL-2-1-3,
                      :QBYA-AMT-PL-2-1-4,
                      :QBYA-AMT-PL-2-1-5,
                      :QBYA-AMT-PL-2-1-6,
                      :QBYA-AMT-PL-2-1-7,
                      :QBYA-AMT-PL-2-1-8,
                      :QBYA-AMT-PL-2-1-9,
                      :QBYA-AMT-PL-2-1-10,
                      :QBYA-AMT-PL-2-2-1,
                      :QBYA-AMT-PL-2-2-2,
                      :QBYA-AMT-PL-2-2-3,
                      :QBYA-AMT-PL-2-2-4,
                      :QBYA-AMT-PL-2-2-5,
                      :QBYA-AMT-PL-2-2-6,
                      :QBYA-AMT-PL-2-2-7,
                      :QBYA-AMT-PL-2-2-8,
                      :QBYA-AMT-PL-2-2-9,
                      :QBYA-AMT-PL-2-2-10,
                      :QBYA-AMT-PL-3-1-1,
                      :QBYA-AMT-PL-3-1-2,
                      :QBYA-AMT-PL-3-1-3,
                      :QBYA-AMT-PL-3-1-4,
                      :QBYA-AMT-PL-3-1-5,
                      :QBYA-AMT-PL-3-1-6,
                      :QBYA-AMT-PL-3-1-7,
                      :QBYA-AMT-PL-3-1-8,
                      :QBYA-AMT-PL-3-1-9,
                      :QBYA-AMT-PL-3-1-10,
                      :QBYA-AMT-PL-3-2-1,
                      :QBYA-AMT-PL-3-2-2,
                      :QBYA-AMT-PL-3-2-3,
                      :QBYA-AMT-PL-3-2-4,
                      :QBYA-AMT-PL-3-2-5,
                      :QBYA-AMT-PL-3-2-6,
                      :QBYA-AMT-PL-3-2-7,
                      :QBYA-AMT-PL-3-2-8,
                      :QBYA-AMT-PL-3-2-9,
                      :QBYA-AMT-PL-3-2-10,
                      :QBYB-INS-NO-WROTE-1-1,
                      :QBYB-INS-NO-WROTE-1-2,
                      :QBYB-INS-NO-WROTE-1-3,
                      :QBYB-INS-NO-WROTE-1-4,
                      :QBYB-INS-NO-WROTE-1-5,
                      :QBYB-INS-NO-WROTE-1-6,
                      :QBYB-INS-NO-WROTE-1-7,
                      :QBYB-INS-NO-WROTE-1-8,
                      :QBYB-INS-NO-WROTE-1-9,
                      :QBYB-INS-NO-WROTE-1-10,
                      :QBYB-INS-NO-WROTE-2-1,
                      :QBYB-INS-NO-WROTE-2-2,
                      :QBYB-INS-NO-WROTE-2-3,
                      :QBYB-INS-NO-WROTE-2-4,
                      :QBYB-INS-NO-WROTE-2-5,
                      :QBYB-INS-NO-WROTE-2-6,
                      :QBYB-INS-NO-WROTE-2-7,
                      :QBYB-INS-NO-WROTE-2-8,
                      :QBYB-INS-NO-WROTE-2-9,
                      :QBYB-INS-NO-WROTE-2-10,
                      :QBYB-UNITS-1-1-1,
                      :QBYB-UNITS-1-1-2,
                      :QBYB-UNITS-1-1-3,
                      :QBYB-UNITS-1-1-4,
                      :QBYB-UNITS-1-1-5,
                      :QBYB-UNITS-1-1-6,
                      :QBYB-UNITS-1-1-7,
                      :QBYB-UNITS-1-1-8,
                      :QBYB-UNITS-1-1-9,
                      :QBYB-UNITS-1-1-10,
                      :QBYB-UNITS-1-2-1,
                      :QBYB-UNITS-1-2-2,
                      :QBYB-UNITS-1-2-3,
                      :QBYB-UNITS-1-2-4,
                      :QBYB-UNITS-1-2-5,
                      :QBYB-UNITS-1-2-6,
                      :QBYB-UNITS-1-2-7,
                      :QBYB-UNITS-1-2-8,
                      :QBYB-UNITS-1-2-9,
                      :QBYB-UNITS-1-2-10,
                      :QBYB-UNITS-2-1-1,
                      :QBYB-UNITS-2-1-2,
                      :QBYB-UNITS-2-1-3,
                      :QBYB-UNITS-2-1-4,
                      :QBYB-UNITS-2-1-5,
                      :QBYB-UNITS-2-1-6,
                      :QBYB-UNITS-2-1-7,
                      :QBYB-UNITS-2-1-8,
                      :QBYB-UNITS-2-1-9,
                      :QBYB-UNITS-2-1-10,
                      :QBYB-UNITS-2-2-1,
                      :QBYB-UNITS-2-2-2,
                      :QBYB-UNITS-2-2-3,
                      :QBYB-UNITS-2-2-4,
                      :QBYB-UNITS-2-2-5,
                      :QBYB-UNITS-2-2-6,
                      :QBYB-UNITS-2-2-7,
                      :QBYB-UNITS-2-2-8,
                      :QBYB-UNITS-2-2-9,
                      :QBYB-UNITS-2-2-10,
                      :QBYB-UNITS-3-1-1,
                      :QBYB-UNITS-3-1-2,
                      :QBYB-UNITS-3-1-3,
                      :QBYB-UNITS-3-1-4,
                      :QBYB-UNITS-3-1-5,
                      :QBYB-UNITS-3-1-6,
                      :QBYB-UNITS-3-1-7,
                      :QBYB-UNITS-3-1-8,
                      :QBYB-UNITS-3-1-9,
                      :QBYB-UNITS-3-1-10,
                      :QBYB-UNITS-3-2-1,
                      :QBYB-UNITS-3-2-2,
                      :QBYB-UNITS-3-2-3,
                      :QBYB-UNITS-3-2-4,
                      :QBYB-UNITS-3-2-5,
                      :QBYB-UNITS-3-2-6,
                      :QBYB-UNITS-3-2-7,
                      :QBYB-UNITS-3-2-8,
                      :QBYB-UNITS-3-2-9,
                      :QBYB-UNITS-3-2-10,
                      :QBYB-OUTBAL-1-1-1,
                      :QBYB-OUTBAL-1-1-2,
                      :QBYB-OUTBAL-1-1-3,
                      :QBYB-OUTBAL-1-1-4,
                      :QBYB-OUTBAL-1-1-5,
                      :QBYB-OUTBAL-1-1-6,
                      :QBYB-OUTBAL-1-1-7,
                      :QBYB-OUTBAL-1-1-8,
                      :QBYB-OUTBAL-1-1-9,
                      :QBYB-OUTBAL-1-1-10,
                      :QBYB-OUTBAL-1-2-1,
                      :QBYB-OUTBAL-1-2-2,
                      :QBYB-OUTBAL-1-2-3,
                      :QBYB-OUTBAL-1-2-4,
                      :QBYB-OUTBAL-1-2-5,
                      :QBYB-OUTBAL-1-2-6,
                      :QBYB-OUTBAL-1-2-7,
                      :QBYB-OUTBAL-1-2-8,
                      :QBYB-OUTBAL-1-2-9,
                      :QBYB-OUTBAL-1-2-10,
                      :QBYB-OUTBAL-2-1-1,
                      :QBYB-OUTBAL-2-1-2,
                      :QBYB-OUTBAL-2-1-3,
                      :QBYB-OUTBAL-2-1-4,
                      :QBYB-OUTBAL-2-1-5,
                      :QBYB-OUTBAL-2-1-6,
                      :QBYB-OUTBAL-2-1-7,
                      :QBYB-OUTBAL-2-1-8,
                      :QBYB-OUTBAL-2-1-9,
                      :QBYB-OUTBAL-2-1-10,
                      :QBYB-OUTBAL-2-2-1,
                      :QBYB-OUTBAL-2-2-2,
                      :QBYB-OUTBAL-2-2-3,
                      :QBYB-OUTBAL-2-2-4,
                      :QBYB-OUTBAL-2-2-5,
                      :QBYB-OUTBAL-2-2-6,
                      :QBYB-OUTBAL-2-2-7,
                      :QBYB-OUTBAL-2-2-8,
                      :QBYB-OUTBAL-2-2-9,
                      :QBYB-OUTBAL-2-2-10,
                      :QBYB-OUTBAL-3-1-1,
                      :QBYB-OUTBAL-3-1-2,
                      :QBYB-OUTBAL-3-1-3,
                      :QBYB-OUTBAL-3-1-4,
                      :QBYB-OUTBAL-3-1-5,
                      :QBYB-OUTBAL-3-1-6,
                      :QBYB-OUTBAL-3-1-7,
                      :QBYB-OUTBAL-3-1-8,
                      :QBYB-OUTBAL-3-1-9,
                      :QBYB-OUTBAL-3-1-10,
                      :QBYB-OUTBAL-3-2-1,
                      :QBYB-OUTBAL-3-2-2,
                      :QBYB-OUTBAL-3-2-3,
                      :QBYB-OUTBAL-3-2-4,
                      :QBYB-OUTBAL-3-2-5,
                      :QBYB-OUTBAL-3-2-6,
                      :QBYB-OUTBAL-3-2-7,
                      :QBYB-OUTBAL-3-2-8,
                      :QBYB-OUTBAL-3-2-9,
                      :QBYB-OUTBAL-3-2-10,
                      :QLC-CODE, 
                      :QLC-SMALLOAN,
                      :QLC-SALEFIN,
                      :QLC-SERVACCT,
                      :QLC-LOANTYPE,
                      :QLC-AFP-CLASS,
                      :QLC-GIFTNET

              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BYA-FIELDS
              PERFORM GET-BYB-FIELDS 
              PERFORM GET-LC-FIELDS.
  
          EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-BYA1-FILE.
           PERFORM CLOSE-IT.

           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BY1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO BYA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-BY1-CONNECTION.

           IF BY1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT BY1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO BY1-CONN-OPEN-SW.


       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".


      * COPY "LIBLP/LPTY1RN.CPY".
      * COPYMEMBER: LIBLP/LPTY1RN
      ***************************************************
       LOAD-TY1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TY-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TY-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TY-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TY-ALL-DATA
              MOVE TY-ALL-PATH             TO TY-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TY-ALL-BASE
              MOVE FILPATH-MACHINE         TO TY-ALL-MACHINE
              MOVE FILPATH-DATA            TO TY-ALL-DATA
              MOVE TY-ALL-PATH             TO TY-PATH.

      *-----------------------------------------------------------------
       OPEN-TY1-FILE.
           PERFORM LOAD-TY1-FILE.
           PERFORM OPEN-IT.
           MOVE TY-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-TY1-FILE. *> EMBEDDED - TYFILE
           PERFORM READ-IT.
           MOVE TY-PATH-OWNBR  TO TY-BRNO. 
           MOVE TY-PATH-SQL    TO E-FILE.
           MOVE TY1-KEY        TO E-KEYX.

           MOVE TY-BRNO             TO QTY-BRNO.
           MOVE TY-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTY-DATE.
           MOVE TY-CLASS           TO QTY-CLASS.

           EXEC SQL
            SELECT TYFILE.TY_BRNO,
                   ISNULL(CAST(TYFILE.TY_DATE AS VARCHAR(10)),'0'),
                   TYFILE.TY_CLASS,
                   TYFILE.TY_NO_1_1,
                   TYFILE.TY_NO_1_2,
                   TYFILE.TY_NO_1_3,
                   TYFILE.TY_NO_1_4,
                   TYFILE.TY_NO_1_5,
                   TYFILE.TY_NO_1_6,
                   TYFILE.TY_NO_1_7,
                   TYFILE.TY_NO_1_8,
                   TYFILE.TY_NO_1_9,
                   TYFILE.TY_NO_1_10,
                   TYFILE.TY_NO_1_11,
                   TYFILE.TY_NO_2_1,
                   TYFILE.TY_NO_2_2,
                   TYFILE.TY_NO_2_3,
                   TYFILE.TY_NO_2_4,
                   TYFILE.TY_NO_2_5,
                   TYFILE.TY_NO_2_6,
                   TYFILE.TY_NO_2_7,
                   TYFILE.TY_NO_2_8,
                   TYFILE.TY_NO_2_9,
                   TYFILE.TY_NO_2_10,
                   TYFILE.TY_NO_2_11,
                   TYFILE.TY_AMT_1_1,
                   TYFILE.TY_AMT_1_2,
                   TYFILE.TY_AMT_1_3,
                   TYFILE.TY_AMT_1_4,
                   TYFILE.TY_AMT_1_5,
                   TYFILE.TY_AMT_1_6,
                   TYFILE.TY_AMT_1_7,
                   TYFILE.TY_AMT_1_8,
                   TYFILE.TY_AMT_1_9,
                   TYFILE.TY_AMT_1_10,
                   TYFILE.TY_AMT_1_11,
                   TYFILE.TY_AMT_2_1,
                   TYFILE.TY_AMT_2_2,
                   TYFILE.TY_AMT_2_3,
                   TYFILE.TY_AMT_2_4,
                   TYFILE.TY_AMT_2_5,
                   TYFILE.TY_AMT_2_6,
                   TYFILE.TY_AMT_2_7,
                   TYFILE.TY_AMT_2_8,
                   TYFILE.TY_AMT_2_9,
                   TYFILE.TY_AMT_2_10,
                   TYFILE.TY_AMT_2_11,
                   TYFILE.TY_NO_PL_1_1,
                   TYFILE.TY_NO_PL_1_2,
                   TYFILE.TY_NO_PL_1_3,
                   TYFILE.TY_NO_PL_1_4,
                   TYFILE.TY_NO_PL_1_5,
                   TYFILE.TY_NO_PL_1_6,
                   TYFILE.TY_NO_PL_1_7,
                   TYFILE.TY_NO_PL_1_8,
                   TYFILE.TY_NO_PL_1_9,
                   TYFILE.TY_NO_PL_1_10,
                   TYFILE.TY_NO_PL_2_1,
                   TYFILE.TY_NO_PL_2_2,
                   TYFILE.TY_NO_PL_2_3,
                   TYFILE.TY_NO_PL_2_4,
                   TYFILE.TY_NO_PL_2_5,
                   TYFILE.TY_NO_PL_2_6,
                   TYFILE.TY_NO_PL_2_7,
                   TYFILE.TY_NO_PL_2_8,
                   TYFILE.TY_NO_PL_2_9,
                   TYFILE.TY_NO_PL_2_10,
                   TYFILE.TY_AMT_PL_1_1,
                   TYFILE.TY_AMT_PL_1_2,
                   TYFILE.TY_AMT_PL_1_3,
                   TYFILE.TY_AMT_PL_1_4,
                   TYFILE.TY_AMT_PL_1_5,
                   TYFILE.TY_AMT_PL_1_6,
                   TYFILE.TY_AMT_PL_1_7,
                   TYFILE.TY_AMT_PL_1_8,
                   TYFILE.TY_AMT_PL_1_9,
                   TYFILE.TY_AMT_PL_1_10,
                   TYFILE.TY_AMT_PL_2_1,
                   TYFILE.TY_AMT_PL_2_2,
                   TYFILE.TY_AMT_PL_2_3,
                   TYFILE.TY_AMT_PL_2_4,
                   TYFILE.TY_AMT_PL_2_5,
                   TYFILE.TY_AMT_PL_2_6,
                   TYFILE.TY_AMT_PL_2_7,
                   TYFILE.TY_AMT_PL_2_8,
                   TYFILE.TY_AMT_PL_2_9,
                   TYFILE.TY_AMT_PL_2_10,
                   TYFILE.TY_INS_NO_WROTE_1,
                   TYFILE.TY_INS_NO_WROTE_2,
                   TYFILE.TY_INS_NO_WROTE_3,
                   TYFILE.TY_INS_NO_WROTE_4,
                   TYFILE.TY_INS_NO_WROTE_5,
                   TYFILE.TY_INS_NO_WROTE_6,
                   TYFILE.TY_INS_NO_WROTE_7,
                   TYFILE.TY_INS_NO_WROTE_8,
                   TYFILE.TY_INS_NO_WROTE_9,
                   TYFILE.TY_INS_NO_WROTE_10,
                   TYFILE.TY_UNITS_1_1,
                   TYFILE.TY_UNITS_1_2,
                   TYFILE.TY_UNITS_1_3,
                   TYFILE.TY_UNITS_1_4,
                   TYFILE.TY_UNITS_1_5,
                   TYFILE.TY_UNITS_1_6,
                   TYFILE.TY_UNITS_1_7,
                   TYFILE.TY_UNITS_1_8,
                   TYFILE.TY_UNITS_1_9,
                   TYFILE.TY_UNITS_1_10,
                   TYFILE.TY_UNITS_2_1,
                   TYFILE.TY_UNITS_2_2,
                   TYFILE.TY_UNITS_2_3,
                   TYFILE.TY_UNITS_2_4,
                   TYFILE.TY_UNITS_2_5,
                   TYFILE.TY_UNITS_2_6,
                   TYFILE.TY_UNITS_2_7,
                   TYFILE.TY_UNITS_2_8,
                   TYFILE.TY_UNITS_2_9,
                   TYFILE.TY_UNITS_2_10,
                   TYFILE.TY_OUTBAL_1_1,
                   TYFILE.TY_OUTBAL_1_2,
                   TYFILE.TY_OUTBAL_1_3,
                   TYFILE.TY_OUTBAL_1_4,
                   TYFILE.TY_OUTBAL_1_5,
                   TYFILE.TY_OUTBAL_1_6,
                   TYFILE.TY_OUTBAL_1_7,
                   TYFILE.TY_OUTBAL_1_8,
                   TYFILE.TY_OUTBAL_1_9,
                   TYFILE.TY_OUTBAL_1_10,
                   TYFILE.TY_OUTBAL_2_1,
                   TYFILE.TY_OUTBAL_2_2,
                   TYFILE.TY_OUTBAL_2_3,
                   TYFILE.TY_OUTBAL_2_4,
                   TYFILE.TY_OUTBAL_2_5,
                   TYFILE.TY_OUTBAL_2_6,
                   TYFILE.TY_OUTBAL_2_7,
                   TYFILE.TY_OUTBAL_2_8,
                   TYFILE.TY_OUTBAL_2_9,
                   TYFILE.TY_OUTBAL_2_10,
                   TYFILE.TY_BRFG
              INTO 
                   :QTY-BRNO,
                   :QTY-DATE,
                   :QTY-CLASS,
                   :QTY-NO-1-1,
                   :QTY-NO-1-2,
                   :QTY-NO-1-3,
                   :QTY-NO-1-4,
                   :QTY-NO-1-5,
                   :QTY-NO-1-6,
                   :QTY-NO-1-7,
                   :QTY-NO-1-8,
                   :QTY-NO-1-9,
                   :QTY-NO-1-10,
                   :QTY-NO-1-11,
                   :QTY-NO-2-1,
                   :QTY-NO-2-2,
                   :QTY-NO-2-3,
                   :QTY-NO-2-4,
                   :QTY-NO-2-5,
                   :QTY-NO-2-6,
                   :QTY-NO-2-7,
                   :QTY-NO-2-8,
                   :QTY-NO-2-9,
                   :QTY-NO-2-10,
                   :QTY-NO-2-11,
                   :QTY-AMT-1-1,
                   :QTY-AMT-1-2,
                   :QTY-AMT-1-3,
                   :QTY-AMT-1-4,
                   :QTY-AMT-1-5,
                   :QTY-AMT-1-6,
                   :QTY-AMT-1-7,
                   :QTY-AMT-1-8,
                   :QTY-AMT-1-9,
                   :QTY-AMT-1-10,
                   :QTY-AMT-1-11,
                   :QTY-AMT-2-1,
                   :QTY-AMT-2-2,
                   :QTY-AMT-2-3,
                   :QTY-AMT-2-4,
                   :QTY-AMT-2-5,
                   :QTY-AMT-2-6,
                   :QTY-AMT-2-7,
                   :QTY-AMT-2-8,
                   :QTY-AMT-2-9,
                   :QTY-AMT-2-10,
                   :QTY-AMT-2-11,
                   :QTY-NO-PL-1-1,
                   :QTY-NO-PL-1-2,
                   :QTY-NO-PL-1-3,
                   :QTY-NO-PL-1-4,
                   :QTY-NO-PL-1-5,
                   :QTY-NO-PL-1-6,
                   :QTY-NO-PL-1-7,
                   :QTY-NO-PL-1-8,
                   :QTY-NO-PL-1-9,
                   :QTY-NO-PL-1-10,
                   :QTY-NO-PL-2-1,
                   :QTY-NO-PL-2-2,
                   :QTY-NO-PL-2-3,
                   :QTY-NO-PL-2-4,
                   :QTY-NO-PL-2-5,
                   :QTY-NO-PL-2-6,
                   :QTY-NO-PL-2-7,
                   :QTY-NO-PL-2-8,
                   :QTY-NO-PL-2-9,
                   :QTY-NO-PL-2-10,
                   :QTY-AMT-PL-1-1,
                   :QTY-AMT-PL-1-2,
                   :QTY-AMT-PL-1-3,
                   :QTY-AMT-PL-1-4,
                   :QTY-AMT-PL-1-5,
                   :QTY-AMT-PL-1-6,
                   :QTY-AMT-PL-1-7,
                   :QTY-AMT-PL-1-8,
                   :QTY-AMT-PL-1-9,
                   :QTY-AMT-PL-1-10,
                   :QTY-AMT-PL-2-1,
                   :QTY-AMT-PL-2-2,
                   :QTY-AMT-PL-2-3,
                   :QTY-AMT-PL-2-4,
                   :QTY-AMT-PL-2-5,
                   :QTY-AMT-PL-2-6,
                   :QTY-AMT-PL-2-7,
                   :QTY-AMT-PL-2-8,
                   :QTY-AMT-PL-2-9,
                   :QTY-AMT-PL-2-10,
                   :QTY-INS-NO-WROTE-1,
                   :QTY-INS-NO-WROTE-2,
                   :QTY-INS-NO-WROTE-3,
                   :QTY-INS-NO-WROTE-4,
                   :QTY-INS-NO-WROTE-5,
                   :QTY-INS-NO-WROTE-6,
                   :QTY-INS-NO-WROTE-7,
                   :QTY-INS-NO-WROTE-8,
                   :QTY-INS-NO-WROTE-9,
                   :QTY-INS-NO-WROTE-10,
                   :QTY-UNITS-1-1,
                   :QTY-UNITS-1-2,
                   :QTY-UNITS-1-3,
                   :QTY-UNITS-1-4,
                   :QTY-UNITS-1-5,
                   :QTY-UNITS-1-6,
                   :QTY-UNITS-1-7,
                   :QTY-UNITS-1-8,
                   :QTY-UNITS-1-9,
                   :QTY-UNITS-1-10,
                   :QTY-UNITS-2-1,
                   :QTY-UNITS-2-2,
                   :QTY-UNITS-2-3,
                   :QTY-UNITS-2-4,
                   :QTY-UNITS-2-5,
                   :QTY-UNITS-2-6,
                   :QTY-UNITS-2-7,
                   :QTY-UNITS-2-8,
                   :QTY-UNITS-2-9,
                   :QTY-UNITS-2-10,
                   :QTY-OUTBAL-1-1,
                   :QTY-OUTBAL-1-2,
                   :QTY-OUTBAL-1-3,
                   :QTY-OUTBAL-1-4,
                   :QTY-OUTBAL-1-5,
                   :QTY-OUTBAL-1-6,
                   :QTY-OUTBAL-1-7,
                   :QTY-OUTBAL-1-8,
                   :QTY-OUTBAL-1-9,
                   :QTY-OUTBAL-1-10,
                   :QTY-OUTBAL-2-1,
                   :QTY-OUTBAL-2-2,
                   :QTY-OUTBAL-2-3,
                   :QTY-OUTBAL-2-4,
                   :QTY-OUTBAL-2-5,
                   :QTY-OUTBAL-2-6,
                   :QTY-OUTBAL-2-7,
                   :QTY-OUTBAL-2-8,
                   :QTY-OUTBAL-2-9,
                   :QTY-OUTBAL-2-10,
                   :QTY-BRFG
              FROM DBO.TYFILE
             WHERE TYFILE.TY_BRNO  = :QTY-BRNO
               AND TYFILE.TY_DATE  = :QTY-DATE
               AND TYFILE.TY_CLASS = :QTY-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TY1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TY-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TY1-FILE.

           PERFORM CLOSE-IT.
 
 
      * COPY "LIBGB/GBRC2RN.CPY".
      * COPYMEMBER: LIBGB/GBRC2RN
       LOAD-RC2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( RC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( RC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO RC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO RC-ALL-MACHINE
              MOVE RC-ALL-PATH             TO RC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( RC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO RC-ALL-BASE
              MOVE FILPATH-MACHINE         TO RC-ALL-MACHINE
              MOVE RC-ALL-PATH             TO RC-PATH.

      *-----------------------------------------------------------------
       OPEN-RC2-FILE.
           PERFORM LOAD-RC2-FILE.
           PERFORM OPEN-IT.
           MOVE RC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-RC2-FILE. *> EMBEDDED - RCFILE
           PERFORM START-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC2-KEY TO E-KEYX.

           IF ( RC2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-RC2-FILE.

           IF ( QRC2-WEND-STATUS = SPACES )
              MOVE RC-STATUS            TO QRC2-WBEG-STATUS
              MOVE ALL "Z"              TO QRC2-WEND-STATUS
              MOVE RC-BRNO              TO QRC2-WBEG-BRNO
              MOVE ALL "9"              TO QRC2-WEND-BRNO

              MOVE RC-TRANS-DATE        TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WBEG-TRANS-DATE

              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WEND-TRANS-DATE.

           EXEC SQL
            DECLARE RC2_CURSOR CURSOR FOR
            SELECT RCFILE.RC_STATUS,
                   RCFILE.RC_BRNO,
                   CAST(RCFILE.RC_TRANS_DATE AS VARCHAR(10)) 
              FROM DBO.RCFILE
             WHERE RCFILE.RC_STATUS     >= :QRC2-WBEG-STATUS
               AND RCFILE.RC_STATUS     <= :QRC2-WEND-STATUS
               AND RCFILE.RC_BRNO       >= :QRC2-WBEG-BRNO
               AND RCFILE.RC_BRNO       <= :QRC2-WEND-BRNO
               AND RCFILE.RC_TRANS_DATE >= :QRC2-WBEG-TRANS-DATE
               AND RCFILE.RC_TRANS_DATE <= :QRC2-WEND-TRANS-DATE
             ORDER BY RCFILE.RC_STATUS,
                      RCFILE.RC_BRNO,
                      RCFILE.RC_TRANS_DATE
           END-EXEC.

           EXEC SQL
               OPEN RC2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QRC2-WBEG-STATUS
                          QRC2-WEND-STATUS
                          QRC2-WBEG-TRANS-DATE
                          QRC2-WEND-TRANS-DATE.

           MOVE ZEROES TO QRC2-WBEG-BRNO
                          QRC2-WEND-BRNO.

           MOVE STAT---GOOD TO RC2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-RC2-FILE-NEXT. *> EMBEDDED - RCFILE
           PERFORM READ-IT.
           MOVE RC-PATH-SQL    TO E-FILE.
           MOVE RC2-KEY        TO E-KEYX.
           INITIALIZE QRC-REC.

           IF ( RC2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH RC2_CURSOR 
                    INTO  
                      :QRC-STATUS, 
                      :QRC-BRNO,
                      :QRC-TRANS-DATE
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RC2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-RC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-RC2-FILE.
           PERFORM CLOSE-IT.
           IF ( RC2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE RC2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO RC2-CURSOR-STAT.


      *- PLACE CLEAR OF TY HERE TO HELP REDUCE COLUMNS RETURN VIA
      *- SHELL EXTRACTING COLUMNS USED
       COPY "LIBLP/LPTYCL.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF ( SKIP-RECORDS-FOUND )
              GO TO EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF SV-BRANCH NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BRANCH TO BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BRANCH TO BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY OPREPT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
