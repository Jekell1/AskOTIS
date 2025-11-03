       IDENTIFICATION DIVISION.
       PROGRAM-ID.      OPREP2.
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  WORLD OPERATIONS WEEKLY VERSION
      *
      * REV:
      *  BLV 072297 FIX TODAYS TOTALS ADDING TO BEG MO GAIN/LOSS &
      *             DELINQUENCY
      *  BLV 081297 ADDED BY-STD-POS: LOAN VOLUME VS STANDARD POSITION
      *  BLV 090397 ADDED D3-VOLUME-PCT: VOL / BEG MO OUTSTANDING
      *  BLV 100797 ADDED D3-DEL-PCT D3-REC-30-AMT
      *  BLV 022398 FIX PREV-SCURR TABLE TO COMP
      *  BLV 030398 ADD BR-MACHINE PATH LOGIC
      *  BLV 031598 ADD GIFTNET REPO # & $ TO REPORT
      *  MJD 041798 NOT CHECKING EXT-ROUTE-BUF BEFORE CALCULATING DATE.
      *             DISPLAYING "(P2)" IN DATE FIELD WHEN IN EOMAIN.
      *  BLV 042198 FIX BEG MONTH DELINQUENCY OPTION
      *  BLV 042498 CHANGE BEG MONTH DELINQUENCY OPTION TO GATHER
      *             BEGMO & BEGMO FROM NEXT MONTHS STATS (NOT CUR & BEGMO)
      *  BLV 052998 FIX TO DISPLAY TOTAL LEVEL FLAGS IN EOMAIN
      *  BLV 060298 FIX TO USE TY-ALL-PATH; USE RC FIRST-OPEN-DAY LOGIC; FIX
      *             TO SKIP TODAYS TOTALS IF NOT RUNNING FOR CURRENT MONTH
      *  BLV 070998 FIX INCORRECT # PRINT FOR OTHER VOLUME; ALSO CLEAN UP
      *             SUBSCRIPTING ON TOTAL VOLUME
      *  BLV 990909 PRINT BLANK LINE AT END SO YOU CAN SEE LAST LINE IN
      *             PERUSE
      *  BLV 000207 CHANGE FIRST-OPEN-DATE FROM 6 TO 7 DIGITS - WASN'T
      *             PICKING UP TY ACTIVITY SINCE NEW CENTURY
      *  BLV 000315 FIX TY-NO GETTING ADDED INSTEAD OF TY-NO-PL
      *  BLV 000804 FIX GROUP PROBLEM IF INCONSISTENT LEVELS IN GROUP
      *             HIERARCHY, SEE GROUP.BUG IN /USR/A0102B/WORLD
      *  BLV 010719 DON'T PRINT "DEL RED %", ADD 30 DAY REMAINING %,
      *             WORLD PR# 273
      *  BLV 021122 SUBTRACT P2'S FROM P&L ACCUMULATIONS FOR WORLD -
      *             THEY GET USED IN INCREASE REMAINING CALCULATION
      *  CS  031113 IF GP-NO-SC-USE-DM IS 'Y' (WORLD) THE FIRST LINE OF
      *             NB (NB + OT) 2ND LINE IS DM  [WORLD]
      *  BLF 041122 ELIMINATE GIFTNET MTD REPO AMOUNT AND REPLACE WITH
      *             MTD P&L AMT, WORLD PR# 363
      *  BAH 051222 H-TITLE PRINT EO-COMMENT WHICH IS PASSED THRU
      *             EXT-EOBUF-MES FROM EOEXEC, WORLD PR# 449
      *  BLF 061114 EXPANDED ALL GROUP SUBSCRIPTS TO ACCOMODATE
      *             EXPANDED GROUP SEARCH TABLES, WORLD PR# 502
      *  BLM 111221 INCREASED WORKERS FOR BALANCE & A FEW OTHER FIELDS
      *             TO ACCOMODATE BILLIONS, WORLD PR# 780, ADDENDUM
      *  BLM 120313 CHANGED GROUP INFO, LINE 1 CHANGE FROM GR-NAME TO
      *             GR-ID, LINE 2 CHANGED FROM GR-CONTRACT TO GR-NAME,
      *             WORLD PR# 796
      *  BLM 120315 ADD "GROUP:" PROMPT FOR CHANGE ABOVE, WORLD PR# 796
      *
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *  BLM 151208 CHANGE DEL % TO BE JUST POTS & 30S INSTEAD OF
      *             EVERYTHING EXCEPT CURRENT, WORLD PR# 1125
      * KEC 2016.0311 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-LV???? TO CD-LV-????
      *  BLM 160314 FIX LOGIC FOR NEW BY-DATE FORMAT, PD0003
      *
      * KEC 2016.0929 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED BY-DATE TO BY-DATE-CCYYMM FOR CONSISTENCY.
      *
      * BAH 170707 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181018 GLOBAL TYFILE
      * BAH 181018 GLOBAL RCFILE
      * KEC 190212 A32 F2SL - REVISED FOR SPLITTING THE BYFILE.
      *            REMOVED LOAN APP FIELDS IN BY & TY.
      * BAH 20190924 REMOVED ACCESS-CALL RCFILE,TYFILE
      * CS 2021.0114 ALLOW FOR CLASS TO HAVE ALPHA INPUT OF 'S' SALES
      *              'L'LOAN CLASSES,'A'WORLD SMALL CLASSES, 'W'WCBC,
      *              'O' WORLD OTHER CLASSES,'F' WORLD SF CLASSES, 'M'
      *               SMALL-LOAN-CLASSES, 'R' NON SMALL LN-CLASSES,
      *              'I' IB CLASSES PR# 1485
      * RMZ 20230802 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      * RMZ 20240116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
      ********************************************************************
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/OPREP2.CBL S35 12/22/23-11:39:16 >".

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
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
      *-----------------------------------------------------------------

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
       01  HIGH-LEVEL           PIC 9            VALUE 1.
       01  PRINTED-DASHES       PIC X            VALUE "N".
       01  TOT-SUB              PIC 99.
       01  GROUP-SUB            PIC 9999.
       01  PREV-SUB             PIC 9999.
       01  TYPE-SUB             PIC 99.
       01  DEL-SUB              PIC 99.
       01  PER-SUB1             PIC 99.
       01  PER-SUB2             PIC 99.
       01  AGE-IN               PIC 99.
       01  AGE-OUT              PIC 99.
       01  REC-SUB              PIC 99.
       01  PL-SUB               PIC 99.
       01  HEADING-FG           PIC X VALUE "Y".
       01  NEW-BRANCH-FG        PIC X VALUE "Y".
       01  NAME-WK              PIC X(20) VALUE SPACES.

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
           03  HOLD-BY-DATE-CCYYMM   PIC 9(6).
           03  HOLD-BY-BRNO          PIC 9(4).
           03  HOLD-BY-CLASS         PIC 9(3).
       01  CUR-MO-BY-DATE-CCYYMM     PIC 9(6).
       01  NEXT-MO-BY-DATE-CCYYMM    PIC 9(6).

       01  LEVEL-QUESTION.
           03  FILLER           PIC X(17) VALUE
               "PRINT TOTALS FOR ".
           03  LEVEL-DESC       PIC X(21).
           03  LEVEL-QUES2      PIC X(7).


       01  TOTAL-TABLE.
           03  TOTAL-TAB        OCCURS 10.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SECTION 1 - GAIN/LOSS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  GAIN-LOSS-NO-CUR     PIC S9(8).
               05  GAIN-LOSS-AMT-CUR    PIC S9(11)V99.
               05  GAIN-LOSS-NO-MTD     PIC S9(8).
               05  GAIN-LOSS-AMT-MTD    PIC S9(11)V99.
               05  GAIN-LOSS-NO-YTD     PIC S9(8).
               05  GAIN-LOSS-AMT-YTD    PIC S9(11)V99.
               05  NO-FT-EMP            PIC S9(9)V99.
               05  NO-PT-EMP            PIC S9(9)V99.
               05  PROJ-PROFIT          PIC S9(11)V99.
               05  PROJ-INCREASE        PIC S9(11)V99.
               05  30-DAY-MOVED         PIC S9(9)V99.
               05  STD-POS              PIC S9(11)V99.
               05  GN-REPO-AMT          PIC S9(9)V99.
               05  GN-REPO-NO           PIC S9(8).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SECTION 2 -  VOLUME (SUB2: 1= MTD, 2= YTD)
      *                      (SUB3: 1-9=TYPE, 10 = TOTAL)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  VOLUME-TOTS OCCURS 2.
                   07  VOLUME-TYPES OCCURS 10.
                       09  VOL-NO-APPS      PIC S9(8).
                       09  VOL-NO           PIC S9(8).
                       09  VOL-AMT          PIC S9(11)V99.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SECTION 3 - DELINQUENCY-RECENCY
      *                      (SUB 2: 1=REC, 2= CONTRACTUAL)
      *                      (SUB 3: 1=30, 2=60, 3=90+120+150, 4=180+210
      *                              5=DEL SUB-TOTAL(TOT OF 1-4), 6=MISPAYS,
      *                              7= 30 DAY POTS, 8=TOTAL SLOW
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  DELINQUINCY-TOTS OCCURS 2.
                   07  DELINQUINCY-TYPES OCCURS 8.
                       09  DEL-NO-CUR        PIC S9(8).
                       09  DEL-AMT-CUR       PIC S9(9)V99.
                       09  DEL-NO-MTD        PIC S9(8).
                       09  DEL-AMT-MTD       PIC S9(9)V99.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SECTION 4 - CHARGE-OFFS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  PL-AMT-MTD                PIC S9(9)V99.
               05  PL-NO-MTD                 PIC S9(8).
               05  PL-AMT-YTD                PIC S9(9)V99.
               05  PL-NO-YTD                 PIC S9(8).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SECTION 5 - INSURANCE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  INS-TABLE                 OCCURS 6.
                   07  INS-NO-MTD            PIC S9(8).

       01  PREV-SCURR-TABLE.
           03  PREV-SCURR       PIC 9999 COMP OCCURS 15.

       01  FIRST-TIME           PIC X        VALUE "Y".

       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".

       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  DISPLAY-DATE             PIC 9(4).
       01  DISPLAY-DATEX            REDEFINES DISPLAY-DATE.
           03  DISPLAY-MO           PIC 99.
           03  DISPLAY-YR           PIC 99.
       01  DATE-LIST                PIC X(5)  VALUE "MMYY.".
       01  FIRST-OPEN-DATE          PIC 9(8)  VALUE 0.

       01  NO-WORK              PIC S9(7)     VALUE +0.
       01  AMT-WORK             PIC S9(10)V99 VALUE +0.
       01  PCT-WORK             PIC S9(3)V99  VALUE +0.
       01  ROUNDING-WK          PIC S9(11)    VALUE +0.


       01  STAR-LINE.
           03  FILLER          PIC X(40) VALUE
           "________________________________________".
           03  FILLER          PIC X(40) VALUE
           "________________________________________".
           03  FILLER          PIC X(40) VALUE
           "________________________________________".
           03  FILLER          PIC X(12) VALUE
           "____________".

       01  DASH-LINE.
           03  FILLER          PIC X(40) VALUE
           "----------------------------------------".
           03  FILLER          PIC X(40) VALUE
           "----------------------------------------".
           03  FILLER          PIC X(40) VALUE
           "----------------------------------------".
           03  FILLER          PIC X(12) VALUE
           "------------".

      *=================================================================
      *
      *     S C R E E N - F I E L D S
      *
      *=================================================================
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  STORE-DATE      PIC 9(4).
           03  STORE-DATEX REDEFINES STORE-DATE.
               05  STORE-MO    PIC 99.
               05  STORE-YR    PIC 99.
           03  BEG-CLASS       PIC 9(3).
           03  BEG-CLASSX REDEFINES BEG-CLASS
                                 PIC X(3).
           03  END-CLASS       PIC 9(3).
           03  END-CLASSX  REDEFINES END-CLASS
                               PIC X(3).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  BRANCH          PIC 9999.
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
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(28)     VALUE " ".
           03  H-TITLE          PIC X(33) VALUE
           "- -  SUMMARY PROGRESS REPORT  - -".
           03  FILLER           PIC X(2)      VALUE " ".
           03  H-MMYY           PIC 99/99.
           03  FILLER           PIC X(51)     VALUE " ".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(44) VALUE
           "BRANCH # /      #REF     # NL     TOT # LNS ".
           03  FILLER           PIC X(32) VALUE
           " NO ACCTS    # FT       BALANCE ".
           03  FILLER           PIC X(21) VALUE
           "  # POTS   INC REM $ ".
           03  FILLER           PIC X(24) VALUE
           " 30+ REMAIN #  30 MOVED ".
           03  FILLER           PIC X(11) VALUE
           "    # SLOW ".

      *-----------------------------------------------------------------
      *01  HEAD4.
      *    03  FILLER           PIC X(44) VALUE
      *    "SUPERVISOR      # FB     # OT        VOLUME ".
      *    03  FILLER           PIC X(32) VALUE
      *    " MTD GAIN    # PT     GAIN/LOSS ".
      *    03  FILLER           PIC X(21) VALUE
      *    "  % POTS   INC REM % ".
      *    03  FILLER           PIC X(24) VALUE
      *    " 30+ REMAIN $     30+ % ".
      *    03  FILLER           PIC X(11) VALUE
      *    "    % SLOW ".
      *--------------------------------------------------------
       01  HEAD4.
      *    03  FILLER           PIC X(44) VALUE
      *    "SUPERVISOR      # FB     # OT        VOLUME ".

           03  FILLER           PIC X(25) VALUE
           "SUPERVISOR      # FB     ".
           03  H-OT-DM          PIC X(12).
           03  FILLER           PIC X(7) VALUE "VOLUME ".

           03  FILLER           PIC X(32) VALUE
           " MTD GAIN    # PT     GAIN/LOSS ".
           03  FILLER           PIC X(21) VALUE
           "  % POTS   INC REM % ".
           03  FILLER           PIC X(24) VALUE
           " 30+ REMAIN $     30+ % ".
           03  FILLER           PIC X(11) VALUE
           "    % SLOW ".


      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(44) VALUE
           "   MTD P&L   # REPOS    VOL %   PROJ PROFIT ".
           03  FILLER           PIC X(32) VALUE
           " PROFIT %     APE       STD POS ".
           03  FILLER           PIC X(21) VALUE
           "  % DEL    PROJ INC  ".
           03  FILLER           PIC X(24) VALUE
           " PROJ INC %      30'S $ ".
           03  FILLER           PIC X(11) VALUE
           "    % 30'S ".

      *=================================================================
      *
      *    D E T A I L - L I N E
      *
      *=================================================================
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D1-BR-PROMPT           PIC X(8).
           03  D1-BRNO                PIC ZZZ9.
           03  D1-GRID REDEFINES D1-BRNO
                                      PIC X(4).
           03  D1-REF-NO              PIC ZZZZ,ZZ9-         BLANK ZERO.
           03  D1-NB-NO               PIC ZZZZ,ZZ9-         BLANK ZERO.
           03  FILLER                 PIC X(5).
           03  D1-TOT-VOL-NO          PIC ZZZZ,ZZ9-         BLANK ZERO.
           03  D1-GAIN-LOSS-NO        PIC ZZZZZ,ZZ9-        BLANK ZERO.
           03  D1-NO-FT-EMP           PIC ZZZZZZ9-          BLANK ZERO.
           03  D1-GAIN-LOSS-AMT       PIC ZZZZZ,ZZZ,ZZ9-    BLANK ZERO.
           03  D1-REC-POTS-NO         PIC ZZZZ,ZZ9-         BLANK ZERO.
           03  D1-INCR-REMAIN-AMT     PIC ZZZ,ZZZ,ZZ9-      BLANK ZERO.
           03  FILLER                 PIC X(5).
           03  D1-REC-30-NO           PIC ZZZZ,ZZ9-         BLANK ZERO.
           03  D1-30-DAY-MOVED        PIC ZZZZZ,ZZ9-        BLANK ZERO.
           03  D1-REC-SLOW-NO         PIC ZZ,ZZZ,ZZ9-       BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  D2-CONTACT             PIC X(12).
           03  D2-FB-NO               PIC ZZZZ,ZZ9-         BLANK ZERO.
           03  D2-OT-NO               PIC ZZZZ,ZZ9-         BLANK ZERO.
           03  D2-TOT-VOL-AMT         PIC Z,ZZZ,ZZZ,ZZ9-    BLANK ZERO.
           03  D2-GAIN-LOSS-NO        PIC ZZZZZ,ZZ9-        BLANK ZERO.
           03  D2-NO-PT-EMP           PIC ZZZZ.99-          BLANK ZERO.
           03  D2-GAIN-LOSS-AMT       PIC ZZZZZ,ZZZ,ZZ9-    BLANK ZERO.
           03  D2-POTS-PCT            PIC ZZZZ9.99-         BLANK ZERO.
           03  FILLER                 PIC X(3).
           03  D2-INCR-REMAIN-PCT     PIC ZZZZ9.99-         BLANK ZERO.
           03  D2-REC-30-REM          PIC ZZZZZ,ZZZ,ZZ9-    BLANK ZERO.
           03  D2-REC-30-PCT          PIC ZZZZZ9.99-        BLANK ZERO.
           03  D2-REC-SLOW-PCT        PIC ZZZ,ZZ9.99-       BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL3 REDEFINES DETAIL-LINE.
           03  D3-MTD-PL-AMT          PIC ZZ,ZZZ,ZZ9-       BLANK ZERO.
           03  D3-GN-REPO-NO          PIC ZZZZZ,ZZ9-        BLANK ZERO.
           03  D3-VOLUME-PCT          PIC ZZZZ9.99-         BLANK ZERO.
           03  D3-PROJ-PROFIT         PIC ZZZZZ,ZZZ,ZZ9-    BLANK ZERO.
           03  D3-PROFIT-PCT          PIC ZZZZZ9.99-        BLANK ZERO.
           03  D3-ACCTS-PER-EMP       PIC ZZZZ.99-          BLANK ZERO.
           03  D3-STD-POS             PIC ZZZZZ,ZZZ,ZZ9-    BLANK ZERO.
           03  D3-DEL-PCT             PIC ZZZZ9.99-         BLANK ZERO.
           03  FILLER                 PIC X(2).
           03  D3-PROJ-INCREASE       PIC ZZZZZ,ZZ9-        BLANK ZERO.
           03  FILLER                 PIC X(5).
           03  D3-INCREASE-PCT        PIC ZZZZ9.99-         BLANK ZERO.
           03  D3-REC-30-AMT          PIC ZZZZZ,ZZ9-        BLANK ZERO.
           03  D3-REC-30-PCT          PIC ZZZZZZ9.99-       BLANK ZERO.

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER                 PIC X(30).
           03  RANGE-SEL              PIC X(6).

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
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBGB/DATERW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/OPREP2_DEF.CPY".
       COPY "LIBLP/OPREP2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/OPREP2_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-CD1-FILE.
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
           MOVE "OPREP2" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.

           MOVE "N" TO RECORDS-FOUND.

           IF GROUP-FLAG NOT = "Y"
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

           IF WGR-SRESULT = 469
              MOVE SPACES TO MESS.

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
           MOVE "Y" TO NEW-BRANCH-FG.

           MOVE CUR-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.

           IF GROUP-FLAG = "Y"
              MOVE WGR-SRESULT TO BY-BRNO
           ELSE
              MOVE BRANCH TO BY-BRNO.
      *    MOVE BEG-CLASS TO BY-CLASS.

           IF BEG-CLASS NUMERIC
              MOVE BEG-CLASS TO BY-CLASS
           ELSE
              MOVE 1         TO BY-CLASS
              MOVE 99        TO END-CLASS.

           MOVE BY-DATE-CCYYMM       TO QBY1-WBEG-DATE-CCYYMM
                                        QBY1-WEND-DATE-CCYYMM.
           MOVE BY-BRNO              TO QBY1-WBEG-BRNO
                                        QBY1-WEND-BRNO.
           MOVE BY-CLASS             TO QBY1-WBEG-CLASS.
           MOVE END-CLASS            TO QBY1-WEND-CLASS.

           PERFORM START-BY1-FILE.

      *-----------------------------------------------------------------
       NEXT-BY1.

           PERFORM READ-BY1-FILE-NEXT.
           IF GROUP-FLAG = "Y"
              IF (IO-FG NOT = 0) OR
                  (BY-BRNO NOT = WGR-SRESULT) OR
                   (BY-DATE-CCYYMM NOT = CUR-MO-BY-DATE-CCYYMM) OR
                    (BY-CLASS > END-CLASS)
                 GO TO NEXT-GROUP-LOC.

           IF GROUP-FLAG NOT = "Y"
              IF (IO-FG NOT = 0) OR
                  (BY-BRNO NOT = BRANCH) OR
                   (BY-DATE-CCYYMM NOT = CUR-MO-BY-DATE-CCYYMM) OR
                    ( BY-CLASS > END-CLASS)
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-ROUTINE.

           IF BEG-CLASS NOT NUMERIC
              PERFORM TEST-CLASS
              IF CORRECT-CLASS = "N"
                 GO TO NEXT-BY1.

           MOVE "N" TO FIRST-TIME.

           MOVE "Y" TO RECORDS-FOUND.

       OK-TO-PROCESS.

           PERFORM CHECK-ADD-TOTALS.
           MOVE "N" TO NEW-BRANCH-FG.
           GO TO NEXT-BY1.

       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           MOVE 0 TO LN-FEED.
           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE "N" TO HEADING-FG.
           PERFORM WRITE-DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
        
           PERFORM RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE OPREP2-QUEUE-POS TO Q-POS.
           MOVE OPREP2-COPIES-POS TO C-POS.
           MOVE OPREP2-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO RECORDS-FOUND.

           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-BR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF DEL-FLAG = "Y"
              MOVE "MONTH-END SUMMARY PROGRESS REPORT" TO H-TITLE.
           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.
           MOVE STORE-DATE TO H-MMYY.

           IF EXT-ROUTE-BUF = "00"
              MOVE " " TO WR-BUF
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

      *----------------------------------------------------------------
       ENTER-DATE.
           MOVE "EN N040MMYY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DATE.

           MOVE VDUNUM0 TO STORE-DATE DISPLAY-DATE.

      *----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       SKIP-PASSWD-CHECK.

           MOVE "N" TO GROUP-FLAG.
           MOVE "SNNN040GROUP." TO VDU.
           MOVE BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
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
       ALPHA-CLASS.
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
           MOVE "SNNN020CLASS1." TO VDU.
           MOVE ENTER-CLASS-WK TO BEG-CLASS LC-CODE VDUNUM0.
           PERFORM SCREENIO.

           IF BEG-CLASS = 0
              GO TO BEGCLS.

           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE. *> EMBEDDED - LCFILE
           PERFORM CLOSE-LC1-FILE.

           IF IO-FG = 9
              MOVE "INVALID LOAN CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO BEGCLS.
      *-----------------------------------------------------------------
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
           PERFORM READ-LC1-FILE. *> EMBEDDED - LCFILE
           PERFORM CLOSE-LC1-FILE.

           IF IO-FG = 9
              MOVE "INVALID LOAN CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO ENDCLS.

           GO TO ENTER-DEL-FLAG.

      *-----------------------------------------------------------------
       ALLCLS.

           MOVE "SNNN020CLASS1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CLASS2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       CHECK-LEVEL-PRINTS.

           IF GROUP-FLAG = "Y"
              PERFORM OPEN-GR1-FILE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y" TO TOTAL-LEVEL-TABLE(9)
              GO TO EXIT-PARM.

      *  SEARCH-GR-INIT NOTES WHICH LEVELS ARE PRESENT WITHIN THE
      *  HIERARCHY OF SELECTED GROUP BY PUTTING "Y" IN WGR-LEVEL-TABLE
      *  FOR THAT LEVEL, SO WE MAY ASK WHICH LEVELS SHOULD PRINT
      
           PERFORM OPEN-CD1-FILE.
           MOVE 0 TO TOT-SUB.
           MOVE 10 TO LEVEL-SUB.

      *-----------------------------------------------------------------
       ASK-LEVEL.

           SUBTRACT 1 FROM LEVEL-SUB.

           IF LEVEL-SUB = 0
              GO TO EXIT-ASK-LEVEL.

           IF WGR-LEVEL-TABLE(LEVEL-SUB) NOT = "Y"
              GO TO ASK-LEVEL.

           ADD 1 TO TOT-SUB.
           MOVE "S  A000Q00." TO VDU.
           MOVE TOT-SUB TO VDUCOL.

           MOVE "LV"      TO CD-TYPE.
           MOVE LEVEL-SUB TO CD-LV-LEVEL.
           MOVE SPACES    TO CD-LV-FILLER.
           PERFORM READ-CD1-FILE. *> EMBEDDED - CDFILE
           IF IO-FG = 0
              MOVE CD-DESC TO LEVEL-DESC
           ELSE
              MOVE LEVEL-SUB TO LEVEL-DESC.

           MOVE SPACES TO LEVEL-QUES2.

           INSPECT LEVEL-QUESTION REPLACING FIRST "  " BY
              " &".

           INSPECT LEVEL-QUESTION REPLACING FIRST "&     "
              BY "LEVEL?" AFTER INITIAL " ".

           MOVE LEVEL-QUESTION TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ASK-LEVEL-QUEST.

           MOVE "E  A010A00." TO VDU.
           MOVE TOT-SUB TO VDUCOL.
           PERFORM SCREENIO.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 MOVE "PLEASE ENTER Y OR N" TO MESS
                 PERFORM SEND-MESS
                 GO TO ASK-LEVEL-QUEST.

           IF VDUBUF = "Y"
              IF LEVEL-SUB > HIGH-LEVEL
              MOVE LEVEL-SUB TO HIGH-LEVEL.

           MOVE VDUBUF TO TOTAL-LEVEL-TABLE(LEVEL-SUB).
           GO TO ASK-LEVEL.

       EXIT-ASK-LEVEL.
           PERFORM CLOSE-CD1-FILE.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A040GROUP." TO VDU
              MOVE ENT-GROUP TO VDUBUF
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

           IF GROUP-FLAG NOT = "Y"
              GO TO EXIT-DISPLAY-PARM.

           PERFORM OPEN-GR1-FILE.
           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      * SEARCH-GR-INIT NOTES WHICH LEVELS ARE PRESENT WITHIN THE
      * HIERARCHY OF SELECTED GROUP BY PUTTING "Y" IN WGR-LEVEL-TABLE
      * FOR THAT LEVEL, SO WE MAY ASK WHICH LEVELS SHOULD PRINT

           PERFORM OPEN-CD1-FILE.
           MOVE 0 TO TOT-SUB.
           MOVE 10 TO LEVEL-SUB.

      *-----------------------------------------------------------------
       DISPLAY-LEVEL.

           SUBTRACT 1 FROM LEVEL-SUB.

           IF LEVEL-SUB = 0
              GO TO EXIT-DISPLAY-LEVEL.

           IF WGR-LEVEL-TABLE(LEVEL-SUB) NOT = "Y"
              GO TO DISPLAY-LEVEL.

           ADD 1 TO TOT-SUB.
           MOVE "S  A000Q00." TO VDU.
           MOVE TOT-SUB TO VDUCOL.

           MOVE "LV"      TO CD-TYPE.
           MOVE LEVEL-SUB TO CD-LV-LEVEL.
           MOVE SPACES    TO CD-LV-FILLER.
           PERFORM READ-CD1-FILE. *> EMBEDDED - CDFILE
           IF IO-FG = 0
              MOVE CD-DESC TO LEVEL-DESC
           ELSE
              MOVE LEVEL-SUB TO LEVEL-DESC.

           MOVE SPACES TO LEVEL-QUES2.

           INSPECT LEVEL-QUESTION REPLACING FIRST "  " BY
              " &".

           INSPECT LEVEL-QUESTION REPLACING FIRST "&     "
              BY "LEVEL?" AFTER INITIAL " ".

           MOVE LEVEL-QUESTION TO VDUBUF.
           PERFORM SCREENIO.

      * DISPLAY LEVEL QUESTION

           MOVE "S  A010A00." TO VDU.
           MOVE TOT-SUB TO VDUCOL.
           MOVE TOTAL-LEVEL-TABLE(LEVEL-SUB) TO VDUBUF.
           PERFORM SCREENIO.

           GO TO DISPLAY-LEVEL.

       EXIT-DISPLAY-LEVEL.
           PERFORM CLOSE-CD1-FILE.

       EXIT-DISPLAY-PARM.
           EXIT.

      ******************************************
       TEST-CLASS SECTION.
      ******************************************
           MOVE "N" TO CORRECT-CLASS.

           MOVE BY-CLASS TO LC-CODE.
      *    PERFORM OPEN-LC1-FILE.
      *    PERFORM READ-LC1-FILE.
      *    PERFORM CLOSE-LC1-FILE.
      *    IF IO-BAD
           IF LC-SMALLOAN = "%"
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
      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       DO-GROUP-TOTALS.

           PERFORM PRINT-TOTALS.
           GO TO CONTINUE-LOOP.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SID.

           IF TOTAL-LEVEL-TABLE(9) NOT = "Y"
              GO TO CONTINUE-LOOP.

           MOVE WGR-SID(PREV-SUB) TO WGR-WORD.
           MOVE WGR-NUMB TO BR-NO.
           PERFORM READ-BR-FILE.
           MOVE 9 TO TOT-SUB.
           PERFORM PRINT-TOTALS.

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       CHECK-TOTAL-FLAG.

           IF TOTAL-LEVEL-TABLE(TOT-SUB) NOT = "Y"
              GO TO CONTINUE-CHECK-TOTALS.

           PERFORM ADD-TOTALS.

      *-----------------------------------------------------------------
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
           MOVE "BRANCH #" TO D1-BR-PROMPT.
           MOVE BR-NO      TO D1-BRNO.
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

      *  SECTION 2: MTD & YTD VOLUME

           ADD BY-NO(1, 2, TYPE-SUB) TO VOL-NO(TOT-SUB, 1, TYPE-SUB).
           ADD BY-AMT(1, 2, TYPE-SUB) TO VOL-AMT(TOT-SUB, 1, TYPE-SUB).
           ADD BY-NO(2, 2, TYPE-SUB) TO VOL-NO(TOT-SUB, 2, TYPE-SUB).
           ADD BY-AMT(2, 2, TYPE-SUB) TO VOL-AMT(TOT-SUB, 2, TYPE-SUB).

      * 10 OCCURENCE = TOTAL OF ALL TYPES

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

      * 10 OCCURENCE = TOTAL OF ALL TYPES
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

      * MISC (MANUALLY ENTERED) BRANCH & BRANCH STAT FIELDS
           IF NEW-BRANCH-FG = "Y"
              ADD BR-NO-FT-EMP TO NO-FT-EMP(TOT-SUB)
              ADD BR-NO-PT-EMP TO NO-PT-EMP(TOT-SUB).
           ADD BY-PROJ-PROFIT TO PROJ-PROFIT(TOT-SUB).
           ADD BY-PROJ-INCREASE TO PROJ-INCREASE(TOT-SUB).
           ADD BY-30-DAY-MOVED TO 30-DAY-MOVED(TOT-SUB).
           ADD BY-STD-POS TO STD-POS(TOT-SUB).
           ADD BY-AMT-REPO(1, 1, 1) TO GN-REPO-AMT(TOT-SUB).
           ADD BY-NO-REPO(1, 1, 1) TO GN-REPO-NO(TOT-SUB).
       EXIT-ADD-TOTALS.
           EXIT.

      ******************************************************************
       PRINT-TOTALS SECTION.
      ******************************************************************

           MOVE "Y" TO HEADING-FG.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           IF TOT-SUB < HIGH-LEVEL
              IF PRINTED-DASHES = "N"
                 MOVE 0 TO LN-FEED
                 MOVE DASH-LINE TO DETAIL-LINE
                 PERFORM WRITE-DETAIL-LINE
                 MOVE 1 TO LN-FEED
                 MOVE "Y" TO PRINTED-DASHES.

           IF TOT-SUB = 9
              MOVE "BRANCH #" TO D1-BR-PROMPT
              MOVE BR-NO TO D1-BRNO
           ELSE
              MOVE "GROUP: " TO D1-BR-PROMPT
              MOVE GR-ID TO D1-GRID.

           MOVE VOL-NO(TOT-SUB,1,2) TO D1-REF-NO.

      *TOTAL OF NEW LOANS (NB,RF,OT,DM)
           IF GP-NO-SC-USE-DM = "Y"
              COMPUTE D1-NB-NO =
               VOL-NO(TOT-SUB,1,1) +
                VOL-NO(TOT-SUB,1,2) +
                 VOL-NO(TOT-SUB,1,8) +
                  VOL-NO(TOT-SUB,1,9)
           ELSE
               MOVE VOL-NO(TOT-SUB,1,1) TO D1-NB-NO.

           MOVE VOL-NO(TOT-SUB,1,10) TO D1-TOT-VOL-NO.
           MOVE GAIN-LOSS-NO-CUR(TOT-SUB) TO D1-GAIN-LOSS-NO.
           MOVE NO-FT-EMP(TOT-SUB) TO D1-NO-FT-EMP.
           COMPUTE ROUNDING-WK ROUNDED =
                   GAIN-LOSS-AMT-CUR(TOT-SUB) * 1.
           MOVE ROUNDING-WK TO D1-GAIN-LOSS-AMT.
           MOVE DEL-NO-CUR(TOT-SUB,1,7) TO D1-REC-POTS-NO.

           COMPUTE AMT-WORK ROUNDED =
                     PL-AMT-MTD(TOT-SUB)
                      + DEL-AMT-CUR(TOT-SUB, 1, 2)
                       + DEL-AMT-CUR(TOT-SUB, 1, 3)
                       + DEL-AMT-CUR(TOT-SUB, 1, 4)
                        - DEL-AMT-MTD(TOT-SUB, 1, 3)
                        - DEL-AMT-MTD(TOT-SUB, 1, 4).

           COMPUTE ROUNDING-WK ROUNDED = AMT-WORK * 1.
           MOVE ROUNDING-WK TO D1-INCR-REMAIN-AMT.

           MOVE DEL-NO-CUR(TOT-SUB, 1, 5) TO D1-REC-30-NO.

           MOVE 30-DAY-MOVED(TOT-SUB) TO D1-30-DAY-MOVED.

           MOVE DEL-NO-CUR(TOT-SUB,2,8) TO D1-REC-SLOW-NO.

           PERFORM WRITE-DETAIL-LINE.

      * 2ND LINE OF DETAIL:

           IF TOT-SUB = 9
              MOVE BR-CITY TO NAME-WK
              INSPECT NAME-WK REPLACING FIRST "  " BY ", "
              INSPECT NAME-WK REPLACING FIRST "  "
              BY BR-STATE AFTER INITIAL " "
              MOVE NAME-WK TO D2-CONTACT
           ELSE
              MOVE GR-NAME TO D2-CONTACT.
           MOVE VOL-NO(TOT-SUB,1,3) TO D2-FB-NO.

           IF GP-NO-SC-USE-DM = "Y"
              MOVE VOL-NO(TOT-SUB,1,8) TO D2-OT-NO
           ELSE
              MOVE VOL-NO(TOT-SUB,1,9) TO D2-OT-NO.

           MOVE VOL-AMT(TOT-SUB,1,10) TO D2-TOT-VOL-AMT.
           COMPUTE NO-WORK = GAIN-LOSS-NO-CUR(TOT-SUB)
                           - GAIN-LOSS-NO-MTD(TOT-SUB).
           MOVE NO-WORK TO D2-GAIN-LOSS-NO.
           MOVE NO-PT-EMP(TOT-SUB) TO D2-NO-PT-EMP.
           COMPUTE ROUNDING-WK ROUNDED = GAIN-LOSS-AMT-CUR(TOT-SUB)
                                       - GAIN-LOSS-AMT-MTD(TOT-SUB).
           MOVE ROUNDING-WK TO D2-GAIN-LOSS-AMT.

           IF GAIN-LOSS-AMT-CUR(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                   DEL-AMT-CUR(TOT-SUB, 1, 7) /
                   GAIN-LOSS-AMT-CUR(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           MOVE PCT-WORK TO D2-POTS-PCT.

           COMPUTE AMT-WORK ROUNDED =
                     PL-AMT-MTD(TOT-SUB)
                      + DEL-AMT-CUR(TOT-SUB, 1, 2)
                       + DEL-AMT-CUR(TOT-SUB, 1, 3)
                       + DEL-AMT-CUR(TOT-SUB, 1, 4)
                        - DEL-AMT-MTD(TOT-SUB, 1, 3)
                        - DEL-AMT-MTD(TOT-SUB, 1, 4).

           IF GAIN-LOSS-AMT-CUR(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 AMT-WORK / GAIN-LOSS-AMT-CUR(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           MOVE PCT-WORK TO D2-INCR-REMAIN-PCT.

           COMPUTE ROUNDING-WK ROUNDED =
              DEL-AMT-CUR(TOT-SUB, 1, 5) * 1.
           MOVE ROUNDING-WK TO D2-REC-30-REM.

           IF GAIN-LOSS-AMT-CUR(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                   DEL-AMT-CUR(TOT-SUB, 1, 5) /
                   GAIN-LOSS-AMT-CUR(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           MOVE PCT-WORK TO D2-REC-30-PCT.

           IF GAIN-LOSS-NO-CUR(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                   DEL-NO-CUR(TOT-SUB, 2, 8) /
                   GAIN-LOSS-NO-CUR(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           MOVE PCT-WORK TO D2-REC-SLOW-PCT.

           MOVE "N" TO HEADING-FG.
           PERFORM WRITE-DETAIL-LINE.

           MOVE PL-AMT-MTD(TOT-SUB) TO D3-MTD-PL-AMT.
           MOVE GN-REPO-NO(TOT-SUB) TO D3-GN-REPO-NO.

           IF GAIN-LOSS-AMT-MTD(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                   VOL-AMT(TOT-SUB, 1, 10) /
                   GAIN-LOSS-AMT-MTD(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           MOVE PCT-WORK TO D3-VOLUME-PCT.

           MOVE PROJ-PROFIT(TOT-SUB) TO D3-PROJ-PROFIT.
           IF GAIN-LOSS-AMT-CUR(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                   PROJ-PROFIT(TOT-SUB) /
                   GAIN-LOSS-AMT-CUR(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           MOVE PCT-WORK TO D3-PROFIT-PCT.

           COMPUTE AMT-WORK = NO-FT-EMP(TOT-SUB) + NO-PT-EMP(TOT-SUB).
           IF AMT-WORK NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 GAIN-LOSS-NO-CUR(TOT-SUB) / AMT-WORK
           ELSE
              MOVE 0 TO PCT-WORK.
           MOVE PCT-WORK TO D3-ACCTS-PER-EMP.

           MOVE STD-POS(TOT-SUB) TO D3-STD-POS.

           IF GAIN-LOSS-AMT-CUR(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                   (DEL-AMT-CUR(TOT-SUB, 1, 1) +
                    DEL-AMT-CUR(TOT-SUB, 1, 7)) /
                   GAIN-LOSS-AMT-CUR(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           MOVE PCT-WORK TO D3-DEL-PCT.

           MOVE PROJ-INCREASE(TOT-SUB) TO D3-PROJ-INCREASE.
           IF GAIN-LOSS-AMT-CUR(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                   PROJ-INCREASE(TOT-SUB) /
                   GAIN-LOSS-AMT-CUR(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           MOVE PCT-WORK TO D3-INCREASE-PCT.

           MOVE DEL-AMT-CUR(TOT-SUB,1,1) TO D3-REC-30-AMT.

           IF GAIN-LOSS-AMT-CUR(TOT-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 DEL-AMT-CUR(TOT-SUB,1, 1) /
                   GAIN-LOSS-AMT-CUR(TOT-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           MOVE PCT-WORK TO D3-REC-30-PCT.

           MOVE "N" TO HEADING-FG.
           PERFORM WRITE-DETAIL-LINE.

           IF TOT-SUB < HIGH-LEVEL
              MOVE 1 TO LN-FEED
              MOVE DASH-LINE TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE
              MOVE 0 TO LN-FEED
              MOVE "Y" TO PRINTED-DASHES
           ELSE
              MOVE "N" TO PRINTED-DASHES.


       ZERO-TOTALS.
           MOVE ZEROS TO TOTAL-TAB(TOT-SUB).

       EXIT-PRINT-TOTALS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF HEADING-FG = "Y"
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
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.

           IF GP-NO-SC-USE-DM = "Y"
              MOVE "# DM" TO H-OT-DM
           ELSE
              MOVE "# OT"  TO H-OT-DM.
           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.

           MOVE HEAD5 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-CONDITIONAL.

       END-HEAD-PAGE.
           MOVE 5 TO LN-COUNT.
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

           MOVE "MONTH END DELINQUENCY FLAG" TO RANGE-LINE.
           MOVE DEL-FLAG TO RANGE-SEL.
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

           MOVE 0 TO FIRST-OPEN-DATE.

           PERFORM OPEN-RC2-FILE.

           MOVE "A1"     TO RC-STATUS
                            QRC2-WBEG-STATUS
                            QRC2-WEND-STATUS.
           MOVE BR-NO    TO RC-BRNO
                            QRC2-WBEG-BRNO
                            QRC2-WEND-BRNO.

           MOVE 19000101 TO RC-TRANS-DATE.

           MOVE RC-TRANS-DATE        TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WBEG-TRANS-DATE.
           MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WEND-TRANS-DATE.

           PERFORM START-RC2-FILE. *> EMBEDDED - RCFILE
           PERFORM READ-RC2-FILE-NEXT. *> EMBEDDED - RCFILE
           IF RC-BRNO = BR-NO
              IF RC-STATUS = "A1"
                 MOVE RC-TRANS-DATE TO FIRST-OPEN-DATE.

           PERFORM CLOSE-RC2-FILE.
       EXIT-GET-FIRST-OPEN.
           EXIT.

      ******************************************************************
       GET-TODAYS-TOTALS SECTION.
      ******************************************************************

           IF FIRST-OPEN-DATE = 0
              PERFORM CLEAR-TY-FILE
              GO TO GET-TODAYS-TOTALS-EXIT.

           MOVE BY-BRNO TO TY-BRNO.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           IF NOT (STORE-MO = ALP-MO AND STORE-YR = ALP-YR)
              PERFORM CLEAR-TY-FILE
              GO TO GET-TODAYS-TOTALS-EXIT.

           MOVE FIRST-OPEN-DATE TO TY-DATE.
           MOVE BY-CLASS        TO TY-CLASS.

           PERFORM OPEN-TY1-FILE.
           PERFORM READ-TY1-FILE.
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
           MOVE EXT-CONAME-CONAME TO OPREP2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO OPREP2-SYSDATE.
           DISPLAY OPREP2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE OPREP2-FORM-FIELDS TO WR-BUF.
           MOVE OPREP2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ********************************************
       SETUP-LINK-INFO SECTION.
      ********************************************
           MOVE OPREP2-HELP--FILE TO HELP-LINK-FILE.
           MOVE OPREP2-HELP--DIR  TO HELP-LINK-DIR.

      ********************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/OPREP2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPTYCL.CPY".

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETPR.CPY".
      *-----------------------------------------------------------------
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
       COPY "LIBLP/LPCDGS_SQL.CPY".
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
           PERFORM READ-BYA1-FILE. *> EMBEDDED - BYA,BYB
           IF ( IO-FG  = 0 )
              PERFORM SET-ALL-TO-BY1
           ELSE 
              MOVE "A" TO IO-MULTI-FG.
          
      *-----------------------------------------------------------------
       READ-BY1-FILE-NEXT.
     
           PERFORM SET-BY1-TO-ALL.
           PERFORM READ-BYA1-FILE-NEXT. *> EMBEDDED - BYA,BYB,LC
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

      *  STORED PROCEDURE SELECT STATEMENT: 
      *
      *  SELECT BYAFILE.BYA_DATE_CCYYMM,
      *         BYAFILE.BYA_BRNO,
      *         BYAFILE.BYA_CLASS,
      *         BYAFILE.BYA_NO_1_1_1,
      *         BYAFILE.BYA_NO_1_1_2,
      *         BYAFILE.BYA_NO_1_1_3,
      *         BYAFILE.BYA_NO_1_1_4,
      *         BYAFILE.BYA_NO_1_1_5,
      *         BYAFILE.BYA_NO_1_1_6,
      *         BYAFILE.BYA_NO_1_1_7,
      *         BYAFILE.BYA_NO_1_1_8,
      *         BYAFILE.BYA_NO_1_1_9,
      *         BYAFILE.BYA_NO_1_1_10,
      *         BYAFILE.BYA_NO_1_1_11,
      *         BYAFILE.BYA_NO_1_2_1,
      *         BYAFILE.BYA_NO_1_2_2,
      *         BYAFILE.BYA_NO_1_2_3,
      *         BYAFILE.BYA_NO_1_2_4,
      *         BYAFILE.BYA_NO_1_2_5,
      *         BYAFILE.BYA_NO_1_2_6,
      *         BYAFILE.BYA_NO_1_2_7,
      *         BYAFILE.BYA_NO_1_2_8,
      *         BYAFILE.BYA_NO_1_2_9,
      *         BYAFILE.BYA_NO_1_2_10,
      *         BYAFILE.BYA_NO_1_2_11,
      *         BYAFILE.BYA_NO_2_1_1,
      *         BYAFILE.BYA_NO_2_1_2,
      *         BYAFILE.BYA_NO_2_1_3,
      *         BYAFILE.BYA_NO_2_1_4,
      *         BYAFILE.BYA_NO_2_1_5,
      *         BYAFILE.BYA_NO_2_1_6,
      *         BYAFILE.BYA_NO_2_1_7,
      *         BYAFILE.BYA_NO_2_1_8,
      *         BYAFILE.BYA_NO_2_1_9,
      *         BYAFILE.BYA_NO_2_1_10,
      *         BYAFILE.BYA_NO_2_1_11,
      *         BYAFILE.BYA_NO_2_2_1,
      *         BYAFILE.BYA_NO_2_2_2,
      *         BYAFILE.BYA_NO_2_2_3,
      *         BYAFILE.BYA_NO_2_2_4,
      *         BYAFILE.BYA_NO_2_2_5,
      *         BYAFILE.BYA_NO_2_2_6,
      *         BYAFILE.BYA_NO_2_2_7,
      *         BYAFILE.BYA_NO_2_2_8,
      *         BYAFILE.BYA_NO_2_2_9,
      *         BYAFILE.BYA_NO_2_2_10,
      *         BYAFILE.BYA_NO_2_2_11,
      *         BYAFILE.BYA_NO_3_1_1,
      *         BYAFILE.BYA_NO_3_1_2,
      *         BYAFILE.BYA_NO_3_1_3,
      *         BYAFILE.BYA_NO_3_1_4,
      *         BYAFILE.BYA_NO_3_1_5,
      *         BYAFILE.BYA_NO_3_1_6,
      *         BYAFILE.BYA_NO_3_1_7,
      *         BYAFILE.BYA_NO_3_1_8,
      *         BYAFILE.BYA_NO_3_1_9,
      *         BYAFILE.BYA_NO_3_1_10,
      *         BYAFILE.BYA_NO_3_1_11,
      *         BYAFILE.BYA_NO_3_2_1,
      *         BYAFILE.BYA_NO_3_2_2,
      *         BYAFILE.BYA_NO_3_2_3,
      *         BYAFILE.BYA_NO_3_2_4,
      *         BYAFILE.BYA_NO_3_2_5,
      *         BYAFILE.BYA_NO_3_2_6,
      *         BYAFILE.BYA_NO_3_2_7,
      *         BYAFILE.BYA_NO_3_2_8,
      *         BYAFILE.BYA_NO_3_2_9,
      *         BYAFILE.BYA_NO_3_2_10,
      *         BYAFILE.BYA_NO_3_2_11,
      *         BYAFILE.BYA_AMT_1_1_1,
      *         BYAFILE.BYA_AMT_1_1_2,
      *         BYAFILE.BYA_AMT_1_1_3,
      *         BYAFILE.BYA_AMT_1_1_4,
      *         BYAFILE.BYA_AMT_1_1_5,
      *         BYAFILE.BYA_AMT_1_1_6,
      *         BYAFILE.BYA_AMT_1_1_7,
      *         BYAFILE.BYA_AMT_1_1_8,
      *         BYAFILE.BYA_AMT_1_1_9,
      *         BYAFILE.BYA_AMT_1_1_10,
      *         BYAFILE.BYA_AMT_1_1_11,
      *         BYAFILE.BYA_AMT_1_2_1,
      *         BYAFILE.BYA_AMT_1_2_2,
      *         BYAFILE.BYA_AMT_1_2_3,
      *         BYAFILE.BYA_AMT_1_2_4,
      *         BYAFILE.BYA_AMT_1_2_5,
      *         BYAFILE.BYA_AMT_1_2_6,
      *         BYAFILE.BYA_AMT_1_2_7,
      *         BYAFILE.BYA_AMT_1_2_8,
      *         BYAFILE.BYA_AMT_1_2_9,
      *         BYAFILE.BYA_AMT_1_2_10,
      *         BYAFILE.BYA_AMT_1_2_11,
      *         BYAFILE.BYA_AMT_2_1_1,
      *         BYAFILE.BYA_AMT_2_1_2,
      *         BYAFILE.BYA_AMT_2_1_3,
      *         BYAFILE.BYA_AMT_2_1_4,
      *         BYAFILE.BYA_AMT_2_1_5,
      *         BYAFILE.BYA_AMT_2_1_6,
      *         BYAFILE.BYA_AMT_2_1_7,
      *         BYAFILE.BYA_AMT_2_1_8,
      *         BYAFILE.BYA_AMT_2_1_9,
      *         BYAFILE.BYA_AMT_2_1_10,
      *         BYAFILE.BYA_AMT_2_1_11,
      *         BYAFILE.BYA_AMT_2_2_1,
      *         BYAFILE.BYA_AMT_2_2_2,
      *         BYAFILE.BYA_AMT_2_2_3,
      *         BYAFILE.BYA_AMT_2_2_4,
      *         BYAFILE.BYA_AMT_2_2_5,
      *         BYAFILE.BYA_AMT_2_2_6,
      *         BYAFILE.BYA_AMT_2_2_7,
      *         BYAFILE.BYA_AMT_2_2_8,
      *         BYAFILE.BYA_AMT_2_2_9,
      *         BYAFILE.BYA_AMT_2_2_10,
      *         BYAFILE.BYA_AMT_2_2_11,
      *         BYAFILE.BYA_AMT_3_1_1,
      *         BYAFILE.BYA_AMT_3_1_2,
      *         BYAFILE.BYA_AMT_3_1_3,
      *         BYAFILE.BYA_AMT_3_1_4,
      *         BYAFILE.BYA_AMT_3_1_5,
      *         BYAFILE.BYA_AMT_3_1_6,
      *         BYAFILE.BYA_AMT_3_1_7,
      *         BYAFILE.BYA_AMT_3_1_8,
      *         BYAFILE.BYA_AMT_3_1_9,
      *         BYAFILE.BYA_AMT_3_1_10,
      *         BYAFILE.BYA_AMT_3_1_11,
      *         BYAFILE.BYA_AMT_3_2_1,
      *         BYAFILE.BYA_AMT_3_2_2,
      *         BYAFILE.BYA_AMT_3_2_3,
      *         BYAFILE.BYA_AMT_3_2_4,
      *         BYAFILE.BYA_AMT_3_2_5,
      *         BYAFILE.BYA_AMT_3_2_6,
      *         BYAFILE.BYA_AMT_3_2_7,
      *         BYAFILE.BYA_AMT_3_2_8,
      *         BYAFILE.BYA_AMT_3_2_9,
      *         BYAFILE.BYA_AMT_3_2_10,
      *         BYAFILE.BYA_AMT_3_2_11,
      *         BYAFILE.BYA_NO_REPO_1_1_1,
      *         BYAFILE.BYA_NO_REPO_1_1_2,
      *         BYAFILE.BYA_NO_REPO_1_1_3,
      *         BYAFILE.BYA_NO_REPO_1_1_4,
      *         BYAFILE.BYA_NO_REPO_1_1_5,
      *         BYAFILE.BYA_NO_REPO_1_2_1,
      *         BYAFILE.BYA_NO_REPO_1_2_2,
      *         BYAFILE.BYA_NO_REPO_1_2_3,
      *         BYAFILE.BYA_NO_REPO_1_2_4,
      *         BYAFILE.BYA_NO_REPO_1_2_5,
      *         BYAFILE.BYA_NO_REPO_2_1_1,
      *         BYAFILE.BYA_NO_REPO_2_1_2,
      *         BYAFILE.BYA_NO_REPO_2_1_3,
      *         BYAFILE.BYA_NO_REPO_2_1_4,
      *         BYAFILE.BYA_NO_REPO_2_1_5,
      *         BYAFILE.BYA_NO_REPO_2_2_1,
      *         BYAFILE.BYA_NO_REPO_2_2_2,
      *         BYAFILE.BYA_NO_REPO_2_2_3,
      *         BYAFILE.BYA_NO_REPO_2_2_4,
      *         BYAFILE.BYA_NO_REPO_2_2_5,
      *         BYAFILE.BYA_NO_REPO_3_1_1,
      *         BYAFILE.BYA_NO_REPO_3_1_2,
      *         BYAFILE.BYA_NO_REPO_3_1_3,
      *         BYAFILE.BYA_NO_REPO_3_1_4,
      *         BYAFILE.BYA_NO_REPO_3_1_5,
      *         BYAFILE.BYA_NO_REPO_3_2_1,
      *         BYAFILE.BYA_NO_REPO_3_2_2,
      *         BYAFILE.BYA_NO_REPO_3_2_3,
      *         BYAFILE.BYA_NO_REPO_3_2_4,
      *         BYAFILE.BYA_NO_REPO_3_2_5,
      *         BYAFILE.BYA_AMT_REPO_1_1_1,
      *         BYAFILE.BYA_AMT_REPO_1_1_2,
      *         BYAFILE.BYA_AMT_REPO_1_1_3,
      *         BYAFILE.BYA_AMT_REPO_1_1_4,
      *         BYAFILE.BYA_AMT_REPO_1_1_5,
      *         BYAFILE.BYA_AMT_REPO_1_2_1,
      *         BYAFILE.BYA_AMT_REPO_1_2_2,
      *         BYAFILE.BYA_AMT_REPO_1_2_3,
      *         BYAFILE.BYA_AMT_REPO_1_2_4,
      *         BYAFILE.BYA_AMT_REPO_1_2_5,
      *         BYAFILE.BYA_AMT_REPO_2_1_1,
      *         BYAFILE.BYA_AMT_REPO_2_1_2,
      *         BYAFILE.BYA_AMT_REPO_2_1_3,
      *         BYAFILE.BYA_AMT_REPO_2_1_4,
      *         BYAFILE.BYA_AMT_REPO_2_1_5,
      *         BYAFILE.BYA_AMT_REPO_2_2_1,
      *         BYAFILE.BYA_AMT_REPO_2_2_2,
      *         BYAFILE.BYA_AMT_REPO_2_2_3,
      *         BYAFILE.BYA_AMT_REPO_2_2_4,
      *         BYAFILE.BYA_AMT_REPO_2_2_5,
      *         BYAFILE.BYA_AMT_REPO_3_1_1,
      *         BYAFILE.BYA_AMT_REPO_3_1_2,
      *         BYAFILE.BYA_AMT_REPO_3_1_3,
      *         BYAFILE.BYA_AMT_REPO_3_1_4,
      *         BYAFILE.BYA_AMT_REPO_3_1_5,
      *         BYAFILE.BYA_AMT_REPO_3_2_1,
      *         BYAFILE.BYA_AMT_REPO_3_2_2,
      *         BYAFILE.BYA_AMT_REPO_3_2_3,
      *         BYAFILE.BYA_AMT_REPO_3_2_4,
      *         BYAFILE.BYA_AMT_REPO_3_2_5,
      *         BYAFILE.BYA_NO_PL_1_1_1,
      *         BYAFILE.BYA_NO_PL_1_1_2,
      *         BYAFILE.BYA_NO_PL_1_1_3,
      *         BYAFILE.BYA_NO_PL_1_1_4,
      *         BYAFILE.BYA_NO_PL_1_1_5,
      *         BYAFILE.BYA_NO_PL_1_1_6,
      *         BYAFILE.BYA_NO_PL_1_1_7,
      *         BYAFILE.BYA_NO_PL_1_1_8,
      *         BYAFILE.BYA_NO_PL_1_1_9,
      *         BYAFILE.BYA_NO_PL_1_1_10,
      *         BYAFILE.BYA_NO_PL_1_2_1,
      *         BYAFILE.BYA_NO_PL_1_2_2,
      *         BYAFILE.BYA_NO_PL_1_2_3,
      *         BYAFILE.BYA_NO_PL_1_2_4,
      *         BYAFILE.BYA_NO_PL_1_2_5,
      *         BYAFILE.BYA_NO_PL_1_2_6,
      *         BYAFILE.BYA_NO_PL_1_2_7,
      *         BYAFILE.BYA_NO_PL_1_2_8,
      *         BYAFILE.BYA_NO_PL_1_2_9,
      *         BYAFILE.BYA_NO_PL_1_2_10,
      *         BYAFILE.BYA_NO_PL_2_1_1,
      *         BYAFILE.BYA_NO_PL_2_1_2,
      *         BYAFILE.BYA_NO_PL_2_1_3,
      *         BYAFILE.BYA_NO_PL_2_1_4,
      *         BYAFILE.BYA_NO_PL_2_1_5,
      *         BYAFILE.BYA_NO_PL_2_1_6,
      *         BYAFILE.BYA_NO_PL_2_1_7,
      *         BYAFILE.BYA_NO_PL_2_1_8,
      *         BYAFILE.BYA_NO_PL_2_1_9,
      *         BYAFILE.BYA_NO_PL_2_1_10,
      *         BYAFILE.BYA_NO_PL_2_2_1,
      *         BYAFILE.BYA_NO_PL_2_2_2,
      *         BYAFILE.BYA_NO_PL_2_2_3,
      *         BYAFILE.BYA_NO_PL_2_2_4,
      *         BYAFILE.BYA_NO_PL_2_2_5,
      *         BYAFILE.BYA_NO_PL_2_2_6,
      *         BYAFILE.BYA_NO_PL_2_2_7,
      *         BYAFILE.BYA_NO_PL_2_2_8,
      *         BYAFILE.BYA_NO_PL_2_2_9,
      *         BYAFILE.BYA_NO_PL_2_2_10,
      *         BYAFILE.BYA_NO_PL_3_1_1,
      *         BYAFILE.BYA_NO_PL_3_1_2,
      *         BYAFILE.BYA_NO_PL_3_1_3,
      *         BYAFILE.BYA_NO_PL_3_1_4,
      *         BYAFILE.BYA_NO_PL_3_1_5,
      *         BYAFILE.BYA_NO_PL_3_1_6,
      *         BYAFILE.BYA_NO_PL_3_1_7,
      *         BYAFILE.BYA_NO_PL_3_1_8,
      *         BYAFILE.BYA_NO_PL_3_1_9,
      *         BYAFILE.BYA_NO_PL_3_1_10,
      *         BYAFILE.BYA_NO_PL_3_2_1,
      *         BYAFILE.BYA_NO_PL_3_2_2,
      *         BYAFILE.BYA_NO_PL_3_2_3,
      *         BYAFILE.BYA_NO_PL_3_2_4,
      *         BYAFILE.BYA_NO_PL_3_2_5,
      *         BYAFILE.BYA_NO_PL_3_2_6,
      *         BYAFILE.BYA_NO_PL_3_2_7,
      *         BYAFILE.BYA_NO_PL_3_2_8,
      *         BYAFILE.BYA_NO_PL_3_2_9,
      *         BYAFILE.BYA_NO_PL_3_2_10,
      *         BYAFILE.BYA_AMT_PL_1_1_1,
      *         BYAFILE.BYA_AMT_PL_1_1_2,
      *         BYAFILE.BYA_AMT_PL_1_1_3,
      *         BYAFILE.BYA_AMT_PL_1_1_4,
      *         BYAFILE.BYA_AMT_PL_1_1_5,
      *         BYAFILE.BYA_AMT_PL_1_1_6,
      *         BYAFILE.BYA_AMT_PL_1_1_7,
      *         BYAFILE.BYA_AMT_PL_1_1_8,
      *         BYAFILE.BYA_AMT_PL_1_1_9,
      *         BYAFILE.BYA_AMT_PL_1_1_10,
      *         BYAFILE.BYA_AMT_PL_1_2_1,
      *         BYAFILE.BYA_AMT_PL_1_2_2,
      *         BYAFILE.BYA_AMT_PL_1_2_3,
      *         BYAFILE.BYA_AMT_PL_1_2_4,
      *         BYAFILE.BYA_AMT_PL_1_2_5,
      *         BYAFILE.BYA_AMT_PL_1_2_6,
      *         BYAFILE.BYA_AMT_PL_1_2_7,
      *         BYAFILE.BYA_AMT_PL_1_2_8,
      *         BYAFILE.BYA_AMT_PL_1_2_9,
      *         BYAFILE.BYA_AMT_PL_1_2_10,
      *         BYAFILE.BYA_AMT_PL_2_1_1,
      *         BYAFILE.BYA_AMT_PL_2_1_2,
      *         BYAFILE.BYA_AMT_PL_2_1_3,
      *         BYAFILE.BYA_AMT_PL_2_1_4,
      *         BYAFILE.BYA_AMT_PL_2_1_5,
      *         BYAFILE.BYA_AMT_PL_2_1_6,
      *         BYAFILE.BYA_AMT_PL_2_1_7,
      *         BYAFILE.BYA_AMT_PL_2_1_8,
      *         BYAFILE.BYA_AMT_PL_2_1_9,
      *         BYAFILE.BYA_AMT_PL_2_1_10,
      *         BYAFILE.BYA_AMT_PL_2_2_1,
      *         BYAFILE.BYA_AMT_PL_2_2_2,
      *         BYAFILE.BYA_AMT_PL_2_2_3,
      *         BYAFILE.BYA_AMT_PL_2_2_4,
      *         BYAFILE.BYA_AMT_PL_2_2_5,
      *         BYAFILE.BYA_AMT_PL_2_2_6,
      *         BYAFILE.BYA_AMT_PL_2_2_7,
      *         BYAFILE.BYA_AMT_PL_2_2_8,
      *         BYAFILE.BYA_AMT_PL_2_2_9,
      *         BYAFILE.BYA_AMT_PL_2_2_10,
      *         BYAFILE.BYA_AMT_PL_3_1_1,
      *         BYAFILE.BYA_AMT_PL_3_1_2,
      *         BYAFILE.BYA_AMT_PL_3_1_3,
      *         BYAFILE.BYA_AMT_PL_3_1_4,
      *         BYAFILE.BYA_AMT_PL_3_1_5,
      *         BYAFILE.BYA_AMT_PL_3_1_6,
      *         BYAFILE.BYA_AMT_PL_3_1_7,
      *         BYAFILE.BYA_AMT_PL_3_1_8,
      *         BYAFILE.BYA_AMT_PL_3_1_9,
      *         BYAFILE.BYA_AMT_PL_3_1_10,
      *         BYAFILE.BYA_AMT_PL_3_2_1,
      *         BYAFILE.BYA_AMT_PL_3_2_2,
      *         BYAFILE.BYA_AMT_PL_3_2_3,
      *         BYAFILE.BYA_AMT_PL_3_2_4,
      *         BYAFILE.BYA_AMT_PL_3_2_5,
      *         BYAFILE.BYA_AMT_PL_3_2_6,
      *         BYAFILE.BYA_AMT_PL_3_2_7,
      *         BYAFILE.BYA_AMT_PL_3_2_8,
      *         BYAFILE.BYA_AMT_PL_3_2_9,
      *         BYAFILE.BYA_AMT_PL_3_2_10,
      *         BYBFILE.BYB_INS_NO_WROTE_1_1,
      *         BYBFILE.BYB_INS_NO_WROTE_1_2,
      *         BYBFILE.BYB_INS_NO_WROTE_1_3,
      *         BYBFILE.BYB_INS_NO_WROTE_1_4,
      *         BYBFILE.BYB_INS_NO_WROTE_1_5,
      *         BYBFILE.BYB_INS_NO_WROTE_1_6,
      *         BYBFILE.BYB_INS_NO_WROTE_1_7,
      *         BYBFILE.BYB_INS_NO_WROTE_1_8,
      *         BYBFILE.BYB_INS_NO_WROTE_1_9,
      *         BYBFILE.BYB_INS_NO_WROTE_1_10,
      *         BYBFILE.BYB_INS_NO_WROTE_2_1,
      *         BYBFILE.BYB_INS_NO_WROTE_2_2,
      *         BYBFILE.BYB_INS_NO_WROTE_2_3,
      *         BYBFILE.BYB_INS_NO_WROTE_2_4,
      *         BYBFILE.BYB_INS_NO_WROTE_2_5,
      *         BYBFILE.BYB_INS_NO_WROTE_2_6,
      *         BYBFILE.BYB_INS_NO_WROTE_2_7,
      *         BYBFILE.BYB_INS_NO_WROTE_2_8,
      *         BYBFILE.BYB_INS_NO_WROTE_2_9,
      *         BYBFILE.BYB_INS_NO_WROTE_2_10,
      *         BYBFILE.BYB_UNITS_1_1_1,
      *         BYBFILE.BYB_UNITS_1_1_2,
      *         BYBFILE.BYB_UNITS_1_1_3,
      *         BYBFILE.BYB_UNITS_1_1_4,
      *         BYBFILE.BYB_UNITS_1_1_5,
      *         BYBFILE.BYB_UNITS_1_1_6,
      *         BYBFILE.BYB_UNITS_1_1_7,
      *         BYBFILE.BYB_UNITS_1_1_8,
      *         BYBFILE.BYB_UNITS_1_1_9,
      *         BYBFILE.BYB_UNITS_1_1_10,
      *         BYBFILE.BYB_UNITS_1_2_1,
      *         BYBFILE.BYB_UNITS_1_2_2,
      *         BYBFILE.BYB_UNITS_1_2_3,
      *         BYBFILE.BYB_UNITS_1_2_4,
      *         BYBFILE.BYB_UNITS_1_2_5,
      *         BYBFILE.BYB_UNITS_1_2_6,
      *         BYBFILE.BYB_UNITS_1_2_7,
      *         BYBFILE.BYB_UNITS_1_2_8,
      *         BYBFILE.BYB_UNITS_1_2_9,
      *         BYBFILE.BYB_UNITS_1_2_10,
      *         BYBFILE.BYB_UNITS_2_1_1,
      *         BYBFILE.BYB_UNITS_2_1_2,
      *         BYBFILE.BYB_UNITS_2_1_3,
      *         BYBFILE.BYB_UNITS_2_1_4,
      *         BYBFILE.BYB_UNITS_2_1_5,
      *         BYBFILE.BYB_UNITS_2_1_6,
      *         BYBFILE.BYB_UNITS_2_1_7,
      *         BYBFILE.BYB_UNITS_2_1_8,
      *         BYBFILE.BYB_UNITS_2_1_9,
      *         BYBFILE.BYB_UNITS_2_1_10,
      *         BYBFILE.BYB_UNITS_2_2_1,
      *         BYBFILE.BYB_UNITS_2_2_2,
      *         BYBFILE.BYB_UNITS_2_2_3,
      *         BYBFILE.BYB_UNITS_2_2_4,
      *         BYBFILE.BYB_UNITS_2_2_5,
      *         BYBFILE.BYB_UNITS_2_2_6,
      *         BYBFILE.BYB_UNITS_2_2_7,
      *         BYBFILE.BYB_UNITS_2_2_8,
      *         BYBFILE.BYB_UNITS_2_2_9,
      *         BYBFILE.BYB_UNITS_2_2_10,
      *         BYBFILE.BYB_UNITS_3_1_1,
      *         BYBFILE.BYB_UNITS_3_1_2,
      *         BYBFILE.BYB_UNITS_3_1_3,
      *         BYBFILE.BYB_UNITS_3_1_4,
      *         BYBFILE.BYB_UNITS_3_1_5,
      *         BYBFILE.BYB_UNITS_3_1_6,
      *         BYBFILE.BYB_UNITS_3_1_7,
      *         BYBFILE.BYB_UNITS_3_1_8,
      *         BYBFILE.BYB_UNITS_3_1_9,
      *         BYBFILE.BYB_UNITS_3_1_10,
      *         BYBFILE.BYB_UNITS_3_2_1,
      *         BYBFILE.BYB_UNITS_3_2_2,
      *         BYBFILE.BYB_UNITS_3_2_3,
      *         BYBFILE.BYB_UNITS_3_2_4,
      *         BYBFILE.BYB_UNITS_3_2_5,
      *         BYBFILE.BYB_UNITS_3_2_6,
      *         BYBFILE.BYB_UNITS_3_2_7,
      *         BYBFILE.BYB_UNITS_3_2_8,
      *         BYBFILE.BYB_UNITS_3_2_9,
      *         BYBFILE.BYB_UNITS_3_2_10,
      *         BYBFILE.BYB_OUTBAL_1_1_1,
      *         BYBFILE.BYB_OUTBAL_1_1_2,
      *         BYBFILE.BYB_OUTBAL_1_1_3,
      *         BYBFILE.BYB_OUTBAL_1_1_4,
      *         BYBFILE.BYB_OUTBAL_1_1_5,
      *         BYBFILE.BYB_OUTBAL_1_1_6,
      *         BYBFILE.BYB_OUTBAL_1_1_7,
      *         BYBFILE.BYB_OUTBAL_1_1_8,
      *         BYBFILE.BYB_OUTBAL_1_1_9,
      *         BYBFILE.BYB_OUTBAL_1_1_10,
      *         BYBFILE.BYB_OUTBAL_1_2_1,
      *         BYBFILE.BYB_OUTBAL_1_2_2,
      *         BYBFILE.BYB_OUTBAL_1_2_3,
      *         BYBFILE.BYB_OUTBAL_1_2_4,
      *         BYBFILE.BYB_OUTBAL_1_2_5,
      *         BYBFILE.BYB_OUTBAL_1_2_6,
      *         BYBFILE.BYB_OUTBAL_1_2_7,
      *         BYBFILE.BYB_OUTBAL_1_2_8,
      *         BYBFILE.BYB_OUTBAL_1_2_9,
      *         BYBFILE.BYB_OUTBAL_1_2_10,
      *         BYBFILE.BYB_OUTBAL_2_1_1,
      *         BYBFILE.BYB_OUTBAL_2_1_2,
      *         BYBFILE.BYB_OUTBAL_2_1_3,
      *         BYBFILE.BYB_OUTBAL_2_1_4,
      *         BYBFILE.BYB_OUTBAL_2_1_5,
      *         BYBFILE.BYB_OUTBAL_2_1_6,
      *         BYBFILE.BYB_OUTBAL_2_1_7,
      *         BYBFILE.BYB_OUTBAL_2_1_8,
      *         BYBFILE.BYB_OUTBAL_2_1_9,
      *         BYBFILE.BYB_OUTBAL_2_1_10,
      *         BYBFILE.BYB_OUTBAL_2_2_1,
      *         BYBFILE.BYB_OUTBAL_2_2_2,
      *         BYBFILE.BYB_OUTBAL_2_2_3,
      *         BYBFILE.BYB_OUTBAL_2_2_4,
      *         BYBFILE.BYB_OUTBAL_2_2_5,
      *         BYBFILE.BYB_OUTBAL_2_2_6,
      *         BYBFILE.BYB_OUTBAL_2_2_7,
      *         BYBFILE.BYB_OUTBAL_2_2_8,
      *         BYBFILE.BYB_OUTBAL_2_2_9,
      *         BYBFILE.BYB_OUTBAL_2_2_10,
      *         BYBFILE.BYB_OUTBAL_3_1_1,
      *         BYBFILE.BYB_OUTBAL_3_1_2,
      *         BYBFILE.BYB_OUTBAL_3_1_3,
      *         BYBFILE.BYB_OUTBAL_3_1_4,
      *         BYBFILE.BYB_OUTBAL_3_1_5,
      *         BYBFILE.BYB_OUTBAL_3_1_6,
      *         BYBFILE.BYB_OUTBAL_3_1_7,
      *         BYBFILE.BYB_OUTBAL_3_1_8,
      *         BYBFILE.BYB_OUTBAL_3_1_9,
      *         BYBFILE.BYB_OUTBAL_3_1_10,
      *         BYBFILE.BYB_OUTBAL_3_2_1,
      *         BYBFILE.BYB_OUTBAL_3_2_2,
      *         BYBFILE.BYB_OUTBAL_3_2_3,
      *         BYBFILE.BYB_OUTBAL_3_2_4,
      *         BYBFILE.BYB_OUTBAL_3_2_5,
      *         BYBFILE.BYB_OUTBAL_3_2_6,
      *         BYBFILE.BYB_OUTBAL_3_2_7,
      *         BYBFILE.BYB_OUTBAL_3_2_8,
      *         BYBFILE.BYB_OUTBAL_3_2_9,
      *         BYBFILE.BYB_OUTBAL_3_2_10,
      *         BYBFILE.BYB_STD_POS,
      *         BYBFILE.BYB_30_DAY_MOVED,
      *         BYBFILE.BYB_PROJ_INCREASE,
      *         BYBFILE.BYB_PROJ_PROFIT,
      *         ISNULL(LCFILE.LC_CODE, ' '), 
      *         ISNULL(LCFILE.LC_SMALLOAN,' '),
      *         ISNULL(LCFILE.LC_SALEFIN,' '),
      *         ISNULL(LCFILE.LC_SERVACCT,' '),
      *         ISNULL(LCFILE.LC_LOANTYPE,' '),
      *         ISNULL(LCFILE.LC_AFP_CLASS,' '),
      *         ISNULL(LCFILE.LC_GIFTNET,' ')
      *   FROM DBO.BYAFILE BYAFILE
      *       ,DBO.BYBFILE BYBFILE
      *   LEFT OUTER JOIN
      *        DBO.LCFILE LCFILE
      *     ON ( LCFILE.LC_CODE                = BYBFILE.BYB_CLASS ) 
      *  WHERE ( BYAFILE.BYA_DATE_CCYYMM BETWEEN @QBY1_WBEG_DATE_CCYYMM
      *                                      AND @QBY1_WEND_DATE_CCYYMM
      *    AND   BYAFILE.BYA_BRNO              = @QBY1_WBEG_BRNO
      *    AND   BYAFILE.BYA_CLASS       BETWEEN @QBY1_WBEG_CLASS
      *                                      AND @QBY1_WEND_CLASS )
      *    AND ( BYBFILE.BYB_DATE_CCYYMM       = BYAFILE.BYA_DATE_CCYYMM 
      *    AND   BYBFILE.BYB_BRNO              = BYAFILE.BYA_BRNO 
      *    AND   BYBFILE.BYB_CLASS             = BYAFILE.BYA_CLASS ) 
      *  ORDER BY BYAFILE.BYA_DATE_CCYYMM,
      *           BYAFILE.BYA_BRNO,
      *           BYAFILE.BYA_CLASS;


           EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BY1_CURSOR CURSOR FOR
                :BY1-RETURN-CODE = EXEC SSP_OPREP2_BYFILE_SELECT 
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
            SELECT  BYAFILE.BYA_DATE_CCYYMM,
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
                    BYAFILE.BYA_NO_REPO_1_1_1,
                    BYAFILE.BYA_NO_REPO_1_1_2,
                    BYAFILE.BYA_NO_REPO_1_1_3,
                    BYAFILE.BYA_NO_REPO_1_1_4,
                    BYAFILE.BYA_NO_REPO_1_1_5,
                    BYAFILE.BYA_NO_REPO_1_2_1,
                    BYAFILE.BYA_NO_REPO_1_2_2,
                    BYAFILE.BYA_NO_REPO_1_2_3,
                    BYAFILE.BYA_NO_REPO_1_2_4,
                    BYAFILE.BYA_NO_REPO_1_2_5,
                    BYAFILE.BYA_NO_REPO_2_1_1,
                    BYAFILE.BYA_NO_REPO_2_1_2,
                    BYAFILE.BYA_NO_REPO_2_1_3,
                    BYAFILE.BYA_NO_REPO_2_1_4,
                    BYAFILE.BYA_NO_REPO_2_1_5,
                    BYAFILE.BYA_NO_REPO_2_2_1,
                    BYAFILE.BYA_NO_REPO_2_2_2,
                    BYAFILE.BYA_NO_REPO_2_2_3,
                    BYAFILE.BYA_NO_REPO_2_2_4,
                    BYAFILE.BYA_NO_REPO_2_2_5,
                    BYAFILE.BYA_NO_REPO_3_1_1,
                    BYAFILE.BYA_NO_REPO_3_1_2,
                    BYAFILE.BYA_NO_REPO_3_1_3,
                    BYAFILE.BYA_NO_REPO_3_1_4,
                    BYAFILE.BYA_NO_REPO_3_1_5,
                    BYAFILE.BYA_NO_REPO_3_2_1,
                    BYAFILE.BYA_NO_REPO_3_2_2,
                    BYAFILE.BYA_NO_REPO_3_2_3,
                    BYAFILE.BYA_NO_REPO_3_2_4,
                    BYAFILE.BYA_NO_REPO_3_2_5,
                    BYAFILE.BYA_AMT_REPO_1_1_1,
                    BYAFILE.BYA_AMT_REPO_1_1_2,
                    BYAFILE.BYA_AMT_REPO_1_1_3,
                    BYAFILE.BYA_AMT_REPO_1_1_4,
                    BYAFILE.BYA_AMT_REPO_1_1_5,
                    BYAFILE.BYA_AMT_REPO_1_2_1,
                    BYAFILE.BYA_AMT_REPO_1_2_2,
                    BYAFILE.BYA_AMT_REPO_1_2_3,
                    BYAFILE.BYA_AMT_REPO_1_2_4,
                    BYAFILE.BYA_AMT_REPO_1_2_5,
                    BYAFILE.BYA_AMT_REPO_2_1_1,
                    BYAFILE.BYA_AMT_REPO_2_1_2,
                    BYAFILE.BYA_AMT_REPO_2_1_3,
                    BYAFILE.BYA_AMT_REPO_2_1_4,
                    BYAFILE.BYA_AMT_REPO_2_1_5,
                    BYAFILE.BYA_AMT_REPO_2_2_1,
                    BYAFILE.BYA_AMT_REPO_2_2_2,
                    BYAFILE.BYA_AMT_REPO_2_2_3,
                    BYAFILE.BYA_AMT_REPO_2_2_4,
                    BYAFILE.BYA_AMT_REPO_2_2_5,
                    BYAFILE.BYA_AMT_REPO_3_1_1,
                    BYAFILE.BYA_AMT_REPO_3_1_2,
                    BYAFILE.BYA_AMT_REPO_3_1_3,
                    BYAFILE.BYA_AMT_REPO_3_1_4,
                    BYAFILE.BYA_AMT_REPO_3_1_5,
                    BYAFILE.BYA_AMT_REPO_3_2_1,
                    BYAFILE.BYA_AMT_REPO_3_2_2,
                    BYAFILE.BYA_AMT_REPO_3_2_3,
                    BYAFILE.BYA_AMT_REPO_3_2_4,
                    BYAFILE.BYA_AMT_REPO_3_2_5,
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
                    BYBFILE.BYB_OUTBAL_3_2_10,
                    BYBFILE.BYB_STD_POS,
                    BYBFILE.BYB_30_DAY_MOVED,
                    BYBFILE.BYB_PROJ_INCREASE,
                    BYBFILE.BYB_PROJ_PROFIT
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
                   :QBYA-NO-REPO-1-1-1,
                   :QBYA-NO-REPO-1-1-2,
                   :QBYA-NO-REPO-1-1-3,
                   :QBYA-NO-REPO-1-1-4,
                   :QBYA-NO-REPO-1-1-5,
                   :QBYA-NO-REPO-1-2-1,
                   :QBYA-NO-REPO-1-2-2,
                   :QBYA-NO-REPO-1-2-3,
                   :QBYA-NO-REPO-1-2-4,
                   :QBYA-NO-REPO-1-2-5,
                   :QBYA-NO-REPO-2-1-1,
                   :QBYA-NO-REPO-2-1-2,
                   :QBYA-NO-REPO-2-1-3,
                   :QBYA-NO-REPO-2-1-4,
                   :QBYA-NO-REPO-2-1-5,
                   :QBYA-NO-REPO-2-2-1,
                   :QBYA-NO-REPO-2-2-2,
                   :QBYA-NO-REPO-2-2-3,
                   :QBYA-NO-REPO-2-2-4,
                   :QBYA-NO-REPO-2-2-5,
                   :QBYA-NO-REPO-3-1-1,
                   :QBYA-NO-REPO-3-1-2,
                   :QBYA-NO-REPO-3-1-3,
                   :QBYA-NO-REPO-3-1-4,
                   :QBYA-NO-REPO-3-1-5,
                   :QBYA-NO-REPO-3-2-1,
                   :QBYA-NO-REPO-3-2-2,
                   :QBYA-NO-REPO-3-2-3,
                   :QBYA-NO-REPO-3-2-4,
                   :QBYA-NO-REPO-3-2-5,
                   :QBYA-AMT-REPO-1-1-1,
                   :QBYA-AMT-REPO-1-1-2,
                   :QBYA-AMT-REPO-1-1-3,
                   :QBYA-AMT-REPO-1-1-4,
                   :QBYA-AMT-REPO-1-1-5,
                   :QBYA-AMT-REPO-1-2-1,
                   :QBYA-AMT-REPO-1-2-2,
                   :QBYA-AMT-REPO-1-2-3,
                   :QBYA-AMT-REPO-1-2-4,
                   :QBYA-AMT-REPO-1-2-5,
                   :QBYA-AMT-REPO-2-1-1,
                   :QBYA-AMT-REPO-2-1-2,
                   :QBYA-AMT-REPO-2-1-3,
                   :QBYA-AMT-REPO-2-1-4,
                   :QBYA-AMT-REPO-2-1-5,
                   :QBYA-AMT-REPO-2-2-1,
                   :QBYA-AMT-REPO-2-2-2,
                   :QBYA-AMT-REPO-2-2-3,
                   :QBYA-AMT-REPO-2-2-4,
                   :QBYA-AMT-REPO-2-2-5,
                   :QBYA-AMT-REPO-3-1-1,
                   :QBYA-AMT-REPO-3-1-2,
                   :QBYA-AMT-REPO-3-1-3,
                   :QBYA-AMT-REPO-3-1-4,
                   :QBYA-AMT-REPO-3-1-5,
                   :QBYA-AMT-REPO-3-2-1,
                   :QBYA-AMT-REPO-3-2-2,
                   :QBYA-AMT-REPO-3-2-3,
                   :QBYA-AMT-REPO-3-2-4,
                   :QBYA-AMT-REPO-3-2-5,
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
                   :QBYB-STD-POS,
                   :QBYB-30-DAY-MOVED,
                   :QBYB-PROJ-INCREASE,
                   :QBYB-PROJ-PROFIT
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
                     :QBYA-NO-REPO-1-1-1,
                     :QBYA-NO-REPO-1-1-2,
                     :QBYA-NO-REPO-1-1-3,
                     :QBYA-NO-REPO-1-1-4,
                     :QBYA-NO-REPO-1-1-5,
                     :QBYA-NO-REPO-1-2-1,
                     :QBYA-NO-REPO-1-2-2,
                     :QBYA-NO-REPO-1-2-3,
                     :QBYA-NO-REPO-1-2-4,
                     :QBYA-NO-REPO-1-2-5,
                     :QBYA-NO-REPO-2-1-1,
                     :QBYA-NO-REPO-2-1-2,
                     :QBYA-NO-REPO-2-1-3,
                     :QBYA-NO-REPO-2-1-4,
                     :QBYA-NO-REPO-2-1-5,
                     :QBYA-NO-REPO-2-2-1,
                     :QBYA-NO-REPO-2-2-2,
                     :QBYA-NO-REPO-2-2-3,
                     :QBYA-NO-REPO-2-2-4,
                     :QBYA-NO-REPO-2-2-5,
                     :QBYA-NO-REPO-3-1-1,
                     :QBYA-NO-REPO-3-1-2,
                     :QBYA-NO-REPO-3-1-3,
                     :QBYA-NO-REPO-3-1-4,
                     :QBYA-NO-REPO-3-1-5,
                     :QBYA-NO-REPO-3-2-1,
                     :QBYA-NO-REPO-3-2-2,
                     :QBYA-NO-REPO-3-2-3,
                     :QBYA-NO-REPO-3-2-4,
                     :QBYA-NO-REPO-3-2-5,
                     :QBYA-AMT-REPO-1-1-1,
                     :QBYA-AMT-REPO-1-1-2,
                     :QBYA-AMT-REPO-1-1-3,
                     :QBYA-AMT-REPO-1-1-4,
                     :QBYA-AMT-REPO-1-1-5,
                     :QBYA-AMT-REPO-1-2-1,
                     :QBYA-AMT-REPO-1-2-2,
                     :QBYA-AMT-REPO-1-2-3,
                     :QBYA-AMT-REPO-1-2-4,
                     :QBYA-AMT-REPO-1-2-5,
                     :QBYA-AMT-REPO-2-1-1,
                     :QBYA-AMT-REPO-2-1-2,
                     :QBYA-AMT-REPO-2-1-3,
                     :QBYA-AMT-REPO-2-1-4,
                     :QBYA-AMT-REPO-2-1-5,
                     :QBYA-AMT-REPO-2-2-1,
                     :QBYA-AMT-REPO-2-2-2,
                     :QBYA-AMT-REPO-2-2-3,
                     :QBYA-AMT-REPO-2-2-4,
                     :QBYA-AMT-REPO-2-2-5,
                     :QBYA-AMT-REPO-3-1-1,
                     :QBYA-AMT-REPO-3-1-2,
                     :QBYA-AMT-REPO-3-1-3,
                     :QBYA-AMT-REPO-3-1-4,
                     :QBYA-AMT-REPO-3-1-5,
                     :QBYA-AMT-REPO-3-2-1,
                     :QBYA-AMT-REPO-3-2-2,
                     :QBYA-AMT-REPO-3-2-3,
                     :QBYA-AMT-REPO-3-2-4,
                     :QBYA-AMT-REPO-3-2-5,
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
                     :QBYB-STD-POS,
                     :QBYB-30-DAY-MOVED,
                     :QBYB-PROJ-INCREASE,
                     :QBYB-PROJ-PROFIT,
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


      * COPY "LIBLP/LPLC1RN.CPY".
      * COPYMEMBER: LIBLP/LPLC1RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LC-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LC-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO LC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LC-ALL-MACHINE
              MOVE LC-ALL-PATH             TO LC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LC-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO LC-ALL-BASE
              MOVE FILPATH-MACHINE         TO LC-ALL-MACHINE
              MOVE LC-ALL-PATH             TO LC-PATH.

      *-----------------------------------------------------------------
       OPEN-LC1-FILE.
           PERFORM LOAD-LC1-FILE.
           PERFORM OPEN-IT.
           MOVE LC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-LC1-FILE. *> EMBEDDED -  LCFILE
           PERFORM READ-IT.
           MOVE LC-PATH-SQL TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.
           INITIALIZE QLC-REC.

           MOVE LC-CODE TO QLC-CODE.

           EXEC SQL
            SELECT  LCFILE.LC_CODE,
                    LCFILE.LC_SMALLOAN, 
                    LCFILE.LC_SALEFIN,
                    LCFILE.LC_SERVACCT,
                    LCFILE.LC_LOANTYPE,
                    LCFILE.LC_AFP_CLASS,
                    LCFILE.LC_GIFTNET
              INTO  
                   :QLC-CODE,
                   :QLC-SMALLOAN, 
                   :QLC-SALEFIN, 
                   :QLC-SERVACCT,
                   :QLC-LOANTYPE,
                   :QLC-AFP-CLASS,
                   :QLC-GIFTNET
              FROM DBO.LCFILE
             WHERE LCFILE.LC_CODE = :QLC-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-LC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LC1-FILE.
           PERFORM CLOSE-IT.
 
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPCD1RN.CPY".
      * COPYMEMBER: LIBLP/LPCD1RN
       LOAD-CD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CD-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CD-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO CD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CD-ALL-MACHINE
              MOVE CD-ALL-PATH             TO CD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CD-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO CD-ALL-BASE
              MOVE FILPATH-MACHINE         TO CD-ALL-MACHINE
              MOVE CD-ALL-PATH             TO CD-PATH.

      *-----------------------------------------------------------------
       OPEN-CD1-FILE.
           PERFORM LOAD-CD1-FILE.
           PERFORM OPEN-IT.
           MOVE CD-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-CD1-FILE. *> EMBEDDED - CDFILE

           PERFORM READ-IT.
           MOVE CD-PATH-SQL TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.
           INITIALIZE QCD-REC.

           MOVE CD-TYPE    TO QCD-TYPE.
           MOVE CD-CODE    TO QCD-CODE.

           EXEC SQL
            SELECT CDFILE.CD_TYPE,
                   CDFILE.CD_CODE,
                   CDFILE.CD_DESC
              INTO 
                   :QCD-TYPE,
                   :QCD-CODE,
                   :QCD-DESC 
  
              FROM DBO.CDFILE
             WHERE CDFILE.CD_TYPE = :QCD-TYPE
               AND CDFILE.CD_CODE = :QCD-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CD1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CD-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CD1-FILE.
           PERFORM CLOSE-IT.
   
       COPY "LIBLP/LPTY1RN.CPY".

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



       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
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
              DESTROY OPREP2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
