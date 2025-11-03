       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DSINQ.
       DATE-WRITTEN.    062294.
      *************************************************************************
      *                 (LP)
      *          NEW  DEALER STATISTICS FILE INQUIRY
      *
      *
      * ******   MAJOR CONSTRUCTION AHEAD ALTERNATE ROUTE SUGGESTED  ******
      *
      *
      * REV:
      *  MJD 102197 REWROTE DEALER STATS TO MATCH BYINQ FORMAT. SAVED LOGIC
      *             FOR DAILY DEALER STAT FILE IN THE FUTURE.
      *  JTG 121297 CHANGED TO DISPLAY INCREASE REMAIN FOR WRLD AND NOT NFR
      *  MJD 031198 FIXED BUG HDS AND HDY FIELDS WERE NOT EXPANDED ADEQUETLY.
      *  MJD 051998 REMOVED DS-PASTDUE.
      *  JTG 060998 ADDED LOGIC TO DISPLAY BANKRUPT DELINQUENCY BUCKETS
      *  JTG 062998 CHANGED DS-CNVT.... TO DS-RNEW....
      *                     DS-SVOS.... TO DS-TODL....
      *  JTG 111798 CORRECTED PROMPTS FOR NON VOLUME STATS APPS PAGE 5
      *  MJD 000101 CHANGED PAGE 5 TO CORRECTLY DISPLAY CHANGE FROM ELM AND ELY
      *  MJD 000322 REMOVED DS-CNEARN/EARNTAKN/EARNDUE/CNUNEARN/ERNOTTAK
      *             ADDED DS-UNEARNED-AMT AND DS-UNEARNED-AMT2
      *  MJD 000328 ADDED PAGE 6 - UNEARNED INCOME (MERC PR#223)
      *  MJD 111116 ADDED DY-PROCEEDS TO CLEAR ROUTINE.  CORRECTED
      *             PROBLEM WITH CLEARING HDY-PROCEEDS THIS WAS CAUSING
      *             DISPLAY ISSUES IN LINUX.
      * BAH 150516 REMOVED ALL MOVES TO EXT-CONAME-USER-GROUP, USE WGR-WORD, DO
      *            NOT WANT TO SET AN EXTERNAL VARIABLE IN HERE!
      *
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      * BLM 160512 FIX DS-DATE FOR NEW FORMAT, INCREASE CLASS TO 3, FIX
      *            DS-UNEARNED, PD0003
      * KEC 2017.0105 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      * BAH 181015 GLOBAL DSFILE
      * BAH 181018 GLOBAL RCFILE
      *  CS 190423 REMOVE DS APPLICATION STATS
      *  CS 190424 ADDED FUTURE FIELDS 
      *  BAH 20190924 REMOVED ACCESS-CALL DSFILE, RCFILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)LP/DSINQ.CBL J35 12/21/23-09:02:13 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".

       01  ERRCD           PIC X     VALUE SPACES.
       01  FIRST-READ      PIC X     VALUE "Y".
       01  IO-TYPE         PIC X(3)  VALUE " ".
       01  ACTION          PIC X(6)  VALUE " ".
       88  ACTION-P1-6               VALUE "    P1"
                                           "    P2"
                                           "    P3"
                                           "    P4"
                                           "    P5"
                                           "    P6".
       01  FILLER REDEFINES ACTION.
           03  FILLER      PIC X(5).
           03  ACTION-P1   PIC 9.


       01  REM             PIC X(5)  VALUE X"72656D2D3E".

       01  TRAP-NO-DY-FG       PIC X     VALUE " ".
       01  TRAP-A-DATE-DY-FG   PIC X     VALUE " ".
       01  DY-TRAP-MSG.
           03  FILLER      PIC X(11) VALUE
               "** DY TRAP(".
           03  DY-TRAP-DATE PIC 9(6) VALUE 0.
           03  FILLER      PIC X(2)  VALUE ")  ".

       01  FORM-BUFX.
           03  FORM-1      PIC X(6) VALUE "DSINQ1".
           03  FORM-2      PIC X(6) VALUE "DSINQ2".
           03  FORM-3      PIC X(6) VALUE "DSINQ3".
           03  FORM-4      PIC X(6) VALUE "DSINQ4".
           03  FORM-5      PIC X(6) VALUE "DSINQ5".
           03  FORM-6      PIC X(6) VALUE "DSINQ6".
       01  FILLER REDEFINES FORM-BUFX.
           03  FORM-BUFFER PIC X(6) OCCURS 6 TIMES.

       01  HOLD-FORM-NAM   PIC X(6) VALUE "DSINQ3".

       01  PAGE-FLAG       PIC 9(2) VALUE 3.
           88  PAGE1       VALUE 01.
           88  PAGE2       VALUE 02.
           88  PAGE3       VALUE 03.
           88  PAGE4       VALUE 04.
           88  PAGE5       VALUE 05.
           88  PAGE6       VALUE 06.

       01  GROUP-FLAG      PIC X.
       01  ENT-GROUP       PIC X(4).
       01  ENT-GROUPX REDEFINES ENT-GROUP.
           03  ENT-GROUP-1ST-3 PIC X(3).
           03  FILLER      PIC X.
       01  SAVE-LEVEL      PIC 9.
       01  HOLD-GR-NAME PIC X(20).

       01  ALL-DEALERS-FLAG         PIC X    VALUE "N".
           88  ONE-DEALER                    VALUE "N".
           88  ALL-DEALERS                   VALUE "Y".
       01  ALL-CLASSES-FLAG         PIC X    VALUE "N".
           88  ONE-CLASS                     VALUE "O".
           88  ALL-CLASSES                   VALUE "Y".
           88  SF-CLASSES                    VALUE "S".
           88  LN-CLASSES                    VALUE "L".
           88  AFP-CLASSES                   VALUE "A".

       01  YYES                     PIC X    VALUE "Y".
       01  NNO                      PIC X    VALUE "N".
       01  STORE-BRNO               PIC 9(4) VALUE 0.
       01  STORE-DLNO               PIC 9(4)V99 VALUE 0.
       01  STORE-DLNAME             PIC X(25) VALUE SPACES.
       01  STORE-CLASS              PIC 9(3)  VALUE 00.
       01  STORE-LCNAME             PIC X(25) VALUE SPACES.
       01  ENTER-CLASS-WK           PIC 9(3).
       01  ENTER-CLASS-ALPHA       REDEFINES ENTER-CLASS-WK.
           03  CLASS-WK1            PIC X.
           03  CLASS-WK2            PIC X.
           03  CLASS-WK3            PIC X.
       01  CORRECT-CLASS            PIC X    VALUE "N".
       01  STORE-DATE               PIC 9(4).
       01  STORE-DATEX             REDEFINES STORE-DATE.
           03  STORE-MO             PIC 9(2).
           03  STORE-YR             PIC 9(2).
       01  CASE-BUF                 PIC X(4).
       01  FIRST-OPEN-DATE          PIC 9(8) VALUE 0.

       01  TODAYS-DATE.
           03  TODAY-MM             PIC 99.
           03  FILLER               PIC X.
           03  TODAY-DD             PIC 99.
           03  FILLER               PIC X.
           03  TODAY-YY             PIC 99.

       01  DISPLAY-DATE             PIC 9(4).
       01  DISPLAY-DATEX            REDEFINES DISPLAY-DATE.
           03  DISPLAY-MO           PIC 99.
           03  DISPLAY-YR           PIC 99.
       01  DATE-LIST                PIC X(5)  VALUE "MMYY.".
       01  HOLD-DS-DATE-CCYYMM      PIC 9(6).

      *----------------------------------------------------------------
      * READ TYPE: PASSED IN FROM MENU  1 - SNAPSHOT (DEFAULT)
      *                                 2 - REALTIME
      * READ-ONCE-FG
      *   SET TO "Y" AFTER ALL STAT RECORDS HAVE BEEN READ IN SNAPSHOT
      *     MODE.
      *----------------------------------------------------------------
       01  READ-TYPE           PIC 9 VALUE 1.
           88  READ-ONCE             VALUE 1.
           88  READ-EVERY            VALUE 2.
       01  READ-TYPEA REDEFINES READ-TYPE
                               PIC X.
       01  READ-ONCE-FG     PIC X VALUE "N".


       01  SUB             PIC 9(6) COMP VALUE 1.
       01  SUB2            PIC 9(6) COMP VALUE 1.
       01  SUB4            PIC 9(6) COMP VALUE 1.
       01  DSSUB1          PIC 9(6) COMP VALUE 1.
       01  DSSUB2          PIC 9(6) COMP VALUE 1.
       01  DSSUB3          PIC 9(6) COMP VALUE 1.
       01  SUBX            PIC 9(6) COMP VALUE 1.
       01  SUBY            PIC 9(6) COMP VALUE 1.
       01  PERIOD          PIC 9(6) COMP VALUE 1.
       01  ACTSUB          PIC 9(6) COMP VALUE 1.
       01  TYPSUB          PIC 9(6) COMP VALUE 1.
       01  RECCON          PIC 9(6) COMP VALUE 1.
       01  AGESUB          PIC 9(6) COMP VALUE 1.
       01  REPOSUB         PIC 9(6) COMP VALUE 1.

       01  O2-TYPE-TABLE.
           03  FILLER     PIC X(27) VALUE
           "-------- ACCOUNTS ---------".
           03  FILLER     PIC X(27) VALUE
           "-------- ACCOUNTS ---------".
           03  FILLER     PIC X(27) VALUE
           "                           ".
           03  FILLER     PIC X(27) VALUE
           "                           ".
           03  FILLER     PIC X(45) VALUE
           "---------------- RECEIVABLES ----------------".
           03  FILLER     PIC X(45) VALUE
           "------------ RECEIVABLES LESS O2 ------------".
           03  FILLER     PIC X(45) VALUE
           "------------------ O2 ONLY ------------------".
           03  FILLER     PIC X(45) VALUE
           "----------------- INTEREST ------------------".
       01  O2-TYPES      REDEFINES O2-TYPE-TABLE.
           03  O2-ACCT   PIC X(27) OCCURS 4 TIMES.
           03  O2-REC    PIC X(45) OCCURS 4 TIMES.
       01  O2-FNAME      PIC X(11)  VALUE "O2_TYPE1:P.".
       01  O2-FNAMEL     PIC X(11)  VALUE "O2_TYPE2:P.".
       01  O2-BUFFER     PIC X(80) VALUE " ".

       01  O2-INT-TYPE-TABLE.
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           "  $REC             MTD             YTD ".
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           "   MTD             YTD             LYR ".
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           " TODAY                                 ".
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           " TODAY                                 ".
       01  O2-INT-TYPES      REDEFINES O2-INT-TYPE-TABLE.
           03  O2-INT-TYPEL  PIC X(80) OCCURS 2 TIMES.
           03  O2-INT-TYPET  PIC X(80) OCCURS 2 TIMES.
       01  ACT-FNAME      PIC X(9)  VALUE "ACT_TYPE.".
       01  ACT-FNAME2     PIC X(12) VALUE "ACT_TYPE2:P.".
       01  ACT-FNAMEL     PIC X(11) VALUE "ACT_LINE:P.".
       01  ACT-BUFFER     PIC X(80) VALUE " ".

      *----------------------------------------------------------------
      * SCREEN PROMPTS PAGE 4
      *----------------------------------------------------------------
       01  O2-INT-TYPE-TABLE4.
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           "    $REC            MTD            YTD ".
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           "     MTD            YTD            LYR ".
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           "   TODAY                               ".
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           " TODAY                                 ".
       01  O2-INT-TYPES4      REDEFINES O2-INT-TYPE-TABLE4.
           03  O2-INT-TYPE4L  PIC X(80) OCCURS 2 TIMES.
           03  O2-INT-TYPE4T  PIC X(80) OCCURS 2 TIMES.

       01  ACT4-TYPE-TABLE.
           03  FILLER     PIC X(21) VALUE
           "*** NET GAIN/LOSS ***".
           03  FILLER     PIC X(21) VALUE
           " *** NEW VOLUME ***  ".
           03  FILLER     PIC X(41) VALUE
           "             NO.     MTD     YTD         ".
           03  FILLER     PIC X(39) VALUE
           "    $REC            MTD            YTD ".
           03  FILLER     PIC X(41) VALUE
           "             MTD     YTD    LYR          ".
           03  FILLER     PIC X(39) VALUE
           "     MTD            YTD            LYR ".
           03  FILLER     PIC X(41) VALUE
           "           TODAY                         ".
           03  FILLER     PIC X(39) VALUE
           "   TODAY                               ".
           03  FILLER     PIC X(41) VALUE
           "           TODAY                         ".
           03  FILLER     PIC X(39) VALUE
           "   TODAY                               ".
       01  ACT4-TYPES      REDEFINES ACT4-TYPE-TABLE.
           03  ACT4-TYPE   PIC X(21) OCCURS 2 TIMES.
           03  ACT4-TYPEL  PIC X(80) OCCURS 2 TIMES.
           03  ACT4-TYPET  PIC X(80) OCCURS 2 TIMES.
       01  PAGE4-TYPE     PIC 9 VALUE 1.
           88  PAGE4-TOTAL      VALUE 1.
           88  PAGE4-TODAY      VALUE 2.
      * DEFAULT VALUE IS 1, DS DOES NOT HAVE O2 OR INT NUMBERS.
       01  PAGE4-O2-TYPE  PIC 9 VALUE 1.
           88  PAGE4-REC        VALUE 1.
           88  PAGE4-NO-O2      VALUE 2.
           88  PAGE4-O2         VALUE 3.
           88  PAGE4-INT        VALUE 4.

      *-----------------------------------------------------------------
      *    PERIOD TYPE ----- FOR PAGE 5
      *-----------------------------------------------------------------
       01  PAGE5-LIST.
           03  FILLER                  PIC X(18)   VALUE
               "PER_GNLS PER_NVOL.".

       01  PAGE5-BUFFER.
           03  PER-BUF-GNLS            PIC X(20)   VALUE " ".
           03  PER-BUF-NVOL            PIC X(20)   VALUE " ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  PAGE5-TYPES-TABLE.
           03  FILLER                  PIC X(20) VALUE
               "CURRENT OUTSTANDING ".
           03  FILLER                  PIC X(20) VALUE
               "MONTH-TO-DATE VOLUME".
           03  FILLER                  PIC X(20) VALUE
               "    MONTH-TO-DATE   ".
           03  FILLER                  PIC X(20) VALUE
               "YEAR-TO-DATE VOLUME ".
           03  FILLER                  PIC X(20) VALUE
               "    YEAR-TO-DATE    ".
           03  FILLER                  PIC X(20) VALUE
               "LAST YEAR VOLUME    ".
       01  PAGE5-TYPESX              REDEFINES PAGE5-TYPES-TABLE.
           03  PAGE5-TYPE-DESC        PIC X(20) OCCURS 6 TIMES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *   USED TO DETERMINE WHAT TO DISPLAY: CUR MTD YTD CHANGE
       01  PAGE5-GNLS                 PIC 9       VALUE 1.
      *   USED TO DETERMINE WHAT TO DISPLAY: MTD YTD LYR VOLUME
       01  PAGE5-NVOL                 PIC 9       VALUE 1.

       01  PAGE5-TYPE                 PIC 9       VALUE 1.
           88  PAGE5-TOTAL                        VALUE 1.
           88  PAGE5-TODAY                        VALUE 2.

      *----------------------------------------------------------------
      * SCREEN PROMPTS PAGE 6
      *----------------------------------------------------------------
       01  PAGE6-TYPE     PIC 9 VALUE 1.
           88  PAGE6-TOTAL      VALUE 1.
           88  PAGE6-TODAY      VALUE 2.
       01  PAGE6-PROMPTS-TABLE.
           03  FILLER      PIC X(19)    VALUE "           CURRENT ".
           03  FILLER      PIC X(19)    VALUE "GAIN-FOR-THE-MONTH ".
           03  FILLER      PIC X(19)    VALUE " GAIN-FOR-THE-YEAR ".
           03  FILLER      PIC X(19)    VALUE "     MONTH-TO-DATE ".
           03  FILLER      PIC X(19)    VALUE "      YEAR-TO-DATE ".
           03  FILLER      PIC X(19)    VALUE "         LAST-YEAR ".
           03  FILLER      PIC X(19)    VALUE "             TODAY ".
           03  FILLER      PIC X(19)    VALUE SPACES.
           03  FILLER      PIC X(19)    VALUE SPACES.
           03  FILLER      PIC X(19)    VALUE "             TODAY ".
           03  FILLER      PIC X(19)    VALUE SPACES.
           03  FILLER      PIC X(19)    VALUE SPACES.
       01  PAGE6-PROMPT-TBL REDEFINES PAGE6-PROMPTS-TABLE.
           03  PAGE6-PROMPT   PIC X(114) OCCURS 2.
       01  PAGE6-PMPT-LIST.
           03  FILLER      PIC X(36) VALUE
           "NET-PMPT1:P NET-PMPT2:P NET-PMPT3:P ".
           03  FILLER      PIC X(36) VALUE
           "VOL-PMPT1:P VOL-PMPT2:P VOL-PMPT3:P.".

      *----------------------------------------------------------------
      * SCREEN PROMPTS COMMON
      *----------------------------------------------------------------
       01  ACTIVITY       PIC 9 VALUE 1.
           88  NET-GAIN-LOSS    VALUE 1.
           88  NEW-VOLUME       VALUE 2.

      *----------------------------------------------------------------
      * SCREEN DISPLAYS FOR INQUIRY TYPE:
      *  INDEXED BY READ-TYPE.
      *   1 - SNAPSHOT STATISTICS (NORMAL PRINT).
      *   2 - REALTIME STATISTICS (REVERSE PRINT).
      *----------------------------------------------------------------
       01  DISP-INQ-TYPES.
           03 FILLER            PIC X(29) VALUE
           "  SNAPSHOT DEALER STATISTICS ".
           03 FILLER            PIC X(29) VALUE
           "  REALTIME DEALER STATISTICS ".
       01  DISP-INQ-TYPES-TBL  REDEFINES DISP-INQ-TYPES.
           03 INQUIRY-TYPE      PIC X(29) OCCURS 2.
       01  DISP-INQ-LISTS.
           03  FILLER           PIC X(9) VALUE "INQ_TYPE.".
           03  FILLER           PIC X(9) VALUE "INQ_TYPE.".
       01  DISP-INQ-LISTS-TBL  REDEFINES DISP-INQ-LISTS.
           03 INQUIRY-LIST      PIC X(9) OCCURS 2.

      ******************************************************************
      *    SCREEN DISPLAY BUFFER
      ******************************************************************
      *=================================================================
      *    S C R E E N - 1 - F I E L D S
      *=================================================================
       01  DISPLAY-BUF1      VALUE SPACES.
           03  A01           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  A02           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  A03           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  A04           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  A05           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  A06           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  A07           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  A08           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  A09           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  A10           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  A11           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  A111          PIC ZZZ,ZZZ,ZZZ,ZZZ.
           03  A13           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  A14           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  A15           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  A16           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  A17           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
      *- - - - - - - - - - - - - - - - - -  VVVVV AVAILABLE SPACES VVVVV
           03  A18           PIC X(6).
      *- - - - - - - - - - - - - - - - - -  ^^^^^ AVAILABLE SPACES ^^^^^
           03  A19           PIC ZZ,ZZZ,ZZZ,ZZZ-.
           03  A20           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  A21           PIC ZZ,ZZZ,ZZZ,ZZZ-.
           03  A22           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  A23           PIC ZZ,ZZZ,ZZZ,ZZZ-.
           03  A24           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.
           03  PROMPT-A25    PIC X(14).
           03  A25           PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.

       01  DISPLAY-LIST1.
           03  FILLER        PIC X(40) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER        PIC X(36) VALUE
           "110 111 130 140 150 160 170 180 190 ".
           03  FILLER        PIC X(35) VALUE
           "200 210 220 230 240 PROMPT-250 250.".

      ******************************************************************
      *    PAGE 1 TOTALS
      ******************************************************************
         01  PAGE-1-TOTALS     VALUE ZERO.
             03  SUM-DLRVPRIN           PIC S9(15)V99  OCCURS 3.
             03  SUM-DLRVCHGS           PIC S9(15)V99  OCCURS 3.
             03  SUM-DLPARVHELD         PIC S9(15)V99.
             03  SUM-UNPAIDCK           PIC S9(15)V99.
             03  SUM-DLDISC             PIC S9(15)V99.
             03  SUM-DLPARVPAID         PIC S9(15)V99.
             03  SUM-SUMWGTTA           PIC S9(15)V99.
             03  SUM-SUMWGTTM           PIC S9(15)V99.
             03  SUM-SUMWGTAV           PIC S9(15)V99.
             03  SUM-DLDISC-VOL         PIC S9(10)V99  OCCURS 3.
             03  SUM-PROCEEDS           PIC S9(10)V99  OCCURS 2.
             03  SUM-RCOURSE-UNITS      PIC S9(15)     OCCURS 3.
             03  SUM-RCOURSE-AMT        PIC S9(15)V99  OCCURS 3.
             03  SUM-AFPDUEDL-MEMO      PIC S9(15)V99.

      *=================================================================
      *
      *    S C R E E N - 2 - F I E L D S
      *
      *=================================================================
       01  DISPLAY-BUF2A      VALUE SPACES.
           03  DISPLAY-BUF2A-TAB  OCCURS 9.
               05  B01       PIC ZZZZZZZZ-      OCCURS 3.
       01  DISPLAY-BUF2B      VALUE SPACES.
           03  DISPLAY-BUF2B-TAB  OCCURS 9.
               05  B02       PIC ZZZZZZZZZZ.ZZ- OCCURS 3.

       01  DISPLAY-LIST2A.
           03  FILLER        PIC X(40) VALUE
           "010 020 030 070 080 090 130 140 150 190 ".
           03  FILLER        PIC X(40) VALUE
           "200 210 250 260 270 310 320 330 370 380 ".
           03  FILLER        PIC X(28) VALUE
           "390 430 440 450 490 500 510.".
       01  DISPLAY-LIST2B.
           03  FILLER        PIC X(40) VALUE
           "040 050 060 100 110 120 160 170 180 220 ".
           03  FILLER        PIC X(40) VALUE
           "230 240 280 290 300 340 350 360 400 410 ".
           03  FILLER        PIC X(28) VALUE
           "420 460 470 480 520 530 540.".

      *=================================================================
      *
      *    P A G E - 2 - T O T A L S
      *
      *=================================================================
       01  PAGE-2-TOTALS      VALUE ZEROS.
           03  SUM2-NPURUNIT       PIC S9(8)     OCCURS 3.
           03  SUM2-NPURAMT        PIC S9(10)V99 OCCURS 3.
           03  SUM2-OPURUNIT       PIC S9(8)     OCCURS 3.
           03  SUM2-OPURAMT        PIC S9(10)V99 OCCURS 3.
           03  SUM2-RNEWUNIT       PIC S9(8)     OCCURS 3.
           03  SUM2-RNEWAMT        PIC S9(10)V99 OCCURS 3.
           03  SUM2-PAIDUNIT       PIC S9(8)     OCCURS 3.
           03  SUM2-PAIDAMT        PIC S9(10)V99 OCCURS 3.
           03  SUM2-TOLNUNIT       PIC S9(8)     OCCURS 3.
           03  SUM2-TOLNAMT        PIC S9(10)V99 OCCURS 3.
           03  SUM2-OUTSUNIT       PIC S9(8)     OCCURS 3.
           03  SUM2-OUTSAMT        PIC S9(10)V99 OCCURS 3.
           03  SUM2-PLOSUNIT       PIC S9(8)     OCCURS 3.
           03  SUM2-PLOSAMT        PIC S9(10)V99 OCCURS 3.
           03  SUM2-RCPLUNIT       PIC S9(8)     OCCURS 3.
           03  SUM2-RCPLAMT        PIC S9(10)V99 OCCURS 3.
           03  SUM2-JDOSUNIT       PIC S9(8)     OCCURS 3.
           03  SUM2-JDOSAMT        PIC S9(10)V99 OCCURS 3.

      *=================================================================
      *
      *    SCREEN 3 FIELDS
      *
      *=================================================================
       01  PROMPT-12345.
           03  FILLER       PIC X(45)   VALUE
      *--->"       UNITS _____ RECENCY ____ PAST DUE    U".
           "       UNITS            RECENCY             U".
           03  FILLER       PIC X(35)   VALUE
      *--->"NITS _ CONTRACTUAL ____ PAST DUE   ".
           "NITS        CONTRACTUAL            ".

      *-----------------------------------------------------------------
       01  DEL-TYPE-TABLE.
           03  FILLER     PIC X(34) VALUE
           " ****  CURRENT INFORMATION  ****  ".
           03  FILLER     PIC X(34) VALUE
           "****  BEGINNING OF THE MONTH  ****".
           03  FILLER     PIC X(34) VALUE
           "   ****  END OF LAST MONTH ****   ".
           03  FILLER     PIC X(34) VALUE
           " ****  TODAYS INFORMATION   ****  ".
           03  FILLER     PIC X(34) VALUE
           "***  CHANGE FROM BEGIN MONTH  *** ".
           03  FILLER     PIC X(34) VALUE
           "**  % CHANGE FROM BEGIN MONTH  ** ".
           03  FILLER     PIC X(34) VALUE
           "**  DAILY OPERATING STATISTICS  **".

      *-----------------------------------------------------------------
       01  DEL-TYPES REDEFINES DEL-TYPE-TABLE.
           03  DEL-TYPE   PIC X(34)
                          OCCURS 7 TIMES.
      *-----------------------------------------------------------------
      * SCREEN PROMPTS FOR PAGE 3
      *-----------------------------------------------------------------
       01  DEL-MSEL-TABLE.
           03  FILLER                  PIC X(31)   VALUE
           "F4-DIFF BEGMO,F5-BEG/END,F6-BAN".
           03  FILLER                  PIC X(25)   VALUE
           "KRUPT,F7-DAILY           ".
           03  FILLER                  PIC X(31)   VALUE
           "F2-CURRENT,F4-DIFF BEGMO,F6-BAN".
           03  FILLER                  PIC X(25)   VALUE
           "KRUPT,F7-DAILY           ".
           03  FILLER                  PIC X(31)   VALUE
           "F2-CURRENT,F4-DIFF BEGMO,F5-BEG".
           03  FILLER                  PIC X(25)   VALUE
           "/END,F7-DAILY            ".
           03  FILLER                  PIC X(31)   VALUE
           "F2-CURRENT,F4-DIFF BEGMO,F5-BEG".
           03  FILLER                  PIC X(25)   VALUE
           "/END,F6-BANKRUPT,F7-DAILY".
           03  FILLER                  PIC X(31)   VALUE
           "F4,F2-CURRENT,F5-BEG/END,F6-BAN".
           03  FILLER                  PIC X(25)   VALUE
           "KRUPT,F7-DAILY           ".
           03  FILLER                  PIC X(31)   VALUE
           "F2-CURRENT,F4,F5-BEG/END,F6-BAN".
           03  FILLER                  PIC X(25)   VALUE
           "KRUPT,F7-DAILY           ".
           03  FILLER                  PIC X(31)   VALUE
           "F7,F2-CURRENT,F4-DIFF BEGMO,F5-".
           03  FILLER                  PIC X(25)   VALUE
           "BEG/END,F6-BANKRUPT      ".

       01  DEL-MSEL-TBL     REDEFINES DEL-MSEL-TABLE.
           03  DEL-MSEL         OCCURS 7 TIMES
                                       PIC X(56).

       01  DEL-MSEL-LIST    PIC X(7)    VALUE "MSEL:P.".


       01  DEL-FNAME      PIC X(9) VALUE "DEL_TYPE.".
       01  DEL-BUFFER     PIC X(80) VALUE " ".
       01  DELINQ         PIC 9 VALUE 7.
       01  DELTYP-COD     PIC 9 VALUE ZERO.
       01  BNKRPT-DEL     PIC 9 VALUE 1.
       01  DIFF-PER       PIC 9 VALUE ZERO.
           88  DIFF       VALUE 0.
           88  PER        VALUE 1.

      *-----------------------------------------------------------------
       01  DISPLAY-BUF3.
           03  DEL-PROMPT1                   PIC X(80).
           03  DEL-TABLE                     OCCURS 18.
               05  DEL-LN-PROMPT             PIC X(5).
               05  DEL-LN                    PIC X(75).
               05  FILLER                    REDEFINES DEL-LN.
                   07  DEL-REC-CON           OCCURS 2.
                       09  DEL-UNITS         PIC Z(6)9-     BLANK ZERO.
                       09  DEL-T-UNITS       REDEFINES DEL-UNITS
                                             PIC Z(6)9-.
                       09  FILLER            REDEFINES DEL-UNITS.
                           11 FILLER         PIC X(7).
                           11 DEL-UNITS-SIGN PIC X.
                       09  DEL-UNITS-P       REDEFINES DEL-UNITS
                                             PIC Z(4).99-    BLANK ZERO.
                       09  DEL-T-UNITS-P     REDEFINES DEL-UNITS
                                             PIC Z(4).99-.

                       09  FILLER            PIC X(2).

                       09  DEL-OUTBAL        PIC Z,ZZZ,ZZZ,ZZ9.99-
                                                 BLANK ZERO.
                       09  DEL-T-OUTBAL      REDEFINES DEL-OUTBAL
                                             PIC Z,ZZZ,ZZZ,ZZ9.99-.
                       09  FILLER            REDEFINES DEL-OUTBAL.
                           11 FILLER         PIC X(16).
                           11 DEL-OUTBAL-SIGN PIC X.

                       09  FILLER            PIC X(10).

      *-----------------------------------------------------------------
       01  DISPLAY-LIST3.
           03  FILLER       PIC X(41) VALUE
           "P1:S 010:S 011 012 013 020:S 021 022 023 ".
           03  FILLER       PIC X(20) VALUE
           "030:S 031 040:S 041 ".
           03  FILLER       PIC X(40) VALUE
           "050:S 051 060:S 061 070:S 071 080 081:S ".
           03  FILLER       PIC X(40) VALUE
           "090:S 091 100:S 101 110 111:S 120:S 121 ".
           03  FILLER       PIC X(40) VALUE
           "130 131:S 140:S 141 150:S 151 160:S 161 ".
           03  FILLER       PIC X(22) VALUE
           "170:S 171:S 180:S 181.".

      *=================================================================
      *
      *    P A G E - 3 - T O T A L S
      *
      *=================================================================
       01  DAILY-TYPE          PIC 9       VALUE 0.
           88  DAILY-TOP       VALUE 0.
           88  DAILY-BOTTOM    VALUE 1.

      *-----------------------------------------------------------------
       01  PAGE-3-TOTALS                   VALUE ZEROS.
           03  SUM-DEL-TABLE               OCCURS 10.
               05  SUM-DEL-REC-CON         OCCURS 2.
                   07  SUM-DEL-UNITS       PIC S9(6).
                   07  SUM-DEL-OUTBAL      PIC S9(10)V99.

      *-----------------------------------------------------------------
       01  PAGE-3-TOT-TABLE                VALUE ZEROS.
           03  TOT-DEL-TABLE               OCCURS 2.
               05  TOT-DEL-REC-CON         OCCURS 2.
                   07  TOT-DEL-UNITS       PIC S9(6)V9(7).
                   07  TOT-DEL-OUTBAL      PIC S9(10)V9(7).

      *-----------------------------------------------------------------
      *  USED TO COMPUTE DEL, SLOW AND TOT PERCENTAGES ON PAGE3,
      *     PERCENT OF CHANGE FROM BEGINNING OF MONTH.
      *     COMPUTED:
      *
      *  HOLD-TOT-DEL-UNITS (1 RECCON) =
      *   SUM-DEL-UNITS(AGESUB RECCON) - SUM-REL-DEL-UNITS(AGESUB RECCON).
      *
      *   HOLD-TOT-DEL-UNITS (1 RECCON) =
      *    HOLD-OT-DEL-UNITS (2 RECCON) + SUM-REL-DEL-UNITS (SUB3 RECCON).
      *
      *                             HOLD-TOT-DEL-UNITS(1 RECCON)
      *   TOT-DEL-UNITS(1 RECCON) = ---------------------------- * 100.
      *                             HOLD-TOT-DEL-UNITS(2 RECCON)
      *
      *     (SAME FORMAT FOR OUTBAL.)
      *
      *  HOLD-TOT-DEL-TABLE:    LEVEL 1 : CHANGE FROM BEGIN OF MONTH.
      *                               2 : BEGINNING OF MONTH.
      *
      *  HOLD-TOT-DEL-REC-CON:        1 : RECENCY.
      *                               2 : CONTRACTUAL.
      *-----------------------------------------------------------------
       01  HOLD-PAGE-3-TOT-TABLE            VALUE ZEROS.
           03  HOLD-TOT-DEL-TABLE           OCCURS 2.
               05  HOLD-TOT-DEL-REC-CON     OCCURS 2.
                   07  HOLD-TOT-DEL-UNITS   PIC S9(6)V9(7).
                   07  HOLD-TOT-DEL-OUTBAL  PIC S9(10)V9(7).

      *-----------------------------------------------------------------
       01  PAGE-3-REL-TOTALS               VALUE ZEROS.
           03  SUM-REL-DEL-TABLE           OCCURS 10.
               05  SUM-REL-DEL-REC-CON     OCCURS 2.
                   07  SUM-REL-DEL-UNITS   PIC S9(6).
                   07  SUM-REL-DEL-OUTBAL  PIC S9(10)V99.

      *-----------------------------------------------------------------
       01  PAGE-3-DAILY-TOTALS             VALUE ZEROS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *        (1) 30 DAYS
      *        (2) 60 DAYS
      *        (3) 90 +
      *        (4) <M>
      *        (5) <P>
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  SUM-DAILY-TABLE             OCCURS 5.
               05  SUM-DAILY-REC-CON       OCCURS 2.
                   07  SUM-DAILY-UNITS     PIC S9(6).
                   07  SUM-DAILY-OUTBAL    PIC S9(10)V99.
           03  SUM-DAILY-NO                PIC S9(9).
           03  SUM-DAILY-AMT               PIC S9(10)V99.
           03  SUM-DAILY-VOLNO             PIC S9(9).
           03  SUM-DAILY-VOL               PIC S9(9)V99.
           03  SUM-DAILY-SUMWGTTM          PIC S9(12).
           03  SUM-DAILY-SUMWGTTA          PIC S9(12)V99.
           03  SUM-DAILY-SUMWGTAV          PIC S9(14)V99.
           03  SUM-DAILY-NB                PIC S99999.
           03  SUM-DAILY-FB                PIC S99999.
           03  SUM-DAILY-PB                PIC S99999.
           03  SUM-DAILY-NB-AMT            PIC S9(9)V99.
           03  SUM-DAILY-FB-AMT            PIC S9(9)V99.
           03  SUM-DAILY-PB-AMT            PIC S99(9)V99.
           03  SUM-DAILY-PL-AMT            PIC S9(11)V99.
           03  SUM-DAILY-PL-NO             PIC S999999.

      *-----------------------------------------------------------------
       01  PAGE-3-DAILY-TOT-TABLE          VALUE ZEROS.
           03  TOT-DAILY-TABLE             OCCURS 2.
               05  TOT-DAILY-REC-CON       OCCURS 2.
                   07  TOT-DAILY-UNITS     PIC S9(6)V9(7).
                   07  TOT-DAILY-P-UNITS   PIC S9(4)V9(2).
                   07  TOT-DAILY-OUTBAL    PIC S9(10)V9(7).
                   07  TOT-DAILY-P-OUTBAL  PIC S9(4)V9(2).

      *-----------------------------------------------------------------
       01  PAGE-3-REL-DAILY-TOTALS           VALUE ZEROS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *        (1) 30 DAYS
      *        (2) 60 DAYS
      *        (3) 90 +
      *        (4) <M>
      *        (5) <P>
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  SUM-REL-DAILY-TABLE           OCCURS 5.
               05  SUM-REL-DAILY-REC-CON     OCCURS 2.
                   07  SUM-REL-DAILY-UNITS   PIC S9(6).
                   07  SUM-REL-DAILY-OUTBAL  PIC S9(10)V99.
           03  SUM-REL-DAILY-NO              PIC S9(9).
           03  SUM-REL-DAILY-AMT             PIC S9(10)V99.

      *-----------------------------------------------------------------
       01  PAGE-3-REL-DAILY-TOT-TABLE         VALUE ZEROS.
           03  TOT-REL-DAILY-TABLE            OCCURS 2.
               05  TOT-REL-DAILY-REC-CON      OCCURS 2.
                   07  TOT-REL-DAILY-UNITS    PIC S9(6)V9(7).
                   07  TOT-REL-DAILY-P-UNITS  PIC S9(4)V9(2).
                   07  TOT-REL-DAILY-OUTBAL   PIC S9(10)V9(7).
                   07  TOT-REL-DAILY-P-OUTBAL PIC S9(4)V9(2).

      *-----------------------------------------------------------------
       01  UNITS-DIFF                      PIC S9(6)      VALUE ZERO.
       01  OUTBAL-DIFF                     PIC S9(10)V99   VALUE ZERO.

       01  UNITS-PER                       PIC S9(6)V9(7) VALUE ZERO.
       01  OUTBAL-PER                      PIC S9(10)V9(7) VALUE ZERO.

      *=================================================================
      *
      *    PAGE 3 DAILY
      *
      *=================================================================
       01  DAILY-BUF3.
           03  DAILY-PROMPT1             PIC X(80).
           03  FILLER.
               05  DAILY-PROMPT2         PIC X(67).
               05  DAILY-INC-REMAIN-P.
                   07  DAILY-NFR-60      PIC ZZZ,ZZZ,ZZZ-.
               05  FILLER                PIC X.
           03  DAILY-OUTBALS-FILLER.
               05  DAILY-OSBAL           PIC ZZZZ,ZZZ,ZZZ-.
               05  DAILY-OSNO            PIC ZZZZ,ZZZ-.
               05  DAILY-OSAVG           PIC ZZZZZ.

               05  DAILY-G-L-BAL         PIC ZZZ,ZZZ,ZZZ-.
               05  DAILY-G-L-NO          PIC ZZZZ,ZZZ-.

               05  DAILY-AMT-PL          PIC ZZZZZZ,ZZZ-.
               05  DAILY-NO-PL           PIC ZZZZZZ-.
               05  FILLER                PIC X.
               05  DAILY-INC-REMAIN      PIC ZZZ,ZZZ,ZZZ-.
               05  DAILY-NFR-90          REDEFINES DAILY-INC-REMAIN
                                         PIC ZZZ,ZZZ,ZZZ-.
               05  FILLER                PIC X.
           03  DAILY-PROMPT3             PIC X(80).
           03  DAILY-PROMPT4             PIC X(80).
           03  DAILY-VOLS-FILLER.
               05  DAILY-VOL-AVG-LN      PIC ZZZZ,ZZZ.
               05  DAILY-VOL-AVG-TM      PIC ZZZZ.
               05  DAILY-VOL-BAL         PIC ZZZ,ZZZ,ZZZ.

               05  DAILY-NB-NO           PIC ZZZZZZ-.
               05  DAILY-NB-PER          PIC ZZZ.Z-.

               05  DAILY-FB-NO           PIC ZZZZZZ-.
               05  DAILY-RN-PER          PIC ZZZ.Z-.

               05  DAILY-PB-NO           PIC ZZZZZZ-.
               05  DAILY-PB-PER          PIC ZZZ.Z-.

               05  DAILY-NB-AVG          PIC ZZZZZ-.
               05  DAILY-FB-AVG          PIC ZZZZZ-.
               05  DAILY-PB-AVG          PIC ZZZZZ-.
           03  DAILY-PROMPT5             PIC X(80).
           03  DAILY-TABLE               OCCURS 12.
               05  DAILY-DELTBL.
                   07  DAILY-LN-PROMPT   PIC X(5).
                   07  DAILY-LN          PIC X(75).
                   07  DAILY-DELINQ-FILLER   REDEFINES DAILY-LN.
                       09  DAILY-REC-CON     OCCURS 2.
                           11  DAILY-UNITS   PIC Z(6)9-  BLANK ZERO.
                           11  DAILY-T-UNITS REDEFINES DAILY-UNITS
                                             PIC Z(6)9-.
                           11  FILLER        REDEFINES DAILY-UNITS.
                               13  DAILY-UNITS-X    PIC X(7).
                               13  DAILY-UNITS-SIGN PIC X.
                           11  FILLER         PIC X.
                           11  DAILY-P-UNITS  PIC ZZ9.9- BLANK ZERO.
                           11  FILLER         REDEFINES DAILY-P-UNITS.
                               13  DAILY-P-UNITS-X    PIC X(5).
                               13  DAILY-P-UNITS-SIGN PIC X.
                           11  FILLER         PIC X.

                           11  DAILY-OUTBAL   PIC Z,ZZZ,ZZZ,ZZZ-
                                                         BLANK ZERO.
                           11  DAILY-T-OUTBAL REDEFINES DAILY-OUTBAL
                                              PIC Z,ZZZ,ZZZ,ZZ9-.
                           11  FILLER         REDEFINES DAILY-OUTBAL.
                               13  DAILY-OUTBAL-X    PIC X(13).
                               13  DAILY-OUTBAL-SIGN PIC X.
                           11  FILLER         PIC X.
                           11  DAILY-P-OUTBAL PIC ZZ9.9- BLANK ZERO.
                           11  FILLER         REDEFINES DAILY-P-OUTBAL.
                               13  DAILY-P-OUTBAL-X    PIC X(5).
                               13  DAILY-P-OUTBAL-SIGN PIC X.

      *-----------------------------------------------------------------
       01  DAILY-LIST3-TOP.
           03  FILLER              PIC X(5)  VALUE
           "P1:P ".
           03  FILLER              PIC X(12) VALUE
           "010:P 011:P ".
           03  DAILY-LIST3-NFR60-T PIC X(7)  VALUE
           "012:RF ".
           03  FILLER              PIC X(12) VALUE
           "013 020 021 ".
           03  DAILY-LIST3-NFR90-T PIC X(7)  VALUE
           "022:RF ".
           03  FILLER       PIC X(28) VALUE
           "023 030:P 031:P 040:P 041:P ".
           03  FILLER       PIC X(40) VALUE
           "050 051 060:P 061:P 070:P 071 080:P 081 ".
           03  FILLER       PIC X(40) VALUE
           "090:P 091 100:P 101 110:P 111 120:P 121 ".
           03  FILLER       PIC X(40) VALUE
           "130:P 131 140:P 141 150:P 151 160 161:P ".
           03  FILLER       PIC X(24) VALUE
           "170 171:P 180:P 181.".

      *-----------------------------------------------------------------
       01  DAILY-LIST3-BOTTOM.
           03  FILLER              PIC X(5)  VALUE
           "P1:P ".
           03  FILLER              PIC X(12) VALUE
           "010:P 011:P ".
           03  DAILY-LIST3-NFR60-B PIC X(7)  VALUE
           "012:RF ".
           03  FILLER              PIC X(12) VALUE
           "013 020 021 ".
           03  DAILY-LIST3-NFR90-B PIC X(7)  VALUE
           "022:RF ".
           03  FILLER       PIC X(28) VALUE
           "023 030:P 031:P 040:P 041:P ".
           03  FILLER       PIC X(40) VALUE
           "050 051 060:P 061:P 070 071:P 080 081:P ".
           03  FILLER       PIC X(40) VALUE
           "090:P 091 100:P 101 110:P 111 120 121:P ".
           03  FILLER       PIC X(40) VALUE
           "130:P 131 140:P 141 150:P 151 160 161:P ".
           03  FILLER       PIC X(24) VALUE
           "170 171:P 180 181:P.".

      *=================================================================
      *
      *    SCREEN 4 FIELDS
      *
      *=================================================================
       01  DISPLAY-BUF4               VALUE " ".
           03  DATA-TYPE-TABLE        OCCURS 5 TIMES.
               05  DATA-NOS-REPO      OCCURS 3 TIMES.
                   07  DATA-NO-REPO   PIC ZZZ,ZZ9-        BLANK ZERO.
               05  FILLER             PIC XX.
               05  DATA-AMTS-REPO     OCCURS 3 TIMES.
                   07  FILLER         PIC X.
                   07  DATA-AMT-REPO  PIC ZZ,ZZZ,ZZ9.99-  BLANK ZERO.

       01  DISPLAY-LIST4.
           03  FILLER                 PIC X(20) VALUE
           "010 020 030 040 050.".

      *=================================================================
      *
      *    SCREEN 4 TOTALS
      *
      *=================================================================
       01  PAGE-4-TOTALS.
           03  SUM-TABLE        OCCURS 5 TIMES.
               05  SUM-NOS-REPO      OCCURS 3 TIMES.
                   07  SUM-NO-REPO   PIC S9(6).
               05  SUM-AMTS-REPO     OCCURS 3 TIMES.
                   07  SUM-AMT-REPO  PIC S9(8)V99.

      *=================================================================
      *
      *    SCREEN DISPLAY BUFFER ----- PAGE 5
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    GNLS = GAIN/LOSS     NVOL = NEW VOLUME
      *=================================================================
      *01  DISPLAY-BUF5.
      *--->03  DATA-APP-TABLE             OCCURS 10.
      *    03  DATA-APP-TABLE             OCCURS 3.
      *        05  DATA-GNLS-PENDING      PIC ZZZZZZ-.
      *        05  DATA-GNLS-COND-APP     PIC ZZZZZZ-.
      *        05  DATA-GNLS-APPROVED     PIC ZZZZZZ-.
      *        05  FILLER                 PIC X(3).
      *        05  DATA-NVOL-PENDING      PIC ZZZZZZ-.
      *        05  DATA-NVOL-COND-APP     PIC ZZZZZZ-.
      *        05  DATA-NVOL-APPROVED     PIC ZZZZZZ-.
      *        05  DATA-PERC-APPR         PIC ZZZZ-.
      *        05  FILLER REDEFINES DATA-PERC-APPR.
      *            07  FILLER             PIC X(4).
      *            07  DATA-PSIGN1        PIC X.
      *        05  DATA-NVOL-MADE         PIC ZZZZZZ-.
      *        05  DATA-PERC-MADE         PIC ZZZZ-.
      *        05  FILLER REDEFINES DATA-PERC-MADE.
      *            07  FILLER             PIC X(4).
      *            07  DATA-PSIGN2        PIC X.
      *        05  DATA-NVOL-TURNDOWN     PIC ZZZZZZ-.
      *        05  DATA-NVOL-TUD          PIC ZZZZZZ-.

      *01  DISPLAY-LIST5.
      *    03  FILLER                     PIC X(12)     VALUE
      *        "010 020 030.".

      *=================================================================
      *
      *    P A G E - 5 - T O T A L S
      *
      *=================================================================
      *01  PAGE-5-TOTALS.
      *    03  SUM-5-TYPE-TBL            OCCURS 3.
      *        05  SUM-5-ACTIVITY-TBL     OCCURS 2.
      *            07  SUM-NO-PENDING       PIC S9(6).
      *            07  SUM-NO-COND-APP      PIC S9(6).
      *            07  SUM-NO-APPROVED      PIC S9(6).
      *            07  SUM-NO-MADE          PIC S9(6).
      *            07  SUM-NO-TURNDOWN      PIC S9(6).
      *            07  SUM-NO-TUD           PIC S9(6).

      *=================================================================
      *
      *    SCREEN DISPLAY BUFFER ----- PAGE 6
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    GNLS = GAIN/LOSS     NVOL = NEW VOLUME
      *=================================================================
       01  DISPLAY-BUF6.
           03  DATA-PERIOD-TABLE                   OCCURS 3.
               05  DATA-ACTIVITY-TABLE             OCCURS 2.
                   07  DATA-TYPE-TABLE             OCCURS 6.
                       09  DISP-UNEARNED-AMT   PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.

       01  DISPLAY-LIST6.
           03  FILLER                     PIC X(48)     VALUE
               "010 020 030 040 050 060 070 080 090 100 110 120 ".
           03  FILLER                     PIC X(48)     VALUE
               "011 021 031 041 051 061 071 081 091 101 111 121 ".
           03  FILLER                     PIC X(48)     VALUE
               "012 022 032 042 052 062 072 082 092 102 112 122.".

      *=================================================================
      *
      *    P A G E - 1 2 - T O T A L S
      *
      *=================================================================
       01  PAGE-6-TOTALS.
           03  SUM-6-PER-TBL            OCCURS 3.
               05  SUM-6-ACT-TBL            OCCURS 2.
                   07  SUM-6-TYP-TBL            OCCURS 6.
                       09  SUM-UNEARNED-AMT PIC S9(12)V99.

      *----------------------------------------------------------------
      * HOLD DS-REC: CONTAINS ACCUMULATED NUMBERS FROM DS-RECORDS READ
      *              IF RUNNING IN SNAPSHOT MODE THIS RECORD IS BUILT ONCE
      *                 AND THEN USED UNTIL A NEW RANGE IS ENTERED.
      *              IF RUNNING IN REAL TIME THIS RECORD IS BUILT EACH
      *                 TIME A NEW ACTIVITY STARTS (NEW PAGE, FUNCTION
      *                 KEY, ETC.
      *----------------------------------------------------------------
       01  HDS-REC.
           03  HDS1-KEY.
               05  HDS-DATE-CCYYMM   PIC 9(6).
               05  HDS2-KEY.
                   07  HDS-BRANCH    PIC 9(4).
                   07  HDS-NUM       PIC 9(6).
                   07  HDS-NUM-RED  REDEFINES HDS-NUM.
                       09  HDS-NO    PIC 9(4).
                       09  HDS-BR    PIC 9(2).
                   07  HDS-DLNO     REDEFINES HDS-NUM
                                    PIC 9(4)V99.
                   07  HDS-CLASS     PIC 9(3).
      *---------------------------------------------------------------
           03  HDS-DATA.
               05  HDS-DLRVPRIN      OCCURS 3
                                    PIC S9(11)V99  COMP.
               05  HDS-DLRVCHGS      OCCURS 3
                                    PIC S9(11)V99  COMP.
               05  HDS-DLPARVHELD    PIC S9(11)V99  COMP.
               05  HDS-UNPAIDCK      PIC S9(11)V99  COMP.
               05  HDS-DLDISC        PIC S9(11)V99  COMP.
      *        THIS FIELD  (HDS-DLPARVPAID) REPRESENTS TOTAL MONIES
      *        PAID TO DEALER IN PARTICIPATION AT CONTRACT PURCHASE.
               05  HDS-DLPARVPAID    PIC S9(11)V99  COMP.
      *---------------------------------------------------------------
               05  HDS-SUMWGTTM      PIC S9(12)     COMP.
               05  HDS-SUMWGTTA      PIC S9(12)V99  COMP.
               05  HDS-SUMWGTAV      PIC S9(14)V99 COMP.

      *---------------------------------------------------------------
      * RECOURSE ACCOUNTS (FULL, PARTIAL & NON)
               05  HDS-RCOURSE-UNITS PIC S9(8)     COMP   OCCURS 3.
               05  HDS-RCOURSE-AMT   PIC S9(12)V99  COMP   OCCURS 3.
      *---------------------------------------------------------------
      *     VOLUME TABLES:
      *        1 - MTD    [DSMTD]
      *        2 - YTD    [DSYTD]
      *        3 - LYR    [DSLYR]
      *---------------------------------------------------------------
      * NEW PURCHASED
               05  HDS-NPURUNIT      PIC S9(8)     COMP   OCCURS 3.
               05  HDS-NPURAMT       PIC S9(11)V99  COMP   OCCURS 3.

      * OTHER PURCHASED
               05  HDS-OPURUNIT      PIC S9(8)     COMP   OCCURS 3.
               05  HDS-OPURAMT       PIC S9(11)V99  COMP   OCCURS 3.

      * RENEWED
               05  HDS-RNEWUNIT      PIC S9(8)     COMP   OCCURS 3.
               05  HDS-RNEWAMT       PIC S9(11)V99  COMP   OCCURS 3.

      * PAID-OFF
               05  HDS-PAIDUNIT      PIC S9(8)     COMP   OCCURS 3.
               05  HDS-PAIDAMT       PIC S9(11)V99  COMP   OCCURS 3.

      * CONVERTED TO DIRECT LOAN
               05  HDS-TODLUNIT      PIC S9(8)     COMP   OCCURS 3.
               05  HDS-TODLAMT       PIC S9(11)V99  COMP   OCCURS 3.
      *---------------------------------------------------------------
      *      NET TABLES:
      *        1 - CURRENT           [DSCUR]
      *        2 - END LAST MONTH    [DSELM]
      *        3 - END LAST YEAR     [DSELY]
      *---------------------------------------------------------------
      * OUTSTANDING
               05  HDS-OUTSUNIT      PIC S9(8)     COMP   OCCURS 3.
               05  HDS-OUTSAMT       PIC S9(11)V99  COMP   OCCURS 3.

      * CHARGED-OFF (P&L)
               05  HDS-PLOSUNIT      PIC S9(8)     COMP   OCCURS 3.
               05  HDS-PLOSAMT       PIC S9(11)V99  COMP   OCCURS 3.

      * RECOVERED  P & L
               05  HDS-RCPLUNIT      PIC S9(8)     COMP   OCCURS 3.
               05  HDS-RCPLAMT       PIC S9(11)V99  COMP   OCCURS 3.

      * JUDGMENTS
               05  HDS-JDOSUNIT      PIC S9(8)     COMP   OCCURS 3.
               05  HDS-JDOSAMT       PIC S9(11)V99  COMP   OCCURS 3.

      *---------------------------------------------------------------
      *     CONTRACTUAL EARNINGS:
      *        1 - CURRENT           [DSCUR]
      *        2 - END LAST MONTH    [DSELM]
      *        3 - END LAST YEAR     [DSELY]
      *---------------------------------------------------------------
               05  HDS-DELUNEDT      PIC 9(9)      COMP.
      *    ------------------------------------------------------
      *    UNEARNED PERIOD TABLE:
      *                   ___________________________________________
      *                   1 - CURRENT                |  1 - MTD
      *                   2 - END LAST MONTH         |  2 - YTD
      *                   3 - END LAST YEAR          |  3 - LYR
      *                                              |
      *    UNEARNED ACT:  1 - NET GAIN/LOSS          |  2 - NEW VOLUME
      *                   ____________________________________________________
      *    UNEARNED TABLE: 1 - INTEREST
      *                    2 - ADDON
      *                    3 - SERVICE CHARGE
      *                    4 - MAINTENANCE FEES
      *                    5 - DEALER DISCOUNT
      *                    6 - OTHER
      *    ------------------------------------------------------
                   05  HDS-UNEARNED-PERIOD           OCCURS 3.
                       07  HDS-UNEARNED-ACTIVITY     OCCURS 2.
                           09  HDS-UNEARNED-TYPE     OCCURS 6.
                               11 HDS-UNEARNED-AMT  PIC S9(11)V99 COMP.

      *-------------------------------------------------------------------
      *    REPO TABLE:
      *    PERIOD TABLE:  ________________________________________________
      *                   1 - CURRENT        [DSCUR] | 1 - MTD     [DSMTD]
      *                   2 - END LAST MONTH [DSELM] | 2 - YTD     [DSYTD]
      *                   3 - END LAST YEAR  [DSELY] | 3 - LYR     [DSLYR]
      *
      *    ACTIVITY TBL:  1 - NET GAIN/LOSS  [DSNET]   2 - NEW VOL [DSVOL]
      *
      *    TYPE TABLE  :  1 - REPO ON HAND
      *                   2 - REPO REDEEMED
      *                   3 - REPO SOLD LIVE RECEIVABLE
      *                   4 - REPO SOLD P&L ACTIVE
      *                   5 - REPO SOLD P&L INACTIVE
      *
      *-------------------------------------------------------------------
               05  HDS-REPO-PERIOD-TBL         OCCURS 3.
                   07  HDS-REPO-ACTIVITY-TBL   OCCURS 2.
                       09  HDS-REPO-TYPE-TBL   OCCURS 5.
                           11  HDS-NO-REPO     PIC S9(7)    COMP.
                           11  HDS-AMT-REPO    PIC S9(11)V99 COMP.
      *---------------------------------------------------------------
      *    HDS-NOW-BMO-LME
      *        1- NOW   (CURRENT)         [DSNOW]
      *        2- BEGINNING OF MONTH      [DSBMO]
      *        3- LAST MONTH END          [DSLME]
      *
      *    HDS-REC-CON
      *        1- RECENCY                 [DSREC]
      *        2- CONTRACTUAL             [DSCON]
      *
      *    HDS-DEL-BUCKET
      *        1- 30
      *        2- 60
      *        3- 90
      *        4- 120
      *        5- 150
      *        6- 180
      *        7- 210+
      *        8- <M>
      *        9- <P>
      *       10- CURRENT
      *
      *    -------------------------------------------------
               05  HDS-NOW-BMO-LME          OCCURS 3.
                   07  HDS-REC-CON          OCCURS 2.
                       09  HDS-DEL-BUCKET   OCCURS 10.
      *                DELINQUENT UNITS:
                           11  HDS-UNITS    PIC S9(11)    COMP.
      *                OUTSTANDING BALANCE:
                           11  HDS-OUTBAL   PIC S9(12)V99 COMP.

      *---------------------------------------------------------------
      *    NOTE: THESE FIELDS ARE BANKRUPT, NON P&L, NON REPO ACCOUNTS
      *
      *    HDS-NOW-BMO-LME-BNKRPT
      *        1- NOW   (CURRENT)         [DSNOW]
      *        2- BEGINNING OF MONTH      [DSBMO]
      *        3- LAST MONTH END          [DSLME]
      *
      *    HDS-REC-CON-BNKRPT
      *        1- RECENCY                 [DSREC]
      *        2- CONTRACTUAL             [DSCON]
      *
      *    HDS-DEL-BUCKET-BNKRPT
      *        1- 30
      *        2- 60
      *        3- 90
      *        4- 120
      *        5- 150
      *        6- 180
      *        7- 210+
      *        8- <M>
      *        9- <P>
      *       10- CURRENT
      *
      *    -------------------------------------------------
               05  HDS-NOW-BMO-LME-BNKRPT          OCCURS 3.
                   07  HDS-REC-CON-BNKRPT          OCCURS 2.
                       09  HDS-DEL-BUCKET-BNKRPT   OCCURS 10.
      *                DELINQUENT UNITS:
                           11  HDS-UNITS-BNKRPT    PIC S9(11)    COMP.
      *                OUTSTANDING BALANCE:
                           11  HDS-OUTBAL-BNKRPT   PIC S9(12)V99 COMP.

           03  HDS-AFPDUEDL-MEMO            PIC S9(12)V99  COMP.


      *-----------------------------------------------------------------
      *    DLDISC-VOL-TBL: 1 - MTD                  NEW VOLUMES
      *                    2 - YTD
      *                    3 - LYR
      *-----------------------------------------------------------------
           03  HDS-DLDISC-VOL       PIC S9(12)V99 COMP OCCURS 3.

           03  HDS-PROCEEDS         PIC S9(12)V99 OCCURS 2.



      *--------------------------------------------------------------- 
      * HOLD DY-REC: CONTAINS ACCUMULATED NUMBERS FROM DY-RECORDS READ
      *              IF RUNNING IN SNAPSHOT MODE THIS RECORD IS BUILT ONCE
      *                 AND THEN USED UNTIL A NEW RANGE IS ENTERED.
      *              IF RUNNING IN REAL TIME THIS RECORD IS BUILT EACH
      *                 TIME A NEW ACTIVITY STARTS (NEW PAGE, FUNCTION
      *                 KEY, ETC.
      *----------------------------------------------------------------
       01  HDY-REC.
      *---------------------------------------------------------------
           03  HDY-DATA.
               05  HDY-DLRVPRIN      OCCURS 3
                                    PIC S9(11)V99  COMP.
               05  HDY-DLRVCHGS      OCCURS 3
                                    PIC S9(11)V99  COMP.
               05  HDY-DLPARVHELD    PIC S9(11)V99  COMP.
               05  HDY-UNPAIDCK      PIC S9(11)V99  COMP.
               05  HDY-DLDISC        PIC S9(11)V99  COMP.
      *        THIS FIELD  (HDY-DLPARVPAID) REPRESENTS TOTAL MONIES
      *        PAID TO DEALER IN PARTICIPATION AT CONTRACT PURCHASE.
               05  HDY-DLPARVPAID    PIC S9(8)V99  COMP.
      *---------------------------------------------------------------
               05  HDY-SUMWGTTM      PIC S9(12)     COMP.
               05  HDY-SUMWGTTA      PIC S9(12)V99  COMP.
               05  HDY-SUMWGTAV      PIC S9(14)V99 COMP.

      *---------------------------------------------------------------
      * RECOURSE ACCOUNTS (FULL, PARTIAL & NON)
               05  HDY-RCOURSE-UNITS PIC S9(8)     COMP   OCCURS 3.
               05  HDY-RCOURSE-AMT   PIC S9(12)V99  COMP   OCCURS 3.
      *---------------------------------------------------------------
      *     VOLUME TABLES:
      *        1 - MTD    [DSMTD]
      *---------------------------------------------------------------
      * NEW PURCHASED
               05  HDY-NPURUNIT      PIC S9(8)     COMP.
               05  HDY-NPURAMT       PIC S9(11)V99  COMP.

      * OTHER PURCHASED
               05  HDY-OPURUNIT      PIC S9(8)     COMP.
               05  HDY-OPURAMT       PIC S9(11)V99  COMP.

      * RENEWED
               05  HDY-RNEWUNIT      PIC S9(8)     COMP.
               05  HDY-RNEWAMT       PIC S9(11)V99  COMP.

      * PAID-OFF
               05  HDY-PAIDUNIT      PIC S9(8)     COMP.
               05  HDY-PAIDAMT       PIC S9(11)V99  COMP.

      * CONVERTED TO DIRECT LOAN
               05  HDY-TOLNUNIT      PIC S9(8)     COMP.
               05  HDY-TOLNAMT       PIC S9(11)V99  COMP.
      *---------------------------------------------------------------
      *      NET TABLES:
      *        1 - CURRENT           [DSCUR]
      *---------------------------------------------------------------
      * OUTSTANDING
               05  HDY-OUTSUNIT      PIC S9(8)     COMP.
               05  HDY-OUTSAMT       PIC S9(11)V99  COMP.

      * CHARGED-OFF (P&L)
               05  HDY-PLOSUNIT      PIC S9(8)     COMP.
               05  HDY-PLOSAMT       PIC S9(11)V99  COMP.

      * RECOVERED  P & L
               05  HDY-RCPLUNIT      PIC S9(8)     COMP.
               05  HDY-RCPLAMT       PIC S9(11)V99  COMP.

      * JUDGMENTS
               05  HDY-JDOSUNIT      PIC S9(8)     COMP.
               05  HDY-JDOSAMT       PIC S9(11)V99  COMP.

      *---------------------------------------------------------------
      *     CONTRACTUAL EARNINGS:
      *        1 - CURRENT           [DSCUR]
      *---------------------------------------------------------------
               05  HDY-DELUNEDT      PIC 9(9)      COMP.
      *    ------------------------------------------------------
      *    UNEARNED PERIOD TABLE:
      *                   ___________________________________________
      *                       CURRENT                |      MTD
      *                                              |
      *    UNEARNED ACT:  1 - NET GAIN/LOSS          |  2 - NEW VOLUME
      *                   ____________________________________________________
      *    UNEARNED TABLE: 1 - INTEREST
      *                    2 - ADDON
      *                    3 - SERVICE CHARGE
      *                    4 - MAINTENANCE FEES
      *                    5 - DEALER DISCOUNT
      *                    6 - OTHER
      *    ------------------------------------------------------
               05  HDY-UNEARNED-ACTIVITY        OCCURS 2.
                   07  HDY-UNEARNED-TYPE        OCCURS 6.
                       09  HDY-UNEARNED-AMT     PIC S9(11)V99 COMP.

      *-------------------------------------------------------------------
      *    REPO TABLE:
      *
      *    ACTIVITY TBL:  1 - NET GAIN/LOSS  [DSNET]   2 - NEW VOL [DSVOL]
      *
      *    TYPE TABLE  :  1 - REPO ON HAND
      *                   2 - REPO REDEEMED
      *                   3 - REPO SOLD LIVE RECEIVABLE
      *                   4 - REPO SOLD P&L ACTIVE
      *                   5 - REPO SOLD P&L INACTIVE
      *
      *-------------------------------------------------------------------
                05  HDY-REPO-ACTIVITY-TBL   OCCURS 2.
                    07  HDY-REPO-TYPE-TBL   OCCURS 5.
                        09  HDY-NO-REPO     PIC S9(7)    COMP.
                        09  HDY-AMT-REPO    PIC S9(10)V99 COMP.
      *---------------------------------------------------------------
      *    HDY-NOW-BMO-LME
      *
      *    HDY-REC-CON
      *        1- RECENCY                 [DSREC]
      *        2- CONTRACTUAL             [DSCON]
      *
      *    HDY-DEL-BUCKET
      *        1- 30
      *        2- 60
      *        3- 90
      *        4- 120
      *        5- 150
      *        6- 180
      *        7- 210+
      *        8- <M>
      *        9- <P>
      *       10- CURRENT
      *
      *    -------------------------------------------------
               05  HDY-REC-CON          OCCURS 2.
                   07  HDY-DEL-BUCKET   OCCURS 10.
      *            DELINQUENT UNITS:
                       09  HDY-UNITS    PIC S9(10)    COMP.
      *            OUTSTANDING BALANCE:
                       09  HDY-OUTBAL   PIC S9(12)V99 COMP.

      *---------------------------------------------------------------
      *    NOTE: THESE FIELDS ARE BANKRUPT, NON P&L, NON REPO ACCOUNTS
      *    HDY-NOW-BMO-LME
      *
      *    HDY-REC-CON-BANKRPT
      *        1- RECENCY                 [DSREC]
      *        2- CONTRACTUAL             [DSCON]
      *
      *    HDY-DEL-BUCKET-BNKRPT
      *        1- 30
      *        2- 60
      *        3- 90
      *        4- 120
      *        5- 150
      *        6- 180
      *        7- 210+
      *        8- <M>
      *        9- <P>
      *       10- CURRENT
      *
      *    -------------------------------------------------
               05  HDY-REC-CON-BNKRPT          OCCURS 2.
                   07  HDY-DEL-BUCKET-BNKRPT   OCCURS 10.
      *            DELINQUENT UNITS:
                       09  HDY-UNITS-BNKRPT    PIC S9(10)    COMP.
      *            OUTSTANDING BALANCE:
                       09  HDY-OUTBAL-BNKRPT   PIC S9(12)V99 COMP.

           03  HDY-AFPDUEDL-MEMO            PIC S9(11)V99  COMP.

      *-------------------- APPLICATIONS -------------------------------
      *    ACTIVITY TBL:  1 - NET GAIN/LOSS  [DSNET] |  2 - NEW VOLUME [DSVOL]
      *                   ____________________________________________________
      *    TYPE TABLE  :  1 - NB
      *                   2 - OTH
      *    NOTE: STATISTICS FOR:
      *            APPS MADE IS THE ACTUAL LOAN VOLUME CARRIED UNDER BY-NO
      *            APPS NOT TAKEN IS NOT EASILY MAINTAINED
      *
      *-----------------------------------------------------------------
           03  HDY-APP-ACTIVITY-TBL        OCCURS 2.
               05  HDY-APP-TYPE-TBL        OCCURS 2.
                   07  HDY-NO-PENDING      PIC S9(8)    COMP.
                   07  HDY-NO-COND-APP     PIC S9(8)    COMP.
                   07  HDY-NO-APPROVED     PIC S9(8)    COMP.
                   07  HDY-NO-MADE         PIC S9(8)    COMP.
                   07  HDY-NO-TURNDOWN     PIC S9(8)    COMP.
                   07  HDY-NO-TURNUSDOWN   PIC S9(8)    COMP.

      *-----------------------------------------------------------------
      *    DLDISC-VOL-TBL:                          NEW VOLUMES
      *-----------------------------------------------------------------
           03 HDY-DLDISC-VOL                   PIC S9(12)V99 COMP.
           03 HDY-PROCEEDS                     PIC S9(12)V99.


      ******************************************************************
       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBLP/DSINQ_CMD.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/DSINQ1_DEF.CPY".
       COPY "LIBLP/DSINQ2_DEF.CPY".
       COPY "LIBLP/DSINQ3_DEF.CPY".
       COPY "LIBLP/DSINQ4_DEF.CPY".
       COPY "LIBLP/DSINQ5_DEF.CPY".
       COPY "LIBLP/DSINQ6_DEF.CPY".
       COPY "LIBLP/DSINQ1_WKS.CPY".
       COPY "LIBLP/DSINQ2_WKS.CPY".
       COPY "LIBLP/DSINQ3_WKS.CPY".
       COPY "LIBLP/DSINQ4_WKS.CPY".
       COPY "LIBLP/DSINQ5_WKS.CPY".
       COPY "LIBLP/DSINQ6_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  FORM-LINK-AREA     PIC X(2000).

       SCREEN SECTION.
       COPY "LIBLP/DSINQ1_SCN.CPY".
       COPY "LIBLP/DSINQ2_SCN.CPY".
       COPY "LIBLP/DSINQ3_SCN.CPY".
       COPY "LIBLP/DSINQ4_SCN.CPY".
       COPY "LIBLP/DSINQ5_SCN.CPY".
       COPY "LIBLP/DSINQ6_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *****************************************************************
      *
      *           MAIN PROGRAM MODULE
      *
      *****************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE DAILY-PROMPTS-I-AM-WORLD TO DAILY-PROMPTS-I-AM.

           MOVE EXT-CONAME-SYSDATE TO TODAYS-DATE.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE 3 TO BR-DSINQ-START-PAGE.

           MOVE BR-DSINQ-START-PAGE TO PAGE-FLAG.
           MOVE FORM-BUFFER(PAGE-FLAG) TO VDU-FORM-NAME.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE HOLD-FORM-NAM TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
      *----------------------------------------------------------------
      * GET TYPE OF RUN: 1 -  SNAPSHOT,  2 - REALTIME.
      *                  IF VALUE UNAVAILABLE DEFAULT TO SNAPSHOT.
      *----------------------------------------------------------------

           IF EXT-MENU-OPTION-FG = "2"
              MOVE 2 TO READ-TYPE
           ELSE
              MOVE 1 TO READ-TYPE.


           PERFORM FORM-RESET.

       MAIN-MODULE.
      D    STOP MESS.

      *----------------------------------------------------------------
      * DISPLAY TYPE OF RUN: SNAPSHOT OR REALTIME.
      *----------------------------------------------------------------
           MOVE INQUIRY-TYPE(READ-TYPE) TO WR-BUF.
           MOVE INQUIRY-LIST(READ-TYPE) TO WR-LIST.
           PERFORM CALL-WRFORM.

      *    PERFORM FORM-RESET.

           PERFORM REC-DS-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           PERFORM ENTRY-MODULE.
           GO TO MAIN-MODULE.

      ******************************************************************
       REC-DS-MODULE SECTION.
      ******************************************************************

           MOVE 1 TO PAGE5-GNLS
                     PAGE5-NVOL.
           PERFORM NEW-PAGE5-PERIOD-SETUP.

      *-----------------------------------------------------------------
       REC-DS-MODULE-NO.
           MOVE "O" TO ALL-CLASSES-FLAG.
           MOVE NNO TO ALL-DEALERS-FLAG.
           MOVE ACTIVITY TO SUB2.
           MOVE " " TO IO-TYPE.

           PERFORM GET-TODAYS-DATE.
           GO TO SETUP-DS-DATE-CCYYMM.
       ENTER-DATE.
           MOVE "ENNN040MMYY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-DS-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DATE.
           MOVE VDUNUM0 TO STORE-DATE DISPLAY-DATE.
           IF STORE-MO < 1 OR STORE-MO > 12
              GO TO ENTER-DATE.
       SETUP-DS-DATE-CCYYMM.
           MOVE STORE-MO TO DATE-MMDDYY-MM.
           MOVE 1        TO DATE-MMDDYY-DD.
           MOVE STORE-YR TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO DS-DATE-CCYYMM HOLD-DS-DATE-CCYYMM.
       ENTER-BRANCH.
           MOVE "E  A040BRNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-DS-MODULE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-DATE.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO ENTER-BRANCH
              ELSE
                 MOVE "S  A000BRNO." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF
                 PERFORM SCREENIO
                 GO TO READ-GROUP-FILE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-BRANCH.

       READ-GROUP-FILE.
           MOVE VDUBUF TO WGR-WORD
                          ENT-GROUP.
           IF WGR-WORD = SPACES
              GO TO ENTER-BRANCH.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
                   AND WGR-ERR NOT = 1
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.
           MOVE WGR-LEVEL TO SAVE-LEVEL.

           PERFORM OPEN-BR-FILE.

           IF WGR-TYPE NOT = "A"
              GO TO BRANCH-NUMBER-ENTERED.

           MOVE WGR-WORD TO ENT-GROUP.
           MOVE GR-NAME TO BR-NAME HOLD-GR-NAME.
           MOVE EXT-CONAME-USER-GROUP TO CASE-BUF.
           CALL "C$TOUPPER" USING CASE-BUF, VALUE 4.
           MOVE CASE-BUF TO WGR-WORD.
           IF WGR-WORD = ENT-GROUP-1ST-3
              GO TO GROUP-OKAY.

           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              GO TO PASSWORD-REQUIRED.

           PERFORM OPEN-GR1-FILE.
           MOVE WGR-WORD TO GR-ID.
           PERFORM SEARCH-GR-INIT.
           MOVE ENT-GROUP TO WGR-TEST-GROUP.
           PERFORM VERIFY-GROUP-MEMBER.
           IF WGR-ERR > 0
              GO TO PASSWORD-REQUIRED.
           GO TO GROUP-OKAY.

       PASSWORD-REQUIRED.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              MOVE
           "VIEWING THIS GROUP IS NOT ALLOWED WITHOUT PASSWORD!!!"
                                                TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

       GROUP-OKAY.
           MOVE "Y" TO GROUP-FLAG.
           MOVE 0 TO STORE-BRNO.
           PERFORM SEND-BR-NAME.
           GO TO ENTER-DLNO.

       BRANCH-NUMBER-ENTERED.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO STORE-BRNO
                               BR-NO
                               DS-BRANCH.
           MOVE "SNNN040BRNO." TO VDU.
           MOVE STORE-BRNO TO VDUNUM0.
           PERFORM SCREENIO.
           IF STORE-BRNO = 0
              GO TO ENTER-BRANCH.

           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO DL-PATH-OVERRIDE
                                              DS-PATH-OVERRIDE
                                              RC-PATH-OVERRIDE.


           PERFORM GET-FIRST-OPEN-DATE.

           PERFORM SEND-BR-NAME.
           IF STORE-BRNO = EXT-CONAME-USER-LOC
              GO TO BRANCH-OKAY.

           MOVE EXT-CONAME-USER-GROUP TO CASE-BUF.
           CALL "C$TOUPPER" USING CASE-BUF, VALUE 4.
           MOVE CASE-BUF TO WGR-WORD.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              GO TO PASSWORD-REQUIRED-BR.

           PERFORM OPEN-GR1-FILE.
           MOVE WGR-WORD TO GR-ID.
           PERFORM SEARCH-GR-INIT.
           MOVE STORE-BRNO TO WGR-SRESULT.
           PERFORM SEARCH-GR-VERIFY.
           IF WGR-ERR > 0
              GO TO PASSWORD-REQUIRED-BR.
           GO TO BRANCH-OKAY.
       PASSWORD-REQUIRED-BR.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              MOVE
           "VIEWING THIS BRANCH IS NOT ALLOWED WITHOUT PASSWORD!!!"
                                                TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

       BRANCH-OKAY.
           PERFORM OPEN-DS1-FILE.

       ENTER-DLNO.
           MOVE "S  A025DLNAME." TO VDU.
           MOVE SPACES TO VDUBUF STORE-DLNAME.
           PERFORM SCREENIO.

           MOVE "ENNN042DLNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" OR "10"
              GO TO ENTER-BRANCH.
           IF VDUSTATUS = "16"
              MOVE YYES TO ALL-DEALERS-FLAG
              MOVE "S  A007DLNO." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE 0 TO DS-DLNO
              MOVE "S  A025DLNAME." TO VDU
              MOVE "ALL DEALERS" TO VDUBUF STORE-DLNAME
              PERFORM SCREENIO
              GO TO GET-CLASS.
           IF VDUSTATUS = "1<"
              PERFORM DEALER-SCAN
              IF IO-FG = 0
                 MOVE "00" TO VDUSTATUS
              ELSE
                 GO TO ENTER-DLNO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DLNO.
           MOVE VDUNUM2 TO STORE-DLNO DL-DLNO
           MOVE "SN N042DLNO." TO VDU.
           MOVE STORE-DLNO TO VDUNUM2.
           PERFORM SCREENIO.

      *    -------------------------------------------------------------
      *    IF GROUP RANGE IS SELECTED WITH A SINGLR DEALER, DO NOT
      *    DISPLAY THE DEALERS NAME (COULD BE DIFFERENT IN EACH BRANCH)
      *    -------------------------------------------------------------
           IF GROUP-FLAG = "Y"
              MOVE SPACES TO DL-NAME
           ELSE
              PERFORM GET-DL
              IF IO-FG NOT = 0
                 GO TO ENTER-DLNO.

           MOVE "S  A025DLNAME." TO VDU
           MOVE DL-NAME TO VDUBUF STORE-DLNAME
           PERFORM SCREENIO.

           MOVE NNO TO ALL-DEALERS-FLAG.

       GET-CLASS.
           MOVE "S  A170CLSDESC." TO VDU.
           PERFORM SCREENIO.

           MOVE "ENRA030CLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-DLNO.
           IF VDUSTATUS = "10"
              GO TO ENTER-BRANCH.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-DS-MODULE.
           IF VDUSTATUS = "16"
              MOVE YYES TO ALL-CLASSES-FLAG
              MOVE 001 TO DS-CLASS
              MOVE "S  A170CLSDESC." TO VDU
              MOVE "ALL CLASSES" TO VDUBUF STORE-LCNAME
              PERFORM SCREENIO
              GO TO TEST-ALL.
           IF VDUSTATUS = "1<"
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO GET-CLASS.
           IF VDUSTATUS NOT = "00"
              GO TO GET-CLASS.

           MOVE VDUBUF TO ENTER-CLASS-ALPHA.
           IF ENTER-CLASS-ALPHA NUMERIC
              GO TO ONE-CLASS-ENTERED.

           IF CLASS-WK3 = "S"
              MOVE "S" TO ALL-CLASSES-FLAG
              MOVE "S  A020CLSDESC." TO VDU
              MOVE "SALES FINANCE" TO VDUBUF STORE-LCNAME
              PERFORM SCREENIO
              GO TO TEST-ALL.
           IF CLASS-WK3 = "L"
              MOVE "L" TO ALL-CLASSES-FLAG
              MOVE "S  A020CLSDESC." TO VDU
              MOVE "DIRECT LOANS" TO VDUBUF STORE-LCNAME
              PERFORM SCREENIO
              GO TO TEST-ALL.
           IF CLASS-WK3 = "A"
              MOVE "A" TO ALL-CLASSES-FLAG
              MOVE "S  A020CLSDESC." TO VDU
              MOVE "AFP LOANS" TO VDUBUF STORE-LCNAME
              PERFORM SCREENIO
              GO TO TEST-ALL.
      *
      *    MOVE CLASS-WK1 TO CLASS-WK2.
      *    MOVE 0 TO CLASS-WK1.
           INSPECT ENTER-CLASS-ALPHA REPLACING ALL " " BY "0".
           IF ENTER-CLASS-ALPHA NUMERIC
              MOVE "SN N030CLASS." TO VDU
              MOVE ENTER-CLASS-WK TO VDUNUM0
              PERFORM SCREENIO
              GO TO ONE-CLASS-ENTERED.
           GO TO GET-CLASS.

       ONE-CLASS-ENTERED.
           MOVE ENTER-CLASS-WK TO DS-CLASS
                                  STORE-CLASS
                                  LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG = 9
              MOVE "INVALID CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO GET-CLASS.
           MOVE "S  A020CLSDESC." TO VDU.
           MOVE LC-DESC TO VDUBUF STORE-LCNAME.
           PERFORM SCREENIO.
           MOVE "O" TO ALL-CLASSES-FLAG.

       TEST-ALL.
           IF GROUP-FLAG = "Y"
              PERFORM OPEN-GR1-FILE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT.
           PERFORM SUMMARY-MODULE.
           PERFORM DISPLAY-PAGE.

       END-REC-DS-MODULE.
           EXIT.

      **************************************************
      *
      *  READ TYPES:
      *
      *    READ-ONCE    1 - SNAPSHOT MODE
      *    READ-EVERY   2 - REALTIME MODE
      *
      *    READ-ONCE-FG "Y/N" SET AT END OF READ SO NO ADDITIONAL READ ARE
      *                  MADE (SNAPSHOT MODE).
      **************************************************
       SUMMARY-MODULE SECTION.
           IF READ-ONCE
              IF READ-ONCE-FG = "Y"
                 IF GROUP-FLAG = "Y"
                    MOVE HOLD-GR-NAME TO BR-NAME
                 END-IF
                 GO TO EXIT-SUMMARY-MODULE.

           MOVE "CALCULATING SUMMARY INFO - STANDBY" TO MESS.
           PERFORM CALL-TMMSG.

      *----------------------------------------------------------------
      * CLEAR HOLD BY/DY FILES.
      *----------------------------------------------------------------
           PERFORM CLEAR-HDS-FILE.
           PERFORM CLEAR-HDY-FILE.

           IF GROUP-FLAG = "Y"
              GO TO INC-NEXT-RECORD-KEY.

           MOVE STORE-BRNO TO DS-BRANCH.

           IF ALL-DEALERS
              MOVE 0       TO DS-DLNO
                              QDS1-WBEG-NUM
              MOVE ALL "9" TO QDS1-WEND-NUM
           ELSE
              MOVE STORE-DLNO TO DS-DLNO
              MOVE DS-NUM     TO QDS1-WBEG-NUM
                                 QDS1-WEND-NUM.
           IF NOT ONE-CLASS
              MOVE 1       TO DS-CLASS
                              QDS1-WBEG-CLASS
              MOVE ALL "9" TO QDS1-WEND-CLASS
           ELSE
              MOVE STORE-CLASS TO DS-CLASS
                                  QDS1-WBEG-CLASS
                                  QDS1-WEND-CLASS.

           MOVE DS-PATH-OWNBR        TO QDS1-WBEG-BRANCH
           MOVE DS-PATH-OWNBR        TO QDS1-WEND-BRANCH
           MOVE DS-DATE-CCYYMM       TO QDS1-WBEG-DATE-CCYYMM
                                        QDS1-WEND-DATE-CCYYMM.
           PERFORM START-DS1-FILE.

           GO TO SKIP-GROUP-LOGIC.

       INC-NEXT-RECORD-KEY.
           IF GROUP-FLAG NOT = "Y"
              GO TO SKIP-GROUP-LOGIC.
           IF FIRST-READ = "N"
              GO TO NEXT-GROUP-DS.

           MOVE "N" TO FIRST-READ.
       NEXT-GROUP-LOC.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO EXIT-SUMMARY-MODULE.

           MOVE HOLD-DS-DATE-CCYYMM TO DS-DATE-CCYYMM.

           MOVE WGR-SRESULT TO DS-BRANCH BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO NEXT-GROUP-LOC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO DL-PATH-OVERRIDE
                                              DS-PATH-OVERRIDE
                                              RC-PATH-OVERRIDE.

           PERFORM GET-FIRST-OPEN-DATE.

           PERFORM OPEN-DS1-FILE.
           IF ALL-DEALERS
              MOVE 0      TO DS-DLNO
                             QDS1-WBEG-NUM
              MOVE ALL "9" TO QDS1-WEND-NUM
           ELSE
              MOVE STORE-DLNO TO DS-DLNO
              MOVE DS-NUM     TO QDS1-WBEG-NUM
                                 QDS1-WEND-NUM.
           IF NOT ONE-CLASS
              MOVE 1       TO DS-CLASS
                              QDS1-WBEG-CLASS
              MOVE ALL "9" TO QDS1-WEND-CLASS
           ELSE
              MOVE STORE-CLASS TO DS-CLASS
                                  QDS1-WBEG-CLASS
                                  QDS1-WEND-CLASS.

           MOVE DS-PATH-OWNBR        TO QDS1-WBEG-BRANCH
           MOVE DS-PATH-OWNBR        TO QDS1-WEND-BRANCH
           MOVE DS-DATE-CCYYMM       TO QDS1-WBEG-DATE-CCYYMM
                                        QDS1-WEND-DATE-CCYYMM.

           PERFORM START-DS1-FILE.
       NEXT-GROUP-DS.
           PERFORM READ-DS1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (DS-BRANCH NOT = WGR-SRESULT)
              PERFORM CLOSE-DS1-FILE
              GO TO NEXT-GROUP-LOC.

           IF DS-DATE-CCYYMM NOT = HOLD-DS-DATE-CCYYMM
              PERFORM CLOSE-DS1-FILE
              GO TO NEXT-GROUP-LOC.

           IF ONE-DEALER
              IF DS-DLNO NOT = STORE-DLNO
                 PERFORM CLOSE-DS1-FILE
                 GO TO NEXT-GROUP-LOC.
           IF ONE-CLASS
              IF DS-CLASS NOT = STORE-CLASS
                 GO TO NEXT-GROUP-DS
              ELSE
                 GO TO ACCUM-DS-TOTALS.

           PERFORM TEST-CLASS.
           IF CORRECT-CLASS = "N"
              GO TO NEXT-GROUP-DS.

           GO TO ACCUM-DS-TOTALS.

      *-----------------------------------------------------------------
      * SKIP GROUP LOGI
      *  PERFORMED WHEN ONLY ONE BRANCH IS SELECTED
      *  ONE BRANCH/ONE OR ALL DEALERS/ONE OR ALL CLASSES
      *-----------------------------------------------------------------
       SKIP-GROUP-LOGIC.
       READ-NEXT-REC.
           PERFORM READ-DS1-FILE-NEXT.
           IF IO-FG = 9
              IF E-CODE NOT = "00"
                 GO TO EXIT-SUMMARY-MODULE.

           IF DS-BRANCH NOT = STORE-BRNO
              GO TO EXIT-SUMMARY-MODULE.

           IF DS-DATE-CCYYMM NOT = HOLD-DS-DATE-CCYYMM
              GO TO EXIT-SUMMARY-MODULE.


           IF ONE-DEALER
              IF DS-DLNO NOT = STORE-DLNO
                 GO TO EXIT-SUMMARY-MODULE.

           IF ONE-CLASS
              IF DS-CLASS NOT = STORE-CLASS
                 GO TO READ-NEXT-REC
              ELSE
                 GO TO ACCUM-DS-TOTALS.

           PERFORM TEST-CLASS.
           IF CORRECT-CLASS = "N"
              GO TO READ-NEXT-REC.

       ACCUM-DS-TOTALS.

      *----------------------------------------------------------------
      * ACCUMULATE BY/DY HOLD FILES.
      *----------------------------------------------------------------
           PERFORM ADD-DS-TO-HDS.

           IF GROUP-FLAG = "Y"
              GO TO NEXT-GROUP-DS.
           IF NOT ONE-CLASS OR ALL-DEALERS
              GO TO INC-NEXT-RECORD-KEY.

       EXIT-SUMMARY-MODULE.
           MOVE ZEROS TO PAGE-1-TOTALS
                         PAGE-2-TOTALS
                         PAGE-3-TOTALS
                         PAGE-3-REL-TOTALS
                         PAGE-3-DAILY-TOTALS
                         PAGE-3-REL-DAILY-TOTALS
                         PAGE-4-TOTALS
      *                  PAGE-5-TOTALS
                         PAGE-6-TOTALS.
           IF PAGE1
              PERFORM SUM-FIELDS-1-DS
              PERFORM SUM-FIELDS-1-DY
              GO TO SUMMARY-MODULE-TEST-ALL.

           IF PAGE2
              PERFORM SUM-FIELDS-2-DS
                  VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              MOVE 1 TO SUB
              PERFORM SUM-FIELDS-2-DY
              GO TO SUMMARY-MODULE-TEST-ALL.

           IF PAGE3
              PERFORM SETUP-PAGE3-SUMMARY-TOT
              GO TO SUMMARY-MODULE-TEST-ALL.

           IF PAGE4
              MOVE ACTIVITY TO ACTSUB
              IF PAGE4-TOTAL
                 PERFORM SUM-FIELDS-4-DS
                     VARYING PERIOD FROM 1 BY 1 UNTIL PERIOD > 3
                         AFTER REPOSUB FROM 1 BY 1 UNTIL REPOSUB > 5
                 PERFORM SUM-FIELDS-4-DY
                     VARYING REPOSUB FROM 1 BY 1 UNTIL REPOSUB > 5
                 GO TO SUMMARY-MODULE-TEST-ALL
              ELSE
                 PERFORM SUM-FIELDS-4-DY
                     VARYING REPOSUB FROM 1 BY 1 UNTIL REPOSUB > 5
                 GO TO SUMMARY-MODULE-TEST-ALL.

      *    IF PAGE5
      * TOTAL GAIN/LOSS:
      *       MOVE PAGE5-GNLS TO PERIOD
      *       MOVE 1 TO ACTSUB
      *       IF PAGE5-TOTAL
      *          PERFORM SUM-FIELDS-5-DS
      *              VARYING TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 2
      *          PERFORM SUM-FIELDS-5-DY
      *              VARYING TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 2
      *       ELSE
      *          PERFORM SUM-FIELDS-5-DY
      *              VARYING TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 2
      *       END-IF
      * TOTAL NEW VOLUME:
      *       MOVE PAGE5-NVOL TO PERIOD
      *       MOVE 2 TO ACTSUB
      *       IF PAGE5-TOTAL
      *          PERFORM SUM-FIELDS-5-DS
      *              VARYING TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 2
      *          PERFORM SUM-FIELDS-5-DY
      *              VARYING TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 2
      *          GO TO SUMMARY-MODULE-TEST-ALL
      *       ELSE
      *          PERFORM SUM-FIELDS-5-DY
      *              VARYING TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 2
      *          GO TO SUMMARY-MODULE-TEST-ALL.

           IF PAGE6
              IF PAGE6-TOTAL
                 PERFORM SUM-FIELDS-6-DS
                 PERFORM SUM-FIELDS-6-DY
                 GO TO SUMMARY-MODULE-TEST-ALL
              ELSE
                 PERFORM SUM-FIELDS-6-DY
                 GO TO SUMMARY-MODULE-TEST-ALL.
      *-----------------------------------------------------------------
       SUMMARY-MODULE-TEST-ALL.
           MOVE "Y" TO FIRST-READ.
           MOVE "Y" TO READ-ONCE-FG.
           MOVE 1 TO WGR-SDEPTH
                     WGR-SCURR(1).

      *******************************************
       SETUP-PAGE3-SUMMARY-TOT SECTION.
      *******************************************
           IF DELINQ = 1 OR 2 OR 3
              MOVE DELINQ TO PERIOD
              PERFORM SUM-FIELDS-3-DS
                  VARYING AGESUB FROM 1 BY 1 UNTIL AGESUB > 10
                    AFTER RECCON FROM 1 BY 1 UNTIL RECCON > 2
              IF DELINQ = 1
                 PERFORM SUM-FIELDS-3-DY
                     VARYING AGESUB FROM 1 BY 1 UNTIL AGESUB > 10
                       AFTER RECCON FROM 1 BY 1 UNTIL RECCON > 2
                 GO TO SETUP-PAGE3-SUMMARY-TOT-EXIT
              ELSE
                 GO TO SETUP-PAGE3-SUMMARY-TOT-EXIT.

           IF DELINQ = 4
              PERFORM SUM-FIELDS-3-DY
                  VARYING AGESUB FROM 1 BY 1 UNTIL AGESUB > 10
                    AFTER RECCON FROM 1 BY 1 UNTIL RECCON > 2
              GO TO SETUP-PAGE3-SUMMARY-TOT-EXIT.

           IF DELINQ = 5
              MOVE 1 TO PERIOD
              PERFORM SUM-FIELDS-3-DS
                  VARYING AGESUB FROM 1 BY 1 UNTIL AGESUB > 10
                    AFTER RECCON FROM 1 BY 1 UNTIL RECCON > 2
              PERFORM SUM-FIELDS-3-DY
                  VARYING AGESUB FROM 1 BY 1 UNTIL AGESUB > 10
                    AFTER RECCON FROM 1 BY 1 UNTIL RECCON > 2
              MOVE 2 TO PERIOD
              PERFORM SUM-FIELDS-3-DS-REL
                  VARYING AGESUB FROM 1 BY 1 UNTIL AGESUB > 10
                    AFTER RECCON FROM 1 BY 1 UNTIL RECCON > 2
              GO TO SETUP-PAGE3-SUMMARY-TOT-EXIT.

       TEST-DELINQ-7.
           IF DELINQ NOT = 7
              GO TO SKIP-TEST-DELINQ-7.

           ADD HDS-OUTSUNIT(1) TO SUM-DAILY-NO.
           ADD HDS-OUTSAMT (1) TO SUM-DAILY-AMT.
           ADD HDS-NPURUNIT(1)
               HDS-OPURUNIT(1) TO SUM-DAILY-VOLNO.
           ADD HDS-NPURAMT (1)
               HDS-OPURAMT (1) TO SUM-DAILY-VOL.

           ADD HDS-SUMWGTTM TO SUM-DAILY-SUMWGTTM.
           ADD HDS-SUMWGTTA TO SUM-DAILY-SUMWGTTA.
           ADD HDS-SUMWGTAV TO SUM-DAILY-SUMWGTAV.

           ADD HDS-NPURUNIT(1)
               HDS-OPURUNIT(1) TO SUM-DAILY-NB.

           ADD HDS-NPURAMT(1)
               HDS-OPURAMT(1) TO SUM-DAILY-NB-AMT.

           ADD HDS-OUTSUNIT(2) TO SUM-REL-DAILY-NO.
           ADD HDS-OUTSAMT (2) TO SUM-REL-DAILY-AMT.

           MOVE 1 TO PERIOD
           PERFORM SUM-FIELDS-3-DAILY-DS
               VARYING AGESUB FROM 1 BY 1 UNTIL AGESUB > 9
                 AFTER RECCON FROM 1 BY 1 UNTIL RECCON > 2.

           ADD HDY-OUTSUNIT TO SUM-DAILY-NO.
           ADD HDY-OUTSAMT  TO SUM-DAILY-AMT.
           ADD HDY-NPURUNIT
               HDY-OPURUNIT TO SUM-DAILY-VOLNO.
           ADD HDY-NPURAMT
               HDY-OPURAMT  TO SUM-DAILY-VOL.

           ADD HDY-SUMWGTTM TO SUM-DAILY-SUMWGTTM.
           ADD HDY-SUMWGTAV TO SUM-DAILY-SUMWGTAV.
           ADD HDY-SUMWGTTA TO SUM-DAILY-SUMWGTTA.

           ADD HDY-NPURUNIT
               HDY-OPURUNIT TO SUM-DAILY-NB.

           ADD HDY-NPURAMT
               HDY-OPURAMT TO SUM-DAILY-NB-AMT.

           PERFORM SUM-FIELDS-3-DAILY-DY
               VARYING AGESUB FROM 1 BY 1 UNTIL AGESUB > 9
                 AFTER RECCON FROM 1 BY 1 UNTIL RECCON > 2.

           MOVE 2 TO SUB4.
           PERFORM SUM-FIELDS-3-DAILY-DS-REL
               VARYING AGESUB FROM 1 BY 1 UNTIL AGESUB > 9
                 AFTER RECCON FROM 1 BY 1 UNTIL RECCON > 2.

       SKIP-TEST-DELINQ-7.
           GO TO SETUP-PAGE3-SUMMARY-TOT-EXIT.

       SETUP-PAGE3-SUMMARY-TOT-EXIT.
           EXIT.

      *******************************************
       SUM-FIELDS-1-DS SECTION.
      *******************************************
           ADD HDS-DLRVPRIN(1)       TO SUM-DLRVPRIN(1).
           ADD HDS-DLRVPRIN(2)       TO SUM-DLRVPRIN(2).
           ADD HDS-DLRVPRIN(3)       TO SUM-DLRVPRIN(3).
           ADD HDS-DLRVCHGS(1)       TO SUM-DLRVCHGS(1).
           ADD HDS-DLRVCHGS(2)       TO SUM-DLRVCHGS(2).
           ADD HDS-DLRVCHGS(3)       TO SUM-DLRVCHGS(3).
           ADD HDS-DLPARVHELD        TO SUM-DLPARVHELD.
           ADD HDS-UNPAIDCK          TO SUM-UNPAIDCK.
           ADD HDS-DLDISC            TO SUM-DLDISC.
           ADD HDS-DLPARVPAID        TO SUM-DLPARVPAID.
           ADD HDS-SUMWGTTM          TO SUM-SUMWGTTM.
           ADD HDS-SUMWGTTA          TO SUM-SUMWGTTA.
           ADD HDS-SUMWGTAV          TO SUM-SUMWGTAV.
           ADD HDS-DLDISC-VOL(1)     TO SUM-DLDISC-VOL(1).
           ADD HDS-DLDISC-VOL(2)     TO SUM-DLDISC-VOL(2).
           ADD HDS-DLDISC-VOL(3)     TO SUM-DLDISC-VOL(3).
           ADD HDS-PROCEEDS(1)       TO SUM-PROCEEDS(1).
           ADD HDS-PROCEEDS(2)       TO SUM-PROCEEDS(2).

           ADD HDS-RCOURSE-UNITS(1)  TO SUM-RCOURSE-UNITS(1).
           ADD HDS-RCOURSE-AMT(1)    TO SUM-RCOURSE-AMT(1).
           ADD HDS-RCOURSE-UNITS(2)  TO SUM-RCOURSE-UNITS(2).
           ADD HDS-RCOURSE-AMT(2)    TO SUM-RCOURSE-AMT(2).
           ADD HDS-RCOURSE-UNITS(3)  TO SUM-RCOURSE-UNITS(3).
           ADD HDS-RCOURSE-AMT(3)    TO SUM-RCOURSE-AMT(3).
       SUM-FIELDS-1-DS-EXIT.
           EXIT.

      *******************************************
       SUM-FIELDS-1-DY SECTION.
      *******************************************
           ADD HDY-DLRVPRIN(1)       TO SUM-DLRVPRIN(1).
           ADD HDY-DLRVPRIN(2)       TO SUM-DLRVPRIN(2).
           ADD HDY-DLRVPRIN(3)       TO SUM-DLRVPRIN(3).
           ADD HDY-DLRVCHGS(1)       TO SUM-DLRVCHGS(1).
           ADD HDY-DLRVCHGS(2)       TO SUM-DLRVCHGS(2).
           ADD HDY-DLRVCHGS(3)       TO SUM-DLRVCHGS(3).
           ADD HDY-DLPARVHELD        TO SUM-DLPARVHELD.
           ADD HDY-UNPAIDCK          TO SUM-UNPAIDCK.
           ADD HDY-DLDISC            TO SUM-DLDISC.
           ADD HDY-DLPARVPAID        TO SUM-DLPARVPAID.
           ADD HDY-SUMWGTTM          TO SUM-SUMWGTTM.
           ADD HDY-SUMWGTTA          TO SUM-SUMWGTTA.
           ADD HDY-SUMWGTAV          TO SUM-SUMWGTAV.
           ADD HDY-DLDISC-VOL        TO SUM-DLDISC-VOL(1).
           ADD HDY-PROCEEDS          TO SUM-PROCEEDS(1).
           ADD HDY-RCOURSE-UNITS(1)  TO SUM-RCOURSE-UNITS(1).
           ADD HDY-RCOURSE-AMT(1)    TO SUM-RCOURSE-AMT(1).
           ADD HDY-RCOURSE-UNITS(2)  TO SUM-RCOURSE-UNITS(2).
           ADD HDY-RCOURSE-AMT(2)    TO SUM-RCOURSE-AMT(2).
           ADD HDY-RCOURSE-UNITS(3)  TO SUM-RCOURSE-UNITS(3).
           ADD HDY-RCOURSE-AMT(3)    TO SUM-RCOURSE-AMT(3).

       SUM-FIELDS-1-DY-EXIT.
           EXIT.

      *******************************************
       SUM-FIELDS-2-DS SECTION.
      *******************************************
           ADD HDS-NPURUNIT(SUB) TO SUM2-NPURUNIT(SUB).
           ADD HDS-NPURAMT (SUB) TO SUM2-NPURAMT (SUB).
           ADD HDS-OPURUNIT(SUB) TO SUM2-OPURUNIT(SUB).
           ADD HDS-OPURAMT (SUB) TO SUM2-OPURAMT (SUB).
           ADD HDS-RNEWUNIT(SUB) TO SUM2-RNEWUNIT(SUB).
           ADD HDS-RNEWAMT (SUB) TO SUM2-RNEWAMT (SUB).
           ADD HDS-PAIDUNIT(SUB) TO SUM2-PAIDUNIT(SUB).
           ADD HDS-PAIDAMT (SUB) TO SUM2-PAIDAMT (SUB).
           ADD HDS-TODLUNIT(SUB) TO SUM2-TOLNUNIT(SUB).
           ADD HDS-TODLAMT (SUB) TO SUM2-TOLNAMT (SUB).
           ADD HDS-OUTSUNIT(SUB) TO SUM2-OUTSUNIT(SUB).
           ADD HDS-OUTSAMT (SUB) TO SUM2-OUTSAMT (SUB).
           ADD HDS-PLOSUNIT(SUB) TO SUM2-PLOSUNIT(SUB).
           ADD HDS-PLOSAMT (SUB) TO SUM2-PLOSAMT (SUB).
           ADD HDS-RCPLUNIT(SUB) TO SUM2-RCPLUNIT(SUB).
           ADD HDS-RCPLAMT (SUB) TO SUM2-RCPLAMT (SUB).
           ADD HDS-JDOSUNIT(SUB) TO SUM2-JDOSUNIT(SUB).
           ADD HDS-JDOSAMT (SUB) TO SUM2-JDOSAMT (SUB).
       EXIT-SUM-FIELDS-2-DS.
           EXIT.

      *******************************************
       SUM-FIELDS-2-DY SECTION.
      *******************************************
           ADD HDY-NPURUNIT TO SUM2-NPURUNIT(SUB).
           ADD HDY-NPURAMT  TO SUM2-NPURAMT (SUB).
           ADD HDY-OPURUNIT TO SUM2-OPURUNIT(SUB).
           ADD HDY-OPURAMT  TO SUM2-OPURAMT (SUB).
           ADD HDY-RNEWUNIT TO SUM2-RNEWUNIT(SUB).
           ADD HDY-RNEWAMT  TO SUM2-RNEWAMT (SUB).
           ADD HDY-PAIDUNIT TO SUM2-PAIDUNIT(SUB).
           ADD HDY-PAIDAMT  TO SUM2-PAIDAMT (SUB).
           ADD HDY-TOLNUNIT TO SUM2-TOLNUNIT(SUB).
           ADD HDY-TOLNAMT  TO SUM2-TOLNAMT (SUB).
           ADD HDY-OUTSUNIT TO SUM2-OUTSUNIT(SUB).
           ADD HDY-OUTSAMT  TO SUM2-OUTSAMT (SUB).
           ADD HDY-PLOSUNIT TO SUM2-PLOSUNIT(SUB).
           ADD HDY-PLOSAMT  TO SUM2-PLOSAMT (SUB).
           ADD HDY-RCPLUNIT TO SUM2-RCPLUNIT(SUB).
           ADD HDY-RCPLAMT  TO SUM2-RCPLAMT (SUB).
           ADD HDY-JDOSUNIT TO SUM2-JDOSUNIT(SUB).
           ADD HDY-JDOSAMT  TO SUM2-JDOSAMT (SUB).

       EXIT-SUM-FIELDS-2-DY.
           EXIT.

      *******************************************
       SUM-FIELDS-3-DS SECTION.
      *******************************************

      * INCLUDE BANKRUPT DEL:
           IF BNKRPT-DEL = 1
              ADD HDS-UNITS(PERIOD RECCON AGESUB)
                                  TO SUM-DEL-UNITS(AGESUB RECCON)
              ADD HDS-OUTBAL(PERIOD RECCON AGESUB)
                                  TO SUM-DEL-OUTBAL(AGESUB RECCON)
           ELSE
      * SKIP BANKRUPT DEL:
           IF BNKRPT-DEL = 2
              ADD HDS-UNITS(PERIOD RECCON AGESUB)
                                  TO SUM-DEL-UNITS(AGESUB RECCON)
              ADD HDS-OUTBAL(PERIOD RECCON AGESUB)
                                  TO SUM-DEL-OUTBAL(AGESUB RECCON)
              SUBTRACT HDS-UNITS-BNKRPT(PERIOD RECCON AGESUB)
                                  FROM SUM-DEL-UNITS(AGESUB RECCON)
              SUBTRACT HDS-OUTBAL-BNKRPT(PERIOD RECCON AGESUB)
                                  FROM SUM-DEL-OUTBAL(AGESUB RECCON)
           ELSE
      * ONLY BANKRUPT DEL:
           IF BNKRPT-DEL = 3
              ADD HDS-UNITS-BNKRPT(PERIOD RECCON AGESUB)
                                  TO SUM-DEL-UNITS(AGESUB RECCON)
              ADD HDS-OUTBAL-BNKRPT(PERIOD RECCON AGESUB)
                                  TO SUM-DEL-OUTBAL(AGESUB RECCON).

      *******************************************
       SUM-FIELDS-3-DY SECTION.
      *******************************************

      * INCLUDE BANKRUPT DEL:
           IF BNKRPT-DEL = 1
              ADD HDY-UNITS(RECCON AGESUB)
                                  TO SUM-DEL-UNITS(AGESUB RECCON)
              ADD HDY-OUTBAL(RECCON AGESUB)
                                  TO SUM-DEL-OUTBAL(AGESUB RECCON)
           ELSE
      * SKIP BANKRUPT DEL:
           IF BNKRPT-DEL = 2
              ADD HDY-UNITS(RECCON AGESUB)
                                  TO SUM-DEL-UNITS(AGESUB RECCON)
              ADD HDY-OUTBAL(RECCON AGESUB)
                                  TO SUM-DEL-OUTBAL(AGESUB RECCON)
              SUBTRACT HDY-UNITS-BNKRPT(RECCON AGESUB)
                                  FROM SUM-DEL-UNITS(AGESUB RECCON)
              SUBTRACT HDY-OUTBAL-BNKRPT(RECCON AGESUB)
                                  FROM SUM-DEL-OUTBAL(AGESUB RECCON)
           ELSE
      * ONLY BANKRUPT DEL:
           IF BNKRPT-DEL = 3
              ADD HDY-UNITS-BNKRPT(RECCON AGESUB)
                                  TO SUM-DEL-UNITS(AGESUB RECCON)
              ADD HDY-OUTBAL-BNKRPT(RECCON AGESUB)
                                  TO SUM-DEL-OUTBAL(AGESUB RECCON).

      *******************************************
       SUM-FIELDS-3-DS-REL SECTION.
      *******************************************

      * INCLUDE BANKRUPT DEL:
           IF BNKRPT-DEL = 1
              ADD HDS-UNITS(PERIOD RECCON AGESUB)
                                  TO SUM-REL-DEL-UNITS(AGESUB RECCON)
              ADD HDS-OUTBAL(PERIOD RECCON AGESUB)
                                  TO SUM-REL-DEL-OUTBAL(AGESUB RECCON)
           ELSE
      * SKIP BANKRUPT DEL:
           IF BNKRPT-DEL = 2
              ADD HDS-UNITS(PERIOD RECCON AGESUB)
                                  TO SUM-REL-DEL-UNITS(AGESUB RECCON)
              ADD HDS-OUTBAL(PERIOD RECCON AGESUB)
                                  TO SUM-REL-DEL-OUTBAL(AGESUB RECCON)
              SUBTRACT HDS-UNITS-BNKRPT(PERIOD RECCON AGESUB)
                                  FROM SUM-REL-DEL-UNITS(AGESUB RECCON)
              SUBTRACT HDS-OUTBAL-BNKRPT(PERIOD RECCON AGESUB)
                                  FROM SUM-REL-DEL-OUTBAL(AGESUB RECCON)
           ELSE
      * ONLY BANKRUPT DEL:
           IF BNKRPT-DEL = 3
              ADD HDS-UNITS-BNKRPT(PERIOD RECCON AGESUB)
                                  TO SUM-REL-DEL-UNITS(AGESUB RECCON)
              ADD HDS-OUTBAL-BNKRPT(PERIOD RECCON AGESUB)
                                  TO SUM-REL-DEL-OUTBAL(AGESUB RECCON).

      *******************************************
       SUM-FIELDS-3-DAILY-DS SECTION.
      *******************************************
           IF AGESUB > 2 AND < 8
              MOVE 3 TO SUBX
           ELSE
              IF AGESUB = 8
                 MOVE 4 TO SUBX
              ELSE
                 IF AGESUB = 9
                    MOVE 5 TO SUBX
                 ELSE
                    MOVE AGESUB TO SUBX.

      * INCLUDE BANKRUPT DEL:
           IF BNKRPT-DEL = 1
              ADD HDS-UNITS(PERIOD RECCON AGESUB)
                               TO SUM-DAILY-UNITS(SUBX RECCON)
              ADD HDS-OUTBAL(PERIOD RECCON AGESUB)
                               TO SUM-DAILY-OUTBAL(SUBX RECCON)
           ELSE
      * SKIP BANKRUPT DEL:
           IF BNKRPT-DEL = 2
              ADD HDS-UNITS(PERIOD RECCON AGESUB)
                               TO SUM-DAILY-UNITS(SUBX RECCON)
              ADD HDS-OUTBAL(PERIOD RECCON AGESUB)
                               TO SUM-DAILY-OUTBAL(SUBX RECCON)
              SUBTRACT HDS-UNITS-BNKRPT(PERIOD RECCON AGESUB)
                               FROM SUM-DAILY-UNITS(SUBX RECCON)
              SUBTRACT HDS-OUTBAL-BNKRPT(PERIOD RECCON AGESUB)
                               FROM SUM-DAILY-OUTBAL(SUBX RECCON)
           ELSE
      * ONLY BANKRUPT DEL:
           IF BNKRPT-DEL = 3
              ADD HDS-UNITS-BNKRPT(PERIOD RECCON AGESUB)
                               TO SUM-DAILY-UNITS(SUBX RECCON)
              ADD HDS-OUTBAL-BNKRPT(PERIOD RECCON AGESUB)
                               TO SUM-DAILY-OUTBAL(SUBX RECCON).

      * SET CONTRACTUAL MISPAYS INTO RECENCY IF WORLD:
           IF RECCON = 2
              IF AGESUB = 8
      * INCLUDE BANKRUPT DEL:
                 IF BNKRPT-DEL = 1
                    ADD HDS-UNITS(PERIOD RECCON AGESUB)
                                   TO SUM-DAILY-UNITS(SUBX 1)
                    ADD HDS-OUTBAL(PERIOD RECCON AGESUB)
                                   TO SUM-DAILY-OUTBAL(SUBX 1)
                 ELSE
      * SKIP BANKRUPT DEL:
                    IF BNKRPT-DEL = 2
                       ADD HDS-UNITS(PERIOD RECCON AGESUB)
                                      TO SUM-DAILY-UNITS(SUBX 1)
                       ADD HDS-OUTBAL(PERIOD RECCON AGESUB)
                                      TO SUM-DAILY-OUTBAL(SUBX 1)
                       SUBTRACT HDS-UNITS-BNKRPT(PERIOD RECCON AGESUB)
                                      FROM SUM-DAILY-UNITS(SUBX 1)
                       SUBTRACT HDS-OUTBAL-BNKRPT(PERIOD RECCON AGESUB)
                                      FROM SUM-DAILY-OUTBAL(SUBX 1)
                    ELSE
      * ONLY BANKRUPT DEL:
                    IF BNKRPT-DEL = 3
                       ADD HDS-UNITS-BNKRPT(PERIOD RECCON AGESUB)
                                      TO SUM-DAILY-UNITS(SUBX 1)
                       ADD HDS-OUTBAL-BNKRPT(PERIOD RECCON AGESUB)
                                      TO SUM-DAILY-OUTBAL(SUBX 1).

      *******************************************
       SUM-FIELDS-3-DAILY-DY SECTION.
      *******************************************
           IF AGESUB > 2 AND < 8
              MOVE 3 TO SUBX
           ELSE
              IF AGESUB = 8
                 MOVE 4 TO SUBX
              ELSE
                 IF AGESUB = 9
                    MOVE 5 TO SUBX
                 ELSE
                    MOVE AGESUB TO SUBX.

      * INCLUDE BANKRUPT DEL:
           IF BNKRPT-DEL = 1
              ADD HDY-UNITS(RECCON AGESUB)
                               TO SUM-DAILY-UNITS(SUBX RECCON)
              ADD HDY-OUTBAL(RECCON AGESUB)
                               TO SUM-DAILY-OUTBAL(SUBX RECCON)
           ELSE
      * SKIP BANKRUPT DEL:
           IF BNKRPT-DEL = 2
              ADD HDY-UNITS(RECCON AGESUB)
                               TO SUM-DAILY-UNITS(SUBX RECCON)
              ADD HDY-OUTBAL(RECCON AGESUB)
                               TO SUM-DAILY-OUTBAL(SUBX RECCON)
              SUBTRACT HDY-UNITS-BNKRPT(RECCON AGESUB)
                               FROM SUM-DAILY-UNITS(SUBX RECCON)
              SUBTRACT HDY-OUTBAL-BNKRPT(RECCON AGESUB)
                               FROM SUM-DAILY-OUTBAL(SUBX RECCON)
           ELSE
      * ONLY BANKRUPT DEL:
           IF BNKRPT-DEL = 3
              ADD HDY-UNITS-BNKRPT(RECCON AGESUB)
                               TO SUM-DAILY-UNITS(SUBX RECCON)
              ADD HDY-OUTBAL-BNKRPT(RECCON AGESUB)
                               TO SUM-DAILY-OUTBAL(SUBX RECCON).

      * SET CONTRACTUAL MISPAYS INTO RECENCY IF WORLD:
           IF RECCON = 2
              IF AGESUB = 8
      * INCLUDE BANKRUPT DEL:
                 IF BNKRPT-DEL = 1
                    ADD HDY-UNITS(RECCON AGESUB)
                                  TO SUM-DAILY-UNITS(SUBX 1)
                    ADD HDY-OUTBAL(RECCON AGESUB)
                                  TO SUM-DAILY-OUTBAL(SUBX 1)
                 ELSE
      * SKIP BANKRUPT DEL:
                 IF BNKRPT-DEL = 2
                    ADD HDY-UNITS(RECCON AGESUB)
                                  TO SUM-DAILY-UNITS(SUBX 1)
                    ADD HDY-OUTBAL(RECCON AGESUB)
                                  TO SUM-DAILY-OUTBAL(SUBX 1)
                    SUBTRACT HDY-UNITS-BNKRPT(RECCON AGESUB)
                                  FROM SUM-DAILY-UNITS(SUBX 1)
                    SUBTRACT HDY-OUTBAL-BNKRPT(RECCON AGESUB)
                                  FROM SUM-DAILY-OUTBAL(SUBX 1)
                 ELSE
      * ONLY BANKRUPT DEL:
                 IF BNKRPT-DEL = 3
                    ADD HDY-UNITS-BNKRPT(RECCON AGESUB)
                                  TO SUM-DAILY-UNITS(SUBX 1)
                    ADD HDY-OUTBAL-BNKRPT(RECCON AGESUB)
                                  TO SUM-DAILY-OUTBAL(SUBX 1).

      ************************************************
       SUM-FIELDS-3-DAILY-DS-REL SECTION.
      ************************************************
           IF AGESUB > 2 AND < 8
              MOVE 3 TO SUBX
           ELSE
              IF AGESUB = 8
                 MOVE 4 TO SUBX
              ELSE
                 IF AGESUB = 9
                    MOVE 5 TO SUBX
                 ELSE
                    MOVE AGESUB TO SUBX.

      * INCLUDE BANKRUPT DEL:
           IF BNKRPT-DEL = 1
              ADD HDS-UNITS(SUB4 RECCON AGESUB)
                             TO SUM-REL-DAILY-UNITS(SUBX RECCON)
              ADD HDS-OUTBAL(SUB4 RECCON AGESUB)
                             TO SUM-REL-DAILY-OUTBAL(SUBX RECCON)
           ELSE
      * SKIP BANKRUPT DEL:
           IF BNKRPT-DEL = 2
              ADD HDS-UNITS(SUB4 RECCON AGESUB)
                             TO SUM-REL-DAILY-UNITS(SUBX RECCON)
              ADD HDS-OUTBAL(SUB4 RECCON AGESUB)
                             TO SUM-REL-DAILY-OUTBAL(SUBX RECCON)
              SUBTRACT HDS-UNITS-BNKRPT(SUB4 RECCON AGESUB)
                             FROM SUM-REL-DAILY-UNITS(SUBX RECCON)
              SUBTRACT HDS-OUTBAL-BNKRPT(SUB4 RECCON AGESUB)
                             FROM SUM-REL-DAILY-OUTBAL(SUBX RECCON)
           ELSE
      * ONLY BANKRUPT DEL:
           IF BNKRPT-DEL = 3
              ADD HDS-UNITS-BNKRPT(SUB4 RECCON AGESUB)
                             TO SUM-REL-DAILY-UNITS(SUBX RECCON)
              ADD HDS-OUTBAL-BNKRPT(SUB4 RECCON AGESUB)
                             TO SUM-REL-DAILY-OUTBAL(SUBX RECCON).

      * SET CONTRACTUAL MISPAYS INTO RECENCY IF WORLD:
           IF RECCON = 2
              IF AGESUB = 8
      * INCLUDE BANKRUPT DEL:
                 IF BNKRPT-DEL = 1
                    ADD HDS-UNITS(SUB4 RECCON AGESUB)
                              TO SUM-REL-DAILY-UNITS(SUBX 1)
                    ADD HDS-OUTBAL(SUB4 RECCON AGESUB)
                             TO SUM-REL-DAILY-OUTBAL(SUBX 1)
                 ELSE
      * SKIP BANKRUPT DEL:
                 IF BNKRPT-DEL = 2
                    ADD HDS-UNITS(SUB4 RECCON AGESUB)
                              TO SUM-REL-DAILY-UNITS(SUBX 1)
                    ADD HDS-OUTBAL(SUB4 RECCON AGESUB)
                             TO SUM-REL-DAILY-OUTBAL(SUBX 1)
                    SUBTRACT HDS-UNITS-BNKRPT(SUB4 RECCON AGESUB)
                              FROM SUM-REL-DAILY-UNITS(SUBX 1)
                    SUBTRACT HDS-OUTBAL-BNKRPT(SUB4 RECCON AGESUB)
                             FROM SUM-REL-DAILY-OUTBAL(SUBX 1)
                 ELSE
      * ONLY BANKRUPT DEL:
                 IF BNKRPT-DEL = 3
                    ADD HDS-UNITS-BNKRPT(SUB4 RECCON AGESUB)
                              TO SUM-REL-DAILY-UNITS(SUBX 1)
                    ADD HDS-OUTBAL-BNKRPT(SUB4 RECCON AGESUB)
                             TO SUM-REL-DAILY-OUTBAL(SUBX 1).

      *****************************************************************
      *    SUM FIELDS BRANCH ANALYSIS PAGE 4
      *****************************************************************
      *  PERIOD:
      *    PERIOD TABLE: 1 - CURRENT           1 - MTD
      *                  2 - LAST MONTH        2 - YTD
      *                  3 - LAST YEAR         3 - LYR
      *  ACTSUB:
      *    ACTIVITY TBL: 1 - NET GAIN / LOSS   2 - NEW VOLUME
      *
      *  REPOSUB:
      *    TYPE TABLE  : 1 - REPO ON HAND
      *                  2 - REPO REDEEMED
      *                  3 - REPO SOLD LIVE RECEIVABLE
      *                  4 - SOLD P&L ACTIVE
      *                  5 - SOLD P&L INACTIVE
      *****************************************************************
       SUM-FIELDS-4-DS SECTION.
      *******************************************
           IF (ACTSUB = 1 AND PERIOD = 1) OR ACTSUB = 2
                PERFORM SUM-FIELDS-4-DS-FIXED.
           IF ACTSUB = 1
              IF PERIOD = 2 OR 3
                    PERFORM SUM-FIELDS-4-DS-DIFF.
       EXIT-SUM-FIELDS-4-DS.
           EXIT.

      *----------------------------------------------------------------
      * SUM FIELDS 4 BY FIXED SECTION:
      *-------------------------------
      * NO OF ACCOUNTS DISPLAYED ONLY WITH PAGE4-REC AND PAGE4-NO-O2.
      *
      *  IF PAGE4-REC   - INCREMENT RECEIVABLES WITH O2 ADDED IN.
      *  IF PAGE4-NO-O2 - INCREMENT RECEIVABLES WITH O2 SUBTRACTED OUT.
      *  IF PAGE4-O2    - INCREMENT O2 AMOUNTS ONLY (NO RECEIVABLES).
      *  IF PAGE4-INT   - INCREMENT INTEREST CHARGES ONLY.
      *****************************************************************
      *******************************************
       SUM-FIELDS-4-DS-FIXED SECTION.
      *******************************************
           IF PAGE4-REC
              ADD HDS-NO-REPO(PERIOD ACTSUB REPOSUB) TO
                  SUM-NO-REPO(REPOSUB PERIOD)
              ADD HDS-AMT-REPO(PERIOD ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB PERIOD)
              GO TO EXIT-SUM-FIELDS-4-DS-FIXED.
       EXIT-SUM-FIELDS-4-DS-FIXED.
           EXIT.

      *----------------------------------------------------------------
      * SUM FIELDS 4 BY DIFF SECTION:
      *------------------------------
      * DISPLAYS DIFFERENCE BETWEEN CURRENT AND ELM, ELY.
      *    NO OF ACCOUNTS DISPLAYED ONLY WITH PAGE4-REC AND PAGE4-NO-O2
      *
      *  IF PAGE4-REC   - INCREMENT RECEIVABLES WITH O2 ADDED IN.
      *                      (CUR) - (ELM)   [O2 INCLUDED]
      *  IF PAGE4-NO-O2 - INCREMENT RECEIVABLES WITH O2 SUBTRACTED OUT.
      *                      (CUR - 02) - (ELM - O2)
      *  IF PAGE4-O2    - INCREMENT O2 AMOUNTS ONLY (NO RECEIVABLES).
      *                      (CUR 02) - (ELM O2)
      *  IF PAGE4-INT   - INCREMENT INTEREST CHARGES ONLY.
      *                      (CUR INT) - (ELM INT)
      *----------------------------------------------------------------
       SUM-FIELDS-4-DS-DIFF SECTION.
           IF PAGE4-REC
              ADD HDS-NO-REPO(1 ACTSUB REPOSUB) TO
                  SUM-NO-REPO(REPOSUB PERIOD)
              SUBTRACT HDS-NO-REPO(PERIOD ACTSUB REPOSUB) FROM
                       SUM-NO-REPO(REPOSUB PERIOD)
              ADD HDS-AMT-REPO(1 ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB PERIOD)
              SUBTRACT HDS-AMT-REPO(PERIOD ACTSUB REPOSUB) FROM
                       SUM-AMT-REPO(REPOSUB PERIOD)
              GO TO EXIT-SUM-FIELDS-4-DS-DIFF.
       EXIT-SUM-FIELDS-4-DS-DIFF.
              EXIT.

      *****************************************************************
      *    SUM FIELDS TODAYS TOTALS PAGE 4
      *****************************************************************
      *
      *  ACTSUB:
      *    ACTIVITY TBL: 1 - NET FIXED / LOSS   2 - NEW VOLUME
      *
      *  REPOSUB:
      *    TYPE TABLE  : 1 - REPO ON HAND
      *                  2 - REPO REDEEMED
      *                  3 - REPO SOLD LIVE RECEIVABLE
      *                  4 - SOLD P&L ACTIVE
      *                  5 - SOLD P&L INACTIVE
      *****************************************************************
       SUM-FIELDS-4-DY SECTION.
      *******************************************
           IF ACTSUB = 1 OR ACTSUB = 2
              PERFORM SUM-FIELDS-4-DY-FIXED.

       EXIT-SUM-FIELDS-4-DY.
           EXIT.

      *----------------------------------------------------------------
      * SUM FIELDS 4 DY FIXED SECTION:
      *-------------------------------
      * DISPLAYS DIFFERENCE BETWEEN CURRENT AND ELM, ELY.
      *    NO OF ACCOUNTS DISPLAYED ONLY WITH PAGE2-REC AND PAGE2-NO-O2.
      *  IF PAGE4-TODAY - INCREMENT ONLY CURRENT TOTALS (SKIP MTD AND YTD).
      *  IF PAGE4-TOTAL - INCREMENT CUR, MTD, YTD.
      *
      *  IF PAGE4-REC   - INCREMENT RECEIVABLES WITH O2 ADDED IN.
      *  IF PAGE4-NO-O2 - INCREMENT RECEIVABLES WITH O2 SUBTRACTED OUT.
      *  IF PAGE4-O2    - INCREMENT O2 AMOUNTS ONLY (NO RECEIVABLES).
      *  IF PAGE4-INT   - INCREMENT INTEREST CHARGES ONLY.
      *----------------------------------------------------------------
       SUM-FIELDS-4-DY-FIXED SECTION.
           IF PAGE4-TODAY
              GO TO PAGE-4-DAILY.

           IF PAGE4-REC
              ADD HDY-NO-REPO(ACTSUB REPOSUB) TO
                  SUM-NO-REPO(REPOSUB 1)
              ADD HDY-AMT-REPO(ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB 1)
              ADD HDY-NO-REPO(ACTSUB REPOSUB) TO
                  SUM-NO-REPO(REPOSUB 2)
              ADD HDY-AMT-REPO(ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB 2)
              IF ACTSUB = 1
                 ADD HDY-NO-REPO(ACTSUB REPOSUB) TO
                     SUM-NO-REPO(REPOSUB 3)
                 ADD HDY-AMT-REPO(ACTSUB REPOSUB) TO
                     SUM-AMT-REPO(REPOSUB 3)
              ELSE
                 GO TO EXIT-SUM-FIELDS-4-DY-FIXED.

           GO TO EXIT-SUM-FIELDS-4-DY-FIXED.
       PAGE-4-DAILY.
              IF PAGE4-REC
                 ADD HDY-NO-REPO(ACTSUB REPOSUB) TO
                     SUM-NO-REPO(REPOSUB 1)
                 ADD HDY-AMT-REPO(ACTSUB REPOSUB) TO
                     SUM-AMT-REPO(REPOSUB 1)
                 GO TO EXIT-SUM-FIELDS-4-DY-FIXED.
       EXIT-SUM-FIELDS-4-DY-FIXED.
           EXIT.

      ******************************************************************
      *
      *    S U M - F I E L D S - 5 - B Y
      *
      *-----------------------------------------------------------------
      *  SUM FIELDS BRANCH ANALYSIS PAGE 5
      *
      *  PERIOD:PERIOD TABLE: 1 - CURRENT           1 - MTD
      *                       2 - LAST MONTH        2 - YTD
      *                       3 - LAST YEAR         3 - LYR
      *
      *  ACTSUB: ACTIVITY  :  1 - NET GAIN / LOSS   2 - NEW VOLUME
      *
      *  TYPSUB: TYPE TABLE:  1 - NB   4 - PB    7 - RN   10 - N/A
      *                       2 - RF   5 - PBD   8 - SC   11 - N/A
      *                       3 - FB   6 - PBI   9 - OT
      *
      ******************************************************************
      *SUM-FIELDS-5-DS SECTION.
      *    IF (ACTSUB = 1 AND PERIOD = 1) OR ACTSUB = 2
      *       MOVE TYPSUB TO SUB4
      *       PERFORM SUM-FIELDS-5-DS-FIXED
      *       MOVE 3 TO SUB4
      *       PERFORM SUM-FIELDS-5-DS-FIXED.
      *    IF ACTSUB = 1 AND (PERIOD = 2 OR 3)
      *       MOVE TYPSUB TO SUB4
      *       PERFORM SUM-FIELDS-5-DS-DIFF
      *       MOVE 3 TO SUB4
      *       PERFORM SUM-FIELDS-5-DS-DIFF.

      *EXIT-SUM-FIELDS-5-DS.
      *    EXIT.

      ******************************************************************
      *
      *    S U M - F I E L D S - 5 - D S - F I X E D
      *
      ******************************************************************
      *SUM-FIELDS-5-DS-FIXED SECTION.
      *    ADD HDS-NO-PENDING (PERIOD ACTSUB TYPSUB) TO
      *        SUM-NO-PENDING (SUB4 ACTSUB).
      *    ADD HDS-NO-COND-APP(PERIOD ACTSUB TYPSUB) TO
      *        SUM-NO-COND-APP(SUB4 ACTSUB).
      *    ADD HDS-NO-APPROVED(PERIOD ACTSUB TYPSUB) TO
      *        SUM-NO-APPROVED(SUB4 ACTSUB).
      *    ADD HDS-NO-MADE(PERIOD ACTSUB TYPSUB) TO
      *        SUM-NO-MADE(SUB4 ACTSUB).
      *    ADD HDS-NO-TURNDOWN(PERIOD ACTSUB TYPSUB) TO
      *        SUM-NO-TURNDOWN(SUB4 ACTSUB).
      *    ADD HDS-NO-TURNUSDOWN(PERIOD ACTSUB TYPSUB) TO
      *        SUM-NO-TUD(SUB4 ACTSUB).

      *EXIT-SUM-FIELDS-5-DS-FIXED.
      *    EXIT.

      ******************************************************************
      *
      *    S U M - F I E L D S - 5 - D S - D I F F
      *
      ******************************************************************
      *SUM-FIELDS-5-DS-DIFF SECTION.
      *    ADD HDS-NO-PENDING (1 ACTSUB TYPSUB) TO
      *        SUM-NO-PENDING (SUB4 ACTSUB).
      *    SUBTRACT HDS-NO-PENDING (PERIOD ACTSUB TYPSUB) FROM
      *        SUM-NO-PENDING (SUB4 ACTSUB).

      *    ADD HDS-NO-COND-APP(1 ACTSUB TYPSUB) TO
      *        SUM-NO-COND-APP(SUB4 ACTSUB).
      *    SUBTRACT HDS-NO-COND-APP(PERIOD ACTSUB TYPSUB) FROM
      *        SUM-NO-COND-APP(SUB4 ACTSUB).

      *    ADD HDS-NO-APPROVED(1 ACTSUB TYPSUB) TO
      *        SUM-NO-APPROVED(SUB4 ACTSUB).
      *    SUBTRACT HDS-NO-APPROVED(PERIOD ACTSUB TYPSUB) FROM
      *        SUM-NO-APPROVED(SUB4 ACTSUB).

      *EXIT-SUM-FIELDS-5-DS-DIFF.
      *    EXIT.

      ******************************************************************
      *
      *    S U M - F I E L D S - 5 - D Y
      *
      *-----------------------------------------------------------------
      *    SUM FIELDS TODAYS TOTALS --- PAGE 5
      *
      *  ACTSUB: ACTIVITY   : 1 - NET FIXED / LOSS   2 - NEW VOLUME
      *
      *  TYPSUB: TYPE TABLE : 1 - NB   4 - PB    7 - RN   10 - N/A(FREE)
      *                       2 - RF   5 - PBD   8 - SC   11 - N/A(FREE)
      *                       3 - FB   6 - PBI   9 - OT
      *
      ******************************************************************
      *SUM-FIELDS-5-DY SECTION.
      *    MOVE TYPSUB TO SUB4.
      *    PERFORM SUM-FIELDS-5-DY-FIXED.
      *    MOVE 3 TO SUB4.
      *    PERFORM SUM-FIELDS-5-DY-FIXED.

      *EXIT-SUM-FIELDS-5-DY.
      *    EXIT.

      ******************************************************************
      *
      *    S U M - F I E L D S - 5 - D Y - F I X E D
      *
      *-----------------------------------------------------------------
      *    DISPLAYS DIFFERENCE BETWEEN CURRENT AND ELM, ELY.
      *
      *  IF PAGE5-TODAY
      *     INCREMENT ONLY CURRENT TOTALS (SKIP MTD AND YTD)
      *  ELSE
      *     INCREMENT CUR, MTD, YTD.
      ******************************************************************
      *SUM-FIELDS-5-DY-FIXED SECTION.

      *    IF PAGE5-TODAY
      *       GO TO PAGE-5-DAILY.

      * DON'T ADD TODAY WHEN DISPLAYING LYR VOLUME:
      *    IF ( ACTSUB = 2 AND PERIOD = 3 )
      *       GO TO EXIT-SUM-FIELDS-5-DY-FIXED.
      *    ADD HDY-NO-PENDING (ACTSUB TYPSUB) TO
      *        SUM-NO-PENDING (SUB4 ACTSUB).
      *    ADD HDY-NO-COND-APP(ACTSUB TYPSUB) TO
      *        SUM-NO-COND-APP(SUB4 ACTSUB).
      *    ADD HDY-NO-APPROVED(ACTSUB TYPSUB) TO
      *        SUM-NO-APPROVED(SUB4 ACTSUB).
      *    ADD HDY-NO-APPROVED(ACTSUB TYPSUB) TO
      *        SUM-NO-APPROVED(SUB4 ACTSUB).
      *    ADD HDY-NO-MADE(ACTSUB TYPSUB) TO
      *        SUM-NO-MADE(SUB4 ACTSUB).
      *    ADD HDY-NO-TURNUSDOWN(ACTSUB TYPSUB) TO
      *        SUM-NO-TUD(SUB4 ACTSUB).

      *    GO TO EXIT-SUM-FIELDS-5-DY-FIXED.

      *PAGE-5-DAILY.
      *    ADD HDY-NO-PENDING (ACTSUB TYPSUB) TO
      *        SUM-NO-PENDING (SUB4 ACTSUB).
      *    ADD HDY-NO-COND-APP(ACTSUB TYPSUB) TO
      *        SUM-NO-COND-APP(SUB4 ACTSUB).
      *    ADD HDY-NO-APPROVED(ACTSUB TYPSUB) TO
      *        SUM-NO-APPROVED(SUB4 ACTSUB).
      *    ADD HDY-NO-MADE(ACTSUB TYPSUB) TO
      *        SUM-NO-MADE(SUB4 ACTSUB).
      *    ADD HDY-NO-TURNDOWN(ACTSUB TYPSUB) TO
      *        SUM-NO-TURNDOWN(SUB4 ACTSUB).
      *    ADD HDY-NO-TURNUSDOWN(ACTSUB TYPSUB) TO
      *        SUM-NO-TUD(SUB4 ACTSUB).

      *EXIT-SUM-FIELDS-5-DY-FIXED.
      *    EXIT.

      ********************************************************
      *    SUM FIELDS PAGE 6 - BRANCH TOTALS
      ********************************************************
      *    PERIOD  :      CUR | MTD
      *                   ELM | YTD
      *                   ELY | LYR
      *
      *    ACTIVITY: NET GAIN | VOLUME
      *
      *    TYPE    :  1. INTEREST
      *               2. ADDON
      *               3. SERVICE CHARGE
      *               4. MAINTENANCE FEE
      *               5. DEALER DISCOUNT
      *               6. OTHER
      ********************************************************
       SUM-FIELDS-6-DS SECTION.
      *******************************************
           PERFORM VARYING PERIOD FROM 1 BY 1 UNTIL PERIOD > 3
                     AFTER ACTSUB FROM 1 BY 1 UNTIL ACTSUB > 2
                     AFTER TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 6
              IF (PERIOD = 1 AND ACTSUB = 1) OR ACTSUB = 2
                 ADD HDS-UNEARNED-AMT(PERIOD ACTSUB TYPSUB) TO
                     SUM-UNEARNED-AMT(PERIOD ACTSUB TYPSUB)
              END-IF
              IF ACTSUB = 1 AND (PERIOD = 2 OR 3)
                 ADD HDS-UNEARNED-AMT(1 ACTSUB TYPSUB) TO
                     SUM-UNEARNED-AMT(PERIOD ACTSUB TYPSUB)
                 SUBTRACT HDS-UNEARNED-AMT(PERIOD ACTSUB TYPSUB) FROM
                          SUM-UNEARNED-AMT(PERIOD ACTSUB TYPSUB)
              END-IF
           END-PERFORM.

       EXIT-SUM-FIELDS-6-DS.
           EXIT.

      ********************************************************
      *    SUM FIELDS PAGE 6 - TODAYS TOTALS
      ********************************************************
      *    ACTIVITY: NET GAIN | VOLUME
      *
      *    TYPE    :  1. INTEREST
      *               2. ADDON
      *               3. SERVICE CHARGE
      *               4. MAINTENANCE FEE
      *               5. DEALER DISCOUNT
      *               6. OTHER
      ********************************************************
       SUM-FIELDS-6-DY SECTION.
      *******************************************
           PERFORM VARYING ACTSUB FROM 1 BY 1 UNTIL ACTSUB > 2
                     AFTER TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 6
              ADD HDY-UNEARNED-AMT(ACTSUB TYPSUB) TO
                         SUM-UNEARNED-AMT(1 ACTSUB TYPSUB)
              IF PAGE6-TOTAL
                 ADD HDY-UNEARNED-AMT(ACTSUB TYPSUB) TO
                            SUM-UNEARNED-AMT(2 ACTSUB TYPSUB)
                            SUM-UNEARNED-AMT(3 ACTSUB TYPSUB)
              END-IF
           END-PERFORM.

       EXIT-SUM-FIELDS-6-DY.
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

           MOVE 19000101 TO RC-TRANS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WBEG-TRANS-DATE.

           MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WEND-TRANS-DATE.

           PERFORM START-RC2-FILE.

           PERFORM READ-RC2-FILE-NEXT.
           IF RC-BRNO = BR-NO
              IF RC-STATUS = "A1"
                 MOVE RC-TRANS-DATE TO FIRST-OPEN-DATE.

           PERFORM CLOSE-RC2-FILE.
       EXIT-GET-FIRST-OPEN.
           EXIT.

      *************************************
       ENTRY-MODULE SECTION.
      *************************************
       ENTER-SELECTION.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO READ-ONCE-FG
              GO TO END-ENTRY-MODULE.

      *******************************************************
      *   TEST FOR SET UP OF TRAP FOR ANY DAY'S DY-REC:
      *
      *******************************************************
      *   - TO ACTIVATE TRAP FOR ANY DAY'S DY-REC;
      *         ONLY ALLOWED IN REAL TIME MODE!!!!
      *           1. PRESS F9
      *           2. ENTER 3535 (HIT RETURN)
      *           3. ENTER TODAYS TOTAL DATE IN QUESTION
      *   - TO CHANGE TRAP DATE;
      *         WHILE TRAP IS ACTIVATED AND IN TODAYS MODE
      *           1. PRESS F9
      *           2. ENTER NEW TRAP DATE
      *   - TO INCREMENT TRAP DATE;
      *         WHILE TRAP IS ACTIVATED AND IN TODAYS MODE
      *           1. PRESS F9
      *           2. PRESS F9 TO INCREMENT TRAP DATE BY 1 DAY
      *   - TO STOP TRAP;
      *         WHILE IN TODAYS MODE, PAGE 3 ONLY,
      *           1. PRESS F9
      *           3. ENTER 0 (HIT RETURN)
      *
      *******************************************************

      *******************************************************
      *   TEST FOR SET UP OF TRAP FOR SKIPPING TODAYS DY-REC:
      *
      *******************************************************
      *   - TO ACTIVATE SKIP TODAYS TOTALS TRAP;
      *         ONLY ALLOWED IN REAL TIME MODE!!!!
      *           1. PRESS F9
      *           2. ENTER 3636 (HIT RETURN)
      *           3. TYPE 'SKIP'
      *   - TO STOP SKIP TODAYS TOTALS TRAP;
      *           1. PRESS F9
      *           2. ENTER 3636 (HIT RETURN)
      *           3. TYPE 'SKIP'
      *******************************************************
      *    IF GROUP-FLAG NOT = "Y"
      *JTG    IF DELINQ = 4
              IF READ-EVERY
                 IF VDUSTATUS = "1("
                    PERFORM ALARM
                    MOVE "ENNN060SEL." TO VDU
                    PERFORM SCREENIO
                    IF VDUNUM0 = 3636
                       MOVE "E  A060SEL." TO VDU
                       PERFORM SCREENIO
                       MOVE VDUBUF TO ACTION
                       IF ACTION = "  SKIP"
                          MOVE "Y" TO TRAP-NO-DY-FG
                          MOVE " " TO TRAP-A-DATE-DY-FG
                          MOVE "TODAYS TOTALS WILL BE SKIPPED!"
                                                        TO MESS
                          PERFORM SEND-MESS
                          PERFORM SEND-BR-NAME
                          GO TO ACTIVITY-OPTION
                       ELSE
                          MOVE " " TO TRAP-NO-DY-FG
                          MOVE "TODAYS TOTALS WILL BE INCLUDED!"
                                                        TO MESS
                          PERFORM SEND-MESS
                          PERFORM SEND-BR-NAME
                          GO TO ACTIVITY-OPTION
                    ELSE
                       IF TRAP-A-DATE-DY-FG = " "
                          IF VDUNUM0 = 3535
                             MOVE "Y" TO TRAP-A-DATE-DY-FG
                             MOVE " " TO TRAP-NO-DY-FG
                             MOVE "TODAYS TOTALS TRAP TURNED ON!"
                                                           TO MESS
                             PERFORM SEND-MESS
                             MOVE TODAY-MM TO NUM-MO
                             MOVE TODAY-DD TO NUM-DA
                             MOVE TODAY-YY TO NUM-YR
                             MOVE NUM-DATE TO DY-TRAP-DATE
                             PERFORM SEND-BR-NAME
                             GO TO ACTIVITY-OPTION
                          ELSE
                             GO TO ENTER-SELECTION
                       ELSE
                          IF VDUSTATUS = "1("
                             MOVE DY-TRAP-DATE TO NUM-DATE
                             PERFORM JUL
                             ADD 1 TO JULIAN-DATE
                             PERFORM CJUL
                             MOVE NUM-DATE TO DY-TRAP-DATE
                             MOVE "Y" TO TRAP-A-DATE-DY-FG
                             MOVE " " TO TRAP-NO-DY-FG
                             MOVE "TODAYS TOTALS TRAP DATE INCREMENTED!"
                                                              TO MESS
                             PERFORM SEND-MESS
                             PERFORM SEND-BR-NAME
                             GO TO ACTIVITY-OPTION
                          ELSE
                             IF VDUNUM0 NOT = 0
                                MOVE VDUNUM0 TO DY-TRAP-DATE
                                MOVE "Y" TO TRAP-A-DATE-DY-FG
                                MOVE " " TO TRAP-NO-DY-FG
                                MOVE "TODAYS TOTALS TRAP DATE SET!"
                                                              TO MESS
                                PERFORM SEND-MESS
                                PERFORM SEND-BR-NAME
                                GO TO ACTIVITY-OPTION
                             ELSE
                                MOVE " " TO TRAP-A-DATE-DY-FG
                                MOVE "TODAYS TOTALS TRAP TURNED OFF!"
                                                              TO MESS
                                PERFORM SEND-MESS
                                PERFORM SEND-BR-NAME
                                GO TO ACTIVITY-OPTION.


           IF PAGE3
      * CURRENT:
              IF VDUSTATUS = "12"
                 MOVE 1 TO DELINQ
                 GO TO DELINQ-OPTION
              ELSE
      * TODAY:
      *------>IF VDUSTATUS = "16"
      *          MOVE 4 TO DELINQ
      *          GO TO DELINQ-OPTION
      *------>ELSE
      * CHANGE SINCE BEGINNING OF MONTH:
              IF VDUSTATUS = "18"
                 IF DELINQ NOT = 5
                    MOVE 5 TO DELINQ
                    MOVE 0 TO DIFF-PER
                    GO TO DELINQ-OPTION
                 ELSE
                    IF DIFF-PER = 0
                       MOVE 1 TO DIFF-PER
                       GO TO DELINQ-OPTION
                    ELSE
                       MOVE 0 TO DIFF-PER
                       GO TO DELINQ-OPTION
              ELSE
      * TOGGLE BEG OF MONTH & END OF LAST MONTH:
              IF VDUSTATUS = "1:"
                 IF DELINQ = 2
                    MOVE 3 TO DELINQ
                 ELSE
                    MOVE 2 TO DELINQ
                 END-IF
                 GO TO DELINQ-OPTION
              ELSE
      * TOGGLE DELINQUENCY TO (1) INCLUDE (2)SKIP (3) ONLY BANKRUPT:
              IF VDUSTATUS = "1<"
                 IF BNKRPT-DEL = 3
                    MOVE 0 TO BNKRPT-DEL
                 END-IF
                 ADD 1 TO BNKRPT-DEL
                 GO TO DELINQ-OPTION
              ELSE
      * DAILY OPERATING STATISTICS:
              IF VDUSTATUS = "1>"
                 IF DELINQ NOT = 7
                    MOVE 7 TO DELINQ
                    MOVE 0 TO DAILY-TYPE
                    GO TO DELINQ-OPTION
                 ELSE
                    IF DAILY-TYPE = 0
                       MOVE 1 TO DAILY-TYPE
                       GO TO DELINQ-OPTION
                    ELSE
                       MOVE 0 TO DAILY-TYPE
                       GO TO DELINQ-OPTION.

           IF PAGE4
              IF VDUSTATUS = "18"
                 MOVE 1 TO ACTIVITY
                 GO TO ACTIVITY-OPTION
              ELSE
                 IF VDUSTATUS = "1:"
                    MOVE 2 TO ACTIVITY
                    GO TO ACTIVITY-OPTION
                 ELSE
                    IF VDUSTATUS = "12"
                       MOVE 1 TO PAGE4-TYPE
                       GO TO ACTIVITY-OPTION.
      **----------->ELSE
      **               IF VDUSTATUS = "16"
      **                  MOVE 2 TO PAGE4-TYPE
      **                  GO TO ACTIVITY-OPTION
      **               ELSE
      **                  IF VDUSTATUS = "1<"
      **                     IF PAGE4-O2
      **                        MOVE 1 TO PAGE4-O2-TYPE
      **                     ELSE
      **                        ADD 1 TO PAGE4-O2-TYPE
      **                     END-IF
      **-------------------->GO TO ACTIVITY-OPTION.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  IF ON PAGE 5, JUST RE-DISPLAY SCREEN, PERIOD WILL CHANGE THERE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              IF ( PAGE5 )
                 IF ( F2-KEY )
                    MOVE 1 TO PAGE5-TYPE
                    PERFORM NEW-PAGE5-SETUP
                    GO TO ACTIVITY-OPTION
                 ELSE
      **-------->IF ( F3-KEY )
      **            MOVE 2 TO PAGE5-TYPE
      **            PERFORM NEW-PAGE5-SETUP
      **            GO TO ACTIVITY-OPTION
      **-------->ELSE
                 IF ( F4-KEY OR F5-KEY )
                    IF PAGE5-TOTAL
                       PERFORM NEW-PAGE5-SETUP
                       GO TO ACTIVITY-OPTION
                    END-IF.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  IF ON PAGE 6, JUST RE-DISPLAY SCREEN, PERIOD WILL CHANGE THERE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              IF PAGE6
                 IF F2-KEY
                    MOVE 1 TO PAGE6-TYPE
                    GO TO ACTIVITY-OPTION.
      *--------->ELSE
      *          IF F3-KEY
      *             MOVE 2 TO PAGE6-TYPE
      *------------>GO TO ACTIVITY-OPTION.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SELECTION.

           MOVE VDUBUF TO ACTION.
           IF PAGE1
              IF ACTION = "    P2" OR "    P3" OR "    P4" OR "    P5"
                       OR "    P6"
                 PERFORM SWITCH-FORM
                 GO TO ENTER-SELECTION.
           IF PAGE2
              IF ACTION = "    P1" OR "    P3" OR "    P4" OR "    P5"
                       OR "    P6"
                 PERFORM SWITCH-FORM
                 GO TO ENTER-SELECTION.
           IF PAGE3
              IF ACTION = "    P1" OR "    P2" OR "    P4" OR "    P5"
                       OR "    P6"
                 PERFORM SWITCH-FORM
                 GO TO ENTER-SELECTION.
           IF PAGE4
              IF ACTION = "    P1" OR "    P2" OR "    P3" OR "    P5"
                       OR "    P6"
                 PERFORM SWITCH-FORM
                 GO TO ENTER-SELECTION.
           IF PAGE5
              IF ACTION = "    P1" OR "    P2" OR "    P3" OR "    P4"
                       OR "    P6"
                 PERFORM NEW-PAGE5-SETUP
                 PERFORM SWITCH-FORM
                 GO TO ENTER-SELECTION.
           IF PAGE6
              IF ACTION = "    P1" OR "    P2" OR "    P3" OR "    P4"
                       OR "    P5"
                 PERFORM SWITCH-FORM
                 GO TO ENTER-SELECTION.

           GO TO ENTER-SELECTION.

       ACTIVITY-OPTION.
           PERFORM SUMMARY-MODULE.
           PERFORM DISPLAY-PAGE.
           GO TO ENTER-SELECTION.

       DELINQ-OPTION.
           MOVE DELINQ TO SUB2.
           PERFORM SUMMARY-MODULE.
           PERFORM DISPLAY-PAGE.
           GO TO ENTER-SELECTION.

       SWITCH-FORM.
           PERFORM FORM-DESTROY.
           MOVE FORM-BUFFER(ACTION-P1) TO VDU-FORM-NAME.
           MOVE ACTION-P1 TO PAGE-FLAG.

           PERFORM SUMMARY-MODULE.

           MOVE FORM-PATH TO FORM-PATHNAME.
           PERFORM FORM-RESET.

      *    PERFORM GETFM.
      *    CALL XFER USING STAT INQUIRY-LIST(READ-TYPE)
      *                         INQUIRY-TYPE(READ-TYPE).
           MOVE INQUIRY-TYPE(READ-TYPE) TO WR-BUF.
           MOVE INQUIRY-LIST(READ-TYPE) TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BRNO." TO VDU
              MOVE ENT-GROUP TO VDUBUF
           ELSE
              MOVE "SN N040BRNO." TO VDU
              MOVE STORE-BRNO TO VDUNUM0
                                 BR-NO
              PERFORM READ-BR-FILE.
           PERFORM SCREENIO.

           PERFORM SEND-BR-NAME.

           MOVE "SN N042DLNO." TO VDU.
           IF ONE-DEALER
              MOVE STORE-DLNO TO VDUNUM2
           ELSE
              MOVE "S  A000DLNO." TO VDU
              MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SN A025DLNAME." TO VDU.
           MOVE STORE-DLNAME TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A030CLASS." TO VDU.
           IF ONE-CLASS
              MOVE "SN N030CLASS." TO VDU
              MOVE STORE-CLASS TO VDUNUM0
           ELSE
              MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000CLSDESC." TO VDU.
           MOVE STORE-LCNAME TO VDUBUF.
           PERFORM SCREENIO.
           PERFORM DISPLAY-PAGE.

       END-ENTRY-MODULE.
           EXIT.

      *****************************************************************
       DISPLAY-PAGE SECTION.
      *****************************************************************
           MOVE DISPLAY-DATE TO WR-BUF.
           MOVE DATE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF PAGE1 PERFORM DISPLAY-FIELDS-1
           ELSE IF PAGE2 PERFORM DISPLAY-FIELDS-2
           ELSE IF PAGE3
                    MOVE DEL-MSEL(DELINQ) TO WR-BUF
                    MOVE DEL-MSEL-LIST TO WR-LIST
                    PERFORM CALL-WRFORM
                    PERFORM DISPLAY-FIELDS-3
           ELSE IF PAGE4 PERFORM DISPLAY-FIELDS-4
           ELSE IF PAGE5 PERFORM DISPLAY-FIELDS-5
           ELSE IF PAGE6 PERFORM DISPLAY-FIELDS-6.

       END-DISPLAY-PAGE.
           EXIT.

      *****************************************************************
       DISPLAY-FIELDS-1 SECTION.
      *****************************************************************
           MOVE " " TO DISPLAY-BUF1
                       MESS.
           PERFORM CALL-TMMSG.
           PERFORM MOVE-1-TOTALS.
           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT-1.
           EXIT.

      *****************************************************************
       MOVE-1-TOTALS SECTION.
      *****************************************************************
           MOVE SUM-DLRVPRIN(1)      TO A01.
           MOVE SUM-DLRVPRIN(2)      TO A02.
           MOVE SUM-DLRVPRIN(3)      TO A03.
           MOVE SUM-DLRVCHGS(1)      TO A04.
           MOVE SUM-DLRVCHGS(2)      TO A05.
           MOVE SUM-DLRVCHGS(3)      TO A06.
           MOVE SUM-DLPARVHELD       TO A07.
           MOVE SUM-UNPAIDCK         TO A08.
           MOVE SUM-DLDISC           TO A09.
           MOVE SUM-DLPARVPAID       TO A10.
           COMPUTE A11  = SUM-SUMWGTTA / SUM-SUMWGTTM.
           COMPUTE A111 = SUM-SUMWGTTA / SUM-SUMWGTAV.
           MOVE SUM-DLDISC-VOL(1)    TO A13.
           MOVE SUM-DLDISC-VOL(2)    TO A14.
           MOVE SUM-DLDISC-VOL(3)    TO A15.
           MOVE SUM-PROCEEDS(1)      TO A16.
           MOVE SUM-PROCEEDS(2)      TO A17.
      *---- AVAILABLE SPACES ------->TO A18.
           MOVE SUM-RCOURSE-UNITS(1) TO A19.
           MOVE SUM-RCOURSE-AMT(1)   TO A20.
           MOVE SUM-RCOURSE-UNITS(2) TO A21.
           MOVE SUM-RCOURSE-AMT(2)   TO A22.
           MOVE SUM-RCOURSE-UNITS(3) TO A23.
           MOVE SUM-RCOURSE-AMT(3)   TO A24.

           IF ( DL-AFP-DEALER = "Y" )
              MOVE "DUE DLR MEMO :" TO PROMPT-A25
              MOVE SUM-AFPDUEDL-MEMO TO A25
           ELSE
              MOVE "N/A          :" TO PROMPT-A25
              MOVE ZEROES           TO A25.

       EXIT-MOVE-1.
           EXIT.

      *****************************************************************
       DISPLAY-FIELDS-2 SECTION.
      *****************************************************************
           PERFORM MOVE-2-TOTALS.
           MOVE DISPLAY-BUF2A TO WR-BUF.
           MOVE DISPLAY-LIST2A TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE DISPLAY-BUF2B TO WR-BUF.
           MOVE DISPLAY-LIST2B TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE " " TO MESS.
           PERFORM CALL-TMMSG.
       DISPLAY-FIELDS-EXIT-2.
           EXIT.

      *****************************************************************
       MOVE-2-TOTALS SECTION.
      *****************************************************************
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF SUB = 1
                 MOVE SUM2-OUTSUNIT(SUB) TO B01(1, SUB)
                 MOVE SUM2-PLOSUNIT(SUB) TO B01(2, SUB)
                 MOVE SUM2-RCPLUNIT(SUB) TO B01(3, SUB)
                 MOVE SUM2-JDOSUNIT(SUB) TO B01(4, SUB)
                 MOVE SUM2-OUTSAMT(SUB)  TO B02(1, SUB)
                 MOVE SUM2-PLOSAMT(SUB)  TO B02(2, SUB)
                 MOVE SUM2-RCPLAMT(SUB)  TO B02(3, SUB)
                 MOVE SUM2-JDOSAMT(SUB)  TO B02(4, SUB)
              ELSE
                 COMPUTE B01(1, SUB) =
                             SUM2-OUTSUNIT(1) - SUM2-OUTSUNIT(SUB)
                 COMPUTE B01(2, SUB) =
                             SUM2-PLOSUNIT(1) - SUM2-PLOSUNIT(SUB)
                 COMPUTE B01(3, SUB) =
                             SUM2-RCPLUNIT(1) - SUM2-RCPLUNIT(SUB)
                 COMPUTE B01(4, SUB) =
                             SUM2-JDOSUNIT(1) - SUM2-JDOSUNIT(SUB)
                 COMPUTE B02(1, SUB) =
                             SUM2-OUTSAMT(1) - SUM2-OUTSAMT(SUB)
                 COMPUTE B02(2, SUB) =
                             SUM2-PLOSAMT(1) - SUM2-PLOSAMT(SUB)
                 COMPUTE B02(3, SUB) =
                             SUM2-RCPLAMT(1) - SUM2-RCPLAMT(SUB)
                 COMPUTE B02(4, SUB) =
                             SUM2-JDOSAMT(1) - SUM2-JDOSAMT(SUB)
              END-IF
              MOVE SUM2-NPURUNIT(SUB) TO B01(5, SUB)
              MOVE SUM2-OPURUNIT(SUB) TO B01(6, SUB)
              MOVE SUM2-RNEWUNIT(SUB) TO B01(7, SUB)
              MOVE SUM2-PAIDUNIT(SUB) TO B01(8, SUB)
              MOVE SUM2-TOLNUNIT(SUB) TO B01(9, SUB)
              MOVE SUM2-NPURAMT(SUB)  TO B02(5, SUB)
              MOVE SUM2-OPURAMT(SUB)  TO B02(6, SUB)
              MOVE SUM2-RNEWAMT(SUB)  TO B02(7, SUB)
              MOVE SUM2-PAIDAMT(SUB)  TO B02(8, SUB)
              MOVE SUM2-TOLNAMT(SUB)  TO B02(9, SUB)
           END-PERFORM.

       EXIT-MOVE-2.
           EXIT.

      *****************************************************************
       DISPLAY-FIELDS-3 SECTION.
      *****************************************************************
           PERFORM NEW-DEL-DELINQ.

           IF DELINQ = 1 OR 2 OR 3 OR 4
              MOVE ZEROS TO PAGE-3-TOT-TABLE
              MOVE 1 TO SUBX
              PERFORM SETUP3-AMOUNT
                   VARYING AGESUB FROM 1 BY 1 UNTIL AGESUB > 10
                     AFTER RECCON FROM 1 BY 1 UNTIL RECCON > 2.

           IF DELINQ = 5
              MOVE ZEROS TO PAGE-3-TOT-TABLE
              MOVE ZEROS TO HOLD-PAGE-3-TOT-TABLE
              MOVE 1 TO SUBX
              IF DIFF
                 PERFORM SETUP3-DIFF
                      VARYING AGESUB FROM 1 BY 1 UNTIL AGESUB > 10
                        AFTER RECCON FROM 1 BY 1 UNTIL RECCON > 2
              ELSE
                 PERFORM SETUP3-PER
                      VARYING AGESUB FROM 1 BY 1 UNTIL AGESUB > 10
                        AFTER RECCON FROM 1 BY 1 UNTIL RECCON > 2.

           IF DELINQ = 7
              PERFORM SETUP3-DAILY-LOAN-STATS
              MOVE ZEROS TO PAGE-3-DAILY-TOT-TABLE
                            PAGE-3-REL-DAILY-TOT-TABLE
              MOVE 1 TO SUBX
              PERFORM SETUP3-DAILY-DELINQ-STATS
                   VARYING AGESUB FROM 1 BY 1 UNTIL AGESUB > 5
                     AFTER RECCON FROM 1 BY 1 UNTIL RECCON > 2.

           IF DELINQ = 7
              IF BNKRPT-DEL = 1
                 MOVE "B.I." TO DAILY-LN-PROMPT(12)
              END-IF
              IF BNKRPT-DEL = 2
                 MOVE "B.S." TO DAILY-LN-PROMPT(12)
              END-IF
              IF BNKRPT-DEL = 3
                 MOVE "B.O." TO DAILY-LN-PROMPT(12)
              END-IF
              IF DAILY-TOP
                 MOVE DAILY-BUF3 TO WR-BUF
                 MOVE DAILY-LIST3-TOP TO WR-LIST
                 PERFORM CALL-WRFORM
              ELSE
                 MOVE DAILY-BUF3 TO WR-BUF
                 MOVE DAILY-LIST3-BOTTOM TO WR-LIST
                 PERFORM CALL-WRFORM
           ELSE
              IF BNKRPT-DEL = 1
                 MOVE "                              BANKRUPT INCLUDED"
                                                   TO DEL-TABLE(17)
                 MOVE "B.I." TO DEL-TABLE(18)
              END-IF
              IF BNKRPT-DEL = 2
                 MOVE "                              BANKRUPT SKIPPED"
                                                   TO DEL-TABLE(17)
                 MOVE "B.S." TO DEL-TABLE(18)
              END-IF
              IF BNKRPT-DEL = 3
                 MOVE "                                BANKRUPT ONLY"
                                                   TO DEL-TABLE(17)
                 MOVE "B.O." TO DEL-TABLE(18)
              END-IF
              MOVE DISPLAY-BUF3 TO WR-BUF
              MOVE DISPLAY-LIST3 TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE " " TO MESS.
           PERFORM CALL-TMMSG.

       DISPLAY-FIELDS-EXIT-3.
           EXIT.

      **********************************************
      *    SETUP AMOUNTS
      *
      *   BY TABLE  AGESUB    SUBX    DISPLAY
      *   --------  ------    ----    -------
      *    30         1        1        30
      *    60         2        2        60
      *    90         3        3        90
      *   120         4        4       120
      *   150         5        5       150
      *   180         6        6       180
      *   210         7        7       210
      *                        8       DEL
      *   <M>         8        9       <M>
      *   <P>         9       10       <P>
      *                       11      SLOW
      *   CURRENT    10       12       CUR
      *                       13     TOTAL
      **********************************************
       SETUP3-AMOUNT SECTION.
      *****************************************************************

      * ADD TO TOTAL DELINQUENCY:

           ADD SUM-DEL-UNITS(AGESUB RECCON)
                               TO TOT-DEL-UNITS(1 RECCON).
           ADD SUM-DEL-OUTBAL(AGESUB RECCON)
                               TO TOT-DEL-OUTBAL(1 RECCON).

      * TEST FOR 90+, IF SO ADDED IN 150+ FOR DISPLAY ONLY:
      *B   IF AGESUB = 3
      *B      ADD 1 AGESUB GIVING SUB
      *B      COMPUTE DEL-UNITS(SUBX RECCON) =
      *B               SUM-DEL-UNITS(AGESUB RECCON)
      *B                    + SUM-DEL-UNITS(SUB RECCON)
      *B      COMPUTE DEL-OUTBAL(SUBX RECCON) =
      *B               SUM-DEL-OUTBAL(AGESUB RECCON)
      *B                    + SUM-DEL-OUTBAL(SUB RECCON)
      *B   ELSE
      * TEST FOR <P>, IF SO ADDED IN <M> FOR DISPLAY ONLY:
      *B   IF AGESUB = 6
      *B      SUBTRACT 1 FROM AGESUB GIVING SUB
      *B      COMPUTE DEL-UNITS(SUBX RECCON) =
      *B               SUM-DEL-UNITS(AGESUB RECCON)
      *B                    + SUM-DEL-UNITS(SUB RECCON)
      *B      COMPUTE DEL-OUTBAL(SUBX RECCON) =
      *B               SUM-DEL-OUTBAL(AGESUB RECCON)
      *B                    + SUM-DEL-OUTBAL(SUB RECCON)
      *B   ELSE
              MOVE SUM-DEL-UNITS(AGESUB RECCON)
                                  TO DEL-UNITS(SUBX RECCON)
              MOVE SUM-DEL-OUTBAL(AGESUB RECCON)
                                  TO DEL-OUTBAL(SUBX RECCON).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * WILL NOT DISPLAY NB, SF, FB, PB, RN, SC, PO, OR PL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF SUBX > 13
              MOVE 0 TO DEL-UNITS(SUBX RECCON)
                        DEL-OUTBAL(SUBX RECCON).

      *----------------------------------------------------------------
      *  AGESUB =  7 (210+) DISPLAY DEL TOTALS  ->  SUBX = 8
      *         =  9 (POTS) DISPLAY SLOW TOTALS ->  SUBX = 11
      *         = 10 (CUR)  DISPLAY TOT TOTALS  ->  SUBX = 13
      *----------------------------------------------------------------
           IF AGESUB = 7 OR 9 OR 10
              ADD 1 SUBX GIVING SUBY
              MOVE TOT-DEL-UNITS(1 RECCON)
                               TO DEL-T-UNITS(SUBY RECCON)
              MOVE TOT-DEL-OUTBAL(1 RECCON)
                                TO DEL-T-OUTBAL(SUBY RECCON)
              PERFORM SETUP3-SIGN
              IF RECCON = 2
                 ADD 1 TO SUBX.

           IF RECCON = 2
              ADD 1 TO SUBX.

       SETUP3-AMOUNT-EXIT.
           EXIT.

      **********************************************
      *    SETUP DIFFERENCE TO RELATIVE
      *
      *   BY TABLE  AGESUB    SUBX    DISPLAY
      *   --------  ------    ----    -------
      *    30         1        1        30
      *    60         2        2        60
      *    90         3        3        90
      *   120         4        4       120
      *   150         5        5       150
      *   180         6        6       180
      *   210         7        7       210
      *                        8       DEL
      *   <M>         8        9       <M>
      *   <P>         9       10       <P>
      *                       11      SLOW
      *   CURRENT    10       12       CUR
      *                       13     TOTAL
      **********************************************
       SETUP3-DIFF SECTION.
      *****************************************************************

           COMPUTE UNITS-DIFF ROUNDED =
                 SUM-DEL-UNITS(AGESUB RECCON)
               - SUM-REL-DEL-UNITS(AGESUB RECCON).

      * ADD TO TOTAL:

           ADD UNITS-DIFF TO TOT-DEL-UNITS(1 RECCON).

      * TEST FOR 90+, IF SO ADDED IN 150+ FOR DISPLAY ONLY:
      *B   IF AGESUB = 3
      *B      ADD 1 TO AGESUB GIVING SUB
      *B      COMPUTE UNITS-DIFF ROUNDED =
      *B          SUM-DEL-UNITS(AGESUB RECCON)
      *B               + SUM-DEL-UNITS(SUB RECCON)
      *B                - SUM-REL-DEL-UNITS(AGESUB RECCON)
      *B                 - SUM-REL-DEL-UNITS(SUB RECCON)
      *B   ELSE
      * TEST FOR <P>, IF SO ADDED IN <M> FOR DISPLAY ONLY:
      *B   IF AGESUB = 6
      *B      SUBTRACT 1 FROM AGESUB GIVING SUB
      *B      COMPUTE UNITS-DIFF ROUNDED =
      *B          SUM-DEL-UNITS(AGESUB RECCON)
      *B               + SUM-DEL-UNITS(SUB RECCON)
      *B                - SUM-REL-DEL-UNITS(AGESUB RECCON)
      *B                 - SUM-REL-DEL-UNITS(SUB RECCON).

           MOVE UNITS-DIFF TO DEL-UNITS(SUBX RECCON).

           COMPUTE OUTBAL-DIFF ROUNDED =
                 SUM-DEL-OUTBAL(AGESUB RECCON)
               - SUM-REL-DEL-OUTBAL(AGESUB RECCON).

      * ADD TO TOTAL:

           ADD OUTBAL-DIFF TO TOT-DEL-OUTBAL(1 RECCON).

      * TEST FOR 90+, IF SO ADDED IN 150+ FOR DISPLAY ONLY:
      *B   IF AGESUB = 3
      *B      ADD 1 TO AGESUB GIVING SUB
      *B      COMPUTE OUTBAL-DIFF ROUNDED =
      *B          SUM-DEL-OUTBAL(AGESUB RECCON)
      *B               + SUM-DEL-OUTBAL(SUB RECCON)
      *B                - SUM-REL-DEL-OUTBAL(AGESUB RECCON)
      *B                 - SUM-REL-DEL-OUTBAL(SUB RECCON)
      *B   ELSE
      * TEST FOR <P>, IF SO ADDED IN <M> FOR DISPLAY ONLY:
      *B   IF AGESUB = 6
      *B      SUBTRACT 1 FROM AGESUB GIVING SUB
      *B      COMPUTE OUTBAL-DIFF ROUNDED =
      *B          SUM-DEL-OUTBAL(AGESUB RECCON)
      *B               + SUM-DEL-OUTBAL(SUB RECCON)
      *B                - SUM-REL-DEL-OUTBAL(AGESUB RECCON)
      *B                 - SUM-REL-DEL-OUTBAL(SUB RECCON).

           MOVE OUTBAL-DIFF TO DEL-OUTBAL(SUBX RECCON).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * WILL NOT DISPLAY NB, SF, FB, PB, RN, SC, PO, OR PL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF SUBX > 13
              MOVE 0 TO DEL-UNITS(SUBX RECCON)
                        DEL-OUTBAL(SUBX RECCON).

      *----------------------------------------------------------------
      *  AGESUB =  7 (210+) DISPLAY DEL TOTALS  ->  SUBX = 8
      *         =  9 (POTS) DISPLAY SLOW TOTALS ->  SUBX = 11
      *         = 10 (CUR)  DISPLAY TOT TOTALS  ->  SUBX = 13
      *----------------------------------------------------------------
           IF AGESUB = 7 OR 9 OR 10
              ADD 1 SUBX GIVING SUBY
              MOVE TOT-DEL-UNITS(1 RECCON)
                               TO DEL-T-UNITS(SUBY RECCON)
              MOVE TOT-DEL-OUTBAL(1 RECCON)
                                TO DEL-T-OUTBAL(SUBY RECCON)
              PERFORM SETUP3-SIGN
              IF RECCON = 2
                 ADD 1 TO SUBX.

           IF RECCON = 2
              ADD 1 TO SUBX.

       SETUP3-DIFF-EXIT.
           EXIT.

      **********************************************
      *    SETUP PERCENT TO RELATIVE
      *
      *   BY TABLE  AGESUB    SUBX    DISPLAY
      *   --------  ------    ----    -------
      *    30         1        1        30
      *    60         2        2        60
      *    90         3        3        90
      *   120         4        4       120
      *   150         5        5       150
      *   180         6        6       180
      *   210         7        7       210
      *                        8       DEL
      *   <M>         8        9       <M>
      *   <P>         9       10       <P>
      *                       11      SLOW
      *   CURRENT    10       12       CUR
      *                       13     TOTAL
      **********************************************
       SETUP3-PER SECTION.
      *****************************************************************

           COMPUTE UNITS-PER ROUNDED =
               (SUM-DEL-UNITS(AGESUB RECCON)
              - SUM-REL-DEL-UNITS(AGESUB RECCON))
              / SUM-REL-DEL-UNITS(AGESUB RECCON) * 100.

      * ADD TO TOTALS:

           ADD SUM-DEL-UNITS(AGESUB RECCON) TO
                  HOLD-TOT-DEL-UNITS(1 RECCON).
           SUBTRACT SUM-REL-DEL-UNITS(AGESUB RECCON) FROM
                  HOLD-TOT-DEL-UNITS(1 RECCON).
           ADD SUM-REL-DEL-UNITS(AGESUB RECCON) TO
                  HOLD-TOT-DEL-UNITS(2 RECCON).

      *     ADD UNITS-PER TO TOT-DEL-UNITS(1 RECCON).
      * TEST FOR 90+, IF SO ADDED IN 150+ FOR DISPLAY ONLY:
      *B   IF AGESUB = 3
      *B      ADD 1 TO AGESUB GIVING SUB
      *B      COMPUTE UNITS-PER ROUNDED =
      *B          (SUM-DEL-UNITS(AGESUB RECCON)
      *B               + SUM-DEL-UNITS(SUB RECCON)
      *B                - SUM-REL-DEL-UNITS(AGESUB RECCON)
      *B                 - SUM-REL-DEL-UNITS(SUB RECCON))
      *B          / (SUM-REL-DEL-UNITS(AGESUB RECCON) +
      *B             SUM-REL-DEL-UNITS(SUB RECCON)) * 100
      *B   ELSE
      * TEST FOR <P>, IF SO ADDED IN <M> FOR DISPLAY ONLY:
      *B   IF AGESUB = 6
      *B      SUBTRACT 1 FROM AGESUB GIVING SUB
      *B      COMPUTE UNITS-PER ROUNDED =
      *B          (SUM-DEL-UNITS(AGESUB RECCON)
      *B               + SUM-DEL-UNITS(SUB RECCON)
      *B                - SUM-REL-DEL-UNITS(AGESUB RECCON)
      *B                 - SUM-REL-DEL-UNITS(SUB RECCON))
      *B          / (SUM-REL-DEL-UNITS(AGESUB RECCON) +
      *B             SUM-REL-DEL-UNITS(SUB RECCON)) * 100.

           IF UNITS-PER NOT = 0
              MOVE UNITS-PER TO DEL-UNITS-P(SUBX RECCON)
              IF DEL-UNITS-SIGN(SUBX RECCON) = " "
                 MOVE "+" TO DEL-UNITS-SIGN(SUBX RECCON).

           COMPUTE OUTBAL-PER ROUNDED =
               (SUM-DEL-OUTBAL(AGESUB RECCON)
              - SUM-REL-DEL-OUTBAL(AGESUB RECCON))
              / SUM-REL-DEL-OUTBAL(AGESUB RECCON) * 100.

      * ADD TO TOTALS:

           ADD SUM-DEL-OUTBAL(AGESUB RECCON) TO
                  HOLD-TOT-DEL-OUTBAL(1 RECCON).
           SUBTRACT SUM-REL-DEL-OUTBAL(AGESUB RECCON) FROM
                  HOLD-TOT-DEL-OUTBAL(1 RECCON).
           ADD SUM-REL-DEL-OUTBAL(AGESUB RECCON) TO
                  HOLD-TOT-DEL-OUTBAL(2 RECCON).

      *     ADD OUTBAL-PER TO TOT-DEL-OUTBAL(1 RECCON).
      * TEST FOR 90+, IF SO ADDED IN 150+ FOR DISPLAY ONLY:
      *B   IF AGESUB = 3
      *B      ADD 1 TO AGESUB GIVING SUB
      *B      COMPUTE OUTBAL-PER ROUNDED =
      *B          (SUM-DEL-OUTBAL(AGESUB RECCON)
      *B               + SUM-DEL-OUTBAL(SUB RECCON)
      *B                - SUM-REL-DEL-OUTBAL(AGESUB RECCON)
      *B                 - SUM-REL-DEL-OUTBAL(SUB RECCON))
      *B          / (SUM-REL-DEL-OUTBAL(AGESUB RECCON) +
      *B             SUM-REL-DEL-OUTBAL(SUB RECCON)) * 100
      *B   ELSE
      * TEST FOR <P>, IF SO ADDED IN <M> FOR DISPLAY ONLY:
      *B   IF AGESUB = 6
      *B      SUBTRACT 1 FROM AGESUB GIVING SUB
      *B      COMPUTE OUTBAL-PER ROUNDED =
      *B          (SUM-DEL-OUTBAL(AGESUB RECCON)
      *B               + SUM-DEL-OUTBAL(SUB RECCON)
      *B                - SUM-REL-DEL-OUTBAL(AGESUB RECCON)
      *B                 - SUM-REL-DEL-OUTBAL(SUB RECCON))
      *B          / (SUM-REL-DEL-OUTBAL(AGESUB RECCON)  +
      *B             SUM-REL-DEL-OUTBAL(SUB RECCON)) * 100.
           IF OUTBAL-PER NOT = 0
              MOVE OUTBAL-PER TO DEL-OUTBAL(SUBX RECCON)
              IF DEL-OUTBAL-SIGN(SUBX RECCON) = " "
                 MOVE "+" TO DEL-OUTBAL-SIGN(SUBX RECCON).

      * ADD TO TOTALS:

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * WILL NOT DISPLAY NB, SF, FB, PB, RN, SC, PO, OR PL
      *  PERCENTAGES.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF SUBX > 13
              MOVE 0 TO DEL-UNITS-P(SUBX RECCON)
                        DEL-OUTBAL(SUBX RECCON).

      *----------------------------------------------------------------
      *  AGESUB =  7 (210+) DISPLAY DEL TOTALS  ->  SUBX = 8
      *         =  9 (POTS) DISPLAY SLOW TOTALS ->  SUBX = 11
      *         = 10 (CUR)  DISPLAY TOT TOTALS  ->  SUBX = 13
      *----------------------------------------------------------------
           IF AGESUB = 7 OR 9 OR 10
              ADD 1 SUBX GIVING SUBY
              COMPUTE TOT-DEL-UNITS(1 RECCON) =
                      HOLD-TOT-DEL-UNITS(1 RECCON) /
                           HOLD-TOT-DEL-UNITS(2 RECCON) * 100
              MOVE TOT-DEL-UNITS(1 RECCON)
                               TO DEL-T-UNITS-P(SUBY RECCON)

              COMPUTE TOT-DEL-OUTBAL(1 RECCON) =
                      HOLD-TOT-DEL-OUTBAL(1 RECCON) /
                           HOLD-TOT-DEL-OUTBAL(2 RECCON) * 100
              MOVE TOT-DEL-OUTBAL(1 RECCON)
                                TO DEL-T-OUTBAL(SUBY RECCON)

              PERFORM SETUP3-SIGN-PER
              IF RECCON = 2
                 ADD 1 TO SUBX.

           IF RECCON = 2
              ADD 1 TO SUBX.

       SETUP3-PER-EXIT.
           EXIT.

      *****************************************************************
       SETUP3-DAILY-LOAN-STATS SECTION.
      *****************************************************************
           MOVE SUM-DAILY-AMT TO DAILY-OSBAL.
           MOVE SUM-DAILY-NO TO DAILY-OSNO.
           COMPUTE DAILY-OSAVG ROUNDED =
                SUM-DAILY-AMT / SUM-DAILY-NO.

           COMPUTE DAILY-G-L-BAL ROUNDED =
                SUM-DAILY-AMT - SUM-REL-DAILY-AMT.
           COMPUTE DAILY-G-L-NO ROUNDED =
                SUM-DAILY-NO - SUM-REL-DAILY-NO.

           MOVE SUM-DAILY-PL-AMT TO DAILY-AMT-PL.
           MOVE SUM-DAILY-PL-NO TO DAILY-NO-PL.

           PERFORM SET-INCREASE-REMAIN.

      * SET VOLUME MTD:
           MOVE SUM-DAILY-VOL TO DAILY-VOL-BAL.

      * AVG LOAN AMOUNT
           MOVE 0 TO DAILY-VOL-AVG-LN.
           IF SUM-DAILY-SUMWGTTM NOT = 0
              DIVIDE SUM-DAILY-SUMWGTTA BY SUM-DAILY-SUMWGTTM
                          GIVING DAILY-VOL-AVG-LN ROUNDED.
      * AVG TERM
           MOVE 0 TO DAILY-VOL-AVG-TM.
           IF SUM-DAILY-SUMWGTAV NOT = 0
              DIVIDE SUM-DAILY-SUMWGTTA BY SUM-DAILY-SUMWGTAV
                          GIVING DAILY-VOL-AVG-TM ROUNDED.

      * NEW LOANS
           MOVE SUM-DAILY-NB TO DAILY-NB-NO.
           IF SUM-DAILY-NO NOT = 0
              COMPUTE DAILY-NB-PER ROUNDED =
                   SUM-DAILY-NB / SUM-DAILY-NO.

           MOVE SUM-DAILY-FB TO DAILY-FB-NO.
           IF SUM-DAILY-NO NOT = 0
              COMPUTE DAILY-RN-PER ROUNDED =
                   SUM-DAILY-FB / SUM-DAILY-NO.

           MOVE SUM-DAILY-PB TO DAILY-PB-NO.
           IF SUM-DAILY-NO NOT = 0
              COMPUTE DAILY-PB-PER ROUNDED =
                   SUM-DAILY-PB / SUM-DAILY-NO.

           IF SUM-DAILY-NB NOT = 0
              COMPUTE DAILY-NB-AVG ROUNDED =
                   SUM-DAILY-NB-AMT / SUM-DAILY-NB.

           IF SUM-DAILY-FB NOT = 0
              COMPUTE DAILY-FB-AVG ROUNDED =
                   SUM-DAILY-FB-AMT / SUM-DAILY-FB.

           IF SUM-DAILY-PB NOT = 0
              COMPUTE DAILY-PB-AVG ROUNDED =
                   SUM-DAILY-PB-AMT / SUM-DAILY-PB.

      **********************************************
       SET-INCREASE-REMAIN SECTION.
      **********************************************
           MOVE " REMAINING" TO DAILY-INC-REMAIN-P.
           MOVE "012:S" TO DAILY-LIST3-NFR60-T
                           DAILY-LIST3-NFR60-B.
           COMPUTE VDUNUM0 ROUNDED =
              SUM-DAILY-OUTBAL(2 1)
                  - (SUM-REL-DAILY-OUTBAL(3 1) - SUM-DAILY-OUTBAL(3 1))
                   + SUM-DAILY-PL-AMT.
           MOVE VDUNUM0 TO DAILY-INC-REMAIN.

           IF VDUNUM0 > 0
              MOVE "022:RF" TO DAILY-LIST3-NFR90-T
                               DAILY-LIST3-NFR90-B
           ELSE
              MOVE "022" TO DAILY-LIST3-NFR90-T
                            DAILY-LIST3-NFR90-B.

      **********************************************
       SETUP3-DAILY-DELINQ-STATS SECTION.
      **********************************************
           IF DAILY-TOP AND AGESUB > 3
              GO TO SETUP3-DAILY-DELINQ-STATS-EXIT.

      * ADD REL:
           ADD SUM-REL-DAILY-UNITS(AGESUB RECCON)
                                  TO TOT-REL-DAILY-UNITS(1 RECCON).
           COMPUTE UNITS-PER ROUNDED =
                     SUM-REL-DAILY-UNITS(AGESUB RECCON)
                                          / SUM-REL-DAILY-NO * 100.
           ADD UNITS-PER TO TOT-REL-DAILY-P-UNITS(1 RECCON).
           ADD SUM-REL-DAILY-OUTBAL(AGESUB RECCON)
                                  TO TOT-REL-DAILY-OUTBAL(1 RECCON).
           COMPUTE OUTBAL-PER ROUNDED =
                     SUM-REL-DAILY-OUTBAL(AGESUB RECCON)
                                         / SUM-REL-DAILY-AMT * 100.
           ADD OUTBAL-PER TO TOT-REL-DAILY-P-OUTBAL(1 RECCON).

      * MOVE REL:
           IF NOT (DAILY-BOTTOM AND (AGESUB = 1 OR 2 OR 3))
              MOVE SUM-REL-DAILY-UNITS(AGESUB RECCON)
                                   TO DAILY-UNITS(SUBX RECCON)
              MOVE UNITS-PER TO DAILY-P-UNITS(SUBX RECCON)

              ADD 0.50 SUM-REL-DAILY-OUTBAL(AGESUB RECCON)
                                 GIVING DAILY-OUTBAL(SUBX RECCON)
              MOVE OUTBAL-PER TO DAILY-P-OUTBAL(SUBX RECCON)
              MOVE SUBX TO SUBY
              PERFORM SETUP3-DAILY-SIGN
              ADD 1 TO SUBX.
      * ADD:
           ADD SUM-DAILY-UNITS(AGESUB RECCON)
                                  TO TOT-DAILY-UNITS(1 RECCON).
           COMPUTE UNITS-PER ROUNDED =
               SUM-DAILY-UNITS(AGESUB RECCON)
                          / SUM-DAILY-NO * 100.
           ADD UNITS-PER TO TOT-DAILY-P-UNITS(1 RECCON).
           ADD SUM-DAILY-OUTBAL(AGESUB RECCON)
                                  TO TOT-DAILY-OUTBAL(1 RECCON).
           COMPUTE OUTBAL-PER ROUNDED =
               SUM-DAILY-OUTBAL(AGESUB RECCON)
                          / SUM-DAILY-AMT * 100.
           ADD OUTBAL-PER TO TOT-DAILY-P-OUTBAL(1 RECCON).

      * MOVE:
           IF NOT (DAILY-BOTTOM AND (AGESUB = 1 OR 2 OR 3))
              MOVE SUM-DAILY-UNITS(AGESUB RECCON)
                                   TO DAILY-UNITS(SUBX RECCON)
              MOVE UNITS-PER TO DAILY-P-UNITS(SUBX RECCON)
              ADD 0.50 SUM-DAILY-OUTBAL(AGESUB RECCON)
                                 GIVING DAILY-OUTBAL(SUBX RECCON)
              MOVE OUTBAL-PER TO DAILY-P-OUTBAL(SUBX RECCON).

           IF (AGESUB = 3 AND DAILY-TOP)
                   OR (AGESUB = 5 AND DAILY-BOTTOM)
              ADD 2 SUBX GIVING SUBY
           ELSE
              IF (AGESUB = 3 AND DAILY-BOTTOM)
                 MOVE SUBX TO SUBY.

           IF (AGESUB = 3)
                   OR (AGESUB = 5 AND DAILY-BOTTOM)
              MOVE TOT-REL-DAILY-UNITS(1 RECCON)
                               TO DAILY-T-UNITS(SUBY RECCON)
              MOVE TOT-REL-DAILY-P-UNITS(1 RECCON)
                               TO DAILY-P-UNITS(SUBY RECCON)
              ADD 0.50 TOT-REL-DAILY-OUTBAL(1 RECCON)
                              GIVING DAILY-T-OUTBAL(SUBY RECCON)
              MOVE TOT-REL-DAILY-P-OUTBAL(1 RECCON)
                               TO DAILY-P-OUTBAL(SUBY RECCON)
              PERFORM SETUP3-DAILY-SIGN

              ADD 1 TO SUBY
              MOVE TOT-DAILY-UNITS(1 RECCON)
                               TO DAILY-T-UNITS(SUBY RECCON)
              MOVE TOT-DAILY-P-UNITS(1 RECCON)
                               TO DAILY-P-UNITS(SUBY RECCON)
              ADD 0.50 TOT-DAILY-OUTBAL(1 RECCON)
                                GIVING DAILY-T-OUTBAL(SUBY RECCON)
              MOVE TOT-DAILY-P-OUTBAL(1 RECCON)
                               TO DAILY-P-OUTBAL(SUBY RECCON)
              IF RECCON = 2
                 ADD 2 TO SUBX.

           IF NOT (DAILY-BOTTOM AND (AGESUB = 1 OR 2 OR 3))
              IF RECCON = 2
                 ADD 2 TO SUBX
              ELSE
                 SUBTRACT 1 FROM SUBX
           ELSE
              IF (DAILY-BOTTOM AND AGESUB = 3)
                 IF RECCON = 2
                    ADD 1 TO SUBX.

       SETUP3-DAILY-DELINQ-STATS-EXIT.
           EXIT.

      **********************************************
       SETUP3-SIGN SECTION.
      **********************************************
           IF DEL-UNITS-SIGN(SUBY RECCON) = " "
              MOVE "*" TO DEL-UNITS-SIGN(SUBY RECCON).
           IF DEL-OUTBAL-SIGN(SUBY RECCON) = " "
              MOVE "*" TO DEL-OUTBAL-SIGN(SUBY RECCON).

      **********************************************
       SETUP3-SIGN-PER SECTION.
      **********************************************
           IF DEL-UNITS-SIGN(SUBY RECCON) = " "
              MOVE "%" TO DEL-UNITS-SIGN(SUBY RECCON).
           IF DEL-OUTBAL-SIGN(SUBY RECCON) = " "
              MOVE "%" TO DEL-OUTBAL-SIGN(SUBY RECCON).

      **********************************************
       SETUP3-DAILY-SIGN SECTION.
      **********************************************
           IF DAILY-UNITS-X(SUBY RECCON) NOT = " "
              IF DAILY-UNITS-SIGN(SUBY RECCON) = " "
                 MOVE "*" TO DAILY-UNITS-SIGN(SUBY RECCON).
           IF DAILY-P-UNITS-X(SUBY RECCON) NOT = " "
              IF DAILY-P-UNITS-SIGN(SUBY RECCON) = " "
                 MOVE "%" TO DAILY-P-UNITS-SIGN(SUBY RECCON).
           IF DAILY-OUTBAL-X(SUBY RECCON) NOT = " "
              IF DAILY-OUTBAL-SIGN(SUBY RECCON) = " "
                 MOVE "*" TO DAILY-OUTBAL-SIGN(SUBY RECCON).
           IF DAILY-P-OUTBAL-X(SUBY RECCON) NOT = " "
              IF DAILY-P-OUTBAL-SIGN(SUBY RECCON) = " "
                 MOVE "%" TO DAILY-P-OUTBAL-SIGN(SUBY RECCON).

      ***********************************
       NEW-DEL-DELINQ SECTION.
      ***********************************
           MOVE DELINQ TO DELTYP-COD.
           IF DELINQ NOT = 5
              MOVE DEL-TYPE(DELINQ) TO DEL-BUFFER
           ELSE
              IF DIFF-PER = 0
                 MOVE DEL-TYPE(5) TO DEL-BUFFER
              ELSE
                 MOVE DEL-TYPE(6) TO DEL-BUFFER.
           MOVE DEL-BUFFER TO WR-BUF.
           MOVE DEL-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE " " TO MESS.
           PERFORM CALL-TMMSG.

           IF DELINQ = 1 OR 2 OR 3 OR 4 OR 5
              MOVE PROMPT-12345 TO DEL-PROMPT1
              MOVE " 30:" TO DEL-LN-PROMPT(1)
              MOVE " 60:" TO DEL-LN-PROMPT(2)
              MOVE " 90:" TO DEL-LN-PROMPT(3)
              MOVE "120:" TO DEL-LN-PROMPT(4)
              MOVE "150:" TO DEL-LN-PROMPT(5)
              MOVE "180:" TO DEL-LN-PROMPT(6)
              MOVE "210:" TO DEL-LN-PROMPT(7)
              MOVE "DEL:" TO DEL-LN-PROMPT(8)
              MOVE "<M>:" TO DEL-LN-PROMPT(9)
              MOVE "<P>:" TO DEL-LN-PROMPT(10)
              MOVE "SLOW" TO DEL-LN-PROMPT(11)
              MOVE "CUR:" TO DEL-LN-PROMPT(12)
              MOVE "TOT:" TO DEL-LN-PROMPT(13)
              GO TO NEW-DEL-DELINQ-EXIT.

           IF DELINQ = 7
              MOVE " " TO DAILY-BUF3
              MOVE DAILY-PROMPTS-NO(1) TO DAILY-PROMPT1
              MOVE DAILY-PROMPTS-NO(2) TO DAILY-PROMPT2

              MOVE DAILY-PROMPTS-NO(3) TO DAILY-PROMPT3
              MOVE DAILY-PROMPTS-NO(4) TO DAILY-PROMPT4

              MOVE DAILY-PROMPTS-NO(5) TO DAILY-PROMPT5
              IF DAILY-TOP
                 MOVE "30 :" TO DAILY-LN-PROMPT(1)
                 MOVE REM TO DAILY-LN-PROMPT(2)
                 MOVE "60 :" TO DAILY-LN-PROMPT(4)
                 MOVE REM TO DAILY-LN-PROMPT(5)
                 MOVE "90+:" TO DAILY-LN-PROMPT(7)
                 MOVE REM TO DAILY-LN-PROMPT(8)
                 MOVE "DEL:" TO DAILY-LN-PROMPT(10)
                 MOVE REM TO DAILY-LN-PROMPT(11)
                 GO TO NEW-DEL-DELINQ-EXIT
              ELSE
                 MOVE "DEL:" TO DAILY-LN-PROMPT(1)
                 MOVE REM TO DAILY-LN-PROMPT(2)
                 MOVE "<M>:" TO DAILY-LN-PROMPT(4)
                 MOVE REM TO DAILY-LN-PROMPT(5)
                 MOVE "<P>:" TO DAILY-LN-PROMPT(7)
                 MOVE REM TO DAILY-LN-PROMPT(8)
                 MOVE "SLOW" TO DAILY-LN-PROMPT(10)
                 MOVE REM TO DAILY-LN-PROMPT(11)
                 GO TO NEW-DEL-DELINQ-EXIT.

       NEW-DEL-DELINQ-EXIT.
           EXIT.

      ****************************************************
       DISPLAY-FIELDS-4 SECTION.
      ****************************************************
           MOVE ACT4-TYPE(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.
           IF PAGE4-TODAY
              IF PAGE4-REC OR PAGE4-NO-O2
                 MOVE ACT4-TYPET(ACTIVITY) TO ACT-BUFFER
              ELSE
                 MOVE O2-INT-TYPE4T(ACTIVITY) TO ACT-BUFFER
              END-IF
           ELSE
              IF PAGE4-REC OR PAGE4-NO-O2
                 MOVE ACT4-TYPEL(ACTIVITY) TO ACT-BUFFER
              ELSE
                 MOVE O2-INT-TYPE4L(ACTIVITY) TO ACT-BUFFER.

           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAMEL TO WR-LIST.
           PERFORM CALL-WRFORM.
      * DISPLAY O2 INFO
           MOVE O2-ACCT(PAGE4-O2-TYPE) TO O2-BUFFER.
           MOVE O2-BUFFER TO WR-BUF.
           MOVE O2-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE O2-REC(PAGE4-O2-TYPE) TO O2-BUFFER.
           MOVE O2-BUFFER TO WR-BUF.
           MOVE O2-FNAMEL TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM MOVE-4-TOTALS.
           MOVE DISPLAY-BUF4 TO WR-BUF.
           MOVE DISPLAY-LIST4 TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE " " TO MESS.
           PERFORM CALL-TMMSG.

       DISPLAY-FIELDS-EXIT-4.
           EXIT.

      ****************************************************
       MOVE-4-TOTALS SECTION.
      ****************************************************

           PERFORM VARYING REPOSUB FROM 1 BY 1 UNTIL REPOSUB > 5
                      AFTER PERIOD FROM 1 BY 1 UNTIL PERIOD > 3
              IF (REPOSUB = 2 AND PERIOD = 1 AND NET-GAIN-LOSS)
                                         AND (NOT (PAGE4-TODAY))
                 MOVE 0 TO DATA-NO-REPO(REPOSUB PERIOD)
                           DATA-AMT-REPO(REPOSUB PERIOD)
              ELSE
                 MOVE SUM-NO-REPO(REPOSUB PERIOD)
                                    TO DATA-NO-REPO(REPOSUB PERIOD)
                 MOVE SUM-AMT-REPO(REPOSUB PERIOD)
                                    TO DATA-AMT-REPO(REPOSUB PERIOD)
              END-IF
           END-PERFORM.

      * COMPUTE WEIGHTED AVERAGE TERM
           MOVE 0 TO VDUNUM2.
      * COMPUTE WEIGHTED AVERAGE LOAN AMOUNT
           MOVE 0 TO VDUNUM2.

       EXIT-MOVE-4.
           EXIT.

      ****************************************************
       DISPLAY-FIELDS-5 SECTION.
      ****************************************************

      *    PERFORM NEW-PAGE5-PERIOD-DISPLAY.
      *    PERFORM MOVE-5-TOTALS.

      *    MOVE DISPLAY-BUF5 TO WR-BUF.
      *    MOVE DISPLAY-LIST4 TO WR-LIST.
      *    PERFORM CALL-WRFORM.

      *    MOVE " " TO MESS.
      *    PERFORM CALL-TMMSG.

       DISPLAY-FIELDS-EXIT-5.
           EXIT.

      ******************************************************************
      *
      *    M O V E - 5 - T O T A L S --- PAGE 5
      *
      ******************************************************************
      *MOVE-5-TOTALS SECTION.

      *    PERFORM VARYING TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 3
      *        MOVE SUM-NO-PENDING(TYPSUB 1)
      *                      TO DATA-GNLS-PENDING(TYPSUB)
      *        MOVE SUM-NO-COND-APP(TYPSUB 1)
      *                      TO DATA-GNLS-COND-APP(TYPSUB)
      *        MOVE SUM-NO-APPROVED(TYPSUB 1)
      *                      TO DATA-GNLS-APPROVED(TYPSUB)

      *        MOVE SUM-NO-PENDING(TYPSUB 2)
      *                      TO DATA-NVOL-PENDING(TYPSUB)
      *        MOVE SUM-NO-COND-APP(TYPSUB 2)
      *                      TO DATA-NVOL-COND-APP(TYPSUB)
      *        MOVE SUM-NO-APPROVED(TYPSUB 2)
      *                      TO DATA-NVOL-APPROVED(TYPSUB)

      *        MOVE 0 TO DATA-PERC-APPR(TYPSUB)
      *        IF SUM-NO-APPROVED(TYPSUB 2) NOT = 0
      *           COMPUTE PERCENT-WK ROUNDED =
      *              SUM-NO-APPROVED(TYPSUB 2) /
      *              SUM-NO-PENDING(TYPSUB 2) * 100
      *           MOVE PERCENT-WK TO DATA-PERC-APPR(TYPSUB)
      *           IF DATA-PSIGN1(TYPSUB) = " "
      *              MOVE "%" TO DATA-PSIGN1(TYPSUB)
      *           END-IF
      *        END-IF

      *        MOVE SUM-NO-MADE(TYPSUB 2)
      *                      TO DATA-NVOL-MADE(TYPSUB)

      *        MOVE 0 TO DATA-PERC-MADE(TYPSUB)
      *        IF SUM-NO-MADE(TYPSUB 2) NOT = 0
      *           COMPUTE PERCENT-WK ROUNDED =
      *              SUM-NO-MADE(TYPSUB 2) /
      *              SUM-NO-PENDING(TYPSUB 2) * 100
      *           MOVE PERCENT-WK TO DATA-PERC-MADE(TYPSUB)
      *           IF DATA-PSIGN2(TYPSUB) = " "
      *              MOVE "%" TO DATA-PSIGN2(TYPSUB)
      *           END-IF
      *        END-IF

      *        MOVE SUM-NO-TURNDOWN(TYPSUB 2)
      *                      TO DATA-NVOL-TURNDOWN(TYPSUB)
      *        MOVE SUM-NO-TUD(TYPSUB 2)
      *                      TO DATA-NVOL-TUD(TYPSUB)
      *    END-PERFORM.


      *SETUP-5TX.
      *    EXIT.

      *EXIT-MOVE-5.
      *    EXIT.

      *****************************************************************
       DISPLAY-FIELDS-6.
      *****************************************************************
           MOVE PAGE6-PROMPT(PAGE6-TYPE) TO WR-BUF.
           MOVE PAGE6-PMPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM VARYING PERIOD FROM 1 BY 1 UNTIL PERIOD > 3
                     AFTER ACTSUB FROM 1 BY 1 UNTIL ACTSUB > 2
                     AFTER TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 6
              MOVE SUM-UNEARNED-AMT (PERIOD ACTSUB TYPSUB) TO
                   DISP-UNEARNED-AMT(PERIOD ACTSUB TYPSUB)
           END-PERFORM.

           MOVE DISPLAY-BUF6 TO WR-BUF.
           MOVE DISPLAY-LIST6 TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE " " TO MESS.
           PERFORM CALL-TMMSG.

       DISPLAY-FIELDS-EXIT-6.
           EXIT.

      *********************************************************
       DEALER-SCAN SECTION.
      *********************************************************
           MOVE 0 TO DLSCAN-NUM.
           MOVE "WI/DLSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF DLSCAN-STATUS NOT = "00" OR DLSCAN-NUM = " "
               MOVE 9 TO IO-FG
               GO TO EXIT-DEALER-SCAN.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           MOVE 0 TO IO-FG.

       EXIT-DEALER-SCAN.
           EXIT.

      *********************************************************
       CLASS-SCAN SECTION.
      *********************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-CLASS-SCAN.

           MOVE "S  A030CLASS." TO VDU.
           MOVE LCSCAN-KEY      TO VDUBUF.
           PERFORM SCREENIO.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ******************************************************************
      * TEST CLASS IF SALES FINANCE OR LOAN CLASSES SELECTED
      *   NOTE - THIS ROUTINE NOT PERFORMED IF ONE-CLASS
      *   OTHERWISE, FLAG WILL MAKE US SKIP SERVICE ACCTS
      ******************************************************************
       TEST-CLASS SECTION.

           MOVE "N" TO CORRECT-CLASS.

           MOVE DS-CLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG = 9
              GO TO EXIT-TEST-CLASS.
           IF SF-CLASSES
              IF LC-SALEFIN = "Y"
                 IF LC-AFP-CLASS NOT = "Y"
                    MOVE "Y" TO CORRECT-CLASS.
           IF AFP-CLASSES
              IF LC-AFP-CLASS = "Y"
                 MOVE "Y" TO CORRECT-CLASS.
           IF LN-CLASSES
              IF LC-SALEFIN = "N"
                 MOVE "Y" TO CORRECT-CLASS.
           IF ALL-CLASSES
              MOVE "Y" TO CORRECT-CLASS.
           IF LC-SERVACCT = "Y"
              MOVE "N" TO CORRECT-CLASS.
       EXIT-TEST-CLASS.
           EXIT.

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "STATS" TO PASSWDW-PGM.
           MOVE SAVE-LEVEL TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "TO VIEW THESE BRANCHES:" TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      *****************************************************************
       ADD-DS-TO-HDS SECTION.
      *****************************************************************
           ADD  DS-DLPARVHELD    TO HDS-DLPARVHELD.
           ADD  DS-UNPAIDCK      TO HDS-UNPAIDCK.
           ADD  DS-DLDISC        TO HDS-DLDISC.
           ADD  DS-DLPARVPAID    TO HDS-DLPARVPAID.
           ADD  DS-SUMWGTTM      TO HDS-SUMWGTTM.
           ADD  DS-SUMWGTTA      TO HDS-SUMWGTTA.
           ADD  DS-SUMWGTAV      TO HDS-SUMWGTAV.
           ADD  DS-DELUNEARN-DATE      TO HDS-DELUNEDT.
           ADD  DS-AFPDUEDL-MEMO TO HDS-AFPDUEDL-MEMO.
           ADD  DS-PROCEEDS(1)   TO HDS-PROCEEDS(1).
           ADD  DS-PROCEEDS(2)   TO HDS-PROCEEDS(2).

           PERFORM ADD-DS-TO-HDS-MTD-YTD
                   VARYING DSSUB1 FROM 1 BY 1 UNTIL DSSUB1 > 3.

           PERFORM VARYING DSSUB1 FROM 1 BY 1 UNTIL DSSUB1 > 3
                     AFTER DSSUB2 FROM 1 BY 1 UNTIL DSSUB2 > 2
                     AFTER DSSUB3 FROM 1 BY 1 UNTIL DSSUB3 > 6
                 ADD DS-UNEARNED-AMT(DSSUB1 DSSUB2 DSSUB3) TO
                    HDS-UNEARNED-AMT(DSSUB1 DSSUB2 DSSUB3)
           END-PERFORM.

           PERFORM ADD-DS-TO-HDS-DELINQ-TABLE
                   VARYING DSSUB1 FROM 1 BY 1 UNTIL DSSUB1 > 3
                     AFTER DSSUB2 FROM 1 BY 1 UNTIL DSSUB2 > 2
                     AFTER DSSUB3 FROM 1 BY 1 UNTIL DSSUB3 > 10.

           PERFORM ADD-DS-TO-HDS-REPO-TABLE
                   VARYING DSSUB1 FROM 1 BY 1 UNTIL DSSUB1 > 3
                     AFTER DSSUB2 FROM 1 BY 1 UNTIL DSSUB2 > 2
                     AFTER DSSUB3 FROM 1 BY 1 UNTIL DSSUB3 > 5.

      *    PERFORM ADD-DS-TO-HDS-APP-TABLE
      *          VARYING DSSUB1 FROM 1 BY 1 UNTIL DSSUB1 > 3
      *             AFTER DSSUB2 FROM 1 BY 1 UNTIL DSSUB2 > 2
      *             AFTER DSSUB3 FROM 1 BY 1 UNTIL DSSUB3 > 2.

           GO TO EXIT-ADD-DS-TO-HDS.

      *-----------------------------------------------------------------
       ADD-DS-TO-HDS-MTD-YTD.
           ADD  DS-DLRVPRIN(DSSUB1)      TO HDS-DLRVPRIN(DSSUB1).
           ADD  DS-DLRVCHGS(DSSUB1)      TO HDS-DLRVCHGS(DSSUB1).
           ADD  DS-RCOURSE-UNITS(DSSUB1) TO HDS-RCOURSE-UNITS(DSSUB1).
           ADD  DS-RCOURSE-AMT  (DSSUB1) TO HDS-RCOURSE-AMT(DSSUB1).
           ADD  DS-NPURUNIT(DSSUB1)      TO HDS-NPURUNIT(DSSUB1).
           ADD  DS-NPURAMT (DSSUB1)      TO HDS-NPURAMT(DSSUB1).
           ADD  DS-OPURUNIT(DSSUB1)      TO HDS-OPURUNIT(DSSUB1).
           ADD  DS-OPURAMT (DSSUB1)      TO HDS-OPURAMT(DSSUB1).
           ADD  DS-RNEWUNIT(DSSUB1)      TO HDS-RNEWUNIT(DSSUB1).
           ADD  DS-RNEWAMT (DSSUB1)      TO HDS-RNEWAMT(DSSUB1).
           ADD  DS-PAIDUNIT(DSSUB1)      TO HDS-PAIDUNIT(DSSUB1).
           ADD  DS-PAIDAMT (DSSUB1)      TO HDS-PAIDAMT(DSSUB1).
           ADD  DS-TODLUNIT(DSSUB1)      TO HDS-TODLUNIT(DSSUB1).
           ADD  DS-TODLAMT (DSSUB1)      TO HDS-TODLAMT(DSSUB1).
           ADD  DS-OUTSUNIT(DSSUB1)      TO HDS-OUTSUNIT(DSSUB1).
           ADD  DS-OUTSAMT (DSSUB1)      TO HDS-OUTSAMT(DSSUB1).
           ADD  DS-PLOSUNIT(DSSUB1)      TO HDS-PLOSUNIT(DSSUB1).
           ADD  DS-PLOSAMT (DSSUB1)      TO HDS-PLOSAMT(DSSUB1).
           ADD  DS-RCPLUNIT(DSSUB1)      TO HDS-RCPLUNIT(DSSUB1).
           ADD  DS-RCPLAMT (DSSUB1)      TO HDS-RCPLAMT(DSSUB1).
           ADD  DS-JDOSUNIT(DSSUB1)      TO HDS-JDOSUNIT(DSSUB1).
           ADD  DS-JDOSAMT (DSSUB1)      TO HDS-JDOSAMT(DSSUB1).
           ADD  DS-DLDISC-VOL(DSSUB1)    TO HDS-DLDISC-VOL(DSSUB1).

      *-----------------------------------------------------------------
       ADD-DS-TO-HDS-REPO-TABLE.
           ADD DS-NO-REPO(DSSUB1 DSSUB2 DSSUB3) TO
                           HDS-NO-REPO(DSSUB1 DSSUB2 DSSUB3).
           ADD DS-AMT-REPO(DSSUB1 DSSUB2 DSSUB3) TO
                              HDS-AMT-REPO(DSSUB1 DSSUB2 DSSUB3).

      *-----------------------------------------------------------------
       ADD-DS-TO-HDS-DELINQ-TABLE.
           ADD DS-UNITS(DSSUB1 DSSUB2 DSSUB3) TO
                              HDS-UNITS(DSSUB1 DSSUB2 DSSUB3).
           ADD DS-OUTBAL(DSSUB1 DSSUB2 DSSUB3) TO
                              HDS-OUTBAL(DSSUB1 DSSUB2 DSSUB3).
           ADD DS-UNITS-BNKRPT(DSSUB1 DSSUB2 DSSUB3) TO
                              HDS-UNITS-BNKRPT(DSSUB1 DSSUB2 DSSUB3).
           ADD DS-OUTBAL-BNKRPT(DSSUB1 DSSUB2 DSSUB3) TO
                              HDS-OUTBAL-BNKRPT(DSSUB1 DSSUB2 DSSUB3).

      *-----------------------------------------------------------------
      *ADD-DS-TO-HDS-APP-TABLE.
      *    ADD DS-NO-PENDING(DSSUB1 DSSUB2 DSSUB3) TO
      *                       HDS-NO-PENDING(DSSUB1 DSSUB2 DSSUB3).
      *    ADD DS-NO-COND-APP(DSSUB1 DSSUB2 DSSUB3) TO
      *                       HDS-NO-COND-APP(DSSUB1 DSSUB2 DSSUB3).
      *    ADD DS-NO-APPROVED(DSSUB1 DSSUB2 DSSUB3) TO
      *                       HDS-NO-APPROVED(DSSUB1 DSSUB2 DSSUB3).
      *    ADD DS-NO-MADE(DSSUB1 DSSUB2 DSSUB3) TO
      *                       HDS-NO-MADE(DSSUB1 DSSUB2 DSSUB3).
      *    ADD DS-NO-TURNDOWN(DSSUB1 DSSUB2 DSSUB3) TO
      *                       HDS-NO-TURNDOWN(DSSUB1 DSSUB2 DSSUB3).
      *    ADD DS-NO-TURNUSDOWN(DSSUB1 DSSUB2 DSSUB3) TO
      *                       HDS-NO-TURNUSDOWN(DSSUB1 DSSUB2 DSSUB3).
       EXIT-ADD-DS-TO-HDS.
              EXIT.

      ***********************************************************************
       CLEAR-HDS-FILE SECTION.
      ***********************************************************************
           MOVE " " TO HDS-REC.
           MOVE 0 TO HDS-DLPARVHELD HDS-UNPAIDCK HDS-DLDISC
                     HDS-DLPARVPAID
                     HDS-SUMWGTTM HDS-SUMWGTAV HDS-SUMWGTTA
                     HDS-AFPDUEDL-MEMO HDS-DELUNEDT
                     HDS-PROCEEDS(1) HDS-PROCEEDS(2).

      * MTD-YTD-LYR BUCKETS:
           PERFORM CLEAR-P2-TABLE VARYING
              DSSUB1 FROM 1 BY 1 UNTIL DSSUB1 > 3.

      * HDS-DEL-BUCKETS:
           PERFORM CLEAR-HDS-DEL-BUCKETS VARYING
               DSSUB3 FROM 1 BY 1 UNTIL DSSUB3 > 10
                  AFTER DSSUB2 FROM 1 BY 1 UNTIL DSSUB2 > 2
                    AFTER DSSUB1 FROM 1 BY 1 UNTIL DSSUB1 > 3.

      * HDS-REPO-ACTIVITY-TBL:
           PERFORM CLEAR-HDS-REPO-ACTIVITY-TBL VARYING
               DSSUB3 FROM 1 BY 1 UNTIL DSSUB3 > 5
                  AFTER DSSUB2 FROM 1 BY 1 UNTIL DSSUB2 > 2
                    AFTER DSSUB1 FROM 1 BY 1 UNTIL DSSUB1 > 3.


      * HDS UNEARNED BUCKETS:
           PERFORM VARYING DSSUB1 FROM 1 BY 1 UNTIL DSSUB1 > 3
                     AFTER DSSUB2 FROM 1 BY 1 UNTIL DSSUB2 > 2
                     AFTER DSSUB3 FROM 1 BY 1 UNTIL DSSUB3 > 6
              MOVE 0 TO HDS-UNEARNED-AMT(DSSUB1 DSSUB2 DSSUB3)
           END-PERFORM.
           GO TO EXIT-CLEAR-HDS-FILE.

       CLEAR-P2-TABLE.
           MOVE 0 TO HDS-DLRVPRIN(DSSUB1)      HDS-DLRVCHGS(DSSUB1)
                     HDS-RCOURSE-UNITS(DSSUB1) HDS-RCOURSE-AMT(DSSUB1)
                     HDS-NPURUNIT(DSSUB1)      HDS-NPURAMT(DSSUB1)
                     HDS-OPURUNIT(DSSUB1)      HDS-OPURAMT(DSSUB1)
                     HDS-RNEWUNIT(DSSUB1)      HDS-RNEWAMT(DSSUB1)
                     HDS-PAIDUNIT(DSSUB1)      HDS-PAIDAMT(DSSUB1)
                     HDS-TODLUNIT(DSSUB1)      HDS-TODLAMT(DSSUB1)
                     HDS-OUTSUNIT(DSSUB1)      HDS-OUTSAMT(DSSUB1)
                     HDS-PLOSUNIT(DSSUB1)      HDS-PLOSAMT(DSSUB1)
                     HDS-RCPLUNIT(DSSUB1)      HDS-RCPLAMT(DSSUB1)
                     HDS-JDOSUNIT(DSSUB1)      HDS-JDOSAMT(DSSUB1)
                     HDS-DLDISC-VOL(DSSUB1).

       CLEAR-HDS-DEL-BUCKETS.
           MOVE 0 TO HDS-UNITS(DSSUB1 DSSUB2 DSSUB3)
                     HDS-OUTBAL(DSSUB1 DSSUB2 DSSUB3).
           MOVE 0 TO HDS-UNITS-BNKRPT(DSSUB1 DSSUB2 DSSUB3)
                     HDS-OUTBAL-BNKRPT(DSSUB1 DSSUB2 DSSUB3).

       CLEAR-HDS-REPO-ACTIVITY-TBL.
           MOVE 0 TO HDS-NO-REPO(DSSUB1 DSSUB2 DSSUB3)
                     HDS-AMT-REPO(DSSUB1 DSSUB2 DSSUB3).


       EXIT-CLEAR-HDS-FILE.
           EXIT.

      ***********************************************************************
       CLEAR-HDY-FILE SECTION.
      ***********************************************************************
           MOVE SPACES TO   HDY-REC.
           MOVE 0 TO HDY-DLRVPRIN(1) HDY-DLRVPRIN(2) HDY-DLRVPRIN(3)
                     HDY-DLRVCHGS(1) HDY-DLRVCHGS(2) HDY-DLRVCHGS(3)
                     HDY-DLPARVHELD HDY-UNPAIDCK HDY-DLDISC
                     HDY-DLPARVPAID HDY-SUMWGTTM HDY-SUMWGTAV
                     HDY-SUMWGTTA

                     HDY-RCOURSE-UNITS(1) HDY-RCOURSE-UNITS(2)
                     HDY-RCOURSE-UNITS(3)
                     HDY-RCOURSE-AMT(1) HDY-RCOURSE-AMT(2)
                     HDY-RCOURSE-AMT(3)

                     HDY-NPURUNIT HDY-NPURAMT HDY-OPURUNIT HDY-OPURAMT
                     HDY-RNEWUNIT HDY-RNEWAMT HDY-PAIDUNIT HDY-PAIDAMT
                     HDY-TOLNUNIT HDY-TOLNAMT HDY-OUTSUNIT HDY-OUTSAMT
                     HDY-PLOSUNIT HDY-PLOSAMT HDY-RCPLUNIT HDY-RCPLAMT
                     HDY-JDOSUNIT HDY-JDOSAMT HDY-DELUNEDT
                     HDY-AFPDUEDL-MEMO HDY-DLDISC-VOL HDY-PROCEEDS.

           PERFORM VARYING DSSUB1 FROM 1 BY 1 UNTIL DSSUB1 > 2
                     AFTER DSSUB2 FROM 1 BY 1 UNTIL DSSUB2 > 10
               MOVE 0 TO HDY-UNITS(DSSUB1 DSSUB2)
                         HDY-OUTBAL(DSSUB1 DSSUB2)
                         HDY-UNITS-BNKRPT(DSSUB1 DSSUB2)
                         HDY-OUTBAL-BNKRPT(DSSUB1 DSSUB2)
               IF DSSUB2 < 7
                  MOVE 0 TO HDY-UNEARNED-AMT(DSSUB1 DSSUB2)
               END-IF
               IF DSSUB2 < 6
                  MOVE 0 TO HDY-NO-REPO(DSSUB1 DSSUB2)
                            HDY-AMT-REPO(DSSUB1 DSSUB2)
               END-IF
           END-PERFORM.

       EXIT-CLEAR-HDY-FILE.
           EXIT.


      *********************************************
       FORM-DESTROY SECTION.
      *********************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN  
           EVALUATE PAGE-FLAG
              WHEN  1 DESTROY DSINQ1-SCREEN
              WHEN  2 DESTROY DSINQ2-SCREEN
              WHEN  3 DESTROY DSINQ3-SCREEN
              WHEN  4 DESTROY DSINQ4-SCREEN
              WHEN  5 DESTROY DSINQ5-SCREEN
              WHEN  6 DESTROY DSINQ6-SCREEN
           END-EVALUATE.

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           EVALUATE PAGE-FLAG
              WHEN 1 DISPLAY DSINQ1-SCREEN
                     MOVE EXT-CONAME-SYSDATE TO DSINQ1-SYSDATE
                     MOVE EXT-CONAME-CONAME TO DSINQ1-CONAME
                     MOVE DSINQ1-FORM-FIELDS TO WR-BUF
                     MOVE DSINQ1-FORM-LIST   TO WR-LIST
              WHEN 2 DISPLAY DSINQ2-SCREEN
                     MOVE EXT-CONAME-SYSDATE TO DSINQ2-SYSDATE
                     MOVE EXT-CONAME-CONAME TO DSINQ2-CONAME
                     MOVE DSINQ2-FORM-FIELDS TO WR-BUF
                     MOVE DSINQ2-FORM-LIST   TO WR-LIST
              WHEN 3 DISPLAY DSINQ3-SCREEN
                     MOVE EXT-CONAME-SYSDATE TO DSINQ3-SYSDATE
                     MOVE DSINQ3-FORM-FIELDS TO WR-BUF
                     MOVE DSINQ3-FORM-LIST   TO WR-LIST
              WHEN 4 DISPLAY DSINQ4-SCREEN
                     MOVE EXT-CONAME-SYSDATE TO DSINQ4-SYSDATE
                     MOVE DSINQ4-FORM-FIELDS TO WR-BUF
                     MOVE DSINQ4-FORM-LIST   TO WR-LIST
              WHEN 5 DISPLAY DSINQ5-SCREEN
                     MOVE EXT-CONAME-SYSDATE TO DSINQ5-SYSDATE
                     MOVE DSINQ5-FORM-FIELDS TO WR-BUF
                     MOVE DSINQ5-FORM-LIST   TO WR-LIST
              WHEN 6 DISPLAY DSINQ6-SCREEN
                     MOVE EXT-CONAME-SYSDATE TO DSINQ6-SYSDATE
                     MOVE DSINQ6-FORM-FIELDS TO WR-BUF
                     MOVE DSINQ6-FORM-LIST   TO WR-LIST
           END-EVALUATE.
           
           PERFORM CALL-WRFORM.
           
       FORM-RESET-EXIT.
           EXIT.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           EVALUATE PAGE-FLAG
                WHEN  1 MOVE DSINQ1-HELP--FILE TO HELP-LINK-FILE
                        MOVE DSINQ1-HELP--DIR  TO HELP-LINK-DIR
                WHEN  2 MOVE DSINQ2-HELP--FILE TO HELP-LINK-FILE
                        MOVE DSINQ2-HELP--DIR  TO HELP-LINK-DIR
                WHEN  3 MOVE DSINQ3-HELP--FILE TO HELP-LINK-FILE
                        MOVE DSINQ3-HELP--DIR  TO HELP-LINK-DIR
                WHEN  4 MOVE DSINQ4-HELP--FILE TO HELP-LINK-FILE
                        MOVE DSINQ4-HELP--DIR  TO HELP-LINK-DIR
                WHEN  5 MOVE DSINQ5-HELP--FILE TO HELP-LINK-FILE
                        MOVE DSINQ5-HELP--DIR  TO HELP-LINK-DIR
                WHEN  6 MOVE DSINQ6-HELP--FILE TO HELP-LINK-FILE
                        MOVE DSINQ6-HELP--DIR  TO HELP-LINK-DIR
           END-EVALUATE.


      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/DSINQ1_EVA.CPY".
              COPY "LIBLP/DSINQ2_EVA.CPY".
              COPY "LIBLP/DSINQ3_EVA.CPY".
              COPY "LIBLP/DSINQ4_EVA.CPY".
              COPY "LIBLP/DSINQ5_EVA.CPY".
              COPY "LIBLP/DSINQ6_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ******************************************************************
       MISC SECTION.
      ******************************************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       GET-DL.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.

      *-----------------------------------------------------------------
       NEW-PAGE5-SETUP.
           PERFORM NEW-PAGE5-PERIOD-SETUP.
           PERFORM NEW-PAGE5-PERIOD-DISPLAY.

      *-----------------------------------------------------------------
       NEW-PAGE5-PERIOD-SETUP.
      *   TOGGLE GAIN/LOSS:
           IF ( F4-KEY )
              IF PAGE5-GNLS = 3
                 MOVE 1 TO PAGE5-GNLS
              ELSE
                 ADD 1 TO PAGE5-GNLS.

      *   TOGGLE NEW VOLUME:
           IF ( F5-KEY )
              IF PAGE5-NVOL = 3
                 MOVE 1 TO PAGE5-NVOL
              ELSE
                 ADD 1 TO PAGE5-NVOL.

      *-----------------------------------------------------------------
       NEW-PAGE5-PERIOD-DISPLAY.

           IF PAGE5-TODAY
              MOVE "     *** TODAY ***    " TO PER-BUF-GNLS
                                              PER-BUF-NVOL
           ELSE
              COMPUTE SUB = PAGE5-GNLS * 2 - 1
              MOVE PAGE5-TYPE-DESC(SUB)           TO PER-BUF-GNLS
              COMPUTE SUB = PAGE5-NVOL * 2
              MOVE PAGE5-TYPE-DESC(SUB)           TO PER-BUF-NVOL.
           MOVE PAGE5-BUFFER TO WR-BUF.
           MOVE PAGE5-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       GET-TODAYS-DATE.
           MOVE TODAY-MM TO STORE-MO DISPLAY-MO.
           MOVE TODAY-YY TO STORE-YR.
           IF STORE-YR < EXT-JULIAN-CC
              ADD 100 TO STORE-YR.
           MOVE STORE-YR TO DISPLAY-YR.
           MOVE DISPLAY-DATE TO WR-BUF.
           MOVE DATE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-BR-NAME.
           MOVE "S  A000BRDESC." TO VDU.
           IF TRAP-NO-DY-FG = "Y"
              MOVE "TODAY TOTALS SKIPPED" TO VDUBUF
           ELSE
              IF TRAP-A-DATE-DY-FG = "Y"
                 MOVE DY-TRAP-MSG TO VDUBUF
              ELSE
                 IF GROUP-FLAG = "Y"
                    MOVE HOLD-GR-NAME TO VDUBUF
                 ELSE
                    MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPDS1RN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBRC2RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ***********************************************************************
       END-ROUTINE SECTION.
      ***********************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPIQMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           PERFORM FORM-DESTROY.
           EXIT PROGRAM.
