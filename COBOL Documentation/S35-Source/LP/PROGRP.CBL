       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PROGRP.
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  PROGRESS REPORT
      *
      * REV:
      *  MJD 080295 ADDED PASSWORD PROTECTION TO BRANCH RANGE.
      *  BLL 080895 30 DAYS POTS NOT PRINTING
      *  BLL 090295 CHANGE BEG-MONTH OPTION TO PRINT BEG MO DELINQUENCY
      *             ON LINE 1, THEN BEG OF LAST MONTH ON LINE 2
      *  MJD 091595 ADDED DS-DATE LOGIC AND ADDED CUR-MO-DS-DATE.
      *  BLL 093095 ADDED LOCAL SPOOL OPTION, MILITARY TIME
      *  JFF 100695 ADDED CLASS SCAN
      *  JTG 103095 TEMPORARY FIX FOR 'NFS' NOT WORKING
      *  BLL 111095 PRINT GLOBAL NAME BY BR #, FIX NO RECS FOUND
      *  JFF 111495 CHANGED FORM TO INCLUDE LOCAL-SPOOLING QUESTION.
      *  JTG 120195 STOPPED PRINTING OF TODATE REDEEMED (NO & $)
      *  JFF 121395 CHANGED "IF LOCAL-PRINT-FG = N" TO
      *             "IF LOCAL PRINT-FG NOT = Y" IN INITIALIZATION.
      *  JTG 122995 CORRECTED CHANGE MADE 120195, PRINTING OF TODATE
      *             REDEEMED (NO & $)
      *  BLL 012296 PROGRAM WAS ADDING YTD PRINCIPAL COLLECTED & USING IT
      *             TO COMPUTE % CASH COL MTD; CHANGED TO ADD MTD PRINCOL
      *  BLL 012596 ADD NEW P4/P5 & RECOVERY STUFF
      *  BLL 020996 COMP FIELDS IN TOTAL-TABLE; INITIALIZE W/LOW-VALUES
      *  BLL 021296 ADD P4P5 SECTION, DLRV & AVG LOSS STUFF
      *  BLL 021596 ADD INC O2 GAIN/LOSS & VOL OPTIONS
      *  BLL 022096 ADD MTD,YTD REPO FREQUENCY PERCENTAGES
      *  BLL 022996 TAKE OUT INC O2 QUESTIONS, DEFAULT PRE MERCURY
      *             TO INC O2 IN P&L & REPO VOLUME
      *  BLL 040196 PER JTG - DON'T ADD PLI TO CHARGE-OFF VOLUME TOTALS
      *  KEC 040296 ADDED 'GROUP TOTALS' WINDOW LOGIC.
      *  BLV 010797 TEMPORARY FIX FOR MERC; DON'T PRINT REPO SOLD LINE
      *  BLV 031097 PRINT REPO REDEEMED & SOLD LINES UNLESS GP-PROGRP-
      *             PRINT-SOLD = N
      *  BLV 050697 FLAG DIRECT-LN-CLASS-FG - IF NO DIRECT CLASSES, SKIP
      *             PRINT DIRECT DELINQUENCY (KEEP RELIANCE TO 2 PAGES)
      *   CS 051297 CHANGED THE MEANING OF GP-PROGRP-PRINT-SOLD = N TO
      *  BLV 062797 CHANGED FOR NEW LOCAL SPOOL (BREXEC DRIVER)
      *  MJD 070297 ADDED DLDISC VOLUME AND % TO SF VOLUME
      *  MJD 071197 ADDED GP-PROGRP-DL-CONT. IF "Y" THEN PRINT CONTRACTUAL
      *             FOR DIRECT LOAN 150+ POT'S
      *  GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  BLV 032598 CHANGED TO ALWAYS PRINT REPO SOLD VOLUME AND REDEEMED
      *             VOLUME IF GP-PROGRP-PRINT-REDEEM = "Y"
      *  BLV 052298 CHANGED TO CHECK GP-PROGRP-CLASSES TO PRIORITIZE WHICH
      *             CLASSES TO PRINT FIRST, PR# 622, CASTLE
      *  BLV 062398 FIX DELINQUENCY FOR A10
      *  BLV 062498 ADD RBS & ROS, INCLUDE PART CHGOFFS, TRASH P4P5
      *             SECTION, ADD BANKRUPT DELINQUENCY SECTION
      *  BLV 062998 ONLY PRINT 6 DEALER LINES TO KEEP REPORT TO 2 PAGES
      *  JTG 063098 CHANGED TO CORRECTLY USE DS-TODL.... FIELDS
      *  BLV 073198 DEALER LINES WEREN'T PRINTING LAST DEALER; FIX BATCH
      *             PRINTING "(P/2)" INSTEAD OF MM/YY IN HEADING
      *  CS  111398 IF GP-I-AM-MERC FORCE INSURANCE ELIGIBLE EQUAL TO
      *             VOLUME UNITS
      *  BLV 990311 CHANGED SKIPPING TOP 10 DEALER STATS FOR MULTIPLE
      *             BRANCHES UNLESS I-AM-MERC
      *  BLV 990317 ADD COLUMN FOR $ AMT OF LOANS MADE BY SOURCE CODE
      *             REGENCY PR# 25
      *  BLV 990416 MERCURY PR#213 - DON'T INCLUDE BANKRUPT DELINQUENCY
      *             IN REGULAR DELINQUENCY TABLE
      *  BLV 990416 MERCURY PR#213 - INCLUDE CURRENT DELINQUENCY -
      *             COMBINE WITH 30 DAY POTS & PRINT
      *  BLV 990720 REGIONAL MGMT PR#903 - OPTION TO SHOW P&L ONLY,
      *             NOT INCLUDE P2 OR P3
      *  BLV 000807 FIX GROUP PROBLEM IF INCONSISTENT LEVELS IN GROUP
      *             HIERARCHY, SEE GROUP.BUG IN /USR/A0102B/WORLD
      *  BLV 010531 ADD OTHER 2 INSURANCE COLUMN, EXCEL PR# 1505
      *  BLV 010605 ADD OTHER 3 INSURANCE COLUMN, PER JR
      *  BLV 010606 FIXED ERROR NOT GETTING EXIT PATH - MADE PROGRAM
      *             GO TO A $
      *  BLV 030122 FIXED TO CHECK GP-THE-ONLY-SF-SRC-CODE, IF THIS IS
      *             BLANK, PRINT "RF" AS SOURCE FOR BUCKET 6 & DON'T
      *             FORCE BUCKET 6 VOLUME TO DEALER TOTALS IF DEALER
      *             COUNT IS 0
      *  BLV 030730 IF LC-PROGRP-CLASS IS NOT 0, GROUP THOSE CLASSES
      *             TOGETHER IN TOP SECTION OF REPORT.  FOR EXAMPLE,
      *             CLASS 10 IS SET IN GP-PROGRP-LN-LINE TO PRINT ON
      *             LINE 1 OF REPORT; PUT 10 IN LC-PROGRP-CLASS FOR
      *             CLASS 20 & IT WILL GET COMBINED WITH CLASS 10.
      *             EXCEL PR# 2098
      *  BLV 031229 BARB & BETH - GET RID OF I-AM-MERC CHECK ON WHETHER
      *             TO SKIP DS IF MORE THAN 1 BRANCH, JUST CHECK ACCESS
      *             ON DS & SKIP TO BY IF NOT THERE
      *  BLV 031231 INTIALIZE CLASS-SEQUENCE TABLE TO ZEROS.  ROUTINE
      *             TO LOAD CLASS TABLE WAS SKIPPING CLASSES IF
      *             GP PROGRP CLASSES WERE SPACES AND LC PROGRP
      *             CLASSES WERE SPACES!
      *  BLF 061114 EXPANDED ALL GROUP SUBSCRIPTS TO ACCOMODATE
      *             EXPANDED GROUP SEARCH TABLES, WORLD PR# 502
      *  BLF 070801 CHANGED INS3 HEADING FROM "FIRE" TO "PROP",
      *             CONSUMER PR# 3025
      *  BLF 070806 INCREASE INS-POL-TABLE FROM 6 TO 10, & PRINT
      *             STATS FOR INS 9 (AD&D) AND 10 (CAR CLUB),
      *             CONSUMER PR# 3027
      *  BLM 140315 FIX FOR NEW BY-DATE FORMAT, PD0003
      *  BLM 140315 FIX FOR NEW DS-DATE FORMAT, PD0003
      *
      * KEC 2016.0929 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED BY-DATE TO BY-DATE-CCYYMM FOR CONSISTENCY.
      *
      * KEC 2017.0105 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      *
      *  BAH 170712 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181015 GLOBAL DSFILE
      *  KEC 190212 A32 F2SL - REVISED FOR SPLITTING THE BYFILE.
      *            REMOVED LOAN APP FIELDS IN BY & TY.
      *   CS 190416 CAR CLUB AND AD&D NOT PRINTING ON PROGRESS REPORT
      *             BARB ADDED NEW INSURANCE LEVELS BUT DIDNT CHANGE
      *             THIS REPORT SO ITS NOT LOOKING AT THE ADD/CAR CLUB
      *             OCCURANCE IN STATS.
      *             IN A30 THE CAR CLUB/ADD ARE IN BY-FEE-TYPES
      *             07  BY-FEE-TYPE            OCCURS 3.
      *                  07  BY-FEE-TYPE            OCCURS 3.
      *                        09  BY-FEE-NO-ELIG     PIC S9(6)    COMP-3.
      *                        09  BY-FEE-NO-WROTE    PIC S9(6)    COMP-3.
      *                        09  BY-FEE-NO-NOTWROTE PIC S9(6)    COMP-3.
      *                        09  BY-FEE-AMT-WROTE   PIC S9(8)V99 COMP-3.
      *             FIXED LOANS ELIGIBLE,POLICIES WRITTEN,POLICIES NOT WRITTEN
      *             PREMIUMS,REFUNDS,NET INSURANCE   QA0004
      *  BAH 190830 INCREASED INS-POL-TABLE TO 13 FROM 10, GETTING GARBAGE
      *             TOTALS, THIS WAS CHANGED IN A30 AND MISSED IN A32!
      * BAH 20190924 REMOVED ACCESS-CALL BYAFILE, BYB, DSFILE
      * BAH 20200827 REMOVED RESET ON BR1 
      * BAH 20220808 INSURANCE WRITTEN SUBSCRIPTS WRONG FOR AD&D AND CARCLUB
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WK-FILE ASSIGN MKTEMP-BUF
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK-KEY
                  ALTERNATE RECORD KEY WK-KEY2 WITH DUPLICATES
                  FILE STATUS FILE-STAT.
      /
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPRL.CPY".
      ******************************************************************
      *       WORK FILE - SORTS DEALER STATS BY # ACCTS
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BRANCH           PIC 9(4).
               05  WK-DLNO             PIC 9(4)V99.
               05  WK-LEVEL            PIC 99.
           03  WK-KEY2.
               05  WK-LEVEL2           PIC 99.
               05  WK-MTD-PUR-AMT      PIC S9(9)V99.

           03  WK-DATA.
               05  WK-DL-NAME          PIC X(20).
               05  WK-DL-MFGCODE       PIC X(3).
               05  WK-OUTSAMT          PIC S9(9)V99  COMP-3.
               05  WK-OUTSUNIT         PIC S9(8)     COMP-3.
               05  WK-MTD-PUR-NO       PIC S9(8)     COMP-3.
               05  WK-YTD-PUR-NO       PIC S9(8)     COMP-3.
               05  WK-YTD-PUR-AMT      PIC S9(9)V99  COMP-3.
               05  WK-MTD-TODL         PIC S9(8)     COMP-3.
               05  WK-YTD-TODL         PIC S9(8)     COMP-3.
               05  WK-YTD-TODL-AMT     PIC S9(9)V99  COMP-3.
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/PROGRP.CBL S35 02/14/22-15:45:33 >".

      ******************************************************************
      *     F I L E   I F N
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BY.CPY".
       COPY "LIBLP/LP01BYA.CPY".
       COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LP01BY_SQL.CPY".
       COPY "LIBLP/LPWSBY.CPY".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      *-----------------------------------------------------------------
       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  WK-IFN               PIC X(2)     VALUE "WK".

      ******************************************************************
      *   LINE WORKERS
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999   COMP   VALUE 0.
           03  LN-COUNT         PIC 999   COMP   VALUE 99.
           03  LN-WK            PIC 999   COMP   VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)  COMP   VALUE 0.

      ******************************************************************
      *   MISC WORKERS
      ******************************************************************
       01  FEE-SUB              PIC 9.
       01  SUB                  PIC 99.
       01  LEVEL-SUB            PIC 9999.
       01  TOT-SUB              PIC 99.
       01  GROUP-SUB            PIC 9999.
       01  PREV-SUB             PIC 9999.
       01  TYPE-SUB             PIC 99.
       01  CLASS-SUB            PIC 99.
       01  GP-SUB               PIC 99.
       01  SAVE-SUB             PIC 99.
       01  DL-SUB               PIC 99.
       01  DEL-SUB              PIC 99.
       01  AGE-SUB              PIC 99.
       01  INS-SUB              PIC 99.
       01  REPO-SUB             PIC 99.
       01  NEXT-SUB             PIC 99.
       01  PL-SUB               PIC 99.
       01  PLA-SUB              PIC 99.
       01  PLI-SUB              PIC 99.
       01  DEALER-COUNT         PIC 9(5).
       01  MORE-DEALERS         PIC X.
       01  ELAPSED-MNTHS        PIC 999.

       01  HOLD-BY1-KEY.
           03  HOLD-BY-DATE-CCYYMM     PIC 9(6).
           03  HOLD-BY-BRNO     PIC 9(4).
           03  HOLD-BY-CLASS    PIC 9(3).
       01  CUR-MO-BY-DATE-CCYYMM       PIC 9(6).
       01  FILLER REDEFINES CUR-MO-BY-DATE-CCYYMM.
           03  FILLER           PIC X(4).
           03  CURRENT-MONTH    PIC 99.
       01  CUR-MO-DS-DATE-CCYYMM       PIC 9(6).
       01  NEXT-MO-BY-DATE-CCYYMM      PIC 9(6).

       01  SOURCE-VALUES.
           03  FILLER           PIC X(16) VALUE
               "  SC  NB  PB  FB".
           03  FILLER           PIC X(20) VALUE
               "  RN  SF  RB  RO TOT".
       01  SOURCE-TABLE REDEFINES SOURCE-VALUES.
           03  SOURCE-ELE       PIC X(4) OCCURS 9 TIMES.

       01  CLASS-MAX            PIC 99 VALUE 0.
       01  CLASS-LINE-COUNT     PIC 99 VALUE 0.
       01  OTHER-LN-CLASSES     PIC X  VALUE "N".
       01  OTHER-SF-CLASSES     PIC X  VALUE "N".
       01  OTHER-LN-SUB         PIC 99 VALUE 0.
       01  OTHER-SF-SUB         PIC 99 VALUE 0.
       01  TOTAL-LN-SUB         PIC 99 VALUE 0.
       01  TOTAL-SF-SUB         PIC 99 VALUE 0.
       01  TOTAL-ALL-SUB        PIC 99 VALUE 0.
       01  SUBTOTAL-SUB         PIC 99 VALUE 0.
       01  DIRECT-LN-CLASS-FG   PIC X  VALUE "Y".
       01  CLASS-SEQUENCE-TABLE.
           03  CLASS-SEQ-TABLE OCCURS 12.
               05  HOLD-CLASS   PIC 9(3).
               05  CLASS-ELE    PIC X(16).

       01  REPO-PROMPT-TABLE.
           03  FILLER       PIC X(27) VALUE
               "ON HAND  REDEEMED SOLD     ".
       01  REPO-PROMPT-TAB REDEFINES REPO-PROMPT-TABLE.
           03  REPO-PROMPT      PIC X(9) OCCURS 3.

       01  P4P5-PROMPT-TABLE.
           03  FILLER       PIC X(4) VALUE
               "P4P5".
       01  P4P5-PROMPT-TAB REDEFINES P4P5-PROMPT-TABLE.
           03  P4P5-PROMPT      PIC X(2) OCCURS 2.

       01  DEALER-SUB-TOTS.
           03  OTHER-OUTSUNIT          PIC S9(8).
           03  OTHER-OUTSAMT           PIC S9(9)V99.
           03  OTHER-MTD-PUR-NO        PIC S9(8).
           03  OTHER-MTD-PUR-AMT       PIC S9(9)V99.
           03  OTHER-YTD-PUR-NO        PIC S9(8).
           03  OTHER-YTD-PUR-AMT       PIC S9(9)V99.
           03  OTHER-MTD-TODL          PIC S9(8).
           03  OTHER-YTD-TODL          PIC S9(8).
           03  OTHER-YTD-TODL-AMT      PIC S9(9)V99.
           03  TOTAL-OUTSUNIT          PIC S9(8).
           03  TOTAL-OUTSAMT           PIC S9(9)V99.
           03  TOTAL-MTD-PUR-NO        PIC S9(8).
           03  TOTAL-MTD-PUR-AMT       PIC S9(9)V99.
           03  TOTAL-YTD-PUR-NO        PIC S9(8).
           03  TOTAL-YTD-PUR-AMT       PIC S9(9)V99.
           03  TOTAL-MTD-TODL          PIC S9(8).
           03  TOTAL-YTD-TODL          PIC S9(8).
           03  TOTAL-YTD-TODL-AMT      PIC S9(9)V99.
       01  CLASS-FG                    PIC X(3)        VALUE "   ".
           88  CLASS-BEG                               VALUE "BEG".
           88  CLASS-END                               VALUE "END".
      ******************************************************************
      *    T O T A L   T A B L E
      ******************************************************************
       01  TOTAL-TABLE.
           03  TOTAL-TAB        OCCURS 10.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SECTION 1 - GAIN/LOSS/VOLUME BY LOAN CLASS
      *              SUB1 = TOTAL LEVEL
      *              SUB2:  VARIABLE FROM 1 TO 12
      *                     UP TO 4 LOAN CLASSES
      *                     MAYBE OTHER LOAN CLASSES
      *                     TOTAL LOAN CLASSES
      *                     UP TO 3 SALES FINANCE CLASSES
      *                     MAYBE OTHER SALES FINANCE CLASSES
      *                     TOTAL SALES FINANCE CLASSES
      *                     TOTAL ALL CLASSES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  CLASS-TABLE OCCURS 12.
                   07  CLASS-GAIN-NO-CUR     PIC S9(9) COMP.
                   07  CLASS-GAIN-NO-ELM     PIC S9(9) COMP.
                   07  CLASS-GAIN-NO-ELY     PIC S9(9) COMP.
                   07  CLASS-GAIN-AMT-CUR    PIC S9(11)V99 COMP.
                   07  CLASS-GAIN-AMT-ELM    PIC S9(11)V99 COMP.
                   07  CLASS-GAIN-AMT-ELY    PIC S9(11)V99 COMP.
                   07  CLASS-VOL-AMT-MTD     PIC S9(11)V99 COMP.
                   07  CLASS-VOL-AMT-YTD     PIC S9(11)V99 COMP.
                   07  CLASS-PRINCOL-MTD     PIC S9(11)V99 COMP.
      * NONFILE (11) AD&D (12)  CARCLUB (13)
               05  INS-POL-TABLE             OCCURS 13.
                   07  INS-NO-ELIG           PIC S9(9) COMP.
                   07  INS-NO-WROTE          PIC S9(9) COMP.
                   07  INS-NO-NOTWROTE       PIC S9(9) COMP.
                   07  INS-AMT-WROTE         PIC S9(11)V99 COMP.
                   07  INS-AMT-REFUND        PIC S9(11)V99 COMP.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SECTION 2 -  VOLUME BY TYPE
      *              SUB1 = TOTAL LEVEL
      *              SUB2:  1 = SC, 2 = NB, 3 = PB, 4 = FB,
      *                     5 = RN, 6 = SF, 7 = RB, 8 = RO,
      *                     9 = TOTAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  TYPE-TABLE OCCURS 9.
                   07  TYPE-VOL-NO-MTD       PIC S9(9) COMP.
                   07  TYPE-VOL-AMT-MTD      PIC S9(11)V99 COMP.
                   07  TYPE-VOL-NO-YTD       PIC S9(9) COMP.
                   07  TYPE-VOL-AMT-YTD      PIC S9(11)V99 COMP.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SECTION 3 -  DEALER STATS - DELINQUENT BUCKETS
      *              SUB1 = TOTAL LEVEL
      *              SUB2:  1 - CURRENT
      *                     2 - BEG MONTH
      *                     3 - END LAST MONTH
      *  UNLESS DEL-FLAG = Y, THEN 1 - BEG NEXTMO, 2 - END CUR MO
      *              SUB3:  1 = 30
      *                     2 = 60
      *                     3 = 90+
      *                     4 = POTENTIAL CHARGE-OFFS (150+)
      *                     5 = 30 DAY POTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  CUR-LAST-BUCKETS OCCURS 3.
                   07  LOAN-BUCKETS OCCURS 5.
                       09  LOAN-DEL-NO        PIC S9(9) COMP.
                       09  LOAN-DEL-AMT       PIC S9(11)V99 COMP.
                       09  LOAN-BNKRPT-NO     PIC S9(9) COMP.
                       09  LOAN-BNKRPT-AMT    PIC S9(11)V99 COMP.
                   07  SALEFIN-BUCKETS OCCURS 5.
                       09  SALEFIN-DEL-NO     PIC S9(9) COMP.
                       09  SALEFIN-DEL-AMT    PIC S9(11)V99 COMP.
                       09  SALEFIN-BNKRPT-NO  PIC S9(9) COMP.
                       09  SALEFIN-BNKRPT-AMT PIC S9(11)V99 COMP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SECTION 5 - LATE CHARGES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  LATE-NO-COL               PIC S9(9) COMP.
               05  LATE-NO-WAIVED            PIC S9(9) COMP.
               05  LATE-AMT-COL              PIC S9(11)V99 COMP.
               05  LATE-AMT-WAIVED           PIC S9(11)V99 COMP.
               05  REPO-TABLE                OCCURS 3.
                   07  REPO-GAIN-NO-CUR      PIC S9(9) COMP.
                   07  REPO-GAIN-NO-MTD      PIC S9(9) COMP.
                   07  REPO-GAIN-NO-YTD      PIC S9(9) COMP.
                   07  REPO-GAIN-AMT-CUR     PIC S9(11)V99 COMP.
                   07  REPO-GAIN-AMT-MTD     PIC S9(11)V99 COMP.
                   07  REPO-GAIN-AMT-YTD     PIC S9(11)V99 COMP.
                   07  REPO-VOL-NO-MTD       PIC S9(9) COMP.
                   07  REPO-VOL-NO-YTD       PIC S9(9) COMP.
                   07  REPO-VOL-NO-LYR       PIC S9(9) COMP.
                   07  REPO-VOL-AMT-MTD      PIC S9(11)V99 COMP.
                   07  REPO-VOL-AMT-YTD      PIC S9(11)V99 COMP.
                   07  REPO-VOL-AMT-LYR      PIC S9(11)V99 COMP.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *              CHARGE-OFFS (PL, P2, P3)
      *              SUB1 = TOTAL LEVEL
      *              SUB2:  1 - LOAN TOTAL
      *                     2 - SALES FINANCE TOTAL
      *                     3 - TOTAL LOAN + SALES FINANCE
      *              AMTS INCLUDE PLA'S + PLI'S
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  PL-CLASS-TABLE            OCCURS 3.
                   07  PL-VOL-AMT-MTD        PIC S9(11)V99 COMP.
                   07  PL-VOL-AMT-YTD        PIC S9(11)V99 COMP.
                   07  PL-RECV-AMT-MTD       PIC S9(11)V99 COMP.
                   07  PL-RECV-AMT-YTD       PIC S9(11)V99 COMP.
                   07  PART-CHGOFF-MTD       PIC S9(11)V99 COMP.
                   07  PART-CHGOFF-YTD       PIC S9(11)V99 COMP.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *              CHARGE-OFFS (P4, P5)
      *              SUB1 = TOTAL LEVEL
      *              SUB2:  1 - P4
      *                     2 - P5
      *              AMTS INCLUDE PLA'S + PLI'S
               05  P45-CLASS-TABLE           OCCURS 2.
                   07  P45-VOL-NO-MTD        PIC S9(9) COMP.
                   07  P45-VOL-NO-YTD        PIC S9(9) COMP.
                   07  P45-VOL-PRIN-MTD      PIC S9(11)V99 COMP.
                   07  P45-VOL-PRIN-YTD      PIC S9(11)V99 COMP.
                   07  P45-VOL-O2-MTD        PIC S9(11)V99 COMP.
                   07  P45-VOL-O2-YTD        PIC S9(11)V99 COMP.
                   07  P45-RECV-PRIN-MTD     PIC S9(11)V99 COMP.
                   07  P45-RECV-PRIN-YTD     PIC S9(11)V99 COMP.
                   07  P45-RECV-O2-MTD       PIC S9(11)V99 COMP.
                   07  P45-RECV-O2-YTD       PIC S9(11)V99 COMP.
                   07  P45-VOL-TOT-MTD       PIC S9(11)V99 COMP.
                   07  P45-VOL-TOT-YTD       PIC S9(11)V99 COMP.
                   07  P45-VOL-TOT-LYR       PIC S9(11)V99 COMP.
                   07  P45-RECV-TOT-MTD      PIC S9(11)V99 COMP.
                   07  P45-RECV-TOT-YTD      PIC S9(11)V99 COMP.
               05  DLRV-GAIN-CUR             PIC S9(11)V99 COMP.
               05  DLRV-GAIN-ELM             PIC S9(11)V99 COMP.
               05  DLDISC-MTD-VOL            PIC S9(11)V99 COMP.

       01  PREV-SCURR-TABLE.
           03  PREV-SCURR                    PIC 9999 COMP OCCURS 15.

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  DISPLAY-DATE             PIC 9(4).
       01  DISPLAY-DATEX            REDEFINES DISPLAY-DATE.
           03  DISPLAY-MO           PIC 99.
           03  DISPLAY-YR           PIC 99.
       01  DATE-LIST                PIC X(5)  VALUE "MMYY.".

       01  NO-WORK              PIC S9(10)    VALUE +0.
       01  AMT-WORK             PIC S9(10)V99 VALUE +0.
       01  AVG-WORK             PIC S9(10)V99 VALUE +0.
       01  PCT-WORK             PIC S9(3)V999 VALUE +0.
       01  PCT-WORKER           PIC S9(3)V9   VALUE +0.
       01  PCT-WORKER2          PIC S9(3)V99  VALUE +0.


       01  STAR-LINE.
           03  FILLER          PIC X(46) VALUE
           "______________________________________________".
           03  FILLER          PIC X(45) VALUE
           "_____________________________________________".
           03  FILLER          PIC X(45) VALUE
           "_____________________________________________".
       01  DASH-LINE.
           03  FILLER          PIC X(46) VALUE
           "----------------------------------------------".
           03  FILLER          PIC X(45) VALUE
           "---------------------------------------------".
           03  FILLER          PIC X(45) VALUE
           "---------------------------------------------".
       01  VERT-BAR-LINE.
           03  D-BAR           PIC X OCCURS 136.

      ******************************************************************
      *     S C R E E N   F I E L D S
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  SERVACCT-FG     PIC X.
           03  DEL-FLAG        PIC X.
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE  PIC X OCCURS 10.
           03  STORE-DATE      PIC 9(4).
           03  STORE-DATEX     REDEFINES STORE-DATE.
               05  STORE-MO    PIC 9(2).
               05  STORE-YR    PIC 9(2).
           03  LOCAL-PRINT-FG  PIC X.
           03  INCLUDE-P2-P3   PIC X.

       01  VERSION-CONTROL     PIC X     VALUE "C".
       01  INC-O2-GAIN         PIC X     VALUE "Y".
       01  INC-O2-VOL          PIC X     VALUE "N".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  SV-LOCAL-PRINT-FG   PIC X     VALUE SPACES.
      ******************************************************************
      *     P R I N T   B U F F E R S
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(28)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(36)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZZ9.
           03  FILLER           PIC X(2)     VALUE " ".

      *----------------------------------------------------------------
       01  HEAD2.
           03  HEAD2-TYPE       PIC X(10).
           03  H-GRID           PIC X(4).
           03  H-BRNO REDEFINES H-GRID PIC Z(4).
           03  FILLER           PIC XX VALUE "  ".
           03  H-GRNAME         PIC X(29) VALUE SPACES.
           03  FILLER           PIC X(7)     VALUE " ".
           03  H-TITLE          PIC X(25) VALUE
           "- -  PROGRESS REPORT  - -".
           03  FILLER           PIC X(2) VALUE SPACES.
           03  H-MMYY           PIC 99/99.
           03  FILLER           PIC X(40) VALUE SPACES.
           03  FILLER           PIC X(6)    VALUE "USER:".
           03  H-USERID         PIC X(6).

      *----------------------------------------------------------------
       01  DETAIL1-HEAD1.
           03  FILLER           PIC X(17) VALUE
               "LOAN CLASS       ".
           03  FILLER           PIC X(42) VALUE
               "  OUTSTANDING  GAIN THIS MO  GAIN THIS YR ".
           03  FILLER           PIC X(36) VALUE
               "   VOLUME MTD    VOLUME YTD  % CASH".
           03  FILLER           PIC X(36) VALUE
               "  NO. ACCTS    GAIN MTD    GAIN YTD ".
           03  FILLER           PIC X(5)    VALUE SPACES.

      *----------------------------------------------------------------
       01  DETAIL1-HEAD2.
           03  FILLER           PIC X(88) VALUE SPACES.
           03  FILLER           PIC X(7) VALUE
               "COL MTD".
           03  FILLER           PIC X(41) VALUE SPACES.

      *----------------------------------------------------------------
       01  DETAIL-INS-HEAD.
           03  FILLER           PIC X(42) VALUE
               "LOAN        NO              $   % TO #    ".
           03  FILLER           PIC X(32) VALUE
               "INSURANCE         LIFE      A&H ".
           03  FILLER           PIC X(36) VALUE
               "    PROP   OTHER1   OTHER2   OTHER3 ".
           03  FILLER           PIC X(18) VALUE
               "    AD&D  CARCLUB ".
       01  DETAIL-INS-HEAD2.
           03  FILLER           PIC X(42) VALUE
               "SRC       MADE           MADE    ACCTS    ".

      *----------------------------------------------------------------
       01  DETAIL2-HEAD1.
           03  FILLER           PIC X(44) VALUE
               "     DEALER             BR#   COM      NO   ".
           03  FILLER           PIC X(33) VALUE
               "   DOLLARS    # PUR     $ PUR    ".
           03  FILLER           PIC X(33) VALUE
               " # PUR       $ PUR   #CON  % CON ".
           03  FILLER           PIC X(25) VALUE
               "   #CON  % CON     YTD SF".
           03  FILLER           PIC X VALUE SPACES.

      *----------------------------------------------------------------
       01  DETAIL2-HEAD2.
           03  FILLER           PIC X(30)  VALUE SPACES.
           03  FILLER           PIC X(12) VALUE
               "MOD     ACCT".
           03  FILLER           PIC X(35) VALUE
               "        OUTS      MTD       MTD    ".
           03  FILLER           PIC X(33) VALUE
               "   YTD         YTD    MTD    MTD ".
           03  FILLER           PIC X(25) VALUE
               "    YTD    YTD      TODLT".
           03  FILLER           PIC X VALUE SPACE.

      *----------------------------------------------------------------
       01  DETAIL3-HEAD1.
           03  DETAIL3-PROMPT1  PIC X(11).
           03  FILLER           PIC X(50) VALUE
               "       1 PAYMENT (30)           2 PAYMENTS (60)   ".
           03  FILLER           PIC X(51) VALUE
               "   3 PMTS & OVER (90+)     POTENTIAL CHG-OFFS(150+)".
           03  FILLER           PIC X(24) VALUE
               "    30 DAY POTENTIALS   ".

      *----------------------------------------------------------------
       01  DETAIL3-HEAD2.
           03  DETAIL3-PROMPT2  PIC X(11).
           03  FILLER           PIC X(50) VALUE
               "   NO.     AMOUNT   % TO    NO.     AMOUNT   % TO ".
           03  FILLER           PIC X(50) VALUE
               "   NO.     AMOUNT   % TO    NO.     AMOUNT   % TO ".
           03  FILLER           PIC X(25) VALUE
               "   NO.     AMOUNT   % TO ".

      *----------------------------------------------------------------
       01  DETAIL3-HEAD3.
           03  DETAIL3-PROMPT3  PIC X(11).
           03  FILLER           PIC X(50) VALUE
               "  ACCTS              OS    ACCTS              OS  ".
           03  FILLER           PIC X(50) VALUE
               "  ACCTS              OS    ACCTS              OS  ".
           03  FILLER           PIC X(25) VALUE
               "  ACCTS              OS  ".

      *----------------------------------------------------------------
       01  DETAIL5-HEAD1.
           03  FILLER           PIC X(48) VALUE
               "CHARGE OFF      MONTH       YEAR  YTD % OS      ".
           03  FILLER           PIC X(48) VALUE
               "RECOVERY        MONTH       YEAR  YTD % NET/OS  ".
           03  FILLER           PIC X(40) VALUE
               "                       LATE CHARGES     ".
      *----------------------------------------------------------------
       01  DETAIL-REPO-HEAD1.
           03  FILLER           PIC X(9) VALUE SPACES.
           03  FILLER           PIC X(20) VALUE
               "  REPO RECEIVABLES  ".
           03  FILLER           PIC X(24) VALUE
               "  REPO UNITS: ACTIVITY  ".
           03  FILLER           PIC X(16) VALUE
               "   REPO FREQ %  ".
           03  FILLER           PIC X(24) VALUE
               "  REPO RECEIV: ACTIVITY ".
       01  DETAIL-REPO-HEAD2.
           03  FILLER           PIC X(9) VALUE SPACES.
           03  FILLER           PIC X(20) VALUE
               "   ACCTS   OUTSTAND ".
           03  FILLER           PIC X(24) VALUE
               "        MTD         YTD ".
           03  FILLER           PIC X(16) VALUE
               "    MTD     YTD ".
           03  FILLER           PIC X(23) VALUE
               "       MTD        YTD  ".
      *----------------------------------------------------------------
       01  DETAIL-P4P5-HEAD1.
           03  FILLER           PIC X(69) VALUE SPACES.
           03  FILLER           PIC X(18) VALUE
               "   AVERAGE LOSS   ".
           03  FILLER           PIC X(28) VALUE
               "         RECOVERIES         ".
           03  FILLER           PIC X(17)    VALUE SPACES.
       01  DETAIL-P4P5-HEAD2.
           03  FILLER           PIC X(15) VALUE SPACES.
           03  FILLER           PIC X(26) VALUE
               "       # MTD        # YTD ".
           03  FILLER           PIC X(28) VALUE
               "        $ MTD         $ YTD ".
           03  FILLER           PIC X(18) VALUE
               "     MTD      YTD ".
           03  FILLER           PIC X(28) VALUE
               "       MTD        YTD ".
           03  FILLER           PIC X(17)    VALUE SPACES.
      *----------------------------------------------------------------
       01  RESERVE-HEAD1.
           03  FILLER           PIC X(16) VALUE SPACES.
           03  FILLER           PIC X(16) VALUE
               "DEALER RESERVE".
           03  FILLER           PIC X(10) VALUE
               " % TO OUTS".
           03  FILLER           PIC X(5) VALUE SPACES.
           03  FILLER           PIC X(16) VALUE
               "RESERVE LOSSES %".
           03  FILLER           PIC X(19) VALUE SPACES.
           03  FILLER           PIC X(10)  VALUE
               "MTD VOLUME".
           03  FILLER           PIC X(6)  VALUE SPACES.
           03  FILLER           PIC X(7)  VALUE
               "% TO SF".
           03  FILLER           PIC X(29) VALUE SPACES.

      ******************************************************************
      *    D E T A I L - L I N E
      ******************************************************************
       01  DETAIL-LINE          PIC X(136) VALUE SPACES.

      *----------------------------------------------------------------
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D1-CLASS           PIC X(16).
           03  FILLER             PIC X.
           03  D1-GAIN-AMT-CUR    PIC ZZZZZ,ZZZ,ZZ9-     BLANK ZERO.
           03  D1-GAIN-AMT-MTD    PIC ZZZZZ,ZZZ,ZZ9-     BLANK ZERO.
           03  D1-GAIN-AMT-YTD    PIC ZZZZZ,ZZZ,ZZ9-     BLANK ZERO.
           03  D1-VOL-AMT-MTD     PIC ZZZZZ,ZZZ,ZZ9-     BLANK ZERO.
           03  D1-VOL-AMT-YTD     PIC ZZZZZ,ZZZ,ZZ9-     BLANK ZERO.
           03  D1-CASH-PCT        PIC ZZZZZ.9-           BLANK ZERO.
           03  D1-GAIN-NO-CUR     PIC ZZZ,ZZZ,ZZ9-       BLANK ZERO.
           03  D1-GAIN-NO-MTD     PIC ZZZ,ZZZ,ZZ9-       BLANK ZERO.
           03  D1-GAIN-NO-YTD     PIC ZZZ,ZZZ,ZZ9-       BLANK ZERO.
           03  FILLER             PIC X(5).

      *----------------------------------------------------------------
       01  DETAIL1A REDEFINES DETAIL-LINE.
           03  D1-PROMPT1            PIC X(19).
           03  D1-PROMPT2            PIC X(16).
           03  D1-NEW-AVG            PIC ZZZZZ,ZZ9-        BLANK ZERO.
           03  D1-PROMPT3            PIC X(18).
           03  D1-MADE-AVG           PIC ZZZZZ,ZZ9-        BLANK ZERO.
           03  D1-PROMPT4            PIC X(24).
           03  D1-LOAN-AVG           PIC ZZZZZ,ZZ9-        BLANK ZERO.
           03  D1-PROMPT5            PIC X(14).
           03  D1-SF-AVG             PIC ZZZZZ,ZZ9-        BLANK ZERO.
           03  FILLER                PIC X(5).

      *----------------------------------------------------------------
       01  DETAIL-INS-LATE REDEFINES DETAIL-LINE.
      *----------------------------------------------------------------
           03  D2-SOURCE             PIC X(5).
           03  D2-VOL-NO             PIC ZZZZZZ,ZZ9-      BLANK ZERO.
           03  D2-VOL-AMT            PIC ZZZZZ,ZZZ,ZZ9-   BLANK ZERO.
           03  D2-VOL-PCT1           PIC ZZZZZ.9-         BLANK ZERO.
           03  FILLER                PIC X(4).
           03  D1-INS-PROMPT         PIC X(14).
           03  D1-INS-NO1            PIC ZZZZ,ZZ9- BLANK ZERO.
           03  D1-INS-PCT1 REDEFINES D1-INS-NO1
                                     PIC ZZ,ZZZ.9- BLANK ZERO.
           03  D1-INS-NO2            PIC ZZZZ,ZZ9- BLANK ZERO.
           03  D1-INS-PCT2 REDEFINES D1-INS-NO2
                                     PIC ZZ,ZZZ.9- BLANK ZERO.
           03  D1-INS-NO3            PIC ZZZZ,ZZ9- BLANK ZERO.
           03  D1-INS-PCT3 REDEFINES D1-INS-NO3
                                     PIC ZZ,ZZZ.9- BLANK ZERO.
           03  D1-INS-NO4            PIC ZZZZ,ZZ9- BLANK ZERO.
           03  D1-INS-PCT4 REDEFINES D1-INS-NO4
                                     PIC ZZ,ZZZ.9- BLANK ZERO.
           03  D1-INS-NO5            PIC ZZZZ,ZZ9- BLANK ZERO.
           03  D1-INS-PCT5 REDEFINES D1-INS-NO5
                                     PIC ZZ,ZZZ.9- BLANK ZERO.
           03  D1-INS-NO6            PIC ZZZZ,ZZ9- BLANK ZERO.
           03  D1-INS-PCT6 REDEFINES D1-INS-NO6
                                     PIC ZZ,ZZZ.9- BLANK ZERO.
           03  D1-INS-NO7            PIC ZZZZ,ZZ9- BLANK ZERO.
           03  D1-INS-PCT7 REDEFINES D1-INS-NO7
                                     PIC ZZ,ZZZ.9- BLANK ZERO.
           03  D1-INS-NO8            PIC ZZZZ,ZZ9- BLANK ZERO.
           03  D1-INS-PCT8 REDEFINES D1-INS-NO8
                                     PIC ZZ,ZZZ.9- BLANK ZERO.
      *----------------------------------------------------------------
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  D2-DL-NAME            PIC X(23).
           03  FILLER                PIC X.
           03  D2-DL-BRNO            PIC ZZZ9.
           03  FILLER                PIC XX.
           03  D2-MFGCODE            PIC X(3).
           03  D2-OUTSUNIT           PIC ZZZZZ,ZZ9-      BLANK ZERO.
           03  D2-OUTSAMT            PIC ZZZZZZZ,ZZ9-    BLANK ZERO.
           03  D2-MTD-PUR-NO         PIC ZZZZ,ZZ9-       BLANK ZERO.
           03  D2-MTD-PUR-AMT        PIC ZZZZZ,ZZ9-      BLANK ZERO.
           03  D2-YTD-PUR-NO         PIC ZZZZZ,ZZ9-      BLANK ZERO.
           03  D2-YTD-PUR-AMT        PIC ZZZZZZZ,ZZ9-    BLANK ZERO.
           03  D2-MTD-TODL           PIC ZZZZZ9-         BLANK ZERO.
           03  D2-TODL-PCT1          PIC ZZZZ.9-         BLANK ZERO.
           03  D2-YTD-TODL           PIC ZZZZZZ9-        BLANK ZERO.
           03  D2-TODL-PCT2          PIC ZZZZ.9-         BLANK ZERO.
           03  D2-YTD-TODL-AMT       PIC ZZZZZ,ZZ9-      BLANK ZERO.
           03  FILLER                PIC X.

      *----------------------------------------------------------------
       01  DETAIL3 REDEFINES DETAIL-LINE.
      *----------------------------------------------------------------
           03  D3-MONTH-PROMPT       PIC X(11).
           03  D3-DEL-BUCKETS OCCURS 5.
               05  D3-DEL-NO         PIC ZZZZZ9-         BLANK ZERO.
               05  D3-DEL-AMT        PIC ZZZZZZZ,ZZ9-    BLANK ZERO.
               05  D3-DEL-PCT1       PIC ZZZ.9-          BLANK ZERO.

      *----------------------------------------------------------------
       01  DETAIL5 REDEFINES DETAIL-LINE.
           03  D5-PROMPT1            PIC X(12).
           03  D5-CHGOFF-MTD         PIC ZZZZZ,ZZ9-     BLANK ZERO.
           03  FILLER                PIC X(1).
           03  D5-CHGOFF-YTD         PIC ZZZZZ,ZZ9-     BLANK ZERO.
           03  FILLER                PIC X(1).
           03  D5-PCT1               PIC ZZZZ.9-         BLANK ZERO.
           03  FILLER                PIC X(7).
           03  D5-PROMPT2            PIC X(12).
           03  D5-RECOVER-MTD        PIC ZZZZZ,ZZ9-      BLANK ZERO.
           03  FILLER                PIC X(1).
           03  D5-RECOVER-YTD        PIC ZZZZZ,ZZ9-      BLANK ZERO.
           03  FILLER                PIC X(3).
           03  D5-PCT2               PIC ZZZZ.9-         BLANK ZERO.
           03  FILLER                PIC X(8).
           03  D5-LATE-PROMPT        PIC X(12).
           03  D5-LATE-AMOUNTS.
               05  D5-NO-COLLECTED       PIC ZZZZ,ZZZ,ZZ9- BLANK ZERO.
               05  D5-AMT-COLLECTED REDEFINES D5-NO-COLLECTED
                                     PIC ZZZZZ,ZZ9.99-     BLANK ZERO.
               05  D5-NO-WAIVED          PIC ZZZ,ZZZ,ZZ9- BLANK ZERO.
               05  D5-AMT-WAIVED REDEFINES D5-NO-WAIVED
                                     PIC ZZZZ,ZZ9.99-     BLANK ZERO.

       01  DETAIL-REPO REDEFINES DETAIL-LINE.
           03  D-REPO-PROMPT         PIC X(9).
           03  D-REPO-GAIN-NO-CUR    PIC ZZZZ,ZZ9-     BLANK ZERO.
      *    03  D-REPO-GAIN-NO-MTD    PIC ZZZZ,ZZ9-     BLANK ZERO.
      *    03  D-REPO-GAIN-NO-YTD    PIC ZZZZ,ZZ9-     BLANK ZERO.
           03  D-REPO-GAIN-AMT-CUR   PIC ZZZZZZ,ZZ9-   BLANK ZERO.
      *    03  D-REPO-GAIN-AMT-MTD   PIC ZZZZZZ,ZZ9-   BLANK ZERO.
      *    03  D-REPO-GAIN-AMT-YTD   PIC ZZZZZZ,ZZ9-   BLANK ZERO.
           03  D-REPO-VOL-NO-MTD     PIC ZZZZZZZ,ZZ9-  BLANK ZERO.
           03  D-REPO-VOL-NO-YTD     PIC ZZZZZZZ,ZZ9-  BLANK ZERO.
           03  D-REPO-PCT-FREQ-MTD   PIC ZZZZ.Z9-      BLANK ZERO.
           03  D-REPO-PCT-FREQ-YTD   PIC ZZZZ.Z9-      BLANK ZERO.
           03  D-REPO-VOL-AMT-MTD    PIC ZZZZZZ,ZZ9-   BLANK ZERO.
           03  D-REPO-VOL-AMT-YTD    PIC ZZZZZZ,ZZ9-   BLANK ZERO.

      *----------------------------------------------------------------
       01  DETAIL-P4P5 REDEFINES DETAIL-LINE.
           03  D-P45-PROMPT1         PIC X(3).
           03  D-P45-PROMPT2         PIC X(12).
           03  D-P45-VOL-NO-MTD      PIC ZZZZ,ZZZ,ZZ9-  BLANK ZERO.
           03  D-P45-VOL-NO-YTD      PIC ZZZZ,ZZZ,ZZ9-  BLANK ZERO.
           03  D-P45-VOL-AMT-MTD     PIC ZZZZZ,ZZZ,ZZ9- BLANK ZERO.
           03  D-P45-VOL-AMT-YTD     PIC ZZZZZ,ZZZ,ZZ9- BLANK ZERO.
           03  D-P45-MTD-AVG-LOSS    PIC ZZZZ,ZZZ-      BLANK ZERO.
           03  D-P45-YTD-AVG-LOSS    PIC ZZZZ,ZZZ-      BLANK ZERO.
           03  D-P45-VOL-RECV-MTD    PIC ZZZZZ,ZZZ,ZZ9- BLANK ZERO.
           03  D-P45-VOL-RECV-YTD    PIC ZZZZZ,ZZZ,ZZ9- BLANK ZERO.
           03  FILLER                PIC X(17).

      *----------------------------------------------------------------
       01  DETAIL-RESERVE REDEFINES DETAIL-LINE.
           03  D-RESERVE-PROMPT      PIC X(18).
           03  D-RESERVE-TOTAL       PIC ZZZZ,ZZZ,ZZ9-  BLANK ZERO.
           03  FILLER                PIC X(2).
           03  D-RESERVE-OUTS-PCT    PIC ZZZZZZ.99-         BLANK ZERO.
           03  FILLER                PIC X(11).
           03  D-RESERVE-LOSS-PCT    PIC ZZZZZZ.99-         BLANK ZERO.

           03  FILLER                PIC X(3).
           03  D-DLDISC-PROMPT       PIC X(13).
           03  FILLER                PIC X(3).
           03  D-DLDISC-MTD-VOL      PIC ZZZZ,ZZZ,ZZ9-  BLANK ZERO.
           03  FILLER                PIC X(3).
           03  D-DLDISC-PCT          PIC ZZZZZZ.99-     BLANK ZERO.

           03  FILLER                PIC X(21).
      *----------------------------------------------------------------

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/PASSWDW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/PROGRP_DEF.CPY".
       COPY "LIBLP/PROGRP_WKS.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/PROGRP_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-BY1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
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
           MOVE "PROGRP" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

      *----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.
           PERFORM SEARCH-GR-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * AT END OF FILE, FORCE WGR-SCURR -TABLE & TPLUS TO CHANGE FROM
      * PREVIOUS SO GRAND TOTALS WILL PRINT - MUST FORCE BECAUSE ALL
      * LEVELS START AT THE SAME POINT (THE SELECTED PARENT GROUP)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BY-PATH-OVERRIDE
                                              DL-PATH-OVERRIDE
                                              DS-PATH-OVERRIDE.

           PERFORM OPEN-BY1-FILE.

           PERFORM OPEN-DS1-FILE.
       START-DS.
           IF GROUP-FLAG = "Y"
              MOVE WGR-SRESULT TO DS-BRANCH
           ELSE
              MOVE BR-NO       TO DS-BRANCH.

           MOVE DS-BRANCH             TO QDS1-WBEG-BRANCH
                                         QDS1-WEND-BRANCH.
           MOVE CUR-MO-DS-DATE-CCYYMM TO DS-DATE-CCYYMM
                                         QDS1-WBEG-DATE-CCYYMM
                                         QDS1-WEND-DATE-CCYYMM.
           MOVE 0                     TO DS-NUM DS-CLASS
                                         QDS1-WBEG-NUM
                                         QDS1-WBEG-CLASS.
           MOVE ALL "9"               TO QDS1-WEND-NUM
                                         QDS1-WEND-CLASS.

           PERFORM START-DS1-FILE.

      *----------------------------------------------------------------
       NEXT-DS1.
           PERFORM READ-DS1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (DS-BRANCH NOT = DS-PATH-OWNBR)
              GO TO START-BY.

           IF DS-DATE-CCYYMM NOT = CUR-MO-DS-DATE-CCYYMM
              GO TO START-BY.

           IF GROUP-FLAG = "Y"
              IF DS-BRANCH NOT = WGR-SRESULT
                 GO TO START-BY.

           IF GROUP-FLAG NOT = "Y"
              IF DS-BRANCH NOT = BR-NO
                 GO TO START-BY.

           IF DS-CLASS < BEG-CLASS OR DS-CLASS > END-CLASS
              GO TO NEXT-DS1.

           MOVE "N" TO FIRST-TIME.

       OK-TO-PROCESS-DS.
           PERFORM CHECK-ADD-DS-TOTALS.

           GO TO NEXT-DS1.

       START-BY.
           MOVE CUR-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM
                                         QBY1-WBEG-DATE-CCYYMM
                                         QBY1-WEND-DATE-CCYYMM.

           IF GROUP-FLAG = "Y"
              MOVE WGR-SRESULT TO BY-BRNO
           ELSE
              MOVE BR-NO       TO BY-BRNO.

           MOVE BY-BRNO              TO QBY1-WBEG-BRNO
                                        QBY1-WEND-BRNO.

           MOVE BEG-CLASS            TO BY-CLASS
                                        QBY1-WBEG-CLASS.
           MOVE END-CLASS            TO QBY1-WEND-CLASS.

           PERFORM START-BY1-FILE.

      *----------------------------------------------------------------
       NEXT-BY1.
           PERFORM READ-BY1-FILE-NEXT.
           IF IO-FG NOT = 0
              IF GROUP-FLAG = "Y"
                 GO TO END-OF-GROUP
              ELSE
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-OF-GROUP.

           IF BY-DATE-CCYYMM NOT = CUR-MO-BY-DATE-CCYYMM
              IF GROUP-FLAG = "Y"
                 GO TO END-OF-GROUP
              ELSE
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-OF-GROUP.

           IF GROUP-FLAG = "Y"
              IF (BY-BRNO NOT = WGR-SRESULT) OR
                  (BY-CLASS > END-CLASS)
                 GO TO END-OF-GROUP.

           IF GROUP-FLAG NOT = "Y"
              IF (BY-BRNO NOT = BR-NO) OR
                  (BY-CLASS > END-CLASS)
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-OF-GROUP.

      *----------------------------------------------------------------
       OK-TO-PROCESS.
           MOVE "N" TO FIRST-TIME.
           PERFORM CHECK-ADD-TOTALS.
           GO TO NEXT-BY1.

      *----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-BY1-FILE.
           IF GROUP-FLAG = "Y"
              GO TO NEXT-GROUP-LOC
           ELSE
              GO TO NEXT-BRANCH-NO.
      *----------------------------------------------------------------
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE PROGRP-QUEUE-POS TO Q-POS.
           MOVE PROGRP-COPIES-POS TO C-POS.
           MOVE PROGRP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".
           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-BR-FILE.

           PERFORM GET-GPENV.

           MOVE GP-BEG-FISCAL-DATE TO DATE-YYYYMMDD.
           IF DATE-YYYYMMDD-MM < CURRENT-MONTH
              COMPUTE ELAPSED-MNTHS = CURRENT-MONTH - DATE-YYYYMMDD-MM
           ELSE
              COMPUTE ELAPSED-MNTHS = CURRENT-MONTH + 12
                                       - DATE-YYYYMMDD-MM.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.


           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE LOCAL-PRINT-FG TO SV-LOCAL-PRINT-FG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
              IF EXT-EOBUF-BRNO NUMERIC
                 MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
                 MOVE "N" TO GROUP-FLAG LOCAL-PRINT-FG
              ELSE
                 MOVE EXT-EOBUF-GROUP TO ENT-GROUP
                 MOVE 0 TO BEG-BRANCH
                 MOVE 9999 TO END-BRANCH
                 MOVE "Y" TO GROUP-FLAG
                 MOVE "N" TO LOCAL-PRINT-FG
              END-IF
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.


           IF DEL-FLAG = "Y"
              MOVE "MONTH-END PROGRESS REPORT" TO H-TITLE.
           MOVE STORE-DATE TO H-MMYY.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS................" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF LOCAL-PRINT-FG NOT = "Y"
              PERFORM OPEN-PR-FILE.

           MOVE LOW-VALUES TO TOTAL-TABLE.

      * CREATE DEALER SORT WORK FILE

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

      * SET UP CLASS SEQUENCE/SUBSCRIPTS

           MOVE ZEROS TO CLASS-SEQUENCE-TABLE.
           PERFORM OPEN-LC1-FILE.
           MOVE 0 TO SUB.
           MOVE 1 TO GP-SUB.
       CHECK-GP-LN-CLASSES.
           IF GP-PROGRP-LN-CLASS(GP-SUB) NUMERIC
             IF GP-PROGRP-LN-CLASS(GP-SUB) NOT = 0
               IF GP-PROGRP-LN-CLASS(GP-SUB) >= BEG-CLASS
                 IF GP-PROGRP-LN-CLASS(GP-SUB) <= END-CLASS
                    ADD 1 TO SUB
                    MOVE GP-PROGRP-LN-CLASS(GP-SUB) TO HOLD-CLASS(SUB).
           ADD 1 TO GP-SUB.
           IF GP-SUB < 4
              GO TO CHECK-GP-LN-CLASSES.

           MOVE BEG-CLASS TO LC-CODE.

           PERFORM START-LC1-FILE.
       FIND-LN-CLASSES.
           PERFORM READ-LC1-FILE-NEXT.
           IF IO-FG = 9
              GO TO EXIT-FIND-LN-CLASSES.

           IF LC-CODE > END-CLASS
              GO TO FIND-LN-CLASSES.

           IF LC-SALEFIN = "Y"
              GO TO FIND-LN-CLASSES.

           IF LC-CODE = HOLD-CLASS(1)
              MOVE LC-DESC TO CLASS-ELE(1)
              GO TO FIND-LN-CLASSES
           ELSE IF LC-CODE = HOLD-CLASS(2)
              MOVE LC-DESC TO CLASS-ELE(2)
              GO TO FIND-LN-CLASSES
           ELSE IF LC-CODE = HOLD-CLASS(3)
              MOVE LC-DESC TO CLASS-ELE(3)
              GO TO FIND-LN-CLASSES.

      * IF LC-PROGRP-CLASS IS SET, THAT CLASS WILL BE GROUPED
      * WITH THE SELECTED CLASS AT TOP OF REPORT, SO SKIP IT:

           IF LC-PROGRP-CLASS NOT = 0
              IF (LC-PROGRP-CLASS = HOLD-CLASS(1) OR
                  LC-PROGRP-CLASS = HOLD-CLASS(2) OR
                  LC-PROGRP-CLASS = HOLD-CLASS(3))
                  GO TO FIND-LN-CLASSES.

           IF SUB < 3
              ADD 1 TO SUB
              MOVE LC-CODE TO HOLD-CLASS(SUB)
              MOVE LC-DESC TO CLASS-ELE(SUB)
           ELSE
              MOVE "Y" TO OTHER-LN-CLASSES.
           GO TO FIND-LN-CLASSES.
       EXIT-FIND-LN-CLASSES.
           IF SUB = 0
              MOVE "N" TO DIRECT-LN-CLASS-FG.
           IF OTHER-LN-CLASSES = "Y"
              ADD 1 TO SUB
              MOVE SUB TO OTHER-LN-SUB
              MOVE 0 TO HOLD-CLASS(SUB)
              MOVE "DIRECT LNS-OTHER" TO CLASS-ELE(SUB).
           ADD 1 TO SUB.
           MOVE SUB TO TOTAL-LN-SUB.
           MOVE 0 TO HOLD-CLASS(SUB).
           MOVE "DIRECT LNS-TOTAL" TO CLASS-ELE(SUB).

           MOVE SUB TO SAVE-SUB.
           MOVE 1 TO GP-SUB.
       CHECK-GP-SF-CLASSES.
           IF GP-PROGRP-SF-CLASS(GP-SUB) NUMERIC
             IF GP-PROGRP-SF-CLASS(GP-SUB) NOT = 0
               IF GP-PROGRP-SF-CLASS(GP-SUB) >= BEG-CLASS
                 IF GP-PROGRP-SF-CLASS(GP-SUB) <= END-CLASS
                    ADD 1 TO SUB
                    MOVE GP-PROGRP-SF-CLASS(GP-SUB) TO HOLD-CLASS(SUB).
           ADD 1 TO GP-SUB.
           IF GP-SUB < 5
              GO TO CHECK-GP-SF-CLASSES.

           MOVE BEG-CLASS TO LC-CODE.

           PERFORM START-LC1-FILE.
       FIND-SF-CLASSES.
           PERFORM READ-LC1-FILE-NEXT.
           IF IO-FG = 9
              GO TO EXIT-FIND-CLASSES.

           IF LC-CODE > END-CLASS
              GO TO FIND-SF-CLASSES.
           IF LC-SALEFIN = "N"
              GO TO FIND-SF-CLASSES.
           IF LC-SERVACCT = "Y"
              IF SERVACCT-FG = "S"
                 GO TO FIND-SF-CLASSES.

           IF LC-CODE = HOLD-CLASS(SAVE-SUB + 1)
              MOVE LC-DESC TO CLASS-ELE(SAVE-SUB + 1)
              GO TO FIND-SF-CLASSES
           ELSE IF LC-CODE = HOLD-CLASS(SAVE-SUB + 2)
              MOVE LC-DESC TO CLASS-ELE(SAVE-SUB + 2)
              GO TO FIND-SF-CLASSES
           ELSE IF LC-CODE = HOLD-CLASS(SAVE-SUB + 3)
              MOVE LC-DESC TO CLASS-ELE(SAVE-SUB + 3)
              GO TO FIND-SF-CLASSES
           ELSE IF LC-CODE = HOLD-CLASS(SAVE-SUB + 4)
              MOVE LC-DESC TO CLASS-ELE(SAVE-SUB + 4)
              GO TO FIND-SF-CLASSES.

      * IF LC-PROGRP-CLASS IS SET, THAT CLASS WILL BE GROUPED
      * WITH THE SELECTED CLASS AT TOP OF REPORT, SO SKIP IT:

           IF LC-PROGRP-CLASS NOT = 0
              IF (LC-PROGRP-CLASS = HOLD-CLASS(SAVE-SUB + 1) OR
                  LC-PROGRP-CLASS = HOLD-CLASS(SAVE-SUB + 2) OR
                  LC-PROGRP-CLASS = HOLD-CLASS(SAVE-SUB + 3) OR
                  LC-PROGRP-CLASS = HOLD-CLASS(SAVE-SUB + 4))
                  GO TO FIND-SF-CLASSES.

           IF SUB < 9
              ADD 1 TO SUB
              MOVE LC-CODE TO HOLD-CLASS(SUB)
              MOVE LC-DESC TO CLASS-ELE(SUB)
           ELSE
              MOVE "Y" TO OTHER-SF-CLASSES.
           GO TO FIND-SF-CLASSES.
       EXIT-FIND-CLASSES.
           MOVE SUB TO CLASS-MAX.
           IF OTHER-SF-CLASSES = "Y"
              ADD 1 TO SUB
              MOVE SUB TO OTHER-SF-SUB
              MOVE 0 TO HOLD-CLASS(SUB)
              MOVE "SALES FIN-OTHER" TO CLASS-ELE(SUB).
           ADD 1 TO SUB.
           MOVE SUB TO TOTAL-SF-SUB.
           MOVE 0 TO HOLD-CLASS(SUB).
           MOVE "SALES FIN-TOTAL" TO CLASS-ELE(SUB).
           ADD 1 TO SUB.
           MOVE SUB TO TOTAL-ALL-SUB CLASS-LINE-COUNT.
           MOVE 0 TO HOLD-CLASS(SUB).
           MOVE "GRAND TOTAL" TO CLASS-ELE(SUB).

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
           MOVE DATE-YYYYMM TO CUR-MO-BY-DATE-CCYYMM
                               CUR-MO-DS-DATE-CCYYMM.

           IF DEL-FLAG = "N"
              GO TO EXIT-EO-EXEC-AUDIT.

      * SETUP NEXT MONTH DATE:

           MOVE 1 TO NDTE-DD.
           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO NEXT-MO-BY-DATE-CCYYMM.

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
           IF VDUSTATUS = "1U" GO TO ENTER-INC-P2-P3.

      *----------------------------------------------------------------
       SEND-DEFAULT-DATE.
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
           MOVE STORE-YR TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO CUR-MO-BY-DATE-CCYYMM
                               CUR-MO-DS-DATE-CCYYMM.

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
              MOVE SPACES TO VDUBUF
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
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *----------------------------------------------------------------
       BEGCLS.
           MOVE "ENNN020CLASS1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLCLS.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO BEGCLS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGCLS.
           MOVE VDUNUM0 TO BEG-CLASS LC-CODE.

           IF BEG-CLASS = 0
              GO TO BEGCLS.

           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.

           IF IO-FG = 9
              MOVE "INVALID LOAN CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO BEGCLS.

      *----------------------------------------------------------------
       ENDCLS.
           MOVE "ENNN020CLASS2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLCLS.
           IF VDUSTATUS = "1U" GO TO BEGCLS.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO ENDCLS.
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

           GO TO INCLUDE-SERV-ACCTS.

      *----------------------------------------------------------------
       ALLCLS.
           MOVE "SNNN020CLASS1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020CLASS2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.


      *----------------------------------------------------------------
       INCLUDE-SERV-ACCTS.
           MOVE "E  A010SVACCT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENDCLS.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INCLUDE-SERV-ACCTS.

           MOVE VDUBUF TO SERVACCT-FG.

           IF SERVACCT-FG NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO INCLUDE-SERV-ACCTS.

      *----------------------------------------------------------------
       ENTER-DEL-FLAG.
           MOVE "E  A010DELFLAG." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO INCLUDE-SERV-ACCTS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-DEL-FLAG.

           MOVE VDUBUF TO DEL-FLAG.

           IF DEL-FLAG NOT = "Y" AND NOT = "N"
              GO TO ENTER-DEL-FLAG.

           IF DEL-FLAG = "N"
              GO TO ENTER-INC-P2-P3.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SETUP NEXT MONTH DATE:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

      *----------------------------------------------------------------
       ENTER-INC-P2-P3.
           MOVE "E  A010P2P3." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-DEL-FLAG.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-INC-P2-P3.

           MOVE VDUBUF TO INCLUDE-P2-P3.

           IF INCLUDE-P2-P3 NOT = "Y" AND NOT = "N"
              GO TO ENTER-INC-P2-P3.

      *----------------------------------------------------------------
       ENTER-O2GAIN-FLAG.
      *    DON'T ASK FOR INCLUDE O2 FLAGS, AT LEAST FOR NOW (2/29/96)
           MOVE "Y" TO INC-O2-GAIN.
           GO TO ENTER-O2VOL-FLAG.
           MOVE "E  A010O2GAIN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-DEL-FLAG.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-O2GAIN-FLAG.

           MOVE VDUBUF TO INC-O2-GAIN.

           IF INC-O2-GAIN NOT = "Y" AND NOT = "N"
              GO TO ENTER-O2GAIN-FLAG.

      *----------------------------------------------------------------
       ENTER-O2VOL-FLAG.
      *    DON'T ASK FOR INCLUDE O2 FLAGS, AT LEAST FOR NOW (2/29/96)
           MOVE "N" TO INC-O2-VOL.
           GO TO CHECK-LEVEL-PRINTS.
           MOVE "E  A010O2VOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-O2GAIN-FLAG.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-O2VOL-FLAG.

           MOVE VDUBUF TO INC-O2-VOL.

           IF INC-O2-VOL NOT = "Y" AND NOT = "N"
              GO TO ENTER-O2VOL-FLAG.

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

      *-----------------------------------------------------------------
       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO EXIT-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO EXIT-PARM.

           IF GROUP-FLAG = "Y"
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO EXIT-PARM.
      *-----------------------------------------------------------------
       ASK-LOCAL.
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO MESS.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CHECK-LEVEL-PRINTS.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

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
           MOVE "SNNN020CLASS1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CLASS2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SVACCT." TO VDU.
           MOVE SERVACCT-FG TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010DELFLAG." TO VDU.
           MOVE DEL-FLAG TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010P2P3." TO VDU.
           MOVE INCLUDE-P2-P3 TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
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

      *----------------------------------------------------------------
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
           MOVE "GROUP: " TO HEAD2-TYPE.
           MOVE GR-ID TO H-GRID.
           MOVE GR-NAME TO H-GRNAME.
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
           MOVE "BRANCH: " TO HEAD2-TYPE.
           MOVE BR-NO TO H-BRNO.
           PERFORM GET-BRANCH-NAME.
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
       CHECK-ADD-DS-TOTALS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "N"
              MOVE 9 TO TOT-SUB
              PERFORM ADD-DS-TOTALS
              MOVE 1 TO TOT-SUB
              PERFORM ADD-DS-TOTALS
              GO TO EXIT-CHECK-DS-TOTALS.

           MOVE WGR-TPLUS TO GROUP-SUB.

       CHECK-DS-TOTAL-LOOP.
           IF GROUP-SUB = 0
              GO TO EXIT-CHECK-DS-TOTALS.

           MOVE WGR-SCURR(GROUP-SUB) TO LEVEL-SUB.

           IF WGR-SID(LEVEL-SUB) IS NUMERIC
              MOVE 9 TO TOT-SUB
              GO TO CHECK-DS-TOTAL-FLAG.

           MOVE WGR-SID(LEVEL-SUB) TO GR-ID.
           PERFORM READ-GR1-FILE.
           MOVE GR-LEVEL TO TOT-SUB.

       CHECK-DS-TOTAL-FLAG.
           IF TOTAL-LEVEL-TABLE(TOT-SUB) NOT = "Y"
              GO TO CONTINUE-CHECK-DS-TOTALS.

           PERFORM ADD-DS-TOTALS.

       CONTINUE-CHECK-DS-TOTALS.
           SUBTRACT 1 FROM GROUP-SUB.

           IF GROUP-SUB > 0
              GO TO CHECK-DS-TOTAL-LOOP.

       EXIT-CHECK-DS-TOTALS.
           EXIT.

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

           IF LOCAL-PRINT-FG = "Y"
              MOVE 0 TO PAGE-NUMBER
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 GO TO EXIT-PRINT-ONE-BRANCH.
           MOVE 9 TO TOT-SUB.
           MOVE "BRANCH: " TO HEAD2-TYPE.
           MOVE BR-NO TO H-BRNO.
           PERFORM GET-BRANCH-NAME.
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

           IF LOCAL-PRINT-FG = "Y"
              GO TO EXIT-PRINT-ONE-GRAND.

           MOVE 1 TO TOT-SUB.
           MOVE " " TO HEAD2-TYPE.
           MOVE 0 TO H-BRNO.
           MOVE "GRAND TOTALS" TO H-GRNAME.
           PERFORM HEAD-PAGE.
           PERFORM PRINT-TOTALS.

       EXIT-PRINT-ONE-GRAND.
           EXIT.

      ******************************************************************
       ADD-DS-TOTALS SECTION.
      ******************************************************************
           MOVE DS-CLASS TO LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-FG = 9
              MOVE "N" TO LC-SALEFIN LC-SERVACCT.

           IF LC-SALEFIN = "N"
              GO TO EXIT-ADD-DS-TOTALS.

           IF LC-SERVACCT = "Y"
              IF SERVACCT-FG = "S"
                 GO TO EXIT-ADD-DS-TOTALS.

           IF LC-SERVACCT = "N"
              IF SERVACCT-FG = "O"
                 GO TO EXIT-ADD-DS-TOTALS.

      *----------------------------------------------------------------
       GET-DS-DEALER.
           PERFORM OPEN-DL1-FILE.
           MOVE DS-DLNO TO DL-DLNO.
           PERFORM READ-DL1-FILE.

           IF IO-FG = 9
              MOVE "NOT ON FILE!" TO DL-NAME
              MOVE SPACES TO DL-MFGCODE.

           PERFORM CLOSE-DL1-FILE.

      *----------------------------------------------------------------
       CREATE-WORK-REC.
           MOVE DS-BRANCH TO WK-BRANCH.
           MOVE DS-DLNO TO WK-DLNO.
           MOVE TOT-SUB TO WK-LEVEL.
           PERFORM READ-WK-FILE.

           IF IO-FG = 0
              GO TO UPDATE-WORK-REC.

           MOVE DS-BRANCH TO WK-BRANCH.
           MOVE DS-DLNO TO WK-DLNO.
           MOVE TOT-SUB TO WK-LEVEL WK-LEVEL2.
           MOVE DL-NAME TO WK-DL-NAME.
           MOVE DL-MFGCODE TO WK-DL-MFGCODE.

           MOVE 0 TO WK-OUTSUNIT WK-OUTSAMT WK-MTD-PUR-NO
                     WK-MTD-PUR-AMT WK-YTD-PUR-NO
                     WK-YTD-PUR-AMT WK-MTD-TODL
                     WK-YTD-TODL WK-YTD-TODL-AMT.

           PERFORM WRITE-WK-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

      *----------------------------------------------------------------
       UPDATE-WORK-REC.
           COMPUTE WK-OUTSUNIT = WK-OUTSUNIT + DS-OUTSUNIT(1).
           COMPUTE WK-OUTSAMT = WK-OUTSAMT + DS-OUTSAMT(1).

           COMPUTE WK-MTD-PUR-NO = WK-MTD-PUR-NO +
                                   DS-NPURUNIT(1) + DS-OPURUNIT(1).

           COMPUTE WK-MTD-PUR-AMT = WK-MTD-PUR-AMT +
                                    DS-NPURAMT(1) + DS-OPURAMT(1).

           COMPUTE WK-YTD-PUR-NO = WK-YTD-PUR-NO +
                                   DS-NPURUNIT(2) + DS-OPURUNIT(2).

           COMPUTE WK-YTD-PUR-AMT = WK-YTD-PUR-AMT +
                                    DS-NPURAMT(2) + DS-OPURAMT(2).

           COMPUTE WK-MTD-TODL = WK-MTD-TODL + DS-TODLUNIT(1).
           COMPUTE WK-YTD-TODL = WK-YTD-TODL + DS-TODLUNIT(2).
           COMPUTE WK-YTD-TODL-AMT = WK-YTD-TODL-AMT + DS-TODLAMT(2).
           PERFORM REWRITE-WK-FILE.

       EXIT-ADD-DS-TOTALS.
           EXIT.

      ******************************************************************
       ADD-TOTALS SECTION.
      ******************************************************************
           MOVE BY-CLASS TO LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-FG = 9
              MOVE "N" TO LC-SALEFIN LC-SERVACCT
              MOVE OTHER-LN-SUB TO CLASS-SUB
              GO TO EXIT-FIND-CLASS-SUB.

           IF LC-SERVACCT = "Y"
              IF SERVACCT-FG = "S"
                 GO TO EXIT-ADD-TOTALS.

           IF LC-SERVACCT = "N"
              IF SERVACCT-FG = "O"
                 GO TO EXIT-ADD-TOTALS.

           MOVE 1 TO SUB.
       FIND-CLASS-SUB.
           IF BY-CLASS = HOLD-CLASS(SUB)
              MOVE SUB TO CLASS-SUB
              GO TO EXIT-FIND-CLASS-SUB.
           IF LC-PROGRP-CLASS NOT = 0
              IF LC-PROGRP-CLASS = HOLD-CLASS(SUB)
                 MOVE SUB TO CLASS-SUB
                 GO TO EXIT-FIND-CLASS-SUB.
           IF SUB < CLASS-MAX
              ADD 1 TO SUB
              GO TO FIND-CLASS-SUB.
           IF LC-SALEFIN = "Y"
              MOVE OTHER-SF-SUB TO CLASS-SUB
           ELSE
              MOVE OTHER-LN-SUB TO CLASS-SUB.
           IF CLASS-SUB = 0
              MOVE 1 TO CLASS-SUB.

       EXIT-FIND-CLASS-SUB.
           IF LC-SALEFIN = "Y"
              MOVE TOTAL-SF-SUB TO SUBTOTAL-SUB
           ELSE
              MOVE TOTAL-LN-SUB TO SUBTOTAL-SUB.

      *----------------------------------------------------------------
       ADD-TOTS.
           MOVE 1 TO TYPE-SUB.

      *----------------------------------------------------------------
       TYPE-LOOP.
           IF TYPE-SUB = 1 OR 9
              MOVE 2 TO SUB
           ELSE IF TYPE-SUB = 2
              MOVE 6 TO SUB
           ELSE IF TYPE-SUB = 3
              MOVE 4 TO SUB
           ELSE IF TYPE-SUB = 4 OR 5 OR 6
              MOVE 3 TO SUB
           ELSE IF TYPE-SUB = 7
              MOVE 5 TO SUB
           ELSE IF TYPE-SUB = 8
              MOVE 1 TO SUB
           ELSE IF TYPE-SUB = 10
              MOVE 7 TO SUB
           ELSE IF TYPE-SUB = 11
              MOVE 8 TO SUB
           ELSE
              MOVE 2 TO SUB.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SECTION 1 & 2: GAIN/LOSS/VOL BY TYPE, CLASS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           ADD BY-NO(1, 2, TYPE-SUB) TO
               TYPE-VOL-NO-MTD(TOT-SUB, SUB)
               TYPE-VOL-NO-MTD(TOT-SUB, 9).

           ADD BY-AMT(1, 2, TYPE-SUB) TO
               TYPE-VOL-AMT-MTD(TOT-SUB, SUB)
               TYPE-VOL-AMT-MTD(TOT-SUB, 9).

           IF INC-O2-VOL = "N"
              SUBTRACT BY-O2(1, 2, TYPE-SUB) FROM
               TYPE-VOL-AMT-MTD(TOT-SUB, SUB)
               TYPE-VOL-AMT-MTD(TOT-SUB, 9).

           ADD BY-NO(2, 2, TYPE-SUB) TO
               TYPE-VOL-NO-YTD(TOT-SUB, SUB)
               TYPE-VOL-NO-YTD(TOT-SUB, 9).

           ADD BY-AMT(2, 2, TYPE-SUB) TO
               TYPE-VOL-AMT-YTD(TOT-SUB, SUB)
               TYPE-VOL-AMT-YTD(TOT-SUB, 9).

           IF INC-O2-VOL = "N"
              SUBTRACT BY-O2(2, 2, TYPE-SUB) FROM
               TYPE-VOL-AMT-YTD(TOT-SUB, SUB)
               TYPE-VOL-AMT-YTD(TOT-SUB, 9).

      *----------------------------------------------------------------
       ADD-CLASS-TOTALS.
           ADD BY-NO(1, 1, TYPE-SUB) TO
               CLASS-GAIN-NO-CUR(TOT-SUB, CLASS-SUB)
               CLASS-GAIN-NO-CUR(TOT-SUB, SUBTOTAL-SUB)
               CLASS-GAIN-NO-CUR(TOT-SUB, TOTAL-ALL-SUB).

           ADD BY-AMT(1, 1, TYPE-SUB) TO
               CLASS-GAIN-AMT-CUR(TOT-SUB, CLASS-SUB)
               CLASS-GAIN-AMT-CUR(TOT-SUB, SUBTOTAL-SUB)
               CLASS-GAIN-AMT-CUR(TOT-SUB, TOTAL-ALL-SUB).

           IF INC-O2-GAIN = "N"
              SUBTRACT BY-O2(1, 1, TYPE-SUB) FROM
               CLASS-GAIN-AMT-CUR(TOT-SUB, CLASS-SUB)
               CLASS-GAIN-AMT-CUR(TOT-SUB, SUBTOTAL-SUB)
               CLASS-GAIN-AMT-CUR(TOT-SUB, TOTAL-ALL-SUB).

           ADD BY-NO(2, 1, TYPE-SUB) TO
               CLASS-GAIN-NO-ELM(TOT-SUB, CLASS-SUB)
               CLASS-GAIN-NO-ELM(TOT-SUB, SUBTOTAL-SUB)
               CLASS-GAIN-NO-ELM(TOT-SUB, TOTAL-ALL-SUB).

           ADD BY-AMT(2, 1, TYPE-SUB) TO
               CLASS-GAIN-AMT-ELM(TOT-SUB, CLASS-SUB)
               CLASS-GAIN-AMT-ELM(TOT-SUB, SUBTOTAL-SUB)
               CLASS-GAIN-AMT-ELM(TOT-SUB, TOTAL-ALL-SUB).

           IF INC-O2-GAIN = "N"
              SUBTRACT BY-O2(2, 1, TYPE-SUB) FROM
               CLASS-GAIN-AMT-ELM(TOT-SUB, CLASS-SUB)
               CLASS-GAIN-AMT-ELM(TOT-SUB, SUBTOTAL-SUB)
               CLASS-GAIN-AMT-ELM(TOT-SUB, TOTAL-ALL-SUB).

           ADD BY-NO(3, 1, TYPE-SUB) TO
               CLASS-GAIN-NO-ELY(TOT-SUB, CLASS-SUB)
               CLASS-GAIN-NO-ELY(TOT-SUB, SUBTOTAL-SUB)
               CLASS-GAIN-NO-ELY(TOT-SUB, TOTAL-ALL-SUB).

           ADD BY-AMT(3, 1, TYPE-SUB) TO
               CLASS-GAIN-AMT-ELY(TOT-SUB, CLASS-SUB)
               CLASS-GAIN-AMT-ELY(TOT-SUB, SUBTOTAL-SUB)
               CLASS-GAIN-AMT-ELY(TOT-SUB, TOTAL-ALL-SUB).

           IF INC-O2-GAIN = "N"
              SUBTRACT BY-O2(3, 1, TYPE-SUB) FROM
               CLASS-GAIN-AMT-ELY(TOT-SUB, CLASS-SUB)
               CLASS-GAIN-AMT-ELY(TOT-SUB, SUBTOTAL-SUB)
               CLASS-GAIN-AMT-ELY(TOT-SUB, TOTAL-ALL-SUB).

           ADD BY-AMT(1, 2, TYPE-SUB) TO
               CLASS-VOL-AMT-MTD(TOT-SUB, CLASS-SUB)
               CLASS-VOL-AMT-MTD(TOT-SUB, SUBTOTAL-SUB)
               CLASS-VOL-AMT-MTD(TOT-SUB, TOTAL-ALL-SUB).

           IF INC-O2-VOL = "N"
              SUBTRACT BY-O2(1, 2, TYPE-SUB) FROM
               CLASS-VOL-AMT-MTD(TOT-SUB, CLASS-SUB)
               CLASS-VOL-AMT-MTD(TOT-SUB, SUBTOTAL-SUB)
               CLASS-VOL-AMT-MTD(TOT-SUB, TOTAL-ALL-SUB).

           ADD BY-AMT(2, 2, TYPE-SUB) TO
               CLASS-VOL-AMT-YTD(TOT-SUB, CLASS-SUB)
               CLASS-VOL-AMT-YTD(TOT-SUB, SUBTOTAL-SUB)
               CLASS-VOL-AMT-YTD(TOT-SUB, TOTAL-ALL-SUB).

           IF INC-O2-VOL = "N"
              SUBTRACT BY-O2(2, 2, TYPE-SUB) FROM
               CLASS-VOL-AMT-YTD(TOT-SUB, CLASS-SUB)
               CLASS-VOL-AMT-YTD(TOT-SUB, SUBTOTAL-SUB)
               CLASS-VOL-AMT-YTD(TOT-SUB, TOTAL-ALL-SUB).

           ADD 1 TO TYPE-SUB.

           IF TYPE-SUB < 12
              GO TO TYPE-LOOP.

      *----------------------------------------------------------------
       ADD-MISC-TOTALS.
           ADD BY-LATE-NO-COL(1) TO LATE-NO-COL(TOT-SUB).
           ADD BY-LATE-NO-WAIVED(1) TO LATE-NO-WAIVED(TOT-SUB).
           ADD BY-LATECOL(1) TO LATE-AMT-COL(TOT-SUB).
           ADD BY-LATE-AMT-WAIVED(1) TO LATE-AMT-WAIVED(TOT-SUB).

           ADD BY-PRINCOL(1) TO
                  CLASS-PRINCOL-MTD(TOT-SUB, CLASS-SUB)
                  CLASS-PRINCOL-MTD(TOT-SUB, SUBTOTAL-SUB)
                  CLASS-PRINCOL-MTD(TOT-SUB, TOTAL-ALL-SUB).

      *    IF INC-O2-VOL = "N"
      *       SUBTRACT BY-OT2COL(1) FROM
      *           CLASS-PRINCOL-MTD(TOT-SUB, CLASS-SUB)
      *           CLASS-PRINCOL-MTD(TOT-SUB, SUBTOTAL-SUB)
      *           CLASS-PRINCOL-MTD(TOT-SUB, TOTAL-ALL-SUB).

           COMPUTE DLRV-GAIN-CUR(TOT-SUB) =
                   DLRV-GAIN-CUR(TOT-SUB) +
                   BY-DLRVPRIN(1, 1, 1) +
                   BY-DLRVPRIN(1, 1, 2) +
                   BY-DLRVPRIN(1, 1, 3) +
                   BY-DLRVCHGS(1, 1, 1) +
                   BY-DLRVCHGS(1, 1, 2) +
                   BY-DLRVCHGS(1, 1, 3) +
                   BY-DLPARVHELD(1, 1).

           COMPUTE DLRV-GAIN-ELM(TOT-SUB) =
                   DLRV-GAIN-ELM(TOT-SUB) +
                   BY-DLRVPRIN(2, 1, 1) +
                   BY-DLRVPRIN(2, 1, 2) +
                   BY-DLRVPRIN(2, 1, 3) +
                   BY-DLRVCHGS(2, 1, 1) +
                   BY-DLRVCHGS(2, 1, 2) +
                   BY-DLRVCHGS(2, 1, 3) +
                   BY-DLPARVHELD(2, 1).

           ADD BY-DLDISC(1) TO DLDISC-MTD-VOL(TOT-SUB).

           IF LC-SALEFIN = "Y"
              ADD BY-PART-CHGOFF(1) TO PART-CHGOFF-MTD(TOT-SUB,2)
              ADD BY-PART-CHGOFF(2) TO PART-CHGOFF-YTD(TOT-SUB,2)
           ELSE
              ADD BY-PART-CHGOFF(1) TO PART-CHGOFF-MTD(TOT-SUB,1)
              ADD BY-PART-CHGOFF(2) TO PART-CHGOFF-YTD(TOT-SUB,1).
           ADD BY-PART-CHGOFF(1) TO PART-CHGOFF-MTD(TOT-SUB,3).
           ADD BY-PART-CHGOFF(2) TO PART-CHGOFF-YTD(TOT-SUB,3).

           MOVE 1 TO PL-SUB PLA-SUB.
           ADD 1 TO PL-SUB GIVING PLI-SUB.
           IF LC-SALEFIN = "Y"
              MOVE 2 TO SUB
           ELSE
              MOVE 1 TO SUB.

       ADD-PL-P2-P3-CHGOFFS.

           COMPUTE PL-VOL-AMT-MTD(TOT-SUB, SUB) =
                   PL-VOL-AMT-MTD(TOT-SUB, SUB)
                 + BY-AMT-PL(1, 2, PLA-SUB).
      *          + BY-AMT-PL(1, 2, PLI-SUB).
           COMPUTE PL-VOL-AMT-YTD(TOT-SUB, SUB) =
                   PL-VOL-AMT-YTD(TOT-SUB, SUB)
                 + BY-AMT-PL(2, 2, PLA-SUB).
      *          + BY-AMT-PL(2, 2, PLI-SUB).

      *     2/29/96 INCLUDE O2 IN PL VOL FOR MERCURY
      *    IF INC-O2-VOL = "N"
      *       COMPUTE PL-VOL-AMT-MTD(TOT-SUB, SUB) =
      *               PL-VOL-AMT-MTD(TOT-SUB, SUB) -
      *               BY-O2-PL(1, 2, PLA-SUB) -
      *               BY-O2-PL(1, 2, PLI-SUB)
      *       COMPUTE PL-VOL-AMT-YTD(TOT-SUB, SUB) =
      *               PL-VOL-AMT-YTD(TOT-SUB, SUB) -
      *               BY-O2-PL(2, 2, PLA-SUB) -
      *               BY-O2-PL(2, 2, PLI-SUB).

           COMPUTE PL-RECV-AMT-MTD(TOT-SUB, SUB) =
                   PL-RECV-AMT-MTD(TOT-SUB, SUB) +
                   BY-PL-PRINCOL(1, PL-SUB) +
                   BY-PL-INTCOL(1, PL-SUB) +
                   BY-PL-LATECOL(1, PL-SUB).
           COMPUTE PL-RECV-AMT-YTD(TOT-SUB, SUB) =
                   PL-RECV-AMT-YTD(TOT-SUB, SUB) +
                   BY-PL-PRINCOL(2, PL-SUB) +
                   BY-PL-INTCOL(2, PL-SUB) +
                   BY-PL-LATECOL(2, PL-SUB).

      *     2/29/96 INCLUDE O2 IN PL VOL FOR MERCURY
      *    IF INC-O2-VOL = "N"
      *       COMPUTE PL-RECV-AMT-MTD(TOT-SUB, SUB) =
      *               PL-RECV-AMT-MTD(TOT-SUB, SUB) -
      *               BY-PL-O2COL(1, PL-SUB)
      *       COMPUTE PL-RECV-AMT-YTD(TOT-SUB, SUB) =
      *               PL-RECV-AMT-YTD(TOT-SUB, SUB) -
      *               BY-PL-O2COL(2, PL-SUB).

           IF INCLUDE-P2-P3 = "N"
              GO TO SKIP-P2-P3.

           ADD 1 TO PL-SUB.
           ADD 2 TO PLA-SUB PLI-SUB.
           IF PL-SUB < 4
              GO TO ADD-PL-P2-P3-CHGOFFS.

       SKIP-P2-P3.
      * GO BACK THROUGH LOOP TO COLLECT TOTAL OF LOAN & SF PL'S
           IF SUB < 3
              MOVE 3 TO SUB
              MOVE 1 TO PL-SUB PLA-SUB
              ADD 1 TO PL-SUB GIVING PLI-SUB
              GO TO ADD-PL-P2-P3-CHGOFFS.

           MOVE 4 TO PL-SUB.
           MOVE 7 TO PLA-SUB.
           MOVE 8 TO PLI-SUB.
       ADD-P4-P5-LOOP.
           IF PL-SUB = 4 MOVE 1 TO SUB
           ELSE
              IF PL-SUB = 5 MOVE 2 TO SUB
              ELSE GO TO EXIT-P4-P5-LOOP.

           COMPUTE P45-VOL-NO-MTD(TOT-SUB, SUB) =
                   P45-VOL-NO-MTD(TOT-SUB, SUB)
                 + BY-NO-PL(1, 2, PLA-SUB).
      *          + BY-NO-PL(1, 2, PLI-SUB).
           COMPUTE P45-VOL-NO-YTD(TOT-SUB, SUB) =
                   P45-VOL-NO-YTD(TOT-SUB, SUB)
                 + BY-NO-PL(2, 2, PLA-SUB).
      *          + BY-NO-PL(2, 2, PLI-SUB).
           COMPUTE P45-VOL-PRIN-MTD(TOT-SUB, SUB) =
                   P45-VOL-PRIN-MTD(TOT-SUB, SUB)
                 + BY-AMT-PL(1, 2, PLA-SUB)
                 - BY-O2-PL(1, 2, PLA-SUB).
      *          + BY-AMT-PL(1, 2, PLI-SUB)
      *          - BY-O2-PL(1, 2, PLI-SUB).
           COMPUTE P45-VOL-TOT-MTD(TOT-SUB, SUB) =
                   P45-VOL-TOT-MTD(TOT-SUB, SUB)
                 + BY-AMT-PL(1, 2, PLA-SUB).
      *          + BY-AMT-PL(1, 2, PLI-SUB).
           COMPUTE P45-VOL-PRIN-YTD(TOT-SUB, SUB) =
                   P45-VOL-PRIN-YTD(TOT-SUB, SUB)
                 + BY-AMT-PL(2, 2, PLA-SUB)
                 - BY-O2-PL(2, 2, PLA-SUB).
      *          + BY-AMT-PL(2, 2, PLI-SUB)
      *          - BY-O2-PL(2, 2, PLI-SUB).
           COMPUTE P45-VOL-TOT-YTD(TOT-SUB, SUB) =
                   P45-VOL-TOT-YTD(TOT-SUB, SUB)
                 + BY-AMT-PL(2, 2, PLA-SUB).
      *          + BY-AMT-PL(2, 2, PLI-SUB).
           COMPUTE P45-VOL-TOT-LYR(TOT-SUB, SUB) =
                   P45-VOL-TOT-LYR(TOT-SUB, SUB)
                 + BY-AMT-PL(3, 2, PLA-SUB).
      *          + BY-AMT-PL(3, 2, PLI-SUB).
           COMPUTE P45-VOL-O2-MTD(TOT-SUB, SUB) =
                   P45-VOL-O2-MTD(TOT-SUB, SUB)
                 + BY-O2-PL(1, 2, PLA-SUB).
      *          + BY-O2-PL(1, 2, PLI-SUB).
           COMPUTE P45-VOL-O2-YTD(TOT-SUB, SUB) =
                   P45-VOL-O2-YTD(TOT-SUB, SUB)
                 + BY-O2-PL(2, 2, PLA-SUB).
      *          + BY-O2-PL(2, 2, PLI-SUB).

           COMPUTE P45-RECV-PRIN-MTD(TOT-SUB, SUB) =
                   P45-RECV-PRIN-MTD(TOT-SUB, SUB) +
                   BY-PL-PRINCOL(1, PL-SUB) +
                   BY-PL-INTCOL(1, PL-SUB) +
                   BY-PL-LATECOL(1, PL-SUB) -
                   BY-PL-O2COL(1, PL-SUB).
           COMPUTE P45-RECV-TOT-MTD(TOT-SUB, SUB) =
                   P45-RECV-TOT-MTD(TOT-SUB, SUB) +
                   BY-PL-PRINCOL(1, PL-SUB) +
                   BY-PL-INTCOL(1, PL-SUB) +
                   BY-PL-LATECOL(1, PL-SUB).
           COMPUTE P45-RECV-PRIN-YTD(TOT-SUB, SUB) =
                   P45-RECV-PRIN-YTD(TOT-SUB, SUB) +
                   BY-PL-PRINCOL(2, PL-SUB) +
                   BY-PL-INTCOL(2, PL-SUB) +
                   BY-PL-LATECOL(2, PL-SUB) -
                   BY-PL-O2COL(2, PL-SUB).
           COMPUTE P45-RECV-TOT-YTD(TOT-SUB, SUB) =
                   P45-RECV-TOT-YTD(TOT-SUB, SUB) +
                   BY-PL-PRINCOL(2, PL-SUB) +
                   BY-PL-INTCOL(2, PL-SUB) +
                   BY-PL-LATECOL(2, PL-SUB).
           COMPUTE P45-RECV-O2-MTD(TOT-SUB, SUB) =
                   P45-RECV-O2-MTD(TOT-SUB, SUB) +
                   BY-PL-O2COL(1, PL-SUB).
           COMPUTE P45-RECV-O2-YTD(TOT-SUB, SUB) =
                   P45-RECV-O2-YTD(TOT-SUB, SUB) +
                   BY-PL-O2COL(2, PL-SUB).

           ADD 1 TO PL-SUB.
           ADD 2 TO PLA-SUB PLI-SUB.
           IF PL-SUB < 6
              GO TO ADD-P4-P5-LOOP.
       EXIT-P4-P5-LOOP.
           EXIT.
      *----------------------------------------------------------------

       ADD-INS-ROUTINE.
           MOVE 1 TO INS-SUB.
      *----------------------------------------------------------------

       ADD-INS-LOOP.
           ADD BY-INS-NO-ELIG(1, INS-SUB) TO
                                     INS-NO-ELIG(TOT-SUB, INS-SUB).

           ADD BY-INS-NO-WROTE(1, INS-SUB) TO
                                    INS-NO-WROTE(TOT-SUB, INS-SUB).

           ADD BY-INS-NO-NOTWROTE(1, INS-SUB) TO
                                 INS-NO-NOTWROTE(TOT-SUB, INS-SUB).

           ADD BY-INS-AMT-WROTE(1, INS-SUB) TO
                                  INS-AMT-WROTE(TOT-SUB, INS-SUB).

           ADD 1 TO INS-SUB.


           IF INS-SUB < 11 GO TO ADD-INS-LOOP.


      * NONFILE INS-NO-WROTE(1,11) AD&D (1,12), CARCLUB (1,13)
      *THIS WILL LOAD THE THE CAR CLUB AND AD&D TO THE WORKING
      *STORAGE INS-POL-TABLE WHICH WAS INCREASED TO 13 FROM 10.
           PERFORM VARYING FEE-SUB FROM 1 BY 1 UNTIL FEE-SUB > 3
               ADD BY-FEE-NO-ELIG(1, FEE-SUB) TO
                              INS-NO-ELIG(TOT-SUB, FEE-SUB + 10)
               ADD BY-FEE-NO-WROTE(1, FEE-SUB) TO
                             INS-NO-WROTE(TOT-SUB, FEE-SUB + 10)
               ADD BY-FEE-NO-NOTWROTE(1, FEE-SUB) TO
                          INS-NO-NOTWROTE(TOT-SUB, FEE-SUB + 10)
               ADD BY-FEE-AMT-WROTE(1, FEE-SUB) TO
                            INS-AMT-WROTE(TOT-SUB, FEE-SUB + 10)
           END-PERFORM.


           ADD BY-CLREB1(1) BY-CLREB2(1) BY-CLREB3(1) BY-CLREB4(1)
               TO INS-AMT-REFUND(TOT-SUB, 1).


           ADD BY-AHREB(1) TO INS-AMT-REFUND(TOT-SUB, 2).
           ADD BY-PPREB(1) TO INS-AMT-REFUND(TOT-SUB, 3).
           ADD BY-O1REB(1) TO INS-AMT-REFUND(TOT-SUB, 4).
           ADD BY-O2REB(1) TO INS-AMT-REFUND(TOT-SUB, 5).
           ADD BY-O3REB(1) TO INS-AMT-REFUND(TOT-SUB, 6).

           MOVE 1 TO REPO-SUB.

      *----------------------------------------------------------------
       ADD-REPO-LOOP.

           ADD BY-NO-REPO(1, 1, REPO-SUB) TO
               REPO-GAIN-NO-CUR(TOT-SUB, REPO-SUB)
               REPO-GAIN-NO-MTD(TOT-SUB, REPO-SUB)
               REPO-GAIN-NO-YTD(TOT-SUB, REPO-SUB).
           ADD BY-AMT-REPO(1, 1, REPO-SUB) TO
               REPO-GAIN-AMT-CUR(TOT-SUB, REPO-SUB)
               REPO-GAIN-AMT-MTD(TOT-SUB, REPO-SUB)
               REPO-GAIN-AMT-YTD(TOT-SUB, REPO-SUB).

           IF INC-O2-GAIN = "N"
              SUBTRACT BY-O2-REPO(1, 1, REPO-SUB) FROM
                 REPO-GAIN-AMT-CUR(TOT-SUB, REPO-SUB)
                 REPO-GAIN-AMT-MTD(TOT-SUB, REPO-SUB)
                 REPO-GAIN-AMT-YTD(TOT-SUB, REPO-SUB).

           SUBTRACT BY-NO-REPO(2, 1, REPO-SUB) FROM
               REPO-GAIN-NO-MTD(TOT-SUB, REPO-SUB).
           SUBTRACT BY-AMT-REPO(2, 1, REPO-SUB) FROM
               REPO-GAIN-AMT-MTD(TOT-SUB, REPO-SUB).
           IF INC-O2-GAIN = "N"
              ADD BY-O2-REPO(2, 1, REPO-SUB) TO
               REPO-GAIN-AMT-MTD(TOT-SUB, REPO-SUB).
           SUBTRACT BY-NO-REPO(3, 1, REPO-SUB) FROM
               REPO-GAIN-NO-YTD(TOT-SUB, REPO-SUB).
           SUBTRACT BY-AMT-REPO(3, 1, REPO-SUB) FROM
               REPO-GAIN-AMT-YTD(TOT-SUB, REPO-SUB).
           IF INC-O2-GAIN = "N"
              ADD BY-O2-REPO(3, 1, REPO-SUB) TO
               REPO-GAIN-AMT-YTD(TOT-SUB, REPO-SUB).

           ADD BY-NO-REPO(1, 2, REPO-SUB) TO
               REPO-VOL-NO-MTD(TOT-SUB, REPO-SUB).
           ADD BY-AMT-REPO(1, 2, REPO-SUB) TO
               REPO-VOL-AMT-MTD(TOT-SUB, REPO-SUB).
      *    IF INC-O2-VOL = "N"
      *       SUBTRACT BY-O2-REPO(1, 2, REPO-SUB) FROM
      *        REPO-VOL-AMT-MTD(TOT-SUB, REPO-SUB).
           ADD BY-NO-REPO(2, 2, REPO-SUB) TO
               REPO-VOL-NO-YTD(TOT-SUB, REPO-SUB).
           ADD BY-AMT-REPO(2, 2, REPO-SUB) TO
               REPO-VOL-AMT-YTD(TOT-SUB, REPO-SUB).
      *    IF INC-O2-VOL = "N"
      *       SUBTRACT BY-O2-REPO(2, 2, REPO-SUB) FROM
      *        REPO-VOL-AMT-YTD(TOT-SUB, REPO-SUB).
           ADD BY-NO-REPO(3, 2, REPO-SUB) TO
               REPO-VOL-NO-LYR(TOT-SUB, REPO-SUB).
           ADD BY-AMT-REPO(3, 2, REPO-SUB) TO
               REPO-VOL-AMT-LYR(TOT-SUB, REPO-SUB).
      *    IF INC-O2-VOL = "N"
      *       SUBTRACT BY-O2-REPO(3, 2, REPO-SUB) FROM
      *        REPO-VOL-AMT-LYR(TOT-SUB, REPO-SUB).

           ADD 1 TO REPO-SUB.
           IF REPO-SUB < 4 GO TO ADD-REPO-LOOP.

           IF DEL-FLAG = "Y"
              PERFORM GET-BEG-MONTH-DELINQUENCY
              GO TO EXIT-ADD-TOTALS.

           MOVE 1 TO AGE-SUB.

           IF LC-SALEFIN = "Y"
              GO TO ADD-DELINQUENT-SALEFIN.

      *----------------------------------------------------------------
       ADD-DELINQUENT-LOANS.
           ADD BY-UNITS(AGE-SUB,2,1) TO
                                     LOAN-DEL-NO(TOT-SUB,AGE-SUB,1).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-UNITS-BNKRPT(AGE-SUB,2,1) FROM
      *                              LOAN-DEL-NO(TOT-SUB,AGE-SUB,1).

           ADD BY-UNITS(AGE-SUB,2,2) TO
                                     LOAN-DEL-NO(TOT-SUB,AGE-SUB,2).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-UNITS-BNKRPT(AGE-SUB,2,2) FROM
      *                              LOAN-DEL-NO(TOT-SUB,AGE-SUB,2).

      * ADD CONTRACTUAL 90, 120, 150, 180, 210 TO BUCKET 3 (90+)
           ADD BY-UNITS(AGE-SUB,2,3)
               BY-UNITS(AGE-SUB,2,4)
               BY-UNITS(AGE-SUB,2,5)
               BY-UNITS(AGE-SUB,2,6)
               BY-UNITS(AGE-SUB,2,7) TO
                                     LOAN-DEL-NO(TOT-SUB,AGE-SUB,3).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-UNITS-BNKRPT(AGE-SUB,2,3)
      *                BY-UNITS-BNKRPT(AGE-SUB,2,4)
      *                BY-UNITS-BNKRPT(AGE-SUB,2,5)
      *                BY-UNITS-BNKRPT(AGE-SUB,2,6)
      *                BY-UNITS-BNKRPT(AGE-SUB,2,7) FROM
      *                              LOAN-DEL-NO(TOT-SUB,AGE-SUB,3).

      * BUCKET 4 POT CHG-OFFS USE RECENCY NOT CONTRACTUAL FOR LNS
      * IF GP-PROGRP-DL-CONY = "Y" THEN USE CONTRACTUAL INSTEAD OF RECENCY
      *                                      MJD REQ#525 (CUMBERLAND)
           IF GP-PROGRP-DL-CONT = "Y"
              ADD BY-UNITS(AGE-SUB,2,5)
                  BY-UNITS(AGE-SUB,2,6)
                  BY-UNITS(AGE-SUB,2,7) TO
                                     LOAN-DEL-NO(TOT-SUB,AGE-SUB,4)
           ELSE
              ADD BY-UNITS(AGE-SUB,1,5)
                  BY-UNITS(AGE-SUB,1,6)
                  BY-UNITS(AGE-SUB,1,7) TO
                                     LOAN-DEL-NO(TOT-SUB,AGE-SUB,4).
      *    IF (GP-I-AM-MERC)
      *       IF GP-PROGRP-DL-CONT = "Y"
      *          SUBTRACT BY-UNITS-BNKRPT(AGE-SUB,2,5)
      *                   BY-UNITS-BNKRPT(AGE-SUB,2,6)
      *                   BY-UNITS-BNKRPT(AGE-SUB,2,7) FROM
      *                              LOAN-DEL-NO(TOT-SUB,AGE-SUB,4)
      *       ELSE
      *          SUBTRACT BY-UNITS-BNKRPT(AGE-SUB,1,5)
      *                   BY-UNITS-BNKRPT(AGE-SUB,1,6)
      *                   BY-UNITS-BNKRPT(AGE-SUB,1,7) FROM
      *                              LOAN-DEL-NO(TOT-SUB,AGE-SUB,4).

           ADD BY-UNITS(AGE-SUB,2,9) TO
                                     LOAN-DEL-NO(TOT-SUB,AGE-SUB,5).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-UNITS-BNKRPT(AGE-SUB,2,9) FROM
      *                              LOAN-DEL-NO(TOT-SUB,AGE-SUB,5)
      *       ADD BY-UNITS(AGE-SUB,2,10) TO
      *                              LOAN-DEL-NO(TOT-SUB,AGE-SUB,5)
      *       SUBTRACT BY-UNITS-BNKRPT(AGE-SUB,2,10) FROM
      *                              LOAN-DEL-NO(TOT-SUB,AGE-SUB,5)
      *       SUBTRACT BY-OUTBAL-BNKRPT(AGE-SUB,2,1) FROM
      *                                LOAN-DEL-AMT(TOT-SUB,AGE-SUB,1)
      *       SUBTRACT BY-OUTBAL-BNKRPT(AGE-SUB,2,2) FROM
      *                                LOAN-DEL-AMT(TOT-SUB,AGE-SUB,2).

           ADD BY-OUTBAL(AGE-SUB,2,1) TO
                                       LOAN-DEL-AMT(TOT-SUB,AGE-SUB,1).

           ADD BY-OUTBAL(AGE-SUB,2,2) TO
                                       LOAN-DEL-AMT(TOT-SUB,AGE-SUB,2).

      * ADD CONTRACTUAL 90, 120, 150, 180, 210 TO BUCKET 3 (90+)
           ADD BY-OUTBAL(AGE-SUB,2,3)
               BY-OUTBAL(AGE-SUB,2,4)
               BY-OUTBAL(AGE-SUB,2,5)
               BY-OUTBAL(AGE-SUB,2,6)
               BY-OUTBAL(AGE-SUB,2,7) TO
                                       LOAN-DEL-AMT(TOT-SUB,AGE-SUB,3).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-OUTBAL-BNKRPT(AGE-SUB,2,3)
      *                BY-OUTBAL-BNKRPT(AGE-SUB,2,4)
      *                BY-OUTBAL-BNKRPT(AGE-SUB,2,5)
      *                BY-OUTBAL-BNKRPT(AGE-SUB,2,6)
      *                BY-OUTBAL-BNKRPT(AGE-SUB,2,7) FROM
      *                                LOAN-DEL-AMT(TOT-SUB,AGE-SUB,3).

      * BUCKET 4 POT CHG-OFFS USE RECENCY NOT CONTRACTUAL FOR LNS
      * IF GP-PROGRP-DL-CONY = "Y" THEN USE CONTRACTUAL INSTEAD OF RECENCY
      *                                      MJD REQ#525 (CUMBERLAND)
           IF GP-PROGRP-DL-CONT = "Y"
              ADD BY-OUTBAL(AGE-SUB,2,5)
                  BY-OUTBAL(AGE-SUB,2,6)
                  BY-OUTBAL(AGE-SUB,2,7) TO
                                     LOAN-DEL-AMT(TOT-SUB,AGE-SUB,4)
           ELSE
              ADD BY-OUTBAL(AGE-SUB,1,5)
                  BY-OUTBAL(AGE-SUB,1,6)
                  BY-OUTBAL(AGE-SUB,1,7) TO
                                     LOAN-DEL-AMT(TOT-SUB,AGE-SUB,4).
      *    IF (GP-I-AM-MERC)
      *       IF GP-PROGRP-DL-CONT = "Y"
      *          SUBTRACT BY-OUTBAL-BNKRPT(AGE-SUB,2,5)
      *                   BY-OUTBAL-BNKRPT(AGE-SUB,2,6)
      *                   BY-OUTBAL-BNKRPT(AGE-SUB,2,7) FROM
      *                              LOAN-DEL-AMT(TOT-SUB,AGE-SUB,4)
      *       ELSE
      *          SUBTRACT BY-OUTBAL-BNKRPT(AGE-SUB,1,5)
      *                   BY-OUTBAL-BNKRPT(AGE-SUB,1,6)
      *                   BY-OUTBAL-BNKRPT(AGE-SUB,1,7) FROM
      *                                 LOAN-DEL-AMT(TOT-SUB,AGE-SUB,4).

           ADD BY-OUTBAL(AGE-SUB,2,9) TO
                                       LOAN-DEL-AMT(TOT-SUB,AGE-SUB,5).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-OUTBAL-BNKRPT(AGE-SUB,2,9) FROM
      *                                LOAN-DEL-AMT(TOT-SUB,AGE-SUB,5)
      *       ADD BY-OUTBAL(AGE-SUB,2,10) TO
      *                                LOAN-DEL-AMT(TOT-SUB,AGE-SUB,5)
      *       SUBTRACT BY-OUTBAL-BNKRPT(AGE-SUB,2,10) FROM
      *                                LOAN-DEL-AMT(TOT-SUB,AGE-SUB,5).

      *ADD BANKRUPT DELINQUENCY STATS:

           ADD BY-UNITS-BNKRPT(AGE-SUB,2,1) TO
                                     LOAN-BNKRPT-NO(TOT-SUB,AGE-SUB,1).

           ADD BY-UNITS-BNKRPT(AGE-SUB,2,2) TO
                                     LOAN-BNKRPT-NO(TOT-SUB,AGE-SUB,2).

      * ADD CONTRACTUAL 90, 120, 150, 180, 210 TO BUCKET 3 (90+)
           ADD BY-UNITS-BNKRPT(AGE-SUB,2,3)
               BY-UNITS-BNKRPT(AGE-SUB,2,4)
               BY-UNITS-BNKRPT(AGE-SUB,2,5)
               BY-UNITS-BNKRPT(AGE-SUB,2,6)
               BY-UNITS-BNKRPT(AGE-SUB,2,7) TO
                                     LOAN-BNKRPT-NO(TOT-SUB,AGE-SUB,3).

      * BUCKET 4 POT CHG-OFFS USE RECENCY NOT CONTRACTUAL FOR LNS
      * IF GP-PROGRP-DL-CONY = "Y" THEN USE CONTRACTUAL INSTEAD OF RECENCY
      *                                      MJD REQ#525 (CUMBERLAND)
           IF GP-PROGRP-DL-CONT = "Y"
              ADD BY-UNITS-BNKRPT(AGE-SUB,2,5)
                  BY-UNITS-BNKRPT(AGE-SUB,2,6)
                  BY-UNITS-BNKRPT(AGE-SUB,2,7) TO
                                     LOAN-BNKRPT-NO(TOT-SUB,AGE-SUB,4)
           ELSE
              ADD BY-UNITS-BNKRPT(AGE-SUB,1,5)
                  BY-UNITS-BNKRPT(AGE-SUB,1,6)
                  BY-UNITS-BNKRPT(AGE-SUB,1,7) TO
                                     LOAN-BNKRPT-NO(TOT-SUB,AGE-SUB,4).

           ADD BY-UNITS-BNKRPT(AGE-SUB,2,9) TO
                                     LOAN-BNKRPT-NO(TOT-SUB,AGE-SUB,5).
      *    IF (GP-I-AM-MERC)
      *       ADD BY-UNITS-BNKRPT(AGE-SUB,2,10) TO
      *                              LOAN-BNKRPT-NO(TOT-SUB,AGE-SUB,5).

           ADD BY-OUTBAL-BNKRPT(AGE-SUB,2,1) TO
                                     LOAN-BNKRPT-AMT(TOT-SUB,AGE-SUB,1).

           ADD BY-OUTBAL-BNKRPT(AGE-SUB,2,2) TO
                                     LOAN-BNKRPT-AMT(TOT-SUB,AGE-SUB,2).

      * ADD CONTRACTUAL 90, 120, 150, 180, 210 TO BUCKET 3 (90+)
           ADD BY-OUTBAL-BNKRPT(AGE-SUB,2,3)
               BY-OUTBAL-BNKRPT(AGE-SUB,2,4)
               BY-OUTBAL-BNKRPT(AGE-SUB,2,5)
               BY-OUTBAL-BNKRPT(AGE-SUB,2,6)
               BY-OUTBAL-BNKRPT(AGE-SUB,2,7) TO
                                     LOAN-BNKRPT-AMT(TOT-SUB,AGE-SUB,3).

      * BUCKET 4 POT CHG-OFFS USE RECENCY NOT CONTRACTUAL FOR LNS
      * IF GP-PROGRP-DL-CONY = "Y" THEN USE CONTRACTUAL INSTEAD OF RECENCY
      *                                      MJD REQ#525 (CUMBERLAND)
           IF GP-PROGRP-DL-CONT = "Y"
              ADD BY-OUTBAL-BNKRPT(AGE-SUB,2,5)
                  BY-OUTBAL-BNKRPT(AGE-SUB,2,6)
                  BY-OUTBAL-BNKRPT(AGE-SUB,2,7) TO
                                     LOAN-BNKRPT-AMT(TOT-SUB,AGE-SUB,4)
           ELSE
              ADD BY-OUTBAL-BNKRPT(AGE-SUB,1,5)
                  BY-OUTBAL-BNKRPT(AGE-SUB,1,6)
                  BY-OUTBAL-BNKRPT(AGE-SUB,1,7) TO
                                     LOAN-BNKRPT-AMT(TOT-SUB,AGE-SUB,4).

           ADD BY-OUTBAL-BNKRPT(AGE-SUB,2,9) TO
                                     LOAN-BNKRPT-AMT(TOT-SUB,AGE-SUB,5).
      *    IF (GP-I-AM-MERC)
      *       ADD BY-OUTBAL-BNKRPT(AGE-SUB,2,10) TO
      *                              LOAN-BNKRPT-AMT(TOT-SUB,AGE-SUB,5).

           IF AGE-SUB < 3
              ADD 1 TO AGE-SUB
              GO TO ADD-DELINQUENT-LOANS.

           GO TO EXIT-ADD-TOTALS.

      *----------------------------------------------------------------
       ADD-DELINQUENT-SALEFIN.
           ADD BY-UNITS(AGE-SUB,2,1) TO
                                     SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,1).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-UNITS-BNKRPT(AGE-SUB,2,1) FROM
      *                              SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,1)
      *       SUBTRACT BY-UNITS-BNKRPT(AGE-SUB,2,2) FROM
      *                              SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,2).

           ADD BY-UNITS(AGE-SUB,2,2) TO
                                     SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,2).

           ADD BY-UNITS(AGE-SUB,2,3)
               BY-UNITS(AGE-SUB,2,4)
               BY-UNITS(AGE-SUB,2,5)
               BY-UNITS(AGE-SUB,2,6)
               BY-UNITS(AGE-SUB,2,7) TO
                                     SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,3).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-UNITS-BNKRPT(AGE-SUB,2,3)
      *                BY-UNITS-BNKRPT(AGE-SUB,2,4)
      *                BY-UNITS-BNKRPT(AGE-SUB,2,5)
      *                BY-UNITS-BNKRPT(AGE-SUB,2,6)
      *                BY-UNITS-BNKRPT(AGE-SUB,2,7) FROM
      *                              SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,3).

           ADD BY-UNITS(AGE-SUB,2,5)
               BY-UNITS(AGE-SUB,2,6)
               BY-UNITS(AGE-SUB,2,7) TO
                                     SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,4).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-UNITS-BNKRPT(AGE-SUB,2,5)
      *                BY-UNITS-BNKRPT(AGE-SUB,2,6)
      *                BY-UNITS-BNKRPT(AGE-SUB,2,7) FROM
      *                              SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,4).

           ADD BY-UNITS(AGE-SUB,2,9) TO
                                     SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,5).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-UNITS-BNKRPT(AGE-SUB,2,9) FROM
      *                              SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,5)
      *       ADD BY-UNITS(AGE-SUB,2,10) TO
      *                              SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,5)
      *       SUBTRACT BY-UNITS-BNKRPT(AGE-SUB,2,10) FROM
      *                              SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,5)
      *       SUBTRACT BY-OUTBAL-BNKRPT(AGE-SUB,2,1) FROM
      *                              SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,1)
      *       SUBTRACT BY-OUTBAL-BNKRPT(AGE-SUB,2,2) FROM
      *                              SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,2).

           ADD BY-OUTBAL(AGE-SUB,2,1) TO
                                     SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,1).

           ADD BY-OUTBAL(AGE-SUB,2,2) TO
                                     SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,2).

           ADD BY-OUTBAL(AGE-SUB,2,3)
               BY-OUTBAL(AGE-SUB,2,4)
               BY-OUTBAL(AGE-SUB,2,5)
               BY-OUTBAL(AGE-SUB,2,6)
               BY-OUTBAL(AGE-SUB,2,7) TO
                                     SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,3).

      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-OUTBAL-BNKRPT(AGE-SUB,2,3)
      *                BY-OUTBAL-BNKRPT(AGE-SUB,2,4)
      *                BY-OUTBAL-BNKRPT(AGE-SUB,2,5)
      *                BY-OUTBAL-BNKRPT(AGE-SUB,2,6)
      *                BY-OUTBAL-BNKRPT(AGE-SUB,2,7) FROM
      *                              SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,3).

           ADD BY-OUTBAL(AGE-SUB,2,5)
               BY-OUTBAL(AGE-SUB,2,6)
               BY-OUTBAL(AGE-SUB,2,7) TO
                                     SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,4).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-OUTBAL-BNKRPT(AGE-SUB,2,5)
      *                BY-OUTBAL-BNKRPT(AGE-SUB,2,6)
      *                BY-OUTBAL-BNKRPT(AGE-SUB,2,7) FROM
      *                              SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,4).

           ADD BY-OUTBAL(AGE-SUB,2,9) TO
                                     SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,5).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-OUTBAL-BNKRPT(AGE-SUB,2,9) FROM
      *                              SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,5)
      *       ADD BY-OUTBAL(AGE-SUB,2,10) TO
      *                              SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,5)
      *       SUBTRACT BY-OUTBAL-BNKRPT(AGE-SUB,2,10) FROM
      *                              SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,5).

      * ADD BANKRUPT DELINQUENCY:
           ADD BY-UNITS-BNKRPT(AGE-SUB,2,1) TO
                                  SALEFIN-BNKRPT-NO(TOT-SUB,AGE-SUB,1).

           ADD BY-UNITS-BNKRPT(AGE-SUB,2,2) TO
                                  SALEFIN-BNKRPT-NO(TOT-SUB,AGE-SUB,2).

           ADD BY-UNITS-BNKRPT(AGE-SUB,2,3)
               BY-UNITS-BNKRPT(AGE-SUB,2,4)
               BY-UNITS-BNKRPT(AGE-SUB,2,5)
               BY-UNITS-BNKRPT(AGE-SUB,2,6)
               BY-UNITS-BNKRPT(AGE-SUB,2,7) TO
                                  SALEFIN-BNKRPT-NO(TOT-SUB,AGE-SUB,3).

           ADD BY-UNITS-BNKRPT(AGE-SUB,2,5)
               BY-UNITS-BNKRPT(AGE-SUB,2,6)
               BY-UNITS-BNKRPT(AGE-SUB,2,7) TO
                                  SALEFIN-BNKRPT-NO(TOT-SUB,AGE-SUB,4).

           ADD BY-UNITS-BNKRPT(AGE-SUB,2,9) TO
                                  SALEFIN-BNKRPT-NO(TOT-SUB,AGE-SUB,5).
      *    IF (GP-I-AM-MERC)
      *       ADD BY-UNITS-BNKRPT(AGE-SUB,2,10) TO
      *                           SALEFIN-BNKRPT-NO(TOT-SUB,AGE-SUB,5).

           ADD BY-OUTBAL-BNKRPT(AGE-SUB,2,1) TO
                                  SALEFIN-BNKRPT-AMT(TOT-SUB,AGE-SUB,1).

           ADD BY-OUTBAL-BNKRPT(AGE-SUB,2,2) TO
                                  SALEFIN-BNKRPT-AMT(TOT-SUB,AGE-SUB,2).

           ADD BY-OUTBAL-BNKRPT(AGE-SUB,2,3)
               BY-OUTBAL-BNKRPT(AGE-SUB,2,4)
               BY-OUTBAL-BNKRPT(AGE-SUB,2,5)
               BY-OUTBAL-BNKRPT(AGE-SUB,2,6)
               BY-OUTBAL-BNKRPT(AGE-SUB,2,7) TO
                                  SALEFIN-BNKRPT-AMT(TOT-SUB,AGE-SUB,3).

           ADD BY-OUTBAL-BNKRPT(AGE-SUB,2,5)
               BY-OUTBAL-BNKRPT(AGE-SUB,2,6)
               BY-OUTBAL-BNKRPT(AGE-SUB,2,7) TO
                                  SALEFIN-BNKRPT-AMT(TOT-SUB,AGE-SUB,4).

           ADD BY-OUTBAL-BNKRPT(AGE-SUB,2,9) TO
                                  SALEFIN-BNKRPT-AMT(TOT-SUB,AGE-SUB,5).

           IF AGE-SUB < 3
              ADD 1 TO AGE-SUB
              GO TO ADD-DELINQUENT-SALEFIN.

       EXIT-ADD-TOTALS.
           EXIT.

      ******************************************************************
       GET-BEG-MONTH-DELINQUENCY SECTION.
      ******************************************************************

           MOVE 1 TO AGE-SUB.
           MOVE 2 TO NEXT-SUB.

           IF LC-SALEFIN = "Y"
              PERFORM ADD-BEG-MONTH-SALEFIN
           ELSE
              PERFORM ADD-BEG-MONTH-LOANS.

           MOVE BY1-KEY TO HOLD-BY1-KEY.
           MOVE NEXT-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.
           PERFORM READ-BY1-FILE.

           IF IO-FG NOT = 0
              GO TO RESTART-CURRENT-BY.

           MOVE 2 TO AGE-SUB.
           MOVE 2 TO NEXT-SUB.

           IF LC-SALEFIN = "Y"
              PERFORM ADD-BEG-MONTH-SALEFIN
           ELSE
              PERFORM ADD-BEG-MONTH-LOANS.

           GO TO RESTART-CURRENT-BY.

      *----------------------------------------------------------------
       ADD-BEG-MONTH-LOANS.
           ADD BY-UNITS(NEXT-SUB,2,1) TO
                                     LOAN-DEL-NO(TOT-SUB,AGE-SUB,1).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-UNITS-BNKRPT(NEXT-SUB,2,1) FROM
      *                              LOAN-DEL-NO(TOT-SUB,AGE-SUB,1)
      *       SUBTRACT BY-UNITS-BNKRPT(NEXT-SUB,2,2) FROM
      *                              LOAN-DEL-NO(TOT-SUB,AGE-SUB,2).

           ADD BY-UNITS(NEXT-SUB,2,2) TO
                                     LOAN-DEL-NO(TOT-SUB,AGE-SUB,2).

           ADD BY-UNITS(NEXT-SUB,2,3)
               BY-UNITS(NEXT-SUB,2,4)
               BY-UNITS(NEXT-SUB,2,5)
               BY-UNITS(NEXT-SUB,2,6)
               BY-UNITS(NEXT-SUB,2,7) TO
                                     LOAN-DEL-NO(TOT-SUB,AGE-SUB,3).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-UNITS-BNKRPT(NEXT-SUB,2,3)
      *                BY-UNITS-BNKRPT(NEXT-SUB,2,4)
      *                BY-UNITS-BNKRPT(NEXT-SUB,2,5)
      *                BY-UNITS-BNKRPT(NEXT-SUB,2,6)
      *                BY-UNITS-BNKRPT(NEXT-SUB,2,7) FROM
      *                              LOAN-DEL-NO(TOT-SUB,AGE-SUB,3).

      * IF GP-PROGRP-DL-CONY = "Y" THEN USE CONTRACTUAL INSTEAD OF RECENCY
      *                                      MJD REQ#525 (CUMBERLAND)
           IF GP-PROGRP-DL-CONT = "Y"
              ADD BY-UNITS(NEXT-SUB,2,5)
                  BY-UNITS(NEXT-SUB,2,6)
                  BY-UNITS(NEXT-SUB,2,7) TO
                                     LOAN-DEL-NO(TOT-SUB,AGE-SUB,4)
           ELSE
              ADD BY-UNITS(NEXT-SUB,1,5)
                  BY-UNITS(NEXT-SUB,1,6)
                  BY-UNITS(NEXT-SUB,1,7) TO
                                     LOAN-DEL-NO(TOT-SUB,AGE-SUB,4).
      *    IF (GP-I-AM-MERC)
      *       IF GP-PROGRP-DL-CONT = "Y"
      *          SUBTRACT BY-UNITS-BNKRPT(NEXT-SUB,2,5)
      *                   BY-UNITS-BNKRPT(NEXT-SUB,2,6)
      *                   BY-UNITS-BNKRPT(NEXT-SUB,2,7) FROM
      *                              LOAN-DEL-NO(TOT-SUB,AGE-SUB,4)
      *       ELSE
      *          SUBTRACT BY-UNITS-BNKRPT(NEXT-SUB,1,5)
      *                   BY-UNITS-BNKRPT(NEXT-SUB,1,6)
      *                   BY-UNITS-BNKRPT(NEXT-SUB,1,7) FROM
      *                              LOAN-DEL-NO(TOT-SUB,AGE-SUB,4).

           ADD BY-UNITS(NEXT-SUB,2,9) TO
                                     LOAN-DEL-NO(TOT-SUB,AGE-SUB,5).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-UNITS-BNKRPT(NEXT-SUB,2,9) FROM
      *                              LOAN-DEL-NO(TOT-SUB,AGE-SUB,5)
      *       ADD BY-UNITS(NEXT-SUB,2,10) TO
      *                              LOAN-DEL-NO(TOT-SUB,AGE-SUB,5)
      *       SUBTRACT BY-UNITS-BNKRPT(NEXT-SUB,2,10) FROM
      *                              LOAN-DEL-NO(TOT-SUB,AGE-SUB,5)
      *       SUBTRACT BY-OUTBAL-BNKRPT(NEXT-SUB,2,1) FROM
      *                                LOAN-DEL-AMT(TOT-SUB,AGE-SUB,1)
      *       SUBTRACT BY-OUTBAL-BNKRPT(NEXT-SUB,2,2) FROM
      *                                LOAN-DEL-AMT(TOT-SUB,AGE-SUB,2).

           ADD BY-OUTBAL(NEXT-SUB,2,1) TO
                                       LOAN-DEL-AMT(TOT-SUB,AGE-SUB,1).

           ADD BY-OUTBAL(NEXT-SUB,2,2) TO
                                       LOAN-DEL-AMT(TOT-SUB,AGE-SUB,2).

           ADD BY-OUTBAL(NEXT-SUB,2,3)
               BY-OUTBAL(NEXT-SUB,2,4)
               BY-OUTBAL(NEXT-SUB,2,5)
               BY-OUTBAL(NEXT-SUB,2,6)
               BY-OUTBAL(NEXT-SUB,2,7) TO
                                       LOAN-DEL-AMT(TOT-SUB,AGE-SUB,3).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-OUTBAL-BNKRPT(NEXT-SUB,2,3)
      *                BY-OUTBAL-BNKRPT(NEXT-SUB,2,4)
      *                BY-OUTBAL-BNKRPT(NEXT-SUB,2,5)
      *                BY-OUTBAL-BNKRPT(NEXT-SUB,2,6)
      *                BY-OUTBAL-BNKRPT(NEXT-SUB,2,7) FROM
      *                                LOAN-DEL-AMT(TOT-SUB,AGE-SUB,3).

      * IF GP-PROGRP-DL-CONY = "Y" THEN USE CONTRACTUAL INSTEAD OF RECENCY
      *                                      MJD REQ#525 (CUMBERLAND)
           IF GP-PROGRP-DL-CONT = "Y"
              ADD BY-OUTBAL(NEXT-SUB,2,5)
                  BY-OUTBAL(NEXT-SUB,2,6)
                  BY-OUTBAL(NEXT-SUB,2,7) TO
                                       LOAN-DEL-AMT(TOT-SUB,AGE-SUB,4)
           ELSE
              ADD BY-OUTBAL(NEXT-SUB,1,5)
                  BY-OUTBAL(NEXT-SUB,1,6)
                  BY-OUTBAL(NEXT-SUB,1,7) TO
                                       LOAN-DEL-AMT(TOT-SUB,AGE-SUB,4).
      *    IF (GP-I-AM-MERC)
      *       IF GP-PROGRP-DL-CONT = "Y"
      *          SUBTRACT BY-OUTBAL-BNKRPT(NEXT-SUB,2,5)
      *                   BY-OUTBAL-BNKRPT(NEXT-SUB,2,6)
      *                   BY-OUTBAL-BNKRPT(NEXT-SUB,2,7) FROM
      *                                LOAN-DEL-AMT(TOT-SUB,AGE-SUB,4)
      *       ELSE
      *          SUBTRACT BY-OUTBAL-BNKRPT(NEXT-SUB,1,5)
      *                   BY-OUTBAL-BNKRPT(NEXT-SUB,1,6)
      *                   BY-OUTBAL-BNKRPT(NEXT-SUB,1,7) FROM
      *                                LOAN-DEL-AMT(TOT-SUB,AGE-SUB,4).

           ADD BY-OUTBAL(NEXT-SUB,2,9) TO
                                       LOAN-DEL-AMT(TOT-SUB,AGE-SUB,5).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-OUTBAL-BNKRPT(NEXT-SUB,2,9) FROM
      *                                LOAN-DEL-AMT(TOT-SUB,AGE-SUB,5)
      *       ADD BY-OUTBAL(NEXT-SUB,2,10) TO
      *                                LOAN-DEL-AMT(TOT-SUB,AGE-SUB,5)
      *       SUBTRACT BY-OUTBAL-BNKRPT(NEXT-SUB,2,10) FROM
      *                                LOAN-DEL-AMT(TOT-SUB,AGE-SUB,5).

      * ADD BANKRUPT DELINQUENCY:
           ADD BY-UNITS-BNKRPT(NEXT-SUB,2,1) TO
                                  LOAN-BNKRPT-NO(TOT-SUB,AGE-SUB,1).

           ADD BY-UNITS-BNKRPT(NEXT-SUB,2,2) TO
                                  LOAN-BNKRPT-NO(TOT-SUB,AGE-SUB,2).

           ADD BY-UNITS-BNKRPT(NEXT-SUB,2,3)
               BY-UNITS-BNKRPT(NEXT-SUB,2,4)
               BY-UNITS-BNKRPT(NEXT-SUB,2,5)
               BY-UNITS-BNKRPT(NEXT-SUB,2,6)
               BY-UNITS-BNKRPT(NEXT-SUB,2,7) TO
                                  LOAN-BNKRPT-NO(TOT-SUB,AGE-SUB,3).

      * IF GP-PROGRP-DL-CONY = "Y" THEN USE CONTRACTUAL INSTEAD OF RECENCY
      *                                      MJD REQ#525 (CUMBERLAND)
           IF GP-PROGRP-DL-CONT = "Y"
              ADD BY-UNITS-BNKRPT(NEXT-SUB,2,5)
                  BY-UNITS-BNKRPT(NEXT-SUB,2,6)
                  BY-UNITS-BNKRPT(NEXT-SUB,2,7) TO
                                  LOAN-BNKRPT-NO(TOT-SUB,AGE-SUB,4)
           ELSE
              ADD BY-UNITS-BNKRPT(NEXT-SUB,1,5)
                  BY-UNITS-BNKRPT(NEXT-SUB,1,6)
                  BY-UNITS-BNKRPT(NEXT-SUB,1,7) TO
                                  LOAN-BNKRPT-NO(TOT-SUB,AGE-SUB,4).

           ADD BY-UNITS-BNKRPT(NEXT-SUB,2,9) TO
                                  LOAN-BNKRPT-NO(TOT-SUB,AGE-SUB,5).
      *    IF (GP-I-AM-MERC)
      *       ADD BY-UNITS-BNKRPT(NEXT-SUB,2,10) TO
      *                           LOAN-BNKRPT-NO(TOT-SUB,AGE-SUB,5).

           ADD BY-OUTBAL-BNKRPT(NEXT-SUB,2,1) TO
                                  LOAN-BNKRPT-AMT(TOT-SUB,AGE-SUB,1).

           ADD BY-OUTBAL-BNKRPT(NEXT-SUB,2,2) TO
                                  LOAN-BNKRPT-AMT(TOT-SUB,AGE-SUB,2).

           ADD BY-OUTBAL-BNKRPT(NEXT-SUB,2,3)
               BY-OUTBAL-BNKRPT(NEXT-SUB,2,4)
               BY-OUTBAL-BNKRPT(NEXT-SUB,2,5)
               BY-OUTBAL-BNKRPT(NEXT-SUB,2,6)
               BY-OUTBAL-BNKRPT(NEXT-SUB,2,7) TO
                                  LOAN-BNKRPT-AMT(TOT-SUB,AGE-SUB,3).

      * IF GP-PROGRP-DL-CONY = "Y" THEN USE CONTRACTUAL INSTEAD OF RECENCY
      *                                      MJD REQ#525 (CUMBERLAND)
           IF GP-PROGRP-DL-CONT = "Y"
              ADD BY-OUTBAL-BNKRPT(NEXT-SUB,2,5)
                  BY-OUTBAL-BNKRPT(NEXT-SUB,2,6)
                  BY-OUTBAL-BNKRPT(NEXT-SUB,2,7) TO
                                  LOAN-BNKRPT-AMT(TOT-SUB,AGE-SUB,4)
           ELSE
              ADD BY-OUTBAL-BNKRPT(NEXT-SUB,1,5)
                  BY-OUTBAL-BNKRPT(NEXT-SUB,1,6)
                  BY-OUTBAL-BNKRPT(NEXT-SUB,1,7) TO
                                  LOAN-BNKRPT-AMT(TOT-SUB,AGE-SUB,4).

           ADD BY-OUTBAL-BNKRPT(NEXT-SUB,2,9) TO
                                  LOAN-BNKRPT-AMT(TOT-SUB,AGE-SUB,5).
      *    IF (GP-I-AM-MERC)
      *       ADD BY-OUTBAL-BNKRPT(NEXT-SUB,2,10) TO
      *                           LOAN-BNKRPT-AMT(TOT-SUB,AGE-SUB,5).
      *----------------------------------------------------------------
       ADD-BEG-MONTH-SALEFIN.
           ADD BY-UNITS(NEXT-SUB,2,1) TO
                                 SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,1).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-UNITS-BNKRPT(NEXT-SUB,2,1) FROM
      *                          SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,1)
      *       SUBTRACT BY-UNITS-BNKRPT(NEXT-SUB,2,2) FROM
      *                          SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,2).

           ADD BY-UNITS(NEXT-SUB,2,2) TO
                                 SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,2).

           ADD BY-UNITS(NEXT-SUB,2,3)
               BY-UNITS(NEXT-SUB,2,4)
               BY-UNITS(NEXT-SUB,2,5)
               BY-UNITS(NEXT-SUB,2,6)
               BY-UNITS(NEXT-SUB,2,7) TO
                                 SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,3).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-UNITS-BNKRPT(NEXT-SUB,2,3)
      *                BY-UNITS-BNKRPT(NEXT-SUB,2,4)
      *                BY-UNITS-BNKRPT(NEXT-SUB,2,5)
      *                BY-UNITS-BNKRPT(NEXT-SUB,2,6)
      *                BY-UNITS-BNKRPT(NEXT-SUB,2,7) FROM
      *                          SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,3).

           ADD BY-UNITS(NEXT-SUB,2,5)
               BY-UNITS(NEXT-SUB,2,6)
               BY-UNITS(NEXT-SUB,2,7) TO
                                 SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,4).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-UNITS-BNKRPT(NEXT-SUB,2,5)
      *                BY-UNITS-BNKRPT(NEXT-SUB,2,6)
      *                BY-UNITS-BNKRPT(NEXT-SUB,2,7) FROM
      *                          SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,4).

           ADD BY-UNITS(NEXT-SUB,2,9) TO
                                 SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,5).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-UNITS-BNKRPT(NEXT-SUB,2,9) FROM
      *                          SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,5)
      *       ADD BY-UNITS(NEXT-SUB,2,10) TO
      *                          SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,5)
      *       SUBTRACT BY-UNITS-BNKRPT(NEXT-SUB,2,10) FROM
      *                          SALEFIN-DEL-NO(TOT-SUB,AGE-SUB,5)
      *       SUBTRACT BY-OUTBAL-BNKRPT(NEXT-SUB,2,1) FROM
      *                          SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,1)
      *       SUBTRACT BY-OUTBAL-BNKRPT(NEXT-SUB,2,2) FROM
      *                          SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,2).

           ADD BY-OUTBAL(NEXT-SUB,2,1) TO
                                 SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,1).

           ADD BY-OUTBAL(NEXT-SUB,2,2) TO
                                 SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,2).

           ADD BY-OUTBAL(NEXT-SUB,2,3)
               BY-OUTBAL(NEXT-SUB,2,4)
               BY-OUTBAL(NEXT-SUB,2,5)
               BY-OUTBAL(NEXT-SUB,2,6)
               BY-OUTBAL(NEXT-SUB,2,7) TO
                                 SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,3).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-OUTBAL-BNKRPT(NEXT-SUB,2,3)
      *                BY-OUTBAL-BNKRPT(NEXT-SUB,2,4)
      *                BY-OUTBAL-BNKRPT(NEXT-SUB,2,5)
      *                BY-OUTBAL-BNKRPT(NEXT-SUB,2,6)
      *                BY-OUTBAL-BNKRPT(NEXT-SUB,2,7) FROM
      *                          SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,3).

           ADD BY-OUTBAL(NEXT-SUB,2,5)
               BY-OUTBAL(NEXT-SUB,2,6)
               BY-OUTBAL(NEXT-SUB,2,7) TO
                                 SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,4).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-OUTBAL-BNKRPT(NEXT-SUB,2,5)
      *                BY-OUTBAL-BNKRPT(NEXT-SUB,2,6)
      *                BY-OUTBAL-BNKRPT(NEXT-SUB,2,7) FROM
      *                          SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,4).

           ADD BY-OUTBAL(NEXT-SUB,2,9) TO
                                 SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,5).
      *    IF (GP-I-AM-MERC)
      *       SUBTRACT BY-OUTBAL-BNKRPT(NEXT-SUB,2,9) FROM
      *                          SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,5)
      *       ADD BY-OUTBAL(NEXT-SUB,2,10) TO
      *                          SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,5)
      *       SUBTRACT BY-OUTBAL-BNKRPT(NEXT-SUB,2,10) FROM
      *                          SALEFIN-DEL-AMT(TOT-SUB,AGE-SUB,5).

      * ADD BANKRUPT DELINQUENCY
           ADD BY-UNITS-BNKRPT(NEXT-SUB,2,1) TO
                                 SALEFIN-BNKRPT-NO(TOT-SUB,AGE-SUB,1).

           ADD BY-UNITS-BNKRPT(NEXT-SUB,2,2) TO
                                 SALEFIN-BNKRPT-NO(TOT-SUB,AGE-SUB,2).

           ADD BY-UNITS-BNKRPT(NEXT-SUB,2,3)
               BY-UNITS-BNKRPT(NEXT-SUB,2,4)
               BY-UNITS-BNKRPT(NEXT-SUB,2,5)
               BY-UNITS-BNKRPT(NEXT-SUB,2,6)
               BY-UNITS-BNKRPT(NEXT-SUB,2,7) TO
                                 SALEFIN-BNKRPT-NO(TOT-SUB,AGE-SUB,3).

           ADD BY-UNITS-BNKRPT(NEXT-SUB,2,5)
               BY-UNITS-BNKRPT(NEXT-SUB,2,6)
               BY-UNITS-BNKRPT(NEXT-SUB,2,7) TO
                                 SALEFIN-BNKRPT-NO(TOT-SUB,AGE-SUB,4).

           ADD BY-UNITS-BNKRPT(NEXT-SUB,2,9) TO
                                 SALEFIN-BNKRPT-NO(TOT-SUB,AGE-SUB,5).
      *    IF (GP-I-AM-MERC)
      *       ADD BY-UNITS-BNKRPT(NEXT-SUB,2,10) TO
      *                          SALEFIN-BNKRPT-NO(TOT-SUB,AGE-SUB,5).

           ADD BY-OUTBAL-BNKRPT(NEXT-SUB,2,1) TO
                                 SALEFIN-BNKRPT-AMT(TOT-SUB,AGE-SUB,1).

           ADD BY-OUTBAL-BNKRPT(NEXT-SUB,2,2) TO
                                 SALEFIN-BNKRPT-AMT(TOT-SUB,AGE-SUB,2).

           ADD BY-OUTBAL-BNKRPT(NEXT-SUB,2,3)
               BY-OUTBAL-BNKRPT(NEXT-SUB,2,4)
               BY-OUTBAL-BNKRPT(NEXT-SUB,2,5)
               BY-OUTBAL-BNKRPT(NEXT-SUB,2,6)
               BY-OUTBAL-BNKRPT(NEXT-SUB,2,7) TO
                                 SALEFIN-BNKRPT-AMT(TOT-SUB,AGE-SUB,3).

           ADD BY-OUTBAL-BNKRPT(NEXT-SUB,2,5)
               BY-OUTBAL-BNKRPT(NEXT-SUB,2,6)
               BY-OUTBAL-BNKRPT(NEXT-SUB,2,7) TO
                                 SALEFIN-BNKRPT-AMT(TOT-SUB,AGE-SUB,4).

           ADD BY-OUTBAL-BNKRPT(NEXT-SUB,2,9) TO
                                 SALEFIN-BNKRPT-AMT(TOT-SUB,AGE-SUB,5).
      *    IF (GP-I-AM-MERC)
      *       ADD BY-OUTBAL-BNKRPT(NEXT-SUB,2,10) TO
      *                          SALEFIN-BNKRPT-AMT(TOT-SUB,AGE-SUB,5).
      *----------------------------------------------------------------
       RESTART-CURRENT-BY.
           MOVE HOLD-BY1-KEY TO BY1-KEY.

           MOVE BY-DATE-CCYYMM       TO QBY1-WBEG-DATE-CCYYMM
                                        QBY1-WEND-DATE-CCYYMM.
           MOVE BY-BRNO              TO QBY1-WBEG-BRNO
                                        QBY1-WEND-BRNO.
           MOVE BY-CLASS             TO QBY1-WBEG-CLASS.
           MOVE END-CLASS            TO QBY1-WEND-CLASS.

           PERFORM START-BY1-FILE.
           PERFORM READ-BY1-FILE-NEXT.

       EXIT-BEG-MONTH-DELINQUENCY.
           EXIT.

      ******************************************************************
       PRINT-TOTALS SECTION.
      ******************************************************************
           MOVE SPACES TO VERT-BAR-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "|" TO D-BAR(18) D-BAR(32) D-BAR(46) D-BAR(60) D-BAR(74)
                       D-BAR(88) D-BAR(96) D-BAR(108) D-BAR(120).

           MOVE DETAIL1-HEAD1 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DETAIL1-HEAD2 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 0 TO LN-FEED.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 1 TO LN-FEED.
           MOVE 1 TO CLASS-SUB.

      *----------------------------------------------------------------
       PRINT-CLASS-LINE.
           MOVE CLASS-ELE(CLASS-SUB) TO D1-CLASS.

           MOVE CLASS-GAIN-AMT-CUR(TOT-SUB, CLASS-SUB) TO
                D1-GAIN-AMT-CUR.

           COMPUTE AMT-WORK =
                CLASS-GAIN-AMT-CUR(TOT-SUB, CLASS-SUB)
              - CLASS-GAIN-AMT-ELM(TOT-SUB, CLASS-SUB).

           MOVE AMT-WORK TO D1-GAIN-AMT-MTD.

           COMPUTE AMT-WORK =
                CLASS-GAIN-AMT-CUR(TOT-SUB, CLASS-SUB)
              - CLASS-GAIN-AMT-ELY(TOT-SUB, CLASS-SUB).

           MOVE AMT-WORK TO D1-GAIN-AMT-YTD.

           MOVE CLASS-VOL-AMT-MTD(TOT-SUB, CLASS-SUB) TO
                D1-VOL-AMT-MTD.

           MOVE CLASS-VOL-AMT-YTD(TOT-SUB, CLASS-SUB) TO
                D1-VOL-AMT-YTD.

           IF CLASS-PRINCOL-MTD(TOT-SUB, CLASS-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                CLASS-PRINCOL-MTD(TOT-SUB, CLASS-SUB) /
                   CLASS-GAIN-AMT-CUR(TOT-SUB, CLASS-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.

           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.

           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D1-CASH-PCT.

           MOVE CLASS-GAIN-NO-CUR(TOT-SUB, CLASS-SUB) TO
                D1-GAIN-NO-CUR.

           COMPUTE NO-WORK =
                CLASS-GAIN-NO-CUR(TOT-SUB, CLASS-SUB)
              - CLASS-GAIN-NO-ELM(TOT-SUB, CLASS-SUB).

           MOVE NO-WORK TO D1-GAIN-NO-MTD.

           COMPUTE NO-WORK =
                CLASS-GAIN-NO-CUR(TOT-SUB, CLASS-SUB)
              - CLASS-GAIN-NO-ELY(TOT-SUB, CLASS-SUB).

           MOVE NO-WORK TO D1-GAIN-NO-YTD.

           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF CLASS-SUB < CLASS-LINE-COUNT
              ADD 1 TO CLASS-SUB
              GO TO PRINT-CLASS-LINE.

           MOVE SPACES TO VERT-BAR-LINE.
           MOVE "|" TO D-BAR(18) D-BAR(46) D-BAR(74) D-BAR(108).

           MOVE "AVERAGE GROSS" TO D1-PROMPT1.
           MOVE " NEW LOANS:" TO D1-PROMPT2.

           IF TYPE-VOL-NO-MTD(TOT-SUB, 2) NOT = 0
              COMPUTE AVG-WORK ROUNDED =
                TYPE-VOL-AMT-MTD(TOT-SUB, 2) /
                   TYPE-VOL-NO-MTD(TOT-SUB, 2)
           ELSE
              MOVE 0 TO AVG-WORK.

           MOVE AVG-WORK TO D1-NEW-AVG.
           MOVE "  LOANS MADE:" TO D1-PROMPT3.

           IF TYPE-VOL-NO-MTD(TOT-SUB, 9) NOT = 0
              COMPUTE AVG-WORK ROUNDED =
                TYPE-VOL-AMT-MTD(TOT-SUB, 9) /
                   TYPE-VOL-NO-MTD(TOT-SUB, 9)
           ELSE
              MOVE 0 TO AVG-WORK.

           MOVE AVG-WORK TO D1-MADE-AVG.
           MOVE "        LOAN BALANCE:" TO D1-PROMPT4.

           IF CLASS-GAIN-NO-CUR(TOT-SUB, TOTAL-LN-SUB) NOT = 0
              COMPUTE AVG-WORK ROUNDED =
                CLASS-GAIN-AMT-CUR(TOT-SUB, TOTAL-LN-SUB) /
                   CLASS-GAIN-NO-CUR(TOT-SUB, TOTAL-LN-SUB)
           ELSE
              MOVE 0 TO AVG-WORK.

           MOVE AVG-WORK TO D1-LOAN-AVG.
           MOVE "   S.F. BAL:" TO D1-PROMPT5.

           IF CLASS-GAIN-NO-CUR(TOT-SUB, TOTAL-SF-SUB) NOT = 0
              COMPUTE AVG-WORK ROUNDED =
                CLASS-GAIN-AMT-CUR(TOT-SUB, TOTAL-SF-SUB) /
                   CLASS-GAIN-NO-CUR(TOT-SUB, TOTAL-SF-SUB)
           ELSE
              MOVE 0 TO AVG-WORK.

           MOVE AVG-WORK TO D1-SF-AVG.

           PERFORM WRITE-DETAIL-LINE.

      *----------------------------------------------------------------
       HEAD-UP-INS-LATE.
      *----------------------------------------------------------------
           MOVE SPACES TO VERT-BAR-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE DASH-LINE TO DETAIL-LINE.
           MOVE 0 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "|" TO D-BAR(6) D-BAR(17) D-BAR(31)
                       D-BAR(40) D-BAR(41)
                       D-BAR(57) D-BAR(66) D-BAR(75)
                       D-BAR(84) D-BAR(93) D-BAR(102)
                       D-BAR(111) D-BAR(120) D-BAR(129).

           MOVE DETAIL-INS-HEAD TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE DETAIL-INS-HEAD2 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 0 TO LN-FEED.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           MOVE 1 TO INS-SUB.

      *----------------------------------------------------------------
       PRINT-INS-LINE.
           MOVE SOURCE-ELE(1) TO D2-SOURCE.
           MOVE TYPE-VOL-NO-MTD(TOT-SUB, 1) TO D2-VOL-NO.
           MOVE TYPE-VOL-AMT-MTD(TOT-SUB, 1) TO D2-VOL-AMT.
           MOVE 0 TO PCT-WORK.

           IF TYPE-VOL-NO-MTD(TOT-SUB, 1) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 TYPE-VOL-NO-MTD(TOT-SUB, 1)
                    / CLASS-GAIN-NO-CUR(TOT-SUB, TOTAL-SF-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.
           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D2-VOL-PCT1.

           MOVE "# LOANS ELIG" TO D1-INS-PROMPT.
           MOVE INS-NO-ELIG(TOT-SUB, 1) TO D1-INS-NO1.
           MOVE INS-NO-ELIG(TOT-SUB, 2) TO D1-INS-NO2.
           MOVE INS-NO-ELIG(TOT-SUB, 3) TO D1-INS-NO3.
           MOVE INS-NO-ELIG(TOT-SUB, 4) TO D1-INS-NO4.
           MOVE INS-NO-ELIG(TOT-SUB, 5) TO D1-INS-NO5.
           MOVE INS-NO-ELIG(TOT-SUB, 6) TO D1-INS-NO6.
      * AD&D
           MOVE INS-NO-ELIG(TOT-SUB, 12) TO D1-INS-NO7.
      * CARCLUB
           MOVE INS-NO-ELIG(TOT-SUB, 13) TO D1-INS-NO8.
           PERFORM WRITE-DETAIL-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SOURCE-ELE(2) TO D2-SOURCE.
           MOVE TYPE-VOL-NO-MTD(TOT-SUB, 2) TO D2-VOL-NO.
           MOVE TYPE-VOL-AMT-MTD(TOT-SUB, 2) TO D2-VOL-AMT.

           MOVE "# POL WRITTEN" TO D1-INS-PROMPT.
           MOVE INS-NO-WROTE(TOT-SUB, 1) TO D1-INS-NO1.
           MOVE INS-NO-WROTE(TOT-SUB, 2) TO D1-INS-NO2.
           MOVE INS-NO-WROTE(TOT-SUB, 3) TO D1-INS-NO3.
           MOVE INS-NO-WROTE(TOT-SUB, 4) TO D1-INS-NO4.
           MOVE INS-NO-WROTE(TOT-SUB, 5) TO D1-INS-NO5.
           MOVE INS-NO-WROTE(TOT-SUB, 6) TO D1-INS-NO6.
      * AD&D
           MOVE INS-NO-WROTE(TOT-SUB, 12) TO D1-INS-NO7.
      * CARCLUB
           MOVE INS-NO-WROTE(TOT-SUB, 13) TO D1-INS-NO8.
           PERFORM WRITE-DETAIL-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      * PB 
           MOVE SOURCE-ELE(3) TO D2-SOURCE.
           MOVE TYPE-VOL-NO-MTD(TOT-SUB, 3) TO D2-VOL-NO.
           MOVE TYPE-VOL-AMT-MTD(TOT-SUB, 3) TO D2-VOL-AMT.
           MOVE 0 TO PCT-WORK.
           IF TYPE-VOL-NO-MTD(TOT-SUB, 3) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 TYPE-VOL-NO-MTD(TOT-SUB, 3)
                 / CLASS-GAIN-NO-CUR(TOT-SUB, TOTAL-LN-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.
           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D2-VOL-PCT1.

           MOVE "# NOT WRITTEN" TO D1-INS-PROMPT.
           MOVE INS-NO-NOTWROTE(TOT-SUB, 1) TO D1-INS-NO1.
           MOVE INS-NO-NOTWROTE(TOT-SUB, 2) TO D1-INS-NO2.
           MOVE INS-NO-NOTWROTE(TOT-SUB, 3) TO D1-INS-NO3.
           MOVE INS-NO-NOTWROTE(TOT-SUB, 4) TO D1-INS-NO4.
           MOVE INS-NO-NOTWROTE(TOT-SUB, 5) TO D1-INS-NO5.
      * OTHER 3
           MOVE INS-NO-NOTWROTE(TOT-SUB, 6) TO D1-INS-NO6.
      * AD&D
           MOVE INS-NO-NOTWROTE(TOT-SUB, 12) TO D1-INS-NO7.
      * CARCLUB
           MOVE INS-NO-NOTWROTE(TOT-SUB, 13) TO D1-INS-NO8.
           PERFORM WRITE-DETAIL-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SOURCE-ELE(4) TO D2-SOURCE.
           MOVE TYPE-VOL-NO-MTD(TOT-SUB, 4) TO D2-VOL-NO.
           MOVE TYPE-VOL-AMT-MTD(TOT-SUB, 4) TO D2-VOL-AMT.

           MOVE "% WRITTEN/ELIG" TO D1-INS-PROMPT.
           IF INS-NO-WROTE(TOT-SUB, 1) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                INS-NO-WROTE(TOT-SUB, 1) /
                   INS-NO-ELIG(TOT-SUB, 1) * 100
              ELSE
                 MOVE 0 TO PCT-WORK.
           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.
           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D1-INS-PCT1.

           IF INS-NO-WROTE(TOT-SUB, 2) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                INS-NO-WROTE(TOT-SUB, 2) /
                   INS-NO-ELIG(TOT-SUB, 2) * 100
              ELSE
                 MOVE 0 TO PCT-WORK.
           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.
           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D1-INS-PCT2.

           IF INS-NO-WROTE(TOT-SUB, 3) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                INS-NO-WROTE(TOT-SUB, 3) /
                   INS-NO-ELIG(TOT-SUB, 3) * 100
              ELSE
                 MOVE 0 TO PCT-WORK.
           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.
           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D1-INS-PCT3.

           IF INS-NO-WROTE(TOT-SUB, 4) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                INS-NO-WROTE(TOT-SUB, 4) /
                   INS-NO-ELIG(TOT-SUB, 4) * 100
              ELSE
                 MOVE 0 TO PCT-WORK.
           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.
           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D1-INS-PCT4.

      *  NO ELIGIBLE FOR O2, O3 INSURANCE:
           MOVE 0 TO D1-INS-PCT5
                     D1-INS-PCT6
                     D1-INS-PCT7
                     D1-INS-PCT8.

           PERFORM WRITE-DETAIL-LINE.

           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SOURCE-ELE(5) TO D2-SOURCE.
           MOVE TYPE-VOL-NO-MTD(TOT-SUB, 5) TO D2-VOL-NO.
           MOVE TYPE-VOL-AMT-MTD(TOT-SUB, 5) TO D2-VOL-AMT.

           MOVE "PREMIUMS" TO D1-INS-PROMPT.
           MOVE INS-AMT-WROTE(TOT-SUB, 1) TO D1-INS-NO1.
           MOVE INS-AMT-WROTE(TOT-SUB, 2) TO D1-INS-NO2.
           MOVE INS-AMT-WROTE(TOT-SUB, 3) TO D1-INS-NO3.
           MOVE INS-AMT-WROTE(TOT-SUB, 4) TO D1-INS-NO4.
           MOVE INS-AMT-WROTE(TOT-SUB, 5) TO D1-INS-NO5.
           MOVE INS-AMT-WROTE(TOT-SUB, 6) TO D1-INS-NO6.
      * AD&D
           MOVE INS-AMT-WROTE(TOT-SUB, 12) TO D1-INS-NO7.
      * CARCLUB
           MOVE INS-AMT-WROTE(TOT-SUB, 13) TO D1-INS-NO8.
           PERFORM WRITE-DETAIL-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           IF GP-THE-ONLY-SF-SRCD NOT = " "
              MOVE GP-THE-ONLY-SF-SRCD TO D2-SOURCE
           ELSE
              MOVE "  RF" TO D2-SOURCE.
           MOVE TYPE-VOL-NO-MTD(TOT-SUB, 6) TO D2-VOL-NO.
           MOVE TYPE-VOL-AMT-MTD(TOT-SUB, 6) TO D2-VOL-AMT.

           MOVE "REFUNDS" TO D1-INS-PROMPT.
           MOVE INS-AMT-REFUND(TOT-SUB, 1) TO D1-INS-NO1.
           MOVE INS-AMT-REFUND(TOT-SUB, 2) TO D1-INS-NO2.
           MOVE INS-AMT-REFUND(TOT-SUB, 3) TO D1-INS-NO3.
           MOVE INS-AMT-REFUND(TOT-SUB, 4) TO D1-INS-NO4.
           MOVE INS-AMT-REFUND(TOT-SUB, 5) TO D1-INS-NO5.
           MOVE INS-AMT-REFUND(TOT-SUB, 6) TO D1-INS-NO6.
      * AD&D
           MOVE INS-AMT-REFUND(TOT-SUB, 12) TO D1-INS-NO7.
      * CARCLUB
           MOVE INS-AMT-REFUND(TOT-SUB, 13) TO D1-INS-NO8.
           PERFORM WRITE-DETAIL-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SOURCE-ELE(7) TO D2-SOURCE.
           MOVE TYPE-VOL-NO-MTD(TOT-SUB, 7) TO D2-VOL-NO.
           MOVE TYPE-VOL-AMT-MTD(TOT-SUB, 7) TO D2-VOL-AMT.

           MOVE "NET INSURANCE" TO D1-INS-PROMPT.

           COMPUTE NO-WORK ROUNDED = INS-AMT-WROTE(TOT-SUB, 1) -
              INS-AMT-REFUND(TOT-SUB, 1).
           MOVE NO-WORK TO D1-INS-NO1.

           COMPUTE NO-WORK ROUNDED = INS-AMT-WROTE(TOT-SUB, 2) -
              INS-AMT-REFUND(TOT-SUB, 2).
           MOVE NO-WORK TO D1-INS-NO2.

           COMPUTE NO-WORK ROUNDED = INS-AMT-WROTE(TOT-SUB, 3) -
              INS-AMT-REFUND(TOT-SUB, 3).
           MOVE NO-WORK TO D1-INS-NO3.

           COMPUTE NO-WORK ROUNDED = INS-AMT-WROTE(TOT-SUB, 4) -
              INS-AMT-REFUND(TOT-SUB, 4).
           MOVE NO-WORK TO D1-INS-NO4.

           COMPUTE NO-WORK ROUNDED = INS-AMT-WROTE(TOT-SUB, 5) -
              INS-AMT-REFUND(TOT-SUB, 5).
           MOVE NO-WORK TO D1-INS-NO5.

           COMPUTE NO-WORK ROUNDED = INS-AMT-WROTE(TOT-SUB, 6) -
              INS-AMT-REFUND(TOT-SUB, 6).
           MOVE NO-WORK TO D1-INS-NO6.

      * AD&D
           COMPUTE NO-WORK ROUNDED = INS-AMT-WROTE(TOT-SUB, 12) -
              INS-AMT-REFUND(TOT-SUB, 12).
           MOVE NO-WORK TO D1-INS-NO7.

      * CARCLUB
           COMPUTE NO-WORK ROUNDED = INS-AMT-WROTE(TOT-SUB, 13) -
              INS-AMT-REFUND(TOT-SUB, 13).
           MOVE NO-WORK TO D1-INS-NO8.

           PERFORM WRITE-DETAIL-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SOURCE-ELE(8) TO D2-SOURCE.
           MOVE TYPE-VOL-NO-MTD(TOT-SUB, 8) TO D2-VOL-NO.
           MOVE TYPE-VOL-AMT-MTD(TOT-SUB, 8) TO D2-VOL-AMT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SOURCE-ELE(9) TO D2-SOURCE.
           MOVE TYPE-VOL-NO-MTD(TOT-SUB, 9) TO D2-VOL-NO.
           MOVE TYPE-VOL-AMT-MTD(TOT-SUB, 9) TO D2-VOL-AMT.
           PERFORM WRITE-DETAIL-LINE.

      *----------------------------------------------------------------
       HEAD-UP-PART2.
           MOVE SPACES TO VERT-BAR-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DASH-LINE TO DETAIL-LINE.
           MOVE 0 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "|" TO D-BAR(24) D-BAR(30) D-BAR(34) D-BAR(44)
                       D-BAR(56) D-BAR(65) D-BAR(75) D-BAR(85)
                       D-BAR(97) D-BAR(104) D-BAR(111) D-BAR(119)
                       D-BAR(126).
           MOVE 1 TO LN-FEED.
           MOVE DETAIL2-HEAD1 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DETAIL2-HEAD2 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 0 TO LN-FEED.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

      *----------------------------------------------------------------
       START-WK.
           MOVE "Y" TO MORE-DEALERS.
           MOVE ZEROS TO DEALER-SUB-TOTS.
           MOVE 0 TO DEALER-COUNT DL-SUB.
           MOVE TOT-SUB TO WK-LEVEL2.
           MOVE 999999999.99 TO WK-MTD-PUR-AMT.
           PERFORM START-WK2-FILE-NOT-GREATER.

      *----------------------------------------------------------------
       NEXT-WK.
           PERFORM READ-WK-FILE-PREVIOUS.

           IF IO-FG NOT = 0
              MOVE "N" TO MORE-DEALERS
              GO TO PRINT-DEALER-LINE.

           IF WK-LEVEL NOT = TOT-SUB
              MOVE "N" TO MORE-DEALERS
              GO TO PRINT-DEALER-LINE.

      *----------------------------------------------------------------
       NEXT-DEALER-LINE.
           ADD 1 TO DEALER-COUNT.
           ADD WK-OUTSUNIT TO TOTAL-OUTSUNIT.
           ADD WK-OUTSAMT TO TOTAL-OUTSAMT.
           ADD WK-MTD-PUR-NO TO TOTAL-MTD-PUR-NO.
           ADD WK-MTD-PUR-AMT TO TOTAL-MTD-PUR-AMT.
           ADD WK-YTD-PUR-NO TO TOTAL-YTD-PUR-NO.
           ADD WK-YTD-PUR-AMT TO TOTAL-YTD-PUR-AMT.
           ADD WK-MTD-TODL TO TOTAL-MTD-TODL.
           ADD WK-YTD-TODL TO TOTAL-YTD-TODL.
           ADD WK-YTD-TODL-AMT TO TOTAL-YTD-TODL-AMT.

           IF DEALER-COUNT < 7
              GO TO PRINT-DEALER-LINE.

           ADD WK-OUTSUNIT TO OTHER-OUTSUNIT.
           ADD WK-OUTSAMT TO OTHER-OUTSAMT.
           ADD WK-MTD-PUR-NO TO OTHER-MTD-PUR-NO.
           ADD WK-MTD-PUR-AMT TO OTHER-MTD-PUR-AMT.
           ADD WK-YTD-PUR-NO TO OTHER-YTD-PUR-NO.
           ADD WK-YTD-PUR-AMT TO OTHER-YTD-PUR-AMT.
           ADD WK-MTD-TODL TO OTHER-MTD-TODL.
           ADD WK-YTD-TODL TO OTHER-YTD-TODL.
           ADD WK-YTD-TODL-AMT TO OTHER-YTD-TODL-AMT.
           GO TO NEXT-WK.

      *----------------------------------------------------------------
       PRINT-DEALER-LINE.
           ADD 1 TO DL-SUB.
           IF MORE-DEALERS = "N"
              GO TO PRINT-DEALER-LINE7.
           IF DL-SUB = 7
              GO TO PRINT-DEALER-LINE7.

           MOVE WK-DL-NAME TO D2-DL-NAME
           MOVE WK-BRANCH TO D2-DL-BRNO
           MOVE WK-DL-MFGCODE TO D2-MFGCODE.
           MOVE WK-OUTSUNIT TO D2-OUTSUNIT.
           MOVE WK-OUTSAMT TO D2-OUTSAMT.
           MOVE WK-MTD-PUR-NO TO D2-MTD-PUR-NO.
           MOVE WK-MTD-PUR-AMT TO D2-MTD-PUR-AMT.
           MOVE WK-YTD-PUR-NO TO D2-YTD-PUR-NO.
           MOVE WK-YTD-PUR-AMT TO D2-YTD-PUR-AMT.
           MOVE WK-MTD-TODL TO D2-MTD-TODL.

           IF WK-MTD-TODL NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 WK-MTD-TODL / WK-OUTSUNIT * 100
           ELSE
              MOVE 0 TO PCT-WORK.

           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.

           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D2-TODL-PCT1.
           MOVE WK-YTD-TODL TO D2-YTD-TODL.

           IF WK-YTD-TODL NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 WK-YTD-TODL / WK-OUTSUNIT * 100
           ELSE
              MOVE 0 TO PCT-WORK.

           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.

           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D2-TODL-PCT2.
           MOVE WK-YTD-TODL-AMT TO D2-YTD-TODL-AMT.
           GO TO WRITE-DEALER-LINE.

      *----------------------------------------------------------------
       PRINT-DEALER-LINE7.
           IF DEALER-COUNT = 0
              IF GP-THE-ONLY-SF-SRCD = " "
                 GO TO HEAD-UP-PART3
              ELSE
                 MOVE 7 TO DL-SUB
                 MOVE CLASS-GAIN-NO-CUR(TOT-SUB, TOTAL-SF-SUB)
                                     TO TOTAL-OUTSUNIT
                 MOVE CLASS-GAIN-AMT-CUR(TOT-SUB, TOTAL-SF-SUB)
                                     TO TOTAL-OUTSAMT
                 MOVE TYPE-VOL-NO-MTD(TOT-SUB, 6) TO TOTAL-MTD-PUR-NO
                 MOVE TYPE-VOL-AMT-MTD(TOT-SUB, 6) TO TOTAL-MTD-PUR-AMT
                 MOVE TYPE-VOL-NO-YTD(TOT-SUB, 6) TO TOTAL-YTD-PUR-NO
                 MOVE TYPE-VOL-AMT-YTD(TOT-SUB, 6) TO TOTAL-YTD-PUR-AMT
                 MOVE TYPE-VOL-NO-MTD(TOT-SUB, 1) TO TOTAL-MTD-TODL
                 MOVE TYPE-VOL-NO-YTD(TOT-SUB, 1) TO TOTAL-YTD-TODL
                 MOVE TYPE-VOL-AMT-YTD(TOT-SUB, 1) TO TOTAL-YTD-TODL-AMT
                 GO TO PRINT-DEALER-LINE8.
           MOVE 7 TO DL-SUB.
           MOVE "OTHERS" TO D2-DL-NAME.
           MOVE SPACES TO D2-MFGCODE.
           MOVE OTHER-OUTSUNIT TO D2-OUTSUNIT.
           MOVE OTHER-OUTSAMT TO D2-OUTSAMT.
           MOVE OTHER-MTD-PUR-NO TO D2-MTD-PUR-NO.
           MOVE OTHER-MTD-PUR-AMT TO D2-MTD-PUR-AMT.
           MOVE OTHER-YTD-PUR-NO TO D2-YTD-PUR-NO.
           MOVE OTHER-YTD-PUR-AMT TO D2-YTD-PUR-AMT.
           MOVE OTHER-MTD-TODL TO D2-MTD-TODL.
           IF OTHER-MTD-TODL NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 OTHER-MTD-TODL / OTHER-OUTSUNIT * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.
           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D2-TODL-PCT1.
           MOVE OTHER-YTD-TODL TO D2-YTD-TODL.
           IF OTHER-YTD-TODL NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 OTHER-YTD-TODL / OTHER-OUTSUNIT * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.
           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D2-TODL-PCT2.
           MOVE OTHER-YTD-TODL-AMT TO D2-YTD-TODL-AMT.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *----------------------------------------------------------------
       PRINT-DEALER-LINE8.
           ADD 1 TO DL-SUB.
           MOVE "TOTAL" TO D2-DL-NAME.
           MOVE SPACES TO D2-MFGCODE.
           MOVE TOTAL-OUTSUNIT TO D2-OUTSUNIT.
           MOVE TOTAL-OUTSAMT TO D2-OUTSAMT.
           MOVE TOTAL-MTD-PUR-NO TO D2-MTD-PUR-NO.
           MOVE TOTAL-MTD-PUR-AMT TO D2-MTD-PUR-AMT.
           MOVE TOTAL-YTD-PUR-NO TO D2-YTD-PUR-NO.
           MOVE TOTAL-YTD-PUR-AMT TO D2-YTD-PUR-AMT.
           MOVE TOTAL-MTD-TODL TO D2-MTD-TODL.
           IF TOTAL-MTD-TODL NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 TOTAL-MTD-TODL / TOTAL-OUTSUNIT * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.
           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D2-TODL-PCT1.
           MOVE TOTAL-YTD-TODL TO D2-YTD-TODL.
           IF TOTAL-YTD-TODL NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 TOTAL-YTD-TODL / TOTAL-OUTSUNIT * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.
           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D2-TODL-PCT2.
           MOVE TOTAL-YTD-TODL-AMT TO D2-YTD-TODL-AMT.

      *----------------------------------------------------------------
       WRITE-DEALER-LINE.
           IF (LN-FEED + LN-COUNT) = 63
              ADD 1 TO LN-COUNT.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF DL-SUB > 7
              GO TO HEAD-UP-PART3.

           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           IF MORE-DEALERS = "Y"
              GO TO NEXT-WK
           ELSE
              GO TO PRINT-DEALER-LINE.

       HEAD-UP-PART3.
           MOVE SPACES TO VERT-BAR-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DASH-LINE TO DETAIL-LINE.
           MOVE 0 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      * HEAD UP NEW PAGE BEFORE DELINQUENCY IF STILL ON PAGE 1
           IF PAGE-NUMBER = 1
              MOVE 99 TO LN-COUNT.
           IF DIRECT-LN-CLASS-FG = "N"
              GO TO SKIP-DIRECT-DELINQUENCY.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "|" TO D-BAR(12) D-BAR(38) D-BAR(63) D-BAR(88)
                                 D-BAR(113).

           MOVE 1 TO LN-FEED.
           MOVE DETAIL3-HEAD1 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE "DELINQUENT" TO D3-MONTH-PROMPT.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "|" TO D-BAR(20) D-BAR(30) D-BAR(45) D-BAR(55)
                                 D-BAR(70) D-BAR(80) D-BAR(95)
                                 D-BAR(105) D-BAR(120) D-BAR(130).

           MOVE "LOANS" TO DETAIL3-PROMPT2.
           MOVE DETAIL3-HEAD2 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DETAIL3-HEAD3 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 0 TO LN-FEED.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 1 TO DEL-SUB.
           MOVE 2 TO AGE-SUB.

      *----------------------------------------------------------------
       PRINT-DEL-LOANS.
           IF AGE-SUB = 2
              IF DEL-FLAG = "N"
                 MOVE "BEG MONTH  " TO D3-MONTH-PROMPT
              ELSE
                 MOVE "BEG THIS MO" TO D3-MONTH-PROMPT
           ELSE
              IF DEL-FLAG = "N"
                 MOVE "CURRENT MO " TO D3-MONTH-PROMPT
              ELSE
                 MOVE "BEG LAST MO" TO D3-MONTH-PROMPT.

           MOVE LOAN-DEL-NO(TOT-SUB, AGE-SUB, DEL-SUB) TO
              D3-DEL-NO(DEL-SUB).

           MOVE LOAN-DEL-AMT(TOT-SUB, AGE-SUB, DEL-SUB) TO
              D3-DEL-AMT(DEL-SUB).

           IF LOAN-DEL-AMT(TOT-SUB, AGE-SUB, DEL-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 LOAN-DEL-AMT(TOT-SUB, AGE-SUB, DEL-SUB) /
                 CLASS-GAIN-AMT-CUR(TOT-SUB, TOTAL-LN-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.

           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.

           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D3-DEL-PCT1(DEL-SUB).
           ADD 1 TO DEL-SUB.

           IF DEL-SUB < 6
              GO TO PRINT-DEL-LOANS.

           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF AGE-SUB = 2
              MOVE 1 TO AGE-SUB
              MOVE 1 TO DEL-SUB
              GO TO PRINT-DEL-LOANS.

      *----------------------------------------------------------------
       HEAD-UP-PART4.
           MOVE SPACES TO VERT-BAR-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DASH-LINE TO DETAIL-LINE.
           MOVE 0 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       SKIP-DIRECT-DELINQUENCY.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "|" TO D-BAR(12) D-BAR(38) D-BAR(63) D-BAR(88)
                                 D-BAR(113).

           MOVE 1 TO LN-FEED.
           MOVE DETAIL3-HEAD1 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE "DELINQUENT" TO D3-MONTH-PROMPT.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "|" TO D-BAR(20) D-BAR(30) D-BAR(45) D-BAR(55)
                                 D-BAR(70) D-BAR(80) D-BAR(95)
                                 D-BAR(105) D-BAR(120) D-BAR(130).

           MOVE "SALES FIN." TO DETAIL3-PROMPT2.
           MOVE DETAIL3-HEAD2 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DETAIL3-HEAD3 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 0 TO LN-FEED.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           MOVE 1 TO DEL-SUB.
           MOVE 2 TO AGE-SUB.

      *----------------------------------------------------------------
       PRINT-DEL-SALEFIN.
           IF AGE-SUB = 2
              IF DEL-FLAG = "N"
                 MOVE "BEG MONTH  " TO D3-MONTH-PROMPT
              ELSE
                 MOVE "BEG THIS MO" TO D3-MONTH-PROMPT
           ELSE
              IF DEL-FLAG = "N"
                 MOVE "CURRENT MO " TO D3-MONTH-PROMPT
              ELSE
                 MOVE "BEG LAST MO" TO D3-MONTH-PROMPT.

           MOVE SALEFIN-DEL-NO(TOT-SUB, AGE-SUB, DEL-SUB) TO
              D3-DEL-NO(DEL-SUB).

           MOVE SALEFIN-DEL-AMT(TOT-SUB, AGE-SUB, DEL-SUB) TO
              D3-DEL-AMT(DEL-SUB).

           IF SALEFIN-DEL-AMT(TOT-SUB, AGE-SUB, DEL-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 SALEFIN-DEL-AMT(TOT-SUB, AGE-SUB, DEL-SUB) /
                 CLASS-GAIN-AMT-CUR(TOT-SUB, TOTAL-SF-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.

           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.

           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D3-DEL-PCT1(DEL-SUB).
           ADD 1 TO DEL-SUB.

           IF DEL-SUB < 6
              GO TO PRINT-DEL-SALEFIN.

           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF AGE-SUB = 2
              MOVE 1 TO DEL-SUB
              MOVE 1 TO AGE-SUB
              GO TO PRINT-DEL-SALEFIN.

      *----------------------------------------------------------------
       HEAD-UP-BANKRUPTS.
           MOVE SPACES TO VERT-BAR-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DASH-LINE TO DETAIL-LINE.
           MOVE 0 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           IF DIRECT-LN-CLASS-FG = "N"
              GO TO SKIP-DIRECT-DEL-BNKRPT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "|" TO D-BAR(12) D-BAR(38) D-BAR(63) D-BAR(88)
                                 D-BAR(113).

           MOVE 1 TO LN-FEED.
           MOVE DETAIL3-HEAD1 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE "DELINQUENT" TO D3-MONTH-PROMPT.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "|" TO D-BAR(20) D-BAR(30) D-BAR(45) D-BAR(55)
                                 D-BAR(70) D-BAR(80) D-BAR(95)
                                 D-BAR(105) D-BAR(120) D-BAR(130).

           MOVE "BANKRUPT" TO DETAIL3-PROMPT2.
           MOVE DETAIL3-HEAD2 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "LOANS" TO DETAIL3-PROMPT3.
           MOVE DETAIL3-HEAD3 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 0 TO LN-FEED.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 1 TO DEL-SUB.
           MOVE 2 TO AGE-SUB.

      *----------------------------------------------------------------
       PRINT-BNKRPT-DEL-LOANS.
           IF AGE-SUB = 2
              IF DEL-FLAG = "N"
                 MOVE "BEG MONTH  " TO D3-MONTH-PROMPT
              ELSE
                 MOVE "BEG THIS MO" TO D3-MONTH-PROMPT
           ELSE
              IF DEL-FLAG = "N"
                 MOVE "CURRENT MO " TO D3-MONTH-PROMPT
              ELSE
                 MOVE "BEG LAST MO" TO D3-MONTH-PROMPT.

           MOVE LOAN-BNKRPT-NO(TOT-SUB, AGE-SUB, DEL-SUB) TO
              D3-DEL-NO(DEL-SUB).

           MOVE LOAN-BNKRPT-AMT(TOT-SUB, AGE-SUB, DEL-SUB) TO
              D3-DEL-AMT(DEL-SUB).

           IF LOAN-BNKRPT-AMT(TOT-SUB, AGE-SUB, DEL-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 LOAN-BNKRPT-AMT(TOT-SUB, AGE-SUB, DEL-SUB) /
                 CLASS-GAIN-AMT-CUR(TOT-SUB, TOTAL-LN-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.

           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.

           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D3-DEL-PCT1(DEL-SUB).
           ADD 1 TO DEL-SUB.

           IF DEL-SUB < 6
              GO TO PRINT-BNKRPT-DEL-LOANS.

           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF AGE-SUB = 2
              MOVE 1 TO AGE-SUB
              MOVE 1 TO DEL-SUB
              GO TO PRINT-BNKRPT-DEL-LOANS.

      *----------------------------------------------------------------
       HEAD-UP-PART4-BNKRPT.
           MOVE SPACES TO VERT-BAR-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DASH-LINE TO DETAIL-LINE.
           MOVE 0 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       SKIP-DIRECT-DEL-BNKRPT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "|" TO D-BAR(12) D-BAR(38) D-BAR(63) D-BAR(88)
                                 D-BAR(113).

           MOVE 1 TO LN-FEED.
           MOVE DETAIL3-HEAD1 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE "DELINQUENT" TO D3-MONTH-PROMPT.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "|" TO D-BAR(20) D-BAR(30) D-BAR(45) D-BAR(55)
                                 D-BAR(70) D-BAR(80) D-BAR(95)
                                 D-BAR(105) D-BAR(120) D-BAR(130).

           MOVE "BANKRUPT" TO DETAIL3-PROMPT2.
           MOVE DETAIL3-HEAD2 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SALES FIN." TO DETAIL3-PROMPT3.
           MOVE DETAIL3-HEAD3 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 0 TO LN-FEED.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           MOVE 1 TO DEL-SUB.
           MOVE 2 TO AGE-SUB.

      *----------------------------------------------------------------
       PRINT-BNKRPT-DEL-SALEFIN.
           IF AGE-SUB = 2
              IF DEL-FLAG = "N"
                 MOVE "BEG MONTH  " TO D3-MONTH-PROMPT
              ELSE
                 MOVE "BEG THIS MO" TO D3-MONTH-PROMPT
           ELSE
              IF DEL-FLAG = "N"
                 MOVE "CURRENT MO " TO D3-MONTH-PROMPT
              ELSE
                 MOVE "BEG LAST MO" TO D3-MONTH-PROMPT.

           MOVE SALEFIN-BNKRPT-NO(TOT-SUB, AGE-SUB, DEL-SUB) TO
              D3-DEL-NO(DEL-SUB).

           MOVE SALEFIN-BNKRPT-AMT(TOT-SUB, AGE-SUB, DEL-SUB) TO
              D3-DEL-AMT(DEL-SUB).

           IF SALEFIN-BNKRPT-AMT(TOT-SUB, AGE-SUB, DEL-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 SALEFIN-BNKRPT-AMT(TOT-SUB, AGE-SUB, DEL-SUB) /
                 CLASS-GAIN-AMT-CUR(TOT-SUB, TOTAL-SF-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.

           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.

           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D3-DEL-PCT1(DEL-SUB).
           ADD 1 TO DEL-SUB.

           IF DEL-SUB < 6
              GO TO PRINT-BNKRPT-DEL-SALEFIN.

           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF AGE-SUB = 2
              MOVE 1 TO DEL-SUB
              MOVE 1 TO AGE-SUB
              GO TO PRINT-BNKRPT-DEL-SALEFIN.

      *----------------------------------------------------------------
       HEAD-UP-PART5.
           MOVE SPACES TO VERT-BAR-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DASH-LINE TO DETAIL-LINE.
           MOVE 0 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "|" TO D-BAR(12) D-BAR(23)
                       D-BAR(34) D-BAR(45) D-BAR(46)
                       D-BAR(60) D-BAR(71) D-BAR(82)
                       D-BAR(96) D-BAR(97) D-BAR(110).

           MOVE 1 TO LN-FEED.
           MOVE DETAIL5-HEAD1 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 0 TO LN-FEED.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "|" TO D-BAR(125).
           MOVE 1 TO CLASS-SUB.

      *----------------------------------------------------------------
       PART5-CLASS-LOOP.
           IF CLASS-SUB = 1
              MOVE "LOANS" TO D5-PROMPT1 D5-PROMPT2
              MOVE TOTAL-LN-SUB TO SUBTOTAL-SUB
           ELSE IF CLASS-SUB = 2
              MOVE "SALES" TO D5-PROMPT1 D5-PROMPT2
              MOVE TOTAL-SF-SUB TO SUBTOTAL-SUB
           ELSE
              MOVE "TOTAL" TO D5-PROMPT1 D5-PROMPT2
              MOVE TOTAL-ALL-SUB TO SUBTOTAL-SUB.

           COMPUTE AMT-WORK = PL-VOL-AMT-MTD(TOT-SUB, CLASS-SUB)
                            + PART-CHGOFF-MTD(TOT-SUB, CLASS-SUB).
           MOVE AMT-WORK TO D5-CHGOFF-MTD.

           COMPUTE AMT-WORK = PL-VOL-AMT-YTD(TOT-SUB, CLASS-SUB)
                            + PART-CHGOFF-YTD(TOT-SUB, CLASS-SUB).
           MOVE AMT-WORK TO D5-CHGOFF-YTD.

           IF CLASS-GAIN-AMT-CUR(TOT-SUB, SUBTOTAL-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
              AMT-WORK / CLASS-GAIN-AMT-CUR(TOT-SUB, SUBTOTAL-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.
           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D5-PCT1.

           MOVE PL-RECV-AMT-MTD(TOT-SUB, CLASS-SUB)
              TO D5-RECOVER-MTD.

           MOVE PL-RECV-AMT-YTD(TOT-SUB, CLASS-SUB)
              TO D5-RECOVER-YTD.

           IF CLASS-GAIN-AMT-CUR(TOT-SUB, SUBTOTAL-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                (PL-VOL-AMT-YTD(TOT-SUB, CLASS-SUB)
               - PL-RECV-AMT-YTD(TOT-SUB, CLASS-SUB)) /
                    CLASS-GAIN-AMT-CUR(TOT-SUB, SUBTOTAL-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           IF PCT-WORK > 0 AND < .1
              MOVE .1 TO PCT-WORK.
           COMPUTE PCT-WORKER ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER TO D5-PCT2.

           IF CLASS-SUB = 1
              MOVE "  COLLECTED      WAIVED" TO D5-LATE-AMOUNTS.

           IF CLASS-SUB = 2
              MOVE " NO. ACCTS" TO D5-LATE-PROMPT
              MOVE LATE-NO-COL(TOT-SUB) TO D5-NO-COLLECTED
              MOVE LATE-NO-WAIVED(TOT-SUB) TO D5-NO-WAIVED.

           IF CLASS-SUB = 3
              MOVE " $ AMOUNT" TO D5-LATE-PROMPT
              MOVE LATE-AMT-COL(TOT-SUB) TO D5-AMT-COLLECTED
              MOVE LATE-AMT-WAIVED(TOT-SUB) TO D5-AMT-WAIVED.

           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF CLASS-SUB < 3
              ADD 1 TO CLASS-SUB
              GO TO PART5-CLASS-LOOP.

           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE 0 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *----------------------------------------------------------------
       HEAD-UP-REPO.
      *----------------------------------------------------------------
           MOVE SPACES TO VERT-BAR-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE DASH-LINE TO DETAIL-LINE.
           MOVE 0 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "|" TO D-BAR(10) D-BAR(30) D-BAR(54) D-BAR(70)
                       D-BAR(92).
           MOVE DETAIL-REPO-HEAD1 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE 0 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           MOVE "|" TO D-BAR(10) D-BAR(19) D-BAR(30) D-BAR(42)
                       D-BAR(54) D-BAR(62) D-BAR(70) D-BAR(81)
                       D-BAR(92).

           MOVE DETAIL-REPO-HEAD2 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 0 TO LN-FEED.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           MOVE 1 TO REPO-SUB.

      *----------------------------------------------------------------
       PRINT-REPO-LOOP.

           MOVE REPO-PROMPT(REPO-SUB) TO D-REPO-PROMPT.
      *JTG BOGUS (REDEEMED) NUMBER SO DON'T PRINT:
           IF REPO-SUB NOT = 2
              MOVE REPO-GAIN-NO-CUR(TOT-SUB, REPO-SUB)
                                 TO D-REPO-GAIN-NO-CUR.
      * DON'T PRINT MTD/YTD GAIN LOSS UNITS OR AMTS FOR MERCURY
      *    MOVE REPO-GAIN-NO-MTD(TOT-SUB, REPO-SUB)
      *                          TO D-REPO-GAIN-NO-MTD.
      *    MOVE REPO-GAIN-NO-YTD(TOT-SUB, REPO-SUB)
      *                          TO D-REPO-GAIN-NO-YTD.
      *JTG BOGUS (REDEEMED) NUMBER SO DON'T PRINT:
           IF REPO-SUB NOT = 2
              MOVE REPO-GAIN-AMT-CUR(TOT-SUB, REPO-SUB)
                                 TO D-REPO-GAIN-AMT-CUR.
      * DON'T PRINT MTD/YTD GAIN LOSS UNITS OR AMTS FOR MERCURY
      *    MOVE REPO-GAIN-AMT-MTD(TOT-SUB, REPO-SUB)
      *                          TO D-REPO-GAIN-AMT-MTD.
      *    MOVE REPO-GAIN-AMT-YTD(TOT-SUB, REPO-SUB)
      *                          TO D-REPO-GAIN-AMT-YTD.
      *    DON'T PRINT VOLUME OR FREQUENCY EXCEPT FOR ON HAND:
      *    IF REPO-SUB > 1
      *       GO TO WRITE-REPO-LINE.
           MOVE REPO-VOL-NO-MTD(TOT-SUB, REPO-SUB)
                                 TO D-REPO-VOL-NO-MTD.
           MOVE REPO-VOL-NO-YTD(TOT-SUB, REPO-SUB)
                                 TO D-REPO-VOL-NO-YTD.

           MOVE 0 TO PCT-WORK.
           IF REPO-SUB = 1
             IF CLASS-GAIN-NO-CUR(TOT-SUB, TOTAL-SF-SUB) NOT = 0
               COMPUTE PCT-WORK ROUNDED =
                 (REPO-VOL-NO-MTD(TOT-SUB, REPO-SUB) * 12) /
                 CLASS-GAIN-NO-CUR(TOT-SUB, TOTAL-SF-SUB) * 100.
           IF PCT-WORK > 0 AND < .01
              MOVE .01 TO PCT-WORK.
           COMPUTE PCT-WORKER2 ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER2 TO D-REPO-PCT-FREQ-MTD.

           MOVE 0 TO PCT-WORK.
           IF REPO-SUB = 1
             IF CLASS-GAIN-NO-CUR(TOT-SUB, TOTAL-SF-SUB) NOT = 0
                IF ELAPSED-MNTHS < 12
                  COMPUTE PCT-WORK ROUNDED =
                   (REPO-VOL-NO-YTD(TOT-SUB, REPO-SUB) -
                    REPO-VOL-NO-MTD(TOT-SUB, REPO-SUB)) /
                    ELAPSED-MNTHS * 12 /
                    CLASS-GAIN-NO-CUR(TOT-SUB, TOTAL-SF-SUB) * 100
      * ELAPSED MONTHS OF 12 - IT'S PERIOD 1, SO USE END OF LAST YEAR
                ELSE
                  COMPUTE PCT-WORK ROUNDED =
                     REPO-VOL-NO-LYR(TOT-SUB, REPO-SUB) /
                     CLASS-GAIN-NO-CUR(TOT-SUB, TOTAL-SF-SUB) * 100.
           IF PCT-WORK > 0 AND < .01
              MOVE .01 TO PCT-WORK.
           COMPUTE PCT-WORKER2 ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER2 TO D-REPO-PCT-FREQ-YTD.

           MOVE REPO-VOL-AMT-MTD(TOT-SUB, REPO-SUB)
                                 TO D-REPO-VOL-AMT-MTD.
           MOVE REPO-VOL-AMT-YTD(TOT-SUB, REPO-SUB)
                                 TO D-REPO-VOL-AMT-YTD.

       WRITE-REPO-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           ADD 1 TO REPO-SUB.

      *ADDED FOR MERCURY WANTS TO PRINT ON HAND + SOLD
           IF GP-PROGRP-PRINT-REDEEM = "N"
              IF REPO-SUB = 2
                 ADD 1 TO REPO-SUB.
      *       GO TO SKIP-REDEEMED-SOLD.

           IF REPO-SUB < 4
              GO TO PRINT-REPO-LOOP.

       SKIP-REDEEMED-SOLD.
           MOVE SPACES TO VERT-BAR-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE 0 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE DASH-LINE TO DETAIL-LINE.
           MOVE 0 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *----------------------------------------------------------------
       HEAD-UP-P4P5.
      *----------------------------------------------------------------
      *  SKIPPING P4P5 TO MAKE ROOM FOR BANKRUPT DELINQ, 6/98 BLV:
           GO TO HEAD-UP-DEALER-RESERVE.

           MOVE SPACES TO VERT-BAR-LINE.
           MOVE 1 TO LN-FEED.
           MOVE "|" TO D-BAR(16) D-BAR(29) D-BAR(42) D-BAR(56)
                       D-BAR(70) D-BAR(88) D-BAR(116).
           MOVE DETAIL-P4P5-HEAD1 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           MOVE "|" TO D-BAR(79) D-BAR(102).

           MOVE DETAIL-P4P5-HEAD2 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 0 TO LN-FEED.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO PL-SUB.

      *----------------------------------------------------------------
       PRINT-P4P5-LOOP.

           MOVE 1 TO LN-FEED.
           MOVE P4P5-PROMPT(PL-SUB) TO D-P45-PROMPT1.
           MOVE "PRINCIPAL" TO D-P45-PROMPT2.
           MOVE P45-VOL-PRIN-MTD(TOT-SUB, PL-SUB)
                                 TO D-P45-VOL-AMT-MTD.
           MOVE P45-VOL-PRIN-YTD(TOT-SUB, PL-SUB)
                                 TO D-P45-VOL-AMT-YTD.
           COMPUTE NO-WORK ROUNDED = P45-VOL-PRIN-MTD(TOT-SUB, PL-SUB) /
                                     P45-VOL-NO-MTD(TOT-SUB, PL-SUB).
           MOVE NO-WORK TO D-P45-MTD-AVG-LOSS.
           COMPUTE NO-WORK ROUNDED = P45-VOL-PRIN-YTD(TOT-SUB, PL-SUB) /
                                     P45-VOL-NO-YTD(TOT-SUB, PL-SUB).
           MOVE NO-WORK TO D-P45-YTD-AVG-LOSS.
           MOVE P45-RECV-PRIN-MTD(TOT-SUB, PL-SUB)
                                 TO D-P45-VOL-RECV-MTD.
           MOVE P45-RECV-PRIN-YTD(TOT-SUB, PL-SUB)
                                 TO D-P45-VOL-RECV-YTD.
           PERFORM WRITE-DETAIL-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE P4P5-PROMPT(PL-SUB) TO D-P45-PROMPT1.
           MOVE "OTHER" TO D-P45-PROMPT2.
           MOVE P45-VOL-O2-MTD(TOT-SUB, PL-SUB)
                                 TO D-P45-VOL-AMT-MTD.
           MOVE P45-VOL-O2-YTD(TOT-SUB, PL-SUB)
                                 TO D-P45-VOL-AMT-YTD.
           COMPUTE NO-WORK ROUNDED = P45-VOL-O2-MTD(TOT-SUB, PL-SUB) /
                                     P45-VOL-NO-MTD(TOT-SUB, PL-SUB).
           MOVE NO-WORK TO D-P45-MTD-AVG-LOSS.
           COMPUTE NO-WORK ROUNDED = P45-VOL-O2-YTD(TOT-SUB, PL-SUB) /
                                     P45-VOL-NO-YTD(TOT-SUB, PL-SUB).
           MOVE NO-WORK TO D-P45-YTD-AVG-LOSS.
           MOVE P45-RECV-O2-MTD(TOT-SUB, PL-SUB)
                                 TO D-P45-VOL-RECV-MTD.
           MOVE P45-RECV-O2-YTD(TOT-SUB, PL-SUB)
                                 TO D-P45-VOL-RECV-YTD.
           PERFORM WRITE-DETAIL-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE P4P5-PROMPT(PL-SUB) TO D-P45-PROMPT1.
           MOVE "TOTAL" TO D-P45-PROMPT2.
           MOVE P45-VOL-NO-MTD(TOT-SUB, PL-SUB) TO D-P45-VOL-NO-MTD.
           MOVE P45-VOL-NO-YTD(TOT-SUB, PL-SUB) TO D-P45-VOL-NO-YTD.
           MOVE P45-VOL-TOT-MTD(TOT-SUB, PL-SUB) TO D-P45-VOL-AMT-MTD.
           COMPUTE NO-WORK ROUNDED = P45-VOL-TOT-MTD(TOT-SUB, PL-SUB) /
                                     P45-VOL-NO-MTD(TOT-SUB, PL-SUB).
           MOVE NO-WORK TO D-P45-MTD-AVG-LOSS.
           MOVE P45-VOL-TOT-YTD(TOT-SUB, PL-SUB) TO D-P45-VOL-AMT-YTD.
           COMPUTE NO-WORK ROUNDED = P45-VOL-TOT-YTD(TOT-SUB, PL-SUB) /
                                     P45-VOL-NO-YTD(TOT-SUB, PL-SUB).
           MOVE NO-WORK TO D-P45-YTD-AVG-LOSS.
           MOVE P45-RECV-TOT-MTD(TOT-SUB, PL-SUB) TO D-P45-VOL-RECV-MTD.
           MOVE P45-RECV-TOT-YTD(TOT-SUB, PL-SUB) TO D-P45-VOL-RECV-YTD.

           PERFORM WRITE-DETAIL-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           ADD 1 TO PL-SUB.
           IF PL-SUB < 3
              MOVE DASH-LINE TO DETAIL-LINE
              MOVE 0 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              GO TO PRINT-P4P5-LOOP.

           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE 0 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *----------------------------------------------------------------
       HEAD-UP-DEALER-RESERVE.
      *----------------------------------------------------------------
           MOVE SPACES TO VERT-BAR-LINE.
           MOVE 1 TO LN-FEED.
           MOVE "|" TO D-BAR(32) D-BAR(44)  D-BAR(45)
                       D-BAR(66) D-BAR(67)  D-BAR(81)
                       D-BAR(97) D-BAR(110) D-BAR(111).
           MOVE RESERVE-HEAD1 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 0 TO LN-FEED.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "|" TO D-BAR(16).
           MOVE 1 TO LN-FEED.
           MOVE "CURRENT" TO D-RESERVE-PROMPT.
           MOVE DLRV-GAIN-CUR(TOT-SUB) TO D-RESERVE-TOTAL.

           IF CLASS-GAIN-AMT-CUR(TOT-SUB, TOTAL-SF-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                      DLRV-GAIN-CUR(TOT-SUB) /
               CLASS-GAIN-AMT-CUR(TOT-SUB, TOTAL-SF-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           IF PCT-WORK > 0 AND < .01
              MOVE .01 TO PCT-WORK.
           COMPUTE PCT-WORKER2 ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER2 TO D-RESERVE-OUTS-PCT.

           IF CLASS-GAIN-AMT-CUR(TOT-SUB, TOTAL-SF-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                     ((P45-VOL-TOT-MTD(TOT-SUB, 1) +
                      P45-VOL-TOT-MTD(TOT-SUB, 2)) * 12) /
                 CLASS-GAIN-AMT-CUR(TOT-SUB, TOTAL-SF-SUB) * 100
           ELSE
              MOVE 0 TO PCT-WORK.
           IF PCT-WORK > 0 AND < .01
              MOVE .01 TO PCT-WORK.
           COMPUTE PCT-WORKER2 ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER2 TO D-RESERVE-LOSS-PCT.

           MOVE "DLR DISCOUNT" TO D-DLDISC-PROMPT.
           MOVE DLDISC-MTD-VOL(TOT-SUB) TO D-DLDISC-MTD-VOL.
           MOVE 0 TO PCT-WORK.
           IF CLASS-VOL-AMT-MTD(TOT-SUB, TOTAL-SF-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 DLDISC-MTD-VOL(TOT-SUB) /
                 CLASS-VOL-AMT-MTD(TOT-SUB, TOTAL-SF-SUB) * 100.
           IF PCT-WORK > 0 AND < .01
              MOVE .01 TO PCT-WORK.
           COMPUTE PCT-WORKER2 ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER2 TO D-DLDISC-PCT.

           PERFORM WRITE-DETAIL-LINE.
      *----------------------------------------------------------------

           MOVE " " TO D-BAR(67)  D-BAR(81) D-BAR(97)
                       D-BAR(110) D-BAR(111).
           MOVE 0 TO LN-FEED.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "LAST MONTH END" TO D-RESERVE-PROMPT.
           MOVE DLRV-GAIN-ELM(TOT-SUB) TO D-RESERVE-TOTAL.

           MOVE 0 TO PCT-WORK.
           IF CLASS-GAIN-AMT-ELM(TOT-SUB, TOTAL-SF-SUB) NOT = 0
              COMPUTE PCT-WORK ROUNDED =
                 DLRV-GAIN-ELM(TOT-SUB) /
                 CLASS-GAIN-AMT-ELM(TOT-SUB, TOTAL-SF-SUB) * 100.
           IF PCT-WORK > 0 AND < .01
              MOVE .01 TO PCT-WORK.
           COMPUTE PCT-WORKER2 ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER2 TO D-RESERVE-OUTS-PCT.

           MOVE 0 TO PCT-WORK.
           IF CLASS-GAIN-AMT-ELM(TOT-SUB, TOTAL-SF-SUB) NOT = 0
              IF ELAPSED-MNTHS < 12
                 COMPUTE PCT-WORK ROUNDED =
                    ((P45-VOL-TOT-YTD(TOT-SUB, 1) +
                      P45-VOL-TOT-YTD(TOT-SUB, 2)) -
                     (P45-VOL-TOT-MTD(TOT-SUB, 1) +
                      P45-VOL-TOT-MTD(TOT-SUB, 2))) /
                      ELAPSED-MNTHS * 12 /
                 CLASS-GAIN-AMT-ELM(TOT-SUB, TOTAL-SF-SUB) * 100
      * ELAPSED MONTHS OF 12 - IT'S PERIOD 1, SO USE END OF LAST YEAR
              ELSE
                 COMPUTE PCT-WORK ROUNDED =
                     (P45-VOL-TOT-LYR(TOT-SUB, 1) +
                      P45-VOL-TOT-LYR(TOT-SUB, 2)) /
                 CLASS-GAIN-AMT-ELM(TOT-SUB, TOTAL-SF-SUB) * 100.
           IF PCT-WORK > 0 AND < .01
              MOVE .01 TO PCT-WORK.
           COMPUTE PCT-WORKER2 ROUNDED = PCT-WORK * 1.
           MOVE PCT-WORKER2 TO D-RESERVE-LOSS-PCT.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *----------------------------------------------------------------
           MOVE 0 TO LN-FEED.
           MOVE STAR-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
       ZERO-TOTALS.
           MOVE LOW-VALUES TO TOTAL-TAB(TOT-SUB).

      *----------------------------------------------------------------
       CLEAR-WK.
           MOVE 0 TO WK-BRANCH WK-DLNO WK-LEVEL.
           PERFORM START-WK-FILE.

      *----------------------------------------------------------------
       CLEAR-NEXT-WK.
           PERFORM READ-WK-FILE-NEXT.

           IF IO-FG NOT = 0
              GO TO EXIT-PRINT-TOTALS.

           IF WK-LEVEL NOT = TOT-SUB
              GO TO CLEAR-NEXT-WK.

           PERFORM DELETE-WK-FILE.
           GO TO CLEAR-NEXT-WK.

       EXIT-PRINT-TOTALS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > 63
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ADD LN-FEED TO LN-COUNT.
           MOVE VERT-BAR-LINE TO PR-REC.
           MOVE 0 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

       WRITE-DETAIL-EXIT.
           EXIT.

      ******************************************************************
       GET-BRANCH-NAME SECTION.
      ******************************************************************
           MOVE GP-SHORT-NAME TO H-GRNAME.
           INSPECT H-GRNAME REPLACING FIRST "   "
              BY " -?".
           INSPECT H-GRNAME REPLACING FIRST "               "
              BY BR-CITY.
           INSPECT H-GRNAME REPLACING FIRST "  " BY ",?".
           INSPECT H-GRNAME REPLACING FIRST "  "
              BY BR-STATE.
           INSPECT H-GRNAME REPLACING ALL "?" BY " ".

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO H-TIME.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE SPACES TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE STAR-LINE TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 1 TO LN-FEED.

      ************************************************************
       CLASS-SCAN SECTION.
      ************************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
             MOVE "SNNN020CLASS1." TO VDU
           ELSE
             MOVE "SNNN020CLASS2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO PROGRP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PROGRP-SYSDATE.
           DISPLAY PROGRP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE PROGRP-FORM-FIELDS TO WR-BUF.
           MOVE PROGRP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE PROGRP-HELP--FILE TO HELP-LINK-FILE.
           MOVE PROGRP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/PROGRP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".

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
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPBYGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBY1RN.CPY".
       COPY "LIBLP/LPDS1RN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
      *----------------------------------------------------------------
       START-WK2-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE WK-IFN TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           START WK-FILE KEY NOT > WK-KEY2.
           UNLOCK WK-FILE.

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
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *----------------------------------------------------------------
       EXIT-PROG.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

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
              DESTROY PROGRP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
