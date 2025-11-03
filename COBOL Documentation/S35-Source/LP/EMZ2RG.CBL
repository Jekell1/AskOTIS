       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EMZ2RG.
       DATE-WRITTEN. 022896.
      *****************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  MONTHLY 'Z2' TRANSACTION REGISTER
      *                   TR1-CODE = "P"
      *
      * REV:
      *  042995 SLC ADDED PLTYPE LOGIC
      *  061195 BLL FIX BAD REDEFINITION OF WK-KEY
      *  072895 BAH PRINT TP-DLRVCHGS-ADJ(1-3)
      *  073195 BLL ADD BRANCH RANGE SECURITY CHECKING
      *  080895 JTG ADDED 'Z2' TO ACCUMULATE IN TOTALS FOR TRAMT
      *  090595 KEC ADDED LOCAL-SPOOLING BATCH LOGIC
      *  091595 KEC CHANGED MILITARY TIME TO AM/PM ON HEADER.
      *  091595 KEC ADDED CHECK FOR VALID BRANCH # FOR CLASS TOTALS.
      *  BAH 092895 RECORDS-FOUND INITIALIZED TO "N" INSTEAD OF " "
      *  100695 JFF ADDED CLASS SCAN
      *  KEC 101395 COMMENTED OUT DEFAULT BRANCH SETTING IN INITIZATION.
      *  JFF 111395 CHANGED FORM TO INCLUDE LOCAL-SPOOLING QUESTION.
      *  MJD 112195 ADDED LOGIC TO PRINT SEPARATE TOTALS FOR PAYMENTS
      *             POSTED FROM OTHER BRANCHES.
      *  KEC 112895 IF TP-OTHBR-FG = "C"; THEN PRINT "INTER-OFFICE
      *             PAYMENT" INTO THE NAME FIELD (D-NAME).
      *  KEC 113095 FIXED LOCAL-SPOOL FOR IF A BRANCH HAS NO DATA.
      *  JTG 113095 ADDED SELECTION FOR ALL OR ONE CASH DRAWER
      *  JFF 121395 CHANGED "IF LOCAL-PRINT-FG = N" TO
      *             "IF LOCAL-PRINT-FG NOT = Y" IN INITIALIZATION.
      *  JFF 010996 ADDED PV-PAID ADJ AND PV-HELD ADJ TO REPORT.
      *  JFF 031196 ADDED PRINT-RANGE-LINES SECTION.
      *  MJD 071896 ADDED WEEKLY UNIT PERIOD LOGIC FOR 6 DIGIT PDTH DATES
      *  JTG 060497 ADDED TEST FOR "R3"
      *  GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  BAH 030698 CHANGED WK2-LINE TO PIC 9(3) FROM 99 BECAUSE TP-LINE
      *             PIC 99 COMP ACTUALLY STORES A 3 DIGIT NUMBER !.
      *  BAH 062598 CHANGED TP-TILL PIC 9 TO TP-TILLNO PIC X
      *  GLB 990610 MODIFY RANKING-DATES ROUTINE TO TEST SPECIFICALLY
      *             FOR 0 DATES IN ORDER TO COMPENSATE FOR "CMPDAT"
      *  GLF 082499 ADD ARCHIVE PATH INFO. FOR TR FILES NOT FOUND IN DATA
      *             SET FOR SELECTED MONTH AND YEAR
      *  BLV 990826 FIX ARCHIVE PATH LOGIC
      *  BLV 991101 CHANGED TO SKIP Z2 TRANSACTIONS WITH OTHBR OF "C":
      *             LOGIC HERE TO SKIP OR INCLUDE BASED ON DATES IN LOAN
      *             RECORD PLUS TOTALS BY THESE, WOULD HAVE TO READ ACCT
      *             IN OTHER BRANCH...IF THEY NEED TO SEE THE CASH/OTHER
      *             BRANCH SIDE OF Z2, THEY CAN SEE THEM ON EMTRRG;
      *             ALSO CHANGED READ ON LN TO READ ON KEY INSTEAD OF
      *             START & READ-NEXT....SOMEONE WAS CONFUSED ABOUT IO!
      *  KEC 000731 {PR#01273} PARADATA FINANCIAL [JUDY]
      *              REPLACED THE PART IN THE DETAIL-LINE IN DISPLAYING
      *              THE ACCTNO WITH SOME STATUS FLAGS WITH STANDARD
      *              LAYOUT FROM LIBLP/LPSTAT (LPS-DISPLAY-LNNO).
      *
      *  KEC 001002 {PR#00000} MERCURY FINANCE [CINDY]
      *              CHANGED WK2-LINE FROM 9(3) TO 9(2) COMP JUST LIKE
      *              TP-LINE; BETH ORIGINALLY MADE THE CHANGE BUT HAD
      *              GOT CAUGHT IN LP/EMTRRG SO CHANGED TO 9(2) COMP.
      *  BAH 070703 DO NOT PRINT SSNO FOR WORLD, WORLD PR# 536
      *
      * KEC 2016.1025 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED SAVE-TR1-KEY AND TEST-TR1-KEY TO HAVE A
      *         PIC X(27) FOR QUICKER VERIFICATION.
      * BAH 2016.1103 CHANGE TILLNO TO 9(2)
      * BAH 2016.1129 MOVED D-TILL TO BEGINNING OF LINE TO FIT 9(2)
      * BAH 170531 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 190404 SPLIT OUT TRFILE, REMOVED ARCH
      * BAH 2019.0524 CLASS TRUNCATED IN CLASS TOTALS, CHANGED DS-CLASS
      *               FROM PIC XX TO PIC 99. QA#0027
      * BAH 20190919 REMOVED ACCESS-CALL ON TRPFILE
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
           SELECT WK2-FILE ASSIGN WK2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      *****************************************************************
      *    FOR TOTALS
      *****************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-CODE  PIC X.
               05  WK-BRNO  PIC 9(4).
               05  WK-CLASS PIC 9(3).
               05  WK-TRCD  PIC XX.
           03  WK-NO        PIC S9(7)  COMP-3.
           03  WK-TRAMT     PIC S9(9)V99  COMP-3.
           03  WK-INTDUE    PIC S9(9)V99  COMP-3.
           03  WK-APOTH     PIC S9(9)V99  COMP-3.
           03  WK-APINT     PIC S9(9)V99  COMP-3.
           03  WK-APLC      PIC S9(9)V99  COMP-3.
           03  WK-APCUR    PIC S9(9)V99  COMP-3.
           03  WK-CHECKS    PIC S9(9)V99  COMP-3.
           03  WK-EARNINGS  PIC S9(9)V99  COMP-3.
           03  WK-DLPROC    PIC S9(9)V99  COMP-3.
       01  WK-GL-REC.
           03  WK-GL1-KEY.
               05  FILLER   PIC X.
               05  WK-GLNO  PIC 9(11)     COMP-3.
               05  FILLER   PIC XX.
           03  WK-GLAMT     PIC S9(7)V99  COMP-3.
           03  FILLER       PIC X(53).

      *****************************************************************
      *  INDEXED TRFILE BY BRANCH, CLASS, ACCOUNT
      *****************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRANCH            PIC 9(4).
               05  WK2-CLASS             PIC 9(3).
               05  WK2-ACCTNO            PIC 9(8).
               05  WK2-DATE              PIC 9(8).
      *        05  WK2-CODE              PIC X.
               05  WK2-LINE              PIC 9(3).

      *****************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      *****************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/EMZ2RG.CBL S34 08/11/21-11:25:20 >".

      *****************************************************************
      *
      *     F I L E   I F N
      *
      *****************************************************************
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
      *-----------------------------------------------------------------
       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  WK2-IFN              PIC X(3)     VALUE "WK2".
       01  WK-IFN               PIC X(2)     VALUE "WK".
       01  WK2-PATH             PIC X(35).

      *****************************************************************
      *
      *   LINE WORKERS
      *
      *****************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      *****************************************************************
      *
      *   MISC WORKERS
      *
      *****************************************************************
       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
       01  SUMMARY-FG           PIC X     VALUE "N".
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  SAVE-TP-REC-FG       PIC X VALUE SPACES.


       01  TEST-TRP1-KEY        PIC X(26)    VALUE SPACES.
       01  FILLER               REDEFINES TEST-TRP1-KEY.
           03  TEST-TRP1-KEY-MAIN.
               05  TEST-TRP1-BRNO             PIC 9(4).
               05  TEST-TRP1-DATE             PIC 9(8).
               05  TEST-TRP1-CLASS            PIC 9(3).
               05  TEST-TRP1-ACCTNO           PIC 9(8).
           03  TEST-TRP1-LINE                 PIC 9(3).

       01  SAVE-TRP1-KEY        PIC X(26)    VALUE SPACES.
       01  FILLER               REDEFINES SAVE-TRP1-KEY.
           03  SAVE-TRP1-KEY-MAIN.
               05  SAVE-TRP1-BRNO             PIC 9(4).
               05  SAVE-TRP1-DATE             PIC 9(8).
               05  SAVE-TRP1-CLASS            PIC 9(3).
               05  SAVE-TRP1-ACCTNO           PIC 9(8).
           03  SAVE-TRP1-LINE                 PIC 9(3).

      *-----------------------------------------------------------------
      *    H O L D - F I E L D S
      *-----------------------------------------------------------------
       01  HOLD-EARNINGS          PIC S9(7)V99  COMP-3 VALUE 0.
       01  HOLD-TP-OTHBR-FG       PIC X                VALUE "?".
       01  HOLD-TP-ACCTNO         PIC 9(8)      COMP-3 VALUE 99999999.
       01  HOLD-TP-SSNO           PIC 9(10)     COMP-3 VALUE 9999999999.

       01  HOLD-BRNO              PIC 9(4)             VALUE 0.
       01  HOLD-CLASS             PIC 9(3)             VALUE 0.

       01  HOLD-TRANS-DATE        PIC 9(8).

      *****************************************************************
      *
      *    S U B S C R I P T S
      *
      *-----------------------------------------------------------------
      *      SUB-PL; SUB-REDEEM; SUB-REPO; SUB-SOLD; SUB-TRANS ---> IS
      *      USED IN RANKING DATES.
      *****************************************************************
       01  SUB                               PIC 9(2)      COMP-3.
       01  SUB1                              PIC 9(2)      COMP-3.
       01  SUB2                              PIC 9(2)      COMP-3.
       01  SUB3                              PIC 9(2)      COMP-3.
       01  SUB4                              PIC 9(2)      COMP-3.
       01  SUB5                              PIC 9(2)      COMP-3.
       01  SUB-PL                            PIC 9(2)      COMP-3.
       01  SUB-REDEEM                        PIC 9(2)      COMP-3.
       01  SUB-REPO                          PIC 9(2)      COMP-3.
       01  SUB-SOLD                          PIC 9(2)      COMP-3.
       01  SUB-TRANS                         PIC 9(2)      COMP-3.
       01  LEV                               PIC 99        COMP-3.
       01  LEV1                              PIC 99        COMP-3.
       01  LEV2                              PIC 99        COMP-3.
       01  LEV3                              PIC 99        COMP-3.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *-----------------------------------------------------------------
      *    C L A S S - F G
      *-----------------------------------------------------------------
       01  CLASS-FG                          PIC X(3)   VALUE "   ".
           88  CLASS-BEG                                VALUE "BEG".
           88  CLASS-END                                VALUE "END".

      *-----------------------------------------------------------------
      *    A C C O U N T - T Y P E
      *-----------------------------------------------------------------
       01  ACCOUNT-TYPE                      PIC 9(2)   VALUE ZEROES.
           88  ACCOUNT-TYPE-OPEN             VALUE 1.
           88  ACCOUNT-TYPE-REPO             VALUE 2.
           88  ACCOUNT-TYPE-PL               VALUE 3.
           88  ACCOUNT-TYPE-VALID            VALUE 1 2 3.

      *-----------------------------------------------------------------
      *    L E V E L - T Y P E
      *-----------------------------------------------------------------
       01  LEVEL-TYPE                        PIC 9(2)   VALUE ZEROES.
           88  LEVEL-TYPE-OPEN               VALUE 1.
           88  LEVEL-TYPE-REPO               VALUE 2.
           88  LEVEL-TYPE-PL                 VALUE 3.
           88  LEVEL-TYPE-P2                 VALUE 4.
           88  LEVEL-TYPE-P3                 VALUE 5.
           88  LEVEL-TYPE-P4                 VALUE 6.
           88  LEVEL-TYPE-P5                 VALUE 7.

      *****************************************************************
      *
      *    D A T E - R A N K  -----  THIS IS TO RANK DATES.
      *
      *-----------------------------------------------------------------
      *    DATE-RANK(X) = 1 -----> LN-REPODATE.
      *    DATE-RANK(X) = 2 -----> LN-REDEEMDATE.
      *    DATE-RANK(X) = 3 -----> LN-SOLDDATE.
      *    DATE-RANK(X) = 4 -----> LN-PLDATE.
      *    DATE-RANK(X) = 5 -----> HOLD-TRANS-DATE.
      *
      *    THE SETUP OF REPO FIRST, THEN REDEEM, THEN SOLD, THEN PL,
      *    THEN LAST TRANS-DATE HAS TO BE THIS WAY TO MAKE PL THE HIGHER
      *    PRIORITY WITH REPODATE THE LOWER PRIORITY IF ALL DATES ARE
      *    THE SAME.
      *
      *****************************************************************
       01  DATE-RANKINGS                     PIC X(5)   VALUE "12345".
       01  DATE-RANKINGS-RDF       REDEFINES DATE-RANKINGS.
           03  DATE-RANK                     PIC X(1)   OCCURS 5 TIMES.

       01  HOLD-DATE-RANK                    PIC X(1)   VALUE SPACES.
       01  HOLD-DATE                         PIC 9(8)   OCCURS 5 TIMES.
       01  HOLD-DATE-FIELD                   PIC 9(8).

      *-----------------------------------------------------------------
       01  MESSAGE-01-LOC-PRT   PIC X(43)    VALUE
           "CANNOT DO LOCAL SPOOLING WITH GROUP TOTALS".

      ******************************************************************
      *
      *    W S - G T T - T A B L E
      *
      ******************************************************************
       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL                 OCCURS 10 TIMES.
               05  WS-GTT-NO              PIC S9(7)     COMP.
               05  WS-GTT-TRAMT           PIC S9(9)V99  COMP.
               05  WS-GTT-INTDUE          PIC S9(9)V99  COMP.
               05  WS-GTT-APOTH           PIC S9(9)V99  COMP.
               05  WS-GTT-APINT           PIC S9(9)V99  COMP.
               05  WS-GTT-APLC            PIC S9(9)V99  COMP.
               05  WS-GTT-APCUR          PIC S9(9)V99  COMP.
               05  WS-GTT-CHECKS          PIC S9(9)V99  COMP.
               05  WS-GTT-EARNINGS        PIC S9(9)V99  COMP.
               05  WS-GTT-DLPROC          PIC S9(9)V99  COMP.

      *-----------------------------------------------------------------
       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.

      *****************************************************************
      *
      *  TYPE   LEV1 = POSTING FROM THIS BRANCH
      *            2 = POSTING FROM OTHER BRANCHES
      *
      *  TOTALS LEV1 = CLASS TOTALS
      *            2 = BRANCH TOTALS
      *            3 = GRAND TOTALS
      *
      *****************************************************************
       01  TOTALS-TAB   VALUE ZEROS.
           03  TYPE-TBL        OCCURS 2.
               05  TOTAL-TBL       OCCURS 3.
                   07  T-TRAMT      PIC S9(9)V99.
                   07  T-INTDUE     PIC S9(9)V99.
                   07  T-APOTH      PIC S9(9)V99.
                   07  T-APINT      PIC S9(9)V99.
                   07  T-APLC       PIC S9(9)V99.
                   07  T-APCUR     PIC S9(9)V99.
                   07  T-CHECKS     PIC S9(9)V99.
                   07  T-EARNINGS   PIC S9(9)V99.
                   07  T-DLPROC     PIC S9(7)V99.

      *****************************************************************
      *  TYPE   LEV1 = CLASS
      *            2 = BRANCH
      *            3 = GRAND
      *
      *  TOTALS TYPE 1 = "OP" OPEN ACCOUNTS  5 = "P3"
      *              2 = "RE" REPO ACCOUNTS  6 = "P4"
      *              3 = "PL"                7 = "P5"
      *              4 = "P2"
      *****************************************************************
       01  TTOTALS-TAB   VALUE ZEROS.
           03  TTOTAL-TBL       OCCURS 3.
               05  TTOTAL-TBL     OCCURS 7.
                   07  TT-TRAMT      PIC S9(9)V99.
                   07  TT-INTDUE     PIC S9(9)V99.
                   07  TT-APOTH      PIC S9(9)V99.
                   07  TT-APINT      PIC S9(9)V99.
                   07  TT-APLC       PIC S9(9)V99.
                   07  TT-APCUR     PIC S9(9)V99.
                   07  TT-CHECKS     PIC S9(9)V99.
                   07  TT-EARNINGS   PIC S9(9)V99.
                   07  TT-DLPROC     PIC S9(7)V99.
      *-----------------------------------------------------------------
       01  LEGEND               PIC X(70).

      *****************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      *****************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH                     PIC 9(4).
           03  END-BRANCH                     PIC 9(4).
           03  BEG-CLASS                      PIC 9(3).
           03  END-CLASS                      PIC 9(3).
           03  END-DATE.
               05  END-MONTH                  PIC 99.
               05  END-YEAR                   PIC 99.
           03  EDATE       REDEFINES END-DATE PIC 9999.
           03  DAY-OF-MONTH                   PIC 99.
           03  GTT-ISO                        PIC X.
           03  LNTYPE                         PIC X.
           03  REPOTYPE                       PIC X.
           03  PLACCT                         PIC X.
           03  PLTYPE                         PIC X.
           03  GROUP-FLAG                     PIC X.
           03  ENT-GROUP                      PIC X(4).
           03  PRT-PRINTING-OPTIONS.
               05  PRT-DETAIL-ANALYSIS        PIC X(1).
               05  PRT-TOTALS-OPTIONS.
                   07  PRT-CLASS-TOTALS       PIC X(1).
                   07  PRT-BRANCH-TOTALS      PIC X(1).
                   07  PRT-GRAND-TOTALS       PIC X(1).
           03  LOCAL-PRINT-FG                 PIC X(1).
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE          PIC X(1)  OCCURS 10.
       01  VERSION-CONTROL                    PIC X VALUE "D".

       01  BEGINNING-DATE       PIC 9(8).
       01  ENDING-DATE          PIC 9(8).
       01  SV-BEG-BR                          PIC 9999  VALUE 9999.
       01  SV-END-BR                          PIC 9999  VALUE 9999.

      *****************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      *****************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(34)    VALUE " ".
           03  FILLER           PIC X(46) VALUE
               "MONTHLY 'Z2' TRANSACTION REGISTER".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).
           03  FILLER           PIC X(46) VALUE " ".

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER           PIC X(10) VALUE "CLASS  :".
           03  H-CLASS           PIC Z99.
           03  FILLER           PIC X VALUE " ".
           03  H-LCDESC         PIC X(20).

      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(17)    VALUE "ACCTNO".
           03  FILLER           PIC X(28)    VALUE
              "DEALER          IB".
           03  FILLER           PIC X(52)    VALUE
           "TR          TRANS       - APPLIED -".
           03  FILLER           PIC X(34)    VALUE
           "---- BALANCES -----          G/L".

      *-----------------------------------------------------------------
       01  HEAD6.
           03  FILLER           PIC X(10)      VALUE
           "    TL".
           03  FILLER           PIC X(58)     VALUE
           "       PROC'D   EARNED PC PAY-DATE CD REFNO   AMOUNT".
           03  FILLER           PIC X(45)     VALUE
           "INT     PRIN  PDTH LCPD     OTH      INT".
           03  FILLER           PIC X(18)     VALUE
           "PRIN      ACCT-AMT".

      *-----------------------------------------------------------------
       01  SUM-HEAD.
           03  FILLER           PIC X(55)    VALUE
           "CLASS               TRCD   NO     TR-AMT  NSF CHECKS   ".
           03  FILLER           PIC X(55)    VALUE
           "APP-OTH    APP-INT     APP-LC    APP-PRIN  REB CHECKS  ".
           03  FILLER           PIC X(20)    VALUE
           " EARNINGS    DL-PROC".

      *-----------------------------------------------------------------
       01  GL-HEAD.
           03  FILLER           PIC X(17)    VALUE "G/L DIST  SUMMARY".

      *-----------------------------------------------------------------
       01  GTT-HEAD1.
           03  FILLER            PIC X(57)  VALUE SPACES.
           03  FILLER            PIC X(18)  VALUE "GROUP TOTALS TABLE".
           03  FILLER            PIC X(57)  VALUE SPACES.

      *-----------------------------------------------------------------
      *01  GTT-HEAD2.
      *    03  FILLER            PIC X(18)  VALUE
      *        "BRANCH/GROUP   GR ".

      *    03  FILLER           PIC X(38)    VALUE
      *    "            NO   TR-AMT  NSF CHECKS   ".
      *    03  FILLER           PIC X(55)    VALUE
      *    "APP-OTH    APP-INT     APP-LC    APP-PRIN  REB CHECKS  ".
      *    03  FILLER           PIC X(20)    VALUE
      *    " EARNINGS    DL-PROC".

      *-----------------------------------------------------------------
       01  GTT-HEAD2.
           03  FILLER            PIC X(24)  VALUE
               "BRANCH/GROUP   GR       ".
           03  FILLER            PIC X(9)   VALUE "  NO     ".
           03  FILLER            PIC X(11)  VALUE "TRANS      ".
           03  FILLER            PIC X(12)  VALUE "NSF         ".
           03  FILLER            PIC X(12)  VALUE "APPLIED     ".
           03  FILLER            PIC X(12)  VALUE "APPLIED     ".
           03  FILLER            PIC X(10)  VALUE "APPLIED   ".
           03  FILLER            PIC X(13)  VALUE "APPLIED   ".
           03  FILLER            PIC X(7)   VALUE "REBATE ".
           03  FILLER            PIC X(13)  VALUE "  EARNINGS   ".
           03  FILLER            PIC X(6)   VALUE "DEALER".

      *-----------------------------------------------------------------
       01  GTT-HEAD3.
           03  FILLER            PIC X(14)  VALUE SPACES.
           03  FILLER            PIC X(10)  VALUE "LEV       ".
           03  FILLER            PIC X(9)   VALUE "         ".
           03  FILLER            PIC X(11)  VALUE "AMOUNT     ".
           03  FILLER            PIC X(12)  VALUE "CHECKS      ".
           03  FILLER            PIC X(12)  VALUE "OTHER       ".
           03  FILLER            PIC X(12)  VALUE "INT         ".
           03  FILLER            PIC X(10)  VALUE "LT-CHG    ".
           03  FILLER            PIC X(13)  VALUE "PRIN         ".
           03  FILLER            PIC X(15)  VALUE "CHECKS         ".
           03  FILLER            PIC X(15)  VALUE "     PROCEEDS  ".

      *****************************************************************
      *
      *    D E T A I L - L I N E
      *
      *****************************************************************
       01  DETAIL-LINE          VALUE SPACES.
           03  FILLER           PIC X(116).
           03  D-BRXX           PIC X(12).
           03  D-BRX            PIC 9(4).

      *-----------------------------------------------------------------
       01  D-LINE1 REDEFINES DETAIL-LINE.

           03  D-DISPLAY-LNNO   PIC X(16).
           03  FILLER           PIC X(03).
           03  D-NAME           PIC X(26).
           03  FILLER           PIC X.
           03  D-MAKER          PIC X.
           03  FILLER           PIC XX.
           03  D-SSNO           PIC 999/99/9999 BLANK ZERO.
           03  FILLER           PIC X(5).
           03  D-DLNO           PIC ZZZZ.ZZ.

      *-----------------------------------------------------------------
       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(4).
           03  D-TILL           PIC Z9.
           03  FILLER           PIC X(9).
           03  D-DLPROC         PIC ZZZZZ.ZZ-.
           03  D-EARNINGS       PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D-IBPC           PIC X.
           03  FILLER           PIC X.
           03  D-DATE           PIC 99/99/99.
           03  FILLER           PIC X.
           03  D-TRCD           PIC XX.
           03  D-REV            PIC X.
           03  D-REFNO          PIC X(5).
           03  FILLER           PIC X.
           03  D-TRAMT          PIC ZZZZZ.ZZ-.
           03  D-APINT          PIC ZZZZZ.ZZ-.
           03  D-APCUR          PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D-PDTHRU-MO      PIC 99.
           03  D-PDTHRU-DAYR    PIC 99.
           03  FILLER           PIC X.
           03  D-LCPD-MO        PIC 99.
           03  D-LCPD-DAYR      PIC 99.
           03  D-OTHBAL         PIC ZZZZZ.ZZ-.
           03  D-INTBAL         PIC ZZZZZ.ZZ-.
           03  D-PRINBAL        PIC ZZZZ9.99-.
           03  FILLER           PIC X(2).
           03  D-GLNO           PIC ZZZ9/9999999.
           03  D-GLAMT          REDEFINES D-GLNO
                                PIC ZZZZZZZZ.ZZ-.
           03  D-PAIDOFF        REDEFINES D-GLNO
                                PIC X(12).

      *-----------------------------------------------------------------
       01  D-LINE3 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(10).
           03  D-DLTAG1         PIC X(10).
           03  D-DLADJ1         PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X(3).
           03  D-DLTAG2         PIC X(10).
           03  D-DLADJ2         PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X(3).
           03  D-DLTAG3         PIC X(10).
           03  D-DLADJ3         PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X(3).
           03  D-DLTAG4         PIC X(11).
           03  D-PVHELD         PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X(3).
           03  D-DLTAG5         PIC X(11).
           03  D-PVPAID         PIC ZZZZZ.ZZ-.

      *-----------------------------------------------------------------
       01  D-LINE4 REDEFINES DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D2-GROUP             PIC X(19).
           03  D2-GROUP-1           REDEFINES D2-GROUP.
               05  DS-CLASS         PIC 99.
               05  FILLER           PIC X.
               05  DS-DESC          PIC X(16).
           03  D2-GROUP-2           REDEFINES D2-GROUP.
               05  D2-PROMPT1       PIC X(7).
               05  FILLER           PIC X(2).
               05  D2-GRNO          PIC X(4).
               05  D2-BRNO          REDEFINES D2-GRNO
                                    PIC 9(4).
               05  FILLER           PIC X(3).
               05  D2-GR-LEVEL      PIC 9(1).
               05  FILLER           PIC X(2).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  FILLER           PIC XX.
           03  DS-TRCD          PIC XX.
           03  FILLER           PIC XXX.
           03  DS-NO            PIC ZZZ.
           03  FILLER           PIC X.
           03  DS-TRAMT         PIC ZZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-INTDUE        PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-APOTH         PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-APINT         PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-APLC          PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-APCUR        PIC ZZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-CHECKS        PIC ZZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-EARNINGS      PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-DLPROC        PIC ZZZZZZ.ZZ-.

      *-----------------------------------------------------------------
       01  D-LINE5 REDEFINES DETAIL-LINE.
           03  DS-GLNO          PIC ZZZ9/9999999.
           03  FILLER           PIC XX.
           03  DS-GLAMT         PIC ZZZZ9.99-.

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(5)9.
               04  R-NUM-REDEF REDEFINES R-NUM.
                   05  R-CHAR   PIC X(6).
      *****************************************************************
      *
      *     C O P Y   L I B R A R Y   S E C T I O N
      *
      *****************************************************************
       COPY "LIBLP/EMZ2RG_CMD.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/EMZ2RG_DEF.CPY".
       COPY "LIBLP/EMZ2RG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/EMZ2RG_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EMZ2RG" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "N" TO RECORDS-FOUND.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           IF LOCAL-PRINT-FG = "Y"
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 GO TO NEXT-GROUP-LOC.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE.

           PERFORM OPEN-TRP1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.

           PERFORM SORT-ROUTINE.

           IF EXT-EOBUF-ERRCD = "X"
              GO TO NEXT-GROUP-LOC.

           PERFORM OPEN-WK2-FILE.

       NEXT-TR-REPORT.
           PERFORM READ-WK2-FILE-NEXT.
           IF ( IO-FG = 9 )
              GO TO END-OF-GROUP.

           MOVE WK2-BRANCH  TO TP-BRNO.
           MOVE WK2-DATE    TO TP-DATE.
           MOVE WK2-CLASS   TO TP-CLASS.
           MOVE WK2-ACCTNO  TO TP-ACCTNO.
           MOVE WK2-LINE    TO TP-LINE.
           PERFORM READ-TRP1-FILE.
           IF ( IO-FG = 9 )
              MOVE "TRANSACTION RECORD MISSING SINCE SORT" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           PERFORM CHECK-PARAMETERS.

           IF NOT ( LN-ACCTNO = WK2-ACCTNO )
              GO TO NEXT-TR-REPORT.

           IF NOT ( ACCOUNT-TYPE-VALID )
              GO TO NEXT-TR-REPORT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( FIRST-TIME = "Y" )
              MOVE TP-BRNO  TO HOLD-BRNO
              MOVE TP-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF ( TP-BRNO NOT = HOLD-BRNO )
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE TP-BRNO TO HOLD-BRNO
              MOVE TP-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS
              IF ( GTT-ISO NOT = "O" )
                 IF ( PRT-DETAIL-ANALYSIS = "Y" )
                    PERFORM HEAD-PAGE.

           IF ( TP-CLASS NOT = HOLD-CLASS )
              PERFORM CLASS-TOTALS
              MOVE TP-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              IF ( GTT-ISO NOT = "O" )
                 IF ( PRT-DETAIL-ANALYSIS = "Y" )
                    PERFORM HEAD-PAGE.

           IF ( PRT-DETAIL-ANALYSIS = "Y" )
              PERFORM DETAIL-LINE-PROCESS
           ELSE
           IF ( FIRST-TIME = "Y" )
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

           MOVE 1 TO LEV.
           PERFORM SETUP-LEVEL-TYPE.
           PERFORM CREATE-WK.

           GO TO NEXT-TR-REPORT.

      *-----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE-OUTPUT.

           IF ( LOCAL-PRINT-FG = "Y" )
              IF ( FIRST-TIME NOT = "Y" )
                 PERFORM CLASS-TOTALS
                 PERFORM BRANCH-TOTALS
                 MOVE "Y" TO FIRST-TIME
                 MOVE 0 TO PAGE-NUMBER
              ELSE
                 MOVE 0 TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

      *--->PERFORM CLASS-TOTALS.
      *--->PERFORM BRANCH-TOTALS.

           IF ( LOCAL-PRINT-FG = "N" )
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              PERFORM GRAND-TOTALS
      *------>PERFORM GL-SUMMARY
              IF ( GTT-ISO NOT = "S" )
                 MOVE 99 TO LN-COUNT
                 PERFORM GROUP-TOTAL-TABLE
                 PERFORM PRINT-RANGE-LINES
                 PERFORM GL-SUMMARY
              ELSE
                 PERFORM GL-SUMMARY
                 PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE EMZ2RG-QUEUE-POS TO Q-POS.
           MOVE EMZ2RG-COPIES-POS TO C-POS.
           MOVE EMZ2RG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM CLEAR-T-TOTALS
                         VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2
                           AFTER SUB2 FROM 1 BY 1 UNTIL SUB2 > 3.

           PERFORM CLEAR-TT-TOTALS
                         VARYING LEV1 FROM 1 BY 1 UNTIL LEV1 > 3
                           AFTER LEV2 FROM 1 BY 1 UNTIL LEV2 > 7.

           MOVE 0 TO PAGE-NUMBER.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CREATE INDEXED KEY FILE FOR TRFILE OUTPUT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WK2 SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WK SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "TRANSACTION REGISTER IN PROGRESS........" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      * SET DATE RANGE FOR TESTING TR FILES
           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           IF DAY-OF-MONTH = 99
              MOVE 1      TO DATE-MMDDYY-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           IF DAY-OF-MONTH = 99
              MOVE 31      TO DATE-MMDDYY-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO ENDING-DATE.

           IF ( LOCAL-PRINT-FG NOT = "Y" )
              PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.

           IF ( GROUP-FLAG = "Y" )
              MOVE LOW-VALUES TO WS-GTT-TOTAL-TABLE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              GO TO EXIT-INITIALIZE
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y" TO TOTAL-LEVEL-TABLE(9).

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO SYS-DATE.
           MOVE S-MM TO END-MONTH.
           MOVE S-YY TO END-YEAR.

           IF EXT-EOBUF-DATE1 NOT = EXT-EOBUF-DATE2
              MOVE 99 TO DAY-OF-MONTH
           ELSE
              MOVE S-DD TO DAY-OF-MONTH.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
       ENTER-PARM.
           IF ( VDUSTATUS = "1U" )
              GO TO PRT-GRAND-TOTALS-YN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * DEFAULT TO USER BRANCH
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "SNNN040BR1." TO VDU.
      *    MOVE 0 TO BEG-BRANCH VDUNUM0.
      *    PERFORM SCREENIO.
      *    MOVE "SNNN040BR2." TO VDU.
      *    MOVE 0 TO END-BRANCH VDUNUM0.
      *--->PERFORM SCREENIO.

      *-----------------------------------------------------------------
       DTE.
           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO DA-01.

           MOVE LEGEND-DTE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           MOVE VDUNUM0 TO EDATE.

           IF END-MONTH < 1 OR > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DTE.

      *-----------------------------------------------------------------
       DA-01.
           IF ( VDUSTATUS = "1U" )
              IF ( DAY-OF-MONTH = 99 )
                 MOVE "SNNN020DA1." TO VDU
                 MOVE 99 TO VDUNUM0
                 PERFORM SCREENIO.

           MOVE "ENNN020DA1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "16"
              MOVE "S  A020DA1." TO VDU
              MOVE "**" TO VDUBUF
              PERFORM SCREENIO
              MOVE 99 TO DAY-OF-MONTH
              GO TO BR-01.

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO DTE
              ELSE
                 GO TO DA-01.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.

           MOVE VDUNUM0 TO DAY-OF-MONTH.

           IF DAY-OF-MONTH > 31  OR
              (DAY-OF-MONTH > 30 AND
              (END-MONTH = 2 OR 4 OR 6 OR 9 OR 11)) OR
              (DAY-OF-MONTH > 29 AND END-MONTH = 2)
              MOVE "DAY IS PAST LAST DAY OF MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DA-01.

      *-----------------------------------------------------------------
       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DA-01.
           IF VDUSTATUS = "16" GO TO ALL-BR.
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

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
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

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "N" TO GROUP-FLAG.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *-----------------------------------------------------------------
       CL-01.
           MOVE LEGEND-CL-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       CL-02.
           MOVE LEGEND-CL-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.

           GO TO OPEN-ACCOUNT.

      *-----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       GTT-ISO-PARAGRAPH.

           IF ( GROUP-FLAG NOT = "Y" )
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "S  A010GTT-ISO." TO VDU
              MOVE "S" TO VDUBUF GTT-ISO
              PERFORM SCREENIO
              GO TO OPEN-ACCOUNT.

           MOVE LEGEND-GTT-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010GTT-ISO." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO CL-02.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO GTT-ISO-PARAGRAPH.

           IF NOT ( VDUBUF = "I" OR "S" OR "O" )
              GO TO GTT-ISO-PARAGRAPH.

           MOVE VDUBUF TO GTT-ISO.

           IF ( GTT-ISO = "I" OR "O" )
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE.

      *----------------------------------------------------------------
       OPEN-ACCOUNT.
           MOVE LEGEND-OPEN-ACCT TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020OPEN-TYPE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OPEN-ACCOUNT.
           MOVE VDUBUF TO LNTYPE.
           IF LNTYPE = "I" GO TO REPO-ACCT.
           IF LNTYPE = "S" GO TO REPO-ACCT.
           IF LNTYPE NOT = "O" GO TO OPEN-ACCOUNT.

      *----------------------------------------------------------------
       REPO-ACCT.
           MOVE LEGEND-REPO-ACCT TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010REPO-TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO OPEN-ACCOUNT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO REPO-ACCT.

           MOVE VDUBUF TO REPOTYPE.
           IF REPOTYPE = "I" GO TO PL-ACCOUNT.
           IF REPOTYPE = "S" GO TO PL-ACCOUNT.
           IF REPOTYPE NOT = "O" GO TO REPO-ACCT.

      *----------------------------------------------------------------
       PL-ACCOUNT.
           MOVE LEGEND-PL-ACCT TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010PL-ACCT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO REPO-ACCT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PL-ACCOUNT.
           MOVE VDUBUF TO PLACCT.
           IF NOT ( PLACCT = "I" OR "S" OR "O" )
              GO TO PL-ACCOUNT.

      *-----------------------------------------------------------------
       PL-ACCT-TYPE.
           IF ( VDUSTATUS = "1U" ) AND ( PLACCT = "S" )
              GO TO PL-ACCOUNT.

           IF ( PLACCT = "S" )
              MOVE "S  A010PL-TYPE." TO VDU
              MOVE SPACES            TO PLTYPE VDUBUF
              PERFORM SCREENIO
              GO TO CHECK-ISO.

           MOVE LEGEND-PL-TYPE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010PL-TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PL-ACCOUNT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PL-ACCT-TYPE.
           MOVE VDUBUF TO PLTYPE.

           IF NOT (PLTYPE = "A" OR "P" OR "2" OR "3" OR "4" OR "5")
              GO TO PL-ACCT-TYPE.

      *-----------------------------------------------------------------
       CHECK-ISO.
           IF ( LNTYPE   = "S" ) AND
              ( REPOTYPE = "S" ) AND
              ( PLACCT   = "S" )
              MOVE "THIS SETUP DOES NOT PRODUCE ANY REPORT!!!" TO MESS
              PERFORM SEND-MESS
              GO TO OPEN-ACCOUNT.

      *-----------------------------------------------------------------
       PRT-DETAIL-ANALYSIS-YN.

           IF ( GTT-ISO = "O" )
              MOVE "S  A010DETAIL-ANALY." TO VDU
              MOVE "N" TO PRT-DETAIL-ANALYSIS VDUBUF
              PERFORM SCREENIO
              GO TO PRT-CLASS-TOTALS-YN.

           MOVE LEGEND-PRINT-DETAIL TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010DETAIL-ANALY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PL-ACCT-TYPE.

           IF ( VDUSTATUS NOT = "00" AND NOT = "1D" )
              GO TO PRT-DETAIL-ANALYSIS-YN.

           IF ( VDUBUF NOT = "Y" ) AND ( VDUBUF NOT = "N" )
              GO TO PRT-DETAIL-ANALYSIS-YN.

           MOVE VDUBUF TO PRT-DETAIL-ANALYSIS.

      *-----------------------------------------------------------------
       PRT-CLASS-TOTALS-YN.
           MOVE LEGEND-PRINT-CLASS TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010CLASS-TOTALS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PRT-DETAIL-ANALYSIS-YN.

           IF ( VDUSTATUS NOT = "00" AND NOT = "1D" )
              GO TO PRT-CLASS-TOTALS-YN.

           IF ( VDUBUF NOT = "Y" ) AND ( VDUBUF NOT = "N" )
              GO TO PRT-CLASS-TOTALS-YN.

           MOVE VDUBUF TO PRT-CLASS-TOTALS.

      *-----------------------------------------------------------------
       PRT-BRANCH-TOTALS-YN.
           MOVE LEGEND-PRINT-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010BRANCH-TOTAL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PRT-CLASS-TOTALS-YN.

           IF ( VDUSTATUS NOT = "00" AND NOT = "1D" )
              GO TO PRT-BRANCH-TOTALS-YN.

           IF ( VDUBUF NOT = "Y" ) AND ( VDUBUF NOT = "N" )
              GO TO PRT-BRANCH-TOTALS-YN.

           MOVE VDUBUF TO PRT-BRANCH-TOTALS.

      *-----------------------------------------------------------------
       PRT-GRAND-TOTALS-YN.
           MOVE LEGEND-PRINT-GRAND TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010GRAND-TOTALS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PRT-BRANCH-TOTALS-YN.

           IF ( VDUSTATUS NOT = "00" AND NOT = "1D" )
              GO TO PRT-GRAND-TOTALS-YN.

           IF ( VDUBUF NOT = "Y" ) AND ( VDUBUF NOT = "N" )
              GO TO PRT-GRAND-TOTALS-YN.

           MOVE VDUBUF TO PRT-GRAND-TOTALS.

      *-----------------------------------------------------------------
       PRT-PRINTING-OPTIONS-YN.
           IF ( PRT-PRINTING-OPTIONS = "NNNN" )
              MOVE "THIS SETUP DOES NOT PRODUCE ANY REPORT!!!" TO MESS
              PERFORM SEND-MESS
              GO TO PRT-DETAIL-ANALYSIS-YN.

      *-----------------------------------------------------------------
       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO END-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO END-PARM.

      *-----------------------------------------------------------------
       ASK-LOCAL.
           MOVE "S  A290SPOOL-PROMPT."          TO VDU.
           MOVE "SPOOL TO LOCAL (Y/N)        :" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF ( VDUSTATUS = "1U" )
              GO TO PRT-GRAND-TOTALS-YN.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

           IF ( GTT-ISO NOT = "S" )
              IF ( LOCAL-PRINT-FG = "Y" )
                 MOVE MESSAGE-01-LOC-PRT TO MESS
                 PERFORM SEND-MESS
                 GO TO ASK-LOCAL.

      *-----------------------------------------------------------------
       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF GTT-SV-ENT-GROUP
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010OPEN-TYPE." TO VDU.
           MOVE LNTYPE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010REPO-TYPE." TO VDU.
           MOVE REPOTYPE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PL-ACCT." TO VDU.
           MOVE PLACCT TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PL-TYPE." TO VDU.
           MOVE PLTYPE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020DA1." TO VDU.
           MOVE DAY-OF-MONTH TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010DETAIL-ANALY." TO VDU.
           MOVE PRT-DETAIL-ANALYSIS TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010CLASS-TOTALS." TO VDU.
           MOVE PRT-CLASS-TOTALS TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010BRANCH-TOTAL." TO VDU.
           MOVE PRT-BRANCH-TOTALS TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010GRAND-TOTALS." TO VDU.
           MOVE PRT-GRAND-TOTALS TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A290SPOOL-PROMPT."          TO VDU.
           MOVE "SPOOL TO LOCAL (Y/N)        :" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       SORT-ROUTINE SECTION.
      ******************************************************************

      * ALWAYS START WITH BR-NO SO YOU ARENT READING ENTIRE TRFILE FROM
      * BEGINNING BRANCH TO ENDING BRANCH, SINCE IT HAS GROUP LOGIC YOU
      * ONLY WANT THE CURRENT BRANCH!

           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
           MOVE ENDING-DATE         TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WEND-DATE.

           MOVE BEG-CLASS           TO TP-CLASS
                                       QTRP1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRP1-WEND-CLASS.
           MOVE 0                   TO TP-ACCTNO TP-LINE
                                       QTRP1-WBEG-ACCTNO
                                       QTRP1-WBEG-LINE.
           MOVE ALL "9"             TO QTRP1-WEND-ACCTNO
                                       QTRP1-WEND-LINE.

           MOVE TP-DATE              TO NDTE-DATE
           MOVE 1                    TO NDTE-HOLD
           PERFORM INCREMENT-DAYS
           MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE.

           IF ( TP-CLASS = 999 )
              MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
           ELSE
              COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1.

           IF ( TP-ACCTNO = 99999999 )
              MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
           ELSE
              COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1.

           IF ( TP-LINE = 999 )
              MOVE TP-LINE           TO QTRP1-WSBEG-LINE
           ELSE
              COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.

           PERFORM START-TRP1-FILE.
      *-----------------------------------------------------------------
       SORT-TR-NEXT.
           PERFORM READ-TRP1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO EXIT-SORT-ROUTINE.

           IF TP-DATE > ENDING-DATE
              GO TO EXIT-SORT-ROUTINE.

      * CODES "F" AND "G" ARE ALSO IN THIS FILE, SO LEAVE THIS HERE
      *    IF TP-CODE NOT = "P"
      *       GO TO SORT-TR-NEXT.

           IF (TP-TRCD NOT = "Z2")
              GO TO SORT-TR-NEXT.

           IF TP-CLASS < BEG-CLASS OR TP-CLASS > END-CLASS
              GO TO SORT-TR-NEXT.

           IF TP-OTHBR-FG = "C"
              GO TO SORT-TR-NEXT.

           MOVE TRP1-KEY TO TEST-TRP1-KEY.

           IF (TEST-TRP1-KEY-MAIN = SAVE-TRP1-KEY-MAIN) AND
                                   (SAVE-TP-REC-FG = "Y")
              GO TO SAVE-THIS-ACCT.

           IF TEST-TRP1-KEY-MAIN NOT = SAVE-TRP1-KEY-MAIN
              MOVE SPACE TO SAVE-TP-REC-FG
              MOVE TRP1-KEY TO SAVE-TRP1-KEY.

      *-----------------------------------------------------------------
       SAVE-THIS-ACCT.
           MOVE TP-BRNO    TO WK2-BRANCH.
           MOVE TP-DATE    TO WK2-DATE.
      *    MOVE TP-CODE    TO WK2-CODE.
           MOVE TP-CLASS   TO WK2-CLASS.
           MOVE TP-ACCTNO  TO WK2-ACCTNO.
           MOVE TP-LINE    TO WK2-LINE.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TR-NEXT.

       EXIT-SORT-ROUTINE.
           PERFORM CLOSE-WK2-FILE.

       SORT-END.
           EXIT.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************
           IF ( FIRST-TIME = "Y" )
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

      *- - - - - - - - - - - - - - - - - - - -{ GET ACCTNO WITH STATUS }
           PERFORM GET-DISPLAY-LNNO.
           MOVE LPS-DISPLAY-LNNO TO D-DISPLAY-LNNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM FIND-BORROWER.
           MOVE TP-MAKER TO D-MAKER.
           MOVE 0        TO D-SSNO.
           MOVE TP-DLNO  TO D-DLNO.

           IF HOLD-TP-ACCTNO = TP-ACCTNO AND
               HOLD-TP-OTHBR-FG = TP-OTHBR-FG AND
                HOLD-TP-SSNO = TP-SSNO
              MOVE SPACES TO DETAIL-LINE
            ELSE
              MOVE 1 TO LN-FEED
              MOVE TP-ACCTNO   TO HOLD-TP-ACCTNO
              MOVE TP-OTHBR-FG TO HOLD-TP-OTHBR-FG
              MOVE TP-SSNO     TO HOLD-TP-SSNO
              IF TP-OTHBR-FG = "G"
                 MOVE "POSTED BY :" TO D-BRXX
                 MOVE TP-POSTING-BR TO D-BRX
                 PERFORM WRITE-DETAIL-LINE
               ELSE
                 IF TP-OTHBR-FG = "C"
                    MOVE "POSTED FOR:" TO D-BRXX
                    MOVE TP-POSTING-BR TO D-BRX
                    PERFORM WRITE-DETAIL-LINE
                  ELSE
                    PERFORM WRITE-DETAIL-LINE.

           MOVE TP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.
           MOVE TP-IBPC TO D-IBPC.
           MOVE TP-TILLNO TO D-TILL.
           COMPUTE HOLD-EARNINGS = TP-EARNINGS(1) + TP-EARNINGS(2) +
                                   TP-EARNINGS(3) + TP-EARNINGS(4) +
                                   TP-EARNINGS(5) + TP-EARNINGS(6).

           MOVE HOLD-EARNINGS TO D-EARNINGS.
           MOVE TP-DLPROC TO D-DLPROC.
           MOVE TP-TRCD TO D-TRCD.

           IF TP-REV = "Y"
              MOVE "*" TO D-REV.

           MOVE TP-REFNO TO D-REFNO.
           MOVE TP-TRAMT TO D-TRAMT.
           ADD TP-APINT TP-APLC GIVING D-APINT.
           MOVE TP-APCUR TO D-APCUR.
           MOVE TP-PDTH-DATE TO NUM-DATE.
           MOVE NUM-MO TO D-PDTHRU-MO.
           IF TP-UNITPER-CD-DAYS
              MOVE NUM-DA TO D-PDTHRU-DAYR
           ELSE
              MOVE NUM-YR TO D-PDTHRU-DAYR.
           MOVE TP-LCPDTH-DATE TO NUM-DATE.
           MOVE NUM-MO TO D-LCPD-MO.
           IF TP-UNITPER-CD-DAYS
              MOVE NUM-DA TO D-LCPD-DAYR
           ELSE
              MOVE NUM-YR TO D-LCPD-DAYR.
           COMPUTE D-OTHBAL = TP-OTHBAL + TP-OT2BAL.
           ADD TP-INTBAL TP-LCBAL GIVING D-INTBAL.
           MOVE TP-CURBAL TO D-PRINBAL.

           IF TP-CURBAL = 0
              IF NOT (TP-TRCD = "RC" OR "RA" OR "RP"
                              OR "R1" OR "R2" OR "R3")
                 MOVE "**PAIDOFF**" TO D-PAIDOFF
                 PERFORM WRITE-DETAIL-LINE.

           IF TP-GLAMT(1) NOT = 0
              MOVE TP-GLNO(1) TO D-GLNO
              INSPECT D-GLNO REPLACING ALL "/" BY "-".

           PERFORM WRITE-DETAIL-LINE.

           IF TP-GLAMT(1) NOT = 0 OR
               TP-DLRVCHGS-ADJ(1) NOT = 0 OR
                TP-DLRVCHGS-ADJ(2) NOT = 0 OR
                 TP-DLRVCHGS-ADJ(3) NOT = 0 OR
                  TP-DLPARVHELD-ADJ NOT = 0 OR
                   TP-DLPARVPAID-ADJ NOT = 0
              PERFORM SETUP-DLRVCHGS-ADJ
              MOVE TP-GLAMT(1) TO D-GLAMT
              PERFORM WRITE-DETAIL-LINE.

           IF TP-GLAMT(2) NOT = 0
              MOVE TP-GLNO(2) TO D-GLNO
              INSPECT D-GLNO REPLACING ALL "/" BY "-"
              PERFORM WRITE-DETAIL-LINE
              MOVE TP-GLAMT(2) TO D-GLAMT
              PERFORM WRITE-DETAIL-LINE.

           IF TP-GLAMT(3) NOT = 0
              MOVE TP-GLNO(3) TO D-GLNO
              INSPECT D-GLNO REPLACING ALL "/" BY "-"
              PERFORM WRITE-DETAIL-LINE
              MOVE TP-GLAMT(3) TO D-GLAMT
              PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       CREATE-WK SECTION.
      ******************************************************************
       CREATE-WK-REC.
           PERFORM SETUP-WK-KEY.
           PERFORM READ-WK-FILE.

           IF ( IO-FG = 0 )
              GO TO CREATE-WK-UPDATE.

           PERFORM SETUP-WK-KEY.
           MOVE 0 TO WK-NO.

           MOVE 0 TO WK-TRAMT WK-INTDUE WK-APOTH WK-APINT WK-APLC
                     WK-APCUR WK-CHECKS WK-EARNINGS WK-DLPROC.

           PERFORM WRITE-WK-FILE.

           IF ( IO-FG NOT = 0 )
              GO TO IO-ERROR.

           GO TO CREATE-WK-REC.

      *-----------------------------------------------------------------
       CREATE-WK-UPDATE.
           IF ( TP-TRAMT NOT < 0 )
              ADD 1 TO WK-NO
           ELSE
              SUBTRACT 1 FROM WK-NO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *                 "C" : POSTED FOR ANOTHER BRANCH.
      *                 "G" : POSTED BY ANOTHER BRANCH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF ( TP-OTHBR-FG = " " OR "C" )
      *------>IF ( TP-REFNO = "NSF  " )
      *          ADD TP-TRAMT TO WK-INTDUE T-INTDUE(1 LEV)
      *------>ELSE
              IF ( TP-TRCD = "Z2" )
                 ADD TP-TRAMT TO WK-TRAMT T-TRAMT(1 LEV)
                                         TT-TRAMT(1 LEVEL-TYPE).

           IF ( TP-OTHBR-FG = " " OR "G" )
              ADD TP-APOTH      TO WK-APOTH    T-APOTH(1 LEV)
                                               TT-APOTH(1 LEVEL-TYPE)
              ADD TP-APOT2      TO WK-APOTH    T-APOTH(1 LEV)
                                               TT-APOTH(1 LEVEL-TYPE)
              ADD HOLD-EARNINGS TO WK-EARNINGS T-EARNINGS(1 LEV)
                                               TT-EARNINGS(1 LEVEL-TYPE)
              ADD TP-DLPROC     TO WK-DLPROC   T-DLPROC(1 LEV)
                                               TT-DLPROC(1 LEVEL-TYPE)
              ADD TP-APLC       TO WK-APLC
              PERFORM TEST-ADD-APLC
      *------>IF ( (TP-TRCD = "RT" OR "ET" OR "LP" OR "NP") OR
      *            (TP-TRCD = "RV" AND
      *              (TP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL") ) )
      *          IF TP-IBPC = "I"
      *             SUBTRACT TP-APCUR FROM WK-APCUR
      *             SUBTRACT TP-APCUR FROM T-APCUR(1 LEV)
      *          ELSE
      *             SUBTRACT TP-TRAMT FROM WK-APCUR
      *             SUBTRACT TP-TRAMT FROM T-APCUR(1 LEV)
      *------>ELSE
                 ADD TP-APCUR TO WK-APCUR T-APCUR(1 LEV)
                                            TT-APCUR(1 LEVEL-TYPE)
                 ADD TP-APINT TO WK-APINT
                 IF ( TP-TRCD NOT = "WI" )
                    ADD TP-APINT TO T-APINT(1 LEV)
                                    TT-APINT(1 LEVEL-TYPE).

           IF ( TP-REFNO = "CHECK" )
              ADD TP-TRAMT TO WK-CHECKS T-CHECKS(1 LEV)
                                        TT-CHECKS(1 LEVEL-TYPE).

           PERFORM REWRITE-WK-FILE.

           IF ( TP-BRNO NOT = TP-POSTING-BR )
              PERFORM OTHER-BRANCH.

       EXIT-CREATE-WK.
           MOVE 0 TO SUB.
           PERFORM OUTPUT-GL 3 TIMES.


      ******************************************************************
       OUTPUT-GL SECTION.
      ******************************************************************
           ADD 1 TO SUB.

           IF TP-GLAMT(SUB) = 0
              GO TO END-OUTPUT-GL.

      * ONLY GIVE THE GL AMT TO THE BRANCH THAT IT WAS MADE FOR
           IF TP-OTHBR-FG NOT = " " AND NOT = "G"
              GO TO END-OUTPUT-GL.

           PERFORM SETUP-WK-GL1-KEY.
           PERFORM READ-WK-FILE.

           IF IO-FG = 0
              GO TO ACCUM-GL-WK.

           PERFORM SETUP-WK-GL1-KEY.
           MOVE TP-GLAMT(SUB) TO WK-GLAMT.
           PERFORM WRITE-WK-FILE.

           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO END-OUTPUT-GL.

      *-----------------------------------------------------------------
       ACCUM-GL-WK.
           ADD TP-GLAMT(SUB) TO WK-GLAMT.
           PERFORM REWRITE-WK-FILE.

       END-OUTPUT-GL.
           EXIT.

      ******************************************************************
       FIND-BORROWER SECTION.
      ******************************************************************
           IF ( TP-OTHBR-FG = "C" )
              MOVE "INTER-OFFICE PAYMENT" TO D-NAME
              GO TO END-FIND-BORROWER.

           MOVE TP-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.

           IF IO-FG = 9
               MOVE "** NO BORROWER MASTER **" TO D-NAME
               GO TO END-FIND-BORROWER.

           MOVE BW-LNAME TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           MOVE BW-FNAME TO FIRST-NAME.

           INSPECT NAME-WORKER REPLACING FIRST "                "
               BY FIRST-NAME.

           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-NAME.

       END-FIND-BORROWER.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.


      ******************************************************************
       SET-CLASS-HEADINGS SECTION.
      ******************************************************************
           MOVE HOLD-CLASS TO H-CLASS LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID" TO LC-DESC.

           MOVE LC-DESC TO H-LCDESC.
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       GL-SUMMARY SECTION.
      ******************************************************************
           MOVE GL-HEAD TO DETAIL-LINE.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE LOW-VALUE TO WK-KEY.
           MOVE "G" TO WK-CODE.
           PERFORM START-WK-FILE.

      *-----------------------------------------------------------------
       PRINT-NEXT-GL.
           PERFORM READ-WK-FILE-NEXT.

           IF IO-FG NOT = 0
              GO TO END-GL-SUMMARY.

           IF WK-CODE NOT = "G"
              GO TO PRINT-NEXT-GL.

           MOVE WK-GLNO TO DS-GLNO.
           INSPECT DS-GLNO REPLACING ALL "/" BY "-".
           MOVE WK-GLAMT TO DS-GLAMT.
           PERFORM WRITE-DETAIL-LINE.
           GO TO PRINT-NEXT-GL.

       END-GL-SUMMARY.
           EXIT.

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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GTT-HEADER-FG = "Y" )
              MOVE GTT-HEAD1 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE GTT-HEAD2 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE GTT-HEAD3 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE ALL "-" TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 10 TO LN-COUNT
              MOVE 1 TO LN-FEED
           ELSE
           IF SUMMARY-FG = "N"
              MOVE HEAD5 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE HEAD6 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE 6 TO LN-COUNT
              MOVE 2 TO LN-FEED
           ELSE
              MOVE 4 TO LN-COUNT
              MOVE 2 TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "MONTH-END DATE" TO RANGE-LINE.
           MOVE EDATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "DAY OF MONTH" TO RANGE-LINE.
           MOVE DAY-OF-MONTH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "GROUP TOTALS" TO RANGE-LINE.
           MOVE GTT-ISO TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.

      ***************************************************************
       CLASS-SCAN SECTION.
      ***************************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
              MOVE "SNNN020CL1." TO VDU
           ELSE
              MOVE "SNNN020CL2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      *****************************************************************
      * PROCESS TRANSACTIONS MADE TO THIS BRANCHES ACCOUNTS BY
      *   OTHER BRANCHES.
      *
      * TP-OTHBR-FG --> " " : POSTING BRANCH = OWNING BRANCH.
      *                 "C" : POSTED FOR ANOTHER BRANCH.
      *                 "G" : POSTED BY ANOTHER BRANCH.
      *****************************************************************
       OTHER-BRANCH SECTION.
           IF TP-OTHBR-FG = " " OR "G"
              IF TP-TRCD = "Z2"
                 ADD TP-TRAMT TO T-TRAMT(2 LEV).

           IF TP-OTHBR-FG = " " OR "G"
              ADD TP-APOTH TO T-APOTH(2 LEV)
              ADD TP-APOT2 TO T-APOTH(2 LEV)
              ADD HOLD-EARNINGS TO T-EARNINGS(2 LEV)
              ADD TP-DLPROC TO T-DLPROC(2 LEV)
              IF TP-TRCD NOT = "WL"
                 ADD TP-APLC TO T-APLC(2 LEV)
              END-IF
      *------>IF ( (TP-TRCD = "RT" OR "ET" OR "LP" OR "NP") OR
      *            (TP-TRCD = "RV" AND
      *              (TP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL") ) )
      *          IF TP-IBPC = "I"
      *             SUBTRACT TP-APCUR FROM T-APCUR(2 LEV)
      *          ELSE
      *             SUBTRACT TP-TRAMT FROM T-APCUR(2 LEV)
      *------>ELSE
                 ADD TP-APCUR TO T-APCUR(2 LEV)
                 IF TP-TRCD NOT = "WI"
                    ADD TP-APINT TO T-APINT(2 LEV).

           IF TP-REFNO = "CHECK"
              ADD TP-TRAMT TO T-CHECKS(2 LEV).
       EXIT-OTHER-BRANCH.
           EXIT.

      ******************************************************************
       CLASS-TOTALS SECTION.
      ******************************************************************

           IF ( GTT-ISO = "O" )
              GO TO END-CLASS-TOTALS.

           IF ( PRT-CLASS-TOTALS = "N" )
              GO TO END-CLASS-TOTALS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SUM-HEAD TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE SPACES TO WK-KEY.
           MOVE "L" TO WK-CODE.
           MOVE HOLD-BRNO TO WK-BRNO.
           MOVE HOLD-CLASS TO WK-CLASS.
           PERFORM START-WK-FILE.

      *-----------------------------------------------------------------
       PRINT-NEXT-CODE.
           PERFORM READ-WK-FILE-NEXT.

           IF IO-FG = 9 OR WK-CODE NOT = "L" OR
              WK-CLASS NOT = HOLD-CLASS OR WK-BRNO NOT = HOLD-BRNO
              PERFORM TYPE-TOTALS
              GO TO END-CLASS-TOTALS.

           MOVE WK-CLASS TO DS-CLASS LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID CLASS" TO LC-DESC.

           MOVE LC-DESC TO DS-DESC.
           MOVE WK-TRCD TO DS-TRCD.
           MOVE WK-NO TO DS-NO.
           MOVE WK-TRAMT TO DS-TRAMT.
           MOVE WK-INTDUE TO DS-INTDUE.
           MOVE WK-APOTH TO DS-APOTH.
           MOVE WK-APINT TO DS-APINT.
           MOVE WK-APLC TO DS-APLC.
           MOVE WK-APCUR TO DS-APCUR.
           MOVE WK-CHECKS TO DS-CHECKS.
           MOVE WK-EARNINGS TO DS-EARNINGS.
           MOVE WK-DLPROC TO DS-DLPROC.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

           GO TO PRINT-NEXT-CODE.

      *-----------------------------------------------------------------
       END-CLASS-TOTALS.

           IF ( GTT-ISO NOT = "O" )
              IF ( PRT-CLASS-TOTALS = "Y" )
                 MOVE SPACES TO DETAIL-LINE
                 PERFORM WRITE-DETAIL-LINE
                 MOVE 1      TO LEV1
                 MOVE 1      TO LEV2
                 PERFORM PRINT-SUMMARY-TT
                        VARYING LEV2 FROM 1 BY 1 UNTIL LEV2 > 7
                 MOVE ALL "-" TO DETAIL-LINE
                 PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE 2 TO LEV.
           MOVE 1 TO SUB.
           PERFORM UPDATE-TABLE2
                   VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2.

           MOVE 1 TO LEV1.
           MOVE 1 TO LEV2.
           MOVE 2 TO LEV3.
           PERFORM UPDATE-TTABLE2
                   VARYING LEV2 FROM 1 BY 1 UNTIL LEV2 > 7.

           MOVE 1 TO SUB2.
           PERFORM CLEAR-T-TOTALS
                   VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2

           MOVE 1 TO LEV1.
           PERFORM CLEAR-TT-TOTALS
                   VARYING LEV2 FROM 1 BY 1 UNTIL LEV2 > 7.

           MOVE "N" TO SUMMARY-FG.

       EXIT-CLASS-TOTALS.
           EXIT.

      ******************************************************************
       BRANCH-TOTALS SECTION.
      ******************************************************************

           IF ( GTT-ISO = "O" )
              GO TO END-BRANCH-TOTALS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PRT-BRANCH-TOTALS = "N" )
              GO TO END-BRANCH-TOTALS.

           IF ( PRT-CLASS-TOTALS = "N" )
              MOVE "Y" TO SUMMARY-FG
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              PERFORM WRITE-DETAIL-LINE
              MOVE SUM-HEAD TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "Y" TO SUMMARY-FG.
           MOVE 0 TO HOLD-CLASS.
           MOVE 2 TO LEV.
           MOVE 1 TO SUB1.
           MOVE 3 TO LN-FEED.
           MOVE "BRANCH TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.
           MOVE 0 TO HOLD-CLASS.
           MOVE 2 TO LEV.
           MOVE 2 TO SUB1.
           MOVE 2 TO LN-FEED.
           MOVE "POSTINGS FROM" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "OTHER BRANCHES:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.

      *-----------------------------------------------------------------
       END-BRANCH-TOTALS.
           IF ( GTT-ISO NOT = "O" )
              IF ( PRT-BRANCH-TOTALS = "Y" )
                 MOVE SPACES TO DETAIL-LINE
                 PERFORM WRITE-DETAIL-LINE
                 MOVE 2      TO LEV1
                 MOVE 1      TO LEV2
                 PERFORM PRINT-SUMMARY-TT
                         VARYING LEV2 FROM 1 BY 1 UNTIL LEV2 > 7
                 MOVE ALL "-" TO DETAIL-LINE
                 PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE 3 TO LEV.
           MOVE 2 TO SUB.
           PERFORM UPDATE-TABLE2 VARYING SUB1 FROM 1 BY 1
                                                   UNTIL SUB1 > 2.

           MOVE 2 TO LEV1.
           MOVE 1 TO LEV2.
           MOVE 3 TO LEV3.
           PERFORM UPDATE-TTABLE2
                   VARYING LEV2 FROM 1 BY 1 UNTIL LEV2 > 7.

           MOVE 2 TO SUB2.
           PERFORM CLEAR-T-TOTALS
                          VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2.

           MOVE 2 TO LEV1.
           PERFORM CLEAR-TT-TOTALS
                         VARYING LEV2 FROM 1 BY 1 UNTIL LEV2 > 7.

           MOVE "N" TO SUMMARY-FG.

       EXIT-BRANCH-TOTALS.
           EXIT.

      ******************************************************************
       GRAND-TOTALS SECTION.
      ******************************************************************

           IF ( GTT-ISO = "O" )
              GO TO END-GRAND-TOTALS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PRT-GRAND-TOTALS = "N" )
              GO TO END-GRAND-TOTALS.

           IF ( PRT-CLASS-TOTALS  = "N" ) AND
              ( PRT-BRANCH-TOTALS = "N" )
              MOVE "Y" TO SUMMARY-FG
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              PERFORM WRITE-DETAIL-LINE
              MOVE SUM-HEAD TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "Y" TO SUMMARY-FG.
           MOVE 3 TO LEV.
           MOVE 1 TO SUB1.
           MOVE 3 TO LN-FEED.
           MOVE "GRAND TOTALS :" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.
           MOVE 0 TO HOLD-CLASS.
           MOVE 3 TO LEV.
           MOVE 2 TO SUB1.
           MOVE 2 TO LN-FEED.
           MOVE "TOTAL POSTINGS" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "FROM OTHER BRANCHES:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.

      *-----------------------------------------------------------------
       END-GRAND-TOTALS.

           IF ( GTT-ISO NOT = "O" )
              IF ( PRT-GRAND-TOTALS = "Y" )
                 MOVE SPACES TO DETAIL-LINE
                 PERFORM WRITE-DETAIL-LINE
                 MOVE 3 TO LEV1
                 MOVE 1 TO LEV2
                 PERFORM PRINT-SUMMARY-TT
                         VARYING LEV2 FROM 1 BY 1 UNTIL LEV2 > 7
                 MOVE ALL "-" TO DETAIL-LINE
                 PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE 3 TO SUB2.
           PERFORM CLEAR-T-TOTALS
                         VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2.

           MOVE 3 TO LEV1.
           PERFORM CLEAR-TT-TOTALS
                         VARYING LEV2 FROM 1 BY 1 UNTIL LEV2 > 7.

           MOVE "Y" TO SUMMARY-FG.

       EXIT-GRAND-TOTALS.
           EXIT.

      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ******************************************************************

           ADD WK-NO          TO  WS-GTT-NO        ( GTT-TOTAL-SUB ).
           ADD WK-TRAMT       TO  WS-GTT-TRAMT     ( GTT-TOTAL-SUB ).
           ADD WK-INTDUE      TO  WS-GTT-INTDUE    ( GTT-TOTAL-SUB ).
           ADD WK-APOTH       TO  WS-GTT-APOTH     ( GTT-TOTAL-SUB ).
           ADD WK-APINT       TO  WS-GTT-APINT     ( GTT-TOTAL-SUB ).
           ADD WK-APLC        TO  WS-GTT-APLC      ( GTT-TOTAL-SUB ).
           ADD WK-APCUR      TO  WS-GTT-APCUR    ( GTT-TOTAL-SUB ).
           ADD WK-CHECKS      TO  WS-GTT-CHECKS    ( GTT-TOTAL-SUB ).
           ADD WK-EARNINGS    TO  WS-GTT-EARNINGS  ( GTT-TOTAL-SUB ).
           ADD WK-DLPROC      TO  WS-GTT-DLPROC    ( GTT-TOTAL-SUB ).

       EXIT-GTT-ADD-TOTALS.
           EXIT.

      ******************************************************************
       GTT-COMPILE-GROUP-TOTALS SECTION.
      ******************************************************************
           MOVE SPACES      TO WK-KEY.
           MOVE "L"         TO WK-CODE.
           MOVE WGR-SRESULT TO WK-BRNO.
           MOVE ZEROES      TO WK-CLASS.
           PERFORM START-WK-FILE.

      *-----------------------------------------------------------------
       GTT-NEXT-WK1.

           PERFORM READ-WK-FILE-NEXT.

           IF ( IO-FG   NOT = 0           ) OR
              ( WK-CODE NOT = "L"         ) OR
              ( WK-BRNO NOT = WGR-SRESULT )
                 GO TO EXIT-GTT-COMPILE-GROUP-TOTALS.

           MOVE "N" TO GTT-FIRST-TIME.

      *-----------------------------------------------------------------
       GTT-OK-TO-PROCESS.

           PERFORM GTT-CHECK-ADD-TOTALS.
           GO TO GTT-NEXT-WK1.

       EXIT-GTT-COMPILE-GROUP-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-GROUP-BR-TOTALS SECTION.
      ******************************************************************

           MOVE WGR-SID(GTT-PREV-SUB) TO WGR-WORD.
           MOVE WGR-NUMB              TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "BRANCH #&&&& NOT ON FILE" TO BR-NAME
              INSPECT BR-NAME REPLACING FIRST "&&&&" BY BR-NO.

           MOVE SPACES    TO DETAIL-LINE.
           MOVE 9         TO GTT-TOTAL-SUB.
           MOVE "BRANCH:" TO D2-PROMPT1 WS-HOLD-PROMPT.
           MOVE BR-NO     TO D2-BRNO.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-BR-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-GROUP-TOTALS SECTION.
      ******************************************************************

           MOVE SPACES    TO DETAIL-LINE.
           MOVE "GROUP :" TO D2-PROMPT1 WS-HOLD-PROMPT.
           MOVE GR-ID     TO D2-GRNO.
           MOVE GR-LEVEL  TO D2-GR-LEVEL.


           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-TOTALS SECTION.
      ******************************************************************
           MOVE WS-GTT-NO        (GTT-TOTAL-SUB) TO DS-NO.
           MOVE WS-GTT-TRAMT     (GTT-TOTAL-SUB) TO DS-TRAMT.
           MOVE WS-GTT-INTDUE    (GTT-TOTAL-SUB) TO DS-INTDUE.
           MOVE WS-GTT-APOTH     (GTT-TOTAL-SUB) TO DS-APOTH.
           MOVE WS-GTT-APINT     (GTT-TOTAL-SUB) TO DS-APINT.
           MOVE WS-GTT-APLC      (GTT-TOTAL-SUB) TO DS-APLC.
           MOVE WS-GTT-APCUR    (GTT-TOTAL-SUB) TO DS-APCUR.
           MOVE WS-GTT-CHECKS    (GTT-TOTAL-SUB) TO DS-CHECKS.
           MOVE WS-GTT-EARNINGS  (GTT-TOTAL-SUB) TO DS-EARNINGS.
           MOVE WS-GTT-DLPROC    (GTT-TOTAL-SUB) TO DS-DLPROC.

      *- - - - - - - - - - - - - - - - - - PRINT DETAIL AFTER LOAD - - -
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( WS-HOLD-PROMPT = "GROUP :" )
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - CLEAR LEVEL IN TABLE  - - -
           MOVE LOW-VALUES TO WS-GTT-TABLE-GROUP-LEVEL(GTT-TOTAL-SUB).

       EXIT-GTT-PRINT-TOTALS.
           EXIT.

      ******************************************************************
       CHECK-PARAMETERS SECTION.
      ******************************************************************

           MOVE TP-DATE    TO HOLD-TRANS-DATE.
           MOVE ZEROES     TO ACCOUNT-TYPE.

           MOVE WK2-BRANCH TO LN-OWNBR.
           MOVE WK2-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF ( IO-FG NOT = 0 )
              GO TO IO-ERROR.

      *-----------------------------------------------------------------
      *
      *    SET-ACCOUNT-TYPE ----- SET TYPE FOR IF OPEN, REPO, OR PL.
      *
      *-----------------------------------------------------------------
       SET-ACCOUNT-TYPE.

           PERFORM RANKING-DATES.

      *- - - - - - - - - - - - - - - - - - - - - - - OPEN ACCOUNTS - - -
           IF ( SUB-TRANS = 1 )
              MOVE 1 TO ACCOUNT-TYPE
           ELSE
           IF ( (SUB-TRANS - 1) = SUB-REDEEM )
              MOVE 1 TO ACCOUNT-TYPE
           ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - REPO ACCOUNTS - - -
           IF ( (SUB-TRANS - 1) = SUB-REPO )
              MOVE 2 TO ACCOUNT-TYPE
           ELSE
           IF ( (SUB-TRANS - 1) = SUB-SOLD )
              MOVE 2 TO ACCOUNT-TYPE
           ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - PL ACCOUNTS - - -
              MOVE 3 TO ACCOUNT-TYPE.

      *-----------------------------------------------------------------
      *
      *    CHECK IF INCLUDE, SKIP OR ONLY.
      *
      *-----------------------------------------------------------------
       CHECK-FOR-ISO.

      *- - - - - - - - - - - - - - - - - - - - - - - OPEN ACCOUNTS - - -
           IF ( LNTYPE = "S" ) AND ( ACCOUNT-TYPE-OPEN )
              MOVE ZEROES TO ACCOUNT-TYPE.

      *- - - - - - - - - - - - - - - - - - - - - - - REPO ACCOUNTS - - -
           IF ( REPOTYPE = "S" ) AND  ( ACCOUNT-TYPE-REPO )
              MOVE ZEROES TO ACCOUNT-TYPE.

      *- - - - - - - - - - - - - - - - - - - - - - - - PL ACCOUNTS - - -
           IF ( PLACCT = "S" ) AND ( ACCOUNT-TYPE-PL )
              MOVE ZEROES TO ACCOUNT-TYPE
           ELSE
           IF ( ACCOUNT-TYPE-PL )
              IF ( PLTYPE NOT = "A" )
                 IF ( LN-PLTYPE NOT = PLTYPE )
                    MOVE ZEROES TO ACCOUNT-TYPE.

       EXIT-CHECK-PARAMETERS.
           EXIT.

      ******************************************************************
      *
      *    R A N K I N G - D A T E S
      *
      *    THIS TO FIND THE STATUS OF THE LOAN AT THE TIME OF POSTING
      *    A "Z2" TRANSACTION.  THE LOAN STATUS CAN ONLY BE ONE OF
      *    THE FOLLOWING AT ONE TIME:
      *       OPEN ACCT --> NO REPODATE, OR NO PLDATE, OR REDEEMDATE IS
      *                     THE MOST RECENT.
      *       REPO ACCT --> THERE IS REPODATE, OR A SOLDDATE, AND THERE
      *                     IS NO PLDATE.
      *         PL ACCT --> THERE IS PLDATE.
      *
      ******************************************************************
       RANKING-DATES SECTION.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DATE-RANKINGS MUST ALWAYS BE RESET TO "12345"!!!
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "12345"         TO DATE-RANKINGS.
           MOVE LN-REPODATE     TO HOLD-DATE(1).
           MOVE LN-REDEEMDATE   TO HOLD-DATE(2).
           MOVE LN-SOLDDATE     TO HOLD-DATE(3).
           MOVE LN-PLDATE       TO HOLD-DATE(4).
           MOVE HOLD-TRANS-DATE TO HOLD-DATE(5).
           MOVE 2               TO SUB5.

           PERFORM COMPARE-DATE-RANGES
                   VARYING SUB3 FROM 1    BY 1 UNTIL SUB3 > 4
                     AFTER SUB4 FROM SUB5 BY 1 UNTIL SUB4 > 5.

           PERFORM SET-SUBSCRIPTS
                   VARYING SUB3 FROM 1 BY 1 UNTIL SUB3 > 5.

           GO TO EXIT-RANKING-DATES.

      *=================================================================
       COMPARE-DATE-RANGES.

           COMPUTE SUB5 = SUB3 + 1.

           IF HOLD-DATE(SUB3) > HOLD-DATE(SUB4)
              MOVE DATE-RANK(SUB3) TO HOLD-DATE-RANK
              MOVE DATE-RANK(SUB4) TO DATE-RANK(SUB3)
              MOVE HOLD-DATE-RANK  TO DATE-RANK(SUB4)
              MOVE HOLD-DATE(SUB3) TO HOLD-DATE-FIELD
              MOVE HOLD-DATE(SUB4) TO HOLD-DATE(SUB3)
              MOVE HOLD-DATE-FIELD TO HOLD-DATE(SUB4).

      *=================================================================
       SET-SUBSCRIPTS.
           IF ( DATE-RANK(SUB3) = "1" )
              MOVE SUB3 TO SUB-REPO
           ELSE
           IF ( DATE-RANK(SUB3) = "2" )
              MOVE SUB3 TO SUB-REDEEM
           ELSE
           IF ( DATE-RANK(SUB3) = "3" )
              MOVE SUB3 TO SUB-SOLD
           ELSE
           IF ( DATE-RANK(SUB3) = "4" )
              MOVE SUB3 TO SUB-PL
           ELSE
           IF ( DATE-RANK(SUB3) = "5" )
              MOVE SUB3 TO SUB-TRANS.

       EXIT-RANKING-DATES.
           EXIT.

      ****************************************************************
      *
      *    S E T U P - L E V - T Y P E
      *
      *-----------------------------------------------------------------
      *  TOTALS TYPE 1 = "OP" OPEN ACCOUNTS  5 = "P3"
      *              2 = "RE" REPO ACCOUNTS  6 = "P4"
      *              3 = "PL"                7 = "P5"
      *              4 = "P2"
      *****************************************************************
       SETUP-LEVEL-TYPE SECTION.

           IF ( ACCOUNT-TYPE-OPEN )
              MOVE 1 TO LEVEL-TYPE
           ELSE
           IF ( ACCOUNT-TYPE-REPO )
              MOVE 2 TO LEVEL-TYPE
           ELSE
           IF ( ACCOUNT-TYPE-PL )
              IF ( TP-PLTYPE = "P" )
                 MOVE 3 TO LEVEL-TYPE
              ELSE
              IF ( TP-PLTYPE = "2" )
                 MOVE 4 TO LEVEL-TYPE
              ELSE
              IF ( TP-PLTYPE = "3" )
                 MOVE 5 TO LEVEL-TYPE
              ELSE
              IF ( TP-PLTYPE = "4" )
                 MOVE 6 TO LEVEL-TYPE
              ELSE
                 MOVE 7 TO LEVEL-TYPE.

      *-----------------------------------------------------------------
       EXIT-SETUP-LEVEL-TYPE.
           EXIT.
      *****************************************************************
      * CLEAR TTOTAL TABLE
      *
      *  LEV1 --> 1 = CLASS  TOTALS
      *           2 = BRANCH TOTALS
      *           3 = GRAND  TOTALS
      *
      *  LEV2 --> 1 = "OP" OPEN ACCOUNTS
      *           2 = "RE" REPO ACCOUNTS
      *           3 = "PL"
      *           4 = "P2"
      *           5 = "P3"
      *           6 = "P4"
      *           7 = "P5"
      *****************************************************************
       CLEAR-TT-TOTALS SECTION.
           MOVE 0 TO TT-TRAMT  (LEV1,LEV2)   TT-INTDUE   (LEV1,LEV2)
                     TT-APOTH  (LEV1,LEV2)   TT-APINT    (LEV1,LEV2)
                     TT-APLC   (LEV1,LEV2)   TT-APCUR   (LEV1,LEV2)
                     TT-CHECKS (LEV1,LEV2)   TT-EARNINGS (LEV1,LEV2)
                     TT-DLPROC (LEV1,LEV2).

      *****************************************************************
      *
      *  TYPE   LEV1 = POSTING FROM THIS BRANCH
      *
      *****************************************************************
      * CLEAR TOTAL TABLE
      *
      *  SUB1 --> 1 = OWNING BRANCH IS POSTING BRANCH
      *           2 = OWNING BRANCH IS NOT THE POSTING BRANCH
      *               POSTED FROM AMOTHER BRANCH.
      *
      *****************************************************************
       CLEAR-T-TOTALS SECTION.
           MOVE 0 TO T-TRAMT  (SUB1,SUB2)   T-INTDUE   (SUB1,SUB2)
                     T-APOTH  (SUB1,SUB2)   T-APINT    (SUB1,SUB2)
                     T-APLC   (SUB1,SUB2)   T-APCUR   (SUB1,SUB2)
                     T-CHECKS (SUB1,SUB2)   T-EARNINGS (SUB1,SUB2)
                     T-DLPROC (SUB1,SUB2).

      ******************************************************************
       GET-DISPLAY-LNNO SECTION.
      ******************************************************************

      * LPS-DISPLAY-LNNO IS SET FIRST SO THAT IF THERE IS A ERROR
      * IT WILL BE ALREADY DISPLAYING FOR A ERROR, OTHERWISE IT WILL
      * RESET LPS-DISPLAY-LNNO IN 'PERFORM LPSTAT-DISPLAY.'

           MOVE TP-CLASS  TO LPS-CLASS.
           MOVE "-"       TO LPS-DASH.
           MOVE TP-ACCTNO TO LPS-NUMBER.
           MOVE "{N/A}"   TO LPS-7STATUS.

           MOVE TP-BRNO   TO LN-OWNBR.
           MOVE TP-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF ( IO-BAD )
              GO TO GET-DISPLAY-LNNO-EXIT.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
           IF ( IO-BAD )
              GO TO GET-DISPLAY-LNNO-EXIT.

           PERFORM LPSTAT-DISPLAY.

       GET-DISPLAY-LNNO-EXIT.
           EXIT.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO EMZ2RG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EMZ2RG-SYSDATE.
           DISPLAY EMZ2RG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EMZ2RG-FORM-FIELDS TO WR-BUF.
           MOVE EMZ2RG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE EMZ2RG-HELP--FILE TO HELP-LINK-FILE.
           MOVE EMZ2RG-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EMZ2RG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       SET-BRANCH-HEADINGS.
           MOVE HOLD-BRNO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.
      *-----------------------------------------------------------------
       SETUP-DLRVCHGS-ADJ.
           IF TP-DLRVCHGS-ADJ(1) NOT = 0
              MOVE "RSV ADJ 1: " TO D-DLTAG1
              MOVE TP-DLRVCHGS-ADJ(1) TO D-DLADJ1.

           IF TP-DLRVCHGS-ADJ(2) NOT = 0
              MOVE "RSV ADJ 2: " TO D-DLTAG2
              MOVE TP-DLRVCHGS-ADJ(2) TO D-DLADJ2.

           IF TP-DLRVCHGS-ADJ(3) NOT = 0
              MOVE "RSV ADJ 3: " TO D-DLTAG3
              MOVE TP-DLRVCHGS-ADJ(3) TO D-DLADJ3.

           IF TP-DLPARVHELD-ADJ NOT = 0
              MOVE "PVHELD ADJ: " TO D-DLTAG4
              MOVE TP-DLPARVHELD-ADJ TO D-PVHELD.

           IF TP-DLPARVPAID-ADJ NOT = 0
              MOVE "PVPAID ADJ: " TO D-DLTAG5
              MOVE TP-DLPARVPAID-ADJ TO D-PVPAID.

      *-----------------------------------------------------------------
       TEST-ADD-APLC.
           IF TP-TRCD NOT = "WL"
              ADD TP-APLC TO T-APLC(1 LEV) TT-APLC(1 LEVEL-TYPE).

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SETUP-WK-KEY.
           MOVE SPACES TO WK-REC.
           MOVE "L" TO WK-CODE.
           MOVE TP-BRNO  TO WK-BRNO.
           MOVE TP-CLASS TO WK-CLASS.
           MOVE TP-TRCD  TO WK-TRCD.

      *-----------------------------------------------------------------
       SETUP-WK-GL1-KEY.
           MOVE SPACES TO WK-REC.
           MOVE "G" TO WK-CODE.
           MOVE TP-GLNO(SUB) TO WK-GLNO.

      *-----------------------------------------------------------------
       UPDATE-TABLE2.
           ADD T-TRAMT(SUB1 SUB) TO T-TRAMT(SUB1 LEV).
           ADD T-INTDUE(SUB1 SUB) TO T-INTDUE(SUB1 LEV).
           ADD T-APOTH(SUB1 SUB) TO T-APOTH(SUB1 LEV).
           ADD T-APINT(SUB1 SUB) TO T-APINT(SUB1 LEV).
           ADD T-APLC(SUB1 SUB) TO T-APLC(SUB1 LEV).
           ADD T-APCUR(SUB1 SUB) TO T-APCUR(SUB1 LEV).
           ADD T-CHECKS(SUB1 SUB) TO T-CHECKS(SUB1 LEV).
           ADD T-EARNINGS(SUB1 SUB) TO T-EARNINGS(SUB1 LEV).
           ADD T-DLPROC(SUB1 SUB) TO T-DLPROC(SUB1 LEV).

      *-----------------------------------------------------------------
      *  LEV1 --> 1 = CLASS  TOTALS
      *  LEV3     2 = BRANCH TOTALS
      *           3 = GRAND  TOTALS
      *
      *  LEV2 --> 1 = "OP" OPEN ACCOUNTS
      *           2 = "RE" REPO ACCOUNTS
      *           3 = "PL"
      *           4 = "P2"
      *           5 = "P3"
      *           6 = "P4"
      *           7 = "P5"
      *-----------------------------------------------------------------
       UPDATE-TTABLE2.
           ADD TT-TRAMT(LEV1 LEV2)    TO TT-TRAMT(LEV3 LEV2).
           ADD TT-INTDUE(LEV1 LEV2)   TO TT-INTDUE(LEV3 LEV2).
           ADD TT-APOTH(LEV1 LEV2)    TO TT-APOTH(LEV3 LEV2).
           ADD TT-APINT(LEV1 LEV2)    TO TT-APINT(LEV3 LEV2).
           ADD TT-APLC(LEV1 LEV2)     TO TT-APLC(LEV3 LEV2).
           ADD TT-APCUR(LEV1 LEV2)   TO TT-APCUR(LEV3 LEV2).
           ADD TT-CHECKS(LEV1 LEV2)   TO TT-CHECKS(LEV3 LEV2).
           ADD TT-EARNINGS(LEV1 LEV2) TO TT-EARNINGS(LEV3 LEV2).
           ADD TT-DLPROC(LEV1 LEV2)   TO TT-DLPROC(LEV3 LEV2).

      *-----------------------------------------------------------------
       TYPE-TOTALS.
           MOVE 1 TO LEV.
           MOVE 1 TO SUB1.
           MOVE 3 TO LN-FEED.
           MOVE "CLASS  TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.

      *-----------------------------------------------------------------
       PRINT-SUMMARY.
           MOVE 0 TO DS-NO.
           MOVE T-TRAMT(SUB1 LEV)    TO DS-TRAMT.
           MOVE T-INTDUE(SUB1 LEV)   TO DS-INTDUE.
           MOVE T-APOTH(SUB1 LEV)    TO DS-APOTH.
           MOVE T-APINT(SUB1 LEV)    TO DS-APINT.
           MOVE T-APLC(SUB1 LEV)     TO DS-APLC.
           MOVE T-APCUR(SUB1 LEV)   TO DS-APCUR.
           MOVE T-CHECKS(SUB1 LEV)   TO DS-CHECKS.
           MOVE T-EARNINGS(SUB1 LEV) TO DS-EARNINGS.
           MOVE T-DLPROC(SUB1 LEV)   TO DS-DLPROC.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------------
       PRINT-SUMMARY-TT.
           IF ( LEV2 = 1 )
              MOVE "OPEN ACCOUNTS:" TO DETAIL-LINE
           ELSE
           IF ( LEV2 = 2 )
              MOVE "REPO ACCOUNTS:" TO DETAIL-LINE
           ELSE
           IF ( LEV2 = 3 )
              MOVE "  PL ACCOUNTS:" TO DETAIL-LINE
           ELSE
           IF ( LEV2 = 4 )
              MOVE "  P2 ACCOUNTS:" TO DETAIL-LINE
           ELSE
           IF ( LEV2 = 5 )
              MOVE "  P3 ACCOUNTS:" TO DETAIL-LINE
           ELSE
           IF ( LEV2 = 6 )
              MOVE "  P4 ACCOUNTS:" TO DETAIL-LINE
           ELSE
           IF ( LEV2 = 7 )
              MOVE "  P5 ACCOUNTS:" TO DETAIL-LINE.

           MOVE 0 TO DS-NO.
           MOVE TT-TRAMT(LEV1, LEV2)    TO DS-TRAMT.
           MOVE TT-INTDUE(LEV1, LEV2)   TO DS-INTDUE.
           MOVE TT-APOTH(LEV1, LEV2)    TO DS-APOTH.
           MOVE TT-APINT(LEV1, LEV2)    TO DS-APINT.
           MOVE TT-APLC(LEV1, LEV2)     TO DS-APLC.
           MOVE TT-APCUR(LEV1, LEV2)   TO DS-APCUR.
           MOVE TT-CHECKS(LEV1, LEV2)   TO DS-CHECKS.
           MOVE TT-EARNINGS(LEV1, LEV2) TO DS-EARNINGS.
           MOVE TT-DLPROC(LEV1, LEV2)   TO DS-DLPROC.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------------
       SETUP-D-TRAMT.
           IF TP-IBPC = "I"
              COMPUTE D-TRAMT = TP-TRAMT * -1
           ELSE
              COMPUTE D-TRAMT = TP-APCUR * -1.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPTRP1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              IF EXT-ROUTE-BUF = "00" OR "70"
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY EMZ2RG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
