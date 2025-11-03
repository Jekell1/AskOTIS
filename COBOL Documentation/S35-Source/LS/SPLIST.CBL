       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPLIST.
       DATE-WRITTEN. 04-16-84
      ******************************************************************
      *
      *    DIR :  LS
      *    DESC:  STATE PARAMETER FILE LIST
      *
      *=================================================================
      *  REV:
      *  870803 AMW ADD SP-ACCRUAL-FLAG TO REPORT.
      *  021588 BAW ADDED NEW SCFRMLA
      *  022688 JPN ADDED CODE FOR SP-ERNFRMLA "T"
      *  033188 JTG MODIFIED LOGIC TO DISPLAY '0'
      *             SP-RBDEFER FOR INTEREST ONLY
      *  121388 JTG ADDED DESC FOR SCFRMLA & DEFPERCD
      *  053089 SLC ADDED THE NEW FIELDS FOR PAGE 14...
      *             EARN ORDERS AND EARN STOP/START INFO.
      *  090689 JTG ADDED DESC FOR SP-LCTYPE 'J'
      *  110789 SLC REMOVED SP-IBINTACRFRMLA.
      *  010590 JTG ADDED LOGIC FRO SP-INSCHGABL = 'S'
      *  033188 JTG MODIFIED LOGIC TO DISPLAY '0'
      *             SP-RBDEFER FOR DEFERMENTS ALSO
      *  040490 JTG ADDED LOGIC FOR RECENCY / DELINQUENCY
      *  040590 SLC SET SUB TO 0 FOR FORM-TABLE. DID NOT RESET IT
      *             FOR MULTIPLE SPR LISTS.
      *  041790 JTG ADDED DISCLOSURE = 'C' (US RULE)
      *  111991 BAH REPLACED EXTENTION WITH ADDON
      *  050192 JTG ADDED OPTIONS FOR SP-RECFRMLA
      *  091192 BAH REMOVED INSURANCE LINE NUMBERS
      *  071095 MJD ADDED SP-INSREJECT-FG() AND PAGE 19.
      *  101995 JFF ADDED CLASS SCAN
      *  MJD 011096 ADDED SP-MAX-ALLOW-DELINQ.
      *  JTG 051596 CHANGED SP-INS-RATE-FILE VALUES TO 71 THRU 99
      *  080196 GLF ADDED TO PAGE 1:  CON PAY FREQ CODE; TRW BANK RULE;
      *             TRW PAYFREQ CODE; RECENCY FORMULA; LC DELINQUENCY,
      *             AND LC PAY FREQ CD. REMOVED FROM PAGE 1: FINANCE
      *             CHG CD AND TRANSFERRED EXPIRED LOAN FIELDS TO
      *             PAGE 20. ALSO ADDED MOST OF PAGE 18.
      *  041097 JTG MADE SP-EXPR-FRMLA 'B' VALID AND ADDED
      *             88  SP-EXPR-FRMLA-ORIG         VALID "B".
      *             88  SP-EXPR-FRMLA-USE-EXPIRED  VALID "A" "B".
      *  MJD 060497 ADDED O3 INSURANCE REBATE INFO TO PAGES 12 AND 13.
      *  KEC 090497 ***CONVERSION-A90-TO-A10***
      *                ADDED SP-DUAL-IN-BRANCH AND SP-DUAL-IN-STATE
      *                TO PAGE 17.
      *  JTG 091697 ADDED 'D' WORLD A90, SPBANKRULE & SP-TRWBANKRUKE
      *  GLF 092397 CHANGED IFLEV TO ILEV, ADDED SP-INSFINCHG
      *  GLF 092497 UPDATED LISTING TO MATCH MAINTENANCE SCREENS
      *             BY ADDING:  SP-RESCIND-SATURDAY; SP-DESC;
      *             SP-MAX-ALLOW-DELINQ; SP-INSAH-JOINT-FG;
      *             SP-RBUNITPER-CD; SP-MIN-CREDIT-SCORE;
      *             SP-PACESETTER-FG; AND SP-WAGE-ASSIGN-DAYS
      *             SP-RESCIND-BUS-DAYS
      *  BLV 110697 INS RATE TABLE WAS GETTING FLAGGED FOR RATES > 10 &
      *             < 100; FIXED TO BE > 70 & < 100 (LOAD-TEMP-TBL)
      *  GLF 121297 ADDED "SUB-CLASS SUMMARY ONLY" OPTION
      *  KEC 071398 ADDED SP-CAN-LOANS-FG.                      PR#: 187
      *  CS  101598 ADDED PAGE 21, PAGE 22 REVOLVING
      *  JTG 990113 ADDED SP-EDT-TYPE 'P' POINTS,           REGENCY #012
      *  CS  990210 FIXED THE SUMMARY PRINT, THE FIRST SPR WAS NOT PRINTING
      *  CS  990308 ADDED ADDITIONAL APPLY-ORDERS (PAGE 21)
      *  GLB 990414 ADD PAGE 2; SP-ALLOW-DELINQ-POLCD, SP-YTD-MAX-ALLOW-
      *      DELINQ, SP-MAX-ALLOW-DELINQ
      *  GLB 990414 ADD PAGE 16; SP-DLDISC-RV-PRINT1-PCT
      *  GLB 990414 ADD PAGE 17; SP-LNCOST-FRMLA, SP-LNCOST-AMT
      *  JTG 990908 ADDED SP-BANKRULE & SP-TRWBANKRULE 'R' & 'S' REGWAN #044
      *  JTG 991214 ADDED SP-DLDISC-RVPRIN1-AF-PCNT, TFC #058
      *  BAH 000124 ADDED SP-RBCOMCD VALUE "B" FOR REGENCY, PR# 1048 FOR NOT
      *             ALLOWING COMBINED CL & AH REBATE OF LESS THAN $10.00,
      *  JTG 010228 ADDED NEW RATE TYPE 'Z' AMORTIZED WHERE RUNTIME COMPUTES
      *             INTEREST ON A 30 DAY METHOD (REAL ESTATE) AND LOAN CALCS
      *             ARE DONE SIMPLE                   LENDMARK PR#1045 & PR#1380
      *  BAH 030930 WAS NOT PRINT SP-PRFORM(11 - 20), NSA LAURA
      *  BAH 031204 ADD EFFDATE AND EXPDATE TO SUMMARY PRINT, KEVIN PR# 2210
      *  040331 BAH ADDED SP-LC-ON-JUDGEMENTS FOR RIVERSIDE, PR# 2272, STATE
      *             LAW SAYS NO LATE CHARGES ON JUDGEMENT ACCOUNTS
      *  AMM 061106 {PL#0560} JUDY; MAKE ALL FIELDS IN LISTING PRINT
      *  AMM 070206 UPDATED LISTING
      *  BAH 090730 ADDED NON-EXPIRED SPR'S ONLY OPTION FOR WORLD PR# 650
      *  BAH 110307 ADDED SP-RBCOMCD "C" FOR REGENCY PR# 3743
      *  BAH 110711 ADD RBFRMLA2 AND REMOVED RBFRMLA DESCR, ADDED OTHER
      *             MISSING FIELDS FROM PAGE 1, JIMR WORLD
      *   CS 110823 ADDED SP-TRWBANKRULE 'F' WORLD MEXICO SEMI-MONTHLY PR#752
      *  MJD 111213 ADDED SP-ADV-TO-1ST-NO-CORR                      71226 #3867
      * BAH 2015.0112 ADDED SPR PAGE NUMBER, SUPPORT PR#4683
      * KEC 2015.0611 REMOVED THE USE OF 'INDEXED BY' ON OCCURS FOR USE WITH
      *               MICROFOCUS.
      * KEC 2016.0428 FIXED THE USE OF ILEV FOR PRINTING THE INSURANCE TABLES
      *               ON PAGES 5 THRU 11 & 19. REGENCY #929
      *  CS 2016.0908 ADD SP-LC-INT-ON-REPO-NON-PL,P-CONTRACTUAL-SET-NONACCRUAL
      *               SP-CONTRACTUAL-RESET-ACCRUAL SP-MONTHS-PAY-NAPP-COUNTER
      *               SP-SHORT-PAY-THRESHOLD SP-CASH-AVAIL-PERCENT
      *               SP-NET-BAL-REQP-AMT-PROCEEDS-REQ SP-AMT-PROCEEDS-REQ
      *               SP-ALT-PREPAY-MONTHS TO PAGE 18.  WORLD PR# 1185
      *               CHANGE BEG-CLASS/END-CLASS TO PIC 9(3), ITS MOVED TO
      *               SP1-KEY, CHANGE TDEX TO 9(3), TESTING > 99 AND
      *               INSTEAD OF 100 IT WAS 0 
      *  CS 2016.0922 CHANGE PRINT FIELD CAL-RATE1 CAL-RATE2 TO ZZZ.ZZZZ  #1192
      * BAH 2016.1010 ADDED PAGE 23 AND SP-PERCENT-REGPAY-AVAIL WORLD PR#1194
      * KEC 2017.0301 CHANGED WS FIELDS TO HAVE 'SP' FILE PREFIXES.
      * BAH 2017.0504 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2018.0111 ADDED SP-NON-LOAN-FLAG & SP-SKIP-TRW WORLD PR#1297
      * BAH 2019.0214 REMOVED REVOLVING
      * KEC 2019.0717 COMMENTED OUT THE USE OF SCREEN-21-REVOL AND
      *               SCREEN-22-REVOL.
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
      /
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/SPLIST.CBL S35 03/29/24-10:37:35 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01IR.CPY".
       COPY "LIBLP/LP01IR_SQL.CPY".
       COPY "LIBLP/LPWSIR.CPY".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 3.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 99       COMP-3 VALUE 0.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X         VALUE " ".
           88  NO-RECORDS-FOUND               VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  INS-AREA-01.
           03  INS-AREA.
               05  FILLER      PIC X(44)    VALUE
                   "CL S DECRE:CL J DECRE:CL S LEVEL:CL J LEVEL:".
               05  FILLER      PIC X(33)    VALUE
                   "A & H INS :PP INS    :INTEREST  :".
               05  FILLER      PIC X(44)    VALUE
                   "SERV CHRG :MAINT FEE :DEFERMENT :NOT USED  :".
               05  FILLER      PIC X(44)    VALUE
                   "OTH 1 INS :OTH 2 INS :OTH 3 INS :COMBINE   :".
           03  INS-TBL REDEFINES INS-AREA.
               05  INS-ITEM    PIC X(11)    OCCURS 15 TIMES.

      *-----------------------------------------------------------------
      *  KEC 2015.0611 REMOVED 'INDEXED BY' FOR 'OCCURS'
      *-----------------------------------------------------------------
      *01  TEMP-TABLE-01.
      *    03  TEMP-TABLE OCCURS 99 TIMES
      *                   INDEXED BY TDEX.
      *        05  TEMP-TBL    PIC X.
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       77  TDEX                        PIC 9(03)     VALUE ZEROES.
       01  TEMP-TABLE-01.
           03  TEMP-TABLE OCCURS 99 TIMES.
               05  TEMP-TBL    PIC X.

      *-----------------------------------------------------------------
       01  KOUNT           PIC 99.
       01  PRINT-SUB       PIC 9(3).
       01  CLASS-FG        PIC X(3)      VALUE "   ".
           88  CLASS-BEG                 VALUE "BEG".
           88  CLASS-END                 VALUE "END".
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  BEG-SP1-KEY.
               05  BEG-SP-ORGST        PIC XX.
               05  BEG-SP-SPRCLASS     PIC 999.
               05  BEG-SP-SUBCLASS     PIC 99.
               05  BEG-SP-LAWCODE      PIC 99.
           03  END-SP1-KEY.
               05  END-SP-ORGST        PIC XX.
               05  END-SP-SPRCLASS     PIC 999.
               05  END-SP-SUBCLASS     PIC 99.
               05  END-SP-LAWCODE      PIC 99.
           03  SUM-ONLY           PIC X.
           03  NON-EXPIRED-ONLY   PIC X.
           03  SPR-PAGE-NUMBER    PIC 99.

       01  ACCEPT-BUF          PIC X.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************

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

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(41)    VALUE
           "- - - STATE PARAMETER FILE LISTING - - -".
           03  FILLER           PIC X(45)    VALUE " ".

       01  HEAD3.
           03  H-TAG       PIC X(14).
           03  H-COLON     PIC X.
           03  FILLER      PIC X        VALUE " ".
           03  H-CODE      PIC XX.
           03  H-CLASS REDEFINES H-CODE PIC 99.
           03  FILLER      PIC X        VALUE " ".
           03  H-DESC      PIC X(16).
           03  GROUP-1.
               05  FILLER      PIC X(13)    VALUE " ".
               05  H-TAG2      PIC X(14).
               05  H-COLON2    PIC X.
               05  FILLER      PIC X        VALUE " ".
               05  H-EFFDATE   PIC 99/99/99.
               05  FILLER      PIC X(23)    VALUE " ".
           03  GROUP-2         REDEFINES GROUP-1.
               05  H-NOTES     PIC X(60).

       01  HEAD3A.
           03  FILLER      PIC X(3).
           03  FILLER      PIC X(5) VALUE "STATE".
           03  FILLER      PIC X(5).
           03  FILLER      PIC X(5) VALUE "CLASS".
           03  FILLER      PIC X(8).
           03  FILLER      PIC X(9) VALUE "SUB CLASS".
           03  FILLER      PIC X(5).
           03  FILLER      PIC X(11) VALUE "DESCRIPTION".
           03  FILLER      PIC X(15).
           03  FILLER      PIC X(8) VALUE "LAW CODE".
           03  FILLER      PIC X(6).
           03  FILLER      PIC X(8) VALUE "EFF DATE".
           03  FILLER      PIC X(6).
           03  FILLER      PIC X(8) VALUE "EXP DATE".

       01  HEAD4.
           03  H-PG1       PIC X(9).
           03  FILLER      PIC X(53)   VALUE " ".
           03  H-PG2       PIC X(9).

       01  HEAD5.
           03  FILLER         PIC X(17)  VALUE " ".
           03  HD5-1          PIC X(15).
           03  FILLER         PIC X(04)  VALUE " ".
           03  HD5-2          PIC X(15).
           03  FILLER         PIC X(72)  VALUE " ".
       01  INS-HEAD1.
           03  INS-TAG1    PIC X(33).
           03  FILLER      PIC X(9)    VALUE " ".
           03  INS-TAG2    PIC X(24).
           03  FILLER      PIC X(10)    VALUE " ".
           03  INS-TAG3    PIC X(26).
           03  FILLER      PIC X(16)    VALUE " ".
           03  INS-TAG4     PIC X(14).

       01  P12-HEAD.
           03  FILLER               PIC X(15)  VALUE " ".
           03  P12-TAG1             PIC X(7).
           03  P12-TAG2             PIC X(8).
           03  P12-TAG3             PIC X(9).
           03  P12-TAG4             PIC X(6).
           03  P12-TAG5             PIC X(10).
           03  P12-TAG6             PIC X(7).
           03  P12-TAG7             PIC X(8).
           03  P12-TAG8             PIC X(8).
           03  P12-TAG9             PIC X(8).
           03  P12-TAG10            PIC X(9).
           03  P12-TAG11            PIC X(6).

       01  INS-REBATE-HEAD.
           03  FILLER                 PIC X(17)  VALUE " ".
           03  R-TAB-TAG1             PIC X(15).
           03  R-TAB-TAG2             PIC X(10).
           03  R-TAB-TAG3             PIC X(6).
           03  R-TAB-TAG4             PIC X(9).
           03  R-TAB-TAG5             PIC X(7).
           03  R-TAB-TAG6             PIC X(8).
           03  R-TAB-TAG7             PIC X(30).

       01  EARNINGS-HEAD.
           03  ERN-HD1                PIC X(31).
           03  ERN-HD2                PIC X(32).
           03  ERN-HD3                PIC X(31).
           03  ERN-HD4                PIC X(31).

       01  EXPIRED-HEAD.
           03  EXP-HD1                PIC X(42).
           03  EXP-HD2                PIC X(21).
           03  EXP-HD3                PIC X(43).
           03  FILLER                 PIC X(26).

       01  HEAD6.
           03  H6-DESC1         PIC X(10).
           03  H6-RATE          PIC 99.
           03  FILLER           PIC X(10).
           03  H6-DESC2         PIC X(20).

       01  HEAD7.
           03  FILLER      PIC X(15) VALUE "TERM---RATE    ".
           03  FILLER      PIC X(15) VALUE "TERM---RATE    ".
           03  FILLER      PIC X(15) VALUE "TERM---RATE    ".
           03  FILLER      PIC X(15) VALUE "TERM---RATE    ".
           03  FILLER      PIC X(15) VALUE "TERM---RATE    ".
           03  FILLER      PIC X(15) VALUE "TERM---RATE    ".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  SCREEN-01 REDEFINES DETAIL-LINE.
           03  S1-LEFT.
               05  S1-FLD1                PIC X(17).
               05  S1-FLD2                PIC XX.
               05  S1-FLD3                PIC X(21).
               05  S1-FLD3-EXP-DATE REDEFINES S1-FLD3.
                   07  S1-3-EXPDATE       PIC 99/99/99   BLANK ZERO.
                   07  FILLER             PIC X(13).
               05  S1-FLD3-JUDG REDEFINES S1-FLD3.
                   07  S1-3-J-FRMLA       PIC X.
                   07  FILLER             PIC X.
                   07  S1-3-J-RATE        PIC ZZ9.999    BLANK ZERO.
                   07  FILLER             PIC X.
                   07  S1-3-J-YRTYPE      PIC ZZ9        BLANK ZERO.
                   07  FILLER             PIC X.
                   07  S1-3-J-INTMETH     PIC X.
                   07  FILLER             PIC X(6).
               05  S1-FLD3-CODE-A REDEFINES S1-FLD3.
                   07  S1-3-CODE-A        PIC X.
                   07  FILLER             PIC XX.
                   07  S1-3-DESC-A        PIC X(18).
               05  S1-FLD3-PAID-THRU REDEFINES S1-FLD3.
                   07  S1-3-PT            PIC ZZ9        BLANK ZERO.
                   07  FILLER             PIC X(18).
               05  S1-FLD3-CODE-B REDEFINES S1-FLD3.
                   07  S1-3-CODE-B        PIC XX.
                   07  FILLER             PIC X.
                   07  S1-3-DESC-B        PIC X(18).
               05  S1-FLD3-CODE-C REDEFINES S1-FLD3.
                   07  S1-3-CODE-C        PIC X.
                   07  FILLER             PIC XX.
                   07  S1-3-DESC-C        PIC X(15).
                   07  FILLER             PIC X.
                   07  S1-3-CODE2-C       PIC X.
                   07  FILLER             PIC X.
               05  S1-FLD3-CODE-D REDEFINES S1-FLD3.
                   07  S1-3-CODE-D        PIC 9.
                   07  FILLER             PIC X(20).
           03  S1-MID                     PIC X(26).
           03  S1-RIGHT                   PIC X(40).
           03  S1-RIGHT-A REDEFINES S1-RIGHT.
               05  S1-FLD5                PIC X(17).
               05  S1-FLD6                PIC XX.
               05  S1-FLD7                PIC X(21).
               05  S1-FLD7-LC REDEFINES S1-FLD7.
                   07  S1-7-LC-CODE       PIC X.
                   07  FILLER             PIC X.
                   07  S1-7-LC-DESC       PIC X(19).
               05  S1-FLD7-LC-RATE REDEFINES S1-FLD7.
                   07  S1-7-LC-RT         PIC Z(4).9(4)    BLANK ZERO.
                   07  FILLER             PIC X(12).
               05  S1-FLD7-LC-MIN-CHG REDEFINES S1-FLD7.
                   07  S1-7-LC-MIN        PIC Z9.99        BLANK ZERO.
                   07  FILLER             PIC X(16).
               05  S1-FLD7-LC-MAX-CHG REDEFINES S1-FLD7.
                   07  S1-7-LC-MAX        PIC Z(4).99      BLANK ZERO.
                   07  FILLER             PIC X(14).
               05  S1-FLD7-LC-GRACE REDEFINES S1-FLD7.
                   07  S1-7-LC-G-DAY      PIC Z9           BLANK ZERO.
                   07  FILLER             PIC XX.
                   07  S1-7-LC-G-CODE     PIC X.
                   07  FILLER             PIC X(16).
               05  S1-FLD7-LC-TYPE REDEFINES S1-FLD7.
                   07  S1-7-LC-TYPE       PIC ZZ9          BLANK ZERO.
                   07  FILLER             PIC X(14).
               05  S1-FLD7-LC-TYPE REDEFINES S1-FLD7.
                   07  S1-7-SC-DESC       PIC X(15).
                   07  FILLER             PIC X(06).
           03  S1-RIGHT-B REDEFINES S1-RIGHT.
               05  S1-FLD8                PIC ZZZ.ZZZ.
               05  FILLER                 PIC X.
               05  S1-FLD9                PIC Z(6).ZZ.
               05  FILLER                 PIC XX.
               05  S1-FLD10               PIC Z(3).
               05  FILLER                 PIC XXXX.
               05  S1-FLD11               PIC X.
               05  FILLER                 PIC X(9).
       01  SCREEN-02 REDEFINES DETAIL-LINE.
           03  S2-LEFT.
               05  S2-FLD1                PIC X(17).
               05  S2-FLD2                PIC XX.
               05  S2-FLD3                PIC X(21).
               05  S2-FLD3-DEF REDEFINES S2-FLD3.
                   07  S2-3-CODE          PIC XX.
                   07  FILLER             PIC X.
                   07  S2-3-DESC          PIC X(18).
               05  S2-FLD3-DEF-WORK REDEFINES S2-FLD3.
                   07  S2-3-D-WORK        PIC Z9.9999      BLANK ZERO.
                   07  FILLER             PIC X(14).
               05  S2-FLD3-DEF-MIN REDEFINES S2-FLD3.
                   07  S2-3-D-MIN         PIC Z9.99        BLANK ZERO.
                   07  FILLER             PIC X(16).
               05  S2-FLD3-DEF-MAX REDEFINES S2-FLD3.
                   07  S2-3-D-MAX         PIC ZZZ9.99      BLANK ZERO.
                   07  FILLER             PIC X(14).
               05  S2-FLD3-DEF-POL REDEFINES S2-FLD3.
                   07  S2-3-D-POL         PIC X.
                   07  FILLER             PIC XX.
                   07  S2-3-D-POL-DESC    PIC X(18).
           03  FILLER                     PIC X(21).
           03  S2-MID                     PIC X(5).
           03  S2-RIGHT                   PIC X(40).
           03  S2-RIGHT-A REDEFINES S2-RIGHT.
               05  S2-FLD5                PIC XXXX.
               05  FILLER                 PIC X.
               05  S2-FLD7                PIC X(21).
               05  FILLER                 PIC X(4).
               05  S2-FLD9                PIC X.
               05  FILLER                 PIC X(9).
           03  S2-RIGHT-B REDEFINES S2-RIGHT.
               05  S2-FLD10               PIC X(26).
               05  S2-FLD11               PIC X.
               05  FILLER                 PIC X(13).

       01  SCREEN-03 REDEFINES DETAIL-LINE.
           03  S3-LEFT                    PIC X(40).
           03  S3-LEFT-A REDEFINES S3-LEFT.
               05  S3-FLD1                PIC X(17).
               05  S3-FLD2                PIC XX.
               05  S3-FLD3                PIC X(21).
               05  S3-FLD3-A REDEFINES S3-FLD3.
                   07  S3-3-CODE-A        PIC X.
                   07  FILLER             PIC XX.
                   07  S3-3-DESC-A        PIC X(18).
               05  S3-FLD3-B REDEFINES S3-FLD3.
                   07  S3-3-CODE-B        PIC Z99        BLANK ZERO.
                   07  FILLER             PIC X(18).
               05  S3-FLD3-C REDEFINES S3-FLD3.
                   07  S3-3-CODE-C        PIC Z9         BLANK ZERO.
                   07  FILLER             PIC X.
                   07  S3-3-DESC-C        PIC X(18).
               05  S3-FLD3-CHARGE REDEFINES S3-FLD3.
                   07  S3-3-CHG           PIC Z(6).99    BLANK ZERO.
                   07  FILLER             PIC X(12).
           03  FILLER                     PIC X(26).
           03  S3-RIGHT                   PIC X(40).
           03  S3-RIGHT-A REDEFINES S3-RIGHT.
               05  S3-FLD5                PIC X(19).
               05  S3-FLD6                PIC XX.
               05  S3-FLD7                PIC X(19).
               05  S3-FLD7-A REDEFINES S3-FLD7.
                   07  S3-7-CODE-A        PIC XX.
                   07  FILLER             PIC X.
                   07  S3-7-DESC-A        PIC X(16).
               05  S3-FLD7-B REDEFINES S3-FLD7.
                   07  FILLER             PIC X(4).
                   07  S3-7-RATE          PIC Z9.99      BLANK ZERO.
                   07  FILLER             PIC X(10).
               05  S3-FLD7-C REDEFINES S3-FLD7.
                   07  FILLER             PIC XX.
                   07  S3-7-MIN           PIC Z(4).99    BLANK ZERO.
                   07  FILLER             PIC X(10).
               05  S3-FLD7-D REDEFINES S3-FLD7.
                   07  S3-7-MAX           PIC Z(6).99    BLANK ZERO.
                   07  FILLER             PIC X(10).
           03  S3-RIGHT-B REDEFINES S3-RIGHT.
               05  S3-FLD8                PIC X(9).
               05  S3-FLD9                PIC XX.
               05  S3-FLD10               PIC XXXX.
               05  S3-FLD10-CODE REDEFINES S3-FLD10.
                   07  S3-10-CODE         PIC X.
                   07  FILLER             PIC XXX.
               05  FILLER                 PIC X(4).
               05  S3-FLD12               PIC ZZZ.ZZZ.
               05  FILLER                 PIC X.
               05  S3-FLD14               PIC Z(6).ZZ.

       01  SCREEN-04 REDEFINES DETAIL-LINE.
           03  D2-TAG1           PIC X(15).
           03  D2-COLON1         PIC X.
           03  FILLER            PIC X.
           03  DET2-TYPE.
               05  D2-TYPE1      PIC XX.
               05  D2-FRMLA1 REDEFINES D2-TYPE1 PIC 99.
               05  D2-DESC1      PIC X(20).
           03  FILLER            PIC X(28).
           03  D2-TAG2           PIC X(15).
           03  D2-COLON2         PIC X.
           03  FILLER            PIC X.
           03  DET2-TYP.
               05  D2-TYPE2      PIC X(2).
               05  D2-FRMLA2 REDEFINES D2-TYPE2
                              PIC 99.
               05  D2-DESC2      PIC X(20).
           03  FILLER            PIC X(26).

       01  HEADSUM REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(4).
           03  D-STATE          PIC XX.
           03  FILLER           PIC X(9).
           03  D-LNCLASS        PIC 99.
           03  FILLER           PIC X(13).
           03  D-SUBCLASS       PIC 99.
           03  FILLER           PIC X(8).
           03  D-SCDESC         PIC X(21).
           03  FILLER           PIC X(8).
           03  D-LAWCODE        PIC XX.
           03  FILLER           PIC X(9).
           03  D-EFFDATE        PIC 99/99/99.
           03  FILLER           PIC X(6).
           03  D-EXPDATE        PIC 99/99/99 BLANK ZERO.

       01  TAB-HEAD REDEFINES DETAIL-LINE.
           03  D-TABHEAD1       PIC X(27).
           03  D-TABHEAD2       PIC X(40).
           03  D-TABHEAD3       PIC X(21).
           03  D-TABHEAD4       PIC X(40).
           03  FILLER           PIC X(4).
       01  EDT-TBL REDEFINES DETAIL-LINE.
           03  EDIT1.
               05  DESC-CODE1       PIC X(10).
               05  DESC-CODE2       PIC X(5).
               05  DESC-CODE3       PIC X(10).
               05  FILLER           PIC XX.
           03  EDIT-CODE1 REDEFINES EDIT1.
               05  EDIT-NUM1       PIC 9.
               05  EDIT-PERIOD1    PIC XX.
               05  CODE1           PIC X.
               05  CODE2           PIC X.
               05  CODE3           PIC XX.
               05  CODE4           PIC X.
               05  FILLER          PIC X(19).
           03  RATE-TYP1        PIC X(6).
           03  RATE1            PIC ZZ.ZZ.
           03  FILLER           PIC XX.
           03  AMT-TYP1         PIC X(6).
           03  AMT1             PIC Z(6).ZZ.
           03  FILLER           PIC X(3).
           03  TERM1            PIC ZZZ.Z.
           03  FILLER           PIC X(4).

           03  EDIT2.
               05  DESC-CD1        PIC X(10).
               05  DESC-CD2        PIC X(5).
               05  DESC-CD3        PIC X(10).
               05  FILLER          PIC X.
           03  EDIT-CODE2 REDEFINES EDIT2.
               05  EDIT-NUM2           PIC 9.
               05  EDIT-PERIOD2        PIC XX.
               05  CD1                 PIC X.
               05  CD2                 PIC X.
               05  CD3                 PIC XX.
               05  CD4                 PIC X.
               05  FILLER              PIC X(13).
           03  RATE-TYP2        PIC X(6).
           03  RATE2            PIC ZZ.ZZ.
           03  FILLER           PIC XX.
           03  AMT-TYP2         PIC X(6).
           03  AMT2             PIC Z(6).ZZ.
           03  FILLER           PIC X(3).
           03  TERM2            PIC ZZZ.Z.
           03  FILLER           PIC X(14).

       01  D-LINE6 REDEFINES DETAIL-LINE.
           03  EARN-TAG-A       PIC X(19).
           03  EARN-COL-A       PIC X.
           03  FILLER           PIC X.
           03  EARN-CODE-A      PIC X(8).
           03  E-CODE-FRMLA-A REDEFINES EARN-CODE-A.
               05  E-CD-FRMLA-A PIC Z9                BLANK ZERO.
               05  FILLER       PIC X(6).
           03  E-CODE-AMT-A REDEFINES EARN-CODE-A PIC Z(5).ZZ.
           03  E-CODE-A REDEFINES EARN-CODE-A.
               05  E-CD-A       PIC X.
               05  FILLER       PIC X.
               05  E-PERCT-A    PIC Z(3).99           BLANK ZERO.
           03  E-CODES-A REDEFINES EARN-CODE-A.
               05  E-CDS-A      PIC X.
               05  FILLER       PIC X(7).
           03  E-CODES-A1 REDEFINES EARN-CODE-A.
               05  E-CDS-A1     PIC ZZZ.
               05  FILLER       PIC X(5).
           03  E-CODES-A3 REDEFINES EARN-CODE-A.
               05  E-PERCT-A3-1 PIC ZZ9.9.
               05  FILLER       PIC X(3).
           03  E-CODES-A4 REDEFINES EARN-CODE-A.
               05 E-DAYS-A4-1  PIC ZZ9.
               05 FILLER       PIC X(5).
           03  E-CODES-A5 REDEFINES EARN-CODE-A.
               05 E-AGE-A14    PIC ZZ9.
               05 FILLER       PIC X(2).
               05 E-DAY-A14    PIC ZZ9.
           03  FILLER           PIC X(2).
           03  EARN-TAG-B       PIC X(20).
           03  EARN-COL-B       PIC X.
           03  FILLER           PIC X.
           03  EARN-CODE-B      PIC X(8).
           03  E-CODE-FRMLA-B REDEFINES EARN-CODE-B.
               05  E-CD-FRMLA-B PIC Z9               BLANK ZERO.
               05  FILLER       PIC X(6).
           03  E-CODE-AMT-B REDEFINES EARN-CODE-B PIC Z(5).ZZ.
           03  E-CODE-B REDEFINES EARN-CODE-B.
               05  E-CD-B       PIC X.
               05  FILLER       PIC X.
               05  E-PERCT-B    PIC Z(3).99         BLANK ZERO.
           03  E-CODES-B REDEFINES EARN-CODE-B.
               05  E-CDS-B      PIC X.
               05  FILLER       PIC X(7).
           03  E-CODES-BA  REDEFINES EARN-CODE-B.            
               05  E-CDS-BA     PIC ZZZZZZ9.
               05  FILLER       PIC X.
           03  E-CODES-B1 REDEFINES EARN-CODE-B.
               05  E-CDS-B1     PIC ZZZ.
               05  FILLER       PIC X(5).
           03  E-CODES-B2 REDEFINES EARN-CODE-B.
               05  E-CDS-B2-1   PIC X(2).
               05  FILLER       PIC X(2).
               05  E-CDS-B2-2   PIC X(2).
               05  FILLER       PIC X(2).
           03  FILLER           PIC X(2).
           03  EARN-TAG-C       PIC X(19).
           03  EARN-COL-C       PIC X.
           03  FILLER           PIC X.
           03  EARN-CODE-C      PIC X(8).
           03  E-CODE-FRMLA-C REDEFINES EARN-CODE-C.
               05  E-CD-FRMLA-C PIC Z9               BLANK ZERO.
               05  FILLER       PIC X(6).
           03  E-CODE-AMT-C REDEFINES EARN-CODE-C PIC Z(5).ZZ.
           03  E-CODE-C REDEFINES EARN-CODE-C.
               05  E-CD-C       PIC X.
               05  FILLER       PIC X.
               05  E-PERCT-C    PIC Z(3).99         BLANK ZERO.
           03  E-CODES-C REDEFINES EARN-CODE-C.
               05  E-CDS-C      PIC X.
               05  FILLER       PIC X(7).
           03  E-CODES-C1 REDEFINES EARN-CODE-C.
               05  E-CDS-C1     PIC ZZZ.
               05  FILLER       PIC X(5).
           03  FILLER           PIC X(2).
           03  EARN-TAG-D       PIC X(19).
           03  EARN-COL-D       PIC X.
           03  FILLER           PIC X.
           03  EARN-CODE-D      PIC X(8).
           03  E-CODE-FRMLA-D REDEFINES EARN-CODE-D.
               05  E-CD-FRMLA-D PIC Z9               BLANK ZERO.
               05  FILLER       PIC X(6).
           03  E-CODE-AMT-D REDEFINES EARN-CODE-D PIC Z(5).ZZ.
           03  E-CODE-D REDEFINES EARN-CODE-D.
               05  E-CD-D       PIC X.
               05  FILLER       PIC X.
               05  E-PERCT-D    PIC Z(3).99         BLANK ZERO.
           03  E-CODES-D REDEFINES EARN-CODE-D.
               05  E-CDS-D      PIC X.
               05  FILLER       PIC X(7).
           03  E-CODES-D1 REDEFINES EARN-CODE-D.
               05  E-CDS-D1     PIC ZZZ.
               05  FILLER       PIC X(5).
           03  E-CODES-D3 REDEFINES EARN-CODE-D.
               05  E-ADD-FG-1   PIC X.
               05  FILLER       PIC X.
               05  E-ADD-FG-2   PIC X(6).

       01  D-LINE7 REDEFINES DETAIL-LINE.
           03  P20-COL-1        PIC X(21).
           03  P20-COL-2        PIC X.
           03  FILLER           PIC X.
           03  MIN-CR-SCORE     PIC 9(4).
           03  SING-PY-FLAG-A REDEFINES MIN-CR-SCORE.
               05  E-PY-FLAG    PIC X.
               05  FILLER       PIC X(3).
           03  WAGE-ASSIGN-A  REDEFINES MIN-CR-SCORE.
               05  WAGE-ASSIGN  PIC 99.
               05  FILLER       PIC X(2).
           03  FILLER           PIC X(105).

      *01  SCREEN-21-REVOL REDEFINES DETAIL-LINE.
      *    03  S21-LEFT                         PIC X(35).
      *    03  S21-LEFT-A REDEFINES S21-LEFT.
      *        05  S21-LEFT-FLD1-PROGRAM-CD     PIC X(2).
      *        05  S21-LEFT-FLD2-DESC           PIC X(30).
      *        05  FILLER                       PIC X(3).

      *    03  S21-MID                          PIC X(31).
      *    03  S21-MID-A REDEFINES S21-MID.
      *        05  S21-MID-FLD3-DAY1            PIC X(6).
      *        05  S21-MID-DAY1-YN              PIC X.
      *        05  FILLER                       PIC X(2).

      *        05  S21-MID-FLD3-DAY2            PIC X(6).
      *        05  S21-MID-DAY2-YN              PIC X.
      *        05  FILLER                       PIC X(2).

      *        05  S21-MID-FLD3-DAY3            PIC X(6).
      *        05  S21-MID-DAY3-YN              PIC X.
      *        05  FILLER                       PIC X(6).

      *    03  S21-RIGHT                        PIC X(17).
      *    03  S21-RIGHT-A REDEFINES S21-RIGHT.
      *        05  S21-RIGHT-FLD4-DESC          PIC X(15).
      *        05  S21-RIGHT-FLD4-APPLY-ORDER   PIC 99.
      *    03  FILLER                           PIC X(49).

      *01  SCREEN-22-REVOL REDEFINES DETAIL-LINE.
      *    03  S22-LEFT                         PIC X(40).
      *    03  S22-LEFT-A REDEFINES S22-LEFT.
      *        05  S22-LEFT-FLD1                PIC X(17).
      *        05  S22-LEFT-FLD2                PIC XX.
      *        05  S22-LEFT-FLD3                PIC X(21).
      *        05  S22-FLD3-PYMT-DUE-FRMLA REDEFINES S22-LEFT-FLD3.
      *            07  S22-L-PYMT-DUE-FRMLA     PIC X.
      *            07  FILLER                   PIC X(20).
      *        05  S22-FLD3-PYMT-DUE-WORKER REDEFINES S22-LEFT-FLD3.
      *            07  S22-L-PYMT-DUE-WORKER    PIC 9(2).
      *            07  FILLER                   PIC X(19).
      *        05  S22-F1D3-PYMT-DUE-PERCENT REDEFINES S22-LEFT-FLD3.
      *            07  S22-L-PYMT-DUE-PERCENT   PIC ZZ9.999.
      *            07  FILLER                   PIC X(14).
      *        05  S22-FLD3-MIN-PYMT-DUE     REDEFINES S22-LEFT-FLD3.
      *            07  S22-L-PYMT-MINIMUM       PIC ZZ9.999.
      *            07  FILLER                   PIC X(14).
      *        05  S22-FLD3-MAX-PYMT-DUE    REDEFINES S22-LEFT-FLD3.
      *            07  S22-L-PYMT-MAXIMUM       PIC ZZ9.999.
      *            07  FILLER                   PIC X(14).
      *        05  S22-FLD3-PYMT-ROUND-CODE REDEFINES S22-LEFT-FLD3.
      *            07  S22-L-PYMT-ROUND-CODE    PIC X.
      *            07  FILLER                   PIC X(20).
      *        05  S22-FLD3-PYMT-EXCEPTION-CD REDEFINES S22-LEFT-FLD3.
      *            07  S22-L-PYMT-EXCEPTION-CD  PIC X.
      *            07  FILLER                   PIC X(20).
      *        05  S22-FLD3-MEMSHIP-FRMLA      REDEFINES S22-LEFT-FLD3.
      *            07  S22-L-MEMSHIP-FRMLA      PIC X.
      *            07  FILLER                   PIC X(20).
      *        05  S22-FLD3-MEMSHIP-FEE        REDEFINES S22-LEFT-FLD3.
      *            07  S22-L-MEMSHIP-FEE        PIC ZZZ9.99.
      *            07  FILLER                   PIC X(14).

      *    03  S22-MID                          PIC X(26).

      *    03  S22-RIGHT                        PIC X(47).
      *    03  S22-RIGHT-A REDEFINES S22-RIGHT.
      *        05  S22-RIGHT-FLD1               PIC X(24).
      *        05  S22-RIGHT-FLD2               PIC XX.
      *        05  S22-RIGHT-FLD3               PIC X(21).
      *        05  S22-FLD3-BALANCE-FRMLA REDEFINES S22-RIGHT-FLD3.
      *            07  S22-R-BALANCE-FRMLA      PIC X.
      *            07  FILLER                   PIC X(20).
      *        05  S22-FLD3-CYCLE-DATE-FRMLA REDEFINES S22-RIGHT-FLD3.
      *            07 S22-R-CYCLE-DATE-FRMLA    PIC X.
      *            07 FILLER                    PIC X(20).
      *        05  S22-FLD3-APPLY-PURCHASE   REDEFINES S22-RIGHT-FLD3.
      *            07  S22-R-APPLY-PURCHASE     PIC X.
      *            07  FILLER                   PIC X(20).
      *        05  S22-FLD3-PRINT-STMT       REDEFINES S22-RIGHT-FLD3.
      *            07  S22-R-PRINT-STMT         PIC 99.
      *            07  FILLER                   PIC X(19).
      *        05  S22-FLD3-STMT-FORMAT      REDEFINES S22-RIGHT-FLD3.
      *            07  S22-R-STMT-FORMAT        PIC X(08).
      *            07  FILLER                   PIC X(12).
      *        05  S22-FLD3-STMT-EXTRACT     REDEFINES S22-RIGHT-FLD3.
      *            07  S22-R-STMT-EXTRACT       PIC X.
      *            07  FILLER                   PIC X(19).

       01  SCREEN-23 REDEFINES DETAIL-LINE.
           03  S23-LEFT                         PIC X(40).
           03  S23-LEFT-A REDEFINES S23-LEFT.
               05  S23-LEFT-FLD1                PIC X(17).
               05  S23-LEFT-FLD2                PIC XX.
               05  S23-TRCD                     PIC XX.
               05  FILLER                       PIC X.
               05  S23-FMNO                     PIC ZZ9.
               05  FILLER                       PIC X(15).
           03  FILLER                           PIC X(26).
           03  S23-RIGHT                        PIC X(47).
           03  S23-RIGHT-A REDEFINES S23-RIGHT.
               05  S23-RIGHT-FLD1               PIC X(24).
               05  S23-RIGHT-FLD2               PIC XX.
               05  S23-RIGHT-FLD3               PIC X(21).
               05  S23-FLD3-BALANCE-FRMLA REDEFINES S23-RIGHT-FLD3.
                   07  S23-R-PERCENT            PIC Z9.
                   07  FILLER                   PIC X(19).


       01  CAL-TAB REDEFINES DETAIL-LINE.
           03  TAB-NUM1         PIC 9.
           03  CAL-PERIOD1      PIC XX.
           03  CAL-RATE1        PIC ZZZ.ZZZZ.
           03  FILLER           PIC X(1).
           03  CAL-AMT1         PIC Z(6).ZZ.
           03  FILLER           PIC X(46).
           03  TAB-NUM2         PIC 9.
           03  CAL-PERIOD2      PIC XX.
           03  CAL-RATE2        PIC ZZZ.ZZZZ.
           03  FILLER           PIC X(1).
           03  CAL-AMT2         PIC Z(6).ZZ.
           03  FILLER           PIC X(36).

       01  D-LINE3 REDEFINES DETAIL-LINE.
           03  INSCO-TAG       PIC X(15).
           03  I-COLON         PIC X.
           03  FILLER          PIC X.
           03  W-CODE.
               05  I-CHG       PIC XXX.
               05  I-RTE REDEFINES I-CHG
                               PIC 999.
               05  I-CHG-DESC  PIC X(19).
               05  I-REJECT REDEFINES I-CHG-DESC.
                   07  FILLER      PIC X(5).
                   07  I-REJCT-DESC PIC X(9).
                   07  I-REJCT-CDE PIC X.
                   07  FILLER      PIC X(4).
           03  I-CHGABLE REDEFINES W-CODE.
               05  I-DEVIAT   PIC Z(4).9(2)          BLANK ZERO.
               05  I-EX REDEFINES I-DEVIAT
                              PIC Z9                 BLANK ZERO.
               05  I-INSRATE REDEFINES I-DEVIAT
                              PIC ZZ.9(4)            BLANK ZERO.
               05  INS-TM REDEFINES I-DEVIAT
                             PIC ZZZ9                BLANK ZERO.
               05  I-MIN-MAX REDEFINES I-DEVIAT
                             PIC Z(6).
               05  FILLER     PIC X(15).
           03  FILLER         PIC X(1).
           03  I-CHG-FG       PIC X.
           03  FILLER         PIC X(1).
           03  INS-TAB1.
               05  FILLER       PIC X(4).
               05  INS-MINAGE   PIC ZZ.
               05  FILLER       PIC X(3).
               05  INS-MAXAGE   PIC ZZ.
               05  FILLER       PIC XX.
               05  INS-MAX-COV    PIC Z(6).
               05  FILLER         PIC X.
               05  INS-MAX-TRM    PIC Z(4).
               05  FILLER         PIC X(10).
           03  HEAD2-INS REDEFINES INS-TAB1.
               05  FILLER         PIC X(3).
               05  I-HEAD2        PIC X(31).
           03  INS-RATE-TBL.
               05  FILLER               PIC X(4).
               05  INS-DESCR            PIC X(6).
               05  INS-NO               PIC X(3).
               05  TERM                 PIC ZZZZ.
               05  FILLER               PIC XX.
               05  RATE-INS             PIC ZZ.ZZZZ.
               05  FILLER               PIC X(30).

       01  D-LINE4 REDEFINES DETAIL-LINE.
           03  REB-TAB-NUM                PIC 99.
           03  REB-PERIOD                 PIC XX.
           03  REB-DESC                   PIC X(11).
           03  FILLER                     PIC XX.
           03  REB-FRMLA                  PIC XX.
           03  FILLER                     PIC X(5).
           03  REB-FRMLA2                 PIC XX.
           03  FILLER                     PIC X(3).
           03  REB-MIN-RET                PIC ZZZZ.ZZ.
           03  FILLER                     PIC X(4).
           03  REB-MIN-MON                PIC ZZ.
           03  FILLER                     PIC XX.
           03  REB-DED-CHG                PIC ZZZZ.ZZ.
           03  FILLER                     PIC X(5).
           03  REB-SPOPT1                 PIC ZZ.
           03  FILLER                     PIC X(6).
           03  REB-EARLY                  PIC ZZ.
           03  FILLER                     PIC X(6).
           03  REB-DEFER                  PIC ZZ.
           03  FILLER                     PIC X.
           03  FILLER                     PIC X(3).
           03  REB-PCT                    PIC ZZZ.Z.
           03  FILLER                     PIC XX.
           03  REB-REDUCT                 PIC ZZZZ.ZZ.
           03  FILLER                     PIC XX.
           03  REB-MIN                    PIC ZZZZ.ZZ.

       01  D-LINE5 REDEFINES DETAIL-LINE.
           03  RB-NUM               PIC ZZ.
           03  RB-PERIOD            PIC XX.
           03  RB-DESCRIPT          PIC X(11).
           03  FILLER               PIC X(4).
           03  RB-ADDAYS1           PIC ZZ.
           03  FILLER               PIC X(3).
           03  RB-REFDATE1          PIC XX.
           03  FILLER               PIC X(2).
           03  RB-YEARTYPE1         PIC ZZZZ.
           03  FILLER               PIC X(6).
           03  RB-ADDAYS2           PIC ZZ.
           03  FILLER               PIC X(3).
           03  RB-REFDATE2          PIC XX.
           03  FILLER               PIC X(2).
           03  RB-YEARTYPE2         PIC ZZZZ.
           03  FILLER               PIC X(10).
           03  RB-RECAST            PIC 99 BLANK ZERO.
           03  FILLER               PIC X(6).
           03  RB-OPT2              PIC 99 BLANK ZERO.
           03  FILLER               PIC X(5).
           03  RB-MANAGER           PIC 99 BLANK ZERO.
           03  FILLER               PIC X(4).
           03  RB-UNITPERCD         PIC X.
           03  FILLER               PIC X(49).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER               PIC X(50).
           03  RANGE-SELECTION.
               04  R-SP-ORGST       PIC XX.
               04  FILLER           PIC XX.
               04  R-SP-SPRCLASS    PIC 99.
               04  FILLER           PIC XX.
               04  R-SP-SUBCLASS    PIC 99.
               04  FILLER           PIC XX.
               04  R-SP-LAWCODE     PIC 99.
               04  FILLER           PIC XX.
               04  FILLER           PIC X(65).

       01  D-LINE REDEFINES DETAIL-LINE.
           03  D-NUM-1         PIC ZZ9             BLANK ZERO.
           03  FILLER          PIC XX.
           03  D-RATE-1        PIC Z9.999          BLANK ZERO.
           03  FILLER          PIC XXXX.
           03  D-NUM-2         PIC ZZ9             BLANK ZERO.
           03  FILLER          PIC XX.
           03  D-RATE-2        PIC Z9.999          BLANK ZERO.
           03  FILLER          PIC XXXX.
           03  D-NUM-3         PIC ZZ9             BLANK ZERO.
           03  FILLER          PIC XX.
           03  D-RATE-3        PIC Z9.999          BLANK ZERO.
           03  FILLER          PIC XXXX.
           03  D-NUM-4         PIC ZZ9             BLANK ZERO.
           03  FILLER          PIC XX.
           03  D-RATE-4        PIC Z9.999          BLANK ZERO.
           03  FILLER          PIC XXXX.
           03  D-NUM-5         PIC ZZ9             BLANK ZERO.
           03  FILLER          PIC XX.
           03  D-RATE-5        PIC Z9.999          BLANK ZERO.
           03  FILLER          PIC XXXX.
           03  D-NUM-6         PIC ZZ9             BLANK ZERO.
           03  FILLER          PIC XX.
           03  D-RATE-6        PIC Z9.999          BLANK ZERO.
           03  FILLER          PIC XXXX.

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  SPLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/SPLIST_DEF.CPY".
       COPY "LIBLS/SPLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/SPLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-IR-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPLIST" TO VDU-FORM-NAME.
           MOVE "SPLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS SPLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.
       NEXT-SP.
           PERFORM READ-SP1-FILE-NEXT.
           IF IO-FG = 9 OR SP-ORGST > END-SP-ORGST
               GO TO END-ROUTINE.

           IF SP-SPRCLASS < BEG-SP-SPRCLASS OR
              SP-SPRCLASS > END-SP-SPRCLASS
               GO TO NEXT-SP.

           IF SP-SUBCLASS < BEG-SP-SUBCLASS OR
              SP-SUBCLASS > END-SP-SUBCLASS
               GO TO NEXT-SP.

           IF SP-LAWCODE < BEG-SP-LAWCODE OR
              SP-LAWCODE > END-SP-LAWCODE
               GO TO NEXT-SP.

           IF NON-EXPIRED-ONLY = "Y"
              IF SP-EXPDATE NOT = 0
                 GO TO NEXT-SP.

           IF SUM-ONLY = "Y"
              PERFORM SUMMARY-PROCESS.

           IF SUM-ONLY = "N"
              PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-SP.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.
           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE " " TO ERRCD.
       SETRP.
           MOVE SPLIST-QUEUE-POS TO Q-POS.
           MOVE SPLIST-COPIES-POS TO C-POS.
           MOVE SPLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00" GO TO ENDIT.
           MOVE " " TO ERRCD.
       ST-01.
           MOVE "E  A020ST1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-ST.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ST-01.
           MOVE VDUBUF TO BEG-SP-ORGST.
       ST-02.
           MOVE "E  A020ST2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO ST-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ST-02.
           MOVE VDUBUF TO END-SP-ORGST.
           IF BEG-SP-ORGST > END-SP-ORGST
               MOVE "INVALID STATE CODE RANGE !" TO MESS
               PERFORM SEND-MESS
               GO TO ST-01.
           GO TO LC-01.
       ALL-ST.
           MOVE "S  A020ST1." TO VDU.
           MOVE SPACES TO BEG-SP-ORGST VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST2." TO VDU.
           MOVE "ZZ" TO END-SP-ORGST VDUBUF.
           PERFORM SCREENIO.
       LC-01.
           MOVE "ENNN020LC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-LC.
           IF VDUSTATUS = "1U" GO TO ST-02.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO LC-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO LC-01.
           MOVE VDUNUM0 TO BEG-SP-SPRCLASS.
       LC-02.
           MOVE "ENNN020LC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO LC-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO LC-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO LC-02.
           MOVE VDUNUM0 TO END-SP-SPRCLASS.
           IF BEG-SP-SPRCLASS > END-SP-SPRCLASS
               MOVE "INVALID LOAN CLASS RANGE !" TO MESS
               PERFORM SEND-MESS
               GO TO LC-01.
           GO TO LSC-01.
       ALL-LC.
           MOVE "S  N020LC1." TO VDU.
           MOVE 0 TO BEG-SP-SPRCLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  N020LC2." TO VDU.
           MOVE 99 TO VDUNUM0.
           MOVE 999 TO END-SP-SPRCLASS.
           PERFORM SCREENIO.
       LSC-01.
           MOVE "ENNN020LSC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-LSC.
           IF VDUSTATUS = "1U" GO TO LC-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO LSC-01.
           MOVE VDUNUM0 TO BEG-SP-SUBCLASS.
       LSC-02.
           MOVE "ENNN020LSC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO LSC-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO LSC-02.
           MOVE VDUNUM0 TO END-SP-SUBCLASS.
           IF BEG-SP-SUBCLASS > END-SP-SUBCLASS
               MOVE "INVALID LOAN SUB-CLASS RANGE !" TO MESS
               PERFORM SEND-MESS
               GO TO LSC-01.
           GO TO SP-01.
       ALL-LSC.
           MOVE "S  N020LSC1." TO VDU.
           MOVE 0 TO BEG-SP-SUBCLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  N020LSC2." TO VDU.
           MOVE 99 TO END-SP-SUBCLASS VDUNUM0.
           PERFORM SCREENIO.
       SP-01.
           MOVE "ENNN020SP1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-SP.
           IF VDUSTATUS = "1U" GO TO LSC-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SP-01.
           MOVE VDUNUM0 TO BEG-SP-LAWCODE.
       SP-02.
           MOVE "ENNN020SP2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO SP-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SP-02.
           MOVE VDUNUM0 TO END-SP-LAWCODE.
           IF BEG-SP-LAWCODE > END-SP-LAWCODE
               MOVE "INVALID LAW CODE RANGE !" TO MESS
               PERFORM SEND-MESS
               GO TO SP-01.

           GO TO SUMYN.
       ALL-SP.
           MOVE "S  N020SP1." TO VDU.
           MOVE 0 TO BEG-SP-LAWCODE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  N020SP2." TO VDU.
           MOVE 99 TO END-SP-LAWCODE VDUNUM0.
           PERFORM SCREENIO.
       SUMYN.
           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO SP-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SUMYN.
           MOVE VDUBUF TO SUM-ONLY.
           IF SUM-ONLY NOT = "N" AND NOT = "Y" GO TO SUMYN.

       NON-EXP-YN.
           MOVE "E  A010NONEXP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO SUMYN.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO NON-EXP-YN.
           MOVE VDUBUF TO NON-EXPIRED-ONLY.
           IF NON-EXPIRED-ONLY NOT = "N" AND NOT = "Y" GO TO NON-EXP-YN.

       SPR-PAGE-NO.
           MOVE "EN N020PAGENO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO NON-EXP-YN.
           IF VDUSTATUS = "16"
              MOVE "S  N020PAGENO." TO VDU
              MOVE 99 TO SPR-PAGE-NUMBER VDUNUM0
              PERFORM SCREENIO
              GO TO OK-YN.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SPR-PAGE-NO.
           MOVE VDUNUM0 TO SPR-PAGE-NUMBER.

       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO SPR-PAGE-NO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N" GO TO ST-01.
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.

      D    STOP MESS.
           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-FD1-FILE.

           MOVE BEG-SP1-KEY TO SP1-KEY.

           MOVE SP-ORGST             TO QSP1-WBEG-ORGST.
           MOVE END-SP-ORGST         TO QSP1-WEND-ORGST.
           MOVE SP-SPRCLASS          TO QSP1-WBEG-SPRCLASS.
           MOVE END-SP-SPRCLASS      TO QSP1-WEND-SPRCLASS.
           MOVE SP-SUBCLASS          TO QSP1-WBEG-SUBCLASS.
           MOVE END-SP-SUBCLASS      TO QSP1-WEND-SUBCLASS.
           MOVE SP-LAWCODE           TO QSP1-WBEG-LAWCODE.
           MOVE END-SP-LAWCODE       TO QSP1-WEND-LAWCODE.

           PERFORM START-SP1-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           IF ( SUM-ONLY = "Y" )
              MOVE 99 TO LN-COUNT.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************
       SUMMARY-PROCESS SECTION.
      ****************************
           MOVE SP-ORGST    TO D-STATE.
           MOVE SP-SPRCLASS TO D-LNCLASS.
           MOVE SP-SUBCLASS TO D-SUBCLASS.
           MOVE SP-DESC     TO D-SCDESC.
           MOVE SP-LAWCODE  TO D-LAWCODE.
           MOVE SP-EFFDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-EFFDATE.
           MOVE SP-EXPDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-EXPDATE.
           MOVE HEADSUM     TO DETAIL-LINE.
           PERFORM WRITE-SUMMARY-LINE.
       END-SUM-PROCESS.
           EXIT.

      ****************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************
           PERFORM HEAD-PAGE.
           MOVE SPACES TO HEAD3.
           MOVE 2 TO LN-FEED.
           MOVE "STATE CODE" TO H-TAG.
           MOVE ":" TO H-COLON.
           MOVE SP-ORGST TO H-CODE.
           MOVE "EFFECTIVE DATE" TO H-TAG2.
           MOVE ":" TO H-COLON2.
           MOVE SP-EFFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-EFFDATE.
           MOVE HEAD3 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PRINT-LC.

           MOVE SPACES TO HEAD3.
           MOVE "LAW  CODE" TO H-TAG.
           MOVE ":" TO H-COLON.
           MOVE SP-LAWCODE TO H-CLASS.
           MOVE HEAD3 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *                       PAGE 1
      ******************************************************************
           IF SPR-PAGE-NUMBER NOT = 1 AND NOT = 99
              GO TO PRINT-PAGE-2.

           MOVE 2 TO LN-FEED.
           MOVE "PAGE 1" TO H-PG1.
           PERFORM PRINT-HEAD4.

           MOVE "EXPIRATION DATE  " TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-EXPDATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO S1-3-EXPDATE.
           MOVE "LATE CHARGE FRMLA" TO S1-FLD5.
           MOVE ": "                TO S1-FLD6.
           MOVE SP-LCFRMLA          TO S1-7-LC-CODE.
           IF SP-LCFRMLA = " "
              MOVE " NOT APPLICABLE" TO S1-7-LC-DESC
           ELSE IF SP-LCFRMLA = "A"
              MOVE " PENN. LATE CHG " TO S1-7-LC-DESC
           ELSE IF SP-LCFRMLA = "B"
              MOVE " " TO S1-7-LC-DESC
           ELSE IF SP-LCFRMLA = "C"
              MOVE " PENN. SERV CHG" TO S1-7-LC-DESC
           ELSE IF SP-LCFRMLA = "D"
              MOVE " " TO S1-7-LC-DESC
           ELSE IF SP-LCFRMLA = "E"
              MOVE " " TO S1-7-LC-DESC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "JUDGEMENT CODES  " TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-JUDGFRMLA        TO S1-3-J-FRMLA.
           MOVE SP-JUDGRATE         TO S1-3-J-RATE.
           MOVE SP-JUDGYRTYPE       TO S1-3-J-YRTYPE.
           MOVE SP-JUDGINTMETH      TO S1-3-J-INTMETH.
           MOVE "             CODE" TO S1-FLD5.
           MOVE ": "                TO S1-FLD6.
           MOVE SP-LCTYPE           TO S1-7-LC-CODE.
           IF SP-LCTYPE = " "
              MOVE "NOT APPLICABLE" TO S1-7-LC-DESC
           ELSE IF SP-LCTYPE = "A"
              MOVE "FIXED% OF NORM PYMT" TO S1-7-LC-DESC
           ELSE IF SP-LCTYPE = "B"
              MOVE "FLAT CG FOR EACH LC" TO S1-7-LC-DESC
           ELSE IF SP-LCTYPE = "C"
              MOVE "FIXED% OF DELIN AMT" TO S1-7-LC-DESC
           ELSE IF SP-LCTYPE = "D"
              MOVE "FIXED MO RATE" TO S1-7-LC-DESC
           ELSE IF SP-LCTYPE = "E"
              MOVE "FIXED% NRM PMT TRUNC"  TO S1-7-LC-DESC
           ELSE IF SP-LCTYPE = "F"
              MOVE "FIXED% NRM PMT ROUND" TO S1-7-LC-DESC
           ELSE IF SP-LCTYPE = "G"
              MOVE "FIXED% CURRENT PMT" TO S1-7-LC-DESC
           ELSE IF SP-LCTYPE = "H"
              MOVE "FIXED% CUR PMT TRUNC" TO S1-7-LC-DESC
           ELSE IF SP-LCTYPE = "I"
              MOVE "FIXED% CUR PMT ROUND" TO S1-7-LC-DESC
           ELSE IF SP-LCTYPE = "J"
              MOVE "FIXED% DELIN (1.00)" TO S1-7-LC-DESC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "1098 REPORT FRMLA" TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-1098FRMLA        TO S1-3-CODE-C.
           IF SP-1098FRMLA = " "
              MOVE "ALL INTEREST PD" TO S1-3-DESC-C
           ELSE IF SP-1098FRMLA = "A"
              MOVE "PRORATA PRINBAL" TO S1-3-DESC-C
           ELSE IF SP-1098FRMLA = "B"
              MOVE "UNEARNED METHOD" TO S1-3-DESC-C
           ELSE IF SP-1098FRMLA = "C"
              MOVE "ACTUARIAL METHD" TO S1-3-DESC-C
           ELSE IF SP-1098FRMLA = "D"
              MOVE "RULE OF 78THS" TO S1-3-DESC-C.
           MOVE SP-1098-INC-LC TO S1-3-CODE2-C.
           MOVE "             RATE" TO S1-FLD5.
           MOVE ": "                TO S1-FLD6.
           MOVE SP-LCRATE           TO S1-7-LC-RT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DISCLOSURE  FRMLA" TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-DISFRMLA         TO S1-3-CODE-A.
           IF SP-DISFRMLA = "A"
              MOVE " ACTUARIAL METHOD" TO S1-3-DESC-A
           ELSE
              IF SP-DISFRMLA = "B"
                 MOVE " SALES FINANCE" TO S1-3-DESC-A
              ELSE
                 IF SP-DISFRMLA = "C"
                    MOVE " U.S. RULE" TO S1-3-DESC-A.
           MOVE "       MIN CHARGE" TO S1-FLD5.
           MOVE ": "                TO S1-FLD6.
           MOVE SP-LCMINCHG         TO S1-7-LC-MIN.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CONTRACTUAL FRMLA" TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-CONTRFRMLA       TO S1-3-CODE-A.
           IF SP-CONTRFRMLA = " "
              MOVE " PRINCIPAL ONLY " TO S1-3-DESC-A
           ELSE IF SP-CONTRFRMLA = "A"
              MOVE " TOTAL MONIES" TO S1-3-DESC-A.
           MOVE "       MAX CHARGE" TO S1-FLD5.
           MOVE ": "                TO S1-FLD6.
           MOVE SP-LCMAXAMT         TO S1-7-LC-MAX.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "        BANK RULE" TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-BANKRULE         TO S1-3-CODE-A.
           IF SP-BANKRULE = " "
              MOVE " 30DAYS AFTER 2 MONTH" TO S1-3-DESC-A
           ELSE IF SP-BANKRULE = "A"
              MOVE " 30DAYS 1ST OF 2ND MO" TO S1-3-DESC-A
           ELSE IF SP-BANKRULE = "B"
              MOVE " 30DAYS AFTER 1 MONTH" TO S1-3-DESC-A
           ELSE IF SP-BANKRULE = "C"
              MOVE " 30DAYS 1ST OF MO FOL" TO S1-3-DESC-A
           ELSE IF SP-BANKRULE = "D"
              MOVE " 30DAYS WORLD 1ST FOL" TO S1-3-DESC-A
           ELSE IF SP-BANKRULE = "F"
              MOVE "WORLD SEMI-MONTHLY"    TO S1-3-DESC-A.
           MOVE "    GRACE DAYS/CD" TO S1-FLD5.
           MOVE ": "                TO S1-FLD6.
           MOVE SP-LCGRACE          TO S1-7-LC-G-DAY.
           MOVE SP-LCGRACECD        TO S1-7-LC-G-CODE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "        YEAR TYPE" TO S1-FLD5.
           MOVE "          PDTHRU%" TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-CONTPDTH         TO S1-3-PT.
           MOVE ": "                TO S1-FLD6.
           MOVE SP-LCYRTYPE         TO S1-7-LC-TYPE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "     DELINQUENCY%" TO S1-FLD5.
           MOVE "       PAYFREQ CD" TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-BANKUNITPER-CD   TO S1-3-CODE-A.
           MOVE ": "                TO S1-FLD6.
           MOVE SP-LCDELFAC         TO S1-7-LC-TYPE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TRW     BANK RULE" TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-TRWBANKRULE      TO S1-3-CODE-A
           IF SP-BANKRULE = " "
              MOVE "30DAYS / 2 MO"  TO S1-3-DESC-A
           ELSE
           IF SP-TRWBANKRULE = "A"
              MOVE "30/1ST 2ND MO"  TO S1-3-DESC-A
           ELSE
           IF SP-TRWBANKRULE = "B"
              MOVE "30 AFTER 1 MO"  TO S1-3-DESC-A
           ELSE
           IF SP-TRWBANKRULE = "C"
              MOVE "30/1ST MO FOL"  TO S1-3-DESC-A
           ELSE
           IF SP-TRWBANKRULE = "D"
              MOVE "WRLD/1 MO FOL"  TO S1-3-DESC-A
           ELSE
           IF SP-TRWBANKRULE = "F"
              MOVE "WRLD/SEMI-MONTHLY"  TO S1-3-DESC-A.
           MOVE "       PAYFREQ CD" TO S1-FLD5.
           MOVE ": "                TO S1-FLD6.
           MOVE SP-LCUNITPER-CD     TO S1-7-LC-CODE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "          PDTHRU%" TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-TRWCONTPDTH      TO S1-3-PT.
           MOVE "    ON JUDGEMENTS" TO S1-FLD5.
           MOVE ": "                TO S1-FLD6.
           MOVE SP-LC-ON-JUDGEMENTS TO S1-7-LC-CODE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "       PAYFREQ CD" TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-TRWBANKUNITPER-CD TO S1-3-CODE-A.
           MOVE "AUTO ASSESS LCHG" TO S1-FLD5.
           MOVE ": "                TO S1-FLD6.
           MOVE SP-AUTO-ASSESS-LCHG TO S1-7-LC-CODE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RECENCY/DEL FRMLA" TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-RECFRMLA      TO S1-3-CODE-A.
           IF SP-RECFRMLA = " "
              MOVE " TRUE DAYS           " TO S1-3-DESC-A
           ELSE IF SP-RECFRMLA = "C"
              MOVE " CALENDAR MONTHS   " TO S1-3-DESC-A.
           MOVE "RENEWAL EDIT"      TO S1-FLD5.
           MOVE ": "                TO S1-FLD6.
           MOVE SP-RENEWAL-EDIT     TO S1-7-LC-CODE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "          PAYCODE" TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-RECPAYCD         TO S1-3-CODE-A.
           MOVE "WRITE INSURANCE? " TO S1-FLD5.
           MOVE ": "                TO S1-FLD6.
           MOVE SP-INSURANCE-WRITTEN TO S1-7-LC-CODE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "          PDTHRU%" TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-RECPDTH          TO S1-3-PT.
           MOVE "LOAN PAY CODE FREQ" TO S1-FLD5.
           MOVE ": "                TO S1-FLD6.
           MOVE SP-UNITPER-CD       TO S1-7-LC-CODE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "  RESCISSION DAYS" TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-RESCIND-BUS-DAYS TO S1-3-CODE-D.
           MOVE "SUB CLASS DESCRIP" TO S1-FLD5.
           MOVE ": "                TO S1-FLD6.
           MOVE SP-DESC             TO S1-7-SC-DESC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "         SATURDAY" TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-RESCIND-SATURDAY TO S1-3-CODE-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CAN LOAN (Y/N/O) " TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-CAN-LOANS-FG     TO S1-3-CODE-D.
           PERFORM WRITE-DETAIL-LINE.

      *    MOVE "        BANK RULE" TO S1-FLD1.
      *    MOVE ": "                TO S1-FLD2.
      *    MOVE SP-RECPAYCD         TO S1-3-CODE-A.
      *    IF SP-RECPAYCD = " "
      *       MOVE " 30DAYS AFTER 2 MONTH" TO S1-3-DESC-A
      *    ELSE IF SP-BANKRULE = "A"
      *       MOVE " 30DAYS 1ST OF 2ND MO" TO S1-3-DESC-A
      *    ELSE IF SP-BANKRULE = "B"
      *       MOVE " 30DAYS AFTER 1 MONTH" TO S1-3-DESC-A
      *    ELSE IF SP-BANKRULE = "C"
      *       MOVE " 30DAYS 1ST OF MO FOL" TO S1-3-DESC-A.
      *    PERFORM WRITE-DETAIL-LINE.

      *    MOVE "          PDTHRU%" TO S1-FLD1.
      *    MOVE ": "                TO S1-FLD2.
      *    MOVE SP-RECPDTH         TO S1-3-PT.
      *    PERFORM WRITE-DETAIL-LINE.


      ******************************************************************
      *                     PAGE 2
      ******************************************************************

       PRINT-PAGE-2.
           IF SPR-PAGE-NUMBER NOT = 2 AND NOT = 99
              GO TO PRINT-PAGE-3.

           PERFORM WRITE-DETAIL-LINE.
           MOVE "PAGE 2" TO H-PG1.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-HEAD4.

           MOVE "DEFERMENT FORMULA" TO S2-FLD1.
           MOVE ": " TO S2-FLD2.
           MOVE SP-DEFFRMLA TO S2-3-CODE.
           IF SP-DEFFRMLA = 99
              MOVE " INTEREST BEARING " TO S2-3-DESC
           ELSE IF SP-DEFFRMLA = 0
              MOVE " CONFIG. DEF. METH" TO S2-3-DESC
           ELSE IF SP-DEFFRMLA = 1
              MOVE " MANUAL INPUT METH" TO S2-3-DESC
           ELSE IF SP-DEFFRMLA = 2
              MOVE " END OF PERIOD    " TO S2-3-DESC
           ELSE IF SP-DEFFRMLA = 3
              MOVE " DAY OF DEFFERMENT" TO S2-3-DESC
           ELSE IF SP-DEFFRMLA = 4
              MOVE " END OF DEFFERMENT" TO S2-3-DESC
           ELSE IF SP-DEFFRMLA = 5
              MOVE " VIRGINIA DEF. CHG" TO S2-3-DESC.
           MOVE "FORM     DESCRIPTION   AUTO-REQ" TO S2-RIGHT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "       PAYFREQ CD" TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-DEFUNITPER-CD    TO S1-3-CODE-A.
           MOVE "  NO                       FLAG" TO S2-RIGHT.
           MOVE ": " TO S2-FLD2.
           MOVE ": " TO S2-FLD2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "          BASE CD" TO S2-FLD1.
           MOVE SP-DEFBASE TO S2-3-CODE.
           IF SP-DEFBASE = 0
              MOVE " REG PAYMENT" TO S2-3-DESC
           ELSE IF SP-DEFBASE = 1
              MOVE " BAL = WRP*REG PYMNT" TO S2-3-DESC
           ELSE IF SP-DEFBASE = 2
              MOVE " LOAN PRIN BAL" TO S2-3-DESC
           ELSE IF SP-DEFBASE = 3
              MOVE " WRP*(REG PMT-MAINF)" TO S2-3-DESC
           ELSE IF SP-DEFBASE = 4
              MOVE " INT + SER + EX + MF" TO S2-3-DESC
           ELSE IF SP-DEFBASE = 5
              MOVE "INT + EXT           " TO S2-3-DESC
           ELSE IF SP-DEFBASE = 6
              MOVE " REG PAY - MAINTF" TO S2-3-DESC.
           MOVE 0 TO SUB.
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "          % CODE" TO S2-FLD1.
           MOVE SP-DEFPERCD TO S2-3-CODE.
           IF SP-DEFPERCD = 0
              MOVE " % IN SP-DEFWORKER" TO S2-3-DESC
           ELSE IF SP-DEFPERCD = 1
              MOVE " 1/12 OF APR" TO S2-3-DESC
           ELSE IF SP-DEFPERCD = 2
              MOVE " RULE 78TH UNIT CHG" TO S2-3-DESC
           ELSE IF SP-DEFPERCD = 3
              MOVE " 1/12 OF AGREED RATE" TO S2-3-DESC
           ELSE IF SP-DEFPERCD = 4
              MOVE " RT INPT AT DEF POST" TO S2-3-DESC
           ELSE IF SP-DEFPERCD = 5
              MOVE "RECIPRICAL OF TERM  " TO S2-3-DESC
           ELSE IF SP-DEFPERCD = 6
              MOVE "LESSER OF CD 0 OR 3" TO S2-3-DESC.
           MOVE " (A) " TO S2-MID.
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "          WORKER" TO S2-FLD1.
           MOVE ": " TO S2-FLD2.
           MOVE SP-DEFWORKER TO S2-3-D-WORK.
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "          MINIMUM" TO S2-FLD1.
           MOVE ": " TO S2-FLD2.
           MOVE SP-DEFMIN TO S2-3-D-MIN.
           MOVE " (B) " TO S2-MID.
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "          MAXIMIM" TO S2-FLD1.
           MOVE ": " TO S2-FLD2.
           MOVE SP-DEFMAX TO S2-3-D-MAX.
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO S2-FLD1.
           MOVE ALL "-" TO S2-FLD2.
           MOVE ALL "-" TO S2-FLD3.
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEFERMENT POLICY" TO S2-FLD1.
           MOVE ": " TO S2-FLD2.
           MOVE SP-DEFPOLCD TO S2-3-D-POL.
           IF SP-DEFPOLCD = " "
              MOVE " M1=DEF/YR M2=/CONTR" TO S2-3-D-POL-DESC
           ELSE IF SP-DEFPOLCD = "A"
              MOVE " M1,2=DF/CONT,M3=#MO" TO S2-3-D-POL-DESC.
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "            MAX1" TO S2-FLD1.
           MOVE ": " TO S2-FLD2.
           MOVE SP-DEFPOLMX (1) TO S2-3-CODE.
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "            MAX2" TO S2-FLD1.
           MOVE ": " TO S2-FLD2.
           MOVE SP-DEFPOLMX (2) TO S2-3-CODE.
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "            MAX3" TO S2-FLD1.
           MOVE ": " TO S2-FLD2.
           MOVE SP-DEFPOLMX (3) TO S2-3-CODE.
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO S2-FLD1.
           MOVE ALL "-" TO S2-FLD2.
           MOVE ALL "-" TO S2-FLD3.
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.

      * GLB  04/14/99
           MOVE "ALLOW DEL POLICY " TO S2-FLD1.
           MOVE ": " TO S2-FLD2.
           MOVE SP-ALLOW-DELINQ-POLCD TO S2-3-CODE.
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "YTD MAX ALLOW DEL" TO S2-FLD1.
           MOVE ": " TO S2-FLD2.
           MOVE SP-YTD-MAX-ALLOW-DELINQ TO S2-3-CODE.
      * 12
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOT MAX ALLOW DEL" TO S2-FLD1.
           MOVE ": " TO S2-FLD2.
           MOVE SP-MAX-ALLOW-DELINQ TO S2-3-CODE.
      * 13
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.
      * 14
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.
      * 15
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.
      * 16
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.
      * 17
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.
      * 18
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.
      * 19
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.
      * 20
           PERFORM FORM-TABLE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " IMMEDIATE RECEIPT PRINT: " TO S2-FLD10.
           MOVE SP-IMED-REC TO S2-FLD11.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LEGEND: (A) CHECKS, (B) RECEIPT" TO S2-RIGHT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " SALESFIN INTERIM COUPON: " TO S2-FLD10.
           MOVE SP-INTERIM-COUPON TO S2-FLD11.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *                      PAGE 3
      ******************************************************************

       PRINT-PAGE-3.
           IF SPR-PAGE-NUMBER NOT = 3 AND NOT = 99
              GO TO PRINT-PAGE-4.

           MOVE "PAGE 3" TO H-PG1.
           PERFORM NEW-PAGE.
           MOVE ALL "-" TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INTEREST METHOD" TO S3-FLD1.
           MOVE ": " TO S3-FLD2.
           MOVE SP-INTMETHOD TO S3-3-CODE-A.
           IF SP-INTMETHOD = "E"
              MOVE " EXACT INT. INT/365" TO S3-3-DESC-A
           ELSE IF SP-INTMETHOD = "O"
              MOVE " ORD INT. INT/360" TO S3-3-DESC-A.
           MOVE "SERVICE CHG FORMULA" TO S3-FLD5.
           MOVE ": " TO S3-FLD6.
           MOVE SP-SCFRMLA TO S3-7-CODE-A.
           IF SP-SCFRMLA = 0
              MOVE " NO SERV CHG" TO S3-7-DESC-A
           ELSE IF SP-SCFRMLA = 1
              MOVE " % PRINCIPAL" TO S3-7-DESC-A
           ELSE IF SP-SCFRMLA = 2
              MOVE " CALC ON TOT NOTE" TO S3-7-DESC-A
           ELSE IF SP-SCFRMLA = 3
              MOVE " 4% * TOTAL NOTE" TO S3-7-DESC-A
           ELSE IF SP-SCFRMLA = 4
              MOVE " RATE * AMT FIN" TO S3-7-DESC-A
           ELSE IF SP-SCFRMLA = 5
              MOVE " FIXED UNIT CHG" TO S3-7-DESC-A
           ELSE IF SP-SCFRMLA = 6
              MOVE " % CASH ADVANCE" TO S3-7-DESC-A
           ELSE IF SP-SCFRMLA = 7
              MOVE " % AMT FINANCED" TO S3-7-DESC-A
           ELSE IF SP-SCFRMLA = 8
              MOVE " % ADJ PRIN" TO S3-7-DESC-A
           ELSE IF SP-SCFRMLA = 9
              MOVE " % TOT REPAYABLE" TO S3-7-DESC-A
           ELSE IF SP-SCFRMLA = 10
              MOVE " % TOT NOTE" TO S3-7-DESC-A
           ELSE IF SP-SCFRMLA = 11
              MOVE " % TOT NOTE *RN" TO S3-7-DESC-A
           ELSE IF SP-SCFRMLA = 38
              MOVE " CALC ON TOT NOTE" TO S3-7-DESC-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "        YEAR TYPE" TO S3-FLD1
           MOVE ": " TO S3-FLD2.
           MOVE SP-YEARTYPE TO S3-3-CODE-B.
           MOVE "            RATE   " TO S3-FLD5.
           MOVE ": " TO S3-FLD6.
           MOVE SP-SCRATE TO S3-7-RATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "     1STPYEXT FRM" TO S3-FLD1.
           MOVE ": " TO S3-FLD2.
           MOVE SP-PYEXFRMLA TO S3-3-CODE-C.
           IF SP-PYEXFRMLA = 0
              MOVE " NO CHG ASSESSED" TO  S3-3-DESC-C
           ELSE IF SP-PYEXFRMLA = 1
              MOVE " EXT DYS=SING INT CG" TO S3-3-DESC-C
           ELSE IF SP-PYEXFRMLA = 2
              MOVE " EXT CG=ORIG PR DIEM" TO S3-3-DESC-C
           ELSE IF SP-PYEXFRMLA = 3
              MOVE " 1/2OR11/2% OF T NOT" TO S3-3-DESC-C
           ELSE IF SP-PYEXFRMLA = 4
              MOVE " PER ORIG CHARGE" TO S3-3-DESC-C
           ELSE IF SP-PYEXFRMLA = 5
              MOVE " 1ST PERIOD CHG/30" TO S3-3-DESC-C
           ELSE IF SP-PYEXFRMLA = 11
              MOVE " INTCHG / ORGTERM " TO S3-3-DESC-C.
           MOVE "            MIN    " TO S3-FLD5.
           MOVE "            MIN    " TO S3-FLD5.
           MOVE ": " TO S3-FLD6.
           MOVE SP-SCMIN TO S3-7-MIN.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO S3-LEFT.
           MOVE "            MAX    " TO S3-FLD5.
           MOVE ": " TO S3-FLD6.
           MOVE SP-SCMAX TO S3-7-MAX.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "1ST PAY EXT  CODE" TO S3-FLD1.
           MOVE ": " TO S3-FLD2.
           MOVE SP-PYEXSPCODE TO S3-3-CODE-C.
           IF SP-PYEXSPCODE = 0
              MOVE " NO SPECL SITUATION" TO  S3-3-DESC-C.
           MOVE ALL "-" TO S3-RIGHT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "        YEAR TYPE" TO S3-FLD1.
           MOVE ": " TO S3-FLD2.
           MOVE SP-PYEXYEARTP TO S3-3-CODE-B.
           MOVE "MAINTEN CHG FORMULA" TO S3-FLD5.
           MOVE ": " TO S3-FLD6.
           MOVE SP-MFFRMLA TO S3-7-CODE-A.
           IF SP-MFFRMLA = 0
              MOVE " NO MAINT FEES" TO S3-7-DESC-A
           ELSE IF SP-MFFRMLA = 1
              MOVE " PYMT=TOT NOT + CHGS" TO S3-7-DESC-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "        FROM DATE" TO S3-FLD1.
           MOVE ": " TO S3-FLD2.
           MOVE SP-PYEXFMDATE TO S3-3-CODE-A.
           IF SP-PYEXFMDATE = "I"
              MOVE " INT DATE" TO S3-3-DESC-A
           ELSE IF SP-PYEXFMDATE = "L"
              MOVE " LOAN DATE" TO S3-3-DESC-A.
           MOVE "            RATE   " TO S3-FLD5.
           MOVE ": " TO S3-FLD6.
           MOVE SP-MFRATE TO S3-7-RATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "        BASE CODE" TO S3-FLD1.
           MOVE ": " TO S3-FLD2.
           MOVE SP-PYEXBASECD TO S3-3-CODE-A.
           IF SP-PYEXBASECD = "D"
              MOVE " DAYS" TO S3-3-DESC-A
           ELSE IF SP-PYEXBASECD = "M"
              MOVE " MONTHS" TO S3-3-DESC-A.
           MOVE "            MIN    " TO S3-FLD5.
           MOVE ": " TO S3-FLD6.
           MOVE SP-MFMIN TO S3-7-MIN.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "        BASE     " TO S3-FLD1.
           MOVE ": " TO S3-FLD2.
           MOVE SP-PYEXBASE TO S3-3-CODE-B.
           MOVE "            MAX    " TO S3-FLD5.
           MOVE ": " TO S3-FLD6.
           MOVE SP-MFMAX TO S3-7-MAX.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "        DAYS     " TO S3-FLD1.
           MOVE ": " TO S3-FLD2.
           MOVE SP-PYEXDAYS TO S3-3-CODE-B.
           MOVE ALL "-" TO S3-RIGHT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO S3-LEFT.
           MOVE "ACCELERATION          RATE    AMOUNT" TO S3-RIGHT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MAX APR FORMULA  " TO S3-FLD1.
           MOVE ": " TO S3-FLD2.
           MOVE SP-MAXAPR-FRMLA TO  S3-3-CODE-C.
           IF SP-MAXAPR-FRMLA = 0
              MOVE " APR EDITED PG4 TBL" TO S3-3-DESC-C.
           MOVE "FORMULA  " TO S3-FLD8.
           MOVE ": " TO S3-FLD9.
           MOVE SP-ACC-FRMLA TO S3-10-CODE.
           MOVE SP-ACC-RATE (1) TO S3-FLD12.
           MOVE SP-ACC-BREAK (1) TO S3-FLD14.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MIN SF  FORMULA  " TO S3-FLD1.
           MOVE ": " TO S3-FLD2.
           MOVE SP-MINSF-FRMLA TO S3-3-CODE-C.
           IF SP-MINSF-FRMLA = 0
              MOVE " SEE MIN SF CHARGE" TO S3-3-DESC-C.
           MOVE "YEAR TYPE" TO S3-FLD8.
           MOVE ": " TO S3-FLD9.
           MOVE SP-ACC-YRTYPE TO S3-FLD10.
           MOVE SP-ACC-RATE (2) TO S3-FLD12.
           MOVE SP-ACC-BREAK (2) TO S3-FLD14.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MIN SF  CHARGE   " TO S3-FLD1.
           MOVE ": " TO S3-FLD2.
           MOVE SP-MINSF-CHG TO S3-3-CHG.
           MOVE "INT METHD" TO S3-FLD8.
           MOVE ": " TO S3-FLD9.
           MOVE SP-ACC-INTMETH TO S3-FLD10.
           MOVE SP-ACC-RATE (3) TO S3-FLD12.
           MOVE SP-ACC-BREAK (3) TO S3-FLD14.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SP-ACC-RATE (4) TO S3-FLD12.
           MOVE SP-ACC-BREAK (4) TO S3-FLD14.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SP-ACC-RATE (5) TO S3-FLD12.
           MOVE SP-ACC-BREAK (5) TO S3-FLD14.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *                        PAGE 04
      ******************************************************************
       PRINT-PAGE-4.
           IF SPR-PAGE-NUMBER NOT = 4 AND NOT = 99
              GO TO PRINT-PAGE-5.

           MOVE 1 TO LN-FEED.
           MOVE "PAGE 4" TO H-PG1.
           PERFORM PRINT-HEAD4.
           MOVE "SPECIAL FRMLA" TO D2-TAG1.
           MOVE ":" TO D2-COLON1.
           MOVE SP-CAL-SPFRMLA TO D2-FRMLA1.
           IF SP-CAL-SPFRMLA = 0
              MOVE " NO SPECIAL REQ" TO D2-DESC1
           ELSE IF SP-CAL-SPFRMLA = 1
              MOVE " EXTENDED EDIT TBL" TO D2-DESC1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RATE TYPE" TO D2-TAG1.
           MOVE ":" TO D2-COLON1.
           MOVE SP-CAL-RATETYPE (1) TO D2-TYPE1.
           IF SP-CAL-RATETYPE (1) = " "
              MOVE " NONE" TO D2-DESC1
           ELSE IF SP-CAL-RATETYPE (1) = "A"
              MOVE " ADDON" TO D2-DESC1
           ELSE IF SP-CAL-RATETYPE (1) = "D"
              MOVE " DISCOUNT" TO D2-DESC1
           ELSE IF SP-CAL-RATETYPE (1) = "S"
              MOVE " SIMPLE INTEREST" TO D2-DESC1
           ELSE IF SP-CAL-RATETYPE (1) = "M"
              MOVE " FLAT MONTHLY   " TO D2-DESC1
           ELSE IF SP-CAL-RATETYPE (1) = "Z"
              MOVE " AMORTIZED INT. " TO D2-DESC1.
           MOVE "RATE TYPE" TO D2-TAG2.
           MOVE ":" TO D2-COLON2.
           MOVE SP-CAL-RATETYPE (2) TO D2-TYPE2.
           IF SP-CAL-RATETYPE (2) = " "
              MOVE " NONE" TO D2-DESC2
           ELSE IF SP-CAL-RATETYPE (2) = "A"
              MOVE " ADDON" TO D2-DESC2
           ELSE IF SP-CAL-RATETYPE (2) = "D"
              MOVE " DISCOUNT" TO D2-DESC2
           ELSE IF SP-CAL-RATETYPE (2) = "S"
              MOVE " SIMPLE INTEREST" TO D2-DESC2
           ELSE IF SP-CAL-RATETYPE (2) = "M"
              MOVE " FLAT MONTHLY   " TO D2-DESC2
           ELSE IF SP-CAL-RATETYPE (2) = "S"
              MOVE " AMORTIZED INT. " TO D2-DESC2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BREAK TYPE" TO D2-TAG1.
           MOVE ":" TO D2-COLON1.
           MOVE SP-CAL-BREAKTYPE (1) TO D2-TYPE1.
           IF SP-CAL-BREAKTYPE (1) = " "
              MOVE " NONE" TO D2-DESC1
           ELSE IF SP-CAL-BREAKTYPE (1) = "A"
              MOVE " AMOUNT BREAK" TO D2-DESC1
           ELSE IF SP-CAL-BREAKTYPE (1) = "T"
              MOVE " TERM BREAK" TO D2-DESC1.
           MOVE "BREAK TYPE" TO D2-TAG2.
           MOVE ":" TO D2-COLON2.
           MOVE SP-CAL-BREAKTYPE (2) TO D2-TYPE2.
           IF SP-CAL-BREAKTYPE (2) = " "
              MOVE " NONE" TO D2-DESC2
           ELSE IF SP-CAL-BREAKTYPE (2) = "A"
              MOVE " AMOUNT BREAK" TO D2-DESC2
           ELSE IF SP-CAL-BREAKTYPE (2) = "T"
              MOVE " TERM BREAK" TO D2-DESC2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHGBLE CD" TO D2-TAG1.
           MOVE ":" TO D2-COLON1.
           MOVE SP-CAL-CHGABLE (1) TO D2-TYPE1.
           IF SP-CAL-CHGABLE (1) = "P"
              MOVE " PROCEEDS OF LOAN" TO D2-DESC1
            ELSE IF SP-CAL-CHGABLE (1) = "I"
              MOVE " LOAN PROCDS+ALL INS" TO D2-DESC1
           ELSE IF SP-CAL-CHGABLE (1) = "S"
              MOVE " LOAN PROCDS+SERV CG" TO D2-DESC1
           ELSE IF SP-CAL-CHGABLE (1) = "B"
              MOVE " LOAN PROCDS+INS+SC" TO D2-DESC1.
           MOVE "CHGBLE CD" TO D2-TAG2.
           MOVE ":" TO D2-COLON2.
           MOVE SP-CAL-CHGABLE (2) TO D2-TYPE2.
           IF SP-CAL-CHGABLE (2) = "P"
              MOVE " PROCEEDS OF LOAN" TO D2-DESC2
           ELSE IF SP-CAL-CHGABLE (2) = "I"
              MOVE " LOAN PROCDS+ALL INS" TO D2-DESC2
           ELSE IF SP-CAL-CHGABLE (2) = "S"
              MOVE " LOAN PROCDS+SERV CG" TO D2-DESC2
           ELSE
              MOVE " LOAN PROCDS+INS+SC" TO D2-DESC2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TBL FRMLA" TO D2-TAG1.
           MOVE ":" TO D2-COLON1.
           MOVE SP-CAL-TBFRMLA (1) TO D2-FRMLA1.
           IF SP-CAL-TBFRMLA (1) = 0
              MOVE " NONE" TO D2-DESC1.
           MOVE "TBL FRMLA" TO D2-TAG2.
           MOVE ":" TO D2-COLON2.
           MOVE SP-CAL-TBFRMLA (2) TO D2-FRMLA2.
           IF SP-CAL-TBFRMLA (2) = 0
              MOVE " NONE" TO D2-DESC2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           MOVE "**   RATE     AMT/TRM" TO D-TABHEAD1.
           MOVE "**   RATE     AMT/TRM" TO D-TABHEAD3.
           PERFORM WRITE-DETAIL-LINE.

           SET CTBL CSTP TO 1.
           PERFORM PRINT-CAL-TBL 5 TIMES.
           MOVE 1 TO LN-FEED.

      *  GLB  04/14/99
           MOVE "NOTE: SINGLE PAYS SEE P20" TO D-TABHEAD1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "** EDITS" TO D-TABHEAD1.
           MOVE "TYPE---RATE  TYPE-----AMOUNT    TERM" TO D-TABHEAD2.
           MOVE "** EDITS" TO D-TABHEAD3.
           MOVE "TYPE---RATE  TYPE-----AMOUNT    TERM" TO D-TABHEAD4.
           PERFORM WRITE-DETAIL-LINE.
           SET ETBL ESTP TO 1.
           PERFORM PRINT-EDIT-TABLES 6 TIMES.

      ******************************************************************
      *                     PAGE 05 - 12    
      *=================================================================
      *    ILEV IS BEING USED IN SECTIONS PRINT-INS-TABLE AND
      *    PRINT-INS-TABLE2.  NEED TO SET ILEV AT THE BEGINNING OF 
      *    EACH PRINT PAGE PARAGRAPH BELOW INSTEAD OF SETTING IT ONCE
      *    (WITHIN PRINT-PAGE-5) THEN INCREMENTING IT AT THE END OF
      *    EACH PRINT-INS-TABLE SECTION...WORKED FOR F3-ALL PAGES    
      *    BUT NOT FOR A GIVEN PAGE [KEC 2016.0428 PL#00929].
      *
      *    PAGE 05 --- ILEV = 1
      *    PAGE 06 --- ILEV = 2
      *    PAGE 07 --- ILEV = 3
      *    PAGE 08 --- ILEV = 4
      *    PAGE 09 --- ILEV = 5
      *    PAGE 10 --- ILEV = 6
      *    PAGE 11 --- ILEV = 7
      *    PAGE 19 --- ILEV = 8
      *
      ******************************************************************

       PRINT-PAGE-5.
           IF SPR-PAGE-NUMBER NOT = 5 AND NOT = 99
              GO TO PRINT-PAGE-6.

           MOVE "PAGE 5" TO H-PG1.
           PERFORM NEW-PAGE.
           MOVE ALL "-" TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "** CREDIT LIFE DECLINING **" TO INS-TAG1.
           SET ILEV TO 1.
           PERFORM PRINT-INS-TABLE.

       PRINT-PAGE-6.
           IF SPR-PAGE-NUMBER NOT = 6 AND NOT = 99
              GO TO PRINT-PAGE-7.

           MOVE 2 TO LN-FEED.
           MOVE "PAGE 6" TO H-PG1.
           PERFORM PRINT-HEAD4.
           MOVE "** CREDIT LIFE DECLINING JOINT **" TO INS-TAG1.
           SET ILEV TO 2.
           PERFORM PRINT-INS-TABLE.

       PRINT-PAGE-7.
           IF SPR-PAGE-NUMBER NOT = 7 AND NOT = 99
              GO TO PRINT-PAGE-8.

           MOVE 2 TO LN-FEED.
           MOVE "PAGE 7" TO H-PG1.
           PERFORM PRINT-HEAD4.
           MOVE "** CREDIT LIFE LEVEL **" TO INS-TAG1.
           SET ILEV TO 3.
           PERFORM PRINT-INS-TABLE.

       PRINT-PAGE-8.
           IF SPR-PAGE-NUMBER NOT = 8 AND NOT = 99
              GO TO PRINT-PAGE-9.

           MOVE 2 TO LN-FEED.
           MOVE "PAGE 8" TO H-PG1.
           PERFORM PRINT-HEAD4.
           MOVE "** CREDIT LIFE LEVEL JOINT **" TO INS-TAG1.
           SET ILEV TO 4.
           PERFORM PRINT-INS-TABLE.

       PRINT-PAGE-9.
           IF SPR-PAGE-NUMBER NOT = 9 AND NOT = 99
              GO TO PRINT-PAGE-10.

           MOVE 2 TO LN-FEED.
           MOVE "PAGE 9" TO H-PG1.
           PERFORM PRINT-HEAD4.
           MOVE "** ACCIDENT/HEALTH **" TO INS-TAG1.
           SET ILEV TO 5.
           PERFORM PRINT-INS-TABLE2.

       PRINT-PAGE-10.
           IF SPR-PAGE-NUMBER NOT = 10 AND NOT = 99
              GO TO PRINT-PAGE-11.

           MOVE 2 TO LN-FEED.
           MOVE "PAGE 10" TO H-PG1.
           PERFORM PRINT-HEAD4.
           MOVE "** PROPERTY/OTHER **" TO INS-TAG1.
           SET ILEV TO 6.
           PERFORM PRINT-INS-TABLE.

       PRINT-PAGE-11.
           IF SPR-PAGE-NUMBER NOT = 11 AND NOT = 99
              GO TO PRINT-PAGE-12.

           MOVE 2 TO LN-FEED.
           MOVE "PAGE 11" TO H-PG1.
           PERFORM PRINT-HEAD4.
           MOVE "** OTHER INSURANCE **" TO INS-TAG1.
           SET ILEV TO 7.
           PERFORM PRINT-INS-TABLE.

       PRINT-PAGE-12.
           IF SPR-PAGE-NUMBER NOT = 12 AND NOT = 99
              GO TO PRINT-PAGE-13.

           MOVE 2 TO LN-FEED.
           MOVE "PAGE 12" TO H-PG1.
           PERFORM PRINT-HEAD4.

           MOVE "REFND" TO P12-TAG1.
           MOVE "REFUND" TO P12-TAG2.
           MOVE " MIN" TO P12-TAG3.
           MOVE "MIN" TO P12-TAG4.
           MOVE "DEDUCT" TO P12-TAG5.
           MOVE "SPEC" TO P12-TAG6.
           MOVE "EARLY" TO P12-TAG7.
           MOVE "DEFER" TO P12-TAG8.
           MOVE "REFND" TO P12-TAG9.
           MOVE "REDUCT" TO P12-TAG10.
           MOVE "REBATE" TO P12-TAG11.
           MOVE P12-HEAD TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO INS-REBATE-HEAD.
           MOVE "FRMLA" TO P12-TAG1.
           MOVE "FRMLA2" TO P12-TAG2.
           MOVE "RETENT" TO P12-TAG3.
           MOVE "MON" TO P12-TAG4.
           MOVE "PRECHG" TO P12-TAG5.
           MOVE "OPT1" TO P12-TAG6.
           MOVE " PROV" TO P12-TAG7.
           MOVE " CODE" TO P12-TAG8.
           MOVE " %AGE" TO P12-TAG9.
           MOVE "AMOUNT" TO P12-TAG10.
           MOVE "   MIN" TO P12-TAG11.
           MOVE P12-HEAD TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO INS-REBATE-HEAD.
           MOVE 0 TO SUB.
           PERFORM PRINT-REBATE-TABLE 15 TIMES.
           MOVE "16. O2 INS CO :" TO INSCO-TAG.
           MOVE SP-DEFAULT-O2-INS-COMP TO REB-FRMLA.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *                      PAGE 13        
      ******************************************************************
       PRINT-PAGE-13.
           IF SPR-PAGE-NUMBER NOT = 13 AND NOT = 99
              GO TO PRINT-PAGE-14.

           MOVE 2 TO LN-FEED.
           MOVE "PAGE 13" TO H-PG1.
           PERFORM PRINT-HEAD4.
           MOVE "PRIOR TO 1STPAY" TO HD5-1.
           MOVE "ON OR AFTER 1ST" TO HD5-2. 
           MOVE HEAD5 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           MOVE " ADD  REF  YEAR" TO R-TAB-TAG1.
           MOVE "     ADD  " TO R-TAB-TAG2.
           MOVE "REF  Y" TO R-TAB-TAG3.
           MOVE "EAR      " TO R-TAB-TAG4.
           MOVE "RECAST " TO R-TAB-TAG5.
           MOVE "   SPEC " TO R-TAB-TAG6.
           MOVE "  MAN-  PAY-                  " TO R-TAB-TAG7.
           MOVE INS-REBATE-HEAD TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO INS-REBATE-HEAD.
           MOVE "DAYS DATE  TYPE" TO R-TAB-TAG1.
           MOVE "    DAYS D" TO R-TAB-TAG2.
           MOVE "ATE  T" TO R-TAB-TAG3.
           MOVE "YPE      " TO R-TAB-TAG4.
           MOVE "  CODE " TO R-TAB-TAG5.
           MOVE "   OPT2 " TO R-TAB-TAG6.
           MOVE "  AGER  FREQ                  " TO R-TAB-TAG7.
           MOVE INS-REBATE-HEAD TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO INS-REBATE-HEAD.
           MOVE 0 TO SUB.
           PERFORM PRINT-INS-REB-TABLE2 14 TIMES.

      ******************************************************************
      *                       PAGE 14           
      ******************************************************************
       PRINT-PAGE-14.
           IF SPR-PAGE-NUMBER NOT = 14 AND NOT = 15 AND NOT = 99
              GO TO PRINT-PAGE-16.

           PERFORM WRITE-DETAIL-LINE.

           MOVE "PAGE 14" TO H-PG1.
           MOVE "PAGE 15" TO H-PG2.
           PERFORM PRINT-HEAD4.

           MOVE "GENERAL EARN CODES         " TO ERN-HD1.
           MOVE "INTEREST EARN CODES        " TO ERN-HD2.
           MOVE "ADDON INSURANCE EARN CODES " TO ERN-HD3.
           MOVE "SERVICE CHARGE EARN CODES  " TO ERN-HD4.
           MOVE EARNINGS-HEAD TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ACCELERATE EARN CD " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-ERNACCEL-CD TO E-CDS-A.
           MOVE "EARNINGS FORMULA   " TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-ERNFRMLA (1) TO E-CD-FRMLA-B.
           MOVE "EARNINGS FORMULA   " TO EARN-TAG-C.
           MOVE ":" TO EARN-COL-C.
           MOVE SP-ERNFRMLA (2) TO E-CD-FRMLA-C.
           MOVE "EARNINGS FORMULA   " TO EARN-TAG-D.
           MOVE ":" TO EARN-COL-D.
           MOVE SP-ERNFRMLA (3) TO E-CD-FRMLA-D.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "JUDGEMENT          " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-ERNJD-CD TO E-CDS-A.
           MOVE "UPFRONT EARN AMT   " TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-ERNAMT (1) TO E-CODE-AMT-B.
           MOVE "UPFRONT EARN AMT   " TO EARN-TAG-C.
           MOVE ":" TO EARN-COL-C.
           MOVE SP-ERNAMT (2) TO E-CODE-AMT-C.
           MOVE "UPFRONT EARN AMT   " TO EARN-TAG-D.
           MOVE ":" TO EARN-COL-D.
           MOVE SP-ERNAMT (3) TO E-CODE-AMT-D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TRF-T-PUR ATERM CD " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-MIDSTRERNCD TO E-CDS-A.
           MOVE "            %CODE/%" TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-ERNPERCD (1) TO E-CD-B.
           MOVE SP-ERNPERCT (1) TO E-PERCT-B.
           MOVE "            %CODE/%" TO EARN-TAG-C.
           MOVE ":" TO EARN-COL-C.
           MOVE SP-ERNPERCD (2) TO E-CD-C.
           MOVE SP-ERNPERCT (2) TO E-PERCT-C.
           MOVE "            %CODE/%" TO EARN-TAG-D.
           MOVE ":" TO EARN-COL-D.
           MOVE SP-ERNPERCD (3) TO E-CD-D.
           MOVE SP-ERNPERCT (3) TO E-PERCT-D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DELINQ STOP FORMULA" TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-ERNDELQFRMLA TO E-CDS-A.
           MOVE "ACCRUAL EARN  CODE " TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-ERNACCRL-FG (1) TO E-CDS-B.
           MOVE "ACCRUAL EARN  CODE " TO EARN-TAG-C.
           MOVE ":" TO EARN-COL-C.
           MOVE SP-ERNACCRL-FG (2) TO E-CDS-C.
           MOVE "ACCRUAL EARN  CODE " TO EARN-TAG-D.
           MOVE ":" TO EARN-COL-D.
           MOVE SP-ERNACCRL-FG (3) TO E-CDS-D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DELINQ STOP POINT  " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-ERNDELQ-STOP TO E-CDS-A1.
           MOVE "ACCRUAL START CODE " TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-ERNACCRL-START-CD (1) TO E-CDS-B.
           MOVE "ACCRUAL START CODE " TO EARN-TAG-C.
           MOVE ":" TO EARN-COL-C.
           MOVE SP-ERNACCRL-START-CD (2) TO E-CDS-C.
           MOVE "ACCRUAL START CODE " TO EARN-TAG-D.
           MOVE ":" TO EARN-COL-D.
           MOVE SP-ERNACCRL-START-CD (3) TO E-CDS-D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DELINQ START POINT " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-ERNDELQ-START TO E-CDS-A1.
           MOVE "ACCRUAL DEF.  CODE " TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-ERNACCRL-DFCD (1) TO E-CDS-B.
           MOVE "ACCRUAL DEF.  CODE " TO EARN-TAG-C.
           MOVE ":" TO EARN-COL-C.
           MOVE SP-ERNACCRL-DFCD (2) TO E-CDS-C.
           MOVE "ACCRUAL DEF.  CODE " TO EARN-TAG-D.
           MOVE ":" TO EARN-COL-D.
           MOVE SP-ERNACCRL-DFCD (3) TO E-CDS-D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "EARNINGS ADD DAYS  " TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-ERN-DAYS (1) TO E-CDS-B1.
           MOVE "EARNINGS ADD DAYS  " TO EARN-TAG-C.
           MOVE ":" TO EARN-COL-C.
           MOVE SP-ERN-DAYS (2) TO E-CDS-C1.
           MOVE "EARNINGS ADD DAYS  " TO EARN-TAG-D.
           MOVE ":" TO EARN-COL-D.
           MOVE SP-ERN-DAYS (3) TO E-CDS-D1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "IB MAX ACCRUE DAYS " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-MAX-IB-ACR-DAYS TO E-CDS-A1.
           MOVE "FIXED TERM         " TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-ERN-TERM (1) TO E-CDS-B1.
           MOVE "FIXED TERM         " TO EARN-TAG-C.
           MOVE ":" TO EARN-COL-C.
           MOVE SP-ERN-TERM (2) TO E-CDS-C1.
           MOVE "FIXED TERM         " TO EARN-TAG-D.
           MOVE ":" TO EARN-COL-D.
           MOVE SP-ERN-TERM (3) TO E-CDS-D1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " DELQ+AGE/MAX DAYS " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-DELQ-IB-AGE        TO E-AGE-A14.
           MOVE SP-DELQ-IB-MAX-DAYS   TO E-DAY-A14.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INTEREST ERN LEVEL " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-EARN-ORDER (1)     TO E-CD-FRMLA-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ADDON FC ERN LEVEL " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-EARN-ORDER (2)     TO E-CD-FRMLA-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SERV CHG ERN LEVEL " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-EARN-ORDER (3)     TO E-CD-FRMLA-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MAINTFEE ERN LEVEL " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-EARN-ORDER (4)     TO E-CD-FRMLA-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEAL DSC ERN LEVEL " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-EARN-ORDER (5)     TO E-CD-FRMLA-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OTHER    ERN LEVEL " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-EARN-ORDER (6)     TO E-CD-FRMLA-A.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *                  PAGE 16 AND 17    
      ******************************************************************
       PRINT-PAGE-16.
           IF SPR-PAGE-NUMBER NOT = 16 AND NOT = 17 AND NOT = 99
              GO TO PRINT-PAGE-18.

           MOVE "PAGE 16" TO H-PG1.
           MOVE "PAGE 17" TO H-PG2.
           PERFORM PRINT-HEAD4.

           MOVE "MAINT. FEES EARN CODES     " TO ERN-HD1.
           MOVE "DEALER DISCOUNT EARN CODES " TO ERN-HD2.
           MOVE "OTHER EARNING CODES        " TO ERN-HD3.
           MOVE "ADDON INSURANCE / FEE INFO " TO ERN-HD4.
           MOVE EARNINGS-HEAD TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "EARNINGS FORMULA   " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-ERNFRMLA (4) TO E-CD-FRMLA-A.
           MOVE "EARNINGS FORMULA   " TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-ERNFRMLA (5) TO E-CD-FRMLA-B.
           MOVE "EARNINGS FORMULA   " TO EARN-TAG-C.
           MOVE ":" TO EARN-COL-C.
           MOVE SP-ERNFRMLA (6) TO E-CD-FRMLA-C.
           MOVE "FIRST PAY MIN DAYS " TO EARN-TAG-D.
           MOVE ":" TO EARN-COL-D.
           MOVE SP-1STPY-ADDON-DAYS TO E-CDS-D1.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "UPFRONT EARN AMT   " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-ERNAMT (4) TO E-CODE-AMT-A.
           MOVE "UPFRONT EARN AMT   " TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-ERNAMT (5) TO E-CODE-AMT-B.
           MOVE "UPFRONT EARN AMT   " TO EARN-TAG-C.
           MOVE ":" TO EARN-COL-C.
           MOVE SP-ERNAMT (6) TO E-CODE-AMT-C.
           MOVE "FIRST PAY YEAR TYPE" TO EARN-TAG-D.
           MOVE ":" TO EARN-COL-D.
           MOVE SP-1STPY-ADDON-YRTYPE TO E-CDS-D1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "            %CODE/%" TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-ERNPERCD (4) TO E-CD-A.
           MOVE SP-ERNPERCT (4) TO E-PERCT-A.
           MOVE "            %CODE/%" TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-ERNPERCD (5) TO E-CD-B.
           MOVE SP-ERNPERCT (5) TO E-PERCT-B.
           MOVE "            %CODE/%" TO EARN-TAG-C.
           MOVE ":" TO EARN-COL-C.
           MOVE SP-ERNPERCD (6) TO E-CD-C.
           MOVE SP-ERNPERCT (6) TO E-PERCT-C.
           MOVE "ADDON RATE TYPE" TO EARN-TAG-D.
           MOVE ":" TO EARN-COL-D.
           MOVE SP-ADDON-RATE-FG TO E-CDS-D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ACCRUAL EARN  CODE " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-ERNACCRL-FG (4) TO E-CDS-A.
           MOVE "ACCRUAL EARN  CODE " TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-ERNACCRL-FG (5) TO E-CDS-B.
           MOVE "ACCRUAL EARN  CODE " TO EARN-TAG-C.
           MOVE ":" TO EARN-COL-C.
           MOVE SP-ERNACCRL-FG (6) TO E-CDS-C.
           MOVE "FIXED RATE (ANNUAL)" TO EARN-TAG-D.
           MOVE ":" TO EARN-COL-D.
           MOVE SP-ADDON-RATE TO E-PERCT-D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ACCRUAL START CODE " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-ERNACCRL-START-CD (4) TO E-CDS-A.
           MOVE "ACCRUAL START CODE " TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-ERNACCRL-START-CD (5) TO E-CDS-B.
           MOVE "ACCRUAL START CODE " TO EARN-TAG-C.
           MOVE ":" TO EARN-COL-C.
           MOVE SP-ERNACCRL-START-CD (6) TO E-CDS-C.
           MOVE "CONSOLIDATE SCHD CD" TO EARN-TAG-D.
           MOVE SP-CONSOLIDATE-RT-FG TO E-CDS-D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ACCRUAL DEF.  CODE " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-ERNACCRL-DFCD (4) TO E-CDS-A.
           MOVE "ACCRUAL DEF.  CODE " TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-ERNACCRL-DFCD (5) TO E-CDS-B.
           MOVE "ACCRUAL DEF.  CODE " TO EARN-TAG-C.
           MOVE ":" TO EARN-COL-C.
           MOVE SP-ERNACCRL-DFCD (6) TO E-CDS-C.
           MOVE "ORIG OR UPDATED FG" TO EARN-TAG-D.
           MOVE ":" TO EARN-COL-D.
           MOVE SP-ADDON-ORG-MATURITY TO E-CDS-D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "EARNINGS ADD DAYS  " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-ERN-DAYS (4) TO E-CDS-A1.
           MOVE "EARNINGS ADD DAYS  " TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-ERN-DAYS (5) TO E-CDS-B1.
           MOVE "EARNINGS ADD DAYS  " TO EARN-TAG-C.
           MOVE ":" TO EARN-COL-C.
           MOVE SP-ERN-DAYS (6) TO E-CDS-C1.
           MOVE "ROUND TO WHOLE $ FG" TO EARN-TAG-D.
           MOVE ":" TO EARN-COL-D.
           MOVE SP-ADDON-WHOLE-FG TO E-CDS-D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "FIXED TERM         " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-ERN-TERM (4) TO E-CDS-A1.
           MOVE "FIXED TERM         " TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-ERN-TERM (5) TO E-CDS-B1.
           MOVE "FIXED TERM         " TO EARN-TAG-C.
           MOVE ":" TO EARN-COL-C.
           MOVE SP-ERN-TERM (6) TO E-CDS-C1.
           MOVE "1STPAY DISCLOSURE" TO EARN-TAG-D.
           MOVE ":" TO EARN-COL-D.
           MOVE SP-ADDON-DISFRMLA TO E-CDS-D.
           PERFORM WRITE-DETAIL-LINE.

      *  GLB  04/14/99
           MOVE "DLDISC-RVPRIN1%" TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-DLDISC-RVPRIN1-PCNT TO E-PERCT-B.
           MOVE "ADDON EXCEPTION FG " TO EARN-TAG-D.
           MOVE ":"                   TO EARN-COL-D.
           MOVE SP-ADDON-EXCEPT-FG    TO E-ADD-FG-1.
           IF SP-ADDON-EXCEPT-FG = "T"
              MOVE "TRANS START" TO E-ADD-FG-2
           ELSE
              MOVE " " TO E-ADD-FG-2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DLDISC-RVPRIN1-AF%"      TO EARN-TAG-B.
           MOVE ":"                       TO EARN-COL-B.
           MOVE SP-DLDISC-RVPRIN1-AF-PCNT TO E-PERCT-B.
           MOVE "CANCEL ADDON FLAG  " TO EARN-TAG-D.
           MOVE ":"                   TO EARN-COL-D.
           MOVE SP-CANCEL-ADDON-FG    TO E-ADD-FG-1.
           IF SP-CANCEL-ADDON-FG = "F"
              MOVE "FINANCE AMT" TO E-ADD-FG-2
           ELSE
              MOVE " " TO E-ADD-FG-2.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE "LOAN COST FORMULA" TO EARN-TAG-C.
           MOVE ":" TO EARN-COL-C.
           MOVE SP-LNCOST-FRMLA TO E-CD-C.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LOAN COST PERCENT"       TO EARN-TAG-C.
           MOVE ":"                       TO EARN-COL-C.
           MOVE SP-LNCOST-AMT             TO E-CODE-AMT-C.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES                        TO ERN-HD1.
           MOVE SPACES                        TO ERN-HD2.
           MOVE "DUAL LOAN INFORMATION      " TO ERN-HD3.
           MOVE "OTHER INSURANCE EDITS      " TO ERN-HD4.
           MOVE EARNINGS-HEAD TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DUAL LOAN IN STATE " TO EARN-TAG-C.
           MOVE ":"                   TO EARN-COL-C.
           MOVE SP-DUAL-IN-STATE      TO E-CDS-C1.
           MOVE "OTHER 1 INS. EDIT"   TO EARN-TAG-D.
           MOVE ":"                   TO EARN-COL-D.
           MOVE SP-INS-O1-EDIT        TO E-CDS-D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DUAL LOAN IN BRANCH" TO EARN-TAG-C.
           MOVE ":"                   TO EARN-COL-C.
           MOVE SP-DUAL-IN-BRANCH     TO E-CDS-C1.
           MOVE "OTHER 2 INS. EDIT"   TO EARN-TAG-D.
           MOVE ":"                   TO EARN-COL-D.
           MOVE SP-INS-O2-EDIT        TO E-CDS-D.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *                      PAGE 18  
      ******************************************************************
       PRINT-PAGE-18.
           IF SPR-PAGE-NUMBER NOT = 18 AND NOT = 99
              GO TO PRINT-PAGE-19.

           MOVE "PAGE 18" TO H-PG1.
           PERFORM PRINT-HEAD4.
           MOVE "MISCELLANOUS FIELDS        " TO ERN-HD1.
           MOVE "FEE FIELDS                 " TO ERN-HD2.
           MOVE SPACES TO ERN-HD3 ERN-HD4.
           MOVE EARNINGS-HEAD TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "IRREGULAR PAYS FG  " TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-IRREGULAR-PAY-FG   TO E-CD-A.
           MOVE "FEE #1 FORMULA/ CD " TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-FEEFRMLA (1) TO E-CDS-B2-1.
           MOVE SP-FEECD (1)    TO E-CDS-B2-2.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RSV CHG #1 RECAP-CD" TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-DLRVCHGS-RB-CD(1)  TO E-CD-A.
           MOVE "FEE #2 FORMULA/ CD " TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-FEEFRMLA (2) TO EARN-CODE-B.
           MOVE SP-FEECD (2)    TO E-CDS-B2-2.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RSV CHG #2 RECAP-CD" TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-DLRVCHGS-RB-CD(2)  TO E-CD-A.
           MOVE "ALLOW LOAN BACK DATE" TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-ALLOW-LN-BACK-DATE TO E-CD-B.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RSV CHG #3 RECAP-CD" TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-DLRVCHGS-RB-CD(3)  TO E-CD-A.
           MOVE "MAX IB INT AT ESTIM " TO EARN-TAG-B.
           MOVE ":" TO EARN-COL-B.
           MOVE SP-CAL-MAX-AT-ESTIMATE TO E-CD-B.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PARTC HELD RECAP-CD" TO EARN-TAG-A.
           MOVE ":" TO EARN-COL-A.
           MOVE SP-PARVHELD-RB-CD     TO E-CD-A.
           MOVE "ALLOW ZERO PROCEEDS " TO EARN-TAG-B.
           MOVE ":"                    TO EARN-COL-B.
           MOVE SP-ALLOW-ZERO-PROC     TO E-CD-B.
           PERFORM WRITE-DETAIL-LINE.
         
           MOVE "      PAID         " TO EARN-TAG-A.
           MOVE ":"                   TO EARN-COL-A.
           MOVE SP-PARVPAID-RB-CD TO E-CD-A.
           MOVE "ADV 1ST PMT OPTIONS " TO EARN-TAG-B.
           MOVE ":"                    TO EARN-COL-B.
           MOVE SP-ADV-TO-1ST-NO-CORR  TO E-CD-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CONT-CREDIT CD/AMT " TO EARN-TAG-A.
           MOVE ":"                   TO EARN-COL-A.
           MOVE SP-CONTR-CREDIT-CD    TO E-CD-A.
           MOVE SP-CONTR-CREDIT       TO E-PERCT-A.
           MOVE "LC/INT- REPO NO PL"  TO EARN-TAG-B.
           MOVE ":"                   TO EARN-COL-B.
           MOVE SP-LC-INT-ON-REPO-NON-PL 
                                      TO E-CD-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "     MIN DELINQ %  " TO EARN-TAG-A.
           MOVE ":"                   TO EARN-COL-A.
           MOVE SP-CONTR-CREDIT-MIN-DEL-PER TO E-PERCT-A3-1.
           MOVE "CON SET NON ACCRUAL" TO EARN-TAG-B.
           MOVE ":"                   TO EARN-COL-B.
           MOVE SP-CONTRACTUAL-SET-NONACCRUAL TO E-CDS-B1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OPT CHARGEABLE CD  " TO EARN-TAG-A.
           MOVE ":"                   TO EARN-COL-A.
           MOVE SP-OPT-CAL-CHGABLE    TO E-CD-A.
           MOVE "CON RESET ACCRUAL  " TO EARN-TAG-B.
           MOVE ":"                   TO EARN-COL-B.
           MOVE SP-CONTRACTUAL-RESET-ACCRUAL
                                      TO E-CDS-B1.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE "PB DELINQ P/DUE DYS" TO EARN-TAG-A.
           MOVE ":"                   TO EARN-COL-A.
           MOVE SP-PBDEL-DAYS         TO E-DAYS-A4-1.
           MOVE "PMT NAPP COUNTER   " TO EARN-TAG-B.
           MOVE ":"                   TO EARN-COL-B.
           MOVE SP-MONTHS-PAY-NAPP-COUNTER
                                      TO E-CDS-B1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PB INCREASE % INCRS" TO EARN-TAG-A.
           MOVE ":"                   TO EARN-COL-A.
           MOVE SP-PBINCR-PERCENT     TO E-DAYS-A4-1.
           MOVE "SHORT PAY THRESHOLD" TO EARN-TAG-B
           MOVE ":"                   TO EARN-COL-B.
           MOVE SP-SHORT-PAY-THRESHOLD TO E-CDS-B1.    
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CON POTENTIAL 30 CD" TO EARN-TAG-A.
           MOVE ":"                   TO EARN-COL-A.
           MOVE SP-CPOT30-CD          TO E-CD-A.
           MOVE "DISPLAY CASH AVAIL " TO EARN-TAG-B.
           MOVE ":"                   TO EARN-COL-B.
           MOVE SP-CASH-AVAIL-PERCENT TO E-CDS-B1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "         CHGOFF CD " TO EARN-TAG-A.
           MOVE ":"                   TO EARN-COL-A.
           MOVE SP-CPOTCHGOFF-CD      TO E-CDS-A.
           MOVE "% OF NET BAL REQUIR" TO EARN-TAG-B.
           MOVE ":"                   TO EARN-COL-B.
           MOVE SP-NET-BAL-REQ        TO E-CDS-B1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REC POTENTIAL 30 CD" TO EARN-TAG-A.
           MOVE ":"                   TO EARN-COL-A.
           MOVE SP-RDPOT30-CD         TO E-CDS-A.
           MOVE "AMT PROCEED REQUIR"  TO EARN-TAG-B.
           MOVE ":"                   TO EARN-COL-B.
           MOVE SP-AMT-PROCEEDS-REQ   TO E-CDS-BA.
           PERFORM WRITE-DETAIL-LINE.          

           MOVE "         CHGOFF CD " TO EARN-TAG-A.
           MOVE ":"                   TO EARN-COL-A.
           MOVE SP-RDPOTCHGOFF-CD     TO E-CDS-A.
           MOVE "ALT. PREPAY MONTHS " TO EARN-TAG-B.
           MOVE ":"                   TO EARN-COL-B.
           MOVE SP-ALT-PREPAY-MONTHS  TO E-CDS-B1.

           PERFORM WRITE-DETAIL-LINE.

      ************************
      * INSURANCE TABLE      *
      ************************

           PERFORM OPEN-IR-FILE.
           PERFORM INS-RATE-TABLE-LIST.
           PERFORM CLOSE-IR-FILE.

       PRINT-PAGE-19.
           IF SPR-PAGE-NUMBER NOT = 19 AND NOT = 99
              GO TO PRINT-PAGE-20.

           MOVE "PAGE 19" TO H-PG1.
           PERFORM NEW-PAGE.
           MOVE ALL "-" TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "** OTHER ONE (1) INSURANCE **" TO INS-TAG1.
           SET ILEV TO 8.
           PERFORM PRINT-INS-TABLE.
            
      *************************************************************
      *                      PAGE 20                             
      *************************************************************  

       PRINT-PAGE-20.
           IF SPR-PAGE-NUMBER NOT = 20 AND NOT = 99
              GO TO PRINT-PAGE-23.

           MOVE "PAGE 20" TO H-PG1.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-HEAD4.
  
           MOVE "MIN CREDIT SCORE     " TO P20-COL-1.
           MOVE ": "                    TO P20-COL-2.
           MOVE SP-MIN-CREDIT-SCORE     TO MIN-CR-SCORE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SINGLE PAY FLAG      " TO P20-COL-1.
           MOVE ": "                    TO P20-COL-2.
           MOVE SP-PACESETTER-FG        TO E-PY-FLAG.   
           PERFORM WRITE-DETAIL-LINE.

           MOVE "WAGE ASSIGNMENT DAYS " TO P20-COL-1.
           MOVE ": "                    TO P20-COL-2.
           MOVE SP-WAGE-ASSIGN-DAYS     TO WAGE-ASSIGN.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CALC PAYMENT ROUND FG" TO P20-COL-1.
           MOVE ": "                    TO P20-COL-2.
           MOVE SP-CAL-PAYMENT-ROUND-FG TO E-PY-FLAG.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NO PAYS TIL RENEWABLE" TO P20-COL-1.
           MOVE ": "                    TO P20-COL-2.
           MOVE SP-RENEWABLE-PAYS       TO MIN-CR-SCORE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NO DAYS TIL RENEWABLE" TO P20-COL-1.
           MOVE ": "                    TO P20-COL-2.
           MOVE SP-RENEWABLE-DAYS       TO MIN-CR-SCORE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 02 TO LN-FEED.
           PERFORM PRINT-EXP-HEAD.

           MOVE "EXPIR DATE FRMLA " TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-EXPD-FRMLA       TO S1-3-CODE-A.
           IF SP-EXPD-FRMLA = " "
              MOVE " UPDATED MATURITY " TO S1-3-DESC-A
           ELSE IF SP-EXPD-FRMLA = "A"
              MOVE " ORIG. MATURITY   " TO S1-3-DESC-A.
           MOVE "   RATE     BREAK" TO S1-FLD5.
           MOVE "YRTP  INTMETHOD  " TO S1-FLD7.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "           MONTHS" TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-EXPD-MONTHS      TO S1-3-CODE-B.
           MOVE SP-EXPR-RATE (1)    TO S1-FLD8.
           MOVE SP-EXPR-BREAK (1)   TO S1-FLD9.
           MOVE SP-EXPR-YRTYPE (1)  TO S1-FLD10.
           MOVE SP-EXPR-INTMETH (1) TO S1-FLD11.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "           DAYS  " TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-EXPD-DAYS        TO S1-3-CODE-B.
           MOVE SP-EXPR-RATE (2)    TO S1-FLD8.
           MOVE SP-EXPR-BREAK (2)   TO S1-FLD9.
           MOVE SP-EXPR-YRTYPE (2)  TO S1-FLD10.
           MOVE SP-EXPR-INTMETH (2) TO S1-FLD11.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE "           EXCEPT" TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-EXPD-EXCEPT      TO S1-3-CODE-A.
           MOVE SP-EXPR-RATE (3)    TO S1-FLD8.
           MOVE SP-EXPR-BREAK (3)   TO S1-FLD9.
           MOVE SP-EXPR-YRTYPE (3)  TO S1-FLD10.
           MOVE SP-EXPR-INTMETH (3) TO S1-FLD11.
           PERFORM WRITE-DETAIL-LINE.
           
           MOVE "EXPIR RATE FRMLA " TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-EXPR-FRMLA       TO S1-3-CODE-A.
           IF SP-EXPR-FRMLA = " "
              MOVE " CONT PER CONTRACT" TO S1-3-DESC-B
           ELSE IF SP-EXPR-FRMLA = "A"
              MOVE " EXPIR TERM BK TBL" TO S1-3-DESC-B
           ELSE IF SP-EXPR-FRMLA = "B"
              MOVE " EXPIR OVRIDE RATE" TO S1-3-DESC-B.
           MOVE SP-EXPR-RATE (4)    TO S1-FLD8.
           MOVE SP-EXPR-BREAK (4)   TO S1-FLD9.
           MOVE SP-EXPR-YRTYPE (4)  TO S1-FLD10.
           MOVE SP-EXPR-INTMETH (4) TO S1-FLD11.
           PERFORM WRITE-DETAIL-LINE.
        
           MOVE "EXPIR L/C CODE   " TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-EXPR-LCHG        TO S1-3-CODE-A.
           MOVE SP-EXPR-RATE (5)    TO S1-FLD8.
           MOVE SP-EXPR-BREAK (5)   TO S1-FLD9.
           MOVE SP-EXPR-YRTYPE (5)  TO S1-FLD10.
           MOVE SP-EXPR-INTMETH (5) TO S1-FLD11.
           PERFORM WRITE-DETAIL-LINE.
         
           MOVE "EXPIR REDU FRMLA " TO S1-FLD1.
           MOVE ": "                TO S1-FLD2.
           MOVE SP-EXPR-REDU-FRMLA  TO S1-3-CODE-A.
           IF SP-EXPR-REDU-FRMLA = " "
              MOVE " CONT REDUCED RATE" TO S1-3-DESC-A
           ELSE IF SP-EXPR-REDU-FRMLA = "A"
              MOVE " LOW OF EXPIR/REDU" TO S1-3-DESC-A.
           PERFORM WRITE-DETAIL-LINE.


      ***********************************
      *    PAGE 23  REPO FORMS & MISC
      ***********************************
       PRINT-PAGE-23.
           IF SPR-PAGE-NUMBER NOT = 23 AND NOT = 99
              GO TO END-DETAIL-LN-PROCESS.

           MOVE "PAGE 23" TO H-PG1.       
           PERFORM NEW-PAGE.
           MOVE ALL "-" TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DISPLAY % REGPY AVAIL"    TO S23-RIGHT-FLD1.
           MOVE ": "                       TO S23-RIGHT-FLD2.
           MOVE SP-PERCENT-REGPAY-AVAIL    TO S23-R-PERCENT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPO TRCD (1)  "          TO S23-LEFT-FLD1.
           MOVE ": "                       TO S23-LEFT-FLD2.
           MOVE SP-REPO-TRCD(1)            TO S23-TRCD.
           MOVE SP-REPO-FMNO(1,1)          TO S23-FMNO.
           MOVE "NON-LOAN FLAG"            TO S23-RIGHT-FLD1.
           MOVE ": "                       TO S23-RIGHT-FLD2.
           MOVE SP-NON-LOAN-FLAG           TO S23-RIGHT-FLD3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SP-REPO-FMNO(1,2)          TO S23-FMNO.
           MOVE "SKIP CREDIT BUREAU"       TO S23-RIGHT-FLD1.
           MOVE ": "                       TO S23-RIGHT-FLD2.
           MOVE SP-SKIP-TRW                TO S23-RIGHT-FLD3.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SP-REPO-FMNO(1,3)          TO S23-FMNO.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SP-REPO-FMNO(1,4)          TO S23-FMNO.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPO TRCD (2)  "          TO S23-LEFT-FLD1.
           MOVE ": "                       TO S23-LEFT-FLD2.
           MOVE SP-AUTO-SEND-TRCD(1)       TO S23-TRCD.
           MOVE SP-AUTO-SEND-FMNO(1,1)     TO S23-FMNO.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SP-AUTO-SEND-FMNO(1,2)     TO S23-FMNO.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SP-AUTO-SEND-FMNO(1,3)     TO S23-FMNO.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SP-AUTO-SEND-FMNO(1,4)     TO S23-FMNO.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "REPO TRCD (3)  "          TO S23-LEFT-FLD1.
           MOVE ": "                       TO S23-LEFT-FLD2.
           MOVE SP-AUTO-SEND-TRCD(2)       TO S23-TRCD.
           MOVE SP-AUTO-SEND-FMNO(2,1)     TO S23-FMNO.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SP-AUTO-SEND-FMNO(2,2)     TO S23-FMNO.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SP-AUTO-SEND-FMNO(2,3)     TO S23-FMNO.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SP-AUTO-SEND-FMNO(2,4)     TO S23-FMNO.
           PERFORM WRITE-DETAIL-LINE.

       END-DETAIL-LN-PROCESS.
           EXIT.


      ***************************************
       PRINT-INS-REB-TABLE2 SECTION.
      ***************************************

      * PRINT INS REBATE TABLE2  (PAGE 13) *
           ADD 1 TO SUB.
           MOVE SUB TO RB-NUM.
           MOVE "." TO RB-PERIOD.
           MOVE INS-ITEM (SUB) TO RB-DESCRIPT.
           MOVE SP-RBDAYS (SUB,1) TO RB-ADDAYS1.
           MOVE SP-RBDTE (SUB,1) TO RB-REFDATE1.
           MOVE SP-RBYRTYPE (SUB,1) TO RB-YEARTYPE1.
           MOVE SP-RBDAYS (SUB,2) TO RB-ADDAYS2.
           MOVE SP-RBDTE (SUB,2) TO RB-REFDATE2.
           MOVE SP-RBYRTYPE (SUB,2) TO RB-YEARTYPE2.
           MOVE SP-RBRECAST (SUB) TO RB-RECAST.
           MOVE SP-RBSPOPT2 (SUB) TO RB-OPT2.
           MOVE SP-RBMANAGER (SUB) TO RB-MANAGER.
           MOVE SP-RBUNITPER-CD (SUB) TO RB-UNITPERCD.
           PERFORM WRITE-DETAIL-LINE.

      ***********************************
       PRINT-REBATE-TABLE SECTION.
      ***********************************

      * PRINT REBATE TABLE  (PAGE 12)  *
           ADD 1 TO SUB.
           MOVE SUB TO REB-TAB-NUM.
           MOVE "." TO REB-PERIOD.
           MOVE INS-ITEM (SUB) TO REB-DESC.
           IF SUB = 15
              MOVE SP-RBCOMCD TO REB-FRMLA
              GO TO PRINT-REBATE-TABLE-EXIT.

           MOVE SP-RBFRMLA (SUB) TO REB-FRMLA.
           MOVE SP-RBFRMLA2 (SUB) TO REB-FRMLA2.

           MOVE SP-RBMINRET (SUB) TO REB-MIN-RET.
           MOVE SP-RBNOMON (SUB) TO REB-MIN-MON.
           MOVE SP-RBDEDCHG (SUB) TO REB-DED-CHG.
           MOVE SP-RBSPOPT1 (SUB) TO REB-SPOPT1.
           MOVE SP-RBEARLY (SUB) TO REB-EARLY.
           MOVE SP-RBDEFER(SUB) TO REB-DEFER.
           MOVE SP-RBPCT (SUB) TO REB-PCT.
           MOVE SP-RBREDUC (SUB) TO REB-REDUCT.
           MOVE SP-RBMIN (SUB) TO REB-MIN.

       PRINT-REBATE-TABLE-EXIT.
           PERFORM WRITE-DETAIL-LINE.

      *****************************************
       PRINT-CAL-TBL SECTION.
      *****************************************
           MOVE CSTP TO TAB-NUM1.
           MOVE "." TO CAL-PERIOD1.
           MOVE SP-CAL-RATE (CTBL,CSTP) TO CAL-RATE1.
           MOVE SP-CAL-BREAK (CTBL,CSTP) TO CAL-AMT1.
           MOVE CSTP TO TAB-NUM2.
           MOVE "." TO CAL-PERIOD2.
           SET CTBL UP BY 1.
           MOVE SP-CAL-RATE (CTBL,CSTP) TO CAL-RATE2.
           MOVE SP-CAL-BREAK (CTBL,CSTP) TO CAL-AMT2.
           PERFORM WRITE-DETAIL-LINE.
           SET CTBL TO 1.
           SET CSTP UP BY 1.

      **************************************
       PRINT-EDIT-TABLES SECTION.
      **************************************
           PERFORM PRINT-EDT1-TBL.
           PERFORM PRINT-EDT2-TBL.
           PERFORM EDIT-CODES.

      **************************************
       PRINT-INS-TABLE SECTION.
      **************************************
           MOVE "** MIN  MAX  VERIFY  MAX" TO INS-TAG2.
           MOVE "**           TERM     RATE" TO INS-TAG3.
           MOVE "TERM     RATE" TO INS-TAG4.
           MOVE INS-HEAD1 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO INS-HEAD1.
           MOVE "AGE  AGE  MAXCOV  TRM" TO I-HEAD2.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "2" TO INS-NO.
           MOVE SP-INSTERM (ILEV,2) TO TERM.
           MOVE SP-INSRATE (ILEV,2) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INS COMPANY" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSCOM (ILEV) TO I-EX.
           MOVE SP-INSMINAGE (ILEV,1) TO INS-MINAGE.
           MOVE SP-INSMAXAGE (ILEV,1) TO INS-MAXAGE.
           MOVE SP-INSMAXCOV (ILEV,1) TO INS-MAX-COV.
           MOVE SP-INSMAXTRM (ILEV,1) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "3" TO INS-NO.
           MOVE SP-INSTERM (ILEV,3) TO TERM.
           MOVE SP-INSRATE (ILEV,3) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RATE CODE" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSRATECD (ILEV) TO I-RTE.
           IF SP-INSRATECD (ILEV) = 01
              MOVE " RATE/$100/ANNUM" TO I-CHG-DESC
           ELSE IF SP-INSRATECD (ILEV) = 02
              MOVE " RATE/$1000/MONTH" TO I-CHG-DESC
           ELSE IF SP-INSRATECD (ILEV) = 03
               MOVE " RATE/$100/TERM" TO I-CHG-DESC
           ELSE IF SP-INSRATECD (ILEV) = 04
              MOVE " RATE/$100/TERM" TO I-CHG-DESC.
           MOVE SP-INSMINAGE (ILEV,2) TO INS-MINAGE.
           MOVE SP-INSMAXAGE (ILEV,2) TO INS-MAXAGE.
           MOVE SP-INSMAXCOV (ILEV,2) TO INS-MAX-COV.
           MOVE SP-INSMAXTRM (ILEV,2) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "4" TO INS-NO.
           MOVE SP-INSTERM (ILEV,4) TO TERM.
           MOVE SP-INSRATE (ILEV,4) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEVIATION FACR" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSJFAC (ILEV) TO I-DEVIAT.
           MOVE SP-INSMINAGE (ILEV,3) TO INS-MINAGE.
           MOVE SP-INSMAXAGE (ILEV,3) TO INS-MAXAGE.
           MOVE SP-INSMAXCOV (ILEV,3) TO INS-MAX-COV.
           MOVE SP-INSMAXTRM (ILEV,3) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "5" TO INS-NO.
           MOVE SP-INSTERM (ILEV,5) TO TERM.
           MOVE SP-INSRATE (ILEV,5) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DISCOUNT" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSDISCNT (ILEV) TO I-INSRATE.
           MOVE SP-INSMINAGE (ILEV,4) TO INS-MINAGE.
           MOVE SP-INSMAXAGE (ILEV,4) TO INS-MAXAGE.
           MOVE SP-INSMAXCOV (ILEV,4) TO INS-MAX-COV.
           MOVE SP-INSMAXTRM (ILEV,4) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "6" TO INS-NO.
           MOVE SP-INSTERM (ILEV,6) TO TERM.
           MOVE SP-INSRATE (ILEV,6) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TERM" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSTERM (ILEV,1) TO INS-TM.
           MOVE SP-INSMINAGE (ILEV,5) TO INS-MINAGE.
           MOVE SP-INSMAXAGE (ILEV,5) TO INS-MAXAGE.
           MOVE SP-INSMAXCOV (ILEV,5) TO INS-MAX-COV.
           MOVE SP-INSMAXTRM (ILEV,5) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "7" TO INS-NO.
           MOVE SP-INSTERM (ILEV,7) TO TERM.
           MOVE SP-INSRATE (ILEV,7) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RATE" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSRATE (ILEV,1) TO I-INSRATE.
      *
      *    MOVE SP-INSMINAGE (ILEV,6) TO INS-MINAGE.
      *    MOVE SP-INSMAXAGE (ILEV,6) TO INS-MAXAGE.
      *    MOVE SP-INSMAXCOV (ILEV,6) TO INS-MAX-COV.
      *    MOVE SP-INSMAXTRM (ILEV,6) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "8" TO INS-NO.
           MOVE SP-INSTERM (ILEV,8) TO TERM.
           MOVE SP-INSRATE (ILEV,8) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHGABLE CD/FG" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSCHGABL (ILEV) TO I-CHG.
           IF SP-INSCHGABL (ILEV) = "P" OR "S"
              MOVE " PRIN= TOTNOT-INT-SC" TO I-CHG-DESC
           ELSE IF SP-INSCHGABL (ILEV) = "N"
              MOVE " TOTAL NOTE" TO I-CHG-DESC
           ELSE IF SP-INSCHGABL (ILEV) = "C"
              MOVE " CSH=NOTE-INT-SC-INS" TO I-CHG-DESC
           ELSE IF SP-INSCHGABL (ILEV) = "X"
              MOVE " SP CSH=NOTE-INT-INS" TO I-CHG-DESC
           ELSE IF SP-INSCHGABL (ILEV) = "M"
              MOVE " TOTAL NOTE + EXT" TO I-CHG-DESC
           ELSE IF SP-INSCHGABL (ILEV) = "R"
              MOVE " TOTAL REPAY" TO I-CHG-DESC
           ELSE IF SP-INSCHGABL (ILEV) = "A"
              MOVE " TOTAL-NOTE - INTEREST" TO I-CHG-DESC
           ELSE IF SP-INSCHGABL (ILEV) = "I"
              MOVE "TOTAL-NOTE - INT*(1+I)" TO I-CHG-DESC.
           MOVE SP-INSCHGABL-FG (ILEV) TO I-CHG-FG.
      *    MOVE SP-INSMINAGE (ILEV,7) TO INS-MINAGE.
      *    MOVE SP-INSMAXAGE (ILEV,7) TO INS-MAXAGE.
      *    MOVE SP-INSMAXCOV (ILEV,7) TO INS-MAX-COV.
      *    MOVE SP-INSMAXTRM (ILEV,7) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "9" TO INS-NO.
           MOVE SP-INSTERM (ILEV,9) TO TERM.
           MOVE SP-INSRATE (ILEV,9) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MIN CHGABLE" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSMINCHGABL (ILEV) TO I-DEVIAT.
      *    MOVE SP-INSMINAGE (ILEV,8) TO INS-MINAGE.
      *    MOVE SP-INSMAXAGE (ILEV,8) TO INS-MAXAGE.
      *    MOVE SP-INSMAXCOV (ILEV,8) TO INS-MAX-COV.
      *    MOVE SP-INSMAXTRM (ILEV,8) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "10" TO INS-NO.
           MOVE SP-INSTERM (ILEV,10) TO TERM.
           MOVE SP-INSRATE (ILEV,10) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MAX CHGABLE" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSMAXCHGABL (ILEV) TO I-MIN-MAX.
      *    MOVE SP-INSMINAGE (ILEV,9) TO INS-MINAGE.
      *    MOVE SP-INSMAXAGE (ILEV,9) TO INS-MAXAGE.
      *    MOVE SP-INSMAXCOV (ILEV,9) TO INS-MAX-COV.
      *    MOVE SP-INSMAXTRM (ILEV,9) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "11" TO INS-NO.
           MOVE SP-INSTERM (ILEV,11) TO TERM.
           MOVE SP-INSRATE (ILEV,11) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MIN PREMIUM" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSMINPREM (ILEV) TO I-MIN-MAX.
      *    MOVE SP-INSMINAGE (ILEV,10) TO INS-MINAGE.
      *    MOVE SP-INSMAXAGE (ILEV,10) TO INS-MAXAGE.
      *    MOVE SP-INSMAXCOV (ILEV,10) TO INS-MAX-COV.
      *    MOVE SP-INSMAXTRM (ILEV,10) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "12" TO INS-NO.
           MOVE SP-INSTERM (ILEV,12) TO TERM.
           MOVE SP-INSRATE (ILEV,12) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           IF ILEV = 5 OR 8
              MOVE "MAX BENEFIT" TO INSCO-TAG
           ELSE
              MOVE "N/A" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSMAXBENIF (ILEV) TO I-MIN-MAX.
      *    MOVE SP-INSMINAGE (ILEV,11) TO INS-MINAGE.
      *    MOVE SP-INSMAXAGE (ILEV,11) TO INS-MAXAGE.
      *    MOVE SP-INSMAXCOV (ILEV,11) TO INS-MAX-COV.
      *    MOVE SP-INSMAXTRM (ILEV,11) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "13" TO INS-NO.
           MOVE SP-INSTERM (ILEV,13) TO TERM.
           MOVE SP-INSRATE (ILEV,13) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MAX TERM" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSMAXTERM (ILEV) TO INS-TM.
      *    MOVE SP-INSMINAGE (ILEV,12) TO INS-MINAGE.
      *    MOVE SP-INSMAXAGE (ILEV,12) TO INS-MAXAGE.
      *    MOVE SP-INSMAXCOV (ILEV,12) TO INS-MAX-COV.
      *    MOVE SP-INSMAXTRM (ILEV,12) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "14" TO INS-NO.
           MOVE SP-INSTERM (ILEV,14) TO TERM.
           MOVE SP-INSRATE (ILEV,14) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHG/BEN FG" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSCHGBEN (ILEV) TO I-CHG.
           MOVE "REJECT : " TO I-REJCT-DESC.
           MOVE SP-INSREJECT-FG(ILEV) TO I-REJCT-CDE.
      *    MOVE SP-INSMINAGE (ILEV,13) TO INS-MINAGE.
      *    MOVE SP-INSMAXAGE (ILEV,13) TO INS-MAXAGE.
      *    MOVE SP-INSMAXCOV (ILEV,13) TO INS-MAX-COV.
      *    MOVE SP-INSMAXTRM (ILEV,13) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "15" TO INS-NO.
           MOVE SP-INSTERM (ILEV,15) TO TERM.
           MOVE SP-INSRATE (ILEV,15) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INSFEE CD/FRMLA" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSFEECD (ILEV) TO I-CHG.
           MOVE SP-INSFEE-FRMLA (ILEV) TO I-CHG-FG.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "16" TO INS-NO.
           MOVE SP-INSTERM (ILEV,16) TO TERM.
           MOVE SP-INSRATE (ILEV,16) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "EXCEPTION" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSEXCEPT (ILEV) TO I-EX.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "17" TO INS-NO.
           MOVE SP-INSTERM (ILEV,17) TO TERM.
           MOVE SP-INSRATE (ILEV,17) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "IS A FINCHG" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSFINCHG (ILEV) TO I-CHG.
      *    MOVE SP-INSMINAGE (ILEV,14) TO INS-MINAGE.
      *    MOVE SP-INSMAXAGE (ILEV,14) TO INS-MAXAGE.
      *    MOVE SP-INSMAXCOV (ILEV,14) TO INS-MAX-COV.
      *    MOVE SP-INSMAXTRM (ILEV,14) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "18" TO INS-NO.
           MOVE SP-INSTERM (ILEV,18) TO TERM.
           MOVE SP-INSRATE (ILEV,18) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "EXTBK" TO INS-DESCR.
           MOVE "19" TO INS-NO.
           MOVE SP-INSTERM (ILEV,19) TO TERM.
           MOVE SP-INSRATE (ILEV,19) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "EXTBK" TO INS-DESCR.
           MOVE "20" TO INS-NO.
           MOVE SP-INSTERM (ILEV,20) TO TERM.
           MOVE SP-INSRATE (ILEV,20) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.


      **************************************
       PRINT-INS-TABLE2 SECTION.
      **************************************
           MOVE "** MIN  MAX  VERIFY  MAX" TO INS-TAG2.
           MOVE "**           TERM     RATE" TO INS-TAG3.
           MOVE "TERM     RATE" TO INS-TAG4.
           MOVE INS-HEAD1 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO INS-HEAD1.
           MOVE "AGE  AGE  MAXCOV  TRM" TO I-HEAD2.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "2" TO INS-NO.
           MOVE SP-INSTERM (ILEV,2) TO TERM.
           MOVE SP-INSRATE (ILEV,2) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INS COMPANY" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSCOM (ILEV) TO I-EX.
           MOVE SP-INSMINAGE (ILEV,1) TO INS-MINAGE.
           MOVE SP-INSMAXAGE (ILEV,1) TO INS-MAXAGE.
           MOVE SP-INSMAXCOV (ILEV,1) TO INS-MAX-COV.
           MOVE SP-INSMAXTRM (ILEV,1) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "3" TO INS-NO.
           MOVE SP-INSTERM (ILEV,3) TO TERM.
           MOVE SP-INSRATE (ILEV,3) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RATE CODE" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSRATECD (ILEV) TO I-RTE.
           IF SP-INSRATECD (ILEV) = 01
              MOVE " RATE/$100/ANNUM" TO I-CHG-DESC
           ELSE IF SP-INSRATECD (ILEV) = 02
              MOVE " RATE/$1000/MONTH" TO I-CHG-DESC
           ELSE IF SP-INSRATECD (ILEV) = 03
               MOVE " RATE/$100/TERM" TO I-CHG-DESC
           ELSE IF SP-INSRATECD (ILEV) = 04
              MOVE " RATE/$100/TERM" TO I-CHG-DESC.
           MOVE SP-INSMINAGE (ILEV,2) TO INS-MINAGE.
           MOVE SP-INSMAXAGE (ILEV,2) TO INS-MAXAGE.
           MOVE SP-INSMAXCOV (ILEV,2) TO INS-MAX-COV.
           MOVE SP-INSMAXTRM (ILEV,2) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "4" TO INS-NO.
           MOVE SP-INSTERM (ILEV,4) TO TERM.
           MOVE SP-INSRATE (ILEV,4) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEVIATION FACR" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSJFAC (ILEV) TO I-DEVIAT.
           MOVE SP-INSMINAGE (ILEV,3) TO INS-MINAGE.
           MOVE SP-INSMAXAGE (ILEV,3) TO INS-MAXAGE.
           MOVE SP-INSMAXCOV (ILEV,3) TO INS-MAX-COV.
           MOVE SP-INSMAXTRM (ILEV,3) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "5" TO INS-NO.
           MOVE SP-INSTERM (ILEV,5) TO TERM.
           MOVE SP-INSRATE (ILEV,5) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DISCOUNT" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSDISCNT (ILEV) TO I-INSRATE.
           MOVE SP-INSMINAGE (ILEV,4) TO INS-MINAGE.
           MOVE SP-INSMAXAGE (ILEV,4) TO INS-MAXAGE.
           MOVE SP-INSMAXCOV (ILEV,4) TO INS-MAX-COV.
           MOVE SP-INSMAXTRM (ILEV,4) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "6" TO INS-NO.
           MOVE SP-INSTERM (ILEV,6) TO TERM.
           MOVE SP-INSRATE (ILEV,6) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TERM" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSTERM (ILEV,1) TO INS-TM.
           MOVE SP-INSMINAGE (ILEV,5) TO INS-MINAGE.
           MOVE SP-INSMAXAGE (ILEV,5) TO INS-MAXAGE.
           MOVE SP-INSMAXCOV (ILEV,5) TO INS-MAX-COV.
           MOVE SP-INSMAXTRM (ILEV,5) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "7" TO INS-NO.
           MOVE SP-INSTERM (ILEV,7) TO TERM.
           MOVE SP-INSRATE (ILEV,7) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RATE" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSRATE (ILEV,1) TO I-INSRATE.
      *    MOVE SP-INSMINAGE (ILEV,6) TO INS-MINAGE.
      *    MOVE SP-INSMAXAGE (ILEV,6) TO INS-MAXAGE.
      *    MOVE SP-INSMAXCOV (ILEV,6) TO INS-MAX-COV.
      *    MOVE SP-INSMAXTRM (ILEV,6) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "8" TO INS-NO.
           MOVE SP-INSTERM (ILEV,8) TO TERM.
           MOVE SP-INSRATE (ILEV,8) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHGABLE CD/FG" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSCHGABL (ILEV) TO I-CHG.
           IF SP-INSCHGABL (ILEV) = "P" OR "S"
              MOVE " PRIN= TOTNOT-INT-SC" TO I-CHG-DESC
           ELSE IF SP-INSCHGABL (ILEV) = "N"
              MOVE " TOTAL NOTE" TO I-CHG-DESC
           ELSE IF SP-INSCHGABL (ILEV) = "C"
              MOVE " CSH=NOTE-INT-SC-INS" TO I-CHG-DESC
           ELSE IF SP-INSCHGABL (ILEV) = "X"
              MOVE " SP CSH=NOTE-INT-INS" TO I-CHG-DESC
           ELSE IF SP-INSCHGABL (ILEV) = "M"
              MOVE " TOTAL NOTE + EXT" TO I-CHG-DESC
           ELSE IF SP-INSCHGABL (ILEV) = "R"
              MOVE " TOTAL REPAY" TO I-CHG-DESC
           ELSE IF SP-INSCHGABL (ILEV) = "A"
              MOVE " TOTAL-NOTE - INTEREST" TO I-CHG-DESC
           ELSE IF SP-INSCHGABL (ILEV) = "I"
              MOVE "TOTAL-NOTE - INT*(1+I)" TO I-CHG-DESC.
           MOVE SP-INSCHGABL-FG (ILEV) TO I-CHG-FG.
      *    MOVE SP-INSMINAGE (ILEV,7) TO INS-MINAGE.
      *    MOVE SP-INSMAXAGE (ILEV,7) TO INS-MAXAGE.
      *    MOVE SP-INSMAXCOV (ILEV,7) TO INS-MAX-COV.
      *    MOVE SP-INSMAXTRM (ILEV,7) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "9" TO INS-NO.
           MOVE SP-INSTERM (ILEV,9) TO TERM.
           MOVE SP-INSRATE (ILEV,9) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MIN CHGABLE" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSMINCHGABL (ILEV) TO I-MIN-MAX.
      *    MOVE SP-INSMINAGE (ILEV,8) TO INS-MINAGE.
      *    MOVE SP-INSMAXAGE (ILEV,8) TO INS-MAXAGE.
      *    MOVE SP-INSMAXCOV (ILEV,8) TO INS-MAX-COV.
      *    MOVE SP-INSMAXTRM (ILEV,8) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "10" TO INS-NO.
           MOVE SP-INSTERM (ILEV,10) TO TERM.
           MOVE SP-INSRATE (ILEV,10) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MAX CHGABLE" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSMAXCHGABL (ILEV) TO I-MIN-MAX.
      *    MOVE SP-INSMINAGE (ILEV,9) TO INS-MINAGE.
      *    MOVE SP-INSMAXAGE (ILEV,9) TO INS-MAXAGE.
      *    MOVE SP-INSMAXCOV (ILEV,9) TO INS-MAX-COV.
      *    MOVE SP-INSMAXTRM (ILEV,9) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "11" TO INS-NO.
           MOVE SP-INSTERM (ILEV,11) TO TERM.
           MOVE SP-INSRATE (ILEV,11) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MIN PREMIUM" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSMINPREM (ILEV) TO I-DEVIAT.
      *    MOVE SP-INSMINAGE (ILEV,10) TO INS-MINAGE.
      *    MOVE SP-INSMAXAGE (ILEV,10) TO INS-MAXAGE.
      *    MOVE SP-INSMAXCOV (ILEV,10) TO INS-MAX-COV.
      *    MOVE SP-INSMAXTRM (ILEV,10) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "12" TO INS-NO.
           MOVE SP-INSTERM (ILEV,12) TO TERM.
           MOVE SP-INSRATE (ILEV,12) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           IF ILEV = 5
              MOVE "MAX BENEFIT" TO INSCO-TAG
           ELSE
              MOVE "N/A" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSMAXBENIF (ILEV) TO I-MIN-MAX.
      *    MOVE SP-INSMINAGE (ILEV,11) TO INS-MINAGE.
      *    MOVE SP-INSMAXAGE (ILEV,11) TO INS-MAXAGE.
      *    MOVE SP-INSMAXCOV (ILEV,11) TO INS-MAX-COV.
      *    MOVE SP-INSMAXTRM (ILEV,11) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "13" TO INS-NO.
           MOVE SP-INSTERM (ILEV,13) TO TERM.
           MOVE SP-INSRATE (ILEV,13) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MAX TERM" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSMAXTERM (ILEV) TO INS-TM.
      *    MOVE SP-INSMINAGE (ILEV,12) TO INS-MINAGE.
      *    MOVE SP-INSMAXAGE (ILEV,12) TO INS-MAXAGE.
      *    MOVE SP-INSMAXCOV (ILEV,12) TO INS-MAX-COV.
      *    MOVE SP-INSMAXTRM (ILEV,12) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "14" TO INS-NO.
           MOVE SP-INSTERM (ILEV,14) TO TERM.
           MOVE SP-INSRATE (ILEV,14) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHG/BEN FG" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSCHGBEN (ILEV) TO I-CHG.
           MOVE "REJECT : " TO I-REJCT-DESC.
           MOVE SP-INSREJECT-FG(ILEV) TO I-REJCT-CDE.
      *    MOVE SP-INSMINAGE (ILEV,13) TO INS-MINAGE.
      *    MOVE SP-INSMAXAGE (ILEV,13) TO INS-MAXAGE.
      *    MOVE SP-INSMAXCOV (ILEV,13) TO INS-MAX-COV.
      *    MOVE SP-INSMAXTRM (ILEV,13) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "15" TO INS-NO.
           MOVE SP-INSTERM (ILEV,15) TO TERM.
           MOVE SP-INSRATE (ILEV,15) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INSFEE CD/FRMLA" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSFEECD (ILEV) TO I-CHG.
           MOVE SP-INSFEE-FRMLA (ILEV) TO I-CHG-FG.
      *    MOVE SP-INSMINAGE (ILEV,14) TO INS-MINAGE.
      *    MOVE SP-INSMAXAGE (ILEV,14) TO INS-MAXAGE.
      *    MOVE SP-INSMAXCOV (ILEV,14) TO INS-MAX-COV.
      *    MOVE SP-INSMAXTRM (ILEV,14) TO INS-MAX-TRM.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "16" TO INS-NO.
           MOVE SP-INSTERM (ILEV,16) TO TERM.
           MOVE SP-INSRATE (ILEV,16) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "EXCEPTION" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSEXCEPT (ILEV) TO I-EX.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "17" TO INS-NO.
           MOVE SP-INSTERM (ILEV,17) TO TERM.
           MOVE SP-INSRATE (ILEV,17) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "JOINT FLAG " TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSAH-JOINT-FG TO I-CHG.
           MOVE "RETRO  : " TO I-REJCT-DESC.
           MOVE SP-INSAH-RETRO TO I-REJCT-CDE.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "18" TO INS-NO.
           MOVE SP-INSTERM (ILEV,18) TO TERM.
           MOVE SP-INSRATE (ILEV,18) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DAYS WAIT" TO INSCO-TAG.
           MOVE ":" TO I-COLON.
           MOVE SP-INSAH-DAYS-WAIT TO I-CHG.
           MOVE "FINCHG :" TO I-REJCT-DESC.
           MOVE SP-INSFINCHG (ILEV) TO I-REJCT-CDE.
           MOVE "EXTBK" TO INS-DESCR.
           MOVE "19" TO INS-NO.
           MOVE SP-INSTERM (ILEV,19) TO TERM.
           MOVE SP-INSRATE (ILEV,19) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "EXTBK" TO INS-DESCR.
           MOVE "20" TO INS-NO.
           MOVE SP-INSTERM (ILEV,20) TO TERM.
           MOVE SP-INSRATE (ILEV,20) TO RATE-INS.
           PERFORM WRITE-DETAIL-LINE.


      **************************************
       PRINT-EDT1-TBL SECTION.
      **************************************
           MOVE ESTP TO EDIT-NUM1.
           MOVE "." TO EDIT-PERIOD1.
           MOVE SP-EDT-TYPE (ETBL,ESTP) TO CODE1.
           MOVE SP-EDT-HL (ETBL,ESTP) TO CODE2.
           MOVE SP-EDT-COLL (ETBL,ESTP) TO CODE3.
           MOVE SP-EDT-CD (ETBL,ESTP) TO CODE4.
           MOVE SP-EDT-RATETYPE (ETBL,ESTP) TO RATE-TYP1.
           MOVE SP-EDT-RATE (ETBL,ESTP) TO RATE1.
           MOVE SP-EDT-AMTYPE (ETBL,ESTP) TO AMT-TYP1.
           MOVE SP-EDT-AMT (ETBL,ESTP) TO AMT1.
           MOVE SP-EDT-TERM (ETBL,ESTP) TO TERM1.

      *******************************************
       PRINT-EDT2-TBL SECTION.
      *******************************************
           MOVE ESTP TO EDIT-NUM2.
           MOVE "." TO EDIT-PERIOD2.
           SET ETBL UP BY 1.
           MOVE SP-EDT-TYPE (ETBL,ESTP) TO CD1.
           MOVE SP-EDT-HL (ETBL,ESTP) TO CD2.
           MOVE SP-EDT-COLL (ETBL,ESTP) TO CD3.
           MOVE SP-EDT-CD (ETBL,ESTP) TO CD4.
           MOVE SP-EDT-RATETYPE (ETBL,ESTP) TO RATE-TYP2.
           MOVE SP-EDT-RATE (ETBL,ESTP) TO RATE2.
           MOVE SP-EDT-AMTYPE (ETBL,ESTP) TO AMT-TYP2.
           MOVE SP-EDT-AMT (ETBL,ESTP) TO AMT2.
           MOVE SP-EDT-TERM (ETBL,ESTP) TO TERM2.
           PERFORM WRITE-DETAIL-LINE.
           SET ETBL TO 1.

      ******************************************
       EDIT-CODES SECTION.
      ******************************************
           IF SP-EDT-TYPE (ETBL,ESTP) = "R"
              MOVE "RATE" TO DESC-CODE1
           ELSE IF SP-EDT-TYPE (ETBL,ESTP) = "A"
              MOVE "AMT" TO DESC-CODE1
           ELSE IF SP-EDT-TYPE (ETBL,ESTP) = "T"
              MOVE "TERM" TO DESC-CODE1
           ELSE IF SP-EDT-TYPE (ETBL,ESTP) = "P"
              MOVE "POINTS" TO DESC-CODE1
           ELSE IF SP-EDT-TYPE (ETBL,ESTP) = "M"
               MOVE "MAX MATUR" TO DESC-CODE1.
           IF SP-EDT-HL (ETBL,ESTP) = "H"
              MOVE "HIGH" TO DESC-CODE2
           ELSE IF SP-EDT-HL (ETBL,ESTP) = "L"
              MOVE "LOW" TO DESC-CODE2.
           IF (SP-EDT-COLL (ETBL,ESTP) = " ") AND
            (SP-EDT-TYPE (ETBL,ESTP) NOT = " ") AND
            (SP-EDT-HL (ETBL,ESTP) NOT = " ")
              MOVE "ALL COLLAT" TO DESC-CODE3
           ELSE IF SP-EDT-COLL (ETBL,ESTP) = "S "
              MOVE "SECURED" TO DESC-CODE3
           ELSE IF SP-EDT-COLL (ETBL,ESTP) = "U "
              MOVE "UNSECURED" TO DESC-CODE3
           ELSE IF SP-EDT-COLL (ETBL,ESTP) = "R"
              MOVE "REALESTAT" TO DESC-CODE3.
           IF SP-EDT-RATETYPE (ETBL,ESTP) = "A"
              MOVE "APR" TO RATE-TYP1
           ELSE IF SP-EDT-RATETYPE (ETBL,ESTP) = "E"
              MOVE "EFFDTE" TO RATE-TYP1.
           IF SP-EDT-AMTYPE (ETBL,ESTP) = "N"
              MOVE "TOTNOT" TO AMT-TYP1
           ELSE IF SP-EDT-AMTYPE (ETBL,ESTP) = "P"
              MOVE "PRINCP" TO AMT-TYP1
           ELSE IF SP-EDT-AMTYPE (ETBL,ESTP) = "C"
              MOVE "CSHADV" TO AMT-TYP1
           ELSE IF SP-EDT-AMTYPE (ETBL,ESTP) = "X"
              MOVE "SPECL" TO AMT-TYP1.
           SET ETBL UP BY 1.
           IF SP-EDT-TYPE (ETBL,ESTP) = "R"
              MOVE "RTE" TO DESC-CD1
           ELSE IF SP-EDT-TYPE (ETBL,ESTP) = "A"
              MOVE "AMT" TO DESC-CD1
           ELSE IF SP-EDT-TYPE (ETBL,ESTP) = "T"
              MOVE "TRM" TO DESC-CD1
           ELSE IF SP-EDT-TYPE (ETBL,ESTP) = "M"
               MOVE "MAX MATUR" TO DESC-CD1.
           IF SP-EDT-HL (ETBL,ESTP) = "H"
              MOVE "HIGH" TO DESC-CD2
           ELSE IF SP-EDT-HL (ETBL,ESTP) = "L"
              MOVE "LOW" TO DESC-CD2.
           IF (SP-EDT-COLL (ETBL,ESTP) = " ") AND
            (SP-EDT-TYPE (ETBL,ESTP) NOT = " ") AND
            (SP-EDT-HL (ETBL,ESTP) NOT = " ")
              MOVE "ALL COLLAT" TO DESC-CD3
           ELSE IF SP-EDT-COLL (ETBL,ESTP) = "S "
              MOVE "SECURED" TO DESC-CD3
           ELSE IF SP-EDT-COLL (ETBL,ESTP) = "U "
              MOVE "UNSECURED" TO DESC-CD3
           ELSE IF SP-EDT-COLL (ETBL,ESTP) = "R"
              MOVE "REALESTAT" TO DESC-CD3.
           IF SP-EDT-RATETYPE (ETBL,ESTP) = "A"
              MOVE "APR" TO RATE-TYP2
           ELSE IF SP-EDT-RATETYPE (ETBL,ESTP) = "E"
              MOVE "EFFDTE" TO RATE-TYP2.
           IF SP-EDT-AMTYPE (ETBL,ESTP) = "N"
              MOVE "TOTNOT" TO AMT-TYP2
           ELSE IF SP-EDT-AMTYPE (ETBL,ESTP) = "P"
              MOVE "PRINCP" TO AMT-TYP2
           ELSE IF SP-EDT-AMTYPE (ETBL,ESTP) = "C"
              MOVE "CSHADV" TO AMT-TYP2
           ELSE IF SP-EDT-AMTYPE (ETBL,ESTP) = "X"
              MOVE "SPECL" TO AMT-TYP2.
           PERFORM WRITE-DETAIL-LINE.
           SET ETBL TO 1.
           SET ESTP UP BY 1.

      ****************************
       PRINT-LC SECTION.
      ****************************
           MOVE SPACES TO HEAD3.
           MOVE "LOAN CLASS" TO H-TAG.
           MOVE ":" TO H-COLON.
           MOVE SP-SPRCLASS TO H-CLASS LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-FG NOT = 0
               MOVE "**NOT ON FILE**" TO H-DESC
               GO TO END-PRINT-LC.
           MOVE LC-DESC TO H-DESC.
           MOVE HEAD3 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO HEAD3.
           MOVE "LOAN SUB-CLASS" TO H-TAG.
           MOVE ":" TO H-COLON.
           MOVE SP-SUBCLASS TO H-CLASS.
           MOVE SP-DESC     TO H-DESC.
           MOVE SP-NOTES    TO H-NOTES.
           MOVE HEAD3 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
       END-PRINT-LC.
           EXIT.

      ****************************************
       PRINT-HEAD4 SECTION.
      ****************************************
           MOVE HEAD4 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SPACES TO HEAD4.
           MOVE ALL "-" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      ********************************
       PRINT-HEAD6 SECTION.
      ********************************
           MOVE "RATE CODE " TO H6-DESC1.
           MOVE "INSURANCE RATE TABLE" TO H6-DESC2.
           MOVE HEAD6 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE HEAD7 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      ****************************************
       FORM-TABLE SECTION.
      ****************************************
           ADD 1 TO SUB.
           IF SUB > 20
              GO TO FORM-TABLE-EXIT.
           IF SP-PRFORM (SUB) = 0
              MOVE "N/A" TO S2-FLD7
              GO TO FORM-TABLE-EXIT.
           MOVE SP-PRFORM (SUB) TO S2-FLD5 FD-FMNO.
           PERFORM READ-FD1-FILE.
           IF IO-FG NOT = 0
              IF SUB < 11
                 MOVE "* NOT ON FILE *" TO FD-DESC
              ELSE
                 MOVE SPACES TO S2-FLD5 FD-DESC.
           MOVE FD-DESC TO S2-FLD7.
           MOVE SP-PRFLAG (SUB) TO S2-FLD9.
       FORM-TABLE-EXIT.
           EXIT.

      ****************************
       PRINT-EXP-HEAD SECTION.
      ****************************
           MOVE ALL "-" TO EXP-HD1.
           MOVE " EXPIRED LOAN FIELDS " TO EXP-HD2.
           MOVE ALL "-" TO EXP-HD3.
           MOVE EXPIRED-HEAD TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *******************************
       INS-RATE-TABLE-LIST SECTION.
      *******************************
           PERFORM INIT-TEMP-TBL
              VARYING TDEX FROM 1 BY 1
                 UNTIL TDEX > 99.
           PERFORM LOAD-TEMP-TBL
              VARYING ILEV FROM 1 BY 1
                 UNTIL ILEV > 7.
           PERFORM SCAN-TEMP-TBL
              VARYING TDEX FROM 1 BY 1
                 UNTIL TDEX > 99.

      ****************************
       INIT-TEMP-TBL SECTION.
      ****************************
           MOVE " " TO TEMP-TBL(TDEX).

      ****************************
       LOAD-TEMP-TBL SECTION.
      ****************************
           IF SP-INSRATECD(ILEV) > 70  AND SP-INSRATECD(ILEV) < 100
              MOVE SP-INSRATECD(ILEV) TO TDEX       
              MOVE "X" TO TEMP-TBL(TDEX).

      ****************************
       SCAN-TEMP-TBL SECTION.
      ****************************
           IF TEMP-TBL(TDEX) = "X"
              MOVE TDEX TO H6-RATE
              MOVE 99 TO LN-FEED
              PERFORM PRINT-HEAD6
              MOVE TDEX TO IR-KEY
              SUBTRACT 70 FROM IR-KEY
              PERFORM READ-IR-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR
              ELSE
                 PERFORM LIST-INS-RATE-TBL
                    VARYING KOUNT FROM 1 BY 1
                       UNTIL KOUNT > 20.

      ****************************
       LIST-INS-RATE-TBL SECTION.
      ****************************
           SET IRIX TO KOUNT.
           SET PRINT-SUB TO IRIX.
           MOVE PRINT-SUB TO D-NUM-1.
           MOVE IR-INSRATE(IRIX) TO D-RATE-1.
           SET IRIX UP BY 20.
           SET PRINT-SUB TO IRIX.
           MOVE PRINT-SUB TO D-NUM-2.
           MOVE IR-INSRATE(IRIX) TO D-RATE-2.
           SET IRIX UP BY 20.
           SET PRINT-SUB TO IRIX.
           MOVE PRINT-SUB TO D-NUM-3.
           MOVE IR-INSRATE(IRIX) TO D-RATE-3.
           SET IRIX UP BY 20.
           SET PRINT-SUB TO IRIX.
           MOVE PRINT-SUB TO D-NUM-4.
           MOVE IR-INSRATE(IRIX) TO D-RATE-4.
           SET IRIX UP BY 20.
           SET PRINT-SUB TO IRIX.
           MOVE PRINT-SUB TO D-NUM-5.
           MOVE IR-INSRATE(IRIX) TO D-RATE-5.
           SET IRIX UP BY 20.
           SET PRINT-SUB TO IRIX.
           MOVE PRINT-SUB TO D-NUM-6.
           MOVE IR-INSRATE(IRIX) TO D-RATE-6.
           SET IRIX UP BY 20.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       WRITE-SUMMARY-LINE SECTION.
      ******************************************************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF ( LN-WK GREATER THAN EXT-CONAME-RPT-LINES )
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

       EXIT-WRITE-SUM-LINE.
           EXIT.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *************************************
       NEW-PAGE SECTION.
      *************************************
           MOVE 99 TO LN-FEED.
           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE SPACES TO HEAD4.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           IF ( SUM-ONLY = "Y" )
              MOVE HEAD3A TO PR-REC
           ELSE
              MOVE SPACES TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE ALL "-" TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           IF ( SUM-ONLY = "Y" )
              MOVE 1 TO LN-FEED
              MOVE 5 TO LN-COUNT
           ELSE
              MOVE 2 TO LN-FEED
              MOVE 1 TO LN-COUNT.

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
               MOVE "SNNN020LC1." TO VDU
           ELSE
               MOVE "SNNN020LC2." TO VDU.

           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.

           MOVE 0 TO IO-FG.
       EXIT-CLASS-SCAN.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO SPLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPLIST-SYSDATE.
           DISPLAY SPLIST-SCREEN UPON SPLIST-WINDOW-HANDLE.
           MOVE SPLIST-FORM-FIELDS TO WR-BUF.
           MOVE SPLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE SPLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/SPLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************************
       MISC SECTION.
      ***********************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       PRINT-RANGE-LINES.
           MOVE 3 TO LN-FEED.
           MOVE "BEGINNING STATE CODE, CLASS, SUB-CLASS, LAW CODE"
                TO RANGE-LINE.
           MOVE BEG-SP-ORGST TO R-SP-ORGST.
           MOVE BEG-SP-SPRCLASS TO R-SP-SPRCLASS.
           MOVE BEG-SP-SUBCLASS TO R-SP-SUBCLASS.
           MOVE BEG-SP-LAWCODE TO R-SP-LAWCODE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING STATE CODE, CLASS, SUB-CLASS, LAW CODE"
                TO RANGE-LINE.
           MOVE END-SP-ORGST TO R-SP-ORGST.
           MOVE END-SP-SPRCLASS TO R-SP-SPRCLASS.
           MOVE END-SP-SUBCLASS TO R-SP-SUBCLASS.
           MOVE END-SP-LAWCODE TO R-SP-LAWCODE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SUB-CLASS SUMMARY ONLY" TO RANGE-LINE.
           MOVE SUM-ONLY TO R-SP-ORGST.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NON-EXPIRED SPR'S ONLY" TO RANGE-LINE.
           MOVE NON-EXPIRED-ONLY TO R-SP-ORGST.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SPR PAGE NUMBER" TO RANGE-LINE.
           MOVE SPR-PAGE-NUMBER TO R-SP-ORGST.
           PERFORM WRITE-DETAIL-LINE.

      *****************************
       IO-ROUTINE SECTION.
      *****************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPIRGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPIRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***********************************************************
       END-PROGRAM SECTION.
      ***********************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE!" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           MOVE "LP/LPL2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY SPLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
