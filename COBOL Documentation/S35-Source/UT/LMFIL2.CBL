       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LMFILE.
       DATE-WRITTEN.  11-01-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 LMFILE WITH A15 LMFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  LOOPS THRU THE A15 FILE AND VERIFIES THE S35 FILE EXIST AND MATCHES
      *
      * * * NEED TO TAKE OUT THE C$JUSTIFY STATEMENTS AFTER BRIAN REMOVES
      *     THE "TRIM" STATEMENT FROM THE ASCII LOAD
      * * *
      *  
      *  
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *COPY "LIBLP/LPFSLN.CPY".
      * COPYMEMBER: LIBLP/LPFSLN
           SELECT LN-FILE ASSIGN TO LN-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY LN1-KEY
                  ALTERNATE RECORD KEY LN2-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY LN3-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY LN4-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY LN5-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY LN6-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY LN7-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.
  
       COPY "LIBGB/GBFSPR.CPY".

      * COPYMEMBER: LIBLP/LPFSLM
           SELECT OLM-FILE ASSIGN TO OLM-PATH
                  WITH COMPRESSION
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OLM1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      * COPY "LIBLP/LPFDLN.CPY".
      * COPYMEMBER: LIBLP/LPFDLN
      ********************************************************************
      *   LOAN MASTER FILE
      *
      ********************************************************************
      $XFD  FILE=LNFILE
       FD  LN-FILE
           LABEL RECORDS ARE STANDARD.
      * COPY "LIBLP/LP01LN".
      * COPYMEMBER: LIBLP/LP01LN
      ********************************************************************
      *   LOAN MASTER FILE       1800  BYTES
      *  
      *   KEY 1 - ACCOUNT
      *   KEY 2 - CLASS, ACCOUNT
      *   KEY 3 - BRANCH, CLASS, ACCOUNT#
      *   KEY 4 - BORROWER(1)
      *   KEY 5 - BORROWER(2)
      *   KEY 6 - BORROWER(3)
      *   KEY 7 - BORROWER(4)
      ********************************************************************
      *   I M P O R T A N T - - - - - - - -         
      *       REMEMBER TO CHANGE:
      *       LPFDPD
      *       (AND ALL PLACES FIELDS ARE INITIALIZED)   
      ********************************************************************
      * REV:
      *  JTG 061995 ADDED LN-O3INS-DLPROC
      *  JTG 062195 ADDED CODES FOR CALCULATION OF O1 INSURANCE
      *  JTG 070995 ADDED LN-INSPREM-DLADJ, ADJUSTMENT WHEN DEALER
      *                   WROTE OUR INS. PREMIUMS WRONG
      *  JTG 071995 ADDED LN-PRLNAMT & LN-PRPDUE-DAYS
      *  JTG 072095 ADDED LN-PLAMT, CURBAL AT TIME OF P&L
      *  JTG 082495 ADDED LN-MTD-PRIN, LN-MTD-LC, LN-MTD-INT,
      *                   LN-MTD-OT, LN-MTD-EX
      *  KEC 083095 ADDED LN-BNKRPT-CHAPTER
      *  JTG 092295 ADDED LN-BMO-CON-STATUS LN-BMO-REC-STATUS
      *  JTG 032996 CHANGED LN-PROCEEDS TO BE SIGNED S9(6)V99 COMP-3
      *  JTG 111296 CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      *  KEC 111296 CHANGED LN-POCD TO BE REDEFINED TO BE: POCD1 & POCD2
      *  KEC 012397 ADDED LN-APPNO: APPLICATION NUMBER(TIED TO MA-APPNO)
      *  BAH 071197 ADDED LN-INSFINCHG AND LN-FEEFINCHG FOR WORLD FROM A90
      *  MJD 080197 CHANGED LN-CAN-LOAN-NO TO PIC XX (WAS PIC 99 COMP-3)
      *  JTG 081297 ADDED LN-GIFTNET AND LN-GIFTNET-OUR-PO FOR WORLD A90
      *                   LN-GIFTNET-EXTR, LN-SHIP-SAME-FG
      *  JTG 082097 CHANGED LN-CONPROFILE & LN-RECPROFILE TO SIXTY MONTH TABLES
      *             ADDED LN-PROC-DISBURSED
      *             ADDED LN-PYMMYY-RECDEL & LN-MTD-PAYS-RECDEL
      *             ADDED LN-AD-D-INS-COVR
      *  BAH 091297 CHANGED LN-SPREFUNDATE TO LN-TRW-1ST-30
      *  JTG 112097 EXPANDED BY 230 BYTES
      *  BAH 010898 ADDED LN-TRW-STATUS FOR CUSTOMERS TO FORCE A CREDIT BUREAU
      *             STATUS, PR# 520
      *   CS 031998 ADDED LN-ORIGIN (FEDERATED #646) AND LN-FOLLOWUPDATE(FUTURE)
      *  JTG 050498 CREATED LN-TOT-PARTIAL-CHGOFF, MERCURY TRCD = 'PX', #E175
      *  JTG 060398 ADDED LN-PO-TRCD TO HOLD THE TRCD USED TO PAYOFF THIS LOAN
      *  BAH 062498 CHANGED LN-TILLNO TO ALPHANUMERIC FOR MORE DRAWERS
      *  JTG 071598 ADDED LN-PRDLDISC-UNEARN TO HOLD CARRY OVER DISCOUNT
      *             UNEARN ON RENEWALS
      *  BAH 081098 ADDED LN-YTDNOALDEL FOR MERCURY'S NEW EDIT ON ALLOWABLE
      *             DELINQUENCY, #193
      *  JTG 082698 ADDED LN-REVOLVING FLAG, REGENCY FINANCE REVOLVING
      *                   LN-PREV-STMT-DATE
      *                   LN-LAST-STMT-DATE
      *                   LN-RPRINBAL
      *                   LN-AGEING-AMT    OCCURS 8
      *  KEC 091798 ADDED LN-REVOL-CREDIT-LIMIT; REGENCY FINANCE REVOLVE
      *  KEC 093098 ADDED LN-CYCLE-DAY; REGENCY FINANCE REVOLVE
      *  JTG 093098 ALLOWED LN-CURBAL & LN-PURBAL TO GO NEGATIVE; REGENCY REVOL
      *  KEC 100598 ADDED THE FOLLOWING FOR REGENCY FINANCE REVOLVE:
      *                   LN-CHARGE-INTEREST-FG
      *                   LN-CHARGE-LATECHG-FG
      *                   LN-CHARGE-MEMBERSHIP-FG
      *  JTG 101498 REMOVED LN-REVOL-CREDIT-LIMIT WILL USE LN-CREDITLIM
      *  JTG 101598 CHANGED LN-CYCLE-DAY TO LN-CYCLE-DATE REGENCY, REVOL
      *  KEC 102798 ADDED LN-INDEX-ADJUST-FACTOR FOR REGENCY REVOLVING.
      *  JTG 111998 ADDED LN-OTHBAL-TABLE REVOLVING, REGENCY
      *  JTG 112398 ADDED LN-RCURPRIN, LN-RCURINT, LN-RCURLC, LN-RCUROTH
      *             AND REMOVED LN-RPRINBAL
      *  JTG 120198 ADDED LN-RPDUEPRIN, LN-RPDUEINT, LN-RPDUELC, LN-RPDUEOTH
      *  MJD 990126 ----- UPDATED IN FLEXGEN -----
      *  MJD 990201 ADDED LN-ACCT-FROZEN
      *  GLF 990201 ADDED LN-LPO,LN-REP-CD,LN-PROCESS-CD, TFC.
      *  JTG 990208 ADDED 88'S LN-INS-NO-ONE, LN-INS-SINGLE, LN-INS-JOINT
      *  JTG 990301 ADDED LN-ZERO-BAL-CNT, REVOLVING
      *  CS  1999.0301 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  JTG 990326 ADDED LN-LAST-ACCRUAL-DATE, SP-ERNDELQFRMLA = 'C'   TFC #005
      *  KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0520 ADDED LN-TRW-SPECIAL-CODE          (TFC) PR#:00023
      *  JTG 990720 ADDED LN-ACTIVE-FG                     REGENCY REVOLVING #45
      *  KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  MJD 990910 CHANGED LN-INSELIGIBLE-O3 TO BE LN-INSELIGIBLE-O1.
      *             AS PER JOHN, IT WAS MISNAMED FROM THE START.
      *  MJD 991005 ADDED LN-PRIOR-RN-FOR10 AND LN-LAST-RN-FOR10.
      *             (REGMGM PR#942)
      *   CS 991012 ADDED 88 LEVEL BULK-PURCHASE FOR LN-CONVERCD AND 88 LEVEL
      *             CONVERSION-NORMAL
      *  KEC 991018 ADDED LN-CREATION-STATUS FOR CMSI/CSI PROJECT (LI)
      *               88  LN-CREATION-EXTERNAL            VALUE "L".
      *               88  LN-CREATION-INTERNAL            VALUE "I" " ".
      *               88  LN-CREATION-LI-VENDOR           VALUE "L".
      *
      *  MJD 991220 ADDED LN-TOTNODEFACT AND LN-YTDNODEFACT (WORLD PR#216)
      *  BLV 991220 ADDED 88 FOR LN-POCD-TRANSFERRED (BT)
      *  MJD 000119 ADDED LN-TOTNOLCHG (WORLD PR#218)
      *  MJD 000307 ADDED LN-PRIOR-STMT-COUNTER, REPLACES FM-STMT-COUNTERA FOR
      *             PRINTING STATEMENTS. (FM FIELD IS USED FOR OTHER USES IN 
      *             DIFFERENT DOCUMENTS) (NEWS/MERC PR#???)
      *  BLV 000310 ADDED LN-WCBC-CREDITLIM, WORLD PR#225
      *  KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  MJD 000616 ADDED LN-CONRATE  (REGACC PR#1220)
      *  BLV 000711 REDEFINE LN-REP-CD AS LN-EMP-ID FOR WORLD CLASS BUYING
      *             CLUB PROMOTION
      *  MJD 000911 ADDED LN-LBOX (MFC PR#227)
      *  JTG 001114 ADDED  78  LN-INSTBL-SIZE  VALUE 46 FOR CL/ICUNR*.C PROGRAMS
      *   CS 010120 CHANGED LN-LMO-PRIOR-UNEARNED TO LN-LMO-PRIOR-INTEREST
      *  BLV 010122 ADDED LN-1099-FG, WORLD PR# 250
      *  BLV 010207 REMOVED LN-1099-FG, WORLD PR# 250, NOT PUTTING CHANGE 
      *             IN A15 
      *  JTG 010227 ADDED LN-EXTRA-PRIN-LFP LN-REMAIN-INT-SINCE-LFP
      *             LN-REMAIN-PRIN-SINCE-LFP  (LFP=LAST-FULL-PAYMENT)
      *             LENDMARK, PR#1045 & PR#1380
      *  BLV 010925 ADDED LN-FOLLOWUP-CD, PR# 1487
      *  BAH 011029 ADDED LN-INTERIM-COUPON FOR REGACC PR# 1632
      *  BAH 011210 ADDED LN-PROMID FOR LAST 3 CHAR OF LOGIN WHO SET THE
      *             PROMISE TO PAY, REGACC PR# 1662
      *  JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99    LENDMARK PR#J021
      *             CHANGED LN-AMT-PAID-LAST TO PIC9(7)V99
      *  JTG 020123 EXPANDED PAYMNTD FIELDS TO 1,000,000        
      *             EXPANDED MTD-PAYS-RECDEL FIELD TO 1,000,000 LENDMARK PR#J021
      *  BAH 020214 ADDED LN-LAST-PYPA-DATE FOR REGACC PR# 1721
      *  BAH 020502 ADDED LN-PROMDAYS-PASTDUE FOR "DAYS PAST DUE AT TIME
      *             PROMISE TAKEN", REGACC PR# 1783
      *  JTG 020530 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *             ADDED LN-MIP-MTD-CLAHPAID, LN-MIP-TOT-CLPAID,
      *                   LN-MIP-TOT-AHPAID,
      *                   LN-MIP-CLPDTH,   LN-MIP-AHPDTH
      *                   LN-MIP-FG
      *  BAH 020625 ADDED LN-PLREASON FOR REGACC PR# 1775
      *  BAH 020712 SIGNED LN-TRADEIN, REGACC PR# 1808
      *  KEC 020806 {DR#00009} PARADATA <A15> [KEN]
      *             ADDED LN-RESCISSION AND LN-RESCISSION-DATE.
      *  JTG 020905 ADDED LN-DLCOMM-O3-CMSI LN-DLCOMM-PP-CMSI  REGACC #1846
      *  BAH 030207 ADDED LN-INTERIM-TRW REGACC PR# 16
      *  JTG 040501 ADDED LOGIC TO CAUSE THE UNEARNED LOAN COST TO ROLL TO
      *             THE NEW LOAN AFTER 'RENEWAL'. NOTE: THIS COST IS REFLECTIVE
      *             OF THE DISCOUNT WHEN PURCHASING LOAN PAPER NOT CARRIED AS
      *             SALES PAPER WITH DEALER DISCOUNT.
      *             ADDED SP-LNCOST-FRMLA = 'R'                    REGENCY #2287
      * KEC 040518 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *            ADDED: 05  LN-APPNO-COMP    PIC 9(12)   COMP-6.
      * BAH 050202  REDEFINE LN-CYCLE-DATE WITH LN-BNKRPT-DISMISSAL
      *             NEED FOR THE CREDIT BUREAU AND DONT WANT TO READ EVERY
      *             LT B TRAILER FOR EVERY LOAN SEARCHING IF IT COULD BE
      *             A DISMISSAL...PL# 461
      * BAH 050324 ADDED LN-TRW-B-CONSUMER-IND AND LN-TRW-J-CONSUMER-IND FOR
      *            METRO2 OVERRIDE ABILITY, ACC, PR# 2502
      * BAH 050428 ADDED LN-ACH, REGACC PR# 102
      * BAH 050601 CHANGED LN-TRW-1ST-30 TO LN-TRW-DOFD, DATE OF FIRST 
      *            DELINQUENCY
      * BLF 050726 ADDED LN-1099-C-FG, WORLD PR# 411
      * BAH 050818 ADDED LN-TRW-COMPLIANCE-CODE, LIBERAL PR# 2591
      * JTG 050922 ADDED LN-POCD-UNWIND "*U" AND LN-POCD-NONLOAN REGACC #0105
      * BAH 051025 RENAMED LN-NOODDPY-PD-JUNK, NOT USED, DONT KNOW WHAT IT IS !
      * BAH 051025 RENAMED LN-LEDGERCNT AND LN-LPCNT TO FILLER, NOT USED....
      * BAH 051026 ADDED LN-FROZEN-REASON, REGACC PR# 114
      * BAH 061205 ADDED LN-COUPON-STATEMENT, REGACC PR# 158
      *
      * KEC 070406 {PR#00169} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *            TOOK LN-ZERO-BAL-CNT  [9(3)] AND REDEFINED IT AS
      *                 LN-NEXIS-CRSCORE [9(3)].
      *            LN-NEXIS-CRSCORE IS FOR CMSI.
      *            LN-ZERO-BAL-CNT IS A REVOLVING FIELD WHICH IS NOT
      *            USED FOR CMSI.
      *
      * KEC 070726 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *            REVISED LN-APPNO TO BETTER IDENTIFY NEW & OLD APPS.
      *
      * BAH 071015 ADDED LN-PART-RECOURSE-DATE REDEFINES LN-FILEDATE
      *           FOR REGACC PR# 184
      * BAH 071017 ADDED LN-GARNISH-EXTR AS REDEFINE OF LN-GIFTNET-EXTR
      *            MODERN PR#3086
      * BLF 071116 REDEFINED LN-CHARGE-MEMBERSHIP-FG (REVOLVING) AS
      *            LN-PARVHELD-PAID-FG, REGACC PR# 185
      *
      * KEC 080529 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *            REDEFINED LN-COLLCD INTO TWO PARTS.
      * BLM 100302 ADDED LN-ESCROW-FG, REGWAN PR# 3506
      * BAH 110301 ADDED LN-IL-GROSS-INCOME REDEFINES LN-MIP-TOT-CLPAID 
      *            FOR WORLD "IL" GROSS INCOME PER LOAN PR# 748
      *  CS 111010 ADD " 1" THRU " Z" TO VALUE FOR LN-POCD-POFF, ON LOANS MADE
      *            & PAIDOFF DURING SAME MONTH IT DIDNT GET NEW PREMIUMS IF
      *            THE POFF CODE WAS " S"  WTD
      *  CS 111011 AFTER TALKING TO BARB CHANGED VALUE TO " 1" THRU "ZZ"
      * BAH 140710 ADDED LN-IL-GROSS-INCOME2 REDEFINES LN-MIP-TOT-AHPAID 
      *            FOR WORLD "IL" GROSS INCOME PER LOAN PR# 1002
      * MJD 160817 ADDED LN-FEECODE-CH1-NUMERIC AND LN-FEECODE-CH2-NUMERIC.
      *            WE NEED TO EXCLUDE LIVE CHECK FEE CODES (START WITH ALPHA)
      *            CODES FROM NONMEMO-FEES WHEN CALCULATING HIGH CREDIT WHEN
      *            LC-CALC-HI-CREDIT = "L"                           61000 #1180
      **************************************************************************
       78  LN-SIZE                     VALUE 1800.
       01  LN-REC.
           03  LN3-KEY.
               05  LN-OWNBR            PIC 9(4).
               05  LN2-KEY.
                   07  LN-CLASS        PIC 99.
                   07  LN1-KEY.
                       09  LN-NUMBER   PIC 9(6).
                       09  LN-SEQNO    PIC 99.
                   07  LN-ACCTNO      REDEFINES LN1-KEY
                                       PIC 9(8).
           03  LN-SUBCLASS         PIC 99.                 
           03  LN-LAWCODE          PIC 99.                 
      *----------------------------------------------------
           03  LN-BWTABLE.
               05  LN-BORRTBL      OCCURS 4 TIMES.
                   07  LN-SSNO     PIC 9(9).
                   07  LN-MAKERCD  PIC X.
           03  LN-BWTABLEX        REDEFINES LN-BWTABLE.
               05  LN4-KEY         PIC 9(9).
               05  FILLER          PIC X.
               05  LN5-KEY         PIC 9(9).
               05  FILLER          PIC X.
               05  LN6-KEY         PIC 9(9).
               05  FILLER          PIC X.
               05  LN7-KEY         PIC 9(9).
               05  FILLER          PIC X.
      *----------------------------------------------------
      *    LOAN STATUS CODES
      *----------------------------------------------------
           03  LN-ORGST            PIC XX.
           03  LN-ORGBR            PIC 9(4)   COMP-3.
           03  LN-OWNST            PIC XX.
           03  LN-SPRCLASS         PIC 99.
           03  LN-MORELN           PIC X.
           03  LN-CONVERCD         PIC X.                  
               88 BULK-PURCHASE    VALUE "B".
               88 CONVERSION-NORMAL VALUE "Y".
      ********************************************************
           03  LN-MIDSTREAM        PIC X.                  
      *
      *    SP - NORMAL PURCHASE:
      *
      *    Y  - MIDSTREAM PURCHASE:
      *           TFR TO PURCHASE (TP)
      *           BULK PURCHASE   (FUTURE)
      *
      *         AT THE TIME THE ACCOUNT WAS PURCHASED,
      *         THE ORIGINAL PRINCIPAL BALANCE
      *         HAD BEEN PREVIOUSLY REDUCED:
      *
      *         LN-LNAMT NOT = LN-ORIGPURAMT
      *         LN-CURBAL    = LN-PURBAL
      *
      *    P  - PARTIAL PURCHASE:  (BIG APPLE)
      *
      *         AT THE TIME THE ACCOUNT WAS PURCHASED,
      *         ONLY A PORTION OF THE RECEIVABLE WAS BOUGHT
      *
      *         LN-LNAMT  NOT = LN-ORIGPURAMT
      *         LN-CURBAL NOT = LN-PURBAL
      ********************************************************
           03  LN-PURCD            PIC XXX.
           03  LN-SRCD             PIC XX.
           03  LN-COLLCD           PIC XX.
           03  FILLER              REDEFINES LN-COLLCD.
               05  LN-COLLCD-1     PIC X(01).
               05  LN-COLLCD-2     PIC X(01).
           03  LN-COLLID           PIC X(20).
           03  LN-POCD             PIC XX.
               88  LN-POCD-NONLOAN       VALUE "**" "*I" "*R" "*U".
               88  LN-POCD-OPEN          VALUE "  ".
               88  LN-POCD-POFF          VALUE " 1" THRU "ZZ".
               88  LN-POCD-INRESCISSION  VALUE "*I".
               88  LN-POCD-RESCIND       VALUE "*R".
               88  LN-POCD-PB-RESC       VALUE "*P".
0105           88  LN-POCD-UNWIND        VALUE "*U".
               88  LN-POCD-VOID          VALUE "**".
               88  LN-POCD-TRANSFERRED   VALUE "BT".
           03  FILLER                   REDEFINES LN-POCD.
               05  LN-POCD1              PIC X.
               05  LN-POCD2              PIC X.
           03  LN-PLCD             PIC X.
           03  LN-PLTYPE           PIC X.
           03  LN-CONFIDENTCD      PIC X.                  
           03  LN-ALLOTCD          PIC X.                  
      * NOT USED  LN-RECASTCD
           03  LN-RECASTCD         PIC X.                  
           03  LN-SKIP-TRW         PIC X.                  
           03  LN-STATUSFG         PIC X.                  
           03  LN-SMALLOAN         PIC X.
           03  LN-SALEFIN          PIC X.
           03  LN-SERVACCT         PIC X.
           03  LN-LOANTYPE         PIC X.
           03  LN-ADDON-FG         PIC X.
           03  LN-PAY-SCHLD-FG     PIC X.
           03  LN-LC-CODE          PIC 99.
           03  LN-LC-CODEX        REDEFINES LN-LC-CODE PIC XX.
           03  LN-CAN-LOAN-NO      PIC XX.
           03  LN-STATFLAGS        PIC X(10).
           03  LN-STATUSFLAGS     REDEFINES LN-STATFLAGS
                                   OCCURS 10 TIMES
                                   INDEXED BY STFG. 
               05  LN-STATFG       PIC X.
      *----------------------------------------------------
      *    LOAN DATES
      *----------------------------------------------------
           03  LN-ENTDATE          PIC 9(6)      COMP-3.
           03  LN-LOANDATE         PIC 9(6)      COMP-3.
           03  LN-PURDATE          PIC 9(6)      COMP-3.
           03  LN-INTDATE          PIC 9(6)      COMP-3.
           03  LN-CASHDATE         PIC 9(6)      COMP-3.
           03  LN-FILEDATE         PIC 9(6)      COMP-3.
           03  LN-RESCISSION-DATE  REDEFINES LN-FILEDATE
                                   PIC 9(6)      COMP-3.
           03  LN-LAST-MEMBERSHIP-DATE REDEFINES LN-FILEDATE
                                   PIC 9(6)      COMP-3.
           03  LN-PART-RECOURSE-DATE REDEFINES LN-FILEDATE
                                   PIC 9(6)      COMP-3.
           03  LN-ORIG-1STPAY      PIC 9(6)      COMP-3.
      *--------------------------------------------------------------
      * FOR NORMAL LOANS:
      *     THIS FIELD WILL ALSO HOLD THE ORIG-1STPAY DATE PLUS
      *     THE CURRENT PAY DAY (LN-1STPYDA)
      *
      * FOR REVOLVING LOANS:
      *     AT ORIGINAL REVOLVING LOAN BOOKING THIS FIELD WILL HOLD
      *        THE ORIG-1STPAY DATE
      *     AS THE ACCOUNT CYCLES THE DAY PORTION OF THIS FIELD WILL
      *        CHANGE TO THE LAST STATEMENT'S DUE DAY
      *--------------------------------------------------------------
           03  LN-1STPYDATE        PIC 9(6).
           03  LN-1STPYDATEX      REDEFINES LN-1STPYDATE.
               05  FILLER          PIC XX.
               05  LN-1STPYDA      PIC 99.
               05  FILLER          PIC XX.
           03  LN-JDDATE           PIC 9(6)      COMP-3.
           03  LN-PLDATE           PIC 9(6)      COMP-3.
           03  LN-ACCELDATE        PIC 9(6)      COMP-3.   
           03  LN-REDUDATE         PIC 9(6)      COMP-3.   
      * LN-PROMDATE2 HOLDS THE ACTUAL TRANDATE OF THE PAYOFF
      * LN-POFFDATE WOULD HOLD THE BACKDATE DATE, WHERE PROMDATE2 WOULD
      * HAVE THE TRUE TRANSACTION DATE
           03  LN-POFFDATE         PIC 9(6)      COMP-3.   
           03  LN-LEGALDATE        PIC 9(6)      COMP-3.
           03  LN-BNKRPTDATE       PIC 9(6)      COMP-3.
           03  LN-DEBTAIDATE       PIC 9(6)      COMP-3.
      * REPOSESSION DATES:
           03  LN-REPODATE         PIC 9(6)      COMP-3.
      * REDEEMDATE DOES NOT CLEAR IF ANOTHER "SX" IS POSTED! 
           03  LN-REDEEMDATE       PIC 9(6)      COMP-3.
           03  LN-SOLDDATE         PIC 9(6)      COMP-3.

           03  LN-INS-CONFIRMATION PIC 9(6)      COMP-3.
      *----------------------------------------------------
      *    RATE INFORMATION
      *----------------------------------------------------
           03  LN-APRATE           PIC 999V999   COMP-3.   
           03  LN-EFFRATE          PIC 999V999   COMP-3.   
           03  LN-SMPRATE          PIC 999V999   COMP-3.   
           03  LN-YLDRATE          PIC 999V999   COMP-3.
           03  LN-OVRDRATE         PIC 999V999   COMP-3.   
           03  LN-ENDORATE         PIC 999V999   COMP-3.   
           03  LN-JDRATE           PIC 999V999   COMP-3.   
           03  LN-ACCRATE          PIC 999V999   COMP-3.   
           03  LN-REDURATE         PIC 999V999   COMP-3.   
           03  LN-SERVPER          PIC 9(3)V99   COMP-3.   
      *----------------------------------------------------
      *    LOAN DISCLOSURE INFORMATION
      *----------------------------------------------------
           03  LN-SELLPR           PIC 9(6)V99   COMP-3.
           03  LN-DOWNPY           PIC 9(6)V99   COMP-3.
           03  LN-TRADEIN          PIC S9(6)V99  COMP-3.
           03  LN-LNAMT            PIC 9(6)V99   COMP-3.
           03  LN-FINCHG           PIC 9(6)V99   COMP-3.
           03  LN-INTCHG           PIC 9(6)V99   COMP-3.
           03  LN-SERCHG           PIC 9(5)V99   COMP-3.
           03  LN-EXTCHG           PIC 9(4)V99   COMP-3.
           03  LN-MAINTFEE         PIC 99V99     COMP-3.
           03  LN-POINTS           PIC 99V99     COMP-3.   
           03  LN-PROCEEDS         PIC S9(6)V99   COMP-3.
      * LOANS ORIGINAL TERM AND REVOLVING ACCOUNTS DRAW PERIOD:
           03  LN-ORGTERM          PIC 9(3)      COMP-3.
           03  LN-1STPYAMT         PIC 9(6)V99   COMP-3.
           03  LN-NOREGPY          PIC 9(3)      COMP-3.
           03  LN-REGPYAMT         PIC 9(6)V99   COMP-3.
           03  LN-LASTPYAMT        PIC S9(6)V99  COMP-3.   
           03  LN-PROCASH          PIC S9(7)V99  COMP-3.
      * HOLDS A "Y" WHEN P&L'S ARE POSTED FOR AN EXTRACTION (RMSEXT)
           03  LN-TILLNO           PIC X.
      * HOLDS SPECIFIC AMOUNTS FOR EACH STATE IF NEEDED:
      * MISSISSIPPI--------------------------------------------
      * Y -  PRORATA REFUND ADJUSTMENT OCCURED ON THIS ACCOUNT
      *      AFTER NORMAL PAYOFF
      * A -  PRORATA REFUND ADJUSTMENT WAS GIVEN FOR AN ACCOUNT
      *--------------------------------------------------------

      * STATE-WORKER ALSO USED BY "SE" (SHIFT P&L EXPENSE)
      *              TO HOLD AMOUNT OD P&L EXPENSE SHIFTED
      * GEORGIA------------------------------------------------
      * STATE-WORKER HOLDS TO 8% PORTION OF THE SERVICE CHARGE
      *--------------------------------------------------------
           03  LN-STATE-FLAG       PIC X.
           03  LN-STATE-WORKER     PIC S9(7)V99 COMP-3.
           03  LN-STATE-WORKER2    PIC S9(5)V99 COMP-3.
      *----------------------------------------------------
      *    FEE INFORMATION
      *----------------------------------------------------

           03  LN-FEETBL           OCCURS 10 TIMES.        
               05  LN-FEECODE      PIC XX.
               05  LN-FEECODEX REDEFINES LN-FEECODE.
                   07  LN-FEECODE-CH1 PIC X.
                   88  LN-FEECODE-CH1-NUMERIC 
                              VALUE '1' '2' '3' '4' '5'
                                    '6' '7' '8' '9' '0'.
                   07  LN-FEECODE-CH2 PIC X.
                   88  LN-FEECODE-CH2-NUMERIC 
                              VALUE '1' '2' '3' '4' '5'
                                    '6' '7' '8' '9' '0'.
      * 334
               05  LN-FEEAMT       PIC 9(4)V99   COMP-3.
               05  LN-FEEOURS      PIC X.                  
               05  LN-FEEFINCHG    PIC X.
      *--------------------------------------------------------
      *    INSURANCE INFORMATION
      *
      *    NOTE:
      *         IF LN-ADDON-FG = "Y"
      *            ALL LEVELS CONTAIN THE MOST RECENT
      *            INSURANCE ADDON INFO.
      *
      *    LEVEL: #1  CREDIT LIFE      #6  OTH INS 3
      *           #2  A&H              #7  OTH INS 4
      *           #3  PER. PROPERTY    #8  OTH INS 5
      *           #4  OTHER INS 1
      *           #5  OTHER INS 2
      * 
      *                                          O1    O1    O1
      *                              PP    PP2         PP    PP2
      *            LL    DL    AH    AH    AH    AH    AH    AH
      *          -------------------------------------------------
      *    LL    |  B  |     |  F  |  L  |  W  |  O  |  3  |  4  | 
      *          -------------------------------------------------
      *    DL    |     |  C  |  G  |  M  |  X  |  P  |  6  |  7  | 
      *          -------------------------------------------------
      *    NO CL |     |     |  D  |  K  |  V  |  Q  |  9  |  0  | 
      *          -------------------------------------------------
      *
      *                  O1          O1    O1    
      *            PP    PP    PP2   PP2          
      *          -------------------------------
      *    LL    |  H  |  U  |  S  |  2  |  A  |
      *          -------------------------------
      *    DL    |  J  |  Y  |  T  |  5  |  I  |
      *          -------------------------------
      *    NO CL |  E  |  Z  |  R  |  8  |  1  |
      *          -------------------------------
      *
      *--------------------------------------------------------
           03  LN-INSCODES.
               05  LN-INSURED      PIC X.
                   88  LN-INS-NO-ONE       VALUE "N".
                   88  LN-INS-SINGLE       VALUE "M" "S".
                   88  LN-INS-JOINT        VALUE "J".
               05  LN-INSTYPES     PIC X.
                   88  LN-INSTYPES-VALID   VALUE "A" THRU "Z"
                                                 "0" "1" "2" "3" "4"
                                                 "5" "6" "7" "8" "9".
                   88  LN-INSTYPES-DL      VALUE "C"
                                                 "G" "I" "J" "M" "P"
                                                 "T" "X" "Y" "5" "6"
                                                 "7".
                   88  LN-INSTYPES-LL      VALUE "B"
                                                 "A" "F" "H" "L" "O"
                                                 "S" "U" "W" "2" "3"
                                                 "4".
                   88  LN-INSTYPES-AH      VALUE "D"
                                                 "F" "G" "K" "L" "M"
                                                 "O" "P" "Q" "V" "W"
                                                 "X" "3" "4" "6" "7"
                                                 "9" "0".
                   88  LN-INSTYPES-PP      VALUE "E"
                                                 "H" "J" "K" "L" "M"
                                                 "U" "Y" "Z" "3" "6"
                                                 "9".
                   88  LN-INSTYPES-PP2     VALUE "R"
                                                 "S" "T" "V" "W" "X"
                                                 "2" "4" "5" "7" "8"
                                                 "0".
                   88  LN-INSTYPES-O1      VALUE "1"
                                                 "A" "I" "O" "P" "Q"
                                                 "U" "Y" "Z" "2" "3"
                                                 "4" "5" "6" "7" "8"
                                                 "9" "0".
               05  LN-INSELIGIBLE  PIC X.                  
                   88  LN-INSELIGIBLE-VALID  VALUE "A" THRU "Z"
                                                   "0" "1" "2" "3" "4"
                                                   "5" "6" "7" "8" "9".
                   88  LN-INSELIGIBLE-DL     VALUE "C"
                                                   "G" "I" "J" "M" "P"
                                                   "T" "X" "Y" "5" "6"
                                                   "7".
                   88  LN-INSELIGIBLE-LL     VALUE "B"
                                                   "A" "F" "H" "L" "O"
                                                   "S" "U" "W" "2" "3"
                                                   "4".
                   88  LN-INSELIGIBLE-AH     VALUE "D"
                                                   "F" "G" "K" "L" "M"
                                                   "O" "P" "Q" "V" "W"
                                                   "X" "3" "4" "6" "7"
                                                   "9" "0".
                   88  LN-INSELIGIBLE-PP     VALUE "E"
                                                   "H" "J" "K" "L" "M"
                                                   "U" "Y" "Z" "3" "6"
                                                   "9".
                   88  LN-INSELIGIBLE-PP2    VALUE "R"
                                                   "S" "T" "V" "W" "X"
                                                   "2" "4" "5" "7" "8"
                                                   "0".
                   88  LN-INSELIGIBLE-O1     VALUE "1"
                                                   "A" "I" "O" "P" "Q"
                                                   "U" "Y" "Z" "2" "3"
                                                   "4" "5" "6" "7" "8"
                                                   "9" "0".
      *********************************************************************
      *    LEVEL: #1  CREDIT LIFE      #6  OTH INS 3
      *           #2  A&H              #7  OTH INS 4
      *           #3  PER. PROPERTY    #8  OTH INS 5
      *           #4  OTHER INS 1
      *           #5  OTHER INS 2
      * 
      *********************************************************************
       78  LN-INSTBL-SIZE             VALUE 46.
           03  LN-INSTBL              OCCURS 8 TIMES.
               05  LN-INSCOMP         PIC 99.                 
               05  LN-INSOURS         PIC X.
               05  LN-INSEFF          PIC 9(6)      COMP-3.   
               05  LN-INSTRM         REDEFINES LN-INSEFF
                                      PIC 9(6)      COMP-3.
               05  LN-INSEXP          PIC 9(6)      COMP-3.   
               05  LN-INSPREM         PIC 9(4)V99   COMP-3.
               05  LN-INSCOVR         PIC 9(6)V99   COMP-3.
               05  LN-INSCOMM         PIC 9(4)V99   COMP-3.

               05  LN-INSANTICERN     PIC S9(6)V99  COMP-3.   
               05  LN-INSANTICADJ     PIC S9(6)V99  COMP-3.   
               05  LN-INSUFERN        PIC S9(6)V99  COMP-3.   
               05  LN-INSACCERN       PIC S9(6)V99  COMP-3.   
               05  LN-INSEARN-ORDER   PIC 9.
               05  LN-INSFINCHG       PIC X.

      *----------------------------------------------------
      *    NEW FILLER AREA OVERWRITES LEVEL 9 & 10
      *               OF INSURANCE TABLE:
      *----------------------------------------------------
           03  LN-PYMMYY-RECDEL       PIC 9(4)      COMP-3.
           03  LN-MTD-PAYS-RECDEL     PIC S9(7)V99  COMP-3.
           03  LN-GIFTNET-EXTR        PIC X.
           03  LN-GARNISH-EXTR REDEFINES LN-GIFTNET-EXTR PIC X.
           03  LN-SHIP-SAME-FG        PIC X.
           03  LN-AD-D-INS-COVR       PIC 9(6)      COMP-3.
           03  LN-TOT-PARTIAL-CHGOFF  PIC S9(6)V99  COMP-3.
      *GLF ADDED FOR TFC
           03  LN-LPO                 PIC X(4).
           03  LN-REP-CD              PIC X(8).
           03  LN-EMP-ID REDEFINES LN-REP-CD
                                      PIC 9(8).
           03  LN-PROCESS-CD          PIC X.
      * DATES ARE STORED INTERNALLY AS 00MMDDYY. LEFT ROOM FOR LATER USE
           03  LN-PRIOR-RN-FOR10      PIC 9(8)       COMP-3.
           03  LN-LAST-RN-FOR10       PIC 9(8)       COMP-3.
           03  LN-WCBC-CREDITLIM      PIC S9(5)      COMP-3.
           03  LN-FOLLOWUP-CD         PIC X(4).
1801  * FIELDS FOR MIP INSURANCE
1801       03  LN-MIP-FG              PIC X.
1801       03  LN-MIP-MTD-CLAHPAID    PIC S9(5)V99   COMP-3.
1801       03  LN-MIP-CLPDTH          PIC 9(6)       COMP-6.
1801       03  LN-MIP-TOT-CLPAID      PIC S9(7)V99   COMP-3.
           03  LN-IL-GROSS-INCOME REDEFINES LN-MIP-TOT-CLPAID 
                                      PIC S9(7)V99   COMP-3.
1801       03  LN-MIP-AHPDTH          PIC 9(6)       COMP-6.
1801       03  LN-MIP-TOT-AHPAID      PIC S9(7)V99   COMP-3.
           03  LN-IL-GROSS-INCOME2 REDEFINES LN-MIP-TOT-AHPAID
                                      PIC S9(7)V99   COMP-3.
           03  LN-RESCISSION          PIC X(1).
           03  LN-INTERIM-TRW         PIC X(1).
        
      *----------------------------------------------------
      *    SALES FINANCE PURCHASE INFORMATION
      *----------------------------------------------------
           03  LN-DLNO             PIC 9(4)V99   COMP-3.
           03  LN-DLPOOL           PIC 9(5)      COMP-3.
           03  LN-DLRECOURSE       PIC X.                  
           03  LN-DLPARTRCAMT      PIC 9(6)V99   COMP-3.   
           03  LN-ORIGPURAMT       PIC 9(6)V99   COMP-3.   
           03  LN-PURBAL           PIC S9(6)V99  COMP-3.
           03  LN-DLRVPRIN         OCCURS 3
                                   PIC 9(6)V99   COMP-3.
           03  LN-DLRVCHGS         OCCURS 3
                                   PIC 9(6)V99   COMP-3.
           03  LN-DLPARVHELD       PIC 9(6)V99   COMP-3.
           03  LN-DLPARVPAID       PIC 9(6)V99   COMP-3.
           03  LN-DLDISC           PIC 9(6)V99   COMP-3.   

      *---------------------------------------------------------------------
      *    CURRENT LOAN INFORMATION 
      *---------------------------------------------------------------------
      *---------------------------------------------------------------------
      *    FOR REVOLVING ACCOUNTS:
      *      THIS FIELD IS SET TO (1) ONE MONTH PRIOR TO THE LAST
      *           STATEMENT'S DUE DATE, PRODUCED FOR THIS ACCOUNT 
      *      WHEN A PAYMENT IS POSTED BETWEEN CYCLES THIS DATE
      *           CAN ADVANCE (1) MONTH, IF A LATE CHARGE IS ASSESED OR 
      *           FULL PAYMENT WAS APPLIED
      *
           03  LN-LCPDTH           PIC 9(6)      COMP-3.   
      *---------------------------------------------------------------------
           03  LN-INTPDTH          PIC 9(6)      COMP-3.   
           03  LN-CURBAL           PIC S9(6)V99  COMP-3.
           03  LN-INTBAL           PIC S9(5)V99  COMP-3.
           03  LN-LCBAL            PIC S9(5)V99  COMP-3.   
           03  LN-OTHBAL           PIC S9(5)V99  COMP-3.
           03  LN-OT2BAL           PIC S9(5)V99  COMP-3.
           03  LN-LCPAID           PIC S9(5)V99  COMP-3.
           03  LN-LCPARTIALS       PIC S9(5)V99  COMP-3.
      * STATEMENT COUNTER CONTAINS SEQNO OF LAST PRINTED PAYMENT RECORD.
           03  LN-STMT-COUNTER     PIC 9(4)      COMP-3.   
           03  LN-FROZEN-REASON    PIC X(2).
           03  FILLER REDEFINES LN-FROZEN-REASON.
               05  LN-FROZEN-REASON1 PIC X.
               05  FILLER            PIC X.
           03  LN-COUPON-STATEMENT PIC X.
      *****************
      ***   WARNING *** THIS FILLER USED TO BE 9(3) COMP-3 ******
          
           03  FILLER              PIC X.
      *****************
      * 967
           03  LN-MONTHEND-BAL     PIC 9(6)V99   COMP-3.
      *-------------------------------------------------------
      *    LOAN EARNINGS ANTICIPATED INFORMATION:
      *
      *    LN-ANTICERN - NET ANTICERN = ORIGINAL AMOUNT AT
      *                  BOOKING OR PURCHASE - ANTICADJ
      *                  NOTE:
      *                       WHEN ACCOUNTS ARE TRANSFERED
      *                       TO SERVICE, THIS FIGURE IS
      *                       ADJUSTED DOWN TO MATCH ACCERN,
      *                       REFLECTING NO UNEARNED ON BOOKS.
      *                  NOTE:
      *                       IF SALES FINANCE AND PARTICIPATION
      *                       EXISTS WHERE BY THE EXPENSE IS
      *                       REFLECTED IN A DEFLATED UNEARNED,
      *                       THE FIELD WILL BE DEFALTED BY THE
      *                       PORTION DEALER IS PARTICIPATING IN.
      *
      *    LN-ANTICADJ - ADJUSTMENTS TO ANTICERN
      *                  1. THIS WILL REFLECT THE AMOUNT OF THE REBATE
      *                     NOTE:
      *                           IF SALES FINANCE AND PARTICIPATION
      *                           EXISTS WHERE BY THE ANTICIPATED
      *                           CHARGE IS CARRIED ADJUSTED, THIS 
      *                           ADJUSTMENT WILL REFLECT THE FINANCE
      *                           COMPANY'S PORTION OF THE REBATE
      *
      *    LN-UFERN    - THAT PORTION OF ANTICERN TAKEN INTO
      *                  INCOME AT BOOKING OR PURCHASE
      *
      *    LN-ACCERN   - TOTAL INCOME (INCLUDING UFERN) TAKEN
      *                  FROM NET-ANTICERN (ANTICERN)
      *
      *    LN-ERNDELQ-STOP - FOR CERTAIN FORMULA'S, WHEN THIS FLAG = "Y"
      *                      ACCRUAL EARNINGS ARE STOPPED.
      *
      *    SO:
      *       GROSS ANTICERN = LN-ANTICERN + LN-ANTICADJ
      *       NET ANTICERN   = LN-ANTICERN
      *       UNEARNED       = LN-ANTICERN - LN-ACCERN
      *       TRUE ANTICERN  = LN-ANTICERN - LN-UFERN
      *
      *    ANTICTBL ELEMENTS:
      *       LEVEL 1 - INTEREST CHARGES
      *         "   2 - 1ST PAY EXTENSION CHARGES
      *         "   3 - SERVICE CHARGE 
      *         "   4 - MAINTENANCE FEES OR OTHER FEES
      *         "   5 - DEALER DISCOUNTS
      *         "   6 - OTHER
      *
      *    LN-EARN-ORDER: IDENTIFIES WHERE TO HOLD THIS LEVELS
      *                   EARNINGS IN THE LOAN PAYMENT RECORDS
      *
      *              1 -  LP-EARN1       4 -  LXE-EARN(1)
      *              2 -  LP-EARN2       5 -  LXE-EARN(2)
      *              3 -  LP-EARN3       6 -  LXE-EARN(3)
      *     
      *-------------------------------------------------------
           03  LN-ANTICTERM        PIC 9(3)      COMP-3.
           03  LN-ANTICTBL         OCCURS 6 TIMES.
               05  LN-ANTICERN     PIC S9(6)V99  COMP-3.   
               05  LN-ANTICADJ     PIC S9(6)V99  COMP-3.   
               05  LN-UFERN        PIC S9(6)V99  COMP-3.   
               05  LN-ACCERN       PIC S9(6)V99  COMP-3.   
               05  LN-EARN-ORDER   PIC 9.
           03  LN-LMO-PRIOR-INTEREST  PIC S9(6)V99  COMP-3.
           03  LN-PRIOR-UNEARNED   PIC S9(6)V99  COMP-3.
           03  LN-ERNDELQ-STOP     PIC X.
           03  LN-NONACRERN        PIC S9(6)V99  COMP-3.
           03  LN-PRIOR-NONACRERN  PIC S9(6)V99  COMP-3.
           03  LN-ERN-IB-PDTH      PIC 9(6)      COMP-3.
           03  LN-ERN-IB-PRINBAL   PIC S9(6)V99  COMP-3.
           03  LN-ERN-IB-INTOWE    PIC S9(6)V99  COMP-3.
      *----------------------------------------------------
      *    LOAN TOTALS
      *----------------------------------------------------
      * TOTAL INCOME SINCE THE ACCOUNT WAS BOOKED ON TO SYSTEM:
      * ALL LN-ACCERN, LN-TOTSERACCT AND THOSE PORTIONS OF
      * LN-TOTINT, LN-TOTLCHG AND LN-TOTDEF WHICH WERE
      * EARNED SINCE BOOKING AS A OWNED ACCOUNT:,
           03  LN-TOTERN           PIC S9(7)V99  COMP-3.   

      * TOTAL MONIES COLLECTED FORM OTHER CHARGES,
      * SINCE BOOKING ACCOUNT, BOTH NORMAL/MIDSTREAM
      * PURCHASE AND SERVICING:
           03  LN-TOTOTH           PIC S9(6)V99  COMP-3.   

      * TOTAL INTEREST CHARGES COLLECTED FROM LOAN INCEPTION:
           03  LN-TOTINT           PIC S9(6)V99  COMP-3.   

      * TOTAL LATE CHARGES COLLECTED FROM LOAN INCEPTION:
           03  LN-TOTLCHG          PIC S9(6)V99  COMP-3.   

      * TOTAL DEFERMENT CHARGES COLLECTED FROM LOAN INCEPTION:
           03  LN-TOTDEF           PIC S9(6)V99  COMP-3.   

      * TOTAL INCOME DURING SERVICE ACCOUNT STATUS:
           03  LN-TOTSERACCT       PIC S9(6)V99  COMP-3.   

           03  LN-LCWAIVERS        PIC S9(5)V99  COMP-3. 
           03  LN-INTWAIVERS       PIC S9(5)V99  COMP-3.   
           03  LN-TOTPAYMNTD       PIC S9(7)V99  COMP-3.   
           03  LN-TOTEXCPAYMNTD    PIC S9(7)V99  COMP-3.   
           03  LN-TOTNODEF         PIC S99       COMP-3.   
           03  LN-YTDNODEF         PIC S99       COMP-3.   
      * TOTAL ALLOWABLE (DEFERMENTS) DELINQUENCY,
      * THESE DEFERMENTS ARE NOT INCLUDED IN TOTNODEF AND YTDNODEF:
           03  LN-TOTNOALDEL       PIC S99       COMP-3.
           03  LN-YTDINTPD         PIC S9(5)V99  COMP-3.   
           03  LN-LYRINTPD         PIC S9(5)V99  COMP-3.   
           03  LN-TOTINTPD         PIC S9(5)V99  COMP-3.   

           03  LN-RECASTINT        PIC 9(6)V99   COMP-3.   
           03  LN-RECASTPRIN       PIC 9(6)V99   COMP-3.   
      *----------------------------------------------------
      *    REBATE INFORMATION
      *----------------------------------------------------
      * DATE OF FIRST DELINQUENCY 
           03  LN-TRW-DOFD         PIC 9(6)      COMP-3.   
           03  LN-REBATE-TAB.
               05  LN-CL-REBATE    PIC X.
               05  LN-AH-REBATE    PIC X.
               05  LN-PP-REBATE    PIC X.
               05  LN-INT-REBATE   PIC X.
               05  LN-SC-REBATE    PIC X.
               05  LN-MF-REBATE    PIC X.
               05  LN-DF-REBATE    PIC X.
               05  LN-EX-REBATE    PIC X.
               05  LN-O1-REBATE    PIC X.
               05  LN-O2-REBATE    PIC X.
               05  LN-O3-REBATE    PIC X.
               05  LN-O4-REBATE    PIC X.
               05  LN-O5-REBATE    PIC X.
           03  LN-REBATE-TABX     REDEFINES LN-REBATE-TAB.
               05  LN-REBATE       PIC X OCCURS 13.

           03  LN-PLREASON         PIC XX.
      *----------------------------------------------------
      *    COLLECTION INFORMATION
      *----------------------------------------------------
           03  LN-COLLECTOR        PIC X(3).               
           03  LN-ACTIONCD         PIC X(4).               
           03  LN-ACTIONCD2        PIC X(4).               
           03  LN-ATCODE           PIC X(3).
           03  LN-RECODE           PIC X(3).

           03  LN-LAST-PYPA-DATE   PIC 9(6)      COMP-6.
           03  LN-ESCROW-FG        PIC X.
           03  FILLER              PIC X(5).

           03  LN-PROMDAYS-PASTDUE PIC S9(3)     COMP-3.
           03  LN-PROMID           PIC X(3).
           03  LN-PROMDATE         PIC 9(6)      COMP-3.   
           03  LN-PROMAMT          PIC 9(6)V99   COMP-3.
           03  LN-DATE-PAID-LAST   PIC 9(6)      COMP-3.   
           03  LN-AMT-PAID-LAST    PIC 9(7)V99   COMP-3.   
           03  LN-LASTCFORMNO      PIC 9(4)      COMP-3.   
           03  LN-LASTCFORMDATE    PIC 9(6)      COMP-3.   
      * NO. OF TIMES EXTRACTED:
           03  LN-DELCNT           PIC 99.                 
      * NO. OF DELINQUENT FORMS SENT:
           03  LN-DELCNT2          PIC 99.                 
           03  LN-CREDITLIM        PIC 9(6)      COMP-3.
           03  LN-CRSCORE          PIC S9(3)     COMP-3.
  
      *----------------------------------------------------
      *    SOLICIT INFORMATION
      *----------------------------------------------------
           03  LN-LASTSFORMNO       PIC 9(4)      COMP-3.   
           03  LN-LASTSFORMDATE     PIC 9(6)      COMP-3.   
      * NO. OF SOLICITATION FORMS SENT:
           03  LN-SOLCNT           PIC 99.
           03  LN-MESSAGE          PIC X(30).
      *    THIS FIELD IS SET MANUALLY IN THE INQUIRIES, ONLY IF BRANCH
      *    FLAG ALLOWS.  THE EXTRACTION UPDATE WILL COMPARE THE EXTRACTION
      *    DATE(TODAY), AFTER INCREMENTED BY # OF DAYS, TO THE SECOND
      *    PROMISE DATE, IF FIRST IS LESS, THEY STAY THE SAME, IF FIRST
      *    IS AFTER SECOND, MOVE SECOND TO FIRST, AND 0 SECOND.
      *   

      * LN-PROMDATE2 HOLDS THE ACTUAL TRANDATE OF THE PAYOFF
      * LN-POFFDATE WOULD HOLD THE BACKDATE DATE, WHERE PROMDATE2 WOULD
      * HAVE THE TRUE TRANSACTION DATE
           03  LN-PROMDATE2        PIC 9(6)      COMP-3.
      *----------------------------------------------------
      *    PRIOR LOAN INFORMATION
      *----------------------------------------------------
           03  LN-OLDACCTNO        PIC 9(8).
           03  LN-OLDACCTNOX      REDEFINES LN-OLDACCTNO.
               05  LN-OLDNUMBER    PIC 9(6).
               05  LN-OLDSEQ       PIC 9(2).
           03  LN-PRLNNO           PIC 9(8)      COMP-3.
           03  LN-PRBAL            PIC S9(6)V99  COMP-3.
           03  LN-PRINTDUE         PIC S9(5)V99  COMP-3.   
           03  LN-PRFINCHGRB       PIC S9(5)V99  COMP-3.   
           03  LN-PRINSRB          PIC S9(5)V99  COMP-3.   
           03  LN-PRPOCD           PIC XX.                 
           03  LN-PRPODATE         PIC 9(6)      COMP-3.   
           03  LN-PRCONPROFILE     PIC 9(12)     COMP-3.   
           03  LN-PRDELINQAMT      PIC S9(7)V99  COMP-3.   
      *----------------------------------------------------
      *    LOAN ADDON INSURANCE CONSOLIDATED INFORMATION
      * NOTE:
      *      LN-SUM-MAINTFEE = TOTAL CONTRACT CHARGE
      *----------------------------------------------------
           03  LN-SUM-ORIGPURAMT   PIC 9(6)V99   COMP-3.
           03  LN-SUM-LNAMT        PIC 9(6)V99   COMP-3.
           03  LN-SUM-FINCHG       PIC 9(6)V99   COMP-3.
           03  LN-SUM-INTCHG       PIC 9(6)V99   COMP-3.
           03  LN-SUM-ORGTERM      PIC 9(3)      COMP-3.
           03  LN-SUM-REGPYAMT     PIC 9(6)V99   COMP-3.
           03  LN-SUM-EXTCHG       PIC 9(4)V99   COMP-3.
           03  LN-SUM-SERCHG       PIC 9(5)V99   COMP-3.
           03  LN-SUM-MAINTFEE     PIC 9(4)V99   COMP-3.

      *----------------------------------------------------
      *    UNIT PERIOD INFORMATION
      *----------------------------------------------------
           03  LN-UNITPER-INFO.
               05  LN-UNITPER-CD    PIC X.
                   88  LN-UNITPER-CD-DAYS    VALUE "D" "S" "W" "B".
                   88  LN-UNITPER-CD-MONTHS  VALUE "M" "Y".
               05  LN-UNITPER-FREQ  PIC 999        COMP-3.
           03  LN-DAYS-IN-UNITPER   PIC 9(3)       COMP-3.   
           03  LN-UNITPER-PER-YEAR  PIC 9(3)V9(12) COMP-3.

      *---------------------------------------------------
      *    LN-REPOCD =         ' '    ACCOUNT WAS NEVER IN REPO
      *    --> IN REPO STATUS  'X'    REPO IS ONHAND
      *    --> IN REPO STATUS  'S'    REPO WAS SOLD
      *    <-- OUT OF REPO     'V'    REPO REDEEMED
      *---------------------------------------------------
           03  LN-REPOCD            PIC X.
               88  LN-IS-REPO-AR     VALUE "X" "S".
               88  LN-REPO-ONHAND    VALUE "X".
               88  LN-REPO-REDEEMED  VALUE "V".
               88  LN-REPO-SOLD      VALUE "S".
           03  LN-BR-TFR-FG         PIC X.
           03  LN-O3INS-DLPROC      PIC 9(6)V99    COMP-3.
           03  LN-INSPREM-DLADJ     PIC S9(5)V99   COMP-3.
           03  LN-PRLNAMT           PIC 9(6)V99    COMP-3.
           03  LN-PRPDUE-DAYS       PIC S9(5)      COMP-3.
      * PLAMT = CURBAL AT TIME OF P&L:
           03  LN-PLAMT             PIC 9(6)V99    COMP-3.
      * MONEYS APPLIED TO PRINCIPAL:
           03  LN-MTD-PRIN          PIC S9(6)V99   COMP-3.
      * MONEYS APPLIED TO LATE CHARGES:
           03  LN-MTD-LC            PIC S9(5)V99   COMP-3.
      * MONEYS APPLIED TO INTEREST:
           03  LN-MTD-INT           PIC S9(5)V99   COMP-3.
      * MONEYS APPLIED TO OTH & OT2:
           03  LN-MTD-OT            PIC S9(5)V99   COMP-3.
      * MONEYS APPLIED RE: EXCEPTION PAYMENTS:
           03  LN-MTD-EX            PIC S9(5)V99   COMP-3.
      * BANKRUPTCY CHAPTER NUMBER:
           03  LN-BNKRPT-CHAPTER    PIC 9(2).
      *----------------------------------------------------------
      * CONTRACTUAL & RECENCY STATUS AT BEGINNING OF MONTH:
      * VALUES:
      *        9  -  NEW LOAN               3  -  90
      *        Y  -  POTENTIAL 30           4  - 120
      *        M  -  MISSPAY                5  - 150
      *        0  -  CURRENT                6  - 180
      *        1  -  30                     7  - 210+
      *        2  -  60
      *----------------------------------------------------------
           03  LN-BMO-CON-STATUS    PIC X.
           03  LN-BMO-REC-STATUS    PIC X.

      *=================================================================
      *    LN-APPNO-MA ---> MA-REC ---> MA-APPNO.
      *    LN-APPNO-SA ---> SA-REC ---> SA-APPNO.
      *=================================================================
           03  LN-APPNO             PIC 9(06).
           03  LN-APPNO-MA          REDEFINES LN-APPNO
                                    PIC 9(06).
           03  LN-APPNO-SA          REDEFINES LN-APPNO
                                    PIC 9(12) COMP-6.

      *=================================================================
           03  LN-GIFTNET           PIC X.
           03  LN-GIFTNET-OUR-PO    PIC 9(6)  COMP-3.
           03  LN-PROC-DISBURSED    PIC X.
      * THIS FILLER IS STILL PD-NOODDPY AND USED !!
           03  FILLER               PIC X.
      * SIXTY MONTH PROFILES:
           03  LN-CONPROFILE       OCCURS 5
                                    PIC 9(12)     COMP-3.   
           03  LN-RECPROFILE       OCCURS 5
                                    PIC 9(12)     COMP-3.   
           03  LN-TRW-STATUS        PIC XX.

           03  LN-ORIGIN            PIC X(4).
           03  LN-FOLLOWUP-DATE     PIC 9(6)      COMP-3.
           03  LN-PO-TRCD           PIC XX.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  JTG 040501 ADDED LOGIC TO CAUSE THE UNEARNED LOAN COST TO ROLL TO
      *             THE NEW LOAN AFTER 'RENEWAL'. NOTE: THIS COST IS REFLECTIVE
      *             OF THE DISCOUNT WHEN PURCHASING LOAN PAPER NOT CARRIED AS
      *             SALES PAPER WITH DEALER DISCOUNT.
      *             ADDED SP-LNCOST-FRMLA = 'R'                    REGENCY #2287
      *       NOTE: LN-PRDLDISC-UNEARN WILL HOLD THE COST (DISCOUNT)
      *             FOR DIRECT LOANS, EVEN THOUGH IT IS NOT TIED TO A DEALER.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  LN-PRDLDISC-UNEARN   PIC S9(7)V99  COMP-3.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  LN-YTDNOALDEL        PIC S99       COMP-3.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FIELDS SPECIFIC TO REVOLVING LOAN CLASSES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  LN-REVOLVING                    PIC X.
           03  LN-PREV-STMT-DATE               PIC 9(6)       COMP-3.
           03  LN-LAST-STMT-DATE               PIC 9(6)       COMP-3.
      * THIS FIELD HOLD THE NO. OF CYCLES WHEN THE LOANS BALANCE WAS ZERO:
           03  LN-ZERO-BAL-CNT                 PIC 9(3)       COMP-3.
           03  LN-NEXIS-CRSCORE                REDEFINES LN-ZERO-BAL-CNT
                                               PIC 9(3)       COMP-3.

           03  LN-ACTIVE-FG                    PIC X.
           03  LN-TRW-B-CONSUMER-IND           PIC X(2).
           03  LN-AGEING-BUCKETS.
               05  LN-AGEING-AMT               OCCURS 8
                                               PIC S9(6)V99   COMP-3.
      *-----------------------------------------------------------------
      *    THIS DATE INDICATES THE NEXT CYCLE DATE OF THE LOAN AND
      *    IS SET IN:
      *
      *    - INITIAL REVOL LOAN INPUT  VONPB0.C ----> LONPB2.C
      *    - STATEMENT UPDATE          RVSTUP.C

           03  LN-CYCLE-DATE                   PIC 9(6).
           03  FILLER                         REDEFINES LN-CYCLE-DATE.
               05  LN-CYCLE-MM                 PIC 99.
               05  LN-CYCLE-DAY                PIC 99.
               05  LN-CYCLE-YY                 PIC 99.
           03  LN-BNKRPT-DISMISSAL REDEFINES LN-CYCLE-DATE PIC 9(6).
      *-----------------------------------------------------------------

           03  LN-CHARGE-INTEREST-FG           PIC X.
           03  LN-CHARGE-LATECHG-FG            PIC X.
           03  LN-CHARGE-MEMBERSHIP-FG         PIC X.
           03  LN-PARVHELD-PAID-FG REDEFINES LN-CHARGE-MEMBERSHIP-FG
                                               PIC X.
           03  LN-INDEX-ADJUST-FACTOR          OCCURS 2
                                               PIC S999V999   COMP-3.
      *-------------------------------------------------------------------------
      * NOTE: FOR REVOLVING THE SUM OF LN-OTHBAL-AMT()S IN THIS TABLE
      *       MUST ADD UP TO LN-OTHBAL
      *
           03  LN-OTHBAL-TABLE.
               05  LN-OTHBAL-AMOUNT            OCCURS 12.
                   07  LN-OTHBAL-AMT           PIC S9(5)V99   COMP-3.
           03  FILLER                         REDEFINES LN-OTHBAL-TABLE.
               05  LN-OTHBAL-CL                PIC S9(5)V99   COMP-3.
               05  LN-OTHBAL-AH                PIC S9(5)V99   COMP-3.
               05  LN-OTHBAL-PP                PIC S9(5)V99   COMP-3.
               05  LN-OTHBAL-O1                PIC S9(5)V99   COMP-3.
               05  LN-OTHBAL-MEMBERSHIP        PIC S9(5)V99   COMP-3.
               05  LN-OTHBAL-NSF               PIC S9(5)V99   COMP-3.
               05  LN-OTHBAL-MISC1             PIC S9(5)V99   COMP-3.
               05  LN-OTHBAL-MISC2             PIC S9(5)V99   COMP-3.
               05  LN-OTHBAL-MISC3             PIC S9(5)V99   COMP-3.
               05  LN-OTHBAL-MISC4             PIC S9(5)V99   COMP-3.
               05  LN-OTHBAL-MISC5             PIC S9(5)V99   COMP-3.
               05  LN-OTHBAL-MISC6             PIC S9(5)V99   COMP-3.
      *-------------------------------------------------------------------------
      *    REVOLVING CURRENT CHARGES
      *    NOTE:
      *          THESE CHARGES REFLECT THOSE APPEARING ON THE LAST
      *          BILLING STATEMENT AS CURRENT CHARGES. THEY WILL BE
      *          REDUCED WITH CREDITS THRU THE NEXT BILLING STATEMENT
      *          AND WILL REFLECT AT THE NEXT STATEMENT RUN THE 
      *          AMOUNTS PAST DUE.
           03  LN-RCURPRIN                     PIC S9(6)V99   COMP-3.
           03  LN-RCURINT                      PIC S9(6)V99   COMP-3.
           03  LN-RCURLC                       PIC S9(6)V99   COMP-3.
           03  LN-RCUROTH                      PIC S9(6)V99   COMP-3.
      *-------------------------------------------------------------------------
      *    REVOLVING PASTDUE CHARGES
      *    NOTE:
      *          THESE CHARGES REFLECT THOSE APPEARING ON THE LAST
      *          BILLING STATEMENT AS PAST DUE CHARGES OR UNPAID
      *          CURRENT CHARGES ABOVE. THEY WILL BE REDUCED WITH
      *          CREDITS THRU THE NEXT BILLING STATEMENT.
           03  LN-RPDUEPRIN                    PIC S9(6)V99   COMP-3.
           03  LN-RPDUEINT                     PIC S9(6)V99   COMP-3.
           03  LN-RPDUELC                      PIC S9(6)V99   COMP-3.
           03  LN-RPDUEOTH                     PIC S9(6)V99   COMP-3.
      *-----------------------------------------------------------------
           03  LN-ACCT-FROZEN                  PIC X.
           03  LN-LAST-ACCRUAL-DATE            PIC 9(6)       COMP-3.
           03  LN-TRW-SPECIAL-CODE             PIC X(2).

      *-----------------------------------------------------------------
      *    LN-CREATION-STATUS:  USED TO KNOW IF CREATED BY A OUTSIDE
      *                         VENDOR (EXTERNAL) OR BY THE LOAN MANAGER
      *                         (INTERNAL).
      *-----------------------------------------------------------------
           03  LN-CREATION-STATUS              PIC X(1).
               88  LN-CREATION-EXTERNAL            VALUE "L".
               88  LN-CREATION-INTERNAL            VALUE "I" " ".
               88  LN-CREATION-LI-VENDOR           VALUE "L".

       
           03  LN-TOTNODEFACT                  PIC S99      COMP-3.
           03  LN-YTDNODEFACT                  PIC S99      COMP-3.
           03  LN-TOTNOLCHG                    PIC S9(3)    COMP-3.
           03  LN-PRIOR-STMT-COUNTER           PIC 9(4)     COMP-3.   
           03  LN-CONRATE                      PIC 999V999  COMP-3.
           03  LN-LBOX                         PIC X(1).
      *-----------------------------------------------------------------
      *    THE FOLLOWING LFP FIELDS ARE USED FOR CALCULATION OF
      *    AMORTIZATION INTEREST RE: REAL ESTATE 
      *    WHERE LFP == (L)AST (F)ULL (P)AYMENT
      *-----------------------------------------------------------------
           03  LN-EXTRA-PRIN-LFP               PIC S9(7)V99 COMP-3.
           03  LN-REMAIN-INT-SINCE-LFP         PIC S9(7)V99 COMP-3.
           03  LN-REMAIN-PRIN-SINCE-LFP        PIC S9(7)V99 COMP-3.
      *-----------------------------------------------------------------
           03  LN-INTERIM-COUPON               PIC X.
1846  *  JTG 020905 ADDED LN-DLCOMM-O3-CMSI LN-DLCOMM-PP-CMSI  REGACC #1846
1846       03  LN-DLCOMM-O3-CMSI               PIC 9(4)V99 COMP-6.
1846       03  LN-DLCOMM-PP-CMSI               PIC 9(4)V99 COMP-6.
           03  LN-TRW-J-CONSUMER-IND           PIC X(2).
           03  LN-ACH                          PIC X.
           03  LN-1099-C-FG                    PIC X.
           03  LN-TRW-COMPLIANCE-CODE          PIC XX.
               88  LN-TRW-COMPLIANCE-VALID     VALUE "XA" THRU "XH" "XJ"
                                                     "XR".
0105       03  LN-UNWIND-REASON-CODE           PIC X(2).


       COPY "LIBGB/GBFDPR.CPY".
       FD  OLM-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01LM.CPY" REPLACING LEADING "LM" BY "OLM".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/LMFILE S35 03/06/24-13:07:00 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       01  OLM-PATH                    PIC X(35) VALUE SPACES.
       01  OLM-ALL-PATH.
           03  OLM-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OLM-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  OLM-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/LP/LMFILE".

      * COPYMEMBER: LIBLP/LPWSLN
      *********************************************************
      *        LIBLP/LPWSLN: LNFILE PATH WORKERS
      *********************************************************
      * 01  LN-PATH             PIC X(35) VALUE
      *     "../LP/LNFILE                       ".
      * 01  LN-ALL-PATH.
      *     03  FILLER          PIC X(9)  VALUE "../../../".
      *     03  LN-ALL-MACHINE  PIC XX.
      *     03  FILLER          PIC X     VALUE "/".
      *     03  LN-ALL-DATA     PIC X(6).
      *     03  FILLER          PIC X(10) VALUE
      *         "/LP/LNFILE".
      * 01  LN-BI-FG            PIC X     VALUE " ".

       01  LN-PATH                    PIC X(35) VALUE SPACES.
       01  LN-ALL-PATH.
           03  LN-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  LN-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  LN-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/LP/LNFILE".

       01  LM1-CONN-OPEN-SW           PIC X        VALUE "N".

      *********************************************************
      *        SQL DECLARE SECTION - WORKERS
      *********************************************************

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-SQL                    PIC X(200).
       01  RET-DB-NAME                 PIC X(50).
       01  RET-SERVER-NAME             PIC X(50).
       01  RET-VERSION                 PIC X(100).
       01  MISC-RETURN-CODE            PIC S9(4)   COMP-5.
       EXEC SQL END DECLARE SECTION END-EXEC.

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************

       01  LM-EOF-SW                   PIC X      VALUE "N".
       01  OLM-EOF-SW                  PIC X      VALUE "N".
       01  ERROR-DATE-SW               PIC X      VALUE "N".

       01  WS-OLM1-KEY.
           03 WS-OLM-BRNO              PIC 9(04)  VALUE ZEROES.
           03 WS-OLM-ACCTNO            PIC 9(08)  VALUE ZEROES.
           03 WS-OLM-DATE-REV          PIC 9(08)  VALUE ZEROES.
           03 WS-OLM-SEQ               PIC 9(03)  VALUE ZEROES.

       01  WS-LM1-KEY.
           03 WS-LM-BRNO               PIC 9(04)  VALUE ZEROES.
           03 WS-LM-ACCTNO             PIC 9(08)  VALUE ZEROES.
           03 FILLER REDEFINES WS-LM-ACCTNO.
              05 WS-LM-ACCTNO16        PIC 9(06).
              05 WS-LM-ACCTNO78        PIC 9(02).
           03 WS-LM-DATE-REV           PIC 9(08)  VALUE ZEROES.
           03 WS-LM-SEQ                PIC 9(03)  VALUE ZEROES.

       01  WORK-ACCTNO. 
           03 WORK-ACCTNO12            PIC 9(02).
           03 WORK-ACCTNO38            PIC 9(06).

      * TFR TO BR XXXX FR BR XXXX, FOR
      * THE END OF THIS MEMO, WE CHANGED THE ACCTNO IN S35 TO BE 8 DIGITS
       01  HOLD-MEMO-A15              PIC X(30).
       01  HOLD-MEMO-A15X REDEFINES HOLD-MEMO-A15.
           03  HOLD-MEMO-TFR          PIC X(9).
           03  HOLD-MEMOX    REDEFINES HOLD-MEMO-TFR.
                05  HOLD-MEMO-FB      PIC X(6).
       01  HOLD-MEMO-S35              PIC X(30).

       01  MACHINE-R1.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 1.
       01  MACHINE-R2.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 2.
       01  OLD-ACCTNO                  PIC 9(8).
       01  FILLER REDEFINES OLD-ACCTNO.
           03  NEW-ACCTNO              PIC 9(6).
           03  FILLER                  PIC 99.

       01  MISC-CURSOR-STAT            PIC X(2) VALUE SPACES.
           88  MISC-CURSOR-STAT-GOOD            VALUE "00".
           88  MISC-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                      "01" THRU "~~".

       01  PROCESS-STAT                PIC X(08)  VALUE SPACES.
           88  PROCESS-STAT-GOOD                  VALUE "00".
           88  PROCESS-STAT-BAD                   VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       01  RECORDS-PROCESSED           PIC 9(9) VALUE 0.
           88  NO-RECORDS-FOUND                     VALUE ZEROES.

       01  S35-RECORD-CTR              PIC 9(10)     VALUE ZEROES.  
       01  A15-RECORD-CTR              PIC 9(10)     VALUE ZEROES. 

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB                         PIC 9(04)     VALUE ZEROES.

       01  MESS-LIST                   PIC X(5)      VALUE "MESS.".

       01  SYSDATE-YYYYMMDD            PIC 9(8)      VALUE ZEROES.

       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING BRANCH/G".
           03  FILLER PIC X(19) VALUE "ROUP               ".

       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G BRANCH NUMBER               ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-BEG-ACCTNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER BEGINNING ACCOUNT NUMBER   ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-ACCTNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G ACCOUNT NUMBER              ".
           03  FILLER PIC X(19) VALUE "                   ".



      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-PATH             PIC X(9).
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  BEG-ACCTNO              PIC 9(06).
           03  END-ACCTNO              PIC 9(06).
           03  GROUP-FLAG              PIC X(01).
           03  ENT-GROUP               PIC X(04).

       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

      *-----------------------------------------------------------------
       01  SV-BEG-BRANCH               PIC 9(04)     VALUE 9999.
       01  SV-END-BRANCH               PIC 9(04)     VALUE 9999.
       01  SV-GROUP-FLAG               PIC X(01)     VALUE SPACES.
       01  SV-ENT-GROUP                PIC X(04)     VALUE SPACES.

       01  WS-DATE-FULL-UNREVERSE      PIC 9(08).
       01  FILLER REDEFINES WS-DATE-FULL-UNREVERSE.  
           03  WS-DATE-FULL-YYYY       PIC 9(04).
           03  WS-DATE-FULL-MM         PIC 9(02).
           03  WS-DATE-FULL-DD         PIC 9(02).
 
       01  LN-COUNT            PIC 9(03)     VALUE 99.
       01  LN-FEED             PIC 9(02).
       01  PAGE-NUMBER         PIC 9(04)     VALUE ZEROES.

      ******************************************************************
      *
      *    H E A D E R - L I N E - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-01.
           03  H01-DATE                PIC X(08) VALUE SPACES.
           03  FILLER                  PIC X(02) VALUE SPACES.
           03  H01-TIME                PIC X(08) VALUE SPACES.
           03  FILLER                  PIC X(6) VALUE SPACES.
           03  H01-NAME                PIC X(40) VALUE SPACES.
           03  FILLER                  PIC X(6) VALUE SPACES.
           03  FILLER                  PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE                PIC Z(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10) VALUE "USER ID:".
           03  H02-USERID              PIC X(10) VALUE SPACES.
           03  FILLER                  PIC X(07) VALUE SPACES.
           03  H02-TITLE               PIC X(50) VALUE 
           "LOAN MEMO FILE (LMFILE) BYPASSED DATES".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(02) VALUE SPACES.
           03  FILLER                PIC X(04) VALUE "BRNO".
           03  FILLER                PIC X(03) VALUE SPACES.
           03  FILLER                PIC X(08) VALUE "ACCTNO  ".
           03  FILLER                PIC X(03) VALUE SPACES.
           03  FILLER                PIC X(08) VALUE "DATE REV".
           03  FILLER                PIC X(03) VALUE SPACES.
           03  FILLER                PIC X(03) VALUE "SEQ".
           03  FILLER                PIC X(03) VALUE SPACES.
           03  FILLER                PIC X(10) VALUE "DATE      ".
           03  FILLER                PIC X(03) VALUE SPACES.
           03  FILLER                PIC X(30) VALUE "MEMO DESCRIPTION".
           03  FILLER                PIC X(03) VALUE SPACES.
           03  FILLER                PIC X(11) VALUE "CURBAL     ". 
           03  FILLER                PIC X(02).
           03  FILLER                PIC X(04) VALUE "POCD".
           03  FILLER                PIC X(03).
           03  FILLER                PIC X(06) VALUE "POFFDT".
           03  FILLER                PIC X(03).
           03  FILLER                PIC X(15) VALUE "MESSAGE".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(133) VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(02).
           03  D01-BRNO                       PIC 9(04).   
           03  FILLER                         PIC X(03).  
           03  D01-ACCTNO                     PIC 9(08).   
           03  FILLER                         PIC X(03).  
           03  D01-DATE-REV                   PIC 9(08).   
           03  FILLER                         PIC X(03).  
           03  D01-SEQ                        PIC 9(03).   
           03  FILLER                         PIC X(03).  
           03  D01-DATE                       PIC X(10).  
           03  FILLER REDEFINES D01-DATE.                 
               05  D01-MM                     PIC X(02).  
               05  D01-D1                     PIC X.       
               05  D01-DD                     PIC X(02).  
               05  D01-D2                     PIC X.       
               05  D01-YYYY                   PIC X(04).
           03  FILLER                         PIC X(3).
           03  D01-MEMO                       PIC X(30).
           03  FILLER                         PIC X(3).
           03  D01-CURBAL                     PIC ZZZ,ZZ9.99-.
           03  FILLER                         PIC X(3).
           03  D01-POCD                       PIC XX.
           03  FILLER                         PIC X(4).
           03  D01-POFFDATE                   PIC 9(6).
           03  FILLER                         PIC X(3).
           03  D01-TEXT                       PIC X(15).

       01  RANGE-LINE-01                      REDEFINES DETAIL-LINE.
           03  R01-PROMPT                     PIC X(32).
           03  R01-FIELD                      PIC X(48).
           03  R01-AMT                        REDEFINES R01-FIELD
                                              PIC ZZZ,ZZZ,ZZ9.99-.
           03  R01-NUMERIC                    REDEFINES R01-FIELD
                                              PIC Z(10)9-.
           03  R01-TEXT                       REDEFINES R01-FIELD
                                              PIC X(48).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUT/LMFILE_DEF.CPY".
       COPY "LIBUT/LMFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/LMFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OLM-FILE LN-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LM1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-LN1-FILE. 
           PERFORM CLOSE-PR-FILE.
           CLOSE OLM-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           PERFORM INITIALIZATION.

           IF ( PROCESS-STAT-BAD )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM EXECUTION-MODULE.

           PERFORM RANGE-LINES.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE STAT---GOOD                TO PROCESS-STAT.

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "LMFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE LMFILE-QUEUE-POS TO Q-POS.
           MOVE LMFILE-COPIES-POS TO C-POS.
           MOVE LMFILE-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO STARTING-TIME.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS......" TO LEGEND
              PERFORM SEND-LEGEND.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYSDATE-YYYYMMDD.

           PERFORM START-MISC-CSR.
           PERFORM READ-MISC-CSR-NEXT.
           PERFORM CLOSE-MISC-CSR.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE GROUP-FLAG              TO SV-GROUP-FLAG
              MOVE ENT-GROUP               TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO              TO BEG-BRANCH END-BRANCH
              MOVE "N"                     TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH      TO PRU-FILE
           ELSE
           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC     TO BEG-BRANCH
                                              END-BRANCH.

           MOVE ZEROES                     TO RECORDS-PROCESSED
                                              A15-RECORD-CTR
                                              S35-RECORD-CTR.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.

      *     PERFORM ENABLE-INTERRUPT.

           GO TO INITIALIZATION-EXIT.

       INITIALIZATION-END.

           MOVE "X"                        TO EXT-EOBUF-ERRCD.
           MOVE STAT---BAD                 TO PROCESS-STAT.

       INITIALIZATION-EXIT.
           EXIT.

      ***************************************************
       ENTER-PARAMETERS SECTION.
      ***************************************************

           MOVE STAT---GOOD                TO PROCESS-STAT.

           IF ( UP-KEY )
              GO TO ENTER-END-ACCTNO.

       ENTER-SOURCE-PATH.

           IF SOURCE-PATH = SPACES
              MOVE "A15_PATH"       TO GETENV-BUF
              PERFORM GETENV-CALL
              IF ( STAT-GOOD )
                 MOVE GETENV-BUF    TO SOURCE-PATH
              ELSE
              MOVE "/USR/DATA"      TO SOURCE-PATH
              END-IF
              CALL "C$TOLOWER" USING SOURCE-PATH, VALUE 9
              MOVE SOURCE-PATH-LIST TO WR-LIST
              MOVE SOURCE-PATH      TO WR-BUF
              PERFORM CALL-WRFORM.

           MOVE "E  A090S-PATH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF NOT ( ENTER-KEY ) GO TO ENTER-SOURCE-PATH.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-SOURCE-PATH.

           MOVE VDUBUF TO DIR-ACCESS-BUF SOURCE-PATH.
           PERFORM DIR-ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "INVALID SOURCE DATA PATH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-SOURCE-PATH.

           MOVE SPACES                     TO DIR-ACCESS-BUF.
           STRING VDUBUF, "/", EXT-FILPATH-MACHINE
                  DELIMITED BY " "       INTO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "INVALID SOURCE DATA PATH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-SOURCE-PATH.

           MOVE VDUBUF                TO SOURCE-PATH.

       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E RA000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              MOVE ZEROES TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES               TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"                  TO GROUP-FLAG
              MOVE ZEROES               TO BEG-BRANCH
              MOVE 9999                 TO END-BRANCH
              GO TO CHECK-BR-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.

           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.

           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE LEGEND-END-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           GO TO CHECK-BR-SECURITY.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "N"                    TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BR-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" ) GO TO ENTER-BEG-BRANCH.


       ENTER-BEG-ACCTNO.

           MOVE LEGEND-BEG-ACCTNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-ACCTNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ACCTNO.

           MOVE VDUNUM0 TO BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-END-ACCTNO.

           MOVE LEGEND-END-ACCTNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060END-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ACCTNO.

           MOVE VDUNUM0 TO END-ACCTNO.

           IF ( BEG-ACCTNO > END-ACCTNO )
              MOVE "INVALID ACCOUNT NUMBER RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ACCTNO.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN060BEG-ACCTNO."  TO VDU.
           MOVE ZEROES                TO BEG-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-ACCTNO."  TO VDU.
           MOVE 999999                TO END-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

       ENTER-PARAMETERS-END.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

           IF ( F1-KEY ) MOVE STAT---BAD   TO PROCESS-STAT.
           IF ( F7-KEY ) MOVE STAT---BAD   TO PROCESS-STAT.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************

           IF ( GROUP-FLAG = "Y" )
              MOVE "S  A000BEG-BRANCH."    TO VDU
              MOVE ENT-GROUP               TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH."    TO VDU
              MOVE SPACES                  TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH."    TO VDU
              MOVE BEG-BRANCH              TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH."    TO VDU
              MOVE END-BRANCH              TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "S  A090S-PATH."        TO VDU.     
           MOVE SOURCE-PATH             TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN060BEG-ACCTNO."       TO VDU.
           MOVE BEG-ACCTNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-ACCTNO."       TO VDU.
           MOVE END-ACCTNO                 TO VDUNUM0.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *********************************************
       SCAN-GR1-FILE SECTION.
      *********************************************

           MOVE "WI/GRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( GRSCAN-STATUS NOT = "00"   ) OR
              ( GRSCAN-KEY      = SPACES )
              MOVE "XX" TO VDUSTATUS
              GO TO SCAN-GR1-FILE-EXIT.

           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE GRSCAN-KEY TO VDUBUF.
           MOVE "00"       TO VDUSTATUS.

       SCAN-GR1-FILE-EXIT.
           EXIT.

      *******************************************
       EXECUTION-MODULE SECTION.
      *******************************************

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO EXECUTION-NEXT-BRANCH.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       EXECUTION-NEXT-GROUP.
           IF ( GROUP-FLAG NOT = "Y" )
              GO TO EXECUTION-NEXT-BRANCH.

           PERFORM SEARCH-GR-NEXT.
           IF ( WGR-SRESULT = 0 )
              GO TO EXECUTION-MODULE-EXIT.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO EXECUTION-PROCESS-BRANCH.

       EXECUTION-NEXT-BRANCH.
           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-BAD             ) OR
              ( BR-NO > END-BRANCH )
              GO TO EXECUTION-MODULE-EXIT.

       EXECUTION-PROCESS-BRANCH.

           PERFORM PROCESS-BRANCH-DRIVER.

           GO TO EXECUTION-NEXT-GROUP.

       EXECUTION-MODULE-EXIT.
           EXIT.

      *********************************************************
       PROCESS-BRANCH-DRIVER SECTION.
      *********************************************************

      *     MOVE 9999                       TO PROGID-SLEEP-COUNTER.
      *     PERFORM SEND-PROGRESS-INDICATOR.

      D    STOP MESS.
           MOVE SOURCE-PATH  TO OLM-ALL-BASE.
           MOVE BR-MACHINE   TO OLM-ALL-MACHINE.
           MOVE BR-DATA-PATH TO OLM-ALL-DATA.
           MOVE OLM-ALL-PATH TO OLM-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              IF OLM-ALL-MACHINE = MACHINE-R1
                 MOVE MACHINE-R2 TO OLM-ALL-MACHINE
                 MOVE OLM-ALL-PATH TO OLM-PATH ACCESS-BUF
                 PERFORM ACCESS-CALL
                 IF ( STAT-BAD )
                    MOVE "MISSING A15 FILE" TO D01-TEXT 
                    MOVE OLM-ALL-PATH       TO D01-MEMO
                    PERFORM WRITE-DETAIL-LINE
                    GO TO PROCESS-BRANCH-DRIVER-END.

           MOVE SOURCE-PATH  TO LN-ALL-BASE.
           MOVE BR-MACHINE   TO LN-ALL-MACHINE.
           MOVE BR-DATA-PATH TO LN-ALL-DATA.
           MOVE LN-ALL-PATH  TO LN-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              IF LN-ALL-MACHINE = MACHINE-R1
                 MOVE MACHINE-R2 TO LN-ALL-MACHINE
                 MOVE LN-ALL-PATH TO LN-PATH ACCESS-BUF
                 PERFORM ACCESS-CALL
                 IF ( STAT-BAD )
                    MOVE "MISSING A15(LN) FILE" TO D01-TEXT 
                    MOVE LN-ALL-PATH            TO D01-MEMO
                    PERFORM WRITE-DETAIL-LINE
                    GO TO PROCESS-BRANCH-DRIVER-END.

            IF EXT-ROUTE-BUF = "00"
              STRING "REPORT IN PROGRESS......", BR-NO INTO LEGEND
              PERFORM SEND-LEGEND.

           MOVE EXT-FILPATH-BASE      TO FILPATH-BASE.
           MOVE BR-MACHINE            TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH          TO FILPATH-DATA.

           MOVE "B"                   TO LM-PATH-OVERRIDE.
           MOVE "N"                   TO OLM-EOF-SW
                                         LM-EOF-SW
                                         ERROR-DATE-SW. 

      *    PERFORM OPEN-LM1-FILE.
           PERFORM OPEN-OLM1-FILE.
           PERFORM OPEN-LN1-FILE.

           MOVE BEG-ACCTNO            TO OLM-ACCTNO.
           MOVE 0                     TO OLM-DATE.
           MOVE 0                     TO OLM-SEQ.

           PERFORM START-OLM1-FILE.

           MOVE LM-PATH-OWNBR         TO QLM1-WBEG-BRNO.     
           MOVE BEG-ACCTNO            TO QLM1-WBEG-ACCTNO.   
           MOVE END-ACCTNO            TO QLM1-WEND-ACCTNO.   
           MOVE 00000000              TO QLM1-WBEG-DATE-REV.
           MOVE 99999999              TO QLM1-WEND-DATE-REV. 
           MOVE 000                   TO QLM1-WBEG-SEQ.     
           MOVE 999                   TO QLM1-WEND-SEQ.    

      *    PERFORM START-LM1-FILE.

           PERFORM PROCESS-READ-OLM1.
      *    PERFORM PROCESS-READ-LM1.

       PROCESS-BRANCH-NEXT-LM.

      *     IF LM-EOF-SW  = "Y" AND
           IF OLM-EOF-SW = "Y"
              GO TO PROCESS-BRANCH-DRIVER-END.              

           SUBTRACT 99999999 FROM OLM-DATE   
              GIVING WS-DATE-FULL-UNREVERSE. 

           IF (WS-DATE-FULL-UNREVERSE < 19500101   OR 
               WS-DATE-FULL-UNREVERSE > 20241231 ) AND
               OLM-DATE           NOT = 00000000   
      *       MOVE "MISSING S35 RECORD" TO D01-TEXT
      *       SUBTRACT 99999999 FROM OLM-DATE  
      *           GIVING WS-DATE-FULL-UNREVERSE 
      D       STOP MESS
              MOVE "Y"                  TO ERROR-DATE-SW
              MOVE BR-NO                TO D01-BRNO 
              MOVE OLM-ACCTNO           TO D01-ACCTNO
              MOVE OLM-DATE             TO D01-DATE-REV
              MOVE OLM-SEQ              TO D01-SEQ
              MOVE WS-DATE-FULL-MM      TO D01-MM
              MOVE "-"                  TO D01-D1
              MOVE WS-DATE-FULL-DD      TO D01-DD 
              MOVE "-"                  TO D01-D2
              MOVE WS-DATE-FULL-YYYY    TO D01-YYYY
              MOVE OLM-MEMO             TO D01-MEMO
              MOVE BR-NO                TO LN-OWNBR
              MOVE OLM-ACCTNO           TO LN1-KEY
              PERFORM READ-LN1-FILE 
              IF IO-BAD
                 MOVE "LN-REC MISSING " TO D01-TEXT 
                 MOVE ZEROES            TO D01-CURBAL
                 MOVE SPACES            TO D01-POCD
                 MOVE ZEROES            TO D01-POFFDATE
              ELSE  
                 MOVE LN-CURBAL         TO D01-CURBAL
                 MOVE LN-POCD           TO D01-POCD
                 MOVE LN-POFFDATE       TO D01-POFFDATE
                 MOVE SPACES            TO D01-TEXT
              END-IF
              PERFORM WRITE-DETAIL-LINE 
              PERFORM PROCESS-READ-OLM1 
              GO TO PROCESS-BRANCH-NEXT-LM.
 
      *    IF WS-LM1-KEY < WS-OLM1-KEY
      *       MOVE "MISSING A15 RECORD" TO D01-TEXT
      *       SUBTRACT 99999999 FROM LM-DATE-REVERSED 
      *           GIVING WS-DATE-FULL-UNREVERSE 
      *       MOVE LM-BRNO              TO D01-BRNO 
      *       MOVE LM-ACCTNO            TO D01-ACCTNO
      *       MOVE LM-DATE-REVERSED     TO D01-DATE-REV
      *       MOVE LM-SEQ               TO D01-SEQ
      *       MOVE WS-DATE-FULL-MM      TO D01-MM
      *       MOVE "-"                  TO D01-D1
      *       MOVE WS-DATE-FULL-DD      TO D01-DD 
      *       MOVE "-"                  TO D01-D2
      *       MOVE WS-DATE-FULL-YYYY    TO D01-YYYY
      *       MOVE LM-MEMO              TO D01-MEMO
      *       PERFORM WRITE-DETAIL-LINE       
      *       PERFORM PROCESS-READ-LM1 
      *       GO TO PROCESS-BRANCH-NEXT-LM.

      *     PERFORM COMPARE-FIELDS.

           PERFORM PROCESS-READ-OLM1.  
      *    PERFORM PROCESS-READ-LM1. 

           GO TO PROCESS-BRANCH-NEXT-LM.

       PROCESS-BRANCH-DRIVER-END.

      *    PERFORM CLOSE-LM1-FILE.
           IF ERROR-DATE-SW = "Y"
              MOVE "N"    TO ERROR-DATE-SW
              MOVE SPACES TO DETAIL-LINE  
              PERFORM WRITE-DETAIL-LINE. 

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       PROCESS-READ-OLM1 SECTION.
      ******************************************************************

           PERFORM READ-OLM1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999          TO WS-OLM-BRNO
              MOVE 99999999      TO WS-OLM-ACCTNO  
              MOVE 99999999      TO WS-OLM-DATE-REV
              MOVE 999           TO WS-OLM-SEQ              
              MOVE "Y"           TO OLM-EOF-SW
              GO TO EXIT-PROCESS-READ-OLM1
           ELSE 
              MOVE LM-PATH-OWNBR TO WS-OLM-BRNO
              MOVE OLM-ACCTNO    TO WS-OLM-ACCTNO  
              MOVE OLM-DATE      TO WS-OLM-DATE-REV
              MOVE OLM-SEQ       TO WS-OLM-SEQ.  

      *     PERFORM SEND-PROGRESS-INDICATOR.
      *     PERFORM TRAP-INTERRUPT.

           IF WS-OLM-DATE-REV = 99999999
              GO TO PROCESS-READ-OLM1.

           ADD  01 TO RECORDS-PROCESSED
                      A15-RECORD-CTR.

       EXIT-PROCESS-READ-OLM1.
           EXIT.

      ******************************************************************
       PROCESS-READ-LM1 SECTION.
      ******************************************************************

           PERFORM READ-LM1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999          TO WS-LM-BRNO
              MOVE 99999999      TO WS-LM-ACCTNO  
              MOVE 99999999      TO WS-LM-DATE-REV
              MOVE 999           TO WS-LM-SEQ              
              MOVE "Y"           TO LM-EOF-SW
              GO TO EXIT-PROCESS-READ-LM1
           ELSE 
              MOVE LM-PATH-OWNBR TO WS-LM-BRNO
              IF LM-ACCTNO        = ZEROES
                 MOVE LM-ACCTNO  TO WS-LM-ACCTNO
              ELSE
                 MOVE LM-ACCTNO     TO WORK-ACCTNO    
                 MOVE WORK-ACCTNO38 TO WS-LM-ACCTNO16
                 MOVE 01            TO WS-LM-ACCTNO78
              END-IF  
              MOVE LM-DATE       TO WS-LM-DATE-REV
              MOVE LM-SEQ        TO WS-LM-SEQ 
              ADD 1              TO S35-RECORD-CTR.

       EXIT-PROCESS-READ-LM1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

      *    MOVE LM-PATH-OWNBR TO LM-BRNO
      *    MOVE OLM-NUMBER    TO LM-ACCTNO.
      *    MOVE OLM-DATE      TO LM-DATE.
      *    MOVE OLM-SEQ       TO LM-SEQ.
      *
      *    PERFORM READ-LM1-FILE.
      *    IF IO-FG NOT = 0
      *       MOVE "MISSING S35 RECORD" TO D01-TEXT
      *       MOVE BR-NO        TO D01-BRNO
      *       MOVE LM-ACCTNO    TO D01-ACCTNO
      *       MOVE LM1-KEY      TO D01-A15
      *       PERFORM WRITE-DETAIL-LINE
      *       GO TO EXIT-COMPARE-FIELDS.

      *     INSPECT OLM-MEMO REPLACING ALL X"09" BY " ".

      * THIS GOT FIXED IN THE UPGRADE BUT THE FILES HAVENT BEEN UPDATED YET
      * THIS X96 GOT CONVERTED AS A ? SOMEHOW IN S35
      *    INSPECT OLM-MEMO REPLACING ALL X"96" BY "-".
      * UNCOMMENT LINE ABOVE WHEN FILES ARE RERUN AND REMOVED THE BELOW
      * 3 LINES. 11/1/2023
      *     MOVE OLM-MEMO TO HOLD-MEMO-A15.
      *     IF HOLD-MEMO-FB = "FB ITA"
      *        INSPECT OLM-MEMO REPLACING ALL X"96" BY "?".
      *
      ** ONLY TEST FIRST 30 CHARACTERS OF TRANSFER MEMO, THEN ACCTNO GOT
      ** CHANGED TO 8 DIGITS
      *     IF HOLD-MEMO-TFR = "TFR FR BR" OR "TFR TO BR"
      *        MOVE LM-MEMO  TO HOLD-MEMO-S35
      *        IF HOLD-MEMO-A15 NOT = HOLD-MEMO-S35
      *           MOVE "LM-MEMO"   TO D01-TEXT
      *           MOVE WS-LM1-KEY  TO D01-KEY  
      *           MOVE OLM-MEMO    TO D01-A15
      *           MOVE LM-MEMO     TO D01-S35
      *        END-IF
      *        GO TO COMPARE-FIELDS-TYPE.
      *        
      *     IF LM-MEMO NOT = OLM-MEMO
      **       CALL "C$JUSTIFY" USING OLM-MEMO, "L"
      *        IF ( LM-MEMO  NOT ALPHABETIC AND 
      *             LM-MEMO  NOT NUMERIC )  OR 
      *           (OLM-MEMO  NOT ALPHABETIC AND 
      *            OLM-MEMO  NOT NUMERIC )   
      *           NEXT SENTENCE
      *        ELSE
      *           MOVE "LM-MEMO"  TO D01-TEXT
      *           MOVE WS-LM1-KEY TO D01-KEY
      *           MOVE OLM-MEMO   TO D01-A15
      *           MOVE LM-MEMO    TO D01-S35
      *           PERFORM WRITE-DETAIL-LINE.
      *
      * COMPARE-FIELDS-TYPE.
      * 
      *     IF LM-MEMO-TYPE NOT = OLM-MEMO-TYPE
      *           MOVE "LM-MEMO-TYPE" TO D01-TEXT
      *           MOVE WS-LM1-KEY     TO D01-KEY 
      *           MOVE OLM-MEMO-TYPE  TO D01-A15
      *           MOVE LM-MEMO-TYPE   TO D01-S35
      *           PERFORM WRITE-DETAIL-LINE.

       EXIT-COMPARE-FIELDS.
           EXIT.
       
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD  LN-FEED                   TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-HEADER-LINES.

           MOVE DETAIL-LINE                TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 01                         TO LN-FEED.
           MOVE SPACES                     TO DETAIL-LINE.

       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
       WRITE-HEADER-LINES SECTION.
      ******************************************************************

           MOVE 01                         TO LN-COUNT.
           ADD  01                         TO PAGE-NUMBER.
           PERFORM GET-TIME.

           MOVE EXT-CONAME-SYSDATE         TO H01-DATE.
           MOVE TIME-EDIT                  TO H01-TIME.
           MOVE EXT-CONAME-CONAME          TO H01-NAME.
           CALL "C$JUSTIFY"             USING H01-NAME "C".
           MOVE PAGE-NUMBER            TO H01-PAGE.

           MOVE HEADER-LINE-01             TO PR-REC.
           MOVE 99                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE EXT-CONAME-USER-ID         TO H02-USERID.

           MOVE HEADER-LINE-02             TO PR-REC.
           ADD  01                         TO LN-COUNT.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-LINE-03             TO PR-REC.
           ADD  02                         TO LN-COUNT.
           MOVE 02                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 2                         TO LN-FEED.

       WRITE-HEADER-LINES-EXIT.
           EXIT.

      ******************************************************************
       RANGE-LINES SECTION.
      ******************************************************************

           MOVE 88                                    TO LN-COUNT.
           MOVE SPACES                                TO DETAIL-LINE.

           IF ( GROUP-FLAG = "Y" )
              MOVE "BEGINNING BRANCH              : " TO R01-PROMPT
              MOVE ENT-GROUP                          TO R01-TEXT
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH                 : " TO R01-PROMPT
              MOVE SPACES                             TO R01-TEXT
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH              : " TO R01-PROMPT
              MOVE BEG-BRANCH                         TO R01-NUMERIC
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH                 : " TO R01-PROMPT
              MOVE END-BRANCH                         TO R01-NUMERIC
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING ACCOUNT NUMBER      : "    TO R01-PROMPT.
           MOVE BEG-ACCTNO                            TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ACCOUNT NUMBER      : "    TO R01-PROMPT.
           MOVE END-ACCTNO                            TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "A15 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE A15-RECORD-CTR                        TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
      *     MOVE "S35 RECORDS PROCESSED         :"     TO R01-PROMPT.
      *     MOVE S35-RECORD-CTR                        TO R01-NUMERIC.
      *     MOVE 02                                    TO LN-FEED.
      *     PERFORM WRITE-DETAIL-LINE.
  
           MOVE "STARTING TIME                 : "    TO R01-PROMPT.
           MOVE STARTING-TIME                         TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME                 : "    TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                             TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DATABASE NAME : "                    TO R01-PROMPT.
           MOVE RET-DB-NAME                           TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       RANGE-LINES-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO LMFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LMFILE-SYSDATE.
           DISPLAY LMFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LMFILE-FORM-FIELDS TO WR-BUF.
           MOVE LMFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE LMFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE LMFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/LMFILE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       INITIAL-PROGRESS-INDICATOR.

           MOVE ZEROES                 TO PROGID-BRANCH.
           MOVE "LEGEND:B."            TO PROGID-LINE-LIST.
           MOVE PRU-LP                 TO PROGID-PRU-LP.
           MOVE EXT-ROUTE-BUF              TO PROGID-ROUTE-BUF.
           MOVE 9999                   TO PROGID-SLEEP-COUNTER.
           MOVE 250                    TO PROGID-SLEEP-COUNTER-MAX.

           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO PROGID-TIME.

           MOVE "B"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

      *-----------------------------------------------------------------
       SEND-PROGRESS-INDICATOR.

           MOVE BR-NO                      TO PROGID-BRANCH.
           MOVE PROGID-TEXT-PROCESSING     TO PROGID-TEXT.

           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO PROGID-TIME.

      *     IF EXT-ROUTE-BUF = "00"
      *        PERFORM PROGID-DISPLAY.

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
   
      * COPY "LIBLP/LPLN1RN.CPY".
      * COPYMEMBER: LIBLP/LPLN1RN
       OPEN-LN1-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH TO E-FILE.
           OPEN INPUT LN-FILE.
           IF IO-FG = 8
              GO TO OPEN-LN1-FILE.
           IF IO-FG = 7
              CLOSE LN-FILE
              GO TO OPEN-LN1-FILE.
       READ-LN1-FILE.
           PERFORM READ-IT.
           MOVE LN-PATH TO E-FILE.
           MOVE LN1-KEY TO E-KEYX.
           READ LN-FILE.
           IF IO-FG = 8
              GO TO READ-LN1-FILE.
       CLOSE-LN1-FILE.
           PERFORM CLOSE-IT.
           CLOSE LN-FILE.

      * COPY "LIBLP/LPLM1RN.CPY".
      * COPYMEMBER: LIBLP/LPLM1RN
       LOAD-LM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LM-ALL-BASE
              MOVE FILPATH-MACHINE         TO LM-ALL-MACHINE
              MOVE FILPATH-DATA            TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH.

      *-----------------------------------------------------------------
       OPEN-LM1-FILE.
           PERFORM LOAD-LM1-FILE.
           PERFORM OPEN-IT.
           MOVE LM-PATH-SQL TO E-FILE.

           IF LM1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO LM1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LM1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LM1-FILE.
           PERFORM START-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.

           IF ( LM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LM1-FILE.

      *    IF ( QLM1-WEND-BRNO = ZEROES    ) OR
      *       ( QLM1-WEND-BRNO NOT NUMERIC )
      *       MOVE LM-PATH-OWNBR        TO QLM1-WBEG-BRNO
      *       MOVE LM-PATH-OWNBR        TO QLM1-WEND-BRNO
      *       MOVE LM-ACCTNO            TO QLM1-WBEG-ACCTNO
      *       MOVE ALL "9"              TO QLM1-WEND-ACCTNO
      *       MOVE LM-DATE-REVERSED     TO QLM1-WBEG-DATE-REV
      *       MOVE ALL "9"              TO QLM1-WEND-DATE-REV
      *       MOVE LM-SEQ               TO QLM1-WBEG-SEQ
      *       MOVE ALL "9"              TO QLM1-WEND-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LM1_CURSOR CURSOR FOR
                :LM1-RETURN-CODE = EXEC SP_LMFILE_LMFILE_SELECT 
              ( :QLM1-WBEG-BRNO,     
                :QLM1-WBEG-ACCTNO,   
                :QLM1-WEND-ACCTNO,   
                :QLM1-WBEG-DATE-REV,
                :QLM1-WEND-DATE-REV, 
                :QLM1-WBEG-SEQ,     
                :QLM1-WEND-SEQ )      
           END-EXEC.

           EXEC SQL
               OPEN LM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLM1-WBEG-BRNO 
                          QLM1-WEND-BRNO
                          QLM1-WBEG-ACCTNO
                          QLM1-WEND-ACCTNO
                          QLM1-WBEG-DATE-REV
                          QLM1-WEND-DATE-REV
                          QLM1-WBEG-SEQ
                          QLM1-WEND-SEQ.

           MOVE STAT---GOOD TO LM1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LM1-FILE.
           PERFORM READ-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO. 
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           INITIALIZE QLM-REC.

           MOVE LM-BRNO          TO QLM-BRNO.
           MOVE LM-ACCTNO        TO QLM-ACCTNO.
           MOVE LM-DATE-REVERSED TO QLM-DATE-REVERSED.
           MOVE LM-SEQ           TO QLM-SEQ.

           EXEC SQL
            SELECT LMFILE.LM_BRNO,
                   LMFILE.LM_ACCTNO,
                   LMFILE.LM_DATE_REVERSED,
                   LMFILE.LM_SEQ,
                   CAST(LMFILE.LM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LMFILE.LM_LTOUCH_TIME_HHMM,
                   LMFILE.LM_LTOUCH_USERID,
                   LMFILE.LM_MEMO,
                   LMFILE.LM_MEMO_TYPE
            INTO :QLM-REC
            FROM DBO.LMFILE
            WHERE LMFILE.LM_BRNO = :QLM-BRNO
              AND LMFILE.LM_ACCTNO = :QLM-ACCTNO
              AND LMFILE.LM_DATE_REVERSED = :QLM-DATE-REVERSED
              AND LMFILE.LM_SEQ = :QLM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LM1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LM-FIELDS.

      *-----------------------------------------------------------------
       READ-LM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           INITIALIZE QLM-REC.

           EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC.

           IF ( LM1-CURSOR-STAT-GOOD )
              EXEC SQL
               FETCH LM1_CURSOR 
                INTO  
                    :QLM-BRNO,
                    :QLM-ACCTNO,
                    :QLM-DATE-REVERSED,
                    :QLM-SEQ,
                    :QLM-MEMO,
                    :QLM-MEMO-TYPE
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LM-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LM1-FILE.

           PERFORM CLOSE-IT.
           IF ( LM1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC 
              EXEC SQL
                   CLOSE LM1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LM1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LM1-CONNECTION.

           IF LM1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LM1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LM1-CONN-OPEN-SW.

      *----------------------------------------------------------------- 
   
       OPEN-OLM1-FILE.
           PERFORM OPEN-IT.
           MOVE OLM-PATH TO E-FILE.
           OPEN INPUT OLM-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLM1-FILE.
           IF IO-FG = 7
              CLOSE OLM-FILE
              GO TO OPEN-OLM1-FILE.
      *READ-OLM1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OLM-PATH TO E-FILE.
      *    MOVE OLM1-KEY TO E-KEYX.
      *    READ OLM-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OLM1-FILE.
       READ-OLM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLM-PATH TO E-FILE.
           MOVE OLM1-KEY TO E-KEYX.
           READ OLM-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLM1-FILE-NEXT.
       START-OLM1-FILE.
           PERFORM START-IT.
           MOVE OLM-PATH TO E-FILE.
           MOVE OLM1-KEY TO E-KEYX.
           START OLM-FILE KEY NOT < OLM1-KEY.
       CLOSE-OLM1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLM-FILE.

      *=================================================================
       START-MISC-CSR.
           MOVE "SSP_TCLP_SELECT" TO E-FILE.
           MOVE SPACES           TO E-KEYX.
           MOVE "START ERROR"    TO E-MSG.
           MOVE "GB/SQLINF"      TO E-PROG.

           IF ( MISC-CURSOR-STAT-GOOD )
              PERFORM CLOSE-MISC-CSR.

           MOVE "SELECT DB_NAME(),@@SERVERNAME,@@VERSION" 
              TO PASS-SQL.

           EXEC SQL
            DECLARE MISC_CURSOR CURSOR FOR
                :MISC-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN MISC_CURSOR
           END-EXEC.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

           MOVE STAT---GOOD TO MISC-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-MISC-CSR-NEXT.
           MOVE "SSP_TCLP_SELECT" TO E-FILE.
           MOVE SPACES           TO E-KEYX.
           MOVE "READ ERROR"     TO E-MSG.
           MOVE "GB/SQLINF"      TO E-PROG.

           IF ( MISC-CURSOR-STAT-GOOD )
              EXEC SQL
                 FETCH MISC_CURSOR INTO  
                   :RET-DB-NAME,
                   :RET-SERVER-NAME, 
                   :RET-VERSION
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

      *-----------------------------------------------------------------
       CLOSE-MISC-CSR.
           MOVE "SSP_TCLP_SELECT" TO E-FILE.
           MOVE SPACES           TO E-KEYX.
           MOVE "CLOSE ERROR"    TO E-MSG.
           MOVE "GB/SQLINF"      TO E-PROG.

           IF ( MISC-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE MISC_CURSOR
              END-EXEC
              MOVE STAT---BAD TO MISC-CURSOR-STAT.

      *-----------------------------------------------------------------

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

      *     IF ( NO-RECORDS-FOUND )
      *        MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
      *        PERFORM SEND-MESS.

       EXIT-PROG.

           IF ( SV-BEG-BRANCH NOT = 9999 )
              IF ( EXT-EOBUF-GROUP-FG = "Y" )
                 MOVE SV-BEG-BRANCH        TO BEG-BRANCH
                 MOVE SV-END-BRANCH        TO END-BRANCH
                 MOVE SV-GROUP-FLAG        TO GROUP-FLAG
                 MOVE SV-ENT-GROUP         TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BRANCH        TO BEG-BRANCH
                 MOVE SV-END-BRANCH        TO END-BRANCH.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH               TO FORM-NAM
              MOVE FORM-PATH               TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LMFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
