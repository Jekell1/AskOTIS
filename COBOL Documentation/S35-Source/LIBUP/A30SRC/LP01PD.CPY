      * COPYMEMBER: LIBLP/LP01PD
      ******************************************************************
      *                  LOAN SCRATCH PAD FILE
      * ID = Q (QUOTES)
      *      R (RENEWALS)
      *      L (SAVED LOANS)
      *      B (SAVED BORROWERS)
      *
      *       WHEN LOAN RECORD IS CHANGED, CHANGE
      *       ALL FIELDS BETWEEN   03  PD-RECORD
      *       AND  05  PD-REST-OF-REC.
      *
      *       N O T E:
      *                REMEMBER TO CHANGE LP01PD
      *                                   LPPDCL
      * REV:
      *  061995 JTG ADDED PD-O3INS-DLPROC
      *  062295 JTG ADDED O3 INSURANCE INSURED/ELIGIBLE CODES
      *  JTG 070995 ADDED PD-INSPREM-DLADJ, ADJUSTMENT WHEN DEALER
      *                   WROTE OUR INS. PREMIUMS WRONG
      *  JTG 071995 ADDED PD-PRLNAMT & PD-PRPDUE-DAYS
      *  JTG 072095 ADDED PD-PLAMT, CURBAL AT TIME OF P&L
      *  JTG 082495 ADDED PD-MTD-PRIN, PD-MTD-LC, PD-MTD-INT
      *                   PD-MTD-OT, LN-MTD-EX
      *  KEC 083095 ADDED PD-BNKRPT-CHAPTER
      *  JTG 092295 ADDED PD-BMO-CON-STATUS PD-BMO-REC-STATUS
      *  KEC 030696 ADDED PD-APPROVAL-LIMIT PD-APPROVAL-USERID.
      *  JTG 032996 CHANGED PD-PROCEEDS TO BE SIGNED S9(6)V99 COMP-3
      *  JTG 042296 ADDED PD-ASSIGN-RN-ACCTNO FOR BANKRUPT RENEWALS
      *  JTG 111296 CHANGED PD-INSTBL (LEVEL'S 9 & 10) TO FILLER
      *  KEC 111296 CHANGED PD-POCD TO BE REDEFINED TO BE: POCD1 & POCD2
      *  BLV 022497 ADD PD-MIN-CRED-SCORE, PD-CRED-SCORE-USERID
      *  KEC 032697 ADDED PD-APPNO: APPLICATION NUMBER
      *  BAH 071197 ADDED PD-INSFINCHG AND PD-FEEFINCHG FOR WORLD
      *  MJD 071597 CHANGED PD-CAN-LOAN-NO FROM PIC 99 COMP-3 TO PIC XX.
      *             REMOVED PD2-CAN-NO.
      *  JTG 081297 ADDED PD-GIFTNET & PD-GIFTNET-OUR-PO FOR WORLD A90
      *                   PD-GIFTNET-EXTR & PD-SHIP-SAME-FG
      *  JTG 082097 CHANGED PD-CONPROFILE & PD-RECPROFILE TO SIXTY MONTH TABLES
      *             ADDED PD-PROC-DISBURSED
      *             ADDED PD-PY-YYYYMM-RECDEL & PD-MTD-PAYS-RECDEL
      *             ADDED PD-AD-D-INS-COVR   & PD-APR-FINAMT
      *  BAH 091297 CHANGED PD-SPREFUNDATE TO LN-TRW-1ST-30
      *  JTG 112097 EXPANED PD-RECORD BY 230 TO 1800 TO MATCH LN-REC
      *  BAH 010898 ADDED PD-TRW-STATUS, PR# 520
      *   CS 031998 ADDED PD-ORIGIN AND PD-FOLLOWUP-DATE
      *  JTG 050498 CREATED PD-TOT-PARTIAL-CHGOFF, MERCURY TRCD = 'PX'
      *  JTG 060398 ADDED PD-PO-TRCD TO HOLD THE TRCD USED TO PAYOFF THIS LOAN
      *  BAH 062498 CHANGED PD-TILLNO TO ALPHANUMERIC
      *  JTG 071598 ADDED PD-PRDLDISC-UNEARN TO HOLD CARRY OVER DISCOUNT
      *             ON RENEWALS
      *  BAH 081098 ADDED PD-YTDNOALDEL FOR MERCURY'S NEW EDIT ON ALLOWABLE
      *             DELINQUENCY, #193
      *  JTG 082698 ADDED PD-REVOLVING FLAG, REGENCY FINANCE REVOLVING
      *                   PD-PREV-STMT-DATE
      *                   PD-LAST-STMT-DATE
      *                   PD-RPRINBAL
      *                   PD-AGEING-AMT    OCCURS 8
      *  KEC 091898 ADDED PD-REVOL-CREDIT-LIMIT; REGENCY REVOLVING.
      *  KEC 100298 ADDED PD-CYCLE-DAY; REGENCY REVOLVING.
      *  JTG 093098 ALLOWED LN-CURBAL & LN-PURBAL TO GO NEGATIVE; REGENCY REVOL
      *  KEC 100598 ADDED THE FOLLOWING FOR REGENCY REVOLVING:
      *                   PD-CHARGE-INTEREST-FG
      *                   PD-CHARGE-LATECHG-FG
      *                   PD-CHARGE-MEMBERSHIP-FG
      *  JTG 101498 REMOVED PD-REVOL-CREDIT-LIMIT WILL USE LN-CREDITLIM
      *  JTG 101598 CHANGED PD-CYCLE-DAY TO PD-CYCLE-DATE REGENCY, REVOL
      *  KEC 102798 ADDED PD-INDEX-ADJUST-FACTOR FOR REGENCY REVOLVING.
      *  JTG 111998 ADDED PD-OTHBAL-TABLE REVOLVING, REGENCY
      *  JTG 112398 ADDED PD-RCURPRIN, PD-RCURINT, PD-RCURLC, PD-RCUROTH
      *             AND REMOVED RPRINBAL
      *  JTG 120198 ADDED PD-RPDUEPRIN, PD-RPDUEINT, PD-RPDUELC, PD-RPDUEOTH
      *  JTG 990301 ADDED PD-ZERO-BAL-CNT, REVOLVING
      *  JTG 990326 ADDED LN-LAST-ACCRUAL-DATE, SP-ERNDELQFRMLA = 'C'   TFC #005
      *  KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0520 ADDED PD-TRW-SPECIAL-CODE         (TFC) PR#: 00023
      *  JTG 990720 ADDED PD-ACTIVE-FG     REGENCY REVOLVING #45
      *  KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  MJD 990910 CHANGED LN-INSELIGIBLE-O3 TO BE LN-INSELIGIBLE-O1.
      *             AS PER JOHN, IT WAS MISNAMED FROM THE START.
      *  MJD 991005 ADDED LN-PRIOR-RN-FOR10-DATE AND LN-LAST-RN-FOR10-DATE.
      *             (REGMGM PR#942)
      *  KEC 991018 ADDED PD-CREATION-STATUS FOR CMSI/CSI PROJECT (LI)
      *               88  PD-CREATION-EXTERNAL            VALUE "L".
      *               88  PD-CREATION-INTERNAL            VALUE "I" " ".
      *               88  PD-CREATION-LI-VENDOR           VALUE "L".
      *
      *  MJD 991230 ADDED PD-TOTNODEFACT AND PD-YTDNODEFACT (WORLDPR#216)
      *  MJD 000119 ADDED PD-TOTNOLCHG (WORLD PR#218)
      *  BLV 000313 ADDED PD-WCBC-CREDITLIT, WORLD PR# 225
      *  MJD 000329 ADDED PD-PRIOR-STMT-COUNTER (MERC/NEWS PR#????)
      *  MJD 000616 ADDED PD-CONRATE (REGACC PR#1220)
      *  BLV 000711 REDEFINED PD-REP-CD AS PD-EMP-ID FOR WORLD
      *  MJD 000914 ADDED PD-LBOX (MFC PR#227)
      *  JTG 010227 ADDED PD-EXTRA-PRIN-LFP PD-REMAIN-INT-SINCE-LFP
      *             PD-REMAIN-PRIN-SINCE-LFP  (LFP=LAST-FULL-PAYMENT)
      *             LENDMARK, PR#1045 & PR#1380
      *  BAH 011029 ADDED PD-INTERMIM-COUPON FOR REGACC, PR# 1632
      *             ALSO ADDED PD-FOLLOWUP-CD (MISSING)
      *  BAH 011210 ADDED PD-PROMID FOR LAST 3 CHAR OF LOGIN WHO SET THE
      *             PROMISE TO PAY, REGACC PR# 1662
      *  JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99    LENDMARK PR#J021
      *             CHANGED PD-AMT-PAID-LAST TO PIC9(7)V99
      *  JTG 020123 EXPANDED PAYMNTD FIELDS TO 1,000,000
      *             EXPANDED MTD-PAYS-RECDEL FIELD TO 1,000,000 LENDMARK PR#J021
      *  BAH 020218 ADDED PD-LAST-PYPA-DATE FOR REGACC PR# 1721
      *  BAH 020502 ADDED PD-PROMDAYS-PASTDUE FOR "DAYS PAST DUE AT TIME
      *             PROMISE TAKEN", REGACC PR# 1783
      *  JTG 020530 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *             ADDED PD-MIP-MTD-CLAHPAID, PD-MIP-TOT-CLPAID,
      *                   PD-MIP-TOT-AHPAID,
      *                   PD-MIP-CLPDTH-DATE,   PD-MIP-AHPDTH-DATE
      *                   PD-MIP-FG
      *  BAH 020625 ADDED PD-PLREASON FOR REGACC PR# 1775
      *  BAH 020712 SIGNED PD-TRADEIN, REGACC PR# 1808
      *  KEC 020806 {DR#00009} PARADATA <A15> [KEN]
      *             ADDED PD-RESCISSION AND PD-RESCISSION-DATE.
      *  JTG 020912 ADDED PD-DLCOMM-PP-CMSI    PD-DLCOMM-O3-CMSI  REGACC #1846
      *                   PD-DLCOMM-PP-CMSI-FG PD-DLCOMM-O3-CMSI-FG
      *  BAH 030207 ADDED PD-INTERIM-TRW FOR REGACC PR# 16
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  JTG 040501 ADDED LOGIC TO CAUSE THE UNEARNED LOAN COST TO ROLL TO
      *             THE NEW LOAN AFTER 'RENEWAL'. NOTE: THIS COST IS REFLECTIVE
      *             OF THE DISCOUNT WHEN PURCHASING LOAN PAPER NOT CARRIED AS
      *             SALES PAPER WITH DEALER DISCOUNT.
      *             ADDED SP-LNCOST-FRMLA = 'R'                    REGENCY #2287
      *       NOTE: PD-PRDLDISC-UNEARN WILL HOLD THE COST (DISCOUNT)
      *             FOR DIRECT LOANS, EVEN THOUGH IT IS NOT TIED TO A DEALER.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * KEC 040518 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *            ADDED: 05  PD-APPNO-COMP    PIC 9(12)   COMP-6.
      * BAH 050202  REDEFINE PD-CYCLE-DATE WITH PD-BNKRPT-DISMISS-DATE
      *             NEED FOR THE CREDIT BUREAU AND DONT WANT TO READ EVERY
      *             LT B TRAILER FOR EVERY LOAN SEARCHING IF IT COULD BE
      *             A DISMISSAL...PL# 461
      * BAH 050324 ADDED PD-TRW-B-CONSUMER-IND AND PD-TRW-J-CONSUMER-IND FOR
      *            METRO2 OVERRIDE ABILITY, ACC, PR# 2502
      * BAH 050428 ADDED PD-ACH, REGACC PR# 102
      * BAH 050601 CHANGED PD-TRW-1ST-30 TO PD-TRW-DOFD-DATE, DATE OF FIRST
      *            DELINQUENCY
      *
      * KEC 070406 {PR#00169} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *            TOOK PD-ZERO-BAL-CNT  [9(3)] AND REDEFINED IT AS
      *                 PD-NEXIS-CRSCORE [9(3)].
      *            PD-NEXIS-CRSCORE IS FOR CMSI.
      *            PD-ZERO-BAL-CNT IS A REVOLVING FIELD WHICH IS NOT
      *            USED FOR CMSI.
      *
      * KEC 070726 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *            REVISED PD-APPNO TO BETTER IDENTIFY NEW & OLD APPS.
      * BAH 071015 ADDED PD-PART-RECOURSE-DATE REDEFINES PD-FILEDATE
      *           FOR REGACC PR# 184
      * BAH 071017 ADDED PD-GARNISH-EXTR AS REDEFINE OF PD-GIFTNET-EXTR
      *            MODERN PR#3086
      * BAH 110301 ADDED PD-IL-GROSS-INCOME REDEFINES PD-MIP-TOT-CLPAID 
      *            FOR WORLD "IL" GROSS INCOME PER LOAN PR# 748
      * BAH 140710 ADDED PD-IL-GROSS-INCOME2 REDEFINES PD-MIP-TOT-AHPAID 
      *            FOR WORLD "IL" GROSS INCOME PER LOAN WORLD PR# 1002
      * KEC 2016.0510 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED MA-APPNO TO USE MA-APP-NUMBER FOR MA1-KEY.
      * BLM 160615 REVISED PD-RECORD TO MATCH REVISED LOAN FIELDS, 
      *            PD0003
      * MJD 160823 ADDED PD-FEECODE-CH1-NUMERIC AND PD-FEECODE-CH2-NUMERIC.
      *            WE NEED TO EXCLUDE LIVE CHECK FEE CODES (START WITH ALPHA)
      *            CODES FROM NONMEMO-FEES WHEN CALCULATING HIGH CREDIT WHEN
      *            LC-CALC-HI-CREDIT = "L"                           61000 #1180
      * BAH 161018 REMOVED COMP-3 FROM PD1-KEY!
      * BLM 161019 CHANGED LN-INSEARN-ORDER TO PIC 99 FROM PIC 9 SINCE
      *            TABLE NOW OCCURS 10 TIMES, REDUCED FILLER (IN
      *            LOAN RECORD AREA) BY 10 BYTES
      * BAH 161220 ADDED PD-LTR FIELDS FOR F3-RECALL IN REVOLVING INPUT
      * BAH 170914 RENAMED PD-PYMMYY-RECDEL TO PD-PY-YYYYMM-RECDEL
      * BAH 171113 CHANGED PD-EARN-ORDER FROM 9 TO 99
      ******************************************************************
      * DO NOT CHANGE THE SIZE UNLESS YOU ARE EXTREMELY CAREFUL WITH REST-OF-REC
      * AND THE WK-FILE!!
       78  PD-SIZE                                  VALUE 3350.
       01  PD-REC.
           03  PD1-KEY.
               05  PD-ID               PIC X.
               05  PD-BRANCH           PIC 9(4).
               05  PD-SOC-SEC          PIC 9(10).
      * PD-RECORD SIZE = 3297
           03  PD-RECORD.
               05  PD-OWNBR            PIC 9(4).
               05  PD-ACCTNO           PIC 9(8).
               05  PD-NUMBER REDEFINES PD-ACCTNO
                                       PIC 9(8).
               05  PD-CLASS            PIC 9(3).
               05  PD-SUBCLASS         PIC 99.
               05  PD-LAWCODE          PIC 99.
      *----------------------------------------------------
               05  PD-BWTABLEX.
                   07  PD4-KEY         PIC 9(9).
                   07  FILLER          PIC X.
                   07  PD5-KEY         PIC 9(9).
                   07  FILLER          PIC X.
                   07  PD6-KEY         PIC 9(9).
                   07  FILLER          PIC X.
                   07  PD7-KEY         PIC 9(9).
                   07  FILLER          PIC X.
               05  PD-BWTABLE REDEFINES PD-BWTABLEX.
                   07  PD-BORRTBL      OCCURS 4 TIMES.
                       09  PD-SSNO     PIC 9(9).
                       09  PD-MAKERCD  PIC X.
      *----------------------------------------------------
      *    LOAN STATUS CODES
      *----------------------------------------------------
               05  PD-ORGST            PIC XX.
               05  PD-ORGBR            PIC 9(4)   COMP-3.
               05  PD-OWNST            PIC XX.
               05  PD-SPRCLASS         PIC 9(3).
               05  PD-CONVERCD         PIC X.
      ********************************************************
               05  PD-MIDSTREAM        PIC X.
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
      *         PD-LNAMT NOT = PD-ORIGPURBAL
      *         PD-CURBAL    = PD-PURBAL
      *
      *    P  - PARTIAL PURCHASE:  (BIG APPLE)
      *
      *         AT THE TIME THE ACCOUNT WAS PURCHASED,
      *         ONLY A PORTION OF THE RECEIVABLE WAS BOUGHT
      *
      *         PD-LNAMT  NOT = PD-ORIGPURBAL
      *         PD-CURBAL NOT = PD-PURBAL
      ********************************************************
               05  PD-PURCD            PIC XXX.
               05  PD-SRCD             PIC XX.
               05  PD-COLLCD           PIC XX.
               05  PD-COLLID           PIC X(20).
               05  PD-POCD             PIC XX.
                   88  PD-POCD-NONLOAN       VALUE "**" "*I" "*R".
                   88  PD-POCD-OPEN          VALUE "  ".
                   88  PD-POCD-POFF          VALUE "1 " THRU "ZZ".
                   88  PD-POCD-INRESCISSION  VALUE "*I".
                   88  PD-POCD-RESCIND       VALUE "*R".
                   88  PD-POCD-VOID          VALUE "**".
               05  FILLER                   REDEFINES PD-POCD.
                   07  PD-POCD1              PIC X.
                   07  PD-POCD2              PIC X.
               05  PD-PLCD             PIC X.
               05  PD-PLTYPE           PIC X.
               05  PD-CONFIDENTCD      PIC X.
               05  PD-ALLOTCD          PIC X.
               05  PD-RECASTCD         PIC X.
               05  PD-SKIP-TRW         PIC X.
               05  PD-STATUSFG         PIC X.
               05  PD-SMALLOAN         PIC X.
               05  PD-SALEFIN          PIC X.
               05  PD-SERVACCT         PIC X.
               05  PD-LOANTYPE         PIC X.
               05  PD-ADDON-FG         PIC X.
               05  PD-PAY-SCHLD-FG     PIC X.
               05  PD-LC-CODE          PIC 99.
               05  PD-LC-CODEX REDEFINES PD-LC-CODE PIC XX.
               05  PD-CAN-LOAN-NO      PIC XX.
               05  PD-STATFLAGS        PIC X(20).

      *----------------------------------------------------
      * LOAN DATES:  DATES ARE STORED INTERNALLY AS 00MMDDYY.
      * LEFT ROOM FOR LATER USE
      *----------------------------------------------------
      *>>XDD DATE=YYYYMMDD
               05  PD-ENTDATE          PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-LOANDATE         PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-PURDATE          PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-INTDATE          PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-CASHDATE         PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-FILEDATE         PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-RESCISSION-DATE  PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-PART-RECOURSE-DATE PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-ORIG-1STPYDATE      PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-1STPYDATE        PIC 9(8).
               05  PD-1STPYDATEX REDEFINES PD-1STPYDATE.
                   07  FILLER          PIC X(4).
                   07  PD-1STPYDA      PIC 99.
                   07  FILLER          PIC XX.
      *>>XDD DATE=YYYYMMDD
               05  PD-JDDATE           PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-PLDATE           PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-PL-TRDATE        PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-ACCELDATE        PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-REDUDATE         PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-POFFDATE         PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-PO-TRDATE        PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-LEGALDATE        PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-BNKRPTDATE       PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-BNKRPT-DISMISS-DATE PIC 9(8) COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-BNKRPT-DISCHARGE-DATE PIC 9(8) COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-BNKRPT-REAFFIRM-DATE  PIC 9(8) COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-DEBTAIDATE       PIC 9(8)      COMP-3.
      * REPOSESSION DATES:
      *>>XDD DATE=YYYYMMDD
               05  PD-REPODATE         PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-REDEEMDATE       PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-SOLDDATE         PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-INS-CONFIRM-DATE PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-PRIOR-RN-FOR10-DATE   PIC 9(8)       COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-LAST-RN-FOR10-DATE    PIC 9(8)       COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-TRW-DOFD-DATE         PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-LAST-PYPA-DATE   PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-BT-TRDATE        PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-METRO2-LAST-PMT-DATE PIC 9(8)  COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-FOLLOWUP-DATE    PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-LAST-ACCRUAL-DATE PIC 9(8)     COMP-3.
      *----------------------------------------------------
      *    RATE INFORMATION
      *----------------------------------------------------
               05  PD-APRATE           PIC 999V999   COMP-3.
               05  PD-EFFRATE          PIC 999V999   COMP-3.
               05  PD-SMPRATE          PIC 999V999   COMP-3.
               05  PD-YLDRATE          PIC 999V999   COMP-3.
               05  PD-OVRDRATE         PIC 999V999   COMP-3.
               05  PD-ENDORATE         PIC 999V999   COMP-3.
               05  PD-JDRATE           PIC 999V999   COMP-3.
               05  PD-ACCRATE          PIC 999V999   COMP-3.
               05  PD-REDURATE         PIC 999V999   COMP-3.
               05  PD-CONRATE          PIC 999V999 COMP-3.
               05  PD-SERVPER          PIC 9(3)V99   COMP-3.
      *----------------------------------------------------
      *    LOAN DISCLOSURE INFORMATION
      *----------------------------------------------------
               05  PD-SELLPR           PIC 9(7)V99   COMP-3.
               05  PD-DOWNPY           PIC 9(7)V99   COMP-3.
               05  PD-TRADEIN          PIC S9(7)V99  COMP-3.
               05  PD-LNAMT            PIC 9(7)V99   COMP-3.
               05  PD-FINCHG           PIC 9(7)V99   COMP-3.
               05  PD-INTCHG           PIC 9(7)V99   COMP-3.
               05  PD-SERCHG           PIC 9(5)V99   COMP-3.
               05  PD-EXTCHG           PIC 9(5)V99   COMP-3.
               05  PD-MAINTFEE         PIC 9(5)V99   COMP-3.
               05  PD-POINTS           PIC 99V99     COMP-3.
               05  PD-PROCEEDS         PIC S9(7)V99  COMP-3.
      * LOANS ORIGINAL TERM AND REVOLVING ACCOUNTS DRAW PERIOD:
               05  PD-ORGTERM          PIC 9(3)      COMP-3.
               05  PD-1STPYAMT         PIC 9(7)V99   COMP-3.
               05  PD-NOREGPY          PIC 9(3)      COMP-3.
               05  PD-REGPYAMT         PIC 9(7)V99   COMP-3.
               05  PD-LASTPYAMT        PIC S9(7)V99  COMP-3.
               05  PD-PROCASH          PIC S9(7)V99  COMP-3.
               05  PD-TILLNO           PIC 9(2).
      * HOLDS SPECIFIC AMOUNTS FOR EACH STATE IF NEEDED:
      * MISSISSIPPI--------------------------------------------
      * Y -  PRORATA REFUND ADJUSTMENT OCCURED ON THIS ACCOUNT
      *      AFTER NORMAL PAYOFF
      * A -  PRORATA REFUND ADJUSTMENT WAS GIVEN FOR AN ACCOUNT
      *--------------------------------------------------------

      * GEORGIA------------------------------------------------
      * STATE-WORKER HOLDS TO 8% PORTION OF THE SERVICE CHARGE
      *--------------------------------------------------------
               05  PD-STATE-FLAG       PIC X.
               05  PD-STATE-WORKER     PIC S9(7)V99 COMP-3.
               05  PD-STATE-WORKER2    PIC S9(5)V99 COMP-3.
      *----------------------------------------------------
      *    FEE INFORMATION
      *----------------------------------------------------
               05  PD-FEETBL           OCCURS 20 TIMES.
                   07  PD-FEECODE      PIC XX.
                   07  PD-FEECODEX REDEFINES PD-FEECODE.
                       09  PD-FEECODE-CH1 PIC X.
                       88  PD-FEECODE-CH1-NUMERIC
                                  VALUE '1' '2' '3' '4' '5'
                                        '6' '7' '8' '9' '0'.
                       09  PD-FEECODE-CH2 PIC X.
                       88  PD-FEECODE-CH2-NUMERIC
                                  VALUE '1' '2' '3' '4' '5'
                                        '6' '7' '8' '9' '0'.
                   07  PD-FEEAMT       PIC 9(5)V99   COMP-3.
                   07  PD-FEEOURS      PIC X.
                   07  PD-FEEFINCHG    PIC X.
      *--------------------------------------------------------
      *    INSURANCE INFORMATION
      *
      *    NOTE:
      *         IF PD-ADDON-FG = "Y"
      *            ALL LEVELS CONTAIN THE MOST RECENT
      *            INSURANCE ADDON INFO.
      *
      *    LEVEL: #1   CREDIT LIFE       #6   OTH INS 3
      *           #2   A&H               #7   OTH INS 4
      *           #3   PER. PROPERTY     #8   OTH INS 5
      *           #4   OTH INS 1
      *           #5   OTH INS 2
      *--------------------------------------------------------
               05  PD-INSCODES.
                   07  PD-INSURED      PIC X.
                       88  PD-INS-NO-ONE    VALUE "N".
                       88  PD-INS-SINGLE    VALUE "M" "S".
                       88  PD-INS-JOINT     VALUE "J".
                   07  PD-INSTYPES     PIC X.
                    88  PD-INSTYPES-VALID   VALUE "A" THRU "Z"
                                                  "0" "1" "2" "3" "4"
                                                  "5" "6" "7" "8" "9".
                    88  PD-INSTYPES-DL      VALUE "C"
                                                  "G" "I" "J" "M" "P"
                                                  "T" "X" "Y" "5" "6"
                                                  "7".
                    88  PD-INSTYPES-LL      VALUE "B"
                                                  "A" "F" "H" "L" "O"
                                                  "S" "U" "W" "2" "3"
                                                  "4".
                    88  PD-INSTYPES-AH      VALUE "D"
                                                  "F" "G" "K" "L" "M"
                                                  "O" "P" "Q" "V" "W"
                                                  "X" "3" "4" "6" "7"
                                                  "9" "0".
                    88  PD-INSTYPES-PP      VALUE "E"
                                                  "H" "J" "K" "L" "M"
                                                  "U" "Y" "Z" "3" "6"
                                                  "9".
                    88  PD-INSTYPES-PP2     VALUE "R"
                                                  "S" "T" "V" "W" "X"
                                                  "2" "4" "5" "7" "8"
                                                  "0".
                    88  PD-INSTYPES-O1      VALUE "1"
                                                  "A" "I" "O" "P" "Q"
                                                  "U" "Y" "Z" "2" "3"
                                                  "4" "5" "6" "7" "8"
                                                  "9" "0".
                   07  PD-INSELIGIBLE  PIC X.
                    88  PD-INSELIGIBLE-VALID  VALUE "A" THRU "Z"
                                                    "0" "1" "2" "3" "4"
                                                    "5" "6" "7" "8" "9".
                    88  PD-INSELIGIBLE-DL     VALUE "C"
                                                    "G" "I" "J" "M" "P"
                                                    "T" "X" "Y" "5" "6"
                                                    "7".
                    88  PD-INSELIGIBLE-LL     VALUE "B"
                                                    "A" "F" "H" "L" "O"
                                                    "S" "U" "W" "2" "3"
                                                    "4".
                    88  PD-INSELIGIBLE-AH     VALUE "D"
                                                    "F" "G" "K" "L" "M"
                                                    "O" "P" "Q" "V" "W"
                                                    "X" "3" "4" "6" "7"
                                                    "9" "0".
                    88  PD-INSELIGIBLE-PP     VALUE "E"
                                                    "H" "J" "K" "L" "M"
                                                    "U" "Y" "Z" "3" "6"
                                                    "9".
                    88  PD-INSELIGIBLE-PP2    VALUE "R"
                                                    "S" "T" "V" "W" "X"
                                                    "2" "4" "5" "7" "8"
                                                    "0".
                    88  PD-INSELIGIBLE-O1     VALUE "1"
                                                    "A" "I" "O" "P" "Q"
                                                    "U" "Y" "Z" "2" "3"
                                                    "4" "5" "6" "7" "8"
                                                    "9" "0".
               05  PD-INSTBL             OCCURS 10 TIMES.
                   07  PD-INSCOMP         PIC 99.
                   07  PD-INSOURS         PIC X.
      *>>XDD DATE=YYYYMMDD
                   07  PD-INSEFF-DATE     PIC 9(8)      COMP-3.
                   07  PD-INSTRM         REDEFINES PD-INSEFF-DATE
                                          PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
                   07  PD-INSEXP-DATE     PIC 9(8)      COMP-3.
                   07  PD-INSPREM         PIC 9(5)V99   COMP-3.
                   07  PD-INSCOVR         PIC 9(7)V99   COMP-3.
                   07  PD-INSCOMM         PIC 9(5)V99   COMP-3.
                   07  PD-INSANTICERN     PIC S9(7)V99  COMP-3.
                   07  PD-INSANTICADJ     PIC S9(7)V99  COMP-3.
                   07  PD-INSUFERN        PIC S9(7)V99  COMP-3.
                   07  PD-INSACCERN       PIC S9(7)V99  COMP-3.
                   07  PD-INSEARN-ORDER   PIC 99.
                   07  PD-INSFINCHG       PIC X.

      *-----------------------------------------------------------------
      *   PD-PY-YYYYMM-RECDEL IS IN CCYYMM FORMAT.
      *-----------------------------------------------------------------
               05  PD-PY-YYYYMM-RECDEL       PIC 9(6)      COMP-3.
               05  PD-MTD-PAYS-RECDEL     PIC S9(7)V99  COMP-3.
               05  PD-GARNISH-EXTR        PIC X.
               05  PD-SHIP-SAME-FG        PIC X.
               05  PD-AD-D-INS-COVR       PIC 9(7)      COMP-3.
               05  PD-TOT-PARTIAL-CHGOFF  PIC S9(7)V99  COMP-3.
               05  PD-LPO                 PIC X(4).
               05  PD-REP-CD              PIC X(8).
               05  PD-EMP-ID              PIC 9(8)      COMP-3.
               05  PD-PROCESS-CD          PIC X.
               05  PD-WCBC-CREDITLIM      PIC S9(5)     COMP-3.
               05  PD-FOLLOWUP-CD         PIC X(4).
      * FIELDS FOR MIP INSURANCE
               05  PD-MIP-FG              PIC X.
               05  PD-MIP-MTD-CLAHPAID    PIC S9(5)V99  COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-MIP-CLPDTH-DATE          PIC 9(8)      COMP-3.
               05  PD-MIP-TOT-CLPAID      PIC S9(7)V99  COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-MIP-AHPDTH-DATE          PIC 9(8)      COMP-3.
               05  PD-MIP-TOT-AHPAID      PIC S9(7)V99  COMP-3.
               05  PD-IL-GROSS-INCOME     PIC S9(7)V99  COMP-3.
               05  PD-IL-GROSS-INCOME2    PIC S9(7)V99  COMP-3.
               05  PD-RESCISSION          PIC X(1).
               05  PD-INTERIM-TRW         PIC X(1).
      *----------------------------------------------------
      *    SALES FINANCE PURCHASE INFORMATION
      *----------------------------------------------------
               05  PD-DLNO             PIC 9(4)V99   COMP-3.
               05  PD-DLPOOL           PIC 9(5)      COMP-3.
               05  PD-DLRECOURSE       PIC X.
               05  PD-DLPARTRCAMT      PIC 9(7)V99   COMP-3.
               05  PD-ORIGPURAMT       PIC 9(7)V99   COMP-3.
               05  PD-PURBAL           PIC S9(7)V99  COMP-3.
               05  PD-DLRVPRIN         OCCURS 3
                                       PIC 9(7)V99   COMP-3.
               05  PD-DLRVCHGS         OCCURS 3
                                       PIC 9(7)V99   COMP-3.
               05  PD-DLPARVHELD       PIC 9(7)V99   COMP-3.
               05  PD-DLPARVPAID       PIC 9(7)V99   COMP-3.
               05  PD-DLDISC           PIC 9(7)V99   COMP-3.
      *----------------------------------------------------
      *    CURRENT LOAN INFORMATION
      *----------------------------------------------------
      *>>XDD DATE=YYYYMMDD
               05  PD-LCPDTH-DATE           PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-INTPDTH-DATE          PIC 9(8)      COMP-3.
               05  PD-CURBAL           PIC S9(7)V99  COMP-3.
               05  PD-INTBAL           PIC S9(7)V99  COMP-3.
               05  PD-LCBAL            PIC S9(7)V99  COMP-3.
               05  PD-OTHBAL           PIC S9(7)V99  COMP-3.
               05  PD-OT2BAL           PIC S9(7)V99  COMP-3.
               05  PD-LCPAID           PIC S9(7)V99  COMP-3.
               05  PD-LCPARTIALS       PIC S9(7)V99  COMP-3.
               05  PD-STMT-COUNTER     PIC 9(5)      COMP-3.
               05  PD-PRIOR-STMT-COUNTER PIC 9(5)  COMP-3.
               05  PD-FROZEN-REASON    PIC XX.
               05  PD-COUPON-STATEMENT PIC X.
               05  PD-MONTHEND-BAL     PIC 9(7)V99   COMP-3.
      *-------------------------------------------------------
      *    LOAN EARNINGS ANTICIPATED INFORMATION:
      *
      *    PD-ANTICERN - NET ANTICERN = ORIGINAL AMOUNT AT
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
      *    PD-ANTICADJ - ADJUSTMENTS TO ANTICERN
      *                  1. THIS WILL REFLECT THE AMOUNT OF THE REBATE
      *                     NOTE:
      *                           IF SALES FINANCE AND PARTICIPATION
      *                           EXISTS WHERE BY THE ANTICIPATED
      *                           CHARGE IS CARRIED ADJUSTED, THIS
      *                           ADJUSTMENT WILL REFLECT THE FINANCE
      *                           COMPANY'S PORTION OF THE REBATE
      *
      *    PD-UFERN    - THAT PORTION OF ANTICERN TAKEN INTO
      *                  INCOME AT BOOKING OR PURCHASE
      *
      *    PD-ACCERN   - TOTAL INCOME (INCLUDING UFERN) TAKEN
      *                  FROM NET-ANTICERN (ANTICERN)
      *
      *    PD-ERNDELQ-STOP - FOR CERTAIN FORMULA'S, WHEN THIS FLAG = "Y"
      *                      ACCRUAL EARNINGS ARE STOPPED.
      *
      *    SO:
      *       GROSS ANTICERN = PD-ANTICERN + PD-ANTICADJ
      *       NET ANTICERN   = PD-ANTICERN
      *       UNEARNED       = PD-ANTICERN - PD-ACCERN
      *       TRUE ANTICERN  = PD-ANTICERN - PD-UFERN
      *
      *    ANTICTBL ELEMENTS:
      *       LEVEL 1 - INTEREST CHARGES
      *         "   2 - 1ST PAY EXTENSION CHARGES
      *         "   3 - SERVICE CHARGE
      *         "   4 - MAINTENANCE FEES OR OTHER FEES
      *         "   5 - DEALER DISCOUNTS
      *         "   6 - OTHER
      *
      *    PD-EARN-ORDER: IDENTIFIES WHERE TO HOLD THIS LEVELS
      *                   EARNINGS IN THE LOAN PAYMENT RECORDS
      *
      *              1 -  LP-EARN1       4 -  LXE-EARN(1)
      *              2 -  LP-EARN2       5 -  LXE-EARN(2)
      *              3 -  LP-EARN3       6 -  LXE-EARN(3)
      *
      *-------------------------------------------------------
               05  PD-ANTICTERM        PIC 9(3)      COMP-3.
               05  PD-ANTICTBL         OCCURS 10 TIMES.
                   07  PD-ANTICERN     PIC S9(7)V99  COMP-3.
                   07  PD-ANTICADJ     PIC S9(7)V99  COMP-3.
                   07  PD-UFERN        PIC S9(7)V99  COMP-3.
                   07  PD-ACCERN       PIC S9(7)V99  COMP-3.
                   07  PD-EARN-ORDER   PIC 99.
               05  PD-LMO-PRIOR-INTEREST PIC S9(7)V99  COMP-3.
               05  PD-PRIOR-UNEARNED   PIC S9(7)V99  COMP-3.
               05  PD-ERNDELQ-STOP     PIC X.
               05  PD-NONACRERN        PIC S9(7)V99  COMP-3.
               05  PD-PRIOR-NONACRERN  PIC S9(7)V99  COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-ERN-IB-PDTH-DATE      PIC 9(8)      COMP-3.
               05  PD-ERN-IB-PRINBAL   PIC S9(7)V99  COMP-3.
               05  PD-ERN-IB-INTOWE    PIC S9(7)V99  COMP-3.
      *----------------------------------------------------
      *    LOAN TOTALS
      *----------------------------------------------------
      * TOTAL INCOME SINCE THE ACCOUNT WAS BOOKED ON TO SYSTEM:
      * ALL PD-ACCERN, PD-TOTSERACCT AND THOSE PORTIONS OF
      * PD-TOTINT, PD-TOTLCHG AND PD-TOTDEF WHICH WERE
      * EARNED SINCE BOOKING AS A OWNED ACCOUNT:
               05  PD-TOTERN           PIC S9(7)V99  COMP-3.
      * TOTAL MONIES COLLECTED FROM OTHER CHARGES,
      * SINCE BOOKING ACCOUNT, BOTH NORMAL/MIDSTREAM
      * PURCHASE AND SERVICING:
               05  PD-TOTOTH           PIC S9(7)V99  COMP-3.
      * TOTAL INTEREST CHARGES COLLECTED FROM LOAN INCEPTION:
               05  PD-TOTINT           PIC S9(7)V99  COMP-3.
      * TOTAL LATE CHARGES COLLECTED FROM LOAN INCEPTION:
               05  PD-TOTLCHG          PIC S9(7)V99  COMP-3.
      * TOTAL DEFERMENT CHARGES COLLECTED FROM LOAN INCEPTION:
               05  PD-TOTDEF           PIC S9(7)V99  COMP-3.
      * TOTAL INCOME DURING SERVICE ACCOUNT STATUS:
               05  PD-TOTSERACCT       PIC S9(7)V99  COMP-3.
               05  PD-LCWAIVERS        PIC S9(5)V99  COMP-3.
               05  PD-INTWAIVERS       PIC S9(5)V99  COMP-3.
               05  PD-TOTPAYMNTD       PIC S9(7)V99  COMP-3.
               05  PD-TOTEXCPAYMNTD    PIC S9(7)V99  COMP-3.
               05  PD-TOTNODEF         PIC S99       COMP-3.
               05  PD-YTDNODEF         PIC S99       COMP-3.
               05  PD-TOTNODEFACT      PIC S99   COMP-3.
               05  PD-YTDNODEFACT      PIC S99   COMP-3.
      * TOTAL ALLOWABLE (DEFERMENTS) DELINQUENCY, THESE DEFERMENTS
      * ARE NOT INCLUDED IN TOTNODEF AND YTDNODEF:
               05  PD-TOTNOALDEL       PIC S99       COMP-3.
               05  PD-YTDNOALDEL       PIC S99       COMP-3.
               05  PD-YTDINTPD         PIC S9(7)V99  COMP-3.
               05  PD-LYRINTPD         PIC S9(7)V99  COMP-3.
               05  PD-TOTINTPD         PIC S9(7)V99  COMP-3.
               05  PD-RECASTINT        PIC 9(7)V99   COMP-3.
               05  PD-RECASTPRIN       PIC 9(7)V99   COMP-3.
      *----------------------------------------------------
      *    REBATE INFORMATION
      *----------------------------------------------------
               05  PD-REBATE-TAB.
                   07  PD-CL-REBATE    PIC X.
                   07  PD-AH-REBATE    PIC X.
                   07  PD-PP-REBATE    PIC X.
                   07  PD-INT-REBATE   PIC X.
                   07  PD-SC-REBATE    PIC X.
                   07  PD-MF-REBATE    PIC X.
                   07  PD-DF-REBATE    PIC X.
                   07  PD-EX-REBATE    PIC X.
                   07  PD-O1-REBATE    PIC X.
                   07  PD-O2-REBATE    PIC X.
                   07  PD-O3-REBATE    PIC X.
                   07  PD-O4-REBATE    PIC X.
                   07  PD-O5-REBATE    PIC X.
                   07  PD-O6-REBATE    PIC X.
                   07  PD-O7-REBATE    PIC X.
      *-REBATES FOR EXPANDED UNEARNED TABLE
                   07  PD-UN-O7-REB    PIC X.
                   07  PD-UN-O8-REB    PIC X.
                   07  PD-UN-O9-REB    PIC X.
                   07  PD-UN-O10-REB   PIC X.
               05  FILLER             REDEFINES PD-REBATE-TAB.
                   07  PD-REBATE       PIC X OCCURS 19.

               05  PD-PLREASON         PIC XX.
      *----------------------------------------------------
      *    COLLECTION INFORMATION
      *----------------------------------------------------
               05  PD-COLLECTOR        PIC X(3).
               05  PD-ACTIONCD         PIC X(4).
               05  PD-ACTIONCD2        PIC X(4).
               05  PD-ATCODE           PIC X(3).
               05  PD-RECODE           PIC X(3).
               05  PD-ESCROW-FG        PIC X.
               05  PD-PROMDAYS-PASTDUE PIC S9(3)     COMP-3.
               05  PD-PROMID           PIC X(3).
      *>>XDD DATE=YYYYMMDD
               05  PD-PROMDATE         PIC 9(8)      COMP-3.
               05  PD-PROMAMT          PIC 9(7)V99   COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-PROMDATE2        PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-DATE-PAID-LAST   PIC 9(8)      COMP-3.
               05  PD-AMT-PAID-LAST    PIC 9(7)V99   COMP-3.
               05  PD-LASTCFORMNO      PIC 9(5)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-LASTCFORMDATE    PIC 9(8)      COMP-3.
      * NO OF TIMES EXTRACTED:
               05  PD-DELCNT           PIC 99.
      * NO OF DELINQUENT FORMS SENT:
               05  PD-DELCNT2          PIC 99.
               05  PD-CREDITLIM        PIC 9(7)      COMP-3.
               05  PD-CRSCORE          PIC S9(3)     COMP-3.
      *----------------------------------------------------
      *    SOLICIATE INFORMATION
      *----------------------------------------------------
               05  PD-LASTSFORMNO       PIC 9(5)      COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-LASTSFORMDATE     PIC 9(8)      COMP-3.
      * NO OF SOLICITATION FORMS SENT:
               05  PD-SOLCNT           PIC 99.
               05  PD-MESSAGE          PIC X(30).
      *-------------------------------------------------------------------
      *    THIS FIELD IS SET MANUALLY IN THE INQUIRIES, ONLY IF BRANCH
      *    FLAG ALLOWS.  THE EXTRACTION UPDATE WILL COMPARE THE EXTRACTION
      *    DATE(TODAY), AFTER INCREMENTED BY # OF DAYS, TO THE SECOND
      *    PROMISE DATE, IF FIRST IS LESS, THEY STAY THE SAME, IF FIRST
      *    IS AFTER SECOND, MOVE SECOND TO FIRST, AND 0 SECOND.
      *----------------------------------------------------
      *    PRIOR LOAN INFORMATION
      *----------------------------------------------------
               05  PD-OLDACCTNO        PIC 9(8)      COMP-3.
               05  PD-PRLNNO           PIC 9(8)      COMP-3.
               05  PD-PRBAL            PIC S9(7)V99  COMP-3.
               05  PD-PRINTDUE         PIC S9(7)V99  COMP-3.
               05  PD-PRFINCHGRB       PIC S9(7)V99  COMP-3.
               05  PD-PRINSRB          PIC S9(7)V99  COMP-3.
               05  PD-PRPOCD           PIC XX.
      *>>XDD DATE=YYYYMMDD
               05  PD-PRPODATE         PIC 9(8)      COMP-3.
               05  PD-PRDELINQAMT      PIC S9(7)V99  COMP-3.
               05  PD-PRLNAMT          PIC 9(7)V99   COMP-3.
               05  PD-PRPDUE-DAYS      PIC S9(5)     COMP-3.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  JTG 040501 ADDED LOGIC TO CAUSE THE UNEARNED LOAN COST TO ROLL TO
      *             THE NEW LOAN AFTER 'RENEWAL'. NOTE: THIS COST IS REFLECTIVE
      *             OF THE DISCOUNT WHEN PURCHASING LOAN PAPER NOT CARRIED AS
      *             SALES PAPER WITH DEALER DISCOUNT.
      *             ADDED SP-LNCOST-FRMLA = 'R'                    REGENCY #2287
      *       NOTE: PD-PRDLDISC-UNEARN WILL HOLD THE COST (DISCOUNT)
      *             FOR DIRECT LOANS, EVEN THOUGH IT IS NOT TIED TO A DEALER.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  PD-PRDLDISC-UNEARN    PIC S9(7)V99  COMP-3.
               05  PD-PRCONPROFILE     PIC 9(12)     COMP-3.
      *----------------------------------------------------
      *    LOAN ADDON INSURANCE CONSOLIDATED INFORMATION
      * NOTE:
      *      PD-SUM-MAINTFEE = TOTAL CONTRACT CHARGE
      *----------------------------------------------------
               05  PD-SUM-ORIGPURAMT   PIC 9(7)V99   COMP-3.
               05  PD-SUM-LNAMT        PIC 9(7)V99   COMP-3.
               05  PD-SUM-FINCHG       PIC 9(7)V99   COMP-3.
               05  PD-SUM-INTCHG       PIC 9(7)V99   COMP-3.
               05  PD-SUM-ORGTERM      PIC 9(3)      COMP-3.
               05  PD-SUM-REGPYAMT     PIC 9(7)V99   COMP-3.
               05  PD-SUM-EXTCHG       PIC 9(5)V99   COMP-3.
               05  PD-SUM-SERCHG       PIC 9(5)V99   COMP-3.
               05  PD-SUM-MAINTFEE     PIC 9(7)V99   COMP-3.
      *----------------------------------------------------
      *    UNIT PERIOD INFORMATION
      *----------------------------------------------------
               05  PD-UNITPER-INFO.
                   07  PD-UNITPER-CD    PIC X.
                       88  PD-UNITPER-CD-DAYS    VALUE "D" "S" "W" "B".
                       88  PD-UNITPER-CD-MONTHS  VALUE "M" "Y".
                   07  PD-UNITPER-FREQ  PIC 999        COMP-3.
               05  PD-DAYS-IN-UNITPER   PIC 9(3)       COMP-3.
               05  PD-UNITPER-PER-YEAR  PIC 9(3)V9(12) COMP-3.

               05  PD-REPOCD            PIC X.
               05  PD-BR-TFR-FG         PIC X.
               05  PD-O3INS-DLPROC      PIC 9(7)V99   COMP-3.
               05  PD-INSPREM-DLADJ     PIC S9(5)V99  COMP-3.
      *        PLAMT = CURBAL AT TIME OF P&L:
               05  PD-PLAMT             PIC 9(7)V99   COMP-3.
               05  PD-MTD-PRIN          PIC S9(7)V99  COMP-3.
               05  PD-MTD-LC            PIC S9(5)V99  COMP-3.
               05  PD-MTD-INT           PIC S9(5)V99  COMP-3.
               05  PD-MTD-OT            PIC S9(5)V99  COMP-3.
               05  PD-MTD-EX            PIC S9(7)V99  COMP-3.
               05  PD-BNKRPT-CHAPTER    PIC 9(2).
               05  PD-BMO-CON-STATUS    PIC X.
               05  PD-BMO-REC-STATUS    PIC X.

               05  PD-APP-NUMBER        PIC 9(09)    COMP-3.
               05  PD-GIFTNET           PIC X.
               05  PD-GIFTNET-OUR-PO    PIC 9(7)     COMP-3.
               05  PD-PROC-DISBURSED    PIC X.
               05  PD-NOODDPY           PIC 9.
               05  PD-CONPROFILE        OCCURS 5
                                        PIC 9(12)     COMP-3.
               05  PD-RECPROFILE        OCCURS 5
                                        PIC 9(12)     COMP-3.
               05  PD-TRW-STATUS        PIC XX.
               05  PD-ORIGIN            PIC X(4).
               05  PD-PO-TRCD           PIC XX.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  PD-REVOLVING          PIC X.
               05  PD-ACTIVE-FG          PIC X.
      * FIELD HOLD THE NO. OF CYCLES WHEN THE REVOLVING LOANS BAL WAS ZERO:
               05  PD-ZERO-BAL-CNT       PIC 9(3)      COMP-3.
               05  PD-NEXIS-CRSCORE      PIC 9(3)      COMP-3.
               05  PD-TRW-B-CONSUMER-IND PIC X(2).
               05  PD-PARVHELD-PAID-FG   PIC X.
      *-------------------------------------------------------------------------
               05  PD-ACCT-FROZEN        PIC X.
               05  PD-TRW-SPECIAL-CODE   PIC X(2).
      *-----------------------------------------------------------------
      *    PD-CREATION-STATUS:  USED TO KNOW IF CREATED BY A OUTSIDE
      *                         VENDOR (EXTERNAL) OR BY THE LOAN MANAGER
      *                         (INTERNAL).
      *-----------------------------------------------------------------
               05  PD-CREATION-STATUS    PIC X(1).
                   88  PD-CREATION-EXTERNAL            VALUE "L".
                   88  PD-CREATION-INTERNAL            VALUE "I" " ".
                   88  PD-CREATION-LI-VENDOR           VALUE "L".
               05  PD-TOTNOLCHG                PIC S9(3) COMP-3.
               05  PD-LBOX                     PIC X(01).
      *-----------------------------------------------------------------
      *    THE FOLLOWING LFP FIELDS ARE USED FOR CALCULATION OF
      *    AMORTIZATION INTEREST RE: REAL ESTATE
      *    WHERE LFP == LAST-FULL-PAYMENT
      *-----------------------------------------------------------------
               05  PD-EXTRA-PRIN-LFP           PIC S9(7)V99 COMP-3.
               05  PD-REMAIN-INT-SINCE-LFP     PIC S9(7)V99 COMP-3.
               05  PD-REMAIN-PRIN-SINCE-LFP    PIC S9(7)V99 COMP-3.
      *-----------------------------------------------------------------
               05  PD-INTERIM-COUPON           PIC X.
1846  *  JTG 020905 ADDED PD-DLCOMM-O3-CMSI PD-DLCOMM-PP-CMSI  REGACC #1846
1846           05  PD-DLCOMM-O3-CMSI           PIC 9(5)V99 COMP-3.
1846           05  PD-DLCOMM-PP-CMSI           PIC 9(5)V99 COMP-3.
               05  PD-TRW-J-CONSUMER-IND       PIC X(2).
               05  PD-ACH                      PIC X.
               05  PD-1099-C-FG                PIC X.
               05  PD-TRW-COMPLIANCE-CODE      PIC XX.
               05  PD-UNWIND-REASON-CODE       PIC X(2).
      * NEW FIELDS
               05  PD-PBI-PBD-FG               PIC X.
               05  PD-OLD-ACCT-FORMAT-FG       PIC X.
               05  PD-PAPERLESS-STMT-FG        PIC X.
               05  PD-APPROVED-BY-USER         PIC X(10).
               05  PD-APPROVED-BY-CODE         PIC X(3).
      *>>XDD DATE=YYYYMMDD
               05  PD-APPROVED-BY-DATE         PIC 9(8)     COMP-3.
               05  PD-APPROVED-BY-TIME         PIC 9(6)     COMP-3.
               05  PD-TAX-AMT                  PIC S9(7)V99 COMP-3.
               05  PD-FUTURE-AMT1              PIC S9(7)V99 COMP-3.
               05  PD-FUTURE-AMT2              PIC S9(7)V99 COMP-3.
               05  PD-FUTURE-AMT3              PIC S9(7)V99 COMP-3.
               05  PD-FUTURE-AMT4              PIC S9(7)V99 COMP-3.
               05  PD-FUTURE-AMT5              PIC S9(7)V99 COMP-3.
               05  PD-FUTURE-AMT6              PIC S9(7)V99 COMP-3.
               05  PD-FUTURE-AMT7              PIC S9(7)V99 COMP-3.
               05  PD-FUTURE-AMT8              PIC S9(7)V99 COMP-3.
               05  PD-FUTURE-AMT9              PIC S9(7)V99 COMP-3.
               05  PD-FUTURE-AMT10             PIC S9(7)V99 COMP-3.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *>>XDD DATE=YYYYMMDD
               05  PD-LTOUCH-DATE              PIC 9(8)       COMP-3.
               05  PD-LTOUCH-TIME              PIC 9(6)       COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-PREV-STMT-DATE           PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  PD-LAST-STMT-DATE           PIC 9(8)     COMP-3.
               05  FILLER                      PIC X(630).

      *---------------  END OF LOAN RECORD   -----------------------
      *
      * ANY CHANGES TO PD-REST-OF-REC, YOU MUST CHECK LP01WK
      * SIZE PD-REST-OF-REC = 497 PD-RECORD = 3297 - 497 = 2800 = LN-REC
      *
               05  PD-REST-OF-REC.
                   07  PD-HOLD-COLLATERAL.
                       10  PD-MSG1             PIC X(30).
                       10  PD-MSG2             PIC X(30).
                   07  PD-DLPAID               PIC X.
                   07  PD-CKNO                 PIC 9(6)      COMP-3.

                   07  PD-FREE-SPACE.
                       10  PD-FREE1            PIC X(70).
                       10  PD-FREE2            PIC X(70).
                       10  FILLER              PIC X(44).
                   07  PD-NEW-FIELDS REDEFINES PD-FREE-SPACE.
                       10  PD-FINAMT           PIC 9(7)V99     COMP-3.
                       10  PD-PROCEEDS-FG      PIC X.
                       10  PD-POINTS-FG        PIC X.
                       10  PD-OVRDRATE-FG      PIC X.
                       10  PD-INS-FLAGS        OCCURS 10.
                           15  PD-INS-FG       PIC X.
                       10  PD-SERCHG-FG        PIC X.
                       10  PD-MAINTFEE-FG      PIC X.
                       10  PD-EXTCHG-FG        PIC X.
                       10  PD-REGPYAMT-FG      PIC X.
                       10  PD-FEECD            OCCURS 20
                                               PIC XX.
                       10  PD-PRPAID-AHEAD     PIC S9(7)V99   COMP-3.
                       10  PD-RESTRICT-ADJ     PIC S9(7)V99   COMP-3.
                       10  PD-DUAL-LOAN-ADJ    PIC S9(7)V99   COMP-3.
                       10  PD-CAN-LOAN-MAX     PIC S9(7)V99   COMP-3.
                       10  PD-PERDOWN          PIC 999V99     COMP-3.
                       10  PD-SELLPR-FG        PIC X.
                       10  PD-DOWNPY-FG        PIC X.
                       10  PD-PERDOWN-FG       PIC X.
                       10  PD-SELL-DOWN-FG     PIC X.
                       10  PD-BALLOON-FG       PIC X.
                       10  PD-BALLOON-LAST-FG  PIC X.
                       10  PD-FEECD-FG         OCCURS 2
                                               PIC X.
                       10  PD-INS-SWITCHES     OCCURS 10.
                           15  PD-INS-TERM-SW  PIC X.
                           15  PD-COVERAGE-SW  PIC X.
                           15  PD-PREMIUM-SW   PIC X.
                       10  PD-DLDISC-FG        PIC X.
                       10  PD-DLRVPRIN-FG      OCCURS 3
                                               PIC X.
                       10  PD-DLRVCHGS-FG      OCCURS 3
                                               PIC X.
                       10  PD-DLPARVHELD-FG    PIC X.
                       10  PD-DLPARVPAID-FG    PIC X.
                       10  PD-ONE-MTH-NO-DAY-EXT-SW        PIC X.
                           88  PD-ONE-MTH-NO-DAY-EXT       VALUE "Y".
                       10  PD-ASSIGN-RN-ACCTNO PIC X.
      *  ' '  - NON RENEWAL VALUE
      *  'N'  - RENEWAL ASSIGN NEW ACCOUNT NO
      *  'S'  - RENEWAL ASSIGN SAME ACCOUNT NO
                       10  PD-APR-FINAMT        PIC S9(7)V99 COMP-3.
1846                   10  PD-DLCOMM-PP-CMSI-FG PIC X.
1846                   10  PD-DLCOMM-O3-CMSI-FG PIC X.
      * NEEDS THESE LTR FIELDS FOR F3-RECALL IN REVOLVING INPUT
      *>>XDD DATE=YYYYMMDD
                       10  PD-LTR-CYCLE-DATE    PIC 9(8)       COMP-3.
                       10  PD-LTR-INDEX-ADJUST-FACTOR OCCURS 2
                                                PIC S999V999   COMP-3.
                       10  PD-LTR-CHARGE-MEMBERSHIP-FG PIC X.
                       10  PD-LTR-CHARGE-INTEREST-FG   PIC X.
                       10  PD-LTR-CHARGE-LATECHG-FG    PIC X.

                       10  FILLER               PIC X(27).
      *                -----------------------------------------------
                   07  PD-POSTING-BR           PIC 9(4)    COMP-3.
                   07  PD-OTHBR-FG             PIC X.
                   07  PD-APPROVAL-LIMIT       PIC 9(7)    COMP-3.
                   07  PD-APPROVAL-USERID      PIC X(3).
                   07  PD-MIN-CRED-SCORE       PIC 9(4)    COMP-3.
                   07  PD-CRED-SCORE-USERID    PIC X(3).
                   07  FILLER                  PIC X(231).
           03  PD2-KEY.
               05  PD2-ID                      PIC X.
               05  PD2-BRANCH                  PIC 9(4).
               05  PD2-BWNAME                  PIC X(33).
E
