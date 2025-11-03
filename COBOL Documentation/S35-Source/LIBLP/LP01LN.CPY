      * COPYMEMBER: LIBLP/LP01LN
      ******************************************************************
      *
      *   DESCRIPTION: LOAN MASTER FILE
      *
      *   RECORD SIZE: 2800 BYTES, RECORD SIZE AFFECTS LP01WK!
      *
      *   LN1-KEY - LN-OWNBR, LN-ACCTNO
      *   LN3-KEY - LN-OWNBR, LN-CLASS, LN-ACCTNO
      *   LN4-KEY - LN-OWNBR, LN-SSNO(1)
      *   LN5-KEY - LN-OWNBR, LN-SSNO(2)
      *
      ******************************************************************
      *   I M P O R T A N T - - - - - - - -
      *       REMEMBER TO CHANGE:
      *       LPFDPD
      *       (AND ALL PLACES FIELDS ARE INITIALIZED)
      ******************************************************************
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
      *  JTG 032996 CHANGED LN-PROCEEDS TO BE SIGNED S9(7)V99 COMP-3
      *  JTG 111296 CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      *  KEC 012397 ADDED LN-APPNO: APPLICATION NUMBER(TIED TO MA-APPNO)
      *  BAH 071197 ADDED LN-INSFINCHG AND LN-FEEFINCHG FOR WORLD FROM A90
      *  MJD 080197 CHANGED LN-CAN-LOAN-NO TO PIC XX (WAS PIC 99 COMP-3)
      *  JTG 081297 ADDED LN-GIFTNET AND LN-GIFTNET-OUR-PO FOR WORLD A90
      *                   LN-GIFTNET-EXTR, LN-SHIP-SAME-FG
      *  JTG 082097 CHANGED LN-CONPROFILE & LN-RECPROFILE TO SIXTY MONTH TABLES
      *             ADDED LN-PROC-DISBURSED
      *             ADDED LN-PY-YYYYMM-RECDEL & LN-MTD-PAYS-RECDEL
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
      *  MJD 991005 ADDED LN-PRIOR-RN-FOR10-DATE AND LN-LAST-RN-FOR10-DATE.
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
      *  JTG 020117 CHANGED FIELDS FROM S9(7)V99 TO S9(7)V99    LENDMARK PR#J021
      *             CHANGED LN-AMT-PAID-LAST TO PIC9(7)V99
      *  JTG 020123 EXPANDED PAYMNTD FIELDS TO 1,000,000
      *             EXPANDED MTD-PAYS-RECDEL FIELD TO 1,000,000 LENDMARK PR#J021
      *  BAH 020214 ADDED LN-LAST-PYPA-DATE FOR REGACC PR# 1721
      *  BAH 020502 ADDED LN-PROMDAYS-PASTDUE FOR "DAYS PAST DUE AT TIME
      *             PROMISE TAKEN", REGACC PR# 1783
      *  JTG 020530 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *             ADDED LN-MIP-MTD-CLAHPAID, LN-MIP-TOT-CLPAID,
      *                   LN-MIP-TOT-AHPAID,
      *                   LN-MIP-CLPDTH-DATE,   LN-MIP-AHPDTH-DATE
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
      * BAH 050202  REDEFINE LN-CYCLE-DATE WITH LN-BNKRPT-DISMISS-DATE
      *             NEED FOR THE CREDIT BUREAU AND DONT WANT TO READ EVERY
      *             LT B TRAILER FOR EVERY LOAN SEARCHING IF IT COULD BE
      *             A DISMISSAL...PL# 461
      * BAH 050324 ADDED LN-TRW-B-CONSUMER-IND AND LN-TRW-J-CONSUMER-IND FOR
      *            METRO2 OVERRIDE ABILITY, ACC, PR# 2502
      * BAH 050428 ADDED LN-ACH, REGACC PR# 102
      * BAH 050601 CHANGED LN-TRW-1ST-30 TO LN-TRW-DOFD-DATE, DATE OF FIRST
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
      * BLM 160615 A30 FILE CHANGES: 8 DIGIT DATES, REMOVE REVOLVING FIELDS,
      *            REMOVE REDEFINES, REMOVE LN2-KEY, CHANGE LN1-KEY TO BE
      *            OWNBR/ACCTNO, CHANGE LN3-KEY TO BE NON-CONTIGUOUS OWNBR/
      *            CLASS/ACCTNO, INCREASE 9(4) AMTS TO BE 9(5), INCREASE
      *            9(6) AMTS TO BE 9(7), CHANGED COMP-6 FIELDS TO BE COMP-3,
      *            INCREASED EARNINGS TABLE FROM 6 TO 10, INCREASE INSURANCE
      *            TABLE FROM 8 TO 10, ADDED LN-PL-TRDATE, LN-PO-TRDATE,
      *            LN-BT-TRDATE, LN-PBI-PBD-FG, LN-BNKRPT-DISCHARGE-DATE,
      *            LN-BNKRPT-REAFFIRM-DATE, LN-METRO2-LAST-PMT-DATE,
      *            LN-LTOUCH-DATE, LN-LTOUCH-TIME,
      *            ADDED UNUSED OF 750.  INCREASED CLASS TO 9(3).
      *            CHANGED LN-APPNO 9(6) TO LN-APP-NUMBER 9(09) COMP-3.
      *            INCREASED FEE TABLE FROM 10 TO 20. 
      *            INCREASED MAINTFEE TO 9(5)V99, SUM-MAINTFEE TO 9(7)V99,
      *            ADDED LN-OLD-ACCT-FORMAT-FG, LN-PAPERLESS-STMT-FG.
      *            CHANGED LN-TILLNO FROM PIC X TO PIC 99.
      *            REMOVED LN-MORELN. REMOVE LN-GIFTNET-EXTR.
      *            CHANGED LN-EMP-ID TO NOT BE A REDEF OF LN-REP-CD.
      *            CHANGED LN-IL-GROSS-INCOME & LN-IL-GROSS-INCOME2 TO 
      *            NOT BE REDEFS OF MIP FIELDS.  ADD LN-O6-REBATE &
      *            LN-O7-REBATE.  ADD 10 FUTURE AMTS.
      *CS 160629 REMOVE LN-ZERO-BAL-CNT AND PUT IN THE LTR(REVOLV TRAILER) IT
      *           TRACKS THE NUMBER OF STATEMENTS TO PRINT IF ZERO BALANCE
      * MJD 160823 ADDED LN-FEECODE-CH1-NUMERIC AND LN-FEECODE-CH2-NUMERIC.
      *            WE NEED TO EXCLUDE LIVE CHECK FEE CODES (START WITH ALPHA)
      *            CODES FROM NONMEMO-FEES WHEN CALCULATING HIGH CREDIT WHEN
      *            LC-CALC-HI-CREDIT = "L"                           61000 #1180
      *  CS 160824 PUT LN-ZERO-BAL-CNT BACK IN LOAN RECORD, ITS CHECKED IN
      *            EOM & ITS BETTER HERE, TOOK OUT OF LIBLP/LP01LTR
      *  CS 160913 CHANGED LN-INSEARN-ORDER TO PIC 99 FROM PIC 9 SINCE THE
      *            TABLE OCCURS 10 TIMES.
      * BLM 161019 REDUCED FILLER FROM 650 TO 640 FOR THE CHANGE ABOVE.
      * BLM 170214 FIX REDEF OF LN-ORIG-1STPYDATE SO DAY IS IN CORRECT
      *            POSITION FOR YYYYMMDD, PD0006
      * BAH 170914 RENAMED LN-PYMMYY-RECDEL TO LN-PY-YYYYMM-RECDEL
      * BAH 171113 CHANGED LN-EARN-ORDER FROM 9 TO 99
      * BAH 180628 REMOVED REDEFINE LN-NUMBER
      * BAH 181130 GLOBAL LNFILE, MOVED LN-OWNBR UNDER LN1-KEY
      * KEC 190219 A32 FFR - REMOVED REDEFINE FOR LN-LC-CODEX; UNUSED
      * BAH 190307 A32 REMOVED LN-FROZEN-REASON1, BECAUSE IT REPLACED A COMP-3
      *            FIELD FROM THE PAST WE WERE TESTING FOR A X'00'. USING A
      *            WORKING STORAGE FIELD NOW
      * BAH 190307 REMOVED THE BREAK OUT OF LN-COLLCD, MADE PIC X(2).
      *  CS 190430 ADD FUTURE FIELDS, KEPT AT 2800 TO MATCH WK-SAVE-REC
      *            WHICH SHOULD ALSO MATCH PD-REC
      * CS 190612 CHANGE LN-INSTBLE-SIZE TO 49 FROM 48
      * BAH 2020-0323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 2023-0600 ADDED FILLER
      * BAH 2023-0714 REMOVED LN6 AND LN7
      * JKC 2023-0912 ADDED "LN-" PREFIX TO THE FOLLOWING:
      *          LN-BULK-PURCHASE, LN-CONVERSION-NORMAL
      * JKC 2024-0404 ADDED LN-LOS-APP-ID PIC 9(18) COMP-3.
      *
      **************************************************************************
       78  LN-SIZE                     VALUE 2800.
       78  LN-INSTBL-SIZE              VALUE 49.
       01  LN-REC.
           03  LN1-KEY.
               05  LN-OWNBR        PIC 9(4).
               05  LN-ACCTNO       PIC 9(8).
           03  LN-CLASS            PIC 9(3).
           03  LN-SUBCLASS         PIC 99.
           03  LN-LAWCODE          PIC 99.
      *----------------------------------------------------
           03  LN-BWTABLEX.
               05  LN-SSNO1        PIC 9(9).
               05  LN-MAKERCD1     PIC X.
               05  LN-SSNO2        PIC 9(9).
               05  LN-MAKERCD2     PIC X.
               05  LN-SSNO3        PIC 9(9).
               05  LN-MAKERCD3     PIC X.
               05  LN-SSNO4        PIC 9(9).
               05  LN-MAKERCD4     PIC X.
           03  LN-BWTABLE          REDEFINES LN-BWTABLEX.
               05  LN-BORRTBL      OCCURS 4 TIMES.
                   07  LN-SSNO     PIC 9(9).
                   07  LN-MAKERCD  PIC X.
      *----------------------------------------------------
      *    LOAN STATUS CODES
      *----------------------------------------------------
           03  LN-ORGST            PIC XX.
           03  LN-ORGBR            PIC 9(4)   COMP-3.
           03  LN-OWNST            PIC XX.
           03  LN-SPRCLASS         PIC 9(3).
           03  LN-CONVERCD         PIC X.
               88  LN-BULK-PURCHASE                  VALUE "B".
               88  LN-CONVERSION-NORMAL              VALUE "Y".

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
           03  LN-COLLCD           PIC X(2).
           03  LN-COLLID           PIC X(20).
           03  LN-POCD             PIC XX.
               88  LN-POCD-NONLOAN       VALUE "**" "*I" "*R" "*U".
               88  LN-POCD-OPEN          VALUE "  ".
               88  LN-POCD-POFF          VALUE " 1" THRU "ZZ".
               88  LN-POCD-INRESCISSION  VALUE "*I".
               88  LN-POCD-RESCIND       VALUE "*R".
               88  LN-POCD-PB-RESC       VALUE "*P".
               88  LN-POCD-UNWIND        VALUE "*U".
               88  LN-POCD-VOID          VALUE "**".
               88  LN-POCD-TRANSFERRED   VALUE "BT".
           03  LN-PLCD             PIC X.
           03  LN-PLTYPE           PIC X.
           03  LN-CONFIDENTCD      PIC X.
           03  LN-ALLOTCD          PIC X.
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
           03  LN-CAN-LOAN-NO      PIC XX.
           03  LN-STATFLAGS        PIC X(20).
           03  LN-STATUSFLAGS      REDEFINES LN-STATFLAGS
                                   OCCURS 20 TIMES
                                   INDEXED BY STFG.
               05  LN-STATFG       PIC X.
      $XFD DATE=YYYYMMDD
           03  LN-ENTDATE          PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-LOANDATE         PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-PURDATE          PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-INTDATE          PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-CASHDATE         PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-FILEDATE         PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-RESCISSION-DATE  PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-PART-RECOURSE-DATE PIC 9(8)    COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-ORIG-1STPYDATE      PIC 9(8)      COMP-3.
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
      $XFD DATE=YYYYMMDD
           03  LN-1STPYDATE        PIC 9(8).
           03  LN-1STPYDATEX       REDEFINES LN-1STPYDATE.
               05  LN-1STPY-YYYYMM PIC 9(6).
               05  LN-1STPYDA      PIC 99.
      $XFD DATE=YYYYMMDD
           03  LN-JDDATE           PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-PLDATE           PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-PL-TRDATE        PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-ACCELDATE        PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-REDUDATE         PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-POFFDATE         PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-PO-TRDATE        PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-LEGALDATE        PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-BNKRPTDATE       PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-BNKRPT-DISMISS-DATE PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-BNKRPT-DISCHARGE-DATE PIC 9(8) COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-BNKRPT-REAFFIRM-DATE  PIC 9(8) COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-DEBTAIDATE       PIC 9(8)      COMP-3.
      * REPOSESSION DATES:
      $XFD DATE=YYYYMMDD
           03  LN-REPODATE         PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-REDEEMDATE       PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-SOLDDATE         PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-INS-CONFIRM-DATE PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-PRIOR-RN-FOR10-DATE   PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-LAST-RN-FOR10-DATE    PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-TRW-DOFD-DATE         PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-LAST-PYPA-DATE   PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-BT-TRDATE        PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
      * LN-METRO2-LAST-PMT-DATE IS NOT BEING USED ANYWHERE
           03  LN-METRO2-LAST-PMT-DATE PIC 9(8)  COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-FOLLOWUP-DATE    PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-LAST-ACCRUAL-DATE PIC 9(8)     COMP-3.
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
           03  LN-CONRATE          PIC 999V999  COMP-3.
           03  LN-SERVPER          PIC 9(3)V99   COMP-3.
      *----------------------------------------------------
      *    LOAN DISCLOSURE INFORMATION
      *----------------------------------------------------
           03  LN-SELLPR           PIC 9(7)V99   COMP-3.
           03  LN-DOWNPY           PIC 9(7)V99   COMP-3.
           03  LN-TRADEIN          PIC S9(7)V99  COMP-3.
           03  LN-LNAMT            PIC 9(7)V99   COMP-3.
           03  LN-FINCHG           PIC 9(7)V99   COMP-3.
           03  LN-INTCHG           PIC 9(7)V99   COMP-3.
           03  LN-SERCHG           PIC 9(5)V99   COMP-3.
           03  LN-EXTCHG           PIC 9(5)V99   COMP-3.
           03  LN-MAINTFEE         PIC 9(5)V99   COMP-3.
           03  LN-POINTS           PIC 99V99     COMP-3.
           03  LN-PROCEEDS         PIC S9(7)V99   COMP-3.
      * LOANS ORIGINAL TERM AND REVOLVING ACCOUNTS DRAW PERIOD:
           03  LN-ORGTERM          PIC 9(3)      COMP-3.
           03  LN-1STPYAMT         PIC 9(7)V99   COMP-3.
           03  LN-NOREGPY          PIC 9(3)      COMP-3.
           03  LN-REGPYAMT         PIC 9(7)V99   COMP-3.
           03  LN-LASTPYAMT        PIC S9(7)V99  COMP-3.
           03  LN-PROCASH          PIC S9(7)V99  COMP-3.
      * HOLDS A "Y" WHEN P&L'S ARE POSTED FOR AN EXTRACTION (RMSEXT)
           03  LN-TILLNO           PIC 9(2).
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
      * 4% PORTION = LN-SERCHG - LN-STATE-WORKER
      *--------------------------------------------------------
           03  LN-STATE-FLAG       PIC X.
           03  LN-STATE-WORKER     PIC S9(7)V99 COMP-3.
           03  LN-STATE-WORKER2    PIC S9(5)V99 COMP-3.
      *----------------------------------------------------
      *    FEE INFORMATION
      *----------------------------------------------------
           03  LN-FEETBL           OCCURS 20 TIMES.
               05  LN-FEECODE      PIC XX.
               05  LN-FEEAMT       PIC 9(5)V99   COMP-3.
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
      *           #4  OTHER INS 1      #9  OTH INS 6
      *           #5  OTHER INS 2     #10  OTH INS 7
      *********************************************************************
           03  LN-INSTBL              OCCURS 10 TIMES.
               05  LN-INSCOMP         PIC 99.
               05  LN-INSOURS         PIC X.
      $XFD DATE=YYYYMMDD
               05  LN-INSEFF-DATE     PIC 9(8)      COMP-3.
      * I CAN ALMOST GUARANTEE YOU WILL NEVER FIND A TERM IN THIS ONCE
      * IT IS A LOAN! IT IS USED IN CONVERSIONS AND LOAN INPUT
      * LNLIST ALWAYS DISPLAYS THE DATE, NEVER A TERM    BAH 2019-0325
               05  LN-INSTRM          REDEFINES LN-INSEFF-DATE
                                      PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
               05  LN-INSEXP-DATE     PIC 9(8)      COMP-3.
               05  LN-INSPREM         PIC 9(5)V99   COMP-3.
               05  LN-INSCOVR         PIC 9(7)V99   COMP-3.
               05  LN-INSCOMM         PIC 9(5)V99   COMP-3.
               05  LN-INSANTICERN     PIC S9(7)V99  COMP-3.
               05  LN-INSANTICADJ     PIC S9(7)V99  COMP-3.
               05  LN-INSUFERN        PIC S9(7)V99  COMP-3.
               05  LN-INSACCERN       PIC S9(7)V99  COMP-3.
               05  LN-INSEARN-ORDER   PIC 99.
               05  LN-INSFINCHG       PIC X.
      * THIS WILL CONVERT AS MMYY UNTIL WE GO CHANGE IT! 6/17/2016 BAH
           03  LN-PY-YYYYMM-RECDEL       PIC 9(6)      COMP-3.
           03  LN-MTD-PAYS-RECDEL     PIC S9(7)V99  COMP-3.
           03  LN-GARNISH-EXTR        PIC X.
           03  LN-SHIP-SAME-FG        PIC X.
           03  LN-AD-D-INS-COVR       PIC 9(7)      COMP-3.
           03  LN-TOT-PARTIAL-CHGOFF  PIC S9(7)V99  COMP-3.
           03  LN-LPO                 PIC X(4).
           03  LN-REP-CD              PIC X(8).
           03  LN-EMP-ID              PIC 9(8)      COMP-3.
           03  LN-PROCESS-CD          PIC X.
           03  LN-WCBC-CREDITLIM      PIC S9(5)     COMP-3.
           03  LN-FOLLOWUP-CD         PIC X(4).
      * FIELDS FOR MIP INSURANCE
           03  LN-MIP-FG              PIC X.
           03  LN-MIP-MTD-CLAHPAID    PIC S9(5)V99  COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-MIP-CLPDTH-DATE          PIC 9(8)      COMP-3.
           03  LN-MIP-TOT-CLPAID      PIC S9(7)V99  COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-MIP-AHPDTH-DATE          PIC 9(8)      COMP-3.
           03  LN-MIP-TOT-AHPAID      PIC S9(7)V99  COMP-3.
           03  LN-IL-GROSS-INCOME     PIC S9(7)V99  COMP-3.
           03  LN-IL-GROSS-INCOME2    PIC S9(7)V99  COMP-3.
           03  LN-RESCISSION          PIC X(1).
           03  LN-INTERIM-TRW         PIC X(1).
      *----------------------------------------------------
      *    SALES FINANCE PURCHASE INFORMATION
      *----------------------------------------------------
           03  LN-DLNO             PIC 9(4)V99   COMP-3.
           03  LN-DLPOOL           PIC 9(5)      COMP-3.
           03  LN-DLRECOURSE       PIC X.
           03  LN-DLPARTRCAMT      PIC 9(7)V99   COMP-3.
           03  LN-ORIGPURAMT       PIC 9(7)V99   COMP-3.
           03  LN-PURBAL           PIC S9(7)V99  COMP-3.
           03  LN-DLRVPRIN         OCCURS 3
                                   PIC 9(7)V99   COMP-3.
           03  LN-DLRVCHGS         OCCURS 3
                                   PIC 9(7)V99   COMP-3.
           03  LN-DLPARVHELD       PIC 9(7)V99   COMP-3.
           03  LN-DLPARVPAID       PIC 9(7)V99   COMP-3.
           03  LN-DLDISC           PIC 9(7)V99   COMP-3.
      *---------------------------------------------------------------------
      *    CURRENT LOAN INFORMATION
      *---------------------------------------------------------------------
      $XFD DATE=YYYYMMDD
           03  LN-LCPDTH-DATE           PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-INTPDTH-DATE          PIC 9(8)      COMP-3.
           03  LN-CURBAL           PIC S9(7)V99  COMP-3.
           03  LN-INTBAL           PIC S9(7)V99  COMP-3.
           03  LN-LCBAL            PIC S9(7)V99  COMP-3.
           03  LN-OTHBAL           PIC S9(7)V99  COMP-3.
           03  LN-OT2BAL           PIC S9(7)V99  COMP-3.
      * LN-LCPAID CAN BE A DATE IN TRIPLE UCC STATES, MMDDYY
      * COULD NOT IDENTIFY HOW TO SPLIT THIS OUT VIA CONVERSION SO LEAVING
      * IT LIKE THIS, MUST STAY AS MMDDYY, USED AS DATE IN DEFI01.CBL
      * LCAP-F LCAP-H LCAP-L LCAP-M LCAP-P LCAP-S
           03  LN-LCPAID           PIC S9(7)V99  COMP-3.
           03  LN-LCPARTIALS       PIC S9(7)V99  COMP-3.
      * STATEMENT COUNTER CONTAINS SEQNO OF LAST PRINTED PAYMENT RECORD.
           03  LN-STMT-COUNTER     PIC 9(5)      COMP-3.
           03  LN-PRIOR-STMT-COUNTER PIC 9(5)     COMP-3.
           03  LN-FROZEN-REASON    PIC X(2).
           03  LN-COUPON-STATEMENT PIC X.
           03  LN-MONTHEND-BAL     PIC 9(7)V99   COMP-3.
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
      *-------------------------------------------------------
           03  LN-ANTICTERM        PIC 9(3)      COMP-3.
           03  LN-ANTICTBL         OCCURS 10 TIMES.
               05  LN-ANTICERN     PIC S9(7)V99  COMP-3.
               05  LN-ANTICADJ     PIC S9(7)V99  COMP-3.
               05  LN-UFERN        PIC S9(7)V99  COMP-3.
               05  LN-ACCERN       PIC S9(7)V99  COMP-3.
               05  LN-EARN-ORDER   PIC 99.
           03  LN-LMO-PRIOR-INTEREST PIC S9(7)V99  COMP-3.
           03  LN-PRIOR-UNEARNED   PIC S9(7)V99  COMP-3.
           03  LN-ERNDELQ-STOP     PIC X.
           03  LN-NONACRERN        PIC S9(7)V99  COMP-3.
           03  LN-PRIOR-NONACRERN  PIC S9(7)V99  COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-ERN-IB-PDTH-DATE      PIC 9(8)      COMP-3.
           03  LN-ERN-IB-PRINBAL   PIC S9(7)V99  COMP-3.
           03  LN-ERN-IB-INTOWE    PIC S9(7)V99  COMP-3.
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
           03  LN-TOTOTH           PIC S9(7)V99  COMP-3.
      * TOTAL INTEREST CHARGES COLLECTED FROM LOAN INCEPTION:
           03  LN-TOTINT           PIC S9(7)V99  COMP-3.
      * TOTAL LATE CHARGES COLLECTED FROM LOAN INCEPTION:
           03  LN-TOTLCHG          PIC S9(7)V99  COMP-3.
      * TOTAL DEFERMENT CHARGES COLLECTED FROM LOAN INCEPTION:
           03  LN-TOTDEF           PIC S9(7)V99  COMP-3.
      * TOTAL INCOME DURING SERVICE ACCOUNT STATUS:
           03  LN-TOTSERACCT       PIC S9(7)V99  COMP-3.
           03  LN-LCWAIVERS        PIC S9(5)V99  COMP-3.
           03  LN-INTWAIVERS       PIC S9(5)V99  COMP-3.
           03  LN-TOTPAYMNTD       PIC S9(7)V99  COMP-3.
           03  LN-TOTEXCPAYMNTD    PIC S9(7)V99  COMP-3.
           03  LN-TOTNODEF         PIC S99       COMP-3.
           03  LN-YTDNODEF         PIC S99       COMP-3.
           03  LN-TOTNODEFACT      PIC S99      COMP-3.
           03  LN-YTDNODEFACT      PIC S99      COMP-3.
      * TOTAL ALLOWABLE (DEFERMENTS) DELINQUENCY,
      * THESE DEFERMENTS ARE NOT INCLUDED IN TOTNODEF AND YTDNODEF:
           03  LN-TOTNOALDEL       PIC S99       COMP-3.
           03  LN-YTDNOALDEL       PIC S99       COMP-3.
           03  LN-YTDINTPD         PIC S9(7)V99  COMP-3.
           03  LN-LYRINTPD         PIC S9(7)V99  COMP-3.
           03  LN-TOTINTPD         PIC S9(7)V99  COMP-3.
           03  LN-RECASTINT        PIC 9(7)V99   COMP-3.
           03  LN-RECASTPRIN       PIC 9(7)V99   COMP-3.
      *----------------------------------------------------
      *    REBATE INFORMATION
      *----------------------------------------------------
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
               05  LN-O6-REBATE    PIC X.
               05  LN-O7-REBATE    PIC X.
      *-REBATES FOR EXPANDED UNEARNED TABLE
               05  LN-UN-O7-REB    PIC X.
               05  LN-UN-O8-REB    PIC X.
               05  LN-UN-O9-REB    PIC X.
               05  LN-UN-O10-REB   PIC X.
           03  LN-REBATE-TABX     REDEFINES LN-REBATE-TAB.
               05  LN-REBATE       PIC X OCCURS 19.

           03  LN-PLREASON         PIC XX.
      *----------------------------------------------------
      *    COLLECTION INFORMATION
      *----------------------------------------------------
           03  LN-COLLECTOR        PIC X(3).
           03  LN-ACTIONCD         PIC X(4).
           03  LN-ACTIONCD2        PIC X(4).
           03  LN-ATCODE           PIC X(3).
           03  LN-RECODE           PIC X(3).
           03  LN-ESCROW-FG        PIC X.
           03  LN-PROMDAYS-PASTDUE PIC S9(3)     COMP-3.
           03  LN-PROMID           PIC X(3).
      $XFD DATE=YYYYMMDD
           03  LN-PROMDATE         PIC 9(8)      COMP-3.
           03  LN-PROMAMT          PIC 9(7)V99   COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-PROMDATE2        PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-DATE-PAID-LAST   PIC 9(8)      COMP-3.
           03  LN-AMT-PAID-LAST    PIC 9(7)V99   COMP-3.
           03  LN-LASTCFORMNO      PIC 9(5)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-LASTCFORMDATE    PIC 9(8)      COMP-3.
      * NO. OF TIMES EXTRACTED:
           03  LN-DELCNT           PIC 99.
      * NO. OF DELINQUENT FORMS SENT:
           03  LN-DELCNT2          PIC 99.
           03  LN-CREDITLIM        PIC 9(7)      COMP-3.
           03  LN-CRSCORE          PIC S9(3)     COMP-3.
      *----------------------------------------------------
      *    SOLICIT INFORMATION
      *----------------------------------------------------
           03  LN-LASTSFORMNO       PIC 9(5)      COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-LASTSFORMDATE     PIC 9(8)      COMP-3.
      * NO. OF SOLICITATION FORMS SENT:
           03  LN-SOLCNT           PIC 99.
           03  LN-MESSAGE          PIC X(30).
      *    THIS FIELD IS SET MANUALLY IN THE INQUIRIES, ONLY IF BRANCH
      *    FLAG ALLOWS.  THE EXTRACTION UPDATE WILL COMPARE THE EXTRACTION
      *    DATE(TODAY), AFTER INCREMENTED BY # OF DAYS, TO THE SECOND
      *    PROMISE DATE, IF FIRST IS LESS, THEY STAY THE SAME, IF FIRST
      *    IS AFTER SECOND, MOVE SECOND TO FIRST, AND 0 SECOND.
      *----------------------------------------------------
      *    PRIOR LOAN INFORMATION
      *----------------------------------------------------
           03  LN-OLDACCTNO        PIC 9(8)      COMP-3.
           03  LN-PRLNNO           PIC 9(8)      COMP-3.
           03  LN-PRBAL            PIC S9(7)V99  COMP-3.
           03  LN-PRINTDUE         PIC S9(7)V99  COMP-3.
           03  LN-PRFINCHGRB       PIC S9(7)V99  COMP-3.
           03  LN-PRINSRB          PIC S9(7)V99  COMP-3.
           03  LN-PRPOCD           PIC XX.
      $XFD DATE=YYYYMMDD
           03  LN-PRPODATE         PIC 9(8)      COMP-3.
           03  LN-PRDELINQAMT      PIC S9(7)V99  COMP-3.
           03  LN-PRLNAMT          PIC 9(7)V99    COMP-3.
           03  LN-PRPDUE-DAYS      PIC S9(5)      COMP-3.
      *  JTG 040501 ADDED LOGIC TO CAUSE THE UNEARNED LOAN COST TO ROLL TO
      *             THE NEW LOAN AFTER 'RENEWAL'. NOTE: THIS COST IS REFLECTIVE
      *             OF THE DISCOUNT WHEN PURCHASING LOAN PAPER NOT CARRIED AS
      *             SALES PAPER WITH DEALER DISCOUNT.
      *             ADDED SP-LNCOST-FRMLA = 'R'                    REGENCY #2287
      *       NOTE: LN-PRDLDISC-UNEARN WILL HOLD THE COST (DISCOUNT)
      *             FOR DIRECT LOANS, EVEN THOUGH IT IS NOT TIED TO A DEALER.
           03  LN-PRDLDISC-UNEARN  PIC S9(7)V99  COMP-3.
           03  LN-PRCONPROFILE     PIC 9(12)     COMP-3.
      *----------------------------------------------------
      *    LOAN ADDON INSURANCE CONSOLIDATED INFORMATION
      * NOTE:
      *      LN-SUM-MAINTFEE = TOTAL CONTRACT CHARGE
      *----------------------------------------------------
           03  LN-SUM-ORIGPURAMT   PIC 9(7)V99   COMP-3.
           03  LN-SUM-LNAMT        PIC 9(7)V99   COMP-3.
           03  LN-SUM-FINCHG       PIC 9(7)V99   COMP-3.
           03  LN-SUM-INTCHG       PIC 9(7)V99   COMP-3.
           03  LN-SUM-ORGTERM      PIC 9(3)      COMP-3.
           03  LN-SUM-REGPYAMT     PIC 9(7)V99   COMP-3.
           03  LN-SUM-EXTCHG       PIC 9(5)V99   COMP-3.
           03  LN-SUM-SERCHG       PIC 9(5)V99   COMP-3.
           03  LN-SUM-MAINTFEE     PIC 9(7)V99   COMP-3.
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
           03  LN-O3INS-DLPROC      PIC 9(7)V99    COMP-3.
           03  LN-INSPREM-DLADJ     PIC S9(5)V99   COMP-3.
           03  LN-PLAMT             PIC 9(7)V99    COMP-3.
      * PLAMT = CURBAL AT TIME OF P&L:
      * MONEYS APPLIED TO PRINCIPAL:
           03  LN-MTD-PRIN          PIC S9(7)V99   COMP-3.
      * MONEYS APPLIED TO LATE CHARGES:
           03  LN-MTD-LC            PIC S9(5)V99   COMP-3.
      * MONEYS APPLIED TO INTEREST:
           03  LN-MTD-INT           PIC S9(5)V99   COMP-3.
      * MONEYS APPLIED TO OTH & OT2:
           03  LN-MTD-OT            PIC S9(5)V99   COMP-3.
      * MONEYS APPLIED RE: EXCEPTION PAYMENTS:
           03  LN-MTD-EX            PIC S9(7)V99   COMP-3.
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
      * LN-APP-NUMBER IS OLD A15; CAN BE REUSED FOR FUTURE USE!!!
      * DID NOT REMOVED DUE TO SQL DATABASE HASSLE!!!
           03  LN-APP-NUMBER        PIC 9(09)     COMP-3.
      * LN-GIFTNET IS USED FOR WORLD DUAL LOAN OVERRIDE
           03  LN-GIFTNET           PIC X.
           03  LN-GIFTNET-OUR-PO    PIC 9(7)      COMP-3.
           03  LN-PROC-DISBURSED    PIC X.
      * THIS FILLER IS STILL PD-NOODDPY AND USED !!
           03  LN-NOODDPY           PIC X.
      * SIXTY MONTH PROFILES:
           03  LN-CONPROFILE        OCCURS 5
                                    PIC 9(12)     COMP-3.
           03  LN-RECPROFILE        OCCURS 5
                                    PIC 9(12)     COMP-3.
           03  LN-TRW-STATUS        PIC XX.

           03  LN-ORIGIN            PIC X(4).
           03  LN-PO-TRCD           PIC XX.
           03  LN-NEXIS-CRSCORE      PIC 9(3)      COMP-3.
           03  LN-TRW-B-CONSUMER-IND PIC X(2).
           03  LN-PARVHELD-PAID-FG   PIC X.
      *-----------------------------------------------------------------
           03  LN-ACCT-FROZEN        PIC X.
           03  LN-TRW-SPECIAL-CODE   PIC X(2).
      *-----------------------------------------------------------------
      *    LN-CREATION-STATUS:  USED TO KNOW IF CREATED BY A OUTSIDE
      *                         VENDOR (EXTERNAL) OR BY THE LOAN MANAGER
      *                         (INTERNAL).
      *-----------------------------------------------------------------
           03  LN-CREATION-STATUS    PIC X(1).
               88  LN-CREATION-EXTERNAL  VALUE "L".
               88  LN-CREATION-INTERNAL  VALUE "I" " ".
               88  LN-CREATION-LI-VENDOR VALUE "L".
           03  LN-TOTNOLCHG          PIC S9(3)    COMP-3.
           03  LN-LBOX               PIC X(1).
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
           03  LN-DLCOMM-O3-CMSI               PIC 9(5)V99 COMP-3.
           03  LN-DLCOMM-PP-CMSI               PIC 9(5)V99 COMP-3.
           03  LN-TRW-J-CONSUMER-IND           PIC X(2).
           03  LN-ACH                          PIC X.
           03  LN-1099-C-FG                    PIC X.
           03  LN-TRW-COMPLIANCE-CODE          PIC XX.
               88  LN-TRW-COMPLIANCE-VALID     VALUE "XA" THRU "XH" "XJ"
                                                     "XR".
           03  LN-UNWIND-REASON-CODE           PIC X(2).
      * NEW FIELDS 
           03  LN-PBI-PBD-FG                   PIC X.
           03  LN-OLD-ACCT-FORMAT-FG           PIC X.
           03  LN-APPROVED-BY-USER             PIC X(10).
           03  LN-APPROVED-BY-CODE             PIC X(3).
      $XFD DATE=YYYYMMDD
           03  LN-APPROVED-BY-DATE             PIC 9(8)     COMP-3.
           03  LN-APPROVED-BY-TIME             PIC 9(6)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  LN-LTOUCH-DATE                  PIC 9(8)     COMP-3.
           03  LN-LTOUCH-TIME                  PIC 9(6)     COMP-3.
      * NEXUS APP ID
           03  LN-LOS-APP-ID                   PIC 9(18)    COMP-3.

           03  FILLER                          PIC X(690).
