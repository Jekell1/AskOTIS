      * COPYMEMBER: LIBLP/LP01EA
      ************************************************************
      *            ( EXTRACTION/ACTION FILE )   BYTES = 222
      *
      *  REV:
      *   062895 BAH ADDED DLNO1 AND DLNO2           # E021
      *                     CONTRACTUAL-DAY1 AND CONTRACTUAL-DAY2
      *   031396 KEC MADE EA-GROUP-ID APART OF THE EA1-KEY.
      *   032896 BLL ADD EA-DESC; EXPAND UNUSED; ADD CQ-POOL-FG
      *   041696 BLL ADD EA-REPO, EA-PL, EA-BANKRUPT - NEW PARAMETERS
      *              NOW ONLY USED IN EQMAIN, CQRUN FOR COL. QUEUE
      *   062496 KEC ADD EA-PL-IN-ACTIVE-BOTH - NEW PARAMETER THAT IS
      *              ATTACHED TO EA-PL.
      *   030497 BLV ADD EA-CONFIDENTIAL-FG, EA-1ST-PMT-MISSES
      *   061997  CS ADD EA-LEGAL, EA-DEBTAID, EA-JUDGEMENT
      *   063097 BLV ADD EA-BROKEN-PROM-FG, EA-PROM-BREAK1,
      *                                     EA-PROM-BREAK2
      *   112097 JTG EXPANED BY 16 BYTES
      *   041798  CS ADDED EA-ORIGIN
      * KEC 1999.0510 <<<<<<<< 'A60-TO-A15' CONVERSION UPDATED >>>>>>>>
      * KEC 2000.0427 {PR#01138} REGIONAL ACCEPTANCE; KEVIN MULLEN
      *                ADDED EA-PL-BEG-DATE AND EA-PL-END-DATE
      *   001017 BLV ADDED EA-RPOTS, EA-MISPAYS, RENAMED EA-POTS TO
      *              EA-CPOTS TO CLARIFY; WORLD PR# 244
      *   010503  CS ADDED EA-ALLOTMENTS ACE PR#1472
      *   011003 BLV ADDED EA-LNDATE-MONTHS1, EA-LNDATE-MONTHS2,
      *              EA-MATURE-MONTHS1, EA-MATURE-MONTHS2,
      *              REGWAN PR# 1587
      *   011011 BLV CHANGED MONTHS SINCE LOANDATE, MONTHS TIL MATURITY
      *              TO DAYS
      *   021029 BLV ADDED DAY OF MONTH DUE RANGE, EA-DAY-OF-MONTH-DUE1
      *   060228 BLF ADDED EA-INCL-COLLECTOR, LENDMARK PR# 2719
      *   130422 BLM ADDED EA-BORR-STATE, SECURITY PR# 4248
      *********************************************************************
       78  EA-SIZE                                            VALUE 222.
       01  EA-REC.
           03  EA1-KEY.
               05  EA-BRANCH          PIC 9(4).
               05  EA-CLASS           PIC 99.
               05  EA-GROUP-ID        PIC X(6).
               05  EA-SEQNO           PIC 99.
           03  EA-DESC                PIC X(60).
           03  EA-PASTDUE-DAY1        PIC 9(4)       COMP-3.
           03  EA-PASTDUE-DAY2        PIC 9(4)       COMP-3.
           03  EA-PASTDUE-AMT1        PIC 9(4)V99    COMP-3.
           03  EA-PASTDUE-AMT2        PIC 9(4)V99    COMP-3.
           03  EA-ACCT-BAL1           PIC 9(6)V99    COMP-3.
           03  EA-ACCT-BAL2           PIC 9(6)V99    COMP-3.
           03  EA-MATURED             PIC X.
           03  EA-PROMISED            PIC X.
           03  EA-ACTION              PIC X(4).
           03  EA-LSTFM-DATE1         PIC 9(6)       COMP-3.
           03  EA-LSTFM-DATE2         PIC 9(6)       COMP-3.
           03  EA-LSTFM-NO1           PIC 9(4)       COMP-3.
           03  EA-LSTFM-NO2           PIC 9(4)       COMP-3.
           03  EA-PASTPROM-DAY1       PIC S9(5)      COMP-3.
           03  EA-PASTPROM-DAY2       PIC S9(5)      COMP-3.
           03  EA-RECENCY-DAY1        PIC 9(4)       COMP-3.
           03  EA-RECENCY-DAY2        PIC 9(4)       COMP-3.
           03  EA-ALPNAME1            PIC X(3).
           03  EA-ALPNAME2            PIC X(3).
           03  EA-RECENCYDATE         PIC X.
           03  EA-DLNO1               PIC 9(4)V99    COMP-3.
           03  EA-DLNO2               PIC 9(4)V99    COMP-3.
           03  EA-CONTRACTUAL-DAY1    PIC S9(5)      COMP-3.
           03  EA-CONTRACTUAL-DAY2    PIC S9(5)      COMP-3.
 
      * ACTION CODES:
           03  EA-SETFM-NO            PIC 9(4)       COMP-3.
           03  EA-SETACT-CD           PIC X(4).
           03  EA-INC-CNT             PIC X.
           03  EA-INC-PROMISED        PIC 9(2).
           03  EA-MORE-FORMS          OCCURS 3 TIMES.
               05  EA-SETFM           PIC 9(3)       COMP-3.
           03  EA-SET-LN-COL          PIC X.
           03  EA-LN-COL-ID           PIC XXX.
           03  EA-CQ-FG               PIC X.
           03  EA-COL-POOL            PIC X(3).
           03  EA-CPOTS               PIC X.
           03  EA-REPO                PIC X.
           03  EA-PL                  PIC X.
           03  EA-BANKRUPT            PIC X.
           03  EA-PL-IN-ACTIVE-BOTH   PIC X.
           03  EA-CONFIDENTIAL-FG     PIC X.
           03  EA-1ST-PMT-MISSES      PIC X.
           03  EA-LEGAL               PIC X.
           03  EA-DEBTAID             PIC X.
           03  EA-JUDGEMENT           PIC X.
           03  EA-BROKEN-PROM-FG      PIC X.
           03  EA-PROM-BREAK1         PIC S9(5)      COMP-3.
           03  EA-PROM-BREAK2         PIC S9(5)      COMP-3.
           03  EA-ORIGIN              PIC X(4).
           03  EA-PL-BEG-DATE         PIC 9(06)      COMP-3.
           03  EA-PL-END-DATE         PIC 9(06)      COMP-3.
           03  EA-RPOTS               PIC X.
           03  EA-MISPAYS             PIC X.
           03  EA-ALLOTMENTS          PIC X.
           03  EA-LNDATE-DAYS1        PIC 9(4)       COMP-6.
           03  EA-LNDATE-DAYS2        PIC 9(4)       COMP-6.
           03  EA-MATURE-DAYS1        PIC 9(4)       COMP-6.
           03  EA-MATURE-DAYS2        PIC 9(4)       COMP-6.
           03  EA-DAY-OF-MO-DUE1      PIC 9(2)       COMP-6.
           03  EA-DAY-OF-MO-DUE2      PIC 9(2)       COMP-6.
           03  EA-INCL-COLLECTOR      PIC X(3).
           03  EA-BORR-STATE          PIC X(2).
