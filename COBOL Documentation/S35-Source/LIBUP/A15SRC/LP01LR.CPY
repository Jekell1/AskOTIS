      * COPYMEMBER: LIBLP/LP01LR
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LP01LR                         RSZ=01141
      *
      * DESCRIPTION   :  LOAN T(R)IAD FILE
      *
      * CUSTOMER NAMES:  REGIONAL ACCEPTANCE
      *
      *=================================================================
      *                                                                *
      *                   T R I A D  I N T E R F A C E                 *
      *                                                                *
      *    COPYMEMBER:  CUSTMSTR                                       *
      *                                                                *
      *    DESCRIPTION: DATA ELEMENTS PASSED FROM THE CLIENT TO        *
      *                 TRIAD ON BILLING, POSTING, AND MARKETING       *
      *                 COMM. CALLS                                    *
      *                                                                *
      *    NOTE: THE FOLLOWING DECISION AREAS ARE SUPPORTED BY         *
      *          THIS INTERFACE:                                       *
      *                                                                *
      *          CREDIT LINE, DELINQUENT COLLECTIONS, OVERLIMIT        *
      *          COLLECTIONS, MARKETING COMMUNICATIONS, PERFORMANCE-   *
      *          BASED PRICING, AND REISSUE                            *
      *                                                                *
      *=================================================================
      * REV:
      * KEC 2003.0318 {PR#00026} REGIONAL ACCEPTANCE <A15> [KEVIN M]
      *               ***NEW***                               RSZ=01141
      *
      * KEC 2003.0918 {PR#00039} REGIONAL ACCEPTANCE <A15> [KEVIN M]
      *               ADDED THE FOLLOWING:
      *    03  LR-CL-FINANCIAL-DATA-02  OCCURS LR-CL-FINANCIAL-DATA-MAX.
      *        05  LR-CL-BILL-BAL                 PIC S9(09)V99  COMP-3.
      *
      * CS  040706  ADD LR-CL-DIALER-COUNTER         PIC 9(3)
      *
      * KEC 2005.1116 {PR#00117} REGIONAL ACCEPTANCE <A15> [KEVIN M]
      *               ADDED THE FIELDS:
      *
      *    03  LR-CL-BPTP-LT-COUNTER              PIC 9(03)      COMP-6.
      *    03  LR-CL-BPTP-ST-COUNTER              PIC 9(02)      COMP-6.
      *    03  LR-CL-BPTP-SCEN-ID                 PIC S9(03)     COMP-3.
      *
      * KEC 2006.0120 {PR#00121} REGIONAL ACCEPTANCE <A15> [KEVIN M]
      *               ADDED THE FIELDS:
      *
      *                 03  LR-CL-RTC-DATE-1   PIC 9(06) COMP-6.
      *                 03  LR-CL-RTC-DATE-2   PIC 9(06) COMP-6.
      * BAH 2006.0615 REGACC PR# 135 FOR ENVELOPE ORDERING EXTRACTION
      *               ADDED;
      *               03  LR-PX10-ENVELOPE-ONLY              PIC X.
      *               03  LR-PX10-ENVELOPE-MAKER-COMAKER     PIC X.
      * BAH 2007.0131 REGACC PR# 162 NEW DEF POLICY FOR 6 MONTHS PAID THRU
      *               ELAPSED BETWEEN DEFERMENTS, HAD TO ADD;
      *               LR-PX10-LAST-DEF-PDTH        PIC 9(6)    COMP-6.
      *               LR-PX10-PREV-DEF-PDTH        PIC 9(6)    COMP-6.
      *  CS   070227  ADD LR-CL-LAST-AL-DATE,LR-CL-LAST-AL-ASSESSMENT.. DATE
      *               AND AMOUNT OF LAST LATE CHARGE ASSESSMENT PR#152-R
      *                                                      REGIONAL
      *  CS   080407  ADD LR-PX10-LAST-DEF-TRCD PR#202 REGACC
      *                   LR-PX10-PREV-DEF-TRCD
      * BLM 090928    ADD LR-CL-TOTLCHG-CLEARED, TO STORE AMT OF TOTAL
      *               LATE CHARGES FROM LN-TOTLCHG, ZEROED OUT BY
      *               RESCHEDULE (LP/RESCHD), REGACC PR# 254
      *  CS 121008    ADD LR-CL-NONACCRUAL-STATUS,LR-CL-NONACCRUAL-START-DATE,
      *               LR-CL-MONTHS-PAY-NAPP-COUNTER   REGACC PR#323
      *  CS 130424    ADD LR-PX10-LAST-DF-REFCD, LR-PX10-PREV-DF-REFCD,
      *               LR-PX10-TDR1, LR-PX10-TDR2,LR-PX10-TDR1-DATE,
      *               LR-PX10-TDR2-DATE REGACC PR#338
      *  CS 131202    ADD LR-CL-NONACCRUAL-STOP-DATE  PR#347 REGACC
      *
      *=================================================================
      * KEC 141215 {PR#356} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *            REVISED FOR TRIAD 9 (TRIAD 7 IS OLD/ORIG).
      *
      *            NEW FIELDS WERE ADDED:
      *
      *            LR-RO20-LN-CAT
      *            LR-RO20-COLL-DATE-BILL-EQV
      *            LR-RO20-DATE-FIRST-COLLS-DA
      *            LR-RO20-COLL-BALANCE-INITIAL
      *            LR-RO20-COLL-AMT-ARREARS
      *            LR-RO20-COLL-BALANCE-ACTUAL
      *            LR-RO20-COLL-PTP
      *            LR-RO20-BAR-FACTOR
      *
      *            EXISTING FIELDS WERE RENAMED USING REDEFINES:
      *
      *            LR-RO20-SCRD-ID        REDEFINES LR-CL-SCRD-ID
      *            LR-RO20-RAW-SCORE      REDEFINES LR-CL-RAW-SCORE
      *            LR-RO20-ALIGNED-SCORE  REDEFINES LR-CL-ALIGNED-SCORE
      *            LR-RO20-SCRD-TYPE      REDEFINES LR-CL-SCRD-TYPE 
      *            LR-RO20-COLL-SCEN-ID   REDEFINES LR-CL-DELQ-SCEN-ID
      *
      *            LR-RO20-COLL-ACTION-CTR          REDEFINES
      *            LR-CL-DELQ-ACTION-CTR
      *
      *            LR-RO20-RET-CODE       REDEFINES LR-PR10-RET-CODE
      *            LR-RO20-SPID           REDEFINES LR-PR10-SPID
      *
      *            LR-RO20-MN-ADVERSE-ACT-CODE      REDEFINES
      *            LR-PR10-MN-ADVERSE-ACT-CODE
      *
      *            LR-RO20-MN-ADVERSE-ACT-REASON    REDEFINES
      *            LR-PR10-MN-ADVERSE-ACT-REASON
      *
      *            LR-RO20-COLL-TENANT-CNTL-IND     REDEFINES
      *            LR-PR10-DELQ-CLIENT-CNTL-IND
      *
      *            LR-RO20-COLL-STGY-ID             REDEFINES
      *            LR-PR10-DELQ-STGY-ID
      *
      *            LR-RO20-COLL-FIXED-ACTION-1      REDEFINES
      *            LR-PR10-DELQ-STMT-MSG-ID
      *
      *            LR-RO20-COLL-TIMED-ACTION-1      REDEFINES
      *            LR-PR10-DELQ-COLL-IND-X
      *
      *            LR-RO20-COLL-TIMED-ACTION-2      REDEFINES
      *            LR-PR10-DELQ-BLK-CODE
      *
      *            LR-RO20-COLL-TIMED-ACTION-3      REDEFINES
      *            LR-PR10-DELQ-LTR-ID
      *
      *            SOME FIELDS GREW IN SIZE.  AT THIS TIME KEVIN & I
      *            DECIDED THE INCREASE IN SIZE IS ***NOT*** NECESSARY:
      *
      *            T7 = LR-PR10-SPID                  PIC 9(02)
      *            T9 =    RO20-SPID                  PIC 9(03)
      *
      *            T7 = LR-CL-DELQ-SCEN-ID            PIC S9(03)
      *            T9 =  RO20-COLL-SCEN-ID            PIC 9(04)
      *
      *            T7 = LR-PR10-DELQ-BLK-CODE         PIC X(05)
      *            T9 =    RO20-COLL-TIMED-ACTION-2   PIC X(04)
      *
      *            T7 = LR-CL-SCRD-TYPE               PIC  9(01)
      *            T9 =  RO20-SCRD-TYPE               PIC  9(02)
      *
      *            T7 = LR-CL-RAW-SCORE               PIC S9(04)
      *            T9 =  RO20-RAW-SCORE               PIC S9(07)
      *
      *            T7 = LR-CL-ALIGNED-SCORE           PIC S9(04)
      *            T9 =  RO20-ALIGNED-SCORE           PIC S9(07)
      *
      ******************************************************************
       78  LR-SIZE                                           VALUE 1141.
      *-----------------------------------------------------------------
       01  LR-REC.

           03  LR1-KEY.
               05  LR-ACCTNO                      PIC 9(08).
               05  FILLER               REDEFINES LR-ACCTNO.
                   07  LR-NUMBER                  PIC 9(06).
                   07  LR-SEQNO                   PIC 9(02).

      ******************************************************************
      *
      *    L R - C L - F I E L D S
      *
      ******************************************************************
      *
      *    E X T R A C T I O N - F I E L D S
      *
      *-----------------------------------------------------------------
      *    FIELDS THAT ARE WITHIN THE EXTRACTION ITSELF.
      *=================================================================
       78  LR-CL-FINANCIAL-DATA-MAX               VALUE 13.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  LR-CL-FINANCIAL-DATA     OCCURS LR-CL-FINANCIAL-DATA-MAX.
               05  LR-CL-AMT-DUE                  PIC S9(09)V99  COMP-3.
               05  LR-CL-AMT-PAY                  PIC S9(09)V99  COMP-3.
      
      *-----------------------------------------------------------------
      *    NOTE: THE FOLLOWING DATE FIELDS ARE IN MMDDYY FORMAT.
      *          WHEN USED IN THE EXTRACTION FILE (LRE) THEY WILL CHANGE
      *-----------------------------------------------------------------
           03  LR-CL-DTE-CB-SCORE                 PIC 9(06)      COMP-6.
           03  LR-CL-DTE-LAST-COMM                PIC 9(06)      COMP-6.
      * CL-DTE-LAST-EXT = DF, D2-D9, AD, A2-A9 POSTING.
      *                   REVERSAL OF THESE SETS BACK TO LAST UNREVERSED
           03  LR-CL-DTE-LAST-EXT                 PIC 9(06)      COMP-6.
      * CL-DTE-PREV-EXT HOLDS PREVIOUS DF, D2-D9, AD, A2-D9 POSTING
      *                 NEEDED FOR UPDATE OF REVERSAL TO MOVE THIS ONE
           03  LR-CL-DTE-PREV-EXT                 PIC 9(06)      COMP-6.
           03  LR-CL-DTE-LAST-NSF                 PIC 9(06)      COMP-6.
           03  LR-CL-DTE-LAST-TERMS-CHG           PIC 9(06)      COMP-6.

      *-----------------------------------------------------------------
       78  LR-CL-SCORE-HISTORY-FLDS-MAX           VALUE 13.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  LR-CL-SCORE-HISTORY-ARRAY.
               05  LR-CL-SCORE-HISTORY-FLDS       OCCURS
                   LR-CL-SCORE-HISTORY-FLDS-MAX.

                   07  LR-CL-SCRD-ID              PIC S9(04)     COMP-3.
                   07  LR-RO20-SCRD-ID            REDEFINES
                       LR-CL-SCRD-ID              PIC S9(04)     COMP-3.

                   07  LR-CL-RAW-SCORE            PIC S9(04)     COMP-3.
                   07  LR-RO20-RAW-SCORE          REDEFINES
                       LR-CL-RAW-SCORE            PIC S9(04)     COMP-3.

                   07  LR-CL-ALIGNED-SCORE        PIC S9(04)     COMP-3.
                   07  LR-RO20-ALIGNED-SCORE      REDEFINES
                       LR-CL-ALIGNED-SCORE        PIC S9(04)     COMP-3.

                   07  LR-CL-SCRD-TYPE            PIC  9(01).
                       88  LR-CL-BEHAVIOR-SCRD        VALUE 0.
                       88  LR-CL-PAYPROJ-SCRD         VALUE 1.
                       88  LR-CL-ATTRITION-SCRD       VALUE 2.
                       88  LR-CL-BANKRUPTCY-SCRD      VALUE 3.
                       88  LR-CL-REVENUE-SCRD         VALUE 4.
                   07  LR-RO20-SCRD-TYPE          REDEFINES
                       LR-CL-SCRD-TYPE            PIC  9(01).

                   07  LR-CL-CB-SCORE             PIC S9(04)     COMP-3.
                   07  LR-CL-CB-SCORE-TYPE        PIC  X(01).

      *-----------------------------------------------------------------
       78  LR-CL-CYC-DELQ-MAX                     VALUE 24.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  LR-CL-CYC-DELQ-HIST.
               05  LR-CL-CYC-DELQ          OCCURS LR-CL-CYC-DELQ-MAX
                                                  PIC S9(02)     COMP-3.

      *-----------------------------------------------------------------
           03  LR-CL-DELQ-SCEN-ID                 PIC S9(03)     COMP-3.
           03  LR-RO20-COLL-SCEN-ID               REDEFINES
               LR-CL-DELQ-SCEN-ID                 PIC S9(03)     COMP-3.

      *-----------------------------------------------------------------
       78  LR-CL-NUM-NSF-MAX                      VALUE 6.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  LR-CL-NUM-NSF-ARRAY.
               05  LR-CL-NUM-NSF           OCCURS LR-CL-NUM-NSF-MAX
                                                  PIC 9(02)      COMP-6.
      
      *-----------------------------------------------------------------
       78  LR-CL-AMT-FEES-MAX                     VALUE 4.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  LR-CL-AMT-FEES              OCCURS LR-CL-AMT-FEES-MAX
                                                  PIC S9(05)V99  COMP-3.

      *-----------------------------------------------------------------
           03  LR-CL-AMT-INT                      PIC S9(05)V99  COMP-3.
           03  LR-CL-NUM-DELQ-LTRS                PIC S9(03)     COMP-3.

           03  LR-CL-DELQ-ACTION-CTR              PIC  9(01).
           03  LR-RO20-COLL-ACTION-CTR            REDEFINES
               LR-CL-DELQ-ACTION-CTR              PIC  9(01).

           03  LR-CL-DELQ-CLIENT-CNTL-IND         PIC  9(01).
               88  LR-CL-DELQ-CLNT-CNTL-SPID-ASMT     VALUE 2.
               88  LR-CL-DELQ-CLNT-CNTL-TRMT-AREA     VALUE 1.
               88  LR-CL-DELQ-CLIENT-CNTL             VALUE 1 2.
               88  LR-CL-DELQ-TRIAD-CNTL              VALUE 0.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    LR-CL-USER-DYN-RECLASS-UP   = 'UP-TODAY'
      *    LR-CL-USER-DYN-RECLASS-DOWN = 'DOWN-TODAY'
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  LR-CL-USER-DELQ-DYN-RECLASS        PIC  9(01).
               88  LR-CL-NO-USER-DYN-RECLASS           VALUE 0.
               88  LR-CL-USER-DYN-RECLASS-UP           VALUE 1.
               88  LR-CL-USER-DYN-RECLASS-DOWN         VALUE 2.
      
           03  LR-CL-NUM-NSF-LF                   PIC S9(03)     COMP-3.
           03  LR-CL-MTHS-W-DELQ-LF               PIC S9(03)     COMP-3.
           03  LR-CL-AMT-AT-RISK-FACTOR           PIC S9V9(07)   COMP-3.

      *-----------------------------------------------------------------
      *78  LR-CL-FINANCIAL-DATA-MAX               VALUE 13.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  LR-CL-FINANCIAL-DATA-02  OCCURS LR-CL-FINANCIAL-DATA-MAX.
               05  LR-CL-BILL-BAL                 PIC S9(09)V99  COMP-3.
      
      *-----------------------------------------------------------------
      *CS  03  LR-CL-FILLER-01                    PIC X(22).
           03  LR-CL-DIALER-COUNTER               PIC S999 COMP-3.
           03  LR-CL-RTC-DATE-1                   PIC 9(06)      COMP-6.
           03  LR-CL-RTC-DATE-2                   PIC 9(06)      COMP-6.
           03  LR-CL-LAST-AL-DATE                 PIC 9(6)       COMP-6.
           03  LR-CL-LAST-AL-ASSESSMENT           PIC S9(7)V99   COMP-3.  
           03  LR-CL-TOTLCHG-CLEARED              PIC S9(6)V99   COMP-3.
           03  FILLER                             PIC X.   
      *=================================================================
      *
      *    N O N - E X T R A C T I O N - F I E L D S
      *
      *-----------------------------------------------------------------
      *    FIELDS NEEDED IN SETTING EXTRACTION FIELDS BUT ARE
      *    NOT WITHIN THE EXTRACTION ITSELF.
      *=================================================================
           03  LR-CL-PROMISE-KEPT-LAST-DATE       PIC 9(06)      COMP-6.
           03  LR-CL-SPECIAL-HANDLING-DATE        PIC 9(06)      COMP-6.
           03  LR-CL-SPECIAL-HANDLING-FG          PIC X(01).
           03  LR-CL-USER-DELQ-EXCL-DATE          PIC 9(06)      COMP-6.
           03  LR-CL-NUM-RPC                      PIC 9(04)      COMP-6.
           03  LR-CL-LAST-CYCLE-DATE              PIC 9(06)      COMP-6.

      *-----------------------------------------------------------------
      *    "BPTP" = BROKEN PROMISE TO PAY
      *    "LT"   = LONG  TERM (LIFETIME)
      *    "ST"   = SHORT TERM
      *-----------------------------------------------------------------
           03  LR-CL-BPTP-LT-COUNTER              PIC 9(03)      COMP-6.
           03  LR-CL-BPTP-ST-COUNTER              PIC 9(02)      COMP-6.
           03  LR-CL-BPTP-SCEN-ID                 PIC S9(03)     COMP-3.

      *-----------------------------------------------------------------
           03  LR-CL-NONACCRUAL-STATUS            PIC X.
           03  LR-CL-NONACCRUAL-START-DATE        PIC 9(6)     COMP-6.
           03  LR-CL-MONTHS-PAY-NAPP-COUNTER      PIC 9(2)     COMP-6.
           03  LR-CL-NONACCRUAL-STOP-DATE         PIC 9(6)     COMP-6.
           03  FILLER                             PIC X(82).
      *    03  LR-CL-FILLER-02                    PIC X(85).

      ******************************************************************
      *
      *    L R - P X 1 0 - F I E L D S
      *
      ******************************************************************
           03  LR-PX10-PRE-POST-BAL               PIC S9(9)V99   COMP-3.
           03  LR-PX10-PRE-POST-DELQ              PIC 9(02)      COMP-6.
           03  LR-PX10-PRE-POST-PTP-IND           PIC X(01).

           03  LR-PX10-NSF-TODAY-IND              PIC X(01).
               88  LR-PX10-NO-NSF-TODAY               VALUE '0'.
               88  LR-PX10-NSF-TODAY                  VALUE '1'.

           03  LR-PX10-DELQ-COLL-QUEUE            PIC X(04).

      *-----------------------------------------------------------------
      *   TAKE FROM  LR-PX10-FILLER FOR MISC FIELDS 
      *-----------------------------------------------------------------
           03  LR-PX10-ENVELOPE-ONLY              PIC X.
           03  LR-PX10-ENVELOPE-MAKER-COMAKER     PIC X.
      * REGACC PR# 162 NEW POLICY FOR 6 MONTHS PAID THRU ELAPSED BETWEEN
      * DEFERMENTS POSTED
           03  LR-PX10-LAST-DEF-PDTH              PIC 9(6)    COMP-6.
           03  LR-PX10-PREV-DEF-PDTH              PIC 9(6)    COMP-6.
           03  LR-PX10-LAST-DEF-TRCD              PIC X(2).
           03  LR-PX10-PREV-DEF-TRCD              PIC X(2).
           03  LR-PX10-LAST-DF-REFNO              PIC X(5).
           03  FILLER REDEFINES LR-PX10-LAST-DF-REFNO.
               05  LR-PX10-LAST-DF-REFNO-3        PIC X(3).
               05  FILLER                         PIC X(2).
           03  LR-PX10-PREV-DF-REFNO              PIC X(5).
           03  FILLER REDEFINES LR-PX10-PREV-DF-REFNO.
               05  LR-PX10-PREV-DF-REFNO-3        PIC X(3).
               05  FILLER                         PIC X(2).
           03  LR-PX10-TDR1                       PIC X.
           03  LR-PX10-TDR1-DATE                  PIC 9(6).
           03  LR-PX10-TDR2                       PIC X.
           03  LR-PX10-TDR2-DATE                  PIC 9(6).
           03  LR-PX10-FILLER                     PIC X(64).

      ******************************************************************
      *
      *    L R - P R 1 0 - F I E L D S
      *
      ******************************************************************
           03  LR-PR10-RET-CODE                   PIC 9(04)      COMP-6.
               88  LR-PR10-NO-ERROR                   VALUE ZEROES.
               88  LR-PR10-FATAL-ERROR                VALUE   1 THRU 30.
               88  LR-PR10-ACCT-ERROR                 VALUE  31 THRU 60.
               88  LR-PR10-WARNING-ERROR              VALUE  61 THRU 90.
           03  LR-RO20-RET-CODE                   REDEFINES
               LR-PR10-RET-CODE                   PIC 9(04)      COMP-6.

           03  LR-PR10-SPID                       PIC 9(02)      COMP-6.
               88  LR-PR10-SPID-EXCLUSION             VALUE 99.
           03  LR-RO20-SPID                       REDEFINES
               LR-PR10-SPID                       PIC 9(02)      COMP-6.
      
      *-----------------------------------------------------------------
       78  LR-PR10-MN-TBL-ENTRY-MAX               VALUE 4.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  LR-PR10-MN-MOST-NEG-ADV-ACT-TB.
               05  LR-PR10-MN-TBL-ENTRY           OCCURS
                   LR-PR10-MN-TBL-ENTRY-MAX.
                   07  LR-PR10-MN-ADVERSE-ACT-CODE   PIC S9(4)   COMP-3.
                   07  LR-RO20-MN-ADVERSE-ACT-CODE   REDEFINES
                       LR-PR10-MN-ADVERSE-ACT-CODE   PIC S9(4)   COMP-3.

                   07  LR-PR10-MN-ADVERSE-ACT-REASON PIC X(50).
                   07  LR-RO20-MN-ADVERSE-ACT-REASON REDEFINES
                       LR-PR10-MN-ADVERSE-ACT-REASON PIC X(50).
      
      *-----------------------------------------------------------------
           03  LR-PR10-DELQ-CLIENT-CNTL-IND       PIC X(01).
               88  LR-PR10-DELQ-CLNT-CNTL-SPID        VALUE '2'.
               88  LR-PR10-DELQ-CLNT-CNTL-TRMT        VALUE '1'.
               88  LR-PR10-DELQ-CLIENT-CNTL           VALUE '1' '2'.
               88  LR-PR10-DELQ-TRIAD-CNTL            VALUE '0'.
           03  LR-RO20-COLL-TENANT-CNTL-IND       REDEFINES
               LR-PR10-DELQ-CLIENT-CNTL-IND       PIC X(01).

           03  LR-PR10-DELQ-STGY-ID               PIC 9(03)      COMP-6.
           03  LR-RO20-COLL-STGY-ID               REDEFINES
               LR-PR10-DELQ-STGY-ID               PIC 9(03)      COMP-6.

           03  LR-PR10-DELQ-STMT-MSG-ID           PIC X(05).
           03  LR-RO20-COLL-FIXED-ACTION-1        REDEFINES
               LR-PR10-DELQ-STMT-MSG-ID           PIC X(05).

           03  LR-PR10-DELQ-COLL-IND-X            PIC X(06).
           03  LR-PR10-DELQ-COLL-IND              REDEFINES
               LR-PR10-DELQ-COLL-IND-X            PIC 9(06).
           03  LR-RO20-COLL-TIMED-ACTION-1        REDEFINES
               LR-PR10-DELQ-COLL-IND-X            PIC X(06).
           03  LR-RO20-COLL-TIMED-ACTION-1N       REDEFINES
               LR-PR10-DELQ-COLL-IND-X            PIC 9(06).

           03  LR-PR10-DELQ-BLK-CODE              PIC X(05).
           03  LR-RO20-COLL-TIMED-ACTION-2        REDEFINES
               LR-PR10-DELQ-BLK-CODE              PIC X(05).

           03  LR-PR10-DELQ-LTR-ID                PIC X(05).
           03  LR-RO20-COLL-TIMED-ACTION-3        REDEFINES
               LR-PR10-DELQ-LTR-ID                PIC X(05).

           03  LR-PR10-ACCT-PROFITABILITY         PIC S9(9)V9(4) COMP-3.
           03  LR-PR10-ACCT-GROSS-REVENUE         PIC S9(9)V9(4) COMP-3.
           03  LR-PR10-ACCT-GROSS-COSTS           PIC S9(9)V9(4) COMP-3.
           03  LR-PR10-AMT-MULTIPLIER             PIC 9(07)      COMP-6.

      *=================================================================
      *    NEW TRIAD 9 FIELDS
      *=================================================================
           03  LR-RO20-LN-CAT           OCCURS 12 PIC 9(02)      COMP-6.
           03  LR-RO20-COLL-DATE-BILL-EQV         PIC 9(06)      COMP-6.
           03  LR-RO20-DATE-FIRST-COLLS-DA        PIC 9(06)      COMP-6.
           03  LR-RO20-COLL-BALANCE-INITIAL       PIC S9(9)      COMP-3.
           03  LR-RO20-COLL-AMT-ARREARS           PIC S9(9)      COMP-3.
           03  LR-RO20-COLL-BALANCE-ACTUAL        PIC S9(9)      COMP-3.
           03  LR-RO20-COLL-PTP                   PIC 9(01).
           03  LR-RO20-BAR-FACTOR                 PIC S9(1)V9(7) COMP-3.
           03  LR-PR10-FILLER                     PIC X(61).

