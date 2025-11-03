      * COPYMEMBER: LIBNP/NP01NT
      ***************************************************************
      *       NT-FILE - NOTE MASTER RECORD
      *  021191 BAH ADDED NT-BEG-NPSTMT NT-END-NPSTMT
      *             MUST HOLD BEG - END STMT DATES SO
      *             THE PRINT PROG. KNOWS
      *  981012 BLV ADDED NT-TRANSFER-ACCTNO (IF INTEREST IS
      *             TO BE TRANSFERRED; NT-INTPYCD = T?
      *  981022 BLV RENAMED LASTPMT TO LAST-STMT-DATE
      *  981022 BLV RENAMED CURFG TO ACTIVE-CD
      *  981109 BLV ADDED NT-INTPY-DATE-CD (ANNIVERSARY OR CALENDAR) 
      *  981116 MJD ADDED MTD/YTD/LYR/NYR TO NT-INTWHELD, ADDED
      *             NYR TO NT-INTEARNED
      *  981207 BLV ADDED NT7-KEY: BRANCH, CLASS, ACCT
      *  981217 GLF ADDED NT-LAST-RN-DATE               
      *  981218 ADDED NT-NC-SEQNO
      *  990121 MJD REMOVED NT-STAT()
      *             ----- IMPORTED INTO FLEXGEN -----
      *  CS  1999.0309 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  990805 GLB  ADD NT-TO-DATE-TABLE OF 5.
      *  KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  BLV 990924 ADDED NT-DAYS-ACCRUED, DAYS ACCRUED SINCE LAST 
      *             ANNIVERSARY PMT
      *  GLB 991001 ADD SIGN TO NT-PARTAMT, USE FOR PAYMENTS ADJUSTS
      *  GLB 991004 RENAME NT-DAYS-ACCRUED TO NT-DAYS-BAL
      *  GLB 991207 INTWHELD NEXT YR REMOVED, FORFEIT LAST YR ADDED
      *  BLV 991221 UNDID 12/07/99 CHANGED, NEED INTWHELD NEXT YR, 
      *             ADDED NEW FIELD FOR LAST YR FORFEITURES,      
      *             RESTORED NEXT YR BUCKET FOR WITHHELD
      *  KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  BLF 041208 ADDED NT-LAST-ACTIVITY-DATE, REGWAN PR# 2410
      *  AJM 121204 ADDED NT-HOLD, NT-DORMANT-REASON-CD, REGWAN PR#4150
      ***************************************************************
       78  NT-SIZE                     VALUE 397.
       01  NT-REC.
           03  NT7-KEY.
               05  NT-BRNO             PIC 9(4)      COMP-3.
               05  NT6-KEY.
                   07  NT-CLASS        PIC 9(2)      COMP-3.
                   07  NT1-KEY.
                       09  NT-ACCTNO   PIC 9(6)      COMP-3.
      * 10,15,20,25
           03  NT-SSNOS.
               05  NT-IVTBL            OCCURS 4.
                   07  NT-SSNO         PIC 9(9)     COMP-3.
           03  NT-SSNOSX REDEFINES NT-SSNOS.
               05  NT2-KEY             PIC X(5).
               05  NT3-KEY             PIC X(5).
               05  NT4-KEY             PIC X(5).
               05  NT5-KEY             PIC X(5).
      * 30
           03  NT-REGISTRATION-CD      PIC X.
      * 31
           03  NT-ISSDATE              PIC 9(6)      COMP-3.
           03  NT-MATUDATE             PIC 9(6)      COMP-3.
           03  NT-LAST-STMT-DATE       PIC 9(6)      COMP-3.
      * 43
           03  NT-ACCRDATE             PIC 9(6)      COMP-3.
           03  NT-NOSHARES             PIC 9(8)      COMP-3.
           03  NT-NOTEAMT              PIC S9(9)V99  COMP-3.
      * 58
           03  NT-NOTEBAL              PIC S9(9)V99  COMP-3.
      * 64
           03  NT-PREVBAL              PIC S9(9)V99  COMP-3.
           03  NT-ACCRUAL              PIC S9(9)V99  COMP-3.
           03  NT-EXPYTD               PIC S9(9)V99  COMP-3.
      *    INTEREST WITHHELD - MTD, YTD, LYR, NYR.
           03  NT-INTWHELD-TABLE.
               05  NT-INTWHELD         OCCURS 4
                                       PIC S9(9)V99  COMP-3.
      * 106
           03  NT-NOTERATE             PIC 99V99     COMP-3.
           03  NT-FACERATE             PIC 99V99     COMP-3.
           03  NT-YRLYPMTS             PIC 9(2)      COMP-3.
      *    HOLD INTEREST CHECK ?
           03  NT-INTCKCD              PIC X.
      *    OLD NOTE RETURNED ?
           03  NT-NTRETFG              PIC X.
      * 116   F-FULL, P-PARTIAL, N-NONE
           03  NT-INTPYCD              PIC X.
      *    PARTIAL INTEREST PMTS
           03  NT-PARTAMT              PIC S9(6)V99  COMP-3.
           03  NT-PARTPCT              PIC 99V99     COMP-3.
      *    INTEREST STATISTICS - MTD, YTD, LYR
           03  NT-STAT-TABLE.
      *    INTEREST EARNED - MTD, YTD, LYR, NYR.
      *    12/07/99 NEXT YR INTEARNED IS AVAILABLE
               05  NT-INTEARNED        OCCURS 4
                                       PIC S9(9)V99  COMP-3.
               05  NT-STAT-MYL         OCCURS 3.
      * 149, 167, 185
                   07  NT-INTACCUM     PIC S9(9)V99  COMP-3.
                   07  NT-INVADDIT     PIC S9(9)V99  COMP-3.
                   07  NT-INVWDRAL     PIC S9(9)V99  COMP-3.
      * 203, 233, 263
           03  NT-ALPHA  OCCURS 3      PIC X(30).
           03  NT-ACTIVE-CD            PIC X.
           03  NT-CURFG REDEFINES NT-ACTIVE-CD PIC X.
           03  NT-NHSEQNO              PIC 9(4)      COMP-3.
      * 297
           03  NT-PAYDATE              PIC 9(6)      COMP-3.
           03  NT-ABLETTER             PIC X.
           03  NT-BEG-NPSTMT           PIC 9(6)      COMP-3.
           03  NT-END-NPSTMT           PIC 9(6)      COMP-3.
           03  NT-TRANSFER-ACCTNO      PIC 9(6)      COMP-3.
           03  NT-FORFEITAMT           PIC S9(7)V99  COMP-3.
           03  NT-FORFEIT-YTD REDEFINES NT-FORFEITAMT
                                       PIC S9(7)V99  COMP-3.
           03  NT-ROLLOVER-CD          PIC X.
           03  NT-TAXCODE              PIC X.
           03  NT-TRANSFER-COUNT       PIC 99        COMP-3.
           03  NT-WITHHOLD-PCT         PIC S99V999 COMP-3.
           03  NT-INTPY-DATE-CD        PIC X.
           03  NT-LAST-RN-DATE         PIC 9(6)      COMP-3.
           03  NT-NC-SEQNO             PIC 9(2).
           03  NT-TO-DATE-TABLE.
               05  NT-INTWHELD-TD         PIC S9(9)V99  COMP-3.
               05  NT-INTEARNED-TD        PIC S9(9)V99  COMP-3.
               05  NT-INTACCUM-TD         PIC S9(9)V99  COMP-3.
               05  NT-INVADDIT-TD         PIC S9(9)V99  COMP-3.
               05  NT-INVWDRAL-TD         PIC S9(9)V99  COMP-3.
           03  NT-DAYS-BAL                PIC S9(3)     COMP-3.
           03  NT-FORFEIT-LYR             PIC S9(7)V99  COMP-3.
           03  NT-ALPHA-FG                PIC X.
           03  NT-LAST-ACTIVITY-DATE      PIC 9(6)      COMP-6.
           03  NT-HOLD                    PIC X.
           03  NT-DORMANT-REASON-CD       PIC X(3).
           03  FILLER                     PIC X(8).
      *  NEED TO SAVE OLD REGENCY ACCTNO FOR CONVERSION & REFERENCE
      *  1/10/03 REMOVED REDEFS OF THIS OLD ACCT#, STILL THERE IN
      *  OLD RECORDS AT REGWAN, BUT ALSO IN FIRST PRIORITY MEMO,
      *  SO WE COULD CLEAR IF WE NEEDED SPACE...BARB
           03  NT-REG-ACCTNO           PIC X(12).
