      * COPYMEMBER: LIBLP/LP01TY
      *************************************************************
      *                     LPFDTY
      *              TODAY'S TOTALS FILE                   
      *                [ NEW VERSION ]       RSZ= 2369 (051898)
      *                                           2253 (100897)
      *                                          (2125 ????????)
      *                                          (1792 OLD-030697)
      *            (KEY: BRANCH/DATE/CLASS)            
      *
      * REV:
      *  061395 BLL EXPAND CKWRITTEN TABLE TO 8 
      *  MJD 100295 CHANGED PROMPTS FROM 90 TO 90-120 AND 120+ TO 150+.
      *  MJD 111795 CHANGED TY-DEL-BUCKETS TO OCCURS 10.
      *             ADDED EXTRA SPACE TO FILLER AT END OF RECORD.
      *  MJD 011696 ADDED TY-O2 AND TY-INT TO PERIOD TABLE.
      *             ADDED TY-O2-REPO AND TY-INT-REPO TO REPO TABLE.
      *             ADDED TY PL TABLE TO HOLD PL - P5.
      *             ADDED TY-PL-RECV TABLE TO TRACK DLR RSV AMOUNTS.
      *             ADDED TY-AMT-[SOLD/TRANS/PURCH]
      *                   TY-NO-[SOLD-TRANS-PURCH]
      *  MJD 012396 ADDED PL RECOVERY PRIN/INT/LATE COLLECTED
      *  MJD 012696 CHANGED DLR RESERVE INFO INTO A NET GAIN/LOSS - VOLUME
      *             TABLE. ADDED TY-PL-O2COL.
      *  MJD ADDED EXPLANATION OF TY-PL-RECOVERED TABLE.
      *  KEC 030797 ***REVISED***
      *              RECORD SIZE: NOW = 2059; WAS = 1792;
      *              REMOVED OLD APPLICATION FIELDS;
      *              ADDED   NEW APPLICATION FIELDS (TABLE).
      *  KEC 051297 ***REVISED***
      *              ADDED TY-NO-MADE.
      *  MJD 070197 ADDED TY-DLDISC. (MERCURY REQ#160)
      *  BLV 091197 NO RECORD CHANGE, BUT DELINQUENCY TABLE ORDER
      *             REVISED & WILL NOW USE ALL 10 BUCKETS
      *  MJD 100897 EXPANDED FIELDS IN ACTIVITY AND REPO TABLE TO
      *             9(5) FOR COUNTERS AND 9(8)V99 FOR AMOUNTS
      *  BLV 122297 TRASHED TY-INS-TBL & TY-MISC-INS-TBL
      *  BLV 031198 EXPANDED TY-SUMWGTTA, TYY-SUMWGTTM
      *  MJD 051898 ADDED DELINQ TABLE FOR BANKRUPTCY, ADDED BY-PART-CHGOFF
      *             FOR PARTIAL CHARGE OFFS, MOVED ALL FILLER TO END OF RECORD.
      *             EXPANDED REC SIZE TO 2369 BYTES
      *  MJD 990127 ----- UPDATED IN FLEXGEN -----
      *  BLV 990302 WILL USE OCCURRENCE 8 OF TY-CKWRITTEN TO RECORD
      *             NOTES PAYABLE CHECKS
      *  KEC 990510 <<<<<<<< 'A60-TO-A15' CONVERSION UPDATED >>>>>>>>
      *  BLV 000321 ADDED UNEARNED TABLE, MERC PR# 223,
      *             CORRECTED TY-SIZE FROM 2369 TO 2365
      *  MJD 000330 ADDED TY-PROCEEDS (MERC PR#223)
      *  MJD 000929 ADDED TY-LOCKBOX (MFC PR#237)
      *************************************************************
       78  TY-SIZE                    VALUE 2365.
       01  TY-REC.
           03  TY1-KEY.
               05  TY-BRNO            PIC 9(4).
               05  TY-DATE            PIC 9(7).
               05  TY-CLASS           PIC 9(2).

      ********************************************************
      *
      *    ACTIVITY TABLE: 1 - NET GAIN / LOSS
      *                    2 - NEW VOLUME
      *
      *    TYPE TABLE  :  1 - NB           7 - RN
      *                   2 - RF           8 - SC
      *                   3 - FB           9 - OT
      *                   4 - PB          10 - N/A
      *                   5 - PB DELINQ   11 - N/A
      *                   6 - PB INCR
      *
      ********************************************************
           03  TY-ACTIVITY-TBL        OCCURS 2.
               05  TY-TYPE-TBL        OCCURS 11.
                   07  TY-NO          PIC S9(5)    COMP.
                   07  TY-AMT         PIC S9(8)V99 COMP.
                   07  TY-O2          PIC S9(8)V99 COMP.
                   07  TY-INT         PIC S9(8)V99 COMP.
      ********************************************************
      *    REPO TABLE:
      *
      *    ACTIVITY TABLE:  1 - NET GAIN / LOSS         [TYNET]
      *                     2 - NEW VOLUME              [TYVOL]
      *
      *    TYPE TABLE    :  1 - REPO ON HAND            [TYONH]
      *                     2 - REPO REDEEMED           [TYRED]
      *                     3 - REPO SOLD LIVE REC      [TYSOL]
      *                     4 - REPO ON HAND P&L REC
      *                     5 - REPO SOLD P&L REC
      *
      ********************************************************
           03  TY-REPO-ACTIVITY-TBL   OCCURS 2.
               05  TY-REPO-TYPE-TBL   OCCURS 5.
                   07  TY-NO-REPO     PIC S9(5)    COMP.
                   07  TY-AMT-REPO    PIC S9(8)V99 COMP.
                   07  TY-O2-REPO     PIC S9(8)V99 COMP.
                   07  TY-INT-REPO    PIC S9(8)V99 COMP.
      *----------------------------------------------------------------------
      * PL ACTIVITY TBL:  1 - NET GAIN/LOSS [TYNET] | 2 - NEW VOLUME [TYVOL]
      *                   ---------------------------------------------------
      * PL TYPE TABLE  :  1 - PL ACTIVE         2 - PL INACTIVE
      *                   3 - P2 ACTIVE         4 - P2 INACTIVE
      *                   5 - P3 ACTIVE         6 - P3 INACTIVE
      *                   7 - P4 ACTIVE         8 - P4 INACTIVE
      *                   9 - P5 ACTIVE        10 - P5 INACTIVE
      *----------------------------------------------------------------------
           03  TY-PL-ACTIVITY-TBL        OCCURS 2.
               05  TY-PL-TYPE-TBL        OCCURS 10.
                   07  TY-NO-PL          PIC S9(5)    COMP.
                   07  TY-AMT-PL         PIC S9(8)V99 COMP.
                   07  TY-O2-PL          PIC S9(8)V99 COMP.
                   07  TY-INT-PL         PIC S9(8)V99 COMP.

      *----------------------------------------------------------------------
      *
      * DLR ACTIVITY TBL:  1 - NET GAIN/LOSS [TYNET] | 2 - NEW VOLUME [TYVOL]
      * 
      *----------------------------------------------------------------------
           03  TY-DLR-ACTIVITY-TBL        OCCURS 2.
               05  TY-DLRVPRIN   OCCURS 3
                                      PIC S9(8)V99 COMP.
               05  TY-DLRVCHGS   OCCURS 3
                                      PIC S9(8)V99 COMP.
               05  TY-DLPARVHELD      PIC S9(8)V99 COMP.

      *      -----------------------------------------------
      *                TY-PL-RECOVERED    1 - PL RECOVERIES
      *                                   2 - P2 RECOVERIES
      *                                   3 - P3 RECOVERIES
      *                                   4 - P4 RECOVERIES
      *                                   5 - P5 RECOVERIES
      *      -----------------------------------------------
           03  TY-PL-RECOVERED OCCURS 5.
               05  TY-PL-PRINCOL      PIC S9(8)V99 COMP.
               05  TY-PL-INTCOL       PIC S9(8)V99 COMP.
               05  TY-PL-LATECOL      PIC S9(8)V99 COMP.
               05  TY-PL-O2COL        PIC S9(8)V99 COMP.

      *----------------------------------------------------------------
      * SOLD  : LOANS SOLD/TRANSFERRED VIA BULKTR.C
      * TRANS : TRANSFERRED TO A GIVEN BRANCH VIA BULKTR.C
      * PURCH : MIDSTREAM PURCHASE FROM OUTSIDE SOURCE.
      *----------------------------------------------------------------
           03  TY-AMT-SOLD            PIC S9(8)V99 COMP.
           03  TY-AMT-TRANS           PIC S9(8)V99 COMP.
           03  TY-AMT-PURCH           PIC S9(8)V99 COMP.

           03  TY-NO-SOLD             PIC S9(5)    COMP.
           03  TY-NO-TRANS            PIC S9(5)    COMP.
           03  TY-NO-PURCH            PIC S9(5)    COMP.

           03  TY-OTHCOL              PIC S9(8)V99 COMP.
           03  TY-OT2COL              PIC S9(8)V99 COMP.
           03  TY-INTCOL              PIC S9(8)V99 COMP.
           03  TY-LATECOL             PIC S9(8)V99 COMP.
           03  TY-PRINCOL             PIC S9(8)V99 COMP.
           03  TY-DEFCOL              PIC S9(8)V99 COMP.
           03  TY-RNPRIN              PIC S9(8)V99 COMP.
           03  TY-INTREB              PIC S9(8)V99 COMP.
           03  TY-CLREB1              PIC S9(8)V99 COMP.
           03  TY-CLREB2              PIC S9(8)V99 COMP.
           03  TY-CLREB3              PIC S9(8)V99 COMP.
           03  TY-CLREB4              PIC S9(8)V99 COMP.
           03  TY-AHREB               PIC S9(8)V99 COMP.
           03  TY-PPREB               PIC S9(8)V99 COMP.
           03  TY-O1REB               PIC S9(8)V99 COMP.
           03  TY-O2REB               PIC S9(8)V99 COMP.
           03  TY-O3REB               PIC S9(8)V99 COMP.
           03  TY-O4REB               PIC S9(8)V99 COMP.
           03  TY-O5REB               PIC S9(8)V99 COMP.
           03  TY-O6REB               PIC S9(8)V99 COMP.
           03  TY-O7REB               PIC S9(8)V99 COMP.
           03  TY-SCREB               PIC S9(8)V99 COMP.
           03  TY-MFREB               PIC S9(8)V99 COMP.
           03  TY-DFREB               PIC S9(8)V99 COMP.
           03  TY-PART-CHGOFF         PIC S9(8)V99 COMP.
      * WORLD
      * SLOT 8 IS NON-FILE, 9 IS AD&D, 10 IS ROADGARD
           03  TY-INS-TYPE            OCCURS 10.
               05  TY-INS-NO-ELIG     PIC S9(6)    COMP.
               05  TY-INS-NO-WROTE    PIC S9(6)    COMP.
               05  TY-INS-NO-NOTWROTE PIC S9(6)    COMP.
               05  TY-INS-AMT-WROTE   PIC S9(8)V99 COMP.

      *-------------------------------------------------------
      *    *NO-COL FIELDS REFLECT THE TOTAL NUMBER OF TIME
      *                   THEY WERE COLLECTED, EVEN THOUGH
      *                   THEIR COUNTER FIELDS ARE BROKEN
      *                   UP FOR NON P&L AND P&L
      *-------------------------------------------------------
           03  TY-OTH-NO-COL          PIC S9(6)    COMP.
           03  TY-OT2-NO-COL          PIC S9(6)    COMP.
           03  TY-INT-NO-COL          PIC S9(6)    COMP.
           03  TY-LATE-NO-COL         PIC S9(6)    COMP.

           03  TY-OTH-NO-WAIVED       PIC S9(6)    COMP.
           03  TY-OT2-NO-WAIVED       PIC S9(6)    COMP.
           03  TY-INT-NO-WAIVED       PIC S9(6)    COMP.
           03  TY-LATE-NO-WAIVED      PIC S9(6)    COMP.

           03  TY-OTH-AMT-WAIVED      PIC S9(8)V99 COMP.
           03  TY-OT2-AMT-WAIVED      PIC S9(8)V99 COMP.
           03  TY-INT-AMT-WAIVED      PIC S9(8)V99 COMP.
           03  TY-LATE-AMT-WAIVED     PIC S9(8)V99 COMP.
      *---------------------------------------------------------------
      * (LOAN, DEALER, MISC, OVERPYMNT, NSF CKS, WITHDRLS, INT DEP, FUTURE)
           03  TY-CKWRITTEN           OCCURS 8
                                      PIC S9(7)V99 COMP.
      *---------------------------------------------------------------
      *    -------------------------------------------------
      *    TY-REC-CON
      *        1- RECENCY
      *        2- CONTRACTUAL
      *
      *    TY-DEL-BUCKET
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
      *    NOTE:
      *          IF GP-DELINQ-WITH-O2 = "Y" THEN THESE FIELDS WILL
      *             INCLUDE O2 BALANCES.
      *    -------------------------------------------------
           03  TY-REC-CON                 OCCURS 2.
               05  TY-DEL-BUCKET          OCCURS 10.
      *        DELINQUENT UNITS:
                   07  TY-UNITS           PIC S9(7)    COMP.
      *        OUTSTANDING BALANCE:
                   07  TY-OUTBAL          PIC S9(9)V99 COMP.
      *    -------------------------------------------------
      *---------------------------------------------------------------
      *    -------------------------------------------------
      *    TY-REC-CON-BNKRPT
      *        1- RECENCY
      *        2- CONTRACTUAL
      *
      *    TY-DEL-BUCKET-BNKRPT
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
      *    NOTE:
      *          IF GP-DELINQ-WITH-O2 = "Y" THEN THESE FIELDS WILL
      *             INCLUDE O2 BALANCES.
      *    -------------------------------------------------
           03  TY-REC-CON-BNKRPT          OCCURS 2.
               05  TY-DEL-BUCKET-BNKRPT   OCCURS 10.
      *        DELINQUENT UNITS:
                   07  TY-UNITS-BNKRPT    PIC S9(7)    COMP.
      *        OUTSTANDING BALANCE:
                   07  TY-OUTBAL-BNKRPT   PIC S9(9)V99 COMP.
      *    -------------------------------------------------
           03  TY-SUMWGTTM                PIC S9(8)    COMP.
           03  TY-SUMWGTAV                PIC S9(9)V99 COMP.
           03  TY-SUMWGTTA                PIC S9(11)V99 COMP.
           03  TY-ALLOTMENTS              PIC S9(7)V99 COMP.
           03  TY-BRFG                    PIC X.

      *-------------------- APPLICATIONS -------------------------------
      *    ACTIVITY TBL:  1 - NET GAIN/LOSS  [TYNET] |  2 - NEW VOLUME [TYVOL]
      *                   ____________________________________________________
      *    TYPE TABLE  :  1 - NB                7 - RN
      *                   2 - RF                8 - SC
      *                   3 - FB                9 - OT
      *                   4 - PB               10 - N/A
      *                   5 - PB DELINQ        11 - N/A
      *                   6 - PB INCR         
      *    NOTE: STATISTICS FOR:
      *            APPS MADE IS THE ACTUAL LOAN VOLUME CARRIED UNDER TY-NO
      *            APPS NOT TAKEN IS NOT EASILY MAINTAINED
      *         
      *-----------------------------------------------------------------
           03  TY-APP-ACTIVITY-TBL            OCCURS 2.
               05  TY-APP-TYPE-TBL            OCCURS 11.
                   07  TY-NO-PENDING          PIC S9(5)    COMP.
                   07  TY-NO-COND-APP         PIC S9(5)    COMP.
                   07  TY-NO-APPROVED         PIC S9(5)    COMP.
                   07  TY-NO-MADE             PIC S9(5)    COMP.
                   07  TY-NO-TURNDOWN         PIC S9(5)    COMP.
                   07  TY-NO-TURNUSDOWN       PIC S9(5)    COMP.

      *-----------------------------------------------------------------
      *   DLDISC: DEALER DISCOUNT VOLUME
      *-----------------------------------------------------------------
           03  TY-DLDISC                      PIC S9(8)V99 COMP.
      *-----------------------------------------------------------------
      *    UNEARNED ACT:  1 - NET GAIN/LOSS          |  2 - NEW VOLUME        
      *                   ____________________________________________________
      *    UNEARNED TABLE: 1 - INTEREST                
      *                    2 - ADDON 
      *                    3 - SERVICE CHARGE              
      *                    4 - MAINTENANCE FEES             
      *                    5 - DEALER DISCOUNT
      *                    6 - OTHER
      *    ------------------------------------------------------
           03  TY-UNEARNED-ACTIVITY           OCCURS 2.
               05  TY-UNEARNED-TYPE           OCCURS 6.
                   07  TY-UNEARNED-AMT        PIC S9(8)V99 COMP.
           03  TY-PROCEEDS                    PIC S9(8)V99 COMP.
           03  TY-LOCKBOX                     PIC S9(7)V99 COMP.
           03  FILLER                         PIC X(31).

