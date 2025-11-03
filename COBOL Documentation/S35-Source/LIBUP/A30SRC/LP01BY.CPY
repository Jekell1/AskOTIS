      * COPYMEMBER: LIBLP/LP01BY
      ******************************************************************
      *
      *                     'BY'
      *            ( BRANCH STATISTICS FILE )
      *                 N E W  VERSION
      * RSZ = 8400 (5755 OLD 112097) (5120 OLD-030697)(2970 OLD)
      *
      * NOTE:  IF YOU CHANGE THIS RECORD, YOU MUST MAKE THE CHANGE TO
      *        THE WORK RECORD IN SMITHS.C FOR WORLD: BY-REC IS
      *        MOVED TO WK-REC IN THIS PROGRAM!!!
      *
      * NOTE:  IF YOU CHANGE THIS RECORD, YOU MUST MAKE THE CHANGE TO
      *        THE BRANCH CONSOLIDATION BRANCH STAT FILE
      *        (LIBBC/BC01BY): BY-REC IS BCBY-REC.
      *
      *=================================================================
      * REV:
      * MJD 100295 CHANGED PROMPT FROM 90+ TO 90-120.
      * MJD 100595 ADDED BY-IB-ACCRUED.
      * MJD 111695 REDUCED BY-DEL-BUCKETS TO OCCURS 10.
      * MJD 011696 ADDED BY-O2 AND BY-INT TO PERIOD TABLE.
      *            ADDED BY-O2-REPO AND BY-INT-REPO TO REPO TABLE.
      *            ADDED BY PL TABLE TO HOLD PL - P5.
      *            ADDED BY-PL-RECV TABLE TO TRACK DLR RSV AMOUNTS.
      * MJD 012396 ADDED PL-RECOVERY  PRINCOL/INTCOL/LATECOL
      * MJD 012696 CHANGED DLR DLRVPRIN, DLRVCHGS, AND DLPARVHELD INTO
      *            A GAIN/LOSS- VOLUME TABLE.
      *            ADDED BY-PL-O2COL TO RECOVERIES.
      * MJD ADDED EXPLANATION OF BY-PL-RECOVERED TABLE.
      * MJD 091796 ADDED NOT ABOVE REGARDING LP01BYFLEX
      * KEC 030797 ***REVISED***
      *             RECORD SIZE: NOW = 5755; WAS = 5120;
      *             REMOVED OLD APPLICATION FIELDS;
      *             ADDED   NEW APPLICATION FIELDS (TABLE).
      * KEC 051297 ***REVISED***
      *             ADDED BY-NO-MADE.
      * MJD 063097 ADDED BY-DLDISC-VOL-TBL. (MERCURY REQ # 160)
      * MJD 082797 ADDED BY-PROJ-INCREASE, BY-PROJ-PROFIT,
      *            BR-30-DAY-MOVED, AND BY-STD-POS
      * BLV 091197 NO RECORD CHANGE, BUT DELINQUENCY TABLE ORDER
      *            REVISED & WILL NOW USE ALL 10 BUCKETS
      * MJD 091297 BY-STD-POS ONLY NEEDS TO OCCUR ONCE.
      * JTG 112097 EXPANDED RECORD BY 200 BYTES
      * BLV 122297 TRASHED BY-MISC-VOL-TBL
      * BLV 031198 EXPANDED BY-SUMWGTTA, BY-SUMWGTTM
      * MJD 051898 ADDED DELINQ TABLE FOR BANKRUPTCY, ADDED BY-PART-CHGOFF
      *            FOR PARTIAL CHARGE OFFS, MOVED ALL FILLER TO END OF RECORD.
      *            EXPANDES RECORD SIZE TO 6399 BYTES
      * MJD 990125 ----- UPDATED IN FLEXGEN -----
      * KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * BLV 990818 SLOTS 4 & 5 IN REPO TABLE NOW REFLECT REPO ON HAND
      *            & REPO SOLD IN P&L RECEIVABLES, NO CHANGES TO
      *            RECORD SIZE, THESE WERE UNUSED OCCURANCES
      * BLV 000321 ADD UNEARNED TABLE, MERCURY PR# 223
      * MJD 000330 ADDED BY-PROCEEDS (MERC PR#223)
      * KEC 2000.0411 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * MJD 000929 ADDED BY-LOCKBOX (MFX PR#237)
      * BLV 020320 NO RECORD CHANGE, JUST MOVED FIELDS FROM
      *            BY-DLDISC-VOL-TBL DOWN FROM 03'S TO 05'S, SO THEY
      *            WILL BE INCLUDED UNDER BY-DATA GROUP ITEM
      * BLM 160217 CHANGED DATE TO BE CCYYMM, CHANGED UNUSED FROM
      *            100 TO 1000, EXPANDED UNEARNED TABLE FROM 6 TO 10,
      *            ADDED BY-LTOUCH-DATE
      * KEC 2016.0929 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADJUST BY-REC/BY-SIZE TO AN EVEN NUMBER (RSZ=7500).
      *         CHANGED BY-DATE TO BE BY-DATE-CCYYMM FOR CONSISTENCY.
      *         MOVED BY-LTOUCH-DATE TO AFTER BY1-KEY FOR CONSISTENCY.
      *  CS 16104 ADD BY-UN7-REB, BY-UN8-REB, BY-UN9-REB, BY-UN10-REB
      *           REDUCED FILLER FROM 990 TO 950
      * BLM 161115 CHANGED ALL COMP FIELDS TO COMP-3. REC-SIZE CHANGED
      *            FROM 7500 TO 8400
      ******************************************************************
       78  BY-SIZE                        VALUE 8400.
       01  BY-REC.
           03  BY1-KEY.
               05  BY-DATE-CCYYMM      PIC 9(6).
               05  BY-BRNO             PIC 9(4).
               05  BY-CLASS            PIC 9(3).

           03  BY-LTOUCH-DATE          PIC 9(08)    COMP-3.

           03  BY-DATA.
      *    ------------------------------------------------------
      *    NOTE:
      *          THESE FIELDS ARE FOR NON P&L, NON REPO ACCOUNTS
      *
      *    PERIOD TABLE:  ____________________________________________________
      *                   1 - CURRENT        [BYCUR] |  1 - MTD        [BYMTD]
      *                   2 - END LAST MONTH [BYELM] |  2 - YTD        [BYYTD]
      *                   3 - END LAST YEAR  [BYELY] |  3 - LYR        [BYLYR]
      *                                              |
      *    ACTIVITY TBL:  1 - NET GAIN/LOSS  [BYNET] |  2 - NEW VOLUME [BYVOL]
      *                   ____________________________________________________
      *    TYPE TABLE  :  1 - NB                7 - RN
      *                   2 - RF (OR SF)        8 - SC
      *                   3 - FB                9 - OT
      *                   4 - PB               10 - RB
      *                   5 - PB DELINQ        11 - RO
      *                   6 - PB INCR
      *
      *            NOTE: BY-AMT INCLUDES BY-O2
      *    ------------------------------------------------------
               05  BY-PERIOD-TBL              OCCURS 3.
                   07  BY-ACTIVITY-TBL        OCCURS 2.
                       09  BY-TYPE-TBL        OCCURS 11.
                           11  BY-NO          PIC S9(5)    COMP-3.
                           11  BY-AMT         PIC S9(8)V99 COMP-3.
                           11  BY-O2          PIC S9(8)V99 COMP-3.
                           11  BY-INT         PIC S9(8)V99 COMP-3.
      *----------------------------------------------------------------------
      *    NOTE:
      *          THESE FIELDS ARE REPO ACCOUNTS ONLY
      *    REPO TABLE:
      *    PERIOD TABLE:  ____________________________________________________
      *                   1 - CURRENT        [BYCUR] |  1 - MTD        [BYMTD]
      *                   2 - END LAST MONTH [BYELM] |  2 - YTD        [BYYTD]
      *                   3 - END LAST YEAR  [BYELY] |  3 - LYR        [BYLYR]
      *
      *    ACTIVITY TBL:  1 - NET GAIN / LOSS             [BYNET]
      *                   2 - NEW VOLUME                  [BYVOL]
      *
      *    TYPE TABLE  :  1 - REPO ON HAND                [BYONH]
      *                   2 - REPO REDEEMED               [BYRED]
      *                   3 - REPO SOLD LIVE RECEIVABLE   [BYSOL]
      *                   4 - REPO ON HAND P&L RECEIVABLE
      *                   5 - REPO SOLD P&L RECEIVABLE
      *
      *            NOTE: BY-AMT-REPO INCLUDES BY-O2-REPO
      *----------------------------------------------------------------------
               05  BY-REPO-PERIOD-TBL         OCCURS 3.
                   07  BY-REPO-ACTIVITY-TBL   OCCURS 2.
                       09  BY-REPO-TYPE-TBL   OCCURS 5.
                           11  BY-NO-REPO     PIC S9(4)    COMP-3.
                           11  BY-AMT-REPO    PIC S9(7)V99 COMP-3.
                           11  BY-O2-REPO     PIC S9(7)V99 COMP-3.
                           11  BY-INT-REPO    PIC S9(7)V99 COMP-3.

      *----------------------------------------------------------------------
      *    NOTE:
      *          THESE FIELDS ARE P&L NON REPO & P&L REPO ACCOUNTS
      *
      * PL PERIOD TABLE: ----------------------------------------------------
      *                  1 - CURRENT        [BYCUR] | 1 - MTD        [BYMTD]
      *                  2 - END LAST MONTH [BYELM] | 2 - YTD        [BYYTD]
      *                  3 - END LAST YEAR  [BYELY] | 3 - LYR        [BYLYR]
      *                                             |
      * PL ACTIVITY TBL:  1 - NET GAIN/LOSS [BYNET] | 2 - NEW VOLUME [BYVOL]
      *                   ---------------------------------------------------
      * PL TYPE TABLE  :  1 - PL ACTIVE         2 - PL INACTIVE
      *                   3 - P1 ACTIVE         4 - P1 INACTIVE
      *                   5 - P3 ACTIVE         6 - P3 INACTIVE
      *                   7 - P4 ACTIVE         8 - P4 INACTIVE
      *                   9 - P5 ACTIVE        10 - P5 INACTIVE
      *
      *            NOTE: BY-AMT-PL INCLUDES BY-O2-PL
      *----------------------------------------------------------------------
               05  BY-PL-PERIOD-TBL              OCCURS 3.
                   07  BY-PL-ACTIVITY-TBL        OCCURS 2.
                       09  BY-PL-TYPE-TBL        OCCURS 10.
                           11  BY-NO-PL          PIC S9(5)    COMP-3.
                           11  BY-AMT-PL         PIC S9(8)V99 COMP-3.
                           11  BY-O2-PL          PIC S9(8)V99 COMP-3.
                           11  BY-INT-PL         PIC S9(8)V99 COMP-3.

      *----------------------------------------------------------------------
      * DLR PERIOD TABLE: ----------------------------------------------------
      *                   1 - CURRENT        [BYCUR] | 1 - MTD        [BYMTD]
      *                   2 - END LAST MONTH [BYELM] | 2 - YTD        [BYYTD]
      *                   3 - END LAST YEAR  [BYELY] | 3 - LYR        [BYLYR]
      *                                              |
      * DLR ACTIVITY TBL: 1 - NET GAIN/LOSS [BYNET]  | 2 - NEW VOLUME [BYVOL]
      *
      *----------------------------------------------------------------------
               05  BY-DLR-PERIOD-TBL              OCCURS 3.
                   07  BY-DLR-ACTIVITY-TBL        OCCURS 2.
                       09  BY-DLRVPRIN   OCCURS 3
                                              PIC S9(8)V99 COMP-3.
                       09  BY-DLRVCHGS   OCCURS 3
                                              PIC S9(8)V99 COMP-3.
                       09  BY-DLPARVHELD      PIC S9(8)V99 COMP-3.

      *    -------------------------------------------------
      *    SUBSCRIPTED FOR MTD AND YTD FIGURE
      *        1- MTD                     [BYMTD]
      *        2- YTD                     [BYYTD]
      *
      *    NOTE: BY-PL-PRINCOL INCLUDES BY-PL-O2COL
      *    -------------------------------------------------
               05  BY-MTD-YTD                 OCCURS 2.

      *      -----------------------------------------------
      *                BY-PL-RECOVERED    1 - PL RECOVERIES
      *                                   2 - P2 RECOVERIES
      *                                   3 - P3 RECOVERIES
      *                                   4 - P4 RECOVERIES
      *                                   5 - P5 RECOVERIES
      *      -----------------------------------------------
                   07  BY-PL-RECOVERED OCCURS 5.
                       09  BY-PL-PRINCOL      PIC S9(8)V99 COMP-3.
                       09  BY-PL-INTCOL       PIC S9(8)V99 COMP-3.
                       09  BY-PL-LATECOL      PIC S9(8)V99 COMP-3.
                       09  BY-PL-O2COL        PIC S9(8)V99 COMP-3.

      *----------------------------------------------------------------
      * SOLD  : LOANS SOLD/TRANSFERRED VIA BULKTR.C
      * TRANS : TRANSFERRED TO A GIVEN BRANCH VIA BULKTR.C
      * PURCH : MIDSTREAM PURCHASE FROM OUTSIDE SOURCE.
      *----------------------------------------------------------------
                   07  BY-AMT-SOLD            PIC S9(8)V99 COMP-3.
                   07  BY-AMT-TRANS           PIC S9(8)V99 COMP-3.
                   07  BY-AMT-PURCH           PIC S9(8)V99 COMP-3.

                   07  BY-NO-SOLD             PIC S9(5)    COMP-3.
                   07  BY-NO-TRANS            PIC S9(5)    COMP-3.
                   07  BY-NO-PURCH            PIC S9(5)    COMP-3.

                   07  BY-OTHCOL              PIC S9(8)V99 COMP-3.
                   07  BY-OT2COL              PIC S9(8)V99 COMP-3.
                   07  BY-INTCOL              PIC S9(8)V99 COMP-3.
                   07  BY-LATECOL             PIC S9(8)V99 COMP-3.
                   07  BY-PRINCOL             PIC S9(8)V99 COMP-3.
                   07  BY-DEFCOL              PIC S9(8)V99 COMP-3.
                   07  BY-RNPRIN              PIC S9(8)V99 COMP-3.
                   07  BY-INTREB              PIC S9(8)V99 COMP-3.
                   07  BY-CLREB1              PIC S9(8)V99 COMP-3.
                   07  BY-CLREB2              PIC S9(8)V99 COMP-3.
                   07  BY-CLREB3              PIC S9(8)V99 COMP-3.
                   07  BY-CLREB4              PIC S9(8)V99 COMP-3.
                   07  BY-AHREB               PIC S9(8)V99 COMP-3.
                   07  BY-PPREB               PIC S9(8)V99 COMP-3.
                   07  BY-O1REB               PIC S9(8)V99 COMP-3.
                   07  BY-O2REB               PIC S9(8)V99 COMP-3.
                   07  BY-O3REB               PIC S9(8)V99 COMP-3.
                   07  BY-O4REB               PIC S9(8)V99 COMP-3.
                   07  BY-O5REB               PIC S9(8)V99 COMP-3.
                   07  BY-O6REB               PIC S9(8)V99 COMP-3.
                   07  BY-O7REB               PIC S9(8)V99 COMP-3.
                   07  BY-SCREB               PIC S9(8)V99 COMP-3.
                   07  BY-MFREB               PIC S9(8)V99 COMP-3.
                   07  BY-DFREB               PIC S9(8)V99 COMP-3.
                   07  BY-UN7-REB             PIC S9(8)V99 COMP-3.
                   07  BY-UN8-REB             PIC S9(8)V99 COMP-3.
                   07  BY-UN9-REB             PIC S9(8)V99 COMP-3.
                   07  BY-UN10-REB            PIC S9(8)V99 COMP-3.

                   07  BY-PART-CHGOFF         PIC S9(8)V99 COMP-3.
                   07  BY-ALLOTMENTS          PIC S9(8)V99 COMP-3.
                   07  BY-INS-TYPE            OCCURS 10.
                       09  BY-INS-NO-ELIG     PIC S9(6)    COMP-3.
                       09  BY-INS-NO-WROTE    PIC S9(6)    COMP-3.
                       09  BY-INS-NO-NOTWROTE PIC S9(6)    COMP-3.
                       09  BY-INS-AMT-WROTE   PIC S9(8)V99 COMP-3.
      * 2/18/16 - NOTE, OCCURANCE 1 OF THIS TABLE WILL BE NON-FILE
      * 2/18/16 - NOTE, OCCURANCE 2 OF THIS TABLE WILL BE AD&D
      * 2/18/16 - NOTE, OCCURANCE 3 OF THIS TABLE WILL BE ROADGARD
                   07  BY-FEE-TYPE            OCCURS 3.
                       09  BY-FEE-NO-ELIG     PIC S9(6)    COMP-3.
                       09  BY-FEE-NO-WROTE    PIC S9(6)    COMP-3.
                       09  BY-FEE-NO-NOTWROTE PIC S9(6)    COMP-3.
                       09  BY-FEE-AMT-WROTE   PIC S9(8)V99 COMP-3.

                   07  BY-OTH-NO-COL          PIC S9(6)    COMP-3.
                   07  BY-OT2-NO-COL          PIC S9(6)    COMP-3.
                   07  BY-INT-NO-COL          PIC S9(6)    COMP-3.
                   07  BY-LATE-NO-COL         PIC S9(6)    COMP-3.

                   07  BY-OTH-NO-WAIVED       PIC S9(6)    COMP-3.
                   07  BY-OT2-NO-WAIVED       PIC S9(6)    COMP-3.
                   07  BY-INT-NO-WAIVED       PIC S9(6)    COMP-3.
                   07  BY-LATE-NO-WAIVED      PIC S9(6)    COMP-3.

                   07  BY-OTH-AMT-WAIVED      PIC S9(8)V99 COMP-3.
                   07  BY-OT2-AMT-WAIVED      PIC S9(8)V99 COMP-3.
                   07  BY-INT-AMT-WAIVED      PIC S9(8)V99 COMP-3.
                   07  BY-LATE-AMT-WAIVED     PIC S9(8)V99 COMP-3.
      *    -------------------------------------------------
      *    NOTE:
      *          THESE FIELDS ARE NON P&L, NON REPO ACCOUNTS
      *          IF GP-DELINQ-WITH-O2 = "Y" THEN THESE FIELDS WILL
      *             INCLUDE O2 BALANCES.
      *
      *    BY-NOW-BMO-LME
      *        1- NOW   (CURRENT)         [BYNOW]
      *        2- BEGINNING OF MONTH      [BYBMO]
      *        3- LAST MONTH END          [BYLME]
      *
      *    BY-REC-CON
      *        1- RECENCY                 [BYREC]
      *        2- CONTRACTUAL             [BYCON]
      *
      *    BY-DEL-BUCKET
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
      *    -------------------------------------------------
               05  BY-NOW-BMO-LME          OCCURS 3.
                   07  BY-REC-CON          OCCURS 2.
                       09  BY-DEL-BUCKET   OCCURS 10.
      *                DELINQUENT UNITS:
                           11  BY-UNITS    PIC S9(7)    COMP-3.
      *                OUTSTANDING BALANCE:
                           11  BY-OUTBAL   PIC S9(9)V99 COMP-3.
      *    -------------------------------------------------
      *    NOTE:
      *          THESE FIELDS ARE BANKRUPT, NON P&L, NON REPO ACCOUNTS
      *          IF GP-DELINQ-WITH-O2 = "Y" THEN THESE FIELDS WILL
      *             INCLUDE O2 BALANCES.
      *
      *    BY-NOW-BMO-LME-BNKRPT
      *        1- NOW   (CURRENT)         [BYNOW]
      *        2- BEGINNING OF MONTH      [BYBMO]
      *        3- LAST MONTH END          [BYLME]
      *
      *    BY-REC-CON-BNKRPT
      *        1- RECENCY                 [BYREC]
      *        2- CONTRACTUAL             [BYCON]
      *
      *    BY-DEL-BUCKET-BNKRPT
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
      *    -------------------------------------------------
               05  BY-NOW-BMO-LME-BNKRPT          OCCURS 3.
                   07  BY-REC-CON-BNKRPT          OCCURS 2.
                       09  BY-DEL-BUCKET-BNKRPT   OCCURS 10.
      *                DELINQUENT UNITS:
                           11  BY-UNITS-BNKRPT    PIC S9(7)    COMP-3.
      *                OUTSTANDING BALANCE:
                           11  BY-OUTBAL-BNKRPT   PIC S9(9)V99 COMP-3.
      *    -------------------------------------------------
               05  BY-SUMWGTTM                PIC S9(8)    COMP-3.
               05  BY-SUMWGTAV                PIC S9(9)V99 COMP-3.
               05  BY-SUMWGTTA                PIC S9(11)V99 COMP-3.
               05  BY-IB-ACCRUED              PIC S9(7)V99 COMP-3.

      *-------------------- APPLICATIONS -------------------------------
      *    PERIOD TABLE:  ____________________________________________________
      *                   1 - CURRENT        [BYCUR] |  1 - MTD        [BYMTD]
      *                   2 - END LAST MONTH [BYELM] |  2 - YTD        [BYYTD]
      *                   3 - END LAST YEAR  [BYELY] |  3 - LYR        [BYLYR]
      *                                              |
      *    ACTIVITY TBL:  1 - NET GAIN/LOSS  [BYNET] |  2 - NEW VOLUME [BYVOL]
      *                   ____________________________________________________
      *    TYPE TABLE  :  1 - NB                7 - RN
      *                   2 - RF                8 - SC
      *                   3 - FB                9 - OT
      *                   4 - PB               10 - N/A
      *                   5 - PB DELINQ        11 - N/A
      *                   6 - PB INCR
      *
      *    NOTE: STATISTICS FOR:
      *          APPS MADE IS THE ACTUAL LOAN VOLUME CARRIED UNDER BY-NO
      *          APPS NOT TAKEN IS NOT EASILY MAINTAINED
      *
      *-----------------------------------------------------------------
               05  BY-APP-PERIOD-TBL              OCCURS 3.
                   07  BY-APP-ACTIVITY-TBL        OCCURS 2.
                       09  BY-APP-TYPE-TBL        OCCURS 11.
                           11  BY-NO-PENDING      PIC S9(5)    COMP-3.
                           11  BY-NO-COND-APP     PIC S9(5)    COMP-3.
                           11  BY-NO-APPROVED     PIC S9(5)    COMP-3.
                           11  BY-NO-MADE         PIC S9(5)    COMP-3.
                           11  BY-NO-TURNDOWN     PIC S9(5)    COMP-3.
                           11  BY-NO-TURNUSDOWN   PIC S9(5)    COMP-3.

      *-----------------------------------------------------------------
      *    DLDISC-VOL-TBL: 1 - MTD                  NEW VOLUMES
      *                    2 - YTD
      *                    3 - LYR
      *
      *-----------------------------------------------------------------
               05  BY-DLDISC-VOL-TBL              OCCURS 3.
                   07 BY-DLDISC                   PIC S9(9)V99 COMP-3.

      *-----------------------------------------------------------------
               05  BY-STD-POS                     PIC S9(6)    COMP-3.
               05  BY-30-DAY-MOVED                PIC S9(6)    COMP-3.
               05  BY-PROJ-INCREASE               PIC S9(6)    COMP-3.
               05  BY-PROJ-PROFIT                 PIC S9(6)    COMP-3.
      *    ------------------------------------------------------
      *    UNEARNED PERIOD TABLE:  ___________________________________________
      *                   1 - CURRENT                |  1 - MTD
      *                   2 - END LAST MONTH         |  2 - YTD
      *                   3 - END LAST YEAR          |  3 - LYR
      *                                              |
      *    UNEARNED ACT:  1 - NET GAIN/LOSS          |  2 - NEW VOLUME
      *                   ____________________________________________________
      *    UNEARNED TABLE: 1 - INTEREST
      *                    2 - ADDON
      *                    3 - SERVICE CHARGE
      *                    4 - MAINTENANCE FEES
      *                    5 - DEALER DISCOUNT
      *                    6 - OTHER
      *                 7-10 - FUTURE USE
      *    ------------------------------------------------------
               05  BY-UNEARNED-PERIOD             OCCURS 3.
                   07  BY-UNEARNED-ACTIVITY       OCCURS 2.
                       09  BY-UNEARNED-TYPE       OCCURS 10.
                           11  BY-UNEARNED-AMT    PIC S9(8)V99 COMP-3.

      *-----------------------------------------------------------------
      *    BY-PROCEEDS     1 - MTD                NEW VOLUMES
      *                    2 - YTD
      *-----------------------------------------------------------------
               05  BY-PROCEEDS      OCCURS 2      PIC S9(8)V99 COMP-3.
               05  BY-LOCKBOX       OCCURS 2      PIC S9(8)V99 COMP-3.
      *-----------------------------------------------------------------
               05  FILLER                         PIC X(881).
