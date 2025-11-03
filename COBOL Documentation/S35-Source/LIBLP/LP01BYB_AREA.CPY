      * COPYMEMBER: LIBLP/LP01BYB_AREA
      ******************************************************************
      *
      * DESCRIPTION: BRANCH STATISTICS FILE - B (SECONDARY) AREA
      *
      * COPY USAGE : LIBLP/LP01BY.CPY [WITHIN LIBLP/LPWSBY.CPY]
      *              LIBLP/LP01BYB.CPY
      *
      * FILE KEY   : #1 = DATE-CCYYMM, BRNO, CLASS
      *
      * FILE PATH  : /USR/DATA/R#/LP/BYBFILE
      *      SIZE  : BYA-AREA-A - 3389
      *      SIZE  : BYB-AREA-B - 3798
      *      SIZE  : BY-REC     - 7500
      *
      * ***ALL*** REVISION NOTES FOR FIELDS ARE LOCATED IN LP01BY.CPY
      *
      *=================================================================
      * REV:
      * KEC 190205 FOR A32, THE BYFILE WAS SPLIT INTO TWO FILES:
      *              BYAFILE (PRIMARY) AND BYBFILE (SECONDARY).
      *            LP01BY.CPY IS USED AS A MASTER LAYOUT (ALL LAYOUTS
      *            MERGED INTO ONE).
      *            LP01BYA/B.CPY IS FOR THE PRIMARY KEY FIELDS.
      *            LP01BYA/B_AREA.CPY IS FOR NON-KEY FIELDS.
      *
      * KEC 190918 PT#0181 DUE TO ACU4GL THINKING THE BYA-REC IS ABOVE
      *            SQL LIMITS (1024 COLUMNS, 8064 BYTE ROWS), I MOVED
      *            THE FUTURE AMOUNT FIELDS FROM THE BYA TO THE BYB TO
      *            LOWER THE LIMITS SO THAT ACU4GL ACCEPTS IT.
      *              WAS: BYA-SIZE   VALUE 4466.
      *              NOW: BYA-SIZE   VALUE 3702.
      * BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * AMM 20210429 REMOVED FUTURE FIELDS, RESIZED FILE
      * BAH 20230614 ADDED FILLER
      ******************************************************************
           03  BYB-AREA-B.

      *    -------------------------------------------------
      *    SUBSCRIPTED FOR MTD AND YTD FIGURE
      *        1- MTD                     [BYMTD]
      *        2- YTD                     [BYYTD]
      *
      *    NOTE: BYB-PL-PRINCOL INCLUDES BYB-PL-O2COL
      *    -------------------------------------------------
               05  BYB-MTD-YTD                 OCCURS 2.

      *      -----------------------------------------------
      *                BYB-PL-RECOVERED    1 - PL RECOVERIES
      *                                   2 - P2 RECOVERIES
      *                                   3 - P3 RECOVERIES
      *                                   4 - P4 RECOVERIES
      *                                   5 - P5 RECOVERIES
      *      -----------------------------------------------
                   07  BYB-PL-RECOVERED OCCURS 5.
                       09  BYB-PL-PRINCOL      PIC S9(9)V99 COMP-3.
                       09  BYB-PL-INTCOL       PIC S9(9)V99 COMP-3.
                       09  BYB-PL-LATECOL      PIC S9(9)V99 COMP-3.
                       09  BYB-PL-O2COL        PIC S9(9)V99 COMP-3.

      *----------------------------------------------------------------
      * SOLD  : LOANS SOLD/TRANSFERRED VIA BULKTR.C
      * TRANS : TRANSFERRED TO A GIVEN BRANCH VIA BULKTR.C
      * PURCH : MIDSTREAM PURCHASE FROM OUTSIDE SOURCE.
      *----------------------------------------------------------------
                   07  BYB-AMT-SOLD            PIC S9(9)V99 COMP-3.
                   07  BYB-AMT-TRANS           PIC S9(9)V99 COMP-3.
                   07  BYB-AMT-PURCH           PIC S9(9)V99 COMP-3.

                   07  BYB-NO-SOLD             PIC S9(7)    COMP-3.
                   07  BYB-NO-TRANS            PIC S9(7)    COMP-3.
                   07  BYB-NO-PURCH            PIC S9(7)    COMP-3.

                   07  BYB-OTHCOL              PIC S9(9)V99 COMP-3.
                   07  BYB-OT2COL              PIC S9(9)V99 COMP-3.
                   07  BYB-INTCOL              PIC S9(9)V99 COMP-3.
                   07  BYB-LATECOL             PIC S9(9)V99 COMP-3.
                   07  BYB-PRINCOL             PIC S9(9)V99 COMP-3.
                   07  BYB-DEFCOL              PIC S9(9)V99 COMP-3.
                   07  BYB-RNPRIN              PIC S9(9)V99 COMP-3.
                   07  BYB-INTREB              PIC S9(9)V99 COMP-3.
                   07  BYB-CLREB1              PIC S9(9)V99 COMP-3.
                   07  BYB-CLREB2              PIC S9(9)V99 COMP-3.
                   07  BYB-CLREB3              PIC S9(9)V99 COMP-3.
                   07  BYB-CLREB4              PIC S9(9)V99 COMP-3.
                   07  BYB-AHREB               PIC S9(9)V99 COMP-3.
                   07  BYB-PPREB               PIC S9(9)V99 COMP-3.
                   07  BYB-O1REB               PIC S9(9)V99 COMP-3.
                   07  BYB-O2REB               PIC S9(9)V99 COMP-3.
                   07  BYB-O3REB               PIC S9(9)V99 COMP-3.
                   07  BYB-O4REB               PIC S9(9)V99 COMP-3.
                   07  BYB-O5REB               PIC S9(9)V99 COMP-3.
                   07  BYB-O6REB               PIC S9(9)V99 COMP-3.
                   07  BYB-O7REB               PIC S9(9)V99 COMP-3.
                   07  BYB-SCREB               PIC S9(9)V99 COMP-3.
                   07  BYB-MFREB               PIC S9(9)V99 COMP-3.
                   07  BYB-DFREB               PIC S9(9)V99 COMP-3.
                   07  BYB-UN7-REB             PIC S9(9)V99 COMP-3.
                   07  BYB-UN8-REB             PIC S9(9)V99 COMP-3.
                   07  BYB-UN9-REB             PIC S9(9)V99 COMP-3.
                   07  BYB-UN10-REB            PIC S9(9)V99 COMP-3.

                   07  BYB-PART-CHGOFF         PIC S9(9)V99 COMP-3.
                   07  BYB-ALLOTMENTS          PIC S9(9)V99 COMP-3.
                   07  BYB-INS-TYPE            OCCURS 10.
                       09  BYB-INS-NO-ELIG     PIC S9(7)    COMP-3.
                       09  BYB-INS-NO-WROTE    PIC S9(7)    COMP-3.
                       09  BYB-INS-NO-NOTWROTE PIC S9(7)    COMP-3.
                       09  BYB-INS-AMT-WROTE   PIC S9(9)V99 COMP-3.

      * 2/18/16 - NOTE, OCCURANCE 1 OF THIS TABLE WILL BE NON-FILE
      * 2/18/16 - NOTE, OCCURANCE 2 OF THIS TABLE WILL BE AD&D
      * 2/18/16 - NOTE, OCCURANCE 3 OF THIS TABLE WILL BE ROADGARD
                   07  BYB-FEE-TYPE            OCCURS 3.
                       09  BYB-FEE-NO-ELIG     PIC S9(7)    COMP-3.
                       09  BYB-FEE-NO-WROTE    PIC S9(7)    COMP-3.
                       09  BYB-FEE-NO-NOTWROTE PIC S9(7)    COMP-3.
                       09  BYB-FEE-AMT-WROTE   PIC S9(9)V99 COMP-3.

                   07  BYB-OTH-NO-COL          PIC S9(7)    COMP-3.
                   07  BYB-OT2-NO-COL          PIC S9(7)    COMP-3.
                   07  BYB-INT-NO-COL          PIC S9(7)    COMP-3.
                   07  BYB-LATE-NO-COL         PIC S9(7)    COMP-3.

                   07  BYB-OTH-NO-WAIVED       PIC S9(7)    COMP-3.
                   07  BYB-OT2-NO-WAIVED       PIC S9(7)    COMP-3.
                   07  BYB-INT-NO-WAIVED       PIC S9(7)    COMP-3.
                   07  BYB-LATE-NO-WAIVED      PIC S9(7)    COMP-3.

                   07  BYB-OTH-AMT-WAIVED      PIC S9(9)V99 COMP-3.
                   07  BYB-OT2-AMT-WAIVED      PIC S9(9)V99 COMP-3.
                   07  BYB-INT-AMT-WAIVED      PIC S9(9)V99 COMP-3.
                   07  BYB-LATE-AMT-WAIVED     PIC S9(9)V99 COMP-3.
      *    -------------------------------------------------
      *    NOTE:
      *          THESE FIELDS ARE NON P&L, NON REPO ACCOUNTS
      *          IF GP-DELINQ-WITH-O2 = "Y" THEN THESE FIELDS WILL
      *             INCLUDE O2 BALANCES.
      *
      *    BYB-NOW-BMO-LME
      *        1- NOW   (CURRENT)         [BYNOW]
      *        2- BEGINNING OF MONTH      [BYBMO]
      *        3- LAST MONTH END          [BYLME]
      *
      *    BYB-REC-CON
      *        1- RECENCY                 [BYREC]
      *        2- CONTRACTUAL             [BYCON]
      *
      *    BYB-DEL-BUCKET
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
               05  BYB-NOW-BMO-LME          OCCURS 3.
                   07  BYB-REC-CON          OCCURS 2.
                       09  BYB-DEL-BUCKET   OCCURS 10.
      *                DELINQUENT UNITS:
                           11  BYB-UNITS    PIC S9(7)    COMP-3.
      *                OUTSTANDING BALANCE:
                           11  BYB-OUTBAL   PIC S9(9)V99 COMP-3.
      *    -------------------------------------------------
      *    NOTE:
      *          THESE FIELDS ARE BANKRUPT, NON P&L, NON REPO ACCOUNTS
      *          IF GP-DELINQ-WITH-O2 = "Y" THEN THESE FIELDS WILL
      *             INCLUDE O2 BALANCES.
      *
      *    BYB-NOW-BMO-LME-BNKRPT
      *        1- NOW   (CURRENT)         [BYNOW]
      *        2- BEGINNING OF MONTH      [BYBMO]
      *        3- LAST MONTH END          [BYLME]
      *
      *    BYB-REC-CON-BNKRPT
      *        1- RECENCY                 [BYREC]
      *        2- CONTRACTUAL             [BYCON]
      *
      *    BYB-DEL-BUCKET-BNKRPT
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
               05  BYB-NOW-BMO-LME-BNKRPT          OCCURS 3.
                   07  BYB-REC-CON-BNKRPT          OCCURS 2.
                       09  BYB-DEL-BUCKET-BNKRPT   OCCURS 10.
      *                DELINQUENT UNITS:
                           11  BYB-UNITS-BNKRPT    PIC S9(7)    COMP-3.
      *                OUTSTANDING BALANCE:
                           11  BYB-OUTBAL-BNKRPT   PIC S9(9)V99 COMP-3.
      *    -------------------------------------------------
               05  BYB-SUMWGTTM                PIC S9(8)    COMP-3.
               05  BYB-SUMWGTAV                PIC S9(9)V99 COMP-3.
               05  BYB-SUMWGTTA                PIC S9(11)V99 COMP-3.
               05  BYB-IB-ACCRUED              PIC S9(7)V99 COMP-3.

      *-----------------------------------------------------------------
      *    DLDISC-VOL-TBL: 1 - MTD                  NEW VOLUMES
      *                    2 - YTD
      *                    3 - LYR
      *
      *-----------------------------------------------------------------
               05  BYB-DLDISC-VOL-TBL              OCCURS 3.
                   07 BYB-DLDISC                   PIC S9(9)V99 COMP-3.

      *-----------------------------------------------------------------
               05  BYB-STD-POS                     PIC S9(7)    COMP-3.
               05  BYB-30-DAY-MOVED                PIC S9(7)    COMP-3.
               05  BYB-PROJ-INCREASE               PIC S9(7)    COMP-3.
               05  BYB-PROJ-PROFIT                 PIC S9(7)    COMP-3.

      *-----------------------------------------------------------------
      *    BYB-PROCEEDS    1 - MTD                NEW VOLUMES
      *                    2 - YTD
      *-----------------------------------------------------------------
               05  BYB-PROCEEDS      OCCURS 2      PIC S9(9)V99 COMP-3.
               05  BYB-LOCKBOX       OCCURS 2      PIC S9(9)V99 COMP-3.


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
               05  BYB-UNEARNED-PERIOD             OCCURS 3.
                   07  BYB-UNEARNED-ACTIVITY       OCCURS 2.
                       09  BYB-UNEARNED-TYPE       OCCURS 10.
                           11  BYB-UNEARNED-AMT    PIC S9(9)V99 COMP-3.

               05  FILLER                          PIC X(917).
