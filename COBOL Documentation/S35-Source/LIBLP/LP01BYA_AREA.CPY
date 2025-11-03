      * COPYMEMBER: LIBLP/LP01BYA_AREA
      ******************************************************************
      *
      * DESCRIPTION: BRANCH STATISTICS FILE - A (PRIMARY) AREA
      *
      * COPY USAGE : LIBLP/LP01BY.CPY [WITHIN LIBLP/LPWSBY.CPY]
      *              LIBLP/LP01BYA.CPY
      *
      * FILE KEY   : #1 = DATE-CCYYMM, BRNO, CLASS
      *
      * FILE PATH  : /USR/DATA/R#/LP/BYAFILE  
      *      SIZE  : BYA-AREA-A - 3689
      *      SIZE  : BYB-AREA-B - 3798
      *      SIZE  : BY1-KEY    -   13
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
      *  CS 190425 ADD FUTURE FIELDS   
      *
      * KEC 190918 PT#0181 DUE TO ACU4GL THINKING THE BYA-REC IS ABOVE
      *            SQL LIMITS (1024 COLUMNS, 8064 BYTE ROWS), I MOVED
      *            THE FUTURE AMOUNT FIELDS FROM THE BYA TO THE BYB TO
      *            LOWER THE LIMITS SO THAT ACU4GL ACCEPTS IT.
      *              WAS: BYA-SIZE   VALUE 4466.
      *              NOW: BYA-SIZE   VALUE 3702.
      * BAH 2021.0910 LTOUCH-DATE HAD NO XFD!
      ******************************************************************
           03  BYA-AREA-A.
      $XFD DATE=YYYYMMDD
               05  BYA-LTOUCH-DATE          PIC 9(08)    COMP-3.

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
      *            NOTE: BYA-AMT INCLUDES BYA-O2
      *    ------------------------------------------------------
               05  BYA-PERIOD-TBL              OCCURS 3.
                   07  BYA-ACTIVITY-TBL        OCCURS 2.
                       09  BYA-TYPE-TBL        OCCURS 11.
                           11  BYA-NO          PIC S9(7)    COMP-3.
                           11  BYA-AMT         PIC S9(9)V99 COMP-3.
                           11  BYA-O2          PIC S9(9)V99 COMP-3.
                           11  BYA-INT         PIC S9(9)V99 COMP-3.
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
      *            NOTE: BYA-AMT-REPO INCLUDES BYA-O2-REPO
      *----------------------------------------------------------------------
               05  BYA-REPO-PERIOD-TBL         OCCURS 3.
                   07  BYA-REPO-ACTIVITY-TBL   OCCURS 2.
                       09  BYA-REPO-TYPE-TBL   OCCURS 5.
                           11  BYA-NO-REPO     PIC S9(7)    COMP-3.
                           11  BYA-AMT-REPO    PIC S9(9)V99 COMP-3.
                           11  BYA-O2-REPO     PIC S9(9)V99 COMP-3.
                           11  BYA-INT-REPO    PIC S9(9)V99 COMP-3.

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
      *            NOTE: BYA-AMT-PL INCLUDES BYA-O2-PL
      *----------------------------------------------------------------------
               05  BYA-PL-PERIOD-TBL              OCCURS 3.
                   07  BYA-PL-ACTIVITY-TBL        OCCURS 2.
                       09  BYA-PL-TYPE-TBL        OCCURS 10.
                           11  BYA-NO-PL          PIC S9(7)    COMP-3.
                           11  BYA-AMT-PL         PIC S9(9)V99 COMP-3.
                           11  BYA-O2-PL          PIC S9(9)V99 COMP-3.
                           11  BYA-INT-PL         PIC S9(9)V99 COMP-3.

      *----------------------------------------------------------------------
      * DLR PERIOD TABLE: ----------------------------------------------------
      *                   1 - CURRENT        [BYCUR] | 1 - MTD        [BYMTD]
      *                   2 - END LAST MONTH [BYELM] | 2 - YTD        [BYYTD]
      *                   3 - END LAST YEAR  [BYELY] | 3 - LYR        [BYLYR]
      *                                              |
      * DLR ACTIVITY TBL: 1 - NET GAIN/LOSS [BYNET]  | 2 - NEW VOLUME [BYVOL]
      *
      *----------------------------------------------------------------------
               05  BYA-DLR-PERIOD-TBL              OCCURS 3.
                   07  BYA-DLR-ACTIVITY-TBL        OCCURS 2.
                       09  BYA-DLRVPRIN   OCCURS 3
                                              PIC S9(9)V99 COMP-3.
                       09  BYA-DLRVCHGS   OCCURS 3
                                              PIC S9(9)V99 COMP-3.
                       09  BYA-DLPARVHELD     PIC S9(9)V99 COMP-3.

