       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BYRSUP.
      *****************************************************
      *  DESC: UPDATE STATISTICS TO RATING SHEET FILE:
      *        /USR/DATA/R1/SP/WRFILE
      *
      * !!!!!!!!!!!! NOTE !!!!!!!!!!!!!!!!!!
      * 9/24/03 - HEY, I AM TEMPORARILIY CHANGING THIS PROGRAM TO UPDATE THE
      *           OUTSTANDING RECEIVABLES...(RATING SHEET LINE 3).
      *           THIS AMOUNT SHOULD COME FROM THE GL UPDATE, BUT WE DON'T
      *           REALLY HAVE VALID GL NUMBERS YET, SO FOR NOW WE CAN GET THEM
      *           FROM HERE; WE REALLY NEED THE AR BALANCE TO TEST 2 OF THE
      *           JOURNAL ENTRY TYPES!!
      *                                   BARB
      *
      * WORLD INS (AS OF 1/05/2022):
      *        IUI = O1 (4) = "KY", "MO", "TX", "IN", "NM", "UT", "ID", "AL"
      *        IUI = O3 (6) = "TN", "SC" AND "OK"
      *        IUI = ANYBODY ELSE, ZERO TO IUI
      *
      *        AUTO = O1 (4) = "TN", "OK" AND "MS"
      *        AUTO = O2 (5) = "SC"
      *        AUTO = EVERYBODY ELSE, AUTO IS O2 + O3
      *
      * STARTED WITH SMITHS AS OF 7/14/03
      * WILL WRITE OR REWRITE DATED WRFILE RECORD
      *
      * *****NOTE AS TO THE FLOW OF THIS PROGRAM*****
      * PROGRAM DRIVES OFF THE BRANCH FILE, READING BY'S.
      * THERE ARE ROUTINES TO COMBINE CLASSES, SO FOR ANY GIVEN
      * BRANCH, THERE JUST 2 PASSES THROUGH THE UPDATE PORTION,
      * ONE FOR CLASS 1 (COMBINED WITH 2, 5, 8, 20, 3, 21) AND
      * ONE FOR WCBC (30 & ANY CLASS > 30).
      *
      * FLOW:
      * READ BR
      * READ BY, SKIP IF NOT CLASS 1 OR 30
      * ZERO OUT HOLD FIELDS
      * IF CLASS 1, PERFORM COMBINE-CLASSES:
      *    MOVE BY-REC TO WK-REC
      *    PERFORM ADD-CLASSES:
      *       ADD BY FIELDS TO WK-FIELDS
      *       ADD TO LARGE LOAN TOTALS IF LARGE LOAN FLAG "Y"
      *       GET CUR MONTH BY FOR INS (ADDS TO HOLD-MTD INS STUFF)
      *    MOVE WK-REC TO BY-REC
      *    GET CUR MONTH BY FOR INS (ADDS TO HOLD-MTD INS STUFF)
      * IF CLASS 30, PERFORM COMBINE-SF-CLASSES (SAME KIND OF STUFF)
      * MOVE BY-REC TO WK-REC
      * GET CUR MONTH BY FOR INS (ADDS TO HOLD-MTD INS FOR CLASS 1)
      * MOVE WK-REC TO BY-REC
      * CREATE RATING SHEET RECORD
      *
      * SO....WHEN YOU ARE ADDING SOMETHING & AREN'T SURE WHERE
      * TO PUT IT, THE INS STUFF HAD TO GET ADDED IN THE HOLD FIELDS
      * IN WORKING STORAGE BECAUSE IT CAME FROM THE CURRENT MONTH &
      * ALL THE OTHER STUFF IS COMBINED IN BY-REC WHICH IS THE NEW
      * MONTH.  SO ANYTHING LIKE A MTD STAT NEEDS TO GET ADDED THERE.
      *
      * NOTES ABOUT INSURANCE:
      * USED TO GET # OF PREMS FROM SMITH STATS & AMTS FROM GL;
      * NOW WILL GET # & AMT FROM HERE AS WELL AS AMT OF REFUND.
      * THERE IS NO INSURANCE WRITTEN ON WCBC LOANS, SO HAVE TO
      * BE CAREFUL TO ONLY ADD/MOVE INSURANCE STUFF ON THE PASS
      * WHEN CLASS < 30
      *
      * WE ARE UPDATING:
      * RATING SHEET LINE  1: # LOANS OUTSTANDING (ELM, GAIN-LOSS, 1-11)
      * RATING SHEET LINE  5: MTD/YTD # NEW LOANS MADE
      * RATING SHEET LINE  6: MTD/YTD # FB LOANS MADE
      * RATING SHEET LINE  7: MTD/YTD # PB LOANS MADE
      * RATING SHEET LINE  9: MTD/YTD $ OF LOAN VOLUME MADE
      * RATING SHEET LINE 34: MTD/YTD # OF PL CHARGE-OFFS MADE
      * RATING SHEET LINE 45: # 30 DAY REC DELINQ ACCTS
      * RATING SHEET LINE 46: $ 30 DAY REC DELINQ ACCTS
      * RATING SHEET LINE 48: # 60 DAY REC DELINQ ACCTS
      * RATING SHEET LINE 49: $ 60 DAY REC DELINQ ACCTS
      * RATING SHEET LINE 51: # 90+ DAY REC DELINQ ACCTS
      * RATING SHEET LINE 52: $ 90+ DAY REC DELINQ ACCTS
      *
      * REV:
      * BLV 031021 ADDED WCBC FIELDS, LARGE LOAN FIELDS
      * BLV 031022 ADDED ELM & ELY FOR NO OF CUST & OUTSTANDING BALANCE
      *            FOR REGULAR & WCBC.  ADDED LOGIC TO READ PRIOR
      *            MONTH RECORD TO GET ELM & ELY VALUES & SOME
      *            YEAR TO DATE FIELDS
      * BLV 031125 USE NEW COPY REPLACING TO PUT WK-REC COPY OF BY-REC
      *            IN WORKING STORAGE
      * BLV 031210 COMPUTE YTD PERCENTS; THESE WILL BE ACCUMULATED, SO
      *            MUST COMPUTE MTD PERCENT & ADD TO LAST MONTH'S TO
      *            GET CURRENT YTD PERCENT
      * BLV 031222 DO UPDATE AR BALANCES; THESE ARE DOCUMENTATED AS COMING
      *            FROM GL AND WILL BE UPDATED IN GLRSUP, BUT WILL ALSO
      *            UPDATE HERE; MAY BE USED FOR CERTAIN WEIGHTED JOURNAL
      *            ENTRIES, OKAY TO UPDATE HERE PER JIM
      * BLV 040109 IF PROCESS-MM IS FIRST MONTH OF FISCAL, MOVE MTD
      *            PERCENT TURNOVER TO YTD PERCENT
      * BLV 040116 UPDATE MTD & YTD WR-NO-NON-FILE-PREMS, NOW WE WILL
      *            UPDATE NON-FILE TO BY INS TABLE OCCURRENCE 8
      * BLV 040116 FIX UPDATE OF YTD INSURANCE FIELDS IN WRFILE TO COMPUTE
      *            AS LAST MONTH YTD + CURRENT MTD (INSTEAD OF JUST ADDDING
      *            TO YTD, SO YOU CAN RE-RUN SAME MONTH WITHOUT MESSING UP
      *            YTD!).
      * BLV 040116 REMOVE DETAILED LOG BY BRANCH, JUST KEEP START & FINISH
      *            TIMES; GET SELECTED GROUP & DATE INTO LOGS
      * BLV 040204 FIX # PREMS, AMT PREMS, AMT REFUNDS UPDATING TO WR
      * BLV 040212 ADD UPDATE OF LARGE LOAN FIELDS (WAS ONLY UPDATING
      *            OUTSTANDING AND 90+ FOR LARGE LOANS)
      * BLV 040315 ADD UPDATE OF 1-YR-AGO FIELDS FOR SUPERVISOR BONUS
      *            REPORT
      * BLV 040330 ADD UPDATE OF WR-CUR-NO-CON-SLOW
      *                          WR-WCBC-CUR-NO-CON-SLOW
      *                          WR-LARGE-CUR-NO-CON-SLOW
      *             UPDATE WITH BEG MO RECENCY (WHICH IS SAME AS CONTRACTUAL)
      * GLB 040406  BUG; REJECT ZERO DATE
      * BLV 040429  FIX CLEARING YTD ON WR-WCBC-PCT-VOLUME
      * BLV 040514  FIXED "OK" TO HAVE O2 FOR AUTO (INSTEAD OF O2 + O3) -
      *             THEY DON'T DO O2 NOW, BUT THEY USE O3 FOR IUI SO O3
      *             WAS ENDING UP IN AUTO & IUI IN RATING SHEET
      * BLV 040514  FIXED MISSING PERIOD WHICH CAUSED GET-CUR-MO-BY-FOR-INS
      *             TO NOT BE PERFORMED FOR CLASS 3
      * BLV 040622  FIX GROUP-SUB IN TOTAL-LEVEL-CHECK
      * BLF 041118  CHANGE TO UPDATE WR-LOTS-NON-FILE-PREMS AND
      *                              WR-LOTS-NO-NON-FILE-PREMS
      * BLF 041213  CHANGED COMPUTATION OF YTD NON-FILE PREMIUMS, DON'T
      *             TAKE YTD FROM STATS WHICH INCLUDES ASSURANT, COMPUTE YTD
      *             NON-FILE PREMS = LAST MTD NON-FILE PREMS & CURRENT MTD
      * BLF 061115  EXPANDED ALL GROUP SUBSCRIPTS TO ACCOMODATE
      *             EXPANDED GROUP SEARCH TABLES, WORLD PR# 502
      * BLM 090112  FIX FOR MEXICO - CHANGED REFERENCES TO GP FISCAL DATES
      *             TO HARD-CODED: 3 FOR ENDING MONTH OF LAST YEAR, 4 FOR
      *             BEGINNING MONTH FOR YTD PERCENTS, WORLD PR# 600
      * BLM 110323  ADDED CHECK ON MONTH ENTERED FOR DATE, LET JOSH UPDATE
      *             FOR 00/11, WORLD
      * BLM 110519  IF STATE IS TX, ADD OTHER-1 INSURANCE TO UNEMPLOYMENT
      *             IN WRFILE, WORLD PR# 753
      * BLM 150527  IF STATE IS "OK" OR "MS", O1 IS AUTO INSURANCE, REMOVE
      *             "OK" FROM USING O2 AS AUTO INSURANCE, WORLD PR# 1085
      * BLM 160315  FIX FOR NEW BY-DATE FORMAT, PD0003
      * KEC 2016.0929 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED BY-DATE TO BY-DATE-CCYYMM FOR CONSISTENCY.
      * BLM 2018.0227 FIX DATE SETUP, NEXT MO & 1 YEAR AGO DATES WRONG
      * BLM 2018.0305 FIX FOR CHANGES THAT STORE NON-FILE, AD&D AND
      *               ROADGARD IN BY-FEE TABLE INSTEAD OF BY-INS TABLE
      * BLM 2018.0307 NEXT-MO-BY-DATE STILL WRONG, FIXED
      *  CS 181129  TAX LOANS WILL BE CLASS 50, BUT ROLL INTO LOAN
      *             DESIGNATION FOR RATING SHEET/GL.  THERE IS A LOT OF
      *             HARD-CODING OF CLASSES IN THIS PROGRAM THAT CANT BE
      *             AVOIDED. THE LARGE-LOAN-FLAG IS BEING USED FOR 2
      *             DIFFERENT PURPOSES WHICH CAUSED PROBLEM. FOR EXAMPLE,
      *             CLASS 5 WAS 'Y', AND THEN NEXT CLASS READ WAS 'N'
      *             (COMBINE-CLASSES) , SO
      *             WHEN GOT TO CREATE-RATING SHEET IT DIDNT MOVE THE TOT-
      *             LARGE-OUTS TO THE RATING SHEET. SO I FORCED A 'Y' AT THE
      *             BEGINNING OF THE CREATE-RATING-SHEET TO THIS FLAG,
      *             THIS DIDNT WORK  EITHER BECAUSE IN THE COMBINE-CLASSES,
      *             THE LARGE-LOAN-FLAG FOR 50 WAS "N' SO TOT-LARGE-OUTS
      *             NEVER GOT POPULATED AND THE VALUE WAS 0 SO THE FORCE OF
      *             "Y" IN THE CREATE MOVED 0 TO THE LARGE LOAN FIELDS
      *             IN THE RATING SHEET.
      *             SO THIS LAST CHANGE APPEARS TO WORK, WHICH IS FORCE 'Y'
      *             TO LARGE-LOAN-FLAG IN CREATE-RATING SHEET UNLESS
      *             CLASS 30 OR 40 WHICH ARE CONSIDERED SALES(WCBC).
      *                                             WORLD PR#1350
      * KEC 190212 A32 F2SL - REVISED FOR SPLITTING THE BYFILE.
      *            REMOVED LOAN APP FIELDS IN BY & TY.
      *  CS 20200528 ADDED CLASSES 4,22 TO SSMALL LOAN, CLASS 6 IS A NEW
      *              CLASS OF LARGE LOAN, INDIANA IB CLASSES PR#1434 WORLD
      * CS 20200611 CLASS 22 WAS GOING INTO LARGE LOAN VOLUME, OPEN CUSTOMER
      *             NB,FB,PB. CHANGED THIS IF CONDITION, YES ITS UGLY BUT THIS
      *             HAS CLASS TESTS ALL OVER #1434A
      *             IF BY-CLASS > 4 AND BY-CLASS < 30
      *         NOW:IF BY-CLASS > 4 AND BY-CLASS < 30 AND BY-CLASS NOT = 22
      * BAH 20220105 ADD O1 TO IUI FOR "IN","NM","UT" AND "ID" #1532
      * BAH 20230221 ADD O1 TO IUI FOR "AL", #1592
      *******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/BYRSUP.CBL S35 03/25/24-15:40:17 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BY.CPY".
       COPY "LIBLP/LP01BYA.CPY".
       COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LP01BY_SQL.CPY".
       COPY "LIBLP/LPWSBY.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBSP/SP01WR.CPY".
       COPY "LIBSP/SP01WR_SQL.CPY".
       COPY "LIBSP/SPWSWR.CPY".


      * LAST MONTH RATING SHEET RECORD
       COPY "LIBSP/SP01WR.CPY" REPLACING LEADING "WR" BY "WRLM".
       COPY "LIBSP/SP01WR.CPY" REPLACING LEADING "WR" BY "WRLY".

       01  ST1-AMOUNT              PIC 9(7)V99.
       01  ST2-PERSLOW             PIC 9(3)V99.
       01  ST4-AMOUNT              PIC 9(10).

       01  BYSUB1          PIC 9(2)    VALUE 0.
       01  BYSUB2          PIC 9(2)    VALUE 0.
       01  BYSUB3          PIC 9(2)    VALUE 0.
       01  PERIOD          PIC 9(6) COMP VALUE 1.
       01  ACTSUB          PIC 9(6) COMP VALUE 1.
       01  TYPSUB          PIC 9(6) COMP VALUE 1.
       01  INSUR           PIC 9(6) COMP VALUE 1.
       01  PLSUB           PIC 9(6) COMP VALUE 1.
       01  MTDYTD          PIC 9(6) COMP VALUE 1.

      *----------------------------------------------------------------
      *  THIS IS AN EXACT DUPLICATE OF BY-REC, USED TO COMBINE
      *  MULTIPLE CLASSES IN GA & NM INTO 1 STAT RECORD
      *  WK-REC FROM 'REPLACING LEADING' ON BY-REC.
      *----------------------------------------------------------------
      *COPY "LIBLP/LP01BY.CPY" REPLACING LEADING "BY" BY "WK".
       01  WK-REC.
           03  WK1-KEY.
               05  WK-DATE-CCYYMM      PIC 9(6).
               05  WK-BRNO             PIC 9(4).
               05  WK-CLASS            PIC 9(3).
       COPY "LIBLP/LP01BYA_AREA.CPY" REPLACING LEADING
                                         "BYA-" BY "WK-".
       COPY "LIBLP/LP01BYB_AREA.CPY" REPLACING LEADING
                                         "BYB-" BY "WK-".

      *=================================================================
       01  TOTAL-LEV-TABLE.
           03  TOTAL-LEVEL-TABLE  PIC X OCCURS 10.

      * LEVEL 1 = COMPANY TOTALS
      *       2 = STATE TOTALS
      *       3 = SUPERVISOR TOTALS
       01  TOTAL-TABLE.
           03  TOT-TABLE        OCCURS 10 TIMES.
               05  TOTAL-UNITS-1   PIC S9(9)V99.
               05  TOTAL-UNITS-2   PIC S9(9)V99.
       01  TOTAL-GIFT-TABLE.
           03  TOT-GIFT-TABLE        OCCURS 10 TIMES.
               05  TOTAL-GIFT-UNITS-1   PIC S9(9)V99.
               05  TOTAL-GIFT-UNITS-2   PIC S9(9)V99.
      * COMPANY TOTALS
       01  TOT-WABD-115        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WABD-120        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WGBD-115        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WGBD-120        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WLBD-115        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WLBD-120        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WARS-01         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WARS-05         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WARS-06         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WARS-07         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WARS-09         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WARS-34         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WARS-45         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WARS-46         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WARS-48         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WARS-49         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WARS-51         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WARS-52         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGN-01         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGN-05         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGN-06         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGN-07         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGN-09         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGN-34         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGN-45         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGN-46         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGN-48         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGN-49         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGN-51         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGN-52         PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAIS-508        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAIS-509        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAIS-525        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAIS-526        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAIS-455        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAIS-457        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAIS-462        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAIS-469        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGI-508        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGI-509        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGI-525        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGI-526        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGI-455        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGI-457        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGI-462        PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-WAGI-469        PIC S9(9)V99 COMP-3 VALUE 0.

       01  HOLD-MTD-AH         PIC S9(5)       COMP-3 VALUE 0.
       01  HOLD-MTD-CL         PIC S9(5)       COMP-3 VALUE 0.
       01  HOLD-MTD-PP         PIC S9(5)       COMP-3 VALUE 0.
       01  HOLD-MTD-O1         PIC S9(5)       COMP-3 VALUE 0.
       01  HOLD-MTD-O2         PIC S9(5)       COMP-3 VALUE 0.
       01  HOLD-MTD-O3         PIC S9(5)       COMP-3 VALUE 0.
       01  HOLD-MTD-AD-D       PIC S9(5)       COMP-3 VALUE 0.
       01  HOLD-MTD-RDGARD     PIC S9(5)       COMP-3 VALUE 0.
       01  HOLD-MTD-NON-FILE   PIC S9(5)       COMP-3 VALUE 0.
       01  HOLD-YTD-AH         PIC S9(5)       COMP-3 VALUE 0.
       01  HOLD-YTD-CL         PIC S9(5)       COMP-3 VALUE 0.
       01  HOLD-YTD-PP         PIC S9(5)       COMP-3 VALUE 0.
       01  HOLD-YTD-O1         PIC S9(5)       COMP-3 VALUE 0.
       01  HOLD-YTD-O2         PIC S9(5)       COMP-3 VALUE 0.
       01  HOLD-YTD-O3         PIC S9(5)       COMP-3 VALUE 0.
       01  HOLD-YTD-AD-D       PIC S9(5)       COMP-3 VALUE 0.
       01  HOLD-YTD-RDGARD     PIC S9(5)       COMP-3 VALUE 0.
       01  HOLD-YTD-NON-FILE   PIC S9(5)       COMP-3 VALUE 0.
       01  HOLD-MTD-AH-AMT     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-MTD-CL-AMT     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-MTD-PP-AMT     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-MTD-O1-AMT     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-MTD-O2-AMT     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-MTD-O3-AMT     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-MTD-AD-D-AMT   PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-MTD-RDGARD-AMT PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-MTD-NON-FILE-AMT PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-YTD-AH-AMT     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-YTD-CL-AMT     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-YTD-PP-AMT     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-YTD-O1-AMT     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-YTD-O2-AMT     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-YTD-O3-AMT     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-YTD-AD-D-AMT   PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-YTD-RDGARD-AMT PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-YTD-NON-FILE-AMT PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-MTD-AH-REF     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-MTD-CL-REF     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-MTD-PP-REF     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-MTD-O1-REF     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-MTD-O2-REF     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-MTD-O3-REF     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-YTD-AH-REF     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-YTD-CL-REF     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-YTD-PP-REF     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-YTD-O1-REF     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-YTD-O2-REF     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-YTD-O3-REF     PIC S9(8)V99    COMP-3 VALUE 0.
       01  HOLD-MTD-NEW-LNS    PIC S9(5)    COMP-3 VALUE 0.
       01  HOLD-YTD-NEW-LNS    PIC S9(5)    COMP-3 VALUE 0.
       01  HOLD-MTD-FB-LNS     PIC S9(5)    COMP-3 VALUE 0.
       01  HOLD-YTD-FB-LNS     PIC S9(5)    COMP-3 VALUE 0.
       01  HOLD-MTD-PB-LNS     PIC S9(5)    COMP-3 VALUE 0.
       01  HOLD-YTD-PB-LNS     PIC S9(5)    COMP-3 VALUE 0.
       01  HOLD-MTD-VOL-AMT    PIC S9(9)V99 COMP-3 VALUE 0.
       01  HOLD-YTD-VOL-AMT    PIC S9(9)V99 COMP-3 VALUE 0.
       01  HOLD-MTD-PL         PIC S9(7)    COMP-3 VALUE 0.
       01  HOLD-YTD-PL         PIC S9(7)    COMP-3 VALUE 0.
       01  HOLD-NO-CUST-1-YR-AGO        PIC S9(5)    COMP-3 VALUE 0.
       01  HOLD-OUTS-BAL-1-YR-AGO       PIC S9(9)V99 COMP-3 VALUE 0.
       01  HOLD-WCBC-NO-CUST-1-YR-AGO   PIC S9(5)    COMP-3 VALUE 0.
       01  HOLD-WCBC-OUTS-BAL-1-YR-AGO  PIC S9(9)V99 COMP-3 VALUE 0.
       01  HOLD-LARGE-NO-CUST-1-YR-AGO  PIC S9(5)    COMP-3 VALUE 0.
       01  HOLD-LARGE-OUTS-BAL-1-YR-AGO PIC S9(9)V99 COMP-3 VALUE 0.
       01  HOLD-CUR-NO-CONT-SLOW        PIC S9(5)    COMP-3 VALUE 0.
       01  HOLD-WCBC-CUR-NO-CONT-SLOW   PIC S9(5)    COMP-3 VALUE 0.
       01  HOLD-LARGE-CUR-NO-CONT-SLOW  PIC S9(5)    COMP-3 VALUE 0.

       01  LN-WORKERS.
           03  LN-FEED         PIC 99     VALUE 0.
           03  LN-COUNT        PIC 999    VALUE 99.
           03  LN-WK           PIC 999    VALUE 0.
       01  PAGE-NUMBER         PIC 9(4)   VALUE 0.

       01  FIRST-TIME           PIC X         VALUE "Y".
       01  PREV-SCURR-TABLE.
           03  PREV-SCURR       PIC 9(4) COMP OCCURS 15.
       01  K                    PIC 9(3)      VALUE 0.
       01  TOT-SUB              PIC 999.
       01  LEVEL-SUB            PIC 9999.
       01  GROUP-SUB            PIC 9999.
       01  PREV-SUB             PIC 9999.
       01  TOT-RECORDS          PIC 9(9)      COMP-3 VALUE 0.
       01  TOT-DISKETTE-RECS    PIC 9(9)      COMP-3 VALUE 0.
       01  WS-UNITS             PIC S9(9)     COMP-3 VALUE 0.
       01  WS-OUTBAL            PIC S9(9)V99  COMP-3 VALUE 0.
       01  WS-MTD-PERCENT       PIC S9(3)V99  COMP-3 VALUE 0.
       01  SLOW-TOP             PIC S9(9)     COMP-3 VALUE 0.
       01  TOT-LARGE-OUTS       PIC S9(9)V99  COMP-3 VALUE 0.
       01  TOT-LARGE-90-PLUS    PIC S9(9)V99  COMP-3 VALUE 0.
       01  TOT-LARGE-CUR-NO-CUST      PIC S9(5)    COMP-3.
       01  TOT-LARGE-MTD-YTD          OCCURS 2.
           03  TOT-LARGE-NO-NB        PIC S9(5)    COMP-3.
           03  TOT-LARGE-NO-FB        PIC S9(5)    COMP-3.
           03  TOT-LARGE-NO-PB        PIC S9(5)    COMP-3.
           03  TOT-LARGE-GROSS-VOLUME PIC S9(9)V99 COMP-3.
       01  TOT-LARGE-NO-30-DAY-ACCTS  PIC S9(7)    COMP-3.
       01  TOT-LARGE-AMT-30-DAY-ACCTS PIC S9(9)V99 COMP-3.
       01  TOT-LARGE-NO-60-DAY-ACCTS  PIC S9(7)    COMP-3.
       01  TOT-LARGE-AMT-60-DAY-ACCTS PIC S9(9)V99 COMP-3.
       01  TOT-LARGE-NO-90-DAY-ACCTS  PIC S9(7)    COMP-3.
       01  LARGE-LOAN-FLAG      PIC X         VALUE "N".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  STORE-DATE       PIC 9(4).
           03  STORE-DATEX      REDEFINES STORE-DATE.
               05  STORE-MO     PIC 9(2).
               05  STORE-YR     PIC 9(2).
           03  ENT-GROUP        PIC X(4).

       01  VERSION-CONTROL      PIC X VALUE "B".

       01  HOLD-BY1-KEY.
           03  HOLD-BY-DATE-CCYYMM     PIC 9(6).
           03  HOLD-BY-BRNO     PIC 9(4).
           03  HOLD-BY-CLASS    PIC 9(3).

       01  CUR-MO-BY-DATE-CCYYMM       PIC 9(6).
       01  FILLER REDEFINES CUR-MO-BY-DATE-CCYYMM.
           03  CURRENT-YEAR     PIC 9(4).
           03  CURRENT-MONTH    PIC 99.
       01  NEXT-MO-BY-DATE-CCYYMM      PIC 9(6).

       01  1-YR-AGO-BY-DATE-CCYYMM    PIC 9(6).
       01  FILLER REDEFINES 1-YR-AGO-BY-DATE-CCYYMM.
           03  1-YR-AGO-BY-DATE-CCYY  PIC 9(4).
           03  1-YR-AGO-BY-DATE-MM    PIC 99.

       01  DISPLAY-DATE             PIC 9(4).
       01  DISPLAY-DATEX            REDEFINES DISPLAY-DATE.
           03  DISPLAY-MO           PIC 99.
           03  DISPLAY-YR           PIC 99.

       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  DISPLAY-BUF.
           03  FILLER           PIC X(12) VALUE "PROCESSING..".
           03  DISPLAY-ACCT     PIC Z(6).
           03  FILLER           PIC X(4) VALUE SPACES.
           03  DISPLAY-COUNT    PIC Z(8).
           03  FILLER           PIC X(20) VALUE SPACES.
       01  PROCESS-YYYYMM       PIC 9(6).
       01  FILLER REDEFINES PROCESS-YYYYMM.
           03  PROCESS-YYYY    PIC 9(4).
           03  PROCESS-MM      PIC 99.

       01  LAST-MO-YYYYMM      PIC 9(6).
       01  FILLER REDEFINES LAST-MO-YYYYMM.
           03  LAST-MO-YYYY    PIC 9(4).
           03  LAST-MO-MM      PIC 99.

       01  LAST-YR-YYYYMM      PIC 9(6).
       01  FILLER REDEFINES LAST-YR-YYYYMM.
           03  LAST-YR-YYYY    PIC 9(4).
           03  LAST-YR-MM      PIC 99.


       01  LOG-CHANGE-MSG.
           03  LOG-BRNOX      PIC X(4).
           03  LOG-BRNO REDEFINES LOG-BRNOX
                              PIC 9(4).
           03  FILLER         PIC X     VALUE SPACES.
           03  LOG-MESS.
               05  LOG-MESS1      PIC X(25) VALUE SPACES.
               05  FILLER         PIC X(7)  VALUE "GROUP: ".
               05  LOG-GROUP      PIC X(4).
               05  FILLER         PIC X(1)  VALUE SPACES.
               05  LOG-DATE.
                   07  LOG-MONTH     PIC 9(2).
                   07  FILLER        PIC X  VALUE "/".
                   07  LOG-YEAR      PIC 9(4).

      ********************************************************
      *            P R I N T    B U F F E R S                *
      ********************************************************
       01  HEAD1.
           03  H-DATE             PIC X(8)    VALUE " ".
           03  FILLER             PIC XX      VALUE " ".
           03  H-HR               PIC 99      VALUE 0.
           03  FILLER             PIC X       VALUE ":".
           03  H-MIN              PIC 99      VALUE 0.
           03  FILLER             PIC X(10)   VALUE " ".
           03  H-NAME             PIC X(40)   VALUE " ".
           03  FILLER             PIC X(6)   VALUE " ".
           03  FILLER             PIC X(5)    VALUE "PAGE ".
           03  H-PAGE-NO          PIC ZZZ9.

       01  HEAD2.
           03  FILLER             PIC X(10)   VALUE "USER ID:".
           03  H-USERID           PIC X(8).
           03  FILLER             PIC X(9)    VALUE " ".
           03  FILLER             PIC X(42)   VALUE
               "STATISTIC LISTING FOR SMITH TAPE CREATE".

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(5).
           03  RANGE1N          PIC Z(10).
           03  FILLER           PIC X(5).
           03  RANGE2           PIC Z(9).99-.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/BYRSUP_DEF.CPY".
       COPY "LIBSP/BYRSUP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/BYRSUP_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-WR1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BY1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *******************************************
       MAIN-MODULE SECTION.
      *******************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BYRSUP" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-GROUP-LOC.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              MOVE ZEROS TO WGR-SCURR-TABLE
              MOVE 15 TO WGR-TPLUS
              PERFORM TOTAL-LEVEL-CHECK
              GO TO END-ROUTINE.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE WGR-SCURR-TABLE TO PREV-SCURR-TABLE
           ELSE
              PERFORM TOTAL-LEVEL-CHECK.

       READ-CURRENT-BRANCH.
           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

      * I KNOW BY IS GLOBAL BUT NEEDS BR-MACHINE!
           MOVE "B"     TO BY-PATH-OVERRIDE.

           PERFORM OPEN-BY1-FILE.

           PERFORM CHECK-CLASS-1-EXISTS.

       START-BY.
           MOVE NEXT-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.
           MOVE WGR-SRESULT            TO BY-BRNO.
           MOVE 0                      TO BY-CLASS.

           PERFORM START-BY1-FILE.
       NEXT-BY1.
           PERFORM READ-BY1-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO NEXT-GROUP-LOC.

           IF BY-DATE-CCYYMM NOT = NEXT-MO-BY-DATE-CCYYMM
              GO TO NEXT-GROUP-LOC.

           IF BY-BRNO NOT = WGR-SRESULT
              GO TO NEXT-GROUP-LOC.

           MOVE "N" TO FIRST-TIME.
      
           MOVE BY-CLASS TO LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-FG NOT = 0
              GO TO NEXT-GROUP-LOC.

           IF BY-CLASS = 2 OR 5 OR 8 OR 20 OR 3 OR 21 OR 50
               OR 4 OR 22 OR 6
              GO TO NEXT-BY1.

      ***************************************************

           MOVE 0 TO HOLD-MTD-AH HOLD-MTD-CL
                     HOLD-MTD-PP HOLD-MTD-O1
                     HOLD-MTD-O2 HOLD-MTD-O3
                     HOLD-MTD-AD-D HOLD-MTD-RDGARD
                     HOLD-MTD-NON-FILE
                     HOLD-YTD-AH HOLD-YTD-CL
                     HOLD-YTD-PP HOLD-YTD-O1
                     HOLD-YTD-O2 HOLD-YTD-O3
                     HOLD-YTD-AD-D HOLD-YTD-RDGARD
                     HOLD-YTD-NON-FILE
                     HOLD-MTD-AH-AMT     HOLD-MTD-CL-AMT
                     HOLD-MTD-PP-AMT     HOLD-MTD-O1-AMT
                     HOLD-MTD-O2-AMT     HOLD-MTD-O3-AMT
                     HOLD-MTD-AD-D-AMT   HOLD-MTD-RDGARD-AMT
                     HOLD-MTD-NON-FILE-AMT
                     HOLD-YTD-AH-AMT     HOLD-YTD-CL-AMT
                     HOLD-YTD-PP-AMT     HOLD-YTD-O1-AMT
                     HOLD-YTD-O2-AMT     HOLD-YTD-O3-AMT
                     HOLD-YTD-AD-D-AMT   HOLD-YTD-RDGARD-AMT
                     HOLD-YTD-NON-FILE-AMT
                     HOLD-MTD-AH-REF     HOLD-MTD-CL-REF
                     HOLD-MTD-PP-REF     HOLD-MTD-O1-REF
                     HOLD-MTD-O2-REF     HOLD-MTD-O3-REF
                     HOLD-YTD-AH-REF     HOLD-YTD-CL-REF
                     HOLD-YTD-PP-REF     HOLD-YTD-O1-REF
                     HOLD-YTD-O2-REF     HOLD-YTD-O3-REF
                     HOLD-MTD-NEW-LNS    HOLD-YTD-NEW-LNS
                     HOLD-MTD-FB-LNS     HOLD-YTD-FB-LNS
                     HOLD-MTD-PB-LNS     HOLD-YTD-PB-LNS
                     HOLD-MTD-VOL-AMT    HOLD-YTD-VOL-AMT
                     HOLD-MTD-PL         HOLD-YTD-PL.

           MOVE 0 TO TOT-LARGE-OUTS
                     TOT-LARGE-90-PLUS
                     TOT-LARGE-CUR-NO-CUST
                     TOT-LARGE-NO-NB(1) TOT-LARGE-NO-NB(2)
                     TOT-LARGE-NO-FB(1) TOT-LARGE-NO-FB(2)
                     TOT-LARGE-NO-PB(1) TOT-LARGE-NO-PB(2)
                     TOT-LARGE-GROSS-VOLUME(1)
                     TOT-LARGE-GROSS-VOLUME(2)
                     TOT-LARGE-NO-30-DAY-ACCTS
                     TOT-LARGE-AMT-30-DAY-ACCTS
                     TOT-LARGE-NO-60-DAY-ACCTS
                     TOT-LARGE-AMT-60-DAY-ACCTS
                     TOT-LARGE-NO-90-DAY-ACCTS.
           MOVE 0 TO HOLD-NO-CUST-1-YR-AGO
                     HOLD-OUTS-BAL-1-YR-AGO
                     HOLD-WCBC-NO-CUST-1-YR-AGO
                     HOLD-WCBC-OUTS-BAL-1-YR-AGO
                     HOLD-LARGE-NO-CUST-1-YR-AGO
                     HOLD-LARGE-OUTS-BAL-1-YR-AGO
                     HOLD-CUR-NO-CONT-SLOW
                     HOLD-WCBC-CUR-NO-CONT-SLOW
                     HOLD-LARGE-CUR-NO-CONT-SLOW.

           MOVE "N" TO LARGE-LOAN-FLAG.

      * GLB  11/25/03
      * BLV  01/16/04 TOO MUCH DETAIL, STARRING OFF LOGGING EACH
      *               BRANCH, AT LEAST FOR NOW
      *    MOVE "  PROCESSING BRANCH STATS"      TO LOG-MESS1.
      *    MOVE BY-BRNO                          TO LOG-BRNO.
      *    PERFORM LOG-CHANGES.

      * COMBINE CLASS 1 AND ALL NON-GIFTNET CLASSES INTO ONE STAT RECORD
           IF BY-CLASS = 1
              PERFORM COMBINE-CLASSES.

      * COMBINE CLASS 30 AND CLASS 40 INTO ONE STAT RECORD
           IF BY-CLASS = 30
              PERFORM COMBINE-SALES-CLASSES.

      * IF CLASS 40, CHECK TO SEE IF CLASS 30 EXISTS - IF SO WE
      * ALREADY ADDED IN CLASS 40, IF NOT WE WILL PROCESS CLASS 40
           IF BY-CLASS = 40
              MOVE BY1-KEY TO HOLD-BY1-KEY
              MOVE 30 TO BY-CLASS
              PERFORM READ-BY1-FILE
              IF IO-FG = 0
                 MOVE HOLD-BY1-KEY TO BY1-KEY
                 PERFORM START-BY1-FILE
                 PERFORM READ-BY1-FILE-NEXT
                 GO TO NEXT-BY1
              ELSE
                 MOVE HOLD-BY1-KEY TO BY1-KEY
                 PERFORM START-BY1-FILE
                 PERFORM READ-BY1-FILE-NEXT.

      * SAVE BY HERE, GET-CUR-MO WILL WIPE OUT COMBINED CLASSES NOW
      * IN BY-REC FOR GA, SC, NM:

           MOVE BY-REC TO WK-REC.

           PERFORM GET-CUR-MO-BY-FOR-INS.
           PERFORM GET-1-YR-AGO-BY.

      * GET-CUR-MO ROUTINE ONLY ADDED TO FIELDS IN WORKING STORAGE
      * & READ BACK GOOD BY-REC, NOW RESTORE CORRECT COMBINED INFO:

           MOVE WK-REC TO BY-REC.

           PERFORM CREATE-RATING-SHEET-REC.

           COMPUTE WS-UNITS =
                   + BY-UNITS(2,1,1) + BY-UNITS(2,1,2)
                   + BY-UNITS(2,1,3) + BY-UNITS(2,1,4)
                   + BY-UNITS(2,1,5) + BY-UNITS(2,1,6)
                   + BY-UNITS(2,1,7) + BY-UNITS(2,1,8)
                   + BY-UNITS(2,1,9).

           PERFORM CHECK-ADD-TOTALS.

           GO TO NEXT-BY1.

       END-ROUTINE.
           MOVE "RATING SHEET STATS DONE  " TO LOG-MESS1.
           MOVE SPACES                      TO LOG-BRNOX.
           PERFORM LOG-CHANGES.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE BYRSUP-QUEUE-POS TO Q-POS.
           MOVE BYRSUP-COPIES-POS TO C-POS.
           MOVE BYRSUP-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "STATS DATA CREATE IN PROCESS" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-WR1-FILE.

           PERFORM OPEN-PR-FILE.

           MOVE 0 TO TOT-RECORDS TOT-DISKETTE-RECS.
           MOVE ZEROS TO TOTAL-TABLE
                         TOTAL-GIFT-TABLE.
           MOVE 0 TO TOT-WABD-115 TOT-WABD-120
                     TOT-WGBD-115 TOT-WGBD-120
                     TOT-WLBD-115 TOT-WLBD-120.
           MOVE 0 TO TOT-WARS-01 TOT-WARS-05 TOT-WARS-06
                     TOT-WARS-07 TOT-WARS-09 TOT-WARS-34
                     TOT-WARS-45 TOT-WARS-46 TOT-WARS-48
                     TOT-WARS-49 TOT-WARS-51 TOT-WARS-52.
           MOVE 0 TO TOT-WAGN-01 TOT-WAGN-05 TOT-WAGN-06
                     TOT-WAGN-07 TOT-WAGN-09 TOT-WAGN-34
                     TOT-WAGN-45 TOT-WAGN-46 TOT-WAGN-48
                     TOT-WAGN-49 TOT-WAGN-51 TOT-WAGN-52.
           MOVE 0 TO TOT-WAIS-508 TOT-WAIS-509
                     TOT-WAIS-525 TOT-WAIS-526 TOT-WAIS-455
                     TOT-WAIS-457 TOT-WAIS-462 TOT-WAIS-469.
           MOVE 0 TO TOT-WAGI-508 TOT-WAGI-509
                     TOT-WAGI-525 TOT-WAGI-526 TOT-WAGI-455
                     TOT-WAGI-457 TOT-WAGI-462 TOT-WAGI-469.

           PERFORM OPEN-GR1-FILE.
           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *
      *    IF WGR-LEVEL-TABLE(3) NOT = "Y"
      *       MOVE "LEVEL MISSING" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO ENDIT.

      *    IF WGR-LEVEL-TABLE(4) NOT = "Y"
      *       MOVE "LEVEL MISSING" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO ENDIT.

      *    IF WGR-LEVEL-TABLE(7) NOT = "Y"
      *       MOVE "DISTRICT LEVEL MISSING" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO ENDIT.

      *    IF WGR-LEVEL-TABLE(9) NOT = "Y"
      *       MOVE "BRANCH LEVEL MISSING" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO ENDIT.

           MOVE "Y" TO TOTAL-LEVEL-TABLE(1)
                       TOTAL-LEVEL-TABLE(3)
                       TOTAL-LEVEL-TABLE(4)
                       TOTAL-LEVEL-TABLE(6)
                       TOTAL-LEVEL-TABLE(7).

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           PERFORM HEAD-PAGE.

           MOVE CUR-MO-BY-DATE-CCYYMM TO PROCESS-YYYYMM.

      * GLB  11/25/03
           MOVE CURRENT-YEAR                TO LOG-YEAR.
           MOVE CURRENT-MONTH               TO LOG-MONTH.
           MOVE ENT-GROUP                   TO LOG-GROUP.
           MOVE "START RATING SHEET STATS " TO LOG-MESS1.
           MOVE SPACES                      TO LOG-BRNOX.
           PERFORM LOG-CHANGES.

           MOVE PROCESS-YYYYMM TO LAST-MO-YYYYMM.
           IF LAST-MO-MM > 1
              SUBTRACT 1 FROM LAST-MO-MM
           ELSE
              SUBTRACT 1 FROM LAST-MO-YYYY
              MOVE 12 TO LAST-MO-MM.

           MOVE PROCESS-YYYYMM TO LAST-YR-YYYYMM.
           MOVE 3 TO LAST-YR-MM.
           IF PROCESS-MM NOT > 3
              SUBTRACT 1 FROM LAST-YR-YYYY.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ***********************************
       ENTER-PARAMETERS SECTION.
      ***********************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO ENTER-GROUP.

      *----------------------------------------------------------------
       ENTER-DATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-GROUP.

           MOVE "EN N040MMYY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DATE.

           IF VDUNUM0 = 0 GO TO ENTER-DATE.

           MOVE VDUNUM0 TO STORE-DATE DISPLAY-DATE.

           IF DISPLAY-MO < 1 OR DISPLAY-MO > 12
              MOVE "MONTH ENTRY ERROR " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-DATE.

           MOVE STORE-MO TO DATE-MMDDYY-MM.
           MOVE STORE-YR TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO CUR-MO-BY-DATE-CCYYMM.

           MOVE CURRENT-MONTH TO NDTE-MM.
           MOVE 1 TO NDTE-DD.
           MOVE CURRENT-YEAR TO NDTE-CCYY.
           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO NEXT-MO-BY-DATE-CCYYMM.
           MOVE NEXT-MO-BY-DATE-CCYYMM TO 1-YR-AGO-BY-DATE-CCYYMM.
           SUBTRACT 1 FROM 1-YR-AGO-BY-DATE-CCYY.

       ENTER-GROUP.
           MOVE "E  A040GROUP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO ENTER-GROUP
              ELSE
                 MOVE "S  A000GROUP." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS = "1U" GO TO ENTER-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-GROUP.
           MOVE VDUBUF TO WGR-WORD
                          ENT-GROUP.
           IF WGR-WORD = SPACES
              GO TO ENTER-GROUP.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
                   AND WGR-ERR NOT = 1
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-GROUP.
           IF WGR-TYPE NOT = "A"
              GO TO ENTER-GROUP.

           MOVE WGR-WORD TO ENT-GROUP.

       EXIT-PARM.
           EXIT.

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************
           MOVE EXT-EOBUF-DATE2 TO DATE-YYYYMMDD NDTE-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO CUR-MO-BY-DATE-CCYYMM.

      * SETUP NEXT MONTH DATE:

           MOVE 1 TO NDTE-HOLD.
           MOVE 1 TO NDTE-DD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO NEXT-MO-BY-DATE-CCYYMM.
           MOVE NEXT-MO-BY-DATE-CCYYMM TO 1-YR-AGO-BY-DATE-CCYYMM.
           SUBTRACT 1 FROM 1-YR-AGO-BY-DATE-CCYY.

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************
           MOVE "S  A040GROUP." TO VDU.
           MOVE ENT-GROUP TO VDUBUF.
           PERFORM SCREENIO.

      **************************************************
      *   CHECK FOR LEVEL SUMMARY TOTALS
      *   WGR-SCURR IS TABLE OF POINTERS FOR CURRENT GROUP
      *   WHICH POINT TO WGR-SID TABLE TO SHOW CURRENT
      *   GROUP HIERARCHY:  WGR-SCURR(1) POINTS TO WGR-SID
      *   OF LOW LEVEL (SELECTED GROUP); WGR-SCURR(WGR-TPLUS)
      *   POINTS TO THE WGR-SID OF THE HIGHEST LEVEL WHICH
      *   IS THE BRANCH NUMBER CURRENTLY BEING PROCESSED.
      *   THIS ROUTINE COMPARES THE WGR-SCURR TABLE TO THE
      *   TABLE SAVED FROM THE PREVIOUS GROUP, PRINTING &
      *   ZEROING TOTALS FOR EACH LEVEL WHICH HAS CHANGED
      *   STARTING WITH THE HIGHEST (BRANCH) LEVEL & WORKING
      *   THROUGH HIERARCHY.  GRAND TOTALS ARE FORCED BY
      *   MOVING 0 TO WGR-SCURR TABLE & 15 TO WGR-TPLUS.
      **************************************************
       TOTAL-LEVEL-CHECK SECTION.
      **************************************************

           MOVE 15 TO GROUP-SUB.
       LEVEL-TOTAL-LOOP.
           IF GROUP-SUB = 0
              GO TO EXIT-TOTAL-LEVEL-CHECK.

           IF WGR-SCURR(GROUP-SUB) = PREV-SCURR(GROUP-SUB)
              GO TO CONTINUE-LOOP.

           MOVE PREV-SCURR(GROUP-SUB) TO PREV-SUB.
           IF PREV-SUB = 0
              GO TO CONTINUE-LOOP.
           IF WGR-SID(PREV-SUB) NUMERIC
              GO TO CONTINUE-LOOP.
           MOVE WGR-SID(PREV-SUB) TO GR-ID.
           PERFORM READ-GR1-FILE.
           IF TOTAL-LEVEL-TABLE(GR-LEVEL) NOT = "Y"
              GO TO CONTINUE-LOOP.
           MOVE GR-LEVEL TO TOT-SUB.

           PERFORM CREATE-STATS-REC.

           GO TO CONTINUE-LOOP.
       CONTINUE-LOOP.
           SUBTRACT 1 FROM GROUP-SUB.
           IF GROUP-SUB < WGR-SSTABLE
              GO TO EXIT-TOTAL-LEVEL-CHECK.
           GO TO LEVEL-TOTAL-LOOP.
       EXIT-TOTAL-LEVEL-CHECK.
           MOVE WGR-SCURR-TABLE TO PREV-SCURR-TABLE.

      **************************************************
       CHECK-ADD-TOTALS SECTION.
      **************************************************
           MOVE WGR-TPLUS TO GROUP-SUB.
       CHECK-TOTAL-LOOP.
           IF GROUP-SUB = 0
              GO TO EXIT-CHECK-TOTALS.

           MOVE WGR-SCURR(GROUP-SUB) TO LEVEL-SUB.
           IF WGR-SID(LEVEL-SUB) IS NUMERIC
              GO TO CONTINUE-CHECK-TOTALS.

           MOVE WGR-SID(LEVEL-SUB) TO GR-ID.
           PERFORM READ-GR1-FILE.
           MOVE GR-LEVEL TO TOT-SUB.
       CHECK-TOTAL-FLAG.
           IF TOTAL-LEVEL-TABLE(TOT-SUB) NOT = "Y"
              GO TO CONTINUE-CHECK-TOTALS.

           PERFORM ADD-TOTALS.

       CONTINUE-CHECK-TOTALS.
           SUBTRACT 1 FROM GROUP-SUB.
           IF GROUP-SUB > 0
              GO TO CHECK-TOTAL-LOOP.
       EXIT-CHECK-TOTALS.
           EXIT.

      **************************************************
       ADD-TOTALS SECTION.
      **************************************************

      * 1350
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50
              COMPUTE TOTAL-GIFT-UNITS-1(TOT-SUB) =
                      TOTAL-GIFT-UNITS-1(TOT-SUB) + WS-UNITS
              COMPUTE TOTAL-GIFT-UNITS-2(TOT-SUB) =
                      TOTAL-GIFT-UNITS-2(TOT-SUB) + WS-UNITS
                                                  + BY-UNITS(2,1,10)
           ELSE
              COMPUTE TOTAL-UNITS-1(TOT-SUB) =
                      TOTAL-UNITS-1(TOT-SUB) + WS-UNITS
              COMPUTE TOTAL-UNITS-2(TOT-SUB) =
                      TOTAL-UNITS-2(TOT-SUB) + WS-UNITS
                                             + BY-UNITS(2,1,10).

      **************************************************
       CREATE-STATS-REC SECTION.
      **************************************************

      * CREATE WASF (SLOW FILE)
           COMPUTE ST2-PERSLOW ROUNDED =
                               (TOTAL-UNITS-1(TOT-SUB) /
                                TOTAL-UNITS-2(TOT-SUB)) * 100.

           IF TOTAL-GIFT-UNITS-1(TOT-SUB) NOT = 0 OR
               TOTAL-GIFT-UNITS-2(TOT-SUB) NOT = 0
              COMPUTE ST2-PERSLOW ROUNDED =
                     (TOTAL-GIFT-UNITS-1(TOT-SUB) /
                      TOTAL-GIFT-UNITS-2(TOT-SUB)) * 100.

           MOVE 0 TO TOTAL-UNITS-1(TOT-SUB) TOTAL-UNITS-2(TOT-SUB).
           MOVE 0 TO TOTAL-GIFT-UNITS-1(TOT-SUB)
                     TOTAL-GIFT-UNITS-2(TOT-SUB).

      *********************************************
       CREATE-RATING-SHEET-REC SECTION.
      *********************************************
           IF BY-CLASS NOT = 30 AND BY-CLASS NOT = 40         
              MOVE "Y" TO LARGE-LOAN-FLAG.

           MOVE BR-NO TO WR-BRNO.
           MOVE PROCESS-YYYYMM TO WR-YYYYMM.
           PERFORM READ-WR1-FILE.
           IF IO-BAD
              PERFORM CLEAR-WR-FILE
              MOVE BR-NO TO WR-BRNO
              MOVE PROCESS-YYYYMM TO WR-YYYYMM
              PERFORM WRITE-WR1-FILE
              IF IO-BAD
                 GO TO IO-ERROR.

           PERFORM RETRIEVE-LAST-MO-YR-WR.

      * CREATE WABD (BAD DEBT)
      * 115 = OUTSTANDING LEDGER BALANCE
      * RATING SHEET LINE 01 :
      * TOTAL OUTSTANDING, ONLY HERE FOR TESTING SINCE WE
      * DON'T HAVE GL TO COMPUTE OUTSTANDING RIGHT NOW.
           MOVE 0 TO WS-OUTBAL.
           PERFORM COMPUTE-WS-OUTBAL VARYING K FROM 1 BY 1
                     UNTIL K > 11.

           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              MOVE WS-OUTBAL TO WR-WCBC-OUTS-BALANCE
           ELSE
              MOVE WS-OUTBAL TO WR-OUTS-BALANCE.

           IF LARGE-LOAN-FLAG = "Y"
              SUBTRACT TOT-LARGE-OUTS FROM WS-OUTBAL.

           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              ADD WS-OUTBAL TO TOT-WGBD-115
           ELSE
              ADD WS-OUTBAL TO TOT-WABD-115.

      * CREATE WLBD (BAD DEBT - LARGE LOANS, CLASS 5-20)
      * 115 = OUTSTANDING LEDGER BALANCE
           IF LARGE-LOAN-FLAG = "Y"
              MOVE TOT-LARGE-OUTS TO WR-LARGE-OUTS-BALANCE
              ADD TOT-LARGE-OUTS TO TOT-WLBD-115.

      * CREATE WABD (BAD DEBT)
      * 120 = AMT OF 90 DAY ACCTS
           COMPUTE ST1-AMOUNT = BY-OUTBAL(2,1,3) + BY-OUTBAL(2,1,4)
                              + BY-OUTBAL(2,1,5) + BY-OUTBAL(2,1,6)
                              + BY-OUTBAL(2,1,7).
           IF LARGE-LOAN-FLAG = "Y"
              SUBTRACT TOT-LARGE-90-PLUS FROM ST1-AMOUNT.

           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              ADD ST1-AMOUNT TO TOT-WGBD-120
           ELSE
              ADD ST1-AMOUNT TO TOT-WABD-120.

      * CREATE WLBD (BAD DEBT - LARGE LOANS, CLASS 5-20)
      * 120 = AMT OF 90 DAY ACCTS
           IF LARGE-LOAN-FLAG = "Y"
              MOVE TOT-LARGE-90-PLUS TO WR-LARGE-AMT-90-DAY-ACCTS
              ADD TOT-LARGE-90-PLUS TO TOT-WLBD-120.

      * CREATE WARS (RATING SHEET)
      * RATING SHEET LINE 01 :
      * # LOANS OUTSTANDING (ELM, GAIN-LOSS, 1-11)

           MOVE 0 TO WS-UNITS.
           PERFORM COMPUTE-WS-UNITS VARYING K FROM 1 BY 1 UNTIL
               K > 11.
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              MOVE WS-UNITS TO WR-WCBC-CUR-NO-CUST
              ADD WS-UNITS TO TOT-WAGN-01
           ELSE
              MOVE WS-UNITS TO WR-CUR-NO-CUST
              ADD WS-UNITS TO TOT-WARS-01.

           IF LARGE-LOAN-FLAG = "Y"
              MOVE TOT-LARGE-CUR-NO-CUST TO WR-LARGE-CUR-NO-CUST
              MOVE TOT-LARGE-NO-NB(1) TO WR-LARGE-NO-NB(1)
              MOVE TOT-LARGE-NO-NB(2) TO WR-LARGE-NO-NB(2)
              MOVE TOT-LARGE-NO-FB(1) TO WR-LARGE-NO-FB(1)
              MOVE TOT-LARGE-NO-FB(2) TO WR-LARGE-NO-FB(2)
              MOVE TOT-LARGE-NO-PB(1) TO WR-LARGE-NO-PB(1)
              MOVE TOT-LARGE-NO-PB(2) TO WR-LARGE-NO-PB(2)
              MOVE TOT-LARGE-GROSS-VOLUME(1) TO WR-LARGE-GROSS-VOLUME(1)
              MOVE TOT-LARGE-GROSS-VOLUME(2) TO WR-LARGE-GROSS-VOLUME(2)
              MOVE TOT-LARGE-NO-30-DAY-ACCTS TO WR-LARGE-NO-30-DAY-ACCTS
              MOVE TOT-LARGE-AMT-30-DAY-ACCTS TO
                    WR-LARGE-AMT-30-DAY-ACCTS
              MOVE TOT-LARGE-NO-60-DAY-ACCTS TO
                    WR-LARGE-NO-60-DAY-ACCTS
              MOVE TOT-LARGE-AMT-60-DAY-ACCTS TO
                    WR-LARGE-AMT-60-DAY-ACCTS
              MOVE TOT-LARGE-NO-90-DAY-ACCTS TO
                    WR-LARGE-NO-90-DAY-ACCTS.

      * RATING SHEET LINE 05 : # NB LOANS MADE MTD & YTD
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              MOVE HOLD-MTD-NEW-LNS TO WR-WCBC-NO-NB(1)
              MOVE HOLD-YTD-NEW-LNS TO WR-WCBC-NO-NB(2)
              ADD HOLD-MTD-NEW-LNS TO TOT-WAGN-05
           ELSE
              MOVE HOLD-MTD-NEW-LNS TO WR-NO-NB(1)
              MOVE HOLD-YTD-NEW-LNS TO WR-NO-NB(2)
              ADD HOLD-MTD-NEW-LNS TO TOT-WARS-05.

      * RATING SHEET LINE 06 : # FB LOANS MADE MTD & YTD
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              MOVE HOLD-MTD-FB-LNS TO WR-WCBC-NO-FB(1)
              MOVE HOLD-YTD-FB-LNS TO WR-WCBC-NO-FB(2)
              ADD HOLD-MTD-FB-LNS TO TOT-WAGN-06
           ELSE
              MOVE HOLD-MTD-FB-LNS TO WR-NO-FB(1)
              MOVE HOLD-YTD-FB-LNS TO WR-NO-FB(2)
              ADD HOLD-MTD-FB-LNS TO TOT-WARS-06.

      * RATING SHEET LINE 07 : # PB LOANS MADE MTD & YTD
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              MOVE HOLD-MTD-PB-LNS TO WR-WCBC-NO-PB(1)
              MOVE HOLD-YTD-PB-LNS TO WR-WCBC-NO-PB(2)
              ADD HOLD-MTD-PB-LNS TO TOT-WAGN-07
           ELSE
              MOVE HOLD-MTD-PB-LNS TO WR-NO-PB(1)
              MOVE HOLD-YTD-PB-LNS TO WR-NO-PB(2)
              ADD HOLD-MTD-PB-LNS TO TOT-WARS-07.

      * RATING SHEET LINE 09 :
      * TOTAL AMT OF LOAN VOLUME DOLLARS MADE MTD & YTD
           MOVE 0 TO WS-OUTBAL.
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              MOVE HOLD-MTD-VOL-AMT TO WR-WCBC-GROSS-VOLUME(1)
              MOVE HOLD-YTD-VOL-AMT TO WR-WCBC-GROSS-VOLUME(2)
              ADD HOLD-MTD-VOL-AMT TO TOT-WAGN-09
           ELSE
              MOVE HOLD-MTD-VOL-AMT TO WR-GROSS-VOLUME(1)
              MOVE HOLD-YTD-VOL-AMT TO WR-GROSS-VOLUME(2)
              ADD HOLD-MTD-VOL-AMT TO TOT-WARS-09.

      * RATING SHEET LINE 10 :
      * YTD PERCENT TURNOVER, COMPUTE MTD & ADD TO LAST MONTH,
      * FOR YTD ACCUMULATED PERCENT
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              IF WR-WCBC-ELM-OUTS-BAL NOT = 0
                 COMPUTE WS-MTD-PERCENT ROUNDED =
                 100 * WR-WCBC-GROSS-VOLUME(1) / WR-WCBC-ELM-OUTS-BAL
              ELSE
                 MOVE 0 TO WS-MTD-PERCENT
              END-IF
              IF PROCESS-MM = 4
                 MOVE WS-MTD-PERCENT TO WR-WCBC-PCT-VOLUME
              ELSE
              COMPUTE WR-WCBC-PCT-VOLUME =
                    WRLM-WCBC-PCT-VOLUME + WS-MTD-PERCENT
              END-IF
            ELSE
              IF WR-ELM-OUTS-BALANCE NOT = 0
                 COMPUTE WS-MTD-PERCENT ROUNDED =
                 100 * WR-GROSS-VOLUME(1) / WR-ELM-OUTS-BALANCE
              ELSE
                 MOVE 0 TO WS-MTD-PERCENT
              END-IF
              IF PROCESS-MM = 4
                 MOVE WS-MTD-PERCENT TO WR-YTD-PCT-TURNOVER
              ELSE
                 COMPUTE WR-YTD-PCT-TURNOVER =
                    WRLM-YTD-PCT-TURNOVER + WS-MTD-PERCENT.

      * RATING SHEET LINE 34 :
      * TOTAL NO OF PL CHARGE-OFFS MADE MTD & YTD
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
      *       MOVE HOLD-MTD-PL TO WR-WCBC-NO-CHARGEOFFS(1)
      *       MOVE HOLD-YTD-PL TO WR-WCBC-NO-CHARGEOFFS(2)
              ADD HOLD-MTD-PL TO TOT-WAGN-34
           ELSE
              MOVE HOLD-MTD-PL TO WR-NO-CHARGEOFFS(1)
              MOVE HOLD-YTD-PL TO WR-NO-CHARGEOFFS(2)
              ADD HOLD-MTD-PL TO TOT-WARS-34.

      * RATING SHEET LINE 45 : # 30 DAY REC DELINQ ACCTS
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              MOVE BY-UNITS(2,1,1) TO WR-WCBC-NO-30-DAY-ACCTS
              ADD BY-UNITS(2,1,1) TO TOT-WAGN-45
           ELSE
              MOVE BY-UNITS(2,1,1) TO WR-NO-30-DAY-ACCTS
              ADD BY-UNITS(2,1,1) TO TOT-WARS-45.

      * RATING SHEET LINE 46 : $ AMT 30 DAY REC DELINQ ACCTS
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              MOVE BY-OUTBAL(2,1,1) TO WR-WCBC-AMT-30-DAY-ACCTS
              ADD BY-OUTBAL(2,1,1) TO TOT-WAGN-46
           ELSE
              MOVE BY-OUTBAL(2,1,1) TO WR-AMT-30-DAY-ACCTS
              ADD BY-OUTBAL(2,1,1) TO TOT-WARS-46.


      * RATING SHEET LINE 48 : # 60 DAY REC DELINQ ACCTS
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              MOVE BY-UNITS(2,1,2) TO WR-WCBC-NO-60-DAY-ACCTS
              ADD BY-UNITS(2,1,2) TO TOT-WAGN-48
           ELSE
              MOVE BY-UNITS(2,1,2) TO WR-NO-60-DAY-ACCTS
              ADD BY-UNITS(2,1,2) TO TOT-WARS-48.

      * RATING SHEET LINE 49 : $ AMT 60 DAY REC DELINQ ACCTS
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              MOVE BY-OUTBAL(2,1,2) TO WR-WCBC-AMT-60-DAY-ACCTS
              ADD BY-OUTBAL(2,1,2) TO TOT-WAGN-49
           ELSE
              MOVE BY-OUTBAL(2,1,2) TO WR-AMT-60-DAY-ACCTS
              ADD BY-OUTBAL(2,1,2) TO TOT-WARS-49.

      * RATING SHEET LINE 51 : # 90+ DAY REC DELINQ ACCTS
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              COMPUTE WR-WCBC-NO-90-DAY-ACCTS = BY-UNITS(2,1,3)
                                              + BY-UNITS(2,1,4)
                                              + BY-UNITS(2,1,5)
                                              + BY-UNITS(2,1,6)
                                              + BY-UNITS(2,1,7)
              ADD WR-WCBC-NO-90-DAY-ACCTS TO TOT-WAGN-51
           ELSE
              COMPUTE WR-NO-90-DAY-ACCTS = BY-UNITS(2,1,3)
                                         + BY-UNITS(2,1,4)
                                         + BY-UNITS(2,1,5)
                                         + BY-UNITS(2,1,6)
                                         + BY-UNITS(2,1,7)
              ADD WR-NO-90-DAY-ACCTS TO TOT-WARS-51.

      * RATING SHEET LINE 52 : $ AMT 90+ DAY REC DELINQ ACCTS
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              COMPUTE WR-WCBC-AMT-90-DAY-ACCTS = BY-OUTBAL(2,1,3)
                                               + BY-OUTBAL(2,1,4)
                                               + BY-OUTBAL(2,1,5)
                                               + BY-OUTBAL(2,1,6)
                                               + BY-OUTBAL(2,1,7)
              ADD WR-WCBC-AMT-90-DAY-ACCTS TO TOT-WAGN-52
           ELSE
              COMPUTE WR-AMT-90-DAY-ACCTS = BY-OUTBAL(2,1,3)
                                          + BY-OUTBAL(2,1,4)
                                          + BY-OUTBAL(2,1,5)
                                          + BY-OUTBAL(2,1,6)
                                          + BY-OUTBAL(2,1,7)
              ADD WR-AMT-90-DAY-ACCTS TO TOT-WARS-52.

      * CREATE WAIS (WORLD INSURANCE)
      * 508 = CL
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              ADD HOLD-MTD-CL TO TOT-WAGI-508
           ELSE
              ADD HOLD-MTD-CL TO TOT-WAIS-508
              MOVE HOLD-MTD-CL TO WR-NO-LIFE-PREMS(1)
              MOVE HOLD-YTD-CL TO WR-NO-LIFE-PREMS(2)
              MOVE HOLD-MTD-CL-AMT TO WR-LIFE-PREMS(1)
              MOVE HOLD-YTD-CL-AMT TO WR-LIFE-PREMS(2)
              MOVE HOLD-MTD-CL-REF TO WR-LIFE-REFS(1)
              MOVE HOLD-YTD-CL-REF TO WR-LIFE-REFS(2).

      * 509 = AH
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              ADD HOLD-MTD-AH TO TOT-WAGI-509
           ELSE
              ADD HOLD-MTD-AH TO TOT-WAIS-509
              MOVE HOLD-MTD-AH TO WR-NO-AH-PREMS(1)
              MOVE HOLD-YTD-AH TO WR-NO-AH-PREMS(2)
              MOVE HOLD-MTD-AH-AMT TO WR-AH-PREMS(1)
              MOVE HOLD-YTD-AH-AMT TO WR-AH-PREMS(2)
              MOVE HOLD-MTD-AH-REF TO WR-AH-REFS(1)
              MOVE HOLD-YTD-AH-REF TO WR-AH-REFS(2).

      * 525 = PP
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              ADD HOLD-MTD-PP TO TOT-WAGI-525
           ELSE
              ADD HOLD-MTD-PP TO TOT-WAIS-525
              MOVE HOLD-MTD-PP TO WR-NO-FIRE-PREMS(1)
              MOVE HOLD-YTD-PP TO WR-NO-FIRE-PREMS(2)
              MOVE HOLD-MTD-PP-AMT TO WR-FIRE-PREMS(1)
              MOVE HOLD-YTD-PP-AMT TO WR-FIRE-PREMS(2)
              MOVE HOLD-MTD-PP-REF TO WR-FIRE-REFS(1)
              MOVE HOLD-YTD-PP-REF TO WR-FIRE-REFS(2).

      * 526 = AUTO
           MOVE BY-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO BR-STATE.

      * 11-16-94 STATE OF "TN" USES O3 FOR IUI INSURANCE, NOT AUTO
      * 04-29-97 STATE OF "TN" USES O2 FOR LIENHOLDER (469) NOW.
      * 05-13-99 STATE OF "TN" USES O1 FOR SINGLE INTEREST AUTO
      * 05-27-15 STATE OF "OK" & "MS" NOW USING O1 FOR AUTO

           IF BY-CLASS  < 30 OR  BY-CLASS  = 50 
              IF (BR-STATE = "TN"  OR "OK" OR "MS")
                MOVE HOLD-MTD-O1 TO ST4-AMOUNT
                MOVE HOLD-MTD-O1 TO WR-NO-AUTO-PREMS(1)
                MOVE HOLD-YTD-O1 TO WR-NO-AUTO-PREMS(2)
                MOVE HOLD-MTD-O1-AMT TO WR-AUTO-PREMS(1)
                MOVE HOLD-YTD-O1-AMT TO WR-AUTO-PREMS(2)
                MOVE HOLD-MTD-O1-REF TO WR-AUTO-REFS(1)
                MOVE HOLD-YTD-O1-REF TO WR-AUTO-REFS(2)
              ELSE
      * 07-01-98 STATE OF "SC" USES O3 FOR IUI INSURANCE, NOT AUTO
              IF BR-STATE = "SC"
                 MOVE HOLD-MTD-O2 TO ST4-AMOUNT
                 MOVE HOLD-MTD-O2 TO WR-NO-AUTO-PREMS(1)
                 MOVE HOLD-YTD-O2 TO WR-NO-AUTO-PREMS(2)
                 MOVE HOLD-MTD-O2-AMT TO WR-AUTO-PREMS(1)
                 MOVE HOLD-YTD-O2-AMT TO WR-AUTO-PREMS(2)
                 MOVE HOLD-MTD-O2-REF TO WR-AUTO-REFS(1)
                 MOVE HOLD-YTD-O2-REF TO WR-AUTO-REFS(2)
              ELSE
                 COMPUTE ST4-AMOUNT          = HOLD-MTD-O3 + HOLD-MTD-O2
                 COMPUTE WR-NO-AUTO-PREMS(1) = HOLD-MTD-O3 + HOLD-MTD-O2
                 COMPUTE WR-NO-AUTO-PREMS(2) = HOLD-YTD-O3 + HOLD-YTD-O2
                 COMPUTE WR-AUTO-PREMS(1) = HOLD-MTD-O3-AMT
                                          + HOLD-MTD-O2-AMT
                 COMPUTE WR-AUTO-PREMS(2) = HOLD-YTD-O3-AMT
                                          + HOLD-YTD-O2-AMT
                 COMPUTE WR-AUTO-REFS(1) =  HOLD-MTD-O3-REF
                                          + HOLD-MTD-O2-REF
                 COMPUTE WR-AUTO-REFS(2) =  HOLD-YTD-O3-REF
                                          + HOLD-YTD-O2-REF.

           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50 *> 1350
              ADD ST4-AMOUNT TO TOT-WAGI-526
           ELSE
              ADD ST4-AMOUNT TO TOT-WAIS-526.

      * 11-16-94 ADD IUI FOR STATE OF "TN", THEY USE O3 INSURANCE
      *          BUT "GA" USES O3 FOR AUTO
      * 07-01-98 ADD IUI FOR STATE OF "SC", THEY USE O3 INSURANCE
      * 04-12-00 ADD IUI FOR STATE OF "KY", THEY USE O1 INSURANCE
      * 06-27-00 ADD IUI FOR STATE OF "MO", THEY USE O1 INSURANCE
      * 04-18-03 ADD IUI FOR STATE OF "OK", THEY USE O3 INSURANCE
      * 05-19-11 ADD IUI FOR STATE OF "TX", THEY USE O1 INSURANCE
      * 01-05-2022 IUI FOR "IN","NM","UT" "ID", THEY USE O1 INSURANCE
      * 02-21-2023 IUI   FOR STATE OF "AL", THEY USE O1 INSURANCE

           IF BY-CLASS < 30 OR BY-CLASS = 50        
              IF BR-STATE = "TN" OR "SC" OR "OK"
                 MOVE HOLD-MTD-O3 TO ST4-AMOUNT
                 MOVE HOLD-MTD-O3 TO WR-NO-UNEMP-PREMS(1)
                 MOVE HOLD-YTD-O3 TO WR-NO-UNEMP-PREMS(2)
                 MOVE HOLD-MTD-O3-AMT TO WR-UNEMP-PREMS(1)
                 MOVE HOLD-YTD-O3-AMT TO WR-UNEMP-PREMS(2)
                 MOVE HOLD-MTD-O3-REF TO WR-UNEMP-REFS(1)
                 MOVE HOLD-YTD-O3-REF TO WR-UNEMP-REFS(2)
              ELSE
              IF BR-STATE = "KY" OR "MO" OR "TX" OR "IN" OR "NM"
                          OR "UT" OR "ID" OR "AL"
                 MOVE HOLD-MTD-O1 TO ST4-AMOUNT
                 MOVE HOLD-MTD-O1 TO WR-NO-UNEMP-PREMS(1)
                 MOVE HOLD-YTD-O1 TO WR-NO-UNEMP-PREMS(2)
                 MOVE HOLD-MTD-O1-AMT TO WR-UNEMP-PREMS(1)
                 MOVE HOLD-YTD-O1-AMT TO WR-UNEMP-PREMS(2)
                 MOVE HOLD-MTD-O1-REF TO WR-UNEMP-REFS(1)
                 MOVE HOLD-YTD-O1-REF TO WR-UNEMP-REFS(2)
              ELSE
                 MOVE 0 TO ST4-AMOUNT
                      WR-NO-UNEMP-PREMS(1)
                      WR-NO-UNEMP-PREMS(2)
                      WR-UNEMP-PREMS(1)
                      WR-UNEMP-PREMS(2)
                      WR-UNEMP-REFS(1)
                      WR-UNEMP-REFS(2).
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50 *> 1350
              ADD ST4-AMOUNT TO TOT-WAGI-455
           ELSE
              ADD ST4-AMOUNT TO TOT-WAIS-455.

      * 01-16-04 ADD NON-FILE; THEY USE A FEECODE TO ENTER IT,
      *          USING OCCURRENCE 8 OF BY INS TABLE.

           IF BY-CLASS < 30 OR BY-CLASS = 50   *> 1350
              MOVE HOLD-MTD-NON-FILE TO WR-LOTS-NO-NON-FILE-PREMS(1)
              MOVE HOLD-YTD-NON-FILE TO WR-LOTS-NO-NON-FILE-PREMS(2)
              MOVE HOLD-MTD-NON-FILE-AMT TO WR-LOTS-NON-FILE-PREMS(1)
              MOVE HOLD-YTD-NON-FILE-AMT TO WR-LOTS-NON-FILE-PREMS(2).

           IF BY-CLASS < 30 OR BY-CLASS = 50   *> 1350
           IF WR-YYYYMM < 200504
              COMPUTE WR-LOTS-NO-NON-FILE-PREMS(2) =
                    WRLM-LOTS-NO-NON-FILE-PREMS(2) + HOLD-MTD-NON-FILE
              COMPUTE WR-LOTS-NON-FILE-PREMS(2) =
                    WRLM-LOTS-NON-FILE-PREMS(2)
                  + HOLD-MTD-NON-FILE-AMT.

      * 08-18-95 ADD AD&D THEY USE A FEECODE TO ENTER IT,
      *          BUT WE CREATED A STATS FIELD !!
           MOVE HOLD-MTD-AD-D TO ST4-AMOUNT.

           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50  *> 1350
              ADD ST4-AMOUNT TO TOT-WAGI-457
           ELSE
              ADD ST4-AMOUNT TO TOT-WAIS-457
              MOVE HOLD-MTD-AD-D TO WR-NO-AD-D-PREMS(1)
              MOVE HOLD-YTD-AD-D TO WR-NO-AD-D-PREMS(2)
              MOVE HOLD-MTD-AD-D-AMT TO WR-AD-D-PREMS(1)
              MOVE HOLD-YTD-AD-D-AMT TO WR-AD-D-PREMS(2).

      * 08-06-96 ADD ROADGARD THEY USE A FEECODE TO ENTER IT,
      *          BUT WE CREATED A STATS FIELD !!
           MOVE HOLD-MTD-RDGARD TO ST4-AMOUNT.
           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50        
              ADD ST4-AMOUNT TO TOT-WAGI-462
           ELSE
              ADD ST4-AMOUNT TO TOT-WAIS-462
              MOVE HOLD-MTD-RDGARD TO WR-NO-ROAD-GARD-PREMS(1)
              MOVE HOLD-YTD-RDGARD TO WR-NO-ROAD-GARD-PREMS(2)
              MOVE HOLD-MTD-RDGARD-AMT TO WR-ROAD-GARD-PREMS(1)
              MOVE HOLD-YTD-RDGARD-AMT TO WR-ROAD-GARD-PREMS(2).

      * 04-29-97 ADD TN LIENHOLDER PROTECTION INSURANCE
      *          THEY USE O2 INSURANCE
           IF BY-CLASS < 30 OR BY-CLASS = 50       
              IF BR-STATE = "TN"
                 MOVE HOLD-MTD-O2 TO ST4-AMOUNT
                 MOVE HOLD-MTD-O2 TO WR-NO-L-PROT-PREMS(1)
                 MOVE HOLD-YTD-O2 TO WR-NO-L-PROT-PREMS(2)
                 MOVE HOLD-MTD-O2-AMT TO WR-L-PROT-PREMS(1)
                 MOVE HOLD-YTD-O2-AMT TO WR-L-PROT-PREMS(2)
              ELSE
                 MOVE 0 TO ST4-AMOUNT
                 MOVE HOLD-MTD-O2 TO WR-NO-L-PROT-PREMS(1)
                 MOVE HOLD-YTD-O2 TO WR-NO-L-PROT-PREMS(2)
                 MOVE HOLD-MTD-O2-AMT TO WR-L-PROT-PREMS(1)
                 MOVE HOLD-YTD-O2-AMT TO WR-L-PROT-PREMS(2).

           IF BY-CLASS NOT < 30 AND BY-CLASS NOT = 50       
              ADD ST4-AMOUNT TO TOT-WAGI-469
           ELSE
              ADD ST4-AMOUNT TO TOT-WAIS-469.

      * CREATE WASF (SLOW FILE)
      * PER BRANCH
           MOVE 0 TO WS-UNITS.
           PERFORM COMPUTE-WS-UNITS-2 VARYING K FROM 1
               BY 1 UNTIL K > 9.
           MOVE WS-UNITS TO SLOW-TOP.
           MOVE 0 TO WS-UNITS.
           PERFORM COMPUTE-WS-UNITS-2 VARYING K FROM 1
                BY 1 UNTIL K > 10.
           COMPUTE ST2-PERSLOW ROUNDED = (SLOW-TOP / WS-UNITS)
                              * 100.

           IF BY-CLASS < 30 OR BY-CLASS = 50 *> 1350
              MOVE SLOW-TOP TO WR-CUR-NO-CONT-SLOW
           ELSE
              MOVE SLOW-TOP TO WR-WCBC-CUR-NO-CONT-SLOW.

           IF LARGE-LOAN-FLAG = "Y"
              MOVE HOLD-LARGE-CUR-NO-CONT-SLOW
                            TO WR-LARGE-CUR-NO-CONT-SLOW.

           IF BY-CLASS < 30  OR BY-CLASS = 50  *> 1350
              MOVE HOLD-NO-CUST-1-YR-AGO TO WR-NO-CUST-1-YR-AGO
              MOVE HOLD-OUTS-BAL-1-YR-AGO TO WR-OUTS-BAL-1-YR-AGO
              MOVE HOLD-LARGE-NO-CUST-1-YR-AGO TO
                     WR-LARGE-NO-CUST-1-YR-AGO
              MOVE HOLD-LARGE-OUTS-BAL-1-YR-AGO TO
                     WR-LARGE-OUTS-BAL-1-YR-AGO
           ELSE
              MOVE HOLD-WCBC-NO-CUST-1-YR-AGO TO
                     WR-WCBC-NO-CUST-1-YR-AGO
              MOVE HOLD-WCBC-OUTS-BAL-1-YR-AGO TO
                     WR-WCBC-OUTS-BAL-1-YR-AGO.

           MOVE BR-NO TO WR-BRNO.
           MOVE PROCESS-YYYYMM TO WR-YYYYMM.
           PERFORM REWRITE-WR1-FILE.

       EXIT-CREATE-RATING-REC.
           EXIT.

      **************************************************
       RETRIEVE-LAST-MO-YR-WR SECTION.
      **************************************************

      *  READ LAST MONTHS RATING SHEET, STORE FIELDS IN WRLM-REC.
      *  RESTORE CURRENT MONTH RECORD & UPDATE END OF LAST MONTH
      *  VALUES WITH FIELDS FROM WRLM-REC

           MOVE BR-NO TO WR-BRNO.
           MOVE LAST-MO-YYYYMM TO WR-YYYYMM.
           PERFORM READ-WR1-FILE.
           IF IO-BAD
              PERFORM CLEAR-WR-FILE.

           MOVE WR-REC TO WRLM-REC.
       RETRIEVE-LAST-YR.
           MOVE BR-NO TO WR-BRNO.
           MOVE LAST-YR-YYYYMM TO WR-YYYYMM.
           PERFORM READ-WR1-FILE.
           IF IO-BAD
              PERFORM CLEAR-WR-FILE.

           MOVE WR-REC TO WRLY-REC.

           MOVE BR-NO TO WR-BRNO.
           MOVE PROCESS-YYYYMM TO WR-YYYYMM.
           PERFORM READ-WR1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE WRLM-CUR-NO-CUST TO WR-ELM-NO-CUST.
           MOVE WRLM-OUTS-BALANCE TO WR-ELM-OUTS-BALANCE.
           MOVE WRLM-AMT-90-DAY-ACCTS TO WR-ELM-AMT-90-DAY-ACCTS.
           MOVE WRLM-WCBC-CUR-NO-CUST TO WR-WCBC-ELM-NO-CUST.
           MOVE WRLM-WCBC-OUTS-BALANCE TO WR-WCBC-ELM-OUTS-BAL.
           MOVE WRLM-WCBC-AMT-90-DAY-ACCTS TO WR-WCBC-ELM-AMT-90-DAYS.
           MOVE WRLM-LARGE-OUTS-BALANCE TO WR-LARGE-ELM-OUTS-BAL.
           MOVE WRLM-LARGE-CUR-NO-CUST TO WR-LARGE-ELM-NO-CUST.
           MOVE WRLM-LARGE-AMT-90-DAY-ACCTS TO WR-LARGE-ELM-AMT-90-DAYS.

           MOVE WRLY-CUR-NO-CUST TO WR-ELY-NO-CUST.
           MOVE WRLY-OUTS-BALANCE TO WR-ELY-OUTS-BALANCE.
           MOVE WRLY-AMT-90-DAY-ACCTS TO WR-ELY-AMT-90-DAY-ACCTS.
           MOVE WRLY-WCBC-CUR-NO-CUST TO WR-WCBC-ELY-NO-CUST.
           MOVE WRLY-WCBC-OUTS-BALANCE TO WR-WCBC-ELY-OUTS-BAL.
           MOVE WRLY-WCBC-AMT-90-DAY-ACCTS TO WR-WCBC-ELY-AMT-90-DAYS.
           MOVE WRLY-LARGE-OUTS-BALANCE TO WR-LARGE-ELY-OUTS-BAL.
           MOVE WRLY-LARGE-CUR-NO-CUST TO WR-LARGE-ELY-NO-CUST.
           MOVE WRLY-LARGE-AMT-90-DAY-ACCTS TO WR-LARGE-ELY-AMT-90-DAYS.

       EXIT-UPDATE-FROM-LAST-MO.
           EXIT.

      **************************************************
       GET-CUR-MO-BY-FOR-INS SECTION.
      **************************************************
           MOVE BY1-KEY TO HOLD-BY1-KEY.
           MOVE CUR-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.
           PERFORM READ-BY1-FILE.
           IF IO-BAD
              GO TO EXIT-GET-CUR-MO.

           ADD BY-INS-NO-WROTE(1,1)   TO HOLD-MTD-CL.
           ADD BY-INS-NO-WROTE(2,1)   TO HOLD-YTD-CL.
           ADD BY-INS-AMT-WROTE(1,1)  TO HOLD-MTD-CL-AMT.
           ADD BY-INS-AMT-WROTE(2,1)  TO HOLD-YTD-CL-AMT.
           COMPUTE HOLD-MTD-CL-REF =     HOLD-MTD-CL-REF +
                                         BY-CLREB1(1) +
                                         BY-CLREB2(1) +
                                         BY-CLREB3(1) +
                                         BY-CLREB4(1).
           COMPUTE HOLD-YTD-CL-REF =     HOLD-YTD-CL-REF +
                                         BY-CLREB1(2) +
                                         BY-CLREB2(2) +
                                         BY-CLREB3(2) +
                                         BY-CLREB4(2).
           ADD BY-INS-NO-WROTE(1,2)   TO HOLD-MTD-AH.
           ADD BY-INS-NO-WROTE(2,2)   TO HOLD-YTD-AH.
           ADD BY-INS-AMT-WROTE(1,2)  TO HOLD-MTD-AH-AMT.
           ADD BY-INS-AMT-WROTE(2,2)  TO HOLD-YTD-AH-AMT.
           ADD BY-AHREB(1)            TO HOLD-MTD-AH-REF.
           ADD BY-AHREB(2)            TO HOLD-YTD-AH-REF.
           ADD BY-INS-NO-WROTE(1,3)   TO HOLD-MTD-PP.
           ADD BY-INS-NO-WROTE(2,3)   TO HOLD-YTD-PP.
           ADD BY-INS-AMT-WROTE(1,3)  TO HOLD-MTD-PP-AMT.
           ADD BY-INS-AMT-WROTE(2,3)  TO HOLD-YTD-PP-AMT.
           ADD BY-PPREB(1)            TO HOLD-MTD-PP-REF.
           ADD BY-PPREB(2)            TO HOLD-YTD-PP-REF.
           ADD BY-INS-NO-WROTE(1,4)   TO HOLD-MTD-O1.
           ADD BY-INS-NO-WROTE(2,4)   TO HOLD-YTD-O1.
           ADD BY-INS-AMT-WROTE(1,4)  TO HOLD-MTD-O1-AMT.
           ADD BY-INS-AMT-WROTE(2,4)  TO HOLD-YTD-O1-AMT.
           ADD BY-O1REB(1)            TO HOLD-MTD-O1-REF.
           ADD BY-O1REB(2)            TO HOLD-YTD-O1-REF.
           ADD BY-INS-NO-WROTE(1,5)   TO HOLD-MTD-O2.
           ADD BY-INS-NO-WROTE(2,5)   TO HOLD-YTD-O2.
           ADD BY-INS-AMT-WROTE(1,5)  TO HOLD-MTD-O2-AMT.
           ADD BY-INS-AMT-WROTE(2,5)  TO HOLD-YTD-O2-AMT.
           ADD BY-O2REB(1)            TO HOLD-MTD-O2-REF.
           ADD BY-O2REB(2)            TO HOLD-YTD-O2-REF.
           ADD BY-INS-NO-WROTE(1,6)   TO HOLD-MTD-O3.
           ADD BY-INS-NO-WROTE(2,6)   TO HOLD-YTD-O3.
           ADD BY-INS-AMT-WROTE(1,6)  TO HOLD-MTD-O3-AMT.
           ADD BY-INS-AMT-WROTE(2,6)  TO HOLD-YTD-O3-AMT.
           ADD BY-O3REB(1)            TO HOLD-MTD-O3-REF.
           ADD BY-O3REB(2)            TO HOLD-YTD-O3-REF.
           ADD BY-FEE-NO-WROTE(1,1)   TO HOLD-MTD-NON-FILE.
           ADD BY-FEE-NO-WROTE(2,1)   TO HOLD-YTD-NON-FILE.
           ADD BY-FEE-AMT-WROTE(1,1)  TO HOLD-MTD-NON-FILE-AMT.
           ADD BY-FEE-AMT-WROTE(2,1)  TO HOLD-YTD-NON-FILE-AMT.
           ADD BY-FEE-NO-WROTE(1,2)   TO HOLD-MTD-AD-D.
           ADD BY-FEE-NO-WROTE(2,2)   TO HOLD-YTD-AD-D.
           ADD BY-FEE-AMT-WROTE(1,2)  TO HOLD-MTD-AD-D-AMT.
           ADD BY-FEE-AMT-WROTE(2,2)  TO HOLD-YTD-AD-D-AMT.
           ADD BY-FEE-NO-WROTE(1,3)  TO HOLD-MTD-RDGARD.
           ADD BY-FEE-NO-WROTE(2,3)  TO HOLD-YTD-RDGARD.
           ADD BY-FEE-AMT-WROTE(1,3) TO HOLD-MTD-RDGARD-AMT.
           ADD BY-FEE-AMT-WROTE(2,3) TO HOLD-YTD-RDGARD-AMT.
           COMPUTE HOLD-MTD-NEW-LNS = HOLD-MTD-NEW-LNS
                                    + BY-NO (1,2,1)
                                    + BY-NO (1,2,2)
                                    + BY-NO (1,2,8)
                                    + BY-NO (1,2,9).
           COMPUTE HOLD-YTD-NEW-LNS = HOLD-YTD-NEW-LNS
                                    + BY-NO (2,2,1)
                                    + BY-NO (2,2,2)
                                    + BY-NO (2,2,8)
                                    + BY-NO (2,2,9).
           ADD BY-NO(1,2,3)         TO HOLD-MTD-FB-LNS.
           ADD BY-NO(2,2,3)         TO HOLD-YTD-FB-LNS.
           COMPUTE HOLD-MTD-PB-LNS  = HOLD-MTD-PB-LNS
                                    + BY-NO (1,2,4)
                                    + BY-NO (1,2,5)
                                    + BY-NO (1,2,6).
           COMPUTE HOLD-YTD-PB-LNS  = HOLD-YTD-PB-LNS
                                    + BY-NO (2,2,4)
                                    + BY-NO (2,2,5)
                                    + BY-NO (2,2,6).
           COMPUTE HOLD-MTD-VOL-AMT = HOLD-MTD-VOL-AMT
                                    + BY-AMT(1,2,1)
                                    + BY-AMT(1,2,2)
                                    + BY-AMT(1,2,3)
                                    + BY-AMT(1,2,4)
                                    + BY-AMT(1,2,5)
                                    + BY-AMT(1,2,6)
                                    + BY-AMT(1,2,7)
                                    + BY-AMT(1,2,8)
                                    + BY-AMT(1,2,9)
                                    + BY-AMT(1,2,10)
                                    + BY-AMT(1,2,11).
           COMPUTE HOLD-YTD-VOL-AMT = HOLD-YTD-VOL-AMT
                                 + BY-AMT(2,2,1)
                                    + BY-AMT(2,2,2)
                                    + BY-AMT(2,2,3)
                                    + BY-AMT(2,2,4)
                                    + BY-AMT(2,2,5)
                                    + BY-AMT(2,2,6)
                                    + BY-AMT(2,2,7)
                                    + BY-AMT(2,2,8)
                                    + BY-AMT(2,2,9)
                                    + BY-AMT(2,2,10)
                                    + BY-AMT(2,2,11).
           COMPUTE HOLD-MTD-PL      = HOLD-MTD-PL
                                    + BY-NO-PL(1,2,1)
                                    + BY-NO-PL(1,2,5)
                                    + BY-NO-PL(1,2,7)
                                    + BY-NO-PL(1,2,9).
           COMPUTE HOLD-YTD-PL      = HOLD-YTD-PL
                                    + BY-NO-PL(2,2,1)
                                    + BY-NO-PL(2,2,5)
                                    + BY-NO-PL(2,2,7)
                                    + BY-NO-PL(2,2,9).

      *    IF BY-CLASS > 4 AND BY-CLASS < 30
           IF BY-CLASS > 4 AND BY-CLASS < 30 AND BY-CLASS NOT = 22
              COMPUTE TOT-LARGE-NO-NB(1) =
                                         TOT-LARGE-NO-NB(1)
                                       + BY-NO (1,2,1)
                                       + BY-NO (1,2,2)
                                       + BY-NO (1,2,8)
                                       + BY-NO (1,2,9)
              COMPUTE TOT-LARGE-NO-NB(2) =
                                         TOT-LARGE-NO-NB(2)
                                       + BY-NO (2,2,1)
                                       + BY-NO (2,2,2)
                                       + BY-NO (2,2,8)
                                       + BY-NO (2,2,9)
              ADD BY-NO(1,2,3)         TO TOT-LARGE-NO-FB(1)
              ADD BY-NO(2,2,3)         TO TOT-LARGE-NO-FB(2)
              COMPUTE TOT-LARGE-NO-PB(1) =
                                         TOT-LARGE-NO-PB(1)
                                       + BY-NO (1,2,4)
                                       + BY-NO (1,2,5)
                                       + BY-NO (1,2,6)
              COMPUTE TOT-LARGE-NO-PB(2) =
                                         TOT-LARGE-NO-PB(2)
                                       + BY-NO (2,2,4)
                                       + BY-NO (2,2,5)
                                       + BY-NO (2,2,6)
              COMPUTE TOT-LARGE-GROSS-VOLUME(1) =
                                         TOT-LARGE-GROSS-VOLUME(1)
                                       + BY-AMT(1,2,1)
                                       + BY-AMT(1,2,2)
                                       + BY-AMT(1,2,3)
                                       + BY-AMT(1,2,4)
                                       + BY-AMT(1,2,5)
                                       + BY-AMT(1,2,6)
                                       + BY-AMT(1,2,7)
                                       + BY-AMT(1,2,8)
                                       + BY-AMT(1,2,9)
                                       + BY-AMT(1,2,10)
                                       + BY-AMT(1,2,11)
              COMPUTE TOT-LARGE-GROSS-VOLUME(2) =
                                         TOT-LARGE-GROSS-VOLUME(2)
                                       + BY-AMT(2,2,1)
                                       + BY-AMT(2,2,2)
                                       + BY-AMT(2,2,3)
                                       + BY-AMT(2,2,4)
                                       + BY-AMT(2,2,5)
                                       + BY-AMT(2,2,6)
                                       + BY-AMT(2,2,7)
                                       + BY-AMT(2,2,8)
                                       + BY-AMT(2,2,9)
                                       + BY-AMT(2,2,10)
                                       + BY-AMT(2,2,11).

       EXIT-GET-CUR-MO.
           MOVE HOLD-BY1-KEY TO BY1-KEY.
           PERFORM READ-BY1-FILE.

      **************************************************
       GET-1-YR-AGO-BY SECTION.
      **************************************************
           MOVE BY1-KEY TO HOLD-BY1-KEY.
           MOVE 1-YR-AGO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.
           PERFORM READ-BY1-FILE.
           IF IO-BAD
              GO TO EXIT-GET-1-YR-AGO-BY.
           IF BY-CLASS < 30 OR BY-CLASS = 50 *> 1350
              COMPUTE HOLD-NO-CUST-1-YR-AGO = HOLD-NO-CUST-1-YR-AGO
                                            + BY-NO(2,1,1)
                                            + BY-NO(2,1,2)
                                            + BY-NO(2,1,3)
                                            + BY-NO(2,1,4)
                                            + BY-NO(2,1,5)
                                            + BY-NO(2,1,6)
                                            + BY-NO(2,1,7)
                                            + BY-NO(2,1,8)
                                            + BY-NO(2,1,9)
                                            + BY-NO(2,1,10)
                                            + BY-NO(2,1,11)
              COMPUTE HOLD-OUTS-BAL-1-YR-AGO =
                                            HOLD-OUTS-BAL-1-YR-AGO
                                            + BY-AMT(2,1,1)
                                            + BY-AMT(2,1,2)
                                            + BY-AMT(2,1,3)
                                            + BY-AMT(2,1,4)
                                            + BY-AMT(2,1,5)
                                            + BY-AMT(2,1,6)
                                            + BY-AMT(2,1,7)
                                            + BY-AMT(2,1,8)
                                            + BY-AMT(2,1,9)
                                            + BY-AMT(2,1,10)
                                            + BY-AMT(2,1,11)
           ELSE
              COMPUTE HOLD-WCBC-NO-CUST-1-YR-AGO =
                                            HOLD-WCBC-NO-CUST-1-YR-AGO
                                            + BY-NO(2,1,1)
                                            + BY-NO(2,1,2)
                                            + BY-NO(2,1,3)
                                            + BY-NO(2,1,4)
                                            + BY-NO(2,1,5)
                                            + BY-NO(2,1,6)
                                            + BY-NO(2,1,7)
                                            + BY-NO(2,1,8)
                                            + BY-NO(2,1,9)
                                            + BY-NO(2,1,10)
                                            + BY-NO(2,1,11)
              COMPUTE HOLD-WCBC-OUTS-BAL-1-YR-AGO =
                                          HOLD-WCBC-OUTS-BAL-1-YR-AGO
                                            + BY-AMT(2,1,1)
                                            + BY-AMT(2,1,2)
                                            + BY-AMT(2,1,3)
                                            + BY-AMT(2,1,4)
                                            + BY-AMT(2,1,5)
                                            + BY-AMT(2,1,6)
                                            + BY-AMT(2,1,7)
                                            + BY-AMT(2,1,8)
                                            + BY-AMT(2,1,9)
                                            + BY-AMT(2,1,10)
                                            + BY-AMT(2,1,11).

           IF BY-CLASS > 4 AND BY-CLASS < 30
              COMPUTE HOLD-LARGE-NO-CUST-1-YR-AGO  =
                                        HOLD-LARGE-NO-CUST-1-YR-AGO
                                         + BY-NO(2,1,1)
                                         + BY-NO(2,1,2)
                                         + BY-NO(2,1,3)
                                         + BY-NO(2,1,4)
                                         + BY-NO(2,1,5)
                                         + BY-NO(2,1,6)
                                         + BY-NO(2,1,7)
                                         + BY-NO(2,1,8)
                                         + BY-NO(2,1,9)
                                         + BY-NO(2,1,10)
                                         + BY-NO(2,1,11)
           COMPUTE HOLD-LARGE-OUTS-BAL-1-YR-AGO =
                                        HOLD-LARGE-OUTS-BAL-1-YR-AGO
                                         + BY-AMT(2,1,1)
                                         + BY-AMT(2,1,2)
                                         + BY-AMT(2,1,3)
                                         + BY-AMT(2,1,4)
                                         + BY-AMT(2,1,5)
                                         + BY-AMT(2,1,6)
                                         + BY-AMT(2,1,7)
                                         + BY-AMT(2,1,8)
                                         + BY-AMT(2,1,9)
                                         + BY-AMT(2,1,10)
                                         + BY-AMT(2,1,11).

       EXIT-GET-1-YR-AGO-BY.
           MOVE HOLD-BY1-KEY TO BY1-KEY.
           PERFORM READ-BY1-FILE.

      **************************************************
       COMBINE-CLASSES SECTION.
      **************************************************

      * ONLY IN HERE IF CLASS = 1, SAVE THIS RECORD TO ADD MORE BY FIGURES
      * INTO.
           MOVE BY-REC TO WK-REC.

           MOVE NEXT-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.
           MOVE WK-BRNO                TO BY-BRNO.
           MOVE 2                      TO BY-CLASS.

           PERFORM START-BY1-FILE.
       COMBINE-NEXT-BY.
           PERFORM READ-BY1-FILE-NEXT.
           IF IO-FG = 0
              IF BY-DATE-CCYYMM  = WK-DATE-CCYYMM
                 IF BY-BRNO = WK-BRNO
                    IF (BY-CLASS = 2 OR BY-CLASS = 3 OR
                        BY-CLASS = 50 OR BY-CLASS = 4 OR
                        BY-CLASS = 22)
                       MOVE "N" TO LARGE-LOAN-FLAG
                       PERFORM ADD-CLASSES
                       GO TO COMBINE-NEXT-BY
                    END-IF
                    IF BY-CLASS < 30
                       MOVE "Y" TO LARGE-LOAN-FLAG
                       PERFORM ADD-CLASSES
                       GO TO COMBINE-NEXT-BY
                    ELSE
                    IF BY-CLASS < 50
                       GO TO COMBINE-NEXT-BY
                    ELSE IF BY-CLASS = 50
                       MOVE "N" TO LARGE-LOAN-FLAG
                       PERFORM ADD-CLASSES.


      * AT THIS POINT, THE WK-REC MAY HAVE 3 CLASSES ADDED INTO IT, NOW
      * NEED TO RESTART BY FILE AND FORCE THE WK-REC INTO BY-REC SO THE
      * ROUTINE THAT CREATES THE SMITH RECORD, USES THESE TOTALS
           MOVE NEXT-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.
           MOVE WK-BRNO                TO BY-BRNO.
           MOVE WK-CLASS               TO BY-CLASS.

      * GET THE BY FILE BACK TO WHERE IS WAS

           PERFORM START-BY1-FILE.
           PERFORM READ-BY1-FILE-NEXT.

      * PUT THE TOTALS INTO THE BY-REC
           MOVE WK-REC TO BY-REC.

      **************************************************
       COMBINE-SALES-CLASSES SECTION.
      **************************************************

      * ONLY IN HERE IF CLASS = 30, SAVE THIS RECORD TO ADD MORE BY FIGURES
      * INTO.
           MOVE BY-REC TO WK-REC.

           MOVE NEXT-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.
           MOVE WK-BRNO TO BY-BRNO.
           MOVE 40 TO BY-CLASS.
           PERFORM READ-BY1-FILE.
           IF IO-FG = 0
              PERFORM ADD-CLASSES.

      * AT THIS POINT, THE WK-REC MAY HAVE 2 CLASSES ADDED INTO IT, NOW
      * NEED TO RESTART BY FILE AND FORCE THE WK-REC INTO BY-REC SO THE
      * ROUTINE THAT CREATES THE SMITH RECORD, USES THESE TOTALS
           MOVE NEXT-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.
           MOVE WK-BRNO TO BY-BRNO.
           MOVE WK-CLASS TO BY-CLASS.
      * GET THE BY FILE BACK TO WHERE IS WAS

           PERFORM START-BY1-FILE.
           PERFORM READ-BY1-FILE-NEXT.
      * PUT THE TOTALS INTO THE BY-REC
           MOVE WK-REC TO BY-REC.

      **************************************************
       ADD-CLASSES SECTION.
      **************************************************

      *  ADD BY TO WK
      *  ACCUMULATES BY RECORDS INTO A HOLD RECORD WK-REC.

           PERFORM ADD-BY-PERIOD-TABLES
               VARYING PERIOD FROM 1 BY 1 UNTIL PERIOD > 3
                 AFTER ACTSUB FROM 1 BY 1 UNTIL ACTSUB > 2
                 AFTER TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 11.

           PERFORM ADD-BY-MTD-YTD-TABLE
              VARYING MTDYTD FROM 1 BY 1 UNTIL MTDYTD > 2.

           ADD BY-SUMWGTTM TO  WK-SUMWGTTM.
           ADD BY-SUMWGTAV TO  WK-SUMWGTAV.
           ADD BY-SUMWGTTA TO  WK-SUMWGTTA.
           ADD BY-IB-ACCRUED TO  WK-IB-ACCRUED.

           IF LARGE-LOAN-FLAG = "Y"
              COMPUTE TOT-LARGE-OUTS = TOT-LARGE-OUTS
                                  + BY-AMT(2,1,1) + BY-AMT(2,1,2)
                                  + BY-AMT(2,1,3) + BY-AMT(2,1,4)
                                  + BY-AMT(2,1,5) + BY-AMT(2,1,6)
                                  + BY-AMT(2,1,7) + BY-AMT(2,1,8)
                                  + BY-AMT(2,1,9) + BY-AMT(2,1,10)
                                  + BY-AMT(2,1,11).

           IF LARGE-LOAN-FLAG = "Y"
              COMPUTE TOT-LARGE-NO-90-DAY-ACCTS =
                      TOT-LARGE-NO-90-DAY-ACCTS
                                     + BY-UNITS(2,1,3)
                                     + BY-UNITS(2,1,4)
                                     + BY-UNITS(2,1,5)
                                     + BY-UNITS(2,1,6)
                                     + BY-UNITS(2,1,7)
              COMPUTE TOT-LARGE-90-PLUS = TOT-LARGE-90-PLUS
                                     + BY-OUTBAL(2,1,3)
                                     + BY-OUTBAL(2,1,4)
                                     + BY-OUTBAL(2,1,5)
                                     + BY-OUTBAL(2,1,6)
                                     + BY-OUTBAL(2,1,7)
              COMPUTE HOLD-LARGE-CUR-NO-CONT-SLOW =
                      HOLD-LARGE-CUR-NO-CONT-SLOW
                                     + BY-UNITS(2,1,1)
                                     + BY-UNITS(2,1,2)
                                     + BY-UNITS(2,1,3)
                                     + BY-UNITS(2,1,4)
                                     + BY-UNITS(2,1,5)
                                     + BY-UNITS(2,1,6)
                                     + BY-UNITS(2,1,7)
                                     + BY-UNITS(2,1,8)
                                     + BY-UNITS(2,1,9).

           IF LARGE-LOAN-FLAG = "Y"
              COMPUTE TOT-LARGE-CUR-NO-CUST =
                      TOT-LARGE-CUR-NO-CUST
                                     + BY-UNITS(2,1,1)
                                     + BY-UNITS(2,1,2)
                                     + BY-UNITS(2,1,3)
                                     + BY-UNITS(2,1,4)
                                     + BY-UNITS(2,1,5)
                                     + BY-UNITS(2,1,6)
                                     + BY-UNITS(2,1,7)
                                     + BY-UNITS(2,1,8)
                                     + BY-UNITS(2,1,9)
                                     + BY-UNITS(2,1,10).

           IF LARGE-LOAN-FLAG = "Y"
              ADD BY-UNITS(2,1,1) TO TOT-LARGE-NO-30-DAY-ACCTS
              ADD BY-OUTBAL(2,1,1) TO TOT-LARGE-AMT-30-DAY-ACCTS
              ADD BY-UNITS(2,1,2) TO TOT-LARGE-NO-60-DAY-ACCTS
              ADD BY-OUTBAL(2,1,2) TO TOT-LARGE-AMT-60-DAY-ACCTS.

           PERFORM GET-CUR-MO-BY-FOR-INS.
           PERFORM GET-1-YR-AGO-BY.

           GO TO EXIT-ADD-CLASSES.

      *----------------------------------------------------------------
      *    ACCUMULATE ACTIVITY.
      *----------------------------------------------------------------
       ADD-BY-PERIOD-TABLES.
           ADD BY-NO(PERIOD ACTSUB TYPSUB) TO
              WK-NO(PERIOD ACTSUB TYPSUB).
           ADD BY-AMT(PERIOD ACTSUB TYPSUB) TO
              WK-AMT(PERIOD ACTSUB TYPSUB).
           ADD BY-O2(PERIOD ACTSUB TYPSUB) TO
              WK-O2(PERIOD ACTSUB TYPSUB).
           ADD BY-INT(PERIOD ACTSUB TYPSUB) TO
              WK-INT(PERIOD ACTSUB TYPSUB).

      *----------------------------------------------------------------
      *    ACCUMULATE P&L AND DELINQUENCY ACTIVITY.
      *----------------------------------------------------------------
           IF TYPSUB < 11
              ADD BY-NO-PL(PERIOD ACTSUB TYPSUB) TO
                 WK-NO-PL(PERIOD ACTSUB TYPSUB)
              ADD BY-AMT-PL(PERIOD ACTSUB TYPSUB) TO
                 WK-AMT-PL(PERIOD ACTSUB TYPSUB)
              ADD BY-O2-PL(PERIOD ACTSUB TYPSUB) TO
                 WK-O2-PL(PERIOD ACTSUB TYPSUB)
              ADD BY-INT-PL(PERIOD ACTSUB TYPSUB) TO
                 WK-INT-PL(PERIOD ACTSUB TYPSUB)
      *       ACCUM DELINQ.
              ADD BY-UNITS(PERIOD ACTSUB TYPSUB) TO
                 WK-UNITS(PERIOD ACTSUB TYPSUB)
              ADD BY-OUTBAL(PERIOD ACTSUB TYPSUB) TO
                 WK-OUTBAL(PERIOD ACTSUB TYPSUB).

      *----------------------------------------------------------------
      *    ACCUMULATE REPO ACTIVITY.
      *----------------------------------------------------------------
           IF TYPSUB < 6
              ADD BY-NO-REPO(PERIOD ACTSUB TYPSUB) TO
                 WK-NO-REPO(PERIOD ACTSUB TYPSUB)
              ADD BY-AMT-REPO(PERIOD ACTSUB TYPSUB) TO
                 WK-AMT-REPO(PERIOD ACTSUB TYPSUB)
              ADD BY-O2-REPO(PERIOD ACTSUB TYPSUB) TO
                 WK-O2-REPO(PERIOD ACTSUB TYPSUB)
              ADD BY-INT-REPO(PERIOD ACTSUB TYPSUB) TO
                 WK-INT-REPO(PERIOD ACTSUB TYPSUB).
      *----------------------------------------------------------------
      *    ACCUMULATE DEALER RESERVES.
      *----------------------------------------------------------------
           IF TYPSUB < 4
              ADD BY-DLRVPRIN(PERIOD ACTSUB TYPSUB) TO
                 WK-DLRVPRIN(PERIOD ACTSUB TYPSUB)
              ADD BY-DLRVCHGS(PERIOD ACTSUB TYPSUB) TO
                 WK-DLRVCHGS(PERIOD ACTSUB TYPSUB)
              IF TYPSUB = 1
                 ADD BY-DLPARVHELD(PERIOD ACTSUB) TO
                    WK-DLPARVHELD(PERIOD ACTSUB)
                 IF ACTSUB = 2
                    ADD BY-DLDISC(PERIOD) TO WK-DLDISC(PERIOD).

      *-----------------------------------------------------------------
       ADD-BY-MTD-YTD-TABLE.
           PERFORM VARYING PLSUB FROM 1 BY 1 UNTIL PLSUB > 5
              ADD BY-PL-PRINCOL(MTDYTD PLSUB)
                               TO  WK-PL-PRINCOL(MTDYTD PLSUB)
              ADD BY-PL-INTCOL(MTDYTD PLSUB)
                               TO  WK-PL-INTCOL(MTDYTD PLSUB)
              ADD BY-PL-LATECOL(MTDYTD PLSUB)
                               TO  WK-PL-LATECOL(MTDYTD PLSUB)
              ADD BY-PL-O2COL(MTDYTD PLSUB)
                               TO  WK-PL-O2COL(MTDYTD PLSUB)
           END-PERFORM.

              ADD BY-AMT-SOLD(MTDYTD)  TO  WK-AMT-SOLD(MTDYTD).
              ADD BY-AMT-TRANS(MTDYTD) TO  WK-AMT-TRANS(MTDYTD).
              ADD BY-AMT-PURCH(MTDYTD) TO  WK-AMT-PURCH(MTDYTD).

              ADD BY-NO-SOLD(MTDYTD)  TO  WK-NO-SOLD(MTDYTD).
              ADD BY-NO-TRANS(MTDYTD) TO  WK-NO-TRANS(MTDYTD).
              ADD BY-NO-PURCH(MTDYTD) TO  WK-NO-PURCH(MTDYTD).

              ADD BY-OTHCOL(MTDYTD)  TO WK-OTHCOL(MTDYTD).
              ADD BY-OT2COL(MTDYTD)  TO WK-OT2COL(MTDYTD).
              ADD BY-INTCOL(MTDYTD)  TO WK-INTCOL(MTDYTD).
              ADD BY-LATECOL(MTDYTD) TO WK-LATECOL(MTDYTD).
              ADD BY-PRINCOL(MTDYTD) TO WK-PRINCOL(MTDYTD).
              ADD BY-DEFCOL(MTDYTD)  TO WK-DEFCOL(MTDYTD).
              ADD BY-RNPRIN(MTDYTD)  TO WK-RNPRIN(MTDYTD).
              ADD BY-INTREB(MTDYTD)  TO WK-INTREB(MTDYTD).
              ADD BY-CLREB1(MTDYTD)  TO WK-CLREB1(MTDYTD).
              ADD BY-CLREB2(MTDYTD)  TO WK-CLREB2(MTDYTD).
              ADD BY-CLREB3(MTDYTD)  TO WK-CLREB3(MTDYTD).
              ADD BY-CLREB4(MTDYTD)  TO WK-CLREB4(MTDYTD).
              ADD BY-AHREB(MTDYTD)   TO WK-AHREB(MTDYTD).
              ADD BY-PPREB(MTDYTD)   TO WK-PPREB(MTDYTD).
              ADD BY-O1REB(MTDYTD)   TO WK-O1REB(MTDYTD).
              ADD BY-O2REB(MTDYTD)   TO WK-O2REB(MTDYTD).
              ADD BY-O3REB(MTDYTD)   TO WK-O3REB(MTDYTD).
              ADD BY-O4REB(MTDYTD)   TO WK-O4REB(MTDYTD).
              ADD BY-O5REB(MTDYTD)   TO WK-O5REB(MTDYTD).
              ADD BY-O6REB(MTDYTD)   TO WK-O6REB(MTDYTD).
              ADD BY-O7REB(MTDYTD)   TO WK-O7REB(MTDYTD).
              ADD BY-SCREB(MTDYTD)   TO WK-SCREB(MTDYTD).
              ADD BY-MFREB(MTDYTD)   TO WK-MFREB(MTDYTD).
              ADD BY-DFREB(MTDYTD)   TO WK-DFREB(MTDYTD).
              ADD BY-PART-CHGOFF(MTDYTD)   TO WK-PART-CHGOFF(MTDYTD).
              ADD BY-ALLOTMENTS(MTDYTD) TO WK-ALLOTMENTS(MTDYTD).

              PERFORM VARYING INSUR FROM 1 BY 1 UNTIL INSUR > 10
                 ADD BY-INS-NO-ELIG(MTDYTD INSUR)
                                   TO WK-INS-NO-ELIG(MTDYTD INSUR)
                 ADD BY-INS-NO-WROTE(MTDYTD INSUR)
                                   TO WK-INS-NO-WROTE(MTDYTD INSUR)
                 ADD BY-INS-NO-NOTWROTE(MTDYTD INSUR)
                                   TO WK-INS-NO-NOTWROTE(MTDYTD INSUR)
                 ADD BY-INS-AMT-WROTE(MTDYTD INSUR)
                                   TO WK-INS-AMT-WROTE(MTDYTD INSUR)
              END-PERFORM.

              PERFORM VARYING INSUR FROM 1 BY 1 UNTIL INSUR > 3
                 ADD BY-FEE-NO-ELIG(MTDYTD INSUR)
                                   TO WK-FEE-NO-ELIG(MTDYTD INSUR)
                 ADD BY-FEE-NO-WROTE(MTDYTD INSUR)
                                   TO WK-FEE-NO-WROTE(MTDYTD INSUR)
                 ADD BY-FEE-NO-NOTWROTE(MTDYTD INSUR)
                                   TO WK-FEE-NO-NOTWROTE(MTDYTD INSUR)
                 ADD BY-FEE-AMT-WROTE(MTDYTD INSUR)
                                   TO WK-FEE-AMT-WROTE(MTDYTD INSUR)
              END-PERFORM.

              ADD BY-OTH-NO-COL(MTDYTD)  TO WK-OTH-NO-COL(MTDYTD).
              ADD BY-OT2-NO-COL(MTDYTD)  TO WK-OT2-NO-COL(MTDYTD).
              ADD BY-INT-NO-COL(MTDYTD)  TO WK-INT-NO-COL(MTDYTD).
              ADD BY-LATE-NO-COL(MTDYTD) TO WK-LATE-NO-COL(MTDYTD).

              ADD BY-OTH-NO-WAIVED(MTDYTD)
                                         TO WK-OTH-NO-WAIVED(MTDYTD).
              ADD BY-OT2-NO-WAIVED(MTDYTD)
                                         TO WK-OT2-NO-WAIVED(MTDYTD).
              ADD BY-INT-NO-WAIVED(MTDYTD)
                                         TO WK-INT-NO-WAIVED(MTDYTD).
              ADD BY-LATE-NO-WAIVED(MTDYTD)
                                         TO WK-LATE-NO-WAIVED(MTDYTD).

              ADD BY-OTH-AMT-WAIVED(MTDYTD)
                                         TO WK-OTH-AMT-WAIVED(MTDYTD).
              ADD BY-OT2-AMT-WAIVED(MTDYTD)
                                         TO WK-OT2-AMT-WAIVED(MTDYTD).
              ADD BY-INT-AMT-WAIVED(MTDYTD)
                                         TO WK-INT-AMT-WAIVED(MTDYTD).
              ADD BY-LATE-AMT-WAIVED(MTDYTD)
                                         TO WK-LATE-AMT-WAIVED(MTDYTD).
       EXIT-ADD-CLASSES.
              EXIT.

      **************************************************
       CHECK-CLASS-1-EXISTS SECTION.
      **************************************************

      * SINCE ALL NON-GIFTNET CLASSES ARE COMBINED INTO A CLASS 1
      * WORK RECORD, MAKE SURE ALL BRANCHES HAVE A CLASS 1 RECORD,
      * IF NOT, CREATE EMPTY ONE

           MOVE NEXT-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.
           MOVE WGR-SRESULT TO BY-BRNO.
           MOVE 1 TO BY-CLASS.
           PERFORM READ-BY1-FILE.
           IF IO-FG = 0
              GO TO EXIT-CHECK-CLASS-1.
           PERFORM CLEAR-BY-FILE.
           MOVE NEXT-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.
           MOVE WGR-SRESULT TO BY-BRNO.
           MOVE 1 TO BY-CLASS.
           PERFORM WRITE-BY1-FILE.
           MOVE CUR-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.
           MOVE WGR-SRESULT TO BY-BRNO.
           MOVE 1 TO BY-CLASS.
           PERFORM READ-BY1-FILE.
           IF IO-FG = 0
              GO TO EXIT-CHECK-CLASS-1.
           PERFORM CLEAR-BY-FILE.
           MOVE CUR-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.
           MOVE WGR-SRESULT TO BY-BRNO.
           MOVE 1 TO BY-CLASS.
           PERFORM WRITE-BY1-FILE.
       EXIT-CHECK-CLASS-1.
           EXIT.

      **************************************************
       WRITE-DETAIL-LINE SECTION.
      **************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      **************************************************
       HEAD-PAGE SECTION.
      **************************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 2 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      **************************************************
       PRINT-RANGE-LINES SECTION.
      **************************************************

           MOVE "TOTAL WABD (BAD DEBT)" TO RANGE-SELECTION.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  115 - OUTSTANDING " TO RANGE-SELECTION.
           MOVE TOT-WABD-115 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  115 - OUTSTANDING LARGE LOANS" TO RANGE-SELECTION.
           MOVE TOT-WLBD-115 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  120 - 90+, 180+   " TO RANGE-SELECTION.
           MOVE TOT-WABD-120 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  120 - 90+, 180+ LARGE LOANS " TO RANGE-SELECTION.
           MOVE TOT-WLBD-120 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL WARS (RATING SHEET)" TO RANGE-SELECTION.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  01 - LOANS OUTSTD " TO RANGE-SELECTION.
           MOVE TOT-WARS-01 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  05 - NB, RF, OT   " TO RANGE-SELECTION.
           MOVE TOT-WARS-05 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  06 - FB  LOANS    " TO RANGE-SELECTION.
           MOVE TOT-WARS-06 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  07 - PB  LOANS    " TO RANGE-SELECTION.
           MOVE TOT-WARS-07 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  09 - LOAN VOLUME  " TO RANGE-SELECTION.
           MOVE TOT-WARS-09 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  34 - CHARGE-OFFS  " TO RANGE-SELECTION.
           MOVE TOT-WARS-34 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  45 - 30DAY RECY   " TO RANGE-SELECTION.
           MOVE TOT-WARS-45 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  46 - AMT 30DAY REC" TO RANGE-SELECTION.
           MOVE TOT-WARS-46 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  48 - 60DAY RECY  " TO RANGE-SELECTION.
           MOVE TOT-WARS-48 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  49 - AMT 60DAY REC" TO RANGE-SELECTION.
           MOVE TOT-WARS-49 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  51 - 90DAY+ RECY" TO RANGE-SELECTION.
           MOVE TOT-WARS-51 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  52 - AMT 90DAY+REC" TO RANGE-SELECTION.
           MOVE TOT-WARS-52 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL WAIS (INSURANCE)" TO RANGE-SELECTION.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  508 - LIFE          " TO RANGE-SELECTION.
           MOVE TOT-WAIS-508 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  509 - A&H           " TO RANGE-SELECTION.
           MOVE TOT-WAIS-509 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  525 - PROP          " TO RANGE-SELECTION.
           MOVE TOT-WAIS-525 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  526 - AUTO          " TO RANGE-SELECTION.
           MOVE TOT-WAIS-526 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  455 - IUI           " TO RANGE-SELECTION.
           MOVE TOT-WAIS-455 TO RANGE2
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  457 - AD&D          " TO RANGE-SELECTION.
           MOVE TOT-WAIS-457 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  462 - ROADGARD      " TO RANGE-SELECTION.
           MOVE TOT-WAIS-462 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  469 - LIENHOLDER    " TO RANGE-SELECTION.
           MOVE TOT-WAIS-469 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL WGBD (BAD DEBT)" TO RANGE-SELECTION.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  115 - OUTSTANDING " TO RANGE-SELECTION.
           MOVE TOT-WGBD-115 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  120 - 90+, 180+   " TO RANGE-SELECTION.
           MOVE TOT-WGBD-120 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL WAGN (RATING SHEET)" TO RANGE-SELECTION.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  02 - LOANS OUTSTD " TO RANGE-SELECTION.
           MOVE TOT-WAGN-01 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  06 - NB, RF, OT   " TO RANGE-SELECTION.
           MOVE TOT-WAGN-05 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  07 - FB  LOANS    " TO RANGE-SELECTION.
           MOVE TOT-WAGN-06 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  08 - PB  LOANS    " TO RANGE-SELECTION.
           MOVE TOT-WAGN-07 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  11 - LOAN VOLUME  " TO RANGE-SELECTION.
           MOVE TOT-WAGN-09 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  34 - CHARGE-OFFS  " TO RANGE-SELECTION.
           MOVE TOT-WAGN-34 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  39 - 30DAY RECY   " TO RANGE-SELECTION.
           MOVE TOT-WAGN-45 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  40 - AMT 30DAY REC" TO RANGE-SELECTION.
           MOVE TOT-WAGN-46 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  41 - 60DAY RECY  " TO RANGE-SELECTION.
           MOVE TOT-WAGN-48 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  42 - AMT 60DAY REC" TO RANGE-SELECTION.
           MOVE TOT-WAGN-49 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  43 - 90DAY+ RECY" TO RANGE-SELECTION.
           MOVE TOT-WAGN-51 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  44 - AMT 90DAY+REC" TO RANGE-SELECTION.
           MOVE TOT-WAGN-52 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL WAGI (INSURANCE)" TO RANGE-SELECTION.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  508 - LIFE          " TO RANGE-SELECTION.
           MOVE TOT-WAGI-508 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  509 - A&H           " TO RANGE-SELECTION.
           MOVE TOT-WAGI-509 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  525 - PROP          " TO RANGE-SELECTION.
           MOVE TOT-WAGI-525 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  526 - AUTO          " TO RANGE-SELECTION.
           MOVE TOT-WAGI-526 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  455 - IUI           " TO RANGE-SELECTION.
           MOVE TOT-WAGI-455 TO RANGE2
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  457 - AD&D          " TO RANGE-SELECTION.
           MOVE TOT-WAGI-457 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  462 - ROADGARD      " TO RANGE-SELECTION.
           MOVE TOT-WAGI-462 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  469 - LIENHOLDER    " TO RANGE-SELECTION.
           MOVE TOT-WAGI-469 TO RANGE2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS" TO RANGE-SELECTION.
           MOVE TOT-RECORDS TO RANGE1N.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      **************************************************
       LOG-CHANGES SECTION.
      **************************************************

      * CODE TO WRITE CHANGE FILE.
           MOVE "W"              TO ERRLOGW-ACTION.

           MOVE LOG-BRNO         TO ERRLOGW-BRNO.
           MOVE LOG-MESS         TO ERRLOGW-EOM-MSG.
           MOVE FORM-PATHNAME    TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBLP/LPBYCL.CPY".
       COPY "LIBSP/SPWRCL.CPY".


      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO BYRSUP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BYRSUP-SYSDATE.
           DISPLAY BYRSUP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BYRSUP-FORM-FIELDS TO WR-BUF.
           MOVE BYRSUP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE BYRSUP-HELP--FILE TO HELP-LINK-FILE.
           MOVE BYRSUP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/BYRSUP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************************************
       MISC-ROUTINES SECTION.
      *********************************************************
       COPY "LIBGB/SYSTEM.CPY".
      * UNITS 2,1 = BEG-MO RECENCY, OUTBAL 3,1 = ELM RECENCY
       COMPUTE-WS-UNITS.
           COMPUTE WS-UNITS = WS-UNITS +  BY-NO(2,1,K).
       COMPUTE-WS-UNITS-2.
           COMPUTE WS-UNITS = WS-UNITS +  BY-UNITS(2,1,K).
       COMPUTE-WS-OUTBAL.
           COMPUTE WS-OUTBAL = WS-OUTBAL +  BY-AMT(2,1,K).
       COMPUTE-WS-OUTBAL-3.
           COMPUTE WS-OUTBAL = WS-OUTBAL +  BY-OUTBAL(3,1,K).
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".

      **************************************************
       IO-ROUTINE SECTION.
      **************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPWRGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBYGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPBY1IN.CPY".
       COPY "LIBSP/SPWR1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      **************************************************
       END-PROGRAM SECTION.
      **************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/WRLDMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BYRSUP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
