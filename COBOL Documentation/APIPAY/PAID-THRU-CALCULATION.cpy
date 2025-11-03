      *================================================================*
      * END COPYBOOK: LIBLP\LPLCAS.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBGB\SPPDTH.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBGB/SPPDTH
      ************************************************************************
      *         CONTRACTUAL PAID THRU CALCULATION
      *
      *   NAME: SPPDTH
      *   DESC: ROUTINE COMPUTES CONTRACTUAL PAID THRU
      *         TAKING INTO ACCOUNT ODD PAYMENT DUE,
      *         AND OPTIONAL PAYMENT SCHEDULE,
      *         AND DEFERMENTS + ALLOWABLE DELINQUENCY.
      *
      *      N O T E:
      *              IF PDTH-DEF-FG NOT = " " THE ROUTINE WILL
      *                 N O T  INCLUDE LN-TOTNODEF IN THE CALCULATION.
      *
      *              IF PDTH-ALDEL-FG NOT = " " THE ROUTINE WILL
      *                 N O T  INCLUDE LN-TOTNOALDEL IN THE CALCULATION.
      *
      *
      *   IN  : PDTH-DATE-WORK      LN-1STPYDATE
      *         PDTH-DEF-FG         ---> OPTIONAL
      *         PDTH-ALDEL-FG       ---> OPTIONAL
      *   OUT : PDTH-DATE           (MMYY) OR (MMDD)
      *         PDTH-LCPDTHRU
      *         PDTH-DATE-FULL      (MMDDYY)
      *         PDTH-PAYS           NO OF FULL PAYMENTS MADE
      *         PDTH-REMAIN         DECIMAL PORTION OF NO. OF PAYMENTS MADE
      *         PDTH-AMT-TO-ADVANCE THE AMOUNT NEEDED TO ADVANCE PDTH
      *
      *         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *         S E E  EXAMPLE THAT FOLLOWS
      *         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *         PDTH-CUR-SCHD-PAY   RETURNS NEXT SCHEDULED PAYMENT AMOUNT
      *                             AND IF PAYMENT IS PARTIALLY PAID,
      *                             IT IS RETURNED
      *         PDTH-CUR-SCHD-DATE  RETURNS PDTH-CUR-SCHD-PAY PAYMENT DUE DATE
      *
      *         PDTH-NXT-SCHD-PAY   RETURNS NEXT SCHEDULED PAYMENT AMOUNT
      *                             AND IF PAYMENT IS PARTIALLY PAID,
      *                             IT IS SKIPPED
      *         PDTH-NXT-SCHD-DATE  RETURNS PDTH-NXT-SCHD-PAY DUE DATE
      *         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *         LOAN DATE      :  06/01/03
      *         FIRST PAY DATE :  07/01/03
      *         LOAN AMOUNT    :              $1200.00
      *         FINANCE CHARGE :                200.00
      *         PROCEEDS       :               1000.00
      *                                        -------
      *         SCHEDULE OF PAY:  07/01/03      160.00
      *                           08/01/03      170.00
      *                           09/01/03      180.00
      *                           10/01/03      190.00
      *                           11/01/03      200.00
      *                           12/01/03      300.00
      *                                        -------
      *                           TOTAL PAYS   1200.00
      *
      *         LN-TOTPAYMNTD     :  $150.00**
      *
      *         SP-CONTPDTH %     :  100%                 93%
      *   NEEDED TO ADVANCE PDTH  :  $160.00 @ 100%       $148.80 @ 93%
      *
      *         PDTH-DATE-FULL    :  06/01/03             07/01/03
      *
      *         PDTH-CUR-SCHD-PAY :  $160.00              $160.00
      *         PDTH-CUR-SCHD-DATE:  07/01/03             08/01/03
      *
      *         PDTH-NXT-SCHD-PAY :  $170.00              $170.00
      *         PDTH-NXT-SCHD-DATE:  08/01/03             08/01/03
      *
      *
      *   COPY: SPPDTHW, LPNDTE, LPNDTEW, INCRMM
      * REV :
      *  JTG 040590 ALLOWED LN-TOTNOALDEL TO BE INCLUDED IN CALCULATING
      *             PAID THRU
      *  JTG 080690 ADDED LOGIC FOR ADDON INSURANCE
      *  SLC 090690 ADDED PDTH-CUR-SCHD-PAY CURRENT SCHEDULED PAYMENT.
      *  BAH 040991 IF PAYMNTD > ODD, PAID THRU WAS NOT ADVANCING
      *  BAH 021992 IF PAYMNTD = 1STPYAMT, 1STPYAMT STILL GETTING SET
      *             IN PDTH-CUR-SCHD-PAY
      *  JTG 070292 CHANGED TO SET PDTH-NXT-SCHD-PAY,
      *                            PDTH-NXT-SCHD-DATE, RE: DEFERMENT TN ADDONS
      *  JTG 072192 ALLOWED RETURN OF PDTH-PAYS AND PDTH-REMAIN
      *  JTG 100992 CORRECTED SET NDTE-HOLD
      *  JTG 030293 CHANGED PAID-THRU-CALC-ADDON SECTION
      *                              AND PAID-THRU-ADDON-SUM SECTION
      *             TO WORK FOR LT-SCHLD-TERM OCCURING 17 TIMES
      *  JTG 032892 ADDED LOGIC FOR UNIT PERIODS
      *  JTG 052493 ADDED LOGIC FOR BALLOONS
      *  JTG 092993 FIXED PROBLEM WITH NORMAL ROUTINE RETURNING PDTH-PAYMENTS
      *  JTG 090294 FIXED PROBLEM WITH NORMAL ROUTINE, WHEN ACCOUNT HAS PAID
      *             MORE THAN THE 1ST AND REG PAYMENTS, RETURNING PDTH-PAYMENTS
      *  JTG 092394 FIXED PROBLEM WITH NORMAL ROUTINE, WHEN ACCOUNT HAS PAID
      *             MORE THAN THE 1ST AND REG PAYMENTS, RETURNING PDTH-PAID THRU
      *             WHICH GOT SCREWED UP RE: FIX ON 090294
      *  JTG 082995 ADDED LOGIC FOR PDTH-DEF-FG, MERCURY SP-ERNFRMLA 61
      *  JTG 041596 ADDED PDTH-CUR-SCHD-DATE
      *  BAH 052797 DISPLAY ACCTNO BEFORE IO-ERROR ON GET-LT-P-REC
      *  JTG 101598 MODIFIED TO WORK FOR REVOLVING ACCOUNTS, REGENCY REVOL
      *  JTG 990124 ADDED COPY STATEMENT FOR LIBLP/RVPMTDUE/W
      *  JTG 000329 ADDED GP-PDTH-GT-SCHED-PAY CHANGED PAID-THRU-CALC-NORMAL
      *             TO NOT EXCEED NO. OF SCHEDULED PAYMENTS      MERCURY #J001
      *  JTG 021111 ADDED PDTH-AMT-TO-ADVANCE                     REGACC #0008
      *  JTG 040517 CORRECTED CALCULATION OF PDTH-AMT-TO-ADVANCE.
      *             WHEN SP-CONTPDTH = 100% (WHICH INDICATES A FULL PAYMENT
      *             MUST BE RECEIVED), DON'T NEED TO ADD 0.01
      *             SINCE PDTH-CUR-SCHD-PAY * (100/100 - PDTH-REMAIN)
      *             YIELDS THE AMOUNT TO PAY A FULL PAYMENT
      *                                                   REGACC LENDMARK #J008
      *  MJD 090219 TESTING CHANGES FOR A NEW BANKRULE ******************
      *  BAH 160113 SPLIT OUT LTFILE, LTPFILE, RENAMED LT- TO LTP-, PD0003
      *  BLM 160615 FIXED PAID-THRU-CALCULATION-REVOL TO CALL GETLTR SINCE
      *             REVOLVING FIELDS ARE IN LTRFILE, NOT LNFILE, PD0003
      * KEC 2016.1116 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED LTR-SEQNO TO LTR1-KEY.
      * BLM 170309 FIX FOR NEW FORMATS OF PDTH-WK-DATE (CCYYMMDD), 
      *            PDTH-DATE(CCYYMM), PD0006
      * BAH 170317 CHANGED PDTH-DATE (CCYYMM) BACK TO YYMM
      * BAH 190211 REMOVED REVOLVING
      * BAH 2024.0321 REMOVED PDTH-CREDIT, NOT USED
      **************************************************************************
       PAID-THRU-CALCULATION SECTION.
           MOVE 0 TO PDTH-DATE           PDTH-DATE-FULL
                     PDTH-LCPDTHRU       PDTH-PAYMENTS
                     PDTH-AMT-TO-ADVANCE
                     PDTH-CUR-SCHD-PAY   PDTH-CUR-SCHD-DATE
                     PDTH-NXT-SCHD-PAY   PDTH-NXT-SCHD-DATE
                     NDTE-HOLD.

           MOVE LN-TOTPAYMNTD TO PDTH-PAYMNTD.

           IF LN-PAY-SCHLD-FG = "Y"
              PERFORM PAID-THRU-CALC-ADDON
           ELSE
              IF PDTH-E-OPTION = "E"
                 PERFORM PAID-THRU-CALC-NORMAL-E
              ELSE
                 PERFORM PAID-THRU-CALC-NORMAL.

           IF PDTH-DEF-FG = " "
              ADD LN-TOTNODEF   TO NDTE-HOLD.
           IF PDTH-ALDEL-FG = " "
              ADD LN-TOTNOALDEL TO NDTE-HOLD.
           COMPUTE NDTE-HOLD ROUNDED = NDTE-HOLD - 1 + PDTH-PAYS.

           COMPUTE PDTH-LCPDTHRU ROUNDED = SP-CONTPDTH / 100.
           IF PDTH-REMAIN > PDTH-LCPDTHRU
              ADD 1 TO NDTE-HOLD.
           MOVE PDTH-DATE-WORK  TO NDTE-DATE.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE       TO PDTH-DATE-FULL.

      * DETERMINE AMOUNT NEEDED TO ADVANCE PDTH
      
           IF PDTH-REMAIN > PDTH-LCPDTHRU
              COMPUTE PDTH-AMT-TO-ADVANCE =
                 (1 - PDTH-REMAIN) * PDTH-CUR-SCHD-PAY
                    + PDTH-NXT-SCHD-PAY * SP-CONTPDTH / 100
              ADD 0.01 TO PDTH-AMT-TO-ADVANCE
           ELSE
              COMPUTE PDTH-AMT-TO-ADVANCE =
                 PDTH-CUR-SCHD-PAY
                     * (SP-CONTPDTH / 100 - PDTH-REMAIN)
              IF SP-CONTPDTH < 100
                 ADD 0.01 TO PDTH-AMT-TO-ADVANCE.
    
           MOVE NDTE-MM         TO PDTH-MM.
           IF LN-UNITPER-CD-DAYS
              MOVE NDTE-DD      TO PDTH-DDYY
           ELSE
              MOVE NDTE-YY      TO PDTH-YY.

           MOVE 1               TO NDTE-HOLD.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE       TO PDTH-CUR-SCHD-DATE
                                   PDTH-NXT-SCHD-DATE.
           IF PDTH-REMAIN NOT > PDTH-LCPDTHRU
              IF PDTH-REMAIN > 0
                 MOVE 1               TO NDTE-HOLD
                 MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD
                 MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
                 PERFORM INCREMENT-UNITPER
                 MOVE NDTE-DATE       TO PDTH-NXT-SCHD-DATE.

       PAID-THRU-CALCULATION-EXIT.
           MOVE " " TO PDTH-DEF-FG
                       PDTH-ALDEL-FG
                       PDTH-E-OPTION.

      *---------------------------------------------------------
      *  EXAMPLE:
      *                       TERM = 1     TERM = 2     TERM = 6
      *                     -----------  -----------  -----------
      *   LN-1STPYAMT    :  $100.00      $100.00      $100.00
      *   LN-REGPYAMT    :  $100.00 X 0  $120.00 X 0  $120.00 X 4
      *   LN-LASTPYAMT   :  $  0.00      $120.00      $130.00
      *                      ------       ------       ------
      *      TOTAL REPAY :  $100.00      $220.00      $710.00
      *---------------------------------------------------------
