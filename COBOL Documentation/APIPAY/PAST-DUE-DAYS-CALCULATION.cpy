      *================================================================*
      * END COPYBOOK: LIBLP\LPUPER.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPPDUE.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPPDUE
      ***********************************************************************
      *          PAST DUE DAYS CALCULATION
      *
      *    NAME: LPPDUE
      *    DESC: ROUTINE COMPUTES DAYS PAST DUE FROM
      *          PAID THRU DATE
      *    IN  : PDUE-TODAY-DATE        (TODAYS DATE)
      *          PDUE-1STPYDATE    (LN-1STPYDATE)
      *    OUT : PDUE-DAYS
      *    USED: LN-CURBAL, LN-PAYFREQ
      *    COPY: LPPDUEW, LPMDTEW, LPMDTE, SPPDTHW, SPPDTH
      * REV:
      *  050986 JTG TOOK OUT CHECK ON PAST MATURITY
      *             WHICH CAUSED INPROPER VIEW OF
      *             PAST DUE DAYS EVEN THOUGH ALL
      *             PAYMENTS BECOME DUE THEN.
      *  041289 SLC ADDED CODE TO ADJUST THE DUE DATE
      *             TO AVOID MONTHEND PROBLEMS.
      *  041693 JTG ADDED LOGIC FOR UNIT PERIODS
      *  040495 JTG CORRECT LOGIC FOR FEB OVER 28TH
      *  JTG 030307 PUT BACK LOGIC FOR TESTING MATURITY:
      *             WHEN PAID-THRU IS EQUAL TO MATURITY AND THE ACCOUNT
      *             STILL HAS A BALANCE, COUNT PAST DUE DAYS FROM MATURITY
      *             AND NOT FROM NEXT DUE DATE                  CREATIVE #398
      *  JTG 030313 CORRECTED CHANGE DONE ON 030307 TO EXAMINE
      *             WHEN GP-PDTH-GT-SCHED-PAY = 'Y'
      *             PAID THRU IS CAPPED AT THE NUMBER OF SCHEDULED PAYS
      *             SO SEE IF PAID-THRU IS AT THE CAP (MATURITY)
      *             AND COUNT PAST DUE DAYS FROM MATURITY
      *             NOT FROM MATURITY + 1 PAY FREQUENCY         CREATIVE #398
      *   CS 130327 PAST DUE DAYS WERE DISPLAYING AS 1 ON 3/14 WHEN
      *             PDTH-DATE-FULL 2/28 & INCREMENT-UNITPER INCREMENTED TO
      *             3/13 (1STPYDA 30) SHOULD BE 3/15. FORCED IF PAID THRU
      *             2/28 SUBTRACT NDTE-DD (28) FROM LN-1STPYDA =2, ADD 2 TO
      *             THE INCORRECT 3/13/ DATE YIELDING 3/15 AND PDUE-DAYS SHOULD
      *             BE 0   SOAUTO PL#793
      *   CS 160318 FIXED BUG IN ABOVE, THIS TEST AND SUBTRACT
      *             (SUBTRACT NDTE-DD FROM LN-1STPYDA) SHOULD ONLY BE DONE
      *             WHEN LN-1STPYDA > 28 #922
      ***********************************************************************
       PAST-DUE-DAYS-CALCULATION SECTION.
           MOVE 0 TO PDUE-DAYS.

      * TEST IF ACCOUNT CLOSED:
           IF LN-CURBAL = 0
              GO TO PAST-DUE-DAYS-EXIT.

      * WHEN GP-PDTH-GT-SCHED-PAY = 'Y'
      *   PAID THRU IS CAPPED AT THE NUMBER OF SCHEDULED PAYS
      *   SO SEE IF PAID-THRU IS AT THE CAP (MATURITY)
      *   AND COUNT PAST DUE DAYS FROM MATURITY
      
           IF GP-PDTH-GT-SCHED-PAY = "Y"
              MOVE LN-1STPYDATE TO PDTH-DATE-WORK
              PERFORM PAID-THRU-CALCULATION
              PERFORM MATURITY-DATE-CALCULATION
              MOVE MDTE-DATE      TO NUM-DATE
              MOVE PDTH-DATE-FULL TO SYS-DATE
              IF SYS-DATE NOT < NUM-DATE
                 GO TO PAST-DUE-DAYS-COMPUTE.

      * SETUP NEXT PAYMENT DUE AND
      * COUNT PAST DUE DAYS FROM NEXT PAYMENT DUE DATE:

           MOVE PDUE-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO NDTE-DATE.

           IF LN-UNITPER-CD = "M"
              MOVE PDUE-1STPYDA TO NDTE-DD.

           MOVE 1 TO NDTE-HOLD.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO NUM-DATE.

      * PAST DUE DAYS WERE DISPLAYING AS 1 ON 3/14 WHEN PDTH-DATE-FULL 2/28
      * & INCREMENT-UNITPER INCREMENTED TO 3/13 (1STPYDA 30) SHOULD BE 3/15.
      * FORCED IF PAID THRU 2/28 SUBTRACT NDTE-DD (28) FROM LN-1STPYDA =2,
      * ADD 2 TO THE INCORRECT 3/13/ DATE. PDUE-DAYS SHOULD BE 0

           IF LN-UNITPER-CD = "S"
              MOVE PDTH-DATE-FULL TO NDTE-DATE
              IF (NDTE-DD = 28 OR NDTE-DD = 29) AND NDTE-MM = 2
              IF LN-1STPYDA > NDTE-DD
                 SUBTRACT NDTE-DD FROM LN-1STPYDA GIVING NDTE-HOLD
                 MOVE NUM-DATE TO NDTE-DATE
                 PERFORM INCREMENT-DAYS
                 MOVE NDTE-DATE TO NUM-DATE.

       PAST-DUE-DAYS-COMPUTE.
           MOVE PDUE-TODAY-DATE      TO SYS-DATE.
           PERFORM TIM.
           IF ELAPSED-DAYS > 0
              MOVE ELAPSED-DAYS TO PDUE-DAYS.

       PAST-DUE-DAYS-EXIT.
           EXIT.
      *================================================================*
      * END COPYBOOK: LIBLP\LPPDUE.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBGB\RBACT.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBGB/RBACT
      **************************************************************************
      *         COMPUTE ACTUARIAL REBATE
      *
      *   NAME: RBACT
      *   DESC: COMPUTE TRUE ACTUARIAL REBATE
      *   IN  : RBACT-RBDATE          REFUND DATE
      *         RBACT-INTDATE         INTEREST START DATE
      *         RBACT-ORIG-1STPAY     ORIGINAL FIRST PAYMENT DUE DATE
      *         RBACT-ORGTERM         TOTAL NO OF PAYMENTS
      *         RBACT-1STPYAMT        AMOUNT OF 1ST PAYMENT
      *         RBACT-REGPYAMT        AMOUNT OF REGULAR PAYMENT STREAM
      *         RBACT-LASTPYAMT       THE AMOUNT OF THE LAST PAYMENT
      *         RBACT-TOTCHG          THE FINANCE CHARGE
      *         RBACT-TOTNODEF        TOTAL NO. OF DEFERMENTS
      *         RBACT-RATE            ANNUAL PERCENTAGE RATE (APR) OR
      *                               EQUIVALENT SIMPLE RATE (ESR)
      *         RBACT-UNITPER-CD      PAYMENT PERIOD CODE   EX. 'M' MONTHLY
      *         RBACT-UNITPER-FREQ    FREQUENCY OF PAYMENTS
      *                                               EX. 2 = EVERY 2 WEEKS
      *         RBACT-ELAPSED-DAYS    PASSED FROM REBATE
      *         RBACT-ELAPSED-MONTHS  PASSED FROM REBATE
      *         RBACT-ELAPSED-REM     PASSED FROM REBATE
      *
      *         RBACT-SKIPDEF-FG      SET FROM LPAERN ACCRUAL ROUTINES
      *                                 NOTE: CLEARED TO SPACES AT EXIT
      *         RBACT-EARN-FULL-MONTH SET FROM LPAERN ACCRUAL ROUTINES
      *                                 NOTE: CLEARED TO SPACES AT EXIT
      *
      *      ----> THE FOLLOWING TWO INPUT FIELDS ARE USED BY LIBGB/REBATE
      *            TO DETERMINE, IF A SWING IS REQUIRED:
      *
      *         RBACT-SUB             REB-SUB
      *         RBACT-SUB2            REB-SUB2
      *
      *   OUT : RBACT-REBATE
      *         RBACT-PRES-VALUE
      *   COPY:
      * REV :
      *  JTG 091196 CORRECTED BUG CAUSED BY SP-RBSPOPT1()=13, WHICH WIPED OUT
      *             ELAPSED-UNITPER & ELAPSED-UNITPER-REM
      *             CREATED & SET RBACT-ELAPSED-UNITPER
      *                                    & RBACT-ELAPSED-UNITPER-REM
      *  JTG 021098 ADDED TEST FOR RBACT-SKIPDEF-FG
      *             ADDED TEST FOR RBACT-EARN-FULL-MONTH, TFC #637
      *             RE: SP-EARNACCRL-FG() = 'B'
      *  JTG 042398 CORRECTED TEST FOR RBACT-EARN-FULL-MONTH, TFC #637
      *             RE: SP-EARNACCRL-FG() = 'B'
      *             THE TEST FOR REBATE DATE = INTEREST DATE CAUSED THE
      *             ROUTINE TO EXIT WITH ZERO VALUE IN RBACT-PRES-VALUE
      *             WHICH IS WRONG. THE RBACT-VALUE SHOULD BE EQUAL TO
      *             THE AMOUNT FINANCED. THIS BUG IMPACTED SP-ERNFRMLA = 11
      *             CAUSING NEGATIVE INCOME FOR DEALER DISCOUNT.
      *  JTG 990324 CHANGED TEST FOR RBACT-EARN-FULL-MONTH, TFC #637
      *             TO TAKE ONE MONTH ON NON MONTHLY
      *  JTG 990831 CHANGED TO USE 360 DAYS FOR RBACT-EARN-FULL-MONTH = "Y"
      *             TFC #50
      *  JTG 990924 CORRECTED TO USE 360 DAYS FOR RBACT-EARN-FULL-MONTH = "Y"
      *             ONLY ON MONTHLY ACCOUNTS    TFC #50
      *  JTG 991122 MODIFIED LOGIC (TEST FOR SWING) TO ALSO TEST FOR
      *             SP-RBDAYS NOT = 0 , MERCURY
      *  JTG 000104 MOVED FROM LIBLP TO LIBGB
      *  JTG 000104 ADDED RBACT-PENALTY-FG ADDED SP-RBREDUC = 9999.04 WISCONSIN
      *             IB REAL ESTATE PREPAY PENALTY, WISCONSIN #1031
      *  JTG 000309 CORRECTED (MERCURY ARIZONA ACTION 24) SP-RBSPOPT1() = 13
      *             PER AUDIT ON BR: AZ0192 #323025, UNDER SECTION R20-4-508
      *             PAYMENTS MADE ON OR BEFORE THE 15TH DAYS FOLLOWING A PAYMENT
      *             DUE DATE WILL BE REFUNDED AS OF THE PRECEDING INSTALLMENT
      *             DATE AND ANY PAYMENT MADE ON OR AFTER THE 16TH WILL BE
      *             REFUNDED AS OF THE SUCEEDING PAYMENT DUE DATE
      *  JTG 010316 CHANGED TO TEST ENVIRONMENT VARIABLE 'FORCE_ACCRUAL'
      *             AND FORCE ACCRUAL OF 032801 REGENCY PR#320
      *  JTG 010402 STARRED OUT CHANGE TO TEST ENVIRONMENT VARIABLE
      *             'FORCE_ACCRUAL' AND FORCE ACCRUAL OF 032801 REGENCY PR#320
      *  JTG 010705 ADDED SET OF REB-EARN-FULL-MONTH,           REGENCY PR#1456
      *             RE: SP-ERNACCRL-FG() 'C'
      *  JTG 011026 CHANGED RBACT-APRATE TO RBACT-RATE FOR NEW REFUND FORMULA
      *             '0  6'                                      REGENCY PR#1631
      **************************************************************************
