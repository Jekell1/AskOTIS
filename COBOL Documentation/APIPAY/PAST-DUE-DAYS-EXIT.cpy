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
