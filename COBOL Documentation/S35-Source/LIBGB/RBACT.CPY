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
       RBACT-ACTUARIAL SECTION.
           MOVE 0 TO RBACT-REBATE
                     RBACT-PRES-VALUE.
           IF RBACT-RATE = 0  OR RBACT-RBDATE = 0
              GO TO RBACT-ACTUARIAL-EXIT.

      ***********   SET UNIT PERIOD INFO   **************************
           MOVE RBACT-UNITPER-CD TO UPER-UNITPER-CD.
           MOVE RBACT-UNITPER-FREQ TO UPER-UNITPER-FREQ.
           MOVE RBACT-INTDATE TO UPER-INTDATE.
           MOVE RBACT-ORIG-1STPAY TO UPER-1STPYDATE.
           MOVE RBACT-ORGTERM TO UPER-TERM.
           PERFORM UPER-CALCULATION.

           MOVE UPER-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE UPER-UNITPER-FREQ TO DATER-UNITPER-FREQ.

      ********************************************************
      *    DETERMINE IF FIRST PAYMENT EXTENTIONS
      *    ARE NOT CHARGED INTEREST AND DISCLOSURE IS
      *    IS DONE FOR SALES FINANCE VIA: 'B', IF SO SET
      *    INTEREST DATE , ONE MONTH PRIOR TO ORIG 1ST PAY
      *    AND ESTABLISH RBACTF-ADJ FOR DAILY INTEREST
      ********************************************************
           MOVE 1 TO RBACTF-ADJ.
           IF RBACT-PENALTY-FG NOT = "Y"
              IF SP-RBSPOPT1-FIRST-30(RBACT-SUB)
                 MOVE RBACT-ORIG-1STPAY TO NDTE-DATE
                 MOVE -1 TO NDTE-HOLD
                 PERFORM INCREMENT-UNITPER
                 MOVE RBACT-INTDATE TO NUM-DATE
                 MOVE NDTE-DATE     TO SYS-DATE
                 IF SYS-DATE > NUM-DATE
                    MOVE NDTE-DATE TO RBACT-INTDATE
                 END-IF
                 IF SP-INTMETHOD = "E"
                    COMPUTE RBACTF-ADJ ROUNDED =
                        UPER-UNITPER-PER-YEAR
                                * UPER-DAYS-IN-UNITPER / 365.

      **************************************************************************
      *    DETERMINE:
      *       IF REBATE DATE IS BEFORE INTDATE,
      *          FULL REBATE
      **************************************************************************
           MOVE RBACT-INTDATE TO NUM-DATE.
           MOVE RBACT-RBDATE  TO SYS-DATE.
           IF RBACT-RBDATE < RBACT-INTDATE
              MOVE RBACT-TOTCHG TO RBACT-REBATE
              GO TO RBACT-ACTUARIAL-EXIT.

      ***************************************************
      *    TEST FOR SWING RULE
      ***************************************************
           MOVE RBACT-RBDATE TO RBACT-REFDATE.
           IF RBACT-PENALTY-FG NOT = "Y"
              IF ( ( RBACT-UNITPER-CD = "M"                  ) AND
                   ( RBACT-UNITPER-FREQ = 1                  ) AND
                   ( SP-RBDAYS(RBACT-SUB RBACT-SUB2) NOT = 0 )
                 )
                OR
                 ( RBACT-EARN-FULL-MONTH = "B" OR "C")
                 IF RBACT-ELAPSED-REM = 0
                    MOVE RBACT-INTDATE        TO NDTE-DATE
                    MOVE RBACT-ELAPSED-MONTHS TO NDTE-HOLD
                    PERFORM INCREMENT-MONTHS
                    MOVE NDTE-DATE TO RBACT-REFDATE.

      *    -------------------------------------------------------------
      *            IF ENVIRONMENT VARIABLE SET THEN CHECK IF
      *                LOAN INTDATE IS 28TH OR 29TH
      *                IF SO FORCE ACCRUAL TO 032801
      *                (REGENCY #PR#320, JTG 010316)
      *    -------------------------------------------------------------
      *            MOVE "FORCE_ACCRUAL" TO GETENV-BUF
      *            PERFORM GETENV-CALL
      *            IF STAT = "00"
      *               IF RBACT-REFDATE = 022801
      *                  MOVE LN-INTDATE TO NUM-DATE
      *                  IF NUM-DA = 28
      *                     MOVE 032801 TO RBACT-REFDATE
      *                  ELSE
      *                  IF NUM-DA = 29
      *                     MOVE 032901 TO RBACT-REFDATE.

      ***************************************************
      *    DETERMINE ELAPSED TIME REMAINING BETWEEN
      *    REFERENCE (REBATE) DATE AND ORIGINAL MATURITY
      ***************************************************
           MOVE RBACT-ORIG-1STPAY TO NDTE-DATE.
           SUBTRACT 1 FROM RBACT-ORGTERM GIVING NDTE-HOLD.
           IF RBACT-PENALTY-FG NOT = "Y"
              IF RBACT-SKIPDEF-FG = " "
                 IF SP-RBDEFER(RBACT-SUB) = 1
                    ADD RBACT-TOTNODEF TO NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           MOVE RBACT-REFDATE TO NUM-DATE.
           MOVE NDTE-DATE TO SYS-DATE.

      * TFC APPROVED 8/25/1999 JTG #50
      * TFC CORRECTED TO ONLY DO TIM360 IF MONTHLY
      * JTG 010705 ADDED SET OF REB-EARN-FULL-MONTH, REGENCY PR#1456
      *            RE: SP-ERNACCRL-FG() 'C'
      * NOTE:
      *       SP-ERNACCRL-FG() 'B' AND 'C' ARE IDENTICAL EXCEPT FOR
      *       THE FOLLOWING IF, WHERE 'B' DOES ELAPSED MONTHS 360

           IF (RBACT-EARN-FULL-MONTH = "B"    ) AND
              (RBACT-UNITPER-CD = "M"         ) AND
              (RBACT-UNITPER-FREQ = 1         ) AND
              (RBACT-PENALTY-FG NOT = "Y"     )
              PERFORM TIM360
              MOVE ELAPSED-MONTHS      TO RBACT-ELAPSED-UNITPER
              MOVE ELAPSED-REM         TO RBACT-ELAPSED-UNITPER-REM
           ELSE
              PERFORM TIMUPER
              MOVE ELAPSED-UNITPER     TO RBACT-ELAPSED-UNITPER
              MOVE ELAPSED-UNITPER-REM TO RBACT-ELAPSED-UNITPER-REM.
           IF SYS-IS-LE
              GO TO RBACT-ACTUARIAL-EXIT.
      * TFC APPROVED 8/25/1999 JTG #50
      * JTG 010705 ADDED SET OF REB-EARN-FULL-MONTH, REGENCY PR#1456

      ***************************************************
      *    SET FRACTIONAL PERIOD FROM REBATE DATE TO NEXT
      *    PAYMENT DUE DATE
      ***************************************************
           COMPUTE RBACTF ROUNDED =
              RBACT-ELAPSED-UNITPER-REM
                   / UPER-DAYS-IN-UNITPER * RBACTF-ADJ.

      ***************************************************
      *    SET WHOLE PERIOD(S) FROM REBATE DATE TO NEXT
      *    PAYMENT DUE DATE
      ***************************************************
           IF RBACT-ELAPSED-UNITPER NOT < RBACT-ORGTERM
              COMPUTE RBACTT =
                   RBACT-ELAPSED-UNITPER - RBACT-ORGTERM + 1
           ELSE
              IF RBACTF = 0
                 MOVE 1 TO RBACTT
              ELSE
                 MOVE 0 TO RBACTT.

      *******************************************************
      *    DETERMINE IF SWING TO NEAREST DUE DATE REQUIRED
      *    (MERCURY ARIZONA ACTION 24)
      *******************************************************
           IF RBACT-PENALTY-FG NOT = "Y"
              IF SP-RBSPOPT1(RBACT-SUB) = 13
                 MOVE RBACT-ORIG-1STPAY TO NUM-DATE
                 MOVE RBACT-RBDATE      TO SYS-DATE
                 IF RBACT-ORIG-1STPAY < RBACT-RBDATE
                    MOVE RBACT-ORIG-1STPAY TO NUM-DATE
                 ELSE
                    MOVE RBACT-INTDATE     TO NUM-DATE
                 END-IF
                 MOVE RBACT-RBDATE      TO NDTE-DATE
                 IF NDTE-DD < NUM-DA
                    MOVE NUM-DA         TO NDTE-DD
                    MOVE -1             TO NDTE-HOLD
                    PERFORM INCREMENT-MONTHS
                 ELSE
                    MOVE NUM-DA         TO NDTE-DD
                 END-IF
                 MOVE NDTE-DATE         TO NUM-DATE
                 MOVE RBACT-RBDATE      TO SYS-DATE
                 PERFORM TIM365
                 IF ELAPSED-DAYS <= 15
                    MOVE 0 TO RBACTF
                    MOVE 1 TO RBACTT
                 ELSE
                    MOVE 0 TO RBACTF
                              RBACTT.

      ***************************************************
      *    COMPUTE INT. RATE PER PMT/COMPOUND INTERVAL:
      ***************************************************
           COMPUTE RBACTI ROUNDED =
              RBACT-RATE * 0.01 / UPER-UNITPER-PER-YEAR.

      ************************************************************
      *    COMPUTE 1ST PAYMENT REQUIREMENTS AND ADJUSTMENT FACTOR
      ************************************************************
      * SINGLE PAY LOANS AND US RULE:
           IF RBACT-ORGTERM < 2 OR SP-DISFRMLA = "C"
              COMPUTE RBACTADJ ROUNDED =
                 1 / (1 + (RBACTT + RBACTF) * RBACTI)
           ELSE
      * ACTUARIAL:
              COMPUTE RBACTADJ ROUNDED =
                 1 / ((1 + RBACTF * RBACTI) * (1 + RBACTI) ** RBACTT).

           MOVE RBACT-1STPYAMT TO RBACTFP.
           IF RBACT-ELAPSED-UNITPER > RBACT-ORGTERM
              MOVE RBACT-ORGTERM TO RBACT-ELAPSED-UNITPER
           ELSE
              IF RBACT-ELAPSED-UNITPER < (RBACT-ORGTERM - 1)
                 MOVE RBACT-REGPYAMT TO RBACTFP
              ELSE
                 IF RBACT-ELAPSED-UNITPER = (RBACT-ORGTERM - 1)
                    IF RBACT-ELAPSED-UNITPER-REM = 0
                       MOVE RBACT-REGPYAMT TO RBACTFP.

      ***********************************************************
      *    COMPUTE REGULAR PAYMENT REQUIREMENTS & ANNUITY FACTOR
      ***********************************************************
           IF RBACT-ORGTERM < 3
              MOVE 0 TO RBACTRP-A--N
                        RBACTRP-N
           ELSE
              MOVE RBACT-REGPYAMT TO RBACTRP
              IF RBACT-ELAPSED-UNITPER-REM = 0
                            OR RBACT-ELAPSED-UNITPER = RBACT-ORGTERM
                 SUBTRACT 2 FROM RBACT-ELAPSED-UNITPER GIVING RBACTRP-N
              ELSE
                 SUBTRACT 1 FROM RBACT-ELAPSED-UNITPER GIVING RBACTRP-N
              END-IF

              IF RBACTRP-N < 0
                 MOVE 0 TO RBACTRP-N
              END-IF

              COMPUTE RBACTRP-A--N ROUNDED =
                     (1 - 1 / (1 + RBACTI) ** RBACTRP-N) / RBACTI.

      ***************************************************
      *    COMPUTE LAST PAYMENT REQUIREMENTS & FACTOR
      ***************************************************
           IF RBACT-ORGTERM < 2
              MOVE 0 TO RBACTLP
              GO TO RBACT-ACTUARIAL-PV.

           MOVE RBACT-LASTPYAMT TO RBACTLP.
           IF RBACT-ELAPSED-UNITPER < 1
              MOVE 0 TO RBACTLP-N
                        RBACTFP
           ELSE
           IF RBACT-ELAPSED-UNITPER = 1
              IF RBACT-ELAPSED-UNITPER-REM = 0
                 MOVE 0 TO RBACTLP-N
                           RBACTFP
              ELSE
                 MOVE 1 TO RBACTLP-N
           ELSE
              IF RBACT-ELAPSED-UNITPER-REM = 0
                            OR RBACT-ELAPSED-UNITPER = RBACT-ORGTERM
                 SUBTRACT 1 FROM RBACT-ELAPSED-UNITPER GIVING RBACTLP-N
              ELSE
                 MOVE RBACT-ELAPSED-UNITPER TO RBACTLP-N.

           COMPUTE RBACTLP-FAC ROUNDED =
                      1 / ((1 + RBACTI) ** RBACTLP-N).

      ***************************************************
      *    COMPUTE PRESENT VALUE OF REMAINING PAYMENTS
      *         AT THE TIME OF REBATE
      ***************************************************
       RBACT-ACTUARIAL-PV.
           COMPUTE RBACTPV ROUNDED =
              RBACTADJ * (RBACTFP + RBACTRP * RBACTRP-A--N
                               + RBACTLP * RBACTLP-FAC
                       ).
           MOVE RBACTPV TO RBACT-PRES-VALUE.

      ***************************************************
      *    COMPUTE REBATE AMOUNT
      ***************************************************
           COMPUTE RBACT-REBATE ROUNDED =
              (RBACTFP + RBACTRP * RBACTRP-N + RBACTLP)
                                       - RBACTPV.

       RBACT-ACTUARIAL-EXIT.
           MOVE " " TO RBACT-SKIPDEF-FG
                       RBACT-EARN-FULL-MONTH.

      *-----------------------------------------------------------------------
      *    DUMMY SECTION TO INCLUDE IN COPY MEMBERS THAT NEED TO BE
      *    REFLECTED IN 'WHAT' COMMANDS FOR SUPPORT
      *-----------------------------------------------------------------------
       SCCS-COPY-ROUTINE SECTION. MOVE
           "@(#)./LIBGB/RBACT.CPY A32 >"
              TO MESS.

      *-----------------------------------------------------------------------
