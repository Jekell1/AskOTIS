      ***************************************************
      *      O L D    R O U T I N E
      *
      *    ACTUARIAL REBATE ROUTINE  F
      *
      *         TRUE ACTUARIAL REBATING ROUTINE:
      *
      * THIS ROUTINE USINGS THE APR AND TAKES INTO ACCOUNT
      * 1ST PAYMENT EXTENSION DAYS (+ OR - NO LIMIT)
      * 1ST PAYMENT CHARGES (ODD 1ST PAYMENTS)
      * DEFERMENTS HAVE NO IMPACT ON CALCULATIONS
      ***************************************************
      *REBATE-ACTUARIAL-F SECTION.
      *    IF REB-LN-APRATE = 0
      *       GO TO REBATE-ACTUARIAL-F-EXIT.
      ********************************************************
      *    DETERMINE IF FIRST PAYMENT EXTENSIONS
      *    ARE NOT CHARGED INTEREST AND DISCLOSURE IS
      *    IS DONE FOR SALES FINANCE VIA: 'B', IF SO SET
      *    INTEREST DATE , ONE MONTH PRIOR TO ORIG 1ST PAY
      *    AND ESTABLISH REBF-ADJ FOR DAILY INTEREST
      ********************************************************
      *    MOVE 1 TO REBF-ADJ.
      *    IF SP-RBSPOPT1-FIRST-30(REB-SUB)
      *       MOVE REB-LN-ORIG-1STPYDATE TO NDTE-DATE
      *       MOVE -1 TO NDTE-HOLD
      *       PERFORM INCREMENT-MONTHS
      *       MOVE REB-LN-INTDATE TO NUM-DATE
      *       MOVE NDTE-DATE TO SYS-DATE
      *       PERFORM CMPDAT
      *       IF SYS-IS-GT
      *          MOVE NDTE-DATE TO REB-LN-INTDATE
      *       END-IF
      *       IF SP-INTMETHOD = "E"
      *          COMPUTE REBF-ADJ ROUNDED = 360 / 365.
      ***************************************************
      *    DETERMINE IF PAYDATE IS BEFORE INTDATE,
      *    IF SO FULL REBATE
      ***************************************************
      *    MOVE REB-LN-INTDATE TO NUM-DATE.
      *    MOVE REB-PAYDATE TO SYS-DATE.
      *    PERFORM CMPDAT.
      *    IF SYS-IS-LE
      *       MOVE REB-TOTCHG TO REB-REBATE
      *       GO TO REBATE-ACTUARIAL-F-EXIT.
      ***************************************************
      *    TEST FOR SWING RULE
      ***************************************************
      *    MOVE REB-PAYDATE TO REB-REFDATE.
      *    IF REB-ELAPSED-REM = 0
      *       MOVE REB-LN-INTDATE TO NDTE-DATE
      *       MOVE REB-ELAPSED-MONTHS TO NDTE-HOLD
      *       PERFORM INCREMENT-MONTHS
      *       MOVE NDTE-DATE TO REB-REFDATE.
      ***************************************************
      *    DETERMINE ELAPSED TIME REMAINING BETWEEN
      *    PAYDATE AND ORIGINAL MATURITY:
      ***************************************************
      *    MOVE REB-LN-ORIG-1STPYDATE TO NDTE-DATE.
      *    SUBTRACT 1 FROM REB-LN-ORGTERM GIVING NDTE-HOLD.
      *    IF REB-SKIPDEF-FG = " "
      *       IF SP-RBDEFER(REB-SUB) = 1
      *          ADD REB-LN-TOTNODEF TO NDTE-HOLD.
      *    PERFORM INCREMENT-MONTHS.
      *    MOVE REB-REFDATE TO NUM-DATE.
      *    MOVE NDTE-DATE TO SYS-DATE.
      *    MOVE 999 TO ELAPSED-YRTYPE.
      *    PERFORM TIMALL.
      *    IF ELAPSED-DAYS NOT > 0
      *       GO TO REBATE-ACTUARIAL-F-EXIT.
      ***************************************************
      *    SET FRACTIONAL PERIOD FROM PAYDATE TO NEXT
      *    PAYMENT DUE DATE:
      ***************************************************
      *    COMPUTE REBF = ELAPSED-REM / 30 * REBF-ADJ.
      ***************************************************
      *    SET WHOLE PERIOD(S) FROM PAYDATE TO NEXT
      *    PAYMENT DUE DATE:
      ***************************************************
      *    IF ELAPSED-MONTHS NOT < REB-LN-ORGTERM
      *       COMPUTE REBT = ELAPSED-MONTHS
      *                      - REB-LN-ORGTERM + 1
      *    ELSE
      *       IF REBF = 0
      *          MOVE 1 TO REBT
      *       ELSE
      *          MOVE 0 TO REBT.
      ***************************************************
      *    DETERMINE 1ST PAYMENT REQUIREMENTS:
      ***************************************************
      * ESTABLISH 1ST PAY AMOUNT:
      *    IF REB-LN-ORGTERM = 1
      *       IF REB-LN-1STPYAMT NOT = 0
      *          MOVE REB-LN-1STPYAMT TO REBP1
      *       ELSE
      *          MOVE REB-LN-REGPYAMT TO REBP1.
      *    IF REB-LN-ORGTERM NOT = 1
      *       IF REB-SUB2 = 2
      * ON OR AFTER 1ST PAYDATE:
      *          MOVE REB-LN-REGPYAMT TO REBP1
      *       ELSE
      * BEFORE 1ST PAYDATE:
      *          IF REB-LN-1STPYAMT NOT = 0
      *             MOVE REB-LN-1STPYAMT TO REBP1
      *          ELSE
      *             MOVE REB-LN-REGPYAMT TO REBP1.
      ***************************************************
      *    ESTABLISH ANNUITY PAYMENT AMT AND TERM:
      ***************************************************
      *    IF REB-LN-ORGTERM = 1
      *       MOVE 0 TO REBN-1 REBP
      *    ELSE
      *       IF ELAPSED-MONTHS > 0
      *          MOVE REB-LN-REGPYAMT TO REBP
      *          SUBTRACT REBT FROM ELAPSED-MONTHS
      *                                  GIVING REBN-1
      *       ELSE
      *          MOVE 0 TO REBN-1 REBP.
      *******************************************************
      *    DETERMINE IF SWING TO NEAREST DUE DATE REQUIRED
      *    (MERCURY ARIZONA ACTION 24)
      *******************************************************
      *    IF SP-RBSPOPT1(REB-SUB) = 13
      *       MOVE REB-PAYDATE TO NUM-DATE
      *       MOVE REB-LN-ORIG-1STPYDATE TO SYS-DATE
      *       PERFORM CMPDAT
      *       IF SYS-IS-GE
      *          MOVE REB-LN-INTDATE TO NUM-DATE
      *          PERFORM TIM365
      *          COMPUTE REB-FACTOR ROUNDED =
      *             (REBF + REBT) * 30 / ELAPSED-DAYS
      *          IF REB-FACTOR NOT = 0
      *             IF REB-FACTOR > 0.5
      *                MOVE 0 TO REBF
      *                MOVE 1 TO REBT
      *             ELSE
      *                MOVE 0 TO REBF
      *                          REBT
      *          ELSE
      *             NEXT SENTENCE
      *       ELSE
      *          IF REBF NOT = 0
      *             IF REBF > 0.5
      *                MOVE 0 TO REBF
      *                MOVE 1 TO REBT
      *             ELSE
      *                MOVE 0 TO REBF
      *                          REBT.
      ***************************************************
      *    COMPUTE INT. RATE PER PMT/COMPOUND INTERVAL:
      ***************************************************
      *    COMPUTE REBI ROUNDED =
      *       (REB-LN-APRATE / 100) / 12.
      ***************************************************
      *    COMPUTE ADJUSTMENT FACTOR TO 1ST DUE PAYMENT:
      ***************************************************
      * SINGLE PAY LOANS AND US RULE:
      *    IF REB-LN-ORGTERM < 2 OR SP-DISFRMLA = "C"
      *       COMPUTE REBADJ ROUNDED =
      *          1 / (1 + (REBT + REBF) * REBI)
      *    ELSE
      * ACTUARIAL:
      *       COMPUTE REBADJ ROUNDED =
      *          1 / ((1 + REBF * REBI) * (1 + REBI) ** REBT).
      ***************************************************
      *    COMPUTE PRESENT WORTH OF $1.00 PER PERIOD:
      ***************************************************
      *    COMPUTE REBPW ROUNDED =
      *       (1 - 1 / (1 + REBI) ** REBN-1) / REBI.
      ***************************************************
      *    COMPUTE PRESENT VALUE OF REMAINING PAYMENTS
      *    AT THE TIME OF REBATE:
      ***************************************************
      *    COMPUTE REBPV ROUNDED =
      *       REBADJ * (REBP1 + REBP * REBPW).
      *    MOVE REBPV TO REB-PRES-VALUE.
      ***************************************************
      * COMPUTE REBATE AMOUNT:
      ***************************************************
      *    COMPUTE REB-REBATE ROUNDED =
      *       (REBP1 + REBP * REBN-1) - REBPV.
      *REBATE-ACTUARIAL-F-EXIT.
      *    EXIT.
      *****************************************************
      *    MASS. A & H ANTICIPATED     (G)
      *
      *    THIS ROUTINE CALCULATES THE INSURANCE PREMIUM
      *    TO COVER THE REMAINING TERM ON THE COVERAGE
      *    REMAINING, AND REBATES THAT AMOUNT.
      *****************************************************
       REBATE-MA-AH-ANTICIPATED SECTION.
           MOVE REB-AHSUB TO REB-INSSUB.
           PERFORM REBATE-FIND-INS-RATE.

           COMPUTE REB-REBATE ROUNDED = REB-LN-REGPYAMT *
                          REB-HUNDREDS * REB-REMTERM / 100.

       REBATE-MA-AH-ANTICIPATED-EXIT.
           EXIT.

      *****************************************************
      *                     (GA)
      *    SERCHG REBATE FRMLA   H -   ROGER WILLIAMS
      *                          I -   JOE DOUGLAS SR
      *
      *    THIS ROUTINE CALCULATES THE SERVICE CHARGE
      *    REBATE, INSURING INTEREST AND SERVICE CHG
      *    EARNINGS DON'T EXCEED 5% PER MONTH.
      *****************************************************
