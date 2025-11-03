      ****************************************************************
      *                     (0 D)
      *    REGIONAL ACCEPTANCE
      *    RULE 78THS USING DAYS IN TERM AND ELAPSED DAYS
      *    INSTEAD OF UNIT PERIODS IN TERM AND ELAPSED UNIT PERIODS
      *
      ****************************************************************
       REBATE-78-DAYS-0D SECTION.

      *   WHEN SPR FIELD SP-RBDAYS(REB-SUB,1) IS CODED FOR SWING
      *   AND ELAPSED DAYS PRIOR TO 1ST PAYMENT
      *   IS LESS THAN OR EQUAL TO SP-RBDAYS(REB-SUB,1) (NO SWING)
      *   FORCE A FLAT CANCEL, FULL REFUND
      *   NOTE: IF A SWING TAKES PLACE REB-ELAPSED-MONTHS WILL = 1
      *
      *          ---- TO  N O T  CAUSE A SWING LIKE IN THE FOLLOWING EX:
      *                   SPR 1STPAY YEAR TYPE SHOULD BE '360' NOT '365' ETC
      *
      *                                         360      365
      *                                 ------  ---      ---
      *         EFFECTIVE     08/29/04  DAYS     29       30
      *         REBATE DATE   09/28/04  MONTHS    0        1
      *                                 REM      29        0
      *                                        NO SWING  WILL SWING

           IF REB-SWING = "Y" AND REB-ELAPSED-MONTHS = 0
              MOVE REB-TOTCHG TO REB-REBATE
              GO TO REBATE-78-DAYS-0D-EXIT
           ELSE
      * DETERMINE DAYS IN IN FORCE
              MOVE REB-PAYDATE   TO NUM-DATE
              MOVE REB-LN-INSEXP TO SYS-DATE
              PERFORM TIM365
              MOVE ELAPSED-DAYS TO REBX-DAYS-IN-FORCE
              IF REBX-DAYS-IN-FORCE < 0
                 MOVE 0 TO REB-REBATE
                 GO TO REBATE-78-DAYS-0D-EXIT
              END-IF
      * DETERMINE DAYS IN INSURANCE TERM
              MOVE REB-LN-INSEFF TO NUM-DATE
              MOVE REB-LN-INSEXP TO SYS-DATE
              PERFORM TIM365
              MOVE ELAPSED-DAYS TO REBX-DAYS-IN-TERM.

      * DETERMINE RULE 78 REBATE:
           COMPUTE REB-REBATE ROUNDED = REB-TOTCHG
              * (REBX-DAYS-IN-FORCE * (REBX-DAYS-IN-FORCE + 1))
              / (REBX-DAYS-IN-TERM * (REBX-DAYS-IN-TERM + 1)).

       REBATE-78-DAYS-0D-EXIT.
           EXIT.

      ****************************************************************
      *                     (0 E)
      *    WORLD
      *    ALABAMA ACTUARIAL REFUND FORMULA
      *    SINGLE PREMIUM RATE MEMORANDUM
      *
      ****************************************************************
