      ****************************************************************
      *                     (0 I)
      *    CITIZENS FINANCE COMPANY
      *    FORMULA FOR TRIST CPI SINGLE PAYMENT LOANS
      *
      ****************************************************************
       REBATE-TRIST-CPI-0I SECTION.

      * REBATE = ORIGINAL PREMIUM / DAYS IN TERM * DAYS REMAINING

      * DETERMINE DAYS IN INSURANCE TERM:
           MOVE REB-LN-INSEFF TO NUM-DATE.
           MOVE REB-LN-INSEXP TO SYS-DATE.
           PERFORM TIM365.
           MOVE ELAPSED-DAYS TO REBX-DAYS-IN-TERM.

      * DETERMINE DAYS REMAINING:
           MOVE REB-PAYDATE   TO NUM-DATE.
           MOVE REB-LN-INSEXP TO SYS-DATE.
           PERFORM TIM365.
           MOVE ELAPSED-DAYS TO REBX-DAYS-REMAINING.

           IF REBX-DAYS-REMAINING < 0
              MOVE 0 TO REB-REBATE
              GO TO REBATE-TRIST-CPI-0I-EXIT.

      * DETERMINE REBATE:
           COMPUTE REB-REBATE =
              REB-TOTCHG / REBX-DAYS-IN-TERM.

           COMPUTE REB-REBATE = REB-REBATE * REBX-DAYS-REMAINING.

      * ROUND REFUND TO THE NEAREST DOLLAR:
           IF REB-REBATE-CENTS > 49
              MOVE 0 TO REB-REBATE-CENTS
              ADD 1 TO REB-REBATE-WHOLE
           ELSE
              MOVE 0 TO REB-REBATE-CENTS.

       REBATE-TRIST-CPI-0I-EXIT.
           EXIT.

      ***********************************************
      *              ( 0 A )
      *    MEAN OF PRORATA AND RULE 78THS
      ***********************************************
