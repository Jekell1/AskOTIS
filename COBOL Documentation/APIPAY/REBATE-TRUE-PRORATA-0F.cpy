      ****************************************************************
      *                     (0 F)
      *    WORLD
      *    COLORADO TRUE DAILY PRORATA REFUND FORMULA
      *
      ****************************************************************
       REBATE-TRUE-PRORATA-0F SECTION.
           PERFORM MATURITY-DATE-CALCULATION.



      * DETERMINE DAYS IN LOAN TERM:
           IF REB-SUB = 7 OR 8 OR 9 OR 10
              MOVE REB-LN-LOANDATE TO NUM-DATE
              MOVE MDTE-DATE       TO SYS-DATE
           ELSE
      * INSURANCES
              MOVE REB-LN-INSEFF TO NUM-DATE
              MOVE REB-LN-INSEXP TO SYS-DATE MDTE-DATE.

           IF SP-RBSPOPT1(REB-SUB) = 24
               PERFORM TIM360
           ELSE
               PERFORM TIM365.
           MOVE ELAPSED-DAYS    TO REB-WORKER.

      * DETERMINE DAYS REMAINING FROM PAYDATE TO MATURITY:
           MOVE REB-PAYDATE     TO NUM-DATE.
           MOVE MDTE-DATE       TO SYS-DATE.

           IF SP-RBSPOPT1(REB-SUB) = 24
              PERFORM TIM360
           ELSE
              PERFORM TIM365.

      * COMPUTE REBATE:
           COMPUTE REB-REBATE ROUNDED =
               REB-TOTCHG * ELAPSED-DAYS / REB-WORKER.

       REBATE-TRUE-PRORATA-0F-EXIT.
           EXIT.

      ****************************************************************
      *                     (0 G)
      *    WORLD
      *    MEXICO MAINTENANCE FEE REFUND
      *
      *    THE REFUND IS IN THE SAME PROPORTION OF THE INTEREST & SERVICE
      *    CHARGE REFUNDS
      *
      ****************************************************************
