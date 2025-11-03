      *****************************************************
      *    SPRING FINANCE (NJ) PRE-PAYOFF PENALTY
      *
      *    IF PAYOFF(PO) IS PRIOR TO MATURITY TAKE PENALTY
      *****************************************************
       REBATE-RBREDUC-NJ SECTION.

           MOVE 0 TO REB-MS-PENALTY.

           IF REB-LPTRCD NOT = "PO"
              GO TO REBATE-RBREDUC-NJ-EXIT.

           PERFORM MATURITY-DATE-CALCULATION.
           MOVE REB-PAYDATE TO NUM-DATE.
           MOVE MDTE-DATE TO SYS-DATE.
           PERFORM TIM.
           IF ELAPSED-DAYS NOT > 0
              GO TO REBATE-RBREDUC-NJ-EXIT.

           PERFORM LOAN-CALCULATIONS.
           IF FINANCED-AMOUNT < 2000.01
              MOVE 20.00 TO REB-MS-PENALTY
              SUBTRACT 20.00 FROM REB-REBATE
           ELSE
              IF FINANCED-AMOUNT < 5000.01
                 COMPUTE REB-MS-PENALTY ROUNDED =
                                 (FINANCED-AMOUNT * .01)
                 COMPUTE REB-REBATE ROUNDED =
                     REB-REBATE - FINANCED-AMOUNT * .01
              ELSE
                 MOVE 100.00 TO REB-MS-PENALTY
                 SUBTRACT 100.00 FROM REB-REBATE.

       REBATE-RBREDUC-NJ-EXIT.
           EXIT.

      *****************************************************
      *    THRIFT INVESTMENT CORP (NJ) PREPAID PENALTY
      *
      * IF PREPAID WITHIN 12 MONTHS AFTER THE FIRST PAY IS
      * IS DUE, YOU CAN CHARGE A PRE-PAYMENT PENALTY.
      *****************************************************
