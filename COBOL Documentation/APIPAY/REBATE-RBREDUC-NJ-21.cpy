      *****************************************************
      *    THRIFT INVESTMENT CORP (NJ) PREPAID PENALTY
      *
      * IF PREPAID WITHIN 12 MONTHS AFTER THE FIRST PAY IS
      * IS DUE, YOU CAN CHARGE A PRE-PAYMENT PENALTY.
      *****************************************************
       REBATE-RBREDUC-NJ-21 SECTION.

           MOVE 0 TO REB-MS-PENALTY.

           IF REB-LPTRCD NOT = "PO"
              GO TO REBATE-RBREDUC-NJ-21-EXIT.

           MOVE LN-ORIG-1STPYDATE TO NUM-DATE.
           MOVE REB-PAYDATE TO SYS-DATE.
           PERFORM TIM365.
           IF ELAPSED-MONTHS NOT < 12
              GO TO REBATE-RBREDUC-NJ-21-EXIT.

      *    0 - 2000 = $20.00
      * 2000 - 5000 = 1% OF AMTFIN
      * 5000 - ?    = $100.00

           PERFORM LOAN-CALCULATIONS.
           IF FINANCED-AMOUNT < 2000.01
              SUBTRACT 20.00 FROM REB-REBATE
              MOVE 20.00 TO REB-MS-PENALTY
           ELSE
              IF FINANCED-AMOUNT < 5000.01
                 COMPUTE REB-MS-PENALTY ROUNDED =
                                             (FINANCED-AMOUNT * .01)
                 COMPUTE REB-REBATE ROUNDED =
                     REB-REBATE - FINANCED-AMOUNT * .01
              ELSE
                 MOVE 100.00 TO REB-MS-PENALTY
                 SUBTRACT 100.00 FROM REB-REBATE.

       REBATE-RBREDUC-NJ-21-EXIT.
           EXIT.

      ********************************************************************
      *    REGENCY FINANCE (OH RBSPOPT1 = 25) PRE-PAYOFF PENALTY
      *
      *    IF PAYOFF IS WITH IN 5 YEARS (360 * 5 = 1800 DAYS) OF LOAN DATE
      *       REDUCE COMPUTED REBATE BY 1% OF ORIGINAL PRINCIPAL
      ********************************************************************
