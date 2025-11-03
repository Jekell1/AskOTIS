      ********************************************
      * MERCURY MISSISSIPPI EARLY REFUND PROVISION
      * IF PREPAID WITHIN 90 DAYS FROM DATE INT STARTS
      * BY COMPANY PAYOFF OR BY THE CUSTOMER BEFORE THE FIRST
      * PAYMENT COMES DUE, REFUND IS PRO RATA TO THE DAY
      * BASED ON THE FOLLOWING FORMULA:
      *   1.   APR
      *        ---   X  AF  X D = EA
      *        360
      *
      *   2. FC - EA = R
      *
      *      APR = WITHOUT SERVICE CHARGE
      *      AF  = TOTAL NOTE - INTCHG - EXTCHG
      *      D   = NUMBER OF DAYS LOAN IN EFFECT
      *      EA  = EARNED AMOUNT
      *      FC  = ORIGINAL FINANCE CHARGE + EXTCHG
      *      R   = REFUND
      ********************************************
       REBATE-EP11 SECTION.
      * TEST FOR COMPANY PAYOFF WITHIN 90 DAYS FROM DATE INT STARTS
           IF (REB-LPTRCD = "PB" OR "RN" OR "SC"
                               OR "RB" OR "RO")
              MOVE 367 TO ELAPSED-YRTYPE
              MOVE REB-LN-INTDATE TO NUM-DATE
              MOVE REB-PAYDATE TO SYS-DATE
              PERFORM TIMALL
              IF ELAPSED-DAYS > 90
                 GO TO REBATE-EP11-EXIT
              ELSE
                 NEXT SENTENCE
           ELSE
      * TEST FOR CUSTOMER PAYOFF BEFORE THE FIRST PAYMENT COMES DUE
              IF REB-SUB NOT = 7
                 GO TO REBATE-EP11-EXIT
              ELSE
                 IF NOT PODATE-B4-1STPYDATE
                    GO TO REBATE-EP11-EXIT
                 ELSE
                    MOVE 367 TO ELAPSED-YRTYPE
                    MOVE REB-LN-INTDATE TO NUM-DATE
                    MOVE REB-PAYDATE TO SYS-DATE
                    PERFORM TIMALL.

           COMPUTE REB-REBATE ROUNDED =
              (LN-INTCHG + LN-EXTCHG) - (LN-EFFRATE / 360 / 100 *
              (LN-LNAMT - LN-INTCHG - LN-EXTCHG) * ELAPSED-DAYS).

           IF REB-REBATE < 0
              MOVE 0 TO REB-REBATE.
           MOVE 0 TO REB-SUB.

       REBATE-EP11-EXIT.
           EXIT.

      *******************************************************
      * MERCURY, NEVADA EARLY REFUND PROVISION
      * WHEN PAYOFF IN DONE BY CUSTOMER REFUND IS
      * BASED ON THE ACTION DATA SHORT RATE TABLE:
      *******************************************************
