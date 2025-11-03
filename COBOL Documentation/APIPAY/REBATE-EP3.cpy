      *****************************************************************
      * MISSISSIPPI COMPANY PAYOFF WITHIN 90 DAYS - 3
      * ALABAMA COMPANY PAYOFF WITHIN 120 DAYS - 16  (SAME AS 3)
      *
      * ALABAMA COMPANY PAYOFF WITHIN 90 DAYS - 12
      * ALABAMA COMPANY PAYOFF WITHIN 120 DAYS - 15
      *    IF PREPAID WITHIN 90 DAYS BY COMPANY PAYOFF, REFUND PRORATA
      *    TO THE DAY. A 365-DAY YEAR IS USED TO COMPUTE THE 90DAY TEST
      *    AND SP-YEARTYPE IS USED TO COMPUTE THE REBATE. (SPREAR=1)
      *****************************************************************
       REBATE-EP3 SECTION.
      * TEST FOR COMPANY PAYOFF:
           IF NOT (REB-LPTRCD = "PB" OR "RN" OR "SC"
                                   OR "RB" OR "RO")
              GO TO REBATE-EP3-EXIT.

      * TEST FOR PAYOFF WITHIN 1ST 90 DAYS:
           MOVE 367 TO ELAPSED-YRTYPE.
      * MERCURY - "AL"
           IF SP-RBEARLY(REB-SUB) = 12 OR 15
              MOVE 365 TO ELAPSED-YRTYPE.
           MOVE REB-LN-INTDATE TO NUM-DATE.
           MOVE REB-PAYDATE TO SYS-DATE.
           PERFORM TIMALL.
           IF SP-RBEARLY(REB-SUB) = 15 OR 16
              IF ELAPSED-DAYS > 120
                 GO TO REBATE-EP3-EXIT
              ELSE
                 NEXT SENTENCE
           ELSE
              IF ELAPSED-DAYS > 90
                 GO TO REBATE-EP3-EXIT.

           IF SP-RBEARLY(REB-SUB) = 12 OR 15
              MOVE REB-LN-INTDATE TO NDTE-DATE
              MOVE REB-LN-ORGTERM TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE REB-LN-LOANDATE TO NUM-DATE
              MOVE NDTE-DATE TO SYS-DATE
              MOVE SP-YEARTYPE TO ELAPSED-YRTYPE
              PERFORM TIMALL
              MOVE ELAPSED-DAYS TO REB-ELAPSED-DAYS
              MOVE REB-LN-INTDATE TO NUM-DATE
              MOVE REB-PAYDATE TO SYS-DATE
              MOVE SP-YEARTYPE TO ELAPSED-YRTYPE
              PERFORM TIMALL
      * ACTION TRUNCATES TO 3 DECIMALS
              COMPUTE REB-PRORATA-30DAY-PCT ROUNDED =
                   (REB-ELAPSED-DAYS - ELAPSED-DAYS) / REB-ELAPSED-DAYS
              COMPUTE REB-REBATE ROUNDED = REB-PRORATA-30DAY-PCT
                                         *  REB-TOTCHG
           ELSE
              COMPUTE REB-REBATE ROUNDED =
                (LN-INTCHG + LN-EXTCHG) - (LN-EFFRATE / 360 / 100 *
                (LN-LNAMT - LN-INTCHG - LN-EXTCHG) * ELAPSED-DAYS).

           IF REB-REBATE < 0
              MOVE 0 TO REB-REBATE.
           MOVE 0 TO REB-SUB.

       REBATE-EP3-EXIT.
           EXIT.

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
