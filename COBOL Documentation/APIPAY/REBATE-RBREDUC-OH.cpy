      ********************************************************************
      *    REGENCY FINANCE (OH RBSPOPT1 = 25) PRE-PAYOFF PENALTY
      *
      *    IF PAYOFF IS WITH IN 5 YEARS (360 * 5 = 1800 DAYS) OF LOAN DATE
      *       REDUCE COMPUTED REBATE BY 1% OF ORIGINAL PRINCIPAL
      ********************************************************************
       REBATE-RBREDUC-OH SECTION.

           MOVE 0 TO REB-MS-PENALTY.
           IF REB-LPTRCD = "PO"
              MOVE LN-LOANDATE TO NUM-DATE
              MOVE REB-PAYDATE TO SYS-DATE
              PERFORM TIM360
              IF ELAPSED-DAYS NOT > 1800
                 PERFORM LOAN-CALCULATIONS
                 COMPUTE REB-MS-PENALTY =
                                 ( FINANCED-AMOUNT * 0.01 )
                 COMPUTE REB-REBATE =
                    REB-REBATE - ( FINANCED-AMOUNT * 0.01 )
                 IF REB-REBATE < 0
                    MOVE 0 TO REB-REBATE.

      ********************************************************************
      *    REGENCY FINANCE (OH RBSPOPT1 = 25) PRE-PAYOFF PENALTY
      *
      *    IF THE ORIGINAL PRINCIPLE AMOUNT IS $75,000 OR LESS
      *       IF PAYOFF IS WITH IN 5 YEARS (360 * 5 = 1800 DAYS) OF LOAN DATE
      *          REDUCE COMPUTED REBATE BY 1% OF THE LOANS NET BALANCE.
      *
      *    IF THE ORIGINAL PRINCIPLE AMOUNT IS OVER $75,000
      *       IF PAYOFF IS WITHIN 1 YEAR  (360 * 1 = 360  DAYS) OF LOAN DATE
      *          REDUCE COMPUTED REBATE BY 2% OF THE LOANS NET BALANCE.
      *       IF PAYOFF IS 1 - 2 YEARS (360 TO 720 DAYS) OF LOAN DATE
      *          REDUCE COMPUTED REBATE BY 1% OF THE LOANS NET BALANCE.
      *       IF PAYOFF IS OVER 5 YEARS (360 * 5 = 1800 DAYS) OF LOAN DATE
      *          THERE IS NO PENALTY.
      ********************************************************************
