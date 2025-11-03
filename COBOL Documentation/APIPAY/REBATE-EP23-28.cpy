      *****************************************************
      * MISSOURI
      * 23-  IF BEFORE 1STPYDATE, REFUND THE DIFF BETWEEN THE
      *       ORIG INTCHG AND INT PRECOMPUTED ON THE ACTUAL
      *       UNPAID PRINCIPAL BALANCE 1/360TH OF THE EFFRATE
      *       FOR EACH DAY FROM LOANDATE TO POFFDATE
      * ILLINOIS
      * 28-  IF BEFORE 1STPYDATE, REFUND THE DIFF BETWEEN THE
      *       (ORIG INTCHG + ORIG EXTCHG) AND INT PRECOMPUTED
      *       ON THE ACTUAL UNPAID PRINCIPAL BALANCE 1/360TH
      *       OF THE EFFRATE FOR EACH DAY FROM LOANDATE TO POFFDATE
      *****************************************************
       REBATE-EP23-28 SECTION.

           MOVE REB-LN-LOANDATE TO NUM-DATE.
           MOVE REB-PAYDATE     TO SYS-DATE.
           PERFORM TIM365.
           IF ELAPSED-DAYS < 0
              ADD LN-INTCHG LN-EXTCHG GIVING REB-REBATE
           ELSE
              PERFORM LOAN-CALCULATIONS
              IF SP-RBEARLY(REB-SUB) = 23
                 COMPUTE REB-REBATE ROUNDED = LN-INTCHG -
                 ( DISCLOSED-FINAMT * (LN-EFFRATE / 36000)
                    * ELAPSED-DAYS )
              ELSE
                 COMPUTE REB-REBATE ROUNDED =
                        (LN-INTCHG + LN-EXTCHG) -
                 ( DISCLOSED-FINAMT * (LN-EFFRATE / 36000)
                        * ELAPSED-DAYS ).


      ******************************************************
      *    INDIANA EARLY PROVISION 27
      *    EXTENSION CHARGE IS NEVER REFUNDED
      *    1ST MONTH IS PRORATED, 30 DAYS EARNS THE ENTIRE
      *    MONTHS EARNINGS EVEN IF 31 DAYS IN THE PERIOD
      *                                     PR#1406
      *****************************************************
