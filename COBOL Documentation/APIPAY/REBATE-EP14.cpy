      ********************************************
      * ALABAMA ANY PAYOFF WITHIN 90 DAYS (14)
      ********************************************
       REBATE-EP14 SECTION.

           MOVE 365 TO ELAPSED-YRTYPE.
           MOVE REB-LN-INTDATE TO NUM-DATE.
           MOVE REB-PAYDATE TO SYS-DATE.
           PERFORM TIMALL.
      * SUN - "AL", AFTER 90 DAYS, SERVICE CHARGE IS FULLY EARNED
      * AND NO REBATES ARE DONE
           IF ELAPSED-DAYS > 90
              MOVE 0 TO REB-REBATE
              GO TO REBATE-EP14-EXIT.

           MOVE REB-LN-INTDATE TO NDTE-DATE.
           MOVE REB-LN-ORGTERM TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE REB-LN-LOANDATE TO NUM-DATE.
           MOVE NDTE-DATE TO SYS-DATE.
           MOVE SP-YEARTYPE TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           MOVE ELAPSED-DAYS TO REB-ELAPSED-DAYS.
           MOVE REB-LN-INTDATE TO NUM-DATE.
           MOVE REB-PAYDATE TO SYS-DATE.
           MOVE SP-YEARTYPE TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           COMPUTE REB-PRORATA-30DAY-PCT ROUNDED =
                (REB-ELAPSED-DAYS - ELAPSED-DAYS) / REB-ELAPSED-DAYS.
           COMPUTE REB-REBATE ROUNDED = REB-PRORATA-30DAY-PCT
                                      *  REB-TOTCHG.

           IF REB-REBATE < 0
              MOVE 0 TO REB-REBATE.

           IF REB-REBATE < SP-RBMIN(REB-SUB)
              MOVE 0 TO REB-REBATE.

       REBATE-EP14-EXIT.
           EXIT.

      *****************************************************************
      * CLAY FINANCIAL SINGLE PAY LOANS ONLY
      *    IF PREPAID BEFORE 1ST AND ONLY DUE DATE, REBATE
      *       IS RULE OF 78THS, WITH NO OF MONTHS TO 1ST
      *       PAYMENT EQUAL TO ORIGINAL TERM
      *
      *  NOTE: THIS WILL NOT WORK WITH REBATE FORMULA 'N'
      *****************************************************************
