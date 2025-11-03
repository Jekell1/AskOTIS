      *****************************************************************
      * CLAY FINANCIAL SINGLE PAY LOANS ONLY
      *    IF PREPAID BEFORE 1ST AND ONLY DUE DATE, REBATE
      *       IS RULE OF 78THS, WITH NO OF MONTHS TO 1ST
      *       PAYMENT EQUAL TO ORIGINAL TERM
      *
      *  NOTE: THIS WILL NOT WORK WITH REBATE FORMULA 'N'
      *****************************************************************
       REBATE-EP17 SECTION.
      * TEST FOR COMPANY PAYOFF:
           IF REB-LN-ORGTERM NOT = 1
              GO TO REBATE-EP17-EXIT.

           MOVE REB-LN-INTDATE TO NUM-DATE
                                  REB-REFDATE.
           MOVE REB-LN-ORIG-1STPYDATE TO SYS-DATE.
           PERFORM TIM360.
           MOVE ELAPSED-MONTHS TO REB-LN-ORGTERM
                                  REB-ORGTERM.

           PERFORM CALC-ELAPSED-REF-THRU-PODATE.
           IF JULIAN-DATE = 0
              MOVE 0 TO REB-SUB
              GO TO REBATE-EP17-EXIT.

           PERFORM REBATE-TEST-SWING.
           PERFORM REBATE-DEDUCT-CHARGE.

           PERFORM REBATE-78.
           IF REB-REBATE < 0
              MOVE 0 TO REB-REBATE.
           MOVE 0 TO REB-SUB.

       REBATE-EP17-EXIT.
           EXIT.

      * TEXAS SERVICE CHARGE EARLY PROVISION:
