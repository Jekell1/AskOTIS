      *****************************************************
      * REGIONAL MGMT FULL CANCEL IF PAIDOFF WITHIN 10 DAYS
      *****************************************************
       REBATE-EARLY-98 SECTION.
      * TEST FOR PAYOFF WITHIN 1ST 10 DAYS:
      *    MOVE SP-RBYRTYPE(REB-SUB,1) TO ELAPSED-YRTYPE.
           MOVE 365 TO ELAPSED-YRTYPE.
           MOVE REB-LN-INTDATE TO NUM-DATE.
           MOVE REB-PAYDATE TO SYS-DATE.
           PERFORM TIMALL.
           IF ELAPSED-DAYS < 11
              MOVE REB-TOTCHG TO REB-REBATE
              MOVE 0 TO REB-SUB2.

       REBATE-EARLY-98-EXIT.
           EXIT.





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
