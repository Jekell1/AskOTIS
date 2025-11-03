      ********************************************
      * TENNEESSEE EARLY REFUND WITHIN 3 DAYS
      ********************************************
       REBATE-EARLY-99 SECTION.

      * TEST FOR PAYOFF WITHIN 1ST 3 DAYS:
           MOVE 365 TO ELAPSED-YRTYPE.
           MOVE REB-LN-INTDATE TO NUM-DATE.
           MOVE REB-PAYDATE TO SYS-DATE.
           PERFORM TIMALL.
           IF ELAPSED-DAYS < 4
              MOVE REB-TOTCHG TO REB-REBATE
              MOVE 0 TO REB-SUB2.

       REBATE-EARLY-99-EXIT.
           EXIT.

      *****************************************************
      * REGIONAL MGMT FULL CANCEL IF PAIDOFF WITHIN 10 DAYS
      *****************************************************
