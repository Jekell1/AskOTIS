      *************************************************
      * SOUTH CAROLINA COMPANY PAYOFF WITHIN 90 DAYS
      * INTEREST CHARGE REFUND
      *************************************************
       REBATE-EP19 SECTION.
      * TEST FOR COMPANY PAYOFF:
           IF NOT (REB-LPTRCD = "PB" OR "RN" OR "SC"
                                   OR "RB" OR "RO")
              GO TO REBATE-EP19-EXIT.

      * TEST FOR PAYOFF WITHIN 1ST 90 DAYS:
           MOVE 365 TO ELAPSED-YRTYPE.
           MOVE REB-LN-INTDATE TO NUM-DATE.
           MOVE REB-PAYDATE TO SYS-DATE.
           PERFORM TIMALL.
           IF ELAPSED-DAYS > 90
              GO TO REBATE-EP19-EXIT.

           COMPUTE REB-REBATE ROUNDED =
              REB-TOTCHG
                - REB-TOTCHG / (REB-ORGTERM * 30) * ELAPSED-DAYS.
           IF REB-REBATE < 0
              MOVE 0 TO REB-REBATE.
           MOVE 0 TO REB-SUB.

       REBATE-EP19-EXIT.
           EXIT.

      *************************************************
      * SOUTH CAROLINA SERVICE CHARGE REFUND,
      * IF CASH ADVANCE < $150.01, NO REFUND
      *************************************************
