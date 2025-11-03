      * TEXAS SERVICE CHARGE EARLY PROVISION:
       REBATE-EP18 SECTION.
           COMPUTE REB-EARN-FOR-1MON ROUNDED =
                         (REB-ORGTERM * LN-SERCHG) /
                             (REB-ORGTERM * (REB-ORGTERM + 1) / 2).
           IF REB-REBATE NOT < 0
              COMPUTE REB-REBATE = LN-SERCHG - REB-EARN-FOR-1MON
              MOVE REB-PAYDATE TO NUM-DATE
              MOVE REB-LN-ORIG-1STPYDATE TO SYS-DATE
              MOVE SP-RBYRTYPE(REB-SUB,REB-SUB2) TO ELAPSED-YRTYPE
              PERFORM TIMALL
              IF ELAPSED-DAYS > 30
                 MOVE LN-SERCHG TO REB-REBATE
                 GO TO REBATE-EP18-EXIT
              ELSE
                 PERFORM REBATE-EP18-REBATE-CHG
                 GO TO REBATE-EP18-EXIT.

       REBATE-EP18-REBATE-CHG.
           IF REB-REBATE NOT < 0
              IF ELAPSED-DAYS > 0
                 COMPUTE REB-PRORATA-30DAY-PCT =
                                         (ELAPSED-DAYS / 30) + .0009
                 COMPUTE REB-REBATE ROUNDED = REB-REBATE +
                           REB-EARN-FOR-1MON * REB-PRORATA-30DAY-PCT.

       REBATE-EP18-EXIT.
           EXIT.
      *************************************************
      * SOUTH CAROLINA COMPANY PAYOFF WITHIN 90 DAYS
      * INTEREST CHARGE REFUND
      *************************************************
