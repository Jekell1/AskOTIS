       REBATE-EP2-REBATE-CHG.
           IF REB-REBATE NOT < 0
              IF ELAPSED-DAYS > 0
                 COMPUTE REB-PRORATA-30DAY-PCT =
                                         (ELAPSED-DAYS / 30) + .0009
                 COMPUTE REB-REBATE ROUNDED = REB-REBATE +
                           REB-EARN-FOR-1MON * REB-PRORATA-30DAY-PCT.

