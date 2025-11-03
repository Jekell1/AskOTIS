                          * (30 - REB-ELAPSED-REM + 1)
                                        / 930 * LN-MAINTFEE ).
       REBATE-PRORATA-ROUTINE.
           IF REB-ORGTERM NOT = 0
              COMPUTE REB-REBATE ROUNDED =
                  (REB-TOTCHG - REB-DEDCHG) *
                         (REB-REMTERM / REB-ORGTERM).


