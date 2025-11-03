      *****************************************************
      *    TEXAS A & H ANTICIPATED     (P)
      *    UVALDE FINANCE
      *
      *****************************************************
       REBATE-TX-ANTICIPATED SECTION.
           COMPUTE REB-REBATE ROUNDED =
              (REB-TOTCHG - REB-DEDCHG) / 2
               * (
                  (REB-REMTERM * (REB-REMTERM + 1))
                   / (REB-ORGTERM * (REB-ORGTERM + 1))
                      + (REB-REMTERM / REB-ORGTERM)
                 ).

      *****************************************************
      *    CALIFORNIA A & H ANTICIPATED     (R)
      *      NREW FORMULA EFFECTIVE: 05-14-94
      *
      *    THIS ROUTINE CALCULATES TO INSURANCE PREMIUM
      *    TO COVER THE REMAINING TERM ON THE COVERAGE
      *    REMAINING, AND REBATES THAT AMOUNT.
      *****************************************************
