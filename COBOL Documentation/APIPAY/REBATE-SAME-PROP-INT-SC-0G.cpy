      ****************************************************************
      *                     (0 G)
      *    WORLD
      *    MEXICO MAINTENANCE FEE REFUND
      *
      *    THE REFUND IS IN THE SAME PROPORTION OF THE INTEREST & SERVICE
      *    CHARGE REFUNDS
      *
      ****************************************************************
       REBATE-SAME-PROP-INT-SC-0G SECTION.

      * COMPUTE REBATE:
           COMPUTE REB-REBATE ROUNDED =
               (REB-LN-INT-REBATE + REB-LN-SC-REBATE)
               * LN-ANTICERN(4) / (LN-INTCHG + LN-SERCHG).

       RB-SAME-PROP-INT-SC-0G-EXIT.
           EXIT.

      *****************************************************
      *    MEXICO CL ANTICIPATED     (0, J)
      *
      *    THIS ROUTINE CALCULATES THE INSURANCE PREMIUM
      *    TO COVER THE REMAINING TERM ON THE COVERAGE
      *    REMAINING, AND REBATES THAT AMOUNT.
      *****************************************************
