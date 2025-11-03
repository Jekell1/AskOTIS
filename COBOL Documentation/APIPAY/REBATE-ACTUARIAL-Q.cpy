                 * REB-TOTCHG.
      ****************************************************************
      *    INSURANCE REFUND   ACTUARIAL  (Q)
      *        VIRGINIA
      *
      ****************************************************************
       REBATE-ACTUARIAL-Q SECTION.
           IF SP-RBSPOPT1(REB-SUB) = 26
              COMPUTE REBI ROUNDED = LN-APRATE / 1200
           ELSE
              COMPUTE REBI ROUNDED = LN-SMPRATE / 1200.
           COMPUTE REB-REBATE ROUNDED =
               REB-TOTCHG
               *
               ( REB-REMTERM
                  -
                    (
                     (1 - 1 / (1 + REBI) ** (LN-ORGTERM
                                              - REB-ORGTERM
                                                + REB-REMTERM)
                     ) / REBI
                    )
                  +
                    (
                     (1 - 1 / (1 + REBI) ** (LN-ORGTERM
                                              - REB-ORGTERM)
                     ) / REBI
                    )
               )
               /
               ( REB-ORGTERM
                  -
                    (
                     (1 - 1 / (1 + REBI) ** (LN-ORGTERM)) / REBI
                    )
                  +
                    (
                     (1 - 1 / (1 + REBI) ** (LN-ORGTERM
                                              - REB-ORGTERM)
                     ) / REBI
                    )
               ).

      *****************************************************
      *    INSURANCE ANTICIPATED
      *      PACESETTER "TX" (8)
      *      WORLD "TX"      RBFRMLA2 = "M"   JUST LIKE 8 EXCEPT DOES
      *                                       NOT TRUNCATE RATE FROM TABLE
      *
      *    THIS ROUTINE CALCULATES TO INSURANCE  PREMIUM
      *    TO COVER THE REMAINING TERM ON THE COVERAGE
      *    REMAINING, AND REBATES THAT AMOUNT.
      *****************************************************
