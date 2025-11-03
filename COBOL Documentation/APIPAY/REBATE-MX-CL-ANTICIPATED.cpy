      *****************************************************
      *    MEXICO CL ANTICIPATED     (0, J)
      *
      *    THIS ROUTINE CALCULATES THE INSURANCE PREMIUM
      *    TO COVER THE REMAINING TERM ON THE COVERAGE
      *    REMAINING, AND REBATES THAT AMOUNT.
      *****************************************************
       REBATE-MX-CL-ANTICIPATED SECTION.
           IF LN-INSTYPES-DL
              IF LN-INS-SINGLE
                 COMPUTE REB-REBATE ROUNDED =
                         ((SP-INSRATE(1 1) / 12 * REB-REMTERM) / 100) *
                                   REB-REMTERM * REB-LN-REGPYAMT
              ELSE
                 COMPUTE REB-REBATE ROUNDED =
                         ((SP-INSRATE(2 1) / 12 * REB-REMTERM) / 100) *
                                   REB-REMTERM * REB-LN-REGPYAMT.

       REBATE-MX-CL-ANTICIPATED-EXIT.
           EXIT.
      *****************************************************
      *    MERC. DAILY PRORATA         (V) & (W)
      *
      *****************************************************
