      ****************************************************************
      *    SECURITY OF AMERICA   ACTUARIAL  (7 OR B)
      *
      *    NOTE:
      *          7 - USES A.P.R.
      *          B - USES EQUIVALENT SIMPLE RATE
      *
      *        1
      *    ------------   + IT - 1
      *     (1 + I)**T
      *   --------------------------  *  REB-TOTCHG
      *        1
      *    ------------   + IN - 1
      *     (1 + I)**N
      *
      * WHERE:   T = REMAINING TERM OF INSURANCE, IN MONTHS
      *          N = ORIGINAL TERM OF INSURANCE, IN MONTHS
      *          I = APR OR SMP RATE OF LOAN DIVIDED BY 1200
      ****************************************************************
       REBATE-ACTUARIAL-7-B SECTION.
           IF SP-RBFRMLA(REB-SUB) = "7"
              MOVE REB-LN-APRATE TO REB-MPRATE
           ELSE
              MOVE LN-SMPRATE TO REB-MPRATE.

           COMPUTE REB-MPRATE ROUNDED = REB-MPRATE / 1200.

      *MEXICO SEMI-MONTHLY ACTUARIAL REBATE USING UNIT PERIODS PER YEAR
      *TO DETERMINE RATE INSTEAD OF A MONTHLY RATE USING FORMULA 'B'
           IF SP-ORGST = "MX"
              IF REB-SUB = 7
                 IF SP-RBUNITPER-CD(REB-SUB) = "B"
                    IF SP-RBFRMLA(REB-SUB) = "B"
                       MOVE LN-SMPRATE TO REB-MPRATE
                        COMPUTE REB-MPRATE = REB-MPRATE * .01
                                 / UPER-UNITPER-PER-YEAR.


           COMPUTE REB-REBATE ROUNDED =
                 (1 / ((1 + REB-MPRATE) ** REB-REMTERM) +
                               (REB-MPRATE * REB-REMTERM) - 1
                 )
                 /
                 (1 / ((1 + REB-MPRATE) ** REB-ORGTERM) +
                             (REB-MPRATE * REB-ORGTERM) - 1
                 )
                 * REB-TOTCHG.

      ****************************************************************
      *    INSURANCE REFUND   ACTUARIAL  (Q)
      *        VIRGINIA
      *
      ****************************************************************
