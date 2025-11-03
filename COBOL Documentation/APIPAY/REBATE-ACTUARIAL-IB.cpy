      *********************************
      *    ACTUARIAL (IB)     (6)
      *
      *    (INTEREST BEARING METHOD)
      *********************************
       REBATE-ACTUARIAL-IB SECTION.
           PERFORM LOAN-CALCULATIONS.
           IF PRINCIPLE < TOTAL-NOTE
              COMPUTE REB-REBATE ROUNDED =
                   (TOTAL-NOTE - PRINCIPLE - LN-TOTINT)
                   / (TOTAL-NOTE - PRINCIPLE) * REB-TOTCHG.
       REBATE-ACTUARIAL-IB-EXIT.
           EXIT.

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
