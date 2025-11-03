      *******************************************************************
      *                     FORMULA (0, 4)
      *    SHORT RATE 'TN' PP REFUND METHOD            TN, CONSUMER #1067
      *******************************************************************
       TN-PP-SHORT-RATE  SECTION.
           IF REB-ELAPSED-DAYS <= 0
              MOVE REB-TOTCHG TO REB-REBATE
              GO TO EXIT-TN-PP-SHORT-RATE.

           IF REB-LPTRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
              PERFORM REBATE-PRORATA-ROUTINE
              GO TO EXIT-TN-PP-SHORT-RATE.

      * COMPUTE TERM OF POLICY IN YEARS (T):
           COMPUTE REBJ ROUNDED = REB-ORGTERM / 12.

      * COMPUTE EARNED PREMIUM FACTOR (E):
           COMPUTE REB-WK ROUNDED =
              (REB-ELAPSED-DAYS / (REBJ * 365)) * 100.

      * DETERMINE INITIAL POLICY WRITING EXPENSE (I):
           IF REB-ELAPSED-DAYS <= (22 * REBJ)
              COMPUTE REB-UPR ROUNDED =
                  0.226 * (REB-ELAPSED-DAYS / REBJ) + 5
           ELSE
           IF REB-ELAPSED-DAYS <= (182 * REBJ)
              MOVE 10.1195 TO REB-UPR
           ELSE
              COMPUTE REB-UPR ROUNDED =
                   -0.054 * (REB-ELAPSED-DAYS / REBJ) + 20.1006.

      * COMPUTE REFUND = TOTCHG - (TOTCHG * REFUND PERCENT(E + I)):
           COMPUTE REB-REBATE ROUNDED =
               REB-TOTCHG - (REB-TOTCHG * (REB-WK + REB-UPR) * 0.01).

       EXIT-TN-PP-SHORT-RATE.
           EXIT.

      *****************************************************
      *                     (U)
      *    MERC. A & H ANTICIPATED
      *          AND 'CL' FOR OREGON RE: SP-RBSBOPT1 = '15'
      *
      *    THIS ROUTINE CALCULATES THE INSURANCE PREMIUM
      *    TO COVER THE REMAINING TERM ON THE COVERAGE
      *    REMAINING, AND REBATES THAT AMOUNT. BASED ON
      *    TOTAL NOTE.
      *****************************************************
