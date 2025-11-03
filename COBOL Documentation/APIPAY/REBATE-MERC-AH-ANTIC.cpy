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
       REBATE-MERC-AH-ANTIC SECTION.
           MOVE REB-AHSUB TO REB-INSSUB.
           PERFORM REBATE-FIND-INS-RATE.
           PERFORM LOAN-CALCULATIONS.

           COMPUTE REB-REBATE ROUNDED = (TOTAL-NOTE / LN-ORGTERM) *
                          REB-MANYDECIMALS * REB-REMTERM / 100.
      * OREGON OPTION:
           IF SP-RBSPOPT1(REB-SUB) = 15
              COMPUTE REB-WORKER ROUNDED = REB-TOTCHG * 0.9
              COMPUTE REB-WORK2 ROUNDED = REB-TOTCHG - 75.00
              IF REB-WORK2 > REB-WORKER
                 MOVE REB-WORK2 TO REB-WORKER
              END-IF
              IF REB-WORKER < REB-REBATE
                 MOVE REB-WORKER TO REB-REBATE.

       REBATE-MERC-AH-ANTIC-EXIT.
           EXIT.

      *****************************************************
      *                     (0 7)
      *    MULLEN 'CL' AND 'AH' REFUNDS PROTECTIVE LIFE
      *           TRUNCATED NET LIFE COVERAGE
      *           NON RETRO 14 DAY AH COVERAGE
      *
      *****************************************************
