      *****************************************************
      *    CALIFORNIA A & H ANTICIPATED     (R)
      *      NREW FORMULA EFFECTIVE: 05-14-94
      *
      *    THIS ROUTINE CALCULATES TO INSURANCE PREMIUM
      *    TO COVER THE REMAINING TERM ON THE COVERAGE
      *    REMAINING, AND REBATES THAT AMOUNT.
      *****************************************************
       REBATE-CA-ANTICIPATED SECTION.
           MOVE 2 TO ITRM-SUB.
           PERFORM INS-TERM-CALCULATION.

           MOVE REB-REMTERM TO REB-WORKER.
           MOVE ITRM-INSTERM TO REB-REMTERM.
           MOVE REB-AHSUB TO REB-INSSUB.
           PERFORM REBATE-FIND-INS-RATE.
           MOVE REB-WORKER TO REB-REMTERM.

           COMPUTE REBF ROUNDED =
               REB-TOTCHG / (ITRM-INSTERM * REB-MANYDECIMALS).

           MOVE REB-AHSUB TO REB-INSSUB.
           PERFORM REBATE-FIND-INS-RATE.

           COMPUTE REB-REBATE ROUNDED =
               REBF * REB-REMTERM * REB-MANYDECIMALS.

       REBATE-CA-ANTICIPATED-EXIT.
           EXIT.

      *****************************************************
      *    CALIFORNIA 'CL' ANTICIPATED     (S)
      *    COSMO
      *
      *****************************************************
