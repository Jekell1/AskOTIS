      *****************************************************
      *    INSURANCE ANTICIPATED
      *      CFSC - ASSURANT - STATE OF IN
      *
      * N - DISCOUNT RATE .037
      * P - DISCOUNT RATE .0142
      *
      *    THIS ROUTINE CALCULATES TO INSURANCE  PREMIUM
      *    TO COVER THE REMAINING TERM ON THE COVERAGE
      *    REMAINING, AND REBATES THAT AMOUNT.
      *
      *****************************************************
       REBATE-ANTICIPATED-N-P SECTION.

      *--->MOVE REB-SUB TO REB-INSSUB.
      *--->PERFORM REBATE-FIND-INS-RATE.

           COMPUTE REBM = ITRM-INSTERM - REB-ELAPSED-MONTHS.
           COMPUTE REBR = LN-ORGTERM - ITRM-INSTERM.

           IF SP-RBFRMLA2(REB-SUB) = "P"
              COMPUTE REBJ = (( 1 + 0.0142)**(1/12)) - 1
           ELSE
              COMPUTE REBJ = (( 1 + 0.037)**(1/12)) - 1.
           COMPUTE REBWK1 = 1 / (( 1 + REBJ ) ** ITRM-INSTERM).
           COMPUTE REBWK2 = 1 / (( 1 + REBJ ) ** REBM).
           COMPUTE REBAN-M-1 = (1 - (1/(1+REBJ)) ** ITRM-INSTERM) / REBJ.
           COMPUTE REBAN-M-1-T = (1 - (1/(1+REBJ))** REBM) / REBJ.

           COMPUTE REB-REBATE ROUNDED =
                REB-TOTCHG *
                ((REBM - (REBR * REBWK2) - REBAN-M-1-T) /
                 (LN-ORGTERM - (REBR * REBWK1)  - REBAN-M-1)).

       REBATE-ANTICIPATED-N-EXIT.
           EXIT.

      **********************************************************************
      *    "O": RULE OF ANTICIPATION FOR LIFE REFUND - STATE OF TEXAS
      *
      *      GROSS LIFE COVERAGE, SINGLE PREMIUM PER $100 PER YEAR STATES,
      *                               WITH DISCOUNT
      *
      * DEFINITIONS:
      *
      * DS-RATE= .035   FIXED
      * OP     = ORIGINAL PREMIUM (REB-TOTCHG)
      * M      = ORIG INS TERM IN MONTHS (ITRM-INSTERM)
      * T      = ELAPSED MONTHS FROM INCEPTION TO REFUND (REB-ELAPSED-MONTHS)
      * DSRATE = DISCOUNT RATE (REB-MANYDECIMALS)
      *
      *                      1 + (DSRATE * M)/24     (M-T)*(M-T)
      * REFUND = ORIG PREM * --------------------- * -----------
      *                      1 + (DSRATE * M-T)/24     (M * M)
      *
      **********************************************************************
