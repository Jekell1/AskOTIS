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
       REBATE-ANTICIPATED-O SECTION.

           MOVE REB-SUB TO REB-INSSUB.
           PERFORM REBATE-FIND-INS-RATE.

           COMPUTE REBM = ITRM-INSTERM - REB-ELAPSED-MONTHS.

           COMPUTE REBWK1 = (1 + (.035 * ITRM-INSTERM)/24)/
                            (1 + (.035 * REBM)/24).

           COMPUTE REBWK2 = (REBM*REBM)/(ITRM-INSTERM*ITRM-INSTERM).

           COMPUTE REB-REBATE ROUNDED = REB-TOTCHG * REBWK1 * REBWK2.

       REBATE-ANTICIPATED-O-EXIT.
           EXIT.

      ******************************************
      *    ACTUARIAL REBATE ROUTINE   C
      ******************************************
      * ROUTINE IS EXCLUSIVELY FOR INTEREST:

