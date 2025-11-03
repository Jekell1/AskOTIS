      ***********************************************************
      *                     (0 7)
      *    MULLEN 'AH' REFUND PROTECTIVE LIFE
      *           NON RETRO 14 DAY AH COVERAGE
      *
      *    REFUND = PREMIUM * REMAIN-TERM / ORIG-TERM
      *                * REMAIN-TERM-RATE / ORIG-TERM-RATE - 10
      ***********************************************************
       REBATE-MULLEN-AH-07 SECTION.

      *   GET SINGLE PREMIUM AH RATE FOR INITIAL COVERED TERM:
      *   WILL STORE IN REB-WK UNTIL CALCULATION IS DONE
      *   AND WILL USE ITRM-INSTERM IN CALCULATION
           MOVE REB-REMTERM TO REB-WORK2.
           MOVE 2 TO ITRM-SUB.
           PERFORM INS-TERM-CALCULATION.
           MOVE ITRM-INSTERM TO REB-REMTERM.
           MOVE REB-AHSUB TO REB-INSSUB.
           PERFORM REBATE-FIND-INS-RATE.
           MOVE REB-MANYDECIMALS TO REB-WK.
           MOVE REB-WORK2 TO REB-REMTERM.

      *   GET SINGLE PREMIUM AH RATE FOR REMAINING TERM:
           MOVE REB-AHSUB TO REB-INSSUB.
           PERFORM REBATE-FIND-INS-RATE.

      *   DETERMINE AH REFUND:
           COMPUTE REB-REBATE ROUNDED =
              REB-TOTCHG * REB-REMTERM / ITRM-INSTERM
               * REB-MANYDECIMALS / REB-WK - 10.

       REBATE-MULLEN-AH-07-EXIT.
           EXIT.

      *****************************************************
      *                     (0 C)  NEWPORT  90%
      *                     (0 K)  MULLEN   95% AND USES INSEFF DATE
      *     COLLATERAL PROTECTION INSURANCE
      *     SHORT RATE REBATE CANCELLATION FORMULA
      *
      *****************************************************
