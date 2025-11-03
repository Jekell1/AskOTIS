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
       REBATE-INS-ANTICIPATED SECTION.
           MOVE REB-SUB TO REB-INSSUB.
           PERFORM REBATE-FIND-INS-RATE.

      *    CREDIT LIFE
           IF REB-SUB < 5
              MOVE 1 TO ITRM-SUB
           ELSE
      *       A&H & PP
              IF REB-SUB < 7
                 SUBTRACT 3 FROM REB-SUB GIVING ITRM-SUB
              ELSE
      *          O1, O2, O3, O4, O5
                 SUBTRACT 8 FROM REB-SUB GIVING ITRM-SUB.

           PERFORM INS-TERM-CALCULATION.

      * DO NOT TRUNCATE THE RATE FROM THE SPR TABLE !
           IF SP-RBFRMLA2(REB-SUB) = "M"
              COMPUTE REB-REBATE ROUNDED =
                  REB-LN-INSCOVR / ITRM-INSTERM
                      * REB-REMTERM * REB-MANYDECIMALS / 100
           ELSE
              COMPUTE REB-REBATE ROUNDED =
                  REB-LN-INSCOVR / ITRM-INSTERM
                      * REB-REMTERM * REB-HUNDREDS / 100.

       REBATE-INS-ANTICIPATED-EXIT.
           EXIT.

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
