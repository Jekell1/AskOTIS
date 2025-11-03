      *****************************************************
      *                     (GA)
      *    SERCHG REBATE FRMLA   H -   ROGER WILLIAMS
      *                          I -   JOE DOUGLAS SR
      *
      *    THIS ROUTINE CALCULATES THE SERVICE CHARGE
      *    REBATE, INSURING INTEREST AND SERVICE CHG
      *    EARNINGS DON'T EXCEED 5% PER MONTH.
      *****************************************************
       REBATE-GA-SC SECTION.
      * COMPUTE ORIGINAL BALANCE:
           IF SP-RBFRMLA(REB-SUB) = "H"
              COMPUTE REB-REBATE = LN-LNAMT - LN-INTCHG
                        - LN-SERCHG - (LN-MAINTFEE * REB-LN-ORGTERM)
           ELSE
              COMPUTE REB-REBATE = LN-LNAMT
                         - (LN-MAINTFEE * REB-LN-ORGTERM).

      * IF SCFRMLA 54 DO NOT INCLUDE 4% CLOSING FEE WHEN CALCULATING
      *  REBATE FOR REMAINING PART OF THE SC.
           IF SP-RBFRMLA(REB-SUB) = "I" AND SP-SCFRMLA = 54
              SUBTRACT LN-POINTS FROM REB-REBATE.

      * COMPUTE MONTHLY PAYMENT:
           COMPUTE REB-WORKER = REB-LN-REGPYAMT - LN-MAINTFEE.

      * COMPUTE MAXIMUM EARNINGS (INT + SER):
           COMPUTE REB-REBATE ROUNDED = .05 *
             ( (REB-ELAPSED-MONTHS * REB-REBATE) -
                (REB-ELAPSED-MONTHS - 1) * REB-ELAPSED-MONTHS / 2
                    * REB-WORKER).

      * COMPUTE REBATE OF SERCHG TAKING MAXIMUM (SC) INCOME:
           IF LN-ANTICADJ(1) NOT = 0
              MOVE LN-ANTICADJ(1) TO REB-STATEWK.

           COMPUTE REB-REBATE = (LN-SERCHG - LN-POINTS)
                    - ( REB-REBATE - (LN-INTCHG - REB-STATEWK) ).

           IF REB-REBATE < 0
              MOVE 0 TO REB-REBATE.

      *  IF SERVICE CHARGE FORMULA 54 - GA 4% CLOSING FEE,
      *  ADD IN THE AMOUNT OF REBATE CALCULATED BY THE EARLY PROVISION.
           IF SP-RBFRMLA(REB-SUB) = "I" AND SP-SCFRMLA = 54
              PERFORM REBATE-EP54
      *       ADD REB-WORK2 TO REB-REBATE
              ADD REB-EP54-AMT TO REB-REBATE.

       REBATE-GA-SC-EXIT.
           EXIT.

      *****************************************************
      *    CALIFORNIA A & H ANTICIPATED     (L)
      *    MULLEN FINANCE
      *
      *****************************************************
