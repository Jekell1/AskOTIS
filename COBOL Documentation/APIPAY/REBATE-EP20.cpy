      *************************************************
      * SOUTH CAROLINA SERVICE CHARGE REFUND,
      * IF CASH ADVANCE < $150.01, NO REFUND
      *************************************************
       REBATE-EP20 SECTION.
           PERFORM LOAN-CALCULATIONS.
           IF CASH-ADVANCE < 150.01
              MOVE 0 TO REB-REBATE.
           MOVE 0 TO REB-SUB.

       REBATE-EP20-EXIT.
           EXIT.

      ********************************************
      * FREEDOM, AL ANY PAYOFF WITHIN 90 DAYS IS PRO RATA
      *             AFTER 90 DAYS, NO REBATE.
      * REGMGM, AL (25) ANY PAYOFF WITHIN 90 DAYS IS PRO RATA
      *             AFTER 90 DAYS, USE SPR TABLE FORMULA'S
      ********************************************
