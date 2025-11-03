      ********************************************
      * FREEDOM, AL ANY PAYOFF WITHIN 90 DAYS IS PRO RATA
      *             AFTER 90 DAYS, NO REBATE.
      * REGMGM, AL (25) ANY PAYOFF WITHIN 90 DAYS IS PRO RATA
      *             AFTER 90 DAYS, USE SPR TABLE FORMULA'S
      ********************************************
       REBATE-EP22 SECTION.

           MOVE 365 TO ELAPSED-YRTYPE.
           MOVE REB-LN-INTDATE TO NUM-DATE.
           MOVE REB-PAYDATE TO SYS-DATE.
           PERFORM TIMALL.
      * AFTER 90 DAYS, SERVICE CHARGE IS FULLY EARNED, NO REBATE
           IF ELAPSED-DAYS > 90
              MOVE 0 TO REB-REBATE
              GO TO REBATE-EP22-EXIT.

      * EARNED
           COMPUTE REB-REBATE ROUNDED =
                            ( ELAPSED-DAYS * REB-TOTCHG / 90 ).

      * MUST EARN AT LEAST THE RBMINRET ($25.00)
      * REBATE
           IF REB-REBATE <  SP-RBMINRET(REB-SUB)
              COMPUTE REB-REBATE = REB-TOTCHG - SP-RBMINRET(REB-SUB)
           ELSE
               COMPUTE REB-REBATE = REB-TOTCHG - REB-REBATE.

           IF REB-REBATE < 0
              MOVE 0 TO REB-REBATE.

           IF REB-REBATE < SP-RBMIN(REB-SUB)
              MOVE 0 TO REB-REBATE.

      * TRIGGER THAT YOU WERE < 90 DAYS AND ALREADY COMPUTED
           MOVE 0 TO REB-SUB.

       REBATE-EP22-EXIT.
           EXIT.

      *******************************************************
      *    TFC SHORT RATE CANCELLATION TABLE REFUND
      *         CL/REBT04.C                               #96
      *******************************************************
