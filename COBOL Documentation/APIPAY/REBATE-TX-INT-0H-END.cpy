       REBATE-TX-INT-0H-END.
      *    MOVE REBTX-CALC-INT-TOT TO
      *                    REBTX-CALC-INT-X.
      *    MOVE REBTX-CALC-INT-X TO MESS.
      *    PERFORM SEND-MESS.
           COMPUTE REB-REBATE =
              1.50 + LN-INTCHG + LN-EXTCHG - REBTX-CALC-INT-TOT.
           IF REB-REBATE < 0
              MOVE 0 TO REB-REBATE.
           IF REB-REBATE > (LN-INTCHG + LN-EXTCHG)
              COMPUTE REB-REBATE = LN-INTCHG + LN-EXTCHG.
           GO TO REBATE-TX-INT-0H-EXIT.

