       SKIP-PAYOFF-CALC.

      * DON'T ALLOW POSTING WHEN LOAN HAS ZERO BALANCE

           IF LN-CURBAL = 0
              MOVE "ALREADY ZERO BALANCE" TO LOG-MSG
              MOVE 46                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

      * TEST FOR VALID GENERAL LEDGER INTERFACE RECORD:

           MOVE LN-OWNBR TO GI-BRANCH.
           MOVE LN-CLASS TO GI-CLASS.
           PERFORM READ-GI1-FILE.
           IF IO-FG NOT = 0
              MOVE "MISSING G/L INTERFACE " TO LOG-MSG
              MOVE 58                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           MOVE " " TO POSTING-ERRCD.

           IF CD-BR-FILE-TYPE = "B"
              PERFORM BANKRUPT-POSTING
           ELSE
           IF BP-TRCD = "RV"
              PERFORM REVERSAL-POSTING
           ELSE
           IF BP-TRCD = "OT"
              PERFORM OTHER-CHG-POSTING
           ELSE
              IF (BP-TRAMT > VALID-POSTING-NOPOFF) AND
                 (HOLD-BP-TRCD = "99"            )
                 PERFORM PAYOFF-POSTING
              ELSE
                 PERFORM PAYMENT-POSTING.

           IF NOT (POSTING-ERRCD = " " OR "O")
              MOVE "GENERIC POSTING ERROR" TO LOG-MSG
              MOVE 59                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.
            
           MOVE 0                   TO RETURN-STATUS.

           MOVE "SUCCESSFUL UPDATE" TO LOG-MSG
           PERFORM CREATE-LOG.

           PERFORM CLOSE-OP-FILE.

