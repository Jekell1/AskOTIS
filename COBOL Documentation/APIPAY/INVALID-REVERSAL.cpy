       INVALID-REVERSAL.
           MOVE "N" TO VALID-REVERSAL-FG.
           MOVE "NO VALID PMT FOR REVERSAL" TO LOG-MSG.
           MOVE 60                 TO RETURN-STATUS.
           IF LP-PAYDATE NOT = BP-PAYDATE
              MOVE 61                 TO RETURN-STATUS
              MOVE "NO REVERSE: NO DATE MATCH" TO LOG-MSG.
           IF LP-TRAMT NOT = BP-TRAMT
              MOVE 62                 TO RETURN-STATUS
              MOVE "NO REVERSE: NO AMT MATCH"  TO LOG-MSG.
           IF LP-REFNO NOT = BP-REFCD
              MOVE 63                 TO RETURN-STATUS
              MOVE "NO REVERSE:NO REFNO MATCH" TO LOG-MSG.
           IF LP-TRCD = "RV" OR LP-REV = "Y"
              MOVE 64                 TO RETURN-STATUS
              MOVE "NO REVERSE:LAST PMT IS RV" TO LOG-MSG.
           IF LP-TRCD NOT = "PA"
              MOVE 65                 TO RETURN-STATUS
              MOVE "NO REVERSE: NOT A 'PA'   " TO LOG-MSG.
           PERFORM CREATE-LOG.

