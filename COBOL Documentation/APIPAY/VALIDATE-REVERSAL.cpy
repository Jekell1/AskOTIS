      ************************************
       VALIDATE-REVERSAL SECTION.
      ************************************
           MOVE "Y" TO VALID-REVERSAL-FG.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF (IO-FG NOT = 0) OR (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO INVALID-REVERSAL.

           IF LP-TRCD = "RV" OR LP-REV = "Y"
              GO TO INVALID-REVERSAL.

           IF LP-TRCD NOT = "PA"
              GO TO INVALID-REVERSAL.

           IF LP-PAYDATE NOT = BP-PAYDATE
              OR LP-TRAMT NOT = BP-TRAMT
                 OR LP-REFNO NOT = BP-REFCD
                    GO TO INVALID-REVERSAL.

           MOVE 0      TO LP-TILLNO.

           MOVE LP-REC TO WK-LP-REC.

           GO TO EXIT-VALIDATE-REVERSAL.
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

       EXIT-VALIDATE-REVERSAL.
           PERFORM CLOSE-LP1-FILE.

      ******************************************
