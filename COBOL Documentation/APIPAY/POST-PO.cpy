      *********************************************************
       POST-PO SECTION.
      *********************************************************

      * SETUP-LP-RECORD

           PERFORM SETUP-LP-RECORD.
           MOVE BP-TRAMT TO LP-TRAMT.
           MOVE "PO"     TO LP-TRCD.

      * SETUP-LONPF-BUFFER

           MOVE "PO" TO HOLD-POCD.

           MOVE "99" TO HOLD-BP-TRCD.

           PERFORM SETUP-LONPF-BUFFER.

      * PAYOFF POSTING

           MOVE "LP/LONPFA" TO FORM-NAM.

           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPFC DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD NOT = " "
              MOVE "LONPFA: PMT NOT APPLIED   " TO LOG-MSG
              MOVE 72                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              MOVE "E" TO POSTING-ERRCD
              GO TO POST-PO-EXIT.

      * PAYOFF UPDATE

           MOVE "LP/LONPF2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                PROG-BUF  UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPF2 DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD = "E" OR "X" OR "M"
              MOVE "LONPF2: PMT NOT APPLIED  " TO LOG-MSG
              MOVE 69                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              MOVE "E" TO POSTING-ERRCD
              GO TO POST-PO-EXIT.

       POST-PO-EXIT.
           EXIT.

      *********************************************************
