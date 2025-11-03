      *********************************************************
       POST-Z2 SECTION.
      *********************************************************

      * SETUP-LP-RECORD

           PERFORM SETUP-LP-RECORD.

           MOVE "Z2" TO LP-TRCD.

      * ASSURE THAT $0.01 REMAININGS IN PRINCIPAL

           IF LN-OT2BAL > (BP-TRAMT - VALID-POSTING-NOPOFF)
              COMPUTE TEST-AMT
                      LP-TRAMT
                      LP-APOT2 = BP-TRAMT - VALID-POSTING-NOPOFF
           ELSE
              MOVE LN-OT2BAL TO TEST-AMT
                                LP-TRAMT
                                LP-APOT2.

      * SETUP-LONPF-BUFFER
           MOVE " "  TO HOLD-POCD.
           MOVE "Z2" TO HOLD-BP-TRCD.

           PERFORM SETUP-LONPF-BUFFER.

      * Z2 POSTING
           MOVE "LP/LONPFC" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME PROG-BUF
                                           UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPFC DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD NOT = " "
              MOVE "LONPFC: PMT NOT APPLIED   " TO LOG-MSG
              MOVE 73                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              MOVE "E" TO POSTING-ERRCD
              GO TO POST-Z2-EXIT.

      * Z2 UPDATE

           MOVE "LP/LONPF2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPF2 DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD = "E" OR "X" OR "M"
              MOVE "LONPF2: PMT NOT APPLIED  " TO LOG-MSG
              MOVE 69                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              MOVE "E" TO POSTING-ERRCD
              GO TO POST-Z2-EXIT.

       POST-Z2-EXIT.
           EXIT.

      *********************************************************
