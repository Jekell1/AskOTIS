      ******************************************************************
       PAYMENT-POSTING SECTION.
      ******************************************************************

      * RESTORE LOAN TO ITS ORIG STATE ELSE WILL ERROR OUT OF LONPF2
      * (LN-REC == WK-SAVE-REC)

           MOVE ORIG-LN-LBOX    TO LN-LBOX.
           MOVE ORIG-LN-ALLOTCD TO LN-ALLOTCD.

      * SETUP-LP-RECORD
           PERFORM SETUP-LP-RECORD.
           MOVE BP-TRAMT TO LP-TRAMT.

      * SETUP-LONPF-BUFFER
           MOVE 0 TO HOLD-OVPAID-CHECK.

           IF PAYOFF-TRANS-FG = "Y"
              MOVE "PO" TO HOLD-POCD.

      * AUTOMATIC BATCH TRCD = "PA" FROM BPBKBR.C
      * MANUAL BATCH MAY BE "PA" OR "PY"

           IF HOLD-BP-TRCD = " "
              MOVE BP-TRCD TO HOLD-BP-TRCD LP-TRCD
           ELSE
              MOVE "PA" TO LP-TRCD.

           PERFORM SETUP-LONPF-BUFFER.

      * PAYMENT POSTING

           IF (HOLD-BP-TRCD = "RE" OR "O2")
              MOVE "LP/LONPF7" TO FORM-NAM
           ELSE
           IF HOLD-BP-TRCD = "RP"
              MOVE "LP/LONPFB" TO FORM-NAM
           ELSE
           IF (HOLD-BP-TRCD = "PL" OR "P2" OR "P3")
              MOVE "LP/LONPF9" TO FORM-NAM
           ELSE
              MOVE "LP/LONPFC" TO FORM-NAM.

           MOVE FORM-PROGX TO PAYMENT-PROG.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                PROG-BUF  UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPFC DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD = "P"
              IF PAYMENT-PROG = "LONPFC"
                 MOVE "LONPFC: ALT PREPAYMENT    " TO LOG-MSG
                 PERFORM CREATE-LOG
                 MOVE 66                 TO RETURN-STATUS
                 MOVE "E" TO POSTING-ERRCD
                 GO TO PAYMENT-POSTING-EXIT.
           
           IF PAYMENT-PROG = "LONPFB"
              IF NOT (ERRCD = " " OR "P")
                 MOVE "######: PMT NOT APPLIED:&" TO LOG-MSG
                 INSPECT LOG-MSG REPLACING FIRST "######"
                                            BY PAYMENT-PROG
                 INSPECT LOG-MSG REPLACING FIRST "&"
                                            BY ERRCD
                 MOVE 67                 TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 MOVE "E" TO POSTING-ERRCD
                 GO TO PAYMENT-POSTING-EXIT
              END-IF
           ELSE
              IF ERRCD NOT = " "
                 MOVE "######: PMT NOT APPLIED:& " TO LOG-MSG
                 INSPECT LOG-MSG REPLACING FIRST "######"
                                            BY PAYMENT-PROG
                 INSPECT LOG-MSG REPLACING FIRST "&"
                                            BY ERRCD
                 MOVE 68                 TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 MOVE "E" TO POSTING-ERRCD
                 GO TO PAYMENT-POSTING-EXIT.

      * PAYMENT UPDATE

           MOVE 0 TO HOLD-PRIOR-DF-POSTDATE.

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
              GO TO PAYMENT-POSTING-EXIT.

       PAYMENT-POSTING-EXIT.
           EXIT.

      ******************************************************************
