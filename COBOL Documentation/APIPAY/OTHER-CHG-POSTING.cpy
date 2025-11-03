      ******************************************************************
       OTHER-CHG-POSTING SECTION.
      ******************************************************************
      * RESTORE LOAN TO ITS ORIG STATE ELSE WILL ERROR OUT OF LONPF2
      * (LN-REC == WK-SAVE-REC)

           MOVE ORIG-LN-LBOX    TO LN-LBOX.
           MOVE ORIG-LN-ALLOTCD TO LN-ALLOTCD.

      * SETUP-LP-RECORD
           MOVE "OT"     TO BP-TRCD.
           PERFORM SETUP-LP-RECORD.

      * SETUP-LONPF-BUFFER
           MOVE 0 TO HOLD-OVPAID-CHECK.
           MOVE BP-TRCD TO HOLD-BP-TRCD LP-TRCD.

           PERFORM SETUP-LONPF-BUFFER.

      * OTHER CHARGES POSTING
           MOVE "LP/LONPF7" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                 PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPF7 DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD NOT = " "
              MOVE "LONPF7: 'OT' NOT APPLIED   " TO LOG-MSG
              PERFORM CREATE-LOG
              MOVE 82                            TO RETURN-STATUS
              MOVE "E" TO POSTING-ERRCD
              GO TO OTHER-CHG-POSTING-EXIT.

      * UPDATE

           MOVE 0 TO HOLD-PRIOR-DF-POSTDATE.

           MOVE "LP/LONPF2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                 PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPF2 DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD = "E" OR "X" OR "M"
              MOVE "LONPF2: PMT NOT APPLIED  " TO LOG-MSG
              MOVE 69 TO RETURN-STATUS
              PERFORM CREATE-LOG
              MOVE "E" TO POSTING-ERRCD
              GO TO OTHER-CHG-POSTING-EXIT.

       OTHER-CHG-POSTING-EXIT.
           EXIT.

      ******************************************************************
