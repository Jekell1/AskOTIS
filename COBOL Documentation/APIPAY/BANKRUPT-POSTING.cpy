      ******************************************************************
       BANKRUPT-POSTING SECTION.
      ******************************************************************

      * RESTORE LOAN TO ITS ORIG STATE ELSE WILL ERROR OUT OF LONPF2
      * (LN-REC == WK-SAVE-REC)

           MOVE ORIG-LN-LBOX    TO LN-LBOX.
           MOVE ORIG-LN-ALLOTCD TO LN-ALLOTCD.

           PERFORM SETUP-LP-RECORD.

           MOVE BP-TRAMT          TO LP-TRAMT.
           MOVE BP-BNKRPT-CASE-NO TO HOLD-BNKRPT-CASE-NO.
           MOVE BP-BNKRPT-STATUS  TO HOLD-BNKRPT-STATUS.
           MOVE LN-OTHBAL         TO LP-OTHBAL.
           MOVE LN-OT2BAL         TO LP-OT2BAL.
           MOVE LN-INTBAL         TO LP-INTBAL.
           MOVE LN-LCBAL          TO LP-LCBAL.
           MOVE LN-CURBAL         TO LP-CURBAL.
           MOVE LP-PAYDATE        TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE IBPC-FG           TO LP-IBPC.
           MOVE LN-CURBAL         TO LP-INTDUE.
           MOVE LN-1STPYDATE      TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL    TO LP-PDTH-DATE.
           ADD LP-APLC            TO HOLD-LN-TOTERN HOLD-LN-TOTLCHG.
           ADD LP-APINT           TO HOLD-LN-TOTERN HOLD-LN-TOTINT.

      * SETUP-LONPF-BUFFER
           MOVE 0 TO HOLD-OVPAID-CHECK.

      * AUTOMATIC BATCH TRCD = "PA" FROM BPBKBR.C
      * MANUAL BATCH MAY BE "PA" OR "PY"

           MOVE BP-TRCD TO HOLD-BP-TRCD LP-TRCD.

           PERFORM SETUP-LONPF-BUFFER.

      * PAYMENT UPDATE

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
              MOVE "E" TO POSTING-ERRCD.

       BANKRUPT-POSTING-EXIT.
           EXIT.

      ******************************************************************
