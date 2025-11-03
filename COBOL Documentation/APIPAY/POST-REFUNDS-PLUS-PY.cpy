      *********************************************************
       POST-REFUNDS-PLUS-PY SECTION.
      *********************************************************

           MOVE 0 TO SUB1.
       POST-REFUNDS-PLUS-PY-AGAIN.
           ADD 1 TO SUB1.
           IF SUB1 > 13
              GO TO POST-REFUNDS-PLUS-PY-NOW.

           IF SUB1 = 1
              IF POFF-REBATE(1) NOT = 0
                 MOVE "RC" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB1) NOT = 0 AND
                    LN-REBATE(SUB1) = " "
                    MOVE "RC" TO HOLD-BP-TRCD
                 ELSE
                    GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 2
              IF POFF-REBATE(2) NOT = 0
                 MOVE "RA" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB1) NOT = 0 AND
                    LN-REBATE(SUB1) = " "
                    MOVE "RA" TO HOLD-BP-TRCD
                 ELSE
                    GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 3
              IF POFF-REBATE(3) NOT = 0
                 MOVE "RP" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB1) NOT = 0 AND
                    LN-REBATE(SUB1) = " "
                    MOVE "RP" TO HOLD-BP-TRCD
                 ELSE
                    GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 4
              IF POFF-REBATE(4) NOT = 0
                 MOVE "RI" TO HOLD-BP-TRCD
              ELSE
                 GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 5
              IF POFF-REBATE(5) NOT = 0
                 MOVE "RS" TO HOLD-BP-TRCD
              ELSE
                 GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 6
              IF POFF-REBATE(6) NOT = 0
                 MOVE "RM" TO HOLD-BP-TRCD
              ELSE
                 GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 7
              IF POFF-REBATE(7) NOT = 0
                 MOVE "RD" TO HOLD-BP-TRCD
              ELSE
                 GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 8
              GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 9
              IF POFF-REBATE(9) NOT = 0
                 MOVE "R1" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB1 - 5) NOT = 0 AND
                    LN-REBATE(SUB1) = " "
                    MOVE "R1" TO HOLD-BP-TRCD
                 ELSE
                    GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 10
              IF POFF-REBATE(10) NOT = 0
                 MOVE "R2" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB1 - 5) NOT = 0 AND
                    LN-REBATE(SUB1) = " "
                    MOVE "R2" TO HOLD-BP-TRCD
                 ELSE
                    GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 11
              IF POFF-REBATE(11) NOT = 0
                 MOVE "R3" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB1 - 5) NOT = 0 AND
                    LN-REBATE(SUB1) = " "
                    MOVE "R3" TO HOLD-BP-TRCD
                 ELSE
                    GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 12
              IF POFF-REBATE(12) NOT = 0
                 MOVE "R4" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB1 - 5) NOT = 0 AND
                    LN-REBATE(SUB1) = " "
                    MOVE "R4" TO HOLD-BP-TRCD
                 ELSE
                 GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 13
              IF POFF-REBATE(13) NOT = 0
                 MOVE "R5" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB1 - 5) NOT = 0 AND
                    LN-REBATE(SUB1) = " "
                    MOVE "R5" TO HOLD-BP-TRCD
                 ELSE
                    GO TO POST-REFUNDS-PLUS-PY-AGAIN.

           MOVE " " TO ERRCD.
           PERFORM SETUP-LP-RECORD.
           MOVE POFF-REBATE(SUB1) TO LP-TRAMT LP-APCUR.
           MOVE HOLD-BP-TRCD      TO LP-TRCD.

      * SETUP LONPF BUFFER

      * THIS SET OF 'F9' TRIGGERS LONPFB.C TO DO A REFUND OF INTEREST
      * WITH "PO" IN REB-LPTRCD, LIKE CRNOR2.C

           IF HOLD-BP-TRCD = "RI"
              MOVE "F9" TO HOLD-BP-TRCD.

           PERFORM SETUP-LONPF-BUFFER.

           MOVE "LP/LONPFB" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME PROG-BUF
                                          UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPFB DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD NOT = " "
              MOVE "LONPFB: REFUND NOT APPLIED" TO LOG-MSG
              MOVE 67                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              MOVE "E" TO POSTING-ERRCD
              GO TO POST-REFUNDS-PLUS-PY-EXIT.

      * UPDATE PROGRAM
           MOVE "LP/LONPF2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPF2 DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD = "E" OR "X" OR "M"
              MOVE "LONPF2: REFUND NOT UPDATED" TO LOG-MSG
              MOVE 74                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              MOVE "E" TO POSTING-ERRCD
              GO TO POST-REFUNDS-PLUS-PY-EXIT.

      * READ UPDATED LOAN
           PERFORM READ-LN1-FILE.

           GO TO POST-REFUNDS-PLUS-PY-AGAIN.

       POST-REFUNDS-PLUS-PY-NOW.

      * READ UPDATED LOAN (ONE MORE TIME, IN CASE THERE WERE NO REBATES)

           PERFORM READ-LN1-FILE.

      * MUST SET TO " " TO GET DEFAULT BP-TRCD SET:
           MOVE " " TO HOLD-BP-TRCD.

      * MERCURY RE:256X
      * MUST SET ORIG-LN-.... TO LN-..... VALUES SINCE THEY HAVE
      * CHANGED WHEN LONPF2.C WAS CALLED ABOVE AND PAYMENT-POSTING
      * ROUTINE SET THEM TO THE ORIGINAL VALUES WHEN THE LOAN WAS
      * FIRST PROCESSED.

           MOVE LN-LBOX    TO ORIG-LN-LBOX.
           MOVE LN-ALLOTCD TO ORIG-LN-ALLOTCD.

           PERFORM PAYMENT-POSTING.

           MOVE WK-LP-REC TO LP-REC.

       POST-REFUNDS-PLUS-PY-EXIT.
           EXIT.

      ******************************************************************
