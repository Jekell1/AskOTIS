      ******************************************************************
       PAYOFF-POSTING SECTION.
      ******************************************************************

      * RESTORE LOAN TO ITS ORIG STATE ELSE WILL ERROR OUT
      * OF LONPF2 (LN-REC == WK-SAVE-REC)

           MOVE ORIG-LN-LBOX    TO LN-LBOX.
           MOVE ORIG-LN-ALLOTCD TO LN-ALLOTCD.
           MOVE 0               TO HOLD-OVPAID-CHECK.

      * NO O2 MONIES

           IF LN-OT2BAL = 0
              IF ( (CD-BR-ALLOT-OPTION = "Y"      ) AND
                   (BR-BP-PAYOFF-OV = "O" OR "M"  ) )
                 OR
                 ( (CD-BR-LBOX-OPTION = "Y"       ) AND
                   (BR-LBOX-PAYOFF-OV = "O" OR "M") )
                 SUBTRACT WILL-POFF-ACCOUNT FROM BP-TRAMT
                                      GIVING HOLD-OVPAID-CHECK
                                             OVPAID-AMT
                 IF OVPAID-AMT NOT = 0
                    MOVE OVPAID-AMT TO DISPLAY-OVPAID-AMT
                    MOVE "OVER PAID = ###########" TO LOG-MSG
                    INSPECT LOG-MSG REPLACING
                          FIRST "###########" BY DISPLAY-OVPAID-AMT
                    MOVE 70                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                 END-IF
                 MOVE WILL-POFF-ACCOUNT TO BP-TRAMT
                 PERFORM POST-PO
                 IF POSTING-ERRCD NOT = " "
                    GO TO PAYOFF-POSTING-EXIT
                 END-IF
              ELSE
                 MOVE 0 TO HOLD-OVPAID-CHECK
                 SUBTRACT WILL-POFF-ACCOUNT FROM BP-TRAMT
                                        GIVING OVPAID-AMT
                 MOVE WILL-POFF-ACCOUNT TO BP-TRAMT
                 IF BP-TRAMT NOT = 0
                    PERFORM POST-PO
                    IF POSTING-ERRCD NOT = " "
                       GO TO PAYOFF-POSTING-EXIT
                    END-IF
                 END-IF
                 IF OVPAID-AMT NOT = 0
                    MOVE OVPAID-AMT TO DISPLAY-OVPAID-AMT
                    MOVE "OVER PAID = ###########" TO LOG-MSG
                    INSPECT LOG-MSG REPLACING
                          FIRST "###########" BY DISPLAY-OVPAID-AMT
                    MOVE "O" TO POSTING-ERRCD
                    MOVE 70                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                 END-IF
              END-IF
           ELSE
      * O2 MONIES EXIST SO EITHER PAYOFF OR REFUND LEAVING $0.01 IN PRIN
              PERFORM POST-Z2
              IF POSTING-ERRCD NOT = " "
                 GO TO PAYOFF-POSTING-EXIT
              END-IF
              SUBTRACT TEST-AMT FROM BP-TRAMT
              PERFORM READ-LN1-FILE
              IF IO-FG NOT = 0
                 MOVE "AFTER Z2, LOAN MISSING" TO LOG-MSG
                 MOVE 71                 TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 MOVE "E" TO POSTING-ERRCD
                 GO TO PAYOFF-POSTING-EXIT
              END-IF
              IF ( (CD-BR-ALLOT-OPTION = "Y"      ) AND
                   (BR-BP-PAYOFF-OV = "O" OR "M"  ) )
                 OR
                 ( (CD-BR-LBOX-OPTION = "Y"       ) AND
                   (BR-LBOX-PAYOFF-OV = "O" OR "M") )
                 IF WILL-POFF-ACCOUNT >= BP-TRAMT
                    MOVE 0 TO HOLD-OVPAID-CHECK
                              OVPAID-AMT
      * REFUNDS + "PY"
                    IF BP-TRAMT > 0
                       PERFORM POST-REFUNDS-PLUS-PY
                    END-IF
                    GO TO PAYOFF-POSTING-EXIT
                 ELSE
                    SUBTRACT WILL-POFF-ACCOUNT FROM BP-TRAMT
                                      GIVING HOLD-OVPAID-CHECK
                                             OVPAID-AMT
                    MOVE OVPAID-AMT TO DISPLAY-OVPAID-AMT
                    MOVE "OVER PAID = ###########" TO LOG-MSG
                    INSPECT LOG-MSG REPLACING
                          FIRST "###########" BY DISPLAY-OVPAID-AMT
                    MOVE 70                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                 END-IF
                 PERFORM POST-PO
                 IF POSTING-ERRCD NOT = " "
                    GO TO PAYOFF-POSTING-EXIT
                 END-IF
              ELSE
                 MOVE 0 TO HOLD-OVPAID-CHECK
                 IF WILL-POFF-ACCOUNT >= BP-TRAMT
                    MOVE 0 TO OVPAID-AMT
      * REFUNDS + "PY"
                    IF BP-TRAMT > 0
                       PERFORM POST-REFUNDS-PLUS-PY
                    END-IF
                    GO TO PAYOFF-POSTING-EXIT
                 ELSE
                    SUBTRACT WILL-POFF-ACCOUNT FROM BP-TRAMT
                                        GIVING OVPAID-AMT
                    IF OVPAID-AMT NOT = 0
                      MOVE OVPAID-AMT TO DISPLAY-OVPAID-AMT
                      MOVE "OVER PAID = ###########" TO LOG-MSG
                      INSPECT LOG-MSG REPLACING
                           FIRST "###########" BY DISPLAY-OVPAID-AMT
                       MOVE 70                 TO RETURN-STATUS
                       PERFORM CREATE-LOG
                    END-IF
                 END-IF
                 PERFORM POST-PO
              END-IF.

       PAYOFF-POSTING-EXIT.
           EXIT.

      *********************************************************
