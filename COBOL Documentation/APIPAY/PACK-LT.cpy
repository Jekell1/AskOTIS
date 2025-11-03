       UNPACK-LT SECTION.
           MOVE 0 TO SCHD-SUB3.
           PERFORM MOVE-PAYMENT
              UNTIL SCHD-SUB3 = LTP-SCHLD-TERM(SCHD-SUB).
           ADD 1 TO SCHD-SUB.

       EXIT-UNPACK-LT.
           EXIT.

