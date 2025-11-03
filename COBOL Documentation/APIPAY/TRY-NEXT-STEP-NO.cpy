       TRY-NEXT-STEP-NO.
           MOVE STEP-COUNT TO EX-SPOOL-STEP.
           MOVE EX-SPOOL TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
             IF (EX-SPOOL-NAME = "REPOEX")
                GO TO EXIT-CREATE-SPOOL-DIR
             ELSE
             IF STEP-COUNT < 999
                ADD 1 TO STEP-COUNT
                GO TO TRY-NEXT-STEP-NO.

