       APRZ-APR-PROCESS.
           IF APRZ-WS-REG-TERM = 1
              MOVE ZERO TO APRZ-WS-REG-TERM
              MOVE ZERO TO APRZ-WS-LAST-TERM
           ELSE
             IF APRZ-WS-LASTPYAMT = ZERO
                SUBTRACT 1 FROM APRZ-WS-REG-TERM
                MOVE ZERO TO APRZ-WS-LAST-TERM
             ELSE
                SUBTRACT 1 FROM APRZ-WS-REG-TERM
                MOVE APRZ-WS-REG-TERM TO APRZ-WS-LAST-TERM
                SUBTRACT 1 FROM APRZ-WS-REG-TERM
             END-IF
           END-IF

           PERFORM VARYING APRZ-WS-CNTR FROM 0 BY 1
                   UNTIL APRZ-WS-CNTR > 49 OR
                         FUNCTION ABS(APRZ-WS-ADJ) NOT > 0.0001

             MOVE APRZ-WS-R TO APRZ-WS-AF-R
             PERFORM APRZ-APR-FIN
             MOVE APRZ-WS-APRW TO APRZ-WS-F0

             COMPUTE APRZ-WS-AF-R = APRZ-WS-R + APRZ-WS-DELTA
             PERFORM APRZ-APR-FIN
             MOVE APRZ-WS-APRW TO APRZ-WS-F1

             COMPUTE APRZ-WS-ADJ = APRZ-WS-DELTA *
                              (APRZ-WS-FINANCED - APRZ-WS-F0 ) /
                              (APRZ-WS-F1 - APRZ-WS-F0)
             COMPUTE APRZ-WS-R = APRZ-WS-R + APRZ-WS-ADJ

           END-PERFORM

           IF APRZ-WS-R < ZERO OR APRZ-WS-CNTR > 50
              MOVE ZERO TO APRZ-WS-APR
           ELSE
              IF APRZ-WS-R > 999.9999
                 MOVE 999.9999 TO APRZ-WS-APR
              ELSE
                 MOVE APRZ-WS-R TO APRZ-WS-APR
              END-IF
           END-IF
           COMPUTE APRZ-WS-APR =
                        FUNCTION INTEGER ( (APRZ-WS-APR * 10000) + 0.5 )



           COMPUTE APR-APR = APRZ-WS-APR / 10000
           .

