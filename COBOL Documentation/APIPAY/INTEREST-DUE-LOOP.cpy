       INTEREST-DUE-LOOP.
           IF INDU-CURBAL = 0
              GO TO INTEREST-DUE-FINAL.
           SET CSTP UP BY 1.
           IF CSTP > SP-CAL-NORATES(CTBL)
              GO TO INTEREST-DUE-FINAL.
           IF SP-CAL-BREAK(CTBL CSTP) = 0
              MOVE INDU-CURBAL TO INDU-AMOUNT
           ELSE
              IF CSTP = 1
                 MOVE SP-CAL-BREAK(CTBL CSTP) TO INDU-AMOUNT
              ELSE
                 SUBTRACT SP-CAL-BREAK(CTBL CSTP - 1) FROM
                             SP-CAL-BREAK(CTBL CSTP)
                                GIVING INDU-AMOUNT.
           IF INDU-CURBAL > INDU-AMOUNT
              SUBTRACT INDU-AMOUNT FROM INDU-CURBAL
           ELSE
              MOVE INDU-CURBAL TO INDU-AMOUNT
              MOVE 0 TO INDU-CURBAL.
           COMPUTE INDU-WORKER ROUNDED = INDU-WORKER +
              (SP-CAL-RATE(CTBL CSTP) / 100) * INDU-AMOUNT.
           GO TO INTEREST-DUE-LOOP.

      ********************************
      *    INTEREST DUE LOOPS
      *    USING SP-ACC TABLE
      ********************************
