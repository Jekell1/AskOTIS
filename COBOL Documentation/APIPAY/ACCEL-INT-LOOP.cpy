      ********************************
      *    INTEREST DUE LOOPS
      *    USING SP-ACC TABLE
      ********************************
       ACCEL-INT-LOOP.
           IF INDU-CURBAL = 0
              GO TO INTEREST-DUE-FINAL.
           SET ASTP UP BY 1.
           IF ASTP > 5
              GO TO INTEREST-DUE-FINAL.
           IF SP-ACC-RATE(ASTP) = 0 AND SP-ACC-BREAK(ASTP) = 0
              GO TO INTEREST-DUE-FINAL.
           IF SP-ACC-BREAK(ASTP) = 0
              MOVE INDU-CURBAL TO INDU-AMOUNT
           ELSE
              IF ASTP = 1
                 MOVE SP-ACC-BREAK(ASTP) TO INDU-AMOUNT
              ELSE
                 SUBTRACT SP-ACC-BREAK(ASTP - 1) FROM
                           SP-ACC-BREAK(ASTP) GIVING INDU-AMOUNT.
           IF INDU-CURBAL > INDU-AMOUNT
              SUBTRACT INDU-AMOUNT FROM INDU-CURBAL
           ELSE
              MOVE INDU-CURBAL TO INDU-AMOUNT
              MOVE 0 TO INDU-CURBAL.
           COMPUTE INDU-WORKER ROUNDED =
             INDU-WORKER + (SP-ACC-RATE(ASTP) / 100) * INDU-AMOUNT.
           GO TO ACCEL-INT-LOOP.

      ******************************************
      * AT THIS POINT INDU-WORKER HOLDS THE
      * CALCULATED ANNUAL CHARGE, WHICH MUST BE
      * CONVERTED TO A DAILY CHARGE.
      * THE DAILY CHARGE IS COMPUTED BASED ON
      * THE VALUE OF INDU-INTMETHOD:
      *           "O" - ORDINARY INTEREST (360)
      *           "E" - EXACT    INTEREST (365)
      ******************************************
