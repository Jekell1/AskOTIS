       REBTX-CALC-INT-0H SECTION.
           MOVE ZEROS TO REBTX-STEP.
           MOVE ZEROS TO REBTX-CALC-INT.
           MOVE ZEROS TO REBTX-RATE-AMT.
           MOVE REBTX-AMTFIN TO REBTX-RATE-BAL.
       REBTX-CALC-INT-0H-LOOP.
           IF REBTX-RATE-BAL = ZEROS
              GO TO REBTX-CALC-INT-0H-END.

           ADD 1 TO REBTX-STEP.

           IF REBTX-STEP > 5
              GO TO REBTX-CALC-INT-0H-END.


           IF SP-CAL-BREAK(1,REBTX-STEP) = ZEROS
              MOVE REBTX-RATE-BAL TO REBTX-RATE-AMT
           ELSE
           IF REBTX-STEP = 1
              MOVE SP-CAL-BREAK(1,REBTX-STEP) TO REBTX-RATE-AMT
           ELSE
              SUBTRACT SP-CAL-BREAK(1,REBTX-STEP - 1) FROM
                        SP-CAL-BREAK(1,REBTX-STEP) GIVING
                                              REBTX-RATE-AMT.
           IF REBTX-RATE-BAL > REBTX-RATE-AMT
              SUBTRACT REBTX-RATE-AMT FROM REBTX-RATE-BAL
           ELSE
              MOVE REBTX-RATE-BAL TO REBTX-RATE-AMT
              MOVE ZERO TO REBTX-RATE-BAL.

           COMPUTE REBTX-CALC-INT = REBTX-CALC-INT +
              REBTX-RATE-AMT *
                 (SP-CAL-RATE(1,REBTX-STEP) * .01).


      *    COMPUTE REBTX-NEW-INT ROUNDED =
      *          REBTX-RATE-AMT *
      *          (SP-CAL-RATE(1,REBTX-STEP) * .01).
      *    COMPUTE REBTX-CALC-INT = REBTX-CALC-INT + REBTX-NEW-INT.


      *    MOVE REBTX-CALC-INT TO REBTX-CALC-INT-X.
      *    MOVE REBTX-CALC-INT-X TO MESS.
      *    PERFORM SEND-MESS.

           GO TO REBTX-CALC-INT-0H-LOOP.

       REBTX-CALC-INT-0H-END.
           COMPUTE REBTX-CALC-INT =
                   REBTX-CALC-INT / 365 * ELAPSED-DAYS.

       REBTX-CALC-INT-0H-EXIT.
           EXIT.

      ****************************************************************
      *                     (0 I)
      *    CITIZENS FINANCE COMPANY
      *    FORMULA FOR TRIST CPI SINGLE PAYMENT LOANS
      *
      ****************************************************************
