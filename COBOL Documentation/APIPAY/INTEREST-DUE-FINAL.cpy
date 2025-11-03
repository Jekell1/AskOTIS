      ******************************************
      * AT THIS POINT INDU-WORKER HOLDS THE
      * CALCULATED ANNUAL CHARGE, WHICH MUST BE
      * CONVERTED TO A DAILY CHARGE.
      * THE DAILY CHARGE IS COMPUTED BASED ON
      * THE VALUE OF INDU-INTMETHOD:
      *           "O" - ORDINARY INTEREST (360)
      *           "E" - EXACT    INTEREST (365)
      ******************************************
       INTEREST-DUE-FINAL.
           MOVE 360          TO INDU-WK.
           IF INDU-INTMETHOD = "E"
              MOVE 365       TO INDU-WK.
           MOVE ELAPSED-DAYS TO INDU-DAYS.
           COMPUTE INDU-INTEREST ROUNDED =
              (INDU-WORKER / INDU-WK) * ELAPSED-DAYS.
