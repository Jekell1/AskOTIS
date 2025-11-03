      * GEORGIA PRE-PAYOFF PENALTY, NEW SOUTH (REAL ESTATE)
      * IF PAYOFF IS PRIOR TO 4TH YEAR TAKE PENALTY:
       INTEREST-DUE-PENALTY-GA.
           IF SP-RBREDUC(7) NOT = 9999.03
              GO TO INTEREST-DUE-PENALTY-WI.

           IF INDU-LPTRCD = "PO"
              IF INDU-ASSESS-PENALTY = "N"
                 MOVE SPACES TO INDU-ASSESS-PENALTY
                 MOVE HOLD-INDU-PENALTY TO INDU-PENALTY
                 MOVE 0 TO HOLD-INDU-PENALTY
                 GO TO NO-PENALTY-GA.

           MOVE 0 TO INDU-PENALTY HOLD-INDU-PENALTY.
           IF INDU-LPTRCD = "PO"
              MOVE LN-LOANDATE TO NUM-DATE
              MOVE INDU-DATE-2 TO SYS-DATE
              PERFORM TIM360
              IF ELAPSED-MONTHS < 12
                 COMPUTE INDU-PENALTY ROUNDED =
                                      (INDU-CURBAL * 0.05)
              ELSE
              IF ELAPSED-MONTHS < 24
                 COMPUTE INDU-PENALTY ROUNDED =
                                      (INDU-CURBAL * 0.04)
              ELSE
              IF ELAPSED-MONTHS < 36
                 COMPUTE INDU-PENALTY ROUNDED =
                                      (INDU-CURBAL * 0.02).

           COMPUTE INDU-INTEREST ROUNDED = INDU-INTEREST
                                         + INDU-PENALTY.

           MOVE "N" TO INDU-ASSESS-PENALTY.
           MOVE INDU-PENALTY TO HOLD-INDU-PENALTY.
