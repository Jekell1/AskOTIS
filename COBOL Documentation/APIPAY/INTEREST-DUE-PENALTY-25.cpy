      * OHIO STATE PREPAYMENT PENALTY (EFF 01-JAN-2007)
      *
      * IF ORIG PRIN <= 75K
      *    PENALTY IS 1% OF LOAN NET BAL IF PAID < 5 YEARS FROM LOAN DATE
      * IF ORIG PRIN  > 75K
      *    IF PAID <=1 YEAR FROM LOAN DATE
      *       PENALTY IS 2% OF LOAN NET BAL
      *    IF PAID > 1  AND <= 2 YEARS FROM LOAN DATE
      *       PENALTY IS 1% OF LOAN NET BAL
      *    IF PAID > 2 YEARS FROM LOAN DATE
      *       NO PENALTY
      *
      * NOTES:
      *    PENALTY IS NOT CHARGED ON RENEWALS.
      *    PENALTY IS ADDED TO THE INTEREST DUE ON PAYOFF.
       INTEREST-DUE-PENALTY-25.
           IF SP-RBSPOPT1(7) NOT = 25
              GO TO INTEREST-DUE-PENALTY-EXIT.

           IF INDU-LPTRCD = "PO"
              IF INDU-ASSESS-PENALTY ="N"
                 MOVE SPACES TO INDU-ASSESS-PENALTY
                 GO TO NO-PENALTY-25.

           IF INDU-LPTRCD = "PO"
              PERFORM LOAN-CALCULATIONS
              IF FINANCED-AMOUNT <= 75000
                 MOVE LN-LOANDATE TO NUM-DATE
                 MOVE RATE-DATE TO SYS-DATE
                 PERFORM TIM360
                 IF ELAPSED-DAYS NOT > 1800
                    COMPUTE INDU-PENALTY = ( LN-CURBAL * 0.01 )
                    COMPUTE INDU-INTEREST ROUNDED = INDU-INTEREST
                                                  + INDU-PENALTY
                 END-IF
              ELSE
                 MOVE LN-LOANDATE TO NUM-DATE
                 MOVE RATE-DATE TO SYS-DATE
                 PERFORM TIM360
                 IF ELAPSED-DAYS NOT > 360
                    COMPUTE INDU-PENALTY = ( LN-CURBAL * 0.02 )
                    COMPUTE INDU-INTEREST ROUNDED = INDU-INTEREST
                                                  + INDU-PENALTY
                 ELSE IF ELAPSED-DAYS > 360 AND NOT > 720
                    COMPUTE INDU-PENALTY = ( LN-CURBAL * 0.01 )
                    COMPUTE INDU-INTEREST ROUNDED = INDU-INTEREST
                                                  + INDU-PENALTY
                 ELSE
                    MOVE 0 TO INDU-PENALTY.

           MOVE "N" TO INDU-ASSESS-PENALTY.

