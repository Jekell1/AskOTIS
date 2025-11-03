      **********************************
      *    CALCULATE SERCHG REBATE:
      **********************************
       PAYOFF-SERCHG-REBATE SECTION.

      * ALLOW REFUND OF SERVICE CHARGE ON IB LOAN TYPES
      
            IF ( LN-SERCHG = 0             ) OR
               ( LN-SC-REBATE = "R" OR "Y" )
               GO TO PAYOFF-SERCHG-REBATE-EXIT.

           MOVE 8 TO REB-SUB.
           MOVE LN-SERCHG TO REB-TOTCHG.
           MOVE LN-ORGTERM TO REB-ORGTERM.
           IF LN-INT-REBATE = " " OR "C"
              MOVE POFF-REBATE(4) TO REB-STATEWK
           ELSE
              MOVE LN-ANTICADJ(1) TO REB-STATEWK.
           PERFORM PAYOFF-GET-REBATE.
           MOVE REB-REBATE TO POFF-REBATE(5)
                              REB-LN-SC-REBATE.

           MOVE REB-EP54-AMT TO POFF-LXR-REB-AMT.

       PAYOFF-SERCHG-REBATE-EXIT.
           EXIT.

      * CALIFORNIA "CA" STATE LAW  1/15/2013 MULLEN & COSMO
      * WHEN AN ACCOUNT IS PAID IN FULL (REGARDLESS OF THE PAYOFF METHOD)
      * ON OR BEFORE THE 3RD INSTALLMENT DUE DATE (CURRENT 1ST PAY DUE DATE
      * PLUS 2 MONTHS) BASED ON A 360 DAY YEAR TYPE, AS FOLLOWS:

      ********************************************
