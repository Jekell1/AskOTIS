      ****************************************************
       INTEREST-DUE-SPECIAL-OPTIONS SECTION.
      ****************************************************
           IF INDU-LPTRCD = "PB" OR "RN" OR "RB" OR "RO"
              IF SP-RBSPOPT1(7) = 1 OR 19
                 IF ELAPSED-DAYS > 60
                    MOVE 60 TO ELAPSED-DAYS
                 ELSE
                    NEXT SENTENCE
              ELSE
              IF SP-RBSPOPT1(7) = 9
                 IF ELAPSED-DAYS > 90
                    MOVE 90 TO ELAPSED-DAYS.

      * SPRING FINANCE:
           IF SP-RBSPOPT2(7) = 2
              IF INDU-LPTRCD = "PB" OR "RN" OR "SC" OR "PO"
                                  OR "RB" OR "RO"
                 ADD 1 TO ELAPSED-DAYS.

      * CALCULATE MAX ACCRUED INT FOR IB ACCRUAL REPORT
      * NOTE: 999 = NO MAXIMUM
           IF ( INDU-MAX-ACCRUE = 1 AND SP-MAX-IB-ACR-DAYS NOT = 0 )
              IF ( SP-MAX-IB-ACR-DAYS NOT = 999 )
                 IF ( ELAPSED-DAYS > SP-MAX-IB-ACR-DAYS )
                    MOVE SP-MAX-IB-ACR-DAYS TO ELAPSED-DAYS.

      **************************************************
