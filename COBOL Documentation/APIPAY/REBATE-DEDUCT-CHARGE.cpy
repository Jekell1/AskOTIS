       REBATE-DEDUCT-CHARGE SECTION.
           IF REB-ADDON-FG = "Y" AND REB-SUB = 7
              MOVE 0 TO REB-DEDCHG
              GO TO REBATE-DEDUCT-CHARGE-EXIT.

           MOVE SP-RBDEDCHG(REB-SUB) TO REB-DEDCHG.

           IF REB-DEDCHG = 9999.01
              COMPUTE REB-DEDCHG ROUNDED =
                            (LN-CURBAL - LN-OT2BAL) * 0.1
              IF REB-DEDCHG > 25.00
                 MOVE 25.00 TO REB-DEDCHG
              ELSE
                 MOVE 0 TO REB-DEDCHG
           ELSE
              IF REB-DEDCHG = 9999.02
                 COMPUTE REB-HALF-ORGTERM = REB-ORGTERM / 2
                 IF REB-REMTERM NOT < REB-HALF-ORGTERM
                    MOVE 25.00 TO REB-DEDCHG
                 ELSE
                    MOVE 0 TO REB-DEDCHG
              ELSE
                 IF REB-DEDCHG = 9999.03
                    MOVE 25.00 TO REB-DEDCHG
                 ELSE
      * 9999.04  IF MORE THAN HALF THE TERM HAS ELAPSED, THE REBATE SHALL
      *          BE COMPUTED WITHOUT DEDUCTING THE PRECHARGE. (WORLD)
      * JUST LIKE 9999.02 EXCEPT NEEDS A DECIMAL ON HALF-ORGTERM
      * 2/17/99 ADDED DECIMAL TO REB-HALF-ORGTERM-DEC FOR THE FOLLOWING EXAMPLE
      * TERM = 9     REMAINING = 4  ELAPSED = 5  HALF = 4.5
      * WITHOUT THE DECIMAL, THIS EXAMPLE TOOK THE 25.00 AND SHOULDN'T
      * TERM = 8     REMAINING = 4  ELAPSED = 4  HALF = 4
      * THIS EXAMPLE TAKES THE 25.00 LIKE IT SHOULD
                   IF REB-DEDCHG = 9999.04
                      COMPUTE REB-HALF-ORGTERM-DEC = REB-ORGTERM / 2
                      IF REB-REMTERM NOT < REB-HALF-ORGTERM-DEC
                         MOVE 25.00 TO REB-DEDCHG
                      ELSE
                         MOVE 0 TO REB-DEDCHG
                   ELSE
                      IF REB-DEDCHG = 9999.05
                         MOVE 30.00 TO REB-DEDCHG.


       REBATE-DEDUCT-CHARGE-EXIT.
           EXIT.

      ******************************************************************
      * EARLY PROVISION FOR ACQUISITION CHARGES - OKLAHOMA    (REQ #163)
      *
      *   1/60TH FOR EACH DAY FROM DATE OF PREPAYMENT TO THE SIXTIETH
      *   DAY OF LOAN (PRO RATA)
      ******************************************************************
