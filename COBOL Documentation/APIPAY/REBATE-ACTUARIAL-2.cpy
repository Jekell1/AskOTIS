      ******************************************
      *    ACTUARIAL REBATE ROUTINE  2
      ******************************************
       REBATE-ACTUARIAL-2 SECTION.
           MOVE 0 TO REB-REBATE.
           IF REB-LN-APRATE = 0
              GO TO REBATE-ACTUARIAL-2-EXIT.

      * TEST FOR INCREASE OF REMAINING TERM BY DEFERMENTS:
           IF REB-SKIPDEF-FG = " "
              ADD REB-LN-TOTNODEF TO REB-REMTERM.

      * SET FULL REBATE IF NO TIME ELAPSED:
           IF REB-REMTERM > REB-ORGTERM
              MOVE REB-TOTCHG TO REB-REBATE
              GO TO REBATE-ACTUARIAL-2-EXIT.

      * TEST FOR PAST MATURITY:
           IF REB-REMTERM < 0
              GO TO REBATE-ACTUARIAL-2-EXIT.

      * APR PER PMT INTERVAL:
           COMPUTE REB-PRATE =
              (REB-LN-APRATE / 100) / 12.

      * COMPOUND INTEREST FACTOR:
           COMPUTE REB-FACTOR ROUNDED =
              1 / ((1 + REB-PRATE) ** REB-REMTERM).

      * COMPUTE BALANCE OWING EXCLUDING PARTIAL MONTHS:
           COMPUTE REB-BAL ROUNDED =
              REB-LN-REGPYAMT * ((1 - REB-FACTOR) / REB-PRATE).

      * INTEREST FOR PARTIAL MONTHS (SP-YEARTYPE OK HERE):
           MOVE 360 TO ELAPSED-YRTYPE.
           IF SP-RBYRTYPE(REB-SUB REB-SUB2) = 365
              MOVE 365 TO ELAPSED-YRTYPE.
           IF REB-ELAPSED-REM NOT = 0
              COMPUTE REB-BAL ROUNDED = REB-BAL *
                 (1 + REB-ELAPSED-REM * (REB-PRATE * 12)
                                           / ELAPSED-YRTYPE).
      * COMPUTE REBATE AMOUNT:
           COMPUTE REB-REBATE =
                       (REB-LN-REGPYAMT * REB-REMTERM) - REB-BAL.

           IF SP-RBSPOPT1(REB-SUB) = 20
      * (A) 10 % OF AMT FINANCED. IF > $75, THEN $75, IF < $75,
      *     THEN 10% AMT FINANCED.
              PERFORM LOAN-CALCULATIONS
              COMPUTE REB-WORKER ROUNDED = FINANCED-AMOUNT *
                                            .10
              IF REB-WORKER > 75.00
                 MOVE 75.00 TO REB-WORKER
              END-IF
      * (B) COMPUTE INTEREST EARNED
      * (C) TAKE THE LARGER FIGURE
              IF REB-WORKER > (REB-TOTCHG - REB-REBATE)
                 COMPUTE REB-REBATE = REB-TOTCHG - REB-WORKER.


       REBATE-ACTUARIAL-2-EXIT.
           EXIT.

      *********************************
      *   ANTICIPATED 78 REBATE  (4)
      *********************************
