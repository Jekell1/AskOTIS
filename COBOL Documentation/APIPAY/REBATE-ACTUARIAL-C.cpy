      ******************************************
      *    ACTUARIAL REBATE ROUTINE   C
      ******************************************
      * ROUTINE IS EXCLUSIVELY FOR INTEREST:
       REBATE-ACTUARIAL-C SECTION.
           MOVE 0 TO REB-REBATE.

      * GET EQUIVALENT SIMPLE NUMBERS:
           PERFORM APRS-RATE.
           IF APRS-SMPRATE = 0
              GO TO REBATE-ACTUARIAL-C-EXIT.

      * TEST FOR INCREASE OF REMAINING TERM BY DEFERMENTS:
           IF REB-SKIPDEF-FG = " "
              ADD REB-LN-TOTNODEF TO REB-REMTERM.

      * SET FULL REBATE IF NO TIME HAS ELAPSED:
           IF REB-REMTERM > REB-ORGTERM
              MOVE REB-TOTCHG TO REB-REBATE
              GO TO REBATE-ACTUARIAL-C-EXIT.

      * TEST FOR PAST MATURITY:
           IF REB-REMTERM < 0
              GO TO REBATE-ACTUARIAL-C-EXIT.

      * APR PER PMT INTERVAL:
           COMPUTE REB-PRATE = (APRS-SMPRATE / 100) / 12.

      * COMPOUND INTEREST FACTOR:
           COMPUTE REB-FACTOR ROUNDED =
              1 / (1 + REB-PRATE) ** REB-REMTERM.


      * COMPUTE BALANCE OWING EXCLUDING PARTIAL MONTHS:
           COMPUTE REB-BAL ROUNDED =
                  APRS-REGPYAMT * ((1 - REB-FACTOR) / REB-PRATE).

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
                       (APRS-REGPYAMT * REB-REMTERM) - REB-BAL.
       REBATE-ACTUARIAL-C-EXIT.
           EXIT.

      ******************************************
      *    ACTUARIAL REBATE ROUTINE  D
      *               IOWA
      ******************************************
