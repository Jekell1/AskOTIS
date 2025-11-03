      ******************************************
      *    ACTUARIAL REBATE ROUTINE  D
      *               IOWA
      ******************************************
       REBATE-ACTUARIAL-D SECTION.
           MOVE 0 TO REB-REBATE.
           IF REB-LN-APRATE = 0
              GO TO REBATE-ACTUARIAL-D-EXIT.

      * TEST FOR INCREASE OF REMAINING TERM BY DEFERMENTS:
           IF REB-SKIPDEF-FG = " "
              ADD REB-LN-TOTNODEF TO REB-REMTERM.

      * SET FULL REBATE IF NO TIME ELAPSED:
           IF REB-REMTERM > REB-ORGTERM
              MOVE REB-TOTCHG TO REB-REBATE
              GO TO REBATE-ACTUARIAL-D-EXIT.

      * TEST FOR PAST MATURITY:
           IF REB-REMTERM < 0
              GO TO REBATE-ACTUARIAL-D-EXIT.

      * SET ELAPSED-YRTYPE:
           MOVE 360 TO ELAPSED-YRTYPE.
           IF SP-RBYRTYPE(REB-SUB REB-SUB2) = 365
              MOVE 365 TO ELAPSED-YRTYPE.

      * DETERMINE NO OF DAYS BASED ON PAYDATE THRU NEXT PAY:
           IF REB-ELAPSED-REM NOT = 0
              MOVE REB-LN-INTDATE TO NUM-DATE
              MOVE REB-PAYDATE TO NDTE-DATE
              IF NDTE-DD < NUM-DA
                 MOVE NUM-DA TO NDTE-DD
              ELSE
                 MOVE NUM-DA TO NDTE-DD
                 MOVE 1 TO NDTE-HOLD
                 PERFORM INCREMENT-MONTHS.
           IF REB-ELAPSED-REM NOT = 0
              MOVE REB-PAYDATE TO NUM-DATE
              MOVE NDTE-DATE TO SYS-DATE
              PERFORM TIMALL
              COMPUTE REB-ELAPSED-REM =
                       (ELAPSED-YRTYPE / 12) - ELAPSED-DAYS.

      * APR PER PMT INTERVAL:
           COMPUTE REB-PRATE = (REB-LN-APRATE / 100) / 12.

      * COMPOUND INTEREST FACTOR:
           COMPUTE REB-FACTOR ROUNDED =
              1 / ((1 + REB-PRATE) ** REB-REMTERM).

      * COMPUTE BALANCE OWING EXCLUDING PARTIAL MONTHS:
           COMPUTE REB-BAL ROUNDED =
              REB-LN-REGPYAMT * ((1 - REB-FACTOR) / REB-PRATE).

      * INTEREST FOR PARTIAL MONTHS (SP-YEARTYPE OK HERE):
           IF REB-ELAPSED-REM NOT = 0
              COMPUTE REB-BAL ROUNDED = REB-BAL *
                 (1 + REB-ELAPSED-REM * (REB-PRATE * 12)
                                           / ELAPSED-YRTYPE).
      * COMPUTE REBATE AMOUNT:
           COMPUTE REB-REBATE =
                       (REB-LN-REGPYAMT * REB-REMTERM) - REB-BAL.
       REBATE-ACTUARIAL-D-EXIT.
           EXIT.

      ****************************************************************
      *    DEFERMENT REBATE ROUTINE  E   (01)
      *              TENNESSEE
      *    DEFERMENT REBATE ROUTINE  O   (01)
      *              TENNESSEE                   (WITH PAY SCHEDULES)
      *    DEFERMENT REBATE ROUTINE  K   (02)
      *              INDIANA
      *    DEFERMENT REBATE ROUTINE  M   (03)
      *              CALIFORNIA
      *    DEFERMENT REBATE ROUTINE  Y   (04)
      *              S. CAROLINA                 (MERCURY)
      *    DEFERMENT REBATE ROUTINE  Z   (05)
      *              ARIZONA                     (MERCURY)
      *    DEFERMENT REBATE ROUTINE  0 5 (06)
      *              WISCONSIN                   (CITIZENS)
      ****************************************************************
