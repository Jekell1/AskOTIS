                        * ELAPSED-DAYS ).
      ******************************************************
      *    INDIANA EARLY PROVISION 27
      *    EXTENSION CHARGE IS NEVER REFUNDED
      *    1ST MONTH IS PRORATED, 30 DAYS EARNS THE ENTIRE
      *    MONTHS EARNINGS EVEN IF 31 DAYS IN THE PERIOD
      *                                     PR#1406
      *****************************************************
       REBATE-EP27 SECTION.
      * IF PAYDATE PRIOR TO 30DAY PRIOR DATE REFUND ALL INTEREST
           IF REB-PAYDATE < REB-REFDATE
               MOVE REB-TOTCHG TO REB-REBATE
               GO TO REBATE-EP27-EXIT.

      * SLICE FOR THE FIRST MONTH
           COMPUTE REB-EARN-FOR-1MON ROUNDED =
                    (REB-ORGTERM * LN-INTCHG) /
                        (REB-ORGTERM * (REB-ORGTERM + 1) / 2).

      * THIS WILL GIVE US REBATE IF 30 DAYS IS USED
           IF REB-REBATE NOT < 0
              COMPUTE REB-REBATE = LN-INTCHG - REB-EARN-FOR-1MON.

      * THIS WILL GIVE US ACTUAL USED DAYS USING 365 YEAR TYPE
              MOVE REB-REFDATE TO NUM-DATE.
              MOVE REB-PAYDATE TO SYS-DATE.
              MOVE 365  TO ELAPSED-YRTYPE.
              PERFORM TIMALL.

      * WHAT WE ARE REALLY COMPUTING IS REMAINING DAYS IN THE
      * BEGINNING START POINT OF 30 DAYS.
              COMPUTE ELAPSED-DAYS = 30 - ELAPSED-DAYS.

              IF ELAPSED-DAYS > 30
                  MOVE 30 TO ELAPSED-DAYS.

              PERFORM REBATE-EP27-REBATE-CHG.

              GO TO REBATE-EP27-EXIT.

       REBATE-EP27-REBATE-CHG.
           IF REB-REBATE NOT < 0
              IF ELAPSED-DAYS > 0
                 COMPUTE REB-PRORATA-30DAY-PCT =
                                         (ELAPSED-DAYS / 30) + .0009
                 COMPUTE REB-REBATE ROUNDED = REB-REBATE +
                          REB-EARN-FOR-1MON * REB-PRORATA-30DAY-PCT.
       REBATE-EP27-EXIT.
           EXIT.

      *******************************************************
      * TENNEESSEE EARLY REFUND WITHIN 3 DAYS - AND -
      * EARLY PROVISION FOR THE 5% CLOSING FEE PORTION THAT
      * IS STORED IN LN-POINTS AND ALSO INCLUDED IN LN-SERCHG
      *******************************************************
