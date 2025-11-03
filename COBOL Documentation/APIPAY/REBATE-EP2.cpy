      * ^^^^^^^^^^
      * THE ABOVE LOGIS IS CONFUSING TO ME, PRIOR TO ADDING "D" THE ELSE
      * JUST SET THE VALUE TO "C" TO USE A LATER DATE DUE TO EXTENSION.
      * FROM THIS POINT FORWARD 'C' AND 'D' ARE THE SAME SO THE ONLY ONE
      * AFFECTED IS 'A' WHICH WILL CONTINUE TO BE FORCED TO A 'C AS IT
      * DID BEFORE.
       REBATE-EP2 SECTION.
           COMPUTE REB-EARN-FOR-1MON ROUNDED =
                         (REB-ORGTERM * LN-INTCHG) /
                             (REB-ORGTERM * (REB-ORGTERM + 1) / 2).
           IF REB-REBATE NOT < 0
              COMPUTE REB-REBATE = LN-INTCHG - REB-EARN-FOR-1MON
              MOVE REB-PAYDATE TO NUM-DATE
              MOVE REB-LN-ORIG-1STPYDATE TO SYS-DATE
              MOVE SP-RBYRTYPE(REB-SUB,REB-SUB2) TO ELAPSED-YRTYPE
              PERFORM TIMALL
              IF ELAPSED-DAYS > 30
                 SUBTRACT 30 FROM ELAPSED-DAYS GIVING
                                                 REB-UNUSED-EXTDAYS
                 PERFORM REBATE-EP2-REBATE-EXTCHG
                 GO TO REBATE-EP2-EXIT
              ELSE
                 PERFORM REBATE-EP2-REBATE-CHG
                 GO TO REBATE-EP2-EXIT.

       REBATE-EP2-REBATE-CHG.
           IF REB-REBATE NOT < 0
              IF ELAPSED-DAYS > 0
                 COMPUTE REB-PRORATA-30DAY-PCT =
                                         (ELAPSED-DAYS / 30) + .0009
                 COMPUTE REB-REBATE ROUNDED = REB-REBATE +
                           REB-EARN-FOR-1MON * REB-PRORATA-30DAY-PCT.

       REBATE-EP2-REBATE-EXTCHG.
           PERFORM REBATE-GET-EXTDAYS.
           IF REB-TOTAL-EXTDAYS > 0
              COMPUTE REB-REBATE ROUNDED = LN-INTCHG +
                       REB-UNUSED-EXTDAYS / REB-TOTAL-EXTDAYS *
                                                           LN-EXTCHG.

       REBATE-EP2-EXIT.
           EXIT.

      *****************************************************
      *    EARN EXTENSION CHARGE PRORATA
      *****************************************************
