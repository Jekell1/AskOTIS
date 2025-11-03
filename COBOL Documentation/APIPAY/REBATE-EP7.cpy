      *****************************************************
      *    EARN EXTENSION CHARGE PRORATA
      *****************************************************
       REBATE-EP7 SECTION.
           MOVE 0 TO REB-EARN-FOR-1MON.
           PERFORM REBATE-GET-EXTDAYS.
           MOVE REB-LN-INTDATE TO NUM-DATE.
           MOVE REB-PAYDATE TO SYS-DATE.
           PERFORM TIM.
           IF REB-TOTAL-EXTDAYS > 0
              COMPUTE REB-EARN-FOR-1MON ROUNDED = LN-EXTCHG -
                 (LN-EXTCHG / REB-TOTAL-EXTDAYS) * ELAPSED-DAYS
              IF REB-EARN-FOR-1MON < 0
                 MOVE 0 TO REB-EARN-FOR-1MON.
           ADD REB-EARN-FOR-1MON TO REB-REBATE.

      **************************************************************************
      *    1/30TH 78THS IF BF 1STPYDATE
      * REV:
      *  JTG 020497 CHANGED TO TEST FOR PAYDATE PRIOR TO
      *             INTEREST START DATE AND SET FULL REFUND
      *  JTG 000317 CHANGED TO CARRY INITIAL CALC OF DAILY FACTOR
      *             TO MORE DECIMALS THAN 2 RE: WORLD BR 588 #01-006564
      *             WRITTEN UP BY AUDITORS
      *  JTG 000322 ALLOWED LOGIC FOR EP10 TO WORK FOR SERVICE CHARGE REFUNDS
      *             ALSO, RE: WORLD BR 588 #01-006564 WRITTEN UP BY AUDITORS
      **************************************************************************
