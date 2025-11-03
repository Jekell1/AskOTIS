      ************************************************
      *               (RBFRMLA2 = 1)
      *    PRO RATA, TO THE DAY REBATE METHOD
      *    MECURY ACTION: FORMULA 9
      ************************************************
       REBATE-MERC-TO-THE-DAY SECTION.
           IF REB-ORGTERM NOT = 0
              MOVE REB-LN-LOANDATE TO NUM-DATE
              MOVE LN-INSEXP-DATE(3) TO SYS-DATE
              PERFORM TIM365
              COMPUTE REB-REBATE ROUNDED =
                  (REB-TOTCHG - REB-DEDCHG)
                        - (REB-TOTCHG - REB-DEDCHG)
                                * REB-ELAPSED-DAYS / ELAPSED-DAYS.

      *********************************
      *    REBATE REF DATE ROUTINE
      *********************************
