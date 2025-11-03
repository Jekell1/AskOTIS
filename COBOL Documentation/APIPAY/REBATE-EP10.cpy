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
       REBATE-EP10 SECTION.
           MOVE REB-LN-INTDATE TO NUM-DATE.
           MOVE REB-PAYDATE TO SYS-DATE.
           PERFORM TIM365.
           IF REB-SUB = 7
              IF ELAPSED-DAYS < 0
                 IF REB-ADDON-FG = "Y"
                    MOVE LTI-ANTICERN(1)        TO REB-REBATE
                 ELSE
                    ADD LN-INTCHG LN-EXTCHG GIVING REB-REBATE
              ELSE
                 IF REB-ADDON-FG = "Y"
                    COMPUTE REB-WK ROUNDED =
                    ( LTI-ANTICERN(1)
                        - (LTI-ANTICERN(1)
                             * (REB-ORGTERM - 1) * (REB-ORGTERM)
                                / (REB-ORGTERM * (REB-ORGTERM + 1))
                          )
                    ) / 30
                 COMPUTE REB-REBATE ROUNDED =
                     LTI-ANTICERN(1)  - ELAPSED-DAYS * REB-WK
              ELSE
                 COMPUTE REB-WK ROUNDED =
                    ( LN-INTCHG
                        - (LN-INTCHG
                                * (REB-ORGTERM - 1) * (REB-ORGTERM)
                                   / (REB-ORGTERM * (REB-ORGTERM + 1))
                          )
                    ) / 30
                 COMPUTE REB-REBATE ROUNDED =
                     LN-INTCHG + LN-EXTCHG - ELAPSED-DAYS * REB-WK
              END-IF
             END-IF
           ELSE
           IF REB-SUB = 8
              IF ELAPSED-DAYS < 0
                 MOVE LN-SERCHG TO REB-REBATE
              ELSE
                 COMPUTE REB-WK ROUNDED =
                    ( LN-SERCHG
                        - (LN-SERCHG
                                * (REB-ORGTERM - 1) * (REB-ORGTERM)
                                   / (REB-ORGTERM * (REB-ORGTERM + 1))
                          )
                    ) / 30
                 COMPUTE REB-REBATE ROUNDED =
                   LN-SERCHG - ELAPSED-DAYS * REB-WK.

      *****************************************************
      * MISSOURI
      * 23-  IF BEFORE 1STPYDATE, REFUND THE DIFF BETWEEN THE
      *       ORIG INTCHG AND INT PRECOMPUTED ON THE ACTUAL
      *       UNPAID PRINCIPAL BALANCE 1/360TH OF THE EFFRATE
      *       FOR EACH DAY FROM LOANDATE TO POFFDATE
      * ILLINOIS
      * 28-  IF BEFORE 1STPYDATE, REFUND THE DIFF BETWEEN THE
      *       (ORIG INTCHG + ORIG EXTCHG) AND INT PRECOMPUTED
      *       ON THE ACTUAL UNPAID PRINCIPAL BALANCE 1/360TH
      *       OF THE EFFRATE FOR EACH DAY FROM LOANDATE TO POFFDATE
      *****************************************************
