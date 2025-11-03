      ************************************************
      *    GET PAIDTHRU
      ************************************************
       COMPUTE-CONTRACTUAL-GET-PDTH SECTION.
           MOVE LN-TOTPAYMNTD TO AGEING-TOTPAYMNTD-SV.
           IF SP-BANKRULE = "E"
              COMPUTE AGEING-E-1STPYAMT ROUNDED =
                        (LN-1STPYAMT * SP-BANKRULE-RATE)/100
              COMPUTE AGEING-E-REGPYAMT ROUNDED =
                        (LN-REGPYAMT * SP-BANKRULE-RATE)/100
              COMPUTE AGEING-E-LASTPYAMT ROUNDED =
                        (LN-LASTPYAMT * SP-BANKRULE-RATE)/100
              MOVE "E" TO PDTH-E-OPTION
           ELSE
             MOVE LN-1STPYAMT  TO AGEING-E-1STPYAMT
             MOVE LN-REGPYAMT  TO AGEING-E-REGPYAMT
             MOVE LN-LASTPYAMT TO AGEING-E-LASTPYAMT.

           IF SP-CONTR-CREDIT-CD NOT = " "
      * CREDIT IS BASED ON TOTAL PAYS MADE:
              IF SP-CONTR-CREDIT-CD = "A"
                 COMPUTE AGEING-WORK =
                    ( LN-TOTPAYMNTD /
                           (AGEING-E-REGPYAMT - SP-CONTR-CREDIT)
                            - LN-TOTPAYMNTD / AGEING-E-REGPYAMT
                    ) * AGEING-E-REGPYAMT
                 COMPUTE LN-TOTPAYMNTD =
                                LN-TOTPAYMNTD + AGEING-WORK
                                 + SP-CONTR-CREDIT-MIN-DEL-PER
                                 * AGEING-E-REGPYAMT * .01
              ELSE
      * CREDIT IS BASED ON ELAPSED PAY PERIODS:
              IF SP-CONTR-CREDIT-CD = "B"
                 COMPUTE LN-TOTPAYMNTD =
                     LN-TOTPAYMNTD + CEPP-PERIODS * SP-CONTR-CREDIT
                     + SP-CONTR-CREDIT-MIN-DEL-PER
                     * LN-REGPYAMT * .01.

           MOVE LN-1STPYDATE  TO PDTH-DATE-WORK.
           MOVE CSUB-ALDEL-FG TO PDTH-ALDEL-FG.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-REMAIN   TO AGEING-HOLD-REMAIN.

           MOVE AGEING-TOTPAYMNTD-SV TO LN-TOTPAYMNTD.


      *******************************************************
      *    CONTRACTUAL FOR SP-BANKUNITPER-CD = 'A' OR 'D'
      *
      *          A == 50% FACTOR
      *          D == 80% FACTOR
      *
      *  NOTE:
      *       THIS FORMULA DEFAULTS TO SP-BANKRULE C
      *******************************************************
