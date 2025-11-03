      *******************************************************
      * NO ASSESSMENT IS REQUIRED; ADVANCE LCAP-LCPDTH-DATE
      * TO WHERE PAYMENTS TODATE ARE PAID THRU, ONLY
      * IF IT WILL ADVANCE LCPDTH, SINCE LCPDTH COULD
      * HAVE ADVANCED BEYOND PAID THRU VIA TIME.
      *******************************************************
       LCAP-M-NOASSESS.
           IF LCAP-LPTRCD NOT = "AL"
              ADD LCAP-TRAMT LCAP-PAYMNTD GIVING PDTH-PAYMNTD.

           IF LCAP-LPTRCD NOT = "AL"
              ADD LN-TOTLCHG LCAP-LPAPLC LCAP-APP LCAP-LPAPINT
                                                 GIVING LCAP-WRK
              ADD LCAP-WRK TO PDTH-PAYMNTD
              COMPUTE PDTH-PAYMENTS = LCAP-WRK / LN-REGPYAMT
              IF PDTH-PAYMENTS > 1
                 COMPUTE PDTH-PAYMNTD =
                    PDTH-PAYMNTD - ( PDTH-PAYS * LN-REGPYAMT).

           IF LCAP-LPTRCD NOT = "AL"
              PERFORM LCAP-CALC-PDTH
              MOVE NDTE-DATE TO SYS-DATE
              PERFORM LCAP-LCPDTH-DATE-TEST
              IF ELAPSED-DAYS NOT < 0
                 MOVE PDTH-DATE-FULL TO LCAP-LCPDTH-DATE
                 IF LCAP-UNPAID-PORTION NOT = 0
                          AND LCAP-UNPAID-PORTION < LCAP-DELFAC
                    PERFORM LCAP-UPD-LCPD.

      * INSURE THAT LCPDTH ADVANCES TO CURRENT PAYMENT DATE
      * IF  CURRENT  PAYMENT IS NOT DELIQUENT:
           IF LCAP-CURDATE NOT = 0
              MOVE LCAP-CURDATE TO SYS-DATE
              PERFORM LCAP-LCPDTH-DATE-TEST
              IF ELAPSED-DAYS > 0
                 IF LCAP-CURDUE < LCAP-DELFAC
                    IF ELAPSED-DAYS > 0
                       MOVE LCAP-CURDATE TO LCAP-LCPDTH-DATE
                    ELSE
                       PERFORM LCAP-UPD-LCPD.

