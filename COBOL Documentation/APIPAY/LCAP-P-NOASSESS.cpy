      *******************************************************
      * NO ASSESSMENT IS REQUIRED; ADVANCE LCAP-LCPDTH-DATE
      * TO WHERE PAYMENTS TODATE ARE PAID THRU, ONLY
      * IF IT WILL ADVANCE LCPDTH, SINCE LCPDTH COULD
      * HAVE ADVANCED BEYOND PAID THRU VIA TIME.
      *******************************************************
       LCAP-P-NOASSESS.
           IF LCAP-LPTRCD NOT = "AL"
              ADD LCAP-TRAMT LCAP-PAYMNTD GIVING PDTH-PAYMNTD
                                                 LCAP-PAYMNTD
              IF SP-LCFRMLA = "H"
                 ADD LCAP-APP TO PDTH-PAYMNTD
                                 LCAP-PAYMNTD.

      * IF LATE CHARGES, INTEREST TOTAL MORE THAN PAYMENT
      * RECOMPUTE PDTH PAYMENTS.
      * ??????? ON WHAT TO DO IF PAYMENT SCHEDULES:
           IF LCAP-LPTRCD NOT = "AL"
              IF SP-LCFRMLA = "P"
                 ADD LN-TOTLCHG LCAP-LPAPLC LCAP-APP LCAP-LPAPINT
                                                 GIVING LCAP-WRK
                 ADD LCAP-WRK TO PDTH-PAYMNTD
                                 LCAP-PAYMNTD
                 COMPUTE PDTH-PAYMENTS = LCAP-WRK / LN-REGPYAMT
                 IF PDTH-PAYMENTS > 1
                    COMPUTE PDTH-PAYMNTD
                            LCAP-PAYMNTD = PDTH-PAYMNTD
                                   - ( PDTH-PAYS * LN-REGPYAMT).

      * CALCULATE PAID THRU, IF SAME AS LATE CHARGE PAID
      * MOVE PDTH TO LATE PAID THRU, IF UNPAID LESS THAN DELFAC
      * UPDATE LATE CHARGE PAID THRU
           IF LCAP-LPTRCD NOT = "AL"
              PERFORM LCAP-PICK-PDTH-CALC
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

      * ? PAST MATURITY,
      * IF SO FORCE LCAP-LCPDTH-DATE TO MATURITY:
