      *************************************************
      *   SP-LCFRML = D
      *
      *   NOTE: THIS IS THE SAME AS FORMULA "G",
      *         EXCEPT CONTRACTUAL USES MONIES TOWARDS
      *         PRINCIPAL AND LATE CHARGES.
      *************************************************
       LCAP-D SECTION.
           ADD LCAP-LPAPLC LCAP-LPAPINT LCAP-TOTPAYMNTD
                           GIVING LCAP-PAYMNTD.

       LCAP-D-BEGIN.

      * PAST MATURITY ?
           PERFORM LCAP-MDTE-CHK.
           IF ELAPSED-DAYS NOT > 0
              GO TO LCAP-D-EXIT.

      * ? ASSESSING LATE CHARGES (AL)
      * STOP ASSESSING WHEN LATE CHARGES ARE BROUGHT UP TODATE,
      * LCAP-LCPDTH-DATE >= PAYMENT DATE:
           IF LCAP-LPTRCD = "AL"
              MOVE LCAP-PAYDATE TO SYS-DATE
              PERFORM LCAP-LCPDTH-DATE-TEST
              IF ELAPSED-DAYS NOT > 0
                 GO TO LCAP-D-EXIT.

      * DOES PAYMENT AND LCPDTH INDICATE AN ASSESSMENT ?
           PERFORM LCAP-ASSESS-CHK.
           IF LCAS-ASSESS NOT = "Y"
              GO TO LCAP-D-NOASSESS.

      * TEST FOR SHORTAGE NOT GREATER
      * THAN THE DELINQUINCY FACTOR IN (SPR'S):
           IF LCAP-LCPDTH-DATE = PDTH-DATE-FULL
              IF LCAP-AMTNEEDED < LCAP-DELFAC
                 GO TO LCAP-D-ADVANCE.

      * LCHG-LATE INITIALIZED FOR SP-LCTYPE = "C" OR "J"
      * SINCE LATE CHARGE IS BASED ON % OF DELINQUENT AMOUNT.
           MOVE LCAP-AMTNEEDED TO LCHG-LATE.
           MOVE LCAP-LCPDTH-DATE TO LCHG-LCPDTH-DATE.
           PERFORM LCAP-CALC-TEST.

      * IF LCAP-TRAMT IS NOT ENOUGH TO COVER CHARGE, USE OWING:
           IF LCAP-TRAMT < LCHG-CHARGE
              COMPUTE LCAP-OWE = LCAP-OWE + LCHG-CHARGE
                                               - LCAP-TRAMT
              ADD LCAP-TRAMT TO LCAP-APP
                                LCAP-PAYMNTD
              MOVE 0 TO LCAP-TRAMT
           ELSE
              ADD LCHG-CHARGE TO LCAP-APP
                                 LCAP-PAYMNTD
              SUBTRACT LCHG-CHARGE FROM LCAP-TRAMT.

       LCAP-D-ADVANCE.
           PERFORM LCAP-UPD-LCPD.
           IF LN-PAY-SCHLD-FG = "Y"
              ADD 1 TO LCAP-PMT-NO
              MOVE SCHD-AMT(LCAP-PMT-NO) TO LCAP-AMTNEEDED
                                            LCHG-CURPAY
              COMPUTE LCAP-DELFAC ROUNDED =
                        SCHD-AMT(LCAP-PMT-NO) * (SP-LCDELFAC / 100)
           ELSE
              MOVE LN-REGPYAMT TO LCAP-AMTNEEDED
                                  LCHG-CURPAY.
           GO TO LCAP-D-BEGIN.

      * NO ASSESSMENT REQUIRED, ADVANCE LCAP-LCPDTH-DATE
      * TO WHERE PAYMENTS TODATE ARE PAID THRU
      * IF IT WILL ADVANCE LCPDTH ONLY, SINCE
      * ADVANCEMENT OF LCPDTH COULD HAVED OCCURED
      * VIA TIME.
       LCAP-D-NOASSESS.
           IF LCAP-LPTRCD NOT = "AL"
              ADD LCAP-TRAMT LCAP-PAYMNTD GIVING PDTH-PAYMNTD
                                                 LCAP-PAYMNTD
              PERFORM LCAP-PICK-PDTH-CALC
              MOVE NDTE-DATE TO SYS-DATE
              PERFORM LCAP-LCPDTH-DATE-TEST
              IF ELAPSED-DAYS > 0
                 MOVE PDTH-DATE-FULL TO LCAP-LCPDTH-DATE.

      * ? PAST MATURITY
      * IF SO FORCE LCAP-LCPDTH-DATE TO MATURITY:
           PERFORM LCAP-MDTE-CHK.
           IF ELAPSED-DAYS NOT > 0
              MOVE MDTE-DATE TO LCAP-LCPDTH-DATE.

       LCAP-D-EXIT.
           EXIT.

      ***********************************************
      *  SP-LCFRMLA = E AND X
      ***********************************************
