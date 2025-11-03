       LCAP-B-J-DATE-CHK.

      * VERIFY DATES FOR ASSESSMENT
           PERFORM LCAP-MDTE-CHK.
           IF ELAPSED-DAYS NOT > 0
              GO TO LCAP-B-J-EXIT.

           PERFORM LCAP-ASSESS-CHK.
           IF LCAS-ASSESS NOT = "Y"
              IF LCAP-LPTRCD = "AL"
                 GO TO LCAP-B-J-EXIT
              ELSE
                 GO TO LCAP-B-J-RETRY.

      * LCHG-LATE INITIALIZED FOR SP-LCTYPE = "C" OR "J",
      * WHERE LATE CHG IS BASED ON % OF LATE PMT:
           MOVE LCAP-AMTNEEDED TO LCHG-LATE.
           MOVE LCAP-LCPDTH-DATE    TO LCHG-LCPDTH-DATE.
           PERFORM LCAP-CALC-TEST.

      * IF TRAMT INSUFFICIENT TO COVER CHARGE, OWING IS FOUND:
           IF LN-ESCROW-FG = "Y"
              IF (LCAP-TRAMT - LCAP-ESCROW-AMT) < LCHG-CHARGE
                 COMPUTE LCAP-OWE = LCHG-CHARGE
                                  - (LCAP-TRAMT - LCAP-ESCROW-AMT)
                 COMPUTE LCAP-APP = LCAP-APP
                                  + (LCAP-TRAMT - LCAP-ESCROW-AMT)
                 PERFORM LCAP-UPD-LCPD
                 GO TO LCAP-B-J-EXIT
              END-IF
           ELSE
              IF LCAP-TRAMT < LCHG-CHARGE
                 SUBTRACT LCAP-TRAMT FROM LCHG-CHARGE GIVING LCAP-OWE
                 ADD LCAP-TRAMT TO LCAP-APP
                 PERFORM LCAP-UPD-LCPD
                 GO TO LCAP-B-J-EXIT.

      * OTHERWISE, TAKE LATE CHARGE AND (ATTEMPT) PMT RECEIPT:
           ADD LCHG-CHARGE TO LCAP-APP
                              LCAP-LCPAID.
           SUBTRACT LCHG-CHARGE FROM LCAP-TRAMT.
           PERFORM LCAP-UPD-LCPD.
           IF (LCAP-TRAMT + LCAP-LPAPLC + LCAP-LPAPINT)
                                           NOT > LCAP-AMTNEEDED
              GO TO LCAP-B-J-EXIT.

      * REDUCE TRANSACTION AMOUNT BY PAYMENT:
