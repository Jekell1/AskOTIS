      ***********************************************
      *  SP-LCFRMLA = B OR J:
      *     "B" == "J" WHICH USES LCPAID
      ***********************************************
       LCAP-B-J SECTION.

      * TEST FOR PRIOR PAYMENT SHORTAGES:

           IF LCAP-LCPDTH-DATE NOT = PDTH-DATE-FULL
              IF (LCAP-TRAMT + LCAP-LPAPLC + LCAP-LPAPINT)
                                                > LCAP-AMTNEEDED
                 GO TO LCAP-B-J-PMT
              ELSE
                 GO TO LCAP-B-J-EXIT.

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
       LCAP-B-J-PMT.
           IF LCAP-AMTNEEDED >= LCAP-TRAMT
              SUBTRACT LCAP-TRAMT FROM LCAP-AMTNEEDED
              MOVE 0 TO LCAP-TRAMT
           ELSE
              SUBTRACT LCAP-AMTNEEDED FROM LCAP-TRAMT
              MOVE 0 TO LCAP-AMTNEEDED
              IF LN-PAY-SCHLD-FG = "Y"
                 ADD 1 TO LCAP-PMT-NO.
   
           IF LCAP-AMTNEEDED >= LCAP-LPAPINT
              SUBTRACT LCAP-LPAPINT FROM LCAP-AMTNEEDED
              MOVE 0 TO LCAP-LPAPINT
           ELSE
              SUBTRACT LCAP-AMTNEEDED FROM LCAP-LPAPINT
              MOVE 0 TO LCAP-AMTNEEDED.
   
           IF LCAP-AMTNEEDED >= LCAP-LPAPLC
              SUBTRACT LCAP-LPAPLC FROM LCAP-AMTNEEDED
              MOVE 0 TO LCAP-LPAPLC
           ELSE
              SUBTRACT LCAP-AMTNEEDED FROM LCAP-LPAPLC
              MOVE 0 TO LCAP-AMTNEEDED.
   
           IF LN-PAY-SCHLD-FG = "Y"
              MOVE SCHD-AMT(LCAP-PMT-NO) TO LCAP-AMTNEEDED
                                            LCHG-CURPAY
           ELSE
              MOVE LN-REGPYAMT TO LCAP-AMTNEEDED
                                  LCHG-CURPAY.
           GO TO LCAP-B-J-DATE-CHK.

       LCAP-B-J-RETRY.
           IF (LCAP-TRAMT + LCAP-LPAPLC + LCAP-LPAPINT)
                                                > LCAP-AMTNEEDED
              PERFORM LCAP-UPD-LCPD
              GO TO LCAP-B-J-PMT.

      * TEST TO SEE IF THE SHORTAGE IS NOT GREATER
      * THAN THE DELINQUINCY FACTOR, SETUP IN STATE PARAMETERS.
           COMPUTE LCAP-WRK2 =
                   LCAP-AMTNEEDED - LCAP-TRAMT
                                  - LCAP-LCPAID
                                  - LCAP-LPAPLC
                                  - LCAP-LPAPINT.

           IF LCAP-WRK2 < LCAP-DELFAC
              PERFORM LCAP-UPD-LCPD.

       LCAP-B-J-EXIT.
           EXIT.

      *************************************************
      *   SP-LCFRML = D
      *
      *   NOTE: THIS IS THE SAME AS FORMULA "G",
      *         EXCEPT CONTRACTUAL USES MONIES TOWARDS
      *         PRINCIPAL AND LATE CHARGES.
      *************************************************
