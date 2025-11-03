      ***********************************************
      *  SP-LCFRMLA = E AND X
      ***********************************************
       LCAP-E-X SECTION.

           ADD LCAP-LPAPINT LCAP-LPAPLC LCAP-TRAMT
                                    GIVING LCAP-TRAMT-PLUS-AP.
           MOVE 0 TO LCAP-APPLIED-TOWARD-SHORTAGE.

      * TEST FOR PRIOR PAYMENT SHORTAGES:
           IF LCAP-LCPDTH-DATE NOT = PDTH-DATE-FULL
              IF LCAP-TRAMT-PLUS-AP > LCAP-AMTNEEDED
                 GO TO LCAP-E-X-PMT
              ELSE
                 GO TO LCAP-E-X-EXIT.

       LCAP-E-X-DATE-CHK.

      * VERIFY DATES FOR ASSESSMENT
           PERFORM LCAP-MDTE-CHK.
           IF ELAPSED-DAYS NOT > 0
              GO TO LCAP-E-X-EXIT.

           PERFORM LCAP-ASSESS-CHK.
           IF LCAS-ASSESS NOT = "Y"
              IF LCAP-LPTRCD = "AL"
                 GO TO LCAP-E-X-EXIT
              ELSE
                 GO TO LCAP-E-X-RETRY.

      * LCHG-LATE INITIALIZED FOR SP-LCTYPE = "C" OR "J",
      * WHERE LATE CHG IS BASED ON % OF LATE PMT:
           MOVE LCAP-AMTNEEDED TO LCHG-LATE.
           MOVE LCAP-LCPDTH-DATE    TO LCHG-LCPDTH-DATE.
           PERFORM LCAP-CALC-TEST.

      * IF TRAMT INSUFFICIENT TO COVER CHARGE,
      * IF 'E' PARTIAL OWING IS FOUND
      * IF 'X' FULL LATE CHARGE IS PUT INTO OWING UNLESS
      *        ENOUGH MONIE WAS PAID TOWARDS THE PRIOR PAYMENT
      *        SHORTAGE TO COVER CHARGE
      
           IF LCAP-TRAMT < LCHG-CHARGE
              IF SP-LCFRMLA = "E"
                 SUBTRACT LCAP-TRAMT FROM LCHG-CHARGE GIVING LCAP-OWE
                 ADD LCAP-TRAMT TO LCAP-APP
              ELSE
                 IF LCHG-CHARGE <= LCAP-APPLIED-TOWARD-SHORTAGE
                    ADD  LCHG-CHARGE TO LCAP-APP
                 ELSE
                    MOVE LCHG-CHARGE TO LCAP-OWE
                 END-IF
              END-IF
              PERFORM LCAP-UPD-LCPD
              GO TO LCAP-E-X-EXIT.

      * OTHERWISE, TAKE LATE CHARGE AND (ATTEMPT) PMT RECEIPT:
           ADD LCHG-CHARGE TO LCAP-APP.
           SUBTRACT LCHG-CHARGE FROM LCAP-TRAMT
                                     LCAP-TRAMT-PLUS-AP
                                     LCAP-AMTNEEDED.
           PERFORM LCAP-UPD-LCPD.
           IF LCAP-TRAMT-PLUS-AP <= LCAP-AMTNEEDED
              GO TO LCAP-E-X-EXIT.

      * REDUCE TRANSACTION AMOUNT BY PAYMENT:
       LCAP-E-X-PMT.
           SUBTRACT LCAP-AMTNEEDED FROM LCAP-TRAMT-PLUS-AP.
           IF LCAP-AMTNEEDED <= LCAP-TRAMT
              SUBTRACT LCAP-AMTNEEDED FROM LCAP-TRAMT
              ADD LCAP-AMTNEEDED TO LCAP-APPLIED-TOWARD-SHORTAGE
              IF LN-PAY-SCHLD-FG = "Y"
                 ADD 1 TO LCAP-PMT-NO.

           IF LN-PAY-SCHLD-FG = "Y"
              MOVE SCHD-AMT(LCAP-PMT-NO) TO LCAP-AMTNEEDED
                                            LCHG-CURPAY
           ELSE
              MOVE LN-REGPYAMT TO LCAP-AMTNEEDED
                                  LCHG-CURPAY.
           GO TO LCAP-E-X-DATE-CHK.

       LCAP-E-X-RETRY.
           IF LCAP-TRAMT-PLUS-AP > LCAP-AMTNEEDED
              PERFORM LCAP-UPD-LCPD
              GO TO LCAP-E-X-PMT.

      * TEST TO SEE IF THE SHORTAGE IS NOT GREATER
      * THAN THE DELINQUINCY FACTOR, SETUP IN STATE PARAMETERS.
           COMPUTE LCAP-WRK2 =
                  LCAP-AMTNEEDED - LCAP-TRAMT-PLUS-AP.

           IF LCAP-WRK2 < LCAP-DELFAC
              PERFORM LCAP-UPD-LCPD.

       LCAP-E-X-EXIT.
           EXIT.

      ***************************************************************
      *   SP-LCFRML = F    LOUISIANA LATE CHARGE FORMULA
      *                    LIKE 'H' EXCEPT PRINCIPAL ONLY
      *
      *   NOTE:
      *        LCAP-PARTIALS (HOLDS LCAP-CURDUE AT EXIT)
      *        LCAP-LCPAID   (HOLDS LCAP-CURDATE [MMDD.YY] AT EXIT)
      ***************************************************************
