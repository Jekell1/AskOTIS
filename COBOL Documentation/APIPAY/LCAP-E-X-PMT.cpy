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

