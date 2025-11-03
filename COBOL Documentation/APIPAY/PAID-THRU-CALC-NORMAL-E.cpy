       PAID-THRU-CALC-NORMAL-E SECTION.
           COMPUTE PDTH-E-1STPYAMT ROUNDED =
                        ((LN-1STPYAMT * SP-BANKRULE-RATE)/100).
           COMPUTE PDTH-E-REGPYAMT ROUNDED =
                        ((LN-REGPYAMT * SP-BANKRULE-RATE)/100).
           COMPUTE PDTH-E-LASTPYAMT ROUNDED =
                        ((LN-LASTPYAMT * SP-BANKRULE-RATE)/100).

           IF PDTH-PAYMNTD < PDTH-E-1STPYAMT
              COMPUTE PDTH-PAYMENTS ROUNDED =
                               PDTH-PAYMNTD / PDTH-E-1STPYAMT
              MOVE PDTH-E-1STPYAMT TO PDTH-CUR-SCHD-PAY
              MOVE PDTH-E-REGPYAMT TO PDTH-NXT-SCHD-PAY
              GO TO PAID-THRU-CALC-NORMAL-E-EXIT.

           SUBTRACT PDTH-E-1STPYAMT FROM PDTH-PAYMNTD.
           MOVE 1 TO PDTH-PAYMENTS.

           IF LN-NOREGPY = 0
              COMPUTE PDTH-PAYMENTS ROUNDED =
                  PDTH-PAYMENTS + PDTH-PAYMNTD / PDTH-E-REGPYAMT
              MOVE PDTH-E-REGPYAMT TO PDTH-CUR-SCHD-PAY
                                  PDTH-NXT-SCHD-PAY
           ELSE
           IF PDTH-PAYMNTD >= (LN-NOREGPY * PDTH-E-REGPYAMT)
              IF GP-PDTH-GT-SCHED-PAY = "Y"
                 COMPUTE PDTH-PAYMENTS = PDTH-PAYMENTS + LN-NOREGPY
                 IF (PDTH-PAYMNTD - (LN-NOREGPY * PDTH-E-REGPYAMT))
                                                 >= PDTH-E-LASTPYAMT
                    ADD 1 TO PDTH-PAYMENTS
                 ELSE
                    COMPUTE PDTH-PAYMENTS ROUNDED = PDTH-PAYMENTS
                        + (PDTH-PAYMNTD
                        - (LN-NOREGPY * PDTH-E-REGPYAMT))
                                   / PDTH-E-LASTPYAMT
                 END-IF
              ELSE
                 COMPUTE PDTH-PAYMENTS ROUNDED =
                     PDTH-PAYMENTS + LN-NOREGPY
                       + (PDTH-PAYMNTD
                       - (LN-NOREGPY * PDTH-E-REGPYAMT))
                                                / PDTH-E-LASTPYAMT
              END-IF
              MOVE PDTH-E-LASTPYAMT TO PDTH-CUR-SCHD-PAY
                                   PDTH-NXT-SCHD-PAY
           ELSE
              COMPUTE PDTH-PAYMENTS ROUNDED =
                 PDTH-PAYMENTS + PDTH-PAYMNTD / PDTH-E-REGPYAMT
              MOVE LN-REGPYAMT TO PDTH-CUR-SCHD-PAY
              IF PDTH-PAYMENTS > LN-NOREGPY
                 MOVE PDTH-E-LASTPYAMT TO PDTH-NXT-SCHD-PAY
              ELSE
                 MOVE PDTH-E-REGPYAMT  TO PDTH-NXT-SCHD-PAY.

       PAID-THRU-CALC-NORMAL-E-EXIT.
           EXIT.
      *---------------------------------------------------------
      *    PAID THRU CALCULATION  (ADDON'S)
      *---------------------------------------------------------
      *
      * PAID THRU IS CAPPED AT MATURITY ON PAYMENT SCHEDULES !!
      *
      *---------------------------------------------------------
