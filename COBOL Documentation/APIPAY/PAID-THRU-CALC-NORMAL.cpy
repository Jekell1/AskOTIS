      *---------------------------------------------------------
      *  EXAMPLE:
      *                       TERM = 1     TERM = 2     TERM = 6
      *                     -----------  -----------  -----------
      *   LN-1STPYAMT    :  $100.00      $100.00      $100.00
      *   LN-REGPYAMT    :  $100.00 X 0  $120.00 X 0  $120.00 X 4
      *   LN-LASTPYAMT   :  $  0.00      $120.00      $130.00
      *                      ------       ------       ------
      *      TOTAL REPAY :  $100.00      $220.00      $710.00
      *---------------------------------------------------------
       PAID-THRU-CALC-NORMAL SECTION.
           IF PDTH-PAYMNTD < LN-1STPYAMT
              COMPUTE PDTH-PAYMENTS ROUNDED =
                               PDTH-PAYMNTD / LN-1STPYAMT
              MOVE LN-1STPYAMT TO PDTH-CUR-SCHD-PAY
              MOVE LN-REGPYAMT TO PDTH-NXT-SCHD-PAY
              GO TO PAID-THRU-CALC-NORMAL-EXIT.

           SUBTRACT LN-1STPYAMT FROM PDTH-PAYMNTD.
           MOVE 1 TO PDTH-PAYMENTS.

           IF LN-NOREGPY = 0
              COMPUTE PDTH-PAYMENTS ROUNDED =
                  PDTH-PAYMENTS + PDTH-PAYMNTD / LN-REGPYAMT
              MOVE LN-REGPYAMT TO PDTH-CUR-SCHD-PAY
                                  PDTH-NXT-SCHD-PAY
           ELSE
           IF PDTH-PAYMNTD >= (LN-NOREGPY * LN-REGPYAMT)
              IF GP-PDTH-GT-SCHED-PAY = "Y"
                 COMPUTE PDTH-PAYMENTS = PDTH-PAYMENTS + LN-NOREGPY
                 IF (PDTH-PAYMNTD - (LN-NOREGPY * LN-REGPYAMT))
                                                 >= LN-LASTPYAMT
                    ADD 1 TO PDTH-PAYMENTS
                 ELSE
                    COMPUTE PDTH-PAYMENTS ROUNDED = PDTH-PAYMENTS
                        + (PDTH-PAYMNTD - (LN-NOREGPY * LN-REGPYAMT))
                                   / LN-LASTPYAMT
                 END-IF
              ELSE
                 COMPUTE PDTH-PAYMENTS ROUNDED =
                     PDTH-PAYMENTS + LN-NOREGPY
                       + (PDTH-PAYMNTD - (LN-NOREGPY * LN-REGPYAMT))
                                                / LN-LASTPYAMT
              END-IF
              MOVE LN-LASTPYAMT TO PDTH-CUR-SCHD-PAY
                                   PDTH-NXT-SCHD-PAY
           ELSE
              COMPUTE PDTH-PAYMENTS ROUNDED =
                 PDTH-PAYMENTS + PDTH-PAYMNTD / LN-REGPYAMT
              MOVE LN-REGPYAMT TO PDTH-CUR-SCHD-PAY
              IF PDTH-PAYMENTS > LN-NOREGPY
                 MOVE LN-LASTPYAMT TO PDTH-NXT-SCHD-PAY
              ELSE
                 MOVE LN-REGPYAMT  TO PDTH-NXT-SCHD-PAY.

       PAID-THRU-CALC-NORMAL-EXIT.
           EXIT.

