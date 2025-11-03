      *************************************************
       LCAP-PAID-THRU-ADDON-SUM SECTION.
      *************************************************
           IF LTP-SCHLD-TERM(LCAP) = 0
              MOVE 17 TO LCAP
              GO TO LCAP-PAID-THRU-ADDON-SUM-EXIT.

           COMPUTE LCAP-WORKER =
                     LTP-SCHLD-PAYMNT(LCAP) * LTP-SCHLD-TERM(LCAP).
           MOVE LTP-SCHLD-PAYMNT(LCAP) TO LCAP-CUR-SCHD-PAY.
           IF LCAP-PAYMNTD > LCAP-WORKER
              ADD LTP-SCHLD-TERM(LCAP) TO NDTE-HOLD
              SUBTRACT LCAP-WORKER FROM LCAP-PAYMNTD
              GO TO LCAP-PAID-THRU-ADDON-SUM-EXIT.

           MOVE 0 TO LCAP-CNT.
       LCAP-PAID-THRU-ADDON-NEXT.
           IF LCAP-CNT < LTP-SCHLD-TERM(LCAP)
              IF LCAP-PAYMNTD > LTP-SCHLD-PAYMNT(LCAP)
                 SUBTRACT LTP-SCHLD-PAYMNT(LCAP) FROM LCAP-PAYMNTD
                 ADD 1 TO NDTE-HOLD
                          LCAP-CNT
                 GO TO LCAP-PAID-THRU-ADDON-NEXT
              ELSE
                 COMPUTE LCAP-PAYMENTS ROUNDED =
                            LCAP-PAYMNTD / LTP-SCHLD-PAYMNT(LCAP)
                 IF LCAP-PAYMNTD = LTP-SCHLD-PAYMNT(LCAP)
                            AND LTP-SCHLD-TERM(LCAP) = (LCAP-CNT + 1)
                    MOVE LTP-SCHLD-PAYMNT(LCAP + 1) TO LCAP-CUR-SCHD-PAY
                    ADD 1 TO NDTE-HOLD
                             LCAP-CNT
                    MOVE 0 TO LCAP-PAYMNTD
                 ELSE
                 IF LCAP-PAYMNTD = LTP-SCHLD-PAYMNT(LCAP)
                    ADD 1 TO NDTE-HOLD
                             LCAP-CNT
                 END-IF
                    MOVE 0 TO LCAP-PAYMNTD
           ELSE
              MOVE 0 TO LCAP-PAYMNTD.

       LCAP-PAID-THRU-ADDON-SUM-EXIT.
           ADD 1 TO LCAP.

      *********************************************************
      * ADVANCE LCAP-LCPDTH-DATE BY ONE PAYMENT FREQUENCY:
      *********************************************************
