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

