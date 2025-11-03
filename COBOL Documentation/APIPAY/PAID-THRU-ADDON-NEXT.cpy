       PAID-THRU-ADDON-NEXT.
           IF PDTH-CNT < LTP-SCHLD-TERM(PDTH)
              IF PDTH-PAYMNTD > LTP-SCHLD-PAYMNT(PDTH)
                 SUBTRACT LTP-SCHLD-PAYMNT(PDTH) FROM PDTH-PAYMNTD
                 ADD 1 TO NDTE-HOLD PDTH-CNT
                 GO TO PAID-THRU-ADDON-NEXT
              ELSE
                 COMPUTE PDTH-PAYMENTS ROUNDED =
                    PDTH-PAYMNTD / LTP-SCHLD-PAYMNT(PDTH)
                 ADD PDTH-PAYS TO NDTE-HOLD
                 MOVE 0 TO PDTH-PAYMNTD
                 IF PDTH-REMAIN = 0 AND
                    LTP-SCHLD-TERM(PDTH) = (PDTH-CNT + 1)
                    MOVE LTP-SCHLD-PAYMNT(PDTH + 1) TO PDTH-CUR-SCHD-PAY
                                                      PDTH-NXT-SCHD-PAY
                 ELSE
                    IF LTP-SCHLD-TERM(PDTH) = (PDTH-CNT + 1)
                       MOVE LTP-SCHLD-PAYMNT(PDTH + 1) TO
                                                      PDTH-NXT-SCHD-PAY
                    ELSE
                       NEXT SENTENCE
           ELSE
              MOVE 0 TO PDTH-PAYMNTD.

