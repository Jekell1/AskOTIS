      *---------------------------------------------------------
      *    PAID THRU CALCULATION  (ADDON'S)
      *---------------------------------------------------------
      *
      * PAID THRU IS CAPPED AT MATURITY ON PAYMENT SCHEDULES !!
      *
      *---------------------------------------------------------
       PAID-THRU-CALC-ADDON SECTION.
           PERFORM GET-LT-P-REC.
           IF IO-FG = 9
              STRING LN1-KEY,"-","MISSING PAYMENT SCHEDULE"
                 DELIMITED BY "  " INTO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR
           ELSE
              MOVE LTP-SCHLD-PAYMNT(1) TO PDTH-CUR-SCHD-PAY
                                          PDTH-NXT-SCHD-PAY
              MOVE 1 TO PDTH
              PERFORM PAID-THRU-ADDON-SUM
                 UNTIL PDTH > 17 OR PDTH-PAYMNTD = 0.
           MOVE NDTE-HOLD TO PDTH-PAYS.
           MOVE 0 TO NDTE-HOLD.

