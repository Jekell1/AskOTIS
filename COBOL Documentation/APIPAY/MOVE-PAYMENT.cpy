       MOVE-PAYMENT SECTION.
           MOVE LTP-SCHLD-PAYMNT(SCHD-SUB) TO SCHD-AMT(SCHD-SUB2).
           ADD 1 TO SCHD-SUB2 SCHD-SUB3.

       EXIT-MOVE-PAYMENT.
           EXIT.

      *********************************************************
      *          PACK LOAN PAYMENT SCHEDULE
      *
      *   NAME:  LPSCHD
      *
      *   DESC:  REDUCES (SCHD) FROM EXPANDED TO FIT
      *          BACK IN THE LTFILE.
      *
      *   IN  :  SCHD-AMT (1 - 360)
      *   OUT :  LT-PAY-SCHLD-TBL (1 - 17)
      *          SCHD-TERM
      *
      *   USED:  SCHD-SUB, SCHD-SUB2 FOR TABLE INDICES.
      *          SCHD-SUB3 FOR A COUNTER.
      *********************************************************
