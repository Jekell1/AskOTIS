      * COPYMEMBER: LIBLP/LPSCHD
      *********************************************************
      *          UNPACK LOAN PAYMENT SCHEDULE
      *
      *   NAME:  LPSCHD
      *
      *   DESC:  EXPANDS (LT) LOAN PAYMENT SCHEDULE INTO
      *          A (SCHD) PER PAYMENT TABLE.
      *
      *   IN  :  LT-PAY-SCHLD-TBL (1 - 17)
      *   OUT :  SCHD-AMT (1 - 360)
      *
      *   USED:  SCHD-SUB, SCHD-SUB2 FOR TABLE INDICES.
      *          SCHD-SUB3 FOR A COUNTER.
      * REV:
      *  JTG 030293 REVISED TO HANDLE LT-PAY-SCHLD-TBL (17)
      *  BAH 160113 SPLIT OUT LTFILE, LTPFILE, PD0003
      *********************************************************
       UNPACK-PAYMENT-SCHEDULE SECTION.
           MOVE LOW-VALUES TO SCHD-PAYMENT-TABLE.
           MOVE 1 TO SCHD-SUB SCHD-SUB2.
           PERFORM UNPACK-LT
              UNTIL SCHD-SUB > 17 OR
                    LTP-SCHLD-TERM(SCHD-SUB) = 0.

       EXIT-UNPACK-PAYMENT-SCHEDULE.
           EXIT.

       UNPACK-LT SECTION.
           MOVE 0 TO SCHD-SUB3.
           PERFORM MOVE-PAYMENT
              UNTIL SCHD-SUB3 = LTP-SCHLD-TERM(SCHD-SUB).
           ADD 1 TO SCHD-SUB.

       EXIT-UNPACK-LT.
           EXIT.

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
       PACK-PAYMENT-SCHEDULE SECTION.
           MOVE LOW-VALUES TO LTP-PAY-SCHLD-AREA.

           MOVE 0 TO SCHD-SUB3 SCHD-TERM.
           MOVE 1 TO SCHD-SUB SCHD-SUB2.
           PERFORM PACK-LT
              UNTIL SCHD-SUB > 360 OR
                    SCHD-AMT(SCHD-SUB) = 0.

       EXIT-PACK-PAYMENT-SCHEDULE.
           EXIT.

       PACK-LT SECTION.
           ADD 1 TO SCHD-SUB.
           PERFORM ADVANCE-SCHED
              UNTIL SCHD-AMT(SCHD-SUB) NOT =
                    SCHD-AMT(SCHD-SUB - 1).
           PERFORM MOVE-TO-LT.

       EXIT-PACK-LT.
           EXIT.

       MOVE-TO-LT SECTION.
           COMPUTE LTP-SCHLD-TERM(SCHD-SUB2) = SCHD-SUB - SCHD-SUB3 - 1.
           ADD LTP-SCHLD-TERM(SCHD-SUB2) TO SCHD-TERM.
           MOVE SCHD-AMT(SCHD-SUB - 1) TO LTP-SCHLD-PAYMNT(SCHD-SUB2).
           COMPUTE SCHD-SUB3  = SCHD-SUB - 1.
           ADD 1 TO SCHD-SUB2.

       EXIT-MOVE-TO-LT.
           EXIT.

       ADVANCE-SCHED SECTION.
           ADD 1 TO SCHD-SUB.

       EXIT-ADVANCE-SCHED.
           EXIT.
