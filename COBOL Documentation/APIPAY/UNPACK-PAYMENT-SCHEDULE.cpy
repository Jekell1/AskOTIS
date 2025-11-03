      *================================================================*
      * END COPYBOOK: LIBLP\LPUPWK.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPSCHD.CPY                           *
      *================================================================*
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

