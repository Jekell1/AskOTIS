      * COPYMEMBER: LIBLP/GETSCH
      **************************************************************
      *         GET PAYMENT SCHEDULE (UNPACKED)
      *
      *   NAME: GETSCH
      *
      *   DESC: THIS ROUTINE IS USED BY ADDON CREATE PROGRAMS,
      *         TO EITHER RETRIVE AN EXISTING SCHEDULE
      *         OR TO CONSTRUCT ONE FROM THE LOAN RECORD INFO
      *            LONPG3.C
      *            LONPG5.C
      *            LONPG7.C
      *   IN  :
      *
      * REV :
      *  160113 BAH SPLIT OUT LTFILE, LTPFILE, RENAMED LT- LTP- PD0003
      *  BAH 20200626 REMOVED OPEN-LTP1-FILE AND CLOSE-LTP1-FILE, PUT IN
      *               PROGRAMS, #TST001
      **************************************************************
       GET-LT-SCHLD SECTION.

           MOVE LTP-PATH-OWNBR  TO LTP-BRNO.
           MOVE PD-ACCTNO       TO LTP-ACCTNO.
           MOVE 1               TO LTP-SEQNO.

           PERFORM READ-LTP1-FILE.
           IF IO-FG = 0
              IF LN-PAY-SCHLD-FG = "Y"
                 GO TO GET-LT-SCHLD-CONT
              ELSE
                 MOVE "ERROR: PAYMENT SCHEDULE EXISTS (NO FLAG)" TO MESS
                 PERFORM SEND-MESS
                 GO TO GET-LT-SCHLD-CONT
           ELSE
              IF LN-PAY-SCHLD-FG = "Y"
                 MOVE "ERROR: NO PAYMENT SCHEDULE (FLAG SET)" TO MESS
                 PERFORM SEND-MESS.

           MOVE SPACES    TO LTP-REC.
           MOVE PD-OWNBR  TO LTP-BRNO.
           MOVE PD-ACCTNO TO LTP-ACCTNO.
           MOVE 1         TO LTP-SEQNO.

           MOVE LOW-VALUES TO LTP-PAY-SCHLD-AREA.

      * SET 1STPYAMT:
           MOVE 1           TO LTP-SCHLD-TERM(1).
           MOVE LN-1STPYAMT TO LTP-SCHLD-PAYMNT(1).
           MOVE 2 TO SCHD-SUB.
      * SET REGPYAMT:
           IF LN-NOREGPY NOT = 0
              MOVE LN-NOREGPY  TO LTP-SCHLD-TERM(SCHD-SUB)
              MOVE LN-REGPYAMT TO LTP-SCHLD-PAYMNT(SCHD-SUB)
              ADD 1 TO SCHD-SUB.
      * SET LASTPYAMT:
           IF LN-LASTPYAMT NOT = 0
              MOVE 1            TO LTP-SCHLD-TERM(SCHD-SUB)
              MOVE LN-LASTPYAMT TO LTP-SCHLD-PAYMNT(SCHD-SUB).

       GET-LT-SCHLD-CONT.
           PERFORM UNPACK-PAYMENT-SCHEDULE.

           PERFORM GET-1ST-PAYMNT-INDEX.
           IF SCHD-SUB < 1 OR SCHD-SUB > 360
              MOVE "ERROR UPDATING PAYMENT SCHEDULE" TO MESS
              PERFORM SEND-MESS
              GO TO GET-LT-SCHLD-EXIT.

       GET-LT-SCHLD-EXIT.
           EXIT.

