       READ-EARNINGS.
      * GET THE LXE-EARN'S FOR THIS REVERSAL
           PERFORM OPEN-LXE1-FILE.
           MOVE LP-BRNO   TO LXE-BRNO.
           MOVE LP-ACCTNO TO LXE-ACCTNO.
           MOVE LP-SEQNO  TO LXE-SEQNO.
           PERFORM READ-LXE1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO LXE-EARN(1) LXE-EARN(2) LXE-EARN(3)
                        LXE-EARN(4) LXE-EARN(5) LXE-EARN(6)
                        LXE-EARN(7) LXE-WORKER LXE-WORKER2.
           IF REV-REBATE NOT = "Y"
              MOVE LXE-REC TO WK-LXE-REC.


      ******************************************************************
