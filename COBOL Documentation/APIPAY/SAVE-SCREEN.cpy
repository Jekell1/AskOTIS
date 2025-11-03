      ******************************************************************
       SAVE-SCREEN SECTION.
      ******************************************************************
           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.

           MOVE SPACES TO WK-SAVE-RECORD.
           MOVE 0      TO WK-ELE.

           MOVE LN-REC  TO WK-SAVE-RECORD.
           MOVE LP-REC  TO WK-LP-REC.
           MOVE LX-REC  TO WK-LX-REC.
           MOVE LXG-REC TO WK-LXG-REC.

           MOVE 0       TO LXE-EARN(1)
                           LXE-EARN(2)
                           LXE-EARN(3)
                           LXE-EARN(4)
                           LXE-EARN(5)
                           LXE-EARN(6)
                           LXE-EARN(7).
           IF SP-ERNFRMLA(1) = 15 OR SP-ERNFRMLA(5) = 16
              MOVE LN-ERN-IB-INTOWE TO LXE-WORKER
           ELSE
              MOVE 0                TO LXE-WORKER.
           MOVE 0 TO LXE-WORKER2.
           MOVE LXE-REC TO WK-LXE-REC.

           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-WK-FILE.
       EXIT-SAVE-SCREEN.
           PERFORM CLOSE-WK-FILE.


      ******************************************************************
      *
      *  CREATE SPOOL DIRECTORY IN /XXX/XXXX/R#/EO
      *
      * STORE SPOOL FILE NAME AS /XXX/XXXX/R$/EO/....
      * WE NEED A FULL PATH NAME HERE ELSE DIR WILL CHANGE WHEN WE
      * CHANGE DIRECTORIES TO ANOTHER BRANCH.
      *
      * CHANGED TO ACCESS AS ../../ORIG/R1/EO/STEPBPREF, WITH ORIG
      * BEING ORIG DATA PATH, FULL PATH TOO LONG FOR ACCESS-BUF
      ******************************************************************
