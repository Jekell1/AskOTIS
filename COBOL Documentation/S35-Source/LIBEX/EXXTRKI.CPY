      * COPYMEMBER: LIBEX/EXXTRKI
      **************************************************************
      *                (EXXTRKI)
      **************************************************************
       OPEN-XTRK-FILE.
           PERFORM OPEN-IT.
           MOVE XTRK-PATH TO E-FILE.
           OPEN I-O XTRK-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRK-FILE.
           IF IO-FG = 7
             CLOSE XTRK-FILE
             GO TO OPEN-XTRK-FILE.
           UNLOCK XTRK-FILE.
       OPEN-XTRK-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XTRK-PATH TO E-FILE.
           OPEN OUTPUT XTRK-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRK-FILE.
           IF IO-FG = 7
             CLOSE XTRK-FILE
             GO TO OPEN-XTRK-FILE.
           UNLOCK XTRK-FILE.
       READ-XTRK-FILE.
           PERFORM READ-IT.
           MOVE XTRK-PATH TO E-FILE.
           MOVE XTRK-KEY TO E-KEYX.
           READ XTRK-FILE.
           IF IO-FG = 8
             GO TO READ-XTRK-FILE.
       WRITE-XTRK-FILE.
           PERFORM WRITE-IT.
           MOVE XTRK-PATH TO E-FILE.
           MOVE XTRK-KEY TO E-KEYX.
           WRITE XTRK-REC.
           IF IO-FG = 8
             GO TO WRITE-XTRK-FILE.
           UNLOCK XTRK-FILE.
       REWRITE-XTRK-FILE.
           PERFORM REWRITE-IT.
           MOVE XTRK-PATH TO E-FILE.
           MOVE XTRK-KEY TO E-KEYX.
           REWRITE XTRK-REC.
           IF IO-FG = 8
             GO TO REWRITE-XTRK-FILE.
           UNLOCK XTRK-FILE.
       DELETE-XTRK-FILE.
           PERFORM DELETE-IT.
           MOVE XTRK-PATH TO E-FILE.
           MOVE XTRK-KEY TO E-KEYX.
           DELETE XTRK-FILE.
           IF IO-FG = 8
             GO TO DELETE-XTRK-FILE.
           UNLOCK XTRK-FILE.
       CLOSE-XTRK-FILE.
           PERFORM CLOSE-IT.
           CLOSE XTRK-FILE.
