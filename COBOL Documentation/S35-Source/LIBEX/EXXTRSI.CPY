      * COPYMEMBER: LIBEX/EXXTRSI
      **************************************************************
      *                (EXXTRSI)
      **************************************************************
       OPEN-XTRS-FILE.
           PERFORM OPEN-IT.
           MOVE XTRS-PATH TO E-FILE.
           OPEN I-O XTRS-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRS-FILE.
           IF IO-FG = 7
             CLOSE XTRS-FILE
             GO TO OPEN-XTRS-FILE.
           UNLOCK XTRS-FILE.
       OPEN-XTRS-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XTRS-PATH TO E-FILE.
           OPEN OUTPUT XTRS-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRS-FILE.
           IF IO-FG = 7
             CLOSE XTRS-FILE
             GO TO OPEN-XTRS-FILE.
           UNLOCK XTRS-FILE.
       READ-XTRS-FILE.
           PERFORM READ-IT.
           MOVE XTRS-PATH TO E-FILE.
           MOVE XTRS-KEY TO E-KEYX.
           READ XTRS-FILE.
           IF IO-FG = 8
             GO TO READ-XTRS-FILE.
       WRITE-XTRS-FILE.
           PERFORM WRITE-IT.
           MOVE XTRS-PATH TO E-FILE.
           MOVE XTRS-KEY TO E-KEYX.
           WRITE XTRS-REC.
           IF IO-FG = 8
             GO TO WRITE-XTRS-FILE.
           UNLOCK XTRS-FILE.
       REWRITE-XTRS-FILE.
           PERFORM REWRITE-IT.
           MOVE XTRS-PATH TO E-FILE.
           MOVE XTRS-KEY TO E-KEYX.
           REWRITE XTRS-REC.
           IF IO-FG = 8
             GO TO REWRITE-XTRS-FILE.
           UNLOCK XTRS-FILE.
       DELETE-XTRS-FILE.
           PERFORM DELETE-IT.
           MOVE XTRS-PATH TO E-FILE.
           MOVE XTRS-KEY TO E-KEYX.
           DELETE XTRS-FILE.
           IF IO-FG = 8
             GO TO DELETE-XTRS-FILE.
           UNLOCK XTRS-FILE.
       CLOSE-XTRS-FILE.
           PERFORM CLOSE-IT.
           CLOSE XTRS-FILE.
