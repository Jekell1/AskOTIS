      * COPYMEMBER: LIBEX/EXXTRCI
      **************************************************************
      *                (EXXTRCI)
      **************************************************************
       OPEN-XTRC-FILE.
           PERFORM OPEN-IT.
           MOVE XTRC-PATH TO E-FILE.
           OPEN I-O XTRC-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRC-FILE.
           IF IO-FG = 7
             CLOSE XTRC-FILE
             GO TO OPEN-XTRC-FILE.
           UNLOCK XTRC-FILE.
       OPEN-XTRC-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XTRC-PATH TO E-FILE.
           OPEN OUTPUT XTRC-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRC-FILE.
           IF IO-FG = 7
             CLOSE XTRC-FILE
             GO TO OPEN-XTRC-FILE.
           UNLOCK XTRC-FILE.
       READ-XTRC-FILE.
           PERFORM READ-IT.
           MOVE XTRC-PATH TO E-FILE.
           MOVE XTRC-KEY TO E-KEYX.
           READ XTRC-FILE.
           IF IO-FG = 8
             GO TO READ-XTRC-FILE.
       WRITE-XTRC-FILE.
           PERFORM WRITE-IT.
           MOVE XTRC-PATH TO E-FILE.
           MOVE XTRC-KEY TO E-KEYX.
           WRITE XTRC-REC.
           IF IO-FG = 8
             GO TO WRITE-XTRC-FILE.
           UNLOCK XTRC-FILE.
       REWRITE-XTRC-FILE.
           PERFORM REWRITE-IT.
           MOVE XTRC-PATH TO E-FILE.
           MOVE XTRC-KEY TO E-KEYX.
           REWRITE XTRC-REC.
           IF IO-FG = 8
             GO TO REWRITE-XTRC-FILE.
           UNLOCK XTRC-FILE.
       DELETE-XTRC-FILE.
           PERFORM DELETE-IT.
           MOVE XTRC-PATH TO E-FILE.
           MOVE XTRC-KEY TO E-KEYX.
           DELETE XTRC-FILE.
           IF IO-FG = 8
             GO TO DELETE-XTRC-FILE.
           UNLOCK XTRC-FILE.
       CLOSE-XTRC-FILE.
           PERFORM CLOSE-IT.
           CLOSE XTRC-FILE.
