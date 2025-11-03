      * COPYMEMBER: LIBEX/EXXTRJI
      **************************************************************
      *                (EXXTRJI)
      **************************************************************
       OPEN-XTRJ-FILE.
           PERFORM OPEN-IT.
           MOVE XTRJ-PATH TO E-FILE.
           OPEN I-O XTRJ-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRJ-FILE.
           IF IO-FG = 7
             CLOSE XTRJ-FILE
             GO TO OPEN-XTRJ-FILE.
           UNLOCK XTRJ-FILE.
       OPEN-XTRJ-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XTRJ-PATH TO E-FILE.
           OPEN OUTPUT XTRJ-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRJ-FILE.
           IF IO-FG = 7
             CLOSE XTRJ-FILE
             GO TO OPEN-XTRJ-FILE.
           UNLOCK XTRJ-FILE.
       READ-XTRJ-FILE.
           PERFORM READ-IT.
           MOVE XTRJ-PATH TO E-FILE.
           MOVE XTRJ-KEY TO E-KEYX.
           READ XTRJ-FILE.
           IF IO-FG = 8
             GO TO READ-XTRJ-FILE.
       WRITE-XTRJ-FILE.
           PERFORM WRITE-IT.
           MOVE XTRJ-PATH TO E-FILE.
           MOVE XTRJ-KEY TO E-KEYX.
           WRITE XTRJ-REC.
           IF IO-FG = 8
             GO TO WRITE-XTRJ-FILE.
           UNLOCK XTRJ-FILE.
       REWRITE-XTRJ-FILE.
           PERFORM REWRITE-IT.
           MOVE XTRJ-PATH TO E-FILE.
           MOVE XTRJ-KEY TO E-KEYX.
           REWRITE XTRJ-REC.
           IF IO-FG = 8
             GO TO REWRITE-XTRJ-FILE.
           UNLOCK XTRJ-FILE.
       DELETE-XTRJ-FILE.
           PERFORM DELETE-IT.
           MOVE XTRJ-PATH TO E-FILE.
           MOVE XTRJ-KEY TO E-KEYX.
           DELETE XTRJ-FILE.
           IF IO-FG = 8
             GO TO DELETE-XTRJ-FILE.
           UNLOCK XTRJ-FILE.
       CLOSE-XTRJ-FILE.
           PERFORM CLOSE-IT.
           CLOSE XTRJ-FILE.
