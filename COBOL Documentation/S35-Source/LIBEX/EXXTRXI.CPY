      * COPYMEMBER: LIBEX/EXXTRXI
      **************************************************************
      *                (EXXTRXI)
      **************************************************************
       OPEN-XTRX-FILE.
           PERFORM OPEN-IT.
           MOVE XTRX-PATH TO E-FILE.
           OPEN I-O XTRX-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRX-FILE.
           IF IO-FG = 7
             CLOSE XTRX-FILE
             GO TO OPEN-XTRX-FILE.
           UNLOCK XTRX-FILE.
       OPEN-XTRX-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XTRX-PATH TO E-FILE.
           OPEN OUTPUT XTRX-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRX-FILE.
           IF IO-FG = 7
             CLOSE XTRX-FILE
             GO TO OPEN-XTRX-FILE.
           UNLOCK XTRX-FILE.
       READ-XTRX-FILE.
           PERFORM READ-IT.
           MOVE XTRX-PATH TO E-FILE.
           MOVE XTRX-KEY TO E-KEYX.
           READ XTRX-FILE.
           IF IO-FG = 8
             GO TO READ-XTRX-FILE.
       WRITE-XTRX-FILE.
           PERFORM WRITE-IT.
           MOVE XTRX-PATH TO E-FILE.
           MOVE XTRX-KEY TO E-KEYX.
           WRITE XTRX-REC.
           IF IO-FG = 8
             GO TO WRITE-XTRX-FILE.
           UNLOCK XTRX-FILE.
       REWRITE-XTRX-FILE.
           PERFORM REWRITE-IT.
           MOVE XTRX-PATH TO E-FILE.
           MOVE XTRX-KEY TO E-KEYX.
           REWRITE XTRX-REC.
           IF IO-FG = 8
             GO TO REWRITE-XTRX-FILE.
           UNLOCK XTRX-FILE.
       DELETE-XTRX-FILE.
           PERFORM DELETE-IT.
           MOVE XTRX-PATH TO E-FILE.
           MOVE XTRX-KEY TO E-KEYX.
           DELETE XTRX-FILE.
           IF IO-FG = 8
             GO TO DELETE-XTRX-FILE.
           UNLOCK XTRX-FILE.
       CLOSE-XTRX-FILE.
           PERFORM CLOSE-IT.
           CLOSE XTRX-FILE.
