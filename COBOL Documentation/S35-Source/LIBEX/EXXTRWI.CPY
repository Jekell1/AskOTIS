      * COPYMEMBER: LIBEX/EXXTRWI
      **************************************************************
      *                (EXXTRWI)
      **************************************************************
       OPEN-XTRW-FILE.
           PERFORM OPEN-IT.
           MOVE XTRW-PATH TO E-FILE.
           OPEN I-O XTRW-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRW-FILE.
           IF IO-FG = 7
             CLOSE XTRW-FILE
             GO TO OPEN-XTRW-FILE.
           UNLOCK XTRW-FILE.
       OPEN-XTRW-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XTRW-PATH TO E-FILE.
           OPEN OUTPUT XTRW-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRW-FILE.
           IF IO-FG = 7
             CLOSE XTRW-FILE
             GO TO OPEN-XTRW-FILE.
           UNLOCK XTRW-FILE.
       READ-XTRW-FILE.
           PERFORM READ-IT.
           MOVE XTRW-PATH TO E-FILE.
           MOVE XTRW-KEY TO E-KEYX.
           READ XTRW-FILE.
           IF IO-FG = 8
             GO TO READ-XTRW-FILE.
       WRITE-XTRW-FILE.
           PERFORM WRITE-IT.
           MOVE XTRW-PATH TO E-FILE.
           MOVE XTRW-KEY TO E-KEYX.
           WRITE XTRW-REC.
           IF IO-FG = 8
             GO TO WRITE-XTRW-FILE.
           UNLOCK XTRW-FILE.
       REWRITE-XTRW-FILE.
           PERFORM REWRITE-IT.
           MOVE XTRW-PATH TO E-FILE.
           MOVE XTRW-KEY TO E-KEYX.
           REWRITE XTRW-REC.
           IF IO-FG = 8
             GO TO REWRITE-XTRW-FILE.
           UNLOCK XTRW-FILE.
       DELETE-XTRW-FILE.
           PERFORM DELETE-IT.
           MOVE XTRW-PATH TO E-FILE.
           MOVE XTRW-KEY TO E-KEYX.
           DELETE XTRW-FILE.
           IF IO-FG = 8
             GO TO DELETE-XTRW-FILE.
           UNLOCK XTRW-FILE.
       CLOSE-XTRW-FILE.
           PERFORM CLOSE-IT.
           CLOSE XTRW-FILE.
