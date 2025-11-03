      * COPYMEMBER: LIBEX/EXXLTAI
      **************************************************************
      *                (EXXLTAI)
      **************************************************************
       OPEN-XLTA-FILE.
           PERFORM OPEN-IT.
           MOVE XLTA-PATH TO E-FILE.
           OPEN I-O XLTA-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLTA-FILE.
           IF IO-FG = 7
             CLOSE XLTA-FILE
             GO TO OPEN-XLTA-FILE.
           UNLOCK XLTA-FILE.
       OPEN-XLTA-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XLTA-PATH TO E-FILE.
           OPEN OUTPUT XLTA-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLTA-FILE.
           IF IO-FG = 7
             CLOSE XLTA-FILE
             GO TO OPEN-XLTA-FILE.
           UNLOCK XLTA-FILE.
       READ-XLTA-FILE.
           PERFORM READ-IT.
           MOVE XLTA-PATH TO E-FILE.
           MOVE XLTA-KEY TO E-KEYX.
           READ XLTA-FILE.
           IF IO-FG = 8
             GO TO READ-XLTA-FILE.
       WRITE-XLTA-FILE.
           PERFORM WRITE-IT.
           MOVE XLTA-PATH TO E-FILE.
           MOVE XLTA-KEY TO E-KEYX.
           WRITE XLTA-REC.
           IF IO-FG = 8
             GO TO WRITE-XLTA-FILE.
           UNLOCK XLTA-FILE.
       REWRITE-XLTA-FILE.
           PERFORM REWRITE-IT.
           MOVE XLTA-PATH TO E-FILE.
           MOVE XLTA-KEY TO E-KEYX.
           REWRITE XLTA-REC.
           IF IO-FG = 8
             GO TO REWRITE-XLTA-FILE.
           UNLOCK XLTA-FILE.
       DELETE-XLTA-FILE.
           PERFORM DELETE-IT.
           MOVE XLTA-PATH TO E-FILE.
           MOVE XLTA-KEY TO E-KEYX.
           DELETE XLTA-FILE.
           IF IO-FG = 8
             GO TO DELETE-XLTA-FILE.
           UNLOCK XLTA-FILE.
       CLOSE-XLTA-FILE.
           PERFORM CLOSE-IT.
           CLOSE XLTA-FILE.
