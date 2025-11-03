      * COPYMEMBER: LIBEX/EXXTRDI
      **************************************************************
      *                (EXXTRDI)
      **************************************************************
       OPEN-XTRD-FILE.
           PERFORM OPEN-IT.
           MOVE XTRD-PATH TO E-FILE.
           OPEN I-O XTRD-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRD-FILE.
           IF IO-FG = 7
             CLOSE XTRD-FILE
             GO TO OPEN-XTRD-FILE.
           UNLOCK XTRD-FILE.
       OPEN-XTRD-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XTRD-PATH TO E-FILE.
           OPEN OUTPUT XTRD-FILE.
           IF IO-FG = 8
             GO TO OPEN-XTRD-FILE.
           IF IO-FG = 7
             CLOSE XTRD-FILE
             GO TO OPEN-XTRD-FILE.
           UNLOCK XTRD-FILE.
       READ-XTRD-FILE.
           PERFORM READ-IT.
           MOVE XTRD-PATH TO E-FILE.
           MOVE XTRD-KEY TO E-KEYX.
           READ XTRD-FILE.
           IF IO-FG = 8
             GO TO READ-XTRD-FILE.
       WRITE-XTRD-FILE.
           PERFORM WRITE-IT.
           MOVE XTRD-PATH TO E-FILE.
           MOVE XTRD-KEY TO E-KEYX.
           WRITE XTRD-REC.
           IF IO-FG = 8
             GO TO WRITE-XTRD-FILE.
           UNLOCK XTRD-FILE.
       REWRITE-XTRD-FILE.
           PERFORM REWRITE-IT.
           MOVE XTRD-PATH TO E-FILE.
           MOVE XTRD-KEY TO E-KEYX.
           REWRITE XTRD-REC.
           IF IO-FG = 8
             GO TO REWRITE-XTRD-FILE.
           UNLOCK XTRD-FILE.
       DELETE-XTRD-FILE.
           PERFORM DELETE-IT.
           MOVE XTRD-PATH TO E-FILE.
           MOVE XTRD-KEY TO E-KEYX.
           DELETE XTRD-FILE.
           IF IO-FG = 8
             GO TO DELETE-XTRD-FILE.
           UNLOCK XTRD-FILE.
       CLOSE-XTRD-FILE.
           PERFORM CLOSE-IT.
           CLOSE XTRD-FILE.
