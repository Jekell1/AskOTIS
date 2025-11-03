      * COPYMEMBER: LIBEX/EXXLTDI
      **************************************************************
      *                (EXXLTDI)
      **************************************************************
       OPEN-XLTD-FILE.
           PERFORM OPEN-IT.
           MOVE XLTD-PATH TO E-FILE.
           OPEN I-O XLTD-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLTD-FILE.
           IF IO-FG = 7
             CLOSE XLTD-FILE
             GO TO OPEN-XLTD-FILE.
           UNLOCK XLTD-FILE.
       OPEN-XLTD-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XLTD-PATH TO E-FILE.
           OPEN OUTPUT XLTD-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLTD-FILE.
           IF IO-FG = 7
             CLOSE XLTD-FILE
             GO TO OPEN-XLTD-FILE.
           UNLOCK XLTD-FILE.
       READ-XLTD-FILE.
           PERFORM READ-IT.
           MOVE XLTD-PATH TO E-FILE.
           MOVE XLTD-KEY TO E-KEYX.
           READ XLTD-FILE.
           IF IO-FG = 8
             GO TO READ-XLTD-FILE.
       WRITE-XLTD-FILE.
           PERFORM WRITE-IT.
           MOVE XLTD-PATH TO E-FILE.
           MOVE XLTD-KEY TO E-KEYX.
           WRITE XLTD-REC.
           IF IO-FG = 8
             GO TO WRITE-XLTD-FILE.
           UNLOCK XLTD-FILE.
       REWRITE-XLTD-FILE.
           PERFORM REWRITE-IT.
           MOVE XLTD-PATH TO E-FILE.
           MOVE XLTD-KEY TO E-KEYX.
           REWRITE XLTD-REC.
           IF IO-FG = 8
             GO TO REWRITE-XLTD-FILE.
           UNLOCK XLTD-FILE.
       DELETE-XLTD-FILE.
           PERFORM DELETE-IT.
           MOVE XLTD-PATH TO E-FILE.
           MOVE XLTD-KEY TO E-KEYX.
           DELETE XLTD-FILE.
           IF IO-FG = 8
             GO TO DELETE-XLTD-FILE.
           UNLOCK XLTD-FILE.
       CLOSE-XLTD-FILE.
           PERFORM CLOSE-IT.
           CLOSE XLTD-FILE.
