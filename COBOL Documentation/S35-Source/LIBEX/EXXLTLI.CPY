      * COPYMEMBER: LIBEX/EXXLTLI
      **************************************************************
      *                (EXXLTLI)
      **************************************************************
       OPEN-XLTL-FILE.
           PERFORM OPEN-IT.
           MOVE XLTL-PATH TO E-FILE.
           OPEN I-O XLTL-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLTL-FILE.
           IF IO-FG = 7
             CLOSE XLTL-FILE
             GO TO OPEN-XLTL-FILE.
           UNLOCK XLTL-FILE.
       OPEN-XLTL-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XLTL-PATH TO E-FILE.
           OPEN OUTPUT XLTL-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLTL-FILE.
           IF IO-FG = 7
             CLOSE XLTL-FILE
             GO TO OPEN-XLTL-FILE.
           UNLOCK XLTL-FILE.
       READ-XLTL-FILE.
           PERFORM READ-IT.
           MOVE XLTL-PATH TO E-FILE.
           MOVE XLTL-KEY TO E-KEYX.
           READ XLTL-FILE.
           IF IO-FG = 8
             GO TO READ-XLTL-FILE.
       WRITE-XLTL-FILE.
           PERFORM WRITE-IT.
           MOVE XLTL-PATH TO E-FILE.
           MOVE XLTL-KEY TO E-KEYX.
           WRITE XLTL-REC.
           IF IO-FG = 8
             GO TO WRITE-XLTL-FILE.
           UNLOCK XLTL-FILE.
       REWRITE-XLTL-FILE.
           PERFORM REWRITE-IT.
           MOVE XLTL-PATH TO E-FILE.
           MOVE XLTL-KEY TO E-KEYX.
           REWRITE XLTL-REC.
           IF IO-FG = 8
             GO TO REWRITE-XLTL-FILE.
           UNLOCK XLTL-FILE.
       DELETE-XLTL-FILE.
           PERFORM DELETE-IT.
           MOVE XLTL-PATH TO E-FILE.
           MOVE XLTL-KEY TO E-KEYX.
           DELETE XLTL-FILE.
           IF IO-FG = 8
             GO TO DELETE-XLTL-FILE.
           UNLOCK XLTL-FILE.
       CLOSE-XLTL-FILE.
           PERFORM CLOSE-IT.
           CLOSE XLTL-FILE.
