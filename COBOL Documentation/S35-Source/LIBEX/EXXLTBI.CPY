      * COPYMEMBER: LIBEX/EXXLTBI
      **************************************************************
      *                (EXXLTBI)
      **************************************************************
       OPEN-XLTB-FILE.
           PERFORM OPEN-IT.
           MOVE XLTB-PATH TO E-FILE.
           OPEN I-O XLTB-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLTB-FILE.
           IF IO-FG = 7
             CLOSE XLTB-FILE
             GO TO OPEN-XLTB-FILE.
           UNLOCK XLTB-FILE.
       OPEN-XLTB-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XLTB-PATH TO E-FILE.
           OPEN OUTPUT XLTB-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLTB-FILE.
           IF IO-FG = 7
             CLOSE XLTB-FILE
             GO TO OPEN-XLTB-FILE.
           UNLOCK XLTB-FILE.
       READ-XLTB-FILE.
           PERFORM READ-IT.
           MOVE XLTB-PATH TO E-FILE.
           MOVE XLTB-KEY TO E-KEYX.
           READ XLTB-FILE.
           IF IO-FG = 8
             GO TO READ-XLTB-FILE.
       WRITE-XLTB-FILE.
           PERFORM WRITE-IT.
           MOVE XLTB-PATH TO E-FILE.
           MOVE XLTB-KEY TO E-KEYX.
           WRITE XLTB-REC.
           IF IO-FG = 8
             GO TO WRITE-XLTB-FILE.
           UNLOCK XLTB-FILE.
       REWRITE-XLTB-FILE.
           PERFORM REWRITE-IT.
           MOVE XLTB-PATH TO E-FILE.
           MOVE XLTB-KEY TO E-KEYX.
           REWRITE XLTB-REC.
           IF IO-FG = 8
             GO TO REWRITE-XLTB-FILE.
           UNLOCK XLTB-FILE.
       DELETE-XLTB-FILE.
           PERFORM DELETE-IT.
           MOVE XLTB-PATH TO E-FILE.
           MOVE XLTB-KEY TO E-KEYX.
           DELETE XLTB-FILE.
           IF IO-FG = 8
             GO TO DELETE-XLTB-FILE.
           UNLOCK XLTB-FILE.
       CLOSE-XLTB-FILE.
           PERFORM CLOSE-IT.
           CLOSE XLTB-FILE.
