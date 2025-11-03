      * COPYMEMBER: LIBEX/EXXLTCI
      **************************************************************
      *                (EXXLTCI)
      **************************************************************
       OPEN-XLTC-FILE.
           PERFORM OPEN-IT.
           MOVE XLTC-PATH TO E-FILE.
           OPEN I-O XLTC-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLTC-FILE.
           IF IO-FG = 7
             CLOSE XLTC-FILE
             GO TO OPEN-XLTC-FILE.
           UNLOCK XLTC-FILE.
       OPEN-XLTC-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XLTC-PATH TO E-FILE.
           OPEN OUTPUT XLTC-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLTC-FILE.
           IF IO-FG = 7
             CLOSE XLTC-FILE
             GO TO OPEN-XLTC-FILE.
           UNLOCK XLTC-FILE.
       READ-XLTC-FILE.
           PERFORM READ-IT.
           MOVE XLTC-PATH TO E-FILE.
           MOVE XLTC-KEY TO E-KEYX.
           READ XLTC-FILE.
           IF IO-FG = 8
             GO TO READ-XLTC-FILE.
       WRITE-XLTC-FILE.
           PERFORM WRITE-IT.
           MOVE XLTC-PATH TO E-FILE.
           MOVE XLTC-KEY TO E-KEYX.
           WRITE XLTC-REC.
           IF IO-FG = 8
             GO TO WRITE-XLTC-FILE.
           UNLOCK XLTC-FILE.
       REWRITE-XLTC-FILE.
           PERFORM REWRITE-IT.
           MOVE XLTC-PATH TO E-FILE.
           MOVE XLTC-KEY TO E-KEYX.
           REWRITE XLTC-REC.
           IF IO-FG = 8
             GO TO REWRITE-XLTC-FILE.
           UNLOCK XLTC-FILE.
       DELETE-XLTC-FILE.
           PERFORM DELETE-IT.
           MOVE XLTC-PATH TO E-FILE.
           MOVE XLTC-KEY TO E-KEYX.
           DELETE XLTC-FILE.
           IF IO-FG = 8
             GO TO DELETE-XLTC-FILE.
           UNLOCK XLTC-FILE.
       CLOSE-XLTC-FILE.
           PERFORM CLOSE-IT.
           CLOSE XLTC-FILE.
