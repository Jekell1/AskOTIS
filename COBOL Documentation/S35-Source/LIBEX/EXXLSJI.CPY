      * COPYMEMBER: LIBEX/EXXLSJI
      **************************************************************
      *                (EXXLSJI)
      **************************************************************
       OPEN-XLSJ-FILE.
           PERFORM OPEN-IT.
           MOVE XLSJ-PATH TO E-FILE.
           OPEN I-O XLSJ-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLSJ-FILE.
           IF IO-FG = 7
             CLOSE XLSJ-FILE
             GO TO OPEN-XLSJ-FILE.
           UNLOCK XLSJ-FILE.
       OPEN-XLSJ-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XLSJ-PATH TO E-FILE.
           OPEN OUTPUT XLSJ-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLSJ-FILE.
           IF IO-FG = 7
             CLOSE XLSJ-FILE
             GO TO OPEN-XLSJ-FILE.
           UNLOCK XLSJ-FILE.
       READ-XLSJ-FILE.
           PERFORM READ-IT.
           MOVE XLSJ-PATH TO E-FILE.
           MOVE XLSJ-KEY TO E-KEYX.
           READ XLSJ-FILE.
           IF IO-FG = 8
             GO TO READ-XLSJ-FILE.
       WRITE-XLSJ-FILE.
           PERFORM WRITE-IT.
           MOVE XLSJ-PATH TO E-FILE.
           MOVE XLSJ-KEY TO E-KEYX.
           WRITE XLSJ-REC.
           IF IO-FG = 8
             GO TO WRITE-XLSJ-FILE.
           UNLOCK XLSJ-FILE.
       REWRITE-XLSJ-FILE.
           PERFORM REWRITE-IT.
           MOVE XLSJ-PATH TO E-FILE.
           MOVE XLSJ-KEY TO E-KEYX.
           REWRITE XLSJ-REC.
           IF IO-FG = 8
             GO TO REWRITE-XLSJ-FILE.
           UNLOCK XLSJ-FILE.
       DELETE-XLSJ-FILE.
           PERFORM DELETE-IT.
           MOVE XLSJ-PATH TO E-FILE.
           MOVE XLSJ-KEY TO E-KEYX.
           DELETE XLSJ-FILE.
           IF IO-FG = 8
             GO TO DELETE-XLSJ-FILE.
           UNLOCK XLSJ-FILE.
       CLOSE-XLSJ-FILE.
           PERFORM CLOSE-IT.
           CLOSE XLSJ-FILE.
