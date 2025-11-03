      * COPYMEMBER: LIBEX/EXXLSFI
      **************************************************************
      *                (EXXLSFI)
      **************************************************************
       OPEN-XLSF-FILE.
           PERFORM OPEN-IT.
           MOVE XLSF-PATH TO E-FILE.
           OPEN I-O XLSF-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLSF-FILE.
           IF IO-FG = 7
             CLOSE XLSF-FILE
             GO TO OPEN-XLSF-FILE.
           UNLOCK XLSF-FILE.
       OPEN-XLSF-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XLSF-PATH TO E-FILE.
           OPEN OUTPUT XLSF-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLSF-FILE.
           IF IO-FG = 7
             CLOSE XLSF-FILE
             GO TO OPEN-XLSF-FILE.
           UNLOCK XLSF-FILE.
       READ-XLSF-FILE.
           PERFORM READ-IT.
           MOVE XLSF-PATH TO E-FILE.
           MOVE XLSF-KEY TO E-KEYX.
           READ XLSF-FILE.
           IF IO-FG = 8
             GO TO READ-XLSF-FILE.
       WRITE-XLSF-FILE.
           PERFORM WRITE-IT.
           MOVE XLSF-PATH TO E-FILE.
           MOVE XLSF-KEY TO E-KEYX.
           WRITE XLSF-REC.
           IF IO-FG = 8
             GO TO WRITE-XLSF-FILE.
           UNLOCK XLSF-FILE.
       REWRITE-XLSF-FILE.
           PERFORM REWRITE-IT.
           MOVE XLSF-PATH TO E-FILE.
           MOVE XLSF-KEY TO E-KEYX.
           REWRITE XLSF-REC.
           IF IO-FG = 8
             GO TO REWRITE-XLSF-FILE.
           UNLOCK XLSF-FILE.
       DELETE-XLSF-FILE.
           PERFORM DELETE-IT.
           MOVE XLSF-PATH TO E-FILE.
           MOVE XLSF-KEY TO E-KEYX.
           DELETE XLSF-FILE.
           IF IO-FG = 8
             GO TO DELETE-XLSF-FILE.
           UNLOCK XLSF-FILE.
       CLOSE-XLSF-FILE.
           PERFORM CLOSE-IT.
           CLOSE XLSF-FILE.
