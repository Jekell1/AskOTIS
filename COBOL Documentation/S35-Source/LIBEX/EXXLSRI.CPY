      * COPYMEMBER: LIBEX/EXXLSRI
      **************************************************************
      *                (EXXLSRI)
      **************************************************************
       OPEN-XLSR-FILE.
           PERFORM OPEN-IT.
           MOVE XLSR-PATH TO E-FILE.
           OPEN I-O XLSR-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLSR-FILE.
           IF IO-FG = 7
             CLOSE XLSR-FILE
             GO TO OPEN-XLSR-FILE.
           UNLOCK XLSR-FILE.
       OPEN-XLSR-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XLSR-PATH TO E-FILE.
           OPEN OUTPUT XLSR-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLSR-FILE.
           IF IO-FG = 7
             CLOSE XLSR-FILE
             GO TO OPEN-XLSR-FILE.
           UNLOCK XLSR-FILE.
       READ-XLSR-FILE.
           PERFORM READ-IT.
           MOVE XLSR-PATH TO E-FILE.
           MOVE XLSR-KEY TO E-KEYX.
           READ XLSR-FILE.
           IF IO-FG = 8
             GO TO READ-XLSR-FILE.
       WRITE-XLSR-FILE.
           PERFORM WRITE-IT.
           MOVE XLSR-PATH TO E-FILE.
           MOVE XLSR-KEY TO E-KEYX.
           WRITE XLSR-REC.
           IF IO-FG = 8
             GO TO WRITE-XLSR-FILE.
           UNLOCK XLSR-FILE.
       REWRITE-XLSR-FILE.
           PERFORM REWRITE-IT.
           MOVE XLSR-PATH TO E-FILE.
           MOVE XLSR-KEY TO E-KEYX.
           REWRITE XLSR-REC.
           IF IO-FG = 8
             GO TO REWRITE-XLSR-FILE.
           UNLOCK XLSR-FILE.
       DELETE-XLSR-FILE.
           PERFORM DELETE-IT.
           MOVE XLSR-PATH TO E-FILE.
           MOVE XLSR-KEY TO E-KEYX.
           DELETE XLSR-FILE.
           IF IO-FG = 8
             GO TO DELETE-XLSR-FILE.
           UNLOCK XLSR-FILE.
       CLOSE-XLSR-FILE.
           PERFORM CLOSE-IT.
           CLOSE XLSR-FILE.
