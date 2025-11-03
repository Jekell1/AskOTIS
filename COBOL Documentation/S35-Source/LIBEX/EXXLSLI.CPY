      * COPYMEMBER: LIBEX/EXXLSLI
      **************************************************************
      *                (EXXLSLI) LEGAL LSFILE "L"
      **************************************************************
       OPEN-XLSL-FILE.
           PERFORM OPEN-IT.
           MOVE XLSL-PATH TO E-FILE.
           OPEN I-O XLSL-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLSL-FILE.
           IF IO-FG = 7
             CLOSE XLSL-FILE
             GO TO OPEN-XLSL-FILE.
           UNLOCK XLSL-FILE.
       OPEN-XLSL-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XLSL-PATH TO E-FILE.
           OPEN OUTPUT XLSL-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLSL-FILE.
           IF IO-FG = 7
             CLOSE XLSL-FILE
             GO TO OPEN-XLSL-FILE.
           UNLOCK XLSL-FILE.
       READ-XLSL-FILE.
           PERFORM READ-IT.
           MOVE XLSL-PATH TO E-FILE.
           MOVE XLSL-KEY TO E-KEYX.
           READ XLSL-FILE.
           IF IO-FG = 8
             GO TO READ-XLSL-FILE.
       WRITE-XLSL-FILE.
           PERFORM WRITE-IT.
           MOVE XLSL-PATH TO E-FILE.
           MOVE XLSL-KEY TO E-KEYX.
           WRITE XLSL-REC.
           IF IO-FG = 8
             GO TO WRITE-XLSL-FILE.
           UNLOCK XLSL-FILE.
       REWRITE-XLSL-FILE.
           PERFORM REWRITE-IT.
           MOVE XLSL-PATH TO E-FILE.
           MOVE XLSL-KEY TO E-KEYX.
           REWRITE XLSL-REC.
           IF IO-FG = 8
             GO TO REWRITE-XLSL-FILE.
           UNLOCK XLSL-FILE.
       DELETE-XLSL-FILE.
           PERFORM DELETE-IT.
           MOVE XLSL-PATH TO E-FILE.
           MOVE XLSL-KEY TO E-KEYX.
           DELETE XLSL-FILE.
           IF IO-FG = 8
             GO TO DELETE-XLSL-FILE.
           UNLOCK XLSL-FILE.
       CLOSE-XLSL-FILE.
           PERFORM CLOSE-IT.
           CLOSE XLSL-FILE.

