      * COPYMEMBER: LIBEX/EXXLSDI
      **************************************************************
      *                (EXXLSDI)
      **************************************************************
       OPEN-XLSD-FILE.
           PERFORM OPEN-IT.
           MOVE XLSD-PATH TO E-FILE.
           OPEN I-O XLSD-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLSD-FILE.
           IF IO-FG = 7
             CLOSE XLSD-FILE
             GO TO OPEN-XLSD-FILE.
           UNLOCK XLSD-FILE.
       OPEN-XLSD-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XLSD-PATH TO E-FILE.
           OPEN OUTPUT XLSD-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLSD-FILE.
           IF IO-FG = 7
             CLOSE XLSD-FILE
             GO TO OPEN-XLSD-FILE.
           UNLOCK XLSD-FILE.
       READ-XLSD-FILE.
           PERFORM READ-IT.
           MOVE XLSD-PATH TO E-FILE.
           MOVE XLSD-KEY TO E-KEYX.
           READ XLSD-FILE.
           IF IO-FG = 8
             GO TO READ-XLSD-FILE.
       WRITE-XLSD-FILE.
           PERFORM WRITE-IT.
           MOVE XLSD-PATH TO E-FILE.
           MOVE XLSD-KEY TO E-KEYX.
           WRITE XLSD-REC.
           IF IO-FG = 8
             GO TO WRITE-XLSD-FILE.
           UNLOCK XLSD-FILE.
       REWRITE-XLSD-FILE.
           PERFORM REWRITE-IT.
           MOVE XLSD-PATH TO E-FILE.
           MOVE XLSD-KEY TO E-KEYX.
           REWRITE XLSD-REC.
           IF IO-FG = 8
             GO TO REWRITE-XLSD-FILE.
           UNLOCK XLSD-FILE.
       DELETE-XLSD-FILE.
           PERFORM DELETE-IT.
           MOVE XLSD-PATH TO E-FILE.
           MOVE XLSD-KEY TO E-KEYX.
           DELETE XLSD-FILE.
           IF IO-FG = 8
             GO TO DELETE-XLSD-FILE.
           UNLOCK XLSD-FILE.
       CLOSE-XLSD-FILE.
           PERFORM CLOSE-IT.
           CLOSE XLSD-FILE.
