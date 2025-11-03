      * COPYMEMBER: LIBEX/EXXLPI
      **************************************************************
      *                (EXXLPI)
      **************************************************************
       OPEN-XLP-FILE.
           PERFORM OPEN-IT.
           MOVE XLP-PATH TO E-FILE.
           OPEN I-O XLP-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLP-FILE.
           IF IO-FG = 7
             CLOSE XLP-FILE
             GO TO OPEN-XLP-FILE.
           UNLOCK XLP-FILE.
       OPEN-XLP-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XLP-PATH TO E-FILE.
           OPEN OUTPUT XLP-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLP-FILE.
           IF IO-FG = 7
             CLOSE XLP-FILE
             GO TO OPEN-XLP-FILE.
           UNLOCK XLP-FILE.
       READ-XLP-FILE.
           PERFORM READ-IT.
           MOVE XLP-PATH TO E-FILE.
           MOVE XLP-KEY TO E-KEYX.
           READ XLP-FILE.
           IF IO-FG = 8
             GO TO READ-XLP-FILE.
       WRITE-XLP-FILE.
           PERFORM WRITE-IT.
           MOVE XLP-PATH TO E-FILE.
           MOVE XLP-KEY TO E-KEYX.
           WRITE XLP-REC.
           IF IO-FG = 8
             GO TO WRITE-XLP-FILE.
           UNLOCK XLP-FILE.
       REWRITE-XLP-FILE.
           PERFORM REWRITE-IT.
           MOVE XLP-PATH TO E-FILE.
           MOVE XLP-KEY TO E-KEYX.
           REWRITE XLP-REC.
           IF IO-FG = 8
             GO TO REWRITE-XLP-FILE.
           UNLOCK XLP-FILE.
       DELETE-XLP-FILE.
           PERFORM DELETE-IT.
           MOVE XLP-PATH TO E-FILE.
           MOVE XLP-KEY TO E-KEYX.
           DELETE XLP-FILE.
           IF IO-FG = 8
             GO TO DELETE-XLP-FILE.
           UNLOCK XLP-FILE.
       CLOSE-XLP-FILE.
           PERFORM CLOSE-IT.
           CLOSE XLP-FILE.
