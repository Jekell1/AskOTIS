      * COPYMEMBER: LIBEX/EXXCAI
      **************************************************************
      *                (EXXCAI)
      **************************************************************
       OPEN-XCA-FILE.
           PERFORM OPEN-IT.
           MOVE XCA-PATH TO E-FILE.
           OPEN I-O XCA-FILE.
           IF IO-FG = 8
             GO TO OPEN-XCA-FILE.
           IF IO-FG = 7
             CLOSE XCA-FILE
             GO TO OPEN-XCA-FILE.
           UNLOCK XCA-FILE.
       OPEN-XCA-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XCA-PATH TO E-FILE.
           OPEN OUTPUT XCA-FILE.
           IF IO-FG = 8
             GO TO OPEN-XCA-FILE.
           IF IO-FG = 7
             CLOSE XCA-FILE
             GO TO OPEN-XCA-FILE.
           UNLOCK XCA-FILE.
       READ-XCA-FILE.
           PERFORM READ-IT.
           MOVE XCA-PATH TO E-FILE.
           MOVE XCA-KEY TO E-KEYX.
           READ XCA-FILE.
           IF IO-FG = 8
             GO TO READ-XCA-FILE.
       WRITE-XCA-FILE.
           PERFORM WRITE-IT.
           MOVE XCA-PATH TO E-FILE.
           MOVE XCA-KEY TO E-KEYX.
           WRITE XCA-REC.
           IF IO-FG = 8
             GO TO WRITE-XCA-FILE.
           UNLOCK XCA-FILE.
       REWRITE-XCA-FILE.
           PERFORM REWRITE-IT.
           MOVE XCA-PATH TO E-FILE.
           MOVE XCA-KEY TO E-KEYX.
           REWRITE XCA-REC.
           IF IO-FG = 8
             GO TO REWRITE-XCA-FILE.
           UNLOCK XCA-FILE.
       DELETE-XCA-FILE.
           PERFORM DELETE-IT.
           MOVE XCA-PATH TO E-FILE.
           MOVE XCA-KEY TO E-KEYX.
           DELETE XCA-FILE.
           IF IO-FG = 8
             GO TO DELETE-XCA-FILE.
           UNLOCK XCA-FILE.
       CLOSE-XCA-FILE.
           PERFORM CLOSE-IT.
           CLOSE XCA-FILE.
