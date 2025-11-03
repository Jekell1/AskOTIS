      * COPYMEMBER: LIBEX/EXXLMI
      **************************************************************
      *                (EXXLMI)
      **************************************************************
       OPEN-XLM-FILE.
           PERFORM OPEN-IT.
           MOVE XLM-PATH TO E-FILE.
           OPEN I-O XLM-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLM-FILE.
           IF IO-FG = 7
             CLOSE XLM-FILE
             GO TO OPEN-XLM-FILE.
           UNLOCK XLM-FILE.
       OPEN-XLM-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XLM-PATH TO E-FILE.
           OPEN OUTPUT XLM-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLM-FILE.
           IF IO-FG = 7
             CLOSE XLM-FILE
             GO TO OPEN-XLM-FILE.
           UNLOCK XLM-FILE.
       READ-XLM-FILE.
           PERFORM READ-IT.
           MOVE XLM-PATH TO E-FILE.
           MOVE XLM-KEY TO E-KEYX.
           READ XLM-FILE.
           IF IO-FG = 8
             GO TO READ-XLM-FILE.
       WRITE-XLM-FILE.
           PERFORM WRITE-IT.
           MOVE XLM-PATH TO E-FILE.
           MOVE XLM-KEY TO E-KEYX.
           WRITE XLM-REC.
           IF IO-FG = 8
             GO TO WRITE-XLM-FILE.
           UNLOCK XLM-FILE.
       REWRITE-XLM-FILE.
           PERFORM REWRITE-IT.
           MOVE XLM-PATH TO E-FILE.
           MOVE XLM-KEY TO E-KEYX.
           REWRITE XLM-REC.
           IF IO-FG = 8
             GO TO REWRITE-XLM-FILE.
           UNLOCK XLM-FILE.
       DELETE-XLM-FILE.
           PERFORM DELETE-IT.
           MOVE XLM-PATH TO E-FILE.
           MOVE XLM-KEY TO E-KEYX.
           DELETE XLM-FILE.
           IF IO-FG = 8
             GO TO DELETE-XLM-FILE.
           UNLOCK XLM-FILE.
       CLOSE-XLM-FILE.
           PERFORM CLOSE-IT.
           CLOSE XLM-FILE.
