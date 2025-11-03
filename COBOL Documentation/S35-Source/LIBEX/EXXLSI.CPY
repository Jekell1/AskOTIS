      * COPYMEMBER: LIBEX/EXXLSI
      **************************************************************
      *                (EXXLSI)
      **************************************************************
       OPEN-XLS-FILE.
           PERFORM OPEN-IT.
           MOVE XLS-PATH TO E-FILE.
           OPEN I-O XLS-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLS-FILE.
           IF IO-FG = 7
             CLOSE XLS-FILE
             GO TO OPEN-XLS-FILE.
           UNLOCK XLS-FILE.
       OPEN-XLS-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XLS-PATH TO E-FILE.
           OPEN OUTPUT XLS-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLS-FILE.
           IF IO-FG = 7
             CLOSE XLS-FILE
             GO TO OPEN-XLS-FILE.
           UNLOCK XLS-FILE.
       READ-XLS-FILE.
           PERFORM READ-IT.
           MOVE XLS-PATH TO E-FILE.
           MOVE XLS-KEY TO E-KEYX.
           READ XLS-FILE.
           IF IO-FG = 8
             GO TO READ-XLS-FILE.
       WRITE-XLS-FILE.
           PERFORM WRITE-IT.
           MOVE XLS-PATH TO E-FILE.
           MOVE XLS-KEY TO E-KEYX.
           WRITE XLS-REC.
           IF IO-FG = 8
             GO TO WRITE-XLS-FILE.
           UNLOCK XLS-FILE.
       REWRITE-XLS-FILE.
           PERFORM REWRITE-IT.
           MOVE XLS-PATH TO E-FILE.
           MOVE XLS-KEY TO E-KEYX.
           REWRITE XLS-REC.
           IF IO-FG = 8
             GO TO REWRITE-XLS-FILE.
           UNLOCK XLS-FILE.
       DELETE-XLS-FILE.
           PERFORM DELETE-IT.
           MOVE XLS-PATH TO E-FILE.
           MOVE XLS-KEY TO E-KEYX.
           DELETE XLS-FILE.
           IF IO-FG = 8
             GO TO DELETE-XLS-FILE.
           UNLOCK XLS-FILE.
       CLOSE-XLS-FILE.
           PERFORM CLOSE-IT.
           CLOSE XLS-FILE.
