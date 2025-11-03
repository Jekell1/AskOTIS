      * COPYMEMBER: LIBEX/EXXGQI
      **************************************************************
      *                (EXXGQI)
      **************************************************************
       OPEN-XGQ-FILE.
           PERFORM OPEN-IT.
           MOVE XGQ-PATH TO E-FILE.
           OPEN I-O XGQ-FILE.
           IF IO-FG = 8
             GO TO OPEN-XGQ-FILE.
           IF IO-FG = 7
             CLOSE XGQ-FILE
             GO TO OPEN-XGQ-FILE.
           UNLOCK XGQ-FILE.
       OPEN-XGQ-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XGQ-PATH TO E-FILE.
           OPEN OUTPUT XGQ-FILE.
           IF IO-FG = 8
             GO TO OPEN-XGQ-FILE.
           IF IO-FG = 7
             CLOSE XGQ-FILE
             GO TO OPEN-XGQ-FILE.
           UNLOCK XGQ-FILE.
       READ-XGQ-FILE.
           PERFORM READ-IT.
           MOVE XGQ-PATH TO E-FILE.
           MOVE XGQ-KEY TO E-KEYX.
           READ XGQ-FILE.
           IF IO-FG = 8
             GO TO READ-XGQ-FILE.
       WRITE-XGQ-FILE.
           PERFORM WRITE-IT.
           MOVE XGQ-PATH TO E-FILE.
           MOVE XGQ-KEY TO E-KEYX.
           WRITE XGQ-REC.
           IF IO-FG = 8
             GO TO WRITE-XGQ-FILE.
           UNLOCK XGQ-FILE.
       REWRITE-XGQ-FILE.
           PERFORM REWRITE-IT.
           MOVE XGQ-PATH TO E-FILE.
           MOVE XGQ-KEY TO E-KEYX.
           REWRITE XGQ-REC.
           IF IO-FG = 8
             GO TO REWRITE-XGQ-FILE.
           UNLOCK XGQ-FILE.
       DELETE-XGQ-FILE.
           PERFORM DELETE-IT.
           MOVE XGQ-PATH TO E-FILE.
           MOVE XGQ-KEY TO E-KEYX.
           DELETE XGQ-FILE.
           IF IO-FG = 8
             GO TO DELETE-XGQ-FILE.
           UNLOCK XGQ-FILE.
       CLOSE-XGQ-FILE.
           PERFORM CLOSE-IT.
           CLOSE XGQ-FILE.
