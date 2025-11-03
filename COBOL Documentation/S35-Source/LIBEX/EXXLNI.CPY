      * COPYMEMBER: LIBEX/EXXLNI
      **************************************************************
      *                (EXXLNI)
      **************************************************************
       OPEN-XLN-FILE.
           PERFORM OPEN-IT.
           MOVE XLN-PATH TO E-FILE.
           OPEN I-O XLN-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLN-FILE.
           IF IO-FG = 7
             CLOSE XLN-FILE
             GO TO OPEN-XLN-FILE.
           UNLOCK XLN-FILE.
       OPEN-XLN-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XLN-PATH TO E-FILE.
           OPEN OUTPUT XLN-FILE.
           IF IO-FG = 8
             GO TO OPEN-XLN-FILE.
           IF IO-FG = 7
             CLOSE XLN-FILE
             GO TO OPEN-XLN-FILE.
           UNLOCK XLN-FILE.
       READ-XLN-FILE.
           PERFORM READ-IT.
           MOVE XLN-PATH TO E-FILE.
           MOVE XLN-KEY TO E-KEYX.
           READ XLN-FILE.
           IF IO-FG = 8
             GO TO READ-XLN-FILE.
       WRITE-XLN-FILE.
           PERFORM WRITE-IT.
           MOVE XLN-PATH TO E-FILE.
           MOVE XLN-KEY TO E-KEYX.
           WRITE XLN-REC.
           IF IO-FG = 8
             GO TO WRITE-XLN-FILE.
           UNLOCK XLN-FILE.
       REWRITE-XLN-FILE.
           PERFORM REWRITE-IT.
           MOVE XLN-PATH TO E-FILE.
           MOVE XLN-KEY TO E-KEYX.
           REWRITE XLN-REC.
           IF IO-FG = 8
             GO TO REWRITE-XLN-FILE.
           UNLOCK XLN-FILE.
       DELETE-XLN-FILE.
           PERFORM DELETE-IT.
           MOVE XLN-PATH TO E-FILE.
           MOVE XLN-KEY TO E-KEYX.
           DELETE XLN-FILE.
           IF IO-FG = 8
             GO TO DELETE-XLN-FILE.
           UNLOCK XLN-FILE.
       CLOSE-XLN-FILE.
           PERFORM CLOSE-IT.
           CLOSE XLN-FILE.
