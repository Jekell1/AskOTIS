      * COPYMEMBER: LIBEX/EXXCII
      **************************************************************
      *                (EXXCII)
      **************************************************************
       OPEN-XCI-FILE.
           PERFORM OPEN-IT.
           MOVE XCI-PATH TO E-FILE.
           OPEN I-O XCI-FILE.
           IF IO-FG = 8
             GO TO OPEN-XCI-FILE.
           IF IO-FG = 7
             CLOSE XCI-FILE
             GO TO OPEN-XCI-FILE.
           UNLOCK XCI-FILE.
       OPEN-XCI-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XCI-PATH TO E-FILE.
           OPEN OUTPUT XCI-FILE.
           IF IO-FG = 8
             GO TO OPEN-XCI-FILE.
           IF IO-FG = 7
             CLOSE XCI-FILE
             GO TO OPEN-XCI-FILE.
           UNLOCK XCI-FILE.
       READ-XCI-FILE.
           PERFORM READ-IT.
           MOVE XCI-PATH TO E-FILE.
           MOVE XCI-KEY TO E-KEYX.
           READ XCI-FILE.
           IF IO-FG = 8
             GO TO READ-XCI-FILE.
       WRITE-XCI-FILE.
           PERFORM WRITE-IT.
           MOVE XCI-PATH TO E-FILE.
           MOVE XCI-KEY TO E-KEYX.
           WRITE XCI-REC.
           IF IO-FG = 8
             GO TO WRITE-XCI-FILE.
           UNLOCK XCI-FILE.
       REWRITE-XCI-FILE.
           PERFORM REWRITE-IT.
           MOVE XCI-PATH TO E-FILE.
           MOVE XCI-KEY TO E-KEYX.
           REWRITE XCI-REC.
           IF IO-FG = 8
             GO TO REWRITE-XCI-FILE.
           UNLOCK XCI-FILE.
       DELETE-XCI-FILE.
           PERFORM DELETE-IT.
           MOVE XCI-PATH TO E-FILE.
           MOVE XCI-KEY TO E-KEYX.
           DELETE XCI-FILE.
           IF IO-FG = 8
             GO TO DELETE-XCI-FILE.
           UNLOCK XCI-FILE.
       CLOSE-XCI-FILE.
           PERFORM CLOSE-IT.
           CLOSE XCI-FILE.
