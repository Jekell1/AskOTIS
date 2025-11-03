      * COPYMEMBER: LIBEX/EXXDLI
      **************************************************************
      *                (EXXDLI)
      **************************************************************
       OPEN-XDL-FILE.
           PERFORM OPEN-IT.
           MOVE XDL-PATH TO E-FILE.
           OPEN I-O XDL-FILE.
           IF IO-FG = 8
             GO TO OPEN-XDL-FILE.
           IF IO-FG = 7
             CLOSE XDL-FILE
             GO TO OPEN-XDL-FILE.
           UNLOCK XDL-FILE.
       OPEN-XDL-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XDL-PATH TO E-FILE.
           OPEN OUTPUT XDL-FILE.
           IF IO-FG = 8
             GO TO OPEN-XDL-FILE.
           IF IO-FG = 7
             CLOSE XDL-FILE
             GO TO OPEN-XDL-FILE.
           UNLOCK XDL-FILE.
       READ-XDL-FILE.
           PERFORM READ-IT.
           MOVE XDL-PATH TO E-FILE.
           MOVE XDL-KEY TO E-KEYX.
           READ XDL-FILE.
           IF IO-FG = 8
             GO TO READ-XDL-FILE.
       WRITE-XDL-FILE.
           PERFORM WRITE-IT.
           MOVE XDL-PATH TO E-FILE.
           MOVE XDL-KEY TO E-KEYX.
           WRITE XDL-REC.
           IF IO-FG = 8
             GO TO WRITE-XDL-FILE.
           UNLOCK XDL-FILE.
       REWRITE-XDL-FILE.
           PERFORM REWRITE-IT.
           MOVE XDL-PATH TO E-FILE.
           MOVE XDL-KEY TO E-KEYX.
           REWRITE XDL-REC.
           IF IO-FG = 8
             GO TO REWRITE-XDL-FILE.
           UNLOCK XDL-FILE.
       DELETE-XDL-FILE.
           PERFORM DELETE-IT.
           MOVE XDL-PATH TO E-FILE.
           MOVE XDL-KEY TO E-KEYX.
           DELETE XDL-FILE.
           IF IO-FG = 8
             GO TO DELETE-XDL-FILE.
           UNLOCK XDL-FILE.
       CLOSE-XDL-FILE.
           PERFORM CLOSE-IT.
           CLOSE XDL-FILE.
