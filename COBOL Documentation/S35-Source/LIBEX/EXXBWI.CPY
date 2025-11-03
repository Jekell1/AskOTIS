      * COPYMEMBER: LIBEX/EXXBWI
      **************************************************************
      *                (EXXBWI)
      **************************************************************
       OPEN-XBW-FILE.
           PERFORM OPEN-IT.
           MOVE XBW-PATH TO E-FILE.
           OPEN I-O XBW-FILE.
           IF IO-FG = 8
             GO TO OPEN-XBW-FILE.
           IF IO-FG = 7
             CLOSE XBW-FILE
             GO TO OPEN-XBW-FILE.
           UNLOCK XBW-FILE.
       OPEN-XBW-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XBW-PATH TO E-FILE.
           OPEN OUTPUT XBW-FILE.
           IF IO-FG = 8
             GO TO OPEN-XBW-FILE.
           IF IO-FG = 7
             CLOSE XBW-FILE
             GO TO OPEN-XBW-FILE.
           UNLOCK XBW-FILE.
       READ-XBW-FILE.
           PERFORM READ-IT.
           MOVE XBW-PATH TO E-FILE.
           MOVE XBW-KEY TO E-KEYX.
           READ XBW-FILE.
           IF IO-FG = 8
             GO TO READ-XBW-FILE.
       WRITE-XBW-FILE.
           PERFORM WRITE-IT.
           MOVE XBW-PATH TO E-FILE.
           MOVE XBW-KEY TO E-KEYX.
           WRITE XBW-REC.
           IF IO-FG = 8
             GO TO WRITE-XBW-FILE.
           UNLOCK XBW-FILE.
       REWRITE-XBW-FILE.
           PERFORM REWRITE-IT.
           MOVE XBW-PATH TO E-FILE.
           MOVE XBW-KEY TO E-KEYX.
           REWRITE XBW-REC.
           IF IO-FG = 8
             GO TO REWRITE-XBW-FILE.
           UNLOCK XBW-FILE.
       DELETE-XBW-FILE.
           PERFORM DELETE-IT.
           MOVE XBW-PATH TO E-FILE.
           MOVE XBW-KEY TO E-KEYX.
           DELETE XBW-FILE.
           IF IO-FG = 8
             GO TO DELETE-XBW-FILE.
           UNLOCK XBW-FILE.
       CLOSE-XBW-FILE.
           PERFORM CLOSE-IT.
           CLOSE XBW-FILE.
