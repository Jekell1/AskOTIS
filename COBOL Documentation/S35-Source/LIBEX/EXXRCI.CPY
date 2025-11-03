      * COPYMEMBER: LIBEX/EXXRCI
      **************************************************************
      *                (EXXRCI)
      **************************************************************
       OPEN-XRC-FILE.
           PERFORM OPEN-IT.
           MOVE XRC-PATH TO E-FILE.
           OPEN I-O XRC-FILE.
           IF IO-FG = 8
             GO TO OPEN-XRC-FILE.
           IF IO-FG = 7
             CLOSE XRC-FILE
             GO TO OPEN-XRC-FILE.
           UNLOCK XRC-FILE.
       OPEN-XRC-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XRC-PATH TO E-FILE.
           OPEN OUTPUT XRC-FILE.
           IF IO-FG = 8
             GO TO OPEN-XRC-FILE.
           IF IO-FG = 7
             CLOSE XRC-FILE
             GO TO OPEN-XRC-FILE.
           UNLOCK XRC-FILE.
       READ-XRC-FILE.
           PERFORM READ-IT.
           MOVE XRC-PATH TO E-FILE.
           MOVE XRC-KEY TO E-KEYX.
           READ XRC-FILE.
           IF IO-FG = 8
             GO TO READ-XRC-FILE.
       WRITE-XRC-FILE.
           PERFORM WRITE-IT.
           MOVE XRC-PATH TO E-FILE.
           MOVE XRC-KEY TO E-KEYX.
           WRITE XRC-REC.
           IF IO-FG = 8
             GO TO WRITE-XRC-FILE.
           UNLOCK XRC-FILE.
       REWRITE-XRC-FILE.
           PERFORM REWRITE-IT.
           MOVE XRC-PATH TO E-FILE.
           MOVE XRC-KEY TO E-KEYX.
           REWRITE XRC-REC.
           IF IO-FG = 8
             GO TO REWRITE-XRC-FILE.
           UNLOCK XRC-FILE.
       DELETE-XRC-FILE.
           PERFORM DELETE-IT.
           MOVE XRC-PATH TO E-FILE.
           MOVE XRC-KEY TO E-KEYX.
           DELETE XRC-FILE.
           IF IO-FG = 8
             GO TO DELETE-XRC-FILE.
           UNLOCK XRC-FILE.
       CLOSE-XRC-FILE.
           PERFORM CLOSE-IT.
           CLOSE XRC-FILE.
