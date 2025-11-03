      * COPYMEMBER: LIBEX/EXXBMI
      **************************************************************
      *                (EXXBMI)
      **************************************************************
       OPEN-XBM-FILE.
           PERFORM OPEN-IT.
           MOVE XBM-PATH TO E-FILE.
           OPEN I-O XBM-FILE.
           IF IO-FG = 8
             GO TO OPEN-XBM-FILE.
           IF IO-FG = 7
             CLOSE XBM-FILE
             GO TO OPEN-XBM-FILE.
           UNLOCK XBM-FILE.
       OPEN-XBM-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XBM-PATH TO E-FILE.
           OPEN OUTPUT XBM-FILE.
           IF IO-FG = 8
             GO TO OPEN-XBM-FILE.
           IF IO-FG = 7
             CLOSE XBM-FILE
             GO TO OPEN-XBM-FILE.
           UNLOCK XBM-FILE.
       READ-XBM-FILE.
           PERFORM READ-IT.
           MOVE XBM-PATH TO E-FILE.
           MOVE XBM-KEY TO E-KEYX.
           READ XBM-FILE.
           IF IO-FG = 8
             GO TO READ-XBM-FILE.
       WRITE-XBM-FILE.
           PERFORM WRITE-IT.
           MOVE XBM-PATH TO E-FILE.
           MOVE XBM-KEY TO E-KEYX.
           WRITE XBM-REC.
           IF IO-FG = 8
             GO TO WRITE-XBM-FILE.
           UNLOCK XBM-FILE.
       REWRITE-XBM-FILE.
           PERFORM REWRITE-IT.
           MOVE XBM-PATH TO E-FILE.
           MOVE XBM-KEY TO E-KEYX.
           REWRITE XBM-REC.
           IF IO-FG = 8
             GO TO REWRITE-XBM-FILE.
           UNLOCK XBM-FILE.
       DELETE-XBM-FILE.
           PERFORM DELETE-IT.
           MOVE XBM-PATH TO E-FILE.
           MOVE XBM-KEY TO E-KEYX.
           DELETE XBM-FILE.
           IF IO-FG = 8
             GO TO DELETE-XBM-FILE.
           UNLOCK XBM-FILE.
       CLOSE-XBM-FILE.
           PERFORM CLOSE-IT.
           CLOSE XBM-FILE.
