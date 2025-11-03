      * COPYMEMBER: LIBUP/UPATRXI
      **************************************************************
      *                ASCII TRANSACTION FILE
      **************************************************************
       OPEN-ATRX-FILE.
           PERFORM OPEN-IT.
           MOVE ATRX-PATH TO E-FILE.
           OPEN I-O ATRX-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRX-FILE.
           IF IO-FG = 7
             CLOSE ATRX-FILE
             GO TO OPEN-ATRX-FILE.
           UNLOCK ATRX-FILE.
       READ-ATRX-FILE.
           PERFORM READ-IT.
           MOVE ATRX-PATH TO E-FILE.
           MOVE ATRX-KEY TO E-KEYX.
           READ ATRX-FILE.
           IF IO-FG = 8
             GO TO READ-ATRX-FILE.
       WRITE-ATRX-FILE.
           PERFORM WRITE-IT.
           MOVE ATRX-PATH TO E-FILE.
           MOVE ATRX-KEY TO E-KEYX.
           WRITE ATRX-REC.
           IF IO-FG = 8
             GO TO WRITE-ATRX-FILE.
           UNLOCK ATRX-FILE.
       REWRITE-ATRX-FILE.
           PERFORM REWRITE-IT.
           MOVE ATRX-PATH TO E-FILE.
           MOVE ATRX-KEY TO E-KEYX.
           REWRITE ATRX-REC.
           IF IO-FG = 8
             GO TO REWRITE-ATRX-FILE.
           UNLOCK ATRX-FILE.
       CLOSE-ATRX-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRX-FILE.
      **********************************************
      * ASCII TRANS FILE DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ATRX-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ATRX-DONE-PATH TO E-FILE.
           OPEN OUTPUT ATRX-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRX-DONE-FILE.
           IF IO-FG = 7
             CLOSE ATRX-DONE-FILE
             GO TO OPEN-ATRX-DONE-FILE.
       CLOSE-ATRX-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRX-DONE-FILE.
