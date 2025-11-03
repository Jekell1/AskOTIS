      * COPYMEMBER: LIBUP/UPATRJI
      **************************************************************
      *                ASCII TRANSACTION FILE
      **************************************************************
       OPEN-ATRJ-FILE.
           PERFORM OPEN-IT.
           MOVE ATRJ-PATH TO E-FILE.
           OPEN I-O ATRJ-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRJ-FILE.
           IF IO-FG = 7
             CLOSE ATRJ-FILE
             GO TO OPEN-ATRJ-FILE.
           UNLOCK ATRJ-FILE.
       READ-ATRJ-FILE.
           PERFORM READ-IT.
           MOVE ATRJ-PATH TO E-FILE.
           MOVE ATRJ-KEY TO E-KEYX.
           READ ATRJ-FILE.
           IF IO-FG = 8
             GO TO READ-ATRJ-FILE.
       WRITE-ATRJ-FILE.
           PERFORM WRITE-IT.
           MOVE ATRJ-PATH TO E-FILE.
           MOVE ATRJ-KEY TO E-KEYX.
           WRITE ATRJ-REC.
           IF IO-FG = 8
             GO TO WRITE-ATRJ-FILE.
           UNLOCK ATRJ-FILE.
       REWRITE-ATRJ-FILE.
           PERFORM REWRITE-IT.
           MOVE ATRJ-PATH TO E-FILE.
           MOVE ATRJ-KEY TO E-KEYX.
           REWRITE ATRJ-REC.
           IF IO-FG = 8
             GO TO REWRITE-ATRJ-FILE.
           UNLOCK ATRJ-FILE.
       CLOSE-ATRJ-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRJ-FILE.
      **********************************************
      * ASCII TRANS FILE DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ATRJ-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ATRJ-DONE-PATH TO E-FILE.
           OPEN OUTPUT ATRJ-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRJ-DONE-FILE.
           IF IO-FG = 7
             CLOSE ATRJ-DONE-FILE
             GO TO OPEN-ATRJ-DONE-FILE.
       CLOSE-ATRJ-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRJ-DONE-FILE.
