      * COPYMEMBER: LIBUP/UPATRPI
      **************************************************************
      *                ASCII TRANSACTION FILE
      **************************************************************
       OPEN-ATRP-FILE.
           PERFORM OPEN-IT.
           MOVE ATRP-PATH TO E-FILE.
           OPEN I-O ATRP-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRP-FILE.
           IF IO-FG = 7
             CLOSE ATRP-FILE
             GO TO OPEN-ATRP-FILE.
           UNLOCK ATRP-FILE.
       READ-ATRP-FILE.
           PERFORM READ-IT.
           MOVE ATRP-PATH TO E-FILE.
           MOVE ATRP-KEY TO E-KEYX.
           READ ATRP-FILE.
           IF IO-FG = 8
             GO TO READ-ATRP-FILE.
       WRITE-ATRP-FILE.
           PERFORM WRITE-IT.
           MOVE ATRP-PATH TO E-FILE.
           MOVE ATRP-KEY TO E-KEYX.
           WRITE ATRP-REC.
           IF IO-FG = 8
             GO TO WRITE-ATRP-FILE.
           UNLOCK ATRP-FILE.
       REWRITE-ATRP-FILE.
           PERFORM REWRITE-IT.
           MOVE ATRP-PATH TO E-FILE.
           MOVE ATRP-KEY TO E-KEYX.
           REWRITE ATRP-REC.
           IF IO-FG = 8
             GO TO REWRITE-ATRP-FILE.
           UNLOCK ATRP-FILE.
       CLOSE-ATRP-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRP-FILE.
      **********************************************
      * ASCII TRANS FILE DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ATRP-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ATRP-DONE-PATH TO E-FILE.
           OPEN OUTPUT ATRP-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRP-DONE-FILE.
           IF IO-FG = 7
             CLOSE ATRP-DONE-FILE
             GO TO OPEN-ATRP-DONE-FILE.
       CLOSE-ATRP-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRP-DONE-FILE.
