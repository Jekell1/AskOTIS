      * COPYMEMBER: LIBUP/UPATRII
      **************************************************************
      *                ASCII TRANSACTION FILE
      **************************************************************
       OPEN-ATRI-FILE.
           PERFORM OPEN-IT.
           MOVE ATRI-PATH TO E-FILE.
           OPEN I-O ATRI-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRI-FILE.
           IF IO-FG = 7
             CLOSE ATRI-FILE
             GO TO OPEN-ATRI-FILE.
           UNLOCK ATRI-FILE.
       READ-ATRI-FILE.
           PERFORM READ-IT.
           MOVE ATRI-PATH TO E-FILE.
           MOVE ATRI-KEY TO E-KEYX.
           READ ATRI-FILE.
           IF IO-FG = 8
             GO TO READ-ATRI-FILE.
       WRITE-ATRI-FILE.
           PERFORM WRITE-IT.
           MOVE ATRI-PATH TO E-FILE.
           MOVE ATRI-KEY TO E-KEYX.
           WRITE ATRI-REC.
           IF IO-FG = 8
             GO TO WRITE-ATRI-FILE.
           UNLOCK ATRI-FILE.
       REWRITE-ATRI-FILE.
           PERFORM REWRITE-IT.
           MOVE ATRI-PATH TO E-FILE.
           MOVE ATRI-KEY TO E-KEYX.
           REWRITE ATRI-REC.
           IF IO-FG = 8
             GO TO REWRITE-ATRI-FILE.
           UNLOCK ATRI-FILE.
       CLOSE-ATRI-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRI-FILE.
      **********************************************
      * ASCII TRANS FILE DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ATRI-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ATRI-DONE-PATH TO E-FILE.
           OPEN OUTPUT ATRI-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRI-DONE-FILE.
           IF IO-FG = 7
             CLOSE ATRI-DONE-FILE
             GO TO OPEN-ATRI-DONE-FILE.
       CLOSE-ATRI-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRI-DONE-FILE.
