      * COPYMEMBER: LIBUP/UPALTII
      **************************************************************
      *                (UPALTII)
      **************************************************************
       OPEN-ALTI-FILE.
           PERFORM OPEN-IT.
           MOVE ALTI-PATH TO E-FILE.
           OPEN I-O ALTI-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALTI-FILE.
           IF IO-FG = 7
             CLOSE ALTI-FILE
             GO TO OPEN-ALTI-FILE.
           UNLOCK ALTI-FILE.
       READ-ALTI-FILE.
           PERFORM READ-IT.
           MOVE ALTI-PATH TO E-FILE.
           MOVE ALTI-KEY TO E-KEYX.
           READ ALTI-FILE.
           IF IO-FG = 8
             GO TO READ-ALTI-FILE.
       WRITE-ALTI-FILE.
           PERFORM WRITE-IT.
           MOVE ALTI-PATH TO E-FILE.
           MOVE ALTI-KEY TO E-KEYX.
           WRITE ALTI-REC.
           IF IO-FG = 8
             GO TO WRITE-ALTI-FILE.
           UNLOCK ALTI-FILE.
       REWRITE-ALTI-FILE.
           PERFORM REWRITE-IT.
           MOVE ALTI-PATH TO E-FILE.
           MOVE ALTI-KEY TO E-KEYX.
           REWRITE ALTI-REC.
           IF IO-FG = 8
             GO TO REWRITE-ALTI-FILE.
           UNLOCK ALTI-FILE.
       CLOSE-ALTI-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALTI-FILE.
      **********************************************
      * ASCII LOAN TRAILER DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ALTI-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ALTI-DONE-PATH TO E-FILE.
           OPEN OUTPUT ALTI-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALTI-DONE-FILE.
           IF IO-FG = 7
             CLOSE ALTI-DONE-FILE
             GO TO OPEN-ALTI-DONE-FILE.
       CLOSE-ALTI-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALTI-DONE-FILE.
