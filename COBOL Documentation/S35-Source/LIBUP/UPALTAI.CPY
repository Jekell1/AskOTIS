      * COPYMEMBER: LIBUP/UPALTAI
      **************************************************************
      *                (UPALTAI)
      **************************************************************
       OPEN-ALTA-FILE.
           PERFORM OPEN-IT.
           MOVE ALTA-PATH TO E-FILE.
           OPEN I-O ALTA-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALTA-FILE.
           IF IO-FG = 7
             CLOSE ALTA-FILE
             GO TO OPEN-ALTA-FILE.
           UNLOCK ALTA-FILE.
       READ-ALTA-FILE.
           PERFORM READ-IT.
           MOVE ALTA-PATH TO E-FILE.
           MOVE ALTA-KEY TO E-KEYX.
           READ ALTA-FILE.
           IF IO-FG = 8
             GO TO READ-ALTA-FILE.
       WRITE-ALTA-FILE.
           PERFORM WRITE-IT.
           MOVE ALTA-PATH TO E-FILE.
           MOVE ALTA-KEY TO E-KEYX.
           WRITE ALTA-REC.
           IF IO-FG = 8
             GO TO WRITE-ALTA-FILE.
           UNLOCK ALTA-FILE.
       REWRITE-ALTA-FILE.
           PERFORM REWRITE-IT.
           MOVE ALTA-PATH TO E-FILE.
           MOVE ALTA-KEY TO E-KEYX.
           REWRITE ALTA-REC.
           IF IO-FG = 8
             GO TO REWRITE-ALTA-FILE.
           UNLOCK ALTA-FILE.
       CLOSE-ALTA-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALTA-FILE.

      **********************************************
      * ASCII LOAN TRAILER DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ALTA-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ALTA-DONE-PATH TO E-FILE.
           OPEN OUTPUT ALTA-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALTA-DONE-FILE.
           IF IO-FG = 7
             CLOSE ALTA-DONE-FILE
             GO TO OPEN-ALTA-DONE-FILE.
       CLOSE-ALTA-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALTA-DONE-FILE.
