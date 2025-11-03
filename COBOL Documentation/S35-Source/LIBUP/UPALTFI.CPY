      * COPYMEMBER: LIBUP/UPALTFI
      **************************************************************
      *                (UPALTFI)
      **************************************************************
       OPEN-ALTF-FILE.
           PERFORM OPEN-IT.
           MOVE ALTF-PATH TO E-FILE.
           OPEN I-O ALTF-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALTF-FILE.
           IF IO-FG = 7
             CLOSE ALTF-FILE
             GO TO OPEN-ALTF-FILE.
           UNLOCK ALTF-FILE.
       READ-ALTF-FILE.
           PERFORM READ-IT.
           MOVE ALTF-PATH TO E-FILE.
           MOVE ALTF-KEY TO E-KEYX.
           READ ALTF-FILE.
           IF IO-FG = 8
             GO TO READ-ALTF-FILE.
       WRITE-ALTF-FILE.
           PERFORM WRITE-IT.
           MOVE ALTF-PATH TO E-FILE.
           MOVE ALTF-KEY TO E-KEYX.
           WRITE ALTF-REC.
           IF IO-FG = 8
             GO TO WRITE-ALTF-FILE.
           UNLOCK ALTF-FILE.
       REWRITE-ALTF-FILE.
           PERFORM REWRITE-IT.
           MOVE ALTF-PATH TO E-FILE.
           MOVE ALTF-KEY TO E-KEYX.
           REWRITE ALTF-REC.
           IF IO-FG = 8
             GO TO REWRITE-ALTF-FILE.
           UNLOCK ALTF-FILE.
       CLOSE-ALTF-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALTF-FILE.
      **********************************************
      * ASCII LOAN TRAILER DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ALTF-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ALTF-DONE-PATH TO E-FILE.
           OPEN OUTPUT ALTF-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALTF-DONE-FILE.
           IF IO-FG = 7
             CLOSE ALTF-DONE-FILE
             GO TO OPEN-ALTF-DONE-FILE.
       CLOSE-ALTF-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALTF-DONE-FILE.
