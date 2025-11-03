      * COPYMEMBER: LIBUP/UPALTCI
      **************************************************************
      *                (UPALTCI)
      **************************************************************
       OPEN-ALTC-FILE.
           PERFORM OPEN-IT.
           MOVE ALTC-PATH TO E-FILE.
           OPEN I-O ALTC-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALTC-FILE.
           IF IO-FG = 7
             CLOSE ALTC-FILE
             GO TO OPEN-ALTC-FILE.
           UNLOCK ALTC-FILE.
       READ-ALTC-FILE.
           PERFORM READ-IT.
           MOVE ALTC-PATH TO E-FILE.
           MOVE ALTC-KEY TO E-KEYX.
           READ ALTC-FILE.
           IF IO-FG = 8
             GO TO READ-ALTC-FILE.
       WRITE-ALTC-FILE.
           PERFORM WRITE-IT.
           MOVE ALTC-PATH TO E-FILE.
           MOVE ALTC-KEY TO E-KEYX.
           WRITE ALTC-REC.
           IF IO-FG = 8
             GO TO WRITE-ALTC-FILE.
           UNLOCK ALTC-FILE.
       REWRITE-ALTC-FILE.
           PERFORM REWRITE-IT.
           MOVE ALTC-PATH TO E-FILE.
           MOVE ALTC-KEY TO E-KEYX.
           REWRITE ALTC-REC.
           IF IO-FG = 8
             GO TO REWRITE-ALTC-FILE.
           UNLOCK ALTC-FILE.
       CLOSE-ALTC-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALTC-FILE.

      **********************************************
      * ASCII LOAN TRAILER DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ALTC-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ALTC-DONE-PATH TO E-FILE.
           OPEN OUTPUT ALTC-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALTC-DONE-FILE.
           IF IO-FG = 7
             CLOSE ALTC-DONE-FILE
             GO TO OPEN-ALTC-DONE-FILE.
       CLOSE-ALTC-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALTC-DONE-FILE.
