      * COPYMEMBER: LIBUP/UPALTLI
      **************************************************************
      *                (UPALTLI)
      **************************************************************
       OPEN-ALTL-FILE.
           PERFORM OPEN-IT.
           MOVE ALTL-PATH TO E-FILE.
           OPEN I-O ALTL-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALTL-FILE.
           IF IO-FG = 7
             CLOSE ALTL-FILE
             GO TO OPEN-ALTL-FILE.
           UNLOCK ALTL-FILE.
       READ-ALTL-FILE.
           PERFORM READ-IT.
           MOVE ALTL-PATH TO E-FILE.
           MOVE ALTL-KEY TO E-KEYX.
           READ ALTL-FILE.
           IF IO-FG = 8
             GO TO READ-ALTL-FILE.
       WRITE-ALTL-FILE.
           PERFORM WRITE-IT.
           MOVE ALTL-PATH TO E-FILE.
           MOVE ALTL-KEY TO E-KEYX.
           WRITE ALTL-REC.
           IF IO-FG = 8
             GO TO WRITE-ALTL-FILE.
           UNLOCK ALTL-FILE.
       REWRITE-ALTL-FILE.
           PERFORM REWRITE-IT.
           MOVE ALTL-PATH TO E-FILE.
           MOVE ALTL-KEY TO E-KEYX.
           REWRITE ALTL-REC.
           IF IO-FG = 8
             GO TO REWRITE-ALTL-FILE.
           UNLOCK ALTL-FILE.
       CLOSE-ALTL-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALTL-FILE.
      **********************************************
      * ASCII LOAN TRAILER DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ALTL-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ALTL-DONE-PATH TO E-FILE.
           OPEN OUTPUT ALTL-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALTL-DONE-FILE.
           IF IO-FG = 7
             CLOSE ALTL-DONE-FILE
             GO TO OPEN-ALTL-DONE-FILE.
       CLOSE-ALTL-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALTL-DONE-FILE.
