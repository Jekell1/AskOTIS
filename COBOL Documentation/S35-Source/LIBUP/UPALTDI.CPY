      * COPYMEMBER: LIBUP/UPALTDI
      **************************************************************
      *                (UPALTDI)
      **************************************************************
       OPEN-ALTD-FILE.
           PERFORM OPEN-IT.
           MOVE ALTD-PATH TO E-FILE.
           OPEN I-O ALTD-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALTD-FILE.
           IF IO-FG = 7
             CLOSE ALTD-FILE
             GO TO OPEN-ALTD-FILE.
           UNLOCK ALTD-FILE.
       READ-ALTD-FILE.
           PERFORM READ-IT.
           MOVE ALTD-PATH TO E-FILE.
           MOVE ALTD-KEY TO E-KEYX.
           READ ALTD-FILE.
           IF IO-FG = 8
             GO TO READ-ALTD-FILE.
       WRITE-ALTD-FILE.
           PERFORM WRITE-IT.
           MOVE ALTD-PATH TO E-FILE.
           MOVE ALTD-KEY TO E-KEYX.
           WRITE ALTD-REC.
           IF IO-FG = 8
             GO TO WRITE-ALTD-FILE.
           UNLOCK ALTD-FILE.
       REWRITE-ALTD-FILE.
           PERFORM REWRITE-IT.
           MOVE ALTD-PATH TO E-FILE.
           MOVE ALTD-KEY TO E-KEYX.
           REWRITE ALTD-REC.
           IF IO-FG = 8
             GO TO REWRITE-ALTD-FILE.
           UNLOCK ALTD-FILE.
       CLOSE-ALTD-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALTD-FILE.
      **********************************************
      * ASCII LOAN TRAILER DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ALTD-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ALTD-DONE-PATH TO E-FILE.
           OPEN OUTPUT ALTD-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ALTD-DONE-FILE.
           IF IO-FG = 7
             CLOSE ALTD-DONE-FILE
             GO TO OPEN-ALTD-DONE-FILE.
       CLOSE-ALTD-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ALTD-DONE-FILE.
