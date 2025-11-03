      * COPYMEMBER: LIBUP/UPATRLI
      **************************************************************
      *                ASCII TRANSACTION FILE
      **************************************************************
       OPEN-ATRL-FILE.
           PERFORM OPEN-IT.
           MOVE ATRL-PATH TO E-FILE.
           OPEN I-O ATRL-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRL-FILE.
           IF IO-FG = 7
             CLOSE ATRL-FILE
             GO TO OPEN-ATRL-FILE.
           UNLOCK ATRL-FILE.
       READ-ATRL-FILE.
           PERFORM READ-IT.
           MOVE ATRL-PATH TO E-FILE.
           MOVE ATRL-KEY TO E-KEYX.
           READ ATRL-FILE.
           IF IO-FG = 8
             GO TO READ-ATRL-FILE.
       WRITE-ATRL-FILE.
           PERFORM WRITE-IT.
           MOVE ATRL-PATH TO E-FILE.
           MOVE ATRL-KEY TO E-KEYX.
           WRITE ATRL-REC.
           IF IO-FG = 8
             GO TO WRITE-ATRL-FILE.
           UNLOCK ATRL-FILE.
       REWRITE-ATRL-FILE.
           PERFORM REWRITE-IT.
           MOVE ATRL-PATH TO E-FILE.
           MOVE ATRL-KEY TO E-KEYX.
           REWRITE ATRL-REC.
           IF IO-FG = 8
             GO TO REWRITE-ATRL-FILE.
           UNLOCK ATRL-FILE.
       CLOSE-ATRL-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRL-FILE.
      **********************************************
      * ASCII TRANS FILE DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ATRL-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ATRL-DONE-PATH TO E-FILE.
           OPEN OUTPUT ATRL-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRL-DONE-FILE.
           IF IO-FG = 7
             CLOSE ATRL-DONE-FILE
             GO TO OPEN-ATRL-DONE-FILE.
       CLOSE-ATRL-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRL-DONE-FILE.
