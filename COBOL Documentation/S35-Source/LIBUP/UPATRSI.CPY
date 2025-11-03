      * COPYMEMBER: LIBUP/UPATRSI
      **************************************************************
      *                ASCII TRANSACTION FILE
      **************************************************************
       OPEN-ATRS-FILE.
           PERFORM OPEN-IT.
           MOVE ATRS-PATH TO E-FILE.
           OPEN I-O ATRS-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRS-FILE.
           IF IO-FG = 7
             CLOSE ATRS-FILE
             GO TO OPEN-ATRS-FILE.
           UNLOCK ATRS-FILE.
       READ-ATRS-FILE.
           PERFORM READ-IT.
           MOVE ATRS-PATH TO E-FILE.
           MOVE ATRS-KEY TO E-KEYX.
           READ ATRS-FILE.
           IF IO-FG = 8
             GO TO READ-ATRS-FILE.
       WRITE-ATRS-FILE.
           PERFORM WRITE-IT.
           MOVE ATRS-PATH TO E-FILE.
           MOVE ATRS-KEY TO E-KEYX.
           WRITE ATRS-REC.
           IF IO-FG = 8
             GO TO WRITE-ATRS-FILE.
           UNLOCK ATRS-FILE.
       REWRITE-ATRS-FILE.
           PERFORM REWRITE-IT.
           MOVE ATRS-PATH TO E-FILE.
           MOVE ATRS-KEY TO E-KEYX.
           REWRITE ATRS-REC.
           IF IO-FG = 8
             GO TO REWRITE-ATRS-FILE.
           UNLOCK ATRS-FILE.
       CLOSE-ATRS-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRS-FILE.
      **********************************************
      * ASCII TRANS FILE DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ATRS-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ATRS-DONE-PATH TO E-FILE.
           OPEN OUTPUT ATRS-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRS-DONE-FILE.
           IF IO-FG = 7
             CLOSE ATRS-DONE-FILE
             GO TO OPEN-ATRS-DONE-FILE.
       CLOSE-ATRS-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRS-DONE-FILE.
