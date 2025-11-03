      * COPYMEMBER: LIBUP/UPATRDI
      **************************************************************
      *                ASCII TRANSACTION FILE
      **************************************************************
       OPEN-ATRD-FILE.
           PERFORM OPEN-IT.
           MOVE ATRD-PATH TO E-FILE.
           OPEN I-O ATRD-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRD-FILE.
           IF IO-FG = 7
             CLOSE ATRD-FILE
             GO TO OPEN-ATRD-FILE.
           UNLOCK ATRD-FILE.
       READ-ATRD-FILE.
           PERFORM READ-IT.
           MOVE ATRD-PATH TO E-FILE.
           MOVE ATRD-KEY TO E-KEYX.
           READ ATRD-FILE.
           IF IO-FG = 8
             GO TO READ-ATRD-FILE.
       WRITE-ATRD-FILE.
           PERFORM WRITE-IT.
           MOVE ATRD-PATH TO E-FILE.
           MOVE ATRD-KEY TO E-KEYX.
           WRITE ATRD-REC.
           IF IO-FG = 8
             GO TO WRITE-ATRD-FILE.
           UNLOCK ATRD-FILE.
       REWRITE-ATRD-FILE.
           PERFORM REWRITE-IT.
           MOVE ATRD-PATH TO E-FILE.
           MOVE ATRD-KEY TO E-KEYX.
           REWRITE ATRD-REC.
           IF IO-FG = 8
             GO TO REWRITE-ATRD-FILE.
           UNLOCK ATRD-FILE.
       CLOSE-ATRD-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRD-FILE.
      **********************************************
      * ASCII TRANS FILE DONE TAG FILE IN ASCII/DONE
      **********************************************
       OPEN-ATRD-DONE-FILE.
           PERFORM OPEN-IT.
           MOVE ATRD-DONE-PATH TO E-FILE.
           OPEN OUTPUT ATRD-DONE-FILE.
           IF IO-FG = 8
             GO TO OPEN-ATRD-DONE-FILE.
           IF IO-FG = 7
             CLOSE ATRD-DONE-FILE
             GO TO OPEN-ATRD-DONE-FILE.
       CLOSE-ATRD-DONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE ATRD-DONE-FILE.
